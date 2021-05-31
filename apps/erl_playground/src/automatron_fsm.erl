-module(automatron_fsm).

-behaviour(gen_statem).

-include("tables.hrl").
-include("statem.hrl").

-export([start_link/1, delete_automatron_pid/1]).
-export([init/1, callback_mode/0, terminate/3, code_change/4]).
-export([list_options/3, operator/3, idle/3, idle_wait/3, chat/3]). %% states

-define(CALL_TIMEOUT, 100).
-define(DISCONNECT_OPERATOR(Reply), {next_state, list_options, Data#data{msg_current = Data#data.msg_max, operator_pid = undefined}, []++Reply}).

-record(data,
        {server_pid,
         username,
         unique :: atom(),
         timeout :: integer(),
         msg_max :: integer(),
         msg_current :: integer(),
         other_user_pid :: pid(),
         monitor :: reference(),
         other_username,
         operator_pid = undefined :: pid()}).

%%%%%%%%%%%%%%%%
%% Server API %%
%%%%%%%%%%%%%%%%

start_link([ServerPid, UserId, Socket]) ->
    start_link([ServerPid, UserId, Socket, 10, 3]);
start_link([ServerPid, UserId, Socket, TimeoutSecs, MsgMax]) ->
    Name = list_to_atom(port_to_list(Socket)),
    gen_statem:start_link({local, Name},
                          ?MODULE,
                          [#data{server_pid = ServerPid,
                                 username = UserId,
                                 unique = Name,
                                 timeout = TimeoutSecs,
                                 msg_max = MsgMax,
                                 msg_current = MsgMax}],
                          []).

delete_automatron_pid(Pid) ->
    ets:delete(?TABLE, Pid),
    lager:info("Deleting ~p from the table ~p", [Pid, ?TABLE]).
    
%%%%%%%%%%%%%%%%%%%%%%%%%
%% Client-Client API   %%
%% All calls are async %%
%% to avoid deadlocks  %%
%%%%%%%%%%%%%%%%%%%%%%%%%

%% Ask the other statem for chat session
ask_chat(OtherPid) ->
    gen_statem:cast(OtherPid, {ask_chat, self()}).

%% Forward the client message accepting the transaction
accept_chat(OtherPid) ->
    gen_statem:cast(OtherPid, {accept_chat, self()}).

tell_other_my_username(OtherPid, MyUsername) ->
    gen_statem:cast(OtherPid, {my_username_is, MyUsername}).

%%%%%%%%%%%%%%%%%%%%%%%%%
%% gen_statem required %%
%%%%%%%%%%%%%%%%%%%%%%%%%

init([Data = #data{}]) ->
    set_not_available(Data#data.username),
    {ok, list_options, Data}.

callback_mode() ->
    state_functions.

terminate(_Reason, _State, _Data = #data{}) ->
    lager:info("Automatron is killed."),
    automatron_fsm:delete_automatron_pid(self()),
    ok.

code_change(_Vsn, State, Data, _Extra) ->
    {ok, State, Data}.

%%%%%%%%%%%%
%% States %%
%%%%%%%%%%%%

list_options({call, From}, {user_request, <<"0">>}, Data = #data{}) ->
    {keep_state, Data, {reply, From, welcome_msg(Data#data.username)}};
list_options({call, From}, {user_request, <<"1">>}, Data = #data{}) ->
    {keep_state, Data, {reply, From, get_joke()}};
list_options({call, From}, {user_request, <<"2">>}, Data = #data{}) ->
    {keep_state, Data, {reply, From, atom_to_binary(Data#data.unique)}};
list_options({call, From}, {user_request, <<"3">>}, Data = #data{timeout = Timeout}) ->
    case operator_sup:connect(?CALL_TIMEOUT) of
        timeout -> 
            {keep_state, Data, [{reply, From, operator_unavailable_msg()}]};
        Pid -> 
            {next_state, operator, Data#data{operator_pid = Pid}, [{state_timeout, Timeout * 1000, []}, {reply, From, operator_msg()}]}
    end;
list_options({call, From}, {user_request, <<"4">>}, Data = #data{timeout = Timeout}) ->
    notice_change(idle),
    set_available(Data#data.username),
    check_presence_other_users(),
    {next_state,
     idle,
     Data,
     [{state_timeout, Timeout * 10 * 1000, []}, {reply, From,  waiting_for_chat_msg()}]};
list_options(cast, check_presence_other_users, Data = #data{}) ->
    {keep_state, Data, [postpone]};
list_options(cast, _Msg, Data = #data{}) ->
        lager:info("automatron received a cast message."),
        {next_state, list_options, Data};
list_options({call, From}, {test_query, state_name}, Data = #data{}) ->
    {keep_state, Data, {reply, From, ?STATE_WELCOME_LIST}};
list_options({call, From}, Msg, Data = #data{}) ->
    lager:info("automatron could not recognize the call with message ~p", [Msg]),
    {keep_state, Data, {reply, From, <<"bad_msg">>}};
list_options(_, Event, Data = #data{}) ->
    unexpected(Event, idle),
    {keep_state, Data}.

idle(cast, {ask_chat, OtherPid}, Data = #data{timeout = Timeout}) ->
    Ref = monitor(process, OtherPid),
    notice_change(idle_wait),
    accept_chat(OtherPid),
    set_not_available(Data#data.username),
    {next_state, idle_wait, Data#data{other_user_pid = OtherPid, monitor = Ref}, [{state_timeout, Timeout * 1000, []}]};
idle(cast, {my_username_is, OtherUsername}, Data = #data{}) ->
    {keep_state, Data#data{other_username = OtherUsername}};
idle(cast, check_presence_other_users, Data = #data{timeout = Timeout, server_pid = ServerPid}) ->
    case pick_user() of
        [] -> 
            tell_user(ServerPid, <<"There are no users available at the moment. Please wait.">>),
            {keep_state, Data};
        [OtherPid, OtherUsername] -> 
            set_not_available(Data#data.username),
            ask_chat(OtherPid),
            notice("asking user ~p to chat~n", [OtherPid]),
            Ref = monitor(process, OtherPid),
            tell_other_my_username(OtherPid, Data#data.username),
            {next_state, idle_wait, Data#data{other_user_pid = OtherPid, monitor = Ref, other_username = OtherUsername}, [{state_timeout, Timeout * 1000, []}]}
    end;
idle(cast, Event, Data = #data{}) ->
    unexpected(Event, idle),
    {keep_state, Data};
idle({call, From}, {test_query, state_name}, Data = #data{}) ->
    {keep_state, Data, {reply, From, ?STATE_IDLE}};
idle({call, From}, _Msg, Data = #data{}) ->
    {keep_state, Data, {reply, From, waiting_for_chat_msg()}};
idle(state_timeout, [], Data = #data{server_pid = ServerPid}) ->
    set_not_available(Data#data.username),
    tell_user(ServerPid, waiting_timeout_msg(Data#data.username)),
    {next_state, list_options, Data};
idle(_, Event, Data = #data{}) ->
    unexpected(Event, idle),
    {keep_state, Data}.
    

%% idle_wait allows to expect replies from the other side and start chat 
idle_wait(cast, {ask_chat, OtherPid}, Data = #data{other_user_pid = OtherPid, server_pid = ServerPid, other_username = OtherUsername}) ->
    accept_chat(OtherPid),
    notice_change(chat),
    % presentations
    tell_user(ServerPid, enter_chat_msg(OtherUsername)),
    {next_state, chat, Data};
idle_wait(cast, {my_username_is, OtherUsername}, Data = #data{}) ->
    {keep_state, Data#data{other_username = OtherUsername}};
idle_wait(cast, {accept_chat, OtherPid}, Data = #data{other_user_pid = OtherPid, server_pid = ServerPid, other_username = OtherUsername}) ->
    accept_chat(OtherPid),
    notice_change(chat),
    % presentations
    tell_user(ServerPid, enter_chat_msg(OtherUsername)),
    {next_state, chat, Data};
idle_wait(info, {'DOWN', Monitor,_,_,_}, Data = #data{monitor=Monitor}) ->
    notice('Other user left, going back to idle', []),
    notice_change(idle),
    {next_state, idle, Data};
idle_wait(cast, Event, Data = #data{}) ->
    unexpected(Event, idle_wait),
    {keep_state, Data};
idle_wait({call, From}, {test_query, state_name}, Data = #data{}) ->
    {keep_state, Data, {reply, From, ?STATE_IDLE_WAIT}};
idle_wait({call, _From}, Event, Data = #data{}) ->
    unexpected(Event, idle_wait),
    {keep_state, Data};
idle_wait(state_timeout, [], Data = #data{server_pid = ServerPid}) ->
    set_not_available(Data#data.username),
    tell_user(ServerPid, waiting_timeout_msg(Data#data.username)),
    {next_state, list_options, Data};
idle_wait(_, Event, Data = #data{}) ->
    unexpected(Event, idle_wait),
    {keep_state, Data}.





chat(cast, {accept_chat, OtherPid}, Data = #data{other_user_pid = OtherPid}) ->
    {keep_state, Data};
chat(cast, {tell_other_user, Msg = <<"bye">>}, Data = #data{server_pid = ServerPid}) ->
    notice("Received the message: \"~p\"~n", [Msg]),
    notice_change(list_options),
	tell_user(ServerPid, <<"The other user left the chat.">>),
	tell_user(ServerPid, welcome_msg()),
    {next_state, list_options, Data};
chat(cast, {tell_other_user, Msg}, Data=#data{server_pid = ServerPid, other_username=OtherUsername}) when is_binary(Msg) ->
    NewMsg = prepend_username(OtherUsername, Msg),
    tell_user(ServerPid, NewMsg),
    {keep_state, Data};
chat({call, From}, {test_query, state_name}, Data = #data{}) ->
    {keep_state, Data, {reply, From, ?STATE_CHAT}};
chat({call, From}, {user_request, Msg = <<"bye">>}, Data = #data{other_user_pid=OtherPid, server_pid = ServerPid}) ->
    notice("Sending message \"~p\"~n", [Msg]),
    gen_statem:cast(OtherPid, {tell_other_user, Msg}),
    notice_change(list_options),
	tell_user(ServerPid, welcome_msg()),
    {next_state, list_options, Data, {reply, From, silent}}; 
chat({call, From}, {user_request, Msg}, Data = #data{other_user_pid=OtherPid}) ->
    notice("Sending message \"~p\" to ~p~n", [Msg, OtherPid]),
    gen_statem:cast(OtherPid, {tell_other_user, Msg}),
    {keep_state, Data, {reply, From, silent}};
chat(info, {'DOWN', Monitor,_,_,_}, Data = #data{monitor=Monitor, server_pid = ServerPid}) ->
    tell_user(ServerPid, <<"The other user left abruptly.">>),
    notice_change(list_options),
	tell_user(ServerPid, welcome_msg()),
    {next_state, list_options, Data};
chat(Event, Msg, Data = #data{}) ->
    unexpected([Event, Msg], chat),
    {keep_state, Data}.    









        
operator({call, From}, {test_query, state_name}, Data = #data{}) ->
    {keep_state, Data, {reply, From, ?STATE_OPERATOR}};
operator({call, From}, Event = {user_request, _Msg}, Data = #data{msg_current = MsgCounter, msg_max = Max, operator_pid = Operator}) when MsgCounter == 0 andalso Operator =/= undefined ->
    send_disconnect_operator(Operator, Event),
    ?DISCONNECT_OPERATOR([{reply, From, operator_timeout_msg(Data#data.username)}]);
operator({call, From}, Event = {user_request, Msg}, Data = #data{server_pid = ServerPid, msg_current = MsgCounter, operator_pid = Operator}) when is_binary(Msg) andalso Operator =/= undefined  ->
    String = erlang:binary_to_list(Msg),
    Req = 
        case string:to_integer(String) of
            {error, no_integer} -> String;
            {N, _Rest} -> N
        end,
    case operator_sup:ask(Req, [Operator, ?CALL_TIMEOUT]) of
        timeout -> 
            unexpected(Event, operator),
            send_disconnect_operator(Operator, Event),
            ?DISCONNECT_OPERATOR([{reply, From, operator_timeout_msg(Data#data.username)}]);
        Answer ->
            {keep_state, Data#data{msg_current = MsgCounter - 1}, {reply, From, Answer}}
    end;
operator(state_timeout, [], Data = #data{server_pid = ServerPid, operator_pid = Operator}) when Operator =/= undefined ->
    send_disconnect_operator(Operator, []),
    tell_user(ServerPid, operator_timeout_msg(Data#data.username)),
    ?DISCONNECT_OPERATOR([]);
operator(_, Event, Data = #data{}) ->
    unexpected(Event, operator),
    {keep_state, Data}.

%%%%%%%%%%%%%%
%% Messages %%
%%%%%%%%%%%%%%

get_joke() ->
    Chosen = rand:uniform(5),
    case Chosen of
        1 ->
            <<"Why did the golfer change his pants? Because he got a hole "
              "in one!">>;
        2 ->
            <<"What does a baby computer call his father? Data!">>;
        3 ->
            <<"Why did the PowerPoint Presentation cross the road? A. To get "
              "to the other slide">>;
        4 ->
            <<"What did the the drummer call his twin daughters? Anna one, "
              "Anna two!">>;
        5 ->
            <<"I would tell you a UDP joke, but you might not get it.">>
    end.

welcome_msg(User) when is_list(User) ->
    List = "WELCOME " ++ User ++ " TO AUTOMATRON CALL CENTER~n" ++ welcome_msg(),
    list_to_binary(List).
welcome_msg() ->
    List = "1 - Press 1 to receive the jokes of the day.~n"
            "2 - Press 2 to know your unique identifier for the call.~n"
            "3 - Press 3 to talk to an operator.~n"
            "4 - Press 4 to chat with another user.~n"
            "0 - Press 0 to listen to this message again.",
    list_to_binary(List).
        
operator_msg() ->
    <<"I am the operator, how can I help you?">>.

operator_unavailable_msg() ->
    <<"There are no operators available at the moment. Please try again later.">>.
    
operator_timeout_msg(User) when is_list(User) ->
    Msg = "Your time with the operator has finished. Goodbye, "
          ++ User
          ++ "!~n"
          ++ binary:bin_to_list(welcome_msg(User)),
    erlang:list_to_binary(Msg).

waiting_timeout_msg(User) when is_list(User) ->
    Msg = "Your time has finished. Goodbye, "
          ++ User
          ++ "!~n"
          ++ binary:bin_to_list(welcome_msg(User)),
    erlang:list_to_binary(Msg).

enter_chat_msg(User) when is_list(User) ->
    Msg = "You are now connected with " ++ User ++ "!~n" ++ "Stop by replying bye.",
    erlang:list_to_binary(Msg).

waiting_for_chat_msg() ->
    Msg = "Please wait for another user to be available.",
    erlang:list_to_binary(Msg).

%%%%%%%%%%%%%%%%%
%%  Utilities  %%
%%%%%%%%%%%%%%%%%

pick_user() ->
    Query = [{{'$1','$2', '$3'}, [{'=:=','$2',?AVAILABLE}], [['$1','$3']]}],
    ListWithoutMe = [ [Pid, Username] || [Pid, Username] <- ets:select(?TABLE, Query), Pid =/= self() ],
    Selected = 
        case ListWithoutMe of
            [] ->
                [];
            UserList when is_list(UserList) ->
                notice("List of length ~p~n", [length(UserList)]),
                N = rand:uniform(length(UserList)),
                lists:nth(N, UserList)
        end,
    notice("picking users I chose ~p~n", [Selected]),
    Selected.

notice_change(NewState) ->
    lager:info("[~p] -> ~p ~n", [self(), NewState]).

notice(Msg, Args) when is_list(Msg) ->
    lager:info("[~p] "++Msg, [self()]++Args).

unexpected(Msg, State) ->
    lager:info("[~p] [UNEXPECTED] During ~p I got: ~p~n", [self(), State, Msg]).
    
set_available(MyUsername) ->
    notice("changing ~p to available~n", [self()]),
    ets:insert(?TABLE, ?TABLE_DATA(self(), ?AVAILABLE, MyUsername)).
set_not_available(MyUsername) ->
    notice("changing ~p to unavailable~n", [self()]),
    ets:insert(?TABLE, ?TABLE_DATA(self(), ?NOT_AVAILABLE, MyUsername)).

tell_user(ServerPid, Msg) when is_binary(Msg) ->
    gen_statem:cast(ServerPid, {send, Msg}).

check_presence_other_users() ->
    gen_statem:cast(self(), check_presence_other_users).

prepend_username(OtherUsername, Msg) ->
    NewMsg = OtherUsername++" says: "++binary:bin_to_list(Msg),
    binary:list_to_bin(NewMsg).

send_disconnect_operator(Pid, Event) ->
    case operator_sup:disconnect([Pid, ?CALL_TIMEOUT]) of
        timeout -> 
            unexpected(Event, operator);
        Ans -> 
            notice(" Disconnection successful: ~p", [Ans]),
            Ans
    end.