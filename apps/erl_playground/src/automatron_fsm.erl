-module(automatron_fsm).

-behaviour(gen_statem).

-include("tables.hrl").

-define(NAME, automatron).

-export([start_link/1]).
-export([init/1, callback_mode/0, terminate/3, code_change/4]).
-export([list_options/3, operator/3, idle/3, idle_wait/3, chat/3]). %% states

-record(data,
        {server_pid,
         username,
         unique :: atom(),
         timeout :: integer(),
         msg_max :: integer(),
         msg_current :: integer(),
         other_user :: pid(),
         monitor :: reference()}).

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

%%%%%%%%%%%%%%%%%%%%%%%%%
%% Client-Client API   %%
%% All calls are async %%
%% to avoid deadlocks  %%
%%%%%%%%%%%%%%%%%%%%%%%%%

%% Ask the other statem for chat session
ask_chat(OtherPid, OwnPid) ->
    gen_statem:cast(OtherPid, {ask_chat, OwnPid}).

%% Forward the client message accepting the transaction
accept_chat(OtherPid, OwnPid) ->
    gen_statem:cast(OtherPid, {accept_chat, OwnPid}).

%%%%%%%%%%%%%%%%%%%%%%%%%
%% gen_statem required %%
%%%%%%%%%%%%%%%%%%%%%%%%%

init([Data = #data{}]) ->
    set_not_available(self()),
    {ok, list_options, Data}.

callback_mode() ->
    state_functions.

terminate(_Reason, _State, _Data) ->
    ok.

code_change(_Vsn, State, Data, _Extra) ->
    {ok, State, Data}.

%%%%%%%%%%%%
%% States %%
%%%%%%%%%%%%

list_options({call, From}, {user_request, <<"0">>}, Data) ->
    {keep_state, Data, {reply, From, welcome_msg(Data#data.username)}};
list_options({call, From}, {user_request, <<"1">>}, Data) ->
    {keep_state, Data, {reply, From, get_joke()}};
list_options({call, From}, {user_request, <<"2">>}, Data) ->
    {keep_state, Data, {reply, From, atom_to_binary(Data#data.unique)}};
list_options({call, From}, {user_request, <<"3">>}, Data = #data{timeout = Timeout}) ->
    {next_state,
     operator,
     Data,
     [{state_timeout, Timeout * 1000, []}, {reply, From, operator_msg()}]};
list_options({call, From}, {user_request, <<"4">>}, Data = #data{timeout = Timeout}) ->
    notice_change(idle),
    set_available(self()),
    check_presence_other_users(),
    {next_state,
     idle,
     Data,
     [{state_timeout, Timeout * 10 * 1000, []}, {reply, From,  waiting_for_chat_msg()}]};
list_options(cast, check_presence_other_users, Data) ->
    {keep_state, Data, [postpone]};
list_options(cast, _Msg, Data) ->
        lager:info("automatron received a cast message."),
        {next_state, list_options, Data};
list_options({call, From}, Msg, Data) ->
    lager:info("automatron could not recognize the call with message ~p", [Msg]),
    {keep_state, Data, {reply, From, <<"bad_msg">>}};
list_options(_, Event, Data) ->
    unexpected(Event, idle),
    {keep_state, Data}.

idle(cast, {ask_chat, OtherPid}, D = #data{timeout = Timeout}) ->
    Ref = monitor(process, OtherPid),
    notice_change(idle_wait),
    accept_chat(OtherPid, self()),
    {next_state, idle_wait, D#data{other_user = OtherPid, monitor = Ref}, [{state_timeout, Timeout * 1000, []}]};
idle(cast, check_presence_other_users, Data = #data{timeout = Timeout}) ->
    case pick_user() of
        [] -> {keep_state, Data};
        OtherPid -> 
            set_not_available(self()),
            ask_chat(OtherPid, self()),
            notice("asking user ~p to chat~n", [OtherPid]),
            Ref = monitor(process, OtherPid),
            {next_state, idle_wait, Data#data{other_user = OtherPid, monitor = Ref}, [{state_timeout, Timeout * 1000, []}]}
    end;
idle(cast, Event, Data) ->
    unexpected(Event, idle),
    {keep_state, Data};
idle({call, From}, _Msg, Data) ->
    {keep_state, Data, {reply, From, waiting_for_chat_msg()}};
idle(state_timeout, [], Data = #data{server_pid = ServerPid}) ->
    set_not_available(self()),
    tell_user(ServerPid, waiting_timeout_msg(Data#data.username)),
    {next_state, list_options, Data};
idle(_, Event, Data) ->
    unexpected(Event, idle),
    {keep_state, Data}.
    

%% idle_wait allows to expect replies from the other side and start chat 
idle_wait(cast, {ask_chat, OtherPid}, D = #data{other_user = OtherPid}) ->
    accept_chat(OtherPid, self()),
    notice_change(chat),
    {next_state, chat, D};
idle_wait(cast, {accept_chat, OtherPid}, D = #data{other_user = OtherPid}) ->
    accept_chat(OtherPid, self()),
    notice_change(chat),
    {next_state, chat, D};
idle_wait(cast, Event, Data) ->
    unexpected(Event, idle_wait),
    {keep_state, Data};
idle_wait({call, _From}, Event, Data) ->
    unexpected(Event, idle_wait),
    {keep_state, Data};
idle_wait(state_timeout, [], Data = #data{server_pid = ServerPid}) ->
    set_not_available(self()),
    tell_user(ServerPid, waiting_timeout_msg(Data#data.username)),
    {next_state, list_options, Data};
idle_wait(_, Event, Data) ->
    unexpected(Event, idle_wait),
    {keep_state, Data}.





chat(cast, {accept_chat, OtherPid}, Data = #data{other_user = OtherPid}) ->
    {keep_state, Data};
chat(cast, {user_request, Msg = <<"bye">>}, Data) ->
    notice("Ricevuto messaggio \"~p\"~n", [Msg]),
    notice_change(list_options),
    {next_state, list_options, Data};
chat(cast, {tell_user, Msg}, Data=#data{server_pid = ServerPid}) when is_binary(Msg) ->
    tell_user(ServerPid, Msg),
    {keep_state, Data};
chat({call, From}, {user_request, Msg = <<"bye">>}, Data = #data{other_user=OtherPid}) ->
    set_available(self()),
    notice("Invio messaggio \"~p\"~n", [Msg]),
    gen_statem:cast(OtherPid, {tell_user, Msg}),
    notice_change(list_options),
    {next_state, list_options, Data, {reply, From, silent}}; 
chat({call, From}, {user_request, Msg}, Data = #data{other_user=OtherPid}) ->
    notice("Invio messaggio \"~p\" a ~p~n", [Msg, OtherPid]),
    gen_statem:cast(OtherPid, {tell_user, Msg}),
    {keep_state, Data, {reply, From, silent}};
chat(Event, _Msg, Data) ->
    unexpected(Event, chat),
    {keep_state, Data};
chat(_, Event, Data) ->
    unexpected(Event, chat),
    {keep_state, Data}.
    









        
operator({call, From},
         {user_request, _Msg},
         Data = #data{msg_current = MsgCounter, msg_max = Max})
    when MsgCounter == 0 ->
    {next_state, list_options, Data#data{msg_current = Max}, {reply, From, operator_timeout_msg(Data#data.username)}};
operator({call, From}, {user_request, Msg}, Data = #data{server_pid = ServerPid, msg_current = MsgCounter}) when is_binary(Msg) ->
    Str = erlang:binary_to_list(Msg),
    Answer =
        case string:to_integer(Str) of
            {error, no_integer} ->
                list_to_binary(pid_to_list(ServerPid));
            {N, _Rest} ->
                if N rem 2 =:= 0 ->
                       <<"even">>;
                   true ->
                       <<"odd">>
                end
        end,
    {keep_state, Data#data{msg_current = MsgCounter - 1}, {reply, From, Answer}};
operator(state_timeout, [], Data = #data{server_pid = ServerPid}) ->
    tell_user(ServerPid, operator_timeout_msg(Data#data.username)),
    {next_state, list_options, Data};
operator(_, Event, Data) ->
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
    List =
        "WELCOME "
        ++ User
        ++ " TO AUTOMATRON CALL CENTER~n1 - Press 1 to receive the jokes "
           "of the day.~n2 - Press 2 to know your unique identifier for "
           "the call.~n3 - Press 3 to talk to an operator.~n0 - Press 0 "
           "to listen to this message again.",
    list_to_binary(List).

operator_msg() ->
    <<"I am the operator, how can I help you?">>.

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

chat_msg(User) ->
    Msg = "You are now connected with " ++ User ++ "!~n" ++ "Stop by replying bye.",
    erlang:list_to_binary(Msg).

waiting_for_chat_msg() ->
    Msg = "Please wait for another user to be available.",
    erlang:list_to_binary(Msg).

%%%%%%%%%%%%%%%%%
%%  Utilities  %%
%%%%%%%%%%%%%%%%%
pick_user() ->
    % Query = [{{'$1','$2'}, [{'andalso',{'=:=','$2',?AVAILABLE},{'=/=','$1',{self()}}}], ['$1']}],
    Query = [{{'$1','$2'}, [{'=:=','$2',?AVAILABLE}], ['$1']}],
    notice("QUERIED TABLE: ~p~n", [ets:select(?TABLE, Query)]),
    Selected = 
        case lists:delete(self(),ets:select(?TABLE, Query)) of
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
    io:format("[~p] -> ~p ~n", [self(), NewState]).

notice(Msg, Args) when is_list(Msg) ->
    io:format("[~p] "++Msg, [self()]++Args).

unexpected(Msg, State) ->
    io:format("[~p] [UNEXPECTED] During ~p I got: ~p~n", [self(), State, Msg]).
    
set_available(User) ->
    notice("changing ~p to available~n", [User]),
    ets:insert(?TABLE, ?TABLE_DATA(User, ?AVAILABLE)).
set_not_available(User) ->
    notice("changing ~p to unavailable~n", [User]),
    ets:insert(?TABLE, ?TABLE_DATA(User, ?NOT_AVAILABLE)).

tell_user(ServerPid, Msg) when is_binary(Msg) ->
    gen_statem:cast(ServerPid, {send, Msg}).

check_presence_other_users() ->
    gen_statem:cast(self(), check_presence_other_users).
    