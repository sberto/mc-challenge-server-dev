-module(automatron_fsm).

-behaviour(gen_statem).

-include("tables.hrl").

-define(NAME, automatron).

-export([start_link/1]).
-export([init/1, callback_mode/0, terminate/3, code_change/4]).
-export([list_options/3, operator/3, waiting_for_chat/3, chat/3]). %% states

-record(data,
        {server_pid,
         username,
         unique :: atom(),
         timeout :: integer(),
         msg_max :: integer(),
         msg_current :: integer(),
         other_user :: pid()}).

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

init([Data = #data{}]) ->
    ?SET_NOT_AVAILABLE(self()),
    {ok, list_options, Data}.

callback_mode() ->
    state_functions.

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
    ?SET_AVAILABLE(self()),
    {next_state,
     waiting_for_chat,
     Data,
     [{state_timeout, Timeout * 1000, []}, {reply, From,  waiting_for_chat_msg()}]};
list_options(cast, _Msg, Data) ->
    lager:info("automatron received a cast message."),
    {next_state, list_options, Data};
list_options({call, From}, Msg, Data) ->
    lager:info("automatron could not recognize the call with message ~p", [Msg]),
    {keep_state, Data, {reply, From, <<"bad_msg">>}}.

waiting_for_chat({call, From}, connect, Data) ->
    case pick_user() of
        false -> {keep_state, Data, {reply, From, <<"Please wait.">>}};
        UserPid -> % TODO ack
            ?SET_NOT_AVAILABLE(self()),
            NewData = Data#data{other_user = UserPid},
            {next_state, chat, NewData, {reply, From,  chat_msg(UserPid)}}
    end;
waiting_for_chat({call, From}, _Msg, Data) ->
    {keep_state, Data, {reply, From, waiting_for_chat_msg()}};
waiting_for_chat(state_timeout, [], Data = #data{server_pid = ServerPid}) ->
        ?SET_NOT_AVAILABLE(self()),
        gen_statem:cast(ServerPid, {send, waiting_timeout_msg(Data#data.username)}),
        {next_state, list_options, Data}.
    
chat({call, From}, {user_request, <<"bye">>}, Data) ->
    ?SET_AVAILABLE(self()),
    {keep_state, Data, {next_state, list_options, Data}};
chat({call, From}, {user_request, Msg}, Data) ->
    {keep_state, Data, {reply, From, Msg}}.
        
operator({call, From},
         {user_request, _Msg},
         Data = #data{msg_current = MsgCounter, msg_max = Max})
    when MsgCounter == 0 ->
    {next_state,
     list_options,
     Data#data{msg_current = Max},
     {reply, From, operator_timeout_msg(Data#data.username)}};
operator({call, From},
         {user_request, Msg},
         Data = #data{server_pid = ServerPid, msg_current = MsgCounter})
    when is_binary(Msg) ->
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
    gen_statem:cast(ServerPid, {send, operator_timeout_msg(Data#data.username)}),
    {next_state, list_options, Data}.

terminate(_Reason, _State, _Data) ->
    ok.

code_change(_Vsn, State, Data, _Extra) ->
    {ok, State, Data}.

%% Private

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

-spec welcome_msg(User :: list()) -> binary().
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

pick_user() ->
    UserList = ets:select(?TABLE, ets:fun2ms(fun({K, V}) when V =:= ?AVAILABLE -> K end)),
    N = rand:uniform(length(UserList)),
    lists:nth(N, UserList).



chat_msg(User) ->
    Msg = "You are now connected with " ++ User ++ "!~n" ++ "Stop by replying bye.",
    erlang:list_to_binary(Msg).

waiting_for_chat_msg() ->
    Msg = "Please wait for another user to be available.",
    erlang:list_to_binary(Msg).
