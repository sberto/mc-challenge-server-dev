-module(automatron_fsm).

-behaviour(gen_statem).

-define(NAME, automatron).

-export([start_link/1]).
-export([init/1, callback_mode/0, terminate/3, code_change/4]).
-export([list_options/3, operator/3]). %% states
% -export([call/1, start/0]). %% DEBUG API

-record(data, {server_pid, username, timeout :: integer(), msg_max :: integer(), msg_current :: integer()}).

% %% DEBUG API %%
% call(Msg) ->
%     gen_statem:call(?NAME, {user_request, Msg}).

% start() ->
%     start_link([self(), unnamed_user]).

start_link([ServerPid, UserId]) ->
    start_link([ServerPid, UserId, 10, 3]);
start_link([ServerPid, UserId, TimeoutSecs, MsgMax]) ->
    gen_statem:start_link({local, ?NAME}, ?MODULE, [#data{server_pid = ServerPid, username = UserId, timeout=TimeoutSecs, msg_max=MsgMax, msg_current=MsgMax}], []).

init([Data = #data{}]) ->
    gen_statem:cast(?NAME, {user_request, 0}), %% ask for full list
    {ok, list_options, Data}.

callback_mode() ->
    state_functions.

list_options({call, From}, {user_request, <<"0">>}, Data) ->
    {keep_state, Data, {reply, From, welcome_msg()}};
list_options({call, From}, {user_request, <<"1">>}, Data) ->
    {keep_state, Data, {reply, From, get_joke()}};
list_options({call, From}, {user_request, <<"2">>}, Data) ->
    {keep_state, Data, {reply, From, Data#data.username}};
list_options({call, From}, {user_request, <<"3">>}, Data=#data{timeout=Timeout}) ->
    {next_state, operator, Data, [{state_timeout, Timeout*1000, []}, {reply, From, operator_msg()}]};
list_options(cast, _Msg, Data) ->
    lager:info("automatron received a cast message."),
    {next_state, list_options, Data};
list_options({call, From}, Msg, Data) ->
    lager:info("automatron could not recognize the call with message ~p", [Msg]),
    {keep_state, Data, {reply, From, <<"bad_msg">>}}.


operator({call, From}, {user_request, _Msg}, Data=#data{msg_current=MsgCounter, msg_max=Max}) when MsgCounter == 0 -> 
    {next_state, list_options, Data#data{msg_current=Max}, {reply, From, timeout_msg()}};
operator({call, From}, {user_request, Msg}, Data=#data{server_pid = ServerPid, msg_current=MsgCounter}) when is_binary(Msg) ->
    Str = erlang:binary_to_list(Msg),
    Answer = case string:to_integer(Str) of
        {error,no_integer} -> 
            list_to_binary(pid_to_list(ServerPid));
        {N,_Rest} -> 
            if N rem 2 =:= 0 ->  <<"even">>; 
                        true -> <<"odd">>
            end
        end,
    {keep_state, Data#data{msg_current=MsgCounter-1}, {reply, From, Answer}};

operator(state_timeout, [], Data = #data{server_pid = ServerPid}) ->
    gen_statem:cast(ServerPid, {send, timeout_msg()}),
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

welcome_msg() ->
    <<"WELCOME TO AUTOMATRON CALL CENTER~n"
    "1 - Press 1 to receive the jokes of the day.~n"
    "2 - Press 2 to know your unique identifier for the call.~n"
    "3 - Press 3 to talk to an operator.~n"
    "0 - Press 0 to listen to this message again.">>.

operator_msg() ->
    <<"I am the operator, how can I help you?">>.

timeout_msg() ->
    Msg = "Your time with the operator has finished. Goodbye!~n"++binary:bin_to_list(welcome_msg()),
    erlang:list_to_binary(Msg).