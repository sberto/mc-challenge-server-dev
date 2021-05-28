-module(sockclient).
-behaviour(gen_server).

-include("erl_playground_pb.hrl").

%% ------------------------------------------------------------------
%% API Function Exports
%% ------------------------------------------------------------------

-export([start_link/0]). -ignore_xref([{start_link, 4}]).
-export([connect/1, disconnect/1]).
-export([send_create_session/2, send_user_request/2]).

%% ------------------------------------------------------------------
%% gen_server Function Exports
%% ------------------------------------------------------------------

-export([init/1, terminate/2, code_change/3]).
-export([handle_call/3, handle_cast/2, handle_info/2]).

%% ------------------------------------------------------------------
%% Record Definitions
%% ------------------------------------------------------------------

-record(state, {
    socket :: any(),
    test_query_ref
}).
-type state() :: #state{}.

%% ------------------------------------------------------------------
%% Macro Definitions
%% ------------------------------------------------------------------

-define(SERVER, default).
-define(CB_MODULE, ?MODULE).

%% ------------------------------------------------------------------
%% API Function Exports
%% ------------------------------------------------------------------

start_link() ->
    {ok, _} = gen_server:start_link({local, list_to_atom(ref_to_list(make_ref()))}, ?CB_MODULE, [], []).

-spec connect(Pid :: pid()) -> ok.
connect(Pid) when is_pid(Pid) ->
    gen_server:call(Pid, connect),
    ok.

-spec disconnect(Pid :: pid()) -> ok.
disconnect(Pid) when is_pid(Pid) ->
    gen_server:call(Pid, disconnect),
    ok.

-spec send_create_session(Pid :: pid(), UsernameList :: list()) -> ok.
send_create_session(Pid, UsernameList) when is_list(UsernameList) andalso is_pid(Pid) ->
    Username = list_to_binary(UsernameList),
    CreateSession = #create_session {
        username = Username
    },
    gen_server:cast(Pid, {create_session, CreateSession}).

send_user_request(Pid, Num) when is_number(Num) andalso is_pid(Pid) ->
    UserReq = #user_request {
        message = list_to_binary(integer_to_list(Num))
    },
    gen_server:cast(Pid, {user_request, UserReq});
send_user_request(Pid, Str) when is_list(Str) andalso is_pid(Pid) ->
    UserReq = #user_request {
        message = list_to_binary(Str)
    },
    gen_server:cast(Pid, {user_request, UserReq});
send_user_request(_, _) ->
    bad_argument.

%% ------------------------------------------------------------------
%% gen_server Function Definitions
%% ------------------------------------------------------------------

%% This function is never called. We only define it so that
%% we can use the -behaviour(gen_server) attribute.
init(_ARgs) ->
    lager:info("sockclient init'ed with pid ~p", [self()]),
    {ok, #state{}}.

handle_cast({create_session, CreateSession}, #state{socket = Socket} = State)
    when Socket =/= undefined ->
    Req = #req {
        type = create_session,
        create_session_data = CreateSession
    },
    Data = utils:add_envelope(Req),

    gen_tcp:send(Socket, Data),

    {noreply, State};
handle_cast({user_request, Msg}, #state{socket = Socket} = State)
    when Socket =/= undefined ->
    Req = #req {
        type = user_request,
        user_request_data = Msg
    },
    Data = utils:add_envelope(Req),

    gen_tcp:send(Socket, Data),

    {noreply, State};
handle_cast(Message, State) ->
    _ = lager:warning("No handle_cast for ~p", [Message]),
    {noreply, State}.

handle_info({tcp_closed, _Port}, State) ->
    {noreply, State#state{socket = undefined}};
handle_info({tcp, _Port, Packet}, State) ->
    Req = utils:open_envelope(Packet),
    State = process_packet(Req, State, utils:unix_timestamp()),
    {noreply, State};
handle_info(Message, State) ->
    _ = lager:warning("No handle_info for~p", [Message]),
    {noreply, State}.

handle_call(_Msg = {test_query, automatron_pid}, From, #state{socket = Socket} = State) when Socket =/= undefined ->
    Req = #req {
        type = test_pid
    },
    Data = utils:add_envelope(Req),

    gen_tcp:send(Socket, Data),
    {noreply, State#state{socket = Socket, test_query_ref = From}};
handle_call(connect, _From, State) ->
    {ok, Host} = application:get_env(erl_playground, tcp_host),
    {ok, Port} = application:get_env(erl_playground, tcp_port),

    {ok, Socket} = gen_tcp:connect(Host, Port, [binary, {packet, 2}]),

    {reply, normal, State#state{socket = Socket}};
handle_call(disconnect, _From, #state{socket = Socket} = State)
    when Socket =/= undefined ->
    
    gen_tcp:shutdown(Socket, read_write),

    {reply, normal, State};
handle_call(Message, _From, State) ->
    _ = lager:warning("No handle_call for ~p", [Message]),
    {reply, normal, State}.

terminate(Reason, _State) ->
    _ = lager:notice("terminate ~p", [Reason]),
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% ------------------------------------------------------------------
%% Internal Function Definitions
%% ------------------------------------------------------------------

-spec process_packet(Req :: #req{}, State :: state(), Now :: integer()) -> NewState :: state().
process_packet(undefined, State, _Now) ->
    lager:notice("server sent invalid packet, ignoring"),
    State;
process_packet(#req{ type = Type } = Req, State = #state{test_query_ref = ToReplyTo}, _Now)
    when Type =:= test_pid ->
    #req{
        test_pid_data = #test_pid{
            pid = Pid
        }
    } = Req,
    
    gen_server:reply(ToReplyTo, erlang:list_to_pid(erlang:binary_to_list(Pid))),
    _ = lager:info("automatron pid: ~p", [Pid]),
    State;
process_packet(#req{ type = Type } = Req, State, _Now)
    when Type =:= server_message ->
    #req{
        server_message_data = #server_message{
            message = Message
        }
    } = Req,
    _ = lager:info("server_message received: ~p", [Message]),
    State.
