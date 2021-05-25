-module(sockserv).
-behaviour(gen_server).
-behaviour(ranch_protocol).

-include("erl_playground_pb.hrl").

%% ------------------------------------------------------------------
%% API Function Exports
%% ------------------------------------------------------------------

-export([start_link/4]). -ignore_xref([{start_link, 4}]).
-export([start/0]).

%% ------------------------------------------------------------------
%% gen_server Function Exports
%% ------------------------------------------------------------------

-export([init/1, terminate/2, code_change/3]).
-export([handle_call/3, handle_cast/2, handle_info/2]).

%% ------------------------------------------------------------------
%% ranch_protocol Function Exports
%% ------------------------------------------------------------------

-export([init/4]). -ignore_xref([{init, 4}]).

%% ------------------------------------------------------------------
%% Record Definitions
%% ------------------------------------------------------------------

-record(state, {
    socket :: any(), %ranch_transport:socket(),
    transport,
    automatron_pid :: pid()
}).
-type state() :: #state{}.

%% ------------------------------------------------------------------
%% Macro Definitions
%% ------------------------------------------------------------------

-define(SERVER, ?MODULE).
-define(CB_MODULE, ?MODULE).

%% ------------------------------------------------------------------
%% API Function Definition
%% ------------------------------------------------------------------

start() ->
    {ok, Port} = application:get_env(erl_playground, tcp_port),
    {ok, MaxConnections} = application:get_env(erl_playground, max_connections),

    TcpOptions = [
        {backlog, 100}
    ],

    {ok, _} = ranch:start_listener(
        sockserv_tcp,
        ranch_tcp,
        [{port, Port},
        {num_acceptors, 100}] ++ TcpOptions,
        sockserv,
        [none]
    ),

    ranch:set_max_connections(sockserv_tcp, MaxConnections),
    lager:info("server listening on tcp port ~p", [Port]),
    ok.

start_link(Ref, Socket, Transport, Opts) ->
    proc_lib:start_link(?MODULE, init, [Ref, Socket, Transport, Opts]).

%% ------------------------------------------------------------------
%% ranch_protocol Function Definitions
%% ------------------------------------------------------------------

init(Ref, Socket, Transport, [_ProxyProtocol]) ->
    lager:info("sockserv init'ed ~p",[Socket]),

    ok = proc_lib:init_ack({ok, self()}),
    ok = ranch:accept_ack(Ref),

    Opts = [{packet, 2}, {packet_size, 16384}, {active, once}, {nodelay, true}],
    _ = Transport:setopts(Socket, Opts),

    State = {ok, #state{
        socket = Socket,
        transport = Transport
    }},

    gen_server:enter_loop(?MODULE, [], State).

%% ------------------------------------------------------------------
%% gen_server Function Definitions
%% ------------------------------------------------------------------

%% This function is never called. We only define it so that
%% we can use the -behaviour(gen_server) attribute.
init([]) -> {ok, undefined}.

handle_cast({send, Msg}, State = {ok, #state{socket = Socket, transport = Transport}}) ->
    send_server_message(Msg, Transport, Socket),
    {noreply, State};
handle_cast(Message, State) ->
    _ = lager:notice("unknown handle_cast ~p", [Message]),
    {noreply, State}.

handle_info({tcp, _Port, <<>>}, State) ->
    _ = lager:notice("empty handle_info state: ~p", [State]),
    {noreply, State};
handle_info({tcp, _Port, Packet}, State = {ok, #state{socket = Socket}}) ->
    Req = utils:open_envelope(Packet),

    NewState = process_packet(Req, State, utils:unix_timestamp()),
    ok = inet:setopts(Socket, [{active, once}]),

    {noreply, NewState};
handle_info({tcp_closed, _Port}, State) ->
    {stop, normal, State};
handle_info(Message, State) ->
    _ = lager:notice("unknown handle_info ~p", [Message]),
    {noreply, State}.

handle_call(Message, _From, State) ->
    _ = lager:notice("unknown handle_call ~p", [Message]),
    {noreply, State}.

terminate(normal, _State) ->
    _ = lager:info("Goodbye!"),
    ok;
terminate(Reason, _State) ->
    _ = lager:notice("No terminate for ~p", [Reason]),
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% ------------------------------------------------------------------
%% Internal Function Definitions
%% ------------------------------------------------------------------

-spec process_packet(Req :: #req{}, State :: state(), Now :: integer()) -> NewState :: state().
process_packet(undefined, State, _Now) ->
    _ = lager:notice("client sent invalid packet, ignoring ~p",[State]),
    State;
process_packet(#req{ type = Type } = Req, _State = {ok, #state{socket = Socket, transport = Transport, automatron_pid = OldPids}}, _Now)
    when Type =:= create_session ->
    #req{
        create_session_data = #create_session {
            username = UserNameBin
        }
    } = Req,
    UserName = binary_to_list(UserNameBin),
    _ = lager:info("create_session received from ~p", [UserName]),
    
    {ok, AutomatronPid} = automatron_fsm:start_link([self(), UserName, Socket]),
    lager:info("automatron connected with Pid ~p", [AutomatronPid]),

    Answer = gen_statem:call(AutomatronPid, {user_request, <<"0">>}),

    send_server_message(Answer, Transport, Socket),
    {ok, #state{socket = Socket, transport = Transport, automatron_pid = AutomatronPid}};
process_packet(#req{ type = Type } = Req, State = {ok, #state{socket = Socket, transport = Transport, automatron_pid=AutomatronPid}}, _Now)
    when Type =:= user_request ->
    #req{
        user_request_data = #user_request {
            message = Message
        }
    } = Req,
    
    Answer = gen_statem:call(AutomatronPid, {user_request, Message}),
    send_server_message(Answer, Transport, Socket),
    State.


send_server_message(Msg, Transport, Socket) when is_binary(Msg) ->
    Req = #req{
        type = server_message,
        server_message_data = #server_message {
            message = Msg
        }
    },
    Data = utils:add_envelope(Req),
    Transport:send(Socket,Data);
send_server_message(silent, _Transport, _Socket) ->
        ok;
send_server_message(Msg, _Transport, _Socket) ->
    lager:info("[WARN] received ~p in send_server_message", [Msg]).