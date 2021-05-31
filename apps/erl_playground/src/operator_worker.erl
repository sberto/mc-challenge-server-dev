-module(operator_worker).
-behaviour(gen_server).
-behaviour(poolboy_worker).

-export([start_link/1]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2,
         code_change/3]).
-record(state, 
    {
        client = undefined
    }).

%%%%%%%%%%%%%%%
%% STATES
%%%%%%%%%%%%%%%

handle_call(connect, _From = {Pid, _Ref}, State = #state{client = undefined}) ->
    {reply, ok, State#state{client = Pid}};
handle_call(disconnect, _From = {Pid, _Ref}, State = #state{client = Pid}) ->
    {reply, ok, State#state{client = undefined}};
handle_call({answer, N}, _From = {Pid, _Ref}, State=#state{client=Pid}) when is_number(N) ->
    Ans = case N rem 2 of
            0 -> "even";
            1 -> "odd"
    end,
    {reply, binary:list_to_bin(Ans), State};
handle_call({answer, _Question}, _From = {Pid, _Ref}, State=#state{client=Pid}) ->
    Ans = "My pid is "++pid_to_list(self()),
    {reply, binary:list_to_bin(Ans), State};
handle_call(Msg,From,State) ->
    _ = lager:info("[~p] Operator received ~p from ~p~n", [self(),Msg,From]),
    {noreply, State}.
    
%%%%%%%%%%%%%%%
%% REQUIRED
%%%%%%%%%%%%%%%

start_link(Args) ->
    gen_server:start_link(?MODULE, Args, []).

init(_Args) ->
    {ok, #state{}}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, #state{}) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
    