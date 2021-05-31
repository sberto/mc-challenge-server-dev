-module(operator_sup).

-behaviour(supervisor).

-export([init/1]).
-export([start_link/0]).
% API
-export([connect/1, ask/2, disconnect/1]).

%%%%%%%%%%%%%%%
%% API
%%%%%%%%%%%%%%%

connect(Timeout) ->
    case catch poolboy:checkout(operator_pool, true, Timeout) of 
        {'EXIT', {timeout, _}} -> timeout;
        Pid -> 
            case catch gen_server:call(Pid, connect, Timeout) of 
                {'EXIT', {timeout, _}} -> timeout;
                _ -> Pid
            end
    end.

ask(Request, [Pid, Timeout]) ->
    case catch gen_server:call(Pid, {answer, Request}, Timeout) of 
        {'EXIT', {timeout, _}} -> timeout;
        Ans -> Ans
    end.
    
disconnect([Pid, Timeout]) ->
    case catch gen_server:call(Pid, disconnect, Timeout) of 
                {'EXIT', {timeout, _}} -> timeout;
                _ -> 
                    case catch poolboy:checkin(operator_pool, Pid) of 
                        {'EXIT', {timeout, _}} -> timeout;
                        Ans -> Ans
                    end
    end.
    
%%%%%%%%%%%%%%%
%% REQUIRED
%%%%%%%%%%%%%%%

start_link() ->
    Pid = supervisor:start_link({global, operator_sup}, ?MODULE, []),
    lager:info("[~p] operator pool started", [Pid]),
    Pid.

init([]) ->
    {ok, Pools} = application:get_env(erl_playground, pools),
    PoolSpecs =
        lists:map(fun({Name, SizeArgs, WorkerArgs}) ->
                     PoolArgs =
                         [{name, {local, Name}}, {worker_module, operator_worker}] ++ SizeArgs,
                     poolboy:child_spec(Name, PoolArgs, WorkerArgs)
                  end,
                  Pools),
    {ok, {{one_for_one, 10, 10}, PoolSpecs}}.
