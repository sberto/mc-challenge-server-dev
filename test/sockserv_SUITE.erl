-module(sockserv_SUITE).

-include_lib("common_test/include/ct.hrl").
-include_lib("tables.hrl").
-include_lib("statem.hrl").

-define(REPEAT, 10).

%% Note: This directive should only be used in test suites.
-compile(export_all).

all() ->    [
             {group, group_multiple_access_one_username},
             {group, group_welcome_list_selection_and_availability},
             {group, group_3_users_operator_connection},
             {group, group_4_users_operator_connection},
             {group, group_2_users_chat},
             {group, group_2_users_chat_disconnection},
             {group, group_3_users_chat}
            ].

%%%%%%%%%%%%
%% GROUPS %%
%%%%%%%%%%%%

groups() ->
    [
        {group_multiple_access_one_username, [], [{group, subgroup_multiple_access_one_username}, check_no_user_left]},
        {subgroup_multiple_access_one_username, [parallel], [client_1_access, client_1_access, client_1_access]},
      
        {group_welcome_list_selection_and_availability, [sequence], [client_wl_joke, client_wl_id, client_wl_operator, client_wl_chat]},

        {group_3_users_operator_connection,	     [sequence, {repeat, ?REPEAT}],						[client_3_operator_connection]},
        {group_4_users_operator_connection,	     [sequence, {repeat, ?REPEAT}],						[client_4_operator_connection]},
        {group_2_users_chat,					[parallel, {repeat, ?REPEAT}],		[client_2_chat]},
        {group_2_users_chat_disconnection,	[parallel, {repeat, ?REPEAT}],		[client_2_chat_disconnection]},
        {group_3_users_chat,					[sequence, {repeat, ?REPEAT}],		[client_3_chat]}
    ].

init_per_group(_, Config) -> Config.

end_per_group(_, _Config) -> 
    ok.

%%%%%%%%%%%%%%%
%% TESTCASES %%
%%%%%%%%%%%%%%%

%% setup
init_per_testcase(client_wl_joke, Config) ->
    [ {user_msg, ?WL_JOKE} | config_connection(Config)];
init_per_testcase(client_wl_id, Config) ->
    [ {user_msg, ?WL_ID} | config_connection(Config)];
init_per_testcase(client_wl_operator, Config) ->
    [ {user_msg, ?WL_OPERATOR} | config_connection(Config)];
init_per_testcase(client_wl_chat, Config) ->
    [ {user_msg, ?WL_CHAT} | config_connection(Config)];
init_per_testcase(client_1_access, Config) ->
    config_connection_no_create_session(Config);
init_per_testcase(check_no_user_left, Config) ->
    Config;
init_per_testcase(_, Config) ->
    config_connection(Config).

end_per_testcase(client_1_access, Config) -> Config;
end_per_testcase(check_no_user_left, Config) ->
    Config;
end_per_testcase(_, Config) ->
    ok = sockclient:disconnect(?config(my_pid, Config)),
    exit(whereis(operator_pool), kill),
    Config.

%% tests
client_1_access(Config) ->
    ok = sockclient:send_create_session(?config(my_pid, Config), "User").

client_wl_joke(Config) ->
    check_client_no_chat(Config).
client_wl_id(Config) ->
    check_client_no_chat(Config).
client_wl_operator(Config) ->
    check_client_no_chat(Config).
client_wl_chat(Config) ->
    check_client_chat(Config).

client_3_operator_connection(Config) ->
    timer:sleep(1),
	Pids = [?config(my_pid, Config), create_client(), create_client()],
	[ send_user_request(Pid, ?WL_OPERATOR) || Pid <- Pids ],
	[?STATE_OPERATOR, ?STATE_OPERATOR, ?STATE_OPERATOR] = [ get_state(Pid) || Pid <- Pids ].

client_4_operator_connection(Config) ->
    timer:sleep(1),
    Pids = [?config(my_pid, Config), create_client(), create_client(), create_client()],
    [ send_user_request(Pid, ?WL_OPERATOR) || Pid <- Pids ],
    StateList = [ get_state(Pid) || Pid <- Pids ],
    check_unordered([?STATE_OPERATOR, ?STATE_OPERATOR, ?STATE_OPERATOR, ?STATE_WELCOME_LIST], StateList).
        
client_2_chat(Config) ->
	Pids = [?config(my_pid, Config), create_client()],
	[ send_user_request(Pid, ?WL_CHAT) || Pid <- Pids ],
    [?STATE_CHAT, ?STATE_CHAT] = [ get_state(Pid) || Pid <- Pids ].

client_2_chat_disconnection(Config) ->
	Pids = [?config(my_pid, Config), create_client()],
	[ send_user_request(Pid, ?WL_CHAT) || Pid <- Pids ],
    timer:sleep(1),
    StateListBefore = [ get_state(Pid) || Pid <- Pids ],
    [?STATE_CHAT, ?STATE_CHAT] = StateListBefore,
    sockclient:disconnect(hd(Pids)),
    StateList = [ get_state(Pid) || Pid <- Pids ],
    timer:sleep(1),
    [?STATE_DISCONNECTED, OtherState] = StateList,
    true = lists:member(OtherState, [?STATE_IDLE, ?STATE_IDLE_WAIT, ?STATE_WELCOME_LIST]).
    
client_3_chat(Config) ->
	Pids = [?config(my_pid, Config), create_client(), create_client()],
	[ send_user_request(Pid, ?WL_CHAT) || Pid <- Pids ],
    timer:sleep(1),
	StateList = [ get_state(Pid) || Pid <- Pids ],
    timer:sleep(1),
    check_unordered([?STATE_CHAT, ?STATE_CHAT, ?STATE_IDLE], StateList).

%%%%%%%%%%%%%%%%
%% API CLIENT %%
%%%%%%%%%%%%%%%%

create_client() ->
	Pid = create_and_connect(),
	ok = sockclient:send_create_session(Pid, ref_to_list(make_ref())),
	Pid.
	
create_and_connect() ->
	{ok, Pid} = sockclient:start_link(),
	ok = sockclient:connect(Pid),
	Pid.

config_connection(Config) ->
    NewConfig = config_connection_no_create_session(Config),
    ok = sockclient:send_create_session(?config(my_pid, NewConfig), ref_to_list(make_ref())),
    NewConfig.

config_connection_no_create_session(Config) ->
    [{my_pid, create_and_connect()} |Config].

send_user_request(Pid, Req) ->
	ok = sockclient:send_user_request(Pid, Req).

send_user_request_from_config(Config) ->
	ok = sockclient:send_user_request(?config(my_pid, Config), ?config(user_msg, Config)).
	
check_client_no_chat(Config) ->
    check_available_not_available(0, 1),
    send_user_request_from_config(Config),
    check_available_not_available(0, 1).

check_client_chat(Config) ->
    check_available_not_available(0, 1),
    send_user_request_from_config(Config),
    check_available_not_available(1, 0).

%%%%%%%%%%%%%%%
%% FUNCTIONS %%
%%%%%%%%%%%%%%%

check_available_not_available(A, NA) ->
    timer:sleep(1),
    {A, NA} = {available_list_size(), not_available_list_size()}.

available_list_size() ->
    length(available_list()).
    
available_list() ->
    Query = [{{'$1','$2', '$3'}, [{'=:=','$2',?AVAILABLE}], [['$1']]}],
    List = ets:select(?TABLE, Query),
    io:format('available_list = ~p~n', [List]),
    List.

not_available_list_size() ->
    length(not_available_list()).
not_available_list() ->
    Query = [{{'$1','$2', '$3'}, [{'=:=','$2',?NOT_AVAILABLE}], [['$1']]}],
    List = ets:select(?TABLE, Query),
    io:format('not_available_list = ~p~n', [List]),
    List.

check_no_user_left(Config) ->
	[{id, _},
	{decentralized_counters, _},
	{read_concurrency, _},
	{write_concurrency, _},
	{compressed, _},
	{memory, _},
	{owner, _},
	{heir, _},
	{name, _},
	{size, Size},
	{node, _},
	{named_table, _},
	{type, _},
	{keypos, _},
    {protection, _}] = ets:info(?TABLE),
    Size =:= 0.

check_unordered(MyList, ListToCompare) ->
    io:format('check_unordered: ~p =:= ~p', [MyList, ListToCompare]),
    true = sets:from_list(MyList) =:= sets:from_list(ListToCompare).

automatron_pid(MyPid) ->
    gen_server:call(MyPid, {test_query, automatron_pid}).

get_state(Pid) ->
    timer:sleep(1),
    AutomatronPid = automatron_pid(Pid),
    case AutomatronPid =:= ?STATE_DISCONNECTED of 
        true ->
            ?STATE_DISCONNECTED;
        false ->
            gen_statem:call(AutomatronPid, {test_query, state_name})
    end.
    