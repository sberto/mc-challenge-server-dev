-module(sockserv_SUITE).

-include_lib("common_test/include/ct.hrl").
-include_lib("tables.hrl").

%% Note: This directive should only be used in test suites.
-compile(export_all).

all() -> [{group, multiple_access_one_username}].

groups() ->
    [{multiple_access_one_username,
      [],
      [{group, parallel_access_one_username}, check_no_user_left]},
     {parallel_access_one_username,
      [parallel],
      [client_only_access, client_only_access, client_only_access]}].


init_per_group(_, Config) -> Config.

end_per_group(_, _Config) -> ok.

init_per_testcase(_, Config) ->
    {ok, Pid} = sockclient:start_link(),
    ok = sockclient:connect(Pid),
    [{my_pid, Pid} |Config].

end_per_testcase(client_only_access, Config) -> Config;
end_per_testcase(_, Config) ->
    ok = sockclient:disconnect(?config(my_pid, Config)),
    Config.

client_only_access(Config) ->
    ok = sockclient:send_create_session(?config(my_pid, Config), "User").

client(Config) ->
    ok = sockclient:send_create_session(?config(my_pid, Config), "User").

client_random_name(_Config) ->
    ok =
        sockclient:send_create_session(ref_to_list(make_ref())).

check_no_user_left(_Config) ->
    [{id, _},
     {decentralized_counters, _},
     {read_concurrency, _},
     {write_concurrency, _},
     {compressed, _},
     {memory, _},
     {owner, _},
     {heir, _},
     {name, _},
     {size, 0},
     {node, _},
     {named_table, _},
     {type, _},
     {keypos, _},
     {protection, _}] =
        ets:info(?TABLE).
