-module(sockserv_SUITE).
-include_lib("common_test/include/ct.hrl").
-export([all/0, groups/0, init_per_group/2, end_per_group/2, init_per_testcase/2, end_per_testcase/2]).
-export([client_only_access/1, client/1, client_random_name/1]).

all() -> [{group, multiple_access_one_username}].

groups() -> [{multiple_access_one_username,
              [{repeat, 10}],
              [client_only_access]}].

init_per_group(_, Config) ->
    Config.

end_per_group(_, _Config) ->
    ok.

init_per_testcase(_, Config) ->
    ok = sockclient:connect(),
    Config.

end_per_testcase(client_only_access, Config) ->
    Config;
end_per_testcase(_, Config) ->
    ok = sockclient:disconnect(),
    Config.

client_only_access(_Config) ->
    ok = sockclient:send_create_session("User").

client(_Config) ->
    ok = sockclient:send_create_session("User").

client_random_name(_Config) ->
    ok = sockclient:send_create_session(ref_to_list(make_ref())).