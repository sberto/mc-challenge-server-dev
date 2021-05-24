-module(sockserv_SUITE).
-include_lib("common_test/include/ct.hrl").
-export([all/0, groups/0, init_per_group/2, end_per_group/2]).
-export([client_a/1, client_b/1]).

all() -> [{group, session}].

groups() -> [{session,
              [],
              [{group, clients}]},
             {clients,
              [parallel, {repeat, 3}],
              [client_a, client_b]}].

init_per_group(session, Config) ->
    % sockserv:start(),
    Config;
init_per_group(_, Config) ->
    Config.

end_per_group(_, _Config) ->
    ok.

client_a(_Config) ->
    sockclient:connect(),
    sockclient:send_create_session("User").

client_b(_Config) ->
    sockclient:connect(),
    sockclient:send_create_session("User").