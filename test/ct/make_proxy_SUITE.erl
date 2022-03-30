-module(make_proxy_SUITE).

-include("make_proxy_ct.hrl").

-compile(export_all).

all() ->
    [handle].

init_per_suite(Config) ->
    {ok, _} = application:ensure_all_started(make_proxy),
    new_meck(),
    Config.

end_per_suite(Config) ->
    del_meck(),
    application:stop(make_proxy),
    Config.

init_per_testcase(_Case, Config) ->
    Config.

end_per_testcase(_Case, _Config) ->
    ok.

new_meck() ->
    ok = meck:new(make_proxy, [non_strict, no_link]),
    ok.

expect() ->
    ok = meck:expect(make_proxy, test, fun() -> {ok, 1} end).
del_meck() ->
    meck:unload().

handle(_Config) ->
    expect(),
    ?assertEqual({ok,1}, make_proxy:test()),
    ok.