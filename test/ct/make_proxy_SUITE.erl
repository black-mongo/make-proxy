-module(make_proxy_SUITE).

-include("make_proxy_ct.hrl").

-compile(export_all).

all() ->
    [handle, socks].

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
    ?assertEqual({ok, 1}, make_proxy:test()),
    ok.

socks(_) ->
    Server = erlang:spawn_link(fun() -> start_echo() end),
    {ok, Conn} = gen_tcp:connect("127.0.0.1", 7777, []),
    ok = gen_tcp:send(Conn, <<5, 1, 0>>),
    %% No authentication
    ?assertEqual(<<5, 0>>, match(Conn)),
    A = 8888 band 255,
    B = 8888 bsr 8,
    %% client connection request
    ok = gen_tcp:send(Conn, <<5,1,0,1,127,0,0,1,B, A>>),
    %% Response packet from server
    %% request granted
    <<5, 0, _/binary>> = match(Conn),
    %% send something
    ok = gen_tcp:send(Conn, <<"hello">>),
    ?assertEqual(<<"hello">>, match(Conn)),
    Server ! close,
    ok.

match(Conn) ->
    receive
        {tcp, Conn, Data}  ->
            iolist_to_binary(Data)
    end.
match(Conn, M) ->
    receive
        {tcp, Conn, M} ->
            %%
            ok
    end.

start_echo() ->
    {ok, L} = gen_tcp:listen(8888, []),
    {ok, S} = gen_tcp:accept(L),
    receive
        close ->
            ok;
        {tcp, _, Data} ->
            ok = gen_tcp:send(S, Data);
        {tcp_closed, _} ->
            ok
    end.
