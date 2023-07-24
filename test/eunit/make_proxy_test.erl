-module(make_proxy_test).

-include_lib("eunit/include/eunit.hrl").

succeed() ->
    ok.

succeeding_test() ->
    succeed().

succeeding_fun_test_() ->
    fun() -> succeed() end.

succeeding_simple_test_() ->
    ?_test(succeed()).

socks_test() ->
    Server = erlang:spawn_link(fun() -> start_echo() end),
    %%    {ok, Conn} = gen_tcp:connect("127.0.0.1", 7777, []),
    {ok, Conn} = gen_tcp:connect("119.9.91.52", 65535, []),
    ok = gen_tcp:send(Conn, <<5, 1, 0>>),
    %% No authentication
    ?assertEqual(<<5, 0>>, match(Conn)),
    A = 8888 band 255,
    B = 8888 bsr 8,
    %% client connection request
    ok = gen_tcp:send(Conn, <<5, 1, 0, 1, 127, 0, 0, 1, B, A>>),
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
        {tcp_closed, _} ->
            tcp_closed;
        {tcp, Conn, Data} ->
            iolist_to_binary(Data)
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
