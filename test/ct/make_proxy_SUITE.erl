-module(make_proxy_SUITE).

-include("make_proxy_ct.hrl").

-compile(export_all).

all() ->
    [check_http_parse, socks, http].

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
    ok.

expect() ->
    ok.

del_meck() ->
    meck:unload().

socks(_) ->
    Server = erlang:spawn_link(fun() -> start_echo() end),
    Port = application:get_env(make_proxy, client_port, 5222),
    {ok, Conn} = gen_tcp:connect("127.0.0.1", Port, []),
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
    ok = gen_tcp:send(Conn, <<"hello\r\n">>),
    ?assertEqual(<<"hello\r\n">>, match(Conn)),
    Server ! close,
    ok.

check_http_parse(_) ->
    ?assertEqual({more, <<"G">>}, http_parse(<<"G">>)),
    ?assertEqual({more, <<"GET / HTTP/1.1">>}, http_parse(<<"GET / HTTP/1.1">>)),
    ?assertEqual({more, <<"GET / HTTP/1.1\r\n">>}, http_parse(<<"GET / HTTP/1.1\r\n">>)),
    ?assertEqual({more, <<"GET / HTTP/1.1\r\nContent-Type:Application/json">>},
                 http_parse(<<"GET / HTTP/1.1\r\nContent-Type:Application/json">>)),
    ?assertEqual({done,
                  <<"GET / HTTP/1.1\r\nContent-Type:Application/json\r\nContent-Length:1\r\n\r\n 1">>},
                 http_parse(<<"GET / HTTP/1.1\r\nContent-Type:Application/json\r\nContent-Length:1\r\n\r\n 1">>)),
    Send =
        <<"GET http://127.0.0.1:8889/ping HTTP/1.1\r\nContent-Type:application/json\r\nContent-Length:4\r\n\r\nPING">>,
    ?assertEqual({done, Send}, http_parse(Send)),
    ok.

http(_) ->
    Server = erlang:spawn_link(fun() -> start_http() end),
    Port = application:get_env(make_proxy, client_port, 5222),
    {ok, S} = gen_tcp:connect("127.0.0.1", Port, [binary, {active, false}]),
    Send =
        <<"GET http://127.0.0.1:8889/ping HTTP/1.1\r\nContent-Type:application/json\r\nContent-Length:4\r\n\r\nPING">>,
    ok = gen_tcp:send(S, Send),
    {ok,
     <<"GET /ping HTTP/1.1\r\nContent-Type:application/json\r\nContent-Length:4\r\n\r\nPING">>} =
        gen_tcp:recv(S, 0),
    ok = gen_tcp:send(S, Send),
    {ok,
     <<"GET /ping HTTP/1.1\r\nContent-Type:application/json\r\nContent-Length:4\r\n\r\nPING">>} =
        gen_tcp:recv(S, 0),
    Server ! close,
    ok.

match(Conn) ->
    receive
        {tcp, Conn, Data} ->
            iolist_to_binary(Data)
    after 500 ->
        exit({match, timeout})
    end.

match(Conn, M) ->
    receive
        {tcp, Conn, M} ->
            %%
            ok
    after 500 ->
        exit({match, timeout, M})
    end.

start_http() ->
    {ok, L} = gen_tcp:listen(8889, [binary, {packet, raw}, {active, true}]),
    accept(L).

accept(L) ->
    {ok, S} = gen_tcp:accept(L),
    Pid = spawn_link(fun() -> loop(S, <<>>) end),
    gen_tcp:controlling_process(S, Pid),
    accept(L).

loop(S, Buffer) ->
    receive
        close ->
            ok;
        {tcp, _, Data} ->
            case http_parse(<<Buffer/binary, Data/binary>>) of
                {done, NewBuffer} ->
                    logger:debug("http_loop ==> ~p~n", [NewBuffer]),
                    ok = gen_tcp:send(S, NewBuffer),
                    loop(S, <<>>);
                {more, NewBuffer} ->
                    loop(S, NewBuffer);
                {error, _R} ->
                    ok = gen_tcp:close(S)
            end;
        {tcp_closed, _} ->
            ok
    end.

http_parse(Data) ->
    do_http_parse(erlang:decode_packet(http, Data, []), Data).

do_http_parse({ok, {http_request, _Method, _, _Version}, Rest}, Data) ->
    do_http_parse(erlang:decode_packet(httph_bin, Rest, []), Data);
do_http_parse({ok, {http_header, _, 'Content-Length', _, Value}, _Rest}, Data) ->
    ContentLen = binary_to_integer(Value),
    case binary:split(Data, <<"\r\n\r\n">>) of
        [_] ->
            {more, Data};
        [_, Body] ->
            case erlang:size(Body) < ContentLen of
                true ->
                    {more, Data};
                _ ->
                    {done, Data}
            end
    end;
do_http_parse({ok, {http_header, _, _, _, _}, Rest}, Data) ->
    do_http_parse(erlang:decode_packet(httph_bin, Rest, []), Data);
do_http_parse({ok, {http_error, _}, _}, _Data) ->
    {error, http_error};
do_http_parse({error, R}, _Data) ->
    {error, R};
do_http_parse({more, undefined}, Data) ->
    {more, Data}.

start_echo() ->
    {ok, L} = gen_tcp:listen(8888, [binary, {packet, line}]),
    {ok, S} = gen_tcp:accept(L),
    gen_tcp:controlling_process(S, self()),
    receive
        close ->
            ok;
        {tcp, _, Data} ->
            ok = gen_tcp:send(S, Data);
        {tcp_closed, _} ->
            ok
    end.
