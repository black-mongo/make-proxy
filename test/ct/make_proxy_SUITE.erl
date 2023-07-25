-module(make_proxy_SUITE).

-include("make_proxy_ct.hrl").

-include_lib("kernel/include/logger.hrl").

-compile(export_all).

all() ->
    %%    [websocket].
    [check_http_parse, socks, http, https, websocket].

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
    ok = gen_tcp:close(S),
    Server ! close,
    ok.

https(_) ->
    Pid = self(),
    Server = erlang:spawn_link(fun() -> start_https(Pid) end),
    receive
        done ->
            ok
    end,
    Send1 =
        <<"GET /ping HTTP/1.1\r\nContent-Type:application/json\r\nContent-Length:4\r\n\r\nPING">>,
    {ok, C} = ssl:connect("localhost", 8890, [binary, {active, false}], 1000),
    ok = ssl:send(C, Send1),
    {ok, Send1} = ssl:recv(C, erlang:size(Send1), 500),
    {ok, Send1} = ssl:recv(C, erlang:size(Send1), 500),
    ok = ssl:close(C),
    {ok, TlsSocket} = https_proxy_connect(<<"localhost:8890">>),
    ok = ssl:send(TlsSocket, Send1),
    {ok, Send1} = ssl:recv(TlsSocket, erlang:size(Send1), 500),
    {ok, Send1} = ssl:recv(TlsSocket, erlang:size(Send1), 500),
    ok = ssl:send(TlsSocket, Send1),
    {ok, Send1} = ssl:recv(TlsSocket, erlang:size(Send1), 500),
    {ok, Send1} = ssl:recv(TlsSocket, 0, 500),
    ok = ssl:send(TlsSocket, Send1),
    {ok, Send1} = ssl:recv(TlsSocket, erlang:size(Send1), 500),
    {ok, Send1} = ssl:recv(TlsSocket, 0, 500),
    ok = ssl:close(TlsSocket),
    erlang:exit(Server, kill),
    ok.

websocket(_) ->
    H =
        <<"GET /websocket HTTP/1.1\r
Sec-WebSocket-Version: 13\r
Sec-WebSocket-Key: pKdxKYCNcdle4Dd3BBYqQg==\r
Connection: Upgrade\r
Upgrade: websocket\r
Host: web.mchat.com\r\n\r\n">>,
    {ok, TlsSocket} = https_proxy_connect(<<"web.mchat.com:443">>),
    ok = ssl:send(TlsSocket, H),
    {ok, <<"HTTP/1.1 101 Switching Protocols", _/binary>>} =
        recv_loop(fun() -> ssl:recv(TlsSocket, 0, 1000) end),
    Ping = cow_ws:masked_frame({text, <<"PING">>}, #{}),
    {text, Json} = recv_websocket(fun() -> ssl:recv(TlsSocket, 0, 1000) end),
    #{response_code := 200,
      type := <<"session_id">>,
      id := _} =
        jsx:decode(Json, [return_maps, {labels, atom}]),
    ok = ssl:send(TlsSocket, Ping),
    {text, <<"PONG">>} = recv_websocket(fun() -> ssl:recv(TlsSocket, 0, 1000) end),
    Ping1 = cow_ws:masked_frame({text, <<"PING">>}, #{}),
    ok = ssl:send(TlsSocket, Ping1),
    {text, <<"PONG">>} = recv_websocket(fun() -> ssl:recv(TlsSocket, 0, 1000) end),
    ok = ssl:close(TlsSocket),
    ok.

https_proxy_connect(Host) ->
    Port = application:get_env(make_proxy, client_port, 5222),
    {ok, S} = gen_tcp:connect("127.0.0.1", Port, [binary, {active, false}]),
    Send =
        <<"CONNECT ",
          Host/binary,
          " HTTP/1.1\r\nHost: ",
          Host/binary,
          "\r\nProxy-Connection: keep-alive\r\nUser-Agent: Mozilla/5.0 (Macintosh; Intel Mac OS X 10_15_7) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/114.0.0.0 Safari/537.36\r\n\r\n">>,
    ok = gen_tcp:send(S, Send),
    {ok, <<"HTTP/1.1 200 OK\r\n\r\n">>} = gen_tcp:recv(S, 0, 1000),
    {ok, _TlsSocket} = ssl:connect(S, [{reuse_sessions, true}]).

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

start_https(Pid) ->
    erlang:spawn_link(fun() ->
                         {_CertFile, KeyFile} = make_proxy:list_ca_file(),
                         {ok, L} =
                             ssl:listen(8890,
                                        [{cert,
                                          mp_client_http:certfile_to_cert(
                                              make_proxy:get_cert_pem(<<"localhost">>))},
                                         {keyfile, erlang:binary_to_list(KeyFile)},
                                         {reuseaddr, true},
                                         binary]),
                         Pid ! done,
                         accept_https(L)
                      end).

accept_https(L) ->
    {ok, SS} = ssl:transport_accept(L),
    Pid = spawn_link(fun() ->
                        {ok, S} = ssl:handshake(SS),
                        ok = ssl:setopts(S, [{active, true}]),
                        loop_https(S, <<>>)
                     end),
    ok = ssl:controlling_process(SS, Pid),
    accept_https(L).

loop_https(S, Buffer) ->
    receive
        close ->
            ok;
        {ssl, _, Data} ->
            case http_parse(<<Buffer/binary, Data/binary>>) of
                {done, <<"GET /ping", _/binary>> = NewBuffer} ->
                    ?LOG_INFO("https PING"),
                    ok = ssl:send(S, NewBuffer),
                    ok = ssl:send(S, NewBuffer),
                    loop_https(S, <<>>);
                {done, NewBuffer} ->
                    ok = ssl:send(S, NewBuffer),
                    loop_https(S, <<>>);
                {more, NewBuffer} ->
                    loop_https(S, NewBuffer);
                {error, _R} ->
                    ok = ssl:close(S)
            end;
        {ssl_closed, _} ->
            ok
    end.

recv_websocket(Fun) ->
    recv_websocket(Fun, <<>>).

recv_websocket(Fun, Buffer) ->
    {ok, Data} = Fun(),
    NewBuffer = <<Buffer/binary, Data/binary>>,
    case cow_ws:parse_header(NewBuffer, #{}, undefined) of
        more ->
            recv_websocket(Fun, NewBuffer);
        {Type, _FragState2, Rsv, Len, MaskKey, Rest} ->
            case cow_ws:parse_payload(Rest, MaskKey, 0, 0, Type, Len, undefined, #{}, Rsv) of
                {ok, Payload, Utf8State2, <<>>} ->
                    cow_ws:make_frame(Type, Payload, Utf8State2, undefine);
                {ok, ClosedCode, Payload, _Utf8State2, <<>>} ->
                    cow_ws:make_frame(Type, Payload, ClosedCode, undefine)
            end;
        error ->
            exit({badframe})
    end.

recv_loop(Fun) ->
    recv_loop(Fun, <<>>).

recv_loop(Fun, Buffer) ->
    {ok, Data} = Fun(),
    case http_parse(<<Buffer/binary, Data/binary>>) of
        {done, NewBuffer} ->
            {ok, NewBuffer};
        {more, NewBuffer} ->
            recv_loop(Fun, NewBuffer)
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
do_http_parse({ok, {http_response, _, _, _}, Rest}, Data) ->
    do_http_parse(erlang:decode_packet(httph_bin, Rest, []), Data);
do_http_parse({ok, http_eoh, _Rest}, Data) ->
    {done, Data};
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
