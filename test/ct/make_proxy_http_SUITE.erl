%%%-------------------------------------------------------------------
%%% @author cam
%%% @copyright (C) 2023, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 26. 7月 2023 16:08
%%%-------------------------------------------------------------------
-module(make_proxy_http_SUITE).

-author("cam").

-include("make_proxy_ct.hrl").

-include_lib("kernel/include/logger.hrl").
-include_lib("make_proxy/include/mp_http_request.hrl").

-compile(export_all).

all() ->
    [http, http_resp, websocket, websocket_resp].

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

http(_) ->
    {ok, S} = make_proxy_http:init([]),
    {ok, #http_state{in = head} = S1} = make_proxy_http:handle({req, <<"G">>}, S),
    {ok, #http_state{in = body} = S2} =
        make_proxy_http:handle({req, <<"ET / HTTP/1.1\r\nA:a\r\nContent-Length:1\r\n\r\n">>}, S1),
    {ok,
     #http_state{in = fin,
                 req_payload = <<"a">>,
                 req_len = 1,
                 req_buffer = <<"GET / HTTP/1.1\r\nA:a\r\nContent-Length:1\r\n\r\na">>,
                 req_buffer_rest = <<>>,
                 type = ?TYPE_HTTP,
                 req_path = <<"/">>,
                 req_method = <<"GET">>,
                 req_headers = [{<<"a">>, <<"a">>}, {<<"content-length">>, <<"1">>}],
                 req_ver = 'HTTP/1.1'}} =
        make_proxy_http:handle({req, <<"a">>}, S2),

    ok.

http_resp(_) ->
    {ok, S} = make_proxy_http:init([]),
    {ok, #http_state{in = head} = S1} = make_proxy_http:handle({resp, <<"HTTP">>}, S),
    {ok, #http_state{in = body} = S2} =
        make_proxy_http:handle({resp, <<"/1.1 200 OK\r\nA:a\r\nContent-Length:1\r\n\r\n">>}, S1),
    {ok,
     #http_state{in = fin,
                 resp_payload = <<"a">>,
                 resp_len = 1,
                 resp_buffer = <<"HTTP/1.1 200 OK\r\nA:a\r\nContent-Length:1\r\n\r\na">>,
                 resp_buffer_rest = <<>>,
                 type = ?TYPE_HTTP,
                 resp_headers = [{<<"a">>, <<"a">>}, {<<"content-length">>, <<"1">>}],
                 resp_status_line = {'HTTP/1.1', 200, <<"OK">>}}} =
        make_proxy_http:handle({resp, <<"a">>}, S2),
    ok.

websocket_resp(_) ->
    H1 = <<"HTTP/1.1">>,
    H2 =
        <<" 101 Switching Protocols\r
Date: Wed, 26 Jul 2023 09:43:43 GMT\r
sec-websocket-accept: J8CrTdTRACnpGifScJH3opeGZKk=\r
upgrade: websocket\r
Server: Apache\r
X-Cache: MISS from megai-cdn120-069\r
Connection: upgrade\r\n\r\n">>,
    {ok, S} = make_proxy_http:init([]),
    {ok, #http_state{in = head, resp_buffer = <<"HTTP/1.1">>} = S1} =
        make_proxy_http:handle({resp, H1}, S),
    {ok,
     #http_state{in = body,
                 resp_buffer = Buffer,
                 resp_buffer_rest = <<>>,
                 type = ?TYPE_WEBSOCKET,
                 resp_status_line = {'HTTP/1.1', 101, <<"Switching Protocols">>}} =
         S2} =
        make_proxy_http:handle({resp, H2}, S1),
    ?assertEqual(<<H1/binary, H2/binary>>, Buffer),
    [begin
         Data =
             erlang:list_to_binary(
                 cow_ws:frame({text, <<"PONG">>}, undefined)),
         First =
             erlang:phash2(
                 crypto:strong_rand_bytes(10), erlang:size(Data)),
         Part1 = binary:part(Data, {0, First}),
         Part2 = binary:part(Data, {First, erlang:size(Data) - First}),
         {ok, #http_state{in = body, resp_buffer_rest = Part1} = S3} =
             make_proxy_http:handle({resp, Part1}, S2),
         {ok,
          #http_state{in = fin,
                      resp_buffer_rest = <<>>,
                      resp_payload = {text, <<"PONG">>}} =
              S4} =
             make_proxy_http:handle({resp, Part2}, S3),
         {ok,
          #http_state{in = fin,
                      resp_buffer_rest = <<>>,
                      resp_payload = {text, <<"PONG">>}}} =
             make_proxy_http:handle({resp, Data}, S4)
     end
     || _N <- lists:seq(1, 10)],
    ok.

websocket(_) ->
    H =
        <<"GET /websocket HTTP/1.1\r
Sec-WebSocket-Version: 13\r
Sec-WebSocket-Key: pKdxKYCNcdle4Dd3BBYqQg==\r
Connection: Upgrade\r
Upgrade: websocket\r
Host: web.mchat.com\r\n\r\n">>,
    {ok, S} = make_proxy_http:init([]),
    {ok,
     #http_state{in = body,
                 type = ?TYPE_WEBSOCKET,
                 req_method = <<"GET">>,
                 req_path = <<"/websocket">>,
                 req_ver = 'HTTP/1.1',
                 req_buffer = H,
                 req_buffer_rest = <<>>,
                 req_payload = <<>>,
                 req_len = 0} =
         S1} =
        make_proxy_http:handle({req, H}, S),
    Data = cow_ws:masked_frame({text, <<"PING">>}, undefined),
    {ok,
     #http_state{in = fin,
                 req_payload = {text, <<"PING">>},
                 req_buffer_rest = <<>>,
                 type = ?TYPE_WEBSOCKET} =
         S2} =
        make_proxy_http:handle({req, erlang:list_to_binary(Data)}, S1),
    Data1 = erlang:list_to_binary(Data),
    Part1 = binary:part(Data1, {0, 1}),
    {ok,
     #http_state{in = body,
                 req_payload = <<>>,
                 req_buffer_rest = Part2} =
         S3} =
        make_proxy_http:handle({req, Part1}, S2),
    ?assertEqual(Part1, Part2),
    {ok,
     #http_state{in = fin,
                 req_payload = {text, <<"PING">>},
                 req_buffer_rest = <<>>}} =
        make_proxy_http:handle({req, binary:part(Data1, {1, erlang:size(Data1) - 1})}, S3),
    ok.
