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
    [http, http_resp, websocket, websocket_resp, transfer_encoding].

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
    {ok, #http_state{req_in = head} = S1} = make_proxy_http:handle({req, <<"G">>}, S),
    {ok, #http_state{req_in = body} = S2} =
        make_proxy_http:handle({req, <<"ET / HTTP/1.1\r\nA:a\r\nContent-Length:1\r\n\r\n">>}, S1),
    {ok,
     #http_state{req_in = fin,
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
    {ok, #http_state{resp_in = head} = S1} = make_proxy_http:handle({resp, <<"HTTP">>}, S),
    {ok, #http_state{resp_in = body} = S2} =
        make_proxy_http:handle({resp, <<"/1.1 200 OK\r\nA:a\r\nContent-Length:1\r\n\r\n">>}, S1),
    {ok,
     #http_state{resp_in = fin,
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
    {ok, #http_state{resp_in = head, resp_buffer = <<"HTTP/1.1">>} = S1} =
        make_proxy_http:handle({resp, H1}, S),
    {ok,
     #http_state{resp_in = body,
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
         {ok, #http_state{resp_in = body, resp_buffer_rest = Part1} = S3} =
             make_proxy_http:handle({resp, Part1}, S2),
         {ok,
          #http_state{resp_in = fin,
                      resp_buffer_rest = <<>>,
                      resp_payload = {text, <<"PONG">>}} =
              S4} =
             make_proxy_http:handle({resp, Part2}, S3),
         {ok,
          #http_state{resp_in = fin,
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
     #http_state{req_in = body,
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
     #http_state{req_in = fin,
                 req_payload = {text, <<"PING">>},
                 req_buffer_rest = <<>>,
                 type = ?TYPE_WEBSOCKET} =
         S2} =
        make_proxy_http:handle({req, erlang:list_to_binary(Data)}, S1),
    Data1 = erlang:list_to_binary(Data),
    Part1 = binary:part(Data1, {0, 1}),
    {ok,
     #http_state{req_in = body,
                 req_payload = <<>>,
                 req_buffer_rest = Part2} =
         S3} =
        make_proxy_http:handle({req, Part1}, S2),
    ?assertEqual(Part1, Part2),
    {ok,
     #http_state{req_in = fin,
                 req_payload = {text, <<"PING">>},
                 req_buffer_rest = <<>>}} =
        make_proxy_http:handle({req, binary:part(Data1, {1, erlang:size(Data1) - 1})}, S3),
    ok.

transfer_encoding(_) ->
    H = <<"HTTP/1.1 200 OK\r\n"
          "server: GitHub.com\r\n"
          "date: Thu, 27 Jul 2023 07:50:43 GMT\r\n"
          "content-type: application/json; charset=utf-8\r\n"
          "cache-control: public, max-age=60, s-maxage=60\r\n"
          "vary: Accept, Accept-Encoding, Accept, X-Requested-With\r\n"
          "etag: W/\"88a610c20346eedbfc777b1d78d91d3928d3ccf7300828eb759418351adea0dc\"\r\n"
          "last-modified: Thu, 27 Jul 2023 07:02:18 GMT\r\n"
          "x-github-media-type: github.v3; format=json\r\n"
          "x-github-api-version-selected: 2022-11-28\r\n"
          "access-control-expose-headers: ETag, Link, Location, Retry-After, X-GitHub-OTP, X-RateLimit-Limit, X-RateLimit-Remaining, X-RateLimit-Used, X-RateLimit-Resource, X-RateLimit-Reset, X-OAuth-Scopes, X-Accepted-OAuth-Scopes, X-Poll-Interval, X-GitHub-Media-Type, X-GitHub-SSO, X-GitHub-Request-Id, Deprecation, Sunset\r\n"
          "access-control-allow-origin: *\r\n"
          "strict-transport-security: max-age=31536000; includeSubdomains; preload\r\n"
          "x-frame-options: deny\r\n"
          "x-content-type-options: nosniff\r\n"
          "x-xss-protection: 0\r\n"
          "referrer-policy: origin-when-cross-origin, strict-origin-when-cross-origin\r\n"
          "content-security-policy: default-src 'none'\r\n"
          "content-encoding: gzip\r\n"
          "x-ratelimit-limit: 60\r\n"
          "x-ratelimit-remaining: 56\r\n"
          "x-ratelimit-reset: 1690446119\r\n"
          "x-ratelimit-resource: core\r\nx-ratelimit-used: 4\r\n"
          "accept-ranges: bytes\r\n"
          "transfer-encoding: chunked\r\n"
          "x-github-request-id: EEDF:99FD:E830D:1D4F7C:64C221D3\r\n\r\n">>,
    {ok, S} = make_proxy_http:init([]),
    {ok, #http_state{resp_in = body_chunked} = S1} = make_proxy_http:handle({resp, H}, S),
    {ok, #http_state{resp_in = body_chunked, resp_buffer_rest = <<"4">>} = S2} =
        make_proxy_http:handle({resp, <<"4">>}, S1),
    {ok, #http_state{resp_in = body_chunked, resp_buffer_rest = <<"4\r">>} = S3} =
        make_proxy_http:handle({resp, <<"\r">>}, S2),
    {ok, #http_state{resp_in = body_chunked, resp_buffer_rest = <<"4\r\n">>} = S4} =
        make_proxy_http:handle({resp, <<"\n">>}, S3),
    {ok, #http_state{resp_in = body_chunked, resp_buffer_rest = <<"4\r\n1">>} = S5} =
        make_proxy_http:handle({resp, <<"1">>}, S4),
    {ok, #http_state{resp_in = body_chunked, resp_buffer_rest = <<"4\r\n12">>} = S6} =
        make_proxy_http:handle({resp, <<"2">>}, S5),
    {ok, #http_state{resp_in = body_chunked, resp_buffer_rest = <<"4\r\n123">>} = S7} =
        make_proxy_http:handle({resp, <<"3">>}, S6),
    {ok, #http_state{resp_in = body_chunked, resp_buffer_rest = <<"4\r\n1234">>} = S8} =
        make_proxy_http:handle({resp, <<"4">>}, S7),
    {ok, #http_state{resp_in = body_chunked, resp_buffer_rest = <<"4\r\n1234\r">>} = S9} =
        make_proxy_http:handle({resp, <<"\r">>}, S8),
    {ok,
     #http_state{resp_in = body_chunked,
                 resp_buffer_rest = <<>>,
                 resp_payload = <<"1234">>} =
         S10} =
        make_proxy_http:handle({resp, <<"\n">>}, S9),
    {ok, #http_state{resp_in = body_chunked, resp_buffer_rest = <<"0">>} = S11} =
        make_proxy_http:handle({resp, <<"0">>}, S10),
    {ok, #http_state{resp_in = body_chunked, resp_buffer_rest = <<"0\r">>} = S12} =
        make_proxy_http:handle({resp, <<"\r">>}, S11),
    {ok, #http_state{resp_in = body_chunked, resp_buffer_rest = <<"0\r\n">>} = S13} =
        make_proxy_http:handle({resp, <<"\n">>}, S12),
    {ok, #http_state{resp_in = body_chunked, resp_buffer_rest = <<"0\r\n\r">>} = S14} =
        make_proxy_http:handle({resp, <<"\r">>}, S13),
    {ok, #http_state{resp_in = fin} = _S15} = make_proxy_http:handle({resp, <<"\n">>}, S14),
    %%
    {ok, SS1} = make_proxy_http:init([]),
    {ok, #http_state{resp_in = fin, resp_payload = <<"1234">>}} =
        make_proxy_http:handle({resp, <<H/binary, "4\r\n1234\r\n0\r\n\r\n">>}, SS1),
    {ok,
     #http_state{resp_in = body_chunked,
                 resp_payload = <<>>,
                 resp_buffer_rest = <<"4\r\n1234\r\n0\r\n">>} =
         SS2} =
        make_proxy_http:handle({resp, <<H/binary, "4\r\n1234\r\n0\r\n">>}, SS1),
    {ok,
     #http_state{resp_in = fin,
                 resp_payload = <<"1234">>,
                 resp_buffer_rest = <<>>}} =
        make_proxy_http:handle({resp, <<"\r\n">>}, SS2),

    {ok,
     #http_state{resp_in = body_chunked,
                 resp_payload = <<>>,
                 resp_buffer_rest = <<"4\r\n1234\r\n0\r\nA:a">>} =
         SS3} =
        make_proxy_http:handle({resp, <<H/binary, "4\r\n1234\r\n0\r\nA:a">>}, SS1),
    {ok,
     #http_state{resp_in = fin,
                 resp_payload = <<"1234">>,
                 resp_buffer_rest = <<>>,
                 resp_headers = Headers}} =
        make_proxy_http:handle({resp, <<"\r\n\r\n">>}, SS3),
    ?assertEqual(true, lists:member({<<"a">>, <<"a">>}, Headers)),
    ok.
