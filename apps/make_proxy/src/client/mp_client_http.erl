-module(mp_client_http).

-behaviour(mp_client_protocol).

-export([detect_head/1, request/2, parse_http_request/1, certfile_to_cert/1]).

-include("mp_client.hrl").
-include("mp_http_request.hrl").

-define(HTTP_METHOD_HEAD,
        [$G,   % GET
         $P,   % POST PUT
         $H,   % HEAD
         $D,   % DELETE
         $T,   % TRACE
         $C,   % CONNECT
         $O]).    % OPTIONS

detect_head(H) ->
    lists:member(H, ?HTTP_METHOD_HEAD).

request(Data, #client{remote = undefined, buffer = Buffer} = State0) ->
    State = State0#client{handle_state = erlang:element(2, make_proxy_http:init([]))},
    Data1 = <<Buffer/binary, Data/binary>>,
    {Data2, Req} = parse_http_request(Data1),
    ?Debug("request => ~p~n", [Data1]),
    case Req#http_request.status of
        done ->
            do_communication(Data2, Req, State);
        error ->
            {error, parse_http_request_error};
        more ->
            {ok, State#client{buffer = Data1}}
    end;
request(Data, #client{remote = Remote, keep_alive = false} = State) ->
    gen_tcp:close(Remote),
    request(Data, State#client{remote = undefined});
request(Data,
        #client{key = _Key,
                remote = Remote,
                handle_state = HandleState,
                keep_alive = true,
                enable_https = Https} =
            State) ->
    case Https of
        true ->
            %%       ?Debug("ssl request = ~p", [Data]),
            {ok,
             #http_state{req_in = _ReqIn,
                         req_headers = Headers,
                         req_payload = Payload} =
                 NewHandleState} =
                make_proxy_http:handle({req, Data}, HandleState),
            case Payload /= <<>> of
                true ->
                    ebus:pub(?TOPIC_PAYLOAD, {?TOPIC_PAYLOAD, req, mp_client_utils:http_state_to_map(NewHandleState)}),
                    ?Debug("SSL request henader = ~p, payload = ~p", [Headers, Payload]);
                _ ->
                    ok
            end,
            ok = ssl:send(Remote, Data),
            {ok, State#client{handle_state = NewHandleState}};
        _ ->
            ok = gen_tcp:send(Remote, Data),
            {ok, State}
    end.

-spec do_communication(binary(), #http_request{}, #client{}) ->
                          {ok, #client{}} | {error, term()}.
do_communication(Data,
                 #http_request{host = Host,
                               port = Port,
                               next_data = NextData} =
                     Req,
                 #client{socket = Socket, transport = Transport} = State) ->
    NewHost = remove_prefix(Host),
    Enable =
        case Req#http_request.method =:= <<"CONNECT">>
             andalso application:get_env(make_proxy, enable_https, true)
        of
            true when NewHost == <<"guyongli.net">>; NewHost == <<"github.com">> ->
                true;
            true ->
                true;
            _ ->
                false
        end,
    case mp_client_utils:connect_to_remote({Host, Port, Enable}) of
        {ok, Remote} ->
            ?LOG_INFO("host=~p,port=~p, nextdata = ~p", [Host, Port, NextData]),
            State1 = State#client{remote = Remote, buffer = NextData},
            case Req#http_request.method =:= <<"CONNECT">> of
                true ->
                    case Enable andalso Port /= 80 of
                        true ->
                            erlang:spawn_link(fun() ->
                                                 timer:sleep(10),
                                                 ok =
                                                     Transport:send(Socket,
                                                                    <<"HTTP/1.1 200 OK\r\n\r\n">>)
                                              end),
                            {ok,
                             starttls(State1#client{keep_alive = true, enable_https = true},
                                      Host,
                                      Port)};
                        _ ->
                            Transport:send(Socket, <<"HTTP/1.1 200 OK\r\n\r\n">>),
                            {ok, State1#client{keep_alive = true}}
                    end;
                false ->
                    ThisData = binary:part(Data, 0, byte_size(Data) - byte_size(NextData)),
                    ?Debug("this data = ~p~n", [ThisData]),
                    ok = gen_tcp:send(Remote, ThisData),
                    {ok, State1}
            end;
        {error, Reason} ->
            ?Debug("host = ~p, port = ~p, error = ~p, method = ~p",
                   [Host, Port, Reason, Req#http_request.method]),
            {error, Reason}
    end.

-spec parse_http_request(binary()) -> {binary(), #http_request{}}.
parse_http_request(Data) ->
    parse_http_request(Data, #http_request{}).

-spec parse_http_request(binary(), #http_request{}) -> {binary(), #http_request{}}.
parse_http_request(Data, Req) ->
    case binary:split(Data, <<"\r\n">>) of
        [Data] ->
            Req1 = Req#http_request{status = more},
            {Data, Req1};
        [RequestLine, Headers] ->
            {RequestLine1, Req1} = parse_request_line(RequestLine, Req),
            Req2 =
                case Req1#http_request.method of
                    <<"CONNECT">> ->
                        Req1#http_request{status = done};
                    _ ->
                        do_parse(erlang:decode_packet(httph_bin, Headers, []), Req1)
                end,

            Data1 = erlang:iolist_to_binary([RequestLine1, "\r\n", Headers]),
            {Data1, Req2}
    end.

-spec do_parse(tuple(), #http_request{}) -> #http_request{}.
do_parse({error, _}, Req) ->
    Req#http_request{status = error};
do_parse({more, _}, Req) ->
    Req#http_request{status = more};
do_parse({ok, http_eoh, Rest}, #http_request{content_length = 0} = Req) ->
    Req#http_request{status = done, next_data = Rest};
do_parse({ok, http_eoh, Rest},
         #http_request{content_length = ContentLength, current_length = CurrentLength} = Req)
    when ContentLength =:= CurrentLength ->
    Req#http_request{status = done, next_data = Rest};
do_parse({ok, http_eoh, Rest},
         #http_request{content_length = ContentLength, current_length = CurrentLength} = Req) ->
    MoreNeedLength = ContentLength - CurrentLength,
    RestLen = byte_size(Rest),

    case RestLen >= MoreNeedLength of
        true ->
            <<_Body:MoreNeedLength/binary, NextData/binary>> = Rest,
            Req#http_request{status = done,
                             current_length = CurrentLength + MoreNeedLength,
                             next_data = NextData};
        false ->
            Req#http_request{status = more, current_length = CurrentLength + RestLen}
    end;
%%do_parse({ok, {http_header, _Num, 'Host', _, Value}, Rest}, Req) ->
%%    {Host, Port} = split_host_and_port(Value),
%%
%%    Req1 = Req#http_request{host = Host, port = Port},
%%    do_parse(erlang:decode_packet(httph_bin, Rest, []), Req1);
do_parse({ok, {http_header, _Num, 'Content-Length', _, Value}, Rest}, Req) ->
    Req1 = Req#http_request{content_length = binary_to_integer(Value)},
    do_parse(erlang:decode_packet(httph_bin, Rest, []), Req1);
do_parse({ok, {http_header, _, _, _, _}, Rest}, Req) ->
    do_parse(erlang:decode_packet(httph_bin, Rest, []), Req).

-spec parse_request_line(binary(), #http_request{}) -> {binary(), #http_request{}}.
parse_request_line(RequestLine, Req) ->
    [Method, URL, Version] = binary:split(RequestLine, <<" ">>, [global]),
    {ok, P} =
        re:compile("^((?<Ascheme>http|https)://)?(?<Bhost>[^:|^/]+):?(?<Cport>\\d*)(?<Dpath>/?.*)"),
    {match, [Scheme, Host, Port, Path]} = re:run(URL, P, [{capture, all_names, binary}]),

    Port1 = find_port(Scheme, Port),
    Path1 =
        case Path of
            <<>> ->
                <<"/">>;
            _ ->
                Path
        end,

    RequestLine1 = erlang:iolist_to_binary([Method, " ", Path1, " ", Version]),

    Req1 =
        Req#http_request{method = Method,
                         host = binary_to_list(Host),
                         port = Port1},

    {RequestLine1, Req1}.

-spec find_port(binary(), binary()) -> integer().
find_port(<<"http">>, <<>>) ->
    80;
find_port(<<"https">>, <<>>) ->
    443;
find_port(_, PortBin) ->
    binary_to_integer(PortBin).

%% proxy start tls
starttls(#client{socket = Socket} = Client, Host, _Port) ->
    %%    Tls = [],
    % {ok, Tls} = application:get_env(make_proxy, tls),
    {CertFile, KeyFile} = make_proxy:list_ca_file(),
%%    NewHost = change_host(Host),
    Tls = [{cacertfile, CertFile},
           {cert, certfile_to_cert(make_proxy:get_cert_pem(erlang:list_to_binary(Host)))},
           {keyfile, KeyFile}],
    ranch_tcp:setopts(Socket, [{active, false}]),
    {ok, TLSSocket} = ssl:handshake(Socket, Tls),
    upgrade_to_tls(Client#client{socket = TLSSocket,
                                 transport = ssl,
                                 ok = ssl,
                                 closed = ssl_closed}).

%% remote socket upgrade to tls
upgrade_to_tls(#client{remote = Remote} = S) ->
    %%    {ok, TlsSocket} = ssl:connect(Remote,[{reuse_sessions,true}]),
    S#client{remote = Remote}.

certfile_to_cert(CertData) ->
    [{_, Der, _}] = public_key:pem_decode(CertData),
    Der.

remove_prefix(Host) ->
    case binary:split(
             erlang:list_to_binary(Host), <<".">>, [global])
    of
        [_, B, C] ->
            <<B/binary, ".", C/binary>>;
        [B, C] ->
            <<B/binary, ".", C/binary>>;
        _ ->
            erlang:list_to_binary(Host)
    end.
