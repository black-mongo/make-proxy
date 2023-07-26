%%%-------------------------------------------------------------------
%%% @author cam
%%% @copyright (C) 2023, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 26. 7月 2023 11:14
%%%-------------------------------------------------------------------
-module(make_proxy_http).

-author("cam").

-include_lib("make_proxy/include/mp_http_request.hrl").
-include_lib("kernel/include/logger.hrl").

%% API
-export([init/1, handle/2]).

init(_) ->
    {ok, #http_state{}}.

%% @doc http request
-spec handle(event(), #http_state{}) -> {ok, #http_state{}}.
handle({req, Data}, #http_state{in = head, req_buffer = Buffer} = State) ->
    NewBuffer = <<Buffer/binary, Data/binary>>,
    case binary:match(NewBuffer, <<"\r\n\r\n">>) of
        nomatch ->
            {ok, State#http_state{req_buffer = NewBuffer}};
        _ ->
            case parse_head(NewBuffer) of
                {ok, {Method, Path, Version}, Headers, Rest, ?TYPE_HTTP = Type} ->
                    PayloadLen =
                        erlang:binary_to_integer(
                            proplists:get_value(<<"content-length">>, Headers)),
                    {IN, ReqPayload, Rest1} = parse_http_body(PayloadLen, Rest),
                    {ok,
                     State#http_state{req_method = Method,
                                      req_path = Path,
                                      req_ver = Version,
                                      type = Type,
                                      req_buffer = NewBuffer,
                                      req_len = PayloadLen,
                                      in = IN,
                                      req_payload = ReqPayload,
                                      req_headers = Headers,
                                      req_buffer_rest = Rest1}};
                {ok, {Method, Path, Version}, Headers, Rest, ?TYPE_WEBSOCKET = Type} ->
                    {IN, ReqPayload, Rest1} = parse_frame(Rest),
                    {ok,
                     State#http_state{req_method = Method,
                                      req_path = Path,
                                      req_ver = Version,
                                      type = Type,
                                      in = IN,
                                      req_buffer = NewBuffer,
                                      req_headers = Headers,
                                      req_payload = ReqPayload,
                                      req_buffer_rest = Rest1}};
                {error, Why} ->
                    {error, Why}
            end
    end;
handle({req, Data},
       #http_state{in = body,
                   req_len = PayloadLen,
                   req_buffer = Buffer,
                   req_buffer_rest = Rest,
                   type = ?TYPE_HTTP} =
           State) ->
    NewBuffer = <<Buffer/binary, Data/binary>>,
    NewRest = <<Rest/binary, Data/binary>>,
    {IN, ReqPayload, Rest1} = parse_http_body(PayloadLen, NewRest),
    {ok,
     State#http_state{in = IN,
                      req_payload = ReqPayload,
                      req_buffer_rest = Rest1,
                      req_buffer = NewBuffer}};
handle({req, Data},
       #http_state{in = body,
                   req_buffer = Buffer,
                   req_buffer_rest = Rest,
                   type = ?TYPE_WEBSOCKET} =
           State) ->
    NewBuffer = <<Buffer/binary, Data/binary>>,
    NewRest = <<Rest/binary, Data/binary>>,
    {In, ReqPayload, Rest1} = parse_frame(NewRest),
    {ok,
     State#http_state{req_buffer = NewBuffer,
                      in = In,
                      req_payload = ReqPayload,
                      req_buffer_rest = Rest1}};
handle({req, Data}, #http_state{in = fin, type = ?TYPE_WEBSOCKET} = State) ->
    handle({req, Data},
           State#http_state{in = body,
                            req_payload = <<>>,
                            req_buffer_rest = <<>>,
                            req_buffer = <<>>});
%% @doc http response
handle({resp, Data}, #http_state{in = head, resp_buffer = Buffer} = State) ->
    NewBuffer = <<Buffer/binary, Data/binary>>,
    case binary:match(NewBuffer, <<"\r\n\r\n">>) of
        nomatch ->
            {ok, State#http_state{resp_buffer = NewBuffer}};
        _ ->
            case parse_resp_head(NewBuffer) of
                {ok, {Ver, Status, String}, Headers, Rest, ?TYPE_HTTP = Type} ->
                    Len = erlang:binary_to_integer(
                              proplists:get_value(<<"content-length">>, Headers)),
                    {IN, Payload, Rest1} = parse_http_body(Len, Rest),
                    {ok,
                     State#http_state{resp_buffer = NewBuffer,
                                      resp_buffer_rest = Rest1,
                                      resp_status_line = {Ver, Status, String},
                                      resp_payload = Payload,
                                      resp_headers = Headers,
                                      in = IN,
                                      resp_len = Len,
                                      type = Type}};
                {ok, {Ver, Status, String}, Headers, Rest, ?TYPE_WEBSOCKET = Type} ->
                    {IN, Payload, Rest1} = parse_frame(Rest),
                    {ok,
                     State#http_state{resp_buffer = NewBuffer,
                                      resp_buffer_rest = Rest1,
                                      resp_headers = Headers,
                                      resp_status_line = {Ver, Status, String},
                                      resp_payload = Payload,
                                      in = IN,
                                      type = Type}}
            end
    end;
handle({resp, Data},
       #http_state{in = body,
                   resp_buffer = Buffer,
                   resp_len = Len,
                   resp_buffer_rest = Rest,
                   type = ?TYPE_HTTP} =
           State) ->
    NewBuffer = <<Buffer/binary, Data/binary>>,
    NewRest = <<Rest/binary, Data/binary>>,
    {IN, Payload, Rest1} = parse_http_body(Len, NewRest),
    {ok,
     State#http_state{in = IN,
                      resp_buffer = NewBuffer,
                      resp_payload = Payload,
                      resp_buffer_rest = Rest1}};
handle({resp, Data},
       #http_state{in = body,
                   resp_buffer = Buffer,
                   resp_buffer_rest = Rest,
                   type = ?TYPE_WEBSOCKET} =
           State) ->
    NewBuffer = <<Buffer/binary, Data/binary>>,
    NewRest = <<Rest/binary, Data/binary>>,
    {IN, Payload, Rest1} = parse_frame(NewRest),
    {ok,
     State#http_state{in = IN,
                      resp_buffer = NewBuffer,
                      resp_payload = Payload,
                      resp_buffer_rest = Rest1}};
handle({resp, Data}, #http_state{in = fin} = State) ->
    handle({resp, Data},
           State#http_state{in = body,
                            resp_buffer_rest = <<>>,
                            resp_buffer = <<>>,
                            resp_payload = <<>>}).

parse_head(Data) ->
    {Method, Target, Version, Rest} = cow_http:parse_request_line(Data),
    {Headers, Rest1} = cow_http:parse_headers(Rest),
    Type = parse_type(Version, Headers),
    {ok, {Method, Target, Version}, Headers, Rest1, Type}.

parse_type(_Version, Headers) ->
    case proplists:get_value(<<"sec-websocket-version">>, Headers) of
        undefined ->
            ?TYPE_HTTP;
        _ ->
            ?TYPE_WEBSOCKET
    end.

parse_frame(Rest) ->
    case cow_ws:parse_header(Rest, #{}, undefined) of
        more ->
            {body, <<>>, Rest};
        {Type, _FragState2, Rsv, Len, MaskKey, Rest1} ->
            case cow_ws:parse_payload(Rest1, MaskKey, 0, 0, Type, Len, undefined, #{}, Rsv) of
                {more, _, _, _} ->
                    {body, <<>>, Rest};
                {more, _, _} ->
                    {body, <<>>, Rest};
                {ok, Payload, Utf8State2, <<>>} ->
                    {fin, cow_ws:make_frame(Type, Payload, Utf8State2, undefine), <<>>};
                {ok, ClosedCode, Payload, _Utf8State2, <<>>} ->
                    {fin, cow_ws:make_frame(Type, Payload, ClosedCode, undefine), <<>>}
            end
    end.

parse_http_body(Len, Rest) ->
    case Len > erlang:size(Rest) of
        true ->
            {body, <<>>, Rest};
        _ ->
            {fin, Rest, <<>>}
    end.

parse_resp_head(Data) ->
    {Ver, Status, String, Rest} = cow_http:parse_status_line(Data),
    {Headers, Rest1} = cow_http:parse_headers(Rest),
    case Status of
        101 ->
            {ok, {Ver, Status, String}, Headers, Rest1, ?TYPE_WEBSOCKET};
        _ ->
            {ok, {Ver, Status, String}, Headers, Rest1, ?TYPE_HTTP}
    end.