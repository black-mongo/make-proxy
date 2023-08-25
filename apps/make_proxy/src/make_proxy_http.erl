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
-export([init/1, handle/2, update_counter/1]).

init(_) ->
    {ok, #http_state{id = update_counter(http_state_id)}}.

update_counter(Name) ->
    ets:update_counter(make_proxy_counters, Name, 1, {Name, 0}).

%% @doc http request
-spec handle(event(), #http_state{}) -> {ok, #http_state{}}.
handle({req, Data}, #http_state{req_in = head, req_buffer = Buffer} = State) ->
    NewBuffer = <<Buffer/binary, Data/binary>>,
    case binary:match(NewBuffer, <<"\r\n\r\n">>) of
        nomatch ->
            {ok, State#http_state{req_buffer = NewBuffer}};
        _ ->
            case parse_head(NewBuffer) of
                {ok, {Method, Path, Version}, Headers, Rest, ?TYPE_HTTP = Type} ->
                    PayloadLen =
                        erlang:binary_to_integer(
                            proplists:get_value(<<"content-length">>, Headers, <<"0">>)),
                    {IN, ReqPayload, Rest1} = parse_http_body(PayloadLen, Rest),
                    {ok,
                     State#http_state{req_method = Method,
                                      req_path = Path,
                                      req_ver = Version,
                                      type = Type,
                                      req_buffer = NewBuffer,
                                      req_len = PayloadLen,
                                      req_in = IN,
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
                                      req_in = IN,
                                      req_buffer = NewBuffer,
                                      req_headers = Headers,
                                      req_payload = ReqPayload,
                                      req_buffer_rest = Rest1}};
                {error, Why} ->
                    {error, Why}
            end
    end;
handle({req, Data},
       #http_state{req_in = body,
                   req_len = PayloadLen,
                   req_buffer = Buffer,
                   req_buffer_rest = Rest,
                   type = ?TYPE_HTTP} =
           State) ->
    NewBuffer = <<Buffer/binary, Data/binary>>,
    NewRest = <<Rest/binary, Data/binary>>,
    {IN, ReqPayload, Rest1} = parse_http_body(PayloadLen, NewRest),
    {ok,
     State#http_state{req_in = IN,
                      req_payload = ReqPayload,
                      req_buffer_rest = Rest1,
                      req_buffer = NewBuffer}};
handle({req, Data},
       #http_state{req_in = body,
                   req_buffer = Buffer,
                   req_buffer_rest = Rest,
                   type = ?TYPE_WEBSOCKET} =
           State) ->
    NewBuffer = <<Buffer/binary, Data/binary>>,
    NewRest = <<Rest/binary, Data/binary>>,
    {In, ReqPayload, Rest1} = parse_frame(NewRest),
    {ok,
     State#http_state{req_buffer = NewBuffer,
                      req_in = In,
                      req_payload = ReqPayload,
                      req_buffer_rest = Rest1}};
handle({req, Data}, #http_state{req_in = fin, type = ?TYPE_HTTP} = State) ->
    handle({req, Data},
           State#http_state{req_in = head,
                            req_payload = <<>>,
                            req_buffer_rest = <<>>,
                            req_buffer = <<>>});
handle({req, Data}, #http_state{req_in = fin, type = ?TYPE_WEBSOCKET} = State) ->
    handle({req, Data},
           State#http_state{req_in = body,
                            req_payload = <<>>,
                            req_buffer = <<>>});
%% @doc http response
handle({resp, Data}, #http_state{resp_in = head, resp_buffer = Buffer} = State) ->
    NewBuffer = <<Buffer/binary, Data/binary>>,
    case binary:match(NewBuffer, <<"\r\n\r\n">>) of
        nomatch ->
            {ok, State#http_state{resp_buffer = NewBuffer}};
        _ ->
            case parse_resp_head(NewBuffer) of
                {ok, {Ver, Status, String}, Headers, Rest, ?TYPE_HTTP = Type, body_chunked} ->
                    NewS =
                        State#http_state{resp_buffer = NewBuffer,
                                         resp_in = body_chunked,
                                         resp_status_line = {Ver, Status, String},
                                         resp_headers = Headers,
                                         type = Type},
                    parse_chunked(Rest, NewS);
                {ok, {Ver, Status, String}, Headers, Rest, ?TYPE_HTTP = Type, {body, Len}} ->
                    {IN, Payload, Rest1} = parse_http_body(Len, Rest),
                    {ok,
                     State#http_state{resp_buffer = NewBuffer,
                                      resp_buffer_rest = Rest1,
                                      resp_status_line = {Ver, Status, String},
                                      resp_payload = Payload,
                                      resp_headers = Headers,
                                      resp_in = IN,
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
                                      resp_in = IN,
                                      type = Type}}
            end
    end;
handle({resp, Data},
       #http_state{resp_in = body,
                   resp_buffer = Buffer,
                   resp_len = Len,
                   resp_buffer_rest = Rest,
                   type = ?TYPE_HTTP} =
           State) ->
    NewBuffer = <<Buffer/binary, Data/binary>>,
    NewRest = <<Rest/binary, Data/binary>>,
    {IN, Payload, Rest1} = parse_http_body(Len, NewRest),
    {ok,
     State#http_state{resp_in = IN,
                      resp_buffer = NewBuffer,
                      resp_payload = Payload,
                      resp_buffer_rest = Rest1}};
handle({resp, Data},
       #http_state{resp_in = body,
                   resp_buffer = Buffer,
                   resp_buffer_rest = Rest,
                   type = ?TYPE_WEBSOCKET} =
           State) ->
    NewBuffer = <<Buffer/binary, Data/binary>>,
    NewRest = <<Rest/binary, Data/binary>>,
    {IN, Payload, Rest1} = parse_frame(NewRest),
    {ok,
     State#http_state{resp_in = IN,
                      resp_buffer = NewBuffer,
                      resp_payload = Payload,
                      resp_buffer_rest = Rest1}};
handle({resp, Data},
       #http_state{resp_in = body_chunked,
                   resp_buffer = Buffer,
                   resp_buffer_rest = Rest,
                   type = ?TYPE_HTTP} =
           State) ->
    NewBuffer = <<Buffer/binary, Data/binary>>,
    NewRest = <<Rest/binary, Data/binary>>,
    parse_chunked(NewRest, State#http_state{resp_buffer = NewBuffer});
handle({resp, Data}, #http_state{resp_in = fin, type = ?TYPE_HTTP} = State) ->
    handle({resp, Data},
           State#http_state{resp_in = head,
                            resp_buffer = <<>>,
                            resp_payload = <<>>});
handle({resp, Data}, #http_state{resp_in = fin, type = ?TYPE_WEBSOCKET} = State) ->
    handle({resp, Data},
           State#http_state{resp_in = body,
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
  case do_parse_frame(Rest, []) of
    {body, Frames, NewRest} ->
      {body, Frames, NewRest};
    {fin, Frames, NewRest} ->
      {fin, Frames, NewRest}
  end.
do_parse_frame({body, Acc, Rest}) when Acc == [] ->
  {body, [], Rest};
do_parse_frame({body, Acc, Rest}) ->
  {fin, Acc, Rest}.
do_parse_frame(<<>>, Acc) ->
  {fin, lists:reverse(Acc), <<>>};
do_parse_frame(Rest, Acc) ->
    case cow_ws:parse_header(Rest, #{}, undefined) of
        more ->
            {body, <<>>, Rest};
        {Type, _FragState2, Rsv, Len, MaskKey, Rest1} ->
            case cow_ws:parse_payload(Rest1, MaskKey, 0, 0, Type, Len, undefined, #{}, Rsv) of
                {more, _, _, _} ->
                    do_parse_frame({body, Acc, Rest});
                {more, _, _} ->
                    do_parse_frame({body, Acc, Rest});
                {ok, Payload, Utf8State2, Rest2} ->
                    do_parse_frame(Rest2, [cow_ws:make_frame(Type, Payload, Utf8State2, undefine) | Acc]);
                {ok, ClosedCode, Payload, _Utf8State2, Rest2} ->
                  do_parse_frame(Rest2, [cow_ws:make_frame(Type, Payload, ClosedCode, undefine) | Acc])
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
            {ok,
             {Ver, Status, String},
             Headers,
             Rest1,
             ?TYPE_HTTP,
             response_io_from_headers(Ver, Status, Headers)}
    end.

response_io_from_headers(Version, _Status, Headers) ->
    case lists:keyfind(<<"transfer-encoding">>, 1, Headers) of
        {_, TE} when Version =:= 'HTTP/1.1' ->
            L = cow_http_hd:parse_transfer_encoding(TE),
            case lists:member(<<"chunked">>, L) of
                true ->
                    body_chunked;
                _ when [<<"identity">>] == L ->
                    {body, 0}
            end;
        _ ->
            case lists:keyfind(<<"content-length">>, 1, Headers) of
                {_, <<"0">>} ->
                    {body, 0};
                {_, Length} ->
                    {body, cow_http_hd:parse_content_length(Length)};
                _ ->
                    {body, 0}
            end
    end.

parse_chunked(Data, #http_state{resp_headers = Headers} = S) ->
    case cow_http_te:stream_chunked(Data, {0, 0}) of
        more ->
            {ok, S#http_state{resp_buffer_rest = Data}};
        {more, _C, _ChunkedS} ->
            {ok, S#http_state{resp_buffer_rest = Data}};
        %%      {ok, S#http_state{resp_chunked_state = ChunkedS, resp_payload = C, resp_buffer_rest = <<>>}};
        {more, C, Len, ChunkedS} when Len == 0 ->
            {ok,
             S#http_state{resp_chunked_state = ChunkedS,
                          resp_payload = C,
                          resp_buffer_rest = <<>>}};
        {more, _C, Len, ChunkedS} when is_integer(Len) ->
            {ok, S#http_state{resp_buffer_rest = Data, resp_chunked_state = ChunkedS}};
        {more, C, Rest, ChunkedS} ->
            {ok,
             S#http_state{resp_chunked_state = ChunkedS,
                          resp_payload = C,
                          resp_buffer_rest = Rest}};
        {done, no_trailers, <<>>} ->
            {ok, S#http_state{resp_in = fin, resp_buffer_rest = <<>>}};
        {done, C, no_trailers, <<>>} ->
            {ok,
             S#http_state{resp_payload = C,
                          resp_buffer_rest = <<>>,
                          resp_in = fin}};
        {done, C, trailers, Rest} ->
            case binary:match(Rest, <<"\r\n\r\n">>) of
                nomatch ->
                    {ok, S#http_state{resp_buffer_rest = Data}};
                _ ->
                    {Trailer, <<>>} = cow_http:parse_headers(Rest),
                    {ok,
                     S#http_state{resp_payload = C,
                                  resp_buffer_rest = <<>>,
                                  resp_headers = Headers ++ Trailer,
                                  resp_in = fin}}
            end
    end.
