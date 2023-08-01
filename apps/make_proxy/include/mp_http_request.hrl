%%%-------------------------------------------------------------------
%%% @author wang
%%% @copyright (C) 2016, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 14. Oct 2016 上午11:52
%%%-------------------------------------------------------------------
-author("wang").

-record(http_request,
        {status = more :: more | done | error,
         method :: binary() | undefined,
         host :: nonempty_string() | undefined,
         port :: inet:port_number() | undefined,
         content_length = 0 :: non_neg_integer(),
         current_length = 0 :: non_neg_integer(),
         next_data = <<>> :: binary()}).

-type in() :: head | body | body_chunked | fin.
-type event() :: {req, binary()} | {resp, binary()}.

-define(TYPE_HTTP, 0).
-define(TYPE_HTTP2, 1).
-define(TYPE_WEBSOCKET, 2).

-record(http_state,
        {
         id = make_ref():: reference(),
         type = ?TYPE_HTTP :: integer(),
         req_buffer = <<>> :: binary(),
         req_buffer_rest = <<>> :: binary(),
         req_headers = [] :: list(),
         req_method = <<>> :: binary(),
         req_path = <<>> :: binary(),
         req_ver = 'HTTP/1.1' :: atom(),
         req_len = 0 :: integer(),
         req_payload = <<>> :: term(),
         req_in = head :: in(),
         resp_status_line = {'HTTP/1.1', 200, <<"OK">>},
         resp_headers = [] :: list(),
         resp_len = 0 :: integer(),
         resp_buffer = <<>> :: binary(),
         resp_buffer_rest = <<>> :: binary(),
         resp_payload = <<>> :: binary(),
         resp_in = head :: in(),
         resp_chunked_state = {0, 0} :: tuple()}).

-define(TOPIC_PAYLOAD, "topic:payload").