%%%-------------------------------------------------------------------
%%% @author wang
%%% @copyright (C) 2016, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 12. Oct 2016 下午5:26
%%%-------------------------------------------------------------------
-module(make_proxy).
-author("wang").

%% API
-export([start_server/0,
    start_client/0,
    start/0]).

start() ->
    do_start_server(application:get_env(make_proxy, server_port, undefined)),
    do_start_client(application:get_env(make_proxy, client_port, undefined)).

start_server() ->
    {ok, []} = application:ensure_all_started(make_proxy),
    {ok, Port} = application:get_env(make_proxy, server_port),
    do_start_server(Port).
do_start_server(undefined) ->
    igore;
do_start_server(Port) ->
    TransOpts = transport_opts(Port),

    {ok, _} = ranch:start_listener(
        make_proxy_server,
        20,
        ranch_tcp,
        TransOpts,
        mp_server_worker, []
    ).

start_client() ->
    {ok, []} = application:ensure_all_started(make_proxy),
    {ok, Port} = application:get_env(make_proxy, client_port),
    do_start_client(Port).
do_start_client(undefined) ->
    ignore;
do_start_client(Port) ->
    TransOpts = transport_opts(Port),

    {ok, _} = ranch:start_listener(
        make_proxy_client,
        20,
        ranch_tcp,
        TransOpts,
        mp_client_worker, []
    ).
transport_opts(Port) ->
    [{port, Port}, {max_connections, infinity}].
