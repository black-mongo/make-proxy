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

-include("cargo.hrl").

%% API
-export([start_server/0, start_client/0, get_cert_pem/1, list_ca_file/0, start/0]).

start() ->
    Dir = code:priv_dir(make_proxy),
    maybe_gen_ca(Dir),
    do_start_server(application:get_env(make_proxy, server_port, undefined)),
    do_start_client(application:get_env(make_proxy, client_port, undefined)).

maybe_gen_ca(Dir) ->
    CacertFile = filename:join(Dir, "cacert.pem"),
    CaKeyFile = filename:join(Dir, "cakey.pem"),
    maybe_gen_ca(Dir,
                 filelib:is_file(CacertFile),
                 filelib:is_file(CaKeyFile),
                 erlang:list_to_binary(CacertFile),
                 erlang:list_to_binary(CaKeyFile)).

maybe_gen_ca(_Dir, true, true, CacertFile, CaKeyFile) ->
    {ok, Ref} = ca:new(CacertFile, CaKeyFile),
    persistent_term:put(ca_resource, Ref),
    ok;
maybe_gen_ca(Dir, _, _, CacertFile, CaKeyFile) ->
    ok = ca:gen_ca(#ca_info{output = erlang:list_to_binary(Dir)}),
    {ok, Ref} = ca:new(CacertFile, CaKeyFile),
    persistent_term:put(ca_resource, Ref),
    ok.

get_cert_pem(Host) when is_binary(Host) ->
    Ref = persistent_term:get(ca_resource),
    ca:gen_cert_pem(Ref, Host).

list_ca_file() ->
    ca:list_ca_file(
        persistent_term:get(ca_resource)).

start_server() ->
    {ok, []} = application:ensure_all_started(make_proxy),
    {ok, Port} = application:get_env(make_proxy, server_port),
    do_start_server(Port).

do_start_server(undefined) ->
    igore;
do_start_server(Port) ->
    TransOpts = transport_opts(Port),
    {ok, _} =
        ranch:start_listener(make_proxy_server, ranch_tcp, TransOpts, mp_server_worker, []).

start_client() ->
    {ok, []} = application:ensure_all_started(make_proxy),
    {ok, Port} = application:get_env(make_proxy, client_port),
    do_start_client(Port).

do_start_client(undefined) ->
    ignore;
do_start_client(Port) ->
    TransOpts = transport_opts(Port),
    {ok, _} =
        ranch:start_listener(make_proxy_client, ranch_tcp, TransOpts, mp_client_worker, []).

transport_opts(Port) ->
    #{socket_opts => [{port, Port}], max_connections => infinity}.
