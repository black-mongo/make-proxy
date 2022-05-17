%%%-------------------------------------------------------------------
%%% @author wang
%%% @copyright (C) 2016, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 13. Oct 2016 下午2:28
%%%-------------------------------------------------------------------
-module(mp_client_utils).
-author("wang").

%% API
-export([connect_to_remote/1,connect_to_remote/0]).

connect_to_remote({Host, Port, true}) ->
    ssl:connect(Host, Port, [binary, {active, once}]);
connect_to_remote({Host, Port, _}) ->
    connect_to_remote({Host, Port});
connect_to_remote({Host, Port}) ->
    {ok, Addr} = inet:getaddr(Host, inet),
    gen_tcp:connect(Addr, Port, [binary, {active, once}]).

connect_to_remote() ->
    {ok, RemoteAddr} = application:get_env(make_proxy, server_addr),
    {ok, RemotePort} = application:get_env(make_proxy, server_port),
    {ok, Addr} = inet:getaddr(RemoteAddr, inet),
    gen_tcp:connect(Addr, RemotePort, [binary, {active, once}, {packet, 4}]).
