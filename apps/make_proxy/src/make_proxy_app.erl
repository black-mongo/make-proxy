%%%-------------------------------------------------------------------
%% @doc make_proxy public API
%% @end
%%%-------------------------------------------------------------------

-module(make_proxy_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

%%====================================================================
%% API
%%====================================================================

start(_StartType, _StartArgs) ->
    cool_tools_logger:start_default_log(true),
    cool_tools_logger:set_global_loglevel(debug),
    ets:new(make_proxy_counters, [named_table, set, public]),
    make_proxy:start(),
    make_proxy_sup:start_link().

%%--------------------------------------------------------------------
stop(_State) ->
    ok.

%%====================================================================
%% Internal functions
%%====================================================================
