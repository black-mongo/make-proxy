-module(ca).

-export([new/2, gen_ca/1, gen_cert_pem/2, list_cert_pem/1, list_ca_file/1, get_cainfo/0]).

-include("cargo.hrl").

-on_load init/0.

-define(NOT_LOADED, not_loaded(?LINE)).

%%%===================================================================
%%% API
%%%===================================================================
%%%
new(_CaCertFile, _CaKeyFile) ->
    ?NOT_LOADED.

gen_cert_pem(_Ref, _Host) ->
    ?NOT_LOADED.

list_cert_pem(_Ref) ->
    ?NOT_LOADED.

list_ca_file(_Ref) ->
    ?NOT_LOADED.

get_cainfo() ->
    ?NOT_LOADED.

-spec gen_ca(CaInfo :: #ca_info{}) -> ok | {error, binary}.
gen_ca(_CaInfo) ->
    ?NOT_LOADED.

%%%===================================================================
%%% NIF
%%%===================================================================

init() ->
    ?load_nif_from_crate(ca, 0).

not_loaded(Line) ->
    erlang:nif_error({not_loaded, [{module, ?MODULE}, {line, Line}]}).

%%%===================================================================
%%% Tests
%%%===================================================================

-ifdef(TEST).

-include_lib("eunit/include/eunit.hrl").

-endif.
