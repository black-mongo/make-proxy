%%%-------------------------------------------------------------------
%%% @author black-mongo

%%% Copyright (c) 2021 by black-mongo(black-mongo0112@gmail.com), All Rights Reserved.
%%%
%%% Licensed under the Apache License, Version 2.0 (the "License");
%%% you may not use this file except in compliance with the License.
%%% You may obtain a copy of the License at
%%%
%%%       https://www.apache.org/licenses/LICENSE-2.0
%%%
%%% Unless required by applicable law or agreed to in writing, software
%%% distributed under the License is distributed on an "AS IS" BASIS,
%%% WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%%% See the License for the specific language governing permissions and
%%% limitations under the License.
%%%

%%% @doc
%%%
%%% @end
%%% Created : 2022-03-30T07:08:49+00:00
%%%-------------------------------------------------------------------
-module(mp_client_xmpp).

-author("black-mongo").

-behaviour(mp_client_protocol).

-include_lib("exml/include/exml.hrl").

-export([detect_head/1, request/2, response/2]).

-include("mp_client.hrl").
-include("mp_http_request.hrl").

detect_head($<) ->
    true;
detect_head(_) ->
    false.

request(Data,
        #client{key = _Key,
                socket = Socket,
                transport = Transport,
                remote = undefined,
                buffer = Buffer} =
            State) ->
    Data1 = <<Buffer/binary, Data/binary>>,
    case find_target(Data1) of
        {ok, Target, Body} ->
            case mp_client_utils:connect_to_remote(Target) of
                {ok, Remote} ->
                    %%                    EncryptedTarget = mp_crypto:encrypt(Key, term_to_binary(Target)),
                    %%                    ok = gen_tcp:send(Remote, EncryptedTarget),
                    case Body of
                        <<>> ->
                            {ok, State#client{remote = Remote}};
                        _ ->
                            send(Body, State#client{remote = Remote})
                    end;
                {error, Reason} ->
                    {error, Reason}
            end;
        {error, Reason} ->
            {error, Reason};
        more ->
            case byte_size(Buffer) =:= 0 of
                true ->
                    % reply success info
                    ok = Transport:send(Socket, <<5, 0>>);
                false ->
                    ok
            end,
            {ok, State#client{buffer = Data1}}
    end;
request(Data, State) ->
    send(Data, State).

send(Data, #client{remote = Remote, ok = OK} = State) ->
    {ok, S, Sended} = do_stanza(send, Data, State),
    case Sended of
        true ->
            ok;
        _ ->
            case OK of
                tcp ->
                    ok = gen_tcp:send(Remote, Data);
                _ ->
                    ok = ssl:send(Remote, Data)
            end
    end,
    {ok, S}.

do_stanza(Key, Data, #client{handle_state = HandleState} = State) ->
    %%    ?Debug("key=~p,data=~p",[Key, Data]),
    Parse = get_parse(Key, HandleState),
    {ok, Parse1, L} = exml_stream:parse(Parse, Data),
    {S1, Sended} = log(L, Key, State),
    {ok, S1#client{handle_state = HandleState#{Key => Parse1}}, Sended}.

log(L, Key, State) ->
    {_, S, Sended} = lists:foldl(fun log/2, {Key, State, false}, L),
    {S, Sended}.

log(Row, {Key, State, Sended}) ->
    case Key == recv andalso Row of
        #xmlel{name = <<"iq">>,
               children =
                   [#xmlel{name = <<"bind">>,
                           children =
                               [#xmlel{name = <<"jid">>,
                                       children = [#xmlcdata{content = Jid}]}]}]} ->
            ID = get_id(),
            set_id(<<ID/binary, ",", Jid/binary>>);
        _ ->
            ok
    end,
    ?Debug("(~ts) => ~p:~ts~n", [get_id(), Key, to_binary(Row)]),
    {NewS, Sended1} = process_tls(Row, Key, State),
    {Key, NewS, Sended orelse Sended1}.

get_id() ->
    case erlang:get(id) of
        undefined ->
            set_id();
        ID ->
            ID
    end.

set_id() ->
    ID = base64:encode(
             crypto:strong_rand_bytes(8)),
    set_id(ID).

set_id(ID) ->
    erlang:put(id, ID),
    ID.

get_parse(Key, HandleState) ->
    {ok, P} = exml_stream:new_parser(),
    maps:get(Key, HandleState, P).

response(Data, State) ->
    do_stanza(recv, Data, State).

find_target(Data) ->
    Host = application:get_env(make_proxy, im, "im.server"),
    {ok, {Host, 5222}, Data}.

process_tls(#xmlel{name = <<"proceed">>,
                   attrs = [{<<"xmlns">>, <<"urn:ietf:params:xml:ns:xmpp-tls">>}]} =
                Stanza,
            recv,
            #client{socket = Socket, transport = Transport} = State) ->
    erlang:spawn_link(fun() ->
                         timer:sleep(10),
                         ok = Transport:send(Socket, exml:to_binary(Stanza))
                      end),
    S = starttls(State),
    {S, true};
%%process_tls(#xmlel{name = <<"starttls">>, attrs = [{<<"xmlns">>, <<"urn:ietf:params:xml:ns:xmpp-tls">>}]}, send, State) ->
%%    process_tls(#xmlel{name = <<"proceed">>, attrs = [{<<"xmlns">>, <<"urn:ietf:params:xml:ns:xmpp-tls">>}]}, recv, State);
process_tls(_, _, State) ->
    {State, false}.

%% proxy start tls
starttls(#client{socket = Socket} = Client) ->
   Tls = case application:get_env(make_proxy, tls) of
         undefined -> 
           {CertFile, KeyFile} = make_proxy:list_ca_file(),
          [{cacertfile, CertFile},
           {cert, get_certs([<<"*.example.com">>, <<"localhost">>])},
           {keyfile, KeyFile}];
         {ok, Tls0} ->
            Tls0
    end,
    {ok, TLSSocket} = ssl:handshake(Socket, Tls),
    ok = ssl:setopts(TLSSocket, [{active, once}]),
    upgrade_to_tls(Client#client{socket = TLSSocket,
                                 transport = ssl,
                                 ok = ssl,
                                 closed = ssl_closed}).

get_certs(L) ->
[mp_client_http:certfile_to_cert(make_proxy:get_cert_pem(Host)) || Host <- L].

%% remote socket upgrade to tls
upgrade_to_tls(#client{remote = Remote} = S) ->
    {ok, TlsSocket} = ssl:connect(Remote, [{reuse_sessions, true},{verify, verify_none}]),
    S#client{handle_state = #{}, remote = TlsSocket}.

to_binary(Value) ->
    re:replace(
        exml:to_binary(Value), "&quot;", "\"", [global, {return, binary}]).
