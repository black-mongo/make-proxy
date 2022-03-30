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

-export([detect_head/1,
    request/2
    ]).

-include("mp_client.hrl").
-include("mp_http_request.hrl").

detect_head(_) ->
    true.

request(Data,
    #client{key = Key, socket = Socket, transport = Transport,
        remote = undefined, buffer = Buffer} = State) ->

    Data1 = <<Buffer/binary, Data/binary>>,
    case find_target(Data1) of
        {ok, Target, Body} ->
            case mp_client_utils:connect_to_remote() of
                {ok, Remote} ->
                    EncryptedTarget = mp_crypto:encrypt(Key, term_to_binary(Target)),
                    ok = gen_tcp:send(Remote, EncryptedTarget),
                    case Body of
                        <<>> -> ok;
                        _ ->
                            ok = gen_tcp:send(Remote, mp_crypto:encrypt(Key, Body))
                    end,
                    {ok, State#client{remote = Remote}};
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

request(Data, #client{key = Key, remote = Remote} = State) ->
    ok = gen_tcp:send(Remote, mp_crypto:encrypt(Key, Data)),
    {ok, State}.
find_target(Data) ->
    Host = application:get_env(make_proxy, im, "im.server"),
    {ok, {Host, 5222}, Data}.