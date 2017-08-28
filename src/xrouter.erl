%%% --------------------------------------------------------------------
%%% BSD 3-Clause License
%%%
%%% Copyright (c) 2017-2018, Soroush
%%% soroush-app.ir
%%% All rights reserved.
%%%
%%% Redistribution and use in source and binary forms, with or without
%%% modification, are permitted provided that the following conditions
%%% are met:
%%%
%%% 1. Redistributions of source code must retain the above copyright
%%% notice, this list of conditions and the following disclaimer.
%%%
%%% 2. Redistributions in binary form must reproduce the above copyright
%%% notice, this list of conditions and the following disclaimer in the
%%% documentation and/or other materials provided with the distribution.
%%%
%%% 3. Neither the name of the copyright holder nor the names of its
%%% contributors may be used to endorse or promote products derived from
%%% this software without specific prior written permission.
%%%
%%% THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
%%% "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
%%% LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS
%%% FOR A  PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE
%%% COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT,
%%% INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING,
%%% BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
%%% LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
%%% CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
%%% LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN
%%% ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
%%% POSSIBILITY OF SUCH DAMAGE.
%%% --------------------------------------------------------------------
%% @author   Pouriya Jahanbakhsh <pouriya.jahanbakhsh@gmail.com>
%% @version  17.8.27
%% ---------------------------------------------------------------------

-module(xrouter).
-author("pouriya.jahanbakhsh@gmail.com").

%% ---------------------------------------------------------------------
%% Exports:





%% API:
-export([start/0
        ,stop/0
        ,start_server/4
        ,start_link_server/4
        ,get_servers/0
        ,stop_server/1
        ,validate_secret_secret/3]).





%% application's Exports:
-export([start/2
        ,stop/1]).





%% ---------------------------------------------------------------------
%% Records & Macros & Includes:





%% I need:
%%  #xmpp_utils_xml{}
-include_lib("xmpp_utils/include/xmpp_utils.hrl").





%% ---------------------------------------------------------------------
%% Behavior info:





-callback
init(Opts::list()) ->
    {'ok', State::any()} | {'stop', Reason::any()}.





-callback
authenticate(Jid::binary()
            ,Id::binary()
            ,HandshakeData::binary()
            ,State::any()) ->
    'ok'                           |
    {'ok', [Opts]}                 |
    'close'                        |
    {'close', [Opts]}              |
    {'stop', Reason::any()}        |
    {'stop', Reason::any(), [Opts]}
when
    Opts :: {'packet', Pkt::binary()} | {'state', State2::binary()}.





-callback
handle_xmpp_xml(XMPP_XML::#xmpp_utils_xml{}, State::any()) ->
    'ok'                           |
    {'ok', [Opts]}                 |
    'close'                        |
    {'close', [Opts]}              |
    {'stop', Reason::any()}        |
    {'stop', Reason::any(), [Opts]}
when
    Opts :: {'packet', Pkt::binary()} | {'state', State2::binary()}.





-callback
handle_call(Request::any(), From::tuple(), State::any()) ->
    'ok'                           |
    {'ok', [Opts]}                 |
    'close'                        |
    {'close', [Opts]}              |
    {'stop', Reason::any()}        |
    {'stop', Reason::any(), [Opts]}
when
    Opts :: {'packet', Pkt::binary()}
          | {'state', State2::binary()}
          | {'reply', From::tuple(), Reply::any()}.





-callback
handle_info(Msg::any(), State::any()) ->
    'ok'                           |
    {'ok', [Opts]}                 |
    'close'                        |
    {'close', [Opts]}              |
    {'stop', Reason::any()}        |
    {'stop', Reason::any(), [Opts]}
when
    Opts :: {'packet', Pkt::binary()} | {'state', State2::binary()}.





-callback
terminate(Reason::any(), State::any()) ->
    any().





%% ---------------------------------------------------------------------
%% API:





-spec
start() ->
    'ok' | {'error', term()}.
start() ->
    case application:ensure_all_started(?MODULE) of
        {ok, _} ->
            ok;
        {error, _}=Err ->
            Err
    end.







-spec
stop() ->
    'ok' | {'error', term()}.
stop() ->
    ok = lists:foreach(fun stop_server/1
                      ,[Server || {_, Server} <- get_servers()]),
    application:stop(?MODULE).







-spec
start_server(atom()
            ,module()
            ,sockerl_types:port_number()
            ,sockerl_types:start_options()) ->
    sockerl_types:start_return().
start_server(Name, Mod, Port, Opts)
    when erlang:is_atom(Name) andalso
         erlang:is_atom(Mod) andalso
         erlang:is_integer(Port) andalso
         erlang:is_list(Opts) ->
    xrouter_sup:start_server(Name, Mod, Port, Opts).







-spec
start_link_server(atom()
                 ,module()
                 ,sockerl_types:port_number()
                 ,sockerl_types:start_options()) ->
    sockerl_types:start_return().
start_link_server(Name, Mod, Port, Opts)
    when erlang:is_atom(Name) andalso
         erlang:is_atom(Mod) andalso
         erlang:is_integer(Port) andalso
         erlang:is_list(Opts) ->
    xrouter_server:start_link(Name, Mod, Port, Opts).







-spec
get_servers() ->
    [] | [{atom(), pid()}].
get_servers() ->
    xrouter_sup:get_servers().







stop_server(Server)
    when erlang:is_atom(Server) orelse
         erlang:is_pid(Server) ->
    xrouter_server:stop(Server).







-spec
validate_secret_secret(binary(), binary(), binary()) ->
    boolean().
validate_secret_secret(Id, HandshakeData, SecretKey)
    when erlang:is_binary(Id) andalso
         erlang:is_binary(HandshakeData) andalso
         erlang:is_binary(SecretKey) ->
    <<Sha1:160/big-unsigned-integer>> =
        crypto:hash(sha, <<Id/binary, SecretKey/binary>>),
    case erlang:iolist_to_binary(io_lib:format("~40.16.0b", [Sha1])) of
        HandshakeData ->
            true;
        _ ->
            false
    end.






%% ---------------------------------------------------------------------
%% application's callbacks:





start(_Type, _InitArg) ->
    xrouter_sup:start_link().







stop(_State) ->
    ok.