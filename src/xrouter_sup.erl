%%% ------------------------------------------------------------------------------------------------
%%% Xrouter is available for use under the following license, commonly known as the 3-clause  (or
%%% "modified") BSD license:
%%%
%%% Copyright (c) 2017-2018, Soroush
%%% http://soroush-app.ir
%%% All rights reserved.
%%%
%%% Redistribution and use in source and binary forms, with or without modification, are permitted
%%% provided that the following conditions are met:
%%%
%%% 1. Redistributions of source code must retain the above copyright notice, this list of
%%%    conditions and the following disclaimer.
%%%
%%% 2. Redistributions in binary form must reproduce the above copyright notice, this list of
%%%    conditions and the following disclaimer in the documentation and/or other materials provided
%%%    with the distribution.
%%%
%%% 3. Neither the name of the copyright holder nor the names of its contributors may be used to
%%%    endorse or promote products derived from this software without specific prior written
%%%    permission.
%%%
%%% THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND ANY EXPRESS OR
%%% IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND
%%% FITNESS FOR A  PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR
%%% CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR
%%% CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR
%%% SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
%%% THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR
%%% OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
%%% POSSIBILITY OF SUCH DAMAGE.
%%% ------------------------------------------------------------------------------------------------
%% @author   Pouriya Jahanbakhsh <pouriya.jahanbakhsh@gmail.com>
%% @version  17.9.3
%% -------------------------------------------------------------------------------------------------

-module(xrouter_sup).
-author("pouriya.jahanbakhsh@gmail.com").

%% -------------------------------------------------------------------------------------------------
%% Exports:





%% API:
-export([start_link/0
        ,start_server/4
        ,get_servers/0]).





%% director's Export:
-export([init/1]).





%% -------------------------------------------------------------------------------------------------
%% API:



-spec
start_link() ->
    director:start_rturn().
start_link() ->
    director:start_link({local, ?MODULE}, ?MODULE, undefined).







-spec
start_server(atom(), module(), sokerl_types:port_number(), sockerl_types:start_options()) ->
    director:start_return().
start_server(Name, Mod, Port, Opts) when erlang:is_atom(Name) andalso
                                         erlang:is_atom(Mod) andalso
                                         erlang:is_integer(Port) andalso
                                         erlang:is_list(Opts) ->
    director:start_child(?MODULE, make_childspec(Name, Mod, Port, Opts)).







-spec
get_servers() ->
    [] | [{atom(), pid()}].
get_servers() ->
    director:get_pids(?MODULE).





%% -------------------------------------------------------------------------------------------------
%% director's callback:





init(undefined) ->
    {ok, make_childspecs(get_servers_from_env())}.





%% -------------------------------------------------------------------------------------------------
%% Internal functions:





make_childspecs(Servers) when erlang:is_list(Servers) ->
    [make_childspec(Name, Mod, Port, Opts) || {Name, Mod, Port, Opts} <- Servers].







make_childspec(Name, Mod, Port, Opts) when erlang:is_atom(Name) andalso
                                           erlang:is_atom(Mod) andalso
                                           erlang:is_integer(Port) andalso
                                           erlang:is_list(Opts) ->
    #{id => Name
     ,start => {xrouter_server, start_link, [Name, Mod, Port, Opts]}
     ,count => infinity
     ,plan => [fun childspec_plan_fun/3]
     ,type => supervisor}.







childspec_plan_fun(_Id, {socket_listen, _}, _ResCount) ->
    stop;
childspec_plan_fun(_Id, normal, _ResCount) ->
    delete;
childspec_plan_fun(_Id, _Reason, ResCount) when (ResCount rem 10 == 0) ->
    {restart, 1000};
childspec_plan_fun(_Id, _Reason, _ResCount) ->
    restart.







get_servers_from_env() ->
    EnvOpts = application:get_all_env(xrouter),
    _Servers = proplists:get_value(servers, EnvOpts, []).