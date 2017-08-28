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

-module(xrouter_server).
-author("pouriya.jahanbakhsh@gmail.com").

%% ---------------------------------------------------------------------
%% Exports:





%% API:
-export([start_link/4
        ,stop/1]).





%% sockerl's exports:
-export([listen_init/2
        ,connector_init/2
        ,handle_packet/3
        ,handle_call/4
        ,handle_cast/3
        ,handle_event/3
        ,handle_info/3
        ,handle_disconnect/2
        ,terminate/3
        ,timeout/2
        ,srtimeout/2
        ,code_change/3]).





%% ---------------------------------------------------------------------
%% Records & Macros & Includes:





-define(STATE, xrouter_server_state).
-record(?STATE, {callback
                ,data
                ,parser
                ,state
                ,jid
                ,id}).

-define(DEF_SERVER_OPTS, [active, binary, {reuseaddr, true}]).

-define(STREAM_TIMEOUT, 2000).
-define(HANDSHAKE_TIMEOUT, 3000).
-define(DEFAULT_TERMINATE_TIMEOUT, 2000).





%% I need:
%%  #xmlel{}
%%  #xmlstreamstart{}
%%  #xmlcdata{}
%%  #xmlstreamend{}
%%  #xmpp_utils_jid{}
-include_lib("xmpp_utils/include/xmpp_utils.hrl").





%% I need:
%%  #sockerl_metadat{}
-include_lib("sockerl/include/sockerl.hrl").





%% ---------------------------------------------------------------------





-spec
start_link(atom()
          ,module()
          ,sockerl_types:port_number()
          ,sockerl_types:start_options()) ->
    sockerl_types:start_return().
start_link(Name, Mod, Port, Opts)
    when erlang:is_atom(Name) andalso
         erlang:is_atom(Mod) andalso
         erlang:is_integer(Port) andalso
         erlang:is_list(Opts) ->
    {ServOpts, Opts2} =
        case lists:keytake(server_options, 1, Opts) of
            {_, {_, ServOpts2}, Opts3} ->
                {ServOpts2++?DEF_SERVER_OPTS, Opts3};
            false ->
                {?DEF_SERVER_OPTS, Opts}
        end,
    ServOpts3 = [{connector_childspec_plan, [delete]}|ServOpts],
    sockerl:start_link_server({local, Name}
                             ,?MODULE
                             ,{Mod, Opts2}
                             ,Port
                             ,ServOpts3).







-spec
stop(atom() | pid()) ->
    ok.
stop(Server) ->
    Terminate =
        fun({_, Pid}) ->
            sys:terminate(Pid
                         ,normal
                         ,?DEFAULT_TERMINATE_TIMEOUT)
        end,
    Pids = lists:reverse([{undefined, Server}
                         |sockerl:get_server_connections(Server)]),
    ok = lists:foreach(Terminate, Pids).





%% ---------------------------------------------------------------------
%% sockerl's callbcaks:





listen_init({Mod, Opts}, _LSock) ->
    case Mod:init(Opts) of
        {ok, Data} ->
            {ok, #?STATE{callback = Mod, data = Data}};
        {stop, Reason} ->
            {stop, Reason};
        {'EXIT', Reason} ->
            {stop, Reason};
        Other ->
            {stop, {bad_return_value, [{returned_value, Other}
                                      ,{module, Mod}
                                      ,{function, init}
                                      ,{options, Opts}]}}
    end.







connector_init(State, _SMD) ->
    {ok, Parser} = exml_stream:new_parser(),
    {ok, [{state, State#?STATE{parser = Parser, state = undefined}}
         ,{timeout, ?STREAM_TIMEOUT}]}.







handle_packet(Pkt
             ,#?STATE{parser = Parser}=State
             ,_SMD) ->
    case exml_stream:parse(Parser, Pkt) of
        {ok, Parser2, Stanzas} ->
            parse_stanza(State#?STATE{parser = Parser2}, Stanzas, []);
        {error, Reason} ->
            Reason2 = {xml_parsing, [{reason, Reason}, {packet, Pkt}]},
            ErrPkt =
                stream_error(<<"bad-request">>
                            ,<<"Error in parsing your XML packets">>),
            {stop, Reason2, [{packet, ErrPkt}]}
    end.







handle_call(Req
           ,From
           ,#?STATE{callback = Mod, data = Data}=State
           ,_SMD) ->
    case catch Mod:handle_call(Req, From, Data) of
        ok ->
            ok;

        {ok, RetOpts} when erlang:is_list(RetOpts) ->
            {Data2, RetOpts2} = concat(Data, [], RetOpts),
            State2 = State#?STATE{data= Data2},
            {ok, [{state, State2}|lists:reverse(RetOpts2)]};

        close ->
            close;

        {close, RetOpts} when erlang:is_list(RetOpts) ->
            {Data2, RetOpts2} = concat(Data, [], RetOpts),
            State2 = State#?STATE{data= Data2},
            {close, [{state, State2}|lists:reverse(RetOpts2)]};

        {stop, Reason} ->
            {stop, Reason};

        {stop, Reason, RetOpts} when erlang:is_list(RetOpts) ->
            {Data2, RetOpts2} = concat(Data, [], RetOpts),
            State2 = State#?STATE{data = Data2},
            {stop, Reason, [{state, State2}|lists:reverse(RetOpts2)]};

        {'EXIT', Reason} ->
            Reason2 = {callback_crash, [{reason, Reason}
                                       ,{module, Mod}
                                       ,{function, handle_call}
                                       ,{request, Req}
                                       ,{from, From}
                                       ,{state, Data}]},
            ErrPkt = stream_error(<<"internal-server-error">>),
            {stop, Reason2, [{packet, ErrPkt}]};

        Other ->
            Reason = {callback_bad_return_value
                     ,[{returned_value, Other}
                      ,{module, Mod}
                      ,{function, handle_call}
                      ,{request, Req}
                      ,{from, From}
                      ,{state, Data}]},
            ErrPkt = stream_error(<<"internal-server-error">>),
            {stop, Reason, [{packet, ErrPkt}]}
    end.







handle_info(Msg, #?STATE{callback = Mod, data = Data}=State, _SMD) ->
    case catch Mod:handle_info(Msg, Data) of
        ok ->
            ok;

        {ok, RetOpts} when erlang:is_list(RetOpts) ->
            {Data2, RetOpts2} = concat(Data, [], RetOpts),
            State2 = State#?STATE{data= Data2},
            {ok, [{state, State2}|lists:reverse(RetOpts2)]};

        close ->
            close;

        {close, RetOpts} when erlang:is_list(RetOpts) ->
            {Data2, RetOpts2} = concat(Data, [], RetOpts),
            State2 = State#?STATE{data= Data2},
            {close, [{state, State2}|lists:reverse(RetOpts2)]};

        {stop, Reason} ->
            {stop, Reason};

        {stop, Reason, RetOpts} when erlang:is_list(RetOpts) ->
            {Data2, RetOpts2} = concat(Data, [], RetOpts),
            State2 = State#?STATE{data = Data2},
            {stop, Reason, [{state, State2}|lists:reverse(RetOpts2)]};

        {'EXIT', Reason} ->
            Reason2 = {callback_crash, [{reason, Reason}
                                       ,{module, Mod}
                                       ,{function, handle_info}
                                       ,{message, Msg}
                                       ,{state, Data}]},
            ErrPkt = stream_error(<<"internal-server-error">>),
            {stop, Reason2, [{packet, ErrPkt}]};

        Other ->
            Reason = {callback_bad_return_value
                     ,[{returned_value, Other}
                      ,{module, Mod}
                      ,{function, handle_info}
                      ,{message, Msg}
                      ,{state, Data}]},
            ErrPkt = stream_error(<<"internal-server-error">>),
            {stop, Reason, [{packet, ErrPkt}]}
    end.







handle_cast(Req, State, SMD) ->
    handle_info({'$gen_cast', Req}, State, SMD).







handle_event(Event, State, SMD) ->
    handle_info({'$gen_event', Event}, State, SMD).







handle_disconnect(_State, _SMD) ->
    close.







terminate(Reason
         ,#?STATE{callback = Mod
                 ,data = Data
                 ,state = CState}
         ,SMD) ->
    case CState of
        undefined ->
            ok;
        _ ->
            _ = sockerl_socket:send(sockerl_metadata:
                                    get_transporter(SMD)
                                   ,sockerl_metadata:get_socket(SMD)
                                   ,<<"</stream:stream>">>
                                   ,sockerl_metadata:get_options(SMD))
    end,
    _ =  Mod:terminate(Reason, Data),
    ok.







timeout(_State, #sockerl_metadata{timeout = ?STREAM_TIMEOUT}) ->
    ErrPkt = stream_error(<<"conflict">>
                         ,<<"Openning stream timeout">>),
    {stop, stream_timeout, [{packet, ErrPkt}]};
timeout(_State, #sockerl_metadata{timeout = ?HANDSHAKE_TIMEOUT}) ->
    ErrPkt = stream_error(<<"conflict">>, <<"handshake timeout">>),
    {stop, handshake_timeout, [{packet, ErrPkt}]}.






srtimeout(_State, _SMD) ->
    ok.







code_change(_OldVsn, State, _Extra) ->
    {ok, State}.





%% ---------------------------------------------------------------------
%% Internal functions:





parse_stanza(#?STATE{state = authenticated
                    ,callback = Mod
                    ,data = Data}=State
            ,[#xmlel{}=XMLEl|Rest]
            ,RetOpts) ->
    XMPPXML = xmpp_utils:parse_xml(XMLEl),
    case catch Mod:handle_xmpp_xml(XMPPXML, Data) of
        ok ->
            parse_stanza(State, Rest, RetOpts);

        {ok, RetOpts2} when erlang:is_list(RetOpts2) ->
            {Data2, RetOpts3} = concat(Data, RetOpts, RetOpts2),
            State2 = State#?STATE{data= Data2},
            parse_stanza(State2, Rest, RetOpts3);

        close ->
            State2 = State#?STATE{data= Data},
            {close, [{state, State2}|lists:reverse(RetOpts)]};

        {close, RetOpts2} when erlang:is_list(RetOpts2) ->
            {Data2, RetOpts3} = concat(Data, RetOpts, RetOpts2),
            State2 = State#?STATE{data= Data2},
            {close, [{state, State2}|lists:reverse(RetOpts3)]};

        {stop, Reason} ->
            State2 = State#?STATE{data= Data},
            {stop, Reason, [{state, State2}|lists:reverse(RetOpts)]};

        {stop, Reason, RetOpts2} when erlang:is_list(RetOpts2) ->
            {Data2, RetOpts3} = concat(Data, RetOpts, RetOpts2),
            State2 = State#?STATE{data= Data2},
            {stop, Reason, [{state, State2}|lists:reverse(RetOpts3)]};

        {'EXIT', Reason} ->
            Reason2 = {callback_crash, [{reason, Reason}
                                       ,{module, Mod}
                                       ,{function, handle_xmpp_xml}
                                       ,{xmpp_xml, XMPPXML}
                                       ,{state, Data}]},
            ErrPkt = stream_error(<<"internal-server-error">>),
            {stop, Reason2, [{packet, ErrPkt}|RetOpts]};

        Other ->
            Reason = {callback_bad_return_value
                     ,[{returned_value, Other}
                      ,{module, Mod}
                      ,{function, handle_xmpp_xml}
                      ,{xmpp_xml, XMPPXML}
                      ,{state, Data}]},
            ErrPkt = stream_error(<<"internal-server-error">>),
            {stop, Reason, [{packet, ErrPkt}|RetOpts]}
    end;
parse_stanza(State, [], RetOpts) ->
    {ok, [{state, State}|lists:reverse(RetOpts)]};
parse_stanza(#?STATE{state = undefined, data = Data}=State
            ,[#xmlstreamstart{name = <<"stream:stream">>
                             ,attrs = Attrs}|Rest]
            ,RetOpts) ->
    case validate_to_attr(Attrs) of
        {ok, To} ->
            {_, _, Int} = os:timestamp(),
            Id = erlang:integer_to_binary(Int),
            StreamAttrs = [{<<"xmlns">>, <<"jabber:component:accept">>}
                          ,{<<"xmlns:stream">>
                           ,<<"http://etherx.jabber.org/streams">>}],
            Pkt = xmpp_utils:make_pkt(<<"stream:stream">>
                                     ,To
                                     ,undefined
                                     ,undefined
                                     ,Id
                                     ,[]
                                     ,StreamAttrs),
            RetOpts2 = [{timeout, ?HANDSHAKE_TIMEOUT}
                       ,{packet, Pkt} | RetOpts],
            parse_stanza(State#?STATE{state = handshake
                                     ,jid = To
                                     ,id = Id}
                        ,Rest
                        ,RetOpts2);
        {error, not_found} ->
            Reason = {openning_stream, [{reason, to_attr_not_found}
                                       ,{attrs, Attrs}]},
            ErrPkt = stream_error(<<"conflict">>
                                 ,<<"'to' attribute could not found">>),
            State2 = State#?STATE{data = Data},
            RetOpts2 = [{state, State2}
                       ,{packet, ErrPkt}
                       |lists:reverse(RetOpts)],
            {stop, Reason, RetOpts2};
        {error, bad_value} ->
            Reason = {openning_stream, [{reason, bad_to_attr}
                                       ,{attrs, Attrs}]},
            ErrPkt = stream_error(<<"conflict">>
                                 ,<<"Bad value for 'to' attribute">>),
            State2 = State#?STATE{data = Data},
            RetOpts2 = [{state, State2}
                       ,{packet, ErrPkt}
                       |lists:reverse(RetOpts)],
            {stop, Reason, RetOpts2}
    end;
parse_stanza(#?STATE{state = handshake
                    ,callback = Mod
                    ,jid = Jid
                    ,data = Data
                    ,id = Id}=State
            ,[#xmlel{name = <<"handshake">>
                    ,attrs = []
                    ,children = [#xmlcdata{content = HS}]}|Rest]
            ,RetOpts) ->
    case catch Mod:authenticate(Jid, Id, HS, Data) of
        ok ->
            State2 = State#?STATE{state = authenticated},
            RetOpts2 = [{timeout, infinity}
                       ,{packet, <<"<handshake/>">>}
                       |RetOpts],
            parse_stanza(State2, Rest, RetOpts2);

        {ok, RetOpts2} when erlang:is_list(RetOpts2) ->
            {Data2, RetOpts3} = concat(Data
                                      ,[{packet, <<"<handshake/>">>}
                                       |RetOpts]
                                      ,RetOpts2),
            State2 = State#?STATE{state = authenticated
                                 ,data = Data2},
            parse_stanza(State2, Rest, RetOpts3);

        close ->
            State2 = State#?STATE{data = Data},
            RetOpts2 = [{state, State2} |lists:reverse(RetOpts)],
            {close, RetOpts2};

        {close, RetOpts2} when erlang:is_list(RetOpts2) ->
            {Data2, RetOpts3} = concat(Data, RetOpts, RetOpts2),
            State2 = State#?STATE{data= Data2},
            {close, [{state, State2}|lists:reverse(RetOpts3)]};

        {stop, Reason} ->
            State2 = State#?STATE{data= Data},
            {stop, Reason, [{state, State2}|lists:reverse(RetOpts)]};

        {stop, Reason, RetOpts2} when erlang:is_list(RetOpts2) ->
            {Data2, RetOpts3} = concat(Data, RetOpts, RetOpts2),
            State2 = State#?STATE{data= Data2},
            {stop, Reason, [{state, State2}|lists:reverse(RetOpts3)]};

        {'EXIT', Reason} ->
            Reason = {callback_crash, [{reason, Reason}
                                      ,{module, Mod}
                                      ,{function, autheticate}
                                      ,{jid_binary, Jid}
                                      ,{id, Id}
                                      ,{handshake_data, HS}
                                      ,{state, Data}]},
            ErrPkt = stream_error(<<"internal-server-error">>),
            State2 = State#?STATE{data = Data},
            RetOpts2 = [{state, State2}
                       ,{packet, ErrPkt}
                       |lists:reverse(RetOpts)],
            {stop, Reason, RetOpts2};

        Other ->
            Reason = {callback_bad_return_value
                     ,[{returned_value, Other}
                      ,{module, Mod}
                      ,{function, autheticate}
                      ,{jid_binary, Jid}
                      ,{id, Id}
                      ,{handshake_data, HS}
                      ,{state, Data}]},
            Pkt = stream_error(<<"internal-server-error">>),
            State2 = State#?STATE{data = Data},
            RetOpts2 = [{state, State2}
                       ,{packet, Pkt}
                       |lists:reverse(RetOpts)],
            {stop, Reason, RetOpts2}
    end;
parse_stanza(#?STATE{state = undefined, data = Data}=State
            ,[XMLEl|_Rest]
            ,RetOpts) ->
    Reason = {openning_stream, [{reason, xml_not_well_formed}
                               ,{xml, XMLEl}]},
    ErrPkt = stream_error(<<"conflict">>, <<"XML not well formed">>),
    State2 = State#?STATE{data = Data},
    RetOpts2 = [{state, State2}
               ,{packet, ErrPkt}
               |lists:reverse(RetOpts)],
    {stop, Reason, RetOpts2};
parse_stanza(#?STATE{state = handshake, data = Data}=State
            ,[#xmlel{name = <<"handshake">>}=XMLEl|_Rest]
            ,RetOpts) ->
    Reason = {handshaking, [{reason, xml_not_well_formed}
                           ,{xml, XMLEl}]},
    Pkt = stream_error(<<"conflict">>, <<"XML not well formed">>),
    State2 = State#?STATE{data = Data},
    RetOpts2 = [{state, State2}
               ,{packet, Pkt}
               |lists:reverse(RetOpts)],
    {stop, Reason, RetOpts2};
parse_stanza(#?STATE{state = authenticated, data = Data}=State
            ,[#xmlstreamend{}|_Rest]
            ,RetOpts) ->
    State2 = State#?STATE{data = Data},
    RetOpts2 = [{state, State2}|lists:reverse(RetOpts)],
    {stop, stream_closed, RetOpts2};
parse_stanza(#?STATE{data = Data}=State, [XMLEl|_XMLs], RetOpts) ->
    Reason = {xml, [{reason, xml_not_well_formed}, {xml, XMLEl}]},
    State2 = State#?STATE{data = Data},
    ErrPkt = stream_error(<<"conflict">>, <<"XML not well formed">>),
    RetOpts2 = [{state, State2}
               ,{packet, ErrPkt}
               |lists:reverse(RetOpts)],
    {stop, Reason, RetOpts2}.






validate_to_attr(Attrs) ->
    case lists:keyfind(<<"to">>, 1, Attrs) of
        {_, ToBin} ->
            case xmpp_utils:parse_jid(ToBin) of
                #xmpp_utils_jid{username = <<>>
                               ,server = <<>>
                               ,resource = <<>>} ->
                    {error, bad_value};
                _ ->
                    {ok, ToBin}
            end;
        false ->
            {error, not_found}
    end.







concat(_Data, RetOpts, [{state, Data2}|RetOpts2]) ->
    concat(Data2, RetOpts, RetOpts2);
concat(Data, RetOpts, [RetOpt|RetOpts2]) ->
    concat(Data, [RetOpt|RetOpts], RetOpts2);
concat(Data, RetOpts, []) ->
    {Data, RetOpts}.







stream_error(Err, Txt) ->
    xmpp_utils:make_pkt(<<"stream:error">>
                       ,undefined
                       ,undefined
                       ,undefined
                       ,undefined
                       ,[xmpp_utils:make_xmpp_error(Err, Txt)]).







stream_error(Err) ->
    xmpp_utils:make_pkt(<<"stream:error">>
                       ,undefined
                       ,undefined
                       ,undefined
                       ,undefined
                       ,[xmpp_utils:make_xmpp_error(Err)]).