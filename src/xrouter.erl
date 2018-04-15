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

-module(xrouter).
-author("pouriya.jahanbakhsh@gmail.com").

%% -------------------------------------------------------------------------------------------------
%% Exports:





%% API:
-export([start_link/5
        ,stop/1
        ,validate_shared_secret/3]).





%% etcp's exports:
-export([listen_init/3
        ,connection_init/2
        ,handle_packet/3
        ,handle_call/4
        ,handle_cast/3
        ,handle_info/3
        ,handle_disconnect/2
        ,terminate/3
        ,code_change/3]).





%% -------------------------------------------------------------------------------------------------
%% Records & Macros & Includes:





-define(STATE, xrouter_server_state).
-record(?STATE, {callback, data, parser, state, jid, id}).

-define(DEF_SOCK_OPTS, [{mode, binary}, {reuseaddr, true}]).
-define(DEF_SERVER_OPTS, [{socket_options, ?DEF_SOCK_OPTS}]).

-define(OPEN_STREAM_TIMEOUT, 5001).
-define(HANDSHAKE_TIMEOUT, 5002).
-define(DEF_TERMINATE_TIMEOUT, 2000).





%% I need:
%%  #xmlel{}
%%  #xmlstreamstart{}
%%  #xmlcdata{}
%%  #xmlstreamend{}
%%  #xmpp_utils_jid{}
-include_lib("xmpp_utils/include/xmpp_utils.hrl").





%% -------------------------------------------------------------------------------------------------
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





-callback
code_change(OldVsn::term(), State::term(), Extra::term()) ->
    {'ok', NewState::term()}.





%% -------------------------------------------------------------------------------------------------





-spec
start_link(atom(), module(), term(), etcp_types:port_number(), etcp_types:start_options()) ->
    etcp_types:start_return().
start_link(Name, Mod, InitArg, Port, Opts) when erlang:is_atom(Name) andalso
    erlang:is_atom(Mod) andalso
    erlang:is_integer(Port) andalso
    erlang:is_map(Opts) ->
    SockOpts =
        case maps:get(transporter_options, Opts, []) of
            [] ->
                ?DEF_SOCK_OPTS;
            SockOpts2 ->
                SockOpts2
        end,
    etcp:start_link_server({local, Name}
                          ,?MODULE
                          ,{Mod, InitArg}
                          ,Port
                          ,Opts#{transporter_options => SockOpts}).







-spec
stop(atom() | pid()) ->
    'ok'.
stop(Server) ->
    Terminate =
        fun({_, Pid}) ->
            sys:terminate(Pid, normal, ?DEF_TERMINATE_TIMEOUT)
        end,
    Pids = lists:reverse([{undefined, Server} | etcp:fetch_server_connections(Server)]),
    ok = lists:foreach(Terminate, Pids).







-spec
validate_shared_secret(binary(), binary(), binary()) ->
    boolean().
validate_shared_secret(Id, HandshakeData, SecretKey) when erlang:is_binary(Id) andalso
    erlang:is_binary(HandshakeData) andalso
    erlang:is_binary(SecretKey) ->
    <<Sha1:160/big-unsigned-integer>> = crypto:hash(sha, <<Id/binary, SecretKey/binary>>),
    case erlang:iolist_to_binary(io_lib:format("~40.16.0b", [Sha1])) of
        HandshakeData ->
            true;
        _ ->
            false
    end.





%% -------------------------------------------------------------------------------------------------
%% etcp's callbcaks:





listen_init({Mod, InitArg}, _, _) ->
    case Mod:init(InitArg) of
        {ok, Data} ->
            {ok, #?STATE{callback = Mod, data = Data}};
        {stop, Reason} ->
            {stop, Reason};
        {'EXIT', Reason} ->
            {stop, Reason};
        Other ->
            {stop, {return, [{value, Other}
                            ,{module, Mod}
                            ,{function, init}
                            ,{arguments, [InitArg]}]}}
    end.







connection_init(State, _) ->
    {ok, Parser} = exml_stream:new_parser(),
    {ok, [{state, State#?STATE{parser = Parser, state = undefined}}
         ,{timeout, ?OPEN_STREAM_TIMEOUT}]}.







handle_packet(Pkt, #?STATE{parser = Parser}=State, _) ->
    case exml_stream:parse(Parser, Pkt) of
        {ok, Parser2, Stanzas} ->
            parse_stanza(State#?STATE{parser = Parser2}, Stanzas, []);
        {error, Reason} ->
            Reason2 = {xml_parsing, [{reason, Reason}, {packet, Pkt}]},
            ErrPkt = stream_error(<<"bad-request">>, <<"Error in parsing your XML packets">>),
            {stop, Reason2, [{packet, ErrPkt}]}
    end.







handle_call(Req, From, #?STATE{callback = Mod, data = Data}=State, _) ->
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
            Reason2 = {crash, [{reason, Reason}
                              ,{module, Mod}
                              ,{function, handle_call}
                              ,{request, Req}
                              ,{from, From}
                              ,{state, Data}]},
            ErrPkt = stream_error(<<"internal-server-error">>),
            {stop, Reason2, [{packet, ErrPkt}]};

        Other ->
            Reason = {return, [{value, Other}
                              ,{module, Mod}
                              ,{function, handle_call}
                              ,{request, Req}
                              ,{from, From}
                              ,{state, Data}]},
            ErrPkt = stream_error(<<"internal-server-error">>),
            {stop, Reason, [{packet, ErrPkt}]}
    end.






handle_info(timeout, _, _) ->
    ErrPkt = stream_error(<<"conflict">>, <<"Timeout">>),
    {stop, timeout, [{packet, ErrPkt}]};
handle_info(Msg, #?STATE{callback = Mod, data = Data}=State, _) ->
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
            Reason2 = {crash, [{reason, Reason}
                              ,{module, Mod}
                              ,{function, handle_info}
                              ,{message, Msg}
                              ,{state, Data}]},
            ErrPkt = stream_error(<<"internal-server-error">>),
            {stop, Reason2, [{packet, ErrPkt}]};

        Other ->
            Reason = {return, [{value, Other}
                              ,{module, Mod}
                              ,{function, handle_info}
                              ,{message, Msg}
                              ,{state, Data}]},
            ErrPkt = stream_error(<<"internal-server-error">>),
            {stop, Reason, [{packet, ErrPkt}]}
    end.







handle_cast(Req, State, SMD) ->
    handle_info({'$gen_cast', Req}, State, SMD).







handle_disconnect(_State, _) ->
    close.







terminate(Reason, #?STATE{callback = Mod, data = Data, state = CState}, SMD) ->
    case CState of
        undefined ->
            ok;
        _ ->
            _ = etcp_transporter:send(etcp_metadata:transporter(SMD)
                                     ,etcp_metadata:socket(SMD)
                                     ,<<"</stream:stream>">>
                                     ,etcp_metadata:transporter_options(SMD))
    end,
    _ =  Mod:terminate(Reason, Data),
    ok.







code_change(OldVsn, #?STATE{callback = Mod, data = Data}=State, Extra) ->
    case Mod:code_change(OldVsn, Data, Extra) of
        {ok, Data2} ->
            {ok, State#?STATE{data = Data2}};
        Other ->
            Other
    end.





%% -------------------------------------------------------------------------------------------------
%% Internal functions:





parse_stanza(#?STATE{state = authenticated, callback = Mod, data = Data}=State
            ,[#xmlel{}=XMLEl | Rest]
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
            Reason2 = {crash, [{reason, Reason}
                              ,{module, Mod}
                              ,{function, handle_xmpp_xml}
                              ,{xmpp_xml, XMPPXML}
                              ,{state, Data}]},
            ErrPkt = stream_error(<<"internal-server-error">>),
            {stop, Reason2, [{packet, ErrPkt}|RetOpts]};

        Other ->
            Reason = {return, [{value, Other}
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
            ,[#xmlstreamstart{name = <<"stream:stream">>, attrs = Attrs} | Rest]
            ,RetOpts) ->
    case validate_to_attr(Attrs) of
        {ok, To} ->
            {_, _, Int} = os:timestamp(),
            Id = erlang:integer_to_binary(Int),
            Pkt = io_lib:format(<<"<stream:stream xmlns='jabber:component:accept' xmlns:stream='htt"
            "p://etherx.jabber.org/streams' from='~s' id='~s'>">>
                               ,[To, Id]),
            Pkt2 = erlang:iolist_to_binary(Pkt),
            RetOpts2 = [{timeout, ?HANDSHAKE_TIMEOUT}, {packet, Pkt2} | RetOpts],
            parse_stanza(State#?STATE{state = handshake, jid = To, id = Id}, Rest, RetOpts2);

        {error, not_found} ->
            Reason = {openning_stream, [{reason, to_attr_not_found}, {attrs, Attrs}]},
            ErrPkt = stream_error(<<"conflict">>, <<"'to' attribute could not found">>),
            State2 = State#?STATE{data = Data},
            RetOpts2 = [{state, State2}, {packet, ErrPkt} | lists:reverse(RetOpts)],
            {stop, Reason, RetOpts2};

        {error, bad_value} ->
            Reason = {openning_stream, [{reason, bad_to_attr}, {attrs, Attrs}]},
            ErrPkt = stream_error(<<"conflict">>, <<"Bad value for 'to' attribute">>),
            State2 = State#?STATE{data = Data},
            RetOpts2 = [{state, State2}, {packet, ErrPkt} | lists:reverse(RetOpts)],
            {stop, Reason, RetOpts2}
    end;

parse_stanza(#?STATE{state = handshake
                    ,callback = Mod
                    ,jid = Jid
                    ,data = Data
                    ,id = Id}=State
            ,[#xmlel{name = <<"handshake">>, children = [#xmlcdata{content = HS}]} | Rest]
            ,RetOpts) ->
    case catch Mod:authenticate(Jid, Id, HS, Data) of
        ok ->
            State2 = State#?STATE{state = authenticated},
            RetOpts2 = [{timeout, infinity}, {packet, <<"<handshake/>">>} | RetOpts],
            parse_stanza(State2, Rest, RetOpts2);

        {ok, RetOpts2} when erlang:is_list(RetOpts2) ->
            {Data2, RetOpts3} = concat(Data
                                      ,[{timeout, infinity}, {packet, <<"<handshake/>">>} | RetOpts]
                                      ,RetOpts2),
            State2 = State#?STATE{state = authenticated, data = Data2},
            parse_stanza(State2, Rest, RetOpts3);

        close ->
            State2 = State#?STATE{data = Data},
            RetOpts2 = [{state, State2} | lists:reverse(RetOpts)],
            {close, RetOpts2};

        {close, RetOpts2} when erlang:is_list(RetOpts2) ->
            {Data2, RetOpts3} = concat(Data, RetOpts, RetOpts2),
            State2 = State#?STATE{data= Data2},
            {close, [{state, State2}| lists:reverse(RetOpts3)]};

        {stop, Reason} ->
            State2 = State#?STATE{data= Data},
            {stop, Reason, [{state, State2} | lists:reverse(RetOpts)]};

        {stop, Reason, RetOpts2} when erlang:is_list(RetOpts2) ->
            {Data2, RetOpts3} = concat(Data, RetOpts, RetOpts2),
            State2 = State#?STATE{data= Data2},
            {stop, Reason, [{state, State2} | lists:reverse(RetOpts3)]};

        {'EXIT', Reason} ->
            Reason = {crash, [{reason, Reason}
                             ,{module, Mod}
                             ,{function, autheticate}
                             ,{jid_binary, Jid}
                             ,{id, Id}
                             ,{handshake_data, HS}
                             ,{state, Data}]},
            ErrPkt = stream_error(<<"internal-server-error">>),
            State2 = State#?STATE{data = Data},
            RetOpts2 = [{state, State2}, {packet, ErrPkt} | lists:reverse(RetOpts)],
            {stop, Reason, RetOpts2};

        Other ->
            Reason = {return, [{value, Other}
                              ,{module, Mod}
                              ,{function, autheticate}
                              ,{jid_binary, Jid}
                              ,{id, Id}
                              ,{handshake_data, HS}
                              ,{state, Data}]},
            Pkt = stream_error(<<"internal-server-error">>),
            State2 = State#?STATE{data = Data},
            RetOpts2 = [{state, State2}, {packet, Pkt} | lists:reverse(RetOpts)],
            {stop, Reason, RetOpts2}
    end;

parse_stanza(#?STATE{state = undefined, data = Data}=State, [XMLEl|_Rest], RetOpts) ->
    Reason = {openning_stream, [{reason, xml_not_well_formed}, {xml, XMLEl}]},
    ErrPkt = stream_error(<<"conflict">>, <<"XML not well formed">>),
    State2 = State#?STATE{data = Data},
    RetOpts2 = [{state, State2}, {packet, ErrPkt} | lists:reverse(RetOpts)],
    {stop, Reason, RetOpts2};

parse_stanza(#?STATE{state = handshake, data = Data}=State
            ,[#xmlel{name = <<"handshake">>}=XMLEl|_Rest]
            ,RetOpts) ->
    Reason = {handshaking, [{reason, xml_not_well_formed}, {xml, XMLEl}]},
    Pkt = stream_error(<<"conflict">>, <<"XML not well formed">>),
    State2 = State#?STATE{data = Data},
    RetOpts2 = [{state, State2}, {packet, Pkt} | lists:reverse(RetOpts)],
    {stop, Reason, RetOpts2};

parse_stanza(#?STATE{state = authenticated, data = Data}=State, [#xmlstreamend{}|_Rest], RetOpts) ->
    State2 = State#?STATE{data = Data},
    RetOpts2 = [{state, State2} | lists:reverse(RetOpts)],
    {stop, stream_closed, RetOpts2};
parse_stanza(#?STATE{data = Data}=State, [XMLEl|_XMLs], RetOpts) ->
    Reason = {xml, [{reason, xml_not_well_formed}, {xml, XMLEl}]},
    State2 = State#?STATE{data = Data},
    ErrPkt = stream_error(<<"conflict">>, <<"XML not well formed">>),
    RetOpts2 = [{state, State2}, {packet, ErrPkt} | lists:reverse(RetOpts)],
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