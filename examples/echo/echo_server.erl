-module(echo_server).
-behaviour(xrouter).


%% I need:
%%  #xmpp_utils_xml{}
-include_lib("xrouter/include/xrouter.hrl").


-export([start/2
        ,get_connections/0
        ,stop/0
        ,init/1
        ,authenticate/4
        ,handle_xmpp_xml/2
        ,handle_call/3
        ,handle_info/2
        ,terminate/2]).



start(Port, Opts) ->
    ServerName = ?MODULE,
    ServerCallbackMod = ?MODULE,
    ServerInitArg = undefined,
    xrouter:start_link(ServerName, ServerCallbackMod, ServerInitArg, Port, Opts).




get_connections() ->
    %% Since xrouter uses sockerl library, you can use sockerl's API
    %% See https://github.com/Pouriya-Jahanbakhsh/sockerl/blob/17.8.9/src/sockerl.erl#L56
    etcp:fetch_server_connections(?MODULE).





stop() ->
    xrouter:stop(?MODULE).



%% -------------------------------------------------------------------------------------------------
%% xrouter's callbacks:



init(_) ->
    %% This function after successful initialization should return
    %% {ok, YourState}
    {ok, #{}}. %% I used an empty map as my state



authenticate(Jid, _Id, _HandshakeData, State) ->
    %% when a connection tries to authenticated and sends its handshake
    %% xrouter calls this function fom callback module.
    %% If you want to check that Handshake data is valid, you can check
    %% Its validation using xrouter:validate_shared_secret/3
    %% For doing this you need Jid's secret key in your side,
    %% An efficient way is reading jids and their shared secrets from
    %% config file and adding them to an ETS table in above function
    %% init/1, and here you can lookup This Jid's shared secret and run
    %% xrouter:validate_shared_secret(Id, HandshakeData, SecretKey)

    %% If you want to accept all authentication (Just do it in trusted
    %% network environment) you dont need to use this function or keep
    %% some jids and shared secrets

    %% If you want to let connection authenticate successfully return:
    %% 'ok'          |
    %% {'ok', RetOpts}
    %% when
    %%     RetOpts :: [] | [RetOpt].
    %%     RetOpt :: {'state', NewState::any()} % Optional and if not
    % defined, state will be
    % previous state value

    %%             | {'packet', Pkt::binary()}. % If you want to send
    % some packets to client
    % define them here

    %% Otherwise return following terms for closing connection:
    %% 'close'                         | % Closes connection normally
    %% {'stop', Reason::any()}         | % Closes connection for Reason
    %% {'close', RetOpts}              | % evaluate RetOpts and closes with
    % reason normal
    %% {'stop', Reason::term(), RetOpts} % evaluate RetOpts and closes with
    % reason Reason
    %% RetOpts can has aboove values.

    %% Here i want to accept all connection and i don't want to send
    %% them anything until they send data
    %% I want to keep this jid in state and have a count for counting
    %% incomming packets.
    {ok, [{state, State#{jid => Jid, count => 0}}]}.


handle_xmpp_xml(#xmpp_utils_xml{kind = Kind
                               ,from = From
                               ,to = To
                               ,type = Type}=XMPP_XML
               ,#{jid := Jid, count := Count}=State) ->
    %% First, i want to print some data about this received XML
    io:format("~s : received ~s packet from ~s to ~s with type ~s~n"
             ,[Jid
              ,Kind
            %% From and To are #xmpp_utils_jid{} record
            %% I will convert them to binary using xmpp_utils:make_jid/1
              ,xmpp_utils:make_jid(From)
              ,xmpp_utils:make_jid(To)
              ,Type]),

    %% I want to make reply packet and i want to send its packet back
    %% I need to change From and To
    Pkt = xmpp_utils:make_pkt(xmpp_utils:change_from_to(XMPP_XML)),
    %% Return value is like return value of authenticate/4
    {ok, [{packet, Pkt}, {state, State#{count => Count+1}}]}.


handle_call(get_count, From, #{count := Count}) ->
    %% Return value is like return value of authenticate/4 except that
    %% here you have new RetOpt {'reply', From::term(), Reply::any()}

    %% one process makes a call (it might use gen_server:call/2-3) for
    %% getting count of received packets and i want to give them to that
    {ok, [{reply, From, Count}]}.


handle_info(print_count,  #{jid := Jid, count := Count}) ->
    %% Return value is like return value of authenticate/4

    io:format("~s received ~p packets~n", [Jid, Count]),
    ok.


terminate(_Reason, _State) ->
    ok.