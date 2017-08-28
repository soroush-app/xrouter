# xrouter
Library for interacting with XMPP components. So you can connect it using any XMPP library which implements [XEP 0114](https://xmpp.org/extensions/xep-0086.html).




# Example
**xrouter** just needs a callback module for every server.  
In the following example we will make an **echo server**.  
An echo server is usually an application which is used to test if the connection between a client and a server is successful. It consists of a server which sends back whatever text the client sent.  

Server code:
```erlang
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
	xrouter:start_server(ServerName, ServerCallbackMod, Port, Opts).
	%% If You need stand-alone server, use xrouter:start_link_server/4




get_connections() ->
    %% Since xrouter uses sockerl library, you can use sockerl's API
    %% See https://github.com/Pouriya-Jahanbakhsh/sockerl/blob/17.8.9/src/sockerl.erl#L56
    sockerl:get_server_connections(?MODULE).





stop() ->
    xrouter:stop_server(?MODULE).



%% ---------------------------------------------------------------------
%% xrouter's callbacks:



init(_Opts) ->
	%% Opts is above Opts which we gave to xrouter:start_server/4
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
```

For client side i used python well-known XMPP library [SleekXMPP](https://github.com/fritzy/SleekXMPP), so for test you have to install python and sleekxmpp.  
I will send message to remote component server every 15 seconds to new destination username.  
Client code:
```python
#!/usr/bin/env python
# -*- coding: utf-8 -*-

import sys
import logging
import getpass
from optparse import OptionParser

import sleekxmpp
from sleekxmpp.componentxmpp import ComponentXMPP

from time import time, sleep

if sys.version_info < (3, 0):
    reload(sys)
    sys.setdefaultencoding('utf8')
else:
    raw_input = input


class EchoComponent(ComponentXMPP):

    def __init__(self, jid, secret, server, port):
        ComponentXMPP.__init__(self, jid, secret, server, port)
        self.add_event_handler("session_start", self.start)



    def start(self, event):
        count = 1
        while count < 100:
            self.send_message(mfrom = "foo@bar/baz",
		                      mto="{}@example.domain".format(count),
		                      mbody=str(count),mtype='chat')
            count += 1
            sleep(15)
        self.disconnect()


if __name__ == '__main__':
    # Setup the command line arguments.
    optp = OptionParser()

    # Output verbosity options.
    optp.add_option('-q', '--quiet', help='set logging to ERROR',
                    action='store_const', dest='loglevel',
                    const=logging.ERROR, default=logging.INFO)
    optp.add_option('-d', '--debug', help='set logging to DEBUG',
                    action='store_const', dest='loglevel',
                    const=logging.DEBUG, default=logging.INFO)
    optp.add_option('-v', '--verbose', help='set logging to COMM',
                    action='store_const', dest='loglevel',
                    const=5, default=logging.INFO)

    optp.add_option("-P", "--port", dest="port",
                    help="port to connect to", type='int')

    opts, args = optp.parse_args()



    if opts.port is None:
		opts.port = 8080

    # Setup logging.
    logging.basicConfig(level=opts.loglevel,
                        format='%(levelname)-8s %(message)s')

    xmpp = EchoComponent("foo@bar/baz", "s3cr3t", "127.0.0.1", opts.port)

    if xmpp.connect():
        xmpp.process(block=True)
        print("Done")
    else:
        print("Unable to connect.")
```

Let's test it:
```erlang
Erlang/OTP 19 [erts-8.3] [source-d5c06c6] [64-bit] [smp:8:8] [async-threads:0] [hipe] [kernel-poll:false]

Eshell V8.3  (abort with ^G)

1> application:start(xrouter).
ok

2> echo_server:start(8080, []).  
{ok,<0.175.0>}
```

Now run component client:
```sh
$ python component_client.py -v

DEBUG    Connecting to 127.0.0.1:8080
DEBUG    Waiting 1.90503973189 seconds before connecting.
DEBUG    Connecting to 127.0.0.1:8080
DEBUG    Event triggered: connected
DEBUG     ==== TRANSITION disconnected -> connected
DEBUG    Starting HANDLER THREAD
DEBUG    Loading event runner
DEBUG    SEND (IMMED): <stream:stream xmlns="jabber:component:accept" xmlns:stream="http://etherx.jabber.org/streams" to='foo@bar/baz'>
DEBUG    RECV: <stream:stream from="foo@bar/baz" id="230681">
DEBUG    SEND (IMMED): <handshake xmlns="jabber:component:accept">da3965db63cd9f6a4d852be4fb97e3e6956cd499</handshake>
DEBUG    RECV: <handshake />
DEBUG    Event triggered: session_bind
DEBUG    Event triggered: session_start

DEBUG    SEND: <message to="1@example.domain" type="chat" from="foo@bar/baz"><body>1</body></message>
DEBUG    RECV: <message to="foo@bar/baz" from="1@example.domain" type="chat"><body>1</body></message>
```

Return back to Erlang shell:
```erlang

3> foo@bar/baz : received message packet from foo@bar/baz to 1@example.domain with type chat

3> echo_server:get_connections().
[{#Ref<0.0.3.2076>,<0.182.0>}]

4> foo@bar/baz : received message packet from foo@bar/baz to 2@example.domain with type chat

4> [{_,Pid}] = echo_server:get_connections().
[{#Ref<0.0.3.2076>,<0.182.0>}]

5> Pid ! print_count.
foo@bar/baz received 2 packets

6> foo@bar/baz : received message packet from foo@bar/baz to 3@example.domain with type chat

6> gen_server:call(Pid, get_count).
3

7> echo_server:stop().
%% Some debug output about termination
```
