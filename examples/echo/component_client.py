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
