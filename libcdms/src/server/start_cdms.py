#!/usr/bin/env python
# -*- Mode: Python; tab-width: 4 -*-

#
# Sample/Template Medusa Startup Script.
#
# This file acts as a configuration file and startup script for Medusa.
#
# You should make a copy of this file, then add, change or comment out
# appropriately.  Then you can start up the server by simply typing
# 
# $ python start_medusa.py
#

import os
import sys

import http_server
import ftp_server
import chat_server
import monitor
import filesys
import default_handler
import cdms_handler
import status_handler
import resolver
import logger
import asyncore
import auth_handler
import cdms_filesystem

if len(sys.argv) > 1:
	# process a few convenient arguments
	[CDMS_SERVER, CDMS_ROOT, WEB_ROOT] = sys.argv[1:]
else:
	# This is the IP address of the network interface you want
	# your servers to be visible from.  This can be changed to ''
	# to listen on all interfaces.
	# IP_ADDRESS			= '205.160.176.5'
	IP_ADDRESS			= ''

	# LDAP server and root node
	CDMS_SERVER = 'ldap://idoru.llnl.gov/'
	CDMS_ROOT	  = 'ou=PCMDI,o=LLNL,c=US'

	# Web root directory
	WEB_ROOT = '/home/httpd/html/cdms'

HTTP_PORT		= 8080 # The standard port is 80
# FTP_PORT		= 8021 # The standard port is 21
# CHAT_PORT		= 8888
MONITOR_PORT	= 9999

# ===========================================================================
# Caching DNS Resolver
# ===========================================================================
# The resolver is used to resolve incoming IP address (for logging),
# and also to resolve hostnames for HTTP Proxy requests.  I recommend
# using a nameserver running on the local machine, but you can also
# use a remote nameserver.

# rs = resolver.caching_resolver ('127.0.0.1')
rs = resolver.caching_resolver ('128.115.18.251')

# ===========================================================================
# Logging.
# ===========================================================================

# There are several types of logging objects. Multiple loggers may be combined,
# See 'logger.py' for more details.

# This will log to stdout:
lg = logger.file_logger (sys.stdout)

# This will log to syslog:
#lg = logger.syslog_logger ('/dev/log')

# This will wrap the logger so that it will
#  1) keep track of the last 500 entries
#  2) display an entry in the status report with a hyperlink
#     to view these log entries.
#
#  If you decide to comment this out, be sure to remove the
#  logger object from the list of status objects below.
#

lg = status_handler.logger_for_status (lg)

# ===========================================================================
# Filesystem Object.
# ===========================================================================
# An abstraction for the file system.  Filesystem objects can be
# combined and implemented in interesting ways.  The default type
# simply remaps a directory to root.

dfs = filesys.os_filesystem (WEB_ROOT)
fs =cdms_filesystem.cdms_filesystem(CDMS_SERVER, CDMS_ROOT)

# ===========================================================================
# Default HTTP handler
# ===========================================================================

# The 'default' handler for the HTTP server is one that delivers
# files normally - this is the expected behavior of a web server.
# Note that you needn't use it:  Your web server might not want to
# deliver files!

# This default handler uses the filesystem object we just constructed.

dh = default_handler.default_handler (dfs)
cdh = cdms_handler.cdms_handler(fs)

# Wrap an authentication handler around the default file handler

ah = auth_handler.auth_handler(dh,'access')
acdh = auth_handler.auth_handler(cdh,'access')

# ===========================================================================
# HTTP Server
# ===========================================================================
hs = http_server.http_server (IP_ADDRESS, HTTP_PORT, rs, lg)

# Here we install the authentication handler created above.
hs.install_handler (ah)
hs.install_handler(acdh)

# ===========================================================================
# Unix user `public_html' directory support
# ===========================================================================
if os.name == 'posix':
	import unix_user_handler
	uh = unix_user_handler.unix_user_handler ('public_html')
	hs.install_handler (uh)

# ===========================================================================
# FTP Server
# ===========================================================================

# Here we create an 'anonymous' ftp server.
# Note: the ftp server is read-only by default. [in this mode, all
# 'write-capable' commands are unavailable]

## ftp = ftp_server.ftp_server (
## 	ftp_server.anon_authorizer (
## 		PUBLISHING_ROOT
## 		),
## 	port=FTP_PORT,
## 	resolver=rs,
## 	logger_object=lg
## 	)

# ===========================================================================
# Monitor Server:
# ===========================================================================

# This creates a secure monitor server, binding to the loopback
# address on port 9999, with password 'fnord'.  The monitor server
# can be used to examine and control the server while it is running.
# If you wish to access the server from another machine, you will
# need to use '' or some other IP instead of '127.0.0.1'.
ms = monitor.secure_monitor_server ('fnord', '127.0.0.1', MONITOR_PORT)

# ===========================================================================
# Status Handler
# ===========================================================================

# These are objects that can report their status via the HTTP server.
# You may comment out any of these, or add more of your own.  The only
# requirement for a 'status-reporting' object is that it have a method
# 'status' that will return a producer, which will generate an HTML
# description of the status of the object.

status_objects = [
	hs,
#	ftp,
	ms,
	rs,
	lg
	]

# Create a status handler.  By default it binds to the URI '/status'...
sh = status_handler.status_extension(status_objects)
# ... and install it on the web server.
hs.install_handler (sh)

# become 'nobody'
if os.name == 'posix':
	import os
	if hasattr (os, 'seteuid'):
		# look in ~medusa/patches for {set,get}euid.
		import pwd
		[uid, gid] = pwd.getpwnam ('nobody')[2:4]
		os.seteuid (uid)
		os.setegid (gid)

# Finally, start up the server loop!  This loop will not exit until
# all clients and servers are closed.  You may cleanly shut the system
# down by sending SIGINT (a.k.a. KeyboardInterrupt).
asyncore.loop()
