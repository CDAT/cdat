#!/usr/bin/env python
#
# The UV-CDAT GUI system commands and miscellaneous functions -  systemCommands module
#
###############################################################################
#                                                                             #
# Module:       System Commands module                                        #
#                                                                             #
# Copyright:    "See file Legal.htm for copyright information."               #
#                                                                             #
# Authors:      PCMDI Software Team                                           #
#               Lawrence Livermore National Laboratory:                       #
#               website: http://uv-cdat.org/                                  #
#                                                                             #
# Description:  UV-CDAT GUI system commands and miscellaneous functons.       #
#                                                                             #
# Version:      6.0                                                           #
#                                                                             #
###############################################################################

import os, sys, string, types
from PyQt4 import QtGui

#------------------------------------------------------------------------
# Redirect the destination of sys.stderr to the UV-CDAT output window
#------------------------------------------------------------------------
class OutLog:
    def __init__(self, edit, out=None, color=None):
        """(edit, out=None, color=None) -> can write stdout, stderr to a
        QTextEdit.
        edit = QTextEdit
        out = alternate stream ( can be the original sys.stdout )
        color = alternate color (i.e. color stderr a different color)
        """
        self.edit = edit
        self.out = None
        self.color = color

    def write(self, m):
        if self.color:
            tc = self.edit.textColor()
            self.edit.setTextColor(self.color)

        self.edit.moveCursor(QtGui.QTextCursor.End)
        self.edit.insertPlainText( m )

        if self.color:
            self.edit.setTextColor(tc)

        if self.out:
            self.out.write(m)


class standard_err:
   def __init__( self, master ):
        self.parent = master

   def write(self, txt):
        print "********* se ********", txt
        #self.parent.appendPlainText( str(txt) )

#        self.parent.log_window.appendtext( s )
#        self.parent.log_window.yview("moveto", 1)
        # save to output file
#        f = open(self.parent.log_dirname, "a+")
#        f.write(s)
#        f.close()

#   def flush(self):
#        err = open('/dev/null', 'a+', 0)
#        os.dup2(err.fileno(), 2)

#------------------------------------------------------------------------
# Redirect the destination of sys.stdout to the UV-CDAT output window
#------------------------------------------------------------------------
class standard_out:
   def __init__( self, master ):
        self.parent = master

   def write(self, txt):
        print "********* se ********", txt
        #self.parent.appendPlainText( str(txt) )

#        self.parent.log_window.appendtext( s )
#        self.parent.log_window.yview("moveto", 1)
        # save to output file
#        f = open(self.parent.log_dirname, "a+")
#        f.write(s)
#        f.close()
        #self.parent.log_window.insert( 'end', s )
        #self.parent.panelA.pan3.insert( 'end', s )

