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
from PyQt4 import QtGui, QtCore

#------------------------------------------------------------------------
# Redirect the destination of sys.stderr to the UV-CDAT output window
#------------------------------------------------------------------------
class OutLog:
    def __init__(self, edit, color=None, original_stream=None):
        """(edit, out=None, color=None) -> can write stdout, stderr to a
        QTextEdit.
        edit = QTextEdit
        out = alternate stream ( can be the original sys.stdout )
        color = alternate color (i.e. color stderr a different color)
        """
        self.edit = edit
        self.color = color
        self.original_stream = original_stream
        

    def write(self, stdtext):
        """ Write the text for standard out or error to the text editor window."""
        tex = QtCore.QString(stdtext)
        if self.color is not None:
            self.edit.setTextColor(self.color) # if color, then it must be "stderr"
        else:
            self.edit.setTextColor( QtGui.QColor(0,0,0)) # if no color, then it must be "stdout"
        self.edit.insertPlainText( tex ) # show the text in the Qt Text Editor window

        if self.original_stream is not None:
            self.original_stream.write(stdtext)

        #---------------------------------------------------------------------------------
        # scroll to bottom of Qt Text Editor window (always show the newly entered text
        #---------------------------------------------------------------------------------
        cursor = self.edit.textCursor()
        cursor.movePosition(cursor.End )
        self.edit.setTextCursor(cursor)


#------------------------------------------------------------------------
# Command history list
#------------------------------------------------------------------------
commandHistory = []
command_num = 0
