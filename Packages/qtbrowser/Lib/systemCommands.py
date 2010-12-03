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
        self.original_stream = original_stream
        self.color = color
        

    def write(self, m):
        tex = QtCore.QString(m)
        if self.color is not None:
            self.edit.setTextColor(self.color)
        else:
            self.edit.setTextColor( QtGui.QColor(0,0,0))
        self.edit.insertPlainText( tex )
        if self.original_stream is not None:
            self.original_stream.write(m)
