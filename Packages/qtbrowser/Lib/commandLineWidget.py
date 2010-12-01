#!/usr/bin/env python


#
# The Ultra-scale Visual Climate Data Analysis Tools (UV-CDAT) - commandLind Widget
#
###############################################################################
#                                                                             #
# Module:       CommandLind Widget                                            #
#                                                                             #
# Copyright:    "See file Legal.htm for copyright information."               #
#                                                                             #
# Authors:      PCMDI Software Team                                           #
#               Lawrence Livermore National Laboratory:                       #
#               website: http://uv-cdat.org/                                  #
#                                                                             #
# Description:  Main widget containing the "Command Line Window", which       #
#               executes the Python Integrated Development Environment (IDLE) #
#               tool developed by Guido van Rossum, the developer of Python.  #
#               The Python Shell/Window gives the user access into Python's   #
#               interactive mode. This tool has been slightly modified to     #
#               allow VCDAT to register keystrokes for reproducibility-a      #
#               feature necessary for underlying workflow and provenance      #
#               procedures.                                                   #
#                                                                             #
#               This class is called for the gui_core.                        #
#                                                                             #
# Version:      6.0                                                           #
#                                                                             #
###############################################################################
#
from PyQt4 import QtGui, QtCore
import vcs, os, sys, string
import __main__
import systemCommands

class QCommandLine(QtGui.QWidget):
    ''' Main widget containing the "Command Line Window", which executes the Python Integrated Development Environment (IDLE) tool developed by Guido van Rossum, the developer of Python. The Python Shell/Window gives the user access into Python's interactive mode. This tool has been slightly modified to allow VCDAT to register keystrokes for reproducibility-a feature necessary for underlying workflow and provenance procedures. '''

    def __init__(self, parent=None):
        QtGui.QWidget.__init__(self, parent)
        self.myParent = parent

        # create objects
        label = QtGui.QLabel(self.tr("Enter CDAT command and press Return"))
        self.le = QtGui.QLineEdit()
        self.te = QtGui.QTextEdit()
        self.te.acceptRichText()

        #------------------------------------------------------------------------------
        # Redirect stderr and stdout to the ouput window.
        #------------------------------------------------------------------------------
        sys.stdout = systemCommands.OutLog( self.te, sys.stdout )
        sys.stderr = systemCommands.OutLog( self.te, sys.stderr, QtGui.QColor(240,0,0) )


        # layout
        layout = QtGui.QVBoxLayout(self)
        layout.addWidget(label)
        layout.addWidget(self.le)
        layout.addWidget(self.te)
        self.setLayout(layout)

        # create connection
        self.connect(self.le, QtCore.SIGNAL("returnPressed(void)"),
                     self.run_command)

    def run_command(self):
        command = str(self.le.text())
        command = string.strip(command)  # Strip leading and/or trailing whitespaces from the command
        self.te.setTextColor( QtGui.QColor(0,0,0))
        self.te.insertPlainText( ">>> " + command + "\n")
        exec( command, __main__.__dict__ )
        self.le.clear()
