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
# Description:  This is the main widget containing the "Command Line Window", #
#               which executes Python commands. The Python Shell/Window       #
#               gives the user access into Python's interactive mode. This    #
#               tool has been slightly modified to allow VCDAT to register    #
#               commands for reproducibility - a feature necessary for        #
#               underlying workflow and provenance procedures.                #
#                                                                             #
#               This class is called from the VCDAT Tab Window.               #
#                                                                             #
# Version:      6.0                                                           #
#                                                                             #
###############################################################################
#
from PyQt4 import QtGui, QtCore
import vcs, os, sys, string
import __main__
import systemCommands
import qtbrowser
import customizeVCDAT

class QCommandLineType(QtGui.QLineEdit):
    """ Command line events to trap the up, down, left, right arrow button events for the Qt Line Edit. """
    def keyPressEvent(self,event):
        if event.key() in (QtCore.Qt.Key_Up, QtCore.Qt.Key_Left):
           systemCommands.command_num += 1
           if systemCommands.command_num > len(systemCommands.commandHistory):
              systemCommands.command_num = len(systemCommands.commandHistory)
           command = systemCommands.commandHistory[len(systemCommands.commandHistory) - systemCommands.command_num]
           self.setText( command )
        elif event.key() in (QtCore.Qt.Key_Down, QtCore.Qt.Key_Right):
           systemCommands.command_num -= 1
           if systemCommands.command_num <= 0:
              systemCommands.command_num = 0
              command = ""
           else:
              command = systemCommands.commandHistory[len(systemCommands.commandHistory) - systemCommands.command_num]
           self.setText( command )
        elif (event.key() == QtCore.Qt.Key_U and event.modifiers() == QtCore.Qt.MetaModifier):
           self.clear()
        QtGui.QLineEdit.keyPressEvent(self,event)

class QCommandLine(QtGui.QWidget):
    """ This is the main widget containing the "Command Line Tab Window", which executes CDAT and Python commands. The Python Shell/Window gives the user access into Python's interactive mode. This tool has been slightly modified to allow VCDAT to register keystrokes for reproducibility - a feature necessary for underlying workflow and provenance procedures. """

    def __init__(self, parent=None):
        QtGui.QWidget.__init__(self, parent)

        #------------------------------------------------------------------------------
        # create objects instance for the Qt Command Line and Text Window
        #------------------------------------------------------------------------------
        self.root=parent.root
        # create objects
        label = QtGui.QLabel(self.tr("Enter CDAT command and press Return"))
        self.le = QCommandLineType()
        self.te = QtGui.QTextEdit()
        self.te.setReadOnly(True)

        #------------------------------------------------------------------------------
        # redirect stderr and stdout to the ouput window
        # if stdout, then the text will be colored black, else if an error occurs
        # (i.e., stderr), then show the text in red
        #------------------------------------------------------------------------------
        if qtbrowser.debug:
            sys.stdout = systemCommands.OutLog( self.te, None, sys.stdout )
            sys.stderr = systemCommands.OutLog( self.te, customizeVCDAT.errorColor, sys.stderr )
        else:
            sys.stdout = systemCommands.OutLog( self.te)
            sys.stderr = systemCommands.OutLog( self.te, customizeVCDAT.errorColor)

        #------------------------------------------------------------------------------
        # layout
        #------------------------------------------------------------------------------
        layout = QtGui.QVBoxLayout(self)
        layout.addWidget(label)
        layout.addWidget(self.le)
        layout.addWidget(self.te)
        self.setLayout(layout)

        #------------------------------------------------------------------------------
	# connect signal - if the return key is pressed, then call run_command
        #------------------------------------------------------------------------------
        self.connect(self.le, QtCore.SIGNAL("returnPressed(void)"),
                     self.run_command)
        
    def run_command(self):
        """ Event that processes the CDAT/Python command and displays the stdout or stderr in the text editor window. """
        #------------------------------------------------------------------------------
        # isolate the command and display it in the text editor window
        #------------------------------------------------------------------------------
        command = str(self.le.text())    # read the command
        command = string.strip(command)  # strip leading and/or trailing whitespaces from the command
        self.te.setTextColor( QtGui.QColor(0,0,0)) # set the text editor output window text to black
        commandLine =  ">>> " + command + "\n"
        self.te.insertPlainText( commandLine )     # display the command in the text window

        #------------------------------------------------------------------------------
        # append the command to the list and rest the list number to 0
        #------------------------------------------------------------------------------
        if command != "": systemCommands.commandHistory.append( command )
        systemCommands.command_num = 0

        #------------------------------------------------------------------------------
        # execute the command and clear the line entry if no error occurs
        #------------------------------------------------------------------------------
        exec( command, __main__.__dict__ )
        self.le.clear()
        self.root.stick_main_dict_into_defvar()
        #------------------------------------------------------------------------------
        # record the command for preproducibility
        #------------------------------------------------------------------------------
        self.root.record("## Command sent from prompt by user")
        self.root.record(command)
