#!/usr/bin/env python


#
# The Ultra-scale Visual Climate Data Analysis Tools (UV-CDAT) - Calculator Widget
#
###############################################################################
#                                                                             #
# Module:       Calculator Widget                                             #
#                                                                             #
# Copyright:    "See file Legal.htm for copyright information."               #
#                                                                             #
# Authors:      PCMDI Software Team                                           #
#               Lawrence Livermore National Laboratory:                       #
#               website: http://uv-cdat.org/                                  #
#                                                                             #
# Description:  This is the main widget containing the "Calculator Window",   #
#               which is used to compute basic and advanced mathematical and  #
#               statistical output. The widget contains climatological        #
#               empirical orthogonal, statistical, and microwave sounding     #
#               units functions (to name a few) used in computng daily and    #
#               monthly variables.                                            #
#                                                                             #
#               As with other CDAT widgets, this tool has been slightly       #
#               modified to allow VCDAT to register commands for              #
#               reproducibility - a feature necessary for underlying workflow #
#               and provenance procedures.                                    #
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

class Lcd(QtGui.QLCDNumber):
    """Main LCD Display"""
    def __init__(self, sizeFactor=1, numDigits=8, parent=None):
        QtGui.QLCDNumber.__init__(self, numDigits, parent)
        self.sizeFactor = sizeFactor
        self.setSegmentStyle(QtGui.QLCDNumber.Filled)
        self.setMinimumSize(10, 23)
        self.setFrameStyle(QtGui.QFrame.NoFrame)

    def setDisplay(self, text, numDigits):
        """Update display value"""
        text = text.replace('e', ' E', 1)  # add space before exp
        if len(text) > numDigits:  # mark if digits hidden
            text = 'c%s' % (text[1-numDigits:])
        self.setNumDigits(numDigits)
        self.display(text)

    def sizeHint(self):
        """Set prefered size"""
        # default in Qt is 23 height & about 10 * numDigits
        size = QtGui.QLCDNumber.sizeHint(self) 
        return QtCore.QSize(int(size.width() * self.sizeFactor),
                            int(size.height() * self.sizeFactor))

class LcdBox(QtGui.QFrame):
    """Frame for LCD display"""
    def __init__(self, parent=None):
        QtGui.QFrame.__init__(self, parent)
        self.setFrameStyle(QtGui.QFrame.Panel | QtGui.QFrame.Sunken)
        self.setLineWidth(3)

    def mouseReleaseEvent(self, event):
        """Mouse release event for popup menus"""
        if event.button() == QtCore.Qt.RightButton:
            popup = self.parentWidget().popupMenu
            popup.exec_(self.mapToGlobal(event.pos()))
            popup.clearFocus()
        QtGui.QFrame.mouseReleaseEvent(self, event)

class CalcButton(QtGui.QPushButton):
    """Calculator button class - size change & emits clicked text signal"""
    def __init__(self, text, parent=None):
        QtGui.QPushButton.__init__(self, text, parent)
        self.setMinimumSize(38, 16)
        self.setSizePolicy(QtGui.QSizePolicy(QtGui.QSizePolicy.Preferred,
                                             QtGui.QSizePolicy.Preferred))
        self.setFocusPolicy(QtCore.Qt.NoFocus)
        self.connect(self, QtCore.SIGNAL('clicked()'), self.clickEvent)

    def clickEvent(self):
        """Emits signal with button text"""
        self.emit(QtCore.SIGNAL('activated(QString &)'), self.text())

    def sizeHint(self):
        """Set prefered size"""
        size = QtGui.QPushButton.sizeHint(self)
        size.setWidth(size.width() / 2)
        return size

    def tmpDown(self, mSec):
        """Button shows pushed in for mSec milliseconds"""
        timer = QtCore.QTimer(self)
        timer.setSingleShot(True)
        self.connect(timer, QtCore.SIGNAL('timeout()'), self.timerUp)
        timer.start(mSec)
        self.setDown(True)

    def timerUp(self): 
        """Button up at end of timer for tmpDown"""
        self.setDown(False)


class QCalculator(QtGui.QWidget):
    """This is the main widget containing the "Calculator Window", which is used to compute basic and advanced mathematical and statistical output. The widget contains climatological empirical orthogonal, statistical, and microwave sounding units functions (to name a few) used in computng daily and monthly variables. As with other CDAT widgets, this tool has been slightly modified to allow VCDAT to register commands for reproducibility - a feature necessary for underlying workflow and provenance procedures. """

    def __init__(self, parent=None):
        QtGui.QWidget.__init__(self, parent)

        #------------------------------------------------------------------------------
        # create objects instance for the Qt Command Line and Text Window
        #------------------------------------------------------------------------------
        self.le = QtGui.QTextEdit()
        self.te = QtGui.QTextEdit()
        self.te.acceptRichText()


        #------------------------------------------------------------------------------
        # layout
        #------------------------------------------------------------------------------
#        layout = QtGui.QVBoxLayout(self)
#        layout.addWidget(self.le)
#        layout.addWidget(self.te)
#        self.setLayout(layout)

        topLay = QtGui.QVBoxLayout(self)
        self.setLayout(topLay)
        topLay.setSpacing(4)
        topLay.setMargin(6)

        lcdBox = LcdBox()
        topLay.addWidget(lcdBox)
        lcdLay = QtGui.QGridLayout(lcdBox)
        lcdLay.setColumnStretch(1, 1)
#        lcdLay.setRowStretch(1, 1)
#        self.extraLabels = [QtGui.QLabel(' T:',), QtGui.QLabel(' Z:',),
#                            QtGui.QLabel(' Y:',)]

        self.cmdLay = QtGui.QGridLayout()
        topLay.addLayout(self.cmdLay)
        self.cmdDict = {}
        self.addCmdButton('x^2', 0, 0)
        self.addCmdButton('y^X', 0, 1)
        self.addCmdButton('1/x', 0, 2)
        self.addCmdButton('sqRT', 0, 3)
        self.addCmdButton('xRT', 0, 4)
        self.addCmdButton('SIN', 1, 0)
        self.addCmdButton('COS', 1, 1)
        self.addCmdButton('TAN', 1, 2)
        self.addCmdButton('LOG', 1, 3)
        self.addCmdButton('10^X', 1, 4)
        self.addCmdButton('ASIN', 2, 0)
        self.addCmdButton('ACOS', 2, 1)
        self.addCmdButton('ATAN', 2, 2)
        self.addCmdButton('LN', 2, 3)
        self.addCmdButton('e^X', 2, 4)

        # Create separator line between each axis widget
#        vline = QtGui.QFrame()
#        vline.setFrameStyle(QtGui.QFrame.HLine | QtGui.QFrame.Sunken)
#        vline.setMidLineWidth(3)
#        palette = vline.palette()
#        role = vline.foregroundRole()
#        palette.setColor(role, QtGui.QColor(220,213,226))
#        #vline.setPalette(palette)
#        #vline.setAutoFillBackground(True)
#        topLay.addWidget(vline)
#        sepLay = QtGui.QGridLayout(vline)

        self.addCmdButton('x<y', 3, 0)
        self.addCmdButton('x>y', 3, 1)
        self.addCmdButton('x<=y', 3, 2)
        self.addCmdButton('x<>y', 3, 3)
        self.addCmdButton('x=y', 3, 4)
        self.addCmdButton('REGRID', 4, 0)
        self.addCmdButton('MASK', 4, 1)
        self.addCmdButton('GET_MASK', 4, 2)
        self.addCmdButton('GROWER', 4, 3)
        self.addCmdButton('PI', 4, 4)
        self.addCmdButton('STD', 5, 0)
        self.addCmdButton('DEG', 5, 1)
        self.addCmdButton('ABS', 5, 2)
        self.addCmdButton('(', 5, 3)
        self.addCmdButton(')', 5, 4)

        self.mainLay = QtGui.QGridLayout()
        topLay.addLayout(self.mainLay)
        self.mainDict = {}
        self.addMainButton(0, 'Clear', 0, 0)
        self.addMainButton(QtCore.Qt.Key_Equal, '=', 0, 1)
        self.addMainButton(QtCore.Qt.Key_Slash, '/', 0, 2)
        self.addMainButton(QtCore.Qt.Key_Asterisk, '*', 0, 3)
        self.addMainButton(QtCore.Qt.Key_7, '7', 1, 0)
        self.addMainButton(QtCore.Qt.Key_8, '8', 1, 1)
        self.addMainButton(QtCore.Qt.Key_9, '9', 1, 2)
        self.addMainButton(QtCore.Qt.Key_Minus, '-', 1, 3)
        self.addMainButton(QtCore.Qt.Key_4, '4', 2, 0)
        self.addMainButton(QtCore.Qt.Key_5, '5', 2, 1)
        self.addMainButton(QtCore.Qt.Key_6, '6', 2, 2)
        self.addMainButton(QtCore.Qt.Key_Plus, '+', 2, 3)
        self.addMainButton(QtCore.Qt.Key_1, '1', 3, 0)
        self.addMainButton(QtCore.Qt.Key_2, '2', 3, 1)
        self.addMainButton(QtCore.Qt.Key_3, '3', 3, 2)
        self.addMainButton(QtCore.Qt.Key_Enter, 'Enter', 3, 3, 1, 0)
        self.addMainButton(QtCore.Qt.Key_0, '0', 4, 0, 0, 1)
        self.addMainButton(QtCore.Qt.Key_Period, '.', 4, 2)

        '''

        '''

    def addCmdButton(self, text, row, col):
        """Adds a CalcButton for command functions"""
        button = CalcButton(text)
        self.cmdDict[text.upper()] = button
        self.cmdLay.addWidget(button, row, col)
##        self.connect(button, QtCore.SIGNAL('activated(QString &)'),
##                     self.issueCmd) 

    def addMainButton(self, key, text, row, col, extraRow=0, extraCol=0):
        """Adds a CalcButton for number and 4-function keys"""
        button = CalcButton(text)
        self.mainDict[key] = button
        self.mainLay.addWidget(button, row, col, 1+extraRow, 1+extraCol)
##        self.connect(button, QtCore.SIGNAL('activated(QString &)'),
##                     self.issueCmd)

