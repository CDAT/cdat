from PyQt4 import QtGui, QtCore
import os,cdms2

## default values
ICONPATH = os.path.join(cdms2.__path__[0], '..','..','..','..','share/icons')
iconsize=25
blackColor = QtGui.QColor(0,0,0)
defaultTextColor = blackColor
redColor = QtGui.QColor(255,0,0)
errorColor = redColor
commentsColor = redColor
