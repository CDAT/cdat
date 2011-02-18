from PyQt4 import QtGui, QtCore
import os,cdms2

## default values
ICONPATH = os.path.join(cdms2.__path__[0], '..','..','..','..','share/icons')
iconsize=25
blackColor = QtGui.QColor(0,0,0)
defaultTextColor = blackColor
redColor = QtGui.QColor(255,0,0)
greenColor = QtGui.QColor(0,255,0)
templatesColor = QtGui.QColor(108,198,105)
gmsColor = QtGui.QColor(252,104,63)
errorColor = redColor
commentsColor = redColor
recordCommands = True
defaultTemplateName = "default"
defaultGraphicsMethodName = "default"

plotTypes = ['Boxfill', 'Isofill', 'Isoline', 'Meshfill', 'Outfill',
             'Outline', 'Scatter', 'Taylordiagram', 'Vector', 'XvsY',
             'Xyvsy', 'Yxvsx','Ext']
