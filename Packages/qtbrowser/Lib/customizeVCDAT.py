from PyQt4 import QtGui, QtCore
import os,cdms2
import vcdatCommons

## default values
ICONPATH = os.path.join(cdms2.__path__[0], '..','..','..','..','share/icons')
iconsize=25
defaultTextColor = vcdatCommons.blackColor
templatesColor = vcdatCommons.defaultTemplatesColor
gmsColor = vcdatCommons.defaultGmsColor
errorColor = vcdatCommons.redColor
commentsColor = vcdatCommons.redColor
recordCommands = True
defaultTemplateName = "default"
defaultGraphicsMethodName = "default"

plotTypes = vcdatCommons.plotTypes
