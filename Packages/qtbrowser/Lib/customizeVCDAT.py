from PyQt4 import QtGui, QtCore
import os,cdms2
import vcdatCommons

## default values
ICONPATH = os.path.join(cdms2.__path__[0], '..', '..', '..', '..', 'share','icons')
appIcon = os.path.join(ICONPATH, "UV-CDAT_logo_sites.gif")
iconsize=35
defaultTextColor = vcdatCommons.blackColor
templatesColor = vcdatCommons.defaultTemplatesColor
gmsColor = vcdatCommons.defaultGmsColor
errorColor = vcdatCommons.redColor
commentsColor = vcdatCommons.redColor
recordCommands = True
defaultTemplateName = "default"
defaultGraphicsMethodName = "default"
colorSelectedStyle = "border:2px solid black"
colorNotSelectedStyle = "border:2px solid white"

## General Styles
appStyles = {}

## Calculator Styles
#scientificButtonsStyles = {"background-color":"#3F3B3C",
scientificButtonsStyles = {"background-color":"qradialgradient(cx:0.3, cy:-0.4, radius:1.35, fx:.3, fy:-0.4, stop:0 white, stop:1 black)",
                           "color":"white",
                           "font":"bold ",
                           "font-size":"16px",
                           }

validateButtonsStyles = {"background-color":"qradialgradient(cx:0.3, cy:-0.4, radius:1.35, fx:.3, fy:-0.4, stop:0 white, stop:1 #7C0404)",
                         "color":"white",
                         "font":"bold",
                         "font-size":"18px",
                         }

numberButtonsStyles = {"background-color":"qradialgradient(cx:0.3, cy:-0.4, radius:1.35, fx:.3, fy:-0.4, stop:0 white, stop:1 #6491DA)",
                       "color":"white",
                       "font":"bold ",
                       "font-size":"18px",
                       }

operatorButtonsStyles = {"background-color":"qradialgradient(cx:0.3, cy:-0.4, radius:1.35, fx:.3, fy:-0.4, stop:0 white, stop:1 #B79626)",
                            "color":"black",
                         "font":"bold ",
                         "font-size":"18px",
                         }

constantsButtonStyles = {"background-color":"qradialgradient(cx:0.3, cy:-0.4, radius:1.35, fx:.3, fy:-0.4, stop:0 white, stop:1 #578038)",
                         "color":"black",
                         "font":"bold ",
                         "font-size":"18px",
                             }

## scientificButtonsStyles = {}
## validateButtonsStyles = {}
## numberButtonsStyles = { }
## operatorButtonsStyles = { }
## constantsButtonStyles = { }

plotTypes = vcdatCommons.plotTypes

#this is a dictionary {plotname: plot object}
extraPlotTypes = {}
