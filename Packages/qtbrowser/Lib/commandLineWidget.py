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
import vcdatCommons

class CalcButton(QtGui.QToolButton):
    """Calculator button class - size change & emits clicked text signal"""
    def __init__(self, text, icon = None, tip = None, styles={}, parent=None):
        QtGui.QToolButton.__init__(self, parent)
        if isinstance(styles,dict):
            st=""
            for k in styles.keys():
                val = styles[k]
                if isinstance(val,QtGui.QColor):
                    val = str(val.name())
                st+="%s:%s; " % (k,val)
            if len(st)>0: self.setStyleSheet(st)
        elif isinstance(styles,str):
            self.setStyleSheet(styles)
            
        self.setToolButtonStyle(QtCore.Qt.ToolButtonTextBesideIcon)
        if icon is not None:
            icon = os.path.join(customizeVCDAT.ICONPATH,icon)
            icon = QtGui.QIcon(icon)
            self.setIcon(icon)
            self.setIconSize(QtCore.QSize(customizeVCDAT.iconsize,customizeVCDAT.iconsize))
        self.setText(text)
        self.setMinimumSize(38, 35)
        ## self.setMaximumSize(60, 50)
        self.setSizePolicy(QtGui.QSizePolicy(QtGui.QSizePolicy.Preferred,
                                             QtGui.QSizePolicy.Preferred))
        self.setFocusPolicy(QtCore.Qt.NoFocus)
        if tip is not None:
            self.setToolTip(tip)
            
        self.connect(self, QtCore.SIGNAL('clicked()'), self.clickEvent)

    def clickEvent(self):
        """Emits signal with button text"""
        self.emit(QtCore.SIGNAL('clickedCalculator'), self)

class QCommandLineType(QtGui.QLineEdit):
    """ Command line events to trap the up, down, left, right arrow button events for the Qt Line Edit. """

    def keyPressEvent(self,event):
        if event.key() in (QtCore.Qt.Key_Up, ):
           if len(systemCommands.commandHistory) == 0:
               return
           systemCommands.command_num += 1
           if systemCommands.command_num > len(systemCommands.commandHistory):
              systemCommands.command_num = len(systemCommands.commandHistory)
           command = systemCommands.commandHistory[len(systemCommands.commandHistory) - systemCommands.command_num]
           self.setText( command )
           self.setFocus()
        elif event.key() in (QtCore.Qt.Key_Down, ):
           systemCommands.command_num -= 1
           if systemCommands.command_num <= 0:
              systemCommands.command_num = 0
              command = ""
           else:
              command = systemCommands.commandHistory[len(systemCommands.commandHistory) - systemCommands.command_num]
           self.setText( command )
           self.setFocus()
        elif (event.key() == QtCore.Qt.Key_U and event.modifiers() == QtCore.Qt.MetaModifier):
           self.clear()
           self.setFocus()
        QtGui.QLineEdit.keyPressEvent(self,event)
        
    def dragEnterEvent(self,event):
        ok = False
        d = event.mimeData().data("definedVariables")
        if d.data() != "":
            ok =True
        if ok:
            event.accept()
        else:
            event.ignore()
        
    def dropEvent(self,event):
        event.accept()
        txt = str(event.mimeData().text()).split()[1]
        ctxt = str(self.text())
        self.setText(ctxt+txt)
        self.setFocus()
        

class QCommandLine(QtGui.QWidget):
    """ This is the main widget containing the "Command Line Tab Window", which executes CDAT and Python commands. The Python Shell/Window gives the user access into Python's interactive mode. This tool has been slightly modified to allow VCDAT to register keystrokes for reproducibility - a feature necessary for underlying workflow and provenance procedures. """

    def __init__(self, parent=None):
        QtGui.QWidget.__init__(self, parent)

        #------------------------------------------------------------------------------
        # create objects instance for the Qt Command Line and Text Window
        #------------------------------------------------------------------------------
        self.root=parent.root
        # create objects
        label = QtGui.QLabel("Enter CDAT command and press Return")
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
        layout.setSpacing(4)
        layout.setMargin(6)
        layout.addWidget(self.te)
        layout.addWidget(label)
        layout.addWidget(self.le)
        self.setLayout(layout)



        ## Scientifc Buttons
        styles = {"background-color":"#3F3B3C",
                  "color":"white",
                  "font":"bold "
                  }
        self.topLay = QtGui.QGridLayout()
        self.Lay=self.topLay
        layout.addLayout(self.Lay)
        self.row=0
        self.col=0
        self.direction = "row"
        
        self.addButton(text='x^2',styles=styles)
        self.addButton(text='sqRT',styles=styles)
        self.addButton(text='1/x',styles=styles)
        self.addButton(text='x^y', styles=styles)
        self.newRow()
        self.addButton(text='LN', styles=styles)
        self.addButton(text='LOG', styles=styles)
        self.addButton(text='e^x', styles=styles)
        self.addButton(text='10^x', styles=styles)
        self.newRow()
        self.addButton(text='x<y', styles=styles)
        self.addButton(text='x>y', styles=styles)
        self.addButton(text='x<>y', styles=styles)
        self.addButton(text='x==y', styles=styles)
        self.newRow()
        self.addButton(text='SIN', styles=styles)
        self.addButton(text='ARCSIN', styles=styles)
        self.addButton(text='COS', styles=styles)
        self.addButton(text='ARCCOS', styles=styles)
        self.newRow()
        self.addButton(text='TAN', styles=styles)
        self.addButton(text='ARCTAN', styles=styles)
        self.addButton(text='STD',styles=styles)
        self.addButton(text='ABS',styles=styles)
        self.newRow()
        self.addButton(text='REGRID', 
                          ## icon="regrid.gif",
                          tip='Spatially regrid the first selected Defined Variable\nto the second selected Defined Variable.',styles=styles)
        self.addButton(text='MASK', 
                          ## icon="mask.gif",
                          tip='Mask variable 2 where variable 1 is "true".',styles=styles)
        self.addButton(text='GET_MASK',
                          ## icon="getmask.gif",
                          tip='Get variable mask',styles=styles)
        self.addButton(text='GROWER', 
                          ## icon="grower.gif",
                          tip='"Grows" variable 1 and variable 2 so that they end up having the same dimensions\n(order of variable 1 plus any extra dims)',styles=styles)


        self.Lay = QtGui.QGridLayout()
        layout.addLayout(self.Lay)
        # Clear/Validate Buttons
        styles["background-color"]="qradialgradient(cx:0.8, cy:0.6, radius:1, fx:.8, fy:0.8, stop:0 red, stop:1 black)"
        styles["background-color"]="#7C0404"
        self.row=0
        self.col=0
        self.direction="col"
        self.addButton(QtCore.Qt.Key_Clear,'Clear', styles=styles)
        self.addButton(QtCore.Qt.Key_Delete, 'Del',styles=styles)
        self.addButton(QtCore.Qt.Key_Enter, 'Enter',styles=styles)
        self.addButton(QtCore.Qt.Key_Equal, 'Plot', styles=styles)

        #Number Buttons
        styles["background-color"]="#6491DA"
        self.col=1
        self.row=0
        self.direction="row"
        self.addButton(QtCore.Qt.Key_7, '7', styles=styles)
        self.addButton(QtCore.Qt.Key_8, '8', styles=styles)
        self.addButton(QtCore.Qt.Key_9, '9', styles=styles)
        self.newRow(col=1)
        self.addButton(QtCore.Qt.Key_4, '4', styles=styles)
        self.addButton(QtCore.Qt.Key_5, '5', styles=styles)
        self.addButton(QtCore.Qt.Key_6, '6', styles=styles)
        self.newRow(col=1)
        self.addButton(QtCore.Qt.Key_1, '1', styles=styles)
        self.addButton(QtCore.Qt.Key_2, '2', styles=styles)
        self.addButton(QtCore.Qt.Key_3, '3', styles=styles)
        self.newRow(col=1)
        self.addButton(QtCore.Qt.Key_0, '0', styles=styles)
        self.addButton(QtCore.Qt.Key_Period, '.',styles=styles)
        self.addButton(QtCore.Qt.Key_plusminus, '+/-', styles=styles)

        # Operators
        styles["background-color"]="#78603C"
        styles["background-color"]="#B79626"
        styles["color"]="black"
        self.col=4
        self.row=0
        self.direction="col"
        self.addButton(QtCore.Qt.Key_Asterisk, '*', styles=styles)
        self.addButton(QtCore.Qt.Key_Slash, '/', styles=styles)
        self.addButton(QtCore.Qt.Key_Plus, '+', styles=styles)
        self.addButton(QtCore.Qt.Key_Minus, '-', styles=styles)

        #pi and ()
        self.newCol()
        styles["background-color"]="#578038"
        self.addButton(text = '(', styles=styles)
        self.addButton(text = ")", styles=styles)
        self.addButton(text="PI", styles=styles)
        self.addButton(text="e", styles=styles)

  
        #self.connect(self,QtCore.SIGNAL("keyRelease"),self.key)
        #------------------------------------------------------------------------------
	# connect signal - if the return key is pressed, then call run_command
        #------------------------------------------------------------------------------
        self.connect(self.le, QtCore.SIGNAL("returnPressed(void)"),
                     self.run_command)


    def newRow(self,col=0):
        self.row+=1
        self.col=col
        
    def newCol(self,row=0):
        self.row=row
        self.col+=1

    def addButton(self, key=None, text="", extraRow=0, extraCol=0,icon=None, tip=None,styles={}):
        """Adds a CalcButton"""
        button = CalcButton(text,icon=icon,tip=tip,styles=styles)
        button.associated_key =key
        self.Lay.addWidget(button, self.row, self.col, 1+extraRow, 1+extraCol)
        self.connect(button, QtCore.SIGNAL('clickedCalculator'),
                    self.issueCmd)
        if self.direction == "row":
            self.col+=1
        else:
            self.row+=1

            
    def issueCmd(self,button):
        st=""
        txt = str(button.text())
        pressEnter=False
        selected = self.root.definedVar.widget.varList.selectedItems()
        # Funcs that can take many many many variables
        if txt  in ["*","+","/","-"]:
            if len(selected)==0:
                st=txt
            elif len(selected)==1:
                st = str(self.le.text())+txt+selected[0].varName
            else:
                st = selected[0].varName
                for s in selected[1:]:
                    st+=txt+s.varName
                self.root.definedVar.widget.unselectItems(selected)
                if str(self.le.text())=="" :
                    pressEnter=True
        # 2 variable commands
        elif txt in ["x<y","x>y","x<>y","x==y"]:
            if len(selected)!=2:
                st=txt[1:-1]
            else:
                st=selected[0].varName+txt[1:-1]+selected[1].varName
                self.root.definedVar.widget.unselectItems(selected)
                if str(self.le.text())=="" :
                    pressEnter=True
        elif txt == "x^y":
            if len(selected)!=2:
                st="MV2.power("
            else:
                st="MV2.power(%s,%s)" % (selected[0].varName,selected[1].varName)
                self.root.definedVar.widget.unselectItems(selected)
                if str(self.le.text())=="" :
                    pressEnter=True
        elif txt == "REGRVARNAME":
            if len(selected)!=2:
                st=".regrvarName("
            else:
                st="%s.regrvarName(%s.getGrvarName())" % (selected[0].varName,selected[1].varName)
                self.root.definedVar.widget.unselectItems(selected)
                if str(self.le.text())=="" :
                    pressEnter=True
        elif txt == "MASK":
            if len(selected)!=2:
                st="MV2.masked_where("
            else:
                st="MV2.masked_where(%s,%s)" % (selected[0].varName,selected[1].varName)
                self.root.definedVar.widget.unselectItems(selected)
                if str(self.le.text())=="" :
                    pressEnter=True
        elif txt == "GROWER":
            if len(selected)!=2:
                st="genutil.grower("
            else:
                st="genutil.grower(%s,%s)" % (selected[0].varName,selected[1].varName)
                self.root.definedVar.widget.unselectItems(selected)
                if str(self.le.text())=="" :
                    pressEnter=True
        # ! variable only
        elif txt == "x^2":
            if len(selected)==1:
                st="%s**2" % selected[0].varName
                self.root.definedVar.widget.unselectItems(selected)
                if str(self.le.text())=="" :
                    pressEnter=True
            else:
                st="**2"
        elif txt == "sqRT":
            if len(selected)==1:
                st="MV2.sqrt(%s)" % selected[0].varName
                self.root.definedVar.widget.unselectItems(selected)
                if str(self.le.text())=="" :
                    pressEnter=True
            else:
                st="MV2.sqrt("
        elif txt == "1/x":
            if len(selected)==1:
                st="1/%s" % selected[0].varName
                self.root.definedVar.widget.unselectItems(selected)
                if str(self.le.text())=="" :
                    pressEnter=True
            else:
                st="1/"
        elif txt == "LN":
            if len(selected)==1:
                st="MV2.log(%s)" % selected[0].varName
                self.root.definedVar.widget.unselectItems(selected)
                if str(self.le.text())=="" :
                    pressEnter=True
            else:
                st="MV2.log("
        elif txt == "LOG":
            if len(selected)==1:
                st="MV2.log10(%s)" % selected[0].varName
                self.root.definedVar.widget.unselectItems(selected)
                if str(self.le.text())=="" :
                    pressEnter=True
            else:
                st="MV2.log10("
        elif txt == "e^x":
            if len(selected)==1:
                st="MV2.exp(%s)" % selected[0].varName
                self.root.definedVar.widget.unselectItems(selected)
                if str(self.le.text())=="" :
                    pressEnter=True
            else:
                st="MV2.exp("
        elif txt == "10^x":
            if len(selected)==1:
                st="MV2.power(10,%s)" % selected[0].varName
                self.root.definedVar.widget.unselectItems(selected)
                if str(self.le.text())=="" :
                    pressEnter=True
            else:
                st="MV2.power(10,"
        elif txt == "ABS":
            if len(selected)==1:
                st="MV2.absolute(%s)" % selected[0].varName
                self.root.definedVar.widget.unselectItems(selected)
                if str(self.le.text())=="" :
                    pressEnter=True
            else:
                st="MV2.absoulte(,"
        elif txt in ["SIN","ARCSIN","COS","ARCOS","TAN","ARCTAN"]:
            if len(selected)==1:
                st="MV2.%s(%s)" % (txt.lower(),selected[0].varName)
                self.root.definedVar.widget.unselectItems(selected)
                if str(self.le.text())=="" :
                    pressEnter=True
            else:
                st="MV2.%s(," % (txt.lower())
        elif txt == "STD":
            if len(selected)==1:
                st="genutil.statistics.std(%s)" % (txt.lower(),selected[0].varName)
                self.root.definedVar.widget.unselectItems(selected)
                if str(self.le.text())=="" :
                    pressEnter=True
            else:
                st="genutil.statistics.std(," % (txt.lower())
        elif txt == "Clear":
            self.le.clear()
        elif txt == "Del":
            st = str(self.le.text())[:-1]
            self.le.clear()
        elif txt == "Enter":
            pressEnter = True
        elif txt == "Plot":
            if len(str(self.le.text()))!=0:
                res = self.run_command()
                self.root.definedVar.widget.unselectItems(selected)
                self.root.definedVar.widget.selectVariableFromName(res)
                self.root.tabView.widget(1).plot()
            elif len(selected)!=0:
                self.root.tabView.widget(1).plot()
        elif txt == "=":
            if len(selected)==1:
                st = "%s =" % selected[0].varName
                self.root.definedVar.widget.unselectItems(selected)
            else:
                st="="
        elif txt == "PI":
            st="numpy.pi"
        elif txt=="e":
            st="numpy.e"
        elif txt == "+/-":
            st = str(self.le.text())
            if st[:2]=="-(" and st[-1]==")" and st.count("(")==st.count(")"):
                st=st[2:-1]
            else:
                if len(st)==0 and len(selected)==1:
                    st = "-%s" % selected[0].varName
                    self.root.definedVar.widget.unselectItems(selected)
                else:
                    st="-(%s)" % st
            self.le.clear()
        else:
            st=txt


 
        if st!="":
            self.le.setText(str(self.le.text())+st)
        if pressEnter:
            self.run_command()
        self.le.setFocus()

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
        results = "temp_results_holder"
        acommand = "temp_results_holder = %s"  % command
        exec( "import MV2,genutil", __main__.__dict__ )
        self.le.clear()
        try:
            exec( acommand, __main__.__dict__ )
        except Exception,err:
            exec( command, __main__.__dict__ )
            results=None
        res = self.root.stick_main_dict_into_defvar(results)
        #------------------------------------------------------------------------------
        # record the command for preproducibility
        #------------------------------------------------------------------------------
        self.root.record("## Command sent from prompt by user")
        self.root.record(command)
        return res
