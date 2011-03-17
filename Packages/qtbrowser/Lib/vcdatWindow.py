from PyQt4 import QtGui, QtCore

import fileWidget
import definedVariableWidget
import mainToolbarWidget
import mainMenuWidget
import commandLineWidget
import plotViewWidget
import variableViewWidget
import commandsRecorderWidget
import vcdatCommons
import os
import cdms2 # need to remove this!
import __main__


class QCDATWindow(QtGui.QMainWindow):
    """ Main class for VCDAT Window. Contains a menu widget, file widget,
    defined variable widget, and variable widget """

    def __init__(self, parent=None,styles={}):
        """ Instantiate the child widgets of the main VCDAT window and setup
        the overall layout """
        centralWidget= QtGui.QWidget()
        QtGui.QMainWindow.__init__(self,parent)

        ## StylesSheet
        st=""
        if isinstance(styles,str):
            st = styles
        elif isinstance(styles,dict):
            for k in styles.keys():
                val = styles[k]
                if isinstance(val,QtGui.QColor):
                    val = str(val.name())
                st+="%s:%s; " % (k,val)
        if len(st)>0: self.setStyleSheet(st)

        
        self.setCentralWidget(centralWidget)
        
        ###########################################################
        ###########################################################
        ## Prettyness
        ###########################################################
        ###########################################################
        self.setGeometry(0,0, 1100,800)
        self.setWindowTitle('The Visual Climate Data Analysis Tools - (VCDAT)')
        self.resize(1100,800)
        self.setMinimumSize(1100,800)
        self.main_window_placement()

        self.root = self
        
        layout = QtGui.QVBoxLayout()
        centralWidget.setLayout(layout)

        # Create the command recorder widget
        self.recorder = commandsRecorderWidget.QCommandsRecorderWidget(self)

        #Adds a shortcut to the record function
        self.record = self.recorder.record
        
        ###########################################################
        ###########################################################
        ## WIDGETS
        ###########################################################
        ###########################################################

        ###########################################################
        # Init Menu Widget
        ###########################################################
        self.mainMenu = mainMenuWidget.QMenuWidget(self)

        ###########################################################
        # Init Main Window Icon Tool Bar at the top of the GUI
        ###########################################################
        self.tool_bar = mainToolbarWidget.QMainToolBarContainer(self)
        layout.addWidget(self.tool_bar)
        
        ###########################################################
        # Splitter between defined variables and controls tabs
        ###########################################################
        hsplitter = QtGui.QSplitter(QtCore.Qt.Horizontal)
        
        ###########################################################
        # Defined Variables Widget
        ###########################################################
        self.definedVar = vcdatCommons.QLabeledWidgetContainer(definedVariableWidget.QDefinedVariableWidget(self),'DEFINED VARIABLES', self)        
        hsplitter.addWidget(self.definedVar)

        ###########################################################
        ###########################################################
        #  Controls Tabs Widget
        ###########################################################
        ###########################################################
        self.tabView = QtGui.QTabWidget()
        hsplitter.addWidget(self.tabView)
        hsplitter.setStretchFactor(1, 2)

        ###########################################################
        # File/Variable Controls Tab
        ###########################################################
        self.tabView.addTab(variableViewWidget.QVariableView(self),"Variables")
        ###########################################################
        # Plotting Controls Tab
        ###########################################################
        self.tabView.addTab(plotViewWidget.QPlotView(self), "Plot")
        ###########################################################
        # Calculator and Command Line Controls Tab
        ###########################################################
        self.tabView.addTab(commandLineWidget.QCommandLine(self), "Calculator")

        layout.addWidget(hsplitter)
        self.show()
        self.tabView.widget(1).show()
        self.tabView.widget(1).plotOptions.plotTypeCombo.setCurrentIndex(0)


        ###########################################################
        ###########################################################
        ## SIGNALS
        ###########################################################
        ###########################################################

        ###########################################################
        ## Connect Signals between QVariableView & QDefinedVariable
        ###########################################################
        self.tabView.widget(0).connect(self.definedVar.getWidget(), QtCore.SIGNAL('selectDefinedVariableEvent'),
                        self.tabView.widget(0).selectDefinedVariableEvent)
        self.tabView.widget(0).connect(self.definedVar.getWidget(), QtCore.SIGNAL('setupDefinedVariableAxes'),
                        self.tabView.widget(0).setupDefinedVariableAxes)
        self.definedVar.connect(self.tabView.widget(0), QtCore.SIGNAL('plotPressed'),
                           self.definedVar.getWidget().defineQuickplot)
        self.definedVar.connect(self.tabView.widget(0), QtCore.SIGNAL('definedVariableEvent'),
                           self.definedVar.getWidget().addVariable)

        ###########################################################
        ## Connect Signals between QFileWidget & QVariableView
        ###########################################################
        self.tabView.widget(0).connect(self.tabView.widget(0).fileWidget.getWidget(), QtCore.SIGNAL('variableSelectedEvent'),
                        self.tabView.widget(0).setupFileVariableAxes)
        self.tabView.widget(0).connect(self.tabView.widget(0).fileWidget.getWidget(), QtCore.SIGNAL('defineVariableFromFileEvent'),
                        self.tabView.widget(0).defineVariableEvent)
        fw = self.tabView.widget(0).fileWidget.widget
        fw.connect(fw.plotButton,QtCore.SIGNAL('clicked(bool)'),self.tabView.widget(1).plot)
    def closeEvent(self, event):
        # TODO
        # closeEvent() isn't called vistrails is closed,
        # perhaps because the event isn't propagated by vistrails? Therefore,
        # any functionality we want to execute when vistrails exits is not done
        # unless the user specifically closes this gui.
        
        self.emit(QtCore.SIGNAL('closeTeachingCommands'))

#    def sizeHint(self):
#        return QtCore.QSize(1024, 600)

    def main_window_placement(self):
        screen = QtGui.QDesktopWidget().screenGeometry()
        size = self.geometry()
        self.move((screen.width()-size.width())/2, (screen.height()-size.height())/2)

    
    def stick_defvar_into_main_dict(self,var):
        __main__.__dict__[var.id]=var

    def stick_main_dict_into_defvar(self,results=None):
        #First evaluate if there's any var in the result
        res = None
        if results is not None:
            tmp = __main__.__dict__[results]
        else:
            tmp=None
        added = []
        remove =[]
        if isinstance(tmp,cdms2.tvariable.TransientVariable):
            __main__.__dict__[tmp.id]=tmp
            added.append(tmp.id)
            res = tmp.id
        else:
            self.processList(tmp,added)
            
        if results is not None:
            del(__main__.__dict__[results])
        for k in __main__.__dict__:
            if isinstance(__main__.__dict__[k],cdms2.tvariable.TransientVariable):
                if __main__.__dict__[k].id in added and k!=__main__.__dict__[k].id:
                    remove.append( __main__.__dict__[k].id)
                    res = k
                if not k in remove:
                    __main__.__dict__[k].id=k
                    self.definedVar.getWidget().addVariable(__main__.__dict__[k])
        for r in remove:
            del(__main__.__dict__[r])

        return res
    
    def processList(self,myList,added):
        for v in myList:
            if isinstance(v,cdms2.tvariable.TransientVariable):
                __main__.__dict__[v.id]=v
                added.append(v.id)
            elif isinstance(v,(list,tuple)):
                self.processList(v,added)
            elif isinstance(v,dict):
                self.processList(dict.values(),added)
        return
