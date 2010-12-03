from PyQt4 import QtGui, QtCore

import fileWidget
import definedVariableWidget
import mainToolbarWidget
import mainMenuWidget
import commandLineWidget
import plotViewWidget
import variableViewWidget
import os
import cdms2 # need to remove this!

class QCDATWindow(QtGui.QMainWindow):
    """ Main class for VCDAT Window. Contains a menu widget, file widget,
    defined variable widget, and variable widget """

    def __init__(self, parent=None):
        """ Instantiate the child widgets of the main VCDAT window and setup
        the overall layout """
        ## QtGui.QWidget.__init__(self, parent)
        centralWidget= QtGui.QWidget()
        QtGui.QMainWindow.__init__(self,parent)
        self.setCentralWidget(centralWidget)
        
        self.setGeometry(0,0, 1100,800)
        self.setWindowTitle('The Visual Climate Data Analysis Tools - (VCDAT)')
        ICONPATH = os.path.join(cdms2.__path__[0], '..', '..', '..', '..', 'bin')
        icon = QtGui.QIcon(os.path.join(ICONPATH, "UVCDATfull.gif"))
        self.setWindowIcon(QtGui.QIcon(icon))
        self.resize(1100,800)
        self.setMinimumSize(1100,800)
        self.main_window_placement()

        
        layout = QtGui.QVBoxLayout()
        centralWidget.setLayout(layout)

        # Init Menu Widget
        self.setMenuWidget = mainMenuWidget.QMenuWidget()

        # Init Main Window Icon Tool Bar at the top of the GUI
        self.tool_bar = mainToolbarWidget.QMainToolBarContainer(QtGui.QWidget(self), "")
        layout.addWidget(self.tool_bar)
        
        # Init File Widget
        vsplitter  = QtGui.QSplitter(QtCore.Qt.Vertical)        
        ## vsplitter.addWidget(fileWidget)

        # Init Defined Variables Widget
        self.definedVar = QLabeledWidgetContainer(definedVariableWidget.QDefinedVariableWidget(self),
                                             'DEFINED VARIABLES', self)
        vsplitter.addWidget(self.definedVar)
        hsplitter = QtGui.QSplitter(QtCore.Qt.Horizontal)
        hsplitter.addWidget(vsplitter)

        # Init Var Plotting Widget
        self.tabView = QtGui.QTabWidget()

        self.tabView.addTab(variableViewWidget.QVariableView(self),"Variables")
        self.tabView.addTab(plotViewWidget.QPlotView(self), "Plot")
        self.tabView.addTab(commandLineWidget.QCommandLine(self), "CommandLine")
        hsplitter.addWidget(self.tabView)
        hsplitter.setStretchFactor(2, 1)
        layout.addWidget(hsplitter)

        ## Connect Signals between QVariableView & QDefinedVariable
        self.tabView.widget(0).connect(self.definedVar.getWidget(), QtCore.SIGNAL('selectDefinedVariableEvent'),
                        self.tabView.widget(0).selectDefinedVariableEvent)
        self.tabView.widget(0).connect(self.definedVar.getWidget(), QtCore.SIGNAL('setupDefinedVariableAxes'),
                        self.tabView.widget(0).setupDefinedVariableAxes)
        self.definedVar.connect(self.tabView.widget(0), QtCore.SIGNAL('plotPressed'),
                           self.definedVar.getWidget().defineQuickplot)
        self.definedVar.connect(self.tabView.widget(0), QtCore.SIGNAL('definedVariableEvent'),
                           self.definedVar.getWidget().addVariable)

        ## ## Connect Signals between QFileWidget & QVariableView
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

    
class QLabeledWidgetContainer(QtGui.QWidget):
    """ Container widget for the 3 main widgets: QVariableView, QCDATFileWidget,
    and QDefinedVariable """

    def __init__(self, widget, label='', parent=None):
        QtGui.QWidget.__init__(self, parent)

        vbox = QtGui.QVBoxLayout()
        vbox.setMargin(0)
        
        self.label = QtGui.QLabel(label)
        self.label.setAutoFillBackground(True)
        self.label.setAlignment(QtCore.Qt.AlignCenter)
        self.label.setFrameStyle(QtGui.QFrame.Panel | QtGui.QFrame.Raised)
        vbox.addWidget(self.label, 0)

        if widget!=None:
            self.widget = widget
        else:
            self.widget = QtGui.QWidget()
        vbox.addWidget(self.widget, 1)
        
        self.setLayout(vbox)

    def getWidget(self):
        return self.widget

    def event(self, e):
        if e.type()==76: #QtCore.QEvent.LayoutRequest:
            self.setMaximumHeight(min(self.label.height()+self.layout().spacing()+
                                      self.widget.maximumHeight(), 16777215))
        return False

