from PyQt4 import QtGui, QtCore

import gui_filewidget
import gui_definedvariablewidget
import gui_variableviewwidget
import gui_controller
import gui_mainmenuwidget
import gui_maintoolbar
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
        self.setMenuWidget = gui_mainmenuwidget.QMenuWidget()

        # Init Main Window Icon Tool Bar at the top of the GUI
        tool_bar = gui_maintoolbar.QMainToolBarContainer(gui_filewidget.QCDATFileWidget(), "")
        layout.addWidget(tool_bar)
        
        # Init File Widget
        vsplitter  = QtGui.QSplitter(QtCore.Qt.Vertical)        
        fileWidget = QLabeledWidgetContainer(gui_filewidget.QCDATFileWidget(),
                                             'FILE VARIABLES')
        vsplitter.addWidget(fileWidget)

        # Init Defined Variables Widget
        definedVar = QLabeledWidgetContainer(gui_definedvariablewidget.QDefinedVariable(),
                                             'DEFINED VARIABLES')
        vsplitter.addWidget(definedVar)
        hsplitter = QtGui.QSplitter(QtCore.Qt.Horizontal)
        hsplitter.addWidget(vsplitter)

        # Init Var Plotting Widget
        varView = QTabWidgetContainer(gui_variableviewwidget.QVariableView(),
                                          'PLOTTING')
        hsplitter.addWidget(varView)
        hsplitter.setStretchFactor(2, 1)
        layout.addWidget(hsplitter)

        # Init guiController
        self.guiController = gui_controller.GuiController(fileWidget.getWidget(),
                                      definedVar.getWidget(),
                                      varView.getWidget())
        self.guiController.initTeachingCommands()

        # Connect signals between self & GuiController
        self.connect(self, QtCore.SIGNAL('setRecordCommands'),
                     self.guiController.setRecordCommands)
        self.connect(self, QtCore.SIGNAL('viewTeachingCommands'),
                     self.guiController.viewTeachingCommands)
        self.connect(self, QtCore.SIGNAL('closeTeachingCommands'),
                     self.guiController.closeTeachingCommands)        

        ## Connect Signals between QVariableView & QDefinedVariable
        varView.connect(definedVar.getWidget(), QtCore.SIGNAL('selectDefinedVariableEvent'),
                        varView.getWidget().selectDefinedVariableEvent)
        varView.connect(definedVar.getWidget(), QtCore.SIGNAL('setupDefinedVariableAxes'),
                        varView.getWidget().setupDefinedVariableAxes)
        definedVar.connect(varView.getWidget(), QtCore.SIGNAL('plotPressed'),
                           definedVar.getWidget().defineQuickplot)
        definedVar.connect(varView.getWidget(), QtCore.SIGNAL('defineVariable'),
                           definedVar.getWidget().defineVariable)

        ## Connect Signals between QFileWidget & QVariableView
        varView.connect(fileWidget.getWidget(), QtCore.SIGNAL('variableChanged'),
                        varView.getWidget().setupDefinedVariableAxes)
        varView.connect(fileWidget.getWidget(), QtCore.SIGNAL('defineVariableEvent'),
                        varView.getWidget().defineVariableEvent)

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




class QTabWidgetContainer(QtGui.QWidget):
    """ Container widget for the 3 main widgets: QVariableView, QCDATFileWidget,
    and QDefinedVariable """

    def __init__(self, widget, label='', parent=None):
        QtGui.QWidget.__init__(self, parent)
       
        vbox = QtGui.QVBoxLayout()
        vbox.setMargin(0)

        tab_widget = QtGui.QTabWidget(self)
        variable_tab = QtGui.QWidget()
        plot_tab = QtGui.QWidget()
        command_tab = QtGui.QWidget()

        variable_grid_layout = QtGui.QGridLayout(variable_tab)
        plot_grid_layout = QtGui.QGridLayout(plot_tab)
        command_grid_layout = QtGui.QGridLayout(command_tab)

        #Adding tabs to the main gui widget
        #Here the SVGTab is a widget that is added as a tab to the "tab_widget"
        tab_widget.addTab(variable_tab, "Variable")
        tab_widget.addTab(plot_tab, "Plot")
        tab_widget.addTab(command_tab, "Command Line")


        if widget!=None:
            self.widget = widget
        else:
            self.widget = QtGui.QWidget()
        vbox.addWidget(self.widget, 1)

        #self.setLayout(vbox)

    def getWidget(self):
        return self.widget

        '''
        self.label = QtGui.QLabel(tab_bar)
        self.label.setAutoFillBackground(True)
        self.label.setAlignment(QtCore.Qt.AlignCenter)
        self.label.setFrameStyle(QtGui.QFrame.Panel | QtGui.QFrame.Raised)
        vbox.addWidget(self.label, 0)

        # Set the background color of the label
        palette = self.label.palette()
        role = self.label.backgroundRole()
        palette.setColor(role, QtGui.QColor(220,213,226))
        self.label.setPalette(palette)
        self.label.setAutoFillBackground(True)

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
'''


