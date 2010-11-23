from PyQt4 import QtCore, QtGui
import qtbrowser
if qtbrowser.use_vistrails:
    from cdat_cell import QCDATWidget
    import api
from gui_controller import *
from graphics_method_controller import QGraphicsMethodAttributeWindow
import cdutil
import genutil
import cdms2
import MV2
import os
import re
import vcs

class QCDATWindow(QtGui.QWidget):
    """ Main class for VCDAT Window. Contains a menu widget, file widget,
    defined variable widget, and variable widget """

    def __init__(self, parent=None):
        """ Instantiate the child widgets of the main VCDAT window and setup
        the overall layout """
        QtGui.QWidget.__init__(self, parent)
        
        # Initialize Main Window's Title and Screen Placement
        self.setGeometry(0,0, 1100,800)
        self.setWindowTitle('The Visual Climate Data Analysis Tools - (VCDAT)')
        ICONPATH = os.path.join(cdms2.__path__[0], '..', '..', '..', '..', 'bin')
        icon = QtGui.QIcon(os.path.join(ICONPATH, "UVCDATfull.gif"))
        self.setWindowIcon(QtGui.QIcon(icon))
        self.resize(1100,800)
        self.setMinimumSize(1100,800)
        self.main_window_placement()

        layout = QtGui.QVBoxLayout()
        self.setLayout(layout)

        # Init Menu Widget
        self.menuWidget = QMenuWidget(self)

        # Init Main Window Icon Tool Bar at the top of the GUI
        tool_bar = QMainToolBarContainer(QCDATFileWidget(), "")
        layout.addWidget(tool_bar)

        # Init File Widget
        vsplitter  = QtGui.QSplitter(QtCore.Qt.Vertical)        
        fileWidget = QLabeledWidgetContainer(QCDATFileWidget(),
                                             'FILE VARIABLES')
        vsplitter.addWidget(fileWidget)

        # Init Defined Variables Widget
        definedVar = QLabeledWidgetContainer(QDefinedVariable(),
                                             'DEFINED VARIABLES')
        vsplitter.addWidget(definedVar)
        hsplitter = QtGui.QSplitter(QtCore.Qt.Horizontal)
        hsplitter.addWidget(vsplitter)

        # Init Var Plotting Widget
        varView = QTabWidgetContainer(QVariableView(),
                                          'PLOTTING')
        hsplitter.addWidget(varView)
        hsplitter.setStretchFactor(2, 1)
        layout.addWidget(hsplitter)

        # Init guiController
        guiController = GuiController(fileWidget.getWidget(),
                                      definedVar.getWidget(),
                                      varView.getWidget())
        guiController.initTeachingCommands()
        self.guiController = guiController # So guicontroller doesn't get garbage collected

        # Connect signals between self & GuiController
        self.connect(self, QtCore.SIGNAL('setRecordCommands'),
                     guiController.setRecordCommands)
        self.connect(self, QtCore.SIGNAL('viewTeachingCommands'),
                     guiController.viewTeachingCommands)
        self.connect(self, QtCore.SIGNAL('closeTeachingCommands'),
                     guiController.closeTeachingCommands)        

        # Connect Signals between QVariableView & QDefinedVariable
        varView.connect(definedVar.getWidget(), QtCore.SIGNAL('selectDefinedVariableEvent'),
                        varView.getWidget().selectDefinedVariableEvent)
        varView.connect(definedVar.getWidget(), QtCore.SIGNAL('setupDefinedVariableAxes'),
                        varView.getWidget().setupDefinedVariableAxes)
        definedVar.connect(varView.getWidget(), QtCore.SIGNAL('plotPressed'),
                           definedVar.getWidget().defineQuickplot)
        definedVar.connect(varView.getWidget(), QtCore.SIGNAL('defineVariable'),
                           definedVar.getWidget().defineVariable)

        # Connect Signals between QFileWidget & QVariableView
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
#        return QtCore.QSize(1100, 800)

    def main_window_placement(self):
        screen = QtGui.QDesktopWidget().screenGeometry()
        size = self.geometry()
        self.move((screen.width()-size.width())/2, (screen.height()-size.height())/2)


class QFileDialogWidget(QtGui.QFileDialog):
    
    def __init__(self, parent=None):
        QtGui.QFileDialog.__init__(self, parent, QtCore.Qt.Widget)
        self.setModal(False)
        self.setSizeGripEnabled(False)
        self.setFileMode(QtGui.QFileDialog.ExistingFile)
        self.setLabelText(QtGui.QFileDialog.LookIn, 'Directory')
        self.setSidebarUrls([QtCore.QUrl('file://')])

        gridLayout = self.findChild(QtGui.QGridLayout, 'gridLayout')
        if gridLayout:
            gridLayout.setMargin(0)
            gridLayout.setVerticalSpacing(0)
            gridLayout.setColumnStretch(1, 1)
            hBoxLayout = gridLayout.itemAtPosition(0, 1).layout()
            if hBoxLayout:
                hBoxLayout.setSpacing(0)
        
        # Hide the Back and Forward button
        backButton = self.findChild(QtGui.QToolButton, 'backButton')
        if backButton: backButton.hide()
        forwardButton = self.findChild(QtGui.QToolButton, 'forwardButton')
        if forwardButton: forwardButton.hide()            
        
        # Hide the File Name indicators
        fileNameLabel = self.findChild(QtGui.QLabel, 'fileNameLabel')
        if fileNameLabel: fileNameLabel.hide()
        fileNameEdit = self.findChild(QtGui.QLineEdit, 'fileNameEdit')
        if fileNameEdit: fileNameEdit.hide()

        # Hide the File Type indicators
        fileTypeLabel = self.findChild(QtGui.QLabel, 'fileTypeLabel')
        if fileTypeLabel: fileTypeLabel.hide()
        fileTypeCombo = self.findChild(QtGui.QComboBox, 'fileTypeCombo')
        if fileTypeCombo: fileTypeCombo.hide()

        # Hide the dialog buttons
        buttonBox = self.findChild(QtGui.QDialogButtonBox, 'buttonBox')
        buttonBox.hide()

        # Adjust the sidebar width
        splitter = self.findChild(QtGui.QSplitter, 'splitter')
        splitter.setSizes([0, 1])

        # Simplify the Details view to List View
        stackedWidget = splitter.widget(1).findChild(QtGui.QStackedWidget, 'stackedWidget')
        listView = stackedWidget.widget(0).findChild(QtGui.QListView, 'listView')
        listView.setAlternatingRowColors(True)
        listView.setWrapping(False)
        self.setViewMode(QtGui.QFileDialog.List)

    def done(self, result):
        pass

    def sizeHint(self):
        return QtCore.QSize(384, 150)

class QCDATFileWidget(QtGui.QWidget):
    """ QCDATFileWidget contains a line-edit to enter the file name and a file
    selection button.  It also has a combo box to choose variables once a file
    is specified. """

    FILTER = "CDAT data (*.cdms *.ctl *.dic *.hdf *.nc *.xml)"

    def __init__(self, parent=None):
        QtGui.QWidget.__init__(self, parent)

        self.cdmsFile = None

        # Start the layout
        layout = QtGui.QVBoxLayout()
        self.setLayout(layout)
        layout.setMargin(0)

        self.fileDialog = QFileDialogWidget()
        self.fileDialog.setNameFilter(QCDATFileWidget.FILTER)
        layout.addWidget(self.fileDialog)

        # A shared layout of the bottom half
        vbox = QtGui.QVBoxLayout()
        vbox.setMargin(0)
        vbox.setSpacing(0)
        layout.addLayout(vbox)
        self.fileVarLayout = vbox
        
        # Create the bottom horizontal indicator
        hbox = QtGui.QHBoxLayout()
        
        self.drawerButton = QtGui.QToolButton()
        self.drawerButton.setArrowType(QtCore.Qt.UpArrow)
        self.drawerButton.setAutoRaise(True)
        self.drawerButton.setIconSize(QtCore.QSize(8, 8))
        hbox.addWidget(self.drawerButton)
        
        hline = QtGui.QFrame()
        hline.setFrameStyle(QtGui.QFrame.HLine | QtGui.QFrame.Sunken)
        hbox.addWidget(hline)

        self.connect(self.drawerButton, QtCore.SIGNAL('clicked(bool)'),
                     self.toggleFileDialog)

        vbox.addLayout(hbox)
        
        # Create the file name box
        grid = QtGui.QGridLayout()
        grid.setHorizontalSpacing(10)
        vbox.addLayout(grid)

        # First line: File
        fileNameLabel = QtGui.QLabel('File')
        grid.addWidget(fileNameLabel)

        hbox = QtGui.QHBoxLayout()
        hbox.setSpacing(6)
        self.fileNameEdit = QtGui.QLineEdit()
        self.fileNameEdit.setToolTip('Enter file name or click on button on the right to select a file')
        hbox.addWidget(self.fileNameEdit, 1)

        self.fileSelectButton = QtGui.QToolButton()
        self.fileSelectButton.setText('...')
        self.fileSelectButton.setToolTip('View and select files')
        hbox.addWidget(self.fileSelectButton)
        
        grid.addLayout(hbox, 0, 1)

        # Second line: Var
        varNameLabel = QtGui.QLabel('Variable')
        grid.addWidget(varNameLabel, 1, 0)

        hbox = QtGui.QHBoxLayout()
        hbox.setSpacing(6)

        # Init combo box
        self.varCombo = QtGui.QComboBox()
        self.varCombo.setToolTip('View and select variables in file')
        self.varCombo.setMinimumContentsLength(10)
        comboPalette = self.varCombo.view().palette()
        comboPalette.setColor(QtGui.QPalette.HighlightedText, QtCore.Qt.white)
        comboPalette.setColor(QtGui.QPalette.Highlight, QtCore.Qt.blue)        
        self.varCombo.view().setPalette(comboPalette)

        hbox.addWidget(self.varCombo, 1)
        
        self.defineVarButton = QtGui.QPushButton('&Define')
        self.defineVarButton.setToolTip('Define variable into memory')
        hbox.addWidget(self.defineVarButton)
        
        grid.addLayout(hbox, 1, 1)

        # Connect signals
        self.connect(self.fileDialog, QtCore.SIGNAL('filesSelected(const QStringList&)'),
                     self.filesSelected)
        self.connect(self.fileNameEdit, QtCore.SIGNAL('returnPressed()'),
                     self.updateCDMSFile)
        self.connect(self.fileSelectButton, QtCore.SIGNAL('clicked(bool)'),
                     self.openSelectFileDialog)
        self.connect(self.varCombo, QtCore.SIGNAL('currentIndexChanged(const QString&)'),
                     self.variableChanged)
        self.connect(self.defineVarButton, QtCore.SIGNAL('clicked(bool)'),
                     self.defineVariablePressed)

        # Init the widget with its file dialog hidden
        self.fileDialog.hide()
        self.setFileName('')
        self.varCombo.setCurrentIndex(1)

    def updateSizeConstraints(self):
        isDialogVisible = self.fileDialog.isVisible()
        if isDialogVisible:
            self.drawerButton.setArrowType(QtCore.Qt.UpArrow)
            self.setMaximumHeight(16777215)
        else:
            self.drawerButton.setArrowType(QtCore.Qt.DownArrow)
            self.setMaximumHeight(self.fileVarLayout.contentsRect().height())
        self.fileSelectButton.setVisible(not isDialogVisible)

    def toggleFileDialog(self, ignored=False):
        self.fileDialog.setVisible(not self.fileDialog.isVisible())
        self.updateSizeConstraints()

    def showEvent(self, e):
        self.updateSizeConstraints()
        self.variableChanged(self.varCombo.currentText())

    def setFileName(self, fn):
        self.fileNameEdit.setText(fn)
        self.updateCDMSFile()

    def updateCDMSFile(self):
        fn = str(self.fileNameEdit.text())
        fi = QtCore.QFileInfo(fn)
        if fi.exists():
            self.fileDialog.setDirectory(fi.dir())
            self.cdmsFile = cdms2.open(fn)
            self.recordOpenFileTeachingCommand(fn)
        else:
            self.cdmsFile = None
        self.updateVariableList()

    def recordOpenFileTeachingCommand(self, file):
        openFileComment = '\n# Open CDMS File\n'
        varName = 'fid2'
        command = "%s = cdms2.open('%s')\n" %(varName, file)

        self.emit(QtCore.SIGNAL('recordTeachingCommand'), openFileComment)
        self.emit(QtCore.SIGNAL('recordTeachingCommand'), command)

    def filesSelected(self, files):
        if files.count()>0:
            self.setFileName(files[0])
            
    def updateVariableList(self):
        self.varCombo.clear()
        if self.cdmsFile!=None:
            # Add Variables sorted based on their dimensions
            curDim = -1
            for (dim, varId) in sorted([(len(var.listdimnames()), var.id)
                                        for var in self.cdmsFile.variables.itervalues()]):
                if dim!=curDim:
                    curDim = dim
                    count = self.varCombo.count()
                    self.varCombo.insertSeparator(count)
                    self.varCombo.model().item(count, 0).setText('%dD VARIABLES' % dim)
                var = self.cdmsFile.variables[varId]
                varName = var.id + ' ' + str(var.shape) + ' ['
                
                if hasattr(var, 'long_name'):
                    varName += var.long_name
                if hasattr(var, 'units') and var.units!='':
                    if varName[-1]!='[': varName += ' '
                    varName += var.units
                varName += ']'
                self.varCombo.addItem(varName, QtCore.QVariant(QtCore.QStringList(['variables', var.id])))

            # Add Axis List
            count = self.varCombo.count()
            self.varCombo.insertSeparator(count)
            self.varCombo.model().item(count, 0).setText('AXIS LIST')
            for axis in self.cdmsFile.axes.itervalues():
                axisName = axis.id + " (" + str(len(axis)) + ") - [" + axis.units + ":  (" + str(axis[0]) + ", " + str(axis[-1]) + ")]"                
                self.varCombo.addItem(axisName, QtCore.QVariant(QtCore.QStringList(['axes', axis.id])))

            # By default, not selecting anything
            self.varCombo.setCurrentIndex(-1)

    def openSelectFileDialog(self):
        file = QtGui.QFileDialog.getOpenFileName(self, 'Open CDAT data file...',
                                                 self.fileDialog.directory().absolutePath(),
                                                 QCDATFileWidget.FILTER + ';;All files (*.*)')
        if not file.isNull():
            self.setFileName(file)

    def variableChanged(self, varName):
        if varName == '':
            return

        self.defineVarButton.setEnabled(not varName.isNull()) # Enable define button
        
        # Send signal to setup axisList in 'quickplot' tab
        self.emit(QtCore.SIGNAL('variableChanged'), self.getCDMSFile(),
                  self.getCDMSVariable(), 'quickplot')
        
    def defineVariablePressed(self):
        self.emit(QtCore.SIGNAL('defineVariableEvent'))

    def getCDMSFile(self):
        return self.cdmsFile

    def getCDMSVariable(self):
        if not self.cdmsFile is None:
            data = self.varCombo.itemData(self.varCombo.currentIndex()).toStringList()
            if data.count() > 0:
                if data[0] == 'variables':
                    return getattr(self.cdmsFile, str(data[0]))[str(data[1])]
                elif data[0] == 'axes':
                    axis = getattr(self.cdmsFile, str(data[0]))[str(data[1])]
                    var = MV2.array(axis)
                    var.setAxis(0, axis)
                    var.id = axis.id
                    return var
        return None

class QMainToolBarContainer( QtGui.QWidget ):
    """ Main icon tool bar widget that is located at the top of VCDAT's main
    window. """

    def __init__(self, widget, label='', parent=None):
        QtGui.QWidget.__init__(self, parent)

        vbox = QtGui.QVBoxLayout()
        vbox.setMargin(0)

        ICONPATH = os.path.join(cdms2.__path__[0], '..', '..', '..', '..', 'bin')
        # Create options bar
        self.toolBar = QtGui.QToolBar()
        self.setFixedHeight(50)
        #self.setFixedWidth(50)
        self.toolBar.setIconSize(QtCore.QSize(48, 48))
        actionInfo = [
            ('UVCDATfull.gif', 'Edit (in memory) selected defined variable.'),
            ('UVCDATfull.gif', 'Save selected defined variable to a netCDF file.'),
            ('UVCDATfull.gif', 'Display selected defined variable information.'),
            ('UVCDATfull.gif', 'Move selected defined variable(s) to trashcan for disposal.'),
            ('UVCDATfull.gif', 'Move [ALL] defined variables to trashcan for disposal.'),
            ('UVCDATfull.gif', 'Logged information about the defined variables.'),
            ('UVCDATfull.gif', 'Defined variable items that can be disposed of permanetly or restored.'),
            ]

        for info in actionInfo:
            icon = QtGui.QIcon(os.path.join(ICONPATH, info[0]))
            action = self.toolBar.addAction(icon, '')
            action.setStatusTip(info[1])
            action.setToolTip(info[1])
        self.toolBar.addSeparator()

        self.opButton = QtGui.QToolButton()
        self.opButton.setText('Ops')

        vbox.addWidget(self.toolBar, 0)
        self.setLayout(vbox)

        '''
        self.label = QtGui.QLabel("toolbar")
        self.label.setAutoFillBackground(True)
        self.label.setAlignment(QtCore.Qt.AlignCenter)
        self.label.setFrameStyle(QtGui.QFrame.Panel | QtGui.QFrame.Raised)
        vbox.addWidget(self.label, 0)

        if widget!=None:
            self.widget = widget
        else:
            self.widget = QtGui.QWidget()
        vbox.addWidget(self.widget, 5)
        self.setLayout(vbox)
'''

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

class QDefinedVariable(QtGui.QWidget):
    """ QDefinedVariable contains a list of the user defined variables and allows the
    user to apply functions on defined variables """

    def __init__(self, parent=None):
        QtGui.QWidget.__init__(self, parent)
        self.warningWidget = QDefVarWarningBox(self) # Popup box to warn var is already defined
        self.quickplotItem = None
        self.numVarsSelected = 0

        # Create Layout
        vbox = QtGui.QVBoxLayout()
        vbox.setMargin(0)
        self.setLayout(vbox)

        # Create Toolbar and add it to the layout
        self.createToolbar()
        vbox.addWidget(self.toolBar)

        # Create CommandLine for Simple Variable Operations
        self.command_line = QtGui.QLineEdit()
        self.command_line.setToolTip('Enter variable expression to generate a new variable (e.g., a = tas - ta + 10.0)')
        self.command_line.setText("defined variable command line")
        palette = self.command_line.palette()
        role = self.command_line.backgroundRole()
        #palette.setColor(role, QtGui.QColor(231,160,163))
        #palette.setColor(role, QtGui.QColor(246,204,174))
        palette.setColor(role, QtGui.QColor(184,212,240))
        #palette.setColor(role, QtGui.QColor(186,212,116))
        self.command_line.setPalette(palette)
        self.command_line.setAutoFillBackground(True)

        vbox.addWidget(self.command_line)

        # Create List for defined variables and add it to the layout
        self.varList = QtGui.QListWidget()
        self.varList.setAlternatingRowColors(True)
        self.varList.setSelectionMode(QtGui.QAbstractItemView.MultiSelection)
        vbox.addWidget(self.varList)

        # Connect Signals
        self.connect(self.varList, QtCore.SIGNAL('clicked(const QModelIndex&)'),
                     self.selectVariableFromListEvent)
        self.connect(self.warningWidget, QtCore.SIGNAL('newVarID'),
                     self.addVariable)      

    def defineQuickplot(self, file, var):
        """ When a user plots a variable that isn't explicitly defined a signal
        is emitted and this function is called to define the variable under
        the name 'quickplot'.  Replace the 'quickplot' variable if it
        already exists
        """
        if self.quickplotItem is None:
            self.quickplotItem = QDefinedVariableItem(file, var, 'quickplot')
            self.varList.addItem(self.quickplotItem)
        else:
            self.quickplotItem.setVariable(var)
            self.quickplotItem.setFile(file)            

    def defineVariable(self, file, var, axesArgString):
        """ Check if a variable with the same name has already been defined.
        If so, warn user and prompt for a new variable name.  Otherwise,
        add the variable to the ListWidget
        """
        if self.isVariableDefined(var.id):
            self.warningWidget.showWarning(var.id, file, var, axesArgString)
        else:
            self.addVariable(var.id, file, var, axesArgString)

    def addVariable(self, varName, file, var, axesArgString):
        """ Add variable into dict / list & emit signal to create
        a tab for the variable
        """
        # If the variable is defined, replace existing variable, else create a new variable
        if self.isVariableDefined(varName):
            item = self.getItem(varName)
            item.setVariable(var)
            item.setFile(file)
        else:
            item = QDefinedVariableItem(file, var, varName)
            self.varList.addItem(item)

        # Recording define variable teaching command
        self.recordDefineVariableTeachingCommand(varName, var.id, file, axesArgString)

        # emit signal to QVariableView to create a new axisList / tab
        self.emit(QtCore.SIGNAL('setupDefinedVariableAxes'), file, var, varName)

    def selectVariableFromListEvent(self, modelIndex):
        """ Update the number next to the selected defined variable and
        send a signal to QVariableView to display the selected variable
        """
        item = self.varList.item(modelIndex.row())
        selectedItems = self.varList.selectedItems()

        # If the item is unselected then change the selection str back to '--'
        # and decrement all the numbers of the other selected vars that are
        # less than the number of the item that was unselected
        if item not in selectedItems:
            unselectedNum = item.getSelectNum()            
            item.updateVariableString(None)
            self.numVarsSelected -= 1
            
            for item in selectedItems:
                num = item.getSelectNum()
                if num > unselectedNum:
                    item.updateVariableString(item.getSelectNum() - 1)
        # If item is selected, change the selection str to a number
        else:
            self.numVarsSelected += 1
            item.updateVariableString(self.numVarsSelected)

        # Send signal of all selected vars to qvariableview and bring up the
        # most recently selected variable's tab
        var = item.getVariable()
        cdmsFile = item.getFile()
        selectedVars = [item.getVariable() for item in selectedItems]
        tabName = item.getVarName()
        
        if item.isQuickplotItem():
            self.emit(QtCore.SIGNAL('selectDefinedVariableEvent'), 'quickplot',
                      cdmsFile, selectedVars)
        else:
            self.emit(QtCore.SIGNAL('selectDefinedVariableEvent'), tabName,
                      cdmsFile, selectedVars)

    def isVariableDefined(self, varID):
        """ Return true if a variable with the given id is defined (this does
        not include 'quickplot' """
        
        for i in range(self.varList.count()):
            item = self.varList.item(i)
            if varID == item.getVariable().id and not item.isQuickplotItem():
                return True
        return False

    def getItem(self, varID):
        """ Return the item with the defined variable with name = varID """
        for i in range(self.varList.count()):
            listItem = self.varList.item(i)
            if varID == listItem.getVariable().id:
                return listItem
        return None  

    def recordDefineVariableTeachingCommand(self, name, varName, file, axesArgString):
        if varName in list(getattr(file, 'variables')):
            fileID = "fid2"            
            command = '\n# Get new slab\n'
            command += "%s = %s('%s', %s)\n" %(name, fileID, varName, axesArgString)        

            self.emit(QtCore.SIGNAL('recordTeachingCommand'), command)

    def createToolbar(self):
        ICONPATH = os.path.join(cdms2.__path__[0], '..', '..', '..', '..', 'bin')

        # Create options bar
        self.toolBar = QtGui.QToolBar()
        self.toolBar.setIconSize(QtCore.QSize(16, 16))
        actionInfo = [
            ('edit_20.gif', 'Edit (in memory) selected defined variable.'),
            ('save_20.gif', 'Save selected defined variable to a netCDF file.'),
            ('info_20.gif', 'Display selected defined variable information.'),
            ('editdelete_20.gif', 'Move selected defined variable(s) to trashcan for disposal.'),
            ('recycle_20.gif', 'Move [ALL] defined variables to trashcan for disposal.'),
            ('log_20.gif', 'Logged information about the defined variables.'),
            ('trashcan_empty_20.gif', 'Defined variable items that can be disposed of permanetly or restored.'),
            ]
        
        for info in actionInfo:
            icon = QtGui.QIcon(os.path.join(ICONPATH, info[0]))
            action = self.toolBar.addAction(icon, '')
            action.setStatusTip(info[1])
            action.setToolTip(info[1])
        self.toolBar.addSeparator()

        self.opButton = QtGui.QToolButton()
        self.opButton.setText('Ops')
        
        # Create Operations Menu
        menu = QtGui.QMenu(self)
        grid = QtGui.QGridLayout()
        grid.setMargin(0)
        grid.setSpacing(0)
        menu.setLayout(grid)
        opDefs =[
            ['Add a number or two (or more)\nselected Defined Variables.\n(Can be used as "or")','add.gif','add'],
            ['Subtract a number or two (or more)\nselected Defined Variables.','subtract.gif','subtract'],
            ['Multiply a number or two (or more)\nselected Defined Variables.\n(Can be used as "and")','multiply.gif','multiply'],
            ['Divide a number or two (or more)\nselected Defined Variables.','divide.gif','divide'],
            ['"Grows" variable 1 and variable 2 so that they end up having the same dimensions\n(order of variable 1 plus any extra dims)','grower.gif','grower'],
            ['Spatially regrid the first selected Defined Variable\nto the second selected Defined Variable.','regrid.gif','regrid'],
            ['Mask variable 2 where variable 1 is "true".','mask.gif','mask'],
            ['Get variable mask','getmask.gif','getmask'],
            ['Return true where variable 1 is less than variable 2 (or number)','less.gif','less'],
            ['Return true where variable 1 is greater than variable 2 (or number)','greater.gif','greater'],
            ['Return true where variable 1 is equal than variable 2 (or number)','equal.gif','equal'],
            ['Return not of variable','not.gif','not'],
            ['Compute the standard deviation\n(over first axis)','std.gif','std'],
            ['Power (i.e., x ** y) of the most recently\nselected two Defined Variables, where\nx = variable 1 and y = variable 2 or float number.','power.gif','power'],
            ['Exp (i.e., e ** x) of the most recently\nselected Defined Variable.','exp.gif','exp'],
            ['Log (i.e., natural log) of the most recently\nselected Defined Variable.','mlog.gif','log'],
            ['Base10 (i.e., 10 ** x) of the most recently\nselected Defined Variable.','base10.gif','base10'],
            ['Log10 (i.e., log base 10) of the most\nrecently selected Defined Variable. ','mlog10.gif','log10'],
            ['Inverse (i.e., 1/x) of the most recently\nselected Defined Variable.','inverse.gif','inverse'],
            ['Abs (i.e., absolute value of x) of the most\nrecently selected Defined Variable.','fabs.gif','fabs'],
            ['Sine (i.e., sin) of the most recently\nselected Defined Variable.','sin.gif','sin'],
            ['Hyperbolic sine (i.e., sinh) of the most recently\nselected Defined Variable.','sinh.gif','sinh'],
            ['Cosine (i.e., cos) of the most recently\nselected Defined Variable.','cos.gif', 'cos'],
            ['Hyperbolic cosine (i.e., cosh) of the most recently\nselected Defined Variable.','cosh.gif','cosh'],
            ['Tangent (i.e., tan) of the most recently\nselected Defined Variable.','tan.gif','tan'],
            ['Hyperbolic tangent (i.e., tanh) of the most recently\nselected Defined Variable.','tanh.gif','tanh'],
            ]
        self.opActions = []
        for i in xrange(len(opDefs)):
            action = QtGui.QAction(QtGui.QIcon(os.path.join(ICONPATH, opDefs[i][1])), opDefs[i][2], menu)
            action.setStatusTip(opDefs[i][0])
            action.setToolTip(opDefs[i][0])
            self.opActions.append(action)
            b = QtGui.QToolButton()
            b.setDefaultAction(action)
            grid.addWidget(b, i/2, i%2)

        self.opButton.setMenu(menu)
        self.opButton.setPopupMode(QtGui.QToolButton.InstantPopup)
        self.connect(self.opButton, QtCore.SIGNAL('clicked(bool)'), self.opButton.showMenu)
        
        self.toolBar.addWidget(self.opButton)

class QDefinedVariableItem(QtGui.QListWidgetItem):
    """ Item to be stored by QDefinedVariable's list widget """
    
    def __init__(self, cdmsFile, variable, varName, parent=None):
        QtGui.QListWidgetItem.__init__(self, parent)
        self.varName = varName # This is also the tabname
        self.cdmsFile = cdmsFile
        self.variable = variable
        
        self.updateVariableString()

    def getVariable(self):
        return self.variable

    def getVarName(self):
        return self.varName

    def getFile(self):
        return self.cdmsFile

    def getSelectNum(self):
        return self.selectNum
        
    def isQuickplotItem(self):
        return self.varName == 'quickplot'

    def updateVariableString(self, num=None):
        """ updateVariableString(num: int)

        Update the variable string that is shown to the user in the list.
        format =  '-- variableName (shape)', where num is the selection number
        """
        if num is None:
            self.selectNum = -1
            numString = '-- '
        elif 0 < num < 10:
            self.selectNum = num
            numString = "-%s " % num
        else:
            self.selectNum = num
            numString = "%s " % num

        varString = numString + self.varName + ' ' + str(self.variable.shape)
        self.setData(0, QtCore.QVariant(QtCore.QString(varString)))

    def setFile(self, cdmsFile):
        self.cdmsFile = cdmsFile
        
    def setVariable(self, variable):
        """ Set the variable and update the variable string that is shown to the
        user in the list
        """
        self.variable = variable
        self.updateVariableString()

class QDefVarWarningBox(QtGui.QDialog):
    """ Popup box to warn a user that a variable with same name is already
    defined. Contains a line edit to allow a user to enter a new variable
    name or to replace the existing defined variable """

    def __init__(self, parent=None):
        QtGui.QDialog.__init__(self, parent)
        self.varID = None

        # Init layout
        vbox = QtGui.QVBoxLayout()
        hbox = QtGui.QHBoxLayout()
        hbox.setDirection(QtGui.QBoxLayout.RightToLeft)
        vbox.setSpacing(10)

        # Add LineEdit
        self.text = QtGui.QLabel()
        self.lineEdit = QtGui.QLineEdit()

        # Add OK / Cancel Buttons
        okButton = QtGui.QPushButton('OK')
        cancelButton = QtGui.QPushButton('Cancel')
        hbox.addWidget(cancelButton)        
        hbox.addWidget(okButton)

        vbox.addWidget(self.text)
        vbox.addWidget(self.lineEdit)
        vbox.addLayout(hbox)
        self.setLayout(vbox)

        # Connect Signals
        self.connect(okButton, QtCore.SIGNAL('pressed()'), self.okPressedEvent)
        self.connect(cancelButton, QtCore.SIGNAL('pressed()'), self.close)
        self.connect(self.lineEdit, QtCore.SIGNAL('returnPressed()'), self.okPressedEvent)

    def showWarning(self, varID, file, var, axesArgString):
        """ Show warning message and prompt user for a new variable name. Or use
        the same var name to replace the existing defined variable """
        
        self.varID = varID
        self.file = file
        self.var = var
        self.axesArgString = axesArgString
        
        message = "'%s' has already been defined.  Enter a new variable name \n or press 'OK' to replace '%s'" %(varID, varID)
        self.text.setText(message)
        self.lineEdit.setText(varID)

        self.open()

    def okPressedEvent(self):
        self.varID = self.lineEdit.text() # get the user entered variable name
        self.close()        

        # Emit signal to QDefinedVar to indicate it's ok to add the variable to defined list
        self.emit(QtCore.SIGNAL('newVarID'),
                  self.varID, self.file, self.var, self.axesArgString)

class QSliderCombo(QtGui.QWidget):
    """ Widget containing min slider, max slider, min label, max label, and a
    corresponding combo box.  The comboBox, labels, sliders must always be in
    sync with each other """

    def __init__(self, axis, parent=None):
        QtGui.QWidget.__init__(self, parent)
        self.isTime = axis.isTime()
        self.indexMode = False
        self.startIndex = 0

        # Init Layout
        hbox = QtGui.QHBoxLayout()
        vbox = QtGui.QVBoxLayout()
        vbox.setSpacing(0)
        vbox.setMargin(0)        
        self.setLayout(hbox)

        # Init combo box
        self.axisCombo = QAxisComboWidget()
        hbox.addWidget(self.axisCombo)

        # Init sliders
        hbox.addLayout(vbox)
        self.topSlider = QtGui.QSlider(QtCore.Qt.Horizontal)
        self.bottomSlider = QtGui.QSlider(QtCore.Qt.Horizontal)        
        vbox.addWidget(self.topSlider)
        vbox.addWidget(self.bottomSlider)

        # Init axis slider value labels
        self.bottomLabel = QtGui.QLabel('To', self)
        self.topLabel = QtGui.QLabel('From', self)
        hbox = QtGui.QHBoxLayout()        
        hbox.addWidget(self.topLabel)
        hbox.addWidget(self.bottomLabel)
        vbox.addLayout(hbox)

        # Initialize the sliders' and comboBox's values
        self.initAxisValues(axis)
        self.setSlidersMinMax()
        self.topSlider.setValue(self.minIndex)
        self.bottomSlider.setValue(self.maxIndex)
        self.axisCombo.initValues(self.axisValues)

        # Connect Signals
        self.connect(self.topSlider, QtCore.SIGNAL('valueChanged (int)'),
                     self.updateMin)
        self.connect(self.bottomSlider, QtCore.SIGNAL('valueChanged (int)'),
                     self.updateMax)
        self.connect(self.axisCombo,
                     QtCore.SIGNAL('axisComboMinValueChanged (int)'),
                     self.updateTopSlider)
        self.connect(self.axisCombo,
                     QtCore.SIGNAL('axisComboMaxValueChanged (int)'),
                     self.updateBottomSlider)

    def initAxisValues(self, axis):
        """ Initialize list containing the axis values and set the top slider /
        combobox value to be the min value and the bottom slider / combobox
        value to be the max value"""
        
        if (axis != None):
            if self.isTime:
                self.axisValues = [repr(t.tocomponent())
                                   for t in axis.asRelativeTime()]
            else:
                self.axisValues = axis.getValue()
        else:
            raise TypeError("Error: axis is not defined")

        self.axisIndices = range(len(self.axisValues))
        self.updateMin(0)
        self.updateMax(len(self.axisValues) - 1)

    def setStartIndex(self, index):
        self.startIndex = index

    def setIndexMode(self, mode):
        """ Set the indexMode to True or False.  If indexMode is True the widget
        will display indices.  If indexMode is False the widget will display
        actual values """
        self.indexMode = mode

    def setSlidersMinMax(self):
        self.topSlider.setMinimum(self.minIndex)
        self.bottomSlider.setMinimum(self.minIndex)
        self.topSlider.setMaximum(self.maxIndex)
        self.bottomSlider.setMaximum(self.maxIndex)

    def updateMin(self, minIndex=None):
        """ Set min value, update the slider label and the comboBox line edit
        to show the new min value"""
        
        if not minIndex is None:
            self.minIndex = minIndex
            
        if (self.indexMode == True):
            minValue = self.minIndex + self.startIndex
        else:
            minValue = self.axisValues[self.minIndex]
            
        self.topLabel.setText(str(minValue))
        self.axisCombo.setMinValue(minValue)

    def updateMax(self, maxIndex=None):
        """ Set max value, update the slider label and the comboBox line edit
        to show the new max value"""
        
        if not maxIndex is None:
            self.maxIndex = maxIndex
            
        if (self.indexMode == True):
            maxValue = self.maxIndex + self.startIndex
        else:
            maxValue = self.axisValues[self.maxIndex]

        self.bottomLabel.setText(str(maxValue))
        self.axisCombo.setMaxValue(maxValue)

    def updateTopSlider(self, index):
        self.minIndex = index
        self.topSlider.setValue(index)

    def updateBottomSlider(self, index):
        self.maxIndex = index
        self.bottomSlider.setValue(index)

    def replaceComboBoxValues(self, values):
        self.axisCombo.replaceComboBoxValues(values)

    def getAxisIndices(self):
        return self.axisIndices
    
    def getAxisValues(self):
        return self.axisValues

    def getCurrentValues(self):
        return (self.axisValues[self.minIndex], self.axisValues[self.maxIndex])

    def getCurrentValuesAsStr(self):
        if self.isTime:
            return "('%s', '%s')" % (self.axisValues[self.minIndex],
                                     self.axisValues[self.maxIndex])
        else:
            return "(%s, %s)" % (self.axisValues[self.minIndex],
                                 self.axisValues[self.maxIndex])

    def numValues(self):
        return len(self.axisValues)

    def getIndex(self):
        return (self.minIndex, self.maxIndex)

class QAxis(QtGui.QWidget):
    """ Axis widget containing: a button + popup-menu for modifying an axis, combobox
    and sliders for setting axis values, and a function def button + popup-menu """

    def __init__(self, axis, axisName, axisIndex, parent=None):
        QtGui.QWidget.__init__(self, parent)
        self.myParent = parent
        self.axis = axis
        self.axisName = axisName # Axis name including the label
        self.axisIndex = axisIndex

        hbox = QtGui.QHBoxLayout()
        hbox.setSpacing(0)
        hbox.setMargin(0)                
        self.sliderCombo = QSliderCombo(axis, self)
        hbox.addWidget(self.sliderCombo)
        self.setLayout(hbox)        

        # These buttons aren't added to the layout here, instead they are added
        # in QAxisList which has a grid layout so they can line up properly
        self.axisOperationsButton = self.createAxisOperationsButtonAndMenu()
        self.axisButton = self.createAxisButtonAndMenu()

        # Connect signals such that when the value of the axis slider is changed,
        # a signal will be emitted to update the value in the corresponding
        # Vistrails 'Variable' or 'Quickplot' box.
        self.connect(self.sliderCombo.topSlider,
                     QtCore.SIGNAL('valueChanged (int)'),
                     parent.setVistrailsVariableAxes)
        self.connect(self.sliderCombo.bottomSlider,
                     QtCore.SIGNAL('valueChanged (int)'),
                     parent.setVistrailsVariableAxes)

    def createAxisOperationsButtonAndMenu(self):
        """ Initialize the button to the right of the axis sliders and it's menu
        with operations: def, sum, avg, wgt, gtm, awt, std
        """
        button = QtGui.QToolButton()
        menu = QtGui.QMenu(self)        
        
        opDefs = ['def default axis points',
                  'sum summation of selected axis points',
                  'avg average of selected axis points',
                  'wgt weighted average of selected axis points',
                  'awt altered weighted average of selected axis points',
                  'gtm geometrical mean of selected axis points',
                  'std standard deviation of selected axis points',]
        
        for op in opDefs:
            action = menu.addAction(op)
            self.connect(action, QtCore.SIGNAL('triggered ()'),
                         self.selectAxesOperationEvent)

        button.setText(' def  ')
        button.setMenu(menu)
        button.setPopupMode(QtGui.QToolButton.InstantPopup)

        # Connect Signals
        self.connect(button, QtCore.SIGNAL('clicked(bool)'),
                     button.showMenu)

        return button

    def selectAxesOperationEvent(self):
        """ Event handler for when a user selects an operation from the
        axes operations popup menu (located to right of the sliders):
        sum, avg, awt, gtm, etc ...
        """
        # Get the operation selected by getting the text of who sent the signal
        op = self.sender().text()[:3] # def, sum, avg, wgt, awt, gtm, or std

        # If the operation is 'awt' ask the user for an alternate weight var
        if op == 'awt':
            definedVars = self.myParent.getParent().getDefinedVars()
            QReplaceAxisWeightsDialog(definedVars, self).show()
            return

        # Set button text to what the user selected
        self.axisOperationsButton.setText(" %s  " % op)

        # Update the vistrails 'Variable' module's axesOperations input
        axesOperations = self.myParent.getAxesOperations()
        varWidget = self.myParent.getParent()
        varWidget.emit(QtCore.SIGNAL('updateModule'),
                       self.myParent.currentTabName(), 'axesOperations',
                       str(axesOperations)) 

    def createAxisButtonAndMenu(self):
        """ createAxisButtonAndMenu(axesNames: list)

        Initialize the button to the left of the sliders / combo box and
        it's menu which currently has options: index, raw, get axis values,
        get axis weight values, replace axis values, re-order dimensions.
        """
        axisMenu = QtGui.QMenu(self)
        menuVbox = QtGui.QVBoxLayout()
        axisMenu.setLayout(menuVbox)
        

        # Add index options to the menu
        indexAction = axisMenu.addAction(self.axis.id + '_index')
        indexAction.setCheckable(True)

        # Add raw index option if this dimension is time
        if self.axis.isTime():
            rawIndexAction = axisMenu.addAction(self.axis.id + '_raw')
            rawIndexAction.setCheckable(True)
        
        axisMenu.addSeparator()

        # Add axis value options to the menu
        axisOptions = ['Get Axis Values', 'Get Axis Weight Values',
                       'Replace Axis Values']
        getAxisValuesAction = axisMenu.addAction(axisOptions[0])
        getAxisWeightValuesAction = axisMenu.addAction(axisOptions[1])
        replaceAxisValuesAction = axisMenu.addAction(axisOptions[2])

        # Add 're-order dimensions' option to menu
        axisMenu.addSeparator()
        reorderAxesMenu = axisMenu.addMenu('Re-Order Dimensions')
        axesNames = self.myParent.getAxesNames()                
        for axisID in axesNames:
            reorderAction = reorderAxesMenu.addAction(axisID)
            self.connect(reorderAction, QtCore.SIGNAL('triggered()'),
                         self.reorderAxesEvent)

        # Create axis button
        axisButton = QtGui.QToolButton()
        axisButton.setMenu(axisMenu)
        axisButton.setPopupMode(QtGui.QToolButton.InstantPopup)

        # Connect signals emitted by the axis button menu
        axisButton.connect(axisButton, QtCore.SIGNAL('clicked(bool)'),
                           axisButton.showMenu)
        self.connect(indexAction, QtCore.SIGNAL('toggled (bool)'),
                     self.setIndexModeEvent)
        self.connect(getAxisValuesAction, QtCore.SIGNAL('triggered ()'),
                     self.getAxisValuesEvent)
        self.connect(replaceAxisValuesAction, QtCore.SIGNAL('triggered ()'),
                     self.getReplacementAxisValuesEvent)        
        self.connect(getAxisWeightValuesAction, QtCore.SIGNAL('triggered ()'),
                     self.getAxisWeightValuesEvent)
        
        if self.axis.isTime():
            self.connect(rawIndexAction, QtCore.SIGNAL('toggled (bool)'),
                         self.setRawIndexModeEvent)
            # Dont allow _raw and _index to be checked simultaneously
            self.connect(rawIndexAction, QtCore.SIGNAL('toggled (bool)'),
                         lambda : indexAction.setChecked(False))
            self.connect(indexAction, QtCore.SIGNAL('toggled (bool)'),
                         lambda : rawIndexAction.setChecked(False))                        
            
        return axisButton

    def reorderAxesEvent(self):
        """ reorderAxesEvent is called when the user selects 're-order
        dimensions from the axis button. Swap this axis with the selected axis
        and update the vistrails' Variable module's 'axes' input
        """
        axisB = self.sender().text()
        self.myParent.swapAxes(self.axisName, axisB)
        self.myParent.setVistrailsVariableAxes()

    def getReplacementAxisValuesEvent(self):
        """ getReplacementAxisValuesEvent is called when the user selects
        'Replace Axis Values' Show a dialog box which asks the user to select
        a defined variable for replacement axis values
        """
        definedVars = self.myParent.getParent().getDefinedVars()
        QReplaceAxisValuesDialog(definedVars, self).show()
        
    def replaceAxisValues(self, newValuesVar):
        """ replaceAxisValues(newValuesVar: tvariable)

        replaceAxisValues is called after the user selects 'Replace Axis Values'
        and selects a replacement variable from the popup dialog.  This method
        replaces the axis' values with 'newValuesVar'
        """
        # TODO doesn't work
        var = self.myParent.getVar()
        axis = var.getAxis(self.axisIndex)
        axis[:] = newValuesVar.astype(axis[:].dtype.char).filled()
        axis.setBounds(None)
        self.myParent.setVar(var)

        # Generate teaching command string
        # TODO

    def getAxisWeightValuesEvent(self):
        """ getAxisWeightValuesEvent is called when the user selects 'Get Axis
        Weight Values' from the axis button menu.  This method will create and
        define a new variable / tab with the axis' weight values
        """
        var = self.myParent.getVar()
        axisVar = genutil.getAxisWeightByName(var, self.axis.id)
        axisVar.id = var.id +'_' + self.axis.id + '_weight'

        # Generate teaching command string
        fileID = 'fid2'
        teachingCommand = "\n## Getting axis %s\n" % self.axis.id
        teachingCommand += "%s = genutil.getAxisWeightByName(%s[\"%s\"], \"%s\")\n" % (axisVar.id, fileID, var.id, self.axis.id)
        teachingCommand += "%s.id = \"%s\"\n" % (axisVar.id, axisVar.id)

        # Record teaching commands associate 'get axis weight values' and
        # define a new variable/tab with only the axis' weight values
        self.myParent.defineVarAxis(axisVar, teachingCommand)

    def getAxisValuesEvent(self):
        """ getAxisValuesEvent is called when the user selects 'Get Axis
         Values' from the axis button menu.  This method will create and
        define a new variable / tab with the axis' values
        """        
        varID = self.myParent.getVar().id
        axisVar = MV2.array(self.axis)
        axisVar.setAxis(0, self.axis)
        axisVar.id = varID +'_' + self.axis.id + '_axis'

        # Generate teaching command string
        fileID = 'fid2'
        teachingCommand = "\n## Getting axis %s\n" % self.axis.id
        teachingCommand += "%s = MV2.array(%s[\"%s\"].getAxisList(axes = \"%s\")[0][:])\n" % (axisVar.id, fileID, varID, self.axis.id)
        teachingCommand += "%s.setAxis(0, %s[\"%s\"].getAxisList(axes = \"%s\")[0])\n" % (axisVar.id, fileID, varID, self.axis.id)
        teachingCommand += "%s.id = \"%s\"\n" % (axisVar.id, axisVar.id)

        # Record teaching commands associate 'get axis values' and
        # define a new variable/tab with only the axis' values        
        self.myParent.defineVarAxis(axisVar, teachingCommand)

    def setIndexModeEvent(self, indexMode):
        """ setIndexModeEvent(indexMode: bool)

        If indexMode is True then set the combobox and
        slider values to be indices.  If indexMode is False then set the
        combobox and slider values to be the actual axis values.
        """
        axisIndices = self.sliderCombo.getAxisIndices()
        axisValues = self.sliderCombo.getAxisValues()        
        
        self.sliderCombo.setIndexMode(indexMode)        
        if (indexMode == True):
            self.sliderCombo.setStartIndex(0)
            self.sliderCombo.replaceComboBoxValues(axisIndices)
        else:
            self.sliderCombo.replaceComboBoxValues(axisValues)

        self.sliderCombo.updateMax()
        self.sliderCombo.updateMin()

    def setRawIndexModeEvent(self, rawIndexMode):
        """ setRawIndexModeEvent(rawIndexMode: bool)
        
        If rawIndexMode is True then set the combobox and
        slider values to be indices since january 1st 1979.  
        otherwise set the combobox and slider values to be the actual axis
        values.
        """
        self.sliderCombo.setIndexMode(rawIndexMode)
        axisValues = self.sliderCombo.getAxisValues()
        axisIndices = self.sliderCombo.getAxisIndices()
        
        if (rawIndexMode == True):
            # Calculate months since jan 1st, 1979
            firstTimeEntry = axisValues[0]
            match = re.compile('(\d\d\d\d)(-)(.+)(-)(.*)').match(firstTimeEntry)

            if match is None:
                raise NameError("Invalid time string: %s" % firstTimeEntry)

            year = int(match.group(1))
            month = int(match.group(3))
            monthsSince1979 = 12 * (year - 1979) + (month - 1)
            self.sliderCombo.setStartIndex(monthsSince1979)

            # Add months since jan 1979 to indices
            rawIndices = map(lambda x: x + monthsSince1979, axisIndices)
            self.sliderCombo.replaceComboBoxValues(rawIndices)
        else:
            self.sliderCombo.replaceComboBoxValues(axisValues)

        self.sliderCombo.updateMax()
        self.sliderCombo.updateMin()

    def sumAxisPoints(self, var):
        """ Update the variable by applying the sum operation to the given
        axis. Note this function is called only when a variable is defined.
        """
        varID = var.id
        var = cdutil.averager(var, axis="(%s)" % self.axis.id, weight='equal',
                              action='sum')        
        var.id = varID
        return var

    def avgAxisPoints(self, var):
        """ Update the variable by applying the avg operation to the given
        axis.
        """
        varID = var.id
        var = cdutil.averager(var, axis="(%s)" % self.axis.id, weight='equal')
        var.id = varID
        return var

    def weightedAvgAxisPoints(self, var):
        """ Update the variable by applying the weighted avg operation to the
        the given axis.
        """
        varID = var.id
        var = cdutil.averager(var, axis="(%s)" % self.axis.id)
        var.id = varID
        return var

    def geoMeanAxisPoints(self, var):
        """ Update the variable by applying the geometrical mean operation to
        the given axis.
        """
        varID = var.id
        var = genutil.statistics.geometricmean(var, axis="(%s)" % self.axis.id)
        var.id = varID
        return var

    def stdAxisPoints(self, var):
        """ Update the variable by applying the standard deviation operation to
        the given axis.
        """
        varID = var.id
        var = genutil.statistics.std(var, axis="(%s)" % self.axis.id)
        var.id = varID
        return var

    def alteredWeightedAvgAxisPoints(self, var):
        """ Update the variable by applying the altered weighted avg operation
        to the given axis.
        """
        varID = var.id
        var = cdutil.averager(var, axis="(%s)" % self.axis.id,
                             weight=self.alteredWeightsVar.filled())
        var.id = varID
        return var

    def setAlteredWeights(self, var):
        self.alteredWeightsVar = var
        self.axisOperationsButton.setText(' awt  ')

    def setAxisButtonText(self, name):
        self.axisButton.setText(name + ' ')
        
    def getCurrentValues(self):
        return self.sliderCombo.getCurrentValues()

    def getCurrentValuesAsStr(self):
        return self.sliderCombo.getCurrentValuesAsStr()

    def numValues(self):
        return self.sliderCombo.numValues()

    def getIndexes(self):
        return self.sliderCombo.getIndexes()

    def getID(self):
        return self.axis.id

    def getAxisOperationsButton(self):
        return self.axisOperationsButton

    def getAxisButton(self):
        return self.axisButton

    def getAlteredWeightsVar(self):
        return self.alteredWeightsVar

class QAxisComboWidget(QtGui.QComboBox):
    """ Specialized ComboBox widget for Axis Values listing / selecting the
    axis' values. """
    
    def __init__(self, parent=None):
        QtGui.QComboBox.__init__(self, parent)
        self.setMin = False
        self.stride = 1 # TODO : Changing the stride does nothing as of now
        self.minValue = 0
        self.maxValue = 0

        self.setMouseTracking(True)
        self.setEditable(True)
        self.setMinimumContentsLength(10)
        self.setCurrentIndex(1)
        self.setMaxVisibleItems(10)
        self.setSizeAdjustPolicy(QtGui.QComboBox.AdjustToMinimumContentsLengthWithIcon)

        # Set highlighted text color to gray instead of default white
        comboPalette = self.view().palette()
        comboPalette.setColor(QtGui.QPalette.HighlightedText, QtCore.Qt.darkGray)
        self.view().setPalette(comboPalette)

        # Connect Signals
        self.connect(self, QtCore.SIGNAL('currentIndexChanged(const QString&)'),
                     self.valueChangedEvent)

    def initValues(self, axisValues):
        """ initValues(axisValues: list)

        Initialize the values in the combo box from a list of axis values.
        """
        for axisValue in axisValues:
            self.addItem(str(axisValue),
                         QtCore.QVariant(QtCore.QStringList(['variables', str(axisValue)])))

        self.minValue = axisValues[0]
        self.maxValue = axisValues[-1]
        self.setLineEditText() # Set lineedit text to be valueA : valueB by stride

    def replaceComboBoxValues(self, axisValues):
        """ replaceComboBoxValues(axisValues: list)

        Replace the values in the comboBox with new values: raw indices, indices
        or actual values.
        """
        for i in range(len(axisValues)):
            data = QtCore.QVariant(QtCore.QStringList(['variables', str(axisValues[i])]))
            self.setItemData(i, data)
            self.setItemText(i, str(axisValues[i]))

        self.minValue = axisValues[0]
        self.maxValue = axisValues[-1]
        self.setLineEditText()

    def setMinValue(self, minValue):
        self.minValue = minValue
        self.setLineEditText()

    def setMaxValue(self, maxValue):
        self.maxValue = maxValue
        self.setLineEditText()
        
    def setLineEditText(self):
        """ Set the comboBox's lineEdit to show:  minValue : maxValue by stride
        """
        self.setEditText(str(self.minValue) + " : " + str(self.maxValue) + " by "
                         + str(self.stride))
        self.lineEdit().setCursorPosition(0)

    def valueChangedEvent(self, axisValue):
        """ valueChangedEvent(axisValue: str)

        Event handler for when a user changes a combobox value by selecting a
        value or entering a value into the line edit.  Update the corresponding
        slider / label with the same value
        """
        index = self.findData(QtCore.QVariant(QtCore.QStringList(['variables', str(axisValue)])))

        # If user entered a value into the lineEdit.
        if index == -1:
            self.updateValueFromLineEditText(axisValue)
            return

        # If user selected a value from the combo box.
        # Selecting values from the combo box will alternate between setting the
        # min and max values & emit a signal to update corresponding slider
        if (self.setMin == True):
            self.setMin = False
            self.minValue = axisValue
            self.emit(QtCore.SIGNAL('axisComboMinValueChanged (int)'), self.currentIndex())
        else:
            self.setMin = True
            self.maxValue = axisValue
            self.emit(QtCore.SIGNAL('axisComboMaxValueChanged (int)'), self.currentIndex())

        self.setLineEditText()

    def updateValueFromLineEditText(self, axisValue):
        """ updateValueFromLineEditText(axisValue: str)

        Check if the user entered a valid string in the line edit and if the
        values exist. If it is valid & the values exist, set the values in the
        corresponding slidingAxisWidget so the combobox and sliders match.
        """
        # The lineEdit text must have format "ValueA : ValueB by Stride"
        pattern = re.compile("(.*)\s:\s(.*)\sby\s(\w*)")
        result = pattern.match(axisValue)

        # If invalid string format, do nothing
        if (result == None):
            return

        # Set stride if it is a valid digit
        # Stride functionality not implemented yet
        if (str(result.group(3)).isdigit() == True):
            self.stride = result.group(3)

        minValue = result.group(1)
        maxValue = result.group(2)
        minIndex = self.findData(QtCore.QVariant(QtCore.QStringList(['variables', str(minValue)])))
        maxIndex = self.findData(QtCore.QVariant(QtCore.QStringList(['variables', str(maxValue)])))

        # If min or max values are not in the list of values do nothing
        if (minIndex == -1 or maxIndex == -1):
            return

        # LineEdit string is valid, emit signal to update the corresponding axis sliders
        self.emit(QtCore.SIGNAL('axisComboMinValueChanged (int)'), minIndex)
        self.emit(QtCore.SIGNAL('axisComboMaxValueChanged (int)'), maxIndex)            

class QAxisList(QtGui.QWidget):
    """ Widget containing a list of axis widgets for the selected variable """

    def __init__(self, file=None, var=[], parent=None):
        QtGui.QWidget.__init__(self, parent)
        self.axisWidgets = [] # List of QAxis widgets
        self.axesNames = [] # List of axis names (including labels)
        self.axisOrder = [] # List of ints to specify axes ordering
        self.file = file # cdms file associated with the variable
        self.var = var # variable associated with the axes
        self.axisList = None # list of axes from the variable
        self.myParent = parent

        # Init & set the layout
        vbox = QtGui.QVBoxLayout()
        self.gridLayout = QtGui.QGridLayout()
        self.gridLayout.setMargin(0)
        self.gridLayout.setSpacing(0)
        vbox.addLayout(self.gridLayout)
        vbox.addStretch()
        vbox.setSpacing(0)
        vbox.setMargin(5)
        self.setLayout(vbox)

    def clear(self):
        """ Remove the QAxis widgets, empty axisWidgets and axesNames lists from
        the grid layout
        """
        self.gridLayout.setRowStretch(self.gridLayout.rowCount()-1, 0)
        for i in reversed(range(self.gridLayout.count())):
            item = self.gridLayout.itemAt(i)
            axis = item.widget()
            if axis:
                self.gridLayout.removeWidget(axis)
                axis.hide()
                axis.deleteLater()
            else:
                self.gridLayout.removeItem(item)
                
        self.axisWidgets = []
        self.axesNames = []

    def setupVariableAxes(self):
        """ Iterate through the variable's axes and create and initialize an Axis
        object for each axis.
        """
        if self.var is None:
            return
        
        if (self.axisList is None):
            self.axisList = self.var.getAxisList()
            self.axisOrder = range(len(self.axisList))

        self.clear()            
        self.setAxesNames()
        
        # Iterate through the variables axes & init each axis widget
        axisIndex = 0
        for axis, axisName in zip(self.axisList, self.axesNames):
            # Create the axis widget
            axisWidget = QAxis(axis, axisName, axisIndex, self)
            axisWidget.setAxisButtonText(axisName)
            self.axisWidgets.append(axisWidget)

            # Setup the layout for each axis
            row = self.gridLayout.rowCount()
            self.gridLayout.addWidget(axisWidget.getAxisButton(), row, 0)
            self.gridLayout.addWidget(axisWidget, row, 1)            
            self.gridLayout.addWidget(axisWidget.getAxisOperationsButton(), row, 2)

            # Create separator line between each axis widget
            vline = QtGui.QFrame()
            vline.setFrameStyle(QtGui.QFrame.HLine | QtGui.QFrame.Sunken)
            vline.setMidLineWidth(37)
            palette = vline.palette()
            role = vline.backgroundRole()
            palette.setColor(role, QtGui.QColor(220,213,226))
            vline.setPalette(palette)
            vline.setAutoFillBackground(True)
            self.gridLayout.addWidget(vline, row+1, 0, 1,
                                      self.gridLayout.columnCount())

            axisIndex += 1

        self.gridLayout.setRowStretch(self.gridLayout.rowCount(), 1)

    def defineVarAxis(self, var, teachingCommand):
        self.myParent.defineVarAxis(var, teachingCommand)

    def swapAxes(self, axisA, axisB):
        """ swapAxes(axisA: QAxis, axisB: QAxis)

        swap the axis widgets: axisA and axisB
        """
        if (axisA == axisB):
            return

        if axisA in self.axesNames and axisB in self.axesNames:
            i = self.axesNames.index(axisA)
            j = self.axesNames.index(axisB)
            self.axisList[i], self.axisList[j] = self.axisList[j], self.axisList[i]
            self.axisOrder[i], self.axisOrder[j] = self.axisOrder[j], self.axisOrder[i]
            self.setupVariableAxes()

    def execAxesOperations(self, var):
        """ Return the updated variable by doing the 'sum', 'avg', 'wgt', 'awt',
        'gtm' and 'std' operations.
        """
        for axis in self.axisWidgets:
            op = str(axis.getAxisOperationsButton().text()).strip()
            if op == 'sum':
                var = axis.sumAxisPoints(var)
            elif op == 'avg':
                var = axis.avgAxisPoints(var)
            elif op == 'wgt':
                var = axis.weightedAvgAxisPoints(var)
            elif op == 'gtm':
                var = axis.geoMeanAxisPoints(var)
            elif op == 'std':
                var = axis.stdAxisPoints(var)
            elif op == 'awt':
                var = axis.alteredWeightedAvgAxisPoints(var)
        return var

    # * At this point in fixing up the code

    def getAxesOperationsTeachingCommands(self, varID):
        """ Create and return the  teaching commands for the axes operations:
        def, sum, avg, wgt, awt, gtm, std
        """
        commands = ""
        for axis in self.axisWidgets:
            axisID = axis.getID()
            op = str(axis.getAxisOperationsButton().text()).strip()
            
            if op == 'sum':
                commands += "%s=cdutil.averager(%s, axis='(%s)', weight='equal', action='sum')\n" % (varID, varID, axisID)
                commands += "%s.id = '%s'\n" % (varID, varID)                
            elif op == 'avg':
                commands += "%s=cdutil.averager(%s, axis='(%s)', weight='equal')\n" % (varID, varID, axisID)                
                commands += "%s.id = '%s'\n" % (varID, varID)
            elif op == 'wgt':
                commands += "%s=cdutil.averager(%s, axis='(%s)')\n" % (varID, varID, axisID)                
                commands += "%s.id = '%s'\n" % (varID, varID)
            elif op == 'awt':
                alteredWeightsID = axis.getAlteredWeightsVar().id
                commands += "%s=cdutil.averager(%s, axis='(%s)', weight=%s.filled())" % (varID, varID, axisID, alteredWeightsID)
                commands += "\n# Currently, VCDAT cannot record the altered average weight command."
            elif op == 'gtm':
                commands += "%s=genutil.statistics.geometricmean(%s, axis='(%s)')\n" % (varID, varID, axisID)
                commands += "%s.id = '%s'\n" % (varID, varID)
            elif op == 'std':
                commands += "%s=genutil.statistics.std(%s, axis='(%s)')\n" % (varID, varID, axisID)
                commands += "%s.id = '%s'\n" % (varID, varID)                                
        return commands

    def getAxesOperations(self):
        """ Return a dict where keys are axis names, and values are the
        operation to be done on the axis such as: def, sum, avg, etc ... """
        
        axisOpsDict = {}
        for axis in self.axisWidgets:
            op = str(axis.getAxisOperationsButton().text()).strip()
            axisOpsDict[axis.getID()] = op
            
        return axisOpsDict

    def getAxesNames(self):
        return self.axesNames

    def getAxisWidgets(self):
        return self.axisWidgets

    def getFile(self):
        return self.file

    def getFileID(self):
        return self.file.id

    def getVar(self):
        return self.var

    def getVarID(self):
        return self.var.id

    def getParent(self):
        return self.myParent

    def getAxesOrderString(self):
        """ Return a string with the axes' order """
        
        order = list(self.axisOrder)
        return ''.join(map(str, order))

    def setAxesNames(self):
        """ Generate a list with the axis label + axis name and store it in
        self.axesNames """
        
        labels = ['T', 'Z', 'Y', 'X'] + [chr(ord('S')-i) for i in xrange(18)]
        if (len(self.axisList) >= 4):
            i = 0
        else:
            i = 4 - len(self.axisList)
            
        for axis in self.axisList:
            self.axesNames.append(labels[i] + ' - ' + axis.id)
            i += 1
            
    def setFile(self, cdmsFile):
        self.file = cdmsFile

    def setVar(self, var):
        self.var = var

    def currentTabName(self):
        return self.myParent.currentTabName()

    def setVistrailsVariableAxes(self):
        """ Vistrails: Update the vistrails Variable modules 'axes' input. This
        method is called whenever the sliders values are changed.
        """
        axesKwargs = {}
        # Add the each axes' args for example: latitude: (-90, 90)
        for axisWidget in self.axisWidgets:
            axesKwargs[axisWidget.axis.id] = axisWidget.getCurrentValues()
        # Add other args
        axesKwargs['squeeze'] = 0
        axesKwargs['order'] = self.getAxesOrderString()

        self.myParent.emit(QtCore.SIGNAL('updateModule'),
                         self.myParent.currentTabName(), 'axes', str(axesKwargs))

class QVariableView(QtGui.QWidget):
    """ Main widget containing plotting related information / options. Contains
    a tab widget with a tab for each defined variable, plotting options widget,
    and variable information widget """
    
    def __init__(self, parent=None):
        QtGui.QWidget.__init__(self, parent)
        self.selectedVars = []
        
        # Init layout
        vbox = QtGui.QVBoxLayout()
        vbox.setMargin(0)
        self.setLayout(vbox)

        self.plotOptions = QPlotOptionsWidget(self)
        self.tabWidget = QAxisListTabWidget(self)

        # Init variable information widget
        self.varInfoWidget = QtGui.QTextEdit()
        self.varInfoWidget.setText('')
        self.varInfoWidget.setReadOnly(True)

        # Create splitter for tabWidget and variable information text
        vsplitter = QtGui.QSplitter(QtCore.Qt.Vertical)
        vsplitter.addWidget(self.tabWidget)
        vsplitter.addWidget(self.varInfoWidget)
        vsplitter.setStretchFactor(1,1)
        vsplitter.setSizes([300, 200])
        
        vbox.addWidget(self.plotOptions)
        vbox.addWidget(vsplitter)

    def getUpdatedVar(self):
        """ Return a new tvariable object with the updated information from
        evaluating the var with the current user selected args / options
        """
        axisList = self.tabWidget.currentWidget()
        kwargs = self.generateKwArgs()
        updatedVar = axisList.getVar()(**kwargs)

        # Get the variable after carrying out the: def, sum, avg... operations
        updatedVar = axisList.execAxesOperations(updatedVar)

        return updatedVar

    def generateKwArgs(self, axisList=None):
        """ Generate and return the variable axes keyword arguments """
        if axisList is None:
            axisList = self.tabWidget.currentWidget()

        kwargs = {}        
        for axisWidget in axisList.getAxisWidgets():
            kwargs[axisWidget.axis.id] = axisWidget.getCurrentValues()

        # Generate additional args
        kwargs['squeeze'] = 0
        kwargs['order'] = axisList.getAxesOrderString()

        return kwargs

    def generateKwargsAsString(self):
        """ Generate and return the variable axes keyword arguments as a string
        that is formatted to be used in teaching commands
        """
        args = ""
        axisList = self.tabWidget.currentWidget()

        for axisWidget in axisList.getAxisWidgets():
            args += "%s = %s, " % (axisWidget.axis.id,
                                   axisWidget.getCurrentValuesAsStr())

        # Generate additional args
        args += 'squeeze = 0'
        args += ", order = '%s' " % axisList.getAxesOrderString()
        return args

    def updateVarInfo(self, axisList):
        """ Update the text box with the variable's information """
        if axisList is None:
            return
        
        var = axisList.getVar()
        varInfo = ''
        for line in var.listall():
            varInfo += line + '\n'
        self.varInfoWidget.setText(varInfo)

    def setupDefinedVariableAxes(self, cdmsFile, var, tabName):
        """ Create a new axis list and tab with the specified tab name and setup
        the axis list.  Initialize the vistrails Variable module associated with
        the tab and variable.
        """
        if var is None:
            return

        # Create and setup the axislist
        axisList = QAxisList(cdmsFile, var, self)
        axisList.setupVariableAxes()
        self.updateVarInfo(axisList)
        self.tabWidget.createNewTab(axisList, tabName)


    def defineVarAxis(self, var, teachingCommand):
        """ Create a new tab/axisList, store it in defined var list, and record
        the associated teaching commands.  This method is called when the user
        clicks on the axisButton -> 'Get Axis Values' """
        
        cdmsFile = self.tabWidget.currentWidget().getFile()
        axisList = QAxisList(cdmsFile, var, self)
        axisList.setupVariableAxes()
        argString = self.generateKwargsAsString()

        self.emit(QtCore.SIGNAL('recordTeachingCommand'), teachingCommand)
        self.emit(QtCore.SIGNAL('defineVariable'), cdmsFile, var, argString)

    def defineVariableEvent(self):
        """ Get a copy of the updated var and file and pass it to the Defined
        Variables widget """
        if self.tabWidget.currentWidget() is None:
            return

        cdmsFile = self.tabWidget.currentWidget().getFile()        
        var = self.getUpdatedVar()
        argString = self.generateKwargsAsString()        

        self.emit(QtCore.SIGNAL('defineVariable'), cdmsFile, var, argString)

    def selectDefinedVariableEvent(self, tabName, cdmsFile, selectedVars):
        """ Save the list of selected variables and show the selected variable,
        variables are sorted in least recently selected to most recently selected
        """
        if selectedVars != []:
            self.selectedVars = selectedVars        
            self.tabWidget.selectAndUpdateDefinedVarTab(tabName, cdmsFile, selectedVars[-1])

    def plot(self):
        """ Create the graphics method and cdatcell modules. Update the input
        port values and setup connections. Then plot.
        """
        if self.tabWidget.count() == 0:
            return

        # Error if not enough slabs
        plotType = str(self.plotOptions.getPlotType())        
        if len(self.selectedVars) < 2 and self.requiresTwoSlabs(plotType):
            self.showError('Error Message to User', 'Vector, Scatter, Meshfill or XvsY plots \nmust have two data variables. The data \nvariables must be selected in the \n"Defined Variables" window.')
            return

        # Create & Update the graphics method / CDATCell vistrails modules
        # *** IMPORTANT ***
        # Everytime plot is pressed, this will create a new Graphics Method and
        # CDATCell Module. Instead it should ONLY create a new graphics method
        # and CDATCell module if the variable isn't already connected to an
        # existing Graphics Method / CDATCell module.  This results in plots 
        # being plotted multiple times.
        axisList = self.tabWidget.currentWidget()
        if qtbrowser.use_vistrails:
            self.setVistrailsCDATCell()

        # Get the names of the 2 slabs so we can connect their modules in vistrails
        if self.requiresTwoSlabs(plotType):
            var1 = self.selectedVars[-1].id
            var2 = self.selectedVars[-2].id
        else:
            var1 = self.currentTabName()
            var2 = None

        # Emit signal to GuiController to connect ports and plot
        self.emit(QtCore.SIGNAL('plot'), var1, var2)

        # If a quickplot is plotted, define current variable under 'quickplot'
        if (self.currentTabName() == 'quickplot'):
            var = self.getUpdatedVar()
            self.emit(QtCore.SIGNAL('plotPressed'), axisList.getFile(), var)

        # Record plot teaching commands
        self.recordPlotTeachingCommand()

    def recordPlotTeachingCommand(self):
        axisList = self.tabWidget.currentWidget()
        tabName = self.tabWidget.currentTabName()
        argString = self.generateKwargsAsString()
        var = axisList.getVar()
        fileID = "fid2"

        slabCommand = ''
        if tabName == 'quickplot':
            slabCommand += '\n# Get new slab\n'
            slabCommand += "%s = %s('%s', %s)\n" %(tabName, fileID, var.id, argString)

        slabCommand += '\n# Get new slab\n'
        slabCommand += "%s = %s(%s)\n" %(tabName, tabName, argString)
            
        slabCommand += axisList.getAxesOperationsTeachingCommands(tabName)

        canvasNum = 0 # Change the canvas # with respect to the cell?
        clearCommand = '\n# Clear the VCS Canvas\n'        
        clearCommand +=  "vcs_canvas_list[%d].clear()\n" % canvasNum

        plotID = "vcs_display"
        plotType = str(self.plotOptions.getPlotType())
        template = self.getTemplateName(plotType)
        gm = self.getGraphicsMethodName(plotType)                
        plotArgs = "%s, '%s', '%s', '%s'" % (tabName, template, plotType, gm)

        if self.plotOptions.getContinentType() is not None:
            plotArgs += ", continents = %d" % self.plotOptions.getContinentType()
        
        plotCommand = '\n# Plot slab\n'        
        plotCommand += "%s = vcs_canvas_list[%d].plot(%s)\n" %(plotID, canvasNum, plotArgs)

        command = slabCommand + clearCommand + plotCommand
        self.emit(QtCore.SIGNAL('recordTeachingCommand'), command)

    def requiresTwoSlabs(self, plotType):
        """ Returns true if the plot requires 2 slabs """
        multiVarPlots = ['Vector', 'Scatter', 'XvsY']
        return plotType in multiVarPlots

    def getDefinedVars(self):
        """ Get a list of all of the defined tabnames / variables """
        numTabs = self.tabWidget.count()
        varList = []
        
        for i in range(numTabs):
            var = self.tabWidget.widget(i).getVar()
            name = self.tabWidget.tabText(i)
            varList.append([name, var])

        return varList

    def getAxisList(self, var):
        for i in range(self.tabWidget.count()):
            if self.tabWidget.widget(i).getVar() is var:
                return self.tabWidget.widget(i)

        return None

    def getTemplateName(self, plotType):
        """ Return the template given the plotType.  This is currently hardcoded
        but should change based on the user? """

        # TODO ?
        return self.getGraphicsMethodName(plotType)

    def getGraphicsMethodName(self, plotType):
        """ Return the graphics method given the plotType.  This is currently
        hardcoded but should change based on the user? """

        # TODO ?
        hasASD = ['Boxfill', 'Isofill', 'Isoline', 'Scatter', 'Taylordiagram']
        hasquick = ['Vector']
        hasASD1 = ['Xyvsy', 'Yxvsx']          

        if plotType in hasASD:
            return 'ASD'
        if plotType in hasquick:
            return 'quick'
        if plotType in hasASD1:
            return 'ASD1'
        return 'default'        
    
    def setVistrailsCDATCell(self):
        """ Vistrails: Update the vistrails' CDAT Cell modules' input ports: """
        
        visInput = []
        plotType = str(self.plotOptions.getPlotType())

        visInput.append(('plotType', plotType))
        visInput.append(('row', str(self.plotOptions.getRow())))
        visInput.append(('col', str(self.plotOptions.getCol())))
        visInput.append(('gmName', self.getGraphicsMethodName(plotType)))
        visInput.append(('template', self.getTemplateName(plotType)))

        if self.plotOptions.getContinentType() is not None:
            visInput.append(('continents', self.plotOptions.getContinentType())) # TODO

        self.emit(QtCore.SIGNAL('updateModuleOps'), cdatcell_name, visInput)

    def setVistrailsGraphicsMethod(self):
        """ Vistrails: Update the vistrails' Graphics Method modules' boxfill
        input ports.  Only set the plotType and graphics method (gm) name.
        Setting the input for gm attributes should be handled by the gm
        controller (graphics_method_controller.py)
        """
        visInput = [] # List of tuples where each tuple = (inputPortName, value)
        plotType = str(self.plotOptions.getPlotType())
        
        visInput.append(('plotType', plotType))
        visInput.append(('gmName', self.getGraphicsMethodName(plotType)))
        self.emit(QtCore.SIGNAL('updateModuleOps'), gm_name, visInput)

    def showError(self, title, text):
        """ Show an error message in a simple popup message box. Currently there
        is no error icon. """
        
        errorWidget = QtGui.QMessageBox(self)
        errorWidget.setWindowTitle(title)
        errorWidget.setText(text)
        errorWidget.show()

    def currentTabName(self):
        return self.tabWidget.currentTabName()

class QSelectVarDialog(QtGui.QDialog):
    """ Popup box that allows user to selected a defined variable to replace an
    axis """

    def __init__(self, definedVars, parent):
        QtGui.QDialog.__init__(self, parent)
        self.selectedVariable = None
        self.currentListIndex = None
        self.definedVars = definedVars
        self.myParent = parent
        
        vbox = QtGui.QVBoxLayout()
        hbox = QtGui.QHBoxLayout()
        self.setLayout(vbox)        

        vbox.addWidget(QtGui.QLabel("Defined Variables:"))

        # Add List of defined vars
        self.varList = QtGui.QListWidget()
        self.varList.setAlternatingRowColors(True)
        vbox.addWidget(self.varList)

        # Initialize the list of defined vars
        # self.definedVars = parent.getDefinedVars()
        for name, var in self.definedVars:
            label = name + ' ' + str(var.shape)
            item = QtGui.QListWidgetItem()
            item.setData(0, QtCore.QVariant(QtCore.QString(label)))
            self.varList.addItem(item)

        # Add OK / Cancel Buttons
        okButton = QtGui.QPushButton('OK')
        cancelButton = QtGui.QPushButton('Cancel')
        hbox.addWidget(okButton)
        hbox.addWidget(cancelButton)                
        vbox.addLayout(hbox)

        # Connect Signals
        self.connect(self.varList, QtCore.SIGNAL('clicked(const QModelIndex&)'),
                     self.selectVariableFromListEvent)        
        self.connect(okButton, QtCore.SIGNAL('pressed()'),
                     self.selectDefinedVariableEvent)
        self.connect(cancelButton, QtCore.SIGNAL('pressed()'), self.close)

    def selectVariableFromListEvent(self, modelIndex):
        self.currentListIndex = modelIndex

    def varIsValid(self, var):
        """ Return true if the var has 1 dimension and it has the same # of
        values as the one it is replacing
        """
        if len(var.getAxisList()) != 1:
            return False
        if self.myParent.numValues() != self.myParent.numValues():
            return False
        return True

    def showError(self):
        errorMsg = "Invalid shape or size. Axis requiers a 1D \narray that matches in size."
        errorMessageBox = QtGui.QMessageBox(self)
        errorMessageBox.setWindowTitle("Error")
        errorMessageBox.setText(errorMsg)
        errorMessageBox.show()
        # TODO need an icon for the message box                
        
    def selectDefinedVariableEvent(self):
        # Implement in child        
        return

class QReplaceAxisValuesDialog(QSelectVarDialog):
    """ Popup box that prompts user to select a variable to replace the current
    axis values """    

    def __init__(self, definedVars, parent):
        QSelectVarDialog.__init__(self, definedVars, parent)
        self.setWindowTitle("Replace Axis Values")

    def selectDefinedVariableEvent(self):
        # Do nothing if no variable is selected
        if self.currentListIndex is None:
            return 
        
        name, var = self.definedVars[self.currentListIndex.row()]
        if self.varIsValid(var):
            self.myParent.replaceAxisValues(var)
            self.close()
        else:
            self.showError()

class QReplaceAxisWeightsDialog(QSelectVarDialog):
    """ Popup box that prompts user to select a variable to replace the current
    axis weight values """

    def __init__(self, definedVars, parent):
        QSelectVarDialog.__init__(self, definedVars, parent)
        self.setWindowTitle("Replace Dimension Weights")

    def selectDefinedVariableEvent(self):
        # Do nothing if no variable is selected        
        if self.currentListIndex is None: 
            return 
        
        name, var = self.definedVars[self.currentListIndex.row()]
        if self.varIsValid(var):
            self.myParent.setAlteredWeights(var)
            self.close()
        else:
            self.showError()

class QPlotOptionsWidget(QtGui.QWidget):
    """ Widget containing plot options: plot button, plot type combobox, cell
    col and row selection combo box, and an options button """
    
    def __init__(self, parent=None):
        QtGui.QWidget.__init__(self, parent)
        self.myParent = parent
        self.cellRow = -1 # if row/col = -1, then automatically plot in an open cell
        self.cellCol = -1
        hbox = QtGui.QHBoxLayout()

        # Add plot button
        self.plotButton = QtGui.QPushButton('&Plot')
        hbox.addWidget(self.plotButton)

        # Add plot type combo box
        self.plotTypeCombo = QtGui.QComboBox()
        comboPalette = self.plotTypeCombo.view().palette()
        comboPalette.setColor(QtGui.QPalette.HighlightedText, QtCore.Qt.white)
        comboPalette.setColor(QtGui.QPalette.Highlight, QtCore.Qt.blue)                
        self.plotTypeCombo.view().setPalette(comboPalette)

        plotTypes = ['Boxfill', 'Isofill', 'Isoline', 'Meshfill', 'Outfill',
                     'Outline', 'Scatter', 'Taylordiagram', 'Vector', 'XvsY',
                     'Xyvsy', 'Yxvsx','Ext']
        self.plotTypeCombo.addItems(plotTypes)
        hbox.addWidget(self.plotTypeCombo)

        # Add cell row / col combo boxes
        rowLabel = QtGui.QLabel('Row')
        colLabel = QtGui.QLabel('Col')
        self.cellRowCombo = QtGui.QComboBox()
        self.cellColCombo = QtGui.QComboBox()

        comboPalette = self.cellRowCombo.view().palette()
        comboPalette.setColor(QtGui.QPalette.HighlightedText, QtCore.Qt.white)
        comboPalette.setColor(QtGui.QPalette.Highlight, QtCore.Qt.blue)                
        self.cellRowCombo.view().setPalette(comboPalette)

        comboPalette = self.cellColCombo.view().palette()
        comboPalette.setColor(QtGui.QPalette.HighlightedText, QtCore.Qt.white)
        self.cellColCombo.view().setPalette(comboPalette)        

        self.cellRowCombo.addItem('Auto')
        self.cellColCombo.addItem('Auto')
        
        for i in range(1, 9):
            self.cellRowCombo.addItems(str(i))
            self.cellColCombo.addItems(str(i))

        hbox.addWidget(rowLabel)
        hbox.addWidget(self.cellRowCombo)
        hbox.addWidget(colLabel)
        hbox.addWidget(self.cellColCombo)

        # Create the options menu
        optionsMenu = QtGui.QMenu(self)
        menuVbox = QtGui.QVBoxLayout()
        menuVbox.setMargin(2)
        optionsMenu.setLayout(menuVbox)

        # Create the Continents Types Menu
        self.continentTypesMenu = QCheckMenu()
        self.continentTypesMenu.setTitle('Continent Types')
        optionsMenu.addMenu(self.continentTypesMenu)        
        self.continentTypes = [('Auto Continents', None),
                               ('No Continents', 0),
                               ('Coarse Continents', 2),
                               ('Fine Continents', 1),
                               ('United States Continents', 3),
                               ('Political Boundary Continents', 4),
                               ('Rivers Continents', 5),
                               ('User1 Continents', 6),
                               ('User2 Continents', 7),
                               ('User3 Continents', 8),
                               ('User4 Continents', 9),
                               ('User5 Continents', 10),
                               ('User6 Continents', 11)]

        for continentType in self.continentTypes:
            self.continentTypesMenu.addCheckableAction(continentType[0])
        self.continentTypesMenu.setDefaultAction('Auto Continents')

        # Create graphic method attribute option / editor
        canvas = vcs.init()
        self.graphicsMethodController = QGraphicsMethodAttributeWindow(canvas, self)
        graphicMethodAction = optionsMenu.addAction('Se&t Graphics Method Attributes')

        # Create Colormap option / widget
        colorMapAction = optionsMenu.addAction("&Colormap Editor")
        self.colorDialog = QtGui.QColorDialog(self)
        self.connect(colorMapAction, QtCore.SIGNAL('triggered ()'),
                     self.colorDialog.open)

        # Create the options button
        self.optionButton = QtGui.QToolButton()
        self.optionButton.setText(' Options  ')
        self.optionButton.setMenu(optionsMenu)
        self.optionButton.setPopupMode(QtGui.QToolButton.InstantPopup)

        hbox.addWidget(self.optionButton)
        hbox.addStretch()        
        self.setLayout(hbox)

        # Connect Signals
        self.connect(self.plotButton, QtCore.SIGNAL('clicked(bool)'),
                     parent.plot)
        self.connect(self.optionButton, QtCore.SIGNAL('clicked(bool)'),
                     self.optionButton.showMenu)
        self.connect(graphicMethodAction, QtCore.SIGNAL('triggered ()'),
                     self.graphicsMethodController.show)

    def getRow(self):
        if self.cellRowCombo.currentText() == 'Auto':
            return -1
        return int(self.cellRowCombo.currentText()) - 1

    def getCol(self):
        if self.cellColCombo.currentText() == 'Auto':
            return -1
        return int(self.cellColCombo.currentText()) - 1

    def getPlotType(self):
        return self.plotTypeCombo.currentText()

    def getContinentType(self):
        """ Returns None or a number from 0-11 corresponding to the option
        selected in the continents menu """
        
        selectedText = self.continentTypesMenu.getCheckedText()
        for text, continentType in self.continentTypes:
            if text == selectedText:
                return continentType

        return None

    def getGraphicsMethodController(self):
        return self.graphicsMethodController

    def getParent(self):
        return self.myParent

class QCheckMenu(QtGui.QMenu):
    """ Menu where only a single 'checkable' action can be checked at a time """

    def __init__(self, parent=None):
        QtGui.QMenu.__init__(self, parent)
        self.defaultAction = None
        self.currentAction = None
        self.handleCheckEvent = False
        self.actions = {}

    def addCheckableAction(self, actionText):
        action = self.addAction(actionText)
        action.setCheckable(True)
        self.actions[actionText] = action
        self.connect(action, QtCore.SIGNAL('toggled (bool)'), self.checkEvent)

    def setDefaultAction(self, actionText):
        if actionText in list(self.actions):
            self.currentAction = self.defaultAction = self.actions[actionText]
            self.defaultAction.setChecked(True)

    def getCheckedText(self):
        return self.currentAction.text()

    def checkEvent(self, isChecked):
        """ Force the menu to have only 1 item checked.  If an action is
        unchecked, check the default item """

        # handleCheckEvent prevents infinite recursion because this method also
        # generates checkEvents
        if self.handleCheckEvent == False:
            self.handleCheckEvent = True
            return
        
        if isChecked == True:
            self.handleCheckEvent = False
            self.currentAction.setChecked(False)            
            self.currentAction = self.sender()
        elif isChecked == False and not self.defaultAction is None:
            self.handleCheckEvent = False
            self.defaultAction.setChecked(True)

class QAxisListTabWidget(QtGui.QTabWidget):
    """ TabWidget where each tab contains a QAxisList """
    
    def __init__(self, parent=None):
        QtGui.QTabWidget.__init__(self, parent)
        self.myParent = parent

        self.connect(self, QtCore.SIGNAL('currentChanged(int)'),
                     self.tabChangeEvent)        

    def createNewTab(self, axisList, tabName):
        """ Create a new tab given the axisList widget and tab name.  If a
        tab with the same name exists already, replace it. Set the current tab
        to the newly created tab """

        if (self.tabExists(tabName)):
            self.removeTab(self.getTabIndexFromName(tabName))

        if tabName == 'quickplot':
            self.insertTab(0, axisList, tabName) # quickplot is always first tab
        else:
            self.addTab(axisList, tabName)
            
        self.setTabTip(tabName)
        self.setCurrentIndex(self.getTabIndexFromName(tabName))

    def setupQuickplotTab(self, axisList):
        """ Create a new quickplot tab if one doesn't exist otherwise replace
        the existing quickplot tab """

        tabName = 'quickplot'
        if (self.tabExists(tabName) == True):
            index = self.getTabIndexFromName(tabName)
            self.removeTab(index)
            
        self.insertTab(0, axisList, tabName)
        self.setTabTip(tabName)
        self.setCurrentIndex(0)

    def setTabTip(self, tabName):
        if self.tabExists(tabName):
            index = self.getTabIndexFromName(tabName)
            toolTip = "'%s' axisList tab widget" % tabName
            self.setTabToolTip(index, toolTip)        

    def tabChangeEvent(self, tabIndex):
        """ Event handler for when a tab is changed. Update the variable info
        text box.
        """
        axisList = self.widget(tabIndex)
        self.myParent.updateVarInfo(axisList)

    def selectAndUpdateDefinedVarTab(self, tabName, cdmsFile, var):
        """ This function selects a tab given the tabName and then updates the
        tab's axisList using the passed var's values
        """
        if (not self.tabExists(tabName)):
            raise NameError("Error: tab '%s' was not found" % tabName)

        tabIndex = self.getTabIndexFromName(tabName)
        self.setCurrentIndex(tabIndex)

        if (tabName == 'quickplot'):
            axisList = QAxisList(cdmsFile, var, self)            
            self.setupQuickplotTab(axisList)
            axisList.setupVariableAxes()
        else:
            axisList = self.currentWidget()
            axisList.setupVariableAxes()

        self.emit(QtCore.SIGNAL('updateVarInfo'), axisList)        

    def tabExists(self, name):
        """ Returns True if a tab with the given name exists """
        numTabs = self.count()        
        for i in range(numTabs):
            if (name == self.tabText(i)):
                return True
        return False

    def getTabIndexFromName(self, name):
        numTabs = self.count()
        for i in range(numTabs):
            if (name == self.tabText(i)):
                return i
        return None

    def currentTabName(self):
        currentTab = self.currentIndex()
        return str(self.tabText(currentTab))

class QMenuWidget(QtGui.QMenuBar):
    def __init__(self, parent=None):
        QtGui.QMenuBar.__init__(self, parent)
        self.myParent = parent
        
        self.file = self.addMenu('&File')
        self.pref = self.addMenu('&Preferences')
        self.tools = self.addMenu('&Tools')
        self.pcmdiTools = self.addMenu('&PCMDITools')
        self.help = self.addMenu('&Help')
        self.initToolsMenu()

    def initToolsMenu(self):
        recordTeachingAction = self.tools.addAction('Record Commands')
        recordTeachingAction.setCheckable(True)
        recordTeachingAction.setChecked(True)
        
        viewTeachingAction = self.tools.addAction('View Teaching Commands')
        closeTeachingAction = self.tools.addAction('Close Teaching Commands')        

        self.connect(viewTeachingAction, QtCore.SIGNAL('triggered ()'),
                     self.viewTeachingCommands)
        self.connect(closeTeachingAction, QtCore.SIGNAL('triggered ()'),
                     self.closeTeachingCommands)        
        self.connect(recordTeachingAction, QtCore.SIGNAL('toggled (bool)'),
                     self.setRecordCommands)

    def setRecordCommands(self, checked):
        self.myParent.emit(QtCore.SIGNAL('setRecordCommands'), checked)

    def viewTeachingCommands(self):
        self.myParent.emit(QtCore.SIGNAL('viewTeachingCommands'))

    def closeTeachingCommands(self):
        self.myParent.emit(QtCore.SIGNAL('closeTeachingCommands'))

