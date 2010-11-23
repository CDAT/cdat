from PyQt4 import QtGui, QtCore
import vcs
import gui_graphicsmethods
import gui_axes
import qtbrowser

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
        self.graphicsMethodController = gui_graphicsmethods.QGraphicsMethodAttributeWindow(canvas, self)
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
        self.tabWidget = gui_axes.QAxisListTabWidget(self)

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
        axisList = gui_axes.QAxisList(cdmsFile, var, self)
        axisList.setupVariableAxes()
        self.updateVarInfo(axisList)
        self.tabWidget.createNewTab(axisList, tabName)


    def defineVarAxis(self, var, teachingCommand):
        """ Create a new tab/axisList, store it in defined var list, and record
        the associated teaching commands.  This method is called when the user
        clicks on the axisButton -> 'Get Axis Values' """
        
        cdmsFile = self.tabWidget.currentWidget().getFile()
        axisList = gui_axes.QAxisList(cdmsFile, var, self)
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
