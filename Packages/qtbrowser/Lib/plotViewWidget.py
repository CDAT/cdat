from PyQt4 import QtGui, QtCore
import vcs
import qtbrowser
import vcsPlotControllerWidget
import customizeVCDAT

class QPlotOptionsWidget(QtGui.QWidget):
    """ Widget containing plot options: plot button, plot type combobox, cell
    col and row selection combo box, and an options button """
    
    def __init__(self, parent=None):
        QtGui.QWidget.__init__(self, parent)
        self.parent = parent
        self.cellRow = -1 # if row/col = -1, then automatically plot in an open cell
        self.cellCol = -1
        layout = QtGui.QVBoxLayout()
        self.setLayout(layout)
        frame1 = QtGui.QFrame()
        layout.addWidget(frame1)
        self.frame2 = QtGui.QFrame()
        layout.addWidget(self.frame2)
        hbox = QtGui.QHBoxLayout()
        frame1.setLayout(hbox)
        self.root = parent.root
        # Add plot button
        self.plotButton = QtGui.QPushButton('&Plot')
        hbox.addWidget(self.plotButton)

        # Add plot type combo box
        self.plotTypeCombo = QtGui.QComboBox()
        comboPalette = self.plotTypeCombo.view().palette()
        comboPalette.setColor(QtGui.QPalette.HighlightedText, QtCore.Qt.white)
        comboPalette.setColor(QtGui.QPalette.Highlight, QtCore.Qt.blue)                
        self.plotTypeCombo.view().setPalette(comboPalette)

        plotTypes = customizeVCDAT.plotTypes
        
        self.plotTypeCombo.addItems(plotTypes)
        hbox.addWidget(self.plotTypeCombo)

        if qtbrowser.useVistrails:
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
        #canvas = vcs.init()
        #self.graphicsMethodController = graphicsMethodsWidgets.QGraphicsMethodAttributeWindow(canvas, self)
        graphicMethodAction = optionsMenu.addAction('Se&t Graphics Method Attributes')

        # Create Colormap option / widget
        colorMapAction = optionsMenu.addAction("&Colormap Editor")
        self.colorDialog = QtGui.QColorDialog(self)
        self.connect(colorMapAction, QtCore.SIGNAL('triggered ()'),
                     self.colorDialog.open)

        # Create the options button
        self.optionButton = QtGui.QPushButton()
        self.optionButton.setText(' Options  ')
        self.optionButton.setMenu(optionsMenu)
        #self.optionButton.setPopupMode(QtGui.QToolButton.InstantPopup)

        hbox.addWidget(self.optionButton)
        #hbox.addStretch()


        self.plotTypeCombo.setCurrentIndex(1)

        # Connect Signals
        self.connect(self.plotButton, QtCore.SIGNAL('clicked(bool)'),
                     parent.plot)
        self.connect(self.optionButton, QtCore.SIGNAL('clicked(bool)'),
                     self.optionButton.showMenu)
        self.connect(self.plotTypeCombo,QtCore.SIGNAL('currentIndexChanged(const QString&)'),
                     self.selectedPlotType)
        self.connect(self.plotTypeCombo,QtCore.SIGNAL('activated(int)'),
                     self.selectedPlotType)
        ## self.connect(graphicMethodAction, QtCore.SIGNAL('triggered ()'),
        ##              self.graphicsMethodController.show)

        self.layout=layout
        
    def selectedPlotType(self,*args):
        ptype = self.getPlotType()
        self.frame2.destroy()
        self.layout.removeWidget(self.frame2)
        if self.parent.isVCSPlot(ptype):
            self.frame2=vcsPlotControllerWidget.QVCSPlotController(self.parent)
            self.layout.addWidget(self.frame2)
        else:
            print 'Cannot construct controller for graphic method type:',ptype
        
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
        return self.parent

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



class QPlotView(QtGui.QWidget):
    """ Main widget containing plotting related information / options. Contains
    a tab widget with a tab for each defined variable, plotting options widget,
    and variable information widget """
    
    def __init__(self, parent=None):
        QtGui.QWidget.__init__(self, parent)
        self.selectedVars = []
        self.root=parent.root
        self.canvas=[]
        for i in range(4):
            self.canvas.append(vcs.init())
        # Init layout
        vbox = QtGui.QVBoxLayout()
        vbox.setMargin(0)
        vbox.setAlignment(QtCore.Qt.AlignTop)
        self.setLayout(vbox)

        self.plotOptions = QPlotOptionsWidget(self)

        
        vbox.addWidget(self.plotOptions)
        #self.plotOptions.plotTypeCombo.emit(QtCore.SIGNAL('currentIndexChanged(QString("Meshfill"))'))
     #   if qtbrowser.useVistrails:
     #       import cdat_cell
     #       self.spreadSheet = cdat_cell.QCDATWidget(self)

    def isVCSPlot(self,plotType):
        if str(plotType).lower() in ['boxfill', 'isofill', 'isoline', 'meshfill', 'outfill',
                                'outline', 'scatter', 'taylordiagram', 'vector', 'xvsy',
                                'xyvsy', 'yxvsx']:
            return True
        else:
            return False
         
    def plot(self):
        """ Create the graphics method and cdatcell modules. Update the input
        port values and setup connections. Then plot.
        """
        ## if self.tabWidget.count() == 0:
        ##     return

        # Error if not enough slabs
        plotType = str(self.plotOptions.getPlotType()).lower()

        
        selectedVars=self.root.definedVar.widget.getSelectedDefinedVariables()

        
        if len(selectedVars) < 2 and self.requiresTwoSlabs(plotType):
            self.showError('Error Message to User', 'Vector, Scatter, Meshfill or XvsY plots \nmust have two data variables. The data \nvariables must be selected in the \n"Defined Variables" window.')
            return


        if len(selectedVars) == 0:
            # Nothing selected grabs what's in the file
            self.root.tabView.widget(0).defineVariableEvent()
            # now "select" it
            vnm = str(self.root.tabView.widget(0).fileWidget.widget.varCombo.currentText()).split()[0]
            for i in range(self.root.definedVar.widget.varList.count()):
                if vnm == str(self.root.definedVar.widget.varList.item(i).text()).split()[1]:
                    break
                
            self.root.definedVar.widget.varList.setCurrentItem(self.root.definedVar.widget.varList.item(i))
            selectedVars=self.root.definedVar.widget.getSelectedDefinedVariables()
            
        # Get the names of the 2 slabs so we can connect their modules in vistrails
        if self.requiresTwoSlabs(plotType):
            var = selectedVars[:2]
            plot_args="%s, %s" % (var[0].id,var[1].id)
        else:
            var = [selectedVars[0],]
            plot_args="%s" % (var[0].id,)


        if self.isVCSPlot(plotType):
            # Template section
            template = self.getTemplateName()
            var.append(template)
            plot_args+=",'%s'" % template

            # Plot type section
            var.append(plotType)
            plot_args+=", '%s'" % plotType

            # Graphic method
            gm_name=self.getGraphicsMethodName()
            var.append(gm_name)
            plot_args+=", '%s'" % gm_name

            icanvas = 0

            if qtbrowser.useVistrails:
                row = self.plotOptions.getRow()
                col = self.plotOptions.getCol()
                self.root.record('## Plotting into the spreadsheet')
                self.root.record('plot(%s,row=%s,col=%s)'% (plot_args,
                                                            row,
                                                            col))
            else:
                self.root.record("## Clearing vcs canvas %i" % icanvas)
                self.root.record("vcs_canvas[%i].clear()" % icanvas)
                self.canvas[icanvas].clear()
                #For now dirty plot_args

                self.root.record("## Plotting onto canvas %i" % icanvas)
                self.root.record("vcs_canvas[%i].plot(%s)" % (icanvas,plot_args))
                self.canvas[icanvas].plot(*var)
        else:
            print "Don't Know how to plot this yet"


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

    def getTemplateName(self):
        """ Return the template given the plotType. """
        return str(self.plotOptions.frame2.templates.templateList.currentItem().text())

    def getGraphicsMethodName(self):
        """ Return the graphics method given the plotType.  This is currently
        hardcoded but should change based on the user? """

        return str(self.plotOptions.frame2.gms.gmList.currentItem().text())
    
    ## def setVistrailsCDATCell(self):
    ##     """ Vistrails: Update the vistrails' CDAT Cell modules' input ports: """
        
    ##     visInput = []
    ##     plotType = str(self.plotOptions.getPlotType())

    ##     visInput.append(('plotType', plotType))
    ##     visInput.append(('row', str(self.plotOptions.getRow())))
    ##     visInput.append(('col', str(self.plotOptions.getCol())))
    ##     visInput.append(('gmName', self.getGraphicsMethodName(plotType)))
    ##     visInput.append(('template', self.getTemplateName(plotType)))

    ##     if self.plotOptions.getContinentType() is not None:
    ##         visInput.append(('continents', self.plotOptions.getContinentType())) # TODO

    ##     self.emit(QtCore.SIGNAL('updateModuleOps'), cdatcell_name, visInput)

    ## def setVistrailsGraphicsMethod(self):
    ##     """ Vistrails: Update the vistrails' Graphics Method modules' boxfill
    ##     input ports.  Only set the plotType and graphics method (gm) name.
    ##     Setting the input for gm attributes should be handled by the gm
    ##     controller (graphics_method_controller.py)
    ##     """
    ##     visInput = [] # List of tuples where each tuple = (inputPortName, value)
    ##     plotType = str(self.plotOptions.getPlotType())
        
    ##     visInput.append(('plotType', plotType))
    ##     visInput.append(('gmName', self.getGraphicsMethodName(plotType)))
    ##     self.emit(QtCore.SIGNAL('updateModuleOps'), gm_name, visInput)

    def showError(self, title, text):
        """ Show an error message in a simple popup message box. Currently there
        is no error icon. """
        
        errorWidget = QtGui.QMessageBox(self)
        errorWidget.setWindowTitle(title)
        errorWidget.setText(text)
        errorWidget.show()

    def currentTabName(self):
        return self.tabWidget.currentTabName()
