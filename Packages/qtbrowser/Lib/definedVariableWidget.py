from PyQt4 import QtGui, QtCore
import os
import cdms2



class QDefinedVariableWidget(QtGui.QWidget):
    """ QDefinedVariable contains a list of the user defined variables and allows the
    user to apply functions on defined variables """

    def __init__(self, parent=None):
        QtGui.QWidget.__init__(self, parent)
        self.warningWidget = QDefVarWarningBox(self) # Popup box to warn var is already defined
        self.quickplotItem = None
        self.numVarsSelected = 0
        self.root=parent.root
        # Create Layout
        vbox = QtGui.QVBoxLayout()
        vbox.setMargin(0)
        self.setLayout(vbox)

        # Create Toolbar and add it to the layout
        self.createToolbar()
        vbox.addWidget(self.toolBar)

        # Create CommandLine for Simple Variable Operations
        ## self.command_line = QtGui.QLineEdit()
        ## self.command_line.setToolTip('Enter variable expression to generate a new variable (e.g., a = tas - ta + 10.0)')
        ## self.command_line.setText("defined variable command line")
        ## palette = self.command_line.palette()
        ## role = self.command_line.backgroundRole()
        ## #palette.setColor(role, QtGui.QColor(231,160,163))
        ## #palette.setColor(role, QtGui.QColor(246,204,174))
        ## palette.setColor(role, QtGui.QColor(184,212,240))
        ## #palette.setColor(role, QtGui.QColor(186,212,116))
        ## self.command_line.setPalette(palette)
        ## self.command_line.setAutoFillBackground(True)

        ## vbox.addWidget(self.command_line)

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
            self.quickplotItem = QDefinedVariableItem(file, var)
            self.varList.addItem(self.quickplotItem)
        else:
            self.quickplotItem.setVariable(var)
            self.quickplotItem.setFile(file)            


    def getSelectedDefinedVariables(self):
        """ Get a list of all of the defined tabnames / variables """
        selectedItems = self.varList.selectedItems()
        varList = []
        for item in selectedItems:
            varList.append(item.getVariable())
        
        return varList
    def addVariable(self, var):
        """ Add variable into dict / list & emit signal to create
        a tab for the variable
        """
        item = QDefinedVariableItem(var)
        for i in range(self.varList.count()-1,-1,-1):
            if self.varList.item(i).getVarName() == var.id:
                self.varList.takeItem(i)
        self.varList.addItem(item)

        # Recording define variable teaching command
#        self.recordDefineVariableTeachingCommand(varName, var.id, file, axesArgString)

        # emit signal to QVariableView to create a new axisList / tab
        self.emit(QtCore.SIGNAL('setupDefinedVariableAxes'), var)

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
        ## selectedVars = [item.getVariable() for item in selectedItems]
        tabName = item.getVarName()
        
        self.emit(QtCore.SIGNAL('selectDefinedVariableEvent'), tabName, var)

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
    
    def __init__(self, variable, parent=None):
        QtGui.QListWidgetItem.__init__(self, parent)
        self.varName = variable.id # This is also the tabname
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
