from PyQt4 import QtGui, QtCore
import axesWidgets
import vcdatWindow
import fileWidget

class QVariableView(QtGui.QWidget):
    """ Main widget containing plotting related information / options. Contains
    a tab widget with a tab for each defined variable, plotting options widget,
    and variable information widget """
    
    def __init__(self, parent=None):
        QtGui.QWidget.__init__(self, parent)
        self.root=parent.root
        self.fileWidget = vcdatWindow.QLabeledWidgetContainer(fileWidget.QCDATFileWidget(self),
                                             'FILE VARIABLE')
       
        # Init layout
        vbox = QtGui.QVBoxLayout()
        vbox.setMargin(0)
        self.setLayout(vbox)

        vbox.addWidget(self.fileWidget)
        
        self.tabWidget = axesWidgets.QAxisListTabWidget(self)

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

    ## def generateKwargsAsString(self):
    ##     """ Generate and return the variable axes keyword arguments as a string
    ##     that is formatted to be used in teaching commands
    ##     """
    ##     args = ""
    ##     axisList = self.tabWidget.currentWidget()

    ##     for axisWidget in axisList.getAxisWidgets():
    ##         args += "%s = %s, " % (axisWidget.axis.id,
    ##                                axisWidget.getCurrentValuesAsStr())

    ##     # Generate additional args
    ##     args += 'squeeze = 0'
    ##     args += ", order = '%s' " % axisList.getAxesOrderString()
    ##     return args

    def updateVarInfo(self, axisList):
        """ Update the text box with the variable's information """
        if axisList is None:
            return
        
        var = axisList.getVar()
        varInfo = ''
        for line in var.listall():
            varInfo += line + '\n'
        self.varInfoWidget.setText(varInfo)

    def setupFileVariableAxes(self, cdmsFile, var, tabName):
        """ Create a new axis list and tab with the specified tab name and setup
        the axis list. 
        """
        if var is None:
            return

        # Create and setup the axislist
        axisList = axesWidgets.QAxisList(cdmsFile, var, self)
        axisList.setupVariableAxes()
        self.updateVarInfo(axisList)
        self.tabWidget.createNewTab(axisList, tabName)
        
    def setupDefinedVariableAxes(self, var):
        """ Create a new axis list and tab with the specified tab name and setup
        the axis list.
        """
        print 'in setupDefinedVariableAxes you need to update this one!'
        if var is None:
            return
        tabName=var.id
        # Create and setup the axislist
        axisList = axesWidgets.QAxisList(None, var, self)
        axisList.setupVariableAxes()
        self.updateVarInfo(axisList)
        self.tabWidget.createNewTab(axisList, tabName)


    def defineVarAxis(self, var, teachingCommand):
        """ Create a new tab/axisList, store it in defined var list, and record
        the associated teaching commands.  This method is called when the user
        clicks on the axisButton -> 'Get Axis Values' """
        
        cdmsFile = self.tabWidget.currentWidget().getFile()
        axisList = axesWidgets.QAxisList(cdmsFile, var, self)
        axisList.setupVariableAxes()
        argString = self.generateKwargsAsString()

        self.emit(QtCore.SIGNAL('recordTeachingCommand'), teachingCommand)
        self.emit(QtCore.SIGNAL('defineVariable'), cdmsFile, var, argString)

    def defineVariableEvent(self):
        """ Get a copy of the updated var and file and pass it to the Defined
        Variables widget """
        if self.tabWidget.currentWidget() is None:
            return


#        self.definedVariables.append( ))
        self.emit(QtCore.SIGNAL('definedVariableEvent'),self.getUpdatedVar())

    def selectDefinedVariableEvent(self, tabName, var):#cdmsFile, selectedVars):
        """ Save the list of selected variables and show the selected variable,
        variables are sorted in least recently selected to most recently selected
        """
        print 'useless function need to repalced  directly with bellow one'
        self.tabWidget.selectAndUpdateDefinedVarTab(tabName, None, var)


    ## def getAxisList(self, var):
    ##     for i in range(self.tabWidget.count()):
    ##         if self.tabWidget.widget(i).getVar() is var:
    ##             return self.tabWidget.widget(i)

    ##     return None

    ## def showError(self, title, text):
    ##     """ Show an error message in a simple popup message box. Currently there
    ##     is no error icon. """
        
    ##     errorWidget = QtGui.QMessageBox(self)
    ##     errorWidget.setWindowTitle(title)
    ##     errorWidget.setText(text)
    ##     errorWidget.show()

    ## def currentTabName(self):
    ##     return self.tabWidget.currentTabName()
