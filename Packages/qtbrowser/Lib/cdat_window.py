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

