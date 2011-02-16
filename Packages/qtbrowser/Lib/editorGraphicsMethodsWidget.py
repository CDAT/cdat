from PyQt4 import QtCore, QtGui
import customizeVCDAT
import graphicsMethodsWidgets

class QEditorGraphicsMethodsWidget(QtGui.QWidget):
    def __init__(self,ptype,parent=None):
        QtGui.QWidget.__init__(self,parent)
        self.parent=parent
        self.root=parent.root
        layout = QtGui.QVBoxLayout()

        gmnm = str(parent.gms.gmList.currentItem().text())
        if ptype == "Boxfill":
            self.widget = graphicsMethodsWidgets.QBoxfillEditor(parent=self,gm = gmnm)
        if gmnm == "default":
            self.widget.widget().setEnabled(False)
            parent.editorTab.widget(1).widget().setEnabled(False)

        layout.addWidget(self.widget)
        
        self.setLayout(layout)


    def applyChanges(self):
        self.widget.applyChanges()
        diff = self.widget.changesString()
        self.root.record(diff)

    def previewChanges(self):
        self.widget.applyChanges()

    def discardChanges(self):
        self.widget.restoreOriginalValues()
        
