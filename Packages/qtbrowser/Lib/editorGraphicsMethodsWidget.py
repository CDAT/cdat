from PyQt4 import QtCore, QtGui
import customizeVCDAT
import graphicsMethodsWidgets

class QEditorGraphicsMethodsWidget(QtGui.QWidget):
    def __init__(self,ptype,parent=None):
        QtGui.QWidget.__init__(self,parent)
        self.parent=parent
        self.root=parent.root
        layout = QtGui.QHBoxLayout()

        if ptype == "Boxfill":
            self.widget = graphicsMethodsWidgets.QBoxfillEditor(parent=self,gm = str(parent.gms.gmList.currentItem().text()))
        layout.addWidget(self.widget)
        
        self.setLayout(layout)

