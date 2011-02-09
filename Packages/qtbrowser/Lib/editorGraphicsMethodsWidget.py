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
            print 'turnong off widget'
            self.widget.widget().setEnabled(False)

        f = QtGui.QFrame()
        hl = QtGui.QHBoxLayout()
        
        self.apply = QtGui.QPushButton("Apply")
        self.preview = QtGui.QPushButton("Preview")
        self.discard = QtGui.QPushButton("Discard")
        hl.addWidget(self.apply)
        hl.addWidget(self.preview)
        hl.addWidget(self.discard)
        f.setLayout(hl)
        layout.addWidget(self.widget)
        layout.addWidget(f)
        
        self.setLayout(layout)

