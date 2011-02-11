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

        f = QtGui.QFrame()
        hl = QtGui.QHBoxLayout()
        
        self.apply = QtGui.QPushButton("Apply")
        self.preview = QtGui.QPushButton("Preview")
        self.preview.setEnabled(False)
        self.discard = QtGui.QPushButton("Discard")
        self.discard.setEnabled(False)
        hl.addWidget(self.apply)
        hl.addWidget(self.preview)
        hl.addWidget(self.discard)
        f.setLayout(hl)
        layout.addWidget(self.widget)
        layout.addWidget(f)
        
        self.setLayout(layout)

        self.connect(self.apply,QtCore.SIGNAL("clicked()"),self.applyChanges)

    def applyChanges(self):
        self.widget.applyChanges()
        diff = self.widget.changesString()
        self.root.record(diff)
