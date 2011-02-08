from PyQt4 import QtCore, QtGui
import customizeVCDAT

class QEditorTemplateWidget(QtGui.QWidget):
    def __init__(self,parent=None):
        QtGui.QWidget.__init__(self,parent)
        self.parent=parent
        self.root=parent.root
        layout = QtGui.QHBoxLayout()

        tnm = parent.templates.templateList.currentItem().text()
        print 'Template name is:',tnm
        self.widget = QtGui.QLabel("Template editor section")
        
        layout.addWidget(self.widget)

        #Set the tab name correctly
        parent.editorTab.setTabText(0,"'%s' Template Properties" % tnm)
        self.setLayout(layout)

