from PyQt4 import QtCore,QtGui
import vcdatCommons
        
class QPageLayoutWidget(QtGui.QWidget):
    def __init__(self,parent=None):
        QtGui.QWidget.__init__(self,parent)
        self.parent=parent
        self.root=parent.root
        self.grid = QtGui.QGridLayout()
        self.nlines=1
        l = QtGui.QLabel("Here is the Page Layout Section")
        self.grid.addWidget(l,0,0)
        self.addLine()
        self.setLayout(self.grid)
        

    def addLine(self):
        self.grid.addWidget(vcdatCommons.QDropLineEdit(types=["templates",]),self.nlines,0)
        self.grid.addWidget(vcdatCommons.QDropLineEdit(types=["graphicmethods",]),self.nlines,1)
        self.nlines+=1
        
