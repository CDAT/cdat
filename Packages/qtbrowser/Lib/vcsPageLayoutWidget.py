from PyQt4 import QtCore,QtGui
import vcdatCommons
import customizeVCDAT
     
class QPageLayoutWidget(QtGui.QWidget):
    def __init__(self,parent=None):
        QtGui.QWidget.__init__(self,parent)
        self.parent=parent
        self.root=parent.root
        self.grid = QtGui.QGridLayout()
        self.nlines=1
        b = QtGui.QPushButton("Page Layout Section")
        self.l = QtGui.QLabel("Page Layout Section")
        self.menu = QtGui.QMenu()
        add = self.menu.addAction("Create New Form")
        b.setMenu(self.menu)
        self.l.setContextMenuPolicy(QtCore.Qt.CustomContextMenu)
        self.grid.addWidget(b,0,0)
        self.grid.addWidget(self.l,0,1)
        self.addLine()
        self.setLayout(self.grid)

        self.connect(add,QtCore.SIGNAL('triggered ()'),self.addLine)
        self.connect(self.l,QtCore.SIGNAL("customContextMenuRequested(QPoint)"),self.popupmenu)


    def popupmenu(self,Point):
        self.menu.popup(self.l.mapToGlobal(Point))
    def addLine(self):
        tmp = vcdatCommons.QDropLineEdit(types=["templates",])
        p = QtGui.QPalette()
        p.setBrush(p.Base,customizeVCDAT.templatesColor)
        tmp.setPalette(p)
        self.grid.addWidget(tmp,self.nlines,0)
        tmp = vcdatCommons.QDropLineEdit(types=["graphicmethods",])
        p = QtGui.QPalette()
        p.setBrush(p.Base,customizeVCDAT.gmsColor)
        tmp.setPalette(p)
        self.grid.addWidget(tmp,self.nlines,1)
        self.nlines+=1
        
