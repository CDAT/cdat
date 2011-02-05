from PyQt4 import QtCore, QtGui
import customizeVCDAT

class QGraphicsMethodsWidget(QtGui.QWidget):
    def __init__(self,ptype,parent=None):
        QtGui.QWidget.__init__(self,parent)
        self.parent=parent
        self.root=parent.root
        layout = QtGui.QVBoxLayout()

        gmsMenu = QtGui.QMenu(self)
        menuVbox = QtGui.QVBoxLayout()
        menuVbox.setMargin(2)
        gmsMenu.setLayout(menuVbox)
        redisplayAction = gmsMenu.addAction('&Redisplay List')
        editAction = gmsMenu.addAction('&Edit')
        copyAction = gmsMenu.addAction('&Copy')
        renameAction = gmsMenu.addAction('Re&name')
        saveAsAction = gmsMenu.addAction('&Save as a Script file')
        removeAction = gmsMenu.addAction('&Remove')

        self.gmButton = QtGui.QPushButton()
        self.gmButton.setText('%s Methods' % ptype)
        self.gmButton.setMenu(gmsMenu)

        layout.addWidget(self.gmButton)

        self.gmList = QtGui.QListWidget()
##         self.gmList.setAlternatingRowColors(True)
        self.gmList.setDragEnabled(True)
        p = QtGui.QPalette()
        p.setBrush(p.Base,customizeVCDAT.gmsColor)
        self.gmList.setPalette(p)
        self.gmList.setAutoFillBackground(True)
        gms = parent.canvas[0].listelements(str(ptype).lower())
        default = 0
        for i in range(len(gms)):
            g = gms[i]
            self.gmList.addItem(g)
            if g == customizeVCDAT.defaultGraphicsMethodName:
                default = i
        self.gmList.setCurrentItem(self.gmList.item(default))
        layout.addWidget(self.gmList)
        self.setLayout(layout)

