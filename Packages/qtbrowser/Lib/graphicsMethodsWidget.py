from PyQt4 import QtCore, QtGui
import customizeVCDAT
import editorGraphicsMethodsWidget

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
        gms = parent.parent.canvas[0].listelements(str(ptype).lower())
        default = 0
        for i in range(len(gms)):
            g = gms[i]
            self.gmList.addItem(g)
            if g == customizeVCDAT.defaultGraphicsMethodName:
                default = i
        self.gmList.setCurrentItem(self.gmList.item(default))
        layout.addWidget(self.gmList)
        self.setLayout(layout)

        self.connect(self.gmList,QtCore.SIGNAL("itemSelectionChanged()"),self.changedGM)


    def changedGM(self, *args):
        self.parent.editorTab.removeTab(1)
        ptype = self.parent.parent.plotOptions.getPlotType()
        self.parent.editorTab.addTab(editorGraphicsMethodsWidget.QEditorGraphicsMethodsWidget(ptype,self.parent),"'%s' %s Graphics Method Properties" % (self.parent.gms.gmList.currentItem().text(),ptype))
        self.parent.editorTab.setCurrentIndex(1)
