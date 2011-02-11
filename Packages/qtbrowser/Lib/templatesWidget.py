from PyQt4 import QtCore, QtGui
import customizeVCDAT

class QTemplatesWidget(QtGui.QWidget):
    def __init__(self,parent=None):
        QtGui.QWidget.__init__(self,parent)
        self.parent=parent
        self.root=parent.root
        layout = QtGui.QVBoxLayout()

        templatesMenu = QtGui.QMenu(self)
        menuVbox = QtGui.QVBoxLayout()
        menuVbox.setMargin(2)
        templatesMenu.setLayout(menuVbox)
        redisplayAction = templatesMenu.addAction('&Redisplay List')
        editAction = templatesMenu.addAction('&Edit')
        copyAction = templatesMenu.addAction('&Copy')
        renameAction = templatesMenu.addAction('Re&name')
        saveAsAction = templatesMenu.addAction('&Save as a Script file')
        removeAction = templatesMenu.addAction('&Remove')

        self.templateButton = QtGui.QPushButton()
        self.templateButton.setText('Templates')
        self.templateButton.setMenu(templatesMenu)

        layout.addWidget(self.templateButton)

        self.templateList = QtGui.QListWidget()
##         self.templateList.setAlternatingRowColors(True)
        self.templateList.setDragEnabled(True)
        p = QtGui.QPalette()
        p.setBrush(p.Base,customizeVCDAT.templatesColor)
        self.templateList.setPalette(p)
        self.templateList.setAutoFillBackground(True)
        tpl = parent.parent.canvas[0].listelements("template")
        default = 0 
        for i in range(len(tpl)):
            t = tpl[i]
            self.templateList.addItem(t)
            if t == customizeVCDAT.defaultTemplateName:
                default = i
        self.templateList.setCurrentItem(self.templateList.item(default))
        layout.addWidget(self.templateList)
        self.setLayout(layout)
