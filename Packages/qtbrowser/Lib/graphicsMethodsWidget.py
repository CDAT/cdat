from PyQt4 import QtCore, QtGui
import customizeVCDAT
import editorGraphicsMethodsWidget

class QGraphicsMethodsWidget(QtGui.QWidget):
    def __init__(self,ptype,parent=None):
        QtGui.QWidget.__init__(self,parent)
        self.parent=parent
        self.root=parent.root
        layout = QtGui.QVBoxLayout()        
        self.gmsMenu = QtGui.QMenu(self)
        menuVbox = QtGui.QVBoxLayout()
        menuVbox.setMargin(2)
        self.gmsMenu.setLayout(menuVbox)
        copyAction = self.gmsMenu.addAction('&Copy')
        renameAction = self.gmsMenu.addAction('Re&name')
        saveAsAction = self.gmsMenu.addAction('&Save as a Script file')
        removeAction = self.gmsMenu.addAction('&Remove')

        self.gmButton = QtGui.QPushButton()
        self.gmButton.setText('%s Methods' % ptype)
        self.gmButton.setMenu(self.gmsMenu)

        layout.addWidget(self.gmButton)

        self.gmList = QtGui.QListWidget()
##         self.gmList.setAlternatingRowColors(True)
        self.gmList.setDragEnabled(True)
        p = QtGui.QPalette()
        p.setBrush(p.Base,customizeVCDAT.gmsColor)
        self.gmList.setPalette(p)
        self.gmList.setAutoFillBackground(True)
        gms = self.root.canvas[0].listelements(str(ptype).lower())
        default = 0
        self.gmList.setContextMenuPolicy(QtCore.Qt.CustomContextMenu)
        for i in range(len(gms)):
            g = gms[i]
            self.gmList.addItem(g)
            if g == customizeVCDAT.defaultGraphicsMethodName:
                default = i
        self.gmList.setSortingEnabled(True)
        self.gmList.setCurrentItem(self.gmList.item(default))
        layout.addWidget(self.gmList)


        self.saveFileDialog = QtGui.QFileDialog()
        self.saveFileDialog.hide()
        
        self.inputDialog = QtGui.QInputDialog()
        self.inputDialog.hide()

        self.messageDialog = QtGui.QErrorMessage()
        self.messageDialog.hide()
        
        self.setLayout(layout)

        self.connect(self.gmList,QtCore.SIGNAL("itemSelectionChanged()"),self.changedGM)
        self.connect(copyAction, QtCore.SIGNAL('triggered ()'),
                         self.copyGM)
        self.connect(renameAction, QtCore.SIGNAL('triggered ()'),
                         self.renameGM)
        self.connect(removeAction, QtCore.SIGNAL('triggered ()'),
                         self.removeGM)
        self.connect(saveAsAction, QtCore.SIGNAL('triggered ()'),
                         self.saveAsGM)

        self.connect(self.gmList,QtCore.SIGNAL("customContextMenuRequested(QPoint)"),self.popupmenu)

    def popupmenu(self,Point):
        item = self.gmList.itemAt(Point)
        if item:
            self.gmsMenu.popup(self.gmList.mapToGlobal(Point))
            
    def changedGM(self, *args):
        self.parent.editorTab.removeTab(2)
        self.parent.editorTab.removeTab(1)
        ptype = self.parent.parent.plotOptions.getPlotType()
        self.parent.editorTab.insertTab(1,editorGraphicsMethodsWidget.QEditorGraphicsMethodsWidget(ptype,self.parent),"'%s' %s Graphics Method Properties" % (self.parent.gms.gmList.currentItem().text(),ptype))
        self.parent.editorTab.setCurrentIndex(1)

    def copyGM(self):
        onm = self.parent.editorTab.widget(1).widget.gm.name
        nm,ok = self.inputDialog.getText(self,"Copying A Graphic Method","Enter name for new graphics method copied from '%s'" % onm)
        rec = "## Copy Graphics Method '%s' to '%s'\n" % (onm,nm)
        rec += self.parent.editorTab.widget(1).widget.getOrCreateGMString(method="create",name = str(nm), original=onm)
        try:
            exec(rec.replace("vcs_canvas","self.root.canvas"))
        except Exception,err:
            self.messageDialog.setWindowTitle("Error Copying GM")
            self.messageDialog.showMessage("Error copying GM '%s' to '%s':\n%s\nrec cmommand was:%s" % (onm,nm,str(err),rec.replace("vcs_canvas","self.root.canvas")))
            return
        self.gmList.addItem(nm)
        newItem = self.gmList.findItems(nm,QtCore.Qt.MatchFixedString)[0]
        self.gmList.setCurrentItem(newItem)
        self.root.record(rec)

    def renameGM(self):
        onm = self.parent.editorTab.widget(1).widget.gm.name
        nm,ok = self.inputDialog.getText(self,"Renaming A Graphic Method","Enter new name for graphics method '%s'" % onm)
        rec = "## Rename Graphics Method '%s' to '%s'\n" % (onm,nm)
        rec += self.parent.editorTab.widget(1).widget.getGMString()
        try:
            self.parent.editorTab.widget(1).widget.gm.name = str(nm)
        except Exception,err:
            self.messageDialog.setWindowTitle("Error renaming GM")
            self.messageDialog.showMessage("Error renaming GM '%s' to '%s':\n%s" % (self.parent.editorTab.widget(1).widget.gm.name,nm,str(err)))
            return
        rec+="gm.name = '%s'" % nm
        self.root.record(rec)
        #Removes old gm from list
        oldItem = self.gmList.findItems(onm,QtCore.Qt.MatchFixedString)[0]
        row = self.gmList.row(oldItem)
        oldItem = self.gmList.takeItem(row)
        #Add new name and sets it
        self.gmList.addItem(nm)
        newItem = self.gmList.findItems(nm,QtCore.Qt.MatchFixedString)[0]
        self.gmList.setCurrentItem(newItem)
        

    def saveAsGM(self,*args):
        fnm = self.saveFileDialog.getSaveFileName(self,
                                                  'Save Graphic Method %s' % self.parent.editorTab.widget(1).widget.gm.name,
                                                  self.saveFileDialog.directory().absolutePath(),
                                                  'VCS Scripts files (*.scr);;Python Scripts Files (*.py);;All Files (*.*)')
        if fnm == "":
            return
        
        self.parent.editorTab.widget(1).widget.gm.script(str(fnm))
    def removeGM(self,*args):
        onm = self.parent.editorTab.widget(1).widget.gm.name
        rec = "## Remove Graphics Method '%s'\n" % (onm)
        rec += self.parent.editorTab.widget(1).widget.getGMString()
        rec += "vcs_canvas[0].removeobject(gm)"
        txt = None
        try:
            remove = self.root.canvas[0].removeobject(self.parent.editorTab.widget(1).widget.gm)
            if remove.find("was not removed")>-1:
                txt = "Error removing GM '%s', it is probably in use" % (onm)
        except Exception,err:
            txt = str(err)
        if txt is not None:
            self.messageDialog.setWindowTitle("Error Removing GM")
            self.messageDialog.showMessage(txt)
            return
        self.root.record(rec)
        #Removes old gm from list
        oldItem = self.gmList.findItems(onm,QtCore.Qt.MatchFixedString)[0]
        row = self.gmList.row(oldItem)
        oldItem = self.gmList.takeItem(row)
        newItem = self.gmList.findItems("default",QtCore.Qt.MatchFixedString)[0]
        self.gmList.setCurrentItem(newItem)


