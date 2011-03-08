from PyQt4 import QtCore, QtGui
import customizeVCDAT
import editorTemplateWidget
import vcdatCommons

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
        copyAction = templatesMenu.addAction('&Copy')
        renameAction = templatesMenu.addAction('Re&name')
        saveAsAction = templatesMenu.addAction('&Save as a Script file')
        removeAction = templatesMenu.addAction('&Remove')

        self.templatesMenu = templatesMenu
        self.templateButton = QtGui.QLabel()
        self.templateButton.setText('Templates')
        #self.templateButton.setMenu(templatesMenu)

        layout.addWidget(self.templateButton)

        self.templateList = vcdatCommons.QDragListWidget(type="templates")
##         self.templateList.setAlternatingRowColors(True)
        self.templateList.setDragEnabled(True)
        p = QtGui.QPalette()
        p.setBrush(p.Base,customizeVCDAT.templatesColor)
        self.templateList.setPalette(p)
        self.templateList.setAutoFillBackground(True)
        tpl = self.root.canvas[0].listelements("template")
        default = 0 
        self.templateList.setContextMenuPolicy(QtCore.Qt.CustomContextMenu)
        for i in range(len(tpl)):
            t = tpl[i]
            self.templateList.addItem(t)
            if t == customizeVCDAT.defaultTemplateName:
                default = i
        self.templateList.setSortingEnabled(True)
        self.templateList.setCurrentItem(self.templateList.item(default))
        layout.addWidget(self.templateList)
        self.setLayout(layout)
        self.saveFileDialog = QtGui.QFileDialog()
        self.saveFileDialog.hide()
        
        self.inputDialog = QtGui.QInputDialog()
        self.inputDialog.hide()

        self.messageDialog = QtGui.QErrorMessage()
        self.messageDialog.hide()
        
        self.setLayout(layout)

        self.connect(self.templateList,QtCore.SIGNAL("itemSelectionChanged()"),self.changedTemplate)
        self.connect(copyAction, QtCore.SIGNAL('triggered ()'),
                         self.copyTemplate)
        self.connect(renameAction, QtCore.SIGNAL('triggered ()'),
                         self.renameTemplate)
        self.connect(removeAction, QtCore.SIGNAL('triggered ()'),
                         self.removeTemplate)
        self.connect(saveAsAction, QtCore.SIGNAL('triggered ()'),
                         self.saveAsTemplate)

        self.connect(self.templateList,QtCore.SIGNAL("customContextMenuRequested(QPoint)"),self.popupmenu)
        
    def popupmenu(self,Point):
        item = self.templateList.itemAt(Point)
        if item:
            self.templatesMenu.popup(self.templateList.mapToGlobal(Point))
            
    def changedTemplate(self, *args):
        self.parent.editorTab.insertTab(0,editorTemplateWidget.QEditorTemplateWidget(self.parent),"'%s' Template Properties" % (self.parent.templates.templateList.currentItem().text()))
        self.parent.editorTab.removeTab(1)
        self.parent.editorTab.setCurrentIndex(0)

    def copyTemplate(self):
        onm = self.parent.editorTab.widget(0).widget.template.name
        nm,ok = self.inputDialog.getText(self,"Copying A Template","Enter name for new template copied from '%s'" % onm)
        rec = "## Copy Template '%s' to '%s'\n" % (onm,nm)
        rec += self.parent.editorTab.widget(0).widget.getOrCreateTemplateString(method="create",name = str(nm), original=onm)
        try:
            exec(rec.replace("vcs_canvas","self.root.canvas"))
        except Exception,err:
            self.messageDialog.setWindowTitle("Error Copying Template")
            self.messageDialog.showMessage("Error copying Template '%s' to '%s':\n%s\nrec cmommand was:%s" % (onm,nm,str(err),rec.replace("vcs_canvas","self.root.canvas")))
            return
        self.templateList.addItem(nm)
        newItem = self.templateList.findItems(nm,QtCore.Qt.MatchFixedString)[0]
        self.templateList.setCurrentItem(newItem)
        self.root.record(rec)

    def renameTemplate(self):
        onm = self.parent.editorTab.widget(0).widget.template.name
        nm,ok = self.inputDialog.getText(self,"Renaming A Graphic Method","Enter new name for graphics method '%s'" % onm)
        rec = "## Rename Graphics Method '%s' to '%s'\n" % (onm,nm)
        rec += self.parent.editorTab.widget(0).widget.getTemplateString()
        try:
            self.parent.editorTab.widget(0).widget.template.name = str(nm)
        except Exception,err:
            self.messageDialog.setWindowTitle("Error renaming Template")
            self.messageDialog.showMessage("Error renaming Template '%s' to '%s':\n%s" % (self.parent.editorTab.widget(0).widget.template.name,nm,str(err)))
            return
        rec+="template.name = '%s'" % nm
        self.root.record(rec)
        #Removes old template from list
        oldItem = self.templateList.findItems(onm,QtCore.Qt.MatchFixedString)[0]
        row = self.templateList.row(oldItem)
        oldItem = self.templateList.takeItem(row)
        #Add new name and sets it
        self.templateList.addItem(nm)
        newItem = self.templateList.findItems(nm,QtCore.Qt.MatchFixedString)[0]
        self.templateList.setCurrentItem(newItem)
        

    def saveAsTemplate(self,*args):
        fnm = self.saveFileDialog.getSaveFileName(self,
                                                  'Save Template %s' % self.parent.editorTab.widget(0).widget.template.name,
                                                  self.saveFileDialog.directory().absolutePath(),
                                                  'VCS Scripts files (*.scr);;Python Scripts Files (*.py);;All Files (*.*)')
        if fnm == "":
            return
        
        self.parent.editorTab.widget(0).widget.template.script(str(fnm))
    def removeTemplate(self,*args):
        onm = self.parent.editorTab.widget(0).widget.template.name
        rec = "## Remove Graphics Method '%s'\n" % (onm)
        rec += self.parent.editorTab.widget(0).widget.getTemplateString()
        rec += "vcs_canvas[0].removeobject(template)"
        txt = None
        try:
            remove = self.root.canvas[0].removeobject(self.parent.editorTab.widget(0).widget.template)
            if remove.find("was not removed")>-1:
                txt = "Error removing Template '%s', it is probably in use" % (onm)
        except Exception,err:
            txt = str(err)
        if txt is not None:
            self.messageDialog.setWindowTitle("Error Removing Template")
            self.messageDialog.showMessage(txt)
            return
        self.root.record(rec)
        #Removes old template from list
        oldItem = self.templateList.findItems(onm,QtCore.Qt.MatchFixedString)[0]
        row = self.templateList.row(oldItem)
        oldItem = self.templateList.takeItem(row)
        newItem = self.templateList.findItems("default",QtCore.Qt.MatchFixedString)[0]
        self.templateList.setCurrentItem(newItem)

