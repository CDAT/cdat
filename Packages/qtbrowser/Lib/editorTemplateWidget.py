from PyQt4 import QtCore, QtGui
import customizeVCDAT

class QEditorTemplateWidget(QtGui.QWidget):
    def __init__(self,parent=None):
        QtGui.QWidget.__init__(self,parent)
        self.parent=parent
        self.root=parent.root
        layout = QtGui.QHBoxLayout()

        tnm = parent.templates.templateList.currentItem().text()
        self.widget = QTemplateEditorWidget(parent=self,template=str(tnm))
        
        layout.addWidget(self.widget)

        #Set the tab name correctly
        parent.editorTab.setTabText(0,"'%s' Template Properties" % tnm)
        self.setLayout(layout)
        
    def applyChanges(self):
        if self.widget.gm.name == "default":
            return
        try:
            self.widget.applyChanges()
            diff = self.widget.changesString()
            self.root.record(diff)
        except Exception,err:
            st = "Error Applying Changes On %s graphic method: %s\n%s" % (self.ptype,self.widget.gm.name,err)
            self.error.showMessage(st)


    def previewChanges(self):
        self.widget.applyChanges()

    def discardChanges(self):
        self.widget.restoreOriginalValues()
        

class QTemplateEditorWidget(QtGui.QScrollArea):
    def __init__(self, parent=None, template=None):
        QtGui.QScrollArea.__init__(self, parent)
        vbox = QtGui.QVBoxLayout()

        # Create Widgets
        frame = QtGui.QFrame()
        frame.setLayout(vbox)
        self.parent=parent
        self.root=parent.root
        self.templateAttributes = [ ]
        self.template = self.root.canvas[0].gettemplate(template)
        self.saveOriginalValues()

        lbl = QtGui.QLabel("Template editor section")

        vbox.addWidget(lbl)
        self.setWidget(frame)

        
        
    def saveOriginalValues(self):
        self.originalValues={}
        for a in self.templateAttributes:
            if a.find(".")>-1:
                sp=a.split(".")
                self.originalValues[a] = getattr(getattr(self.template,sp[0]),sp[1])
            else:
                self.originalValues[a] = getattr(self.template,a)
            
    def restoreOriginalValues(self):
        for a in self.templateAttributes:
            if a.find(".")>-1:
                sp=a.split(".")
                tmp = getattr(self.template,sp[0])
                setattr(tmp,sp[1],self.originalValues[a])
            else:
                setattr(self.template,a,self.originalValues[a])
        self.initValues()
        
    def getOrCreateTemplateString(self,canvas=0,method = "get", name = None, original=""):
        if original != "":
            original=", '%s'" % original
        if name is None:
            name = self.template.name
        return "template = vcs_canvas[%i].%stemplate('%s'%s)\n" % (canvas,method,name,original)

    def getTemplateString(self,canvas=0):
        return self.getOrCreateTemplateString(canvas)
    
    def changesString(self):
        rec = "## Change Graphics method attributes\n"
        rec += self.getTemplateString()
        for a in self.templateAttributes:
            if self.originalValues[a]!=getattr(self.template,a):
                rec+="template.%s = %s\n" % (a,repr(getattr(self.template,a)))
        return rec
