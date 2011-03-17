from PyQt4 import QtCore,QtGui
import vcdatCommons
import cdms2


class preFuncPopUp(QtGui.QDialog):
    def __init__(self,parent=None,defs={},axes=True,multiAxes=True):
        QtGui.QDialog.__init__(self,parent=parent)
        self.defs = defs
        self.parent=parent
        self.root = parent.root
        l = QtGui.QVBoxLayout()
        self.setLayout(l)

        self.axes = axes
        self.multiAxes = multiAxes

        module = defs["func"].__module__
        if module == "genutil.averager":
            nm = module
        else:
            nm = "%s.%s" % (module,defs["func"].__name__)
        self.setWindowTitle(nm)

         ## First a few error checks
        selectedVars=self.root.definedVar.widget.getSelectedDefinedVariables()
        if len(selectedVars)<defs["nargsMin"]:
            parent.errorMsg.showMessage("%s requires at least %i input variables" % (defs["func"].__name__,defs["nargsMin"]))
            self.accept()
            return
        if len(selectedVars)>defs["nargsMax"]:
            parent.errorMsg.showMessage("%s requires at most %i input variables" % (defs["func"].__name__,defs["nargsMax"]))
            self.accept()
            return
        
        lb = QtGui.QLabel("Options for: %s" % nm)
        l.addWidget(lb)
        lb = QtGui.QLabel("Documentation:")
        l.addWidget(lb)
        te = QtGui.QTextEdit()
        txt = defs["func"].__doc__
        te.setPlainText(txt)
        te.setReadOnly(True)
        f = te.currentFont()
        fm = QtGui.QFontMetrics(f)
        maxWidth = max(map(fm.width,txt.split("\n")))
        self.parent.setMinimumWidth(maxWidth)
        self.setMinimumWidth(maxWidth)
        l.addWidget(te)

        ## Show the user what he's using
        lb = QtGui.QLabel("Variables selected:")
        l.addWidget(lb)
        te = QtGui.QTextEdit()
        te.setDocumentTitle("BLA")
        te.setMaximumHeight(fm.height()*(len(selectedVars)+1))
        txt=""
        for v in selectedVars:
            txt+="%s %s\n" % (v.id, str(v.shape))
        te.setPlainText(txt[:-1])
        te.setReadOnly(True)
        l.addWidget(te)

        ## section showing the axes
        if axes:
            axFrame = vcdatCommons.QFramedWidget("Axes Options:")
            if multiAxes:
                self.selAxes=[]
                for ax in selectedVars[0].getAxisList():
                    c = axFrame.addCheckBox("%s" % ax.id,newRow=True)
                    self.selAxes.append(c)
                    if ax == selectedVars[0].getAxis(0):
                        c.setCheckState(QtCore.Qt.Checked)
            else:
                self.selAxes = axFrame.addLabeledComboBox("Axis:",selectedVars[0].getAxisListIds())

            l.addWidget(axFrame)
        ## Ok now the "choices"
        self.choices=[]
        choices = vcdatCommons.QFramedWidget("Options:")
        hasChoices = False
        for c in sorted(defs.get("choices",[])):
            hasChoices = True
            if isinstance(c,str):
                ## Ok it's a simple yes/no
                self.choices.append(choices.addCheckBox(c,newRow=True))
            else:
                ## Ok it's a radio button or a combobox if too many
                vals = c[1]
                if len in vals:
                    vals.pop(vals.index(len))
                    vals+=range(max(selectedVars[0].shape))
                vals2=[]
                for v in vals:
                    vals2.append(repr(v))
                vals=vals2
                if len(vals)>5:
                    self.choices.append(choices.addLabeledComboBox(c[0],vals,newRow=True))
                else:
                    self.choices.append(choices.addRadioFrame(c[0],vals,newRow=True))
        if hasChoices:
            l.addWidget(choices)
        ##Ok and Finally the ok/cancel buttons
        valid = QtGui.QFrame()
        hl = QtGui.QHBoxLayout()
        ok = QtGui.QPushButton("Apply")
        cancel = QtGui.QPushButton("Cancel")
        hl.addWidget(ok)
        hl.addWidget(cancel)
        valid.setLayout(hl)
        l.addWidget(valid)
        
        self.connect(ok,QtCore.SIGNAL("clicked()"),self.ok)
        self.connect(cancel,QtCore.SIGNAL("clicked()"),self.accept)

    def ok(self):

        ## First construct the args list
        ## starting with the sleected vars
        args = self.root.definedVar.widget.getSelectedDefinedVariables()

        kargs = {}
        ## now do we have an axis option
        if self.axes:
            ## single axis or multi?
            if self.multiAxes:
                nms = ""
                for c in self.selAxes:
                    if c.isChecked():
                        nms+="(%s)" % str(c.text())
            else:
                nms = "(%s)" % str(self.selAxes.buttonGroup.checkedButton().text())
            if nms == "":
                self.parent.errorMsg.showMessage("%s requires at least one axis to be checked" % (self.defs["func"].__name__))
                return
            kargs["axis"]=nms

        ## Now the Options
        for c in self.choices:
            if isinstance(c,vcdatCommons.QRadioButtonFrame):
                val = eval(str(c.buttonGroup.checkedButton().text()))
                nm = str(c.label.text())
            elif isinstance(c,QtGui.QComboBox):
                val = eval(str(c.itemText(c.currentIndex())))
                nm = str(c.label.text())
            elif isinstance(c,QtGui.QCheckBox):
                nm = str(c.text())
                if c.isChecked():
                    val = 1
                else:
                    val = 0
            kargs[nm]=val

        fnm = str(self.windowTitle())
        c = self.cursor()
        self.setCursor(QtCore.Qt.BusyCursor)
        try:
            tmp  = self.defs["func"](*args,**kargs)
        except Exception, err:
            self.parent.errorMsg.showMessage("The following exception was raised while runnning %S:\n%s" % (self.defs["func"].__name__,str(err)))
            self.setCursor(c)
            self.accept()
            return
        self.setCursor(c)

        if isinstance(tmp,cdms2.tvariable.TransientVariable):
            tmp.id="%s.%s" % (fnm,tmp.id)
            self.root.definedVar.widget.addVariable(tmp)
            res = ", %s" % tmp.id
        else:
            res = self.processList(tmp,"%s.%s" % (args[0].id,fnm),"")
        self.root.record("## %s" % fnm)
        cargs=args[0].id
        for a in args[1:]:
            cargs+=", %s" % a.id
        ckargs = ""
        for k in kargs.keys():
            ckargs+=", %s=%s" % (k,repr(kargs[k]))
        cmd="%s = %s(%s%s)" % (res[2:],fnm,cargs,ckargs)
        self.root.record(cmd)
        self.accept()

    def processList(self,myList,fnm,res):
        for v in myList:
            if isinstance(v,cdms2.tvariable.TransientVariable):
                v.id="%s.%s" % (v.id,fnm)
                res+=", %s" %v.id
                self.root.definedVar.widget.addVariable(v)
            elif isinstance(v,(list,tuple)):
                res+=", (%s)" % self.processList(v,fnm,"")[2:]
            else:
                res+=", nonvar"
        return res

