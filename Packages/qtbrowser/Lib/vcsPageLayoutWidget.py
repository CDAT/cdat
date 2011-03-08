from PyQt4 import QtCore,QtGui
import vcdatCommons
import customizeVCDAT
import os
import qtbrowser


class QPageLayoutWidget(QtGui.QWidget):
    def __init__(self,parent=None):
        QtGui.QWidget.__init__(self,parent)
        self.parent=parent
        self.root=parent.root
        self.nlines=0
        self.removeQIcon = QtGui.QIcon(os.path.join(customizeVCDAT.ICONPATH, 'remove.gif'))
        self.offQIcon = QtGui.QIcon(os.path.join(customizeVCDAT.ICONPATH, 'off.gif'))
        self.onQIcon = QtGui.QIcon(os.path.join(customizeVCDAT.ICONPATH, 'on.gif'))

        vlayout = QtGui.QVBoxLayout()
        vlayout.setMargin(0)
        
        f=QtGui.QFrame()
        f.setContentsMargins(vcdatCommons.noMargins)
        h=QtGui.QHBoxLayout()
        h.setMargin(0)
        f.setLayout(h)
        b = QtGui.QPushButton("Create New Page Layout Line")
        l = QtGui.QLabel("Page Layout Section")
        h.addWidget(l)        
        h.addWidget(b)
        vlayout.addWidget(f)

        s = QtGui.QScrollArea()
        s.setContentsMargins(vcdatCommons.noMargins)
        f =QtGui.QFrame()
        f.setContentsMargins(vcdatCommons.noMargins)
        self.vlayout = QtGui.QVBoxLayout()
        self.vlayout.setMargin(0)
        f.setLayout(self.vlayout)
        vlayout.addWidget(s)
        self.addLine()
        s.setWidget(f)
        s.setWidgetResizable(True)
        self.setLayout(vlayout)
        self.connect(b,QtCore.SIGNAL('clicked()'),self.addLine)


    def removeLine(self,*args):
        for i in range(self.nlines):
            remove =self.vlayout.itemAt(i).widget().layout().itemAt(0).widget()
            if remove.widget.isChecked():
                self.vlayout.itemAt(i).widget().destroy()
                self.vlayout.removeItem(self.vlayout.itemAt(i))
                self.vlayout.update()
                self.nlines-=1
                break
                
    def changedPriority(self,*args):
        for i in range(self.nlines):
            line = self.vlayout.itemAt(i).widget().layout()
            n = line.count()
            
    def onOff(self):
        for i in range(self.nlines):
            line = self.vlayout.itemAt(i).widget().layout()
            n = line.count()
            onoff =line.itemAt(1).widget()
            
            if onoff.widget.isChecked():
                onoff.widget.setIcon(self.onQIcon)
                onoff.label.setText("On")
            else:
                onoff.widget.setIcon(self.offQIcon)
                onoff.label.setText("Off")
        
    def addLine(self):
        f=QtGui.QFrame()
        f.setContentsMargins(vcdatCommons.noMargins)
        h=QtGui.QHBoxLayout()
        h.setMargin(0)
        f.setLayout(h)
        
        tmp = QtGui.QToolButton()
        tmp.setIcon(self.removeQIcon)
        tmp.setCheckable(True)
        tmp.setChecked(False)
        h.addWidget(vcdatCommons.QLabeledWidgetContainer(tmp,"Remove"))
        self.connect(tmp,QtCore.SIGNAL("clicked()"),self.removeLine,self.nlines)
        
        tmp = QtGui.QToolButton()
        tmp.setIcon(self.onQIcon)
        tmp.setCheckable(True)
        tmp.setChecked(True)
        h.addWidget(vcdatCommons.QLabeledWidgetContainer(tmp,"On"))
        self.connect(tmp,QtCore.SIGNAL("clicked()"),self.onOff)
 
        tmp = vcdatCommons.QDropLineEdit(types=["templates",])
        self.connect(tmp,QtCore.SIGNAL("droppedInto"),self.droppedTemplate)
        p = QtGui.QPalette()
        p.setBrush(p.Base,customizeVCDAT.templatesColor)
        tmp.setPalette(p)
        h.addWidget(vcdatCommons.QLabeledWidgetContainer(tmp,"Template"))
        tmp = vcdatCommons.QDropLineEdit(types=["graphicmethods",])
        p = QtGui.QPalette()
        p.setBrush(p.Base,customizeVCDAT.gmsColor)
        tmp.setPalette(p)
        self.connect(tmp,QtCore.SIGNAL("droppedInto"),self.droppedGM)
        h.addWidget(vcdatCommons.QLabeledWidgetContainer(tmp,"GM"))

        tmp = vcdatCommons.QDropLineEdit(types=["definedVariables",])
        self.connect(tmp,QtCore.SIGNAL("droppedInto"),self.droppedVariable)
        h.addWidget(vcdatCommons.QLabeledWidgetContainer(tmp,"Variable"))

        tmp = QtGui.QDoubleSpinBox()
        tmp.setMinimum(0)
        tmp.setSingleStep(1.)
        tmp.setValue(1.)
        self.connect(tmp,QtCore.SIGNAL("valueChanged(double)"),self.changedPriority)
        h.addWidget(vcdatCommons.QLabeledWidgetContainer(tmp,"P."))

        if qtbrowser.useVistrails:
            tmp = QtGui.QComboBox()
            for i in ['Auto',1,2,3,4,5,6,7,8]:
                tmp.addItem(str(i))
            h.addWidget(vcdatCommons.QLabeledWidgetContainer(tmp,"Row"))
            tmp = QtGui.QComboBox()
            for i in ['Auto',1,2,3,4,5,6,7,8]:
                tmp.addItem(str(i))
            h.addWidget(vcdatCommons.QLabeledWidgetContainer(tmp,"Col"))
        else:
            tmp = QtGui.QComboBox()
            for i in [1,2,3,4]:
                tmp.addItem(str(i))
            h.addWidget(vcdatCommons.QLabeledWidgetContainer(tmp,"Canvas"))

        self.nlines+=1
        self.vlayout.addWidget(f)
        self.vlayout.update()

    def droppedTemplate(self,target):
        for i in range(self.nlines):
            tmp = self.vlayout.itemAt(i)
            h=tmp.widget().layout()
            tmpi = h.itemAt(2).widget()
            if tmpi.widget is target:
                h.itemAt(1).widget().widget.setChecked(True)
                self.onOff()
                break

    def droppedGM(self,target):
        gmtype = str(self.parent.plotOptions.plotTypeCombo.currentText())
        for i in range(self.nlines):
            tmp = self.vlayout.itemAt(i)
            hw=tmp.widget()
            h=hw.layout()
            tmpi=h.itemAt(3).widget()
            tmpw=tmpi.widget
            if tmpw is target:
                tmpi.label.setText(gmtype)
                #Ok at that point we need to possibly add variable windows
                nvars = self.parent.nSlabsRequired(gmtype)
                navar = h.count()-6
                if qtbrowser.useVistrails:
                    navar-=1
                if nvars>navar:
                    for i in range(nvars-navar):
                        tmp = vcdatCommons.QDropLineEdit(types=["definedVariables",])
                        self.connect(tmp,QtCore.SIGNAL("droppedInto"),self.droppedVariable)
                        h.insertWidget(5,vcdatCommons.QLabeledWidgetContainer(tmp,"Variable %i"%(i+2)))
                        h.update()
                        self.vlayout.update()
                        for j in range(h.count()):
                            t = h.itemAt(j)
                elif nvars<navar:
                    for i in range(navar-nvars):
                        h.itemAt(5).widget().destroy()
                        h.removeItem(h.itemAt(5))
                h.itemAt(1).widget().widget.setChecked(True)
                self.onOff()
                break

    def droppedVariable(self,target):
        for i in range(self.nlines):
            tmp = self.vlayout.itemAt(i)
            h=tmp.widget().layout()
            for j in range(4,h.count()-1):
                tmpi=h.itemAt(j).widget()
                if tmpi.widget is target:
                    #Ok construct the variable name
                    vsel =  tmpi.widget.text()
                    vnm = " ".join(str(vsel).split()[1:2])
                    vsel = " ".join(str(vsel).split()[1:])
                    tmpi.label.setText(vsel)
                    tmpi.widget.setText(vnm)
                    h.itemAt(1).widget().widget.setChecked(True)
                    self.onOff()
                    return


