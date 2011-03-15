from PyQt4 import QtGui, QtCore
import qtbrowser

import os
import customizeVCDAT

class QCommandsRecorderWidget(QtGui.QDialog):
    def __init__(self,parent=None):
        QtGui.QDialog.__init__(self,parent)
        self.root=parent.root
        layout = QtGui.QVBoxLayout()
        layout.setMargin(2)
        self.setLayout(layout)
        self.toolBar = QtGui.QToolBar()
        self.toolBar.setIconSize(QtCore.QSize(customizeVCDAT.iconsize, customizeVCDAT.iconsize))
        icon = QtGui.QIcon(os.path.join(customizeVCDAT.ICONPATH, 'Save.gif'))
        action = self.toolBar.addAction(icon, 'saveas',self.saveAs)
        #action.setStatusTip(info[1])
        action.setToolTip('Save to a new file.')
        self.toolBar.addSeparator()
        layout.addWidget(self.toolBar)

        self.text = QtGui.QTextEdit(self)
        self.text.setReadOnly(True)
        self.text.ensureCursorVisible()
        
        layout.addWidget(self.text)

        self.setWindowTitle("VCDAT Recorded Commands")

        self.setMinimumWidth(600)
        self.initCommands()

    def initCommands(self):
        
        txt = """## This file records all user commands
## First a few necessary imports
import vcs,cdms2,MV2,genutil,cdutil
vcs_canvas=[]
for i in range(4):
   vcs_canvas.append(vcs.init())

## And now whatever the user decides to do...
## Thanks for using VCDAT have fun
"""
   
        self.addText(txt)
        
    def saveAs(self):
        cont = True
        while cont is True:
            fnm = QtGui.QFileDialog.getSaveFileName()
            print 'Got file:',fnm
            try:
                f=open(fnm,'w')
                cont=False
            except:
                if fnm is None:
                    cont=False
                    return
        

        print >> f, self.text.toPlainText()
        f.close()
        
    def addText(self,textString):
        
        sp = textString.split("\n")
        for l in sp:
            st=l.strip()
            if len(st)>0 and st[0]=="#": # comment
                self.text.setTextColor( customizeVCDAT.commentsColor )
            else:
                self.text.setTextColor( customizeVCDAT.defaultTextColor )
            
        
            self.text.insertPlainText(l+'\n')
        

    
    def record(self,commands,*vistrails):
        for a in self.root.mainMenu.tools.actions():
            if a.text() == 'Record Commands' and a.isChecked():
                self.addText(commands)
                break
        if qtbrowser.useVistrails:
	    self.emit(QtCore.SIGNAL("recordCommands"),commands)

