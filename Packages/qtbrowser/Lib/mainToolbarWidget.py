from PyQt4 import QtGui, QtCore
import os
import customizeVCDAT
import vcdatCommons

class QMainToolBarContainer( QtGui.QWidget ):
    """ Main icon tool bar widget that is located at the top of VCDAT's main
    window. """

    def __init__(self, parent=None):
        QtGui.QWidget.__init__(self, parent)
        self.root=parent.root
        vbox = QtGui.QVBoxLayout()
        vbox.setMargin(0)

        # Create options bar
        self.toolBar = QtGui.QToolBar()
        self.setFixedHeight(50)
        #self.setFixedWidth(50)
        self.toolBar.setIconSize(QtCore.QSize(customizeVCDAT.iconsize, customizeVCDAT.iconsize))
        actionInfo = [
            ('script_folder_smooth.ico', 'Open a script file.',self.openScript,True),
            ('folder_image_blue.ico', 'Save plots.',self.savePlots,True),
            ('printer.ico', 'Print plots.',self.printPlots,True),
            ('vistrails_icon.png', 'Vistrails Builder.',self.vistrails,True),
            ('connected.ico', 'Connection to the Earth System Grid Federation (ESGF) data archive.',self.ESG,False),
            ('symbol_help.ico', 'Display assistant content for this application.',self.help,False),
            ]

        for info in actionInfo:
            pth = os.path.join(customizeVCDAT.ICONPATH, info[0])
            icon = QtGui.QIcon(pth)
            action = self.toolBar.addAction(icon, 'help')
            action.setToolTip(info[1])
            self.connect(action,QtCore.SIGNAL("triggered()"),info[2])
            action.setEnabled(info[3])
        self.toolBar.addSeparator()


        vbox.addWidget(self.toolBar, 0)
        self.setLayout(vbox)
        self.files=[]


        ## Printers dialog
        osprinters = os.environ.get("PRINTER",None)
        if osprinters is None:
            printers=[]
        else:
            printers=[osprinters,]
        vcsprinters =  sorted(vcdatCommons.getAvailablePrinters())
        if osprinters is not None and osprinters in vcsprinters:
            vcsprinters.pop(vcsprinters.index(osprinters))
        printers+=vcsprinters
        printers += ["Custom",]
        
        d = QtGui.QDialog()
        l = QtGui.QVBoxLayout()
        d.setLayout(l)
        fp = vcdatCommons.QFramedWidget("Printer Selection",parent=self)
        self.qprinters = fp.addLabeledComboBox("Printer",printers)
        self.connect(self.qprinters,QtCore.SIGNAL("currentIndexChanged(int)"),self.changedPrinter)
        l.addWidget(fp)
        self.customPrinter=fp.addLabeledLineEdit("Custom Printer Name:",newRow=True)
        f = QtGui.QFrame()
        h = QtGui.QHBoxLayout()
        b1 = QtGui.QPushButton("Print")
        h.addWidget(b1)
        b2 = QtGui.QPushButton("Cancel")
        h.addWidget(b2)
        self.connect(b1,QtCore.SIGNAL("clicked()"),self.printerSelected)
        self.connect(b2,QtCore.SIGNAL("clicked()"),d.hide)
        f.setLayout(h)
        l.addWidget(f)
        self.qprinters.setCurrentIndex(1)
        self.qprinters.setCurrentIndex(0)

        self.printers = d
        d.hide()


    def openScript(self):
        fnm = str(QtGui.QFileDialog.getOpenFileName(self,"Open Python Script",filter="Python Scripts (*.py);; All (*.*)"))
        d = vcdatCommons.QCommandsFileWidget(self,fnm,False,fnm)
        d.show()
        self.files.append(d)
        
    
    def savePlots(self):
        for x in self.root.canvas:
            if x.iscanvasdisplayed():
                fnm = str(QtGui.QFileDialog.getOpenFileName(self,"Save Plot",filter="VCS Images (*.png *.svg *.gif *.pdf *.ps *.eps);; All (*.*)"))
                if fnm.lower()[-3:] == ".ps":
                    x.postscript(fnm)
                elif fnm.lower()[:-4] == ".eps":
                    x.eps(fnm)
                elif fnm.lower()[:4]==".png":
                    x.png(fnm)
                elif fnm.lower()[:4]==".pdf":
                    x.pdf(fnm)
                elif fnm.lower()[:4]==".gif":
                    x.gif(fnm)
                elif fnm.lower()[:4]==".svg":
                    x.svg(fnm)
                else:
                    x.png(fnm)
  
    def printPlots(self):
        self.printers.show()
        pass
    def printerSelected(self):
        self.printers.hide()
        printer = str(self.qprinters.currentText())
        if printer == "Custom":
            printer = str(self.customPrinter.text()).strip()
        if printer=="":
            return
        for x in self.root.canvas:
            if x.iscanvasdisplayed():
                x.printer(printer)
                
    def changedPrinter(self,*args):
        if str(self.qprinters.currentText()) == "Custom":
            self.customPrinter.setEnabled(True)
            self.customPrinter.label.setEnabled(True)
        else:
            self.customPrinter.setEnabled(False)            
            self.customPrinter.label.setEnabled(False)            
        
    def ESG(self):
        return
    def help(self):
        return
    def vistrails(self):
        return
    
