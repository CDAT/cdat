from PyQt4 import QtGui, QtCore
import os
import customizeVCDAT

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
            ('ESG_download.gif', 'Connection to the Earth System Grid Federation (ESGF) data archive.',self.ESG,False),
            ('symbol_help.ico', 'Display assistant content for this application.',self.help,False),
            ]

        for info in actionInfo:
            icon = QtGui.QIcon(os.path.join(customizeVCDAT.ICONPATH, info[0]))
            action = self.toolBar.addAction(icon, 'help')
            #action.setStatusTip(info[1])
            action.setToolTip(info[1])
            self.connect(action,QtCore.SIGNAL("triggered()"),info[2])
            action.setEnabled(info[3])
        self.toolBar.addSeparator()

        ## self.opButton = QtGui.QToolButton()
        ## self.opButton.setText('Ops')

        vbox.addWidget(self.toolBar, 0)
        self.setLayout(vbox)


    def openScript(self):
        fnm = QtGui.QFileDialog.getOpenFileName(self,"Open Python Script",filter="Python Scripts (*.py);; All (*.*)")
        print "Filename:",fnm
        #l = QtGui.QVBoxLayout()
        
        pass
    def savePlots(self):
        pass
    def printPlots(self):
        pass
    def ESG(self):
        pass
    def help(self):
        pass
