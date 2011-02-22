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
            ('Open_folder.gif', 'Open a script file.'),
            ('Save.gif', 'Save selected defined variable to a netCDF file.'),
            ('Print.gif', 'Print selected defined variable information or selected plot.'),
            ('Script.gif', 'Script out the button clicks and commands to a file.'),
            ('ESG_download.gif', 'Connection to the Earth System Grid Federation (ESGF) data archive.'),
            ('Help.gif', 'Display assistant content for this application.'),
            ]

        for info in actionInfo:
            icon = QtGui.QIcon(os.path.join(customizeVCDAT.ICONPATH, info[0]))
            action = self.toolBar.addAction(icon, 'help')
            #action.setStatusTip(info[1])
            action.setToolTip(info[1])
        self.toolBar.addSeparator()

        self.opButton = QtGui.QToolButton()
        self.opButton.setText('Ops')

        vbox.addWidget(self.toolBar, 0)
        self.setLayout(vbox)

##         '''
##         self.label = QtGui.QLabel("toolbar")
##         self.label.setAutoFillBackground(True)
##         self.label.setAlignment(QtCore.Qt.AlignCenter)
##         self.label.setFrameStyle(QtGui.QFrame.Panel | QtGui.QFrame.Raised)
##         vbox.addWidget(self.label, 0)

##         if widget!=None:
##             self.widget = widget
##         else:
##             self.widget = QtGui.QWidget()
##         vbox.addWidget(self.widget, 5)
##         self.setLayout(vbox)
## '''
