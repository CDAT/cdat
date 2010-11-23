from PyQt4 import QtGui, QtCore
import os
import cdms2

class QMainToolBarContainer( QtGui.QWidget ):
    """ Main icon tool bar widget that is located at the top of VCDAT's main
    window. """

    def __init__(self, widget, label='', parent=None):
        QtGui.QWidget.__init__(self, parent)

        vbox = QtGui.QVBoxLayout()
        vbox.setMargin(0)

        ICONPATH = os.path.join(cdms2.__path__[0], '..', '..', '..', '..', 'bin')
        # Create options bar
        self.toolBar = QtGui.QToolBar()
        self.setFixedHeight(50)
        #self.setFixedWidth(50)
        self.toolBar.setIconSize(QtCore.QSize(48, 48))
        actionInfo = [
            ('UVCDATfull.gif', 'Edit (in memory) selected defined variable.'),
            ('UVCDATfull.gif', 'Save selected defined variable to a netCDF file.'),
            ('UVCDATfull.gif', 'Display selected defined variable information.'),
            ('UVCDATfull.gif', 'Move selected defined variable(s) to trashcan for disposal.'),
            ('UVCDATfull.gif', 'Move [ALL] defined variables to trashcan for disposal.'),
            ('UVCDATfull.gif', 'Logged information about the defined variables.'),
            ('UVCDATfull.gif', 'Defined variable items that can be disposed of permanetly or restored.'),
            ]

        for info in actionInfo:
            icon = QtGui.QIcon(os.path.join(ICONPATH, info[0]))
            action = self.toolBar.addAction(icon, '')
            action.setStatusTip(info[1])
            action.setToolTip(info[1])
        self.toolBar.addSeparator()

        self.opButton = QtGui.QToolButton()
        self.opButton.setText('Ops')

        vbox.addWidget(self.toolBar, 0)
        self.setLayout(vbox)

        '''
        self.label = QtGui.QLabel("toolbar")
        self.label.setAutoFillBackground(True)
        self.label.setAlignment(QtCore.Qt.AlignCenter)
        self.label.setFrameStyle(QtGui.QFrame.Panel | QtGui.QFrame.Raised)
        vbox.addWidget(self.label, 0)

        if widget!=None:
            self.widget = widget
        else:
            self.widget = QtGui.QWidget()
        vbox.addWidget(self.widget, 5)
        self.setLayout(vbox)
'''
