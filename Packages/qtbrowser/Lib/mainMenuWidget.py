from PyQt4 import QtGui, QtCore
import os
import commandsRecorderWidget
import customizeVCDAT

class QMenuWidget(QtGui.QMenuBar):
    def __init__(self, parent=None):
        QtGui.QMenuBar.__init__(self, parent)
        
        self.file = self.addMenu('&File')
        self.pref = self.addMenu('&Preferences')
        self.tools = self.addMenu('&Tools')
        self.pcmdiTools = self.addMenu('&PCMDITools')
        self.help = self.addMenu('&Help')
        self.root=parent.root
        
        recordTeachingAction = self.tools.addAction('Record Commands')
        recordTeachingAction.setCheckable(True)
        recordTeachingAction.setChecked(customizeVCDAT.recordCommands)
        
        viewTeachingAction = self.tools.addAction('View Teaching Commands')

        self.connect(viewTeachingAction, QtCore.SIGNAL('triggered ()'),
                     self.root.recorder.show)
        ## self.connect(closeTeachingAction, QtCore.SIGNAL('triggered ()'),
        ##              self.closeTeachingCommands)        

