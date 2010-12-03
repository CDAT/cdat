from PyQt4 import QtGui, QtCore

class QMenuWidget(QtGui.QMenuBar):
    def __init__(self, parent=None):
        QtGui.QMenuBar.__init__(self, parent)
        
        self.file = self.addMenu('&File')
        self.pref = self.addMenu('&Preferences')
        self.tools = self.addMenu('&Tools')
        self.pcmdiTools = self.addMenu('&PCMDITools')
        self.help = self.addMenu('&Help')
        self.initToolsMenu()

    def initToolsMenu(self):
        recordTeachingAction = self.tools.addAction('Record Commands')
        recordTeachingAction.setCheckable(True)
        recordTeachingAction.setChecked(True)
        
        viewTeachingAction = self.tools.addAction('View Teaching Commands')
        closeTeachingAction = self.tools.addAction('Close Teaching Commands')        

        self.connect(viewTeachingAction, QtCore.SIGNAL('triggered ()'),
                     self.viewTeachingCommands)
        self.connect(closeTeachingAction, QtCore.SIGNAL('triggered ()'),
                     self.closeTeachingCommands)        
        self.connect(recordTeachingAction, QtCore.SIGNAL('toggled (bool)'),
                     self.setRecordCommands)

    def setRecordCommands(self, checked):
        self.parent().emit(QtCore.SIGNAL('setRecordCommands'), checked)

    def viewTeachingCommands(self):
        self.parent().emit(QtCore.SIGNAL('viewTeachingCommands'))

    def closeTeachingCommands(self):
        self.parent().emit(QtCore.SIGNAL('closeTeachingCommands'))
