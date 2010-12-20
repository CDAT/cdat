from PyQt4 import QtCore, QtGui

class QTemplatesWidget(QtGui.QWidget):
    def __init__(self,parent=None):
        QtGui.QWidget.__init__(self,parent)
        bg = QtGui.QColor(0,100,0) # red
        p = QtGui.QPalette(bg)
        self.setPalette(p)
        v = QtGui.QVBoxLayout()
        v.setMargin(0)
        
        self.setLayout(v)
        self.label = QtGui.QLabel("Templates")
        self.label.setAutoFillBackground(True)
        self.label.setAlignment(QtCore.Qt.AlignCenter)
        self.label.setFrameStyle(QtGui.QFrame.Panel | QtGui.QFrame.Raised)
        v.addWidget(self.label)
        
        
