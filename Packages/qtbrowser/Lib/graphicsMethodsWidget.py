from PyQt4 import QtCore, QtGui

class QGraphicsMethodsWidget(QtGui.QWidget):
    def __init__(self,parent=None):
        QtGui.QWidget.__init__(self,parent)
        bg = QtGui.QColor(100,0,0) # red
        p = QtGui.QPalette(bg)
        self.setPalette(p)
        v = QtGui.QVBoxLayout()
        l = QtGui.QLabel()
        self.label = QtGui.QLabel("Graphics Methods")
        self.label.setAutoFillBackground(True)
        self.label.setAlignment(QtCore.Qt.AlignCenter)
        self.label.setFrameStyle(QtGui.QFrame.Panel | QtGui.QFrame.Raised)
        v.addWidget(self.label)
        self.setLayout(v)
        
