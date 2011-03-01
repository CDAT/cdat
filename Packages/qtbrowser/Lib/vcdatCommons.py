from PyQt4 import QtCore, QtGui

class QDropLineEdit(QtGui.QLineEdit):
    def __init__(self,parent=None,types=["test",]):
        QtGui.QLineEdit.__init__(self,parent=parent)
        self.types=types

    def dragEnterEvent(self,event):
        ok = False
        for t in self.types:
            d = event.mimeData().data(t)
            if d.data() != "":
                ok =True
                break
        if ok:
            event.accept()
        else:
            event.ignore()
        
    def dropEvent(self,event):
        event.setDropAction(QtCore.Qt.CopyAction)
        event.accept()
        self.setText(event.mimeData().text())
        
class QDragListWidget(QtGui.QListWidget):
    
    def __init__(self,parent=None,type="test"):
        QtGui.QListWidget.__init__(self,parent=parent)
        self.type=type
        

    def mouseMoveEvent(self,e):
        d =QtGui.QDrag(self)
        m = QtCore.QMimeData()
        a = QtCore.QByteArray()
        a.append(self.currentItem().text())
        m.setData("%s" % self.type,a)
        m.setText(self.currentItem().text())
        d.setMimeData(m)
        d.start()
        
