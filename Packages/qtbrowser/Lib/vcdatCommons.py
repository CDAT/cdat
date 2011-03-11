from PyQt4 import QtCore, QtGui

blackColor = QtGui.QColor(0,0,0)
redColor = QtGui.QColor(255,0,0)
greenColor = QtGui.QColor(0,255,0)
defaultTemplatesColor = QtGui.QColor(108,198,105)
defaultGmsColor = QtGui.QColor(252,104,63)
defaultTemplatesColor = QtGui.QColor(58,79,108)
defaultGmsColor = QtGui.QColor(179,140,138)
plotTypes = ['Boxfill', 'Isofill', 'Isoline', 'Meshfill', 'Outfill',
             'Outline', 'Scatter', 'Taylordiagram', 'Vector', 'XvsY',
             'Xyvsy', 'Yxvsx']
gmInfos= {'Boxfill' :{'nSlabs':1},
          'Isofill' :{'nSlabs':1},
          'Isoline' :{'nSlabs':1},
          'Meshfill' :{'nSlabs':1},
          'Outfill' :{'nSlabs':1},
          'Outline' :{'nSlabs':1},
          'Scatter' : {'nSlabs':2},
          'Taylordiagram' :{'nSlabs':1},
          'Vector' :{'nSlabs':2},
          'XvsY' :{'nSlabs':2},
          'Xyvsy' :{'nSlabs':1},
          'Yxvsx' :{'nSlabs':1},
          }
noMargins = QtCore.QMargins(0,0,0,0)

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
        event.accept()
        self.setText(event.mimeData().text())
        self.emit(QtCore.SIGNAL("droppedInto"),self)
        
class QDragListWidget(QtGui.QListWidget):
    
    def __init__(self,parent=None,type="test",dropTypes=[]):
        QtGui.QListWidget.__init__(self,parent=parent)
        self.type=type
        self.dropTypes=dropTypes

    def dragEnterEvent(self,event):
        ok = False
        for t in self.dropTypes:
            d = event.mimeData().data(t)
            if d.data() != "":
                ok =True
                break
        if ok:
            event.accept()
        else:
            event.ignore()
        
    def dragMoveEvent(self,event):
        event.accept()
        
    def dropEvent(self,event):
        #event.setDropAction(QtCore.Qt.CopyAction)
        event.accept()
        #self.setText(event.mimeData().text())
        self.emit(QtCore.SIGNAL("droppedInto"),self)
        
    def mouseMoveEvent(self,e):
        d =QtGui.QDrag(self)
        m = QtCore.QMimeData()
        a = QtCore.QByteArray()
        a.append(self.currentItem().text())
        m.setData("%s" % self.type,a)
        m.setText(self.currentItem().text())
        d.setMimeData(m)
        d.start()
        
class QLabeledWidgetContainer(QtGui.QWidget):
    """ Container widget for the 3 main widgets: QVariableView, QCDATFileWidget,
    and QDefinedVariable """

    def __init__(self, widget, label='', parent=None,margins=noMargins,labelAlign = QtCore.Qt.AlignCenter,frameStyle = QtGui.QFrame.Panel | QtGui.QFrame.Raised):
        QtGui.QWidget.__init__(self, parent)

        vbox = QtGui.QVBoxLayout()
        #vbox.setMargin(0)
        
        self.label = QtGui.QLabel(label)
        self.label.setAutoFillBackground(False)
        self.label.setAlignment(labelAlign)
        self.label.setFrameStyle(frameStyle)
        vbox.addWidget(self.label)

        if widget!=None:
            self.widget = widget
        else:
            self.widget = QtGui.QWidget()
        vbox.addWidget(self.widget, 1)
        
        self.setLayout(vbox)
        self.setContentsMargins(margins)

    def getWidget(self):
        return self.widget

    ## def event(self, e):
    ##     if e.type()==76: #QtCore.QEvent.LayoutRequest:
    ##         self.setMaximumHeight(min(self.label.height()+self.layout().spacing()+
    ##                                   self.widget.maximumHeight(), 16777215))
    ##     return False

noMargins = QtCore.QMargins(0,0,0,0)
