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
indentSpacing = 10

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

        
            
class QRadioButtonFrame(QtGui.QFrame):
    """ Framed widget containing a label and list of radiobuttons """
    def __init__(self, labelText=None, buttonList=None, parent=None):
        QtGui.QFrame.__init__(self, parent)
        self.setFrameStyle(QtGui.QFrame.Raised)
        self.setFrameShape(QtGui.QFrame.Box)

        self.hbox = QtGui.QHBoxLayout()
        self.setLayout(self.hbox)
        self.buttonGroup = QtGui.QButtonGroup()
        self.buttons = {}

        if labelText is not None:
            self.addLabel(QtGui.QLabel(labelText))
        if buttonList is not None:
            self.addButtonList(buttonList)

    def addLabel(self, label):
        self.hbox.addWidget(label)
        self.label = label

    def addButton(self, button):
        self.buttonGroup.addButton(button)
        self.hbox.addWidget(button)
        self.buttons[str(button.text())] = button

    def setChecked(self, buttonText):
        if buttonText in list(self.buttons):
            self.buttons[buttonText].setChecked(True)

    def isChecked(self, buttonText):
        if buttonText in list(self.buttons):
            return self.buttons[buttonText].isChecked()

        return False

    def getButton(self, buttonText):
        if buttonText in list(self.buttons):
            return self.buttons[buttonText]

        return None

    def addButtonList(self, buttonList):
        """ Create and add buttons from a list of strings """
        for buttonText in buttonList:
            button = QtGui.QRadioButton(buttonText)
            self.buttons[buttonText] = button
            self.addButton(button)

class QFramedWidget(QtGui.QFrame):

    def __init__(self, titleText=None, parent=None,margins=noMargins,frameStyle = QtGui.QFrame.Panel | QtGui.QFrame.Raised, frameShape=QtGui.QFrame.Box):
        QtGui.QFrame.__init__(self, parent)    
        self.setFrameStyle(frameStyle)
        self.setFrameShape(frameShape)
        self.setContentsMargins(margins)
        self.lineEditList = []
        self.vbox = QtGui.QVBoxLayout()
        self.setLayout(self.vbox)
        self.indent = 10

        if titleText is not None:
            title = QtGui.QLabel(titleText)
            title.setFont(QtGui.QFont("Times", 14, QtGui.QFont.Bold))
            self.vbox.addWidget(title)

        self.newRow()

    def setSpacing(self, spacing):
        self.vbox.setSpacing(spacing)

    def newRow(self):
        self.hbox = QtGui.QHBoxLayout()
        self.vbox.addLayout(self.hbox)

    def addCheckBox(self, text, indent=True, newRow=True):
        if newRow == True:
            self.newRow()
        if indent == True:
            self.hbox.addSpacing(indentSpacing)

        checkbox = QtGui.QCheckBox(text)
        self.addWidget(checkbox)
        return checkbox

    def addLabeledComboBox(self, text, comboStringList, indent=True, newRow=True):
        if newRow == True:
            self.newRow()
        if indent == True:
            self.hbox.addSpacing(indentSpacing)

        # Init combo box & set text to white on blue
        comboBox = QtGui.QComboBox()
        comboPalette = comboBox.view().palette()
        comboPalette.setColor(QtGui.QPalette.HighlightedText, QtCore.Qt.white)        
        comboPalette.setColor(QtGui.QPalette.Highlight, QtCore.Qt.blue)
        comboBox.view().setPalette(comboPalette)
        for string in comboStringList:
            comboBox.addItem(string)        
            
        comboBox.label = self.addLabel(text)
        self.addWidget(comboBox)
        return comboBox

    def addLabeledSpinBox(self, text, minValue=None, maxValue=None,
                          indent=True, newRow=True):
        if newRow == True:
            self.newRow()
        if indent == True:
            self.hbox.addSpacing(indentSpacing)

        spinbox = QtGui.QSpinBox()
        if maxValue is not None:
            spinbox.setMaximum(maxValue)
        if minValue is not None:
            spinbox.setMinimum(minValue)
        self.addWidget(QtGui.QLabel(text))
        self.addWidget(spinbox)
        
        return spinbox

    def addLabeledDoubleSpinBox(self, text, minValue=None, maxValue=None,
                                step=None, indent=True, newRow=True):
        if newRow == True:
            self.newRow()
        if indent == True:
            self.hbox.addSpacing(indentSpacing)

        spinbox = QtGui.QDoubleSpinBox()
        if maxValue is not None:
            spinbox.setMaximum(maxValue)
        if minValue is not None:
            spinbox.setMinimum(minValue)
        if step is not None:
            spinbox.setSingleStep(step)
            
        self.addWidget(QtGui.QLabel(text))
        self.addWidget(spinbox)
        return spinbox    

    def addButton(self, text, newRow=True):
        if newRow == True:
            self.newRow()
            
        button = QtGui.QToolButton()
        button.setText(text)
        self.addWidget(button)
        return button

    def addRadioFrame(self, text, buttonList, newRow=True):
        if newRow == True:
            self.newRow()
            
        buttonGroup = QRadioButtonFrame(text, buttonList)            
        self.addWidget(buttonGroup)
        return buttonGroup

    def clearAllLineEdits(self):
        for lineEdit in self.lineEditList:
            lineEdit.setText('')

    def addLabeledLineEdit(self, text, indent=True, newRow=True, align=None):
        if newRow == True:
            self.newRow()
        if indent == True:
            self.hbox.addSpacing(indentSpacing)
            
        lineEdit = QtGui.QLineEdit()
        self.lineEditList.append(lineEdit)
        self.addWidget(QtGui.QLabel(text))
        self.addWidget(lineEdit, align)

        if align is not None:
            self.vbox.setAlignment(self.hbox, align)
        
        return lineEdit

    def addLabelAndLineEdit(self, text, indent=True, newRow=True):
        if newRow == True:
            self.newRow()
        if indent == True:
            self.hbox.addSpacing(indentSpacing)

        lineEdit = QtGui.QLineEdit()
        self.lineEditList.append(lineEdit)
        label = QtGui.QLabel(text)
        
        self.addWidget(label)
        self.addWidget(lineEdit)
        return label, lineEdit        

    def addWidget(self, widget, align=None):
        if align is None:
            self.hbox.addWidget(widget)
        else:
            self.hbox.addWidget(widget, alignment=align)

    def addLabel(self, text):
        label = QtGui.QLabel(text)
        self.addWidget(label)
        self.label = label
        return label

    def addSlider(self):
        slider = QtGui.QSlider(QtCore.Qt.Horizontal)
        self.addWidget(slider)
        return slider

    def addLabeledSlider(self, text, newRow=True,divider=1):
        if newRow == True:
            self.newRow()
            
        labeledSlider = QLabeledSlider(text,divider=divider)
        self.addWidget(labeledSlider)
        return labeledSlider.getSlider()
    
class QSimpleMessageBox(QtGui.QMessageBox):

    def __init__(self, text, parent):
        QtGui.QMessageBox.__init__(self, parent)
        self.setText(text)

class QLabeledSlider(QtGui.QWidget):
    def __init__(self, text, parent=None, divider=1):
        QtGui.QWidget.__init__(self, parent)
        vbox = QtGui.QVBoxLayout()
        vbox.setSpacing(10)
        self.setLayout(vbox)
        self.text = text
        self.label = QtGui.QLabel(text)
        self.slider = QtGui.QSlider(QtCore.Qt.Horizontal)
        self.divider=divider
        
        vbox.addWidget(self.label)
        vbox.addWidget(self.slider)

        self.connect(self.slider, QtCore.SIGNAL('valueChanged(int)'),
                     self.updateLabel)

    def getSlider(self):
        return self.slider

    def updateLabel(self, value):
        val = float(value)/self.divider
        val = "%.2f" % val
        if val[-2:]=="00":
            val=val[:-3]
        self.label.setText("%s  %s" %(self.text, val))

def round_number( N ):
   import numpy
   P = 10.0 ** ( numpy.floor(numpy.log10(abs(N) ) ) )
   return( sign(N) * int( abs(N)/P + 0.5) * P )

def sign ( N ):
   if (N < 0): return -1
   else: return 1

        
