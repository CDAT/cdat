from PyQt4 import QtGui, QtCore
import vcs
import qtbrowser
import editorGraphicsMethodsWidget
import editorTemplateWidget
import graphicsMethodsWidget
import templatesWidget

class QVCSPlotController(QtGui.QWidget):
    """ Widget containing plot options: plot button, plot type combobox, cell
    col and row selection combo box, and an options button """
    
    def __init__(self, parent=None):
        QtGui.QWidget.__init__(self, parent)
        self.parent = parent
        self.root = parent.root
        ptype = parent.plotOptions.getPlotType()
        layout = QtGui.QVBoxLayout()
        self.setLayout(layout)

        # First we need a vertical splitter to separate the templates/gm section from the plots section

        vsplitter = QtGui.QSplitter(QtCore.Qt.Vertical)

        # Now lets prepare the top section (templates and gms)
        self.top = QtGui.QFrame()
        hlayout_top = QtGui.QHBoxLayout()
        self.top.setLayout(hlayout_top)

        self.choicesFrame =QtGui.QFrame()
        hlayout_choices = QtGui.QHBoxLayout()
        self.choicesFrame.setLayout(hlayout_choices)
        
        self.templates = templatesWidget.QTemplatesWidget(parent)
        hlayout_choices.addWidget(self.templates)

        self.gms = graphicsMethodsWidget.QGraphicsMethodsWidget(ptype,parent)

        hlayout_choices.addWidget(self.gms)

        vsplitter2 = QtGui.QSplitter(QtCore.Qt.Vertical)

        vsplitter2.addWidget(self.choicesFrame)

        self.editorTab = QtGui.QTabWidget()
        self.editorTab.addTab(editorTemplateWidget.QEditorTemplateWidget(self),"'%s' Template Properties" % self.templates.templateList.currentItem().text())
        self.editorTab.addTab(editorGraphicsMethodsWidget.QEditorGraphicsMethodsWidget(ptype,self),"'%s' %s Graphics Method Properties" % (self.gms.gmList.currentItem().text(),ptype))
        self.editorTab.setCurrentIndex(1)
        
        vsplitter2.addWidget(self.editorTab)

        hlayout_top.addWidget(vsplitter2)
        
        vsplitter.addWidget(self.top)

        # Now prepare the bottom section

        self.bottom=QtGui.QFrame()
        vlayout = QtGui.QVBoxLayout()
        self.bottom.setLayout(vlayout)
        vsplitter.addWidget(self.bottom)
        
        layout.addWidget(vsplitter)
        
        
        
