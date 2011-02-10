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

        ##########################################################
        # Graphics Methods and Templates Selection Section
        ##########################################################
        hlayout_choices = QtGui.QHBoxLayout()
        self.templates = templatesWidget.QTemplatesWidget(self)
        hlayout_choices.addWidget(self.templates)
        self.gms = graphicsMethodsWidget.QGraphicsMethodsWidget(ptype,self)
        hlayout_choices.addWidget(self.gms)

        self.choicesFrame =QtGui.QFrame()
        self.choicesFrame.setLayout(hlayout_choices)

        vsplitter.addWidget(self.choicesFrame)


        ##########################################################
        # Plots setup section
        ##########################################################
        self.plotsSetup=QtGui.QFrame()
        vlayout = QtGui.QVBoxLayout()
        vlayout.addWidget(QtGui.QLabel("plot setup section"))
        self.plotsSetup.setLayout(vlayout)
        vsplitter.addWidget(self.plotsSetup)


        ##########################################################
        # Graphics Method and Templates editor Section
        ##########################################################
        self.editorTab = QtGui.QTabWidget()
        self.editorTab.addTab(editorTemplateWidget.QEditorTemplateWidget(self),"'%s' Template Properties" % self.templates.templateList.currentItem().text())
        self.editorTab.addTab(editorGraphicsMethodsWidget.QEditorGraphicsMethodsWidget(ptype,self),"'%s' %s Graphics Method Properties" % (self.gms.gmList.currentItem().text(),ptype))
        self.editorTab.setCurrentIndex(1)
        vsplitter.addWidget(self.editorTab)        


        layout.addWidget(vsplitter)
        
        
        
