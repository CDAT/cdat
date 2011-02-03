from PyQt4 import QtGui, QtCore
import vcs
import qtbrowser
import graphicsMethodsWidgets
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
        hlayout = QtGui.QHBoxLayout()
        self.top.setLayout(hlayout)
        self.templates = templatesWidget.QTemplatesWidget(parent)

        hlayout.addWidget(self.templates)

        self.gms = graphicsMethodsWidget.QGraphicsMethodsWidget(ptype,parent)

        hlayout.addWidget(self.gms)

        vsplitter.addWidget(self.top)

        # Now prepare the bottom section

        self.bottom=QtGui.QFrame()
        vlayout = QtGui.QVBoxLayout()
        self.bottom.setLayout(vlayout)
        vsplitter.addWidget(self.bottom)
        
        layout.addWidget(vsplitter)
        
        
        
