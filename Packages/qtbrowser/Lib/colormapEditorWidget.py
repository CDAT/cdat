from PyQt4 import QtCore,QtGui
import customizeVCDAT
import os
import vcdatCommons

def parseLayout(l,prefix=""):
    for i in range(l.count()):
        it=l.itemAt(i)
        print prefix,i,it.__class__
        if isinstance(it,QtGui.QWidgetItem):
            w=it.widget()
            print prefix,w,
            if isinstance(w,(QtGui.QPushButton,QtGui.QLabel)):
                print prefix,w.text()
            else:
                print
        elif isinstance(it,QtGui.QLayoutItem):
            print prefix,it.__class__
            l2 = it.layout()
            if l2 is None:
                print "No Layout"
            else:
                parseLayout(l2,"%s\t" % prefix)
class QColormapEditor(QtGui.QColorDialog):
    def __init__(self,parent):
        QtGui.QColorDialog.__init__(self,parent)
        self.parent=parent
        self.root=parent.root
        self.setOption(QtGui.QColorDialog.DontUseNativeDialog,True)
        
        ## l = QtGui.QVBoxLayout()

        l = self.layout()

        parseLayout(l)

        l.removeWidget(l.itemAt(1).widget())
        l.removeItem(l.itemAt(1))
        self.toolBar = QtGui.QToolBar()
        self.toolBar.setIconSize(QtCore.QSize(customizeVCDAT.iconsize, customizeVCDAT.iconsize))
        actionInfo = [
            ('folder_image_blue.ico', 'Save Colormap.',self.save,True),
            ]

        for info in actionInfo:
            icon = QtGui.QIcon(os.path.join(customizeVCDAT.ICONPATH, info[0]))
            action = self.toolBar.addAction(icon, 'help')
            action.setToolTip(info[1])
            self.connect(action,QtCore.SIGNAL("triggered()"),info[2])
            action.setEnabled(info[3])
        self.toolBar.addSeparator()

        l.addWidget(self.toolBar)

        f = QtGui.QFrame()
        h = QtGui.QHBoxLayout()
        colormaps = sorted(self.root.canvas[0].listelements("colormap"))
        self.colormap = QtGui.QComboBox(self)
        for i in colormaps:
            self.colormap.addItem(i)
            
        h.addWidget(self.colormap)
        le =QtGui.QLineEdit()
        h.addWidget(le)
        b=QtGui.QPushButton("Rename")
        self.connect(b,QtCore.SIGNAL("clicked()"),self.renamed)
        f.setLayout(h)
        self.connect(self.colormap,QtCore.SIGNAL("currentIndexChanged(int)"),self.updateColors)


        self.colors=QtGui.QFrame()
        self.mylayout = l
        self.colormap.setCurrentIndex(colormaps.index(self.root.canvas[0].getcolormapname()))
        self.setLayout(l)
        self.installEventFilter(self)

        
    def save(self):
        pass

    def renamed(self):
        pass

    def colorButtonClicked(self,*args):
        print "bclicke",args
        
    def updateColors(self):
        self.colors.destroy()
        cmap = self.root.canvas[0].getcolormap(str(self.colormap.currentText()))
        self.colors=QtGui.QFrame()
        grid = QtGui.QGridLayout()
        self.colors.setLayout(grid)

        icolor = 0
        for i in range(16):
            for j in range(16):
                cell = cmap.index[icolor]
                b = vcdatCommons.CalcButton("%i" % icolor,styles= {"background-color":"rgb(%i,%i,%i)" % (cell[0],cell[1],cell[2])},signal="clickedVCSColorButton")
                b.setStyleSheet("background-color : rgb(%i,%i,%i)" % (cell[0],cell[1],cell[2]))
                b.vcscolor=icolor
                self.connect(b,QtCore.SIGNAL("clickedVCSColorButton"),self.colorButtonClicked)
                grid.addWidget(b,i,j)
                icolor+=1
        self.layout().addWidget(self.colors)
        self.update()
        
    
