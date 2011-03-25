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

        self.vcscolor=[0,0,0]
        ## l = QtGui.QVBoxLayout()

        l = self.layout()

        self.nclicks = 0
        self.clicks=[None,None]
        #parseLayout(l)

        editColor = l.itemAt(0)
        buttons = l.itemAt(1)
        #l.removeItem(editColor)
        l.removeItem(buttons)
        #l.addItem(editColor)

        ## Colormap selection Area
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
        h.addWidget(b)
        f.setLayout(h)
        l.addWidget(f)

        ## Toolbar section
        self.toolBar = QtGui.QToolBar()
        self.toolBar.setIconSize(QtCore.QSize(customizeVCDAT.iconsize, customizeVCDAT.iconsize))
        actionInfo = [
            ('folder_image_blue.ico', 'Save Colormap To File.',self.save,False),
            ('blender-icon.png', 'Blend From First To Last Highlighted Colors.',self.blend,True),
            ('symbol_refresh.ico', 'Reset Changes.',self.resetChanges,True),
            ('symbol_check.ico', 'Apply Changes.',self.applyChanges,True),
            ]
        for info in actionInfo:
            icon = QtGui.QIcon(os.path.join(customizeVCDAT.ICONPATH, info[0]))
            action = self.toolBar.addAction(icon, 'help')
            action.setToolTip(info[1])
            self.connect(action,QtCore.SIGNAL("triggered()"),info[2])
            action.setEnabled(info[3])

        l.addWidget(self.toolBar)

        # Color Buttons Are
        self.colors=QtGui.QFrame()
        self.grid = QtGui.QGridLayout()
        self.grid.setHorizontalSpacing(1)
        self.grid.setVerticalSpacing(1)
        self.colors.setLayout(self.grid)
        l.addWidget(self.colors)

        # Ok/Cancel Buttons # Unconnected
        l.addItem(buttons)

        ## SIGNALS
        self.connect(self.colormap,QtCore.SIGNAL("currentIndexChanged(int)"),self.updateColors)
        self.connect(self,QtCore.SIGNAL("currentColorChanged(QColor)"),self.colorChanged)

        ## Now that it's connected select the colormap
        self.colormap.setCurrentIndex(colormaps.index(self.root.canvas[0].getcolormapname()))

    def getRgb(self,i,j=None,max=255):
        if j is None:
            if max>=100:
                mx = max/100.
            else:
                mx=max
            nr,ng,nb = self.cmap.index[i]
            nr=int(nr*mx)
            ng=int(ng*mx)
            nb=int(nb*mx)
        else:
            if max>=100:
                mx=max/255.
            else:
                mx=max
            styles = str(self.grid.itemAtPosition(i,j).widget().styleSheet()).split(";")
            for style in styles:
                sp =style.split(":")
                if sp[0].strip() == "background-color":
                        
                    r,g,b = eval(sp[1].strip()[4:-1])
                    r=int(r*mx)
                    g=int(g*mx)
                    b=int(b*mx)
                    return r,g,b
            return 0,0,0
            
        return nr,ng,nb

    def applyChanges(self):
        cnm = self.root.canvas[0].getcolormapname()
        n=0
        for i in range(15):
            for j in range(16):
                r,g,b = self.getRgb(i,j,max=100)
                self.root.canvas[0].setcolorcell(n,r,g,b)
                n+=1
        self.root.canvas[0].setcolormap(cnm)
        
    def resetChanges(self):
        for i in range(16):
            for j in range(16):
                bt = self.grid.itemAtPosition(i,j).widget()
                r,g,b = self.getRgb(bt.vcscolor[2])
                self.setButton(i,j,bt.vcscolor[2],r,g,b)
    def colorChanged(self):
        current = self.currentColor()
        nr,ng,nb = self.getRgb(self.vcscolor[2])
        cr,cg,cb,ca = current.getRgb()
        if cr!=nr or cg!=ng or cb!=nb:
            b = self.setButton(self.vcscolor[0],self.vcscolor[1],self.vcscolor[2],cr,cg,cb)
            self.setAButtonFrame(b)
    def save(self):
        pass

    def renamed(self):
        pass

    def colorButtonClicked(self,b):
        current = self.currentColor()
        nr,ng,nb = self.getRgb(b.vcscolor[2])
        cr,cg,cb,ca = current.getRgb()
        if cr!=nr or cg!=ng or cb!=nb:
            self.vcscolor = b.vcscolor
            self.setCurrentColor(QtGui.QColor(nr,ng,nb))
        self.nclicks+=1
        if self.nclicks==3:
            self.nclicks=1
        else:
            self.clicks[self.nclicks-1]=b.vcscolor
        self.setButtonFrame(b)
        self.clicks[self.nclicks-1]=b.vcscolor

    def setButtonFrame(self,button):
        firstButton = self.clicks[0]
        i0,j0 = firstButton[:2]
        lastButton = self.clicks[1]
        if lastButton is not None: # Not the first time
            i1,j1 = lastButton[:2]
            if i1<i0:
                tmp1 = i0
                tmp2=j0
                i0=i1
                j0=j1
                i1=tmp1
                j1=tmp2
            elif i0==i1 and j1<j0:
                tmp1=j0
                j0=j1
                j1=tmp1
                
            for i in range(i0,i1+1):
                if i==i0:
                    ij0=j0
                else:
                    ij0=0
                if i==i1:
                    ij1=j1+1
                else:
                    ij1=16
                for j in range(ij0,ij1):
                    ob = self.grid.itemAtPosition(i,j).widget()
                    if self.nclicks==2:
                        self.setAButtonFrame(ob)
                    else:
                        self.setAButtonFrame(ob,on=False)
            if self.nclicks==1:
                self.setAButtonFrame(button)
        else:
            self.setAButtonFrame(button)


    def blend(self):
        first = None
        last = None
        n=0
        for i in range(16):
            for j in range(16):
                b= self.grid.itemAtPosition(i,j).widget()
                stsh = str(b.styleSheet())
                if stsh.find(customizeVCDAT.colorSelectedStyle)>-1:
                    n+=1
                    if first is None:
                        first = b.vcscolor
                    else:
                        last = b.vcscolor
        if n<2:
            return

        fr,fg,fb = self.getRgb(*first[:2])
        lr,lg,lb = self.getRgb(*last[:2])

        
        dr = float(lr-fr)/float(n-1)
        dg = float(lg-fg)/float(n-1)
        db = float(lb-fb)/float(n-1)

        n=0
        for i in range(first[0],last[0]+1):
            if i == first[0]:
                j0 = first[1]
            else:
                j0=0
            if i== last[0]:
                j1=last[1]+1
            else:
                j1=16
            for j in range(j0,j1):
                button = self.grid.itemAtPosition(i,j).widget()
                r= int(fr+n*dr)
                g= int(fg+n*dg)
                b= int(fb+n*db)
                self.setButton(i,j,button.vcscolor[2],r,g,b)
                self.setAButtonFrame(button)
                n+=1
        
        button = self.grid.itemAtPosition(first[0],first[1]).widget()
        button.click()
        button = self.grid.itemAtPosition(last[0],last[1]).widget()
        button.click()
        
    def setAButtonFrame(self,button,on=True):
        styles = str(button.styleSheet()).split(";")
        newstyles=[]
        for style in styles:
            sp=style.split(":")
            if sp[0].strip() not in ["border"]:
                newstyles.append(":".join(sp))
        if on:
            newstyles.append(customizeVCDAT.colorSelectedStyle)
        else:
            newstyles.append(customizeVCDAT.colorNotSelectedStyle)
            
        styles=";".join(newstyles)
        button.setStyleSheet(styles)
        
        
    def setButton(self,i,j,icolor,r,g,b):
        it = self.grid.itemAtPosition(i,j)
        if it is not None:
            self.grid.removeItem(it)
            it.widget().destroy()
        button = vcdatCommons.CalcButton("%i" % icolor,styles={},signal="clickedVCSColorButton",minimumXSize=15,minimumYSize=15)
        stsh = button.styleSheet()
        stsh+=" background-color : rgb(%i,%i,%i)" % (r,g,b)
        if g<200:
            stsh+=";color : white"
        button.setStyleSheet(stsh)
        button.vcscolor=(i,j,icolor)
        self.connect(button,QtCore.SIGNAL("clickedVCSColorButton"),self.colorButtonClicked)
        self.grid.addWidget(button,i,j)
        return button
        
    def updateColors(self):
        n = self.layout().count()
        self.cmap = self.root.canvas[0].getcolormap(str(self.colormap.currentText()))
        self.colors=QtGui.QFrame()
        self.root.canvas[0].setcolormap(self.cmap)

        icolor = 0
        for i in range(16):
            for j in range(16):
                r,g,b = self.cmap.index[icolor]
                r=int(r*2.55)
                g=int(g*2.55)
                b=int(b*2.55)
                self.setButton(i,j,icolor,r,g,b)
                icolor+=1
        self.update()
        
    
