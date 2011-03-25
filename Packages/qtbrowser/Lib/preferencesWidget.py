from PyQt4 import QtGui, QtCore
import vcdatCommons
import cdms2
import os

class QPreferencesDialog(QtGui.QDialog):

    def __init__(self, parent):
        QtGui.QDialog.__init__(self, parent)
        self.parent=parent
        self.root=parent.root
        self._status_bar = QtGui.QStatusBar(self)
        self.setWindowTitle('UV-CDAT Preferences')
        layout = QtGui.QHBoxLayout(self)
        layout.setMargin(0)
        layout.setSpacing(0)
        self.setLayout(layout)

        f = QtGui.QFrame()
        layout.addWidget(f)
        
        l = QtGui.QVBoxLayout(f)
        f.setLayout(l)
        
        self._tab_widget = QtGui.QTabWidget(f)
        l.addWidget(self._tab_widget)
        self._tab_widget.setSizePolicy(QtGui.QSizePolicy.Expanding,
                                       QtGui.QSizePolicy.Expanding)

        self._tab_widget.addTab(self.guiTab(self),"GUI")
        self._tab_widget.addTab(self.ioTab(self),"I/O")
        self._tab_widget.addTab(self.vcsTab(self),"VCS")

    def guiTab(self,parent):
        tab= QtGui.QFrame()
        l=QtGui.QVBoxLayout()
        tab.setLayout(l)
        b = QtGui.QCheckBox("Confirm Before Exiting")
        b.setEnabled(False)
        l.addWidget(b)
        b=QtGui.QPushButton("Save GUI State")
        self.connect(b,QtCore.SIGNAL("clicked()"),self.saveState)
        b.setEnabled(False)
        l.addWidget(b)
        return tab
    def ioTab(self,parent):
        tab= QtGui.QFrame()
        l=QtGui.QVBoxLayout()
        tab.setLayout(l)
        nc = vcdatCommons.QFramedWidget("NetCDF Settings")
        self.ncShuffle = nc.addCheckBox("Shuffle")
        if cdms2.getNetcdfShuffleFlag():
            self.ncShuffle.setChecked(True)
        self.ncDeflate = nc.addCheckBox("Deflate",newRow=True)
        if cdms2.getNetcdfDeflateFlag():
            self.ncDeflate.setChecked(True)
        self.ncDeflateLevel = nc.addLabeledSlider("Deflate Level",newRow=True,minimum=0,maximum=9)
        self.ncDeflateLevel.setTickInterval(1)
        self.ncDeflateLevel.setTickPosition(QtGui.QSlider.TicksAbove)
        self.ncDeflateLevel.setValue(cdms2.getNetcdfDeflateLevelFlag())
        self.connect(self.ncShuffle,QtCore.SIGNAL("stateChanged(int)"),self.nc)
        self.connect(self.ncDeflate,QtCore.SIGNAL("stateChanged(int)"),self.nc)
        self.connect(self.ncDeflateLevel,QtCore.SIGNAL("valueChanged(int)"),self.nc)
        l.addWidget(nc)
        printers = vcdatCommons.QFramedWidget("Printers Settings")
        printers.setEnabled(False)
        l.addWidget(printers)
        return tab

    def vcsTab(self,parent):
        tab= QtGui.QFrame()
        l=QtGui.QVBoxLayout()
        tab.setLayout(l)
        self.saveVCS = QtGui.QPushButton("Save VCS Settings")
        self.connect(self.saveVCS,QtCore.SIGNAL("clicked()"),self.root.canvas[0].saveinitialfile)
        l.addWidget(self.saveVCS)

        fonts = sorted(self.root.canvas[0].listelements("font"))
        #fonts.pop(fonts.index("default"))
        font = vcdatCommons.QFramedWidget("Fonts")
        self.vcsFont = c = font.addLabeledComboBox("Default Font",fonts)
        c.setCurrentIndex(fonts.index(self.root.canvas[0].getfontname(1)))
        self.connect(c,QtCore.SIGNAL("currentIndexChanged(int)"),self.newDefaultFont)

        b = font.addButton("Load a font for file",newRow=True)
        self.connect(b,QtCore.SIGNAL("clicked()"),self.addFont)
        l.addWidget(font)

        
        return tab

    def nc(self):
        if self.ncShuffle.isChecked():
            cdms2.setNetcdfShuffleFlag(1)
        else:
            cdms2.setNetcdfShuffleFlag(0)
        if self.ncDeflate.isChecked():
            cdms2.setNetcdfDeflateFlag(1)
        else:
            cdms2.setNetcdfDeflateFlag(0)
        cdms2.setNetcdfDeflateLevelFlag(self.ncDeflateLevel.value())

    def saveState(self):
        "TODO"
        return
    
    def newDefaultFont(self):
        fnm = str(self.vcsFont.currentText())
        self.root.canvas[0].setdefaultfont(fnm)
        
    def addFont(self):
        fpth = str(QtGui.QFileDialog.getOpenFileName(self,"font",filter ="Fonts (*.ttf) ;; All (*.*)"))
        fnm = os.path.split(fpth)[1].lower()
        if fnm[-4:]==".ttf":
            fnm=fnm[:-4]
        fnm=fnm.replace(" ","_")
        fontsold = sorted(self.root.canvas[0].listelements("font"))
        try:
            self.root.canvas[0].addfont(fpth,fnm)
        except Exception,err:
            return
        fontsnew = sorted(self.root.canvas[0].listelements("font"))
        for i in range(len(fontsnew)):
            if not fontsnew[i] in fontsold:
                ## ok that the new one
                self.vcsFont.insertItem(i,fontsnew[i])
                
        
