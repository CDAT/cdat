from PyQt4 import QtGui, QtCore
import cdms2



class QCDATFileWidget(QtGui.QWidget):
    """ QCDATFileWidget contains a line-edit to enter the file name and a file
    selection button.  It also has a combo box to choose variables once a file
    is specified. """

    FILTER = "CDAT data (*.cdms *.ctl *.dic *.hdf *.nc *.xml)"

    def __init__(self, parent=None):
        QtGui.QWidget.__init__(self, parent)

        self.cdmsFile = None
        
        # Start the layout
        layout = QtGui.QVBoxLayout()
        self.setLayout(layout)
        layout.setMargin(0)

        self.fileDialog = QFileDialogWidget()
        self.fileDialog.setNameFilter(QCDATFileWidget.FILTER)
        layout.addWidget(self.fileDialog)

        # A shared layout of the bottom half
        vbox = QtGui.QVBoxLayout()
        vbox.setMargin(0)
        vbox.setSpacing(0)
        layout.addLayout(vbox)
        self.fileVarLayout = vbox
        
        # Create the bottom horizontal indicator
        hbox = QtGui.QHBoxLayout()
        
        self.drawerButton = QtGui.QToolButton()
        self.drawerButton.setArrowType(QtCore.Qt.UpArrow)
        self.drawerButton.setAutoRaise(True)
        self.drawerButton.setIconSize(QtCore.QSize(8, 8))
        hbox.addWidget(self.drawerButton)
        
        hline = QtGui.QFrame()
        hline.setFrameStyle(QtGui.QFrame.HLine | QtGui.QFrame.Sunken)
        hbox.addWidget(hline)

        self.connect(self.drawerButton, QtCore.SIGNAL('clicked(bool)'),
                     self.toggleFileDialog)

        vbox.addLayout(hbox)
        
        # Create the file name box
        grid = QtGui.QGridLayout()
        grid.setHorizontalSpacing(10)
        vbox.addLayout(grid)

        # First line: File
        fileNameLabel = QtGui.QLabel('File')
        grid.addWidget(fileNameLabel)

        hbox = QtGui.QHBoxLayout()
        hbox.setSpacing(6)
        self.fileNameEdit = QtGui.QLineEdit()
        self.fileNameEdit.setToolTip('Enter file name or click on button on the right to select a file')
        hbox.addWidget(self.fileNameEdit, 1)

        self.fileSelectButton = QtGui.QToolButton()
        self.fileSelectButton.setText('...')
        self.fileSelectButton.setToolTip('View and select files')
        hbox.addWidget(self.fileSelectButton)
        
        grid.addLayout(hbox, 0, 1)

        # Second line: Var
        varNameLabel = QtGui.QLabel('Variable')
        grid.addWidget(varNameLabel, 1, 0)

        hbox = QtGui.QHBoxLayout()
        hbox.setSpacing(6)

        # Init combo box
        self.varCombo = QtGui.QComboBox()
        self.varCombo.setToolTip('View and select variables in file')
        self.varCombo.setMinimumContentsLength(10)
        comboPalette = self.varCombo.view().palette()
        comboPalette.setColor(QtGui.QPalette.HighlightedText, QtCore.Qt.white)
        comboPalette.setColor(QtGui.QPalette.Highlight, QtCore.Qt.blue)        
        self.varCombo.view().setPalette(comboPalette)

        hbox.addWidget(self.varCombo, 1)
        
        self.defineVarButton = QtGui.QPushButton('&Define')
        self.defineVarButton.setToolTip('Define variable into memory')
        hbox.addWidget(self.defineVarButton)
        self.plotButton = QtGui.QPushButton('&Plot')
        self.plotButton.setToolTip('Plot')
        hbox.addWidget(self.plotButton)

        grid.addLayout(hbox, 1, 1)

        # Connect signals
        self.connect(self.fileDialog, QtCore.SIGNAL('filesSelected(const QStringList&)'),
                     self.filesSelected)
        self.connect(self.fileNameEdit, QtCore.SIGNAL('returnPressed()'),
                     self.updateCDMSFile)
        self.connect(self.fileSelectButton, QtCore.SIGNAL('clicked(bool)'),
                     self.openSelectFileDialog)
        self.connect(self.varCombo, QtCore.SIGNAL('currentIndexChanged(const QString&)'),
                     self.variableSelected)
        self.connect(self.defineVarButton, QtCore.SIGNAL('clicked(bool)'),
                     self.defineVariablePressed)

        # Init the widget with its file dialog hidden
        self.fileDialog.hide()
        self.setFileName('')
        self.varCombo.setCurrentIndex(1)

    def updateSizeConstraints(self):
        isDialogVisible = self.fileDialog.isVisible()
        if isDialogVisible:
            self.drawerButton.setArrowType(QtCore.Qt.UpArrow)
            self.setMaximumHeight(16777215)
        else:
            self.drawerButton.setArrowType(QtCore.Qt.DownArrow)
            self.setMaximumHeight(self.fileVarLayout.contentsRect().height())
        self.fileSelectButton.setVisible(not isDialogVisible)

    def toggleFileDialog(self, ignored=False):
        self.fileDialog.setVisible(not self.fileDialog.isVisible())
        self.updateSizeConstraints()

    def showEvent(self, e):
        self.updateSizeConstraints()
        self.variableSelected(self.varCombo.currentText())

    def setFileName(self, fn):
        self.fileNameEdit.setText(fn)
        self.updateCDMSFile()

    def updateCDMSFile(self):
        fn = str(self.fileNameEdit.text())
        fi = QtCore.QFileInfo(fn)
        if fi.exists():
            self.fileDialog.setDirectory(fi.dir())
            self.cdmsFile = cdms2.open(fn)
            self.recordOpenFileTeachingCommand(fn)
        else:
            self.cdmsFile = None
        self.updateVariableList()

    def recordOpenFileTeachingCommand(self, file):
        openFileComment = '\n# Open CDMS File\n'
        varName = 'fid2'
        command = "%s = cdms2.open('%s')\n" %(varName, file)

        self.emit(QtCore.SIGNAL('recordTeachingCommand'), openFileComment)
        self.emit(QtCore.SIGNAL('recordTeachingCommand'), command)

    def filesSelected(self, files):
        if files.count()>0:
            self.setFileName(files[0])
            
    def updateVariableList(self):
        self.varCombo.clear()
        if self.cdmsFile!=None:
            # Add Variables sorted based on their dimensions
            curDim = -1
            for (dim, varId) in sorted([(len(var.listdimnames()), var.id)
                                        for var in self.cdmsFile.variables.itervalues()]):
                if dim!=curDim:
                    curDim = dim
                    count = self.varCombo.count()
                    self.varCombo.insertSeparator(count)
                    self.varCombo.model().item(count, 0).setText('%dD VARIABLES' % dim)
                var = self.cdmsFile.variables[varId]
                varName = var.id + ' ' + str(var.shape) + ' ['
                
                if hasattr(var, 'long_name'):
                    varName += var.long_name
                if hasattr(var, 'units') and var.units!='':
                    if varName[-1]!='[': varName += ' '
                    varName += var.units
                varName += ']'
                self.varCombo.addItem(varName, QtCore.QVariant(QtCore.QStringList(['variables', var.id])))

            # Add Axis List
            count = self.varCombo.count()
            self.varCombo.insertSeparator(count)
            self.varCombo.model().item(count, 0).setText('AXIS LIST')
            for axis in self.cdmsFile.axes.itervalues():
                axisName = axis.id + " (" + str(len(axis)) + ") - [" + axis.units + ":  (" + str(axis[0]) + ", " + str(axis[-1]) + ")]"                
                self.varCombo.addItem(axisName, QtCore.QVariant(QtCore.QStringList(['axes', axis.id])))

            # By default, not selecting anything
            self.varCombo.setCurrentIndex(-1)

    def openSelectFileDialog(self):
        file = QtGui.QFileDialog.getOpenFileName(self, 'Open CDAT data file...',
                                                 self.fileDialog.directory().absolutePath(),
                                                 QCDATFileWidget.FILTER + ';;All files (*.*)')
        if not file.isNull():
            self.setFileName(file)

    def variableSelected(self, varName):
        if varName == '':
            return

        self.defineVarButton.setEnabled(not varName.isNull()) # Enable define button
        
        # Send signal to setup axisList in 'quickplot' tab
        self.emit(QtCore.SIGNAL('variableSelectedEvent'), self.getCDMSFile(),
                  self.getCDMSVariable(), '%s (in file)' % self.getCDMSVariable().id)
        
    def defineVariablePressed(self):
        self.emit(QtCore.SIGNAL('defineVariableFromFileEvent'))

    def getCDMSFile(self):
        return self.cdmsFile

    def getCDMSVariable(self):
        if not self.cdmsFile is None:
            data = self.varCombo.itemData(self.varCombo.currentIndex()).toStringList()
            if data.count() > 0:
                if data[0] == 'variables':
                    return getattr(self.cdmsFile, str(data[0]))[str(data[1])]
                elif data[0] == 'axes':
                    axis = getattr(self.cdmsFile, str(data[0]))[str(data[1])]
                    var = MV2.array(axis)
                    var.setAxis(0, axis)
                    var.id = axis.id
                    return var
        return None

class QFileDialogWidget(QtGui.QFileDialog):
    
    def __init__(self, parent=None):
        QtGui.QFileDialog.__init__(self, parent, QtCore.Qt.Widget)
        self.setModal(False)
        self.setSizeGripEnabled(False)
        self.setFileMode(QtGui.QFileDialog.ExistingFile)
        self.setLabelText(QtGui.QFileDialog.LookIn, 'Directory')
        self.setSidebarUrls([QtCore.QUrl('file://')])

        gridLayout = self.findChild(QtGui.QGridLayout, 'gridLayout')
        if gridLayout:
            gridLayout.setMargin(0)
            gridLayout.setVerticalSpacing(0)
            gridLayout.setColumnStretch(1, 1)
            hBoxLayout = gridLayout.itemAtPosition(0, 1).layout()
            if hBoxLayout:
                hBoxLayout.setSpacing(0)
        
        # Hide the Back and Forward button
        backButton = self.findChild(QtGui.QToolButton, 'backButton')
        if backButton: backButton.hide()
        forwardButton = self.findChild(QtGui.QToolButton, 'forwardButton')
        if forwardButton: forwardButton.hide()            
        
        # Hide the File Name indicators
        fileNameLabel = self.findChild(QtGui.QLabel, 'fileNameLabel')
        if fileNameLabel: fileNameLabel.hide()
        fileNameEdit = self.findChild(QtGui.QLineEdit, 'fileNameEdit')
        if fileNameEdit: fileNameEdit.hide()

        # Hide the File Type indicators
        fileTypeLabel = self.findChild(QtGui.QLabel, 'fileTypeLabel')
        if fileTypeLabel: fileTypeLabel.hide()
        fileTypeCombo = self.findChild(QtGui.QComboBox, 'fileTypeCombo')
        if fileTypeCombo: fileTypeCombo.hide()

        # Hide the dialog buttons
        buttonBox = self.findChild(QtGui.QDialogButtonBox, 'buttonBox')
        buttonBox.hide()

        # Adjust the sidebar width
        splitter = self.findChild(QtGui.QSplitter, 'splitter')
        splitter.setSizes([0, 1])

        # Simplify the Details view to List View
        stackedWidget = splitter.widget(1).findChild(QtGui.QStackedWidget, 'stackedWidget')
        listView = stackedWidget.widget(0).findChild(QtGui.QListView, 'listView')
        listView.setAlternatingRowColors(True)
        listView.setWrapping(False)
        self.setViewMode(QtGui.QFileDialog.List)

    def done(self, result):
        pass

    def sizeHint(self):
        return QtCore.QSize(384, 150)
