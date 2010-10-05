import types
import qtbrowser
from PyQt4 import QtCore, QtGui
if qtbrowser.use_vistrails:
    from gui_controller import gm_name

indentSpacing = 10

class QGraphicsMethodAttributeWindow(QtGui.QMainWindow):

    def __init__(self, canvas=None, parent=None):
        QtGui.QMainWindow.__init__(self, parent)
        initialWidth = 480
        initialHeight = 640
        self.resize(initialWidth, initialHeight)        
        self.setWindowTitle('Graphics Methods Attrtibute Settings')
        self.parent = parent

        # Create tab widgets
        self.tabWidget = QtGui.QTabWidget()
        if canvas is not None:
            self.boxfillEditor = QBoxfillEditor(gm=canvas.getboxfill('ASD'))
            self.continentsEditor = QContinentsEditor(gm=canvas.getcontinents('ASD'))
            self.meshfillEditor = QMeshfillEditor(gm=canvas.getmeshfill())
            self.outfillEditor = QOutfillEditor(gm=canvas.getoutfill())
            self.scatterEditor = QScatterEditor(gm=canvas.getscatter('ASD'))
            self.taylorEditor = QTaylorDiagramEditor(gm=canvas.gettaylordiagram('ASD'))
            self.vectorEditor = QVectorEditor(gm=canvas.getvector())
        else:
            self.boxfillEditor = QBoxfillEditor()
            self.continentsEditor = QContinentsEditor()
            self.meshfillEditor = QMeshfillEditor()
            self.outfillEditor = QOutfillEditor()
            self.scatterEditor = QScatterEditor()
            self.taylorEditor = QTaylorDiagramEditor()
            self.vectorEditor = QVectorEditor()
            
        self.contourEditor = QContourEditor()
        self.oneDimEditor = Q1DPlotEditor()
        self.outlineEditor = QOutlineEditor()
        
        # Add tabs
        self.tabWidget.addTab(self.boxfillEditor, 'Boxfill')
        self.tabWidget.addTab(self.continentsEditor, 'Continents')
        self.tabWidget.addTab(self.contourEditor, 'Contour')
        self.tabWidget.addTab(self.meshfillEditor, 'Meshfill')
        self.tabWidget.addTab(self.oneDimEditor, '1D Plot')
        self.tabWidget.addTab(self.outfillEditor, 'Outfill')
        self.tabWidget.addTab(self.outlineEditor, 'Outline')
        self.tabWidget.addTab(self.scatterEditor, 'Scatter')
        self.tabWidget.addTab(self.taylorEditor, 'Taylor Diagram')
        self.tabWidget.addTab(self.vectorEditor, 'Vector')
        self.setToolTips()        
        
        # Add preview, reset, apply, cancel buttons
        previewButton = self.getButton('Preview')
        resetButton = self.getButton('Reset')
        applyButton = self.getButton('Apply')
        cancelButton = self.getButton('Cancel')

        hbox = QtGui.QHBoxLayout()
        hbox.addWidget(previewButton)
        hbox.addWidget(resetButton)
        hbox.addWidget(applyButton)
        hbox.addWidget(cancelButton)

        # Setup the layout
        vbox = QtGui.QVBoxLayout()
        vbox.addWidget(self.tabWidget)
        vbox.addLayout(hbox)
        wrapperWidget = QtGui.QWidget()
        wrapperWidget.setLayout(vbox)
        self.setCentralWidget(wrapperWidget)

        # Connect Signals
        self.connect(cancelButton, QtCore.SIGNAL('pressed()'), self.close)
        self.connect(resetButton, QtCore.SIGNAL('pressed()'), self.resetPressedEvent)
        self.connect(applyButton, QtCore.SIGNAL('pressed()'), self.applyPressedEvent)
        self.connect(previewButton, QtCore.SIGNAL('pressed()'), self.previewPressedEvent)

    def resetPressedEvent(self):
        self.boxfillEditor.initValues()
        self.continentsEditor.initValues()
        self.meshfillEditor.initValues()
        self.outfillEditor.initValues()
        self.scatterEditor.initValues()
        self.taylorEditor.initValues()
        self.vectorEditor.initValues()
            
        self.contourEditor.initValues()
        self.oneDimEditor.initValues()
        self.outlineEditor.initValues()

    def applyPressedEvent(self):
        #self.boxfillEditor.setVistrailsGraphicsMethod(self.parent.getParent())
        
        # TODO
        # self.continentsEditor.setVistrailsGraphicsMethod()
        # self.meshfillEditor.setVistrailsGraphicsMethod()
        # self.outfillEditor.setVistrailsGraphicsMethod()
        # self.scatterEditor.setVistrailsGraphicsMethod()
        # self.taylorEditor.setVistrailsGraphicsMethod()
        # self.vectorEditor.setVistrailsGraphicsMethod()
        # self.contourEditor.setVistrailsGraphicsMethod()
        # self.oneDimEditor.setVistrailsGraphicsMethod()
        # self.outlineEditor.setVistrailsGraphicsMethod()
        return #todo

    def previewPressedEvent(self):
        return # TODO

    def setToolTips(self):
        self.tabWidget.setTabToolTip(0, 'The Boxfill graphics method displays a two-dimensional data\narray by surrounding each data value with a colored grid box.')
        self.tabWidget.setTabToolTip(1, "The Continents graphics method draws a predefined,\ngeneric set of continental outlines in a longitude\nby latitude space. (To draw continental outlines,\nno external data set is required.)")
        self.tabWidget.setTabToolTip(2, "This Contour notebook tab represent both the Isofill\nand Isoline graphics methods. The Isofill graphics\nmethod fills the area between selected isolevels\n(levels of constant value) of a two-dimensional\narray; the manner of filling the area is determined\nby the named fill area attributes. The Isoline\ngraphics method draws lines of constant value at\nspecified levels to graphically represent the values\nof a two-dimensional array; labels also can be\ndisplayed on the isolines.\nIsolines can also have \"orientation\" arrows, indicating clockwise or counter-clockwise")
        self.tabWidget.setTabToolTip(3, "The Meshfill graphics method draws data on irregular grid (or 'mesh')at specified levels to graphically represent\nthe values of a one-dimensional array;\nUnless the irregular grid is supported by cdms2, a mesh array must be passed as well")
        self.tabWidget.setTabToolTip(4, "This 1D Plot notebook tab represent the XvsY, Xyvsy,\nand Yxvsx graphics methods. The XvsY graphics method\ndisplays a line plot from two 1D data arrays, that\nis X(t) and Y(t), where t represents the 1D\ncoordinate values. The Xyvsy graphics method displays\na line plot from a 1D data array, that is X(y),\nwhere y represents the 1D coordinate values. The\nYxvsx graphics method displays a line plot from\na 1D data array, that is Y(x), where y represents\nthe 1D coordinate values.")
        self.tabWidget.setTabToolTip(5, "The Outfill graphics method fills a set of integer\nvalues in any data array. Its primary purpose is\nto display continents by filling their area as\ndefined by a surface type array that indicates land,\nocean, and sea-ice points. ")
        self.tabWidget.setTabToolTip(6, "The Outline graphics method outlines a set of integer\nvalues in any data array. Its primary purpose is\nto display continental outlines as defined by a\nsurface type array that indicates land, ocean, and\nsea-ice points.")
        self.tabWidget.setTabToolTip(7, "The Scatter graphics method displays a scatter plot\nof two 4-dimensional data arrays, e.g. A(x,y,z,t)\nand B(x,y,z,t). ")
        self.tabWidget.setTabToolTip(8, "The Taylor diagram graphics method provides a statistical\nsummary of pattern correspondence. A single point on\nthe diagram indicates how similar two patterns are in\nterms of their correlation, root-mean-square (RMS)\ndifference, and the ratio of their variances.  The\nstandard deviation of a pattern is proportional to the\nradial distance.  The correlation is given by the cosine\nof the azimuthal angle. The RMS difference is proportional\nto the distance between the plotted points and the\nreference point (often chosen to be the observed\npattern), which is located along the abscissa at a radial\ndistance proportional to its standard deviation.")
        self.tabWidget.setTabToolTip(9, "The Vector graphics method displays a vector plot\nof a 2D vector field. Vectors are located at the\ncoordinate locations and point in the direction of\nthe data vector field. Vector magnitudes are the\nproduct of data vector field lengths and a scaling\nfactor. ")

    def getButton(self, text):
        button = QtGui.QToolButton()
        button.setText(text)
        return button

class QVectorEditor(QtGui.QScrollArea):
    def __init__(self, parent=None, gm=None):
        QtGui.QScrollArea.__init__(self, parent)
        vbox = QtGui.QVBoxLayout()

        frame = QFramedWidget()
        self.lineType = frame.addLabeledComboBox('Vector Line Type:',
                                                 ['solid', 'dash', 'dot','dash-dot', 'long-dash'])
        self.colorIndex = frame.addLabeledSpinBox('Vector Line Color Index',
                                                  0, 255)
        self.scale = frame.addLabeledDoubleSpinBox('Vector Scale', -1e20, 1e20, .1)
        self.alignment = frame.addLabeledComboBox('Vector Alignment:',
                                                  ['head', 'center', 'tail'])
        self.headType = frame.addLabeledComboBox('Vector Head Type:',
                                                 ['arrows', 'barbs', 'solidarrows'])
        self.reference = frame.addLabeledDoubleSpinBox('Vector reference', -1e20, 1e20, .1)
    
        vbox.addWidget(frame)

        # Init values & set tool tips
        self.initValues()
        self.lineType.setToolTip("Select the vector line type.")
        self.colorIndex.setToolTip("Select the vector color index.")
        self.scale.setToolTip("Select the vector scale factor.")
        self.alignment.setToolTip("Set the vector alignment.")
        self.headType.setToolTip("Set the vector head type.")
        self.reference.setToolTip("Set the vector reference. Note: if the value is 1e+20,\nthen VCS will determine the vector reference.")        

        # Set up the scrollbar
        widgetWrapper = QtGui.QWidget()
        widgetWrapper.setLayout(vbox)
        widgetWrapper.setMinimumWidth(580)
        self.setWidget(widgetWrapper)

    def initValues(self):
        # TODO: don't init w/ hardcoded values?
        self.colorIndex.setValue(241)
        self.scale.setValue(1)
        self.alignment.setCurrentIndex(1)
        self.headType.setCurrentIndex(0)
        self.reference.setValue(1e20)

class QTaylorDiagramEditor(QtGui.QScrollArea):
    def __init__(self, parent=None, gm=None):
        QtGui.QScrollArea.__init__(self, parent)
        vbox = QtGui.QVBoxLayout()

        tabWidget = QtGui.QTabWidget()
        interfaceTab = QTaylorInterfaceTab()
        tickAndLabelTab = QTaylorTicksLabels()
        markersTab = QTaylorMarkers()
        tabWidget.addTab(interfaceTab, 'Interface')
        tabWidget.addTab(markersTab, 'Markers')
        tabWidget.addTab(tickAndLabelTab, 'Ticks and Labels')
        vbox.addWidget(tabWidget)

        # Set up the scrollbar / widget size
        widgetWrapper = QtGui.QWidget()
        widgetWrapper.resize(670, 520)
        widgetWrapper.setLayout(vbox)
        self.setWidget(widgetWrapper)

    def initValues(self):
        interfaceTab.initValues()
        markersTab.initValues()
        tickAndLabelTab.initValues()

class QTaylorMarkers(QtGui.QScrollArea):
    """ Tabbed Widget for Taylor -> Markers """
    
    def __init__(self, parent=None, gm=None):
        QtGui.QScrollArea.__init__(self, parent)
        self.row = 1
        self.grid = QtGui.QGridLayout()
        self.markerList = []
        vbox = QtGui.QVBoxLayout()
        
        # Create Column Labels
        self.grid.addWidget(QtGui.QLabel(''), 0, 0)
        self.grid.addWidget(QtGui.QLabel('#'), 0, 1)
        self.grid.addWidget(QtGui.QLabel('Active'), 0, 2)
        self.grid.addWidget(QtGui.QLabel('Symbol'), 0, 3)
        self.grid.addWidget(QtGui.QLabel('Color'), 0, 4)
        self.grid.addWidget(QtGui.QLabel('Size'), 0, 5)
        self.grid.addWidget(QtGui.QLabel('Id'), 0, 6)
        self.grid.addWidget(QtGui.QLabel('Id Size'), 0, 7)
        self.grid.addWidget(QtGui.QLabel('+X'), 0, 8)
        self.grid.addWidget(QtGui.QLabel('+Y'), 0, 9)
        self.grid.addWidget(QtGui.QLabel('Id Color'), 0, 10)
        self.grid.addWidget(QtGui.QLabel('Id Font'), 0, 11)
        self.grid.addWidget(QtGui.QLabel('Line'), 0, 12)
        self.grid.addWidget(QtGui.QLabel('Type'), 0, 13)
        self.grid.addWidget(QtGui.QLabel('Size'), 0, 14)
        self.grid.addWidget(QtGui.QLabel('Color'), 0, 15)        

        vbox.addLayout(self.grid)

        # Create Buttons
        hbox = QtGui.QHBoxLayout()
        addButton = self.createButton('Add')
        delButton = self.createButton('Delete')
        insertButton = self.createButton('Insert')
        hbox.addWidget(addButton)
        hbox.addWidget(delButton)
        hbox.addWidget(insertButton)
        vbox.addLayout(hbox)
        vbox.setAlignment(self.grid, QtCore.Qt.AlignTop)
        vbox.setAlignment(hbox, QtCore.Qt.AlignBottom)
        
        # Set up the scrollbar
        widgetWrapper = QtGui.QWidget()
        widgetWrapper.resize(1200, 440)
        widgetWrapper.setLayout(vbox)
        self.setWidget(widgetWrapper)

        # Connect Signals
        self.connect(addButton, QtCore.SIGNAL('pressed()'), self.addMarker)
        self.connect(delButton, QtCore.SIGNAL('pressed()'), self.removeSelectedMarkers)
        self.connect(insertButton, QtCore.SIGNAL('pressed()'), self.insertMarkers)

    def initValues(self):
        # Delete all markers
        # TODO
        return

    def removeMarker(self, marker):
        widgets = marker.getWidgets()
        for key in list(widgets):
            self.grid.removeWidget(widgets[key])
            widgets[key].hide()

        self.row -= 1

    def removeSelectedMarkers(self):
        """ Delete all of the selected markers from the grid layout"""

        removeList = []
        for marker in self.markerList:
            if marker.isSelected():
                self.removeMarker(marker)
                removeList.append(marker)

                # Update each subsequent marker's entry number, and position in the grid
                for i in range(self.markerList.index(marker) + 1, len(self.markerList)):
                    row = int(self.markerList[i].getEntryNumber())
                    self.markerList[i].setEntryNumber(row - 1)
                    self.moveMarkerRow(row, row-1)

        for marker in removeList:
            if marker.isSelected():
                self.markerList.remove(marker)

    def moveMarkerRow(self, row, newRow):
        """ Move a marker grid item at row to newRow """
        numCols = 16
        for col in range(numCols):
            if self.grid.itemAtPosition(row, col) is not None:
                widget = self.grid.itemAtPosition(row, col).widget()
                self.grid.removeWidget(widget)
            self.grid.addWidget(widget, newRow, col, QtCore.Qt.AlignTop)

    def insertMarkers(self):
        """ For each marker that is selected, create and insert a new marker before it """
        selectedMarkerList = []
        for marker in self.markerList:
            if marker.isSelected():
                selectedMarkerList.append(marker)
                # Update the selected and each subsequent marker's entry number, and position in the grid
                for i in range(self.markerList.index(marker), len(self.markerList)):
                    row = int(self.markerList[i].getEntryNumber())
                    self.markerList[i].setEntryNumber(row + 1)
                    self.moveMarkerRow(row, row + 1)

        # Insert the new markers in the correct positions
        for marker in selectedMarkerList:
            index = self.markerList.index(marker)
            newMarker = QMarkerEditorEntry(self.grid, index + 1)                
            self.markerList.insert(index, newMarker)
            self.row += 1

    def addMarker(self):
        self.markerList.append(QMarkerEditorEntry(self.grid, self.row))
        self.row += 1
        
    def createButton(self, text):
        button = QtGui.QToolButton()
        button.setText(text)
        return button

class QMarkerEditorEntry(QtGui.QWidget):
    def __init__(self, grid, row, parent=None):
        QtGui.QWidget.__init__(self, parent)
        self.widgets = {}

        symbolList = ["dot", "plus", "star", "circle", "cross", "diamond", "triangle_up", "triangle_down", "triangle_left", "triangle_right", "square", "diamond_fill", "triangle_up_fill", "triangle_down_fill", "triangle_left_fill", "triangle_right_fill", "square_fill"]
        lineList = ['None', 'Tail', 'Head', 'Line']
        typeList = ['solid', 'dash', 'dot', 'dash-dot', 'long-dash']
        
        self.widgets['selectedBox'] = QtGui.QCheckBox()
        self.widgets['entryNumber'] = QtGui.QLabel(str(row))
        self.widgets['activeBox'] = QtGui.QCheckBox()
        self.widgets['symbolCombo'] = self.createCombo(symbolList)
        self.widgets['colorCombo1'] = self.createCombo(['TODO'])
        self.widgets['size'] = QtGui.QLineEdit()
        self.widgets['id'] = QtGui.QLineEdit()
        self.widgets['idSize'] = QtGui.QLineEdit()
        self.widgets['x'] = QtGui.QLineEdit()
        self.widgets['y'] = QtGui.QLineEdit()
        self.widgets['idColorCombo'] = self.createCombo(['TODO'])
        self.widgets['idFontCombo'] = self.createCombo(map(str, range(1,10)))
        self.widgets['lineCombo'] = self.createCombo(lineList)
        self.widgets['typeCombo'] = self.createCombo(typeList)
        self.widgets['size2'] = QtGui.QLineEdit()
        self.widgets['colorCombo2'] = self.createCombo(['TODO'])        
        
        grid.addWidget(self.widgets['selectedBox'], row, 0, QtCore.Qt.AlignTop)
        grid.addWidget(self.widgets['entryNumber'], row, 1, QtCore.Qt.AlignTop)
        grid.addWidget(self.widgets['activeBox'], row, 2, QtCore.Qt.AlignTop)
        grid.addWidget(self.widgets['symbolCombo'], row, 3, QtCore.Qt.AlignTop)
        grid.addWidget(self.widgets['colorCombo1'], row, 4, QtCore.Qt.AlignTop)
        grid.addWidget(self.widgets['size'], row, 5, QtCore.Qt.AlignTop)
        grid.addWidget(self.widgets['id'], row, 6, QtCore.Qt.AlignTop)
        grid.addWidget(self.widgets['idSize'], row, 7, QtCore.Qt.AlignTop)
        grid.addWidget(self.widgets['x'], row, 8, QtCore.Qt.AlignTop)
        grid.addWidget(self.widgets['y'], row, 9, QtCore.Qt.AlignTop)
        grid.addWidget(self.widgets['idColorCombo'], row, 10, QtCore.Qt.AlignTop)
        grid.addWidget(self.widgets['idFontCombo'], row, 11, QtCore.Qt.AlignTop)
        grid.addWidget(self.widgets['lineCombo'], row, 12, QtCore.Qt.AlignTop)
        grid.addWidget(self.widgets['typeCombo'], row, 13, QtCore.Qt.AlignTop)
        grid.addWidget(self.widgets['size2'], row, 14, QtCore.Qt.AlignTop)
        grid.addWidget(self.widgets['colorCombo2'], row, 15, QtCore.Qt.AlignTop)

        # TODO initialize the values

    def getWidgets(self):
        return self.widgets

    def getColor2(self):
        return self.widgets['colorCombo2'].currentText()

    def getSize2(self):
        return self.widgets['size2'].currentText()

    def getType(self):
        return self.widgets['typeCombo'].currentText()

    def getLine(self):
        return self.widgets['lineCombo'].currentText()

    def getIdFont(self):
        return self.widgets['idFontCombo'].currentText()

    def getIdColor(self):
        return self.widgets['idColorCombo'].currentText()

    def getSize(self):
        return self.widgets['size'].currentText()

    def getID(self):
        return self.widgets['id'].currentText()

    def getIdSize(self):
        return self.widgets['idSize'].currentText()

    def getX(self):
        return self.widgets['x'].currentText()

    def gety(self):
        return self.widgets['y'].currentText()    

    def getColor(self):
        return self.widgets['colorCombo1'].currentText()

    def getSymbol(self):
        return self.widgets['symbolCombo'].currentText()
        
    def getEntryNumber(self):
        return int(self.widgets['entryNumber'].text())

    def setEntryNumber(self, num):
        self.widgets['entryNumber'].setText(str(num))

    def isActive(self):
        return self.widgets['activeBox'].isChecked()

    def isSelected(self):
        return self.widgets['selectedBox'].isChecked()

    def createCombo(self, entries):
        comboBox = QtGui.QComboBox()
        comboBox.setMaximumWidth(120)
        comboPalette = comboBox.view().palette()
        comboPalette.setColor(QtGui.QPalette.HighlightedText, QtCore.Qt.white)        
        comboPalette.setColor(QtGui.QPalette.Highlight, QtCore.Qt.blue)
        comboBox.view().setPalette(comboPalette)
        
        for entry in entries:
            comboBox.addItem(entry)

        return comboBox

class QTaylorTicksLabels(QtGui.QScrollArea):
    def __init__(self, parent=None, gm=None):
        QtGui.QScrollArea.__init__(self, parent)
        vbox = QtGui.QVBoxLayout()

        vbox = QtGui.QVBoxLayout()
        xFrame = QFramedWidget('X axis')
        self.xlabels = xFrame.addLabeledLineEdit('Labels:')
        self.xticks = xFrame.addLabeledLineEdit('Ticks:')
        vbox.addWidget(xFrame)

        yFrame = QFramedWidget('Y axis')
        self.ylabels = yFrame.addLabeledLineEdit('Labels:')
        self.yticks = yFrame.addLabeledLineEdit('Ticks:')
        vbox.addWidget(yFrame)

        corArcFrame = QFramedWidget('Correlation arc')
        self.corLabels = corArcFrame.addLabeledLineEdit('Labels:')
        self.corTicks = corArcFrame.addLabeledLineEdit('Ticks:')
        vbox.addWidget(corArcFrame)

        # Init line edits
        self.initValues()

        # Set up the scrollbar
        wrapperWidget = QtGui.QWidget()
        wrapperWidget.setLayout(vbox)
        wrapperWidget.setMinimumWidth(580)        
        self.setWidget(wrapperWidget)

    def initValues(self):
        self.xlabels.setText('*')
        self.xticks.setText('*')
        self.ylabels.setText('*')
        self.yticks.setText('*')
        self.corLabels.setText('*')
        self.corTicks.setText('*')        

class QTaylorInterfaceTab(QtGui.QScrollArea):
    def __init__(self, parent=None, gm=None):
        QtGui.QScrollArea.__init__(self, parent)
        vbox = QtGui.QVBoxLayout()

        # General Aspect
        genFrame = QFramedWidget('General Aspect')
        self.detailSlider = genFrame.addLabeledSlider('Detail:', newRow=False)
        self.maxValue = genFrame.addLabeledLineEdit('Maximum Value:')
        self.quadran = genFrame.addRadioFrame('Quadran', ['1', '2'])
        self.refValue = genFrame.addLabeledLineEdit('Reference Value:')
        vbox.addWidget(genFrame)

        # Skill
        skillFrame = QFramedWidget('Skill')
        self.skillValues = skillFrame.addLabeledLineEdit('Skill Values:')
        self.skillLineColor = skillFrame.addLabeledComboBox('Skill Lines Color:',
                                                            ['TODO'])
        self.drawLabels = skillFrame.addCheckBox('Draw Skill Contour Labels')
        self.skillCoefficients = skillFrame.addLabeledLineEdit('Skill Coefficients:')
        vbox.addWidget(skillFrame)
        
        # Arrows
        arrowFrame = QFramedWidget('Arrows')
        self.lengthSlider = arrowFrame.addLabeledSlider('Length:', newRow=False)
        self.angleSlider = arrowFrame.addLabeledSlider('Angle:')
        self.baseSlider = arrowFrame.addLabeledSlider('Base:')
        # TODO - Create a widget to draw the arrow
        
        vbox.addWidget(arrowFrame)
        vbox.setAlignment(arrowFrame, QtCore.Qt.AlignTop)

        # Set up the scrollbar
        wrapperWidget = QtGui.QWidget()
        wrapperWidget.setLayout(vbox)
        wrapperWidget.setMinimumWidth(580)
        self.setWidget(wrapperWidget)        

        # Init Values
        self.initValues()

        # Connect Signals
        self.connect(self.lengthSlider, QtCore.SIGNAL('valueChanged(int)'), self.lengthChangedEvent)
        self.connect(self.angleSlider, QtCore.SIGNAL('valueChanged(int)'), self.angleChangedEvent)
        self.connect(self.baseSlider, QtCore.SIGNAL('valueChanged(int)'), self.baseChangedEvent)        

    def initValues(self):
        # TODO Init w/ non hardcoded values?

        # General Aspect
        self.detailSlider.setTickPosition(QtGui.QSlider.TicksBelow)
        self.detailSlider.setMinimum(0)
        self.detailSlider.setMaximum(100)
        self.detailSlider.setSingleStep(5)
        self.detailSlider.setTickInterval(10)

        self.maxValue.setText('None')
        self.refValue.setText('1.0')
        self.quadran.setChecked('1')

        # Arrows
        self.lengthSlider.setTickPosition(QtGui.QSlider.TicksBelow)
        self.angleSlider.setTickPosition(QtGui.QSlider.TicksBelow)
        self.baseSlider.setTickPosition(QtGui.QSlider.TicksBelow)

    def lengthChangedEvent(self, int):
        return # TODO

    def angleChangedEvent(self, int):
        return # TODO

    def baseChangedEvent(self, int):
        return # TODO

class QScatterEditor(QtGui.QScrollArea):
    def __init__(self, parent=None, gm=None):
        QtGui.QScrollArea.__init__(self, parent)
        vbox = QtGui.QVBoxLayout()

        # Create Widgets
        frame = QFramedWidget()
        markerTypes = ["dot", "plus", "star", "circle", "cross", "diamond", "triangle_up",
                       "triangle_down", "triangle_left", "triangle_right", "square",
                       "diamond_fill", "triangle_up_fill", "triangle_down_fill",
                       "triangle_left_fill", "triangle_right_fill", "square_fill"]
        self.markerType = frame.addLabeledComboBox('Scatter Marker Type:',
                                                   markerTypes, indent=False)
        self.colorIndex = frame.addLabeledSpinBox('Scatter Marker Color Index',
                                                  0, 255, indent=False)
        self.markerSize = frame.addLabeledSpinBox('Scatter Marker Size',
                                                  1, 300, indent=False)

        self.initValues()

        # Set toolTips
        self.markerType.setToolTip("Select the scatter marker type. ")
        self.colorIndex.setToolTip("Select the scatter marker color index. ")
        self.markerSize.setToolTip("Select the scatter marker size. ")

        vbox.addWidget(frame)
        vbox.setAlignment(frame, QtCore.Qt.AlignTop)

        # Set up the scrollbar
        widgetWrapper = QtGui.QWidget()
        widgetWrapper.setMinimumWidth(580)        
        widgetWrapper.setLayout(vbox)
        self.setWidget(widgetWrapper)

    def initValues(self):
        #  TODO: init w/ non-hardcoded values?        
        self.colorIndex.setValue(241)
        self.markerSize.setValue(3)        

class QOutlineEditor(QtGui.QScrollArea):
    def __init__(self, parent=None, gm=None):
        QtGui.QScrollArea.__init__(self, parent)
        vbox = QtGui.QVBoxLayout()

        # Create Widgets
        frame = QFramedWidget()
        self.lineType = frame.addLabeledComboBox('Outfill Fill Area Style:',
                                                 ['solid', 'dash', 'dot', 'dash-dot', 'long-dash'],
                                                 indent=False)
        self.lineColorIndex = frame.addLabeledSpinBox('Outfill Line Color Index:',
                                                      0, 255, indent=False)
        self.indexValues = frame.addLabeledLineEdit('Outfill Index Values:',
                                                    indent=False)

        # Init Values - TODO: init w/ non-hardcoded values?
        self.lineColorIndex.setValue(241)
        self.indexValues.setText('1')

        # Set tool tips
        self.lineType.setToolTip("Select the outline line type. ")
        self.lineColorIndex.setToolTip("Enter the line color index value. There can only\nbe one color index value (ranging from 0 to 255).\nIf an error in the color index value occurs, then the\ndefault color value index (i.e., 241) will be used.")
        self.indexValues.setToolTip("Outlines are drawn to enclose the specified values\nin the data array. As few as one, or\nas many as\nten values, can be specified:\noutline=([n1,[n2,[n3,...[n10]...]]]).")        

        vbox.addWidget(frame)
        vbox.setAlignment(frame, QtCore.Qt.AlignTop)

        # Set up the scrollbar
        widgetWrapper = QtGui.QWidget()
        widgetWrapper.setMinimumWidth(580)        
        widgetWrapper.setLayout(vbox)
        self.setWidget(widgetWrapper)        

class QOutfillEditor(QtGui.QScrollArea):
    def __init__(self, parent=None, gm=None):
        QtGui.QScrollArea.__init__(self, parent)
        vbox = QtGui.QVBoxLayout()

        # Create Widgets
        frame = QFramedWidget()
        self.fillArea = frame.addLabeledComboBox('Outfill Fill Area Style:',
                                                 ['Solid', 'Hatch', 'Pattern', 'Hallow'],
                                                 indent=False)
        self.fillAreaIndex = frame.addLabeledSpinBox('Outfill Fill Area Index:',
                                                     1, 20, indent=False)
        self.fillColorIndex = frame.addLabeledSpinBox('Outfill Fill Area Color Index:',
                                                      0, 255, indent=False)
        self.indexValues = frame.addLabeledLineEdit('Outfill Index Values:',
                                                    indent=False)
        vbox.addWidget(frame)
        vbox.setAlignment(frame, QtCore.Qt.AlignTop)

        self.initValues()

        # Set ToolTips
        self.fillArea.setToolTip("Select the outfill fill area style type. ")
        self.fillAreaIndex.setToolTip("Select the outfill fill area index value. ")
        self.fillColorIndex.setToolTip("Enter the fillarea color index value. There can only\nbe one color index value (ranging from 0 to 255).\nIf an error in the color index value occurs, then the\ndefault color value index (i.e., 241) will be used.")
        self.indexValues.setToolTip("Outlines are filled to enclose the selected values\nthat appear in the data array. As few as one, or\nas many as ten values, can be specified:\noutline=([n1,[n2,[n3,...[n10]...]]]).")

        # Set up the scrollbar
        widgetWrapper = QtGui.QWidget()
        widgetWrapper.setMinimumWidth(580)        
        widgetWrapper.setLayout(vbox)
        self.setWidget(widgetWrapper)

    def initValues(self):
        # TODO - init w/ non hardcoded values?
        self.fillAreaIndex.setValue(1)
        self.fillColorIndex.setValue(241)
        self.indexValues.setText('1')
        
class Q1DPlotEditor(QtGui.QScrollArea):

    def __init__(self, parent=None, gm=None):
        QtGui.QScrollArea.__init__(self, parent)
        vbox = QtGui.QVBoxLayout()

        frame = QFramedWidget()
        frame.addWidget(QtGui.QLabel('Define up to 15 values:'), QtCore.Qt.AlignTop)
        self.lineTypes = frame.addLabeledLineEdit('Line Types: ')
        self.lineColors = frame.addLabeledLineEdit('Line Colors: ')
        self.lineWidths = frame.addLabeledLineEdit('Line Widths: ')
        self.markerTypes = frame.addLabeledLineEdit('Marker Types: ')
        self.markerColors = frame.addLabeledLineEdit('Marker Colors: ')
        self.markerSizes = frame.addLabeledLineEdit('Marker Sizes: ')

        self.initValues()
        self.lineTypes.setToolTip("Set the line types. The line values can either be\n('solid', 'dash', 'dot', 'dash-dot', 'long-dash')\nor (0, 1, 2, 3, 4) or None")
        self.lineColors.setToolTip("Set the line colors. The line color attribute\n values must be integers ranging from 0 to 255.\n(e.g., 16, 32, 48, 64) ")
        self.lineWidths.setToolTip("Set the line width. The line width is an integer\nor float value in the range (1 to 100)")
        self.markerTypes.setToolTip("Set the marker types. The marker values can either\nbe (None, 'dot', 'plus', 'star', 'circle', 'cross', 'diamond',\n'triangle_up', 'triangle_down', 'triangle_left',\n'triangle_right', 'square', 'diamond_fill',\n'triangle_up_fill', 'triangle_down_fill',\n'triangle_left_fill', 'triangle_right_fill',\n'square_fill') or (0, 1, 2, 3, 4, 5, 6, 7, 8, 9,\n10, 11, 12, 13, 14, 15, 16, 17) or None. ")
        self.markerColors.setToolTip("Set the marker colors. The marker color attribute\nvalues must be integers ranging from 0 to 255.")
        self.markerSizes.setToolTip("Set the marker sizes. The marker size attribute\nvalues must be integers or floats ranging  from\n1 to 300. " )

        vbox.addWidget(frame)
        vbox.setAlignment(frame, QtCore.Qt.AlignTop)

        # Set up the scrollbar
        widgetWrapper = QtGui.QWidget()
        widgetWrapper.setMinimumWidth(580)
        widgetWrapper.setLayout(vbox)
        self.setWidget(widgetWrapper)

    def initValues(self):
        # TODO ?
        self.lineTypes.setText('None')
        self.lineColors.setText('None')
        self.lineWidths.setText('None')
        self.markerTypes.setText('None')
        self.markerColors.setText('None')
        self.markerSizes.setText('None')

class QMeshfillEditor(QtGui.QScrollArea):

    def __init__(self, parent=None, gm=None):
        QtGui.QWidget.__init__(self, parent)
        
        if type(gm) in [types.ListType, types.TupleType, types.NoneType]:
            self.gm = gm
        else:
            self.gm=[gm,]

        vbox = QtGui.QVBoxLayout()
    
        # General Settings
        genSettings = QFramedWidget('General Settings')
        self.missing = genSettings.addLabeledLineEdit('Missing:')
        self.xWrap = genSettings.addLabeledLineEdit('X Wrap:')
        self.yWrap = genSettings.addLabeledLineEdit('Y Wrap:', newRow = False)
        self.genLegend = genSettings.addLabeledLineEdit('Legend Labels')
        self.showMesh = genSettings.addRadioFrame('Show Mesh:', ['No', 'Yes'])
        self.ext1 = genSettings.addRadioFrame('Ext 1:', ['No', 'Yes'], newRow=False)
        self.ext2 = genSettings.addRadioFrame('Ext 2:', ['No', 'Yes'], newRow=False)
        vbox.addWidget(genSettings)

        # Custom Settings
        custSettings = QFramedWidget('Custom Settings')
        custSettings.addLabel('Define iso level range values')
        self.incZero = custSettings.addRadioFrame('Include Zero:', ['Off', 'On'],
                                                  newRow=False)
        self.ranges = custSettings.addLabeledLineEdit('Ranges:')
        self.colors = custSettings.addLabeledLineEdit('Colors:')
        self.lineTypes = custSettings.addLabeledLineEdit('Line Types:')
        self.lineWidths = custSettings.addLabeledLineEdit('Line Widths:')
        self.orientation = custSettings.addLabeledLineEdit('Orientation:')
        self.arrowScale = custSettings.addLabeledLineEdit('Contour Arrows Scale:')
        self.arrowSpacing = custSettings.addLabeledLineEdit('Contour Arrows Spacing:')
        self.arrowAngle = custSettings.addLabeledLineEdit('Contour Arrows Angle:')
        self.custLegend = custSettings.addLabeledLineEdit('Legend Labels:')
        self.isolineLabels = custSettings.addRadioFrame('Isoline Labels:',
                                                        ['Off', 'On'])
        custSettings.newRow()
        custSettings.addLabel('Define iso level parameters:')
        self.spacing = custSettings.addRadioFrame('Spacing:',
                                                  ['Linear', 'Log'],
                                                  newRow=False)
        self.minVal = custSettings.addLabeledLineEdit('Minimum Value:')
        self.maxVal = custSettings.addLabeledLineEdit('Maximum Value:')
        self.nIntervals = custSettings.addLabeledSpinBox('Number of Intervals:',
                                                         2, 223)
        self.smallestExpLabel, self.smallestExp = custSettings.addLabelAndLineEdit('Smallest Exponent for Negative Values:')
        self.numNegDecLabel, self.numNegDec = custSettings.addLabelAndLineEdit('Number of Negative Decades:')
        genRangesButton = custSettings.addButton('Generate Ranges')
        clearButton = custSettings.addButton('Clear All', newRow=False)
        vbox.addWidget(custSettings)

        # Init values / tool tips
        self.initValues()
        self.setToolTips()

        # Set up the scrollbar
        widgetWrapper = QtGui.QWidget()
        widgetWrapper.setLayout(vbox)
        self.setWidget(widgetWrapper)        

        # Connect Signals
        self.connect(clearButton, QtCore.SIGNAL('pressed()'),
                     custSettings.clearAllLineEdits)
        self.connect(self.spacing.getButton('Linear'),
                     QtCore.SIGNAL('pressed()'),
                     lambda : self.setEnabledLogLineEdits(False))
        self.connect(self.spacing.getButton('Log'),
                     QtCore.SIGNAL('pressed()'),
                     lambda : self.setEnabledLogLineEdits(True))
        self.connect(genRangesButton, QtCore.SIGNAL('pressed()'),
                     self.generateRanges)        

    def initValues(self):
        # TODO : init missing, x wrap, y wrap, legend labels

        self.showMesh.setChecked('No')
        self.ext1.setChecked('No')
        self.ext2.setChecked('No')
        self.incZero.setChecked('Off')
        self.spacing.setChecked('Linear')
        self.setEnabledLogLineEdits(False)

    def generateRanges(self):
        try:
            minValue = float(self.minVal.text())
            maxValue = float(self.maxVal.text())
            numIntervals = int(self.nIntervals.text())
        except:
            QSimpleMessageBox('Values must be a number', self).show()
            return
        
        if numIntervals < 2 or numIntervals > 223:
            QSimpleMessageBox("The 'number of intervals' value must be between 2 and 223.",
                              self).show()
            return

        colors = []
        values = []                
        value = minValue
        color = 16

        # Generate ranges and colors (Linear)
        if self.spacing.isChecked('Linear'):
            delta = float((maxValue - minValue) / numIntervals)
            d = int(222 / (numIntervals - 1))
            
            for a in range(numIntervals + 1):
                if color <= 238:
                    colors.append(color)
                values.append(value)
                color += d
                value += delta
        # Generate ranges (Log)                
        else:
            A = float(self.minVal.text())
            B = float(self.maxVal.text())
            C = float(self.nIntervals.text())
            try:
                D = float(self.smallestExp.text())
            except:
                D = 0
            try:
                E = float(self.numNegDec.text())
            except:
                E = 0

            if C > 0:
                if E > 0:  # Generate negative contours
                    for i in range(int(E * C), 0, -1):
                        values.append(round_number(-10.0 ** (D+(i-1)/C)))
                if B > 0:  # Generate positive contours
                    for i in range(1, int((B * C) + 1)):
                        values.append(round_number(10.0 ** (A+(i-1)/C)))
            else:
                QSimpleMessageBox("The 'Levels per Decade' must be a positive number.").show()

            # Gen colors (Log)
            numIntervals = len(values) - 1
            d = int(222 / (numIntervals - 1))
            for a in range(numIntervals):
                colors.append(16 + a * d)
            
        if self.incZero.isChecked('On'):
            values.insert(0, 0.0)

        self.ranges.setText(str(values))
        self.colors.setText(str(colors))

    def setEnabledLogLineEdits(self, enable):
        """ Disable or Enable the 'Num neg decades' and 'smallest exp for neg
        values' lineEdits """
        self.smallestExp.setEnabled(enable)
        self.numNegDec.setEnabled(enable)
        self.smallestExpLabel.setEnabled(enable)
        self.numNegDecLabel.setEnabled(enable)

        if enable == True:
            self.smallestExp.setToolTip("Smallest exponent for negative values")
            self.numNegDec.setToolTip("Number of negative decades.")
        else:
            self.smallestExp.setToolTip("Disabled. Not in use for linear spacing.")
            self.numNegDec.setToolTip("Disabled. Not in use for linear spacing.")                    

    def setToolTips(self):
        # General Setting Tips
        self.missing.setToolTip("Set the missing color index value. The colormap\nranges from 0 to 255, enter the desired color index value 0\nthrough 255.")
        self.xWrap.setToolTip("Set the wrapping along X axis, 0 means no wrapping")
        self.yWrap.setToolTip("Set the wrapping along Y axis, 0 means no wrapping")
        self.genLegend.setToolTip("Specify the desired legend labels.\nFor example:\nNone -- Allow VCS to generate legend labels\n( ), or [ ], or { } -- No legend labels\n[ 0, 10, 20 ]  or  { 0:'0', 10:'10', 20:'20' }\n[ 0, 10 ]  or  { 0:'text', 10:'more text' }")        
        
        # Custom Setting Tips
        self.ranges.setToolTip("The iso level range values. (e.g., 10, 20, 30, 40, 50).")
        self.colors.setToolTip("The iso level color index values. The index colors range\nfrom 0 to 255. For example:\n   Use explicit indices: 16, 32, 48, 64, 80;\n   Use two values to generate index range: 16, 32")
        self.lineTypes.setToolTip("The line type for the isolines. (e.g., 4, 1, 3, 2, 0).")
        self.lineWidths.setToolTip("The width values for the isolines. (e.g., 1, 3, 5, 2, 7).")
        self.orientation.setToolTip("Drawing orientation arrows:\n none (0)\n clokwise (1)\n clockwise where y axis is positive (2)\n clockwise where x axis is positive(3)\n Negative values indicate counter-clockwise.")
        self.arrowScale.setToolTip("Scale factor for arrows length")
        self.arrowSpacing.setToolTip("Spacing factor for arrows")
        self.arrowAngle.setToolTip("Angle of Arrows heads")
        self.isolineLabels.setToolTip("Toggle 'Isoline Labels' on or off.")
        self.custLegend.setToolTip("Specify the desired legend labels.\nFor example:\nNone -- Allow VCS to generate legend labels\n( ), or [ ], or { } -- No legend labels\n[ 0, 10, 20 ]  or  { 0:'0', 10:'10', 20:'20' }\n[ 0, 10 ]  or  { 0:'text', 10:'more text' }")
        self.minVal.setToolTip("The minimum contour level.")
        self.maxVal.setToolTip("The maximum contour level.")
        self.nIntervals.setToolTip("The number of intervals between each contour level. Maximum number range [2 to 223].")
        self.smallestExp.setToolTip("Disabled. Not in use for linear spacing.")
        self.numNegDec.setToolTip("Disabled. Not in use for linear spacing.")     

class QContourEditor(QtGui.QScrollArea):
    def __init__(self, parent=None, gm=None):
        QtGui.QScrollArea.__init__(self, parent)
        vbox = QtGui.QVBoxLayout()

        #### TODO gm stuff

        frame = QFramedWidget()

        frame.addWidget(QtGui.QLabel('Define iso level range values:'))
        self.includeZeroButtonGroup = frame.addRadioFrame('Include Zero:',
                                                          ['Off', 'On'],
                                                          newRow=False)
        self.ranges = frame.addLabeledLineEdit('Ranges:')
        self.colors = frame.addLabeledLineEdit('Colors:')
        self.lineTypes = frame.addLabeledLineEdit('Line Types:', )
        self.lineWidths = frame.addLabeledLineEdit('Line Widths:')
        self.orientation = frame.addLabeledLineEdit('Orientation:')
        self.arrowScale = frame.addLabeledLineEdit('Contour Arrows Scale:')
        self.arrowSpacing = frame.addLabeledLineEdit('Contour Arrows Spacing:')
        self.angle = frame.addLabeledLineEdit('Contour Arrows Angle:')
        self.labelButtonGroup = frame.addRadioFrame('Isoline Labels:',
                                                    ['Off', 'On'])
        self.legendLabels = frame.addLabeledLineEdit('Legend Labels')
        frame.newRow()
        frame.addWidget(QtGui.QLabel('Define iso level parameters:'))
        self.spacingButtonGroup = frame.addRadioFrame('Spacing:',
                                                      ['Linear', 'Log'],
                                                      newRow=False)
        self.minVal = frame.addLabeledLineEdit('Minimum Value:')
        self.maxVal = frame.addLabeledLineEdit('Maximum Value:')
        self.nIntervals = frame.addLabeledSpinBox('Number of Intervals:',
                                                  2, 223,)
        self.smallestExpLabel, self.smallestExp = frame.addLabelAndLineEdit('Smallest Exponent for Negative Values:')
        self.numNegDecLabel, self.numNegDec = frame.addLabelAndLineEdit('Number of Negative Decades:')
        genRangesButton = frame.addButton('Generate Ranges')
        clearButton = frame.addButton('Clear All', newRow=False)

        vbox.addWidget(frame)
        self.frame = frame

        # Init values / tool tips
        self.initValues()
        self.setToolTips()

        # Set up the scrollbar
        widgetWrapper = QtGui.QWidget()
        widgetWrapper.setLayout(vbox)
        widgetWrapper.setMinimumWidth(630)        
        self.setWidget(widgetWrapper)        

        # Connect Signals
        self.connect(clearButton, QtCore.SIGNAL('pressed()'),
                     self.frame.clearAllLineEdits)
        self.connect(genRangesButton, QtCore.SIGNAL('pressed()'),
                     self.generateRanges)
        self.connect(self.spacingButtonGroup.getButton('Linear'),
                     QtCore.SIGNAL('pressed()'),
                     lambda : self.setEnabledLogLineEdits(False))
        self.connect(self.spacingButtonGroup.getButton('Log'),
                     QtCore.SIGNAL('pressed()'),
                     lambda : self.setEnabledLogLineEdits(True))

    def initValues(self):
        self.includeZeroButtonGroup.setChecked('Off')
        self.spacingButtonGroup.setChecked('Linear')
        self.setEnabledLogLineEdits(False)

        self.minVal.setText('1.0')
        self.maxVal.setText('2.0')
        self.nIntervals.setValue(2)

    def generateRanges(self):
        try:
            minValue = float(self.minVal.text())
            maxValue = float(self.maxVal.text())
            numIntervals = int(self.nIntervals.text())
        except:
            QSimpleMessageBox('Values must be a number', self).show()
            return
        
        if numIntervals < 2 or numIntervals > 223:
            QSimpleMessageBox("The 'number of intervals' value must be between 2 and 223.",
                              self).show()
            return

        colors = []
        values = []                
        value = minValue
        color = 16

        # Generate ranges and colors (Linear)
        if self.spacingButtonGroup.isChecked('Linear'):
            delta = float((maxValue - minValue) / numIntervals)
            d = int(222 / (numIntervals - 1))
            
            for a in range(numIntervals + 1):
                if color <= 238:
                    colors.append(color)
                values.append(value)
                color += d
                value += delta
        # Generate ranges (Log)                
        else:
            A = float(self.minVal.text())
            B = float(self.maxVal.text())
            C = float(self.nIntervals.text())
            try:
                D = float(self.smallestExp.text())
            except:
                D = 0
            try:
                E = float(self.numNegDec.text())
            except:
                E = 0

            if C > 0:
                if E > 0:  # Generate negative contours
                    for i in range(int(E * C), 0, -1):
                        values.append(round_number(-10.0 ** (D+(i-1)/C)))
                if B > 0:  # Generate positive contours
                    for i in range(1, int((B * C) + 1)):
                        values.append(round_number(10.0 ** (A+(i-1)/C)))
            else:
                QSimpleMessageBox("The 'Levels per Decade' must be a positive number.").show()

            # Gen colors (Log)
            numIntervals = len(values) - 1
            d = int(222 / (numIntervals - 1))
            for a in range(numIntervals):
                colors.append(16 + a * d)
            
        if self.includeZeroButtonGroup.isChecked('On'):
            values.insert(0, 0.0)

        self.ranges.setText(str(values))
        self.colors.setText(str(colors))

    def setEnabledLogLineEdits(self, enable):
        """ Disable or Enable the 'Num neg decades' and 'smallest exp for neg
        values' lineEdits """        
        self.smallestExp.setEnabled(enable)
        self.numNegDec.setEnabled(enable)
        self.smallestExpLabel.setEnabled(enable)
        self.numNegDecLabel.setEnabled(enable)

        if enable == True:
            self.smallestExp.setToolTip("Smallest exponent for negative values")
            self.numNegDec.setToolTip("Number of negative decades.")
        else:
            self.smallestExp.setToolTip("Disabled. Not in use for linear spacing.")
            self.numNegDec.setToolTip("Disabled. Not in use for linear spacing.")            

    def setToolTips(self):
        self.ranges.setToolTip("The iso level range values. (e.g., 10, 20, 30, 40, 50).")
        self.colors.setToolTip("The iso level color index values. The index colors range\nfrom 0 to 255. For example:\n   Use explicit indices: 16, 32, 48, 64, 80;\n   Use two values to generate index range: 16, 32")
        self.lineTypes.setToolTip("The line type for the isolines. (e.g., 4, 1, 3, 2, 0).")
        self.lineWidths.setToolTip("The width values for the isolines. (e.g., 1, 3, 5, 2, 7).")
        self.orientation.setToolTip("Drawing orientation arrows:\n none (0)\n clokwise (1)\n clockwise where y axis is positive (2)\n clockwise where x axis is positive(3)\n Negative values indicate counter-clockwise.")
        self.arrowScale.setToolTip("Scale factor for arrows length")
        self.arrowSpacing.setToolTip("Spacing factor for arrows")
        self.angle.setToolTip("Angle of Arrows heads")
        self.labelButtonGroup.setToolTip("Toggle 'Isoline Labels' on or off.")
        self.legendLabels.setToolTip("Specify the desired legend labels.\nFor example:\nNone -- Allow VCS to generate legend labels\n( ), or [ ], or { } -- No legend labels\n[ 0, 10, 20 ]  or  { 0:'0', 10:'10', 20:'20' }\n[ 0, 10 ]  or  { 0:'text', 10:'more text' }")
        self.minVal.setToolTip("The minimum contour level.")
        self.maxVal.setToolTip("The maximum contour level.")
        self.nIntervals.setToolTip("The number of intervals between each contour level. Maximum number range [2 to 223].")
        self.smallestExp.setToolTip("Disabled. Not in use for linear spacing.")
        self.numNegDec.setToolTip("Disabled. Not in use for linear spacing.")        

class QContinentsEditor(QtGui.QScrollArea):
    def __init__(self, parent=None, gm=None):
        QtGui.QScrollArea.__init__(self, parent)
        vbox = QtGui.QVBoxLayout()
        frame = QFramedWidget()
        
        self.lineTypeComboBox = frame.addLabeledComboBox('Continents Line Type:',
                                                         ['solid', 'dash', 'dot', 'dash-dot', 'long-dash'])
        self.lineColorIndex = frame.addLabeledSpinBox('Continents Line Color Index:',
                                                      0, 255)
        self.lineWidth = frame.addLabeledSpinBox('Continents Line Width:', 1, 300)
        self.typeIndex = frame.addLabeledSpinBox('Continents Type Index:', -1, 11)
        vbox.addWidget(frame)

        # Init values / tool tips
        self.initValues()
        self.setToolTips()

        # Set up the scrollbar
        widgetWrapper = QtGui.QWidget()
        widgetWrapper.setMinimumWidth(580)
        widgetWrapper.setLayout(vbox)
        self.setWidget(widgetWrapper)

    def initValues(self):
        self.lineColorIndex.setValue(0)
        self.lineWidth.setValue(1)
        self.typeIndex.setValue(-1)

    def setToolTips(self):
        self.lineTypeComboBox.setToolTip('Select the continents line type.')
        self.lineColorIndex.setToolTip('Select the continents color index.')
        self.lineWidth.setToolTip('Set the continents line width.')
        self.typeIndex.setToolTip("Select the continents type index.Where\n-1 = 'Auto Continents';\n0 = 'No Continents';\n1 = 'Fine Continents';\n2 = 'Coarse Continents';\n3 = 'United States Continents';\n4 = 'Political Borders Continents';\n5 = 'Rivers Continents';\n6 - 11 = 'User defined Continents'")

    def getLineType(self):
        return self.lineTypeComboBox.currentText()
        
    def getTypeIndex(self):
        try:
            return int(self.typeIndex.text())
        except:
            return None

    def getLineWidth(self):
        try:
            return int(self.lineWidth.text())
        except:
            return None

    def getLineColorIndex(self):
        try:
            return int(self.lineColorIndex.text())
        except:
            return None        
        
class QBoxfillEditor(QtGui.QScrollArea):

    def __init__(self, parent=None, gm=None):
        QtGui.QScrollArea.__init__(self, parent)
        vbox = QtGui.QVBoxLayout()
        
        if type(gm) in [types.ListType, types.TupleType, types.NoneType]:
            self.gm = gm
        elif type(gm) is not None:
            self.gm=[gm,]

        # 'define boxfill attributes + boxfill type radio buttons'
        hbox1 = QtGui.QHBoxLayout()
        hbox1.addWidget(QtGui.QLabel('Define boxfill attribute values'))

        self.boxfillTypeButtonGroup = QRadioButtonFrame('Boxfill type:')
        self.boxfillTypeButtonGroup.addButtonList(['linear', 'log10', 'custom'])
        hbox1.addWidget(self.boxfillTypeButtonGroup)
                                             
        vbox.addLayout(hbox1)

        # General Settings 
        generalSettings = QFramedWidget('General Settings')
        self.missingLineEdit = generalSettings.addLabeledLineEdit('Missing:')
        self.ext1ButtonGroup = generalSettings.addRadioFrame('Ext1:',
                                                             ['No', 'Yes'],
                                                             newRow = False)
        self.ext2ButtonGroup = generalSettings.addRadioFrame('Ext2:',
                                                             ['No', 'Yes'],
                                                             newRow = False)
        self.legendLineEdit = generalSettings.addLabeledLineEdit('Legend Labels:')
        vbox.addWidget(generalSettings)
        
        # Linear & Log Settings
        linLogSettings = QFramedWidget('Linear and Log Settings')

        self.level1LineEdit = linLogSettings.addLabeledLineEdit('Level 1:')
        self.level2LineEdit = linLogSettings.addLabeledLineEdit('Level 2:',
                                                                newRow = False)
        self.color1LineEdit = linLogSettings.addLabeledLineEdit('Color 1:')
        self.color2LineEdit = linLogSettings.addLabeledLineEdit('Color 2:',
                                                                newRow = False)        
        vbox.addWidget(linLogSettings)
        
        # Custom Settings
        customSettings = QFramedWidget('Custom Settings')

        customSettings.addWidget(QtGui.QLabel('Define iso level range values:'))
        self.includeZeroButtonGroup = customSettings.addRadioFrame('Include Zero:',
                                                                   ['Off', 'On'],
                                                                   newRow=False)
        self.rangeLineEdit = customSettings.addLabeledLineEdit('Ranges:')
        self.colorsLineEdit = customSettings.addLabeledLineEdit('Colors:')

        customSettings.newRow()
        customSettings.addWidget(QtGui.QLabel('Define iso level parameters:'))
        self.spacingButtonGroup = customSettings.addRadioFrame('spacing:',
                                                               ['Linear', 'Log'],
                                                               newRow=False)

        self.minValLineEdit = customSettings.addLabeledLineEdit('Minimum Value:')
        self.maxValLineEdit = customSettings.addLabeledLineEdit('Maximum Value:')
        self.nIntervals = customSettings.addLabeledSpinBox('Number of Intervals:',
                                                                   2, 223)
        self.smallestExpLabel, self.expLineEdit = customSettings.addLabelAndLineEdit('Smallest Exponent for Negative Values:')
        self.numNegDecLabel, self.negDecadesLineEdit = customSettings.addLabelAndLineEdit('Number of Negative Decades:')
        genRangesButton = customSettings.addButton('Generate Ranges')
        clearButton = customSettings.addButton('Clear All', newRow=False)        
        vbox.addWidget(customSettings)

        # Init values
        self.initValues()
        self.setToolTips()

        # Set up the scrollbar
        widgetWrapper = QtGui.QWidget()
        widgetWrapper.setLayout(vbox)
        self.setWidget(widgetWrapper)

        # Connect Signals
        self.connect(clearButton, QtCore.SIGNAL('pressed()'), self.clearCustomSettings)
        self.connect(genRangesButton, QtCore.SIGNAL('pressed()'), self.generateRanges)
        self.connect(self.spacingButtonGroup.getButton('Linear'),
                     QtCore.SIGNAL('pressed()'),
                     lambda : self.setEnabledLogLineEdits(False))
        self.connect(self.spacingButtonGroup.getButton('Log'),
                     QtCore.SIGNAL('pressed()'),
                     lambda : self.setEnabledLogLineEdits(True))

    def initValues(self):
        if self.gm is None:
            return

        # Init Line Edit Text
        self.missingLineEdit.setText(str(self.gm[0].missing))
        self.legendLineEdit.setText('None')
        self.level1LineEdit.setText(str(self.gm[0].level_1))
        self.level2LineEdit.setText(str(self.gm[0].level_2))
        self.color1LineEdit.setText(str(self.gm[0].color_1))
        self.color2LineEdit.setText(str(self.gm[0].color_2))
        self.rangeLineEdit.setText(str(self.gm[0].levels))
        self.colorsLineEdit.setText('None')
        self.minValLineEdit.setText('')
        self.maxValLineEdit.setText('')
        self.expLineEdit.setText('')
        self.negDecadesLineEdit.setText('')
        self.nIntervals.setValue(2)
        self.setEnabledLogLineEdits(False)

        # Init selected radio buttons
        self.boxfillTypeButtonGroup.setChecked('linear')
        self.ext1ButtonGroup.setChecked('No')
        self.ext2ButtonGroup.setChecked('No')
        self.includeZeroButtonGroup.setChecked('Off')
        self.spacingButtonGroup.setChecked('Linear')

    def generateRanges(self):
        try:
            minValue = float(self.minValLineEdit.text())
            maxValue = float(self.maxValLineEdit.text())
            numIntervals = int(self.nIntervals.text())
        except:
            QSimpleMessageBox('Values must be a number', self).show()
            return
        
        if numIntervals < 2 or numIntervals > 223:
            QSimpleMessageBox("The 'number of intervals' value must be between 2 and 223.",
                              self).show()
            return

        colors = []
        values = []                
        value = minValue
        color = 16

        # Generate ranges and colors (Linear)
        if self.spacingButtonGroup.isChecked('Linear'):
            delta = float((maxValue - minValue) / numIntervals)
            d = int(222 / (numIntervals - 1))
            
            for a in range(numIntervals + 1):
                if color <= 238:
                    colors.append(color)
                values.append(value)
                color += d
                value += delta
        # Generate ranges (Log)                
        else:
            A = float(self.minValLineEdit.text())
            B = float(self.maxValLineEdit.text())
            C = float(self.nIntervals.text())

            try:
                D = float(self.expLineEdit.text())
            except:
                D = 0
            try:
                E = float(self.negDecadesLineEdit.text())
            except:
                E = 0

            if C > 0:
                if E > 0:  # Generate negative contours
                    for i in range(int(E * C), 0, -1):
                        values.append(round_number(-10.0 ** (D+(i-1)/C)))
                if B > 0:  # Generate positive contours
                    for i in range(1, int((B * C) + 1)):
                        values.append(round_number(10.0 ** (A+(i-1)/C)))
            else:
                QSimpleMessageBox("The 'Levels per Decade' must be a positive number.").show()

            # Gen colors (Log)
            numIntervals = len(values) - 1
            d = int(222 / (numIntervals - 1))
            for a in range(numIntervals):
                colors.append(16 + a * d)
            
        if self.includeZeroButtonGroup.isChecked('On'):
            values.insert(0, 0.0)

        self.rangeLineEdit.setText(str(values))

        self.colorsLineEdit.setText(str(colors))
        
    def setEnabledLogLineEdits(self, enable):
        """ Disable or Enable the 'Num neg decades' and 'smallest exp for neg
        values' lineEdits """
        self.expLineEdit.setEnabled(enable)
        self.negDecadesLineEdit.setEnabled(enable)
        self.smallestExpLabel.setEnabled(enable)
        self.numNegDecLabel.setEnabled(enable)

        if enable == True:
            self.expLineEdit.setToolTip("Smallest exponent for negative values")
            self.negDecadesLineEdit.setToolTip("Number of negative decades.")
        else:
            self.expLineEdit.setToolTip("Disabled. Not in use for linear spacing.")
            self.negDecadesLineEdit.setToolTip("Disabled. Not in use for linear spacing.")            

    def setToolTips(self):
        self.missingLineEdit.setToolTip("Set the missing color index value. The colormap\nranges from 0 to 255, enter the desired color index value 0\nthrough 255.")
        self.legendLineEdit.setToolTip("Specify the desired legend labels.\nFor example:\n None -- Allow VCS to generate legend labels\n(), or [ ], or { } -- No legend  labels\n [0, 10, 20] or { 0:'0', 10:'10', 20:'20' }\n[ 0, 10 ] or { 0:'text', 10:'more text'}")
        self.level1LineEdit.setToolTip("The minimum data value. If level 1 is set to '1e+20',\nthen VCS will select the level.")
        self.level2LineEdit.setToolTip("The maximum data value. If level 2 is set to '1e+20',\nthen VCS will select the level.")
        self.color1LineEdit.setToolTip("The minimum color range index value. The colormap\nranges from 0 to 255, but only color indices 0\nthrough 239 can be changed.")
        self.color2LineEdit.setToolTip("The maximum color range index value. The colormap\nranges from 0 to 255, but only color indices 0\nthrough 239 can be changed.")
        self.rangeLineEdit.setToolTip("The iso level range values. (e.g., 10, 20, 30, 40, 50).")
        self.colorsLineEdit.setToolTip("The iso level color index values. The index colors range\nfrom 0 to 255. For example:\n   Use explicit indices: 16, 32, 48, 64, 80;\n   Use two values to generate index range: 16, 32")
        self.minValLineEdit.setToolTip("The minimum contour level.")
        self.maxValLineEdit.setToolTip("The maximum contour level.")
        self.nIntervals.setToolTip("The number of intervals between each contour level. Maximum number range [2 to 223].")
        self.expLineEdit.setToolTip("Disabled. Not in use for linear spacing.")
        self.negDecadesLineEdit.setToolTip("Disabled. Not in use for linear spacing.")

    def clearCustomSettings(self):
        self.rangeLineEdit.setText('')
        self.colorsLineEdit.setText('')
        self.minValLineEdit.setText('')
        self.maxValLineEdit.setText('')
        self.nIntervals.setValue(2)
        self.expLineEdit.setText('')
        self.negDecadesLineEdit.setText('')

    def linearButtonPressEvent(self):
        """ Disable the 'Num neg decades' and 'smallest exp for neg values'
        lineEdits """
        
        self.expLineEdit.setReadOnly(True)
        self.negDecadesLineEdit.setReadOnly(True)
        self.expLineEdit.setText('')
        self.negDecadesLineEdit.setText('')
        self.expLineEdit.setToolTip("Disabled. Not in use for linear spacing.")
        self.negDecadesLineEdit.setToolTip("Disabled. Not in use for linear spacing.")        

    def logButtonPressEvent(self):
        """ Enable the 'Num neg decades' and 'smallest exp for neg values'
        lineEdits """
        
        self.expLineEdit.setReadOnly(False)
        self.negDecadesLineEdit.setReadOnly(False)
        self.expLineEdit.setText('')
        self.negDecadesLineEdit.setText('')
        self.expLineEdit.setToolTip("Smallest exponent for negative values")
        self.negDecadesLineEdit.setToolTip("Number of negative decades.")

            
    def getValue(self, lineEdit, convertType, default=None):
        try:
            return convertType(lineEdit.text())
        except:
            return default
        
class QSimpleMessageBox(QtGui.QMessageBox):

    def __init__(self, text, parent):
        QtGui.QMessageBox.__init__(self, parent)
        self.setText(text)
        
class QFramedWidget(QtGui.QFrame):

    def __init__(self, titleText=None, parent=None):
        QtGui.QFrame.__init__(self, parent)    
        self.setFrameStyle(QtGui.QFrame.Raised)
        self.setFrameShape(QtGui.QFrame.Box)
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
            
        self.addLabel(text)            
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
        return label

    def addSlider(self):
        slider = QtGui.QSlider(QtCore.Qt.Horizontal)
        self.addWidget(slider)
        return slider

    def addLabeledSlider(self, text, newRow=True):
        if newRow == True:
            self.newRow()
            
        labeledSlider = QLabeledSlider(text)
        self.addWidget(labeledSlider)
        return labeledSlider.getSlider()

class QLabeledSlider(QtGui.QWidget):
    def __init__(self, text, parent=None):
        QtGui.QWidget.__init__(self, parent)
        vbox = QtGui.QVBoxLayout()
        vbox.setSpacing(10)
        self.setLayout(vbox)
        self.text = text
        self.label = QtGui.QLabel(text)
        self.slider = QtGui.QSlider(QtCore.Qt.Horizontal)

        vbox.addWidget(self.label)
        vbox.addWidget(self.slider)

        self.connect(self.slider, QtCore.SIGNAL('valueChanged(int)'),
                     self.updateLabel)

    def getSlider(self):
        return self.slider

    def updateLabel(self, value):
        self.label.setText("%s  %d" %(self.text, value))
            
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

def round_number( N ):
   import numpy
   P = 10.0 ** ( numpy.floor(numpy.log10(abs(N) ) ) )
   return( sign(N) * int( abs(N)/P + 0.5) * P )

def sign ( N ):
   if (N < 0): return -1
   else: return 1

        
