import types
import qtbrowser
from PyQt4 import QtCore, QtGui
if qtbrowser.useVistrails:
    from gui_controller import gm_name
import vcs

indentSpacing = 10


class VCSGMs():
    def saveOriginalValues(self):
        self.originalValues={}
        for a in self.gmAttributes:
            if a.find(".")>-1:
                sp=a.split(".")
                self.originalValues[a] = getattr(getattr(self.gm,sp[0]),sp[1])
            else:
                self.originalValues[a] = getattr(self.gm,a)
            
    def restoreOriginalValues(self):
        for a in self.gmAttributes:
            if a.find(".")>-1:
                sp=a.split(".")
                tmp = getattr(self.gm,sp[0])
                setattr(tmp,sp[1],self.originalValues[a])
            else:
                setattr(self.gm,a,self.originalValues[a])
        self.initValues()

    def getOrCreateGMString(self,canvas=0,method = "get", name = None, original=""):
        if original != "":
            original=", '%s'" % original
        if name is None:
            name = self.gm.name
        if isinstance(self.gm,vcs.boxfill.Gfb):
            return "gm = vcs_canvas[%i].%sboxfill('%s'%s)\n" % (canvas,method,name,original)
        elif isinstance(self.gm,vcs.isofill.Gfi):
            return "gm = vcs_canvas[%i].%sisofill('%s'%s)\n" % (canvas,method,name,original)
        elif isinstance(self.gm,vcs.isoline.Gi):
            return "gm = vcs_canvas[%i].%sisoline('%s'%s)\n" % (canvas,method,name,original)
        elif isinstance(self.gm,vcs.meshfill.Gfm):
            return "gm = vcs_canvas[%i].%smeshfill('%s'%s)\n" % (canvas,method,name,original)
        elif isinstance(self.gm,vcs.outfill.Gfo):
            return "gm = vcs_canvas[%i].%soutfill('%s'%s)\n" % (canvas,method,name,original)
        elif isinstance(self.gm,vcs.outline.Go):
            return "gm = vcs_canvas[%i].%soutline('%s'%s)\n" % (canvas,method,name,original)
        elif isinstance(self.gm,vcs.scatter.GSp):
            return "gm = vcs_canvas[%i].%sscatter('%s'%s)\n" % (canvas,method,name,original)
        elif isinstance(self.gm,vcs.xyvsy.GXy):
            return "gm = vcs_canvas[%i].%sxyvsy('%s'%s)\n" % (canvas,method,name,original)
        elif isinstance(self.gm,vcs.yxvsx.GYx):
            return "gm = vcs_canvas[%i].%syxvsx('%s'%s)\n" % (canvas,method,name,original)
        elif isinstance(self.gm,vcs.xvsy.GXY):
            return "gm = vcs_canvas[%i].%sxvsy('%s'%s)\n" % (canvas,method,name,original)
        elif isinstance(self.gm,vcs.vector.Gv):
            return "gm = vcs_canvas[%i].%svector('%s'%s)\n" % (canvas,method,name,original)
        elif isinstance(self.gm,vcs.taylor.Gtd):
            return "gm = vcs_canvas[%i].%staylordiagram('%s'%s)\n" % (canvas,method,name,original)

    def getGMString(self,canvas=0):
        return self.getOrCreateGMString(canvas)
    
    def changesString(self):
        rec = "## Change Graphics method attributes\n"
        rec += self.getGMString()
        for a in self.gmAttributes:
            if self.originalValues[a]!=getattr(self.gm,a):
                rec+="gm.%s = %s\n" % (a,repr(getattr(self.gm,a)))
        return rec

    def initCommonValues(self):
        self.datawc_x1.setText(str(self.gm.datawc_x1))
        self.datawc_x2.setText(str(self.gm.datawc_x2))
        self.datawc_y1.setText(str(self.gm.datawc_y1))
        self.datawc_y2.setText(str(self.gm.datawc_y2))
        self.xticlabels1.setText(str(self.gm.xticlabels1))
        self.yticlabels1.setText(str(self.gm.yticlabels1))
        self.xticlabels2.setText(str(self.gm.xticlabels2))
        self.yticlabels2.setText(str(self.gm.yticlabels2))
        self.xmtics1.setText(str(self.gm.xmtics1))
        self.ymtics1.setText(str(self.gm.ymtics1))
        self.xmtics2.setText(str(self.gm.xmtics2))
        self.ymtics2.setText(str(self.gm.ymtics2))
        for i in range(self.projection.count()):
            if str(self.projection.itemText(i))==self.gm.projection:
                self.projection.setCurrentIndex(i)
                break
        if "xaxisconvert" in self.gmAttributes:
            for b in self.xaxisconvert.buttonGroup.buttons():
                if str(b.text()) == self.gm.xaxisconvert:
                    b.setChecked(True)
                    break
        if "yaxisconvert" in self.gmAttributes:
            for b in self.yaxisconvert.buttonGroup.buttons():
                if str(b.text()) == self.gm.yaxisconvert:
                    b.setChecked(True)
                    break

    def setupCommonSection(self):
        sc=QtGui.QScrollArea()
        frame = QtGui.QFrame()
        layout = QtGui.QVBoxLayout()
        frame.setLayout(layout)
        
        world = QFramedWidget('World Coordinates')
        self.datawc_x1 = world.addLabeledLineEdit('datawc_x1')
        self.datawc_x2 = world.addLabeledLineEdit('datawc_x2')
        self.datawc_y1 = world.addLabeledLineEdit('datawc_y1')
        self.datawc_y2 = world.addLabeledLineEdit('datawc_y2')
        self.datawc_x1.setToolTip("Set new X1 world coordinate value. If value is 1e+20,\nthen VCS will use the data's coordinate value specified\nin the dimension.")
        self.datawc_x2.setToolTip("Set new X2 world coordinate value. If value is 1e+20,\nthen VCS will use the data's coordinate value specified\nin the dimension.")
        self.datawc_y1.setToolTip("Set new Y1 world coordinate value. If value is 1e+20,\nthen VCS will use the data's coordinate value specified\nin the dimension.")
        self.datawc_y2.setToolTip("Set new Y2 world coordinate value. If value is 1e+20,\nthen VCS will use the data's coordinate value specified\nin the dimension.")
        layout.addWidget(world)

        ticks = QFramedWidget('Ticks and Labels')
        self.xticlabels1 = ticks.addLabeledLineEdit('xticlabels1\t')
        self.xticlabels1.setToolTip("Specify a predefine VCS list name (i.e., lon20, lon30,\np_levels etc.). Or allow VCS to generate the xticlabels#1 by\nentering '*'. Or create a Python dictionary. For example:\n{10:'10', 20:'20', 30:'30'} or {0:'text', 10:'more text'}.\n\nNote: if the Python dictionary is not correct, then no\nxticlabels#1 will be plotted.")
        self.yticlabels1 = ticks.addLabeledLineEdit('yticlabels1\t',newRow=False)
        self.yticlabels1.setToolTip("Specify a predefine VCS list name (i.e., lon20, lon30,\np_levels etc.). Or allow VCS to generate the yticlabels#1 by\nentering '*'. Or create a Python dictionary. For example:\n{10:'10', 20:'20', 30:'30'} or {0:'text', 10:'more text'}.\n\nNote: if the Python dictionary is not correct, then no\nxticlabels#1 will be plotted.")
        self.xticlabels2 = ticks.addLabeledLineEdit('xticlabels2\t')
        self.xticlabels2.setToolTip("Specify a predefine VCS list name (i.e., lon20, lon30,\np_levels etc.). Or allow VCS to generate the xticlabels#2 by\nentering '*'. Or create a Python dictionary. For example:\n{10:'10', 20:'20', 30:'30'} or {0:'text', 10:'more text'}.\n\nNote: if the Python dictionary is not correct, then no\nxticlabels#1 will be plotted.")
        self.yticlabels2 = ticks.addLabeledLineEdit('yticlabels2\t',newRow=False)
        self.yticlabels2.setToolTip("Specify a predefine VCS list name (i.e., lon20, lon30,\np_levels etc.). Or allow VCS to generate the yticlabels#2 by\nentering '*'. Or create a Python dictionary. For example:\n{10:'10', 20:'20', 30:'30'} or {0:'text', 10:'more text'}.\n\nNote: if the Python dictionary is not correct, then no\nxticlabels#1 will be plotted.")
        self.xmtics1 = ticks.addLabeledLineEdit('xmtics1\t')
        self.xmtics1.setToolTip("Specify a predefine VCS list name (i.e., lon20, lon30,\np_levels etc.). Or allow VCS to generate the xmtics#1 by\nentering '*'. Or create a Python dictionary. For example:\n{10:'10', 20:'20', 30:'30'} or {0:'text', 10:'more text'}.\n\nNote: if the Python dictionary is not correct, then no\nxmtics#1 will be plotted.")
        self.ymtics1 = ticks.addLabeledLineEdit('ymtics1\t',newRow=False)
        self.ymtics1.setToolTip("Specify a predefine VCS list name (i.e., lon20, lon30,\np_levels etc.). Or allow VCS to generate the ymtics#1 by\nentering '*'. Or create a Python dictionary. For example:\n{10:'10', 20:'20', 30:'30'} or {0:'text', 10:'more text'}.\n\nNote: if the Python dictionary is not correct, then no\nxmtics#1 will be plotted.")

        self.xmtics2 = ticks.addLabeledLineEdit('xmtics2\t')
        self.xmtics2.setToolTip("Specify a predefine VCS list name (i.e., lon20, lon30,\np_levels etc.). Or allow VCS to generate the xmtics#2 by\nentering '*'. Or create a Python dictionary. For example:\n{10:'10', 20:'20', 30:'30'} or {0:'text', 10:'more text'}.\n\nNote: if the Python dictionary is not correct, then no\nxmtics#1 will be plotted.")
        self.ymtics2 = ticks.addLabeledLineEdit('ymtics2\t',newRow=False)
        self.ymtics2.setToolTip("Specify a predefine VCS list name (i.e., lon20, lon30,\np_levels etc.). Or allow VCS to generate the ymtics#1 by\nentering '*'. Or create a Python dictionary. For example:\n{10:'10', 20:'20', 30:'30'} or {0:'text', 10:'more text'}.\n\nNote: if the Python dictionary is not correct, then no\nxmtics#1 will be plotted.")
        layout.addWidget(ticks)

        proj = QFramedWidget('Projection and Axes')
        self.projection = proj.addLabeledComboBox("Projection",self.root.canvas[0].listelements("projection"))
        self.projection.setToolTip("Choose Graphics Method Projection")
        self.projedit = proj.addButton("Edit",newRow=False)
        self.projedit.setToolTip('Edit projection properties')
        self.projedit.setEnabled(False)
        if "xaxisconvert" in self.gmAttributes:
            self.xaxisconvert = proj.addRadioFrame("X axis transform",["linear","log10","ln","exp","area_wt"])
            self.xaxisconvert.setToolTip("Choose X (horizontal) axis representation")
        if "yaxisconvert" in self.gmAttributes:
            self.yaxisconvert = proj.addRadioFrame("Y axis transform",["linear","log10","ln","exp","area_wt"])
            self.yaxisconvert.setToolTip("Choose Y (vertical) axis representation")
        layout.addWidget(proj)
        sc.setWidget(frame)
        self.parent.parent.editorTab.addTab(sc,"'%s' World Coordinates and Axes" % self.gm.name)

    def applyCommonChanges(self):
        self.gm.projection = str(self.projection.currentText())
        try:
            self.gm.xticlabels1 = eval(str(self.xticlabels1.text()))
        except:
            self.gm.xticlabels1 = str(self.xticlabels1.text())
        try:
            self.gm.xticlabels2 = eval(str(self.xticlabels2.text()))
        except:
            self.gm.xticlabels2 = str(self.xticlabels2.text())
        try:
            self.gm.yticlabels1 = eval(str(self.yticlabels1.text()))
        except:
            self.gm.yticlabels1 = str(self.yticlabels1.text())
        try:
            self.gm.yticlabels2 = eval(str(self.yticlabels2.text()))
        except:
            self.gm.yticlabels2 = str(self.yticlabels2.text())
        try:
            self.gm.xmtics1 = eval(str(self.xmtics1.text()))
        except:
            self.gm.xmtics1 = str(self.xmtics1.text())
        try:
            self.gm.xmtics2 = eval(str(self.xmtics2.text()))
        except:
            self.gm.xmtics2 = str(self.xmtics2.text())
        try:
            self.gm.ymtics1 = eval(str(self.ymtics1.text()))
        except:
            self.gm.ymtics1 = str(self.ymtics1.text())
        try:
            self.gm.ymtics2 = eval(str(self.ymtics2.text()))
        except:
            self.gm.ymtics2 = str(self.ymtics2.text())
        self.gm.datawc_x1 = eval(str(self.datawc_x1.text()))
        self.gm.datawc_x2 = eval(str(self.datawc_x2.text()))
        self.gm.datawc_y1 = eval(str(self.datawc_y1.text()))
        self.gm.datawc_y2 = eval(str(self.datawc_y2.text()))
        ## self.gm.datawc_time_units = 
        ## self.gm.datawc_time_calendar =
        if "xaxisconvert" in self.gmAttributes:
            self.gm.xaxisconvert = str(self.xaxisconvert.buttonGroup.button(self.xaxisconvert.buttonGroup.checkedId()).text())
        if "yaxisconvert" in self.gmAttributes:
            self.gm.yaxisconvert = str(self.yaxisconvert.buttonGroup.button(self.yaxisconvert.buttonGroup.checkedId()).text())

class VCSGMs1D:

    def setupLines(self, target):
        self.lineType = target.addLabeledComboBox('Type: ',
                                                  ["solid", "dash", "dot", "dash-dot", "long-dash"])
        self.lineColor = target.addLabeledSpinBox('Color: ',0,255)
        self.lineWidth = target.addLabeledSpinBox('Width: ',0,300)
        self.lineType.setToolTip("Set the line types. The line values can either be\n('solid', 'dash', 'dot', 'dash-dot', 'long-dash')\nor (0, 1, 2, 3, 4) or None")
        self.lineColor.setToolTip("Set the line colors. The line color attribute\n values must be integers ranging from 0 to 255.\n(e.g., 16, 32, 48, 64) ")
        self.lineWidth.setToolTip("Set the line width. The line width is an integer\nor float value in the range (1 to 300)")
        self.initLineValues()
        
    def setupMarkers(self, target):
        self.markerType = target.addLabeledComboBox('Type:',
                                                     ['dot', 'plus', 'star', 'circle', 'cross', 'diamond','triangle_up', 'triangle_down', 'triangle_left','triangle_right', 'square', 'diamond_fill','triangle_up_fill', 'triangle_down_fill','triangle_left_fill', 'triangle_right_fill','square_fill'])
        self.markerColor = target.addLabeledSpinBox('Color:',0,255)
        self.markerSize = target.addLabeledSpinBox('Sizes:',0,300)
        self.markerType.setToolTip("Set the marker types. The marker values can either\nbe (None, 'dot', 'plus', 'star', 'circle', 'cross', 'diamond',\n'triangle_up', 'triangle_down', 'triangle_left',\n'triangle_right', 'square', 'diamond_fill',\n'triangle_up_fill', 'triangle_down_fill',\n'triangle_left_fill', 'triangle_right_fill',\n'square_fill') or (0, 1, 2, 3, 4, 5, 6, 7, 8, 9,\n10, 11, 12, 13, 14, 15, 16, 17) or None. ")
        self.markerColor.setToolTip("Set the marker colors. The marker color attribute\nvalues must be integers ranging from 0 to 255.")
        self.markerSize.setToolTip("Set the marker sizes. The marker size attribute\nvalues must be integers or floats ranging  from\n1 to 300. " )
        self.initMarkerValues()

    def initLineValues(self):
        self.lineType.setCurrentIndex(0)
        for i in range(self.lineType.count()):
            if str(self.lineType.itemText(i))==self.gm.line:
                self.lineType.setCurrentIndex(i)
                break
        if self.gm.linecolor is None:
            self.lineColor.setValue(241)
        else:
            self.lineColor.setValue(self.gm.linecolor)
        if self.gm.linewidth is None:
            self.lineWidth.setValue(1)
        else:
            self.lineWidth.setValue(self.gm.linewidth)
        
    def initMarkerValues(self):
        self.markerType.setCurrentIndex(0)
        for i in range(self.markerType.count()):
            if str(self.markerType.itemText(i))==self.gm.marker:
                self.markerType.setCurrentIndex(i)
                break
        if self.gm.markercolor is None:
            self.markerColor.setValue(241)
        else:
            self.markerColor.setValue(self.gm.markercolor)
        if self.gm.markersize is None:
            self.markerSize.setValue(1)
        else:
            self.markerSize.setValue(self.gm.markersize)

    def applyMarkerChanges(self):
        self.gm.marker=str(self.markerType.currentText())
        self.gm.markersize=int(self.markerSize.text())
        self.gm.markercolor=int(self.markerColor.text())
        
    def applyLineChanges(self):
        self.gm.line=str(self.lineType.currentText())
        self.gm.linewidth=int(self.lineWidth.text())
        self.gm.linecolor=int(self.lineColor.text())

class VCSGMRanges:
    def rangeSettings(self,target):
        target.addWidget(QtGui.QLabel('Define level range:'))
        self.includeZeroButtonGroup = target.addRadioFrame('Include Zero Level:',
                                                                   ['Off', 'On'],
                                                                   newRow=False)
        self.rangeLineEdit = target.addLabeledLineEdit('Ranges:')
        self.rangeLineEdit.setToolTip("The level range values.\ne.g: (10, 20, 30, 50)\nor: ([10,20],[20,30],[30,50])")
        self.colorsLineEdit = target.addLabeledLineEdit('Colors:')
        self.colorsLineEdit.setToolTip("The level color index values. The index colors range\nfrom 0 to 255. For example:\n   Use explicit indices: 16, 32, 48, 64, 80;\n   Use two values to generate index range: 16, 32")
        if hasattr(self.gm,"fillareaindices"):
            self.patternsLineEdit = target.addLabeledLineEdit('Patterns:')
            self.patternsLineEdit.setToolTip("The level pattern index values. The index pattern range\nfrom 0 to 18.")
            self.patternTypeButtonGroup = target.addRadioFrame('Type:',
                                                               ['solid', 'hatch','pattern'],
                                                               newRow=False)
        else:
            self.patternsLineEdit = target.addLabeledLineEdit('Line types:')
            self.patternsLineEdit.setToolTip('The level line type values.\nThe index can either be ("solid", "dash", "dot", "dash-dot", "long-dash"), (0, 1, 2, 3, 4), or a line object.')
            self.lineWidthsEdit = target.addLabeledLineEdit('Line widths:')
            self.lineWidthsEdit.setToolTip('The level line widths values.')
        target.newRow()
        target.addWidget(QtGui.QLabel('Generate level:'))
        self.spacingButtonGroup = target.addRadioFrame('spacing:',
                                                               ['Linear', 'Log'],
                                                               newRow=False)

        self.minValLineEdit = target.addLabeledLineEdit('Minimum Value:')
        self.minValLineEdit.setToolTip("The first level")
        self.maxValLineEdit = target.addLabeledLineEdit('Maximum Value:')
        self.maxValLineEdit.setToolTip("The last level")
        self.nIntervals = target.addLabeledSpinBox('Number of Intervals:',
                                                                   2, 223)
        self.nIntervals.setToolTip("The number of intervals between each contour level. Maximum number range [2 to 223].")
        
        self.smallestExpLabel, self.expLineEdit = target.addLabelAndLineEdit('Smallest Exponent for Negative Values:')
        self.smallestExpLabel.setEnabled(False)
        self.numNegDecLabel, self.negDecadesLineEdit = target.addLabelAndLineEdit('Number of Negative Decades:')
        self.numNegDecLabel.setEnabled(False)
        genRangesButton = target.addButton('Generate Ranges')
        genRangesButton.setToolTip("Use the 'Minimun Value', 'Maximum Value', and 'Number\nof Intervals' to generate the iso level range values\nand color index values. Note: if 'Ranges' and 'Colors'\nare specified, then the plot will use these numbers\nto generate the contour levels.")
        clearButton = target.addButton('Clear All', newRow=False)        
        self.initRangeValues()

        # Connect Signals        
        self.connect(clearButton, QtCore.SIGNAL('pressed()'), self.clearCustomSettings)
        self.connect(genRangesButton, QtCore.SIGNAL('pressed()'), self.generateRanges)

    def applyRangeSettings(self):
        if hasattr(self.gm,"fillareastyle"):
            self.gm.fillareastyle = str(self.patternTypeButtonGroup.buttonGroup.button(self.patternTypeButtonGroup.buttonGroup.checkedId()).text())
            self.gm.fillareacolors = eval(str(self.colorsLineEdit.text()))
            self.gm.fillareaindices = eval(str(self.patternsLineEdit.text()))
        else:
            self.gm.linecolors=eval(str(self.colorsLineEdit.text()))
            self.gm.line= eval(str(self.patternsLineEdit.text()))
            self.gm.linewidths = eval(str(self.lineWidthsEdit.text()))
        self.levels = eval(str(self.rangeLineEdit.text()))

    def initRangeValues(self):
        self.minValLineEdit.setText('')
        self.maxValLineEdit.setText('')
        self.expLineEdit.setText('')
        self.negDecadesLineEdit.setText('')
        self.nIntervals.setValue(2)
        self.setEnabledLogLineEdits(False)
        self.includeZeroButtonGroup.setChecked('Off')
        self.spacingButtonGroup.setChecked('Linear')
        self.rangeLineEdit.setText(str(self.gm.levels))
        if hasattr(self.gm,"fillareastyle"):
            self.colorsLineEdit.setText(str(self.gm.fillareacolors))
            self.patternsLineEdit.setText(str(self.gm.fillareaindices))
            self.patternTypeButtonGroup.setChecked(self.gm.fillareastyle)
        else:
            self.colorsLineEdit.setText(str(self.gm.linecolors))
            self.patternsLineEdit.setText(str(self.gm.line))
            self.lineWidthsEdit.setText(str(self.gm.linewidths))

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

        
## class QGraphicsMethodAttributeWindow(QtGui.QWidget):

##     def __init__(self, canvas=None, parent=None):
##         QtGui.QWidget.__init__(self, parent)
##         initialWidth = 480
##         initialHeight = 640
##         self.resize(initialWidth, initialHeight)        
##         self.setWindowTitle('Graphics Methods Attrtibute Settings')
##         self.parent = parent

##         # Create tab widgets
##         self.tabWidget = QtGui.QTabWidget()
##         if canvas is not None:
##             self.boxfillEditor = QBoxfillEditor(gm=canvas.getboxfill('ASD'))
##             self.continentsEditor = QContinentsEditor(gm=canvas.getcontinents('ASD'))
##             self.meshfillEditor = QMeshfillEditor(gm=canvas.getmeshfill())
##             self.outfillEditor = QOutfillEditor(gm=canvas.getoutfill())
##             self.scatterEditor = QScatterEditor(gm=canvas.getscatter('ASD'))
##             self.taylorEditor = QTaylorDiagramEditor(gm=canvas.gettaylordiagram('ASD'))
##             self.vectorEditor = QVectorEditor(gm=canvas.getvector())
##         else:
##             self.boxfillEditor = QBoxfillEditor()
##             self.continentsEditor = QContinentsEditor()
##             self.meshfillEditor = QMeshfillEditor()
##             self.outfillEditor = QOutfillEditor()
##             self.scatterEditor = QScatterEditor()
##             self.taylorEditor = QTaylorDiagramEditor()
##             self.vectorEditor = QVectorEditor()
            
##         self.contourEditor = QContourEditor()
##         self.oneDimEditor = Q1DPlotEditor()
##         self.outlineEditor = QOutlineEditor()
        
##         # Add tabs
##         self.tabWidget.addTab(self.boxfillEditor, 'Boxfill')
##         self.tabWidget.addTab(self.continentsEditor, 'Continents')
##         self.tabWidget.addTab(self.contourEditor, 'Contour')
##         self.tabWidget.addTab(self.meshfillEditor, 'Meshfill')
##         self.tabWidget.addTab(self.oneDimEditor, '1D Plot')
##         self.tabWidget.addTab(self.outfillEditor, 'Outfill')
##         self.tabWidget.addTab(self.outlineEditor, 'Outline')
##         self.tabWidget.addTab(self.scatterEditor, 'Scatter')
##         self.tabWidget.addTab(self.taylorEditor, 'Taylor Diagram')
##         self.tabWidget.addTab(self.vectorEditor, 'Vector')
##         self.setToolTips()        
        
##         # Add preview, reset, apply, cancel buttons
##         previewButton = self.getButton('Preview')
##         resetButton = self.getButton('Reset')
##         applyButton = self.getButton('Apply')
##         cancelButton = self.getButton('Cancel')

##         hbox = QtGui.QHBoxLayout()
##         hbox.addWidget(previewButton)
##         hbox.addWidget(resetButton)
##         hbox.addWidget(applyButton)
##         hbox.addWidget(cancelButton)

##         # Setup the layout
##         vbox = QtGui.QVBoxLayout()
##         vbox.addWidget(self.tabWidget)
##         vbox.addLayout(hbox)
##         wrapperWidget = QtGui.QWidget()
##         wrapperWidget.setLayout(vbox)
##         self.setCentralWidget(wrapperWidget)

##         # Connect Signals
##         self.connect(cancelButton, QtCore.SIGNAL('pressed()'), self.close)
##         self.connect(resetButton, QtCore.SIGNAL('pressed()'), self.resetPressedEvent)
##         self.connect(applyButton, QtCore.SIGNAL('pressed()'), self.applyPressedEvent)
##         self.connect(previewButton, QtCore.SIGNAL('pressed()'), self.previewPressedEvent)

##     def resetPressedEvent(self):
##         self.boxfillEditor.initValues()
##         self.continentsEditor.initValues()
##         self.meshfillEditor.initValues()
##         self.outfillEditor.initValues()
##         self.scatterEditor.initValues()
##         self.taylorEditor.initValues()
##         self.vectorEditor.initValues()
            
##         self.contourEditor.initValues()
##         self.oneDimEditor.initValues()
##         self.outlineEditor.initValues()

##     def applyPressedEvent(self):
##         #self.boxfillEditor.setVistrailsGraphicsMethod(self.parent.getParent())
        
##         # TODO
##         # self.continentsEditor.setVistrailsGraphicsMethod()
##         # self.meshfillEditor.setVistrailsGraphicsMethod()
##         # self.outfillEditor.setVistrailsGraphicsMethod()
##         # self.scatterEditor.setVistrailsGraphicsMethod()
##         # self.taylorEditor.setVistrailsGraphicsMethod()
##         # self.vectorEditor.setVistrailsGraphicsMethod()
##         # self.contourEditor.setVistrailsGraphicsMethod()
##         # self.oneDimEditor.setVistrailsGraphicsMethod()
##         # self.outlineEditor.setVistrailsGraphicsMethod()
##         return #todo

##     def previewPressedEvent(self):
##         return # TODO

##     def setToolTips(self):
##         self.tabWidget.setTabToolTip(0, 'The Boxfill graphics method displays a two-dimensional data\narray by surrounding each data value with a colored grid box.')
##         self.tabWidget.setTabToolTip(1, "The Continents graphics method draws a predefined,\ngeneric set of continental outlines in a longitude\nby latitude space. (To draw continental outlines,\nno external data set is required.)")
##         self.tabWidget.setTabToolTip(2, "This Contour notebook tab represent both the Isofill\nand Isoline graphics methods. The Isofill graphics\nmethod fills the area between selected isolevels\n(levels of constant value) of a two-dimensional\narray; the manner of filling the area is determined\nby the named fill area attributes. The Isoline\ngraphics method draws lines of constant value at\nspecified levels to graphically represent the values\nof a two-dimensional array; labels also can be\ndisplayed on the isolines.\nIsolines can also have \"orientation\" arrows, indicating clockwise or counter-clockwise")
##         self.tabWidget.setTabToolTip(3, "The Meshfill graphics method draws data on irregular grid (or 'mesh')at specified levels to graphically represent\nthe values of a one-dimensional array;\nUnless the irregular grid is supported by cdms2, a mesh array must be passed as well")
##         self.tabWidget.setTabToolTip(4, "This 1D Plot notebook tab represent the XvsY, Xyvsy,\nand Yxvsx graphics methods. The XvsY graphics method\ndisplays a line plot from two 1D data arrays, that\nis X(t) and Y(t), where t represents the 1D\ncoordinate values. The Xyvsy graphics method displays\na line plot from a 1D data array, that is X(y),\nwhere y represents the 1D coordinate values. The\nYxvsx graphics method displays a line plot from\na 1D data array, that is Y(x), where y represents\nthe 1D coordinate values.")
##         self.tabWidget.setTabToolTip(5, "The Outfill graphics method fills a set of integer\nvalues in any data array. Its primary purpose is\nto display continents by filling their area as\ndefined by a surface type array that indicates land,\nocean, and sea-ice points. ")
##         self.tabWidget.setTabToolTip(6, "The Outline graphics method outlines a set of integer\nvalues in any data array. Its primary purpose is\nto display continental outlines as defined by a\nsurface type array that indicates land, ocean, and\nsea-ice points.")
##         self.tabWidget.setTabToolTip(7, "The Scatter graphics method displays a scatter plot\nof two 4-dimensional data arrays, e.g. A(x,y,z,t)\nand B(x,y,z,t). ")
##         self.tabWidget.setTabToolTip(8, "The Taylor diagram graphics method provides a statistical\nsummary of pattern correspondence. A single point on\nthe diagram indicates how similar two patterns are in\nterms of their correlation, root-mean-square (RMS)\ndifference, and the ratio of their variances.  The\nstandard deviation of a pattern is proportional to the\nradial distance.  The correlation is given by the cosine\nof the azimuthal angle. The RMS difference is proportional\nto the distance between the plotted points and the\nreference point (often chosen to be the observed\npattern), which is located along the abscissa at a radial\ndistance proportional to its standard deviation.")
##         self.tabWidget.setTabToolTip(9, "The Vector graphics method displays a vector plot\nof a 2D vector field. Vectors are located at the\ncoordinate locations and point in the direction of\nthe data vector field. Vector magnitudes are the\nproduct of data vector field lengths and a scaling\nfactor. ")

##     def getButton(self, text):
##         button = QtGui.QToolButton()
##         button.setText(text)
##         return button


class QVectorEditor(VCSGMs1D,VCSGMs,QtGui.QScrollArea):
    def __init__(self, parent=None, gm=None):
        QtGui.QScrollArea.__init__(self, parent)
        vbox = QtGui.QVBoxLayout()

        # Create Widgets
        frame = QtGui.QFrame()
        frame.setLayout(vbox)
        self.parent=parent
        self.root=parent.root
        self.gmAttributes = [ 'datawc_calendar', 'datawc_timeunits', 'datawc_x1', 'datawc_x2', 'datawc_y1', 'datawc_y2', 'projection', 'xaxisconvert', 'xmtics1', 'xmtics2', 'xticlabels1', 'xticlabels2', 'yaxisconvert', 'ymtics1', 'ymtics2', 'yticlabels1', 'yticlabels2','line','linecolor','linewidth','scale','alignment','reference','type']
        self.gm = self.root.canvas[0].getvector(gm)
        self.saveOriginalValues()
        self.setupCommonSection()

        lines = QFramedWidget("Line Settings")
        self.setupLines(lines)
        vbox.addWidget(lines)
        vector = QFramedWidget("Vector Settings")
        self.scale = vector.addLabeledDoubleSpinBox('Vector Scale', -1e20, 1e20, .1)
        self.alignment = vector.addLabeledComboBox('Vector Alignment:',
                                                  ['head', 'center', 'tail'])
        self.headType = vector.addLabeledComboBox('Vector Head Type:',
                                                 ['arrows', 'barbs', 'solidarrows'])
        self.reference = vector.addLabeledDoubleSpinBox('Vector reference', -1e20, 1e20, .1)
    
        vbox.addWidget(vector)

        # Init values & set tool tips
        self.initValues()
        self.scale.setToolTip("Select the vector scale factor.")
        self.alignment.setToolTip("Set the vector alignment.")
        self.headType.setToolTip("Set the vector head type.")
        self.reference.setToolTip("Set the vector reference. Note: if the value is 1e+20,\nthen VCS will determine the vector reference.")        

        self.setWidget(frame)

    def initValues(self):
        self.initCommonValues()
        self.initLineValues()
        self.scale.setValue(self.gm.scale)
        for i in range(self.headType.count()):
            if str(self.headType.itemText(i))==self.gm.type:
                self.headType.setCurrentIndex(i)
                break
        for i in range(self.alignment.count()):
            if str(self.alignment.itemText(i))==self.gm.alignment:
                self.alignment.setCurrentIndex(i)
                break
        self.reference.setValue(self.gm.reference)

    def applyChanges(self):
        self.applyCommonChanges()
        self.applyLineChanges()
        self.gm.alignment = str(self.alignment.currentText())
        self.gm.type = str(self.headType.currentText())
        self.gm.reference = float(self.reference.value())
        self.gm.scale = float(self.scale.value())

class QTaylorDiagramEditor(VCSGMs,QtGui.QScrollArea):
    def __init__(self, parent=None, gm=None):
        QtGui.QScrollArea.__init__(self, parent)
        vbox = QtGui.QVBoxLayout()
        # Create Widgets
        frame = QtGui.QFrame()
        frame.setLayout(vbox)
        self.parent=parent
        self.root=parent.root
        self.gmAttributes = [ 'xmtics1', 'xticlabels1', 'ymtics1', 'yticlabels1','cmtics1', 'cticlabels1', 'detail',
                              'max','quadrans','skillValues','skillColor','skillDrawLabels','skillCoefficient',
                              'referencevalue','arrowlength','arrowangle','arrowbase',
                              'Marker.status','Marker.line','Marker.id','Marker.id_size','Marker.id_color',
                              'Marker.id_font','Marker.symbol','Marker.color','Marker.size','Marker.xoffset',
                              'Marker.yoffset','Marker.line_color','Marker.line_size','Marker.line_type',
                              ]
        self.gm=self.root.canvas[0].gettaylordiagram(gm)
        self.saveOriginalValues()

        # General Aspect
        genFrame = QFramedWidget('General Aspect')
        self.detailSlider = genFrame.addLabeledSlider('Detail:', newRow=False)
        self.detailSlider.setTickPosition(QtGui.QSlider.TicksBelow)
        self.detailSlider.setMinimum(0)
        self.detailSlider.setMaximum(100)
        self.detailSlider.setSingleStep(5)
        self.detailSlider.setTickInterval(10)
        self.maxValue = genFrame.addLabeledLineEdit('Maximum Value:')
        self.quadran = genFrame.addRadioFrame('Quadran', ['1', '2'])
        self.refValue = genFrame.addLabeledLineEdit('Reference Value:')
        vbox.addWidget(genFrame)

        # Skill
        skillFrame = QFramedWidget('Skill')
        self.drawLabels = skillFrame.addCheckBox('Draw Skill Contour Labels')
        self.skillValues = skillFrame.addLabeledLineEdit('Values:')
        self.skillLineColor = skillFrame.addLabeledSpinBox('Color: ',0,255)
        self.skillCoefficients = skillFrame.addLabeledLineEdit('Skill Coefficients:')
        vbox.addWidget(skillFrame)
        
        # Arrows
        arrowFrame = QFramedWidget('Arrows')
        self.lengthSlider = arrowFrame.addLabeledSlider('Length:', newRow=False,divider=100.)
        self.lengthSlider.setTickPosition(QtGui.QSlider.TicksBelow)
        self.lengthSlider.setMinimum(0)
        self.lengthSlider.setMaximum(1000)
        self.lengthSlider.setSingleStep(5)
        self.lengthSlider.setTickInterval(100)
        self.angleSlider = arrowFrame.addLabeledSlider('Angle:',newRow=False)
        self.angleSlider.setMaximum(360)
        self.angleSlider.setTickPosition(QtGui.QSlider.TicksBelow)
        self.baseSlider = arrowFrame.addLabeledSlider('Base:',newRow=False,divider=100.)
        self.baseSlider.setTickPosition(QtGui.QSlider.TicksBelow)
        self.baseSlider.setMaximum(1000)
        self.baseSlider.setSingleStep(5)
        self.baseSlider.setTickInterval(100)
        # TODO - Create a widget to draw the arrow
        vbox.addWidget(arrowFrame)

        ticFrame = QFramedWidget('Ticks and Labels')
        self.xlabels = ticFrame.addLabeledLineEdit('xticlabels1:')
        self.ylabels = ticFrame.addLabeledLineEdit('yticlabels:',newRow=False)
        self.corLabels = ticFrame.addLabeledLineEdit('cticklabels1:',newRow=False)
        self.xticks = ticFrame.addLabeledLineEdit('xmticks1:')
        self.yticks = ticFrame.addLabeledLineEdit('ymticks1:',newRow=False)
        self.corTicks = ticFrame.addLabeledLineEdit('cmticks1:',newRow=False)
        vbox.addWidget(ticFrame)

        self.markersTab = QTaylorMarkers(self)
        self.parent.parent.editorTab.addTab(self.markersTab, "'%s' Markers" % self.gm.name)

        self.initValues()
        self.setWidget(frame)

    def initValues(self):
        # General Aspect
        self.detailSlider.setValue(self.gm.detail)
        self.maxValue.setText(repr(self.gm.max))
        self.quadran.setChecked(str(self.gm.quadrans))
        self.refValue.setText(repr(self.gm.referencevalue))

        # Skills
        if self.gm.skillDrawLabels=="y":
            self.drawLabels.setChecked(True)
        else:
            self.drawLabels.setChecked(False)
        self.skillValues.setText(repr(self.gm.skillValues))
        if isinstance(self.gm.skillColor,str):
            self.skillLineColor.setValue(self.root.canvas[0].match_color(self.gm.skillColor))
        else:
            self.skillLineColor.setValue(self.gm.skillColor)
        self.skillCoefficients.setText(repr(self.gm.skillCoefficient))
        
        # Arrows
        self.lengthSlider.setValue(self.gm.arrowlength*100)
        self.angleSlider.setValue(self.gm.arrowangle)
        self.baseSlider.setValue(self.gm.arrowbase*100)

        self.xlabels.setText(repr(self.gm.xticlabels1))
        self.xticks.setText(repr(self.gm.xmtics1))
        self.ylabels.setText(repr(self.gm.yticlabels1))
        self.yticks.setText(repr(self.gm.ymtics1))
        self.corLabels.setText(repr(self.gm.cticlabels1))
        self.corTicks.setText(repr(self.gm.cmtics1))

        self.markersTab.initValues()

class QTaylorMarkers(QtGui.QScrollArea):
    """ Tabbed Widget for Taylor -> Markers """
    
    def __init__(self, parent=None, gm=None):
        QtGui.QScrollArea.__init__(self, parent)
        self.row = 1
        self.grid = QtGui.QGridLayout()
        self.markerList = []
        self.parent=parent
        self.root=parent.root
        
        vbox = QtGui.QVBoxLayout()
        
        # Create Column Labels
        self.grid.addWidget(QtGui.QLabel(''), 0, 0)
        self.grid.addWidget(QtGui.QLabel('#'), 0, 1)
        self.grid.addWidget(QtGui.QLabel('Active'), 0, 2)
        self.grid.addWidget(QtGui.QLabel('Symbol'), 0, 3)
        self.grid.addWidget(QtGui.QLabel('Color'), 0, 4)
        self.grid.addWidget(QtGui.QLabel('Size'), 0, 5)
        self.grid.addWidget(QtGui.QLabel('+X'), 0, 6)
        self.grid.addWidget(QtGui.QLabel('+Y'), 0, 7)
        self.grid.addWidget(QtGui.QLabel('Id'), 0, 8)
        self.grid.addWidget(QtGui.QLabel('Id Size'), 0, 9)
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

    def setAComboxItem(self,combo,value):
        for i in range(combo.count()):
            if combo.itemText(i) == value:
                combo.setCurrentIndex(i)
                break
        return
    
    def initValues(self):
        for m in self.markerList:
            m.widgets['selectedBox'].setChecked(True)
        self.removeSelectedMarkers()
        # Ok now determines the number of elets to add
        M = self.parent.gm.Marker
        nmax = 0
        for a in ['status','line','id','id_size','id_color','id_font','symbol','color','size','xoffset','yoffset','line_color','line_size','line_type']:
            nmax=max(nmax,len(getattr(M,a)))
        for i in range(nmax):
            self.addMarker()
            #Ok now we need to initialize the values of these markers
            m = self.markerList[-1]
            w = m.getWidgets()
            s = self.getGMMarkerAttributeValue(M,"status",i)
            if s ==1:
                w['activeBox'].setChecked(True)
            else:
                w['activeBox'].setChecked(False)
            s = self.getGMMarkerAttributeValue(M,"symbol",i)
            self.setAComboxItem(w['symbolCombo'],s)
            c = self.getGMMarkerAttributeValue(M,"color",i)
            w['colorCombo1'].setText(str(c))
            s = self.getGMMarkerAttributeValue(M,"size",i)
            w['size'].setText(str(s))
            s = self.getGMMarkerAttributeValue(M,"id",i)
            w['id'].setText(str(s))
            s = self.getGMMarkerAttributeValue(M,"id_size",i)
            w['idSize'].setText(str(s))
            s = self.getGMMarkerAttributeValue(M,"id_color",i)
            w['idColorCombo'].setText(str(s))
            s = self.root.canvas[0].getfont(self.getGMMarkerAttributeValue(M,"id_font",i))
            self.setAComboxItem(w['idFontCombo'],s)
            s = self.getGMMarkerAttributeValue(M,"xoffset",i)
            w['x'].setText(str(s))
            s = self.getGMMarkerAttributeValue(M,"yoffset",i)
            w['y'].setText(str(s))
            s = str(self.getGMMarkerAttributeValue(M,"line",i))
            self.setAComboxItem(w['lineCombo'],s)
            s = str(self.getGMMarkerAttributeValue(M,"line_type",i))
            self.setAComboxItem(w['typeCombo'],s)
            s = self.getGMMarkerAttributeValue(M,"line_size",i)
            w['size2'].setText(str(s))
            s = self.getGMMarkerAttributeValue(M,"line_color",i)
            w['colorCombo2'].setText(str(s))
        return

    def getGMMarkerAttributeValue(self,M,a,i):
        v = getattr(M,a)
        if len(v)<i:
            return v[-1]
        else:
            return v[i]
        
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
        self.markerList.append(QMarkerEditorEntry(self.grid, self.row,parent=self.parent))
        self.row += 1
        
    def createButton(self, text):
        button = QtGui.QToolButton()
        button.setText(text)
        return button

class QMarkerEditorEntry(QtGui.QWidget):
    def __init__(self, grid, row, parent=None):
        QtGui.QWidget.__init__(self, parent)
        self.widgets = {}
        self.parent=parent
        self.root=self.parent.root
        symbolList = ["dot", "plus", "star", "circle", "cross", "diamond", "triangle_up", "triangle_down", "triangle_left", "triangle_right", "square", "diamond_fill", "triangle_up_fill", "triangle_down_fill", "triangle_left_fill", "triangle_right_fill", "square_fill"]
        lineList = ['None', 'tail', 'head', 'line']
        typeList = ['solid', 'dash', 'dot', 'dash-dot', 'long-dash']
        
        self.widgets['selectedBox'] = QtGui.QCheckBox()
        self.widgets['entryNumber'] = QtGui.QLabel(str(row))
        self.widgets['activeBox'] = QtGui.QCheckBox()
        self.widgets['symbolCombo'] = self.createCombo(symbolList)
        self.widgets['colorCombo1'] = QtGui.QLineEdit()
        self.widgets['size'] = QtGui.QLineEdit()
        self.widgets['id'] = QtGui.QLineEdit()
        self.widgets['idSize'] = QtGui.QLineEdit()
        self.widgets['x'] = QtGui.QLineEdit()
        self.widgets['y'] = QtGui.QLineEdit()
        self.widgets['idColorCombo'] = QtGui.QLineEdit()
        self.widgets['idFontCombo'] = self.createCombo(self.root.canvas[0].listelements("font"))
        self.widgets['lineCombo'] = self.createCombo(lineList)
        self.widgets['typeCombo'] = self.createCombo(typeList)
        self.widgets['size2'] = QtGui.QLineEdit()
        self.widgets['colorCombo2'] = QtGui.QLineEdit()
        
        grid.addWidget(self.widgets['selectedBox'], row, 0, QtCore.Qt.AlignTop)
        grid.addWidget(self.widgets['entryNumber'], row, 1, QtCore.Qt.AlignTop)
        grid.addWidget(self.widgets['activeBox'], row, 2, QtCore.Qt.AlignTop)
        grid.addWidget(self.widgets['symbolCombo'], row, 3, QtCore.Qt.AlignTop)
        grid.addWidget(self.widgets['colorCombo1'], row, 4, QtCore.Qt.AlignTop)
        grid.addWidget(self.widgets['size'], row, 5, QtCore.Qt.AlignTop)
        grid.addWidget(self.widgets['x'], row, 6, QtCore.Qt.AlignTop)
        grid.addWidget(self.widgets['y'], row, 7, QtCore.Qt.AlignTop)
        grid.addWidget(self.widgets['id'], row, 8, QtCore.Qt.AlignTop)
        grid.addWidget(self.widgets['idSize'], row, 9, QtCore.Qt.AlignTop)
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


class QScatterEditor(VCSGMs1D,VCSGMs,QtGui.QScrollArea):
    def __init__(self, parent=None, gm=None):
        QtGui.QScrollArea.__init__(self, parent)
        vbox = QtGui.QVBoxLayout()

        # Create Widgets
        frame = QtGui.QFrame()
        frame.setLayout(vbox)
        self.parent=parent
        self.root=parent.root
        self.gmAttributes = [ 'datawc_calendar', 'datawc_timeunits', 'datawc_x1', 'datawc_x2', 'datawc_y1', 'datawc_y2', 'projection', 'xaxisconvert', 'xmtics1', 'xmtics2', 'xticlabels1', 'xticlabels2', 'yaxisconvert', 'ymtics1', 'ymtics2', 'yticlabels1', 'yticlabels2','marker','markercolor','markersize']
        self.gm = self.root.canvas[0].getscatter(gm)
        self.saveOriginalValues()
        self.setupCommonSection()

        markers = QFramedWidget("Markers Settings")
        self.setupMarkers(markers)
        vbox.addWidget(markers)
        self.initValues()
        self.setWidget(frame)


    def initValues(self):
        self.initCommonValues()
        self.initMarkerValues()

    def applyChanges(self):
        self.applyCommonChanges()
        self.applyMarkerChanges()
        
class Q1DPlotEditor(VCSGMs1D,VCSGMs,QtGui.QScrollArea):
    def __init__(self, parent=None, gm=None,type="xyvsy"):
        QtGui.QScrollArea.__init__(self, parent)
        vbox = QtGui.QVBoxLayout()

        # Create Widgets
        frame = QtGui.QFrame()
        frame.setLayout(vbox)
        self.parent=parent
        self.root=parent.root
        self.gmAttributes = [ 'datawc_calendar', 'datawc_timeunits', 'datawc_x1', 'datawc_x2', 'datawc_y1', 'datawc_y2', 'projection', 'xaxisconvert', 'xmtics1', 'xmtics2', 'xticlabels1', 'xticlabels2', 'yaxisconvert', 'ymtics1', 'ymtics2', 'yticlabels1', 'yticlabels2','line','linecolor','linewidth','marker','markercolor','markersize']
        if type == "xyvsy":
            self.gm = self.root.canvas[0].getxyvsy(gm)
            self.gmAttributes.pop(self.gmAttributes.index("xaxisconvert"))
        elif type == "yxvsx":
            self.gm = self.root.canvas[0].getyxvsx(gm)
            self.gmAttributes.pop(self.gmAttributes.index("yaxisconvert"))
        elif type == "xvsy":
            self.gm = self.root.canvas[0].getxvsy(gm)
        self.saveOriginalValues()
        self.setupCommonSection()

        lines = QFramedWidget("Lines Settings")
        self.setupLines(lines)
        vbox.addWidget(lines)
        markers = QFramedWidget("Markers Settings")
        self.setupMarkers(markers)
        vbox.addWidget(markers)
        self.initValues()
        self.setWidget(frame)


    def initValues(self):
        self.initCommonValues()
        self.initMarkerValues()
        self.initLineValues()

    def applyChanges(self):
        self.applyCommonChanges()
        self.applyMarkerChanges()
        self.applyLineChanges()
        
class QOutlineEditor(VCSGMs,QtGui.QScrollArea):
    def __init__(self, parent=None, gm=None):
        QtGui.QScrollArea.__init__(self, parent)
        vbox = QtGui.QVBoxLayout()
        frame = QtGui.QFrame()
        frame.setLayout(vbox)
        self.parent=parent
        self.root=parent.root
        self.gmAttributes = [ 'datawc_calendar', 'datawc_timeunits', 'datawc_x1', 'datawc_x2', 'datawc_y1', 'datawc_y2', 'projection', 'xaxisconvert', 'xmtics1', 'xmtics2', 'xticlabels1', 'xticlabels2', 'yaxisconvert', 'ymtics1', 'ymtics2', 'yticlabels1', 'yticlabels2','linecolor', 'linewidth', 'line', 'outline']
        self.gm = self.root.canvas[0].getoutline(gm)
        self.saveOriginalValues()
        self.setupCommonSection()

        # Create Widgets
        genSettings = QFramedWidget("Outline Line Settings")
        self.lineType = genSettings.addLabeledComboBox('Style:',
                                                 ['solid', 'dash', 'dot', 'dash-dot', 'long-dash'],
                                                 indent=False)
        self.lineColorIndex = genSettings.addLabeledSpinBox('Color:',
                                                      0, 255, indent=False)
        self.lineWidth = genSettings.addLabeledSpinBox('Width:',
                                                      0, 300, indent=False)
        self.indexValues = genSettings.addLabeledLineEdit('Levels:',
                                                    indent=False)

        vbox.addWidget(genSettings)

        #vbox.setAlignment(frame, QtCore.Qt.AlignTop)
        self.initValues()
        self.setWidget(frame)
        
    def initValues(self):
        # Init common area
        self.initCommonValues()
        self.indexValues.setText('1')
        if self.gm.linewidth is None:
            self.lineWidth.setValue(1)
        else:
            self.lineWidth.setValue(selg.gm.linewidth)
        if self.gm.linecolor is None:
            self.lineColorIndex.setValue(241)
        else:
            self.lineColorIndex.setValue(self.gm.linecolor)
        for i in range(self.lineType.count()):
            if str(self.lineType.itemText(i))==self.gm.line:
                self.lineType.setCurrentIndex(i)
                break
        self.indexValues.setText(repr(self.gm.outline))
        
    def applyChanges(self):
        self.applyCommonChanges()
        try:
            self.gm.outline=eval(str(self.indexValues.text()))
        except:
            self.gm.outline=str(self.indexValues.text())
        self.gm.linecolor=int(self.lineColorIndex.text())
        self.gm.linewidth=int(self.lineWidth.text())
        self.gm.line=str(self.lineType.currentText())
        
    def setToolTips(self):
        # Set tool tips
        self.lineType.setToolTip("Select the outline line type. ")
        self.lineWidth.setToolTip("Enter the line width value. There can only\nbe one value (ranging from 0 to 300).")
        self.lineColorIndex.setToolTip("Enter the line color index value. There can only\nbe one color index value (ranging from 0 to 255).\nIf an error in the color index value occurs, then the\ndefault color value index (i.e., 241) will be used.")
        self.indexValues.setToolTip("Outlines are drawn to enclose the specified values\nin the data array. As few as one, or\nas many as\nten values, can be specified:\noutline=([n1,[n2,[n3,...[n10]...]]]).")        

        

class QOutfillEditor(VCSGMs,QtGui.QScrollArea):
    def __init__(self, parent=None, gm=None):
        QtGui.QScrollArea.__init__(self, parent)
        vbox = QtGui.QVBoxLayout()

        # Create Widgets
        frame = QtGui.QFrame()
        frame.setLayout(vbox)
        self.parent=parent
        self.root=parent.root
        self.gmAttributes = [ 'datawc_calendar', 'datawc_timeunits', 'datawc_x1', 'datawc_x2', 'datawc_y1', 'datawc_y2', 'projection', 'xaxisconvert', 'xmtics1', 'xmtics2', 'xticlabels1', 'xticlabels2', 'yaxisconvert', 'ymtics1', 'ymtics2', 'yticlabels1', 'yticlabels2','fillareacolor', 'fillareaindex', 'fillareastyle', 'outfill']
        self.gm = self.root.canvas[0].getoutfill(gm)
        self.saveOriginalValues()
        self.setupCommonSection()
        
        genSettings = QFramedWidget('Outfill Fill Area Settings')
        self.fillArea = genSettings.addLabeledComboBox('Style:',
                                                 ['solid', 'hatch', 'pattern', 'hallow'],
                                                 indent=False)
        self.fillAreaIndex = genSettings.addLabeledSpinBox('Index:',
                                                     1, 18, indent=False)
        self.fillColorIndex = genSettings.addLabeledSpinBox('Color:',
                                                      0, 255, indent=False)
        self.indexValues = genSettings.addLabeledLineEdit('Levels:',
                                                    indent=False)
        vbox.addWidget(genSettings)
        ## vbox.setAlignment(frame, QtCore.Qt.AlignTop)

        self.initValues()

        self.setWidget(frame)

    def initValues(self):
        # Init common area
        self.initCommonValues()
        if self.gm.fillareaindex is None:
            self.fillAreaIndex.setValue(1)
        else:
            self.fillAreaIndex.setValue(selg.gm.fillareaindex)
        if self.gm.fillareacolor is None:
            self.fillColorIndex.setValue(241)
        else:
            self.fillColorIndex.setValue(self.gm.fillareacolor)
        for i in range(self.fillArea.count()):
            if str(self.fillArea.itemText(i))==self.gm.fillareastyle:
                self.fillArea.setCurrentIndex(i)
                break
        self.indexValues.setText(repr(self.gm.outfill))

    def applyChanges(self):
        self.applyCommonChanges()
        try:
            self.gm.outfill=eval(str(self.indexValues.text()))
        except:
            self.gm.outfill=str(self.indexValues.text())
        self.gm.fillareacolor=int(self.fillColorIndex.text())
        self.gm.fillareaindex = int(self.fillAreaIndex.text())
        self.gm.fillareastyle=str(self.fillArea.currentText())
        
    def setToolTips(self):
        # Set ToolTips
        self.fillArea.setToolTip("Select the outfill fill area style type. ")
        self.fillAreaIndex.setToolTip("Select the outfill fill area index value. ")
        self.fillColorIndex.setToolTip("Enter the fillarea color index value. There can only\nbe one color index value (ranging from 0 to 255).\nIf an error in the color index value occurs, then the\ndefault color value index (i.e., 241) will be used.")
        self.indexValues.setToolTip("Outlines are filled to enclose the selected values\nthat appear in the data array. As few as one, or\nas many as ten values, can be specified:\noutline=([n1,[n2,[n3,...[n10]...]]]).")

        

class QMeshfillEditor(QtGui.QScrollArea,VCSGMs,VCSGMRanges):

    def __init__(self, parent=None, gm=None):
        QtGui.QScrollArea.__init__(self, parent)
        vbox = QtGui.QVBoxLayout()
        frame = QtGui.QFrame()
        frame.setLayout(vbox)
        self.parent=parent
        self.root=parent.root
        self.gmAttributes = [ 'datawc_calendar', 'datawc_timeunits', 'datawc_x1', 'datawc_x2', 'datawc_y1', 'datawc_y2', 'projection', 'xaxisconvert', 'xmtics1', 'xmtics2', 'xticlabels1', 'xticlabels2', 'yaxisconvert', 'ymtics1', 'ymtics2', 'yticlabels1', 'yticlabels2','levels','ext_1', 'ext_2', 'fillareacolors', 'fillareaindices', 'fillareastyle', 'legend', 'missing', 'mesh','wrap']
        self.gm = self.root.canvas[0].getmeshfill(gm)
        self.saveOriginalValues()
        self.setupCommonSection()

        # General Settings
        genSettings = QFramedWidget('Mesh Settings')
        self.xWrap = genSettings.addLabeledLineEdit('X Wrap:')
        self.yWrap = genSettings.addLabeledLineEdit('Y Wrap:', newRow = False)
        self.showMesh = genSettings.addRadioFrame('Show Mesh:', ['No', 'Yes'])
        vbox.addWidget(genSettings)

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

        self.isoSettings = QFramedWidget('Levels Range Settings')

        self.rangeSettings(self.isoSettings)
        vbox.addWidget(self.isoSettings)


        self.setWidget(frame)
        
        # Init values / tool tips
        self.initValues()
        self.setToolTips()

    def initValues(self):
        # Init common area
        self.initCommonValues()
        # Init Line Edit Text
        self.missingLineEdit.setText(str(self.gm.missing))
        self.legendLineEdit.setText(repr(self.gm.legend))

        
        if self.gm.ext_1 == "n":
            self.ext1ButtonGroup.setChecked('No')
        else:
            self.ext1ButtonGroup.setChecked('Yes')
        if self.gm.ext_2 == "n":
            self.ext2ButtonGroup.setChecked('No')
        else:
            self.ext2ButtonGroup.setChecked('Yes')

        #Init Range section
        if self.gm.mesh == 0:
            self.showMesh.setChecked('No')
        else:
            self.showMesh.setChecked('Yes')
        self.xWrap.setText(repr(self.gm.wrap[1]))
        self.yWrap.setText(repr(self.gm.wrap[0]))
        #Init range section
        self.initRangeValues()


    def applyChanges(self):
        self.applyCommonChanges()
        self.gm.legend = eval(str(self.legendLineEdit.text()))
        self.gm.ext_1 = str(self.ext1ButtonGroup.buttonGroup.button(self.ext1ButtonGroup.buttonGroup.checkedId()).text()).lower()[0]
        self.gm.ext2 = str(self.ext2ButtonGroup.buttonGroup.button(self.ext2ButtonGroup.buttonGroup.checkedId()).text()).lower()[0]
        self.gm.missing = eval(str(self.missingLineEdit.text()))
        self.gm.wrap = [eval(str(self.yWrap.text())),eval(str(self.xWrap.text()))]
        self.gm.mesh = str(self.showMesh.buttonGroup.button(self.showMesh.buttonGroup.checkedId()).text()).lower()[0]
        self.applyRangeSettings()

    def setToolTips(self):
        # General Setting Tips
        self.missingLineEdit.setToolTip("Set the missing color index value. The colormap\nranges from 0 to 255, enter the desired color index value 0\nthrough 255.")
        self.ext1ButtonGroup.setToolTip("Turn on 1st arrow on legend")
        self.ext2ButtonGroup.setToolTip("Turn on 2nd arrow on legend")
        self.legendLineEdit.setToolTip("Specify the desired legend labels.\nFor example:\n None -- Allow VCS to generate legend labels\n(), or [ ], or { } -- No legend  labels\n [0, 10, 20] or { 0:'0', 10:'10', 20:'20' }\n[ 0, 10 ] or { 0:'text', 10:'more text'}")
        self.xWrap.setToolTip("Set the wrapping along X axis, 0 means no wrapping")
        self.yWrap.setToolTip("Set the wrapping along Y axis, 0 means no wrapping")

class QIsofillEditor(QtGui.QScrollArea,VCSGMs,VCSGMRanges):
    def __init__(self, parent=None, gm=None):
        QtGui.QScrollArea.__init__(self, parent)
        vbox = QtGui.QVBoxLayout()
        frame = QtGui.QFrame()
        frame.setLayout(vbox)
        self.parent=parent
        self.root=parent.root
        self.gmAttributes = [ 'datawc_calendar', 'datawc_timeunits', 'datawc_x1', 'datawc_x2', 'datawc_y1', 'datawc_y2', 'levels','ext_1', 'ext_2', 'fillareacolors', 'fillareaindices', 'fillareastyle', 'legend', 'missing', 'projection', 'xaxisconvert', 'xmtics1', 'xmtics2', 'xticlabels1', 'xticlabels2', 'yaxisconvert', 'ymtics1', 'ymtics2', 'yticlabels1', 'yticlabels2']
        self.gm = self.root.canvas[0].getisofill(gm)
        self.saveOriginalValues()
        self.setupCommonSection()


        # Isofill General Settings 
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

        self.isoSettings = QFramedWidget('Levels Range Settings')

        self.rangeSettings(self.isoSettings)
        vbox.addWidget(self.isoSettings)
        self.setWidget(frame)
        
        self.initValues()
        self.setToolTips()
    def initValues(self):

        # Init common area
        self.initCommonValues()
        
        # Init Line Edit Text
        self.missingLineEdit.setText(str(self.gm.missing))
        self.legendLineEdit.setText(repr(self.gm.legend))

        
        if self.gm.ext_1 == "n":
            self.ext1ButtonGroup.setChecked('No')
        else:
            self.ext1ButtonGroup.setChecked('Yes')
        if self.gm.ext_2 == "n":
            self.ext2ButtonGroup.setChecked('No')
        else:
            self.ext2ButtonGroup.setChecked('Yes')

        #Init range section
        self.initRangeValues()
        

    def setToolTips(self):
        self.missingLineEdit.setToolTip("Set the missing color index value. The colormap\nranges from 0 to 255, enter the desired color index value 0\nthrough 255.")
        self.ext1ButtonGroup.setToolTip("Turn on 1st arrow on legend")
        self.ext2ButtonGroup.setToolTip("Turn on 2nd arrow on legend")
        self.legendLineEdit.setToolTip("Specify the desired legend labels.\nFor example:\n None -- Allow VCS to generate legend labels\n(), or [ ], or { } -- No legend  labels\n [0, 10, 20] or { 0:'0', 10:'10', 20:'20' }\n[ 0, 10 ] or { 0:'text', 10:'more text'}")
    def applyChanges(self):
        self.applyCommonChanges()
        self.gm.legend = eval(str(self.legendLineEdit.text()))
        self.gm.ext_1 = str(self.ext1ButtonGroup.buttonGroup.button(self.ext1ButtonGroup.buttonGroup.checkedId()).text()).lower()[0]
        self.gm.ext2 = str(self.ext2ButtonGroup.buttonGroup.button(self.ext2ButtonGroup.buttonGroup.checkedId()).text()).lower()[0]
        self.gm.missing = eval(str(self.missingLineEdit.text()))
        self.applyRangeSettings()

class QIsolineEditor(QtGui.QScrollArea,VCSGMs,VCSGMRanges):
    def __init__(self, parent=None, gm=None):
        QtGui.QScrollArea.__init__(self, parent)
        vbox = QtGui.QVBoxLayout()
        frame = QtGui.QFrame()
        frame.setLayout(vbox)
        self.parent=parent
        self.root=parent.root
        self.gmAttributes = [ 'datawc_calendar', 'datawc_timeunits', 'datawc_x1', 'datawc_x2', 'datawc_y1', 'datawc_y2', 'projection', 'xaxisconvert', 'xmtics1', 'xmtics2', 'xticlabels1', 'xticlabels2', 'yaxisconvert', 'ymtics1', 'ymtics2', 'yticlabels1', 'yticlabels2', 'level', 'line','linecolors','linewidths','text','textcolors','clockwise','scale','angle','spacing']
        self.gm = self.root.canvas[0].getisoline(gm)
        self.saveOriginalValues()
        self.setupCommonSection()


        # Isoline Levels Settings 
        self.isoSettings = QFramedWidget('Levels Range Settings')
        self.rangeSettings(self.isoSettings)
        vbox.addWidget(self.isoSettings)

        #Isoline Labels Setting
        self.textSettings = QFramedWidget('Levels Labels Settings')
        self.textLabelsOnOff = self.textSettings.addRadioFrame("Draw Labels:",["No","Yes"],newRow=False)
        self.textFonts = self.textSettings.addLabeledLineEdit("Text Fonts:")
        self.textColors = self.textSettings.addLabeledLineEdit("Text Colors:")
        vbox.addWidget(self.textSettings)

        # Isoline Streamline Settings 
        self.streamSettings = QFramedWidget('Streamlines Settings')
        self.streamDirection = self.streamSettings.addLabeledLineEdit("Directions:")
        self.streamScale = self.streamSettings.addLabeledLineEdit("Scales:")
        self.streamAngle = self.streamSettings.addLabeledLineEdit("Angles:")
        self.streamSpacing = self.streamSettings.addLabeledLineEdit("Spacings:")
        vbox.addWidget(self.streamSettings)
        
        self.setWidget(frame)
        
        self.initValues()
        self.setToolTips()
    def initValues(self):

        # Init common area
        self.initCommonValues()
        
        #Init range section
        self.initRangeValues()

        #Init text labels section
        if self.gm.label=="n":
            self.textLabelsOnOff.setChecked("No")
        else:
            self.textLabelsOnOff.setChecked("Yes")
        self.textFonts.setText(repr(self.gm.text))
        self.textColors.setText(repr(self.gm.textcolors))

        #init Streamlines section
        self.streamDirection.setText(repr(self.gm.clockwise))
        self.streamScale.setText(repr(self.gm.scale))
        self.streamAngle.setText(repr(self.gm.angle))
        self.streamSpacing.setText(repr(self.gm.spacing))
        

    def setToolTips(self):
        self.textFonts.setToolTip("Text Font numbers")
        self.textColors.setToolTip("Text Colors")
        self.streamDirection.setToolTip("draw directional arrows\n+-(0,1,2) indicate none/clockwise/clokwise on y axis >0/clockwise on x axis positive\nnegative value inverts behaviour")
        self.streamScale.setToolTip("scales the directional arrows length")
        self.streamAngle.setToolTip("directional arrows head angle")
        self.streamSpacing.setToolTip("scales spacing between directional arrows")

    def applyChanges(self):
        self.applyCommonChanges()
        self.applyRangeSettings()
        # Applies changes to text
        self.gm.label = str(self.textLabelsOnOff.buttonGroup.button(self.textLabelsOnOff.buttonGroup.checkedId()).text()).lower()[0]
        self.gm.text = eval(str(self.textFonts.text()))
        self.gm.textcolors = eval(str(self.textColors.text()))
        #Applies changes to streamlines
        self.gm.clockwise = eval(str(self.streamDirection.text()))
        self.gm.scale = eval(str(self.streamScale.text()))
        self.gm.angle = eval(str(self.streamAngle.text()))
        self.gm.spacing = eval(str(self.streamSpacing.text()))

        
class QContourEditor():
    def contoursSettings(self, target):
        
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

class QBoxfillEditor(QtGui.QScrollArea,VCSGMs,VCSGMRanges):

    def __init__(self, parent=None, gm=None):
        QtGui.QScrollArea.__init__(self, parent)
        vbox = QtGui.QVBoxLayout()
        # Set up the scrollbar
        widgetWrapper = QtGui.QFrame()
        widgetWrapper.setLayout(vbox)
        self.parent=parent
        self.root=parent.root
        self.gmAttributes = ['boxfill_type', 'color_1', 'datawc_calendar', 'datawc_timeunits', 'datawc_x1', 'datawc_x2', 'datawc_y1', 'datawc_y2', 'levels','ext_1', 'ext_2', 'fillareacolors', 'fillareaindices', 'fillareastyle', 'legend', 'level_1', 'level_2', 'missing', 'projection', 'xaxisconvert', 'xmtics1', 'xmtics2', 'xticlabels1', 'xticlabels2', 'yaxisconvert', 'ymtics1', 'ymtics2', 'yticlabels1', 'yticlabels2']
        self.gm = self.root.canvas[0].getboxfill(gm)
        self.saveOriginalValues()
        
        # 'define boxfill attributes + boxfill type radio buttons'
        #hbox1 = QtGui.QHBoxLayout()
        #hbox1.addWidget(QtGui.QLabel('Define boxfill attribute values'))

        ## self.boxfillTypeButtonGroup = QRadioButtonFrame('Boxfill type:')
        ## self.boxfillTypeButtonGroup.addButtonList(['linear', 'log10', 'custom'])
        ## hbox1.addWidget(self.boxfillTypeButtonGroup)
                                             
        #vbox.addLayout(hbox1)

        self.setupCommonSection()
        
        # Boxfill General Settings 
        generalSettings = QFramedWidget('Boxfill Settings')
        self.boxfillTypeButtonGroup = generalSettings.addRadioFrame('Boxfill type:',
                                                                    ['linear', 'log10', 'custom'])
        self.boxfillTypeButtonGroup.setToolTip("The boxfill type defines how the legend boxes and labels are picked\nlinear: uses colors from color1 to color2 and uses this number of colors to create levels ranging from level1 to level2\nlog10: same as linear but the levels are created using a log10 scale.custom: colors to use are specified by the user in any order, as well as the level range for each boxes")
        #hbox1.addWidget(self.boxfillTypeButtonGroup)
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
        self.linLogSettings = QFramedWidget('Linear and Log Settings')

        self.level1LineEdit = self.linLogSettings.addLabeledLineEdit('Level 1:')
        self.level2LineEdit = self.linLogSettings.addLabeledLineEdit('Level 2:',
                                                                newRow = False)
        self.color1LineEdit = self.linLogSettings.addLabeledLineEdit('Color 1:')
        self.color2LineEdit = self.linLogSettings.addLabeledLineEdit('Color 2:',
                                                                newRow = False)        
        vbox.addWidget(self.linLogSettings)
        
        # Custom Settings
        self.customSettings = QFramedWidget('Custom Settings')

        self.rangeSettings(self.customSettings)
        
        vbox.addWidget(self.customSettings)

        # Init values
        self.initValues()
        self.setToolTips()

        self.setWidget(widgetWrapper)

        # Connect Signals
        self.connect(self.spacingButtonGroup.getButton('Linear'),
                     QtCore.SIGNAL('pressed()'),
                     lambda : self.setEnabledLogLineEdits(False))
        self.connect(self.spacingButtonGroup.getButton('Log'),
                     QtCore.SIGNAL('pressed()'),
                     lambda : self.setEnabledLogLineEdits(True))
        self.connect(self.boxfillTypeButtonGroup.buttonGroup,
                     QtCore.SIGNAL('buttonClicked(int)'),
                     self.clickedBoxType)


    def applyChanges(self):
        self.applyCommonChanges()
        self.boxfill_type = str(self.boxfillTypeButtonGroup.buttonGroup.button(self.boxfillTypeButtonGroup.buttonGroup.checkedId()).text())
        self.gm.level_1 = eval(str(self.level1LineEdit.text()))
        self.gm.level_2 = eval(str(self.level2LineEdit.text()))
        self.gm.color_1 = eval(str(self.color1LineEdit.text()))
        self.gm.color_2 = eval(str(self.color2LineEdit.text()))
        self.gm.legend = eval(str(self.legendLineEdit.text()))
        self.gm.ext_1 = str(self.ext1ButtonGroup.buttonGroup.button(self.ext1ButtonGroup.buttonGroup.checkedId()).text()).lower()[0]
        self.gm.ext2 = str(self.ext2ButtonGroup.buttonGroup.button(self.ext2ButtonGroup.buttonGroup.checkedId()).text()).lower()[0]
        self.gm.missing = eval(str(self.missingLineEdit.text()))
        self.applyRangeSettings()
        

    def clickedBoxType(self,*args):

        cid = self.boxfillTypeButtonGroup.buttonGroup.checkedId()
        if str(self.boxfillTypeButtonGroup.buttonGroup.button(cid).text()) == "custom":
            self.linLogSettings.setEnabled(False)
            self.customSettings.setEnabled(True)
        else:
            self.linLogSettings.setEnabled(True)
            self.customSettings.setEnabled(False)
        
    def initValues(self):

        # Init common area
        self.initCommonValues()
        
        # Init Line Edit Text
        self.missingLineEdit.setText(str(self.gm.missing))
        self.legendLineEdit.setText('None')
        self.level1LineEdit.setText(str(self.gm.level_1))
        self.level2LineEdit.setText(str(self.gm.level_2))
        self.color1LineEdit.setText(str(self.gm.color_1))
        self.color2LineEdit.setText(str(self.gm.color_2))

        # Init selected radio buttons
        self.boxfillTypeButtonGroup.setChecked(self.gm.boxfill_type)
        self.clickedBoxType()
        
        if self.gm.ext_1 == "n":
            self.ext1ButtonGroup.setChecked('No')
        else:
            self.ext1ButtonGroup.setChecked('Yes')
        if self.gm.ext_2 == "n":
            self.ext2ButtonGroup.setChecked('No')
        else:
            self.ext2ButtonGroup.setChecked('Yes')

        #Init range section
        self.initRangeValues()
        

    def setToolTips(self):
        self.missingLineEdit.setToolTip("Set the missing color index value. The colormap\nranges from 0 to 255, enter the desired color index value 0\nthrough 255.")
        self.ext1ButtonGroup.setToolTip("Turn on 1st arrow on legend")
        self.ext2ButtonGroup.setToolTip("Turn on 2nd arrow on legend")
        self.legendLineEdit.setToolTip("Specify the desired legend labels.\nFor example:\n None -- Allow VCS to generate legend labels\n(), or [ ], or { } -- No legend  labels\n [0, 10, 20] or { 0:'0', 10:'10', 20:'20' }\n[ 0, 10 ] or { 0:'text', 10:'more text'}")
        self.level1LineEdit.setToolTip("The minimum data value. If level 1 is set to '1e+20',\nthen VCS will select the level.")
        self.level2LineEdit.setToolTip("The maximum data value. If level 2 is set to '1e+20',\nthen VCS will select the level.")
        self.color1LineEdit.setToolTip("The minimum color range index value. The colormap\nranges from 0 to 255, but only color indices 0\nthrough 239 can be changed.")
        self.color2LineEdit.setToolTip("The maximum color range index value. The colormap\nranges from 0 to 255, but only color indices 0\nthrough 239 can be changed.")
        


            
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

    def addLabeledSlider(self, text, newRow=True,divider=1):
        if newRow == True:
            self.newRow()
            
        labeledSlider = QLabeledSlider(text,divider=divider)
        self.addWidget(labeledSlider)
        return labeledSlider.getSlider()

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

        
