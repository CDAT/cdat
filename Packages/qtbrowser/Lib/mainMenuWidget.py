from PyQt4 import QtGui, QtCore
import os
import commandsRecorderWidget
import customizeVCDAT
import genutil,cdutil

class QMenuWidget(QtGui.QMenuBar):
    def __init__(self, parent=None):
        QtGui.QMenuBar.__init__(self, parent)
        
        self.file = self.addMenu('&File')
        self.pref = self.addMenu('&Preferences')
        self.tools = self.addMenu('&Tools')
        self.pcmdiTools = self.addMenu('&PCMDITools')
        self.pcmdiTools.setTearOffEnabled(True)
        self.help = self.addMenu('&Help')
        self.root=parent.root
        
        recordTeachingAction = self.tools.addAction('Record Commands')
        recordTeachingAction.setCheckable(True)
        recordTeachingAction.setChecked(customizeVCDAT.recordCommands)
        
        viewTeachingAction = self.tools.addAction('View Teaching Commands')

        self.connect(viewTeachingAction, QtCore.SIGNAL('triggered ()'),
                     self.root.recorder.show)
        ## self.connect(closeTeachingAction, QtCore.SIGNAL('triggered ()'),
        ##              self.closeTeachingCommands)        


        self.time = self.pcmdiTools.addMenu("Time Tools")
        self.time.setTearOffEnabled(True)
        m = self.time.addMenu("Bounds Set")
        m.setTearOffEnabled(True)
        m.addAction("Set Bounds For Yearly Data")
        m.addAction("Set Bounds For Monthly Data")
        m.addAction("Set Bounds For Daily Data")
        m.addAction("Set Bounds For Twice-daily Data")
        m.addAction("Set Bounds For 6-Hourly Data")
        m.addAction("Set Bounds For Hourly Data")
        m.addAction("Set Bounds For X-Daily Data")
        self.connect(m,QtCore.SIGNAL("triggered(QAction *)"),self.setBounds)
        self.time.addSeparator()
        for t in ["Extract","Climatology","Departures"]:
            m = self.time.addMenu(t)
            m.setTearOffEnabled(True)
            m.addAction("Annual Means")
            m.addSeparator()
            m.addAction("Seasonal Means")
            for s in ["DJF","MAM","JJA","SON"]:
                m.addAction(s)
            m.addSeparator()
            for s in ["Monthly Means",
                      "JAN","FEB","MAR",
                      "APR","MAY","JUN",
                      "JUL","AUG","SEP",
                      "OCT","NOV","DEC"]:
                m.addAction(s)
            self.connect(m,QtCore.SIGNAL("triggered(QAction *)"),self.seasons)

        self.stats = self.pcmdiTools.addMenu("Statistics")
        self.stats.setTearOffEnabled(True)
        self.vert = self.pcmdiTools.addMenu("Vertical Dims")
        self.vert.setTearOffEnabled(True)
        self.filters = self.pcmdiTools.addMenu("Filters")
        self.filters.setTearOffEnabled(True)
        self.nsdfiles = self.pcmdiTools.addMenu("Not Self Describing Files")
        self.nsdfiles.setTearOffEnabled(True)


    def seasons(self,action):
        menu = str(action.parentWidget().title())
        nm = str(action.text())
        rec = "## Computing "
        ## First which season ?
        if nm == "Annual Means":
            func = cdutil.times.YEAR
            funcnm = 'cdutil.times.YEAR'
        elif nm == "Seasonal Means":
            func = cdutil.times.SEASONALCYCLE
            funcnm = 'cdutil.times.SEASONALCYCLE'
        elif nm == "Monthly Means":
            func = cdutil.times.ANNUALCYCLE
            funcnm = 'cdutil.times.ANNUALCYCLE'
        else:
            func = getattr(cdutil.times,nm)
            funcnm = "cdutil.times.%s" % nm
        ## Now which operator?
        if menu == "Climatology":
            func = func.climatology
            funcnm+=".climatology"
            rec = "climatological "
        elif menu == "Departures":
            func=func.departures
            funcnm+=".departures"
            rec = "departures from "
        rec += nm.lower()
        selectedVars=self.root.definedVar.widget.getSelectedDefinedVariables()
        for v in selectedVars:
            tmp = func(v)
            ext = "".join(nm.lower().split())
            newid = "%s_%s" % (v.id,ext)
            if menu != "Extract":
                newid+=menu.lower()
            tmp.id = newid
            self.root.definedVar.widget.addVariable(tmp)
            self.root.record(rec)
            self.root.record("%s = %s(%s)" % (newid,funcnm,v.id))
            
        
    def setBounds(self,action):
        nm = str(action.text())
        if nm == "Set Bounds For X-Daily Data":
            self.bDialog = QtGui.QInputDialog()
            ## self.bDialog.setInputMode(QtGui.QInputDialog.DoubleInput)
            val,ok = self.bDialog.getDouble(self,"Reset Time Bounds to X-Hourly", "Frequency (# of samples per day)")
            if ok is False or val <= 0.:
                return
        selectedVars=self.root.definedVar.widget.getSelectedDefinedVariables()
        for v in selectedVars:
            if nm == "Set Bounds For Yearly Data":
                cdutil.times.setTimeBoundsYearly(v)
                self.root.record("## Set Bounds For Yearly Data")
                self.root.record("cdutil.times.setTimeBoundsYearly(%s)" % v.id)
            elif nm == "Set Bounds For Monthly Data":
                cdutil.times.setTimeBoundsMonthly(v)
                self.root.record("## Set Bounds For Monthly Data")
                self.root.record("cdutil.times.setTimeBoundsMonthly(%s)" % v.id)
            elif nm == "Set Bounds For Daily Data":
                cdutil.times.setTimeBoundsDaily(v)
                self.root.record("## Set Bounds For Daily Data")
                self.root.record("cdutil.times.setTimeBoundDaily(%s)" % v.id)
            elif nm == "Set Bounds For Twice-daily Data":
                cdutil.times.setTimeBoundsDaily(v,2)
                self.root.record("## Set Bounds For Twice-daily Data")
                self.root.record("cdutil.times.setTimeBoundDaily(%s,2)" % v.id)
            elif nm == "Set Bounds For 6-Hourly Data":
                cdutil.times.setTimeBoundsDaily(v,4)
                self.root.record("## Set Bounds For 6-Hourly Data")
                self.root.record("cdutil.times.setTimeBoundDaily(%s,4)" % v.id)
            elif nm == "Set Bounds For Hourly Data":
                cdutil.times.setTimeBoundsDaily(v,24)
                self.root.record("## Set Bounds For Hourly Data")
                self.root.record("cdutil.times.setTimeBoundDaily(%s,24)" % v.id)
            elif nm == "Set Bounds For X-Daily Data":
                cdutil.times.setTimeBoundsDaily(v,val)
                self.root.record("## Set Bounds For X-Daily Data")
                self.root.record("cdutil.times.setTimeBoundDaily(%s,%g)" % (v.id,val))

                
