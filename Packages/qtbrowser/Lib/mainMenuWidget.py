from PyQt4 import QtGui, QtCore
import os
import commandsRecorderWidget
import customizeVCDAT
import genutil,cdutil
import preFunctionPopUpWidget
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

        stats = self.pcmdiTools.addMenu("Statistics")
        stats.setTearOffEnabled(True)
        self.statsFuncs = {"Mean" : {"func":cdutil.averager,"nargsMin":1,"nargsMax":2},
                           "Variance" :{"func":genutil.statistics.variance,"nargsMin":1,"nargsMax":2,"choices":["centered","biased","max_pct_missingoptions"]},
                           "Standard Deviation" : {"func":genutil.statistics.std,"nargsMin":1,"nargsMax":2,"choices":["centered","biased","max_pct_missingoptions"]},
                           "Root Mean Square" : {"func":genutil.statistics.rms,"nargsMin":2,"nargsMax":3,"choices":["centered","biased","max_pct_missingoptions"]},
                           "Correlation" : {"func":genutil.statistics.correlation,"nargsMin":2,"nargsMax":3,"choices":["centered","biased","max_pct_missingoptions"]},
                           "Lagged Corelation" : {"func":genutil.statistics.laggedcorrelation,"nargsMin":2,"nargsMax":2,"choices":["centered","partial","biased","noloop",("lag",[None,len])]},
                           "Covariance" : {"func":genutil.statistics.covariance,"nargsMin":2,"nargsMax":3,"choices":["centered","biased","max_pct_missingoptions"]},
                           "Lagged Covariance" : {"func":genutil.statistics.laggedcovariance,"nargsMin":2,"nargsMax":2,"choices":["centered","partial","noloop",("lag",[None,len])]},
                           "Autocorrelation" : {"func":genutil.statistics.autocorrelation,"nargsMin":1,"nargsMax":1,"choices":["centered","partial","biased","noloop",("lag",[None,len])]},
                           "Autocovariance" : {"func":genutil.statistics.autocovariance,"nargsMin":1,"nargsMax":1,"choices":["centered","partial","noloop",("lag",[None,len])]},
                           "Mean Absolute Difference" : {"func":genutil.statistics.meanabsdiff,"nargsMin":2,"nargsMax":3,"choices":["centered",]},
                           "Linear Regression": {"func":genutil.statistics.linearregression,"nargsMin":1,"nargsMax":2,"choices":[("error",[0,1,2,3]),"probability","nointercept","noslope"]},
                           "Geometric Mean":{"func":genutil.statistics.geometricmean,"nargsMin":1,"nargsMax":1},
                           "Median":{"func":genutil.statistics.median,"nargsMin":1,"nargsMax":1},
                           "Rank (in %)":{"func":genutil.statistics.rank,"nargsMin":1,"nargsMax":1},
                           }
        for nm in sorted(self.statsFuncs.keys()):
            a = stats.addAction(nm)
            a.setToolTip(self.statsFuncs[nm]["func"].__doc__)
        self.connect(stats,QtCore.SIGNAL("triggered(QAction *)"),self.stats)

        vert = self.pcmdiTools.addMenu("Vertical Dims")
        vert.setTearOffEnabled(True)
        self.vertFuncs = {"Reconstruct Pressure: P=B*Ps+A*P0" : {"func":cdutil.vertical.reconstructPressureFromHybrid,"nargsMin":4,"nargsMax":4,"axes":False},
                          "Linear interpolation" : {"func":cdutil.vertical.linearInterpolation,"nargsMin":2,"nargsMax":3,"axes":False},
                          "Log-Linear interpolation" : {"func":cdutil.vertical.logLinearInterpolation,"nargsMin":2,"nargsMax":3,"axes":False},
                          }
        for nm in sorted(self.vertFuncs.keys()):
            a = vert.addAction(nm)
            a.setToolTip(self.vertFuncs[nm]["func"].__doc__)
        self.connect(vert,QtCore.SIGNAL("triggered(QAction *)"),self.vert)
        
        filters = self.pcmdiTools.addMenu("Filters")
        filters.setTearOffEnabled(True)
        self.filterFuncs = {"Running Average" : {"func":genutil.filters.runningaverage,"nargsMin":1,"nargsMax":1,"choices":[("N",[len,]),],"multiAxes":False},
                            "121 Filter" : {"func":genutil.filters.smooth121,"nargsMin":1,"nargsMax":1,"multiAxes":False},
                            "Custom Filter" : {"func":genutil.filters.custom1D,"nargsMin":2,"nargsMax":2,"multiAxes":False},
                            }
        for nm in sorted(self.filterFuncs.keys()):
            a = filters.addAction(nm)
            a.setToolTip(self.filterFuncs[nm]["func"].__doc__)
        self.connect(filters,QtCore.SIGNAL("triggered(QAction *)"),self.filters)
        
        nsdfiles = self.pcmdiTools.addMenu("Not Self Describing Files")
        nsdfiles.setTearOffEnabled(True)
        self.nsdfilesFuncs = {"Read ASCII File" : {"func":genutil.ASCII.readAscii,
                                                   "nargsMin":0,"nargsMax":0,
                                                   "choices":[("header",list(range(50))),],
                                                   "axes":False,
                                                   "entries":["ids","shape","next","separators"],
                                                   "fileEntries":["text_file",]},
                              "Read ASCII File in Columns" : {"func":genutil.ASCII.readAsciiCols,
                                                              "nargsMin":0,"nargsMax":0,
                                                              "choices":[("header",list(range(50))),("cskip",list(range(25))),("cskip_type",["columns","rows"]),"axis","idrow"],
                                                              "axes":False,
                                                              "entries":["ids","separators"],
                                                              "fileEntries":["text_file",]},
                            }
        for nm in sorted(self.nsdfilesFuncs.keys()):
            a = nsdfiles.addAction(nm)
            a.setToolTip(self.nsdfilesFuncs[nm]["func"].__doc__)
        self.connect(nsdfiles,QtCore.SIGNAL("triggered(QAction *)"),self.nsdfiles)


        self.errorMsg=QtGui.QErrorMessage()
        self.errorMsg.hide()

    def stats(self,action):
        nm = str(action.text())
        self.pop = preFunctionPopUpWidget.preFuncPopUp(parent=self,defs=self.statsFuncs[nm])
        self.pop.show()
    def vert(self,action):
        nm = str(action.text())
        self.pop = preFunctionPopUpWidget.preFuncPopUp(parent=self,defs=self.vertFuncs[nm])
    def filters(self,action):
        nm = str(action.text())
        self.pop = preFunctionPopUpWidget.preFuncPopUp(parent=self,defs=self.filterFuncs[nm])
    def nsdfiles(self,action):
        nm = str(action.text())
        self.pop = preFunctionPopUpWidget.preFuncPopUp(parent=self,defs=self.nsdfilesFuncs[nm])
        
        
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

                
