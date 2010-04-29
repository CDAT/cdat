#####################################################################
#####################################################################
#                                                                   #
#  File:  MainMenu.py                                               #
#  Date:  16-Mar-2006                                               #
#  Desc:  Form for the main menu of the CDAT demo application.      #
#                                                                   #
#####################################################################
#####################################################################

import Tkinter
import Pmw
import sys
import os
from threading import Thread

import DemoSet
import About

#############################################################
# Class: MainMenu                                           #
# Descr: The main for of the CDAT demo app.                 #
#############################################################
class MainMenu (Tkinter.Frame):

    _demos = {}

    def __init__ (self, master, hide_vcdat=0, stdout=0):
        
        Tkinter.Frame.__init__ (self, master)
        
        self._master      = master
        self._stdout      = stdout
        self._hide_vcdat  = hide_vcdat
        self._about       = 0
        self._select      = 0
        self._idleEditor  = 0
        self._threadVCDAT = 0
        master.title ('CDAT Demo - Main Menu')

        #-----------------------------------
        # Create and install the MenuBar.
        #-----------------------------------

        self._menuBar = Pmw.MainMenuBar (master)
        master.config (menu=self._menuBar)

        # Add File
        self._menuBar.addmenu ('File', '')

        # Add File->Exit
        self._menuBar.addmenuitem ('File', 'command',
                                   label = 'Exit',
                                   command = sys.exit)

        # Add Applications
        self._menuBar.addmenu ('Applications', '')

        # Add Applications->...
        applications = [ ('Earth Sciences', self._cbackEarthSci),
                         ('Engineering',    self._cbackEngineering),
                         ('Air Pollution',  self._cbackAirPollution) ]
        for lbl, cmd in applications:
            self._menuBar.addmenuitem ('Applications', 'command',
                                       label = lbl,
                                       command = cmd)
            
        # Add Features
        self._menuBar.addmenu ('Features', '')

        # Add Features->...
        features = [ ('Visualization', self._cbackVisual),
                     ('Diagnostics',   self._cbackDiagnostics),
                     ('Xmgrace',       self._cbackXmgrace),
                     ('Data Analysis', self._cbackDataAnalysis),
                     ('Application Development', self._cbackAppDevel) ]
        
        for lbl, cmd in features:
            self._menuBar.addmenuitem ('Features', 'command',
                                       label = lbl,
                                       command = cmd)

 
        if not self._hide_vcdat:
            # Add Run CDAT
            self._menuBar.addmenu ('VCDAT', '')

            # Add Run CDAT->...
            runcdat = [ ('VCDAT (Lite)', self._cbackVCDATLite),
                        ('VCDAT (Full)', self._cbackVCDATFull) ]
            for lbl, cmd in runcdat:
                self._menuBar.addmenuitem ('VCDAT', 'command',
                                           label = lbl,
                                           command = cmd)

        # Add Help
        self._menuBar.addmenu ('Help', '')

        # Add Help->About
        self._menuBar.addmenuitem ('Help', 'command',
                                   label = 'About',
                                   command = self._aboutDialog)
        
        # Create buttons.
        if not self._hide_vcdat:
            background_fname = os.path.join (sys.exec_prefix, 'bin/images', 'main_background.gif') 
            buttons = [('Earth Sciences',   1, 23, self._cbackEarthSci,     self._onBtnEarthSciEnter),
                       ('Engineering',      1, 24, self._cbackEngineering,  self._onBtnEngineeringEnter),
                       ('Air Pollution',    1, 25, self._cbackAirPollution, self._onBtnAirPollutionEnter),
                       ('CDAT Overview',    1, 31, self._cbackCDATOverview, self._onBtnCDATOverviewEnter),
                       ('Run VCDAT (Lite)', 5, 28, self._cbackVCDATLite,    self._onBtnVCDATLiteEnter),
                       ('Run VCDAT (Full)', 5, 29, self._cbackVCDATFull,    self._onBtnVCDATFullEnter),
                       ('Visualization',    9, 27, self._cbackVisual,       self._onBtnVisualEnter),
                       ('Diagnostics',      9, 28, self._cbackDiagnostics,  self._onBtnDiagnosticsEnter),
                       ('Xmgrace',          9, 29, self._cbackXmgrace,      self._onBtnXmgraceEnter),
                       ('Data Analysis',    9, 30, self._cbackDataAnalysis, self._onBtnDataAnalysisEnter),
                       ('Application Development', 9, 31, self._cbackAppDevel, self._onBtnAppDevelEnter)]
        else:
            background_fname = os.path.join (sys.exec_prefix, 'bin/images', 'main_background2.gif') 
            buttons = [('Earth Sciences',   3, 23, self._cbackEarthSci,     self._onBtnEarthSciEnter),
                       ('Engineering',      3, 24, self._cbackEngineering,  self._onBtnEngineeringEnter),
                       ('Air Pollution',    3, 25, self._cbackAirPollution, self._onBtnAirPollutionEnter),
                       ('CDAT Overview',    3, 27, self._cbackCDATOverview, self._onBtnCDATOverviewEnter),
                       ('Visualization',    8, 23, self._cbackVisual,       self._onBtnVisualEnter),
                       ('Diagnostics',      8, 24, self._cbackDiagnostics,  self._onBtnDiagnosticsEnter),
                       ('Xmgrace',          8, 25, self._cbackXmgrace,      self._onBtnXmgraceEnter),
                       ('Data Analysis',    8, 26, self._cbackDataAnalysis, self._onBtnDataAnalysisEnter),
                       ('Application Development', 8, 27, self._cbackAppDevel, self._onBtnAppDevelEnter)]
 
        # Create a display frame container.
        self._display = Pmw.ScrolledFrame (master)
        self._display.pack (expand=1, fill='both')

        # Create the Main Menu.
        import MainDisplay
        self._curDisplay = MainDisplay.MainDisplay (self._display.interior(), background_fname, buttons, self._onBtnLeave)
        self._curDisplay.pack(expand=1, fill='both')
        self._curDisplay.update_idletasks()

        # Save the Main Menu display; will be used when
        # user clicks "Return to Main Menu" button.
        self._mainDisplay = self._curDisplay
        
        self._display.component('borderframe')['bd'] = 0
        self._display.component('clipper')['width'] = self._curDisplay.getWidth() + 4
        self._display.component('clipper')['height'] = self._curDisplay.getHeight() + 4
        
        # Create status display.
        self._statusBox = Pmw.Group (master, tag_pyclass = None)
        self._statusBox.pack (fill='x', padx=3, pady=2)
        self._statusText = Tkinter.Label (self._statusBox.interior())
        self._statusText.pack(side='left', padx = 5, pady = 2)
        self._setDefaultStatusText()
        self._setStatusText()

        # Demo sets configuration.
        self._demoNames = {
            'EARTH_SCI'     : ['Earth Sciences',          'earth_sciences.gif', '',                     '', ''],
            'ENGINEERING'   : ['Engineering',             'engineering.gif',    'engineering_ss.gif',   '', ''],
            'AIR_POLLUTION' : ['Air Pollution',           'air_pollution.gif',  '',                     '', ''],
            'CDAT_OVERVIEW' : ['Overview',                'cdat_overview.gif',  'cdat_overview_ss.gif', '', ''],
            'VISUAL'        : ['Visualization',           'visualization.gif',  '',                     '', ''],
            'DIAGNOSTICS'   : ['Diagnostics',             'diagnostics.gif',    '',                     '', ''],
            'XMGRACE'       : ['Xmgrace',                 'xmgrace.gif',        '',                     '', ''],
            'DATA_ANALYSIS' : ['Data Analysis',           'data_analysis.gif',  '',                     '', ''],
            'APP_DEVEL'     : ['Application Development', 'application_development.gif', '',            '', ''],
           }

        # Initialize demo sets to zero.
        for val in self._demoNames.values():
            name = val[0]
            self._demos[name] = 0
        
    def _setDefaultStatusText (self, arg=''):
        if arg=='':
            self._defaultStatusText = 'Welcome to the CDAT demo. Please select a topic.'
        else:
            self._defaultStatusText = arg
            
    def _onBtnLeave (self, event):
        self._setStatusText ()
        
    def _onBtnEarthSciEnter (self, event):
        self._setStatusText ('Select Earth Sciences topic.')
        
    def _onBtnEngineeringEnter (self, event):
        self._setStatusText ('Select Engineering topic.')
        
    def _onBtnAirPollutionEnter (self, event):
        self._setStatusText ('Select Air Pollution topic.')
        
    def _onBtnCDATOverviewEnter (self, event):
        self._setStatusText ('Select CDAT overview topic.')
        
    def _onBtnVisualEnter (self, event):
        self._setStatusText ('Select visualization topic.')
        
    def _onBtnVCDATLiteEnter (self, event):
        self._setStatusText('Run VCDAT in lite mode.')
        
    def _onBtnVCDATFullEnter (self, event):
        self._setStatusText('Run VCDAT in full mode.')
        
    def _onBtnDiagnosticsEnter (self, event):
        self._setStatusText ('Select diagnostics topic.')
        
    def _onBtnXmgraceEnter (self, event):
        self._setStatusText ('Select Xmgrace topic.')
        
    def _onBtnDataAnalysisEnter (self, event):
        self._setStatusText ('Select data analysis topic.')
        
    def _onBtnAppDevelEnter (self, event):
        self._setStatusText ('Select application development topic.')
        
    def _setStatusText (self, arg=''):
        if arg=='':
            self._statusText['text'] = self._defaultStatusText
        else:
            self._statusText['text'] = arg
        
    def _onAboutClose (self):
        self._about = 0

    def _aboutDialog (self):
        if self._about != 0:
            return
        self._about = About.About (self._master, self._onAboutClose)

        # Disable resize of splash screen.
        self._about.resizable (height=0, width=0)

    #----------------------------------------------------------------
    #  Called when user clicks "Return to Main Menu" button.
    #----------------------------------------------------------------
    def _onReturnDemo (self):

        # Remove current display
        self._curDisplay.pack_forget()

        # Set current display to be the Main Menu.
        self._curDisplay = self._mainDisplay
        self._curDisplay.pack()

        # Set ScrolledFrame dimensions.
        self._display.component('clipper')['width'] = self._curDisplay.getWidth() + 4
        self._display.component('clipper')['height'] = self._curDisplay.getHeight() + 4

        # Change title.
        self._master.title ('CDAT Demo - Main Menu')

        # Reset status text.
        self._setDefaultStatusText()
        self._setStatusText()
        
    #----------------------------------------------------
    # Callback for closing the source code viewer.
    #----------------------------------------------------
    def _closeEditor (self):
        self._idleEditor.top.withdraw()
        self._idleEditor = 0
        
    #----------------------------------------------------
    # Display the Idle editor window.
    #----------------------------------------------------
    def _showEditor (self, fname, onlyIfExists=False):

        # Bail if requesting an already existing editor, but editor not there.
        if onlyIfExists and not self._idleEditor:
            return

        # If editor already exists, just update the file.
        if self._idleEditor:
            self._idleEditor.top.deiconify()
            self._idleEditor.top.tkraise()
            self._idleEditor.io.loadfile (fname)
            return
        
        from idlelib import PyShell
        flist = PyShell.PyShellFileList(self._master)
        sys.ps1 = 'CDAT> '
        from idlelib import EditorWindow
        self._idleEditor = EditorWindow.EditorWindow (root = self._master,
                                                      filename = fname,
                                                      flist = flist)
        self._idleEditor.top.protocol("WM_DELETE_WINDOW", self._closeEditor)

        # Delete the menubar options from the editor.
        self._idleEditor.menubar.delete('File')
        self._idleEditor.menubar.delete('Edit')
        self._idleEditor.menubar.delete('Format')
        self._idleEditor.menubar.delete('Run')
        self._idleEditor.menubar.delete('Options')
        self._idleEditor.menubar.delete('Windows')
        self._idleEditor.menubar.delete('Help')

    def _cbackGeneric (self, keywd):
        index           = self._demoNames[keywd][0]
        fnameLogo       = self._demoNames[keywd][1]
        fnameScreenshot = self._demoNames[keywd][2]
        balloon         = self._demoNames[keywd][3]
        linky           = self._demoNames[keywd][4]

        # Change the status text.
        self._setDefaultStatusText ('CDAT ' + index + ' demo.')
        self._setStatusText ()

        # Remove current display.
        self._curDisplay.pack_forget()
        
        # Set current display to be the selected demo set.
        self._curDisplay = DemoSet.DemoSet (self._display.interior(),
                                            fnameLogo, fnameScreenshot, keywd, balloon, linky,
                                            self._stdout, self._setStatusText, self._onReturnDemo,
                                            self._showEditor)
        self._curDisplay.pack(expand=0)

        # Set ScrolledFrame dimensions.        
        self._display.component('clipper')['width']  = self._curDisplay.getWidth() + 4
        self._display.component('clipper')['height'] = self._curDisplay.getHeight() + 4
        
        # Change title.
        self._master.title ('CDAT Demo - ' + index)
        
    def _cbackEarthSci (self):
        self._cbackGeneric ('EARTH_SCI')
        
    def _cbackEngineering (self):
        self._cbackGeneric ('ENGINEERING')

    def _cbackAirPollution (self):
        self._cbackGeneric ('AIR_POLLUTION')
        
    def _cbackSpacePhys (self):
        self._cbackGeneric ('SPACE_PHYS')
    
    def _cbackCDATOverview (self):
        self._cbackGeneric ('CDAT_OVERVIEW')
        
    def _cbackVCDATLite (self):
        if self._threadVCDAT != 0:
            return 
        self._threadVCDAT = Thread(target = self._callVCDATLite)
        self._threadVCDAT.start()
        self._setVCDATSelectionState('disabled')
        
    def _cbackVCDATFull (self):
        if self._threadVCDAT != 0:
            return 
        self._threadVCDAT = Thread(target = self._callVCDATFull)
        self._threadVCDAT.start()
        self._setVCDATSelectionState('disabled')
        
    def _cbackVisual (self):
        self._cbackGeneric ('VISUAL')

    def _cbackDiagnostics (self):
        self._cbackGeneric ('DIAGNOSTICS')
        
    def _cbackXmgrace (self):
        self._cbackGeneric ('XMGRACE')
        
    def _cbackDataAnalysis (self):
        self._cbackGeneric ('DATA_ANALYSIS')

    def _cbackAppDevel (self):
        self._cbackGeneric ('APP_DEVEL')
        
    def _callVCDATLite (self):
        # Run VCDAT in a blocking call, then reset after it returns.
        os.system ('vcdat')
        self._threadVCDAT = 0 
        self._setVCDATSelectionState('active')
        
    def _callVCDATFull (self):
        # Run VCDAT in a blocking call, then reset after it returns.
        os.system ('vcdat -f')
        self._threadVCDAT = 0                
        self._setVCDATSelectionState('active')
        
    def _setVCDATSelectionState (self, state):
        self._mainDisplay.getButton('Run VCDAT (Lite)')['state'] = state
        self._mainDisplay.getButton('Run VCDAT (Full)')['state'] = state
        self._menuBar.component('VCDAT').entryconfig(0,state=state)
        self._menuBar.component('VCDAT').entryconfig(1,state=state)
        
