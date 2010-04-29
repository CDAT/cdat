#####################################################################
#####################################################################
#                                                                   #
#  File:  DemoSet.py                                                #
#  Date:  16-Mar-2006                                               #
#  Desc:  Form for a specific demo topic, and support functions.    #
#                                                                   #
#####################################################################
#####################################################################

import Tkinter
import Pmw
import time
import sys
import string
import os


###########################################################
#  Class: DemoSet                                         #
#  Descr: Form for a specific demo topic.                 #
###########################################################
class DemoSet (Tkinter.Frame):

    def __init__ (self, master, logofname,
                  picfname, keywd, balloon, linky,
                  stdio, setStatusText, onReturn, showEditor):

        Tkinter.Frame.__init__ (self, master)
        
        # Redirect standard I/O.
        if not stdio:
            sys.stdout = StdOut (0)
            sys.stderr = StdOut (0)
            sys.stdin  = StdIn ()

        self._master = master
        self._keywd = keywd
        self._threadDebug = 0
        self._showEditor = showEditor
        self._onReturn = onReturn
        self._picfname = picfname
        self._setStatusText = setStatusText

        fname = os.path.join (sys.exec_prefix, 'bin/images', logofname)
        self._logo = Tkinter.PhotoImage (file = fname)
        lab = Tkinter.Label (self, relief='sunken', bg='white', image=self._logo)
        lab.image = self._logo
        lab.pack (fill='both')

        # Create contents.
        if picfname == '':
            gotprogs = self._createProgListing()
            self._createButtons()
            if gotprogs:
                self._loadDemos()
        elif keywd == 'CDAT_OVERVIEW':
            self._createOverview()
            self._createButtons(True)
        else:
            self._createPicOnly(picfname, balloon, linky)
            self._createButtons(True)

    #----------------------------------------------------
    # Create CDAT overview content.
    #----------------------------------------------------
    def _createOverview (self):
        self._displayWidth = self._logo.width()
        self._displayHeight = 400
        self._display = Pmw.ScrolledCanvas(self,
                                           borderframe = 1,
                                           usehullsize = 1,
                                           hull_width  = self._displayWidth,
                                           hull_height = self._displayHeight,
                                           )
        from OverviewCanvas import overviewCanvas
        overviewCanvas (self._display)
        self._display.interior().xview_moveto (0.0)
        self._display.interior().yview_moveto (0.0)
        self._display.pack()
        self._display.resizescrollregion()
        
        
    #----------------------------------------------------
    # Create the demo screenshot.
    #----------------------------------------------------
    def _createPicOnly(self, picfname, balloon, linky):

        fname = os.path.join (sys.exec_prefix, 'bin/images', picfname)
        self._photo = Tkinter.PhotoImage (file = fname)

        self._displayWidth  = min (self._photo.width(),  self._logo.width())
        self._displayHeight = min (self._photo.height(), 400)
        self._display = Pmw.ScrolledCanvas(self,
                                           borderframe = 1,
                                           usehullsize = 1,
                                           hull_width  = self._displayWidth,
                                           hull_height = self._displayHeight,
                                           )
        item = self._display.create_image (0, 0, image=self._photo, anchor='nw')
        self._balloon = Pmw.Balloon(self, relmouse='both')
        self._balloon.tagbind(self._display.component('canvas'),
                              item,
                              'Click to go to the "Getting Started with VCDAT" web page.')

        # Make the canvas a clickable link to the web page.
        self._link = linky
        self._display.pack()
        self._display.resizescrollregion()
        self._display.component('canvas').bind('<1>', self._onPicLeftClick)
        self._display.component('canvas')['cursor'] = 'hand2'
        
    #----------------------------------------------------        
    # Event handler when user left-clicks on the pic.
    #----------------------------------------------------
    def _onPicLeftClick (self, event):
        import webbrowser
        webbrowser.open_new (self._link)

    #----------------------------------------------------
    # Create the selectable list of demo programs in the GUI.
    #----------------------------------------------------
    def _createProgListing(self):

        # Create vertical panes container.
        self._panes = Pmw.PanedWidget(self, orient = 'vertical')
        self._panes.pack(fill = 'both', expand = 1,
                         padx = 3, pady = 3)

        if self._keywd == 'AIR_POLLUTION':
            try:
                import ioapiTools
            except:
                self._panes.add('libmissing', min=0.2)
                lab = Tkinter.Label (
                    self._panes.pane('libmissing'),
                    relief = 'sunken',
                    anchor = 'nw',
                    justify = 'left',
                    bg = 'white',
                    padx = 10,
                    pady = 10,
                    wraplength = 400,
                    text = '\
Your CDAT installation is missing ioapiTools package needed \
to run Air Pollution demos. If you\'d like to run these demos, \
please install CDAT with the option:\n\n\
   --enable-ioapi\n\
')        
                lab.pack (expand = 1, fill='both')
                return False
        
        # Add 'proglist' pane.
        self._panes.add('proglist', min=0.2)
        self._programList = Pmw.ScrolledListBox (
            self._panes.pane('proglist'),
            selectioncommand = Pmw.busycallback(self._selectProg),
            label_text       = 'Select a program:',
            labelpos         = 'nw',
            vscrollmode      = 'dynamic',
            hscrollmode      = 'none',
            usehullsize      = 1,
            listbox_bg       = 'white',
            listbox_exportselection = 0)
        self._programList.pack(expand = 1, fill='both')

        # Placeholder for the description pane, created later.
        self._desc = 0

        return True
    
    def _resetStatusText (self, event):
        self._setStatusText()
    def _onBtnSourceEnter (self, event):
        if self._btnSource['state'] == 'normal':
            self._setStatusText('View the source code.')
    def _onBtnDebugEnter (self, event):
        if self._btnDebug['state'] == 'normal':
            self._setStatusText('Run demo in debugger.')
    def _onBtnReturnEnter (self, event):
        self._setStatusText('Return to main menu.')
            
    #----------------------------------------------------
    # Create buttons in the GUI. 
    #----------------------------------------------------
    def _createButtons(self,
                       onlyReturn=False):  # Whether to create only the 'Return' button.
        
        self._buttonFrame = Tkinter.Frame (self)
        self._buttonFrame.pack(fill='both')

        if not onlyReturn:
            # Add the source button.
            self._btnSource = Tkinter.Button (self._buttonFrame,
                                              text = 'View Source',
                                              state = 'disabled',
                                              command = self._viewSource)
            self._btnSource.bind ('<Enter>', self._onBtnSourceEnter)
            self._btnSource.bind ('<Leave>', self._resetStatusText)
            self._btnSource.pack (anchor='se', expand=1, side = 'left')

            # Add the debug button.
            self._btnDebug = Tkinter.Button (self._buttonFrame,
                                             text = 'Run in Debugger',
                                             state = 'disabled',
                                             command = self._runDebug)
            self._btnDebug.bind ('<Enter>', self._onBtnDebugEnter)
            self._btnDebug.bind ('<Leave>', self._resetStatusText)
            self._btnDebug.pack (anchor='s', expand=1, side = 'left')
            exitAnchor = 'sw'
        else:
            exitAnchor = 's'
            
        # Add the return button.
        self._btnReturn = Tkinter.Button (self._buttonFrame, 
                                          text = 'Return to Main Menu',
                                          command = self._onReturn)
        self._btnReturn.bind ('<Enter>', self._onBtnReturnEnter)
        self._btnReturn.bind ('<Leave>', self._resetStatusText)
        self._btnReturn.pack(anchor=exitAnchor, expand=1, side='left')

    #----------------------------------------------------
    # Threaded function for running pydebug.
    #----------------------------------------------------
    def _callPydebug (self):
        fname = self._getSelectedFile()
        if fname != '':
            os.putenv ('BP_PYDEBUG', '/tmp') 
            os.system ('pydebug ' + fname)
        self._threadDebug = 0

        # Reactivate the debugger button.
        self._btnDebug['state'] = 'normal'  
                

    #----------------------------------------------------
    # When user selects to run a program in the debugger. 
    #----------------------------------------------------
    def _runDebug (self):
        if self._threadDebug != 0:
            return
        from threading import Thread 
        self._threadDebug = Thread (target = self._callPydebug)
        self._threadDebug.start()
        self._btnDebug['state'] = 'disabled'
        
    #----------------------------------------------------
    # Returns name of source file currently selected,
    # or empty string (i.e. '')  if none selected.
    #----------------------------------------------------
    def _getSelectedFile (self):
        title = self._programList.getcurselection()[0]
        pos   = title.find('.')
        title = title [pos+3:]
        fname = self._demoTitles[title]
        return fname

    #----------------------------------------------------
    # When user selects to view source code of a program.
    #----------------------------------------------------
    def _viewSource (self):
        
        # Bail if user hadn't made a selection yet.
        fname = self._getSelectedFile()
        if fname == '':
            return

        self._showEditor (fname)
        
    #----------------------------------------------------
    # Callback for selecting a program from the list.
    #----------------------------------------------------
    def _selectProg (self):

        fname = self._getSelectedFile()
        if fname == '':
            return

        if not self._desc : 
            self._panes.add('desc', min = 0.2)
            self._desc = Pmw.ScrolledText (
                self._panes.pane('desc'),
                borderframe = 1,
                labelpos    = 'n',
                label_text  = 'Description',
                usehullsize = 1,
                hull_width  = 400,
                hull_height = 300,
                text_bg     = 'white',
                text_padx   = 10,
                text_pady   = 10,
                text_wrap   = 'word',
                text_state  = 'disabled')
            self._desc.pack (fill = 'both', expand = 1, pady = 4)
            self._panes.updatelayout()
        
        # Activate demo-specific buttons.
        self._btnSource['state'] = 'normal'
        if not self._threadDebug:
            self._btnDebug['state'] = 'normal'
                        
        # Display the description.
        self._desc.settext(self._demoDescs[fname])

        self._showEditor (fname, True)
        
    #----------------------------------------------------
    # Populate the program listing.
    #----------------------------------------------------    
    def _loadDemos (self):
        self._demoTitles = {}
        self._demoDescs = {}

        fname = os.path.join (sys.exec_prefix, 'bin/tutorials', 'index.py')
        exec open (fname, 'r')
        count = 0
        for category in index:
            for demo in category[1]:
                if string.find (demo[1], self._keywd) < 0:
                    continue
                count += 1
                self._programList.insert ('end', str(count) + '.  ' + demo[2])
                fname = os.path.join (sys.exec_prefix, 'bin/tutorials', demo[0])
                self._demoTitles[demo[2]] = fname
                self._demoDescs [fname] = demo[3]

    #----------------------------------------------------
    # Returns width of the contents.
    #----------------------------------------------------
    def getWidth (self):
        if self._picfname=='':
            result = self._logo.width()
        else:
            result = self._displayWidth #self._photo.width()
        #print 'getWidth', result
        return result
        
    #----------------------------------------------------
    # Returns height of the contents.
    #----------------------------------------------------
    def getHeight (self):
        if self._picfname=='':
            result = self._logo.height() + self._panes.component('hull')['height'] + 40
        else:
            result = self._logo.height() + self._display.component('hull')['height'] + 35
        #print 'getHeight', result
        return result
    
###########################################################
#  Class: StdIn                                           #
#  Descr: Override sys.stdin with this object to change   #
#         standard input with a sleep.                    #
###########################################################
class StdIn:
    def __init__(self):
        return
    def readline (self):
        time.sleep(1)


###########################################################
#  Class: StdOut                                          #
#  Descr: Override sys.stdout with this object to         #
#         divert output text to a different command.      #
###########################################################
class StdOut:
    def __init__(self, command):
        self._displayCommand = command
    def write(self, text):
        if not self._displayCommand:
            return
        self.displayCommand(text)

