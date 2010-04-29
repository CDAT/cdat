#####################################################################
#####################################################################
#                                                                   #
#  File:  MainFrame.py                                              #
#  Date:  31-Oct-2007                                               #
#  Desc:  Form for the main display of the CDAT help application.   #
#                                                                   #
#####################################################################
#####################################################################

import Pmw
import sys
import threading
import Tkinter
import os

import About
import CDATHelp
import ProgressBar
import QueryFrame
import RebuildSearch
import ResultsFrame

class MainFrame(Tkinter.Frame):

    def __init__(self, master):
        
        Tkinter.Frame.__init__(self, master)
        
        self.__master = master
        self.__about  = 0
        self.__rebuildSearch = 0
        
        master.title('CDAT Help')

        progressBar = ProgressBar.ProgressBar(master,'CDAT Help Progress')
        progressBar.withdraw()
        progressBar.resizable(height=0, width=0)

        self.__cdatHelp = CDATHelp.CDATHelp(progressBar)        
        
        # Create and install the MenuBar.
        self.__menuBar = Pmw.MainMenuBar(master)
        master.config(menu=self.__menuBar)
        self.__menuBar.addmenu('File', '')
        self.__menuBar.addmenuitem('File', 'command',
                                   label = 'Exit',
                                   command = sys.exit)
        self.__menuBar.addmenu('Options', '')
        self.__menuBar.addmenuitem('Options', 'command',
                                   label = 'Rebuild search',
                                   command = self.__rebuildSearchDialog)

        self.__menuBar.addmenu('Help', '')
        self.__menuBar.addmenuitem('Help', 'command',
                                   label = 'About',
                                   command = self.__aboutDialog)

        # Create vertical panes container.
        self.__panes = Pmw.PanedWidget(master, orient = 'horizontal')
        
        self.__panes.add('query', min=0.2)
        self.__queryFrame = QueryFrame.QueryFrame(self.__panes.pane('query'),self.__cdatHelp)
        self.__queryFrame.pack(expand=1, fill='both')

        self.__panes.add('results')
        self.__resultsFrame = ResultsFrame.ResultsFrame(self.__panes.pane('results'))
        self.__resultsFrame.pack(expand=1, fill='both')
        self.__panes.pack(fill = 'both', expand = 1, padx = 3, pady = 3)

        self.__queryFrame.setResultsSetter(self.__resultsFrame.set)

        self.startupTimer = threading.Timer (0.0, self.onStartupTimer)
        self.startupTimer.start()

    def onStartupTimer(self):
        self.startupTimer.cancel()
        self.__queryFrame.addContent(2)
        
    def __onRebuildSearchClose(self):
        self.__rebuildSearch = 0
        
    def __rebuildSearchDialog(self):
        if self.__rebuildSearch != 0:
            return
        self.__rebuildSearch = RebuildSearch.RebuildSearch(self.__master,
                                                           self.__onRebuildSearchClose,
                                                           self.__cdatHelp.getDepth(),
                                                           self.__onRebuildOK)
        self.__rebuildSearch.resizable(height=0, width=0)

    def __onRebuildOK(self,depth):
        self.__queryFrame.addContent(depth,True)
        
    def __onAboutClose(self):
        self.__about = 0

    def __aboutDialog(self):
        if self.__about != 0:
            return
        self.__about = About.About(self.__master, self.__onAboutClose)
        self.__about.resizable(height=0, width=0)

        
