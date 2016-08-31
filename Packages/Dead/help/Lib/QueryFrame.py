#####################################################################
#####################################################################
#                                                                   #
#  File:  QueryFrame.py                                             #
#  Date:  31-Oct-2007                                               #
#  Desc:  Frame to input user query for CDAT help.                  #
#                                                                   #
#####################################################################
#####################################################################

import os
import Pmw
import string
import subprocess
import sys
import thread
import threading
import time
import Tkinter

import CDATHelp
import HelpStdout

class QueryFrame (Pmw.NoteBook):

    def status(self,text):
        #print 'QueryFrame: %s'%text
        pass
    
    def __init__ (self, master, cdatHelp):
        
        Pmw.NoteBook.__init__(self,master)

        self.master = master
        self.cdatHelp = cdatHelp
        
        index = self.add('Index')                
        self.indexQuery = Pmw.EntryField(index,
                                         labelpos    = 'nw',
                                         label_text  = 'Look For:',
                                         entry_bg    = 'white',
                                         entry_width = 128,
                                         command     = self.indexQueryReturn,
                                         modifiedcommand = self.indexQueryModified)             
        self.indexQuery.pack(anchor='nw')                
        self.indexList = Pmw.ScrolledListBox(index,
                                             labelpos   = 'nw',
                                             label_text = '',
                                             listbox_bg = 'white',
                                             selectioncommand = self.indexListSelected)                
        self.indexList.pack(expand=1, fill='both')
        
        search = self.add('Search')
        self.searchFrame = Tkinter.Frame(search)
        self.searchFrame.pack(fill='both')
        
        self.searchLabel = Tkinter.Label(self.searchFrame, text='Search For:')
        self.searchLabel.pack(side='left')

        self.searchType = Pmw.RadioSelect(self.searchFrame,
                                          buttontype = 'radiobutton',
                                          orient     = 'horizontal',
                                          command    = self.searchTypeModified)
        self.searchType.add('Match all')
        self.searchType.add('Match any')
        self.searchType.pack(side='right')
        
        self.searchQuery = Pmw.EntryField(search,
                                          labelpos    = None,
                                          entry_bg    = 'white',
                                          entry_width = 128,
                                          command     = self.searchQueryReturn,
                                          modifiedcommand = self.searchQueryModified)

        self.searchQuery.pack(anchor='nw')  
        self.searchList = Pmw.ScrolledListBox(search,
                                              labelpos = 'nw',
                                              label_text = 'Found Documents:',
                                              listbox_bg = 'white',
                                              selectioncommand = self.searchListSelected)
        self.searchList.pack(expand=1, fill='both')

        # Set initial search type as inclusive.
        self.searchType.invoke('Match all')

        # Set 'Index' as the default page.
        self.selectpage('Index')

        # Function to call when a new page is selected.
        Pmw.NoteBook.configure(self,raisecommand = self.pageSelected)

    def addContent(self,depth,forceRebuild=False):
        '''Populates the GUI with indexes and search terms.'''
        
        fname = os.path.join('%s'%os.environ['HOME'],'PCMDI_GRAPHICS','cdathelp.sav')

        if forceRebuild:
            self.cdatHelp.clear()

        if forceRebuild or not self.cdatHelp.load(fname):
            self.cdatHelp.build(depth)
            self.cdatHelp.dump(fname)
        
        # Populate the index.
        self.indexList.setlist(self.cdatHelp.getIndexList())

        # Initial selection is the first entry in the list.
        if len(self.cdatHelp.getIndexList()) > 0:
            self.indexList.setvalue(self.cdatHelp.getIndexList()[0])
            self.indexQueryModified()
            self.indexListSelected()
        
    def pageSelected(self, text):
        '''Called when a new page is selected.'''
        if text == 'Index':
            self.indexQuery.component('entry').focus_set()
        elif text == 'Search':
            self.searchQuery.component('entry').focus_set()        

    def getIndexPosition(self,text):
        '''Returns position in the index list of the nearest incomplete match.'''
        for ii in range(len(self.cdatHelp.getIndexList())):
            entry = self.cdatHelp.getIndexList()[ii]
            short = entry[:len(text)]
            if short == text:
                return ii
            lower = string.lower(short)
            if lower == text:
                return ii
        return -1
    
    def indexQueryReturn(self):
        '''Called when user hits <Return> in the Index query.'''
        self.indexListSelected()
        listbox = self.indexList.component('listbox')
        listbox.focus_set()
        
    indexSelected = False

    def indexQueryModified(self):
        '''Called when Index query string is changed.'''
        if self.indexSelected:
            return
        value = self.indexQuery.getvalue()
        pos = self.getIndexPosition(value)
        if pos == -1:
            return            
        listbox = self.indexList.component('listbox')
        for ii in listbox.curselection():
            listbox.selection_clear(ii)
        listbox.yview(pos)
        listbox.selection_set(pos)
        listbox.activate(pos)
        
    def indexListSelected(self):
        '''Called when user makes a selection from the Index list.'''
        selection = self.indexList.getvalue()
        self.indexSelected = True
        self.indexQuery.setvalue(selection)
        self.indexSelected = False
        self.updateResults(selection)

    def searchTypeModified(self,tag):
        '''Called when user selects search type.'''
        self.searchQueryReturn()
        
    def searchQueryReturn(self):
        '''Called when user hits <Return> in the Search query.'''
        query = self.searchQuery.getvalue()
        query = string.lower(query)
        self.status('Looking for ' + query)
        lookup = self.cdatHelp.getSearchTable().get(query)
        
        value = self.searchType.getvalue()
        result = []
        if value == 'Match all':
            num = len(string.split(query))
            for key,val in lookup.iteritems():
                if len(val) == num:
                    result.append(key)
            self.searchList.setlist(result)
        else:
            result = lookup.keys()
        result.sort(self.compareLowercase)
        self.searchList.setlist(result)

    def compareLowercase(self, a, b):
        aa = string.lower(a)
        bb = string.lower(b)
        if aa > bb:
            return 1
        if aa== bb:
            return 0
        if aa < bb:
            return -1
        
    def searchQueryModified(self):
        '''Called when Search string is changed.'''
        #self.searchQueryReturn()
        pass
    
    def searchListSelected(self):
        '''Called when user makes a selection from the Search list.'''
        selection = self.searchList.getvalue()
        query = self.searchQuery.getvalue()
        query = string.lower(query)
        self.updateResults(selection,query)
        
    def setResultsSetter(self,setter):
        self.resultsSetter = setter

        if len(self.cdatHelp.getIndexList()) > 0:
            default = self.indexItems[0]
            self.indexList.setvalue(default)
            self.indexQuery.setvalue(default)
            self.updateResults((default,))

        self.pageSelected('Index')

    def updateResults(self,selection,query=''):
        if len(selection) == 0:
            return
        selection = selection[0]
        package = string.split(selection,'.')[0]

        old = sys.stdout
        new = HelpStdout.HelpStdout()
        sys.stdout = new
        
        try:
            cmd = 'import %s'%package
            exec cmd
            cmd = 'help(%s)'%selection
            exec cmd
        except:
            pass
        
        sys.stdout = old
        new.split()
        doc = new.get()      
        
        if not len(doc):
            doc = ['Help not available for %s'%selection]
            
        # Strip off trailing whitespace.
        for ii in range(len(doc)):
            doc[ii] = doc[ii].rstrip()
            
        # Write documentation contents to destination. 
        self.resultsSetter(doc,query)        

