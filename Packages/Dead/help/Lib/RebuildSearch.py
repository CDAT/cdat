#############################################################################
#############################################################################
#  File:    RebuildSearch.py                                                #
#  Date:    04-Dec-2007                                                     #
#############################################################################
#############################################################################
 
from Tkinter import *
import Pmw
import geoparse

class RebuildSearch(Toplevel):

    def __init__(self, parent, onClose, depth, onChangeSearchDepth):
        
        Toplevel.__init__ (self, parent)
        self.transient(parent)       
        self.title ('Rebuild Search')
        self.onClose = onClose
        self.onChangeSearchDepth = onChangeSearchDepth

        # Position the dialog box relative to parent.
        xpos = int(geoparse.get_x (parent.geometry())) + 100
        ypos = int(geoparse.get_y (parent.geometry())) + 100
        self.geometry ('+' + str(xpos) + '+' + str(ypos))

        Label(self).grid(row=1)
        Label(self,text='This will rebuild the search table.').grid(row=5)
        Label(self).grid(row=7)
        Label(self,text='A large search depth may').grid(row=9)
        Label(self,text='take a long time.').grid(row=10)

        Label(self).grid(row=13)
        
        self.depthCounter = Pmw.Counter(self,
                                        labelpos='w',
                                        label_text='Search depth:',
                                        entryfield_value=depth,
                                        entryfield_validate = {'validator' : 'integer',
                                                               'min' : 1,
                                                               'max' : 4},
                                        entry_width = 2,
                                        entry_bg = 'white'
                                        )
        self.depthCounter.grid(row=15)

        Label(self).grid(row=17)

        Label(self,text='Click OK to proceed.').grid(row=20)

        box = Pmw.ButtonBox(self)
        box.add('OK', command = self.onOK)
        box.add('Cancel', command = self.close)
        box.setdefault('OK')
        box.alignbuttons()
        box.grid(row=25)

        # Override DELETE_WINDOW handler to a local callback.
        self.protocol ('WM_DELETE_WINDOW', self.close)
        
    def onOK(self):
        self.withdraw()
        self.onChangeSearchDepth(int(self.depthCounter.get()))
        self.close()
        
    # Event handler when user closes the window.
    def close(self):
        self.onClose()
        self.destroy()
        

