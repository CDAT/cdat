#############################################################################
#############################################################################
#  File:    ProgressBar.py                                                  #
#  Date:    05-Dec-2007                                                     #
#############################################################################
#############################################################################
 
import Pmw
from Tkinter import *
import geoparse
import exceptions

class ProgressBarCancelledException(exceptions.Exception):
    pass

class ProgressBar(Toplevel):

    def __init__ (self,parent,title):
        
        Toplevel.__init__(self,parent)
        self.title(title)

        self.__parent = parent
        self.__barWidth = 500
        self.__barHeight = 25

        self.__setPosition()
        
        self.__label = Label(self)
        self.__label.grid(row=2,sticky='w') 

        self.__canvas = Canvas(self,
                               bg='white',
                               borderwidth=1,
                               relief='sunken',
                               width=self.__barWidth,
                               height=self.__barHeight)
        self.__canvas.grid(row=6,padx=10,pady=10)

        box = Pmw.ButtonBox(self)
        box.add('Cancel', command = self.cancel)
        box.setdefault('Cancel')
        box.alignbuttons()
        box.grid(row=7)
                
        # Override DELETE_WINDOW handler to do nothing.
        self.protocol ('WM_DELETE_WINDOW', self.onClose)

        # Whether user clicked Cancel.
        self.__wasCancelled = False
        
    def cancel(self):
        self.__wasCancelled = True
        
    def update(self, text, ratio):
        '''Raise exceptions if user clicked Cancel.'''
        if self.__wasCancelled:
            self.__wasCancelled = False
            raise ProgressBarCancelledException
        self.__label.config(text=text)
        self.__canvas.delete(ALL)
        self.__canvas.create_rectangle(0, 0, self.__barWidth*ratio, self.__barHeight, fill='gray')
        pos = (self.__barWidth/2, self.__barHeight/2)
        val = '%d'%(100*ratio) + '%'
        self.__canvas.create_text(pos,text=val)
        Toplevel.update(self)
    
    def deiconify(self):
        '''Override superclass deiconify to first reset position.'''
        self.__setPosition()
        Toplevel.deiconify(self)

    def __setPosition(self):
        '''Position the dialog box relative to parent.'''
        self.transient(self.__parent)
        xpos = int(geoparse.get_x (self.__parent.geometry())) + 100
        ypos = int(geoparse.get_y (self.__parent.geometry())) + 100
        self.geometry ('+' + str(xpos) + '+' + str(ypos))

    def onClose(self):
        pass
