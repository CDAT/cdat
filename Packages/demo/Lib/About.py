#############################################################################
#############################################################################
#  File:    about.py                                                        #
#  Author:  Velimir Mlaker, mlaker1@llnl.gov                                #
#  Date:    26-Jul-2005                                                     #
#############################################################################
#############################################################################
 
from Tkinter import *
import geoparse
import os

#############################################################################
#  Class: About                                                             #
#  Desc:  Dialog box for Help->About menu item.                             #
#############################################################################
class About (Toplevel):

    def __init__ (self, parent, on_close):
        
        Toplevel.__init__ (self, parent)
        self.transient(parent)       
        self.title ('About CDAT Demo')
        self.on_close = on_close

        # Position the dialog box relative to parent.
        xpos = int(geoparse.get_x (parent.geometry())) + 100
        ypos = int(geoparse.get_y (parent.geometry())) + 100
        self.geometry ('+' + str(xpos) + '+' + str(ypos))

        # Create background image.
        fname = os.path.join (sys.exec_prefix, 'bin/images', 'about.gif')
        self.photo = PhotoImage (file = fname)
        imageLabel = Label (self, bd = 0, image=self.photo)
        imageLabel.grid(rowspan = 15)

        # Create URL link.
        self.linkLabel = Label (self,
                                bd = 0,
                                bg = 'white',
                                fg = 'blue',
                                cursor = 'hand2',
                                text = 'http://www-pcmdi.llnl.gov/cdat')
        self.linkLabel.grid (row=11)
        self.linkLabel.bind ('<ButtonRelease-1>', self.onLeftClick)
        self.linkLabel.bind ('<Enter>', self.onEnter)
        self.linkLabel.bind ('<Leave>', self.onLeave)

        # Create close button.
        closeButton = Button (self, text='Close', command=self.close)
        closeButton.grid(row=14)

        # Override DELETE_WINDOW handler to a local callback.
        self.protocol ('WM_DELETE_WINDOW', self.close)

    # Event handler when user closes the window.
    def close (self):
        self.on_close()
        self.destroy()

    # Event handler when user left-clicks on the icon.
    def onLeftClick (self, event):
        import webbrowser
        webbrowser.open_new ('http://www-pcmdi.llnl.gov/cdat')
        
    def onEnter (self, event):
        self.linkLabel.config (underline=1)

    def onLeave (self, event):
        self.linkLabel.config (underline=0)
    



