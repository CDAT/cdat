#####################################################################
#####################################################################
#                                                                   #
#  File:  DemoSplash.py                                             #
#  Date:  16-Mar-2006                                               #
#  Desc:  Splash screen class.                                      #
#                                                                   #
#####################################################################
#####################################################################

import Tkinter
import sys
import os

class DemoSplash (Tkinter.Toplevel):

    def __init__(self):

        Tkinter.Toplevel.__init__ (self)
        
        self.title('Welcome to CDAT demo')

        # Frame will encapsulate photo and caption.
        self._frame = Tkinter.Frame (self)

        # Create the photo.
        fname = os.path.join (sys.exec_prefix, 'bin/images', 'splash_screen.gif')
        self.photo = Tkinter.PhotoImage (file = fname)        
        self._photo = Tkinter.Label (self._frame, bd=0, image=self.photo)
        self._photo.grid (row=0)

        self._frame.pack()
        return

        

    
        
        
