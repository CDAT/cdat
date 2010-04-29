#############################################################################
#############################################################################
#  File:    About.py                                                        #
#  Date:    26-Jul-2005                                                     #
#############################################################################
#############################################################################
 
from Tkinter import *
import Pmw
import geoparse
import gui_support

class About (Toplevel):

    def __init__ (self, parent, on_close):
        
        Toplevel.__init__ (self, parent)
        self.transient(parent)       
        self.title ('About CDAT Help')
        self.on_close = on_close

        # Position the dialog box relative to parent.
        xpos = int(geoparse.get_x (parent.geometry())) + 100
        ypos = int(geoparse.get_y (parent.geometry())) + 100
        self.geometry ('+' + str(xpos) + '+' + str(ypos))

        p,c = gui_support.buildDate()

        text  = """Climate Data Analysis Tools (CDAT) Help\n\n"""
        text += """Python built on: %s\n"""%p
        text += """CDAT   built on: %s\n\n"""%c
        text += """Copyright 2001-2008, Regents of the University of California
All rights reserved.\n\n
This release of CDAT Help incorporates modules from
other organizations besides LLNL some of which are distributed
under open source licenses of their choosing, and therefore may be subject
to different terms and conditions.  We ask that users respect these terms
and conditions as well as any copyright notices requirements.\n
"""
        text += """Go to http://cdat.sourceforge.net for documentation, support, 
bug reporting, and releases.

Program for Climate Model Diagnosis and Intercomparison
Lawrence Livermore National Laboratory
Livermore, CA 94550
"""
        label = Label(self,text=text).grid(row=2)
        
        # Create close button.
        button = Button (self, text='Close', command=self.close).grid(row=10)

        # Override DELETE_WINDOW handler to a local callback.
        self.protocol ('WM_DELETE_WINDOW', self.close)

    # Event handler when user closes the window.
    def close (self):
        self.on_close()
        self.destroy()

