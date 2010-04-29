#!/usr/bin/env python
#
# The PCMDI Data Browser Messages -  gui_message module
#
#################################################################################
#                                                                               #
# Module:       gui_message module                                              #
#                                                                               #
# Copyright:    "See file Legal.htm for copyright information."                 #
#                                                                               #
# Authors:      PCMDI Software Team                                             #
#               Lawrence Livermore NationalLaboratory:                          #
#               support@pcmdi.llnl.gov                                          #
#                                                                               #
# Description:  PCMDI Software System browser message popup.                    #
#                                                                               #
# Version:      4.0                                                             #
#                                                                               #
#################################################################################
#
#---------------------------------------------------------------------
# NOTE: need to use version of Python that imports Tkinter and Pmw
#---------------------------------------------------------------------
import Tkinter, Pmw
from tkMessageBox import showerror, showinfo, askquestion

def error( str = None ):
   showerror( "Error Message to User", str )

def info( str = None ):
   showinfo( "Information Message to User", str, default='' )

def ask( str = None ):
   return askquestion("Are you sure you want to do this?", str, default='yes') 
   return ask_val

class wait:
   def __init__(self, parent, wait_string):
        self.parent = parent
        self.dialog = Pmw.Dialog( parent,
            title = 'Information Message to User',
            buttons = (),
            )

        lbl1=Tkinter.Label(self.dialog.interior(),
            text = wait_string,
            justify = 'left',
            anchor = 'w',
            )
        lbl1.pack( expand = 1, fill = 'both', padx=10, pady=5 )

        self.stop = 1
        self.textv_lbl2 = Tkinter.StringVar()
        self.textv_lbl2.set('')
        self.lbl2=Tkinter.Label(self.dialog.interior(),
            textvariable = self.textv_lbl2,
            justify = 'left',
            anchor = 'w',
            )
        self.lbl2.pack( expand = 1, fill = 'both', padx=10, pady=5 )

        self.dialog.geometry( "+%d+%d" % (parent.dxwin, parent.dywin) )

   def progress(self):
        new_lbl2 =  self.textv_lbl2.get()
        new_lbl2 = new_lbl2 + '.'
        self.textv_lbl2.set(new_lbl2)
        self.parent.update()

   def execute(self):
        self.stop = 0
        self.dialog.destroy()

