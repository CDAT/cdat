#!/usr/bin/env python
#
# The PCMDI Data Browser Plot Secondary Text -  gui_set_text_orientation module
#
#################################################################################
#                                                                               #
# Module:       gui_set_text_orientation                                        #
#                                                                               #
# Copyright:    "See file Legal.htm for copyright information."                 #
#                                                                               #
# Authors:      PCMDI Software Team                      		        #
#               Lawrence Livermore National Laboratory:                         #
#               support@pcmdi.llnl.gov                                          #
#                                                                               #
# Description:  PCMDI Software System browser to set the seconday text          #
#               orientation                                                     #
#                                                                               #
# Version:      3.0                                                             #
#                                                                               #
#################################################################################
#
#---------------------------------------------------------------------
# NOTE: need to use version of Python that imports Tkinter and Pmw
#---------------------------------------------------------------------
import Tkinter, Pmw
import string
import gui_control
import vcs_function

#---------------------------------------------------------------------
#
# Start of Popup Dialog
#
#---------------------------------------------------------------------------
# VCS Text Orientation Dialog Popup
#---------------------------------------------------------------------------
#
class create:
   def __init__(self, dialog, parent, o_name):
        self.parent = parent
        self.dialog = Pmw.Dialog( parent,
            title = "Set %s's text orientation"%o_name,
            buttons = ('Re-Plot', 'Apply', 'Dismiss'),
            defaultbutton = 'Dismiss',
            command = gui_control.Command(self.execute, parent, o_name) )

        self.dialog.transient( dialog ) # draw widget on top of its parent

        text_o = parent.vcs[ parent.vcs_id ].gettextorientation( o_name )
        
        frameb = Tkinter.Frame( self.dialog.interior() )
	frameb.pack( side='top', fill = 'both', expand=1 )

        path_lst = ["right","left","up","down"]
        self.text_path = Tkinter.StringVar()
        self.text_path.set(text_o.path)
        self.path = Pmw.OptionMenu( frameb,
                 labelpos = 'w',
		 label_text = 'Path:',
                 menubutton_width=10,
                 menubutton_textvariable = self.text_path,
                 items=path_lst
                 )
        self.path.pack( side='left', fill='both', expand=1 )

        halign_lst = ["left","center","right"]
        self.text_halign = Tkinter.StringVar()
        self.text_halign.set(text_o.halign)
        self.halign = Pmw.OptionMenu( frameb,
                 labelpos = 'w',
                 label_text = 'HAlign:',
                 menubutton_width=10,
                 menubutton_textvariable = self.text_halign,
                 items=halign_lst
                 )
        self.halign.pack( side='left', fill='both', expand=1 )

        valign_lst = ["top","cap","half","base","bottom"]
        self.text_valign = Tkinter.StringVar()
        self.text_valign.set(text_o.valign)
        self.valign = Pmw.OptionMenu( frameb,
                 labelpos = 'w',
                 label_text = 'Valign:',
                 menubutton_width=10,
                 menubutton_textvariable = self.text_valign,
                 items=valign_lst
                 )
        self.valign.pack( side='left', fill='both', expand=1 )

        framea = Tkinter.Frame( self.dialog.interior() )
	framea.pack( side='top', fill = 'both', expand=1 )

        self.height = Pmw.Counter( framea,
             labelpos = 'w',
             label_text = 'Height:',
             entry_background = 'white',
             entry_foreground = 'black',
             entry_width=5,
             entryfield_value = text_o.height,
             entryfield_validate = {'validator':'integer','minstrict':1}
            )
        self.height.pack(side='left', fill='y', expand=1)
        self.height.component('entry').unbind("<Return>")

        self.angle = Pmw.Counter( framea,
             labelpos = 'w',
             label_text = 'Angle:',
             entry_background = 'white',
             entry_foreground = 'black',
             entry_width=5,
             entryfield_value = text_o.angle,
             #entryfield_modifiedcommand,
             entryfield_validate = {'validator':'integer','min':0, 'max':360, 'minstrict':1}
            )
        self.angle.pack( side='left', fill='y', expand=1)
        self.angle.component('entry').unbind("<Return>")

        entries = ( self.height.component('label'), self.angle.component('label'),
                    self.path.component('label'), self.halign.component('label'),
                    self.halign.component('label') )
        Pmw.alignlabels(entries)

        # Position dialog popup
        parent_geom = self.parent.geometry()
        geom = string.split(parent_geom, '+')
        d1 = string.atoi( geom[1] )
        d2 = string.atoi( geom[2] )
	self.dialog.geometry( "+%d+%d" % (d1, d2) )

   def get_text_orientation_settings( self, parent, o_name ):

      text_o = parent.vcs[ parent.vcs_id ].gettextorientation( o_name )

      try:
         text_o.path = self.path.getcurselection( )
      except:
         text_o.path = 'right'

      try:
         text_o.halign =  self.halign.getcurselection( )
      except:
         text_o.halign = 'left'

      try:
         text_o.valign = self.valign.getcurselection( )
      except:
         text_o.valign = 'half'

      try:
         text_o.height = eval( self.height.get() )
      except:
         text_o.height = 12

      try:
         text_o.angle = eval( self.angle.get() )
      except:
         text_o.angle = 0

   def execute(self, parent, o_name, result):
      if result == 'Re-Plot':
         self.get_text_orientation_settings( parent, o_name )
         parent.panelGC.evt_plot( parent )
#         vcs_function.re_plot( parent, 0 )
      elif result == 'Apply':
         self.get_text_orientation_settings( parent, o_name )
      else:
         self.dialog.destroy()


#---------------------------------------------------------------------
#
# End text orientation popup dialog
#
#---------------------------------------------------------------------
#
#---------------------------------------------------------------------
# End of File
#---------------------------------------------------------------------
