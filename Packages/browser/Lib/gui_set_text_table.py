#!/usr/bin/env python
#
# The PCMDI Data Browser Plot Secondary Text -  gui_set_text_table module
#
#################################################################################
#                                                                               #
# Module:       gui_set_text_table                                              #
#                                                                               #
# Copyright:    "See file Legal.htm for copyright information."                 #
#                                                                               #
# Authors:      PCMDI Software Team                      		        #
#               Lawrence Livermore National Laboratory:                         #
#               support@pcmdi.llnl.gov                                          #
#                                                                               #
# Description:  PCMDI Software System browser to set the seconday text          #
#               table.                                                          #
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
# VCS Text Table Dialog Popup
#---------------------------------------------------------------------------
#
class create:
   def __init__(self, dialog, parent, t_name):
        self.parent = parent
        self.dialog = Pmw.Dialog( parent,
            title = "Set %s's text properties (i.e., table)"%t_name,
            buttons = ('Re-Plot', 'Apply', 'Dismiss'),
            defaultbutton = 'Dismiss',
            command = gui_control.Command(self.execute, parent, t_name) )

        self.dialog.transient( dialog ) # draw widget on top of its parent

        text_t = parent.vcs[ parent.vcs_id ].gettexttable( t_name )
        
        framea = Tkinter.Frame( self.dialog.interior() )
	framea.pack( side='top', fill = 'both', expand=1 )

        self.font = Pmw.Counter( framea,
             labelpos = 'w',
             label_text = 'Font:',
             entry_background = 'white',
             entry_foreground = 'black',
             entry_width=5,
             entryfield_value = text_t.font,
             entryfield_validate={'validator':'integer','min':1, 'max':9, 'minstrict':1}
            )
        self.font.pack(side='left', fill='y', expand=1)
#        self.font.component('entry').unbind("<Return>")

        self.color = Pmw.Counter( framea,
             labelpos = 'w',
             label_text = 'Color:',
             entry_background = 'white',
             entry_foreground = 'black',
             entry_width=5,
             entryfield_value = text_t.color,
             entryfield_validate={'validator':'integer','min':0,'max':255,'minstrict':1}
             )
        self.color.pack(side='left', fill='y', expand=1)
#        self.color.component('entry').unbind("<Return>")

        frameb = Tkinter.Frame( self.dialog.interior() )
	frameb.pack( side='top', fill = 'both', expand=1 )

        self.spacing = Pmw.Counter( frameb,
             labelpos = 'w',
             label_text = 'Spacing:',
             entry_background = 'white',
             entry_foreground = 'black',
             entry_width=5,
             entryfield_value = text_t.spacing,
             entryfield_validate = {'validator':'integer','min':-50, 'max':50, 'minstrict':1}
            )
        self.spacing.pack(side='left', fill='y', expand=1)
#        self.spacing.component('entry').unbind("<Return>")

        self.expansion = Pmw.Counter( frameb,
             labelpos = 'w',
             label_text = 'Expansion:',
             entry_background = 'white',
             entry_foreground = 'black',
             entry_width=5,
             entryfield_value = text_t.expansion,
             #entryfield_modifiedcommand,
             entryfield_validate = {'validator':'integer','min':50, 'max':150, 'minstrict':1}
            )
        self.expansion.pack( side='left', fill='y', expand=1)
#        self.expansion.component('entry').unbind("<Return>")

        entries = ( self.font.component('label'), self.color.component('label'),
                    self.spacing.component('label'), self.expansion.component('label') )
        Pmw.alignlabels(entries)

        # Position dialog popup
        parent_geom = self.parent.geometry()
        geom = string.split(parent_geom, '+')
        d1 = string.atoi( geom[1] )
        d2 = string.atoi( geom[2] )
	self.dialog.geometry( "+%d+%d" % (d1, d2) )

   def get_text_table_settings( self, parent, t_name ):

      text_t = parent.vcs[ parent.vcs_id ].gettexttable( t_name )

      try:
         text_t.font = eval( self.font.get( ) )
      except:
         text_t.font = 1

      try:
         text_t.color =  eval( self.color.get( ) )
      except:
         text_t.color = 241

      try:
         text_t.spacing = eval( self.spacing.get( ) )
      except:
         text_t.spacing = 2

      try:
         text_t.expansion = eval( self.expansion.get( ) )
      except:
         text_t.expansion = 100

   def execute(self, parent, t_name, result):
      if result == 'Re-Plot':
         self.get_text_table_settings( parent, t_name )
         parent.panelGC.evt_plot( parent )
#         vcs_function.re_plot( parent, 0 )
      elif result == 'Apply':
         self.get_text_table_settings( parent, t_name )
      else:
         self.dialog.destroy()


#---------------------------------------------------------------------
#
# End text table popup dialog
#
#---------------------------------------------------------------------
#
#---------------------------------------------------------------------
# End of File
#---------------------------------------------------------------------
