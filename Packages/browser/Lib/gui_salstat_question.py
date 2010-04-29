#!/usr/bin/env python
#
# The PCMDI Data Browser Statistics Question to User -  gui_statistics_question module
#
###############################################################################
#                                                                             #
# Module:       gui_statistics_question                                       #
#                                                                             #
# Copyright:    "See file Legal.htm for copyright information."               #
#                                                                             #
# Authors:      PCMDI Software Team                      	              #
#               Lawrence Livermore National Laboratory:                       #
#               support@pcmdi.llnl.gov                                        #
#                                                                             #
# Description:  PCMDI Software System browser to ask the user which dimensions#
#               to do selected statistics function over.                      #
#                                                                             #
# Version:      4.0                                                           #
#                                                                             #
###############################################################################
#
#---------------------------------------------------------------------
# NOTE: need to use version of Python that imports Tkinter and Pmw
#---------------------------------------------------------------------
import Tkinter, Pmw
import string
import gui_menu
import gui_control
import gui_message
from gui_support import gui_color

#---------------------------------------------------------------------
#
# Start of Popup Dialog
#
#---------------------------------------------------------------------------
# VCS Canvas Geometry Dialog Popup
#---------------------------------------------------------------------------
#
class create:
   def __init__(self, parent, statistics_type):
        self.parent = parent
        lst = parent.panelDV.selected_list.keys()
        if len(lst)==0:
           return
        var1=parent.panelDV.lst1[ parent.panelDV.selected_list[ lst[0] ]]
        
        self.dialog = Pmw.Dialog( parent,
            title = "%s Question to User"%statistics_type,
            buttons = ('OK', 'Dismiss'),
            defaultbutton = 'OK',
            command = gui_control.Command(self.execute, parent, statistics_type, var1) )

        if parent.menu.popup_window_settings_flg == 1:
           self.dialog.transient( self.parent ) # Keep widget on top of its parent

        lbl=Tkinter.Label(self.dialog.interior(),
            text = "Operate the '%s' function on the following\ndimension(s):"%statistics_type,
            justify = 'left',
            anchor = 'w', 
            )
        lbl.pack( expand = 1, fill = 'both', padx=10, pady=5 )

        self.dimension_name = {}
        self.dimension_toggles1 ={}
        self.dimension_toggles2 ={}
        self.dimension_state ={}
        for text in var1.listdimnames():
           frame = Tkinter.Frame( self.dialog.interior() )
           frame.pack( side='top', expand = 1, fill = 'both', padx=10, pady=5 )
           self.dimension_name[text] = Tkinter.Label(frame,
            text = '%s: ' % text,
            justify = 'left',
            anchor = 'w',
            )
           self.dimension_name[text].pack( side='left', expand = 1, fill = 'both', padx=10, pady=5 )
	   # Create and pack a check buttons for the variable axis.
           self.dimension_toggles1[text] = Tkinter.Checkbutton( frame,
             selectcolor=gui_color.one,
             text = 'Yes                                            ',
             command = gui_control.Command(self.weighted_cb, text, 1)
           )
           self.dimension_toggles1[text].pack(side = 'left', fill = 'both', padx=5, pady=5)
           self.dimension_state[text] = 0
           if text == var1.listdimnames()[0] :
              self.dimension_toggles1[text].select()
              self.dimension_state[text] = 1


        # Position dialog popup
        parent_geom = self.parent.geometry()
        geom = string.split(parent_geom, '+')
        d1 = string.atoi( geom[1] )
        d2 = string.atoi( geom[2] )
	self.dialog.geometry( "+%d+%d" % (d1, d2) )

   def weighted_cb(self, text, tag):
        if (tag == 1) and (self.dimension_state[text] == 1):
           self.dimension_toggles1[text].deselect()
           self.dimension_state[text] = 0
        elif (tag == 1) and (self.dimension_state[text] == 2):
           self.dimension_toggles2[text].deselect()
           self.dimension_state[text] = 1
        elif (tag == 2) and (self.dimension_state[text] == 2):
           self.dimension_toggles2[text].deselect()
           self.dimension_state[text] = 0
        elif (tag == 2) and (self.dimension_state[text] == 1):
           self.dimension_toggles1[text].deselect()
           self.dimension_state[text] = 2
        else:
           self.dimension_state[text] = tag



   def execute(self, parent, statistics_type,var1, result):
      self.dialog.destroy()
      if result == 'OK':
         axis_t = ''
         weight_l = []
         for text in var1.listdimnames():
            if self.dimension_state[text] in (1,2):
               axis_t += '(%s)'%text
         parent.menu.pcmdi_tools_menu.evt_salstat(parent, statistics_type, axis=axis_t)

#---------------------------------------------------------------------
#
# End Salstats Question popup dialog
#
#---------------------------------------------------------------------
#
#---------------------------------------------------------------------
# End of File
#---------------------------------------------------------------------
