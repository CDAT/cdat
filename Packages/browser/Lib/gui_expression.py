#!/usr/bin/env python
# Adapted for numpy/ma/cdms2 by convertcdms.py
#
# The PCMDI Data Browser Plot Secondary Text -  gui_expression module
#
#################################################################################
#                                                                               #
# Module:       gui_expression                                                  #
#                                                                               #
# Copyright:    "See file Legal.htm for copyright information."                 #
#                                                                               #
# Authors:      PCMDI Software Team                      		        #
#               Lawrence Livermore National Laboratory:                         #
#               support@pcmdi.llnl.gov                                          #
#                                                                               #
# Description:  PCMDI Software System browser to set the function expression    #
#               and name.                                                       #
#                                                                               #
# Version:      3.0                                                             #
#                                                                               #
#################################################################################
#
#---------------------------------------------------------------------
# NOTE: need to use version of Python that imports Tkinter and Pmw
#---------------------------------------------------------------------
import Tkinter, Pmw
import __main__, cdms2 as cdms
import string, types
import gui_control
import gui_message
from gui_support import gui_color
import gui_defined_variables

#---------------------------------------------------------------------
#
# Start of Popup Dialog
#
#---------------------------------------------------------------------------
# VCS Variable name and expression Dialog Popup
#---------------------------------------------------------------------------
#
class create:
   def __init__(self, parent, fself, fn_name, expression):
        self.parent = parent
        self.fself = fself
        self.dialog = Pmw.Dialog( parent,
            title = "Enter Variable Name and Expression",
            buttons = ('Apply', 'Clear', 'Dismiss'),
            command = gui_control.Command(self.execute, parent) )

        self.dialog.transient( parent ) # draw widget on top of its parent

        framea = Tkinter.Frame( self.dialog.interior() )
	framea.pack( side='top', fill = 'both', expand=1 )

       	self.name = Pmw.EntryField( self.dialog.interior(),
	            labelpos = 'n',
	            label_text = 'Name',
                    value = fn_name,
                    entry_background = 'white',
                    entry_foreground = 'black',
                    entry_width =  15
	          )
        self.name.pack( side='left', expand = 1, fill = 'both', padx=3, pady=10 )
        self.parent.balloon.bind( self.name, "Enter variable name." )

        equal_sign = Tkinter.Label( self.dialog.interior(),
                    text = " = ",
                    justify = 'left',
                    anchor = 'w',
                  )
        equal_sign.pack( side='left', expand = 1, fill = 'both', padx=3, pady=10 )

        self.expression = Pmw.EntryField( self.dialog.interior(),
                    labelpos = 'n',
                    label_text = 'Expression',
                    value = expression,
                    entry_background = 'white',
                    entry_foreground = 'black',
                    entry_width =  45
                  )
        self.expression.pack( side='left', expand = 1, fill = 'both', padx=3, pady=10 )
        self.expression.component('entry').bind( "<Key>", gui_control.Command(self.evt_change_color, parent) )
        self.expression.component('entry').bind( "<Return>", gui_control.Command(self.evt_get_entry, parent) )
        self.parent.balloon.bind( self.expression, "Enter or modify expression, followed by the <Return> key." )

        # Position dialog popup
        parent_geom = self.parent.geometry()
        geom = string.split(parent_geom, '+')
        d1 = string.atoi( geom[1] )
        d2 = string.atoi( geom[2] )
	self.dialog.geometry( "+%d+%d" % (d1, d2) )

   def evt_change_color( self, parent, event ):
      keycolor = Pmw.Color.changebrightness(parent, 'red', 0.85)
      self.expression.configure( entry_background = keycolor )

   def evt_get_entry( self, parent, event ):
      # change backgound color to white
      self.expression.configure( entry_background = 'white' )
      self.fself.saved_expression = self.expression.get()

   def execute(self, parent, result):
      if result == 'Apply':
         try:
            v_name = self.name.get()
            expression = self.expression.get()
            if not __main__.__dict__.has_key('MV'):
               exec "from cdms2 import MV" in __main__.__dict__
            __main__.__dict__[v_name] = eval( expression, __main__.__dict__ )
            if type(__main__.__dict__[v_name]) in [types.IntType,types.FloatType]:
               __main__.__dict__[v_name] = cdms.asVariable(__main__.__dict__[v_name])
            __main__.__dict__[v_name].id = __main__.__dict__[v_name].name = v_name
            gui_defined_variables.update_defined( )
            gui_control.record_command(parent, "\n# Record the expression", 1 )
            gui_control.record_command(parent, "%s = %s" % (v_name, expression), 1 )
         except:
            gui_message.error("Bad expression! Check expression and try again.")
      elif result == 'Clear':
         self.expression.clear()
         self.fself.doing_calculation_type = ''
         self.fself.saved_expression = ''
         self.fself.build_expression = 'no'
      else:
         self.dialog.destroy()
         self.fself.gui_expression = None


#---------------------------------------------------------------------
#
# End Expression Popup Dialog
#
#---------------------------------------------------------------------
#
#---------------------------------------------------------------------
# End of File
#---------------------------------------------------------------------
