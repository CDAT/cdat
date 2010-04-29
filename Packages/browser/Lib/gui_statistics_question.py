#!/usr/bin/env python
#
# The PCMDI Data Browser Statistics Question to User -  gui_statistics_question module
#
###################################################################################
#                                                                                 #
# Module:       gui_statistics_question                                           #
#                                                                                 #
# Copyright:    "See file Legal.htm for copyright information."                   #
#                                                                                 #
# Authors:      PCMDI Software Team                      		          #
#               Lawrence Livermore National Laboratory:                           #
#               support@pcmdi.llnl.gov                                            #
#                                                                                 #
# Description:  PCMDI Software System browser to ask the user which dimension(s)  #
#               to do selected statistics function over.                          #
#                                                                                 #
# Version:      4.0                                                               #
#                                                                                 #
###################################################################################
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
import gui_set_graphics_methods

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
        var1, var2 = parent.menu.pcmdi_tools_menu.get_var( parent )
        if var1 is None: return

        self.parent = parent
        self.dialog = Pmw.Dialog( parent,
            title = "%s Question to User"%statistics_type,
            buttons = ('OK', 'Dismiss'),
            defaultbutton = 'OK',
            command = gui_control.Command(self.execute, parent, statistics_type, var1, var2) )

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
           if string.lower(statistics_type) not in ['laggedcovariance', 'laggedcorrelation', 'autocovariance', 'autocorrelation', 'linearregression', 'geometricmean','rank','median']:
	      self.dimension_toggles1[text] = Tkinter.Checkbutton( frame,
                selectcolor=gui_color.one,
                text = 'Weighted',
		command = gui_control.Command(self.weighted_cb, text, 1)
	      )
	      self.dimension_toggles1[text].pack(side = 'left', fill = 'both', padx=5, pady=5)
              self.dimension_toggles2[text] = Tkinter.Checkbutton( frame,
                selectcolor=gui_color.one,
                text = 'Unweighted',
               command = gui_control.Command(self.weighted_cb, text, 2)
              )
	      self.dimension_toggles2[text].pack(side = 'left', fill = 'both', padx=5, pady=5)
              self.dimension_state[text] = 0
              if string.lower(text) in gui_control.latitude_alias:
                 if text == var1.listdimnames()[0] :
                    self.dimension_toggles1[text].select()
                    self.dimension_state[text] = 1
              else:
                 if text == var1.listdimnames()[0] :
                    self.dimension_toggles2[text].select()
                    self.dimension_state[text] = 2
           else:
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

        if string.lower(statistics_type) in ['laggedcovariance', 'laggedcorrelation', 'autocovariance', 'autocorrelation']:
           frame = Tkinter.Frame( self.dialog.interior() )
           frame.pack( side='top', expand = 1, fill = 'both', padx=10, pady=5 )
           self.lags=Pmw.EntryField(frame,
                                    labelpos = 'w',
                                    label_text = 'Lags:',
                                    entry_width=8,
                                    value='',
                                    entry_state='normal',
                                    label_state='normal',
                                    )
           self.lags.pack(side = 'left', fill = 'both', padx=5, pady=5)
           frame = Tkinter.Frame( self.dialog.interior() )
           frame.pack( side='top', expand = 1, fill = 'both', padx=10, pady=5 )
           self.loopvar=Tkinter.Variable()
           loop=Tkinter.Checkbutton( frame,
                                          selectcolor=gui_color.one,
                                          text = 'Loop ?',
                                          variable=self.loopvar,
                                          )
           loop.pack(side = 'left', fill = 'both', padx=5, pady=5)
           loop.select()
           
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

   def execute(self, parent, statistics_type, var1, var2, result):
      if string.lower(statistics_type) in ['laggedcovariance', 'laggedcorrelation', 'autocovariance', 'autocorrelation']:               
         lags=gui_set_graphics_methods._get_list(self.lags)
         loopvar=self.loopvar.get()
      self.dialog.destroy()
      if result == 'OK':
         axis_t = ''
         weight_l = []
         for text in var1.listdimnames():
            if self.dimension_state[text] in (1,2):
               axis_t += '(%s)'%text
            if self.dimension_state[text] == 1:
               weight_l.append('generate')
            elif self.dimension_state[text] == 2:
               weight_l.append('equal')
         noloop = None
         if string.lower(statistics_type) in ['laggedcovariance', 'laggedcorrelation', 'autocovariance', 'autocorrelation']:               
            if lags==[]:
               if loopvar=='0':
                  noloop=[None,1]
               else:
                  noloop=[None,0]
            else:
               if loopvar=='0':
                  noloop=[lags,1]
               else:
                  if len(lags)==1:
                     noloop=[lags[0],0]
                  else:
                     noloop=[lags,1]
                     
         parent.menu.pcmdi_tools_menu.evt_statistics(parent, statistics_type, axis=axis_t, weights=weight_l, noloop = noloop)

#---------------------------------------------------------------------
#
# End Statistics Question popup dialog
#
#---------------------------------------------------------------------
#
#---------------------------------------------------------------------
# End of File
#---------------------------------------------------------------------
