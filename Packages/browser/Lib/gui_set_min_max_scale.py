#!/usr/bin/env python
#
# The PCMDI Data Browser Set Min, Max, and Scale Popup -  gui_set_min_max_scale module
#
#################################################################################
#                                                                               #
# Module:       gui_set_min_max_scale module                                    #
#                                                                               #
# Copyright:    "See file Legal.htm for copyright information."                 #
#                                                                               #
# Authors:      PCMDI Software Team                                             #
#               Lawrence Livermore NationalLaboratory:                          #
#               support@pcmdi.llnl.gov                                          #
#                                                                               #
# Description:  PCMDI Software System browser set min, max, and scale popup.    #
#                                                                               #
# Version:      4.0                                                             #
#                                                                               #
#################################################################################
#
#---------------------------------------------------------------------
# NOTE: need to use version of Python that imports Tkinter and Pmw
#---------------------------------------------------------------------
import Tkinter, Pmw
import os, sys, string
import gui_menu
import gui_functions
from gui_support import gui_color
## import gui_control

#---------------------------------------------------------------------
#
# Start of Popup Dialogs
#
#---------------------------------------------------------------------------
# VCS Set Minimum and Maximum and Scale Values Popup
#---------------------------------------------------------------------------
#
class create:
   def __init__(self, parent):
        self.parent = parent
        self.preview_flg = 0
        self.dialog = Pmw.Dialog( parent,
            title = 'Set Minimum and Maximum and Scale Value',
            buttons = ('Preview', 'Unset', 'Apply', 'Cancel'),
            defaultbutton = 'Apply',
            command = self.execute )

        if parent.menu.popup_window_settings_flg == 1:
           self.dialog.transient( self.parent ) # Keep widget on top of its parent
        self.dialog.withdraw()		     # this is really slow

        lbl1=Tkinter.Label(self.dialog.interior(),
            text = "Set the plot [axes] min and max values:",
            justify = 'left',
            anchor = 'w',
            )
        lbl1.pack( expand = 1, fill = 'both', padx=10, pady=5 )

        self.eny1=Pmw.EntryField(self.dialog.interior(),
            labelpos = 'w',
            label_text = 'X Minimum:',
            entry_background = 'white',
            entry_foreground = 'black',
            entry_width =  41,
            )
        self.eny1.pack( expand = 1, fill = 'both', padx=20, pady=5 )
        self.parent.balloon.bind(self.eny1, "Enter the 'X Minimum' value.")
        if self.parent.app_x_min is not None:
           self.eny1.setentry( self.parent.app_x_min )

        self.eny2=Pmw.EntryField(self.dialog.interior(),
            labelpos = 'w',
            label_text = 'X Maximum:',
            entry_background = 'white',
            entry_foreground = 'black',
            entry_width =  41,
            )
        self.eny2.pack( expand = 1, fill = 'both', padx=20, pady=5 )
        self.parent.balloon.bind(self.eny2, "Enter the 'X Maximum' value.")
        if self.parent.app_x_max is not None:
           self.eny2.setentry( self.parent.app_x_max )

        self.eny3=Pmw.EntryField(self.dialog.interior(),
            labelpos = 'w',
            label_text = 'Y Minimum:',
            entry_background = 'white',
            entry_foreground = 'black',
            entry_width =  41,
            )
        self.eny3.pack( expand = 1, fill = 'both', padx=20, pady=5 )
        self.parent.balloon.bind(self.eny3, "Enter the 'Y Minimum' value.")
        if self.parent.app_y_min is not None:
           self.eny3.setentry( self.parent.app_y_min )

        self.eny4=Pmw.EntryField(self.dialog.interior(),
            labelpos = 'w',
            label_text = 'Y Maximum:',
            entry_background = 'white',
            entry_foreground = 'black',
            entry_width =  41,
            )
        self.eny4.pack( expand = 1, fill = 'both', padx=20, pady=5 )
        self.parent.balloon.bind(self.eny4, "Enter the 'Y Maximum' value.")
        if self.parent.app_y_max is not None:
           self.eny4.setentry( self.parent.app_y_max )

        lbl2=Tkinter.Label(self.dialog.interior(),
            text = "Set the [data] min and max values:",
            justify = 'left',
            anchor = 'w',
            )
        lbl2.pack( expand = 1, fill = 'both', padx=10, pady=5 )

        self.eny5=Pmw.EntryField(self.dialog.interior(),
            labelpos = 'w',
            label_text = 'Data Minimum:',
            entry_background = 'white',
            entry_foreground = 'black',
            entry_width =  41,
            )
        self.eny5.pack( expand = 1, fill = 'both', padx=20, pady=5 )
        self.parent.balloon.bind(self.eny5, "Enter the 'Data Minimum' value.")
        if self.parent.app_d_min is not None:
           self.eny5.setentry( self.parent.app_d_min )

        self.eny6=Pmw.EntryField(self.dialog.interior(),
            labelpos = 'w',
            label_text = 'Data Maximum:',
            entry_background = 'white',
            entry_foreground = 'black',
            entry_width =  41,
            )
        self.eny6.pack( expand = 1, fill = 'both', padx=20, pady=5 )
        self.parent.balloon.bind(self.eny6, "Enter the 'Data Maximum' value.")
        if self.parent.app_d_max is not None:
           self.eny6.setentry( self.parent.app_d_max )

        lbl3=Tkinter.Label(self.dialog.interior(),
            text = "Set the [data] scale value:",
            justify = 'left',
            anchor = 'w',
            )
        lbl3.pack( expand = 1, fill = 'both', padx=10, pady=5 )

        self.eny7=Pmw.EntryField(self.dialog.interior(),
            labelpos = 'w',
            label_text = 'Multiplier:',
            entry_background = 'white',
            entry_foreground = 'black',
            entry_width =  41,
            )
        self.eny7.pack( expand = 1, fill = 'both', padx=20, pady=5 )
        self.parent.balloon.bind(self.eny7, "Enter the 'Multiplier' value.")
        if self.parent.multiplier is not None:
           self.eny7.setentry( self.parent.multiplier )

#        btn1=Tkinter.Button(self.dialog.interior(),
#            text = 'Re-Plot',
#            background = gui_color.replot,
#            command = gui_control.Command(self.set_replot, parent)
#            )
#        btn1.pack( expand = 1, fill = 'both', padx=190, pady=5 )

#        btn2=Tkinter.Button(self.dialog.interior(),
#            text = 'Clear All',
#            background = gui_color.one,
#            command = self.set_clear,
#            )
#        btn2.pack( expand = 1, fill = 'both', padx=190, pady=5 )

        entries = (self.eny1, self.eny2, self.eny3, self.eny4, self.eny5, self.eny6, self.eny7)
        Pmw.alignlabels(entries)

        # This is needed to Reset and Cancel the GUI
        self.hold_min_max_scale_original_settings( )

        # Position dialog popup
        parent_geom = self.parent.geometry()
        geom = string.split(parent_geom, '+')
        d1 = string.atoi( geom[1] )
        d2 = string.atoi( geom[2] )
	self.dialog.geometry( "+%d+%d" % (d1, d2) )

        self.dialog.show()

   def get_settings( self ):
        if self.eny1.get( ) != '':
            self.parent.app_x_min = string.atof(self.eny1.get( ))
        else:
            self.parent.app_x_min = None
        if self.eny2.get( ) != '':
            self.parent.app_x_max = string.atof(self.eny2.get( ))
        else:
             self.parent.app_x_max = None
        if self.eny3.get( ) != '':
            self.parent.app_y_min = string.atof(self.eny3.get( ))
        else:
            self.parent.app_y_min = None
        if self.eny4.get( ) != '':
            self.parent.app_y_max = string.atof(self.eny4.get( ))
        else:
            self.parent.app_y_max = None
        if self.eny5.get( ) != '':
            self.parent.app_d_min = string.atof(self.eny5.get( ))
        else:
            self.parent.app_d_min = None
        if self.eny6.get( ) != '':
            self.parent.app_d_max = string.atof(self.eny6.get( ))
        else:
            self.parent.app_d_max = None
        if self.eny7.get( ) != '':
            self.parent.multiplier = string.atof(self.eny7.get( ))
        else:
            self.parent.multiplier = None

        # Redraw the plot
        self.parent.panelGC.evt_plot( self.parent )

   def reset_mms_to_original_settings( self ):
      self.parent.app_x_min = self.store_app_x_min
      self.parent.app_x_max = self.store_app_x_max
      self.parent.app_y_min = self.store_app_y_min
      self.parent.app_y_max = self.store_app_y_max
      self.parent.app_d_min = self.store_app_d_min
      self.parent.app_d_max = self.store_app_d_max
      self.parent.multiplier= self.store_multiplier

      # Redraw the plot
      self.parent.panelGC.evt_plot( self.parent )

   def unset( self ):
      self.eny1.clear()
      self.eny2.clear()
      self.eny3.clear()
      self.eny4.clear()
      self.eny5.clear()
      self.eny6.clear()
      self.eny7.clear()
      self.parent.app_x_min = None
      self.parent.app_x_max = None
      self.parent.app_y_min = None
      self.parent.app_y_max = None
      self.parent.app_d_min = None
      self.parent.app_d_max = None
      self.parent.multiplier= None

      # Redraw the plot
      self.parent.panelGC.evt_plot( self.parent )

   def execute(self, result):
        if result == 'Preview':
           self.preview_flg = 1
           self.get_settings()
        elif result == 'Unset':
           self.unset( )
           self.preview_flg = 1
        elif result == 'Apply':
           self.get_settings()
           self.dialog.destroy()
        elif result == 'Cancel' or result is None:
           if self.preview_flg: self.reset_mms_to_original_settings( )
           self.dialog.destroy()

   def hold_min_max_scale_original_settings( self ):
        self.store_app_x_min = self.parent.app_x_min
        self.store_app_x_max = self.parent.app_x_max
        self.store_app_y_min = self.parent.app_y_min
        self.store_app_y_max = self.parent.app_y_max
        self.store_app_d_min = self.parent.app_d_min
        self.store_app_d_max = self.parent.app_d_max
        self.store_multiplier= self.parent.multiplier
#
#---------------------------------------------------------------------
#
# End set min, max, and scale popup dialog
#
#---------------------------------------------------------------------
#
#---------------------------------------------------------------------
# End of File
#---------------------------------------------------------------------
