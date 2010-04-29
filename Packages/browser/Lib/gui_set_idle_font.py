#!/usr/bin/env python
#
# The PCMDI Data Browser Set IDLE's Font -  gui_set_idle_font module
#
#################################################################################
#                                                                               #
# Module:       gui_set_idle_font module                                        #
#                                                                               #
# Copyright:    "See file Legal.htm for copyright information."                 #
#                                                                               #
# Authors:      PCMDI Software Team                      		        #
#               Lawrence Livermore National Laboratory:                         #
#               support@pcmdi.llnl.gov                                          #
#                                                                               #
# Description:  PCMDI Software System browser IDLE font control.                #
#                                                                               #
# Version:      4.0                                                             #
#                                                                               #
#################################################################################
#
#---------------------------------------------------------------------
# NOTE: need to use version of Python that imports Tkinter and Pmw
#---------------------------------------------------------------------
import Tkinter, Pmw
#from idlelib import IdleConf
import os, sys, string
from gui_support import gui_color
import gui_control

#---------------------------------------------------------------------
#
# Start of Popup Dialog
#
#---------------------------------------------------------------------------
# Idle Font Specification            
#---------------------------------------------------------------------------
#
class create:
   def __init__(self, parent):
        self.parent = parent
        self.dialog = Pmw.Dialog( parent,
            title = "Set the Edit and Command Line Font",
            buttons = ('OK', 'Dismiss'),
            defaultbutton = 'OK',
            command = gui_control.Command(self.execute, parent) )

        if parent.menu.popup_window_settings_flg == 1:
           self.dialog.transient( self.parent ) # Keep widget on top of its parent

        lbl=Tkinter.Label(self.dialog.interior(),
            text = "Set Font Specifications:",
            justify = 'left',
            anchor = 'w',
            )
        lbl.pack( expand = 1, fill = 'both', padx=10, pady=5 )

        self.eny1=Pmw.EntryField(self.dialog.interior(),
            labelpos = 'w',
            label_text = "Font-Name: ",
            entry_background = 'white',
            entry_foreground = 'black',
            entry_width =  47,
            )
        self.eny1.pack( expand = 1, fill = 'both', padx=10, pady=5 )
        if gui_control.idle_font_name is not None: self.eny1.setentry( gui_control.idle_font_name )

        self.eny2=Pmw.EntryField(self.dialog.interior(),
            labelpos = 'w',
            label_text = "Font-Size:",
            entry_background = 'white',
            entry_foreground = 'black',
            entry_width =  47,
            )
        self.eny2.pack( expand = 1, fill = 'both', padx=10, pady=5 )
        if gui_control.idle_font_size is not None: self.eny2.setentry( gui_control.idle_font_size )

        self.eny3=Pmw.EntryField(self.dialog.interior(),
            labelpos = 'w',
            label_text = "Font-Width:",
            entry_background = 'white',
            entry_foreground = 'black',
            entry_width =  47,
            )
        self.eny3.pack( expand = 1, fill = 'both', padx=10, pady=5 )
        if gui_control.idle_font_width is not None: self.eny3.setentry( gui_control.idle_font_width )

        self.eny4=Pmw.EntryField(self.dialog.interior(),
            labelpos = 'w',
            label_text = "Font-Height:",
            entry_background = 'white',
            entry_foreground = 'black',
            entry_width =  47,
            )
        self.eny4.pack( expand = 1, fill = 'both', padx=10, pady=5 )
        if gui_control.idle_font_height is not None: self.eny4.setentry( gui_control.idle_font_height )

        entries = ( self.eny1, self.eny2, self.eny3, self.eny4 )
        Pmw.alignlabels(entries)

        # Position dialog popup
        parent_geom = self.parent.geometry()
        geom = string.split(parent_geom, '+')
        d1 = string.atoi( geom[1] )
        d2 = string.atoi( geom[2] )
	self.dialog.geometry( "+%d+%d" % (d1, d2) )

   def execute(self, parent, result):
##       if result == 'OK':
##          IdleConf.load( os.path.join(sys.exec_prefix, 'bin', 'idle') )
##          a=IdleConf.idleconf._ConfigParser__sections[ 'EditorWindow' ]
##          if self.eny1.get( ) == '':
##             a[ 'font-type' ] = 'courier'
##          else:
##             a[ 'font-type' ] =  self.eny1.get( )
##          gui_control.idle_font_name = a[ 'font-type' ]
##          if self.eny2.get( ) == '':
##             a[ 'font-size' ] = '10'
##          else:
##             a[ 'font-size' ] = self.eny2.get( )
##          gui_control.idle_font_size = a[ 'font-size' ]
##          if self.eny3.get( ) == '':
##             a[ 'width' ] = '80'
##          else:
##             a[ 'width' ] = self.eny3.get( )
##          gui_control.idle_font_width = a[ 'width' ]
##          if self.eny4.get( ) == '':
##             a[ 'height' ] = '10'
##          else:
##             a[ 'height' ] = self.eny4.get( )
##          gui_control.idle_font_height = a[ 'height' ]
      self.dialog.destroy()

#---------------------------------------------------------------------
#
# End Idle Font Specification
#
#---------------------------------------------------------------------
#
#---------------------------------------------------------------------
# End of File
#---------------------------------------------------------------------
