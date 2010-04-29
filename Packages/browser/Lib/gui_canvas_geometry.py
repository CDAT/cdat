#!/usr/bin/env python
#
# The PCMDI Data Browser Plot Annotation -  gui_canvas_geometry module
#
#################################################################################
#                                                                               #
# Module:       gui_canvas_geometry module                                      #
#                                                                               #
# Copyright:    "See file Legal.htm for copyright information."                 #
#                                                                               #
# Authors:      PCMDI Software Team                      		        #
#               Lawrence Livermore National Laboratory:                         #
#               support@pcmdi.llnl.gov                                          #
#                                                                               #
# Description:  PCMDI Software System browser to set VCS Canvas geometry.       #
#                                                                               #
# Version:      4.0                                                             #
#                                                                               #
#################################################################################
#
#---------------------------------------------------------------------
# NOTE: need to use version of Python that imports Tkinter and Pmw
#---------------------------------------------------------------------
import Tkinter, Pmw
import sys, string
import gui_menu
import gui_functions
from gui_support import gui_color
import gui_control
import vcs_function

#---------------------------------------------------------------------
#
# Start of Popup Dialog
#
#---------------------------------------------------------------------------
# VCS Canvas Geometry Dialog Popup
#---------------------------------------------------------------------------
#
class create:
   def __init__(self, parent):
        self.parent = parent
        self.preview_flg = 0
        self.apply_flg = 1
        self.dialog = Pmw.Dialog( parent,
            title = "Set VCS Canvas %i Geometry" % (parent.vcs_id+1),
            buttons = ('Preview', 'Unset', 'Apply', 'Cancel'),
            defaultbutton = 'Apply',
            command = gui_control.Command(self.execute, parent) )

        if parent.menu.popup_window_settings_flg == 1:
           self.dialog.transient( self.parent ) # Keep widget on top of its parent
        self.dialog.withdraw()		     # this is really slow

        lbl=Tkinter.Label(self.dialog.interior(),
            text = ("Set Geometry for VCS Canvas %i:" % (parent.vcs_id+1)),
            justify = 'left',
            anchor = 'w',
            )
        lbl.pack( expand = 1, fill = 'both', padx=10, pady=5 )

        try:
           w_attr= parent.vcs[ parent.vcs_id ].canvasinfo( )
           width  = w_attr['width']
           height = w_attr['height']
        except:
           width = None
           height = None

        self.eny1=Pmw.EntryField(self.dialog.interior(),
            labelpos = 'w',
            label_text = 'Width:',
            entry_background = 'white',
            entry_foreground = 'black',
            entry_width =  48,
            )
        if width is not None: self.eny1.setentry( "%d" % width )
        self.eny1.pack( expand = 1, fill = 'both', padx=10, pady=5 )
        self.parent.balloon.bind(self.eny1, "Enter the 'Width' for VCS Canvas %i."%(parent.vcs_id+1))
        if self.parent.vcg[ parent.vcs_id ][0] is not None:
           self.eny1.setentry( self.parent.vcg[ parent.vcs_id ][0] )
        self.eny1.component('entry').bind( "<Key>", gui_control.Command(self.evt_set_apply, parent, 1) )

        self.eny2=Pmw.EntryField(self.dialog.interior(),
            labelpos = 'w',
            label_text = 'Height:',
            entry_background = 'white',
            entry_foreground = 'black',
            entry_width =  48,
            )
        if height is not None: self.eny2.setentry( "%d" % height )
        self.eny2.pack( expand = 1, fill = 'both', padx=10, pady=5 )
        self.parent.balloon.bind(self.eny2, "Enter the 'Height' for VCS Canvas %i."%(parent.vcs_id+1))
        if self.parent.vcg[ parent.vcs_id ][1] is not None:
           self.eny2.setentry( self.parent.vcg[ parent.vcs_id ][1] )
        self.eny2.component('entry').bind( "<Key>", gui_control.Command(self.evt_set_apply, parent, 2) )

        self.eny3=Pmw.EntryField(self.dialog.interior(),
            labelpos = 'w',
            label_text = 'X-Position:',
            entry_background = 'white',
            entry_foreground = 'black',
            entry_width =  48,
            )
        self.eny3.pack( expand = 1, fill = 'both', padx=10, pady=5 )
        self.parent.balloon.bind(self.eny3, "Enter the 'X-Position' for VCS Canvas %i.\n\nNote, setting only the X-Position has no effect on the VCS Canvas\nwithout a change to the width or height."%(parent.vcs_id+1))
        if self.parent.vcg[ parent.vcs_id ][2] is not None:
           self.eny3.setentry( self.parent.vcg[ parent.vcs_id ][2] )
        self.eny3.component('entry').bind( "<Key>", gui_control.Command(self.evt_set_apply, parent, 3) )

        self.eny4=Pmw.EntryField(self.dialog.interior(),
            labelpos = 'w',
            label_text = 'Y-Position: ',
            entry_background = 'white',
            entry_foreground = 'black',
            entry_width =  48,
            )
        self.eny4.pack( expand = 1, fill = 'both', padx=10, pady=5 )
        self.parent.balloon.bind(self.eny4, "Enter the 'Y-Position' for VCS Canvas %i.\n\nNote, setting only the Y-Position has no effect on the VCS Canvas\nwithout a change to the width or height."%(parent.vcs_id+1))
        if self.parent.vcg[ parent.vcs_id ][3] is not None:
           self.eny4.setentry( self.parent.vcg[ parent.vcs_id ][3] )
        self.eny4.component('entry').bind( "<Key>", gui_control.Command(self.evt_set_apply, parent, 4) )

#        btn1=Tkinter.Button(self.dialog.interior(),
#            text = 'Re-Plot',
#            background = gui_color.replot,
#            command = gui_control.Command(self.geometry_replot, parent)
#            )
#        btn1.pack( expand = 1, fill = 'both', padx=190, pady=5 )

#        btn2=Tkinter.Button(self.dialog.interior(),
#            text = 'Clear All',
#            background = gui_color.one,
#            command = self.geometry_clear,
#            )
#        btn2.pack( expand = 1, fill = 'both', padx=190, pady=5 )

        entries = ( self.eny1, self.eny2, self.eny3, self.eny4 )
        Pmw.alignlabels(entries)

        # This is needed to Reset and Cancel the GUI
        self.hold_geom_original_settings( )

        # Position dialog popup
        parent_geom = self.parent.geometry()
        geom = string.split(parent_geom, '+')
        d1 = string.atoi( geom[1] )
        d2 = string.atoi( geom[2] )
	self.dialog.geometry( "+%d+%d" % (d1, d2) )

        self.dialog.show()

   def geometry_replot( self, parent ):
      geom_0 = parent.vcg[ parent.vcs_id ][0]
      geom_1 = parent.vcg[ parent.vcs_id ][1]
      geom_2 = parent.vcg[ parent.vcs_id ][2]
      geom_3 = parent.vcg[ parent.vcs_id ][3]
      if geom_0 is None: geom_0 = 756
      if geom_1 is None: geom_1 = 567
      if geom_2 is None: geom_2 = 0
      if geom_3 is None: geom_3 = 0
      try: parent.vcs[ parent.vcs_id ].geometry( geom_0, geom_1, geom_2, geom_3 )
      except:
           parent.vcs[ parent.vcs_id ].open( )
           parent.vcs[ parent.vcs_id ].geometry( geom_0, geom_1, geom_2, geom_3 )

     # vcs_function.re_plot( parent, 0 )
      parent.vcs[ parent.vcs_id ].updateorientation()
      parent.panelGC.evt_plot( parent )

   def evt_set_apply( self, parent, num, event ):
      if num == 1:
         if string.strip( self.eny1.get( ) ) != '': self.apply_flg = 1
      elif num == 2:
         if string.strip( self.eny2.get( ) ) != '': self.apply_flg = 1
      elif num == 3:
         if string.strip( self.eny3.get( ) ) != '': self.apply_flg = 1
      elif num == 4:
         if string.strip( self.eny4.get( ) ) != '': self.apply_flg = 1


   def show_canvas_geometry( self, parent ):
      w_attr= parent.vcs[ parent.vcs_id ].canvasinfo( )
      width  = w_attr['width']
      height = w_attr['height']
      if width is not None: self.eny1.setentry( "%d" % width )
      if height is not None: self.eny2.setentry( "%d" % height )


   def get_geometry_settings( self, parent ):
      try:
         parent.vcg[ parent.vcs_id ][0] = int( self.eny1.get( ) )
      except:
         parent.vcg[ parent.vcs_id ][0] = 756
      try:
         parent.vcg[ parent.vcs_id ][1] = int( self.eny2.get( ) )
      except:
         parent.vcg[ parent.vcs_id ][1] = 567
      try:
         parent.vcg[ parent.vcs_id ][2] = int( self.eny3.get( ) )
      except:
         parent.vcg[ parent.vcs_id ][2] = 0
      try:
         parent.vcg[ parent.vcs_id ][3] = int( self.eny4.get( ) )
      except:
         parent.vcg[ parent.vcs_id ][3] = 0

      # Redraw the plot
      self.geometry_replot( parent )

   def geometry_unset( self, parent ):
      self.eny1.clear()
      self.eny2.clear()
#      self.eny3.clear()
#      self.eny4.clear()
      parent.vcg[ parent.vcs_id ][0] = None
      parent.vcg[ parent.vcs_id ][1] = None
      parent.vcg[ parent.vcs_id ][2] = None
      parent.vcg[ parent.vcs_id ][3] = None

      # Redraw the plot
      #self.geometry_replot( parent )

   def reset_geom_to_original_settings( self, parent ):
      geom_0 = parent.vcg[ parent.vcs_id ][0] = self.geom_0
      geom_1 = parent.vcg[ parent.vcs_id ][1] = self.geom_1
      geom_2 = parent.vcg[ parent.vcs_id ][2] = self.geom_2
      geom_3 = parent.vcg[ parent.vcs_id ][3] = self.geom_3

      # Redraw the plot
      self.geometry_replot( parent )
     
   def execute(self, parent, result):
      if result == 'Preview':
         self.preview_flg = 1
         self.apply_flg = 0
         self.show_canvas_geometry( parent )
         self.get_geometry_settings( parent )
      if result == 'Unset':
         self.preview_flg = 1
         self.geometry_unset( parent )
      #   self.show_canvas_geometry( parent )
      #   self.dialog.destroy()
      if result == 'Apply':
         if self.apply_flg == 1:
            self.get_geometry_settings( parent )
         self.dialog.destroy()
      if result == 'Cancel' or result is None:
         if self.preview_flg: self.reset_geom_to_original_settings( parent )
         self.dialog.destroy()

   def hold_geom_original_settings( self ):
      self.geom_0 = self.parent.vcg[ self.parent.vcs_id ][0]
      self.geom_1 = self.parent.vcg[ self.parent.vcs_id ][1]
      self.geom_2 = self.parent.vcg[ self.parent.vcs_id ][2]
      self.geom_3 = self.parent.vcg[ self.parent.vcs_id ][3]


#---------------------------------------------------------------------
#
# End VCS Canvas Geometry popup dialog
#
#---------------------------------------------------------------------
#
#---------------------------------------------------------------------
# End of File
#---------------------------------------------------------------------
