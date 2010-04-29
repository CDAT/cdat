#!/usr/bin/env python
#
# The PCMDI Data Browser Alter Notebook Popup -  gui_alter_plot (PDB) module
#
###############################################################################
#                                                                             #
# Module:       gui_alter_plot notebook module                                #
#                                                                             #
# Copyright:    "See file Legal.htm for copyright information."               #
#                                                                             #
# Authors:      PCMDI Software Team                  			      #
#               Lawrence Livermore NationalLaboratory:                        #
#               support@pcmdi.llnl.gov                                        #
#                                                                             #
# Description:  GUI popup dialog to alter the appearance of the VCS plot      #
#                                                                             #
# Version:      4.0                                                           #
#                                                                             #
###############################################################################
#
#---------------------------------------------------------------------
# NOTE: need to use version of Python that imports Tkinter and Pmw
#---------------------------------------------------------------------
import Tkinter, Pmw
import sys, os, string, types, numpy
import vcs_function
from gui_support import gui_color
import gui_control

global_xticlabels = '*'
global_xmtics = '*'
global_yticlabels = '*'
global_ymtics = '*'

# Get the previously saved state of the GUI
try:
   fn = '%s/PCMDI_GRAPHICS' % os.environ['HOME']
   sys.path.append(fn)
   import vcdat_initial
except:
   pass

# Generate log10 dictionary values
def generate_log_values():
   major_log_axis={}
   minor_log_axis={}
   a=10000.0
   b=a/10.0
   for j in range(8):
    for i in range (int(a/b)):
     log_num = numpy.log10(a - (i*b))
     minor_log_axis[log_num] = str(a - (i*b))
     if ( log_num - int(log_num) ) == 0:
      major_log_axis[log_num] = str(a - (i*b))
    a=b
    b=a/10.0
   return major_log_axis, minor_log_axis

# Generate sine dictionary values
def generate_sine_values():
   major_sine_axis={}
   minor_sine_axis={}
   a =[-90,-60,-50,-30,-10,0,10,30,50,60,90]
   for j in range(len(a)):
       major_sine_axis[ numpy.sin( a[j] * numpy.pi / 180 ) ] = str( a[ j ] )
       k = a[ j ]
       if a[ j ] != 90:
          delta = numpy.absolute( a[j] - a[j+1] ) / 5
          for i in range ( 4 ):
              k +=  delta
              minor_sine_axis[ numpy.sin( k * numpy.pi / 180 ) ] = str( k )
   return major_sine_axis, minor_sine_axis


#-------------------------------------------------------------------------------
# Initialize the alter plot's default settings
#-------------------------------------------------------------------------------
def initialize(parent):
   # 			Set the X Labels
   try:
      parent.x_labels = vcdat_initial.set.x_labels
   except:
      parent.x_labels = 'bottom'
   if parent.x_labels is not None:
      parent.x_labels = string.lower(parent.x_labels)
      if parent.x_labels not in ['top','bottom','both']: parent.x_labels = 'bottom'

   # 			Set the X Major Ticks
   try:
     parent.x_major_ticks = vcdat_initial.set.x_major_ticks
   except:
     parent.x_major_ticks = 'bottom'
   if parent.x_major_ticks is not None:
      parent.x_major_ticks = string.lower(parent.x_major_ticks)
      if parent.x_major_ticks not in ['top','bottom','both']: parent.x_major_ticks = 'bottom'

   # 			Set the X Minor Ticks
   try:
     parent.x_minor_ticks = vcdat_initial.set.x_minor_ticks
   except:
     parent.x_minor_ticks = None
   if parent.x_minor_ticks is not None:
      parent.x_minor_ticks = string.lower(parent.x_minor_ticks)
      if parent.x_minor_ticks not in ['top','bottom','both', None]: parent.x_minor_ticks = None

   # 			Set the X Axis Type
   try:
     parent.x_axis_type = string.lower(vcdat_initial.set.x_axis_type)
   except:
     parent.x_axis_type = 'linear'
   if parent.x_axis_type not in ['linear', 'log10']: parent.x_axis_type = 'linear'

   # 			Set the X Major Values
   try:
     parent.x_major_values = vcdat_initial.set.x_major_values
   except:
     parent.x_major_values = {}

   # 			Set the X Minor Values
   try:
     parent.x_minor_values = vcdat_initial.set.x_minor_values
   except:
     parent.x_minor_values = {}

   # 			Set the Y Labels
   try:
      parent.y_labels = vcdat_initial.set.y_labels
   except:
      parent.y_labels = 'left'
   if parent.y_labels is not None:
      parent.y_labels = string.lower(parent.y_labels)
      if parent.y_labels not in ['left', 'right', 'both']: parent.y_labels = 'left'

   # 			Set the Y Major Ticks
   try:
      parent.y_major_ticks = vcdat_initial.set.y_major_ticks
   except:
      parent.y_major_ticks = 'left'
   if parent.y_major_ticks is not None:
      parent.y_major_ticks = string.lower(parent.y_major_ticks)
      if parent.y_major_ticks not in ['left', 'right', 'both']: parent.y_major_ticks = 'left'

   # 			Set the Y Minor Ticks
   try:
      parent.y_minor_ticks = vcdat_initial.set.y_minor_ticks
   except:
      parent.y_minor_ticks = None
   if parent.y_minor_ticks is not None:
      parent.y_minor_ticks = string.lower(parent.y_minor_ticks)
      if parent.y_minor_ticks not in ['left', 'right', 'both']: parent.y_minor_ticks = None

   # 			Set the Y Axis Type
   try:
      parent.y_axis_type = string.lower(vcdat_initial.set.y_axis_type)
   except:
      parent.y_axis_type = 'linear'
   if parent.y_axis_type not in ['linear', 'log10']: parent.y_axis_type = 'linear'

   # 			Set the Y Major Values
   try:
      parent.y_major_values = vcdat_initial.set.y_major_values
   except:
      parent.y_major_values = {}

   # 			Set the Y Minor Values
   try:
      parent.y_minor_values = vcdat_initial.set.y_minor_values
   except:
      parent.y_minor_values = {}

   # 			Set the Legend Status
   try:
      parent.legend_status = string.lower(vcdat_initial.set.legend_status)
   except:
      parent.legend_status = 'on'
   if parent.legend_status not in ['on', 'off']:
      parent.legend_status = 'on'
   # 			Set the Legend Orientation
   try:
      parent.legend_orientation = string.lower(vcdat_initial.set.legend_orientation)
   except:
      parent.legend_orientation = 'horizontal'
   if parent.legend_orientation not in ['horizontal', 'vertical']:
      parent.legend_orientation = 'horizontal'

   # 			Set the Legend X Position 
   try:
      parent.legend_x_position = vcdat_initial.set.legend_x_position
   except:
      parent.legend_x_position = 0
   if (type(parent.legend_x_position) != types.IntType) or (parent.legend_x_position not in range(-100,101)):
      parent.legend_x_position = 0

   # 			Set the Legend Y Position 
   try:
      parent.legend_y_position = vcdat_initial.set.legend_y_position
   except:
      parent.legend_y_position = 0
   if (type(parent.legend_y_position) != types.IntType) or (parent.legend_y_position not in range(-100,101)):
      parent.legend_y_position = 0

   # 			Set the Legend Width
   try:
      parent.legend_width = vcdat_initial.set.legend_width
   except:
      parent.legend_width = 0
   if (type(parent.legend_width) != types.IntType) or (parent.legend_width not in range(-100,101)):
      parent.legend_width = 0

   # 			Set the Legend Height 
   try:
      parent.legend_height = vcdat_initial.set.legend_height
   except:
      parent.legend_height = 0
   if (type(parent.legend_height) != types.IntType) or (parent.legend_height not in range(-100,101)):
      parent.legend_height = 0

   # 			Set the Plot X Position 
   try:
      parent.plot_x_position = vcdat_initial.set.plot_x_position
   except:
      parent.plot_x_position = 0
   if (type(parent.plot_x_position) != types.IntType) or (parent.plot_x_position not in range(-100,101)):
      parent.plot_x_position = 0

   # 			Set the Plot Y Position 
   try:
      parent.plot_y_position = vcdat_initial.set.plot_y_position
   except:
      parent.plot_y_position = 0
   if (type(parent.plot_y_position) != types.IntType) or (parent.plot_y_position not in range(-100,101)):
      parent.plot_y_position = 0
   # 			Set the Plot Ratio 
   try:
      parent.plot_ratio = vcdat_initial.set.plot_ratio
   except:
      parent.plot_ratio = 'autot'

   # 			Set the Plot Width
   try:
      parent.plot_width = vcdat_initial.set.plot_width
   except:
      parent.plot_width = 0
   if (type(parent.plot_width) != types.IntType) or (parent.plot_width not in range(-100,101)):
      parent.plot_width = 0

   # 			Set the Plot Height
   try:
      parent.plot_height = vcdat_initial.set.plot_height
   except:
      parent.plot_height = 0
   if (type(parent.plot_height) != types.IntType) or (parent.plot_height not in range(-100,101)):
      parent.plot_height = 0


#-------------------------------------------------------------------------------
# Popup to alter the visual appearance of the VCS plot.                                  
#-------------------------------------------------------------------------------
#
class note_book:
   def __init__(self, parent):
        self.parent = parent
        self.parent.alter_notebook = self
        self.preview_flg = 0
        self.dialog = Pmw.Dialog( parent,
            title = 'Alter Plot',
            buttons = ('Preview', 'Reset', 'Apply', 'Cancel'),
            defaultbutton = 'Apply',
            command = gui_control.Command(self.execute, parent) )

        if parent.menu.popup_window_settings_flg == 1:
           self.dialog.transient( self.parent ) # Keep widget on top of its parent

        # Create and pack the NoteBook
        notebook = Pmw.NoteBook( self.dialog.interior() )
        notebook.pack(fill = 'both', expand = 1, padx = 5, pady = 5)

        #----------------------------------------------------------------------
        # Add the "Template" page to the notebook.
        #----------------------------------------------------------------------
  #      page = notebook.add('Template')
  #      notebook.tab('Template').focus_set()
  #      self.template_page = write_template_page( self, page )

        #----------------------------------------------------------------------
        # Add the "X-Axis" page to the notebook.
        #----------------------------------------------------------------------
        page = notebook.add('X-Axis')
        notebook.tab('X-Axis').focus_set()
        self.x_axis_page = write_x_axis_page( self, page )

        #----------------------------------------------------------------------
        # Add the "Y-Axis" page to the notebook
        #----------------------------------------------------------------------
        page = notebook.add('Y-Axis')
        self.y_axis_page = write_y_axis_page( self, page )

        #----------------------------------------------------------------------
        # Add the "Shape" page to the notebook
        #----------------------------------------------------------------------
        page = notebook.add('Shape')
        self.shape_page = write_shape_page( self, page )

        #----------------------------------------------------------------------
        # Add the "Legend" page to the notebook
        #----------------------------------------------------------------------
        page = notebook.add('Legend')
        self.legend_page = write_legend_page( self, page )

        # This is needed for the Reset and Cancel
        self.hold_alter_original_attr_settings( parent )

        # Position dialog popup
        parent_geom = self.parent.geometry()
        geom = string.split(parent_geom, '+')
        d1 = string.atoi( geom[1] )
        d2 = string.atoi( geom[2] )
        self.dialog.geometry( "+%d+%d" % (d1, d2) )
        notebook.setnaturalsize()
#
#---------------------------------------------------------------------------
# Define events for notebook         
#---------------------------------------------------------------------------
#
   def cbn1_cb( self ):
      if self.cbn1.value == 1: self.cbn1.value = 0
      else: self.cbn1.value = 1
   def cbn2_cb( self ):
      if self.cbn2.value == 1: self.cbn2.value = 0
      else: self.cbn2.value = 1
   #
   def cbn3_cb( self ):
      if self.cbn3.value == 1: self.cbn3.value = 0
      else: self.cbn3.value = 1
   def cbn4_cb( self ):
      if self.cbn4.value == 1: self.cbn4.value = 0
      else: self.cbn4.value = 1
   #
   def cbn5_cb( self ):
      if self.cbn5.value == 1: self.cbn5.value = 0
      else: self.cbn5.value = 1
   def cbn6_cb( self ):
      if self.cbn6.value == 1: self.cbn6.value = 0
      else: self.cbn6.value = 1
   #
   def set_default_xlabels( self ):
      try:
         self.eny1.setentry( '*' )
         self.eny2.setentry( '*' )
      except:
         pass
      try:
         self.parent.graphics_method.xticlabels1 = '*'
         self.parent.graphics_method.xticlabels2 = '*'
         self.parent.graphics_method.xmtics1 = '*'
         self.parent.graphics_method.xmtics2 = '*'
      except:
         pass
   #
   def set_default_ylabels( self ):
      try:
         self.eny3.setentry( '*' )
         self.eny4.setentry( '*' )
      except:
         pass
      try:
         self.parent.graphics_method.yticlabels1 = '*'
         self.parent.graphics_method.yticlabels2 = '*'
         self.parent.graphics_method.ymtics1 = '*'
         self.parent.graphics_method.ymtics2 = '*'
      except:
         pass
   #
   def cbn7_cb( self ):
      self.cbn7.select()
      self.cbn8.deselect()
#      self.cbn9.deselect()
      self.cbnLn1.deselect()
      self.cbnAwt1.deselect()
#      self.cbn10.deselect()
      self.cbn7.value = 1
      self.cbn8.value = 0
      self.cbnLn1.value = 0
      self.cbnAwt1.value = 0
#      self.cbn9.value = 0
#      self.cbn10.value = 0
      self.set_default_xlabels()
   def cbn8_cb( self ):
      '''
      major_log_axis, minor_log_axis = generate_log_values()
      try:
         self.eny1.setentry( major_log_axis )
         self.eny2.setentry( minor_log_axis )
      except:
         pass
      self.parent.graphics_method.xticlabels1 = major_log_axis
      self.parent.graphics_method.xticlabels2 = major_log_axis
      self.parent.graphics_method.xmtics1 = minor_log_axis
      self.parent.graphics_method.xmtics2 = minor_log_axis
      '''
      self.cbn7.deselect()
      self.cbn8.select()
#      self.cbn9.deselect()
      self.cbnLn1.deselect()
      self.cbnAwt1.deselect()
#      self.cbn10.deselect()
      self.cbn7.value = 0
      self.cbn8.value = 1
      self.cbnLn1.value = 0
      self.cbnAwt1.value = 0
#      self.cbn9.value = 0
#      self.cbn10.value = 0
      self.set_default_xlabels()
   def cbnLn1_cb( self ):
      self.cbn7.deselect()
      self.cbn8.deselect()
#      self.cbn9.deselect()
      self.cbnLn1.select()
      self.cbnAwt1.deselect()
#      self.cbn10.deselect()
      self.cbn7.value = 0
      self.cbn8.value = 0
      self.cbnLn1.value = 1
      self.cbnAwt1.value = 0
#      self.cbn9.value = 0
#      self.cbn10.value = 0
      self.set_default_xlabels()
   def cbnAwt1_cb( self ):
      self.cbn7.deselect()
      self.cbn8.deselect()
#      self.cbn9.deselect()
      self.cbnLn1.deselect()
      self.cbnAwt1.select()
#      self.cbn10.deselect()
      self.cbn7.value = 0
      self.cbn8.value = 0
      self.cbnLn1.value = 0
      self.cbnAwt1.value = 1
#      self.cbn9.value = 0
#      self.cbn10.value = 0
      self.set_default_xlabels()
   def cbn9_cb( self ):
      '''
      major_sine_axis, minor_sine_axis = generate_sine_values()
      try:
         self.eny1.setentry( major_sine_axis )
         self.eny2.setentry( minor_sine_axis )
      except:
         pass
      self.parent.graphics_method.xticlabels1 = major_sine_axis
      self.parent.graphics_method.xticlabels2 = major_sine_axis
      self.parent.graphics_method.xmtics1 = minor_sine_axis
      self.parent.graphics_method.xmtics2 = minor_sine_axis
      '''
      self.cbn7.deselect()
      self.cbn8.deselect()
#      self.cbn9.select()
      self.cbnLn1.deselect()
      self.cbnAwt1.deselect()
#      self.cbn10.deselect()
      self.cbn7.value = 0
      self.cbn8.value = 0
      self.cbnLn1.value = 0
      self.cbnAwt1.value = 0
#      self.cbn9.value = 1
#      self.cbn10.value = 0
      self.set_default_xlabels()
   def cbn10_cb( self ):
      self.cbn7.deselect()
      self.cbn8.deselect()
#      self.cbn9.deselect()
#      self.cbn10.select()
      self.cbn7.value = 0
      self.cbn8.value = 0
#      self.cbn9.value = 0
#      self.cbn10.value = 1

   def cbn11_cb( self ):
      if self.cbn11.value == 1: self.cbn11.value = 0
      else: self.cbn11.value = 1
   def cbn12_cb( self ):
      if self.cbn12.value == 1: self.cbn12.value = 0
      else: self.cbn12.value = 1
   #
   def cbn13_cb( self ):
      if self.cbn13.value == 1: self.cbn13.value = 0
      else: self.cbn13.value = 1
   def cbn14_cb( self ):
      if self.cbn14.value == 1: self.cbn14.value = 0
      else: self.cbn14.value = 1
   #
   def cbn15_cb( self ):
      if self.cbn15.value == 1: self.cbn15.value = 0
      else: self.cbn15.value = 1
   def cbn16_cb( self ):
      if self.cbn16.value == 1: self.cbn16.value = 0
      else: self.cbn16.value = 1
   #
   def cbn17_cb( self ):
      self.cbn17.select()
      self.cbn18.deselect()
#      self.cbn19.deselect()
      self.cbnLn2.deselect()
      self.cbnAwt2.deselect()
#      self.cbn20.deselect()
      self.cbn17.value = 1
      self.cbn18.value = 0
#      self.cbn19.value = 0
      self.cbnLn2.value = 0
      self.cbnAwt2.value = 0
#      self.cbn20.value = 0
      self.set_default_ylabels()
   def cbn18_cb( self ):
      self.cbn17.deselect()
      self.cbn18.select()
#      self.cbn19.deselect()
      self.cbnLn2.deselect()
      self.cbnAwt2.deselect()
#      self.cbn20.deselect()
      '''
      major_log_axis, minor_log_axis = generate_log_values()
      try:
         self.eny3.setentry( major_log_axis )
         self.eny4.setentry( minor_log_axis )
      except:
         pass
      self.parent.graphics_method.yticlabels1 = major_log_axis
      self.parent.graphics_method.yticlabels2 = major_log_axis
      self.parent.graphics_method.ymtics1 = minor_log_axis
      self.parent.graphics_method.ymtics2 = minor_log_axis
'''
      self.cbn17.value = 0
      self.cbn18.value = 1
#      self.cbn19.value = 0
      self.cbnLn2.value = 0
      self.cbnAwt2.value = 0
#      self.cbn20.value = 0
      self.set_default_ylabels()
   def cbnLn2_cb( self ):
      self.cbn17.deselect()
      self.cbn18.deselect()
#      self.cbn19.deselect()
      self.cbnLn2.select()
      self.cbnAwt2.deselect()
#      self.cbn20.deselect()
      self.cbn17.value = 0
      self.cbn18.value = 0
#      self.cbn19.value = 0
      self.cbnLn2.value = 1
      self.cbnAwt2.value = 0
      self.set_default_ylabels()
   def cbnAwt2_cb( self ):
      self.cbn17.deselect()
      self.cbn18.deselect()
#      self.cbn19.deselect()
      self.cbnLn2.deselect()
      self.cbnAwt2.select()
#      self.cbn20.deselect()
      self.cbn17.value = 0
      self.cbn18.value = 0
#      self.cbn19.value = 0
      self.cbnLn2.value = 0
      self.cbnAwt2.value = 1
      self.set_default_ylabels()
   def cbn19_cb( self ):
      self.cbn17.deselect()
      self.cbn18.deselect()
      self.cbn19.select()
      self.cbnLn2.deselect()
      self.cbnAwt2.deselect()
#      self.cbn20.deselect()
      major_sine_axis, minor_sine_axis = generate_sine_values()
      try:
         self.eny3.setentry( major_sine_axis )
         self.eny4.setentry( minor_sine_axis )
      except:
         pass
      self.parent.graphics_method.yticlabels1 = major_sine_axis
      self.parent.graphics_method.yticlabels2 = major_sine_axis
      self.parent.graphics_method.ymtics1 = minor_sine_axis
      self.parent.graphics_method.ymtics2 = minor_sine_axis
      self.cbn17.value = 0
      self.cbn18.value = 0
      self.cbn19.value = 1
      self.cbnLn2.value = 0
      self.cbnAwt2.value = 0
#      self.cbn20.value = 0
   def cbn20_cb( self ):
      self.cbn17.deselect()
      self.cbn18.deselect()
#      self.cbn19.deselect()
#      self.cbn20.select()
      self.cbn17.value = 0
      self.cbn18.value = 0
#      self.cbn19.value = 0
#      self.cbn20.value = 1

   def cbnstat_on_cb( self ):
      self.cbnstat_on.select()
      self.cbnstat_off.deselect()
      self.cbnstat_on.value = 1
      self.cbnstat_off.value = 0
      try:
         self.ctr1.setentry( 0 )
         self.ctr2.setentry( 0 )
         self.ctr3.setentry( 0 )
         self.ctr4.setentry( 0 )
         self.ctr5.setentry( 0 )
         self.ctr6.setentry( 0 )
         self.ctr7.setentry( 0 )
      except:
         pass

   def cbnstat_off_cb( self ):
      self.cbnstat_off.select()
      self.cbnstat_on.deselect()
      self.cbnstat_off.value = 1
      self.cbnstat_on.value = 0
      try:
         self.ctr1.setentry( 0 )
         self.ctr2.setentry( 0 )
         self.ctr3.setentry( 0 )
         self.ctr4.setentry( 0 )
         self.ctr5.setentry( 0 )
         self.ctr6.setentry( 0 )
         self.ctr7.setentry( 0 )
      except:
         pass

   def cbn21_cb( self ):
      self.cbn21.select()
      self.cbn22.deselect()
      self.cbn21.value = 1
      self.cbn22.value = 0
      try:
         self.ctr1.setentry( 0 )
         self.ctr2.setentry( 0 )
         self.ctr3.setentry( 0 )
         self.ctr4.setentry( 0 )
         self.ctr5.setentry( 0 )
         self.ctr6.setentry( 0 )
         self.ctr7.setentry( 0 )
      except:
         pass

   def cbn22_cb( self ):
      self.cbn21.deselect()
      self.cbn22.select()
      self.cbn21.value = 0
      self.cbn22.value = 1
      try:
         self.ctr1.setentry( -2 )
         self.ctr2.setentry( 7 )
         self.ctr3.setentry( 0 )
         self.ctr4.setentry( -15 )
         self.ctr5.setentry( 0 )
         self.ctr6.setentry( 0 )
         self.ctr7.setentry( -7 )
      except:
         pass

   def get_x_settings( self ):
      self.parent.x_labels = None
      if (self.cbn1.value == 1) and (self.cbn2.value == 1):
         self.parent.x_labels = 'both'
      elif (self.cbn1.value == 1):
         self.parent.x_labels = 'top'
      elif (self.cbn2.value == 1):
         self.parent.x_labels = 'bottom'

      self.parent.x_major_ticks = None
      if (self.cbn3.value == 1) and (self.cbn4.value == 1):
         self.parent.x_major_ticks = 'both'
      elif (self.cbn3.value == 1):
         self.parent.x_major_ticks = 'top'
      elif (self.cbn4.value == 1):
         self.parent.x_major_ticks = 'bottom'

      self.parent.x_minor_ticks = None
      if (self.cbn5.value == 1) and (self.cbn6.value == 1):
         self.parent.x_minor_ticks = 'both'
      elif (self.cbn5.value == 1):
         self.parent.x_minor_ticks = 'top'
      elif (self.cbn6.value == 1):
         self.parent.x_minor_ticks = 'bottom'

      if self.cbn7.value == 1:
         self.parent.x_axis_type = 'linear'
      if self.cbn8.value == 1:
         self.parent.x_axis_type = 'log10'
      if self.cbnLn1.value == 1:
         self.parent.x_axis_type = 'ln'
      if self.cbnAwt1.value == 1:
         self.parent.x_axis_type = 'area_wt'
#      if self.cbn9.value == 1:
#         self.parent.x_axis_type = 'area_wt'
#      if self.cbn10.value == 1:
#         self.parent.x_axis_type = 'exp'

   def get_y_settings( self ):
      self.parent.y_labels = None
      if (self.cbn11.value == 1) and (self.cbn12.value == 1):
         self.parent.y_labels = 'both'
      elif (self.cbn11.value == 1):
         self.parent.y_labels = 'left'
      elif (self.cbn12.value == 1):
         self.parent.y_labels = 'right'

      self.parent.y_major_ticks = None
      if (self.cbn13.value == 1) and (self.cbn14.value == 1):
         self.parent.y_major_ticks = 'both'
      elif (self.cbn13.value == 1):
         self.parent.y_major_ticks = 'left'
      elif (self.cbn14.value == 1):
         self.parent.y_major_ticks = 'right'

      self.parent.y_minor_ticks = None
      if (self.cbn15.value == 1) and (self.cbn16.value == 1):
         self.parent.y_minor_ticks = 'both'
      elif (self.cbn15.value == 1):
         self.parent.y_minor_ticks = 'left'
      elif (self.cbn16.value == 1):
         self.parent.y_minor_ticks = 'right'

      if self.cbn17.value == 1:
         self.parent.y_axis_type = 'linear'
      if self.cbn18.value == 1:
         self.parent.y_axis_type = 'log10'
      if self.cbnLn2.value == 1:
         self.parent.y_axis_type = 'ln'
      if self.cbnAwt2.value == 1:
         self.parent.y_axis_type = 'area_wt'
#      if self.cbn19.value == 1:
#         self.parent.y_axis_type = 'area_wt'
#      if self.cbn20.value == 1:
#         self.parent.y_axis_type = 'exp'

   def get_x_y_axis_entries( self ):
      global global_xticlabels, global_xmtics
      global global_yticlabels, global_ymtics

      # Get the X-Axis settings
      global_xticlabels = self._get_axis_dict( self.eny1 )
      global_xmtics = self._get_axis_dict( self.eny2 )

      # Get the Y-Axis settings
      global_yticlabels = self._get_axis_dict( self.eny3 )
      global_ymtics = self._get_axis_dict( self.eny4 )

   def _get_axis_dict(self, entry_box):
      "Get an entry item that is supposed to be a dictionary."
      text = entry_box.get().strip()
      if (text == '*') or (text == ''):
         text = '*'
      elif text == 'None':
         text = 'None'
      else:
        try:
           text = eval(text)
        except:
           raise ValueError, 'Bad item ' + text + ' in entry box.'
      if (isinstance(text, types.ListType)):
         d={}
         for x in text: d[x] = str(x)
         text = d
      elif (isinstance(text, types.DictType)):
         for x in text: text[x] = str(text[x])
      return text
      

   def get_legend_settings( self ):
      if self.cbnstat_on.value == 1:
         self.parent.legend_status = 'on'
      else:
         self.parent.legend_status = 'off'
      #
      if self.cbn21.value == 1:
         self.parent.legend_orientation = 'horizontal'
      else:
         self.parent.legend_orientation = 'vertical'
      #
      self.parent.legend_x_position = self.ctr1.get()
      self.parent.legend_y_position = self.ctr2.get()
      self.parent.legend_width = self.ctr3.get()
      self.parent.legend_height = self.ctr4.get()

   def get_plot_settings( self ):
      self.parent.plot_x_position = self.ctr5.get()
      self.parent.plot_y_position = self.ctr6.get()
      self.parent.plot_ratio = self.eny5.get().strip().lower()
      tmp=self.chkb5.getcurselection()
      if tmp=='yes':
         self.parent.plot_ratio+='t'
      self.parent.plot_width = self.ctr7.get()
      self.parent.plot_height = self.ctr8.get()

   def get_template_settings( self ):
      template_name = self.t_list.get( )
      template_list = self.parent.vcs[ parent.vcs_id ].listelements('template')
      if template_name not in template_list:
         template_name = 'ASD'
      self.parent.template_name = template_name

   def execute(self, parent, result):
      if result == 'Preview':
         self.preview_flg = 1
         self.apply( parent )
      elif result == 'Reset':
         self.preview_flg = 0
         self.reset_alter_to_original_settings( parent )
      elif result == 'Clear':
         self.preview_flg = 0
      elif result == 'Apply':
         self.preview_flg = 0
         self.apply( parent )
         self.dialog.destroy()
      elif (result == 'Cancel') or (result == None):
         if self.preview_flg: self.reset_alter_to_original_settings( parent )
         self.dialog.destroy()

      '''
      if result == 'Re-Plot':
    #     self.get_template_settings()
         self.get_x_settings()
         self.get_y_settings()
         self.get_legend_settings()
         self.get_plot_settings()
         self.get_x_y_axis_entries()
         parent.panelGC.evt_plot( parent )
#         vcs_function.re_plot( parent, 0 )
      elif result == 'Apply':
    #     self.get_template_settings()
         self.get_x_settings()
         self.get_y_settings()
         self.get_legend_settings()
         self.get_plot_settings()
         self.get_x_y_axis_entries( )
      else:
         self.dialog.destroy()
   #
'''

   def hold_alter_original_attr_settings ( self, parent ):
       try:
          # Store the Graphics Method Store
          self.store_gm_xticlabels1 = parent.graphics_method.xticlabels1
          self.store_gm_xmtics1 = parent.graphics_method.xmtics1
          self.store_gm_yticlabels1 = parent.graphics_method.yticlabels1
          self.store_gm_ymtics1 = parent.graphics_method.ymtics1
   
          # X - Axis Store
          self.store_x_labels       = parent.x_labels
          self.store_x_major_ticks  = parent.x_major_ticks
          self.store_x_minor_ticks  = parent.x_minor_ticks
          self.store_x_axis_type    = parent.x_axis_type
          self.store_x_major_values = parent.x_major_values
          self.store_x_minor_values = parent.x_minor_values
   
          # Y - Axis Store
          self.store_y_labels       = parent.y_labels
          self.store_y_major_ticks  = parent.y_major_ticks
          self.store_y_minor_ticks  = parent.y_minor_ticks
          self.store_y_axis_type    = parent.y_axis_type
          self.store_y_major_values = parent.y_major_values
          self.store_y_minor_values = parent.y_minor_values
   
          # Legend Store
          self.store_legend_status      = parent.legend_status
          self.store_legend_orientation = parent.legend_orientation
          self.store_legend_x_position  = parent.legend_x_position
          self.store_legend_y_position  = parent.legend_y_position
          self.store_legend_width       = parent.legend_width
          self.store_legend_height      = parent.legend_height
   
          # Shape Store
          self.store_plot_x_position  = parent.plot_x_position
          self.store_plot_y_position  = parent.plot_y_position
          self.store_plot_ratio       = parent.plot_ratio
          self.store_plot_width       = parent.plot_width
          self.store_plot_height      = parent.plot_height
       except:
          pass

   def reset_alter_to_original_settings( self, parent ):
       global global_xticlabels, global_xmtics
       global global_yticlabels, global_ymtics

       # Graphics Method Reset
       global_xticlabels =self.store_gm_xticlabels1
       global_xmtics = self.store_gm_xmtics1
       global_yticlabels =self.store_gm_yticlabels1
       global_ymtics = self.store_gm_ymtics1

       # X - Axis Reset
       parent.x_labels       = self.store_x_labels
       self.cbn1.deselect(); self.cbn2.deselect() # Deselect the toggle buttons
       self.cbn1.value=0; self.cbn2.value=0       # Initialize the toggle flags
       if (parent.x_labels == 'top') or (parent.x_labels == 'both'): self.cbn1.invoke()
       if (parent.x_labels == 'bottom') or (parent.x_labels == 'both'): self.cbn2.invoke()
       parent.x_major_ticks  = self.store_x_major_ticks
       self.cbn3.deselect(); self.cbn4.deselect() # Deselect the toggle buttons
       self.cbn3.value=0; self.cbn4.value=0       # Initialize the toggle flags
       if (parent.x_major_ticks == 'top') or (parent.x_major_ticks == 'both'): self.cbn3.invoke()
       if (parent.x_major_ticks == 'bottom') or (parent.x_major_ticks == 'both'): self.cbn4.invoke()
       parent.x_minor_ticks  = self.store_x_minor_ticks
       self.cbn5.deselect(); self.cbn6.deselect() # Deselect the toggle buttons
       self.cbn5.value=0; self.cbn6.value=0       # Initialize the toggle flags
       if (parent.x_minor_ticks == 'top') or (parent.x_minor_ticks == 'both'): self.cbn5.invoke()
       if (parent.x_minor_ticks == 'bottom') or (parent.x_minor_ticks == 'both'): self.cbn6.invoke()
       parent.x_axis_type    = self.store_x_axis_type
       self.cbn7.deselect(); self.cbn8.deselect(); self.cbnLn1.deselect(); self.cbnAwt1.deselect(); # Deselect toggle buttons
       if (parent.x_axis_type == 'linear'): self.cbn7_cb()
       elif (parent.x_axis_type == 'log10'): self.cbn8_cb()
       elif (parent.x_axis_type == 'ln'): self.cbnLn1_cb()
       elif (parent.x_axis_type == 'area_wt'): self.cbnAwt1_cb()

       # Y - Axis Reset
       parent.y_labels       = self.store_y_labels
       self.cbn11.deselect(); self.cbn12.deselect() # Deselect the toggle buttons
       self.cbn11.value=0; self.cbn12.value=0       # Initialize the toggle flags
       if (parent.y_labels == 'left') or (parent.y_labels == 'both'): self.cbn11.invoke()
       if (parent.y_labels == 'right') or (parent.y_labels == 'both'): self.cbn12.invoke()
       parent.y_major_ticks  = self.store_y_major_ticks
       self.cbn13.deselect(); self.cbn14.deselect() # Deselect the toggle buttons
       self.cbn13.value=0; self.cbn14.value=0       # Initialize the toggle flags
       if (parent.y_major_ticks == 'left') or (parent.y_major_ticks == 'both'): self.cbn13.invoke()
       if (parent.y_major_ticks == 'right') or (parent.y_major_ticks == 'both'): self.cbn14.invoke()
       parent.y_minor_ticks  = self.store_y_minor_ticks
       self.cbn15.deselect(); self.cbn16.deselect() # Deselect the toggle buttons
       self.cbn15.value=0; self.cbn16.value=0       # Initialize the toggle flags
       if (parent.y_minor_ticks == 'left') or (parent.y_minor_ticks == 'both'): self.cbn15.invoke()
       if (parent.y_minor_ticks == 'right') or (parent.y_minor_ticks == 'both'): self.cbn16.invoke()
       parent.y_axis_type    = self.store_y_axis_type
       self.cbn17.deselect(); self.cbn18.deselect(); self.cbnLn2.deselect(); self.cbnAwt2.deselect();# Deselect toggle buttons
       if (parent.y_axis_type == 'linear'): self.cbn17_cb()
       elif (parent.y_axis_type == 'log10'): self.cbn18_cb()
       elif (parent.y_axis_type == 'ln'): self.cbnLn2_cb()
       elif (parent.y_axis_type == 'area_wt'): self.cbnAwt2_cb()

       # Legend Store
       parent.legend_status = self.store_legend_status
       self.cbnstat_on.deselect(); self.cbnstat_off.deselect() # Deselect the toggle buttons
       self.cbnstat_on.value=0; self.cbnstat_off.value=0       # Initialize the toggle flags
       if (parent.legend_status == 'on'): self.cbnstat_on.invoke()
       if (parent.legend_status == 'off'): self.cbnstat_off.invoke()

       parent.legend_orientation = self.store_legend_orientation
       self.cbn21.deselect(); self.cbn22.deselect() # Deselect the toggle buttons
       self.cbn21.value=0; self.cbn22.value=0       # Initialize the toggle flags
       if (parent.legend_orientation == 'horizontal'): self.cbn21.invoke()
       if (parent.legend_orientation == 'vertical'): self.cbn22.invoke()
       parent.legend_x_position = self.store_legend_x_position
       self.ctr1.setentry( self.store_legend_x_position )
       parent.legend_y_position = self.store_legend_y_position
       self.ctr2.setentry( self.store_legend_y_position )
       parent.legend_width = self.store_legend_width
       self.ctr3.setentry( self.store_legend_height )
       parent.legend_height = self.store_legend_height
       self.ctr4.setentry( self.store_legend_height )

       # Shape Store
       parent.plot_x_position = self.store_plot_x_position
       self.ctr5.setentry( self.store_plot_x_position )
       parent.plot_y_position = self.store_plot_y_position
       self.ctr6.setentry( self.store_plot_y_position )
       parent.plot_ratio = self.store_plot_ratio
       if self.store_plot_ratio[-1]=='t':
          self.eny5.setentry( self.store_plot_ratio[:-1] )
       else:
          self.eny5.setentry( self.store_plot_ratio )
       parent.plot_width = self.store_plot_width
       self.ctr7.setentry( self.store_plot_width )
       parent.plot_height = self.store_plot_height
       self.ctr8.setentry( self.store_plot_height )

       # Redraw the plot
       parent.panelGC.evt_plot( parent )

   def apply( self, parent ):
#       self.get_template_settings() - No longer shown here
       self.get_x_settings()
       self.get_y_settings()
       self.get_legend_settings()
       self.get_plot_settings()
       self.get_x_y_axis_entries( )

       # Redraw the plot
       parent.panelGC.evt_plot( parent )

#
#---------------------------------------------------------------------
#
# End Popup Dialogs
#
#---------------------------------------------------------------------
#

#
#----------------------------------------------------------------------
# Write the "Template" page of the notebook.
#----------------------------------------------------------------------
def write_template_page( self, page ):
   group = Pmw.Group(page, tag_text = 'Select Template and Set Template Attributes')
   group.pack(fill = 'both', expand = 1, padx = 5, pady = 5)
   #     1st - line in X-Axis Info group
   fm=Tkinter.Frame(group.interior())

   template_list = self.parent.vcs[ parent.vcs_id ].listelements('template')
   template_name = 'default'
   if self.parent.template_name in  template_list:
      template_name = self.parent.template_name
   self.t_list = Pmw.ComboBox(fm,
                label_text = 'Select Template:',
	        labelpos = 'nw',
		scrolledlist_items = template_list,
                entryfield_value = template_name,
                entry_background='green',
                entry_foreground='black',
                dropdown = 0
#                selectioncommand = self.changeText
	)
   self.t_list.pack(side = 'left', fill = 'both', padx = 8, pady = 8)
   self.t_list.component('listbox').configure(
                background = 'green',
                foreground='black',
                )

   fm.pack( side='top', fill='both', expand=1 )
#
#----------------------------------------------------------------------
# Write the "X-Axis" page of the notebook.
#----------------------------------------------------------------------
def write_x_axis_page( self, page ):
   group = Pmw.Group(page, tag_text = 'View X-Axis Labels and Ticks')
   group.pack(fill = 'both', expand = 1, padx = 5, pady = 5)
   #     1st - line in X-Axis Info group
   fm=Tkinter.Frame(group.interior())
   lbl1=Tkinter.Label(fm, text = "X_Labels:", justify = 'left')
   lbl1.pack( side='left', fill = 'both', padx=5, pady=5 )
   self.cbn1 = Tkinter.Checkbutton(fm, text = 'Top', selectcolor=gui_color.one, command=self.cbn1_cb)
   self.cbn1.pack( side='left', fill = 'both', after=lbl1, padx=5, pady=5 )
   self.cbn2 = Tkinter.Checkbutton(fm, text = 'Bottom', selectcolor=gui_color.one,command=self.cbn2_cb)
   self.cbn2.pack( side='left', fill = 'both', after=self.cbn1, padx=5, pady=5 )
   self.cbn1.value=0
   self.cbn2.value=0
   if (self.parent.x_labels == 'top') or (self.parent.x_labels == 'both'):
       self.cbn1.invoke()
   if (self.parent.x_labels == 'bottom') or (self.parent.x_labels == 'both'): 
       self.cbn2.invoke()
   fm.pack( side='top', fill='both', expand=1 )
   #
   fm=Tkinter.Frame(group.interior())
   lbl2=Tkinter.Label(fm, text = "X_Major_Ticks:", justify = 'left')
   lbl2.pack( side='left', fill = 'both', padx=5, pady=5 )
   self.cbn3 = Tkinter.Checkbutton(fm, text = 'Top', selectcolor=gui_color.one, command=self.cbn3_cb)
   self.cbn3.pack( side='left', fill = 'both', after=lbl2, padx=5, pady=5 )
   self.cbn4 = Tkinter.Checkbutton(fm, text = 'Bottom', selectcolor=gui_color.one, command=self.cbn4_cb)
   self.cbn4.pack( side='left', fill = 'both', after=self.cbn3, padx=5, pady=5 )
   self.cbn3.value=0
   self.cbn4.value=0
   if (self.parent.x_major_ticks == 'top') or (self.parent.x_major_ticks == 'both'):
       self.cbn3.invoke()
   if (self.parent.x_major_ticks=='bottom') or (self.parent.x_major_ticks=='both'):
       self.cbn4.invoke()
   fm.pack( side='top', fill='both', expand=1 )
   #
   fm=Tkinter.Frame(group.interior())
   lbl3=Tkinter.Label(fm, text = "X_Minor_Ticks:", justify = 'left')
   lbl3.pack( side='left', fill = 'both', padx=5, pady=5 )
   self.cbn5 = Tkinter.Checkbutton(fm, text = 'Top', selectcolor=gui_color.one, command=self.cbn5_cb)
   self.cbn5.pack( side='left', fill = 'both', after=lbl3, padx=5, pady=5 )
   self.cbn6 = Tkinter.Checkbutton(fm, text = 'Bottom', selectcolor=gui_color.one, command=self.cbn6_cb)
   self.cbn6.pack( side='left', fill = 'both', after=self.cbn5, padx=5, pady=5 )
   self.cbn5.value=0
   self.cbn6.value=0
   if (self.parent.x_minor_ticks == 'top') or (self.parent.x_minor_ticks == 'both'):
       self.cbn5.invoke()
   if (self.parent.x_minor_ticks=='bottom') or (self.parent.x_minor_ticks=='both'):
       self.cbn6.invoke()
   fm.pack( side='top', fill='both', expand=1 )

   group = Pmw.Group(page, tag_text = 'X-Axis Type')
   group.pack(fill = 'both', expand = 1, padx = 5, pady = 5)
   #
   fm=Tkinter.Frame(group.interior())
   lbl4=Tkinter.Label(fm, text = "X_Type:", justify = 'left')
   lbl4.pack( side='left', fill = 'both', padx=5, pady=5 )
   self.cbn7 = Tkinter.Checkbutton(fm, text = 'Linear', selectcolor=gui_color.one, command=self.cbn7_cb)
   self.cbn7.pack( side='left', fill = 'both', padx=5, pady=5 )
   self.cbn8 = Tkinter.Checkbutton(fm, text = 'Log10', selectcolor=gui_color.one, command=self.cbn8_cb)
   self.cbn8.pack( side='left', fill = 'both', after=self.cbn7, padx=5, pady=5 )
   self.parent.balloon.bind( self.cbn8, "This X-axis type only works for the Isofill and Isoline\ngraphics methods." )
   self.cbnLn1 = Tkinter.Checkbutton(fm, text = 'Ln', selectcolor=gui_color.one, command=self.cbnLn1_cb)
   self.cbnLn1.pack( side='left', fill = 'both', after=self.cbn8, padx=5, pady=5 )
   self.parent.balloon.bind( self.cbnLn1, "This X-axis type only works for the Isofill and Isoline\ngraphics methods." )
   self.cbnAwt1 = Tkinter.Checkbutton(fm, text = 'Area_wt', selectcolor=gui_color.one, command=self.cbnAwt1_cb)
   self.cbnAwt1.pack( side='left', fill = 'both', after=self.cbnLn1, padx=5, pady=5 )
###   self.cbn9 = Tkinter.Checkbutton(fm, text = 'Sine', selectcolor=gui_color.one, command=self.cbn9_cb)
###   self.cbn9.pack( side='left', fill = 'both', after=self.cbnAwt1, padx=5, pady=5 )
#   self.cbn10 = Tkinter.Checkbutton(fm, text = 'Exp', selectcolor=gui_color.one, command=self.cbn10_cb)
#   self.cbn10.pack( side='left', fill = 'both', after=self.cbn9, padx=5, pady=5 )
   self.cbn7.value=0
   self.cbn8.value=0
#   self.cbn9.value=0
#   self.cbn10.value=0
   if self.parent.x_axis_type == 'linear': 
      self.cbn7.invoke()
   elif self.parent.x_axis_type == 'log10':
      self.cbn8.invoke()
   elif self.parent.y_axis_type == 'ln':
      self.cbnLn1.invoke()
   elif self.parent.y_axis_type == 'area_wt':
      self.cbnAwt1.invoke()
#   elif self.parent.x_axis_type == 'sine':
#      self.cbn9.invoke()
#   elif self.parent.x_axis_type == 'exp':
#      self.cbn10.invoke()
   fm.pack( side='right', fill='both', expand=1 )
   #

   group = Pmw.Group(page, tag_text = 'Edit X-Axis')
   group.pack(fill = 'both', expand = 1, padx = 5, pady = 5)
   #
   fm=Tkinter.Frame(group.interior())
#   btn1=Tkinter.Button(fm,
#       text = 'Generate X-Tick Values',
#       background = gui_color.one
#       #command = self.set_replot,
#       )
#   btn1.pack( expand = 1, fill = 'y', pady=5 )
   self.eny1=Pmw.EntryField(fm,
       labelpos = 'w',
       label_text = 'X Major Values:',
       entry_background = 'white',
       entry_foreground = 'black',
       entry_width =  41,
       value = global_xticlabels,
       )
   self.eny1.pack( expand = 1, fill = 'both', padx=5, pady=5 )
   self.parent.balloon.bind( self.eny1, "Specify the desired X-axis major tick marks and labels.\nFor example:\nNone\n[ ] or { }\n[ 0, 10, 20 ]  or  { 0:'0', 10:'10', 20:'20' }\n[ 0, 10 ]  or  { 0:'text', 10:'more text' }" )
   self.eny1.clear( )
   try:
      self.eny1.setentry( global_xticlabels )
   except:
      pass
   self.eny2=Pmw.EntryField(fm,
       labelpos = 'w',
       label_text = 'X Minor Values:',
       entry_background = 'white',
       entry_foreground = 'black',
       entry_width =  41,
       )
   self.eny2.pack( expand = 1, fill = 'both', padx=5, pady=5 )
   self.parent.balloon.bind( self.eny2, "Specify the desired X-axis minor tick marks.\nFor example:\nNone\n[ ] or { }\n[ 0, 10, 20 ]  or  { 0:'0', 10:'10', 20:'20' }\n[ 0, 10 ]  or  { 0:'text', 10:'more text' }" )
   self.eny2.clear( )
   try:
      self.eny2.setentry( global_xmtics )
   except:
      pass
   fm.pack( side='right', fill='both', expand=1 )

#----------------------------------------------------------------------
# End the "X-Axis" page of the notebook
#----------------------------------------------------------------------

#----------------------------------------------------------------------
# Write the "Y-Axis" page to the notebook
#----------------------------------------------------------------------
def write_y_axis_page( self, page ):
   group = Pmw.Group(page, tag_text = 'View Y-Axis Labels and Ticks')
   group.pack(fill = 'both', expand = 1, padx = 5, pady = 5)
   #     1st - line in X-Axis Info group
   fm=Tkinter.Frame(group.interior())
   lbl1=Tkinter.Label(fm, text = "Y_Labels:", justify = 'left')
   lbl1.pack( side='left', fill = 'both', padx=5, pady=5 )
   self.cbn11 = Tkinter.Checkbutton(fm, text = 'Left', selectcolor=gui_color.one, command=self.cbn11_cb)
   self.cbn11.pack( side='left', fill = 'both', after=lbl1, padx=5, pady=5 )
   self.cbn12 = Tkinter.Checkbutton(fm, text = 'Right', selectcolor=gui_color.one, command=self.cbn12_cb)
   self.cbn12.pack( side='left', fill = 'both', after=self.cbn11, padx=5, pady=5 )
   self.cbn11.value=0
   self.cbn12.value=0
   if (self.parent.y_labels == 'left') or (self.parent.y_labels == 'both'):
       self.cbn11.invoke()
   if (self.parent.y_labels == 'right') or (self.parent.y_labels == 'both'):
       self.cbn12.invoke()
   fm.pack( side='top', fill='both', expand=1 )
   #
   fm=Tkinter.Frame(group.interior())
   lbl2=Tkinter.Label(fm, text = "Y_Major_Ticks:", justify = 'left')
   lbl2.pack( side='left', fill = 'both', padx=5, pady=5 )
   self.cbn13 = Tkinter.Checkbutton(fm, text = 'Left', selectcolor=gui_color.one, command=self.cbn13_cb)
   self.cbn13.pack( side='left', fill = 'both', after=lbl2, padx=5, pady=5 )
   self.cbn14 = Tkinter.Checkbutton(fm, text = 'Right', selectcolor=gui_color.one, command=self.cbn14_cb)
   self.cbn14.pack( side='left', fill = 'both', after=self.cbn13, padx=5, pady=5 )
   self.cbn13.value=0
   self.cbn14.value=0
   if (self.parent.y_major_ticks == 'left') or (self.parent.y_major_ticks == 'both'):
       self.cbn13.invoke()
   if (self.parent.y_major_ticks=='right') or (self.parent.y_major_ticks=='both'):
       self.cbn14.invoke()
   fm.pack( side='top', fill='both', expand=1 )
   #
   fm=Tkinter.Frame(group.interior())
   lbl3=Tkinter.Label(fm, text = "Y_Minor_Ticks:", justify = 'left')
   lbl3.pack( side='left', fill = 'both', padx=5, pady=5 )
   self.cbn15 = Tkinter.Checkbutton(fm, text = 'Left', selectcolor=gui_color.one, command=self.cbn15_cb)
   self.cbn15.pack( side='left', fill = 'both', after=lbl3, padx=5, pady=5 )
   self.cbn16 = Tkinter.Checkbutton(fm, text = 'Right', selectcolor=gui_color.one, command=self.cbn16_cb)
   self.cbn16.pack( side='left', fill = 'both', after=self.cbn15, padx=5, pady=5 )
   self.cbn15.value=0
   self.cbn16.value=0
   if (self.parent.y_minor_ticks == 'left') or (self.parent.y_minor_ticks == 'both'):
       self.cbn15.invoke()
   if (self.parent.y_minor_ticks=='right') or (self.parent.y_minor_ticks=='both'):
       self.cbn16.invoke()
   fm.pack( side='top', fill='both', expand=1 )

   group = Pmw.Group(page, tag_text = 'Y-Axis Type')
   group.pack(fill = 'both', expand = 1, padx = 5, pady = 5)
   #
   fm=Tkinter.Frame(group.interior())
   lbl4=Tkinter.Label(fm, text = "Y_Type:", justify = 'left')
   lbl4.pack( side='left', fill = 'both', padx=5, pady=5 )
   self.cbn17 = Tkinter.Checkbutton(fm, text = 'Linear', selectcolor=gui_color.one, command=self.cbn17_cb)
   self.cbn17.pack( side='left', fill = 'both', padx=5, pady=5 )
   self.cbn18 = Tkinter.Checkbutton(fm, text = 'Log10', selectcolor=gui_color.one, command=self.cbn18_cb)
   self.cbn18.pack( side='left', fill = 'both', after=self.cbn17, padx=5, pady=5 )
   self.parent.balloon.bind( self.cbn18, "This Y-axis type only works for the Isofill and Isoline\ngraphics methods." )
   self.cbnLn2 = Tkinter.Checkbutton(fm, text = 'Ln', selectcolor=gui_color.one, command=self.cbnLn2_cb)
   self.cbnLn2.pack( side='left', fill = 'both', after=self.cbn18, padx=5, pady=5 )
   self.parent.balloon.bind( self.cbnLn2, "This Y-axis type only works for the Isofill and Isoline\ngraphics methods." )
   self.cbnAwt2 = Tkinter.Checkbutton(fm, text = 'Area_wt', selectcolor=gui_color.one, command=self.cbnAwt2_cb)
   self.cbnAwt2.pack( side='left', fill = 'both', after=self.cbnLn2, padx=5, pady=5 )
###   self.cbn19 = Tkinter.Checkbutton(fm, text = 'Sine', selectcolor=gui_color.one, command=self.cbn19_cb)
###   self.cbn19.pack( side='left', fill = 'both', after=self.cbnAwt2, padx=5, pady=5 )
#   self.cbn20 = Tkinter.Checkbutton(fm, text = 'Exp', selectcolor=gui_color.one, command=self.cbn20_cb)
#   self.cbn20.pack( side='left', fill = 'both', after=self.cbn19, padx=5, pady=5 )
   self.cbn17.value=0
   self.cbn18.value=0
#   self.cbn19.value=0
#   self.cbn20.value=0
   if self.parent.y_axis_type == 'linear': 
      self.cbn17.invoke()
   elif self.parent.y_axis_type == 'log10':
      self.cbn18.invoke()
   elif self.parent.y_axis_type == 'ln':
      self.cbnLn2.invoke()
   elif self.parent.y_axis_type == 'area_wt':
      self.cbnAwt2.invoke()
#   elif self.parent.y_axis_type == 'sine':
#      self.cbn19.invoke()
#   elif self.parent.y_axis_type == 'exp':
#      self.cbn20.invoke()
   fm.pack( side='right', fill='both', expand=1 )
   #

   group = Pmw.Group(page, tag_text = 'Edit Y-Axis')
   group.pack(fill = 'both', expand = 1, padx = 5, pady = 5)
   #
   fm=Tkinter.Frame(group.interior())
#   btn1=Tkinter.Button(fm,
#       text = 'Generate Y-Tick Values',
#       background = gui_color.one
#       #command = self.set_replot,
#       )
#   btn1.pack( expand = 1, fill = 'y', pady=5 )
   self.eny3=Pmw.EntryField(fm,
       labelpos = 'w',
       label_text = 'Y Major Values:',
       entry_background = 'white',
       entry_foreground = 'black',
       entry_width =  41,
       )
   self.eny3.pack( expand = 1, fill = 'both', padx=5, pady=5 )
   self.parent.balloon.bind( self.eny3, "Specify the desired Y-axis major tick marks and labels.\nFor example:\nNone\n[ ] or { }\n[ 0, 10, 20 ]  or  { 0:'0', 10:'10', 20:'20' }\n[ 0, 10 ]  or  { 0:'text', 10:'more text' }" )
   self.eny3.clear( )
   try:
      self.eny3.setentry( global_yticlabels )
   except:
      pass
   self.eny4=Pmw.EntryField(fm,
       labelpos = 'w',
       label_text = 'Y Minor Values:',
       entry_background = 'white',
       entry_foreground = 'black',
       entry_width =  41,
       )
   self.eny4.pack( expand = 1, fill = 'both', padx=5, pady=5 )
   self.parent.balloon.bind( self.eny4, "Specify the desired Y-axis minor tick marks.\nFor example:\nNone\n[ ] or { }\n[ 0, 10, 20 ]  or  { 0:'0', 10:'10', 20:'20' }\n[ 0, 10 ]  or  { 0:'text', 10:'more text' }" )
   self.eny4.clear( )
   try:
      self.eny4.setentry( global_ymtics )
   except:
      pass
   fm.pack( side='right', fill='both', expand=1 )
#----------------------------------------------------------------------
# End the "Y-Axis" page to the notebook
#----------------------------------------------------------------------

#----------------------------------------------------------------------
# Write the "Legend" page to the notebook
#----------------------------------------------------------------------
def write_legend_page( self, page ):
   # Legend Status
   group = Pmw.Group(page, tag_text = 'Legend Status')
   group.pack(fill = 'x', expand = 1, padx = 5, pady = 5)
   fm=Tkinter.Frame(group.interior())
   lblstatus=Tkinter.Label(fm, text = "Status:", justify = 'left')
   lblstatus.pack( side='left', fill = 'both', padx=5, pady=5 )
   self.cbnstat_on = Tkinter.Checkbutton(fm, text = 'On', selectcolor=gui_color.one, command=self.cbnstat_on_cb)
   self.cbnstat_on.pack( side='left', fill = 'both', after=lblstatus, padx=5, pady=5 )
   self.cbnstat_off = Tkinter.Checkbutton(fm, text = 'Off', selectcolor=gui_color.one, command=self.cbnstat_off_cb)
   self.cbnstat_off.pack( side='left', fill = 'both', after=self.cbnstat_on, padx=5, pady=5 )
   self.cbnstat_on.value=0
   self.cbnstat_off.value=0
   if self.parent.legend_status == 'on':
      self.cbnstat_on.invoke()
   elif self.parent.legend_status == 'off':
      self.cbnstat_off.invoke()
   fm.pack( side='top', fill='both', expand=1 )
   self.parent.balloon.bind( fm, "Legend status is either on or off." )
   #
   # Legend Orientation
   group = Pmw.Group(page, tag_text = 'Legend Orientation')
   group.pack(fill = 'x', expand = 1, padx = 5, pady = 5)
   fm=Tkinter.Frame(group.interior())
   lbl5=Tkinter.Label(fm, text = "Orientation:", justify = 'left')
   lbl5.pack( side='left', fill = 'both', padx=5, pady=5 )
   self.cbn21 = Tkinter.Checkbutton(fm, text = 'Horizontal', selectcolor=gui_color.one, command=self.cbn21_cb)
   self.cbn21.pack( side='left', fill = 'both', after=lbl5, padx=5, pady=5 )
   self.cbn22 = Tkinter.Checkbutton(fm, text = 'Vertical', selectcolor=gui_color.one, command=self.cbn22_cb)
   self.cbn22.pack( side='left', fill = 'both', after=self.cbn21, padx=5, pady=5 )
   self.cbn21.value=0
   self.cbn22.value=0
   if self.parent.legend_orientation == 'horizontal':
      self.cbn21.invoke()
   elif self.parent.legend_orientation == 'vertical':
      self.cbn22.invoke()
   fm.pack( side='top', fill='both', expand=1 )
   self.parent.balloon.bind( fm, "Position the legend horizontally or vertically." )
   #
   # Legend Page
   group = Pmw.Group(page, tag_text = 'Legend Position')
   group.pack(fill = 'x', expand = 1, padx = 5, pady = 5)
   fm=Tkinter.Frame(group.interior())
   self.ctr1 = Pmw.Counter(fm,
		labelpos = 'w',
		label_text = 'X Position:',
                entry_background = 'white',
                entry_foreground = 'black',
                entryfield_entry_width = 4,
		entryfield_value = self.parent.legend_x_position,
	        #datatype = {'counter' : 'integer'},
	        entryfield_validate = {'validator' : 'integer',
	                           'min' : '-100', 'max' : '100'},
 	        increment = 1)
   self.ctr1.pack( side='left', fill = 'x', padx=5, pady=5 )
   self.parent.balloon.bind( self.ctr1, "Move the legend left or right along the X-axis.\nNegative values moves the legend to the left and\npositive values moves the legend to the right." )
   self.ctr2 = Pmw.Counter(fm,
                labelpos = 'w', 
                label_text = 'Y Position:',
                entry_background = 'white',
                entry_foreground = 'black',
                entryfield_entry_width = 4,
                entryfield_value = self.parent.legend_y_position,
                #datatype = {'counter' : 'integer'},
                entryfield_validate = {'validator' : 'integer',
                                   'min' : '-100', 'max' : '100'},
                increment = 1)
   self.ctr2.pack( side='left', fill = 'x', padx=5, pady=5 )
   self.parent.balloon.bind( self.ctr2, "Move the legend up or down along the Y-axis.\nNegative values moves the legend down and\npositive values moves the legend up." )

   fm.pack( side='top', fill='x', expand=1 )
   #
   # Legend Size
   group = Pmw.Group(page, tag_text = 'Legend Size')
   group.pack(fill = 'x', expand = 1, padx = 5, pady = 5)
   fm=Tkinter.Frame(group.interior())
   self.ctr3 = Pmw.Counter(fm,
                labelpos = 'w', 
                label_text = 'Width:',
                entry_background = 'white',
                entry_foreground = 'black',
                entryfield_entry_width = 4,
                entryfield_value = self.parent.legend_width,
                datatype = {'counter' : 'integer'},
                entryfield_validate = {'validator' : 'integer',
                                   'min' : '-100', 'max' : '100'},
                increment = 1)
   self.ctr3.pack( side='left', fill = 'x', padx=5, pady=5 )
   self.parent.balloon.bind( self.ctr3, "Make the legend wider or smaller along the X-axis. The\n0 value represents the default width." )
   self.ctr4 = Pmw.Counter(fm,
                labelpos = 'w', 
                label_text = 'Height:',
                entry_background = 'white',
                entry_foreground = 'black',
                entryfield_entry_width = 4,
                entryfield_value = self.parent.legend_height, 
                datatype = {'counter' : 'integer'},
                entryfield_validate = {'validator' : 'integer',
                                   'min' : '-100', 'max' : '100'},
                increment = 1)
   self.ctr4.pack( side='left', fill = 'x', padx=5, pady=5 )
   self.parent.balloon.bind( self.ctr4, "Make the legend higher or smaller along the Y-axis. The\n0 value represents the default height." )
   fm.pack( side='top', fill='x', expand=1 )
#----------------------------------------------------------------------
# End the "Legend" page of the notebook
#----------------------------------------------------------------------

#----------------------------------------------------------------------
# Write the "Shape" page of the notebook
#----------------------------------------------------------------------
def write_shape_page( self, page ):
   group = Pmw.Group(page, tag_text = 'Plot Position')
   group.pack(fill = 'x', expand = 1, padx = 5, pady = 5)
   fm=Tkinter.Frame(group.interior())
   self.ctr5 = Pmw.Counter(fm,
                labelpos = 'w', 
                label_text = 'X Position:',
                entry_background = 'white',
                entry_foreground = 'black',
                entryfield_entry_width = 4,
                entryfield_value = self.parent.plot_x_position,
                datatype = {'counter' : 'integer'},
                entryfield_validate = {'validator' : 'integer',
                                   'min' : '-100', 'max' : '100'},
                increment = 1)
   self.ctr5.pack( side='left', fill = 'x', padx=5, pady=5 )
   self.parent.balloon.bind( self.ctr5, "Move the plot along the X-axis. The 0 value represents\nthe center X position. Negative values will move the\nplot to the left and positive values will move the\nplot to the right." )   
   self.ctr6 = Pmw.Counter(fm,
                labelpos = 'w', 
                label_text = 'Y Position:',
                entry_background = 'white',
                entry_foreground = 'black',
                entryfield_entry_width = 4,
                entryfield_value = self.parent.plot_y_position,
                datatype = {'counter' : 'integer'},
                entryfield_validate = {'validator' : 'integer',
                                   'min' : '-100', 'max' : '100'},
                increment = 1)
   self.ctr6.pack( side='left', fill = 'x', padx=5, pady=5 )
   self.parent.balloon.bind( self.ctr6, "Move the plot along the Y-axis. The 0 value represents\nthe center Y position. Negative values will move the\nplot down and positive values will move the plot up." )
   fm.pack( side='top', fill='x', expand=1 )
   #
   group = Pmw.Group(page, tag_text = 'Plot Aspect (Width-to-Height) Ratio')
   group.pack(fill = 'x', expand = 1, padx = 5, pady = 5)
   fm=Tkinter.Frame(group.interior())
   if self.parent.plot_ratio[-1]=='t':
      val=self.parent.plot_ratio[:-1]
   else:
      val=self.parent.plot_ratio
   self.eny5=Pmw.EntryField(fm,
       labelpos = 'w',
       label_text = 'Plot Aspect Ratio:',
       entry_background = 'white',
       entry_foreground = 'black',
       value = val,
       entry_width =  41,
       )
   self.eny5.pack( expand = 1, fill = 'both', padx=5, pady=5 )
   self.parent.balloon.bind( self.eny5, "Specify the desired plot ratio.\n\nFor example:\n\t0 represents VCS's default plot ratio (i.e., turn ratio off)\n\tEnter an int or float value: 0.5, or 0.7, or 2, or 5, etc.\nOr 'auto' if you have a lat/lon plot and wish to preserve the aspect ratio as much as possible" )
   fm.pack( side='top', fill='x', expand=1 )
   #
   self.chkb5 = Pmw.RadioSelect(group.interior(),
                       labelpos = 'w',
                       label_text = 'Move Tick Marks and Boxes to match ratio:',
                       labelmargin = 75,
                       buttontype = 'radiobutton')
##                        command = gui_control.Command(self.chkb5_switch, parent) )
   self.parent.balloon.bind(self.chkb5, "If resizing the data area to fit your ratio, do you also want to move tickmarks, label and boxes ?")

   self.chkb5.add('yes')
   self.chkb5.add('no')
   if self.parent.plot_ratio[-1]=='t':
      self.chkb5.invoke( 'yes' )
   else:
      self.chkb5.invoke( 'no' )
      
   self.chkb5.pack(side = 'left', expand = 1, fill = 'x',padx = 10,pady = 0)
   group = Pmw.Group(page, tag_text = 'Plot Size')
   group.pack(fill = 'x', expand = 1, padx = 5, pady = 5)
   fm=Tkinter.Frame(group.interior())
   self.ctr7 = Pmw.Counter(fm,
                labelpos = 'w', 
                label_text = 'Width:',
                entry_background = 'white',
                entry_foreground = 'black',
                entryfield_entry_width = 4,
                entryfield_value = self.parent.plot_width,
                datatype = {'counter' : 'integer'},
                entryfield_validate = {'validator' : 'integer',
                                   'min' : '-100', 'max' : '100'},
                increment = 1)
   self.ctr7.pack( side='left', fill = 'x', padx=5, pady=5 )
   self.parent.balloon.bind( self.ctr7, "Make the plot wider or smaller along the X-axis. The\n0 value represents the default width. Note, this\nparameter only works if the 'Plot Ratio' above is\nset to 0." )
   self.ctr8 = Pmw.Counter(fm,
                labelpos = 'w', 
                label_text = 'Height:',
                entry_background = 'white',
                entry_foreground = 'black',
                entryfield_entry_width = 4,
                entryfield_value = self.parent.plot_height,
                datatype = {'counter' : 'integer'},
                entryfield_validate = {'validator' : 'integer',
                                   'min' : '-100', 'max' : '100'},
                increment = 1)
   self.ctr8.pack( side='left', fill = 'x', padx=5, pady=5 )
   self.parent.balloon.bind( self.ctr8, "Make the plot higher or smaller along the Y-axis. The\n0 value represents the default height. Note, this\nparameter only works if the 'Plot Ratio' above is\nset to 0." )
   fm.pack( side='top', fill='x', expand=1 )
#----------------------------------------------------------------------
# End the "Shape" page of the notebook
#----------------------------------------------------------------------

#----------------------------------------------------------------------------
# Set the graphics methods x-axis according to the "alter plot" popup controls
#----------------------------------------------------------------------------
def generate_x_axis( parent, graphics_method, replot_flg ):
    global global_xticlabels, global_xmtics

    graphics_method.xticlabels1 = global_xticlabels
    graphics_method.xticlabels2 = global_xticlabels
    graphics_method.xmtics1 = global_xmtics
    graphics_method.xmtics2 = global_xmtics

#   if graphics_method.xaxisconvert == 'log10':
#      major_log_axis, minor_log_axis = generate_log_values()
#      #
#      graphics_method.xticlabels1 = major_log_axis
#      graphics_method.xmtics1 = minor_log_axis
#      graphics_method.xticlabels2=major_log_axis
#      graphics_method.xmtics2 = minor_log_axis
#   elif replot_flg != 3:
#      graphics_method.xticlabels1 = '*'
#      graphics_method.xmtics1 = '*'
#      graphics_method.xticlabels2 = '*'
#      graphics_method.xmtics2 = '*'
#   else:
#      major_str = parent.alter_notebook.eny1.get()
#      minor_str = parent.alter_notebook.eny2.get()
#      graphics_method.xticlabels1 = '*'
#      if (major_str != '*') and (major_str != ''):
#         s=eval(major_str)
#         graphics_method.xticlabels1 = s
#      graphics_method.xmtics1 = ''
#      if (minor_str != '*') and (minor_str != ''):
#         s=eval(minor_str)
#         graphics_method.xmtics1 = s
#      graphics_method.xticlabels2 = '*'
#      graphics_method.xmtics2 = ''

#----------------------------------------------------------------------------
# Set the graphics methods y-axis according to the "alter plot" popup controls
#----------------------------------------------------------------------------
def generate_y_axis( parent, graphics_method, replot_flg ):
    global global_yticlabels, global_ymtics

    graphics_method.yticlabels1 = global_yticlabels
    graphics_method.ymtics1 = global_ymtics
    graphics_method.yticlabels2 = global_yticlabels
    graphics_method.ymtics2 = global_ymtics

#   if graphics_method.yaxisconvert == 'log10':
#      major_log_axis={}
#      minor_log_axis={}
#      a=10000.0
#      b=a/10.0
#      for j in range(8):
#       for i in range (a/b):
#        log_num = numpy.log10(a - (i*b))
#        minor_log_axis[log_num] = str(a - (i*b))
#        if ( log_num - int(log_num) ) == 0:
#         major_log_axis[log_num] = str(a - (i*b))
#       a=b
#       b=a/10.0
#      #
#      graphics_method.yticlabels1 = major_log_axis
#      graphics_method.ymtics1 = minor_log_axis
#      graphics_method.yticlabels2=major_log_axis
#      graphics_method.ymtics2 = minor_log_axis
#   elif replot_flg != 3:
#      graphics_method.yticlabels1 = '*'
#      graphics_method.ymtics1 = '*'
#      graphics_method.yticlabels2 = '*'
#      graphics_method.ymtics2 = '*'
#   else:
#      major_str = parent.alter_notebook.eny3.get()
#      minor_str = parent.alter_notebook.eny4.get()
#      graphics_method.yticlabels1 = '*'
#      if (major_str != '*') and (major_str != ''):
#         s=eval(major_str)
#         graphics_method.yticlabels1 = s
#      graphics_method.ymtics1 = ''
#      if (minor_str != '*') and (minor_str != ''):
#         s=eval(minor_str)
#         graphics_method.ymtics1 = s
#      graphics_method.yticlabels2 = '*'
#      graphics_method.ymtics2 = ''
#
#
#----------------------------------------------------------------------------
# Set the graphics methods according to the "alter plot" popup controls
#----------------------------------------------------------------------------
def settings( parent, replot_flg, g_name, template_name  ):
   if template_name != '':
      t = parent.vcs[ parent.vcs_id ].gettemplate( template_name )
      t2 = parent.vcs[ parent.vcs_id ].gettemplate( 'ASD_dud' )
      graphics_method = parent.graphics_method
      if template_name[-3:] == 'dud': return
   else:
      return
   #
   # Record Template
   gui_control.record_command( parent, "\n# Alter the Plot's Appearance" )
   gui_control.record_command( parent, "templ = vcs_canvas_list[ %d ].gettemplate('%s')" % (parent.vcs_id, parent.template_name) )

   #
   # Get the x-axis settings
   #
   t.xlabel1.priority = 0
   t.xlabel2.priority = 0
   if (parent.x_labels == 'both') or (parent.x_labels == 'top'): t.xlabel2.priority = 1
   if (parent.x_labels == 'both') or (parent.x_labels == 'bottom'): t.xlabel1.priority = 1
   gui_control.record_command( parent, "templ.xlabel1.priority=%d" % t.xlabel1.priority )
   gui_control.record_command( parent, "templ.xlabel2.priority=%d" % t.xlabel2.priority )

   t.xtic1.priority = 0
   t.xtic2.priority = 0
   if (parent.x_major_ticks == 'both') or (parent.x_major_ticks == 'top'): t.xtic2.priority = 1
   if (parent.x_major_ticks == 'both') or (parent.x_major_ticks == 'bottom'): t.xtic1.priority = 1
   gui_control.record_command( parent, "templ.xtic1.priority=%d" % t.xtic1.priority )
   gui_control.record_command( parent, "templ.xtic2.priority=%d" % t.xtic2.priority )

   t.xmintic1.priority = 0
   t.xmintic2.priority = 0
   if (parent.x_minor_ticks == 'both') or (parent.x_minor_ticks == 'top'): t.xmintic2.priority = 1
   if (parent.x_minor_ticks == 'both') or (parent.x_minor_ticks == 'bottom'): t.xmintic1.priority = 1
   gui_control.record_command( parent, "templ.xmintic1.priority=%d" % t.xmintic1.priority )
   gui_control.record_command( parent, "templ.xmintic2.priority=%d" % t.xmintic2.priority )

   if (graphics_method.g_name not in [ None,'Gtd','Gcon','GXy']) and (g_name != "default"):
      if parent.x_axis_type == 'sine': 
         graphics_method.xaxisconvert = 'linear'
      else:
         graphics_method.xaxisconvert = parent.x_axis_type
      generate_x_axis( parent, graphics_method, replot_flg )

   #
   # Get the y-axis settings
   #
   t.ylabel1.priority = 0
   t.ylabel2.priority = 0
   if (parent.y_labels == 'both') or (parent.y_labels == 'left'): t.ylabel1.priority = 1
   if (parent.y_labels == 'both') or (parent.y_labels == 'right'): t.ylabel2.priority = 1
   gui_control.record_command( parent, "templ.ylabel1.priority=%d" % t.ylabel1.priority )
   gui_control.record_command( parent, "templ.ylabel2.priority=%d" % t.ylabel2.priority )

   t.ytic1.priority = 0
   t.ytic2.priority = 0
   if (parent.y_major_ticks == 'both') or (parent.y_major_ticks == 'left'): t.ytic1.priority = 1
   if (parent.y_major_ticks == 'both') or (parent.y_major_ticks == 'right'): t.ytic2.priority = 1
   gui_control.record_command( parent, "templ.ytic1.priority=%d" % t.ytic1.priority )
   gui_control.record_command( parent, "templ.ytic2.priority=%d" % t.ytic2.priority )

   t.ymintic1.priority = 0
   t.ymintic2.priority = 0
   if (parent.y_minor_ticks == 'both') or (parent.y_minor_ticks == 'left'): t.ymintic1.priority = 1
   if (parent.y_minor_ticks == 'both') or (parent.y_minor_ticks == 'right'): t.ymintic2.priority = 1
   gui_control.record_command( parent, "templ.ymintic1.priority=%d" % t.ymintic1.priority )
   gui_control.record_command( parent, "templ.ymintic2.priority=%d" % t.ymintic2.priority )

   # If multiple plots are selected for the canvas, then return and let the
   # vcs_function.set_template take over to set where the data is to be displayed.
   if parent.vcs_canvas_plot_number > 1:
      return

   if (graphics_method.g_name not in ['Gtd','Gcon','GYx']) and (g_name != "default"):
      if parent.y_axis_type == 'sine':
         graphics_method.yaxisconvert = 'linear'
      else:
         graphics_method.yaxisconvert = parent.y_axis_type
      generate_y_axis( parent, graphics_method, replot_flg )

   if (graphics_method.g_name not in ['GXy', 'GYx','Gtd','Gcon']) and (g_name != "default"):
      #
      # Get the legend settings
      #
      if parent.legend_status == 'on': t.legend.priority = 1
      else: t.legend.priority = 0

      x_pos = float( parent.legend_x_position ) / 100.0
      y_pos = float( parent.legend_y_position ) / 100.0
      width = float( parent.legend_width ) / 100.0
      height = float( parent.legend_height ) / 100.0
      if parent.legend_orientation == 'vertical':
         t.legend.x1=(0.925-width*0.5) + x_pos
         t.legend.x2=(0.955+width*0.5) + x_pos
         t.legend.y1=(0.15-height*0.5) + y_pos
         t.legend.y2=(0.85+height*0.5) + y_pos
      else:
         t.legend.x1=(0.075-width*0.5) + x_pos
         t.legend.x2=(0.927+width*0.5) + x_pos
         t.legend.y1=(0.14-height*0.5) + y_pos
         t.legend.y2=(0.17+height*0.5) + y_pos
      gui_control.record_command( parent, "templ.legend.x1=%.17g" % t.legend.x1 )
      gui_control.record_command( parent, "templ.legend.x2=%.17g" % t.legend.x2 )
      gui_control.record_command( parent, "templ.legend.y1=%.17g" % t.legend.y1 )
      gui_control.record_command( parent, "templ.legend.y2=%.17g" % t.legend.y2 )
      #
      # comment4 is now used for the legend label. So alter according to the 
      # legend.
      t.comment4.y= (0.085-height*0.5) + y_pos
   
      #
      # Get the plot settings
      #
      x_pos = float( parent.plot_x_position ) / 100.0
      y_pos = float( parent.plot_y_position ) / 100.0
      width = float( parent.plot_width ) / 100.0
      height = float( parent.plot_height ) / 100.0
      t.data.x1 = t2.data.x1 = t.box1.x1 = (0.075-width*0.5) + x_pos
      if parent.plot_projection in ['linear', 'default']:
         t.data.x2 = t2.data.x2 = t.box1.x2 = (0.927+width*0.5) + x_pos

      t.data.y1 = t2.data.y1 = t.box1.y1 = (0.282-height*0.5) + y_pos
      t.data.y2 = t2.data.y2 = t.box1.y2 = (0.85+height*0.5) + y_pos
      gui_control.record_command( parent, "templ.data.x1=%.17g" % t.data.x1 )
      gui_control.record_command( parent, "templ.data.x2=%.17g" % t.data.x2 )
      gui_control.record_command( parent, "templ.data.y1=%.17g" % t.data.y1 )
      gui_control.record_command( parent, "templ.data.y2=%.17g" % t.data.y2 )
      #
      t.source.y= (0.94+height*0.5) + y_pos
      t.dataname.y= (0.91+height*0.5) + y_pos
      t.title.y= (0.91+height*0.5) + y_pos
      t.units.y= (0.91+height*0.5) + y_pos
      t.comment1.y= (0.97+height*0.5) + y_pos
      t.comment1.x = t.comment2.x+.1
      t.comment2.y= (0.97+height*0.5) + y_pos
      t.comment3.y= (0.91+height*0.5) + y_pos
      t.crdate.y= (0.91+height*0.5) + y_pos
      t.crtime.y= (0.91+height*0.5) + y_pos
      t.mean.y= (0.88+height*0.5) + y_pos
      t.max.y = (0.88+height*0.5) + y_pos
      t.min.y = (0.88+height*0.5) + y_pos
      #
      gui_control.record_command( parent, "templ.source.y=%.17g" % t.source.y )
      gui_control.record_command( parent, "templ.dataname.y=%.17g" % t.dataname.y )
      gui_control.record_command( parent, "templ.title.y=%.17g" % t.title.y )
      gui_control.record_command( parent, "templ.units.y=%.17g" % t.units.y )
      gui_control.record_command( parent, "templ.comment1.y=%.17g" % t.comment1.y )
      gui_control.record_command( parent, "templ.comment2.y=%.17g" % t.comment2.y )
      gui_control.record_command( parent, "templ.comment3.y=%.17g" % t.comment3.y )
      gui_control.record_command( parent, "templ.comment4.y=%.17g" % t.comment4.y )
      gui_control.record_command( parent, "templ.crdate.y=%.17g" % t.crdate.y )
      gui_control.record_command( parent, "templ.crtime.y=%.17g" % t.crtime.y )
   
      t.xlabel1.y = t.data.y1 - 0.02
      t.ylabel1.x = t.data.x1 - 0.011
      t.xlabel2.y = t.data.y2 + 0.02
      t.ylabel2.x = t.data.x2 + 0.011
      gui_control.record_command( parent, "templ.xlabel1.y=%.17g" % t.xlabel1.y )
      gui_control.record_command( parent, "templ.ylabel1.x=%.17g" % t.ylabel1.x )
      gui_control.record_command( parent, "templ.xlabel2.y=%.17g" % t.xlabel2.y )
      gui_control.record_command( parent, "templ.ylabel2.x=%.17g" % t.ylabel2.x )
   
      if parent.plot_projection in ['linear', 'default']:
         t.xtic1.y1 = t.data.y1
         t.xtic1.y2 = t.data.y1 - 0.012
         t.ytic1.x1 = t.data.x1
         t.ytic1.x2 = t.data.x1 - 0.01
      t.xtic2.y1 = t.data.y2
      t.xtic2.y2 = t.data.y2 + 0.012
      t.ytic2.x1 = t.data.x2
      t.ytic2.x2 = t.data.x2 + 0.01
      gui_control.record_command( parent, "templ.xtic1.y1=%.17g" % t.xtic1.y1 )
      gui_control.record_command( parent, "templ.xtic1.y2=%.17g" % t.xtic1.y2 )
      gui_control.record_command( parent, "templ.ytic1.x1=%.17g" % t.ytic1.x1 )
      gui_control.record_command( parent, "templ.ytic1.x2=%.17g" % t.ytic1.x2 )
      gui_control.record_command( parent, "templ.xtic2.y1=%.17g" % t.xtic2.y1 )
      gui_control.record_command( parent, "templ.xtic2.y2=%.17g" % t.xtic2.y2 )
      gui_control.record_command( parent, "templ.ytic2.x1=%.17g" % t.ytic2.x1 )
      gui_control.record_command( parent, "templ.ytic2.x2=%.17g" % t.ytic2.x2 )
   
      t.xname.x = t.data.x1 + ((t.data.x2 - t.data.x1)*0.51)
      t.xname.y = t.data.y1 - 0.05
      t.yname.x = t.data.x1 - 0.065
      t.yname.y = t.data.y1 + ((t.data.y2 - t.data.y1)*0.5)
      gui_control.record_command( parent, "templ.xname.x=%.17g" % t.xname.x )
      gui_control.record_command( parent, "templ.xname.y=%.17g" % t.xname.y )
      gui_control.record_command( parent, "templ.yname.x=%.17g" % t.yname.x )
      gui_control.record_command( parent, "templ.yname.y=%.17g" % t.yname.y )

#---------------------------------------------------------------------
# End of File
#---------------------------------------------------------------------






