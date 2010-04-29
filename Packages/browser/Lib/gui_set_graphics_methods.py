#!/usr/bin/env python
#
# The PCMDI Data Browser Contour Levels Popup - gui_set_graphics_methods module
#
###############################################################################
#                                                                             #
# Module:       gui_set_graphics_methods module                               #
#                                                                             #
# Copyright:    "See file Legal.htm for copyright information."               #
#                                                                             #
# Authors:      PCMDI Software Team                                           #
#               Lawrence Livermore NationalLaboratory:                        #
#               support@pcmdi.llnl.gov                                        #
#                                                                             #
# Description:  PCMDI Software System browser to set graphics method          #
#               attributes popup.                                             #
#                                                                             #
# Version:      4.0                                                           #
#                                                                             #
###############################################################################
#
#---------------------------------------------------------------------
# NOTE: need to use version of Python that imports Tkinter and Pmw
#---------------------------------------------------------------------
import Tkinter, Pmw
from tkMessageBox import showerror
import os, sys, string, cdtime
import gui_menu
import gui_functions
from gui_support import gui_color
import gui_control
import gui_message
## import vcs_function
import types
from gui_taylor import TDGui


#---------------------------------------------------------------------
#
# Start of Popup Dialog
#
#---------------------------------------------------------------------------
# VCS User Define Contour Levels Popup
#---------------------------------------------------------------------------
#
class note_book:
   def __init__(self, parent):
      self.parent = parent
      self.parent.alter_notebook = self
      self.preview_flg = 0
      self.dialog = Pmw.Dialog( parent,
          title = 'Graphics Methods Attribute Settings',
          buttons=('Preview', 'Reset', 'Apply', 'Cancel'),
          defaultbutton = 'Apply',
          command = gui_control.Command(self.execute, parent)
          )

      self.dialog.withdraw()                  # this is way too slow

      if parent.menu.popup_window_settings_flg == 1:
         self.dialog.transient( self.parent ) # Keep widget on top of its parent

      # Create and pack the NoteBook
      self.notebook = notebook = Pmw.NoteBook( self.dialog.interior() )
      notebook.pack(fill = 'both', expand = 1, padx = 5, pady = 5)

      #----------------------------------------------------------------------
      # Add the "Boxfill Settings" page to the notebook.
      #----------------------------------------------------------------------
      page = notebook.add('Boxfill')
      notebook.tab('Boxfill').focus_set()
      parent.balloon.bind( notebook.tab('Boxfill'), 'The Boxfill graphics method displays a two-dimensional data\narray by surrounding each data value with a colored grid box.' )
      self.boxfill_attributes = boxfill_attributes( page, parent, gm=parent.vcs[ parent.vcs_id].getboxfill('ASD') )

      #----------------------------------------------------------------------
      # Add the "Continents Settings" page to the notebook.
      #----------------------------------------------------------------------
      page = notebook.add('Continents')
      parent.balloon.bind( notebook.tab('Continents'), "The Continents graphics method draws a predefined,\ngeneric set of continental outlines in a longitude\nby latitude space. (To draw continental outlines,\nno external data set is required.)")
      self.continents_attributes = continents_attributes( page, parent)

      #----------------------------------------------------------------------
      # Add the "Contour Levels" page to the notebook.
      #----------------------------------------------------------------------
      page = notebook.add('Contour')
      parent.balloon.bind( notebook.tab('Contour'), "This Contour notebook tab represent both the Isofill\nand Isoline graphics methods. The Isofill graphics\nmethod fills the area between selected isolevels\n(levels of constant value) of a two-dimensional\narray; the manner of filling the area is determined\nby the named fill area attributes. The Isoline\ngraphics method draws lines of constant value at\nspecified levels to graphically represent the values\nof a two-dimensional array; labels also can be\ndisplayed on the isolines.\nIsolines can also have \"orientation\" arrows, indicating clockwise or counter-clockwise")
      self.contour_levels = contour_levels( page, parent)

      #----------------------------------------------------------------------
      # Add the "Meshfill Settings" page to the notebook.
      #----------------------------------------------------------------------
      page = notebook.add('Meshfill')
      parent.balloon.bind( notebook.tab('Meshfill'), "The Meshfill graphics method draws data on irregular grid (or 'mesh')at specified levels to graphically represent\nthe values of a one-dimensional array;\nUnless the irregular grid is supported by cdms2, a mesh array must be passed as well")
      self.meshfill_attributes = meshfill_attributes( page, parent )

      #----------------------------------------------------------------------
      # Add the "1D Plot Settings" page to the notebook.
      #----------------------------------------------------------------------
      page = notebook.add('1D Plot')
      parent.balloon.bind( notebook.tab('1D Plot'), "This 1D Plot notebook tab represent the XvsY, Xyvsy,\nand Yxvsx graphics methods. The XvsY graphics method\ndisplays a line plot from two 1D data arrays, that\nis X(t) and Y(t), where t represents the 1D\ncoordinate values. The Xyvsy graphics method displays\na line plot from a 1D data array, that is X(y),\nwhere y represents the 1D coordinate values. The\nYxvsx graphics method displays a line plot from\na 1D data array, that is Y(x), where y represents\nthe 1D coordinate values.")
      # Setup the list of the graphic methods to reset....
      self.oneD_attributes = oneD_attributes( page, parent)

      #----------------------------------------------------------------------
      # Add the "Outfill Levels" page to the notebook.
      #----------------------------------------------------------------------
      page = notebook.add('Outfill')
      parent.balloon.bind( notebook.tab('Outfill'), "The Outfill graphics method fills a set of integer\nvalues in any data array. Its primary purpose is\nto display continents by filling their area as\ndefined by a surface type array that indicates land,\nocean, and sea-ice points. ")
      self.outfill_attributes = outfill_attributes( page, parent )

      #----------------------------------------------------------------------
      # Add the "Outline Levels" page to the notebook.
      #----------------------------------------------------------------------
      page = notebook.add('Outline')
      parent.balloon.bind( notebook.tab('Outline'), "The Outline graphics method outlines a set of integer\nvalues in any data array. Its primary purpose is\nto display continental outlines as defined by a\nsurface type array that indicates land, ocean, and\nsea-ice points.")
      self.outline_attributes = outline_attributes( page, parent )


      #----------------------------------------------------------------------
      # Add the "Scatter Settings" page to the notebook.
      #----------------------------------------------------------------------
      page = notebook.add('Scatter')
      parent.balloon.bind( notebook.tab('Scatter'), "The Scatter graphics method displays a scatter plot\nof two 4-dimensional data arrays, e.g. A(x,y,z,t)\nand B(x,y,z,t). ")
      self.scatter_attributes = scatter_attributes( page, parent)

      #----------------------------------------------------------------------
      # Add the "Taylor Settings" page to the notebook.
      #----------------------------------------------------------------------
      page = notebook.add('Taylor\nDiagram')
      parent.balloon.bind( notebook.tab('Taylor\nDiagram'), "The Taylor diagram graphics method provides a statistical\nsummary of pattern correspondence. A single point on\nthe diagram indicates how similar two patterns are in\nterms of their correlation, root-mean-square (RMS)\ndifference, and the ratio of their variances.  The\nstandard deviation of a pattern is proportional to the\nradial distance.  The correlation is given by the cosine\nof the azimuthal angle. The RMS difference is proportional\nto the distance between the plotted points and the\nreference point (often chosen to be the observed\npattern), which is located along the abscissa at a radial\ndistance proportional to its standard deviation.")
      self.taylor_attributes = taylor_attributes( page, parent, vcs=parent.vcs[ parent.vcs_id], gm_name=parent.graphics_method_name )

      #----------------------------------------------------------------------
      # Add the "Vector Settings" page to the notebook.
      #----------------------------------------------------------------------
      page = notebook.add('Vector')
      parent.balloon.bind( notebook.tab('Vector'), "The Vector graphics method displays a vector plot\nof a 2D vector field. Vectors are located at the\ncoordinate locations and point in the direction of\nthe data vector field. Vector magnitudes are the\nproduct of data vector field lengths and a scaling\nfactor. ")
      self.vector_attributes = vector_attributes( page, parent)

      # Hold the original value settings for the Graphics Methods Attribute Settings ONLY. 
      # The other holdings can be found in the graphicsmethoddui.py file, which is used for all other
      # popup GUIs. This is needed for Reset and Cancel
      self.hold_gm_original_attr_settings( )

      # Position dialog popup
      parent_geom = parent.geometry()
      geom = string.split(parent_geom, '+')
      d1 = string.atoi( geom[1] )
      d2 = string.atoi( geom[2] )
      self.dialog.geometry( "650x640+%d+%d" % (d1, d2) )
      self.dialog.show()
      notebook.setnaturalsize()

   def execute(self, parent, result):
      notebook_page =  self.notebook.getcurselection()
      graphics_method = parent.panelGC.text_opt2.get()
      if (graphics_method in ['Isofill', 'Isoline']): graphics_method = 'Contour'
      elif (graphics_method in ['Xyvsy', 'Yxvsx', 'XvsY']): graphics_method = '1D Plot'

      if result == 'Preview':
         self.preview_flg = 1
         self.apply( parent, notebook_page, graphics_method )
      elif result == 'Reset':
         self.preview_flg = 0
         self.reset_gms_to_original_settings( parent, notebook_page, graphics_method )
      elif result == 'Clear':
         self.preview_flg = 0
      elif result == 'Apply':
         self.preview_flg = 0
         self.apply( parent, notebook_page, graphics_method )
         self.persistant_set( )
         self.dialog.destroy()
      elif (result == 'Cancel') or (result == None):
         if self.preview_flg: self.reset_gms_to_original_settings( parent, notebook_page, graphics_method )
         self.dialog.destroy()

   def hold_gm_original_attr_settings ( self ):
       # Store current Boxfill settings
       self.store_boxfill_type= self.boxfill_attributes.gen_type.getcurselection( )
       self.store_boxfill_missing = self.parent.boxfill_missing
       self.store_boxfill_ext1 = self.boxfill_attributes.ext1_toggle.getcurselection( )
       self.store_boxfill_ext2 = self.boxfill_attributes.ext2_toggle.getcurselection( )
       self.store_boxfill_legend = self.parent.boxfill_legend
       self.store_boxfill_level1 = self.parent.boxfill_level1
       self.store_boxfill_level2 = self.parent.boxfill_level2
       self.store_boxfill_color1 = self.parent.boxfill_color1
       self.store_boxfill_color2 = self.parent.boxfill_color2
       self.store_boxfill_include_zero    = self.boxfill_attributes.contour_levels.include_zero.getcurselection( )
       self.store_boxfill_gen_type        = self.boxfill_attributes.contour_levels.gen_type.getcurselection( )

       # Store current continents settings
       self.store_cont_line               = self.continents_attributes.var1.get( )
       self.store_cont_linecolor          = self.continents_attributes.ctr1.get( )
       self.store_cont_linewidth          = self.continents_attributes.ctr3.get( )
       self.store_cont_type               = self.continents_attributes.ctr2.get( )

       # Store current contour settings
       self.store_contour_include_zero    = self.contour_levels.include_zero.getcurselection( )
       self.store_contour_iso_ranges      = self.parent.iso_ranges
       self.store_contour_iso_colors      = self.parent.iso_colors
       self.store_contour_iso_line_types  = self.parent.iso_line_types
       self.store_contour_iso_line_widths = self.parent.iso_line_widths
       self.store_contour_iso_legend      = self.parent.iso_legend
       self.store_contour_gen_type        = self.contour_levels.gen_type.getcurselection( )
       self.store_contour_iso_min         = self.parent.iso_min
       self.store_contour_iso_max         = self.parent.iso_max
       self.store_contour_iso_num         = self.parent.iso_num
       self.store_contour_iso_neg_val     = self.parent.iso_neg_val
       self.store_contour_iso_neg_dec     = self.parent.iso_neg_dec
       self.store_contour_iso_line_clockwise = self.parent.iso_line_clockwise
       self.store_contour_iso_line_arrow_scale = self.parent.iso_line_arrow_scale
       self.store_contour_iso_line_arrow_spacing = self.parent.iso_line_arrow_spacing
       self.store_contour_iso_line_arrow_angle = self.parent.iso_line_arrow_angle

       # Store current Meshfill settings
       self.store_meshfill_missing        = self.meshfill_attributes.eny6.get( )
       self.store_meshfill_x_wrap         = self.meshfill_attributes.eny1.get( )
       self.store_meshfill_y_wrap         = self.meshfill_attributes.eny2.get( )
       self.store_meshfill_show_mesh      = self.meshfill_attributes.mesh_toggle.getcurselection( )
       self.store_meshfill_ext1           = self.meshfill_attributes.ext1_toggle.getcurselection( )
       self.store_meshfill_ext2           = self.meshfill_attributes.ext2_toggle.getcurselection( )
       self.store_meshfill_legend         = self.meshfill_attributes.eny5.get( )

       # Store current OneD Plot settings
       self.store_oneD_ltypes             = self.oneD_attributes.eny1.get( )
       self.store_oneD_lcolors            = self.oneD_attributes.eny2.get( )
       self.store_oneD_lwidths            = self.oneD_attributes.eny3.get( )
       self.store_oneD_mtypes             = self.oneD_attributes.eny4.get( )
       self.store_oneD_mcolors            = self.oneD_attributes.eny5.get( )
       self.store_oneD_mwidths            = self.oneD_attributes.eny6.get( )

       # Store current Outfill settings
       self.store_outf_style              = self.outfill_attributes.var1.get( )
       self.store_outf_index              = self.outfill_attributes.ctr1.get( )
       self.store_outf_fillcolor          = self.outfill_attributes.eny2.get( )
       self.store_outf_outfill            = self.outfill_attributes.eny1.get( )

       # Store current Outline settings
       self.store_outl_line               = self.outline_attributes.var1.get( )
       self.store_outl_linecolor          = self.outline_attributes.eny2.get( )
       self.store_outl_outline            = self.outline_attributes.eny1.get( )

       # Store current Scatter settings
       self.store_scat_marker             = self.scatter_attributes.var1.get( )
       self.store_scat_markercolor        = self.scatter_attributes.ctr1.get( )
       self.store_scat_markersize         = self.scatter_attributes.ctr2.get( )

       # Store current Vector settings
       self.store_vec_line                = self.vector_attributes.var1.get( )
       self.store_vec_linecolor           = self.vector_attributes.ctr1.get( )
       self.store_vec_scale               = self.vector_attributes.ctr2.get( )
       self.store_vec_alignment           = self.vector_attributes.var2.get( )
       self.store_vec_type                = self.vector_attributes.var3.get( )
       self.store_vec_ref                 = self.vector_attributes.ctr3.get( )

   def reset_gms_to_original_settings ( self, parent, notebook_page, graphics_method ):
       if notebook_page == "Boxfill":
          self.boxfill_attributes.gen_type.invoke( self.store_boxfill_type )
          if self.store_boxfill_missing is not None:
             self.boxfill_attributes.eny6.setentry( repr( self.store_boxfill_missing ) )
          self.boxfill_attributes.ext1_toggle.invoke( self.store_boxfill_ext1 )
          self.boxfill_attributes.ext2_toggle.invoke( self.store_boxfill_ext2 )
          self.boxfill_attributes.eny5.setentry( repr( self.store_boxfill_legend ) )
          self.boxfill_attributes.eny1.setentry( repr( self.store_boxfill_level1 ) )
          self.boxfill_attributes.eny2.setentry( repr( self.store_boxfill_level2 ) )
          self.boxfill_attributes.eny3.setentry( repr( self.store_boxfill_color1 ) )
          self.boxfill_attributes.eny4.setentry( repr( self.store_boxfill_color2 ) )
          self.boxfill_attributes.contour_levels.include_zero.invoke( self.store_boxfill_include_zero )
          self.boxfill_attributes.contour_levels.gen_type.invoke( self.store_boxfill_gen_type )
       elif notebook_page == "Continents":
          self.continents_attributes.var1.set( self.store_cont_line )
          self.continents_attributes.ctr1.component('entryfield').setentry( self.store_cont_linecolor )
          self.continents_attributes.ctr3.component('entryfield').setentry( self.store_cont_linewidth )
          self.continents_attributes.ctr2.component('entryfield').setentry( self.store_cont_type )
       elif notebook_page == "Contour":
          # Reset the contor (i.e., isofill and isoline) fields
          self.contour_levels.include_zero.invoke( self.store_contour_include_zero )
          self.contour_levels.gen_type.invoke( self.store_contour_gen_type ) # Also clears the text fields
          if self.store_contour_iso_ranges is not None:
             self.contour_levels.eny1.setentry( repr( self.store_contour_iso_ranges ) )
          if self.store_contour_iso_colors is not None:
             self.contour_levels.eny2.setentry( repr( self.store_contour_iso_colors ) )
          if self.store_contour_iso_line_types is not None:
             self.contour_levels.eny8.setentry( repr( self.store_contour_iso_line_types ) )
          if self.store_contour_iso_line_widths is not None:
             self.contour_levels.eny9.setentry( repr( self.store_contour_iso_line_widths ) )
          if self.store_contour_iso_line_clockwise is not None:
             self.contour_levels.eny11.setentry( repr( self.store_contour_iso_line_clockwise ) )
          if self.store_contour_iso_line_arrow_scale is not None:
             self.contour_levels.eny12.setentry( repr( self.store_contour_iso_line_arrow_scale ) )
          if self.store_contour_iso_line_arrow_spacing is not None:
             self.contour_levels.eny13.setentry( repr( self.store_contour_iso_line_arrow_spacing ) )
          if self.store_contour_iso_line_arrow_angle is not None:
             self.contour_levels.eny14.setentry( repr( self.store_contour_iso_line_arrow_angle ) )
          self.contour_levels.eny10.setentry( repr( self.store_contour_iso_legend ) )
          if self.store_contour_iso_min is not None:
             self.contour_levels.eny3.setentry( repr( self.store_contour_iso_min ) )
          if self.store_contour_iso_max is not None:
             self.contour_levels.eny4.setentry( repr( self.store_contour_iso_max ) )
          if self.store_contour_iso_num is not None:
             self.contour_levels.eny5.setentry( repr( self.store_contour_iso_num ) )
          if self.store_contour_iso_neg_val is not None:
             self.contour_levels.eny6.setentry( repr( self.store_contour_iso_neg_val ) )
          if self.store_contour_iso_neg_dec is not None:
             self.contour_levels.eny7.setentry( repr( self.store_contour_iso_neg_dec ) )
       elif notebook_page == "Meshfill":
          self.meshfill_attributes.eny6.setentry( self.store_meshfill_missing )
          self.meshfill_attributes.eny1.setentry( self.store_meshfill_x_wrap )
          self.meshfill_attributes.eny2.setentry( self.store_meshfill_y_wrap )

          self.boxfill_attributes.gen_type.invoke( self.store_boxfill_type )

          self.meshfill_attributes.mesh_toggle.invoke( self.store_meshfill_show_mesh )
          self.meshfill_attributes.ext1_toggle.invoke( self.store_meshfill_ext1 )
          self.meshfill_attributes.ext2_toggle.invoke( self.store_meshfill_ext2 )
          self.meshfill_attributes.eny5.setentry( self.store_meshfill_legend )
       elif notebook_page == "1D Plot":
          self.oneD_attributes.eny1.setentry( self.store_oneD_ltypes )
          self.oneD_attributes.eny2.setentry( self.store_oneD_lcolors )
          self.oneD_attributes.eny3.setentry( self.store_oneD_lwidths )
          self.oneD_attributes.eny4.setentry( self.store_oneD_mtypes )
          self.oneD_attributes.eny5.setentry( self.store_oneD_mcolors )
          self.oneD_attributes.eny6.setentry( self.store_oneD_mwidths )
       elif notebook_page == "Outfill":
          self.outfill_attributes.var1.set( self.store_outf_style )
          self.outfill_attributes.ctr1.component('entryfield').setentry( self.store_outf_index )
          self.outfill_attributes.eny2.component('entryfield').setentry( self.store_outf_fillcolor )
          self.outfill_attributes.eny1.setentry( self.store_outf_outfill )
       elif notebook_page == "Outline":
          self.outline_attributes.var1.set( self.store_outl_line )
          self.outline_attributes.eny2.component('entryfield').setentry( self.store_outl_linecolor )
          self.outline_attributes.eny1.setentry( self.store_outl_outline )
       elif notebook_page == "Scatter":
          self.scatter_attributes.var1.set( self.store_scat_marker )
          self.scatter_attributes.ctr1.component('entryfield').setentry( self.store_scat_markercolor )
          self.scatter_attributes.ctr2.component('entryfield').setentry( self.store_scat_markersize )
       elif notebook_page == "Vector":
          self.vector_attributes.var1.set( self.store_vec_line )
          self.vector_attributes.ctr1.component('entryfield').setentry( self.store_vec_linecolor )
          self.vector_attributes.ctr2.component('entryfield').setentry( self.store_vec_scale )
          self.vector_attributes.var2.set( self.store_vec_alignment )
          self.vector_attributes.var3.set( self.store_vec_type )
          self.vector_attributes.ctr3.component('entryfield').setentry( self.store_vec_ref )

       # Plot the settings
       self.apply( parent, notebook_page, graphics_method )
      
   def apply( self, parent, notebook_page, graphics_method ):
      self.boxfill_attributes.get_settings()
      self.meshfill_attributes.get_settings()
      self.continents_attributes.get_settings()
      self.contour_levels.get_settings()
      self.oneD_attributes.get_settings()
      self.outfill_attributes.get_settings()
      self.outline_attributes.get_settings()
      self.scatter_attributes.get_settings()
      self.vector_attributes.get_settings()
      # This is needed so the plot won't redraw eight times. Only redraw the plot
      # one time.
      if (notebook_page == 'Boxfill') and (graphics_method == 'Boxfill'):
          self.boxfill_attributes.boxfill_replot( parent )
      elif (notebook_page == 'Meshfill') and (graphics_method == 'Meshfill'):
         self.meshfill_attributes.meshfill_replot( parent )
      elif (notebook_page == 'Continents'):
          self.continents_attributes.cont_replot( parent )
      elif (notebook_page == 'Contour') and (graphics_method == 'Contour'):
          self.contour_levels.contour_replot( parent )
      elif (notebook_page == '1D Plot') and (graphics_method == '1D Plot'):
          self.oneD_attributes.oneD_replot( parent )
      elif (notebook_page == 'Outfill') and (graphics_method == 'Outfill'):
          self.outfill_attributes.outf_replot( parent )
      elif (notebook_page == 'Outline') and (graphics_method == 'Outline'):
           self.outline_attributes.outl_replot( parent )
      elif (notebook_page == 'Scatter') and (graphics_method == 'Scatter'):
           self.scatter_attributes.scat_replot( parent )
#      elif (notebook_page == 'Taylor\nDiagram'): self.taylor_attributes._replot( parent )
      elif (notebook_page == 'Vector') and (graphics_method == 'Vector'):
           self.vector_attributes.vector_replot( parent )

   def persistant_set( self ):
      self.continents_attributes.persistant_set( )

class taylor_attributes(TDGui):
   def __init__(self,page, parent, vcs = None, gm_name='ASD'):
      # Perform the proper initialization of the TDGui base class.
      # Must pass it the notebook page and the VCS Canvas.
      TDGui.__init__( self, root = page, canvas = vcs, grname=gm_name )
   
def _get_text_str(entry_box):
      text_str = string.replace( entry_box, ' ', ',' )
      while string.find(text_str,',,',0) != -1: text_str=string.replace(text_str,',,',',')
      while string.find(text_str,'(',0) != -1: text_str=string.replace(text_str,'(','')
      while string.find(text_str,'[',0) != -1: text_str=string.replace(text_str,'[','')
      while string.find(text_str,')',0) != -1: text_str=string.replace(text_str,')','')
      while string.find(text_str,']',0) != -1: text_str=string.replace(text_str,']','')
      if ( (len(text_str) > 0) and (text_str[-1] == ',') ): text_str = text_str[:-1]
      text_str = string.split(text_str, ',')
      return text_str

def _get_list (entry_box):
    "Get an entry item that is supposed to be a list."
    prev = ''
    text = entry_box.get().strip()
    while prev!=text:
	prev = text
	text = text.replace( ': ', ':' )
        text = text.replace('{ ','{')
	text = text.replace(' }','}')
	text = text.replace(' :',':')

    text = string.replace( text, ' ', ',' )
    while string.find(text,',,',0) != -1: text=string.replace(text,',,',',')
    while string.find(text,'(',0) != -1: text=string.replace(text,'(','[')
    while string.find(text,')',0) != -1: text=string.replace(text,')',']')
    if ( (len(text) > 0) and (text[-1] == ',') ): text = text[:-1]
    if len(text) == 0: text = '[]'
    if text[0] not in ['[', '{']: text = '[' + text
    if text[-1] not in [']', '}']: text = text + ']'
    if string.lower(text[:5]) == '[none':
       text = None
    if not text: return None
    if string.find(text,':',0) != -1: text = '{' + text[1:-1] + '}'
    try:
         value = eval(text)
    except:
         raise ValueError, 'Bad item ' + text + ' in entry box.'
    if (isinstance(value, types.ListType)) or (isinstance(value, types.DictType)):
        return value
    else:
        return text.split()
     
def _get_list_as_legend(entry_box):
    "Get an entry item that is supposed to be a list and convert it to legend"
    text = string.replace( entry_box.get().strip(), ': ', ':' )
    text = string.replace( text, ' ', ',' )
    while string.find(text,',,',0) != -1: text=string.replace(text,',,',',')
    while string.find(text,'(',0) != -1: text=string.replace(text,'(','[')
    while string.find(text,')',0) != -1: text=string.replace(text,')',']')
    if ( (len(text) > 0) and (text[-1] == ',') ): text = text[:-1]
    if len(text) == 0: text = '[]'
    if text[0] not in ['[', '{']: text = '[' + text
    if text[-1] not in [']', '}']: text = text + ']'
    if string.lower(text[:5]) == '[none':
       text = None
    if not text: return None
    if string.find(text,':',0) != -1: text = '{' + text[1:-1] + '}'
    try:
         value = eval(text)
    except:
         raise ValueError, 'Bad item ' + text + ' in entry box.'
    if (isinstance(value, list)):
       result=vcs.mklabels(list)
    elif (isinstance(value, dict)):
       result = value
    else:
       result = text.split()
    return result

    
        
class boxfill_attributes:
   def __init__(self, page, parent, re_plot_flag = 1, gm=None):
      self.parent = parent
      if type(gm) in [types.ListType, types.TupleType, types.NoneType]:
         self.gm = gm
      else:
         self.gm=[gm,]

      # Too much on this page, so I needed to add a scroll window
      scl_Frame=Pmw.ScrolledFrame(page,
                                labelpos = 'n',
                                label_text = 'Boxfill Editor',
                                )
      scl_Frame.pack(fill = 'both', expand = 1,side='top')
      page=scl_Frame.interior()

      framea=Tkinter.Frame(page)
      framea.pack( expand = 1, fill = 'both' )
      lbl1=Tkinter.Label(framea,
          text = "Define boxfill attributes values:",
          justify = 'left',
          anchor = 'w',
          )
      lbl1.pack( side = 'left', expand = 1, fill = 'x', padx=10, pady=5 )

      # Create the Boxfill type choice option menu.
#      self.boxfill_var = Tkinter.StringVar()
#      self.boxfill_var.set('linear')
#      self.boxfill_type = Pmw.OptionMenu(framea,
#	      labelpos = 'w',
#	      label_text = 'Boxfill Type:',
#	      menubutton_textvariable = self.boxfill_var,
#	      items = ['linear', 'log10', 'custom'],
#	      menubutton_width = 10,
#              hull_borderwidth = 2,
#              hull_relief = 'ridge',
#      )
#      self.boxfill_type.pack(side = 'left', anchor = 'w', padx = 10, pady = 10)

      # Create and pack a RadioSelect widget, with radiobuttons.
      self.gen_type = Pmw.RadioSelect(framea,
              buttontype = 'radiobutton',
              orient = 'horizontal',
              labelpos = 'w',
              command = gui_control.Command(self.evt_boxfill_linear_log, parent),
              label_text = 'Boxfill type:',
              hull_borderwidth = 2,
              hull_relief = 'ridge',
      )
      self.gen_type.pack(side = 'left', expand = 1, padx = 10, pady = 4)

      # Add some buttons to the radiobutton RadioSelect.
      try:
         for text in ('linear', 'log10', 'custom'):
            self.gen_type.add(text)
         self.gen_type.invoke(self.parent.boxfill_type)
      except:
         self.gen_type.invoke('linear')

      gen=Pmw.Group(page,
                    tag_text='General Settings',
                    tag_font=('times',14,'bold'),
                    )
      gen.pack(fill = 'both', expand = 1,side='top')
      gen=gen.interior()
      frame = Tkinter.Frame(gen)
      frame.pack()
      self.eny6=Pmw.EntryField(frame,
          labelpos = 'w',
          label_text = 'Missing:',
          entry_background = 'white',
          entry_foreground = 'black',
          entry_width =  25,
          )
      self.eny6.pack( side='left',expand = 1, fill = 'x', padx=20, pady=5 )
      self.parent.balloon.bind( self.eny6, "Set the missing color index value. The colormap\nranges from 0 to 255, enter the desired color index value 0\nthrough 255." )

      # Create and pack a RadioSelect widget, with radiobuttons.
      self.ext1_toggle = Pmw.RadioSelect(frame,
              buttontype = 'radiobutton',
              orient = 'horizontal',
              labelpos = 'w',
              label_text = 'Ext 1:  ',
              hull_borderwidth = 2,
              hull_relief = 'ridge',
      )
      self.ext1_toggle.pack(side='left', expand = 1, fill = 'x', padx = 20, pady = 5)

      # Add some buttons to the radiobutton RadioSelect.
      for text in ('No', 'Yes'):
         self.ext1_toggle.add(text)
      self.ext1_toggle.invoke('No')


      # Create and pack a RadioSelect widget, with radiobuttons.
      self.ext2_toggle = Pmw.RadioSelect(frame,
              buttontype = 'radiobutton',
              orient = 'horizontal',
              labelpos = 'w',
              label_text = 'Ext 2:  ',
              hull_borderwidth = 2,
              hull_relief = 'ridge',
      )
      self.ext2_toggle.pack(side='left', expand = 1, fill = 'x', padx = 20, pady = 5)

      # Add some buttons to the radiobutton RadioSelect.
      for text in ('No', 'Yes'):
         self.ext2_toggle.add(text)
      self.ext2_toggle.invoke('No')

      self.eny5=Pmw.EntryField(gen,
          labelpos = 'w',
          label_text = 'Legend Labels:',
          entry_background = 'white',
          entry_foreground = 'black',
          entry_width =  20,
          )
      self.eny5.pack( expand = 1, fill = 'x', padx=20, pady=5 )
      self.parent.balloon.bind( self.eny5, "Specify the desired legend labels.\nFor example:\nNone -- Allow VCS to generate legend labels\n( ), or [ ], or { } -- No legend labels\n[ 0, 10, 20 ]  or  { 0:'0', 10:'10', 20:'20' }\n[ 0, 10 ]  or  { 0:'text', 10:'more text' }" )


      gen=Pmw.Group(page,
                    tag_text='Linear and Log Settings',
                    tag_font=('times',14,'bold'),
                    )
      gen.pack(fill = 'both', expand = 1,side='top',pady=20)
      gen=gen.interior()
      frame=Tkinter.Frame(gen)
      frame.pack()
      self.eny1=Pmw.EntryField(frame,
                               labelpos = 'w',
                               label_text = 'Level 1:',
                               entry_background = 'white',
                               entry_foreground = 'black',
                               entry_width =  25,
                               )
      self.eny1.pack( side='left',expand = 1, fill = 'x', padx=20, pady=5 )
      self.parent.balloon.bind( self.eny1, "The minimum data value. If level 1 is set to '1e+20',\nthen VCS will select the level." )

      self.eny2=Pmw.EntryField(frame,
                               labelpos = 'w',
                               label_text = 'Level 2:',
                               entry_background = 'white',
                               entry_foreground = 'black',
                               entry_width =  25,
                               )
      self.eny2.pack( side='left', expand = 1, fill = 'x', padx=20, pady=5 )
      self.parent.balloon.bind( self.eny2, "The maximum data value. If level 2 is set to '1e+20',\nthen VCS will select the level." )

      frame=Tkinter.Frame(gen)
      frame.pack()
      self.eny3=Pmw.EntryField(frame,
          labelpos = 'w',
          label_text = 'Color 1:',
          entry_background = 'white',
          entry_foreground = 'black',
          entry_width =  25,
          )
      self.eny3.pack( side='left',expand = 1, fill = 'x', padx=20, pady=5 )
      self.parent.balloon.bind( self.eny3, "The minimum color range index value. The colormap\nranges from 0 to 255, but only color indices 0\nthrough 239 can be changed." )
      self.eny4=Pmw.EntryField(frame,
          labelpos = 'w',
          label_text = 'Color 2:',
          entry_background = 'white',
          entry_foreground = 'black',
          entry_width =  25,
          )
      self.eny4.pack( side='left',expand = 1, fill = 'x', padx=20, pady=5 )
      self.parent.balloon.bind( self.eny4, "The maximum color range index value. The colormap\nranges from 0 to 255, but only color indices 0\nthrough 239 can be changed." )
      
      # Adds the contour part
      
      gen=Pmw.Group(page,
                    tag_text='Custom Settings',
                    tag_font=('times',14,'bold'),
                    )
      gen.pack(fill = 'both', expand = 1,side='top')
      gen=gen.interior()
      self.contour_levels=contour_levels( gen, parent, re_plot_flag=0, gm=gm, gm_name='Gfb' )
      
      # Set the boxfill values
      self.reset_boxf_attributes()

      entries = (self.eny1, self.eny2, self.eny3, self.eny4, self.eny6, self.eny5)
      Pmw.alignlabels(entries)

# No longer needed
#      btn2=Tkinter.Button(page,
#          text = 'Plot and Set',
#          background = gui_color.replot,
#          command = gui_control.Command(self.boxfill_replot, parent)
#          )
#      if (re_plot_flag == 1):
#         btn2.pack( expand = 1, fill = 'x', padx=170, pady=5 )

      btn3=Tkinter.Button(page,
          text = 'Reset Boxfill Attributes',
          background = gui_color.one,
          command = self.reset_boxf_attributes,
          )
#      btn3.pack( expand = 1, fill = 'x', padx=170, pady=5 )

   def evt_boxfill_linear_log( self, parent, tag ):
       try:
          self.parent.boxfill_type = tag
       except Exception,err:
##           print 'Error:',err
	  pass

   def get_settings( self ):
      self.contour_levels.get_settings()
      if not self.gm is None:
         for g in self.gm:
            g.boxfill_type = self.parent.boxfill_type
            try: g.level_1=eval(self.eny1.get( ))
            except: pass
            try: g.level_2=eval(self.eny2.get())
            except: pass
            try: g.color_1=eval(self.eny3.get())
            except: pass
            try: g.color_2=eval(self.eny4.get())
            except: pass
            g.missing=eval(self.eny6.get())
            g.legend =_get_list(self.eny5)
            g.ext_1='n'
            if self.ext1_toggle.getcurselection() == 'Yes':
               g.ext_1='y'
            g.ext_2='n'
            if self.ext2_toggle.getcurselection() == 'Yes':
               g.ext_2='y'

            if g.name == 'ASD':
               self.parent.boxfill_type = g.boxfill_type
               self.parent.boxfill_level1 = g.level_1
               self.parent.boxfill_level2 = g.level_2
               self.parent.boxfill_levels = g.levels
               self.parent.boxfill_color1 = g.color_1
               self.parent.boxfill_color2 = g.color_2
               self.parent.boxfill_levels = g.levels
               self.parent.boxfill_fillareacolors = g.fillareacolors
               self.parent.boxfill_missing = g.missing
               self.parent.boxfill_legend = g.legend
               self.parent.boxfill_ext1 = g.ext_1
               self.parent.boxfill_ext2 = g.ext_2
      else:
         try:
            self.parent.boxfill_type = self.gen_type.getcurselection()
            if self.eny1.get( ) == '':
               self.parent.boxfill_level1 = None
            else:
               self.parent.boxfill_level1 = eval(self.eny1.get( ))
            if self.eny2.get( ) == '':
               self.parent.boxfill_level2 = None
            else:
               self.parent.boxfill_level2 = eval(self.eny2.get( ))   
            if self.eny3.get( ) == '':
               self.parent.boxfill_color1 = None
            else:
               self.parent.boxfill_color1 = eval(self.eny3.get( ))
            if self.eny4.get( ) == '':
               self.parent.boxfill_color2 = None
            else:
               self.parent.boxfill_color2 = eval(self.eny4.get( ))
            if self.eny6.get( ) == '':
               self.parent.boxfill_missing = None
            else:
               self.parent.boxfill_missing = eval(self.eny6.get( ))
            if self.eny5.get( ) == '':
               self.parent.boxfill_legend = None
            else:
               self.parent.boxfill_legend=_get_list(self.eny5)
            self.parent.boxfill_ext1 = 'n'
            if self.ext1_toggle.getcurselection() == 'Yes':
               self.parent.boxfill_ext1 = 'y'
            self.parent.boxfill_ext2 = 'n'
            if self.ext2_toggle.getcurselection() == 'Yes':
               self.parent.boxfill_ext2 = 'y'
         except:
            pass
     
   def boxfill_replot( self, parent ):
      self.get_settings()
      parent.panelGC.evt_plot( parent )
      #vcs_function.re_plot( parent, 0 )

   def reset_boxf_attributes(self):
      self.contour_levels.reset_contour_attributes()
      if not self.gm is None:
         self.gm[0].boxfill_type = self.parent.boxfill_type
         self.gen_type.invoke(self.gm[0].boxfill_type)
         self.eny1.setentry( repr(self.gm[0].level_1) )
         self.eny2.setentry( repr(self.gm[0].level_2) )
##          self.eny7.setentry( repr(self.gm[0].levels) )
         self.eny3.setentry( repr(self.gm[0].color_1) )
         self.eny4.setentry( repr(self.gm[0].color_2) )
##          self.eny8.setentry( repr(self.gm[0].fillareacolors) )
         self.eny6.setentry( repr(self.gm[0].missing) )
         if self.gm[0].legend == '':
            self.eny5.setentry( 'None' )
         else:
            self.eny5.setentry( repr(self.gm[0].legend) )
         self.ext1_toggle.invoke('No')
         if self.gm[0].ext_1 == 'y':
            self.ext1_toggle.invoke('Yes')
         self.ext2_toggle.invoke('No')
         if self.gm[0].ext_2 == 'y':
            self.ext2_toggle.invoke('Yes')
      else:
         self.gen_type.invoke(self.parent.boxfill_type)
         self.eny1.setentry( repr(self.parent.boxfill_level1) )
         self.eny2.setentry( repr(self.parent.boxfill_level2) )
##          self.eny7.setentry( repr(self.parent.boxfill_levels) )
         self.eny3.setentry( repr(self.parent.boxfill_color1) )
         self.eny4.setentry( repr(self.parent.boxfill_color2) )
##          self.eny8.setentry( repr(self.parent.boxfill_fillareacolors) )
         self.eny6.setentry( repr(self.parent.boxfill_missing) )
         self.eny5.setentry( repr(self.parent.boxfill_legend) )
         self.ext1_toggle.invoke('No')
         if self.parent.boxfill_ext1 == 'y':
            self.ext1_toggle.invoke('Yes')
         self.ext2_toggle.invoke('No')
         if self.parent.boxfill_ext2 == 'y':
            self.ext2_toggle.invoke('Yes')
         
   def boxfill_clear( self ):
      self.contour_levels.contour_clear()
      self.gen_type.invoke('Linear')
      self.eny1.clear()
      self.eny2.clear()
      self.eny7.clear()
      self.eny3.clear()
      self.eny4.clear()
      self.eny8.clear()
      self.eny6.clear()
      self.eny5.clear()
      self.ext1_toggle.invoke('No')
      self.ext2_toggle.invoke('No')
        
class meshfill_attributes:
   def __init__(self, page, parent, re_plot_flag = 1, gm=None):
      self.parent = parent
      if type(gm) in [types.ListType, types.TupleType, types.NoneType]:
         self.gm = gm
      else:
         self.gm=[gm,]

      # Too much on this page, so I needed to add a scroll window
      scl_Frame=Pmw.ScrolledFrame(page,
                                labelpos = 'n',
                                label_text = 'Meshfill Editor',
                                )
      scl_Frame.pack(fill = 'both', expand = 1,side='top')
      page=scl_Frame.interior()

      framea=Tkinter.Frame(page)
      framea.pack( expand = 1, fill = 'both' )
      lbl1=Tkinter.Label(framea,
          text = "Define meshfill attributes values:",
          justify = 'left',
          anchor = 'w',
          )
      lbl1.pack( side = 'left', expand = 1, fill = 'x', padx=10 )

      gen=Pmw.Group(page,
                    tag_text='General Settings',
                    tag_font=('times',14,'bold'),
                    )
      gen.pack(fill = 'both', expand = 1, side='top', pady=20)
      gen=gen.interior()
      frame1 = Tkinter.Frame(gen)
      frame1.pack(side='top')
      self.eny6=Pmw.EntryField(frame1,
          labelpos = 'w',
          label_text = 'Missing:',
          entry_background = 'white',
          entry_foreground = 'black',
          entry_width =  25,
          )
      self.eny6.pack(  )
      frame = Tkinter.Frame(gen)
      frame.pack()
      self.parent.balloon.bind( self.eny6, "Set the missing color index value. The colormap\nranges from 0 to 255, enter the desired color index value 0\nthrough 255." )

      self.eny1=Pmw.EntryField(frame,
          labelpos = 'w',
          label_text = 'X Wrap:',
          entry_background = 'white',
          entry_foreground = 'black',
          entry_width =  25,
          )
      self.eny1.pack( side='left',expand = 1, fill = 'x', padx=20, pady=5 )
      self.parent.balloon.bind( self.eny1, "Set the wrapping along X axis, 0. means no wrapping" )

      self.eny2=Pmw.EntryField(frame,
          labelpos = 'w',
          label_text = 'Y Wrap:',
          entry_background = 'white',
          entry_foreground = 'black',
          entry_width =  25,
          )
      self.eny2.pack( side='left',expand = 1, fill = 'x', padx=20, pady=5 )
      self.parent.balloon.bind( self.eny1, "Set the wrapping along Y axis, 0. means no wrapping" )

      frame = Tkinter.Frame(gen)
      frame.pack()
      # Create and pack a RadioSelect widget, with radiobuttons for mesh
      self.mesh_toggle = Pmw.RadioSelect(frame,
              buttontype = 'radiobutton',
              orient = 'horizontal',
              labelpos = 'w',
              label_text = 'Show Mesh:  ',
              hull_borderwidth = 2,
              hull_relief = 'ridge',
      )
      self.mesh_toggle.pack( side = 'left', expand = 1, fill = 'x', padx = 20, pady = 5)

      # Add some buttons to the radiobutton RadioSelect.
      for text in ('No', 'Yes'):
         self.mesh_toggle.add(text)
      self.mesh_toggle.invoke('No')
      

      frame = Tkinter.Frame(gen)
      frame.pack()
      # Create and pack a RadioSelect widget, with radiobuttons.
      self.ext1_toggle = Pmw.RadioSelect(frame,
              buttontype = 'radiobutton',
              orient = 'horizontal',
              labelpos = 'w',
              label_text = 'Ext 1:  ',
              hull_borderwidth = 2,
              hull_relief = 'ridge',
      )
      self.ext1_toggle.pack(side='left',expand = 1, fill = 'x', padx = 20, pady = 5)

      # Add some buttons to the radiobutton RadioSelect.
      for text in ('No', 'Yes'):
         self.ext1_toggle.add(text)
      self.ext1_toggle.invoke('No')


      # Create and pack a RadioSelect widget, with radiobuttons.
      self.ext2_toggle = Pmw.RadioSelect(frame,
              buttontype = 'radiobutton',
              orient = 'horizontal',
              labelpos = 'w',
              label_text = 'Ext 2:  ',
              hull_borderwidth = 2,
              hull_relief = 'ridge',
      )
      self.ext2_toggle.pack(side='left',expand = 1, fill = 'x', padx = 20, pady = 5)

      # Add some buttons to the radiobutton RadioSelect.
      for text in ('No', 'Yes'):
         self.ext2_toggle.add(text)
      self.ext2_toggle.invoke('No')

      self.eny5=Pmw.EntryField(gen,
          labelpos = 'w',
          label_text = 'Legend Labels:',
          entry_background = 'white',
          entry_foreground = 'black',
          entry_width =  20,
          )
      self.eny5.pack( side = 'left', expand = 1, fill = 'x', padx=20, pady=5 )
      self.parent.balloon.bind( self.eny5, "Specify the desired legend labels.\nFor example:\nNone -- Allow VCS to generate legend labels\n( ), or [ ], or { } -- No legend labels\n[ 0, 10, 20 ]  or  { 0:'0', 10:'10', 20:'20' }\n[ 0, 10 ]  or  { 0:'text', 10:'more text' }" )

      
      # Adds the contour part
      
      gen=Pmw.Group(page,
                    tag_text='Custom Settings',
                    tag_font=('times',14,'bold'),
                    )
      gen.pack(fill = 'both', expand = 1,side='top')
      gen=gen.interior()
      self.contour_levels=contour_levels( gen, parent, re_plot_flag=0, gm=gm, gm_name='Gfm' )
      
      # Set the meshfill values
      self.reset_meshf_attributes()

      entries = (self.eny6, self.eny5)
      Pmw.alignlabels(entries)

# No longer needed
#      btn2=Tkinter.Button(page,
#          text = 'Plot and Set',
#          background = gui_color.replot,
#          command = gui_control.Command(self.meshfill_replot, parent)
#          )
#      if (re_plot_flag == 1):
#         btn2.pack( expand = 1, fill = 'x', padx=170, pady=5 )

      btn3=Tkinter.Button(page,
          text = 'Reset Meshfill Attributes',
          background = gui_color.one,
          command = self.reset_meshf_attributes,
          )
#      btn3.pack( expand = 1, fill = 'x', padx=170, pady=5 )

   def get_settings( self ):
      self.contour_levels.get_settings()
      if not self.gm is None:
         for g in self.gm:
##             if self.parent.iso_ranges is not None:
##                g.levels = self.parent.iso_ranges
##             else:
##                g.levels = ([1.0000000200408773e+20, 1.0000000200408773e+20],)
##             g.fillareacolors=self.parent.iso_colors
            
            g.missing=eval(self.eny6.get())
            g.legend =_get_list(self.eny5)
            g.wrap=[eval(self.eny2.get()),eval(self.eny1.get())]
            
            g.mesh='n'
            if self.mesh_toggle.getcurselection() == 'Yes':
               g.mesh='y'
               
            g.ext_1='n'
            if self.ext1_toggle.getcurselection() == 'Yes':
               g.ext_1='y'
            g.ext_2='n'
            if self.ext2_toggle.getcurselection() == 'Yes':
               g.ext_2='y'

            if g.name == 'ASD':
               self.parent.meshfill_levels = g.levels
               self.parent.meshfill_fillareacolors = g.fillareacolors
               self.parent.meshfill_missing = g.missing
               self.parent.meshfill_legend = g.legend
               self.parent.meshfill_ext1 = g.ext_1
               self.parent.meshfill_ext2 = g.ext_2
               self.parent.meshfill_mesh = 'n'
               if g.mesh:
                  self.parent.meshfill_mesh = 'y'
               self.parent.meshfill_wrap = g.wrap
      else:
         try:
            if self.parent.iso_ranges is not None:
               self.parent.meshfill_levels=self.parent.iso_ranges
            else:
               self.parent.meshfill_levels = ([1.0000000200408773e+20, 1.0000000200408773e+20],)
            self.parent.meshfill_fillareacolors=self.parent.iso_colors
            if self.eny6.get( ) == '':
               self.parent.meshfill_missing = None
            else:
               self.parent.meshfill_missing = eval(self.eny6.get( ))
            if self.eny5.get( ) == '':
               self.parent.meshfill_legend = None
            else:
               self.parent.meshfill_legend=_get_list(self.eny5)
            self.parent.meshfill_wrap=[eval(self.eny2.get()),eval(self.eny1.get())]
            self.parent.meshfill_mesh = 'n'
            if self.mesh_toggle.getcurselection() == 'Yes':
               self.parent.meshfill_mesh = 'y'
            self.parent.meshfill_ext1 = 'n'
            if self.ext1_toggle.getcurselection() == 'Yes':
               self.parent.meshfill_ext1 = 'y'
            self.parent.meshfill_ext2 = 'n'
            if self.ext2_toggle.getcurselection() == 'Yes':
               self.parent.meshfill_ext2 = 'y'
         except Exception, err:
            pass
     
   def meshfill_replot( self, parent ):
      self.get_settings()
      parent.panelGC.evt_plot( parent )
      #vcs_function.re_plot( parent, 0 )

   def reset_meshf_attributes(self):
      self.contour_levels.reset_contour_attributes()
      if not self.gm is None:
##          self.eny7.setentry( repr(self.gm[0].levels) )
##          self.eny8.setentry( repr(self.gm[0].fillareacolors) )
         self.eny6.setentry( repr(self.gm[0].missing) )
         self.eny1.setentry( repr(self.gm[0].wrap[1]) )
         self.eny2.setentry( repr(self.gm[0].wrap[0]) )
         if self.gm[0].legend == '':
            self.eny5.setentry( 'None' )
         else:
            self.eny5.setentry( repr(self.gm[0].legend) )
         self.mesh_toggle.invoke('No')
         if self.gm[0].mesh == 1:
            self.mesh_toggle.invoke('Yes')
         self.ext1_toggle.invoke('No')
         if self.gm[0].ext_1 == 'y':
            self.ext1_toggle.invoke('Yes')
         self.ext2_toggle.invoke('No')
         if self.gm[0].ext_2 == 'y':
            self.ext2_toggle.invoke('Yes')
      else:
##          self.eny7.setentry( repr(self.parent.boxfill_levels) )
##          self.eny8.setentry( repr(self.parent.boxfill_fillareacolors) )
         self.eny6.setentry( repr(self.parent.meshfill_missing) )
         self.eny5.setentry( repr(self.parent.meshfill_legend) )
         self.eny1.setentry( repr(self.parent.meshfill_wrap[1]) )
         self.eny2.setentry( repr(self.parent.meshfill_wrap[0]) )
         self.mesh_toggle.invoke('No')
         if self.parent.meshfill_mesh == 'y':
            self.mesh_toggle.invoke('Yes')
         self.ext1_toggle.invoke('No')
         if self.parent.meshfill_ext1 == 'y':
            self.ext1_toggle.invoke('Yes')
         self.ext2_toggle.invoke('No')
         if self.parent.meshfill_ext2 == 'y':
            self.ext2_toggle.invoke('Yes')
         
   def meshfill_clear( self ):
      self.contour_levels.contour_clear()
      self.eny1.clear()
      self.eny2.clear()
      self.eny7.clear()
      self.eny3.clear()
      self.eny4.clear()
      self.eny8.clear()
      self.eny6.clear()
      self.eny5.clear()
      self.ext1_toggle.invoke('No')
      self.ext2_toggle.invoke('No')
      self.mesh_toggle.invoke('No')

class contour_levels:
   def __init__(self, page, parent, re_plot_flag = 1, gm=None, gm_name=None):
      self.parent = parent
      if type(gm) in [types.ListType, types.TupleType, types.NoneType]:
         self.gm = gm
      else:
         self.gm=[gm,]

      # Too much on this page, so I needed to add a scroll window
      do_scroll = 0
      if ((gm is not None) and (gm.g_name not in [ 'Gfb', 'Gfm'])): do_scroll = 1
      elif ((gm_name is not None) and (gm_name in [ 'Gfb', 'Gfm'])): do_scroll = 0
      elif (gm is None): do_scroll = 1

      if do_scroll == 1:
         scl_Frame=Pmw.ScrolledFrame(page,
                                labelpos = 'n',
                                label_text = 'Contour Editor',
                                )
         scl_Frame.pack(fill = 'both', expand = 1,side='top')
         page=scl_Frame.interior()

      framea=Tkinter.Frame(page)
      framea.pack( expand = 1, fill = 'both' )
      lbl1=Tkinter.Label(framea,
          text = "Define iso level range values:",
          justify = 'left',
          anchor = 'w',
          )
      lbl1.pack( side = 'left', expand = 1, fill = 'both', padx=10, pady=4 )

      # Create and pack a RadioSelect widget, with radiobuttons.
      self.include_zero = Pmw.RadioSelect(framea,
              buttontype = 'radiobutton',
              orient = 'horizontal',
              labelpos = 'w',
              label_text = 'Include Zero:',
              hull_borderwidth = 2,
              hull_relief = 'ridge',
      )
      self.include_zero.pack(side = 'left', expand = 1, padx = 10, pady = 4)

      # Add some buttons to the radiobutton RadioSelect.
      self.zero_btns = {}
      for text in ('Off', 'On'):
         self.zero_btns[ text ] = self.include_zero.add( text )
      self.include_zero.invoke('Off')

      #self.include_zero.configure( label_state = 'disabled' )
      #for x in self.zero_btns.keys():
      #  self.zero_btns[ x ].configure( state = 'disabled' )

      self.eny1=Pmw.EntryField(page,
          labelpos = 'w',
          label_text = 'Ranges:',
          entry_background = 'white',
          entry_foreground = 'black',
          entry_width =  47,
          )
      self.eny1.pack( expand = 1, fill = 'both', padx=20, pady=4 )
      self.parent.balloon.bind( self.eny1, "The iso level range values. (e.g., 10, 20, 30, 40, 50)." )

      self.eny2=Pmw.EntryField(page,
          labelpos = 'w',
          label_text = 'Colors:',
          entry_background = 'white',
          entry_foreground = 'black',
          entry_width =  47,
          )
      self.eny2.pack( expand = 1, fill = 'both', padx=20, pady=4 )
      self.parent.balloon.bind( self.eny2, "The iso level color index values. The index colors range\nfrom 0 to 255. For example:\n   Use explicit indices: 16, 32, 48, 64, 80;\n   Use two values to generate index range: 16, 32" )
      if (gm is None) or (self.gm[0].g_name=='Gi'):
         self.eny8=Pmw.EntryField(page,
             labelpos = 'w',
             label_text = 'Line Types:',
             entry_background = 'white',
             entry_foreground = 'black',
             entry_width =  47,
             )
         self.eny8.pack( expand = 1, fill = 'both', padx=20, pady=4 )
         self.parent.balloon.bind( self.eny8, "The line type for the isolines. (e.g., 4, 1, 3, 2, 0)." )

         self.eny9=Pmw.EntryField(page,
             labelpos = 'w',
             label_text = 'Line Widths:',
             entry_background = 'white',
             entry_foreground = 'black',
             entry_width =  47,
             )
         self.eny9.pack( expand = 1, fill = 'both', padx=20, pady=4 )
         self.parent.balloon.bind(self.eny9,"The width values for the isolines. (e.g., 1, 3, 5, 2, 7)." )


         self.eny11=Pmw.EntryField(page,
             labelpos = 'w',
             label_text = 'Orientation:',
             entry_background = 'white',
             entry_foreground = 'black',
             entry_width =  47,
             )
         self.eny11.pack( expand = 1, fill = 'both', padx=20, pady=4 )
         self.parent.balloon.bind(self.eny11,"Drawing orientation arrows:\n none (0)\n clokwise (1)\n clockwise where y axis is positive (2)\n clockwise where x axis is positive(3)\n Negative values indicate counter-clockwise." )

         self.eny12=Pmw.EntryField(page,
             labelpos = 'w',
             label_text = 'Countour Arrows Scale:',
             entry_background = 'white',
             entry_foreground = 'black',
             entry_width =  47,
             )
         self.eny12.pack( expand = 1, fill = 'both', padx=20, pady=4 )
         self.parent.balloon.bind(self.eny12,"Scale factor for arrows length" )

         self.eny13=Pmw.EntryField(page,
             labelpos = 'w',
             label_text = 'Countour Arrows Spacing:',
             entry_background = 'white',
             entry_foreground = 'black',
             entry_width =  47,
             )
         self.eny13.pack( expand = 1, fill = 'both', padx=20, pady=4 )
         self.parent.balloon.bind(self.eny13,"Spacing factor for arrows" )

         self.eny14=Pmw.EntryField(page,
             labelpos = 'w',
             label_text = 'Countour Arrows Angle:',
             entry_background = 'white',
             entry_foreground = 'black',
             entry_width =  47,
             )
         self.eny14.pack( expand = 1, fill = 'both', padx=20, pady=4 )
         self.parent.balloon.bind(self.eny14,"Angle of Arrows heads" )
      
         self.isolabels = Pmw.RadioSelect(page,
                            labelpos = 'w',
                            label_text = 'Isoline Labels:',
                            labelmargin = 75,
                            buttontype = 'radiobutton',
                            hull_borderwidth = 2,
                            hull_relief = 'ridge',
                          #  command = gui_control.Command(self.master_switch, parent)
                          )
         self.parent.balloon.bind(self.isolabels, "Toggle 'Isoline Labels' on or off.")

         self.isolabels.add('on')
         self.isolabels.add('off')
         self.isolabels.pack( expand = 1, fill = 'both',padx = 10,pady = 0)

      if (gm is None) or (self.gm[0].g_name=='Gfi'):
         self.eny10=Pmw.EntryField(page,
             labelpos = 'w',
             label_text = 'Legend Labels:',
             entry_background = 'white',
             entry_foreground = 'black',
             entry_width =  47,
             )
         self.eny10.pack( expand = 1, fill = 'both', padx=20, pady=4 )
         self.parent.balloon.bind( self.eny10, "Specify the desired legend labels.\nFor example:\nNone -- Allow VCS to generate legend labels\n( ), or [ ], or { } -- No legend labels\n[ 0, 10, 20 ]  or  { 0:'0', 10:'10', 20:'20' }\n[ 0, 10 ]  or  { 0:'text', 10:'more text' }" )

      frameb=Tkinter.Frame(page)
      frameb.pack( expand = 1, fill = 'both' )
      lbl2=Tkinter.Label(frameb,
          text = "Define iso level parameters:",
          justify = 'left',
          anchor = 'w',
          )
      lbl2.pack( side = 'left', expand = 1, fill = 'both', padx=10, pady=4 )

      # Create and pack a RadioSelect widget, with radiobuttons.
      self.gen_type = Pmw.RadioSelect(frameb,
              buttontype = 'radiobutton',
              orient = 'horizontal',
              labelpos = 'w',
              command = gui_control.Command(self.evt_contour_linear_log, parent),
              label_text = 'Spacing:',
              hull_borderwidth = 2,
              hull_relief = 'ridge',
      )
      self.gen_type.pack(side = 'left', expand = 1, padx = 10, pady = 4)

      # Add some buttons to the radiobutton RadioSelect.
      for text in ('Linear', 'Log'):
         self.gen_type.add(text)
      self.gen_type.invoke('Linear')

      self.eny3_lbl = Tkinter.StringVar()
      self.eny3_lbl.set('Minimum Value:')
      self.eny3=Pmw.EntryField(page,
          labelpos = 'w',
          label_textvariable = self.eny3_lbl,
          entry_background = 'white',
          entry_foreground = 'black',
          entry_width =  10,
          )
      self.eny3.pack( expand = 1, fill = 'both', padx=20, pady=4 )
      self.parent.balloon.bind( self.eny3, "The minimum contour level." )

      self.eny4_lbl = Tkinter.StringVar()
      self.eny4_lbl.set('Maximum Value:')
      self.eny4=Pmw.EntryField(page,
          labelpos = 'w',
          label_textvariable = self.eny4_lbl,
          entry_background = 'white',
          entry_foreground = 'black',
          entry_width =  10,
          )
      self.eny4.pack( expand = 1, fill = 'both', padx=20, pady=4 )
      self.parent.balloon.bind( self.eny4, "The maximum contour level." )

      self.eny5_lbl = Tkinter.StringVar()
      self.eny5_lbl.set('Number of Intervals:')
      self.eny5 = Pmw.Counter(page,
                labelpos = 'w',
                label_textvariable = self.eny5_lbl,
                entry_background = 'white',
                entry_foreground = 'black',
                entryfield_validate = {'validator' : 'integer',
                                   'min' : '1', 'max' : '223'},
                increment = 1)
      self.eny5.pack( expand = 1, fill = 'both', padx=20, pady=4 )
      self.parent.balloon.bind( self.eny5, "The number of intervals between each contour level. Maximum number range [2 to 223]." )

      self.eny6=Pmw.EntryField(page,
          labelpos = 'w',
          label_text = 'Smallest Exponent for Negative Values:',
          entry_background = 'white',
          entry_foreground = 'black',
          entry_width =  10,
          )
      self.eny6.pack( expand = 1, fill = 'both', padx=20, pady=4 )
      self.parent.balloon.bind( self.eny6, "Disabled. Not in use for linear spacing.")

      self.eny7=Pmw.EntryField(page,
          labelpos = 'w',
          label_text = 'Number of Negative Decades:',
          entry_background = 'white',
          entry_foreground = 'black',
          entry_width =  10,
          )
      self.eny7.pack( expand = 1, fill = 'both', padx=20, pady=4 )
      self.parent.balloon.bind( self.eny7, "Disabled. Not in use for linear spacing.")

      if (gm is not None) and (self.gm[0].g_name=='Gi'):
         entries = (self.eny1, self.eny2, self.eny8, self.eny9,self.eny11,self.eny12,self.eny13,self.eny14)
      elif (gm is not None) and (self.gm[0].g_name=='Gfi'):
         entries = (self.eny1, self.eny2, self.eny10)
      elif (gm is not None) and (self.gm[0].g_name in ['Gfb','Gfm']):
         entries = (self.eny1, self.eny2)
      else:
         entries = (self.eny1, self.eny2, self.eny8, self.eny9,self.eny10)
      Pmw.alignlabels(entries)

      entries = (self.eny3, self.eny4, self.eny5, self.eny6, self.eny7)
      Pmw.alignlabels(entries)

      self.gen_btn=Tkinter.Button(page,
          text = 'Generate Ranges',
          background = gui_color.three,
          command = gui_control.Command(self.contour_gen_ranges, parent)
          )
      self.gen_btn.pack( expand = 1, fill = 'both', padx=170, pady=4 )
      self.parent.balloon.bind( self.gen_btn, "Use the 'Minimun Value', 'Maximum Value', and 'Number\nof Intervals' to generate the iso level range values\nand color index values. Note: if 'Ranges' and 'Colors'\nare specified, then the plot will use these numbers\nto generate the contour levels." )

# No longer needed
#      btn2=Tkinter.Button(page,
#          text = 'Plot and Set',
#          background = gui_color.replot,
#          command = gui_control.Command(self.contour_replot, parent)
#          )
#      if (re_plot_flag == 1):
#         btn2.pack( expand = 1, fill = 'both', padx=170, pady=4 )

      btn3=Tkinter.Button(page,
          text = 'Reset',
          background = gui_color.one,
          command = self.reset_contour_attributes,
          )
#      btn3.pack( expand = 1, fill = 'both', padx=170, pady=4 )
      btn4=Tkinter.Button(page,
          text = 'Clear All',
          background = gui_color.one,
          command = self.contour_clear,
          )
      btn4.pack( expand = 1, fill = 'both', padx=170, pady=4 )
      self.reset_contour_attributes()

#	self.dialog.geometry( "+%d+%d" % (self.parent.dxwin, self.parent.dywin) )

   def reset_contour_attributes(self):
      self.eny6.configure( label_state = 'disabled', entry_state = 'disabled')
      self.eny7.configure( label_state = 'disabled', entry_state = 'disabled' )
      if self.gm is None:
         self.gen_type.invoke( self.parent.iso_spacing )
         if self.parent.iso_min is not None:
            self.eny3.setentry( self.parent.iso_min )
         if self.parent.iso_max is not None:
            self.eny4.setentry( self.parent.iso_max )
         if self.parent.iso_num is not None:
            self.eny5.setentry( self.parent.iso_num )
         if self.parent.iso_neg_val is not None:
            self.eny6.setentry( self.parent.iso_neg_val )
         if self.parent.iso_neg_dec is not None:
            self.eny7.setentry( self.parent.iso_neg_dec )
         if self.parent.iso_ranges is not None:
            self.eny1.setentry( tuple( self.parent.iso_ranges ) )
         if self.parent.iso_colors is not None:
            self.eny2.setentry( tuple( self.parent.iso_colors ) )

         if self.parent.iso_line_types is not None:
            self.eny8.setentry( tuple( self.parent.iso_line_types ) )
         if self.parent.iso_line_widths is not None:
            self.eny9.setentry( tuple( self.parent.iso_line_widths ) )
         if self.parent.iso_legend is not None:
            self.eny10.setentry( tuple( self.parent.iso_legend ) )
         if self.parent.iso_line_clockwise is not None:
            self.eny11.setentry( tuple( self.parent.iso_line_clockwise ) )
         if self.parent.iso_line_arrow_scale is not None:
            self.eny12.setentry( tuple( self.parent.iso_line_arrow_scale ) )
         if self.parent.iso_line_arrow_spacing is not None:
            self.eny13.setentry( tuple( self.parent.iso_line_arrow_spacing ) )
         if self.parent.iso_line_arrow_angle is not None:
            self.eny14.setentry( tuple( self.parent.iso_line_arrow_angle ) )
      else:
         #
         # Note: see functions hold_gm_original_settings and gm_reset for the
         # storing and retrieval of original values. These functions are located
         # in the vcs package and in the graphicsmethodgui.py file.
         #
         if self.gm[0].g_name in ['Gfi','Gfb','Gfm']:
            self.eny1.setentry(repr(self.gm[0].levels))
         else:
            self.eny1.setentry(repr(self.gm[0].level))
         if self.gm[0].g_name in ['Gfi','Gfb','Gfm']:
            self.eny2.setentry(repr(self.gm[0].fillareacolors))
         else:
            self.eny2.setentry(repr(self.gm[0].linecolors))
         if self.gm[0].g_name == 'Gi':
            self.eny8.setentry( self.gm[0].line )
            self.eny9.setentry( self.gm[0].linewidths )
            if (self.gm[0].label == 'n'): self.isolabels.invoke( 'off' )
            else: self.isolabels.invoke( 'on' )
         if self.gm[0].g_name=='Gfi':
            self.eny10.setentry(repr(self.gm[0].legend))
           
   def evt_contour_linear_log( self, parent, tag ):
      try:
         self.eny1.clear()
         self.eny2.clear()
         self.eny3.clear()
         self.eny4.clear()
         self.eny5.clear()
         self.eny8.clear()
         self.eny9.clear()
         try:
            self.eny10.clear()
         except:
            pass
         try:
            self.eny11.clear()
            self.eny12.clear()
            self.eny13.clear()
            self.eny14.clear()
         except:
            pass
         self.parent.iso_spacing = tag
         if tag == 'Linear':
           #self.include_zero.configure( label_state = 'disabled' )
           #for x in self.zero_btns.keys():
           #  self.zero_btns[ x ].configure( state = 'disabled' )
           self.eny6.clear()
           self.eny7.clear()
           self.eny3_lbl.set('Minimum Value:')
           self.parent.balloon.bind( self.eny3, "The minimum contour level." )
           self.eny4_lbl.set('Maximum Value:')
           self.parent.balloon.bind( self.eny4, "The maximum contour level." )
           self.eny5_lbl.set('Number of Intervals:')
           self.parent.balloon.bind( self.eny5,
               "The number of intervals between each contour level. Maximum number range [2 to 223]." )
           self.eny6.configure( label_state = 'disabled', entry_state = 'disabled' )
           self.parent.balloon.bind( self.eny6, "Disabled. Not in use for linear spacing.")
           self.eny7.configure( label_state = 'disabled', entry_state = 'disabled' )
           self.parent.balloon.bind( self.eny7, "Disabled. Not in use for linear spacing.")
           self.parent.balloon.bind( self.gen_btn, "Use the 'Minimun Value', 'Maximum Value', and 'Number\nof Intervals' to generate the iso level range values\nand color index values. Note: if 'Ranges' and 'Colors'\nare specified, then the plot will use these numbers\nto generate the contour levels." )
         elif tag == 'Log':
           #self.include_zero.configure( label_state = 'normal' )
           #for x in self.zero_btns.keys():
           #  self.zero_btns[ x ].configure( state = 'normal' )
           self.eny3_lbl.set('Smallest Exponent for Positive Values:')
           self.parent.balloon.bind( self.eny3, "Smallest exponent for positive values." )
           self.eny4_lbl.set('Number of Positive Decades:')
           self.parent.balloon.bind( self.eny4, "Number of positive decades." )
           self.eny5_lbl.set('Levels per Decade:')
           self.parent.balloon.bind( self.eny5, "Levels per decade." )
           self.eny6.configure( label_state = 'normal', entry_state = 'normal' )
           self.parent.balloon.bind( self.eny6, "Smallest exponent for negative values." )
           self.eny7.configure( label_state = 'normal', entry_state = 'normal' )
           self.parent.balloon.bind( self.eny7, "Number of negative decades." )
           self.parent.balloon.bind( self.gen_btn, "Use the 'Positive Value', Positive Decades', 'Levels\nper Decade', 'Negative Values', and 'Negative Decades'\nto generate log spaced contour levels. Note: if\n'Ranges' and 'Colors' are specified, then the plot\nwill use these numbers to generate the contour levels." )
      except:
         pass

   def get_settings( self ):
      if self.eny3.get( ) != '':
         self.parent.iso_min = return_float_None( self.eny3.get( ) )
      else:
         self.parent.iso_min = None
      if self.eny4.get( ) != '':
         self.parent.iso_max = return_float_None( self.eny4.get( ) )
      else:
         self.parent.iso_max = None
      if self.eny5.get( ) != '':
         self.parent.iso_num = return_int_None( self.eny5.get( ) )
      else:
         self.parent.iso_num = None
      if self.eny6.get( ) != '':
         self.parent.iso_neg_val = return_float_None( self.eny6.get( ) )
      else:
         self.parent.iso_neg_val = None
      if self.eny7.get( ) != '':
         self.parent.iso_neg_dec = return_float_None( self.eny7.get( ) )
      else:
         self.parent.iso_neg_dec = None
      if self.gm is None:
         if self.eny1.get( ) != '':
            s = _get_text_str( self.eny1.get( ) )
            for i in range(len(s)): s[i]=string.atof(s[i]) # make sure the values are floats
            self.parent.iso_ranges = s
            if len(self.parent.iso_ranges) == 1:
               self.parent.iso_ranges.append(self.parent.iso_ranges[0])
         else:
            self.parent.iso_ranges = None

         if self.eny2.get( ) != '':
            cstr = _get_text_str( self.eny2.get( ) )

            self.parent.iso_colors = []
            ranges_len = len( self.parent.iso_ranges )
            if (len(cstr) in [1, 2]) and ((ranges_len-1) != len(cstr)):
               i=0; j=0
               while i < ranges_len:
                   try:
                      c_index_1 = c_index = string.atoi( cstr[ j ] )
                      c_index_2 = c_index = string.atoi( cstr[ j+1 ] )
                      step = 1
                      if (c_index_1 > c_index_2): step = -1
	              for k in range( c_index_1, c_index_2, step ):
                           self.parent.iso_colors.append( k )
		           i+=1
		           if i>=ranges_len: break
                      j+=1
                   except:
                      self.parent.iso_colors.append( c_index )
		      i+=1
            else:
               for i in range(len(cstr)):
                  self.parent.iso_colors.append( string.atoi( cstr[i] ) )
               for j in range(len(cstr), (len(self.parent.iso_ranges))):
                  self.parent.iso_colors.append( string.atoi( cstr[i] ) )
         else:
            self.parent.iso_colors = None
         
         # This is here to remove the extra number at the end. Not sure what effects this will
	 # have on the rest of the code. This is a note to my set to look at later if problems
	 # occur..... Dean
         try:
            if self.parent.iso_colors[-1] == self.parent.iso_colors[-2]: del self.parent.iso_colors[-1]
         except:
            pass

         if self.eny8.get( ) != '':
            s = _get_text_str( self.eny8.get( ) )
            for i in range(len(s)): s[i]=string.atoi(s[i]) # make sure the values are integers
            self.parent.iso_line_types = s
         else:
            self.parent.iso_line_types = None
         if self.eny9.get( ) != '':
            s = _get_text_str( self.eny9.get( ) )
            for i in range(len(s)): s[i]=string.atof(s[i]) # make sure the values are floats
            self.parent.iso_line_widths = s
         else:
            self.parent.iso_line_widths = None

         if self.eny10.get( ) == '':
            self.parent.iso_legend = None
         else:
            try: self.parent.iso_legend =_get_list(self.eny10)
            except: self.parent.iso_legend = []
         if self.eny11.get( ) != '':
            s = _get_text_str( self.eny11.get( ) )
            for i in range(len(s)): s[i]=int(s[i]) # make sure the values are int
            self.parent.iso_line_clockwise = s
         else:
            self.parent.iso_line_clockwise = None
         if self.eny12.get( ) != '':
            s = _get_text_str( self.eny12.get( ) )
            for i in range(len(s)): s[i]=float(s[i]) # make sure the values are floats
            self.parent.iso_line_arrow_scale = s
         else:
            self.parent.iso_line_arrow_scale = None
         if self.eny13.get( ) != '':
            s = _get_text_str( self.eny13.get( ) )
            for i in range(len(s)): s[i]=float(s[i]) # make sure the values are floats
            self.parent.iso_line_arrow_spacing = s
         else:
            self.parent.iso_line_arrow_spacing = None
         if self.eny14.get( ) != '':
            s = _get_text_str( self.eny14.get( ) )
            for i in range(len(s)): s[i]=float(s[i]) # make sure the values are floats
            self.parent.iso_line_arrow_angle = s
         else:
            self.parent.iso_line_arrow_angle = None
      else:
         #
         # Note: see functions hold_gm_original_settings and gm_reset for the
         # storing and retrieval of original values. These functions are located
         # in the vcs package and in the graphicsmethodgui.py file.
         #
         for g in self.gm:
            try:
               eny1 = _get_text_str( self.eny1.get( ) )
               meny1 = []
               for i in range(len(eny1)):    # remove repeated values
                   if eny1[i] not in meny1: meny1.append(eny1[i])
               if len(meny1) == 1: meny1.append( meny1[0] ) # if length is 1, then append the first item
               s = ''
               for x in meny1: s= s + x + ', '
               levs=eval( s )
            except Exception,err:
               levs=([1.0000000200408773e+20, 1.0000000200408773e+20],)
            if levs=='' or levs==(None,):
               levs=([1.0000000200408773e+20, 1.0000000200408773e+20],)
            elif not type(levs) in [types.ListType,types.TupleType,types.NoneType]:
               levs=[levs]
            if g.g_name in ['Gfi','Gfb','Gfm']:
               g.levels=levs
            else:       # Below is for Gi - isoline
               l = [levs[0],]
               for i in range(len(levs)-1):
                   if levs[i] != 0: l.append(levs[i])
               g.level=l
            try:
               eny2 = _get_text_str( self.eny2.get( ) )
               s = ''
               for x in eny2: s= s + x + ', '
               levs=eval(s)
            except:
               levs=None
            if levs=='' or levs==(None,):
               levs=None
            elif not type(levs) in [types.ListType,types.TupleType,types.NoneType]:
               levs=[levs]
            if g.g_name in ['Gfi','Gfb','Gfm']:
               g.fillareacolors=levs
            else:
               g.linecolors=levs

            if g.g_name == 'Gi':
               g.line =_get_list(self.eny8)
               g.linewidths =_get_list(self.eny9)
               g.clockwise =_get_list(self.eny11)
               g.scale =_get_list(self.eny12)
               g.spacing =_get_list(self.eny13)
               g.angle =_get_list(self.eny14)

            if g.g_name == 'Gi':
               if (self.isolabels.getcurselection() == "on"): g.label = 'y'
               else: g.label = 'n'

            if g.g_name == 'Gfi':
               g.legend =_get_list(self.eny10)
            
#      print '\n\n\n################## after getttings ################## = ', self.parent.iso_ranges, self.parent.iso_colors, '\n\n\n'

   def contour_gen_ranges( self, parent ):
        eny3 = self.eny3.get( )
        eny4 = self.eny4.get( )
        eny5 = self.eny5.get( )
        if self.gen_type.getcurselection() == 'Linear':
           if ((eny3 != '') and (eny5 != '') and (eny4 != '')):
              iso_min = return_float_None( eny3 )
              iso_max = return_float_None( eny4 )
              iso_num = return_int_None( eny5 )
              if iso_num < 2:
                 showerror('Error Message', 'The "Number of Intervals" value must be between 2 and 223.')
                 return

              # Generate ranges and color indices
              rcolors = []
              l=[]
              levels=iso_min
              delta = float( (iso_max - iso_min)/ (iso_num) )
              d = int(222/(iso_num-1))
              for a in range(int(iso_num+1)):
                 l.append(levels)
                 levels=levels+delta
                 rcolors.append(16 + a*d)

              # Include the zero if requested by the user
              if self.include_zero.getcurselection() == 'On':
                 found_zero_place = 0
                 for i in range(len(l)):
	              if l[i] == 0.0:
                          found_zero_place = 1
		          break
	              elif l[i] > 0.0:
                          found_zero_place = 1
		          l.insert(i, 0.0)
		          break
                 if found_zero_place == 0:
		      l.insert(len(l), 0.0)
                 d = int(222/(len(l)-2))
                 rcolors = []
                 for a in range(len(l)):
                    rcolors.append(16 + a*d)

              s=str(l)
              self.eny1.clear()
              self.eny1.insert(0, s[1:(len(s)-1)])
              rcolors.remove(rcolors[-1])
              s=str(rcolors)
              self.eny2.clear()
              self.eny2.insert(0, s[1:(len(s)-1)])

           #if self.include_zero.getcurselection() == 'On':
              #contours.insert( 0, 0.0)

           else:
              gui_message.error('All iso-level parameters were not included.')
        elif self.gen_type.getcurselection() == 'Log':
           eny6 = self.eny6.get( )
           eny7 = self.eny7.get( )
           try:
              A = return_float_None( eny3 )
           except:
              A = 0
           try:
              B = return_float_None( eny4 )
           except:
              B = 0
           try:
              C = return_float_None( eny5 )
           except:
              C = 0
           try:
              D = return_float_None( eny6 )
           except:
              D = 0
           try:
              E = return_float_None( eny7 )
           except:
              E = 0
           contours = []
           if (C > 0):              # Generate positive and/or negative contours
              if (E > 0):           # Generate negative contours...
                 for i in range( int((E*C)),0,-1 ):
                     contours.append( round_number( -10.0 ** (D+(i-1)/C) ) )
              if (B > 0):           # Generate positive contours...
                 for i in range( 1, int((B*C)+1) ):
                     contours.append( round_number( 10.0 ** (A+(i-1)/C) ) )
           else:
              gui_message.error("The 'Levels per Decade' must be a postive number.")

           # Include the zero if requested by the user
           if self.include_zero.getcurselection() == 'On':
              found_zero_place = 0
              for i in range(len(contours)):
                   if contours[i] == 0.0:
                       found_zero_place = 1
                       break
                   elif contours[i] > 0.0:
                       found_zero_place = 1
                       contours.insert(i, 0.0)
                       break
              if found_zero_place == 0:
                   contours.insert(len(contours), 0.0)

           # Show contour spacing
           s=str(contours)
           self.eny1.clear()
           self.eny1.insert(0, s[1:(len(s)-1)])

           # Set the iso range values to contour
           parent.iso_ranges = contours

           # Show color indices
           rcolors = []
           iso_num = int( len( contours ) -1 )
           d = int(222/(iso_num-1))
           for a in range(iso_num):
              rcolors.append(16 + a*d)
           s=str(rcolors)
           self.eny2.clear()
           self.eny2.insert(0, s[1:(len(s)-1)])

   def contour_replot( self, parent ):
#      self.get_settings()
      if parent.iso_spacing == 'Log':
         self.contour_gen_ranges( parent )
      parent.panelGC.evt_plot( parent )
      #vcs_function.re_plot( parent, 0 )

   def contour_clear( self ):
      self.eny1.clear()
      self.eny2.clear()
      self.eny3.clear()
      self.eny4.clear()
      self.eny5.clear()
      self.eny6.clear()
      self.eny7.clear()
      try:
         self.eny8.clear()
         self.eny9.clear()
         self.eny11.clear()
         self.eny12.clear()
         self.eny13.clear()
         self.eny14.clear()
      except:
         pass
      try: self.eny10.clear()
      except: pass

def round_number( N ):
   import numpy
   P = 10.0 ** ( numpy.floor(numpy.log10(abs(N) ) ) )
   return( sign(N) * int( abs(N)/P + 0.5) * P )

def sign ( N ):
   if (N < 0): return -1
   else: return 1

class oneD_attributes:      
   def __init__(self, page, parent, re_plot_flag = 1, gm=None):
        self.parent = parent
        if type(gm) in [types.ListType, types.TupleType, types.NoneType]:
           self.gm = gm
        else:
           self.gm=[gm,]

        # Too much on this page, so I needed to add a scroll window
        scl_Frame=Pmw.ScrolledFrame(page,
                                labelpos = 'n',
                                label_text = '1D Plot Editor',
                                )
        scl_Frame.pack(fill = 'both', expand = 1,side='top')
        page=scl_Frame.interior()

        lbl1=Tkinter.Label(page,
            text = "Define up to 15 values:",
            justify = 'left',
            anchor = 'w',
            )
        lbl1.pack( expand = 1, fill = 'x', padx=10 )

        self.eny1=Pmw.EntryField(page,
            labelpos = 'w',
            label_text = 'Line Types:',
            entry_background = 'white',
            entry_foreground = 'black',
            entry_width =  47,
            )
        self.eny1.pack( expand = 1, fill = 'x', padx=20 )
        self.parent.balloon.bind( self.eny1, "Set the line types. The line values can either be\n('solid', 'dash', 'dot', 'dash-dot', 'long-dash')\nor (0, 1, 2, 3, 4) or None" )

        self.eny2=Pmw.EntryField(page,
            labelpos = 'w',
            label_text = 'Line Colors:',
            entry_background = 'white',
            entry_foreground = 'black',
            entry_width =  47,
            )
        self.eny2.pack( expand = 1, fill = 'x', padx=20 )
        self.parent.balloon.bind( self.eny2, "Set the line colors. The line color attribute\n values must be integers ranging from 0 to 255.\n(e.g., 16, 32, 48, 64) " )

        self.eny3=Pmw.EntryField(page,
            labelpos = 'w',
            label_text = 'Line Widths:',
            entry_background = 'white',
            entry_foreground = 'black',
            entry_width =  47,
            )
        self.eny3.pack( expand = 1, fill = 'x', padx=20 )
        self.parent.balloon.bind( self.eny3, "Set the line width. The line width is an integer\nor float value in the range (1 to 100)" )

        self.eny4=Pmw.EntryField(page,
            labelpos = 'w',
            label_text = 'Marker Types:',
            entry_background = 'white',
            entry_foreground = 'black',
            entry_width =  47,
            )
        self.eny4.pack( expand = 1, fill = 'x', padx=20 )
        self.parent.balloon.bind( self.eny4, "Set the marker types. The marker values can either\nbe (None, 'dot', 'plus', 'star', 'circle', 'cross', 'diamond',\n'triangle_up', 'triangle_down', 'triangle_left',\n'triangle_right', 'square', 'diamond_fill',\n'triangle_up_fill', 'triangle_down_fill',\n'triangle_left_fill', 'triangle_right_fill',\n'square_fill') or (0, 1, 2, 3, 4, 5, 6, 7, 8, 9,\n10, 11, 12, 13, 14, 15, 16, 17) or None. " )

        self.eny5=Pmw.EntryField(page,
            labelpos = 'w',
            label_text = 'Marker Colors:',
            entry_background = 'white',
            entry_foreground = 'black',
            entry_width =  47,
            )
        self.eny5.pack( expand = 1, fill = 'x', padx=20 )
        self.parent.balloon.bind( self.eny5, "Set the marker colors. The marker color attribute\nvalues must be integers ranging from 0 to 255." )

        self.eny6=Pmw.EntryField(page,
            labelpos = 'w',
            label_text = 'Marker Sizes:',
            entry_background = 'white',
            entry_foreground = 'black',
            entry_width =  47,
            )
        self.eny6.pack( expand = 1, fill = 'x', padx=20 )
        self.parent.balloon.bind( self.eny6, "Set the marker sizes. The marker size attribute\nvalues must be integers or floats ranging  from\n1 to 300. " )

        entries = (self.eny1, self.eny2, self.eny3, self.eny4, self.eny5, self.eny6)
        Pmw.alignlabels(entries)
        self.reset_oneD_attributes()
# No longer needed
#        btn1=Tkinter.Button(page,
#            text = 'Plot and Set',
#            background = gui_color.replot,
#            command = gui_control.Command(self.oneD_replot, parent)
#            )
#        if re_plot_flag == 1:
#           btn1.pack( expand = 1, fill = 'x', padx=170, pady=5 )

        btn2=Tkinter.Button(page,
            text = 'Reset OneD attributes',
            background = gui_color.one,
            command = self.reset_oneD_attributes
            )
#        btn2.pack( expand = 1, fill = 'x', padx=170, pady=5 )

   def get_settings( self ):
      if not self.gm is None:
         for g in self.gm:
            try:
               g.line=eval(self.eny1.get())
            except:
               g.line=str(self.eny1.get())
            try:
               g.linecolor=eval(self.eny2.get())
            except:
               g.linecolor=str(self.eny2.get())
            try:
               g.linewidth=eval(self.eny3.get())
            except:
               g.linewidth=str(self.eny3.get())
            try:
               g.marker=eval(self.eny4.get())
            except:
               g.marker=str(self.eny4.get())
            g.markercolor=eval(self.eny5.get())
            g.markersize=eval(self.eny6.get())
      else:
         # Set the Line attributes
         if self.eny1.get( ) != '':
            eny1 = string.replace( self.eny1.get( ), ' ', ',' )
            while string.find(eny1,',,',0) != -1: eny1=string.replace(eny1,',,',',')
            if eny1[-1] == ',': eny1 = eny1[:-1]
            ltypes = string.split(eny1, ',')
            eny2 = string.replace( self.eny2.get( ), ' ', ',' )
            while string.find(eny2,',,',0) != -1: eny2=string.replace(eny2,',,',',')
            if eny2[-1] == ',': eny2 = eny2[:-1]
            lcolors = string.split(eny2, ',')
            eny3 = string.replace( self.eny3.get( ), ' ', ',' )
            while string.find(eny3,',,',0) != -1: eny3=string.replace(eny3,',,',',')
            if eny3[-1] == ',': eny3 = eny3[:-1]
            lwidths = string.split(eny3, ',')
            self.parent.oneD_ltypes = ''
            self.parent.oneD_lcolors = ''
            self.parent.oneD_lwidths = ''
            for i in range(len(ltypes)):
               line = self.parent.vcs[ self.parent.vcs_id ].getline( 'ASD%d' % (i+1) )
               try:
                  line.type=eval(ltypes[i])
               except:
                  try:
                     line.type=ltypes[i]
                  except: line.type=['solid']
               if line.type is not None:
                  self.parent.oneD_ltypes += line.type[0] + ', '
               else:
                  self.parent.oneD_ltypes = None
               try:
                  line.color=[ string.atoi(lcolors[i]) ]
               except:
                  line.color=[241]
               if line.color is not None:
                  self.parent.oneD_lcolors += repr( line.color[0] ) + ', '
               else:
                  self.parent.oneD_lcolors = None
               try:
                  line.width=[ string.atof(lwidths[i]) ]
               except:
                  line.width=[5]
               if line.width is not None:
                  self.parent.oneD_lwidths +=  repr(line.width[0] ) + ', '
               else:
                  self.parent.oneD_lwidths = None
         else:
            self.parent.oneD_ltypes = None
            self.parent.oneD_lcolors = None
            self.parent.oneD_lwidths = None

         # Set the marker attributes
         if self.eny4.get( ) != '':
            eny4 = string.replace( self.eny4.get( ), ' ', ',' )
            while string.find(eny4,',,',0) != -1: eny4=string.replace(eny4,',,',',')
            if eny4[-1] == ',': eny4 = eny4[:-1]
            mtypes = string.split(eny4, ',')
            eny5 = string.replace( self.eny5.get( ), ' ', ',' )
            while string.find(eny5,',,',0) != -1: eny5=string.replace(eny5,',,',',')
            if eny5[-1] == ',': eny5 = eny5[:-1]
            mcolors = string.split(eny5, ',')
            eny6 = string.replace( self.eny6.get( ), ' ', ',' )
            while string.find(eny6,',,',0) != -1: eny6=string.replace(eny6,',,',',')
            if eny6[-1] == ',': eny6 = eny6[:-1]
            mwidths = string.split(eny6, ',')
            self.parent.oneD_mtypes = ''
            self.parent.oneD_mcolors = ''
            self.parent.oneD_mwidths = ''
            for i in range(len(mtypes)):
               marker = self.parent.vcs[ self.parent.vcs_id ].getmarker( 'ASD%d' % (i+1) )
               try:
                  marker.type=eval(mtypes[i])
               except:
                  marker.type=mtypes[i]
                  try:
                     marker.type=mtypes[i]
                  except: marker.type=['dot']
               if mtypes[i] == '0': marker.type = None
               if marker.type is not None: 
                  self.parent.oneD_mtypes += marker.type[0] + ', '
               else:
                  self.parent.oneD_mtypes = None
               try:
                  marker.color=[ string.atoi(mcolors[i]) ]
               except:
                  marker.color=[241]
               if marker.color is not None: 
                  self.parent.oneD_mcolors += repr( marker.color[0] ) + ', '
               else:
                  self.parent.oneD_mcolors = None
               try:
                  marker.size=[ string.atof(mwidths[i]) ]
               except:
                  marker.size=[5]
               if marker.size is not None: 
                  self.parent.oneD_mwidths +=  repr( marker.size[0] ) + ', '
               else:
                  self.parent.oneD_mwidths = None
         else:
            self.parent.oneD_mtypes = None
            self.parent.oneD_mcolors = None
            self.parent.oneD_mwidths = None

   def reset_oneD_attributes(self):
      if not self.gm is None:
         self.eny1.setentry( repr(self.gm[0].line) )
         self.eny2.setentry( repr(self.gm[0].linecolor) )
         self.eny3.setentry( repr(self.gm[0].linewidth) )
         self.eny4.setentry( repr(self.gm[0].marker) )
         self.eny5.setentry( repr(self.gm[0].markercolor) )
         self.eny6.setentry( repr(self.gm[0].markersize) )
      else:
         if self.parent.oneD_ltypes is None:
            self.eny1.setentry( repr(self.parent.oneD_ltypes) )
         else:
            self.eny1.setentry( self.parent.oneD_ltypes )
            k = len(self.parent.oneD_ltypes)
            self.eny1.delete(k-2, k)

         if self.parent.oneD_lcolors is None:
            self.eny2.setentry( repr(self.parent.oneD_lcolors) )
         else:
            self.eny2.setentry( self.parent.oneD_lcolors )
            k = len(self.parent.oneD_lcolors)
            self.eny2.delete(k-2, k)
  
         if self.parent.oneD_lwidths is None:
            self.eny3.setentry( repr(self.parent.oneD_lwidths) )
         else:
            self.eny3.setentry( self.parent.oneD_lwidths )
            k = len(self.parent.oneD_lwidths)
            self.eny3.delete(k-2, k)

         if self.parent.oneD_mtypes is None:
            self.eny4.setentry( repr(self.parent.oneD_mtypes) )
         else:
            self.eny4.setentry( self.parent.oneD_mtypes )
            k = len(self.parent.oneD_mtypes)
            self.eny4.delete(k-2, k)

         if self.parent.oneD_mcolors is None:
            self.eny5.setentry( repr(self.parent.oneD_mcolors) )
         else:
            self.eny5.setentry( self.parent.oneD_mcolors )
            k = len(self.parent.oneD_mcolors)
            self.eny5.delete(k-2, k)
  
         if self.parent.oneD_mwidths is None:
            self.eny6.setentry( repr(self.parent.oneD_mwidths) )
         else:
            self.eny6.setentry( self.parent.oneD_mwidths )
            k = len(self.parent.oneD_mwidths)
            self.eny6.delete(k-2, k)

   def oneD_replot( self, parent ):
      self.get_settings()
      parent.panelGC.evt_plot( parent )
      #vcs_function.re_plot( parent, 0 )

   def oneD_clear( self, parent ):
      self.eny1.clear()
      self.eny2.clear()
      self.eny3.clear()
      self.eny4.clear()
      self.eny5.clear()
      self.eny6.clear()

      # Reset the line attributes back to original settings
      for i in range(1,16):
         line_name= 'ASD%d' % i
         line = parent.vcs[ parent.vcs_id ].getline(line_name)
         line.type = 'solid'
         line.width = 4
         line.color = 240 + i
         marker_name= 'ASD%d' % i
         marker = parent.vcs[ parent.vcs_id ].getmarker(marker_name)
         marker.type = 'dot'
         marker.width = 4
         marker.color = 240 + i

class vector_attributes:
   def __init__(self, page, parent, re_plot_flag = 1, gm=None):
      self.parent = parent
      if type(gm) in [types.ListType, types.TupleType, types.NoneType]:
         self.gm = gm
      else:
         self.gm=[gm,]
   
      # Too much on this page, so I needed to add a scroll window
      scl_Frame=Pmw.ScrolledFrame(page,
                                labelpos = 'n',
                                label_text = 'Vector Editor',
                                )
      scl_Frame.pack(fill = 'both', expand = 1,side='top')
      page=scl_Frame.interior()

      self.var1 = Tkinter.StringVar()
      self.opt1 = Pmw.OptionMenu(page,
		labelpos = 'w',
		label_text = 'Vector Line Type:',
		menubutton_textvariable = self.var1,
		items = ['solid', 'dash', 'dot', 'dash-dot', 'long-dash'],
		menubutton_width = 12,
      )
      self.opt1.pack(expand=1, padx=5, pady=5)
      self.parent.balloon.bind( self.opt1, "Select the vector line type. " )

      self.ctr1 = Pmw.Counter(page,
                labelpos = 'w',
                label_text = 'Vector Line Color Index:',
                entry_background = 'white',
                entry_foreground = 'black',
                entryfield_entry_width = 5,
                entryfield_validate = {'validator' : 'integer',
                                   'min' : '0', 'max' : '255'},
                increment = 1)
      self.ctr1.pack( expand=1, padx=5, pady=5 )
      self.parent.balloon.bind( self.ctr1, "Select the vector color index. " )

      self.ctr2 = Pmw.Counter(page,
                labelpos = 'w',
                label_text = 'Vector Scale:',
                entry_background = 'white',
                entry_foreground = 'black',
                entryfield_entry_width = 12,
                datatype={'counter':'real','separator':'.'},
                entryfield_validate = {'validator' : 'real', 'separator':'.'},
                increment = 0.1)
      self.ctr2.pack( expand=1, padx=5, pady=5 )
      self.parent.balloon.bind( self.ctr2, "Set the vector scale factor. " )

      self.var2 = Tkinter.StringVar()
      self.opt2 = Pmw.OptionMenu(page,
                labelpos = 'w',
                label_text = 'Vector Alignment:',
                menubutton_textvariable = self.var2,
                items = ['head', 'center', 'tail'],
                menubutton_width = 12,
      )
      self.opt2.pack(expand = 1, padx=5, pady=5)
      self.parent.balloon.bind( self.opt2, "Set the vector alignment. " )

      self.var3 = Tkinter.StringVar()
      self.opt3 = Pmw.OptionMenu(page,
                labelpos = 'w',
                label_text = 'Vector head Type:',
                menubutton_textvariable = self.var3,
                items = ['arrows', 'barbs', 'solidarrows'],
                menubutton_width = 12,
      )
      self.opt3.pack(expand = 1, padx=5, pady=5)
      self.parent.balloon.bind( self.opt3, "Set the vector head type. " )

      self.ctr3 = Pmw.Counter(page,
                labelpos = 'w',
                label_text = 'Vector Reference:',
                entry_background = 'white',
                entry_foreground = 'black',
                entryfield_entry_width = 12,
                datatype={'counter':'real','separator':'.'},
                entryfield_validate = {'validator' : 'real', 'separator': '.'},
                increment = 0.1)
      self.ctr3.pack( expand = 1, padx=5, pady=5 )
      self.parent.balloon.bind( self.ctr3, "Set the vector reference. Note: if the value is 1e+20,\nthen VCS will determine the vector reference." )

      self.reset_vector_attributes()
      
# No longer needed
#      self.btn1=Tkinter.Button(page,
#            text = 'Plot and Set',
#            background = gui_color.replot,
#            command = gui_control.Command(self.vector_replot, parent)
#            )
#      if re_plot_flag == 1:
#         self.btn1.pack( expand = 1, fill = 'x', padx=170, pady=5 )

      self.btn2=Tkinter.Button(page,
            text = 'Reset Vector Attributes',
            background = gui_color.three,
            command = self.reset_vector_attributes
            )
#      self.btn2.pack( expand = 1, padx=170, pady=5 )

   def vector_replot( self, parent ):
      self.get_settings()
      parent.panelGC.evt_plot( parent )
      #vcs_function.re_plot( parent, 0 )

   def get_settings( self ):
      if not self.gm is None:
         for g in self.gm:
            g.line=self.opt1.getcurselection()
            g.linecolor = eval(self.ctr1.get())
            g.scale = eval(self.ctr2.get())
            g.alignment = self.opt2.getcurselection()
            g.type = self.opt3.getcurselection()
            g.reference = eval(self.ctr3.get())
      else:
         self.parent.vec_line=self.opt1.getcurselection()
         self.parent.vec_linecolor=eval(self.ctr1.get())
         self.parent.vec_scale=eval(self.ctr2.get())
         self.parent.vec_alignement=self.opt2.getcurselection()
         self.parent.vec_type=self.opt3.getcurselection()
         self.parent.vec_ref=eval(self.ctr3.get())

   def reset_vector_attributes( self ):
      if not self.gm is None:
         if self.gm[0].line is None :
            self.opt1.invoke('solid')
         else:
            self.opt1.invoke(self.gm[0].line)
         self.opt2.invoke(self.gm[0].alignment)
         self.opt3.invoke(self.gm[0].type)
         if self.gm[0].linecolor is None :
            self.ctr1.setentry(241  )
         else:
            self.ctr1.setentry(self.gm[0].linecolor  )
         self.ctr2.setentry(self.gm[0].scale)
         self.ctr3.setentry(self.gm[0].reference)
      else:
         if self.parent.vec_line is None :
            self.opt1.invoke('solid')
         else:
            self.opt1.invoke(self.parent.vec_line)
         self.opt2.invoke(self.parent.vec_alignment)
         self.opt3.invoke(self.parent.vec_type)
         if self.parent.vec_linecolor is None :
            self.ctr1.setentry(241  )
         else:
            self.ctr1.setentry(self.parent.vec_linecolor  )
         if self.parent.vec_scale is None:
            self.ctr2.setentry(repr(1))
         else:
            self.ctr2.setentry(self.parent.vec_scale)
         self.ctr3.setentry(self.parent.vec_ref)
        

class continents_attributes:
   def __init__(self, page, parent, re_plot_flag = 1, gm=None):
      self.parent = parent
      if type(gm) in [types.ListType, types.TupleType, types.NoneType]:
         self.gm = gm
      else:
         self.gm=[gm,]

      # Too much on this page, so I needed to add a scroll window
      scl_Frame=Pmw.ScrolledFrame(page,
                                labelpos = 'n',
                                label_text = 'Continents Editor',
                                )
      scl_Frame.pack(fill = 'both', expand = 1,side='top')
      page=scl_Frame.interior()


      self.var1 = Tkinter.StringVar()
      self.opt1 = Pmw.OptionMenu(page,
                labelpos = 'w',
                label_text = 'Continents Line Type:',
                menubutton_textvariable = self.var1,
                items = ['solid', 'dash', 'dot', 'dash-dot', 'long-dash'],
                menubutton_width = 12,
      )
      self.opt1.pack(expand=1, padx=5, pady=5)
      self.parent.balloon.bind( self.opt1, "Select the continents line type. " )

      self.ctr1 = Pmw.Counter(page,
                labelpos = 'w',
                label_text = 'Continents Line Color Index:',
                entry_background = 'white',
                entry_foreground = 'black',
                entryfield_entry_width = 5,
                entryfield_validate = {'validator' : 'integer',
                                   'min' : '0', 'max' : '255'},
                increment = 1)
      self.ctr1.pack( expand=1, padx=5, pady=5 )
      self.parent.balloon.bind( self.ctr1, "Select the continents color index. " )

      self.ctr3 = Pmw.Counter(page,
                labelpos = 'w',
                label_text = 'Continents Line Width:',
                entry_background = 'white',
                entry_foreground = 'black',
                entryfield_entry_width = 5,
                entryfield_validate = {'validator' : 'integer',
                                   'min' : '1', 'max' : '300'},
                increment = 1)
      self.ctr3.pack( expand=1, padx=5, pady=5 )
      self.parent.balloon.bind( self.ctr3, "Set the continents line width. " )

      self.ctr2 = Pmw.Counter(page,
                labelpos = 'w',
                label_text = 'Continents Type Index:',
                entry_background = 'white',
                entry_foreground = 'black',
                entryfield_entry_width = 5,
                entryfield_validate = {'validator' : 'integer',
                                   'min' : '-1', 'max' : '11'},
                increment = 1)
      self.ctr2.pack( expand=1, padx=5, pady=5 )
      self.parent.balloon.bind( self.ctr2, "Select the continents type index.Where\n-1 = 'Auto Continents';\n0 = 'No Continents';\n1 = 'Fine Continents';\n2 = 'Coarse Continents';\n3 = 'United States Continents';\n4 = 'Political Borders Continents';\n5 = 'Rivers Continents';\n6 - 11 = 'User defined Continents'" ) 

      self.reset_cont_attributes()
# No longer needed
#      self.btn1=Tkinter.Button(page,
#            text = 'Plot and Set',
#            background = gui_color.replot,
#            command = gui_control.Command(self.cont_replot, parent)
#            )
#      if re_plot_flag == 1:
#         self.btn1.pack( expand = 1, fill = 'x', padx=170, pady=5 )

      self.btn2=Tkinter.Button(page,
            text = 'Reset Continents Attributes',
            background = gui_color.three,
            command = self.reset_cont_attributes
            )
#      self.btn2.pack( expand = 1, padx=170, pady=5 )

   def get_settings( self ):
      if not self.gm is None:
         for g in self.gm:
            g.line = self.opt1.getcurselection()
            g.linecolor = eval(self.ctr1.get())
            g.type = eval(self.ctr2.get())
      else:
         cont_line_obj = self.parent.vcs[ self.parent.vcs_id ].getline( 'continents' )
         cont_line_obj.type = self.opt1.getcurselection()
         cont_line_obj.color = int( self.ctr1.get() )
         cont_line_obj.width = int( self.ctr3.get() )
         continent_value = int(self.ctr2.get())
         if continent_value == -1: continent_value = None
         self.parent.panelGC.evt_continents_toggle( self.parent, continent_value )

   def persistant_set( self ):
      self.parent.cont_line = self.opt1.getcurselection( )
      self.parent.cont_linecolor = self.ctr1.get( )
      self.parent.cont_linewidth = self.ctr3.get( )

   def cont_replot( self, parent ):
      self.get_settings()
      parent.panelGC.evt_plot( parent )
      #vcs_function.re_plot( parent, 0 )

   def reset_cont_attributes( self ):
      if not self.gm is None:
         if (self.gm[0].line is None):
            self.opt1.invoke('solid')
         else:
            self.opt1.invoke(self.gm[0].line)
         if self.gm[0].linecolor is None:
            self.ctr1.setentry( 241  )
         else:
            self.ctr1.setentry( self.gm[0].linecolor  )
         if self.gm[0].type is None:
            self.ctr2.setentry( -1 )
         else:
            self.ctr2.setentry( self.gm[0].type )
      else:
         if (self.parent.cont_line is None):
            self.opt1.invoke('solid')
         else:
            self.opt1.invoke(self.parent.cont_line)
         if self.parent.cont_linecolor is None:
            self.ctr1.setentry( 241  )
         else:
            self.ctr1.setentry( self.parent.cont_linecolor  )
         if self.parent.cont_linewidth is None:
            self.ctr3.setentry( 2  )
         else:
            self.ctr3.setentry( self.parent.cont_linewidth  )
         if self.parent.panelGC.cont_flg is None:
            self.ctr2.setentry( -1 )
         else:
            self.ctr2.setentry( self.parent.panelGC.cont_flg )


class outfill_attributes:
   def __init__(self, page, parent, re_plot_flag = 1,gm=None):
      self.parent = parent
      if type(gm) in [types.ListType, types.TupleType, types.NoneType]:
         self.gm = gm
      else:
         self.gm=[gm,]

      # Too much on this page, so I needed to add a scroll window
      scl_Frame=Pmw.ScrolledFrame(page,
                                labelpos = 'n',
                                label_text = 'Outfill Editor',
                                )
      scl_Frame.pack(fill = 'both', expand = 1,side='top')
      page=scl_Frame.interior()

      self.var1 = Tkinter.StringVar()
      self.opt1 = Pmw.OptionMenu(page,
                labelpos = 'w',
                label_text = 'Outfill Fill Area Style:',
                menubutton_textvariable = self.var1,
                items = ['solid', 'hatch', 'pattern', 'hallow'],
                menubutton_width = 12,
      )
      self.opt1.pack(expand=1, padx=5, pady=5)
      self.parent.balloon.bind( self.opt1, "Select the outfill fill area style type. " )

      self.ctr1 = Pmw.Counter(page,
                labelpos = 'w',
                label_text = 'Outfill Fill Area Index:',
                entry_background = 'white',
                entry_foreground = 'black',
                entryfield_entry_width = 5,
                #entryfield_value = self.parent.legend_x_position,
                entryfield_validate = {'validator' : 'integer',
                                   'min' : '1', 'max' : '20'},
                increment = 1)
      self.ctr1.pack( expand=1, padx=5, pady=5 )
      self.parent.balloon.bind( self.ctr1, "Select the outfill fill area index value. " )


      self.eny2 = Pmw.Counter(page,
                labelpos = 'w',
                label_text = 'Outfill Fill Area Color Index:',
                entry_background = 'white',
                entry_foreground = 'black',
                entryfield_entry_width = 5,
                entryfield_validate = {'validator' : 'integer',
                                   'min' : '0', 'max' : '255'},
                increment = 1)
      self.eny2.pack( expand = 1, padx=20, pady=5 )
      self.parent.balloon.bind( self.eny2, "Enter the fillarea color index value. There can only\nbe one color index value (ranging from 0 to 255).\nIf an error in the color index value occurs, then the\ndefault color value index (i.e., 241) will be used.")

      self.eny1=Pmw.EntryField(page,
            labelpos = 'w',
            label_text = 'Outfill Index Values:',
            entry_background = 'white',
            entry_foreground = 'black',
            #entryfield_value = repr(self.parent.outf_outfill),
            entry_width =  47,
            )
      self.eny1.pack( expand = 1, fill = 'x', padx=20, pady=5 )
      self.parent.balloon.bind( self.eny1, "Outlines are filled to enclose the selected values\nthat appear in the data array. As few as one, or\nas many as ten values, can be specified:\noutline=([n1,[n2,[n3,...[n10]...]]])." )

      entries = (self.eny1, self.eny2 )
      Pmw.alignlabels(entries)

      self.reset_outf_attributes()
# No longer needed
#      self.btn1=Tkinter.Button(page,
#            text = 'Plot and Set',
#            background = gui_color.replot,
#            command = gui_control.Command(self.outf_replot, parent)
#            )
#      if re_plot_flag == 1:
#         self.btn1.pack( expand = 1, fill = 'x', padx=170, pady=5 )

      self.btn2=Tkinter.Button(page,
            text = 'Reset Outfill Attributes',
            background = gui_color.three,
            command = self.reset_outf_attributes
            )
#      self.btn2.pack( expand = 1, padx=170, pady=5 )

   def get_settings( self ):
      if not self.gm is None:
         for g in self.gm:
            g.fillareastyle = self.opt1.getcurselection()
            g.fillareaindex = eval(self.ctr1.get())
            g.fillareacolor = eval(self.eny2.get())
            g.outfill = eval(self.eny1.get())
      else:
         self.parent.outf_style=self.opt1.getcurselection()
         self.parent.outf_index=self.ctr1.get()
         try:
            self.parent.outf_fillcolor=eval(self.eny2.get())
            if self.parent.outf_fillcolor not in range(0, 256):
               self.parent.outf_fillcolor=241
         except:
            self.parent.outf_fillcolor=241
         if self.eny1.get( ) != '':
            xtr = string.replace( self.eny1.get( ), ' ', ',' )
            while string.find(xtr,',,',0) != -1: xtr=string.replace(xtr,',,',',')
            if xtr[-1] == ',': xtr = xtr[:-1]
            self.parent.outf_outfill = string.split(xtr, ',')
            for i in range(len(self.parent.outf_outfill)):
               self.parent.outf_outfill[i] = string.atof(self.parent.outf_outfill[i])
               
   def outf_replot( self, parent ):
      self.get_settings()
      parent.panelGC.evt_plot( parent )
      #vcs_function.re_plot( parent, 0 )
      self.reset_outf_attributes();

   def reset_outf_attributes( self ):
      if not self.gm is None:
         if self.gm[0].fillareastyle is None:
            self.opt1.invoke( 'solid' )
         else:
            self.opt1.invoke(self.gm[0].fillareastyle)
         if self.gm[0].fillareaindex is None:
            self.ctr1.setentry(1)
         else:
            self.ctr1.setentry(self.gm[0].fillareaindex)
         if self.gm[0].fillareacolor is not None:
            self.eny2.setentry(repr(self.gm[0].fillareacolor))
         else:
            self.eny2.setentry(repr(241))
         if self.gm[0].outfill is not None:
            self.eny1.setentry(repr(self.gm[0].outfill))
         else:
            self.eny1.setentry(repr(1))
      else:
         if (self.parent.outf_style is None):
            self.opt1.invoke( 'solid' )
         else:
            self.opt1.invoke( self.parent.outf_style )
         if self.parent.outf_index is None:
            self.ctr1.setentry(1)
         else:
            self.ctr1.setentry(self.parent.outf_index)
         if self.parent.outf_fillcolor is not None:
            self.eny2.setentry(repr(self.parent.outf_fillcolor))
         else:
            self.eny2.setentry(repr(241))
         if self.parent.outf_outfill is not None:
            self.eny1.setentry( tuple( self.parent.outf_outfill ) )

   def outfill_clear( self ):
      self.eny1.clear( )
      self.eny2.clear( )
      
class outline_attributes:
   def __init__(self, page, parent, re_plot_flag = 1, gm=None):
      self.parent = parent
      if type(gm) in [types.ListType, types.TupleType, types.NoneType]:
         self.gm = gm
      else:
         self.gm=[gm,]

      # Too much on this page, so I needed to add a scroll window
      scl_Frame=Pmw.ScrolledFrame(page,
                                labelpos = 'n',
                                label_text = 'Outline Editor',
                                )
      scl_Frame.pack(fill = 'both', expand = 1,side='top')
      page=scl_Frame.interior()

      self.var1 = Tkinter.StringVar()
      self.opt1 = Pmw.OptionMenu(page,
                labelpos = 'w',
                label_text = 'Outline Line Type:',
                menubutton_textvariable = self.var1,
                items = ['solid', 'dash', 'dot', 'dash-dot', 'long-dash'],
                menubutton_width = 12,
      )
      self.opt1.pack(expand=1, padx=5, pady=5)
      self.parent.balloon.bind( self.opt1, "Select the outline line type. " )

      self.eny2 = Pmw.Counter(page,
                labelpos = 'w',
                label_text = 'Outline Line Color Index:',
                entry_background = 'white',
                entry_foreground = 'black',
                entryfield_entry_width = 5,
                entryfield_validate = {'validator' : 'integer',
                                   'min' : '0', 'max' : '255'},
                increment = 1)
      self.eny2.pack( expand = 1, padx=20, pady=5 )
      self.parent.balloon.bind( self.eny2, "Enter the line color index value. There can only\nbe one color index value (ranging from 0 to 255).\nIf an error in the color index value occurs, then the\ndefault color value index (i.e., 241) will be used.")

      self.eny1=Pmw.EntryField(page,
            labelpos = 'w',
            label_text = 'Outline Index Values:',
            entry_background = 'white',
            entry_foreground = 'black',
            entry_width =  47
            )
      self.eny1.pack( expand = 1, padx=20, pady=5 )
      self.parent.balloon.bind( self.eny1, "Outlines are drawn to enclose the specified values\nin the data array. As few as one, or\nas many as\nten values, can be specified:\noutline=([n1,[n2,[n3,...[n10]...]]])." )

      entries = (self.eny1, self.eny2 )
      Pmw.alignlabels(entries)

      self.reset_outl_attributes()
# No longer needed
#      self.btn1=Tkinter.Button(page,
#            text = 'Plot and Set',
#            background = gui_color.replot,
#            command = gui_control.Command(self.outl_replot, parent)
#            )
#      if re_plot_flag == 1:
#         self.btn1.pack( expand = 1, fill = 'x', padx=170, pady=5 )

      self.btn2=Tkinter.Button(page,
            text = 'Reset Outline Attributes',
            background = gui_color.three,
            command = self.reset_outl_attributes
            )
#      self.btn2.pack( expand = 1, padx=170, pady=5 )

   def get_settings( self ):
      if not self.gm is None:
         for g in self.gm:
            g.line = self.opt1.getcurselection()
            g.linecolor = eval(self.eny2.get())
            g.outline = eval(self.eny1.get())
      else:
         self.parent.outl_line=self.opt1.getcurselection()
         try:
            self.parent.outl_linecolor=eval(self.eny2.get())
            if self.parent.outl_linecolor not in range(0, 256):
               self.parent.outl_linecolor=241
         except:
            self.parent.outl_linecolor=241
         if self.eny1.get( ) != '':
            xtr = string.replace( self.eny1.get( ), ' ', ',' )
            while string.find(xtr,',,',0) != -1: xtr=string.replace(xtr,',,',',')
            if xtr[-1] == ',': xtr = xtr[:-1]
            self.parent.outl_outline = string.split(xtr, ',')
            for i in range(len(self.parent.outl_outline)):
               self.parent.outl_outline[i] = string.atof(self.parent.outl_outline[i])

   def outl_replot( self, parent ):
      self.get_settings()
      parent.panelGC.evt_plot( parent )
      #vcs_function.re_plot( parent, 0 )
      self.reset_outl_attributes();

   def reset_outl_attributes( self ):
      if not self.gm is None:
         if (self.gm[0].line is None):
            self.opt1.invoke('solid')
         else:
            self.opt1.invoke(self.gm[0].line)
         if self.gm[0].linecolor is not None:
            self.eny2.setentry(repr(self.gm[0].linecolor))
         else:
            self.eny2.setentry(repr(241))

         self.eny1.setentry(repr(self.gm[0].outline))
      else:
         if (self.parent.outl_line is None):
            self.opt1.invoke('solid')
         else:
            self.opt1.invoke(self.parent.outl_line)
         if self.parent.outl_linecolor is not None:
            self.eny2.setentry(repr(self.parent.outl_linecolor))
         else:
            self.eny2.setentry(repr(241))
         if self.parent.outl_outline is not None:
            self.eny1.setentry(tuple(self.parent.outl_outline))

   def outline_clear( self ):
      self.eny1.clear( )
      self.eny2.clear( )
      
         
class scatter_attributes:
   def __init__(self, page, parent, re_plot_flag = 1, gm=None):
      self.parent = parent
      if type(gm) in [types.ListType, types.TupleType, types.NoneType]:
         self.gm = gm
      else:
         self.gm=[gm,]

      # Too much on this page, so I needed to add a scroll window
      scl_Frame=Pmw.ScrolledFrame(page,
                                labelpos = 'n',
                                label_text = 'Scatter Editor',
                                )
      scl_Frame.pack(fill = 'both', expand = 1,side='top')
      page=scl_Frame.interior()

      self.var1 = Tkinter.StringVar()
      self.opt1 = Pmw.OptionMenu(page,
                labelpos = 'w',
                label_text = 'Scatter Marker Type:',
                menubutton_textvariable = self.var1,
                items = ["dot", "plus", "star", "circle", "cross", "diamond", "triangle_up", "triangle_down", "triangle_left", "triangle_right", "square", "diamond_fill", "triangle_up_fill", "triangle_down_fill", "triangle_left_fill", "triangle_right_fill", "square_fill"],
                menubutton_width = 12,
      )
      self.opt1.pack(expand=1, padx=5, pady=5)
      self.parent.balloon.bind( self.opt1, "Select the scatter marker type. " )

      self.ctr1 = Pmw.Counter(page,
                labelpos = 'w',
                label_text = 'Scatter Marker Color Index:',
                entry_background = 'white',
                entry_foreground = 'black',
                entryfield_entry_width = 5,
                entryfield_validate = {'validator' : 'integer',
                                   'min' : '0', 'max' : '255'},
                increment = 1)
      self.ctr1.pack( expand=1, padx=5, pady=5 )
      self.parent.balloon.bind( self.ctr1, "Select the scatter marker color index. " )

      self.ctr2 = Pmw.Counter(page,
                labelpos = 'w',
                label_text = 'Scatter Marker Size:',
                entry_background = 'white',
                entry_foreground = 'black',
                entryfield_entry_width = 5,
                entryfield_validate = {'validator' : 'integer',
                                   'min' : '1', 'max' : '300'},
                increment = 1)
      self.ctr2.pack( expand=1, padx=5, pady=5 )
      self.parent.balloon.bind( self.ctr1, "Select the scatter marker size. " )

      self.reset_scat_attributes()
# No longer needed
#      self.btn1=Tkinter.Button(page,
#            text = 'Plot and Set',
#            background = gui_color.replot,
#            command = gui_control.Command(self.scat_replot, parent)
#            )
#      if re_plot_flag == 1:
#         self.btn1.pack( expand = 1, fill = 'x', padx=170, pady=5 )

      self.btn2=Tkinter.Button(page,
            text = 'Reset Scatter Attributes',
            background = gui_color.three,
            command = self.reset_scat_attributes
            )
#      self.btn2.pack( expand = 1, padx=170, pady=5 )

   def get_settings( self ):
      if not self.gm is None:
         for g in self.gm:
            g.marker = self.opt1.getcurselection()
            g.markercolor = eval(self.ctr1.get())
            g.markersize = eval(self.ctr2.get())
      else:
         self.parent.scat_marker=self.opt1.getcurselection()
         self.parent.scat_markercolor=eval(self.ctr1.get())
         self.parent.scat_markersize=eval(self.ctr2.get())


   def scat_replot( self, parent ):
      self.get_settings()
      #print ' scatter settings = ', parent.scat_marker, parent.scat_markercolor, parent.scat_markersize
      parent.panelGC.evt_plot( parent )
      #vcs_function.re_plot( parent, 0 )

   def reset_scat_attributes( self ):
      if not self.gm is None:
         if (self.gm[0].marker is None):
            self.opt1.invoke( 'dot' )
         else:
            self.opt1.invoke(self.gm[0].marker)

         if self.gm[0].markercolor is None:
            self.ctr1.setentry( repr(241) )
         else:
            self.ctr1.setentry( self.gm[0].markercolor )

         if self.gm[0].markersize is None:
            self.ctr2.setentry( 3 )
         else:
            self.ctr2.setentry( self.gm[0].markersize )
      else:
         if (self.parent.scat_marker is None):
            self.opt1.invoke( 'dot' )
         else:
            self.opt1.invoke(self.parent.scat_marker)

         if self.parent.scat_markercolor is None:
            self.ctr1.setentry( repr(241) )
         else:
            self.ctr1.setentry( repr(self.parent.scat_markercolor))

         if self.parent.scat_markersize is None:
            self.ctr2.setentry( repr(3) )
         else:
            self.ctr2.setentry( repr(self.parent.scat_markersize) )



#---------------------------------------------------------------------
#
# End set graphics methods popup dialog
#
#---------------------------------------------------------------------
def return_float_None( val ):
   try:
      return string.atof( val )
   except:
      return None

def return_int_None( val ):
   try:
      return string.atoi( val )
   except:
      return None

#
#---------------------------------------------------------------------
# End of File
#---------------------------------------------------------------------



