#!/usr/bin/env python
#
# The VCS Graphics Method GUI controls -  graphicsmethodgui module
#
###############################################################################
#                                                                             #
# Module:       graphicsmethodgui module                                      #
#                                                                             #
# Copyright:    "See file Legal.htm for copyright information."               #
#                                                                             #
# Authors:      PCMDI Software Team                                           #
#               Lawrence Livermore NationalLaboratory:                        #
#               support@pcmdi.llnl.gov                                        #
#                                                                             #
# Description:  PCMDI's VCS GUI graphics method editor. Tkinter version.      #
#                                                                             #
# Version:      4.0                                                           #
#                                                                             #
###############################################################################

#---------------------------------------------------------------------
# NOTE: need to use version of Python that imports Tkinter and Pmw
#---------------------------------------------------------------------
import Tkinter, Pmw, tkFileDialog
from tkMessageBox import showerror
import vcs_legacy
import os, string, sys
from queries import graphicsmethodlist
import browser
from browser import gui_message
from gui_support import gui_color
from error import vcs_legacyError
import gui_support
from browser import gui_control
import projectiongui
#
# Create the Tkinter/Pmw Colormap editor interface
#
_entry_width = 44

class GraphicsMethodGui:
    def __init__( self, vcs_legacy=None, gm_type='boxfill', gm_name='default',
                 gui_parent=None):
        if vcs_legacy is None:
            import Canvas
            vcs_legacy = Canvas.Canvas()
           
        gm_type = string.lower( gm_type )
        self.gm_name = gm_name
        self.gm_type = gm_type
        if ( gm_type not in graphicsmethodlist() ):
           str = "The graphics method type (%s) does not exist!" % (gm_type)
           showerror( "Error Message to User", str )
        elif ( gm_name not in vcs_legacy.listelements( gm_type ) ):
           str = "The %s graphics method name (%s) does not exist!" % (gm_type2, gm_name)
           showerror( "Error Message to User", str )
   
        gm_type2 = "%s%s" %( string.upper( gm_type[0] ), gm_type[1:] )
        title = "VCS's Graphics Method Editor: Editing -- %s: %s" % \
                                                 ( gm_type2, gm_name ) 
        self.preview_flg = 0
        self.root = gui_support.root()
        self.dialog = gui_support.VcsDialog (title=title,
                          buttons=('Preview', 'Reset', 'Save', 'Cancel'),
#                          buttons=('Preview', 'Reset', 'Clear', 'Save', 'Cancel'),
                          defaultbutton='Save',
                          command = G_Command(self.execute, gui_parent)
                      )
        self.dialog.dialog.withdraw()
        if gui_parent is None:
           gui_parent_updated=self.dialog
        else:
           gui_parent_updated=gui_parent
        parent = self.dialog.interior()
        self.parent = parent
        self.vcs_legacy = vcs_legacy
        # initialize the color index list and the copy buffer list
        self.color_indices = []
        self.copy_buffer = []

        # create the graphicsmethod main toplevel menu
        self.cmain_menu = Pmw.MenuBar(parent,
                hull_relief = 'raised',
                hull_borderwidth = 2,
                balloon = gui_support.balloon
                )
        self.cmain_menu.pack(side='top', fill='both')

        #-------------------------------------------
        # menu 1 -- 'File'
        #-------------------------------------------
        create_graphicsmethod_file_menu( self.cmain_menu, self, gui_parent)

        #-------------------------------------------
        # menu 4 -- 'Help'
        #-------------------------------------------
        create_graphicsmethod_help_menu( self.cmain_menu, self, parent)

        #----------------------------------------------------------------------
        # Create and pack the noteBook
        #----------------------------------------------------------------------
        notebook = Pmw.NoteBook( parent )
        notebook.pack(fill = 'both', expand = 1, padx = 5, pady = 5)
        #----------------------------------------------------------------------
        # Add the "Graphics Method Settings" page to the notebook.
        #----------------------------------------------------------------------
        if (gm_type == 'boxfill'):
           page = notebook.add('Boxfill Settings')
           notebook.tab('Boxfill Settings').focus_set()
           gui_support.balloon.bind( notebook.tab('Boxfill Settings'), 'The Boxfill graphics method displays a two-dimensional data\narray by surrounding each data value with a colored grid box.' )
##            gm = self.set_gm_entries(gm_type, gm_name, vcs_legacy )
           gm = vcs_legacy.getboxfill(gm_name)
           self.GM=self.boxfill_attributes = browser.gui_set_graphics_methods.boxfill_attributes( page, gui_parent_updated, re_plot_flag=0,gm=gm )
        elif (gm_type in [ 'isofill', 'isoline' ]):
           if gm_type == 'isofill': contour_type = 'Isofill Settings'
           elif gm_type == 'isoline': contour_type = 'Isoline Settings'
           page = notebook.add(contour_type)
           notebook.tab(contour_type).focus_set()
           if gm_type == 'isofill':
              gui_support.balloon.bind( notebook.tab(contour_type), "The Isofill graphics method fills the area between\nselected isolevels (levels of constant value) of a\ntwo-dimensional array; the manner of filling the\narea is determined by the named fill area attributes." )
              gm = vcs_legacy.getisofill(gm_name)
           elif gm_type == 'isoline':
              gui_support.balloon.bind( notebook.tab(contour_type), "The Isoline graphics method draws lines of constant\nvalue at specified levels to graphically represent\nthe values of a two-dimensional array; labels also\ncan be displayed on the isolines." )
              gm = vcs_legacy.getisoline(gm_name)
##            gm = self.set_gm_entries(gm_type, gm_name, vcs_legacy )
           self.GM=self.isofill_attributes = browser.gui_set_graphics_methods.contour_levels( page, gui_parent_updated, re_plot_flag=0, gm=gm )
        elif (gm_type in [ 'meshfill', ]):
           page = notebook.add('Meshfill Settings')
           notebook.tab('Meshfill Settings').focus_set()
           gui_support.balloon.bind( notebook.tab('Meshfill Settings'), "The Meshfill graphics method draws data on irregular grid (or 'mesh')at specified levels to graphically represent\nthe values of a one-dimensional array;\nUnless the irregular grid is supported by cdms, a mesh array must be passed as well" )
           gm = vcs_legacy.getmeshfill(gm_name)
           self.GM=self.meshfill_attributes = browser.gui_set_graphics_methods.meshfill_attributes( page, gui_parent_updated, re_plot_flag=0, gm=gm )
        elif (gm_type in [ 'outfill', 'outline' ]):
           if gm_type == 'outfill':
               contour_type = 'Outfill Settings'
               gm=vcs_legacy.getoutfill(gm_name)
           elif gm_type == 'outline':
               contour_type = 'Outline Settings'
               gm=vcs_legacy.getoutline(gm_name)
           page = notebook.add(contour_type)
           notebook.tab(contour_type).focus_set()
           if gm_type == 'outfill':
              gui_support.balloon.bind( notebook.tab(contour_type), "The primary purpose of the Outfill graphics method\nis to display filled continents or sea ice using\na surface type array that indicates land, ocean,\nand sea ice points. In general, however, this\ngraphics method can be used to fill a set of integer\nvalues for any array. " )
##               gm = self.set_gm_entries(gm_type, gm_name, vcs_legacy )
              self.GM=self.outfill_attributes=browser.gui_set_graphics_methods.outfill_attributes( page, gui_parent_updated, re_plot_flag=0,gm=gm )
           elif gm_type == 'outline':
              gui_support.balloon.bind( notebook.tab(contour_type), "The primary purpose of the Outline graphics method\nis to display outlined continents or sea ice using\na surface type array that indicates land, ocean,\nand sea ice points. In general, however, this\ngraphics method can be used to outline a set of\ninteger values for any array. " )
##               gm = self.set_gm_entries(gm_type, gm_name, vcs_legacy )
              self.GM=self.outline_attributes=browser.gui_set_graphics_methods.outline_attributes( page, gui_parent_updated, re_plot_flag=0, gm=gm )
        elif (gm_type in ['xvsy', 'xyvsy', 'yxvsx'] ):
           if gm_type == 'xvsy':
               oneD_type = 'XvsY Settings'
               gm=vcs_legacy.getxvsy(gm_name)
           elif gm_type == 'xyvsy':
               oneD_type = 'Xyvsy Settings'
               gm=vcs_legacy.getxyvsy(gm_name)
           elif gm_type == 'yxvsx':
               oneD_type = 'Yxvsx Settings'
               gm=vcs_legacy.getyxvsx(gm_name)
           page = notebook.add(oneD_type)
           notebook.tab(oneD_type).focus_set()
           if gm_type == 'xvsy':
              gui_support.balloon.bind( notebook.tab(oneD_type), "The XvsY graphics method displays a line plot from\ntwo 1D data arrays, that is X(t) and Y(t), where t\nrepresents the 1D coordinate values." )
           elif gm_type == 'xyvsy':
              gui_support.balloon.bind( notebook.tab(oneD_type), "The Xyvsy graphics method displays a line plot from\na 1D data array, that is X(y), where y represents\nthe 1D coordinate values." )
           elif gm_type == 'yxvsx':
              gui_support.balloon.bind( notebook.tab(oneD_type), "The Yxvsx graphics method displays a line plot from\na 1D data array, that is Y(x), where y represents\nthe 1D coordinate values." )

##            gm = self.set_gm_entries(gm_type, gm_name, vcs_legacy )
           self.GM=self.oneD_attributes = browser.gui_set_graphics_methods.oneD_attributes( page, gui_parent_updated, re_plot_flag=0, gm=gm )
        elif (gm_type == 'scatter'):
           page = notebook.add('Scatter Settings')
           notebook.tab('Scatter Settings').focus_set()
           gui_support.balloon.bind( notebook.tab('Scatter Settings'), "The Scatter graphics method displays a a scatter\nplot of two data arrays A(x, y, z, t) and B(x, y,\nz, t). " )
           gm=vcs_legacy.getscatter(gm_name)
##            gm = self.set_gm_entries(gm_type, gm_name, vcs_legacy )
           self.GM=self.scatter_attributes=browser.gui_set_graphics_methods.scatter_attributes( page, gui_parent_updated, re_plot_flag=0 , gm=gm)
        elif (gm_type == 'taylordiagram'):
           page = notebook.add('Taylordiagram Settings')
           notebook.tab('Taylordiagram Settings').focus_set()
           gui_support.balloon.bind( notebook.tab('Taylordiagram Settings'), "The Taylor diagrams provide a way of graphically\nsummarizing how well patterns match each other in terms of their correlation, their root-mean-square\ndifference and the amplitude of their variations\n(as quantified by their standard deviations)." )
           gm=vcs_legacy.gettaylordiagram(gm_name)
##            gm = self.set_gm_entries(gm_type, gm_name, vcs_legacy )
           self.GM=self.taylordiagram_attributes = browser.gui_set_graphics_methods.taylor_attributes( page, vcs_legacy, vcs_legacy=vcs_legacy, gm_name=gm.name )
        elif (gm_type == 'vector'):
           page = notebook.add('Vector Settings')
           notebook.tab('Vector Settings').focus_set()
           gui_support.balloon.bind( notebook.tab('Vector Settings'), "The Vector graphics method displays a vector plot\nof a two-dimensional field. Vectors are located at\nthe coordinate locations and point in the direction\nof the data vector field. Vector magnitudes are the\nproduct of data vector field lengths and a scaling\nfactor." )
           gm=vcs_legacy.getvector(gm_name)
##            gm = self.set_gm_entries(gm_type, gm_name, vcs_legacy )
           self.GM=self.vector_attributes = browser.gui_set_graphics_methods.vector_attributes( page, gui_parent_updated, re_plot_flag=0, gm=gm )
        elif (gm_type == 'continents'):
           page = notebook.add('Continents Settings')
           notebook.tab('Continents Settings').focus_set()
           gui_support.balloon.bind( notebook.tab('Continents Settings'), "The primary purpose of the Continents graphics method\nis to display a predefined, generic set of continental\noutlines in a longitude x latitude space." )
           gm=vcs_legacy.getcontinents(gm_name)
##            gm = self.set_gm_entries(gm_type, gm_name, vcs_legacy )
           self.GM=self.continents_attributes = browser.gui_set_graphics_methods.continents_attributes( page, gui_parent_updated, re_plot_flag=0 , gm=gm)

        #----------------------------------------------------------------------
        # Add the "Common Attribute Settings" page to the notebook.
        #----------------------------------------------------------------------
        if gm_type != 'taylordiagram':
           page = notebook.add('X Ticks and Labels')
           gui_support.balloon.bind( notebook.tab('X Ticks and Labels'), 'The X labels and tick marks are defined by a VCS list. Each\nlist value defines a position with respect to the real-world\ndimension coordinates and a string for display.' )
           self.x_attributes = x_attributes( page, gui_parent_updated, gm, vcs_legacy )

           page = notebook.add('Y Ticks and Labels')
           gui_support.balloon.bind( notebook.tab('Y Ticks and Labels'), 'The Y labels and tick marks are defined by a VCS list. Each\nlist value defines a position with respect to the real-world\ndimension coordinates and a string for display.' )
           self.y_attributes = y_attributes( page, gui_parent_updated, gm, vcs_legacy )

           page = notebook.add('World Coordinates')
           gui_support.balloon.bind( notebook.tab('World Coordinates'), "The data space defined in the VCS picture template, expressed\nin normalized device coordinates, is mapped to the real-world\ncoordinates given in 'datawc'." )
           self.wc_attributes = wc_attributes( page, gui_parent_updated, gm )

           page = notebook.add('Projection and Axis')
           gui_support.balloon.bind( notebook.tab('Projection and Axis'), "Change, Alter or Create VCS Projection\nAlso, the X- and Y-Axis representation can be altered." )
           notebook.setnaturalsize()

           self.pa_attributes = pa_attributes( page, gui_parent_updated, gm, gm_type, vcs_legacy, gui_parent )
        
        # Hold the original value settings. This is needed for Reset and Cancel
        self.gm = gm
        self.hold_gm_original_settings( gui_parent )

        # Decide where to put it on the screen
        if gui_parent is None:
            d=[self.dialog.dialog.winfo_screenwidth()/4,
               self.dialog.dialog.winfo_screenheight()/4
              ]
            self.dialog.geometry("%sx%s+%s+%s"% (700,600,d[0],d[1]))
        else:
            g=gui_parent.geometry()
            d = g.split('+')[1:]
            e = g.split('+')[0].split('x')
            self.dialog.geometry("%sx%s+%s+%s"% (e[0],int(string.atoi(e[1])*.65),d[0],d[1]))
            
        if gui_parent is not None:
           self.dialog.dialog.transient(gui_parent)
        
        self.dialog.dialog.deiconify()


    def hold_gm_original_settings ( self, gui_parent ):
        # Store current values
        if self.gm_type == 'boxfill':
            #if gui_parent is not None: self.store_boxfill_type= self.gm.boxfill_type
            self.store_boxfill_type= self.gm.boxfill_type
            self.store_gm_level_1 = self.gm.level_1
            self.store_gm_level_2 = self.gm.level_2
            self.store_gm_levels = self.gm.levels
            self.store_gm_color_1 = self.gm.color_1
            self.store_gm_color_2 = self.gm.color_2
            self.store_gm_fillareacolors = self.gm.fillareacolors
            self.store_gm_missing = self.gm.missing
            self.store_gm_legend = self.gm.legend
            self.store_gm_ext_1 = self.gm.ext_1
            self.store_gm_ext_2 = self.gm.ext_2
        elif self.gm_type == 'meshfill':
            self.store_gm_missing = self.gm.missing
            self.store_gm_ext_1 = self.gm.ext_1
            self.store_gm_ext_2 = self.gm.ext_2
            self.store_gm_fillareastyle = self.gm.fillareastyle
            self.store_gm_fillareaindices = self.gm.fillareaindices
            self.store_gm_fillareacolors = self.gm.fillareacolors
            self.store_gm_levels = self.gm.levels
            self.store_gm_legend = self.gm.legend
            self.store_gm_wrap = self.gm.wrap
            self.store_gm_mesh = self.gm.mesh
        elif self.gm_type == 'isofill':
            self.store_gm_missing = self.gm.missing
            self.store_gm_ext_1 = self.gm.ext_1
            self.store_gm_ext_2 = self.gm.ext_2
            self.store_gm_fillareastyle = self.gm.fillareastyle
            self.store_gm_fillareaindices = self.gm.fillareaindices
            self.store_gm_fillareacolors = self.gm.fillareacolors
            self.store_gm_levels = self.gm.levels
            self.store_gm_legend = self.gm.legend
        elif self.gm_type == 'isoline':
            self.store_gm_label = self.gm.label
            self.store_gm_line = self.gm.line
            self.store_gm_linecolors = self.gm.linecolors
            self.store_gm_linewidths = self.gm.linewidths
            self.store_gm_text = self.gm.text
            self.store_gm_textcolors = self.gm.textcolors
            self.store_gm_level = self.gm.level
        elif self.gm_type == 'outfill':
            self.store_gm_fillareastyle = self.gm.fillareastyle
            self.store_gm_fillareaindex = self.gm.fillareaindex
            self.store_gm_fillareacolor = self.gm.fillareacolor
            self.store_gm_outfill = self.gm.outfill
        elif self.gm_type == 'outline':
            self.store_gm_line = self.gm.line
            self.store_gm_linecolor = self.gm.linecolor
            self.store_gm_outline = self.gm.outline
        elif self.gm_type == 'vector':
            self.store_gm_line = self.gm.line
            self.store_gm_linecolor = self.gm.linecolor
            self.store_gm_scale = self.gm.scale
            self.store_gm_alignment = self.gm.alignment
            self.store_gm_type = self.gm.type
            self.store_gm_reference = self.gm.reference
        elif self.gm_type == 'scatter':
            self.store_gm_marker = self.gm.marker
            self.store_gm_markercolor = self.gm.markercolor
            self.store_gm_markersize = self.gm.markersize
        elif self.gm_type in ['xvsy', 'xyvsy', 'yxvsx']:
            self.store_gm_line = self.gm.line
            self.store_gm_linecolor = self.gm.linecolor
            self.store_gm_marker = self.gm.marker
            self.store_gm_markercolor = self.gm.markercolor
            self.store_gm_markersize = self.gm.markersize
        elif self.gm_type == 'taylordiagram':
            self.store_gm_detail = self.gm.detail
            self.store_gm_max = self.gm.max
            self.store_gm_quadrans = self.gm.quadrans
            self.store_gm_skillValues = self.gm.skillValues
            self.store_gm_skillColor = self.gm.skillColor
            self.store_gm_skillDrawLabels = self.gm.skillDrawLabels
            self.store_gm_skillCoefficient = self.gm.skillCoefficient
            self.store_gm_referencevalue = self.gm.referencevalue
            self.store_gm_arrowlength = self.gm.arrowlength
            self.store_gm_arrowangle = self.gm.arrowangle
            self.store_gm_arrowbase = self.gm.arrowbase
            self.store_gm_marker_status = self.gm.Marker.status
            self.store_gm_marker_line = self.gm.Marker.line
            self.store_gm_marker_id = self.gm.Marker.id
            self.store_gm_marker_id_size = self.gm.Marker.id_size
            self.store_gm_marker_id_color = self.gm.Marker.id_color
            self.store_gm_marker_id_font = self.gm.Marker.id_font
            self.store_gm_marker_symbol = self.gm.Marker.symbol
            self.store_gm_marker_color = self.gm.Marker.color
            self.store_gm_marker_size = self.gm.Marker.size
            self.store_gm_marker_xoffset = self.gm.Marker.xoffset
            self.store_gm_marker_yoffset = self.gm.Marker.yoffset
            self.store_gm_marker_line_color = self.gm.Marker.line_color
            self.store_gm_marker_line_size = self.gm.Marker.line_size
            self.store_gm_marker_line_type = self.gm.Marker.line_type

##         # Store the "Common Attribute Settings" for X ticks and labels
##         self.store_gm_xticlabels1 = self.gm.xticlabels1
##         self.store_gm_xticlabels2 = self.gm.xticlabels2
##         self.store_gm_xmtics1 = self.gm.xmtics1
##         self.store_gm_xmtics2 = self.gm.xmtics2

        if self.gm_type == 'taylordiagram': return
        # Store the "Common Attribute Settings" for X ticks and labels
        self.store_gm_xticlabels1 = self.gm.xticlabels1
        self.store_gm_xticlabels2 = self.gm.xticlabels2
        self.store_gm_xmtics1 = self.gm.xmtics1
        self.store_gm_xmtics2 = self.gm.xmtics2

        # Store the "Common Attribute Settings" for Y ticks and labels
        self.store_gm_yticlabels1 = self.gm.yticlabels1
        self.store_gm_yticlabels2 = self.gm.yticlabels2
        self.store_gm_ymtics1 = self.gm.ymtics1
        self.store_gm_ymtics2 = self.gm.ymtics2

        # Store the "Common Attribute Settings" for World Coordinates
        self.store_gm_datawc_x1 = self.gm.datawc_x1
        self.store_gm_datawc_y1 = self.gm.datawc_y1
        self.store_gm_datawc_x2 = self.gm.datawc_x2
        self.store_gm_datawc_y2 = self.gm.datawc_y2

        # Store the "Common Attribute Settings" for Projection and Axis
        self.store_gm_projection = self.gm.projection
        if self.gm_type != 'continents':
           if self.gm_type != 'xyvsy':
              self.store_gm_xaxisconvert = self.gm.xaxisconvert
           if self.gm_type != 'yxvsx':
              self.store_gm_xaxisconvert = self.gm.yaxisconvert

    def gm_reset ( self, gui_parent ):
        # Restore current values
        if self.gm_type == 'boxfill':
            #if gui_parent is not None: self.gm.boxfill_type = self.store_boxfill_type
            self.gm.boxfill_type = self.store_boxfill_type
            self.GM.gen_type.invoke( self.gm.boxfill_type )
            self.gm.level_1 = self.store_gm_level_1
            self.gm.level_2 = self.store_gm_level_2
            self.gm.levels = self.store_gm_levels
            self.gm.color_1 = self.store_gm_color_1
            self.gm.color_2 = self.store_gm_color_2
            self.gm.fillareacolors = self.store_gm_fillareacolors
            self.gm.missing = self.store_gm_missing
            self.gm.legend = self.store_gm_legend
            self.gm.ext_1 = self.store_gm_ext_1
            self.gm.ext_2 = self.store_gm_ext_2
            self.GM.reset_boxf_attributes( )
        elif self.gm_type == 'meshfill':
            self.gm.ext_1 = self.store_gm_ext_1
            self.gm.ext_2 = self.store_gm_ext_2
            self.gm.fillareastyle = self.store_gm_fillareastyle
            self.gm.fillareaindices = self.store_gm_fillareaindices
            self.gm.fillareacolors = self.store_gm_fillareacolors
            self.gm.wrap=self.store_gm_wrap
            self.gm.mesh=self.store_gm_mesh
            self.gm.levels = self.store_gm_levels
            self.gm.legend = self.store_gm_legend
            self.GM.reset_meshf_attributes( )
        elif self.gm_type == 'isofill':
            try: self.gm.missing = self.store_gm_missing
            except: pass
            self.gm.ext_1 = self.store_gm_ext_1
            self.gm.ext_2 = self.store_gm_ext_2
            self.gm.fillareastyle = self.store_gm_fillareastyle
            self.gm.fillareaindices = self.store_gm_fillareaindices
            self.gm.fillareacolors = self.store_gm_fillareacolors
            self.gm.levels = self.store_gm_levels
            self.gm.legend = self.store_gm_legend
            self.GM.contour_clear( )
            self.GM.reset_contour_attributes( )
        elif self.gm_type == 'isoline':
            self.gm.label = self.store_gm_label
            self.gm.line = self.store_gm_line
            self.gm.linecolors = self.store_gm_linecolors
            self.gm.linewidths = self.store_gm_linewidths
            self.gm.text = self.store_gm_text
            self.gm.textcolors = self.store_gm_textcolors
            self.gm.level = self.store_gm_level
            self.GM.contour_clear( )
            self.GM.reset_contour_attributes( )
        elif self.gm_type == 'outfill':
            self.gm.fillareastyle = self.store_gm_fillareastyle
            self.gm.fillareaindex = self.store_gm_fillareaindex
            self.gm.fillareacolor = self.store_gm_fillareacolor
            self.gm.outfill = self.store_gm_outfill
            self.GM.outfill_clear( )
            self.GM.reset_outf_attributes( )
        elif self.gm_type == 'outline':
            self.gm.line = self.store_gm_line
            self.gm.linecolor = self.store_gm_linecolor
            self.gm.outline = self.store_gm_outline
            self.GM.outline_clear( )
            self.GM.reset_outl_attributes( )
        elif self.gm_type == 'vector':
            self.gm.line = self.store_gm_line
            self.gm.linecolor = self.store_gm_linecolor
            self.gm.scale = self.store_gm_scale
            self.gm.alignment = self.store_gm_alignment
            self.gm.type = self.store_gm_type
            self.gm.reference = self.store_gm_reference
            self.GM.reset_vector_attributes( )
        elif self.gm_type == 'scatter':
            self.gm.marker = self.store_gm_marker
            self.gm.markercolor = self.store_gm_markercolor
            self.gm.markersize = self.store_gm_markersize
            self.GM.reset_scat_attributes( )
        elif self.gm_type in ['xvsy', 'xyvsy', 'yxvsx']:
            self.gm.line = self.store_gm_line
            self.gm.linecolor = self.store_gm_linecolor
            self.gm.marker = self.store_gm_marker
            self.gm.markercolor = self.store_gm_markercolor
            self.gm.markersize = self.store_gm_markersize
            self.GM.oneD_clear( gui_parent )
            self.GM.reset_oneD_attributes( )
        elif self.gm_type == 'taylordiagram':
            self.GM.detail = self.store_gm_detail
            self.GM.max = self.store_gm_max
            self.GM.quadrans = self.store_gm_quadrans
            self.GM.skillValues = self.store_gm_skillValues
            self.GM.skillColor = self.store_gm_skillColor
            self.GM.skillDrawLabels = self.store_gm_skillDrawLabels
            self.GM.skillCoefficient = self.store_gm_skillCoefficient
            self.GM.referencevalue = self.store_gm_referencevalue
            self.GM.arrowlength = self.store_gm_arrowlength
            self.GM.arrowangle = self.store_gm_arrowangle
            self.GM.arrowbase = self.store_gm_arrowbase
            self.GM.Marker.status = self.store_gm_marker_status
            self.GM.Marker.line = self.store_gm_marker_line
            self.GM.Marker.id = self.store_gm_marker_id
            self.GM.Marker.id_size = self.store_gm_marker_id_size
            self.GM.Marker.id_color = self.store_gm_marker_id_color
            self.GM.Marker.id_font = self.store_gm_marker_id_font
            self.GM.Marker.symbol = self.store_gm_marker_symbol
            self.GM.Marker.color = self.store_gm_marker_color
            self.GM.Marker.size = self.store_gm_marker_size
            self.GM.Marker.xoffset = self.store_gm_marker_xoffset
            self.GM.Marker.yoffset = self.store_gm_marker_yoffset
            self.GM.Marker.line_color = self.store_gm_marker_line_color
            self.GM.Marker.line_size = self.store_gm_marker_line_size
            self.GM.Marker.line_type = self.store_gm_marker_line_type


        # Store the "Common Attribute Settings" for X ticks and labels
        self.gm.xticlabels1 = self.store_gm_xticlabels1
        self.gm.xticlabels2 = self.store_gm_xticlabels2
        self.gm.xmtics1 = self.store_gm_xmtics1
        self.gm.xmtics2= self.store_gm_xmtics2
        self.x_attributes.reset()

        # Store the "Common Attribute Settings" for Y ticks and labels
        self.gm.yticlabels1 = self.store_gm_yticlabels1
        self.gm.yticlabels2 = self.store_gm_yticlabels2
        self.gm.ymtics1 = self.store_gm_ymtics1
        self.gm.ymtics2 = self.store_gm_ymtics2
        self.y_attributes.reset()

        # Store the "Common Attribute Settings" for World Coordinates
        self.gm.datawc_x1 = self.store_gm_datawc_x1
        self.gm.datawc_y1 = self.store_gm_datawc_y1
        self.gm.datawc_x2 = self.store_gm_datawc_x2
        self.gm.datawc_y2 = self.store_gm_datawc_y2
        self.wc_attributes.reset()

        # Store the "Common Attribute Settings" for Projection and Axis
        self.gm.projection = self.store_gm_projection
        self.gm.xaxisconvert = self.store_gm_xaxisconvert
        self.gm.yaxisconvert = self.store_gm_xaxisconvert
        self.pa_attributes.reset()

        # Plot the settings
        self.apply( gui_parent )

    def gm_clear ( self, gui_parent ):
        # Clear Graphics Method values
        if self.gm_type == 'boxfill':
           self.GM.boxfill_clear()
        elif self.gm_type == 'meshfill':
           self.GM.meshfill_clear()
        elif self.gm_type in ['isofill', 'isoline']:
           self.GM.contour_clear()
        elif self.gm_type == 'outfill':
           self.GM.outfill_clear()
        elif self.gm_type == 'outline':
           self.GM.outline_clear()
        elif self.gm_type in ['xvsy', 'xyvsy',  'yxvsx']:
           self.GM.oneD_clear( gui_parent )


        # Clear the "Common Attribute Settings" for X ticks and labels
        self.x_attributes.clear()

        # Clear the "Common Attribute Settings" for Y ticks and labels
        self.y_attributes.clear()

        # Clear the "Common Attribute Settings" for World Coordinates
        self.wc_attributes.clear()

        # Clear the "Common Attribute Settings" for Projection and Axis
        self.pa_attributes.clear()

    def execute (self, gui_parent, result):
        if result == 'Preview':
          self.preview_flg = 1
          self.apply( gui_parent )
        if result == 'Reset':
          self.preview_flg = 0
          self.gm_reset( gui_parent )
        if result == 'Clear':
          self.preview_flg = 0
          self.gm_clear( gui_parent )
        if result == 'Save':
          self.preview_flg = 0
          self.apply( gui_parent )
          self.dismiss()
        if result in ['Cancel', None]:
          if self.preview_flg: self.gm_reset( gui_parent )
          self.dismiss()

    def apply(self, gui_parent):
        if self.gm_name == 'default':
            showerror('Error Message',
                      'You cannot edit the graphics method named "default".'
            )
            return
        self.GM.get_settings()
        if self.gm_type!='taylordiagram':
            self.x_attributes.set()
            self.y_attributes.set()
            self.wc_attributes.set()
            self.pa_attributes.set()

        # Automatic update of the display if necessary. Only update if it is called from VCDAT.
        if gui_parent is not None:
           for i in gui_parent.pl.form.keys():
               gm_type = vcs_legacy.graphicsmethodtype( self.gm )
#               dfj = self.vcs_legacy.getisofill('ASD')
               fm_label = string.lower( gui_parent.pl.form[i].gm.type )
               if (gui_parent.pl.form[i].gm.get() == self.gm.name) and (gm_type == fm_label):
                   gui_parent.pl.evt_replace_gm( 1, i, gui_parent, None)
        
    def dismiss(self):
        self.dialog.destroy()            # Destroy the graphicsmethod GUI


###########################################################################################
###########################################################################################
##                                                                                       ##
## Start the classes and functions necessary to operate the VCS graphics method editor.  ##
##                                                                                       ##
###########################################################################################
###########################################################################################

#---------------------------------------------------------------------------------
# Event handling function that will allow the passing of arguments
#---------------------------------------------------------------------------------
class G_Command:
   def __init__(self, func, *args, **kw):
      self.func = func
      self.args = args
      self.kw = kw

   def __call__(self, *args, **kw):
      args = self.args + args
      kw.update(self.kw)
      return apply(self.func, args, kw)

#----------------------------------------------------------------------------------------
# Create the graphicsmethod File menu and its menu items
#----------------------------------------------------------------------------------------
def create_graphicsmethod_file_menu (main_menu, eself, gui_parent):
      main_menu.addmenu('File', 'Open/Save VCS Graphics Method', tearoff = 1)
      #
      # Create the "New graphics method" menu item
      main_menu.addmenuitem('File', 'command', 'New graphics method',
                         label = 'New Graphics Method...', 
                         command = G_Command(evt_new_graphics_methods, eself, gui_parent)
                        )
      #
      # Create the "Edit graphics method" menu item
      main_menu.addmenuitem('File', 'command', 'Load graphics method',
                         label = 'Edit Graphics Method...',
                         command = G_Command(evt_edit_graphics_methods, eself, gui_parent)
                        )
      #
      # Create the "Rename graphics method" menu item
      main_menu.addmenuitem('File', 'command', 'Save under new name',
                         label = 'Rename %s Graphics Method to...' % string.capitalize( eself.gm_type ),
                         command = G_Command(evt_rename_graphics_methods, eself, gui_parent)
                        )
      #
      # Create the "Save to Script file " menu item
      main_menu.addmenuitem('File', 'command', 'Save to script file',
                         label = 'Save %s as a Script File...' % string.capitalize( eself.gm_type ),
                         command = G_Command(evt_savescript_graphics_methods, eself, gui_parent)
                        )
      main_menu.addmenuitem('File', 'command', 'Close Graphics Method Editor',
                          label = "Exit",
                          command = eself.dismiss
                         )

gmchlst = [ "Boxfill", "Isofill", "Isoline", "Outfill", "Outline", "Xyvsy", "Meshfill",
                  "Yxvsx", "XvsY", "Vector", "Scatter", "Taylordiagram" ]
class hold: pass

def evt_new_graphics_methods( eself, gui_parent ):
      # Popup use dialog widget
      hold.dialog = dialog = Pmw.PromptDialog(eself.root,
                                title='Create new %s graphics method from %s' % (eself.gm_type, eself.gm_name),
                                label_text='Enter New %s Name:' % string.capitalize( eself.gm_type ),
                                entryfield_labelpos='nw',
                                entry_width = 50,
                                defaultbutton=0, buttons=('OK', 'Cancel'),
                                command = G_Command(execute_new_selection, eself, gui_parent, hold))

      dialog.text_opt2 = Tkinter.StringVar()
      dialog.text_opt2.set(string.capitalize( eself.gm_type ))
      opt2 = Pmw.OptionMenu( dialog.interior(),
                 menubutton_width=10,
                 menubutton_textvariable = dialog.text_opt2,
                 labelpos = 'w',
                 label_text='Select the Graphics Method:',
                 items=gmchlst,
                 command=G_Command(evt_which_graphics_method, dialog)
      )
      enyfield = dialog.component('entryfield')
      opt2.pack( side='top', before=enyfield, fill='both', expand=1, padx = 20, pady = 10 )
      gui_support.balloon.bind( opt2, 'Select the graphics method type.' )

      dialog.transient( eself.dialog.interior() ) # draw widget on top of its parent

      # Position dialog popup
      parent_geom = eself.dialog.geometry()
      geom = string.split(parent_geom, '+')
      d1 = string.atoi( geom[1] )
      d2 = string.atoi( geom[2] )
      dialog.geometry( "+%d+%d" % (d1, d2) )
               
def execute_new_selection( eself, gui_parent, hold, result ):
      dialog = hold.dialog
      if result == 'OK':
         new_name = dialog.component('entry').get()
         gm_type = dialog.text_opt2.get()
         gm_name = eself.gm_name
         if (eself.gm_type != string.lower(gm_type)):
            gm_name = 'default'
         if new_name in eself.vcs_legacy.listelements( gm_type ):
            gui_message.error( 'Error occurred while trying to create the new %s graphics method %s from %s. Make sure the new graphics method name does not already exist.' % (gm_type, new_name, gm_name) )
            return
         try:
            if string.lower( gm_type ) == 'boxfill':
               eself.vcs_legacy.createboxfill(new_name, gm_name)
            elif string.lower( gm_type ) == 'meshfill':
               eself.vcs_legacy.createmeshfill(new_name, gm_name)
            elif string.lower( gm_type ) == 'isofill':
               eself.vcs_legacy.createisofill(new_name, gm_name)
            elif string.lower( gm_type ) == 'isoline':
               eself.vcs_legacy.createisoline(new_name, gm_name)
            elif string.lower( gm_type ) == 'outfill':
               eself.vcs_legacy.createoutfill(new_name, gm_name)
            elif string.lower( gm_type ) == 'outline':
               eself.vcs_legacy.createoutline(new_name, gm_name)
            elif string.lower( gm_type ) == 'vector':
               eself.vcs_legacy.createvector(new_name, gm_name)
            elif string.lower( gm_type ) == 'scatter':
               eself.vcs_legacy.createscatter(new_name, gm_name)
            elif string.lower( gm_type ) == 'taylordiagram':
               eself.vcs_legacy.createtaylordiagram(new_name, gm_name)
            elif string.lower( gm_type ) == 'xvsy':
               eself.vcs_legacy.createxvsy(new_name, gm_name)
            elif string.lower( gm_type ) == 'xyvsy':
               eself.vcs_legacy.createxyvsy(new_name, gm_name)
            elif string.lower( gm_type ) == 'yxvsx':
               eself.vcs_legacy.createyxvsx(new_name, gm_name)
            elif string.lower( gm_type ) == 'continents':
               eself.vcs_legacy.createcontinents(new_name, gm_name)
         except:
            gui_message.error( 'Error occurred while trying to create the new %s graphics method %s from %s. Make sure the new graphics method name does not already exist.' % (gm_type, new_name, gm_name) )
            return

         # Popup new graphics method gui
         eself.vcs_legacy.graphicsmethodgui(gm_type, new_name, gui_parent = gui_parent)

         # Update the graphics method window
         if (gui_parent != None) and (gui_parent.panelDV.gm_name == gm_type):
               # Redisplay the graphics method list
               gm_list = eself.vcs_legacy.listelements( eself.gm_type )
               gui_parent.panelDV.gm_listbox.setlist( gm_list )
               name_index = gm_list.index( new_name )
               gui_parent.panelDV.gm_listbox.select_set( name_index )
               gui_parent.panelDV.gm_listbox.see( name_index )

      dialog.destroy()
      del dialog

def evt_which_graphics_method( dialog, event ):
      gm_type = dialog.text_opt2.get()
      dialog.configure(label_text='Enter New %s Name:' % string.capitalize( gm_type ))

def evt_edit_graphics_methods( eself, gui_parent ):
      # Create the popup dialog.
      eself.dialog_tmp = Pmw.ComboBoxDialog( eself.root,
            title = 'Select %s Graphics Method' % string.capitalize( eself.gm_type ),
            buttons = ('OK', 'Cancel'),
            defaultbutton = 'OK',
            combobox_labelpos = 'n',
            label_text = 'Enter or Select %s Name:' % string.capitalize( eself.gm_type ),
            scrolledlist_items = eself.vcs_legacy.listelements( eself.gm_type ),
            command = G_Command(execute_load_selection, eself, gui_parent)
      )

      eself.dialog_tmp.text_opt2 = Tkinter.StringVar()
      eself.dialog_tmp.text_opt2.set(string.capitalize( eself.gm_type ))
      opt2 = Pmw.OptionMenu( eself.dialog_tmp.interior(),
                 menubutton_width=10,
                 menubutton_textvariable = eself.dialog_tmp.text_opt2,
                 labelpos = 'w',
                 label_text='Select the Graphics Method:',
                 items=gmchlst,
                 command=G_Command(evt_which_edit_graphics_method, eself, eself.dialog_tmp)
      )
      enyfield = eself.dialog_tmp.component('combobox')
      opt2.pack( side='top', before=enyfield, fill='both', expand=1, padx = 20, pady = 10 )
      gui_support.balloon.bind( opt2, 'Select the graphics method type.' )

      eself.dialog_tmp.transient( eself.dialog.interior() ) # draw widget on top of its parent

      # Position dialog popup
      parent_geom = eself.dialog.geometry()
      geom = string.split(parent_geom, '+')
      d1 = string.atoi( geom[1] )
      d2 = string.atoi( geom[2] )
      eself.dialog_tmp.geometry( "+%d+%d" % (d1, d2) )

def evt_which_edit_graphics_method( eself, dialog, event ):
      eself.gm_type = dialog.text_opt2.get()
      dialog.configure(label_text='Enter New %s Name:' % string.capitalize( eself.gm_type ))
      dialog.setlist( eself.vcs_legacy.listelements( eself.gm_type ) )

def execute_load_selection( eself, gui_parent, result ):
     if result == 'OK':
        new_name = eself.dialog_tmp.get()
        if new_name in eself.vcs_legacy.listelements( eself.gm_type ):
           # Popup new graphics method gui
           eself.vcs_legacy.graphicsmethodgui(eself.gm_type, new_name, gui_parent = gui_parent)
        else:
           gui_message.error( 'Error occurred while trying to load %s graphics method %s. Make sure the graphics method name exist.' % (eself.gm_type, new_name) )
           eself.dialog_tmp.setlist( eself.vcs_legacy.listelements( eself.gm_type ) )
           return

     eself.dialog_tmp.destroy()
     del eself.dialog_tmp

def evt_rename_graphics_methods( eself, gui_parent ):
      # Popup use dialog widget
      hold.dialog = dialog = Pmw.PromptDialog(eself.root,
                                title='Rename %s graphics method from %s to ?' % (eself.gm_type, eself.gm_name),
                                label_text='Enter New %s Name:' % string.capitalize( eself.gm_type ),
                                entryfield_labelpos='nw',
                                entry_width = 50,
                                defaultbutton=0, buttons=('OK', 'Cancel'),
                                command = G_Command(execute_rename_selection, eself, gui_parent, hold))
               
      dialog.transient( eself.dialog.interior() ) # draw widget on top of its parent

      # Position dialog popup
      parent_geom = eself.dialog.geometry()
      geom = string.split(parent_geom, '+')
      d1 = string.atoi( geom[1] )
      d2 = string.atoi( geom[2] )
      dialog.geometry( "+%d+%d" % (d1, d2) )

def execute_rename_selection( eself, gui_parent, hold, result ):
      dialog = hold.dialog
      if result == 'OK':
         new_name = dialog.component('entry').get()
         if new_name in eself.vcs_legacy.listelements( eself.gm_type ):
            gui_message.error( 'An error occurred while trying to rename the %s graphics method %s to %s. Make sure the new graphics method name does not already exist.' % (eself.gm_type, eself.gm_name, new_name) )
            return
         try:
            if string.lower( eself.gm_type ) == 'boxfill':
               r = eself.vcs_legacy.getboxfill( eself.gm_name )
            elif string.lower( eself.gm_type ) == 'meshfill':
               r = eself.vcs_legacy.getmeshfill( eself.gm_name )
            elif string.lower( eself.gm_type ) == 'isofill':
               r = eself.vcs_legacy.getisofill( eself.gm_name )
            elif string.lower( eself.gm_type ) == 'isoline':
               r = eself.vcs_legacy.getisoline( eself.gm_name )
            elif string.lower( eself.gm_type ) == 'outfill':
               r = eself.vcs_legacy.getoutfill( eself.gm_name )
            elif string.lower( eself.gm_type ) == 'outline':
               r = eself.vcs_legacy.getoutline( eself.gm_name )
            elif string.lower( eself.gm_type ) == 'vector':
               r = eself.vcs_legacy.getvector( eself.gm_name )
            elif string.lower( eself.gm_type ) == 'scatter':
               r = eself.vcs_legacy.getscatter( eself.gm_name )
            elif string.lower( eself.gm_type ) == 'taylordiagram':
               r = eself.vcs_legacy.gettaylordiagram( eself.gm_name )
            elif string.lower( eself.gm_type ) == 'xvsy':
               r = eself.vcs_legacy.getxvsy( eself.gm_name )
            elif string.lower( eself.gm_type ) == 'xyvsy':
               r = eself.vcs_legacy.getxyvsy( eself.gm_name )
            elif string.lower( eself.gm_type ) == 'yxvsx':
               r = eself.vcs_legacy.getyxvsx( eself.gm_name )
            elif string.lower( eself.gm_type ) == 'continents':
               r = eself.vcs_legacy.getcontinents( eself.gm_name )
         except:
            gui_message.error( 'An error occurred while trying to rename the %s graphics method %s to %s. Make sure the new graphics method name does not already exist.' % (eself.gm_type, eself.gm_name, new_name) )
            return

         r.name = eself.gm_name = new_name
         eself.dialog.title( "VCS's Graphics Method Editor: Editing -- %s: %s" % \
                                       ( string.capitalize( eself.gm_type), new_name ) )

         # Update the graphics method window
         if gui_parent != None:
            # Redisplay the graphics method list
            gm_list = eself.vcs_legacy.listelements( eself.gm_type )
            gui_parent.panelDV.gm_listbox.setlist( gm_list )
            name_index = gm_list.index( new_name )
            gui_parent.panelDV.gm_listbox.select_set( name_index )
            gui_parent.panelDV.gm_listbox.see( name_index )

      dialog.destroy()
      del dialog

def evt_savescript_graphics_methods( eself, gui_parent ):
      # Show the popup directory dialog
      filetypes = [ ("Python", ".py"), ("VCS", ".scr") ]
      save_dialog = tkFileDialog.asksaveasfilename(master=eself.dialog.interior(),
                    filetypes=filetypes,
                    title = 'Save %s graphics method %s to file' % (eself.gm_type, eself.gm_name))

      if save_dialog == '': return
      if (save_dialog[-3:] != '.py') and (save_dialog[-4:] != '.scr'): save_dialog += '.py'

      if string.lower( eself.gm_type ) == 'boxfill':
         r = eself.vcs_legacy.getboxfill( eself.gm_name )
      elif string.lower( eself.gm_type ) == 'meshfill':
         r = eself.vcs_legacy.getmeshfill( eself.gm_name )
      elif string.lower( eself.gm_type ) == 'isofill':
         r = eself.vcs_legacy.getisofill( eself.gm_name )
      elif string.lower( eself.gm_type ) == 'isoline':
         r = eself.vcs_legacy.getisoline( eself.gm_name )
      elif string.lower( eself.gm_type ) == 'outfill':
         r = eself.vcs_legacy.getoutfill( eself.gm_name )
      elif string.lower( eself.gm_type ) == 'outline':
         r = eself.vcs_legacy.getoutline( eself.gm_name )
      elif string.lower( eself.gm_type ) == 'vector':
         r = eself.vcs_legacy.getvector( eself.gm_name )
      elif string.lower( eself.gm_type ) == 'scatter':
         r = eself.vcs_legacy.getscatter( eself.gm_name )
      elif string.lower( eself.gm_type ) == 'taylordiagram':
         r = eself.vcs_legacy.gettaylordiagram( eself.gm_name )
      elif string.lower( eself.gm_type ) == 'xvsy':
         r = eself.vcs_legacy.getxvsy( eself.gm_name )
      elif string.lower( eself.gm_type ) == 'xyvsy':
         r = eself.vcs_legacy.getxyvsy( eself.gm_name )
      elif string.lower( eself.gm_type ) == 'yxvsx':
         r = eself.vcs_legacy.getyxvsx( eself.gm_name )
      elif string.lower( eself.gm_type ) == 'continents':
         r = eself.vcs_legacy.getcontinents( eself.gm_name )

      # Script the graphics method selected in the list
      eself.vcs_legacy.scriptobject( r, save_dialog, mode = 'w' )

#----------------------------------------------------------------------------------------
# Create the graphicsmethod Help menu and its menu items
#----------------------------------------------------------------------------------------
class create_graphicsmethod_help_menu:
   def __init__( self, main_menu, eself, parent ):
      main_menu.addmenu('Help', 'VCS Graphics Method Help', side='right', tearoff = 1)
      gui_support.add_balloon_help(main_menu, 'Help')
      main_menu.addmenuitem('Help', 'separator')

      main_menu.addmenuitem('Help', 'command', 'Help About',
                            label = 'About the Graphics Method Editor',
                            command = G_Command(self.evt_about_dialog, eself, parent)
                           )

   # Create about grapnics method dialog.
   def evt_about_dialog( self, eself, parent ):
        Pmw.aboutversion(sys.prefix)
        Pmw.aboutcopyright('\nCopyright:    2001, Regents of the University of California\nThis software may not be distributed to others without\npermission of the authors.\nAll rights reserved.')
        Pmw.aboutcontact(
            """Go to cdat.sourceforge.net for documentation, support, bug reporting, and releases.\nProgram for Climate Model Diagnosis and Intercomparison
Lawrence Livermore National Laboratory Livermore, CA 94550 """)
        self.about = Pmw.AboutDialog(parent, applicationname = 'The Visualization Control System - (VCS) Graphics Method Editor')
        self.about.transient( eself.root ) # draw widget on top of its parent
        parent_geom = eself.dialog.geometry()
        geom = string.split(parent_geom, '+')
        d1 = string.atoi( geom[1] )
        d2 = string.atoi( geom[2] )
        self.about.geometry( "+%d+%d" % (d1, d2) )

#----------------------------------------------------------------------------------------
# Create the general graphics method attributes
#----------------------------------------------------------------------------------------
class x_attributes:
    def __init__(self, page, parent, gm, vcs_legacy):
        self.gm=gm
        self.parent = parent

        scl_Frame=Pmw.ScrolledFrame(page,
                                labelpos = 'n',
                                label_text = 'X Ticks and Labels Editor',
                                )
        scl_Frame.pack(fill = 'both', expand = 1,side='top')
        page=scl_Frame.interior()

        vcs_legacylist = vcs_legacy.listelements('list')
        vcs_legacylist.insert(0,'*')
        group1 = Pmw.Group( page, tag_text = 'X Major Ticks and Labels' )
        group1.pack( side = 'top', fill = 'both', expand = 1, padx = 5, pady = 20 )
        self.xtic1=Pmw.ComboBox( group1.interior(),
            scrolledlist_usehullsize = 1,
            labelpos = 'w',
            label_text = 'xticlabels#1:',
            entry_background = 'white',
            entry_foreground = 'black',
            entry_width =  _entry_width,
            scrolledlist_items=vcs_legacylist
            )
        self.xtic1.pack( expand = 1, fill = 'x', padx=20, pady=5 )
        gui_support.balloon.bind( self.xtic1, "Specify a predefine VCS list name (i.e., lon20, lon30,\np_levels etc.). Or allow VCS to generate the xticlabels#1 by\nentering '*'. Or create a Python dictionary. For example:\n{10:'10', 20:'20', 30:'30'} or {0:'text', 10:'more text'}.\n\nNote: if the Python dictionary is not correct, then no\nxticlabels#1 will be plotted." )
        self.xtic2=Pmw.ComboBox( group1.interior(),
            labelpos = 'w',
            label_text = 'xticlabels#2:',
            entry_background = 'white',
            entry_foreground = 'black',
            entry_width =  _entry_width,
            scrolledlist_items=vcs_legacylist
            )
        self.xtic2.pack( expand = 1, fill = 'x', padx=20, pady=5 )
        gui_support.balloon.bind( self.xtic2, "Specify a predefine VCS list name (i.e., lon20, lon30,\np_levels etc.). Or allow VCS to generate the xticlabels#2 by\nentering '*'. Or create a Python dictionary. For example:\n{10:'10', 20:'20', 30:'30'} or {0:'text', 10:'more text'}.\n\nNote: if the Python dictionary is not correct, then no\nxticlabels#2 will be plotted." )

        group2 = Pmw.Group( page, tag_text = 'X Minor Ticks' )
        group2.pack( side = 'top', fill = 'both', expand = 1, padx = 5, pady = 20 )
        self.xmtic1=Pmw.ComboBox( group2.interior(),
            labelpos = 'w',
            label_text = 'xmtics#1:',
            entry_background = 'white',
            entry_foreground = 'black',
            entry_width =  _entry_width,
            scrolledlist_items=vcs_legacylist
            )
        self.xmtic1.pack( expand = 1, fill = 'x', padx=20, pady=5 )
        gui_support.balloon.bind( self.xmtic1, "Specify a predefine VCS list name (i.e., lon20, lon30,\np_levels etc.). Or allow VCS to generate the xmtics#1 by\nentering '*'. Or create a Python dictionary. For example:\n{10:'10', 20:'20', 30:'30'} or {0:'text', 10:'more text'}.\n\nNote: if the Python dictionary is not correct, then no\nxmtics#1 will be plotted." )
        self.xmtic2=Pmw.ComboBox( group2.interior(),
            labelpos = 'w',
            label_text = 'xmtics#2:',
            entry_background = 'white',
            entry_foreground = 'black',
            entry_width =  _entry_width,
            scrolledlist_items=vcs_legacylist
            )
        self.xmtic2.pack( expand = 1, fill = 'x', padx=20, pady=5 )
        gui_support.balloon.bind( self.xmtic2, "Specify a predefine VCS list name (i.e., lon20, lon30,\np_levels etc.). Or allow VCS to generate the xmtics#2 by\nentering '*'. Or create a Python dictionary. For example:\n{10:'10', 20:'20', 30:'30'} or {0:'text', 10:'more text'}.\n\nNote: if the Python dictionary is not correct, then no\nxmtics#2 will be plotted." )
#        btn=Tkinter.Button(page,
#            text = 'Reset X Ticks and Labels',
#            background = gui_color.one,
#            command = self.reset,
#            )
#        btn.pack( expand = 1, padx=5, pady=5 )
        self.reset()
    def reset(self):
        self.xtic1.setentry(self.gm.xticlabels1)
        self.xtic2.setentry(self.gm.xticlabels2)
        self.xmtic1.setentry(self.gm.xmtics1)
        self.xmtic2.setentry(self.gm.xmtics2)
    def clear(self):
        self.xtic1.clear()
        self.xtic2.clear()
        self.xmtic1.clear()
        self.xmtic2.clear()
    def set(self):
        try: self.gm.xticlabels1=eval(self.xtic1.get())
        except: self.gm.xticlabels1=self.xtic1.get()
        try: self.gm.xticlabels2=eval(self.xtic2.get())
        except:self.gm.xticlabels2=self.xtic2.get()
        try: self.gm.xmtics1=eval(self.xmtic1.get())
        except: self.gm.xmtics1=self.xmtic1.get()
        try: self.gm.xmtics2=eval(self.xmtic2.get())
        except: self.gm.xmtics2=self.xmtic2.get()
        
class y_attributes:
    def __init__(self, page, parent, gm, vcs_legacy):
        self.parent = parent
        self.gm=gm
        
        scl_Frame=Pmw.ScrolledFrame(page,
                                labelpos = 'n',
                                label_text = 'Y Ticks and Labels Editor',
                                )
        scl_Frame.pack(fill = 'both', expand = 1,side='top')
        page=scl_Frame.interior()

        vcs_legacylist = vcs_legacy.listelements('list')
        vcs_legacylist.insert(0,'*')
        group1 = Pmw.Group( page, tag_text = 'X Major Ticks and Labels' )
        group3 = Pmw.Group( page, tag_text = 'Y Major Ticks and Labels' )
        group3.pack( side = 'top', fill = 'both', expand = 1, padx = 5, pady = 20 )
        self.ytic1=Pmw.ComboBox( group3.interior(),
            labelpos = 'w',
            label_text = 'yticlabels#1:',
            entry_background = 'white',
            entry_foreground = 'black',
            entry_width =  _entry_width,
            scrolledlist_items=vcs_legacylist
            )
        self.ytic1.pack( expand = 1, fill = 'x', padx=20, pady=5 )
        gui_support.balloon.bind( self.ytic1, "Specify a predefine VCS list name (i.e., lon20, lon30,\np_levels etc.). Or allow VCS to generate the yticlabels#1 by\nentering '*'. Or create a Python dictionary. For example:\n{10:'10', 20:'20', 30:'30'} or {0:'text', 10:'more text'}.\n\nNote: if the Python dictionary is not correct, then no\nyticlabels#1 will be plotted." )
        self.ytic2=Pmw.ComboBox( group3.interior(),
            labelpos = 'w',
            label_text = 'yticlabels#2:',
            entry_background = 'white',
            entry_foreground = 'black',
            entry_width =  _entry_width,
            scrolledlist_items=vcs_legacylist
            )
        self.ytic2.pack( expand = 1, fill = 'x', padx=20, pady=5 )
        gui_support.balloon.bind( self.ytic2, "Specify a predefine VCS list name (i.e., lon20, lon30,\np_levels etc.). Or allow VCS to generate the yticlabels#2 by\nentering '*'. Or create a Python dictionary. For example:\n{10:'10', 20:'20', 30:'30'} or {0:'text', 10:'more text'}.\n\nNote: if the Python dictionary is not correct, then no\nyticlabels#2 will be plotted." )

        group4 = Pmw.Group( page, tag_text = 'Y Minor Ticks' )
        group4.pack( side = 'top', fill = 'both', expand = 1, padx = 5, pady = 20 )
        self.ymtic1=Pmw.ComboBox( group4.interior(),
            labelpos = 'w',
            label_text = 'ymtics#1:',
            entry_background = 'white',
            entry_foreground = 'black',
            entry_width =  _entry_width,
            scrolledlist_items=vcs_legacylist
            )
        self.ymtic1.pack( expand = 1, fill = 'x', padx=20, pady=5 )
        gui_support.balloon.bind( self.ymtic1, "Specify a predefine VCS list name (i.e., lon20, lon30,\np_levels etc.). Or allow VCS to generate the ymtics#1 by\nentering '*'. Or create a Python dictionary. For example:\n{10:'10', 20:'20', 30:'30'} or {0:'text', 10:'more text'}.\n\nNote: if the Python dictionary is not correct, then no\nymtics#1 will be plotted." )
        self.ymtic2=Pmw.ComboBox( group4.interior(),
            labelpos = 'w',
            label_text = 'ymtics#2:',
            entry_background = 'white',
            entry_foreground = 'black',
            entry_width =  _entry_width,
            scrolledlist_items=vcs_legacylist
            )
        self.ymtic2.pack( expand = 1, fill = 'x', padx=20, pady=5 )
        gui_support.balloon.bind( self.ymtic2, "Specify a predefine VCS list name (i.e., lon20, lon30,\np_levels etc.). Or allow VCS to generate the ymtics#2 by\nentering '*'. Or create a Python dictionary. For example:\n{10:'10', 20:'20', 30:'30'} or {0:'text', 10:'more text'}.\n\nNote: if the Python dictionary is not correct, then no\nymtics#2 will be plotted." )
#        btn=Tkinter.Button(page,
#            text = 'Reset Y Ticks and Labels',
#            background = gui_color.one,
#            command = self.reset,
#            )
#        btn.pack( expand = 1, padx=5, pady=5 )
        self.reset()
    def reset(self):
        self.ytic1.setentry(self.gm.yticlabels1)
        self.ytic2.setentry(self.gm.yticlabels2)
        self.ymtic1.setentry(self.gm.ymtics1)
        self.ymtic2.setentry(self.gm.ymtics2)
    def clear(self):
        self.ytic1.clear( )
        self.ytic2.clear( )
        self.ymtic1.clear( )
        self.ymtic2.clear( )
    def set(self):
        try: self.gm.yticlabels1=eval(self.ytic1.get())
        except:self.gm.yticlabels1=self.ytic1.get()
        try: self.gm.yticlabels2=eval(self.ytic2.get())
        except:self.gm.yticlabels2=self.ytic2.get()
        try: self.gm.ymtics1=eval(self.ymtic1.get())
        except:self.gm.ymtics1=self.ymtic1.get()
        try: self.gm.ymtics2=eval(self.ymtic2.get())
        except:self.gm.ymtics2=self.ymtic2.get()

class wc_attributes:
    def __init__(self, page, parent, gm):
        self.parent = parent
        self.gm=gm

        scl_Frame=Pmw.ScrolledFrame(page,
                                labelpos = 'n',
                                label_text = 'World Coordinates Editor',
                                )
        scl_Frame.pack(fill = 'both', expand = 1,side='top')
        page=scl_Frame.interior()

        group1 = Pmw.Group( page, tag_text = 'X Major Ticks and Labels' )
        group5 = Pmw.Group( page, tag_text = 'World Coordinates' )
        group5.pack( side = 'top', fill = 'both', expand = 1, padx = 5, pady = 20 )
        self.datawc1=Pmw.EntryField( group5.interior(),
            labelpos = 'w',
            label_text = 'datawc_x1:',
            entry_background = 'white',
            entry_foreground = 'black',
            entry_width =  _entry_width,
            )
        self.datawc1.pack( expand = 1, fill = 'x', padx=20, pady=5 )
        gui_support.balloon.bind( self.datawc1, "Set new X1 world coordinate value. If value is 1e+20,\nthen VCS will use the data's coordinate value specified\nin the dimension." )
        self.datawc2=Pmw.EntryField( group5.interior(),
            labelpos = 'w',
            label_text = 'datawc_y1:',
            entry_background = 'white',
            entry_foreground = 'black',
            entry_width =  _entry_width,
            )
        self.datawc2.pack( expand = 1, fill = 'x', padx=20, pady=5 )
        gui_support.balloon.bind( self.datawc2, "Set new Y1 world coordinate value. If value is 1e+20,\nthen VCS will use the data's coordinate value specified\nin the dimension." )
        self.datawc3=Pmw.EntryField( group5.interior(),
            labelpos = 'w',
            label_text = 'datawc_x2:',
            entry_background = 'white',
            entry_foreground = 'black',
            entry_width =  _entry_width,
            )
        self.datawc3.pack( expand = 1, fill = 'x', padx=20, pady=5 )
        gui_support.balloon.bind( self.datawc3, "Set new X2 world coordinate value. If value is 1e+20,\nthen VCS will use the data's coordinate value specified\nin the dimension." )
        self.datawc4=Pmw.EntryField( group5.interior(),
            labelpos = 'w',
            label_text = 'datawc_y2:',
            entry_background = 'white',
            entry_foreground = 'black',
            entry_width =  _entry_width,
            )
        self.datawc4.pack( expand = 1, fill = 'x', padx=20, pady=5 )
        gui_support.balloon.bind( self.datawc4, "Set new Y2 world coordinate value. If value is 1e+20,\nthen VCS will use the data's coordinate value specified\nin the dimension." )
        self.reset()
#        btn=Tkinter.Button(page,
#            text = 'Reset World Coordinates',
#            background = gui_color.one,
#            command = self.reset,
#            )
#        btn.pack( expand = 1, padx=5, pady=5 )
    def reset(self):
        self.datawc1.setentry(repr(self.gm.datawc_x1))
        self.datawc2.setentry(repr(self.gm.datawc_y1))
        self.datawc3.setentry(repr(self.gm.datawc_x2))
        self.datawc4.setentry(repr(self.gm.datawc_y2))
    def clear(self):
        self.datawc1.clear( )
        self.datawc2.clear( )
        self.datawc3.clear( )
        self.datawc4.clear( )
    def set(self):
        self.gm.datawc_x1=eval(self.datawc1.get())
        self.gm.datawc_x2=eval(self.datawc3.get())
        self.gm.datawc_y1=eval(self.datawc2.get())
        self.gm.datawc_y2=eval(self.datawc4.get())

class pa_attributes:
    
    def evt_projection(self,canvas,gui_parent):
        name=self.projection.getvalue()
        proj=canvas.listelements('projection')
        self.projection.setitems(proj)
        self.projection.setvalue(name)
        canvas.projectiongui(gui_parent=gui_parent,projection=name)
        proj.sort()
        return
    
    def __init__(self, page, parent, gm, gm_type, canvas, gui_parent):
        self.parent = parent
        self.gui_parent=gui_parent
        self.gm=gm
        self.gm_type=gm_type
        self.canvas=canvas
        scl_Frame=Pmw.ScrolledFrame(page,
                                labelpos = 'n',
                                label_text = 'Projection and Axis Editor',
                                )
        scl_Frame.pack(fill = 'both', expand = 1,side='top')
        page=scl_Frame.interior()

        group1 = Pmw.Group( page, tag_text = 'X Major Ticks and Labels' )
        projections=canvas.listelements('projection')
        projections.sort()
        group6 = Pmw.Group( page, tag_text = 'Plot Projection' )
        group6.pack( side = 'top', fill = 'both', expand = 1, padx = 5, pady = 20 )
        self.frm=frm=group6.interior()
        self.button=Tkinter.Button(frm,
                         text='Projection',
                         command=gui_control.Command(projectiongui.ProjGUI,canvas=self.canvas,gui_parent=gui_parent,dialog_parent=self.parent,master=frm,name=self.gm.projection,Parent=self),
                         bg=gui_support.gui_color.Proj_color,
                         )
        self.button.pack(side='left')
	self.projection = Pmw.OptionMenu( frm,
		labelpos = 'w',
## 		label_text = 'projection:',
## 		menubutton_text = self.var1,
		items = projections,
                command = self.set,
		menubutton_width = 10,
	)
        gui_support.balloon.bind( self.projection, "Choose new graphics method projection." )
	self.projection.pack(side='left', anchor = 'w', padx = 10, pady = 10)


        group7 = Pmw.Group( page, tag_text = 'X- and Y-axis representation' )
        group7.pack( side = 'top', fill = 'both', expand = 1, padx = 5, pady = 20 )
        if (gm_type not in [ 'xyvsy', 'continents' ] ):
            self.xaxisconvert = Pmw.OptionMenu( group7.interior(),
                 labelpos = 'w',
                 label_text = 'X-Axis Conversion',
                 items = ['linear', 'log10', 'ln', 'exp', 'area_wt'],
                 menubutton_width = 10,
            )
            self.xaxisconvert.pack(anchor = 'w', padx = 10, pady = 10)
            gui_support.balloon.bind( self.xaxisconvert, "Choose new X-axis representation." )

        if (gm_type not in [ 'yxvsx', 'continents' ] ):
            self.yaxisconvert = Pmw.OptionMenu(group7.interior(),
                 labelpos = 'w',
                 label_text = 'Y-Axis Conversion',
                 items = ['linear', 'log10', 'ln', 'exp', 'area_wt'],
                 menubutton_width = 10,
            )
            self.yaxisconvert.pack(anchor = 'w', padx = 10, pady = 10)
            gui_support.balloon.bind( self.yaxisconvert, "Choose new Y-axis representation." )
        self.reset()
#        btn=Tkinter.Button(page,
#            text = 'Reset Projection and Axis',
#            background = gui_color.one,
#            command = self.reset,
#            )
#        btn.pack( expand = 1, padx=5, pady=5 )
    def reset(self):
        self.projection.invoke(self.gm.projection)
        if (self.gm_type not in [ 'xyvsy', 'continents' ] ):
            self.xaxisconvert.invoke(self.gm.xaxisconvert)
        if (self.gm_type not in [ 'yxvsx', 'continents' ] ):
            self.yaxisconvert.invoke(self.gm.yaxisconvert)
    def clear(self):
        self.projection.invoke('linear')
        if self.gm_type != 'xyvsy':
           self.xaxisconvert.invoke('linear')
        if self.gm_type != 'yxvsx':
           self.yaxisconvert.invoke('linear')
    def set(self,*crapargs):
        self.gm.projection=self.projection.getcurselection()
        self.button.configure(command=gui_control.Command(projectiongui.ProjGUI,canvas=self.canvas,gui_parent=self.gui_parent,dialog_parent=self.parent,master=self.frm,name=self.gm.projection,Parent=self))

        if (self.gm_type not in [ 'xyvsy', 'continents' ] ):
            self.gm.xaxisconvert=self.xaxisconvert.getcurselection()
        if (self.gm_type not in [ 'yxvsx', 'continents' ] ):
            self.gm.xaxisconvert=self.yaxisconvert.getcurselection()



# Create/Popup graphics method gui editor for VCS.
def create(canvas=None, gm_type='boxfill', gm_name='default', gui_parent=None):
    GraphicsMethodGui(canvas, gm_type, gm_name, gui_parent)
#---------------------------------------------------------------------
# End of File
#---------------------------------------------------------------------










