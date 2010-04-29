#!/usr/bin/env python
# Adapted for numpy/ma/cdms2 by convertcdms.py
#
# The PCMDI Data Browser VCS Plot Functions -  vcs_function module
#
###############################################################################
#                                                                             #
# Module:       vcs_function module                                           #
#                                                                             #
# Copyright:    "See file Legal.htm for copyright information."               #
#                                                                             #
# Authors:      PCMDI Software Team                                           #
#               Lawrence Livermore NationalLaboratory:                        #
#               support@pcmdi.llnl.gov                                        #
#                                                                             #
# Description:  PCMDI Software System browser VCS plot functions.             #
#                                                                             #
# Version:      4.0                                                           #
#                                                                             #
###############################################################################
#
#---------------------------------------------------------------------
# NOTE: need to use version of Python that imports Tkinter and Pmw
#---------------------------------------------------------------------
import Tkinter, Pmw
import os, sys, types, string, cdms2, vcs
import MV2
import gui_menu
import gui_alter_plot
import gui_functions
import gui_control
import gui_formulate
import gui_message
import gui_annotate
import __main__

# Get the previously saved state of the GUI
try:
   fn = '%s/PCMDI_GRAPHICS' % os.environ['HOME']
   sys.path.append(fn)
   import vcdat_initial
except:
   pass

#---------------------------------------------------------------------------
# VCS functions
#---------------------------------------------------------------------------
def dotemplate(parent,template,id, replot_flg, g_name):
   """
   Reset the tempalte nae and template attributes if neeeded
   """
   template_name = template
   if template_name[:3] != "ASD": parent.template_skip = 1
   if parent.template_skip == 0: # don't set template if called from evt_status_button
      template_name = set_template( parent, template_name )
      parent.pl.form[id].template.setentry(template_name) # Set the form line template entry
   if template_name[0:3]!='ASD' or ((template_name[0:3]=='ASD'and template_name.find('of_')==-1) and (template_name[0:3]=='ASD'and template_name.find('map')==-1)):
      gui_alter_plot.settings( parent, replot_flg, g_name, template_name ) # Set the plot's visual appearance
   return template_name

#------------------------------------------------------------------------
# Initialize the VCS grapics packages
#------------------------------------------------------------------------
def initialize ( root , dosplashscreen, counter_bar):
   import sys
   root.show_debug=False
   root.vcs={}
   root.number_of_vcs_canvas = 4
   root.plot_ct = {}
   asd_script = os.path.join(vcs.__path__[0],'..','..','..','..','bin','ASD.scr')
   try: root.colormap_name = vcdat_initial.set.colormap_name
   except: root.colormap_name = 'ASD'
   if root.menu.vcs_canvas_gui_settings_flg == 0:
      for i in range( root.number_of_vcs_canvas ):
         if dosplashscreen == 1: counter_bar.update()
         root.vcs[i]=vcs.init(call_from_gui = 1)
         root.vcs[ i ].scriptrun( asd_script )
         try:
            # I put back the 0. I am tagging it to see there are other problems.
            # Must have the 0 or it will seg fault when bring up VCDAT on IA64 platforms.
            #root.vcs[ i ].setcolormap( root.colormap_name )
            root.vcs[ i ].setcolormap( root.colormap_name, 0 )
         except:
            pass
         root.vcs[ i ].mode = 1
         root.plot_ct[ i ] = 0
   else:
      if dosplashscreen == 1: counter_bar.update()
      root.vcs[ 0 ]=vcs.init(call_from_gui = 1, gui=1)
      root.vcs[ 0 ].scriptrun( asd_script )
      try: root.vcs[ 0 ].setcolormap( root.colormap_name, 0 )
      except: pass
      root.vcs[ 0 ].mode = 1
      root.plot_ct[ 0 ] = 0
   root.vcs_id = 0
   root.ret_val=None
   root.dir_history=["============================================================================"]
   root.animate_canvas = root.vcs_id
   root.vcs_canvas_plot_number = 1
   root.plot_projection = 'linear'
##    root.plot_ratio = 'autot'
   root.d={}
   root.dct=0
   root.QuickPlot_name={}
   root.annotate_get_info=1
   root.annotate_var_name=None
   root.annotate_view=0
   root.annotate_source=None
   root.annotate_name=None
   root.annotate_title=None
   root.annotate_units=None
   root.annotate_date=1
   root.annotate_time=1
   root.annotate_mean=1
   root.annotate_min=1
   root.annotate_max=1
   root.annotate_multiplier=1
   root.annotate_xlabel=None
   root.annotate_ylabel=None
   root.hannotate_xlabel=None
   root.hannotate_ylabel=None
   root.annotate_legendlabel=None
   root.annotate_retain='no'
   root.annotate_start_over=1
   try:
      root.annotate_status=string.lower(vcdat_initial.set.annotate_status)
   except:
      root.annotate_status='on'
   if root.annotate_status not in ['on','off']:
      root.annotate_status='on'
   try:
      root.vcg = vcdat_initial.set.vcg
   except:
      root.vcg={0:[None, None, None, None], 1:[None, None, None, None], 2:[None, None, None, None], 3:[None, None, None, None]}
   root.vc2=[]
   root.vc3=[]
   root.vc4=[]
   root.toggle_state = [1,1,1,1,1,1]
   root.boxfill_level1=None
   root.boxfill_level2=None
   root.boxfill_levels=None
   root.boxfill_color1=None
   root.boxfill_color2=None
   root.boxfill_fillareacolors=None
   root.boxfill_missing=241
   root.boxfill_legend=None
   root.boxfill_ext1=None
   root.boxfill_ext2=None
   root.boxfill_type='linear'
   root.meshfill_levels=([1.0000000200408773e+20, 1.0000000200408773e+20],)
   root.meshfill_fillareacolors=None
   root.meshfill_missing=241
   root.meshfill_legend=None
   root.meshfill_ext1=None
   root.meshfill_ext2=None
   root.meshfill_mesh='n'
   root.meshfill_wrap=[0.,0.]
   root.iso_ranges=None
   root.iso_colors=None
   root.iso_line_types=None
   root.iso_line_widths=None
   root.iso_legend=None
   root.iso_min=None
   root.iso_max=None
   root.iso_num=None
   root.iso_spacing='Linear'
   root.iso_neg_val=None
   root.iso_neg_dec=None
   root.iso_line_clockwise = None
   root.iso_line_arrow_scale = None
   root.iso_line_arrow_spacing = None
   root.iso_line_arrow_angle = None
   root.oneD_ltypes=None
   root.oneD_lcolors=None
   root.oneD_lwidths=None
   root.oneD_mtypes=None
   root.oneD_mcolors=None
   root.oneD_mwidths=None
   root.app_x_min = None
   root.app_x_max = None
   root.app_y_min = None
   root.app_y_max = None
   root.app_d_min = None
   root.app_d_max = None
   root.yx_xmin = None
   root.yx_xmax = None
   root.yx_ymin = None
   root.yx_ymax = None
   root.Boxfillct = [0,0,0,0]
   root.Isofillct = [0,0,0,0]
   root.Isolinect = [0,0,0,0]
   root.Outfillct = [0,0,0,0]
   root.Outlinect = [0,0,0,0]
   root.Taylordiagramct = [0,0,0,0]
   root.Vectorct = [0,0,0,0]
   root.Meshfillct = [0,0,0,0]
   root.Scatterct = [0,0,0,0]
   root.XvsYct = [0,0,0,0]
   root.XYct = 0
   root.XYname = None
   root.XYct_restart=0
   root.multiplier = None
   root.printer=None
   root.vec_line=None
   root.vec_linecolor=None
   root.vec_scale=None
   root.vec_alignment='center'
   root.vec_type='arrows'
   root.vec_ref=1.e20
   root.cont_line=None
   root.cont_linecolor=None
   root.cont_linewidth=None
   root.cont_type=None
   root.outf_style=None
   root.outf_index=None
   root.outf_fillcolor=None
   root.outf_outfill=[1]
   root.outl_line=0
   root.outl_linecolor=None
   root.outl_outline=[1]
   root.scat_marker=None
   root.scat_markercolor=None
   root.scat_markersize=None
   root.save_region = 0
   try:
      root.template_name = vcdat_initial.set.template_name
   except:
      root.template_name = 'ASD'
   root.template_name_dud = 'ASD_dud'
   root.template_skip = 0
   try:
      root.graphics_method_name = vcdat_initial.set.graphics_method_name
   except:
      root.graphics_method_name = 'ASD'
   try:
      root.show_defined_variables_tools =  vcdat_initial.set.show_defined_variables_tools
   except:
      root.show_defined_variables_tools =  1
   try:
      root.show_template_graphics_method = vcdat_initial.set.show_template_graphics_method
   except:
      root.show_template_graphics_method = 0

   # VCDAT default font for graphics
   try:
      root.default_font = vcdat_initial.set.default_font
   except:
      root.default_font = 'AvantGarde'
   root.vcs[0].setdefaultfont(root.default_font)
   
   if dosplashscreen == 1: counter_bar.update()
   #
   # record the event: select a directory
   gui_control.record_command( root, '\n# Initialize the four VCS Canvases by creating', 1 )
   gui_control.record_command( root, '# a list to hold the 4 VCS Canvas', 1 )
   gui_control.record_command( root, 'vcs_canvas_list = []', 1 )
   gui_control.record_command( root, '\n# Loop (from 0 to 3) to create VCS Canvas 1, 2, 3, and 4', 1 )
   gui_control.record_command( root, 'for i in range(4):', 1 )
   gui_control.record_command( root, '   vcs_canvas_list.append(  vcs.init() )', 1 )
   gui_control.record_command( root, '\n# Run VCS script that contains the appropriate VCS templates,')
   gui_control.record_command( root, '# graphics methods, and color maps')
   gui_control.record_command( root, 'for i in range(4):')
   command = "   vcs_canvas_list[ i ].scriptrun( '%s' )" % asd_script
   gui_control.record_command( root, command )
   gui_control.record_command( root, "   vcs_canvas_list[ i ].setcolormap('ASD')")
##    gui_control.record_command( root, '   vcs_canvas_list[ i ].mode = 0')
   gui_control.record_command( root, '\n# Set the Command Line VCS Canvas hooks', 1 )
   gui_control.record_command( root, 'vcs_hook1 = vcs_canvas_list[0]', 1 )
   gui_control.record_command( root, 'vcs_hook2 = vcs_canvas_list[1]', 1 )
   gui_control.record_command( root, 'vcs_hook3 = vcs_canvas_list[2]', 1 )
   gui_control.record_command( root, 'vcs_hook4 = vcs_canvas_list[3]', 1 )
#   gui_control.record_command( root, '# variables that are in memory', 1 )
#   gui_control.record_command( root, 'slabm={}', 1)

   #
   # create templates for 2 multiple plots on a page
   try:
      t=root.vcs[ root.vcs_id ].gettemplate('ASD1_of_2')
   except:
      gui_control.record_command( root, '\n# Create templates for 2 plots on a page.')
      t=root.vcs[ root.vcs_id ].createtemplate('ASD1_of_2', root.template_name)
      t2=root.vcs[ root.vcs_id ].createtemplate('ASD1_of_2_dud', root.template_name_dud)
      modify_template( root, t, t2, root.template_name, root.template_name_dud, 0, 26, -33, -24, 'horizontal', 1 )
      t=root.vcs[ root.vcs_id ].createtemplate('ASD2_of_2', root.template_name)
      t2=root.vcs[ root.vcs_id ].createtemplate('ASD2_of_2_dud', root.template_name_dud)
      modify_template( root, t, t2, root.template_name, root.template_name_dud, 0, -25, -33, -24, 'horizontal', 0 )
      # molwd, robinson, and polar projection template
      t=root.vcs[ root.vcs_id ].createtemplate('ASD_map1of2', 'ASD_map')
      t2=root.vcs[ root.vcs_id ].createtemplate('ASD_map1of2_dud', 'ASD_map')
      modify_template( root, t, t2, 'ASD_map', 'ASD_map', 0, 26, -33, -24, 'horizontal', 1 )
      t=root.vcs[ root.vcs_id ].createtemplate('ASD_map2of2', 'ASD_map')
      t2=root.vcs[ root.vcs_id ].createtemplate('ASD_map2of2_dud', 'ASD_map')
      modify_template( root, t, t2, 'ASD_map', 'ASD_map', 0, -25, -33, -24, 'horizontal', 0 )

   #
   # create templates for 3 multiple plots on a page
   try:
      t=root.vcs[ root.vcs_id ].gettemplate('ASD1_of_3')
   except:
      t=root.vcs[ root.vcs_id ].createtemplate('ASD1_of_3', root.template_name)
      t2=root.vcs[ root.vcs_id ].createtemplate('ASD1_of_3_dud', root.template_name_dud)
      modify_template( root, t, t2, root.template_name, root.template_name_dud, -22, 25, -45, -28, 'horizontal', 1 )
      t=root.vcs[ root.vcs_id ].createtemplate('ASD2_of_3', root.template_name)
      t2=root.vcs[ root.vcs_id ].createtemplate('ASD2_of_3_dud', root.template_name_dud)
      modify_template( root, t, t2, root.template_name, root.template_name_dud, 27, 25, -45, -28, 'horizontal', 1 )
      t=root.vcs[ root.vcs_id ].createtemplate('ASD3_of_3', root.template_name)
      t2=root.vcs[ root.vcs_id ].createtemplate('ASD3_of_3_dud', root.template_name_dud)
      modify_template( root, t, t2, root.template_name, root.template_name_dud, 0, -25, -45, -28, 'horizontal', 0 )
      # molwd, robinson, and polar projection template
      t=root.vcs[ root.vcs_id ].createtemplate('ASD_map1of3', 'ASD_map')
      t2=root.vcs[ root.vcs_id ].createtemplate('ASD_map1of3_dud', 'ASD_map')
      modify_template( root, t, t2, 'ASD_map', 'ASD_map', -22, 25, -45, -28, 'horizontal', 1 )
      t=root.vcs[ root.vcs_id ].createtemplate('ASD_map2of3', 'ASD_map')
      t2=root.vcs[ root.vcs_id ].createtemplate('ASD_map2of3_dud', 'ASD_map')
      modify_template( root, t, t2, 'ASD_map', 'ASD_map', 27, 25, -45, -28, 'horizontal', 1 )
      t=root.vcs[ root.vcs_id ].createtemplate('ASD_map3of3', 'ASD_map')
      t2=root.vcs[ root.vcs_id ].createtemplate('ASD_map3of3_dud', 'ASD_map')
      modify_template( root, t, t2, 'ASD_map', 'ASD_map', 0, -25, -45, -28, 'horizontal', 0 )

   #
   # create templates for 4 multiple plots on a page
   try:
      t=root.vcs[ root.vcs_id ].gettemplate('ASD1_of_4')
   except:
      t=root.vcs[ root.vcs_id ].createtemplate('ASD1_of_4', root.template_name)
      t2=root.vcs[ root.vcs_id ].createtemplate('ASD1_of_4_dud', root.template_name_dud)
      modify_template( root, t, t2,root.template_name, root.template_name_dud, -22, 25, -45, -28, 'horizontal', 1 )
      t=root.vcs[ root.vcs_id ].createtemplate('ASD2_of_4', root.template_name)
      t2=root.vcs[ root.vcs_id ].createtemplate('ASD2_of_4_dud', root.template_name_dud)
      modify_template( root, t, t2, root.template_name, root.template_name_dud, 27, 25, -45, -28, 'horizontal', 1 )
      t=root.vcs[ root.vcs_id ].createtemplate('ASD3_of_4', root.template_name)
      t2=root.vcs[ root.vcs_id ].createtemplate('ASD3_of_4_dud', root.template_name_dud)
      modify_template( root, t, t2, root.template_name, root.template_name_dud, -22, -25, -45, -28, 'horizontal', 0 )
      t=root.vcs[ root.vcs_id ].createtemplate('ASD4_of_4', root.template_name)
      t2=root.vcs[ root.vcs_id ].createtemplate('ASD4_of_4_dud', root.template_name_dud)
      modify_template( root, t, t2, root.template_name, root.template_name_dud, 27, -25, -45, -28, 'horizontal', 0 )
      # ASD_map, robinson, and polar projection template
      t=root.vcs[ root.vcs_id ].createtemplate('ASD_map1of4', 'ASD_map')
      t2=root.vcs[ root.vcs_id ].createtemplate('ASD_map1of4_dud', 'ASD_map')
      modify_template( root, t, t2, 'ASD_map', 'ASD_map', -22, 25, -45, -28, 'horizontal', 1 )
      t=root.vcs[ root.vcs_id ].createtemplate('ASD_map2of4', 'ASD_map')
      t2=root.vcs[ root.vcs_id ].createtemplate('ASD_map2of4_dud', 'ASD_map')
      modify_template( root, t, t2, 'ASD_map', 'ASD_map', 27, 25, -45, -28, 'horizontal', 1 )
      t=root.vcs[ root.vcs_id ].createtemplate('ASD_map3of4', 'ASD_map')
      t2=root.vcs[ root.vcs_id ].createtemplate('ASD_map3of4_dud', 'ASD_map')
      modify_template( root, t, t2, 'ASD_map', 'ASD_map', -22, -25, -45, -28, 'horizontal', 0 )
      t=root.vcs[ root.vcs_id ].createtemplate('ASD_mapo4of4', 'ASD_map')
      t2=root.vcs[ root.vcs_id ].createtemplate('ASD_map4of4_dud', 'ASD_map')
      modify_template( root, t, t2, 'ASD_map', 'ASD_map', 27, -25, -45, -28, 'horizontal', 0 )

   # Initialize the Directory interface by storing the first directory into history list
   t = os.getcwd()
   gui_functions._scn_dir_history( root, t ) 
   for j in range(len( root.dir_history )): root.panelSV.tin2.insert( j, root.dir_history[j] )

#---------------------------------------------------------------------------
# Generate Box Discrete ranges and color indices
#---------------------------------------------------------------------------
def custom_ranges( min, max, num):
      rcolors = []
      l=[]
      levels=min
      delta = float( (max - min)/ (num) )
      d = int(206/(num-1))
      for a in range(int(num+1)):
         if -1.e-8<levels<1.e-8 and abs(levels)/delta<1.E-4:
            levels=0.
         l.append(levels)
         levels=levels+delta
         rcolors.append(16 + a*d)
      
      return tuple(l), tuple(rcolors)

#---------------------------------------------------------------------------
# Plot the data via VCS
#---------------------------------------------------------------------------
#def plot(parent, g_type, replot_flg, bg, formulate_data = 1):
def plot(parent=None, slab1=None, slab2=None, template=None, g_name=None, g_type=None, bg=0, id = 1):
   replot_flg = 0          # This was a parameter, May not need
   formulate_data = 1      # This was a parameter, May not need
   from_selected = 0
   return_value = "OK"
   template_name = template
   if template is None: template_name = parent.template_name
   if g_name=='default':
      raise

#   try:
#      if (parent.panelDM.var3 is not None):
#         slab1=gui_formulate.data( parent, d_name = parent.panelDM.var3)
#      else:
#         from_selected = 1
#         if (len(parent.panelDV.selected) == 0):
#            gui_message.error('Must first select a variable from the "Select Variable" panel above or from the "Defined Variables" list window below.')
#            return
#         if formulate_data == 1:
#            slab1=gui_formulate.data( parent, var = parent.panelDV.lst1[ cted ])
#         else:
#            slab1 =  parent.panelDV.lst1[ data1 ]
#
#         if (g_type in ['Vector', 'Scatter']):   # Vector or Scatter graphics method
#            try:
#               lst = parent.panelDV.selected_list.keys()
#               slab2=parent.panelDV.lst1[ data2 ]
#            except:
#               gui_message.error( "Vector or Scatter plots must have two data variables. These data variables must be selected in the 'Defined Variables' window.")
#               return
#   except:
#      _t,_v,_tr = sys.exc_info()
#      strt = str(_t)
#      if _v is not None:
#         strt += ': '+str(_v)
#      gui_message.error( strt)
#      return
   #
   # If slab1 is None, then return
   if slab1 is None: return "Error", None

   size = 1
   for i in range(len(slab1.shape)):
	size*=slab1.shape[i]
   if size == 1:
      parent.vcs[ parent.vcs_id ].clear()
      try:
         val = float(slab1)
         if parent.multiplier is not None:
            val = val * parent.multiplier
            gui_message.info( "The single point value is:\n%s" % val )
         else:
            gui_message.info( "The single point value is:\n%s" % val )
      except Exception,err:
         gui_message.info( "The single point value is masked %s\n" % err)
      return "break_off", None

   #
   # check to see if the data should be scaled. If so, then
   # call the function re_scale_data.
   #
   if (parent.multiplier is not None):
      slab1 = gui_formulate.re_scale_data( slab1, parent.multiplier )
      #if (g_type in ['Vector', 'Scatter', 'XvsY', "Meshfill"]):   # Vector, Scatter, or XvsY graphics method
      if (g_type in ['Vector', 'Scatter', 'XvsY']):   # Vector, Scatter, or XvsY graphics method, for Meshfill slab2 should be the mesh, no multiply
         slab2 = gui_formulate.re_scale_data( slab2, parent.multiplier )
   #
   # Check data's dimension size (VCS cannot take variables with
   # with dimensions larger than 4D, below makes sure the variable
   # has no more than 4 dimensions
   #
   while (len(slab1.shape) > 4):
      # Scale the first dimension down to size 1, then squeeze it out.
      slab1= (MV2.take(slab1,(0,),0)).subRegion( squeeze=1)
   s_shape = slab1.shape

   # Set the x and y annotation label from the data. At one point, this was done in 
   # "gui_annotate.py.
   try:
      parent.hannotate_xlabel=slab1.getAxis(len(slab1.shape)-1).id
   except: pass
   try: parent.hannotate_ylabel = slab1.getAxis(len(slab1.shape)-2).id,
   except: pass
   if g_type in ['Xyvsy']:
      tmp = parent.hannotate_ylabel
      parent.hannotate_ylabel = parent.hannotate_xlabel
      parent.hannotate_xlabel = tmp

   #       Vector, Scatter, or XvsY graphics method
   if g_type in [ "Vector", "Scatter", "XvsY"] or (g_type == "Meshfill" and len(parent.panelDV.selected_list)>=2):
##    if (g_type in ['Vector', 'Scatter', 'XvsY', "Meshfill"]):
      try:
         s_shape = slab2.shape
      except:
         gui_message.error( "Vector, Scatter, Meshfill or XvsY plots must have two data variables. These data variables must be selected in the 'Defined Variables' window.+++++++++++")
         return "Error", None
      if (len(s_shape) > 4):
         # Scale the first dimension down to size 1, then squeeze it out.
         slab2= (MV2.take(slab2,(0,),0)).subRegion( squeeze=1)
   #
   # set the plot annotation
   #
   # Reset VCDAT's annotation flag so that the Annotation GUI gets
   # information from the data and graphics method
   #if parent.annotate_start_over == 1 and parent.annotate_retain == 'no': 
   if parent.annotate_retain == 'no': 
      parent.annotate_get_info = 1 
      if (parent.annotate_view == 0) or (parent.annotate_start_over == 1):
         gui_annotate.get_annotation_info( parent, slab1.id )
         parent.annotate_var_name = parent.annotate_name

   ## Checks the keywords to pass to the plot function,
   ## The ones altered in annotation_editor
   plot_kw={} ## dictionary of keywords arguments

   plot_kw['yrev']=0
   
   plot_kw['continents']=parent.panelGC.cont_flg
   if bg==1:
      plot_kw['bg']=bg

   if parent.plot_ratio!=0:
      plot_kw['ratio']=parent.plot_ratio
      
   parent.annotate_start_over = 0
   source = parent.annotate_source
   aname = parent.annotate_name
   title = parent.annotate_title
   units = parent.annotate_units
   xlabel=parent.annotate_xlabel
   ylabel = parent.annotate_ylabel
   amultiplier = parent.multiplier
   lmultiplier = None
   legendlabel = parent.annotate_legendlabel

   
   gm_type = parent.panelGC.text_opt2.get()
   if from_selected == 1:
      if gm_type=='Xyvsy':
         ylabel_hold = slab1.getdimattribute( (len(slab1.shape)-1), 'name')
         xlabel_hold = slab1.getdimattribute( (len(slab1.shape)-2), 'name')
      else:
         xlabel_hold = slab1.getdimattribute( (len(slab1.shape)-1), 'name')
         ylabel_hold = slab1.getdimattribute( (len(slab1.shape)-2), 'name')
   if (source is not None):
      if source!=slab1.getattribute('source') and (source!='' and slab1.getattribute('source') is not None):
         plot_kw['file_comment']=source
         
   if (aname is not None):
      if aname != slab1.getattribute('id') and (aname!='' and slab1.getattribute('id') is not None):
         plot_kw['name']=aname

   if (title is not None):
      if title!=slab1.getattribute('title') and title!='':
         plot_kw['long_name']=title
         
   if (units is not None):
      if units !=slab1.getattribute('units') and (units!='' and slab1.getattribute('units') is not None):
         plot_kw['units']=units


   if xlabel is not None:
      if gm_type != 'Xyvsy':
         if xlabel!=slab1.getAxis(-1).id:
            plot_kw['xname']=xlabel
      elif ylabel is not None:
         plot_kw['xname']=ylabel
   if ylabel is not None:
      if ylabel!=slab1.getAxis(-2).id:
         plot_kw['yname']=ylabel
         
##    if xlabel is None:
##       try:
##          xlabel=slab1.getdimattribute( (len(slab1.shape)-1), 'name')
##          if xlabel is None:
##             xlabel = ""
##          else:
##             plot_kw['xname']=xlabel
##       except:
##          xlabel = ""
##    if ylabel is None:
##       try:
##          ylabel=slab1.getdimattribute( (len(slab1.shape)-2), 'name')
##          if ylabel is None:
##             ylabel = ""
##          else:
##             plot_kw['yname']=ylabel
##       except:
##          ylabel = ""
   if (amultiplier is not None):
      amultiplier = vcs.mklabels([1.0 / amultiplier],output='list')[0]
      lmultiplier = "Scaled by: "
      plot_kw['comment1']=amultiplier
      plot_kw['comment2']=lmultiplier

   # Set the source annotation text on the plot if specified by the annotation editor
   try: slab1.createattribute("source", source)
   except: slab1.setattribute("source", source)

   # Set the annotation legend label
   plot_kw['comment4']=legendlabel

   #
   # Check the template editor's priority setting for units, xname, and yname. If set to 0, then
   # set the appropriate annotation values to blank (i.e., ""). This is done because the plot parameters
   # for units, xname, and yname are always past in this function and if these values are past, then VCS
   # will automatically turn the priority value to 1, thus making the attribute visible on the plot even
   # if the template editor turns it off.
   #
##    unitp = getattr(getattr(template_p,'units'),'priority')
##    xlabelp = getattr(getattr(template_p,'xname'),'priority')
##    ylabelp = getattr(getattr(template_p,'yname'),'priority')
##    print '1 - template name = ', template_name, ': unitp = ', unitp, 'xlabelp = ', xlabelp, 'ylabelp = ', ylabelp
##    print 'keywords:',plot_kw

   template_p = parent.vcs[ parent.vcs_id ].gettemplate(template_name)
   if template_p.comment1.priority==0 and 'comment1' in plot_kw.keys():
      del(plot_kw['comment1'])
   if template_p.comment2.priority==0 and 'comment2' in plot_kw.keys():
      del(plot_kw['comment2'])
   if template_p.comment3.priority==0 and 'comment3' in plot_kw.keys():
      del(plot_kw['comment3'])
   if template_p.comment4.priority==0 and 'comment4' in plot_kw.keys():
      del(plot_kw['comment4'])
   if template_p.units.priority==0 and 'units' in plot_kw.keys():
      del(plot_kw['units'])
   if template_p.title.priority==0 and 'long_name' in plot_kw.keys():
      del(plot_kw['long_name'])
   if template_p.source.priority==0 and 'file_comment' in plot_kw.keys():
      del(plot_kw['units'])
   if template_p.dataname.priority==0 and 'name' in plot_kw.keys():
      del(plot_kw['name'])
   if template_p.xlabel1.priority==0 and 'xname' in plot_kw.keys():
      del(plot_kw['xname'])
   if template_p.ylabel1.priority==0 and 'yname' in plot_kw.keys():
      del(plot_kw['yname'])

      
   #**********************************************************************
   # If the canvas is not displayed, then use the VCS Canvas Geometry
   # setting if it exist. This flag will be used after the plot has
   # been drawn. Look for "first_canvas".
   #**********************************************************************
   first_canvas = 1
   if (parent.vcs[ parent.vcs_id ].iscanvasdisplayed()): first_canvas = 0

   #
   # Set the xmin, xmax, ymin, ymax values for the graphics method
   #
   page_layout_flg = 0
   Box_discrete_flg = 0                                         # Boxfill Discrete flag


   if g_type.split()[0]=='Ext':
      if g_type.split()[1][1:-1]=='boxfill':
         g_type='Boxfill'
      else:
         g_type='Ext'

   if (g_type in ['Boxfill', 'BoxDiscrete']):         		# Boxfill graphics method
##      print parent.vcs[ parent.vcs_id ].gettemplate(template_name).list()
##      print 'gname, etc...',g_name,template_name
     if (g_name != 'ASD') or (template_name[0:3] != 'ASD'):
##        print 'In if !!!!!!',g_name,template_name
       page_layout_flg = 1
       if g_name not in parent.vcs[ parent.vcs_id ].listelements('boxfill'):
##           print 'setting to default in boxfill'
          g_name = 'default'
          parent.pl.form[id].gm.setentry( g_name ) # Set the form line graphics entry
       parent.graphics_method = g_method = parent.vcs[ parent.vcs_id ].getboxfill(g_name)
       if g_method.name=='ASD':g_method.projection = parent.plot_projection

       if (template_name[0:3] == 'ASD' and template_name.find('of')==-1):
          template_name = set_template( parent, template_name )
          # DNW - 6.29.04
          # The browser should NOT be altering the appearance of the graphics method if it is not ASD, which
          # is always the case at this stage of the code. The below has been commented out. Watch for 
          # graphics method complaints. 
          # gui_alter_plot.settings( parent, replot_flg, g_name, template_name ) # Set the plot's visual appearance
     else:
##        print 'Inm else'
       if ((parent.panelGC.over_flg == 0) or (parent.Boxfillct[ parent.vcs_id ] == 0)):
          g_name = 'ASD'
          parent.Boxfillct[ parent.vcs_id ] = 1 + (parent.vcs_id*1000)
       else:
          g_name = 'ASD%d' % parent.Boxfillct[ parent.vcs_id ]
          parent.Boxfillct[ parent.vcs_id ] += 1

       sh=slab1.shape

       # Distribute the axis values to the poles using the sine function. The labeling is controled via the 
       # alter plot GUI.
       xdim = len( sh ) -1
       if (parent.x_axis_type == 'sine') and (slab1.getAxis(xdim).id in gui_control.latitude_alias):
          a = MV2.sin(slab1.getAxis(xdim)[:]*MV2.pi/180)
          old_dim = slab1.getAxis( xdim )
          old_dim[:] = a[:].astype(old_dim.dtype.char)
       ydim = len( sh ) -2
       if (parent.y_axis_type == 'sine') and (slab1.getAxis(ydim).id in gui_control.latitude_alias):
          a = MV2.sin(slab1.getAxis(ydim)[:]*MV2.pi/180)
          old_dim = slab1.getAxis( ydim )
          old_dim[:] = a[:].astype(old_dim.dtype.char)

       if len(sh) >= 2 or (len(sh)==1 and isinstance(slab1.getGrid(),cdms2.gengrid.AbstractGenericGrid)):
        if isinstance(slab1.getGrid(),cdms2.gengrid.AbstractGenericGrid):
	  ilen=sh[-1]
	else:
	  ilen=sh[-1]*sh[-2]
        try:
           minv, maxv = vcs.minmax(MV2.ravel(slab1)[:ilen])
        except Exception,err:
           gui_message.error('Unable to plot data! Please check data. Data must be either in integer or float format.')
           return "Error", None
        if replot_flg != 3:
           try:
              parent.graphics_method = g_method = parent.vcs[ parent.vcs_id ].getboxfill(g_name)
           except:
              parent.graphics_method = g_method = parent.vcs[ parent.vcs_id ].createboxfill(g_name, 'ASD')
        else:
           try:
              g_method = parent.graphics_method
           except:
              try:
                 parent.graphics_method = g_method = parent.vcs[ parent.vcs_id ].getboxfill(g_name)
              except:
                 parent.graphics_method = g_method = parent.vcs[ parent.vcs_id ].createboxfill(g_name, 'ASD')
        #
        # Set the graphics method boxfill type to either linear, log10, or custom
        g_method.boxfill_type = parent.boxfill_type

        #
        # set the map projection
        g_method.projection = parent.plot_projection

        template_name=dotemplate(parent,template,id,replot_flg, g_name)
        #
        # Record Graphics Method Min and Max
        gui_control.record_command( parent, "\n# Set Boxfill Method Attributes" )
        gui_control.record_command( parent, "g_method = vcs_canvas_list[ %d ].getboxfill('%s')" % (parent.vcs_id,g_name) )
        gui_control.record_command( parent, "g_method.boxfill_type = '%s'" % g_method.boxfill_type )
        gui_control.record_command( parent, "g_method.projection = '%s'" % g_method.projection )
        #
        g_method.level_1=1e20
        g_method.level_2=1e20
        g_method.color_1=16
        g_method.color_2=239
        g_method.ext_1='n'
        g_method.ext_2='n'
        #
        if ( ( (parent.app_d_min is None) and (parent.app_d_max is None) and
             (parent.boxfill_level1 is None) and (parent.boxfill_level2 is None) and
             g_method.boxfill_type in ['linear', 'custom'] ) or (g_type== 'BoxDiscrete') ):
           try:
              levs=vcs.mkscale( minv, maxv )
           except:
              levs=[]
              levs.append( minv )
           t_legend = g_method.legend=vcs.mklabels(levs)
           g_method.legend = t_legend
           gui_control.record_command( parent, "g_method.legend=%s" % str(g_method.legend) )
           g_method.level_1=levs[0]
           g_method.level_2=levs[-1]
        elif ((parent.boxfill_level1 is not None) or (parent.boxfill_level2 is not None)):
           g_method.legend=None
           gui_control.record_command( parent, "g_method.legend=%s" % g_method.legend )
           if parent.boxfill_level1 is not None:
              g_method.level_1 = parent.boxfill_level1
              gui_control.record_command( parent, "g_method.level_1=%s" % str(g_method.level_1) )
           else:
              g_method.level_1 = minv
           if parent.boxfill_level2 is not None:
              g_method.level_2 = parent.boxfill_level2
              gui_control.record_command( parent, "g_method.level_2=%s" % str(g_method.level_2) )
           else:
              g_method.level_2 = maxv
        elif ((parent.app_d_min is not None) or (parent.app_d_max is not None)):
           g_method.legend=None
           gui_control.record_command( parent, "g_method.legend=%s" % g_method.legend )
           if parent.app_d_min is not None:
              level_1 = parent.app_d_min
           else:
              level_1 = minv
           if parent.app_d_max is not None:
              level_2 = parent.app_d_max
           else:
              level_2 = maxv

           try:
              levs=vcs.mkscale( level_1, level_2 )
           except:
              levs=[]
              levs.append( minv )
           g_method.legend=vcs.mklabels(levs)
           gui_control.record_command( parent, "g_method.legend=%s" % str(g_method.legend) )
           g_method.level_1=levs[0]
           g_method.level_2=levs[-1]

        if (parent.boxfill_levels is not None):
           g_method.levels = parent.boxfill_levels

        if (parent.boxfill_fillareacolors is not None):
           g_method.fillareacolors = parent.boxfill_fillareacolors

        if (parent.boxfill_legend is not None):
           g_method.legend= parent.boxfill_legend

        if (parent.boxfill_color1 is not None) or (parent.boxfill_color2 is  not None):
           if parent.boxfill_color1 is not None:
              g_method.color_1 = parent.boxfill_color1
              gui_control.record_command( parent, "g_method.color_1=%s" % str(g_method.color_1) )
           else:
              g_method.color_1 = 16
           if parent.boxfill_color2 is not None:
              g_method.color_2 = parent.boxfill_color2
              gui_control.record_command( parent, "g_method.color_2=%s" % str(g_method.color_2) )
           else:
              g_method.color_2 = 239

        if (parent.boxfill_ext1 is not None) or (parent.boxfill_ext2 is  not None):
           if (parent.boxfill_ext1 is not None) and (parent.boxfill_ext1 == 'y'):
              g_method.ext_1 = str( parent.boxfill_ext1 )
              gui_control.record_command( parent, "g_method.ext_1='%s'" % str(g_method.ext_1) )
           else:
              g_method.ext_1 = 'n'
           if (parent.boxfill_ext2 is not None) and (parent.boxfill_ext2 == 'y'):
              g_method.ext_2 = str( parent.boxfill_ext2 )
              gui_control.record_command( parent, "g_method.ext_2='%s'" % str(g_method.ext_2) )
           else:
              g_method.ext_2 = 'n'

        #
        # If graphics method type is Boxfill Discrete, then reset boxfill type and set the levels
	# and fill area colors.
        #
        Box_discrete_type = parent.boxfill_type
        if g_type == 'BoxDiscrete':
            Box_discrete_flg = 1
            g_type = 'Boxfill'
            g_method.boxfill_type = 'custom'
            g_method.levels, g_method.fillareacolors = custom_ranges(  g_method.level_1, g_method.level_2, 10 )
            g_method.legend = vcs.mklabels(levs)
        else:
            g_method.boxfill_type = parent.boxfill_type
       else:
        g_type = "Yxvsx"
        parent.XYct = 0
        parent.XYct_restart=1
        gui_message.info('You may want to select "Yxvsx" or "Xyvsy" (instead of "Boxfill") in order to produce and control 1D plots.')
        return_value = "break_off"
   #
   #
   # Set the xmin, xmax, ymin, ymax values for the graphics method
   #
   elif (g_type == 'Meshfill'):        #       Meshfill graphics method
      if (g_name != 'ASD') or (template_name[0:3] != 'ASD'):
         page_layout_flg = 1
         if g_name not in parent.vcs[ parent.vcs_id ].listelements('meshfill'):
            g_name = 'default'
            parent.pl.form[id].gm.setentry( g_name ) # Set the form line graphics entry
         parent.graphics_method = g_method = parent.vcs[ parent.vcs_id ].getmeshfill(g_name)
         if g_method.name=='ASD':g_method.projection = parent.plot_projection
         if (template_name[0:3] == 'ASD'):
            template_name = set_template( parent, template_name )
            # DNW - 6.29.04
            # The browser should NOT be altering the appearance of the graphics method if it is not ASD, which
            # is always the case at this stage of the code. The below has been commented out. Watch for 
            # graphics method complaints. 
            # gui_alter_plot.settings( parent, replot_flg, g_name, template_name ) # Set the plot's visual appearance
      else:
         # Distribute the axis values to the poles using the sine function. The labeling is controled via the 
         # alter plot GUI.
         #sh=slab1.shape
         #xdim = len( sh ) -1
         #if (parent.x_axis_type == 'sine') and (slab1.getAxis(xdim).id in gui_control.latitude_alias):
         #   a = MV2.sin(slab1.getAxis(xdim)[:]*MV2.pi/180)
         #   old_dim = slab1.getAxis( xdim )
         #   old_dim[:] = a[:].astype(old_dim.dtype.char)
         #ydim = len( sh ) -2
         #if (parent.y_axis_type == 'sine') and (slab1.getAxis(ydim).id in gui_control.latitude_alias):
         #   a = MV2.sin(slab1.getAxis(ydim)[:]*MV2.pi/180)
         #   old_dim = slab1.getAxis( ydim )
         #   old_dim[:] = a[:].astype(old_dim.dtype.char)

         if ((parent.panelGC.over_flg == 0) or (parent.Meshfillct[ parent.vcs_id ] == 0)):
            g_name = 'ASD'
            parent.Meshfillct[ parent.vcs_id ] = 1 + (parent.vcs_id*1000)
         else:
            g_name = 'ASD%d' % parent.Meshfillct[ parent.vcs_id ]
            parent.Meshfillct[ parent.vcs_id ] += 1
         if replot_flg != 3:
            try:
               parent.graphics_method = g_method = parent.vcs[ parent.vcs_id ].getmeshfill(g_name)
            except:
               parent.graphics_method = g_method = parent.vcs[ parent.vcs_id ].createmeshfill(g_name, 'ASD')
         else:
            try:
               g_method = parent.graphics_method
            except:
               try:
                  parent.graphics_method = g_method = parent.vcs[ parent.vcs_id ].getmeshfill(g_name)
               except:
                  parent.graphics_method = g_method = parent.vcs[ parent.vcs_id ].createmeshfill(g_name, 'ASD')
         #
         #
         # set the map projection
         g_method.projection = parent.plot_projection

         # Set the isoline legend
         g_method.legend = parent.meshfill_legend

         template_name = dotemplate(parent,template,id,replot_flg, g_name)

         #
         # Record Graphics Method
         gui_control.record_command( parent, "\n# Set Meshfill Method Attributes" )
         gui_control.record_command( parent, "g_method = vcs_canvas_list[ %d ].getmeshfill('%s')" % (parent.vcs_id,g_name) )
         gui_control.record_command( parent, "g_method.projection = '%s'" % g_method.projection )

##          g_method.levels=parent.meshfill_levels
##          gui_control.record_command( parent, "g_method.levels=%s" % str(g_method.levels) )

##          g_method.fillareacolors=parent.meshfill_fillareacolors          
##          gui_control.record_command( parent, "g_method.fillareacolors=%s" % str(g_method.fillareacolors) )

         # Charles' code for nice labels
         if slab2 is None:
            m = slab1.getGrid().getMesh()
            sh = m.shape
            n = sh[0]
         else:
            sh =slab2.shape
            n =sh[0]
         minv,maxv=vcs.minmax(MV2.ravel(slab1).asma()[:n])
         try:
            levs=vcs.mkscale( minv, maxv )
         except:
            levs=[]
            levs.append( maxv )
         lst=[]
         rcolors = []
         try:
            d = int(222/(len(levs)-1))
         except:
            d = 1
         for a in range(len(levs)):
            rcolors.append(16 + a*d)
            lst.append(levs[a])
 ##        print '*********** rcolors 1 = ', rcolors
         g_method.levels=lst
         g_method.fillareacolors = rcolors
         #
         # End Charles' code for nice labels
         
         if (parent.iso_ranges is not None):
           rcolors = []
           l=[]
 ##          print '*********** rcolors 2 = ', rcolors 
           if (len(parent.iso_ranges) != 2):
             d = int(222/(len(parent.iso_ranges)-2))
           else:
             d = 0
           for a in range(len(parent.iso_ranges)):
              rcolors.append(16 + a*d)
              try:
                l.append(float(parent.iso_ranges[a]))
              except:
                parent.iso_ranges = None
                raise ValueError, 'invalid literal for float()'
           g_method.levels = l
           rcolors.remove(rcolors[-1])
           if (parent.iso_colors is not None):
              s = parent.iso_colors
              if type(s[0]) is types.StringType:
                 s = []
                 for x in parent.iso_colors: s.append(string.atoi(x))
              g_method.fillareacolors = s
           else:
              g_method.fillareacolors = rcolors
         elif ((parent.iso_min is not None) and (parent.iso_max is not None) and
               (parent.iso_num is not None)):
            rcolors = []
            l=[]
            levels=parent.iso_min
            delta = (parent.iso_max - parent.iso_min)/ (parent.iso_num)
            d = int(222/(parent.iso_num-1))
 ##           print '*********** rcolors 3 = ', rcolors
            for a in range(parent.iso_num+1):
               l.append(levels)
               levels=levels+delta
               rcolors.append(16 + a*d)
            rcolors.remove(rcolors[-1])
            g_method.levels = l
            g_method.fillareacolors = rcolors
         elif (parent.app_d_min is not None) or (parent.app_d_max is not None):
 ##           print '*********** rcolors 4 = ', rcolors
            if parent.app_d_min is not None:
               d_min = parent.app_d_min
            else:
               d_min = minv
            if parent.app_d_max is not None:
               d_max = parent.app_d_max
            else:
               d_max = maxv
            l=[]
            interations = 10
            levels = d_min
            delta = (d_max - d_min)/ float(interations)
            for a in range(interations):
               l.append(levels)
               levels=levels+delta
            l.append(levels)
            g_method.levels = l
            g_method.fillareacolors = [16,38,80,103,112,127,158,183,192,207]

         # Set the isoline legend
         g_method.legend = parent.iso_legend
         #
         gui_control.record_command( parent, "g_method.levels=%s" % str(g_method.levels) )
         gui_control.record_command( parent, "g_method.fillareacolors=%s" % str(g_method.fillareacolors) )



         g_method.wrap=parent.meshfill_wrap
         gui_control.record_command( parent, "g_method.wrap=%s" % str(g_method.wrap) )

         g_method.mesh=parent.meshfill_mesh
         gui_control.record_command( parent, "g_method.mesh=%s" % str(g_method.mesh) )
         
   elif (g_type == 'Taylordiagram'):         #       taylordiagram graphics method
     if (g_name != 'ASD') or (template_name[0:3] != 'ASD'):
       page_layout_flg = 1
       if g_name not in parent.vcs[ parent.vcs_id ].listelements('taylordiagram'):
          g_name = 'default'
          parent.pl.form[id].gm.setentry( g_name ) # Set the form line graphics entry
       parent.graphics_method = g_method = parent.vcs[ parent.vcs_id ].gettaylordiagram(g_name)
       if (template_name[0:3] == 'ASD'):
          template_name = set_template( parent, template_name )
          # DNW - 6.29.04
          # The browser should NOT be altering the appearance of the graphics method if it is not ASD, which
          # is always the case at this stage of the code. The below has been commented out. Watch for 
          # graphics method complaints. 
          # gui_alter_plot.settings( parent, replot_flg, g_name, template_name ) # Set the plot's visual appearance
     else:
       if ((parent.panelGC.over_flg == 0) or (parent.Taylordiagramct[ parent.vcs_id ] == 0)):
          g_name = 'ASD'
          parent.Taylordiagramct[ parent.vcs_id ] = 1 + (parent.vcs_id*1000)
       else:
          g_name = 'ASD%d' % parent.Taylordiagramct[ parent.vcs_id ]
          parent.Taylordiagramct[ parent.vcs_id ] += 1

       sh=slab1.shape
       ilen=sh[-1]*sh[-2]

       # Distribute the axis values to the poles using the sine function. The labeling is controled via the 
       # alter plot GUI.
       xdim = len( sh ) -1
       if (parent.x_axis_type == 'sine') and (slab1.getAxis(xdim).id in gui_control.latitude_alias):
          a = MV2.sin(slab1.getAxis(xdim)[:]*MV2.pi/180)
          old_dim = slab1.getAxis( xdim )
          old_dim[:] = a[:].astype(old_dim.dtype.char)
       ydim = len( sh ) -2
       if (parent.y_axis_type == 'sine') and (slab1.getAxis(ydim).id in gui_control.latitude_alias):
          a = MV2.sin(slab1.getAxis(ydim)[:]*MV2.pi/180)
          old_dim = slab1.getAxis( ydim )
          old_dim[:] = a[:].astype(old_dim.dtype.char)

       try:
          minv, maxv = vcs.minmax(slab1[:,0])
       
       except Exception,err:
          gui_message.error('Unable to plot data! Please check data. Data must be either in integer or float format.')
          return "Error", None
       if replot_flg != 3:
          try:
             parent.graphics_method = g_method = parent.vcs[ parent.vcs_id ].gettaylordiagram(g_name)
          except:
             parent.graphics_method = g_method = parent.vcs[ parent.vcs_id ].createtaylordiagram(g_name,'ASD')
       else:
          try:
             g_method = parent.graphics_method
          except:
             try:
                parent.graphics_method = g_method = parent.vcs[ parent.vcs_id ].gettaylordiagram(g_name)
             except:
                parent.graphics_method = g_method = parent.vcs[ parent.vcs_id ].createtaylordiagram(g_name, 'ASD')
       #
       template_name = dotemplate(parent,template,id,replot_flg, g_name)
       #
       # Record Graphics Method Min and Max
       gui_control.record_command( parent, "\n# Set the Graphics Method Max Value" )
       gui_control.record_command( parent, "g_method = vcs_canvas_list[ %d ].gettaylordiagram(%s)" % (parent.vcs_id, g_name) )
       #
       g_method.max=maxv
       #
   #
   elif (g_type == 'Isofill'):       #       isofill graphics method
     if (g_name != 'ASD') or (template_name[0:3] != 'ASD'):
       page_layout_flg = 1
       if g_name not in parent.vcs[ parent.vcs_id ].listelements('isofill'):
          g_name = 'default'
          parent.pl.form[id].gm.setentry( g_name ) # Set the form line graphics entry
       parent.graphics_method = g_method = parent.vcs[ parent.vcs_id ].getisofill(g_name)
       if g_method.name == 'ASD':g_method.projection = parent.plot_projection
       if (template_name[0:3] == 'ASD'):
          template_name = set_template( parent, template_name )
          # DNW - 6.29.04
          # The browser should NOT be altering the appearance of the graphics method if it is not ASD, which
          # is always the case at this stage of the code. The below has been commented out. Watch for 
          # graphics method complaints. 
          # gui_alter_plot.settings( parent, replot_flg, g_name, template_name ) # Set the plot's visual appearance
     else:
       if ((parent.panelGC.over_flg == 0) or (parent.Isofillct[ parent.vcs_id ] == 0)):
          g_name = 'ASD'
          parent.Isofillct[ parent.vcs_id ] = 1 + (parent.vcs_id*1000)
       else:
          g_name = 'ASD%d' % parent.Isofillct[ parent.vcs_id ]
          parent.Isofillct[ parent.vcs_id ] += 1

       sh=slab1.shape

       # Distribute the axis values to the poles using the sine function. The labeling is controled via the 
       # alter plot GUI.
       xdim = len( sh ) -1
       if (parent.x_axis_type == 'sine') and (slab1.getAxis(xdim).id in gui_control.latitude_alias):
          a = MV2.sin(slab1.getAxis(xdim)[:]*MV2.pi/180)
          old_dim = slab1.getAxis( xdim )
          old_dim[:] = a[:].astype(old_dim.dtype.char)
       ydim = len( sh ) -2
       if (parent.y_axis_type == 'sine') and (slab1.getAxis(ydim).id in gui_control.latitude_alias):
          a = MV2.sin(slab1.getAxis(ydim)[:]*MV2.pi/180)
          old_dim = slab1.getAxis( ydim )
          old_dim[:] = a[:].astype(old_dim.dtype.char)

       if len(sh) >= 2:
        ilen=sh[-1]*sh[-2]
        try:
           minv, maxv = vcs.minmax(MV2.ravel(slab1)[:ilen])
        
        except Exception,err:
           gui_message.error('Unable to plot data! Please check data. Data must be either in integer or float format.')
           return "Error", None
        if replot_flg != 3:
           try:
             parent.graphics_method = g_method = parent.vcs[ parent.vcs_id ].getisofill(g_name)
           except:
             parent.graphics_method = g_method = parent.vcs[ parent.vcs_id ].createisofill(g_name,'ASD')
        else:
           try:
              g_method = parent.graphics_method
           except:
              try:
                 parent.graphics_method = g_method = parent.vcs[ parent.vcs_id ].getisofill(g_name)
              except:
                 parent.graphics_method = g_method = parent.vcs[ parent.vcs_id ].createisofill(g_name, 'ASD')
        #
        # set the map projection
        g_method.projection = parent.plot_projection
         
        template_name = dotemplate(parent,template,id,replot_flg, g_name)
        #
        # Record Graphics Method Min and Max
        gui_control.record_command( parent, "\n# Set Isofill Method Attributes" )
        gui_control.record_command( parent, "g_method = vcs_canvas_list[ %d ].getisofill('%s')" % (parent.vcs_id, g_name) )
        gui_control.record_command( parent, "g_method.projection = '%s'" % g_method.projection )
        #
        # Charles' code for nice labels
        try:
           levs=vcs.mkscale( minv, maxv )
        except:
           levs=[]
           levs.append( maxv )
        lst=[]
        rcolors = []
        try:
           d = int(222/(len(levs)-1))
        except:
           d = 1
        for a in range(len(levs)):
           rcolors.append(16 + a*d)
           lst.append(levs[a])
 #       print '*********** rcolors 1 = ', rcolors
        g_method.levels=lst
        g_method.fillareacolors = rcolors
        #
        # End Charles' code for nice labels
 
        if (parent.iso_ranges is not None):
           rcolors = []
           l=[]
 #          print '*********** rcolors 2 = ', rcolors
           if (len(parent.iso_ranges) != 2):
             d = int(222/(len(parent.iso_ranges)-2))
           else:
             d = 0
           for a in range(len(parent.iso_ranges)):
              rcolors.append(16 + a*d)
              try:
                l.append(float(parent.iso_ranges[a]))
              except:
                parent.iso_ranges = None
                raise ValueError, 'invalid literal for float()'
           g_method.levels = l
           rcolors.remove(rcolors[-1])
           if (parent.iso_colors is not None):
              s = parent.iso_colors
              if type(s[0]) is types.StringType:
                 s = []
                 for x in parent.iso_colors: s.append(string.atoi(x))
              g_method.fillareacolors = s
           else:
              g_method.fillareacolors = rcolors
        elif ((parent.iso_min is not None) and (parent.iso_max is not None) and
             (parent.iso_num is not None)):
           rcolors = []
           l=[]
           levels=parent.iso_min
           delta = (parent.iso_max - parent.iso_min)/ (parent.iso_num)
           d = int(222/(parent.iso_num-1))
 #          print '*********** rcolors 3 = ', rcolors
           for a in range(parent.iso_num+1):
              l.append(levels)
              levels=levels+delta
              rcolors.append(16 + a*d)
           rcolors.remove(rcolors[-1])
           g_method.levels = l
           g_method.fillareacolors = rcolors
        elif (parent.app_d_min is not None) or (parent.app_d_max is not None):
 #          print '*********** rcolors 4 = ', rcolors
           if parent.app_d_min is not None:
              d_min = parent.app_d_min
           else:
              d_min = minv
           if parent.app_d_max is not None:
              d_max = parent.app_d_max
           else:
              d_max = maxv
           l=[]
           interations = 10
           levels = d_min
           delta = (d_max - d_min)/ float(interations)
           for a in range(interations):
              l.append(levels)
              levels=levels+delta
           l.append(levels)
           g_method.levels = l
           g_method.fillareacolors = [16,38,80,103,112,127,158,183,192,207]

        # Set the isoline legend
        g_method.legend = parent.iso_legend
        #
        gui_control.record_command( parent, "g_method.levels=%s" % str(g_method.levels) )
        gui_control.record_command( parent, "g_method.fillareacolors=%s" % str(g_method.fillareacolors) )
       else:
        g_type = "Yxvsx"
        parent.XYct = 0
        parent.XYct_restart=1
        gui_message.info('You may want to select "Yxvsx" or "Xyvsy" (instead of "Boxfill") in order to produce and control 1D plots.')
        return_value = "break_off"
   elif (g_type == 'Isoline'):       #       Isoline graphics method
     if (g_name != 'ASD') or (template_name[0:3] != 'ASD'):
       page_layout_flg = 1
       if g_name not in parent.vcs[ parent.vcs_id ].listelements('isoline'):
          g_name = 'default'
          parent.pl.form[id].gm.setentry( g_name ) # Set the form line graphics entry
       parent.graphics_method = g_method = parent.vcs[ parent.vcs_id ].getisoline(g_name)
       if g_method.name == 'ASD':g_method.projection = parent.plot_projection
       if (template_name[0:3] == 'ASD'):
          template_name = set_template( parent, template_name )
          # DNW - 6.29.04
          # The browser should NOT be altering the appearance of the graphics method if it is not ASD, which
          # is always the case at this stage of the code. The below has been commented out. Watch for 
          # graphics method complaints. 
          # gui_alter_plot.settings( parent, replot_flg, g_name, template_name ) # Set the plot's visual appearance
     else:
       if ((parent.panelGC.over_flg == 0) or (parent.Isolinect[ parent.vcs_id ] == 0)):
          g_name = 'ASD'
          parent.Isolinect[ parent.vcs_id ] = 1 + (parent.vcs_id*1000)
       else:
          g_name = 'ASD%d' % parent.Isolinect[ parent.vcs_id ]
          parent.Isolinect[ parent.vcs_id ] += 1

       sh=slab1.shape

       # Distribute the axis values to the poles using the sine function. The labeling is controled via the 
       # alter plot GUI.
       xdim = len( sh ) -1
       if (parent.x_axis_type == 'sine') and (slab1.getAxis(xdim).id in gui_control.latitude_alias):
          a = MV2.sin(slab1.getAxis(xdim)[:]*MV2.pi/180)
          old_dim = slab1.getAxis( xdim )
          old_dim[:] = a[:].astype(old_dim.dtype.char)
       ydim = len( sh ) -2
       if (parent.y_axis_type == 'sine') and (slab1.getAxis(ydim).id in gui_control.latitude_alias):
          a = MV2.sin(slab1.getAxis(ydim)[:]*MV2.pi/180)
          old_dim = slab1.getAxis( ydim )
          old_dim[:] = a[:].astype(old_dim.dtype.char)

       if len(sh) >= 2:
        ilen=sh[-1]*sh[-2]
        try:
           minv, maxv = vcs.minmax(MV2.ravel(slab1)[:ilen])
        
        except Exception,err:
           gui_message.error('Unable to plot data! Please check data. Data must be either in integer or float format.')
           return "Error", None
        if replot_flg != 3:
           try:
             parent.graphics_method = g_method = parent.vcs[ parent.vcs_id ].getisoline(g_name)
           except:
             parent.graphics_method = g_method = parent.vcs[ parent.vcs_id ].createisoline(g_name,'ASD')
        else:
           try:
              g_method = parent.graphics_method
           except:
              try:
                parent.graphics_method = g_method = parent.vcs[ parent.vcs_id ].getisoline(g_name)
              except:
                parent.graphics_method = g_method = parent.vcs[ parent.vcs_id ].createisoline(g_name,'ASD')
        parent.graphics_method.label=parent.panelGC.isol_label_flg
        #
        # set the map projection
        g_method.projection = parent.plot_projection
         
        template_name = dotemplate(parent,template,id,replot_flg, g_name)
        #
        # Record Graphics Method Min and Max
        # Record Graphics Method
        gui_control.record_command( parent, "\n# Set Isoline Method Attributes" )
        gui_control.record_command( parent, "g_method = vcs_canvas_list[ %d ].getisoline('%s')" % (parent.vcs_id,g_name) )
        s= "g_method.label = %s" % parent.panelGC.isol_label_flg
        gui_control.record_command( parent, s)
        #
        # Charles' code for nice labels
        try:
           levs=vcs.mkscale( minv, maxv )
        except:
           levs=[]
           levs.append( maxv )
        lst=[]
        for a in range(len(levs)):
           lst.append(levs[a])
        g_method.level=lst
        # End Charles' code for nice labels
        g_method.linecolors = None
        if (parent.iso_ranges is not None):
           l=[]
           for a in range(len(parent.iso_ranges)):
              try:
                l.append(float(parent.iso_ranges[a]))
              except:
                parent.iso_ranges = None
                raise ValueError, 'invalid literal for float()'
           g_method.level = l
           if (parent.iso_colors is not None):
              s = parent.iso_colors
              if type(s[0]) is types.StringType:
                 s = []
                 for x in parent.iso_colors: s.append(string.atoi(x))
              g_method.linecolors = s
           else:
              g_method.linecolors = None
        elif ((parent.iso_min is not None) and (parent.iso_max is not None) and
             (parent.iso_num is not None)):
           l=[]
           levels=parent.iso_min
           delta = (parent.iso_max - parent.iso_min)/ (parent.iso_num)
           for a in range(parent.iso_num+1):
              l.append(levels)
              levels=levels+delta
           g_method.level = l
        elif (parent.app_d_min is not None) or (parent.app_d_max is not None):
           if parent.app_d_min is not None:
              d_min = parent.app_d_min
           else:
              d_min = minv
           if parent.app_d_max is not None:
              d_max = parent.app_d_max
           else:
              d_max = maxv
           l=[]
           interations = 10.0
           levels = d_min
           delta = (d_max - d_min)/ interations
           for a in range(interations):
              l.append(levels)
              levels=levels+delta
           l.append(levels)
           g_method.level = l
        gui_control.record_command( parent, "g_method.level=%s" % str(g_method.level) )
        gui_control.record_command( parent, "g_method.linecolors=%s" % str(g_method.linecolors) )

        if (parent.iso_line_types is not None):
           g_method.line = parent.iso_line_types
        else:
           g_method.line = []
        if (parent.iso_line_widths is not None):
           g_method.linewidths = parent.iso_line_widths
        else:
           g_method.linewidths = []
        if (parent.iso_line_clockwise is not None):
           g_method.clockwise = parent.iso_line_clockwise
        else:
           g_method.clockwise = []
        if (parent.iso_line_arrow_scale is not None):
           g_method.scale = parent.iso_line_arrow_scale
        else:
           g_method.scale = []
        if (parent.iso_line_arrow_spacing is not None):
           g_method.spacing = parent.iso_line_arrow_spacing
        else:
           g_method.spacing = []
        if (parent.iso_line_arrow_angle is not None):
           g_method.angle = parent.iso_line_arrow_angle
        else:
           g_method.angle = []
       else:
        g_type = "Yxvsx"
        parent.XYct = 0
        parent.XYct_restart=1
        gui_message.info('You may want to select "Yxvsx" or "Xyvsy" (instead of "Boxfill") in order to produce and control 1D plots.')
        return_value = "break_off"
   elif (g_type == 'Outfill'):       #       Outfill graphics method
     if (g_name != 'ASD') or (template_name[0:3] != 'ASD'):
       page_layout_flg = 1
       if g_name not in parent.vcs[ parent.vcs_id ].listelements('outfill'):
          g_name = 'default'
          parent.pl.form[id].gm.setentry( g_name ) # Set the form line graphics entry
       parent.graphics_method = g_method = parent.vcs[ parent.vcs_id ].getoutfill(g_name)
       if g_method.name == 'ASD':g_method.projection = parent.plot_projection
       if (template_name[0:3] == 'ASD'):
          template_name = set_template( parent, template_name )
          # DNW - 6.29.04
          # The browser should NOT be altering the appearance of the graphics method if it is not ASD, which
          # is always the case at this stage of the code. The below has been commented out. Watch for 
          # graphics method complaints. 
          # gui_alter_plot.settings( parent, replot_flg, g_name, template_name ) # Set the plot's visual appearance
     else:
       if ((parent.panelGC.over_flg == 0) or (parent.Outfillct[ parent.vcs_id ] == 0)):
          g_name = 'ASD'
          parent.Outfillct[ parent.vcs_id ] = 1 + (parent.vcs_id*1000)
       else:
          g_name = 'ASD%d' % parent.Outfillct[ parent.vcs_id ]
          parent.Outfillct[ parent.vcs_id ] += 1

       sh=slab1.shape

       # Distribute the axis values to the poles using the sine function. The labeling is controled via the 
       # alter plot GUI.
       xdim = len( sh ) -1
       if (parent.x_axis_type == 'sine') and (slab1.getAxis(xdim).id in gui_control.latitude_alias):
          a = MV2.sin(slab1.getAxis(xdim)[:]*MV2.pi/180)
          old_dim = slab1.getAxis( xdim )
          old_dim[:] = a[:].astype(old_dim.dtype.char)
       ydim = len( sh ) -2
       if (parent.y_axis_type == 'sine') and (slab1.getAxis(ydim).id in gui_control.latitude_alias):
          a = MV2.sin(slab1.getAxis(ydim)[:]*MV2.pi/180)
          old_dim = slab1.getAxis( ydim )
          old_dim[:] = a[:].astype(old_dim.dtype.char)

       if len(sh) >= 2:
        if replot_flg != 3:
            try:
              parent.graphics_method = g_method = parent.vcs[ parent.vcs_id ].getoutfill(g_name)
            except:
              parent.graphics_method = g_method = parent.vcs[ parent.vcs_id ].createoutfill(g_name,'ASD')
        else:
            try:
               g_method = parent.graphics_method
            except:
               try:
                  parent.graphics_method = g_method = parent.vcs[ parent.vcs_id ].getoutfill(g_name)
               except:
                  parent.graphics_method = g_method = parent.vcs[ parent.vcs_id ].createoutfill(g_name, 'ASD')
        #
        # set the map projection
        g_method.projection = parent.plot_projection
 
        template_name = dotemplate(parent,template,id,replot_flg, g_name)
        #
        # Set the Outfill attributes
        g_method.fillareastyle = parent.outf_style
        if parent.outf_index is not None:
           g_method.fillareaindex = string.atoi(str(parent.outf_index))
        if parent.outf_fillcolor is not None:
           g_method.fillareacolor = string.atoi(str(parent.outf_fillcolor))
        g_method.outfill = parent.outf_outfill
        #
        # Record Outfill commands
        gui_control.record_command( parent, "\n# Set Outfill Method Attributes" )
        gui_control.record_command( parent, "g_method = vcs_canvas_list[ %d ].getoutfill('%s')" % (parent.vcs_id,g_name) )
        gui_control.record_command( parent, "g_method.fillareastyle='%s'" % str(g_method.fillareastyle) )
        if g_method.fillareaindex is not None:
           gui_control.record_command( parent, "g_method.fillareaindex=%s" % str(g_method.fillareaindex) )
        if g_method.fillareacolor is not None:
           gui_control.record_command( parent, "g_method.fillcolor=%s" % str(g_method.fillareacolor) )
        gui_control.record_command( parent, "g_method.outfill=%s" % str(g_method.outfill))
       else:
        g_type = "Yxvsx"
        parent.XYct = 0
        parent.XYct_restart=1
        gui_message.info('You may want to select "Yxvsx" or "Xyvsy" (instead of "Boxfill") in order to produce and control 1D plots.')
        return_value = "break_off"
   elif (g_type == 'Outline'):       #       Outline graphics method
     if (g_name != 'ASD') or (template_name[0:3] != 'ASD'):
       page_layout_flg = 1
       if g_name not in parent.vcs[ parent.vcs_id ].listelements('outline'):
          g_name = 'default'
          parent.pl.form[id].gm.setentry( g_name ) # Set the form line graphics entry
       parent.graphics_method = g_method = parent.vcs[ parent.vcs_id ].getoutline(g_name)
       if g_method.name == 'ASD':g_method.projection = parent.plot_projection
       if (template_name[0:3] == 'ASD'):
          template_name = set_template( parent, template_name )
          # DNW - 6.29.04
          # The browser should NOT be altering the appearance of the graphics method if it is not ASD, which
          # is always the case at this stage of the code. The below has been commented out. Watch for 
          # graphics method complaints. 
          # gui_alter_plot.settings( parent, replot_flg, g_name, template_name ) # Set the plot's visual appearance
     else:
       if ((parent.panelGC.over_flg == 0) or (parent.Outlinect[ parent.vcs_id ] == 0)):
          g_name = 'ASD'
          parent.Outlinect[ parent.vcs_id ] = 1 + (parent.vcs_id*1000)
       else:
          g_name = 'ASD%d' % parent.Outlinect[ parent.vcs_id ]
          parent.Outlinect[ parent.vcs_id ] += 1

       sh=slab1.shape

       # Distribute the axis values to the poles using the sine function. The labeling is controled via the 
       # alter plot GUI.
       xdim = len( sh ) -1
       if (parent.x_axis_type == 'sine') and (slab1.getAxis(xdim).id in gui_control.latitude_alias):
          a = MV2.sin(slab1.getAxis(xdim)[:]*MV2.pi/180)
          old_dim = slab1.getAxis( xdim )
          old_dim[:] = a[:].astype(old_dim.dtype.char)
       ydim = len( sh ) -2
       if (parent.y_axis_type == 'sine') and (slab1.getAxis(ydim).id in gui_control.latitude_alias):
          a = MV2.sin(slab1.getAxis(ydim)[:]*MV2.pi/180)
          old_dim = slab1.getAxis( ydim )
          old_dim[:] = a[:].astype(old_dim.dtype.char)

       if len(sh) >= 2:
        if replot_flg != 3:
            try:
              parent.graphics_method = g_method = parent.vcs[ parent.vcs_id ].getoutline(g_name)
            except:
              parent.graphics_method = g_method = parent.vcs[ parent.vcs_id ].createoutline(g_name,'ASD')
        else:
            try:
               g_method = parent.graphics_method
            except:
               try:
                  parent.graphics_method = g_method = parent.vcs[ parent.vcs_id ].getoutline(g_name)
               except:
                  parent.graphics_method = g_method = parent.vcs[ parent.vcs_id ].createoutline(g_name, 'ASD')
        #
        # set the map projection
        g_method.projection = parent.plot_projection
 
        template_name = dotemplate(parent,template,id,replot_flg, g_name)
 
        #
        # Set the Outline attributes
        g_method.line = parent.outl_line
        if parent.outl_linecolor is not None:
           g_method.linecolor = string.atoi(str(parent.outl_linecolor))
        g_method.outline = parent.outl_outline
        #
        # Record Outline commands
        gui_control.record_command( parent, "\n# Set Outline Method Attributes" )
        gui_control.record_command( parent, "g_method = vcs_canvas_list[ %d ].getoutline('%s')" % (parent.vcs_id,g_name) )
        gui_control.record_command( parent, "g_method.line='%s'" % str(g_method.line) )
        if g_method.linecolor is not None:
           gui_control.record_command( parent, "g_method.linecolor=%s" % str(g_method.linecolor))
        gui_control.record_command( parent, "g_method.outline=%s" % str(g_method.outline))
       else:
        g_type = "Yxvsx"
        parent.XYct = 0
        parent.XYct_restart=1
        gui_message.info('You may want to select "Yxvsx" or "Xyvsy" (instead of "Boxfill") in order to produce and control 1D plots.')
        return_value = "break_off"
   elif (g_type == 'Vector'):        #       Vector graphics method
     if (g_name != 'ASD') or (template_name[0:3] != 'ASD'):
       page_layout_flg = 1
       if g_name not in parent.vcs[ parent.vcs_id ].listelements('vector'):
          g_name = 'default'
          parent.pl.form[id].gm.setentry( g_name ) # Set the form line graphics entry
       parent.graphics_method = g_method = parent.vcs[ parent.vcs_id ].getvector(g_name)
       if g_method.name == 'ASD':g_method.projection = parent.plot_projection
       if (template_name[0:3] == 'ASD'):
          template_name = set_template( parent, template_name )
          # DNW - 6.29.04
          # The browser should NOT be altering the appearance of the graphics method if it is not ASD, which
          # is always the case at this stage of the code. The below has been commented out. Watch for 
          # graphics method complaints. 
          # gui_alter_plot.settings( parent, replot_flg, g_name, template_name ) # Set the plot's visual appearance
     else:

       # Distribute the axis values to the poles using the sine function. The labeling is controled via the 
       # alter plot GUI.
       sh=slab1.shape
       xdim = len( sh ) -1
       if (parent.x_axis_type == 'sine') and (slab1.getAxis(xdim).id in gui_control.latitude_alias):
          a = MV2.sin(slab1.getAxis(xdim)[:]*MV2.pi/180)
          old_dim = slab1.getAxis( xdim )
          old_dim[:] = a[:].astype(old_dim.dtype.char)
       ydim = len( sh ) -2
       if (parent.y_axis_type == 'sine') and (slab1.getAxis(ydim).id in gui_control.latitude_alias):
          a = MV2.sin(slab1.getAxis(ydim)[:]*MV2.pi/180)
          old_dim = slab1.getAxis( ydim )
          old_dim[:] = a[:].astype(old_dim.dtype.char)

       if ((parent.panelGC.over_flg == 0) or (parent.Vectorct[ parent.vcs_id ] == 0)):
          g_name = 'ASD'
          parent.Vectorct[ parent.vcs_id ] = 1 + (parent.vcs_id*1000)
       else:
          g_name = 'ASD%d' % parent.Vectorct[ parent.vcs_id ]
          parent.Vectorct[ parent.vcs_id ] += 1
       if replot_flg != 3:
          try:
             parent.graphics_method = g_method = parent.vcs[ parent.vcs_id ].getvector(g_name)
          except:
             parent.graphics_method = g_method = parent.vcs[ parent.vcs_id ].createvector(g_name, 'ASD')
       else:
          try:
             g_method = parent.graphics_method
          except:
             try:
                parent.graphics_method = g_method = parent.vcs[ parent.vcs_id ].getvector(g_name)
             except:
                parent.graphics_method = g_method = parent.vcs[ parent.vcs_id ].createvector(g_name, 'ASD')
       #
       g_method.line = parent.vec_line
       if parent.vec_linecolor is not None:
          g_method.linecolor = string.atoi(str(parent.vec_linecolor))
       if parent.vec_scale is not None:
          g_method.scale = string.atof(str(parent.vec_scale))
       g_method.alignment = parent.vec_alignment
       g_method.type = parent.vec_type
       g_method.reference = string.atof(str(parent.vec_ref))

       #
       # set the map projection
       g_method.projection = parent.plot_projection
        
       template_name = dotemplate(parent,template,id,replot_flg, g_name)
       #
       # Record Graphics Method
       gui_control.record_command( parent, "\n# Set Vector Method Attributes" )
       gui_control.record_command( parent, "g_method = vcs_canvas_list[ %d ].getvector('%s')" % (parent.vcs_id,g_name) )
       gui_control.record_command( parent, "g_method.line=%s" % str(g_method.line) )
       gui_control.record_command( parent, "g_method.linecolor=%s" % str(g_method.linecolor))
       gui_control.record_command( parent, "g_method.scale=%s" % str(g_method.scale))
       gui_control.record_command( parent, "g_method.alignment='%s'" % str(g_method.alignment))
       gui_control.record_command( parent, "g_method.type='%s'" % str(g_method.type))
       gui_control.record_command( parent, "g_method.reference=%s" % str(g_method.reference))


   elif (g_type == 'Scatter'):       #      Scatter graphics method
     if (g_name != 'ASD') or (template_name[0:3] != 'ASD'):
       page_layout_flg = 1
       if g_name not in parent.vcs[ parent.vcs_id ].listelements('scatter'):
          g_name = 'default'
          parent.pl.form[id].gm.setentry( g_name ) # Set the form line graphics entry
       parent.graphics_method = g_method = parent.vcs[ parent.vcs_id ].getscatter(g_name)
       if g_method.name == 'ASD':g_method.projection = parent.plot_projection
       if (template_name[0:3] == 'ASD'):
          template_name = set_template( parent, template_name )
          # DNW - 6.29.04
          # The browser should NOT be altering the appearance of the graphics method if it is not ASD, which
          # is always the case at this stage of the code. The below has been commented out. Watch for 
          # graphics method complaints. 
          # gui_alter_plot.settings( parent, replot_flg, g_name, template_name ) # Set the plot's visual appearance
     else:

       # Distribute the axis values to the poles using the sine function. The labeling is controled via the 
       # alter plot GUI.
       sh=slab1.shape
       xdim = len( sh ) -1
       if (parent.x_axis_type == 'sine') and (slab1.getAxis(xdim).id in gui_control.latitude_alias):
          a = MV2.sin(slab1.getAxis(xdim)[:]*MV2.pi/180)
          old_dim = slab1.getAxis( xdim )
          old_dim[:] = a[:].astype(old_dim.dtype.char)
       ydim = len( sh ) -2
       if (parent.y_axis_type == 'sine') and (slab1.getAxis(ydim).id in gui_control.latitude_alias):
          a = MV2.sin(slab1.getAxis(ydim)[:]*MV2.pi/180)
          old_dim = slab1.getAxis( ydim )
          old_dim[:] = a[:].astype(old_dim.dtype.char)

       if ((parent.panelGC.over_flg == 0) or (parent.Scatterct[ parent.vcs_id ] == 0)):
          g_name = 'ASD'
          parent.Scatterct[ parent.vcs_id ] = 1 + (parent.vcs_id*1000)
       else:
          g_name = 'ASD%d' % parent.Scatterct[ parent.vcs_id ]
          parent.Scatterct[ parent.vcs_id ] += 1

       if replot_flg != 3:
          try:
             parent.graphics_method = g_method = parent.vcs[ parent.vcs_id ].getscatter(g_name)
          except:
             parent.graphics_method = g_method = parent.vcs[ parent.vcs_id ].createscatter(g_name, 'ASD')
       else:
          try:
             g_method = parent.graphics_method
          except:
             try:
                parent.graphics_method = g_method = parent.vcs[ parent.vcs_id ].getscatter(g_name)
             except:
                parent.graphics_method = g_method = parent.vcs[ parent.vcs_id ].createscatter(g_name, 'ASD')
       #
       if parent.scat_marker is None:
          g_method.marker = 'dot'
       else:
          g_method.marker = parent.scat_marker
       if parent.scat_markercolor is None:
          g_method.markercolor = 241
       else:
          g_method.markercolor = parent.scat_markercolor
       if parent.scat_markersize is None:
          g_method.markersize = 3
       else:
          g_method.markersize = parent.scat_markersize
       #
       # Record Graphics Method
       gui_control.record_command( parent, "\n# Set Scatter Method Attributes" )
       gui_control.record_command( parent, "g_method = vcs_canvas_list[ %d ].getscatter('%s')" % (parent.vcs_id,g_name))
       gui_control.record_command( parent, "g_method.marker='%s'" % str(g_method.marker) )
       gui_control.record_command( parent, "g_method.markercolor=%s" % str(g_method.markercolor) )
       gui_control.record_command( parent, "g_method.markersize=%s" % str(g_method.markersize) )

#       g_method.line = parent.vec_line
#       if parent.vec_linecolor is not None:
#          g_method.linecolor = string.atoi(parent.vec_linecolor)
#       if parent.vec_scale is not None:
#          g_method.scale = string.atof(parent.vec_scale)
#       g_method.alignment = parent.vec_alignment
#       g_method.type = parent.vec_type
#       g_method.reference = string.atof(parent.vec_ref)
#
       #
       # set the map projection
       g_method.projection = parent.plot_projection
        
       template_name = dotemplate(parent,template,id,replot_flg, g_name)

   elif (g_type == 'XvsY'):       #      XvsY graphics method
       # Distribute the axis values to the poles using the sine function. The labeling is controled via the 
       # alter plot GUI.
       sh=slab1.shape
       xdim = len( sh ) -1
       if (parent.x_axis_type == 'sine') and (slab1.getAxis(xdim).id in gui_control.latitude_alias):
          a = MV2.sin(slab1.getAxis(xdim)[:]*MV2.pi/180)
          old_dim = slab1.getAxis( xdim )
          old_dim[:] = a[:].astype(old_dim.dtype.char)
       ydim = len( sh ) -2
       if (parent.y_axis_type == 'sine') and (slab1.getAxis(ydim).id in gui_control.latitude_alias):
          a = MV2.sin(slab1.getAxis(ydim)[:]*MV2.pi/180)
          old_dim = slab1.getAxis( ydim )
          old_dim[:] = a[:].astype(old_dim.dtype.char)

       if ((parent.panelGC.over_flg == 0) or (parent.XvsYct[ parent.vcs_id ] == 0)):
          g_name = 'ASD'
          parent.XvsYct[ parent.vcs_id ] = 1 + (parent.vcs_id*1000)
       else:
          g_name = 'ASD%d' % parent.XvsYct[ parent.vcs_id ]
          parent.XvsYct[ parent.vcs_id ] += 1

       if replot_flg != 3:
          try:
             parent.graphics_method = g_method = parent.vcs[ parent.vcs_id ].getxvsy(g_name)
          except:
             parent.graphics_method = g_method = parent.vcs[ parent.vcs_id ].createxvsy('ASD')
       else:
          try:
             g_method = parent.graphics_method
          except:
             try:
                parent.graphics_method = g_method = parent.vcs[ parent.vcs_id ].getxvsy(g_name)
             except:
                parent.graphics_method = g_method = parent.vcs[ parent.vcs_id ].createxvsy('ASD')
       #
       g_method.marker = 'star'
       g_method.markercolor = 241
       g_method.markersize = 3
       #
       # Record Graphics Method
       gui_control.record_command( parent, "\n# Set XvsY Method Attributes" )
       gui_control.record_command( parent, "g_method = vcs_canvas_list[ %d ].getxvsy('%s')" % (parent.vcs_id,g_name))
       gui_control.record_command( parent, "g_method.marker='%s'" % str(g_method.marker) )
       gui_control.record_command( parent, "g_method.markercolor=%s" % str(g_method.markercolor) )
       gui_control.record_command( parent, "g_method.markersize=%s" % str(g_method.markersize) )


#       g_method.line = parent.vec_line
#       if parent.vec_linecolor is not None:
#          g_method.linecolor = string.atoi(parent.vec_linecolor)
#       if parent.vec_scale is not None:
#          g_method.scale = string.atof(parent.vec_scale)
#       g_method.alignment = parent.vec_alignment
#       g_method.type = parent.vec_type
#       g_method.reference = string.atof(parent.vec_ref)
#
       #
       # set the map projection
       g_method.projection = parent.plot_projection
        
       template_name = set_template( parent, template_name )
       gui_alter_plot.settings( parent, replot_flg, g_name, template_name ) # Set the plot's visual appearance
      
   if (g_type != 'Ext') and (g_type != "Yxvsx") and (g_type != "Xyvsy") and (g_type!="Taylordiagram") and (g_name != 'default') and (page_layout_flg == 0):
      if (parent.app_x_min is None):
         g_method.datawc_x1 = 1.e20
      else:
         g_method.datawc_x1 = parent.app_x_min
      if (parent.app_x_max is None):
         g_method.datawc_x2 = 1.e20
      else:
         g_method.datawc_x2 = parent.app_x_max
      if (parent.app_y_min is None):
        g_method.datawc_y1 = 1.e20
      else:
         g_method.datawc_y1 = parent.app_y_min
      if (parent.app_y_max is None):
         g_method.datawc_y2 = 1.e20
      else:
         g_method.datawc_y2 = parent.app_y_max

   # If using VCS Canvas GUI, then record the current slab1, slab2, template, and graphics method
   if parent.vcs[ parent.vcs_id ].canvas_gui is not None:
        parent.vcs[ parent.vcs_id ].clear() # Make sure the canvas is cleared
        parent.vcs[ parent.vcs_id ].canvas_gui.slab1 = slab1
        parent.vcs[ parent.vcs_id ].canvas_gui.slab2 = slab2
        parent.vcs[ parent.vcs_id ].canvas_gui.template = template_name
        parent.vcs[ parent.vcs_id ].canvas_gui.g_type = g_type
        parent.vcs[ parent.vcs_id ].canvas_gui.g_name = g_name

   #
   # Plot the graphics method on the VCS Canvas
   #

   if (g_type == 'Yxvsx'):           #       Yxvsx graphics method
     if (template_name[0:3] != 'ASD'):
       del(plot_kw['continents'])
       parent.d[parent.dct]=apply(parent.vcs[ parent.vcs_id ].plot,(slab1, template_name, g_type, g_name),plot_kw)
       #comment1=amultiplier, comment2 = lmultiplier, file_comment=source, name=aname,
       #long_name=title, units=units, xname=xlabel, yname=ylabel, yrev=0, bg=bg, ratio=parent.plot_ratio )
       parent.vcs[ parent.vcs_id ].update()
       record_plot( parent, slab1.id, None,template_name , g_type, g_name,plot_kw)
       return return_value, parent.d[parent.dct]
     else:
       # Charles' code for nice labels
       sh=slab1.shape
       ilen=sh[-1]

       # Distribute the axis values to the poles using the sine function. The labeling is controled via the 
       # alter plot GUI.
       if parent.x_axis_type == 'sine':
          a = MV2.sin(slab1.getAxis(0)[:]*MV2.pi/180)
          old_dim = slab1.getAxis( 0 )
          old_dim[:] = a[:].astype(old_dim.dtype.char)

       try:
          minv, maxv = vcs.minmax(MV2.ravel(slab1)[:ilen])
       except Exception,err:
          gui_message.error('Unable to plot data! Please check data. Data must be either in integer or float format.')
          return "Error", None
       if parent.app_d_min is not None:
          dmin = parent.app_y_min = parent.app_d_min
       else:
          dmin = minv
       if parent.app_d_max is not None:
          dmax = parent.app_y_max = parent.app_d_max
       else:
          dmax = maxv
       dic={}
       try:
          if dmin==dmax:
             dmin*=(1-1E-6)
             dmax*=(1+1E-6)
          levs=vcs.mkscale(dmin, dmax)
          dic=vcs.mklabels(levs)
          if (dmin == levs[0] ):
             dmin=levs[0]-(levs[1]-levs[0])
          else:
             dmin=levs[0]
          if (dmax == levs[-1] ):
             dmax=levs[-1]+(levs[-1]-levs[-2])
          else:
             dmax=levs[-1]
          levs=vcs.mkscale(dmin, dmax)
          dic=vcs.mklabels(levs)
          dmin=levs[0]
          dmax=levs[-1]
       except:
          levs=[]
          levs.append( dmax )
       # End Charles' code for nice labels
       if ( (parent.panelGC.over_flg == 0) or (parent.XYct_restart == 1) ):
          parent.XYct = 0
          parent.XYct_restart = 0
          parent.yx_xmin = parent.app_x_min
          parent.yx_xmax = parent.app_x_max
          parent.yx_ymin = parent.app_y_min
          parent.yx_ymax = parent.app_y_max
          if (len(parent.d) != 0):
#            parent.vcs[ parent.vcs_id ].clear()
            parent.d = {}
            parent.dct = 0
       if (parent.XYct < 15):
          parent.XYct = parent.XYct + 1
       else:
#          parent.vcs[ parent.vcs_id ].clear()
          parent.XYct = 1       
       pg_name= 'ASD%d' % parent.XYct
       parent.pl.form[id].template.setentry( pg_name ) # Set the form line template entry
       if replot_flg != 3:
          parent.graphics_method = g_Yxvsx = parent.vcs[ parent.vcs_id ].getyxvsx(pg_name)
          g_Yxvsx_line = parent.vcs[ parent.vcs_id ].getline(pg_name)
          try:
             g_Yxvsx_line.type = string.atoi(parent.oneD_ltypes[parent.XYct-1])
          except:
             pass
          try:
             g_Yxvsx_line.width = string.atoi(string.split(parent.oneD_lwidths, ',')[parent.XYct-1])
          except:
             pass
#          try:
#             g_Yxvsx_line.color = string.atoi(parent.oneD_lcolors[parent.XYct-1])
#          except:
#             pass
          g_Yxvsx.line = g_Yxvsx_line

          g_Yxvsx_marker = parent.vcs[ parent.vcs_id ].getmarker(pg_name)
          found_marker = 0
          try:
             s_marker = string.split(parent.oneD_mtypes, ',')
             if type(s_marker[parent.XYct-1]) is types.StringType:  found_marker = 1
             g_Yxvsx_marker.type = s_marker[parent.XYct-1]
          except:
             pass
          try:
             g_Yxvsx_marker.size = string.atoi(string.split(parent.oneD_mwidths, ',')[parent.XYct-1])
          except:
             pass
#          try:
#             g_Yxvsx_marker.color = string.atoi(parent.oneD_mcolors[parent.XYct-1])
#          except:
#             pass
          if found_marker == 1: g_Yxvsx.marker = g_Yxvsx_marker
          template_name = pg_name
       else:
          g_Yxvsx = g_method = parent.graphics_method
       if parent.XYct == 1:
          gui_alter_plot.settings( parent, replot_flg, g_name, template_name ) # Set the plot's visual appearance
       if (len(dic) > 0) and (parent.XYct == 1):
          g_Yxvsx.yticlabels1=dic
          g_Yxvsx.yticlabels2=dic
          g_Yxvsx.ymtics1=dic
          g_Yxvsx.ymtics2=dic
       x_dim = slab1.getdimattribute((len(slab1.shape)-1), 'values')
       if (parent.yx_xmin is None) and (parent.XYct == 1):
          if g_Yxvsx.xaxisconvert not in ['log10','ln']:
             g_Yxvsx.datawc_x1 = parent.yx_xmin = x_dim[0]
          else:
             g_Yxvsx.datawc_x1 = 1.E20
       else:
          g_Yxvsx.datawc_x1 = parent.yx_xmin
       if (parent.yx_xmax is None) and (parent.XYct == 1):
          if g_Yxvsx.xaxisconvert not in ['log10','ln']:
             g_Yxvsx.datawc_x2 = parent.yx_xmax = x_dim[len(x_dim) - 1]
          else:
             g_Yxvsx.datawc_x2 = 1.E20
       else:
          g_Yxvsx.datawc_x2 = parent.yx_xmax
       if (parent.yx_ymin is None) and (parent.XYct == 1):
         g_Yxvsx.datawc_y1 = parent.yx_ymin = dmin
       else:
          g_Yxvsx.datawc_y1 = parent.yx_ymin
       if (parent.yx_ymax is None) and (parent.XYct == 1):
          g_Yxvsx.datawc_y2 = parent.yx_ymax = dmax
       else:
          g_Yxvsx.datawc_y2 = parent.yx_ymax
       t=parent.vcs[ parent.vcs_id ].gettemplate(pg_name)
       #
       # Record Graphics Method
       gui_control.record_command( parent, "\n# Set Yxvsx Method Attributes" )
       gui_control.record_command( parent, "g_method = vcs_canvas_list[ %d ].getyxvsx('%s')" % (parent.vcs_id, pg_name) )
       #
       turn_on_off_1Dplot_annotation( parent, pg_name )
       del(plot_kw['continents'])
       parent.d[parent.dct]=apply(parent.vcs[ parent.vcs_id ].plot,(slab1, pg_name, g_type, pg_name),plot_kw)
       #comment1=amultiplier, comment2 = lmultiplier, file_comment=source, name=aname,
       #long_name=title, units=units, xname=xlabel, yname=ylabel, yrev=0, bg=bg, ratio=parent.plot_ratio )
       parent.vcs[ parent.vcs_id ].update()
       record_plot( parent, slab1.id, None, pg_name, g_type, pg_name,plot_kw)
       #amultiplier, lmultiplier, source, aname, title, units, xlabel, ylabel, 0, bg, ratio=parent.plot_ratio  )
       return return_value, parent.d[parent.dct]
   elif (g_type == 'Xyvsy'):
     if (template_name[0:3] != 'ASD'):
       del(plot_kw['continents'])
       parent.d[parent.dct]=apply(parent.vcs[ parent.vcs_id ].plot,(slab1, template_name, g_type, g_name),plot_kw)
       #comment1=amultiplier, comment2 = lmultiplier, file_comment=source, name=aname,
       #long_name=title, units=units, xname=xlabel, yname=ylabel, yrev=0, bg=bg, ratio=parent.plot_ratio )
       parent.vcs[ parent.vcs_id ].update()
       record_plot( parent, slab1.id, None,template_name , g_type, g_name,plot_kw)
       return return_value, parent.d[parent.dct]
     else:
       # Charles' code for nice labels
       sh=slab1.shape
       ilen=sh[-1]

       # Distribute the axis values to the poles using the sine function. The labeling is controled via the 
       # alter plot GUI.
       if parent.y_axis_type == 'sine':
          a = MV2.sin(slab1.getAxis(0)[:]*MV2.pi/180)
          old_dim = slab1.getAxis( 0 )
          old_dim[:] = a[:].astype(old_dim.dtype.char)

       try:
          minv, maxv = vcs.minmax(MV2.ravel(slab1)[:ilen])
       except Exception,err:
          gui_message.error('Unable to plot data! Please check data. Data must be either in integer or float format.')
          return "Error", None
       if parent.app_d_min is not None:
          dmin = parent.app_x_min = parent.app_d_min
       else:
          dmin = minv
       if parent.app_d_max is not None:
          dmax = parent.app_x_max = parent.app_d_max
       else:
          dmax = maxv
       dic={}
       try:
          levs=vcs.mkscale(dmin, dmax)
          dic=vcs.mklabels(levs)
   #       lbls=vcs.mklabels(levs)
   #       for l in range(len(levs)):
   #            dic[levs[l]]=lbls[l]
          dmin = levs[0]
          dmax = levs[-1]
       except:
          levs=[]
          levs.append( dmax )
       # End Charles' code for nice labels
       if ( (parent.panelGC.over_flg == 0) or (parent.XYct_restart == 1) ):
          parent.XYct = 0
          parent.XYct_restart = 0
          parent.yx_xmin = parent.app_x_min
          parent.yx_xmax = parent.app_x_max
          parent.yx_ymin = parent.app_y_min
          parent.yx_ymax = parent.app_y_max
          if (len(parent.d) != 0):
#            parent.vcs[ parent.vcs_id ].clear()
            parent.d = {}
            parent.dct = 0
       elif (len(parent.d) != 0):
          parent.dct = parent.dct + 1
       if (parent.XYct < 15):
          parent.XYct = parent.XYct + 1
       else:
#          parent.vcs[ parent.vcs_id ].clear()
          parent.XYct = 1
       pg_name= 'ASD%d' % parent.XYct
       parent.pl.form[id].template.setentry( pg_name ) # Set the form line template entry
       if replot_flg !=3:
          parent.graphics_method = g_Xyvsy = parent.vcs[ parent.vcs_id ].getxyvsy(pg_name)
          g_Xyvsy_line = parent.vcs[ parent.vcs_id ].getline(pg_name)
          try:
             g_Xyvsy_line.type = string.atoi(parent.oneD_ltypes[parent.XYct-1])
          except:
             pass
          try:
             g_Xyvsy_line.width = string.atoi(string.split(parent.oneD_lwidths,',')[parent.XYct-1])
          except:
             pass
#       try:
#          g_Xyvsy_line.color = string.atoi(parent.oneD_lcolors[parent.XYct-1])
#       except:
#          pass
          g_Xyvsy.line = g_Xyvsy_line

          g_Xyvsy_marker = parent.vcs[ parent.vcs_id ].getmarker(pg_name)
          found_marker = 0
          try:
             s_marker = string.split(parent.oneD_mtypes, ',')
             if type(s_marker[parent.XYct-1]) is types.StringType:  found_marker = 1
             g_Xyvsy_marker.type = s_marker[parent.XYct-1]
          except:
             pass
          try:
             g_Xyvsy_marker.width = string.atoi(string.split(parent.oneD_mwidths, ',')[parent.XYct-1])
          except:
             pass
#       try:
#          g_Xyvsy_marker.color = string.atoi(parent.oneD_mcolors[parent.XYct-1])
#       except:
#          pass
          if found_marker == 1: g_Xyvsy.marker = g_Xyvsy_marker
          template_name = pg_name
       else:
          g_Xyvsy = g_method = parent.graphics_method

       if parent.XYct == 1:
          gui_alter_plot.settings( parent, replot_flg, g_name, template_name ) # Set the plot's visual appearance
       if (len(dic) > 0) and (parent.XYct == 1):
          g_Xyvsy.xticlabels1=dic
          g_Xyvsy.xticlabels2=dic
          g_Xyvsy.xmtics1=dic
          g_Xyvsy.xmtics2=dic
       y_dim = slab1.getdimattribute((len(slab1.shape)-1), 'values')
       if (parent.yx_xmin is None) and (parent.XYct == 1):
          g_Xyvsy.datawc_x1 = parent.yx_xmin = dmin
       else:
          g_Xyvsy.datawc_x1 = parent.yx_xmin
       if (parent.yx_xmax is None) and (parent.XYct == 1):
          g_Xyvsy.datawc_x2 = parent.yx_xmax = dmax
       else:
          g_Xyvsy.datawc_x2 = parent.yx_xmax
       if (parent.yx_ymin is None) and (parent.XYct == 1):
          if g_Xyvsy.yaxisconvert not in ['log10','ln']:
             g_Xyvsy.datawc_y1 = parent.yx_ymin = y_dim[0]
          else:
             g_Xyvsy.datawc_y1 = 1.E20
       else:
          g_Xyvsy.datawc_y1 = parent.yx_ymin
       if (parent.yx_ymax is None) and (parent.XYct == 1):
          if g_Xyvsy.yaxisconvert not in ['log10','ln']:
             g_Xyvsy.datawc_y2 = parent.yx_ymax = y_dim[len(y_dim) - 1]
          else:
             g_Xyvsy.datawc_y2 = 1.E20
       else:
          g_Xyvsy.datawc_y2 = parent.yx_ymax
       #
       # Record Graphics Method
       gui_control.record_command( parent, "\n# Set Xyvsy Method Attributes" )
       gui_control.record_command( parent, "g_method = vcs_canvas_list[ %d ].getxyvsy('%s')" % (parent.vcs_id, pg_name) )
       #
       turn_on_off_1Dplot_annotation( parent, pg_name )
       #print 'i am here 6'
       del(plot_kw['continents'])
       parent.d[parent.dct]=apply(parent.vcs[ parent.vcs_id ].plot,(slab1, pg_name, g_type, pg_name),plot_kw)
       #comment1=amultiplier, comment2 = lmultiplier, file_comment=source, name=aname,
       #long_name=title, units=units, xname=ylabel, yname=xlabel, yrev=0, bg=bg, ratio=parent.plot_ratio )
       parent.vcs[ parent.vcs_id ].update()
       record_plot( parent, slab1.id, None, pg_name, g_type, pg_name, plot_kw)
       #amultiplier, lmultiplier, source, aname, title, units, xlabel, ylabel, 0, bg, ratio=parent.plot_ratio )
       return return_value, parent.d[parent.dct]
##    elif (g_type == 'Taylordiagram'):
##       print 'In there ?'
##       parent.d[parent.dct]=parent.vcs[ parent.vcs_id ].taylor(slab1,template=template_name)
   elif (g_type=='Ext'):
      parent.pl.form[id].gm.setentry( '' ) # Set the form line graphics entry
      parent.panelDV.gm_listbox.clear()
      parent.panelDV.gm_name_obj.set('')
      parent.panelGC.menuBar.component(parent.panelGC.O_Name + '-menu').entryconfigure(13, state = 'disabled')
      class Crap:
         pass
      fake = Crap()
      fake.g_name = None
      parent.graphics_method=fake
      template_name=dotemplate(parent,template,id,replot_flg,"")
      vars=[slab1,]
      if slab2 is not None:
         vars.append(slab2)
         for j in range(len(parent.pl.form[id].datas)):
            d = parent.pl.form[id].datas[j].get()
            if d=="":
               break
            else:
               vars.append(__main__.__dict__[ d ])

      #insert parent so we can check what's on/off, etc...
      vars.insert(0,parent)
      # insert the canvas
      vars.insert(1,parent.vcs[ parent.vcs_id ])
      # insert the template name
      vars.insert(2,parent.template_name)
      gui_control.record_command( parent, "\n#External Plotter: %s" % parent.external_plot_functions[id].__name__, 1 )
      if (len(parent.d) != 0):
         if (parent.vcs_canvas_plot_number == 1):
            parent.d = {}
            parent.dct = 0
         else:
            parent.dct = parent.dct + 1
      parent.d[parent.dct] = apply(parent.external_plot_functions[id],vars)
      return return_value, parent.d[parent.dct]        
   elif ( parent.panelGC.over_flg ):
      if (len(parent.d) == 0):
         if (template_name[:3] == 'ASD'): 
            turn_on_off_plot_annotation( parent, template_name )
         if (g_type not in ['Vector', 'Scatter', 'XvsY', 'Meshfill']):  # Vector, Scatter, XvsY graphics method
##             print 'i am here 5
            parent.d[parent.dct]=apply(parent.vcs[ parent.vcs_id ].plot,(slab1, template_name, g_type, g_name),plot_kw)
            #comment1=amultiplier, comment4=legendlabel, file_comment=source, name=aname, long_name=title, units=units, xname=xlabel,
            #yname=ylabel, continents=parent.panelGC.cont_flg, yrev=0, bg=bg, ratio=parent.plot_ratio )
            record_plot( parent, slab1.id, None, template_name, g_type, g_name, plot_kw)
            #amultiplier, lmultiplier, source, aname, title, units, xlabel, ylabel, parent.panelGC.cont_flg, bg , ratio=parent.plot_ratio)
         else:
##             print 'i am here 4'
            parent.d[parent.dct]=apply(parent.vcs[ parent.vcs_id ].plot,(slab2, slab1, template_name, g_type, g_name),plot_kw)
            #comment1=amultiplier, comment4=legendlabel, file_comment=source, name=aname, long_name=title, units=units,
            #xname=xlabel, yname=ylabel, continents=parent.panelGC.cont_flg, yrev=0, bg=bg, ratio=parent.plot_ratio )
            record_plot( parent, slab2.id, slab1.id, template_name, g_type, g_name, plot_kw)
            #amultiplier, lmultiplier, source, aname, title, units, xlabel, ylabel, parent.panelGC.cont_flg, bg, ratio=parent.plot_ratio )
      else:
         parent.dct = parent.dct + 1
         if (g_type not in ['Vector','Scatter','XvsY',"Meshfill"]): # Vector,Scatter,XvsY graphics method
##             print 'i am here 3', parent.template_name, template_name
            parent.d[parent.dct]=apply(parent.vcs[ parent.vcs_id ].plot, (slab1, template_name, g_type, g_name), plot_kw)
            #comment1=amultiplier, comment4=legendlabel, file_comment=source, name=aname, long_name=title, units=units,
            #xname=xlabel, yname=ylabel, continents=parent.panelGC.cont_flg, yrev=0, bg=bg, ratio=parent.plot_ratio )
            record_plot( parent, slab1.id, None, template_name, g_type, g_name, plot_kw)
            #amultiplier, lmultiplier, source, aname, title, units, xlabel, ylabel, parent.panelGC.cont_flg, bg, ratio=parent.plot_ratio )
         else:
##             print 'i am here 2'
            parent.d[parent.dct]=apply(parent.vcs[ parent.vcs_id ].plot,(slab2, slab1, parent.template_name, g_type, g_name), plot_kw)
            #comment1=amultiplier, comment4=legendlabel, file_comment=source, name=aname, long_name=title, units=units,
            #xname=xlabel, yname=ylabel, continents=parent.panelGC.cont_flg, yrev=0, bg=bg, ratio=parent.plot_ratio )
            record_plot( parent, slab2.id,  slab1.id, parent.template_name, g_type, g_name, plot_kw)
            #amultiplier, lmultiplier, source, aname, title, units, xlabel, ylabel, parent.panelGC.cont_flg, bg , ratio=parent.plot_ratio)
      
   else:
      if (len(parent.d) != 0) and (parent.vcs_canvas_plot_number == 1):
#       I cannot clear at this point any longer, move the clear to evt_plot. Only clear when
#       doing a QuickPlot
#         parent.vcs[ parent.vcs_id ].clear()      I cannot clear at this point any longer
         parent.d = {}
         parent.dct = 0
      if (template_name[:3] == 'ASD'): turn_on_off_plot_annotation( parent, template_name )
      if (g_type not in [ "Vector", "Scatter", "XvsY", 'Meshfill']) or (g_type == "Meshfill" and slab2 is None):
##       if (g_type not in [ "Vector", "Scatter", "XvsY", 'Meshfill']) or (g_type == "Meshfill" and len(parent.panelDV.selected_list)<2):
##       if (g_type not in ['Vector','Scatter','XvsY',"Meshfill"]): # Vector, Scatter, XvsY graphics method
##          print 'here ?',template_name, g_type, g_name,plot_kw
##          parent.vcs[ parent.vcs_id ].gettemplate(template_name).list()
         parent.d[parent.dct]=apply(parent.vcs[ parent.vcs_id ].plot,(slab1, template_name, g_type, g_name), plot_kw)
         #comment1=amultiplier, comment4=legendlabel, file_comment=source, name=aname, long_name=title,
         #units=units, xname=xlabel, yname=ylabel, continents=parent.panelGC.cont_flg, yrev=0, bg=bg, ratio=parent.plot_ratio )
         record_plot( parent, slab1.id, None, template_name, g_type, g_name, plot_kw)
         #amultiplier, lmultiplier, source, aname, title, units, xlabel, ylabel, parent.panelGC.cont_flg, bg, ratio=parent.plot_ratio )
      else:
         parent.d[parent.dct]=apply(parent.vcs[ parent.vcs_id ].plot, (slab2, slab1, template_name, g_type,g_name), plot_kw)
         #comment1=amultiplier, comment4=legendlabel, file_comment=source, name=aname, long_name=title, units=units,
         #xname=xlabel, yname=ylabel, continents=parent.panelGC.cont_flg, yrev=0, bg=bg, ratio=parent.plot_ratio )
         record_plot( parent, slab2.id, slab1.id, template_name, g_type, g_name, plot_kw)
         #amultiplier, lmultiplier, source, aname, title, units, xlabel, ylabel, parent.panelGC.cont_flg, bg, ratio=parent.plot_ratio )
   #########################################################################################
   #                                                                                       #
   # Get the saved VCS Canvas settings (i.e., width, height, x-position, and y-position).  #
   # If the VCS Canvas settings are not set, then look to the previous VCS Canvas settings.#
   # That is, if VCS Canvas 4 canvas settings are None, then use VCS Canvas 3 settings. If #
   # VCS Canvas 3 settings are None too, then look to use VCS Canvas 2 settings. If VCS    #
   # Canvas 2 are settings are None as well, then use VCS Canvas 1 settings. If VCS Canvas #
   # 1 settings are None then use the VCS Canvas default settings.                         #
   #                                                                                       #
   #########################################################################################
   if first_canvas == 1: # Only do this if no canvas was previously drawn.
      try:
         vcs_id = parent.vcs_id
         g = parent.vcg[ vcs_id ]
         while (vcs_id > 0) and (g[ 0 ] is None):
               vcs_id -= 1
               g = parent.vcg[ vcs_id ]
         c = parent.vcs[ parent.vcs_id ].canvasinfo()
         if ( (c['width'] != g[0]) or (c['height'] != g[1]) ):
            y = g[3] + ((parent.vcs_id+1)*100)
            parent.vcs[ parent.vcs_id ].geometry( g[0], g[1], g[2], y )
            parent.vcs[ parent.vcs_id ].updateorientation()

         # If the VCS Canvas is displayed, then update the backing store and unblock the 
         # X server.
         if (parent.vcs[ parent.vcs_id ].iscanvasdisplayed()):
            parent.vcs[ parent.vcs_id ].flush()
            parent.vcs[ parent.vcs_id ].backing_store()
      except: pass

   # Note to Dean: this should be fixed in the "vcs.plot" routine.
   if from_selected == 1:
      slab1.setdimattribute( (len(slab1.shape)-1), 'name', xlabel_hold)
      try:
         slab1.setdimattribute( (len(slab1.shape)-2), 'name', ylabel_hold)
      except:
         pass

   # Reset boxfill type to "linear, log10, or custom, if Boxfill Discrete
   if Box_discrete_flg == 1: parent.boxfill_type = Box_discrete_type

   return return_value, parent.d[parent.dct]

#---------------------------------------------------------------------------
# Re-plot the data via VCS
#---------------------------------------------------------------------------
def re_plot( parent, replot_flg ):
   parent.vcs[ parent.vcs_id ].updateVCSsegments()
   parent.vcs[ parent.vcs_id ].update()

#   bg = 0
#   if parent.panelGC.text_opt3.get() == 'Background Canvas':
#      bg = 1
#   if (len(parent.d) != 0):
#      d = parent.d
#      dct = parent.dct
#      g_type = {}
#      hold_over_flg = -99
#      for i in range(len(d)):
#         g_type[i] = d[i].g_type
#      if (g_type[0] != 'Yxvsx') and (g_type[0] != 'Xyvsy'):
#         hold_over_flg = parent.panelGC.over_flg
#         parent.panelGC.over_flg=1
#      parent.vcs[ parent.vcs_id ].clear()
#      parent.d = {}
#      parent.dct = 0
#      parent.XYct = 0
#      parent.yx_xmin = None
#      parent.yx_xmax = None
#      parent.yx_ymin = None
#      parent.yx_ymax = None
#      for i in range(len(d)):
#         plot( parent, g_type[i], replot_flg, bg)
#         parent.plot_ct[ parent.vcs_id ] -= 1
#      if (hold_over_flg != -99):
#         parent.panelGC.over_flg = hold_over_flg
#   else:
#      plot( parent, parent.panelGC.text_opt2.get(), replot_flg, bg)

#---------------------------------------------------------------------------
# Record VCS plot command
#---------------------------------------------------------------------------
def record_plot( parent, slab1=None, slab2=None, templ=None, gtype=None, 
                 gname=None, plot_kw={} ):
   ratio=plot_kw['ratio']
   if ratio[-1]=='t':
      ratio=repr(ratio)
   else:
      ratio=repr(ratio[:-1])
   if (slab1 is not None):
##       if (parent.panelDM.var3 is not None): slab1 = 'slab'
##       else: slab1 = "%s" % slab1
##       else: slab1 = "slabm['%s']" % slab1
      slab1 = "%s" % slab1
   if (slab2 is not None):
      slab2 = "%s" % slab2
#      slab2 = "slabm['%s']" % slab2
##    am = amult
##    lm = lmult
##    if amult is None:
##        am=''
##        lm=''
   # Record the vcs clear command
   if parent.panelGC.over_flg == 0:
      gui_control.record_command( parent, "\n# Clear the VCS Canvas", 1 )
      gui_control.record_command( parent,"vcs_canvas_list[ %d ].clear( )" % parent.vcs_id,1 )

   if (slab1 is not None) and (slab2 is None):
      s_b  = "vcs_display = vcs_canvas_list[ %d ].plot( %s, '%s', '%s', '%s'" % (parent.vcs_id, slab1, templ, gtype, gname)
      for k in plot_kw.keys():
         s_b+=', '+k+' = '+repr(plot_kw[k])
      s_b+=')'
      s=s_b
      gui_control.record_command( parent, "\n# Plot Slab", 1 )
   elif (slab1 is not None) and (slab2 is not None):
      s_b  = "vcs_display = vcs_canvas_list[ %d ].plot( %s, %s, '%s', '%s', '%s'" % (parent.vcs_id, slab1, slab2, parent.template_name, gtype, gname)
      for k in plot_kw.keys():
         s_b+=', '+k+' = '+repr(plot_kw[k])
      s_b+=')'
      s=s_b
      gui_control.record_command( parent, "\n# Plot Slab1 and Slab2", 1 )
##       s_b = "vcs_display = vcs_canvas_list[ %d ].plot( %s, %s, '%s', '%s', '%s')" % (parent.vcs_id, slab1, slab2, templ, gtype, gname)
   else:
      s_b = "vcs_display = vcs_canvas_list[ %d ].plot( parent.template_name_dud, 'continents', 'ASD'"
      for k in plot_kw.keys():
         s_b+=', '+k+' = '+repr(plot_kw[k])
      s_b+=')'
      s=s_b
      gui_control.record_command( parent, "\n# Plot Continents" )
      gui_control.record_command( parent, s )
      return
   #
   gui_control.record_command( parent, s )
   gui_control.record_command( parent, s_b, 2 )

#---------------------------------------------------------------------------
# Turn on/off annotation
#---------------------------------------------------------------------------
def turn_on_off_plot_annotation( parent, template_name ):
   if template_name[-3:] == 'dud': return
   try:
      t = parent.vcs[ parent.vcs_id ].gettemplate( template_name )
      #
      # Record Template
      gui_control.record_command( parent, "\n# View Annotated Plot or Not" )
      gui_control.record_command( parent, "templ = vcs_canvas_list[ %d ].gettemplate('ASD')" % parent.vcs_id )
   except:
      gui_message.error( ('Cannnot find template [ %s ].' % template_name) )
   if (parent.annotate_status == 'on') and (parent.vcs_canvas_plot_number == 1):
      t.source.priority=1;   gui_control.record_command( parent, "templ.source.priority=1")
      t.dataname.priority=1; gui_control.record_command( parent, "templ.dataname.priority=1")
      t.title.priority=1;    gui_control.record_command( parent, "templ.title.priority=1")
      t.units.priority=1;    gui_control.record_command( parent, "templ.units.priority=1")
      t.crdate.priority=parent.annotate_date;   gui_control.record_command( parent, "templ.crdate.priority=%i" % parent.annotate_date)
      t.crtime.priority=parent.annotate_time;   gui_control.record_command( parent, "templ.crtime.priority=%i" % parent.annotate_time)

      # Leave these turned off for now and see if the users complain.
      t.xvalue.priority=0;   gui_control.record_command( parent, "templ.xvalue.priority=1")
      t.yvalue.priority=0;   gui_control.record_command( parent, "templ.yvalue.priority=1")
      t.zvalue.priority=0;   gui_control.record_command( parent, "templ.zvalue.priority=1")
      t.tvalue.priority=0;   gui_control.record_command( parent, "templ.tvalue.priority=1")
      t.mean.priority=parent.annotate_mean;     gui_control.record_command( parent, "templ.mean.priority=%i" % parent.annotate_mean)
      t.min.priority=parent.annotate_min;      gui_control.record_command( parent, "templ.min.priority=%i" % parent.annotate_min)
      t.max.priority=parent.annotate_max;      gui_control.record_command( parent, "templ.max.priority=%i" % parent.annotate_max)
      t.file.priority=1;     gui_control.record_command( parent, "templ.file.priority=1")
      t.comment1.priority=parent.annotate_multiplier; gui_control.record_command( parent, "templ.comment1.priority=%i" % parent.annotate_multiplier)
      t.comment2.priority=parent.annotate_multiplier; gui_control.record_command( parent, "templ.comment2.priority=%i" % parent.annotate_multiplier)
      t.comment3.priority=1; gui_control.record_command( parent, "templ.comment3.priority=1")
      t.comment4.priority=1; gui_control.record_command( parent, "templ.comment4.priority=1")
   else:
      t.source.priority=0;   gui_control.record_command( parent, "templ.source.priority=0")
      t.dataname.priority=0; gui_control.record_command( parent, "templ.dataname.priority=0")
      t.title.priority=0;    gui_control.record_command( parent, "templ.title.priority=0")
      t.units.priority=0;    gui_control.record_command( parent, "templ.units.priority=0")
      t.crdate.priority=0;   gui_control.record_command( parent, "templ.crdate.priority=0")
      t.crtime.priority=0;   gui_control.record_command( parent, "templ.crtime.priority=0")
      t.xvalue.priority=0;   gui_control.record_command( parent, "templ.xvalue.priority=0")
      t.yvalue.priority=0;   gui_control.record_command( parent, "templ.yvalue.priority=0")
      t.zvalue.priority=0;   gui_control.record_command( parent, "templ.zvalue.priority=0")
      t.tvalue.priority=0;   gui_control.record_command( parent, "templ.tvalue.priority=0")
      t.mean.priority=0;     gui_control.record_command( parent, "templ.mean.priority=0")
      t.min.priority=0;      gui_control.record_command( parent, "templ.min.priority=0")
      t.max.priority=0;      gui_control.record_command( parent, "templ.max.priority=0")
      t.file.priority=0;     gui_control.record_command( parent, "templ.file.priority=0")
      t.comment1.priority=0; gui_control.record_command( parent, "templ.comment1.priority=0")
      t.comment2.priority=0; gui_control.record_command( parent, "templ.comment2.priority=0")
      t.comment3.priority=0; gui_control.record_command( parent, "templ.comment3.priority=0")
      t.comment4.priority=0; gui_control.record_command( parent, "templ.comment4.priority=0")

#---------------------------------------------------------------------------
# Turn on/off annotation for 1D plots
#---------------------------------------------------------------------------
def turn_on_off_1Dplot_annotation( parent, template_name ):
   try:
      t = parent.vcs[ parent.vcs_id ].gettemplate( template_name )
      #
      # Record Template
      gui_control.record_command( parent, "\n# View Annotated Plot or Not for 1D Plots" )
      gui_control.record_command( parent, "templ = vcs_canvas_list[ %d ].gettemplate('ASD')" % parent.vcs_id )
   except:
      gui_message.error( ('Cannnot find template [ %s ].' % template_name) )
   if (parent.annotate_status == 'on') and (parent.vcs_canvas_plot_number == 1):
      # Leave these turned off for now and see if the users complain.
      t.xvalue.priority=0;   gui_control.record_command( parent, "templ.xvalue.priority=1")
      t.yvalue.priority=0;   gui_control.record_command( parent, "templ.yvalue.priority=1")
      t.zvalue.priority=0;   gui_control.record_command( parent, "templ.zvalue.priority=1")
      t.tvalue.priority=0;   gui_control.record_command( parent, "templ.tvalue.priority=1")
      t.comment1.priority=parent.annotate_multiplier; gui_control.record_command( parent, "templ.comment1.priority=%i" % parent.annotate_multiplier)
      t.comment2.priority=parent.annotate_multiplier; gui_control.record_command( parent, "templ.comment2.priority=%i" % parent.annotate_multiplier)
   else:
      t.comment1.priority=0; gui_control.record_command( parent, "templ.comment1.priority=0")
      t.comment2.priority=0; gui_control.record_command( parent, "templ.comment2.priority=0")

#---------------------------------------------------------------------------
# Modify the template to fit on the VCS Canvas
#---------------------------------------------------------------------------
def set_template( parent, template_name = None ):
   number  = parent.vcs_canvas_plot_number
   
   # Increment the counter and clear page if needed
   parent.plot_ct[ parent.vcs_id ] += 1
   if (number > 1) and (parent.plot_ct[ parent.vcs_id ] > number):
      parent.vcs[ parent.vcs_id ].clear()
      parent.plot_ct[ parent.vcs_id ] = 1

   if number == 1:
       if parent.plot_projection not in ['linear', 'default']:
          parent.template_name = 'ASD_map'
       elif template_name == None:
          parent.template_name = 'ASD'
          parent.template_name_dud = 'ASD_dud'
       else:
##           print 'Setting tnmae to:',template_name
          parent.template_name = template_name
       return parent.template_name
   elif number == 2:
       if parent.plot_projection not in ['linear', 'default']:
          if parent.plot_ct[ parent.vcs_id ] == 1:
             parent.template_name = 'ASD_map1of2'
             parent.template_name_dud = 'ASD_map1of2_dud'
          else:
             parent.template_name = 'ASD_map2of2'
             parent.template_name_dud = 'ASD_map2of2_dud'
       else:
          if parent.plot_ct[ parent.vcs_id ] == 1:
             parent.template_name = 'ASD1_of_2'
             parent.template_name_dud = 'ASD1_of_2_dud'
          else:
             parent.template_name = 'ASD2_of_2'
             parent.template_name_dud = 'ASD2_of_2_dud'
       return parent.template_name
   elif number == 3:
       if parent.plot_projection not in ['linear', 'default']:
          if parent.plot_ct[ parent.vcs_id ] == 1:
             parent.template_name = 'ASD_map1of3'
             parent.template_name_dud = 'ASD_map1of3_dud'
          elif parent.plot_ct[ parent.vcs_id ] == 2:
             parent.template_name = 'ASD_map2of3'
             parent.template_name_dud = 'ASD_map2of3_dud'
          else:
             parent.template_name = 'ASD_map3of3'
             parent.template_name_dud = 'ASD_map3of3_dud'
       else:
          if parent.plot_ct[ parent.vcs_id ] == 1:
             parent.template_name = 'ASD1_of_3'
             parent.template_name_dud = 'ASD1_of_3_dud'
          elif parent.plot_ct[ parent.vcs_id ] == 2:
             parent.template_name = 'ASD2_of_3'
             parent.template_name_dud = 'ASD2_of_3_dud'
          else:
             parent.template_name = 'ASD3_of_3'
             parent.template_name_dud = 'ASD3_of_3_dud'
       return parent.template_name
   elif number == 4:
       if parent.plot_projection not in ['linear', 'default']:
          if parent.plot_ct[ parent.vcs_id ] == 1:
             parent.template_name = 'ASD_map1of4'
             parent.template_name_dud = 'ASD_map1of4_dud'
          elif parent.plot_ct[ parent.vcs_id ] == 2:
             parent.template_name = 'ASD_map2of4'
             parent.template_name_dud = 'ASD_map2of4_dud'
          elif parent.plot_ct[ parent.vcs_id ] == 3:
             parent.template_name = 'ASD_map3of4'
             parent.template_name_dud = 'ASD_map3of4_dud'
          else:
             parent.template_name = 'ASD_map4of4'
             parent.template_name_dud = 'ASD_map4of4_dud'
       else:
          if parent.plot_ct[ parent.vcs_id ] == 1:
             parent.template_name = 'ASD1_of_4'
             parent.template_name_dud = 'ASD1_of_4_dud'
          elif parent.plot_ct[ parent.vcs_id ] == 2:
             parent.template_name = 'ASD2_of_4'
             parent.template_name_dud = 'ASD2_of_4_dud'
          elif parent.plot_ct[ parent.vcs_id ] == 3:
             parent.template_name = 'ASD3_of_4'
             parent.template_name_dud = 'ASD3_of_4_dud'
          else:
             parent.template_name = 'ASD4_of_4'
             parent.template_name_dud = 'ASD4_of_4_dud'
       return parent.template_name
   else:
     return template_name


#---------------------------------------------------------------------------
# Modify the template to fit on the VCS Canvas
#---------------------------------------------------------------------------
def modify_template( parent, t, t2, n, n2, x, y, w, h, orientation='horizontal', legend_top=1 ):
      # Record the create template
      gui_control.record_command( parent, "t = vcs_canvas_list[ 0 ].createtemplate('%s', '%s')" % (t.name, n))
      gui_control.record_command( parent, "t2 = vcs_canvas_list[ 0 ].createtemplate('%s', '%s')" % (t2.name, n2))

      #
      # Get the plot settings
      #
      x_pos = float( x ) / 100.0
      y_pos = float( y ) / 100.0
      width = float( w ) / 100.0
      height = float( h ) / 100.0
      t.data.x1 = t2.data.x1 = t.box1.x1 = (0.075-width*0.5) + x_pos
      t.data.x2 = t2.data.x2 = t.box1.x2 = (0.927+width*0.5) + x_pos
      t.data.y1 = t2.data.y1 = t.box1.y1 = (0.282-height*0.5) + y_pos
      t.data.y2 = t2.data.y2 = t.box1.y2 = (0.85+height*0.5) + y_pos
      gui_control.record_command( parent, "t.data.x1=t2.data.x1=%.17g" % t.data.x1 )
      gui_control.record_command( parent, "t.data.x2=t2.data.x2=%.17g" % t.data.x2 )
      gui_control.record_command( parent, "t.data.y1=t2.data.y1=%.17g" % t.data.y1 )
      gui_control.record_command( parent, "t.data.y2=t2.data.y2=%.17g" % t.data.y2 )

      t.xlabel1.y = t.data.y1 - 0.02
      t.ylabel1.x = t.data.x1 - 0.011
      t.xlabel2.y = t.data.y2 + 0.02
      t.ylabel2.x = t.data.x2 + 0.011
      gui_control.record_command( parent, "t.xlabel1.y=%.17g" % t.xlabel1.y )
      gui_control.record_command( parent, "t.ylabel1.x=%.17g" % t.ylabel1.x )
      gui_control.record_command( parent, "t.xlabel2.y=%.17g" % t.xlabel2.y )
      gui_control.record_command( parent, "t.ylabel2.x=%.17g" % t.ylabel2.x )
  
      t.xtic1.y1 = t.data.y1
      t.xtic1.y2 = t.data.y1 - 0.012
      t.ytic1.x1 = t.data.x1
      t.ytic1.x2 = t.data.x1 - 0.01
      t.xtic2.y1 = t.data.y2
      t.xtic2.y2 = t.data.y2 + 0.012
      t.ytic2.x1 = t.data.x2
      t.ytic2.x2 = t.data.x2 + 0.01
      gui_control.record_command( parent, "t.xtic1.y1=%.17g" % t.xtic1.y1 )
      gui_control.record_command( parent, "t.xtic1.y2=%.17g" % t.xtic1.y2 )
      gui_control.record_command( parent, "t.ytic1.x1=%.17g" % t.ytic1.x1 )
      gui_control.record_command( parent, "t.ytic1.x2=%.17g" % t.ytic1.x2 )
      gui_control.record_command( parent, "t.xtic2.y1=%.17g" % t.xtic2.y1 )
      gui_control.record_command( parent, "t.xtic2.y2=%.17g" % t.xtic2.y2 )
      gui_control.record_command( parent, "t.ytic2.x1=%.17g" % t.ytic2.x1 )
      gui_control.record_command( parent, "t.ytic2.x2=%.17g" % t.ytic2.x2 )

      t.xname.x = t.data.x1 + ((t.data.x2 - t.data.x1)*0.51)
      t.xname.y = t.data.y1 - 0.05
      t.yname.x = t.data.x1 - 0.065
      t.yname.y = t.data.y1 + ((t.data.y2 - t.data.y1)*0.5)
      gui_control.record_command( parent, "t.xname.x=%.17g" % t.xname.x )
      gui_control.record_command( parent, "t.xname.y=%.17g" % t.xname.y )
      gui_control.record_command( parent, "t.yname.x=%.17g" % t.yname.x )
      gui_control.record_command( parent, "t.yname.y=%.17g" % t.yname.y )

      #
      # Set the legend settings
      #
      t.legend.x1=(0.075-width*0.5) + x_pos
      t.legend.x2=(0.927+width*0.5) + x_pos
      if (legend_top == 1):
         t.legend.y1=0.551
         t.legend.y2=0.571
      else:
         t.legend.y1=0.04
         t.legend.y2=0.06
      gui_control.record_command( parent, "t.legend.x1=%.17g" % t.legend.x1 )
      gui_control.record_command( parent, "t.legend.x2=%.17g" % t.legend.x2 )
      gui_control.record_command( parent, "t.legend.y1=%.17g" % t.legend.y1 )
      gui_control.record_command( parent, "t.legend.y2=%.17g" % t.legend.y2 )
  
#
#---------------------------------------------------------------------------
# End VCS function calls
#---------------------------------------------------------------------------
#
#---------------------------------------------------------------------
# End of File
#---------------------------------------------------------------------





