#!/usr/bin/env python
# Adapted for numpy/ma/cdms2 by convertcdms.py
#
# The PCMDI Data Browser Reset GUI to Initial State -  gui_reset module
#
###############################################################################
#                                                                             #
# Module:       gui_reset module                                              #
#                                                                             #
# Copyright:    "See file Legal.htm for copyright information."               #
#                                                                             #
# Authors:      PCMDI Software Team                                           #
#               Lawrence Livermore NationalLaboratory:                        #
#               support@pcmdi.llnl.gov                                        #
#                                                                             #
# Description:  PCMDI Software System browser reset GUI to its initial state. #
#                                                                             #
# Version:      3.0                                                           #
#                                                                             #
###############################################################################
#
import Tkinter
import gui_alter_plot, gui_menu
import cdms2 as cdms

def to_initial_state( parent, call_from=1 ):

   # Get the previously saved state of the GUI
   try:
      import sys, os
      fn = '%s/PCMDI_GRAPHICS' % os.environ['HOME']
      sys.path.append(fn)
      import vcdat_initial
   except:
      pass

   #
   # Reset Continents Flag
   #
   try:
      parent.panelGC.cont_flg = vcdat_initial.set.cont_flg
   except:
      parent.panelGC.cont_flg = None
##    if parent.panelGC.cont_flg == -1: parent.panelGC.cont_toggle_0.set( 1 )
##    else: parent.panelGC.cont_toggle_0.set( 0 )
   if parent.panelGC.cont_flg == None: parent.panelGC.cont_toggle_1.set( 1 )
   else: parent.panelGC.cont_toggle_1.set( 0 )
   if parent.panelGC.cont_flg == 0: parent.panelGC.cont_toggle_2.set( 1 )
   else: parent.panelGC.cont_toggle_2.set( 0 )
   if parent.panelGC.cont_flg == 2: parent.panelGC.cont_toggle_3.set( 1 )
   else: parent.panelGC.cont_toggle_3.set( 0 )
   if parent.panelGC.cont_flg == 1: parent.panelGC.cont_toggle_4.set( 1 )
   else: parent.panelGC.cont_toggle_4.set( 0 )
   if parent.panelGC.cont_flg == 3: parent.panelGC.cont_toggle_5.set( 1 )
   else: parent.panelGC.cont_toggle_5.set( 0 )
   if parent.panelGC.cont_flg == 4: parent.panelGC.cont_toggle_6.set( 1 )
   else: parent.panelGC.cont_toggle_6.set( 0 )
   if parent.panelGC.cont_flg == 5: parent.panelGC.cont_toggle_7.set( 1 )
   else: parent.panelGC.cont_toggle_7.set( 0 )
   if parent.panelGC.cont_flg == 6: parent.panelGC.cont_toggle_8.set( 1 )
   else: parent.panelGC.cont_toggle_8.set( 0 )
   if parent.panelGC.cont_flg == 7: parent.panelGC.cont_toggle_9.set( 1 )
   else: parent.panelGC.cont_toggle_9.set( 0 )
   if parent.panelGC.cont_flg == 8: parent.panelGC.cont_toggle_10.set( 1 )
   else: parent.panelGC.cont_toggle_10.set( 0 )
   if parent.panelGC.cont_flg == 9: parent.panelGC.cont_toggle_11.set( 1 )
   else: parent.panelGC.cont_toggle_11.set( 0 )
   if parent.panelGC.cont_flg == 10: parent.panelGC.cont_toggle_12.set( 1 )
   else: parent.panelGC.cont_toggle_12.set( 0 )
   if parent.panelGC.cont_flg == 11: parent.panelGC.cont_toggle_13.set( 1 )
   else: parent.panelGC.cont_toggle_13.set( 0 )


#   parent.panelGC.var_continents.set(parent.panelGC.cont_flg)

   #
   # Reset Overlay Flag
   #
   try:
      parent.panelGC.over_flg = vcdat_initial.set.over_flg
   except:
      parent.panelGC.over_flg = 0
   parent.panelGC.var_overlay.set(parent.panelGC.over_flg)

   #
   # Reset Subset Selection Flag
   #
   try:
      parent.panelGC.isol_label_flg = vcdat_initial.set.isol_label_flg
   except:
      parent.panelGC.isol_label_flg = 1
   parent.panelGC.var_labels.set(parent.panelGC.isol_label_flg)

   #
   # Reset the "Fortran Order" Flag
   #
   try:
      parent.menu.fortran_order_flg = vcdat_initial.set.fortran_order_flg
   except:
      parent.menu.fortran_order_flg = 0
   parent.menu.main_menu.var_fortran.set(parent.menu.fortran_order_flg)

   #
   # Reset Retain Dimension Subset Flag
   #
   try:
      parent.menu.save_region = vcdat_initial.set.save_region
   except:
      parent.menu.save_region = 0
   #parent.menu.main_menu.var_retain_sub.set(parent.menu.save_region)

   #
   # Squeeze Dimension Flag
   #
   try:
      parent.menu.squeeze_dim_flg = vcdat_initial.set.squeeze_dim_flg
   except:
      parent.menu.squeeze_dim_flg = 1
   parent.menu.main_menu.var_squeeze_dim.set(parent.menu.squeeze_dim_flg)

   #
   # View Axes Flag
   #
   try:
      parent.menu.view_axes_flg = vcdat_initial.set.view_axes_flg
   except:
      parent.menu.view_axes_flg = 1
   parent.menu.main_menu.view_axes.set(parent.menu.view_axes_flg)


   #
   # View Bounds and Weights Flag
   #
   try:
      parent.menu.view_bounds_weights_flg = vcdat_initial.set.view_bounds_weights_flg
   except:
      parent.menu.view_bounds_weights_flg = 1
   parent.menu.main_menu.view_bounds_weights.set(parent.menu.view_bounds_weights_flg)

   #
   # Set Meridian State FLag
   #
   try:
      parent.menu.meridian_flg = vcdat_initial.set.meridian_flg
   except:
      parent.menu.meridian_flg = 0
   parent.menu.create_options_menu.evt_set_meridian_toggle( parent, parent.menu.meridian_flg )

   #
   # Set Automatic Conversion to MV FLag
   #
   try:
      parent.menu.convert_to_MV_flg = vcdat_initial.set.convert_to_MV_flg
   except:
      parent.menu.convert_to_MV_flg = 0
   parent.menu.main_menu.var_convert_to_MV.set(parent.menu.convert_to_MV_flg)

   # Initialize the "Show VCDAT Popup Exit" Flag
   try:
      parent.menu.show_exit_popup_flg = vcdat_initial.set.show_exit_popup_flg
   except:
      parent.menu.show_exit_popup_flg = 1
   parent.menu.main_menu.exit_popup_window.set(parent.menu.show_exit_popup_flg)

   # Initialize the checkbutton to set the Defined Variables single selection mode:
   try:
      parent.menu.main_menu.DV_single_selection_mode.set( vcdat_initial.set.DV_single_selection_mode )
   except:
      parent.menu.main_menu.DV_single_selection_mode.set( 0 )

   # Initialize the "VCS Canvas GUI Settings" Flag
   try:
      parent.menu.vcs_canvas_gui_settings_flg = vcdat_initial.set.vcs_canvas_gui_settings_flg
   except:
      parent.menu.vcs_canvas_gui_settings_flg = 0
   parent.menu.main_menu.var_popup_window.set(parent.menu.vcs_canvas_gui_settings_flg)

   # Initialize the "Popup Window Settings" Flag
   try:
      parent.menu.popup_window_settings_flg = vcdat_initial.set.popup_window_settings_flg
   except:
      parent.menu.popup_window_settings_flg = 1
   parent.menu.main_menu.var_popup_window.set(parent.menu.popup_window_settings_flg)

   # reset default font
   try:
      parent.default_font=vcdat_initial.set.default_font
   except:
      parent.default_font = 'AvantGarde'
   parent.vcs[0].setdefaultfont(parent.default_font)

   #
   # Reset Retain User Defined Settings Flag
   # THIS IS NO LONGER IN USE!!!
   #if call_from == 1:
   #   try:
   #      parent.menu.retain_user_settings_flg = vcdat_initial.set.retain_user_settings_flg
   #   except:
   #      parent.menu.retain_user_settings_flg = 1
   #   parent.menu.main_menu.var_retain_user.set(parent.menu.retain_user_settings_flg)

   #
   # Reset Annotation Parameters
   #
   try:
      parent.annotate_status=string.lower(vcdat_initial.set.annotate_status)
   except:
      parent.annotate_status='on'
   if parent.annotate_status not in ['on','off']:
      parent.annotate_status='on'
   #
   parent.annotate_source = None
   parent.annotate_name = None
   parent.annotate_title = None
   parent.annotate_xlabel = None
   parent.annotate_ylabel = None

   #
   # Reset Min, Max and Scale Parameters
   #
   parent.app_x_min = None
   parent.app_x_max = None
   parent.app_y_min = None
   parent.app_y_max = None
   parent.app_d_min = None
   parent.app_d_max = None
   parent.multiplier = None

   #
   # Reset Boxfill Attributes
   #
   parent.boxfill_level1=None
   parent.boxfill_level2=None
   parent.boxfill_color1=None
   parent.boxfill_color2=None
   parent.boxfill_ext1=None
   parent.boxfill_ext2=None
   #
   # Reset Contour Levels
   #
   parent.iso_ranges = None
   parent.iso_colors = None
   parent.iso_min = None
   parent.iso_max = None
   parent.iso_num = None

   #
   # Reset 1D Attributes
   #
   parent.oneD_ltypes = None
   parent.oneD_lcolors = None
   parent.oneD_lwidths = None
   parent.oneD_mtypes = None
   parent.oneD_mcolors = None
   parent.oneD_mwidths = None

   #
   # Reset Vector Attributes
   #
   parent.vec_line = 'solid'
   parent.vec_linecolor = '241'
   parent.vec_scale = None
   parent.vec_alignment = 'center'
   parent.vec_type = 'arrows'
   parent.vec_ref = 1e+20

   #
   # Reset Scatter Attributes
   #
   parent.scat_marker='dot'
   parent.scat_markercolor='241'
   parent.scat_markersize='7'


   #
   # Reset Continents Attributes
   #
   parent.cont_line = 'solid'
   parent.cont_linecolor = '241'
   parent.cont_linewidth = '2'
   parent.cont_type = '0'

   #
   # Reset Outfill Attributes
   #
   parent.outf_style = 'solid'
   parent.outf_index = '1'
   parent.outf_fillcolor = '241'
   parent.outf_outfill = [1]

   #
   # Reset Outline Attributes
   #
   parent.outl_line = 'solid'
   parent.outl_linecolor = '241'
   parent.outl_outline = [1]

   #
   # Reset Alter Plot GUI
   #
   try:
      parent.graphics_method.xticlabels1 = ''
      parent.graphics_method.xmtics1 = ''
      parent.graphics_method.yticlabels1 = ''
      parent.graphics_method.ymtics1 = ''
   except:
      pass
   gui_alter_plot.initialize( parent )

   try:
      #
      # Reset Alter Plot X-axis
      #
      parent.alter_notebook.cbn1.value = 0
      parent.alter_notebook.cbn2.value = 0
      parent.alter_notebook.cbn1.deselect()
      parent.alter_notebook.cbn2.deselect()
      if (parent.x_labels == 'top') or (parent.x_labels == 'both'):
          parent.alter_notebook.cbn1.invoke()
      if (parent.x_labels == 'bottom') or (parent.x_labels == 'both'):
          parent.alter_notebook.cbn2.invoke()
   
      parent.alter_notebook.cbn3.value = 0
      parent.alter_notebook.cbn4.value = 0
      parent.alter_notebook.cbn3.deselect()
      parent.alter_notebook.cbn4.deselect()
      if (parent.x_major_ticks == 'top') or (parent.x_major_ticks == 'both'):
          parent.alter_notebook.cbn3.invoke()
      if (parent.x_major_ticks=='bottom') or (parent.x_major_ticks=='both'):
          parent.alter_notebook.cbn4.invoke()
   
      parent.alter_notebook.cbn5.value = 0
      parent.alter_notebook.cbn6.value = 0
      parent.alter_notebook.cbn5.deselect()
      parent.alter_notebook.cbn6.deselect()
      if (parent.x_minor_ticks == 'top') or (parent.x_minor_ticks == 'both'):
          parent.alter_notebook.cbn5.invoke()
      if (parent.x_minor_ticks=='bottom') or (parent.x_minor_ticks=='both'):
          parent.alter_notebook.cbn6.invoke()
   
      if parent.x_axis_type == 'linear':
         parent.alter_notebook.cbn7.invoke()
      elif parent.x_axis_type == 'log10':
         parent.alter_notebook.cbn8.invoke()
   #   elif parent.x_axis_type == 'ln':
   #      parent.alter_notebook.cbn9.invoke()
   #   elif parent.x_axis_type == 'exp':
   #      parent.alter_notebook.cbn10.invoke()
   
      parent.alter_notebook.eny1.clear( )
   
      parent.alter_notebook.eny2.clear( )
   
      #
      # Reset Alter Plot Y-axis
      #
      parent.alter_notebook.cbn11.value = 0
      parent.alter_notebook.cbn12.value = 0
      parent.alter_notebook.cbn11.deselect()
      parent.alter_notebook.cbn12.deselect()
      if (parent.y_labels == 'left') or (parent.y_labels == 'both'):
          parent.alter_notebook.cbn11.invoke()
      if (parent.y_labels == 'right') or (parent.y_labels == 'both'):
          parent.alter_notebook.cbn12.invoke()
      
      parent.alter_notebook.cbn13.value = 0
      parent.alter_notebook.cbn14.value = 0
      parent.alter_notebook.cbn13.deselect()
      parent.alter_notebook.cbn14.deselect() 
      if (parent.y_major_ticks == 'left') or (parent.y_major_ticks == 'both'):
          parent.alter_notebook.cbn13.invoke()
      if (parent.y_major_ticks=='right') or (parent.y_major_ticks=='both'):        
          parent.alter_notebook.cbn14.invoke()
      
      parent.alter_notebook.cbn15.value = 0
      parent.alter_notebook.cbn16.value = 0
      parent.alter_notebook.cbn15.deselect()
      parent.alter_notebook.cbn16.deselect() 
      if (parent.y_minor_ticks == 'left') or (parent.y_minor_ticks == 'both'):
          parent.alter_notebook.cbn15.invoke()
      if (parent.y_minor_ticks=='right') or (parent.y_minor_ticks=='both'):        
          parent.alter_notebook.cbn16.invoke()
      
      if parent.y_axis_type == 'linear':
         parent.alter_notebook.cbn17.invoke()
      elif parent.y_axis_type == 'log10':
         parent.alter_notebook.cbn18.invoke()
   #   elif parent.y_axis_type == 'ln':
   #      parent.alter_notebook.cbn19.invoke()
   #   elif parent.y_axis_type == 'exp':
   #      parent.alter_notebook.cbn20.invoke()
      
      parent.alter_notebook.eny3.clear( )
  
      parent.alter_notebook.eny4.clear( )
   
      #
      # Reset Alter Plot Legend
      #
      parent.alter_notebook.cbnstat_on.value = 0
      parent.alter_notebook.cbnstat_off.value = 0
      parent.alter_notebook.cbnstat_on.deselect()
      parent.alter_notebook.cbnstat_off.deselect()
      if (parent.legend_status == 'on'):
         parent.alter_notebook.cbnstat_on.invoke()
      else:
         parent.alter_notebook.cbnstat_off.invoke()

      parent.alter_notebook.cbn21.value = 0
      parent.alter_notebook.cbn22.value = 0
      parent.alter_notebook.cbn21.deselect()
      parent.alter_notebook.cbn22.deselect()
      if (parent.legend_orientation == 'horizontal'):
         parent.alter_notebook.cbn21.invoke()
      else:
         parent.alter_notebook.cbn22.invoke()

      parent.alter_notebook.ctr1.setentry('0')
      parent.alter_notebook.ctr2.setentry('0')
      parent.alter_notebook.ctr3.setentry('0')
      parent.alter_notebook.ctr4.setentry('0')
   
      #
      # Reset Alter Plot Shapes
      #
      parent.alter_notebook.ctr5.setentry('0')
      parent.alter_notebook.ctr6.setentry('0')
      parent.alter_notebook.ctr7.setentry('0')
      parent.alter_notebook.ctr8.setentry('0')
   
   except:
      pass

   #
   # Reset dimensions aliases
   #
   cdms_list = {0:cdms.axis.longitude_aliases, 1:cdms.axis.latitude_aliases,
               2:cdms.axis.time_aliases, 3:cdms.axis.level_aliases}
   try:
      dim_list = {0:vcdat_initial.set.longitude_aliases, 1:vcdat_initial.set.latitude_aliases,
               2:vcdat_initial.set.time_aliases, 3:vcdat_initial.set.level_aliases}
      for i in range( 4 ):
         for j in range( len(dim_list[i]) ):
            if dim_list[i][j] not in cdms_list[i]:
                cdms_list[i].append( dim_list[i][j] )
   except:
      pass

   #
   # Reset the calculation mode
   #
   try:
      parent.calculate_mode = vcdat_initial.set.calculate_mode
   except:
      parent.calculate_mode = 1
   if parent.calculate_mode == 1:
      parent.panelDV.func_mode_icon.create_image(0,0, anchor=Tkinter.NW,
                                 image=parent.panelDV.func_mode1_image )
   else:
      parent.panelDV.func_mode_icon.create_image(0,0, anchor=Tkinter.NW,
                                 image=parent.panelDV.func_mode2_image )
   
   #
   # Reset the defined variables tools mode
   #
   try:
      parent.show_defined_variables_tools = vcdat_initial.set.show_defined_variables_tools
   except:
      parent.show_defined_variables_tools = 0
   parent.panelDV.evt_show_defined_variables_tools( parent, None )

   #
   # Reset the template and graphics methods mode
   #
   try:
      parent.show_template_graphics_method = vcdat_initial.set.show_template_graphics_method
   except:
      parent.show_template_graphics_method = 0
   parent.panelDV.evt_show_template_graphics_method( parent, None )

   #
   # Reset the colormap maximum intensity setting
   #
   for i in range(len(parent.menu.main_menu.color_intensity_toggle)):
      parent.menu.main_menu.color_intensity_toggle[i].set( 0 )
   try:
      parent.menu.main_menu.color_intensity = vcdat_initial.set.colormap_intensity_setting
   except:
      parent.menu.main_menu.color_intensity = 0
   parent.menu.main_menu.color_intensity_toggle[parent.menu.main_menu.color_intensity].set( 1 )


#---------------------------------------------------------------------
# End of File
#---------------------------------------------------------------------
