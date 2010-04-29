#!/usr/bin/env python
# Adapted for numpy/ma/cdms2 by convertcdms.py
#
# The PCMDI Data Browser Saved Settings -  gui_saved_settings module
#
###############################################################################
#                                                                             #
# Module:       gui_saved_setting module                                      #
#                                                                             #
# Copyright:    "See file Legal.htm for copyright information."               #
#                                                                             #
# Authors:      PCMDI Software Team                                           #
#               Lawrence Livermore NationalLaboratory:                        #
#               support@pcmdi.llnl.gov                                        #
#                                                                             #
# Description:  PCMDI Software System browser saved settings.                 #
#                                                                             #
# Version:      4.0                                                           #
#                                                                             #
###############################################################################
#
import os, string
import gui_message
import gui_control
import gui_menu
import cdms2
import gui_support

#---------------------------------------------------------------------
# Write to file
#---------------------------------------------------------------------
def create( parent ):
   try:
      fn = '%s/PCMDI_GRAPHICS' % os.environ['HOME']
   except:
      gui_message.error( 'Could not find the PCMDI_GRAPHICS directory.' )
      return

   file_name = fn + '/vcdat_initial.py'
   fp = open(file_name, 'w')

   if (fp.tell() == 0): # Must be a new file, so include below
      fp.write("#####################################################\n")
      fp.write("#                                                   #\n")
      fp.write("# Save the initial state of Visual CDAT             #\n")
      fp.write("#                                                   #\n")
      fp.write("#####################################################\n")
      fp.write("#\n")
      fp.write("class set:\n")
   fp.write("#                                                    \n")
   fp.write("#####################################################\n")
   fp.write("# GUI Geometry Settings                             #\n")
   fp.write("#####################################################\n")
   fp.write("#                                                    \n")
   geometry = parent.geometry()
   geometry = string.split(geometry, 'x')
   fp.write("   gui_width = %s                                  # GUI Width\n" % geometry[0])
   geometry = string.split(geometry[1], '+')
   fp.write("   gui_height = %s                                 # GUI Height\n" % geometry[0])
   fp.write("   gui_x_position = %s                             # GUI X Position\n" % geometry[1])
   fp.write("   gui_y_position = %s                             # GUI Y Position\n" % geometry[2])
   fp.write("#                                                    \n")
   fp.write("#####################################################\n")
   fp.write("# VCS Canvas Geometry Settings                      #\n")
   fp.write("#####################################################\n")
   fp.write("#                                                    \n")
   fp.write("# 0 corresponds to VCS Canvas 1:                     \n")
   fp.write("# 1 corresponds to VCS Canvas 2:                     \n")
   fp.write("# 2 corresponds to VCS Canvas 3:                     \n")
   fp.write("# 3 corresponds to VCS Canvas 4:                     \n")
   fp.write("#                                                    \n")
   fp.write("# [Width, Height, X-Position, Y-Position]            \n")
   fp.write("#                                                    \n")
   fp.write("   vcg = %s                                        # VCS Canvas Geometry for all four canvases\n" % parent.vcg)
   fp.write("#                                                    \n")
   fp.write("#####################################################\n")
   fp.write("# VCS Canvas Default Font Setting                    #\n")
   fp.write("#####################################################\n")
   fp.write("   default_font = '%s'                             # VCS Canvas default font for all four canvases\n" % parent.default_font)
   fp.write("#                                                    \n")
   fp.write("########################################################\n")
   fp.write("# VCS Canvas Orientation (i.e., Landscape or Portrait  #\n")
   fp.write("########################################################\n")
   fp.write("#                                                    \n")
   fp.write("   page_orientation_flg = %s                         # Set page orientation\n" % parent.panelGC.page_orientation_flg)
   fp.write("#                                                    \n")
   fp.write("#####################################################\n")
   fp.write("# Swap Size: Retrieved data cannot be larger that   #\n")
   fp.write("#            this values. The default size is       #\n")
   fp.write("#            64,000,000 bytes. If your machine can  #\n")
   fp.write("#            handle more then set the value below   #\n")
   fp.write("#            to the appropriate size.               #\n")
   fp.write("#####################################################\n")
   fp.write("#                                                    \n") 
   fp.write("   swap_size = %s                             # Set platform swap size\n" % parent.swap_size)
   fp.write("#                                                    \n")
   fp.write("#####################################################\n")
   fp.write("# VCDAT Full or Lite Setting                        #\n")
   fp.write("#####################################################\n")
   fp.write("#                                                    \n")
   fp.write("   vcdat_lite = %d                         # Select the full or lite gui setting\n" % parent.vcdat_lite )
   fp.write("#                                                    \n")
   ## Charles' changes no need to save the zero size of paned stuff if vcdat_lite mode is on
   if parent.vcdat_lite==0:
      fp.write("#####################################################\n")
      fp.write("# Pane Settings                                     #\n")
      fp.write("#####################################################\n")
      fp.write("#                                                    \n")
      fp.write("   pane1_min_position = %d                         # Select variable min pane position\n" % parent.pane._min['panelSV'] )
      fp.write("   pane1_max_position = %d                         # Select variable max pane position\n" % parent.pane._max['panelSV'] )
      fp.write("   pane2_min_position = %d                         # Graphics row min pane position\n" % parent.pane._min['panelGC'] )
      fp.write("   pane2_max_position = %d                         # Graphics row max pane position\n" % parent.pane._max['panelGC'] )
      fp.write("   pane2_size_position = %d                        # Graphics row size pane position\n" % parent.pane._size['panelGC'] )
      fp.write("   pane3_min_position = %d                         # Dimension min pane position\n" % parent.pane._min['panelDM'] )
      fp.write("   pane3_size_position = %d                        # Dimension size pane position\n" % parent.pane._size['panelDM'] )
      fp.write("   pane4_min_position = %d                         # Defined Variables min pane position\n" % parent.pane._min['panelDV'] )
      fp.write("   pane4_size_position = %d                        # Defined Variables max pane position\n" % parent.pane._size['panelDV'] )
      fp.write("   pane5_min_position = %d                         # Variable Information min pane position\n" % parent.pane._min['panelVI'] )
   fp.write("#                                                    \n")
   fp.write("#####################################################\n")
   fp.write("# Options Pulldown Settings                         #\n")
   fp.write("#####################################################\n")
   fp.write("#                                                    \n")
   if parent.panelGC.cont_flg is None:
      fp.write("   cont_flg = %s                                  # Set Continents flag (-1, None, 0, 1, 2, ..., 11)\n" % parent.panelGC.cont_flg)
   else:
      fp.write("   cont_flg = %d                                     # Set Continents flag (-1, None, 0, 1, 2, ..., 11)\n" % parent.panelGC.cont_flg)
   fp.write("   over_flg = %d                                     # Set Overlay flag (0 or 1)\n" % parent.panelGC.over_flg)
   fp.write("   isol_label_flg = %d                               # Set the Isoline label flag (0 or 1)\n" % parent.panelGC.isol_label_flg)
#   fp.write("   retain_user_settings_flg = %d                     # Set the Retain User Defined Settings flag (0 or 1)\n" % parent.menu.retain_user_settings_flg)
   fp.write("   vcs_canvas_gui_settings_flg = %d                  # Set the Popup Window Settings flag (0 or 1)\n" % parent.menu.vcs_canvas_gui_settings_flg)
   fp.write("   popup_window_settings_flg = %d                    # Set the Popup Window Settings flag (0 or 1)\n" % parent.menu.popup_window_settings_flg)
   fp.write("   annotate_status = '%s'                           # Set Annotation Status (on or off)\n" % parent.annotate_status)
   fp.write("#                                                    \n")
   fp.write("#####################################################\n")
   fp.write("# Help Pulldown Settings                            #\n")
   fp.write("#####################################################\n")
   fp.write("#                                                    \n")
   if gui_support.balloon_state() == 'none':
       dballoon = 0
   else:
       dballoon = 1
   fp.write("   enable_balloons_flg = %d                          # Set the Help Balloon flag (0 or 1)\n" % dballoon)
   fp.write("#                                                    \n")
   fp.write("#####################################################\n")
   fp.write("# Alter Plot Popup Settings                         #\n")
   fp.write("#####################################################\n")
   fp.write("#                                                    \n")
   fp.write("# X-Axis Settings                                    \n")
   if parent.x_labels:
      fp.write("   x_labels = '%s'                              # X labels\n" % parent.x_labels)
   else:
      fp.write("   x_labels = %s                                # X labels\n" % parent.x_labels)
   if parent.x_major_ticks:
      fp.write("   x_major_ticks = '%s'                         # X major ticks\n" % parent.x_major_ticks)
   else:
      fp.write("   x_major_ticks = %s                           # X major ticks\n" % parent.x_major_ticks)
   if parent.x_minor_ticks:
      fp.write("   x_minor_ticks = '%s'                           # X minor ticks \n" % parent.x_minor_ticks)
   else:
      fp.write("   x_minor_ticks = %s                             # X minor ticks \n" % parent.x_minor_ticks)
   fp.write("   x_axis_type = '%s'                           # X axis type\n" % parent.x_axis_type)
   fp.write("   x_major_values = %s                              # X major values\n" % parent.x_major_values)
   fp.write("   x_minor_values = %s                              # X minor values\n" % parent.x_minor_values)
   fp.write("#                                                    \n")
   fp.write("# Y-Axis Settings                                    \n")
   if parent.y_labels:
      fp.write("   y_labels = '%s'                                # Y lables\n" % parent.y_labels)
   else:
      fp.write("   y_labels = %s                                  # Y lables\n" % parent.y_labels)
   if parent.y_major_ticks:
      fp.write("   y_major_ticks = '%s'                           # Y major ticks\n" % parent.y_major_ticks)
   else:
      fp.write("   y_major_ticks = %s                             # Y major ticks\n" % parent.y_major_ticks)
   if parent.y_minor_ticks:
     fp.write("   y_minor_ticks = '%s'                           # Y minor ticks\n" % parent.y_minor_ticks)
   else:
     fp.write("   y_minor_ticks = %s                             # Y minor ticks\n" % parent.y_minor_ticks)
   fp.write("   y_axis_type = '%s'                           # Y axis ticks\n" % parent.y_axis_type)
   fp.write("   y_major_values = %s                              # Y major values\n" % parent.y_major_values)
   fp.write("   y_minor_values = %s                              # Y minor values\n" % parent.y_minor_values)
   fp.write("#                                                    \n")
   fp.write("# Legend Settings                                    \n")
   fp.write("   legend_status = '%s'                             # Legend status\n" % parent.legend_status)
   fp.write("   legend_orientation = '%s'                # Legend orientation\n" % parent.legend_orientation)
   fp.write("   legend_x_position = %s                            # Legend x position\n" % parent.legend_x_position)
   fp.write("   legend_y_position = %s                            # Legend y position\n" % parent.legend_y_position)
   fp.write("   legend_width = %s                                 # Legend width\n" % parent.legend_width)
   fp.write("   legend_height = %s                                # Legend height\n" % parent.legend_height)
   fp.write("#                                                    \n")
   fp.write("# Plot Shape Settings                                \n")
   fp.write("   plot_x_position = %s                              # Plot x position\n" % parent.plot_x_position)
   fp.write("   plot_y_position = %s                              # Plot y position\n" % parent.plot_y_position)
   fp.write("   plot_ratio = %s                                   # Plot ratio\n" % repr(parent.plot_ratio))
   fp.write("   plot_width = %s                                   # Plot width\n" % parent.plot_width)
   fp.write("   plot_height = %s                                  # Plot height\n" % parent.plot_height)
   fp.write("#                                                    \n")
   fp.write("#####################################################\n")
   fp.write("# Dimension Pulldown Settings                       #\n")
   fp.write("#####################################################\n")
   fp.write("#                                                    \n")
   fp.write("   save_region = %s                                 # Retain dimension flag\n" % parent.menu.save_region)
   fp.write("   squeeze_dim_flg = %s                              # Squeeze dimension flag\n" % parent.menu.squeeze_dim_flg)
   fp.write("   view_axes_flg = %s                                # View axes in variable list flag\n" % parent.menu.view_axes_flg)
   fp.write("   view_bounds_weights_flg = %s                      # View bounds and weights in variable list flag\n" % parent.menu.view_bounds_weights_flg)
   fp.write("   meridian_flg = %s                                 # View meridian flag (0-None, 1-Greenwich, 2-Date Line)\n" % parent.menu.meridian_flg)
   fp.write("   convert_to_MV_flg = %s                            # Automatic numpy/numpy.ma conversion to MV\n" % parent.menu.convert_to_MV_flg)
#   fp.write("   fortran_order_flg = %s                                # Fortran order flag\n" % parent.menu.fortran_order_flg)
   #l_menu = gui_menu.gui_extend_menus.l_menu
   #if l_menu != []:
   #   fp.write("#                                                    \n")
   #   fp.write("#####################################################\n")
   #   fp.write("# Extended Menus Settings                           #\n")
   #   fp.write("#####################################################\n")
   #   fp.write("#                                                    \n")
   #   fp.write("   menu_dict = [                                      \n")
   #   for i in range(len(l_menu)):
   #      fp.write("      {'Menu_name' : '%s'},\n" % l_menu[i]['m_nam'])
   #      fp.write("      {'Menu_balloon' : '%s'},\n" % l_menu[i]['m_info'])
   #      for j in range(len(l_menu[i]['m_items'])):
   #         fp.write("         {'item_Name' : '%s'},\n" % l_menu[i]['m_items'][j]['i_nam'])
   #         fp.write("         {'item_Directory' : '%s'},\n" % l_menu[i]['m_items'][j]['i_dir'])
   #         fp.write("         {'item_Function_File' : '%s'},\n" % l_menu[i]['m_items'][j]['i_fun'])
   #         fp.write("         {'item_Function_Name' : '%s'},\n" % l_menu[i]['m_items'][j]['i_file'])
   #      fp.write("#                                                    \n")
   #   fp.write("   ] # end the menu dictionary                       \n")
   fp.write("#                                                    \n")
   fp.write("#####################################################\n")
   fp.write("# Dimension Aliases                                 #\n")
   fp.write("#####################################################\n")
   fp.write("#                                                    \n")
   fp.write("   longitude_aliases = [")
   long_list = cdms2.axis.longitude_aliases
   for x in long_list: fp.write("'%s', " % x)
   fp.write("\n                        ]           # end Longitude aliases               \n")
   fp.write("   latitude_aliases = [")
   lat_list = cdms2.axis.latitude_aliases
   for x in lat_list: fp.write("'%s', " % x)
   fp.write("\n                        ]           # end Latitude aliases               \n")
   fp.write("   time_aliases = [")
   time_list = cdms2.axis.time_aliases
   for x in time_list: fp.write("'%s', " % x)
   fp.write("\n                        ]           # end Time aliases               \n")
   fp.write("   level_aliases = [")
   lev_list = cdms2.axis.level_aliases
   for x in lev_list: fp.write("'%s', " % x)
   fp.write("\n                        ]           # end Level aliases               \n")
   fp.write("#                                                    \n")
   fp.write("#####################################################\n")
   fp.write("# Calculation Mode (either 1 or 2)                  #\n")
   fp.write("#####################################################\n")
   fp.write("#                                                    \n")
   fp.write("   calculate_mode = %d                               # Set calculation mode\n" % parent.calculate_mode)
   fp.write("#                                                    \n")
   fp.write("#####################################################\n")
   fp.write("# Save Defined Variables Tools State                #\n")
   fp.write("#####################################################\n")
   fp.write("#                                                    \n")
   if parent.show_defined_variables_tools == 0:
      fp.write("   show_defined_variables_tools = %d                 # Set defined variables tools mode\n" % 1)
   else:
      fp.write("   show_defined_variables_tools = %d                 # Set defined variables tools mode\n" % 0)
   fp.write("#                                                    \n")
   fp.write("#####################################################\n")
   fp.write("# Save Template & Graphics Method  State            #\n")
   fp.write("#####################################################\n")
   fp.write("#                                                    \n")
   if parent.show_template_graphics_method == 0:
      fp.write("   show_template_graphics_method = %d                # Set template & graphics method mode\n" % 1)
   else:
      fp.write("   show_template_graphics_method = %d                # Set template & graphics method mode\n" % 0)
   fp.write("#                                                    \n")
   fp.write("#####################################################\n")
   fp.write("# Save Template Default Name                        #\n")
   fp.write("#####################################################\n")
   fp.write("#                                                    \n")
   if (parent.plot_projection not in ['linear']): parent.template_name = 'ASD'
   if ((parent.vcs_canvas_plot_number > 1) and (parent.template_name[:3] in ['ASD'])):
        parent.template_name = 'ASD'
   fp.write("   template_name = '%s'                              # Set template default name\n" % parent.template_name)
   fp.write("#                                                    \n")
   fp.write("#####################################################\n")
   fp.write("# Save Graphics Method Default Name                 #\n")
   fp.write("#####################################################\n")
   fp.write("#                                                    \n")
   fp.write("   graphics_method_name = '%s'                       # Set graphics method default name\n" % parent.graphics_method_name)
   fp.write("#                                                    \n")
   fp.write("#####################################################\n")
   fp.write("# Save Colormap Default Name                        #\n")
   fp.write("#####################################################\n")
   fp.write("#                                                    \n")
   fp.write("   colormap_name = '%s'                              # Set colormap default name\n" % parent.colormap_name)
   fp.write("#                                                    \n")
   fp.write("#####################################################\n")
   fp.write("# Save Defined Variables Selection Mode             #\n")
   fp.write("#####################################################\n")
   fp.write("#                                                    \n")
   fp.write("   DV_single_selection_mode = %d                     # Set Defined Variables selection mode\n" % parent.menu.main_menu.DV_single_selection_mode.get())
   fp.write("#                                                    \n")
   fp.write("#####################################################\n")
   fp.write("# Save VCDAT Exit Popup Window Mode                 #\n")
   fp.write("#####################################################\n")
   fp.write("#                                                    \n")
   fp.write("   show_exit_popup_flg = %d                          # Set show VCDAT exit popup window mode\n" % parent.menu.show_exit_popup_flg)
   fp.write("#####################################################\n")
   fp.write("# Save Colormap Maximum Intensity Setting           #\n")
   fp.write("#####################################################\n")
   fp.write("#                                                    \n")
   fp.write("   colormap_intensity_setting = %d                   # Set Colormap Intensity setting\n" % parent.menu.main_menu.color_intensity)
   fp.write("#                                                    \n")
   fp.write("#####################################################\n")
   fp.write("# End of File                                       #\n")
   fp.write("#####################################################\n")
   fp.write("#                                                    \n")
   save_menus()

#-----------------------------------------------------
# Save extended menus
#-----------------------------------------------------
def save_menus():
    file_name = os.environ['HOME']+'/PCMDI_GRAPHICS/user_menus.py'
    fp = open(file_name, 'w')
    fp.write("class set:\n")
    l_menu = gui_menu.gui_extend_menus.l_menu
    if l_menu != []: 
        fp.write("#                                                    \n")
        fp.write("#####################################################\n")
        fp.write("# Extended Menus Settings                           #\n")
        fp.write("#####################################################\n")
        fp.write("#                                                    \n")
        fp.write("   menu_dict = [                                      \n")
        for i in range(len(l_menu)):
            fp.write("      {'Menu_name' : '%s'},\n" % l_menu[i]['m_nam'])
            fp.write("      {'Menu_balloon' : '%s'},\n" % l_menu[i]['m_info'])
            for j in range(len(l_menu[i]['m_items'])):
                fp.write("         {'item_Name' : '%s'},\n" % l_menu[i]['m_items'][j]['i_nam'])
                fp.write("         {'item_Directory' : '%s'},\n" % l_menu[i]['m_items'][j]['i_dir'])
                fp.write("         {'item_Function_File' : '%s'},\n" % l_menu[i]['m_items'][j]['i_file'])
                fp.write("         {'item_Function_Name' : '%s'},\n" % l_menu[i]['m_items'][j]['i_fun'])
            fp.write("# \n")
        fp.write("   ] # end the menu dictionary \n")
        fp.flush()

#---------------------------------------------------------------------
# End of File
#---------------------------------------------------------------------
