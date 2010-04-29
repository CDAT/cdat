#!/usr/bin/env python
#
# The PCMDI Data Browser Menu Bar -  gui_menu module
#
###############################################################################
#                                                                             #
# Module:       gui_menu module                                               #
#                                                                             #
# Copyright:    "See file Legal.htm for copyright information."               #
#                                                                             #
# Authors:      PCMDI Software Team                                           #
#               Lawrence Livermore NationalLaboratory:                        #
#               support@pcmdi.llnl.gov                                        #
#                                                                             #
# Description:  PCMDI Software System browser Tkinter Main Menu Bar.          #
#                                                                             #
# Version:      4.0                                                           #
#                                                                             #
###############################################################################
#
#---------------------------------------------------------------------
# NOTE: need to use version of Python that imports Tkinter and Pmw
#---------------------------------------------------------------------
import Tkinter, Pmw, tkFileDialog
from tkMessageBox import askyesno, askokcancel
import __main__
import webbrowser
import genutil, cdutil, os, sys, string
from gui_support import gui_color
import gui_extend_menus
import gui_control
import gui_select_variable
import gui_output
import gui_about
import gui_functions
import gui_reset
import gui_saved_settings
import gui_set_idle_font
import gui_user_menus
import gui_message
import gui_formulate
import gui_busy
import gui_edit_list
import gui_support
import gui_defined_variables
import gui_statistics_question
#import gui_salstat_question
import gui_filters_question
import gui_bounds_question
import gui_ascii
import gui_ascii_cols
import gui_read_Struct
import gui_graphics_control
import gui_thermo
import gui_wk
import cdms2

# Get the previously saved state of the GUI
try:
   fn = '%s/PCMDI_GRAPHICS' % os.environ['HOME']
   sys.path.append(fn)
   import vcdat_initial
except:
   pass

#---------------------------------------------------------------------------
#
# Start of the "Defined Variables" panel GUI Layout
#
#---------------------------------------------------------------------------
# Start the Tkinter/Pmw GUI layout. The layout is listed from top to bottom.
# Starting with: the menu bar; followed by the "Select Variable" panel, which
# allows the user to select data from a directory or a database; followed by
# the "Graphics Control" panel, which allows the user to plot the selected or
# defined variables; followed by the "Dimension" panel, which allows the user to
# select subsets of the selected variable before plotting or storing into memory;
# followed by the "Defined Variables" panel, which allows the user to modify the
# variables that are stored in memory; and finally followed by the "Variable
# Information" scroll window, which displays variable information.
#
# All panels are contained within a paned widget. Thus, allowing the size of
# each section to expand or constrict.
#

#
#----------------------------------------------------------------------------------------
####### event for "Exiting" the VCDAT GUI
#----------------------------------------------------------------------------------------
#
class exit_browser:

   def __init__(self, parent ):
      if parent.menu.show_exit_popup_flg == 1:
         self.exit_dialog = Pmw.Dialog( parent,
               title = "Exiting VCDAT",
               buttons = ('Cancel', 'OK'),
               defaultbutton = 'Cancel',
               command = gui_control.Command(self.exit_execute, parent)
               )
   
         self.exit_dialog.transient( parent ) # Keep widget on top of its parent
   
         # Set up go home icon
         questionicon = Tkinter.Canvas(self.exit_dialog.interior(), bd=0, highlightthickness=0,
                                         background = gui_color.off_color_bg, 
                                         width = 32, height = 32)
         questionicon.pack( side='left', fill=Tkinter.BOTH, expand='no',  padx=1, pady=1 )
         img = Tkinter.PhotoImage( file = os.path.join(cdms2.__path__[0],'..','..','..','..', 'bin', 'remove.gif') )
         questionicon.create_image(0,0, anchor=Tkinter.NW, image=img )
   
         lbl=Tkinter.Label(self.exit_dialog.interior(),
               text = "Are you sure you want to end this VCDAT session?",
               justify = 'left',
               anchor = 'w',)
         lbl.pack( side = 'top', expand = 1, fill = 'both', padx=10, pady=5 )
   
         self.chbtn1_state = 0
         self.chkbtn1 = Tkinter.Checkbutton(self.exit_dialog.interior(),
                               text = "Save current VCDAT's GUI settings",
                               selectcolor=gui_color.one,
                               command = gui_control.Command(self.evt_save_gui_state)
                              )
         self.chkbtn1.pack(side = 'top',anchor = 'nw', expand = 1, padx = 10,pady=1)
   
         self.chbtn2_state = 0
         self.chkbtn2 = Tkinter.Checkbutton(self.exit_dialog.interior(),
                               text = 'Save current VCS settings',
                               selectcolor=gui_color.one,
                               command = gui_control.Command(self.evt_save_VCS_state)
                              )
         self.chkbtn2.pack(side = 'top',anchor = 'nw', expand = 1, padx = 10,pady=1)
   
         # Position dialog popup
         parent_geom = parent.geometry()
         geom = string.split(parent_geom, '+')
         geom2= string.split(geom[0],'x')
         d1 = string.atoi( geom[1] ); d2 = string.atoi( geom[2] )
         d3 = string.atoi( geom2[0] ); d4 = string.atoi( geom2[1] )
         d5=d1+(d3/7); d6=d2+(d4/3)

         self.exit_dialog.activate(globalMode = 'nograb' , geometry="+%d+%d" % (d5, d6))
      else:
         sys.exit( 0 ) # parent.destroy() is an alternative way to exit

   def evt_save_gui_state( self ):
      if self.chbtn1_state == 0: self.chbtn1_state = 1
      else: self.chbtn1_state = 0

   def evt_save_VCS_state( self ):
      if self.chbtn2_state == 0: self.chbtn2_state = 1
      else: self.chbtn2_state = 0

   def exit_execute( self, parent, button ):
      if button=='OK':
         if self.chbtn1_state == 1:
            gui_saved_settings.create( parent )
         if self.chbtn2_state == 1:
            msg = ''
            while (msg == ''):
               # Close all VCS Canvases
               for i in range(4):
                   try: parent.vcs[ i ].close()
                   except: pass
               # Remove all temporary templates
               template_list = parent.vcs[ parent.vcs_id ].listelements('template')
               for x in template_list:
                   if ((x[:5] == '_temp') or (x[:2] == '__')):
                      parent.vcs[ parent.vcs_id ].removeP( x )
         
               msg = parent.vcs[ parent.vcs_id ].saveinitialfile()
         self.exit_dialog.destroy()
         sys.exit( 0 ) # parent.destroy() is an alternative way to exit

      self.exit_dialog.destroy()

#
####### event to set "VCS Canvas GUI Defined Settings"
#
def evt_vcs_canvas_gui_toggle( parent ):
   import vcs
   from tkMessageBox import askquestion

   # Warn the user before executing the canvas switch
   if parent.menu.vcs_canvas_gui_settings_flg == 1:
      # Make sure the template editor and animation are closed before existing. 
      parent.vcs[ parent.vcs_id ].canvas_gui.notebook.selectpage("Data & Plot Info")
      parent.vcs[ parent.vcs_id ].animate.close()
      button = askquestion("Change to VCS Canvas?", "By making this change, you will destroy all currently displayed plots on the VCS Canvas GUI.", default='no')
   else:
      button = askquestion("Change to VCS Canvas GUI?", "By making this change, you will destroy all currently displayed plots on the VCS Canvas.", default='no')

   if button in ['no', False]:
      parent.menu.main_menu.vcs_canvas_gui.set(parent.menu.vcs_canvas_gui_settings_flg)
   else:
      asd_script = os.path.join(cdms2.__path__[0],'..','..','..','..','bin','ASD.scr')
      if parent.menu.vcs_canvas_gui_settings_flg == 1:
         for i in range( parent.number_of_vcs_canvas ):
            try: 
               parent.vcs[ i ].close()
               del parent.vcs[ i ]
               del __main__.__dict__['vcs_hook%i'%(i+1)] # IDLE's connection to VCS
            except: pass
         for i in range( parent.number_of_vcs_canvas ):
            parent.vcs[ i ]=vcs.init(call_from_gui = 1)
            parent.vcs[ i ].scriptrun( asd_script )
            __main__.__dict__['vcs_hook%i'%(i+1)] = parent.vcs[i] # IDLE's connection to VCS
            try:
               parent.vcs[ i ].setcolormap( parent.colormap_name, 0 )
            except:
               pass
            parent.vcs[ i ].mode = 1
            parent.plot_ct[ i ] = 0
         parent.menu.vcs_canvas_gui_settings_flg = 0
         parent.panelGC.evt_which_vcs_canvas(parent, None)
      else:
         for i in range( parent.number_of_vcs_canvas ):
            try:
               parent.vcs[ i ].close()
               del parent.vcs[ i ]
               del __main__.__dict__['vcs_hook%i'%(i+1)] # IDLE's connection to VCS
            except: pass
         parent.vcs[ 0 ]=vcs.init(call_from_gui = 1, gui=1)
         parent.vcs[ 0 ].scriptrun( asd_script )
         __main__.__dict__['vcs_hook1'] = parent.vcs[ 0 ] # IDLE's connection to VCS
         try: parent.vcs[ 0 ].setcolormap( parent.colormap_name, 0 )
         except: pass
         parent.vcs[ 0 ].mode = 0
         parent.plot_ct[ 0 ] = 0
   
         parent.menu.vcs_canvas_gui_settings_flg = 1
         parent.panelGC.evt_which_vcs_canvas(parent, None)

      parent.vcs_id = 0
      # Mac OS X 10.4 cannot support this feature
      if ((sys.platform == 'darwin') and (gui_control.do_aqua == 1)): parent.menu.vcs_canvas_gui_settings_flg = 0

      # Disable menu items if calling VCS Canvas GUI
      gui_graphics_control.show_and_unshow_graphics_options( parent )

#----------------------------------------------------------------------------------------
# Begin the creation of the file menu and its menu items
#----------------------------------------------------------------------------------------
class create:
   def __init__( self, parent ):
      # create the main toplevel menu
      self.main_menu = Pmw.MenuBar(parent,
                hull_relief = 'raised',
                hull_borderwidth = 2,
                balloon = parent.balloon
                )
      self.main_menu.pack(side='top', before=parent.pane, fill='x')

      parent.protocol("WM_DELETE_WINDOW", gui_control.Command(exit_browser, parent))

      # Set the tear off value
      tear_it = 1
      if ((sys.platform == 'darwin') and (gui_control.do_aqua == 1)): tear_it = 0

      #-------------------------------------------
      # menu 1 -- 'File'
      #-------------------------------------------
      create_file_menu( self.main_menu, parent, tear_it )

      #-------------------------------------------
      # menu 2 -- 'Preferences'
      #-------------------------------------------
      # Initialize the "Save Region Selection" Flag
      try:
         self.save_region = vcdat_initial.set.save_region
      except:
         self.save_region = 0
      # Initialize the "Squeeze Dimensions" Flag
      try:
         self.squeeze_dim_flg = vcdat_initial.set.squeeze_dim_flg
      except:
         self.squeeze_dim_flg = 1
      # Initialize the "Fortran Order" Flag
      try:
         self.fortran_order_flg = vcdat_initial.set.fortran_order_flg
      except:
         self.fortran_order_flg = 0
      # Initialize the "Retain User Defined Settings" Flag
      # THIS IS NO LONGER IN USE!!!
#      try:
#         self.retain_user_settings_flg = vcdat_initial.set.retain_user_settings_flg
#      except:
#         self.retain_user_settings_flg = 1
      # Initialize the "VCS Canvas GUI Settings" Flag
      try:
         self.vcs_canvas_gui_settings_flg = vcdat_initial.set.vcs_canvas_gui_settings_flg
      except:
         self.vcs_canvas_gui_settings_flg = 0
# DEAN or CHARLES -- Remove the line below to get the VCS Canvas GUI to show from VCDAT
  #    self.vcs_canvas_gui_settings_flg = 0
      # Mac OS X 10.4 cannot support this feature
      if ((sys.platform == 'darwin') and (gui_control.do_aqua == 1)): self.vcs_canvas_gui_settings_flg = 1
      # Initialize the "Popup Window Settings" Flag
      try:
         self.popup_window_settings_flg = vcdat_initial.set.popup_window_settings_flg
      except:
         self.popup_window_settings_flg = 1
      # Mac OS X 10.4 cannot support this feature
      if ((sys.platform == 'darwin') and (gui_control.do_aqua == 1)): self.popup_window_settings_flg = 0
      # Initialize the "View Axes" Flag
      try:
         self.view_axes_flg = vcdat_initial.set.view_axes_flg
      except:
         self.view_axes_flg = 1
      # Initialize the "View Bounds and Weights" Flag
      try:
         self.view_bounds_weights_flg = vcdat_initial.set.view_bounds_weights_flg
      except:
         self.view_bounds_weights_flg = 1
      # Initialize the "Meridian" Flag
      try:
         self.meridian_flg = vcdat_initial.set.meridian_flg
      except:
         self.meridian_flg = 0
      # Initialize the "Conversion to MV" Flag
      try:
         self.convert_to_MV_flg = vcdat_initial.set.convert_to_MV_flg
      except:
         self.convert_to_MV_flg = 0
      # Initialize the "Show VCDAT Popup Exit" Flag
      try:
         self.show_exit_popup_flg = vcdat_initial.set.show_exit_popup_flg
      except:
         self.show_exit_popup_flg = 1

      self.create_options_menu = create_options_menu( self.main_menu, parent,
              self.save_region,
              self.squeeze_dim_flg,
              self.fortran_order_flg,
              self.vcs_canvas_gui_settings_flg,
              self.popup_window_settings_flg,
              self.view_axes_flg,
              self.view_bounds_weights_flg,
              self.meridian_flg,
              self.convert_to_MV_flg,
              self.show_exit_popup_flg,
              tear_it
              )

      #-------------------------------------------
      # menu 3 -- 'Tools'
      #-------------------------------------------
      # Initialize the "Record Commands" Flag
      try:
         self.record_commands_flg = vcdat_initial.set.record_commands_flg
      except:
         self.record_commands_flg = 1
      create_tools_menu( self.main_menu, parent, 
              self.record_commands_flg,
              tear_it
              )
      if self.record_commands_flg == 1:
         gui_control.start_recording_commands( parent )

      #-------------------------------------------
      # menu 4 -- 'PCMDI Tools'
      #-------------------------------------------
      self.pcmdi_tools_menu = create_pcmdi_tools_menu( self.main_menu, parent, tear_it)
      #-------------------------------------------
      # menu 5 -- 'Visus Tools'
      #-------------------------------------------
      try:
         import pyvisus
         self.plot_tools_menu = pyvisus.create_visus_tools_menu( self.main_menu, parent, tear_it)
      except Exception,err:
         pass
      #-------------------------------------------
      # menu 6 -- 'Plot Tools'
      #-------------------------------------------
      self.plot_tools_menu = create_external_plot_tools_menu( self.main_menu, parent, tear_it)
      #-------------------------------------------
      # menu 7 -- 'Help'
      #-------------------------------------------
      create_help_menu( self.main_menu, parent, tear_it)
#

#----------------------------------------------------------------------------------------
# Begin the creation of the second file menu and its menu items
#----------------------------------------------------------------------------------------
class create2:
   def __init__( self, parent ):
      # create the main toplevel menu
      self.main_menu2 = Pmw.MenuBar(parent,
                hull_relief = 'raised',
                hull_borderwidth = 2,
                balloon = parent.balloon
                )
      self.main_menu2.pack(side='top', before=parent.pane, fill='x')

      parent.protocol("WM_DELETE_WINDOW", gui_control.Command(exit_browser, parent))

      # Set the tear off value
      tear_it = 1
      if ((sys.platform == 'darwin') and (gui_control.do_aqua == 1)): tear_it = 0
      # Initialize the "VCS Canvas GUI Settings" Flag
      try:
         self.vcs_canvas_gui_settings_flg = vcdat_initial.set.vcs_canvas_gui_settings_flg
      except:
         self.vcs_canvas_gui_settings_flg = 1
      # Mac OS X 10.4 cannot support this feature
      if ((sys.platform == 'darwin') and (gui_control.do_aqua == 1)): self.vcs_canvas_gui_settings_flg = 1

      #-------------------------------------------
      # menu 1 -- 'File'
      #-------------------------------------------
      create_file_menu( self.main_menu2, parent, tear_it )

      #-------------------------------------------
      # menu 2 -- 'Preferences'
      #-------------------------------------------
      self.create_options_menu2 = create_options_menu2( self.main_menu2, parent,
              self.vcs_canvas_gui_settings_flg,
              tear_it
              )

      #-------------------------------------------
      # menu 5 -- 'Help'
      #-------------------------------------------
      create_help_menu( self.main_menu2, parent, tear_it)


#----------------------------------------------------------------------------------------
# Begin the creation of the file menu and its menu items
#----------------------------------------------------------------------------------------
class create_file_menu:
   def __init__( self, main_menu, parent, tear_it ):
      file_name = 'File'
      if ((sys.platform == 'darwin') and (gui_control.do_aqua == 1)): file_name = 'File '
      main_menu.addmenu(file_name, 'Open/Save Files and Close Browser', tearoff = tear_it)
      #
      # Create the "Open File" menu item
      main_menu.addmenuitem(file_name, 'command', 'Open data file',
                         label = 'Open File...',
                         command = gui_control.Command(self.evt_open_file, parent)
                        )
      #
      # Create the cascade "Open File Type" menu and its items
      main_menu.addcascademenu(file_name, 'Open File Types',
                              'Open data file types',
                              label = 'Open File Types',
                              traverseSpec = 'z', tearoff = tear_it
                             )
      main_menu.addmenuitem('Open File Types', 'command', 'Open data file type',
                       label = 'Data Files',
                       command = gui_control.Command(self.evt_data_files, parent)
                      )
      main_menu.addmenuitem('Open File Types', 'command', 'Open data file type',
                       label = '*.nc Files',
                       command = gui_control.Command(self.evt_nc_files, parent)
                      )
      main_menu.addmenuitem('Open File Types', 'command', 'Open data file type',
                       label = '*.ctl Files',
                       command = gui_control.Command(self.evt_ctl_files, parent)
                      )
      main_menu.addmenuitem('Open File Types', 'command', 'Open data file type',
                       label = '*.cdms Files',
                       command = gui_control.Command(self.evt_cdms_files, parent)
                      )
      main_menu.addmenuitem('Open File Types', 'command', 'Open data file type',
                       label = '*.dic Files',
                       command = gui_control.Command(self.evt_dic_files, parent)
                      )
      main_menu.addmenuitem('Open File Types', 'command', 'Open data file type',
                       label = '*.hdf Files',
                       command = gui_control.Command(self.evt_hdf_files, parent)
                      )
      main_menu.addmenuitem('Open File Types', 'command', 'Open data file type',
                       label = '*.xml Files',
                       command = gui_control.Command(self.evt_xml_files, parent)
                      )
      main_menu.addmenuitem('Open File Types', 'command', 'Open data file type',
                       label = '*.pp Files',
                       command = gui_control.Command(self.evt_pp_files, parent)
                      )
      main_menu.addmenuitem('Open File Types', 'command', 'Open data file type',
                       label = '*.cdml Files',
                       command = gui_control.Command(self.evt_cdml_files, parent)
                      )
      main_menu.addmenuitem('Open File Types', 'command', 'Open data file type',
                       label = 'All Files',
                       command = gui_control.Command(self.evt_all_files, parent)
                      )
      main_menu.addmenuitem('Open File Types', 'command', 'Open data file type',
                       label = 'Find Pattern',
                       command = gui_control.Command(self.evt_find_files, parent)
                      )
      main_menu.addmenuitem('Open File Types', 'command', 'Open data file type',
                       label = 'Datasets',
                       command = gui_control.Command(self.evt_datasets_files, parent)
                      )
      #
      # Create the cascade "Save VCS Canvas" menu and its items
      main_menu.addcascademenu(file_name, 'Save VCS Canvas As', 'Save VCS Canvas',
                          label = 'Save Plot As',
                          traverseSpec = 'z', tearoff = tear_it
                         )
      main_menu.addmenuitem('Save VCS Canvas As', 'command', 'Save current VCS Canvas as',
                          label = 'Postscript...',
                          command = gui_control.Command(self.evt_save_plot_to_postscript, parent)
                         )
      main_menu.addmenuitem('Save VCS Canvas As', 'command', 'Save current VCS Canvas as',
                          label = 'PNG...',
                          command = gui_control.Command(self.evt_save_plot_to_png, parent)
                         )
      main_menu.addmenuitem('Save VCS Canvas As', 'command', 'Save current VCS Canvas as',
                          label = 'SVG...',
                          command = gui_control.Command(self.evt_save_plot_to_svg, parent)
                         )
      main_menu.addmenuitem('Save VCS Canvas As', 'command', 'Save current VCS Canvas as',
                          label = 'PDF...',
                          command = gui_control.Command(self.evt_save_plot_to_pdf, parent)
                         )
      main_menu.addmenuitem('Save VCS Canvas As', 'command', 'Save current VCS Canvas as',
                          label = 'GIF...',
                          command = gui_control.Command(self.evt_save_plot_to_gif, parent)
                         )
      main_menu.addmenuitem('Save VCS Canvas As', 'command', 'Save current VCS Canvas as',
                          label = 'EPS...',
                          command = gui_control.Command(self.evt_save_plot_to_eps, parent)
                         )
      main_menu.addmenuitem('Save VCS Canvas As', 'command', 'Save current VCS Canvas as',
                          label = 'CGM...',
                          command = gui_control.Command(self.evt_save_plot_to_cgm, parent)
                         )
      #
      # Create the cascade "Print VCS Canvas" menu and its items
      main_menu.addcascademenu(file_name, 'Print VCS Canvas To', 'Print VCS Canvas',
                          label = 'Print Plot On',
                          traverseSpec = 'z', tearoff = tear_it
                         )
      parent.printer_list = printer_list = gui_functions.get_available_printers()
      for x in printer_list:
         main_menu.addmenuitem('Print VCS Canvas To', 'command', 'Name of Printer',
                               label = x,
                               command = gui_control.Command(self.evt_returned_printers, parent, x)
                              )

      main_menu.addmenuitem('Print VCS Canvas To', 'separator')
      main_menu.addmenuitem('Print VCS Canvas To', 'command', 'Print current VCS Canvas to',
                            label = 'Specify Printer',
                            command=gui_control.Command(self.evt_specify_printer, parent)
                           )
      #
      # Create the save state of the System
      main_menu.addmenuitem(file_name, 'command', 'User defined options',
                label = 'Save Current State of VCS (Graphics System)...',
                command = gui_control.Command(self.evt_save_state_of_VCS, parent)
              )
      main_menu.addmenuitem(file_name, 'command', 'User defined options',
                label = 'Save Current State of System as...',
                command = gui_control.Command(self.evt_save_state_of_System, parent)
              )
      main_menu.addmenuitem(file_name, 'command', 'User defined options',
                label = 'Read Script File...',
                command = gui_control.Command(self.evt_read_script_file, parent)
              )
      #
      # Create the cascade "Exit" menu and its items
      main_menu.addmenuitem(file_name, 'separator')
      main_menu.addmenuitem(file_name, 'command', 'Close browser',
                          label = "Exit VCDAT",
                          command = gui_control.Command(exit_browser, parent)
			 )

   #---------------------------------------------------------------------------
   # Define event function's for "File" menu items
   #---------------------------------------------------------------------------
   #
   #
   ####### event that will pop up the tkFile dialog and enter the directory and file string
   # 	   in the directory and file entry widgets
   #
   def evt_open_file( self, parent ):
      # Show the popup directory dialog
      self.dialog = tkFileDialog.Open(master=parent,
                           filetypes=gui_control.datatypes, title = 'File Select')
      dirfilename=self.dialog.show(initialdir=os.getcwd())
      if dirfilename in ['', ()]: return
      f=string.split( dirfilename, '/' )[-1]
      d=dirfilename[0:(len(dirfilename) - len(f) - 1)]
      parent.panelSV.tin2.clear()
      parent.panelSV.tin3.clear()
      parent.panelSV.tin2.setentry( d )
      parent.file_search_pattern = 'All Files'
      gui_select_variable.evt_enter_directory( parent, None )
      parent.panelSV.tin3.setentry( f )
      tmp = parent.file_search_pattern
      gui_select_variable.evt_enter_file( parent, None )
      parent.file_search_pattern = tmp
   #
   ####### event for seleting the PCMDI data files
   #
   def evt_data_files( self, parent ):
      parent.file_search_pattern = None
      gui_select_variable.evt_enter_directory( parent, None )
   #
   ####### event for seleting netCDF data files
   #
   def evt_nc_files( self, parent ):
      parent.file_search_pattern = "*.nc"
      gui_select_variable.evt_enter_directory( parent, None )
   #
   ####### event for seleting GrADS data files
   #
   def evt_ctl_files( self, parent ):
      parent.file_search_pattern = "*.ctl"
      gui_select_variable.evt_enter_directory( parent, None )
   #
   ####### event for seleting CDMS data files
   #
   def evt_cdms_files( self, parent ):
      parent.file_search_pattern = "*.cdms"
      gui_select_variable.evt_enter_directory( parent, None )
   #
   ####### event for seleting DRS data files
   #
   def evt_dic_files( self, parent ):
      parent.file_search_pattern = "*.dic"
      gui_select_variable.evt_enter_directory( parent, None )
   #
   def evt_hdf_files( self, parent ):
      parent.file_search_pattern = "*.hdf"
      gui_select_variable.evt_enter_directory( parent, None )
   #
   ####### event for seleting XML data files
   #
   def evt_xml_files( self, parent ):
      parent.file_search_pattern = "*.xml"
      gui_select_variable.evt_enter_directory( parent, None )
   #
   ####### event for seleting PP data files
   #
   def evt_pp_files( self, parent ):
      parent.file_search_pattern = "*.pp"
      gui_select_variable.evt_enter_directory( parent, None )
   #
   ####### event for seleting CDML data files
   #
   def evt_cdml_files( self, parent ):
      parent.file_search_pattern = "*.cdml"
      gui_select_variable.evt_enter_directory( parent, None )
   #
   ####### event for seleting ALL data files
   #
   def evt_all_files( self, parent ):
      parent.file_search_pattern = "All Files"
      gui_select_variable.evt_enter_directory( parent, None )
   #
   ####### event for seleting Pattern files
   #
   def evt_find_files( self, parent ):
      parent.file_search_pattern = "Find Pattern"
      gui_output.create(parent, 's','Search Directory for String Occurrence:','Enter Pattern Search Sub-String:')
      gui_select_variable.evt_enter_directory( parent, None )
   #
   ####### event for seleting the Database data files
   #
   def evt_datasets_files( self, parent ):
      parent.file_search_pattern = None
      gui_select_variable.evt_enter_directory( parent, None )
   #
   ####### event to save "Plot" in a 'Postscript' file
   #
   def evt_save_plot_to_postscript( self, parent ):
      gui_busy.busyStart( self, parent )
      filetypes = [ ("Postscript File", ".ps") ]
      # Show the popup directory dialog
      sfile = tkFileDialog.asksaveasfilename( master=parent, filetypes = filetypes,
                           title = 'Save VCS Plot to a Postscript File' )
      if sfile in ['', ()]: gui_busy.busyEnd( self, parent ); return
      if sfile[-3:] != '.ps': sfile += '.ps'

      # Get the page orientation (i.e., either l='landscape', or p='portrait')
##       o = 'l'
##       if parent.panelGC.page_orientation_flg == 0: o = 'p'
      parent.vcs[ parent.vcs_id ].postscript( sfile )
      gui_control.record_command( parent, "\n# Save VCS Canvas in Postscript File" )
      gui_control.record_command( parent, "vcs_canvas_list[ %d ].postscript( '%s' )" % (parent.vcs_id, sfile))
      gui_busy.busyEnd( self, parent )
   #
   ####### event to save "Plot" in a 'EPS' file
   #
   def evt_save_plot_to_eps( self, parent ):
      gui_busy.busyStart( self, parent )
      filetypes = [ ("Postscript File", ".eps") ]
      # Show the popup directory dialog
      sfile = tkFileDialog.asksaveasfilename( master=parent, filetypes = filetypes,
                           title = 'Save VCS Canvas in Encapsulated Postscript File' )
      if sfile in ['', ()]: gui_busy.busyEnd( self, parent ); return
      if sfile[-4:] != '.eps': sfile += '.eps'

      # Get the page orientation (i.e., either l='landscape', or p='portrait')
      o = 'l'
      if parent.panelGC.page_orientation_flg == 0: o = 'p'
      parent.vcs[ parent.vcs_id ].eps( sfile, o )
      gui_control.record_command( parent, "\n# Save VCS Canvas in Encapsulated Postscript File" )
      gui_control.record_command( parent, "vcs_canvas_list[ %d ].eps( '%s', '%s' )" % (parent.vcs_id, sfile, o))
      gui_busy.busyEnd( self, parent )
   #
   ####### event to save "Plot" in a 'GIF' file
   #
   def evt_save_plot_to_gif( self, parent ):
      gui_busy.busyStart( self, parent )
      filetypes = [ ("Postscript File", ".gif") ]
      # Show the popup directory dialog
      sfile = tkFileDialog.asksaveasfilename( master=parent, filetypes = filetypes,
                           title = 'Save VCS Canvas in GIF File' )
      if sfile in ['', ()]: gui_busy.busyEnd( self, parent ); return
      if sfile[-4:] != '.gif': sfile += '.gif'

      # Get the page orientation (i.e., either l='landscape', or p='portrait')
      o = 'l'
      if parent.panelGC.page_orientation_flg == 0: o = 'p'
      parent.vcs[ parent.vcs_id ].gif( sfile, o )
      gui_control.record_command( parent, "\n# Save VCS Canvas in GIF File" )
      gui_control.record_command( parent, "vcs_canvas_list[ %d ].gif( '%s', '%s' )" % (parent.vcs_id, sfile, o))
      gui_busy.busyEnd( self, parent )
   #
   ####### event to save "Plot" in a 'CGM' file
   #
   def evt_save_plot_to_cgm( self, parent ):
      gui_busy.busyStart( self, parent )
      filetypes = [ ("CGM File", ".cgm") ]
      # Show the popup directory dialog
      sfile = tkFileDialog.asksaveasfilename( master=parent, filetypes = filetypes,
                           title = 'Save VCS Canvas in CGM File' )
      if sfile in ['', ()]: gui_busy.busyEnd( self, parent ); return
      if sfile[-4:] != '.cgm': sfile += '.cgm'

      # Get the page orientation (i.e., either l='landscape', or p='portrait')
      o = 'l'
      if parent.panelGC.page_orientation_flg == 0: o = 'p'
      parent.vcs[ parent.vcs_id ].cgm( sfile, o )
      gui_control.record_command( parent, "\n# Save VCS Canvas in CGM File" )
      gui_control.record_command( parent, "vcs_canvas_list[ %d ].cgm( '%s', '%s' )" % (parent.vcs_id, sfile, o))
      gui_busy.busyEnd( self, parent )
   #
   ####### event to save "Plot" in a 'PDF' file
   #
   def evt_save_plot_to_pdf( self, parent ):
      gui_busy.busyStart( self, parent )
      filetypes = [ ("PDF File", ".pdf") ]
      # Show the popup directory dialog
      sfile = tkFileDialog.asksaveasfilename( master=parent, filetypes = filetypes,
                           title = 'Save VCS Canvas in PDF File' )
      if sfile in ['', ()]: gui_busy.busyEnd( self, parent ); return
      if sfile[-4:] != '.pdf': sfile += '.pdf'

      parent.vcs[ parent.vcs_id ].pdf( sfile )
      gui_control.record_command( parent, "\n# Save VCS Canvas in PDF File" )
      gui_control.record_command( parent, "vcs_canvas_list[ %d ].pdf( '%s', '%s' )" % (parent.vcs_id, sfile))
      gui_busy.busyEnd( self, parent )
   #
   #
   ####### event to save "Plot" in a 'SVG' file
   #
   def evt_save_plot_to_svg( self, parent ):
      gui_busy.busyStart( self, parent )
      filetypes = [ ("SVG File", ".svg") ]
      # Show the popup directory dialog
      sfile = tkFileDialog.asksaveasfilename( master=parent, filetypes = filetypes,
                           title = 'Save VCS Canvas in SVG File' )
      if sfile in ['', ()]: gui_busy.busyEnd( self, parent ); return
      if sfile[-4:] != '.svg': sfile += '.svg'

      parent.vcs[ parent.vcs_id ].svg( sfile )
      gui_control.record_command( parent, "\n# Save VCS Canvas in PDF File" )
      gui_control.record_command( parent, "vcs_canvas_list[ %d ].svg( '%s', '%s' )" % (parent.vcs_id, sfile))
      gui_busy.busyEnd( self, parent )
   #
   #
   ####### event to save "Plot" in a 'PNG' file
   #
   def evt_save_plot_to_png( self, parent ):
      gui_busy.busyStart( self, parent )
      filetypes = [ ("PNG File", ".png") ]
      # Show the popup directory dialog
      sfile = tkFileDialog.asksaveasfilename( master=parent, filetypes = filetypes,
                           title = 'Save VCS Canvas in PNG File' )
      if sfile in ['', ()]: gui_busy.busyEnd( self, parent ); return
      if sfile[-4:] != '.png': sfile += '.png'

      parent.vcs[ parent.vcs_id ].png( sfile )
      gui_control.record_command( parent, "\n# Save VCS Canvas in PDF File" )
      gui_control.record_command( parent, "vcs_canvas_list[ %d ].png( '%s', '%s' )" % (parent.vcs_id, sfile))
      gui_busy.busyEnd( self, parent )
   #
   ####### event to plot VCS Canvas to desinated printer
   #
   def evt_returned_printers( self, parent, printer_name ):
      o = 'l'
      if parent.panelGC.page_orientation_flg == 0: o = 'p'
      try:
         parent.vcs[ parent.vcs_id ].printer( printer_name  )
         gui_control.record_command( parent, "\n# Send VCS Canvas to %s Printer" % printer_name )
         gui_control.record_command( parent, "vcs.printer( '%s' )" % (printer_name ,) )
      except:
            gui_message.error( 'Error in printing.' )
   #
   ####### event to plot VCS Canvas to "Specified" printer
   #
   def evt_specify_printer( self, parent ):
       gui_output.create(parent, 't','Save VCS Plot Output to Printer:','Enter Printer Name:')
   #
   ####### event to "Save the Current State of the VCS graphics System"
   #
   def evt_save_state_of_VCS( self, parent ):
      msg = parent.vcs[ parent.vcs_id ].saveinitialfile()
      s = Pmw.TextDialog(parent, title='Session saved.')
      s.insert('end', msg)
   #
   ####### event to "Save the Current State of the System"
   #
   def evt_save_state_of_System( self, parent ):
      filetypes = [
         ("Python Output File", ".py")
         ]
      # Show the popup directory dialog
      sfile = tkFileDialog.asksaveasfilename( master=parent,
                           filetypes = filetypes, 
                           title = 'Save Current State of System As' )
      if sfile:
         try:
            record_script = ( '%s/PCMDI_GRAPHICS/vcdat_recording_script_file.py' %
                  os.environ['HOME'] )
         except:
            gui_message.error( 'Cannot find the PCMDI_GRAPHICS directory.' )
            return
         if sfile[-3:] != '.py': sfile += '.py'
         sys_cmd = 'cp %s %s' % (record_script, sfile)
         os.system( sys_cmd )
         msg = parent.vcs[ parent.vcs_id ].saveinitialfile()

   #
   ####### event to "Read Script File"
   #
   def evt_read_script_file( self, parent ):
      datatypes = [
         ("Search for Data files", "*.scr *.py"),
         ("Search for VCS files", "*.scr"),
         ("Search for Python files", "*.py"),
         ("All files", "*")
         ]
      # Show the popup directory dialog
      self.dialog = tkFileDialog.Open(master=parent,
                           filetypes=datatypes, title = 'Script File Select')
      dirfilename=self.dialog.show(initialdir=os.getcwd())
      if dirfilename == '':
        return
      elif (dirfilename[-4:] == '.scr'):    # read VCS script file
        parent.panelDM.var3 = None
        parent.vcs[ parent.vcs_id ].clear( )
        parent.vcs[ parent.vcs_id ].scriptrun( dirfilename )
        gui_defined_variables.update_defined( parent )
      elif (dirfilename[-3:] == '.py'):    # read VCS script file
        execfile( dirfilename, __main__.__dict__ )

      # Redisplay the Template and Graphics Method list windows
      parent.panelDV.redisplay_template( parent )
      parent.panelDV.redisplay_graphics_method( parent )

#----------------------------------------------------------------------------------------
# Create the Preferences menu and its menu items
#----------------------------------------------------------------------------------------
class create_options_menu2: 
   def __init__( self, main_menu, parent, vcs_canvas_gui_settings_flg, tear_it ):
      Pre_name = 'Preferences'
      if ((sys.platform == 'darwin') and (gui_control.do_aqua == 1)): Pre_name = 'Preferences  '
      main_menu.addmenu(Pre_name, 'Set User Browser Preferences', tearoff = tear_it)

      main_menu.addmenuitem(Pre_name, 'command', 'Change to VCDAT Full',
                label = 'Change to VCDAT Full',
                command = gui_control.Command(self.evt_change_to_vcdat_full, parent)
              )

      # Save the current GUI and VCS settings and show the EXIT popup (or not)
      main_menu.addmenuitem(Pre_name, 'command', 'User defined options',
                label = "Save curent VCDAT's GUI Settings",
                command = gui_control.Command(self.evt_save_GUI_settings, parent)
              )
# DEAN or CHARLES -- Uncomment the lines below back to get the VCS Canvas GUI to show from VCDAT
      main_menu.addmenuitem(Pre_name, 'separator')

      # Initialize the checkbutton to vcs_canvas_gui_settings_flg
      main_menu.vcs_canvas_gui = Tkinter.IntVar()
      main_menu.vcs_canvas_gui.set(vcs_canvas_gui_settings_flg)
      main_menu.addmenuitem(Pre_name, 'checkbutton', 'Toggle me on/off',
                label = 'Use the VCS Canvas GUI to display plots',
                selectcolor=gui_color.one,
                variable = main_menu.vcs_canvas_gui,
                command = gui_control.Command(evt_vcs_canvas_gui_toggle, parent)
              )


   ####### event to "Save GUI Settings"
   #
   def evt_save_GUI_settings( self, parent ):
      gui_saved_settings.create( parent )

   def evt_change_to_vcdat_full( self, parent ):
      pos = '%dx%d+%d+%d' % (parent.win_w, parent.win_h, parent.dxwin, parent.dywin)
      parent.geometry(pos)
      parent.pane.configurepane('panelDV', min=parent.pane4_min, size=parent.pane4_size, max = parent.win_h)
      parent.pane.configurepane('panelVI', min=parent.pane5_min, max = parent.win_h)

      parent.pane.configurepane( 'panelDM', min=parent.pane3_min, size=parent.pane3_size )
      parent.pane.configurepane( 'panelDV', min=parent.pane4_min, size=parent.pane4_size )
      parent.pane.configurepane( 'panelVI', min=parent.pane5_min )

      parent.panelGC.opt_btn.pack(side='left', before=parent.panelGC.sep1, fill='both')
      parent.panelGC.define_btn.pack(side='left', fill='both', expand= 1, padx=0)
      parent.menu2.main_menu2.forget()
      parent.menu.main_menu.pack( side='top', before=parent.pane, fill='x' )
      parent.menu.main_menu.vcs_canvas_gui.set(parent.menu.vcs_canvas_gui_settings_flg)
      parent.vcdat_lite = 0

#----------------------------------------------------------------------------------------
# Create the Preferences menu and its menu items
#----------------------------------------------------------------------------------------
class create_options_menu:
   def __init__( self, main_menu, parent, save_region, squeeze_dim_flg, fortran_order_flg, 
              vcs_canvas_gui_settings_flg, popup_window_settings_flg, view_axes_flg,
              view_bounds_weights_flg, meridian_flg, convert_to_MV_flg, show_exit_popup_flg, tear_it ):
      Pre_name = 'Preferences'
      if ((sys.platform == 'darwin') and (gui_control.do_aqua == 1)): Pre_name = 'Preferences  '
      main_menu.addmenu(Pre_name, 'Set User Browser Preferences', tearoff = tear_it)

      main_menu.addmenuitem(Pre_name, 'command', 'Change to VCDAT Lite',
                label = 'Change to VCDAT Lite',
                command = gui_control.Command(self.evt_change_to_vcdat_lite, parent)
              )

      # Initialize the checkbutton to vcs_canvas_gui_settings_flg
      main_menu.vcs_canvas_gui = Tkinter.IntVar()
      main_menu.vcs_canvas_gui.set(vcs_canvas_gui_settings_flg)
      main_menu.addmenuitem(Pre_name, 'checkbutton', 'Toggle me on/off',
                label = 'Use the VCS Canvas GUI to display plots',
                selectcolor=gui_color.one,
                variable = main_menu.vcs_canvas_gui,
                command = gui_control.Command(evt_vcs_canvas_gui_toggle, parent)
              )

      main_menu.addmenuitem(Pre_name, 'separator')

      # Space for fonts
      main_menu.addmenuitem(Pre_name, 'command', 'Change def  Font',
                          label = "Change VCDAT's Default Font",
                          command = gui_control.Command(self.evt_change_font, parent)
                         )
      main_menu.addmenuitem(Pre_name, 'command', 'Add a Font',
                          label = 'Add a font',
                          command = gui_control.Command(self.evt_add_font, parent)
                         )
      
      main_menu.addmenuitem(Pre_name, 'separator')
      # Set the GUI Preferences
   
      main_menu.look_and_feel_mode_toggle=[]
      for i in range(4):
         main_menu.look_and_feel_mode_toggle.append( Tkinter.IntVar() )
         main_menu.look_and_feel_mode_toggle[i].set( 0 )
      main_menu.look_and_feel_mode_toggle[ 0 ].set( 1 )
      main_menu.addcascademenu(Pre_name, 'LookandFeel',
                'VCDAT Look and Feel',
                label = "Select VCDAT's Primary Mode of Operation",
                traverseSpec = 'z', tearoff = tear_it
              )
      main_menu.addmenuitem('LookandFeel', 'checkbutton', 'Toggle, Define None',
                          label = 'Default Mode',
                          selectcolor=gui_color.one,
                          variable = main_menu.look_and_feel_mode_toggle[0],
                          command = gui_control.Command(self.evt_mode_of_operation, parent, 0)
                         )
      main_menu.addmenuitem('LookandFeel', 'checkbutton', 'Toggle, Define None',
                          label = 'Browse Variable Mode',
                          selectcolor=gui_color.one,
                          variable = main_menu.look_and_feel_mode_toggle[1],
                          command = gui_control.Command(self.evt_mode_of_operation, parent, 1)
                         )
      main_menu.addmenuitem('LookandFeel', 'checkbutton', 'Toggle, Define None',
                          label = 'Manipulation Data Mode',
                          selectcolor=gui_color.one,
                          variable = main_menu.look_and_feel_mode_toggle[2],
                          command = gui_control.Command(self.evt_mode_of_operation, parent, 2)
                         )
      main_menu.addmenuitem('LookandFeel', 'checkbutton', 'Toggle, Define None',
                          label = 'Manipulate Graphics Display Mode',
                          selectcolor=gui_color.one,
                          variable = main_menu.look_and_feel_mode_toggle[3],
                          command = gui_control.Command(self.evt_mode_of_operation, parent, 3)
                         )

      # Initialize the checkbutton to retain_user_settings_flg:
      # THIS IS NO LONGER IN USE!!!
#      main_menu.var_retain_sub = Tkinter.IntVar()
#      main_menu.var_retain_user = Tkinter.IntVar()
#      main_menu.var_retain_user.set(retain_user_settings_flg)
#      main_menu.addmenuitem(Pre_name, 'checkbutton', 'Toggle me on/off',
#                label = 'Retain User Defined Settings',
#                selectcolor=gui_color.one,
#                variable = main_menu.var_retain_user,
#                command = gui_control.Command(self.evt_retain_user_toggle, parent)
#              )

      # Initialize the checkbutton to popup_window_settings_flg:
      main_menu.var_popup_window = Tkinter.IntVar()
      main_menu.var_popup_window.set(popup_window_settings_flg)
      main_menu.addmenuitem(Pre_name, 'checkbutton', 'Toggle me on/off',
                label = 'Keep Popup Windows in front of Main GUI',
                selectcolor=gui_color.one,
                variable = main_menu.var_popup_window,
                command = gui_control.Command(self.evt_popup_window_toggle, parent)
              )

      main_menu.addmenuitem(Pre_name, 'command', 'User defined options',
                label = 'Reset GUI Swap Space...',
                command = gui_control.Command(self.evt_reset_GUI_swap_space, parent)
              )
      main_menu.addmenuitem(Pre_name, 'command', 'User defined options',
                label = 'Reset GUI to Initial State',
                command = gui_control.Command(self.evt_reset_GUI_state, parent)
              )

      main_menu.addmenuitem(Pre_name, 'separator')

      # Save the current GUI and VCS settings and show the EXIT popup (or not)
      main_menu.addmenuitem(Pre_name, 'command', 'User defined options',
                label = "Save curent VCDAT's GUI Settings",
                command = gui_control.Command(self.evt_save_GUI_settings, parent)
              )
##       main_menu.addmenuitem(Pre_name, 'command', 'User defined options',
##                 label = 'Save current VCS Settings',
##                 command = gui_control.Command(self.evt_save_VCS_settings, parent)
##               )
      main_menu.exit_popup_window = Tkinter.IntVar()
      main_menu.exit_popup_window.set(show_exit_popup_flg)
      main_menu.addmenuitem(Pre_name, 'checkbutton', 'Toggle me on/off',
                label = 'Show VCDAT Exit Popup',
                selectcolor=gui_color.one,
                variable = main_menu.exit_popup_window,
                command = gui_control.Command(self.evt_exit_popup_toggle, parent)
              )

      main_menu.addmenuitem(Pre_name, 'separator')

      # Initialize the checkbutton for axes, to indicate whether to view axes in
      # selection panel.
      main_menu.view_axes = Tkinter.IntVar()
      main_menu.view_axes.set(view_axes_flg)
      main_menu.addmenuitem(Pre_name, 'checkbutton', 'Toggle me on/off',
                label = 'View Axes in Variable List',
                selectcolor=gui_color.one,
                variable = main_menu.view_axes,
                command = gui_control.Command(self.evt_view_axes_toggle, parent)
              )

      # Initialize the checkbutton for bounds and weights, to indicate whether to 
      # view bounds and weights in selection panel.
      main_menu.view_bounds_weights = Tkinter.IntVar()
      main_menu.view_bounds_weights.set(view_bounds_weights_flg)
      main_menu.addmenuitem(Pre_name, 'checkbutton', 'Toggle me on/off',
                label = 'View bounds and weights in Variable List',
                selectcolor=gui_color.one,
                variable = main_menu.view_bounds_weights,
                command = gui_control.Command(self.evt_view_bounds_weights_toggle, parent)
              )

      main_menu.addmenuitem(Pre_name, 'separator')

      #
      # Set the Variable and Variable Dimension Preferences
      main_menu.addcascademenu(Pre_name, 'Region',
                'Dimension Alias for each Dimension',
                label = "Select Predefined Region",
                traverseSpec = 'z', tearoff = tear_it
              )
      region_names = {0:'None', 1:'Africa', 2:'Antarctic', 3:'Arctic', 4:'Asia', 5:'Australia', 6:'Europe', 7:'Indian Ocean', 8:'North America', 9:'North Atlantic', 10:'North Pacific', 11:'South America', 12:'South Atlantic', 13:'South Pacific', 14:'Tropical Atlantic', 15:'Tropical Pacific'}
      main_menu.region_toggle=[]
      main_menu.meridian_toggle_none = Tkinter.IntVar()
      for i in range(len(region_names)):
         main_menu.region_toggle.append( Tkinter.IntVar() )
         main_menu.region_toggle[i].set( 0 )
         if i == save_region:
            main_menu.region_toggle[i].set( 1 )
         main_menu.addmenuitem('Region', 'checkbutton', region_names[i],
                             label = region_names[i],
                             variable = main_menu.region_toggle[i],
                             selectcolor=gui_color.one,
                command = gui_control.Command(self.evt_set_region, parent, i)
                 )

      # Initialize the checkbutton to squeeze_dim_flg:
      main_menu.var_squeeze_dim = Tkinter.IntVar()
      main_menu.var_squeeze_dim.set(squeeze_dim_flg)
      main_menu.addmenuitem(Pre_name, 'checkbutton', 'Toggle me on/off',
                label = 'Squeeze Dimensions',
                selectcolor=gui_color.one,
                variable = main_menu.var_squeeze_dim,
                command = gui_control.Command(self.evt_squeeze_dim_toggle, parent)
              )

      #
      # Dimension Aliases
      main_menu.addcascademenu(Pre_name, 'Dimension Aliases',
                'Dimension Alias for each Dimension',
                label = "Dimension Aliases for",
                traverseSpec = 'z', tearoff = tear_it
              )
      main_menu.addmenuitem('Dimension Aliases', 'command', 'Longitude Aliases',
                label = 'Longitude...',
                command = gui_control.Command(gui_edit_list.create, parent, 'Longitude')
              )
      main_menu.addmenuitem('Dimension Aliases', 'command', 'Latitude Aliases',
                label = 'Latitude...',
                command = gui_control.Command(gui_edit_list.create, parent, 'Latitude')
              )
      main_menu.addmenuitem('Dimension Aliases', 'command', 'Time Aliases',
                label = 'Time...',
                command = gui_control.Command(gui_edit_list.create, parent, 'Time')
              )
      main_menu.addmenuitem('Dimension Aliases', 'command', 'Level Aliases',
                label = 'Level...',
                command = gui_control.Command(gui_edit_list.create, parent, 'Level')
              )

      #
      # Define Meridian
      main_menu.addcascademenu(Pre_name, 'Define Meridian',
                'Define Meridian for Longitude',
                label = "Define Meridian for Longitude Dimension",
                traverseSpec = 'z', tearoff = tear_it
              )
      main_menu.meridian_toggle_none = Tkinter.IntVar()
      if meridian_flg == 0: main_menu.meridian_toggle_none.set( 1 )
      else: main_menu.meridian_toggle_none.set( 0 )
      main_menu.addmenuitem('Define Meridian', 'checkbutton', 'Toggle, Define None',
                          label = 'None',
                          selectcolor=gui_color.one,
                          variable = main_menu.meridian_toggle_none,
                          command = gui_control.Command(self.evt_set_meridian_toggle, parent, 0)
                         )

      main_menu.meridian_toggle_greenwich = Tkinter.IntVar()
      if meridian_flg == 1: main_menu.meridian_toggle_greenwich.set( 1 )
      else: main_menu.meridian_toggle_greenwich.set( 0 )
      main_menu.addmenuitem('Define Meridian', 'checkbutton', 'Toggle, Define None',
                          label = 'Meridian of Greenwich (-180 to 180)',
                          selectcolor=gui_color.one,
                          variable = main_menu.meridian_toggle_greenwich,
                          command = gui_control.Command(self.evt_set_meridian_toggle, parent, 1)
                         )

      main_menu.meridian_toggle_dataline = Tkinter.IntVar()
      if meridian_flg == 2: main_menu.meridian_toggle_dataline.set( 1 )
      else: main_menu.meridian_toggle_dataline.set( 0 )
      main_menu.addmenuitem('Define Meridian', 'checkbutton', 'Toggle, Define None',
                          label = 'Date Line (0 to 360)',
                          selectcolor=gui_color.one,
                          variable = main_menu.meridian_toggle_dataline,
                          command = gui_control.Command(self.evt_set_meridian_toggle, parent, 2)
                         )

      # Initialize the checkbutton to convert_to_MV_flg:
      main_menu.var_convert_to_MV = Tkinter.IntVar()
      main_menu.var_convert_to_MV.set(convert_to_MV_flg)
      main_menu.addmenuitem(Pre_name, 'checkbutton', 'Toggle me on/off',
                label = 'Automatic Conversion to MV',
                selectcolor=gui_color.one,
                variable = main_menu.var_convert_to_MV,
                command = gui_control.Command(self.evt_convert_to_MV_toggle, parent)
              )

      main_menu.addmenuitem(Pre_name, 'separator')

      main_menu.addmenuitem(Pre_name, 'command', 'User defined options',
                label = 'Set the Default Template Name...',
                command = gui_control.Command(self.evt_set_default_template, parent)
              )

      main_menu.addmenuitem(Pre_name, 'command', 'User defined options',
                label = 'Set the Default Graphics Method Name...',
                command = gui_control.Command(self.evt_set_default_graphics_method, parent)
              )

      main_menu.addmenuitem(Pre_name, 'command', 'User defined options',
                label = 'Set the Default Colormap Name...',
                command = gui_control.Command(self.evt_set_default_colormap, parent)
              )

      # Color map intensity value
      try: main_menu.color_intensity = vcdat_initial.set.colormap_intensity_setting
      except: main_menu.color_intensity = 0
      main_menu.color_intensity_toggle=[]
      for i in range(2):
         main_menu.color_intensity_toggle.append( Tkinter.IntVar() )
         main_menu.color_intensity_toggle[i].set( 0 )

      main_menu.color_intensity_toggle[main_menu.color_intensity].set( 1 )

      main_menu.addcascademenu(Pre_name, 'Colorintensity', 'VCS Color Intensity',
                label = "Colormap Intensity Value",
                traverseSpec = 'z', tearoff = tear_it
              )
      main_menu.addmenuitem('Colorintensity', 'checkbutton', 'Toggle, Colormap Intensity',
                          label = 'Maximum Intensity is 100',
                          selectcolor=gui_support.gui_color.one,
                          variable = main_menu.color_intensity_toggle[0],
                          command = gui_control.Command(self.evt_color_intensity, parent, 0)
                         )
      main_menu.addmenuitem('Colorintensity', 'checkbutton', 'Toggle, Colormap Intensity',
                          label = 'Maximum Intensity is 255',
                          selectcolor=gui_support.gui_color.one,
                          variable = main_menu.color_intensity_toggle[1],
                          command = gui_control.Command(self.evt_color_intensity, parent, 1)
                         )

      main_menu.addmenuitem(Pre_name, 'separator')

      main_menu.addmenuitem(Pre_name, 'command', 'User defined options',
                label = 'Show Defined Variables Tools',
                command = gui_control.Command(self.evt_show_defined_var_tools, parent)
              )
      main_menu.addmenuitem(Pre_name, 'command', 'User defined options',
                label = 'Show Template and Graphics Method Windows',
                command = gui_control.Command(self.evt_show_template_graphics_method_windows, parent)
              )
      # Initialize the checkbutton to set the Defined Variables single selection mode:
      main_menu.DV_single_selection_mode = Tkinter.IntVar()
      try:
         main_menu.DV_single_selection_mode.set( vcdat_initial.set.DV_single_selection_mode )
      except:
         main_menu.DV_single_selection_mode.set( 0 )
      main_menu.addmenuitem(Pre_name, 'checkbutton', 'Toggle me on/off',
                label = 'Defined Variables Single Selection Mode',
                selectcolor=gui_color.one,
                variable = main_menu.DV_single_selection_mode,
                command = gui_control.Command(self.evt_DV_single_selection_mode, parent)
              )

      main_menu.var_fortran = Tkinter.IntVar()
      # Initialize the checkbutton to fortran_order_flg:
#      main_menu.var_fortran.set(fortran_order_flg)
#      main_menu.addmenuitem(Pre_name, 'checkbutton', 'Toggle me on/off',
#                label = 'Fortran Order',
#                selectcolor=gui_color.one,
#                variable = main_menu.var_fortran,
#                command = gui_control.Command(self.evt_fortran_toggle, parent),
#              )
   
   #----------------------------------------------------
   # Class "Preferences" events
   #----------------------------------------------------
   #
   ####### event to set "Colormap Maximun Intensity Value"
   #
   def evt_color_intensity( self, parent, color_intensity ):
      for i in range(len(parent.menu.main_menu.color_intensity_toggle)):
         parent.menu.main_menu.color_intensity_toggle[i].set( 0 )
      parent.menu.main_menu.color_intensity_toggle[ color_intensity ].set( 1 )
      parent.menu.main_menu.color_intensity = color_intensity
   #
   def evt_change_to_vcdat_lite( self, parent ):
      parent.vcdat_lite = 1
      parent.pane.configurepane('panelDV', min=0, max=0)
      parent.pane.configurepane('panelVI', min=0, max=0)
      parent.panelGC.opt_btn.forget()
      parent.panelGC.define_btn.forget()
      parent.menu.main_menu.forget()
      parent.menu2.main_menu2.pack( side='top', before=parent.pane, fill='x' )
## Commented for version 4.3
##       parent.menu2.main_menu2.vcs_canvas_gui.set(parent.menu.vcs_canvas_gui_settings_flg)
      pos = '%dx%d+%d+%d' % (parent.win_w, parent.win_h*.6, parent.dxwin, parent.dywin)
      parent.geometry(pos)

   #
   ####### event to set "Save Region Selection"
   #
   def evt_mode_of_operation( self, parent, operation_mode ):
      for i in range(len(parent.menu.main_menu.look_and_feel_mode_toggle)):
          parent.menu.main_menu.look_and_feel_mode_toggle[i].set( 0 )
      parent.menu.main_menu.look_and_feel_mode_toggle[operation_mode].set( 1 )
      max_w = parent.winfo_screenwidth()
      max_h = parent.winfo_screenheight()
      try: pane3_size = vcdat_initial.set.pane3_size_position
      except: pane3_size = 270
      try: pane4_size = vcdat_initial.set.pane4_size_position
      except: pane4_size = 295

      if operation_mode == 0:     # Default Mode
         parent.pane.configurepane( 'panelDM', size = pane3_size )     # Reset the Dimension Pane
         parent.pane.configurepane( 'panelDV', size = pane4_size )     # Reset the Data Pane
      elif operation_mode == 1:   # Browse Variable Mode
         parent.pane.configurepane( 'panelDM', size = 1 )              # Move the Dimension Pane Up
         parent.pane.configurepane( 'panelDV', size = 1 )              # Move the Data Pane Up
         parent.show_template_graphics_method = 1
         parent.panelDV.framet.pack_forget( )
         parent.panelDV.frameg.pack_forget( )
         parent.pl.pmain_menu.pack_forget( )
         parent.pl.scl_frame.pack_forget( )
         parent.panelVI.scl1.pack( fill = 'both', expand=1, pady=3 )

      elif operation_mode == 2:   # Manipulate Data Mode
         dm_size = int( max_h * .30)
         dv_size = int( max_h * .50)
         parent.pane.configurepane( 'panelDM', size = dm_size )       # Resize the Dimension Pane
         parent.pane.configurepane( 'panelDV', size = dv_size )       # Resize the Data Pane
         parent.show_template_graphics_method = 1
         parent.panelDV.framet.pack_forget( )
         parent.panelDV.frameg.pack_forget( )
         parent.pl.pmain_menu.pack_forget( )
         parent.pl.scl_frame.pack_forget( )
         parent.panelVI.scl1.pack( fill = 'both', expand=1, pady=3 )
      elif operation_mode == 3:   # Manipulate Graphics Display Mode
         dv_size = int( max_h * .40)
         parent.pane.configurepane( 'panelDM', size = 1 )             # Move the Dimension Pane Up
         parent.pane.configurepane( 'panelDV', size = dv_size )       # Resize the Dimension Pane
         parent.show_template_graphics_method = 0
         parent.panelDV.framet.pack(side='left', before=parent.panelDV.framec, expand=1, fill='both')
         parent.panelDV.frameg.pack(side='left', after=parent.panelDV.framet, fill ='both', expand=1, padx = 5)
         parent.pl.pmain_menu.pack( side='top', fill='both' )
         parent.pl.scl_frame.pack( side='top', fill='both' )
         parent.panelVI.scl1.pack_forget( )


   def setdefaultfont(self,parent,name,dialog):
      parent.vcs[ parent.vcs_id ].setdefaultfont(name)
      parent.default_font = name
      dialog.destroy()
     
   def evt_change_font(self,parent):
      dialog = Pmw.Dialog(parent,
                          title = "Changing VCDAT's Default Font",
                          buttons = ('OK',),
                          )
      f1 = Tkinter.Frame(dialog.interior())
      f1.pack(side='left',expand=1,fill='both')
      f2 = Tkinter.Frame(dialog.interior())
      f2.pack(side='left',expand=1,fill='both')
      fonts = parent.vcs[ parent.vcs_id ].listelements("font")
      nfonts = len(fonts)
      for i in range(nfonts):
         if i%2: # right one
            b = Tkinter.Button(f2,text=fonts[i],command = gui_control.Command(self.setdefaultfont,parent,fonts[i],dialog))
         else:
            b = Tkinter.Button(f1,text=fonts[i],command = gui_control.Command(self.setdefaultfont,parent,fonts[i],dialog))
         b.pack(side='top',fill='x',expand=1)
      dialog.transient( parent ) # Keep widget on top of its parent
      dialog.activate()
##       dialog.destroy()
      
   def evt_add_font(self,parent):
      datatypes = [
         ("Search for Font files", "*.ttf","*.TTF",),
         ("All files", "*")
         ]
      dialog_icon = tkFileDialog.Open(master=parent,
                                      filetypes=datatypes, title = 'File Select')
      font=dialog_icon.show(initialdir=os.getcwd())
      try:
         parent.vcs[ parent.vcs_id ].addfont(font)
      except:
         pass
      


   #
   ####### event to set "Save Region Selection"
   #
   def evt_set_region( self, parent, region_type ):
      for i in range(len(parent.menu.main_menu.region_toggle)):
          parent.menu.main_menu.region_toggle[i].set( 0 )
      parent.menu.main_menu.region_toggle[region_type].set( 1 )
      parent.menu.save_region = region_type
      try:
         if parent.panelDM.var3 is not None:
            gui_select_variable.evt_enter_variable( parent, None )
      except:
         pass
   #
   ####### event to set "Squeeze Dimension Selection"
   #
   def evt_squeeze_dim_toggle( self, parent ):
      if parent.menu.squeeze_dim_flg == 1:
         parent.menu.squeeze_dim_flg = 0
      else:
         parent.menu.squeeze_dim_flg = 1
   #
   ####### event to set "View Axes Selection"
   #
   def evt_view_axes_toggle( self, parent ):
      if parent.menu.view_axes_flg == 1:
         parent.menu.view_axes_flg = 0
      else:
         parent.menu.view_axes_flg = 1
   #
   ####### event to set "View Bounds and Weights Selection"
   #
   def evt_view_bounds_weights_toggle( self, parent ):
      if parent.menu.view_bounds_weights_flg == 1:
         parent.menu.view_bounds_weights_flg = 0
      else:
         parent.menu.view_bounds_weights_flg = 1
   #
   ####### event to set "Meridian Selection"
   #
   def evt_set_meridian_toggle( self, parent, number ):
      if number == 0:
         parent.menu.main_menu.meridian_toggle_none.set( 1 )
         parent.menu.main_menu.meridian_toggle_greenwich.set( 0 )
         parent.menu.main_menu.meridian_toggle_dataline.set( 0 )
      elif number == 1:
         parent.menu.main_menu.meridian_toggle_none.set( 0 )
         parent.menu.main_menu.meridian_toggle_greenwich.set( 1 )
         parent.menu.main_menu.meridian_toggle_dataline.set( 0 )
      elif number == 2:
         parent.menu.main_menu.meridian_toggle_none.set( 0 )
         parent.menu.main_menu.meridian_toggle_greenwich.set( 0 )
         parent.menu.main_menu.meridian_toggle_dataline.set( 1 )

      # Set the meridian flag number
      parent.menu.meridian_flg = number
      try:
         if parent.panelDM.var3 is not None:
            gui_select_variable.evt_enter_variable( parent, None )
      except:
         pass
   #
   ####### event to set "Automatic Conversion of numpy and numpy.ma arrays to MV"
   #
   def evt_convert_to_MV_toggle( self, parent ):
      if parent.menu.convert_to_MV_flg == 1:
         parent.menu.convert_to_MV_flg = 0
      else:
         parent.menu.convert_to_MV_flg = 1
         # convert numpy and numpy.ma variables to MV and
	 # show them in the defined variable window
         gui_defined_variables.selected_updated( )
   #
   ####### event to set "Fortran Order"
   #
   def evt_fortran_toggle( self, parent ):
      if parent.menu.fortran_order_flg == 1:
         parent.menu.fortran_order_flg = 0
      else:
         parent.menu.fortran_order_flg = 1
   #
   ####### event to set "Retain User Defined Settings"
   #   THIS IS NO LONGER IN USE!!!
#   def evt_retain_user_toggle( self, parent ):
#      if parent.menu.retain_user_settings_flg == 1:
#         parent.menu.retain_user_settings_flg = 0
#      else:
#         parent.menu.retain_user_settings_flg = 1
   #
   ####### event to set "Popup Window Defined Settings"
   #
   def evt_popup_window_toggle( self, parent ):
      if parent.menu.popup_window_settings_flg == 1:
         parent.menu.popup_window_settings_flg = 0
      else:
         parent.menu.popup_window_settings_flg = 1
      # Mac OS X 10.4 cannot support this feature
      if ((sys.platform == 'darwin') and (gui_control.do_aqua == 1)): parent.menu.popup_window_settings_flg = 0
   #
   ####### event to "Reset the GUI's Swap Space Size"
   #
   def evt_set_default_template( self, parent ):
      gui_output.create(parent, 'tt',"Set the Template Default Name",'Enter Your Default Template Name:', parent.template_name)
   #
   ####### event to "Reset the GUI's Swap Space Size"
   #
   def evt_set_default_graphics_method( self, parent ):
      gui_output.create(parent, 'gm',"Set the Graphics Method Default Name",'Enter Your Default Graphics Method Name:', parent.graphics_method_name)
   #
   ####### event to "Reset the GUI's Swap Space Size"
   #
   def evt_set_default_colormap( self, parent ):
      # Popup use dialog widget
      c_names = parent.vcs[ parent.vcs_id ].listelements('colormap')
      c_names.sort()
      cname = parent.vcs[ parent.vcs_id ].getcolormapname()

      # Create the dialog.
      self.cdialog = Pmw.ComboBoxDialog( master=parent,
            title = 'Set the Colormap Default Name',
            buttons = ('OK', 'Dismiss'),
            defaultbutton = 'OK',
            combobox_labelpos = 'n',
            entry_background = 'white', 
            entry_foreground = 'black', 
            label_text = 'Enter or Select VCS Colormap:',
            scrolledlist_items = c_names,
            command = gui_control.Command(self.execute_c_selection, parent.vcs[ parent.vcs_id ], parent) )
      self.cdialog.setentry( cname )

      self.cdialog.transient( parent ) # draw widget on top of its parent

      # Position dialog popup
      parent_geom = parent.geometry()
      geom = string.split(parent_geom, '+')
      d1 = string.atoi( geom[1] )
      d2 = string.atoi( geom[2] )
      self.cdialog.geometry( "+%d+%d" % (d1, d2) )

   # Set the colormap selection
   def execute_c_selection ( self, vcs, parent, result ):
      if (result == 'OK') or (result == 'Apply'):
         new_name = self.cdialog.component('entry').get()
         if new_name not in parent.vcs[ parent.vcs_id ].listelements( 'colormap' ):
            gui_message.error( 'Could not find the colormap name ( %s ) in the colormap list. Please check the colormap list to make sure it exist.' % new_name )
            return
         parent.colormap_name = new_name
         parent.vcs[ parent.vcs_id ].setcolormap( parent.colormap_name )

      self.cdialog.destroy()

#      dialog = Pmw.PromptDialog(parent, title='Set the Colormap Default Name',
#                                label_text='Enter Your Default Colormap Name:',
#                                entryfield_labelpos='nw',
#                                entry_width = 50, 
#                                entry_background = 'white', 
#                                entry_foreground = 'black', 
#                                defaultbutton=0, buttons=('OK', 'Dismiss'))
#   
#      # Position dialog popup on top of the main GUI and wait for results
#      parent_geom = parent.geometry()
#      geom = string.split(parent_geom, '+')
#      d1 = string.atoi( geom[1] ); d2 = string.atoi( geom[2] )
#
#      result = dialog.activate(globalMode = 'nograb', geometry= "+%d+%d" % (d1, d2) )
#
#      if result == 'OK':
#         new_name = dialog.component('entry').get()
#         if new_name not in parent.vcs[ parent.vcs_id ].listelements( 'colormap' ):
#            gui_message.error( 'Could not find the colormap name ( %s ) in the colormap list. Please check the colormap list to make sure it exist.' % new_name )
#            return
#         parent.colormap_name = new_name
#         parent.vcs[ parent.vcs_id ].setcolormap( parent.colormap_name )
#
#      dialog.destroy()

   #
   ####### event to "Reset the GUI's Swap Space Size"
   #
   def evt_reset_GUI_swap_space( self, parent ):
      gui_output.create(parent, 'ss',"Reset the GUI's Swap Space",'Enter New Swap Space Size in MBytes:', parent.swap_size/1000000.)
   #
   ####### event to "Reset GUI to its Initial State"
   #
   def evt_reset_GUI_state( self, parent ):
      gui_reset.to_initial_state( parent, 1)
   #
   ####### event to "Save GUI Settings"
   #
   def evt_save_GUI_settings( self, parent ):
      gui_saved_settings.create( parent )
   #
   ####### event to "Save VCS Settings"
   #
##    def evt_save_VCS_settings( self, parent ):
##       msg = ''
##       while (msg == ''):
##          # Remove all temporary templates
##          template_list = parent.vcs[ parent.vcs_id ].listelements('template')
##          for x in template_list:
##              if ((x[:5] == '_temp') or (x[:2] == '__')):
##                 parent.vcs[ parent.vcs_id ].removeP( x )
      
##          msg = parent.vcs[ parent.vcs_id ].saveinitialfile()
   #
   ####### event to set "Show Popup Window for Existing VCDAT"
   #
   def evt_exit_popup_toggle( self, parent ):
      if parent.menu.show_exit_popup_flg == 1:
         parent.menu.show_exit_popup_flg = 0
      else:
         parent.menu.show_exit_popup_flg = 1
   #
   ####### event to "Save GUI's Initial State"
   #
   def evt_save_GUI_state( self, parent ):
      result = askyesno('Save GUI State?', 'Save the state of the GUI permanently?')
      if not result: return
      msg = parent.vcs[ parent.vcs_id ].saveinitialfile()
      gui_saved_settings.create( parent )
      s = Pmw.TextDialog(parent, title='Session saved.') 
      s.insert('end', msg)
   #
   #
   def evt_show_defined_var_tools( self, parent ):
      parent.show_defined_variables_tools = 1
      parent.panelDV.evt_show_defined_variables_tools(parent, None)
   #
   #
   def evt_show_template_graphics_method_windows( self, parent ):
      parent.show_defined_variables_tools = 1
      parent.panelDV.evt_show_defined_variables_tools(parent, None)
      parent.show_template_graphics_method = 1
      parent.panelDV.evt_show_template_graphics_method(parent, None)

   #
   def evt_DV_single_selection_mode( self, parent ):
      # Update the selection numbering scheme to nothing selected
      for x in parent.panelDV.number_lst1.keys(): 
         parent.panelDV.number_lst1[ x ] = gui_control.dvholder + x

      # Unselect all that are selected
      gui_defined_variables.update_defined_variable_list( parent )
      dvlt = parent.panelDV.scl1.get()
      for i in range( len( dvlt ) ):
          if dvlt[ i ][0:gui_control.dvsize] != gui_control.dvholder:
             parent.panelDV.scl1.select_clear( i )

      # Change the Selection Mode to EXTENDED
      if parent.menu.main_menu.DV_single_selection_mode.get() == 1:
         parent.panelDV.scl1.component('listbox').configure( selectmode = Tkinter.EXTENDED )
      else:
         parent.panelDV.scl1.component('listbox').configure( selectmode = Tkinter.MULTIPLE )



#----------------------------------------------------------------------------------------
# Create the Tools menu and its menu items
#----------------------------------------------------------------------------------------
#
class create_tools_menu:
   def __init__( self, main_menu, parent, record_commands_flg, tear_it ):
      T_name = 'Tools'
      if ((sys.platform == 'darwin') and (gui_control.do_aqua == 1)): T_name = 'Tools  '
      main_menu.addmenu(T_name, 'Helpful Operation Tools', tearoff = tear_it)
      main_menu.addmenuitem(T_name, 'command', 'Tool options',
                            label = 'Edit New File...',
                            command = gui_control.Command(self.evt_popup_new_idle_editor_window, parent)
                           )
      main_menu.addmenuitem(T_name, 'command', 'Tool options',
                            label = 'Edit Old File...',
                            command = gui_control.Command(self.evt_popup_idle_editor_window, parent)
                           )
      main_menu.addmenuitem(T_name, 'command', 'Tool options',
                            label = 'Command Line Window...',
                            command = gui_control.Command(self.evt_popup_idle_command_window, parent)
                           )
### No longer needed, since IDLE has it own "Configuration" GUI, which changes the fonts.
#      main_menu.addmenuitem(T_name, 'command', 'Tool options',
#                            label = 'Change Edit and Command Line Font...',
#                            command = gui_control.Command(self.evt_popup_change_idle_font_pupop, parent)
#                           )
###
#      main_menu.addmenuitem(T_name, 'command', 'Tool options', label = 'Emacs Editor')
      main_menu.addmenuitem(T_name, 'separator')
      main_menu.addmenuitem(T_name, 'command', 'Tool options',
                            label = 'Add User New Menu...',
                            command = gui_control.Command(gui_extend_menus.create, parent)
                           )
      main_menu.addmenuitem(T_name, 'separator')
      main_menu.var_record = Tkinter.IntVar()
      # Initialise the checkbutton to record_commands_flg:
      main_menu.var_record.set(record_commands_flg)
      main_menu.addmenuitem(T_name, 'checkbutton', 'Toggle me on/off',
                label = 'Record Commands',
                selectcolor=gui_color.one,
                variable = main_menu.var_record,
                command = gui_control.Command(self.evt_record_toggle, parent)
              )
      main_menu.addmenuitem(T_name, 'command', 
                            'Tool options',
                            label = "View Teaching Commands...",
##                             state='disabled',
                            command = gui_control.Command(self.evt_view_record_b, parent)
                            )
      main_menu.addmenuitem(T_name, 'command', 
                            'Tool options',
                            label = 'View Recorded Commands...',
##                             state='disabled',
                            command = gui_control.Command(self.evt_view_record, parent)
                            )
      main_menu.addmenuitem(T_name, 'command', 
                'Tool options',
                 label = 'View GUI Initial State...',
                command = gui_control.Command(self.evt_view_GUI_state, parent)
              )
   #
   ####### event to set record flag
   #
   def evt_record_toggle( self, parent ):
      if parent.menu.record_commands_flg == 1:
         parent.menu.record_commands_flg = 0
      else:
         parent.menu.record_commands_flg = 1
   #
   ####### event to view beginner's recorded commands
   #
   def evt_view_record_b( self, parent ):
      from idlelib import EditorWindow
      # Position dialog popup
      parent_geom = parent.geometry()
      geom = string.split(parent_geom, '+')
      d1 = string.atoi( geom[1] )
      d2 = string.atoi( geom[2] )
      #
      sys.ps1 = "CDAT> " # String representation of the command line prompt (i.e., ">>>" or "CDAT>")
      edit = EditorWindow.EditorWindow(flist=parent.flist, filename=parent.fp_filename_b, root=parent)

### Must redirect the closing of the window. If this is not done, then closing IDLE will also 
### close VCDAT.
      if parent.firstidle:
         edit.top.protocol("WM_DELETE_WINDOW", gui_control.Command(self.withdraw_idle, edit, parent.fp_filename_b))
         parent.save_idle = edit
      parent.firstidle = True

      edit.menudict['file'].delete('Close')
      edit.menudict['file'].delete('Exit')
      edit.menudict['file'].add_command(label='Close', command=gui_control.Command(self.withdraw_idle,edit,parent.fp_filename_b), underline=0 )

      if parent.menu.popup_window_settings_flg == 1:
         edit.top.transient( parent ) # Keep widget on top of its parent
      if parent.menu.popup_window_settings_flg == 1:
         edit.top.transient( parent ) # Keep widget on top of its parent
      edit.top.geometry( "%dx%d+%d+%d" % (parent.win_w, (parent.win_h*0.5), d1, d2) )
   #
   ####### event to view recorded commands
   #
   def evt_view_record( self, parent ):
      from idlelib import EditorWindow
      # Position dialog popup
      parent_geom = parent.geometry()
      geom = string.split(parent_geom, '+')
      d1 = string.atoi( geom[1] )
      d2 = string.atoi( geom[2] )
      #
      sys.ps1 = "CDAT> " # String representation of the command line prompt (i.e., ">>>" or "CDAT>")
      edit = EditorWindow.EditorWindow(flist=parent.flist, filename=parent.fp_filename, root=parent)

### Must redirect the closing of the window. If this is not done, then closing IDLE will also 
### close VCDAT.
      if parent.firstidle:
         edit.top.protocol("WM_DELETE_WINDOW", gui_control.Command(self.withdraw_idle, edit, parent.fp_filename))
         parent.save_idle = edit
      parent.firstidle = True

      edit.menudict['file'].delete('Close')
      edit.menudict['file'].delete('Exit')
      edit.menudict['file'].add_command(label='Close', command=gui_control.Command(self.withdraw_idle,edit, parent.fp_filename), underline=0 )

      if parent.menu.popup_window_settings_flg == 1:
         edit.top.transient( parent ) # Keep widget on top of its parent
      if parent.menu.popup_window_settings_flg == 1:
         edit.top.transient( parent ) # Keep widget on top of its parent
      edit.top.geometry( "%dx%d+%d+%d" % (parent.win_w, (parent.win_h*0.5), d1, d2) )
   #
   ####### event to "view GUI State"
   #
   def evt_view_GUI_state( self, parent ):
      from idlelib import EditorWindow
      try:
         fn = '%s/PCMDI_GRAPHICS' % os.environ['HOME']
      except:
         gui_message.error( 'Could not find the PCMDI_GRAPHICS directory.' )
         return

      file_name = fn + '/vcdat_initial.py'
      # Position dialog popup
      parent_geom = parent.geometry()
      geom = string.split(parent_geom, '+')
      d1 = string.atoi( geom[1] )
      d2 = string.atoi( geom[2] )
      #
      sys.ps1 = "CDAT> " # String representation of the command line prompt (i.e., ">>>" or "CDAT>")
      edit = EditorWindow.EditorWindow(flist=parent.flist, filename=file_name, root=parent)

### Must redirect the closing of the window. If this is not done, then closing IDLE will also 
### close VCDAT.
      if parent.firstidle:
         edit.top.protocol("WM_DELETE_WINDOW", gui_control.Command(self.withdraw_idle, edit, file_name))
         parent.save_idle = edit
      parent.firstidle = True

      edit.menudict['file'].delete('Close')
      edit.menudict['file'].delete('Exit')
      edit.menudict['file'].add_command(label='Close', command=gui_control.Command(self.withdraw_idle,edit, file_name), underline=0 )

      if parent.menu.popup_window_settings_flg == 1:
         edit.top.transient( parent ) # Keep widget on top of its parent
      if parent.menu.popup_window_settings_flg == 1:
         edit.top.transient( parent ) # Keep widget on top of its parent
      edit.top.geometry( "%dx%d+%d+%d" % (parent.win_w, (parent.win_h*0.5), d1, d2) )

   #----------------------------------------------------
   # Class "Tools" events
   #----------------------------------------------------
   ####### event to popup the idle editor window for old file
   #
   def evt_popup_new_idle_editor_window( self, parent ):
      from idlelib import EditorWindow

      # Position dialog popup
      parent_geom = parent.geometry()
      geom = string.split(parent_geom, '+')
      d1 = string.atoi( geom[1] )
      d2 = string.atoi( geom[2] )
      #
      sys.ps1 = "CDAT> " # String representation of the command line prompt (i.e., ">>>" or "CDAT>")
      edit = EditorWindow.EditorWindow(flist=parent.flist, root=parent)

### Must redirect the closing of the window. If this is not done, then closing IDLE will also 
### close VCDAT.
      if parent.firstidle:
         edit.top.protocol("WM_DELETE_WINDOW", gui_control.Command(self.withdraw_idle, edit, None))
         parent.save_idle = edit
      parent.firstidle = True

      edit.menudict['file'].delete('Close')
      edit.menudict['file'].delete('Exit')
      edit.menudict['file'].add_command(label='Close', command=gui_control.Command(self.withdraw_idle,edit, None), underline=0 )

      if parent.menu.popup_window_settings_flg == 1:
         edit.top.transient( parent ) # Keep widget on top of its parent
      if parent.menu.popup_window_settings_flg == 1:
         edit.top.transient( parent ) # Keep widget on top of its parent
      edit.top.geometry( "%dx%d+%d+%d" % (parent.win_w, (parent.win_h*0.5), d1, d2) )
   ####### event to popup the idle editor window for old file
   #
   def evt_popup_idle_editor_window( self, parent ):
      from idlelib import EditorWindow
      dialog = tkFileDialog.Open(filetypes=gui_control.filetypes,title = 'File Select')
      filename=dialog.show(initialdir=os.getcwd())
      if len(filename) == 0: return

      # Position dialog popup
      parent_geom = parent.geometry()
      geom = string.split(parent_geom, '+')
      d1 = string.atoi( geom[1] )
      d2 = string.atoi( geom[2] )
      #
      sys.ps1 = "CDAT> " # String representation of the command line prompt (i.e., ">>>" or "CDAT>")
      edit = EditorWindow.EditorWindow(flist=parent.flist, filename=filename, root=parent)

### Must redirect the closing of the window. If this is not done, then closing IDLE will also 
### close VCDAT.
      if parent.firstidle:
         edit.top.protocol("WM_DELETE_WINDOW", gui_control.Command(self.withdraw_idle, edit, filename))
         parent.save_idle = edit
      parent.firstidle = True

      edit.menudict['file'].delete('Close')
      edit.menudict['file'].delete('Exit')
      edit.menudict['file'].add_command(label='Close', command=gui_control.Command(self.withdraw_idle,edit, filename), underline=0 )

      if parent.menu.popup_window_settings_flg == 1:
         edit.top.transient( parent ) # Keep widget on top of its parent
      edit.top.geometry( "%dx%d+%d+%d" % (parent.win_w, (parent.win_h*0.5), d1, d2) )
   #
   ####### event to popup the idle command window
   #
   def evt_popup_idle_command_window( self, parent ):
      from idlelib import PyShell, idlever

      # Call idle command (i.e., PyShell) window and set up the information text for the user
      PyShell.use_subprocess = False ## Fix suggested by Guido van Rossum
      shell = PyShell.PyShell(parent.flist)
      shell.shell_title = "VCDAT's Command Line Window"
      shell.text.bind("<Return>", gui_control.Command(self.update_defined_for_user,shell,parent))
      shell.resetoutput()
      shell.write("Python %s on %s\n%s\nIDLE %s -- press F1 for help\n\nVCS: To send command directly to the VCS Canvas use the VCDAT commands: vcs_hook1, for VCS Canvas 1; vcs_hook2, for VCS Canvas 2; vcs_hook3, for VCS Canvas 3; and vcs_hook4, for VCS Canvas 4'.\n\nFor example: vcs_hook1.show('template')\n\n" % (sys.version, sys.platform, shell.COPYRIGHT, idlever.IDLE_VERSION))
      ##########################################################
      # This no longer works for Python 2.5... Note this and 
      # keep watch.
      # try:
      #     sys.ps1
      # except AttributeError:
      #     sys.ps1 = "CDAT> "
      ##########################################################
      sys.ps1 = "CDAT> " # String representation of the command line prompt (i.e., ">>>" or "CDAT>")
      shell.showprompt()

### Must redirect the closing of the window. If this is not done, then closing IDLE will also 
### close VCDAT.
      if parent.firstidle:
         shell.top.protocol("WM_DELETE_WINDOW", gui_control.Command(self.withdraw_idle, shell, None))
      parent.firstidle = True

      shell.menudict['file'].delete('Close')
      shell.menudict['file'].delete('Exit')
      shell.menudict['file'].add_command(label='Close', command=gui_control.Command(self.withdraw_idle,shell, None), underline=0 )

      # Position dialog popup
      parent_geom = parent.geometry()
      geom = string.split(parent_geom, '+')
      d1 = string.atoi( geom[1] )
      d2 = string.atoi( geom[2] )
      #
      if parent.menu.popup_window_settings_flg == 1:
         shell.top.transient( parent ) # keep widget on top of its parent
      shell.top.geometry( "%dx%d+%d+%d" % (parent.win_w, (parent.win_h*0.42), d1, d2) )

### Withdraw the IDLE window.
   def withdraw_idle(self, shell,filename):
      shell.top.withdraw()
      if filename is not None:
         try:
              edit = shell.flist.dict[filename]
              del shell.flist.dict[filename]
         except:
              pass
         if shell.flist.dict == {}:
            for x in shell.flist.inversedict.keys():
               del shell.flist.inversedict[ x ]
      return "break"

   def update_defined_for_user(self, shell, parent, event ):

       # Record the command in the teaching and recording script commands
       txt_output = shell.text.get('1.0', 'end')
       start = string.rindex(txt_output, 'CDAT>') + 5
       command = string.strip( txt_output[start:] )
       gui_control.record_command(parent,"\n# Recorded command from the Command Line Window", 1 )
       gui_control.record_command(parent, "%s" % command, 1 )

       shell.enter_callback(event)
       gui_defined_variables.update_defined( parent )

       return "break"
   #
   ####### event to change idle's font
   #
   def evt_popup_change_idle_font_pupop( self, parent ):
      gui_set_idle_font.create( parent )

#----------------------------------------------------------------------------------------
# Create the PCMDI Tools menu and its menu items
#----------------------------------------------------------------------------------------
class create_pcmdi_tools_menu:
   def __init__( self, main_menu, parent, tear_it ):
      PT_name = 'PCMDITools'
      if ((sys.platform == 'darwin') and (gui_control.do_aqua == 1)): PT_name = 'PCMDITools  '
      main_menu.addmenu(PT_name, 'PCMDI Tools', tearoff = tear_it)
      #
      # Create the cascade "PCMDI Tools" menu and its items
      main_menu.addcascademenu(PT_name, 'TimeTools',
                              'PCMDI Time Dimension Extract Tools',
                              label = 'Time Tools',
                              traverseSpec = 'z', tearoff = tear_it
                             )
      #
      # Create the cascade "Time Extract Tools" menu and its items
      main_menu.addcascademenu('TimeTools', 'BoundsSet',
                              'Time Dimension Extract Tools',
                              label = 'Bounds Set',
                              traverseSpec = 'z', tearoff = tear_it
                             )
      main_menu.addmenuitem('BoundsSet', 'command', 'Set Bounds For Yearly Data',
                       label = 'Set Bounds for Yearly Data',
                       command = gui_control.Command(self.yearlyboundsset, parent)
                      )
      main_menu.addmenuitem('BoundsSet', 'command', 'Set Bounds For Monthly Data',
                       label = 'Set Bounds for Monthly Data',
                       command = gui_control.Command(self.monthlyboundsset, parent)
                      )
      main_menu.addmenuitem('BoundsSet', 'command', 'Set Bounds For Daily Data',
                       label = 'Set Bounds for Daily Data',
                       command = gui_control.Command(self.dailyboundsset, parent,1)
                      )
      main_menu.addmenuitem('BoundsSet', 'command', 'Set Bounds For Twice-Daily Data',
                       label = 'Set Bounds for Twice-daily Data',
                       command = gui_control.Command(self.dailyboundsset, parent,2)
                      )
      main_menu.addmenuitem('BoundsSet', 'command', 'Set Bounds For 6-Hourly Data',
                       label = 'Set Bounds for 6-Hourly Data',
                       command = gui_control.Command(self.dailyboundsset, parent,4)
                      )
      main_menu.addmenuitem('BoundsSet', 'command', 'Set Bounds For Hourly Data',
                       label = 'Set Bounds for Hourly Data',
                       command = gui_control.Command(self.dailyboundsset, parent,24)
                      )
      main_menu.addmenuitem('BoundsSet', 'command', 'Set Bounds for X Hourly Data',
                       label = 'Set Bounds for X Hourly Data',
                       command = gui_control.Command(gui_bounds_question.create, parent, 'Xhourly')
                      )
      main_menu.addmenuitem('TimeTools', 'separator')
      main_menu.addcascademenu('TimeTools', 'Extract',
                              'Time Dimension Extract Tools',
                              label = 'Extract',
                              traverseSpec = 'z', tearoff = tear_it
                             )
      main_menu.addmenuitem('Extract', 'command', 'Annual Mean Extract',
                       label = 'Annual Means',
                       command = gui_control.Command(self.evt_extract, parent, 'Annual' )
                      )
      main_menu.addmenuitem('Extract', 'command', 'Seasonal Means Extract',
                       label = 'Seasonal Means',
                       command = gui_control.Command(self.evt_extract, parent, 'Seasonal' )
                      )
      main_menu.addmenuitem('Extract', 'separator')
      main_menu.addmenuitem('Extract', 'command', 'DJF Extract',
                       label = 'DJF',
                       command = gui_control.Command(self.evt_extract, parent, 'DJF' )
                      )
      main_menu.addmenuitem('Extract', 'command', 'MAM Extract',
                       label = 'MAM',
                       command = gui_control.Command(self.evt_extract, parent, 'MAM' )
                      )
      main_menu.addmenuitem('Extract', 'command', 'JJA Extract',
                       label = 'JJA',
                       command = gui_control.Command(self.evt_extract, parent, 'JJA' )
                      )
      main_menu.addmenuitem('Extract', 'command', 'SON Extract',
                       label = 'SON',
                       command = gui_control.Command(self.evt_extract, parent, 'SON' )
                      )
      main_menu.addmenuitem('Extract', 'separator')
      main_menu.addmenuitem('Extract', 'command', 'Monthly Means Extract',
                       label = 'Monthly Means',
                       command = gui_control.Command(self.evt_extract, parent, 'Monthly' )
                      )
      main_menu.addmenuitem('Extract', 'command', 'JAN Extract',
                       label = 'JAN',
                       command = gui_control.Command(self.evt_extract, parent, 'JAN' )
                      )
      main_menu.addmenuitem('Extract', 'command', 'FEB Extract',
                       label = 'FEB',
                       command = gui_control.Command(self.evt_extract, parent, 'FEB' )
                      )
      main_menu.addmenuitem('Extract', 'command', 'MAR Extract',
                       label = 'MAR',
                       command = gui_control.Command(self.evt_extract, parent, 'MAR' )
                      )
      main_menu.addmenuitem('Extract', 'command', 'APR Extract',
                       label = 'APR',
                       command = gui_control.Command(self.evt_extract, parent, 'APR' )
                      )
      main_menu.addmenuitem('Extract', 'command', 'MAY Extract',
                       label = 'MAY',
                       command = gui_control.Command(self.evt_extract, parent, 'MAY' )
                      )
      main_menu.addmenuitem('Extract', 'command', 'JUN Extract',
                       label = 'JUN',
                       command = gui_control.Command(self.evt_extract, parent, 'JUN' )
                      )
      main_menu.addmenuitem('Extract', 'command', 'JUL Extract',
                       label = 'JUL',
                       command = gui_control.Command(self.evt_extract, parent, 'JUL' )
                      )
      main_menu.addmenuitem('Extract', 'command', 'AUG Extract',
                       label = 'AUG',
                       command = gui_control.Command(self.evt_extract, parent, 'AUG' )
                      )
      main_menu.addmenuitem('Extract', 'command', 'SEP Extract',
                       label = 'SEP',
                       command = gui_control.Command(self.evt_extract, parent, 'SEP' )
                      )
      main_menu.addmenuitem('Extract', 'command', 'OCT Extract',
                       label = 'OCT',
                       command = gui_control.Command(self.evt_extract, parent, 'OCT' )
                      )
      main_menu.addmenuitem('Extract', 'command', 'NOV Extract',
                       label = 'NOV',
                       command = gui_control.Command(self.evt_extract, parent, 'NOV' )
                      )
      main_menu.addmenuitem('Extract', 'command', 'DEC Extract',
                       label = 'DEC',
                       command = gui_control.Command(self.evt_extract, parent, 'DEC' )
                      )

      #
      # Create the cascade "Time Climatology Tools" menu and its items
      main_menu.addcascademenu('TimeTools', 'Climatology',
                              'Time Dimension Climatology Tools',
                              label = 'Climatology',
                              traverseSpec = 'z', tearoff = tear_it
                             )
      main_menu.addmenuitem('Climatology', 'command', 'Annual',
                       label = 'Annual',
                       command = gui_control.Command(self.evt_climatology, parent, 'Annual' )
                      )
      main_menu.addmenuitem('Climatology', 'separator')
      main_menu.addmenuitem('Climatology', 'command', 'Seasonal',
                       label = 'Seasonal',
                       command = gui_control.Command(self.evt_climatology, parent, 'Seasonal' )
                      )
      main_menu.addmenuitem('Climatology', 'command', 'DJF Climatology',
                       label = 'DJF',
                       command = gui_control.Command(self.evt_climatology, parent, 'DJF' )
                      )
      main_menu.addmenuitem('Climatology', 'command', 'MAM Climatology',
                       label = 'MAM',
                       command = gui_control.Command(self.evt_climatology, parent, 'MAM' )
                      )
      main_menu.addmenuitem('Climatology', 'command', 'JJA Climatology',
                       label = 'JJA',
                       command = gui_control.Command(self.evt_climatology, parent, 'JJA' )
                      )
      main_menu.addmenuitem('Climatology', 'command', 'SON Climatology',
                       label = 'SON',
                       command = gui_control.Command(self.evt_climatology, parent, 'SON' )
                      )
      main_menu.addmenuitem('Climatology', 'separator')
      main_menu.addmenuitem('Climatology', 'command', 'Monthly',
                       label = 'Monthly',
                       command = gui_control.Command(self.evt_climatology, parent, 'Monthly' ))
      main_menu.addmenuitem('Climatology', 'command', 'JAN Climatology',
                       label = 'JAN',
                       command = gui_control.Command(self.evt_climatology, parent, 'JAN' )
                      )
      main_menu.addmenuitem('Climatology', 'command', 'FEB Climatology',
                       label = 'FEB',
                       command = gui_control.Command(self.evt_climatology, parent, 'FEB' )
                      )
      main_menu.addmenuitem('Climatology', 'command', 'MAR Climatology',
                       label = 'MAR',
                       command = gui_control.Command(self.evt_climatology, parent, 'MAR' )
                      )
      main_menu.addmenuitem('Climatology', 'command', 'APR Climatology',
                       label = 'APR',
                       command = gui_control.Command(self.evt_climatology, parent, 'APR' )
                      )
      main_menu.addmenuitem('Climatology', 'command', 'MAY Climatology',
                       label = 'MAY',
                       command = gui_control.Command(self.evt_climatology, parent, 'MAY' )
                      )
      main_menu.addmenuitem('Climatology', 'command', 'JUN Climatology',
                       label = 'JUN',
                       command = gui_control.Command(self.evt_climatology, parent, 'JUN' )
                      )
      main_menu.addmenuitem('Climatology', 'command', 'JUL Climatology',
                       label = 'JUL',
                       command = gui_control.Command(self.evt_climatology, parent, 'JUL' )
                      )
      main_menu.addmenuitem('Climatology', 'command', 'AUG Climatology',
                       label = 'AUG',
                       command = gui_control.Command(self.evt_climatology, parent, 'AUG' )
                      )
      main_menu.addmenuitem('Climatology', 'command', 'SEP Climatology',
                       label = 'SEP',
                       command = gui_control.Command(self.evt_climatology, parent, 'SEP' )
                      )
      main_menu.addmenuitem('Climatology', 'command', 'OCT Climatology',
                       label = 'OCT',
                       command = gui_control.Command(self.evt_climatology, parent, 'OCT' )
                      )
      main_menu.addmenuitem('Climatology', 'command', 'NOV Climatology',
                       label = 'NOV',
                       command = gui_control.Command(self.evt_climatology, parent, 'NOV' )
                      )
      main_menu.addmenuitem('Climatology', 'command', 'DEC Climatology',
                       label = 'DEC',
                       command = gui_control.Command(self.evt_climatology, parent, 'DEC' )
                      )

      #
      # Create the cascade "Time Departures Tools" menu and its items
      main_menu.addcascademenu('TimeTools', 'Departures',
                              'Time Dimension Departures Tools',
                              label = 'Departures',
                              traverseSpec = 'z', tearoff = tear_it
                             )
      main_menu.addmenuitem('Departures', 'command', 'Annual Means Departures',
                       label = 'Annual Means',
                       command = gui_control.Command(self.evt_departures, parent, 'Annual' )
                      )
      main_menu.addmenuitem('Departures', 'separator')
      main_menu.addmenuitem('Departures', 'command', 'Seasonal Means Departures',
                       label = 'Seasonal Means',
                       command = gui_control.Command(self.evt_departures, parent, 'Seasonal' )
                      )
      main_menu.addmenuitem('Departures', 'command', 'DJF Departures',
                       label = 'DJF',
                       command = gui_control.Command(self.evt_departures, parent, 'DJF' )
                      )
      main_menu.addmenuitem('Departures', 'command', 'MAM Departures',
                       label = 'MAM',
                       command = gui_control.Command(self.evt_departures, parent, 'MAM' )
                      )
      main_menu.addmenuitem('Departures', 'command', 'JJA Departures',
                       label = 'JJA',
                       command = gui_control.Command(self.evt_departures, parent, 'JJA' )
                      )
      main_menu.addmenuitem('Departures', 'command', 'SON Departures',
                       label = 'SON',
                       command = gui_control.Command(self.evt_departures, parent, 'SON' )
                      )
      main_menu.addmenuitem('Departures', 'separator')
      main_menu.addmenuitem('Departures', 'command', 'Monthly Means Departures',
                       label = 'Monthly Means',
                       command = gui_control.Command(self.evt_departures, parent, 'Monthly' )
                      )
      main_menu.addmenuitem('Departures', 'command', 'JAN Departures',
                       label = 'JAN',
                       command = gui_control.Command(self.evt_departures, parent, 'JAN' )
                      )
      main_menu.addmenuitem('Departures', 'command', 'FEB Departures',
                       label = 'FEB',
                       command = gui_control.Command(self.evt_departures, parent, 'FEB' )
                      )
      main_menu.addmenuitem('Departures', 'command', 'MAR Departures',
                       label = 'MAR',
                       command = gui_control.Command(self.evt_departures, parent, 'MAR' )
                      )
      main_menu.addmenuitem('Departures', 'command', 'APR Departures',
                       label = 'APR',
                       command = gui_control.Command(self.evt_departures, parent, 'APR' )
                      )
      main_menu.addmenuitem('Departures', 'command', 'MAY Departures',
                       label = 'MAY',
                       command = gui_control.Command(self.evt_departures, parent, 'MAY' )
                      )
      main_menu.addmenuitem('Departures', 'command', 'JUN Departures',
                       label = 'JUN',
                       command = gui_control.Command(self.evt_departures, parent, 'JUN' )
                      )
      main_menu.addmenuitem('Departures', 'command', 'JUL Departures',
                       label = 'JUL',
                       command = gui_control.Command(self.evt_departures, parent, 'JUL' )
                      )
      main_menu.addmenuitem('Departures', 'command', 'AUG Departures',
                       label = 'AUG',
                       command = gui_control.Command(self.evt_departures, parent, 'AUG' )
                      )
      main_menu.addmenuitem('Departures', 'command', 'SEP Departures',
                       label = 'SEP',
                       command = gui_control.Command(self.evt_departures, parent, 'SEP' )
                      )
      main_menu.addmenuitem('Departures', 'command', 'OCT Departures',
                       label = 'OCT',
                       command = gui_control.Command(self.evt_departures, parent, 'OCT' )
                      )
      main_menu.addmenuitem('Departures', 'command', 'NOV Departures',
                       label = 'NOV',
                       command = gui_control.Command(self.evt_departures, parent, 'NOV' )
                      )
      main_menu.addmenuitem('Departures', 'command', 'DEC Departures',
                       label = 'DEC',
                       command = gui_control.Command(self.evt_departures, parent, 'DEC' )
                      )
      #
      # Create the cascade "Statistics & Probabilites" menu and its items
      main_menu.addcascademenu(PT_name, 'Statistics & Probabilities',
                              'Statistical and Probabilities Tools',
                              label = 'Statistics & Probabilities',
                              traverseSpec = 'z', tearoff = tear_it
                             )
      # Create the cascade "Statistics" menu and its items
      main_menu.addcascademenu('Statistics & Probabilities','Statistics',
                              'PCMDI Statistical Tools',
                              label = 'PCMDI',
                              traverseSpec = 'z', tearoff = tear_it
                             )
      main_menu.addmenuitem('Statistics', 'command', 'Mean',
                       label = 'Mean...',
                       command = gui_control.Command(gui_statistics_question.create, parent, 'Mean' )
                      )
      main_menu.addmenuitem('Statistics', 'command', 'Statistics Covariance',
                       label = 'Covariance...',
                       command = gui_control.Command(gui_statistics_question.create, parent, 'Covariance' )
                      )
      main_menu.addmenuitem('Statistics', 'command', 'Staticsics Variance',
                       label = 'Variance...',
                       command = gui_control.Command(gui_statistics_question.create, parent, 'Variance' )
                      )
      main_menu.addmenuitem('Statistics', 'command', 'Statistics Standard Deviation',
                       label = 'Standard Deviation...',
                       command = gui_control.Command(gui_statistics_question.create, parent, 'Std' )
                      )
      main_menu.addmenuitem('Statistics', 'command', 'Statistics Root Mean Square',
                       label = 'Root Mean Square...',
                       command = gui_control.Command(gui_statistics_question.create, parent, 'Rms' )
                      )
      main_menu.addmenuitem('Statistics', 'command', 'Statistics Correlation',
                       label = 'Correlation...',
                       command = gui_control.Command(gui_statistics_question.create, parent, 'Correlation' )
                      )
      main_menu.addmenuitem('Statistics', 'command', 'Statistics Lagged Correlation',
                       label = 'Lagged Correlation...',
                       command = gui_control.Command(gui_statistics_question.create, parent, 'Laggedcorrelation' )
                      )
      main_menu.addmenuitem('Statistics', 'command', 'Statistics Covariance',
                       label = 'Lagged Covariance...',
                       command = gui_control.Command(gui_statistics_question.create, parent, 'laggedcovariance' )
                      )
      main_menu.addmenuitem('Statistics', 'command', 'Statistics Autocorrelation',
                       label = 'Autocorrelation...',
                       command = gui_control.Command(gui_statistics_question.create, parent, 'autocorrelation' )
                      )
      main_menu.addmenuitem('Statistics', 'command', 'Statistics Autocovariance',
                       label = 'Autocovariance...',
                       command = gui_control.Command(gui_statistics_question.create, parent, 'autocovariance' )
                      )
      main_menu.addmenuitem('Statistics', 'command', 'Statistics Mean Absolute Diffencnce',
                       label = 'Mean Absolute Difference...',
                       command = gui_control.Command(gui_statistics_question.create, parent, 'Meanabsdiff' )
                      )
      main_menu.addmenuitem('Statistics', 'command', 'Statistics Linear Regression',
                       label = 'Linear Regression...',
                       command = gui_control.Command(gui_statistics_question.create, parent, 'linearregression' )
                      )
      main_menu.addmenuitem('Statistics', 'command', 'Statistics Geometric Mean',
                       label = 'Geometric Mean...',
                       command = gui_control.Command(gui_statistics_question.create, parent, 'geometricmean' )
                      )
      main_menu.addmenuitem('Statistics', 'command', 'Statistics Median',
                       label = 'Median...',
                       command = gui_control.Command(gui_statistics_question.create, parent, 'median' )
                      )
      main_menu.addmenuitem('Statistics', 'command', 'Statistics Rank',
                       label = 'Rank (between 0 and 100)...',
                       command = gui_control.Command(gui_statistics_question.create, parent, 'rank' )
                      )


     
      # Create the cascade "Vertical Interpolation" menu and its items
      main_menu.addcascademenu(PT_name, 'Vertical',
                              'PCMDI Vertical Dimensions Tools',
                              label = 'Vertical Dims',
                              traverseSpec = 'z', tearoff = tear_it
                             )
      main_menu.addmenuitem('Vertical', 'command', 'Sigma ReconstructPressure',
                       label = 'Reconstruct Pressure: P=B*Ps+A*Po',
                       command = gui_control.Command(self.evt_sigma, parent, 'ReconstructedPressure')
                      )
      main_menu.addmenuitem('Vertical', 'command', 'Sigma Interpolate Linear',
                       label = 'Linear Interpolation',
                       command = gui_control.Command(self.evt_sigma, parent, 'InterpolatedToPressureLinear')
                      )
      main_menu.addmenuitem('Vertical', 'command', 'Sigma Interpolate Log',
                       label = 'Log-Linear Interpolation',
                       command = gui_control.Command(self.evt_sigma, parent, 'InterpolatedToPressureLog')
                      )
      
      # Create the cascade "Filters" menu and its items
      main_menu.addcascademenu(PT_name, 'Filters',
                              'PCMDI Filters Tools',
                              label = 'Filters',
                              traverseSpec = 'z', tearoff = tear_it
                             )
      main_menu.addmenuitem('Filters', 'command', 'Running Average',
                       label = 'Running Average',
                       command = gui_control.Command(gui_filters_question.create, parent, 'RunningAverage')
                      )
      main_menu.addmenuitem('Filters', 'command', '121',
                       label = '121 Filter',
                       command = gui_control.Command(gui_filters_question.create, parent, '121')
                      )
      main_menu.addmenuitem('Filters', 'command', 'custom',
                       label = 'Custom Filter',
                       command = gui_control.Command(gui_filters_question.create, parent, 'Custom')
                      )

      # Create the cascade "Vertical Levels" menu and its items
      main_menu.addcascademenu(PT_name, 'NoDescribe',
                              'PCMDI not self-describing files Tools',
                              label = 'Not Self Describing Files',
                              traverseSpec = 'z', tearoff = tear_it
                             )
      main_menu.addmenuitem('NoDescribe', 'command', 'BINARY READ DIALOG',
                       label = 'Read Binary File',
                       command = gui_control.Command(gui_read_Struct.read_bin_popup, self,parent)
                      )
      main_menu.addmenuitem('NoDescribe', 'command', 'ASCII READ DIALOG',
                       label = 'Read ASCII File',
                       command = gui_control.Command(gui_ascii.read_popup, self,parent)
                      )
      main_menu.addmenuitem('NoDescribe', 'command', 'ASCII READ DIALOG',
                       label = 'Read ASCII File - Time Series in Columns',
                       command = gui_control.Command(gui_ascii_cols.read_columns_popup, self,parent)
                      )
      
      
   def monthlyboundsset(self,parent):
      gui_busy.busyStart( self, parent )
      try:
         var=parent.panelDV.lst1[ parent.panelDV.selected]
      except Exception,err:
         var=None
      if var is None: 
         gui_busy.busyEnd( self, parent )
         gui_message.error( "The 'Set Bounds Time Tool' could not complete its function, because a variable was not selected.\nError:\n"+str(err) )
         return
      cdutil.setTimeBoundsMonthly(var)
      gui_control.record_command( parent, "\n# Monthly Data Bounds Set Function" , 1 )
      gui_control.record_command( parent, "cdutil.times.setTimeBoundsMonthly( %s )" % (var.id), 1 )
      gui_busy.busyEnd( self, parent )
      return
   
   def yearlyboundsset(self,parent):
      gui_busy.busyStart( self, parent )
      try:
         var=parent.panelDV.lst1[ parent.panelDV.selected]
      except Exception,err:
         var=None
      if var is None: 
         gui_busy.busyEnd( self, parent )
         gui_message.error( "The 'Set Bounds Time Tool' could not complete its function, because a variable was not selected.\nError:\n"+str(err) )
         return
      cdutil.setTimeBoundsYearly(var)
      gui_control.record_command( parent, "\n# Yearly Data Bounds Set Function" , 1 )
      gui_control.record_command( parent, "cdutil.times.setTimeBoundsYearly( %s )" % (var.id), 1 )
      gui_busy.busyEnd( self, parent )
      return
   
   def dailyboundsset(self,parent,fqcy):
      gui_busy.busyStart( self, parent )
      try:
         var=parent.panelDV.lst1[ parent.panelDV.selected]
      except Exception,err:
         var=None
      if var is None: 
         gui_busy.busyEnd( self, parent )
         gui_message.error( "The 'Set Bounds Time Tool' could not complete its function, because a variable was not selected.\nError:\n"+str(err) )
         return
      cdutil.setTimeBoundsDaily(var,frequency=fqcy)
      gui_control.record_command( parent, "\n# X-Daily Data Bounds Set Function" , 1 )
      gui_control.record_command( parent, "cdutil.times.setTimeBoundsDaily( %s, frequency=%s )" % (var.id,str(fqcy)), 1 )
      gui_busy.busyEnd( self, parent )
      return
   #----------------------------------------------------
   # Class "Extract" events
   #----------------------------------------------------
   def evt_extract( self, parent, fname ):
      import genutil
      genutil.statusbar.tk__=1
      gui_busy.busyStart( self, parent )
      var, var2 = self.get_var( parent )
      if var is None: 
         gui_message.error( "The 'Extract Time Tool' could not complete its function, because a variable was not selected." )
         gui_busy.busyEnd( self, parent )
         return
      try:
         if fname == 'Annual':
            slab=cdutil.YEAR.get( var , statusbar=1)
         elif fname == 'Seasonal':
            slab=cdutil.SEASONALCYCLE.get( var , statusbar=1)
         elif fname == 'DJF':
            slab=cdutil.DJF.get( var , statusbar=1)
         elif fname == 'MAM':
            slab=cdutil.MAM.get( var , statusbar=1)
         elif fname == 'JJA':
            slab=cdutil.JJA.get( var , statusbar=1)
         elif fname == 'SON':
            slab=cdutil.SON.get( var , statusbar=1)
         elif fname == 'Monthly':
            slab=cdutil.ANNUALCYCLE.get( var , statusbar=1)
         elif fname == 'JAN':
            slab=cdutil.JAN.get( var , statusbar=1)
         elif fname == 'FEB':
            slab=cdutil.FEB.get( var , statusbar=1)
         elif fname == 'MAR':
            slab=cdutil.MAR.get( var , statusbar=1)
         elif fname == 'APR':
            slab=cdutil.APR.get( var , statusbar=1)
         elif fname == 'MAY':
            slab=cdutil.MAY.get( var , statusbar=1)
         elif fname == 'JUN':
            slab=cdutil.JUN.get( var , statusbar=1)
         elif fname == 'JUL':
            slab=cdutil.JUL.get( var , statusbar=1)
         elif fname == 'AUG':
            slab=cdutil.AUG.get( var , statusbar=1)
         elif fname == 'SEP':
            slab=cdutil.SEP.get( var , statusbar=1)
         elif fname == 'OCT':
            slab=cdutil.OCT.get( var , statusbar=1)
         elif fname == 'NOV':
            slab=cdutil.NOV.get( var , statusbar=1)
         elif fname == 'DEC':
            slab=cdutil.DEC.get( var , statusbar=1)
      except Exception,err:
         gui_message.error( "The 'Extract Time Tool' could not complete its function, because of CDMSError: 'No axis matching order spec time dimension.\nError:\n"+str(err) )
         gui_busy.busyEnd( self, parent )
         return

      slab.id = slab.name = self.return_unique_name( var.id+'_Extract_' + fname )
      gui_user_menus.user_menus_put( slab.id, slab )

      # Record command
      if fname == 'Annual': fname = 'YEAR'
      elif fname == 'Seasonal': fname = 'SEASONALCYCLE'
      elif fname == 'Monthly': fname = 'ANNUALCYCLE'
      gui_control.record_command( parent, "\n# Extract Time Tool Function %s" % fname, 1 )
      gui_control.record_command( parent, "%s = cdutil.%s.get( %s, statusbar=1 )" % (slab.id, fname, var.id), 1 )

      gui_busy.busyEnd( self, parent )

   def evt_sigma(self,parent,fname):
      gui_busy.busyStart( self, parent )
      lst = parent.panelDV.selected_list.keys()
      data=[]
      slabnames=''
      try:
         for l in lst:
            data.append(parent.panelDV.lst1[ parent.panelDV.selected_list[ l ] ])
   ##          data.append(gui_formulate.data( parent, var = parent.panelDV.lst1[ parent.panelDV.selected ]))
            slabnames+=data[-1].id+', '
         slabnames=slabnames[:-2]
         if fname == 'ReconstructedPressure':
            slab=apply(cdutil.reconstructPressureFromHybrid,data)
         elif fname== 'InterpolatedToPressureLog':
            slab=apply(cdutil.logLinearInterpolation,data)
         elif fname== 'InterpolatedToPressureLinear':
            slab=apply(cdutil.linearInterpolation,data)
         else:
            raise 'Unkonwn command'+fname
      except Exception, err:
         gui_message.error( "The 'Sigma Hybrid  Tool' could not complete its function\nError:\n"+str(err) )
         gui_busy.busyEnd( self, parent )
         return
      slab.id = slab.name = self.return_unique_name( data[0].id+'_Sigma_' + fname )
      gui_user_menus.user_menus_put( slab.id, slab )
      # Record command
      if fname == 'ReconstructedPressure': fname = 'reconstructPressureFromHybrid'
      elif fname == 'InterpolatedToPressureLog': fname = 'logLinearInterpolation'
      elif fname == 'InterpolatedToPressureLinear': fname = 'linearInterpolation'
      gui_control.record_command( parent, "\n# Hybrid Sigma Level Tool: %s" % fname, 1 )
      gui_control.record_command( parent, "%s = cdutil.%s( %s )" % (slab.id, fname, slabnames), 1 )

      gui_busy.busyEnd( self, parent )

   def evt_filters(self,parent,slab,filter_name,fname,varid,options):
      slab.id = slab.name = self.return_unique_name( varid+'_Filters_' + filter_name )
      gui_user_menus.user_menus_put( slab.id, slab )
      gui_control.record_command( parent, "\n# Filters Tool Function %s" % fname, 1 )
      gui_control.record_command( parent, "%s = genutil.filters.%s( %s, %s )" % (slab.id, fname, varid, options), 1 )

   #----------------------------------------------------
   # Class "Climatology" events
   #----------------------------------------------------
   def evt_climatology( self, parent, fname ):
      import genutil
      genutil.statusbar.tk__=1
      gui_busy.busyStart( self, parent )
      var, var2 = self.get_var( parent )
      if var is None: 
         gui_message.error( "The 'Climatology Time Tool' could not complete its function, because a variable was not selected." )
         gui_busy.busyEnd( self, parent )
         return
      try:
         if fname == 'Annual':
            slab=cdutil.YEAR.climatology( var , statusbar=1)
         elif fname == 'Seasonal':
            slab=cdutil.SEASONALCYCLE.climatology( var , statusbar=1)
         elif fname == 'DJF':
            slab=cdutil.DJF.climatology( var , statusbar=1)
         elif fname == 'MAM':
            slab=cdutil.MAM.climatology( var , statusbar=1)
         elif fname == 'JJA':
            slab=cdutil.JJA.climatology( var , statusbar=1)
         elif fname == 'SON':
            slab=cdutil.SON.climatology( var , statusbar=1)
         elif fname == 'Monthly':
            slab=cdutil.ANNUALCYCLE.climatology( var , statusbar=1)
         elif fname == 'JAN':
            slab=cdutil.JAN.climatology( var , statusbar=1)
         elif fname == 'FEB':
            slab=cdutil.FEB.climatology( var , statusbar=1)
         elif fname == 'MAR':
            slab=cdutil.MAR.climatology( var , statusbar=1)
         elif fname == 'APR':
            slab=cdutil.APR.climatology( var , statusbar=1)
         elif fname == 'MAY':
            slab=cdutil.MAY.climatology( var , statusbar=1)
         elif fname == 'JUN':
            slab=cdutil.JUN.climatology( var , statusbar=1)
         elif fname == 'JUL':
            slab=cdutil.JUL.climatology( var , statusbar=1)
         elif fname == 'AUG':
            slab=cdutil.AUG.climatology( var , statusbar=1)
         elif fname == 'SEP':
            slab=cdutil.SEP.climatology( var , statusbar=1)
         elif fname == 'OCT':
            slab=cdutil.OCT.climatology( var , statusbar=1)
         elif fname == 'NOV':
            slab=cdutil.NOV.climatology( var , statusbar=1)
         elif fname == 'DEC':
            slab=cdutil.DEC.climatology( var , statusbar=1)
      except Exception,err:
         gui_message.error( "The 'Climatology Time Tool' could not complete its function, because of CDMSError: 'No axis matching order spec time dimension.\nError:\n"+str(err) )
         gui_busy.busyEnd( self, parent )
         return

      slab.id = slab.name = self.return_unique_name( var.id+'_Climatology_' + fname )
      gui_user_menus.user_menus_put( slab.id, slab )

      # Record command
      if fname == 'Annual': fname = 'YEAR'
      elif fname == 'Seasonal': fname = 'SEASONALCYCLE'
      elif fname == 'Monthly': fname = 'ANNUALCYCLE'
      gui_control.record_command( parent, "\n# Climatology Time Tool Function %s" % fname, 1 )
      gui_control.record_command( parent, "%s = cdutil.%s.climatology( %s, statusbar=1 )" % (slab.id, fname, var.id), 1 )
      gui_busy.busyEnd( self, parent )

   #----------------------------------------------------
   # Class "Departures" events
   #----------------------------------------------------
   def evt_departures( self, parent, fname ):
      import genutil
      genutil.statusbar.tk__=1
      gui_busy.busyStart( self, parent )
      var, var2 = self.get_var( parent )
      if var is None: 
         gui_message.error( "The 'Departures Time Tool' could not complete its function, because a variable was not selected." )
         gui_busy.busyEnd( self, parent )
         return
      try:
         if fname == 'Annual':
            slab=cdutil.YEAR.departures( var , statusbar=1)
         elif fname == 'Seasonal':
            slab=cdutil.SEASONALCYCLE.departures( var , statusbar=1)
         elif fname == 'DJF':
            slab=cdutil.DJF.departures( var , statusbar=1)
         elif fname == 'MAM':
            slab=cdutil.MAM.departures( var , statusbar=1)
         elif fname == 'JJA':
            slab=cdutil.JJA.departures( var , statusbar=1)
         elif fname == 'SON':
            slab=cdutil.SON.departures( var , statusbar=1)
         if fname == 'Monthly':
            slab=cdutil.ANNUALCYCLE.departures( var , statusbar=1)
         elif fname == 'JAN':
            slab=cdutil.JAN.departures( var , statusbar=1)
         elif fname == 'FEB':
            slab=cdutil.FEB.departures( var , statusbar=1)
         elif fname == 'MAR':
            slab=cdutil.MAR.departures( var , statusbar=1)
         elif fname == 'APR':
            slab=cdutil.APR.departures( var , statusbar=1)
         elif fname == 'MAY':
            slab=cdutil.MAY.departures( var , statusbar=1)
         elif fname == 'JUN':
            slab=cdutil.JUN.departures( var , statusbar=1)
         elif fname == 'JUL':
            slab=cdutil.JUL.departures( var , statusbar=1)
         elif fname == 'AUG':
            slab=cdutil.AUG.departures( var , statusbar=1)
         elif fname == 'SEP':
            slab=cdutil.SEP.departures( var , statusbar=1)
         elif fname == 'OCT':
            slab=cdutil.OCT.departures( var , statusbar=1)
         elif fname == 'NOV':
            slab=cdutil.NOV.departures( var , statusbar=1)
         elif fname == 'DEC':
            slab=cdutil.DEC.departures( var , statusbar=1)
      except Exception,err:
         gui_message.error( "The 'Departures Time Tool' could not complete its function, because of CDMSError: 'No axis matching order spec time dimension.\nError:\n"+str(err) )
         gui_busy.busyEnd( self, parent )
         return

      slab.id = slab.name = self.return_unique_name( var.id+'_Departures_' + fname )
      gui_user_menus.user_menus_put( slab.id, slab )

      # Record command
      if fname == 'Annual': fname = 'YEAR'
      elif fname == 'Seasonal': fname = 'SEASONALCYCLE'
      elif fname == 'Monthly': fname = 'ANNUALCYCLE'
      gui_control.record_command( parent, "\n# Departures Time Tool Function %s" % fname, 1 )
      gui_control.record_command( parent, "%s = cdutil.%s.departures( %s, statusbar=1 )" % (slab.id, fname, var.id), 1 )

      gui_busy.busyEnd( self, parent )

   #----------------------------------------------------
   # Method "Statistics" events
   #----------------------------------------------------
   def evt_statistics( self, parent, fname, axis = None, weights = None, noloop = 0 ):
      gui_busy.busyStart( self, parent )
      weights_passed=repr(weights)
      fname = string.lower( fname )
      current_var, previous_var = self.get_var( parent, fname )
      if current_var is None:
#         gui_message.error( "The 'Statistics Tool' could not complete its function, because a variable was not selected." )
         gui_busy.busyEnd( self, parent )
         return
      if ( (fname in ['covariance', 'rms', 'correlation', 'laggedcorrelation', 'laggedcovariance', 'meanabsdiff']) and
         (previous_var is None) ):
         gui_message.error( "'%s' must have two data variables. These data variables must be selected in the 'Defined Variables' window below." % fname )
         gui_busy.busyEnd( self, parent )
         return
      try:
##          print 'Trying for',fname
         if fname == 'covariance':
            slab=genutil.statistics.covariance( previous_var, current_var,
                                                axis=axis, weights=weights )
         elif fname == 'variance':
            slab=genutil.statistics.variance( current_var, axis=axis, weights=weights )
         elif fname == 'mean':
            slab=cdutil.averager( current_var, axis=axis, weight=weights )
         elif fname == 'std':
            slab=genutil.statistics.std( current_var, axis=axis, weights=weights )
         elif fname == 'rms':
            slab=genutil.statistics.rms( previous_var, current_var,
                                         axis=axis, weights=weights )
         elif fname == 'correlation':
            slab=genutil.statistics.correlation( previous_var, current_var,
                                                 axis=axis, weights=weights )
         elif fname == 'laggedcorrelation':
            slab=genutil.statistics.laggedcorrelation( previous_var, current_var, axis=axis, lag=noloop[0], noloop=noloop[1] )
         elif fname == 'laggedcovariance':
            slab=genutil.statistics.laggedcovariance( previous_var, current_var, axis=axis, lag=noloop[0], noloop=noloop[1] )
         elif fname == 'autocorrelation':
            slab=genutil.statistics.autocorrelation( current_var, axis=axis, lag=noloop[0], noloop=noloop[1] )
         elif fname == 'autocovariance':
            slab=genutil.statistics.autocovariance( current_var, axis=axis, lag=noloop[0], noloop=noloop[1] )
         elif fname == 'meanabsdiff':
            slab=genutil.statistics.meanabsdiff( previous_var, current_var,
                                                 axis=axis, weights=weights )
         elif fname == 'linearregression':
            slab1, slab2=genutil.statistics.linearregression( current_var, axis=axis )
         elif fname == 'geometricmean':
            slab=genutil.statistics.geometricmean( current_var, axis=axis )
         elif fname == 'median':
            slab=genutil.statistics.median( current_var, axis=axis )
         elif fname == 'rank':
            slab=genutil.statistics.rank( current_var, axis=axis )
      except Exception, err:
         gui_busy.busyEnd( self, parent )
         gui_message.error( "The 'Statistics Tool' could not complete its function, because of an error.\nError:\n" +str(err))
         return

      if fname != 'linearregression':
         # Record command
         if (fname in ['covariance', 'rms', 'correlation', 'meanabsdiff']):
            slab.id = slab.name = self.return_unique_name( '_Statistics_' + fname+'_'+current_var.id+'_'+previous_var.id )
            gui_control.record_command( parent, "\n# Statistics Tool Function %s" % fname, 1 )
            gui_control.record_command( parent, "%s = genutil.statistics.%s( %s, %s, axis=%s, weights=%s )" % (slab.id, fname,previous_var.id,current_var.id,repr(axis),weights_passed), 1 )
         elif (fname in [ 'laggedcorrelation', 'laggedcovariance']):
            slab.id = slab.name = self.return_unique_name( '_Statistics_' + fname+'_'+current_var.id+'_'+previous_var.id )
            gui_control.record_command( parent, "\n# Statistics Tool Function %s" % fname, 1 )
            gui_control.record_command( parent, "%s = genutil.statistics.%s( %s, %s, axis=%s, lag=%s, noloop=%s )" % (slab.id, fname,previous_var.id,current_var.id,repr(axis),repr(noloop[0]),repr(noloop[1])), 1 )
         elif (fname in [  'autocorrelation', 'autocovariance']):
            slab.id = slab.name = self.return_unique_name( '_Statistics_' + fname+'_'+current_var.id )
            gui_control.record_command( parent, "\n# Statistics Tool Function %s" % fname, 1 )
            gui_control.record_command( parent, "%s = genutil.statistics.%s( %s, axis=%s, lag=%s, noloop=%s )" % (slab.id, fname, current_var.id,repr(axis),repr(noloop[0]),repr(noloop[1])), 1 )
         elif fname=='mean':
##             slab=cdutil.averager( current_var, axis=axis, weight=weights )
            slab.id = slab.name = self.return_unique_name( '_Statistics_' + fname+'_'+current_var.id )
            gui_control.record_command( parent, "\n#Averager Tool Function" , 1 )
            gui_control.record_command( parent, "%s = cdutil.averager( %s, axis=%s, weights=%s )" % (slab.id,current_var.id,repr(axis),weights_passed), 1 )            
         elif fname in ['var','std']:
            slab.id = slab.name = self.return_unique_name( '_Statistics_' + fname+'_'+current_var.id )
            gui_control.record_command( parent, "\n# Statistics Tool Function %s" % fname, 1 )
            gui_control.record_command( parent, "%s = genutil.statistics.%s( %s, axis=%s, weights=%s )" % (slab.id, fname, current_var.id,repr(axis), weights_passed), 1 )
         else:
            slab.id = slab.name = self.return_unique_name( '_Statistics_' + fname+'_'+current_var.id )
            gui_control.record_command( parent, "\n# Statistics Tool Function %s" % fname, 1 )
            gui_control.record_command( parent, "%s = genutil.statistics.%s( %s, axis=%s )" % (slab.id, fname, current_var.id,repr(axis)), 1 )
         gui_user_menus.user_menus_put( slab.id, slab )
      else:
         slab1.id = slab1.name = self.return_unique_name( '_Statistics_' + fname+'_'+current_var.id+ '_' + slab1.id )
         gui_user_menus.user_menus_put( slab1.id, slab1 )
         slab2.id = slab2.name = self.return_unique_name( '_Statistics_' + fname+'_'+current_var.id + '_' + slab2.id )
         gui_user_menus.user_menus_put( slab2.id, slab2 )
         # Record command
         gui_control.record_command( parent, "\n# Statistics Tool Function %s" % fname, 1 )
         gui_control.record_command( parent, "%s, %s = genutil.statistics.%s( %s )" % (slab1.id, slab2.id, current_var.id, fname), 1 )

      gui_busy.busyEnd( self, parent )

   #----------------------------------------------------
   # Method "Statistics" events
   #----------------------------------------------------
   def evt_salstat( self, parent, fname, axis = None ):
      gui_busy.busyStart( self, parent )
      fname = string.lower( fname )
      lst = parent.panelDV.selected_list.keys()
      data=[]
      slabnames=''
      for f in ['OneSampleTTest','OneSampleSignTest','ChiSquareVariance','FTest',
                'TTestUnpaired','TTestPaired','PearsonsCorrelation',
                'TwoSampleSignTest','KendallsTau','KolmogorovSmirnov',
                'SpearmansCorrelation','WilcoxonRankSums','WilcoxonSignedRanks',
                'MannWhitneyU','LinearRegression','PairedPermutation','ChiSquare','Range',
                'anovaWithin','anovaBetween','KruskalWallisH','FriedmanChiSquare',
                'CochranesQ']:
         if f.lower()==fname:
            fname=f
      ## Check for function that take 1 argument only
      if fname in ['rankdata','tiecorrect','erfcc','zprob','ksprob','gamma','sumsquares',
                   'Range','harmonicmean','median','medianranks','mad','numberuniques',
                   'center','ssdevs','geometricmean','unbiasedvariance','variance',
                   'standarddeviation','coefficentvariance','skewness','kurtosis',
                   'standarderror','mode']:
         if len(lst)!=1:
            gui_message.error( "The Salstat function '"+fname+"' needs 1 argument only" )
            gui_busy.busyEnd( self, parent )
            return
      elif fname in ['differencesquared','chisqprob','inversechi','tprob',
                     'OneSampleTTest','OneSampleSignTest','ChiSquareVariance',
                     'TTestUnpaired','TTestPaired','PearsonsCorrelation',
                     'TwoSampleSignTest','KendallsTau','KolmogorovSmirnov',
                     'SpearmansCorrelation','WilcoxonRankSums','WilcoxonSignedRanks',
                     'MannWhitneyU','LinearRegression','PairedPermutation','ChiSquare',
                     ]:
          if len(lst)!=2:
            gui_message.error( "The Salstat function '"+fname+"' needs exactly 2 arguments" )
            gui_busy.busyEnd( self, parent )
            return
      elif fname in ['fprob','inversef','betacf','betai','FTest']:
          if len(lst)!=3:
            gui_message.error( "The Salstat function '"+fname+"' needs exactly 3 arguments" )
            gui_busy.busyEnd( self, parent )
            return
         
      try:
         for l in lst:
            data.append(parent.panelDV.lst1[ parent.panelDV.selected_list[ l ] ])
   ##          data.append(gui_formulate.data( parent, var = parent.panelDV.lst1[ parent.panelDV.selected ]))
            slabnames+=data[-1].id+', '
         slabnames=slabnames[:-2]
         ##axis ?
         if fname in ['chisqprob','inversechi','tprob','erfcc','zprob','ksprob','gamma','fprob','inversef','betacf','betai']:
            kw={}
         else:
            kw={'axis':axis}
         exec('slab=apply(genutil.salstat.'+fname+',data,kw)')
      except Exception, err:
         gui_busy.busyEnd( self, parent )
         gui_message.error( "The 'Salstat  Tool' could not complete its function\nError:\n"+str(err))
         return
      ## Construct the unique name that as all passed variables names
      dataid=''
      for d in data:
         dataid+=d.id+'_'
      dataid=dataid[:-1] #removes the trailing _
      if not isinstance(slab,list) and not isinstance(slab,tuple):
         slab.id = self.return_unique_name( data[0].id+'_Salstat_' + fname )
         gui_user_menus.user_menus_put( slab.id, slab )
      elif fname=='OneSampleTTest':
         slab[0].id = self.return_unique_name( dataid+'_Salstat_' + fname +'_t')
         slab[1].id = self.return_unique_name( dataid+'_Salstat_' + fname +'_prob')
         slab[2].id = self.return_unique_name( dataid+'_Salstat_' + fname +'_df')
      elif fname=='OneSampleSignTest':
         slab[0].id = self.return_unique_name( dataid+'_Salstat_' + fname +'_nplus')
         slab[1].id = self.return_unique_name( dataid+'_Salstat_' + fname +'_nminus')
         slab[2].id = self.return_unique_name( dataid+'_Salstat_' + fname +'_ntotal')
         slab[3].id = self.return_unique_name( dataid+'_Salstat_' + fname +'_z')
         slab[4].id = self.return_unique_name( dataid+'_Salstat_' + fname +'_prob')
      elif fname=='ChiSquareVariance':
         slab[0].id = self.return_unique_name( dataid+'_Salstat_' + fname +'_chisq')
         slab[1].id = self.return_unique_name( dataid+'_Salstat_' + fname +'_prob')
         slab[2].id = self.return_unique_name( dataid+'_Salstat_' + fname +'_df')
      elif fname=='TTestUnpaired':
         slab[0].id = self.return_unique_name( dataid+'_Salstat_' + fname +'_t')
         slab[1].id = self.return_unique_name( dataid+'_Salstat_' + fname +'_prob')
         slab[2].id = self.return_unique_name( dataid+'_Salstat_' + fname +'_df')
      elif fname=='TTestPaired':
         slab[0].id = self.return_unique_name( dataid+'_Salstat_' + fname +'_t')
         slab[1].id = self.return_unique_name( dataid+'_Salstat_' + fname +'_prob')
         slab[2].id = self.return_unique_name( dataid+'_Salstat_' + fname +'_df')
      elif fname=='PearsonsCorrelation':
         slab[0].id = self.return_unique_name( dataid+'_Salstat_' + fname +'_r')
         slab[1].id = self.return_unique_name( dataid+'_Salstat_' + fname +'_t')
         slab[2].id = self.return_unique_name( dataid+'_Salstat_' + fname +'_prob')
         slab[3].id = self.return_unique_name( dataid+'_Salstat_' + fname +'_df')
      elif fname=='FTest':
         slab[0].id = self.return_unique_name( dataid+'_Salstat_' + fname +'_f')
         slab[1].id = self.return_unique_name( dataid+'_Salstat_' + fname +'_prob')
         slab[2].id = self.return_unique_name( dataid+'_Salstat_' + fname +'_df1')
         slab[3].id = self.return_unique_name( dataid+'_Salstat_' + fname +'_df2')
      elif fname=='TwoSampleSignTest':
         slab[0].id = self.return_unique_name( dataid+'_Salstat_' + fname +'_nplus')
         slab[1].id = self.return_unique_name( dataid+'_Salstat_' + fname +'_nminus')
         slab[2].id = self.return_unique_name( dataid+'_Salstat_' + fname +'_ntotal')
         slab[3].id = self.return_unique_name( dataid+'_Salstat_' + fname +'_z')
         slab[4].id = self.return_unique_name( dataid+'_Salstat_' + fname +'_prob')
      elif fname=='KendallsTau':
         slab[0].id = self.return_unique_name( dataid+'_Salstat_' + fname +'_tau')
         slab[1].id = self.return_unique_name( dataid+'_Salstat_' + fname +'_z')
         slab[2].id = self.return_unique_name( dataid+'_Salstat_' + fname +'_prob')
      elif fname=='KolmogorovSmirnov':
         slab[0].id = self.return_unique_name( dataid+'_Salstat_' + fname +'_KS')
         slab[1].id = self.return_unique_name( dataid+'_Salstat_' + fname +'_prob')
      elif fname=='SpearmansCorrelation':
         slab[0].id = self.return_unique_name( dataid+'_Salstat_' + fname +'_rho')
         slab[1].id = self.return_unique_name( dataid+'_Salstat_' + fname +'_t')
         slab[2].id = self.return_unique_name( dataid+'_Salstat_' + fname +'_prob')
         slab[3].id = self.return_unique_name( dataid+'_Salstat_' + fname +'_df')
      elif fname=='WilcoxonRankSums':
         slab[0].id = self.return_unique_name( dataid+'_Salstat_' + fname +'_WR')
         slab[1].id = self.return_unique_name( dataid+'_Salstat_' + fname +'_prob')
      elif fname=='WilcoxonSignedRanks':
         slab[0].id = self.return_unique_name( dataid+'_Salstat_' + fname +'_W')
         slab[1].id = self.return_unique_name( dataid+'_Salstat_' + fname +'_Z')
         slab[2].id = self.return_unique_name( dataid+'_Salstat_' + fname +'_prob')
      elif fname=='MannWhitneyU':
         slab[0].id = self.return_unique_name( dataid+'_Salstat_' + fname +'_bigu')
         slab[1].id = self.return_unique_name( dataid+'_Salstat_' + fname +'_smallu')
         slab[2].id = self.return_unique_name( dataid+'_Salstat_' + fname +'_z')
         slab[3].id = self.return_unique_name( dataid+'_Salstat_' + fname +'_prob')
      elif fname=='LinearRegression':
         slab[0].id = self.return_unique_name( dataid+'_Salstat_' + fname +'_r')
         slab[1].id = self.return_unique_name( dataid+'_Salstat_' + fname +'_t')
         slab[2].id = self.return_unique_name( dataid+'_Salstat_' + fname +'_prob')
         slab[3].id = self.return_unique_name( dataid+'_Salstat_' + fname +'_slope')
         slab[4].id = self.return_unique_name( dataid+'_Salstat_' + fname +'_intercept')
         slab[5].id = self.return_unique_name( dataid+'_Salstat_' + fname +'_stderr')
         slab[6].id = self.return_unique_name( dataid+'_Salstat_' + fname +'_df')         
      elif fname=='PairedPermutation':
         slab[0].id = self.return_unique_name( dataid+'_Salstat_' + fname +'_utail')
         slab[1].id = self.return_unique_name( dataid+'_Salstat_' + fname +'_crit')
         slab[2].id = self.return_unique_name( dataid+'_Salstat_' + fname +'_prob')
      elif fname=='ChiSquare':
         slab[0].id = self.return_unique_name( dataid+'_Salstat_' + fname +'_chisq')
         slab[1].id = self.return_unique_name( dataid+'_Salstat_' + fname +'_prob')
         slab[2].id = self.return_unique_name( dataid+'_Salstat_' + fname +'_df')
      elif fname=='anovaWithin':
         slab[0].id = self.return_unique_name( dataid+'_Salstat_' + fname +'_SSint')
         slab[1].id = self.return_unique_name( dataid+'_Salstat_' + fname +'_SSres')
         slab[2].id = self.return_unique_name( dataid+'_Salstat_' + fname +'_SSbet')
         slab[3].id = self.return_unique_name( dataid+'_Salstat_' + fname +'_SStot')
         slab[4].id = self.return_unique_name( dataid+'_Salstat_' + fname +'_MSbet')
         slab[5].id = self.return_unique_name( dataid+'_Salstat_' + fname +'_MSwit')
         slab[6].id = self.return_unique_name( dataid+'_Salstat_' + fname +'_MSres')
         slab[7].id = self.return_unique_name( dataid+'_Salstat_' + fname +'_F')
         slab[8].id = self.return_unique_name( dataid+'_Salstat_' + fname +'_prob')
         slab[9].id = self.return_unique_name( dataid+'_Salstat_' + fname +'_dfbet')
         slab[10].id = self.return_unique_name( dataid+'_Salstat_' + fname +'_dfwit')
         slab[11].id = self.return_unique_name( dataid+'_Salstat_' + fname +'_dfres')
         slab[12].id = self.return_unique_name( dataid+'_Salstat_' + fname +'_dftot')         
      elif fname=='anovaBetween':
         slab[0].id = self.return_unique_name( dataid+'_Salstat_' + fname +'_SSbet')
         slab[1].id = self.return_unique_name( dataid+'_Salstat_' + fname +'_SSwit')
         slab[2].id = self.return_unique_name( dataid+'_Salstat_' + fname +'_SStot')
         slab[3].id = self.return_unique_name( dataid+'_Salstat_' + fname +'_MSbet')
         slab[4].id = self.return_unique_name( dataid+'_Salstat_' + fname +'_MSerr')
         slab[5].id = self.return_unique_name( dataid+'_Salstat_' + fname +'_F')
         slab[6].id = self.return_unique_name( dataid+'_Salstat_' + fname +'_prob')
         slab[7].id = self.return_unique_name( dataid+'_Salstat_' + fname +'_dfbet')
         slab[8].id = self.return_unique_name( dataid+'_Salstat_' + fname +'_dferr')
         slab[9].id = self.return_unique_name( dataid+'_Salstat_' + fname +'_dftot')
      elif fname=='KruskalWallisH':
         slab[0].id = self.return_unique_name( dataid+'_Salstat_' + fname +'_KruskalWallisH')
         slab[1].id = self.return_unique_name( dataid+'_Salstat_' + fname +'_prob')
         slab[2].id = self.return_unique_name( dataid+'_Salstat_' + fname +'_df')
      elif fname=='FriedmanChiSquare':
         slab[0].id = self.return_unique_name( dataid+'_Salstat_' + fname +'_sumranks')
         slab[1].id = self.return_unique_name( dataid+'_Salstat_' + fname +'_FriedmanChiSquare')
         slab[2].id = self.return_unique_name( dataid+'_Salstat_' + fname +'_prob')
         slab[3].id = self.return_unique_name( dataid+'_Salstat_' + fname +'_df')
      elif fname=='CochranesQ':
         slab[0].id = self.return_unique_name( dataid+'_Salstat_' + fname +'_CochranesQ')
         slab[1].id = self.return_unique_name( dataid+'_Salstat_' + fname +'_prob')
         slab[2].id = self.return_unique_name( dataid+'_Salstat_' + fname +'_df')
         
      # Record command
      gui_control.record_command( parent, "\n# Salstat Tool: %s" % fname, 1 )
      if not isinstance(slab,list) and not isinstance(slab,tuple):
         gui_control.record_command( parent, "%s = genutil.salstat.%s( %s )" % (slab.id, fname, slabnames), 1 )
      else:
         nms=''
         for s in slab:
            nms+=s.id+', '
            gui_user_menus.user_menus_put( s.id, s )
         nms=nms[:-2]
         gui_control.record_command( parent, "%s = genutil.salstat.%s( %s )" % (nms, fname, slabnames), 1 )

      gui_busy.busyEnd( self, parent )

   #
   # Get the selected variable( s ) from a file or defined in memory
   #
   def get_var( self, parent, fname=None ):
      slab1 = None
      slab2 = None
      try:
         if (parent.panelDM.var3 is not None):
            slab1=gui_formulate.data( parent, d_name = parent.panelDM.var3)
         else:
            from_selected = 1
            if (len(parent.panelDV.selected) == 0):
               gui_message.error('Must first select a variable from the "Select Variable" panel above or from the "Defined Variables" list window below.')
               return (None, None)
            slab1=gui_formulate.data( parent, var = parent.panelDV.lst1[ parent.panelDV.selected ])
            # Statics functions that need two slabs
            if (fname in ['covariance', 'rms', 'correlation', 'laggedcorrelation', 'laggedcovariance', 'meanabsdiff']):
               try:
                  lst = parent.panelDV.selected_list.keys()
                  slab2=parent.panelDV.lst1[ parent.panelDV.selected_list[ lst[ -2 ] ] ]
##                   gui_control.record_command( parent, "\n# Assign slab1 and slab2", 1 )
##                   gui_control.record_command(parent,("slab1 = %s" % string.split(parent.panelDV.selected_list[ lst[ -2 ] ], ' ')[0]), 1 )
##                   gui_control.record_command(parent,("slab2= %s" % string.split(parent.panelDV.selected, ' ')[0]), 1 )
               except Exception,err:
                  gui_message.error( "'%s' must have two data variables. These data variables must be selected in the 'Defined Variables' window below." % fname )
                  gui_busy.busyEnd( self, parent )
                  return (None, None)
      except Exception,err:
         gui_message.error( 'Invalid data or no variable selected. Cannot compute field.\nError:\n'+str(err) )
         gui_busy.busyEnd( self, parent )
         return (None, None)
      return (slab1, slab2)
#   def get_var( self, parent ):
#      try:
#         if (parent.panelDM.var3 is not None):
#            slab=gui_formulate.data( parent, d_name = parent.panelDM.var3)
#         else:
#            from_selected = 1
#            if (len(parent.panelDV.selected) == 0):
#                gui_message.error('Must first select a variable from the "Select Variable" panel above or from the "Defined Variables" list window below.')
#               return None
#            slab=gui_formulate.data( parent, var = parent.panelDV.lst1[ parent.panelDV.selected ])
#      except:
#         gui_message.error( 'Invalid data or no variable selected. Cannot plot.' )
#         return None
#      return slab
   #
   # Return a unique name to be placed in the defined variable window
   #
   def return_unique_name( self, name ):
      a=dir(__main__)
      n_name = name
      i = 1
      while n_name in a:
         n_name=name+str(i)
         i += 1
      return ( n_name )

class create_external_plot_tools_menu:
   def __init__( self, main_menu, parent, tear_it ):
      PT_name = 'SpecializedDiagnosis'
      if ((sys.platform == 'darwin') and (gui_control.do_aqua == 1)): PT_name += '  '
      main_menu.addmenu(PT_name, 'Plotting Tools', tearoff = tear_it)
      #
      # Create the cascade "Thermo Tools" menu and its items
      main_menu.addcascademenu(PT_name, 'thermo',
                              'PCMDI thermodynamic Tools',
                              label = 'Thermodynamic Diagrams',
                              traverseSpec = 'z', tearoff = tear_it
                             )
      
      main_menu.thermo_options = Tkinter.IntVar()
      main_menu.thermo_options.set(0)
      main_menu.addmenuitem('thermo', 'checkbutton', 'Toggle me on/off',
                label = 'Show options dialog',
                selectcolor=gui_color.one,
                variable = main_menu.thermo_options,
              )
      main_menu.addmenuitem('thermo', 'command', 'dew',
                       label = 'Dewpoint (K)',
                       command = gui_control.Command(gui_thermo.dewpoint, self,parent)
                      )
      main_menu.addmenuitem('thermo', 'command', 'es',
                       label = 'Saturated Pressure (Pa)',
                       command = gui_control.Command(gui_thermo.es, self,parent)
                      )
      main_menu.addmenuitem('thermo', 'command', 'lapse',
                       label = 'Moist Adiabatic Lapse Rate (K/Pa)',
                       command = gui_control.Command(gui_thermo.lapse, self,parent)
                      )
      main_menu.addmenuitem('thermo', 'command', 'lift',
                       label = 'Lift Moist Adiabatically',
                       command = gui_control.Command(gui_thermo.lift, self,parent)
                      )
      main_menu.addmenuitem('thermo', 'command', 'ws',
                       label = 'Saturated Mixing Ratio',
                       command = gui_control.Command(gui_thermo.ws, self,parent)
                      )
      main_menu.addmenuitem('thermo', 'command', 'SkewT',
                       label = 'Skew-T',
                       command = gui_control.Command(gui_thermo.skewT, self,parent)
                      )
      main_menu.addmenuitem('thermo', 'command', 'SkewTW',
                       label = 'Skew-T with Wind',
                       command = gui_control.Command(gui_thermo.skewTWind, self,parent)
                      )
      main_menu.addmenuitem('thermo', 'command', 'emagram',
                       label = 'Emagram',
                       command = gui_control.Command(gui_thermo.emagram, self,parent)
                      )
      main_menu.addmenuitem('thermo', 'command', 'emagramW',
                       label = 'Emagram with Wind',
                       command = gui_control.Command(gui_thermo.emagramWind, self,parent)
                      )
      main_menu.addmenuitem('thermo', 'command', 'tephi',
                       label = 'Tephigram',
                       command = gui_control.Command(gui_thermo.tephigram, self,parent)
                      )
      main_menu.addmenuitem('thermo', 'command', 'tephiW',
                       label = 'Tephigram with wind',
                       command = gui_control.Command(gui_thermo.tephigramWind, self,parent)
                      )
      main_menu.addmenuitem('thermo', 'command', 'stuve',
                       label = 'Stuve',
                       command = gui_control.Command(gui_thermo.stuve, self,parent)
                      )
      main_menu.addmenuitem('thermo', 'command', 'stuveW',
                       label = 'Stuve with Wind',
                       command = gui_control.Command(gui_thermo.stuveWind, self,parent)
                      )
      # Create the cascade "Thermo Tools" menu and its items
      main_menu.addcascademenu(PT_name, 'wk',
                              'Wheeler-Koladis 99 Tools',
                              label = 'Wheeler-Koladis 99',
                              traverseSpec = 'z', tearoff = tear_it
                             )
      
      main_menu.wk_options = Tkinter.IntVar()
      main_menu.wk_options.set(0)
      main_menu.addmenuitem('wk', 'checkbutton', 'Toggle me on/off',
                label = 'Show options dialog',
                selectcolor=gui_color.one,
                variable = main_menu.wk_options,
              )
      main_menu.addmenuitem('wk', 'command', 'power',
                       label = 'Process ALL',
                       command = gui_control.Command(gui_wk.process, self,parent)
                      )
      main_menu.addmenuitem('wk', 'command', 'power',
                       label = 'Compute Power',
                       command = gui_control.Command(gui_wk.power, self,parent)
                      )
      main_menu.addmenuitem('wk', 'command', 'split',
                       label = 'Split Power bet Sym/ASym components',
                       command = gui_control.Command(gui_wk.split, self,parent)
                      )
      main_menu.addmenuitem('wk', 'command', 'bg',
                       label = 'Compute background power',
                       command = gui_control.Command(gui_wk.background, self,parent)
                      )
      main_menu.addmenuitem('wk', 'command', 'fig1',
                       label = 'Draw Figure 1 (Sym/ASym power)',
                       command = gui_control.Command(gui_wk.fig1, self,parent)
                      )
      main_menu.addmenuitem('wk', 'command', 'fig2',
                       label = 'Draw Figure 2 (background)',
                       command = gui_control.Command(gui_wk.fig2, self,parent)
                      )
      main_menu.addmenuitem('wk', 'command', 'fig3',
                       label = 'Draw Figure 3 (Sym/ASym power with waves)',
                       command = gui_control.Command(gui_wk.fig3, self,parent)
                      )



#----------------------------------------------------------------------------------------
# Create the Help menu and its menu items
#----------------------------------------------------------------------------------------
class create_help_menu:
   def __init__( self, main_menu, parent, tear_it):
      H_name = 'Help'
      if ((sys.platform == 'darwin') and (gui_control.do_aqua == 1)): H_name = 'Help '
      main_menu.addmenu(H_name, 'VCDAT Help', side='right', tearoff = tear_it)
      gui_support.add_balloon_help(main_menu, H_name)
      main_menu.addmenuitem(H_name, 'separator')
      
      main_menu.addmenuitem(H_name, 'command', "run help",
                            label = 'CDAT Help',
                            command = self.evt_cdat_help
                           )
      main_menu.addmenuitem(H_name, 'command', "run demo",
                            label = 'Run CDAT Demo',
                            command = self.evt_cdat_demo
                           )
      main_menu.addmenuitem(H_name, 'command', "Go to CDAT's Web site",
                            label = 'CDAT Web Site',
                            command = self.evt_cdat_web
                           )
      main_menu.addmenuitem(H_name, 'command', "Go to PCMDI's Web site",
                            label = 'PCMDI Web Site',
                            command = self.evt_pcmdi_web
                           )
      main_menu.addmenuitem(H_name, 'command', "Go to CDAT/LAS Web site",
                            label = 'CDAT-LAS Web Site',
                            command = self.evt_cdat_las_web
                           )
      main_menu.addmenuitem(H_name, 'command', "Go to numpy's Web Site",
                            label = 'numpy Web Site',
                            command=self.evt_numpy_web
                           )
      main_menu.addmenuitem(H_name, 'command', "Go to Python's Web Site",
                            label = 'Python Web Site',
                            command=self.evt_python_web
                           )

      main_menu.addmenuitem(H_name, 'separator')

      main_menu.addmenuitem(H_name, 'command', 'Help About',
                            label = 'About VCDAT',
                            command = gui_control.Command(gui_about.create, parent)
                           )
   #----------------------------------------------------
   # Class "Help" events
   #----------------------------------------------------
   #
   ####### event to bring up CDAT Help app
   #
   def evt_cdat_help( self ):
      os.popen("cdathelp &")
   #
   ####### event to bring up CDAT Demo app
   #
   def evt_cdat_demo( self ):
      os.popen("cdatdemo -hide_vcdat &")
   #
   ####### event to bring up a web browser that is displaying the CDAT web page
   #
   def evt_cdat_web( self ):
      webbrowser.open('http://cdat.sourceforge.net')
   #
   ####### event to bring up a web browser that is displaying the PCMDI web page
   #
   def evt_pcmdi_web( self ):
      webbrowser.open('http://www-pcmdi.llnl.gov')
   #
   ####### event to bring up a web browser that is displaying the CDAT-LAS web page
   #
   def evt_cdat_las_web( self ):
      webbrowser.open('http://esg.llnl.gov/las')
   #
   ####### event to bring up a web browser that is displaying the numpy web page
   #
   def evt_numpy_web( self ):
      webbrowser.open('http://numpy.sourceforge.net')
   #
   ####### event to bring up a web browser that is displaying the PCMDI Software web page
   #
   def evt_python_web( self ):
      webbrowser.open('http://www.python.org')
   #

#
#---------------------------------------------------------------------------
# Define event function's of menu's
#---------------------------------------------------------------------------
#
#---------------------------------------------------------------------
# End menu Layout
#---------------------------------------------------------------------
#
#---------------------------------------------------------------------
# End of File
#---------------------------------------------------------------------
