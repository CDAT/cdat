#!/usr/bin/env python
# Adapted for numpy/ma/cdms2 by convertcdms.py
#
# The PCMDI Data Browser controls -  gui_control module
#
###############################################################################
#                                                                             #
# Module:       gui_control module                                            #
#                                                                             #
# Copyright:    "See file Legal.htm for copyright information."               #
#                                                                             #
# Authors:      PCMDI Software Team                                           #
#               Lawrence Livermore NationalLaboratory:                        #
#               support@pcmdi.llnl.gov                                        #
#                                                                             #
# Description:  PCMDI Software System browser interface controls.             #
#                                                                             #
# Version:      4.0                                                           #
#                                                                             #
###############################################################################

import os, string, types, time, cdtime, cdms2 as cdms, cdat_info

#------------------------------------------------------------------------
# Redirect the destination of sys.stderr to the CDAT output window
#------------------------------------------------------------------------
#class standard_err:
#   def __init__( self, master ):
#        self.parent = master
#
#   def write(self,s):
#        self.parent.panelA.pan3.insert( 'end', s )

#------------------------------------------------------------------------
# Redirect the destination of sys.stdout to the CDAT output window
#------------------------------------------------------------------------
#class standard_out:
#   def __init__( self, master ):
#        self.parent = master
#
#   def write(self,s):
#        self.parent.panelA.pan3.insert( 'end', s )

#--------------------------------------------------------------------------
# Number of dimensions the GUI can handle and the dimension axis labels
# To increase the number of dimensions that VCDAT can handle, just increase
# the ndim value and give the appropriate dim_axis representitive.
#--------------------------------------------------------------------------
ndim = 26
dim_axis=['X-Axis','Y-Axis','Z-Axis','T-Axis','W-axis','U-axis','V-axis']
## Now adds labels from 'A' to 'R'
for i in range(18):
   dim_axis.append(chr(i+65))

#--------------------------------------------------------------------------
# This number represents the maximum number of digits that is used to
# represented sequence of selected variables in the Defined Variable window. 
# The maximum number is 2 + the space represents a lenth of size 3. To 
# increase this number, change the number below and also change the place
# holer symbol "00 " below.
# You will need to change one other place in the gui_defined_variables.py file
# if you are going to increase the number of selected to be greater than 100.
# In the files gui_defined_variables and gui_alter_variable.py, search for:
# 'Update the selection numbering scheme'
#--------------------------------------------------------------------------
dvsize = 3
dvholder = "-- "

#--------------------------------------------------------------------------
# Alias names for dimension values                                         
#--------------------------------------------------------------------------
longitude_alias = ['longitude'] + cdms.axis.longitude_aliases
latitude_alias  = ['latitude'] + cdms.axis.latitude_aliases
level_alias     = ['level', 'plev'] + cdms.axis.level_aliases
time_alias      = ['time'] + cdms.axis.time_aliases

#--------------------------------------------------------------------------
# Favorite directory list
#--------------------------------------------------------------------------
favorite_directories = []
favorite_index=None

#--------------------------------------------------------------------------
# Favorite files list
#--------------------------------------------------------------------------
favorite_files = []
favorite_files_index=None

#--------------------------------------------------------------------------
# Search for the following callable types in the "Extened GUI Menus" popup            
#--------------------------------------------------------------------------
search_function_type_list = [types.FunctionType, types.InstanceType, types.BuiltinFunctionType]

#--------------------------------------------------------------------------
# Search list to exclude the following modules in the "Extended GUI Menus" popup            
#--------------------------------------------------------------------------
do_not_show_in_list = ['__builtins__', '__doc__', '__main__', '__name__', 'Pmw', 'Tkinter', 'browser', 'gui_alter_plot', 'gui_color', 'gui_control', 'gui_defined_variables', 'gui_dimensions', 'gui_functions', 'gui_graphics_control', 'gui_menu', 'gui_select_variable', 'gui_variable_information', 'types', 'os', 'string', 'sys', 'tk_root', 'vcs', 'vcs_function']

#--------------------------------------------------------------------------
# Calendar list from cdtime
#--------------------------------------------------------------------------
calendar_list = {'gregorian':cdtime.GregorianCalendar, 'julian':cdtime.JulianCalendar,
                 'noleap':cdtime.NoLeapCalendar, 'd360':cdtime.Calendar360, 'clim':cdtime.ClimCalendar,
                 'climleap':cdtime.ClimLeapCalendar, 'no_calendar':cdtime.DefaultCalendar,'proleptic_gregorian':cdtime.StandardCalendar }


#---------------------------------------------------------------------
# Controls for the GUI fonts
#---------------------------------------------------------------------
gui_font=("helvetica", 12 )
menu_font=("helvetica", 12 )
mbutton_font=("helvetica", 12  )

#---------------------------------------------------------------------
# Idle's Controls for the GUI fonts
#---------------------------------------------------------------------
idle_font_height='10'
idle_font_size='12'
idle_font_width='80'
idle_font_name='courier'

#---------------------------------------------------------------------
# Control the width of the listboxes in the third panel
#---------------------------------------------------------------------
listbox_width = 16
dimname_width = 10

#---------------------------------------------------------------------------------
# Control the width of the scrolling space of vertical and horizontal scroll bars
#---------------------------------------------------------------------------------
scl_width = 10

#---------------------------------------------------------------------------------
# Control the minimum width of the defined variable and template listbox
#---------------------------------------------------------------------------------
mini_defined_template_gm_width = 135
defined_frame_scale=1.75

#---------------------------------------------------------------------------------
# Controls for the dimensions panel
#---------------------------------------------------------------------------------
dim_scale_ratio=2.
dim_button_ratio=.8
dim_button_min=50
dim_button_max=150

#---------------------------------------------------------------------------------
# Flag to state directory use or CDMS database connection
#---------------------------------------------------------------------------------
directory_or_database = 'directory'

#---------------------------------------------------------------------------------
# Flag to show CDMS GUI database connection GUI
#---------------------------------------------------------------------------------
try:
   import ldap
   have_cdms_database = 1
   try:
      uri = os.environ['CDMSROOT']
   except:
      have_cdms_database = 0
except:
   have_cdms_database = 0

#---------------------------------------------------------------------------------
# CDMS database connection handle
#---------------------------------------------------------------------------------
db = None

#---------------------------------------------------------------------------------
# List of CDMS database connections
#---------------------------------------------------------------------------------
db_connections = [] 

#---------------------------------------------------------------------------------
# Dictionary that contains dynamic dimension information
#---------------------------------------------------------------------------------
locked_dim_information = {}
locked_dim_order = {}

#---------------------------------------------------------------------------------
# Input for the pull down menu options located above the "Selected" scroll window
# and the dimension scroll windows
#---------------------------------------------------------------------------------
dbdchlst = [ "Directory",
          "Database"
        ]

dirimpchlst = [ "File",
          "Import"
        ]

gmchlst = [ "Boxfill",
            "BoxDiscrete",
            "BoxedIsoline",
            "FilledIsoline",
            "Isofill",
            "Isoline",
            "Meshfill",
            "Outfill",
            "Outline",
            "Scatter",
            "Taylordiagram",
            "Vector",
            "XvsY",
            "Xyvsy",
            "Yxvsx",
            "Ext (tbd)",
            ]

potchlst = [ "VCS Canvas 1",
          "VCS Canvas 2",
          "VCS Canvas 3",
          "VCS Canvas 4",
          "Background Canvas",
          "Clear VCS Canvas 1",
          "Close VCS Canvas 1"
        ]

potchlst2 = [ "VCS Canvas GUI",
          "Clear VCS Canvas GUI",
          "Close VCS Canvas GUI"
        ]

datafilechlst = [ "Data Files (by extension)",
                  "*.nc Files",
                  "*.ctl Files",
                  "*.cdms Files",
                  "*.dic Files",
                  "*.hdf Files",
                  "*.xml Files",
                  "*.cdml Files",
                  "*.pp Files",
                  "All Files",
                  "Find Pattern",
                  "Datasets"
                ]

dimchlst =  [ 'def  default axis points',
              'sum  summation of selected axis points',
              'avg  average of selected axis points',
              'wgt  weighted average of selected axis points',
              'awt  altered weighted average of selected axis points',
              'gtm  geometrical mean of selected axis points',
              'std  standard deviation over selected axis points',
            ]

dimchlst2 =  [ 'def default axis points',
              'sum  summation of selected axis points',
              'avg  average of selected axis points',
              'awt  altered weighted average of selected axis points',
              'gtm  geometrical mean of selected axis points',
              'std  standard deviation over selected axis points',
            ]

datachlst = [ "a  alter the variable's attributes",
              "n  save variable to netCDF file",
              "r  remove the selected data variable",
              "R  remove [all] the variables in the data list"
            ]

#---------------------------------------------------------------------------------
# List of filetypes to search for in the "File Select" popup dialog window
#---------------------------------------------------------------------------------
filetypes = [
        ("Python and text files", "*.py *.pyw *.txt", "TEXT"),
        ("All text files", "*", "TEXT"),
        ("All files", "*"),
        ]

#---------------------------------------------------------------------------------
# List of filetypes to search for in the "File Select" popup dialog window
#---------------------------------------------------------------------------------
datatypes = [
         ("Search for Data files", "*.nc *.ctl *.cdms *.dic *.hdf *.xml *.cdml *.pp"),
         ("Search for netCDF files", "*.nc"),
         ("Search for GrADS files", "*.ctl"),
         ("Search for CDMS files", "*.cdms"),
         ("Search for DRS files", "*.dic"),
         ("Search for PP files", "*.pp"),
         ("Search for HDF files", "*.hdf"),
         ("Search for XML files", "*.xml"),
         ("Search for XML files", "*.cdml"),
         ("Search for PSQL files", "*.cdms"),
         ("All files", "*")
         ]

#---------------------------------------------------------------------------------
# Set the Mac OS X Aqua Tcl/TK flag to alter the VCDAT GUI        
# The default setting is off (i.e., 0). To turn Aqua setting
# on set to 1 (e.g., do_aqua = 1).
#---------------------------------------------------------------------------------
do_aqua = cdat_info.enable_aqua

#---------------------------------------------------------------------------------
# Event handling function that will allow the passing of arguments
#---------------------------------------------------------------------------------
class Command:
   def __init__(self, func, *args, **kw):
      self.func = func
      self.args = args
      self.kw = kw

   def __call__(self, *args, **kw):
      args = self.args + args
      kw.update(self.kw)
      return apply(self.func, args, kw)

#-----------------------------------------------------------
max_help_width = 50
#-----------------------------------------------------------

#---------------------------------------------------------------------------------
# Start recording command file
#---------------------------------------------------------------------------------
def start_recording_commands( parent ):
   import sys, os, gui_message

   # Start recording advanced script file
   try:
       fn = '%s/PCMDI_GRAPHICS' % os.environ['HOME']
   except:
       print "Could not find the $HOME directory. Set your environment variable 'HOME'"
       print "to your home directory. (e.g., 'setenv HOME /home/user')."
       sys.exit()
   #
   # Create PCMDI_GRAPHICS directory if it does not exist
   if os.access(fn, os.X_OK) == 0:
      try:
         os.mkdir( fn )
      except:
         print 'Do not have write permission for home directory. Must have write permissions.'
         sys.exit()
   
   file_name = fn + '/vcdat_recording_script_file.py'
   parent.fp_filename = file_name
   parent.fp_record = open( parent.fp_filename, 'w')
   parent.fp_record.write( 'import cdms2, vcs, cdutil, genutil, os, sys\n' )
   parent.fp_record.write( 'import MV2\n' )
   parent.fp_record.write( '\n' )
   parent.fp_tell = parent.fp_record.tell()
   parent.fp_record.close()

   # Start recording beginner's script file
   file_name = fn + '/vcdat_teaching_script_file.py'
   parent.fp_filename_b = file_name
   parent.fp_record_b = open( parent.fp_filename_b, 'w')
   parent.fp_record_b.write( 'import cdms2, vcs, cdutil, genutil, os, sys\n' )
   parent.fp_record_b.write( 'import MV2\n' )
   parent.fp_record_b.write( '\n' )
   parent.fp_tell_b = parent.fp_record_b.tell()
   parent.fp_record_b.close()


#---------------------------------------------------------------------------------
# Record command in the command file
#
# 0 - Only do advanced scripting
# 1 - Do both advanced and beginner scripting
# 2 - Only do beginner scripting
#---------------------------------------------------------------------------------
def record_command( parent, command_str, do_beginner = 0 ):
   if parent.menu.record_commands_flg != 1:
      return
   else:
      if do_beginner != 2:
         parent.fp_record = open( parent.fp_filename, 'a')
         parent.fp_record.seek( parent.fp_tell )
         #
         c_string = command_str + '\n'
         parent.fp_record.write( c_string )
         #
         parent.fp_tell = parent.fp_record.tell()
         parent.fp_record.close()

      if do_beginner != 0:
         parent.fp_record_b = open( parent.fp_filename_b, 'a')
         parent.fp_record_b.seek( parent.fp_tell_b )
         #
         c_string = command_str + '\n'
         parent.fp_record_b.write( c_string )
         #
         parent.fp_tell_b = parent.fp_record_b.tell()
         parent.fp_record_b.close()

#---------------------------------------------------------------------------------
# Start tracking directories, files, and variables requested by the user. 
# If the "TrackUser" directory doesn't exist in the "$HOME/PCMDI_GRAPHICS
# directory, then create it.
#---------------------------------------------------------------------------------
def start_tracking_directory_file_variable_log( parent ):
   # Create the TrackUser directory string
   try:
       fn = '%s/PCMDI_GRAPHICS/TrackUser' % os.environ['HOME']
   except:
       print "Could not find the $HOME directory. Set your environment variable 'HOME'"
       print "to your home directory. (e.g., 'setenv HOME /home/user')."
       sys.exit()

   # Create $HOME/PCMDI_GRAPHICS/TrackUser directory, if it does not already exist
   if os.access(fn, os.X_OK) == 0:
      try:
         os.mkdir( fn )
      except:
         print 'Do not have write permission for home directory. Must have write permissions.'
         sys.exit()

   user_name = os.getlogin()
   user_date = time.asctime()
   file_name = fn + '/' + user_name + '_' + string.replace(user_date, ' ', '_')
   parent.fp_filename_log = file_name

   parent.fp_record_log = open( parent.fp_filename_log, 'w')
   parent.fp_record_log.write( '# This file contains the directories, files, and variables requested by the user: %s.\n' % user_name )
   parent.fp_record_log.write( '# These requests were made on the date: %s. \n#\n' % user_date )
   parent.fp_record_log.write( '#______________________________________________________________________________\n\n' )
   parent.fp_tell_log = parent.fp_record_log.tell()
   parent.fp_record_log.close()

def track_user( parent, command_str ):
   parent.fp_record_log = open( parent.fp_filename_log, 'a')
   parent.fp_record_log.seek( parent.fp_tell_log )
   #
   c_string = command_str + '\n'
   parent.fp_record_log.write( c_string )
   #
   parent.fp_tell_log = parent.fp_record_log.tell()
   parent.fp_record_log.close()

#---------------------------------------------------------------------
# End of File
#---------------------------------------------------------------------









