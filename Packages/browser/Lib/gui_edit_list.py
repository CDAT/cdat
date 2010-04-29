#!/usr/bin/env python
# Adapted for numpy/ma/cdms2 by convertcdms.py
#
# The PCMDI Data Browser alias directory editor -  gui_edit_list module
#
#################################################################################
#                                                                               #
# Module:       gui_edit_list module                                            #
#                                                                               #
# Copyright:    "See file Legal.htm for copyright information."                 #
#                                                                               #
# Authors:      PCMDI Software Team                                             #
#               Lawrence Livermore National Laboratory:                         #
#               support@pcmdi.llnl.gov                                          #
#                                                                               #
# Description:  Sets and removes dimension named aliases. For example,          #
#               the time dimension name can equal the following names:          #
#		'time', 'TIME', 'Time', 'T', or 't'. Also, adds to the          #
# 		favorite directory list.                                        #
#                                                                               #
# Version:      4.0                                                             #
#                                                                               #
#################################################################################
#
#---------------------------------------------------------------------
# NOTE: need to use version of Python that imports Tkinter and Pmw
#---------------------------------------------------------------------
import Tkinter, Pmw
import string, os, sys
import cdms2 as cdms
import gui_control
import gui_message
import gui_select_variable
from gui_support import gui_color
import cdat_info

#---------------------------------------------------------------------
#
# Start of Popup Dialog
#
#---------------------------------------------------------------------------
# Dimension Alias Dialog Popup
#---------------------------------------------------------------------------
#
class create:
   def __init__(self, parent, dim_name, event=None):
      if dim_name == 'directory':
        title = 'List of Favorite Directories'
      elif dim_name == 'mybinfiles':
        title = 'List of Favorite Files'
      else:
         title = 'Dimension Aliases for %s' % dim_name
      self.parent = parent
      self.dialog = Pmw.Dialog( parent,
         title = title,
         buttons = ( 'Dismiss', ),
         defaultbutton = 'Dismiss' )
      self.dialog.unbind( "<Return>" )

      if parent.menu.popup_window_settings_flg == 1:
         self.dialog.transient( self.parent ) # Keep widget on top of its parent

      # Create the ScrolledListBox.
      if dim_name == 'directory':
        label_text = 'Directories:'
        items=gui_control.favorite_directories
        dbclkcmd=gui_control.Command(self.goto_directory, parent)
      elif dim_name == 'mybinfiles':
        label_text = 'Files:'
        items=gui_control.favorite_files
        dbclkcmd=gui_control.Command(self.open_file, parent)
      else:
         label_text = '%s Aliases:' % dim_name
	 items=return_alias_list( dim_name )
         dbclkcmd=gui_control.Command(self.goto_directory, parent)
         
      self.alias_list = Pmw.ScrolledListBox(self.dialog.interior(),
		items=items,
		labelpos='nw',
		label_text = label_text,
		listbox_height = 6,
                listbox_background = 'white',
                listbox_foreground = 'black',
                dblclickcommand = dbclkcmd,
		usehullsize = 1,
		hull_width = 250,
		hull_height = 200
      )
      self.alias_list.pack(side = 'top', expand = 1, fill = 'both', padx = 5, pady = 5)
      if dim_name == 'directory':
         parent.balloon.bind(self.alias_list, ("This window contains the list of favorite directories.\n\nTo add a new directory, enter the directory in the entry\nwindow below followed by the <Enter> key.\n\nTo remove a directory from the list, select the\ndesired directory in this list, then select the 'Remove'\nbutton below.\n\nDouble click on any directory in the list and go directly\nthere.") )
      elif dim_name == 'mybinfiles':
         parent.balloon.bind(self.alias_list, ("This window contains the list of favorite files.\n\nTo add a new file, enter the file in the entry\nwindow below followed by the <Enter> key.\n\nTo remove a file from the list, select the\ndesired file in this list, then select the 'Remove'\nbutton below.\n\nDouble click on any file in the list and open it directly.") )
      else:
         parent.balloon.bind(self.alias_list, ("This window contains the list of alias names for\n%s.\n\nTo add new alias names, enter the name in the entry\nwindow below followed by the <Enter> key.\n\nTo remove an alias name from the list, select the\ndesired alias name in this list, then select the 'Remove'\nbutton below." % dim_name) )

      #
      # Create a group widget to contain the add new alias.
      #
      if dim_name == 'directory':
         tag_text='Add New Directory'
      elif dim_name == 'mybinfiles':
         tag_text='Add New File'
      else:
         tag_text='Add New %s Alias Name' % dim_name
      group1 = Pmw.Group(self.dialog.interior(), tag_text=tag_text)
      group1.pack(side = 'top', expand = 1, fill = 'both', padx = 5, pady = 5)
      self.new_alias = Tkinter.Entry(group1.interior(),
                background='white', 
                foreground='black',
                width = 40
      )
      self.new_alias.pack(padx = 10, pady = 10)
      if dim_name == 'directory':
         self.new_alias.insert(0, os.getcwd() )
#         self.new_alias.configure( state = 'disabled' )
         parent.balloon.bind(self.new_alias, "Enter new directory, followed by the <Enter> key.")
      elif dim_name == 'mybinfiles':
         a=parent.panelSV.tin3.get() # get the file name
         if a != 'Type in a file name or select file using pull down arrow -->.':
         
            if a[:7]!='http://': # add the path if not dods stuff
               a=os.path.join(os.getcwd(),a)
            self.new_alias.insert(0, a )
#         self.new_alias.configure( state = 'disabled' )
         parent.balloon.bind(self.new_alias, "Enter new file, followed by the <Enter> key.")
      else:
         parent.balloon.bind(self.new_alias, "Enter new alias name, followed by the <Enter> key.")
      self.new_alias.bind( "<Key>", gui_control.Command(self.evt_change_alias_entry_color, parent) )
      self.new_alias.bind( "<Return>", gui_control.Command(self.enter_new_alias, dim_name) )

      if dim_name == 'directory':
         btn_text = 'Add Directory'
      if dim_name == 'mybinfiles':
         btn_text = 'Add File'
      else:
         btn_text = 'Add ' + string.capitalize(dim_name)
      btn1 = Tkinter.Button( group1.interior(),
                text = btn_text,
                background = gui_color.three,
                command = gui_control.Command(self.enter_new_alias, dim_name)
                            )
      btn1.pack( expand = 1, fill = 'both', padx = 190, pady = 5 )
      if dim_name == 'directory':
         parent.balloon.bind(btn1, "Add directory to directory list.")
      elif dim_name == 'mybinfiles':
         parent.balloon.bind(btn1, "Add file to file list.")
      else:
         parent.balloon.bind(btn1, "Add %s alias to list." % dim_name)

      if dim_name == 'directory':
         btn2 = Tkinter.Button( group1.interior(),
                text = "Goto Selected Directory",
                background = gui_color.two,
                command = gui_control.Command(self.goto_directory, parent )
                            )
         btn2.pack( expand = 1, fill = 'both', padx = 190, pady = 5 )
         parent.balloon.bind(btn1, "Goto the selected directory.")
         
      if dim_name == 'mybinfiles':
         btn2 = Tkinter.Button( group1.interior(),
                text = "Open Selected File",
                background = gui_color.two,
                command = gui_control.Command(self.open_file, parent )
                            )
         btn2.pack( expand = 1, fill = 'both', padx = 190, pady = 5 )
         parent.balloon.bind(btn1, "Open the selected file.")

      #
      # Create a group widget to contain the removal of dimension alias.
      #
      if dim_name == 'directory':
         tag_text='Remove Directory from List'
      elif dim_name == 'mybinfiles':
         tag_text='Remove File from List'
      else:
         tag_text='Remove Alias Name from %s List' % dim_name
      group2 = Pmw.Group(self.dialog.interior(), tag_text=tag_text)
      group2.pack(side = 'top', expand = 1, fill = 'both', padx = 5, pady = 5)
      self.remove_btn = Tkinter.Button(group2.interior(),
                text = tag_text,
                background = 'red',
                command = gui_control.Command(self.remove_alias, dim_name)
      )
      self.remove_btn.pack(padx = 10, pady = 10)
      if dim_name == 'directory':
         parent.balloon.bind(self.remove_btn, "Remove the directory from the directory list by first\nselecting the directory in the list above, then\nselect this removal button.")
      if dim_name == 'mybinfiles':
         parent.balloon.bind(self.remove_btn, "Remove the file from the files list by first\nselecting the file in the list above, then\nselect this removal button.")
      else:
         parent.balloon.bind(self.remove_btn, "Remove the alias name from the alias list by first\nselecting the alias name in the list above, then\nselect this removal button.")

      # Position dialog popup
      parent_geom = parent.geometry()
      geom = string.split(parent_geom, '+')
      d1 = string.atoi( geom[1] )
      d2 = string.atoi( geom[2] )
      self.dialog.geometry( "+%d+%d" % (d1, d2) )

   def evt_change_alias_entry_color( self, parent, event ):
      keycolor = Pmw.Color.changebrightness(parent, 'red', 0.85)
      self.new_alias.configure( background = keycolor )

   #---------------------------------------------------------------------------------
   # Add new dimension alias name to the dimension alias list
   #---------------------------------------------------------------------------------
   def enter_new_alias( self, dim_name, event=None ):
      self.new_alias.configure( background = 'white' )
      new_alias_name = self.new_alias.get()
      d_name = string.lower( dim_name )
      if dim_name == "directory":
         if os.access( new_alias_name, 0 ) == 0:
            gui_message.error('This directory does not exist!')
            return
         new_alias_name = string.strip( new_alias_name )
         if new_alias_name[-1] == '/': new_alias_name = new_alias_name[:-1]
      elif dim_name=="mybinfiles":
         if new_alias_name[:7]!='http://':
            if os.access( new_alias_name, 0 ) == 0:
               gui_message.error('This file does not exist!')
               return
         else:
            try:
               f=cdms.open(new_alias_name)
               f.close()
            except:
               gui_message.error('This DODS file is not accessible! DODS server down ?')
               return
      if check_new_alias_name( new_alias_name, dim_name ) == 0:
         if dim_name not in[ "directory",'mybinfiles']:
            self.new_alias.delete(0, len(new_alias_name))# clear entry widget
         return

      # Store new alias name
      if d_name == 'longitude':
         cdms.axis.longitude_aliases.append( new_alias_name )
      elif d_name == 'latitude':
         cdms.axis.latitude_aliases.append( new_alias_name )
      elif d_name == 'time':
         cdms.axis.time_aliases.append( new_alias_name )
      elif d_name == 'level':
         cdms.axis.level_aliases.append( new_alias_name )
      elif d_name == 'directory':
      	gui_control.favorite_directories.append( new_alias_name )
      elif d_name == 'mybinfiles':
      	gui_control.favorite_files.append( new_alias_name )

      # Update the alias list widget
      ret_alias_list = return_alias_list( dim_name )
      self.alias_list.setlist( ret_alias_list )

      self.alias_list.see( len(ret_alias_list) )   # view last entry

      # Update VCDAT's alias list
      if d_name not in [ 'directory','mybinfiles']:
         self.new_alias.delete(0, len(new_alias_name))# clear entry widget
         update_vcdat_alias_list()
      elif d_name in [ 'directory','mybinfiles']:
         record_bookmarks( d_name )

   #---------------------------------------------------------------------------------
   # Goto the selected directory
   #---------------------------------------------------------------------------------
   def goto_directory( self, parent ):
      try:
         d = self.alias_list.getcurselection()[0]
         parent.panelSV.tin2.setentry( d )
         gui_select_variable.evt_enter_directory( parent, None )
         self.dialog.destroy()
         parent.panelSV.tin2.component( 'entry' ).focus_set()
      except:
         pass

   #---------------------------------------------------------------------------------
   # Open the selected file
   #---------------------------------------------------------------------------------
   def open_file( self, parent ):
      try:
         d = self.alias_list.getcurselection()[0]
         if (d[:7]!='http://'):
            sp=os.path.split(d)
            d = os.path.join(sp[:-1])
            parent.panelSV.tin2.setentry( d )
            gui_select_variable.evt_enter_directory( parent, None )
            d=sp[-1]
         elif cdat_info.CDMS_INCLUDE_DODS == 'yes':
            parent.panelSV.tin2.setentry( '' )
            parent.panelSV.tin3.clear( )
         parent.panelSV.tin3.setentry( d )
         gui_select_variable.evt_enter_file(parent, None)
         self.dialog.destroy()
         parent.panelSV.tin3.component( 'entry' ).focus_set()
      except Exception, err:
         pass

   #---------------------------------------------------------------------------------
   # Remove the alias name from the list
   #---------------------------------------------------------------------------------
   def remove_alias( self, dim_name ):
      try:
         selected_alias = self.alias_list.getcurselection()[0]
         d_name = string.lower( dim_name )
      except:
         return
      d_name = string.lower( dim_name )

      if d_name == 'longitude':
         cdms.axis.longitude_aliases.remove( selected_alias )
      elif d_name == 'latitude':
         cdms.axis.latitude_aliases.remove( selected_alias )
      elif d_name == 'time':
         cdms.axis.time_aliases.remove( selected_alias )
      elif d_name == 'level':
         cdms.axis.level_aliases.remove( selected_alias )
      elif d_name == 'directory':
         gui_control.favorite_directories.remove( selected_alias )
      elif d_name == 'mybinfiles':
         gui_control.favorite_files.remove( selected_alias )

      # Update the alias list widget
      ret_alias_list = return_alias_list( dim_name )
      self.alias_list.setlist( ret_alias_list )

      # Update VCDAT's alias list
      if d_name not in [ 'directory','mybinfiles']:
         update_vcdat_alias_list()
      else:
         record_bookmarks( d_name )

#---------------------------------------------------------------------------------
# Return the dimension alias list.
#---------------------------------------------------------------------------------
def return_alias_list( dim_name ):
   d_name = string.lower( dim_name )
   if d_name == 'longitude':
      return cdms.axis.longitude_aliases
   elif d_name == 'latitude':
      return cdms.axis.latitude_aliases
   elif d_name == 'time':
      return cdms.axis.time_aliases
   elif d_name == 'level':
      return cdms.axis.level_aliases
   elif d_name == 'directory':
      gui_control.favorite_directories.sort()
      if gui_control.favorite_directories == []:
         gui_control.favorite_index = None
      else:
         gui_control.favorite_index = 0
      return gui_control.favorite_directories
   elif d_name == 'mybinfiles':
      gui_control.favorite_files.sort()
      if gui_control.favorite_files == []:
         gui_control.favorite_files_index = None
      else:
         gui_control.favorite_files_index = 0
      return gui_control.favorite_files

#---------------------------------------------------------------------------------
# Return a flag that indicates whether or not to add to the
# new alias list. A return of 0 means don't add to the alias
# list. A return of 1 means add name to dimension alias list.
#---------------------------------------------------------------------------------
def check_new_alias_name( alias_name, dim_name ):
   if (len(alias_name) < 1): return 0 # check for blank

   # Check for name in alias list
   if dim_name not in [ 'directory','mybinfiles']:
      lower_alias_name = string.lower( alias_name )
   else:
      lower_alias_name = alias_name
   ret_alias_list = return_alias_list( dim_name )
   if lower_alias_name in ret_alias_list:
        return 0

   return 1

#---------------------------------------------------------------------------------
# Update VCDAT's alias list
#---------------------------------------------------------------------------------
def update_vcdat_alias_list():
      gui_control.longitude_alias = ['longitude'] + cdms.axis.longitude_aliases
      gui_control.latitude_alias  = ['latitude'] + cdms.axis.latitude_aliases
      gui_control.level_alias     = ['level'] + cdms.axis.level_aliases
      gui_control.time_alias      = ['time'] + cdms.axis.time_aliases

#---------------------------------------------------------------------------------
# Create New file containing bookmarks for favorite directories
#---------------------------------------------------------------------------------
def record_bookmarks( dim_name ):
   if dim_name in [ 'directory','mybinfiles']:
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
   
      # Start recording favorite directory script file
      if dim_name=='directory':
         file_name = fn + '/bookmark_directory_files.py'
         fp_record_b = open( file_name, 'w')
         for i in range( len( gui_control.favorite_directories ) ):
            fp_record_b.write( gui_control.favorite_directories[i] + '\n' )
         fp_record_b.close()
      elif dim_name=='mybinfiles':
         file_name = fn + '/bookmark_files.py'
         fp_record_b = open( file_name, 'w')
         for i in range( len( gui_control.favorite_files ) ):
            fp_record_b.write( gui_control.favorite_files[i] + '\n' )
         fp_record_b.close()
         
   

#---------------------------------------------------------------------
# End of File
#---------------------------------------------------------------------
