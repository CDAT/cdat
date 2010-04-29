#!/usr/bin/env python
#
# The PCMDI Data Browser Extend Toplevel Menus -  gui_extend_menus module
#
#################################################################################
#                                                                               #
# Module:       gui_extend_menus module                                         #
#                                                                               #
# Copyright:    "See file Legal.htm for copyright information."                 #
#                                                                               #
# Authors:      PCMDI Software Team                  			        #
#               Lawrence Livermore National Laboratory:                         #
#               support@pcmdi.llnl.gov                                          #
#                                                                               #
# Description:  PCMDI Software System browser Extend Toplevel Menus.            #
#                                                                               #
# Version:      4.0                                                             #
#                                                                               #
#################################################################################
#
#---------------------------------------------------------------------
# NOTE: need to use version of Python that imports Tkinter and Pmw
#---------------------------------------------------------------------
import Tkinter, Pmw, tkFileDialog
import __main__, os, sys, types, string
import gui_select_variable
from gui_support import gui_color
from glob import glob
import gui_control
import gui_message
import gui_saved_settings
import gui_defined_variables

# Get the previously saved state of the GUI
try:
   fn = '%s/PCMDI_GRAPHICS' % os.environ['HOME']
   sys.path.append(fn)
   import user_menus
except:
   pass

#---------------------------------------------------------------------
# NOTE: Key dictionaries and lists for save GUI-state software
#---------------------------------------------------------------------
#
# l_menu -- list of added menus, each element a m_dict dictionary
#           [ m_dict, m_dict, m_dict ]
#
# m_dict -- dictionary containing description of a menu
#           { m_nam  : 'abc',    -- the name of the menu
#             m_info : 'abc',    -- the balloon descriptive
#             m_items: l_item,   -- menu item list (in order)
#           }
#
# l_item -- list of items, each element a i_dict dictionary
#           [ i_dict, i_dict, i_dict ]
#
# i_dict -- dictionary containing description of a menu item
#           { i_nam: 'item1',  -- the name of the item
#             i_dir: '/a/b/c', -- directory name
#             i_file: 'my.py', -- file name in directory
#             i_fun: 'fun1'    -- function name in file
#           }
#---------------------------------------------------------------------
l_menu = []


#---------------------------------------------------------------------
#
# Start of Popup Dialog
#
#---------------------------------------------------------------------------
# Extend Toplevel Menus Dialog Popup
#---------------------------------------------------------------------------
#
class create:
   def __init__(self, parent):
      self.parent = parent
      dialog = Pmw.Dialog( parent,
         title = 'User menus',
         buttons = ( 'Close', ),
         defaultbutton = 'Close' )

      current_directory = os.getcwd()
      # Hide 'Manage user menus' window until all drawing is complete
      dialog.withdraw()

      dialog.unbind( "<Return>" )
      if parent.menu.popup_window_settings_flg == 1:
         dialog.transient( self.parent ) # Keep widget on top of its parent

      self.directory = os.environ['HOME']

      # Generate a list of user defined menu names
      self.menus = []
      for i in range(len(l_menu)):
          self.menus.append(l_menu[i]['m_nam'])
      
      # Initialize the list of functions for selected menu
      self.functions = []

      # Set mode of getting functions to file
      self.mode = 'File'

      # Setup groups for dialog window
      group1 = Pmw.Group(dialog.interior(),
                        tag_text = 'Create user menu:',
                        tag_font = ('Times', 14, 'bold'),
                        tagindent = 15)
      group2 = Pmw.Group(dialog.interior(),
                        tag_text = 'Modify user menu:',
                        tag_font = ('Times', 14, 'bold'),
                        tagindent = 15)
      group3 = Pmw.Group(dialog.interior(),
                        tag_text = 'Delete user menu:',
                        tag_font = ('Times', 14, 'bold'),
                        tagindent = 15)

      # Populate group1
      self.eny1 = Pmw.EntryField(group1.interior(), 
                        labelpos = 'w', 
                        label_text = 'Menu name:', 
                        entry_background = 'white',
                        entry_foreground = 'black',
                        validate = 'alphanumeric') 
      self.eny1.component('entry').bind('<Return>',gui_control.Command(self.add_top_menu,parent))
      help_message = wrap_balloon_help("Enter a new menu name. Use only alphanumeric characters. Press <Enter> or click on the 'Add' button to add the new menu to the main menu panel.")
      parent.balloon.bind(self.eny1,help_message)
      self.eny1.pack(expand = 1,fill='x',padx=10,pady = 1)
      self.eny2 = Pmw.EntryField(group1.interior(),
                     labelpos = 'w',
                     label_text = 'Description:',
                     entry_background = 'white',
                     entry_foreground = 'black')
      self.eny2.pack( expand = 1,fill='x', padx=10,pady=1)
      self.eny2.component('entry').bind('<Return>',gui_control.Command(self.add_top_menu,parent))
      help_message = wrap_balloon_help("(Optional) Enter a description to be displayed as part of the ballon help for the new menu. This message will be displayed when the user rests the mouse pointer over the menu name.")
      parent.balloon.bind(self.eny2,help_message) 
      labels = (self.eny1, self.eny2)
      Pmw.alignlabels(labels)

      tempframe = Tkinter.Frame(group1.interior())
      btn1 = Tkinter.Button(tempframe,
	   			text = 'Clear',
				command = gui_control.Command(self.clear_top, parent))
      btn1.pack( side = 'left', expand = 0, fill = 'x', padx = 10, pady = 5 )
      help_message = wrap_balloon_help("Clear menu name and description fields.")
      parent.balloon.bind(btn1,help_message)
	  
      btn2 = Tkinter.Button(tempframe, 
                text = 'Add',
                default = 'active',
                command = gui_control.Command(self.add_top_menu, parent))
      btn2.pack( side = 'left', expand = 0, fill = 'x', padx = 10)
      help_message = wrap_balloon_help("Add menu to main menu panel.")
      parent.balloon.bind(btn2,help_message)
      tempframe.pack()
      group1.pack(fill = 'x', anchor = 'nw', expand = 1, padx = 10, pady = 10)

      # Populate group2 
      self.eny3 = Pmw.ComboBox(group2.interior(),
                                labelpos = 'w',
                                label_text = 'Menu name:',
                                entry_background = 'white',
                                entry_foreground = 'black',
                                dropdown = 1,
                                selectioncommand = gui_control.Command(self.populate_group2),
                                scrolledlist_items = self.menus)
      self.eny3.pack(side = 'top',expand = 1, fill = 'x', padx=10, pady = 1)
      help_message = wrap_balloon_help("Select a menu to modify by clicking on the pull down menu or typing in the menu name by hand and pressing <Enter>.")
      parent.balloon.bind(self.eny3,help_message)
      tempframe = Tkinter.Frame(group2.interior())
      self.eny4 = Pmw.EntryField(tempframe,
                                labelpos = 'w',
                                label_text = 'Description:',
                                entry_background = 'white',
                                entry_foreground = 'black')
      self.eny4.component('entry').bind('<Return>',gui_control.Command(self.change_description,parent))
      help_message = wrap_balloon_help('Enter a new description and press <Enter> or click on the \"Change\" button to change the help balloon associated with the selected menu.')
      parent.balloon.bind(self.eny4,help_message)
      self.eny4.pack(side = 'left', expand = 1, fill = 'x', padx = 10)
      btn3 = Tkinter.Button(tempframe,
                                text = 'Change',
                                command = gui_control.Command(self.change_description,parent))
      btn3.pack(side = 'left',padx = 10)
      help_message = wrap_balloon_help('Change help balloon associated with the selected menu.')
      parent.balloon.bind(btn3,help_message)
      tempframe.pack(expand = 1, fill = 'x',pady = 1)

      labels2 = (self.eny3, self.eny4)
      Pmw.alignlabels(labels2)

      # Setup group2a
      self.group2a = Pmw.Group(group2.interior(),
                                tag_text = 'Add function to menu from file:',
                                tag_font = ('Times', 14),
                                tagindent = 10)
      tempframe = Tkinter.Frame(self.group2a.interior())
      self.eny5_file = Pmw.EntryField(tempframe,
                                labelpos = 'w',
                                label_pyclass = Pmw.OptionMenu,
                                label_items = gui_control.dirimpchlst,
                                label_command = gui_control.Command(self.file_or_import,parent),
                                label_initialitem = 'File',
                                entry_background = 'white',
                                entry_foreground = 'black')
      self.eny5_file.component('entry').bind('<Return>',gui_control.Command(self.get_functions))
      self.eny5_file.pack(side = 'left', fill = 'x',expand = 1, padx = 10)
      help_message = wrap_balloon_help('Select a python file to add functions from by typing in the name of the file and pressing <Enter>, or click on the \'Browse\' button and search for your file. If you enter the file name by hand and don\'t specify the full path, the file location is relative to your home directory.') 
      parent.balloon.bind(self.eny5_file,help_message)
      self.eny5_import = Pmw.ComboBox(tempframe,
                                labelpos = 'w',
                                label_pyclass = Pmw.OptionMenu,
                                label_items = gui_control.dirimpchlst,
                                label_command = gui_control.Command(self.file_or_import,parent),
                                selectioncommand = gui_control.Command(self.get_functions),
                                entry_background = 'white',
                                entry_foreground = 'black')
      help_message = wrap_balloon_help('Select a module to import functions from by clicking on the pull down menu or entering the module name by hand and pressing <Enter>.')
      parent.balloon.bind(self.eny5_import,help_message)
      self.btn4 = Tkinter.Button(tempframe,
                                text = 'Browse',
                                command = gui_control.Command(self.browse_files,parent))
      self.btn4.pack(side = 'left',padx = 10)
      help_message = wrap_balloon_help('Select a python file to import functions from.')
      parent.balloon.bind(self.btn4,help_message)
      tempframe.pack(side = 'top',anchor = 'w',expand = 1,fill = 'x', pady = 1)
      
      self.eny6 = Pmw.ComboBox(self.group2a.interior(),
                                labelpos = 'w',
                                label_text = 'Function:',
                                entry_background = 'white',
                                entry_foreground = 'black',
                                entry_width = 40,
                                selectioncommand = gui_control.Command(self.select_function),
                                dropdown = 1)
      self.eny6.pack(side = 'top', expand = 1, fill = 'x', padx = 10, pady = 1)

      help_message = wrap_balloon_help('Select a function name to add by clicking on the pull down menu.')
      parent.balloon.bind(self.eny6,help_message)
      tempframe = Tkinter.Frame(self.group2a.interior())
      ## checkbox to have the function wrapped as a plotting extension
      self.eny7b = Pmw.RadioSelect(tempframe,
                                   buttontype = 'checkbutton',
                                   labelpos = 'w',
                                   label_text = 'This function plots onto the VCS canvas',
                                   )
      self.eny7b.pack(side='left',expand=1,fill='x',padx=10)
      self.eny7b.add("")
      help_message = wrap_balloon_help('If this is checked you can then drop defined variable in the display lines to redraw with new variable, and use different templates')
      parent.balloon.bind(self.eny7b,help_message)
      tempframe.pack(side = 'top',anchor = 'w', fill = 'x', expand = 1, pady = 1) 
      tempframe = Tkinter.Frame(self.group2a.interior())
      self.eny7 = Pmw.EntryField(tempframe,
                                labelpos = 'w',
                                label_text = 'Function name:',
                                entry_background = 'white',
                                entry_foreground = 'black')
      self.eny7.component('entry').bind('<Return>',gui_control.Command(self.add_function,parent))
      self.eny7.pack(side = 'left',expand = 1, fill = 'x', padx = 10)
      help_message = wrap_balloon_help('Enter the name of the function to be added to the selected menu.')
      parent.balloon.bind(self.eny7,help_message)

      

      
      btn5 = Tkinter.Button(tempframe,
                                text = 'Add',
                                command = gui_control.Command(self.add_function,parent))
      btn5.pack(side = 'left', padx = 10)
      help_message = wrap_balloon_help('Add function to selected menu.')
      parent.balloon.bind(btn5,help_message)
      tempframe.pack(side = 'top',anchor = 'w', fill = 'x', expand = 1, pady = 1) 
      labels = (self.eny5_file, self.eny6, self.eny7)
      Pmw.alignlabels(labels)

      self.group2a.pack(side = 'top',expand = 1, fill = 'x',padx = 10, pady = 5)

      # Setup group2b
      group2b = Pmw.Group(group2.interior(),
                                tag_text = 'Delete or Rename function:',
                                tag_font = ('Times', 14),
                                tagindent = 10)
      tempframe = Tkinter.Frame(group2b.interior())
      self.eny8 = Pmw.ComboBox(tempframe,
                                labelpos = 'w',
                                label_text = 'Function name:',
                                entry_background = 'white',
                                entry_foreground = 'black',
                                entry_state = 'disabled',
                                dropdown = 1)
      self.eny8.component('entry').bind('<KeyPress>',gui_control.Command(self.do_nothing))
      self.eny8.pack(side = 'left', expand = 1, fill = 'x', padx = 10)
      help_message = wrap_balloon_help('Select function name by clicking on the pull down menu.')
      parent.balloon.bind(self.eny8,help_message)
      btn7 = Tkinter.Button(tempframe,
                                text = 'Delete',
                                command  = gui_control.Command(self.delete_function,parent))
      btn7.pack(side = 'left',padx = 10)
      help_message = wrap_balloon_help('Delete function from menu.')
      parent.balloon.bind(btn7,help_message)
      tempframe.pack(side = 'top',fill = 'x', expand = 1,pady=1)

      tempframe = Tkinter.Frame(group2b.interior())
      self.eny8a = Pmw.EntryField(tempframe,
                                labelpos = 'w',
                                label_text = 'New name:',
                                entry_background = 'white',
                                entry_foreground = 'black')
      self.eny8a.component('entry').bind('<Return>',gui_control.Command(self.rename))
      self.eny8a.pack(side = 'left', expand = 1, fill = 'x', padx = 10)
      help_message = wrap_balloon_help('Change function name by typing in new function name and pressing <Enter> or clicking on the \'Delete\' button.')
      parent.balloon.bind(self.eny8a,help_message)
      btn6 = Tkinter.Button(tempframe,
                                text = 'Rename',
                                command  = gui_control.Command(self.rename))
      btn6.pack(side = 'left',padx = 10)
      help_message = wrap_balloon_help('Rename function.')
      parent.balloon.bind(btn6,help_message)
      labels = (self.eny8, self.eny8a)
      Pmw.alignlabels(labels)
      tempframe.pack(side = 'top',fill = 'x', expand = 1,pady=1)
      
      group2b.pack(side = 'top',expand = 1, fill = 'x',padx = 10,pady=10)

      group2.pack( fill = 'x', anchor = 'nw', expand = 1, padx = 10, pady = 5)

      # Populate group3
      self.eny9 = Pmw.ComboBox(group3.interior(),
                                labelpos = 'w',
                                label_text = "Menu name:",
                                entry_background = 'white',
                                entry_foreground = 'black',
                                scrolledlist_items = self.menus,
                                dropdown = 1)
      self.eny9.pack(side = 'left', expand = 1, fill = 'x', padx = 10)
      help_message = wrap_balloon_help('Select a menu to delete by clicking on the pull down menu.')
      parent.balloon.bind(self.eny9,help_message)
      btn8 = Tkinter.Button(group3.interior(),
                                text = 'Delete',
                                command = gui_control.Command(self.del_top_menu,parent))
      btn8.pack(side = 'left',padx = 10)
      help_message = wrap_balloon_help('Delete menu from main menu bar.')
      parent.balloon.bind(btn8,help_message)
      group3.pack(side = 'top',anchor = 'nw',expand = 1, fill = 'x', padx = 10, pady = 15, ipady = 2 )

      # Position dialog popup on top of the main GUI
      parent_geom = self.parent.geometry()
      geom = string.split(parent_geom, '+')
      d1 = string.atoi( geom[1] )
      d2 = string.atoi( geom[2] ) + 40

#      dialog.activate(globalMode = 'nograb' ,geometry = 'centerscreenalways')
      dialog.geometry( "+%d+%d" % (d1, d2) )
      dialog.show()

   # End of create function


   #--------------------------------------------------------------
   # Use this function to "lock" a field (bind <KeyPress> to do_nothing)
   #--------------------------------------------------------------
   def do_nothing(self,result):
      return "break"

   #--------------------------------------------------------------
   # Get functions from file or import and populate the function
   #  pull down menu
   #--------------------------------------------------------------
   def get_functions(self,result = None):
      self.func_list = []
      self.eny6.clear()
      self.eny7.clear()
      if self.mode == 'File':
          filename = self.eny5_file.get()
          if len(filename) == 0:
              return
          # This allows for relative pathnames (relative from users home dir)
          if filename[0] != '/':
              self.menus_directory = os.environ['HOME']
          else:
              self.menus_directory = os.path.dirname(filename)
          f = os.path.basename(string.split(filename,'.')[0])
         
          # Check to make sure path and file exists. Catch early to avoid 
          #  problems later
          if (os.path.isfile('%s/%s.py'%(self.menus_directory,f))) != 1:
              gui_message.error('The Python file %s/%s.py does not exist'%(self.menus_directory,f))
              return
          try:
             current_directory = os.getcwd()
             os.chdir(self.menus_directory)
             exec "import %s"%f
             a = eval("dir(%s)"%f) 
             a.sort()
             for func in a:
                 b = eval("type( %s.%s )"%(f,func))
                 if (b == types.FunctionType):
                     self.func_list.append(func)
             if len(self.func_list) == 0:
                 gui_message.error('Could not find any functions in file %s/%s.py'%(self.menus_directory,f))
                 self.eny5_file.clear()
                 os.chdir(current_directory)
                 return
             os.chdir(current_directory)
          except:
             gui_message.error('Check the Python file for errors. Or check for valid file or directory.')
             self.eny5_file.clear()
             os.chdir(current_directory)
             return
      else:
          self.func_list = returned_functions_and_instances(self)
      # Populate function list and function name with first returned function
      if len(self.func_list) > 0:
          self.eny6.setlist(self.func_list)
          self.eny6.setentry(self.func_list[0])
          self.eny7.setentry(self.func_list[0])
      
   #---------------------------------------------
   # Toggle between file and import mode.  Change the appearance of 
   #  group2 and group2a appropriately
   #---------------------------------------------
   def file_or_import(self,parent,result):
      if self.mode != result:
          self.eny5_file.clear()
          self.eny5_import.clear()
          self.eny6.clear()
          self.eny7.clear()
          # Set mode of function aquisition
          self.mode = result
          if result == 'Import':
              self.group2a.component('tag').configure(text = 'Add function to menu from import module')
              self.eny5_file.pack_forget()
              self.eny5_import.pack(side = 'left', before = self.btn4, fill = 'x',expand = 1, padx = 10)
              labels = (self.eny5_import, self.eny6, self.eny7)
              Pmw.alignlabels(labels)
              self.eny5_import.component('label').component('menubutton').configure(text = 'Import')
              self.btn4.configure(state = 'disabled')
              self.import_functions()
          else:
              self.group2a.component('tag').configure(text = 'Add function to menu from file')
              self.eny5_import.pack_forget()
              self.eny5_file.pack(side = 'left', before = self.btn4, fill = 'x',expand = 1, padx = 10)
              labels = (self.eny5_import, self.eny6, self.eny7)
              Pmw.alignlabels(labels)
              self.eny5_file.component('label').component('menubutton').configure(text = 'File')
              self.btn4.configure(state = 'normal')
       

   #---------------------------------------------
   # Change the value of "Function name" field to value of "Function"
   #---------------------------------------------
   def select_function(self,parent):
      func_name = self.eny6.get()
      self.eny7.setentry(func_name)

   #---------------------------------------------
   # Get functions from an import
   #---------------------------------------------
   def import_functions(self):
         self.modules = find_modules()
         if len(self.modules) == 0:
            return
         self.eny5_import.setlist(self.modules)
         self.eny5_import.setentry(self.modules[0])
         functions = returned_functions_and_instances( self )
         if len(functions) == 0:
            self.eny6.clear()
            self.eny7.clear()
            return

         self.eny6.setlist(functions)
         self.eny6.setentry(functions[0])
         self.eny7.setentry(functions[0])
       
   #---------------------------------------------
   # Rename a function
   #---------------------------------------------
   def rename(self,evt = None):
      global l_menu
      menu_name = self.eny3.get()
      function_name = self.eny8.get()
      new_name = self.eny8a.get() 
      # If user tried to change the name but did not enter a new name, return
      if len(new_name) == 0:
          return
      else:
          for i in range(len(l_menu)):
              if l_menu[i]['m_nam'] == menu_name:
                  for j in range(len(l_menu[i]['m_items'])):
                      # Check to see if function name already exists
                      if l_menu[i]['m_items'][j]['i_nam'] == new_name:
                          gui_message.error('Function name already exists')
                          return
                      if l_menu[i]['m_items'][j]['i_nam'] == function_name:
                          try:
                              old_filename = '%s/PCMDI_GRAPHICS/%s_%s.py'%(os.environ['HOME'],l_menu[i]['m_nam'],function_name)
                              new_filename = '%s/%s_%s.py'%(os.path.dirname(old_filename),l_menu[i]['m_nam'],new_name)
                              infile = open(old_filename,'r')
                              outfile = open(new_filename,'w')
                              a = infile.read().split('\n')
                              # Change function wrapper to use new name
                              a[0] = 'def %s( item_name, func_dir ):'%new_name
                              for k in range(len(a)):
                                  outfile.write('%s\n'%a[k]) 
                              outfile.close()
                              # Remove old files
                              try:
                                  os.remove(old_filename)
                              except:
                                  pass

                              try:
                                  os.remove('%so'%old_filename)
                              except:
                                  pass

                              l_menu[i]['m_items'][j]['i_nam'] = new_name 
                              index = self.functions.index(function_name)
                              self.functions[index] = new_name 
                              # Dean will add code here to rename a function
                              #  and change the command of that function to
                              #  the new wrapped py file
                              self.eny8.setlist(self.functions)
                              self.eny8.setentry(new_name)
                          except os.error, values:
                              gui_message.error( 'Could not rename file: %s'%values[1])
                              return
                          break
                  break  
          gui_saved_settings.save_menus()

   #---------------------------------------------
   # When user selects a menu name from the "Modify user menu" window, 
   #  populate all appropriate fields with current menu information
   #---------------------------------------------
   def populate_group2(self,result):
      global l_menu
      nam = self.eny3.get()
      self.functions = []

      for i in range(len(l_menu)):
          if l_menu[i]['m_nam'] == nam:
              # We found our menu, populate description
              self.eny4.setentry(l_menu[i]['m_info'])
              items = l_menu[i]['m_items']
              # Add function names to our list of functions
              for j in range(len(items)):
                  self.functions.append(items[j]['i_nam'])
                  j += 1
              break

      self.eny8.clear()
      if len(self.functions) > 0:
          self.eny8.setlist(self.functions)

   #---------------------------------------------
   # Add a new menu to the main menu bar
   #---------------------------------------------
   def add_top_menu( self, parent, evt = None ):
      global l_menu
      addmenu = 1
      a = parent.menu.main_menu
      nam = self.eny1.get()
      inf = self.eny2.get()

      if len(nam) == 0:
         gui_message.error( ('Please enter a new menu name') )
         return

      for i in range( len(l_menu) ):
         x = l_menu[i]
         # Add check here to make sure menu is not already defined as 
         #   a main menu
         if x[ 'm_nam' ] == nam:
            addmenu = 0
            gui_message.error( ('Menu name already exists.\nPlease choose another menu name.'))
            break
       
      
      # Update main menu and "Menu name" pull down menu. Update
      #  all appropriate fields
      if addmenu:
              men7 = a.addmenu( nam, inf, tearoff = 1 ) 
              m_dict = { 'm_nam' : nam, 'm_info' : inf, 'm_items' : [] } 
              l_menu.append( m_dict )
              self.menus.append(nam)
              self.eny3.setlist(self.menus)
              self.eny3.setentry(nam)
              self.eny4.setentry(inf)
              self.eny9.clear()
              self.eny9.setlist(self.menus)
              gui_saved_settings.save_menus()  
              self.eny1.clear()
              self.eny2.clear()
              self.eny5_file.clear()
              self.eny5_import.clear()
              self.eny6.clear()
              self.eny7.clear()
              self.eny8.clear()

   #---------------------------------------------
   # Change menu description
   #---------------------------------------------
   def change_description(self,parent,evt = None):
        global l_menu
        a = parent.menu.main_menu
        nam = self.eny3.get()
        descrip = self.eny4.get()
        
        if len(nam) == 0:
            return

        # Freakin' yeah, this was tough enough to figure out  =)
        parent.balloon.bind(parent.menu.main_menu.component('%s-button'%nam),descrip)
        for i in range(len(l_menu)):
            if l_menu[i]['m_nam'] == nam:
                l_menu[i]['m_info'] = descrip
                gui_saved_settings.save_menus()
                break

   #---------------------------------------------
   # Clear "Menu name" and "Description" from the "Create user menu" window
   #---------------------------------------------
   def clear_top(self,parent):
        self.eny1.clear()
        self.eny2.clear()

   #---------------------------------------------
   # Remove a menu from the main menu bar
   #---------------------------------------------
   def del_top_menu(self,parent):
        global l_menu

        a = parent.menu.main_menu
        nam = self.eny9.get()
        if len(nam) == 0:
            return
        if not (gui_message.ask('Do you really want to\ndelete menu [%s]?'%nam)):
            return

        for i in range(len(l_menu)): 
             if l_menu[i]['m_nam'] == nam:
                 a.deletemenu(nam)
                 del l_menu[i]

                 #--------------------------------------
                 # Remove files from ~/PCMDI_GRAPHICS
                 #--------------------------------------
                 menuname = glob(os.environ['HOME']+'/PCMDI_GRAPHICS/'+nam+'.py')
                 if menuname:
                     os.remove(menuname)
                 menuitems = glob(os.environ['HOME']+'/PCMDI_GRAPHICS/'+nam+'_*.py*')
                 for j in range(len(menuitems)):
                     os.remove(menuitems[j])

                 for k in range(len(self.menus)):
                     if self.menus[k] == nam:
                         del self.menus[k] 
                         self.eny3.setlist(self.menus)
                         self.eny9.setlist(self.menus)
                         self.eny9.setentry('')
                         if nam == self.eny3.get():
                             self.eny3.setentry('')
                             self.functions = []
                             self.func_list = []
                             self.eny4.clear()
                             self.eny5_file.clear()
                             self.eny6.clear()
                             self.eny7.clear()
                             self.eny8.clear()
                         break
                 break
        gui_saved_settings.save_menus()

   #---------------------------------------------
   # Open a tkFileDialog window and get filename from user
   #---------------------------------------------
   def browse_files(self,parent):
        # Create a file browse window
        browse_dialog = tkFileDialog.Open(master=parent,
                            filetypes = gui_control.filetypes,
                            title = 'File Selection')
        # Select the file
        filename = browse_dialog.show(initialdir=os.environ['HOME'])
        if len(filename) > 0:
            self.eny5_file.setentry(filename)
            self.get_functions()

   #---------------------------------------------
   # Add a function to a pre-existing menu from the main menu bar
   #---------------------------------------------
   def add_function(self,parent,evt=None):
      global l_menu
      menu_name = self.eny3.get()
      function_name = self.eny6.get()
      menu_item = self.eny7.get()
      isplotting = self.eny7b.getvalue()
      
      for i in range(len(menu_item)):
          if (not menu_item[i].isalpha()) and (not menu_item[i].isdigit()) :
              if menu_item[i] != '_':
                  gui_message.error("Function can only contain letters, numbers, and '_'")
                  return
      if self.mode == 'File':
          if self.eny5_file.get()[0] != '/':
              directory = os.environ['HOME']
          else:
              directory = os.path.dirname(self.eny5_file.get())
          filename = os.path.basename(string.split(self.eny5_file.get(),'.')[0])
      else:
          directory = '-999'
          filename = self.eny5_import.get()

      # Does the main menu exist
      if len(menu_name) < 1:
          gui_message.error('Please select a menu name to add\nfunction to.')
          return

      # Make sure value is not null
      if len(function_name) < 1:
          gui_message.error('Please enter a function name')
          return

      # Check if function is already in menu
      if menu_item in self.functions:
          gui_message.error('Function name is already in use.\nPlease select another name.')
          return

      m_dict = None
      for i in range( len(l_menu) ):
         x = l_menu[i]
         if x[ 'm_nam' ] == menu_name:
            m_dict = x
            break

      if m_dict == None:
         gui_message.error( ('Menu [ %s ] does not exist' % menu_name ) )
         return

      l_item = m_dict[ 'm_items' ]

      # add item to menu
      d = os.getcwd()

      if self.mode == 'File':
          current_directory = os.getcwd()
          try:
             eval( ("os.chdir( '%s' )" % directory) )
          except:
             gui_message.error( ('Cannot access directory [ %s ].' % directory) )
             return
          eval('os.chdir(current_directory)')

      i_dict = { 'i_nam' : menu_item, 
            'i_dir' : directory, 
            'i_file' : filename, 
            'i_fun' : function_name }
      l_item.append( i_dict )

      # ------------------------------------------
      # create the wrapper function
      # ------------------------------------------
      try:
         fn = '%s/PCMDI_GRAPHICS' % os.environ['HOME']
      except:
         gui_message.error( 'Could not find the PCMDI_GRAPHICS directory.' )
         return  
      pfile_name = menu_name + '_' + i_dict['i_nam']
      file_name = fn + '/' + pfile_name + '.py'
      fp = open(file_name, 'w')
      fp.write("import sys\nsys.path.insert(0,'%s')\n" % i_dict['i_dir'])
      function = 'def ' + i_dict['i_nam'] + '( item_name, func_dir ):' + '\n'
      fp.write( function )
      fun_import = '   from %s import %s\n' % (i_dict['i_file'], i_dict['i_fun']) 
      fp.write( fun_import )
      fp.write('   import __main__\n' )
      if isplotting!=():
         fp.write("   parent = __main__.__dict__['tk_root']\n")
         fp.write("   parent.external_plot = %s\n" % i_dict['i_fun'])
         fp.write('   v = parent.panelGC.opt2.getvalue()\n')
         fp.write("   items =  parent.panelGC.opt2.cget('items')\n")
         fp.write("   items[-1]='Ext (%s)' \n" % i_dict['i_fun'])
         fp.write('   parent.panelGC.opt2.setitems(items)\n')
         fp.write('   parent.panelGC.opt2.invoke(items[-1])\n')
         fp.write('   parent.panelGC.evt_plot(parent)\n')
      else:
         fp.write('   import os\n' )
         fp.write('   import browser\n' )
         fp.write('   import cdms2\n' )
         fp.write('   import browser.gui_message\n' )
         fp.write('   import MV2\n' )
         fp.write('   import numpy\n' )
         fp.write('\n')
         fp.write('   variables = browser.gui_defined_variables.get_vars()\n')
         fp.write('\n')
         fp.write('   d=os.getcwd()\n')
         fp.write('   if func_dir != \'-999\':\n')
         fp.write('      try:\n')
         fp.write('          os.chdir( func_dir )\n')
         fp.write('      except:\n')
         fp.write('          browser.gui_message.error(\'Could not access directory\')\n')
         fp.write('   return_var = []\n\n')
         fp.write('   keep_going = 1\n')
         fp.write('   for i in range(len(variables)):\n')
         fp.write('       try:\n')
         fp.write('           return_var = apply(%s,variables[i:])\n'%i_dict['i_fun'])
         fp.write('           keep_going = 0\n')
         fp.write('           break\n')
         fp.write('       except:\n')
         fp.write('           keep_going = 1\n')
         fp.write('   if keep_going:\n')
         fp.write('       try:\n')
         fp.write('           return_var = %s()\n'%i_dict['i_fun'])
         fp.write('       except:\n')
         fp.write('           browser.gui_message.error("Error in passing variable(s) to the function")\n')
         fp.write('           return\n')
         fp.write('\n')
         fp.write('   os.chdir( d )\n')
         fp.write('\n')
         fp.write('   try:\n')
         fp.write("      new_name = variables[0].name + '_'\n")
         fp.write("      new_name = new_name + '%s_'\n" % i_dict['i_file'])
         fp.write("      new_name = new_name + item_name\n")
         fp.write('   except:\n')
         fp.write("      new_name = '%s_'\n" % i_dict['i_file'])
         fp.write("      new_name = new_name + item_name\n")
         fp.write('\n')
         fp.write('   a=dir(__main__)\n')
         fp.write('   n_name = new_name\n')
         fp.write('   i = 1\n')
         fp.write('   while n_name in a:\n')
         fp.write('      n_name=new_name+str(i)\n')
         fp.write('      i += 1\n')
         fp.write('   new_name = n_name\n')
         fp.write( '\n')
         fp.write("   if type(return_var) == type('String'):\n")
         fp.write('       browser.gui_message.info("Result: %s"%return_var)\n')
         fp.write('   else:\n')
         fp.write('       browser.gui_user_menus.user_menus_put( new_name, return_var )')
      fp.close()

      # ----------------------------------------------------------
      # point the created item to the above wrapper function
      # ----------------------------------------------------------
      try:
         exec "from %s import %s" % (pfile_name, i_dict['i_nam'])
      except Exception,err:
         m1 = ( 'Cannot access def [ %s ] in .py file [ %s ]\nError was %s'
                % (pfile_name, i_dict['i_nam'],err) )
         gui_message.error( m1 )
         return

      f = eval( i_dict['i_nam'] )

      men7 = parent.menu.main_menu.addmenuitem( menu_name, 'command', ' ',
                label = menu_item,
                command = gui_control.Command( f, i_dict['i_nam'], i_dict['i_dir'] ) )
      self.eny6.setentry('')
      self.eny7.clear()
      gui_saved_settings.save_menus()
      self.functions.append(menu_item)
      self.eny8.setlist(self.functions)

   #---------------------------------------------
   # Delete a function from the main menu bar
   #---------------------------------------------
   def delete_function(self,parent):
      global l_menu
      menu_name = self.eny3.get()
      func_name = self.eny8.get()

      if len(func_name) == 0:
          return
      
      if gui_message.ask('Do you really want to delete function [%s]?'%func_name):
          for i in range(len(l_menu)):
              if l_menu[i]['m_nam'] == menu_name:
                  functions = l_menu[i]['m_items']
                  number_of_functions = len(functions)
                  for j in range(number_of_functions):
                      if functions[j]['i_nam'] == func_name:
                          del l_menu[i]['m_items'][j]
                          index = parent.menu.main_menu.component('%s-menu'%menu_name).index(func_name)
                          parent.menu.main_menu.component('%s-menu'%menu_name).delete(index)
                          self.functions.remove(func_name)
                          try:
                              os.remove('%s/PCMDI_GRAPHICS/%s_%s.py'%(os.environ['HOME'],menu_name,func_name))
                          except:
                              pass

                          try:
                              os.remove('%s/PCMDI_GRAPHICS/%s_%s.pyo'%(os.environ['HOME'],menu_name,func_name))
                          except:
                              pass
                          self.eny8.clear()
                          if len(self.functions) > 0:
                              self.eny8.setlist(self.functions)
                          break  
                  break
      gui_saved_settings.save_menus() 
       
                  

   
                        
#---------------------------------------------------------------------
# Find the py files in the directory
#---------------------------------------------------------------------
def find_py_files( parent ):
   d = []
   f = []
   for x in os.listdir( parent.menus_directory ):
      c = "%s/%s" % (parent.menus_directory, x)
      if os.path.isdir( c ):
	 d.append(x)
      elif os.path.isfile( c ):
         if x[-3:] == '.py':
	   f.append(x)
   d.sort()
   dir = parent.menus_directory
   i = 0
   while dir != "" and dir !=  "/":
        d.insert(i, dir)
        s=string.split( dir, '/' )[-1]
        dir = dir[0:(len(dir)-len(s)-1)]
        i += 1
   d.insert(i, "/")
   d.insert(i+1, "============================================================================")
   f.sort()

   return d,f
     
#---------------------------------------------------------------------
# Return the Modules that have been imported
#---------------------------------------------------------------------
def find_modules( ):
   m = []
   for x in dir(__main__):
      if ( (type(__main__.__dict__[x]) == types.ModuleType) and
         (x not in gui_control.do_not_show_in_list) ):
          m.append( x )   
   m.sort()
   return m

#---------------------------------------------------------------------
# Return the Module's function list
#---------------------------------------------------------------------
def returned_functions_and_instances( self ):
   m = self.eny5_import.get()
   ee = []
   if len(m) == 0:
       return ee

   try:
      exec "import %s" % m
   except:
      gui_message.error('Could not import module: %s'%m)
      return ee

   a = eval ( "dir( %s )" % m )
   a.sort()
   for x in a:
       b = eval( "type( %s.%s )" % ( m, x) )
       if b in gui_control.search_function_type_list:
          ee.append( x )
   if len(ee) == 0:
       gui_message.error('No functions in module %s'%m)
       return ee
   return ee

#---------------------------------------------------------------------
# Restore Menus in the Main Menu Bar
#---------------------------------------------------------------------
def restore_menus( parent ):
   try:
      for i in range(len(user_menus.set.menu_dict)):
         success = 1
         if user_menus.set.menu_dict[i].keys()[0] == 'Menu_name':
            nam = user_menus.set.menu_dict[i].values()[0]
            inf = user_menus.set.menu_dict[i+1].values()[0]
            menu_name = parent.menu.main_menu.addmenu( nam, inf, tearoff = 1 )
            m_dict = { 'm_nam' : nam, 
                       'm_info' : inf, 
                       'm_items' : [] }
            l_menu.append( m_dict )
   
         if user_menus.set.menu_dict[i].keys()[0] == 'item_Name':
            i_nam = user_menus.set.menu_dict[i].values()[0]
            i_dir = user_menus.set.menu_dict[i+1].values()[0]
            i_fun = user_menus.set.menu_dict[i+2].values()[0]
            i_file = user_menus.set.menu_dict[i+3].values()[0]
   
            i_dict = { 'i_nam' :i_nam,
                       'i_dir' : i_dir, 
                       'i_file' : i_file,
                       'i_fun' : i_fun
                     }
            l_item = m_dict[ 'm_items' ]
            
            pfile_name = nam + '_' + i_dict['i_nam']
            if i_dir != '-999':
                current_directory = os.getcwd()
                try:
                    eval('os.chdir(i_dir)')
                except:
                    success = 0
                    gui_message.error('Could not access module directory')
                    pass
                eval('os.chdir(current_directory)')

            try:
               exec "from %s import %s" % (pfile_name, i_dict['i_nam'])
            except:
               success = 0
               m1 = ( 'Cannot access def [ %s ] in .py file [ %s ]'
                      % (i_dict['i_nam'],pfile_name) )
               gui_message.error( m1 )
               pass
   
            try:
              f = eval( i_dict['i_nam'] )
              parent.menu.main_menu.addmenuitem( nam, 'command', ' ',
                    label = i_nam,
                    command = gui_control.Command( f, i_dict['i_nam'], i_dict['i_dir'] ) )
            except:
              success = 0
              pass

            if success:
                l_item.append( i_dict )

      gui_saved_settings.save_menus()
   except:
      pass

# Make formatting of help balloons easier
def wrap_balloon_help(contents,max_width = None):
    if max_width != None:
        width = max_width
    else:
        width = gui_control.max_help_width

    words = contents.split()
    balloon_help = ''
    current_line = ''
    
    for i in range(len(words)):
        if len(current_line) < width:
            current_line += '%s '%str(words[i])
        else:
            balloon_help += '%s\n'%current_line
            current_line = '%s '%str(words[i])
    balloon_help += '%s'%current_line
    return balloon_help

#---------------------------------------------------------------------
# End of File
#---------------------------------------------------------------------
