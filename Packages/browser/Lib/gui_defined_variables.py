#!/usr/bin/env python
# Adapted for numpy/ma/cdms2 by convertcdms.py
#
# The PCMDI Data Browser Defined Variables Panel -  gui_defined_variables module
#
###############################################################################
#                                                                             #
# Module:       gui_defined_variables module                                  #
#                                                                             #
# Copyright:    "See file Legal.htm for copyright information."               #
#                                                                             #
# Authors:      PCMDI Software Team                                           #
#               Lawrence Livermore NationalLaboratory:                        #
#               support@pcmdi.llnl.gov                                        #
#                                                                             #
# Description:  PCMDI Software System browser Tkinter "Defined Variables"     #
#               panel GUI.                                                    #
#                                                                             #
# Version:      4.0                                                           #
#                                                                             #
###############################################################################
#
#---------------------------------------------------------------------
# NOTE: need to use version of Python that imports Tkinter and Pmw
#---------------------------------------------------------------------
import Tkinter, Pmw, tkFileDialog
from tkMessageBox import askquestion, showerror
import __main__, sys, os, time, string, types, cdms2, numpy
import MV2
import gui_functions
import gui_menu, vcs_function
import gui_control
import gui_alter_variable
import gui_writenetcdf
import gui_reset
from gui_support import gui_color
import gui_message
import gui_user_menus
import gui_output
import gui_expression
import genutil
import webbrowser
# List of high-level graphics methods
# Comment out the Continents for later development. It is about 70% implemented
# in VCDAT.
#graphics_method_list = ['Boxfill', 'Continents', 'Isofill', 'Isoline',
#                        'Meshfill', 'Outfill', 'Outline', 'Scatter',
#                        'Taylordiagram', 'Vector', 'XvsY', 'Xyvsy',
#                        'Yxvsx']
graphics_method_list = ['Boxfill', 'Isofill', 'Isoline',
                        'Meshfill', 'Outfill', 'Outline', 'Scatter',
                        'Taylordiagram', 'Vector', 'XvsY', 'Xyvsy',
                        'Yxvsx','Ext']

trash_str = '_waiting_for_user_to_dispose_of_via_trash_can'
trash_store = '__trashed_on__'
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
#---------------------------------------------------------------------
# Begin the creation of "Defined Variables" panel
#---------------------------------------------------------------------
class create:
   def __init__( self, parent ):

      #-------------------------------------------
      # create 1st sub-panel
      #-------------------------------------------
      self.a=parent.pane.pane( 'panelDV' )
      self.a.bind('<Configure>',gui_control.Command( self.alterpaneDV, parent ))
      FRAME = Tkinter.Frame( parent.pane.pane( 'panelDV' ) )
      FRAME.pack( side='top', fill='both', expand=1 )

      #-----------------------------------------------
      # scrollbar 1 -- 'Data' now called 'Selected'
      #-----------------------------------------------
      self.lst1 = {}
      self.number_lst1 = {}
      self.selected = []
      self.selected_list = {}
      self.template_bin_name = 'default'
      self.bin_type = 'Boxfill'
      self.gm_bin_name = 'default'

      self.framea = Tkinter.Frame( FRAME )
      self.framea.pack( side='top', fill = 'both' )
#      lbl1 = Tkinter.Label(self.framea, text = 'Defined Variables' )
#      lbl1.pack( side='left')

      frameb = Tkinter.Frame( FRAME )
      frameb.pack( side='top', fill = 'both' )

#      framed = Pmw.ScrolledFrame( self.framea, 
#                  usehullsize=1,
#                  hull_height = 70,
#                  horizflex = 'expand',
#                  hscrollmode='static',
#                  horizscrollbar_width=gui_control.scl_width,
#                  vscrollmode='static',
#                  vertflex='expand',
#                  vertscrollbar_width=gui_control.scl_width,
#                  )
#      framed.pack( side='top', fill = 'both', expand=1 )

#      lbl2 = Tkinter.Label(framed.interior(), background = gui_color.six,)
#      lbl2.pack(padx = 2, pady = 2, expand='yes', fill='both')
#      lbl3 = Tkinter.Label(lbl2, background = gui_color.six,)
#      lbl3.pack(padx = 2, pady = 2)

      # Set up devel icon
      self.canvas_develicon = Tkinter.Canvas(self.framea, bd=0, highlightthickness=0,
                                        width = 20, height = 20)
      self.canvas_develicon.pack( side='left', fill=Tkinter.BOTH, expand='no', padx=1, pady=1 )
      parent.balloon.bind( self.canvas_develicon, "Hide defined variables tools." )
      self.develimage = Tkinter.PhotoImage( file = os.path.join(cdms2.__path__[0],'..','..','..','..', 'bin', 'devel_20.gif') )
      self.canvas_develicon.create_image(0,0, anchor=Tkinter.NW, image=self.develimage )
      self.canvas_develicon.bind( '<1>', gui_control.Command( self.evt_show_defined_variables_tools, parent ))

      # Set up tg = Template and Graphics Methods icon
      self.canvas_tgicon = Tkinter.Canvas(self.framea, bd=0, highlightthickness=0,
                                        width = 20, height = 20)
      self.canvas_tgicon.pack( side='left', fill=Tkinter.BOTH, expand='no', padx=1, pady=1 )
      parent.balloon.bind( self.canvas_tgicon,
                 "Display the Template and Graphics Method windows." )
      self.tgimage = Tkinter.PhotoImage( file = os.path.join(cdms2.__path__[0],'..','..','..','..', 'bin', 'tg2_20.gif') )
      self.canvas_tgicon.create_image(0,0, anchor=Tkinter.NW, image=self.tgimage )
      self.canvas_tgicon.bind( '<1>', gui_control.Command( self.evt_show_template_graphics_method, parent ))

      # Set up edit icon
      self.canvas_editicon = Tkinter.Canvas(self.framea, bd=0, highlightthickness=0,
                                        width = 20, height = 20)
      self.canvas_editicon.pack( side='left', fill=Tkinter.BOTH, expand='no', padx=1, pady=1 )
      parent.balloon.bind( self.canvas_editicon,
                 "Edit (in memory) selected defined variable." )
      self.editimage = Tkinter.PhotoImage( file = os.path.join(cdms2.__path__[0],'..','..','..','..', 'bin', 'edit_20.gif') )
      self.canvas_editicon.create_image(0,0, anchor=Tkinter.NW, image=self.editimage )
      self.canvas_editicon.bind( '<1>', gui_control.Command( self.evt_call_gui_alter_variable, parent ))

      # Set up save icon
      self.canvas_saveicon = Tkinter.Canvas(self.framea, bd=0, highlightthickness=0,
                                        width = 20, height = 20)
      self.canvas_saveicon.pack( side='left', fill=Tkinter.BOTH, expand='no', padx=1, pady=1 )
      parent.balloon.bind( self.canvas_saveicon,
                 "Save selected defined variable to a netCDF file." ) 
      self.saveimage = Tkinter.PhotoImage( file = os.path.join(cdms2.__path__[0],'..','..','..','..', 'bin', 'save_20.gif') )
      self.canvas_saveicon.create_image(0,0, anchor=Tkinter.NW, image=self.saveimage )
      self.canvas_saveicon.bind( '<1>', gui_control.Command( self.evt_call_gui_writenetcdf, parent ))

      # Set up info icon
      self.canvas_infoicon = Tkinter.Canvas(self.framea, bd=0, highlightthickness=0,
                                        width = 20, height = 20)
      self.canvas_infoicon.pack( side='left', fill=Tkinter.BOTH, expand='no', padx=1, pady=1 )
      parent.balloon.bind( self.canvas_infoicon,
                 "Display selected defined variable information." )
      self.infoimage = Tkinter.PhotoImage( file = os.path.join(cdms2.__path__[0],'..','..','..','..', 'bin', 'info_20.gif') )
      self.canvas_infoicon.create_image(0,0, anchor=Tkinter.NW, image=self.infoimage )
      self.canvas_infoicon.bind( '<1>', gui_control.Command( self.evt_info_on_defined, parent))

      # Set up delete file icon
      self.canvas_dediticon = Tkinter.Canvas(self.framea, bd=0, highlightthickness=0,
                                        width = 20, height = 20)
      self.canvas_dediticon.pack( side='left', fill=Tkinter.BOTH, expand='no', padx=1, pady=1 )
      parent.balloon.bind( self.canvas_dediticon,
                 "Move selected defined variable(s) to trashcan for disposal." )
      self.editdeleteimage = Tkinter.PhotoImage( file = os.path.join(cdms2.__path__[0],'..','..','..','..', 'bin', 'editdelete_20.gif') )
      self.canvas_dediticon.create_image(0,0, anchor=Tkinter.NW, image=self.editdeleteimage )
      self.canvas_dediticon.bind( '<1>', gui_control.Command( self.evt_remove_selected_defined, parent, None ))

      # Set up recycle file icon
      self.canvas_recycleicon = Tkinter.Canvas(self.framea, bd=0, highlightthickness=0,
                                        width = 20, height = 20)
      self.canvas_recycleicon.pack( side='left', fill=Tkinter.BOTH, expand='no', padx=1, pady=
1 )
      parent.balloon.bind( self.canvas_recycleicon,
                 "Move [ALL] defined variables to trashcan for disposal." )
      self.recycleimage = Tkinter.PhotoImage( file = os.path.join(cdms2.__path__[0],'..','..','..','..', 'bin', 'recycle_20.gif') )
      self.canvas_recycleicon.create_image(0,0, anchor=Tkinter.NW, image=self.recycleimage )
      self.canvas_recycleicon.bind( '<1>', gui_control.Command( self.evt_remove_all_defined, parent ))

      # Set up log icon
      self.canvas_logicon = Tkinter.Canvas(self.framea, bd=0, highlightthickness=0,
                                        width = 20, height = 20)
      self.canvas_logicon.pack( side='left', fill=Tkinter.BOTH, expand='no', padx=1, 
pady=
1 )
      parent.balloon.bind( self.canvas_logicon,
                 "Logged information about the defined variables." )
      self.logimage = Tkinter.PhotoImage( file = os.path.join(cdms2.__path__[0],'..','..','..','..', 'bin', 'log_20.gif') )
      self.canvas_logicon.create_image(0,0, anchor=Tkinter.NW, image=self.logimage )
      self.canvas_logicon.bind( '<1>', gui_control.Command( self.evt_log_on_defined, parent ))

      # Set up trashcan icon
      self.canvas_trashcanicon = Tkinter.Canvas(self.framea, bd=0, highlightthickness=0,
                                        width = 20, height = 20)
      self.canvas_trashcanicon.pack( side='left', fill=Tkinter.BOTH, expand='no', padx=1, pady=1 )
      parent.balloon.bind( self.canvas_trashcanicon,
                 "Defined variable items that can be disposed of permanetly or restored." )
      self.trashcan_empty_image = Tkinter.PhotoImage( file = os.path.join(cdms2.__path__[0],'..','..','..','..', 'bin', 'trashcan_empty_20.gif') )
      self.trashcan_full_image = Tkinter.PhotoImage( file = os.path.join(cdms2.__path__[0],'..','..','..','..', 'bin', 'trashcan_full_20.gif') )
      self.canvas_trashcanicon.create_image(0,0, anchor=Tkinter.NW, image=self.trashcan_empty_image )
      self.canvas_trashcanicon.bind( '<1>', gui_control.Command( self.evt_dispose_defined_variables, parent ))

      # Set up cdatdemo icon
      self.cdatdemoicon = Tkinter.Canvas(self.framea, bd=0, highlightthickness=0,
                                        width = 20, height = 20)
      self.cdatdemoicon.pack( side='left', fill=Tkinter.BOTH, expand='no', padx=1, pady=1 )
      parent.balloon.bind( self.cdatdemoicon,
                 "Run CDAT Demo GUI." )
      self.cdatdemo_image = Tkinter.PhotoImage( file = os.path.join(cdms2.__path__[0],'..','..','..','..', 'bin', 'cdatdemo.gif') )
      self.cdatdemoicon.create_image(0,0, anchor=Tkinter.NW, image=self.cdatdemo_image )
      self.cdatdemoicon.bind( '<1>', gui_control.Command( self.evt_cdat_demo ))
      # setup search thing
      self.search = Pmw.EntryField(self.framea,entry_background = 'yellow')
      self.search.setentry('')
      self.search.pack(side = 'left', fill='x', padx = 10)
      parent.balloon.bind(self.search, 'Search CDAT documentation')
      self.search.component('entry').bind( "<Return>", gui_control.Command(self.evt_search_command, parent) )
      # Set up command line 
      self.command = Pmw.EntryField(self.framea,entry_width = 1000,entry_background = gui_color.six)
      self.command.setentry('')
      self.command.pack(side = 'left', fill='x', padx = 10)
      parent.balloon.bind(self.command, 'Command for quickly forming expressions (e.g., c = a + b)')
      self.command.component('entry').bind( "<Key>", gui_control.Command(self.evt_change_command_color, parent) )
      self.command.component('entry').bind( "<Return>", gui_control.Command(self.evt_compute_command, parent) )
      #print FRAME.configure(),FRAME.configure()['height']
      hull_initial_size=3
      Frame=Tkinter.Frame(frameb)
      Frame.pack(side='top',fill='both')
      
      Framea=Tkinter.Frame(Frame)
      Framea.pack(side='left', fill ='both', expand=1, padx = 5)
      
##       Frameb=Tkinter.Frame(Frame)
##       Frameb.pack(side='left', padx = 5)
      
      self.framedb = frameb = Framea

##       self.framedb = Pmw.ScrolledFrame( Framea,
      
##                   usehullsize=1,
##                   hull_height = hull_initial_size,
## #                  horizflex = 'expand',
## #                  hscrollmode='static',
##                   horizscrollbar_width=gui_control.scl_width,
## #                  vscrollmode='static',
## #                  vertflex='expand',
##                   vertscrollbar_width=gui_control.scl_width,
##                   )
##       self.framedb.pack( side='top', fill = 'x', expand=1 )
## ##       self.framedb.configure(hull_height=300)
##       frameb = self.framedb.interior()
##       frameb.pack()
      # Set up the Template list
      self.framet = Tkinter.Frame(frameb)
      self.framet.pack(side='left', expand=1, fill='both')

      # create the template menu and scroll list
      self.tmain_menu = Pmw.MenuBar(self.framet,
                hull_relief = 'raised',
                hull_borderwidth = 2,
                balloon = parent.balloon
                )
      self.tmain_menu.pack(side='top', fill='x')
      self.tmain_menu.addmenu('Template       ', "Template Options/Preferences", tearoff = 1)
       
      # Create the cascade "Template" options
      self.tmain_menu.addmenuitem('Template       ', 'command', 'rename',
                       label = "Redisplay List",
                       command = gui_control.Command( self.redisplay_template, parent )
                      )
      self.tmain_menu.addmenuitem('Template       ', 'separator')
      self.tmain_menu.addmenuitem('Template       ', 'command', 'rename',
                       label = "Edit",
                       command = gui_control.Command( self.edit_template, parent )
                      )
      self.tmain_menu.addmenuitem('Template       ', 'command', 'rename',
                       label = "Copy",
                       command = gui_control.Command(self.copy_template, parent)
                      )
      self.tmain_menu.addmenuitem('Template       ', 'command', 'rename',
                       label = "Rename",
                       command = gui_control.Command(self.rename_template, parent)
                      )
      self.tmain_menu.addmenuitem('Template       ', 'command', 'rename',
                       label = "Save as a Script File",
                       command = gui_control.Command(self.script_template, parent)
                      )
      self.tmain_menu.addmenuitem('Template       ', 'command', 'rename',
                       label = "Remove",
                       command = gui_control.Command(self.remove_template, parent)
                      )
      # Create the Template list
      template_list = parent.vcs[ parent.vcs_id ].listelements('template')

      self.template_listbox = Pmw.ScrolledListBox(self.framet,
#                label_text = 'Select Template:',
#                labelpos = 'nw',

                usehullsize=1,
                hull_height = hull_initial_size,
                hull_width = gui_control.mini_defined_template_gm_width,
                
                listbox_width=15,
                items = template_list,
                listbox_background = gui_color.template_bg,
                listbox_foreground=gui_color.template_fg,
                horizscrollbar_width=gui_control.scl_width,
                vertscrollbar_width=gui_control.scl_width,
                selectioncommand = self.select_template
           )
      self.template_listbox.pack(side='top', fill = 'both', expand=1)
      parent.balloon.bind(self.template_listbox, "Select 'Template' name with the left mouse button.\nMove pointer to the %s 'Template' area of the\n'Page Layout Form' and depress the left mouse\nbutton. The new template name will replace the\nold name." % gui_color.template_bg)

      #
      # Set up the Graphics Method list
      self.frameg=Tkinter.Frame(frameb)
      self.frameg.pack(side='left', fill ='both', expand=1, padx = 5)
      self.gm_name = 'Boxfill'

      # create the graphics method menu and scroll list
      self.gmain_menu = Pmw.MenuBar(self.frameg,
                hull_relief = 'raised',
                hull_borderwidth = 2,
                balloon = parent.balloon
                )
      self.gmain_menu.pack(side='top', fill='x')
      self.gmain_menu.addmenu('Graphics Method        ', "Graphics Method Options/Preferences", tearoff = 1)
       
      # Create the cascade "Graphics Method" options
      self.gmain_menu.addmenuitem('Graphics Method        ', 'command', 'rename',
                       label = "Redisplay List",
                       command = gui_control.Command(self.redisplay_graphics_method, parent)
                      )
      self.gmain_menu.addmenuitem('Graphics Method        ', 'separator')
      self.gmain_menu.addcascademenu('Graphics Method        ', 'Select Graphics Method',
                      'select graphics method',
                      label = 'Select Graphics Method',
                      traverseSpec = 'z', tearoff = 1)
      for x in graphics_method_list[:-1]:
         self.gmain_menu.addmenuitem('Select Graphics Method','command','View graphics methods',
                      label = x,
                      command = gui_control.Command(self.evt_select_graphics_method, x, parent)
                     )
      self.gmain_menu.addmenuitem('Graphics Method        ', 'command', 'rename',
                       label = "Edit",
                       command = gui_control.Command(self.edit_graphics_method, parent)
                      )
      self.gmain_menu.addmenuitem('Graphics Method        ', 'command', 'rename',
                       label = "Copy",
                       command = gui_control.Command(self.copy_graphics_method, parent)
                      )
      self.gmain_menu.addmenuitem('Graphics Method        ', 'command', 'rename',
                       label = "Rename",
                       command = gui_control.Command(self.rename_graphics_method, parent)
                      )
      self.gmain_menu.addmenuitem('Graphics Method        ', 'command', 'rename',
                       label = "Save as a Script File",
                       command = gui_control.Command(self.script_graphics_method, parent)
                      )
      self.gmain_menu.addmenuitem('Graphics Method        ', 'command', 'rename',
                       label = "Remove",
                       command = gui_control.Command(self.remove_graphics_method, parent)
                      )
      gm_list =  parent.vcs[ parent.vcs_id ].listelements( self.gm_name )
      self.gm_name_obj = Tkinter.StringVar()
      self.gm_name_obj.set( 'Select ' + self.gm_name + ':' )
      self.gm_listbox = Pmw.ScrolledListBox(self.frameg,
      
                usehullsize=1,
                hull_height = hull_initial_size,
                hull_width=gui_control.mini_defined_template_gm_width,
               
                label_textvariable = self.gm_name_obj,
                labelpos = 'nw',
                listbox_width=15,
                items = gm_list,
                listbox_background = gui_color.graphics_method_bg,
                listbox_foreground=gui_color.graphics_method_fg,
                horizscrollbar_width=gui_control.scl_width,
                vertscrollbar_width=gui_control.scl_width,
                selectioncommand = self.select_gm
           )
      self.gm_listbox.pack(side='top', fill = 'both', expand=1)
      parent.balloon.bind(self.gm_listbox, "Select 'Graphics Method' name with the left mouse\nbutton. Move pointer to the %s 'Graphics Method'\narea of the 'Page Layout Form' and depress the\nleft mouse button. The new graphics method name\nwill replace the old name." % gui_color.graphics_method_bg)

      # Turn off the tools
      self.evt_show_defined_variables_tools( parent, None )
      self.evt_show_template_graphics_method( parent, None )


      # Set up the Data (Defined Variables) list 

      self.bct = 0

      self.framec = Tkinter.Frame( frameb )
      self.framec.pack( side='left', fill = 'both', expand=1 )
      self.scl1 = Pmw.ScrolledListBox( self.framec,
      
                             usehullsize=1,
                             hull_height = hull_initial_size,
                             hull_width = gui_control.mini_defined_template_gm_width*gui_control.defined_frame_scale,
                             
                             items=self.lst1.keys(),
                             label_text = 'Defined Variables',
                             labelpos = 'n',
##                              listbox_height=30,
                             listbox_selectbackground=gui_color.six,
                             listbox_selectforeground='black',
                             listbox_background = 'white',
                             listbox_foreground = 'black',
                             listbox_exportselection = Tkinter.NO,
                             listbox_selectmode=Tkinter.MULTIPLE,
                             horizscrollbar_width=gui_control.scl_width,
##                              vscrollmode='static',
                             vertscrollbar_width=gui_control.scl_width,
                             )
      self.scl1.pack( side='top', padx=3, pady=0, fill='both', expand=1 )
      self.scl1.component('listbox').bind('<ButtonRelease-1>', gui_control.Command( self.evt_selected_defined_variable, parent ) )
      self.scl1.component('listbox').bind('<Control-ButtonRelease-1>', gui_control.Command( self.evt_selected_defined_variable2, parent ) )
      self.scl1.component('listbox').bind('<Shift-ButtonPress-1>', "break" )
      self.scl1.component('listbox').bind('<Shift-ButtonRelease-1>', "break" )
      parent.balloon.bind(self.scl1, "This is the 'Defined Variables' list window.\n\nDisplays the list of variables that are stored in\nmemory. To save a variable to a netCDF file, or to\nalter a variable's attributes, or to remove defined\nvariables from memory, select the 'Defined Variables\nOptions' menu above.")

      # Popup Menu for the Defined Variables window
      popupm = Tkinter.Frame( self.scl1.interior() )

      menu0 = Tkinter.Menu( popupm, title="Defined Variables Popup Menu", background = 'white', tearoff=1)
      self.img_toggle = Tkinter.PhotoImage( file = os.path.join(cdms2.__path__[0],'..','..','..','..', 'bin', 'toggle_menu.gif') )
      menu0.add_command( label="Toggle between Single and Multiple selection", command = gui_control.Command(self.evt_toggle_DV_selection_mode, parent ),image=self.img_toggle)
      menu0.add_separator( )
      self.img_dev = Tkinter.PhotoImage( file = os.path.join(cdms2.__path__[0],'..','..','..','..', 'bin', 'devel_menu.gif') )
      menu0.add_command( label="Show/Hide the defined variables icon", command = gui_control.Command(self.evt_show_defined_variables_tools, parent, None ) ,image=self.img_dev )
      self.img_tg = Tkinter.PhotoImage( file = os.path.join(cdms2.__path__[0],'..','..','..','..', 'bin', 'tg_menu.gif') )
      menu0.add_command( label="Show/Hide the Template and Graphics Method window", command = gui_control.Command( self.evt_show_template_graphics_method, parent, None ),image=self.img_tg)
      #, image=self.tgimage )
      self.img_edit = Tkinter.PhotoImage( file = os.path.join(cdms2.__path__[0],'..','..','..','..', 'bin', 'edit_menu.gif') )
      menu0.add_command( label="Edit the most recent selected defined variable", command = gui_control.Command( self.evt_call_gui_alter_variable, parent, None ),image=self.img_edit)
      #,image=self.editimage )
      self.img_save = Tkinter.PhotoImage( file = os.path.join(cdms2.__path__[0],'..','..','..','..', 'bin', 'save_menu.gif') )
      menu0.add_command( label="Save the most recent defined variable to a netCDF file", command = gui_control.Command( self.evt_call_gui_writenetcdf, parent, None ),image=self.img_save)
      #,image=self.saveimage )
      self.img_info = Tkinter.PhotoImage( file = os.path.join(cdms2.__path__[0],'..','..','..','..', 'bin', 'info_menu.gif') )
      menu0.add_command( label="Display the most recent defined variable information", command = gui_control.Command( self.evt_info_on_defined, parent, None ),image=self.img_info)
      #,image=self.infoimage )
      self.img_remove = Tkinter.PhotoImage( file = os.path.join(cdms2.__path__[0],'..','..','..','..', 'bin', 'editdelete_menu.gif') )
      menu0.add_command( label="Remove all selected defined variable(s) to trashcan", command = gui_control.Command( self.evt_remove_selected_defined, parent, None, None),image=self.img_remove)
      #,image=self.editdeleteimage )
      self.img_recycle = Tkinter.PhotoImage( file = os.path.join(cdms2.__path__[0],'..','..','..','..', 'bin', 'recycle_menu.gif') )
      menu0.add_command( label="Remove all defined variable(s) to trashcan", command = gui_control.Command( self.evt_remove_all_defined, parent, None ),image=self.img_recycle)
      #,image=self.recycleimage )
      self.img_log = Tkinter.PhotoImage( file = os.path.join(cdms2.__path__[0],'..','..','..','..', 'bin', 'log_menu.gif') )
      menu0.add_command( label="View logged information about the defined variables", command = gui_control.Command( self.evt_log_on_defined, parent, None ),image=self.img_log)
      #, image=self.logimage  )
      self.img_trash = Tkinter.PhotoImage( file = os.path.join(cdms2.__path__[0],'..','..','..','..', 'bin', 'trashcan_menu.gif') )
      menu0.add_command( label="Dispose of or restore defined variables located in trashcan", command = gui_control.Command( self.evt_dispose_defined_variables, parent, None ),image=self.img_trash)
      #, image=self.trashcan_empty_image )

## ??? Sems to be useless....
##       menu1 = Tkinter.Menu( menu0, tearoff=0)
##       menu2 = Tkinter.Menu( menu1, tearoff=0)

      # Popup the menu up when the right mouse button is selected
      def showMenu(event):
              menu0.tk_popup( event.x_root, event.y_root )
      #       menu0.post( event.x_root, event.y_root )

      self.scl1.component('listbox').bind( "<Button-3>", showMenu)

#      framee = Tkinter.Frame( frameb )
#      framee.pack( side='left', after=self.scl1, fill = 'both', expand=1 )

      # Set up the Icon Function list
##       frameicons = Tkinter.Frame( Framea )
##       frameicons.pack( side='left', fill = 'both', expand=1 )
      self.framef = Pmw.ScrolledFrame( Frame,
                                       
                  usehullsize=1,
                  hull_height = hull_initial_size,
                  hull_width=115,
                                       
                  label_text = 'Calculator      ',
                  labelpos = 'n',
#                  horizflex = 'expand',
#                  hscrollmode='static',
#                  horizscrollbar_width=gui_control.scl_width,
#                  vscrollmode='static',
                  vertflex='expand',
                  vertscrollbar_width=gui_control.scl_width,
                  )
      self.framef.pack( side='left', fill = 'both', expand=1 )

      lbl2 = Tkinter.Label(self.framef.interior(), background = gui_color.six,)
      lbl2.pack(padx = 2, pady = 2, expand='yes', fill='both')
      lbl3 = Tkinter.Label(lbl2, background = gui_color.six,)
      lbl3.pack(padx = 2, pady = 2)

      # Set up operator icons
      icons_def=[
      ['Add a number or two (or more)\nselected Defined Variables.\n(Can be used as "or")','add.gif','add'],
      ['Subtract a number or two (or more)\nselected Defined Variables.','subtract.gif','subtract'],
      ['Multiply a number or two (or more)\nselected Defined Variables.\n(Can be used as "and")','multiply.gif','multiply'],
      ['Divide a number or two (or more)\nselected Defined Variables.','divide.gif','divide'],
      ['"Grows" variable 1 and variable 2 so that they end up having the same dimensions\n(order of variable 1 plus any extra dims)','grower.gif','grower'],
      ['Spatially regrid the first selected Defined Variable\nto the second selected Defined Variable.','regrid.gif','regrid'],
      ['Mask out variable 2 where variable 1 is "true".','mask.gif','mask'],
      ['Get variable mask','getmask.gif','getmask'],
      ['Return true where variable 1 is less than variable 2 (or number)','less.gif','less'],
      ['Return true where variable 1 is greater than variable 2 (or number)','greater.gif','greater'],
      ['Return true where variable 1 is equal than variable 2 (or number)','equal.gif','equal'],
      ['Return not of variable','not.gif','not'],
      ['Compute the standard deviation\n(over first axis)','std.gif','std'],
      ['Power (i.e., x ** y) of the most recently\nselected two Defined Variables, where\nx = variable 1 and y = variable 2 or float number.','power.gif','power'],
      ['Exp (i.e., e ** x) of the most recently\nselected Defined Variable.','exp.gif','exp'],
      ['Log (i.e., natural log) of the most recently\nselected Defined Variable.','mlog.gif','log'],
      ['Base10 (i.e., 10 ** x) of the most recently\nselected Defined Variable.','base10.gif','base10'],
      ['Log10 (i.e., log base 10) of the most\nrecently selected Defined Variable. ','mlog10.gif','log10'],
      ['Inverse (i.e., 1/x) of the most recently\nselected Defined Variable.','inverse.gif','inverse'],
      ['Abs (i.e., absolute value of x) of the most\nrecently selected Defined Variable.','fabs.gif','fabs'],
      ['Sine (i.e., sin) of the most recently\nselected Defined Variable.','sin.gif','sin'],
      ['Hyperbolic sine (i.e., sinh) of the most recently\nselected Defined Variable.','sinh.gif','sinh'],
      ['Cosine (i.e., cos) of the most recently\nselected Defined Variable.','cos.gif', 'cos'],
      ['Hyperbolic cosine (i.e., cosh) of the most recently\nselected Defined Variable.','cosh.gif','cosh'],
      ['Tangent (i.e., tan) of the most recently\nselected Defined Variable.','tan.gif','tan'],
      ['Hyperbolic tangent (i.e., tanh) of the most recently\nselected Defined Variable.','tanh.gif','tanh'],
      ]
      self.operator_icon = {}
      self.operator_ballon = {}
      self.operator_image = {}
      self.operator_images = {}
      operator_type        = {}
      for i in range(len(icons_def)):
         self.operator_ballon[i]=icons_def[i][0]
         self.operator_images[i]=icons_def[i][1]
         operator_type[i]=icons_def[i][2]

      # For calculation mode 2, set the doing_calculation_type flag to 'no'
      self.doing_calculation_type = ''
      self.saved_expression = ''
      self.build_expression = 'no'
      self.gui_expression = None
      try:
         parent.calculate_mode = vcdat_initial.set.calculate_mode
      except:
         parent.calculate_mode = 1

      # Show the calculation mode and the function icon
      framect = Tkinter.Frame( lbl3, background = gui_color.six )
      framect.pack( side='top', fill = 'both', expand='no', padx=1, pady=1 )

      self.func_mode_icon = Tkinter.Canvas(framect, bd=0, highlightthickness=0,
                                        width = 32, height = 32)
      self.func_mode_icon.pack( side='left', fill=Tkinter.BOTH, expand='no',
                                      padx=2, pady=0 )
      parent.balloon.bind( self.func_mode_icon, "This toggle icon contains two modes for calculating:\n\nMode 1 (the default mode):\nWants the user to select the defined variable(s)\nfirst, then select one of the functions below.\nFor example1: the user could select, in the 'Defined\nVariables' window var1, var2, and var3, then select\nthe 'Add' icon 'x+y' to produce the variable\n'_add_var1_var2_var3'.\n\nMode 2\nWants the user to form an equation, then select the\n'Apply' button in the associated expression widget.\nTo view, modify, and apply the equation (at any\ntime), select the expression icon to the right\n(i.e., 'f()'). The expression widget will appear.\nFor example2: select the 'f()' icon, then select\nvar1, then 'x+y' icon, then var2, then 'x/y' icon,\nthen var3, then 'e**x' icon. Finally, select the\n'Apply' button in the expression widget." )
      self.func_mode1_image = Tkinter.PhotoImage( file =
            os.path.join(cdms2.__path__[0],'..','..','..','..', 'bin', 'hand1.gif') )
      self.func_mode2_image = Tkinter.PhotoImage( file =
            os.path.join(cdms2.__path__[0],'..','..','..','..', 'bin', 'hand2.gif') )
      if parent.calculate_mode == 1:
         self.func_mode_icon.create_image(0,0, anchor=Tkinter.NW,
                                         image=self.func_mode1_image )
      else:
         self.func_mode_icon.create_image(0,0, anchor=Tkinter.NW,
                                         image=self.func_mode2_image )
      self.func_mode_icon.bind( '<1>', gui_control.Command(
                     self.evt_set_mode_operation, parent ))

      self.func_expression_icon = Tkinter.Canvas(framect, bd=0, highlightthickness=0,
                                     width = 32, height = 32)
      self.func_expression_icon.pack( side='left', fill=Tkinter.BOTH, expand='no',
                                   padx=0, pady=0 )
      parent.balloon.bind( self.func_expression_icon, 'View the expression widget for creating, modifying,\nand applying equations.' )
      self.func_expression_image = Tkinter.PhotoImage( file =
            os.path.join(cdms2.__path__[0],'..','..','..','..', 'bin', 'function.gif' ) )
      self.func_expression_icon.create_image(0,0, anchor=Tkinter.NW,
                                         image=self.func_expression_image )
      self.func_expression_icon.bind( '<1>', gui_control.Command(
                     self.evt_expression_widget, parent ))


      # Show the function icons
      for i in range(0, len(icons_def), 2):
         framect = Tkinter.Frame( lbl3, background = gui_color.six )
         framect.pack( side='top', fill = 'both', expand='yes', padx=1, pady=1 )

         self.operator_icon[i] = Tkinter.Canvas(framect, bd=0, highlightthickness=0,
                                        width = 32, height = 32)
         self.operator_icon[i].pack( side='left', fill=Tkinter.BOTH, expand='no',
                                      padx=2, pady=0 )
         parent.balloon.bind( self.operator_icon[i], self.operator_ballon[i] )
         self.operator_image[i] = Tkinter.PhotoImage( file = 
               os.path.join(cdms2.__path__[0],'..','..','..','..', 'bin', self.operator_images[i]) )
         self.operator_icon[i].create_image(0,0, anchor=Tkinter.NW,
                                            image=self.operator_image[i] )
         self.operator_icon[i].bind( '<1>', gui_control.Command(
                        self.evt_do_operation, parent, operator_type[i] ))

         self.operator_icon[i+1] = Tkinter.Canvas(framect, bd=0, highlightthickness=0,
                                        width = 32, height = 32)
         self.operator_icon[i+1].pack( side='left', fill=Tkinter.BOTH, expand='no',
                                      padx=0, pady=0 )
         parent.balloon.bind( self.operator_icon[i+1], self.operator_ballon[i+1] )
         self.operator_image[i+1] = Tkinter.PhotoImage( file = 
               os.path.join(cdms2.__path__[0],'..','..','..','..', 'bin', self.operator_images[i+1]) )
         self.operator_icon[i+1].create_image(0,0, anchor=Tkinter.NW,
                                            image=self.operator_image[i+1] )
         self.operator_icon[i+1].bind( '<1>', gui_control.Command(
                        self.evt_do_operation, parent, operator_type[i+1] ))

   ## Function to resize the scrolled box when the panel changes
   def alterpaneDV(self,parent,event=None):
      padx=10
      pady=40
      # Determine the heights  available and used
      hvalue=parent.pane.pane('panelDV').winfo_height()
      hvalue2=hvalue-((1-parent.show_template_graphics_method)+1)*pady
      
      ## Available width
      width=parent.pane.pane('panelDV').winfo_width()
      mini=gui_control.mini_defined_template_gm_width
      newsize=(width-115.-((1-parent.show_template_graphics_method)*1.8+1)*padx)/(2*(1-parent.show_template_graphics_method)+gui_control.defined_frame_scale)
      wvalue=int(max(newsize,mini))
      ## Reconfigure
##       self.framedb.configure(hull_height=hvalue)
      self.template_listbox.configure(hull_height=hvalue2)
      self.template_listbox.configure(hull_width=wvalue)
      self.gm_listbox.configure(hull_height=hvalue2)
      self.gm_listbox.configure(hull_width=wvalue)
      self.scl1.configure(hull_height=hvalue2)
      self.scl1.configure(hull_width=wvalue*gui_control.defined_frame_scale)
##       self.framef.configure(hull_height=hvalue2)
      pass
      

   #-----------------------------------------------------------------
   # event functions associated with the "Defined Variable" panel buttons
   #-----------------------------------------------------------------
   #
   ####### event to toggle the Defined Variable window selection mode
   #
   def evt_toggle_DV_selection_mode(self, parent):
      if parent.menu.main_menu.DV_single_selection_mode.get() == 1:
         parent.menu.main_menu.DV_single_selection_mode.set( 0 )
      else:
         parent.menu.main_menu.DV_single_selection_mode.set( 1 )
      parent.menu.create_options_menu.evt_DV_single_selection_mode( parent )
   #
   ####### event to redisplay the template list
   #
   def redisplay_template (self, parent):
      # Redisplay the template list
      t_list = parent.vcs[ parent.vcs_id ].listelements( 'template' )
      self.template_listbox.setlist( t_list )
   #
   ####### event to bring up the template editor
   #
   def edit_template (self, parent):
      s = self.template_listbox.getcurselection()
      if s in [(), '']:
         gui_message.error( 'Must select a template from the list below.' )
         return
      template_selected = ''
      if len(s) > 0:
          template_selected = s[0]
      plot = None
      priority = -999
      template_orig_name =  template_selected
      if (template_selected == 'ASD'):
         showerror( "Error Message to the User", "The ASD template can only be modified from within VCDAT's 'Set Graphics Method Attributes...' GUI. Please select another template for modification or copy the template under a new name.")
         return
      try:
        for i in parent.pl.form.keys():
           if ((parent.pl.form[i].display.off == 0) and 
              (parent.pl.form[i].display._template_origin == template_selected)):  # This means that it is on
               if (parent.pl.form[i].display.priority >= priority):                # same template
                   plot = parent.pl.form[i].display
                   template_selected = parent.pl.form[i].display.template
      except:
        pass
      parent.vcs[ parent.vcs_id ].templateeditor(template_name=template_selected, template_orig_name=template_orig_name,gui_parent=parent, plot=plot)
   #
   ####### event to copy template
   #
   def copy_template (self, parent):
      # Get the Template name
      try:
         t_selected = new_name = self.template_listbox.getcurselection()[0]
      except:
         gui_message.error( 'Must select a template from the list below.' )
         return

      # Popup use dialog widget
      dialog = Pmw.PromptDialog(parent, title='Copy Template',
                                label_text='Enter Template Name:', 
				entryfield_labelpos='nw',
                                entry_width = 50,
                                defaultbutton=0, buttons=('OK', 'Cancel'))

      # Position dialog popup on top of the main GUI and wait for results
      parent_geom = parent.geometry()
      geom = string.split(parent_geom, '+')
      geom2= string.split(geom[0],'x')
      d1 = string.atoi( geom[1] ); d2 = string.atoi( geom[2] )
      d3 = string.atoi( geom2[0] ); d4 = string.atoi( geom2[1] )
      d5=d2+(d4/2)

      result = dialog.activate(globalMode = 'nograb', geometry= "+%d+%d" % (d1, d5) )

      if result == 'OK':
         new_name = dialog.component('entry').get()
         if new_name in parent.vcs[ parent.vcs_id ].listelements( 'template' ):
            gui_message.error( 'Error occurred while trying to copy template %s to %s. Make sure the new template name does not already exist.' % (t_selected, new_name) )
            return
         try:
            parent.vcs[ parent.vcs_id ].createtemplate(new_name, t_selected) 
         except:
            gui_message.error( 'Error occurred while trying to copy template %s to %s. Make sure the new template name does not already exist.' % (t_selected, new_name) )
            return

      # Redisplay the template list
      t_list = parent.vcs[ parent.vcs_id ].listelements( 'template' )
      self.template_listbox.setlist( t_list )
      name_index = t_list.index( new_name )
      parent.panelDV.template_listbox.select_set( name_index )
      parent.panelDV.template_listbox.see( name_index )
   #
   ####### event to rename template
   #
   def rename_template (self, parent):
      # Get the Template name
      try:
         t_selected = new_name = self.template_listbox.getcurselection()[0]
      except:
         gui_message.error( 'Must select a template from the list below.' )
         return

      # Check for 'default' and 'ASD' template
      if ( (t_selected[:7] == 'default') or (t_selected[:3] == 'ASD') ):
         showerror( "Error Message to the User", "The default and ASD templates cannot be modified. Please select another template for modification or copy the template under a new name.")
         return

      # Popup use dialog widget
      dialog = Pmw.PromptDialog(parent, title='Rename Template',
                                label_text='Enter Template Name:', 
				entryfield_labelpos='nw',
                                entry_width = 50,
                                defaultbutton=0, buttons=('OK', 'Cancel'))

      # Position dialog popup on top of the main GUI and wait for results
      parent_geom = parent.geometry()
      geom = string.split(parent_geom, '+')
      geom2= string.split(geom[0],'x')
      d1 = string.atoi( geom[1] ); d2 = string.atoi( geom[2] )
      d3 = string.atoi( geom2[0] ); d4 = string.atoi( geom2[1] )
      d5=d2+(d4/2)

      result = dialog.activate(globalMode = 'nograb', geometry= "+%d+%d" % (d1, d5) )

      if result == 'OK':
         new_name = dialog.component('entry').get()
         if new_name in parent.vcs[ parent.vcs_id ].listelements( 'template' ):
            gui_message.error( 'Error occurred while trying to copy template %s to %s. Make sure the new template name does not already exist.' % (t_selected, new_name) )
            return
         try:
            r = parent.vcs[ parent.vcs_id ].gettemplate(t_selected) 
            r.name = new_name
         except:
            gui_message.error( 'Error occurred while trying to copy template %s to %s. Make sure the new template name does not already exist.' % (t_selected, new_name) )
            return

      # Redisplay the template list
      t_list = parent.vcs[ parent.vcs_id ].listelements( 'template' )
      self.template_listbox.setlist( t_list )
      name_index = t_list.index( new_name )
      parent.panelDV.template_listbox.select_set( name_index )
      parent.panelDV.template_listbox.see( name_index )
   #
   ####### event to save template to a script file
   #
   def script_template (self, parent):
      # Get the Template name
      try:
         t_selected = self.template_listbox.getcurselection()[0]
      except:
         gui_message.error( 'Must select a template from the list below.' )
         return

      # Show the popup directory dialog
      filetypes = [ ("Python", ".py"), ("VCS", ".scr") ]
      save_dialog = tkFileDialog.asksaveasfilename(master=parent,
                       filetypes=filetypes,
                       title = 'Save template %s to file' % (t_selected))

      if save_dialog == '': return
      if (save_dialog[-3:] != '.py') and (save_dialog[-4:] != '.scr'): save_dialog += '.py'

      # Script the template selected in the list
      r = parent.vcs[ parent.vcs_id ].gettemplate(t_selected) 
      parent.vcs[ parent.vcs_id ].scriptobject( r, save_dialog, mode = 'w' )

      # Redisplay the graphics method list
      t_list = parent.vcs[ parent.vcs_id ].listelements( 'template' )
      name_index = t_list.index( t_selected )
      parent.panelDV.template_listbox.select_set( name_index )
      parent.panelDV.template_listbox.see( name_index )
   #
   ####### event to remove template
   #
   def remove_template (self, parent):
      # Get the Template name
      try:
         t_selected = self.template_listbox.getcurselection()[0]
      except:
         gui_message.error( 'Must select a template from the list below.' )
         return

      if t_selected not in parent.vcs[ parent.vcs_id ].listelements( 'template' ):
            gui_message.error( 'Error occurred while trying to remove template %s from the list. Make sure the graphics method name exist in the list.' % (t_selected) )
            return

      if ((t_selected[0:7] == 'default') or (t_selected[0:3] == 'ASD')):
            gui_message.error( 'Removal of the %s template is not permitted.' % t_selected)
            return

      button = askquestion("Remove Template", "Do you really want to REMOVE %s from the template list?" % (t_selected), default='no')

      if button in ['yes', True]:
         r = parent.vcs[ parent.vcs_id ].gettemplate(t_selected) 
         parent.vcs[ parent.vcs_id ].removeobject( r )

         # Redisplay the template list
         t_list = parent.vcs[ parent.vcs_id ].listelements( 'template' )
         self.template_listbox.setlist( t_list )
   #
   ####### event to select the graphics method
   #
   def evt_select_graphics_method(self, gm_name, parent):
      self.gm_name = gm_name
      self.gm_name_obj.set( 'Select ' + self.gm_name + ':' )
      gm_list = parent.vcs[ parent.vcs_id ].listelements( self.gm_name )
      self.gm_listbox.setlist( gm_list )
   #
   ####### event to redisplay the graphics method list
   #
   def redisplay_graphics_method(self, parent):
      # Redisplay the graphics method list
      gm_list = parent.vcs[ parent.vcs_id ].listelements( self.gm_name )
      self.gm_listbox.setlist( gm_list )
   #
   ####### event to edit the graphics method
   #
   def edit_graphics_method(self, parent):
      try:
         gm_selected = self.gm_listbox.getcurselection()[0]
      except:
         gui_message.error( 'Must select a %s graphics method from the list below.' % self.gm_name )
         return
      if (gm_selected == 'default'):
         showerror( "Error Message to the User", "The %s default graphics method cannot be modified. Please select another %s graphics method for modification or copy the graphics method under a new name." %(self.gm_name,self.gm_name))
      elif (gm_selected == 'ASD'):
         showerror( "Error Message to the User", "The %s ASD graphics method can only be modified from within VCDAT's 'Set Graphics Method Attributes...' GUI. Please select another %s graphics method for modification or copy the graphics method under a new name." %(self.gm_name,self.gm_name))
      else:
         parent.vcs[ parent.vcs_id ].graphicsmethodgui(self.gm_name, gm_selected,
            gui_parent = parent) # popup the graphics method gui
   #
   ####### event to copy the graphics method
   #
   def copy_graphics_method(self, parent):
      # Get the graphics method name
      try:
         gm_selected = new_name = self.gm_listbox.getcurselection()[0]
      except:
         gui_message.error( 'Must select a %s graphics method from the list below.' % self.gm_name )
         return

      # Popup use dialog widget
      dialog = Pmw.PromptDialog(parent, title='Copy %s graphics method' % self.gm_name,
                                label_text='Enter %s Name:' % self.gm_name, 
				entryfield_labelpos='nw',
                                entry_width = 50,
                                defaultbutton=0, buttons=('OK', 'Cancel'))

      # Position dialog popup on top of the main GUI and wait for results
      parent_geom = parent.geometry()
      geom = string.split(parent_geom, '+')
      geom2= string.split(geom[0],'x')
      d1 = string.atoi( geom[1] ); d2 = string.atoi( geom[2] )
      d3 = string.atoi( geom2[0] ); d4 = string.atoi( geom2[1] )
      d5=d2+(d4/2)

      result = dialog.activate(globalMode = 'nograb', geometry= "+%d+%d" % (d1, d5) )

      if result == 'OK':
         new_name = dialog.component('entry').get()
         if new_name in parent.vcs[ parent.vcs_id ].listelements( self.gm_name ):
            gui_message.error( 'Error occurred while trying to copy %s graphics method %s to %s. Make sure the new graphics method name does not already exist.' % (self.gm_name, gm_selected, new_name) )
            return
         try:
            if string.lower( self.gm_name ) == 'boxfill':
               parent.vcs[ parent.vcs_id ].createboxfill(new_name, gm_selected) 
            elif string.lower( self.gm_name ) == 'isofill':
               parent.vcs[ parent.vcs_id ].createisofill(new_name, gm_selected) 
            elif string.lower( self.gm_name ) == 'isoline':
               parent.vcs[ parent.vcs_id ].createisoline(new_name, gm_selected) 
            elif string.lower( self.gm_name ) == 'outfill':
               parent.vcs[ parent.vcs_id ].createoutfill(new_name, gm_selected) 
            elif string.lower( self.gm_name ) == 'outline':
               parent.vcs[ parent.vcs_id ].createoutline(new_name, gm_selected) 
            elif string.lower( self.gm_name ) == 'vector':
               parent.vcs[ parent.vcs_id ].createvector(new_name, gm_selected) 
            elif string.lower( self.gm_name ) == 'scatter':
               parent.vcs[ parent.vcs_id ].createscatter(new_name, gm_selected) 
            elif string.lower( self.gm_name ) == 'taylordiagram':
               parent.vcs[ parent.vcs_id ].createtaylordiagram(new_name, gm_selected) 
            elif string.lower( self.gm_name ) == 'xvsy':
               parent.vcs[ parent.vcs_id ].createxvsy(new_name, gm_selected) 
            elif string.lower( self.gm_name ) == 'xyvsy':
               parent.vcs[ parent.vcs_id ].createxyvsy(new_name, gm_selected) 
            elif string.lower( self.gm_name ) == 'yxvsx':
               parent.vcs[ parent.vcs_id ].createyxvsx(new_name, gm_selected) 
            elif string.lower( self.gm_name ) == 'continents':
               parent.vcs[ parent.vcs_id ].createcontinents(new_name, gm_selected) 
            elif string.lower( self.gm_name ) == 'meshfill':
               parent.vcs[ parent.vcs_id ].createmeshfill(new_name, gm_selected) 
         except:
            gui_message.error( 'Error occurred while trying to copy %s graphics method %s to %s. Make sure the new graphics method name does not already exist.' % (self.gm_name, gm_selected, new_name) )
            return

      # Redisplay the graphics method list
      gm_list = parent.vcs[ parent.vcs_id ].listelements( self.gm_name )
      self.gm_listbox.setlist( gm_list )
      name_index = gm_list.index( new_name )
      parent.panelDV.gm_listbox.select_set( name_index )
      parent.panelDV.gm_listbox.see( name_index )
   #
   ####### event to rename the graphics method
   #
   def rename_graphics_method(self, parent):
      # Get the graphics method name
      try:
         gm_selected = new_name = self.gm_listbox.getcurselection()[0]
      except:
         gui_message.error( 'Must select a %s graphics method from the list below.' % self.gm_name )
         return

      # Check for 'default' and 'ASD' template
      if ( (gm_selected[:7] == 'default') or (gm_selected[:3] == 'ASD') ):
         showerror( "Error Message to the User", "The default and ASD graphics methods cannot be modified. Please select another graphics method for modification or copy the graphics method under a new name.")
         return


      # Popup use dialog widget
      dialog = Pmw.PromptDialog(parent, title='Rename %s graphics method' % self.gm_name,
                                label_text='Enter %s Name:' % self.gm_name, 
				entryfield_labelpos='nw',
                                entry_width = 50,
                                defaultbutton=0, buttons=('OK', 'Cancel'))

      # Position dialog popup on top of the main GUI and wait for results
      parent_geom = parent.geometry()
      geom = string.split(parent_geom, '+')
      geom2= string.split(geom[0],'x')
      d1 = string.atoi( geom[1] ); d2 = string.atoi( geom[2] )
      d3 = string.atoi( geom2[0] ); d4 = string.atoi( geom2[1] )
      d5=d2+(d4/2)

      result = dialog.activate(globalMode = 'nograb', geometry= "+%d+%d" % (d1, d5) )

      if result == 'OK':
         new_name = dialog.component('entry').get()
         if new_name in parent.vcs[ parent.vcs_id ].listelements( self.gm_name ):
            gui_message.error( 'Error occurred while trying to copy %s graphics method %s to %s. Make sure the new graphics method name does not already exist.' % (self.gm_name, gm_selected, new_name) )
            return
         try:
            if string.lower( self.gm_name ) == 'boxfill':
               r = parent.vcs[ parent.vcs_id ].getboxfill(gm_selected) 
            elif string.lower( self.gm_name ) == 'isofill':
               r = parent.vcs[ parent.vcs_id ].getisofill(gm_selected) 
            elif string.lower( self.gm_name ) == 'isoline':
               r = parent.vcs[ parent.vcs_id ].getisoline(gm_selected) 
            elif string.lower( self.gm_name ) == 'outfill':
               r = parent.vcs[ parent.vcs_id ].getoutfill(gm_selected) 
            elif string.lower( self.gm_name ) == 'outline':
               r = parent.vcs[ parent.vcs_id ].getoutline(gm_selected) 
            elif string.lower( self.gm_name ) == 'vector':
               r = parent.vcs[ parent.vcs_id ].getvector(gm_selected) 
            elif string.lower( self.gm_name ) == 'scatter':
               r = parent.vcs[ parent.vcs_id ].getscatter(gm_selected) 
            elif string.lower( self.gm_name ) == 'taylordiagram':
               r = parent.vcs[ parent.vcs_id ].gettaylordiagram(gm_selected) 
            elif string.lower( self.gm_name ) == 'xvsy':
               r = parent.vcs[ parent.vcs_id ].getxvsy(gm_selected) 
            elif string.lower( self.gm_name ) == 'xyvsy':
               r = parent.vcs[ parent.vcs_id ].getxyvsy(gm_selected) 
            elif string.lower( self.gm_name ) == 'yxvsx':
               r = parent.vcs[ parent.vcs_id ].getyxvsx(gm_selected) 
            elif string.lower( self.gm_name ) == 'continents':
               r = parent.vcs[ parent.vcs_id ].getcontinents(gm_selected) 
            elif string.lower( self.gm_name ) == 'meshfill':
               r = parent.vcs[ parent.vcs_id ].getmeshfill(gm_selected) 
         except:
            gui_message.error( 'Error occurred while trying to copy %s graphics method %s to %s. Make sure the new graphics method name does not already exist.' % (self.gm_name, gm_selected, new_name) )
            return
         r.name = new_name

      # Redisplay the graphics method list
      gm_list = parent.vcs[ parent.vcs_id ].listelements( self.gm_name )
      self.gm_listbox.setlist( gm_list )
      name_index = gm_list.index( new_name )
      parent.panelDV.gm_listbox.select_set( name_index )
      parent.panelDV.gm_listbox.see( name_index )
   #
   ####### event to script the graphics method
   #
   def script_graphics_method(self, parent):
      # Get the graphics method name
      try:
         gm_selected = self.gm_listbox.getcurselection()[0]
      except:
         gui_message.error( 'Must select a %s graphics method from the list below.' % self.gm_name )
         return

      # Show the popup directory dialog
      filetypes = [ ("Python", ".py"), ("VCS", ".scr") ]
      save_dialog = tkFileDialog.asksaveasfilename(master=parent,
                       filetypes=filetypes,
                       title = 'Save %s graphics method %s to file' % (self.gm_name, gm_selected))

      if save_dialog == '': return
      if (save_dialog[-3:] != '.py') and (save_dialog[-4:] != '.scr'): save_dialog += '.py'

      if string.lower( self.gm_name ) == 'boxfill':
          r = parent.vcs[ parent.vcs_id ].getboxfill(gm_selected) 
      elif string.lower( self.gm_name ) == 'isofill':
         r = parent.vcs[ parent.vcs_id ].getisofill(gm_selected) 
      elif string.lower( self.gm_name ) == 'isoline':
         r = parent.vcs[ parent.vcs_id ].getisoline(gm_selected) 
      elif string.lower( self.gm_name ) == 'outfill':
         r = parent.vcs[ parent.vcs_id ].getoutfill(gm_selected) 
      elif string.lower( self.gm_name ) == 'outline':
         r = parent.vcs[ parent.vcs_id ].getoutline(gm_selected) 
      elif string.lower( self.gm_name ) == 'vector':
         r = parent.vcs[ parent.vcs_id ].getvector(gm_selected) 
      elif string.lower( self.gm_name ) == 'scatter':
         r = parent.vcs[ parent.vcs_id ].getscatter(gm_selected) 
      elif string.lower( self.gm_name ) == 'taylordiagram':
         r = parent.vcs[ parent.vcs_id ].gettaylordiagram(gm_selected) 
      elif string.lower( self.gm_name ) == 'xvsy':
         r = parent.vcs[ parent.vcs_id ].getxvsy(gm_selected) 
      elif string.lower( self.gm_name ) == 'xyvsy':
         r = parent.vcs[ parent.vcs_id ].getxyvsy(gm_selected) 
      elif string.lower( self.gm_name ) == 'yxvsx':
         r = parent.vcs[ parent.vcs_id ].getyxvsx(gm_selected) 
      elif string.lower( self.gm_name ) == 'continents':
         r = parent.vcs[ parent.vcs_id ].getcontinents(gm_selected) 
      elif string.lower( self.gm_name ) == 'meshfill':
         r = parent.vcs[ parent.vcs_id ].getmeshfill(gm_selected) 

      # Script the graphics method selected in the list
      parent.vcs[ parent.vcs_id ].scriptobject( r, save_dialog, mode = 'w' )

      # Redisplay the graphics method list
      gm_list = parent.vcs[ parent.vcs_id ].listelements( self.gm_name )
      name_index = gm_list.index( gm_selected )
      parent.panelDV.gm_listbox.select_set( name_index )
      parent.panelDV.gm_listbox.see( name_index )

   #
   ####### event to remove the graphics method
   #
   def remove_graphics_method(self, parent):
      # Get the graphics method name
      try:
         gm_selected = self.gm_listbox.getcurselection()[0]
      except:
         gui_message.error( 'Must select a %s graphics method from the list below.' % self.gm_name )
         return

      if gm_selected not in parent.vcs[ parent.vcs_id ].listelements( self.gm_name ):
            gui_message.error( 'Error occurred while trying to remove %s graphics method %s from the list. Make sure the graphics method name exist in the list.' % (self.gm_name, gm_selected) )
            return

      if ((gm_selected[0:7] == 'default') or (gm_selected[0:3] == 'ASD')):
            gui_message.error( 'Removal of the %s %s graphics method is not permitted.' % (gm_selected, self.gm_name) )
            return

      button = askquestion("Remove %s graphics method" % self.gm_name, "Do you really want to REMOVE %s from the %s graphics method list?" % (gm_selected, self.gm_name), default='no')

      if button in ['yes', True]:
         if string.lower( self.gm_name ) == 'boxfill':
            r = parent.vcs[ parent.vcs_id ].getboxfill(gm_selected) 
         elif string.lower( self.gm_name ) == 'isofill':
            r = parent.vcs[ parent.vcs_id ].getisofill(gm_selected) 
         elif string.lower( self.gm_name ) == 'isoline':
            r = parent.vcs[ parent.vcs_id ].getisoline(gm_selected) 
         elif string.lower( self.gm_name ) == 'outfill':
            r = parent.vcs[ parent.vcs_id ].getoutfill(gm_selected) 
         elif string.lower( self.gm_name ) == 'outline':
            r = parent.vcs[ parent.vcs_id ].getoutline(gm_selected) 
         elif string.lower( self.gm_name ) == 'vector':
            r = parent.vcs[ parent.vcs_id ].getvector(gm_selected) 
         elif string.lower( self.gm_name ) == 'scatter':
            r = parent.vcs[ parent.vcs_id ].getscatter(gm_selected) 
         elif string.lower( self.gm_name ) == 'taylordiagram':
            r = parent.vcs[ parent.vcs_id ].gettaylordiagram(gm_selected) 
         elif string.lower( self.gm_name ) == 'xvsy':
            r = parent.vcs[ parent.vcs_id ].getxvsy(gm_selected) 
         elif string.lower( self.gm_name ) == 'xyvsy':
            r = parent.vcs[ parent.vcs_id ].getxyvsy(gm_selected) 
         elif string.lower( self.gm_name ) == 'yxvsx':
            r = parent.vcs[ parent.vcs_id ].getyxvsx(gm_selected) 
         elif string.lower( self.gm_name ) == 'continents':
            r = parent.vcs[ parent.vcs_id ].getcontinents(gm_selected) 
         elif string.lower( self.gm_name ) == 'meshfill':
            r = parent.vcs[ parent.vcs_id ].getmeshfill(gm_selected) 
         parent.vcs[ parent.vcs_id ].removeobject( r )

         # Redisplay the graphics method list
         gm_list = parent.vcs[ parent.vcs_id ].listelements( self.gm_name )
         self.gm_listbox.setlist( gm_list )
   #
   ####### event to select the desired template
   #
   def select_template(self):
       try:
          self.template_bin_name = self.template_listbox.getcurselection()[0]
       except:
          pass
   def evt_cdat_demo(self,parent):
      os.popen("cdatdemo -hide_vcdat &")

   #
   ####### event to select the desired graphics method
   #
   def select_gm(self):
      self.bin_type = self.gm_name
      try:
         self.gm_bin_name = self.gm_listbox.getcurselection()[0]
      except:
         pass
   #
   ####### event to change the command line expression color
   #
   def evt_change_command_color( self, parent, event ):
      keycolor = Pmw.Color.changebrightness(parent, 'red', 0.85)
      self.command.configure( entry_background = keycolor )
   #
   ####### event to call the command line calculator expression
   #
   def evt_compute_command( self, parent, event ):
      # change backgound color to white
      self.command.configure( entry_background = gui_color.six )
      # get the command statement or expression
      command = self.command.get()
      # exec runs statements, so try the exec command with __main__ as the
      # global name space.
      try:
           command = string.strip(command)  # Strip leading and/or trailing whitespaces from the command
           exec( command, __main__.__dict__ )
           self.command.setentry( "" )
           update_defined( )
           gui_control.record_command(parent, "\n# Recorded command from the Defined Variables\n# expression entry panel", 1 )
           gui_control.record_command(parent, "%s" % (command), 1 )
      except:
           gui_message.error("Bad statement or expression! Check the command and try again.")
   #
   ####### event to call the command line calculator expression
   #
   def evt_search_command( self, parent, event ):
      # change backgound color to white
##       self.search.configure( entry_background = gui_color.four )
      # get the command statement or expression
      command = self.search.get().replace(" ","+")
      webbrowser.open('http://www-pcmdi.llnl.gov/software-portal/search?SearchableText=%s' % command)
      # exec runs statements, so try the exec command with __main__ as the
      # global name space.
   #
   ####### event to call the gui_alter_variable popup
   #
   def evt_show_defined_variables_tools( self, parent, event ):
      if parent.show_defined_variables_tools == 0:
            parent.show_defined_variables_tools = 1
            parent.balloon.bind( self.canvas_develicon, "Show defined variables tools." )
            self.canvas_tgicon.pack_forget( )
            self.canvas_editicon.pack_forget( )
            self.canvas_saveicon.pack_forget( )
            self.canvas_infoicon.pack_forget( )
            self.canvas_dediticon.pack_forget( )
            self.canvas_recycleicon.pack_forget( )
            self.canvas_logicon.pack_forget( )
            self.canvas_trashcanicon.pack_forget( )
       #     self.command.pack_forget( )
      else:
            parent.show_defined_variables_tools = 0
            parent.balloon.bind( self.canvas_develicon, "Hide defined variables tools." )
            self.canvas_tgicon.pack( side='left', after=self.canvas_develicon, fill=Tkinter.BOTH, expand='no', padx=1, pady=1 )
            self.canvas_editicon.pack( side='left', after=self.canvas_tgicon, fill=Tkinter.BOTH, expand='no', padx=1, pady=1 )
            self.canvas_saveicon.pack( side='left', after=self.canvas_editicon, fill=Tkinter.BOTH, expand='no', padx=1, pady=1 )
            self.canvas_infoicon.pack( side='left', after=self.canvas_saveicon, fill=Tkinter.BOTH, expand='no', padx=1, pady=1 )
            self.canvas_dediticon.pack( side='left', after=self.canvas_infoicon, fill=Tkinter.BOTH, expand='no', padx=1, pady=1 )
            self.canvas_recycleicon.pack( side='left', after=self.canvas_dediticon, fill=Tkinter.BOTH, expand='no', padx=1, pady=1 )
            self.canvas_logicon.pack( side='left', after=self.canvas_recycleicon, fill=Tkinter.BOTH, expand='no', padx=1, pady=1 )
            self.canvas_trashcanicon.pack( side='left', after=self.canvas_logicon, fill=Tkinter.BOTH, expand='no', padx=1, pady=1 )
       #     self.command.pack(side = 'left', fill='x', padx = 10)
            pass
   #
   ####### event to call the gui_alter_variable popup
   #
   def evt_show_template_graphics_method( self, parent, event ):
      if parent.show_template_graphics_method == 0:
        parent.show_template_graphics_method = 1
        try:
           self.framet.pack_forget( )
           self.frameg.pack_forget( )
           parent.pl.pmain_menu.pack_forget( )
           parent.pl.scl_frame.pack_forget( )
           parent.panelVI.scl1.pack( fill = 'both', expand=1, pady=3 )
        except:
           pass
      else:
        parent.show_template_graphics_method = 0
        try:
           self.framet.pack(side='left', before=self.framec, expand=1, fill='both')
           self.frameg.pack(side='left', after=self.framet, fill ='both', expand=1, padx = 5)
           parent.pl.pmain_menu.pack( side='top', fill='both' )
           parent.pl.scl_frame.pack( side='top', fill='both' )
           parent.panelVI.scl1.pack_forget( )
        except:
           pass
      try:
         self.alterpaneDV(parent)
      except:
         pass
   #
   ####### event to call the gui_alter_variable popup
   #
   def evt_call_gui_alter_variable( self, parent, event ):
      gui_alter_variable.create( parent )
   #
   ####### event to call the writenetCDF popup
   #
   def evt_call_gui_writenetcdf( self, parent, event ):
      gui_writenetcdf.create( parent )
   #
   ####### event to remove the selected defined
   #
   def update_page_layout( self, parent, remove_list):
      # Make sure the buttons are all turned off. This will clear the VCS Canvas
         for i in parent.pl.form.keys():
             gm_type =  parent.pl.form[i].gm.cget( 'label_text' )
             if gm_type not in [ "Vector", "Scatter", "XvsY"] or (gm_type=="Meshfill" and parent.panelDV.selected == []):
                if parent.pl.form[i].data1.get( ) in remove_list:
                   parent.pl.evt_remove_form( i, parent, None )
             else:
                if (parent.pl.form[i].data1.get( ) in remove_list) and (parent.pl.form[i].data2.get( ) in remove_list):
                   parent.pl.evt_remove_form( i, parent, None )
                elif (parent.pl.form[i].data1.get( ) in remove_list) or (parent.pl.form[i].data2.get( ) in remove_list):
                   if parent.pl.form[i].data1.get( ) in remove_list:
                      parent.pl.form[i].data1.setentry( "" )
                   elif parent.pl.form[i].data2.get( ) in remove_list:
                      parent.pl.form[i].data2.setentry( "" )

                   parent.pl.form[i].display.off = 1
                   parent.pl.form[i].off_btn.create_image(0,0, anchor=Tkinter.NW, image=parent.pl.form[i].off_btnimage )
                   parent.pl.form[i].lbl.configure( label_text = 'Off' )
                   vcs = parent.vcs[ parent.vcs_id ].update()


   #
   ####### event to remove the selected defined
   #
   def evt_remove_selected_defined( self, parent, var_name, event):
         if var_name == None:
            selected = parent.panelDV.scl1.getcurselection()
         else:
            selected = var_name
         if selected == ():
            gui_message.error( "You did not select a variable to move to the trashcan." )
            return
         gui_control.record_command( parent, "\n# Rename slab for disposal later",1 )
         trash_can_full = 0
         for x in selected: 
            a = string.split( x, ' ')[1]
            time_str = trash_store
            for y in time.localtime()[0:-3]:
               time_str += '%i_' % y
            b = a + time_str[:-1] + trash_str
            c = b + x[(gui_control.dvsize + len(a)):]
            parent.panelDV.lst1[ x[gui_control.dvsize:] ].id = b
            parent.panelDV.lst1[ c ] = __main__.__dict__[ b ] = parent.panelDV.lst1[ x[gui_control.dvsize:] ]

            del parent.panelDV.lst1[ x[gui_control.dvsize:] ]
            del parent.panelDV.number_lst1[ x[gui_control.dvsize:] ]
            del __main__.__dict__[a]
            trash_can_full = 1
            gui_control.record_command(parent,("%s = %s" % (b.split(trash_str)[0],a)), 1 )
            gui_control.record_command(parent,("del %s" % a), 1 )
         d=parent.panelDV.lst1.keys()
         d.sort()

         # Remove page layout forms if necessary
         r_list = []
         for x in selected:
             r_list.append( string.split(x, ' ')[1] )
         self.update_page_layout( parent, r_list )

         update_defined_variable_list( parent )
         #
         if var_name == None: gui_functions._blank_dim1_to_ndim( parent )
         #
         if trash_can_full != 0:
            self.canvas_trashcanicon.create_image(0,0, anchor=Tkinter.NW, image=self.trashcan_full_image )

   #
   ####### event to remove all the defined
   #
   def evt_remove_all_defined( self, parent, event ):
         got = parent.panelDV.scl1.get( 0, 'end' )
         gui_control.record_command( parent, "\n# Rename slab for disposal later", 1 )
         trash_can_full = 0
         for x in got: 
            a = string.split( x, ' ')[1]
            time_str = trash_store
            for y in time.localtime()[0:-3]:
               time_str += '%i_' % y
            b = a + time_str[:-1] + trash_str
            c = b + x[(gui_control.dvsize + len(a)):]
            parent.panelDV.lst1[ x[gui_control.dvsize:] ].id = b
            parent.panelDV.lst1[ c ] = __main__.__dict__[ b ] = parent.panelDV.lst1[ x[gui_control.dvsize:] ]

            del parent.panelDV.lst1[ x[gui_control.dvsize:] ]
            del parent.panelDV.number_lst1[ x[gui_control.dvsize:] ]
            del __main__.__dict__[a]
            trash_can_full = 1
            gui_control.record_command(parent,("%s = %s" % (b.split(trash_str)[0],a)), 1 )
            gui_control.record_command(parent,("del %s" % a), 1 )

         d=parent.panelDV.lst1.keys()
         d.sort()

         if parent.panelDV.selected_list != {}:
            gui_functions._blank_dim1_to_ndim( parent )

         # Remove page layout forms if necessary
         r_list = []
         for x in got:
             r_list.append( string.split(x, ' ')[1] )
         self.update_page_layout( parent, r_list )

         update_defined_variable_list( parent )
         #
         if trash_can_full != 0:
            self.canvas_trashcanicon.create_image(0,0, anchor=Tkinter.NW, image=self.trashcan_full_image )

         # It seems as if I need to do a clear to make sure that everything is re-initialized
         parent.panelGC.evt_clear_display( parent )

   #
   ####### event to return information on the defined variables
   #
   def evt_info_on_defined( self, parent, event ):
        info_text = ''
        size = len( parent.panelDV.selected_list.keys() )
        if size != 0:
           for i in range( size ):
              v = parent.panelDV.selected_list.values()[ size - i -1 ]

              self.dialog = Pmw.Dialog(parent,
                buttons = ('Dismiss',),
                defaultbutton = 'OK',
                title = '%s  -  Defined Variable Information' % v,
                command = gui_control.Command(self.evt_dispose_execute, parent, None)
                )
              self.dialog.transient( parent ) # draw widget on top of its parent

	      # Create the ScrolledText.
	      sclt = Pmw.ScrolledText(self.dialog.interior(),
		borderframe = 1,
		labelpos = 'n',
		usehullsize = 1,
		hull_width = 400,
		hull_height = 300,
                text_background = 'white',
                text_foreground = 'black',
		text_padx = 10,
		text_pady = 10,
		text_wrap='none'
	      )
              sclt.pack(padx = 5, pady = 5, fill = 'both', expand = 1)

              # Write slab information to text window
              slab_list = parent.panelDV.lst1[ v ].listall() 
              sclt.configure( text_state = 'normal' )
              first_time = 1
              for x in slab_list:
                 if string.find( x, " Dimension " ) != -1:
                    if first_time:
                       ln=len(parent.panelDV.lst1[ v ].shape)
                       num_dim = 'Number of dimensions: %d' % ln
                       sclt.insert( 'end',( num_dim + '\n') )
                       first_time = 0
                    sclt.insert( 'end', '\n' )
                 sclt.insert( 'end', (x + '\n') )
              sclt.configure( text_state = 'disabled' )

              # Position dialog popup
              parent_geom = parent.geometry()
              geom = string.split(parent_geom, '+')
              geom2 = string.split(geom[0], 'x')
              d1 = string.atoi( geom[1] )
              d2 = string.atoi( geom[2] )
              d3 = string.atoi( geom2[0] )
              d4 = string.atoi( geom2[1] ) * 0.88
              self.dialog.activate( geometry="%dx%d+%d+%d" % (d3, d4, d1, d2) )

              # Clear info_text for next defined variable
              info_text = ''
        else:
           gui_message.error('No variable(s) selected in the "Defined Variables" list window.')

   #
   ####### event to return information on the defined variables
   #
   def evt_log_on_defined( self, parent, event ):
        self.dialog = Pmw.Dialog(parent,
                buttons = ('Dismiss',),
                defaultbutton = 'OK',
                title = 'Logged information about Defined Variables',
                command = gui_control.Command(self.evt_dispose_execute, parent, None)
                )
        self.dialog.transient( parent ) # draw widget on top of its parent

        log_text = 'Defined Variables:\n\n'
        var_list = []
        for x in parent.panelDV.lst1.keys():
           a = string.split( x, ' ')[0]
           if a[-len(trash_str):] != trash_str:
              var_list.append( x )
        var_list.sort()
        total_defined_size = 0
        for x in var_list:
            v = parent.panelDV.lst1[ x ]
            data_size = 8
            if (v.dtype.char == 'f') or (v.dtype.char == 'i'): data_size = 4
            for l in v.shape: data_size *= l
            total_defined_size += data_size
            log_text += '\tVariable %s is %d bytes' % (x, data_size)
            log_text += '\n'
        log_text += '\n\tTotal size of Defined Variables is %d bytes\n\n\n' % total_defined_size

        log_text += 'Defined Variables in Trashcan:\n\n'
        var_list = []
        total_trash_can_size = 0
        for x in parent.panelDV.lst1.keys():
           a = string.split( x, ' ')[0]
           if a[-len(trash_str):] == trash_str:
              vn =  a[:len(a)-len(trash_str)] + x[len(a):]
              v = parent.panelDV.lst1[ x ]
              var_list.append( a[:len(a)-len(trash_str)] + x[len(a):] )
              data_size = 8
              if (v.dtype.char == 'f') or (v.dtype.char == 'i'): data_size = 4
              for l in v.shape: data_size *= l
              total_trash_can_size += data_size
              log_text += '\tVariable %s is %d bytes' % (vn, data_size)
              log_text += '\n'
        log_text += '\n\tTotal size of Defined Variables in Trashcan is %d bytes\n\n\n' % total_trash_can_size

        total_size = total_defined_size + total_trash_can_size
        log_text += 'The Combined Total Size is %d bytes\n\n' % total_size

	# Create the ScrolledText.
	sclt = Pmw.ScrolledText(self.dialog.interior(),
		borderframe = 1,
		labelpos = 'n',
		usehullsize = 1,
		hull_width = 400,
		hull_height = 300,
                text_background = 'white',
                text_foreground = 'black',
		text_padx = 10,
		text_pady = 10,
		text_wrap='none'
	)
        sclt.pack(padx = 5, pady = 5, fill = 'both', expand = 1)
        sclt.settext( log_text )

        # Position dialog popup
        parent_geom = parent.geometry()
        geom = string.split(parent_geom, '+')
        geom2 = string.split(geom[0], 'x')
        d1 = string.atoi( geom[1] )
        d2 = string.atoi( geom[2] )
        d3 = string.atoi( geom2[0] )
        d4 = string.atoi( geom2[1] ) * 0.55
        self.dialog.activate( geometry="%dx%d+%d+%d" % (d3, d4, d1, d2) )

   def return_trash_can_list( self, parent ):
        var_list = []
        total_trash_can_size = 0
        for x in parent.panelDV.lst1.keys():
           a = string.split( x, ' ')[0]
           if a[-len(trash_str):] == trash_str:
              v = parent.panelDV.lst1[ x ]
              data_size = 8
              if (v.dtype.char == 'f') or (v.dtype.char == 'i'): data_size = 4
              for l in v.shape: data_size *= l
              total_trash_can_size += data_size
              var_list.append( a[:len(a)-len(trash_str)] + x[len(a):] )
        var_list.sort()

        return var_list, total_trash_can_size
   #
   ####### event to dispose of unwanted variables
   #
   def evt_dispose_defined_variables( self, parent, event ):
        self.dialog = Pmw.Dialog(parent,
                buttons = ('Dispose\nAll', 'Dispose\nSelected', 'Restore\nAll', 'Restore\nSelected', 'Dismiss'),
                defaultbutton = 'OK',
                title = 'Trashcan for Disposing of Unwanted Variables',
                command = gui_control.Command(self.evt_dispose_execute, parent, None)
                )
        self.dialog.transient( parent ) # draw widget on top of its parent

        var_list, total_trash_can_size = self.return_trash_can_list( parent )
        
        # Create the "Trashcan" scroll list that will show the unwanted variables
        self.scl1_trash_can = Pmw.ScrolledListBox( self.dialog.interior(),
                             items=var_list,
                             listbox_selectbackground=gui_color.six,
                             listbox_selectforeground='black',
                             listbox_background = 'white',
                             listbox_foreground = 'black',
                             listbox_exportselection = Tkinter.NO,
                             listbox_selectmode=Tkinter.MULTIPLE,
                             horizscrollbar_width=gui_control.scl_width,
                             vertscrollbar_width=gui_control.scl_width,
                             selectioncommand=gui_control.Command(self.evt_selected_trash_variable, parent)
                             )
        self.scl1_trash_can.pack( side='top', padx=3, pady=0, fill='both', expand=1 )
        parent.balloon.bind(self.scl1_trash_can, "Select the unwanted variables for disposal or retrieval.\n\nNote: The trailing numbers after the variable name(s) represent\nthe time of placement in the trashcan. The numbers are\nrepresented as _yyyy:m[m]:d[d]:h[h]:m[m]:s[s]." )

        # Create the "Selected Info" contents of the dialog.
        group = Pmw.Group(self.dialog.interior(), tag_text = 'Trashcan Infomation')
        group.pack(side='top', fill = 'both', expand = 1, padx = 3)
        self.textv_lbl1 = Tkinter.StringVar()
        self.textv_lbl1.set('Selected Deletion Date: None')
        lbl1=Tkinter.Label(group.interior(),
            textvariable = self.textv_lbl1,
            justify = 'left',
            anchor = 'w',
            )
        lbl1.pack( side='top', expand = 1, fill = 'both', padx=10, pady=3 )
        self.textv_lbl2 = Tkinter.StringVar()
        self.textv_lbl2.set('Selected Size: 0 bytes')
        lbl2=Tkinter.Label(group.interior(),
            textvariable = self.textv_lbl2,
            justify = 'left',
            anchor = 'w',
            )
        lbl2.pack( side='top', expand = 1, fill = 'both', padx=10, pady=3 )

        # Create the "Trashcan" size
        self.textv_lbl3 = Tkinter.StringVar()
        self.textv_lbl3.set('Total Trash Size: %d bytes' % total_trash_can_size)
        lbl3=Tkinter.Label(group.interior(),
            textvariable = self.textv_lbl3,
            justify = 'left',
            anchor = 'w',
            )
        lbl3.pack( side='top', expand = 1, fill = 'both', padx=10, pady=3 )

        # Position dialog popup
        parent_geom = parent.geometry()
        geom = string.split(parent_geom, '+')
        d1 = string.atoi( geom[1] )
        d2 = string.atoi( geom[2] )
        self.dialog.activate( geometry="+%d+%d" % (d1, d2) )

   def evt_selected_trash_variable( self, parent ):
      a = self.scl1_trash_can.getcurselection()
      total_selected_size=0
      selected_ct = 0
      for x in a:
         x1 = string.split( x, ' ')[0]
         for y in parent.panelDV.lst1.keys():
            y1 = string.split( y, ' ')[0][:-len(trash_str)]
            if x1 == y1:
               v = parent.panelDV.lst1[ y ]
               data_size = 8
               if (v.dtype.char == 'f') or (v.dtype.char == 'i'): data_size = 4
               for l in v.shape: data_size *= l
               total_selected_size += data_size
               selected_ct += 1
      #
      if selected_ct > 1:
         self.textv_lbl1.set('%d items selected.' % selected_ct )
      elif selected_ct == 1:
         dt = string.split(string.split(x1,trash_store)[-1],'_')
         date = dt[1] + '/' + dt[2] + '/' + dt[0]
         time = dt[3] + ':' + dt[4] + ':' + dt[5]
         self.textv_lbl1.set('Selected Delection Date: %s,  at time: %s' % (date, time) )
      else:
         self.textv_lbl1.set('Selected Deletion Date: None' )
      self.textv_lbl2.set('Selected Size: %d bytes' % total_selected_size)


   def evt_dispose_execute( self, parent, var_names, result ):
      if result == 'Dispose\nAll':
         button = askquestion("Trashcan Question:", "Do you really want to empty the trashcan?", default='no')
         if button in ['yes', True]:
            gui_control.record_command(parent, "\n# Dispose of all Variable in 'Trashcan'" , 1 )
            for x in parent.panelDV.lst1.keys():
               a = string.split( x, ' ')[0]
               if a[-len(trash_str):] == trash_str:
                  del parent.panelDV.lst1[ x ]
                  del __main__.__dict__[a]
                  gui_control.record_command(parent, "del %s" % a.split(trash_str)[0] , 1 )
           #
            self.canvas_trashcanicon.create_image(0,0, anchor=Tkinter.NW, image=self.trashcan_empty_image )
         self.dialog.destroy()
      elif result == 'Dispose\nSelected':
         # If var_name = None, then it was called from the icon gui, otherwise it was called
         # from QuickPlot.
         if var_names == None:
            selected_trash = self.scl1_trash_can.curselection()
         else:
            selected_trash = var_names
            button = 'yes'
            if selected_trash == (): return

         if selected_trash == ():
            gui_message.info( "You did not select a variable." )
            return
         if var_names == None:
            button = askquestion("Trashcan Question:", "Do you really want to remove the selected variable(s) from the trashcan?", default='no')
         if button in ['yes', True]:
            gui_control.record_command(parent, "\n# Dispose of selected Variable in 'Trashcan'" , 1 )
            if var_names == None:
               a = self.scl1_trash_can.getcurselection()
            else:
               a = var_names
            for x in a:
               x1 = string.split( x, ' ')[0]
               for y in parent.panelDV.lst1.keys():
                  y1 = string.split( y, ' ')[0][:-len(trash_str)]
                  if x1 == y1:
                     del parent.panelDV.lst1[ y ]
                     del __main__.__dict__[ string.split( y, ' ')[0] ]
                     gui_control.record_command(parent, "del %s" % string.split( y, ' ')[0].split(trash_str)[0]  , 1 )
            #
            trash_size = 0
            for x in parent.panelDV.lst1.keys():
               a = string.split( x, ' ')[0]
               if a[-len(trash_str):] == trash_str:
                  trash_size +=1
            if trash_size < 1:
               self.canvas_trashcanicon.create_image(0,0, anchor=Tkinter.NW, image=self.trashcan_empty_image )
         else:
            return
         #
         if var_names == None:
            for i in range(len(selected_trash)):
               self.scl1_trash_can.delete( selected_trash[len(selected_trash)-i-1] )
            self.textv_lbl1.set('Selected Deletion Date: None' )
            self.textv_lbl2.set('Selected Size: 0 bytes')
            total_trash_can_size = 0
            for x in parent.panelDV.lst1.keys():
              a = string.split( x, ' ')[0]
              if a[-len(trash_str):] == trash_str:
                 v = parent.panelDV.lst1[ x ]
                 data_size = 8
                 if (v.dtype.char == 'f') or (v.dtype.char == 'i'): data_size = 4
                 for l in v.shape: data_size *= l
                 total_trash_can_size += data_size
            self.textv_lbl3.set('Total Trash Size: %d bytes' % total_trash_can_size)

      elif result == 'Restore\nAll':
         for x in parent.panelDV.lst1.keys():
            a = string.split( x, ' ')[0]
            if a[-len(trash_str):] == trash_str:
               b = a[:-len(trash_str)]
               c = b + x[len(a):]
               parent.panelDV.lst1[ x ].id = b
               parent.panelDV.lst1[ c ] = __main__.__dict__[ b ] = parent.panelDV.lst1[ x ]
               parent.panelDV.number_lst1[ c ] = gui_control.dvholder + c
               #
               del parent.panelDV.lst1[ x ]
               del __main__.__dict__[ a ]
         #
         update_defined_variable_list( parent )
         #
         self.canvas_trashcanicon.create_image(0,0, anchor=Tkinter.NW, image=self.trashcan_empty_image )
         self.dialog.destroy()
      elif result == 'Restore\nSelected':
         a = self.scl1_trash_can.getcurselection()
         selected_trash = self.scl1_trash_can.curselection()
         if selected_trash == ():
            gui_message.info( "You did not select a variable." )
            return
         for x in a:
            x1 = string.split( x, ' ')[0]
            for y in parent.panelDV.lst1.keys():
               y2 = string.split( y, ' ')[0]
               y1 = string.split( y, ' ')[0][:-len(trash_str)]
               if x1 == y1:
                  c = x1 + y[len(y2):]
                  parent.panelDV.lst1[ y ].id = y1
                  parent.panelDV.lst1[ c ] = __main__.__dict__[ y1 ] = parent.panelDV.lst1[ y ]
                  parent.panelDV.number_lst1[ c ] = gui_control.dvholder + c
                  #
                  del parent.panelDV.lst1[ y ]
                  del __main__.__dict__[ y2 ]
         #
         update_defined_variable_list( parent )
         #
         #
         trash_size = 0
         for x in parent.panelDV.lst1.keys():
            a = string.split( x, ' ')[0]
            if a[-len(trash_str):] == trash_str:
               trash_size +=1
         if trash_size < 1:
            self.canvas_trashcanicon.create_image(0,0, anchor=Tkinter.NW, image=self.trashcan_empty_image )
         #
         for i in range(len(selected_trash)):
            self.scl1_trash_can.delete( selected_trash[len(selected_trash)-i-1] )
         self.textv_lbl1.set('Selected Deletion Date: None' )
         self.textv_lbl2.set('Selected Size: 0 bytes')
         total_trash_can_size = 0
         for x in parent.panelDV.lst1.keys():
           a = string.split( x, ' ')[0]
           if a[-len(trash_str):] == trash_str:
              v = parent.panelDV.lst1[ x ]
              data_size = 8
              if (v.dtype.char == 'f') or (v.dtype.char == 'i'): data_size = 4
              for l in v.shape: data_size *= l
              total_trash_can_size += data_size
         self.textv_lbl3.set('Total Trash Size: %d bytes' % total_trash_can_size)
      else:
         self.dialog.destroy()
#
####### event to set the mode operation on the calculation operator
#
   def evt_set_mode_operation( self, parent, event ):
      if parent.calculate_mode == 1:
          self.func_mode_icon.create_image(0,0, anchor=Tkinter.NW,
                                         image=self.func_mode2_image )
          parent.calculate_mode = 2
      else:
          self.func_mode_icon.create_image(0,0, anchor=Tkinter.NW,
                                         image=self.func_mode1_image )
          parent.calculate_mode = 1
          try:
             self.gui_expression.expression.clear()
          except:
             pass
          self.doing_calculation_type = ''
          self.saved_expression = ''
          self.build_expression = 'no'

#
####### event to display the expression widget
#
   def evt_expression_widget( self, parent, event ):
      if self.gui_expression == None:
         fn_name = parent.menu.pcmdi_tools_menu.return_unique_name( '_calculation' )
         self.gui_expression = gui_expression.create( parent, self, fn_name, self.saved_expression )

#
####### event to operate on the selected 'Data' in the scroll window
#
   def evt_do_operation( self, parent, type, event ):
      if parent.calculate_mode == 1:
#         self.gui_expression.expression.clear()
#         print 'clear', dir(self.gui_expression)
#         self.doing_calculation_type = ''
#         self.saved_expression = ''
#         self.build_expression = 'no'
         self.mode_1_operation( parent, type, event )
      else:
         self.mode_2_operation( parent, type, event )
#
####### Mode 1 operates on the selected 'Data' in the scroll window
####### without displaying the "Expression" widget.
#
   def mode_1_operation( self, parent, type, event ):
      lst_ct = len( parent.panelDV.selected_list.keys() )
      if (type in [ 'regrid','grower']) and (lst_ct < 2):
          gui_message.error( "Cannot do '%s' operation. Must have two variables selected in the 'Defined Variables' window located to the left.." % type )
          return
      elif (lst_ct < 1):
          gui_message.error( "Cannot do '%s' operation. Must have at least one variable selected in the 'Defined Variables' window located to the left.." % type )
          return
      elif ( (type in [ 'add', 'subtract', 'multiply', 'divide', 'power','root','less','greater','equal' ])
              and (lst_ct == 1) ):
          gui_output.create(parent, 'operation', "Return the Number for Operation.",
                "What number do you want to %s?"%type, 1) 
          if parent.ret_val == None:
             return
          gui_control.record_command(parent, "\n# Record the %s" % type , 1 )
          if type == 'add':
             var = parent.panelDV.lst1[ parent.panelDV.selected ] + parent.ret_val
             ret_val = string.replace( ('%s'%parent.ret_val), '.', '_')
             var.id = var.name = '_%s_%s_%s'%(type,parent.panelDV.lst1[ parent.panelDV.selected ].id,ret_val)
             gui_control.record_command( parent, "%s = %s + %s" % (var.id, parent.panelDV.lst1[ parent.panelDV.selected ].id, parent.ret_val), 1)
          elif type == 'subtract':
             var = parent.panelDV.lst1[ parent.panelDV.selected ] - parent.ret_val
             ret_val = string.replace( ('%s'%parent.ret_val), '.', '_')
             var.id = var.name = '_%s_%s_%s'%(type,parent.panelDV.lst1[ parent.panelDV.selected ].id,ret_val)
             gui_control.record_command( parent, "%s = %s - %s" % (var.id, parent.panelDV.lst1[ parent.panelDV.selected ].id, parent.ret_val), 1)
          elif type == 'multiply':
             var = parent.panelDV.lst1[ parent.panelDV.selected ] * parent.ret_val
             ret_val = string.replace( ('%s'%parent.ret_val), '.', '_')
             var.id = var.name = '_%s_%s_%s'%(type,parent.panelDV.lst1[ parent.panelDV.selected ].id,ret_val)
             gui_control.record_command( parent, "%s = %s * %s" % (var.id, parent.panelDV.lst1[ parent.panelDV.selected ].id, parent.ret_val), 1)
          elif type == 'divide':
             var = parent.panelDV.lst1[ parent.panelDV.selected ] / parent.ret_val
             ret_val = string.replace( ('%s'%parent.ret_val), '.', '_')
             var.id = var.name = '_%s_%s_%s'%(type,parent.panelDV.lst1[ parent.panelDV.selected ].id,ret_val)
             gui_control.record_command( parent, "%s = %s / %s" % (var.id, parent.panelDV.lst1[ parent.panelDV.selected ].id, parent.ret_val), 1)
          elif type == 'root':
             var = MV2.power( parent.panelDV.lst1[ parent.panelDV.selected ],
                             1./parent.ret_val )
             ret_val = string.replace( ('%s'%parent.ret_val), '.', '_')
             var.id = var.name = '_%s_%s_%s'%(type,parent.panelDV.lst1[ parent.panelDV.selected ].id,ret_val)
             gui_control.record_command( parent, "%s = MV2.power( %s, 1.0/%s )" % (var.id, parent.panelDV.lst1[ parent.panelDV.selected ].id, parent.ret_val), 1)
          elif type == 'power':
             var = MV2.power( parent.panelDV.lst1[ parent.panelDV.selected ],
                             parent.ret_val )
             ret_val = string.replace( ('%s'%parent.ret_val), '.', '_')
             var.id = var.name = '_%s_%s_%s'%(type,parent.panelDV.lst1[ parent.panelDV.selected ].id,ret_val)
             gui_control.record_command( parent, "%s = MV2.power( %s, %s )" % (var.id, parent.panelDV.lst1[ parent.panelDV.selected ].id, parent.ret_val), 1)
          elif type == 'less':
             var = MV2.less( parent.panelDV.lst1[ parent.panelDV.selected ],
                             parent.ret_val )
             ret_val = string.replace( ('%s'%parent.ret_val), '.', '_')
             var.id = var.name = '_%s_%s_%s'%(type,parent.panelDV.lst1[ parent.panelDV.selected ].id,ret_val)
             gui_control.record_command( parent, "%s = MV2.less( %s, %s )" % (var.id, parent.panelDV.lst1[ parent.panelDV.selected ].id, parent.ret_val), 1)
          elif type == 'greater':
             var = MV2.greater( parent.panelDV.lst1[ parent.panelDV.selected ],
                             parent.ret_val )
             ret_val = string.replace( ('%s'%parent.ret_val), '.', '_')
             var.id = var.name = '_%s_%s_%s'%(type,parent.panelDV.lst1[ parent.panelDV.selected ].id,ret_val)
             gui_control.record_command( parent, "%s = MV2.greater( %s, %s )" % (var.id, parent.panelDV.lst1[ parent.panelDV.selected ].id, parent.ret_val), 1)
          elif type == 'equal':
             var = MV2.equal( parent.panelDV.lst1[ parent.panelDV.selected ],
                             parent.ret_val )
             ret_val = string.replace( ('%s'%parent.ret_val), '.', '_')
             var.id = var.name = '_%s_%s_%s'%(type,parent.panelDV.lst1[ parent.panelDV.selected ].id,ret_val)
             gui_control.record_command( parent, "%s = MV2.equal( %s, %s )" % (var.id, parent.panelDV.lst1[ parent.panelDV.selected ].id, parent.ret_val), 1)

      elif (type in [ 'add', 'subtract', 'multiply', 'divide' ]) and (lst_ct > 1):
          gui_control.record_command(parent, "\n# Record the %s" % type , 1 )
          var_ct = 0
          var_name = ''
          cmd = ''
          for x in parent.panelDV.selected_list.keys():
             if var_ct == 0:
                var = parent.panelDV.lst1[ parent.panelDV.selected_list[ x ] ]
                var_name = '_%s_%s' % (type,var.id)
                #gui_control.record_command( parent, "%s = %s" % (var_name, parent.panelDV.lst1[ parent.panelDV.selected_list[ x ] ].id), 1)
                cmd = var.id
             else:
                #var_name_hold = var_name
                if type == 'add':
                   var += parent.panelDV.lst1[ parent.panelDV.selected_list[ x ] ]
                   var_name += ( '_%s'% parent.panelDV.lst1[ parent.panelDV.selected_list[ x ] ].id )
                   var.id = var.name = var_name
                   cmd +=  ' + %s' % parent.panelDV.lst1[ parent.panelDV.selected_list[ x ] ].id
                elif type == 'subtract':
                   var -= parent.panelDV.lst1[ parent.panelDV.selected_list[ x ] ]
                   var_name += ( '_%s'% parent.panelDV.lst1[ parent.panelDV.selected_list[ x ] ].id )
                   var.id = var.name = var_name
                   cmd +=  ' - %s' % parent.panelDV.lst1[ parent.panelDV.selected_list[ x ] ].id
                elif type == 'multiply':
                   var *= parent.panelDV.lst1[ parent.panelDV.selected_list[ x ] ]
                   var_name += ( '_%s'% parent.panelDV.lst1[ parent.panelDV.selected_list[ x ] ].id )
                   var.id = var.name = var_name
                   cmd +=  ' * %s' % parent.panelDV.lst1[ parent.panelDV.selected_list[ x ] ].id
                elif type == 'divide':
                   var /= parent.panelDV.lst1[ parent.panelDV.selected_list[ x ] ]
                   var_name += ( '_%s'% parent.panelDV.lst1[ parent.panelDV.selected_list[ x ] ].id )
                   var.id = var.name = var_name
                   cmd +=  ' / %s' % parent.panelDV.lst1[ parent.panelDV.selected_list[ x ] ].id
             var_ct += 1
             if var_ct == len(parent.panelDV.selected_list.keys()):
                cmd = var.id + ' = ' + cmd
                gui_control.record_command( parent, cmd, 1)

      elif ( (type in ['regrid','power','less','greater','equal','mask']) and (lst_ct > 1) ):
          gui_control.record_command(parent, "\n# Record the %s" % type , 1 )
          lst = parent.panelDV.selected_list.keys()
          if type == 'regrid':
             var = parent.panelDV.lst1[parent.panelDV.selected_list[lst[-2]]].regrid( parent.panelDV.lst1[ parent.panelDV.selected ].getGrid())
             var.id = var.name = '_%s_%s_%s'%('regrid',parent.panelDV.lst1[parent.panelDV.selected_list[lst[-2]]].id,parent.panelDV.lst1[ parent.panelDV.selected ].id)
             gui_control.record_command( parent, "%s = %s.regrid( %s.getGrid() )" % (var.id, parent.panelDV.lst1[ parent.panelDV.selected_list[ lst[ -2 ] ] ].id, parent.panelDV.lst1[ parent.panelDV.selected ].id), 1)
          elif type == 'power':
             var = MV2.power(parent.panelDV.lst1[parent.panelDV.selected_list[lst[-2]]], parent.panelDV.lst1[ parent.panelDV.selected ] )
             var.id = var.name = '_%s_%s_%s'%('power',parent.panelDV.lst1[parent.panelDV.selected_list[lst[-2]]].id,parent.panelDV.lst1[ parent.panelDV.selected ].id)
             gui_control.record_command( parent, "%s = MV2.power( %s, %s )" % (var.id, parent.panelDV.lst1[parent.panelDV.selected_list[lst[-2]]].id, parent.panelDV.lst1[ parent.panelDV.selected ].id), 1)
          elif type == 'less':
             var = MV2.less(parent.panelDV.lst1[parent.panelDV.selected_list[lst[-2]]], parent.panelDV.lst1[ parent.panelDV.selected ] )
             var.id = var.name = '_%s_%s_%s'%('less',parent.panelDV.lst1[parent.panelDV.selected_list[lst[-2]]].id,parent.panelDV.lst1[ parent.panelDV.selected ].id)
             gui_control.record_command( parent, "%s = MV2.less( %s, %s )" % (var.id, parent.panelDV.lst1[parent.panelDV.selected_list[lst[-2]]].id, parent.panelDV.lst1[ parent.panelDV.selected ].id), 1)
          elif type == 'greater':
             var = MV2.greater(parent.panelDV.lst1[parent.panelDV.selected_list[lst[-2]]], parent.panelDV.lst1[ parent.panelDV.selected ] )
             var.id = var.name = '_%s_%s_%s'%('greater',parent.panelDV.lst1[parent.panelDV.selected_list[lst[-2]]].id,parent.panelDV.lst1[ parent.panelDV.selected ].id)
             gui_control.record_command( parent, "%s = MV2.greater( %s, %s )" % (var.id, parent.panelDV.lst1[parent.panelDV.selected_list[lst[-2]]].id, parent.panelDV.lst1[ parent.panelDV.selected ].id), 1)
          elif type == 'equal':
             var = MV2.equal(parent.panelDV.lst1[parent.panelDV.selected_list[lst[-2]]], parent.panelDV.lst1[ parent.panelDV.selected ] )
             var.id = var.name = '_%s_%s_%s'%('equal',parent.panelDV.lst1[parent.panelDV.selected_list[lst[-2]]].id,parent.panelDV.lst1[ parent.panelDV.selected ].id)
             gui_control.record_command( parent, "%s = MV2.equal( %s, %s )" % (var.id, parent.panelDV.lst1[parent.panelDV.selected_list[lst[-2]]].id, parent.panelDV.lst1[ parent.panelDV.selected ].id), 1)
          elif type == 'mask':
             var = MV2.masked_where(parent.panelDV.lst1[parent.panelDV.selected_list[lst[-2]]], parent.panelDV.lst1[ parent.panelDV.selected ] )
             var.id = var.name = '_%s_%s_%s'%('masked_where',parent.panelDV.lst1[parent.panelDV.selected_list[lst[-2]]].id,parent.panelDV.lst1[ parent.panelDV.selected ].id)
             gui_control.record_command( parent, "%s = MV2.masked_where( %s, %s )" % (var.id, parent.panelDV.lst1[parent.panelDV.selected_list[lst[-2]]].id, parent.panelDV.lst1[ parent.panelDV.selected ].id), 1)
      elif ( (type in ['grower']) and (lst_ct > 1) ):
          gui_control.record_command(parent, "\n# Record the %s" % type , 1 )
          lst = parent.panelDV.selected_list.keys()
          if type == 'grower':
             var1,var2 = genutil.grower(parent.panelDV.lst1[parent.panelDV.selected_list[lst[-2]]], parent.panelDV.lst1[ parent.panelDV.selected ] )
             var1.id=var1.name=parent.panelDV.lst1[parent.panelDV.selected_list[lst[-2]]].id
             var2.id=var2.name=parent.panelDV.lst1[parent.panelDV.selected_list[lst[-1]]].id
             gui_control.record_command( parent, "%s, %s = genutil.grower( %s, %s )" % (parent.panelDV.lst1[parent.panelDV.selected_list[lst[-2]]].id, parent.panelDV.lst1[ parent.panelDV.selected ].id, parent.panelDV.lst1[parent.panelDV.selected_list[lst[-2]]].id, parent.panelDV.lst1[ parent.panelDV.selected ].id), 1)
      elif ( (type in [ 'sqrt', 'exp', 'base10', 'log',
                        'log10', 'inverse', 'fabs','not','std','getmask' ]) ):
          gui_control.record_command(parent, "\n# Record the %s" % type , 1 )
          if type == 'not':
             var = MV2.logical_not( parent.panelDV.lst1[ parent.panelDV.selected ] )
             var.id = var.name = '_not_%s'%parent.panelDV.lst1[ parent.panelDV.selected ].id
             gui_control.record_command( parent, "%s = MV2.logical_not( %s )" % (var.id, parent.panelDV.lst1[ parent.panelDV.selected ].id), 1)
          elif type == 'std':
             var = genutil.statistics.std( parent.panelDV.lst1[ parent.panelDV.selected ] )
             var.id = var.name = '_std_%s'%parent.panelDV.lst1[ parent.panelDV.selected ].id
             gui_control.record_command( parent, "%s = genutil.statistics.std( %s )" % (var.id, parent.panelDV.lst1[ parent.panelDV.selected ].id), 1)
          elif type == 'getmask':
             m=MV2.getmask( parent.panelDV.lst1[ parent.panelDV.selected ] )
             if m is None:
                var=MV2.zeros(parent.panelDV.lst1[ parent.panelDV.selected ].shape,axes=parent.panelDV.lst1[ parent.panelDV.selected ].getAxisList())
                var.id = var.name = '_mask_%s'%parent.panelDV.lst1[ parent.panelDV.selected ].id
                gui_control.record_command( parent, "%s = MV2.zeros( %s.shape,axes=%s.getAxisList() )" % (var.id, parent.panelDV.lst1[ parent.panelDV.selected ].id,parent.panelDV.lst1[ parent.panelDV.selected ].id), 1)
             else:
                var = MV2.array(m,
                               typecode=MV2.int,
                               axes=parent.panelDV.lst1[ parent.panelDV.selected ].getAxisList())
                var.id = var.name = '_mask_%s'%parent.panelDV.lst1[ parent.panelDV.selected ].id
                gui_control.record_command( parent, "%s = MV2.array( MV2.getmask( %s ), typecode=MV2.int, axes=%s.getAxisList() )" % (var.id, parent.panelDV.lst1[ parent.panelDV.selected ].id, parent.panelDV.lst1[ parent.panelDV.selected ].id), 1)
          elif type == 'sqrt':
             var = MV2.sqrt( parent.panelDV.lst1[ parent.panelDV.selected ] )
             var.id = var.name = '_sqrt_%s'%parent.panelDV.lst1[ parent.panelDV.selected ].id
             gui_control.record_command( parent, "%s = MV2.sqrt( %s )" % (var.id, parent.panelDV.lst1[ parent.panelDV.selected ].id), 1)
          elif type == 'exp':
             var = MV2.exp( parent.panelDV.lst1[ parent.panelDV.selected ] )
             var.id = var.name = '_exp_%s'%parent.panelDV.lst1[ parent.panelDV.selected ].id
             gui_control.record_command( parent, "%s = MV2.exp( %s )" % (var.id, parent.panelDV.lst1[ parent.panelDV.selected ].id), 1)
          elif type == 'base10':
             var = MV2.power( 10, parent.panelDV.lst1[ parent.panelDV.selected ] )
             var.id = var.name = '_base10_%s'%parent.panelDV.lst1[ parent.panelDV.selected ].id
             gui_control.record_command( parent, "%s = MV2.power( 10, %s )" % (var.id, parent.panelDV.lst1[ parent.panelDV.selected ].id), 1)
          elif type == 'log':
             var = MV2.log( parent.panelDV.lst1[ parent.panelDV.selected ] )
             var.id = var.name = '_log_%s'%parent.panelDV.lst1[ parent.panelDV.selected ].id
             gui_control.record_command( parent, "%s = MV2.log( %s )" % (var.id, parent.panelDV.lst1[ parent.panelDV.selected ].id), 1)
          elif type == 'log10':
             var = MV2.log10( parent.panelDV.lst1[ parent.panelDV.selected ] )
             var.id = var.name = '_log10_%s'%parent.panelDV.lst1[ parent.panelDV.selected ].id
             gui_control.record_command( parent, "%s = MV2.log10( %s )" % (var.id, parent.panelDV.lst1[ parent.panelDV.selected ].id), 1)
          elif type == 'inverse':
             var = MV2.power( parent.panelDV.lst1[ parent.panelDV.selected ], -1.0 )
             var.id = var.name = '_inverse_%s'%parent.panelDV.lst1[ parent.panelDV.selected ].id
             gui_control.record_command( parent, "%s = MV2.power( %s, -1.0 )" % (var.id, parent.panelDV.lst1[ parent.panelDV.selected ].id), 1)
          elif type == 'fabs':
             var = MV2.fabs( parent.panelDV.lst1[ parent.panelDV.selected ] )
             var.id = var.name = '_fabs_%s'%parent.panelDV.lst1[ parent.panelDV.selected ].id
             gui_control.record_command( parent, "%s = MV2.fabs( %s )" % (var.id, parent.panelDV.lst1[ parent.panelDV.selected ].id), 1)
      elif ( type in [ 'sin', 'sinh', 'cos', 'cosh', 'tan', 'tanh' ] ):
          gui_control.record_command(parent, "\n# Record the %s" % type , 1 )
          if type == 'sin':
             var = MV2.sin( parent.panelDV.lst1[ parent.panelDV.selected ] )
             var.id = var.name = '_sin_%s'%parent.panelDV.lst1[ parent.panelDV.selected ].id
             gui_control.record_command( parent, "%s = MV2.sin( %s )" % (var.id, parent.panelDV.lst1[ parent.panelDV.selected ].id), 1)
          elif type == 'sinh':
             var = MV2.sinh( parent.panelDV.lst1[ parent.panelDV.selected ] )
             var.id = var.name = '_sinh_%s'%parent.panelDV.lst1[ parent.panelDV.selected ].id
             gui_control.record_command( parent, "%s = MV2.sinh( %s )" % (var.id, parent.panelDV.lst1[ parent.panelDV.selected ].id), 1)
          elif type == 'cos':
             var = MV2.cos( parent.panelDV.lst1[ parent.panelDV.selected ] )
             var.id = var.name = '_cos_%s'%parent.panelDV.lst1[ parent.panelDV.selected ].id
             gui_control.record_command( parent, "%s = MV2.cos( %s )" % (var.id, parent.panelDV.lst1[ parent.panelDV.selected ].id), 1)
          elif type == 'cosh':
             var = MV2.cosh( parent.panelDV.lst1[ parent.panelDV.selected ] )
             var.id = var.name = '_cosh_%s'%parent.panelDV.lst1[ parent.panelDV.selected ].id
             gui_control.record_command( parent, "%s = MV2.cosh( %s )" % (var.id, parent.panelDV.lst1[ parent.panelDV.selected ].id), 1)
          elif type == 'tan':
             var = MV2.tan( parent.panelDV.lst1[ parent.panelDV.selected ] )
             var.id = var.name = '_tan_%s'%parent.panelDV.lst1[ parent.panelDV.selected ].id
             gui_control.record_command( parent, "%s = MV2.tan( %s )" % (var.id, parent.panelDV.lst1[ parent.panelDV.selected ].id), 1)
          elif type == 'tanh':
             var = MV2.tanh( parent.panelDV.lst1[ parent.panelDV.selected ] )
             var.id = var.name = '_tanh_%s'%parent.panelDV.lst1[ parent.panelDV.selected ].id
             gui_control.record_command( parent, "%s = MV2.tanh( %s )" % (var.id, parent.panelDV.lst1[ parent.panelDV.selected ].id), 1)

      if not type in ['grower']:
         gui_user_menus.user_menus_put( var.id, var )
      else:
         gui_user_menus.user_menus_put( var1.id, var1 )
         gui_user_menus.user_menus_put( var2.id, var2 )

#
####### Mode 2 operates on the selected 'Data' in the scroll window,
####### but the user must display the "Expression" widget in order to
####### do calculations.
#
   def mode_2_operation( self, parent, type, event ):
      self.doing_calculation_type = type

      a=['+','-','*','/']
      found = 0
      for x in a:
         if string.find(self.saved_expression, x) > -1:
             found = 1; break
      if found == 1: self.saved_expression = '(' + self.saved_expression + ')'

      if type == 'add':
         self.saved_expression += '+'
      elif type == 'subtract':
         self.saved_expression += '-'
      elif type == 'multiply':
         self.saved_expression += '*'
      elif type == 'divide':
         self.saved_expression += '/'
      elif type == 'regrid':
         self.saved_expression = self.saved_expression + '.regrid('
      elif type == 'grower':
         self.saved_expression = 'genutil.grower(' + self.saved_expression + ','
      elif type == 'mask':
         self.saved_expression = 'MV2.masked_where(' + self.saved_expression + ','
      elif type == 'not':
         self.saved_expression = 'MV2.logical_not(' + self.saved_expression + ','
      elif type in [ 'exp', 'log', 'log10', 'fabs', 'sin', 'sinh', 'cos', 'cosh', 'tan', 'tanh','less','greater','power','equal',]:
         self.saved_expression = 'MV2.' + type + '(' + self.saved_expression + ')'
         gui_functions._blank_dim1_to_ndim( parent )
         self.scl1.select_clear( 0, 'end' )
         parent.panelDV.selected = []
         parent.panelDV.selected_list = {}
      elif type == 'base10':
         self.saved_expression = 'MV2.power(10,' + self.saved_expression + ')'
         gui_functions._blank_dim1_to_ndim( parent )
         self.scl1.select_clear( 0, 'end' )
         parent.panelDV.selected = []
         parent.panelDV.selected_list = {}
      elif type == 'std':
         self.saved_expression = 'genutil.statistics.std(' + self.saved_expression + ')'
         gui_functions._blank_dim1_to_ndim( parent )
         self.scl1.select_clear( 0, 'end' )
         parent.panelDV.selected = []
         parent.panelDV.selected_list = {}
      elif type == 'getmask':
         self.saved_expression = 'MV2.array(MV2.getmask(' + self.saved_expression + '),typecode=MV2.int)'
         gui_functions._blank_dim1_to_ndim( parent )
         self.scl1.select_clear( 0, 'end' )
         parent.panelDV.selected = []
         parent.panelDV.selected_list = {}
      elif type == 'not':
         self.saved_expression = 'MV2.logical_not(' + self.saved_expression + ')'
         gui_functions._blank_dim1_to_ndim( parent )
         self.scl1.select_clear( 0, 'end' )
         parent.panelDV.selected = []
         parent.panelDV.selected_list = {}
      elif type == 'inverse':
         self.saved_expression = 'MV2.power(' + self.saved_expression + ',-1.0)'
         gui_functions._blank_dim1_to_ndim( parent )
         self.scl1.select_clear( 0, 'end' )
         parent.panelDV.selected = []
         parent.panelDV.selected_list = {}

      try:
         self.gui_expression.expression.setentry( self.saved_expression )
      except:
         pass
#---------------------------------------------------------------------
# End Defined Variables Panel Layout
#---------------------------------------------------------------------

#-------------------------------------------
# event functions
#-------------------------------------------
#
####### event of selection in 'Data' or 'Selected' scroll window
#
   def evt_selected_defined_variable( self, parent, event ):
      # Select the variable in MULTIPLE mode only. The logic seems as though you can 
      # remove this function and call evt_selected_defined_variable3 directly, but don't
      # be fooled. This setup helps to handle both MULTIPLE and EXTENDED modes simultaneously.
      self.evt_selected_defined_variable3( parent )

   def evt_selected_defined_variable2( self, parent, event ):
      if (parent.menu.main_menu.DV_single_selection_mode.get() == 0): "break"
      else: self.evt_selected_defined_variable3( parent, control_key = 1 )

   def evt_selected_defined_variable3( self, parent, control_key = 0 ):
      Nselected = list( parent.panelDV.scl1.getcurselection() )
      Oselected = parent.panelDV.selected_list.values()

      # Remove beginning numbers (e.g., '00 ') from the new selected list. See the 
      # gui_control.py file for the setting.
      for x in range(len(Nselected)): Nselected[x] = Nselected[x][gui_control.dvsize:]

      if (len(Nselected) == 0) and (len(Oselected) == 0): return
      #
      if (parent.panelDM.var3 is not None):
         parent.panelDM.remember_var3 = parent.panelDM.var3
      parent.panelDM.var3 = None
      #
      k = []
      k2 = []
      for x in Nselected:
          if x not in Oselected:
             k = x
             break
      if len( k ) == 0:
         for x in Oselected:
             if x not in Nselected:
                k2 = x
                break
      parent.panelVI.scl1.settext( '' )
      gui_functions._blank_dim1_to_ndim( parent )
      if len( k ) == 0 and len( k2 ) != 0:
         for i in parent.panelDV.selected_list.keys(): # remove k2 from dictionary
            if parent.panelDV.selected_list[i] == k2:
               single_mode_select = k2
               del parent.panelDV.selected_list[i]
               break
         if len( parent.panelDV.selected_list ) != 0:
            l=parent.panelDV.selected_list.keys()
            l.sort()
            parent.panelDV.selected = parent.panelDV.selected_list[ l[-1] ]
            single_mode_select = parent.panelDV.selected
         else:
            parent.panelDM.remember_var3 = parent.panelDM.var3 = None
            # Reset selection counters back to '-- ', See the gui_control.py 
            # file for the setting.
            for x in parent.panelDV.number_lst1.keys():
               parent.panelDV.number_lst1[ x ] = gui_control.dvholder + x
            update_defined_variable_list( parent )
            return
      else:
         parent.panelDV.selected = k
         if len(parent.panelDV.selected_list) != 0:
            l = parent.panelDV.selected_list.keys()
            l.sort()
         else:
            l=[-1]
         parent.panelDV.selected_list[ l[-1]+1 ] = single_mode_select = k
      #
      try:
         slab_list = parent.panelDV.lst1[ parent.panelDV.selected ].listall()
      except:
         single_mode_select = parent.panelDV.selected = Nselected[0]
         slab_list = parent.panelDV.lst1[ parent.panelDV.selected ].listall()
      parent.panelVI.scl1.configure( text_state = 'normal' )
      first_time = 1
      for x in slab_list:
         if string.find( x, " Dimension " ) != -1:
            if first_time:
               ln=len(parent.panelDV.lst1[ parent.panelDV.selected ].shape)
               num_dim = 'Number of dimensions: %d' % ln
               parent.panelVI.scl1.insert( 'end',( num_dim + '\n') )
               first_time = 0
            parent.panelVI.scl1.insert( 'end', '\n' )
         parent.panelVI.scl1.insert( 'end', (x + '\n') )
      parent.panelVI.scl1.configure( text_state = 'disabled' )
      #
      parent.panelSV.tin4._entryfield.clear()
      #

      # If single section mode is on, then reset selected and selected list
      if (control_key == 0) and (parent.menu.main_menu.DV_single_selection_mode.get() == 1):
         parent.panelDV.selected = single_mode_select
         parent.panelDV.selected_list = {}
         parent.panelDV.selected_list[0] = parent.panelDV.selected

      # Update the selection numbering scheme
      ct = 0
      for x in parent.panelDV.number_lst1.keys():
            parent.panelDV.number_lst1[ x ] = gui_control.dvholder + x
      for x in parent.panelDV.selected_list.values():
         ct += 1
         if ct < 10:
            parent.panelDV.number_lst1[ x ] = ("%s%d " % (gui_control.dvholder[0], ct)) + x
         elif ct < 100:
            parent.panelDV.number_lst1[ x ] = ("%d " % ct) + x
         else:
            gui_message.error('Cannot exceed over 100 selections. See gui_control.py for modification instructions.')
          
      hold_selected_list =  parent.panelDV.selected_list
      hold_selected = parent.panelDV.selected
      update_defined_variable_list( parent )

      # Adjust the view in teh scroll window's listbox so that the elememt selected is visable.
      got = parent.panelDV.scl1.get( 0, 'end' )
      lh = []
      for gh in got: lh.append(gh[3:])
      DVindex = 0
      if (k != []):
         DVindex = lh.index(k)
      elif (k2 != []):
         DVindex = lh.index(k2)
      parent.panelDV.scl1.see(DVindex)

      dvlt = parent.panelDV.scl1.get()
      for i in range( len( dvlt ) ):
          if dvlt[ i ][0:gui_control.dvsize] != gui_control.dvholder:
             parent.panelDV.scl1.select_set( i )
      parent.panelDV.selected_list = hold_selected_list
      parent.panelDV.selected = hold_selected

      # Set the data selection for the Page Layout Option
      self.data_bin_name = hold_selected

      # Show the dimension information in the dimenesion panel
      gui_functions._srl_var_dim1_to_ndim( parent, slab=parent.panelDV.lst1[ parent.panelDV.selected ] )
      for k in range(parent.panelDM.ndim):
         parent.panelDM.dim[k].comb.component('entry').configure(background = gui_color.six )
         parent.panelDM.dim[k].first_scl.configure( troughcolor=gui_color.six )
         parent.panelDM.dim[k].last_scl.configure( troughcolor=gui_color.six )
      #
      # This is one of two places where the "Retain User Defined Settings"
      # is called. THIS IS NO LONGER IN USE!!!
      #if parent.menu.retain_user_settings_flg == 0:
      #   gui_reset.to_initial_state(parent, 0)

      #
      # This section of code is for the calculation handler. I must get the
      # last variable selected for calculation.
      #
      if self.doing_calculation_type in [ 'add', 'subtract', 'multiply', 'divide','less','greater','equal','not','mask' ]:
         self.saved_expression += parent.panelDV.lst1[ parent.panelDV.selected ].id
         self.doing_calculation_type = ''
         self.build_expression = 'yes'
         gui_functions._blank_dim1_to_ndim( parent )
         self.scl1.select_clear( 0, 'end' )
         parent.panelDV.selected = []
         parent.panelDV.selected_list = {}
      elif self.doing_calculation_type == 'regrid':
         self.saved_expression += parent.panelDV.lst1[ parent.panelDV.selected ].id + '.getGrid())'
         self.doing_calculation_type = ''
         self.build_expression = 'yes'
         gui_functions._blank_dim1_to_ndim( parent )
         self.scl1.select_clear( 0, 'end' )
         parent.panelDV.selected = []
         parent.panelDV.selected_list = {}
      elif self.doing_calculation_type in ['power','grower']:
         self.saved_expression += parent.panelDV.lst1[ parent.panelDV.selected ].id + ')'
         self.doing_calculation_type = ''
         self.build_expression = 'yes'
         gui_functions._blank_dim1_to_ndim( parent )
         self.scl1.select_clear( 0, 'end' )
         parent.panelDV.selected = []
         parent.panelDV.selected_list = {}
      elif self.doing_calculation_type in ['exp','log','base10','log10','inverse','fabs']:
         self.doing_calculation_type = ''
         self.build_expression = 'yes'
      elif self.build_expression == 'yes':
         pass
      else:
         self.saved_expression = parent.panelDV.lst1[ parent.panelDV.selected ].id

      try:
         self.gui_expression.expression.setentry( self.saved_expression )
      except:
         pass

####### event of button above Data scrollbar

#
######## event of selection in 'CDAT Command' scrollbar

#
#-----------------------------------------------------------------------------------
# In the selected panel the user will need to get all the changes coming from the
# [ idle ] Shell window. This function will update the selected window with the
# new or removed TransientVariable (i.e., MV2), MA, or numpy arrays.
#-----------------------------------------------------------------------------------
def selected_updated( args = () ):
   aa = __main__.__dict__

   parent = aa['tk_root']
   if args == ():
      mkeys = aa.keys()
      parent.panelDV.lst1 = {}
      parent.panelDV.number_lst1 = {}
   else:
      mkeys = args
   try:
      for x in mkeys:
         y = aa[ x ]

         if ( ( isinstance( y , numpy.ndarray) ) or
              ( cdms2.isVariable( y ) ) or
              ( MV2.isMA( y ) ) ):
            # Convert numpy or MA arrays to MV2
            if parent.menu.convert_to_MV_flg == 1: aa[x] = cdms2.asVariable( y )
   
            str_var = x + ' ('
            if len(aa[x].shape) > 0:
               for i in range(len(aa[x].shape)):
                  if parent.menu.fortran_order_flg:
                     s ="%d" % aa[x].shape[len(aa[x].shape)-i-1]
                  else:
                     s ="%d" % aa[x].shape[i]
                  s ="%d" % aa[x].shape[i]
                  str_var = str_var + s + ', '
               str_var = str_var[0:len(str_var)-2]

            str_var = str_var + ')'
            if (numpy.ma.rank(aa[x])==0 ) and (isinstance(aa[x],numpy.ma.MaskedArray)) and aa[x].mask==1:
             pass
            else:
             try:
               if str_var[-2:] == '()': str_var = str_var + (' = %.17g' % aa[ x ])
               if ( not isinstance( aa[ x ] , numpy.ndarray) ): aa[x].id = aa[x].name = x
               if cdms2.isVariable( aa[ x ] ): parent.panelDV.lst1[ str_var ] = aa[ x ]
             except:
               pass
   except:
#      gui_message.error("Must pass string parameter arguments (i.e., 'ps', 'tas').")
      mkeys = aa.keys()
      parent.panelDV.lst1 = {}
      parent.panelDV.number_lst1 = {}
      for x in mkeys:
         y = aa[ x ]
        
         if ( ( isinstance( y , numpy.ndarray) ) or
              ( cdms2.isVariable( y ) ) or
              ( MV2.isMA( y ) ) ):
            # Convert numpy or MA arrays to MV2
            if parent.menu.convert_to_MV_flg == 1: aa[x] = cdms2.asVariable( y )
  
            str_var = x + ' ('
            if len(aa[x].shape) > 0:
               for i in range(len(aa[x].shape)):
                  if parent.menu.fortran_order_flg:
                     s ="%d" % aa[x].shape[len(aa[x].shape)-i-1]
                  else:
                     s ="%d" % aa[x].shape[i]
                  s ="%d" % aa[x].shape[i]
                  str_var = str_var + s + ', '
               str_var = str_var[0:len(str_var)-2]

            str_var = str_var + ')'
	    if (numpy.ma.rank(aa[x])==0 ) and (isinstance(aa[x],numpy.ma.MaskedArray)) and aa[x].mask==1:
             pass
            else:
             try:
               if str_var[-2:] == '()': str_var = str_var + (' = %.17g' % aa[ x ])
               if ( not isinstance( aa[ x ] , numpy.ndarray) ): aa[x].id = aa[x].name = x
               if cdms2.isVariable( aa[ x ] ): parent.panelDV.lst1[ str_var ] = aa[ x ]
             except:
               pass

   d = parent.panelDV.lst1.keys()
   d.sort()
   # Reset selection counters back to initial state, (e.g., '00 ')
   for x in d: parent.panelDV.number_lst1[ x ] = gui_control.dvholder + x

   #
   if parent.panelDV.selected_list != {}:
      gui_functions._blank_dim1_to_ndim( parent )
   #
   update_defined_variable_list( parent )

def update_defined_variable_list( parent ):
   var_list = []
   for x in parent.panelDV.lst1.keys():
      a = string.split( x, ' ')[0]
      if a[-len(trash_str):] != trash_str:
         var_list.append( x )
   #
   var_list.sort()

   # Instead of just displaying the variables by themselves, display the selected 
   # number sequence and the variable
   show_dv_list = []
   for x in var_list: show_dv_list.append( parent.panelDV.number_lst1[ x ] )

   parent.panelDV.scl1.setlist( show_dv_list )
   parent.panelDV.selected = []
   parent.panelDV.selected_list = {}

   try:
      parent.pegui.bins.data_listbox.setlist( var_list )
   except:
      pass

   # Check to make sure the Page Layout Form's data exist. If the data doesn't exist, then
   # remove from Page Layout Form.
   var_only = []
   for i in var_list:
       var_only.append( string.split(i, ' ')[0] )
   var_only.append( '' )
   for i in parent.pl.form.keys():
      if (parent.pl.form[i].needed_data >= 1):
         if (parent.pl.form[i].data1.get() not in var_only):
            parent.pl.evt_remove_form( i, parent, None )
            break
      if (parent.pl.form[i].needed_data == 2):
         if (parent.pl.form[i].data2.get() not in var_only):
            parent.pl.evt_remove_form( i, parent, None )
            break


class update_defined:
   def __init__( parent, *args ):
      aa = __main__.__dict__
      sargs = []
      for x in args:
         if cdms2.isVariable( x ):
            for i in range( len(aa) ):
               if ( (cdms2.isVariable( aa.values()[i] )) and (aa.values()[i].id == x.id) ):
                  sargs.append( aa.keys()[i] )
         else:
            sargs.append( x )

      selected_updated( tuple(sargs) )

# Return a list of all selected variables in the order in
#  which they were selected 
def get_vars():
    parent = __main__.__dict__['tk_root']
    variables = parent.panelDV.selected_list.keys()
    data = []
    for i in variables:
        data.append(parent.panelDV.lst1[parent.panelDV.selected_list[i]])
    return data
#
#---------------------------------------------------------------------
# End of File
#---------------------------------------------------------------------
