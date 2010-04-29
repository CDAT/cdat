#!/usr/bin/env python
# Adapted for numpy/ma/cdms2 by convertcdms.py
#
# The PCMDI Data Browser Select Variable Panel -  gui_select_variable module
#
###############################################################################
#                                                                             #
# Module:       gui_select_variable module                                    #
#                                                                             #
# Copyright:    "See file Legal.htm for copyright information."               #
#                                                                             #
# Authors:      PCMDI Software Team                                           #
#               Lawrence Livermore NationalLaboratory:                        #
#               support@pcmdi.llnl.gov                                        #
#                                                                             #
# Description:  PCMDI Software System browser Tkinter "Select Variable" panel #
#               GUI.              					      #
#                                                                             #
# Version:      4.0                                                           #
#                                                                             #
###############################################################################
#
#---------------------------------------------------------------------
# NOTE: need to use version of Python that imports Tkinter and Pmw
#---------------------------------------------------------------------
import Tkinter, Pmw, tkFileDialog
import sys, os, re, types, string, cdms2
import gui_functions
from gui_support import gui_color
import gui_select_variable
import gui_control
import gui_formulate
import gui_database
import gui_edit_list
import gui_message
import gui_defined_variables
import cdat_info
import gui_opendap

#---------------------------------------------------------------------------
#
# Start of the "Select Variable" panel GUI Layout
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
# Begin the creation of "Select Variable" panel
#---------------------------------------------------------------------
class create:
   def __init__( self, parent ):

      #-------------------------------------------
      # create 1st sub-panel
      #-------------------------------------------
      FRAME = Tkinter.Frame( parent.pane.pane( 'panelSV' ) )
      FRAME.pack( side='top', pady=3, fill='x', expand=1 )

      #-------------------------------------------
      # line 1
      #-------------------------------------------
      framea = Tkinter.Frame( FRAME )
      self.lab1 = Tkinter.Label( framea, text="Select Variable" )
      self.lab1.pack( )
      framea.pack( side='top', fill='x', expand=1 )

      d,f = gui_functions._scn_a_dir( parent )

      #-------------------------------------------
      # line 2
      #-------------------------------------------
      frameb = Tkinter.Frame( FRAME )

      # Set up database or directory combo box
      if gui_control.have_cdms_database:
         self.textv_opt9 = Tkinter.StringVar()
         self.textv_opt9.set('Directory')
         self.opt9 = Pmw.OptionMenu( frameb,
                             menubutton_width=7,
                             menubutton_textvariable = self.textv_opt9,
                             items=gui_control.dbdchlst,
                             command=gui_control.Command(evt_popup_database_gui, self, parent) )
         self.opt9.configure( menubutton_text=gui_control.dbdchlst[0] )
      else:
         self.opt9 = Tkinter.Label( frameb, text="Directory:" )
      self.opt9.pack( side='top' )
      parent.balloon.bind( self.opt9, 'Select Variables from a Directory or a Database' )

      # Set up go home icon
      self.canvas_homeicon = Tkinter.Canvas(frameb, bd=0, highlightthickness=0,
                                        width = 27, height = 27)
      self.canvas_homeicon.pack( side='left', fill=Tkinter.BOTH, expand='no', padx=5 )
      parent.balloon.bind( self.canvas_homeicon,
                 "Go to user's home directory." )
      self.img4 = Tkinter.PhotoImage( file = os.path.join(cdms2.__path__[0],'..','..','..','..', 'bin', 'gohome.gif') )
      self.canvas_homeicon.create_image(0,0, anchor=Tkinter.NW, image=self.img4 )
      self.canvas_homeicon.bind( '<1>', gui_control.Command( evt_icon_open_file, parent, os.environ['HOME'] ))

      # Set up file open icon
      self.canvas_openicon = Tkinter.Canvas(frameb, bd=0, highlightthickness=0,
                                        width = 27, height = 27)
      self.canvas_openicon.pack( side='left', fill=Tkinter.BOTH, expand='no', padx=5 )
      parent.balloon.bind( self.canvas_openicon, 
                 "Display 'File Select' browser for 'Directory' and 'File' selection." )
      self.img = Tkinter.PhotoImage( file = os.path.join(cdms2.__path__[0],'..','..','..','..', 'bin', 'open.gif') )
      self.canvas_openicon.create_image(0,0, anchor=Tkinter.NW, image=self.img )
      self.canvas_openicon.bind( '<1>', gui_control.Command( evt_icon_open_file, parent, None ))

      # Set up opendap icon, but only if built with opendap
      import cdat_info
      if cdat_info.CDMS_INCLUDE_DAP == 'yes':
         self.canvas_opendapicon = Tkinter.Canvas(frameb, bd=0, highlightthickness=0,
                                                  width = 27, height = 27)
         self.canvas_opendapicon.pack( side='left', fill=Tkinter.BOTH, expand='no', padx=5 )
         parent.balloon.bind( self.canvas_opendapicon,
                              "Display 'Portal Internet Manager' for data browsing and searching." )
         self.img5 = Tkinter.PhotoImage( file = os.path.join(cdms2.__path__[0],'..','..','..','..', 'bin', 'opendap.gif') )
         self.canvas_opendapicon.create_image(0,0, anchor=Tkinter.NW, image=self.img5 )
         self.canvas_opendapicon.bind( '<1>', gui_control.Command( evt_icon_opendap_file, parent, None ))

         
      self.tin2 = Pmw.ComboBox( frameb,
                                       scrolledlist_items = d,
		                       entryfield_value = os.getcwd(),
                                       entry_width=10,
                                       entry_background='white', entry_foreground='black',
                                       selectioncommand=gui_control.Command(evt_enter_directory, parent)
                                     )
      parent.balloon.bind( self.tin2._entryfield, "Enter the directory of the variable file(s). Can\nuse the arrow button to the right or the 'Open File'\nmenu item located under the 'File' menu option to\nalso set the directory.\n\nNote: The tab key can be used to complete subdirectory\nnames.")
      parent.balloon.bind( self.tin2._arrowBtn, 'View and Select Directories' )
      self.opt9.pack( side='left', padx=0 )
      self.tin2.pack( side='left', fill='x', expand=1,
                        padx=5, ipady=2 )

      # Set up bookmarks (i.e., favorite directories icon)
      self.canvas_bookicon = Tkinter.Canvas(frameb, bd=0, highlightthickness=0,
                                        width = 27, height = 27)
      self.canvas_bookicon.pack( side='left', fill='x', padx=5 )
      parent.balloon.bind( self.canvas_bookicon,
                 "Bookmark your favorite working directories." )
      self.img2 = Tkinter.PhotoImage( file = os.path.join(cdms2.__path__[0],'..','..','..','..', 'bin', 'bookmark_folder.gif') )
      self.canvas_bookicon.create_image(0,0, anchor=Tkinter.NW, image=self.img2 )
      self.canvas_bookicon.bind( '<1>', gui_control.Command( gui_edit_list.create, parent, 'directory' ))

      # Set up cycle throught favorite directories icon
      self.canvas_cycleicon = Tkinter.Canvas(frameb, bd=0, highlightthickness=0,
                                        width = 27, height = 27)
      self.canvas_cycleicon.pack( side='left', fill='x', padx=5 )
      parent.balloon.bind( self.canvas_cycleicon,
                 "Cycle through your favorite working directories." )
      self.img3 = Tkinter.PhotoImage( file = os.path.join(cdms2.__path__[0],'..','..','..','..', 'bin', 'cycle.gif') )
      self.canvas_cycleicon.create_image(0,0, anchor=Tkinter.NW, image=self.img3 )
      self.canvas_cycleicon.bind( '<1>', gui_control.Command( evt_cycle_through_directories, parent ))
      
      frameb.pack( side='top', fill='x', expand=0 )

      #-------------------------------------------
      # line 3
      #-------------------------------------------
      framec = Tkinter.Frame( FRAME )
      self.textv_lab3 = Tkinter.StringVar()
      self.lab3 = Tkinter.Label( framec, textvariable=self.textv_lab3 )
      if cdat_info.CDMS_INCLUDE_DAP != 'yes':
         self.textv_lab3.set("File:        " )
      else:
         self.textv_lab3.set("File or URL: " )
      self.tin3 = Pmw.ComboBox( framec, 
                                    scrolledlist_items = f,
		                    entryfield_value = "Type in a file name or select file using pull down arrow -->.",
                                    entry_width=1,
                                    entry_background='white', entry_foreground='black',
                                    selectioncommand=gui_control.Command(evt_enter_file, parent)
                                   )
      parent.balloon.bind( self.tin3._entryfield, "Enter the file name. Can use the arrow button to\nthe right or the 'Open File' menu item located\nunder the 'File' menu option to also set the file\nname." )
      parent.balloon.bind( self.tin3._arrowBtn, 'View and Select Files in Directory' )
      self.tin3.first_time = 1
      self.lab3.pack( side='left', padx=2 )
      self.tin3.pack( side='left', fill='x', expand=1,
                        pady=5, padx=5, ipady=2 )
      # Set up files bookmarks (i.e., favorite directories icon)
      self.canvas_bookicon_file = Tkinter.Canvas(framec, bd=0, highlightthickness=0,
                                        width = 27, height = 27)
      self.canvas_bookicon_file.pack( side='left', fill='x', padx=5 )
      parent.balloon.bind( self.canvas_bookicon_file,
                 "Bookmark your favorite working files." )
      self.img2_file = Tkinter.PhotoImage( file = os.path.join(cdms2.__path__[0],'..','..','..','..', 'bin', 'save_file.gif') )
      self.canvas_bookicon_file.create_image(0,0, anchor=Tkinter.NW, image=self.img2_file )
      self.canvas_bookicon_file.bind( '<1>', gui_control.Command( gui_edit_list.create, parent, 'mybinfiles' ))

      # Set up cycle throught favorite files icon
      self.canvas_cycleicon_files = Tkinter.Canvas(framec, bd=0, highlightthickness=0,
                                        width = 27, height = 27)
      self.canvas_cycleicon_files.pack( side='left', fill='x', padx=5 )
      parent.balloon.bind( self.canvas_cycleicon_files,
                 "Cycle through your favorite working files." )
      self.img3_files = Tkinter.PhotoImage( file = os.path.join(cdms2.__path__[0],'..','..','..','..', 'bin', 'recycle_file.gif') )
      self.canvas_cycleicon_files.create_image(0,0, anchor=Tkinter.NW, image=self.img3_files )
      self.canvas_cycleicon_files.bind( '<1>', gui_control.Command( evt_cycle_through_files, parent ))

      framec.pack( side='top', fill='both', expand=0 )

      #-------------------------------------------
      # line 4
      #-------------------------------------------
      framed = Tkinter.Frame( FRAME )
      self.textv_lab4 = Tkinter.StringVar()
      self.lab4 = Tkinter.Label( framed, textvariable=self.textv_lab4 )
      if cdat_info.CDMS_INCLUDE_DAP != 'yes':
         self.textv_lab4.set("Variable: " )
      else:
         self.textv_lab4.set("Variable:      " )
      self.tin4 = Pmw.ComboBox( framed,
	                    entryfield_value = "Type in variable name or select variable using pull down arrow -->.",
                            entry_width=1,
                            entry_background='white', entry_foreground='black',
                            selectioncommand=gui_control.Command(evt_enter_variable, parent)
                            )
      parent.balloon.bind( self.tin4._entryfield, "Enter the variable name. Can use the arrow button\nto the right to view and select variables." )
      parent.balloon.bind( self.tin4._arrowBtn, 'View and Select Variables in File' )
      self.tin4.first_time = 1
      self.lab4.pack( side='left', padx=2, pady=2 )
      self.tin4.pack( side='left', fill='x', expand=1,
                        padx=5, ipady=2 )

      # Set up cycle throught favorite directories icon
      self.file_infoicon = Tkinter.Canvas(framed, bd=0, highlightthickness=0,
                                        width = 27, height = 27)
      self.file_infoicon.pack( side='left', fill='x', padx=5 )
      parent.balloon.bind( self.file_infoicon,
                 "Display file variable information." )
      self.img_info = Tkinter.PhotoImage( file = os.path.join(cdms2.__path__[0],'..','..','..','..', 'bin', 'info.gif') )
      self.file_infoicon.create_image(0,0, anchor=Tkinter.NW, image=self.img_info )
      self.file_infoicon.bind( '<1>', gui_control.Command( evt_display_file_info, parent ))
      framed.pack( side='top', fill='both', expand=0 )

      self.tin2.component('entry').bind( "<Key>", gui_control.Command(evt_change_color, parent) )
      self.tin2.component('entry').bind( "<Return>", gui_control.Command(evt_enter_directory, parent) )
      #self.tin2.component('entry').bind( "/", gui_control.Command(evt_enter_directory, parent) )
      self.tin2.component('entry').bind('<Tab>', gui_control.Command(evt_tab,parent) )
      self.tin2.component('entry').bind( "<BackSpace>", gui_control.Command(evt_backspace, parent) )
      self.tin3.component('entry').bind( "<Return>", gui_control.Command(evt_enter_file, parent) )
      self.tin4.component('entry').bind( "<Return>", gui_control.Command(evt_enter_variable, parent)  )

#---------------------------------------------------------------------
# End Select Varialbe Panel Layout
#---------------------------------------------------------------------
#

#-----------------------------------------------------------------
# event functions associated with the "Select Variable" panel
#-----------------------------------------------------------------
def evt_change_color( parent, event ):
   keycolor = Pmw.Color.changebrightness(parent, 'red', 0.85)
   parent.panelSV.tin2.configure( entry_background = keycolor )
#
####### event 'icon file open' for directory and file entry
#
def evt_icon_open_file( parent, dirfilename=None, event=None ):
   if gui_control.directory_or_database == 'directory':
      # Show the popup directory dialog
      if dirfilename == None:
         dialog_icon = tkFileDialog.Open(master=parent,
                           filetypes=gui_control.datatypes, title = 'File Select')
         dirfilename=dialog_icon.show(initialdir=os.getcwd())
         if dirfilename in [(), '']: return
         f=string.split( dirfilename, '/' )[-1]
         d=dirfilename[0:(len(dirfilename) - len(f) - 1)]
         parent.panelSV.tin2.clear()
         parent.panelSV.tin3.clear()
         parent.panelSV.tin2.setentry( d )
         gui_select_variable.evt_enter_directory( parent, None )
         parent.panelSV.tin3.setentry( f )
         tmp=parent.file_search_pattern
         parent.file_search_pattern='All Files'
         gui_select_variable.evt_enter_file( parent, None )
         parent.file_search_pattern=tmp
         parent.panelSV.tin3.component( 'entry' ).focus_set()
      else:
         parent.panelSV.tin2.setentry( dirfilename )
         gui_select_variable.evt_enter_directory( parent, None )
         parent.panelSV.tin2.component( 'entry' ).focus_set()
#
####### event 'icon opendap' for directory and file entry
#
def evt_icon_opendap_file( parent, dirfilename=None, event=None ):
   f = gui_opendap.create( parent, parent.menu.popup_window_settings_flg )
#
####### event 'cycle through directories' for directory entry
#
def evt_cycle_through_directories( parent, event ):
   if gui_control.favorite_index is not None:
      dlen = len( gui_control.favorite_directories ) - 1
      f = gui_control.favorite_directories[ gui_control.favorite_index ]
      parent.panelSV.tin2.setentry( f )
      evt_enter_directory( parent, None )

      # Set the cycle control index
      gui_control.favorite_index += 1
      if (gui_control.favorite_index > dlen):
         gui_control.favorite_index = 0
      parent.panelSV.tin2.component( 'entry' ).focus_set()
   else:
      return
#
####### event 'cycle through files' for files entry
#
def evt_cycle_through_files( parent, event ):
   dlen = len( gui_control.favorite_files ) - 1
   if (gui_control.favorite_files_index is None): return
   df = gui_control.favorite_files[ gui_control.favorite_files_index ]
   if (df[:7] != 'http://') and (cdat_info.CDMS_INCLUDE_DAP == 'yes'):
      if os.access(df, os.R_OK) == 1:
        d = df[:string.rfind(df,'/')]
        f = df[(string.rfind(df,'/')+1):]
      else:
        d = df
      parent.panelSV.tin2.setentry( d )
      evt_enter_directory( parent, None )
   elif (df[:7] == 'http://') and (cdat_info.CDMS_INCLUDE_DAP == 'yes'):
      f = df
      parent.panelSV.tin2.setentry('') # clear "directory" entry
      parent.panelSV.tin3.clear( )     # clear "file" and combo box 
   elif (df[:7] != 'http://') and (cdat_info.CDMS_INCLUDE_DAP != 'yes'):
      if os.access(df, os.R_OK) == 1:
        d = df[:string.rfind(df,'/')]
        f = df[(string.rfind(df,'/')+1):]
      else:
        d = df
      parent.panelSV.tin2.setentry( d )
      evt_enter_directory( parent, None )
   elif (df[:7] == 'http://') and (cdat_info.CDMS_INCLUDE_DAP != 'yes'):
      f = df
      parent.panelSV.tin2.setentry('') # clear "directory" entry
      parent.panelSV.tin3.clear( )     # clear "file" and combo box entry and list
      parent.panelSV.tin4.clear( )	   # clear "variable" combo box entry and list

   try: parent.panelSV.tin3.setentry( f )
   except: return
   evt_enter_file( parent, None )

   # Set the cycle control index
   gui_control.favorite_files_index += 1
   if (gui_control.favorite_files_index > dlen):
      gui_control.favorite_files_index = 0
   parent.panelSV.tin2.component( 'entry' ).focus_set()
#
####### event 'cycle through directories' for directory entry
#
def evt_display_file_info( parent, event ):
   try: g = parent.panelDM.fid2
   except: 
        gui_message.error("Must enter file or variable name in the 'File' or 'Variable' entry window.")
        return
   try:
      v =parent.panelDM.var3
   except:
      v = parent.panelDM.var3 = None

   if (v is not None) or (g is not None):
      if v is not None:
         info_text = gui_functions._scn_a_var( parent, v )
      else:
         info_text = gui_functions._scn_a_file( parent, v, g )

      dialog = Pmw.Dialog(parent,
                buttons = ('Dismiss',),
                defaultbutton = 'OK',
                title = '%s  -  File Variable Information' % v,
                #command = gui_control.Command(evt_dispose_execute, parent)
        )
      dialog.transient( parent ) # draw widget on top of its parent

      # Create the ScrolledText.
      sclt = Pmw.ScrolledText(dialog.interior(),
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
      sclt.settext( info_text )
      sclt.configure( text_state = 'disabled' )

      # Position dialog popup
      parent_geom = parent.geometry()
      geom = string.split(parent_geom, '+')
      geom2 = string.split(geom[0], 'x')
      d1 = string.atoi( geom[1] )
      d2 = string.atoi( geom[2] )
      d3 = string.atoi( geom2[0] )
      d4 = string.atoi( geom2[1] ) * 0.88
      dialog.activate( geometry="%dx%d+%d+%d" % (d3, d4, d1, d2) )
   else:
      gui_message.error("Must enter file or variable name in the 'File' or 'Variable' entry window.")

#
####### event 'text input' for directory or database entry
#
def evt_enter_directory( parent, event, who_called=0 ):
   # change backgound color to white
   parent.panelSV.tin2.configure( entry_background = 'white' )
   #
   Match_found = None
   if gui_control.directory_or_database == 'directory':
      if type(event) == types.StringType:
            if event[0] == '/':
               a = event
            elif event[0:5] == '=====':
               a = os.getcwd()
            else:
               a = os.getcwd() + '/' + event
      else:
         a = parent.panelSV.tin2.get()
         # Add tilde function to entry window
         if a == '~':
           a = os.environ['HOME']
         elif a[0:2] == '~/':
           a = os.environ['HOME'] + '/' + a[2:]
         elif a[0] == '~':
           a = os.environ['HOME'] + '/' + a[1:]
         a = a + '/'
      try:
         os.chdir( a )
      except:
         gui_message.error(('Cannot access directory [ %s ].' % a))
      a=a[0:-1]

      if parent.startusertask == 1: gui_control.track_user( parent, "Directory: %s" % a)

      parent.panelSV.tin2.clear( )		# clear "directory" combo box entry and list
      if parent.panelSV.tin3.first_time == 1:
         parent.panelSV.tin3.delete(0, 'end')	# clear only the "file" combo box list
      else:
         parent.panelSV.tin3.clear( )	        # clear "file" combo box entry and list
      if parent.panelSV.tin4.first_time == 1:
         parent.panelSV.tin4.delete(0, 'end')	# clear only the "variable" combo box list
      else:
         parent.panelSV.tin4.clear( )	        # clear "variable" combo box entry and list
      #
      if who_called != 1:
         t = os.getcwd()
      else:
         t = os.getcwd() + ' '
      parent.panelSV.tin2._entryfield.setentry( t )
      #
      d,f = gui_functions._scn_a_dir( parent )        # re-write the new directory information and save directory list
      gui_functions._scn_dir_history( parent, t )      # store access directory information to the history directory list
      for j in range(len( parent.dir_history )): parent.panelSV.tin2.insert( j, parent.dir_history[j] )
      for i in range(len(d)): parent.panelSV.tin2.insert( j+i+1, d[i] )
      #
      for i in range(len(f)): parent.panelSV.tin3.insert( i, f[i] )
      #
      parent.panelDM.fid2 = None
      parent.panelDM.var3 = parent.panelDM.remember_var3 = None
      #
      gui_functions._blank_dim1_to_ndim( parent )
      #
      if (parent.panelSV.tin3.first_time != 1) and (len(f) > 0):
         parent.panelSV.tin3.setentry( f[0] )
         evt_enter_file( parent, None )
      #
      # record the event: select a directory
      command = "os.chdir( '%s' )" % os.getcwd()
      gui_control.record_command( parent, command )
   elif gui_control.directory_or_database == 'database':
      if event.keysym == 'Return':
         try:
            parent.parent.update()
            g_wait = gui_message.wait(parent.parent,'Please wait while the connection\n to the database is made.')
            gui_control.db=cdms2.connect( parent.parent.tin2.get() )
            parent.parent.update()
            g_wait.progress()
            for x in gui_control.db_connections:
               Match_found = re.match( parent.parent.tin2.get(), x )
               if Match_found: break
            if not Match_found:
               gui_control.db_connections.append( parent.parent.tin2.get() )
            parent.parent.panelC.scl1.setlist( gui_control.db_connections )
            db_list = gui_control.db.listDatasets()
            db_list.sort()
            parent.parent.panelC.scl2.setlist( db_list )
            parent.parent.panelC.scl3.setlist( '' )
            gui_functions._blank_dim1_to_ndim( parent )
            g_wait.execute()
         except:
            g_wait.execute()
            gui_message.error("Invalid database string.\nCould not connect to specified database.")
#
####### event for directory entry only, search for a tab entry
#
def evt_tab( parent, event ):
   if gui_control.directory_or_database == 'directory':
      s=string.split( parent.panelSV.tin2.get(), '/' )[-1]
      s2=string.split( parent.panelSV.tin2.get(), '/' )
      d = ''
      for x in os.listdir( './' ):
         if os.path.isfile( x ):
           pass
         else:
            if string.find( x, s ) == 0:
               if len(d):
                  if len(d) > len(x):
                     d = x
               else:
                  d = x
      if d == '': d = s2[-1]
      r = ''
      for x in s2[1:-1]: r = r + '/' + x
      r = r + '/' + d
      parent.panelSV.tin2.delete( 0, 'end' )
      parent.panelSV.tin2.setentry( r )
      #
      evt_enter_directory( parent, None )
      # 
      # Override the behavior of the Tab for this widget.
      # Just do the above and stop.
      return "break"
   elif gui_control.directory_or_database == 'database':
      pass

#
####### event for directory entry only, search for back space entry
#
def evt_backspace( parent, event ):
   if gui_control.directory_or_database == 'directory':
      keycolor = Pmw.Color.changebrightness(parent, 'red', 0.85)
      parent.panelSV.tin2.configure( entry_background = keycolor )
      t = parent.panelSV.tin2.get()
      s=string.split( t, '/' )[-1]
      if s == '':
         evt_enter_directory( parent, None, 1 )
#
####### event for file entry
#
def evt_enter_file( parent, event ):
   a = parent.panelSV.tin3.get()

   # Must close some files (like DRS) before you can open it again
   try: parent.panelDM.fid2.close()
   except: pass
   
   if parent.startusertask == 1: gui_control.track_user( parent, "File: %s" % a)
   if gui_control.directory_or_database == 'directory':
      if a[:7]!="http://":
         d,f = gui_functions._scn_a_dir( parent )     # re-write the new directory information and save directory list
         #
         parent.panelSV.tin3.first_time = 0
         #
         found = 0
         for i in range(len(f)):
            if (f[i] == a):
               try:
                  parent.panelDM.fid2 = cdms2.open( a ) 
                  #
                  # record the event: select a file to be opened
                  gui_control.record_command( parent, '\n# Open CDMS File', 1 )
                  command = "fid2 = cdms2.open( '%s/%s' )" % (os.getcwd(), a)
                  gui_control.record_command( parent, command, 1 )
                  found = 1
               except:
                  found = 0
                  gui_message.error('Cannot open data file [%s].' % a)
                  return
               gui_functions._srl_file_var( parent )
               gui_functions._blank_dim1_to_ndim( parent )
               break
         if found == 0:
             gui_message.error("Invalid file name.")
         else:
            if ( (parent.panelSV.tin4.first_time != 1) and
                 (parent.panelDM.lst3 != []) and
                 (len(parent.panelDM.lst3[0]) > 0) ):
               pass
               #parent.panelSV.tin4.setentry( parent.panelDM.lst3[0] )
               #evt_enter_variable( parent, None )
      else:
         try:
            parent.panelDM.fid2 = cdms2.open( a ) 
            #
            # record the event: select a file to be opened
            gui_control.record_command( parent, '\n# Open CDMS File', 1 )
            command = "fid2 = cdms2.open( '%s' )" % (a)
            gui_control.record_command( parent, command, 1 )
         except:
            import cdat_info
            if cdat_info.CDMS_INCLUDE_DAP=='yes':
               gui_message.error('Cannot open data file [%s].' % a)
               return
            else:
               gui_message.error('Your version of CDAT is not built with DAP support !')
               return
         gui_functions._srl_file_var( parent )
         gui_functions._blank_dim1_to_ndim( parent )
            
   else:
      f = parent.panelSV.tin3.ds_list
      found = 0
      for i in range(len(f)):
         if (f[i] == a):
            try:
               parent.panelDM.fid2 = parent.panelDM.lst2 = gui_control.db.open( a ) 
            except:
               gui_message.error('Cannot open database file [%s].' % a)
            #parent.panelDM.lst2 = parent.panelDM.fid2
            gui_functions._srl_file_var( parent )
            gui_functions._blank_dim1_to_ndim( parent )
            found = 1
            break
      if found == 0:
         gui_message.error("Invalid file name.")
#
####### event for variable entry
#
def evt_enter_variable( parent, event ):
   a = b = parent.panelSV.tin4.get()
   parent.panelSV.lab1.configure(text = "Select Variable")
   if a.find('- [') != -1:
      a=a[:a.find('- [')]
   if a.find('(') !=-1:
      a = a[:a.rfind('(')-1]
   parent.panelSV.tin4.setentry( a )
   if parent.startusertask == 1: gui_control.track_user( parent, "Variable: %s" % a)
   #
   if ( (a[17:] in ['Variable List *************']) or (a in ['******** Bounds and Weights List ******', '*************** Axes List **************']) ):
     parent.panelSV.tin4.setentry( '' )
     gui_functions._blank_dim1_to_ndim( parent )
     parent.panelDM.var3 = parent.panelDM.remember_var3 = None
     return
   parent.panelDM.var3 = parent.panelDM.remember_var3 = a
   #
   parent.panelSV.tin4.first_time = 0
   #
   var_list = parent.panelSV.tin4._list.get(None)
   found = 0
   for j in range(len(var_list)):
      n = len(a)
      if a+' '==var_list[j][:n+1]:
         found = 1
         break
##          if var_list[j].find('(') == -1:
##             if a == var_list[j]: 
##                found = 1
##                break
##          else:
##             if a == var_list[j][:var_list[j].find('(')-1]: 
##                found = 1
##                break
##          j = j + 1
   if found == 0:
      gui_message.error('Variable [ ++%s++ ] cannot be found in the file.' % a)
      parent.panelSV.tin4.setentry( '' )
      gui_functions._blank_dim1_to_ndim( parent )
      parent.panelDM.var3 = parent.panelDM.remember_var3 = None
      return
   #
   parent.panelVI.lst1 = gui_functions._scn_a_var( parent, a )
   parent.panelVI.scl1.settext( parent.panelVI.lst1 )

   # Blank the dimension's window
   gui_functions._blank_dim1_to_ndim( parent )

   # Reset selection counters back to initial state, (e.g., '00 ')
   if parent.panelDV.lst1 != {}:
      for x in parent.panelDV.lst1.keys():
          parent.panelDV.number_lst1[ x ] = gui_control.dvholder + x
      gui_defined_variables.update_defined_variable_list( parent )

   gui_functions._srl_var_dim1_to_ndim( parent )
   for k in range(parent.panelDM.ndim):
      parent.panelDM.dim[k].comb.component('entry').configure(background = gui_color.dim )
      parent.panelDM.dim[k].first_scl.configure( troughcolor=gui_color.dim )
      parent.panelDM.dim[k].last_scl.configure( troughcolor=gui_color.dim )
   #
   parent.panelDV.scl1.select_clear( 0, 'end' )
   parent.panelDV.selected_list = {}
   #
   parent.panelSV.lab1.configure(text = "Selected Variable -- %s" % b)
#
####### event in 'Directory or Database settings'
#
def restore_directory_setting( self, parent ):
   # restore the directory name
   self.textv_opt9.set('Directory')

   # Restore the icons
   self.canvas_homeicon.pack( side='left', after=self.opt9, fill=Tkinter.BOTH, padx=5 )
   self.canvas_openicon.pack( side='left', after=self.canvas_homeicon, fill=Tkinter.BOTH, padx=5 )
   self.canvas_portalicon.pack( side='left', after=self.canvas_openicon, fill=Tkinter.BOTH, padx=5 )
   self.canvas_bookicon.pack( side='left', fill=Tkinter.BOTH, padx=5 )
   self.canvas_cycleicon.pack( side='left', fill=Tkinter.BOTH, padx=5 )

   gui_control.directory_or_database = 'directory'
   #
   parent.balloon.bind(parent.panelSV.tin2._arrowBtn,'View and Select Directories')        
   parent.balloon.bind(parent.panelSV.tin3._arrowBtn,'View and Select Files in Directory')
   parent.balloon.bind(parent.panelSV.tin4._arrowBtn, 'View and Select Variables in File')
   #
   parent.panelSV.textv_lab3.set("File:        " )
   #
   d,f = gui_functions._scn_a_dir( parent )
   #
   parent.panelSV.tin2.clear( )        # clear "directory" combo box entry and list
   parent.panelSV.tin2.setentry(  os.getcwd() ) # re-write directory information
   for i in range(len(d)): parent.panelSV.tin2.insert( i, d[i] )
   parent.panelSV.tin3.clear( )        # clear "directory" combo box entry and list
   for i in range(len(f)): parent.panelSV.tin3.insert( i, f[i] )
   parent.panelSV.tin4.clear( )        # clear "directory" combo box entry and list
   gui_functions._blank_dim1_to_ndim( parent )
#
####### event to toggle between the 'Directory' or 'Database' 
#
def evt_popup_database_gui( self, parent, event ):
   if (event == 'Database'):
      self.canvas_homeicon.pack_forget()
      self.canvas_openicon.pack_forget()
      self.canvas_portalicon.pack_forget()
      self.canvas_bookicon.pack_forget()
      self.canvas_cycleicon.pack_forget()
      gui_control.directory_or_database = 'database'
    #  parent.panelSV.textv.set( 'Databases' )
    #  parent.panelSV.textv_opt3.set( 'Datasets' )
    #  parent.panelSV.opt3.configure( menubutton_state = 'disabled' )
      #
      (host, port, path) = gui_database.get_cdms_database_info( )
      if host is not None:
         gui_database.create( parent, host, port, path )
         parent.panelSV.tin2.delete( 0, 'end' )
    #     parent.panelSV.scl1.setlist( '' )
    #     parent.panelSV.scl2.setlist( '' )
    #     parent.panelSV.scl3.setlist( '' )
         gui_functions._blank_dim1_to_ndim( parent )
      else:
         restore_directory_setting( self, parent )
   else:
      restore_directory_setting( self, parent )

#---------------------------------------------------------------------
# End of File
#---------------------------------------------------------------------
