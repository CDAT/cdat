#!/usr/bin/env python
#
# The VCS Animation GUI controls -  animationgui module
#
###############################################################################
#                                                                             #
# Module:       animationgui module                                           #
#                                                                             #
# Copyright:    "See file Legal.htm for copyright information."               #
#                                                                             #
# Authors:      PCMDI Software Team                                           #
#               Lawrence Livermore NationalLaboratory:                        #
#               support@pcmdi.llnl.gov                                        #
#                                                                             #
# Description:  PCMDI's VCS animation GUI browser and editor.                 #
#                                                                             #
# Version:      5.0                                                           #
#                                                                             #
###############################################################################

#---------------------------------------------------------------------
# NOTE: need to use version of Python that imports Tkinter and Pmw
#---------------------------------------------------------------------
import Tkinter, Pmw, tkFileDialog
from tkMessageBox import showerror
import os, string, sys
import gui_support
from browser import gui_control
import cdms2

#-----------------------------------------------------------------------------
#-----------------------------------------------------------------------------
#
# Create the Tkinter/Pmw Animation editor interface
#
#------------------------------------------------------------------------------
#------------------------------------------------------------------------------
class AnimationGui:
    def __init__( self, animation, gui_parent=None, transient=0):
        # Set the animation min and max values
        self.setting = 0
        self.min = None
        self.max = None
        self.animation = animation
        title = "%d. - VCS's Canvas Animation Editor" % \
                animation.vcs_legacy_self.canvasid() 

        self.dialog = gui_support.VcsDialog(title=title,
                                 buttons=(),
                                 command = A_Command(self.execute, gui_parent)
                                 )
        self.dialog.dialog.withdraw()
        parent = self.dialog.interior()
        self.parent = parent
        # create the animation main toplevel menu
        self.amain_menu = Pmw.MenuBar(parent,
                hull_relief = 'raised',
                hull_borderwidth = 2,
                balloon = gui_support.balloon
                )
        self.amain_menu.pack(side='top', fill='both')

        # Set the tear off value
        tear_it = 1
        if ((sys.platform == 'darwin') and (gui_control.do_aqua == 1)): tear_it = 0

        #-------------------------------------------
        # menu 1 -- 'File'
        #-------------------------------------------
        self.create_file_menu(self.amain_menu,gui_parent,tear_it)

        #-------------------------------------------
        # menu 3 -- 'Options'
        #-------------------------------------------
        if animation.vcs_legacy_self._canvas_gui is None:
           create_animation_options_menu(self,self.amain_menu,parent,animation,tear_it)

        #-------------------------------------------
        # menu 4 -- 'Help'
        #-------------------------------------------
        if animation.vcs_legacy_self._canvas_gui is None:
           self.create_help_menu( gui_parent, self.amain_menu, tear_it)


        #-------------------------------------------
        # File Controls -- 'Create and load'
        #-------------------------------------------
        if animation.vcs_legacy_self._canvas_gui is None:
           create_animation_file_controls ( self, parent, gui_parent, animation )

        #-------------------------------------------
        # Button Controls -- 'Create, Run, and Stop Buttons'
        #-------------------------------------------
        create_animation_control_buttons ( self, parent, gui_parent, animation )
        if animation.vcs_legacy_self._canvas_gui is not None:
           self.start_btn.configure(state = 'disabled')

        #-------------------------------------------
        # 'Controls Animation Frames'
        #-------------------------------------------
        create_control_animation_frame ( self, parent, animation )

        #-------------------------------------------
        # 'Zoom and Pan Animation Frames'
        #-------------------------------------------
        create_zoom_and_pan_animation_frame ( self, parent, animation )
        # Decide where to put it on the screen
        if gui_parent is None:
            d=[self.dialog.dialog.winfo_screenwidth()/4,
               self.dialog.dialog.winfo_screenheight()/4
              ]
        else:
            g=gui_parent.geometry()
            d = g.split('+')[1:]
            
        self.dialog.geometry("+%s+%s"% (d[0],d[1]))
        if (gui_parent is not None) and (transient==1):
           self.dialog.dialog.transient(gui_parent) # Keep widget on top of its parent
        
        self.dialog.dialog.deiconify()

    def execute (self, parent, event):
        if self.animation.vcs_legacy_self._canvas_gui is not None:
           self.dialog.dialog.withdraw()
           return
        self.destroy_close_animation( parent )
        # Reset the Page Description Panel so that no forms are "On".
        who_is_on = []
        if (parent is not None):
         if (parent.panelGC.over_flg != 1) or (parent.panelDV.selected == []):
           for i in parent.pl.form.keys():
             if parent.pl.form[i].lbl.cget( 'label_text' ) == 'On':
                try:
                   who_is_on.append( i )
                   parent.pl.form[i].display.off = 1
                except:
                   pass
                parent.pl.form[i].off_btn.create_image(0,0, anchor=Tkinter.NW, image=parent.pl.form[i].off_btnimage )
                parent.pl.form[i].lbl.configure( label_text = 'Off' )

        # Restore the VCS Canvas to its original state before animation.
        if (parent is not None):
           for i in range(len(who_is_on)):
               parent.pl.evt_status_button( who_is_on[i], parent, None )

    def create_file_menu(self, main_menu, gui_parent,tear_it):
        file_name = 'File'
        if ((sys.platform == 'darwin') and (gui_control.do_aqua == 1)): file_name = 'File '
        main_menu.addmenu(file_name, "Exit VCS's Animation Editor", tearoff = tear_it)
        main_menu.addmenuitem(file_name, 'command', 'Exit Animation Editor',
                          label = "Exit Animation Editor",
                          command = A_Command(self.execute, gui_parent, None)
#                          command = A_Command(self.destroy_close_animation, gui_parent)
                         )

   ####### event to close animation and destroy the animation GUI
   #
    def destroy_close_animation(self, gui_parent=None):
        self.animation.stop( )        # Stop the animation if it is running
        self.animation.close( )	# Close the animation procedure
        self.animation.vcs_legacy_self.clear( ) # Clear the VCS window
        self.animation.gui_popup = 0	 # Reset the animation GUI popup flag
        self.dialog.destroy()

    def create_help_menu( self, parent, main_menu, tear_it):
        help_name = 'Help'
        if ((sys.platform == 'darwin') and (gui_control.do_aqua == 1)): help_name = 'Help '
        main_menu.addmenu(help_name, 'VCS Animation Help', side='right', tearoff = tear_it)
        gui_support.add_balloon_help (main_menu, help_name)
        main_menu.addmenuitem(help_name, 'separator')

        main_menu.addmenuitem(help_name, 'command', 'Help About',
                            label = 'About the Animation Editor',
                            command = A_Command(self.evt_about_dialog, parent)
                           )

   ####### event to create the About dialog        
   #
    def evt_about_dialog( self, parent):
        Pmw.aboutversion(sys.prefix)
        Pmw.aboutcopyright('\nCopyright:    2001, Regents of the University of California\nThis software may not be distributed to others without\npermission of the authors.\nAll rights reserved.')
        Pmw.aboutcontact(
            """Go to cdat.sourceforge.net for documentation, support, bug reporting, and releases.
Program for Climate Model Diagnosis and Intercomparison
Lawrence Livermore National Laboratory Livermore, CA 94550 """)
        self.about = Pmw.AboutDialog(self.parent, applicationname = "The Visualization Control System's - (VCS) Animation Editor")
        self.about.transient(self.parent ) # draw widget on top of its parent
        # Position dialog popup
        parent_geom = parent.geometry()
        geom = string.split(parent_geom, '+')
        d1 = string.atoi( geom[1] )
        d2 = string.atoi( geom[2] )
        self.about.geometry( "+%d+%d" % (d1, d2) )

#----------------------------------------------------------------------------------------
#----------------------------------------------------------------------------------------
#
# Create the Animation Options menu and its menu items
#
#----------------------------------------------------------------------------------------
#----------------------------------------------------------------------------------------
class create_animation_options_menu:
   def __init__( self, eself, main_menu, parent, animation, tear_it ):
      opt_name = 'Options'
      if ((sys.platform == 'darwin') and (gui_control.do_aqua == 1)): opt_name = 'Options   '
      main_menu.addmenu(opt_name, 'Set VCS Animation Preferences', tearoff = tear_it)

      main_menu.addmenuitem(opt_name, 'command', 'view colormap values',
                       label = 'Set Animation Min and Max',
                       command = A_Command(self.evt_set_min_max, eself, parent, animation)
                      )
      main_menu.addcascademenu(opt_name, 'Set Mode', 'set animation mode',
                       label = 'Set Animation Mode',
                       traverseSpec = 'z', tearoff = tear_it)

      self.mode_toggle_1 = Tkinter.IntVar()
      self.mode_toggle_1.set( 1 )
      main_menu.addmenuitem('Set Mode', 'checkbutton', 'Toggle, Cycle',
                          label = 'Cycle',
                          variable = self.mode_toggle_1,
                          command = A_Command(self.evt_set_mode, animation, parent, 1)
                         )
#      self.mode_toggle_2 = Tkinter.IntVar()
#      self.mode_toggle_2.set( 0 )
#      main_menu.addmenuitem('Set Mode', 'checkbutton', 'Toggle Once',
#                          label = 'Once',
#                          variable = self.mode_toggle_2,
#                          command = A_Command(self.evt_set_mode, animation, parent, 2)
#                         )
      self.mode_toggle_3 = Tkinter.IntVar()
      self.mode_toggle_3.set( 0 )
      main_menu.addmenuitem('Set Mode', 'checkbutton', 'Toggle Forth and Back',
                          label = 'Forth and Back',
                          variable = self.mode_toggle_3,
                          command = A_Command(self.evt_set_mode, animation, parent, 3)
                         )

      main_menu.addcascademenu(opt_name, 'Set Direction', 'set animation direction',
                       label = 'Set Animation Direction',
                       traverseSpec = 'z', tearoff = tear_it)
      self.direction_toggle_1 = Tkinter.IntVar()
      self.direction_toggle_1.set( 1 )
      main_menu.addmenuitem('Set Direction', 'checkbutton', 'Toggle Forward',
                          label = 'Forward',
                          variable = self.direction_toggle_1,
                          command = A_Command(self.evt_set_direction, animation, parent, 1)
                         )
      self.direction_toggle_2 = Tkinter.IntVar()
      self.direction_toggle_2.set( 0 )
      main_menu.addmenuitem('Set Direction', 'checkbutton', 'Toggle Backward',
                          label = 'Backward',
                          variable = self.direction_toggle_2,
                          command = A_Command(self.evt_set_direction, animation, parent, 2)
                         )

   ####### event to set the animation mode 
   #
   def evt_set_mode( self, animation, parent, number ):
      if number == 1:                # Cycle through frames
         self.mode_toggle_1.set( 1 )
#         self.mode_toggle_2.set( 0 )
         self.mode_toggle_3.set( 0 )
#      elif number == 2:              # Once through frames
#         self.mode_toggle_1.set( 0 )
#         self.mode_toggle_2.set( 1 )
#         self.mode_toggle_3.set( 0 )
      elif number == 3:              # Forth and back through frames
         self.mode_toggle_1.set( 0 )
#         self.mode_toggle_2.set( 0 )
         self.mode_toggle_3.set( 1 )
      animation.vcs_legacy_self.animate.mode( number )


   ####### event to set the animation direction 
   #
   def evt_set_direction( self, animation, parent, number ):
      if number == 1:       # Move forward through frames
         self.direction_toggle_1.set( 1 )
         self.direction_toggle_2.set( 0 )
      elif number == 2:       # Move backward through frames
         self.direction_toggle_1.set( 0 )
         self.direction_toggle_2.set( 1 )
      animation.vcs_legacy_self.animate.direction( number )

   ####### event to set the animation direction
   #
   def evt_set_min_max( self, eself, parent, animation ):
        self.mmdialog = Pmw.Dialog( parent,
            title = 'Set Minimum and Maximum Animation Values',
            buttons = ('OK', 'Dismiss'),
            defaultbutton = 'OK',
            command = A_Command(self.mmexecute, eself, parent) )

        self.mmdialog.transient( parent ) # draw widget on top of its parent

	# Create and pack a RadioSelect widget, with radiobuttons.
	self.radiobuttons = Pmw.RadioSelect(self.mmdialog.interior(),
		buttontype = 'radiobutton',
		orient = 'vertical',
		labelpos = 'w',
	#	command = self.callback,
	#	label_text = 'Vertical,\nusing\nradiobuttons',
		hull_borderwidth = 2,
		hull_relief = 'ridge',
	)
	self.radiobuttons.pack(side = 'left', expand = 1, padx = 10, pady = 10)

	# No longer need to update the display at this point.
        # animation.update_animate_display_list( ) # make sure the list is updated

        if len(animation.vcs_legacy_self.animate_info) < 2:
	   for text in ('Animation sets min and max', 'User sets min and max', 'Turn off animation min and max'):
	       self.radiobuttons.add(text)
	   self.radiobuttons.invoke('Animation sets min and max')
        else:
	   for text in ('Animation sets min and max for all data sets', 'User sets min and max for each data set', 'Turn off animation min and max for each data set'):
	       self.radiobuttons.add(text)
	   self.radiobuttons.invoke('Animation sets min and max for all data sets')

	self.min = Pmw.EntryField(self.mmdialog.interior(),
		labelpos = 'w',
		label_text = 'Min:',
                value = '0',
		modifiedcommand = A_Command(self.set_toggle_flg, animation)
                )
	self.min.pack(side = 'top', expand = 1, padx = 10, pady = 10)
        if len(animation.vcs_legacy_self.animate_info) < 2:
           gui_support.balloon.bind( self.min, "Enter the minimum value for the data set")
        else:
           gui_support.balloon.bind( self.min, "Enter the minimum value for each data set (e.g., 10, 20, 30, 40).")

	self.max = Pmw.EntryField(self.mmdialog.interior(),
		labelpos = 'w',
		label_text = 'Max:',
                value = '0',
		modifiedcommand = A_Command(self.set_toggle_flg, animation)
                )
	self.max.pack(side = 'left', expand = 1, padx = 10, pady = 10)
        if len(animation.vcs_legacy_self.animate_info) < 2:
           gui_support.balloon.bind( self.max, "Enter the maximum value for the data set")
        else:
           gui_support.balloon.bind( self.max, "Enter the maximum value for each data set (e.g., 60, 70, 80, 90).")

        # Position mmdialog popup
        parent_geom = eself.dialog.geometry()
        geom = string.split( parent_geom, '+' )
        d1 = string.atoi(geom[1])
        d2 = string.atoi(geom[2])
        self.mmdialog.geometry( "+%d+%d" % (d1, d2) )


   ####### event to set radio button              
   #
   def set_toggle_flg(self, animation):
        if len(animation.vcs_legacy_self.animate_info) < 2:
           self.radiobuttons.invoke('User sets min and max')
        else:
           self.radiobuttons.invoke('User sets min and max for each data set')

   ####### event to destory the set min and max dialog panel
   #
   def mmexecute(self, eself, parent, result):
        eself.setting = 0
        eself.min = None
        eself.max = None
        if result == 'OK':
           selection = self.radiobuttons.getcurselection()
           if selection in ['Animation sets min and max','Animation sets min and max for all data sets']:
               eself.setting = 0
               eself.min = None
               eself.max = None
           elif selection in ['User sets min and max','User sets min and max for each data set']:
               eself.setting = 1
               if self.min.get( ) != '':
                  s = string.replace( self.min.get( ), ' ', ',' )
                  while string.find(s,',,',0) != -1: s=string.replace(s,',,',',')
                  if s[-1] == ',': s = s[:-1]
                  eself.min = string.split(s, ',')
                  for i in range(len(eself.min)): eself.min[i] = string.atof( eself.min[i] )
               else:
                  eself.min = None

               if self.max.get( ) != '':
                  s = string.replace( self.max.get( ), ' ', ',' )
                  while string.find(s,',,',0) != -1: s=string.replace(s,',,',',')
                  if s[-1] == ',': s = s[:-1]
                  eself.max = string.split(s, ',')
                  for i in range(len(eself.max)): eself.max[i] = string.atof( eself.max[i] )
               else:
                  eself.max = None
           elif selection in ['Turn off animation min and max','Turn off animation min and max for each data set']:
               eself.setting = 2
               eself.min = 1e20
               eself.max = -1e20

        self.mmdialog.destroy()

def nofunc(self):
    return
def evt_select_anim_output_type(self,parent,type):
    if not self.animate_type.getvalue().lower() == 'ras':
        self.animload_binds = self.animate_load.bind()
        self.animload_binds_funcs=[]
        for b in self.animload_binds:
            self.animload_binds_funcs.append(self.animate_load.bind(b))
            self.animate_load.unbind(b)
        self.eny2.configure(entry_state="disabled")
        self.run_btn.configure(state=Tkinter.DISABLED)
        self.runanim_btn.configure(state=Tkinter.DISABLED)
        self.downarrow_binds = self.cntrl_nxt_int.component('downarrow').bind()
        self.downarrow_binds_funcs=[]
        for b in self.downarrow_binds:
            self.downarrow_binds_funcs.append(self.cntrl_nxt_int.component('downarrow').bind(b))
            self.cntrl_nxt_int.component('downarrow').unbind(b)
        self.uparrow_binds = self.cntrl_nxt_int.component('uparrow').bind()
        self.uparrow_binds_funcs=[]
        for b in self.uparrow_binds:
            self.uparrow_binds_funcs.append(self.cntrl_nxt_int.component('uparrow').bind(b))
            self.cntrl_nxt_int.component('uparrow').unbind(b)
        self.cntrl_nxt_int.configure(entry_state='disable')
        self.next_scl.configure(state=Tkinter.DISABLED)
        self.zoom_comb_binds = self.zoom_comb.component('arrowbutton').bind()
        self.zoom_comb_binds_funcs=[]
        for b in self.zoom_comb_binds:
            self.zoom_comb_binds_funcs.append(self.zoom_comb.component('arrowbutton').bind(b))
            self.zoom_comb.component('arrowbutton').unbind(b)
        self.zoom_comb.component("entry").configure(state="disable")
        self.zoom_scl.configure(state="disabled")
        
        self.pan_hor_comb_binds = self.pan_hor_comb.component('arrowbutton').bind()
        self.pan_hor_comb_binds_funcs=[]
        for b in self.pan_hor_comb_binds:
            self.pan_hor_comb_binds_funcs.append(self.pan_hor_comb.component('arrowbutton').bind(b))
            self.pan_hor_comb.component('arrowbutton').unbind(b)
        self.pan_hor_comb.component("entry").configure(state="disable")
        self.pan_hor_scl.configure(state="disabled")
        
        self.pan_ver_comb_binds = self.pan_ver_comb.component('arrowbutton').bind()
        self.pan_ver_comb_binds_funcs=[]
        for b in self.pan_ver_comb_binds:
            self.pan_ver_comb_binds_funcs.append(self.pan_ver_comb.component('arrowbutton').bind(b))
            self.pan_ver_comb.component('arrowbutton').unbind(b)
        self.pan_ver_comb.component("entry").configure(state="disable")
        self.pan_ver_scl.configure(state="disabled")
    else:
        self.eny2.configure(entry_state="normal")
        n = len(self.animload_binds)
        for i in range(n):
            b= self.animload_binds[i]
            self.animload_binds_funcs.append(b)
            self.animate_load.bind(b,self.animload_binds_funcs[i])
        self.run_btn.configure(state=Tkinter.NORMAL)
        self.runanim_btn.configure(state=Tkinter.NORMAL)
        n = len(self.downarrow_binds)
        for i in range(n):
            b=self.downarrow_binds[i]
            self.cntrl_nxt_int.component('downarrow').bind(b,self.downarrow_binds_funcs[i])
        n = len(self.uparrow_binds)
        for i in range(n):
            b=self.uparrow_binds[i]
            self.cntrl_nxt_int.component('uparrow').bind(b,self.uparrow_binds_funcs[i])
        self.cntrl_nxt_int.configure(entry_state='normal')
        n = len(self.zoom_comb_binds)
        for i in range(n):
            self.zoom_comb.component("arrowbutton").bind(self.zoom_comb_binds[i],self.zoom_comb_binds_funcs[i])
        self.zoom_comb.component("entry").configure(state="normal")
        self.zoom_scl.configure(state="normal")
        n = len(self.pan_ver_comb_binds)
        for i in range(n):
            self.pan_ver_comb.component("arrowbutton").bind(self.pan_ver_comb_binds[i],self.pan_ver_comb_binds_funcs[i])
        self.pan_ver_comb.component("entry").configure(state="normal")
        self.pan_ver_scl.configure(state="normal")
        n = len(self.pan_hor_comb_binds)
        for i in range(n):
            self.pan_hor_comb.component("arrowbutton").bind(self.pan_hor_comb_binds[i],self.pan_hor_comb_binds_funcs[i])
        self.pan_hor_comb.component("entry").configure(state="normal")
        self.pan_hor_scl.configure(state="normal")
#----------------------------------------------------------------------------------------
#----------------------------------------------------------------------------------------
#
# Create the animation file controls                
#
#----------------------------------------------------------------------------------------
#----------------------------------------------------------------------------------------
def create_animation_file_controls( self, parent, gui_parent, animation ):
        framea = Tkinter.Frame(parent)

        group = Pmw.Group(framea, tag_text = 'Animation File Controls')
        group.pack(fill = 'both', ipadx = 10, ipady=10)

        frameb = Tkinter.Frame(  group.interior() )
        text_save = Tkinter.StringVar()
        lab_save = Tkinter.Label( frameb, textvariable=text_save )
        text_save.set("Save: " )
        lab_save.pack( side='left', padx=2 )
        # Create file save icon
        self.animate_save = Tkinter.Canvas(frameb, bd=0, highlightthickness=0, width = 32, height = 32)
        self.animate_save.pack( side='left', fill=Tkinter.BOTH, expand='no', padx=5 )
        gui_support.balloon.bind( self.animate_save, "Save images to a file. Enter the file name by typing in the\nname in the entry window or by selecting the file icon to the left." )
        self.img_save = Tkinter.PhotoImage( file = os.path.join(cdms2.__path__[0],'..','..','..','..', 'bin', 'animate_save.gif') )
        self.animate_save.create_image(0,0, anchor=Tkinter.NW, image=self.img_save )
        self.animate_save.bind( '<1>', A_Command( evt_enter_save_file, self, parent, animation, 1 ))

        # Create the save file  entry field
        self.eny1=Pmw.EntryField( frameb,
            entry_background = 'white',
            entry_foreground = 'black',
            entry_width =  48,
            )
        self.eny1.pack( side='left', expand = 1, fill = 'both', padx=10, pady=5 )
        gui_support.balloon.bind(self.eny1, "Save images to a file. Enter the file name by typing in the\nname in the entry window or by selecting the file icon to the left. ")
        self.eny1.component('entry').bind( "<Key>", A_Command(evt_change_save_color, self, parent) )
        self.eny1.component('entry').bind( "<Return>", A_Command(evt_enter_save_file, self, parent, animation, 2) )
        self.animate_type = Pmw.RadioSelect(frameb,buttontype='radiobutton',command=A_Command(evt_select_anim_output_type,self,parent))
        self.animate_type.pack(side='left')
        for t in ['ras','mp4']:
            self.animate_type.add(t)
        self.animate_type.setvalue("ras")
        frameb.pack( side='top', fill='x', expand=0 )

        framec = Tkinter.Frame( group.interior() )
        text_load = Tkinter.StringVar()
        lab_load = Tkinter.Label( framec, textvariable=text_load )
        text_load.set("Load: " )
        lab_load.pack( side='left', padx=2 )
        # Create file load icon
        self.animate_load = Tkinter.Canvas(framec, bd=0, highlightthickness=0, width = 32, height = 32)
        self.animate_load.pack( side='left', fill=Tkinter.BOTH, expand='no', padx=5 )
        gui_support.balloon.bind( self.animate_load, "Load images into memory. Enter the file name by typing in the\nname in the entry window or by selecting the file icon to the left." )
        self.img_load = Tkinter.PhotoImage( file = os.path.join(cdms2.__path__[0],'..','..','..','..', 'bin', 'animate_load.gif') )
        self.animate_load.create_image(0,0, anchor=Tkinter.NW, image=self.img_load )
        self.animate_load.bind( '<1>', A_Command( evt_enter_load_file, self, parent, gui_parent, animation, 1 ))
        # Create the load file entry field
        self.eny2=Pmw.EntryField( framec,
            entry_background = 'white',
            entry_foreground = 'black',
            entry_width =  48,
            )
        self.eny2.pack( side='left', expand = 1, fill = 'both', padx=10, pady=5 )
        gui_support.balloon.bind( self.eny2, "Load images into memory. Enter the file name by typing in the\nname in the entry window or by selecting the file icon to the left. " )
        self.eny2.component('entry').bind( "<Key>", A_Command(evt_change_load_color, self, parent) )
        self.eny2.component('entry').bind( "<Return>", A_Command(evt_enter_load_file, self, parent, gui_parent, animation, 2) )
        framec.pack( side='top', fill='x', expand=0 )

        framea.pack(side='top', fill='both', expand = 1, padx=10, pady=10)

imagefiletypes = [ ("Raster files", "*.ras"), ("All files", "*"), ]

def evt_change_save_color( self, parent, event ):
         keycolor = Pmw.Color.changebrightness(parent, 'red', 0.85)
         self.eny1.configure( entry_background = keycolor )
 
def evt_enter_save_file( self, parent, animation, who_called, event ):
         # If creating or running an animation, then exit
         if animation.vcs_legacy_self.animate.creating_animation_flg() == 1: return
         if animation.vcs_legacy_self.animate.run_animation_flg() == 1: return

         # change backgound color to white
         self.eny1.configure( entry_background = 'white' )

         # Get the save file name
         if who_called == 1:
            os.chdir( os.getcwd( ) )
            sfile = tkFileDialog.asksaveasfilename( master=parent, filetypes = imagefiletypes+[('MP4','*.mp4'),],
                                                    title = 'Save Animation to Raster/MP4 File' )
         else:
            sfile = self.eny1.get()

         if (len(sfile) == 0): return

         if sfile.split('.')[-1].lower() != self.animate_type.getvalue():
             for i in range(self.animate_type.numbuttons()):
                 ext = self.animate_type.button(i).config()['text'][4]
                 if sfile.split('.')[-1].lower() == ext:
                     self.animate_type.invoke(i)
         if sfile.split('.')[-1].lower() != self.animate_type.getvalue():
             sfile += '.'+self.animate_type.getvalue()
         if string.find( sfile, '/' ) == -1:
            directory = os.getcwd()
            if os.access(directory, os.W_OK) == 1:
               sfile = directory + '/' + sfile
            else:
               showerror( "Error Message to the User", "The specfied directory does not have write permission or does not exist. Please check the directory permissions.")
               self.eny1.setentry('')
               return
         else:
           directory = sfile[:string.rindex( sfile, '/' )]
           if os.access(directory, os.W_OK) == 1:
              pass
           else:
              showerror( "Error Message to the User", "The specfied directory does not have write permission or does not exist. Please check the directory permissions.")
              self.eny1.setentry('')
              return
         self.eny1.setentry( sfile )

def evt_change_load_color( self, parent, event ):
         keycolor = Pmw.Color.changebrightness(parent, 'red', 0.85)
         self.eny2.configure( entry_background = keycolor )

def evt_enter_load_file( self, parent, gui_parent, animation, who_called, event ):
         # If creating or running an animation, then exit
         if animation.vcs_legacy_self.animate.creating_animation_flg() == 1: return
         if animation.vcs_legacy_self.animate.run_animation_flg() == 1: return

         # change backgound color to white
         self.eny2.configure( entry_background = 'white' )

         # Get the load file name
         if who_called == 1:
            dialog_icon = tkFileDialog.Open(master=parent,
                           title = 'Load Animation from Raster File',
                           filetypes=imagefiletypes )
            lfile=dialog_icon.show(initialdir=os.getcwd())
         else:
            lfile = self.eny2.get()
            if (len(lfile) == 0): return
            if string.find( lfile, '/' ) == -1:
               lfile = os.getcwd() + '/' + lfile


         if lfile[-4:] != '.ras': lfile += '.ras'
         if os.access(lfile, os.R_OK) == 0:
                showerror( "Error Message to the User", "The specfied file does not have read permission or does not exist. Please check the availability of the file.")
                self.eny2.setentry('')
                return
         self.eny2.setentry( lfile )

         # Load the animation into memory
         animation.vcs_legacy_self.animate.load_from_file( parent=gui_parent, load_file=lfile )

         # Re-initialize the Pan and Zoom settings
         self.zoom_scl.set( 1 )
         self.pan_hor_scl.set( 0 )
         self.pan_ver_scl.set( 0 )

#----------------------------------------------------------------------------------------
#----------------------------------------------------------------------------------------
#
# Create the animation control buttons                
#
#----------------------------------------------------------------------------------------
#----------------------------------------------------------------------------------------
def create_animation_control_buttons( self, parent, gui_parent, animation ):

        framea = Tkinter.Frame(parent)

        group = Pmw.Group(framea, tag_text = 'Animation Control Buttons')
        group.pack(fill = 'both', ipady=10)

        self.start_lbl = Pmw.LabeledWidget(group.interior(),
		    labelpos = 'n',
		    label_text = 'Create animation frames:'
                    )
        self.start_lbl.component('hull').configure(relief='sunken', borderwidth=2)
	self.start_btn = Tkinter.Button(self.start_lbl.interior(), 
                         text='Create\nImages',
                         background='green',
                         #command = A_Command(animation.vcs_legacy_self.animate.create)
                         command = A_Command(create_control_images, self, parent, gui_parent, animation )
                        )
	self.start_btn.pack(padx=10, pady=10, expand='yes', fill='both')
	self.start_lbl.pack(side='left', padx = 10)

        self.runanim_lbl = Pmw.LabeledWidget(group.interior(),
		    labelpos = 'n',
		    label_text = 'Loop through images:'
                    )
        self.runanim_lbl.component('hull').configure(relief='sunken', borderwidth=2)
	self.runanim_btn = Tkinter.Button(self.runanim_lbl.interior(),
                    text='Run\nAnimation',
                    background='cyan',
                    command = A_Command(animation.vcs_legacy_self.animate.run)
                    )
	self.runanim_btn.pack(padx=10, pady=10, expand='yes', fill='both')
	self.runanim_lbl.pack(side='left', padx = 10)

        self.run_lbl = Pmw.LabeledWidget(group.interior(),
		    labelpos = 'n',
		    label_text = 'Stop animation loop:'
                    )
        self.run_lbl.component('hull').configure(relief='sunken', borderwidth=2)
	self.run_btn = Tkinter.Button(self.run_lbl.interior(),
                    text='Stop Creation\n Stop Animation',
                    background='orange',
                    command = A_Command(evt_stop_animate, animation.vcs_legacy_self)
                    #command = A_Command(animation.vcs_legacy_self.animate.stop)
                    )
	self.run_btn.pack(padx=10, pady=10, expand='yes', fill='both')
	self.run_lbl.pack(side='left', padx = 10)

        framea.pack(side='top', fill='both', expand = 1, padx=10, pady=10)


#####################################################################################
# Create the images on the VCS Canvas for play back                                 #
#####################################################################################
def evt_stop_animate(canvas):
   canvas.animate.stop()
   if canvas._canvas_gui is not None: canvas.animate.stop()

def create_control_images( self, parent, gui_parent, animation ):
        # Get the save file text
        save_file = self.eny1.get()
        if (save_file != ''):
           self.eny1.configure( entry_background = 'white' )
           if save_file.split('.')[-1].lower() != self.animate_type.getvalue(): save_file += '.'+self.animate_type.getvalue()
           try:
              directory = save_file[:string.rindex( save_file, '/' )]
           except:
              showerror( "Error Message to the User", "The specfied directory does not have write permission or does not exist. Please check the directory permissions.")
              self.eny1.setentry('')
              return
           if os.access(directory, os.W_OK) == 1:
              pass
           else:
              showerror( "Error Message to the User", "The specfied directory does not have write permission or does not exist. Please check the directory permissions.")
              self.eny1.setentry('')
              return
           self.eny1.setentry(save_file)
           # figures out how many images per seconds
           rate = self.cntrl_slw_int.get()
           if int(rate)==0:
               rate=5.
           else:
               rate = 10./float(rate)
        else:
           save_file = None
           rate=1


        self.cntrl_nxt_int.setentry('1')
        animation.vcs_legacy_self.animate.create( parent=gui_parent, min=self.min, max=self.max, save_file=save_file, rate=rate )
        if self.animate_type.getvalue() == 'mp4':
            print 'got mp4 animation'
            self.execute(gui_parent,None)

#----------------------------------------------------------------------------------------
#----------------------------------------------------------------------------------------
#
# Create the Animation "Control animation frames" section
#
#----------------------------------------------------------------------------------------
#----------------------------------------------------------------------------------------
def create_control_animation_frame( self, parent, animation ):
        framef = Tkinter.Frame(parent)
        group = Pmw.Group(framef, tag_text = 'Control Animation Frames')
        group.pack(fill = 'both', ipady=10)

        # Create the next frame slider
        frameg = Tkinter.Frame( group.interior() )
        canvas = Tkinter.Canvas(frameg, width = 10, height = 10)
        bgc = canvas.configure('background')
        canvas.configure(highlightbackground = bgc[4])
        canvas.create_arc(0, 0, 7,7, fill = 'cyan', start=0, extent=359, outline = 'cyan')
        canvas.pack( side='left' )
	self.cntrl_slw_int = Pmw.Counter(frameg,
		labelpos = 'w',
                entryfield_modifiedcommand = A_Command( evt_cntrl_slw_cnt, self ),
		label_text = 'Pause animation:',
		entry_width = 5,
		entryfield_value = 0,
	        entryfield_validate = {'validator' : 'integer',
			'min' : 0, 'max' : 100},
                )
        gui_support.balloon.bind( self.cntrl_slw_int, 'Pause the time (1/10 sec.) between each frame while animation is running.' )
        self.cntrl_slw_int.pack( side='left', fill='both', padx=10, pady=2 )
        self.pause_scl= Tkinter.Scale( frameg,
           orient           = 'horizontal',
           width            = 12,
           showvalue        = 0,
           length           = 240,
           from_            = 0,
           to               = 100,
           command          = A_Command( evt_cntrl_slw_scl, self, animation )
          )
        gui_support.balloon.bind( self.pause_scl, 'Pause the time  (1/10 sec.) between each frame while animation is running.' )
        self.pause_scl.pack( side='left' )
        frameg.pack( side='top', fill='both' )

        # Create the next frame slider
        frameh = Tkinter.Frame( group.interior() )
        canvas = Tkinter.Canvas(frameh, width = 10, height = 10)
        bgc = canvas.configure('background')
        canvas.configure(highlightbackground = bgc[4])
        canvas.create_arc(0, 0, 7,7, fill = 'orange', start=0, extent=359, outline = 'orange')
        canvas.pack( side='left' )
        self.cntrl_nxt_int = Pmw.Counter(frameh,
                labelpos = 'w',
                entryfield_modifiedcommand = A_Command( evt_cntrl_nxt_cnt, self ),
                label_text = 'Next frame:',
                entry_width = 5,
                entryfield_value = 1,
                entryfield_validate = {'validator' : 'integer',
                        'min' : 1, 'max' : 100}
                )
        gui_support.balloon.bind( self.cntrl_nxt_int, 'Stop animation first, then step through the animation frames.' )
        self.cntrl_nxt_int.pack( side='left', fill='both', padx=10, pady=2 )
        self.next_scl= Tkinter.Scale( frameh,
           orient           = 'horizontal',
           width            = 12,
           showvalue        = 0,
           length           = 240,
           from_            = 1,
           to               = 100,
           command          = A_Command( evt_cntrl_nxt_scl, self, animation )
          )
        self.next_scl.max = 100
        gui_support.balloon.bind( self.next_scl, 'Stop animation first, then step through the animation frames.' )
        self.next_scl.pack( side='left' )
        frameh.pack( side='top', fill='both' )

        # Line up zoom and pan text
        Pmw.alignlabels((self.cntrl_slw_int, self.cntrl_nxt_int))

        framef.pack( side='top', fill='both', padx=10, pady=10 )

#####################################################################################
# Call the Scale widget event function below to pause the speed of the animation    #
#####################################################################################
def evt_cntrl_slw_cnt( self ):
      try:
         self.pause_scl.set( string.atoi( self.cntrl_slw_int.get()) )
      except:
         pass

#####################################################################################
# Slow the animate speed                                                            #
#####################################################################################
def evt_cntrl_slw_scl( self, animation, event ):
      animation.vcs_legacy_self.animate.pause( string.atoi(event) )
      self.cntrl_slw_int.setentry( event )

#####################################################################################
# Call the Scale widget event function below to show the appropriate frame          #
#####################################################################################
def evt_cntrl_nxt_cnt( self ):
      try:
         self.next_scl.set( string.atoi( self.cntrl_nxt_int.get()) )
      except:
         pass

#####################################################################################
# Show the appropriate frame requested by the user                                  #
#####################################################################################
def evt_cntrl_nxt_scl( self, animation, event ):
      num_frames = animation.vcs_legacy_self.animate.number_of_frames() # Get the number of frames

      # Reset the max values of the "Next frame" Counter and Scale widgets
      if ((num_frames is not None) and (self.next_scl.max != num_frames)):
         self.next_scl.configure(to = num_frames)
         self.next_scl.max = num_frames
         self.cntrl_nxt_int.configure(entryfield_validate = {'validator' : 'integer',
                        'min' : 1, 'max' : num_frames})

      # Reset to the requested frame and indicate the visiable frame number
      animation.vcs_legacy_self.animate.frame( string.atoi(event) )
      self.cntrl_nxt_int.setentry( event )

#----------------------------------------------------------------------------------------
#----------------------------------------------------------------------------------------
#
# Create the Animation "Zoom and pan animation frames" section
#
#----------------------------------------------------------------------------------------
#----------------------------------------------------------------------------------------
def create_zoom_and_pan_animation_frame( self, parent, animation ):
        #-------------------------------------------
        # View zoom and pan frame controls -- 'Zoom and Pan Controls'
        #-------------------------------------------
        one_to_20 = []
        for i in range( 1, 21 ): one_to_20.append( i )
        minus_100_to_100 = []
        for i in range( -100, 101 ): minus_100_to_100.append( i )

        frameb = Tkinter.Frame(parent)
        group = Pmw.Group(frameb, tag_text = 'Zoom and Pan Animation Frames')
        group.pack(fill = 'both', ipady=10)

        # Create the zoom slider
        framec = Tkinter.Frame( group.interior() )
        canvas = Tkinter.Canvas(framec, width = 10, height = 10)
        bgc = canvas.configure('background')
        canvas.configure(highlightbackground = bgc[4])
        canvas.create_arc(0, 0, 7,7, fill = 'cyan', start=0, extent=359, outline = 'cyan')
        canvas.pack( side='left' )
        canvas = Tkinter.Canvas(framec, width = 10, height = 10)
        canvas.configure(highlightbackground = bgc[4])
        canvas.create_arc(0, 0, 7,7, fill = 'orange', start=0, extent=359, outline = 'orange')
        canvas.pack( side='left' )
        self.zoom_comb = Pmw.ComboBox( framec,
           scrolledlist_items = one_to_20,
           labelpos = 'w',
           label_text = 'Zoom:',
           entry_width = 7,
           entry_foreground = 'black',
           selectioncommand = A_Command( evt_zoom_image_cmb, self )
           )
        gui_support.balloon.bind( self.zoom_comb, 'Zoom in on the animation image(s).' )
        self.zoom_comb.pack( side='left', fill='both', padx=10, pady=2 )
        self.zoom_scl= Tkinter.Scale( framec,
           orient           = 'horizontal',
           width            = 12,
           showvalue        = 0,
           length           = 240,
           from_            = 1,
           to               = 20,
           command          = A_Command( evt_zoom_image_scl, self, animation )
          )
        gui_support.balloon.bind( self.zoom_scl, 'Zoom in on the animation image(s).' )
        self.zoom_scl.pack( side='left' )
        framec.pack( side='top', fill='both' )

        # Create the pan x slider
        framed = Tkinter.Frame( group.interior() )
        canvas = Tkinter.Canvas(framed, width = 10, height = 10)
        canvas.create_arc(0, 0, 7,7, fill = 'cyan', start=0, extent=359, outline = 'cyan')
        canvas.pack( side='left' )
        canvas.configure(highlightbackground = bgc[4])
        canvas = Tkinter.Canvas(framed, width = 10, height = 10)
        canvas.configure(highlightbackground = bgc[4])
        canvas.create_arc(0, 0, 7,7, fill = 'orange', start=0, extent=359, outline = 'orange')
        canvas.pack( side='left' )
        self.pan_hor_comb = Pmw.ComboBox( framed,
           scrolledlist_items = minus_100_to_100, 
           labelpos = 'w',
           label_text = 'Horizontal pan:',
           entry_width = 7,
           entry_foreground = 'black',
           selectioncommand = A_Command( evt_change_hor_cmb, self )
           )
        gui_support.balloon.bind( self.pan_hor_comb, 'Pan the zoomed image(s) in the x direction..' )
        self.pan_hor_comb.pack( side='left', fill='both', padx=10, pady=2 )
        self.pan_hor_scl= Tkinter.Scale( framed,
           orient           = 'horizontal',
           width            = 12,
           showvalue        = 0,
           length           = 240,
           from_            = -100,
           to               = 100,
           command          = A_Command( evt_change_hor_scl, self, animation )
          )
        gui_support.balloon.bind( self.pan_hor_scl, 'Pan the zoomed image(s) in the x direction.' )
        self.pan_hor_scl.pack( side='left' )
        framed.pack( side='top', fill='both' )

        # Create the pan y slider
        framee = Tkinter.Frame( group.interior())
        canvas = Tkinter.Canvas(framee, width = 10, height = 10)
        canvas.configure(highlightbackground = bgc[4])
        canvas.create_arc(0, 0, 7,7, fill = 'cyan', start=0, extent=359, outline = 'cyan')
        canvas.pack( side='left' )
        canvas = Tkinter.Canvas(framee, width = 10, height = 10)
        canvas.configure(highlightbackground = bgc[4])
        canvas.create_arc(0, 0, 7,7, fill = 'orange', start=0, extent=359, outline = 'orange')
        canvas.pack( side='left' )
        self.pan_ver_comb = Pmw.ComboBox( framee,
           scrolledlist_items = minus_100_to_100, 
           labelpos = 'w',
           label_text = 'Vertical pan:',
           entry_width = 7,
           entry_foreground = 'black',
           selectioncommand = A_Command( evt_change_ver_cmb, self )
           )
        gui_support.balloon.bind( self.pan_ver_comb, 'Pan the zoomed image(s) in the y direction.' )
        self.pan_ver_comb.pack( side='left', fill='both', padx=10, pady=2 )
        self.pan_ver_scl= Tkinter.Scale( framee,
           orient           = 'horizontal',
           width            = 12,
           showvalue        = 0,
           length           = 240,
           from_            = -100,
           to               = 100,
           command          = A_Command( evt_change_ver_scl, self, animation )
          )
        gui_support.balloon.bind( self.pan_ver_scl, 'Pan the zoomed image(s) in the y direction.' )
        self.pan_ver_scl.pack( side='left' )
        framee.pack( side='top', fill='both' )

        # Line up zoom and pan text
        Pmw.alignlabels((self.zoom_comb, self.pan_hor_comb, self.pan_ver_comb))

        frameb.pack( side='top', fill='both', padx=10, pady=10 )

#####################################################################################
# Call the Zoom widget event function below to zoom in on the animation             #
#####################################################################################
def evt_zoom_image_cmb( self, event ):
      self.zoom_scl.set( string.atoi( event ) )

#####################################################################################
# Zoom in on the animation frame(s)                                                 #
#####################################################################################
def evt_zoom_image_scl( self, animation, event ):
      animation.vcs_legacy_self.animate.zoom( string.atoi(event) )
      self.zoom_comb.setentry( event )

#####################################################################################
# Call horizontal widget event function below to pan animation in the x direction   #
#####################################################################################
def evt_change_hor_cmb( self, event ):
      self.pan_hor_scl.set( string.atoi( event ) )

#####################################################################################
# Pan image frames in the x direction                                               #
#####################################################################################
def evt_change_hor_scl( self, animation, event ):
      animation.vcs_legacy_self.animate.horizontal( string.atoi(event) )
      self.pan_hor_comb.setentry( event )

#####################################################################################
# Call vertical widget event function below to pan animation in the y direction     #
#####################################################################################
def evt_change_ver_cmb( self, event ):
      self.pan_ver_scl.set( string.atoi( event ) )

#####################################################################################
# Pan image frames in the y direction                                               #
#####################################################################################
def evt_change_ver_scl( self, animation, event ):
      animation.vcs_legacy_self.animate.vertical( string.atoi(event) )
      self.pan_ver_comb.setentry( event )

###########################################################################################
###########################################################################################
##                                                                                       ##
##                                                                                       ##
## Start the classes and functions necessary to operate the VCS colormap editor.         ##
##                                                                                       ##
##                                                                                       ##
###########################################################################################
###########################################################################################

#---------------------------------------------------------------------------------
#---------------------------------------------------------------------------------
#
# Event handling function that will allow the passing of arguments
#
#---------------------------------------------------------------------------------
#---------------------------------------------------------------------------------
class A_Command:
   def __init__(self, func, *args, **kw):
      self.func = func
      self.args = args
      self.kw = kw

   def __call__(self, *args, **kw):
      args = self.args + args
      kw.update(self.kw)
      return apply(self.func, args, kw)

#####################################################################################
# Create/Popup animation GUI editor for VCS                                         #
#####################################################################################
def create( animation_obj, parent=None, transient=0):
    # Create the animation GUI
    a = AnimationGui(animation_obj, parent, transient)
    return a
        

#---------------------------------------------------------------------
# End of File
#---------------------------------------------------------------------
