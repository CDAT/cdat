#!/usr/bin/env python
#
# The PCMDI Data Browser Graphics Control Panel -  gui_graphics_control module
#
###############################################################################
#                                                                             #
# Module:       gui_graphics_control module                                   #
#                                                                             #
# Copyright:    "See file Legal.htm for copyright information."               #
#                                                                             #
# Authors:      PCMDI Software Team                                           #
#               Lawrence Livermore NationalLaboratory:                        #
#               support@pcmdi.llnl.gov                                        #
#                                                                             #
# Description:  PCMDI Software System browser Tkinter "Graphics Control"      #
#               panel  GUI.                                                   #
#                                                                             #
# Version:      4.0                                                           #
#                                                                             #
###############################################################################
#
#---------------------------------------------------------------------
# NOTE: need to use version of Python that imports Tkinter and Pmw
#---------------------------------------------------------------------
import Tkinter, Pmw, vcs
import os, sys, string, __main__
import gui_control
import gui_support
from gui_support import gui_color
import vcs_function
import gui_formulate
import gui_functions
import gui_annotate
import gui_canvas_geometry
import gui_set_min_max_scale
import gui_set_graphics_methods
import gui_alter_plot
import gui_message
import gui_output
import gui_defined_variables
import gui_busy

# Get the previously saved state of the GUI
try:
   fn = '%s/PCMDI_GRAPHICS' % os.environ['HOME']
   sys.path.append(fn)
   import vcdat_initial
except:
   pass

#---------------------------------------------------------------------------
#
# Start of the "Graphics Control" panel GUI Layout
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
# Begin the creation of "Graphics Control" panel
#---------------------------------------------------------------------
class create:
   def __init__( self, parent ):
      # Set the name of the top level menu
      O_Name = 'Options' # This is the name of the pulldown menu that the user sees
      if ((sys.platform == 'darwin') and (gui_control.do_aqua == 1)): O_Name = 'Options   '
      self.O_Name = O_Name
      #-------------------------------------------
      # create 2nd sub-panel
      #-------------------------------------------
      FRAME = Tkinter.Frame( parent.pane.pane( 'panelGC' ) )
      FRAME.pack( side='top', fill='both' )

      # Set the tear off value
      tear_it = 1
      if ((sys.platform == 'darwin') and (gui_control.do_aqua == 1)): tear_it = 0

      #-------------------------------------------
      # Set Plot Button
      #-------------------------------------------
      if ((sys.platform == 'darwin') and (gui_control.do_aqua == 1)):
         framec = Tkinter.Frame( FRAME, borderwidth=4, background = gui_color.seven, relief = 'raised' )
      else:
         framec = Tkinter.Frame( FRAME, borderwidth=4, relief = 'raised' )
#      if ((sys.platform == 'darwin') and (gui_control.do_aqua == 1)): framec = Tkinter.Frame( FRAME )
      framec.pack( side='left', fill='both' )
      btn1 = Tkinter.Button( framec, 
                                    text='Plot',
                                    background = gui_color.seven,
                                    command=gui_control.Command(self.evt_plot, parent) )
      if ((sys.platform == 'darwin') and (gui_control.do_aqua == 1)):
           btn1.configure( highlightbackground = gui_color.seven )
      btn1.pack( side='left', fill='both', expand=1 )
      parent.balloon.bind( btn1, 'Plot (or Re-Plot) Variable: Selected or Defined' )

      #-------------------------------------------------------------
      # Set Graphics Method and Destination Option Menu Buttons
      #-------------------------------------------------------------
      framea = Tkinter.Frame( FRAME, borderwidth=4, relief = 'raised' )
#      if ((sys.platform == 'darwin') and (gui_control.do_aqua == 1)): framea = Tkinter.Frame( FRAME )
      framea.pack( side='left', fill='both' )

      self.text_opt2 = Tkinter.StringVar()
      self.text_opt2.set('Boxfill')
      self.opt2 = Pmw.OptionMenu( framea,
                 menubutton_width=10,
                 menubutton_textvariable = self.text_opt2,
                 items=gui_control.gmchlst,
                 command=gui_control.Command(self.evt_which_graphics_method,parent,O_Name))
      self.opt2.configure( menubutton_text=gui_control.gmchlst[0] )
      self.opt2.pack( side='left', fill='both', expand=1 )
      parent.balloon.bind( self.opt2, 'Select Graphics Method' )

      self.text_opt3 = Tkinter.StringVar()
      if parent.menu.vcs_canvas_gui_settings_flg == 0:
         self.text_opt3.set( gui_control.potchlst[0] )
         potchlst = gui_control.potchlst
      else:
         self.text_opt3.set( gui_control.potchlst2[0] )
         potchlst = gui_control.potchlst2
      self.opt3 = Pmw.OptionMenu( framea,
                 menubutton_width=16,
                 menubutton_textvariable = self.text_opt3,
                 items=potchlst,
                 command=gui_control.Command(self.evt_which_vcs_canvas,parent),
                 )
      self.opt3.configure( menubutton_text=gui_control.potchlst[0] )
      self.opt3.pack( side='left', fill='both', expand=1 )
      parent.balloon.bind( self.opt3, 'Select Plot Output Destination' )

      #-------------------------------------------
      # Set Options Menu
      #-------------------------------------------

      self.opt_btn = frameb = Tkinter.Frame( FRAME, borderwidth=4, relief = 'raised' )
#      if ((sys.platform == 'darwin') and (gui_control.do_aqua == 1)): frameb = Tkinter.Frame( FRAME )
      frameb.pack( side='left', fill='both' )
      h_relief = 'raised'
#      if ((sys.platform == 'darwin') and (gui_control.do_aqua == 1)): h_relief = 'flat'
      menuBar = Pmw.MenuBar(frameb,
		hull_relief = h_relief,
		hull_borderwidth = 2,
		balloon = parent.balloon)
      self.menuBar = menuBar
      menuBar.pack(fill = 'both', expand=1)
      menuBar.addmenu(O_Name, 'Set User Plot Preferences', tearoff = tear_it)
      # Initialize the "Continents" Flag
      try:
         self.cont_flg = vcdat_initial.set.cont_flg
      except:
         self.cont_flg = None
      #
      # Create the cascade "Continents" menu and its items
      menuBar.addcascademenu(O_Name, 'Continents',
                              'Continents Types',
                              label = 'Continents Types',
                              traverseSpec = 'z', tearoff = tear_it
                             )
##       self.cont_toggle_0 = Tkinter.IntVar()
##       if self.cont_flg == -1: self.cont_toggle_0.set( 1 )
##       else: self.cont_toggle_0.set( 0 )
##       menuBar.addmenuitem('Continents', 'checkbutton', 'Filled Continents',
##                        selectcolor=gui_color.one,
##                        label = 'Filled Fine Continents',
##                        variable = self.cont_toggle_0,
##                        command = gui_control.Command(self.evt_continents_toggle, parent, -1))
      self.cont_toggle_1 = Tkinter.IntVar()
      if self.cont_flg == None: self.cont_toggle_1.set( 1 )
      else: self.cont_toggle_1.set( 0 )
      menuBar.addmenuitem('Continents', 'checkbutton', 'Auto Continents',
                       selectcolor=gui_color.one,
                       label = 'Auto Continents',
                       variable = self.cont_toggle_1,
                       command = gui_control.Command(self.evt_continents_toggle, parent, None))
      self.cont_toggle_2 = Tkinter.IntVar()
      if self.cont_flg == 0: self.cont_toggle_2.set( 1 )
      else: self.cont_toggle_2.set( 0 )
      menuBar.addmenuitem('Continents', 'checkbutton', 'No Continents',
                       label = 'No Continents',
                       selectcolor=gui_color.one,
                       variable = self.cont_toggle_2,
                       command = gui_control.Command(self.evt_continents_toggle, parent, 0))
      self.cont_toggle_3 = Tkinter.IntVar()
      if self.cont_flg == 2: self.cont_toggle_3.set( 1 )
      else: self.cont_toggle_3.set( 0 )
      menuBar.addmenuitem('Continents', 'checkbutton', 'Coarse Continents',
                       label = 'Coarse Continents',
                       selectcolor=gui_color.one,
                       variable = self.cont_toggle_3,
                       command = gui_control.Command(self.evt_continents_toggle, parent, 2))
      self.cont_toggle_4 = Tkinter.IntVar()
      if self.cont_flg == 1: self.cont_toggle_4.set( 1 )
      else: self.cont_toggle_4.set( 0 )
      menuBar.addmenuitem('Continents', 'checkbutton', 'Fine Continents',
                       label = 'Fine Continents',
                       selectcolor=gui_color.one,
                       variable = self.cont_toggle_4,
                       command = gui_control.Command(self.evt_continents_toggle, parent, 1))
      self.cont_toggle_5 = Tkinter.IntVar()
      if self.cont_flg == 3: self.cont_toggle_5.set( 1 )
      else: self.cont_toggle_5.set( 0 )
      menuBar.addmenuitem('Continents', 'checkbutton', 'United States Continents',
                       label = 'United States Continents',
                       selectcolor=gui_color.one,
                       variable = self.cont_toggle_5,
                       command = gui_control.Command(self.evt_continents_toggle, parent, 3))
      self.cont_toggle_6 = Tkinter.IntVar()
      if self.cont_flg == 4: self.cont_toggle_6.set( 1 )
      else: self.cont_toggle_6.set( 0 )
      menuBar.addmenuitem('Continents', 'checkbutton', 'Political Boundary Continents',
                       label = 'Political Boundary Continents',
                       selectcolor=gui_color.one,
                       variable = self.cont_toggle_6,
                       command = gui_control.Command(self.evt_continents_toggle, parent, 4))
      self.cont_toggle_7 = Tkinter.IntVar()
      if self.cont_flg == 5: self.cont_toggle_7.set( 1 )
      else: self.cont_toggle_7.set( 0 )
      menuBar.addmenuitem('Continents', 'checkbutton', 'Rivers Continents',
                       label = 'Rivers Continents',
                       selectcolor=gui_color.one,
                       variable = self.cont_toggle_7,
                       command = gui_control.Command(self.evt_continents_toggle, parent, 5))
      self.cont_toggle_8 = Tkinter.IntVar()
      if self.cont_flg == 6: self.cont_toggle_8.set( 1 )
      else: self.cont_toggle_8.set( 0 )
      menuBar.addmenuitem('Continents', 'checkbutton', 'User1 Continents',
                       label = 'User1 Continents',
                       selectcolor=gui_color.one,
                       variable = self.cont_toggle_8,
                       command = gui_control.Command(self.evt_continents_toggle, parent, 6))
      self.cont_toggle_9 = Tkinter.IntVar()
      if self.cont_flg == 7: self.cont_toggle_9.set( 1 )
      else: self.cont_toggle_9.set( 0 )
      menuBar.addmenuitem('Continents', 'checkbutton', 'User2 Continents',
                       label = 'User2 Continents',
                       selectcolor=gui_color.one,
                       variable = self.cont_toggle_9,
                       command = gui_control.Command(self.evt_continents_toggle, parent, 7))
      self.cont_toggle_10 = Tkinter.IntVar()
      if self.cont_flg == 8: self.cont_toggle_10.set( 1 )
      else: self.cont_toggle_10.set( 0 )
      menuBar.addmenuitem('Continents', 'checkbutton', 'User3 Continents',
                       label = 'User3 Continents',
                       selectcolor=gui_color.one,
                       variable = self.cont_toggle_10,
                       command = gui_control.Command(self.evt_continents_toggle, parent, 8))
      self.cont_toggle_11 = Tkinter.IntVar()
      if self.cont_flg == 9: self.cont_toggle_11.set( 1 )
      else: self.cont_toggle_11.set( 0 )
      menuBar.addmenuitem('Continents', 'checkbutton', 'User4 Continents',
                       label = 'User4 Continents',
                       selectcolor=gui_color.one,
                       variable = self.cont_toggle_11,
                       command = gui_control.Command(self.evt_continents_toggle, parent, 9))
      self.cont_toggle_12 = Tkinter.IntVar()
      if self.cont_flg == 10: self.cont_toggle_12.set( 1 )
      else: self.cont_toggle_12.set( 0 )
      menuBar.addmenuitem('Continents', 'checkbutton', 'User5 Continents',
                       label = 'User5 Continents',
                       selectcolor=gui_color.one,
                       variable = self.cont_toggle_12,
                       command = gui_control.Command(self.evt_continents_toggle, parent, 10))
      self.cont_toggle_13 = Tkinter.IntVar()
      if self.cont_flg == 11: self.cont_toggle_13.set( 1 )
      else: self.cont_toggle_13.set( 0 )
      menuBar.addmenuitem('Continents', 'checkbutton', 'User6 Continents',
                       label = 'User6 Continents',
                       selectcolor=gui_color.one,
                       variable = self.cont_toggle_13,
                       command = gui_control.Command(self.evt_continents_toggle, parent, 11))
      #
      # Create the cascade "Page Orientation" menu and its items
      # Initialize the "Page Orientation" Flag
      try:
         self.page_orientation_flg = vcdat_initial.set.page_orientation_flg
      except:
         self.page_orientation_flg = 1

      menuBar.addcascademenu(O_Name, 'Orientation',
                              'Page Orientation',
                              label = 'Page Orientation',
                              traverseSpec = 'z', tearoff = tear_it
                             )
      self.page_landscape = Tkinter.IntVar()
      if (self.page_orientation_flg != 0): self.page_landscape.set( 1 )
      else: self.page_landscape.set( 0 )
      menuBar.addmenuitem('Orientation', 'checkbutton', 'Landscape',
                       selectcolor=gui_color.one,
                       label = 'Landscape',
                       variable = self.page_landscape,
                       command = gui_control.Command(self.evt_page_orientation, parent, "landscape")
                       )
      self.page_portrait = Tkinter.IntVar()
      if self.page_orientation_flg == 0: self.page_portrait.set( 1 )
      else: self.page_portrait.set( 0 )
      menuBar.addmenuitem('Orientation', 'checkbutton', 'Portrait',
                       label = 'Portrait',
                       selectcolor=gui_color.one,
                       variable = self.page_portrait,
                       command = gui_control.Command(self.evt_page_orientation, parent, "portrait")
                       )

      # Initialize the "Overlay" flag for 1D plots
      try:
         self.over_flg = vcdat_initial.set.over_flg
      except:
         self.over_flg = 0
      self.var_overlay = Tkinter.IntVar()
      # Initialise the checkbutton to 1:
      self.var_overlay.set(self.over_flg)
      menuBar.addmenuitem(O_Name, 'checkbutton', 'Toggle me on/off',
                label = 'Overlay',
                selectcolor=gui_color.one,
                variable = self.var_overlay,
                command = gui_control.Command(self.evt_overlay_toggle, parent, O_Name))

      # Initialize the "Isoline Labels" Flag
      try:
         self.isol_label_flg = vcdat_initial.set.isol_label_flg
      except:
         self.isol_label_flg = 1
      self.var_labels = Tkinter.IntVar()
      # Initialise the checkbutton to 1:
      self.var_labels.set(self.isol_label_flg)
      menuBar.addmenuitem(O_Name, 'checkbutton', 'Toggle me on/off',
                label = 'Isoline Labels',
                selectcolor=gui_color.one,
                variable = self.var_labels,
                command = gui_control.Command(self.evt_isoline_labels_toggle, parent))

      menuBar.addmenuitem(O_Name, 'command', 'Annotate the plot',
                label = 'Annotation...',
                command = gui_control.Command(gui_annotate.create, parent)
                )
      menuBar.addmenuitem(O_Name, 'command', 'Set VCS Canvas Geometry',
                label = 'Set VCS Canvas Geometry...',
                command = gui_control.Command(gui_canvas_geometry.create, parent)
                )
      menuBar.addmenuitem(O_Name, 'command', 'Set Min Max Values',
                label = 'Set Min Max Values...',
                command = gui_control.Command(gui_set_min_max_scale.create, parent)
                )
      menuBar.addmenuitem(O_Name, 'command', 'Set Graphics Method Attributes',
                label = 'Set Graphics Method Attributes...',
                command = gui_control.Command(gui_set_graphics_methods.note_book, parent)
                )
      #
      # Create the cascade "Number of Plots" menu and its items
      menuBar.addcascademenu(O_Name, 'Number of Plots', 'Number of Plots on Canvas',
                          label = "Number of Plots on VCS Canvas",
                          traverseSpec = 'z', tearoff = tear_it
                         )
      self.plot_toggle_1 = Tkinter.IntVar()
      self.plot_toggle_1.set( 1 )
      menuBar.addmenuitem('Number of Plots', 'checkbutton', 'Toggle, 1 Plot per Canvas',
                          label = '1 Plot',
                          selectcolor=gui_color.one,
                          variable = self.plot_toggle_1,
                          command = gui_control.Command(self.evt_number_of_plots_on_canvas, parent, 1, O_Name)
                         )
      self.plot_toggle_2 = Tkinter.IntVar()
      self.plot_toggle_2.set( 0 )
      menuBar.addmenuitem('Number of Plots', 'checkbutton', 'Toggle, 2 Plots per Canvas',
                          label = '2 Plots',
                          selectcolor=gui_color.one,
                          variable = self.plot_toggle_2,
                          command = gui_control.Command(self.evt_number_of_plots_on_canvas, parent, 2, O_Name)
                         )
      self.plot_toggle_3 = Tkinter.IntVar()
      self.plot_toggle_3.set( 0 )
      menuBar.addmenuitem('Number of Plots', 'checkbutton', 'Toggle, 3 Plots per Canvas',
                          label = '3 Plots',
                          selectcolor=gui_color.one,
                          variable = self.plot_toggle_3,
                          command = gui_control.Command(self.evt_number_of_plots_on_canvas, parent, 3, O_Name)
                         )
      self.plot_toggle_4 = Tkinter.IntVar()
      self.plot_toggle_4.set( 0 )
      menuBar.addmenuitem('Number of Plots', 'checkbutton', 'Toggle, 4 Plots per Canvas',
                          label = '4 Plots',
                          selectcolor=gui_color.one,
                          variable = self.plot_toggle_4,
                          command = gui_control.Command(self.evt_number_of_plots_on_canvas, parent, 4, O_Name)
                         )
      #
      # Create the cascade "Set Map Projection" menu and its items
      menuBar.addcascademenu(O_Name, 'Set Map Projection', 'Set Map Projection for Plot',
                          label = "Set Plot Map Projection",
                          traverseSpec = 'z', tearoff = tear_it
                         )
      self.projection_toggle=[]
      x=parent.vcs[parent.vcs_id]
      ii=0      
      for p in x.listelements('projection'):
         self.projection_toggle.append(Tkinter.IntVar())
         if p=='default':
            self.projection_toggle[-1].set( 1 )
         else:
            self.projection_toggle[-1].set( 0 )
      
         menuBar.addmenuitem('Set Map Projection', 'checkbutton', 'Toggle '+p,
                             label = p+' Projection',
                             selectcolor=gui_color.one,
                             variable = self.projection_toggle[-1],
                             command = gui_control.Command(self.evt_set_plot_projection, parent, ii, O_Name)
                             )
         ii=ii+1

#      menuBar.addcascademenu(O_Name, 'Editors', 'Editors',
#                          label = 'Editors',
#                          traverseSpec = 'z', tearoff = tear_it
#                         )
      menuBar.addmenuitem(O_Name, 'command', 'Alter Plot Editor',
                label = 'Alter Plot Editor...',
                command = gui_control.Command(gui_alter_plot.note_book, parent)
                )
      menuBar.addmenuitem(O_Name, 'command', 'Colormap Editor',
                label = 'Colormap Editor...',
                command = gui_control.Command(self.evt_colormap, parent)
                )
## Editors menu disappeared, projection editor accessible from graphic method editors
##       menuBar.addmenuitem(O_Name, 'command', 'Projection Editor',
##                 label = 'Projection Editor...',
##                 command = gui_control.Command(self.evt_projection, parent)
##                 )
# No need to show it...
#      menuBar.addmenuitem('Editors', 'command', 'Graphics Method Editor',
#                label = 'Graphics Method Editor',
#                command = gui_control.Command(self.evt_gmeditor, parent)
#                )
#      # the Graphics Method Editor is not that useful right now from here.
#      # it needs to have an 'open' menu item first. So disable it.
#      mi = menuBar.component('Options-menu')
#      mi.entryconfigure('Graphics Method Editor',state='disabled')

#      menuBar.addmenuitem('Editors', 'command', 'Template Editor',
#                label = 'Template Editor...',
#                command = gui_control.Command(self.evt_templateeditor, parent)
#                )
      menuBar.addmenuitem(O_Name, 'command', 'VCS Animation',
                label = 'Animate...',
                command = gui_control.Command(self.evt_animate, parent, O_Name)
                )
#      menuBar.addmenuitem(O_Name, 'command', 'Clear VCS Canvas',
#                label = 'Clear VCS Canvas',
#                command = gui_control.Command(self.evt_clear_display, parent)
#                )
#      menuBar.addmenuitem(O_Name, 'command', 'Close VCS Canvas',
#                label = 'Close VCS Canvas',
#                command = gui_control.Command(self.evt_close_canvas, parent)
#                )

      # Disable the animation selection if overlay is on
      if self.over_flg == 1:
         self.menuBar.component(O_Name + '-menu').entryconfigure(13, state = 'disabled')

      #-------------------------------------------------------
      # Set Separator Between Plot Options and Define
      #-------------------------------------------------------
      self.sep1 = sep1 = Tkinter.Frame ( FRAME, relief = 'sunken',
                    height = 55, width = 6, borderwidth = 0, background=gui_color.one)
      sep1.pack(side = 'left',padx=15 )

      #-------------------------------------------
      # Set the Define Button
      #-------------------------------------------
      if ((sys.platform == 'darwin') and (gui_control.do_aqua == 1)):
         self.define_btn = framed = Tkinter.Frame( FRAME, borderwidth=4, background = gui_color.six, relief = 'raised' )
      else:
         self.define_btn = framed = Tkinter.Frame( FRAME, borderwidth=4, relief = 'raised' )
#      if ((sys.platform == 'darwin') and (gui_control.do_aqua == 1)): framed = Tkinter.Frame( FRAME )
      framed.pack( side='left', fill='both', expand= 1, padx=0 )

      btn2 = Tkinter.Button( framed, 
                                    text='Define',
                                    background = gui_color.six,
                                    command=gui_control.Command(self.evt_define, parent) )
      if ((sys.platform == 'darwin') and (gui_control.do_aqua == 1)):
           btn2.configure( highlightbackground = gui_color.six )
      btn2.pack( fill='both', expand=1 )
      parent.balloon.bind( btn2, "Define Variable into Memory\n\nCan either select a variable from the 'Select Variable'\npanel above or from the 'Defined Variables' list\nwindow below.\n\nNote: The defined variable will be displayed in the\n'Defined Variable' list window below." )

#---------------------------------------------------------------------
# End Graphics Control Panel Layout
#---------------------------------------------------------------------

#
#
#------------------------------------------------------------------------------------------
# Class "Options" events for graphics control panel
#------------------------------------------------------------------------------------------
#
   def turn_off_all_plots( self, parent ):
       for i in parent.pl.form.keys():
         if parent.pl.form[i].lbl.cget( 'label_text' ) == 'On':
            try:
               if parent.pl.form[i].__dict__.has_key("display") and parent.pl.form[i].display is not None:
                  if isinstance(parent.pl.form[i].display,(tuple,list)):
                     displays = parent.pl.form[i].display
                  else:
                     displays = [parent.pl.form[i].display,]
               for d in displays:
                  try:
                     d.off = 1
                     parent.vcs[ parent.vcs_id ].remove_display_name( d.name )
                  except:
                     pass
            except Exception,err:
##                print err
               del parent.pl.form[i].display
            parent.pl.form[i].off_btn.create_image(0,0, anchor=Tkinter.NW, image=parent.pl.form[i].off_btnimage )
            parent.pl.form[i].lbl.configure( label_text = 'Off' )

   def which_plot_is_on( self, parent ):
       on = []
       for i in parent.pl.form.keys():
         if parent.pl.form[i].lbl.cget( 'label_text' ) == 'On':
            on.append( i )
       return on

   def turn_on_listed_plots( self, parent, on_list ):
       for i in on_list:
          try: 
             if parent.pl.form[i].__dict__.has_key("display") and parent.pl.form[i].display is not None:
               if isinstance(parent.pl.form[i].display,(tuple,list)):
                  displays = parent.pl.form[i].display
               else:
                  displays = [parent.pl.form[i].display,]
               for d in displays:
                  d.off = 0
          except:
             try: 
                parent.pl.do_plot( i )
                parent.pl.form[i].off_btn.create_image(0,0, anchor=Tkinter.NW, image=parent.pl.form[i].on_btnimage )
                parent.pl.form[i].lbl.configure( label_text = 'On' )
             except:
                pass



   def remove_variable_from_defined_variable_list( self, parent, remove_variable ):
       removeQ = []
       kys=[]                                #       window.
       for i in range(len(parent.panelDV.lst1.keys())):
           kys.append(string.split(parent.panelDV.lst1.keys()[i])[0])
           if kys[i] == remove_variable:
              removeQ.append( '-- ' + parent.panelDV.lst1.keys()[i] )
       if (remove_variable in kys):
          parent.panelDV.evt_remove_selected_defined( parent, removeQ, None )
  
       # Remove remove_variable from the Trashcan, so the user doesn't have to do it.
       trash_can_list, total_trash_size = parent.panelDV.return_trash_can_list( parent )
       removeT=[]                                #       trashcan.
       for x in trash_can_list:
           kys =  string.split(x,'_')[0]
           if kys == remove_variable:
              removeT.append( x )
       parent.panelDV.evt_dispose_execute( parent, tuple(removeT), 'Dispose\nSelected' )
 
   #
   ####### event to plot
   # This is called because there is a need for an intermediate step to check whether
   # an overlay is needed (i.e., FilledIsoline or BoxedIsoline). If an overlay is needed,
   # Then call "call_do_plot" twice; once for isofill or boxfill; and again for the isoline
   # overlay.
   #
   def evt_plot( self, parent ):
       # Stop the replotting of data when the template editor is in use.
       if (parent.vcs[ parent.vcs_id ].SCREEN_MODE() == "TEDITOR"):
          gui_message.error("You must cancel the 'Template Editor' GUI before re-plotting.")
          return

       if ((parent.vcs[ parent.vcs_id ].animate.gui_popup == 1) or (parent.vcs[ parent.vcs_id ].animate.create_flg == 1)):
          if parent.menu.vcs_canvas_gui_settings_flg == 1:
             gui_message.error('You must cancel "Animation" before re-plotting. \n\nSelect any of the other label tabs to exist "Animation" (e.g., "Command Line", "Primitives", etc.).')
          else:
             gui_message.error("You must cancel the 'Animation Editor' GUI before re-plotting.")
          return

       # Make sure something is selected from the file or the defined variable window
       gm_type = parent.panelGC.text_opt2.get()
       if gm_type not in ["Continents"]:
        if (parent.panelDM.var3 is None) and (len(parent.panelDV.selected) == 0):
           gui_message.error('Must first select a variable from the "Select Variable" panel above or from the "Defined Variables" list window below.')
           return

       # Block the X server from processing any X11 commands, then make sure that the 
       # VCS Canvas is in front of the VCDAT GUI before plotting.
       parent.vcs[ parent.vcs_id ].BLOCK_X_SERVER()
       parent.vcs[ parent.vcs_id ].canvasraised( )

       # Check for the isoline overlay, and reset the graphics method if needed
       overlay = 0
       if gm_type not in ["Yxvsx", "Xyvsy"]:
          if gm_type == 'FilledIsoline':
             gm_type = 'Isofill'
             overlay = 1
          elif gm_type == 'BoxedIsoline':
             gm_type = 'Boxfill'
             overlay = 1

##           print 'Plot #',parent.vcs_canvas_plot_number
          # Plot using the appropriate graphics method
          if parent.vcs_canvas_plot_number == 1:
             if parent.panelGC.over_flg == 0:
##                 print 'We are starting from here'
                variable_name, id = self.call_do_plot( parent, gm_type )
             else: # Do overlay of plots
                if parent.plot_ct[ parent.vcs_id ] == 0 :
                   variable_name, id = self.call_do_plot( parent, gm_type )
                   parent.plot_ct[ parent.vcs_id ] = 1
                else:
                   if parent.plot_ct[ parent.vcs_id ] != 0: parent.template_name = 'ASD_dud'
                   parent.template_skip = 1
                   variable_name, id = self.call_multiple_plot(parent, gm_type)
                   parent.plot_ct[ parent.vcs_id ] += 1
                   if parent.plot_ct[ parent.vcs_id ] != 0: parent.template_name = 'ASD'
                   parent.template_skip = 0
          else:
             # Clear the display and page form if first time
             # This will clear the Page Layout forms
             if parent.panelDV.selected != []: # selected from "Defined Variables"
                if (parent.plot_ct[ parent.vcs_id ] == parent.vcs_canvas_plot_number):
                    for i in parent.pl.form.keys():
                       parent.pl.evt_remove_form( i, parent, None )

             if (parent.plot_ct[ parent.vcs_id ] == parent.vcs_canvas_plot_number):  self.evt_clear_display( parent )
             # Set the appropriate multiple picture template
##              print 'vcs_id, Counts, #:',parent.vcs_id,parent.plot_ct[ parent.vcs_id ]+1,parent.vcs_canvas_plot_number
             parent.template_name = a="ASD%d_of_%d" % \
                                    ((parent.plot_ct[ parent.vcs_id ]+1), parent.vcs_canvas_plot_number)
             # Plot on multiple Canvas page
             variable_name, id = self.call_multiple_plot(parent, gm_type)
   
          # If an overlay is selected, then plot with the isoline graphics method
          if overlay == 1:
             variable_name, id = self.call_do_plot( parent, gm_type = 'Isoline', var_name = variable_name )
       else: # 1D Plots (i.e., "Yxvsx" and "Xyvsy") are handled a little differently
          if parent.panelGC.over_flg == 0:
             # Plot using the appropriate graphics method
             parent.XYname, id = self.call_do_plot( parent, gm_type )
             variable_name = parent.XYname
          else:
             if parent.XYct == 15:       # Re-initiate overlay process by clearing the display
                parent.XYct = 0
                self.evt_clear_display( parent )

             if parent.XYct == 0:        # First time plot using the appropriate graphics method 
                parent.XYname, id = self.call_do_plot( parent, gm_type )
                variable_name = parent.XYname
             else:                       # Overlay plots
                if parent.panelDV.selected == []:
                   XYname = "%s%d" % (parent.XYname,parent.XYct+1)
   
                   # Remove existing defined variable from the defined variable window
                   for i in range(len(parent.panelDV.lst1.keys())):
                       if (string.split(parent.panelDV.lst1.keys()[i])[0] == XYname):
                          self.remove_variable_from_defined_variable_list( parent, XYname)
                          break
                    
                   if (self.evt_define( parent, var_name = XYname ) == 0): return None, None
                   variable_name, id = self.call_do_plot( parent, gm_type, var_name=XYname )
                else:
                   variable_name, id = self.call_do_plot( parent, gm_type, var_name=string.split( parent.panelDV.selected, ' ' )[0], new_form=1 )

             # Make sure the Template and Graphics Method names displayed are the same
             parent.pl.form[ id ].gm.setentry( parent.pl.form[ id ].template.get() )

       # If the VCS Canvas is displayed, then update the backing store and unblock the 
       # X server.
       if (parent.vcs[ parent.vcs_id ].iscanvasdisplayed()):
          parent.vcs[ parent.vcs_id ].flush()
          parent.vcs[ parent.vcs_id ].backing_store()
          parent.vcs[ parent.vcs_id ].canvas.UNBLOCK_X_SERVER()
   #
   ####### call do plot
   # This function can be called twice. Once for the inital graphics method and second for an
   # overlay isoline.
   #
   def call_do_plot( self, parent, gm_type = "Boxfill", var_name = None, new_form = 0 ):
      # If var_name is None, then this is the first call to plot on the screen
      if var_name == None:
         # If the overlay toggle button is not selected or QuickPlot, then make sure the buttons 
         # are all turned off. This will clear the VCS Canvas
         if (parent.panelGC.over_flg != 1) or (parent.panelDV.selected == []):
           self.turn_off_all_plots ( parent )
         # if selected is [], then the user is doing a QuickPlot
         if parent.panelDV.selected == []:
            # QuickPlot cannot do Vector, Scatter, or XvsY graphics methods. These graphics
            # methods require two variables and QuickPlot only produces one.
            if (gm_type in ['Vector', 'Scatter', 'XvsY']):
               gui_message.error( "Vector, Scatter or XvsY plots must have two data variables. The data variables must be selected in the 'Defined Variables' window.")
               return None, None

            search_for = "QuickPlot"
            selecteds = [search_for,]
            # Remove search_for from the "Defined Variables" window
            # search_for is sent to the Trashcan were the user can restore it if they wish.
            self.remove_variable_from_defined_variable_list( parent, search_for)

            # Show search_for in the "Defined Variables" window
            if (self.evt_define( parent, var_name = search_for  ) == 0): return None, None
         else:   # Must be plotting a Defined Variable
            self.turn_off_all_plots ( parent )
            search_for = string.split( parent.panelDV.selected, ' ' )[0]
##             if gm_type== 'Meshfill' and len(parent.panelDV.selected_list)<2:
##                ## Trying to quickplot a mesh object
##                search_for = 'QuickPlot'

            if gm_type in [ "Vector", "Scatter", "XvsY"] or (gm_type=="Meshfill" and len(parent.panelDV.selected_list)>=2):
               for j in parent.panelDV.selected_list.keys():
                  if parent.panelDV.selected_list[ j ] == parent.panelDV.selected:
                     break
               try:
                  lst = parent.panelDV.selected_list.keys()
                  selected1 = string.split( parent.panelDV.selected_list[ lst[0] ], ' ' )[0]
                  selected2 = string.split( parent.panelDV.selected_list[ lst[1] ], ' ' )[0]
               except:
##                   if gm_type == 'Meshfill':
##                      try:
                        
##                         lst = parent.panelDV.selected_list.keys()
##                         selected1 = string.split( parent.panelDV.selected_list[ lst[0] ], ' ' )[0]
##                         print 'Done it all'
##                      except:
##                         print 'Error ?'
##                         raise
                  gui_message.error( "Vector, Scatter, Meshfill or XvsY plots must have two data variables. The data variables must be selected in the 'Defined Variables' window.-----------------")
                  return None, None
            elif gm_type.split()[0] == 'Ext':
                  lst = parent.panelDV.selected_list.keys()
                  selecteds = []
                  for j in range(len(lst)):
                     selecteds.append(string.split( parent.panelDV.selected_list[ lst[j] ], ' ' )[0])

      else: # This must be an overlay, so we already have the variable name
         search_for = var_name

      # Search for search_for in the Page Layout
      form_search_for = None
      found_search_for = 0
      empty_form_id = None
      i = 0  # if parent.pl.form is {}, then i is
      if var_name == None: # Must be the first time to plot      
         if gm_type in [ "Vector", "Scatter", "XvsY"] or (gm_type == "Meshfill" and len(parent.panelDV.selected_list)>=2):
            for i in parent.pl.form.keys():
              if parent.pl.form[ i ].data1.get() == selected1 and parent.pl.form[ i ].data2.get() == selected2:
                 form_search_for = parent.pl.form[ i ]
                 found_search_for = 1
                 break
              elif (parent.pl.form[ i ].data1.get() == "") and (parent.pl.form[ i ].data2.get() == "") and (empty_form_id == None):
                 empty_form_id = i
         elif gm_type.split()[0] == 'Ext':
            for i in parent.pl.form.keys():
               ok = True
               empty = 0
               for j in range(len(selecteds)):
                  if j==0:
                     if parent.pl.form[ i ].data1.get() != selecteds[j]:
                        ok = False
                     if parent.pl.form[ i ].data1.get() =="":
                        empty+=1
                  elif j==1:
                     if parent.pl.form[ i ].data2.get() != selecteds[j]:
                        ok = False
                     if parent.pl.form[ i ].data2.get() =="":
                        empty+=1
                  elif len(parent.pl.form[i].datas)>j:
                     if parent.pl.form[ i ].datas[j-2].get() != selecteds[j]:
                        ok = False
                     if parent.pl.form[ i ].datas[j-2].get() =="":
                        empty+=1
                  else:
                     ok = False
               if ok:
                  form_search_for = parent.pl.form[ i ]
                  found_search_for = 1
                  break
               elif empty == len(selecteds):
                  empty_form_id = i
         else:
            for i in parent.pl.form.keys():
              if parent.pl.form[ i ].data1.get() == search_for:
                 form_search_for = parent.pl.form[ i ]
                 found_search_for = 1
                 break
              elif (parent.pl.form[ i ].data1.get() == "") and (empty_form_id == None):
                 empty_form_id = i
      else:     # Must have requested an overlay
         ct = 0
         for i in parent.pl.form.keys():
             if parent.pl.form[ i ].data1.get() == search_for:
                ct += 1
                if ct == 2:
                  form_search_for = parent.pl.form[ i ]
                  found_search_for = 1
                  break
             elif (parent.pl.form[ i ].data1.get() == "") and (empty_form_id == None):
                empty_form_id = i

      # I now have my Page Layout Form ID
      id = i

      # Want a new Page layout form created. Needed for selected 1D plot overlays
      if new_form: found_search_for = 0

      # Put search_for in a Page Layout Form
      if (form_search_for == None) and (parent.pl.form == {}):
         parent.pl.evt_create_new_pl_form( parent )
         i = 1
      if ((len( parent.pl.form ) == 1) and ( parent.pl.form[ i ].data1.get() == '' )):
             form_search_for = parent.pl.form[ i ]
             id = i
      elif (found_search_for == 0) and (empty_form_id != None):
             form_search_for = parent.pl.form[ empty_form_id ]
             id = empty_form_id
      elif (found_search_for == 0) and (empty_form_id == None):
             id = parent.pl.evt_create_new_pl_form( parent )
             form_search_for = parent.pl.form[ id ]

      # Move the Page Layout Scroll Window to display the displayed form
      parent.pl.scl_frame.yview( 'moveto', float(id-1.0)/ len(parent.pl.form) )

      # Change the Form Line Graphics Method to the Plot's Graphics Method
      parent.pl.form[id].gm.configure( label_text = gm_type )
      parent.pl.form[id].gm.type = gm_type

      if gm_type in [ "Vector", "Scatter", "XvsY"] or (gm_type == "Meshfill" and len(parent.panelDV.selected_list)>=2):
         parent.pl.form[id].data1.configure( label_text = 'U(X,Y)' )
         parent.pl.form[id].data2.configure( label_text = 'V(X,Y)' )
         parent.pl.form[id].data2.pack( side='left', after= parent.pl.form[id].data1, padx = 5)
         form_search_for.data1.setentry( selected1 )
         form_search_for.data2.setentry( selected2 )
         #         if (parent.pl.form[id].data1.get() != "") and (parent.pl.form[id].data2.get() != ""):
      elif gm_type.split()[0] == 'Ext':
         parent.pl.form[id].data1.pack( side='left', after= parent.pl.form[id].gm, padx = 5)
         parent.pl.form[id].data1.configure( label_text = 'A' )
         form_search_for.data1.setentry( selecteds[0] )
         if len(selecteds)>1:
            parent.pl.form[id].data2.pack( side='left', after= parent.pl.form[id].data1, padx = 5)
            parent.pl.form[id].data2.configure( label_text = 'B' )
            form_search_for.data2.setentry( selecteds[1] )
         if len(selecteds)>2:
            for j in range(len(selecteds)-2):
               if j==0:
                  parent.pl.form[id].datas[j].pack( side='left', after= parent.pl.form[id].data2, padx = 5)
               else:
                  parent.pl.form[id].datas[j].pack( side='left', after= parent.pl.form[id].datas[j-1], padx = 5)
               parent.pl.form[id].datas[j].configure( label_text = '%s' % chr(67+j) )
               form_search_for.datas[j].setentry( selecteds[j+2] )
      else:
         parent.pl.form[id].data1.pack( side='left', after= parent.pl.form[id].gm, padx = 5)
         parent.pl.form[id].data1.configure( label_text = 'A(X,Y)' )
         if gm_type!='Meshfill' : parent.pl.form[id].data2.pack_forget( )
         form_search_for.data1.setentry( search_for )


      # Set the Template, Graphics Method, and Data entries
      # Note: 1D plots are special. The templates are set in the vcs_function.plot function.
      # This is because the user may wish to do 1D plot overlays, which are different than
      # overlays for other graphics methods.
#      if (parent.panelDV.selected != []) and (parent.panelGC.over_flg == 1) and (var_name == None) and (len(parent.d) != 0):
#          parent.template_name = 'ASD_dud'
#      else:
#          parent.template_name = 'ASD'

      # Change this again so that it will work with the Page Form Layout
  #    form_search_for.template.setentry( parent.template_name )
  #    form_search_for.gm.setentry( parent.graphics_method_name )
      # Crap for taylordiagrams
      if gm_type=='Taylordiagram':
         tname = form_search_for.template.get()
         if tname[:3]=='ASD':
            tname = 'deftaylor'+tname[3:]
         form_search_for.template.setentry( tname )
      else:
         form_search_for.template.setentry( form_search_for.template.get() )
      form_search_for.gm.setentry( form_search_for.gm.get() )

      # Set the Name of the Plot to Variable name in the "Select Variable" panel
      quick_plot_flag = 0
      if ( (search_for[:9] == "QuickPlot") and 
           ((parent.annotate_name is None) or (parent.annotate_name == "QuickPlot")) ):
         parent.annotate_name = parent.panelSV.tin4.get() + search_for[9:] # "Selected Panel" variable
         parent.QuickPlot_name[search_for] = parent.panelSV.tin4.get()
         quick_plot_flag = 1

      # Show Plot results
      parent.pl.do_plot( id )
      if quick_plot_flag == 1: parent.annotate_name = None # Reset the annotate_name

      # Need the name of the variable if an overlay is needed
      return search_for, id
   #
   ####### call multiple plot
   # This function can be called twice. Once for the inital graphics method and second for an
   # overlay isoline.
   #
   def call_multiple_plot( self, parent, gm_type = "Boxfill", var_name = None, new_form = 0 ):
      if var_name == None:
         # if selected is [], then the user is doing a QuickPlot
         if parent.panelDV.selected == []:
            # QuickPlot cannot do Vector, Scatter, or XvsY graphics methods. These graphics
            # methods require two variables and QuickPlot only produces one.
            if (gm_type in ['Vector', 'Scatter', 'XvsY']):
               gui_message.error( "Vector, Scatter or XvsY plots must have two data variables. The data variables must be selected in the 'Defined Variables' window.")
               return None, None

            search_for = "QuickPlot"
            # Remove search_for from the "Defined Variables" window
            # search_for is sent to the Trashcan were the user can restore it if they wish.
            if parent.plot_ct[ parent.vcs_id ] > 0:                  # get unique name
               search_for = "%s%d" % (search_for,(parent.plot_ct[ parent.vcs_id ]+1))
            self.remove_variable_from_defined_variable_list( parent, search_for)

            # Show search_for in the "Defined Variables" window
            if (self.evt_define( parent, var_name = search_for  ) == 0): return None, None
            selecteds=[search_for]
         else:   # Must be plotting a Defined Variable
#            self.turn_off_all_plots ( parent )
            search_for = string.split( parent.panelDV.selected, ' ' )[0]
##             if gm_type== 'Meshfill' and len(parent.panelDV.selected_list)<2:
##                ## Trying to quickplot a mesh object
##                search_for = 'QuickPlot'

            if gm_type in [ "Vector", "Scatter", "XvsY"] or (gm_type=="Meshfill" and len(parent.panelDV.selected_list)>=2):
               for j in parent.panelDV.selected_list.keys():
                  if parent.panelDV.selected_list[ j ] == parent.panelDV.selected:
                     break
               try:
                  lst = parent.panelDV.selected_list.keys()
                  selected1 = string.split( parent.panelDV.selected_list[ lst[0] ], ' ' )[0]
                  selected2 = string.split( parent.panelDV.selected_list[ lst[1] ], ' ' )[0]
               except:
##                   if gm_type == 'Meshfill':
##                      try:

##                         lst = parent.panelDV.selected_list.keys()
##                         selected1 = string.split( parent.panelDV.selected_list[ lst[0] ], ' ' )[0]
##                         print 'Done it all'
##                      except:
##                         print 'Error ?'
##                         raise
                  gui_message.error( "Vector, Scatter, Meshfill or XvsY plots must have two data variables. The data variables must be selected in the 'Defined Variables' window.-----------------")
                  return None, None
            elif gm_type.split()[0] == 'Ext':
               lst = parent.panelDV.selected_list.keys()
               selecteds = []
               for j in range(len(lst)):
                  selecteds.append(string.split( parent.panelDV.selected_list[ lst[j] ], ' ' )[0])

      else: # This must be an overlay, so we already have the variable name
         if gm_type.split()[0] == 'Ext':
            lst = parent.panelDV.selected_list.keys()
            selecteds = []
            for j in range(len(lst)):
               selecteds.append(string.split( parent.panelDV.selected_list[ lst[j] ], ' ' )[0])
         search_for = var_name

      # Create the page form in the "Page Layout" panel
      id = parent.pl.evt_create_new_pl_form( parent )
      form_search_for = parent.pl.form[ id ]

      # Change the Form Line Graphics Method to the Plot's Graphics Method
      parent.pl.form[id].gm.configure( label_text = gm_type )
      parent.pl.form[id].gm.type = gm_type

      if gm_type in [ "Vector", "Scatter", "XvsY"] or (gm_type == "Meshfill" and len(parent.panelDV.selected_list)>=2):
         parent.pl.form[id].data1.configure( label_text = 'U(X,Y)' )
         parent.pl.form[id].data2.configure( label_text = 'V(X,Y)' )
         parent.pl.form[id].data2.pack( side='left', after= parent.pl.form[id].data1, padx = 5)
         form_search_for.data1.setentry( selected1 )
         form_search_for.data2.setentry( selected2 )
      elif gm_type.split()[0] == 'Ext':
         parent.pl.form[id].data1.pack( side='left', after= parent.pl.form[id].gm, padx = 5)
         parent.pl.form[id].data1.configure( label_text = 'A' )
         form_search_for.data1.setentry( selecteds[0] )
         if len(selecteds)>1:
            parent.pl.form[id].data2.pack( side='left', after= parent.pl.form[id].data1, padx = 5)
            parent.pl.form[id].data2.configure( label_text = 'B' )
            form_search_for.data2.setentry( selecteds[1] )
         if len(selecteds)>2:
            for j in range(len(selecteds)-2):
               if j==0:
                  parent.pl.form[id].datas[j].pack( side='left', after= parent.pl.form[id].data2, padx = 5)
               else:
                  parent.pl.form[id].datas[j].pack( side='left', after= parent.pl.form[id].datas[j-1], padx = 5)
               parent.pl.form[id].datas[j].configure( label_text = '%s' % chr(67+j) )
               form_search_for.datas[j].setentry( selecteds[j+2] )
      else:
         parent.pl.form[id].data1.pack( side='left', after= parent.pl.form[id].gm, padx = 5)
         parent.pl.form[id].data1.configure( label_text = 'A(X,Y)' )
         if gm_type!='Meshfill' : parent.pl.form[id].data2.pack_forget( )
         form_search_for.data1.setentry( search_for )


      # Set the Template, Graphics Method, and Data entries
      # Note: 1D plots are special. The templates are set in the vcs_function.plot function.
      # This is because the user may wish to do 1D plot overlays, which are different than
      # overlays for other graphics methods.
      form_search_for.template.setentry( parent.template_name )
      form_search_for.gm.setentry( parent.graphics_method_name )

      # Set the Name of the Plot to Variable name in the "Select Variable" panel
      quick_plot_flag = 0
      if ( (search_for[:9] == "QuickPlot") and 
           ((parent.annotate_name is None) or (parent.annotate_name == "QuickPlot")) ):
         parent.annotate_name = parent.panelSV.tin4.get() + search_for[9:] # "Selected Panel" variable
         parent.QuickPlot_name[search_for] = parent.panelSV.tin4.get()
         quick_plot_flag = 1

      # Update the plot segements to make sure that all currently displayed 
      # segments are redrawn on the VCS Canvas properly.
      parent.vcs[ parent.vcs_id ].updateVCSsegments()

      # Show Plot results
      parent.pl.do_plot( id )

      if quick_plot_flag == 1: parent.annotate_name = None # Reset the annotate_name

      # Need the name of the variable if an overlay is needed
      return search_for, id
   #
   ####### event to set continents flag type
   #
   def evt_continents_toggle( self, parent, number ):
      self.cont_toggle_1.set( 0 ) # Auto = None
      self.cont_toggle_2.set( 0 ) # No = 0
      self.cont_toggle_3.set( 0 ) # Coarse = 2
      self.cont_toggle_4.set( 0 ) # Fine = 1
      self.cont_toggle_5.set( 0 ) # US = 3
      self.cont_toggle_6.set( 0 ) # PB = 4
      self.cont_toggle_7.set( 0 ) # River = 5
      self.cont_toggle_8.set( 0 ) # U1 = 6
      self.cont_toggle_9.set( 0 ) # U2 = 7
      self.cont_toggle_10.set( 0 ) # U3 = 8
      self.cont_toggle_11.set( 0 ) # U4 = 9
      self.cont_toggle_12.set( 0 ) # U5 = 10
      self.cont_toggle_13.set( 0 ) # U6 = 11
      if number == 0:           # No Continents
         self.cont_toggle_2.set( 1 )
      elif number == None:       # Auto Continents
         self.cont_toggle_1.set( 1 )
      elif number == 1:         # Fine Continents
         self.cont_toggle_4.set( 1 )
      elif number == 2:         # Coarse Continents
         self.cont_toggle_3.set( 1 )
      elif number == 3:         # United States Continents
         self.cont_toggle_5.set( 1 )
      elif number == 4:         # Political Boundary Continents
         self.cont_toggle_6.set( 1 )
      elif number == 5:         # Rivers Continents
         self.cont_toggle_7.set( 1 )
      elif number == 6:         # User1 Continents
         self.cont_toggle_8.set( 1 )
      elif number == 7:         # User2 Continents
         self.cont_toggle_9.set( 1 )
      elif number == 8:         # User3 Continents
         self.cont_toggle_10.set( 1 )
      elif number == 9:         # User4 Continents
         self.cont_toggle_11.set( 1 )
      elif number == 10:        # User5 Continents
         self.cont_toggle_12.set( 1 )
      elif number == 11:        # User6 Continents
         self.cont_toggle_13.set( 1 )

      parent.panelGC.cont_flg = number
      try:
         parent.pegui.continents = number
      except:
         pass

   #
   ####### event to set page orientation
   #
   def evt_page_orientation( self, parent, orientation ):

# Don't remove the Page Layout forms and clear the VCS Canvas. Instead, show the
# user what their plot will look like in the alternative page layout.
      # Remove page layout forms if necessary
#      parent.vcs[ parent.vcs_id ].clear()
#      got = parent.panelDV.scl1.get( 0, 'end' )
#      r_list = []
#      for x in got:
#          r_list.append( string.split(x, ' ')[1] )
#      parent.panelDV.update_page_layout( parent, r_list )
#
#      for x in parent.panelDV.number_lst1.keys():
#         parent.panelDV.number_lst1[ x ] = gui_control.dvholder + x
#      gui_defined_variables.update_defined_variable_list( parent )
#      gui_functions._blank_dim1_to_ndim( parent )

##       parent.vcs[ parent.vcs_id ].clear()
      try:
         dict=parent.vcs[ parent.vcs_id ].canvasinfo()
      except:
         dict={}
      x=dict.get('x',None)
      y=dict.get('y',None)
      w=dict.get('width',None)
      h=dict.get('height',None)
      
      if orientation == 'portrait':
         if h is None:
            parent.vcs[ parent.vcs_id ].portrait(clear = 1)
         else:
            parent.vcs[ parent.vcs_id ].portrait(width=min(h,w), height=max(h,w), x=x, y=y, clear = 1)
         self.page_landscape.set( 0 )
         self.page_portrait.set( 1 )
         self.page_orientation_flg = 0
      else:
         if h is None:
            parent.vcs[ parent.vcs_id ].landscape(clear = 1)
         else:
            parent.vcs[ parent.vcs_id ].landscape(width=max(h,w), height=min(h,w), x=x, y=y, clear = 1)
         self.page_landscape.set( 1 )
         self.page_portrait.set( 0 )
         self.page_orientation_flg = 1

      # Redraw the plot
      parent.panelGC.evt_plot( parent )

   #
   ####### event to set overlay flag
   #
   def evt_overlay_toggle( self, parent, O_Name ):
      parent.template_name = 'ASD'
      if parent.panelGC.over_flg == 1:
         parent.panelGC.over_flg = 0
         self.menuBar.component(O_Name + '-menu').entryconfigure(13, state = 'normal')
         self.menuBar.component(O_Name + '-menu').entryconfigure(5, state = 'normal')
      else:
         parent.panelGC.over_flg = 1
         self.menuBar.component(O_Name + '-menu').entryconfigure(13, state = 'disabled')
         self.menuBar.component(O_Name + '-menu').entryconfigure(5, state = 'disabled')
   #
   ####### event to set isoline labels flag
   #
   def evt_isoline_labels_toggle( self, parent ):
      if parent.panelGC.isol_label_flg == 1:
         parent.panelGC.isol_label_flg = 0
      else:
         parent.panelGC.isol_label_flg = 1
   #
   ####### event to define a variable
   #
   def evt_define( self, parent, var_name = None ):
      import __main__, sys
      gm_type = parent.panelGC.text_opt2.get()
      if gm_type in ["Continents"]: return 1
      gui_busy.busyStart( self, parent )
      if 1:
##       try:
         if parent.panelDM.var3 is not None:
            if var_name == None:
               var_name = parent.panelSV.tin4.get()  # send the variable to the "Selected panel"
            if parent.panelDM.fid2 is None:
               gui_message.error('Must select a file from the "Data Files" window.')
               gui_busy.busyEnd( self, parent )
               return 0
            kys=[]                                #       window.
            for i in range(len(parent.panelDV.lst1.keys())):
               kys.append(string.split(parent.panelDV.lst1.keys()[i])[0])
            if (var_name not in kys):
               d = gui_formulate.data(parent, var_name)
            else:
               gui_output.create(parent, 'd','Overwrite or Create a New Defined Variable:','Enter New Variable Name or Overwrite Existing Variable:', entry_str = var_name)
               if parent.new_defined_var is not None:
                  d = gui_formulate.data(parent, var_name)
                  var_name = parent.new_defined_var
                  d.id = d.name = parent.new_defined_var # Save the new id name
            if 1:
               if (d is None):
                  gui_busy.busyEnd( self, parent )
                  return 0

               str_var = var_name + ' ('
               for i in range(len(d.shape)):
                  s ="%d" % d.shape[i]
                  str_var = str_var + s + ', '
               if len(d.shape) > 0:
                  str_var = str_var[0:len(str_var)-2]
               str_var = str_var + ')'
               if str_var[-2:] == '()': str_var = str_var + (' = %.17g' % d)
               parent.panelDV.lst1[ str_var ] = d
               # Add the selected number placeholders (i.e., 00) to the front of the variable name
               # then, place it in the dictionary that contains the sequence number for displaying 
               # the variables defined in the list window
               parent.panelDV.number_lst1[ str_var ] = gui_control.dvholder + str_var
               __main__.__dict__[ var_name ] = d
               gui_defined_variables.update_defined( parent )
#               gui_control.record_command( parent, "\n# Record slab into memory", 1 )
#               gui_control.record_command( parent, ("%s = slab" % var_name), 1 )


##                gui_message.error("Must be a unique data variable name.")
      
#            d=parent.panelDV.lst1.keys()
#            d.sort()
#            parent.panelDV.scl1.setlist( d )
            gui_defined_variables.update_defined_variable_list( parent )
         elif parent.panelDV.selected_list != {}:
            var_name = string.split(parent.panelDV.selected, ' ')[0]
            parent.new_defined_var = None
            gui_output.create(parent, 'd','Overwrite or Create a New Defined Variable:','Enter New Variable Name or Overwrite Existing Variable:', entry_str = var_name)
            if parent.new_defined_var is not None:
               slab=gui_formulate.data( parent, var = parent.panelDV.lst1[ parent.panelDV.selected ], new_var = parent.new_defined_var)
               slab.id = slab.name = parent.new_defined_var # Save the new id name
#               gui_control.record_command( parent, "\n# Record slab into memory", 1 )
#               gui_control.record_command(parent,("%s = slab" % parent.new_defined_var), 1)
               __main__.__dict__[ parent.new_defined_var ] = slab
               gui_defined_variables.update_defined( parent )
               gui_functions._blank_dim1_to_ndim( parent )
               parent.panelDV.scl1.select_clear( 0, 'end' )
               parent.panelDV.selected_list = {}
         else:
            gui_message.error("'Must first select a variable from the 'Select Variable' panel above or from the 'Defined Variables' list window below.")
            gui_busy.busyEnd( self, parent )
            return 0

         gui_busy.busyEnd( self, parent )
         return 1
##       except:
         gui_message.error("'Must first select a variable from the 'Select Variable' panel above or from the 'Defined Variables' list window below.")
         gui_busy.busyEnd( self, parent )
         return 0

   #
   ####### event to set the plot projection
   #
   def evt_set_plot_projection( self, parent, number, O_Name ):
      # Disable Number of plots on a page when doing a
      # mollweide, robinson, and polar projections
      self.menuBar.component(O_Name + '-menu').entryconfigure(9, state = 'disabled')
      x=parent.vcs[parent.vcs_id]
      p=x.listelements('projection')
      for i in range(len(p)):
         self.projection_toggle[i].set( 0 )
      parent.plot_projection = p[number]
      if (parent.plot_projection in ['default', 'linear']): parent.template_name = 'ASD'
      self.projection_toggle[number].set( 1 )
      self.menuBar.component(O_Name + '-menu').entryconfigure(9, state = 'normal')
   #
   ####### event to set the nubmer plots on a VCS Canvas
   #
   def evt_number_of_plots_on_canvas( self, parent, number, O_Name ):
      # Disable Overlay when more that one plot per VCS Canvas
      self.menuBar.component(O_Name + '-menu').entryconfigure(3, state = 'disabled')
      parent.panelGC.over_flg = 0
      self.var_overlay.set( 0 )

      # Disable Animation when more that one plot per VCS Canvas
   #   self.menuBar.component(O_Name + '-menu').entryconfigure(10, state = 'disabled')
      self.menuBar.component(O_Name + '-menu').entryconfigure(13, state = 'disabled')

      if number == 1:
         self.plot_toggle_1.set( 1 )
         self.plot_toggle_2.set( 0 )
         self.plot_toggle_3.set( 0 )
         self.plot_toggle_4.set( 0 )
         self.menuBar.component(O_Name + '-menu').entryconfigure(3, state = 'normal')
   #     self.menuBar.component(O_Name + '-menu').entryconfigure(10, state = 'normal')
         self.menuBar.component(O_Name + '-menu').entryconfigure(13, state = 'normal')
         parent.template_name = 'ASD'
      elif number == 2:
         self.plot_toggle_1.set( 0 )
         self.plot_toggle_2.set( 1 )
         self.plot_toggle_3.set( 0 )
         self.plot_toggle_4.set( 0 )
      elif number == 3:
         self.plot_toggle_1.set( 0 )
         self.plot_toggle_2.set( 0 )
         self.plot_toggle_3.set( 1 )
         self.plot_toggle_4.set( 0 )
      elif number == 4:
         self.plot_toggle_1.set( 0 )
         self.plot_toggle_2.set( 0 )
         self.plot_toggle_3.set( 0 )
         self.plot_toggle_4.set( 1 )

      # Set the number of plots to be put on a VCS Canvas
      parent.vcs_canvas_plot_number = number

      # Reset all VCS Plot Counts to 0
      for i in range(parent.number_of_vcs_canvas):
         parent.plot_ct[ i ] = 0

      # Clear the forms and the VCS Canvas
      self.evt_clear_display( parent )
#      if (len(parent.d) != 0):
#         parent.vcs[ parent.vcs_id ].clear()
#         parent.d = {}
#         parent.dct = 0
   #
   #
   def evt_templateeditor( self, parent ):
      parent.vcs[ parent.vcs_id ].templateeditor(template_name='',
                  gui_parent=parent)
      # 
      gui_control.record_command( parent, "\n# View Template Editor" )
      gui_control.record_command( parent, "vcs.templateeditor(template_name='')" )

   def evt_gmeditor( self, parent ):
      parent.vcs[ parent.vcs_id ].graphicsmethodgui(gui_parent=parent)
      # 
      gui_control.record_command( parent, "\n# View Graphics Method Editor" )
      gui_control.record_command( parent, "vcs.graphicsmethodgui()" )

   def evt_pageeditor( self, parent ):
      parent.pegui = parent.vcs[ parent.vcs_id ].pageeditor(gui_parent=parent,
                     continents=parent.panelGC.cont_flg)
      # 
      gui_control.record_command( parent, "\n# View Page Editor" )
      gui_control.record_command( parent, "vcs.pageeditor()" )
      
##    def evt_projection( self, parent ):
##       parent.pegui = parent.vcs[ parent.vcs_id ].projectiongui(gui_parent=parent)
##       # 
##       gui_control.record_command( parent, "\n# View Projection Editor" )
##       gui_control.record_command( parent, "vcs.projectiongui()" )
   #
   ####### event to pop up the VCS 'Colormap'
   #
   def evt_colormap( self, parent ):
#      if (len(parent.d) == 0):
#         parent.vcs[ parent.vcs_id ].open()
      maxi = 100.0
      if (parent.menu.main_menu.color_intensity_toggle[1].get(  ) == 1): maxi = 255.0
      parent.vcs[ parent.vcs_id ].colormapgui(gui_parent=parent, transient = parent.menu.popup_window_settings_flg, max_intensity=maxi)
      # 
      # Record VCS Colormap Command
      gui_control.record_command( parent, "\n# View VCS Colormap" )
      gui_control.record_command( parent, 'vcs.colormapgui()' )
   #
   ####### event to 'VCS Clear Display'
   #
   def evt_clear_display( self, parent ):
      # Make sure all buttons are turned off before you make the VCS clear call
      canvas_id = str( parent.vcs[ parent.vcs_id ].canvasid() )
      for i in parent.pl.form.keys():
         form_id = parent.pl.form[i].lbln.cget( 'text' )[0]
         if (parent.pl.form[i].lbl.cget( 'label_text' ) == 'On') and (form_id == canvas_id):
            try:
               if parent.pl.form[i].__dict__.has_key("display") and parent.pl.form[i].display is not None:
                  if isinstance(parent.pl.form[i].display,(tuple,list)):
                     displays = parent.pl.form[i].display
                  else:
                     displays = [parent.pl.form[i].display,]
               for d in displays:
                  d.off = 1
                  parent.vcs[ parent.vcs_id ].remove_display_name( d.name )
               parent.pl.form[i].off_btn.create_image(0,0, anchor=Tkinter.NW, image=parent.pl.form[i].off_btnimage )
               parent.pl.form[i].lbl.configure( label_text = 'Off' )
            except Exception,err:
##                print err
               del parent.pl.form[i].display

      # Clear the VCS Canvas
      parent.vcs[ parent.vcs_id ].clear()
      if (len(parent.d) != 0):
         parent.d = {}
         parent.dct = 0
      parent.plot_ct[ parent.vcs_id ] = 0
      parent.XYct_restart = 1
      parent.XYct = 0

      remove_graphics_methods( parent )

      # Remove QuickPlot from the Defined Variables window and from the Page Layout
      remove_Q = "QuickPlot"
      parent.panelGC.remove_variable_from_defined_variable_list( parent, remove_Q )
      for i in range(16):
         parent.panelGC.remove_variable_from_defined_variable_list( parent, remove_Q+"%d"%i )

      for i in parent.pl.form.keys():
          if ( ( parent.pl.form[i].data1.get()[:9] == "QuickPlot") or
               ( parent.pl.form[i].data2.get()[:9] == "QuickPlot") ):
             parent.pl.evt_remove_form( i, parent, None )
      # 
      # Record Clear Command
      gui_control.record_command( parent, "\n# Clear VCS Canvas", 1 )
      gui_control.record_command( parent, ('vcs_canvas_list[ %d ].clear()' % parent.vcs_id), 1)
   #
   ####### event to 'Close VCS Canvas'
   #
   def evt_close_canvas( self, parent ):
      import time
      parent.vcs[ parent.vcs_id ].animate.close()
      time.sleep(1)  # All threading time to sync things up...
      parent.vcs[ parent.vcs_id ].close()
   #
   ####### event to 'Animate' the VCS Canvas
   #
   def evt_animate( self, parent, O_Name ):
# Dean disable this later when doing an animation.
#      self.menuBar.component(O_Name + '-menu').entryconfigure(3, state = 'disabled')
#      self.menuBar.component(O_Name + '-menu').entryconfigure(9, state = 'disabled')

      # Stop the replotting of data when the template editor is in use.
      if (parent.vcs[ parent.vcs_id ].SCREEN_MODE() == "TEDITOR"):
          gui_message.error("You must cancel the 'Template Editor' GUI before bring up the 'Animation' GUI.")
          return

      if parent.animate_canvas == parent.vcs_id:
         if (len(parent.d) == 0):
            parent.vcs[ parent.vcs_id ].open()
         parent.vcs[ parent.vcs_id ].animate.gui(gui_parent=parent, transient=parent.menu.popup_window_settings_flg )
         # 
         # Record Animation Command
         gui_control.record_command( parent, "\n# Animate VCS Canvas" )
         gui_control.record_command( parent, 'vcs.animate.gui()' )
      else:
         gui_message.error("The VCS animation cannot remember data displayed on a past canvas. You must redisplay the plot the canvas in order to animate it.")
   #
   ####### event to check which graphics method is in use
   #
   def evt_which_graphics_method( self, parent, O_Name, event ):
      gm_str = parent.panelGC.text_opt2.get()
      sp = gm_str.split()
      if sp[0]=="Ext":
         if sp[1][1:-1]=='boxfill':
            gm_str = "Boxfill"
         else:
            gm_str=sp[0]
            
      if gm_str not in ['FilledIsoline', 'BoxedIsoline', 'BoxDiscrete','Ext']:
         parent.panelDV.evt_select_graphics_method(gm_str, parent)
      if ((gm_str in ["FilledIsoline", "BoxedIsoline", "Scatter", "Taylordiagram"])
          or (parent.vcs_canvas_plot_number > 1)):
         self.menuBar.component(O_Name + '-menu').entryconfigure(13, state = 'disabled')
      else:
         if self.over_flg != 1:
            self.menuBar.component(O_Name + '-menu').entryconfigure(13, state = 'normal')
   #
   ####### event to check which VCS Canvas is in use
   #
      if parent.menu.vcs_canvas_gui_settings_flg == 0:
         self.text_opt3.set( gui_control.potchlst[ parent.vcs_id ] )
      else:
         self.text_opt3.set( gui_control.potchlst2[0] )
   #
   ####### event to check which VCS Canvas is in use
   #
   def evt_which_vcs_canvas( self, parent, event ):
      vcs_str = parent.panelGC.text_opt3.get()
      if parent.menu.vcs_canvas_gui_settings_flg == 0:
         if vcs_str == 'VCS Canvas 1':
            opt_index = parent.vcs_id = 0
         elif vcs_str == 'VCS Canvas 2':
            opt_index = parent.vcs_id = 1
         elif vcs_str == 'VCS Canvas 3':
            opt_index = parent.vcs_id = 2
         elif vcs_str == 'VCS Canvas 4':
            opt_index = parent.vcs_id = 3
         elif vcs_str[0:16] == 'Clear VCS Canvas':
            opt_index = parent.vcs_id 
            try: self.evt_clear_display( parent )
            except: pass
         elif vcs_str[0:16] == 'Close VCS Canvas':
            opt_index = parent.vcs_id 
            try: 
               if (parent.vcs[ parent.vcs_id ].iscanvasdisplayed()): self.evt_close_canvas( parent )
            except: pass
         elif vcs_str[0:16] == 'VCS Canvas GUI':
            opt_index = parent.vcs_id = 0
   
         potchlst = []
         for x in gui_control.potchlst: potchlst.append( x )
         if vcs_str != 'Background Canvas':
            potchlst[5] = potchlst[5][0:16] + " %d" % (parent.vcs_id+1)
            potchlst[6] = potchlst[6][0:16] + " %d" % (parent.vcs_id+1)
         else :
            opt_index = 4
            potchlst.remove( potchlst[5] )
            potchlst.remove( potchlst[5] )
   
         self.opt3.setitems( potchlst, index = opt_index )
      else:
         # Make sure we are out of the template editor mode before we clear or exist.
         try: parent.vcs[ parent.vcs_id ].canvas_gui.notebook.selectpage("Data & Plot Info")
         except: pass

         if vcs_str[0:16] == 'Clear VCS Canvas':
            opt_index = parent.vcs_id
            try: self.evt_clear_display( parent )
            except: pass
         elif vcs_str[0:16] == 'Close VCS Canvas':
            opt_index = parent.vcs_id
            try:
               if (parent.vcs[ parent.vcs_id ].iscanvasdisplayed()): self.evt_close_canvas( parent )
            except: pass
         self.opt3.setitems( gui_control.potchlst2, index = 0 )

      try:
         parent.vcs[ parent.vcs_id ].canvasraised()
      except:
         pass

def remove_graphics_methods( parent ):
   BFrange = parent.Boxfillct[ parent.vcs_id ] - (parent.vcs_id*1000) - 1
   for i in range(BFrange):
      gmname = 'ASD' + str( (parent.vcs_id*1000) + i + 1 )
      try:
         g_method = parent.vcs[ parent.vcs_id ].getboxfill(gmname)
         parent.vcs[ parent.vcs_id ].removeobject( g_method )
      except:
         pass
   parent.Boxfillct[ parent.vcs_id ] = 0

   IFrange = parent.Isofillct[ parent.vcs_id ] - (parent.vcs_id*1000) - 1
   for i in range(IFrange):
      gmname = 'ASD' + str( (parent.vcs_id*1000) + i + 1 )
      try:
         g_method = parent.vcs[ parent.vcs_id ].getisofill(gmname)
         parent.vcs[ parent.vcs_id ].removeobject( g_method )
      except:
         pass
   parent.Isofillct[ parent.vcs_id ] = 0

   ILrange = parent.Isolinect[ parent.vcs_id ] - (parent.vcs_id*1000) - 1
   for i in range(ILrange):
      gmname = 'ASD' + str( (parent.vcs_id*1000) + i + 1 )
      try:
         g_method = parent.vcs[ parent.vcs_id ].getisoline(gmname)
         parent.vcs[ parent.vcs_id ].removeobject( g_method )
      except:
         pass
   parent.Isolinect[ parent.vcs_id ] = 0

   TDrange = parent.Taylordiagramct[ parent.vcs_id ] - (parent.vcs_id*1000) - 1
   for i in range(TDrange):
      gmname = 'ASD' + str( (parent.vcs_id*1000) + i + 1 )
      try:
         g_method = parent.vcs[ parent.vcs_id ].gettaylordiagram(gmname)
         parent.vcs[ parent.vcs_id ].removeobject( g_method )
      except:
         pass
   parent.Taylordiagramct[ parent.vcs_id ] = 0

   VRrange = parent.Vectorct[ parent.vcs_id ] - (parent.vcs_id*1000) - 1
   for i in range(VRrange):
      gmname = 'ASD' + str( (parent.vcs_id*1000) + i + 1 )
      try:
         g_method = parent.vcs[ parent.vcs_id ].getvector(gmname)
         parent.vcs[ parent.vcs_id ].removeobject( g_method )
      except:
         pass
   parent.Vectorct[ parent.vcs_id ] = 0

   MRrange = parent.Meshfillct[ parent.vcs_id ] - (parent.vcs_id*1000) - 1
   for i in range(MRrange):
      gmname = 'ASD' + str( (parent.vcs_id*1000) + i + 1 )
      try:
         g_method = parent.vcs[ parent.vcs_id ].getmeshfill(gmname)
         parent.vcs[ parent.vcs_id ].removeobject( g_method )
      except:
         pass
   parent.Meshfillct[ parent.vcs_id ] = 0

   SRrange = parent.Scatterct[ parent.vcs_id ] - (parent.vcs_id*1000) - 1
   for i in range(SRrange):
      gmname = 'ASD' + str( (parent.vcs_id*1000) + i + 1 )
      try:
         g_method = parent.vcs[ parent.vcs_id ].getscatter(gmname)
         parent.vcs[ parent.vcs_id ].removeobject( g_method )
      except:
         pass
   parent.Scatterct[ parent.vcs_id ] = 0

def show_and_unshow_graphics_options( parent ): 
      O_Name = 'Options' # This is the name of the pulldown menu that the user sees
      if ((sys.platform == 'darwin') and (gui_control.do_aqua == 1)): O_Name = 'Options   '

      if parent.menu.vcs_canvas_gui_settings_flg == 0:
         parent.panelGC.menuBar.component(O_Name + '-menu').entryconfigure(2, state = 'normal')
         parent.panelGC.menuBar.component(O_Name + '-menu').entryconfigure(3, state = 'normal')
         parent.panelGC.menuBar.component(O_Name + '-menu').entryconfigure(5, state = 'normal')
         parent.panelGC.menuBar.component(O_Name + '-menu').entryconfigure(6, state = 'normal')
         parent.panelGC.menuBar.component(O_Name + '-menu').entryconfigure(8, state = 'normal')
         parent.panelGC.menuBar.component(O_Name + '-menu').entryconfigure(9, state = 'normal')
         parent.panelGC.menuBar.component(O_Name + '-menu').entryconfigure(11, state = 'normal')
         parent.panelGC.menuBar.component(O_Name + '-menu').entryconfigure(13, state = 'normal')

         # Allow the user bring up the template and graphics editors only if VCS Canvas GUI is *not* in use
         parent.panelDV.canvas_tgicon.pack( side='left', after=parent.panelDV.canvas_develicon, fill=Tkinter.BOTH, expand='no', padx=1, pady=1 )
      else:
         parent.panelGC.menuBar.component(O_Name + '-menu').entryconfigure(2, state = 'disabled')
         parent.panelGC.menuBar.component(O_Name + '-menu').entryconfigure(3, state = 'disabled')
         parent.panelGC.menuBar.component(O_Name + '-menu').entryconfigure(5, state = 'disabled')
         parent.panelGC.menuBar.component(O_Name + '-menu').entryconfigure(6, state = 'disabled')
         parent.panelGC.menuBar.component(O_Name + '-menu').entryconfigure(8, state = 'disabled')
         parent.panelGC.menuBar.component(O_Name + '-menu').entryconfigure(9, state = 'disabled')
         parent.panelGC.menuBar.component(O_Name + '-menu').entryconfigure(11, state = 'disabled')
         parent.panelGC.menuBar.component(O_Name + '-menu').entryconfigure(13, state = 'disabled')

         # Don't let the user bring up the template  and graphics editors if VCS Canvas GUI is in use
         parent.panelDV.canvas_tgicon.pack_forget( )
         parent.show_template_graphics_method = 0
         parent.panelDV.evt_show_template_graphics_method(parent, None)

#---------------------------------------------------------------------
# End of File
#---------------------------------------------------------------------
