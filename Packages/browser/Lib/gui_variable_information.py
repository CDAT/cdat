#!/usr/bin/env python
# Adapted for numpy/ma/cdms2 by convertcdms.py
#
# The PCMDI Data Browser Variable Information Panel -  gui_variable_information module
#
###############################################################################
#                                                                             #
# Module:       gui_variable_information module                               #
#                                                                             #
# Copyright:    "See file Legal.htm for copyright information."               #
#                                                                             #
# Authors:      PCMDI Software Team                       	              #
#               Lawrence Livermore NationalLaboratory:                        #
#               support@pcmdi.llnl.gov                                        #
#                                                                             #
# Description:  PCMDI Software System browser Tkinter "Variable Information"  #
#               panel GUI.                                                    #
#                                                                             #
# Version:      3.0                                                           #
#                                                                             #
###############################################################################
#
#---------------------------------------------------------------------
# NOTE: need to use version of Python that imports Tkinter and Pmw
#---------------------------------------------------------------------
import Tkinter, Pmw
import __main__
import sys, os, string, types, cdms2
import gui_control
import gui_message
import gui_defined_variables
import gui_busy
import vcs_function
import gui_formulate
from gui_support import gui_color

#---------------------------------------------------------------------------
#
# Start of the "Variable Information" panel GUI Layout
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
# Begin the creation of "Variable Information"  panel
#---------------------------------------------------------------------
class create:
   def __init__( self, parent ):

      #-------------------------------------------
      # create 5th sub-panel
      #-------------------------------------------
      self.lst1 = []
      self.scl1 = Pmw.ScrolledText( parent.pane.pane( 'panelVI' ),
                                 label_text='Variable Information',
                                 labelpos='n',
                                 text_wrap='none',
                                 text_background='white',
                                 text_foreground='black',
                                 text_state='disabled',
                                 horizscrollbar_width=gui_control.scl_width,
                                 vertscrollbar_width=gui_control.scl_width )
      self.scl1.pack( fill = 'both', expand=1, pady=3 )
      parent.balloon.bind(self.scl1, "This is the 'Variable Information' list window.\n\nDisplays the variable information that is selected\nin the above 'Select Variable' 'Variable:' entry\nfield or in the above 'Defined Variables' list\nwindow.") 


#----------------------------------------------------------------------
#----------------------------------------------------------------------
#
# Show the VCS "Page Layout" .
#
#----------------------------------------------------------------------
#----------------------------------------------------------------------
#Create dummy form class
class Form:
   pass

# Create page description panel's forms list
class create_page_description_panel:
   def __init__( self, parent ):
        self.eself = parent

        # create the page description main toplevel menu
        self.pmain_menu = Pmw.MenuBar(parent.pane.pane( 'panelVI' ),
                hull_relief = 'raised',
                hull_borderwidth = 2,
                balloon = self.eself.balloon
                )
        self.pmain_menu.pack(side='top', fill='both')

        self.pmain_menu.addmenu('Page Layout Option', 'Set VCS Page Layout Preferences', tearoff = 1)

        self.pmain_menu.addmenuitem('Page Layout Option', 'command', 'create new page description form',
                       label = 'Create Page Layout Form',
                       command = gui_control.Command(self.evt_create_new_pl_form, parent)
                      )

        self.scl_frame = Pmw.ScrolledFrame( parent.pane.pane( 'panelVI' ),
#                labelpos = 'nw',
#                label_text = 'Page Layout Forms:',
                horizscrollbar_width=gui_control.scl_width,
                vertscrollbar_width=gui_control.scl_width
                )
        self.scl_frame.pack( side='top', fill='both' )
        self.eself.balloon.bind(self.scl_frame, " The Page Layout Editor is used to specify picture descriptor\n forms that define how plots are displayed on the\n VCS Canvas (drawing area). There are three primary\n objects in each picture descriptor form that correspond\n to text windows of the template (in green), the\n graphics method (in blue), and the data (in red).\n Names are assigned to these text windows from the\n scroll windows below the 'Page Layout Forms'.\n These scroll windows include a green `Template'\n window, a blue `Graphics Method' window, and a red\n `Data' window.")

        # Keep track of created forms
        self.form={}
   #
   # Create a new page layout form
   #
   def evt_create_new_pl_form( self, parent ):
        # find unique id number
        id_num = 0
        for i in parent.pl.form.keys():
           if id_num < parent.pl.form[i].id:
              id_num = parent.pl.form[i].id
        id_num += 1

        parent.pl.create_form( parent, None, id = id_num, vcs=parent.vcs[ parent.vcs_id ] )

        return id_num

   # Create page description form
   #
   def create_form( self, parent, data1=None, data2=None, data3=5, id = 1, vcs=None ):
        # Get the appropriate template and graphics method
        graphics_method = parent.panelGC.text_opt2.get()
        if string.lower(graphics_method) not in parent.vcs[ parent.vcs_id ].listelements():
           graphics_method = 'Boxfill'
        if parent.template_name not in parent.vcs[ parent.vcs_id ].listelements('template'):
           template = 'default'
        else:
           template = parent.template_name
        if parent.graphics_method_name not in parent.vcs[ parent.vcs_id ].listelements( graphics_method ):
##            print parent.graphics_method_name,graphics_method,parent.vcs[ parent.vcs_id ].listelements( graphics_method )
           graphics_name = 'default'
           graphics_name = 'ASD'
        else:
           graphics_name = parent.graphics_method_name
        form = Form()
        self.form[id] = form
        self.form[id].display = None

        form.line=Tkinter.Frame(self.scl_frame.interior())
        form.line.pack(side='top', fill='both')

        # Scroll to the bottom of the window to show new form
        self.scl_frame.yview( 'moveto', 1 )

        # Set the unique id number
        form.id = id

        # Store the Canvas ID
        form.canvasid = vcs.canvasid()

        form.remove = Pmw.LabeledWidget(form.line,
                    labelpos = 'n',
                    label_text = 'Remove')
        form.remove.pack( side='left', padx = 5 )
        form.remove_btn = Tkinter.Canvas(form.remove.interior(), bd=0, highlightthickness=0,
                                        width = 32, height = 32)
        form.remove_btn.pack( padx=1, pady=1 )
        self.eself.balloon.bind( form.remove_btn, "Remove the page layout form and displayed  graphics on the VCS Canvas." )
        form.remove_btnimage = Tkinter.PhotoImage( file = os.path.join(cdms2.__path__[0],'..','..','..','..', 'bin', 'remove.gif') )
        form.on_btnimage = Tkinter.PhotoImage( file = os.path.join(cdms2.__path__[0],'..','..','..','..', 'bin', 'on.gif') )
        form.remove_btn.create_image(0,0, anchor=Tkinter.NW, image=form.remove_btnimage )
        form.remove_btn.bind( '<1>', gui_control.Command( self.evt_remove_form, id, parent ))

        form.lbl = Pmw.LabeledWidget(form.line,
                    labelpos = 'n',
                    label_text = 'Off'
                    )
        form.lbl.pack( side='left', padx = 5 )
        self.eself.balloon.bind(form.lbl, "Toggle the plot diplayed on the VCS Canvas 'On' or 'Off'.")

        form.off_btn = Tkinter.Canvas(form.lbl.interior(), bd=0, highlightthickness=0,
                                      width = 32, height = 32)
        form.off_btn.pack( side='left', fill=Tkinter.BOTH, expand='no', padx=1, pady=1 )
        self.eself.balloon.bind( form.off_btn, "Select button to remove or redraw plot." )
        form.off_btnimage = Tkinter.PhotoImage( file = os.path.join(cdms2.__path__[0],'..','..','..','..', 'bin', 'off.gif') )
        form.on_btnimage = Tkinter.PhotoImage( file = os.path.join(cdms2.__path__[0],'..','..','..','..', 'bin', 'on.gif') )
        form.off_btn.create_image(0,0, anchor=Tkinter.NW, image=form.off_btnimage )
        form.off_btn.bind( '<1>', gui_control.Command( self.evt_status_button, id, parent ))

#        form.btn = Tkinter.Button(form.lbl.interior(),
#            background = 'red',
#            height = 1,
#            command = gui_control.Command(self.evt_status_button, id, parent)
#            )
#        form.btn.pack(fill='both')

        form.template = Pmw.EntryField(form.line,
                labelpos = 'n',
                label_text = 'Template',
                entry_width = 15,
                entry_background = gui_color.template_bg,
                entry_foreground = gui_color.template_fg,
                value = template,
                validate = None)
        form.template.pack( side='left', padx = 5)
        form.template.component('entry').bind('<ButtonPress>', gui_control.Command(self.evt_replace_template, 1, id, parent ))
        form.template.component('entry').bind('<ButtonRelease>', gui_control.Command(self.evt_replace_template, 0, id, parent ))
        form.template.component('entry').bind('<KeyPress>', gui_control.Command(self.evt_replace_template, 0, id, parent ))
        self.eself.balloon.bind(form.template, "Select an entry from the %s 'Template' list above." % gui_color.template_bg)

        form.gm = Pmw.EntryField(form.line,
                labelpos = 'n',
                label_text = graphics_method,
                entry_width = 15,
                entry_background = gui_color.graphics_method_bg,
                entry_foreground = gui_color.graphics_method_fg,
                value = graphics_name,
                validate = None)
        form.gm.pack( side='left', padx = 5)
        form.gm.component('entry').bind('<ButtonPress>', gui_control.Command(self.evt_replace_gm,1,id, parent))
        form.gm.component('entry').bind('<ButtonRelease>', gui_control.Command(self.evt_replace_gm,0,id, parent))
        form.gm.component('entry').bind('<KeyPress>', gui_control.Command(self.evt_replace_gm,0,id, parent))
        form.gm.type = graphics_method
        self.eself.balloon.bind(form.gm, "Select an entry from the %s 'Graphics Method' list above." % gui_color.graphics_method_bg)

        label_txt = 'A(X,Y)'
        if graphics_method in ['Boxfill']:
           label_txt = 'A(X,Y)'
        data1_txt = ''
        if data1 is not None:
           data1_txt = data1
        form.needed_data = 1
        form.data1 = Pmw.EntryField(form.line,
                labelpos = 'n',
                label_text = label_txt,
                entry_width = 15,
                entry_background = gui_color.six,
                entry_foreground = 'black',
                value = data1_txt,
                validate = None)
        form.data1.pack( side='left', padx = 5)
        form.data1.component('entry').bind('<ButtonPress>', gui_control.Command(self.evt_replace_data, 1, id, 1, parent))
        form.data1.component('entry').bind('<ButtonRelease>', gui_control.Command(self.evt_replace_data, 0, id, 1, parent))
        form.data1.component('entry').bind('<KeyPress>', gui_control.Command(self.evt_replace_data, 0, id, 1, parent))
        self.eself.balloon.bind(form.data1, "Select an entry from the white 'Data' list above.")

        label_txt = 'B(X,Y)'
        if graphics_method in ['Boxfill']:
           label_txt = 'B(X,Y)'
        data2_txt = ''
        if data2 is not None:
           data2_txt = data2
        form.data2 = Pmw.EntryField(form.line,
                labelpos = 'n',
                label_text = label_txt,
                entry_width = 15,
                entry_background = gui_color.six,
                entry_foreground = 'black',
                value = data2_txt,
                validate = None)
#        form.data2.pack( side='left', padx = 5)
        form.data2.component('entry').bind('<ButtonPress>', gui_control.Command(self.evt_replace_data, 1, id, 2, parent))
        form.data2.component('entry').bind('<ButtonRelease>', gui_control.Command(self.evt_replace_data, 0, id, 2, parent))     
        form.data2.component('entry').bind('<KeyPress>', gui_control.Command(self.evt_replace_data, 0, id, 2, parent))        
        self.eself.balloon.bind(form.data2, "Select an entry from the white 'Data' list above.")

        ## Charles Doutriaux code for additional vars
        ## resuisng data3 which was not used, if we ever want to allow creation of forms for more than 5 extra vars
        form.datas = []
        for j in range(data3):
           form.datas.append(
              Pmw.EntryField(form.line,
                             labelpos = 'n',
                             label_text = label_txt,
                             entry_width = 15,
                             entry_background = gui_color.six,
                             entry_foreground = 'black',
                             value = "",
                             validate = None)
              )
           #        form.data2.pack( side='left', padx = 5)
           form.datas[-1].component('entry').bind('<ButtonPress>', gui_control.Command(self.evt_replace_data, 1, id, j+3, parent))
           form.datas[-1].component('entry').bind('<ButtonRelease>', gui_control.Command(self.evt_replace_data, 0, id, j+3, parent))     
           form.datas[-1].component('entry').bind('<KeyPress>', gui_control.Command(self.evt_replace_data, 0, id, j+3, parent))        
           self.eself.balloon.bind(form.datas[-1], "Select an entry from the white 'Data' list above.")
           
        form.priority = Pmw.EntryField(form.line,
                labelpos = 'n',
                label_text = 'P',
                entry_width = 3,
                value = '0',
                validate = None,
                command = gui_control.Command(self.evt_change_priority, id, parent)
                )
        
        form.priority.pack( side='left', padx = 5)
        self.eself.balloon.bind(form.priority, "Enter priority level for plot.")
        form.priority.component('entry').bind( "<Key>", gui_control.Command(self.evt_form_priority_change_color, id, parent) )

        form.lblc = Pmw.LabeledWidget(form.line,
                    labelpos = 'n',
                    label_text = 'Canvas')
        form.lblc.pack( side='left', padx = 5 )
        form.lbln = Tkinter.Label(form.lblc.interior(),
                text = '%d.' % vcs.canvasid())
        form.lbln.pack(fill='x', expand=1, padx=10, pady=5)
        self.eself.balloon.bind(form.lblc, "Set VCS Canvas ID from main VCDAT 'Plot Output\nDestination' choice button.")

#        form.btn2 = Tkinter.Button(form.remove.interior(),
#            background = gui_color.one,
#            height = 1,
#            command = gui_control.Command(self.evt_remove_form, id, parent)
#            )
#        form.btn2.pack(fill='both')


#        form.data2 = Pmw.EntryField(form.line,
#                labelpos = 'n',
#                label_text = 'A(X,Y)',
#                entry_width = 15,
#                entry_background = 'red',
#                entry_foreground = 'white',
#                validate = None)
#        form.data2.pack( side='left', padx = 5)
#        form.data2.component('entry').bind('<ButtonPress>', gui_control.Command(self.evt_replace_data,1,id,parent))
#        form.data2.component('entry').bind('<ButtonRelease>', gui_control.Command(self.evt_replace_data,0,id,parent))
#        form.data2.component('entry').bind('<KeyPress>', gui_control.Command(self.evt_replace_data,0,id,parent))
#
#        form.data3 = Pmw.EntryField(form.line,
#                labelpos = 'n',
#                label_text = 'A(X,Y)',
#                entry_width = 15,
#                entry_background = 'red',
#                entry_foreground = 'white',
#                validate = None)
#        form.data3.pack( side='left', padx = 5)
#        form.data3.component('entry').bind('<ButtonPress>', gui_control.Command(self.evt_replace_data,1,id,parent))
#        form.data3.component('entry').bind('<ButtonRelease>', gui_control.Command(self.evt_replace_data,0,id,parent))
#        form.data3.component('entry').bind('<KeyPress>', gui_control.Command(self.evt_replace_data,0,id,parent))

        form.separator = Tkinter.Frame ( self.scl_frame.interior(), relief = 'raised',
                    height = 4, width = 820, borderwidth = 2, background="MEDIUM ORCHID")
        form.separator.pack(side = 'top' )


   def evt_form_priority_change_color( self, id, parent, event ):
      # Stop the replotting of data when the template editor is in use.
      if (parent.vcs[ parent.vcs_id ].SCREEN_MODE() == "TEDITOR"):
          gui_message.error("You must cancel the 'Template Editor' GUI before changing the plot priority.")
          return "break"

      # change backgound color to red
      self.form[ id ].priority.configure( entry_background = Pmw.Color.changebrightness(self.eself,'red',0.85) )

   def evt_change_priority( self, id, parent ):
      # Stop the replotting of data when the template editor is in use.
      if (parent.vcs[ parent.vcs_id ].SCREEN_MODE() == "TEDITOR"):
          gui_message.error("You must cancel the 'Template Editor' GUI before changing the plot priority.")
          return "break"

      self.form[ id ].priority.configure( entry_background = 'white' )
      try:
         p_val = string.atoi(self.form[id].priority.get())
      except:
          gui_message.error( "The 'P' priority value must be an integer." )
          return

      if ((self.form[id].__dict__.has_key('display')) and (self.form[id].display is not None)):
         if isinstance(self.form[id].display,list):
            displays = self.form[id].display
         else:
            displays = [self.form[id].display,]
      do_plot = False
      for display in displays:
         if ((isinstance(p_val, types.IntType)) and (display is not None)):
             try:
                display.priority = p_val
                vcs = self.eself.vcs[self.eself.vcs_id].update()
             except:
                do_plot = True

      if do_plot:
         self.do_plot( id )
         if isinstance(self.form[id].display,list):
            for display in self.form[id].display:
               display.priority = p_val
         else:
            self.form[id].display.priority = p_val

   def evt_remove_form( self, id, parent, event ):
      # Stop the replotting of data when the template editor is in use.
      if (parent.vcs[ parent.vcs_id ].SCREEN_MODE() == "TEDITOR"):
          gui_message.error("You must cancel the 'Template Editor' GUI before removing the plot.")
          return "break"

      if ((self.form[id].__dict__.has_key('display')) and (self.form[id].display is not None)):
         if isinstance(self.form[id].display,list):
            displays = self.form[id].display
         else:
            displays = [self.form[id].display,]
         for display in displays:
            try:
               display.off = 1
               self.eself.vcs[ self.eself.vcs_id ].remove_display_name( display.name )
            except Exception,err:
               if parent.show_debug: print err
##                print 'failed'
               pass
      vcs = self.eself.vcs[self.eself.vcs_id].update()
      form = self.form[id]
      self.eself.balloon.unbind( form.remove_btn)
      form.separator.destroy()
      form.line.destroy()
      try:
         del self.form[id].display
      except: pass
     
      del self.form[id]

   def evt_status_button( self, id, parent, event ):
      # Stop the replotting of data when the template editor is in use.
      if (parent.vcs[ parent.vcs_id ].SCREEN_MODE() == "TEDITOR"):
          gui_message.error("You must cancel the 'Template Editor' GUI before removing the plot.")
          return "break"

      if ((self.form[id].__dict__.has_key('display')) and (self.form[id].display is not None)):
         if isinstance(self.form[id].display,list):
            displays = self.form[id].display
         else:
            displays = [self.form[id].display,]

         did_error = False
         for display in displays:
            if hasattr(display,'off') and display.off == 0:
               try:
                  display.off = 1
                  #parent.vcs[ parent.vcs_id ].remove_display_name( display.name )
               except Exception,err:
                  if parent.show_debug: print 'error in crap',display.name,err
                  pass
               self.form[id].off_btn.create_image(0,0, anchor=Tkinter.NW, image=self.form[id].off_btnimage )
               self.form[id].lbl.configure( label_text = 'Off' )
            else:
               parent.template_skip=1   # step over the set_template function in vcs_function.py
               try:
                  display.off = 0
               except Exception,err:
                  did_error = True
               self.form[id].off_btn.create_image(0,0, anchor=Tkinter.NW, image=self.form[id].on_btnimage )
               self.form[id].lbl.configure( label_text = 'On' )
               parent.template_skip=0
         if did_error:
            self.do_plot( id )

         vcs = self.eself.vcs[self.eself.vcs_id].update()
      

   def evt_replace_template( self, pe, id, parent, event ):
      # Stop the replotting of data when the template editor is in use.
      if (parent.vcs[ parent.vcs_id ].SCREEN_MODE() == "TEDITOR"):
          gui_message.error("You must cancel the 'Template Editor' GUI before replacng the template.")
          return "break"

      if pe == 1:
         self.form[id].template.setentry( self.eself.panelDV.template_bin_name )
         if (self.form[id].data1.get( ) != '') or (self.eself.panelDV.bin_type in ["Continents"]):
            if self.eself.panelDV.template_bin_name != "ASD_dud":
               self.do_plot( id )
            else:
               parent.template_skip=1   # step over the set_template function in vcs_function.py
               parent.template_name = self.eself.panelDV.template_bin_name
               self.do_plot( id )
               parent.template_skip=0   # step over the set_template function in vcs_function.py

      return "break"

   def evt_replace_gm( self, pe, id, parent, event ):
      # Stop the replotting of data when the template editor is in use.
      if (parent.vcs[ parent.vcs_id ].SCREEN_MODE() == "TEDITOR"):
          gui_message.error("You must cancel the 'Template Editor' GUI before replacing the graphics method.")
          return "break"

      if pe == 1:
         self.form[id].gm.setentry( self.eself.panelDV.gm_bin_name )
         self.form[id].gm.configure( label_text = self.eself.panelDV.bin_type )
         self.form[id].gm.type = self.eself.panelDV.bin_type
         if self.eself.panelDV.bin_type in [ "Vector", "Scatter", "XvsY","Meshfill"]:
            self.form[id].needed_data = 2
            self.form[id].data1.configure( label_text = 'U(X,Y)' )
            self.form[id].data2.configure( label_text = 'V(X,Y)' )
            self.form[id].data2.pack( side='left', after= self.form[id].data1, padx = 5)
            if (self.form[id].data1.get() != "") and (self.form[id].data2.get() != ""):
               if self.eself.panelDV.template_bin_name != "ASD_dud":
                  self.do_plot( id )
               else:
                  parent.template_skip=1   # step over the set_template function in vcs_function.py
                  parent.template_name = self.eself.panelDV.template_bin_name
                  self.do_plot( id )
                  parent.template_skip=0   # step over the set_template function in vcs_function.py
            elif  (self.form[id].data1.get() != "") and self.eself.panelDV.bin_type=="Meshfill":
               self.do_plot( id )
               
         elif self.eself.panelDV.bin_type == "Continents":
            self.form[id].needed_data = 0
            self.form[id].data1.setentry( "" )
            self.form[id].data2.setentry( "" )
            self.form[id].data1.pack_forget( )
            self.form[id].data2.pack_forget( )
            self.do_plot( id )
         else:
            self.form[id].needed_data = 1
            self.form[id].data1.pack( side='left', after= self.form[id].gm, padx = 5)
            self.form[id].data1.configure( label_text = 'A(X,Y)' )
            self.form[id].data2.pack_forget( )
            if self.form[id].data1.get( ) != '': 
               if self.eself.panelDV.template_bin_name != "ASD_dud":
                  self.do_plot( id )
               else:
                  parent.template_skip=1   # step over the set_template function in vcs_function.py
                  parent.template_name = self.eself.panelDV.template_bin_name
                  self.do_plot( id )
                  parent.template_skip=0   # step over the set_template function in vcs_function.py

      return "break"

   def evt_replace_data( self, pe, id, data_entry, parent, event ):
      # Stop the replotting of data when the template editor is in use.
      if (parent.vcs[ parent.vcs_id ].SCREEN_MODE() == "TEDITOR"):
          gui_message.error("You must cancel the 'Template Editor' GUI before replacing the data.")
          return "break"

      if data_entry == 1:
          data = self.form[id].data1
      elif data_entry == 2:
          data = self.form[id].data2
      elif data_entry > 2:
           data = self.form[id].datas[data_entry-3]

      if pe == 1:
           try:
             if self.eself.panelDV.selected == []: 
                data.setentry( "" )
                gui_message.error( "Must first select a variable from the 'Defined Variables' window located above." )
                return "break"
             else:
                data.setentry( string.split(self.eself.panelDV.data_bin_name," ")[0] )
           except:
             pass

           if self.eself.panelDV.bin_type in [ "Vector", "Scatter", "XvsY","Meshfill"]:
              if (self.form[id].data1.get() != "") and (self.form[id].data2.get() != ""):
                 self.do_plot( id )
                 return "break"
           if self.eself.panelDV.bin_type in ["Meshfill"]:
              try:
                 self.do_plot( id )
              except Exception,err:
                 gui_busy.busyEnd( self, self.eself )
                 return "break"
           else:
              self.do_plot( id )

      return "break"

   def do_plot( self, id ):
       parent = self.eself                # Parent is equal to root found in tk_cdat.py
       gui_busy.busyStart( self, parent ) # Set the pointer to a watch

       # remembers the external function for replotting later
       parent.external_plot_functions[id] = parent.external_plot
       # If you already have a plot displayed and you are changing a primary object, then
       # turn the display off first, then replace the object.
       #
       # Note to Dean:
       # Must check later to see if the array's are being freed. See the array = ['plot_1']
       # in the display.list()
       if ((self.form[id].__dict__.has_key('display')) and (self.form[id].display is not None)):
          if isinstance(self.form[id].display,list):
             displays = self.form[id].display
          else:
             displays = [self.form[id].display,]
          for display in displays:
            try:
               display.off = 1
               self.eself.vcs[ self.eself.vcs_id ].remove_display_name( display.name )
            except Exception,err:
               if parent.show_debug: print err
               pass
          try:
             del self.form[id].display
          except Exception,err:
##              print err
             pass

       # Initialize parameters Page Layout form
       vcs = self.eself.vcs[ self.form[ id ].canvasid - 1 ] # Make sure to plot on the correct canvas
#       continents = vcs.continents                    
       data1 = self.form[id].data1.get()              # Get the first data set in the form
       data2 = self.form[id].data2.get()              # Get the second data set in the form
       template = self.form[id].template.get()        # Get the template used in the form
       gm_type = self.form[id].gm.type                # Get the graphics method type from form
       g_name = self.form[id].gm.get()                # Get the graphics method in the form
       bg = 0                                         # Set background graphics to "Off"
       if parent.panelGC.text_opt3.get() == 'Background Canvas': bg = 1 # Set background "On"
       p_val = string.atoi(self.form[id].priority.get()) # Get the plot priority level
       if isinstance(p_val, types.IntType):
          priority = p_val

       # Make sure that the data dimension size equals the required minimum graphics method size
       gm_data_size = 0
       if string.lower(gm_type) in ['boxfill', 'isofill', 'isoline', 'outfill', 'outline', 'scatter', 'vector']:
          gm_data_size = 2
       elif string.lower(gm_type) in ['xvsy', 'xyvsy', 'yxvsx']:
          gm_data_size = 1
       elif string.lower(gm_type) in ['meshfill']:
          gm_data_size = 3

       # Get the data name from the "Page Description Form"
       if (gm_type not in ['Continents']):
        if (gm_type.split()[0]=='Ext'):
           if (parent.panelDM.var3 is None) and (parent.panelDV.selected == []):
              d1 = __main__.__dict__[ data1 ]  # Get Data defined in Page Layout Form
           elif (parent.panelDM.var3 is not None) or (string.split( parent.panelDV.selected, ' ')[0] != data1):
              d1 = __main__.__dict__[ data1 ]  # Get Data defined in memory
           else:                               # Get Data that has been modified by the dimensions panel
              d1=gui_formulate.data( parent, var = parent.panelDV.lst1[ parent.panelDV.selected ])
           d2 = None
           for j in parent.panelDV.number_lst1.keys():
              if (string.split(j, ' ')[0] == data2):   # Match up the variable's dimensions
                 d2 = parent.panelDV.lst1[ j ]

        elif (gm_type not in ['Vector', 'Scatter', 'XvsY', "Meshfill"]) or (gm_type == "Meshfill" and data2 == ''): # Not Vector, Scatter, or XvsY graphics method
          if (parent.panelDM.var3 is None) and (parent.panelDV.selected == []):
             d1 = __main__.__dict__[ data1 ]  # Get Data defined in Page Layout Form
          elif (parent.panelDM.var3 is not None) or (string.split( parent.panelDV.selected, ' ')[0] != data1):
             d1 = __main__.__dict__[ data1 ]  # Get Data defined in memory
          else:                               # Get Data that has been modified by the dimensions panel
             d1=gui_formulate.data( parent, var = parent.panelDV.lst1[ parent.panelDV.selected ])
          d2 = None           
        else:  # Get the two data name defined in the "Page Description form"
           try:
              d1 = __main__.__dict__[ data1 ]  # Get the first defined variable
           except:
              gui_busy.busyEnd( self, parent )
              return
           for j in parent.panelDV.number_lst1.keys():
              if (string.split(j, ' ')[0] == data2):   # Match up the variable's dimensions
                 d2 = d1
                 d1 = parent.panelDV.lst1[ j ]
                 x_axis_store = d1.getAxis( -1 )
                 y_axis_store = d1.getAxis( -2 )
                 if gm_type!='Meshfill':
                    d1.setAxis( -1, d2.getAxis( -1 ) )
                    d1.setAxis( -2, d2.getAxis( -2 ) )
       else:
        d1 = None
        d2 = None

       # Plot the data accordingly
       try:
          status, self.form[id].display = vcs_function.plot(parent=parent, slab1=d1, slab2=d2,
                    template=template, g_name = g_name, g_type = gm_type, bg = bg, id = id)
       except Exception,err:
          status = None

       if (status in ['OK', 'break_off']):
          self.form[id].off_btn.create_image(0,0, anchor=Tkinter.NW, image=self.form[id].on_btnimage )
          self.form[id].lbl.configure( label_text = 'On' )

          if (gm_type.split()[0]=='Ext'):
             # Unset the busy mouse icon (i.e., change the mouse pointer back to an arrow)
             gui_busy.busyEnd( self, parent )
             return

          dn = parent.vcs[ parent.vcs_id ].return_display_names()
## Commented out C.Doutriaux never used...
##           fdn = {}
##           rdn = []
          sdn = []
          for i in parent.pl.form.keys():
             if ((self.form[i].__dict__.has_key('display')) and (self.form[i].display is not None)):
                if isinstance(self.form[i].display,list):
                   displays = self.form[i].display
                else:
                   displays = [self.form[i].display,]
##              fdn[i] = []
                for display in displays:
##                 if fdn[i]==[]:
##                    fdn[i]=[ self.form[i].canvasid, display.name ]
##                 else:
##                    fdn[i].append(display.name)
                   sdn.append(display.name)
                   for d in display.extradisplays:
                      sdn.append(d.name)
             
          for i in range(len(dn)):
             if dn[i] not in sdn:
##                 rdn.append( dn[i] )
                parent.vcs[ parent.vcs_id ].remove_display_name( dn[i] )

          if (status ==  'break_off'): self.form[id].gm.configure( label_text = "Yxvsx" )


       # Restore the X and Y axis selection
       if (gm_type in ['Vector', 'Scatter', 'XvsY']) or (gm_type == "Meshfill" and data2 != ''):   # Vector, Scatter, or XvsY graphics method
           if status is not None:
              d1.setAxis( -1, x_axis_store )
              d1.setAxis( -2, y_axis_store )

       # Get the form Canvas number and the Canvas id. Change the form Canvas
       # number to the Cavnas id if necearry
       canvas_id = str( parent.vcs[ parent.vcs_id ].canvasid() )
       form_id = self.form[id].lbln.cget( 'text' )[0]
       if canvas_id != form_id:
           self.form[id].lbln.configure( text = '%s.' % canvas_id )

       # Unset the busy mouse icon (i.e., change the mouse pointer back to an arrow)
       gui_busy.busyEnd( self, parent )

#---------------------------------------------------------------------
# End Variable Information Panel Layout
#---------------------------------------------------------------------
#
#---------------------------------------------------------------------
# End of File
#---------------------------------------------------------------------
