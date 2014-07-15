#!/usr/bin/env python
# Adapted for numpy/ma/cdms2 by convertcdms.py
#
# The VCS Page Layout GUI controls -  pagegui module
#
#################################################################################
#                                                                               #
# Module:       pagegui module                                                  #
#                                                                               #
# Copyright:    "See file Legal.htm for copyright information."                 #
#                                                                               #
# Authors:      PCMDI Software Team                                             #
#               Lawrence Livermore NationalLaboratory:                          #
#               support@pcmdi.llnl.gov                                          #
#                                                                               #
# Description:  PCMDI's VCS page description GUI browser and editor.            #
#                                                                               #
# Version:      4.0                                                             #
#                                                                               #
#################################################################################

#---------------------------------------------------------------------
# NOTE: need to use version of Python that imports Tkinter and Pmw
#---------------------------------------------------------------------
import Tkinter, Pmw, tkFileDialog
from tkMessageBox import askquestion
import os, string, sys, types
import __main__
import cdms2
from browser import gui_message, gui_output, gui_defined_variables
import gui_support
from gui_support import gui_color
# List of high-level graphics methods
graphics_method_list = ['Boxfill', 'Isofill', 'Isoline', 'Outfill', 'Outline', 'Vector',
                        'Scatter', 'XvsY', 'Xyvsy', 'Yxvsx', 'Continents']

#---------------------------------------------------------------------------------
#----------------------------------------------------------------------------------------
#
# Create the Tkinter/Pmw Page Layout editor interface
#
#----------------------------------------------------------------------------------------
#----------------------------------------------------------------------------------------
class PageDescriptionEditor:
    def __init__(self, canvas=None, gui_parent=None, continents=None):
        if canvas is None:
            import Canvas
            canvas = Canvas.Canvas()
        canvas.clear()
        self.vcs_legacy = canvas
        self.continents = continents
        title = "VCS Canvas Page Layout Editor"
        self.dialog= gui_support.VcsDialog( title = title, buttons = ())
        self.dialog.dialog.withdraw()
        self.mainframe = self.dialog.interior()
        max_w = gui_support.root().winfo_screenwidth()
        max_h = gui_support.root().winfo_screenheight()
        pane_pd = Pmw.PanedWidget( self.mainframe, 
                                   hull_width=max_w*0.44, 
                                   hull_height=max_w*0.42, 
                                   orient = 'vertical' 
                                 )
        pane_pd.add('PageForms', min=250, size = 250)
        pane_pd.add('PageBins', min=120, size = 450)
        pane_pd.pack( expand = 1, fill='both' )
        FRAME1 = Tkinter.Frame( pane_pd.pane( 'PageForms' ) )
        FRAME1.pack( side='top', pady=3, fill='both', expand=1 )
        FRAME2 = Tkinter.Frame( pane_pd.pane( 'PageBins' ) )
        FRAME2.pack( side='top', pady=3, fill='both', expand=1 )
        self.FRAME1 = FRAME1
        self.FRAME2 = FRAME2

        # create the page description main toplevel menu
        self.pmain_menu = Pmw.MenuBar(FRAME1,
                hull_relief = 'raised',
                hull_borderwidth = 2,
                balloon = gui_support.balloon
                )
        self.pmain_menu.pack(side='top', fill='both')

        #-------------------------------------------
        # menu 1 -- 'File'
        #-------------------------------------------
        self.create_file_menu (self.pmain_menu)

        #-------------------------------------------
        # menu 3 -- 'Options'
        #-------------------------------------------
        create_pd_options_menu( self, self.pmain_menu )

        #-------------------------------------------
        # menu 4 -- 'Help'
        #-------------------------------------------
        self.create_help_menu (self.pmain_menu)

        #-------------------------------------------
        # Page Layout Panel
        #-------------------------------------------
        self.pd = create_page_description_panel(self, FRAME1 )

        #-------------------------------------------
        # Page Layout Panel -- 'Default Page Form'
        #-------------------------------------------
        self.pd.create_form( 'default', 'Boxfill', 'default', None, vcs_legacy=self.vcs_legacy)

        #------------------------------------------------------------
        # Group the Template, Graphics Method and Data Scroll Lists
        #------------------------------------------------------------
        group_bins = Pmw.Group(FRAME2, tag_text = 'Select Template, Graphics Method, and Data')
        group_bins.pack(side='top', fill = 'both', expand = 1, padx = 5, pady = 5)

        #------------------------------------------------------------
        # Select Controls for the Template, Graphics Method and Data
        #------------------------------------------------------------
        self.bins = show_bin( group_bins, self.dialog )

        #------------------------------------------------------------
        # Show the Template, Graphics Method, and Data lists
        #------------------------------------------------------------
        self.bins.template( self.vcs_legacy )
        self.bins.graphics_method( self.vcs_legacy, 'Boxfill' )
        self.bins.data( self.vcs_legacy )
        # Decide where to put it on the screen
        if gui_parent is None:
            d=[max_w/4,
               max_h/4
              ]
        else:
            g=gui_parent.geometry()
            d = g.split('+')[1:]
        if gui_parent is not None:
           self.dialog.dialog.transient(gui_parent)
        self.dialog.geometry("+%s+%s"% (d[0],d[1]))
        self.dialog.dialog.deiconify()

    def create_file_menu(self, main_menu):
        main_menu.addmenu('File', "Exit VCS's Page Layout Editor", 
                           tearoff = 1)
        main_menu.addmenuitem('File', 'command', 'Exit',
                          label = "Exit",
                          command = self.dialog.destroy
                         )

    def create_help_menu( self, main_menu):
        main_menu.addmenu('Help', 'VCS Page Layout Help', side='right', 
                        tearoff = 1)
        gui_support.add_balloon_help (main_menu, 'Help')
        main_menu.addmenuitem('Help', 'separator')
        main_menu.addmenuitem('Help', 'command', 'Help About',
                            label = 'About the Page Layout Editor',
                            command = self.evt_about_dialog 
                           )

    def evt_about_dialog(self):
        Pmw.aboutversion(sys.prefix)
        Pmw.aboutcopyright(\
"""Copyright:    2001, Regents of the University of California
""")
        Pmw.aboutcontact(\
"""Go to cdat.sourceforge.net for documentation, support, bug reporting, and releases.
Program for Climate Model Diagnosis and Intercomparison
Lawrence Livermore National Laboratory Livermore, CA 94550 """)
        self.about = Pmw.AboutDialog(self.mainframe, 
                     applicationname = "The Visualization Control System's - (VCS) Page Layout Editor")

#
# Create the page description Options menu and its menu items
#
#----------------------------------------------------------------------------------------
#----------------------------------------------------------------------------------------
class create_pd_options_menu:
   def __init__( self, eself, main_menu ):
      main_menu.addmenu('Options', 'Set VCS Page Layout Preferences', tearoff = 1)

      main_menu.addmenuitem('Options', 'command', 'create new page description form',
                       label = 'Create Page Layout Form',
                       command = P_Command(self.evt_create_new_pd_form, eself)
                      )

#      main_menu.addmenuitem('Options', 'command', 'remove page description form',
#                       label = 'Remove Page Layout Form',
#                       command = P_Command(self.evt_remove_pd_form, eself)
#                      )

   def evt_create_new_pd_form( self, eself ):
        # find unique id number
        id_num = 0
#        print 'form = ', len(eself.pd.form), dir(eself.pd.form)
        for i in eself.pd.form.keys():
#           print 'esself id = ', eself.pd.form[i].id
           if id_num < eself.pd.form[i].id:
              id_num = eself.pd.form[i].id
        id_num += 1

#        print 'id_num = ', id_num

        eself.pd.create_form( 'default', 'Boxfill', 'default', None, id = id_num, 
                              vcs_legacy=eself.vcs_legacy )

   def evt_remove_pd_form( self, eself ):
#        print 'eself.pd.form 1= ', eself.pd.form
        for i in eself.pd.form.keys():
           eself.pd.form[i].line.destroy()
           eself.pd.form[i].separator.destroy()
           del eself.pd.form[i]

#        print 'eself.pf.form 2= ', eself.pd.form

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
   def __init__( self, eself, parent ):
        self.parent = parent
        self.eself = eself
        self.scl_frame = Pmw.ScrolledFrame( self.parent,
                labelpos = 'nw',
                label_text = 'Page Layout Forms:',
                )
        self.scl_frame.pack( side='top', fill='both' )
        gui_support.balloon.bind(self.scl_frame, " The Page Layout Editor is used to specify picture descriptor\n forms that define how plots are displayed on the\n VCS Canvas (drawing area). There are three primary\n objects in each picture descriptor form that correspond\n to text windows of the template (in green), the\n graphics method (in blue), and the data (in red).\n Names are assigned to these text windows from the\n scroll windows below the 'Page Layout Forms'.\n These scroll windows include a green `Template'\n window, a blue `Graphics Method' window, and a red\n `Data' window.")

        # Keep track of created forms
        self.form={}

   # Create page description form
   def create_form( self, template='default', graphics_method='Boxfill', graphics_name='default', data1=None, data2=None, data3=None, id = 1, vcs_legacy=None ):

        form = Form()
        self.form[id] = form
        self.form[id].display = None

        form.line=Tkinter.Frame(self.scl_frame.interior())
        form.line.pack(side='top', fill='both')

        # Set the unique id number
        form.id = id

        form.lblc = Pmw.LabeledWidget(form.line,
                    labelpos = 'n',
                    label_text = 'Canvas')
        form.lblc.pack( side='left', padx = 5 )
        form.lbln = Tkinter.Label(form.lblc.interior(),
                text = '%d.' % vcs_legacy.canvasid())
        form.lbln.pack(fill='x', expand=1, padx=10, pady=5)
        gui_support.balloon.bind(form.lblc, "Set VCS Canvas ID from main VCDAT 'Plot Output\nDestination' choice button.")

        form.priority = Pmw.EntryField(form.line,
                labelpos = 'n',
                label_text = 'P',
                entry_width = 3,
                value = '0',
                validate = None,
                command = P_Command(self.evt_change_priority, id)
                )
        form.priority.pack( side='left', padx = 5)
        gui_support.balloon.bind(form.priority, "Enter priority level for plot.")

        form.lbl = Pmw.LabeledWidget(form.line,
                    labelpos = 'n',
                    label_text = 'Err'
                    )
        form.lbl.pack( side='left', padx = 5 )
        form.btn = Tkinter.Button(form.lbl.interior(),
            background = 'red',
            height = 1,
            command = P_Command(self.evt_status_button, id)
            )
        form.btn.pack(fill='both')
        gui_support.balloon.bind(form.lbl, "Select button to remove or redraw plot.")

        form.template = Pmw.EntryField(form.line,
                labelpos = 'n',
                label_text = 'Template',
                entry_width = 15,
                entry_background = 'green',
                entry_foreground = 'black',
                value = template,
                validate = None)
        form.template.pack( side='left', padx = 5)
        form.template.component('entry').bind('<ButtonPress>', P_Command(self.evt_replace_template, 1, id))
        form.template.component('entry').bind('<ButtonRelease>', P_Command(self.evt_replace_template, 0, id))
        form.template.component('entry').bind('<KeyPress>', P_Command(self.evt_replace_template, 0, id))
        gui_support.balloon.bind(form.template, "Set entry from green 'Template' section below.")

        form.gm = Pmw.EntryField(form.line,
                labelpos = 'n',
                label_text = graphics_method,
                entry_width = 15,
                entry_background = 'blue',
                entry_foreground = 'white',
                value = graphics_name,
                validate = None)
        form.gm.pack( side='left', padx = 5)
        form.gm.component('entry').bind('<ButtonPress>', P_Command(self.evt_replace_gm,1,id))
        form.gm.component('entry').bind('<ButtonRelease>', P_Command(self.evt_replace_gm,0,id))
        form.gm.component('entry').bind('<KeyPress>', P_Command(self.evt_replace_gm,0,id))
        form.gm.type = graphics_method
        gui_support.balloon.bind(form.gm, "Set entry from blue 'Graphics Method' section below.")

        label_txt = 'A(X,Y)'
        if graphics_method in ['Boxfill']:
           label_txt = 'A(X,Y)'
        data1_txt = ''
        if data1 is not None:
           data1_txt = data1
        form.data1 = Pmw.EntryField(form.line,
                labelpos = 'n',
                label_text = label_txt,
                entry_width = 15,
                entry_background = 'red',
                entry_foreground = 'white',
                value = data1_txt,
                validate = None)
        form.data1.pack( side='left', padx = 5)
        form.data1.component('entry').bind('<ButtonPress>', P_Command(self.evt_replace_data,1,id))
        form.data1.component('entry').bind('<ButtonRelease>', P_Command(self.evt_replace_data,0,id))
        form.data1.component('entry').bind('<KeyPress>', P_Command(self.evt_replace_data,0,id))
        gui_support.balloon.bind(form.data1, "Set entry from red 'Data' section below.")

        form.remove = Pmw.LabeledWidget(form.line,
                    labelpos = 'n',
                    label_text = 'Remove')
        form.remove.pack( side='left', padx = 5 )
        form.btn2 = Tkinter.Button(form.remove.interior(),
            background = gui_color.one,
            height = 1,
            command = P_Command(self.evt_remove_form, id)
            )
        form.btn2.pack(fill='both')


#        form.data2 = Pmw.EntryField(form.line,
#                labelpos = 'n',
#                label_text = 'A(X,Y)',
#                entry_width = 15,
#                entry_background = 'red',
#                entry_foreground = 'white',
#                validate = None)
#        form.data2.pack( side='left', padx = 5)
#        form.data2.component('entry').bind('<ButtonPress>', P_Command(self.evt_replace_data,1,id))
#        form.data2.component('entry').bind('<ButtonRelease>', P_Command(self.evt_replace_data,0,id))
#        form.data2.component('entry').bind('<KeyPress>', P_Command(self.evt_replace_data,0,id))
#
#        form.data3 = Pmw.EntryField(form.line,
#                labelpos = 'n',
#                label_text = 'A(X,Y)',
#                entry_width = 15,
#                entry_background = 'red',
#                entry_foreground = 'white',
#                validate = None)
#        form.data3.pack( side='left', padx = 5)
#        form.data3.component('entry').bind('<ButtonPress>', P_Command(self.evt_replace_data,1,id))
#        form.data3.component('entry').bind('<ButtonRelease>', P_Command(self.evt_replace_data,0,id))
#        form.data3.component('entry').bind('<KeyPress>', P_Command(self.evt_replace_data,0,id))

        form.separator = Tkinter.Frame ( self.scl_frame.interior(), relief = 'raised',
                    height = 4, width = 820, borderwidth = 2, background="MEDIUM ORCHID")
        form.separator.pack(side = 'top' )


   def evt_change_priority( self, id ):
      p_val = string.atoi(self.form[id].priority.get())
      if ((isinstance(p_val, types.IntType)) and (self.form[id].display is not None)):
         if (self.form[id].display.priority != p_val):
            self.do_plot( id )

   def evt_status_button( self, id ):
      if (self.form[id].display is not None):
         if self.form[id].display.off == 0:
            self.form[id].display.off = 1
            self.form[id].btn.configure( background = 'yellow')
            self.form[id].lbl.configure( label_text = 'Off' )
         else:
            self.form[id].display.off = 0
            self.form[id].btn.configure( background = gui_color.seven)
            self.form[id].lbl.configure( label_text = 'On' )

            # Draw continental outlines if specified
            tv = __main__.__dict__[ self.form[id].data1.get() ]
            rank = len( tv.shape )
            xdim = rank-1
            ydim = rank-2
            contout = self.eself.continents
            if contout is None:
                if xdim>=0 and ydim>=0 and tv.getAxis(xdim).isLongitude() and tv.getAxis(ydim).isLatitude():
                    contout = 1
                else:
                    contout = 0
            if (xdim>=0 and ydim>=0 and (contout>=1) and (contout<12)):
                self.eself.vcs_legacy.setcontinentstype( contout )
            else:
                self.eself.vcs_legacy.setcontinentstype( 0 )

         vcs_legacy = self.eself.vcs_legacy.update()
      

   def evt_replace_template( self, pe, id, event ):
      if pe == 1:
         if self.eself.bins.bin_type == 'Template':
               self.form[id].template.setentry( self.eself.bins.bin_name )
         self.do_plot( id )
      return "break"

   def evt_replace_gm( self, pe, id, event ):
      if pe == 1:
         if self.eself.bins.bin_type in graphics_method_list:
               self.form[id].gm.setentry( self.eself.bins.bin_name )
               self.form[id].gm.configure( label_text = self.eself.bins.bin_type )
               self.form[id].gm.type = self.eself.bins.bin_type
         self.do_plot( id )
      return "break"

   def evt_replace_data( self, pe, id, event ):
      if pe == 1:
         if self.eself.bins.bin_type == 'Data':
           try:
             self.form[id].data1.setentry( string.split(self.eself.bins.bin_name," ")[0] )
           except:
             pass
         self.do_plot( id )
      return "break"

   def evt_remove_form( self, id ):
      if self.form[id].display is not None:
         self.form[id].display.off = 1
         vcs_legacy = self.eself.vcs_legacy.update()
      self.form[id].line.destroy()
      self.form[id].separator.destroy()
      del self.form[id]

   def do_plot( self, id ):
       vcs_legacy = None
       priority = 0
       data1=None
       template=None
       gm_type=None
       gm=None
       vcs_legacy = self.eself.vcs_legacy
       continents = self.eself.continents
       data1 = self.form[id].data1.get()
       template = self.form[id].template.get()
       gm_type = self.form[id].gm.type
       gm = self.form[id].gm.get()

       p_val = string.atoi(self.form[id].priority.get())
       if isinstance(p_val, types.IntType):
          priority = p_val

       gm_min_size = 0
       if string.lower(gm_type) in ['boxfill', 'isofill', 'isoline', 'outfill', 'outline', 'scatter', 'vector']:
          gm_min_size = 2
       elif string.lower(gm_type) in ['xvsy', 'xyvsy', 'yxvsx']:
          gm_min_size = 1
       try:
          data_size = len(__main__.__dict__[ data1 ].shape)
          if data_size < gm_min_size:
            gui_message.error( "The variable's number of dimensions must be greater than %d." % (gm_min_size-1) )
            self.form[id].data1.setentry( '' )
            vcs_legacy.clear()
            self.form[id].btn.configure( background = 'red')
            self.form[id].lbl.configure( label_text = 'Err' )
            if (self.form[id].display is not None):
               del self.form[id].display
               self.form[id].display = None
            return
       except:
           pass

       plot_flg = 0
       if (vcs_legacy is not None): plot_flg += 1
       if (len(data1) > 0): plot_flg += 1
       if (len(template) > 0): plot_flg += 1
       if (len(gm_type) > 0): plot_flg += 1
       if (len(gm) > 0): plot_flg += 1
       if plot_flg == 5:
          if self.form[id].display != None:
            self.form[id].display.off = 1
            self.form[id].display.priority = priority
          self.form[id].display = vcs_legacy.plot( __main__.__dict__[ data1 ], template, gm_type, gm, continents=continents)
          self.form[id].btn.configure( background = gui_color.seven)
          self.form[id].lbl.configure( label_text = 'On' )
#          print 'display = ', dir(self.form[id].display)
#          print 'display name = ', self.form[id].display.name
#          print 'display array = ', self.form[id].display.array
#          print 'display g_name = ', self.form[id].display.g_name
#          print 'display g_type = ', self.form[id].display.g_type
#          print 'display off = ', self.form[id].display.off
#          print 'display priority = ', self.form[id].display.priority
#          print 'display template = ', self.form[id].display.template
#          print 'display s_name = ', self.form[id].display.s_name


#----------------------------------------------------------------------
#----------------------------------------------------------------------
#
# Show the VCS "Templates" .
#
#----------------------------------------------------------------------
#----------------------------------------------------------------------
class show_bin:
   def __init__( self, group_bins, dialog ):
      self.fm=Tkinter.Frame(group_bins.interior())
      self.fm.pack(side='top')
      self.bin_type = None
      self.bin_name = None
      self.dialog = dialog

   def template( self, vcs_legacy ):
      #
      framet = Tkinter.Frame(self.fm)
      framet.pack(side='left', expand=1, fill='both')
      self.template_button = Tkinter.Button(framet,
                text = 'Template',
                relief = 'raised',
                borderwidth = 2,
                command = P_Command(self.edit_template, vcs_legacy)
                )
      self.template_button.pack(side='top', fill='both', expand = 1)
      gui_support.balloon.bind(self.template_button, 'Click to edit a template')
      #
      template_list = vcs_legacy.listelements('template')
      self.template_listbox = Pmw.ScrolledListBox(framet,
                label_text = 'Select Template:',
                labelpos = 'nw',
                items = template_list,
                listbox_background = 'green',
                listbox_foreground='black',
                selectioncommand = self.select_template
           )
      self.template_listbox.pack(side='top', fill = 'both', expand=1)
      gui_support.balloon.bind(self.template_listbox, "Select 'Template' name with the left mouse button.\nMove pointer to the green 'Template' area of the\n'Page Layout Form' and depress the left mouse\nbutton. The new template name will replace the\nold name.")

   def graphics_method( self, vcs_legacy, gm_name ):
      frameg=Tkinter.Frame(self.fm)
      frameg.pack(side='left', fill ='both', padx = 5)
      self.gm_name = gm_name

      # create the graphics method menu and scroll list
      #
      self.gmain_menu = Pmw.MenuBar(frameg,
                hull_relief = 'raised',
                hull_borderwidth = 2,
                balloon = gui_support.balloon
                )
      self.gmain_menu.pack(side='top', fill='both', expand = 1)
      self.gmain_menu.addmenu('Graphics Method        ', "Graphics Method Options/Preferences", tearoff = 1)
      #
      # Create the cascade "Graphics Method" options
      self.gmain_menu.addcascademenu('Graphics Method        ', 'Select Graphics Method', 'select graphics method',
                       label = 'Select Graphics Method',
                       traverseSpec = 'z', tearoff = 1)
      for x in graphics_method_list:
         self.gmain_menu.addmenuitem('Select Graphics Method', 'command','View graphics methods',
                          label = x,
                          command = P_Command(self.evt_select_graphics_method, x, vcs_legacy)
                         )
      self.gmain_menu.addmenuitem('Graphics Method        ', 'command', 'rename',
                          label = "Edit",
                          command = P_Command(self.edit_graphics_method, vcs_legacy)
                         )
      gm_list = vcs_legacy.listelements( self.gm_name )
      self.gm_name_obj = Tkinter.StringVar()
      self.gm_name_obj.set( 'Select ' + self.gm_name + ':' )
      self.gm_listbox = Pmw.ScrolledListBox(frameg,
                #label_text = self.gm_name_obj,
                label_textvariable = self.gm_name_obj,
                labelpos = 'nw',
                items = gm_list,
                listbox_background = 'blue',
                listbox_foreground='white',
                selectioncommand = self.select_gm
           )
      self.gm_listbox.pack(side='top', fill = 'both', expand=1)
      gui_support.balloon.bind(self.gm_listbox, "Select 'Graphics Method' name with the left mouse\nbutton. Move pointer to the blue 'Graphics Method'\narea of the 'Page Layout Form' and depress the\nleft mouse button. The new graphics method name\nwill replace the old name.")

   def data( self, vcs_legacy ):
      framed=Tkinter.Frame(self.fm)
      framed.pack(side='left', fill ='both', padx = 5)

      # create the data menu and scroll list
      #
      self.dmain_menu = Pmw.MenuBar(framed,
                hull_relief = 'raised',
                hull_borderwidth = 2,
                balloon = gui_support.balloon
                )
      self.dmain_menu.pack(side='top', fill='both', expand = 1)
      self.dmain_menu.addmenu('Data                           ', "Data Options/Preferences", tearoff = 1)
      #
      # Create the cascade "Data" options
      self.dmain_menu.addmenuitem('Data                           ', 'command', 'rename',
                          label = "Update Data List",
                          command = self.update_data_list
                         )
#      self.dmain_menu.addmenuitem('Data', 'command', 'rename',
#                          label = "Rename"
#                          #command = root.destroy
#                         )
#      self.dmain_menu.addmenuitem('Data', 'command', 'copy',
#                          label = "Copy"
#                          #command = root.destroy
#                         )
#      self.dmain_menu.addmenuitem('Data', 'command', 'remove',
#                          label = "Remove"
#                          #command = root.destroy
#                         )
#      self.dmain_menu.addmenuitem('Data', 'command', 'save as',
#                          label = "Save as..."
#                          #command = root.destroy
#                         )
      self.data_listbox = Pmw.ScrolledListBox(framed,
                label_text = 'Select Data:',
                labelpos = 'nw',
                items = return_defined_data_list(),
                listbox_background = 'red',
                listbox_foreground='white',
                selectioncommand = self.select_data
           )
      self.data_listbox.pack(side='top', fill = 'both', expand=1)
      gui_support.balloon.bind(self.data_listbox, "Select 'Data' name with the left mouse button.\nMove pointer to the red 'Data' area of the 'Page\nLayout Form' and depress the left mouse button.\nThe new data name will replace the old name.")

   def select_template(self):
       self.bin_type = 'Template'
       try:
          self.bin_name = self.template_listbox.getcurselection()[0]
       except:
          pass

   def select_gm(self):
       self.bin_type = self.gm_name
       try:
          self.bin_name = self.gm_listbox.getcurselection()[0]
       except:
          pass

   def select_data(self):
       self.bin_type = 'Data'
       try:
          self.bin_name = self.data_listbox.getcurselection()[0]
       except:
          pass

   def evt_select_graphics_method(self, gm_name, vcs_legacy):
       self.gm_name = gm_name
       self.gm_name_obj.set( 'Select ' + self.gm_name + ':' )
       gm_list = vcs_legacy.listelements( self.gm_name )
       self.gm_listbox.setlist( gm_list )

   def edit_graphics_method(self, vcs_legacy):
      try:
         gm_selected = self.gm_listbox.getcurselection()[0]
      except:
         gui_message.error( 'Must select a %s graphics method from the list below.' % self.gm_name )
         return
      vcs_legacy.graphicsmethodgui(self.gm_name, gm_selected,
            gui_parent = self.dialog.dialog) # popup the graphics method gui

   def edit_template (self, vcs_legacy):
      s = self.template_listbox.getcurselection()
      template_selected = ''
      if len(s) > 0:
          template_selected = s[0]
      vcs_legacy.templateeditor(template_selected, gui_parent=self.dialog.dialog)

   def update_data_list(self):
      self.data_listbox.setlist( return_defined_data_list() )

#---------------------------------------------------------------------------------
#---------------------------------------------------------------------------------
#
# Event handling function that will allow the passing of arguments
#
#---------------------------------------------------------------------------------
#---------------------------------------------------------------------------------
class P_Command:
   def __init__(self, func, *args, **kw):
      self.func = func
      self.args = args
      self.kw = kw

   def __call__(self, *args, **kw):
      args = self.args + args
      kw.update(self.kw)
      return apply(self.func, args, kw)

# Find the data that is defined
def return_defined_data_list():
      tv = __main__.__dict__
      trash_str = gui_defined_variables.trash_str
      data_list = []
      for x in tv.keys():
#         if ( ( isinstance( tv[x] , numpy.ndarray) ) or ( cdms2.isVariable( tv[x] ) ) or
#            ( MV2.isMA( tv[x] ) ) ):
         if ( cdms2.isVariable( tv[x] ) ):

            str_var = x + ' ('
            for i in range(len(tv[x].shape)):
               s ="%d" % tv[x].shape[i]

               str_var = str_var + s + ', '

            if len (tv[x]) > 0:
               str_var = str_var[0:len(str_var)-2]
            str_var = str_var + ')'

            a = string.split( str_var, ' ')[0]
            if a[-len(trash_str):] != trash_str:
               if str_var[-2:] == '()': str_var = str_var + (' = %.17g' % tv[ x ])
               data_list.append( str_var )

      data_list.sort()

      return data_list

#####################################################################################
# Create/Popup page description GUI editor for VCS                                  #
#####################################################################################
def create(canvas=None, gui_parent=None):
    PageDescriptionEditor(canvas, gui_parent)
#---------------------------------------------------------------------
# End of File
#---------------------------------------------------------------------
