# Adapted for numpy/ma/cdms2 by convertcdms.py
import sys, os, time
import __main__
sys.path[:0] = ['../../..']

import Tkinter, Pmw, cdms2, vcs_legacy, sys, os, string, types
import Tkinter, Pmw, tkFileDialog
from tkMessageBox import showerror
from gui_support import gui_color
from browser import gui_control, gui_busy
import gui_support
from browser import gui_message
import gui_template_editor as _gui_template_editor

# Plot annotation dictionary
plot_kw={} # dictionary of keywords arguments
last_tab = "Animation"

zoom_ct = 1
pan_hori_ct = 0
pan_vert_ct = 0
global_xticlabels = '*'
global_xmtics = '*'
global_yticlabels = '*'
global_ymtics = '*'

def rgb2hex(r,g,b):
    return '#%02X%02X%02X'%(r,g,b)
winColor = rgb2hex(124, 204, 74)

class CanvasGUI:
    def __init__(self, canvas=None, top_parent=None, got_root=0):
        # I know the below "global" looks SICK, but when you drop the last reference to 
        # an Tkinter.Image object, then the image is destroyed and thus not displayed.
        global img1, img2, img3, img4, img5, img6, img7
        global zoom_img, unzoom_img, panup_img, pandown_img, panleft_img, panright_img

        self.dialog = gui_support.VcsDialog(buttons=(),
                                            command = gui_control.Command(self.close_VCS_Canvas, canvas)
                                            )
        parent = self.dialog.interior()
        self.parent = parent

        # Create a stopwatch curser
        top_parent.busyCursor = 'watch'
        top_parent.preBusyCursors = None

        self.top_parent = top_parent
        self.called_from_vcdat = 1
        try : self.top_parent.menu.vcs_legacy_canvas_gui_settings_flg
        except: self.called_from_vcdat = 0
        self.canvas = canvas

        # Animation flags and counters
        self.create_flg = 0
        self.nofs = 0
        self.frame_count = 0
        self.frame_position = 1
        self.animation_gui = None
        self.animation_gui_first_time = 0
        self.animated_dimension = "3rd"

        # Shell command objects and counters
        self.history = []
        self.histact = 0
        self.lastcommand = ""

        # Set data, template, and graphics method selections for VCDAT; the actural 
        # values are set in ../../browser/vcs_legacy_function.py
        self.slab1 = None
	self.slab2 = None
	self.template = None
	self.g_type = None
	self.g_name = None


        self.dialog.dialog.withdraw()

	# Create and pack the MenuBar.
	menuBar = Pmw.MenuBar(parent,
		hull_relief = 'raised',
		hull_borderwidth = 1,
                balloon = gui_support.balloon)
	menuBar.pack(fill = 'x')
	self.menuBar = menuBar

	# Add some buttons to the MenuBar.
#	menuBar.addmenu('VCScanvas', 'Cut, copy or paste' )
#	menuBar.addmenuitem('VCScanvas', 'command', 'Delete the current selection',
#		#command = PrintOne('Action: delete'),
#		label = 'Delete')

	menuBar.addmenu('File', 'Save the plot or close the VCS Canvas')
	menuBar.addmenuitem('File', 'command', 'Close this window',
		#command = PrintOne('Action: close'),
		label = 'Save As...',
                command = gui_control.Command(self.evt_save_plot_to_file, parent, canvas))
	menuBar.addmenuitem('File', 'separator')
	menuBar.addmenuitem('File', 'command', 'Exit the application',
		command = gui_control.Command(self.close_VCS_Canvas, canvas, None),
		label = 'Close VCS Canvas')

# DEAN        parent.protocol("WM_DELETE_WINDOW", gui_control.Command(self.close_VCS_Canvas, canvas))

#	menuBar.addmenu('View', 'Cut, copy or paste' )
#	menuBar.addmenuitem('View', 'command', 'Delete the current selection',
#		#command = PrintOne('Action: delete'),
#		label = 'Delete')

        # Set the Preferences options
	menuBar.addmenu('Preferences', 'Set the VCS Canvas GUI Preferences')
        menuBar.addcascademenu('Preferences', 'Orientation',
                              'Page Orientation',
                              label = 'Page Orientation',
                              traverseSpec = 'z', tearoff = 1
                             )
        self.page_landscape = Tkinter.IntVar()
        self.page_orientation_flg = 1
        self.page_landscape.set( 1 ) 
        menuBar.addmenuitem('Orientation', 'checkbutton', 'Landscape',
                       selectcolor=gui_color.one,
                       label = 'Landscape',
                       variable = self.page_landscape,
                       command = gui_control.Command(self.evt_page_orientation, parent, canvas, "landscape")
                       )
        self.page_portrait = Tkinter.IntVar()
        self.page_portrait.set( 0 )
        menuBar.addmenuitem('Orientation', 'checkbutton', 'Portrait',
                       label = 'Portrait',
                       selectcolor=gui_color.one,
                       variable = self.page_portrait,
                       command = gui_control.Command(self.evt_page_orientation, parent, canvas, "portrait")
                       )


	menuBar.addmenu('Help', 'VCScanvas Help',side='right')
        gui_support.add_balloon_help(menuBar, "Help")

	# Create and pack the NoteBook.
        bigframe = Tkinter.Frame(parent )
        bigframe.pack(side='top', fill = 'both', expand=1)

	# Create big frame
        self.frame = Tkinter.Frame(bigframe, container=True, width=767, height=582)
        self.frame.grid(column=0, row=0, rowspan=8, sticky='nsew')

        self.notebook = Pmw.NoteBook(bigframe,raisecommand = gui_control.Command(self.evt_tab, parent, canvas))
        self.notebook.grid(column=0, row=9, rowspan=2, sticky='nsew')

        # Make sure that the Canvas and the Notebook are resized proportionally
        bigframe.grid_rowconfigure(0, weight=1)
        bigframe.grid_rowconfigure(1, weight=1)
        bigframe.grid_rowconfigure(2, weight=1)
        bigframe.grid_rowconfigure(3, weight=1)
        bigframe.grid_rowconfigure(4, weight=1)
        bigframe.grid_rowconfigure(5, weight=1)
        bigframe.grid_rowconfigure(6, weight=1)
        bigframe.grid_rowconfigure(7, weight=1)
        bigframe.grid_rowconfigure(8, weight=1)
        bigframe.grid_rowconfigure(9, weight=1)
        bigframe.grid_rowconfigure(10, weight=1)
	bigframe.grid_columnconfigure(0, weight=1)

        # Add the "data and plot info" page to the notebook.
        #if (self.called_from_vcdat != 1): # Not called VCDAT
        page = self.notebook.add('Data & Plot Info')
        self.notebook.tab('Data & Plot Info').focus_set()
        self.data_plot_tab = data_plot_info_tab_gui( self, page, parent, canvas )

        # Add the "Shell" page to the notebook.
        page = self.notebook.add('Command Line')
        self.command_line_tab = command_line_tab_gui( self, page, parent, canvas )

        # Add template pages.
        page = self.notebook.add('Edit Plot')
        self.template_tab = template_tab_gui( self, page, parent, canvas )

        # Get the display names
        if (self.called_from_vcdat != 1): # Not called from VCDAT
           page = self.notebook.add("Graphics Method")
           self.gm_tab = graphics_method_tab_gui( self, page, parent, canvas )

#        page = self.notebook.add('Plot Options')

        # Add the primatives as needed
        page = self.notebook.add('Primitives')
        self.primitive_tab = primitive_tab_gui( self, page, parent, canvas )

        # Add the "Animation" page to the notebook.
        if (got_root == 0):
           page = self.notebook.add('Animation')

           frame1 = Tkinter.Frame(page )
           frame1.pack(side='top', fill = 'both', expand=1)
   
           self.timelabel = Tkinter.Label(frame1, text="%s 1:1 [1 step]: " % (self.animated_dimension))
           self.timelabel.pack(side='left')

           self.framescale = Tkinter.Scale( frame1, orient = 'horizontal', troughcolor = winColor, from_ = 1, to = 1)
           self.framescale.configure(command = gui_control.Command(self.evt_framescale, parent, canvas))
           self.framescale.pack(side='left', fill='x', expand=1, padx=5, pady=5 )
           gui_support.balloon.bind( self.framescale, "Stop the animation first, then drag to select a time slice.")

           frame3 = Tkinter.Frame( frame1, background = winColor, borderwidth = 2, relief = 'sunken' )
           frame3.pack(side='left', padx=10, pady=5)

           '''
           self.more_animation_controls = Tkinter.Button(frame3, text = 'Save',
	        command = gui_control.Command(self.show_animation_gui, parent, canvas)
                )
           self.more_animation_controls.pack(side='top', padx = 2, pady = 2)
           gui_support.balloon.bind( self.more_animation_controls, "Save animation to a raster file.")

           self.more_animation_controls = Tkinter.Button(frame3, text = 'Load',
	        command = gui_control.Command(self.show_animation_gui, parent, canvas)
                )
           self.more_animation_controls.pack(side='top', padx = 2, pady = 2)
           gui_support.balloon.bind( self.more_animation_controls, "Load a created animation file.")
           '''

	   self.animation_speed = Pmw.Counter(frame3, labelpos = 'w', label_text = 'Slow Speed', 
                                           entryfield_validate = {'validator' : 'integer', 'min' : '0', 'max' : '10'},
                                           entryfield_modifiedcommand = gui_control.Command( self.evt_animation_speed, parent, canvas ),
                                           entryfield_value = 0,
                                           entryfield_entry_width = 3, entry_background = winColor,
                                           entry_foreground = 'black', increment = 1)
	   self.animation_speed.pack(side='top', padx=10, pady=5)
           gui_support.balloon.bind( self.animation_speed, "Pause the time (1/10 sec.) between each frame while the animation is running.")

           #frame3b = Tkinter.Frame( frame3, background = winColor, borderwidth = 1, relief = 'sunken' )
           frame3b = Tkinter.Frame( frame3, background = winColor, borderwidth = 1, relief = 'sunken' )
           frame3b.pack(side='top', padx=0, pady=2)

           zoom_img = Tkinter.PhotoImage( master = frame3b, file = os.path.join(vcs_legacy.__path__[0],'..','..','..','..', 'bin', 'viewmag+.gif') )
	   self.zoom_btn = Tkinter.Button(frame3b, image = zoom_img, command=gui_control.Command(self.evt_zoom, parent, canvas, 1))
           self.zoom_btn.pack(side='left', fill='x', expand=1, padx=0, pady=0 )
           gui_support.balloon.bind( self.zoom_btn, "Zoom In")

           unzoom_img = Tkinter.PhotoImage( master = frame3b, file = os.path.join(vcs_legacy.__path__[0],'..','..','..','..', 'bin', 'viewmag-.gif') )
	   self.unzoom_btn = Tkinter.Button(frame3b, image = unzoom_img, command=gui_control.Command(self.evt_zoom, parent, canvas, -1))
           self.unzoom_btn.pack(side='left', fill='x', expand=1, padx=0, pady=0 )
           gui_support.balloon.bind( self.unzoom_btn, "Zoom Out")

           panup_img = Tkinter.PhotoImage( master = frame3b, file = os.path.join(vcs_legacy.__path__[0],'..','..','..','..', 'bin', 'pan_up.gif') )
	   self.panup_btn = Tkinter.Button(frame3b, image = panup_img, command=gui_control.Command(self.evt_pan_vert, parent, canvas, 20))
           self.panup_btn.pack(side='left', fill='x', expand=1, padx=0, pady=0 )
           gui_support.balloon.bind( self.panup_btn, "Pan Up")

           pandown_img = Tkinter.PhotoImage( master = frame3b, file = os.path.join(vcs_legacy.__path__[0],'..','..','..','..', 'bin', 'pan_down.gif') )
	   self.pandown_btn = Tkinter.Button(frame3b, image = pandown_img, command=gui_control.Command(self.evt_pan_vert, parent, canvas, -20))
           self.pandown_btn.pack(side='left', fill='x', expand=1, padx=0, pady=0 )
           gui_support.balloon.bind( self.pandown_btn, "Pan Down")

           panleft_img = Tkinter.PhotoImage( master = frame3b, file = os.path.join(vcs_legacy.__path__[0],'..','..','..','..', 'bin', 'pan_left.gif') )
	   self.panleft_btn = Tkinter.Button(frame3b, image = panleft_img, command=gui_control.Command(self.evt_pan_hori, parent, canvas, -20))
           self.panleft_btn.pack(side='left', fill='x', expand=1, padx=0, pady=0 )
           gui_support.balloon.bind( self.panleft_btn, "Pan Left")

           panright_img = Tkinter.PhotoImage( master = frame3b, file = os.path.join(vcs_legacy.__path__[0],'..','..','..','..', 'bin', 'pan_right.gif') )
	   self.panright_btn = Tkinter.Button(frame3b, image = panright_img, command=gui_control.Command(self.evt_pan_hori, parent, canvas, 20))
           self.panright_btn.pack(side='left', fill='x', expand=1, padx=0, pady=0 )
           gui_support.balloon.bind( self.panright_btn, "Pan Right")

           frame2 = Tkinter.Frame(page )
           frame2.pack(side='top', fill='x', expand=1)

           img1 = Tkinter.PhotoImage( master = frame2, file = os.path.join(vcs_legacy.__path__[0],'..','..','..','..', 'bin', '2leftarrow.gif') )
	   self.firstframe = Tkinter.Button(frame2, image = img1, command=gui_control.Command(self.evt_firstframe, parent, canvas))
           self.firstframe.pack(side='left', fill='x', expand=1, padx=5, pady=5 )
           gui_support.balloon.bind( self.firstframe, "First")
           self.firstframe.configure(state = 'disabled')
   
           img2 = Tkinter.PhotoImage( file = os.path.join(vcs_legacy.__path__[0],'..','..','..','..', 'bin', 'player_start.gif') )
	   self.stepback = Tkinter.Button(frame2, image = img2, command=gui_control.Command(self.evt_stepback, parent, canvas))
           self.stepback.pack(side='left', fill='x', expand=1,  padx=5, pady=5 )
           gui_support.balloon.bind( self.stepback, "Step Back")
           self.stepback.configure(state = 'disabled')
   
           img3 = Tkinter.PhotoImage( file = os.path.join(vcs_legacy.__path__[0],'..','..','..','..', 'bin', 'player_rev.gif') )
	   self.playback = Tkinter.Button(frame2, image = img3, command=gui_control.Command(self.evt_play, parent, canvas, 2))
           self.playback.pack(side='left',  fill='x', expand=1, padx=5, pady=5 )
           gui_support.balloon.bind( self.playback, "Play Back")
   
           self.img4 = Tkinter.PhotoImage( file = os.path.join(vcs_legacy.__path__[0],'..','..','..','..', 'bin', 'player_stop.gif') )
	   self.pause = Tkinter.Button(frame2, image = self.img4, command=gui_control.Command(self.evt_stop, parent, canvas, 0))
           self.pause.pack(side='left',  fill='x', expand=1, padx=5, pady=5 )
           gui_support.balloon.bind( self.pause, "Stop/Pause")
           self.img4a = Tkinter.PhotoImage( file = os.path.join(vcs_legacy.__path__[0],'..','..','..','..', 'bin', 'player_pause.gif') )
#	self.pause = Tkinter.Button(frame2, image = img4, command=gui_control.Command(self.evt_pause, parent, canvas))
#        self.pause.pack(side='left',  fill='x', expand=1, padx=5, pady=5 )
#        self.pause.configure(state = 'disabled')

           img5 = Tkinter.PhotoImage( file = os.path.join(vcs_legacy.__path__[0],'..','..','..','..', 'bin', 'player_play.gif') )
	   self.playforward = Tkinter.Button(frame2, image = img5, command=gui_control.Command(self.evt_play, parent, canvas, 1))
           self.playforward.pack(side='left',  fill='x', expand=1, padx=5, pady=5 )
           gui_support.balloon.bind( self.playforward, "Play Forward")

           img6 = Tkinter.PhotoImage( file = os.path.join(vcs_legacy.__path__[0],'..','..','..','..', 'bin', 'player_end2.gif') )
	   self.stepforward = Tkinter.Button(frame2, image = img6, command=gui_control.Command(self.evt_stepforward, parent, canvas))
           self.stepforward.pack(side='left',  fill='x', expand=1, padx=5, pady=5 )
           gui_support.balloon.bind( self.stepforward, "Step Forward")
           self.stepforward.configure(state = 'disabled')

           img7 = Tkinter.PhotoImage( file = os.path.join(vcs_legacy.__path__[0],'..','..','..','..', 'bin', '2rightarrow.gif') )
	   self.lastframe = Tkinter.Button(frame2, image = img7, command=gui_control.Command(self.evt_lastframe, parent, canvas))
           self.lastframe.pack(side='left',  fill='x', expand=1, padx=5, pady=5 )
           gui_support.balloon.bind( self.lastframe, "Last")
           self.lastframe.configure(state = 'disabled')
   
        self.notebook.setnaturalsize()

        #self.dialog,dialog.deiconify()

        def destroy_canvas(self, parent):
            parent.destroy()

    ######################################################################
    #  Tab Envents                                                       #
    ######################################################################
    def close_VCS_Canvas( self, canvas, event):
         import time
         try: 
            canvas.canvas_gui.notebook.selectpage("Data & Plot Info")
            canvas.animate.close()
            time.sleep(1) # Allow threading time to sync things up...
            canvas.close()
         except: pass

    def evt_save_plot_to_file( self, parent, canvas ):
      #gui_busy.busyStart( self, parent )
      filetypes = [ ("Postscript File", ".ps"), ("GIF File", ".gif"), ("CGM File", ".cgm") ]
      # Show the popup directory dialog
      sfile = tkFileDialog.asksaveasfilename( master=parent, filetypes = filetypes,
                           title = 'Save Plot to a File' )

      path= string.strip(sfile[:int(string.rfind(sfile,"."))])
      ext= sfile[int(string.rfind(sfile,".")+1):]

      # Save the outptut
      try:
         o = 'l'
         if (ext == "ps"):
            canvas.postscript( sfile, o )
         elif (ext == "gif"):
            canvas.gif( sfile, o )
         elif (ext == "cgm"):
            canvas.cgm( sfile, o )
      except:
         showerror( "Error Message to User", "There is an error with the output name. So nothing was saved.")
      
#      gui_busy.busyEnd( self, parent )

    def resize_canvas( self, w2, h2 ):
        self.frame.configure( width=w2, height=h2)
    #
    ####### event to set page orientation
    #
    def evt_page_orientation( self, parent, canvas, orientation ):
        import thread
        vcs_legacy.Canvas.finish_queued_X_server_requests( self.canvas )
  
        self.canvas.BLOCK_X_SERVER()
        vcs_legacy.Canvas.finish_queued_X_server_requests( self.canvas )
        if orientation == 'portrait':
           w2 = int(self.top_parent.winfo_screenwidth()*.44); h2 = int(self.top_parent.winfo_screenheight()*0.77)
           self.frame.configure( width=w2, height=h2)
           self.top_parent.update_idletasks() # Flush events without blocking the thread: could have used "self.top_parent.update()"
           self.page_landscape.set( 0 )
           self.page_portrait.set( 1 )
           self.page_orientation_flg = 0
           canvas.portrait()
        else:
           w2 = int(self.top_parent.winfo_screenwidth()*.55); h2 = int(self.top_parent.winfo_screenheight()*0.54)
           self.frame.configure( width=w2, height=h2)
           self.top_parent.update_idletasks() # Flush events without blocking the thread: could have used "self.top_parent.update()"
           self.page_landscape.set( 1 )
           self.page_portrait.set( 0 )
           self.page_orientation_flg = 1
           canvas.landscape()
        self.canvas.UNBLOCK_X_SERVER()
        thread.start_new_thread( self.resize_canvas, (w2, h2,) )

    def evt_tab(self, parent, canvas, text):
        global last_tab
        global plot_kw

        if (text in ["Edit Plot"]):
           if last_tab == "Animation":
              self.stop_animation ( parent, canvas )
              try:
               if (self.called_from_vcdat == 1): # Must be from VCDAT
                  tmpl = self.templateeditor.execute_from_canvas()
              except: pass

           #tmplt=canvas.gettemplate('new')

           # Get existing display plot info
           #plt=canvas.getplot('dpy_plot_1')
           vcs_legacy_dpy = canvas.return_display_names()
           for d in vcs_legacy_dpy:
              plt = canvas.getplot( d )

           self.top_parent.busyWidgets = ( parent, self.animation_speed.component('entry') )
           self.templateeditor = canvas.templateeditor(gui_parent = self.top_parent, canvas = canvas, template_name = plt.template , plot = plt, called_from = 1)

#           canvas._SCREEN_TEMPLATE_FLAG()
#           _gui_template_editor.create(gui_parent=parent, canvas=canvas, plot=None, template_name='new2', template_orig_name='default')
           #_gui_template_editor.create(gui_parent=parent, canvas=self, plot=plot, template_name=template_name, template_orig_name=template_orig_name)
        else:
           if last_tab == "Edit Plot":
  # DEAN            print "plot_kw = ", plot_kw
              slab1, slab2, tmpl, gtype, gname = self.show_plot_after_animation(parent, canvas)

              # Save the changes to the plot
              try: slab1.filename=plot_kw['file']
              except: pass
              try: slab1.source=plot_kw['source']
              except: pass
              try: slab1.id=plot_kw['dataname']
              except: pass
              try: slab1.long_name = plot_kw['title']
              except: pass
              try: slab1.units = plot_kw['units']
              except: pass
              try: slab1.getAxis(len(slab1.shape)-1).id=plot_kw['xname']
              except: pass
              try: slab1.getAxis(len(slab1.shape)-2).id=plot_kw['yname']
              except: pass
              try: slab1.comment1 = plot_kw['comment1']
              except: pass
              try: slab1.comment2 = plot_kw['comment2']
              except: pass
              try: slab1.comment3 = plot_kw['comment3']
              except: pass
              try: slab1.comment4 = plot_kw['comment4']
              except: pass

              tmpl = self.templateeditor.execute_from_canvas()

              try: 
                  canvas.update_animation_data( slab1 ) # Update the saved animation slab to reflect the changes
              except: pass

              canvas.update()
           try: canvas._SCREEN_DATA_FLAG()
           except: pass

           if last_tab == "Animation": 
              self.stop_animation ( parent, canvas )
              try:
               if (self.called_from_vcdat == 1): # Must be from VCDAT
                  tmpl = self.templateeditor.execute_from_canvas()
              except: pass

           if (text in ["Animation"]):
                 self.animated_dimension = canvas.return_dimension_info()['name'][-1]
                 #try: self.show_plot_after_animation(parent, canvas)
                 #except: pass
                 pass
           elif (text in ["Data & Plot Info"]):
                 try: self.show_data_plot_info ( self, parent, canvas )
                 except: pass
           elif (text in ["Graphics Method"]):
                 show_graphics_method ( self, parent, canvas )

        last_tab = text 
# DEAN      print "1 ---  other = ", text

    #----------------------------------------------------------------------------------
    # Stop animate creation or run              
    #----------------------------------------------------------------------------------
    def stop_animation ( self, parent, canvas ):
          try:
             #    print "last tab last tab las tab = ", last_tab
             if (self.animation_gui_first_time != 0): self.animation_gui.dialog.withdraw()
             self.evt_stop(self.top_parent, self.canvas, 0.5)
             slab1, slab2, tmpl, gtype, gname = self.show_plot_after_animation(self.top_parent, canvas)
             canvas.animate.close()
             #    print "before close = ", slab1, slab2, tmpl, gtype, gname
                 #print " canvas = ", dir(canvas)
             #    print " canvas.varglist[0] = ", canvas.varglist[0].info()
                 #canvas.plot(canvas.varglist[0])
             self.timelabel.configure(text="%s 1:0 [1 step]: " % (self.animated_dimension) )
             self.create_flg = self.frame_count = self.nofs = 0
             self.framescale.configure(from_ = 1, to = 1)
             self.pause.configure(image = self.img4, command=gui_control.Command(self.evt_stop, self.top_parent, canvas, 0))
             self.firstframe.configure(state = 'disable')
             self.stepback.configure(state = 'disable')
             self.stepforward.configure(state = 'disable')
             self.lastframe.configure(state = 'disable')
        #     print "plot_kw ************ = ", plot_kw
             arg_size = len(canvas.varglist)

             # Clear the canvas and replot with updated data
             canvas.clear()
             if ( (gtype in ['Vector', 'Scatter', 'Xvsy']) and (slab2 is None)):
                 showerror( "Error Message to User", "Must have two variables to plot %s." % self.new_gm)
                 return     
             if slab2 is None: vdpy = canvas.plot(slab1, tmpl, gtype, gname, sal=0)
             else: vdpy = canvas.plot(slab1, slab2, tmpl, gtype, gname, sal=0)
#             try:
#                temp_arg = []
#                for i in range(arg_size):
#                    temp_arg.append(canvas.varglist[i])
#             except: pass
#             print "I am here", temp_arg
#             #canvas.varglist = []
#             print "I am here 2 ", type(canvas.varglist), canvas.varglist
#             for x in temp_arg:
#                canvas.varglist.append(x)
#             print "arglist = ", canvas.varglist
             
#                 vdpy.name = "dpy_plot_1" # Do this so that the template will know with plot to get
#                 vdpy.array[0]="plot_1" # Do this so that the template will know which data to get
          except: pass

    #----------------------------------------------------------------------------------
    # Show the new data and plot information
    #----------------------------------------------------------------------------------
    def show_plot_after_animation ( self, parent, canvas ):
      slab1 = None
      slab2 = None
      aa = __main__.__dict__
      vcs_legacy_dpy = canvas.return_display_names()
      for d in vcs_legacy_dpy:
          dpy_plot = canvas.getplot( d )
      try:
         if (self.called_from_vcdat == 1): # Must be from VCDAT
            slab1 = canvas.varglist[0] = canvas.canvas_gui.slab1
            slab2 = canvas.varglist[1] = canvas.canvas_gui.slab2
            template = dpy_plot.template
            g_type = canvas.canvas_gui.g_type
            g_name = canvas.canvas_gui.g_name
         else:
            error                         # Must not be from VCDAT, so do the except below
      except:
         for x in aa:
          if cdms2.isVariable( aa[x] ):
             if ( (cdms2.isVariable( aa[x] )) and (canvas.varglist[0] is not None) and (aa[x].name == canvas.varglist[0].name) ):
                slab1 = canvas.varglist[0]
             if ( (cdms2.isVariable( aa[x] )) and (canvas.varglist[1] is not None) and (aa[x].name == canvas.varglist[1].name) ):
                slab2 = canvas.varglist[1]

         # Get the display names
         template = dpy_plot.template
         g_type = dpy_plot.g_type
         g_name = dpy_plot.g_name

      return slab1, slab2, template, g_type, g_name

    #----------------------------------------------------------------------------------
    # Show the variable information
    #----------------------------------------------------------------------------------
    def display_variable_attributes(self, slab_list):
          self.scl2.insert( 'end', 'id:' )   # start id
          for x in slab_list:
             if x[0:3] in ['id:']: varname = x[4:]
          slab_list[0] = '*** More variable attributes ***'

          ct = 0
          for x in slab_list:
             if x in ['** Dimension 1 **']: break
             else: ct += 1

          try: self.name_btn.destroy()
          except: pass
          self.name_btn = Pmw.ComboBox(self.scl2.component('text'),
                      labelpos = 'w',
                      label_text = varname,
                      entry_background = 'white',
                      entry_foreground = 'black',
                      entry_width = 25,
                      scrolledlist_items = slab_list[0:ct],
                      dropdown = 1)
          self.name_btn.selectitem(slab_list[0])
          gui_support.balloon.bind( self.name_btn, "Show more variable information.")
	  self.scl2.window_create('end', window = self.name_btn)
          self.scl2.insert( 'end', '\n' )   # end id

          for x in slab_list:
              if x[0:9] in ['long_name']: self.scl2.insert( 'end', (x + '\n') )  # long_name
          for x in slab_list:
              if x[0:5] in ['title']: self.scl2.insert( 'end', (x + '\n') )      # title
          for x in slab_list:
              if x[0:5] in ['units']: self.scl2.insert( 'end', (x + '\n') )      # units
          for x in slab_list:
              if x[0:8] in ['filename']: self.scl2.insert( 'end', (x + '\n') )   # filename
          for x in slab_list:
              if x[0:5] in ['shape']: self.scl2.insert( 'end', (x + '\n') )      # shape

    #----------------------------------------------------------------------------------
    # Show the dimension information
    #----------------------------------------------------------------------------------
    def display_dimension_attributes(self, slab_list):
          self.scl3.settext ('') # Clear text
          dim_id = []
          dim_units = []
          dim_length = []
          dim_first = []
          dim_last = []
          for y in slab_list:
                if y[0:6] in ['   id:']: dim_id.append( y.strip() )          # id
                if y[0:9] in ['   units:']: dim_units.append( y.strip() )    # units
                if y[0:10] in ['   Length:']: dim_length.append( y.strip() ) # Length
                if y[0:9] in ['   First:']: dim_first.append( y.strip() )    # First
                if y[0:8] in ['   Last:']: dim_last.append( y.strip() )      # Last
                
          for i in range(len(dim_id)):
             self.scl3.insert( 'end', (dim_id[i] + '\n') )    # id
             self.scl3.insert( 'end', (dim_units[i] + '\n') )   # units
             self.scl3.insert( 'end', (dim_length[i] + '  ' + dim_first[i] + '  ' + dim_last[i] + '\n') )   # length, first, last
             self.scl3.insert( 'end', '--------------------------------------------------\n' ) 

    #----------------------------------------------------------------------------------
    # Show the new data and plot information
    #----------------------------------------------------------------------------------
    def show_data_plot_info ( self, parent, canvas ):
          self.scl2.delete( "1.0", "end" )
          self.scl2.configure( text_state='normal' ) # Allow for modification
    
          # Write slab information to text window
          slab1 = None
          slab2 = None
          plot_name_list = []
          try:
             plot_name_list.append(canvas.varglist[0].name)
             plot_name_list.append(canvas.varglist[1].name)
          except: 
             pass
          aa = __main__.__dict__
          for x in aa: 
              if cdms2.isVariable( aa[x] ): 
                 if ( (cdms2.isVariable( aa[x] )) and (canvas.varglist[0] is not None) and (aa[x].name == canvas.varglist[0].name) ):
                    slab1_list = canvas.varglist[0].listall()
                    self.display_variable_attributes(slab1_list)

                 if ( (cdms2.isVariable( aa[x] )) and (canvas.varglist[1] is not None) and (aa[x].name == canvas.varglist[1].name) ):
                    slab2_list = canvas.varglist[1].listall()
                    self.display_variable_attributes(slab2_list)
          self.scl2.insert( 'end', '\n\n' )

          try:
             if (self.called_from_vcdat == 1): # Must be from VCDAT
                if canvas.canvas_gui.slab1 is not None:
                   slab1_list = canvas.canvas_gui.slab1.listall()
                   self.scl2.settext ('') # Clear text
                   self.display_variable_attributes(slab1_list)
                if canvas.canvas_gui.slab2 is not None:
                   slab2_list = canvas.canvas_gui.slab2.listall()
                   self.display_variable_attributes(slab2_list)
                self.scl2.insert( 'end', '\n\n' )
          except:
             pass
    
          # View the display and plot information
          vcs_legacy_dpy = canvas.return_display_names()
          for d in vcs_legacy_dpy:
             dpy_plot = canvas.getplot( d )

          plot_list = []
          plot_list.append("*** View plot information ***")
          plot_list.append( "Canvas Mode = " + str(canvas.mode) )
          plot_list.append( "name = " + str(dpy_plot.name) )
          plot_list.append( "off = " + str(dpy_plot.off))
          plot_list.append("priority = " + str(dpy_plot.priority) )
          plot_list.append( "template = " + str(dpy_plot.template) )
          plot_list.append( "g_type = " + str(dpy_plot.g_type) )
          plot_list.append( "g_name = " + str(dpy_plot.g_name) )

          try: self.plot_btn.destroy()
          except: pass
          self.plot_btn = Pmw.ComboBox(self.scl2.component('text'),
                                labelpos = 'w',
                                label_text = "Plot",
                                entry_background = 'white',
                                entry_foreground = 'black',
                                entry_width = 27,
                                scrolledlist_items = plot_list,
                                dropdown = 1)
          self.plot_btn.selectitem(plot_list[0])
          gui_support.balloon.bind( self.plot_btn, "Show plot information.")
          self.scl2.window_create('end', window = self.plot_btn)
          self.scl2.insert( 'end', '\n\n' )   # end id

          self.scl2.configure( text_state='disabled' ) # Don't allow the user to modify the text
    
          self.scl2.see('end')

          self.display_dimension_attributes(slab1_list)

    ######################################################################
    #  Animation Tab Events                                              #
    ######################################################################
    def show_animation_gui(self, parent, canvas):
        if ( (self.animation_gui is None) and (self.animation_gui_first_time == 0) ):
           self.animation_gui = canvas.animate.gui(gui_parent = self.top_parent)
           self.animation_gui_first_time = 1
        else:
           self.animation_gui.dialog.transient(self.top_parent) # Keep widget on top of its parent
           self.animation_gui.dialog.deiconify()

    def evt_framescale( self, parent, canvas, event ):
        self.frame_position = string.atoi(event)
        canvas.animate.frame( self.frame_position )
        self.timelabel.configure(text="%s 1:%i [%i step]: " % (self.animated_dimension, self.nofs, self.frame_position) )

    def evt_animation_speed( self, parent, canvas):
        canvas.animate.pause(string.atoi( self.animation_speed.get()) )

    def evt_zoom(self, parent, canvas, in_out):
        global zoom_ct

        zoom_ct += in_out
        if zoom_ct < 1: zoom_ct = 1
        if zoom_ct > 20: zoom_ct = 20
        canvas.animate.zoom( zoom_ct )

    def evt_pan_hori(self, parent, canvas, up_down):
        global pan_hori_ct

        pan_hori_ct += up_down
        if pan_hori_ct < -100: pan_hori_ct = -100
        if pan_hori_ct > 100: pan_hori_ct = 100
        canvas.animate.horizontal( pan_hori_ct )

    def evt_pan_vert(self, parent, canvas, left_right):
        global pan_vert_ct

        pan_vert_ct += left_right
        if pan_vert_ct < -100: pan_vert_ct = -100
        if pan_vert_ct > 100: pan_vert_ct = 100
        canvas.animate.vertical( pan_vert_ct )

    def evt_firstframe(self, parent, canvas):
        canvas.animate.frame( 1 )
        self.timelabel.configure(text="%s 1:%i [1 step]: " % (self.animated_dimension, self.nofs ))
        self.framescale.set( 1 )

    def evt_stepback(self, parent, canvas):
        if (self.frame_position > 1):
           self.frame_position -= 1
           canvas.animate.frame( self.frame_position )
           self.timelabel.configure(text="%s 1:%i [%i step]: " % (self.animated_dimension, self.nofs, self.frame_position) )
           self.framescale.set( self.frame_position )

    def evt_stepforward(self, parent, canvas):
        if (self.frame_position < self.nofs):
           self.frame_position += 1
           canvas.animate.frame( self.frame_position )
           self.timelabel.configure(text="%s 1:%i [%i step]: " % (self.animated_dimension, self.nofs, self.frame_position) )
           self.framescale.set( self.frame_position )

    def evt_lastframe(self, parent, canvas):
        canvas.animate.frame( self.nofs )
        self.timelabel.configure(text="%s 1:%i [%i step]: " % (self.animated_dimension, self.nofs, self.nofs) )
        self.framescale.set( self.nofs )

    def evt_stop(self, parent, canvas, pause):
        if (self.create_flg == 1):
           canvas.animate.stop()
           canvas.animate.stop()
           #canvas.animate.stop_create()

           time.sleep(pause) # Timing issue. Let the animation thread catch up
           self.timelabel.configure(text="%s 1:%i [%i step]: " % ( self.animated_dimension, int(self.frame_count), int(self.frame_count) ))
           self.framescale.configure(from_ = 1, to = int(self.frame_count))
           self.pause.configure(image = self.img4a, command=gui_control.Command(self.evt_pause, self.top_parent, canvas))

    def evt_pause(self, parent, canvas):
        canvas.animate.stop()

    def update_end_of_animation_creation(self):
        # Continue to run through the frames
        sf = dir(self) # Slow down the animation so that threading can catch up with the GUI updates.
        #self.top_parent.update_idletasks() # Flush events without blocking the thread: could have used "self.top_parent.update()"
        self.evt_stop(self.parent, self.canvas, 0)
        self.canvas.animate.run()
        

    def update_animation(self, frame_count):
        sf = dir(self) # Slow down the animation so that threading can catch up with the GUI updates.
        #self.self.top_parent.update_idletasks() # Flush events without blocking the thread: could have used "self.top_parent.update()"
        self.timelabel.configure(text="%s 1:%i [%i step]: " % (self.animated_dimension, frame_count, frame_count))
        self.framescale.configure(from_ = 1, to = int(frame_count))
        self.framescale.set( frame_count )
        self.frame_count = self.nofs = frame_count
        return 1

    def evt_play(self, parent, canvas, direction):
        # Only create if the first time
        if (self.create_flg == 0):
           # Note: cannot set the min and max values if the default graphics method is used. So just create the animation.
           # For example: mn, mx = canvas.animate.return_animation_min_max()
           #              canvas.animate.create(thread_it=0, min=mn, max=mx) 
           canvas.animate.create()
           canvas.animate.run_flg = 1 # Needed to prevent flashing and insure the first image is saved
   
           # Activate the other control buttons
           self.firstframe.configure(state = 'normal', command=gui_control.Command(self.evt_firstframe, parent, canvas))
           self.stepback.configure(state = 'normal', command=gui_control.Command(self.evt_stepback, parent, canvas))
           #self.pause.configure(state = 'normal', command=gui_control.Command(self.evt_pause, parent, canvas))
           self.stepforward.configure(state = 'normal', command=gui_control.Command(self.evt_stepforward, parent, canvas))
           self.lastframe.configure(state = 'normal', command=gui_control.Command(self.evt_lastframe, parent, canvas))

           self.create_flg = 1

        # Set the directon of the animation
        canvas.animate.direction( direction )

        # Set the run flag and start the run thread
        canvas.animate.run()

######################################################################
#  Set up the Edit Plot GUI                                          #
######################################################################
def template_tab_gui ( self, page, parent, canvas ):
      framea = Tkinter.Frame( page, background = winColor, borderwidth = 2, relief = 'sunken' )
      framea.pack(side='left', padx=10, pady=5)
      gui_support.balloon.bind( page, "Edit Plot:\n     Edit the plot by selecting any plot segment with the left mouse button. For example,\n     select the legend with the left mouse button. (Red boxes at the corners of the selected\n     segment will indicate a selection.) While moving the pointer within the selected region,\n     the cursor will change to indicate a specific type of user action. To execute an action,\n     select and hold the left mouse button and move the pointer to a desired location. Once\n     at the desired location, release the left mouse button to complete the execution. Arrow\n     indication:\n       * Cross Arrows indicates moving the plot segment to a new location\n       * Up Arrow Line indicates moving the top border to a new location\n       * Down Arrow Line indicates moving the bottom border to a new location\n       * Left Arrow Line indicates moving the left border to a new location\n       * Right Arrow Line indicates moving the right border to a new location\n\nText Edits:\n     If the segment selected is text, then the text will be displayed in the text window for\n     editing. Select the text window and edit the text. Select the <return> key to register the\n     new text on the plot. Selecting the down arrow to the right of the text window is another\n     way to select text on the plot.\n\n Additional Controls:\n     By selecting the Additional Controls button, the user can make further plot\n     modifications.")

      # Write slab information to text window
#      try: 
#         slab1 = None
#         slab2 = None
#         aa = __main__.__dict__
#         for x in aa:
#             if cdms2.isVariable( aa[x] ):
#                if ( (cdms2.isVariable( aa[x] )) and (canvas.varglist[0] is not None) and (aa[x].name == canvas.varglist[0].name) ):
#                   slab1 = canvas.varglist[0]
#                if ( (cdms2.isVariable( aa[x] )) and (canvas.varglist[1] is not None) and (aa[x].name == canvas.varglist[1].name) ):
#                   slab2 = canvas.varglist[1]
	# Create and pack the dropdown ComboBox.
      edit_text = ('File', 'Source', 'Dataname', 'Title', 'Units', 'Xname',
                'Yname', 'Comment1', 'Comment2', 'Comment3', 'Comment4')
      self.txt = Pmw.ComboBox( framea,
                label_text = "Source",
                label_font = ('helvetica',12),
	        labelpos = 'w',
                entry_foreground = "black",
                entry_background = winColor,
                entry_width=45,
                selectioncommand = gui_control.Command(evt_template_text, self, parent, canvas),
		scrolledlist_items = edit_text,
      )

#      self.txt = Pmw.EntryField( framea,
#                    labelpos = 'w',
#                    label_text = "Source",
#                    label_font = ('helvetica',10),
#                    entry_width=30,
##                    entry_state = 'disabled',
#                    entry_foreground = "black",
#                    entry_background = "white")
      self.txt.pack(side='top')
      self.txt.component('entry').bind( "<Return>", gui_control.Command(evt_change_text, self, parent, canvas) )
#      self.txt.setentry( slab1.id )

      #----------------------------------------------------------------------
      # Edit the "X-Axis"
      #----------------------------------------------------------------------
      group = Pmw.Group(framea, tag_text = 'Edit X-Axis')
      group.pack(fill = 'both', expand = 1, padx = 0, pady = 0)
      fm=Tkinter.Frame(group.interior())
      self.eny1=Pmw.EntryField(fm,
          labelpos = 'w',
          label_text = 'Bottom Values:',
          entry_background = winColor,
          entry_foreground = 'black',
          entry_width =  41,
          value = global_xticlabels,
          )
      self.eny1.pack( expand = 1, fill = 'both', padx=0, pady=0 )
      gui_support.balloon.bind( self.eny1, "Specify the desired X-axis major tick marks and labels.\nFor example:\nNone\n[ ] or { }\n[ 0, 10, 20 ]  or  { 0:'0', 10:'10', 20:'20' }\n[ 0, 10 ]  or  { 0:'text', 10:'more text' }" )
      self.eny1.clear( )
      try: self.eny1.setentry( global_xticlabels )
      except: pass
      self.eny1.component('entry').bind( "<Return>", gui_control.Command(evt_change_x_major_axis, self, parent, canvas) )
      self.eny2=Pmw.EntryField(fm,
          labelpos = 'w',
          label_text = 'Top Values:',
          entry_background = winColor,
          entry_foreground = 'black',
          entry_width =  41,
          value = global_xmtics,
          )
      self.eny2.pack( expand = 1, fill = 'both', padx=0, pady=0 )
      gui_support.balloon.bind( self.eny2, "Specify the desired X-axis minor tick marks.\nFor example:\nNone\n[ ] or { }\n[ 0, 10, 20 ]  or  { 0:'0', 10:'10', 20:'20' }\n[ 0, 10 ]  or  { 0:'text', 10:'more text' }" )
      self.eny2.clear( )
      try: self.eny2.setentry( global_xmtics )
      except: pass
      self.eny2.component('entry').bind( "<Return>", gui_control.Command(evt_change_x_minor_axis, self, parent, canvas) )
      fm.pack( side='right', fill='both', expand=1 )
      #----------------------------------------------------------------------
      # End the "X-Axis"
      #----------------------------------------------------------------------

      #----------------------------------------------------------------------
      # Edit the "Y-Axis"
      #----------------------------------------------------------------------
      group = Pmw.Group(framea, tag_text = 'Edit Y-Axis')
      group.pack(fill = 'both', expand = 1, padx = 0, pady = 0)
      fm=Tkinter.Frame(group.interior())
      self.eny3=Pmw.EntryField(fm,
          labelpos = 'w',
          label_text = 'Left Values:',
          entry_background = winColor,
          entry_foreground = 'black',
          entry_width =  41,
          )
      self.eny3.pack( expand = 1, fill = 'both', padx=0, pady=0 )
      gui_support.balloon.bind( self.eny3, "Specify the desired Y-axis major tick marks and labels.\nFor example:\nNone\n[ ] or { }\n[ 0, 10, 20 ]  or  { 0:'0', 10:'10', 20:'20' }\n[ 0, 10 ]  or  { 0:'text', 10:'more text' }" )
      self.eny3.clear( )
      try: self.eny3.setentry( global_yticlabels )
      except: pass
      self.eny3.component('entry').bind( "<Return>", gui_control.Command(evt_change_y_major_axis, self, parent, canvas) )
      self.eny4=Pmw.EntryField(fm,
          labelpos = 'w',
          label_text = 'Right Values:',
          entry_background = winColor,
          entry_foreground = 'black',
          entry_width =  41,
          )
      self.eny4.pack( expand = 1, fill = 'both', padx=0, pady=0 )
      gui_support.balloon.bind( self.eny4, "Specify the desired Y-axis minor tick marks.\nFor example:\nNone\n[ ] or { }\n[ 0, 10, 20 ]  or  { 0:'0', 10:'10', 20:'20' }\n[ 0, 10 ]  or  { 0:'text', 10:'more text' }" )
      self.eny4.clear( )
      try: self.eny4.setentry( global_ymtics )
      except: pass
      self.eny4.component('entry').bind( "<Return>", gui_control.Command(evt_change_y_minor_axis, self, parent, canvas) )
      fm.pack( side='right', fill='both', expand=1 )
      #----------------------------------------------------------------------
      # End the "Y-Axis"
      #----------------------------------------------------------------------

      frameb = Tkinter.Frame( page, background = winColor, borderwidth = 2, relief = 'sunken' )
      frameb.pack(side='left')

      self.additional_controls = Tkinter.Button(frameb, text = 'Additional Controls',
                command = gui_control.Command(show_template_gui, self, parent, canvas)
                )
      self.additional_controls.pack(side='top', padx = 12, pady = 6)

      self.save_template = Tkinter.Button(frameb, text = 'Save Template',
	        command = gui_control.Command(save_template, self, parent, canvas)
                )
      self.save_template.pack(side='top', padx = 2, pady = 6)
      gui_support.balloon.bind( self.save_template, "Save the template to a file.")


def evt_template_text(self, parent, canvas, txt):
   import gui_template_editor
   from gui_support import gui_color
   #print "txt = ", txt
   attr_name, attr_str = gui_template_editor.get_slab_info(self.top_parent, canvas, string.lower(txt), "Axis")
   #print "name and str = ", attr_name, attr_str
   self.txt.configure( label_text = txt )
   try:
      self.txt.setentry( attr_str )
   except:
      self.txt.setentry( " " )
   #print "self.templateeditor = ", dir(self.templateeditor)
   a_names = ["file", "function", "logicalmask", "transformation", "source", "dataname", "title", "units", "crdate", "crtime", "comment1", "comment2", "comment3", "comment4", "xname", "yname", "zname", "tname", "xunits", "yunits", "zunits", "tunits", "xvalue", "yvalue", "zvalue", "tvalue", "mean", "min", "max", "xtic1", "xtic2", "xmintic1", "xmintic2", "ytic1", "ytic2", "ymintic1", "ymintic2", "xlabel1", "xlabel2", "ylabel1", "ylabel2", "box1", "box2", "box3", "box4", "line1", "line2", "line3", "line4", "legend", "data"]
   for x in a_names: 
      try: eval("self.templateeditor."+x+".display.configure( selectcolor = gui_color.one )")
      except: pass
   self.templateeditor.refresh_toggle(attr_name, 1)

def evt_change_text(self, parent, canvas, event):
   global plot_kw
   str_txt = self.txt.get()
   lbl_txt = self.txt.cget("label_text")
   plot_kw[string.lower(lbl_txt)] = str_txt
   #print "1 - screen mode = ", canvas.SCREEN_MODE()
   evt_change_gm_name( self, parent, canvas )
   #evt_change_gm_name( self, parent, canvas, event )
   #evt_change_gm_name( self, parent, canvas, event, plot_kw )
   #print "I am here changing = ", lbl_txt, " - ", str_txt
   #print "2 - screen mode = ", canvas.SCREEN_MODE()

def evt_change_x_major_axis(self, parent, canvas, event):
   template_priority = "self.templateeditor.new_template.xtic1.priority=1"
   exec(template_priority)
   template_priority = "self.templateeditor.new_template.xlabel1.priority=1"
   exec(template_priority)
   g = return_graphics_method(self, canvas)
   global_xticlabels = _get_axis_dict(self, self.eny1 )
   g.xticlabels1 = global_xticlabels
   canvas.update()

def evt_change_x_minor_axis(self, parent, canvas, event):
   template_priority = "self.templateeditor.new_template.xtic2.priority=1"
   exec(template_priority)
   template_priority = "self.templateeditor.new_template.xlabel2.priority=1"
   exec(template_priority)
   g = return_graphics_method(self, canvas)
   global_xmtics = _get_axis_dict(self, self.eny2 )
   g.xticlabels2 = global_xmtics
   canvas.update()

def evt_change_y_major_axis(self, parent, canvas, event):
   template_priority = "self.templateeditor.new_template.ytic1.priority=1"
   exec(template_priority)
   template_priority = "self.templateeditor.new_template.ylabel1.priority=1"
   exec(template_priority)
   g = return_graphics_method(self, canvas)
   global_yticlabels = _get_axis_dict(self, self.eny3 )
   g.yticlabels1 = global_yticlabels
   canvas.update()

def evt_change_y_minor_axis(self, parent, canvas, event):
   template_priority = "self.templateeditor.new_template.ytic2.priority=1"
   exec(template_priority)
   template_priority = "self.templateeditor.new_template.ylabel2.priority=1"
   exec(template_priority)
   g = return_graphics_method(self, canvas)
   global_ymtics = _get_axis_dict(self, self.eny4 )
   g.yticlabels2 = global_ymtics
   canvas.update()

def return_graphics_method(self, canvas):
   if string.lower( self.g_type ) == 'boxfill':
         g = canvas.getboxfill(self.g_name)
   elif string.lower( self.g_type ) == 'isofill':
         g = canvas.getisofill(self.g_name)
   elif string.lower( self.g_type ) == 'isoline':
         g = canvas.getisoline(self.g_name)
   elif string.lower( self.g_type ) == 'outfill':
         g = canvas.getoutfill(self.g_name)
   elif string.lower( self.g_type ) == 'outline':
         g = canvas.getoutline(self.g_name)
   elif string.lower( self.g_type ) == 'vector':
         g = canvas.getvector(self.g_name)
   elif string.lower( self.g_type ) == 'scatter':
         g = canvas.getscatter(self.g_name)
   elif string.lower( self.g_type ) == 'taylordiagram':
         g = canvas.gettaylordiagram(self.g_name)
   elif string.lower( self.g_type ) == 'xvsy':
         g = canvas.getxvsy(self.g_name)
   elif string.lower( self.g_type ) == 'xyvsy':
         g = canvas.getxyvsy(self.g_name)
   elif string.lower( self.g_type ) == 'yxvsx':
         g = canvas.getyxvsx(self.g_name)
   elif string.lower( self.g_type ) == 'continents':
         g = canvas.getcontinents(self.g_name)
   elif string.lower( self.g_type ) == 'meshfill':
         g = canvas.getmeshfill(self.g_name)

   return g

def _get_axis_dict(self, entry_box):
   text = entry_box.get().strip() 
   try:
      if (text == '*') or (text == ''):
         text = '*'
      elif text == 'None':
         text = 'None'
      else:
        try:
           text = eval(text)
        except:
           pass 
      if (isinstance(text, types.ListType)):
         d={}
         for x in text: d[x] = str(x)
         text = d
      elif (isinstance(text, types.DictType)):
         for x in text: text[x] = str(text[x])
      return text
   except:
      pass


#----------------------------------------------------------------------------------
# Show the colormap GUI                  
#----------------------------------------------------------------------------------
#def update_template_text( self, parent, canvas ):
#    print "I am here in update template text!\n"

def show_template_gui( self, parent, canvas ):
    self.templateeditor.dialog.dialog.deiconify()   # Show the template editor GUI

def save_template( self, parent, canvas):
    self.templateeditor.savescript()

######################################################################
#  Set up the Primitives GUI                                         #
######################################################################
def primitive_tab_gui ( self, page, parent, canvas ):
      global winColor
      frameb = Tkinter.Frame( page,
                              background = winColor,
                              borderwidth = 2,
                              height = 1,
                              relief = 'sunken' )
      frameb.pack( side='left' )
      # Create some buttons to launch the dialogs.
      self.prm_colormap = Tkinter.Button(frameb,
                text = 'Modify Colormap',
                command = gui_control.Command(evt_colormapgui, self, parent, canvas)
                )
      self.prm_colormap.pack(side = "left", padx = 2, pady = 2)
      gui_support.balloon.bind( self.prm_colormap, "Display the VCS Colormap Editor.")

      self.prm_line = Tkinter.Button(frameb,
                text = 'Line',
                 state = "disabled",
                )
      self.prm_line.pack(side = "left", padx = 2, pady = 2)
      gui_support.balloon.bind( self.prm_line, "Display the Line Editor.")

      self.prm_marker = Tkinter.Button(frameb,
                text = 'Marker',
                 state = "disabled",
                )
      self.prm_marker.pack(side = "left", padx = 2, pady = 2)
      gui_support.balloon.bind( self.prm_marker, "Display the Line Editor.")

      self.prm_fillarea = Tkinter.Button(frameb,
                text = 'Fill area',
                 state = "disabled",
                )
      self.prm_fillarea.pack(side = "left", padx = 2, pady = 2)
      gui_support.balloon.bind( self.prm_fillarea, "Display the Line Editor.")

      self.prm_text = Tkinter.Button(frameb,
                text = 'Text',
                 state = "disabled",
                )
      self.prm_text.pack(side = "left", padx = 2, pady = 2)
      gui_support.balloon.bind( self.prm_text, "Display the Line Editor.")


#----------------------------------------------------------------------------------
# Show the colormap GUI                  
#----------------------------------------------------------------------------------
def evt_colormapgui ( self, parent, canvas ):
      canvas.colormapgui()

######################################################################
#  Set up the Data and Plot Info GUI                                 #
######################################################################
def data_plot_info_tab_gui ( self, page, parent, canvas ):
      framea = Tkinter.Frame( page )
      framea.pack( side='top', fill = 'both', expand=1)

      # Set up the output scroll window
      self.scl2 = Pmw.ScrolledText( framea,
                                 text_wrap='none',
                                 text_background=winColor,
                                 text_foreground='black',
                                 text_height = "12",
                                 usehullsize = 1,
                                 hull_width = 320,
                                  )
      self.scl2.pack(side = 'left', fill = 'y', expand=0 )
      gui_support.balloon.bind( self.scl2, "Scroll window to view the Data and Plot information.")

      # Set up the output scroll window
      self.scl3 = Pmw.ScrolledText( framea,
                                 text_wrap='none',
                                 text_background=winColor,
                                 text_foreground='black',
                                 text_height = "12",
                                 usehullsize = 1,
                                  )
      self.scl3.pack(side = 'left', fill = 'both', expand=1 )
      gui_support.balloon.bind( self.scl3, "Scroll window to view the Data and Plot information.")

######################################################################
#  Set up the Graphics Method  GUI                                   #
######################################################################
def graphics_method_tab_gui( self, page, parent, canvas ):
      framea = Tkinter.Frame( page )
      framea.pack( side='top', fill = 'both', expand = 1 )
      # Create and pack the simple ComboBox to select the graphics method.
      graphics_methods = ('Boxfill', 'Isofill', 'Isoline', 'Meshfill', 'Outfill', 'Outline', 'Scatter', 'Taylordiagram', 'Vector', 'Xvsy', 'Xyvsy', 'Yxvsx')
      self.select_graphics_method = Pmw.ScrolledListBox(framea,
                label_text = 'Select the Graphics Method Type:\nGraphics Method Type selected: ',
	        labelpos = 'nw',
                ##entry_state = 'disabled',
                listbox_background = winColor,
                listbox_height = 1,
                selectioncommand = gui_control.Command(evt_change_graphics_method, self, parent, canvas),
		items = graphics_methods
                #dropdown = 0
                )
      self.select_graphics_method.pack(side = 'left', fill = 'both', expand = 1, padx = 2, pady = 2)
      gui_support.balloon.bind( self.select_graphics_method, "Select a graphics method.")

      self.select_gm_name = Pmw.ScrolledListBox(framea,
                label_text = 'Select Graphics Method: Graphics Method name:',
	        labelpos = 'nw',
                listbox_background = winColor,
                listbox_height = 1,
                selectioncommand = gui_control.Command(evt_change_gm_name, self, parent, canvas),
		items = graphics_methods
                #dropdown = 0,
                )
      self.select_gm_name.pack(side = 'left', fill = 'both', expand = 1, padx = 2, pady = 2)
      gui_support.balloon.bind( self.select_gm_name, "Select a graphics method name.")

      Tkinter.Label(framea,text='Graphics Method Options:').pack(padx = 2, pady = 0)

      frameb = Tkinter.Frame( framea, 
                background = winColor,
                borderwidth = 2, 
                height = 1,
                relief = 'sunken' )
      frameb.pack( side='left', fill = 'both', expand = 1 )
      # Create some buttons to launch the dialogs.
      self.gm_editor = Tkinter.Button(frameb,
                text = 'Edit Graphics Method',
	        command = gui_control.Command(evt_graphicsmethodgui, self, parent, canvas)
                )
      self.gm_editor.pack(padx = 2, pady = 2)
      gui_support.balloon.bind( self.gm_editor, "Display the graphics method Editor.")

      self.gm_copy = Tkinter.Button(frameb, text = 'Copy Graphics Method',
	        command = gui_control.Command(copy_graphics_method, self, parent, canvas),
                )
      self.gm_copy.pack(padx = 2, pady = 2)
      gui_support.balloon.bind( self.gm_copy, "Copy the graphics method to a new name.")

#      self.gm_rename = Tkinter.Button(frameb, text = 'Rename Graphics Method',
#	        command = gui_control.Command(rename_graphics_method, self, parent, canvas),
#                )
#      self.gm_rename.pack(padx = 2, pady = 2)
#      gui_support.balloon.bind( self.gm_rename, "Rename the graphics method.")

      self.gm_save = Tkinter.Button(frameb, text = 'Save as Script File',
	        command = gui_control.Command(script_graphics_method, self, parent, canvas),
                )
      self.gm_save.pack(padx = 2, pady = 2)
      gui_support.balloon.bind( self.gm_save, "Save the graphics method to a file.")

#      self.gm_remove = Tkinter.Button(frameb, text = 'Remove Graphics Method',
#	        command = self._createOnTheFly)
#                )
#      self.gm_remove.pack(padx = 2, pady = 2)
#      gui_support.balloon.bind( self.gm_remove, "Remove the graphics method from the VCS list.")


#----------------------------------------------------------------------------------
# Change the Graphics Method
#----------------------------------------------------------------------------------
def evt_change_graphics_method( self, parent, canvas ):
      # Get the display names
      vcs_legacy_dpy = canvas.return_display_names() 
      for d in vcs_legacy_dpy:
         dpy_plot = canvas.getplot( d )

      # Show the graphics method's type and names
      self.new_gm = self.select_graphics_method.getcurselection()[0]
      self.gm_name = dpy_plot.g_name
      self.select_gm_name.configure( label_text = "Select " + self.new_gm + ":" )
      self.select_gm_name.setlist( canvas.listelements( self.new_gm ) )
      #self.select_gm_name.setvalue( "default" )

      # Change the button names to match the graphics method
      self.select_graphics_method.configure( label_text = 'Select the Graphics Method\nGraphics Method selected: ' + self.new_gm);
      self.select_gm_name.configure( label_text = 'Select Graphics Method name\nSelected name: ' + dpy_plot.g_name);
      self.gm_editor.configure( text = 'Edit ' + self.new_gm )
      self.gm_copy.configure( text = 'Copy ' + self.new_gm )
     # self.gm_rename.configure( text = 'Rename ' + self.new_gm )
      self.gm_save.configure( text = 'Save ' + self.new_gm + ' as Script File' )
     # self.gm_remove.configure( text = 'Remove ' + self.new_gm )

#----------------------------------------------------------------------------------
# Show the graphics method in the entry window
#----------------------------------------------------------------------------------
def show_graphics_method ( self, parent, canvas ):
      canvas._SCREEN_GM_FLAG()
      vcs_legacy_dpy = canvas.return_display_names() 
      for d in vcs_legacy_dpy:
           dpy_plot = canvas.getplot( d )
      g_type = string.capitalize( dpy_plot.g_type )
      self.new_gm = dpy_plot.g_type
      self.gm_name = dpy_plot.g_name

      # Show the graphics method's type and names
      self.select_gm_name.configure( label_text = 'Select Graphics Method name\nSelected name: ' + dpy_plot.g_name);
      self.select_gm_name.setlist( canvas.listelements( g_type ) )

      # Change the button names to match the graphics method
      self.select_graphics_method.configure( label_text = 'Select the Graphics Method\nGraphics Method selected: ' + g_type);
      self.gm_editor.configure( text = 'Edit ' + g_type )
      self.gm_copy.configure( text = 'Copy ' + g_type )
     # self.gm_rename.configure( text = 'Rename ' + g_type )
      self.gm_save.configure( text = 'Save ' + g_type + ' as Script File' )
     # self.gm_remove.configure( text = 'Remove ' + g_type )

#----------------------------------------------------------------------------------
# Change the Graphics Method name and replot
#----------------------------------------------------------------------------------
def evt_change_gm_name( self, parent, canvas ):
      vcs_legacy_dpy = canvas.return_display_names()
      for d in vcs_legacy_dpy:
         dpy_plot = canvas.getplot( d )
      #print "dpy_plot = ", dpy_plot

      # Write slab information to text window
      slab1 = None
      slab2 = None
      plot_name_list = []
      try:
         plot_name_list.append(canvas.varglist[0].name)
         plot_name_list.append(canvas.varglist[1].name)
      except: 
         pass
      aa = __main__.__dict__
      for x in aa: 
          if cdms2.isVariable( aa[x] ): 
             if ( (cdms2.isVariable( aa[x] )) and (canvas.varglist[0] is not None) and (aa[x].name == canvas.varglist[0].name) ):
                slab1 = canvas.varglist[0]
             if ( (cdms2.isVariable( aa[x] )) and (canvas.varglist[1] is not None) and (aa[x].name == canvas.varglist[1].name) ):
                slab2 = canvas.varglist[1]
#      canvas.clear() This was here before I started modifying may need to put back

      if (canvas.SCREEN_MODE() in ["TEDITOR"]):
         template = self.templateeditor.new_template_name
         #template = canvas.varglist[2]
         self.new_gm  = canvas.varglist[3]
         self.gm_name = gm_name = canvas.varglist[4]
         #print "plot_kw = ", plot_kw, self.new_gm, gm_name
         #print "1 - arglist = ", canvas.varglist
         #print "self.templateeditor = ", dir(self.templateeditor)
         #print "The plot display = ", self.templateeditor.plot.list()
         #print "The new template name = ", self.templateeditor.new_template_name
         #print "plot_kw = ", plot_kw.keys()[-1], plot_kw.values()[-1]
         #print "**************  plot_kw = ", plot_kw
         str_txt = self.txt.get()
         lbl_txt = self.txt.cget("label_text")
         template_priority = "self.templateeditor.new_template."+string.lower(lbl_txt)+".priority=1"
         exec(template_priority)
         canvas.plot_annotation(string.lower(lbl_txt), str_txt, self.templateeditor.plot.array[0])
         self.templateeditor.toggle_on_off_for_canvas( self.templateeditor,string.lower(lbl_txt) )
      else:
         self.gm_name = gm_name = self.select_gm_name.getcurselection()[0]
         self.select_gm_name.configure( label_text = 'Select Graphics Method name\nSelected name: ' + self.gm_name);
         selected_display = canvas.get_selected_display()
         if selected_display is None:
             selected_display = vcs_legacy_dpy[0]
         # New way to do this, simply tells it what to update
         canvas.change_display_graphic_method(selected_display,self.new_gm,gm_name)
         # Clear the canvas and replot with updated data
##          canvas.clear()
##          template = canvas.varglist[2]
##          if ( (self.new_gm in ['Vector', 'Scatter', 'Xvsy']) and (slab2 is None)):
##             showerror( "Error Message to User", "Must have two variables to plot %s." % self.new_gm)
##             return     
##          print 'New graphic method:',gm_name,template,self.new_gm,vcs_legacy_dpy[0]
##          if slab2 is None: canvas.plot(slab1, template, self.new_gm, gm_name, sal=0)
##          else: canvas.plot(slab1, slab2, template, self.new_gm, gm_name, sal=0)
         #if slab2 is None: canvas.plot(slab1, template, self.new_gm, gm_name, plot_kw)
         #else: canvas.plot(slab1, slab2, template, self.new_gm, gm_name)

def evt_graphicsmethodgui( self, parent, canvas ):
      canvas.graphicsmethodgui(canvas.varglist[3], canvas.varglist[4]  )

#----------------------------------------------------------------------------------
# Copy the Graphics Method to a new name 
#----------------------------------------------------------------------------------
def copy_graphics_method(self, parent, canvas):
      # Get the graphics method name
      gm_selected = self.new_gm

      # Popup use dialog widget
      dialog = Pmw.PromptDialog(parent, title='Copy %s graphics method' % self.gm_name,
                                label_text='Enter %s Name:' % self.gm_name,
                                entryfield_labelpos='nw',
                                entry_width = 50,
                                defaultbutton=0, buttons=('OK', 'Cancel'))

      # Position dialog popup on top of the main GUI and wait for results
#      parent_geom = self.top_parent.geometry()
#      geom = string.split(parent_geom, '+')
#      geom2= string.split(geom[0],'x')
#      print "geom = ", geom, "geom2 = ", geom2, "parent_geom = ", parent_geom
#      d1 = string.atoi( geom[1] ); d2 = string.atoi( geom[2] )
#      d3 = string.atoi( geom2[0] ); d4 = string.atoi( geom2[1] )
#      d5=d2+(d4/2)
#      print "d1   d4 = ", d1, d2, d3, d4, d5
#
#      result = dialog.activate(globalMode = 'nograb', geometry= "+%d+%d" % (d1, d5) )
      result = dialog.activate(globalMode = 'nograb' )

      if result == 'OK':
         new_name = dialog.component('entry').get()
         if new_name in canvas.listelements( self.new_gm ):
            gui_message.error( 'Dean - Error occurred while trying to copy %s graphics method %s to %s. Make sure the new graphics method name does not already exist.' % (self.gm_name, gm_selected, new_name) )
            return
         try:
            if string.lower( self.new_gm ) == 'boxfill':
               canvas.createboxfill(new_name, self.gm_name)
            elif string.lower( self.new_gm ) == 'isofill':
               canvas.createisofill(new_name, self.gm_name)
            elif string.lower( self.new_gm ) == 'isoline':
               canvas.createisoline(new_name, self.gm_name)
            elif string.lower( self.new_gm ) == 'outfill':
               canvas.createoutfill(new_name, self.gm_name)
            elif string.lower( self.new_gm ) == 'outline':
               canvas.createoutline(new_name, self.gm_name)
            elif string.lower( self.new_gm ) == 'vector':
               canvas.createvector(new_name, self.gm_name)
            elif string.lower( self.new_gm ) == 'scatter':
               canvas.createscatter(new_name, self.gm_name)
            elif string.lower( self.new_gm ) == 'taylordiagram':
               canvas.createtaylordiagram(new_name, self.gm_name)
            elif string.lower( self.new_gm ) == 'xvsy':
               canvas.createxvsy(new_name, self.gm_name)
            elif string.lower( self.new_gm ) == 'xyvsy':
               canvas.createxyvsy(new_name, self.gm_name)
            elif string.lower( self.new_gm ) == 'yxvsx':
               canvas.createyxvsx(new_name, self.gm_name)
            elif string.lower( self.new_gm ) == 'continents':
               canvas.createcontinents(new_name, self.gm_name)
            elif string.lower( self.new_gm ) == 'meshfill':
               canvas.createmeshfill(new_name, self.gm_name)
         except:
            gui_message.error( 'Naomi - Error occurred while trying to copy %s graphics method %s to %s. Make sure the new graphics method name does not already exist.' % (self.new_gm, self.gm_name, new_name) )
            return

      # Redisplay the graphics method list
      self.select_gm_name.setlist( canvas.listelements( self.new_gm ) )

#----------------------------------------------------------------------------------
# Rename the Graphics Method to a new name 
#----------------------------------------------------------------------------------
def rename_graphics_method(self, parent, canvas):
      # Get the graphics method name
      gm_selected = self.new_gm

      # Popup use dialog widget
      dialog = Pmw.PromptDialog(parent, title='Rename %s graphics method' % self.new_gm,
                                label_text='Rename %s to:' % self.gm_name,
                                entryfield_labelpos='nw',
                                entry_width = 50,
                                defaultbutton=0, buttons=('OK', 'Cancel'))

#      parent_geom = self.top_parent.geometry()
#      geom = string.split(parent_geom, '+')
#      geom2= string.split(geom[0],'x')
#      print "geom = ", geom, "geom2 = ", geom2, "parent_geom = ", parent_geom
#      d1 = string.atoi( geom[1] ); d2 = string.atoi( geom[2] )
#      d3 = string.atoi( geom2[0] ); d4 = string.atoi( geom2[1] )
#      d5=d2+(d4/2)
#      print "d1   d4 = ", d1, d2, d3, d4, d5
#
#      result = dialog.activate(globalMode = 'nograb', geometry= "+%d+%d" % (d1, d5) )
      result = dialog.activate(globalMode = 'nograb' )

      if result == 'OK':
         new_name = dialog.component('entry').get()
         if new_name in canvas.listelements( self.new_gm ):
            gui_message.error( 'Error occurred while trying to copy %s graphics method %s to %s. Make sure the new graphics method name does not already exist.' % (self.gm_name, gm_selected, new_name) )
            return
         try:
            if string.lower( self.new_gm ) == 'boxfill':
               r = canvas.getboxfill(self.gm_name)
            elif string.lower( self.new_gm ) == 'isofill':
               r = canvas.getisofill(self.gm_name)
            elif string.lower( self.new_gm ) == 'isoline':
               r = canvas.getisoline(self.gm_name)
            elif string.lower( self.new_gm ) == 'outfill':
               r = canvas.getoutfill(self.gm_name)
            elif string.lower( self.new_gm ) == 'outline':
               r = canvas.getoutline(self.gm_name)
            elif string.lower( self.new_gm ) == 'vector':
               r = canvas.getvector(self.gm_name)
            elif string.lower( self.new_gm ) == 'scatter':
               r = canvas.getscatter(self.gm_name)
            elif string.lower( self.new_gm ) == 'taylordiagram':
               r = canvas.gettaylordiagram(self.gm_name)
            elif string.lower( self.new_gm ) == 'xvsy':
               r = canvas.getxvsy(self.gm_name)
            elif string.lower( self.new_gm ) == 'xyvsy':
               r = canvas.getxyvsy(self.gm_name)
            elif string.lower( self.new_gm ) == 'yxvsx':
               r = canvas.getyxvsx(self.gm_name)
            elif string.lower( self.new_gm ) == 'continents':
               r = canvas.getcontinents(self.gm_name)
            elif string.lower( self.new_gm ) == 'meshfill':
               r = canvas.getmeshfill(self.gm_name)
         except:
            gui_message.error( 'Error occurred while trying to copy %s graphics method %s to %s. Make sure the new graphics method name does not already exist.' % (self.new_gm, gm_name, new_name) )
            return
         r.name = new_name

      # Redisplay the graphics method list
      self.select_gm_name.setlist( canvas.listelements( self.new_gm ) )
#      gm_list = parent.vcs_legacy[ parent.vcs_legacy_id ].listelements( self.gm_name )
#      self.gm_listbox.setlist( gm_list )
#      name_index = gm_list.index( new_name )
#      parent.panelDV.gm_listbox.select_set( name_index )
#      parent.panelDV.gm_listbox.see( name_index )

#----------------------------------------------------------------------------------
# Save the Graphics Method to a script 
#----------------------------------------------------------------------------------
def script_graphics_method(self, parent, canvas):
      # Get the graphics method name
      gm_selected = self.new_gm

      # Show the popup directory dialog
      filetypes = [ ("Python", ".py"), ("VCS", ".scr") ]
      save_dialog = tkFileDialog.asksaveasfilename(master=parent,
                       filetypes=filetypes,
                       title = 'Save %s graphics method %s to file' % (self.new_gm, self.gm_name))

      if save_dialog == '': return
      if (save_dialog[-3:] != '.py') and (save_dialog[-4:] != '.scr'): save_dialog += '.py'

      if string.lower( self.new_gm ) == 'boxfill':
          r = canvas.getboxfill(self.gm_name)
      elif string.lower( self.new_gm ) == 'isofill':
         r = canvas.getisofill(self.gm_name)
      elif string.lower( self.new_gm ) == 'isoline':
         r = canvas.getisoline(self.gm_name)
      elif string.lower( self.new_gm ) == 'outfill':
         r = canvas.getoutfill(self.gm_name)
      elif string.lower( self.new_gm ) == 'outline':
         r = canvas.getoutline(self.gm_name)
      elif string.lower( self.new_gm ) == 'vector':
         r = canvas.getvector(self.gm_name)
      elif string.lower( self.new_gm ) == 'scatter':
         r = canvas.getscatter(self.gm_name)
      elif string.lower( self.new_gm ) == 'taylordiagram':
         r = canvas.gettaylordiagram(self.gm_name)
      elif string.lower( self.new_gm ) == 'xvsy':
         r = canvas.getxvsy(self.gm_name)
      elif string.lower( self.new_gm ) == 'xyvsy':
         r = canvas.getxyvsy(self.gm_name)
      elif string.lower( self.new_gm ) == 'yxvsx':
         r = canvas.getyxvsx(self.gm_name)
      elif string.lower( self.new_gm ) == 'continents':
         r = canvas.getcontinents(self.gm_name)
      elif string.lower( self.new_gm ) == 'meshfill':
         r = canvas.getmeshfill(self.gm_name)

      # Script the graphics method selected in the list
      canvas.scriptobject( r, save_dialog, mode = 'w' )

######################################################################
#  Set up the Command Line GUI                                       #
######################################################################
def command_line_tab_gui( self, page, parent, canvas ):
      framea = Tkinter.Frame( page )
      framea.pack( side='top', fill = 'both', expand = 1 )

      # Set up the entry window for entering commands
      self.shell = Pmw.EntryField(framea,
                                 label_text='>>>',
                                 label_font=('Times', 18, 'bold'),
                                 labelpos='w',
                             entry_background = winColor)
      self.shell.setentry('')
      self.shell.pack(side = 'top', fill='x', expand = 1)
      self.shell.component('entry').bind( "<Return>", gui_control.Command(evt_shell_command, self, parent) )
      self.shell.bind_all("<Up>", gui_control.Command(evt_prevhist_command, self, parent) )
      self.shell.bind_all("<Down>", gui_control.Command(evt_nexthist_command, self, parent) )
      gui_support.balloon.bind( self.shell, "Command Line: \n     Select the left mouse button to enter CDAT commands in this window,\n     then select the <return> key to execute the command. The output from\n     the command will be visible in the scroll window below. \n\nArrow Key functionality:\n     * left arrow key moves the input cursor to the left\n     * right arrow key moves the input cursor to right\n     * up arrow key shows the previous command\n     * down arrow key shows the next command")

      # Set up the standard out and standard error scroll window
      self.scl1 = Pmw.ScrolledText( framea,
                                 label_text='Output',
                                 labelpos='n',
                                 text_wrap='none',
                                 text_background=winColor,
                                 text_foreground='black',
                                 text_height = "10",
                                  )
      self.scl1.pack(side='top', fill = 'both', expand=1 )
      gui_support.balloon.bind( self.scl1, "Output Window: \n     Shows the results of the CDAT commands.")

######################################################################
#  Events classes and functions needed for the Command Line          #
######################################################################
#----------------------------------------------------------------------------------
# Redirect the destination of sys.stdout and sys.stderr to the Shell output window
#----------------------------------------------------------------------------------
class Redirect_Output:
    def __init__(self, displayCommand):
        self.displayCommand = displayCommand
    
    def write(self, text):
        self.displayCommand.insert( 'end', text )
        self.displayCommand.see('end')

#----------------------------------------------------------------------------------
# Get the previous command
#----------------------------------------------------------------------------------
def evt_prevhist_command( self, parent, event ):
      if len(self.history) == 0: return "-- empty --"
      self.histact = self.histact - 1
      if self.histact < 0:
            self.histact = 0
            print "-- begin of history -- "
      prev_command = self.history[self.histact]
      self.shell.setentry(prev_command)

#----------------------------------------------------------------------------------
# Get the next command
#----------------------------------------------------------------------------------
def evt_nexthist_command( self, parent, event ):
      if len(self.history) == 0: return "-- empty --"
      self.histact = self.histact + 1
      if self.histact > (len(self.history) - 1):
            self.histact = len(self.history) - 1
            print "-- end of history -- "
      next_command = self.history[self.histact]
      self.shell.setentry(next_command)

#----------------------------------------------------------------------------------
# Execute the command functions
#----------------------------------------------------------------------------------
def evt_shell_command( self, parent, event ):
      from browser import gui_defined_variables
      self.save_stdout = sys.stdout
      self.save_stderr = sys.stderr
      command = self.shell.get() # get the command statement or expression
      command = string.strip(command)
      if command[:1] == '!': command = command[1:]
      locals =  __main__.__dict__
      globals =  __main__.__dict__
      globals['__privileged__'] = 1
      # Execute the command or return the error
      try:
          sys.stdout = sys.stderr = Redirect_Output(self.scl1) # Redirect stdout and stderr to Shell output window
          code = compile(command + '\n', '<stdin>', 'single')
          exec code in globals, locals
      except:
          if type(sys.exc_type) == type(''):
              exc_type_name = sys.exc_type
          else: exc_type_name = sys.exc_type.__name__
          print '***', exc_type_name + ':', sys.exc_value

      # Save the history of the command
      if command != self.lastcommand: self.lastcommand = command
      self.history.append(command)
      self.histact = len(self.history)

      # Show the variable in the VCDAT "Defined Variable" window
      try: gui_defined_variables.update_defined( ) # Must be from VCDAT
      except: pass

      self.shell.setentry( "" )

      # Get output back to old device
      sys.stdout = self.save_stdout
      sys.stderr = self.save_stderr


#####################################################################################
# Create GUI editor for VCS Canvas                                                  #
#####################################################################################
def create( canvas=None, top_parent=None, got_root=0 ):
    # Create the VCS Canvas GUI
    a = CanvasGUI( canvas, top_parent, got_root )
    return a

######################################################################
#  end of file                                                       #
######################################################################
