#!/usr/bin/env python
# Adapted for numpy/ma/cdms2 by convertcdms.py
#
# The PCMDI's Visual Climate Data Analysis Tools (VCDAT) - tk_cdat module
#
###############################################################################
#                                                                             #
# Module:       tk_cdat module                                                #
#                                                                             #
# Copyright:    "See file Legal.htm for copyright information."               #
#                                                                             #
# Authors:      PCMDI Software Team                                           #
#               Lawrence Livermore NationalLaboratory:                        #
#               support@pcmdi.llnl.gov                                        #
#                                                                             #
# Description:  CDAT GUI command wrapper for PCMDI Software System Browser.   #
#               This is the top level Python/Tkinter file that calls other    #
#               Python/Tkinter files to create the CDAT GUI.                  #
#                                                                             #
#                                                                             #
###############################################################################
#

import Tkinter, Pmw
import os, sys, types, string, time, getopt, cdms2, cdtime, vcs, math
import __main__
import gui_menu
import gui_select_variable
import gui_graphics_control
import gui_dimensions
import gui_defined_variables
import gui_variable_information
import vcs_function
import gui_alter_plot
import gui_functions
from gui_support import gui_color
import gui_control
import gui_extend_menus
import gui_support
Version = gui_support.version()

# Get the previously saved state of the GUI
try:
   fn = '%s/PCMDI_GRAPHICS' % os.environ['HOME']
   sys.path.append(fn)
   import vcdat_initial
except:
   pass

#
# IDLE is looking for a close or exit on its Edit or Shell window.
# So, lets give it what he wants. This stops IDLE from popping up a 
# Tkinter root window on the close or exit of a Edit or Shell window.
#
def closeA(): pass

class TkWaitBar:
    def __init__(self,splash,canvas,bar,width,height,bar_width,bar_height,total):
        self.splash=splash
        self.bar=bar
        self.canvas=canvas
        self.total=total
        self.count=0.
        self.width=width
        self.height=height
        self.bar_width=bar_width
        self.bar_height=bar_height
        self.x0=width*(1.-bar_width)/2.
        
    def update(self,count=None):
        if count is None:
            self.count+=1
        else:
            self.count=count
        self.canvas.coords(self.bar,self.x0,
                           self.height-self.bar_height-10,
                           self.x0+self.width*self.count/self.total*self.bar_width,
                           self.height-10)
        self.splash.update_idletasks()
        
        
usage_msg = """\
The Visual Climate Data Analysis Tools (VCDAT) version %s:\n
usage: vcdat [-h] [-n] [-u] [-f] ...

-h\t\tprint out this message
-n\t\tno splash screen at startup
-u\t\tturn user tracking mode on; that is, log all directories,\n\t\tfiles, and variables requested by the user
-f\t\trun VCDAT Full

The Visual Climate Data Analysis Tools (VCDAT) is the graphical user\ninterface for the Climate Data Analysis Tools (CDAT). It can be used\nfor quickly accessing and computing data, producing a picture that\nvisually represents the data values, refining the picture, and saving\nthe state of a session so that it can be reused later. 

It is important to note that VCDAT does not require learning Python\nand the CDAT software.

For documentation on VCDAT visit the CDAT web site: http://esg.llnl.gov/cdat.\nVCDAT also comes with on-line help.

VCDAT is open source software developed by the Program for Climate\nModel Diagnosis and Intercomparison (PCMDI), Lawrence Livermore\nLaboratory, Livermore, California, 94550, United States of\nAmerica.
""" % (Version,)

def main():
    import gui_support,tkFont
    
    # Get the previously saved state of the GUI
    try:
        fn = '%s/PCMDI_GRAPHICS' % os.environ['HOME']
        sys.path.append(fn)
        import vcdat_initial
    except:
        pass

    # Get VCDAT options
    startusertask = 0
    dosplashscreen = 1
    try:
       vcdat_lite = vcdat_initial.set.vcdat_lite
    except:
       vcdat_lite = 1

    try:
        opts, args = getopt.getopt(sys.argv[1:], "hnuf")
    except getopt.error, msg:
        sys.stderr.write("Error: %s\n\n" % str(msg))
        sys.stderr.write(usage_msg)
        sys.exit(2)

    for o, a in opts:
        if o == '-h':
            sys.stderr.write(usage_msg)
            sys.exit(2)
        if o == '-n':
            dosplashscreen = 0
            counter_bar = None
        if o == '-u':
            startwalltime = time.time()
            startcputime  = time.clock()
            startusertask = 1    # Store user directory, file, and variable requests
        if o == '-f':
            vcdat_lite = 0

    root = gui_support.root()
    root.Version=Version
    root.startusertask = startusertask
    if startusertask == 1: gui_control.start_tracking_directory_file_variable_log( root )

    if dosplashscreen == 1:
       splash = Tkinter.Toplevel()
       splash.withdraw()
       splash.title('Welcome to VCDAT')
       splash.update_idletasks()

       total_load=23.
       already_loaded=0
       img = Tkinter.PhotoImage( file = os.path.join(vcs.__path__[0],'..','..','..','..', 'bin', 'splash.gif') )
       canvas = Tkinter.Canvas(splash,background='white')
       Font=tkFont.Font(splash,family='Helvetica', size=-25, weight='bold')
       Text="""
Visual Climate Data Analysis Tools


Version %s

http://cdat.sf.net







Program for Climate Model Diagnosis and Intercomparison
Lawrence Livermore Laboratory
Livermore, California, 94550
United States of America

Loading...

""" % (Version,)

       # Gets the width of the text
       sp=string.split(Text,'\n')
       text_width=0
##        Font.cget seems to return an int now...
##        text_height=-string.atoi(Font.cget('size'))*len(sp)*1.1
       text_height=-Font.cget('size')*len(sp)*1.1
       for txt in sp:
           Twidth=Font.measure(txt)+10
           text_width=max(text_width,Twidth)
          
       width = splash.winfo_reqwidth()
       height = splash.winfo_reqheight()
       splash_width=img.width()
       splash_height=img.height()
       width=max(width,splash_width)
       width=max(width,text_width)
       height=max(height,splash_height)
       height=max(height,text_height)
       x = (root.winfo_screenwidth() - width) / 2 - root.winfo_vrootx()
       y = (root.winfo_screenheight() - height) / 3 - root.winfo_vrooty()
       if x < 0:
           x = 0
       if y < 0:
           y = 0
       height=height
       geometry = '%dx%d+%d+%d' % (width, height, x, y)
    
       splash.geometry(geometry)
       canvas.create_image(width/2,height/2,image=img,anchor=Tkinter.CENTER)
       text = canvas.create_text(width/2,height/2,
           font=Font,
           justify=Tkinter.CENTER,
           text =Text)
       bar_width=.6
       bar_height=40
       x0=width*(1.-bar_width)/2.
       frame=canvas.create_rectangle(x0,
                                  height-bar_height-10,
                                  width*bar_width+x0,
                                  height-10,
                                  outline='black')
       bar=canvas.create_rectangle(x0,
                                height-bar_height-10,
                                x0+width*already_loaded/total_load*bar_width,
                                height-10,
                                fill='red',
                                outline='black')
       counter_bar=TkWaitBar(splash,canvas,bar,width,height,bar_width,bar_height,total_load)
       canvas.pack(expand=1,fill=Tkinter.BOTH)

       splash.update_idletasks()
       splash.deiconify()
       root.update()

    root.external_plot_functions={}
    root.external_plot = None
    root.config( background=gui_color.one, borderwidth=4 )
    root.option_add('*Font', gui_control.gui_font )
    root.option_add('*Menu*Font', gui_control.menu_font )
    root.option_add('*Menubutton*Font', gui_control.mbutton_font )
    root.title( "The Visual Climate Data Analysis Tools - (VCDAT)" )
    root.file_search_pattern=None
    Pmw.initialise( root )
 #
 #-----------------------------------------------------------------
 # Initialize [ idle ] in the PCMDI browser. Below lets idle know
 # that the PCMDI browser is the parent.
 #-----------------------------------------------------------------
 #
    idle_path = os.path.join(vcs.__path__[0],'..','..','..','..', 'bin','idle')
    sys.path.insert(0,idle_path)
    from idlelib.configHandler import IdleConf
##     idle_dir = os.path.dirname(IdleConf.__file__)
##     IdleConf.load(idle_dir)
    IdleConf.LoadCfgFiles(IdleConf())

    if dosplashscreen == 1: counter_bar.update()

    from idlelib import PyShell
    PyShellEditorWindow=PyShell.PyShellEditorWindow
    PyShellFileList=PyShell.PyShellFileList
 
    root.flist = PyShellFileList( root )
    if dosplashscreen == 1: counter_bar.update()

###  No longer needed in Python version 2.3
###    root.flist.dict['vcdat: place holder for idle'] = root
###    root.close = closeA
###    root.flist.pyshell = None
###    root.flist.inversedict[root] = 'vcdat: place holder for idle'
### In stead of the above set a flag for first time use of IDLE
    root.firstidle = True
    root.save_idle = None

    # Used mainly to update the "Selected" window on the browser
    # and to update the menu on the [ idle ] Edit and Shell windows
    __main__.__dict__['tk_root'] = root
    __main__.__dict__['update_defined'] = gui_defined_variables.update_defined
    __main__.__dict__['update_menu_flg'] = 0
 
 #---------------------------------------------------------------------
 # Create a stopwatch curser
 #---------------------------------------------------------------------
    root.busyCursor = 'watch'
    root.preBusyCursors = None
 
 #---------------------------------------------------------------------
 # Create a Paned Widget and set the four panels inside
 #---------------------------------------------------------------------
    max_w = root.winfo_screenwidth()
    max_h = root.winfo_screenheight()
    root.pane = Pmw.PanedWidget( root, hull_width=max_w*0.42, hull_height=max_h*0.783203125, orient = 'vertical' )
    if dosplashscreen == 1: counter_bar.update()
    try:
       root.pane1_min = pane1_min = vcdat_initial.set.pane1_min_position
    except:
       root.pane1_min = pane1_min = 27
    try:
       root.pane1_max = pane1_max = vcdat_initial.set.pane1_max_position
    except:
       root.pane1_max = pane1_max=145
    #
    try:
       root.pane2_min = pane2_min = vcdat_initial.set.pane2_min_position
    except:
       root.pane2_min = pane2_min = 40
    try:
       root.pane2_max = pane2_max = vcdat_initial.set.pane2_max_position
    except:
       root.pane2_max = pane2_max = 40
    try:
       root.pane2_size = pane2_size = vcdat_initial.set.pane2_size_position
    except:
       root.pane2_size = pane2_size = 40
    #
    try:
       root.pane3_min = pane3_min = vcdat_initial.set.pane3_min_position
    except:
       root.pane3_min = pane3_min = 30
    try:
       root.pane3_size = pane3_size = vcdat_initial.set.pane3_size_position
    except:
       root.pane3_size = pane3_size = 270
    #
    try:
       root.pane4_min = pane4_min = vcdat_initial.set.pane4_min_position
    except:
       root.pane4_min = pane4_min = 50
    try:
       root.pane4_size = pane4_size = vcdat_initial.set.pane4_size_position
    except:
       root.pane4_size = pane4_size = 295
    try:
       root.pane5_min = pane5_min = vcdat_initial.set.pane5_min_position
    except:
       root.pane5_min = pane5_min = 30
    if dosplashscreen == 1: counter_bar.update()
 
    root.pane.add( 'panelSV', min=pane1_min, max=pane1_max )
    if dosplashscreen == 1: counter_bar.update()
    root.pane.add( 'panelGC', min=pane2_min, max=pane2_max, size=pane2_size )
    if dosplashscreen == 1: counter_bar.update()
    root.pane.add( 'panelDM', min=pane3_min, size=pane3_size )
    if dosplashscreen == 1: counter_bar.update()
    root.pane.add( 'panelDV', min=pane4_min, size=pane4_size )
    if dosplashscreen == 1: counter_bar.update()
    root.pane.add( 'panelVI', min=pane5_min )
    if dosplashscreen == 1: counter_bar.update()
    root.pane.pack( expand = 1, fill='both' )
    if dosplashscreen == 1: counter_bar.update()
 
 #---------------------------------------------------------------------
 # Set Paned window highlight colors
 #---------------------------------------------------------------------
    e = root.pane.pane( 'panelSV' )
    e.config( background=gui_color.one )
    e = root.pane.pane( 'panelGC' )
    e.config( background=gui_color.one )
    e = root.pane.pane( 'panelDM' )
    e.config( background=gui_color.one )
    e = root.pane.pane( 'panelDV' )
    e.config( background=gui_color.one )
    e = root.pane.pane( 'panelVI' )
    e.config( background=gui_color.one )
 
 #---------------------------------------------------------------------
 # Create username, password, and URL information for this session
 #---------------------------------------------------------------------
    root.username = None
    root.password = None
    root.auth_realm = None
    root.auth_host = None

 #---------------------------------------------------------------------
 # Create a Menu-Bar widget at top of root widget
 #---------------------------------------------------------------------
    root.menu = gui_menu.create( root )
    root.menu2 = gui_menu.create2( root )
    root.menu2.main_menu2.forget()
 #   print 'root.menu = ', dir(root.menu)
 
    if dosplashscreen == 1: counter_bar.update()
 #---------------------------------------------------------------------
 # Create the 'Select Variable' panel (i.e., 1st under the menu bar)
 #---------------------------------------------------------------------
    root.panelSV = gui_select_variable.create( root )
    if dosplashscreen == 1: counter_bar.update()
 #   print 'root.panelSV = ', dir(root.panelSV)
 
 #---------------------------------------------------------------------
 # Initialize the VCS grapics packages
 #---------------------------------------------------------------------
    vcs_function.initialize( root , dosplashscreen, counter_bar)
    gui_alter_plot.initialize( root )  # set plot's visual default settings
    __main__.__dict__['vcs_hook1'] = root.vcs[0] # IDLE's connection to VCS
    if root.menu.vcs_canvas_gui_settings_flg == 0:
       __main__.__dict__['vcs_hook2'] = root.vcs[1] # IDLE's connection to VCS
       __main__.__dict__['vcs_hook3'] = root.vcs[2] # IDLE's connection to VCS
       __main__.__dict__['vcs_hook4'] = root.vcs[3] # IDLE's connection to VCS
 
 #---------------------------------------------------------------------
 # Create the 'Graphics Control' panel (ie. 2nd under the menu bar)
 #---------------------------------------------------------------------
    root.panelGC = gui_graphics_control.create( root )
    if dosplashscreen == 1: counter_bar.update()
 #   print 'root.panelGC = ', dir(root.panelGC)
 
 #---------------------------------------------------------------------
 # Create the 'Dimension 1-to-ndim' panel (ie. 3rd under the menu bar)
 #---------------------------------------------------------------------
    root.panelDM = gui_dimensions.create( root )
    root.panelDM.var3 = None
    gui_functions._blank_dim1_to_ndim( root ) # Don't show the dimension scroll windows
    if dosplashscreen == 1: counter_bar.update()
 #   print 'root.panelDM = ', dir(root.panelDM)
 #   print 'root.panelDM.dim[0] = ', dir(root.panelDM.dim[0])
 
 #---------------------------------------------------------------------
 # Create the 'Defined Variables' panel (ie. 4th under the menu bar)
 #---------------------------------------------------------------------
    root.panelDV = gui_defined_variables.create( root )
    gui_graphics_control.show_and_unshow_graphics_options( root ) # disable menu items
    if dosplashscreen == 1: counter_bar.update()
 #   print 'root.panelDV = ', dir(root.panelDV)
 
 #--------------------------------------------------------------------------------------------
 # Create the 'Variable Information' and 'Page Layout' panel (ie. 4th under the menu bar)
 #--------------------------------------------------------------------------------------------
    root.panelVI = gui_variable_information.create( root )
    if dosplashscreen == 1: counter_bar.update()
    root.pl = gui_variable_information.create_page_description_panel( root )
    root.pl.create_form( root, None, vcs=root.vcs[0])

    # Show the Page Layout window in place of the Variable Information or 
    # show the Variable Information window in place of the Page Layout.
    if root.show_template_graphics_method == 1:
       root.pl.scl_frame.pack_forget( )
       root.panelVI.scl1.pack( fill = 'both', expand=1, pady=3 )
    else:
       root.panelVI.scl1.pack_forget( )
       root.pl.scl_frame.pack( side='top', fill='both' )
 #   print 'root.panelVI = ', dir(root.panelVI)
 
 #---------------------------------------------------------------------
 # Restore User Menus and Their Items on the Main Menu Bar
 #---------------------------------------------------------------------
    gui_extend_menus.restore_menus( root )
 
 #---------------------------------------------------------------------
 # Reset the dimension aliases
 #---------------------------------------------------------------------
    cdms_list = {0:cdms2.axis.longitude_aliases, 1:cdms2.axis.latitude_aliases, 
                2:cdms2.axis.time_aliases, 3:cdms2.axis.level_aliases}
    try:
       dim_list = {0:vcdat_initial.set.longitude_aliases, 1:vcdat_initial.set.latitude_aliases,
                2:vcdat_initial.set.time_aliases, 3:vcdat_initial.set.level_aliases}
       for i in range( 4 ):
          for j in range( len(dim_list[i]) ):
             if dim_list[i][j] not in cdms_list[i]:
                 cdms_list[i].append( dim_list[i][j] )
 
       # Update VCDAT's alias list
       gui_control.longitude_alias = gui_control.longitude_alias + cdms2.axis.longitude_aliases
       gui_control.latitude_alias  = gui_control.latitude_alias + cdms2.axis.latitude_aliases
       gui_control.level_alias     = gui_control.level_alias + cdms2.axis.level_aliases
       gui_control.time_alias      = gui_control.time_alias + cdms2.axis.time_aliases
    except:
       pass
 
 #---------------------------------------------------------------------
 # Reset the favorite directories
 #---------------------------------------------------------------------
    try:
       f = open('%s/PCMDI_GRAPHICS/bookmark_directory_files.py' % os.environ['HOME'])
       while 1:
            a=f.readline()
            if a == '': break
            gui_control.favorite_directories.append( a[0:-1] )

       gui_control.favorite_directories.sort()

       gui_control.favorite_index = None
       if len( gui_control.favorite_directories ) > 0:
          gui_control.favorite_index = 0
    except:
       pass
    
 #---------------------------------------------------------------------
 # Reset the favorite files
 #---------------------------------------------------------------------
    try:
       f = open('%s/PCMDI_GRAPHICS/bookmark_files.py' % os.environ['HOME'])
       while 1:
            a=f.readline()
            if a == '': break
            gui_control.favorite_files.append( a[0:-1] )

       gui_control.favorite_files.sort()

       gui_control.favorite_files_index = None
       if len( gui_control.favorite_files ) > 0:
          gui_control.favorite_files_index = 0
    except:
       pass
 
 #---------------------------------------------------------------------
 # Set the swap size for retrieving files
 #---------------------------------------------------------------------
    try:
       root.swap_size =  vcdat_initial.set.swap_size
    except:
       root.swap_size = 250000000  # Most machines can handle 250MB
 
 #---------------------------------------------------------------------
 # Create a table to hold the cursors for widgets
 # which get changed when we go busy
 #---------------------------------------------------------------------
    root.busyWidgets = ( root, root.panelVI.scl1.component('text'), root.panelSV.tin2.component('entry'),
                         root.panelSV.tin3.component('entry'), root.panelSV.tin4.component('entry') )
    if dosplashscreen == 1: counter_bar.update()
 
 #---------------------------------------------------------------------
 # Position the data browser at desired location on screen (i.e., the
 # lower right side.
 #---------------------------------------------------------------------
    max_w = root.winfo_screenwidth()
    max_h = root.winfo_screenheight()
 
    #                    Set the GUI Width
    try:
       win_w = root.win_w = vcdat_initial.set.gui_width
    except:
       win_w = root.win_w = root.pane.winfo_reqwidth()
    if type(win_w) != types.IntType:
       win_w = root.win_w = root.pane.winfo_reqwidth()
 
    #                    Set the GUI Height
    try:
       win_h = root.win_h = vcdat_initial.set.gui_height
    except:
       win_h = root.win_h = root.pane.winfo_reqheight()
    if type(win_h) != types.IntType:
       win_h = root.win_h = root.pane.winfo_reqheight()
 
    #                    Set the GUI X Position
    try:
       dxwin = root.dxwin = vcdat_initial.set.gui_x_position
    except:
       dxwin = root.dxwin = int(max_w - (win_w*1.1))
    if type(dxwin) != types.IntType:
       dxwin = root.dxwin = (max_w - win_w)
 
    #                    Set the GUI Y Position
    try:
       dywin = root.dywin = vcdat_initial.set.gui_y_position
    except:
       dywin = root.dywin = int(max_h-(win_h*1.1))
    if type(dywin) != types.IntType:
       dywin = root.dywin = (max_h-win_h)
 
    pos = '%dx%d+%d+%d' % (win_w, win_h, dxwin, dywin)
    root.geometry(pos)
    if dosplashscreen == 1:
       counter_bar.update()
       splash.destroy()

 #----------------------------------------------------------------------------
 # Import the standard CDAT packages (i.e., cdms2, vcs, genutil, and cdutil).
 # Also, import numpy, numpy.ma, and MV2 automatically for the users, and use
 # the MV2 math package for new users.
 #----------------------------------------------------------------------------
    exec( "import cdms2", __main__.__dict__ )
    exec( "import vcs", __main__.__dict__ )
    exec( "import genutil", __main__.__dict__ )
    exec( "import cdutil", __main__.__dict__ )
    exec( "import numpy", __main__.__dict__ )
    exec( "import numpy.ma", __main__.__dict__ )
    exec( "import MV2", __main__.__dict__ )
    exec( "from MV2 import *", __main__.__dict__ )

 #---------------------------------------------------------------------
 # Enter the Tkinter event loop.
 #---------------------------------------------------------------------
    root.vcdat_lite = vcdat_lite
    if vcdat_lite == 1:
       root.pane.configurepane('panelDV', min=0, max=0)
       root.pane.configurepane('panelVI', min=0, max=0)
       root.panelGC.opt_btn.forget()
       root.panelGC.define_btn.forget()
       root.menu.main_menu.forget()
       root.menu2.main_menu2.pack(side='top', before=root.pane, fill='x')
       pos = '%dx%d+%d+%d' % (win_w, win_h*.6, dxwin, dywin)
       root.geometry(pos)

    root.deiconify() 
    root.update()
    root.lift()
    if len(sys.argv)>1:
       pth, f = os.path.split(sys.argv[1])
       if pth == '' : pth='.'
       root.panelSV.tin2.setentry(pth)
       gui_select_variable.evt_enter_directory(root,None)
       root.panelSV.tin3.selectitem(f)
       gui_select_variable.evt_enter_file(root,None)
       if len(sys.argv)>2:
          root.panelSV.tin4.setentry(sys.argv[2])
          gui_select_variable.evt_enter_variable(root,None)

    root.mainloop()
    if root.startusertask == 1:
       gui_control.track_user( root, "#______________________________________________________________________________")
       gui_control.track_user( root, "End Session Time: %s" % time.asctime())
       gui_control.track_user( root, "Total Session Wall-Clock Time in Minutes: %g" % ((int(time.time()-startwalltime))/60.0))
       gui_control.track_user( root, "Total Session Processor Time in Minutes: %g" % ((int(time.clock()-startcputime))/60.0))

if __name__ == "__main__":
    main()

#---------------------------------------------------------------------
# End of File
#---------------------------------------------------------------------

