# Adapted for numpy/ma/cdms2 by convertcdms.py
"""
# Template Editor GUI Module
"""
###############################################################################
#                                                                             #
# Module:       Template Editor GUI Module                                    #
#                                                                             #
# Copyright:    2000, Regents of the University of California                 #
#               This software may not be distributed to others without        #
#               permission of the authors.                                    #
#                                                                             #
# Authors:      PCMDI Software Team                                           #
#               Lawrence Livermore National Laboratory                        #
#               support@pcmdi.llnl.gov                                        #
#                                                                             #
# Description:  Picture Template editor for VCS graphics. The picture template#
#               is defined in VCS as one of three a primary objects in VCS. The
#               other two primary objects are data and graphics methods.      #
#                                                                             #
#               Picture template attributes describe where segment of a picture
#               will be displayed. The segments are graphics representations  #
#               of: textual identification of the data; formatted values of   #
#               single-valued dimensions and mean, maximum, and minimum data  #
#               values: axes tick marks and labels, boxes and lines; a legend #
#               that is graphics method specific; and the data. Picture       #
#               templates also describe how to display all but the data and   #
#               legend.                                                       #
#                                                                             #
#               The picture template editor GUI allows the user to design     #
#               templates for either portrait or landscape orientation plots. #
#                                                                             #
#                                                                             #
#                                                                             #
# Version:      4.0                                                           #
#                                                                             #
###############################################################################
#
#
###############################################################################
#
# Import: VCS C extension module.                                             #
#                                                                             #
###############################################################################

import Tkinter,Pmw, tkFileDialog
from browser import gui_control, gui_message, gui_busy
import Canvas
import cdms2 as cdms
import vcs
import _vcs
import __main__
import sys, os, string
import gui_support
from random import randint
from gui_support import gui_color
from tkMessageBox import askyesno, showerror
import fonteditorgui
import lineeditorgui
import numpy

# The color for the toggle button when the object is highlighted (i.e., selected)
# in the VCS Canvas.
hot_color = "green"

def get_slab_info(parent, canvas, attr_name, label_type):
     # Write slab information to text window
     slab1 = None
     slab2 = None
     attr_str = " "
     aa = __main__.__dict__
     try:
        if (parent.menu.vcs_canvas_gui_settings_flg == 1): # Must be from VCDAT
           slab1 = canvas.varglist[0] = canvas.canvas_gui.slab1
           slab2 = canvas.varglist[1] = canvas.canvas_gui.slab2
           template = canvas.canvas_gui.template
           g_type = canvas.canvas_gui.g_type
           g_name = canvas.canvas_gui.g_name
     except:
        for x in aa:
            if cdms.isVariable( aa[x] ):
               if ( (cdms.isVariable( aa[x] )) and (canvas.varglist[0] is not None) and (aa[x].name == canvas.varglist[0].name) ):
                  slab1 = aa[x]
               if ( (cdms.isVariable( aa[x] )) and (canvas.varglist[1] is not None) and (aa[x].name == canvas.varglist[1].name) ):
                  slab2 = aa[x]
     if attr_name in ["dataname"]:
           attr_str = slab1.getattribute( "id" )
     elif attr_name in ["title"]:
           attr_str = slab1.getattribute( "long_name" )
     elif attr_name in ["xname"]:
           attr_str=slab1.getAxis(len(slab1.shape)-1).id
     elif attr_name in ["yname"]:
           attr_str=slab1.getAxis(len(slab1.shape)-2).id
     elif attr_name in ["zname"]:
           attr_str=slab1.getAxis(len(slab1.shape)-3).id
     elif attr_name in ["tname"]:
           attr_str=slab1.getAxis(len(slab1.shape)-4).id
     else:
           attr_str = slab1.getattribute( attr_name )

     return attr_name, attr_str

# Dictionary of checkbutton values.  This is for use 
#   in keeping track of which values are turned on and off
class template_state:
    def __init__(self):
        self.checkbutton = { 'dataname'  : 0,
                    'title'     : 0,
                    'source'    : 0,
                    'file'      : 0,
                    'units'     : 0,
                    'crdate'    : 0,
                    'crtime'    : 0,
                    'comment1'  : 0,
                    'comment2'  : 0,
                    'comment3'  : 0,
                    'comment4'  : 0,
                    #'function'  : 0,
                    'xlabel1'   : 0,
                    'xlabel2'   : 0,
                    'ylabel1'   : 0,
                    'ylabel2'   : 0,
                    'xtic1'     : 0,
                    'xtic2'     : 0,
                    'ytic1'     : 0,
                    'ytic2'     : 0,
                    'xmintic1'  : 0,
                    'xmintic2'  : 0,
                    'ymintic1'  : 0,
                    'ymintic2'  : 0,
                    'xname'     : 0,
                    'yname'     : 0,
                    'zname'     : 0,
                    'tname'     : 0,
                    'xunits'    : 0,
                    'yunits'    : 0,
                    'zunits'    : 0,
                    'tunits'    : 0,
                    'box1'      : 0,
                    'box2'      : 0,
                    'box3'      : 0,
                    'box4'      : 0,
                    'line1'     : 0, 
                    'line2'     : 0,
                    'line3'     : 0,
                    'line4'     : 0,
                    'data'      : 0,
                    'legend'    : 0,
                    'min'       : 0,
                    'max'       : 0,
                    'mean'      : 0,
                    'xvalue'    : 0,
                    'yvalue'    : 0,
                    'zvalue'    : 0,
                    'tvalue'    : 0 }
                    
class Template_Editor:
    def __init__(self,gui_parent=None, canvas = None, plot=None, template_name = 'quick', template_orig_name = 'quick', graphics_method = 'isofill', graphics_method_name = 'default', called_from = 0):

        # Turn on instant replotting
        self.mode = 1 
        # Set and initialize template data information
        self.parent = gui_parent
        self.info = template_state()
        self.template_name = template_name
        self.template_orig_name = template_orig_name
        self.finished_gui = 0

        # These are used for the sample plots only
        self.gm = graphics_method
        self.gm_name = graphics_method_name
        self.plot = plot
        if canvas is None:
            canvas = Canvas.Canvas()
        self.canvas = canvas

        self.plot_that_are_on = []
        if (self.parent is not None):
           try: self.plot_that_are_on = self.parent.panelGC.which_plot_is_on( self.parent )
           except: pass
        self.show_canvas() # Show the canvas with the data and template but only if not coming from switching

	    # Associate the VCS Canvas with the the template editor
        self.canvas.canvas_template_editor = self

        # Open template and initialize template copy
        self.get_template()

        # Turn re-plotting of data off until after we have initialized
        #  the template editor checkbuttons
        self.plot.parent.mode = 0
        self.dialog = gui_support.VcsDialog(
             title = 'Template Editor: '+template_orig_name,
             buttons = ('Apply', 'Revert', 'Cancel', 'Dismiss'),
             defaultbutton='Apply',
             command = self.execute
             )
        self.dialog.dialog.withdraw()

        # Remove the buttons at the bottom if the canvas has a GUI around it
        if canvas.canvas_gui is not None: self.dialog.dialog.configure(buttons = ())
        
        # Create menu bar and menu bar items
        self.create_menu_bar()
        
        # Create notebook 
        notebook = Pmw.NoteBook(self.dialog.interior())

        #-----------------------------------------------------
        # Create a page for labels
        #-----------------------------------------------------
        page = notebook.add('Labels')
        self.create_labels(page)
        #-----------------------------------------------------
        # Create a page for Axis
        #-----------------------------------------------------
        page = notebook.add('Axis')
        self.create_axis(page)
        #-----------------------------------------------------
        # Create a page for Borders and Lines
        #-----------------------------------------------------
        page = notebook.add('Borders/Lines')
        self.create_borders_and_lines(page)
        #-----------------------------------------------------
        # Create a page for Data/Legend
        #-----------------------------------------------------
        page = notebook.add('Data/Legend')
        self.create_data_and_legend(page)

        notebook.pack(fill='both', expand = 1)
        notebook.selectpage('Labels')
        notebook.setnaturalsize()

        # Decide where to put it on the screen
        if gui_parent is None:
            d=[int(self.dialog.dialog.winfo_screenwidth()/6),
               int(self.dialog.dialog.winfo_screenheight()/6)
              ]
            self.dialog.geometry("650x640+%s+%s" % (d[0],d[1]))
            self.no_gui_parent = 1
        else:
            self.dialog.position_over(gui_parent)
            self.dialog.geometry("650x640")
            self.no_gui_parent = 0

        if (called_from ==0): self.dialog.dialog.deiconify()   # Show the template editor GUI

        # Initialize the template editors status 
        self.changed = 0
        self.finished_gui = 1
        self.notebook = notebook
        self.nopopupswitch = False
        
        # Make sure everything is unselected, possibly from a previous template edit
        self.canvas._unselect_all()
        #        self.canvas.flush()
        #        self.canvas.backing_store()

        self.vcssavedialog = Pmw.Dialog(self.dialog.interior(),
                                        title = 'Save changes?',
                                        buttons = ('Save', 'Discard'),
                                        command = gui_control.Command(self._ask_save))
        Tkinter.Label(self.vcssavedialog.interior(),
                      text = 'Changes have been made to the template.\n Do you want to save your changes?').pack(expand = 1, fill = 'x', padx = 10, pady = 10)
        
        self.askagain = Pmw.RadioSelect(self.vcssavedialog.interior(),
                                        buttontype = 'checkbutton',)
        self.askagain.add('Do not ask me again')
        self.askagain.pack()
        self.vcssavedialog.withdraw()
        
        ####        self.dialog.dialog.lift()
        

    #-----------------------------------------------------
    # Populate the 'Labels' tab 
    #-----------------------------------------------------
    def create_labels(self,page):

        #----------------------------------------------
        # If too much is on a page, I may need to 
        # add a scroll window
        #----------------------------------------------
        scl_Frame=Pmw.ScrolledFrame(page,
                                labelpos = 'n',
                                label_text = 'Labels Editor',
                                )
        scl_Frame.pack(fill = 'both', expand = 1, side='top')
        page=scl_Frame.interior()

        #----------------------------------------------
        # Populate "Standard labels" group
        #  (left side)
        #----------------------------------------------
        group1 = Pmw.Group(page,
                        tag_text = 'Standard labels',
                        tag_font = ('helvetica',10,'bold'),
                        tagindent = 10)
        master = Tkinter.Frame(group1.interior())

        #Tkinter.Label(master,
                        #font=('helvetica',10),
                        #text = 'Modify').grid(row=0,column=0)
        Tkinter.Label(master,
                        font=('helvetica',10),
                        text = 'On/Off').grid(row=0,column=1)

        # Number of rows per attribute
        Interval = 1
        self.dataname = create_attribute('dataname', self, master, x1 = 'X', y1 = 'Y', row = Interval*1)
        self.title = create_attribute('title', self, master, x1 = 'X', y1 = 'Y', row = Interval*2)
        self.source = create_attribute('source', self, master, x1 = 'X', y1 = 'Y', row = Interval*3) 
        self.file = create_attribute('file', self, master, x1 = 'X', y1 = 'Y', row = Interval*4)
        master.grid(row = 0,column = 0,padx = 5,sticky = 'nwe')

        #----------------------------------------------
        # Insert spacer for asthetics
        #----------------------------------------------
        spacer(group1.interior()).grid(row=0,column=1,padx=10,sticky='ns')

        #----------------------------------------------
        #  (right side)
        #----------------------------------------------
        master = Tkinter.Frame(group1.interior())
        #Tkinter.Label(master,
                        #font=('helvetica',10),
                        #text = 'Modify').grid(row=0,column=0)
        Tkinter.Label(master,
                        font=('helvetica',10),
                        text = 'On/Off').grid(row=0,column=1)

        Interval = 1

        self.units = create_attribute('units', self, master, x1 = 'X', y1 = 'Y', row = Interval*1)
        self.crdate = create_attribute('crdate', self, master, x1 = 'X', y1 = 'Y', row = Interval*2)
        self.crtime = create_attribute('crtime', self, master, x1 = 'X', y1 = 'Y', row = Interval*3)
        master.grid(row = 0,column = 2,padx = 5,sticky = 'nwe')

        group1.grid(row = 0,column = 0,columnspan = 1,ipadx=5,ipady=5,padx = 5,pady=5,sticky = 'nsew')

        #----------------------------------------------
        # Populate "Additional Text fields" group
        # Current members
        #----------------------------------------------
        group2 = Pmw.Group(page,
                        tag_text = 'Additional text fields',
                        tag_font = ('helvetica',10,'bold'),
                        tagindent = 10)
        master = group2.interior()

        #Tkinter.Label(master,
                        #font=('helvetica',10),
                        #text = 'Modify').grid(row=0,column=0)
        Tkinter.Label(master,
                        font=('helvetica',10),
                        text = 'On/Off').grid(row=0,column=1)

        Interval = 1
        self.comment1 = create_attribute('comment1', self, master, x1 = 'X', y1 = 'Y', row = Interval*1)
        self.comment2 = create_attribute('comment2', self, master, x1 = 'X', y1 = 'Y', row = Interval*2)
        self.comment3 = create_attribute('comment3', self, master, x1 = 'X', y1 = 'Y', row = Interval*3)
        self.comment4 = create_attribute('comment4', self, master, x1 = 'X', y1 = 'Y', row = Interval*4)
        group2.grid(row = 2, column = 0,padx = 5,pady=5,ipadx = 5, ipady = 5,sticky = 'nw')


    def create_borders_and_lines(self,page):
        #----------------------------------------------
        # If too much is on a page, I may need to 
        # add a scroll window
        #----------------------------------------------
        scl_Frame=Pmw.ScrolledFrame(page,
                                labelpos = 'n',
                                label_text = 'Borders/Lines Editor',
                                )
        scl_Frame.pack(fill = 'both', expand = 1,side='top')
        page=scl_Frame.interior()

        #----------------------------------------------
        # Populate "Borders" group
        # Current members
        #----------------------------------------------
        group1 = Pmw.Group(page,
                        tag_text = 'Borders',
                        tag_font = ('helvetica',10,'bold'),
                        tagindent = 20)
        master = group1.interior()

        #Tkinter.Label(master,
                        #font=('helvetica',10),
                        #text = 'Modify').grid(row=0, column=0)
        Tkinter.Label(master,
                        font=('helvetica',10),
                        text = 'On/Off').grid(row=0, column=1)
        Interval = 3
        self.box1 = create_attribute('box1', self, master, x1 = 'X1', x2 = 'X2', y1 = 'Y1', y2 = 'Y2', row = Interval*1)
        self.box2 = create_attribute('box2', self, master, x1 = 'X1', x2 = 'X2', y1 = 'Y1', y2 = 'Y2', row = Interval*2)
        self.box3 = create_attribute('box3', self, master, x1 = 'X1', x2 = 'X2', y1 = 'Y1', y2 = 'Y2', row = Interval*3)
        self.box4 = create_attribute('box4', self, master, x1 = 'X1', x2 = 'X2', y1 = 'Y1', y2 = 'Y2', row = Interval*4)
        group1.grid(row=0,column=0,ipadx = 5,ipady = 5,padx = 10,pady=5)
         
        #----------------------------------------------
        # Populate "Lines" group
        # Current members
        #----------------------------------------------
        group2 = Pmw.Group(page,
                        tag_text = 'Lines',
                        tag_font = ('helvetica',10,'bold'),
                        tagindent = 20)
        master = group2.interior()

        #Tkinter.Label(master,
                        #font=('helvetica',10),
                        #text = 'Modify').grid(row=0,column=0)
        Tkinter.Label(master,
                        font=('helvetica',10),
                        text = 'On/Off').grid(row=0,column=1)

        Interval = 3 
        self.line1 = create_attribute('line1', self, master, x1 = 'X1', x2 = 'X2', y1 = 'Y1', y2 = 'Y2', row = Interval*1)
        self.line2 = create_attribute('line2', self, master, x1 = 'X1', x2 = 'X2', y1 = 'Y1', y2 = 'Y2', row = Interval*2)
        self.line3 = create_attribute('line3', self, master, x1 = 'X1', x2 = 'X2', y1 = 'Y1', y2 = 'Y2', row = Interval*3)
        self.line4 = create_attribute('line4', self, master, x1 = 'X1', x2 = 'X2', y1 = 'Y1', y2 = 'Y2', row = Interval*4)
        group2.grid(row=0,column=1,ipadx = 5,ipady=5,padx = 10,pady=5,sticky = 'nsew')
        #Tkinter.Button(page,text = 'Modify Selected').grid(row = 2,column = 0)

    def create_axis(self,page):
        #----------------------------------------------
        # If too much is on a page, I may need to 
        # add a scroll window
        #----------------------------------------------
        scl_Frame=Pmw.ScrolledFrame(page,
                                labelpos = 'n',
                                label_text = 'Axis Editor',
                                )
        scl_Frame.pack(fill = 'both', expand = 1,side='top')
        page=scl_Frame.interior()

        #---------------------------------------------------------
        # Create "Axis labels" group
        #---------------------------------------------------------
        group1 = Pmw.Group(page,
                        tag_text = 'Axis labels',
                        tag_font = ('helvetica',10,'bold'),
                        tagindent = 10)
        master = group1.interior()

        #Tkinter.Label(master,
                        #font=('helvetica',10),
                        #text = 'Modify').grid(row=0,column=0)
        Tkinter.Label(master,
                        font=('helvetica',10),
                        text = 'On/Off').grid(row=0,column=1)

        Interval = 1
        self.xname = create_attribute('xname', self, master, x1 = 'X', y1 = 'Y', row = Interval*1)
        self.yname = create_attribute('yname', self, master, x1 = 'X', y1 = 'Y', row = Interval*2)
        self.zname = create_attribute('zname', self, master, x1 = 'X', y1 = 'Y', row = Interval*3)
        self.tname = create_attribute('tname', self, master, x1 = 'X', y1 = 'Y', row = Interval*4)

        #---------------------------------------------------------
        # Axis units group 
        #---------------------------------------------------------
        group2 = Pmw.Group(page,
                        tag_text = 'Axis units',
                        tag_font = ('helvetica',10,'bold'),
                        tagindent = 10)
        master = group2.interior()

        #Tkinter.Label(master,
                        #font=('helvetica',10),
                        #text = 'Modify').grid(row=0,column=0)
        Tkinter.Label(master,
                        font=('helvetica',10),
                        text = 'On/Off').grid(row=0,column=1)

        Interval = 1
        self.xunits = create_attribute('xunits', self, master, x1 = 'X', y1 = 'Y', row = Interval*1)
        self.yunits = create_attribute('yunits', self, master, x1 = 'X', y1 = 'Y', row = Interval*2)
        self.zunits = create_attribute('zunits', self, master, x1 = 'X', y1 = 'Y', row = Interval*3)
        self.tunits = create_attribute('tunits', self, master, x1 = 'X', y1 = 'Y', row = Interval*4)

        #-----------------------------------------------------
        # Setup group3
        #-----------------------------------------------------
        group3 = Pmw.Group(page,
                        tag_text = 'Tickmark label location\n(top/bottom and left/right)',
                        tag_font = ('helvetica',10,'bold'),
                        tagindent = 10)

        master = group3.interior()
        tempframe = Tkinter.Frame(master)

        #Tkinter.Label(tempframe,
                        #font=('helvetica',10),
                        #text = 'Modify').grid(row=0,column=0)
        Tkinter.Label(tempframe,
                        font=('helvetica',10),
                        text = 'On/Off').grid(row=0,column=1)

        Interval = 1
        self.xlabel1 = create_attribute('xlabel1', self, tempframe, y1 = 'Y', row = Interval*1)
        self.xlabel2 = create_attribute('xlabel2', self, tempframe, y1 = 'Y', row = Interval*2)
        tempframe.grid(row = 0, column = 0,sticky = 'nw')

        # Insert spacer for asthetics
        spacer(master).grid(row=0,column=1,padx=10)

        tempframe = Tkinter.Frame(master)
        #Tkinter.Label(tempframe,
                        #font=('helvetica',10),
                        #text = 'Modify').grid(row=0,column=0)
        Tkinter.Label(tempframe,
                        font=('helvetica',10),
                        text = 'On/Off').grid(row=0,column=1)
        self.ylabel1 = create_attribute('ylabel1',self, tempframe, x1 = 'X', row = Interval*1)
        self.ylabel2 = create_attribute('ylabel2', self, tempframe, x1 = 'X', row = Interval*2)
        tempframe.grid(row = 0, column = 2,sticky = 'ne')


        #---------------------------------------------------------
        # Populate group4
        #---------------------------------------------------------
        group4 = Pmw.Group(page,
                        tag_text = 'Major tickmarks',
                        tag_font = ('helvetica',10,'bold'),
                        tagindent = 10)
        master = group4.interior()

        #Tkinter.Label(master,
                        #font=('helvetica',10),
                        #text = 'Modify').grid(row=0,column=0)
        Tkinter.Label(master,
                        font=('helvetica',10),
                        text = 'On/Off').grid(row=0,column=1)

        Interval = 1
        self.xtic1 = create_attribute('xtic1', self, master, y1 = 'Y1', y2 = 'Y2', row = Interval*1)
        self.xtic2 = create_attribute('xtic2', self, master, y1 = 'Y1', y2 = 'Y2', row = Interval*2)
        self.ytic1 = create_attribute('ytic1', self, master, x1 = 'X1', x2 = 'X2', row = Interval*3)
        self.ytic2 = create_attribute('ytic2', self, master, x1 = 'X1', x2 = 'X2', row = Interval*4)

        #---------------------------------------------------------
        # Populate group5
        #---------------------------------------------------------
        group5 = Pmw.Group(page,
                        tag_text = 'Minor tickmarks',
                        tag_font = ('helvetica',10,'bold'),
                        tagindent = 10)
        master = group5.interior()

        #Tkinter.Label(master,
                        #font=('helvetica',10),
                        #text = 'Modify').grid(row=0,column=0)
        Tkinter.Label(master,
                        font=('helvetica',10),
                        text = 'On/Off').grid(row=0,column=1)

        Interval = 1
        self.xmintic1 = create_attribute('xmintic1', self, master, y1 = 'Y1', y2 = 'Y2', row = Interval*1)
        self.xmintic2 = create_attribute('xmintic2', self, master,y1 = 'Y1', y2 = 'Y2', row = Interval*2)
        self.ymintic1 = create_attribute('ymintic1', self, master,x1 = 'X1', x2 = 'X2', row = Interval*3)
        self.ymintic2 = create_attribute('ymintic2', self, master, x1 = 'X1', x2 = 'X2', row = Interval*4)

        group1.grid(row = 3, column = 0,padx = 5,pady=5,sticky = 'nsew')
        group2.grid(row = 3, column = 1,padx = 5,pady=5,sticky = 'nsew')
        group3.grid(row = 1,column = 0,columnspan = 3,ipadx=5,ipady=5,padx = 5,pady=5,sticky = 'nsew')
        group4.grid(row = 2, column = 0,padx = 5,pady=5,sticky = 'nsew')
        group5.grid(row = 2, column = 1,padx = 5,pady=5,sticky = 'nsew')

        #Tkinter.Button(page,
                #text = 'Modify Selected').grid(row = 5,column = 0,columnspan=2)
                
    def create_data_and_legend(self,page):
        #----------------------------------------------
        # If too much is on a page, I may need to 
        # add a scroll window
        #----------------------------------------------
        scl_Frame=Pmw.ScrolledFrame(page,
                                labelpos = 'n',
                                label_text = 'Data/Legend Editor',
                                )
        scl_Frame.pack(fill = 'both', expand = 1,side='top')
        page=scl_Frame.interior()

        group1 = Pmw.Group(page,
                        tag_text = 'Data and Legend',
                        tag_font = ('helvetica',10,'bold'),
                        tagindent = 20)
        master = group1.interior()

        #Tkinter.Label(master,
                        #font=('helvetica',10),
                        #text = 'Modify').grid(row=0, column=0)
        Tkinter.Label(master,
                        font=('helvetica',10),
                        text = 'On/Off').grid(row=0, column=1)

        Interval = 3 
        self.data = create_attribute('data', self, master, x1 = 'X1', x2 = 'X2', y1 = 'Y1', y2 = 'Y2', row = Interval*1)
        self.legend = create_attribute('legend', self, master, x1 = 'X1', x2 = 'X2', y1 = 'Y1', y2 = 'Y2', row = Interval*2)
        group1.grid(row = 0, column = 0, padx = 5, pady = 5, ipadx = 5, ipady = 5,sticky = 'nsew')

        group2 = Pmw.Group(page,
                        tag_text = 'Coordinate values',
                        tag_font = ('helvetica',10,'bold'),
                        tagindent = 20)
        master = group2.interior()

        #Tkinter.Label(master,
                        #font=('helvetica',10),
                        #text = 'Modify').grid(row=0,column=0)
        Tkinter.Label(master,
                        font=('helvetica',10),
                        text = 'On/Off').grid(row=0,column=1)

        Interval = 1
        self.xvalue = create_attribute('xvalue', self, master, x1 = 'X', y1 = 'Y', row = Interval*1)
        self.yvalue = create_attribute('yvalue', self, master, x1 = 'X', y1 = 'Y', row = Interval*2)
        self.zvalue = create_attribute('zvalue', self, master, x1 = 'X', y1 = 'Y', row = Interval*3)
        self.tvalue = create_attribute('tvalue', self, master, x1 = 'X', y1 = 'Y', row = Interval*4)
        group2.grid(row = 0, column = 1, padx = 5, pady = 5, ipadx = 5, ipady = 5,sticky = 'nsew')

        group3 = Pmw.Group(page,
                        tag_text = 'Derived values',
                        tag_font = ('helvetica',10,'bold'),
                        tagindent = 20)
        master = group3.interior()

        #Tkinter.Label(master,
                        #font=('helvetica',10),
                        #text = 'Modify').grid(row=0,column=0)
        Tkinter.Label(master,
                        font=('helvetica',10),
                        text = 'On/Off').grid(row=0,column=1)

        Interval = 1
        self.min = create_attribute('min', self, master, x1 = 'X', y1 = 'Y', row = Interval*1)
        self.max = create_attribute('max', self, master, x1 = 'X', y1 = 'Y', row = Interval*2)
        self.mean = create_attribute('mean', self, master, x1 = 'X', y1 = 'Y', row = Interval*3)
        group3.grid(row = 2, column = 0, padx = 5, pady = 5, ipadx = 5, ipady = 5,sticky = 'nsew')

        master = Tkinter.Frame(group1.interior())
        #Tkinter.Button(page,text = 'Modify Selected').grid(row = 4,column = 0)

    def create_menu_bar(self):
        self.main_menu = Pmw.MenuBar(self.dialog.interior(),
                        hull_relief = 'raised',
                        hull_borderwidth = 2)
        self.main_menu.pack(side='top',fill = 'x')
        self.main_menu.addmenu('Template',None,None, padx = 5, tearoff = 1)
        self.main_menu.addmenuitem('Template','command',
                                label = 'Create New Template...',
                                command = gui_control.Command(self.create_new))
###
# It doesn't make sense to have Open Template. Since we can only have one template open at a 
# time, the sure will have to finish up the current editing of a template, then close it and 
# open up a new template for edit.  This is partically implemented. It will allow the your to
# open select another template for editing, then it will close up the current edited template.
#        self.main_menu.addmenuitem('Template','command',
#                                label = 'Open Template',
#                                # This part needs work.  We want it to do the
#                                #  same thing as it does from the main 
#                                #  VCDAT window.
#                                #command = gui_control.Command(browser.edit_template,self))
#                                command = gui_control.Command(self.open_new))
        self.main_menu.addmenuitem('Template','command',
                                label = 'Save',
                                command = gui_control.Command(self._save, None, 'OK'))
        self.main_menu.addmenuitem('Template','command',
                                label = 'Save As...',
                                command = gui_control.Command(self.saveas))
        self.main_menu.addmenuitem('Template','command',
                                label = 'Save to Script...',
                                command = gui_control.Command(self.savescript))
###
# This is not useful.
#        self.main_menu.addmenuitem('Template','command',
#                                label = 'Save Session',
#                                command = gui_control.Command(self.savesession))
        self.main_menu.addmenuitem('Template','separator')
        self.main_menu.addmenuitem('Template','command',
                                label = 'Exit',
                                command = gui_control.Command(self.execute,None))

        self.main_menu.addmenu('Edit',None,None, padx = 5, tearoff = 1)
        self.main_menu.addmenuitem('Edit','command',
                                label = 'Scale Template',
                                command = gui_control.Command(self.scale))
        self.main_menu.addmenuitem('Edit','command',
                                label = 'Select All',
                                command = gui_control.Command(self.select_all))
        self.main_menu.addmenuitem('Edit','command',
                                label = 'Unselect All',
                                command = gui_control.Command(self.unselect_all))

        self.main_menu.addcascademenu('Edit', 'AutoSave',
                'No Popup Question',
                label = "Switch Templates",
              )
        popupoptions = ['Ask','Auto save','Auto discard changes']
        self.main_menu.popup_toggle=[]
        self.main_menu.popup_toggle_none = Tkinter.IntVar()
        for i in range(len(popupoptions)):
             self.main_menu.popup_toggle.append( Tkinter.IntVar() )
             self.main_menu.popup_toggle[i].set( 0 )
             self.main_menu.addmenuitem('AutoSave', 'checkbutton', popupoptions[i],
                                        label = popupoptions[i],
                                        variable = self.main_menu.popup_toggle[i],
                                        selectcolor=gui_color.one,
                                        command = gui_control.Command(self.evt_set_popup, i)
                                        )
        self.main_menu.popup_toggle[0].set( 1 )

        #self.main_menu.addmenu('Other editors',None,None, padx = 10)
        #self.main_menu.addmenuitem('Other editors','command',
                                #label = 'Font')
        #self.main_menu.addmenuitem('Other editors','command',
                                #label = 'Orientation')
        #self.main_menu.addmenuitem('Other editors','command',
                                #label = 'Format')
        #self.main_menu.addmenuitem('Other editors','command',
                                #label = 'Line')
            #
        #self.main_menu.addmenu('Help',None,None,side = 'right')
         
    def refresh_self_canvas(self):
        Canvas = dir(self.canvas) # This command is need to flush the events for the Tk/Tcl GUI. 
                                  # I don't undstanad why? But with without this command, the 
                                  # GUI is not updated.
         
    def refresh_toggle(self, attr_name, toggle_val):
        Canvas = dir(self.canvas) # This command is need to flush the events for the Tk/Tcl GUI. 
                                  # I don't undstanad why? But with without this command, the 
                                  # GUI is not updated.
        if (toggle_val in [1,2]):
           eval("self."+attr_name+".display.configure( selectcolor = hot_color )")
           if (attr_name in ["data", "legend", "min", "max", "mean", "xvalue", "zvalue","tvalue"]):
              self.notebook.selectpage('Data/Legend')
           elif (attr_name in ["box1", "box2", "box3", "box4", "line1", "line2", "line3","line4"]):
              self.notebook.selectpage('Borders/Lines')
           elif (attr_name in ["dataname", "title", "source", "file", "units", "crdate", "crtime","comment1", "comment2", "comment3", "comment4"]):
              try:
                 self.notebook.selectpage("Labels")
                 attr_name, attr_str = get_slab_info(self.parent, self.canvas, attr_name, "Labels")
           #      print "1 - DNW gui_template_editor attr_name = ", attr_name , "attr_str = ", attr_str
                 self.canvas.canvas_gui.txt.configure( label_text = string.capitalize( attr_name ) )
                 if toggle_val == 1: self.canvas.canvas_gui.txt.setentry( attr_str )
                 self.canvas.canvas_gui.txt.configure( entry_state = 'normal' )
           #      eval("self."+attr_name+".display.configure( selectcolor = gui_color.one )")
              except:
                 pass
           else:
              try:
                 self.notebook.selectpage("Axis")
                 attr_name, attr_str = get_slab_info(self.parent, self.canvas, attr_name, "Axis")
           #      print "2 - DNW gui_template_editor attr_name = ", attr_name , "attr_str = ", attr_str
                 self.canvas.canvas_gui.txt.configure( label_text = string.capitalize( attr_name ) )
                 if toggle_val == 1: self.canvas.canvas_gui.txt.setentry( attr_str )
                 self.canvas.canvas_gui.txt.configure( entry_state = 'normal' )
           #      eval("self."+attr_name+".display.configure( selectcolor = gui_color.one )")
              except:
                 pass
           if (attr_name not in ["dataname", "title", "source", "file", "units","comment1", "comment2", "comment3", "comment4", "xname", "yname"]):
              self.canvas.canvas_gui.txt.configure( label_text = "" )
              self.canvas.canvas_gui.txt.setentry( "Cannot change this text." )
              self.canvas.canvas_gui.txt.configure( entry_state = 'disabled' )
        else:
           try: eval("self."+attr_name+".display.configure( selectcolor = gui_color.one )")
           except: pass

    def refresh_data(self):
        Canvas = dir(self.canvas) # This command is need to flush the events for the Tk/Tcl GUI. 
                                  # I don't undstanad why? But with without this command, the 
                                  # GUI is not updated.

        self.changed = 1 
        template = self.canvas.gettemplate(self.new_template.name)
        keys = self.info.checkbutton.keys()
        # Reset On/Off checkbutton for each plot attribute
        for key in keys:
            template_priority = "template."+key+".priority"
            gui_priority = "self.info.checkbutton['"+key+"']"
            if (eval(template_priority) != eval(gui_priority)):
                exec(gui_priority+"="+template_priority)
                eval("change_field_mode(self."+key+","+template_priority+")")
                eval("self."+key+".display.toggle()")

        self.dataname.x1.setentry(format_number(template.dataname.x))
        self.dataname.y1.setentry(format_number(template.dataname.y))
        self.title.x1.setentry(format_number(template.title.x))
        self.title.y1.setentry(format_number(template.title.y))
        self.source.x1.setentry(format_number(template.source.x))
        self.source.y1.setentry(format_number(template.source.y))
        self.file.x1.setentry(format_number(template.file.x))
        self.file.y1.setentry(format_number(template.file.y))
        self.units.x1.setentry(format_number(template.units.x))
        self.units.y1.setentry(format_number(template.units.y))
        self.crdate.x1.setentry(format_number(template.crdate.x))
        self.crdate.y1.setentry(format_number(template.crdate.y))
        self.crtime.x1.setentry(format_number(template.crtime.x))
        self.crtime.y1.setentry(format_number(template.crtime.y))
        self.comment1.x1.setentry(format_number(template.comment1.x))
        self.comment1.y1.setentry(format_number(template.comment1.y))
        self.comment2.x1.setentry(format_number(template.comment2.x))
        self.comment2.y1.setentry(format_number(template.comment2.y))
        self.comment3.x1.setentry(format_number(template.comment3.x))
        self.comment3.y1.setentry(format_number(template.comment3.y))
        self.comment4.x1.setentry(format_number(template.comment4.x))
        self.comment4.y1.setentry(format_number(template.comment4.y))
        self.xlabel1.y1.setentry(format_number(template.xlabel1.y))
        self.xlabel2.y1.setentry(format_number(template.xlabel2.y))
        self.ylabel1.x1.setentry(format_number(template.ylabel1.x))
        self.ylabel2.x1.setentry(format_number(template.ylabel2.x))
        self.xtic1.y1.setentry(format_number(template.xtic1.y1))
        self.xtic1.y2.setentry(format_number(template.xtic1.y2))
        self.xtic2.y1.setentry(format_number(template.xtic2.y1))
        self.xtic2.y2.setentry(format_number(template.xtic2.y2))
        self.xmintic1.y1.setentry(format_number(template.xmintic1.y1))
        self.xmintic1.y2.setentry(format_number(template.xmintic1.y2))
        self.xmintic2.y1.setentry(format_number(template.xmintic2.y1))
        self.xmintic2.y2.setentry(format_number(template.xmintic2.y2))
        self.ytic1.x1.setentry(format_number(template.ytic1.x1))
        self.ytic1.x2.setentry(format_number(template.ytic1.x2))
        self.ytic2.x1.setentry(format_number(template.ytic2.x1))
        self.ytic2.x2.setentry(format_number(template.ytic2.x2))
        self.ymintic1.x1.setentry(format_number(template.ymintic1.x1))
        self.ymintic1.x2.setentry(format_number(template.ymintic1.x2))
        self.xname.x1.setentry(format_number(template.xname.x))
        self.xname.y1.setentry(format_number(template.xname.y))
        self.yname.x1.setentry(format_number(template.yname.x))
        self.yname.y1.setentry(format_number(template.yname.y))
        self.zname.x1.setentry(format_number(template.zname.x))
        self.zname.y1.setentry(format_number(template.zname.y))
        self.tname.x1.setentry(format_number(template.tname.x))
        self.tname.y1.setentry(format_number(template.tname.y))
        self.xunits.x1.setentry(format_number(template.xunits.x))
        self.xunits.y1.setentry(format_number(template.xunits.y))
        self.yunits.x1.setentry(format_number(template.yunits.x))
        self.yunits.y1.setentry(format_number(template.yunits.y))
        self.zunits.x1.setentry(format_number(template.zunits.x))
        self.zunits.y1.setentry(format_number(template.zunits.y))
        self.tunits.x1.setentry(format_number(template.tunits.x))
        self.tunits.y1.setentry(format_number(template.tunits.y))
        self.box1.x1.setentry(format_number(template.box1.x1))
        self.box1.y1.setentry(format_number(template.box1.y1))
        self.box1.x2.setentry(format_number(template.box1.x2))
        self.box1.y2.setentry(format_number(template.box1.y2))
        self.box2.x1.setentry(format_number(template.box2.x1))
        self.box2.y1.setentry(format_number(template.box2.y1))
        self.box2.x2.setentry(format_number(template.box2.x2))
        self.box2.y2.setentry(format_number(template.box2.y2))
        self.box3.x1.setentry(format_number(template.box3.x1))
        self.box3.y1.setentry(format_number(template.box3.y1))
        self.box3.x2.setentry(format_number(template.box3.x2))
        self.box3.y2.setentry(format_number(template.box3.y2))
        self.box4.x1.setentry(format_number(template.box4.x1))
        self.box4.y1.setentry(format_number(template.box4.y1))
        self.box4.x2.setentry(format_number(template.box4.x2))
        self.box4.y2.setentry(format_number(template.box4.y2))
        self.line1.x1.setentry(format_number(template.line1.x1))
        self.line1.y1.setentry(format_number(template.line1.y1))
        self.line1.x2.setentry(format_number(template.line1.x2))
        self.line1.y2.setentry(format_number(template.line1.y2))
        self.line2.x1.setentry(format_number(template.line2.x1))
        self.line2.y1.setentry(format_number(template.line2.y1))
        self.line2.x2.setentry(format_number(template.line2.x2))
        self.line2.y2.setentry(format_number(template.line2.y2))
        self.line3.x1.setentry(format_number(template.line3.x1))
        self.line3.y1.setentry(format_number(template.line3.y1))
        self.line3.x2.setentry(format_number(template.line3.x2))
        self.line3.y2.setentry(format_number(template.line3.y2))
        self.line4.x1.setentry(format_number(template.line4.x1))
        self.line4.y1.setentry(format_number(template.line4.y1))
        self.line4.x2.setentry(format_number(template.line4.x2))
        self.line4.y2.setentry(format_number(template.line4.y2))
        self.data.x1.setentry(format_number(template.data.x1))
        self.data.y1.setentry(format_number(template.data.y1))
        self.data.x2.setentry(format_number(template.data.x2))
        self.data.y2.setentry(format_number(template.data.y2))
        self.legend.x1.setentry(format_number(template.legend.x1))
        self.legend.y1.setentry(format_number(template.legend.y1))
        self.legend.x2.setentry(format_number(template.legend.x2))
        self.legend.y2.setentry(format_number(template.legend.y2))
        self.xvalue.x1.setentry(format_number(template.xvalue.x))
        self.xvalue.y1.setentry(format_number(template.xvalue.y))
        self.yvalue.x1.setentry(format_number(template.yvalue.x))
        self.yvalue.y1.setentry(format_number(template.yvalue.y))
        self.zvalue.x1.setentry(format_number(template.zvalue.x))
        self.zvalue.y1.setentry(format_number(template.zvalue.y))
        self.tvalue.x1.setentry(format_number(template.tvalue.x))
        self.tvalue.y1.setentry(format_number(template.tvalue.y))
        self.min.x1.setentry(format_number(template.min.x))
        self.min.y1.setentry(format_number(template.min.y))
        self.max.x1.setentry(format_number(template.max.x))
        self.max.y1.setentry(format_number(template.max.y))
        self.mean.x1.setentry(format_number(template.mean.x))
        self.mean.y1.setentry(format_number(template.mean.y))

    def sample_data(self):
        a1 = numpy.arange(10) * 18.0 - 90.
        a2 = numpy.arange(10) * 36.0 - 180.
        d = numpy.outer(numpy.sin(a1*numpy.pi/360.),
                            numpy.cos(a2*numpy.pi/360.))
        d.shape=(1,1,10,10)
        a1a = cdms.createAxis(a1)
        a1a.designateLatitude()
        a2a = cdms.createAxis(a2)
        a2a.designateLongitude()
        a3a = cdms.createAxis([1979.])
        a3a.designateTime()
        a4a = cdms.createAxis([0.5])
        a4a.designateLevel()
        data = cdms.MV2.array(d, axes=[a3a, a4a, a1a, a2a])
        return data

    def show_canvas(self):
        if (self.plot is not None):
            return

        # Turn off all other plots
        if (self.parent is not None):
           try: self.parent.panelGC.turn_off_all_plots( self.parent )
           except: pass

        self.plot = self.canvas.plot(self.sample_data(),self.template_name, self.gm, self.gm_name,
            comment1 = 'comment1',
            comment2 = 'comment2',
            comment3 = 'comment3',
            comment4 = 'comment4',
            file_comment = 'file_comment',
            long_name = 'long_name',
            units = 'units',
            xunits = 'xunits',
            yunits = 'yunits',
            tunits = 'tunits',
            zunits = 'zunits',
            xname = 'xname',
            yname = 'yname',
            tname = 'tname',
            zname = 'zname' 
        )

        self.generated_plot = 1

        return 

    def replot(self):
        from Canvas import finish_queued_X_server_requests

        finish_queued_X_server_requests( self.canvas )
        self.canvas.BLOCK_X_SERVER()

#        if self.plot.parent.mode == 0:
#            self.plot.parent.mode = 1
        # This section needs more work to get the plot to do the 
        #   backing_store part correctly
        self.plot.template = self.plot.template
        self.canvas.canvasraised()
        self.canvas._update_continents_check()
        self.canvas.flush()
        self.plot.parent.mode = 0

        self.canvas.UNBLOCK_X_SERVER()

    # Main command control function.  Figure out what to do with the 
    #  request (ie. Save, Cancel, etc.)
    def execute(self,result, template_name=None, restore =0):
        if self.canvas.canvas_gui is not None:
           self.dialog.dialog.withdraw()
           return
        if result == 'Apply' or result == 'Save':
            self._save(None, 'OK')
            # Change the color of the buttons back to gui_color one (i.e., purple)
            keys = self.info.checkbutton.keys()
            for key in keys:
               eval("self."+key+".display.configure( selectcolor = gui_color.one )")
        elif result == 'Save As':
            self.changed = 0
            self.saveas()
        elif result == 'Revert':
            self.changed = 0
            self.get_template()
            self.refresh_data()
            self.replot()
            keys = self.info.checkbutton.keys()
            for key in keys:
               eval("self."+key+".display.configure( selectcolor = gui_color.one )")
        elif result == 'Cancel':
            self.plot.template = self.template_name
            self.exit()
            self.restore_plots_on_canvas()
        elif ((result == 'Dismiss') or (result is None)):
            # Check and see if the template has been changed, if so, prompt
            #  user to save changes for current session
            if self.changed:
                self.save(template_name)
                # Reset canvas mode to DATA
                self.canvas._SCREEN_DATA_FLAG()
            else:
                self.exit()
                self.restore_plots_on_canvas()

    def execute_from_canvas(self):
         self.plot.template = self.template_name
         canvas = self.canvas
         plot = self.plot
         self.changed = 0
         old_template = self.new_template.name
         Pic_name_src = self.new_template.name
         Pic_name = self.template_name
         self.canvas.syncP(Pic_name, Pic_name_src)
         if (Pic_name != self.template_orig_name):
            self.canvas.syncP(self.template_orig_name, Pic_name)
         self.get_template()
         self.canvas._unselect_all()
         self.exit()
         return Pic_name

    def restore_plots_on_canvas( self ):
        self.canvas._SCREEN_TEMPLATE_FLAG()
        self.canvas.clear()
        if ((self.parent is not None) and (self.plot_that_are_on != [])):
           self.parent.panelGC.turn_on_listed_plots( self.parent, self.plot_that_are_on )
        self.canvas._SCREEN_CHECKMODE_DATA_FLAG()
        self.canvas._SCREEN_DATA_FLAG()

    def evt_set_popup(self,i):
         for j in range(len(self.main_menu.popup_toggle)):
              self.main_menu.popup_toggle[j].set(0)
         self.main_menu.popup_toggle[i].set(1)
         if i==0:
              self.answered_question = None
              self.nopopupswitch = False
         elif i==1:
              self.answered_question='Save'
              self.nopopupswitch = True
         elif i==2:
              self.answered_question='Discard'
              self.nopopupswitch = True

    # Prompt user to save changes if they are trying to close the template
    #  editor and changes have been made.
    def ask_save_from_vcs(self):
         ## window to save template or not while editing and changing template
         ## need to set a vcdat flag to not popup if user wants automatic mode
         if self.nopopupswitch:
              return self.answered_question
         self.answered_question = None
         # Position dialog popup
         canvasinfo = self.canvas.canvasinfo()
         w = canvasinfo['width']
         h = canvasinfo['height']
         d1 = canvasinfo['x']+w/2
         d2 = canvasinfo['y']+h/2
         self.vcssavedialog.geometry( "+%d+%d" % (d1, d2) )
         result = self.vcssavedialog.show()
         while self.answered_question is None:
              pass
         return self.answered_question
         
    def _ask_save(self,*args):
         askagain = self.askagain.getvalue()
         if askagain!=():
              self.nopopupswitch = True
              for i in range(len(self.main_menu.popup_toggle)):
                   self.main_menu.popup_toggle[i].set(0)
              if args[0]=='Save':
                   self.main_menu.popup_toggle[1].set(1)
              else:
                   self.main_menu.popup_toggle[2].set(1)

         self.vcssavedialog.withdraw()
         self.answered_question = args[0]

    def save(self,template_name=None):
        self.savedialog = Pmw.Dialog(self.dialog.interior(),
                title = 'Save changes?',
                buttons = ('OK', 'Discard \nchanges', 'Cancel'),
                command = gui_control.Command(self._save,template_name))
        Tkinter.Label(self.savedialog.interior(),
                        text = 'Changes have been made to the template.\n Do you want to save your changes?').pack(expand = 1, fill = 'x', padx = 10, pady = 10)

        # Position dialog popup
        parent_geom = self.dialog.geometry()
        geom = string.split(parent_geom, '+')
        d1 = string.atoi( geom[1] )
        d2 = string.atoi( geom[2] )
        self.savedialog.geometry( "+%d+%d" % (d1, d2) )

    def remove_temp_templates( self ):
        template_list = vcs.listelements('template')
        for x in template_list:
            if (x[:5] == '_temp'):
               self.canvas.removeP( x )

    # Process the save request.  This can be called directly or through the
    #  save function.  If it was called directly, leave the template editor
    #  open, otherwise save and close the template editor.
    def _save(self, template_name, result):
        gui_parent = self.parent
        canvas = self.canvas
        plot = self.plot
        if result == 'OK':
            self.changed = 0
            old_template = self.new_template.name
            Pic_name_src = self.new_template.name
            Pic_name = self.template_name
            self.canvas.syncP(Pic_name, Pic_name_src)
            if (Pic_name != self.template_orig_name):
               self.canvas.syncP(self.template_orig_name, Pic_name)
            self.get_template()
            self.canvas._unselect_all()
            if 'savedialog' in dir(self):
                self.savedialog.destroy()
                self.exit()
                self.restore_plots_on_canvas()
        elif result == 'Discard \nchanges':
            self.savedialog.destroy()
            try: self.plot.template = self.template_name
            except: pass
            self.exit()
            self.restore_plots_on_canvas()
        elif result == 'Cancel':
            self.savedialog.destroy()
        if template_name is not None:
            # Redisplay the template list
            if (self.no_gui_parent == 0):
               t_list = vcs.listelements( 'template' )

               self.parent.panelDV.template_listbox.setlist( t_list )
               name_index = t_list.index( template_name )
               self.parent.panelDV.template_listbox.select_set( name_index )
               self.parent.panelDV.template_listbox.see( name_index )

            canvas.templateeditor(template_name=template_name, gui_parent=gui_parent)

    # Verify template being saved and give users one more chance to 
    #  cancel their save request.
    def savesession(self):
        if self.template_name == 'default' or \
            self.template_name == 'default_dud':
            showerror('Save Session Error', 'You cannot modify default or default_dud.')
            return
        result = askyesno('Save Session?', 'Save these values permanently?')
        if result:
            self._savesession()

    # Save session
    def _savesession(self):
        self.canvas.saveinitialfile()
        s = Pmw.TextDialog(self.dialog.interior(), title='Session saved.')
        s.transient(self.dialog.interior())
        self.dialog.position_popup(s)
        s.insert('end',
        """Session saved.
        The old script file
        %s/%s/initial.attributes
        has been saved as
        %s/%s/initial.attributes%%.
        """ % (os.environ['HOME'], self.canvas._dotdir,
                os.environ['HOME'],self.canvas._dotdir)
                )

    def savescript(self):
      # Get the Template name 
      try:
         t_selected = self.template_name
      except:
         gui_message.error( 'Must select a template from the list below.' )
         return

      # Show the popup directory dialog
      filetypes = [ ("Python", ".py"), ("VCS", ".scr") ]
      savescript_dialog = tkFileDialog.asksaveasfilename(master=self.dialog.interior(),
                       filetypes=filetypes,
                       title = 'Save template %s to file' % (t_selected))

      if savescript_dialog == (): return
      if (savescript_dialog[-3:] != '.py') and (savescript_dialog[-4:] != '.scr'): savescript_dialog += '.py'

      # Script the template selected in the list
      r = self.canvas.gettemplate(self.new_template_name)
      self.canvas.scriptobject( r, savescript_dialog, mode = 'w' )



    def scale(self):
        self.scale = Pmw.Dialog(self.dialog.interior(),
                    title = 'Scale Template',
                    buttons = ('OK', 'Cancel'),
                    defaultbutton='OK',
                    command = self._scale)
        self.percentage = Pmw.EntryField(self.scale.interior(),
                    labelpos = 'w',
                    label_text = 'Scale by (%): ',
                    label_font = ('helvetica',12),
                    entry_background = 'white')
        self.percentage.pack(expand = 1, fill = 'x', padx = 10, pady = 10)

        # Decide where to put it on the screen
        parent_geom = self.dialog.geometry()
        geom = string.split(parent_geom, '+')
        d1 = string.atoi( geom[1] )
        d2 = string.atoi( geom[2] )
        self.scale.geometry( "+%d+%d" % (d1, d2) )


    def _scale(self,result):
        if result == 'OK':
            percentage = self.percentage.get()
            if percentage == '':
                showerror("Error","Please enter a percentage to scale\n the template by.")
            else:
                try:
                    value = float(percentage)
                    if (float(percentage) <= 0):
                        showerror("Error","You must select a scale factor \ngreater than 0.")
                    else:
                        self.new_template.scale(float(percentage)/100.)
                        self.replot()
                        self.scale.destroy()
                except:
                    showerror("Error","Please enter a percentage to scale\n the template by.")
        else:
            self.scale.destroy()

    def select_all(self):
        keys = self.info.checkbutton.keys()
        for key in keys:
           eval("self."+key+".display.configure( selectcolor = hot_color )")

        if (self.canvas.return_display_ON_num() == 1):
            self.canvas._select_all()
        else:
            showerror('Error','"Select All" currently only works for a single plot displayed on a VCS Canvas. Please select the plot objects manually. That is, depress the "Shift" key and select multiple plot objects.')

    def unselect_all(self):
        keys = self.info.checkbutton.keys()
        for key in keys:
           eval("self."+key+".display.configure( selectcolor = gui_color.one )")

        self.canvas._unselect_all()

    # Exit template editor mode.  Set flag for data mode upon exit of 
    #  template editor.
    def exit(self):
        self.remove_temp_templates()
        self.canvas._SCREEN_DATA_FLAG()
        if self.no_gui_parent == 1:
            self.dialog.dialog.destroy()
        else:
            self.dialog.destroy()

    def toggle_on_off_for_canvas(self,parent,name):
        parent.changed = 1
        item = eval('parent.new_template.%s'%name)
        obj  = eval("self."+name)
        store_checkbutton = parent.info.checkbutton[name]
        if (parent.finished_gui):
            try: x1=string.atof( obj.x1.get() )
            except: x1 = -999.0
            try: x2=string.atof( obj.x2.get() )
            except: x2 = -999.0
            try: y1=string.atof( obj.y1.get() )
            except: y1 = -999.0
            try: y2=string.atof( obj.y2.get() )
            except: y2 = -999.0
         
        item.priority = max(item.priority,1)
        self.change_field_mode_for_canvas(obj, 1)
        if (parent.mode):
            parent.canvas.flush()
            try: parent.replot()
            except: pass
            parent.canvas.flush()
    #    print "parent.finished_gui = ", parent.new_template.name, name
        if (parent.finished_gui):
            parent.info.checkbutton[name] = 1 
     #       parent.canvas._select_one(parent.new_template.name,name,x1, x2, y1, y2) # show red boxes on text
            obj.display.configure( selectcolor = hot_color )
            obj.display.select( )
            self.refresh_toggle(name, 2)
                
    def change_field_mode_for_canvas(self,obj, mode):
        if mode == 0:
            obj.label.configure(font = ('helvetica', 10))
            for i in (obj.x1, obj.x2, obj.y1, obj.y2):
                if i:
                    i.configure(entry_background = gui_color.off_color_bg)
                    i.configure(entry_foreground = gui_color.off_color_fg)
                    i.configure(entry_state = 'disabled')
            obj.properties.configure(state = 'disabled')
        else:
            obj.label.configure(font = ('helvetica', 10,'bold'))
            for i in (obj.x1, obj.x2, obj.y1, obj.y2):
                if i:
                    i.configure(entry_background = gui_color.on_color_bg)
                    i.configure(entry_foreground = gui_color.on_color_fg)
                    i.configure(entry_state = 'normal')
            obj.properties.configure(state = 'normal')

    def reinitialize_editor(self):
        self.plot.parent.mode = 0
        temp = self.mode
        self.mode = 0
        items = self.info.checkbutton.keys()
        coordinates = ['x1', 'x2', 'y1', 'y2']
        for i in range(len(items)):
            for j in range(len(coordinates)):
                field = 'self.%s.%s'%(items[i],coordinates[j])
                try:
                    eval('%s.setentry(format_number(%s))'%(field,value))
                except:
                    pass
            field = 'self.%s.display'%(items[i])
            if priority != self.info.checkbutton[items[i]]:
                exec('%s.invoke()'%field)
                self.info.checkbutton[items[i]] = priority
        self.mode = temp

    #----------------------------------------------------------
    # Create a copy of the template and then tell the canvas to use
    #  the template copy to display the plot. (This way we're not
    #  actually modifying the real template in case we want to 
    #  revert or do a "Save As".)
    #----------------------------------------------------------
    def get_template(self):
        template_name = self.template_name
        try_again = 1
        while try_again:
            try:
                new_template_name = '__mytemp%i'%randint(0,10000)
                self.canvas.createtemplate(new_template_name,template_name)
                self.new_template = self.canvas.gettemplate(new_template_name)
                self.plot.parent.mode = 0
                self.plot.template = new_template_name
                self.new_template_name = new_template_name
                try_again = 0
            except Exception,err:
##                  print 'Err',err
                 pass

    #----------------------------------------------------------
    # Change the value of a particular attribute in the 
    #  template so that it reflects the state of the GUI.
    #  This is more for the pull down menu options such as
    #  text orientation, text table, etc.
    #----------------------------------------------------------
    def set_value(self, attribute, result):
        exec('self.new_template.%s = \'%s\''%(attribute,result))
        if self.mode:
            self.canvas.flush()
            self.replot()

    #----------------------------------------------------------
    # Don't do anything.
    #----------------------------------------------------------
    def do_nothing(self, event):
        return "break"

    #----------------------------------------------------------
    # Change the color of the text window when inputing text.
    #----------------------------------------------------------
    def evt_change_color(self, master, obj, event):
        keycolor = Pmw.Color.changebrightness(master, 'red', 0.85)
        obj.configure( entry_background = keycolor )

    #----------------------------------------------------------
    # Update the new value for the template in use.  This is normally called
    #  from a bound object when someone changes focus to another window or 
    #  hits 'Enter'.
    #----------------------------------------------------------
    def update_value(self,item,name,evt=None):
        item.configure( entry_background = 'white' ) # change backgound color to white
        self.changed = 1
        value = item.get()
        object = name.split('.')[0]
        attribute = name.split('.')[1]
        if (attribute == 'x') or (attribute == 'y'):
            attribute += '1'
        name1 = 'self.new_template.%s'%name
        name2 = 'self.%s.%s'%(object,attribute)
        if (type(eval(name1)) == type(4.3) and format_number(eval(name1)) != value):
            assignment = '%s = %s'%(name1,value)
            try:
                exec('%s.setentry(format_number(%s))'%(name2,value))
            except:
                pass
            exec(assignment)
            if self.mode:
                self.replot()
        elif (type(eval(name1)) != type(4.3) and eval(name1) != value):
            assignment = '%s = %s'%(name1,value)
            try:
                exec('%s.setentry(format_number(%s))'%(name2,value))
            except:
                pass
            exec(assignment)
            if self.mode:
                self.replot()
        return "break"

    def priority_change(self, name, evt=None):
        exec('self.new_template.%s.priority = int(self.properties_priority.get())'%name)
        if self.mode:
            self.canvas.flush()
            self.replot()

    #--------------------------------------------------------------
    # Open properties button for selected attribute
    #--------------------------------------------------------------
    def show_properties(self,parent,master):
        a = eval('self.new_template.'+parent.Name)
        self.info.revert_script = ''
        attributes = dir(a)
        
        self.dialog2 = Pmw.Dialog(master,
                    title = 'Change attribute properties',
                    buttons = ('OK', 'Cancel'),
                    command = gui_control.Command(self.execute2,parent.Name))
        self.dialog2.withdraw()

        group1 = Pmw.Group(self.dialog2.interior(),
                        tag_font = ('helvetica',10,'bold'),
                        tag_text = parent.Name.capitalize()+' Properties',
                        tagindent = 10)

        self.properties_priority = Pmw.Counter(group1.interior(),
                        labelpos = 'w',
                        label_text = 'Priority',
                        entryfield_validate = { 'validator' : 'integer', 'min' : 1},
                        entryfield_value = a.priority,
                        entryfield_modifiedcommand = gui_control.Command(self.priority_change, parent.Name),
                        entry_background = 'white')
        self.properties_priority.pack(side = 'top', expand = 1, fill = 'x', pady = 2, padx = 5)
        self.info.revert_script = 'self.new_template.%s.priority = %s\n'%(parent.Name,a.priority)
        self.properties_priority.component('entry').bind('<Return>',gui_control.Command(self.do_nothing))
        self.properties_priority.component('entry').bind('<Key>',gui_control.Command(self.do_nothing))

        labels = []
        labels.append(self.properties_priority)
        
        if 'texttable' in attributes:
            attribute = '%s.texttable'%parent.Name
            self.parent_Name=parent.Name
            f=Tkinter.Frame(group1.interior())
            self.properties_font = Pmw.ComboBox(f,
##                         labelpos = 'w',
##                         label_text = 'Font',
                        entry_background = 'white',
                        selectioncommand = gui_control.Command(self.set_value,attribute),
                        scrolledlist_items = vcs.listelements('texttable'),
                        dropdown = 1)
            b=Tkinter.Button(f,
                             font = ('helvetica',10),
                             bg=gui_support.gui_color.Tt_color,
                             text='Table',
                             command=gui_control.Command(fonteditorgui.FontGUI,self.canvas,self.parent,self.dialog2,f,a.texttable,a.textorientation,self),
                             )
            b.pack(side='left')
            self.properties_font.selectitem(a.texttable)
            self.properties_font.pack(expand = 1, fill = 'x', pady = 2, padx = 5) 
            self.info.revert_script += 'self.new_template.%s.texttable = \'%s\'\n'%(parent.Name,a.texttable)
            self.properties_font.component('entry').bind('<Return>',gui_control.Command(self.do_nothing))
            self.properties_font.component('entry').bind('<Key>',gui_control.Command(self.do_nothing))
            #self.properties_font.bind('<FocusOut>',gui_control.Command(self.update_value,self.properties_font,attribute))
            #self.properties_font.component('entry').bind('<Return>',gui_control.Command(self.update_value,self.properties_font,attribute))
            labels.append(self.properties_font)
            f.pack()
        if 'textorientation' in attributes:
            self.parent_Name=parent.Name
            attribute = '%s.textorientation'%parent.Name
            f=Tkinter.Frame(group1.interior())
            b=Tkinter.Button(f,
                             font = ('helvetica',10),
                             bg=gui_support.gui_color.To_color,
                             text='Orientation',
                             command=gui_control.Command(fonteditorgui.FontGUI,self.canvas,self.parent,self.dialog2,f,a.texttable,a.textorientation,self),
                             )
            b.pack(side='left')
            self.properties_orientation= Pmw.ComboBox(f,
##                         labelpos = 'w',
##                         label_text = 'Orientation',
                        entry_background = 'white',
                        selectioncommand = gui_control.Command(self.set_value,attribute),
                        scrolledlist_items = vcs.listelements('textorientation'),
                        dropdown = 1)
            self.properties_orientation.selectitem(a.textorientation)
            self.properties_orientation.pack(expand = 1, fill = 'x', pady = 2, padx = 5) 
            self.info.revert_script += 'self.new_template.%s.textorientation = \'%s\'\n'%(parent.Name,a.textorientation)
            self.properties_orientation.component('entry').bind('<Return>',gui_control.Command(self.do_nothing))
            self.properties_orientation.component('entry').bind('<Key>',gui_control.Command(self.do_nothing))
            #self.properties_orientation.bind('<FocusOut>',gui_control.Command(self.update_value,self.properties_orientation,attribute))
            #self.properties_orientation.component('entry').bind('<Return>',gui_control.Command(self.update_value,self.properties_orientation,attribute))
            labels.append(self.properties_orientation)
            f.pack()
        if 'format' in attributes:
            attribute = '%s.format'%parent.Name
            self.properties_format = Pmw.ComboBox(group1.interior(),
                        labelpos = 'w',
                        label_text = 'Format',
                        entry_background = 'white',
                        scrolledlist_items = vcs.listelements('format'),
                        dropdown = 1)
            self.properties_format.selectitem(a.format)
            self.properties_format.pack(expand = 1, fill = 'x', pady = 2, padx = 5) 
            self.info.revert_script += 'self.new_template.%s.format = \'%s\'\n'%(parent.Name,a.format)
            self.properties_format.bind('<FocusOut>',gui_control.Command(self.update_value,self.properties_format,attribute))
            self.properties_format.component('entry').bind('<Return>',gui_control.Command(self.update_value,self.properties_format,attribute))
            labels.append(self.properties_format)
        if 'line' in attributes:
            attribute = '%s.line'%parent.Name
            self.parent_Name=parent.Name
            f=Tkinter.Frame(group1.interior())
            b=Tkinter.Button(f,
                             font = ('helvetica',10),
                             bg=gui_support.gui_color.L_color,
                             text='Line',
                             command=gui_control.Command(lineeditorgui.LineGUI,self.canvas,self.parent,self.dialog2,f,a.line,self),
                             )
            b.pack(side='left')
            self.properties_line = Pmw.ComboBox(f,
##                         labelpos = 'w',
##                         label_text = 'Line',
                        entry_background = 'white',
                        selectioncommand = gui_control.Command(self.set_value,attribute),
                        scrolledlist_items = vcs.listelements('line'),
                        dropdown = 1)
            self.properties_line.selectitem(a.line)
            self.properties_line.pack(expand = 1, fill = 'x', pady = 2, padx = 5) 
            self.info.revert_script += 'self.new_template.%s.line = \'%s\'\n'%(parent.Name,a.line)
            self.properties_line.bind('<FocusOut>',gui_control.Command(self.update_value,self.properties_line,attribute))
            self.properties_line.component('entry').bind('<Return>',gui_control.Command(self.update_value,self.properties_line,attribute))
            labels.append(self.properties_line)
            f.pack()

        Tkinter.Label(group1.interior(),
                        text = '-----------Coordinates-----------',
                        font = ('helvetica',10,'bold')).pack(expand = 1, fill = 'x',pady = 5)
        x1 = x2 = y1 = y2 = None
        coordinate_mode = 0
        if ('x' in attributes) and ('y' in attributes):
            attribute = '%s.x'%parent.Name
            tempframe = Tkinter.Frame(group1.interior())
            self.properties_x1 = Pmw.EntryField(tempframe,
                        labelpos = 'w',
                        label_text = 'X',
                        label_font = ('helvetica',10),
                        value = format_number(a.x),
                        entry_width = 8,
                        entry_background = 'white')
            self.properties_x1.pack(side = 'left',expand = 1, fill = 'x')
            self.properties_x1.bind('<FocusOut>',gui_control.Command(self.update_value,self.properties_x1,attribute))
            self.properties_x1.component('entry').bind('<Return>',gui_control.Command(self.update_value,self.properties_x1,attribute))
            self.properties_x1.component('entry').bind('<Key>', gui_control.Command(self.evt_change_color, master, self.properties_x1))

            attribute = '%s.y'%parent.Name
            self.properties_y1 = Pmw.EntryField(tempframe,
                        labelpos = 'w',
                        label_text = 'Y',
                        label_font = ('helvetica',10),
                        value = format_number(a.y),
                        entry_width = 8,
                        entry_background = 'white')
            self.properties_y1.pack(side = 'left',expand = 1, fill = 'x')
            self.properties_y1.bind('<FocusOut>',gui_control.Command(self.update_value,self.properties_y1,attribute))
            self.properties_y1.component('entry').bind('<Return>',gui_control.Command(self.update_value,self.properties_y1,attribute))
            self.properties_y1.component('entry').bind('<Key>', gui_control.Command(self.evt_change_color, master, self.properties_y1))

            self.info.revert_script += 'self.new_template.%s.x = %s\n'%(parent.Name,a.x)
            self.info.revert_script += 'self.new_template.%s.y = %s\n'%(parent.Name,a.y)
            self.info.revert_script += 'self.%s.x1.setentry(format_number(%s))\n'%(parent.Name,a.x)
            self.info.revert_script += 'self.%s.y1.setentry(format_number(%s))\n'%(parent.Name,a.y)
            self.info.revert_script += 'self.new_template.%s.y = %s\n'%(parent.Name,a.y)
            tempframe.pack(expand = 1, fill = 'x',padx = 15, pady = 5)
        elif 'x' in attributes:
            attribute = '%s.x'%parent.Name
            self.properties_x1 = Pmw.EntryField(group1.interior(),
                        labelpos = 'w',
                        label_text = 'X',
                        label_font = ('helvetica',10),
                        value = format_number(a.x),
                        entry_width = 8,
                        entry_background = 'white')
            self.info.revert_script += 'self.new_template.%s.x = %s\n'%(parent.Name,a.x)
            self.info.revert_script += 'self.%s.x1.setentry(format_number(%s))\n'%(parent.Name,a.x)
            self.properties_x1.pack(expand = 1, fill = 'x', padx = 15, pady = 5)
            self.properties_x1.bind('<FocusOut>',gui_control.Command(self.update_value,self.properties_x1,attribute))
            self.properties_x1.component('entry').bind('<Return>',gui_control.Command(self.update_value,self.properties_x1,attribute))
        elif 'y' in attributes:
            attribute = '%s.y'%parent.Name
            self.properties_y1 = Pmw.EntryField(group1.interior(),
                        labelpos = 'w',
                        label_text = 'Y',
                        label_font = ('helvetica',10),
                        value = format_number(a.y),
                        entry_width = 8,
                        entry_background = 'white')
            self.info.revert_script += 'self.new_template.%s.y = %s\n'%(parent.Name,a.y)
            self.info.revert_script += 'self.%s.y1.setentry(format_number(%s))\n'%(parent.Name,a.y)
            self.properties_y1.pack(expand = 1, fill = 'x', padx = 15, pady = 5)
            self.properties_y1.bind('<FocusOut>',gui_control.Command(self.update_value,self.properties_y1,attribute))
            self.properties_y1.component('entry').bind('<Return>',gui_control.Command(self.update_value,self.properties_y1,attribute))
        elif ('x1' in attributes) and ('y1' in attributes):
            attribute = '%s.x1'%parent.Name
            tempframe = Tkinter.Frame(group1.interior())
            self.properties_x1 = Pmw.EntryField(tempframe,
                        labelpos = 'w',
                        label_text = 'X1',
                        label_font = ('helvetica',10),
                        value = format_number(a.x1),
                        entry_width = 8,
                        entry_background = 'white')
            self.properties_x1.pack(side = 'left',expand = 1, fill = 'x')
            self.properties_x1.bind('<FocusOut>',gui_control.Command(self.update_value,self.properties_x1,attribute))
            self.properties_x1.component('entry').bind('<Return>',gui_control.Command(self.update_value,self.properties_x1,attribute))

            attribute = '%s.x1'%parent.Name
            self.properties_y1 = Pmw.EntryField(tempframe,
                        labelpos = 'w',
                        label_text = 'Y1',
                        label_font = ('helvetica',10),
                        value = format_number(a.y1),
                        entry_width = 8,
                        entry_background = 'white')
            self.properties_y1.pack(side = 'left',expand = 1, fill = 'x')
            self.properties_y1.bind('<FocusOut>',gui_control.Command(self.update_value,self.properties_y1,attribute))
            self.properties_y1.component('entry').bind('<Return>',gui_control.Command(self.update_value,self.properties_y1,attribute))

            self.info.revert_script += 'self.new_template.%s.x1 = %s\n'%(parent.Name,a.x1)
            self.info.revert_script += 'self.new_template.%s.y1 = %s\n'%(parent.Name,a.y1)
            self.info.revert_script += 'self.%s.x1.setentry(format_number(%s))\n'%(parent.Name,a.x1)
            self.info.revert_script += 'self.%s.y1.setentry(format_number(%s))\n'%(parent.Name,a.y1)
            tempframe.pack(expand = 1, fill = 'x', padx = 15, pady = 2)
            if ('x2' in attributes) and ('y2' in attributes):
                tempframe = Tkinter.Frame(group1.interior())
                attribute = '%s.x2'%parent.Name
                self.properties_x2 = Pmw.EntryField(tempframe,
                            labelpos = 'w',
                            label_text = 'X2',
                            label_font = ('helvetica',10),
                            value = format_number(a.x2),
                            entry_width = 8,
                            entry_background = 'white')
                self.properties_x2.pack(side = 'left',expand = 1, fill = 'x')
                self.properties_x2.bind('<FocusOut>',gui_control.Command(self.update_value,self.properties_x2,attribute))
                self.properties_x2.component('entry').bind('<Return>',gui_control.Command(self.update_value,self.properties_x2,attribute))

                attribute = '%s.y2'%parent.Name
                self.properties_y2 = Pmw.EntryField(tempframe,
                            labelpos = 'w',
                            label_text = 'Y2',
                            label_font = ('helvetica',10),
                            value = format_number(a.y2),
                            entry_width = 8,
                            entry_background = 'white')
                self.properties_y2.pack(side = 'left',expand = 1, fill = 'x')
                self.properties_y2.bind('<FocusOut>',gui_control.Command(self.update_value,self.properties_y2,attribute))
                self.properties_y2.component('entry').bind('<Return>',gui_control.Command(self.update_value,self.properties_y2,attribute))
                self.info.revert_script += 'self.new_template.%s.x2 = %s\n'%(parent.Name,a.x2)
                self.info.revert_script += 'self.new_template.%s.y2 = %s\n'%(parent.Name,a.y2)
                self.info.revert_script += 'self.%s.x2.setentry(format_number(%s))\n'%(parent.Name,a.x2)
                self.info.revert_script += 'self.%s.y2.setentry(format_number(%s))\n'%(parent.Name,a.y2)
                tempframe.pack(expand = 1, fill = 'x', padx = 15, pady = 5)
        else:
            tempframe = Tkinter.Frame(group1.interior())
            if 'x1' in attributes:
                attribute = '%s.x1'%parent.Name
                self.properties_x1 = Pmw.EntryField(tempframe,
                            labelpos = 'w',
                            label_text = 'X1',
                            label_font = ('helvetica',10),
                            value = format_number(a.x1),
                            entry_width = 8,
                            entry_background = 'white')
                self.properties_x1.pack(side = 'left',expand = 1, fill = 'x')
                self.properties_x1.bind('<FocusOut>',gui_control.Command(self.update_value,self.properties_x1,attribute))
                self.properties_x1.component('entry').bind('<Return>',gui_control.Command(self.update_value,self.properties_x1,attribute))

                attribute = '%s.x2'%parent.Name
                self.properties_x2 = Pmw.EntryField(tempframe,
                            labelpos = 'w',
                            label_text = 'X2',
                            label_font = ('helvetica',10),
                            value = format_number(a.x2),
                            entry_width = 8,
                            entry_background = 'white')
                self.properties_x2.pack(side = 'left',expand = 1, fill = 'x')
                self.properties_x2.bind('<FocusOut>',gui_control.Command(self.update_value,self.properties_x2,attribute))
                self.properties_x2.component('entry').bind('<Return>',gui_control.Command(self.update_value,self.properties_x2,attribute))
                self.info.revert_script += 'self.new_template.%s.x1 = %s\n'%(parent.Name,a.x1)
                self.info.revert_script += 'self.new_template.%s.x2 = %s\n'%(parent.Name,a.x2)
                self.info.revert_script += 'self.%s.x1.setentry(format_number(%s))\n'%(parent.Name,a.x1)
                self.info.revert_script += 'self.%s.x2.setentry(format_number(%s))\n'%(parent.Name,a.x2)
            else:
                attribute = '%s.y1'%parent.Name
                self.properties_y1 = Pmw.EntryField(tempframe,
                            labelpos = 'w',
                            label_text = 'Y1',
                            label_font = ('helvetica',10),
                            value = format_number(a.y1),
                            entry_width = 8,
                            entry_background = 'white')
                self.properties_y1.pack(side = 'left',expand = 1, fill = 'x')
                self.properties_y1.bind('<FocusOut>',gui_control.Command(self.update_value,self.properties_y1,attribute))
                self.properties_y1.component('entry').bind('<Return>',gui_control.Command(self.update_value,self.properties_y1,attribute))

                attribute = '%s.y2'%parent.Name
                self.properties_y2 = Pmw.EntryField(tempframe,
                            labelpos = 'w',
                            label_text = 'Y2',
                            label_font = ('helvetica',10),
                            value = format_number(a.y2),
                            entry_width = 8,
                            entry_background = 'white')
                self.properties_y2.pack(side = 'left',expand = 1, fill = 'x')
                self.properties_y2.bind('<FocusOut>',gui_control.Command(self.update_value,self.properties_y2,attribute))
                self.properties_y2.component('entry').bind('<Return>',gui_control.Command(self.update_value,self.properties_y2,attribute))
                self.info.revert_script += 'self.new_template.%s.y1 = %s\n'%(parent.Name,a.y1)
                self.info.revert_script += 'self.new_template.%s.y2 = %s\n'%(parent.Name,a.y2)
                self.info.revert_script += 'self.%s.y1.setentry(format_number(%s))\n'%(parent.Name,a.y1)
                self.info.revert_script += 'self.%s.y2.setentry(format_number(%s))\n'%(parent.Name,a.y2)
            tempframe.pack(expand = 1, fill = 'x', padx = 15, pady = 5)
    
        Pmw.alignlabels(labels)
        group1.pack(expand = 1, fill = 'x', padx = 10, pady = 10)

        # Position dialog popup
        if self.parent is not None:
           parent_geom = self.parent.geometry()
           geom = string.split(parent_geom, '+')
           d1 = string.atoi( geom[1] )
           d2 = string.atoi( geom[2] )
           self.dialog2.activate( geometry="+%d+%d" % (d1, d2) )
        else:
           self.dialog2.activate(geometry = 'centerscreenalways')

    def savefile(self):
        if self.template_name == 'default':
            self.saveas()
        else:
            result = askyesno('Warning','Are you sure want to overwrite\nthe \'%s\' template?'%self.template_name)
            if result == 'OK':
                self.copy_template()
                self.canvas.saveinitialfile()

    def copy_template(self):
        template = self.canvas.gettemplate(self.template_name)


    #----------------------------------------------------------
    # Handle the closing of the "Properties" button
    #----------------------------------------------------------
    def execute2(self,name,result):
        if result == 'OK':
            # Must set both, the template priority and the checkbutton toggle value. This comparison
            # check is used in the refresh_data function to update the template GUI.
            exec('self.new_template.%s.priority = int(self.properties_priority.get())'%name)
            exec('self.info.checkbutton["%s"] = self.new_template.%s.priority' % (name,name))
        else:
            commands = self.info.revert_script.split('\n')
            for i in range(len(commands)):
                exec(commands[i])
            self.canvas.flush()
            self.replot()
        self.dialog2.destroy()

    def saveas(self):
        self.saveasdialog = Pmw.Dialog(self.dialog.dialog.interior(),
                            title = 'Save Template As...',
                            buttons = ('OK', 'Cancel'),
                            defaultbutton='OK',
                            command = self._saveas)
        self.newname = Pmw.EntryField(self.saveasdialog.interior(),
                    labelpos = 'w',
                    label_text = 'New name:',
                    entry_background = 'white')
        self.newname.component('entry').bind('<Return>',gui_control.Command(self._saveas,'OK'))
        self.newname.pack(side = 'left', fill = 'x', expand = 1, padx = 10, pady = 10)
        
        # Position dialog popup
        parent_geom = self.dialog.geometry()
        geom = string.split(parent_geom, '+')
        d1 = string.atoi( geom[1] )
        d2 = string.atoi( geom[2] )
        self.saveasdialog.geometry( "+%d+%d" % (d1, d2) )

    def _saveas(self,result,evt = None):
        if result == 'OK':
            newname = self.newname.get()
            templates = vcs.listelements('template')
            if newname in templates:
                gui_message.error('Error - Template name already exists.\nPlease choose a new name.')    
                return
            else:
                try:
                    # Copy temporary template to new template name. Then tell
                    #  the plot to use the new template...refresh the data
                    self.canvas.createtemplate(newname,self.new_template.name)
                    self.template_name = newname
                    self.plot.template = newname
                    self.get_template()
                    self.refresh_data()
 
                    self.dialog.title( "Template Editor: %s" % newname)
                    # Redisplay the template list
                    if (self.no_gui_parent == 0):
                       t_list = vcs.listelements( 'template' )
                       self.parent.panelDV.template_listbox.setlist( t_list )
                       name_index = t_list.index( newname )
                       self.parent.panelDV.template_listbox.select_set( name_index )
                       self.parent.panelDV.template_listbox.see( name_index )

                except:
                    gui_message.error('Error - Could not save template as %s.' % newname)    

        self.saveasdialog.destroy()
    
    def create_new(self):
        self.create_new_dialog = Pmw.PromptDialog(self.dialog.interior(),
                 title = 'Create New Template from %s' % self.template_name,
                 label_text='Enter New Template Name:',
                 entryfield_labelpos='nw',
                 entry_width = 50,
                 entry_background = 'white',
                 entry_foreground = 'black',
                 buttons = ('OK', 'Cancel'),
                 defaultbutton='OK',
                 command = self.evt_create_new)

        # Position dialog popup
        parent_geom = self.dialog.geometry()
        geom = string.split(parent_geom, '+')
        d1 = string.atoi( geom[1] )
        d2 = string.atoi( geom[2] )
        self.create_new_dialog.geometry( "+%d+%d" % (d1, d2) )

    def evt_create_new(self,result):
        if result == 'OK':
            new_name = self.create_new_dialog.component('entry').get()
            if new_name in vcs.listelements( 'template' ):
               gui_message.error( 'Error occurred while trying to create a new template %s from %s. Make sure the new template name does not already exist.' % (self.template_name, new_name) )
               return
            try:
               self.canvas.createtemplate( new_name, self.new_template.name )
            except:
               gui_message.error( 'Error occurred while trying to create a new template %s from %s. Make sure the new template name does not already exist.' % (self.template_name, new_name) )
               return

            # Redisplay the template list
            if (self.no_gui_parent == 0):
               t_list = vcs.listelements( 'template' )
               self.parent.panelDV.template_listbox.setlist( t_list )
               name_index = t_list.index( new_name )
               self.parent.panelDV.template_listbox.select_set( name_index )
               self.parent.panelDV.template_listbox.see( name_index )

            self.create_new_dialog.destroy()
        elif (result in [None,'Cancel']):
            self.create_new_dialog.destroy()

    def open_new(self):
        self.open_new_dialog = Pmw.Dialog(self.dialog.interior(),
                        title = 'Choose Template to Edit',
                        buttons = ('OK','Cancel'),
                        defaultbutton='OK',
                        command = gui_control.Command(self._open_new))
        self.templates = Pmw.ComboBox(self.open_new_dialog.interior(),
                        labelpos = 'w',
                        label_text = 'Template:',
                        entry_background = 'white',
                        scrolledlist_items = vcs.listelements('template'))
        self.templates.selectitem(vcs.listelements('template')[0])
        self.templates.pack(side = 'left', fill = 'x', expand = 1, padx = 10, pady = 10)
                
        # Position dialog popup
        parent_geom = self.dialog.geometry()
        geom = string.split(parent_geom, '+')
        d1 = string.atoi( geom[1] )
        d2 = string.atoi( geom[2] )
        self.open_new_dialog.geometry( "+%d+%d" % (d1, d2) )

    def _open_new(self,result):
        if result == 'OK':
            template_name = self.templates.get()
            if template_name not in vcs.listelements('template'):
                showerror('Error','%s template does not exist'%template_name)
                return
            else:
                self.open_new_dialog.destroy()
                self.execute('Dismiss', template_name)
        else:
            self.open_new_dialog.destroy()
        
    def exit(self):
        # Reset canvas mode to DATA
        self.remove_temp_templates()
        self.canvas._SCREEN_DATA_FLAG()
        if self.no_gui_parent == 1:
            self.dialog.dialog.destroy()
        else:
            self.dialog.destroy()



#----------------------------------------------------------
# This creates a template object and packs it into the current
#  master widget.
#----------------------------------------------------------
class create_attribute:
    def __init__(self,
            name = None,  
            parent = None,
            master = None, 
            x1 = None, 
            x2 = None, 
            y1=None, 
            y2=None, 
            row = 0):
            
        if parent:
             a = eval('parent.new_template.'+name)
        else:
             return

        self.Name = name
        #self.select = Tkinter.Checkbutton(master)
        self.display = Tkinter.Checkbutton(master,
                selectcolor=gui_color.one,
                command = gui_control.Command(toggle_on_off,self,parent))

        self.label = Tkinter.Label(master,
                text = name.capitalize(),
                font = ('helvetica',10),
                anchor = 'w')

        if x1 is not None:
            try:
                x = a.x
                attribute = '%s.x'%name
            except:
                x = a.x1
                attribute = '%s.x1'%name
            self.x1 = Pmw.EntryField(master,
                    labelpos = 'w',
                    label_text = x1,
                    label_font = ('helvetica',10),
                    validate = 'real',
                    value = format_number(x),
                    entry_width=8,
                    entry_state = 'disabled',
                    entry_foreground = gui_color.off_color_fg,
                    entry_background = gui_color.off_color_bg)
            self.x1.bind('<FocusOut>',gui_control.Command(parent.update_value,self.x1,attribute))
            self.x1.component('entry').bind('<Return>',gui_control.Command(parent.update_value,self.x1,attribute))
            self.x1.component('entry').bind('<Key>', gui_control.Command(parent.evt_change_color, master, self.x1))
        else:
            self.x1 = x1
        if x2 is not None:
            attribute = '%s.x2'%name
            self.x2 = Pmw.EntryField(master,
                    labelpos = 'w',
                    label_text = x2,
                    label_font = ('helvetica',10),
                    validate = 'real',
                    value = format_number(a.x2),
                    entry_state = 'disabled',
                    entry_width=8,
                    entry_foreground = gui_color.off_color_fg,
                    entry_background = gui_color.off_color_bg)
            self.x2.bind('<FocusOut>',gui_control.Command(parent.update_value,self.x2,attribute))
            self.x2.component('entry').bind('<Return>',gui_control.Command(parent.update_value,self.x2,attribute))
            self.x2.component('entry').bind('<Key>', gui_control.Command(parent.evt_change_color, master, self.x2))
        else:
            self.x2 = x2

        if y1 is not None:
            try:
                y = a.y
                attribute = '%s.y'%name
            except:
                y = a.y1
                attribute = '%s.y1'%name
            self.y1 = Pmw.EntryField(master,
                    labelpos = 'w',
                    label_text = y1,
                    label_font = ('helvetica',10),
                    validate = 'real',
                    value = format_number(y),
                    entry_state = 'disabled',
                    entry_width=8,
                    entry_foreground = gui_color.off_color_fg,
                    entry_background = gui_color.off_color_bg)
            self.y1.bind('<FocusOut>',gui_control.Command(parent.update_value,self.y1,attribute))
            self.y1.component('entry').bind('<Return>',gui_control.Command(parent.update_value,self.y1,attribute))
            self.y1.component('entry').bind('<Key>', gui_control.Command(parent.evt_change_color, master, self.y1))
        else:
            self.y1 = y1 

        if y2 is not None:
            attribute = '%s.y2'%name
            y = '%3.3f'%a.y2
            self.y2 = Pmw.EntryField(master,
                    labelpos = 'w',
                    label_text = y2,
                    label_font = ('helvetica',10),
                    validate = 'real',
                    value = format_number(a.y2),
                    entry_state = 'disabled',
                    entry_width=8,
                    entry_foreground = gui_color.off_color_fg,
                    entry_background = gui_color.off_color_bg)
            self.y2.bind('<FocusOut>',gui_control.Command(parent.update_value,self.y2,attribute))
            self.y2.component('entry').bind('<Return>',gui_control.Command(parent.update_value,self.y2,attribute))
            self.y2.component('entry').bind('<Key>', gui_control.Command(parent.evt_change_color, master, self.y2))
        else:
            self.y2 = y2

        self.properties = Tkinter.Button(master,
                            font = ('helvetica',10),
                            state = 'disabled',
                            command = gui_control.Command(parent.show_properties,self,master),
                            text = 'Properties')
    
        # If this is attribute is a box, pack it accordingly
        if x1 and x2 and y1 and y2: 
            #self.select.grid(row = row, rowspan = 2, column = 0, sticky = 'nsew')
            self.display.grid(row = row, rowspan = 2, column = 1, sticky = 'nsew')
            self.label.grid(row = row, rowspan = 2, column = 2, padx = 2,sticky = 'nsew')
            self.x1.grid(row = row, column = 3)
            self.y1.grid(row = row, column = 4) 
            self.x2.grid(row = row+1, column = 3)
            self.y2.grid(row = row+1, column = 4) 
            self.properties.grid(row = row, rowspan = 2, column = 5,padx = 10, sticky = 'we')
            spacer(master).grid(row = row+2, column = 0,pady = 3)
            # If this is attribute only has an x,y coordinate, pack accordingly
        elif x1 and y1:
            #self.select.grid(row = row, column = 0, sticky = 'nsew')
            self.display.grid(row = row, column = 1, sticky = 'nsew')
            self.label.grid(row = row, column = 2, padx = 2,sticky = 'nsew')
            self.x1.grid(row = row, column = 3)
            self.y1.grid(row = row, column = 4) 
            self.properties.grid(row = row, column = 5,padx = 10, sticky = 'we')
        # If this is attribute only has an x1,x2 coordinate, pack accordingly
        elif x1 and x2:
            #self.select.grid(row = row, column = 0, sticky = 'nsew')
            self.display.grid(row = row, column = 1, sticky = 'nsew')
            self.label.grid(row = row, column = 2, padx = 2,sticky = 'nsew')
            self.x1.grid(row = row, column = 3)
            self.x2.grid(row = row, column = 4) 
            self.properties.grid(row = row, column = 5,padx = 10, sticky = 'we')
        # If this is attribute only has an y1,y2 coordinate, pack accordingly
        elif y1 and y2:   
            #self.select.grid(row = row, column = 0, sticky = 'nsew')
            self.display.grid(row = row, column = 1, sticky = 'nsew')
            self.label.grid(row = row, column = 2, padx = 2,sticky = 'nsew')
            self.y1.grid(row = row, column = 3)
            self.y2.grid(row = row, column = 4) 
            self.properties.grid(row = row, column = 5,padx = 10, sticky = 'we')
        elif x1:
            #self.select.grid(row = row, column = 0, sticky = 'nsew')
            self.display.grid(row = row, column = 1, sticky = 'nsew')
            self.label.grid(row = row, column = 2, padx = 2,sticky = 'nsew')
            self.x1.grid(row = row, column = 3)
            self.properties.grid(row = row, column = 5,padx = 10, sticky = 'we')
        elif y1:
            #self.select.grid(row = row, column = 0, sticky = 'nsew')
            self.display.grid(row = row, column = 1, sticky = 'nsew')
            self.label.grid(row = row, column = 2, padx = 2,sticky = 'nsew')
            self.y1.grid(row = row, column = 3)
            self.properties.grid(row = row, column = 5,padx = 10, sticky = 'we')
    
        if a.priority > 0:
            temp = parent.mode
            parent.mode = 0
            self.display.invoke()
            parent.mode = temp

#--------------------------------------------------------------
# When an attribute is selected to be displayed, highlight the 
#  label name, activate the coordinate entryfields and activate
#  the properties button. When it is unselected, do the reverse.
#--------------------------------------------------------------
def toggle_on_off(self,parent):
    parent.changed = 1
    item = eval('parent.new_template.%s'%self.Name)
    if (parent.finished_gui):
        try: x1=string.atof( self.x1.get() )
        except: x1 = -999.0
        try: x2=string.atof( self.x2.get() )
        except: x2 = -999.0
        try: y1=string.atof( self.y1.get() )
        except: y1 = -999.0
        try: y2=string.atof( self.y2.get() )
        except: y2 = -999.0

    if parent.info.checkbutton[self.Name]:
        parent.info.checkbutton[self.Name] = 0
        if (parent.finished_gui):
            item.priority = 0
            parent.canvas.flush()
            try: parent.replot()
            except: pass
            item.priority = 1
            parent.canvas._unselect_one(parent.new_template.name,self.Name,x1, x2, y1, y2)
        item.priority = 0
        change_field_mode(self,0)
    else:
        parent.info.checkbutton[self.Name] = 1 
        item.priority = max(item.priority,1)
        change_field_mode(self,1)
        if (parent.mode):
            parent.canvas.flush()
            try: parent.replot()
            except: pass
            parent.canvas.flush()
        if (parent.finished_gui):
            parent.canvas._select_one(parent.new_template.name,self.Name,x1, x2, y1, y2)
            self.display.configure( selectcolor = hot_color )

def change_field_mode(self,mode):
    if mode == 0:
        self.label.configure(font = ('helvetica', 10))
        for i in (self.x1, self.x2, self.y1, self.y2):
            if i:
                i.configure(entry_background = gui_color.off_color_bg)
                i.configure(entry_foreground = gui_color.off_color_fg)
                i.configure(entry_state = 'disabled')
        self.properties.configure(state = 'disabled')
    else:
        self.label.configure(font = ('helvetica', 10,'bold'))
        for i in (self.x1, self.x2, self.y1, self.y2):
            if i:
                i.configure(entry_background = gui_color.on_color_bg)
                i.configure(entry_foreground = gui_color.on_color_fg)
                i.configure(entry_state = 'normal')
        self.properties.configure(state = 'normal')

#--------------------------------------------------------------
# Create an invisible spacer to get around padx and pady
#  putting spaces on both sides
#--------------------------------------------------------------
def spacer(interior,height = 1,width = 1):
    return Tkinter.Frame(interior,
                borderwidth = 2,
                height = height, 
                width = width)

    
#--------------------------------------------------------------
# Format printed number so it is only has 'x' digits after the
#  decimal
#--------------------------------------------------------------
def format_number(value,digits = 4):
    format_string = "'%3."+str(digits)+"f'%value"
    return eval(format_string)

#----------------------------------------------------------
# Create a Dummy class
#----------------------------------------------------------
class busy:
   pass

#--------------------------------------------------------------
# Call mainloop() if called from the command line
#--------------------------------------------------------------
# Create/Popup template editor for VCS.
def create(gui_parent=None, canvas=None, plot=None, template_name='', template_orig_name='', called_from = 0):
    a = busy
    if gui_parent is not None: gui_busy.busyStart(a, gui_parent)
    t=Template_Editor(gui_parent=gui_parent, canvas=canvas, plot=plot, template_name=template_name, template_orig_name=template_orig_name, called_from = called_from )
    if gui_parent is not None: gui_busy.busyEnd( a, gui_parent )
    return t

if __name__ == '__main__':
    root = Tkinter.Tk()
    Pmw.initialise(root)
    root.option_add('*Font',('helvetica',10))
    Template_Editor(root,template_name = 'ASD')
    Tkinter.mainloop()
