## The projection GUI editor

###############################################################################
#                                                                             #
# Module:       projectiongui module                                          #
#                                                                             #
# Copyright:    "See file Legal.htm for copyright information."               #
#                                                                             #
# Authors:      PCMDI Software Team                                           #
#               Lawrence Livermore NationalLaboratory:                        #
#               support@pcmdi.llnl.gov                                        #
#                                                                             #
# Description:  PCMDI's VCS GUI projection editor.                            #
#                                                                             #
#                                                                             #
###############################################################################

import vcs_legacy
import Tkinter,Pmw,tkFileDialog
import string,browser
import os
import gui_support
from browser import gui_control
import copy

# Create/Popup projection GUI for VCS.
def create(gui_parent=None,projection='default',canvas=None):
    return ProjGUI(canvas,gui_parent,projection)

class ProjGUI:
    def save_vals(self):
        orig={}
        orig['name']=self.projection.name
        orig['parameters']=self.projection.parameters
        orig['type']=self.projection.type
        self.orig.append(copy.copy(orig))
             
    def __init__(self, canvas=None, gui_parent=None, dialog_parent=None, master=None, name='default',Parent=None):
        self.gui_parent=gui_parent
        self.dialog_parent=dialog_parent
        self.master=master
        
#        print 'PASSED INSIDE:',gui_parent
        if canvas is None:
##             import Canvas
##             self.canvas = Canvas.Canvas()
            self.canvas=vcs_legacy.init()
        else:
            self.canvas=canvas
            
        self.projection=self.canvas.getprojection(name)
        if self.projection is None:
            raise 'Erro invalid projection name'
        
        self.parent=Parent
        ## Saves original values
        self.orig=[]
        self.save_vals()
        
        Proj_color = gui_support.gui_color.Proj_color

#################################################################################################
# At the moment, this will never happen. When we need to pop this editor up on its own, then we
# will revist this case.
##         if self.gui_parent is None:
##             self.gui_parent=Tkinter.Toplevel()
##             #self.gui_parent=Tkinter.Tk(":0.0") # Use the localhost:0.0 for the DISPLAY and screen
##             self.gui_parent.withdraw()
#################################################################################################

        title='Projection Editor - '+self.projection.name
        self.dialog = Pmw.Dialog(master,
                                 title=title,
                                 buttons=(),
                                )
        self.dialog.withdraw()

        if gui_support.root_exists():
            root = gui_support.root()
            self.top_parent = root
        else:
            root = gui_support.root()
            self.top_parent = None
        self.root = root
        parent=self.dialog.interior()
        parent.configure(bg=Proj_color)
        self.cmain_menu = Pmw.MenuBar(parent,
                hull_relief = 'raised',
                hull_borderwidth = 2,
                balloon = gui_support.balloon
                )
        self.cmain_menu.pack(side='top', fill='both')

        self.cmain_menu.addmenu('File', 'Open/Save VCS Projections', tearoff = 1)
        self.cmain_menu.addmenuitem('File', 'command', 'Open projection file',
                         label = 'Open Projection File',
                         command = self.evt_open_file,
                        )

        self.cmain_menu.addmenuitem('File', 'separator')

        #
        # Create the cascade "Save Colormap" menu and its items
##         self.cmain_menu.addmenuitem('File', 'command', 'Select projection',
##                                     label = 'Select',
##                                     command = self.evt_loadproj,
##                                     )
        self.cmain_menu.addmenuitem('File', 'command', 'Save projection',
                                    label = 'Save (i.e Apply changes)',
                                    command = self.setprojection,
                                    )
        self.cmain_menu.addmenuitem('File', 'command', 'Copy projection',
                                    label = 'Copy Projection',
                                    command = self.evt_save_as,
                                    )
        self.cmain_menu.addmenuitem('File', 'command', 'Save as file',
                                    label = 'Save To File',
                                    command = self.evt_save_to_file,
                                    )

        # Create the cascade "Exit" menu
        self.cmain_menu.addmenuitem('File', 'separator')
        self.cmain_menu.addmenuitem('File', 'command',
                                    statusHelp='Close Projection Editor',
                                    label = "Exit Projection Editor",
                                    command = self.dialog.destroy,
                                    )
        
##         self.ftype=Tkinter.Frame(parent)
##         self.ftype.pack(side='top')
        projs=self.canvas.listelements('projection')
        projs.sort()

        self.projections=Pmw.OptionMenu(parent,
                                  items=projs,
                                  labelpos='w',
                                  label_text='Name: ',
                                  command=self.loadproj,
                                  initialitem=self.projection.name,
                                  label_bg=Proj_color,
                                  hull_bg=Proj_color,
                                  menu_bg=Proj_color,
                                  menu_activebackground=Proj_color,
                                  menubutton_bg=Proj_color,
                                  menubutton_activebackground=Proj_color,
                                 )
        self.projections.pack()
        
        self.fprop=Pmw.Group(parent,tag_text='Properties',
                             tagindent=10,
                             tag_bg=Proj_color,
                             ring_bg=Proj_color,
                             hull_bg=Proj_color)
        self.fprop.pack(expand='yes',fill='both')
        inter=self.fprop.interior()
        inter.configure(bg=Proj_color)
        self.fproperties=Tkinter.Frame(inter,bg=Proj_color)
        self.fbuttons=Tkinter.Frame(parent,bg=Proj_color)
        self.fbuttons.pack()

        self.projtypes=["polar (non gctp)",
                        "mollweide (non gctp)",
                        "robinson (non gctp)",
                        "linear",
                        "utm",
                        "state plane",
                        "albers equal area",
                        "lambert",
                        "mercator",
                        "polar stereographic",
                        "polyconic",
                        "equid conic",
                        "transverse mercator",
                        "stereographic",
                        "lambert azimuthal",
                        "azimuthal",
                        "gnomonic",
                        "orthographic",
                        "gen. vert. near per",
                        "sinusoidal",
                        "equirectangular",
                        "miller cylindrical",
                        "van der grinten",
                        "hotin oblique",
                        "robinson",
                        "space oblique",
                        "alaska conformal",
                        "interrupted goode",
                        "mollweide",
                        "interrupted mollweide",
                        "hammer",
                        "wagner iv",
                        "wagner vii",
                        "oblated equal area",
                        ]
        
        self.projtype = Pmw.OptionMenu (inter,
                                        labelpos='w',
                                        label_text='Projection Type',
                                        label_bg=Proj_color,
                                        hull_bg=Proj_color,
                                        menu_bg=Proj_color,
                                        menu_activebackground=Proj_color,
                                        menubutton_bg=Proj_color,
                                        menubutton_activebackground=Proj_color,
                                        items=  self.projtypes,
                                        command=self.settype,
                                        )
        self.projtype.pack()
        b1=Tkinter.Button(self.fbuttons,
                          text='Cancel',
                          command=self.cancel)
        b1.pack(side='left')
        b2=Tkinter.Button(self.fbuttons,
                          text='Apply',
                          command=self.exit)
        b2.pack(side='left')
        b3=Tkinter.Button(self.fbuttons,
                          text='Revert',
                          command=self.reset)
        b3.pack(side='left')

        self.setgui()

        # Position dialog popup
        if dialog_parent is not None:
            parent_geom = dialog_parent.geometry()
            geom = string.split(parent_geom, '+')
            d1 = string.atoi( geom[1] )
            d2 = string.atoi( geom[2] )
            self.dialog.activate( geometry="+%d+%d" % (d1, d2) )
        else:
            self.dialog.activate(geometry = 'centerscreenalways')

        return

    def exit(self):
        self.setprojection()
        self.dialog.destroy()

##     def evt_loadproj(self):
##         names = self.canvas.listelements('projection')
##         names.sort()
##         name=self.projection.name
##         self.loaddialog = Pmw.ComboBoxDialog( self.gui_parent,
## 	    title = 'Select VCS Projection',
## 	    buttons = ('OK', 'Apply', 'Cancel'),
## 	    defaultbutton = 'OK',
## 	    combobox_labelpos = 'n',
## 	    label_text = 'Enter or Select VCS Projection',
## 	    scrolledlist_items = names,
##             command = self.loadproj)
##         self.loaddialog.setentry( name )

    def loadproj(self,result):
        a=self.projections.getvalue()
        self.projection=self.canvas.getprojection(a)
        self.save_vals()
        self.dialog.title('Projection Editor - '+self.projection.name)
        self.setgui()
        
    def settype(self,*crap):
        a=self.projtype.getvalue()
        self.projection.type=self.projtype.index(a)-3
        self.setgui()
        
    def setgui(self,*crap):
        Proj_color=gui_support.gui_color.Proj_color
        projs=self.canvas.listelements('projection')
        projs.sort()
        a=self.projection.name
        self.projections.setitems(projs)
        self.projections.setvalue(a)
        self.parent.projection.setitems(projs)
        self.parent.projection.invoke(a)
        self.parent.button.configure(command=gui_control.Command(ProjGUI,canvas=self.canvas,gui_parent=self.gui_parent,dialog_parent=self.dialog_parent,master=self.master,name=a,Parent=self.parent))

        self.projtype.configure(menubutton_state='normal')
        self.projtype.setvalue(self.projtypes[self.projection._type+3])
        self.fproperties.destroy()
        inter=self.fprop.interior()
        inter.configure(bg=Proj_color)
        self.fproperties=Tkinter.Frame(inter,bg=Proj_color,relief='sunken')
        self.fproperties.pack(expand='yes',fill='both')
        self.fields=[]
        p=[]
        balloons={
            'smajor':'Semi-major axis of ellipsoid.\nIf zero, Clarke 1866 in meters is assumed.',
            'sminor':'Eccentricity squared of the ellipsoid if less than zero\nIf zero, a spherical form is assumed\nIf greater than zero, the semi-minor axis of ellipsoid.',
            'sphere':'Radius of reference sphere.\nIf zero, 6370997 meters is used.',
            'standardparallel':'Latitude of the standard parallel',
            'standardparallel1':'Latitude of the first standard parallel',
            'standardparallel2':'Latitude of the second standard parallel',
            'centralmeridian':' Longitude of the central meridian',
            'originlatitude':'Latitude of the projection origin',
            'falseeasting':'False easting in the same units as the semi-major axis',
            'falsenorthing':'False northing in the same units as the semi-major axis',
            'truescale':'Latitude of true scale',
            'LongPol':'Longitude down below pole of map',
            'factor':'Scale factor at central meridian (Transverse Mercator)\nor\nCenter of projection (Hotine Oblique Mercator)',
            'centerlongitude':'Longitude of center of projection',
            'centerlatitude':'Latitude of center of projection',
            'height':'Height of perspective point',
            'longitude1':'Longitude of first point on center line',
            'longitude2':'Longitude of second point on center line',
            'latitude1':'Latitude of first point on center line',
            'latitude2':'Latitude of second point on center line',
            'azimuthalangle':'Azimuth angle east of north of center line',
            'azimuthallongitude':' Longitude of point on central meridian where azimuth occurs',
            'orbitinclination':'Inclination of orbit at ascending node\ncounter-clockwise from equator',
            'orbitlongitude':' Longitude of ascending orbit at equator',
            'satelliterevolutionperiod':'Period of satellite revolution in minutes',
            'landsatcompensationratio':'Landsat ratio to compensate for confusion at northern end of orbit\n(use 0.5201613)',
            'pathflag':'End of path flag for Landsat\n0 = start of path, 1 = end of path',
            'satellite':'Landsat Satellite Number',
            'path':'Landsat Path Number\nUse WRS-1 for Landsat 1, 2 and 3\nUse WRS-2 for Landsat 4, 5 and 6.',
            'shapem':'Oblated Equal Area oval shape parameter m',
            'shapen':'Oblated Equal Area oval shape parameter n',
            'angle':'Oblated Equal Area oval rotation angle',
            'subtype':'Subtype of the projection (0 or 1)',
            }
        if self.projection._type in [3,4]:
             p.append('smajor')
             p.append('sminor')
             p.append('standardparallel1')
             p.append('standardparallel2')
             p.append('centralmeridian')
             p.append('originlatitude')
             p.append('falseeasting')
             p.append('falsenorthing')
        elif self.projection._type==5:
             p.append('smajor')
             p.append('sminor')
             p.append('centralmeridian')
             p.append('truescale')
             p.append('falseeasting')
             p.append('falsenorthing')
        elif self.projection._type==6:
             p.append('smajor')
             p.append('sminor')
             p.append('centerlongitude')
             p.append('truescale')
             p.append('falseeasting')
             p.append('falsenorthing')
        elif self.projection._type==7:
             p.append('smajor')
             p.append('sminor')
             p.append('centralmeridian')
             p.append('originlatitude')
             p.append('falseeasting')
             p.append('falsenorthing')
        elif self.projection._type==8:
             p.append('subtype')
             p.append('smajor')
             p.append('sminor')
             p.append('centralmeridian')
             p.append('originlatitude')
             p.append('falseeasting')
             p.append('falsenorthing')       
             if self.projection.subtype==0:
                  p.append('standardparallel')
             else:
                  p.append('standardparallel1')
                  p.append('standardparallel2')
        elif self.projection._type==9:
             p.append('smajor')
             p.append('sminor')
             p.append('factor')
             p.append('centralmeridian')
             p.append('originlatitude')
             p.append('falseeasting')
             p.append('falsenorthing')    
        elif self.projection._type in [10,11,12,13,14]:
             p.append('sphere')
             p.append('centerlongitude')
             p.append('centerlatitude')
             p.append('falseeasting')
             p.append('falsenorthing')      
        elif self.projection._type==15:
             p.append('sphere')
             p.append('height')
             p.append('centerlongitude')
             p.append('centerlatitude')
             p.append('falseeasting')
             p.append('falsenorthing')  
        elif self.projection._type in [16,18,21,25,27,28,29]:
             p.append('sphere')
             p.append('centralmeridian')
             p.append('falseeasting')
             p.append('falsenorthing')     
        elif self.projection._type==17:
             p.append('sphere')
             p.append('centralmeridian')
             p.append('truescale')
             p.append('falseeasting')
             p.append('falsenorthing')      
        elif self.projection._type==19:
             p.append('sphere')
             p.append('centralmeridian')
             p.append('originlatitude')
             p.append('falseeasting')
             p.append('falsenorthing')        
        elif self.projection._type==20:
             p.append('subtype')
             p.append('smajor')
             p.append('sminor')
             p.append('factor')
             p.append('originlatitude')
             p.append('falseeasting')
             p.append('falsenorthing')
             if self.projection.subtype==0:
                  p.append('longitude1')
                  p.append('latitude1')
                  p.append('longitude2')
                  p.append('latitude2')
             else:
                  p.append('azimuthalangle')
                  p.append('azimuthallongitude')
        elif self.projection._type==22:
             p.append('subtype')
             p.append('smajor')
             p.append('sminor')
             p.append('falseeasting')
             p.append('falsenorthing')
             if self.projection.subtype==0:
                  p.append('orbitinclination')
                  p.append('orbitlongitude')
                  p.append('satelliterevolutionperiod')
                  p.append('landsatcompensationratio')
                  p.append('pathflag')
             else:
                  p.append('satellite')
                  p.append('path')
        elif self.projection._type==23:
             p.append('smajor')
             p.append('sminor')
             p.append('falseeasting')
             p.append('falsenorthing')      
        elif self.projection._type in [24,26]:
             p.append('sphere')
        elif self.projection._type==30:
             p.append('sphere')
             p.append('shapem')
             p.append('shapen')
             p.append('centerlongitude')
             p.append('centerlatitude')
             p.append('falseeasting')
             p.append('falsenorthing')
        self.properties_names=p
        for p in self.properties_names:
            f=Tkinter.Frame(self.fproperties)
            f.pack(expand='yes',fill='both')
            a=Pmw.EntryField(self.fproperties,
                             label_width=20,labelpos='w',
                             label_justify='left',
                             label_text=p,
                             label_bg=Proj_color,
                             hull_bg=Proj_color,
                             entry_relief='sunken',
                             entry_background='white',
                             command=self.setprojection,
                             value=getattr(self.projection,p),
##                              balloon=gui_support.balloon,
                             )
            a.pack(expand=1,fill='both')
            b=gui_support.balloon.bind( a, balloons[p] )
            self.fields.append(a)
        if self.projection.name in  ['default','robinson','polar','mollweide','linear']:
            self.projtype.configure(menubutton_state='disabled')
            for a in self.fields:
                a.configure(entry_state='disabled')
        self.setprojection()
        return

    def setprojection(self,*crap):
        n=len(self.properties_names)
        for i in range(n):
            p=self.properties_names[i]
            v=string.atof(self.fields[i].get())
            setattr(self.projection,p,v)
        self.canvas.update()
       

    # Open VCS file
    #
    def evt_open_file( self, ):
        # search for the following files
        filetypes = [
          ("VCS script files", "*.scr"),
#        ("Python files", "*.py *.pyw" ),
          ("All files", "*"),
          ]

        # Show the popup directory dialog
        dialog = tkFileDialog.Open(master=self.gui_parent,
                             filetypes=filetypes,
                             title = 'VCS Projections File Select')

        dirfilename=dialog.show(initialdir=os.getcwd())

        # Run VCS script
        if len(dirfilename) > 0:
            self.canvas.scriptrun(dirfilename)

    # Save VCS projection as a new name
    #
    def evt_save_as( self):
##    def __init__(self, eself, parent, vcs_legacy, type, title, text, entry_str = ''):
##       create_entry_popup(eself, parent, vcs_legacy, 'sa',('Save VCS colormap ( %s ) as:' % vcs_legacy.getcolormapname()),'Enter the new name of the colormap:\n\tFor example:\n\t\tnewcolormapname')
        self.savedialog = Pmw.Dialog( self.gui_parent,
            title = 'Save VCS Projection ( %s ) as:' % self.projection.name,
            buttons = ('OK', 'Dismiss'),
            defaultbutton = 'OK',
            command = self.rename_proj )

        lbl=Tkinter.Label(self.savedialog.interior(),
            text = 'Enter the new name of the projection:\n\tFor example:\n\t\tnewprojectionname',
            justify = 'left',
            anchor = 'w',
            )
        lbl.pack( expand = 1, fill = 'both', padx=10, pady=5 )

        self.eny1=Pmw.EntryField(self.savedialog.interior(),
            labelpos = 'w',
            entry_background = 'white',
            entry_foreground = 'black',
            value = '',
            entry_width =  50,
            )
        self.eny1.pack( expand = 1, fill = 'both', padx=10, pady=5 )

        # Position dialog popup
        parent_geom = self.dialog.geometry()
        geom = string.split(parent_geom, '+')
        d1 = string.atoi( geom[1] )
        d2 = string.atoi( geom[2] )
        geom = "+%d+%d" % (d1, d2)
        self.savedialog.activate(geometry = geom)

    # Save, copy, and write colormap to a file
    #
    def rename_proj(self,result):
##         print 'Result is:',result
        if result=='OK':
            newname=self.eny1.get( )
            self.projection=self.canvas.createprojection(newname,self.projection.name)
            self.save_vals()
##             print 'Should be changing the dialog now !',self.projection.name
            self.dialog.title('Projection Editor - '+self.projection.name)
            self.setgui()
        self.savedialog.destroy()
        
    # Save VCS Color map to a file
    #
    def evt_save_to_file( self):
        filetypes = [ ("VCS Script File", ".scr") ]
        # Show the popup directory dialog
        sfile = tkFileDialog.asksaveasfilename( master=self.gui_parent, filetypes = filetypes,
                             title = 'Save VCS Projection to a File' )
        if sfile == '': return
        if sfile[-4:] != '.scr': sfile += '.scr'

        self.projection.script(sfile)

    def cancel(self):
        self.reset()
        self.dialog.destroy()

    def reset(self):
        if self.orig!=[]:
            try:
                for orig in self.orig[::-1]:
                    self.projection=self.canvas.getprojection(orig['name'])
                    if not self.projection.name in  ['default','robinson','polar','mollweide','linear']:
                        for a in ['type','parameters']:
                            setattr(self.projection,a,orig[a])
                self.setgui()
                self.orig=[]
                self.save_vals()
            except Exception,err:
                print 'Error:',err,'--------------------------------------'
    
# Create/Popup projection GUI for VCS.
def create(gui_parent=None,projection='default',canvas=None):
    return ProjGUI(canvas,gui_parent,projection)

if __name__=='__main__':
    gui=create()
    gui.gui_parent.mainloop()
