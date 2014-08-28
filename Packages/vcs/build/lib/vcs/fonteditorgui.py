# Adapted for numpy/ma/cdms2 by convertcdms.py
## The font GUI editor

###############################################################################
#                                                                             #
#                                                                             #
# Copyright:    "See file Legal.htm for copyright information."               #
#                                                                             #
# Authors:      PCMDI Software Team                                           #
#               Lawrence Livermore NationalLaboratory:                        #
#               support@pcmdi.llnl.gov                                        #
#                                                                             #
# Description:  PCMDI's VCS GUI font editor.                            #
#                                                                             #
#                                                                             #
###############################################################################

import vcs
import Tkinter,Pmw,tkFileDialog
from browser import gui_control
import string,browser
import os
import gui_support
import copy

# Create/Popup projection GUI for VCS.
def create(gui_parent=None,table='default',orientation='default',canvas=None,parent=None):
    return FontGUI(canvas,gui_parent,table,orientation,parent)

class FontGUI:

    def save_vals(self):
        orig={}
        orig['font']=self.text.font
        orig['spacing']=self.text.spacing
        orig['expansion']=self.text.expansion
        orig['color']=self.text.color
        orig['height']=self.text.height
        orig['angle']=self.text.angle
        orig['path']=self.text.path
        orig['halign']=self.text.halign
        orig['valign']=self.text.valign
        orig['Tt_name']=self.text.Tt_name
        orig['To_name']=self.text.To_name
        self.orig.append(copy.copy(orig))
             
    def __init__(self, canvas=None, gui_parent=None, dialog_parent=None, master=None, t_name='default',o_name='default',Parent=None):
        self.gui_parent=gui_parent
        if canvas is None:
##             import Canvas
##             self.canvas = Canvas.Canvas()
            self.canvas=vcs.init()
        else:
            self.canvas=canvas

        self.text=self.canvas.gettext(t_name,o_name)
        if self.text is None:
            if not t_name in vcs.listelements('texttable'):
                raise 'Error '+t_name+' is not a valid texttable name'
            elif not o_name in vcs.listelements('textorientation'):
                raise 'Error '+o_name+' is not a valid textorientation name'
            else:
                raise 'Weird unkwnown error but no text object returned....'
        self.parent=Parent
        try:
            self.parent_Name=self.parent.parent_Name
        except:
            pass
        ## Saves original values
        self.orig=[]
        self.save_vals()
        
        Tt_color = gui_support.gui_color.Tt_color
        To_color = gui_support.gui_color.To_color
        
#################################################################################################
# At the moment, this will never happen. When we need to pop this editor up on its own, then we
# will revist this case.
#        if self.gui_parent is None:
#            self.gui_parent=Tkinter.Toplevel()
#            #self.gui_parent=Tkinter.Tk(":0.0") # Use the localhost:0.0 for the DISPLAY and screen
#            self.gui_parent.withdraw()
#################################################################################################

        title='Text Editor - table: '+self.text.Tt_name+', orientation: '+self.text.To_name
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
        parent.configure(bg=Tt_color)
        self.cmain_menu = Pmw.MenuBar(parent,
                hull_relief = 'raised',
                hull_borderwidth = 2,
                balloon = gui_support.balloon
                )
        self.cmain_menu.pack(side='top', fill='both')

        self.cmain_menu.addmenu('File', 'Open/Save VCS Text Objects', tearoff = 1)
        self.cmain_menu.addmenuitem('File', 'command', 'Open textobject file',
                         label = 'Open TextObject File',
                         command = gui_control.Command( self.evt_open_file, master ),
                        )

        self.cmain_menu.addmenuitem('File', 'separator')

        #
        # Create the cascade "Save Colormap" menu and its items
##         self.cmain_menu.addmenuitem('File', 'command', 'Select Table',
##                                     label = 'Select Table',
##                                     command = self.evt_loadtable,
##                                     )
##         self.cmain_menu.addmenuitem('File', 'command', 'Select Orientation',
##                                     label = 'Select Orientation',
##                                     command = self.evt_loadorientation,
##                                     )
        self.cmain_menu.addmenuitem('File', 'command', 'Save TextObject',
                                    label = 'Save (i.e Apply changes)',
                                    command = self.setfont,
                                    )
        self.cmain_menu.addmenuitem('File', 'command', 'Copy Table',
                                    label = 'Copy Table',
                                    command = gui_control.Command( self.evt_save_table_as, self.dialog ),
                                    )
        self.cmain_menu.addmenuitem('File', 'command', 'Copy Orientation',
                                    label = 'Copy Orientation',
                                    command = gui_control.Command( self.evt_save_orientation_as, self.dialog ),
                                    )
        self.cmain_menu.addmenuitem('File', 'command', 'Save to file',
                                    label = 'Save To File',
                                    command = gui_control.Command( self.evt_save_to_file, master ),
                                    )

        # Create the cascade "Exit" menu
        self.cmain_menu.addmenuitem('File', 'separator')
        self.cmain_menu.addmenuitem('File', 'command',
                                    statusHelp='Close TextObject Editor',
                                    label = "Exit TextObject Editor",
                                    command = self.dialog.destroy,
                                    )
        
##         self.ftype=Tkinter.Frame(parent)
##         self.ftype.pack(side='top')
        self.tprop=Pmw.Group(parent,
                             tag_text='Table',
                             tag_bg=Tt_color,
                             tagindent=10,
                             hull_bg=Tt_color,
                             hull_highlightbackground=Tt_color,
                             hull_highlightcolor=Tt_color,
                            ring_bg=Tt_color,
                             )
        self.tprop.pack(expand='yes',fill='both')
        self.tproperties=self.tprop.interior()
        self.tproperties.configure(bg=Tt_color)
        tables=vcs.listelements('texttable')
        tables.sort()

        labels=[]
        self.table=Pmw.OptionMenu(self.tproperties,
                                  items=tables,
                                  labelpos='w',
                                  label_text='Name:',
                                  command=self.loadtable,
                                  initialitem=self.text.Tt_name,
                                  label_bg=Tt_color,
                                  hull_bg=Tt_color,
                                  menu_bg=Tt_color,
                                  menu_activebackground=Tt_color,
                                  menubutton_bg=Tt_color,
                                  menubutton_activebackground=Tt_color,
                                 )
        self.table.pack()
        labels.append(self.table)
        
        fonts=vcs.listelements("font")
        self.font=Pmw.OptionMenu(self.tproperties,
                                 items=fonts,
                                 labelpos='w',
                                 label_text='Font:',
                                 label_bg=Tt_color,
                                 hull_bg=Tt_color,
                                 menu_bg=Tt_color,
                                 menu_activebackground=Tt_color,
                                 menubutton_bg=Tt_color,
                                 menubutton_activebackground=Tt_color,
                                 command=self.setfont,
                                 )
        self.font.pack()
        labels.append(self.font)

        f=Tkinter.Frame(self.tproperties,bg=Tt_color)
        l=Tkinter.Label(f,
                        text='Spacing:',
                        bg=Tt_color,
                        )
        l.pack(side='left')
        self.spacing=Tkinter.Scale(f,
                                   bigincrement=10,
                                   from_=-50,
                                   to=50,
                                   orient='horizontal',
                                   tickinterval=25,
                                   length=200,
                                   bg=Tt_color,
                                   activebackground=Tt_color,
                                   highlightbackground=Tt_color,
                                   command=self.setfont,
                                  )
        self.spacing.pack()
        labels.append(l)
        f.pack()
        f=Tkinter.Frame(self.tproperties,bg=Tt_color)
        l=Tkinter.Label(f,
                        text='Width:',
                        bg=Tt_color,
                        )
        l.pack(side='left')
        self.expansion=Tkinter.Scale(f,
                                     bigincrement=10,
                                     from_=50,
                                     to=150,
                                     orient='horizontal',
                                     tickinterval=25,
                                     length=200,
                                     bg=Tt_color,
                                     activebackground=Tt_color,
                                     highlightbackground=Tt_color,
                                     command=self.setfont,
                                     )
        self.expansion.pack()
        labels.append(l)
        f.pack()

        f=Tkinter.Frame(self.tproperties,bg=Tt_color)
        
        l=Tkinter.Label(f,
                        text='Color:',
                        bg=Tt_color,
                        )
        
        l.pack(side='left')
        
        self.Color=Tkinter.Scale(f,
                                 bigincrement=50,
                                 from_=0,
                                 to=255,
                                 orient='horizontal',
                                 tickinterval=50,
                                 length=200,
                                 bg=Tt_color,
                                 activebackground=Tt_color,
                                 highlightbackground=Tt_color,
                                 command=self.setfont,
                                 )
        
        self.Color.pack()
        labels.append(l)
        f.pack()

        Pmw.alignlabels(labels)
        labels=[]
        self.oprop=Pmw.Group(parent,tag_text='Orientation',tagindent=10,tag_bg=To_color,ring_bg=To_color,hull_bg=To_color)
        self.oprop.pack(expand='yes',fill='both')
        self.oproperties=self.oprop.interior()
        self.oproperties.configure(bg=To_color)
        
        orientations=vcs.listelements('textorientation')
        orientations.sort()
        self.orientation=Pmw.OptionMenu(self.oproperties,
                                        items=orientations,
                                        labelpos='w',
                                        label_text='Name:',
                                        label_bg=To_color,
                                        hull_bg=To_color,
                                        menu_bg=To_color,
                                        menu_activebackground=To_color,
                                        menubutton_bg=To_color,
                                        menubutton_activebackground=To_color,
                                        command=self.loadorientation,
                                        initialitem=self.text.To_name,
                                 )
        self.orientation.pack()
        labels.append(self.orientation)
        
        self.height=Pmw.EntryField(self.oproperties,
                                   label_text='Size:',
                                   labelpos='w',
                                   label_justify='left',
                                   label_bg=To_color,
                                   hull_bg=To_color,
                                   validate={'min':0,'validator':'real'},
                                   command=self.setfont,
                                   )
        self.height.pack()
        labels.append(self.height)
        
        f=Tkinter.Frame(self.oproperties,bg=To_color)
        l=Tkinter.Label(f,
                        text='Angle:',
                        bg=To_color,
                        )
        l.pack(side='left')
        labels.append(l)
        self.angle=Tkinter.Scale(f,
                                 bigincrement=30,
                                 from_=-180,
                                 to=180,
                                 orient='horizontal',
                                 tickinterval=90,
                                 length=200,
                                 bg=To_color,
                                 activebackground=To_color,
                                 highlightbackground=To_color,
                                 command=self.setfont,
                                 )
        self.angle.pack()
        f.pack()

        paths=["right","left","up","down"]
        self.path=Pmw.OptionMenu(self.oproperties,
                                 items=paths,
                                 labelpos='w',
                                 label_text='Path:',
                                 label_bg=To_color,
                                 hull_bg=To_color,
                                 menu_bg=To_color,
                                 menu_activebackground=To_color,
                                 menubutton_bg=To_color,
                                 menubutton_activebackground=To_color,
                                 command=self.setfont,
                                 )
        self.path.pack()
        labels.append(self.path)
        
        haligns=["left","center","right"]
        self.halign=Pmw.OptionMenu(self.oproperties,
                                   items=haligns,
                                   labelpos='w',
                                   label_text='Halign:',
                                   label_bg=To_color,
                                   hull_bg=To_color,
                                   menu_bg=To_color,
                                   menu_activebackground=To_color,
                                   menubutton_bg=To_color,
                                   menubutton_activebackground=To_color,
                                   command=self.setfont,
                                   )
        self.halign.pack()
        labels.append(self.halign)
        
        valigns=["top","cap","half","base","bottom"]
        self.valign=Pmw.OptionMenu(self.oproperties,
                                   items=valigns,
                                   labelpos='w',
                                   label_text='Valign:',
                                   label_bg=To_color,
                                   hull_bg=To_color,
                                   menu_bg=To_color,
                                   menu_activebackground=To_color,
                                   menubutton_bg=To_color,
                                   menubutton_activebackground=To_color,
                                   command=self.setfont,
                                   )
        self.valign.pack()
        labels.append(self.valign)

        Pmw.alignlabels(labels)
        
        self.fbuttons=Tkinter.Frame(parent)
        self.fbuttons.pack()
##         b0=Tkinter.Button(self.fbuttons,
##                           text='Preview',
##                           command=self.setfont)
##         b0.pack(side='left')
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
        self.setfont()
        self.dialog.destroy()

    def setgui(self):
        ## First sets everuthing to "normal" so we can update the gui!
        self.font.configure(menubutton_state='normal')
        self.spacing.configure(state='normal')
        self.expansion.configure(state='normal')
        self.Color.configure(state='normal')
        self.height.configure(entry_state='normal')
        self.angle.configure(state='normal')
        self.path.configure(menubutton_state='normal')
        self.halign.configure(menubutton_state='normal')
        self.valign.configure(menubutton_state='normal')

        self.font.setvalue(self.canvas.getfont(self.text.font))
        self.spacing.set(self.text.spacing)
        self.expansion.set(self.text.expansion)
        self.Color.set(self.text.color)
        self.height.setentry(str(self.text.height))
        self.angle.set(self.text.angle)
        self.path.invoke(self.text.path)
        self.halign.invoke(self.text.halign)
        self.valign.invoke(self.text.valign)
        tables=vcs.listelements('texttable')
        tables.sort()
        self.table.setitems(tables)
        if self.text.Tt_name=='default':
            self.font.configure(menubutton_state='disabled')
            self.spacing.configure(state='disabled')
            self.expansion.configure(state='disabled')
            self.Color.configure(state='disabled')

        if self.text.To_name in ['default','defcentup','defcentdown','defright','defcenter']:
            self.height.configure(entry_state='disabled')
            self.angle.configure(state='disabled')
            self.path.configure(menubutton_state='disabled')
            self.halign.configure(menubutton_state='disabled')
            self.valign.configure(menubutton_state='disabled')
            
        self.parent.properties_font.component('scrolledlist').setlist(tables)
        self.parent.properties_font.selectitem(self.text.Tt_name)
        self.table.setvalue(self.text.Tt_name)
        try:
            attribute = '%s.texttable'%self.parent_Name
            self.parent.set_value(attribute,self.text.Tt_name)
        except:
##             self.parent.dialog.invoke('Apply')
            pass
        orientations=vcs.listelements('textorientation')
##         self.parent.properties_orientation.configure(scrolledlist_items=tables)
        self.parent.properties_orientation.component('scrolledlist').setlist(orientations)
        self.parent.properties_orientation.selectitem(self.text.To_name)
        orientations.sort()
        self.orientation.setitems(orientations)
        self.orientation.setvalue(self.text.To_name)
        try:
            attribute = '%s.textorientation'%self.parent_Name
            self.parent.set_value(attribute,self.text.To_name)
        except:
            self.parent.dialog.invoke('Apply')
            pass
        self.setfont()
        return
    # Open VCS file
    #
    def evt_open_file( self,master=None ):
        # search for the following files
        filetypes = [
          ("VCS script files", "*.scr"),
#        ("Python files", "*.py *.pyw" ),
          ("All files", "*"),
          ]

        # Show the popup directory dialog
        dialog = tkFileDialog.Open(master=master,
                             filetypes=filetypes,
                             title = 'VCS TextObject File Select')

        dirfilename=dialog.show(initialdir=os.getcwd())

        # Run VCS script
        if len(dirfilename) > 0:
            self.canvas.scriptrun(dirfilename)

    # Save VCS projection as a new name
    #
    def evt_save_table_as( self, master=None):
##    def __init__(self, eself, parent, vcs, type, title, text, entry_str = ''):
##       create_entry_popup(eself, parent, vcs, 'sa',('Save VCS colormap ( %s ) as:' % vcs.getcolormapname()),'Enter the new name of the colormap:\n\tFor example:\n\t\tnewcolormapname')
        self.savedialog = Pmw.Dialog( master=master,
            title = 'Save VCS Table ( %s ) as:' % self.text.Tt_name,
            buttons = ('OK', 'Dismiss'),
            defaultbutton = 'OK',
            command = self.rename_table )

        lbl=Tkinter.Label(self.savedialog.interior(),
            text = 'Enter the new name of the text table:\n\tFor example:\n\t\tnewtexttablename',
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
        self.savedialog.activate(geometry="+%d+%d" % (d1, d2) )

    # Save VCS projection as a new name
    #
    def evt_save_orientation_as( self, master=None):
##    def __init__(self, eself, parent, vcs, type, title, text, entry_str = ''):
##       create_entry_popup(eself, parent, vcs, 'sa',('Save VCS colormap ( %s ) as:' % vcs.getcolormapname()),'Enter the new name of the colormap:\n\tFor example:\n\t\tnewcolormapname')
        self.savedialog = Pmw.Dialog( master=master,
            title = 'Save VCS TextOrientation ( %s ) as:' % self.text.To_name,
            buttons = ('OK', 'Dismiss'),
            defaultbutton = 'OK',
            command = self.rename_orientation )

        lbl=Tkinter.Label(self.savedialog.interior(),
            text = 'Enter the new name of the text orientation:\n\tFor example:\n\t\tnewname',
            justify = 'left',
            anchor = 'w',
            )
        lbl.pack( expand = 1, fill = 'both', padx=10, pady=5 )

        self.eny2=Pmw.EntryField(self.savedialog.interior(),
            labelpos = 'w',
            entry_background = 'white',
            entry_foreground = 'black',
            value = '',
            entry_width =  50,
            )
        self.eny2.pack( expand = 1, fill = 'both', padx=10, pady=5 )

        # Position dialog popup
        parent_geom = self.dialog.geometry()
        geom = string.split(parent_geom, '+')
        d1 = string.atoi( geom[1] )
        d2 = string.atoi( geom[2] )
        self.savedialog.activate(geometry= "+%d+%d" % (d1, d2) )

    def evt_save_as( self, master=None):
##    def __init__(self, eself, parent, vcs, type, title, text, entry_str = ''):
##       create_entry_popup(eself, parent, vcs, 'sa',('Save VCS colormap ( %s ) as:' % vcs.getcolormapname()),'Enter the new name of the colormap:\n\tFor example:\n\t\tnewcolormapname')
        self.savedialog = Pmw.Dialog( master=master,
            title = 'Save VCS TextObject ( %s ) as:' % self.text.Tt_name,
            buttons = ('OK', 'Dismiss'),
            defaultbutton = 'OK',
            command = self.rename_text )

        lbl=Tkinter.Label(self.savedialog.interior(),
            text = 'Enter the new name of the text object:\n\tFor example:\n\t\tnewname',
            justify = 'left',
            anchor = 'w',
            )
        lbl.pack( expand = 1, fill = 'both', padx=10, pady=5 )

        self.eny3=Pmw.EntryField(self.savedialog.interior(),
            labelpos = 'w',
            entry_background = 'white',
            entry_foreground = 'black',
            value = '',
            entry_width =  50,
            )
        self.eny3.pack( expand = 1, fill = 'both', padx=10, pady=5 )

        # Position dialog popup
        parent_geom = self.dialog.geometry()
        geom = string.split(parent_geom, '+')
        d1 = string.atoi( geom[1] )
        d2 = string.atoi( geom[2] )
        self.savedialog.activate(geometry= "+%d+%d" % (d1, d2) )

    # Save, copy, and write colormap to a file
    #
    def rename_table(self,result):
##         print 'Result is:',result
        if result=='OK':
            newname=self.eny1.get( )
            text=self.canvas.createtexttable(newname,self.text.Tt_name)
##             print 'Should be changing the dialog now !',self.projection.name
            self.text=self.canvas.gettext(newname,self.text.To_name)
            self.save_vals()
            title='Text Editor - table: '+self.text.Tt_name+', orientation: '+self.text.To_name
            self.dialog.title(title)
            self.setgui()
        self.savedialog.destroy()
        
    def rename_orientation(self,result):
##         print 'Result is:',result
        if result=='OK':
            newname=self.eny2.get( )
            text=self.canvas.createtextorientation(newname,self.text.To_name)
##             print 'Should be changing the dialog now !',self.projection.name
            self.text=self.canvas.gettext(self.text.Tt_name,newname)
            self.save_vals()
            self.dialog.title('Orientation Editor - '+self.text.Tt_name)
            self.setgui()
        self.savedialog.destroy()
    def rename_text(self,result):
##         print 'Result is:',result
        if result=='OK':
            newname=self.eny3.get( )
            text=self.canvas.createtexttable(newname,self.text.Tt_name)
            text=self.canvas.createtextorientation(newname,self.text.To_name)
##             print 'Should be changing the dialog now !',self.projection.name
            self.text=self.canvas.gettext(newname,newname)
            self.save_vals()
            title='Text Editor - table: '+self.text.Tt_name+', orientation: '+self.text.To_name
            self.dialog.title(title)
            self.setgui()
        self.savedialog.destroy()
    # Save VCS Color map to a file
    #
    def evt_save_to_file( self, master=None):
        filetypes = [ ("VCS Script File", ".scr") ]
        # Show the popup directory dialog
        sfile = tkFileDialog.asksaveasfilename( master=master, filetypes = filetypes,
                             title = 'Save VCS TextObject to a File' )
        if sfile == '': return
        if sfile[-4:] != '.scr': sfile += '.scr'

        self.text.script(sfile)

    def loadtable(self,result):
        a=self.table.getvalue()
        self.text=self.canvas.gettext(a,self.text.To_name)
        self.save_vals()
        title='Text Editor - table: '+self.text.Tt_name+', orientation: '+self.text.To_name
        self.dialog.title(title)
        self.setgui()
        
    def loadorientation(self,result):
        a=self.orientation.getvalue()
        self.text=self.canvas.gettext(self.text.Tt_name,a)
        self.save_vals()
        title='Text Editor - table: '+self.text.Tt_name+', orientation: '+self.text.To_name
        self.dialog.title(title)
        self.setgui()        

    def setfont(self,*crap):
        if self.text.Tt_name!='default':
            self.text.font=self.canvas.getfont(self.font.getvalue())
            self.text.spacing=self.spacing.get()
            self.text.expansion=self.expansion.get()
            self.text.color=self.Color.get()
        if not self.text.To_name in  ['default','defcentup','defcentdown','defright','defcenter']:
            self.text.height=float(self.height.get())
            self.text.angle=self.angle.get()
            self.text.path=self.path.getvalue()
            self.text.halign=self.halign.getvalue()
            self.text.valign=self.valign.getvalue()
        self.canvas.update()
        

    def cancel(self):
        self.reset()
        self.dialog.destroy()

    def reset(self):
        if self.orig!=[]:
            try:
                for orig in self.orig[::-1]:
                    tmpo=self.canvas.gettextorientation(orig['To_name'])
                    if not tmpo.name in  ['default','defcentup','defcentdown','defright','defcenter']:
                        for a in ['height','angle','path','halign','valign']:
                            setattr(tmpo,a,orig[a])
                    tmpt=self.canvas.gettexttable(orig['Tt_name'])
                    if tmpt.name!='default':
                        for a in ['spacing','expansion','color','font']:
                            setattr(tmpt,a,orig[a])
                self.text=self.canvas.gettext(tmpt.name,tmpo.name)
                self.setgui()
                self.orig=[]
                self.save_vals()
            except Exception,err:
                print 'Error:',err,'--------------------------------------'

if __name__=='__main__':
    import vcs,cdms2 as cdms
    x=vcs.init()
    tt=x.gettext('std','7left')
    tt.string=['Hello']
    tt.x=[.5]
    tt.y=[.5]
    f=cdms.open('/pcmdi/obs/mo/tas/rnl_ncep/tas.rnl_ncep.ctl')
    s=f('tas',time=slice(0,1))
    x.plot(s)
    gui=create(canvas=x,table='std',orientation='7left')
    gui.gui_parent.mainloop()
    
