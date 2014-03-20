# Adapted for numpy/ma/cdms2 by convertcdms.py
## The line GUI editor

###############################################################################
#                                                                             #
#                                                                             #
# Copyright:    "See file Legal.htm for copyright information."               #
#                                                                             #
# Authors:      PCMDI Software Team                                           #
#               Lawrence Livermore NationalLaboratory:                        #
#               support@pcmdi.llnl.gov                                        #
#                                                                             #
# Description:  PCMDI's VCS GUI line editor.                                  #
#                                                                             #
#                                                                             #
###############################################################################

import vcs_legacy
import Tkinter,Pmw,tkFileDialog
import string,browser
import os
import gui_support
import copy

# Create/Popup projection GUI for VCS.
def create(gui_parent=None,line='default',canvas=None,parent=None):
    return LineGUI(canvas,gui_parent,line,parent)

class LineGUI:

    def save_vals(self):
        orig={}
        orig['type']=self.line.type[0]
        orig['width']=self.line.width[0]
        orig['color']=self.line.color[0]
        orig['name']=self.line.name
        self.orig.append(copy.copy(orig))
             
    def __init__(self, canvas=None, gui_parent=None, dialog_parent=None, master=None, name='default',Parent=None):
        self.gui_parent=gui_parent
        if canvas is None:
            self.canvas=vcs_legacy.init()
        else:
            self.canvas=canvas

        self.line=self.canvas.getline(name)
        if self.line is None:
            if not name in self.canvas.listelements('line'):
                raise 'Error '+t_name+' is not a valid line name'
            else:
                raise 'Weird unkwnown error but no line object returned....'
        self.parent=Parent
        self.parent_Name=self.parent.parent_Name
        ## Saves original values
        self.orig=[]
        self.save_vals()
        
        L_color = gui_support.gui_color.L_color
        
        if self.gui_parent is None:
            self.gui_parent=Tkinter.Toplevel()
            #self.gui_parent=Tkinter.Tk(":0.0") # Use the localhost:0.0 for the DISPLAY and screen
            self.gui_parent.withdraw()
        title='Line Editor - table: '+self.line.name
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
        parent.configure(bg=L_color)
        self.cmain_menu = Pmw.MenuBar(parent,
                hull_relief = 'raised',
                hull_borderwidth = 2,
                balloon = gui_support.balloon
                )
        self.cmain_menu.pack(side='top', fill='both')

        self.cmain_menu.addmenu('File', 'Open/Save VCS Line Objects', tearoff = 1)
        self.cmain_menu.addmenuitem('File', 'command', 'Open line object file',
                         label = 'Open LineObject File',
                         command = self.evt_open_file,
                        )

        self.cmain_menu.addmenuitem('File', 'separator')

        #
        # Create the cascade "Save Colormap" menu and its items
        self.cmain_menu.addmenuitem('File', 'command', 'Save Line',
                                    label = 'Save (i.e Apply changes)',
                                    command = self.setline,
                                    )
        self.cmain_menu.addmenuitem('File', 'command', 'Copy Line',
                                    label = 'Copy Line',
                                    command = self.evt_save_line_as,
                                    )
        self.cmain_menu.addmenuitem('File', 'command', 'Save to file',
                                    label = 'Save To File',
                                    command = self.evt_save_to_file,
                                    )

        # Create the cascade "Exit" menu
        self.cmain_menu.addmenuitem('File', 'separator')
        self.cmain_menu.addmenuitem('File', 'command',
                                    statusHelp='Close Line Editor',
                                    label = "Exit Line Editor",
                                    command = self.dialog.destroy,
                                    )
        
##         self.ftype=Tkinter.Frame(parent)
##         self.ftype.pack(side='top')
        self.tprop=Pmw.Group(parent,
                             tag_text='Line',
                             tag_bg=L_color,
                             tagindent=10,
                             hull_bg=L_color,
                             hull_highlightbackground=L_color,
                             hull_highlightcolor=L_color,
                            ring_bg=L_color,
                             )
        self.tprop.pack(expand='yes',fill='both')
        self.tproperties=self.tprop.interior()
        self.tproperties.configure(bg=L_color)
        lines=self.canvas.listelements('line')
        lines.sort()

        labels=[]
        self.Line=Pmw.OptionMenu(self.tproperties,
                                  items=lines,
                                  labelpos='w',
                                  label_text='Name:',
                                  command=self.loadline,
                                  initialitem=self.line.name,
                                  label_bg=L_color,
                                  hull_bg=L_color,
                                  menu_bg=L_color,
                                  menu_activebackground=L_color,
                                  menubutton_bg=L_color,
                                  menubutton_activebackground=L_color,
                                 )
        self.Line.pack()
        labels.append(self.Line)
        items=["solid", "dash", "dot", "dash-dot", "long-dash"]
        self.type=Pmw.OptionMenu(self.tproperties,
                                 items=items,
                                 labelpos='w',
                                 label_text='Type:',
                                 label_bg=L_color,
                                 hull_bg=L_color,
                                 menu_bg=L_color,
                                 menu_activebackground=L_color,
                                 menubutton_bg=L_color,
                                 menubutton_activebackground=L_color,
                                 command=self.setline,
                                 )
        self.type.pack()
        labels.append(self.type)
        f=Tkinter.Frame(self.tproperties,bg=L_color)
        l=Tkinter.Label(f,
                        text='Width:',
                        bg=L_color,
                        )
        l.pack(side='left')
        self.width=Tkinter.Scale(f,
                                 bigincrement=10,
                                 from_=1,
                                 to=300,
                                 orient='horizontal',
                                 tickinterval=50,
                                 length=200,
                                 bg=L_color,
                                 activebackground=L_color,
                                 highlightbackground=L_color,
                                 command=self.setline,
                                 )
        self.width.pack()
        labels.append(l)
        f.pack()

        f=Tkinter.Frame(self.tproperties,bg=L_color)
        
        l=Tkinter.Label(f,
                        text='Color:',
                        bg=L_color,
                        )
        
        l.pack(side='left')
        
        self.Color=Tkinter.Scale(f,
                                 bigincrement=50,
                                 from_=0,
                                 to=255,
                                 orient='horizontal',
                                 tickinterval=50,
                                 length=200,
                                 bg=L_color,
                                 activebackground=L_color,
                                 highlightbackground=L_color,
                                 command=self.setline,
                                 )
        
        self.Color.pack()
        labels.append(l)
        f.pack()

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
        self.setline()
        self.dialog.destroy()

    def setgui(self):
        ## First sets everuthing to "normal" so we can update the gui!
        self.Line.configure(menubutton_state='normal')
        self.type.configure(menubutton_state='normal')
        self.width.configure(state='normal')
        self.Color.configure(state='normal')

        self.type.setvalue(self.line.type[0])
        self.width.set(self.line.width[0])
        self.Color.set(self.line.color[0])
        lines=self.canvas.listelements('line')
        lines.sort()
        self.Line.setitems(lines)
        if self.line.name=='default':
            self.type.configure(menubutton_state='disabled')
            self.width.configure(state='disabled')
            self.Color.configure(state='disabled')
            
        self.parent.properties_line.component('scrolledlist').setlist(lines)
        self.parent.properties_line.selectitem(self.line.name)
        self.Line.setvalue(self.line.name)
        attribute = '%s.line'%self.parent_Name
        self.parent.set_value(attribute,self.line.name)

        self.setline()
        return
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
                             title = 'VCS LineObject File Select')

        dirfilename=dialog.show(initialdir=os.getcwd())

        # Run VCS script
        if len(dirfilename) > 0:
            self.canvas.scriptrun(dirfilename)

    # Save VCS projection as a new name
    #
    def evt_save_line_as( self):
##    def __init__(self, eself, parent, vcs_legacy, type, title, text, entry_str = ''):
##       create_entry_popup(eself, parent, vcs_legacy, 'sa',('Save VCS colormap ( %s ) as:' % vcs_legacy.getcolormapname()),'Enter the new name of the colormap:\n\tFor example:\n\t\tnewcolormapname')
        self.savedialog = Pmw.Dialog( 
            title = 'Save VCS Line ( %s ) as:' % self.line.name,
            buttons = ('OK', 'Dismiss'),
            defaultbutton = 'OK',
            command = self.rename_line )

        lbl=Tkinter.Label(self.savedialog.interior(),
            text = 'Enter the new name of the line:\n\tFor example:\n\t\tnewlinename',
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

    def evt_save_as( self):
        self.savedialog = Pmw.Dialog( self.gui_parent,
            title = 'Save VCS Line ( %s ) as:' % self.line.name,
            buttons = ('OK', 'Dismiss'),
            defaultbutton = 'OK',
            command = self.rename_line )

        lbl=Tkinter.Label(self.savedialog.interior(),
            text = 'Enter the new name of the line object:\n\tFor example:\n\t\tnewname',
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
    def rename_line(self,result):
##         print 'Result is:',result
        if result=='OK':
            newname=self.eny1.get( )
            line=self.canvas.createline(newname,self.line.name)
##             print 'Should be changing the dialog now !',self.projection.name
            self.line=self.canvas.getline(newname)
            title='Line Editor - line: '+self.line.name
            self.dialog.title(title)
            self.setgui()
        self.savedialog.destroy()
        
    # Save VCS Color map to a file
    #
    def evt_save_to_file( self):
        filetypes = [ ("VCS Script File", ".scr") ]
        # Show the popup directory dialog
        sfile = tkFileDialog.asksaveasfilename( master=self.gui_parent, filetypes = filetypes,
                             title = 'Save VCS LineObject to a File' )
        if sfile == '': return
        if sfile[-4:] != '.scr': sfile += '.scr'

        self.line.script(sfile)

    def loadline(self,result):
        a=self.Line.getvalue()
        self.line=self.canvas.getline(a)
        self.save_vals()
        title='Line Editor - line: '+self.line.name
        self.dialog.title(title)
        self.setgui()
        
    def setline(self,*crap):
        if self.line.name!='default':
            self.line.type=self.type.getvalue()
            self.line.width=self.width.get()
            self.line.color=self.Color.get()
        self.canvas.update()
        

    def cancel(self):
        self.reset()
        self.dialog.destroy()

    def reset(self):
        if self.orig!=[]:
            try:
                for orig in self.orig[::-1]:
                    tmp=self.canvas.getline(orig['name'])
                    if not tmp.name in  ['default','defcentup','defcentdown','defright','defcenter']:
                        for a in ['width','type','color']:
                            setattr(tmp,a,orig[a])
                self.line=self.canvas.getline(tmp.name)
                self.setgui()
                self.orig=[]
                self.save_vals()
            except Exception,err:
                print 'Error:',err,'--------------------------------------'

if __name__=='__main__':
    import vcs_legacy,cdms2 as cdms
    x=vcs_legacy.init()
    tt=x.getline('cyan')
    tt.x=[.5,.8]
    tt.y=[.5,.8]
    x.plot(tt)
    gui=create(canvas=x,line='cyan')
    gui.gui_parent.mainloop()
    
