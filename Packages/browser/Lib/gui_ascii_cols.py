import string
import tkFileDialog
import Pmw
import os
import gui_control
import Tkinter
import sys
import gui_user_menus
import gui_busy
import gui_message
import genutil
import vcs

read=genutil.ASCII.read_col

class read_columns_popup:
    def evt_icon_open_file(self, parent, dirfilename=None, event=None):
        datatypes = [
            ("Search for Text files", "*.asc *.ascii *.txt *.text"),
            ("All files", "*")
            ]
        
        dialog_icon = tkFileDialog.Open(master=parent,
                                        filetypes=datatypes, title = 'File Select')
        dirfilename=dialog_icon.show(initialdir=os.getcwd())
        self.file.setentry(dirfilename)
        
    def __init__(self, Self,parent):
        self.parent = parent
        self.Self=Self
        self.dialog = Pmw.Dialog( parent,
                                  title = "Reading an ASCII File",
                                  buttons = ('OK', 'Dismiss'),
                                  defaultbutton = 'OK',
                                  command = gui_control.Command(self.execute, parent) )
        
        if parent.menu.popup_window_settings_flg == 1:
            self.dialog.transient( self.parent ) # Keep widget on top of its parent
            
        d=self.dialog.interior()
        frame=Tkinter.Frame(d)
        frame.pack(expand=1,fill='x',padx=5,pady=20)
        file=Tkinter.Label(frame,text='File:')
        file.pack(side='left')
        self.canvas_openicon = Tkinter.Canvas(frame, bd=0, highlightthickness=0,
                                              width = 27, height = 27)
        self.canvas_openicon.pack( side='left', fill=Tkinter.BOTH, expand='no', padx=5 )
        parent.balloon.bind( self.canvas_openicon, 
                 "Display 'File Select' browser for 'Directory' and 'File' selection." )
        self.img = Tkinter.PhotoImage( file = os.path.join(vcs.__path__[0],'..','..','..','..', 'bin', 'open.gif') )
        self.canvas_openicon.create_image(0,0, anchor=Tkinter.NW, image=self.img )
        self.canvas_openicon.bind( '<1>', gui_control.Command( self.evt_icon_open_file, parent, None ))
        self.file=Pmw.EntryField(frame,labelpos='w',
                                 label_text='',
                                 entry_background = 'white',
                                 entry_foreground = 'black',
                                 entry_width =  20,
                                 )
        self.file.pack( side='left',expand = 1, fill = 'x', padx=20, pady=5 )
        self.parent.balloon.bind( self.file, "Browse to get the file to read" )
        self.header=Pmw.EntryField(d,labelpos='w',
                                   label_text='Header:',
                                   entry_background = 'white',
                                   entry_foreground = 'black',
                                   entry_width =  20,
                                   )
        self.header.pack( expand = 1, fill = 'x', padx=20, pady=5 )
        self.parent.balloon.bind( self.header, "The number of lignes to skip at the begining of the files" )
        frame2=Tkinter.Frame(d)
##         frame.pack(expand=1,fill='x',padx=5,pady=20)
        frame2.pack()
        self.cskip=Pmw.Counter(frame2,
                               labelpos='w',
                               label_text='On each line skip:',
                               datatype='integer',
                               orient='horizontal',
                               entry_background='white',
                               entry_foreground = 'black',
                                   )
        self.cskip.pack( side='left')

        self.parent.balloon.bind( self.cskip, "The number of lignes to skip at the begining of the files" )

        self.cskip_type=Pmw.RadioSelect(frame2,
                                        buttontype='radiobutton',
                                        orient='horizontal',
                                        )
        self.cskip_type.pack(side='left')
        self.cskip_type.add('columns')
        self.cskip_type.add('characters')
        self.cskip_type.invoke('columns')
        
        self.axis=Pmw.RadioSelect(d,
                                   labelpos='w',
                                   label_text='First Column is X-Axis',
                                   buttontype='radiobutton',
                                   )
        self.axis.pack(side='left',expand=1,fill='x',padx=20,pady=5)
        self.axis.add('Yes')
        self.axis.add('No')
        self.axis.invoke('No')
        self.parent.balloon.bind( self.axis, "Check if the first column represent axis values\nNot necessarily ordered, but will be reordered automatically\nIf ids filled, then first id is axis name" )
        frame3=Tkinter.Frame(d)
##         frame.pack(expand=1,fill='x',padx=5,pady=20)
        frame3.pack(fill='x',expand=1)
        self.id=Pmw.EntryField(frame3,labelpos='w',
                               label_text='Id:',
                               entry_background = 'white',
                               entry_foreground = 'black',
                               entry_width =  20,
                               )
        self.id.pack( side='left', fill='x',padx=20,pady=5)
        self.parent.balloon.bind( self.id, "The variables id(s) (space or comma separated)" )
        self.idrow=Pmw.RadioSelect(frame3,
                                   labelpos='w',
                                   label_text='Ids in file: ',
                                   buttontype='checkbutton',
                                   )
        self.idrow.pack(side='left')
        self.idrow.add('')
        self.parent.balloon.bind( self.idrow, "Check if the variables ids are stored in first row of data" )
        self.separators=Pmw.EntryField(d,labelpos='w',
                                       label_text='Separators:',
                                       entry_background = 'white',
                                       entry_foreground = 'black',
                                       entry_width =  20,
                                       )
        self.separators.pack( expand = 1, fill = 'x', padx=20, pady=5 )
        self.separators.setentry("; , :")
        self.parent.balloon.bind( self.separators, "The numbers separators\nOther than (space), \n (return) and \t (tab)\nSpace separated" )
        entries = ( frame3,self.axis,self.separators, self.separators, self.header,self.axis)
        Pmw.alignlabels(entries)
        
    def execute(self, parent,button):
        if button=='OK':
##             gui_busy.busyStart( self, parent )
##             try:
                
                ## File selection
                file=self.file.get()
                while file=="":
                    file=self.evt_icon_open_file(self.parent,None)
                    file=self.file.get()

                ## header skipping ?
                h=self.header.get()
                if h=='':
                    h=0
                else:
                    h=eval(h)

                ## Characters skip
                cskip=self.cskip.getvalue()
                if cskip=="":
                    cskip=0
                else:
                    cskip=string.atoi(cskip)

                ## Character skip type
                cskip_type=self.cskip_type.getvalue()

                ## values separation
                sep=self.separators.get()
                if sep=="":
                    sep=[]
                else:
                    sep=sep.split()

                ## ids
                id=self.id.get()
                if id=="":
                    id=None
                else:
                    id=id.split(',')
                    id=string.join(id)
                    id=id.split()
                ## stored in first row ?
                idrow=self.idrow.getvalue()
                if len(idrow)>0:
                    idrow=True
                else:
                    idrow=False
                    
                ## Axis values passed ?
                axis=self.axis.getvalue()
                if axis=='Yes':
                    axis=True
                else:
                    axis=False
                
                vars=read( file ,header=h, cskip=cskip, cskip_type=cskip_type, axis=axis, ids=id, idrow=idrow, separators=sep)
                gui_control.record_command( parent, "\n# PCMDI's read ASCII Tool\nimport genutil\n", 1 )
                ids=''
                ids_lst=''
                if isinstance(vars,list):
                    for v in vars:
                        v.id = self.Self.return_unique_name( v.id)
                        ids+=v.id+', '
                        ids_lst+=repr(v.id)+', '
                        gui_user_menus.user_menus_put( v.id, v )
                else:
                    v=vars
                    v.id = self.Self.return_unique_name( v.id)
                    ids+=v.id+', '
                    ids_lst+=repr(v.id)+', '
                    gui_user_menus.user_menus_put( v.id, v )

                gui_control.record_command( parent, "%s = genutil.ASCII.read_col(%s, header=%s, cskip=%s, cskip_type=%s, axis=%s, ids=%s, idrow=%s, separators=%s)" % (ids[:-2], repr(file), repr(h), repr(cskip), repr(cskip_type), str(axis), '[ '+ids_lst[:-2]+' ]', repr(idrow), repr(sep)), 1 )
##                 gui_busy.busyEnd( self, parent )
##             except Exception, err: 
##                 gui_busy.busyEnd( self, parent )
##                 gui_message.error( "The 'ASCII Read  Tool' could not complete its function\nError:\n"+str(err))

        self.dialog.destroy()
