# Adapted for numpy/ma/cdms2 by convertcdms.py
import string
import MV2 as MV
import tkFileDialog
import Pmw
import os
import gui_control
import Tkinter
import sys
import gui_user_menus
import gui_busy
import gui_message
import genutil,vcs

read = genutil.ASCII.readAscii

class read_popup:
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
        frame.pack()
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
        self.separators=Pmw.EntryField(d,labelpos='w',
                                       label_text='Separators:',
                                       entry_background = 'white',
                                       entry_foreground = 'black',
                                       entry_width =  20,
                                       )
        self.separators.pack( expand = 1, fill = 'x', padx=20, pady=5 )
        self.separators.setentry("; , :")
        self.parent.balloon.bind( self.separators, "The numbers separators\nOther than (space), \n (return) and \t (tab)\nSpace separated" )
        self.id=Pmw.EntryField(d,labelpos='w',
                               label_text='Id:',
                               entry_background = 'white',
                               entry_foreground = 'black',
                               entry_width =  20,
                               )
        self.id.pack( expand = 1, fill = 'x', padx=20, pady=5 )
        self.parent.balloon.bind( self.id, "The variables id(s) (space or comma separated)" )
        self.shape=Pmw.EntryField(d,labelpos='w',
                                  label_text='Shape:',
                                  entry_background = 'white',
                                  entry_foreground = 'black',
                                  entry_width =  20,
                                  )
        self.shape.pack( expand = 1, fill = 'x', padx=20, pady=5 )
        self.parent.balloon.bind( self.shape, "The variables shape\nDimensions are spaces or comma separated)" )
        self.vs=Pmw.EntryField(d,labelpos='w',
                               label_text='Variables separators:',
                               entry_background = 'white',
                               entry_foreground = 'black',
                               entry_width =  20,
                               )
        self.vs.pack( expand = 1, fill = 'x', padx=20, pady=5 )
        self.parent.balloon.bind( self.vs, "The string sequence that separates variables" )
        self.vs.setentry('------')
        entries = ( self.id, self.separators, self.shape, self.vs, self.header)
        Pmw.alignlabels(entries)
        
    def execute(self, parent,button):
        if button=='OK':
##             gui_busy.busyStart( self, parent )
##             try:
                file=self.file.get()
                while file=="":
                    file=self.evt_icon_open_file(self.parent,None)
                    file=self.file.get()
                h=self.header.get()
                if h=='':
                    h=0
                else:
                    h=eval(h)
                sep=self.separators.get()
                if sep=="":
                    sep=[]
                else:
                    sep=sep.split()
                id=self.id.get()
                if id=="":
                    id=None
                else:
                    id=id.replace("'","")
                    id=id.replace('"',"")
                    id=id.split(',')
                    id=' '.join(id)
                    id=id.split()
                sh=self.shape.get()
                print sh
                if sh=="":
                    sh=None
                else:
                    sh=sh.split(',')
                    sh=' '.join(sh)
                    sh=sh.split('(')
                    sh=' '.join(sh)
                    sh2=sh.split(')')
                    sh=[]
                    for s in sh2:
                        if s.strip!='':
                            tmp = eval('(%s)' % ','.join(s.split()))
                            if not isinstance(tmp,tuple):
                                tmp=(tmp,)
                            sh.append(tmp)
                    print 'out:',sh
                vs=self.vs.get()
                if vs=="":
                    vs=None
                vars=read( file ,header=h, ids=id, shape=sh, next=vs,separators=sep)
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

                gui_control.record_command( parent, "%s = genutil.ASCII.readAscii(%s, header=%s, ids= %s, shape=%s, separators=%s, next=%s)" % (ids[:-2], repr(file), repr(h), '[ '+ids_lst[:-2]+' ]', repr(sh), repr(sep),repr(vs)), 1 )
##                 gui_busy.busyEnd( self, parent )
##             except Exception, err: 
##                 gui_busy.busyEnd( self, parent )
##                 gui_message.error( "The 'ASCII Read  Tool' could not complete its function\nError:\n"+str(err))

        self.dialog.destroy()
