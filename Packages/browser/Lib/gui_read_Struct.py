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
import struct
import cdms2


def make_var(lap,id=None,shape=None):
    lap=MV.array(lap)
    if shape is not None:
        lap=MV.reshape(lap,shape)
    if id is not None:
        lap.id=id
    return lap

def read( file ,format="", endian='@', datatype='f', ids=[], shape=[], separator=""):
    
    f=open(file)
    s=f.read()
    f.close()

    ## first if format has not been entered!
    if format=="":
        if shape==[]:
            format=str(len(s)/struct.calcsize(datatype))+datatype
            s=s[:struct.calcsize(format)]
    if format=="":
        if len(ids)>1 and len(shape)!=len(ids):
            if len(shape)==1:
                shape=shape*len(ids)
            else:
                raise "Error shapes and ids are not compatible"
        format=""
        for sh in shape:
            n=1
            for S in sh: n=n*S
            if format!="":
                format=format+separator+str(n)+datatype
            else:
                format=format+str(n)+datatype
    
    ## Now test on format
    fsz=struct.calcsize(format)

    if len(s)!=fsz:
        ## pbm with sizes, not good....
        if len(ids)>1 and len(shape)==1: ## Ok 1 shape multiple vars!
            shape=shape*len(ids)
            format2=''
            for i in range(len(ids)):
                if format2!="":
                    format2=format2+separator+format
                else:
                    format2+=format
            if len(s)==struct.calcsize(format2):
                format=format2
        elif len(shape)==1 and ids==[]: ## Ok let's try to repeat
            format2=''
            sz=0
            n=0
            while sz<len(s):
                n+=1
                sz=struct.calcsize(format2)
                if format2!='' :
                    format2=format2+separator+format
                else:
                    format2+=format
            if sz==len(s):
                format=format2
                shape=shape*n
        else: # everything failed if format smaller than s, thne let's hope it's garbage a the end
            if len(s)<fsz:
                s=s[:fsz]
            else:
                raise "Error coudln't read your data !"

    if not format[0] in ['@','=','<','>','!']:
        format=endian+format

        
    s=struct.unpack(format,s)
    vars=[]
    if shape!=[]:
        i=0
        for sh in shape:
            n=1
            for S in sh: n*=S
            v=MV.array(s[:n])
            v=MV.reshape(v,sh)
            if ids!=[]:
                v.id=ids[i]
            s=s[n:]
            vars.append(v)
            i+=1
    else:
        v=MV.array(s)
        if ids!=[]:
            v.id=ids[0]
        vars.append(v)

    if len(vars)>1:
        return vars
    else:
        return vars[0]


class read_bin_popup:
    def evt_icon_open_file(self, parent, dirfilename=None, event=None):
        datatypes = [
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
                                  title = "Reading a Binary File",
                                  buttons = ('OK', 'Dismiss'),
                                  defaultbutton = 'OK',
                                  command = gui_control.Command(self.execute, parent) )
        
        if parent.menu.popup_window_settings_flg == 1:
            self.dialog.transient( self.parent ) # Keep widget on top of its parent
            
        d=self.dialog.interior()
        frame=Tkinter.Frame(d)
        frame.pack(fill='x',pady=5)
        file=Tkinter.Label(frame,text='File:')
        file.pack(fill='x',pady=5,side='left')
        self.canvas_openicon = Tkinter.Canvas(frame, bd=0, highlightthickness=0,
                                              width = 27, height = 27)
        self.canvas_openicon.pack( side='left', fill='x', pady=5 )
        parent.balloon.bind( self.canvas_openicon, 
                 "Display 'File Select' browser for 'Directory' and 'File' selection." )
        self.img = Tkinter.PhotoImage( file = os.path.join(cdms2.__path__[0],'..','..','..','..', 'bin', 'open.gif') )
        self.canvas_openicon.create_image(0,0, anchor=Tkinter.NW, image=self.img )
        self.canvas_openicon.bind( '<1>', gui_control.Command( self.evt_icon_open_file, parent, None ))
        self.file=Pmw.EntryField(frame,labelpos='w',
                                 label_text='',
                                 entry_background = 'white',
                                 entry_foreground = 'black',
                                 entry_width =  20,
                                 )
        self.file.pack( side='left',fill = 'x', pady=5 )
        self.parent.balloon.bind( self.file, "Browse to get the file to read" )

        ## Big or Little Endian Button
        endian=['Native (@)','Native (=)','Little-Endian (<)', 'Big-Endian (>)',]
        self.endian=Pmw.OptionMenu(d,
                                   labelpos='w',
                                   label_text='Endian:',
                                   items=endian,
                                   initialitem='Native (@)',
                                   )
        self.endian.pack( pady=5,fill='x')
        self.parent.balloon.bind( self.endian, "The Endian in which the File as been written, Big or Little" )

        ## Variables format
        self.format=Pmw.EntryField(d,labelpos='w',
                               label_text='Format:',
                               entry_background = 'white',
                               entry_foreground = 'black',
                               entry_width =  20,
                               )
        self.format.pack( fill ='x', pady=5 )
        self.parent.balloon.bind( self.format, "The format of each variables" )
        self.format.setentry('')
        
        ## datatype
        datatypes=['char','signed char','unsigned char','short','unsigned short','int','unsigned int','long','unsigned long','float','double']
        self.datatype=Pmw.OptionMenu(d,
                                     labelpos='w',
                                     label_text='Datatype:',
                                     items=datatypes,
                                     initialitem='float',
                                     )

        self.datatype.pack(fill='x',pady=5)
        self.parent.balloon.bind( self.datatype, "Datatype in the file (only if all data are from same type)\nEntering anything in Format will override this" )

        ## Number of elements or shape
        self.shape=Pmw.EntryField(d,labelpos='w',
                                  label_text='Shape:',
                                  entry_background = 'white',
                                  entry_foreground = 'black',
                                  entry_width =  20,
                                  )
        self.shape.pack( fill = 'x', pady=5 )
        self.parent.balloon.bind( self.shape,
                                  "The variables shape or number of elements in the file\nDimensions are spaces or comma separated)\nMultiple shapes can be entered must be in format (n1,n2,n3..), (m1,m2,m3,...)\nIf only one shape and multiple ids, then it is assumed shape is identical for all variables\nIf Format is filled then shape(s) and Format must match, otherwise format will prevail and a 1D array will be returned",
                                  )
                
        ## Variable id once into the Defined variable
        self.id=Pmw.EntryField(d,labelpos='w',
                               label_text='Id:',
                               entry_background = 'white',
                               entry_foreground = 'black',
                               entry_width =  20,
                               )
        self.id.pack( fill = 'x', pady=5 )
        self.parent.balloon.bind( self.id, "The variables id(s) (space or comma separated)" )

        ## Variables separator
        self.vs=Pmw.EntryField(d,labelpos='w',
                               label_text='Variables separators:',
                               entry_background = 'white',
                               entry_foreground = 'black',
                               entry_width =  20,
                               )
        self.vs.pack( fill = 'x', pady=5 )
        self.parent.balloon.bind( self.vs, "The string sequence that separates variables\nWill be inserted between each variable format" )
        self.vs.setentry('')

        
        entries = ( self.endian, self.format, self.datatype, self.shape, self.id, self.vs)
        Pmw.alignlabels(entries)
        
    def execute(self, parent,button):
        if button=='OK':
            gui_busy.busyStart( self, parent )
            try:
                file=self.file.get()
                while file=="":
                    file=self.evt_icon_open_file(self.parent,None)
                    file=self.file.get()
                e=self.endian.index(self.endian.getvalue())
                endians=['@','=','<','>']
                e=endians[e]
                f=self.format.get()
                try:
                    struct.calcsize(f)
                except:
                    Pmw.Dialog(self.parent,
                               title = "Wrong Format !",
                               buttons = ('OK',),
                               )
                    gui_busy.busyEnd( self, parent )
                    return
                
                id=self.id.get()
                if id=="":
                    id=[]
                else:
                    id=id.split(',')
                    id=string.join(id)
                    id=id.split()
                sh=self.shape.get()
                if sh=="":
                    sh=[]
                else:
                    sh=sh.strip()
                    if sh[0]=='(':
                        sh=eval(sh)
                    else:
                        sh=sh.split(',')
                        sh=string.join(sh)
                        sh=sh.split('(')
                        sh=string.join(sh)
                        sh=sh.split(')')
                        sh=string.join(sh)
                        sh2=sh.split()
                        sh=[]
                        for s in sh2:
                            sh.append(eval(s))
                        sh=[list(sh),]
                        
                datatype=self.datatype.index(self.datatype.getvalue())
                dtp=['c','b','B','h','H','i','I','l','L','f','d']
                datatype=dtp[datatype]
                
                vs=self.vs.get()
                try:
                    struct.calcsize(vs)
                except:
                    Pmw.Dialog(self.parent,
                               title = "Wrong Format for Variable Separators !",
                               buttons = ('OK',),
                               )
                    gui_busy.busyEnd( self, parent )
                    return
                
                vars=read( file ,format=f, endian=e, datatype=datatype, ids=id, shape=sh,separator=vs)
                gui_control.record_command( parent, "\n# PCMDI's read Binary Tool\nimport browser", 1 )
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
                    v.id = self.Self.return_unique_name( vars.id)
                    ids+=v.id+', '
                    ids_lst+=repr(v.id)+', '
                    gui_user_menus.user_menus_put( v.id, v )

                gui_control.record_command( parent, "%s = browser.gui_read_Struct.read(%s, format=%s, endian=%s, datatype=%s, ids= %s, shape=%s, separator=%s)" % (ids[:-2], repr(file), repr(f), repr(e), repr(datatype), '[ '+ids_lst[:-2]+' ]', str(sh), repr(vs)), 1 )
                
                gui_busy.busyEnd( self, parent )
            except Exception, err: 
                gui_busy.busyEnd( self, parent )
                gui_message.error( "The 'Binary Read Tool' could not complete its function\nError:\n"+str(err))

        self.dialog.destroy()
