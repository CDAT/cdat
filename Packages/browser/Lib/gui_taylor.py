#!/usr/bin/env python
# Adapted for numpy/ma/cdms2 by convertcdms.py
#
# The PCMDI Data Browser Taylor Diagram Dialog -  gui_taylor module
#
###############################################################################
#                                                                             #
# Module:       gui_taylor module                                             #
#                                                                             #
# Copyright:    "See file Legal.htm for copyright information."               #
#                                                                             #
# Authors:      PCMDI Software Team                                           #
#               Lawrence Livermore NationalLaboratory:                        #
#               support@pcmdi.llnl.gov                                        #
#                                                                             #
# Description:  Tkinter GUI interface to the Taylor Diagram graphics method.  #
#                                                                             #
# Version:      3.0                                                           #
###############################################################################
#
#---------------------------------------------------------------------
# NOTE: need to use version of Python that imports Tkinter and Pmw
#---------------------------------------------------------------------

#from tdiagram import TaylorDiagram
from vcs import colors
import vcs
import Tkinter,Pmw,string,numpy

class TDGui:
    
    def __init__(self,root=None,canvas=None,grname='ASD',data=None):
        if canvas is None:
            canvas=vcs.init()
        self.x=canvas
        try:
            self.GM=self.x.createtaylordiagram(grname)
        except:
            self.GM=self.x.gettaylordiagram(grname)

        self.root=root
        self.guiVar=[]
        self.usedgui=0
        self.guiMarkers=[]
        self.grname=grname
        self.data=data
        self.gui_start()
        

        
    def gui_start(self):
        if self.root is None:
            self.root=Tkinter.Tk(":0.0") # Use the localhost:0.0 for the DISPLAY and screen
        try:
            self.root.title('Taylor Diagrams Control Center')
        except:
            pass
        self.notebook=Pmw.NoteBook(self.root, hull_height=900)
        self.notebook.pack(fill = 'both', expand = 1)
        self.notebook.add('Interface')
        self.notebook.add('Markers')
        self.notebook.add('Ticks and Labels')
        self.MainFrame=Tkinter.Frame(self.root)
        self.MainFrame.pack(side='top',fill = 'both', expand = 1)
        self.gui_generateMarkersGui()
        self.gui_generateInterfaceGui()
        self.gui_generateLabelsGui()
##         self.dialog = tkFileDialog.Open(master=self.root)
        
    def gui_copyMarkers(self):
        self.GM.Marker.equalize()
        nins=0
        self.GM.Marker.number
        for i in range(self.GM.Marker.number):
            isins = self.guiVar[i+nins][0].get()
            if isins:
                self.guiVar.insert(i+nins,[Tkinter.IntVar(),Tkinter.IntVar(),Tkinter.StringVar()])
                self.GM.Marker.insert(i+nins)
                self.guiMarkers.insert(i+nins,self.gui_addMarkerInGui(i+nins))
                nins=nins+1
        for i in range(self.GM.Marker.number+nins):
            g=self.guiMarkers[i]
            for j in range(len(g)):
                g[j].grid_configure(row=i+1)
                g[1]['text']=string.zfill(i+1,3)
                g[0].deselect()
                
    def gui_deleteMarkers(self):
        self.GM.Marker.equalize()
        ndel=0
        for i in range(self.GM.Marker.number):
            isdel = self.guiVar[i-ndel][0].get()
            if isdel:
                self.guiVar.pop(i-ndel)
                self.GM.Marker.pop(i-ndel)
                for t in self.guiMarkers.pop(i-ndel):
                    t.destroy()
                ndel=ndel+1
        for i in range(self.GM.Marker.number-ndel):
            g=self.guiMarkers[i]
            for j in range(len(g)):
                g[j].grid_configure(row=i+1)
                g[1]['text']=string.zfill(i+1,3)
                g[0].deselect()
    
    def gui_addMarker(self):
        M=self.GM.Marker
        M.equalize()
        nadd=0
        for i in range(M.number):
            isins = self.guiVar[i][0].get()
            if isins:
                self.addMarker(status=M.status[i],line=M.line[i],id=M.id[i],
                               symbol=M.symbol[i],color=M.color[i],size=M.size[i],
                               xoffset=M.xoffset[i],yoffset=M.yoffset[i],
                               id_font=M.id_font[i],id_size=M.id_size[i],id_color=M.id_color[i],
                               line_color=M.line_color[i],line_size=M.line_size[i],line_type=M.line_type[i])
                M.equalize()
                self.guiVar.append([Tkinter.IntVar(),Tkinter.IntVar(),Tkinter.StringVar()])
                self.guiMarkers.append(self.gui_addMarkerInGui(M.number-1))
                nadd=nadd+1
        if nadd==0:
            if M.number!=0:
                M.addMarker(status=M.status[-1],line=M.line[-1],id=M.id[-1],
                               symbol=M.symbol[-1],color=M.color[-1],size=M.size[-1],
                               xoffset=M.xoffset[-1],yoffset=M.yoffset[-1],
                               id_font=M.id_font[-1],id_size=M.id_size[-1],id_color=M.id_color[-1],
                               line_color=M.line_color[-1],line_size=M.line_size[-1],line_type=M.line_type[-1])
            else:
                M.addMarker()
            M.equalize()
            self.guiVar.append([Tkinter.IntVar(),Tkinter.IntVar(),Tkinter.StringVar()])
            self.guiMarkers.append(self.gui_addMarkerInGui(M.number-1))
               
    def gui_generateMarkersGui(self):
        root=self.notebook.page('Markers')
        #root.pack(fill = 'both', expand = 1)
        Frame=Pmw.ScrolledFrame(root,
                                labelpos = 'n',
                                label_text = 'Markers Editor',
##                                 hull_height=10,
                                )
##         Frame.pack(side='top',fill='both',expand=1)
        Frame.pack(fill = 'both', expand = 1)
        self.f=Frame.interior()
        F=Tkinter.Frame(root)
        F.pack(fill = 'both', expand = 1,side='bottom')
        Tkinter.Button(F,text='Add',command=self.gui_addMarker).pack(side='left')
##         Tkinter.Button(F,text='Update',command=self.gui_updateMarkers).pack(side='left')
        Tkinter.Button(F,text='Delete',command=self.gui_deleteMarkers).pack(side='left')
        Tkinter.Button(F,text='Insert',command=self.gui_copyMarkers).pack(side='left')
        Frame.pack_configure(side='top')
        Frame.pack_configure(fill='both')
        Frame.pack_configure(expand=1)
        Tkinter.Label(self.f,text=' ').grid(column=0,row=0)
        Tkinter.Label(self.f,text='#').grid(column=1,row=0)
        Tkinter.Label(self.f,text='Active').grid(column=2,row=0)
        Tkinter.Label(self.f,text='Symbol').grid(column=3,row=0)
        Tkinter.Label(self.f,text='Color').grid(column=4,row=0)
        Tkinter.Label(self.f,text='Size').grid(column=5,row=0)
        Tkinter.Label(self.f,text='Id').grid(column=6,row=0)
        Tkinter.Label(self.f,text='Id Size').grid(column=7,row=0)
        Tkinter.Label(self.f,text='+X').grid(column=8,row=0)
        Tkinter.Label(self.f,text='+Y').grid(column=9,row=0)
        Tkinter.Label(self.f,text='Id Color').grid(column=10,row=0)
        Tkinter.Label(self.f,text='Id Font').grid(column=11,row=0)
        Tkinter.Label(self.f,text='Line').grid(column=12,row=0)
        Tkinter.Label(self.f,text='Type').grid(column=13,row=0)
        Tkinter.Label(self.f,text='Size').grid(column=14,row=0)
        Tkinter.Label(self.f,text='Color').grid(column=15,row=0)
        
        self.guiMarkers=[]
        self.GM.Marker.equalize()
        for i in range(self.GM.Marker.number):
            if i>=len(self.guiVar):
                self.guiVar.append([Tkinter.IntVar(),Tkinter.IntVar(),Tkinter.StringVar()])
            self.guiMarkers.append(self.gui_addMarkerInGui(i))
        self.usedgui=1

    def gui_generateLabelsGui(self):
        root=self.notebook.page('Ticks and Labels')
        Frame=Pmw.ScrolledFrame(root,
                                labelpos = 'n',
                                label_text = 'Ticks and Labels',
##                                 hull_width=500,
                                )
##         Frame.pack(side='left',fill='both',expand=1)
        Frame.pack(fill = 'both', expand = 1,side='top')
        Frame=Frame.interior()
        gen=Pmw.Group(Frame,
                      tag_text='X axis',
                      tag_font=('times',14,'bold'),
                      )
        gen.pack(fill = 'both', expand = 1,side='top')
        gen=gen.interior()
        self.xticlabel=Pmw.EntryField(gen,
                                     labelpos = 'w',
                                     label_text='Labels',
                                     )
        self.xticlabel.pack(side='top')
        self.xticlabel.setentry(str(self.GM.xticlabels1))
        self.xtic=Pmw.EntryField(gen,
                                     labelpos = 'w',
                                     label_text='Ticks',
                                     )
        self.xtic.pack(side='top')
        self.xtic.setentry(str(self.GM.xmtics1))
        gen=Pmw.Group(Frame,
                      tag_text='Y axis',
                      tag_font=('times',14,'bold'),
                      )
        gen.pack(fill = 'both', expand = 1,side='top')
        gen=gen.interior()
        self.yticlabel=Pmw.EntryField(gen,
                                     labelpos = 'w',
                                     label_text='Labels',
                                     )
        self.yticlabel.pack(side='top')
        self.yticlabel.setentry(str(self.GM.yticlabels1))
        self.ytic=Pmw.EntryField(gen,
                                     labelpos = 'w',
                                     label_text='Ticks',
                                     )
        self.ytic.pack(side='top')
        self.ytic.setentry(str(self.GM.ymtics1))
        gen=Pmw.Group(Frame,
                      tag_text='Correlation arc',
                      tag_font=('times',14,'bold'),
                      )
        gen.pack(fill = 'both', expand = 1,side='top')
        gen=gen.interior()
        self.cticlabel=Pmw.EntryField(gen,
                                     labelpos = 'w',
                                     label_text='Labels',
                                     )
        self.cticlabel.pack(side='top')
        self.cticlabel.setentry(str(self.GM.cticlabels1))
        self.ctic=Pmw.EntryField(gen,
                                     labelpos = 'w',
                                     label_text='Ticks',
                                     )
        self.ctic.pack(side='top')
        self.ctic.setentry(str(self.GM.ymtics1))
        
    def gui_generateInterfaceGui(self):
        root=self.notebook.page('Interface')
        Frame=Pmw.ScrolledFrame(root,
                                labelpos = 'n',
                                label_text = 'Interface Editor',
##                                 hull_width=500,
                                )
##         Frame.pack(side='left',fill='both',expand=1)
        Frame.pack(fill = 'both', expand = 1,side='top')
        Frame=Frame.interior()
        #Frame=root
        gen=Pmw.Group(Frame,
                      tag_text='General Aspect',
                      tag_font=('times',14,'bold'),
                      )
        gen.pack(fill = 'both', expand = 1,side='top')
        gen=gen.interior()
        self.detail=Tkinter.Scale(gen,
                                  label='Detail',
                                  from_=50,
                                  to=500,
                                  orient='horizontal',
                                  resolution=5,
                                  tickinterval=50,
                                  length=350,
                                  )
        val=self.GM.detail
        if val<50:
            self.detail.configure(from_=val)
        if val>500:
            self.detail.configure(to=val)
        self.detail.pack(side='top')
        self.detail.set(val)

        self.maxvalue=Pmw.EntryField(gen,
                                     labelpos = 'w',
                                     label_text='Maximum Value',
                                     )
        self.maxvalue.pack(side='top')
        self.maxvalue.setentry(str(self.GM.max))
        self.quadran=Pmw.RadioSelect(gen,
                                     buttontype = 'radiobutton',
                                     orient=Tkinter.HORIZONTAL,
                                     labelpos = 'w',
                                     label_text='Quadran',
                                     )
        self.quadran.pack(side='top')
        self.quadran.add('1')
        self.quadran.add('2')
        self.quadran.invoke(self.GM.quadrans-1)

        self.refval=Pmw.EntryField(gen,
                                       labelpos = 'w',
                                       label_text='Reference Value',
                                       validate='real',
                                     )
        self.refval.pack(side='top')
        self.refval.setentry(self.GM.referencevalue)

        ## Skill related things
        skillFrame=Pmw.Group(Frame,
                             tag_text='Skill',
                             tag_font=('times',14,'bold'),
                             )
        skillFrame.pack(fill = 'both', expand = 1, side='top')
        skillFrame=skillFrame.interior()
        self.skillvalues=Pmw.EntryField(skillFrame,
                                     labelpos = 'w',
                                     label_text='Skill Values',
                                     )
        self.skillvalues.pack(side='top')
        self.skillvalues.setentry(str(self.GM.skillValues))
        
        cols=colors.cols.keys()
        cols.sort()
        self.skillcolor=Pmw.ComboBox(skillFrame,
                                     scrolledlist_items=cols,
                                     labelpos='w',
                                     label_text='Skill Lines Color',
                                     entry_width=10)
        self.skillcolor.pack(side='top')
        c=self.GM.skillColor
        if type(c)==type(6):
            c=self.x.getcolorcell(c)
            c=colors.rgb2str(c[0]*2.55,c[1]*2.55,c[2]*2.55)
        elif type(c)==type([]):
            c=colors.rgb2str(c)
        elif type(c)==type(''):
            r,g,b=colors.str2rgb(c)
            c=colors.rgb2str(r,g,b)
        self.skillcolor.selectitem(c)

        self.drawSkillVar=Tkinter.Variable()
        self.drawSkill=Tkinter.Checkbutton(skillFrame,
                                           text='Draw Skill Contour Lables',
                                           variable=self.drawSkillVar,
                                           )
        self.drawSkill.pack(side='top')
        
        if self.GM.skillDrawLabels=='y' :
            self.drawSkill.select()
        else:
            self.drawSkill.deselect()
                
        self.skillcoeff=Pmw.EntryField(skillFrame,
                                     labelpos = 'w',
                                     label_text='Skill Coefficients',
                                     )
        self.skillcoeff.pack(side='top')
        self.skillcoeff.setentry(str(self.GM.skillCoefficient))


        arrowframe=Pmw.Group(Frame,
                             tag_text='Arrows',
                             tag_font=('times',14,'bold'),
                             )
        arrowframe.pack(side='top')
        arrowframe=arrowframe.interior()
        
        self.length=Tkinter.Scale(arrowframe,
                                  label='Length',
                                  from_=0,
                                  to=100,
                                  orient='horizontal',
                                  resolution=1,
                                  tickinterval=10,
                                  length=350,
##                                   command=self.updatearrow,
                                  )
        self.length.pack(side='top')
        self.length.set(self.GM.arrowlength*100.)
        
        self.angle=Tkinter.Scale(arrowframe,
                                 label='Angle',
                                 from_=0,
                                 to=90,
                                 orient='horizontal',
                                 resolution=1,
                                 tickinterval=15,
                                 length=350,
                                 command=self.updatearrow,
                                 )
        self.angle.pack(side='top')
        self.angle.set(self.GM.arrowangle)
        
        self.base=Tkinter.Scale(arrowframe,
                                label='Base',
                                from_=0,
                                to=100,
                                orient='horizontal',
                                resolution=1,
                                tickinterval=10,
                                length=350,
                                command=self.updatearrow,
                                )
        self.base.pack(side='top')
        self.base.set(self.GM.arrowbase*100.)

        self.arrowcanvaswidth=75
        self.arrowCanvas=Tkinter.Canvas(arrowframe,
                                        width=self.arrowcanvaswidth,
                                        height=self.arrowcanvaswidth*2,
                                        )
        self.arrowCanvas.pack(side='top')
        self.arrowCanvas.create_rectangle(self.arrowcanvaswidth*.02,2*self.arrowcanvaswidth*.49,self.arrowcanvaswidth*.96,2*self.arrowcanvaswidth*.51,fill='black')
        self.arrow=self.arrowCanvas.create_polygon(196.0, 50.0, 186.0, 46.0, 191.0, 48.0, 186.0, 46.0, 196.0, 50.0,fill='black')

        
    def updateInterface(self):
        self.GM.detail=self.detail.get()
        v=self.maxvalue.getvalue()
        if v=='None':
            v=None
        else:
            v=string.atof(v)
        self.GM.max=v
        self.GM.quadrans=string.atoi(self.quadran.getcurselection())
        self.GM.referencevalue=string.atof(self.refval.getvalue())
        self.GM.skillValues=eval(self.skillvalues.getvalue())
        self.GM.skillColor=self.skillcolor.getvalue()[0]
        if self.drawSkill.cget('state')=='normal':
            self.GM.skillDrawLabels='y'
        else:
            self.GM.skillDrawLabels='n'            
        self.GM.skillCoefficient=eval(self.skillcoeff.getvalue())
        self.GM.arrowlength=self.length.get()/100.
        self.GM.arrowangle=self.angle.get()
        self.GM.arrowbase=self.base.get()/100.
        v=self.xticlabel.getvalue().strip()
        if v[0]=='{':
            v=eval(v)
        self.GM.xticlabels1=v
        v=self.xtic.getvalue().strip()
        if v[0]=='{':
            v=eval(v)
        self.GM.xmtics1=v
        v=self.yticlabel.getvalue().strip()
        if v[0]=='{':
            v=eval(v)
        self.GM.yticlabels1=v
        v=self.ytic.getvalue().strip()
        if v[0]=='{':
            v=eval(v)
        self.GM.ymtics1=v
        v=self.cticlabel.getvalue().strip()
        if v[0]=='{':
            v=eval(v)
        self.GM.cticlabels1=v
        v=self.ctic.getvalue().strip()
        if v[0]=='{':
            v=eval(v)
        self.GM.cmtics1=v

    def updatearrow(self,*args):
        l=.85
        a=self.angle.get()*1.
        b=self.base.get()/100.
        # Clean the window
        # The head
        xloc=self.arrowcanvaswidth*.99
        yloc=self.arrowcanvaswidth
        xs=[xloc,]
        ys=[yloc,]
        # first determine the angle:
        Alpha=a/180.*numpy.pi
        # First point behind
        yx=yloc-l*numpy.sin(Alpha)*self.arrowcanvaswidth
        xx=xloc-l*numpy.cos(Alpha)*self.arrowcanvaswidth
        xs.append(xx)
        ys.append(yx)
        # This should be the middle point
        yx=yloc
        xx=xloc-l*self.arrowcanvaswidth*b*numpy.cos(Alpha)
        xs.append(xx)
        ys.append(yx)
        Alpha=-Alpha
        # Second  point behind
        yx=yloc-l*numpy.sin(Alpha)*self.arrowcanvaswidth
        xx=xloc-l*numpy.cos(Alpha)*self.arrowcanvaswidth
        xs.append(xx)
        ys.append(yx)
        # Back to the begining
        xs.append(xs[0])
        ys.append(ys[0])
        coords=[self.arrow]
        for i in range(len(xs)):
            coords.append(xs[i])
            coords.append(ys[i])
        apply(self.arrowCanvas.coords,coords)
        return
                                           
    def gui_addMarkerInGui(self,row):
        gm=[]
        M=self.GM.Marker
        M.equalize()
        gm.append(Tkinter.Checkbutton(self.f,variable=self.guiVar[row][0]))
        gm.append(Tkinter.Label(self.f,text=string.zfill(row+1,3)))
        gm.append(Tkinter.Checkbutton(self.f,variable=self.guiVar[row][1]))
        if M.status[row]=='on' : gm[-1].select()
        s=M.symbol[row]
        syms=["dot", "plus", "star", "circle", "cross", "diamond",
              "triangle_up", "triangle_down", "triangle_left", "triangle_right", "square",
              "diamond_fill",
              "triangle_up_fill", "triangle_down_fill", "triangle_left_fill", "triangle_right_fill",
              "square_fill"]
        if type(s)==type(1):
            s=s+1
        for j in range(len(syms)):
            if s==syms[j] : s=j
        gm.append(Pmw.OptionMenu(self.f,items=syms,initialitem=s))
        cols=colors.cols.keys()
        cols.sort()
        cols.insert(0,'Same')
        gm.append(Pmw.ComboBox(self.f,scrolledlist_items=cols,entry_width=10))
        c=M.color[row]
        if type(c)==type(6):
            c=self.x.getcolorcell(c)
            c=colors.rgb2str(c[0]*2.55,c[1]*2.55,c[2]*2.55)
        elif type(c)==type([]):
            c=colors.rgb2str(c)
        gm[-1].selectitem(c)
        gm.append(Tkinter.Text(self.f,width=3,height=1))
        gm[-1].insert(Tkinter.END,str(M.size[row]))
        gm.append(Tkinter.Text(self.f,width=5,height=1))
        gm[-1].insert(Tkinter.END,str(M.id[row]))
        gm.append(Tkinter.Text(self.f,width=4,height=1))
        gm[-1].insert(Tkinter.END,M.id_size[row])
        gm.append(Tkinter.Text(self.f,width=4,height=1))
        gm[-1].insert(Tkinter.END,M.xoffset[row])
        gm.append(Tkinter.Text(self.f,width=4,height=1))
        gm[-1].insert(Tkinter.END,M.yoffset[row])
        gm.append(Pmw.ComboBox(self.f,scrolledlist_items=cols,entry_width=10))
        c=M.id_color[row]
        if type(c)==type(6):
            c=self.x.getcolorcell(c)
            c=colors.rgb2str(c[0]*2.55,c[1]*2.55,c[2]*2.55)
        elif type(c)==type([]):
            c=colors.rgb2str(c)
        gm[-1].selectitem(c)
        gm.append(Pmw.OptionMenu(self.f,items=['1','2','3','4','5','6','7','8','9'],initialitem=str(M.id_font[row])))
        iitem=M.line[row]
        if isinstance(iitem,str):
            iitem=string.capitalize(iitem)
        gm.append(Pmw.OptionMenu(self.f,items=['None','Tail','Head','Line'],initialitem=iitem))
        gm.append(Pmw.OptionMenu(self.f,items=["solid", "dash", "dot", "dash-dot", "long-dash"],initialitem=M.line_type[row]))
        gm.append(Tkinter.Text(self.f,width=4,height=1))
        gm[-1].insert(Tkinter.END,'1.')
        gm.append(Pmw.ComboBox(self.f,scrolledlist_items=cols,entry_width=10))
        c=M.line_color[row]
        if type(c)==type(6):
            c=self.x.getcolorcell(c)
            c=colors.rgb2str(c[0]*2.55,c[1]*2.55,c[2]*2.55)
        elif type(c)==type([]):
            c=colors.rgb2str(c)
        gm[-1].selectitem(c)

        for j in range(len(gm)):
            gm[j].grid(column=j,row=row+1)
        return gm
    
    def get_settings(self):
        self.updateInterface()
        self.gui_updateMarkers()
        return

    def gui_updateMarkers(self):
        M=self.GM.Marker
        M.equalize()
        n=M.number
        for i in range(n):
            gm=self.guiMarkers[i]
            s=self.guiVar[i][1].get()
            if s==0:
                M.status[i]='off'
            else:
                M.status[i]='on'
            a=gm[3].getcurselection()
            M.size[i]=string.atoi(gm[5].get(0.0,Tkinter.END))
            M.symbol[i]=gm[3].getcurselection()
            M.color[i]=gm[4].get()
            M.id[i]=gm[6].get(0.0,Tkinter.END)[:-1]
            M.id_size[i]=string.atoi(gm[7].get(0.0,Tkinter.END))
            x=string.atof(gm[8].get(0.0,Tkinter.END))
            y=string.atof(gm[9].get(0.0,Tkinter.END))
            M.xoffset[i]=x
            M.yoffset[i]=y
            M.id_color[i]=gm[10].get()
            M.id_font[i]=string.atoi(gm[11].getcurselection())
            M.line[i]=string.lower(gm[12].getcurselection())
            if M.line[i]=='none' : M.line[i]=None
            M.line_type[i]=gm[13].getcurselection()
            M.line_size[i]=string.atof(gm[14].get(0.0,Tkinter.END))
            M.line_color[i]=gm[15].get()


if __name__=='__main__':
    #print 'Init'
    import sys
    sys.path.append('/home/doutriau/tmp/tdiag/')
    import KarlLegacy
    root=Tkinter.Tk(":0.0") # Use the localhost:0.0 for the DISPLAY and screen
    x=vcs.init()
    TD=TDGui(root,x)
    TD.gui_start()
    root.mainloop()
    print 'Done'
    #sys.stdin.readline()



            



