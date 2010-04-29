#!/usr/bin/env python
import sys,os

try:
    import Tkinter,tkFileDialog,Dialog
except:
    print "Your Python located in ( "+sys.prefix+" ) is not built with Tcl/Tk support"
    print "You now have 3 options"
    print "1- Try running this script with another Python"
    print "2- Type a path where you would like to build a new python and where CDAT will be installed"
    print "3- Try to do the install yourself"
    answer=raw_input("Press Enter to quit or type a path where to build CDAT")
    if answer == "":
        sys.exit()
    else:
        try:
            os.makedirs(answer)
        except:
            pass
        cmd="./express_install "+answer
        print 'Now running basic script to install Python'
        print 'Command is:' +cmd
        a=os.popen4(cmd,0)[1]
        cont=1
        l=a.readline()
        while l!="":
            print l,
            l=a.readline()

## Ok now the hard part figuring out the Tk and all....

class InstallGui(Tkinter.Frame):
    def __init__(self):
        sys.path.insert(0,'Packages/browser/Lib')
        from cdat_version import Version
        self.Version=Version
        sys.path.pop(0)
        Tkinter.Frame.__init__(self)
        self.master.title('Installing CDAT')
        self.master.geometry('+10+15')
        txt="""
Hello
Welcome to the Climate Data Analysis Tools installation GUI
Version 1.0
This will help you install CDAT onto your system.
At this time it is not intended to be used to reinstall parts of CDAT
To do so you will have to operate from the command line
ALL of CDAT proper packages will ALWAYS be reinstalled
To Continue, press "Next" to cancel and quit press "Exit"
        """
        self.GUI_color='white'
        self.lbl=Tkinter.Label(self.master,text=txt,bg=self.GUI_color)
        self.lbl.pack(side='top',expand='y',fill='both')
        self.middle_frame=Tkinter.Frame(self.master,bg=self.GUI_color)
        self.middle_frame.pack(side='top',fill='both',expand='y')
        self.bottom_frame=Tkinter.Frame(self.master,bg=self.GUI_color)
        self.button=Tkinter.Button(self.bottom_frame,text='Exit',command=self.master.destroy,bg=self.GUI_color)
        self.button.pack(side='left',anchor='w')
        self.next=Tkinter.Button(self.bottom_frame,text="Next -->",command=self.step1,bg=self.GUI_color)
        self.next.pack(side='right',anchor='e')
        self.bottom_frame.pack(side='top',expand='y',fill='both')
        self.open_img=Tkinter.PhotoImage(file='Scripts/open.gif')
        self.path=Tkinter.StringVar()
        self.path.set('/usr/local/cdat/'+self.Version)
        self.todestroy=[self.button]
        self.actions=['./clean_script all']
        self.ex=[]
        self.op=[]
        self.pk=[]
        self.licences=['Legal.txt','gpl.txt','External_License/CCLRC_CDAT_License.txt']
        
    def step1(self):
        
                
        for d in self.todestroy:
            d.destroy()
        txt="""
What kind of installation would you like ?
"""
        self.lbl.configure(text=txt)
        self.master.title("Installing CDAT: Step 1 - Installation Type" )
        self.t1=Tkinter.StringVar()
        self.t2=Tkinter.StringVar()
        self.c1=Tkinter.Checkbutton(self.middle_frame,text="Standard",variable=self.t1,command=self.toggle1,bg=self.GUI_color)
        self.c1.select()
        self.c2=Tkinter.Checkbutton(self.middle_frame,text="Custom",variable=self.t2,command=self.toggle2,bg=self.GUI_color)
        self.c2.deselect()
        self.c1.pack(anchor='w')
        self.c2.pack(anchor='w')
        self.next.configure(command=self.step2,text="Next-->")
        self.todestroy=[self.c1,self.c2]
        
    def toggle1(self):
        self.c1.select()
        self.c2.deselect()
        
    def toggle2(self):
        self.c2.select()
        self.c1.deselect()

    def std(self):
        for d in self.todestroy:
            d.destroy()
        self.actions.append("./express_install "+self.path.get())
        packages=[]
        force=1
        clist=[]
        a=execfile('installation/contrib.py')
        self.extra_licences=[]
        for p,d,l in clist:
            if l.split('/')[-1].strip() not  in ['GNU','UC','']:
                self.extra_licences.append(p+'/'+l)
        self.button=Tkinter.Button(self.bottom_frame,command=self.step2,text="<-- Back",bg=self.GUI_color)
        self.button.pack(side='left',anchor='w')
        self.todestroy=[self.button]
        self.finish()

    def accept(self):
        if self.ok.get()=='1':
            self.next.configure(state='active')
        else:
            self.next.configure(state='disabled')
            
    def finish(self):
        try:
            os.makedirs(self.path.get())
        except:
            if not os.path.exists(self.path.get()):
                print 'Error could not create install dir, you might need more privileges'
                sys.exit()
            
        for d in self.todestroy:
            d.destroy()
            
        txt="""
Licences
Please read and agree with the following licences agreement that come with CDAT and the Packages you picked.
"""
        self.lbl.configure(text=txt)
        self.ok=Tkinter.StringVar()
        def cont():
            self.cont=0
        self.next.configure(command=cont)

        licences=self.licences+self.extra_licences
        for l in licences:
            if l==licences[-1]:
                self.next.configure(text='Install CDAT')
            self.cont=1
            f=open(l)
            txt=f.read()
            mf=Tkinter.Frame(self.middle_frame,bg=self.GUI_color)
            mf.pack(side='top',expand='y',fill='both')
            sb=Tkinter.Scrollbar(mf,orient='vertical',bg=self.GUI_color)
            sb.pack(side='right',expand='y',fill='y')
            licence_label=Tkinter.Text(mf,yscrollcommand=sb.set,bg=self.GUI_color)
            licence_label.insert('end',txt)
            accept=Tkinter.Checkbutton(self.middle_frame,text='I Accept',command=self.accept,variable=self.ok,bg=self.GUI_color)
            accept.deselect()
            self.next.configure(state='disabled')
            licence_label.pack(side='left',expand='y',fill='both')
            accept.pack(side='top',anchor='w')
            while self.cont:
                self.master.update()
            licence_label.destroy()
            accept.destroy()
            mf.destroy()
            
        f=open('build_CDAT.sh','w')
        print >>f , "#!/usr/bin/env sh"
        for act in self.actions:
            print >>f,act
        f.close()
        txt="""
Now installing CDAT
Build script has been saved in "build_CDAT.sh"
Build output is being dumped to the screen
"""
        self.lbl.configure(text=txt)
        self.next.configure(text='Finished',command=self.master.destroy,state='disabled')
        mf=Tkinter.Frame(self.middle_frame,bg=self.GUI_color)
        mf.pack(side='top',expand='y',fill='both')
        sb=Tkinter.Scrollbar(mf,orient='vertical',bg=self.GUI_color)
        sb.pack(side='right',expand='y',fill='y')
        t=Tkinter.Text(mf,yscrollcommand=sb.set)
        t.pack(side='left',anchor='nw',expand='y',fill='both')
        os.chmod("build_CDAT.sh",0770)
        a=os.popen4('build_CDAT.sh',0)[1]
        l=a.readline()
        while l!="":
            t.insert('end',l)
            self.update()
            l=a.readline()
        self.next.configure(state='active')
        txt="""
CDAT Installation Finished
Thanks!
"""
        self.lbl.configure(text=txt)


    def python(self):
        python=self.path.get()+'/bin/python'
        if os.path.exists(python):
            a=os.popen4(python+' test_python_ok.py')[1].readlines()
            if a==['Your Python checked OK!\n']:
                self.exsrc()
            else:
                txt="Your Python ( "+python+" ) is not compatible with CDAT\n"
                txt+='Reason invoked:\n'
                for t in a:
                    txt+=t
                d=Dialog.Dialog(self.master,title='Python Not Working',text=txt,bitmap='',default='quit',strings=['Try Again'])
        else:
            self.actions.append('cd pysrc; ./install_script '+self.path.get()+' ; cd ..')
            self.exsrc()
        pass

    def getdir(self,event):
        self.path.set(tkFileDialog.askdirectory(title='hi',initialdir=self.path.get()))
        
    def step2(self):
        for n,v,b,f in self.ex:
            b.pack_forget()
            f.pack_forget()
        for n,v,b,f in self.op:
            b.pack_forget()
            f.pack_forget()
                
        for d in self.todestroy:
            d.destroy()
        self.todestroy=[]
        self.lbl2=Tkinter.Label(self.middle_frame,text='Installation directory:',bg=self.GUI_color)
        self.lbl2.pack(side='left')
        self.todestroy.append(self.lbl2)
        self.icon=Tkinter.Canvas(self.middle_frame,bd=0, highlightthickness=0,
                                 width = 27, height = 27)
        self.icon.create_image(0,0,anchor=Tkinter.NW,image=self.open_img)
        self.icon.bind('<1>',self.getdir)
        self.icon.pack(side='left')
        self.todestroy.append(self.icon)
        if self.t1.get()=='1': #Standard install
            self.master.title('CDAT: Installation Path')
            self.next.configure(command=self.std,text='Next -->')
            txt="""
Choose a directory where to install CDAT
"""
            self.lbl.configure(text=txt)
        else:
            self.next.configure(command=self.python,text='Next -->')
            self.master.title('CDAT Install: Choosing your Python')
            txt="""
Custom Install
--------------
Step 1: Python install

You seem to be using a working Python from %s
to use this Python click "Next"
or to use a different one or build a new one, first select another directory and click "Next"
To Exit without doing anything click "Cancel"
""" % (sys.prefix)
            self.lbl.configure(text=txt)
            self.path.set(sys.prefix)
        self.Path=Tkinter.Entry(self.middle_frame,textvariable=self.path,bg=self.GUI_color)
        self.Path.pack(side='left',fill='x',expand='y')
        self.todestroy.append(self.Path)
        self.button=Tkinter.Button(self.bottom_frame,command=self.step1,text="<-- Back",bg=self.GUI_color)
        self.button.pack(side='left',anchor='w')
        self.todestroy.append(self.button)
        self.quit=Tkinter.Button(self.bottom_frame,command=self.master.destroy,text="Cancel",bg=self.GUI_color)
        self.quit.pack(side='left',anchor='c')
        self.todestroy.append(self.quit)

    def exsrc(self):
        for n,v,b,f in self.pk:
            b.pack_forget()
            f.pack_forget()
        for d in self.todestroy:
            d.destroy()
        if self.ex==[]:
            new=1
        else:
            new=0
        txt="""
Custom Install
--------------
Step 2: External (non PCMDI) Packages needed by CDAT

Here you can choose to upgrade existing package needed by CDAT or install Optional Packages

"""
        self.lbl.configure(text=txt)
        self.next.configure(command=self.Packages,text='Next -->')
        self.button=Tkinter.Button(self.bottom_frame,command=self.step2,text="<-- Back",bg=self.GUI_color)
        self.button.pack(side='left',anchor='w')
        self.todestroy=[self.button]

        self.externals=[['NetCDF','The Netcdf library'],
                        ['Numeric','C based Numerical library, the heart of our maths'],
                        ['Pmw','An extension on Tkinter, adds a lot of features'],
                        ['Pyfort','CDAT prefered Fortran wrapper'],
                        ['XGKS','VCS underlying graphic libraries, (header files and fonts)'],
                        ['gplot','Converts cgm to postscript'],
                        ['Ghostscript','Used to convert from postscript to numerous format'],
                        ['gifsicle','Used to create animated GIF files'],
                        ]
        if sys.platform in ['linux2','darwin','cygwin']:
            self.externals.append(['netpbm','Image format conversion tools'])
        else:
            self.externals.append(['pbmplus','Image format conversion tools'])

        self.optionals=[['VTK','The Visual Tool Kit, handles 3D and meshes, very long to build'],
                        ['R', 'Amazing Statistical Package(s), very long to build'],
                        ['ioapi','Air Pollution Data I/O, requires Netcdf with Fortran (no DODS)'],
                        ]
        if sys.platform in ['linux2','darwin']:
            self.optionals.insert(0,['DODS','OpenDAP CLient, replaces NetCDF'])
        self.needed=['NetCDF']

        pth=self.path.get()+'/bin/python'
        if os.path.exists(pth):
            a=os.popen4(pth+' test_exsrc_ok.py')[1].readlines()
            for l in a:
                sp=l.split(':')
                self.needed.append(sp[0].strip())
        else:
            for e,d in self.externals:
                self.needed.append(e)

        lbl=Tkinter.Label(self.middle_frame,text='\n----- Must Have -----\n',bg=self.GUI_color)
        lbl.pack(side='top',anchor='c')
        self.todestroy.append(lbl)
        ex=[]
        for e,d in self.externals:
            if new:
                f=Tkinter.Frame(self.middle_frame,bg=self.GUI_color)
                v=Tkinter.StringVar()
                b=Tkinter.Checkbutton(f,text=e,variable=v,bg=self.GUI_color)
                b.deselect()
            else:
                for n,vv,bb,ff in self.ex:
                    if n==e:
                        v=vv
                        b=bb
                        f=ff
            f.pack(side='top',expand='y',fill='x')
            b.pack(side='left',anchor='w')
            l=Tkinter.Label(f,text=d,bg=self.GUI_color)
            l.pack(side='right',anchor='e')
            self.todestroy.append(l)
            if e in self.needed:
                b.select()
                b.configure(state='disabled')
            ex.append([e,v,b,f])
        self.ex=ex
        op=[]
        lbl=Tkinter.Label(self.middle_frame,text='\n----- Optionals -----\n',bg=self.GUI_color)
        lbl.pack(side='top',anchor='c')
        self.todestroy.append(lbl)
        for o,d in self.optionals:
            if new:
                v=Tkinter.StringVar()
                f=Tkinter.Frame(self.middle_frame,bg=self.GUI_color)
                v=Tkinter.StringVar()
                b=Tkinter.Checkbutton(f,text=o,variable=v,bg=self.GUI_color)
                b.deselect()
                if o=='DODS':
                    b.configure(command=self.DODS)
                    b.invoke()
                if o=='ioapi':
                    b.configure(command=self.ioapi)
            else:
                for n,vv,bb,ff in self.op:
                    if n==o:
                        v=vv
                        b=bb
                        f=ff
            f.pack(side='top',expand='y',fill='x')
            b.pack(side='left',anchor='w')
            l=Tkinter.Label(f,text=d,bg=self.GUI_color)
            l.pack(side='right',anchor='e')
            self.todestroy.append(l)
            op.append([o,v,b,f])
        self.op=op

    def ioapi(self):
        for n,v,b,f in self.ex:
            if n=='NetCDF':
                b.select()
        for n,v,b,f in self.op:
            if n=='DODS':
                b.deselect()

    def DODS(self):
        value='1'
        for n,v,b,f in self.op:
            if n=='DODS':
                value = v.get()
        for n,v,b,f in self.ex:
            if n=='NetCDF':
                if value=='1':
                    b.deselect()
                else:
                    b.select()
        for n,v,b,f in self.op:
            if n=='ioapi':
                if value:
                    b.deselect()
                
    def Packages(self):
        cmd='cd exsrc ; ./install_script '+self.path.get()+' '
        for n,v,b,f in self.ex:
            b.pack_forget()
            f.pack_forget()
            if v.get()=='0':
                cmd+=' --without-'+n.lower()
        for n,v,b,f in self.op:
            b.pack_forget()
            f.pack_forget()
            if v.get()=='1':
                cmd+=' --with-'+n.lower()
            elif n=='DODS':
                cmd+=' --without-DODS'
        for n,v,b,f,l in self.pk:
            b.pack_forget()
            f.pack_forget()
        cmd+=' ; cd ..'
        self.actions.append(cmd)
        for n,v,b,f in self.op:
            if v.get()=='0':
                cmd+=' --without-'+n.lower()
            else:
                cmd+=' --with-'+n.lower()
        for d in self.todestroy:
            d.destroy()
        txt="""
Custom Install
--------------
Step 3 : Contrib (non PCMDI) Packages that you can add to CDAT

Here you can choose to add packages to CDAT
These have been developped outside of PCMDI and are not supported by PCMDI
Most of them Require Fortran
They're located in the contrib directory and more documentation may have been provided
by their developpers in their own directory

"""
        self.lbl.configure(text=txt)
        self.next.configure(command=self.donePackages,text='Next -->')
        self.button=Tkinter.Button(self.bottom_frame,command=self.exsrc,text="<-- Back",bg=self.GUI_color)
        self.button.pack(side='left',anchor='w')
        self.todestroy=[self.button]
        
        packages=[]
        force=1
        clist=[]
        a=execfile('installation/contrib.py')
        if self.pk==[]:
            new=1
        else:
            new=0
        pk=[]
        clist.append(['contrib/Rpy','Python Interface to the R library','GNU'])
        for p,d,l in clist:
            t=p.split('contrib/')[-1].strip()
            if new:
                f=Tkinter.Frame(self.middle_frame,bd=2,relief='sunken',bg=self.GUI_color)
                v=Tkinter.StringVar()
                b=Tkinter.Checkbutton(f,text=t,variable=v,bg=self.GUI_color)
                b.select()
            else:
                for n,vv,bb,ff,ll in self.pk:
                    if n==t:
                        v=vv
                        b=bb
                        f=ff
                        l=ll
            T=Tkinter.Label(f,text=d,wraplength=400,justify='right',bg=self.GUI_color)
            f.pack(side='top',expand='y',fill='x')
            b.pack(side='left',anchor='w')
            T.pack(side='right',anchor='e')
            self.todestroy.append(T)
            pk.append([t,v,b,f,p.strip()+'/'+l])
        self.pk=pk
    def donePackages(self):
        for d in self.todestroy:
            d.destroy()
        for t,v,b,f,l in self.pk:
            b.pack_forget()
            f.pack_forget()
        cmd=self.path.get()+'/bin/python install.py --without-contrib --force'
        for n,v,b,f in self.op:
            b.pack_forget()
            f.pack_forget()
            if v.get()=='0' and  n=='DODS':
                cmd+=' --without-DODS'
        
        cmd+=' -c gui_contrib.py'
        file=open('gui_contrib.py','w')
        self.extra_licences=[]
        print >> file, 'clist=['
        for n,v,b,f,l in self.pk:
            if v.get()=='1':
                print >> file , '"contrib/'+n+'",'
                if l.split('/')[-1].strip() not in ['GNU','UC','']:
                    self.extra_licences.append(l)
        print >> file, ']'
        print >> file, 'for c in clist:'
        print >> file, '  packages.append(c)'
        file.close()
        self.actions.append(cmd)
        self.button=Tkinter.Button(self.bottom_frame,command=self.Packages,text="<-- Back",bg=self.GUI_color)
        self.button.pack(side='left',anchor='w')
        self.todestroy=[self.button]
        self.finish()
    
InstallGui().mainloop()
