# Adapted for numpy/ma/cdms2 by convertcdms.py
import cdms2 as cdms,vcs,WK

import Pmw,Tkinter
import gui_control
import gui_message
import gui_formulate
import gui_user_menus
import gui_busy

def get_vars_from_gm_line(parent):
    vars = []
    d1 = parent.pl.form[id].data1.get()
    if d1=="":
        return vars
    vars.append(__main__.__dict__[ d1 ])
 
    d2 = parent.pl.form[id].data2.get()
    if d2=="":
        return vars
    vars.append(__main__.__dict__[ d2 ])
 
    for j in range(len(parent.pl.form[id].datas)):
        d = parent.pl.form[id].datas[j].get()
        if d=="":
            return vars
        else:
            vars.append(__main__.__dict__[ d ])


def get_vars(parent):
    vars = []
    try:
        if (parent.panelDM.var3 is not None):
            vars.append(gui_formulate.data( parent, d_name = parent.panelDM.var3))
        else:
            from_selected = 1
            if (len(parent.panelDV.selected) == 0):
                gui_message.error('Must first select a variable from the "Select Variable" panel above or from the "Defined Variables" list window below.')
                return vars
            lst = parent.panelDV.selected_list.keys()
            for v in lst:
                vars.append(gui_formulate.data( parent, var = parent.panelDV.lst1[ parent.panelDV.selected_list[ v ] ]))
    except Exception,err:
        gui_message.error( 'Invalid data or no variable selected. Cannot compute field.\nError:\n'+str(err) )
        gui_busy.busyEnd( self, parent )
        return []
    return vars

def prepW(parent):
    W=WK.WK(tkbar=1)
    default_wn = ((1000,10,),)
    default_fn = ((.1,5),(.2,10),(.3,20),(1000,40))
    if parent.menu.main_menu.wk_options.get():
        try:
            dialog = Pmw.Dialog(parent,
                                title = "Options for Processing WK",
                                buttons = ('OK',),
                                )
            f = Pmw.Group(dialog.interior(),tag_text='General')
            f.pack(side='top',expand=1,fill='both')
            f=f.interior()
            
            dialog.fqcy = Pmw.EntryField(f,
                                       label_text='Fqcy of data',
                                       labelpos='w',
                                       value = '%f' % W.frequency,
                                       validate={'validator':'real'},
                                       )
            parent.balloon.bind(dialog.fqcy,'Frequency of your data in number of cycles per day')
            dialog.fqcy.pack(side='top',fill='x',expand=1)
            dialog.window = Pmw.EntryField(f,
                                       label_text='Window length',
                                       labelpos='w',
                                       value = '%i' % W.number_of_days,
                                       validate={'validator':'integer'},
                                       )
            dialog.window.pack(side='top',fill='x',expand=1)
            parent.balloon.bind(dialog.window,'Power is computed over windows of constant length shifted per a number of days each time')
            dialog.shift = Pmw.EntryField(f,
                                       label_text='Shift',
                                       labelpos='w',
                                       value = '%i' % W.shift,
                                       validate={'validator':'integer'},
                                       )
            dialog.shift.pack(side='top',fill='x',expand=1)
            parent.balloon.bind(dialog.shift,'Power is computed over windows of constant length shifted per a number of days each time')
            f = Pmw.Group(dialog.interior(),tag_text='Splitting Sym/Antisym')
            f.pack(side='top',expand=1,fill='both')
            f=f.interior()
            dialog.split = Pmw.RadioSelect(f,
                                           labelpos='w',
                                           buttontype='checkbutton',
                                           orient='vertical',
                                           label_text='Do you want to :',
                                           hull_borderwidth = 2,
                                           hull_relief = 'ridge',
                                           )
            dialog.split.pack(side='top',fill='x',expand=1)
            for text in ('Average Time Sub-domains','Apply 121Smooth'):
                dialog.split.add(text)
                dialog.split.invoke(text)
            f = Pmw.Group(dialog.interior(),tag_text='Computing Background')
            f.pack(side='top',expand=1,fill='both')
            f=f.interior()
            dialog.wn = Pmw.EntryField(f,
                                       label_text='Wavenumbers smoothing:',
                                       labelpos='w',
                                       value = '%s' % str(default_wn)
                                       )
            parent.balloon.bind(dialog.wn,'list of tuples of wavenumber/number of 121 smoothing, wavenumber must be increasing')
            dialog.wn.pack(side='top',fill='x',expand=1)
            dialog.fn = Pmw.EntryField(f,
                                       label_text='Frequencies smoothing:',
                                       labelpos='w',
                                       value = '%s' % str(default_fn)
                                       )
            parent.balloon.bind(dialog.fn,'list of tuples of frequencies/number of 121 smoothing, frequencies must be increasing')
            dialog.fn.pack(side='top',fill='x',expand=1)
          
            dialog.activate()

            ## Inits WK objects
            fqcy = float(dialog.fqcy.get())
            nd = int(dialog.window.get())
            shift = int(dialog.shift.get())
            W = WK.WK(frequency=fqcy,number_of_days=nd,shift=shift,tkbar=1)

            ## Options for split
            ons = dialog.split.getvalue()
            splits = [0,0]
            if 'Average Time Sub-domains' in ons:
                splits[0] = 1
            if 'Apply 121Smooth' in ons:
                splits[1] = 1

            ## options for bg
            cont = True
            while cont:
                try:
                    wn = eval(dialog.wn.get())
                    if not isinstance(wn,(list,tuple)):
                        gui_message.error('Wavenumbers smoothing must be list of couples value/number')
                    for w in wn:
                        if not isinstance(w,(list,tuple)):
                            gui_message.error('Wavenumbers smoothing must be list of couples value/number')
                            break
                        if len(w)!=2:
                            gui_message.error('Wavenumbers smoothing must be list of couples value/number')
                            break
                    fn = eval(dialog.fn.get())
                    if not isinstance(fn,(list,tuple)):
                        gui_message.error('Frequencies smoothing must be list of couples value/number')
                    for f in fn:
                        if not isinstance(f,(list,tuple)):
                            gui_message.error('Frequencies smoothing must be list of couples value/number')
                            break
                        if len(f)!=2:
                            gui_message.error('Frequencies smoothing must be list of couples value/number')
                            break
                    cont = False
                except:
                    pass
            dialog.destroy()
            return W,splits,[wn,fn]
        except Exception,err:
            pass
    return W,[1,1],[default_wn,default_fn]

def power(self,parent):
    vars = get_vars(parent)
    if len(vars)>1:
        gui_message.error('Power computation requires one variable only')
        return
    
    try:
        W,splits,bg = prepW(parent)
        gui_busy.busyStart( self, parent )
        power = W.process(vars[0])
        gui_user_menus.user_menus_put( power.id,power)
    except Exception,err:
        gui_message.error('Error computing power:\n%s' % err)
        gui_busy.busyEnd( self, parent )
        return
    gui_busy.busyEnd( self, parent )
     
def split(self,parent):
    vars = get_vars(parent)
    if len(vars)>1:
        gui_message.error('Split computation requires one variable only')
        return
    
    try:
        W,splits,bg = prepW(parent)
        gui_busy.busyStart( self, parent )
        S,A = W.split(vars[0],compresstime=splits[0],smooth=splits[1])
        gui_user_menus.user_menus_put( A.id,A)
        gui_user_menus.user_menus_put( S.id,S)
    except Exception,err:
        gui_message.error('Error computing split:\n%s' % err)
        gui_busy.busyEnd( self, parent )
        return
    gui_busy.busyEnd( self, parent )

def background(self,parent):
    vars = get_vars(parent)
    if len(vars)!=2:
        gui_message.error('Background computation requires 2 variables')
        return
    try:
        W,splits,bg = prepW(parent)
        gui_busy.busyStart( self, parent )
        bg = W.background(vars[0],vars[1],wavenumber_smoothing_windows=bg[0],frequencies_smoothing_windows=bg[1])
        gui_user_menus.user_menus_put( bg.id,bg)
    except Exception,err:
        gui_message.error('Error computing background:\n%s' % err)
        gui_busy.busyEnd( self, parent )
        return
    gui_busy.busyEnd( self, parent )

def process(self,parent):
    vars = get_vars(parent)
    if len(vars)>1:
        gui_message.error('WK computations requires one variable only')
        return
    
    try:
        W,splits,bg = prepW(parent)
        gui_busy.busyStart( self, parent )
        power = W.process(vars[0])
        S,A = W.split(power,compresstime=splits[0],smooth=splits[1])
        bg = W.background(S,A,wavenumber_smoothing_windows=bg[0],frequencies_smoothing_windows=bg[1])
        gui_user_menus.user_menus_put( power.id,power)
        gui_user_menus.user_menus_put( A.id,A)
        gui_user_menus.user_menus_put( S.id,S)
        gui_user_menus.user_menus_put( bg.id,bg)
    except Exception,err:
        gui_message.error('Error computing power:\n%s' % err)
        gui_busy.busyEnd( self, parent )
        return
    gui_busy.busyEnd( self, parent )
   
def fig1(self,parent):
    parent.external_plot = dofig1
    v = parent.panelGC.opt2.getvalue()
    items =  parent.panelGC.opt2.cget('items')
    items[-1]='Ext (%s)' % 'WK1'
    parent.panelGC.opt2.setitems(items)
    parent.panelGC.opt2.invoke(items[-1])
    parent.panelGC.evt_plot(parent)

def fig2(self,parent):
    parent.external_plot = dofig2
    v = parent.panelGC.opt2.getvalue()
    items =  parent.panelGC.opt2.cget('items')
    items[-1]='Ext (%s)' % 'WK2'
    parent.panelGC.opt2.setitems(items)
    parent.panelGC.opt2.invoke(items[-1])
    parent.panelGC.evt_plot(parent)

def fig3(self,parent):
    parent.external_plot = dofig3
    v = parent.panelGC.opt2.getvalue()
    items =  parent.panelGC.opt2.cget('items')
    items[-1]='Ext (%s)' % 'WK3'
    parent.panelGC.opt2.setitems(items)
    parent.panelGC.opt2.invoke(items[-1])
    parent.panelGC.evt_plot(parent)


def prepWKPlot(parent):
    WP = WK.WKPlot(x=parent.vcs[ parent.vcs_id ])

    if parent.menu.main_menu.wk_options.get():
        try:
            dialog = Pmw.Dialog(parent,
                                title = "Options for Processing WK",
                                buttons = ('OK',),
                                )
            f = Pmw.Group(dialog.interior(),tag_text='General')
            f.pack(side='top',expand=1,fill='both')
            f=f.interior()
            
            dialog.datawc_x1 = Pmw.EntryField(f,
                                       label_text='Min Wave Number',
                                       labelpos='w',
                                       value = '%f' % WP.datawc_x1,
                                       validate={'validator':'real'},
                                       )
            dialog.datawc_x1.pack(side='top',fill='x',expand=1)
            dialog.datawc_x2 = Pmw.EntryField(f,
                                       label_text='Max Wave Number',
                                       labelpos='w',
                                       value = '%f' % WP.datawc_x2,
                                       validate={'validator':'real'},
                                       )
            dialog.datawc_x2.pack(side='top',fill='x',expand=1)
            dialog.datawc_y1 = Pmw.EntryField(f,
                                       label_text='Min Frequency',
                                       labelpos='w',
                                       value = '%f' % WP.datawc_y1,
                                       validate={'validator':'real'},
                                       )
            dialog.datawc_y1.pack(side='top',fill='x',expand=1)
            dialog.datawc_y2 = Pmw.EntryField(f,
                                       label_text='Max Frequency',
                                       labelpos='w',
                                       value = '%f' % WP.datawc_y2,
                                       validate={'validator':'real'},
                                       )
            dialog.datawc_y2.pack(side='top',fill='x',expand=1)
            dialog.min = Pmw.EntryField(f,
                                       label_text='Power minimum',
                                       labelpos='w',
                                       value = '%f' % WP.min,
                                       validate={'validator':'real'},
                                       )
            dialog.min.pack(side='top',fill='x',expand=1)
            dialog.max = Pmw.EntryField(f,
                                       label_text='Power maximum',
                                       labelpos='w',
                                       value = '%f' % WP.max,
                                       validate={'validator':'real'},
                                       )
            dialog.max.pack(side='top',fill='x',expand=1)
            dialog.delta_f = Pmw.EntryField(f,
                                       label_text='Isofill delta',
                                       labelpos='w',
                                       value = '%f' % WP.delta_isofill,
                                       validate={'validator':'real'},
                                       )
            dialog.delta_f.pack(side='top',fill='x',expand=1)
            dialog.delta_l = Pmw.EntryField(f,
                                       label_text='Isoline delta',
                                       labelpos='w',
                                       value = '%f' % WP.delta_isoline,
                                       validate={'validator':'real'},
                                       )
            dialog.delta_l.pack(side='top',fill='x',expand=1)
          
            f = Pmw.Group(dialog.interior(),tag_text='Figure 3')
            f.pack(side='top',expand=1,fill='both')
            f=f.interior()
            dialog.H = Pmw.EntryField(f,
                                       label_text='H lines',
                                       labelpos='w',
                                       value = '%s' % repr(WP.H),
                                       )
            dialog.H.pack(side='top',fill='x',expand=1)
          
            
            dialog.activate()

            WP.datawc_x1 = float(dialog.datawc_x1.get())
            WP.datawc_x2 = float(dialog.datawc_x2.get())
            WP.datawc_y1 = float(dialog.datawc_y1.get())
            WP.datawc_y2 = float(dialog.datawc_y2.get())
            WP.min = float(dialog.min.get())
            WP.max = float(dialog.max.get())
            WP.delta_isoline = float(dialog.delta_l.get())
            WP.delta_isofill = float(dialog.delta_f.get())
            WP.H = eval(dialog.H.get())
            
        except Exception,err:
            print err
            pass
    
    return WP
    
def dofig1(parent,x,template, *vars):
    if len(vars)!=2:
        gui_message.error('Unable to plot data! You need to pass 2 variables')
        return
    WP = prepWKPlot(parent)
    WP.x.clear()
    WP.x.landscape()

    return WP.plot_figure1(vars[0],vars[1])

def dofig2(parent,x,template, *vars):
    if len(vars)!=1:
        gui_message.error('Unable to plot data! You need to pass 1 variable')
        return
    WP = prepWKPlot(parent)
    WP.x.clear()
    WP.x.landscape()
    
    return WP.plot_figure2(vars[0])

def dofig3(parent,x,template, *vars):
    if len(vars)!=2:
        gui_message.error('Unable to plot data! You need to pass 2 variables')
        return
    WP = prepWKPlot(parent)
    WP.x.clear()
    WP.x.landscape()
    
    return WP.plot_figure3(vars[0],vars[1])
