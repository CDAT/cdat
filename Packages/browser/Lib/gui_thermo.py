import thermo
import gui_message
import Pmw,Tkinter
import gui_control
import gui_wk
import gui_user_menus
import gui_busy

def es(self,parent):
    gui_busy.busyStart( self, parent )
    vars = gui_wk.get_vars(parent)
    doc = thermo.Es.__doc__
    if len(vars)!=1:
        gui_message.error('This computation requires one variable only\n%s' % doc)
        gui_busy.busyEnd( self, parent )
        return
    try:
        es =thermo.Es(vars[0])
        gui_user_menus.user_menus_put( "saturated_pressure_from_%s" % vars[0].id,es)
    except Exception,err :
        gui_message.error('Error computing Es:\n%s\n%s' % (err,doc))
        gui_busy.busyEnd( self, parent )
        return
    gui_busy.busyEnd( self, parent )

def ws(self,parent):
    gui_busy.busyStart( self, parent )
    vars = gui_wk.get_vars(parent)
    doc = thermo.Ws.__doc__
    if len(vars)!=2:
        gui_message.error('This computation requires 2 variables\n%s' % doc)
        gui_busy.busyEnd( self, parent )
        return
    try:
        es =thermo.Ws(vars[0],vars[1])
        gui_user_menus.user_menus_put( "saturated_mixing_ratio_from_%s_and_%s" % (vars[0].id,vars[1].id),es)
    except Exception,err:
        gui_message.error('Error computing Ws:\n%s\n%s' % (err,doc))
        gui_busy.busyEnd( self, parent )
        return
    gui_busy.busyEnd( self, parent )

def lapse(self,parent):
    gui_busy.busyStart( self, parent )
    vars = gui_wk.get_vars(parent)
    doc = thermo.gammaw.__doc__
    if len(vars)!=3:
        gui_message.error('This computation requires 3 variables\n%s' % doc)
        gui_busy.busyEnd( self, parent )
        return
    try:
        es = thermo.gammaw(vars[0],vars[1],vars[2])
        gui_user_menus.user_menus_put( "moist_adiabatic_lapse_rate_from_%s_%s_%s" % (vars[0].id,vars[1].id,vars[2].id),es)
    except Exception,err:
        gui_message.error('Error computing lapse rate:\n%s\n%s' % (err,doc))
        gui_busy.busyEnd( self, parent )
        return
    gui_busy.busyEnd( self, parent )

def lift(self,parent):
    gui_busy.busyStart( self, parent )
    vars = gui_wk.get_vars(parent)
    doc = thermo.gammaw.__doc__
    if len(vars)!=3:
        gui_message.error('This computation requires 3 variables\n%s' % doc)
        gui_busy.busyEnd( self, parent )
        return
    try:
        T,P = thermo.LiftWet(vars[0],vars[1],vars[2])
        gui_user_menus.user_menus_put( "%s_lifted_moist_adiabatically_from_%s_to_%s" % (vars[0].id,vars[1].id,vars[2].id),T)
        gui_user_menus.user_menus_put( "%s_lifted_moist_adiabatically_from_%s_to_%s_pressures" % (vars[0].id,vars[1].id,vars[2].id),P)
    except Exception,err:
        gui_message.error('Error computing lapse rate:\n%s\n%s' % (err,doc))
        gui_busy.busyEnd( self, parent )
        return
    gui_busy.busyEnd( self, parent )
        
def dewpoint(self,parent):
    gui_busy.busyStart( self, parent )
    vars = gui_wk.get_vars(parent)
    doc = thermo.Dewpoint.__doc__
    if len(vars)!=2:
        gui_message.error('This computation requires 2 variables\n%s' % doc)
        gui_busy.busyEnd( self, parent )
        return
    try:
        es = thermo.Dewpoint(vars[0],vars[1])
        gui_user_menus.user_menus_put( "dewpoint_from_%s_%s" % (vars[0].id,vars[1].id),es)
    except Exception,err:
        gui_message.error('Error computing Dewpoint:\n%s\n%s' % (err,doc))
        gui_busy.busyEnd( self, parent )
        return
    gui_busy.busyEnd( self, parent )
        
def doskewT(parent,x,template, *vars):
    if len(vars)!=1:
        gui_message.error('Unable to plot data! You need to pass 1 variable only')
        return
    return aThermo(parent=parent,template=template,type="skewT",vars=vars)

def doskewTWind(parent,x,template, *vars):
    if len(vars)!=3:
        gui_message.error('Unable to plot data! You need to pass 3 variables')
        return
    return aThermo(parent=parent,template=template,type="skewT",vars=vars)

def doemagram(parent,x,template, *vars):
    if len(vars)!=1:
        gui_message.error('Unable to plot data! You need to pass 1 variable only')
        return
    return aThermo(parent=parent,template=template,type="emagram", vars=vars)
def doemagramWind(parent,x,template, *vars):
    if len(vars)!=3:
        gui_message.error('Unable to plot data! You need to pass 3 variables')
        return
    return aThermo(parent=parent,template=template,type="emagram", vars=vars)
      
def dotephigram(parent,x,template, *vars):
    if len(vars)!=1:
        gui_message.error('Unable to plot data! You need to pass 1 variable only')
        return
    return aThermo(parent=parent,template=template,type="tephigram", vars=vars)
def dotephigramWind(parent,x,template, *vars):
    if len(vars)!=3:
        gui_message.error('Unable to plot data! You need to pass 3 variables')
        return
    return aThermo(parent=parent,template=template,type="tephigram", vars=vars)
    
def dostuve(parent,x,template, *vars):
    if len(vars)!=1:
        gui_message.error('Unable to plot data! You need to pass 1 variable only')
        return
    return aThermo(parent=parent,template=template,type="stuve", vars=vars)
def dostuveWind(parent,x,template, *vars):
    if len(vars)!=3:
        gui_message.error('Unable to plot data! You need to pass 3 variables')
        return
    return aThermo(parent=parent,template=template,type="stuve", vars=vars)

def skewT(self,parent):
    parent.external_plot = doskewT
    v = parent.panelGC.opt2.getvalue()
    items =  parent.panelGC.opt2.cget('items')
    items[-1]='Ext (%s)' % 'skewT'
    parent.panelGC.opt2.setitems(items)
    parent.panelGC.opt2.invoke(items[-1])
    parent.panelGC.evt_plot(parent)

def skewTWind(self,parent):
    parent.external_plot = doskewTWind
    v = parent.panelGC.opt2.getvalue()
    items =  parent.panelGC.opt2.cget('items')
    items[-1]='Ext (%s)' % 'skewT_W'
    parent.panelGC.opt2.setitems(items)
    parent.panelGC.opt2.invoke(items[-1])
    parent.panelGC.evt_plot(parent)
    
def emagram(self,parent):
    parent.external_plot = doemagram
    v = parent.panelGC.opt2.getvalue()
    items =  parent.panelGC.opt2.cget('items')
    items[-1]='Ext (%s)' % 'emagr'
    parent.panelGC.opt2.setitems(items)
    parent.panelGC.opt2.invoke(items[-1])
    parent.panelGC.evt_plot(parent)
    
def emagramWind(self,parent):
    parent.external_plot = doemagramWind
    v = parent.panelGC.opt2.getvalue()
    items =  parent.panelGC.opt2.cget('items')
    items[-1]='Ext (%s)' % 'ema_W'
    parent.panelGC.opt2.setitems(items)
    parent.panelGC.opt2.invoke(items[-1])
    parent.panelGC.evt_plot(parent)
  
def tephigram(self,parent):
    parent.external_plot = dotephigram
    v = parent.panelGC.opt2.getvalue()
    items =  parent.panelGC.opt2.cget('items')
    items[-1]='Ext (%s)' % 'tephi'
    parent.panelGC.opt2.setitems(items)
    parent.panelGC.opt2.invoke(items[-1])
    parent.panelGC.evt_plot(parent)
    
def tephigramWind(self,parent):
    parent.external_plot = dotephigramWind
    v = parent.panelGC.opt2.getvalue()
    items =  parent.panelGC.opt2.cget('items')
    items[-1]='Ext (%s)' % 'tephW'
    parent.panelGC.opt2.setitems(items)
    parent.panelGC.opt2.invoke(items[-1])
    parent.panelGC.evt_plot(parent)

def stuve(self,parent):
    parent.external_plot = dostuve
    v = parent.panelGC.opt2.getvalue()
    items =  parent.panelGC.opt2.cget('items')
    items[-1]='Ext (%s)' % 'stuve'
    parent.panelGC.opt2.setitems(items)
    parent.panelGC.opt2.invoke(items[-1])
    parent.panelGC.evt_plot(parent)
def stuveWind(self,parent):
    parent.external_plot = dostuveWind
    v = parent.panelGC.opt2.getvalue()
    items =  parent.panelGC.opt2.cget('items')
    items[-1]='Ext (%s)' % 'stuvW'
    parent.panelGC.opt2.setitems(items)
    parent.panelGC.opt2.invoke(items[-1])
    parent.panelGC.evt_plot(parent)

def aThermo(parent,template,type,vars):
    th = thermo.Gth(x=parent.vcs[ parent.vcs_id ],name="my")
    ot = th.x.gettemplate(template)
    
    if not hasattr(parent,'thermo_profiles_count'):
        parent.thermo_profiles_count = 0
    if parent.panelGC.var_overlay.get():
        parent.thermo_profiles_count+=1
    else:
        parent.thermo_profiles_count = 0

    if len(vars)==1 or len(vars)>2:
        th.type=type

        # createtemplate
        template=th.x.createtemplate(source=ot)
        ## World Coordinates
        ## Temperatures at the bottom of the grap (in C)
        goportrait = True
        if parent.menu.main_menu.thermo_options.get():
            try:
                dialog = Pmw.Dialog(parent,
                                    title = "Options for Thermodynamic Diagram",
                                    buttons = ('OK',),
##                                     defaultbutton = 'Cancel',
##                                     command = gui_control.Command(thermo_options, parent)
                                    )
                f = Tkinter.Frame(dialog.interior())
                f.pack(expand=1,fill='both')

                dialog.portrait =  Pmw.RadioSelect(f,
                                                   labelpos='w',
                                                   buttontype='checkbutton',
                                                   orient='vertical',
                                                   label_text='Preserve Orientation',
                                                   hull_borderwidth = 2,
                                                   hull_relief = 'ridge',
                                                   )
                dialog.portrait.pack(side='top',fill='x',expand=1)
                for text in ('(no auto-portrait)',):
                    dialog.portrait.add(text)
                    
                dialog.x1 = Pmw.EntryField(f,
                                           label_text='Temp. Left',
                                           labelpos='w',
                                           value = '-50.',
                                           validate={'validator':'real'},
                                           )
                dialog.x1.pack(side='top',fill='x',expand=1)
                dialog.x2 = Pmw.EntryField(f,
                                           label_text='Temp. Right',
                                           labelpos='w',
                                           validate={'validator':'real'},
                                           value = '50.',
                                           )
                dialog.x2.pack(side='top',fill='x',expand=1)
                dialog.y1 = Pmw.EntryField(f,
                                           label_text='Pressure Bottom',
                                           labelpos='w',
                                           value = '1050.',
                                           validate={'validator':'real'},
                                           )
                dialog.y1.pack(side='top',fill='x',expand=1)
                dialog.y2 = Pmw.EntryField(f,
                                           label_text='Pressure Top',
                                           labelpos='w',
                                           validate={'validator':'real'},
                                           value = '100.',
                                           )
                dialog.y2.pack(side='top',fill='x',expand=1)

                dialog.draw = Pmw.RadioSelect(f,
                                              labelpos='w',
                                              buttontype='checkbutton',
                                              orient='vertical',
                                              label_text='Do you want to draw:',
                                              hull_borderwidth = 2,
                                              hull_relief = 'ridge',
                                              )
                dialog.draw.pack(side='top',fill='x',expand=1)
                for text in ('Filled Isotherms','Isotherms','Isobars','Dry Adiabatics','Pseudo Adiabatics','Mixing Ratio'):
                    dialog.draw.add(text)
                    dialog.draw.invoke(text)
                
                dialog.transient( parent ) # Keep widget on top of its parent
                results = dialog.activate()

                x1 = dialog.x1.get()
                x2 = dialog.x2.get()
                y1 = dialog.y1.get()
                y2 = dialog.y2.get()
                
                draws = dialog.draw.getvalue()
                
                d = [0,]*dialog.draw.numbuttons()
                for i in range(dialog.draw.numbuttons()):
                    b = dialog.draw.button(i)
                    nm =  b.cget("text")
                    if nm in draws:
                        d[i]=1



                if len(dialog.portrait.getvalue())!=0:
                    goportrait = False
                th.datawc_x1=float(x1)
                th.datawc_x2=float(x2)
                th.datawc_y1=float(y1)
                th.datawc_y2=float(y2)
                th.drawisothermsfilled=d[0]
                th.drawisotherms=d[1]
                th.drawisobars=d[2]
                th.drawdryadiabats=d[3]
                th.drawpseudoadiabats=d[4]
                th.drawmixingratio=d[5]
                dialog.destroy()
                
            except Exception,err:
                print err
        else:
            th.datawc_x1=-50.
            th.datawc_x2=50.
            th.datawc_y1=1050.
            th.datawc_y2=100.

        th.line.color = [241 + parent.thermo_profiles_count,]
        if goportrait and th.x.orientation()=='landscape':
            th.x.portrait()
            
        if len(vars)>2:
            template.reset('x',ot.data.x1,ot.data.x1+(ot.data.x2-ot.data.x1)*.95)
        #template.reset('y',.1,.9)

        th.plot_TP(vars[0],template=template)

    ## Gets variables for windbarbs
    if len(vars) == 2:
        u,v = vars
    elif len(vars)>2:
        u,v=vars[1],vars[2]
    else:
        u = None

    if u is not None:
        template=th.x.createtemplate(source=ot)
        if len(vars)>2:
            template.reset('x',ot.data.x1+(ot.data.x2-ot.data.x1)*.96,ot.data.x2)
        #template.reset('y',.1,.9)
        th.plot_windbarb(u,v,template=template)

    return th.displays
