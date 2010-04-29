# Adapted for numpy/ma/cdms2 by convertcdms.py
## Wheeler Koladis Reproduction Package
import graphics, vcs, MV2, numpy

class WKPlot(object):
    
    def __init__(self,x=None,datawc_x1=-15,datawc_x2=15,datawc_y1=0,datawc_y2=.8,min=-1.4,max=2.,delta_isofill=.2,delta_isoline=.1,days_lines=(30,6,3,2,),H=[12.,25.,50.]):
        """
        Plotting class for WK data
        """
        if x is None:
            self.x=vcs.init()
        else:
            self.x=x
        self.min = min
        self.max = max
        self.delta_isoline = delta_isoline
        self.days_lines = days_lines
        self.H=H
        self.delta_isofill=delta_isofill
        self.datawc_x1=datawc_x1
        self.datawc_x2=datawc_x2
        self.datawc_y1=datawc_y1
        self.datawc_y2=datawc_y2

        return
    __slots__ = [
        'min',
        'max',
        'delta_isoline',
        'delta_isofill',
        'days_lines',
        'x',
        'H',
        'datawc_x1',
        'datawc_x2',
        'datawc_y1',
        'datawc_y2',
        ]
    
    def plot_figure1(self,S,A,bg=0,x=None,min=None,max=None,delta_isofill=None,delta_isoline=None,days_lines=None):

        title = 'Figure 1'
        
        displays=[] # store displays used

        if min is None:
            min = self.min
        if max is None:
            max = self.max
        if delta_isoline is None:
            delta_isoline = self.delta_isoline
        if days_lines is None:
            days_lines = self.days_lines
        if delta_isofill is None:
            delta_isofill = self.delta_isofill
        if x is None:
            x=self.x

        m =x.mode
        x.mode=1
        x.landscape()
        tmpl,tmplnoleg,isof,isol1,isol2=graphics.createTemplateandGM(x,min,max,delta_isofill,delta_isoline,days_lines,ntemplate=2)

        for gm in isof,isol1,isol2:
            gm.datawc_x1=self.datawc_x1
            gm.datawc_x2=self.datawc_x2
            gm.datawc_y1=self.datawc_y1
            gm.datawc_y2=self.datawc_y2
            
        tmpl2=x.createtemplate(source=tmpl.name)
        tmpl2.moveto(.54,.2)
        tmpl2noleg=x.createtemplate(source=tmpl2.name)
        tmpl2noleg.legend.priority=0

        for (sym,templ,templnoleg) in [(-1,tmpl,tmplnoleg),(1,tmpl2,tmpl2noleg)]:
            if sym==-1:
                power=A
            else:
                power=S
            id=power.id
            power=MV2.log10(power)
            power.id=id
            fq=power.getAxis(0)
            fq.id='Frequency (CPD)'
            w=power.getAxis(1)
            w.id='Westward     Zonal Wave Number     Eastward'

            displays.append(x.plot(power,isof,templ,bg=bg))
            displays.append(x.plot(power,isol1,templnoleg,bg=bg))
            displays.append(x.plot(power,isol2,templnoleg,bg=bg))
            
        tt=x.createtext()
        tt.x=[.5]
        tt.y=[.97]
        tt.height=25
        tt.halign='center'
        tt.string=[title,]
        displays.append(x.plot(tt,bg=bg))
        x.update()
        x.mode=m
        return displays
    

    def plot_figure2(self,bg_power,bg=0,x=None,min=-1.4,max=2.,delta_isofill=None,delta_isoline=.1,days_lines=(30,6,3,2,)):
        displays=[] # store displays used

        title = 'Background power'
        
        if min is None:
            min = self.min
        if max is None:
            max = self.max
        if delta_isoline is None:
            delta_isoline = self.delta_isoline
        if delta_isofill is None:
            delta_isofill = self.delta_isofill
        if days_lines is None:
            days_lines = self.days_lines
        if x is None:
            x=self.x
        orientation = 'portrait'
        
        x.portrait()
        tmpl,tmplnoleg,isof,isol1,isol2=graphics.createTemplateandGM(x,min,max,delta_isofill,delta_isoline,days_lines,orientation=orientation)
        for gm in isof,isol1,isol2:
            gm.datawc_x1=self.datawc_x1
            gm.datawc_x2=self.datawc_x2
            gm.datawc_y1=self.datawc_y1
            gm.datawc_y2=self.datawc_y2
        fq=bg_power.getAxis(0)
        fq.id='Frequency (CPD)'
        w=bg_power.getAxis(1)
        w.id='Westward     Zonal Wave Number     Eastward'

        displays.append(x.plot(bg_power,isof,tmpl,bg=bg))
        displays.append(x.plot(bg_power,isol1,tmplnoleg,bg=bg))
        displays.append(x.plot(bg_power,isol2,tmplnoleg,bg=bg))

        tt=x.createtext()
        tt.x=[.5]
        tt.y=[.97]
        tt.string=[title,]
        tt.halign='center'
        tt.height=25
        displays.append(x.plot(tt,bg=bg))
        
        return displays

    def plot_figure3(self,S,A,bg=0,x=None,min=None,max=None,delta_isofill=None,delta_isoline=None,days_lines=None,H=None):

        title = 'Figure 3'
        
        displays=[] # store displays used

        if min is None:
            min = self.min
        if max is None:
            max = self.max
        if delta_isoline is None:
            delta_isoline = self.delta_isoline
        if days_lines is None:
            days_lines = self.days_lines
        if H is None:
            H = self.H
        if delta_isofill is None:
            delta_isofill = self.delta_isofill
        if x is None:
            x=self.x
        m = x.mode
        x.mode=0
        x.landscape()
        tmpl,tmplnoleg,isof,isol1,isol2=graphics.createTemplateandGM(x,min,max,delta_isofill,delta_isoline,days_lines,ntemplate=2)

        for gm in isof,isol1,isol2:
            gm.datawc_x1=self.datawc_x1
            gm.datawc_x2=self.datawc_x2
            gm.datawc_y1=self.datawc_y1
            gm.datawc_y2=self.datawc_y2
        tmpl2=x.createtemplate(source=tmpl.name)
        tmpl2.moveto(.54,.2)
        tmpl2noleg=x.createtemplate(source=tmpl2.name)
        tmpl2noleg.legend.priority=0



        for (sym,templ,templnoleg) in [(-1,tmpl,tmplnoleg),(1,tmpl2,tmpl2noleg)]:
            if sym==-1:
                power = A
            else:
                power = S
            id=power.id
            
            power.id=id

            fq=power.getAxis(0)
            oidf=fq.id
            fq.id='Frequency (CPD)'
            w=power.getAxis(1)
            oidw=w.id
            w.id='Westward     Zonal Wave Number     Eastward'

            displays.append(x.plot(power,isof,templ,bg=bg))
            displays.append(x.plot(power,isol1,templnoleg,bg=bg))
            displays.append(x.plot(power,isol2,templnoleg,bg=bg))

            # Put back the original ids
            fq.id=oidf
            w.id=oidw

        ## Ok now the curves
        ## First the graphic stuff
        l=x.createline()
        l.width=2
        l.color=242

        yx=x.createyxvsx()
        yx.datawc_x1=isol1.datawc_x1
        yx.datawc_x2=isol1.datawc_x2
        yx.datawc_y1=isol1.datawc_y1
        yx.datawc_y2=isol1.datawc_y2
        yx.xticlabels1={}
        yx.yticlabels1={}
        yx.xticlabels2={}
        yx.yticlabels2={}
        yx.line=l
        yx.marker=1
        yx.markersize=1
        yx.markercolor=242

        ## Now the equations
        g=9.81
        lat=0.
        U=0.
        Un=0. # since Un=U*T/L
        ll=2.*numpy.pi*6.37E6*numpy.cos(abs(lat))
        Beta=2.*7.292E-5*numpy.cos(abs(lat))/6.37E6
        wvn=power.getAxis(1)[:] # get the wave number
        k=2*numpy.pi*wvn/ll
        kax=power.getAxis(1)

        tmplnoleg.dataname.priority=0
        tmpl2noleg.dataname.priority=0
        for h in H:
            T=1./numpy.ma.sqrt(Beta)*numpy.ma.power(g*h,.25)
            L=numpy.ma.power(g*h,.25)/numpy.ma.sqrt(Beta)
            ## Anti-Symetric
            ## First type
            t1=numpy.ma.sqrt(1.+(4*Beta)/(k*k*numpy.ma.sqrt(g*h)))
            t1=numpy.ma.masked_greater(t1,1.e30)
            t1b=k*numpy.ma.sqrt(g*h)*(1.+t1)/2.
            t1=k*numpy.ma.sqrt(g*h)*(1.-t1)/2.

            # second type
            t2=numpy.ma.sqrt(numpy.ma.sqrt(g*h)*Beta)

            ## MRG wave and IG n=0 waves
            MRGandIG0=numpy.ma.where(numpy.ma.equal(k,0),t2,t1)
            MRGandIG0=numpy.ma.where(numpy.ma.greater(k,0),t1b,MRGandIG0)
            MRGandIG0=converttofreq(MRGandIG0,kax)

            ## IG n=2 waves
            n=2
            dl=Beta*numpy.ma.sqrt(g*h)
            t3=numpy.ma.sqrt((2*n+1.)*dl+g*h*k*k)
            for i in range(5):
                t3=numpy.ma.sqrt((2*n+1.)*dl+(g*h)*k*k+g*h*Beta*k/t3)
            IG2=converttofreq(t3,kax)

            ## ER wave, n=1
            n=1
            t4=(Beta/numpy.ma.sqrt(g*h))*(2*n+1.)
            t4=-Beta*k/(k*k+t4)
            ER=numpy.ma.ones(k.shape,'f')
            ER=numpy.ma.masked_equal(ER,1)
            ER=numpy.ma.where(numpy.ma.less(k,0.),t4,ER)
            ER=converttofreq(ER,kax)

            ## Kelvin Wave
            KW=numpy.ma.array(k*numpy.ma.sqrt(g*h))
            KW=converttofreq(KW,kax)

            ## IG n=1
            n=1
            dl=Beta*numpy.ma.sqrt(g*h)
            t5=numpy.ma.sqrt((2*n+1.)*dl+g*h*k*k)
            for i in range(5):
                t5=numpy.ma.sqrt((2*n+1.)*dl+(g*h)*k*k+g*h*Beta*k/t5)
            IG=converttofreq(t5,kax)

            okid=kax.id
            kax.id='Westward     Zonal Wave Number     Eastward'    
            displays.append(x.plot(MRGandIG0,tmplnoleg,yx,bg=bg))
            displays.append(x.plot(IG2,tmplnoleg,yx,bg=bg))
            displays.append(x.plot(ER,tmpl2noleg,yx,bg=bg))
            displays.append(x.plot(KW,tmpl2noleg,yx,bg=bg))
            displays.append(x.plot(IG,tmpl2noleg,yx,bg=bg))
            kax.id=okid

            ## Now plot the labels....
            ## h
            t=x.createtext()
            t.string=[str(h)]
            xx=12
            t.x=[xx]
            t.y=[float(MRGandIG0(planetaryzonalwavenumber=(xx,xx,'ccb'),squeeze=1))]
            t.height=25
            t.worldcoordinate=[yx.datawc_x1,yx.datawc_x2,yx.datawc_y1,yx.datawc_y2]
            t.viewport=[tmpl.data.x1,tmpl.data.x2,tmpl.data.y1,tmpl.data.y2]
            t.priority=2
            t.color=242
            displays.append(x.plot(t,bg=bg))
            xx=0
            t=x.createtext(Tt_source=t.Tt_name,To_source=t.To_name)
            t.x=[xx]
            t.y=[float(IG2(planetaryzonalwavenumber=(xx,xx,'ccb'),squeeze=1))]
            displays.append(x.plot(t,bg=bg))
            if h==H[2]:
                xx=5
                t=x.createtext(Tt_source=t.Tt_name,To_source=t.To_name)
                t.worldcoordinate=[yx.datawc_x1,yx.datawc_x2,yx.datawc_y1,yx.datawc_y2]
                t.viewport=[tmpl.data.x1,tmpl.data.x2,tmpl.data.y1,tmpl.data.y2]
                t.x=[xx]
                t.y=[float(MRGandIG0(planetaryzonalwavenumber=(xx,xx,'ccb'),squeeze=1))]
                t.string=['n=0 EIG']
                displays.append(x.plot(t,bg=bg))
            if h==H[1]:
                xx=-12
                t=x.createtext(Tt_source=t.Tt_name,To_source=t.To_name)
                t.worldcoordinate=[yx.datawc_x1,yx.datawc_x2,yx.datawc_y1,yx.datawc_y2]
                t.viewport=[tmpl.data.x1,tmpl.data.x2,tmpl.data.y1,tmpl.data.y2]
                t.x=[xx]
                t.y=[float(IG2(planetaryzonalwavenumber=(xx,xx,'ccb'),squeeze=1))]
                t.string=['n=2 WIG']
                displays.append(x.plot(t,bg=bg))
                xx=6
                t=x.createtext(Tt_source=t.Tt_name,To_source=t.To_name)
                t.worldcoordinate=[yx.datawc_x1,yx.datawc_x2,yx.datawc_y1,yx.datawc_y2]
                t.viewport=[tmpl.data.x1,tmpl.data.x2,tmpl.data.y1,tmpl.data.y2]
                t.x=[xx]
                t.y=[float(IG2(planetaryzonalwavenumber=(xx,xx,'ccb'),squeeze=1))]
                t.string=['n=2 EIG']
                displays.append(x.plot(t,bg=bg))
                xx=-8
                t.worldcoordinate=[yx.datawc_x1,yx.datawc_x2,yx.datawc_y1,yx.datawc_y2]
                t.viewport=[tmpl.data.x1,tmpl.data.x2,tmpl.data.y1,tmpl.data.y2]
                t=x.createtext(Tt_source=t.Tt_name,To_source=t.To_name)
                t.x=[xx]
                t.y=[float(MRGandIG0(planetaryzonalwavenumber=(xx,xx,'ccb'),squeeze=1))]
                t.string=['MRG']
                displays.append(x.plot(t,bg=bg))

            ## Ok now the string are in the second template
            xx=0
            t=x.createtext(Tt_source=t.Tt_name,To_source=t.To_name)
            t.worldcoordinate=[yx.datawc_x1,yx.datawc_x2,yx.datawc_y1,yx.datawc_y2]
            t.viewport=[tmpl2.data.x1,tmpl2.data.x2,tmpl2.data.y1,tmpl2.data.y2]
            t.x=[xx,]
            t.y=[float(IG(planetaryzonalwavenumber=(xx,xx,'ccb'),squeeze=1))]
            t.string=[str(h),]
            displays.append(x.plot(t,bg=bg))
            if h == H[0]:
                xx=12
            elif h ==H[1]:
                xx=9
            else:
                xx=7
            t=x.createtext(Tt_source=t.Tt_name,To_source=t.To_name)
            t.worldcoordinate=[yx.datawc_x1,yx.datawc_x2,yx.datawc_y1,yx.datawc_y2]
            t.viewport=[tmpl2.data.x1,tmpl2.data.x2,tmpl2.data.y1,tmpl2.data.y2]
            t.x=[xx]
            t.y=[float(KW(planetaryzonalwavenumber=(xx,xx,'ccb'),squeeze=1))]
            displays.append(x.plot(t,bg=bg))
            if h==H[0]:
                xx=3
                t=x.createtext(Tt_source=t.Tt_name,To_source=t.To_name)
                t.worldcoordinate=[yx.datawc_x1,yx.datawc_x2,yx.datawc_y1,yx.datawc_y2]
                t.viewport=[tmpl2.data.x1,tmpl2.data.x2,tmpl2.data.y1,tmpl2.data.y2]
                t.x=[xx]
                t.y=[float(KW(planetaryzonalwavenumber=(xx,xx,'ccb'),squeeze=1))]
                t.string=['  MJO']
                displays.append(x.plot(t,bg=bg))
                xx=7
                t=x.createtext(Tt_source=t.Tt_name,To_source=t.To_name)
                t.worldcoordinate=[yx.datawc_x1,yx.datawc_x2,yx.datawc_y1,yx.datawc_y2]
                t.viewport=[tmpl2.data.x1,tmpl2.data.x2,tmpl2.data.y1,tmpl2.data.y2]
                t.x=[xx]
                t.y=[float(KW(planetaryzonalwavenumber=(xx,xx,'ccb'),squeeze=1))]
                t.string=['Kelvin']
                displays.append(x.plot(t,bg=bg))
            if h==H[1]:
                xx=-13
                t=x.createtext(Tt_source=t.Tt_name,To_source=t.To_name)
                t.worldcoordinate=[yx.datawc_x1,yx.datawc_x2,yx.datawc_y1,yx.datawc_y2]
                t.viewport=[tmpl2.data.x1,tmpl2.data.x2,tmpl2.data.y1,tmpl2.data.y2]
                t.x=[xx]
                t.y=[float(IG(planetaryzonalwavenumber=(xx,xx,'ccb'),squeeze=1))]
                t.string=['n=1 WIG']
                displays.append(x.plot(t,bg=bg))
                xx=6
                t=x.createtext(Tt_source=t.Tt_name,To_source=t.To_name)
                t.worldcoordinate=[yx.datawc_x1,yx.datawc_x2,yx.datawc_y1,yx.datawc_y2]
                t.viewport=[tmpl2.data.x1,tmpl2.data.x2,tmpl2.data.y1,tmpl2.data.y2]
                t.x=[xx]
                t.y=[float(IG(planetaryzonalwavenumber=(xx,xx,'ccb'),squeeze=1))]
                t.string=['n=1 EIG']
                displays.append(x.plot(t,bg=bg))
            if h==H[-1]:
                xx=-12
                t=x.createtext(Tt_source=t.Tt_name,To_source=t.To_name)
                t.worldcoordinate=[yx.datawc_x1,yx.datawc_x2,yx.datawc_y1,yx.datawc_y2]
                t.viewport=[tmpl2.data.x1,tmpl2.data.x2,tmpl2.data.y1,tmpl2.data.y2]
                t.x=[xx]
                t.y=[float(ER(planetaryzonalwavenumber=(xx,xx,'ccb'),squeeze=1))+.02]
                t.string=['n=1 ER']
                displays.append(x.plot(t,bg=bg))


        tt=x.createtext()
        tt.x=[.5]
        tt.y=[.97]
        tt.halign='center'
        tt.height=25
        tt.string=[title,]
        displays.append(x.plot(tt,bg=bg))
        x.mode=m
        x.update()
        return displays
    
def converttofreq(data,kax):
    P=2*numpy.pi/(data*24*60*60)
    P=MV2.array(1./P)
    P.setAxis(0,kax)
    return P
