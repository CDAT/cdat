#!/usr/bin/env python
# Adapted for numpy/ma/cdms2 by convertcdms.py

import sys,string,vcs,cdms2 as cdms,copy,numpy


class Gcf:
    def __init__(self):
        self.name='default'
        self.g_name='Gcf'
        self.line='n'
        self.line_width=1
        self.line_color=241
        self.fill='y'
        self.fill_color=252
        self.datawc_x1 = -180.0
        self.datawc_x2 =  180.0
        self.datawc_y1 = - 90.0
        self.datawc_y2 =   90.0
        self.projection='linear'

    def readData(self):
        """This function reads the continents outline"""
        import worldmap
        xs=[]
        ys=[]
        for area in dir(worldmap):
##             print 'AREA:',area,type(area),area
            if area[:3]=='far':
                exec('area = worldmap.'+area)
                x=[]
                y=[]
                n=len(area)
                for i in range(0,n,2):
                    y.append(float(area[i])/100.)
                    x.append(float(area[i+1])/100.)
                    if i!=0:
                        if x[-1]<0. and x[-2]>150:
                            x[-1]=x[-1]+360.
                        
                xs.append(x)
                ys.append(y)
        return xs,ys

    def plot(self,x=None,template=None,bg=0,ratio=None):
        try:
            if x is None:
                x=vcs.init()
            if ratio is None:
                ratio=x.ratio
            if not template is None:
    ##             print 'you passed a template'
                if not vcs.istemplate(template):
    ##                 print 'Getting it from string'
                    t=x.gettemplate(template)
                else:
    ##                 print 'Getting it from object'
                    t=template
            else:
                t=x.gettemplate()
            ovp=x.viewport
            owc=copy.copy(x.worldcoordinate)
    ##         print 'OWC',owc
            wc=x.worldcoordinate
            if not (self.datawc_x1 is None) and self.datawc_x1!='*':
                wc[0]=self.datawc_x1
            if not (self.datawc_x2 is None) and self.datawc_x2!='*':
                wc[1]=self.datawc_x2
            if not (self.datawc_y1 is None) and self.datawc_y1!='*':
                wc[2]=self.datawc_y1
            if not (self.datawc_y2 is None) and self.datawc_y2!='*':
                wc[3]=self.datawc_y2
    ##         print 'Ratio:',x.ratio
            if ratio in ['auto','autot']:
                t.ratio_linear_projection(wc[0],wc[1],wc[2],wc[3])
            elif isinstance(ratio,str):
                if ratio[-1]=='t':
                    r=float(ratio[:-1])
                else:
                    r=float(ratio)
                t.ratio(r)
            elif ratio!=0:
                t.ratio(ratio)
    ##         t.data.list()
            vp = [ t.data.x1, t.data.x2, t.data.y1, t.data.y2 ]
            fa=x.createfillarea()
            li=x.createline()
            fa.projection=self.projection
            li.projection=self.projection
            fa.viewport=vp
            fa.worldcoordinate=wc
            fa.priority=t.data.priority+1
            li.worldcoordinate=wc
            li.viewport=vp
            li.priority=t.data.priority+1
           # Now plots the stuff
            xs,ys=self.readData()
    ##         xs=xs[15]
    ##         ys=ys[15]
            fa.x=xs
            fa.y=ys
            fa.color=self.fill_color
            fa.priority=t.data.priority+1
            li.x=xs
            li.y=ys
            li.color=self.line_color
            li.width=self.line_width
            li.priority=t.data.priority+1
            if self.fill=='y':
                x.plot(fa,bg=bg)
            if self.line=='y':
                x.plot(li,bg=bg)
            xs=fa.x
            for i in range(len(xs)):
                for j in range(len(xs[i])):
                    xs[i][j]=xs[i][j]-360.
            fa2=x.createfillarea()
            li2=x.createline()

            fa2.viewport=vp
            fa2.worldcoordinate=wc

            fa2.x=xs
            li2.x=xs
            if self.fill=='y':
                x.plot(fa2,bg=bg)
            if self.line=='y':
                x.plot(li2,bg=bg)
            xs=fa2.x
            for i in range(len(xs)):
                for j in range(len(xs[i])):
                    xs[i][j]=xs[i][j]+720.
            icont=1
            fa3=x.createfillarea(source = fa.name)
            li3=x.createline(source = li.name)
            fa3.viewport=vp
            fa3.worldcoordinate=wc
            fa3.x=xs
            li3.x=xs
            if self.fill=='y':
                x.plot(fa3,bg=bg)
            if self.line=='y':
                x.plot(li3,bg=bg)
    ##         x.viewport=ovp
    ##         x.worldcoordinate=owc
        except Exception,err:
            print 'Error in continents:',err

if __name__ == '__main__':
    import os,sys,cdutil,vcs,cdms2 as cdms
    fnm=os.path.join(sys.prefix,'sample_data','clt.nc')
    f=cdms.open(fnm)
    s=f('clt',0,longitude=(-180.,180.))
    x=vcs.init()
    iso=x.createisoline('new')
    iso.datawc_x1=-180.
    iso.datawc_x2=180.
    iso.datawc_y1=-90.
    iso.datawc_y2=90.
    t=x.createtemplate('new')
    t.data.priority=1
    t.legend.priority=0
    x.plot(s,t,iso,continents=0)
    co=cdutil.continent_fill.Gcf()
    co.line='y'
    co.line_width=1
    co.line_color=241
    co.fill='y'
    co.fill_color=252
    co.datawc_x1 = -180.0
    co.datawc_x2 =  180.0
    co.datawc_y1 = - 90.0
    co.datawc_y2 =   90.0
    co.projection='linear'
    co.plot(x)
    x.postscript('tmp.ps')
    sys.stdin.readline()
    



            
