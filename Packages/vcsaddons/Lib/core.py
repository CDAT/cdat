import vcsaddons,vcs
import numpy

class VCSaddon(object):
    def __init__(self,name=None,source='default',x=None,template=None):
        self._saves={}
        self.g_nslabs=1
        if not self.g_name in vcsaddons.gms.keys():
            vcsaddons.gms[self.g_name]={}
        if name is None:
            cont = True
            while cont:
                num= numpy.random.randint(1000)
                nm = 'Ghg_'+str(num)
                if not nm in vcsaddons.gms[self.g_name].keys():
                    name = nm
                    cont = False

        if x is None:
            self.x=vcs.init()
        else:
            self.x=x
            
        if template is None:
            self.template = self.x.gettemplate()
        elif isinstance(template,str):
            self.template = self.x.gettemplate(template)
        elif vcs.istemplate(template):
            self.template = template
        else:
            raise "Error did not know what to do with template: %s" % template

        if name in vcsaddons.gms[self.g_name].keys():
            raise "Error graphic method %s already exists" % name

        if source=='default':
            self.datawc_x1=1.e20
            self.datawc_x2=1.e20
            self.datawc_y1=1.e20
            self.datawc_y2=1.e20
            self.colormap="default"
            self.xmtics1='*'
            self.xmtics2='*'
            self.ymtics1='*'
            self.ymtics2='*'
            self.xticlabels1='*'
            self.xticlabels2='*'
            self.yticlabels1='*'
            self.yticlabels2='*'
            self.xaxisconvert= 'linear'
            self.yaxisconvert= 'linear'
            self.color_1 = 16
            self.color_2 = 239
            self.legend = None
            self.projection='linear'
        else:
            gm =  vcsaddons.gms[self.g_name].get(source,None)
            if gm is None:
                raise "error could not find graphic method %s (of type %s)" % (source, self.g_name)
            self.datawc_x1=gm.datawc_x1
            self.datawc_x2=gm.datawc_x2
            self.datawc_y1=gm.datawc_y1
            self.datawc_y2=gm.datawc_x2
            self.colormap=gm.colormap
            self.xmtics1=gm.xmtics1
            self.xmtics2=gm.xmtics2
            self.ymtics1=gm.ymtics1
            self.ymtics2=gm.ymtics2
            self.xticlabels1=gm.xticlabels1
            self.xticlabels2=gm.xticlabels2
            self.yticlabels1=gm.yticlabels1
            self.yticlabels2=gm.yticlabels2
            self.xaxisconvert=gm.xaxisconvert
            self.yaxisconvert= gm.yaxisconvert
            self.color_1 = gm.color_1
            self.color_2 = gm.color_2
            self.legend = gm.legend
            self.projection=gm.projection
        self.name = name
        vcsaddons.gms[self.g_name][name]=self
        

    def list(self):
        print 'graphics method = ',self.g_name
        print 'name = ',self.name
        print 'datawc_x1 = ',self.datawc_x1
        print 'datawc_x2 = ',self.datawc_x2
        print 'datawc_y1 = ',self.datawc_y1
        print 'datawc_y2 = ',self.datawc_y2
        print 'xmtics1 = ',self.xmtics1
        print 'xmtics2 = ',self.xmtics2
        print 'ymtics1 = ',self.ymtics1
        print 'ymtics2 = ',self.ymtics2
        print 'xticlabels1 = ',self.xticlabels1
        print 'xticlabels2 = ',self.xticlabels2
        print 'yticlabels1 = ',self.yticlabels1
        print 'yticlabels2 = ',self.yticlabels2
        print 'xaxisconvert = ',self.xaxisconvert
        print 'yaxisconvert = ',self.yaxisconvert
        print 'legend = ',self.legend

    def plot(self):
        raise "Plot function not implemented for graphic method type: %s" % self.g_name

    def prep_plot(self,xmn,xmx,ymn,ymx):
        
        self.save()
        
        if self.datawc_x1!=1.e20:
            xmn = self.datawc_x1
        if self.datawc_x2!=1.e20:
            xmx = self.datawc_x2
        if self.datawc_y1!=1.e20:
            ymn = self.datawc_y1
        if self.datawc_y2!=1.e20:
            ymx = self.datawc_y2

        self.datawc_x1=xmn
        self.datawc_x2=xmx
        self.datawc_y1=ymn
        self.datawc_y2=ymx

        for axes in ['x','y']:
            for sec in ['mtics','ticlabels']:
                for n in ['1','2']:
                    if getattr(self,axes+sec+n) == '*':
                        sc = vcs.mkscale(getattr(self,'datawc_'+axes+'1'),getattr(self,'datawc_'+axes+'2'))
                        setattr(self,axes+sec+n,vcs.mklabels(sc))
        return xmn,xmx,ymn,ymx

    def save(self,attribute = None):
        if attribute is not None:
            self._saves[attribute] = getattr(self,attribute)
        else:
            for att in ['datawc_x1', 'datawc_x2', 'datawc_y1', 'datawc_y2',
                        'xmtics1', 'xmtics2', 'ymtics1', 'ymtics2',
                        'xticlabels1', 'xticlabels2', 'yticlabels1', 'yticlabels2',
                        'xaxisconvert', 'yaxisconvert', 'legend']:
                self._saves[att] = getattr(self,att)

    def restore(self,cleanup=True):
        for att in self._saves.keys():
            setattr(self,att,self._saves[att])
        if cleanup:
            self._saves={}


    def getgm(self,source="default"):
        gm = None
        for nm in vcsaddons.gms[self.g_name].keys():
            if source == nm:
                return vcsaddons.gms[self.g_name][nm]

        if gm is None:
            raise "Could not find graphic method %s named: %s" % (self.g_type, source)

    def creategm(self,name,source='default'):
        return self.__init__(name,source=source,x=self.x,template=self.template)
        
