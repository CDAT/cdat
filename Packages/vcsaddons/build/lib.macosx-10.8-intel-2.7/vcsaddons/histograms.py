from core import VCSaddon
import cdms2,MV2,vcs,vcsaddons

class Ghg(VCSaddon):
    def __init__(self,name=None,source='default',x=None,template = None):
        self.g_name='Ghg'
        self.g_type='histogram'
        VCSaddon.__init__(self,name,source,x,template)
        if source == 'default':
            self.fillareastyles = ['solid',]
            self.fillareaindices = [1,]
            self.fillareacolors = [252,]
            self.line = ['solid',]
            self.linewidth=[1.0,]
            self.linecolors=[241,]
        else:
            gm = vcsaddons.gms[self.g_name][source]
            self.fillareastyle= gm.fillareastyles
            self.fillareaindices = gm.fillareaindices
            self.fillareacolors = gm.fillareacolors
            self.line = gm.line
            self.linewidth = gm.linewidth
            self.linecolors = gm.linecolors
            

    def list(self):
        print '---------- Histogram (Ghg) member (attribute) listings ----------'
        print 'Canvas Mode = ',self.x.mode
        VCSaddon.list(self)
        print 'fillareastyles = ', self.fillareastyles
        print 'fillareaindices = ', self.fillareaindices
        print 'fillareacolors = ', self.fillareacolors
        print 'line = ', self.line
        print 'linewidth = ', self.linewidth
        print 'linecolors = ', self.linecolors
        
    
    def plot(self,data,template = None, bg=0, x=None):
        if x is None:
            x = self.x
        if template is None:
            template = self.template
        elif isinstance(template,str):
            template = x.gettemplate(template)
        elif not vcs.istemplate(template):
            raise "Error did not know what to do with template: %s" % template
        
        if not isinstance(data,cdms2.tvariable.TransientVariable):
            mode= cdms2.getAutoBounds()
            cdms2.setAutoBounds("on")
            data = MV2.array(data)
            data.getAxis(-1).getBounds()
            cdms2.setAutoBounds(mode)

        while data.rank()>1:
            data = data[0]

        # ok now we have a good x and a good data
        nbars = len(data)

        # create the primitive
        fill = x.createfillarea()
        line = x.createline()
        fill.viewport = [template.data.x1,template.data.x2,template.data.y1,template.data.y2]
        line.viewport = [template.data.x1,template.data.x2,template.data.y1,template.data.y2]
        axb = data.getAxis(0).getBounds()
        xmn,xmx = vcs.minmax(axb)
        ymn,ymx = vcs.minmax(data)
        
        xmn,xmx,ymn,ymx = self.prep_plot(xmn,xmx,ymn,ymx)
        
        fill.worldcoordinate=[xmn,xmx,ymn,ymx]
        line.worldcoordinate=[xmn,xmx,ymn,ymx]
        
        styles =[]
        cols = []
        indices = []
        lt = []
        lw =[]
        lc = []
        xs = []
        ys = []
        

        for i in range(nbars):
            if i < len(self.fillareastyles):
                styles.append(self.fillareastyles[i])
            else:
                styles.append(self.fillareastyles[-1])
            if i < len(self.fillareacolors):
                cols.append(self.fillareacolors[i])
            else:
                cols.append(self.fillareacolors[-1])
            if i < len(self.fillareaindices):
                indices.append(self.fillareaindices[i])
            else:
                indices.append(self.fillareaindices[-1])
            if i < len(self.line):
                lt.append( self.line[i])
            else:
                lt.append(self.line[-1])
            if i < len(self.linewidth):
                lw.append( self.linewidth[i])
            else:
                lw.append(self.linewidth[-1])
            if i < len(self.line):
                lc.append( self.linecolors[i])
            else:
                lc.append(self.linecolors[-1])
            
            xs.append( [axb[i][0],axb[i][1],axb[i][1],axb[i][0],axb[i][0]])
            ys.append( [0,0,data[i],data[i],0])


        fill.style = styles
        fill.x = xs
        fill.y = ys
        fill.style
        fill.index = indices
        fill.color = cols
        line.x = xs
        line.y = ys
        line.type = lt
        line.width = lw
        line.color = lc

        displays = []
        displays.append(x.plot(fill,bg=bg))
        displays.append(x.plot(line,bg=bg))

        x.worldcoordinate = fill.worldcoordinate 
        dsp = template.plot(data,self,bg=bg)
        for d in dsp:
            displays.append(d)

        self.restore()
        return displays
