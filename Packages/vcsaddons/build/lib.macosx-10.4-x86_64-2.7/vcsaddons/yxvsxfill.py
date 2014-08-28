from core import VCSaddon
import cdms2,MV2,vcs,vcsaddons,numpy

class Gyf(VCSaddon):
    def __init__(self,name=None,source='default',x=None,template = None):
        self.g_name='Gyf'
        VCSaddon.__init__(self,name,source,x,template)
        self.g_nslabs=2
        if source == 'default':
            self.fillareastyle = 'solid'
            self.fillareaindex = 1
            self.fillareacolor = 252
            self.line = 'solid'
            self.linewidth=1.0
            self.linecolor=241
        else:
            gm = vcsaddons.gms[self.g_name][source]
            self.fillareastyle= gm.fillareastyle
            self.fillareaindex = gm.fillareaindex
            self.fillareacolor = gm.fillareacolor
            self.line = gm.line
            self.linewidth = gm.linewidth
            self.linecolor = gm.linecolor
            

    def list(self):
        print '---------- Yxvsx Fill (Gyf) member (attribute) listings ----------'
        print 'Canvas Mode = ',self.x.mode
        VCSaddon.list(self)
        print 'fillareastyle = ', self.fillareastyle
        print 'fillareaindex = ', self.fillareaindex
        print 'fillareacolor = ', self.fillareacolor
        print 'line = ', self.line
        print 'linewidth = ', self.linewidth
        print 'linecolor = ', self.linecolor
        
    
    def plot(self,data,data2,template = None, bg=0, x=None):
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
        while data2.rank()>1:
            data2 = data2[0]

        # ok now we have a good x and a good data
        npts1 = len(data)
        npts2 = len(data2)

        # create the primitive
        fill = x.createfillarea()
        line = x.createline()
        fill.viewport = [template.data.x1,template.data.x2,template.data.y1,template.data.y2]
        line.viewport = [template.data.x1,template.data.x2,template.data.y1,template.data.y2]
        ax = data.getAxis(0)[:]
        ax2 = data.getAxis(0)[:]
        xmn,xmx = vcs.minmax(ax,ax2)
        ymn,ymx = vcs.minmax(data,data2)
        
        xmn,xmx,ymn,ymx = self.prep_plot(xmn,xmx,ymn,ymx)
        
        fill.worldcoordinate=[xmn,xmx,ymn,ymx]
        line.worldcoordinate=[xmn,xmx,ymn,ymx]
        
        fill.style = [self.fillareastyle,]
        fill.color = [self.fillareacolor,]
        fill.index = [self.fillareaindex,]

        line.type = [self.line,]
        line.width = [self.linewidth,]
        line.color = [self.linecolor,]
        
        xs = []
        ys = []
        

        xs = numpy.concatenate((ax[:],ax2[::-1])).tolist()
        ys = numpy.concatenate((data[:],data2[::-1])).tolist()

        xs.append(xs[0])
        ys.append(ys[0])
        
        fill.x = xs
        fill.y = ys

        line.x = xs
        line.y = ys

        
        displays = []
        displays.append(x.plot(fill,bg=bg))
        displays.append(x.plot(line,bg=bg))

        x.worldcoordinate = fill.worldcoordinate 

        dsp = template.plot(data,self,bg=bg)
        for d in dsp:
            displays.append(d)

        self.restore()
        return displays
