## This is meant to ease creation of 1D plots for users
# it will create 1D templates, based on an original template
# and moves the legend according to the number of datasets
import vcs

class oneD():
    """
    ## This is meant to ease creation of 1D plots for users
    # it will create 1D templates, based on an original template
    # and moves the legend according to the number of datasets
    """
    
    __slots__ = ['x',
                 'template',
                 '_template',
                 'counter'
                 'x',
                 ]
    
    def _gettemplate(self):
        return self._template
    def _settemplate(self,value):
        if isinstance(value,vcs.template.P):
            self._template=value.name
        elif isinstance(value,str):
            if not value in self.x.listelements('template'):
                raise ValueError, value+' is not an existing template'
            self._template=value
        else:
            raise ValueError,'template must be an existing template name or a template'
    template = property(_gettemplate,_settemplate,None,"'model' vcs template to use for generating templates")
    def __init__(self,n=10,template='default'):
        """ creates a !D EzTemplate generator keywords are
        n : number of 1D line that will be plotted
        template: source template to start with
        """
        found=False
        self.counter=0
        self.n = n
        for obj in globals():
            if isinstance(obj,vcs.Canvas.Canvas):
                self.x=obj
                found=True
                break
        if found is False:
            self.x=vcs.init()
        self.template=template

    def clean(self):
        for t in self.template_names:
            self.x.removeP(t)
        self.template_names=[]
        
    def list(self):
        print self
        return
    
    def __str__(self):
        st =  '----------1D EzTemplate (attribute) listings ----------'
        st+='\ntemplate = '+self.template
        return st
    
    def get(self,n=None,orientation='vertical'):
        """ gets the template appropriate to plot the 'n'th line on a single template
        basically simply moves the legend
        keywords:
        n: nth line to be plotted (starts at 0)
        orientation: 'horizontal' or 'vertical' (default), how to space the legends between 1Ds
        return:
        a vcs template object
        """
        t=self.x.createtemplate(source=self.template)
        if orientation == 'vertical':
            delta = t.legend.y2-t.legend.y1 # no abs so we can data going up
        else:
            delta = t.legend.x2-t.legend.x1 # no abs so we can data going toward right

        if n is None:
            n = self.counter
        single = float(delta)/float(self.n+1.)
        if orientation == 'vertical':
            t.legend.y2 = t.legend.y2 - (n+1)*single 
            t.legend.y1 = t.legend.y2 -  single/10.
        else:
            t.legend.x1 = (n+1)*single + t.legend.x1

        self.counter+=1
        self.counter = self.counter % self.n
        return t
            
