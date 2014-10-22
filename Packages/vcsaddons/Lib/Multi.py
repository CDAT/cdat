import vcs,EzTemplate,random

def isNumber(value,min=None,max=None):
     """ Checks if value is a Number, optionaly can check if min<value<max
     """
     if not isinstance(value,(int,long,float)):
          return False
     if min is not None and value<min:
          return -1
     if max is not None and value>max:
          return -2
     return True
 
def checkNumber(name,value,minvalue=None,maxvalue=None):
     n=isNumber(value,min=minvalue,max=maxvalue)
     if n is False:
          raise ValueError, name+' must be a number'
     if n==-1:
          raise ValueError, name+' values must be at least '+str(minvalue)
     if n==-2:
          raise ValueError, name+' values must be at most '+str(maxvalue)
     return value

class Spacing(object):
    __slots__=['vertical','horizontal','_vertical','_horizontal']
    def __init__(self,horizontal,vertical):
        self.horizontal=horizontal
        self.vertical=vertical
    def _gethorizontal(self):
        return self._horizontal
    def _sethorizontal(self,value):
        value=checkNumber('horizontal',value,0.,1.)
        self._horizontal=value
    horizontal=property(_gethorizontal,_sethorizontal,None,"Horizontal Spacing")
    def _getvertical(self):
        return self._vertical
    def _setvertical(self,value):
        value=checkNumber('vertical',value,0.,1.)
        self._vertical=value
    vertical=property(_getvertical,_setvertical,None,"Vertical Spacing")
    def __str__(self):
        st='horizontal = '+str(self.horizontal)
        st+='\nvertical = '+str(self.vertical)
        return st

class Margins(object):

    __slots__=['top','bottom','left','right','_top','_bottom','_left','_right']

    def __str__(self):
        st='top = '+str(self.top)
        st+='\nbottom = '+str(self.bottom)
        st+='\nright = '+str(self.right)
        st+='\nleft = '+str(self.left)
        return st

    def __init__(self,top,bottom,left,right):
        self.right=right
        self.top=top
        self.bottom=bottom
        self.left=left
    def _gettop(self):
        return self._top
    def _settop(self,value):
        value=checkNumber('top',value,0.,1.)
        self._top=value
    top=property(_gettop,_settop,None,"Top Margin")
    def _getbottom(self):
        return self._bottom
    def _setbottom(self,value):
        value=checkNumber('bottom',value,0.,1.)
        self._bottom=value
    bottom=property(_getbottom,_setbottom,None,"Bottom Margin")
    def _getright(self):
        return self._right
    def _setright(self,value):
        value=checkNumber('right',value,0.,1.)
        self._right=value
    right=property(_getright,_setright,None,"Right Margin")
    def _getleft(self):
        return self._left
    def _setleft(self,value):
        value=checkNumber('left',value,0.,1.)
        self._left=value
    left=property(_getleft,_setleft,None,"Left Margin")

class Legend(object):
    
    __slots__= ['direction','fat','thickness','stretch','_direction','_fat','_thickness','_stretch']

    def __init__(self,direction,fat,thickness,stretch):
        self._direction=direction
        self._fat=fat
        self._thickness=thickness
        self._stretch=stretch
        
    def _getdirection(self):
        return self._direction
    def _setdirection(self,value):
        if not isinstance(value,str):
            raise ValueError, 'direction must be a string'
        if not value.lower() in ['vertical','horizontal']:
            raise ValueError, 'direction attribute must be either "vertical" or "horizontal"'
        self._direction=value
    direction=property(_getdirection,_setdirection,None,"Direction of the Legend (horizontal or vertical)")
    def _getthickness(self):
        return self._thickness
    def _setthickness(self,value):
        value=checkNumber('thickness',value,0.)
        self._thickness=value
    thickness=property(_getthickness,_setthickness,None,"Thickness")
    def _getfat(self):
        return self._fat
    def _setfat(self,value):
        value=checkNumber('fat',value,0.)
        self._fat=value
    fat=property(_getfat,_setfat,None,"Fat")
    def _getstretch(self):
        return self._stretch
    def _setstretch(self,value):
        value=checkNumber('stretch',value,0.)
        self._stretch=value
    stretch=property(_getstretch,_setstretch,None,"Stretch")

    def __str__(self):
        st='direction = '+self.direction
        st+='\nstretch = '+str(self.stretch)
        st+='\nthickness = '+str(self.thickness)
        st+='\nfat = '+str(self.fat)
        return st
            
    
class Multi(object):
    """
    Multi Plotter Object
    Allows easy creation of templates for multiple plots
    """
    
    __slots__ = ['rows',
                 'columns',
                 'margins',
                 'spacing',
                 'legend',
                 'x',
                 'template',
                 '_rows',
                 '_columns',
                 'margins',
                 'spacing',
                 'legend',
                 '_template',
                 'template_names',
                 'id',
                 ]

    def _getrows(self):
        return self._rows
    def _setrows(self,value):
        if not isinstance(value,(int,long)):
            raise ValueError, 'rows must be an integer'
        self._rows=value
    rows=property(_getrows,_setrows,None,"Number of Rows")
    def _getcolumns(self):
        return self._columns
    def _setcolumns(self,value):
        if not isinstance(value,(int,long)):
            raise ValueError, 'columns must be an integer'
        self._columns=value
    columns=property(_getcolumns,_setcolumns,None,"Number of Columns")
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
    
    
    def __init__(self,columns=2,rows=3,
                 template='default',
                 top_margin=.05,bottom_margin=.075,left_margin=.033,right_margin=.05,
                 horizontal_spacing=0.05,vertical_spacing=0.035,
                 legend_thickness=.2,legend_direction='horizontal',legend_stretch=.8,legend_fat=0.05,
		 x=None
                 ):
        """ Initialize the Object, need to pass number of row and columns (rows and columns)
        default: 2 cols, 3 rows
        """
        if hasattr(EzTemplate,'ids'):
             mx = len(EzTemplate.ids)+1000
             n = random.randint(0,mx)
             while n is EzTemplate.ids:
                  n = random.randint(0,mx)
             EzTemplate.ids.append(n)
             self.id=n
        else:
             EzTemplate.ids=[1,]
             self.id=1
        
        self._rows    = rows
        self._columns = columns

        self.margins = Margins(top_margin,bottom_margin,left_margin,right_margin)
        self.spacing = Spacing(horizontal_spacing,vertical_spacing)
        self.legend  = Legend(direction=legend_direction,fat=legend_fat,thickness=legend_thickness,stretch=legend_stretch)

        found=False
	if x is not None:
		self.x=x
	else:
          for obj in globals():
            if isinstance(obj,vcs.Canvas.Canvas):
                self.x=obj
                found=True
                break
          if found is False:
            self.x=vcs.init()
            
        self.template_names = []
        self.template=template

    def clean(self):
        for t in self.template_names:
            self.x.removeP(t)
        self.template_names=[]
        
    def get(self,column=None,row=None,legend=None,font=True,fontlimit=0.8):
        """
        Returns a template to use in vcs
        Usage:
        get(self,column=None,row=None,legend=None,font=1)
        Options
        column/row : Number of of the column/row you wish to use
                     if either one is None then it will start from the upper/left corner
                     and keep going (left then down) with each call
        legend : Type of legend for this specific one
                 local means the legend is added inside the space reserved for this plot
                 global or None means the legend is to be used for all plots on the page
        font :   True/False scale the fonts from "original" template
        fontlimit :   Will shrink the fonts by more than this value
        """
        if self.rows>self.columns:
            if self.x.orientation()=='landscape' :
                self.x.portrait()
        elif self.rows<self.columns:
            if self.x.orientation()!='landscape' : self.x.landscape()
        ## Location 0,0 is top/left
        if column is None or row is None:
             row,column=divmod(len(self.template_names),self.columns)

        ## Test for out of bound row/column
        if column >= self.columns:
             raise ValueError, 'You requested template for column %d but you defined %d columns only' % (column,self.columns)
        if row >= self.rows:
             raise ValueError, 'You requested template for row %d but you defined %d rows only' % (row,self.rows)
        
        xl=0 # offset for legend in x
        yl=0 # offset for legend in y
        nm = '%i_x_%i_%i' % (column,row,self.id)
        try:
            t=self.x.createtemplate(nm,source=self.template)
            self.template_names.append(t.name)
        except:
            t=self.x.gettemplate(nm)

        lg='global'
        if isinstance(legend,str):
            lg=legend.lower()
            if lg in ['single','local']:
                if self.legend.direction=='vertical':
                    xl=self.legend.fat
                else:
                    yl=self.legend.fat
        

        ## X width
        dx=(1.-self.margins.right-self.margins.left-(self.columns-1)*self.spacing.horizontal)/self.columns
        DX = t.data.x2-t.data.x1
        
        ## Start/End of xs (data area)
        x1=self.margins.left+column*(dx+self.spacing.horizontal)
        x2=x1+dx-xl
        t.reset('x',x1,x2,t.data.x1,t.data.x2)

        ## Y width
        dy=(1.-self.margins.top-self.margins.bottom-(self.rows-1)*self.spacing.vertical)/self.rows
        DY=t.data.y2-t.data.y1
        Ratio=DY/DX
        
        ## Start/End of ys (data area)
        y1=1.-self.margins.top-row*(dy+self.spacing.vertical)-dy+yl
        y2=y1+dy-yl
        t.reset('y',y1,y2,t.data.y1,t.data.y2)
        if font:
             sc = 1./(self.columns)
             if sc<fontlimit:
                  sc=fontlimit
             t.scalefont(sc)
        if lg in ['single','local']:
            t.legend.priority=1
            if self.legend.direction=='vertical':
                x1=t.data.x2
                x2=t.data.x2+xl
                y1=t.data.y1
                y2=t.data.y2
                dx=self.legend.thickness
                dy=self.legend.stretch
            else:
                x1=t.data.x1
                x2=t.data.x2
                y1=t.data.y1-yl
                y2=t.data.y1
                dx=self.legend.stretch
                dy=self.legend.thickness
        else:
            if column==0 and row==0  or legend is not None:
                t.legend.priority=1
            else:
                t.legend.priority=0
            if self.legend.direction=='vertical':
                x1=1.-self.margins.right
                x2=1.
                y1=self.margins.bottom
                y2=1.-self.margins.top
                dx=self.legend.thickness
                dy=self.legend.stretch
            else:
                x1=self.margins.left
                x2=1.-self.margins.right
                y1=0.
                y2=self.margins.bottom
                dx=self.legend.stretch
                dy=self.legend.thickness
##                 print x1,x2,y1,y2,dx,dy
        t.legend.x1=x1+(x2-x1)*(1.-dx)/2.
        t.legend.x2=x2-(x2-x1)*(1.-dx)/2.
        t.legend.y1=y1+(y2-y1)*(1.-dy)/2.
        t.legend.y2=y2-(y2-y1)*(1.-dy)/2.            
        return t

    def preview(self,out='EZTemplate_Multi',bg=1):
        """ Draws the layout for your setup"""
        n=self.rows*self.columns
        ir=0
        ic=0
        self.x.clear()
        if self.template_names==[]:
            tmpl=range(n)
        else:
            tmpl=self.template_names
        for i in tmpl:
            if tmpl==range(n):
                t=self.get(row=ir,column=ic)
                ic+=1
                if ic==self.columns :
                    ic=0
                    ir+=1
            else:
                t=self.x.gettemplate(i)
                sp=i.split('_x_')
                ic=int(sp[0])
                ir=int(sp[1].split('_')[0])
            l=self.x.createline('_EZTM'+str(i))
            l.x=[t.data.x1,t.data.x2,t.data.x2,t.data.x1,t.data.x1]
            l.y=[t.data.y1,t.data.y1,t.data.y2,t.data.y2,t.data.y1]
            l.type='dot'
            self.x.plot(l,bg=bg)
            txt=self.x.createtext('__EZTM'+str(i))
            txt.x=[(t.data.x1+t.data.x2)/2.]
            txt.y=[(t.data.y1+t.data.y2)/2.]
            txt.halign='center'
            txt.valign='half'
            txt.string=['C/R:'+str(ic)+'/'+str(ir)]
            self.x.plot(txt,bg=bg)
            l=self.x.createline('_EZTMleg'+str(i))
            l.x=[t.legend.x1,t.legend.x2,t.legend.x2,t.legend.x1,t.legend.x1]
            l.y=[t.legend.y1,t.legend.y1,t.legend.y2,t.legend.y2,t.legend.y1]
            l.type='dot'
            l.priority=t.legend.priority
            self.x.plot(l,bg=bg)
                
            
        self.x.postscript(out)
        self.x.pdf(out)
        self.x.png(out)
            
            
    
    def list(self):
        print self
        return
    
    def __str__(self):
        st =  '----------Template (P) member (attribute) listings ----------'
        st+='\nrows = '+str(self.rows)
        st+='\ncolumns = '+str(self.columns)
        st+='\ntemplate = '+self.template
        st+='\nmember = margins\n'+str(self.margins)
        st+='\nmember = spacing\n'+str(self.spacing)
        st+='\nmember = legend\n'+str(self.legend)
        return st
    
