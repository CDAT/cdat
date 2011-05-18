class StringConstructor:
    """ This Class aims at spotting keyword in string and replacing them
    Usage
    Filler=StringConstructor(template)
    or
    Filler=StringConstructor()
    Filler.template=template

    template is a string of form
    template = 'my string here with %(keywords) in it'

    You can have has many 'keyword' as you want, and use them as many times as you want
    keywords are delimited on the left by %( and ) on the right
    
    In order to construct the string (i.e. replace keywrods with some values:

    str=Filler(keyword='kwstr')
    or
    Filler.keyword='kwstr'
    str=Filler()

    Example:
        structure='/pcmdi/amip/mo/%(variable)/%(model)/%(variable)_%(model).xml'
        Filler=StringConstructor(structure)
        Filler.variable='tas'
        myfilename=Filler.construct(structure,model='ugamp-98a')

        print myfilename # '/pcmdi/amip/mo/tas/ugamp-98a/tas_ugamp-98a.xml'
    
    """
    def __init__(self,template=None):
        self.template=template
        ## ok we need to generate the keys and set them to empty it seems like a better idea
        keys = self.keys()
        for k in keys:
            setattr(self,k,"")

    def keys(self,template=None):
        if template is None:
            template=self.template
        if template is None:
            return []
##         # First sets all the keyword values passed
##         for k in kw.keys():
##             setattr(self,k,kw[k])
        # Now determine the keywords in the template:
        end=0
        s2=template.split('%(')
        keys=[]
        for k in s2:
            sp=k.split(')')
            i=len(sp[0])
            if len(k)>i:
                if k[i]==')' and (not sp[0] in  keys):
                    keys.append(sp[0])
        return keys

    def construct(self,template=None,**kw):
        """
        construct, accepts a string with a unlimited number of keyword to replace
        keyword to replace must be in the format %(keyword) within the string
        keyword value are either passed as keyword to the construct function or preset
        Example:
        structure='/pcmdi/amip/mo/%(variable)/%(model)/%(variable)_%(model).xml'
        Filler=StringConstructor()
        Filler.variable='tas'
        myfilename=Filler.construct(structure,model='ugamp-98a')

        print myfilename
        """
        if template is None:
            template=self.template
        # Now determine the keywords in the template:
        keys = self.keys()
        # Now replace the keywords with their values
        for k in keys:
               template=template.replace('%('+k+')',kw.get(k,getattr(self,k,'')))
##             cmd='template=string.replace(template,\'%('+k+')\',self.'+k+')'
##             exec(cmd)
        return template

    def reverse(self,name,debug=False):
        out={}
        template = self.template
        for k in self.keys():
            sp=template.split("%%(%s)" % k)
            n = len(sp)
            i1=name.find(sp[0])+len(sp[0])
            j1=sp[1].find("%(")
            if j1==-1:
                if sp[1]=="":
                    val=name[i1:]
                else:
                    i2=name.find(sp[1])
                    val = name[i1:i2]
                if debug:
                    print k,j1,sp[1],"****",sp
                    print k,name[i1:i2]
                    print k,i1,i2,val
            else:
                i2=name[i1:].find(sp[1][:j1])
                val=name[i1:i1+i2]
                if debug:
                    print k,j1,sp[1][:j1]
                    print k,name[i1:]
                    print k,i1,i2,val
            if debug:
                print '-----------------'
            template=template.replace("%%(%s)"%k,val)
            if debug:
                print template
            out[k]=val
        if debug:
            print out
        if self.construct(self.template,**out)!=name:
            raise "Invalid pattern sent"
        return out
    
    def __call__(self,*args,**kw):
        """default call is construct function"""
        return self.construct(*args,**kw)
    
Filler=StringConstructor()

if __name__=='__main__':
    Filler.variable='tas'
    structure='/pcmdi/amip/mo/%(variable)/%(model)/%(variable)_%(model).xml'
    myfilename=Filler.construct(structure,model='*')    
    print myfilename
