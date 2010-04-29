#!/usr/bin/env python
# Adapted for numpy/ma/cdms2 by convertcdms.py


import MV2,vcs,sys,genutil,numpy

def createTemplateandGM(x,min,max,deltaisof,deltaisol,days_line,ntemplate=1,orientation='landscape'):

##     x.scriptrun('resol_24.scr')
##     x.setcolormap('cmap')
    dot=x.createline()
    dot.type='dot'
    if orientation[0].lower()=='p':
        x.portrait()
    tmpl=x.createtemplate()

    ## Turns off everything
    for a in dir(tmpl):
        try:
            setattr(getattr(tmpl,a),'priority',0)
        except:
            pass

    ## Turns on what we want
    tmpl.data.priority=1
    tmpl.box1.priority=1
    tmpl.legend.priority=1
    tmpl.xlabel1.priority=1
    tmpl.xlabel2.priority=1
    tmpl.ylabel1.priority=2
    tmpl.ylabel2.priority=2
    tmpl.xtic1.priority=2
    tmpl.xtic2.priority=2
    tmpl.ytic1.priority=2
    tmpl.ytic2.priority=2
    tmpl.xname.priority=1
    tmpl.yname.priority=1
    tmpl.dataname.priority=1
    
    
    if ntemplate==2:
        tmpl.reset('x',.06,.44,tmpl.data.x1,tmpl.data.x2)
    else:
        tmpl.reset('x',.2,.9,tmpl.data.x1,tmpl.data.x2)
    tmpl.reset('y',.2,.9,tmpl.data.y1,tmpl.data.y2)
    tmpl.ytic2.x1=tmpl.data.x1
    tmpl.ytic2.x2=tmpl.data.x2
    tmpl.ytic2.line=dot

    to=x.createtextorientation()
    to.halign='center'
    tmpl.dataname.x=(tmpl.data.x1+tmpl.data.x2)/2.
    tmpl.dataname.y=tmpl.data.y2+.03
    tmpl.dataname.textorientation=to

    tmpl.legend.x1=.3
    tmpl.legend.x2=.7

    tmpl.yname.x=tmpl.yname.x-.01
    tmpl.xname.y=tmpl.xname.y-.075

    tmpl.xtic2.y1=tmpl.data.y1
    tmpl.xtic2.y2=tmpl.data.y2
    tmpl.xtic2.line=dot
    tmpl.scalefont(.8)
    
    
    tmplnoleg=x.createtemplate(source=tmpl.name)
    tmplnoleg.legend.priority=0
    isof=x.createisofill()
    levs2=list(numpy.arange(min,max,deltaisof))
    levs1a=list(numpy.arange(min,0,deltaisol))
    levs1b=list(numpy.arange(0,max,deltaisol))
    isof.levels=levs2
    isof.fillareacolors=vcs.getcolors(levs2,colors=range(16,40))

    levs1a=list(numpy.arange(min,0,deltaisol))
    levs1b=list(numpy.arange(0,max,deltaisol))

    isol1=x.createisoline()
    isol1.level=levs1a
    isol1.label='y'
    isol1.line=['dot']
    isol2=x.createisoline()
    isol2.level=levs1b
    isol2.label='y'

    tick2={}
    for i in days_line:
        tick2[1./i]=str(i)+' days'
    tick1={}
    for i in (0,):
        tick1[i]=' '

    for gm in [isof,isol1,isol2]:
        gm.datawc_x1=-15
        gm.datawc_x2=15
        gm.datawc_y1=0.
        gm.datawc_y2=.8
        gm.yticlabels2=tick2
        gm.xticlabels2=tick1
    return tmpl,tmplnoleg,isof,isol1,isol2

def mask_power(power,wvnb,fqcy):
    w=power.getAxis(1)
    f=power.getAxis(0)
##     print w
##     print f
    iw=-1
    for i in range(len(w)):
        #print w[i],wvnb
        if w[i]==wvnb:
            iw=i
            break
    if iw==-1:
        raise 'Could not find wave number:'+str(wvnb)
    ifq=-1
    for i in range(len(f)):
        #print f[i],fqcy,numpy.ma.subtract(f[i],fqcy)
        if numpy.ma.allclose(f[i],fqcy):
            ifq=i
            break
    if ifq==-1:
        print 'Could not find frequency:'+str(fqcy)
    else:
        power[ifq,iw]=-999
    power=MV2.masked_equal(power,-999)
    return power

