#/usr/bin/env python
import sys,vcs,time
preopen = True
drawline   = True
drawfill   = False
drawmarker = False
drawtext   = False
drawdata   = True
doclear    = False
doclose    = False
doresize= True
domultiple = True
dumpps = False
doanim = False
docreate = False
x=vcs.init()
if domultiple:
    y=vcs.init()
##     z=vcs.init()

if preopen:
    #print "opening"
    x.open()
    if domultiple:
        y.open()
##         z.open()
    print "done opening"
if drawfill:
    f=x.createfillarea()
    f.x=[.2,.8,.2,.8]
    f.y=[.2,.2,.8,.8]
    f.color=244
    f.style=['hatch']
    f.index = 3
    x.plot(f)
    if domultiple: y.plot(f)
if drawline:
    l=x.createline()
    l.x=[.2,.2,.5,.8,.8]
    l.y=[.2,.8,.6,.8,.2]
    l.width=5
    l.color=242
    x.plot(l)
    if domultiple:
        l2=x.createline()
        l2.x=[.2,.2,.5,.8,.8]
        l2.y=[.2,.8,.9,.8,.2]
        l2.width=5
        l2.color=244
        y.plot(l2)
        l3=x.createline()
        l3.x=[.2,.2,.5,.8,.8]
        l3.y=[.2,.8,.6,.8,.2]
        l3.width=5
        l3.color=246
##         z.plot(l3)
    #raw_input()
if doresize:
    x.geometry(640,480,10,30)
    print 'Back from geometry..........................'
    raw_input()
    #x.update(1)
    if domultiple:
        y.geometry(640,480,660,30)
if drawmarker:
    m=x.createmarker()
    m.x=[.5,.8]
    m.y=[.5,.8]
    m.type=['hurricane']
    m.size=20
    m.color=243
    x.plot(m)
    if domultiple: y.plot(m)
if drawtext:
    t=x.createtext()
    t.x=[.5,.8]
    t.y=[.5,.8]
    t.height=20
    t.color=245
    t.string=['hi','there']
    x.plot(t)
    if domultiple: y.plot(t)
if drawdata:
    import cdms2,sys
    #print sys.prefix+'/sample_data/clt.nc'
    f=cdms2.open(sys.prefix+'/sample_data/clt.nc')
    s=f("clt",slice(0,12))
    t=x.createtemplate("noX")
    print "back from template"
    gm=x.createisofill()
    x.plot(s,t,gm)
    if domultiple: y.plot(s,t,gm)
    if dumpps: x.postscript("test")
    if doanim:
        if docreate:
            x.animate.create( thread_it = 0, save_file="crap.ras" )
            raw_input("created anim")
        else:
            x.animate.load_from_file( thread_it = 0, load_file="crap.ras" )
            raw_input("loaded anim")

##         x.animate.run()
##         raw_input("running anim")
##         x.animate.stop()
##         raw_input("stopped anim")
##         x.animate.run( )
##         x.animate.pause( 3 )
##         raw_input("running anim with pause 3")
            
##         # Zoom in on the animated frames
##         x.animate.zoom( 2 )
##         raw_input("running anim with zoom 2")
        
##         # Move the animation horizonatally to the up and down
##         #x.animate.horizontal( 50 )
##         #raw_input("running anim with horiz pan")
            
##         # Move the animation vertically left and right
##         #x.animate.vertical( 50 )
##         #raw_input("running anim with vert pan")
        
## ##         # Stop the animation and view frame 5, 10, and 15
## ##        x.animate.zoom(1)
## ##	raw_input("zooming back to 1")
##         x.animate.stop( )
##         raw_input("stopped anim")
##         x.animate.frame( 3 )
##         raw_input("showed fram 3")
##         x.animate.frame( 5 )
##         raw_input("showed fram 5")
##         x.animate.frame( 10 )
##         raw_input("showed fram 10")

if doclear:
    x.clear()
    if domultiple: y.clear()
if doclose:
    print >>sys.stderr,"%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%"
    x.close()
    if domultiple: y.close()
    
raw_input()
