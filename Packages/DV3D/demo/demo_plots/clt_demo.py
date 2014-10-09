import vcs, cdms2, sys

def onAnyWindowEvent( self, caller=None, event=None ):
    print "Window Event: ", event

x = vcs.init()
f = cdms2.open(sys.prefix+"/sample_data/clt.nc")   
v = f["clt"] 
dv3d = vcs.get3d_scalar()
x.plot( v, dv3d )
x.backend.renWin.AddObserver( 'AnyEvent', onAnyWindowEvent )
x.interact()

