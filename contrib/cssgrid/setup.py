import sys,os
target_prefix = sys.prefix
for i in range(len(sys.argv)):
    a = sys.argv[i]
    if a=='--prefix':
        target_prefix=sys.argv[i+1]
    sp = a.split("--prefix=")
    if len(sp)==2:
        target_prefix=sp[1]
sys.path.insert(0,os.path.join(target_prefix,'lib','python%i.%i' % sys.version_info[:2],'site-packages')) 
from numpy.distutils.core import Extension

sources = """
Src/csaddnod.f  Src/cscircum.f  Src/csgeti.f    Src/csintadd.f  Src/css2cd.f     Src/cssgrid.f
Src/csstrid.f   Src/csunif.f
Src/csaplyr.f   Src/csgetnp.f   Src/csintrc0.f  Src/css2c.f     Src/csstri.f     Src/csvorod.f
Src/csaplyrt.f  Src/csconstr.f  Src/csgetr.f    Src/csintrc1.f  Src/csscoordd.f  Src/csswap.f            Src/csvoro.f
Src/csarcint.f  Src/cscovsph.f  Src/csgetsig.f  Src/csintrsc.f  Src/csscoord.f   Src/cssig0.f            Src/csswptst.f  
Src/csarclen.f  Src/cscrlist.f  Src/csgivens.f  Src/csjrand.f   Src/csserr.f     Src/cssig1.f            Src/cstransd.f
Src/csareas.f   Src/csdelarc.f  Src/csgradg.f   Src/csleft.f    Src/cssetd.f     Src/cssig2.f            Src/cstrans.f
Src/csbdyadd.f  Src/csdelnb.f   Src/csgradl.f   Src/cslstptr.f  Src/csseti.f     Src/cssmsgs.f           Src/cstrfind.f
Src/csblda.f    Src/csdelnod.f  Src/csgrcoef.f  Src/csnbcnt.f   Src/cssetr.f     Src/cssmsurf.f          Src/cstrlist.f
Src/csbnodes.f  Src/csedge.f    Src/cshval.f    Src/csnearnd.f  Src/cssetup.f    Src/cssnhcsh.f          Src/cstrlprt.f
Src/csc2sd.f    Src/csfval.f    Src/csinsert.f  Src/csoptim.f   Src/cssgprnt.f   Src/cstrmesh.f
Src/csc2s.f     Src/csgetd.f    Src/csinside.f  Src/csrotate.f  Src/cssgridd.f   Src/csstore.f           Src/cstrprnt.f
""".split()

extra_link_args=[]
if sys.platform=='darwin':
    extra_link_args = ['-bundle','-bundle_loader '+sys.prefix+'/bin/python']
ext1 = Extension(name = 'cssgridmodule',
                 extra_link_args=extra_link_args,
                 sources = ['Src/cssgridmodule.pyf',]+sources)

if __name__ == "__main__":
    from numpy.distutils.core import setup
    setup(#name = 'css',
          ext_modules = [ext1,],
          packages = ['css'],
          package_dir = {'css': 'Lib',
                     },
          )
# Src/cscomn.h , Src/cssproto.h
