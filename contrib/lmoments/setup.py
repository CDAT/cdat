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
import sys

sources = """
Src/alg136.for  Src/cdfgum.for  Src/clukm.for   Src/lmrgam.for  Src/lmrnor.for  Src/pelgno.for  Src/quaexp.for  Src/quakap.for  Src/samlmr.for
Src/cdfexp.for  Src/cdfkap.for  Src/derf.for    Src/lmrgev.for  Src/lmrpe3.for  Src/pelgpa.for  Src/quagam.for  Src/quanor.for  Src/samlmu.for
Src/cdfgam.for  Src/cdfnor.for  Src/digamd.for  Src/lmrglo.for  Src/lmrwak.for  Src/pelgum.for  Src/quagev.for  Src/quape3.for  Src/sampwm.for
Src/cdfgev.for  Src/cdfpe3.for  Src/dlgama.for  Src/lmrgno.for  Src/pelexp.for  Src/pelkap.for  Src/quaglo.for  Src/quastn.for  Src/sort.for
Src/cdfglo.for  Src/cdfwak.for  Src/durand.for  Src/lmrgpa.for  Src/pelgam.for  Src/pelnor.for  Src/quagno.for  Src/quawak.for
Src/cdfgno.for  Src/cluagg.for  Src/gamind.for  Src/lmrgum.for  Src/pelgev.for  Src/pelpe3.for  Src/quagpa.for  Src/reglmr.for
Src/cdfgpa.for  Src/cluinf.for  Src/lmrexp.for  Src/lmrkap.for  Src/pelglo.for  Src/pelwak.for  Src/quagum.for  Src/regtst.for
""".split()

extra_link_args=[]
if sys.platform=='darwin':
    extra_link_args = ['-bundle','-bundle_loader '+sys.prefix+'/bin/python']
ext1 = Extension(name = 'lmoments',
                 extra_link_args=extra_link_args,
                 sources = ['Src/flmoments.pyf',]+sources)

if __name__ == "__main__":
    from numpy.distutils.core import setup
    setup(name = 'lmoments',
          ext_modules = [ext1,],
##           packages = ['lmoments'],
##           package_dir = {'lmoments': 'Lib',
##                      },
          )
