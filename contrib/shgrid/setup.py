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
Src/sh3grd.f  Src/shblda.f  Src/shgeti.f    Src/shgetnp.f   Src/shgrid.f   Src/shrot.f   Src/shsetup.f
Src/sh3val.f  Src/sherr.f   Src/shgetnp3.f  Src/shgivens.f  Src/shqshep.f  Src/shseti.f  Src/shstore3.f
""".split()

extra_link_args=[]
if sys.platform=='darwin':
    extra_link_args = ['-bundle','-bundle_loader '+sys.prefix+'/bin/python']
ext1 = Extension(name = 'shgridmodule',
                 extra_link_args=extra_link_args,
                 sources = ['Src/shgridmodule.pyf',]+sources)

if __name__ == "__main__":
    from numpy.distutils.core import setup
    setup(name = 'sh',
          ext_modules = [ext1,],
          packages = ['sh'],
          package_dir = {'sh': 'Lib',
                     },
          )
