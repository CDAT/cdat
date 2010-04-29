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
Src/alf.f     Src/gradgs.f  Src/idvtgs.f   Src/isfvpgs.f  Src/ivrtec.f  Src/shaes.f  Src/slapec.f   Src/vhaes.f   Src/vlapgc.f   Src/vtsgc.f
Src/divec.f   Src/hrfft.f   Src/igradec.f  Src/islapec.f  Src/ivrtes.f  Src/shagc.f  Src/slapes.f   Src/vhagc.f   Src/vlapgs.f   Src/vtsgs.f
Src/dives.f   Src/idivec.f  Src/igrades.f  Src/islapes.f  Src/ivrtgc.f  Src/shags.f  Src/slapgc.f   Src/vhags.f   Src/vrtec.f
Src/divgc.f   Src/idives.f  Src/igradgc.f  Src/islapgc.f  Src/ivrtgs.f  Src/shigc.f  Src/slapgs.f   Src/vhsec.f   Src/vrtes.f
Src/divgs.f   Src/idivgc.f  Src/igradgs.f  Src/islapgs.f  Src/sfvpec.f  Src/shigs.f  Src/sphcom.f   Src/vhses.f   Src/vrtgc.f
Src/gaqd.f    Src/idivgs.f  Src/ihgeod.f   Src/ivlapec.f  Src/sfvpes.f  Src/shsec.f  Src/sshifte.f  Src/vhsgc.f   Src/vrtgs.f
Src/gradec.f  Src/idvtec.f  Src/isfvpec.f  Src/ivlapes.f  Src/sfvpgc.f  Src/shses.f  Src/trssph.f   Src/vhsgs.f   Src/vshifte.f
Src/grades.f  Src/idvtes.f  Src/isfvpes.f  Src/ivlapgc.f  Src/sfvpgs.f  Src/shsgc.f  Src/trvsph.f   Src/vlapec.f  Src/vtsec.f
Src/gradgc.f  Src/idvtgc.f  Src/isfvpgc.f  Src/ivlapgs.f  Src/shaec.f   Src/shsgs.f  Src/vhaec.f    Src/vlapes.f  Src/vtses.f
""".split()

extra_link_args=[]
if sys.platform=='darwin':
    extra_link_args = ['-bundle','-bundle_loader '+sys.prefix+'/bin/python']
ext1 = Extension(name = 'spherepack',
                 extra_link_args=extra_link_args,
                 sources = ['Src/spherepack.pyf',]+sources)

if __name__ == "__main__":
    from numpy.distutils.core import setup
    setup(name = 'sphere',
          ext_modules = [ext1,],
          packages = ['sphere'],
          package_dir = {'sphere': 'Lib',
                     },
          )
