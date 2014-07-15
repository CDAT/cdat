#!/usr/bin/env python
# Adapted for numpy/ma/cdms2 by convertcdms.py

import cdms2 as cdms,vcs_legacy,sys,os,support
bg=support.bg

x=vcs_legacy.init()

fnmirr = os.path.join(cdms.__path__[0],'..','..','..','..','sample_data','sampleGenGrid3.nc')

f=cdms.open(fnmirr)
gen=f('sample')

m=x.createmeshfill('tst_ratio')
proj=x.createprojection('test_ratio')
proj.type='orthographic'

print 'Should shrink a bit'
x.plot(gen,bg=bg)
support.check_plot(x)

x.ratio='off'
print 'x.ratio set to off should not shrink'
x.clear() ; x.plot(gen,bg=bg)
support.check_plot(x)


print sys.argv
if '--extended' not in sys.argv:
     print '\n************* PARTIAL TEST *****************'
     print 'FOR COMPLETE TEST OF THIS MODULE USE '
     print '   -F (--full) or -E (--extended) option'
     print '************* PARTIAL TEST *****************\n'
     sys.exit()

x.ratio='auto'
print 'x.ratio set back to auto should not shrink because using ASD template'
x.clear() ; x.plot(gen,'ASD',bg=bg)
support.check_plot(x)

x.ratio='auto'
print 'x.ratio set back to auto should not shrink because using ASD template, but no ticks moved'
x.clear() ; x.plot(gen,'ASD',ratio="auto",bg=bg)
support.check_plot(x)

x.ratio='auto'
print 'Should shrink because using ASD template AND ratio="autot", tick marks should be moved'
x.clear() ; x.plot(gen,'ASD',ratio="autot",bg=bg)
support.check_plot(x)
print 'should not shrink because using ratio=0'
x.clear() ; x.plot(gen,ratio=0,bg=bg) # No transform
support.check_plot(x)

print 'should not shrink because using ratio="off"'
x.clear() ; x.plot(gen,ratio='off',bg=bg) # No transform
support.check_plot(x)

print 'should have y = x/2 (ratio=.5), tick mark moved since default template'
x.clear() ; x.plot(gen,ratio=.5,bg=bg) # y half x size
support.check_plot(x)

print 'should have y = x/10 (ratio=.1), tick mark moved since default template'
x.clear() ; x.plot(gen,ratio=.1,bg=bg) # y one tenth of x
support.check_plot(x)

print 'should have y = 3x (ratio=3), tick mark moved since default template'
x.clear() ; x.plot(gen,ratio=3,bg=bg) # y 3 times x
support.check_plot(x)

print 'should have y = 3x (ratio=3), tick mark not moved since ASD template'
x.clear() ; x.plot(gen,'ASD',ratio=3,bg=bg) # y 3 times x
support.check_plot(x)

print 'should have y = 3x (ratio=3), tick mark not moved since self.ratio=3'
x.ratio=3
x.clear() ; x.plot(gen,'ASD',bg=bg) # y 3 times x
support.check_plot(x)

print 'should have y = 3x (ratio=3), tick mark moved since self.ratio=3T'
x.ratio='3t'
x.clear() ; x.plot(gen,'ASD',bg=bg) # y 3 times x
support.check_plot(x)

print 'should have y = 3x (ratio="3t"), tick mark moved since ASD template and ratio="3T"'
x.ratio='auto'
x.clear() ; x.plot(gen,'ASD',ratio="3t",bg=bg) # y 3 times x
support.check_plot(x)

print 'should shrink a bit since linear proj, tick mark moved since default template'
x.clear() ; x.plot(gen,m,bg=bg) # test with a meshfill
support.check_plot(x)

m.projection=proj
print 'should not shrink since proj is not linear'
x.clear() ; x.plot(gen,m,bg=bg) # now should be uggly
support.check_plot(x)

print 'should shrink since proj is not linear BUT ratio=1 passed'
x.clear() ; x.plot(gen,m,ratio=1,bg=bg) # now should be pretty
support.check_plot(x)

print 'should shrink since proj is not linear BUT ratio=1 passed and ASD template so no tick moved'
x.clear() ; x.plot(gen,'ASD',m,ratio=1,bg=bg) # now should be pretty
support.check_plot(x)


