#!/usr/bin/env python

# A simple script showing that the 'q'uit command in a DV3D canvas
# will exit python, when it should only go back to the interpreter
#
# Use: python -i this_script.py
#
# J-Y Peterschmitt - LSCE - 01/2015

import vcs, cdms2, sys
__author__ = 'tpmaxwel'

# Canvas with a 3D plot
print 'Creating a simple 3D plot canvas...\n'

x = vcs.init()
f = cdms2.open(vcs.sample_data+"/clt.nc")
v = f("clt", time=slice(0,10))
dv3d = vcs.get3d_scalar()
x.plot( v, dv3d )

# Canvas with a default 2D plot
print '\nCreating a simple 2D plot canvas...\n'
y = vcs.init()
y.plot(v)

# Enter the interactive mode, then try to quit it to go back to the
# interpreter
print '\nEntering the interactive mode...'
print 'Typing "q" in the 3D canvas will exit python => NOT good :('
print 'Typing "q" in the 2D canvas will go back to the interpreter => GOOD! :)\n'
x.interact()

# The end
