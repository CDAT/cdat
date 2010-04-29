# helper routine for installing Pmw since it has no installer.
import sys, os
for x in sys.path:
    y = os.path.basename(x)
    if y == 'site-packages':
        print x
        break
else:  #If there is none such as on older windows versions
    print sys.path[-1]
