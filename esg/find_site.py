# a utility script to find Python's site-packages directory and print it.
# used for installing Python megawidgets.
import sys, os
for x in sys.path:
   if os.path.basename(x) == 'site-packages':
       print x;raise SystemExit, 0
else:
   raise SystemExit, 1
