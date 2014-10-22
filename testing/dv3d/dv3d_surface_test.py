import sys
from TestDefinitions import testManager

interactive = False
baselinedir = None
for arg in sys.argv[1:]:
    if arg == "-i":
        interactive = True
    elif arg.startswith("--baselinedir="):
        baselinedir = arg.split("=")[1]

testManager.runTest( 'dv3d_surface_test', interactive, baselinedir )
