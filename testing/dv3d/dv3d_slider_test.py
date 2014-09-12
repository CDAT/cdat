import sys
from TestDefinitions import testManager
interactive = ( len(sys.argv) > 1 ) and ( sys.argv[1] == '-i' )
testManager.runTest( 'dv3d_slider_test', interactive )
