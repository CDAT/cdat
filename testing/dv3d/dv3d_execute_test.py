import sys
from TestDefinitions import testManager
test_name = sys.argv[1]
interactive = sys.argv[2] if ( len( sys.argv ) > 2 ) else False
baseline_dir = sys.argv[3] if ( len( sys.argv ) > 3 ) else None
testManager.runTest( test_name, interactive, baseline_dir )
