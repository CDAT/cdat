import sys
from TestDefinitions import testManager
test_name = sys.argv[1].strip("' \"")
interactive = sys.argv[2].strip("' \"") if ( len( sys.argv ) > 2 ) else False
baseline_dir = sys.argv[3].strip("' \"") if ( len( sys.argv ) > 3 ) else None
testManager.runTest( test_name, interactive, baseline_dir )
