from coverage import coverage

import glob
import os

c = coverage(auto_data=True, source=["vcs.vtk_ui"])
c.start()

for test in glob.iglob("test_*.py"):
    module_file = os.path.basename(test)
    module_name, ext = os.path.splitext(module_file)
    print "Test", module_name
    module = __import__(module_name)
    test_class = getattr(module, module_name)
    test_object = test_class()
    test_object.do_test()

c.stop()
c.html_report(directory="covhtml")