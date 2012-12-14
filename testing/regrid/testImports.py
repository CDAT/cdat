"""
$Id: testImports.py 2354 2012-07-11 15:28:14Z pletzer $

Test diagnostics

"""

import numpy
import cdms2
import unittest
import ESMP

class Test(unittest.TestCase):

    def setUp(self):
        pass
 
    def test1(self):	
        from cdms2 import CdmsRegrid
        from regrid2 import ESMFRegrid
        from regrid2 import LibCFRegrid
        import regrid2.esmf

    def test2(self):
        import cdms2
        import cdms2

if __name__ == '__main__':
    print ""
    ESMP.ESMP_Initialize()
    suite = unittest.TestLoader().loadTestsFromTestCase(Test)
    unittest.TextTestRunner(verbosity = 1).run(suite)


