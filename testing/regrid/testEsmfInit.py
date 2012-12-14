#!/usr/bin/env python

import ESMP
import unittest

class Test(unittest.TestCase):
    def setUp(self):
        pass

    def test1_pairedInitializeFinalize(self):
        ESMP.ESMP_Initialize()
        ESMP.ESMP_Finalize()

    def test2_multipleInitializeFinalize(self):
        ESMP.ESMP_Initialize()
        ESMP.ESMP_Finalize()
        ESMP.ESMP_Initialize()
        ESMP.ESMP_Finalize()

    def test3_multitpleInitialize(self):
        ESMP.ESMP_Initialize()
        ESMP.ESMP_Initialize()
        ESMP.ESMP_Finalize()

    def test4_multipleFinalize(self):
        ESMP.ESMP_Initialize()
        ESMP.ESMP_Finalize()
        ESMP.ESMP_Finalize()

    def test5_noFinalize(self):
        ESMP.ESMP_Initialize()

if __name__ == '__main__': 
    print "" # Spacer
    suite = unittest.TestLoader().loadTestsFromTestCase(Test)
    unittest.TextTestRunner(verbosity = 1).run(suite)
    




    
