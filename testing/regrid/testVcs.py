import cdms2
import numpy
import unittest
import vcs

class TestVcs(unittest.TestCase):
    """
    All test interpolate to the same grid
    """

    def setUp(self):
        pass

    def test_test1(self):
        u = cdms2.open('clt.nc')('u')[0, 0,...]
        w = vcs.init()
        w.plot(u)
        w.png("u")

        
if __name__ == '__main__':
    print "" # Spacer
    suite = unittest.TestLoader().loadTestsFromTestCase(TestVcs)
    unittest.TextTestRunner(verbosity = 1).run(suite)
