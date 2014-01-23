import cdms2,os,unittest

class TestCDATLite(unittest.TestCase):
    def setUp(self):
        self.pth = os.path.dirname(os.path.abspath(__file__))
        print "PTH:",self.pth

    def test_dimension_isUnlimited(self):
        f=cdms2.open(self.pth+"/tas_mo_clim.nc")
        try:
            v=f.variables['climseas']
            t=v.getTime()

            #Fails with AttributeError: CdmsFile.dimensions
            assert t.isUnlimited()
        finally:
            f.close()

if __name__=="__main__":
    unittest.main()

