import unittest
import cdms2 as cdms
import cdutil, string, os
from genutil import statistics
import numpy
from matplotlib import pylab
import ESMP
import sys

class Test(unittest.TestCase):
    """
    Unit test from Dan Bergman
    """

    def setUp(self):
        pass

    def test_1(self):
        # Calculate a space-time RMSE metric for the CMIP3 models (ensemble run1 only)
        def getRef(var):
          if var == 'pr': ref = 'GPCP'
          if var == 'rlut': ref = 'CERES'
          return ref

        cmip3_clim_path = '/pcmdi/PCMDI1/database/ipcc_ar4/20c3m/atm/mo/'  #VAR/MOD'  #/run1/ac/'
        obs_path = '/pcmdi/clim_obs/obs/atm/mo/VAR/'

        vars = ['rlut']

        for var in vars:

        ### GET OBSERVATIONS
           obs_pathi = string.replace(obs_path,'VAR',var)
           print obs_pathi
           ref = getRef(var)
           obs_path_full = sys.prefix + '/sample_data/' + \
               var + '_' + ref  + '_000001-000012_ac.nc'
           print obs_path_full
           fobs = cdms.open(obs_path_full)
           dobs = fobs(var)
           fobs.close()
           obs_grid = dobs.getGrid()   # GET OBS GRID FOR REGRIDDING MODS TO OBS GRID

           print dobs.shape

        ### TRAP MODELS AVAILABLE
           mods = ('mpi_echam5',)       

           for mod in mods:
             mod_path = sys.prefix + '/sample_data/' + \
                 var + '_' + mod + '_1980-1999_ac.nc'
             fmod = cdms.open(mod_path)
             dmod = fmod(var)

             dmodMax = dmod.max()
             dmodMin = dmod.min()

             # exercise all regridding methods/tools
             
             print 'default...'
             diag = {}
             dmod_default = dmod.regrid(obs_grid, diag = diag)
             self.assertEqual(diag['regridTool'], 'esmf')
             self.assertLess(abs(dmod_default.max() - dmodMax)/dmodMax, 0.01)
             self.assertLess(abs(dmod_default.min() - dmodMin)/dmodMin, 0.01)

             print 'regrid...'
             diag = {}
             dmod_regrid = dmod.regrid(obs_grid, regridTool = 'regrid', diag = diag)
             self.assertEqual(diag['regridTool'], 'regrid')
             self.assertLess(abs(dmod_regrid.max() - dmodMax)/dmodMax, 0.01)
             self.assertLess(abs(dmod_regrid.min() - dmodMin)/dmodMin, 0.01)
             
             print 'esmf linear...'
             diag = {}
             dmod_esmf_linear = dmod.regrid(obs_grid, regridTool = 'esmf', regridMethod = 'linear', diag = diag)
             self.assertEqual(diag['regridTool'], 'esmf')
             self.assertEqual(diag['regridMethod'], 'linear')
             self.assertLess(abs(dmod_esmf_linear.max() - dmodMax)/dmodMax, 0.01)
             self.assertLess(abs(dmod_esmf_linear.min() - dmodMin)/dmodMin, 0.01)
        
             print 'esmf patch...'
             diag = {}
             dmod_esmf_patch = dmod.regrid(obs_grid, regridTool = 'esmf', regridMethod = 'patch', diag = diag)
             self.assertEqual(diag['regridTool'], 'esmf')
             self.assertEqual(diag['regridMethod'], 'patch')
             self.assertLess(abs(dmod_esmf_patch.max() - dmodMax)/dmodMax, 0.01)
             self.assertLess(abs(dmod_esmf_patch.min() - dmodMin)/dmodMin, 0.01)

             print 'esmf conserve...'
             diag = {}
             dmod_esmf_conserve = dmod.regrid(obs_grid, regridTool = 'esmf', regridMethod = 'conserve', diag = diag) 
             self.assertEqual(diag['regridTool'], 'esmf')
             self.assertEqual(diag['regridMethod'], 'conserve')
             self.assertLess(abs(dmod_esmf_conserve.max() - dmodMax)/dmodMax, 0.01)
             self.assertLess(abs(dmod_esmf_conserve.min() - dmodMin)/dmodMin, 0.01)

        
if __name__ == '__main__':

    print "" # Spacer
    suite = unittest.TestLoader().loadTestsFromTestCase(Test)
    unittest.TextTestRunner(verbosity = 1).run(suite)

