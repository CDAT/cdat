'''
Created on Aug 28, 2014

@author: tpmaxwel
'''
from TestManager import TestManager, vcsTest
import vcs, sys, os

testManager = TestManager()   
     
test1 =  vcsTest( 'dv3d_initial_test', roi=( -105.0, -15.0, 7.0, 50.0 ), 
                     parameters={'VerticalScaling': 3.0, 
                                 'ToggleVolumePlot': vcs.on, 
                                 'ScaleColormap': [-15.383988656109274, 10.447561660497996, 1], 
                                 'ScaleTransferFunction':  [6.016036405845739, 12.382244144298838, 1], 
                                 'XSlider': -105.0, 
                                 'ZSlider': ( 0.0, vcs.on ),
                                 'YSlider': ( 7.0, vcs.on ), 
                                 }  )       
    
if __name__ == '__main__':
    
    testManager.reviewTests()
    
