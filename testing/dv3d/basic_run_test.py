'''
Created on Aug 27, 2014

@author: tpmaxwel
'''
import cdms2, cdutil, genutil
import vcs, os

image_check = False

if __name__ == '__main__':
    import sys
    
    f = cdms2.open( os.path.join( sys.prefix, "sample_data", "geos5-sample.nc") )
    U = f["uwnd"] 
    U = U(lon=(-105.0, -15.0),lev=(1000.0, 0.10000000100000001),lat=(7.0, 50.0),squeeze=1,)
    canvas = vcs.init()
    gm3D_Scalar = vcs.get3d_scalar('default')
    args = []
    args.append(U)
    gm3D_Scalar.VerticalScaling = 1.0
    gm3D_Scalar.IsosurfaceValue = 24.463887522462002
    gm3D_Scalar.ToggleVolumePlot =  vcs.on
    gm3D_Scalar.ScaleColormap = [-15.383988656109274, 10.447561660497996, 1]
    gm3D_Scalar.ScaleTransferFunction = [6.016036405845739, 12.382244144298838, 1]
    gm3D_Scalar.XSlider = -105.0
    gm3D_Scalar.ScaleOpacity = [0.0, 0.42135967065922453]
    gm3D_Scalar.Animation = 0.0
    gm3D_Scalar.ZSlider = 0.0, vcs.on
    gm3D_Scalar.YSlider = 7.0, vcs.on
    kwargs = { 'cdmsfile': f.id }
    args.append( gm3D_Scalar )
    canvas.plot( *args, **kwargs )
    plotApp = canvas.backend.plotApps[ gm3D_Scalar ]
    
    test_image = 'dv3d_basic_run_test.png'
    reference_image = 'dv3d_basic_run_test_ref.png'
    canvas.png( test_image )
    
    if image_check:
        import checkimage
        ret = checkimage.check_result_image( test_image, reference_image, 0.05 )
        print " Image Test returned:  %d " % ret
        sys.exit(ret)
        
    canvas.interact()