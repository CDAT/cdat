import cdms2, cdutil, genutil
import vcs

if __name__ == '__main__':
    import sys
    cdmsfile = cdms2.open('/Users/tpmaxwell/data/AConaty/comp-ECMWF/ecmwf.inst3_3d_wxm_Np.20110520_00z+20110520_00z.nc')
    U_velocity = cdmsfile('U_velocity')
    U_velocity = U_velocity(isobaric=(1000.0, 10.0),lon=(0.0, 359.0),time=('2011-5-20 0:0:0.0', '2011-5-20 0:0:0.0'),lat=(90.0, -90.0),squeeze=1,)
    axesOperations = eval("{'lat': 'def', 'isobaric': 'def', 'lon': 'def', 'time': 'def'}")
    for axis in list(axesOperations):
        if axesOperations[axis] == 'sum':
            U_velocity = cdutil.averager(U_velocity, axis='(%s)'%axis, weight='equal', action='sum')
        elif axesOperations[axis] == 'avg':
            U_velocity = cdutil.averager(U_velocity, axis='(%s)'%axis, weight='equal')
        elif axesOperations[axis] == 'wgt':
            U_velocity = cdutil.averager(U_velocity, axis='(%s)'%axis)
        elif axesOperations[axis] == 'gtm':
            U_velocity = genutil.statistics.geometricmean(U_velocity, axis='(%s)'%axis)
        elif axesOperations[axis] == 'std':
            U_velocity = genutil.statistics.std(U_velocity, axis='(%s)'%axis)
    canvas = vcs.init()
    gm3D_Scalar = vcs.get3d_scalar('default')
    args = []
    args.append(U_velocity)
    gm3D_Scalar.axes = [['xyz']]
    gm3D_Scalar.Colorbar = [[1]]
    gm3D_Scalar.VerticalScaling = [5.0686491905313265]
    gm3D_Scalar.IsosurfaceValue = [16.432937622070312]
    gm3D_Scalar.ToggleVolumePlot = [[1]], vcs.on
    gm3D_Scalar.ScaleColormap = [-14.551153825899483, 17.267348412394284, 1]
    gm3D_Scalar.ScaleTransferFunction = [20.770853227597186, 81.085205, 1], vcs.on
    gm3D_Scalar.XSlider = [119.87534943731247], vcs.on
    gm3D_Scalar.ScaleOpacity = [1.0, 1.0]
    gm3D_Scalar.Animation = [0.0]
    gm3D_Scalar.ZSlider = [0.6777807722949307], vcs.on
    gm3D_Scalar.YSlider = [21.796957974561707], vcs.on
    gm3D_Scalar.BasemapOpacity = [0.5]
    kwargs = {}
    kwargs[ 'cdmsfile' ] = cdmsfile.id
    args.append( gm3D_Scalar )
    canvas.plot( *args, **kwargs )
    canvas.interact()
