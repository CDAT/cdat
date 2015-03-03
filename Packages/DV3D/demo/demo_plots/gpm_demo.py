import collections, vcs, cdms2, h5py, cdtime
from cdms2.hgrid import TransientCurveGrid
from cdms2.coord import TransientAxis2D, TransientVirtualAxis
import numpy as np
import numpy.ma as ma
eps = 0.0001
time_units = "days since 1996-1-1"

def print_attributes( grp ):
    print "\n ----> %s attributes:" % grp.name
    for attr in grp.attrs.keys():
        print "    > Attr %s = %s: " % ( attr, str(grp.attrs.get(attr)) )

def getFileMetadata( attrs ):
    md = {}
    for mdkey in attrs.keys():
        mdtext = attrs[mdkey]
        for mdentry in mdtext.split(';'):
            mdelems = mdentry.split('=')
            if len( mdelems ) == 2:
               md[ mdelems[0].strip() ] = mdelems[1].strip()
    return md

def getReltime( timestamp, units ):
    tstoks = timestamp.split('T')
    ymd = tstoks[0].split('-')
    hms = tstoks[1].split('.')[0].split(':')
    ctval = cdtime.comptime( int(ymd[0]), int(ymd[1]), int(ymd[2]), int(hms[0]), int(hms[1]), int(hms[2])  )
    return ctval.torel( units )

def merge_data_arrays( threshold_var, color_var, vthresholds, n_color_vals=256 ):
    vrange = ( color_var.min(), color_var.max() )
    indexed_color_var = ( ( color_var - vrange[0] ) * ( ( n_color_vals - 0.01 ) / (vrange[1] - vrange[0]) ) ).astype(int)
    for vthresh in vthresholds:
        indexed_color_var[ threshold_var > vthresh ] += n_color_vals
    return indexed_color_var

def createTransientVariable( hfile, phase_var_name, data_var_name, phase_range=None, value_threshold = None ):
        point_coord_var_names=[ "Latitude", "Longitude" ]
        lev_axis_name='nbin'
        time_axis_name=None
        slice = None
        tcgrid = None
        caxes = None
        file_mdata = None
        point_coord_axes = collections.OrderedDict()

        phase_var = hfile[ phase_var_name ]
        phase_fillval = int( phase_var.attrs['CodeMissingValue'] )
        phase_array = np.zeros(phase_var.shape, phase_var.dtype )
        phase_var.read_direct(phase_array)
        phase_array = ma.masked_equal( phase_array, phase_fillval )

        data_var = hfile[ data_var_name ]
        fillval = float(data_var.attrs['CodeMissingValue'])
        data_array = np.zeros(data_var.shape, data_var.dtype )
        data_var.read_direct(data_array)
        data_array = ma.masked_inside( data_array, fillval - eps, fillval + eps )

        if value_threshold is not None:
            data_array = ma.masked_less( data_array, (value_threshold + eps)  )
            phase_array.mask = data_array.mask

        if phase_range is not None:
            phase_array = ma.masked_outside( phase_array, phase_range[0], phase_range[1]  )
            data_array.mask = phase_array.mask

        print "Phase value range: ", str( [ phase_array.min(), phase_array.max() ])

        toks = data_var_name.split('/')
        groups = toks[0:-1]
        varname = toks[-1]

        if tcgrid == None:
            var_shape = list(data_var.shape)
            subgrp = hfile
            group_path = [ hfile ]
            for group in groups:
                if group:
                    subgrp = subgrp[ group ]
                    group_path.append( subgrp )

            for grp in group_path: print_attributes( grp )
            file_mdata = getFileMetadata( hfile.attrs )
            timestamp = file_mdata.get( 'StartGranuleDateTime', None )

            dim_names = data_var.attrs['DimensionNames'].split(',')

            coord_axes_rec = collections.OrderedDict()
            for idim, dim_id in enumerate( dim_names ):
                axis_name = dim_names[ idim ]
                dim_size = var_shape[idim]
                coord_axes_rec[ axis_name ] = dim_size

            transient_axes = collections.OrderedDict()
            for point_coord_var_name in point_coord_var_names:
                for group in group_path:
                    point_coord_var = group.get( point_coord_var_name, None )
                    if point_coord_var is not None:
                        dim_names = point_coord_var.attrs['DimensionNames'].split(',')
                        trans_axes = [ transient_axes.setdefault( dim_name, TransientVirtualAxis( dim_name, coord_axes_rec[ dim_name ] ) ) for dim_name in dim_names ]
                        axis2D = TransientAxis2D( point_coord_var[()], axes=trans_axes, attributes=point_coord_var.attrs, id=point_coord_var_name )
                        point_coord_axes[point_coord_var_name] = axis2D

            tcgrid = TransientCurveGrid( point_coord_axes[point_coord_var_names[0]], point_coord_axes[point_coord_var_names[1]], id='GPM_Swath' )

            caxes = []
            if slice is not None:
               caxes = transient_axes.values()
            else:
                time_axis = None
                for caxis_name in coord_axes_rec.keys():
                    trans_axis = transient_axes.get( caxis_name, None )
                    if trans_axis is not None:
                       caxes.append( trans_axis )
                    else:
                        caxis = cdms2.createAxis( range(coord_axes_rec[caxis_name]), id=caxis_name )
                        if   caxis_name == lev_axis_name:
                            caxis.designateLevel()
                            caxis.units = "indexed"
                            caxis.top_to_bottom = True
                        elif caxis_name == time_axis_name:
                            caxis.designateTime()
                            caxis.units = "indexed"
                            time_axis = caxis
                        caxes.append( caxis )
                if time_axis == None:
                    tval = getReltime( timestamp, time_units )
                    caxis = cdms2.createAxis( [ tval ], id="time" )
                    caxis.designateTime()
                    caxis.units = time_units
                    caxes.insert( 0, caxis )
                    var_shape.insert(0, 1 )

            if slice is not None:
                data_array = data_array[:,:,slice]
                phase_array = data_array[:,:,slice]
            else:
                if list(data_var.shape) <> var_shape:
                    data_array = data_array.reshape( var_shape )
                    phase_array = phase_array.reshape( var_shape )

#        merged_data_array = merge_data_arrays( phase_array, data_array, phase_thresholds )
#        merged_fillval = -1
#        v = cdms2.createVariable( ma.filled( merged_data_array, merged_fillval ), merged_data_array.dtype, 0, 0, merged_data_array.mask, merged_fillval, tcgrid, caxes, file_mdata, varname, 0 )

#        merged_fillval = -1
#        v = cdms2.createVariable( ma.filled( data_array, merged_fillval ), data_array.dtype, 0, 0, data_array.mask, merged_fillval, tcgrid, caxes, file_mdata, varname, 0 )

        merged_fillval = 0
        v = cdms2.createVariable( ma.filled( phase_array, merged_fillval ), phase_array.dtype, 0, 0, phase_array.mask, merged_fillval, tcgrid, caxes, file_mdata, varname, 0 )

        return v

if __name__ == "__main__":
    data_file="/Users/tpmaxwel/Dropbox/Data/GPM/2A.GPM.Ku.V520140829.20150201-S011128-E014127.V03B.RT-H5"
    phase_varname =  "/NS/DSD/phase"
    data_varname = "/NS/SLV/precipRate"
    singleCore = True


    hfile = h5py.File( data_file, 'r' )
    vthresh = 0.0
    v = createTransientVariable( hfile, phase_varname, data_varname, [ 0, 254 ], vthresh )
    hfile.close()

    x=vcs.init()
    dv3d = vcs.get3d_scalar()
    dv3d.VerticalScaling = 0.05
    dv3d.ToggleVolumePlot = vcs.on
    dv3d.ToggleSphericalProj = vcs.off
    vmax = v.max()

    if singleCore:
        dv3d.NumCores = 1
        phase_vthresh = 1
        x.plot( v, dv3d, maxNumSerialPoints=50000000, vthresh=phase_vthresh, level_range=[76,176] )
    else:
        x.plot( v, dv3d, vthresh=vthresh, level_range=[76,176] )

    x.interact()
