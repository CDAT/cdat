import collections, vcs, cdms2, h5py
from cdms2.hgrid import TransientCurveGrid
from cdms2.coord import TransientAxis2D, TransientVirtualAxis
import numpy as np
import numpy.ma as ma
eps = 0.0001

def print_attributes( grp ):
    print "\n ----> %s attributes:" % grp.name
    for attr in grp.attrs.keys():
        print "    > Attr %s = %s: " % ( attr, str(grp.attrs.get(attr)) )

data_file="/Users/tpmaxwel/Data/GPM/2A.GPM.Ku.V520140829.20150201-S011128-E014127.V03B.RT-H5"
point_coord_var_names=[ "Latitude", "Longitude" ]
lev_axis_name='nbin'
time_axis_name=None
varname = "/NS/SLV/precipRate"
slice = None

hfile = h5py.File( data_file, 'r' )

var = hfile[ varname ]
point_coord_axes = collections.OrderedDict()
print "Shape: ", str( var.shape )

fillval = float(var.attrs['CodeMissingValue'])
var_shape = var.shape[:]
data_array = np.zeros(var.shape, var.dtype )
var.read_direct(data_array)
data_array = ma.masked_inside( data_array, fillval - eps, fillval + eps )
print_attributes( var )

toks = varname.split('/')
groups = toks[0:-1]
varname = toks[-1]
subgrp = hfile
group_path = [ hfile ]
for group in groups:
    if group:
        subgrp = subgrp[ group ]
        group_path.append( subgrp )

for grp in group_path: print_attributes( grp )

dim_names = var.attrs['DimensionNames'].split(',')

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
   data_array = data_array[:,:,slice]

else:
    for caxis_name in coord_axes_rec.keys():
        trans_axis = transient_axes.get( caxis_name, None )
        if trans_axis is not None:
           caxes.append( trans_axis )
        else:
            caxis = cdms2.createAxis( range(coord_axes_rec[caxis_name]), id=caxis_name )
            if   caxis_name == lev_axis_name:
                caxis.designateLevel()
                caxis.units = "indexed"
            elif caxis_name == time_axis_name:
                caxis.designateTime()
                caxis.units = "indexed"
            caxes.append( caxis )

v = cdms2.createVariable( data_array, var.dtype, 0, 0, data_array.mask, fillval, tcgrid, caxes, hfile.attrs, varname, 0 )

hfile.close()

x=vcs.init()
dv3d = vcs.get3d_scalar()
x.plot( v, dv3d )
x.interact()
