
import cmor_const,numpy,os,_cmor

try:
    import cdtime
    has_cdtime = True
except:
    has_cdtime = False

try:
    import cdms2
    has_cdms2 = True
except:
    has_cdms2 = False

try:
    import MV2
    has_MV2 = True
except:
    has_MV2 = False

try:
    import numpy.oldnumeric.ma.MaskedArray
    has_oldma = True
except:
    has_oldma = False

def time_varying_grid_coordinate(grid_id, table_entry,units,type='f',missing_value=None):
    """ Create a cmor variable for grid coordinates in case of time varying grids
    Usage:
    coord_grid_id = grid_time_varying_coordinate(grid_id, table_entry,units,type='f',missing_value=None)
    Where:
    grid_id : The grid_id return by a call to cmor.grid
    table_entry: The name of the variable in the CMOR table
    units: variable units
    type: type of the missing_value, which must be the same as the type of the array  that will be passed to cmor_write.  The options are: 'd' (double), 'f' (float), 'l' (long) or 'i' (int).
    missing_value : scalar that is used to indicate missing data for this variable.  It must be the same type as the data that will be passed to cmor_write.  This missing_value will in general be replaced by a standard missing_value specified in the MIP table.  If there are no missing data, and the user chooses not to declare the missing value, then this argument may be either omitted or assigned the value 'none' (i.e., missing_value='none').
    """
    if not isinstance(table_entry,str):
        raise Exception, "Error you must pass a string for the variable table_entry"
    
    if not isinstance(units,str):
        raise Exception, "Error you must pass a string for the variable units"
    if not isinstance(type,str):
        raise Exception, "error tpye must a a string"
    type = type.lower()
    if type == 's':
        type ='c'
    if not type in ["c","d","f","l","i"]:
        raise Exception, 'error unknown type: "%s", must be one of: "c","d","f","l","i"'

    if not isinstance(grid_id,(int,numpy.int,numpy.int32)):
        raise Exception, "error grid_id must be an integer"
    grid_id = int(grid_id)

    if missing_value is not None:
        if not isinstance(missing_value,(float,int,numpy.float,numpy.float32,numpy.int,numpy.int32)):
            raise Exception, "error missing_value must be a number, you passed: %s" % type(missing_value)
        missing_value = float(missing_value)
        
    return _cmor.time_varying_grid_coordinate(grid_id,table_entry,units,type,missing_value)
    

def _to_numpy(vals, message):
    if isinstance(vals, (list, tuple)):
        vals = numpy.ascontiguousarray(vals)
    elif not isinstance(vals,numpy.ndarray):
        try:
            vals = numpy.ascontiguousarray(vals.filled())
        except:
            raise Exception, "Error could not convert %s to a numpy array" % message
        
    return vals

def grid(axis_ids,latitude=None,longitude=None,latitude_vertices=None,longitude_vertices=None,nvertices=None):
    """ Creates a cmor grid
    Usage:
    grid_id = grid(axis_ids,latitude,longitude,latitude_vertices=None,longitude_vertices=None)
    Where:
    axis_ids : array contianing the axes ids for this grid.
    latitude/longitude: the values for longitude/latitude arrays (unless it is a time varying grid)
    latitude_vertices/longitude_vertices: coordinates of vertices for each latitude/latitude (unless it is a time varying grid)
    """
    if numpy.ma.isMA(axis_ids):
        axis_ids = numpy.ascontiguousarray(axis_ids.filled())
    elif has_oldma and numpy.oldnumeric.ma.isMA(axis_ids):
        axis_ids = numpy.ascontiguousarray(axis_ids.filled())
    elif has_cdms2 and cdms2.isVariable(axis_ids):
        axis_ids = numpy.ascontiguousarray(axis_ids.filled())
    elif has_cdms2 and cdms2.isVariable(axis_ids):
        axis_ids = numpy.ascontiguousarray(axis_ids.filled())
    elif isinstance(axis_ids,(list,tuple)):
        axis_ids = numpy.ascontiguousarray(axis_ids)
    elif not isinstance(axis_ids, numpy.ndarray):
        raise Exception, "Error could not convert axis_ids list to a numpy array"

    if numpy.rank(axis_ids)>1:
        raise Exception, "error axes list/array must be 1D"

    if latitude is not None:
        latitude = _to_numpy(latitude, 'latitude')
            
        if numpy.rank(latitude)!=len(axis_ids):
            raise Exception, "latitude's rank does not match number of axes passed via axis_ids"

        type = latitude.dtype.char
        nvert = 0
        if not type in ['d','f','i','l']:
            raise Exception, "latitude array must be of type 'd','f','l' or 'i'"

        longitude = _to_numpy(longitude, 'longitude')

        if numpy.rank(longitude)!=len(axis_ids):
            raise Exception, "longitude's rank does not match number of axes passed via axis_ids"
        
    ##     print 'longitude type:',longitude.dtype.char
        if longitude.dtype.char!=type:
            longitude = longitude.astype(type)
    elif longitude is not None:
        raise Exception, "latitude and longitude must be BOTH an array or None"
    else:
        type='f'
        if nvertices is None :
            nvert=0
        else:
            nvert = nvertices
        
    if latitude_vertices is not None:
        latitude_vertices = _to_numpy(latitude_vertices, 'latitude_vertices')

        if numpy.rank(latitude_vertices)!=len(axis_ids)+1:
            raise Exception, "latitude_vertices's rank does not match number of axes passed via axis_ids +1 (for vertices)"
##         print 'latitude_vert type:',latitude_vertices.dtype.char
        if latitude_vertices.dtype.char!=type:
            latitude_vertices = latitude_vertices.astype(type)
        nvert = latitude_vertices.shape[-1]
        if nvertices is not None:
            if nvert!=nvertices:
                raise Exception,"you passed nvertices as: %i, but from your latitude_vertices it seems to be: %i" % (nvertices,nvert)
        
    if longitude_vertices is not None:
        longitude_vertices = _to_numpy(longitude_vertices, 'longitude_vertices')
        if numpy.rank(longitude_vertices)!=len(axis_ids)+1:
            raise Exception, "longitude_vertices's rank does not match number of axes passed via axis_ids +1 (for vertices)"
##         print 'longitude_vert type:',longitude_vertices.dtype.char
        if longitude_vertices.dtype.char!=type:
            longitude_vertices = longitude_vertices.astype(type)
        nvert2 = longitude_vertices.shape[-1]
        if latitude_vertices is None:
            nvert = nvert2
        elif nvert!=nvert2:
            raise Exception, "error in shape longitude_vertices and latitude_vertices seem to have different # of vertices: %i vs %i, %s" % (nvert,nvert2, str(longitude_vertices.shape ))
        if nvertices is not None:
            if nvert!=nvertices:
                raise Exception,"you passed nvertices as: %i, but from your longitude_vertices it seems to be: %i" % (nvertices,nvert)

        
##     if area is not None:
##         if not isinstance(area,numpy.ndarray):
##             try:
##                 area = numpy.ascontiguousarray(area.filled())
##             except:
##                 raise Exception, "Error could not convert area to a numpy array"
##             if numpy.rank(area)!=len(axis_ids):
##                 raise Exception, "area's rank does not match number of axes passed via axis_ids"
##         if area.dtype.char!=type:
##             area = area.astype(type)
    n = len(axis_ids)
    axis_ids=axis_ids.astype('i');
    return _cmor.grid(n,axis_ids,type,latitude,longitude,nvert,latitude_vertices,longitude_vertices)


def set_grid_mapping(grid_id,mapping_name,parameter_names,parameter_values=None,parameter_units=None):
    """Sets the grid mapping for CF convention
    Usage:
       set_grid_mapping(grid_id,mapping_name,parameter_names,parameter_values,parameter_units)
    Where:
       grid_id :: grid_id return by cmor.grid
       mapping_name     :: name of the mapping (see CF conventions)
       parameter_names  :: list of parameter names or dictionary with name as keys and values can be either [value,units] list/tuple or dictionary with "values"/"units" as keys
       parameter_values :: array/list of parameter values in the same order of parameter_names (ignored if parameter_names is ditcionary)
       parameter_units  :: array/list of parameter units  in the same order of parameter_names (ignored if parameter_names is ditcionary)
    """
    if not isinstance(grid_id,(numpy.int32,int,long)):
        raise Exception, "grid_id must be an integer: %s"%type(grid_id)
    if not isinstance(mapping_name,str):
        raise Exception, "mapping name must be a string"

    if isinstance(parameter_names,dict):
        pnams = []
        pvals = []
        punit = []
        for k in parameter_names.keys():
            pnams.append(k)
            val = parameter_names[k]
            if isinstance(val,dict):
                ks = val.keys()
                if not 'value' in ks or not 'units' in ks:
                    raise Exception, "error parameter_names key '%s' dictionary does not contain both 'units' and 'value' keys" % k
                pvals.append(val['value'])
                punit.append(val['units'])
            elif isinstance(val,(list,tuple)):
                if len(val)>2:
                    raise Exception, "parameter_names '%s' as more than 2 values" % k
                for v in val:
                    if isinstance(v,str):
                        punit.append(v)
                    try:
                        pvals.append(float(v))
                    except:
                        pass
                if len(pvals)!=len(punits) or len(pvals)!=len(pnams):
                    raise Exception, "could not figure out values for parameter_name: '%s' " % k
            else:
                raise Exception, "could not figure out values for parameter_name: '%s' " % k
    elif isinstance(parameter_names,(list,tuple)):
        pnms = list(parameter_names)
        # now do code for parameter_units
        if parameter_values is None:
            raise Exception, "you must pass a list or array for parameter_values"
        if parameter_units is None:
            raise Exception, "you must pass a list for parameter_units"
        if not isinstance(parameter_units,(list,tuple)):
            raise Exception, "you must pass a list for parameter_units"
        if len(parameter_units)!=len(pnms):
            raise Exception, "length of parameter_units list does not match length of parameter_names"
        punit = list(parameter_units)
        if isinstance(parameter_values,(list,tuple)):
            pvals = list(parameter_values)
        else:
            try:
                pvals = numpy.ascontiguousarray(parameter_values.filled())
            except:
                raise Exception, "Error could not convert parameter_values to a numpy array"
        if len(pvals) != len(parameter_names):
            raise Exception, "length of parameter_values list does not match length of parameter_names"
    else:
        raise Exception, "parameter_names must be either dictionary or list"

    pvals = numpy.ascontiguousarray(pvals).astype('d')
    return _cmor.set_grid_mapping(grid_id,mapping_name,pnms,pvals,punit)

    

def axis(table_entry,units=None,length=None,coord_vals=None,cell_bounds=None,interval=None):
    """ Creates an cmor_axis
    Usage:
    axis_id = axis(table_entry,units=None,length=None,coord_vals=None,cell_bounds=None,interval=None)
    Where:
    table_entry: table_entry in the cmor table
    units: the axis units
    length: the number of coord_vals to actuall y use, or simply the number of coord_vals in case in index_only axes
    coord_vals: cmds2 axis or numpy/MV2 array (1D)
    cell_bounds: numpy or MV2 array, if coord_vals is a cdms2 axis then will try to obtain bounds from it
    interval: a string used for time axes only (???)
    """
    if not isinstance(table_entry,str):
        raise Exception, "You need to pass a table_entry to match in the cmor table"

    if coord_vals is None:
        if cell_bounds is not None:
            raise Exception, "you passed cell_bounds but no coords"
    else:
        if has_cdms2 and isinstance(coord_vals,cdms2.axis.TransientAxis):
            if units is None:
                if hasattr(coord_vals,"units"):
                    units = coord_vals.units
            if cell_bounds is None:
                cell_bounds = coord_vals.getBounds()

            if interval is None and hasattr(coord_vals,"interval"):
                interval = coord_vals.interval
            coord_vals = numpy.ascontiguousarray(coord_vals[:])
        elif isinstance(coord_vals,(list,tuple)):
            coord_vals = numpy.ascontiguousarray(coord_vals)
        elif has_cdms2 and cdms2.isVariable(coord_vals):
            if units is None:
                if hasattr(coord_vals,"units"):
                    units = coord_vals.units
            if interval is None and hasattr(coord_vals,"interval"):
                interval = coord_vals.interval
            coord_vals = numpy.ascontiguousarray(coord_vals.filled())
        elif has_oldma and numpy.oldnumeric.ma.isMA(coord_vals):
            coord_vals = numpy.ascontiguousarray(coord_vals.filled())
        elif numpy.ma.isMA(coord_vals):
            coord_vals = numpy.ascontiguousarray(coord_vals.filled())

        if not isinstance(coord_vals,numpy.ndarray):
            raise Exception, "Error coord_vals must be an array or cdms2 axis or list/tuple"

        if numpy.rank(coord_vals)>1:
            raise Exception, "Error, you must pass a 1D array!"

    if numpy.ma.isMA(cell_bounds):
        cell_bounds = numpy.ascontiguousarray(cell_bounds.filled())
    elif has_oldma and numpy.oldnumeric.ma.isMA(cell_bounds):
        cell_bounds = numpy.ascontiguousarray(cell_bounds.filled())
    elif has_cdms2 and cdms2.isVariable(cell_bounds):
        cell_bounds = numpy.ascontiguousarray(cell_bounds.filled())
    elif has_cdms2 and cdms2.isVariable(cell_bounds):
        cell_bounds = numpy.ascontiguousarray(cell_bounds.filled())
    elif isinstance(cell_bounds,(list,tuple)):
        cell_bounds = numpy.ascontiguousarray(cell_bounds)
        
    if cell_bounds is not None:
        if numpy.rank(cell_bounds)>2:
            raise Exception, "Error cell_bounds rank must be at most 2"
        if numpy.rank(cell_bounds)==2:
            if cell_bounds.shape[0]!=coord_vals.shape[0]:
                raise Exception, "Error, coord_vals and cell_bounds do not have the same length"
            if cell_bounds.shape[1]!=2:
                raise Exception, "Error, cell_bounds' second dimension must be of length 2"
            cbnds = 2
            cell_bounds = numpy.ascontiguousarray(numpy.ravel(cell_bounds))
        else:
            cbnds = 1
            if len(cell_bounds)!=len(coord_vals)+1:
                raise Exception, "error cell_bounds are %i long and axes coord_vals are %i long this is not consistent" % (len(cell_bounds),len(coord_vals))
    else:
        cbnds = 0

    if coord_vals is not None:
        l = len(coord_vals)
        type = coord_vals.dtype.char[0]

        if not type in ['i','l','f','d','S']:
            raise Exception, "error allowed data type are: i,l,f,d or S"

        if type == 'S':
            type = 'c'
            cbnds = 0
            for s in coord_vals:
                #print 'testing:',s,len(s)
                if len(s)>cbnds:
                    cbnds = len(s)
            #cbnds+=1
    else:
        l = 0
        type = 'd'
        
    if cell_bounds is not None:
        if type !=cell_bounds.dtype.char:
            cell_bounds = cell_bounds.astype(type)
            
    if units is None:
        if coord_vals is not None:
            raise Exception, "Error you need to provide the units your coord_vals are in"
        else:
            units = "1"
    
    if interval is None:
        interval = ""

    if length is not None:
        l = int(length)
        
    return _cmor.axis(table_entry,units,l,coord_vals,type,cell_bounds,cbnds,interval)

def variable(table_entry,units,axis_ids,type='f',missing_value=None,tolerance = 1.e-4,positive=None,original_name=None,history=None,comment=None):
    
    if not isinstance(table_entry,str):
        raise Exception, "Error you must pass a string for the variable table_entry"
    
    if not isinstance(units,str):
        raise Exception, "Error you must pass a string for the variable units"

    if original_name is not None:
        if not isinstance(original_name,str):
            raise Exception, "Error you must pass a string for the variable original_name"
    else:
        original_name = ""
        
    if history is not None:
        if not isinstance(history,str):
            raise Exception, "Error you must pass a string for the variable history"
    else:
        history = ""
        
    if comment is not None:
        if not isinstance(comment,str):
            raise Exception, "Error you must pass a string for the variable comment"
    else:
        comment = ""
        
    if numpy.ma.isMA(axis_ids):
        axis_ids = numpy.ascontiguousarray(axis_ids.filled())
    elif has_oldma and numpy.oldnumeric.ma.isMA(axis_ids):
        axis_ids = numpy.ascontiguousarray(axis_ids.filled())
    elif has_cdms2 and cdms2.isVariable(axis_ids):
        axis_ids = numpy.ascontiguousarray(axis_ids.filled())
    elif isinstance(axis_ids,(list,tuple)):
        axis_ids = numpy.ascontiguousarray(axis_ids)
    elif not isinstance(axis_ids, numpy.ndarray):
        raise Exception, "Error could not convert axis_ids list to a numpy array"

    if numpy.rank(axis_ids)>1:
        raise Exception, "error axis_ids list/array must be 1D"

    if not isinstance(type,str):
        raise Exception, "error tpye must a a string"
    type = type.lower()
    if type == 's':
        type ='c'
    if not type in ["c","d","f","l","i"]:
        raise Exception, 'error unknown type: "%s", must be one of: "c","d","f","l","i"'

    ndims = len(axis_ids)

    if positive is None:
        positive = ""
    else:
        positive = str(positive)

    if history is None:
        history = ""
    else:
        history = str(history)

    if comment is None:
        comment = ""
    else:
        comment = str(comment)

    if not isinstance(tolerance,(float,int,numpy.float,numpy.float32,numpy.int,numpy.int32)):
        raise Exception, "error tolerance must be a number"

    tolerance = float(tolerance)

    if missing_value is not None:
        if not isinstance(missing_value,(float,int,numpy.float,numpy.float32,numpy.int,numpy.int32)):
            raise Exception, "error missing_value must be a number, you passed: %s" % repr(missing_value)

        missing_value = float(missing_value)

    axis_ids=axis_ids.astype('i')
    return _cmor.variable(table_entry,units,ndims,axis_ids,type,missing_value,tolerance,positive,original_name,history,comment)
    
def zfactor(zaxis_id,zfactor_name,units="",axis_ids=None,type=None,zfactor_values=None,zfactor_bounds=None):

    if not isinstance(zaxis_id,(int,numpy.int,numpy.int32)):
        raise Exception, "error zaxis_id must be a number"
    zaxis_id = int(zaxis_id)

    if not isinstance(zfactor_name,str):
        raise Exception, "Error you must pass a string for the variable zfactor_name"
    
    if not isinstance(units,str):
        raise Exception, "Error you must pass a string for the variable units"

    if numpy.ma.isMA(axis_ids):
        axis_ids = numpy.ascontiguousarray(axis_ids.filled())
    elif has_oldma and numpy.oldnumeric.ma.isMA(axis_ids):
        axis_ids = numpy.ascontiguousarray(axis_ids.filled())
    elif has_cdms2 and cdms2.isVariable(axis_ids):
        axis_ids = numpy.ascontiguousarray(axis_ids.filled())
    elif has_cdms2 and cdms2.isVariable(axis_ids):
        axis_ids = numpy.ascontiguousarray(axis_ids.filled())
    elif isinstance(axis_ids,(list,tuple)):
        axis_ids = numpy.ascontiguousarray(axis_ids)
    elif axis_ids is None:
        pass
    elif isinstance(axis_ids,(int,numpy.int,numpy.int32)):
        axis_ids = numpy.array([axis_ids,])
    elif not isinstance(axis_ids, numpy.ndarray):
        raise Exception, "Error could not convert axis_ids list to a numpy array"

    if numpy.rank(axis_ids)>1:
        raise Exception, "error axis_ids list/array must be 1D"

    if axis_ids is None:
        ndims = 0
        axis_ids = numpy.array(1)
    else:
        ndims = len(axis_ids)

##     if ndims>1 and zfactor_values is not None:
##         raise Exception, "Error you can only pass zfactor_values for zfactor with rank <=1"
##     if ndims>1 and zfactor_bounds is not None:
##         raise Exception, "Error you can only pass zfactor_bounds for zfactor with rank <=1"

    if zfactor_values is not None:
        if isinstance(zfactor_values,(float,int,numpy.float,numpy.float32,numpy.int,numpy.int32)):
            zfactor_values = numpy.array((zfactor_values,))
        elif numpy.ma.isMA(zfactor_values):
            zfactor_values = numpy.ascontiguousarray(zfactor_values.filled())
        elif has_oldma and numpy.oldnumeric.ma.isMA(zfactor_values):
            zfactor_values = numpy.ascontiguousarray(zfactor_values.filled())
        elif has_cdms2 and cdms2.isVariable(zfactor_values):
            zfactor_values = numpy.ascontiguousarray(zfactor_values.filled())
        elif isinstance(zfactor_values,(list,tuple)):
            zfactor_values = numpy.ascontiguousarray(zfactor_values)
        elif not isinstance(zfactor_values, numpy.ndarray):
            raise Exception, "Error could not convert zfactor_values to a numpy array"

        if type is None:
            try:
                type = zfactor_values.dtype.char
            except:
                if isinstance(zfactor_values,(float,numpy.float,numpy.float32)):
                    type = 'f'
                elif isinstance(zfactor_values,(int,numpy.int,numpy.int32)):
                    type = 'd'
                else:
                    raise Exception, "Error unknown type for zfactor_values: %s" % repr(zfactor_values)
    elif type is None:
        type='d'
        
        
    if not isinstance(type,str):
        raise Exception, "error tpye must a a string"
    type = type.lower()
    if type == 's':
        type ='c'
    if not type in ["c","d","f","l","i"]:
        raise Exception, 'error unknown type: "%s", must be one of: "c","d","f","l","i"'
    
    if zfactor_bounds is not None:
        if numpy.ma.isMA(zfactor_bounds):
            zfactor_bounds = numpy.ascontiguousarray(zfactor_bounds.filled())
        elif has_oldma and numpy.oldnumeric.ma.isMA(zfactor_bounds):
            zfactor_bounds = numpy.ascontiguousarray(zfactor_bounds.filled())
        elif has_cdms2 and cdms2.isVariable(zfactor_bounds):
            zfactor_bounds = numpy.ascontiguousarray(zfactor_bounds.filled())
        elif isinstance(zfactor_bounds,(list,tuple)):
            zfactor_bounds = numpy.ascontiguousarray(zfactor_bounds)
        elif not isinstance(zfactor_bounds, numpy.ndarray):
            raise Exception, "Error could not convert zfactor_bounds to a numpy array"
        if numpy.rank(zfactor_bounds)>2:
            raise Exception, "error zfactor_bounds must be rank 2 at most"
        elif numpy.rank(zfactor_bounds)==2:
            if zfactor_bounds.shape[1]!=2:
                raise Exception, "error zfactor_bounds' 2nd dimension must be of length 2"
            bnds =[]
            b = zfactor_bounds[0]
            for i in range(zfactor_bounds.shape[0]):
                b = zfactor_bounds[i]
                bnds.append(b[0])
                if (i<zfactor_bounds.shape[0]-1) and (b[1]!=zfactor_bounds[i+1][0]):
                    raise Exception, "error zfactor_bounds have gaps between them"
            bnds.append(zfactor_bounds[-1][1])
            zfactor_bounds=numpy.array(bnds)
    axis_ids = axis_ids.astype('i')

##     print "sending",zaxis_id,zfactor_name,units,ndims,axis_ids,type,zfactor_values,zfactor_bounds
    return _cmor.zfactor(zaxis_id,zfactor_name,units,ndims,axis_ids,type,zfactor_values,zfactor_bounds)

def write(var_id,data,ntimes_passed=None,file_suffix="",time_vals=None,time_bnds=None,store_with=None):

    """ write data to a cmor variable
    Usage:
    ierr = write(var_id,data,ntimes_passed=None,file_suffix="",time_vals=None,time_bnds=None,store_with=None
    """
    if not isinstance(var_id,(int,numpy.int,numpy.int32)):
        raise Exception, "error var_id must be an integer"
    var_id = int(var_id)

    if not isinstance(file_suffix,str):
        raise Exception,  "Error file_suffix must be a string"

    if store_with is not None:
        if not isinstance(store_with,(int,numpy.int,numpy.int32)):
            raise Exception, "error store_with must be an integer"
        store_with = int(store_with)

    if numpy.ma.isMA(data):
        data = numpy.ascontiguousarray(data.filled())
    elif has_oldma and numpy.oldnumeric.ma.isMA(data):
        data = numpy.ascontiguousarray(data.filled())
    elif has_cdms2 and cdms2.isVariable(data):
        if time_vals is None:
            time_vals = data.getTime() 
        data = numpy.ascontiguousarray(data.filled())
    elif isinstance(data,(list,tuple)):
        data = numpy.ascontiguousarray(data)
    elif not isinstance(data, numpy.ndarray):
            raise Exception, "Error could not convert data to a numpy array"


    if time_vals is None:
        pass
    elif numpy.ma.isMA(time_vals):
        time_vals = numpy.ascontiguousarray(time_vals.filled())
    elif has_oldma and numpy.oldnumeric.ma.isMA(time_vals):
        time_vals = numpy.ascontiguousarray(time_vals.filled())
    elif has_cdms2 and isinstance(time_vals,cdms2.axis.TransientAxis):
        if time_bnds is None:
            time_bnds = time_vals.getBounds()
        time_vals = numpy.ascontiguousarray(time_vals[:])
    elif has_cdms2 and cdms2.isVariable(time_vals):
        time_vals = numpy.ascontiguousarray(time_vals.filled())
    elif isinstance(time_vals,(list,tuple)):
        time_vals = numpy.ascontiguousarray(time_vals)
    elif not isinstance(time_vals, numpy.ndarray):
        try:
            time_vals = numpy.ascontiguousarray(time_vals)
        except:
            raise Exception, "Error could not convert time_vals to a numpy array"

    if time_vals is not None:
        type = time_vals.dtype.char
        if not type in ['f','d','i','l']:
            raise Exception, "Error time_vals type must one of: 'f','d','i','l', please convert first"
        time_vals=time_vals.astype("d")
       

    if ntimes_passed is None:
        if time_vals is None:
            ntimes_passed = 0
        else:
            ntimes_passed = len(time_vals)
    if not isinstance(ntimes_passed,(int,numpy.int,numpy.int32)):
        raise Exception, "error ntimes_passed must be an integer"
    ntimes_passed = int(ntimes_passed)


    #At that ponit we check that shapes matches!
    goodshape = _cmor.get_original_shape(var_id,1)
    osh = data.shape
    ogoodshape=list(goodshape)
    sh=list(osh)
    j=0
    while sh.count(1)>0:
        sh.remove(1)
    while goodshape.count(1)>0:
        goodshape.remove(1)
    for i in range(len(goodshape)):
        if goodshape[i]!=0:
            if sh[j]!=goodshape[i]:
                if goodshape[i]!=1:
                    raise Exception,"error your data shape (%s) does not match the expect variable shape (%s)" % (str(osh),str(ogoodshape))
            j+=1
        elif ntimes_passed!=1:
            j+=1



    data = numpy.ascontiguousarray(numpy.ravel(data))


    if time_bnds is not None:
        if numpy.ma.isMA(time_bnds):
            time_bnds = numpy.ascontiguousarray(time_bnds.filled())
        elif has_oldma and numpy.oldnumeric.ma.isMA(time_bnds):
            time_bnds = numpy.ascontiguousarray(time_bnds.filled())
        elif has_cdms2 and cdms2.isVariable(time_bnds):
            if time_vals is None:
                time_vals = time_bnds.getTime() 
            time_bnds = numpy.ascontiguousarray(time_bnds.filled())
        elif isinstance(time_bnds,(list,tuple)):
            time_bnds = numpy.ascontiguousarray(time_bnds)
        elif not isinstance(time_bnds, numpy.ndarray):
            raise Exception, "Error could not convert time_bnds to a numpy array"

        if numpy.rank(time_bnds)>2:
            raise Exception, "bounds rank cannot be greater than 2"
        elif numpy.rank(time_bnds)==2:
            if time_bnds.shape[1]!=2:
                raise Exception, "error time_bnds' 2nd dimension must be of length 2"
            bnds =[]
            if time_bnds.shape[0] > 1:
                _check_time_bounds_contiguous(time_bnds)
                bnds = _flatten_time_bounds(time_bnds)
            else:
                bnds = time_bnds.ravel()
            time_bnds=numpy.array(bnds)
        else: # ok it is a rank 1!
            if numpy.rank(time_vals)==0:
                ltv=1
            else:
                ltv=len(time_vals)
            if len(time_bnds)!=ltv+1:
                raise Exception,"error time_bnds if 1D must be 1 elt greater than time_vals, you have %i vs %i" % (len(time_bnds),ltv)
            bnds=[]
            for i in range(ltv):
                bnds.append([time_bnds[i],time_bnds[i+1]])
            bnds=numpy.array(bnds)
            bnds = _flatten_time_bounds(bnds)
            time_bnds=numpy.array(bnds)

    if time_bnds is not None:
        type = time_bnds.dtype.char
        if not type in ['f','d','i','l']:
            raise Exception, "Error time_bnds type must one of: 'f','d','i','l', please convert first"
        time_bnds=time_bnds.astype("d")

    type = data.dtype.char
    if not type in ['f','d','i','l']:
        raise Exception, "Error data type must one of: 'f','d','i','l', please convert first"

    return _cmor.write(var_id,data,type,file_suffix,ntimes_passed,time_vals,time_bnds,store_with)

def _check_time_bounds_contiguous(time_bnds):
    '''
    checks that time bounds are contiguous
    '''
    for i in range(time_bnds.shape[0] - 1 ):
        b = time_bnds[i]
        if b[1]!=time_bnds[i+1][0]:
            raise Exception, "error time_bnds have gaps between them"

def _flatten_time_bounds(time_bnds):
    '''
    return a 1-d list of the time_bnds flattened appropriate for the C call
    '''
    bnds = list()
    for i in range(time_bnds.shape[0]):
        bnds.extend([time_bnds[i][0], time_bnds[i][1]])
    return bnds

def setup(inpath='.',netcdf_file_action=cmor_const.CMOR_PRESERVE,set_verbosity=cmor_const.CMOR_NORMAL,exit_control=cmor_const.CMOR_NORMAL,logfile=None,create_subdirectories=1):
    """
    Usage cmor_setup(inpath='.',netcdf_file_action=cmor.CMOR_PRESERVE,set_verbosity=cmor.CMOR_NORMAL,exit_control=cmor.CMOR_NORMAL)
    Where:
    path:                  Alternate directory where to find tables if not in current directory
    netcdf_file_action:    What to do when opening the netcdf file, valid options are:
                           CMOR_PRESERVE, CMOR_APPEND, CMOR_REPLACE, CMOR_PRESERVE_4, CMOR_APPEND_4, CMOR_REPLACE_4, CMOR_PRESERVE_3, CMOR_APPEND_3 or CMOR_REPLACE_3
                           The _3 means netcdf will be created in the old NetCDF3 format (no compression nor chunking), _4 means use NetCDF4 classic format. No _ is equivalent to _3

    set_verbosity:         CMOR_QUIET or CMOR_NORMAL
    exit_control:          CMOR_EXIT_ON_WARNING, CMOR_EXIT_ON_MAJOR, CMOR_NORMAL
    create_subdirectories: 1 to create subdirectories structure, 0 to dump files directly where cmor_dataset tells to
"""
    if not isinstance(exit_control,int) or not exit_control in [ cmor_const.CMOR_EXIT_ON_WARNING, cmor_const.CMOR_EXIT_ON_MAJOR, cmor_const.CMOR_NORMAL]:
        raise Exception, "exit_control must an integer valid values are: CMOR_EXIT_ON_WARNING, CMOR_EXIT_ON_MAJOR, CMOR_NORMAL"

    if not isinstance(netcdf_file_action,int) or not netcdf_file_action in [ cmor_const.CMOR_PRESERVE, cmor_const.CMOR_APPEND, cmor_const.CMOR_REPLACE, cmor_const.CMOR_PRESERVE_3, cmor_const.CMOR_APPEND_3, cmor_const.CMOR_REPLACE_3,cmor_const.CMOR_PRESERVE_4, cmor_const.CMOR_APPEND_4, cmor_const.CMOR_REPLACE_4 ]:
        raise Exception, "netcdf_file_action must be an integer. Valid values are: CMOR_PRESERVE, CMOR_APPEND, CMOR_REPLACE, CMOR_PRESERVE_3, CMOR_APPEND_3 or CMOR_REPLACE_3, CMOR_PRESERVE_4, CMOR_APPEND_4 or CMOR_REPLACE_4"

    if not isinstance(set_verbosity,int) or not set_verbosity in [ cmor_const.CMOR_QUIET, cmor_const.CMOR_NORMAL]:
        raise Exception, "set_verbosity must an integer valid values are: CMOR_QUIET, CMOR_NORMAL"

    if not isinstance(inpath,str) and not os.path.exists(inpath):
        raise Exception, "path must be a Valid path"
    if logfile is None:
        logfile = ""

    if not create_subdirectories in [0,1]:
        raise Exception, "create_subdirectories must be 0 or 1"
    return _cmor.setup(inpath,netcdf_file_action,set_verbosity,exit_control,logfile,create_subdirectories)

def load_table(table):
    """ loads a cmor table
    Usage:
    load_table(table)
    """
    if not isinstance(table,str):
        raise Exception, "Error, must pass a string"
##     if not os.path.exists(table):
##         raise Exception, "Error, the table you specified (%s) does not exists" % table
    return _cmor.load_table(table)

def dataset(experiment_id,institution,source,calendar,outpath='.',realization=1,contact="",history="",comment="",references="",leap_year=0,leap_month=0,month_lengths=None,model_id="",forcing="",initialization_method=None,physics_version=None,institute_id="",parent_experiment_id="",branch_time=None,parent_experiment_rip=""):
    """ Initialize a cmor dataset 
    Usage:
    dataset(experiment_id,institution,source,outpath='.',calendar=None,realization=None,contact=None,history="",comment="",references="",leap_year=None,leap_month=None,month_lengths=None,model_id="",forcing="",initialization_method=None,physics_version=None,institute_id="",parent_experiment_id="",branch_time=None,parent_experiment_rip="")
    """

    if isinstance(calendar,int):
        if has_cdtime: # put this in a try loop in case cdtime is not available on the system 
            if calendar == cdtime.Calendar360:
                calendar = "360_day"
            elif calendar == cdtime.ClimCalendar:
                calendar = "clim"
            elif calendar == cdtime.DefaultCalendar:
                calendar = "standard"
            elif calendar == cdtime.GregorianCalendar:
                calendar = "proleptic_gregorian"
            elif calendar == cdtime.JulianCalendar:
                calendar = "julian"
            elif calendar == cdtime.MixedCalendar:
                calendar= "standard"
            elif calendar == cdtime.NoLeapCalendar:
                calendar = "noleap"
            elif calendar == cdtime.StandardCalendar:
                calendar = "proleptic_gregorian"
            elif calendar == cdtime.ClimLeapCalendar:
                calendar = "climleap"
    elif calendar is None:
        calendar ="none"
                
    for st in [outpath,experiment_id,institution,source,contact,history,comment,references,model_id,forcing,institute_id,parent_experiment_id,parent_experiment_rip]:
        if not isinstance(st,str):
            for o in dir():
                if locals()[o] is st:
                    raise Exception, "Error argument %s must be a string" % o

    calendar = calendar.lower()
    
    for i in [realization,leap_year,leap_month]:
        if not isinstance(i,int):
            for o in dir():
                if locals()[o] is i:
                    raise Exception, "Error argument %s must be an integer" % o
    if isinstance(month_lengths,(list,tuple)):
        month_lengths = numpy.array(month_lengths)
    elif has_cdms2 and cdms2.isVariable(month_lengths):
        month_lengths = month_lengths.filled()
    elif isinstance(month_lengths,(numpy.ma.core.MaskedArray)):
        month_lengths = month_lengths.filled()
    elif has_oldma and isinstance(month_lengths,numpy.oldnumeric.ma.MaskedArray):
        month_lengths = month_lengths.filled()
        
    if isinstance(month_lengths,numpy.ndarray):
        if not numpy.rank(month_lengths)==1:
            raise Exception, "Error month_lengths must be 1D"
        if len(month_lengths)!=12:
            raise Exception, "Error month_lengths must have 12 elements exactly"
        months_lengths = numpy.ascontiguousarray(month_lengths)
    elif month_lengths is not None:
        raise Exception, "Error month_lengths must be a 12 elts array or list"
    if initialization_method is not None:
        if not isinstance(initialization_method,int):
            raise Exception, "initialization_method must be an int"
    else:
        initialization_method=0
    if physics_version is not None:
        if not isinstance(physics_version,int):
            raise Exception, "physics_version must be an int"
    else:
        physics_version=0

    if branch_time is not None:
        if not isinstance(branch_time,(float,int,numpy.float,numpy.float32,numpy.int,numpy.int32)):
            raise Exception,"branch_time must be a float"
        else:
            branch_time=float(branch_time)

    return _cmor.dataset(outpath,experiment_id,institution,source,calendar,realization,contact,history,comment,references,leap_year,leap_month,month_lengths,model_id,forcing,initialization_method,physics_version,institute_id,parent_experiment_id,branch_time,parent_experiment_rip)

def set_table(table):
    if not isinstance(table,int):
        raise Exception, "error you need to pass and integer as the table id"
    return _cmor.set_table(table)

def close(var_id=None,file_name=False, preserve=False):
    """ Close CMOR variables/file
    Usage:
      cmor.close(varid=None)
    Where:
      var_id: id of variable to close, if passing None, means close every open ones.
      [file_name] True/False (default False) if True: return name of the file just closed, works only if var_id is not None
      [preserve] True/False (default False) if True: close the file but preserve the var definition in CMOR to write more data with this variable (into a new file)
      """
    if var_id is not None and not isinstance(var_id,int):
        raise Exception, "Error var_id must be None or a integer"

    if (preserve is False):
        if (file_name is False):
            return _cmor.close(var_id,0,0)
        else:
            return _cmor.close(var_id,1,0)
    else:
        if (file_name is False):
            return _cmor.close(var_id,0,1)
        else:
            return _cmor.close(var_id,1,1)
        

def set_cur_dataset_attribute(name,value):
    """Sets an attribute onto the current cmor dataset
    Usage:
      cmor.set_cur_dataset_attribute(name,value)
    Where:
      name: is the name of the attribute
      value: is the value for this attribute
    """
    if value is None:
        val=""
    else:
        val = str(value)
    return _cmor.set_cur_dataset_attribute(name,val)

def has_cur_dataset_attribute(name):
    """determines if the current cmor dataset has an attribute
    Usage:
      cmor.het_cur_dataset_attribute(name)
    Where:
      name: is the name of the attribute
    Returns True if the dataset has the attribute, False otherwise
    """
    test = _cmor.has_cur_dataset_attribute(name)
    if test == 0 :
        return True
    else:
        return False

def get_cur_dataset_attribute(name):
    """Gets an attribute from the current cmor dataset
    Usage:
      cmor.get_cur_dataset_attribute(name)
    Where:
      name: is the name of the attribute
    Returns none if attribute is non-existant
    """
    if has_cur_dataset_attribute(name):
        return _cmor.get_cur_dataset_attribute(name)
    else:
        return None
    
def set_variable_attribute(var_id,name,value):
    """Sets an attribute onto a cmor variable
    Usage:
      cmor.set_variable_attribute(var_id,name,value)
    Where:
      var_id: is cmor variable id
      name  : is the name of the attribute
      value : is the value for this attribute
    """
    if value is None:
        val=""
    else:
        val = str(value)
    return _cmor.set_variable_attribute(var_id,name,val)

def has_variable_attribute(var_id,name):
    """determines if the a cmor variable has an attribute
    Usage:
      cmor.het_variable_attribute(name)
    Where:
      var_id: is cmor variable id
      name: is the name of the attribute
    Returns True if the dataset has the attribute, False otherwise
    """
    test = _cmor.has_variable_attribute(var_id,name)
    if test == 0 :
        return True
    else:
        return False

def get_variable_attribute(var_id,name):
    """Gets an attribute from a cmor variable
    Usage:
      cmor.get_variable_attribute(name)
    Where:
      var_id: is cmor variable id
      name: is the name of the attribute
    Returns none if attribute is non-existant
    """
    ## print 'In there asking for attribute: ',name,'on var',var_id
    if has_variable_attribute(var_id,name):
        ## print 'Seems to have it',var_id,name
        return _cmor.get_variable_attribute(var_id,name)
    else:
        return None
    
    
def create_output_path(varid):
    """returns the output path where a variable would be stored, given a varid (as returned by a call to cmor.variable)
    Usage:
      path = create_output_path(var_id)
    Where:
      var_id : id to variable as returned by a call to variable(...)
    Returns: the path where the file will be created
    """
    return _cmor.create_output_path(varid)
