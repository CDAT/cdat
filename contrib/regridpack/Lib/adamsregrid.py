# Adapted for numpy/ma/cdms2 by convertcdms.py

"""
  
  INTRODUCTION
  
    This module provides access through Python to the collection of Fortran programs in
    REGRIDPACK, which is a collection of programs produced at the National Center for 
    Atmospheric Research by John C. Adams for linear or cubic interpolation in one, two,
    three or four dimensions. 
  
  RESTRICTIONS
  
    This module provides regridding by interpolation only. In other words, the output coordinate
    vector can not extend beyond the limits of the input coordinate vector.
 
    Each coordinate vector must be monotonically increasing or decreasing.
 
    Missing data is not allowed in the input data.  
  
  
  CAPABLITIES 
  
    This module allows linear or cubic interpolation in one, two, three or four dimensions. The
    computation can use the grid vectors assocated with the input and the output data or just
    thier sizes if the input and output vectors are uniformly spaced. However, the selection of
    of the uniform option must apply to all the dimensions in the requested interpolation. In
    addition, the computation in the nonuniform case can use the log of the grid vectors choosen
    dimension by dimension. It is possible to regrid a subset of the dimensions in the input data.
    A utility function is provide to generate a gaussian or a uniform grids to use as a output
    coordinate vector.
 
  
  ORGANIZATION 
 
    This module is object oriented for simplicity. It is organized as a single class called Regrid,
    which begins with a capital letter. Python is case sensitive. It contains the single function,
    rgrd, which performs the computation in one, two, three or four dimensions.
 
    Access to the function rgrd is provided through a simple two step process. The first step is making
    an instance of the class passing the coordinate vectors and associated information.  The second step
    is calling the regridding function with the data in the argument list and an optional missing data value
    to request a check for the presence of a missing data.
  
  HELP 
 
    To obtain a prescription for making an instance,  type

                adamsregrid.help('Regrid')
 
    To acquire instructions on the use of the rgrd function, type
 
                adamsregrid.help('rgrd')
 
    To look at a general one dimensional example, type
 
                adamsregrid.help('OneDexample')
 
    To look at a general four dimensional example, type
 
                adamsregrid.help('FourDexample')
 
   DOCUMENTATION
  
        Documentation written to the file regridmodule.doc can be produced after importing the adamsregrid module
        by typing 
  
               adamsregrid.document() 
  
        As an alternate to using the help package, online documentation is available from three individual doctrings
        by using
  
               print adamsregrid.Regrid.__doc__           -- documentation for the module. 
               print adamsregrid.Regrid.__init__.__doc__  -- documentation for making an instance. 
               print adamsregrid.Regrid.rgrd.__doc__      -- documentation for the rgrd method. 
         
   TESTING 
 
        After importing adamstest, typing 
         
            cdat adamstest.py
         
        generates some testing of the adamsregridmodule using analytical functions as fields. It also
        writes a hard copy of the documentation to the file regridmodule.doc and a copy of the information
        displayed on the screen to screen.asc. 
         
"""
import sys, string
import regridpack, numpy, math
#regridpack.set_pyfort_option(regridpack.MIRROR)
debug = 0                                           # set to 1 for debug prints

usefilled = 'yes'
try:
    import numpy.ma
except ImportError:
    print 'Can not convert from numpy.ma to numpy array without module numpy.ma'
    print 'Can not check for pressence of missing data without module numpy.ma'
    usefilled = 'no'

class Regrid:

    def __init__(self, x = None, xp = None, xtype = None, xdim = None, y = None, yp = None, ytype = None,
      ydim = None, z = None, zp = None, ztype = None, zdim = None, t = None, tp = None, ttype = None, tdim = None): 

        """    --------------------------------------------------------------------------------------------------------
      
         PREPARATION 
      
                The user selects the dimensions to regrid by interpolation. The remaining dimensions,
                if any, are unaffected. In addition, each selected dimension may have a linear or
                cubic treatment independently of the others. 
      
                For each dimension, the user must assemble four pieces of information (the
                coordinateVectorIn, the coordinateVectorOut, the interpolationChoice and the
                dataDimensionPosition) which have the following meanings: 
     
                    coordinateVectorIn -  the coordinate vector associated with the input data requiring
                                          interpolation or the number of points for uniform case.
     
                    coordinateVectorOut -  the coordinate vector associated with the output data after
                                           interpolation or the number of points for uniform case.
     
                    interpolationChoice -  the interpolation scheme as linear ('linear'), linear in the log
                                           ('linearLog') , cubic ('cubic') or cubic in the log ('cubicLog')
                                           interpolation.
     
                    dataDimensionPosition - an integer in the range 0 to 3 identifying the location of the 
                                            dimension in the input data array in Python's order.
     
                However, if any dimension is treated as uniform by passing the size rather than the dimension
                vector, all dimensions interpolated must be uniform. 
     
                     
         USAGE -- MAKING AN INSTANCE:                      
                     
                    1D interpolation
           
                             r=adamsregrid.Regrid(xcoordinateVectorIn, xcoordinateVectorOut,
                                             xinterpolationChoice, xdataDimensionPosition)
                    2D interpolation
     
                             r=adamsregrid.Regrid(xcoordinateVectorIn, xcoordinateVectorOut,
                                             xinterpolationChoice, xdataDimensionPosition,
     
                                             ycoordinateVectorIn, ycoordinateVectorOut,
                                             yinterpolationChoice, ydataDimensionPosition)
     
                    3D interpolation
     
                             r=adamsregrid.Regrid(xcoordinateVectorIn, xcoordinateVectorOut,
                                             xinterpolationChoice, xdataDimensionPosition,
     
                                             ycoordinateVectorIn, ycoordinateVectorOut,
                                             yinterpolationChoice, ydataDimensionPosition,
     
                                             zcoordinateVectorIn, zcoordinateVectorOut,
                                             zinterpolationChoice, zdataDimensionPosition)
     
     
                    4D interpolation
     
                             r=adamsregrid.Regrid(xcoordinateVectorIn, xcoordinateVectorOut,
                                             xinterpolationChoice, xdataDimensionPosition,
     
                                             ycoordinateVectorIn, ycoordinateVectorOut,
                                             yinterpolationChoice, ydataDimensionPosition,
     
                                             zcoordinateVectorIn, zcoordinateVectorOut,
                                             zinterpolationChoice, zdataDimensionPosition,
     
                                             tcoordinateVectorIn, tcoordinateVectorOut,
                                             tinterpolationChoice, tdataDimensionPosition)
     
                    where the prefixes x, y, z and t distinguish the four dimensions. The association of x,
                    y, z and t with physical dimensions is at the discretion of the user.
                     
         EFFICIENCY NOTE:                     
                     
                The choice for the order used in selecting the interpolations determines the need for
                transposing the data. The data is transposed so that x is on the right with y, z antd t
                following. This order is the one that is given by python's shape function. The need for
                a transpose can be avoided by using the order already in the data set.
     
         DEFINITION: __init__(self, x = None, xp = None, xtype = None, xdim = None,
                                        y = None, yp = None, ytype = None, ydim = None, 
                                        z = None, zp = None, ztype = None, zdim = None,
                                        t = None, tp = None, ttype = None, tdim = None): 
         
                where as notation the required input is
         
                        x = input grid description - the x vector or number of x points for uniform case
                        xp = output grid description - the x vector or number of x points for uniform case
                        xtype = regrid using linear, linear_log, cubic, or cubic_log interpolation
                        xdim = location of x dimension in the data
         
                and as optional input the same information as required for x but for y, z and t depending
                on the number of dimensions requiring interpolation.
    --------------------------------------------------------------------------------------------------------""" 

        # Find the number of dimensions, check for completeness, call checkdimension to return
        # monotonically increasing grid vectors, the log of the vectors if requested and create instance data.

        self.dimdict = {}     # make dictionary to keep track of pertinent dimension information

        if x is None or xp is None or xtype is None or xdim is None:      # x dimension is necessary
            msg = 'CANNOT CREATE INSTANCE - Must specify the x dimension completely with four entries'
            raise ValueError, msg

        elif y is None:                            # 1D interpolation         

            try:                                               # try is successful for general grids
                size = len(x)

                xreverse, xpreverse, x, xp = checkdimension(x, xp, xtype)
                self.dimdict['x'] = (xdim, xreverse, xpreverse) 
                self.requestSize = (len(xp),)
                self.gridComp = Rgrd1(x, xp, xtype)               # 1D instance
            except TypeError:
                self.dimdict['x'] = (xdim, 'no', 'no') 
                self.requestSize = (xp,)
                self.gridComp = Rgrd1u(x, xp, xtype)              # 1Du instance

        elif z is None:                           # 2D interpolation         

            if y is None or yp is None or ytype is None or ydim is None:
                msg = 'CANNOT CREATE INSTANCE - Must specify the y dimension completely with four entries'
                raise ValueError, msg

            try: 
                size = len(x) 

                xreverse, xpreverse, x, xp = checkdimension(x, xp, xtype)
                yreverse, ypreverse, y, yp = checkdimension(y, yp, ytype)
                self.dimdict['x'] = (xdim, xreverse, xpreverse) 
                self.dimdict['y'] = (ydim, yreverse, ypreverse) 
                self.requestSize = (len(yp), len(xp))
                self.gridComp = Rgrd2(x, xp, xtype, y, yp, ytype)        # 2D instance
            except TypeError:
                try: 
                    size = len(y)
                    msg = 'CANNOT CREATE INSTANCE - If any dimension is uniform, all must be also'
                    raise ValueError, msg
                except TypeError:
                    self.dimdict['x'] = (xdim, 'no', 'no') 
                    self.dimdict['y'] = (ydim, 'no', 'no') 
                    self.requestSize = (yp, xp)
                    self.gridComp = Rgrd2u(x, xp, xtype, y, yp, ytype)   # 2Du instance

        elif t is None:                          # 3D interpolation         

            if y is None or yp is None or ytype is None or ydim is None:
                msg = 'CANNOT CREATE INSTANCE - Must specify the y dimension completely with four entries'
                raise ValueError, msg
            elif z is None or zp is None or ztype is None or zdim is None:
                msg = 'CANNOT CREATE INSTANCE - Must specify the z dimension completely with four entries'
                raise ValueError, msg

            try:
                size = len(x)

                xreverse, xpreverse, x, xp = checkdimension(x, xp, xtype)
                yreverse, ypreverse, y, yp = checkdimension(y, yp, ytype)
                zreverse, zpreverse, z, zp = checkdimension(z, zp, ztype)
                self.dimdict['x'] = (xdim, xreverse, xpreverse) 
                self.dimdict['y'] = (ydim, yreverse, ypreverse) 
                self.dimdict['z'] = (zdim, zreverse, zpreverse) 
                self.requestSize = (len(zp), len(yp), len(xp))
                self.gridComp = Rgrd3(x, xp, xtype, y, yp, ytype, z, zp, ztype)        # 3D instance
            except TypeError:
                try:
                    size = len(y)
                    msg = 'CANNOT CREATE INSTANCE - If any dimension is uniform, all must be also'
                    raise ValueError, msg
                except TypeError:
                    try:                              # make sure y is also uniform
                        size = len(z)
                        msg = 'CANNOT CREATE INSTANCE - If any dimension is uniform, all must be also'
                        raise ValueError, msg
                    except TypeError:
                        self.dimdict['x'] = (xdim, 'no', 'no') 
                        self.dimdict['y'] = (ydim, 'no', 'no') 
                        self.dimdict['z'] = (zdim, 'no', 'no') 
                        self.requestSize = (zp, yp, xp)
                        self.gridComp = Rgrd3u(x, xp, xtype, y, yp, ytype, z, zp, ztype) # 4D instance

        else:                                    # 4D interpolation         

            if y is None or yp is None or ytype is None or ydim is None:
                msg = 'CANNOT CREATE INSTANCE - Must specify the y dimension completely with four entries'
                raise ValueError, msg
            elif z is None or zp is None or ztype is None or zdim is None:
                msg = 'CANNOT CREATE INSTANCE - Must specify the z dimension completely with four entries'
                raise ValueError, msg
            elif tp is None or ttype is None or tdim is None:
                msg = 'CANNOT CREATE INSTANCE - Must specify the t dimension completely with four entries'
                raise ValueError, msg

            try:                                    # try is successful for general grids
                size = len(x)

                xreverse, xpreverse, x, xp = checkdimension(x, xp, xtype)
                yreverse, ypreverse, y, yp = checkdimension(y, yp, ytype)
                zreverse, zpreverse, z, zp = checkdimension(z, zp, ztype)
                treverse, tpreverse, t, tp = checkdimension(t, tp, ttype)
                self.dimdict['x'] = (xdim, xreverse, xpreverse) 
                self.dimdict['y'] = (ydim, yreverse, ypreverse) 
                self.dimdict['z'] = (zdim, zreverse, zpreverse) 
                self.dimdict['t'] = (tdim, treverse, tpreverse) 
                self.requestSize = (len(tp), len(zp), len(yp), len(xp))

                self.gridComp = Rgrd4(x, xp, xtype, y, yp, ytype, z, zp, ztype, t, tp, ttype) # 4D instance
            except TypeError:
                try:                                                  # make sure y is also uniform
                    size = len(y)
                    msg = 'CANNOT CREATE INSTANCE - If any dimension is uniform, all must be also'
                    raise ValueError, msg
                except TypeError:
                    try:                              # make sure y is also uniform
                        size = len(z)
                        msg = 'CANNOT CREATE INSTANCE - If any dimension is uniform, all must be also'
                        raise ValueError, msg
                    except TypeError:
                        try:                                           # make sure y is also uniform
                            size = len(t)
                            msg = 'CANNOT CREATE INSTANCE - If any dimension is uniform, all must be also'
                            raise ValueError, msg
                        except TypeError:
                            self.dimdict['x'] = (xdim, 'no', 'no') 
                            self.dimdict['y'] = (ydim, 'no', 'no') 
                            self.dimdict['z'] = (zdim, 'no', 'no') 
                            self.dimdict['t'] = (tdim, 'no', 'no') 
                            self.requestSize = (tp, zp, yp, xp)
                            self.gridComp = Rgrd4u(x, xp, xtype, y, yp, ytype, z, zp, ztype, t, tp, ttype) 

        # --- check for duplicate dimension numbers in the interpoltion request ---

        alldimdictKeys = ['x', 'y', 'z', 't']         # list of all possible dimdict keys
        self.intpolDimlist =[]                        # extract list of dimensions used in interpolation
        for n in range(len(self.dimdict)):
             value = self.dimdict[alldimdictKeys[n]][0]
             if value < 0 or value > 3:
                 msg = 'CANNOT CREATE INSTANCE -Dimension position value is less than 0 or greater than 3' 
                 raise ValueError, msg
             else:
                 self.intpolDimlist.append(value)

        self.intpolDimlist.reverse()                                                # place x on the far right

        # --- check for duplicate dimension numbers in the interpoltion request ---

        checkdict = {} 
        for n in range(len(self.intpolDimlist)):
           checkdict[ self.intpolDimlist[n] ] = self.intpolDimlist[n] 

        if len(checkdict) < len(self.dimdict):
            msg = 'CANNOT CREATE INSTANCE - Can not handle a duplicate in the dimension list'
            raise ValueError, msg


    def rgrd(self, p, missingValue = None):
        """    --------------------------------------------------------------------------------------------------------

         ROUTINE:   rgrd 
    
         PURPOSE:   rgrd method regrids by linear or cubic interpolation in 1, 2, 3 or 4 dimensions 
    
         USAGE:     
                   q = r.rgrd( p, missingValue)  where 
    
                   r -- an instance of Regrid 
                   p -- the  given data 
                   q --  the data interpolated to another grid 
                   missingValue -- the optional value to use in checking for the presence of missing data, an error
    
         DEFINITION: rgrd(self, p, missingValue = None):
    
    --------------------------------------------------------------------------------------------------------""" 
        if debug == 1:
            print 'performing interpolation'

        if usefilled == 'yes':
            p = numpy.ma.filled(p)

        gridComp = self.gridComp
        dimdict = self.dimdict 
        requestSize = self.requestSize 

        numberIntpol = len(dimdict) 

        # --- Check data type and change to float if necessary ----

        if p.dtype.char != 'f':
            print 'WARNING - data will be converted to float32'
            p = p.astype(numpy.float32)
   
        # ----- Check for missing data -------

        if missingValue is not None and usefilled == 'yes':
            um = numpy.ma.missing_value(u, missingValue)
            if um.mask is not None:
                msg = 'CANNOT PROCESS THE DATA - field has missing data'
                raise ValueError, msg


        # --- check consistency between source coordinate vectors and data sizes ----

        try:                                    # get length of vector or size as stored
            gridSize = len(gridComp.x)
        except TypeError:
            gridSize = gridComp.x

        dataSize = p.shape[dimdict['x'][0]]
        if dataSize != gridSize:
            msg = 'INCONSISTENT INPUT DATA - First dimension grid is %d and data size is %d' % (gridSize, dataSize)
            raise ValueError, msg

        if numberIntpol >= 2:
            try: 
                gridSize = len(gridComp.y)
            except TypeError:
                gridSize = gridComp.y

            dataSize = p.shape[dimdict['y'][0]]
            if dataSize != gridSize:
                msg = 'INCONSISTENT INPUT DATA - Second dimension grid is %d and data size is %d' % (gridSize, dataSize)
                raise ValueError, msg

        if numberIntpol >= 3:
            try: 
                gridSize = len(gridComp.z)
            except TypeError:
                gridSize = gridComp.z

            dataSize = p.shape[dimdict['z'][0]]
            if dataSize != gridSize:
                msg = 'INCONSISTENT INPUT DATA - Third dimension grid is %d and data size is %d' % (gridSize, dataSize)
                raise ValueError, msg

        if numberIntpol >= 4:
            try: 
                gridSize = len(gridComp.t)
            except TypeError:
                gridSize = gridComp.t

            dataSize = p.shape[dimdict['t'][0]]
            if dataSize != gridSize:
                msg = 'INCONSISTENT INPUT DATA - Fourth dimension grid is %d and data size is %d' % (gridSize, dataSize)
                raise ValueError, msg

        # --- put information in dimdict into lists ----

        alldimdictKeys = ['x', 'y', 'z', 't']         # list of all possible dimdict keys

        intpolDimlist = self.intpolDimlist             # list of dimension numbers (x on right)

        sourceRevlist =[]                                    # extract list of dimensions to reverse in source data
        for n in range(len(dimdict)):
            sourceRevlist.append(dimdict[alldimdictKeys[n]][1])

        targetRevlist =[]                                    # extract list of dimensions to reverse in target data
        for n in range(len(dimdict)):
            targetRevlist.append(dimdict[alldimdictKeys[n]][2])

        sizeData = len(p.shape)
        if max(intpolDimlist) > (sizeData - 1):          # check the value of the maximum dimension request 
            msg = 'CANNOT INTERPOLATE - A dimension request exceeds the number of dimensions in the data'
            raise ValueError, msg

        newOrder, inverseOrder, needsTranspose = getnewOrder(intpolDimlist, sizeData)

        if debug == 1:
            print 'dimdict = ', dimdict
            print 'sourceRevlist = ', sourceRevlist
            print 'targetRevlist = ', targetRevlist
            print 'intpolDimlist = ', intpolDimlist
            print 'sizeData = ', sizeData
            print 'needsTranspose = ', needsTranspose
            print 'newOrder = ', newOrder
            print 'inverseOrder = ', inverseOrder

        if needsTranspose == 'yes':                # if needed transpose data with x intpol dimensions on far right
            p = numpy.transpose(p, newOrder)                           
            p = numpy.array(p.astype(numpy.float32), numpy.float32) # make contiguous 

        if 'yes' in sourceRevlist:
            sourceDataSize = len(p.shape)
            slice_list = [slice(None,None,None)]*sourceDataSize        # reverse direction of source data if needed
            for n in range(len(sourceRevlist)):
                if  sourceRevlist[n] == 'yes':
                    axis = sourceDataSize - 1 - n 
                    slice_list[axis] = slice(None,None,-1)
            p = p[slice_list]

        numberLoops = sizeData - numberIntpol

        # --- allocate memory for q  ----

        qshape_list = []                     # list to hold complete shape of q
        for n in range(numberLoops):
            qshape_list.append(p.shape[n]) 
        for n in range(numberIntpol):
            qshape_list.append(requestSize[n]) 
        qshape = tuple(qshape_list)

        q = numpy.zeros(qshape, numpy.float32)

        # --- set up loops with requests on the right - note: loop indices have no x,y,z,t association here  ----
        # --- the data has been arranged with x on the right followed by y, z, t as appliable

        if numberLoops == 0:
            numberCalls = 1
            q = gridComp.rgrd(p)

        elif numberLoops == 1:
            numberCalls = p.shape[0]

            for i in range(p.shape[0]):
                if numberIntpol == 1:
                    q[i,:] = gridComp.rgrd(p[i,:])
                elif numberIntpol == 2:
                    q[i,:,:] = gridComp.rgrd(p[i,:,:])
                elif numberIntpol == 3:
                    q[i,:,:,:] = gridComp.rgrd(p[i,:,:,:])

        elif numberLoops == 2:
            numberCalls = p.shape[0]*p.shape[1]

            for i in range(p.shape[0]):
                for j in range(p.shape[1]):
                    if numberIntpol == 1:
                        q[i,j,:] = gridComp.rgrd(p[i,j,:])
                    elif numberIntpol == 2:
                        q[i,j,:,:] = gridComp.rgrd(p[i,j,:,:])

        elif numberLoops == 3:
            numberCalls = p.shape[0]*p.shape[1]*p.shape[2]

            for i in range(p.shape[0]):
                for j in range(p.shape[1]):
                    for k in range(p.shape[2]):
                        q[i,j,k,:] = gridComp.rgrd(p[i,j,k,:])

        if 'yes' in targetRevlist:
            targetDataSize = len(q.shape)
            slice_list = [slice(None,None,None)]*targetDataSize    # reverse direction of target data if needed
            for n in range(len(targetRevlist)):
                if  targetRevlist[n] == 'yes':
                    axis = targetDataSize - 1 - n 
                    slice_list[axis] = slice(None,None,-1)
            q = q[slice_list]

        if needsTranspose == 'yes':                    # if needed transpose data with intpol dimensions on right
            if debug == 1:
                print 'Doing transpose next'
            q = numpy.transpose(q, inverseOrder)                        # transpose data to original order 
            q = numpy.array(q.astype(numpy.float32), numpy.float32) # make contiguous 

        if debug == 1:
            print 'returning with the interpolated data'

        return q 


# +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# +++++++++++++++++++++++++++++++ General 1D Interpolation ++++++++++++++++++++++++++++++++++
# +++++++++++++++++++++++++++++++++++++++i+++++++++++++++++++++++++++++++++++++++++++++++++++

class Rgrd1:

    def __init__(self, x, xp, xtype):
        #---------------------------------------------------------------------------------------
        #    routine:  __init__
        #
        #    purpose:  'init' assigns values to the instance data 
        # 
        #    usage:     r = adamsregrid.Rgrd1(x,xp,xtype)                      1D interpolation
        #
        #    passed:    x = input grid description - the x vector or number of x points for uniform case
        #               xp = output grid description - the x vector or number of x points for uniform case
        #               xtype = regrid using linear, linear_log, cubic, or cubic_log interpolation
        #
        #    returned:  x -- an instance of the Rgrd1 class to qualify using dot notation to make a computation
        #
        #    definition: __init__(self, x, xp, xtype):
        #
        #---------------------------------------------------------------------------------------
        if debug == 1:
            print 'making instance of Rgrd1'

        self.x = x
        self.xp = xp
        self.xtype = xtype

    def rgrd(self, p):
        #---------------------------------------------------------------------------------------
        #                                      
        #     purpose: linear or cubic interpolation in 1D between given general grids
        #
        #     usage:   q = r.rgrd(p)
        #
        #---------------------------------------------------------------------------------------
        x = self.x 
        nx = len(x)
        xx = self.xp 
        mx = len(xx)

        if self.xtype == 'linear' or self.xtype == 'linearLog':
            intpol = 1
            lw = mx
        elif self.xtype == 'cubic' or self.xtype == 'cubicLog':
            intpol = 3
            lw = 4*mx
        else:
            print 'In 1D rgrd the interpolation type is not set to linear, linearLog, cubic or cubicLog'
            raise ValueError
            return

        liw = mx

        w  = numpy.zeros((lw,),'f')
        iw = numpy.zeros((liw,),'i')
        q, ier = regridpack.rgrd1( x, p, xx, intpol, w, iw)

        if ier != 0:
            print 'In return from call to rgrd1 ier = ', ier
            raise ValueError

        return q 



# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# +++++++++++++++++++++++++++++++ Uniform 1D Interpolation ++++++++++++++++++++++++++++++++++++++++++
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

class Rgrd1u:

    def __init__(self, x, xp, xtype):
        #---------------------------------------------------------------------------------------
        #    routine:  __init__
        #
        #    purpose:  'init' assigns values to the instance data 
        # 
        #    usage:     x = adamsregrid.Rgrd1u(x,xp,xtype)                         1D interpolation
        #
        #    passed:    x = input grid description - the x vector or number of x points for uniform case
        #               xp = output grid description - the x vector or number of x points for uniform case
        #               xtype = regrid using linear, linear_log, cubic, or cubic_log interpolation
        #
        #    returned:  x -- an instance of the Rgrd1 class to qualify using dot notation to make a computation
        #
        #    definition: __init__(self, x, xp, xtype):
        #
        #---------------------------------------------------------------------------------------

        if debug == 1:
            print 'making instance of Rgrd1u'

        self.x = x
        self.xp = xp
        self.xtype = xtype

    def rgrd(self, p):
        #---------------------------------------------------------------------------------------
        #                                      
        #     purpose: linear or cubic interpolation in 1D between uniform grids
        #
        #     usage:   q = r.rgrd(p)
        #
        #---------------------------------------------------------------------------------------
        nx = self.x 
        mx = self.xp 

        if self.xtype == 'linear' or self.xtype == 'linearLog':
            intpol = 1
            lw = mx
        elif self.xtype == 'cubic' or self.xtype == 'cubicLog':
            intpol = 3
            lw = 4*mx
        else:
            print 'In 1D rgrd1 the interpolation type is not set to linear, linearLog, cubic or cubicLog'
            raise ValueError
            return

        if (nx-1)%(mx-1) == 0:              # override choice from above
            lw = 1

        liw = mx

        w  = numpy.zeros((lw,),'f')
        iw = numpy.zeros((liw,),'i')
        q, ier = regridpack.rgrd1u(p, mx, intpol, w, iw)

        if ier!= 0:
            print 'In return from call to rgrd1u ier = ', ier
            raise ValueError

        return q 


# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# +++++++++++++++++++++++++++++++ General 2D Interpolation ++++++++++++++++++++++++++++++++++++++++++
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

class Rgrd2:

    def __init__(self, x, xp, xtype, y, yp, ytype):
        #---------------------------------------------------------------------------------------
        #    routine:  __init__
        #
        #    purpose:  'init' assigns values to the instance data 
        # 
        #    usage:     r = adamsregrid.Rgrd(x,xp,xtype,y,yp,ytype)                      2D interpolation
        #
        #    passed:    x = input grid description - the x vector or number of x points for uniform case
        #               xp = output grid description - the x vector or number of x points for uniform case
        #               xtype = regrid using linear, linear_log, cubic, or cubic_log interpolation
        #
        #               y = input grid description - the y vector or number of y points for uniform case
        #               yp = output grid description - the y vector or number of y points for uniform case
        #               ytype = regrid using linear, linear_log, cubic, or cubic_log interpolation
        #
        #    returned:  x -- an instance of the Rgrd2 class to qualify using dot notation to make a computation
        #
        #    definition: __init__(self, x, xp, xtype, y, yp, ytype):
        #
        #---------------------------------------------------------------------------------------
        if debug == 1:
            print 'making instance of Rgrd2'

        self.x = x
        self.xp = xp
        self.xtype = xtype
        self.y = y
        self.yp = yp
        self.ytype = ytype

    def rgrd(self, p):
        #---------------------------------------------------------------------------------------
        #                                      
        #     purpose: linear or cubic interpolation in 2D between given general grids
        #
        #     usage:   q = r.rgrd(p)
        #
        #---------------------------------------------------------------------------------------
        x = self.x 
        nx = len(x)
        xx = self.xp 
        mx = len(xx)

        y = self.y 
        ny = len(y)
        yy = self.yp 
        my = len(yy)

        intpol = numpy.zeros((2,))

        if self.xtype == 'linear' or self.xtype == 'linearLog':
            intpol[0] = 1
            lwx = mx
        elif self.xtype == 'cubic' or self.xtype == 'cubicLog':
            intpol[0] = 3
            lwx = 4*mx
        else:
            print 'In 2D rgrd first dimension interpolation type not set to linear, linearLog, cubic or cubicLog'
            raise ValueError
            return

        if self.ytype == 'linear' or self.ytype == 'linearLog':
            intpol[1] = 1
            lwy = 2*mx + my
        elif self.ytype == 'cubic' or self.ytype == 'cubicLog':
            intpol[1] = 3
            lwy = 4*(mx + my)
        else:
            print 'In 2D rgrd second dimension interpolation type not set to linear, linearLog, cubic or cubicLog'
            raise ValueError
            return

        lw = lwx + lwy
        liw = mx + my

        w  = numpy.zeros((lw,),'f')
        iw = numpy.zeros((liw,),'i')
        q, ier = regridpack.rgrd2(x, y, numpy.transpose(p), xx, yy, intpol, w, iw)
        q = numpy.transpose(q)
        if ier != 0:
            print 'In return from call to rgrd2 ier = ', ier
            raise ValueError

        return q 


# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# +++++++++++++++++++++++++++++++ Uniform 2D Interpolation ++++++++++++++++++++++++++++++++++++++++++
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

class Rgrd2u:

    def __init__(self, x, xp, xtype, y, yp, ytype):
        #---------------------------------------------------------------------------------------
        #    routine:  __init__
        #
        #    purpose:  'init' assigns values to the instance data 
        # 
        #    usage:     r = adamsregrid.Rgrd(x,xp,xtype,y,yp,ytype)              2D interpolation
        #
        #    passed:    x = input grid description - the x vector or number of x points for uniform case
        #               xp = output grid description - the x vector or number of x points for uniform case
        #               xtype = regrid using linear, linear_log, cubic, or cubic_log interpolation
        #
        #               y = input grid description - the y vector or number of y points for uniform case
        #               yp = output grid description - the y vector or number of y points for uniform case
        #               ytype = regrid using linear, linear_log, cubic, or cubic_log interpolation
        #
        #    returned:  x -- an instance of the Rgrd2 class to qualify using dot notation to make a computation
        #
        #    definition: __init__(self, x, xp, xtype, y, yp, ytype):
        #
        #---------------------------------------------------------------------------------------
        if debug == 1:
            print 'making instance of Rgrd2u'

        self.x = x
        self.xp = xp
        self.xtype = xtype
        self.y = y
        self.yp = yp
        self.ytype = ytype

    def rgrd(self, p):
        #---------------------------------------------------------------------------------------
        #                                      
        #     purpose: linear or cubic interpolation in 2D between given general grids
        #
        #     usage:   q = r.rgrd(p)
        #
        #---------------------------------------------------------------------------------------
        nx = self.x 
        mx = self.xp 
        ny = self.y 
        my = self.yp 

        intpol = numpy.zeros((2,))

        if self.xtype == 'linear' or self.xtype == 'linearLog':
            intpol[0] = 1
            if (nx-1)%(mx-1) == 0:
                lwx = 1
            else:
                lwx = mx
        elif self.xtype == 'cubic' or self.xtype == 'cubicLog':
            intpol[0] = 3
            lwx = 4*mx
        else:
            print 'In 2D rgrd first dimension interpolation type not set to linear, linearLog, cubic or cubicLog'
            raise ValueError
            return

        if self.ytype == 'linear' or self.ytype == 'linearLog':
            intpol[1] = 1
            if (ny-1)%(my-1) == 0:
                lwy = 0
            else:
                lwy = 2*mx + my
        elif self.ytype == 'cubic' or self.ytype == 'cubicLog':
            intpol[1] = 3
            lwy = 4*(mx + my)
        else:
            print 'In 2D rgrd second dimension interpolation type not set to linear, linearLog, cubic or cubicLog'
            raise ValueError
            return

        lw = lwx + lwy
        liw = mx + my

        w  = numpy.zeros((lw,),'f')
        iw = numpy.zeros((liw,),'i')
        q, ier = regridpack.rgrd2u(numpy.transpose(p), mx, my, intpol, w, iw)
        q = numpy.transpose(q)
        if ier != 0:
            print 'In return from call to rgrd2u ier = ', ier
            raise ValueError

        return q 



# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# +++++++++++++++++++++++++++++++ General 3D Interpolation ++++++++++++++++++++++++++++++++++++++++++
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

class Rgrd3:

    def __init__(self, x, xp, xtype, y, yp, ytype, z, zp, ztype):
        #---------------------------------------------------------------------------------------
        #    routine:  __init__
        #
        #    purpose:  'init' assigns values to the instance data 
        # 
        #    usage:     r = adamsregrid.Rgrd(x,xp,xtype,y,yp,ytype,z,zp,ztype)                  3D interpolation
        #
        #    passed:    x = input grid description - the x vector or number of x points for uniform case
        #               xp = output grid description - the x vector or number of x points for uniform case
        #               xtype = regrid using linear, linear_log, cubic, or cubic_log interpolation
        #
        #               y = input grid description - the y vector or number of y points for uniform case
        #               yp = output grid description - the y vector or number of y points for uniform case
        #               ytype = regrid using linear, linear_log, cubic, or cubic_log interpolation
        #
        #               z = input grid description - the z vector or number of z points for uniform case
        #               zp = output grid description - the z vector or number of z points for uniform case
        #               ztype = regrid using linear, linear_log, cubic, or cubic_log interpolation
        #
        #    returned:  x -- an instance of the Rgrd2 class to qualify using dot notation to make a computation
        #
        #    definition: __init__(self, x, xp, xtype, y, yp, ytype, z, zp, ztype):
        #
        #---------------------------------------------------------------------------------------
        if debug == 1:
            print 'making instance of Rgrd3'

        self.x = x
        self.xp = xp
        self.xtype = xtype
        self.y = y
        self.yp = yp
        self.ytype = ytype
        self.z = z
        self.zp = zp
        self.ztype = ztype

    def rgrd(self, p):
        #---------------------------------------------------------------------------------------
        #                                      
        #     purpose: linear or cubic interpolation in 3D between given general grids
        #
        #     usage:   q = r.rgrd(p)
        #
        #---------------------------------------------------------------------------------------
        x = self.x 
        nx = len(x)
        xx = self.xp 
        mx = len(xx)

        y = self.y 
        ny = len(y)
        yy = self.yp 
        my = len(yy)

        z = self.z 
        nz = len(z)
        zz = self.zp 
        mz = len(zz)

        intpol = numpy.zeros((3,))

        if self.xtype == 'linear' or self.xtype == 'linearLog':
            intpol[0] = 1
            lwx = mx
        elif self.xtype == 'cubic' or self.xtype == 'cubicLog':
            intpol[0] = 3
            lwx = 4*mx
        else:
            print 'In 3D rgrd first dimension interpolation type not set to linear, linearLog, cubic or cubicLog'
            raise ValueError
            return

        if self.ytype == 'linear' or self.ytype == 'linearLog':
            intpol[1] = 1
            lwy = 2*mx + my
        elif self.ytype == 'cubic' or self.ytype == 'cubicLog':
            intpol[1] = 3
            lwy = 4*(mx + my)
        else:
            print 'In 3D rgrd second dimension interpolation type not set to linear, linearLog, cubic or cubicLog'
            raise ValueError
            return

        if self.ztype == 'linear' or self.ztype == 'linearLog':
            intpol[2] = 1
            lwz = 2*mx*my + mz
        elif self.ztype == 'cubic' or self.ztype == 'cubicLog':
            intpol[2] = 3
            lwz = 4*(mx*my + mz)
        else:
            print 'In 3D rgrd third dimension interpolation type not set to linear, linearLog, cubic or cubicLog'
            raise ValueError
            return

        lw = lwx + lwy + lwz
        liw = mx * my + mz

        w  = numpy.zeros((lw,),'f')
        iw = numpy.zeros((liw,),'i')
        q, ier = regridpack.rgrd3(x, y, z, numpy.transpose(p), xx, yy, zz, intpol, w, iw)
        q= numpy.transpose(q)
        if ier != 0:
            print 'In return from call to rgrd3 ier = ', ier
            raise ValueError

        return q 


# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# +++++++++++++++++++++++++++++++ Uniform 3D Interpolation ++++++++++++++++++++++++++++++++++++++++++
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

class Rgrd3u:

    def __init__(self, x, xp, xtype, y, yp, ytype, z, zp, ztype):
        #---------------------------------------------------------------------------------------
        #    routine:  __init__
        #
        #    purpose:  'init' assigns values to the instance data 
        # 
        #    usage:     r = adamsregrid.Rgrd(x,xp,xtype,y,yp,ytype,z,zp,ztype)            3D interpolation
        #
        #    passed:    x = input grid description - the x vector or number of x points for uniform case
        #               xp = output grid description - the x vector or number of x points for uniform case
        #               xtype = regrid using linear, linear_log, cubic, or cubic_log interpolation
        #
        #               y = input grid description - the y vector or number of y points for uniform case
        #               yp = output grid description - the y vector or number of y points for uniform case
        #               ytype = regrid using linear, linear_log, cubic, or cubic_log interpolation
        #
        #               z = input grid description - the z vector or number of z points for uniform case
        #               zp = output grid description - the z vector or number of z points for uniform case
        #               ztype = regrid using linear, linear_log, cubic, or cubic_log interpolation
        #
        #    returned:  x -- an instance of the Rgrd2 class to qualify using dot notation to make a computation
        #
        #    definition: __init__(self, x, xp, xtype, y, yp, ytype, z, zp, ztype):
        #
        #---------------------------------------------------------------------------------------
        if debug == 1:
            print 'making instance of Rgrd3u'

        self.x = x
        self.xp = xp
        self.xtype = xtype
        self.y = y
        self.yp = yp
        self.ytype = ytype
        self.z = z
        self.zp = zp
        self.ztype = ztype

    def rgrd(self, p):
        #---------------------------------------------------------------------------------------
        #                                      
        #     purpose: linear or cubic interpolation in 3D between given general grids
        #
        #     usage:   q = r.rgrd(p)
        #
        #---------------------------------------------------------------------------------------
        nx = self.x 
        mx = self.xp 
        ny = self.y 
        my = self.yp 
        nz = self.z 
        mz = self.zp 

        intpol = numpy.zeros((3,))

        if self.xtype == 'linear' or self.xtype == 'linearLog':
            intpol[0] = 1
            if (nx-1)%(mx-1) == 0:
                lwx = 1
            else:
                lwx = mx
        elif self.xtype == 'cubic' or self.xtype == 'cubicLog':
            intpol[0] = 3
            lwx = 4*mx
        else:
            print 'In 3D rgrd first dimension interpolation type not set to linear, linearLog, cubic or cubicLog'
            raise ValueError
            return

        if self.ytype == 'linear' or self.ytype == 'linearLog':
            intpol[1] = 1
            if (ny-1)%(my-1) == 0:
                lwy = 0
            else:
                lwy = 2*mx + my
        elif self.ytype == 'cubic' or self.ytype == 'cubicLog':
            intpol[1] = 3
            lwy = 4*(mx + my)
        else:
            print 'In 3D rgrd second dimension interpolation type not set to linear, linearLog, cubic or cubicLog'
            raise ValueError
            return

        if self.ztype == 'linear' or self.ztype == 'linearLog':
            intpol[2] = 1
            if (nz-1)%(mz-1) == 0:
                lwz = 0
            else:
                lwz = 2*mx*my + mz
        elif self.ztype == 'cubic' or self.ztype == 'cubicLog':
            intpol[2] = 3
            lwz = 4*(mx*my + mz)
        else:
            print 'In 3D rgrd third dimension interpolation type not set to linear, linearLog, cubic or cubicLog'
            raise ValueError
            return

        lw = lwx + lwy + lwz
        liw = mx * my + mz

        w  = numpy.zeros((lw,),'f')
        iw = numpy.zeros((liw,),'i')
        q, ier = regridpack.rgrd3u(numpy.transpose(p), mx, my, mz, intpol, w, iw)
        q = numpy.transpose(q)
        
        if ier != 0:
            print 'In return from call to rgrd3u ier = ', ier
            raise ValueError

        return q 

# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# +++++++++++++++++++++++++++++++ General 4D Interpolation ++++++++++++++++++++++++++++++++++++++++++
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

class Rgrd4:

    def __init__(self, x, xp, xtype, y, yp, ytype, z, zp, ztype, t, tp, ttype):
        #---------------------------------------------------------------------------------------
        #    routine:  __init__
        #
        #    purpose:  'init' assigns values to the instance data 
        # 
        #    usage:     r = adamsregrid.Rgrd(x,xp,xtype,y,yp,ytype,z,zp,ztype,t,tp,ttype)     4D interpolation
        #
        #    passed:    x = input grid description - the x vector or number of x points for uniform case
        #               xp = output grid description - the x vector or number of x points for uniform case
        #               xtype = regrid using linear, linear_log, cubic, or cubic_log interpolation
        #
        #               y = input grid description - the y vector or number of y points for uniform case
        #               yp = output grid description - the y vector or number of y points for uniform case
        #               ytype = regrid using linear, linear_log, cubic, or cubic_log interpolation
        #
        #               z = input grid description - the z vector or number of z points for uniform case
        #               zp = output grid description - the z vector or number of z points for uniform case
        #               ztype = regrid using linear, linear_log, cubic, or cubic_log interpolation
        #
        #               t = input grid description - the t vector or number of t points for uniform case
        #               tp = output grid description - the t vector or number of t points for uniform case
        #               ttype = regrid using linear, linear_log, cubic, or cubic_log interpolation
        #
        #    returned:  x -- an instance of the Rgrd2 class to qualify using dot notation to make a computation
        #
        #    definition: __init__(self, x, xp, xtype, y, yp, ytype, z, zp, ztype, t, tp, ttype):
        #
        #---------------------------------------------------------------------------------------
        if debug == 1:
            print 'making instance of Rgrd4'

        self.x = x
        self.xp = xp
        self.xtype = xtype
        self.y = y
        self.yp = yp
        self.ytype = ytype
        self.z = z
        self.zp = zp
        self.ztype = ztype
        self.t = t
        self.tp = tp
        self.ttype = ttype

    def rgrd(self, p):
        #---------------------------------------------------------------------------------------
        #                                      
        #     purpose: linear or cubic interpolation in 3D between given general grids
        #
        #     usage:   q = r.rgrd(p)
        #
        #---------------------------------------------------------------------------------------
        x = self.x 
        nx = len(x)
        xx = self.xp 
        mx = len(xx)

        y = self.y 
        ny = len(y)
        yy = self.yp 
        my = len(yy)

        z = self.z 
        nz = len(z)
        zz = self.zp 
        mz = len(zz)

        t = self.t 
        nt = len(t)
        tt = self.tp 
        mt = len(tt)

        intpol = numpy.zeros((4,))

        if self.xtype == 'linear' or self.xtype == 'linearLog':
            intpol[0] = 1
            lwx = mx
        elif self.xtype == 'cubic' or self.xtype == 'cubicLog':
            intpol[0] = 3
            lwx = 4*mx
        else:
            print 'In 4D rgrd first dimension interpolation type not set to linear, linearLog, cubic or cubicLog'
            raise ValueError
            return

        if self.ytype == 'linear' or self.ytype == 'linearLog':
            intpol[1] = 1
            lwy =  2*mx + my
        elif self.ytype == 'cubic' or self.ytype == 'cubicLog':
            intpol[1] = 3
            lwy = 4*(mx + my)
        else:
            print 'In 4D rgrd second dimension interpolation type not set to linear, linearLog, cubic or cubicLog'
            raise ValueError
            return

        if self.ztype == 'linear' or self.ztype == 'linearLog':
            intpol[2] = 1
            lwz = 2*mx*my + mz
        elif self.ztype == 'cubic' or self.ztype == 'cubicLog':
            intpol[2] = 3
            lwz = 4*(mx*my + mz)
        else:
            print 'In 4D rgrd third dimension interpolation type not set to linear, linearLog, cubic or cubicLog'
            raise ValueError
            return

        if self.ttype == 'linear' or self.ttype == 'linearLog':
            intpol[3] = 1
            lwt = 2*mx*my*mz + mt
        elif self.ttype == 'cubic' or self.ttype == 'cubicLog':
            intpol[3] = 3
            lwt = 4*(mx*my*mz + mt)
        else:
            print 'In 4D rgrd fourth dimension interpolation type not set to linear, linearLog, cubic or cubicLog'
            raise ValueError
            return

        lw = lwx + lwy + lwz + lwt
        liw = mx * my + mz + mt

        w  = numpy.zeros((lw,),'f')
        iw = numpy.zeros((liw,),'i')
        q, ier = regridpack.rgrd4(x, y, z, t, numpy.transpose(p), xx, yy, zz, tt, intpol, w, iw)
        q = numpy.transpose(q)
        
        if ier != 0:
            print 'In return from call to rgrd4 ier = ', ier
            raise ValueError

        return q 


# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# +++++++++++++++++++++++++++++++ Uniform 4D Interpolation ++++++++++++++++++++++++++++++++++++++++++
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++


class Rgrd4u:

    def __init__(self, x, xp, xtype, y, yp, ytype, z, zp, ztype, t, tp, ttype):
        #---------------------------------------------------------------------------------------
        #    routine:  __init__
        #
        #    purpose:  'init' assigns values to the instance data 
        # 
        #    usage:     r = adamsregrid.Rgrd4u(x,xp,xtype,y,yp,ytype,z,zp,ztype,t,tp,ttype)     4D interpolation
        #
        #    passed:    x = input grid description - the x vector or number of x points for uniform case
        #               xp = output grid description - the x vector or number of x points for uniform case
        #               xtype = regrid using linear, linear_log, cubic, or cubic_log interpolation
        #               xdim = location of x dimension in the data
        #
        #               y = input grid description - the y vector or number of y points for uniform case
        #               yp = output grid description - the y vector or number of y points for uniform case
        #               ytype = regrid using linear, linear_log, cubic, or cubic_log interpolation
        #               ydim = location of y dimension in the data
        #
        #               z = input grid description - the z vector or number of z points for uniform case
        #               zp = output grid description - the z vector or number of z points for uniform case
        #               ztype = regrid using linear, linear_log, cubic, or cubic_log interpolation
        #               zdim = location of z dimension in the data
        #
        #               t = input grid description - the t vector or number of t points for uniform case
        #               tp = output grid description - the t vector or number of t points for uniform case
        #               ttype = regrid using linear, linear_log, cubic, or cubic_log interpolation
        #               tdim = location of t dimension in the data
        #
        #    returned:  x -- an instance of the Rgrd2 class to qualify using dot notation to make a computation
        #
        #    definition: __init__(self, x, xp, xtype, y, yp, ytype, z, zp, ztype, t, tp, ttype):
        #
        #---------------------------------------------------------------------------------------
        if debug == 1:
            print 'making instance of Rgrd4u'

        self.x = x
        self.xp = xp
        self.xtype = xtype
        self.y = y
        self.yp = yp
        self.ytype = ytype
        self.z = z
        self.zp = zp
        self.ztype = ztype
        self.t = t
        self.tp = tp
        self.ttype = ttype


    def rgrd(self, p):
        #---------------------------------------------------------------------------------------
        #                                      
        #     purpose: linear or cubic interpolation in 3D between given general grids
        #
        #     usage:   q = r.rgrd(p)
        #
        #---------------------------------------------------------------------------------------

        nx = self.x 
        mx = self.xp 
        ny = self.y 
        my = self.yp 
        nz = self.z 
        mz = self.zp 
        nt = self.t 
        mt = self.tp 

        intpol = numpy.zeros((4,))

        if self.xtype == 'linear' or self.xtype == 'linearLog':
            intpol[0] = 1
            if (nx-1)%(mx-1) == 0:
                lwx = 1
            else:
                lwx = mx
        elif self.xtype == 'cubic' or self.xtype == 'cubicLog':
            intpol[0] = 3
            lwx = 4*mx
        else:
            print 'In 4D rgrd first dimension interpolation type not set to linear, linearLog, cubic or cubicLog'
            raise ValueError
            return

        if self.ytype == 'linear' or self.ytype == 'linearLog':
            intpol[1] = 1
            if (ny-1)%(my-1) == 0:
                lwy = 0
            else:
                lwy = 2*mx + my
        elif self.ytype == 'cubic' or self.ytype == 'cubicLog':
            intpol[1] = 3
            lwy = 4*(mx + my)
        else:
            print 'In 4D rgrd second dimension interpolation type not set to linear, linearLog, cubic or cubicLog'
            raise ValueError
            return

        if self.ztype == 'linear' or self.ztype == 'linearLog':
            intpol[2] = 1
            if (nz-1)%(mz-1) == 0:
                lwz = 0
            else:
                lwz = 2*mx*my + mz
        elif self.ztype == 'cubic' or self.ztype == 'cubicLog':
            intpol[2] = 3
            lwz = 4*(mx*my + mz)
        else:
            print 'In 4D rgrd third dimension interpolation type not set to linear, linearLog, cubic or cubicLog'
            raise ValueError
            return

        if self.ttype == 'linear' or self.ttype == 'linearLog':
            intpol[3] = 1
            if (nt-1)%(mt-1) == 0:
                lwt = 0
            else:
                lwt = 2*mx*my*mz + mt
        elif self.ttype == 'cubic' or self.ttype == 'cubicLog':
            intpol[3] = 3
            lwt = 4*(mx*my*mz + mt)
        else:
            print 'In 4D rgrd fourth dimension interpolation type not set to linear, linearLog, cubic or cubicLog'
            raise ValueError
            return

        lw = lwx + lwy + lwz + lwt
        liw = mx * my + mz + mt

        w  = numpy.zeros((lw,),'f')
        iw = numpy.zeros((liw,),'i')
        q, ier = regridpack.rgrd4u(numpy.transpose(p), mx, my, mz, mt, intpol, w, iw)
        q = numpy.transpose(q)

        if ier != 0:
            print 'In return from call to rgrd4u ier = ', ier
            raise ValueError

        return q 

# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# ++++++++++++++++++++++++++++++++ Utility Functions +++++++++++++++++++++++++++++++++
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

def checkdimension(x, xp, xtype):
    #---------------------------------------------------------------------------------------
    #
    #    purpose:  dimension  checks
    #                  1. x and xp are vectors - not numbers
    #                  2. valid interpolation type request
    #                  3. adequate vector lengths for interpolation type request
    #                  4. data type is  float32 
    #                  5. no request for extrapolation 
    #                  6. monotonically increasing vectors 
    # 
    #    passed :  x - source vector
    #              xp - target vector
    #              xtype - interpolation type
    #             
    #    returned: reverse, xpreverse, x, xp -- dimension vectors reverse and the need in 
    #                                           corresponding data and the vectors
    #
    #---------------------------------------------------------------------------------------
    # -----  make sure that x and xp are vectors  -----  
    try:
        xsize = len(x)
    except:
        msg = 'CANNOT CREATE INSTANCE - Expected an array for the dimension'
        raise TypeError, msg

    try:
        xpsize = len(xp)
    except:
        msg = 'CANNOT CREATE INSTANCE - Expected an array for the dimension'
        raise TypeError, msg

    # -----  check request for valid interpoloation type  -----  

    if xtype != 'linear' and xtype != 'linearLog' and xtype != 'cubic' and xtype != 'cubicLog':
        msg = 'CANNOT CREATE INSTANCE - interpolation choice must be linear, linearLog, cubic or cubicLog'
        raise ValueError, msg

    # -----  check for inadequate coordinate vector sizes  -----  

    if xtype == 'cubic' or xtype == 'cubicLog':
        minLength = 4
        if xsize < minLength:
            msg =  'ERROR - input grid vector needs size of least 4, but has only %d' % (xsize,)
            raise ValueError, msg
        if xpsize < minLength:
            msg =  'ERROR - input grid vector needs size of least 4, but has only %d' % (xpsize,)
            raise ValueError, msg
    else:
        minLength = 2
        if xsize < minLength:
            msg =  'ERROR - input grid vector needs size of least 2, but has only %d' % (xsize,)
            raise ValueError, msg
        if xpsize < minLength:
            msg =  'ERROR - input grid vector needs size of least 2, but has only %d' % (xpsize,)
            raise ValueError, msg

    # -----  take log of vectors if needed  -----  

    if xtype == 'linearLog' or xtype == 'cubicLog':

        n = numpy.add.reduce(numpy.where(numpy.less(numpy.ravel(x), 0.0),1,0))  # look for negative values
        if n > 0:
            msg =  'ERROR IN TAKING LOG OF INPUT COORDINATE - negative values'
            raise ValueError, msg
        n = numpy.add.reduce(numpy.where(numpy.less(numpy.ravel(xp), 0.0),1,0))  # look for negative values
        if n > 0:
            msg =  'ERROR IN TAKING LOG OF OUTPUT COORDINATE - negative values'
            raise ValueError, msg

        n = numpy.add.reduce(numpy.where(numpy.equal(numpy.ravel(x), 0.0),1,0))  
        if n > 0:                                                                        # found zero in x
            print 'IN TAKING LOG OF INPUT COORDINATE - 0.0 replaced by 1.0e-6'
            x = numpy.where(numpy.equal(x, 0.0), 1.0e-6, x)  
            x = numpy.log(x)
        n = numpy.add.reduce(numpy.where(numpy.equal(numpy.ravel(xp), 0.0),1,0))  
        if n > 0:                                                                        # found zero in xp
            print  'IN TAKING LOG OF INPUT COORDINATE - 0.0 replaced by 1.0e-6'
            xp = numpy.where(numpy.equal(xp, 0.0), 1.0e-6, xp)  
            xp = numpy.log(xp)

    # ----- Check dimension type and change to float if necessary -------

    if x.dtype.char != 'f':
        print 'WARNING - source dimension vector will be converted to float32'
        x = x.astype(numpy.float32)
    if xp.dtype.char != 'f':
        print 'WARNING - target dimension vector will be converted to float32'
        xp = xp.astype(numpy.float32)

    if x[0] > x[xsize - 1]:
        x = x[::-1]
        xreverse = 'yes'
    else:
        xreverse = 'no'

    if xp[0] > xp[xpsize - 1]:
        xp = xp[::-1]
        xpreverse = 'yes'
    else:
        xpreverse = 'no'

    # -----  check for consistency  -----  

    small = 1.0e-04
    if (x[0] - xp[0] > small) or ( xp[xpsize - 1] - x[xsize - 1] > small):
        msg = 'CANNOT CREATE INSTANCE - extrapolation is not posible'
        raise ValueError,msg
        return

    for n in range(1,xsize):
        if x[n] < x[n - 1]:
            msg = 'CANNOT CREATE INSTANCE - source vector not monotonic'
            raise ValueError,msg
            return

    for n in range(1,xpsize):
        if xp[n] < xp[n - 1]:
            msg = 'CANNOT CREATE INSTANCE - target vector not monotonic'
            raise ValueError,msg
            return

    return xreverse, xpreverse, x, xp

def getnewOrder(intpolDimlist, sizeData):
    #---------------------------------------------------------------------------------------
    #
    #    purpose: determine the new order and its inverse if transpose is needed 
    # 
    #    passed :  intpolDimlist - list of dimension numbers requiring interplotaion (x on for right)
    #              sizeData - number of dimensions in the data
    #             
    #    returned: newOrder, reverseOrder, needsTranspose
    #
    #---------------------------------------------------------------------------------------
    # -----  Determine the new order  -----  

    orderList = []                   # list to hold the dimension order

    for n in range(sizeData):        # insert dimension numbers not requiring a change
        if n not in intpolDimlist:
            orderList.append(n)
    for n in intpolDimlist:          # complete list by adding the dimension numbers needing interpolation
        orderList.append(n)

    naturnalList = []
    for n in range(sizeData):
        naturnalList.append(n)       # for example, naturnalList = [0, 1, 2, 3] in 4D

    if orderList == naturnalList:
        needsTranspose = 'no'
    else:
        needsTranspose = 'yes'

    newOrder = tuple(orderList)

    # -----  Determine the inverse to this new order  -----  

    xform = []
    for i in range(len(newOrder)):
        xform.append( [newOrder[i], i] )
    xform.sort()

    inverse_shapelist = []
    for item in xform:
        inverse_shapelist.append(item[1])
    inverseOrder = tuple(inverse_shapelist)

    return newOrder, inverseOrder, needsTranspose

def uniformGrid(first, last, number):
    #---------------------------------------------------------------------------------------
    #                                      
    #     routine: uniformGrid 
    #                                      
    #     purpose: generate a evenly spaced  grid vector
    #
    #     usage:   grid = adamsregrid.uniformGrid(first, last, number)
    #
    #     passed:  first  -- first vector element
    #              last   -- last vector element
    #              number -- number of points
    #
    #     return:  grid -  the single precision grid vector
    #
    #     definition: uniformGrid(first, last, number):
    #
    #---------------------------------------------------------------------------------------

    delta = (last - first)/(number - 1)
    gridlist = []

    for i in range(number):
            gridlist.append(first + i*delta)

    grid =  numpy.array(gridlist, numpy.float32)

    return grid

def gaussianGrid(nlat):
    #---------------------------------------------------------------------------------------
    #                                      
    #     routine: gaussianGrid
    #                                      
    #     purpose: compute the single  precision gaussian grid points using the spherepack
    #              function gaqd 
    #
    #     usage:   grid = gaussianGrid(nlat) 
    #
    #     passed:  nlat -- number of points
    #
    #     return:  grid -  the single precision grid vector
    #
    #     definition: gaussianGrid(nlat):
    #
    #
    #---------------------------------------------------------------------------------------

    rad2deg = 180.0/math.pi

    # get the gaussian points and weights from spherepack

    ldwork = nlat*(nlat + 2)
    points, wts, ierror = regridpack.gaqd(nlat, ldwork)

    if ierror != 0:
        print 'In return from call to gaqd ierror = ', ierror
        raise ValueError

    # convert points to geophysical format

    colatlist = list(points) 
    latlist = map( (lambda x: 90.0 - x*180.0/math.pi), colatlist)

    grid =  numpy.array(latlist, numpy.float32)

    return grid

def help(choice = None):

    import adamsregrid

    if choice is None:
        print """    --------------------------------------------------------------------------------------------------------
      
      INSTRUCTIONS 
     
        To get an overview of the module, type
     
                    adamsregrid.help('overview')
     
        To obtain a prescription for making an instance,  type
     
                    adamsregrid.help('Regrid')
     
        To acquire instructions on the use of the rgrd function, type
     
                    adamsregrid.help('rgrd')
     
        To look at a general one dimensional example, type
     
                    adamsregrid.help('OneDexample')
     
        To look at a general four dimensional example, type
     
                    adamsregrid.help('FourDexample')
     
        To write the above information to the file regridmodule.doc, import adamstest and  type
     
                    adamstest.document()
     
    --------------------------------------------------------------------------------------------------------""" 
    elif choice is 'overview':
        print adamsregrid.__doc__
    elif choice == 'Regrid':
        print adamsregrid.Regrid.__init__.__doc__
    elif choice == 'rgrd':
        print adamsregrid.Regrid.rgrd.__doc__
    elif choice == 'OneDexample':
        print """     ------------------------------------------------------------------------------------------------------
     
      EXAMPLE OF ONE DIMENSIONAL INTERPOLATION  
         
        Step A.  At the python prompt type
         
                   import adamsregrid
         
        Step B. Choose the dimension requiring interpolation.
     
                Get a copy of the input coordinate vector or its size to select the uniform option.
     
                Get a copy of the output coordinate vector or its size to select the uniform option.
     
                Select the interpolation scheme as a string from the choices: 'linear', 'linearLog', 'cubic', or
                'cubicLog'.
     
                Associate a number with the position of the dimension in the input data. As a concrete example, if
                the shape of the data is (12,2,46,72), then  0 regrids the dimension having size 12 or alernately 
                3 regrids the dimension having the size 72. Clearly, this value  and the identification of the
                 input grid vectors must be consistent.
     
        Step C. As the first step in the two step process, use the information assembled in Step C to make the
                instance, r, by typing
     
                   r = adamsregrid.Regrid( coordinateVectorIn, coordinateVectorOut,
                                      interpolationChoice, dataDimensionPosition )
     
                where
     
                coordinateVectorIn - the coordinate vector associated with the input data requiring interpolation
                                     or number of points for uniform case.
     
                coordinateVectorOut - the coordinate vector associated with the output data after interpolation or
                                      number of points for uniform case.
     
                interpolationChoice -  the interpolation scheme as linear ('linear'), linear in the log
                                       ('linearLog') , cubic ('cubic') or cubic in the log ('cubicLog')
                                       interpolation.
     
                dataDimensionPosition - an integer in the range 0 to 3 identifying the location of the 
                                        dimension in the input data array in Python's order.
         
        Step D. As the second step in the two step process, perform the computation by typing 
     
                outputData = r.rgrd( inputData) 
     
                This process can continue with another data as
     
                anotherOutputData = r.rgrd( anotherInputData) 
     
     -----------------------------------------------------------------------------------------------------------"""

    elif choice == 'FourDexample':
        print """     ------------------------------------------------------------------------------------------------------
     
         
      EXAMPLE OF FOUR DIMENSIONAL INTERPOLATION  
         
        Step A.  At the python prompt type
         
                     import adamsregrid
         
        Step B. Get a copy of the input coordinate vectors or thier sizes if the uniform option is requested.
     
                Get a copy of the output coordinate vectors or thier sizes if the uniform option is requested.
     
                For each of the dimensions, select the interpolation scheme as a string from the choices: 'linear',
                'linearLog', 'cubic', or 'cubicLog'.
     
                Associate a number with the position of each of the dimension in the input data. As a concrete 
                example, if the shape of the data is (12,2,46,72), then 1 for the first dimension, 3 for the first
                dimension, 2 for the third dimension and 0 for the fourth dimension regrids the dimension having
                sizes 2, 72, 46 and 12 using the corresponding other information. Clearly, this value and the 
                identification of the input grid vectors must be consistent.
         
     
        Step C. As the first step in the two step process, make the  instance, r, by typing
     
                r=adamsregrid.Regrid( xcoordinateVectorIn, xcoordinateVectorOut,
                                 xinterpolationChoice, xdataDimensionPosition,
     
                                 ycoordinateVectorIn, ycoordinateVectorOut,
                                 yinterpolationChoice, ydataDimensionPosition,
     
                                 zcoordinateVectorIn, zcoordinateVectorOut,
                                 zinterpolationChoice, zdataDimensionPosition,
     
                                 tcoordinateVectorIn, tcoordinateVectorOut,
                                 tinterpolationChoice, tdataDimensionPosition )
     
                where the prefixes x, y, z and t distinguish the four choices. The association of x, y, z and with
                phsical dimensions is at the discretion of the user. For each dimension, neglecting the prefixes x,
                y, z and t, the meaning is as follows:
     
                coordinateVectorIn - the coordinate vector associated with the input data requiring interpolation
                                     or number of points for uniform case.
     
                coordinateVectorOut - the coordinate vector associated with the output data after interpolation or
                                      number of points for uniform case.
     
                interpolationChoice -  the interpolation scheme as linear ('linear'), linear in the log
                                       ('linearLog') , cubic ('cubic') or cubic in the log ('cubicLog')
                                       interpolation.
     
                dataDimensionPosition - an integer in the range 0 to 3 identifying the location of the 
                                        dimension in the input data array in Python's order.
     
         
        Step D. As the second step in the two step process, perform the computation by typing 
     
                outputData = r.rgrd( inputData) 
     
                This process can continue with another data as
     
                anotherOutputData = r.rgrd( anotherInputData) 
     
     -------------------------------------------------------------------------------------------------"""

    else:
        print 'Could not find this choice in help'

    return None

