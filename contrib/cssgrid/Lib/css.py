# Adapted for numpy/ma/cdms2 by convertcdms.py
"""---------------------------------------------------------------------------------------------
  
   INTRODUCTION TO NGMATH
  
        The ngmath library is a collection of interpolators and approximators for one-dimensional, two-dimensional
        and three-dimensional data. The packages, which were obtained from NCAR, are:
 
            natgrid -- a two-dimensional random data interpolation package based on Dave Watson's nngridr.
 
            dsgrid --  a three-dimensional random data interpolator based on a simple inverse distance weighting
                       algorithm.
 
            fitgrid -- an interpolation package for one-dimensional and two-dimensional gridded data based on 
                       Alan Cline's Fitpack. Fitpack uses splines under tension to interpolate in one and two
                       dimensions.
 
            csagrid -- an approximation package for one-dimensional, two-dimensional and three-dimensional random 
                       data based on David Fulker's Splpack. csagrid uses cubic splines to calculate its
                       approximation function.
 
            cssgrid -- an interpolation package for random data on the surface of a sphere based on the work of
                       Robert Renka. cssgrid uses cubic splines to calculate its interpolation function.
 
            shgrid --  an interpolation package for random data in 3-space based on the work of Robert Renka. 
                       shgrid uses a modified Shepard's algorithm to calculate its interpolation function.
  
   COMPARISION OF NGMATH PACKAGES
  
        Three-dimensional packages -- shgrid, csagrid and dsgrid.
 
            shgrid is probably the package of choice for interpolation. It uses a least squares fit of biquadratics
            to construct its interpolation function. The interpolation function will pass through the original data 
            points. 
 
            csagrid uses a least squares fit of cubic splines to calculate its approximation function: the calculated
            surface will not necesarily pass through the original data points. The algorithm can become unstable in data
            sparse regions.
 
            dsgrid uses a weighted average algorithm and is stable in all cases, but the resultant interpolation is
            not usually smooth and execution time is very slow. dsgrid is probably best used when csagrid and shgrid
            fail or for comparative purposes.
  
        Two-dimensional packages -- natgrid, fitgrid, csagrid and dsgrid.
 
            natgrid is the package of choice in most cases. It implements a very stable algorithm and has parameters
            for adjusting the smoothness of the output surface. 
 
            fitgrid offers user-settable parameters for specifiying derivatives along the boundary of the output grid
            which are not available in natgrid.
 
            csagrid produces an approximate two-dimensional surface which may be smoother than that produced by fitgrid
            and natgrid.
 
            dsgrid is not recommended for two-dimensional surfaces. natgrid is superior in all respects.
  
        One-dimensional packages -- fitgrid  and csagrid.
 
            fitgrid is definitely the package of choice. It has many features not available in csagrid, such as
            interpolating parametric curves, finding integrals, handling periodic functions, allowing smoothing that
            varies from linear to a full cubic spline interpolation and specifying slopes at the end points.
  
        Interpolation on a sphere -- cssgrid.
 
            cssgrid is designed specifically for interpolating on a sphere. It uses cubic splines to calculate an
            interpolation function.
  
  
   CSSGRID PACKAGE
  
        cssgrid is the only algorithm in the ngmath library specifically designed for interpolation on a sphere.
        cssgrid implements a tension spline interpolation algorithm to fit a function to input data. Tension splines
        are an extension of cubic splines whereby a user-specified tension factor controls the fitting function
        from essentially linear interpolation to pure cubic spline interpolation. The input data is specified on a 
        randomly spaced set of latitude-longitude coordinates. 
 
        There are entries in the cssgrid package for: interpolating random data on a sphere to a user-specfied grid,
        converting between latitude-longitude coordinates and the equivalent Cartesian coordinates on a unit sphere,
        for calculating Delaunay triangulations, for calculating Voronoi polygons, and for setting and retrieving
        user-specified control parameters. Values of the default control parameters should suffice in some cases. 
 
   CSSGRID CONTENTS
  
        Access through Python to the cssgrid package from NCAR's ngmath distribution is provided directly through the module
        cssgridmodule.so which was generated by connecting the Fortran routines to Python using the Pyfort implementation
        by Paul Dubois. 
 
   REQUIRED FILE
 
        cssgridmodule.so -- the Python interface to the ngmath cssgrid package.
 
   USEFUL FILES
 
        css.py -- the object oriented interface including a general help package.
        cssgridtest.py -- the code to test css.py and to write documentation.

   USAGE
 
        This module is designed to use in two ways. One is through the use of the object oriented interface to the underlying
        functions. This approach is recommended for users not already familiar with the original cssgrid distribtution because
        it simplifies the calls to the routines. The other method uses the original functions calling them directly from Python. 
 
        -------------------  OBJECT ORIENTED APPROACH ----------------
 
            The css module contains the Cssgrid class and its single method, rgrd, which provides access to all the cssgrid 
            functions (except css2c and csc2s which convert from spherical to cartesian coordinates available as utilities
            directly from the cssgridmodule as noted below). The object oriented approach has been organized as a two step
            process.
 
            STEP 1. 
 
            To make an instance, r, type:
 
                import css
 
                r = css.Cssgrid(lati, loni, lato, lono) 

                or

                r = css.Cssgrid(lati, loni)
 
                    where lati, loni and lato, lono are the input and output grid coordinate arrays in degrees.

                The input grid must be organized in a list format. The size of the lati array and the loni array are 
                necessarily equal. For example, if there are n input data points randomly spaced on the sphere, there
                are n latitudes and n longitudes. The output grid coordinate arrays describe a rectangular grid on the
                sphere.

                The choice between the two examples is made according to requirements in subseqent calls to the method 
                function. The first choice is necessary for the interpolation from the input to the output grid. The latter
                choice is sufficient if the subsequent request is for Delauanay triangles or Voronoi polygons which require
                only an input grid.
 
                The grid coordinate arrays can be single precision (numpy.float32) or double precision (numpy.float64). The
                decision on whether to call for a single or a double precision computation subsequently is made by looking at
                the type of these arrays. 
 
            To look at the default settings for the control parameters and a brief description of thier properties, type
 
                r.printDefaultParameterTable()
 
            To change a setting type the new value. For example, to set sig to 0.5, type
 
                r.sig = 0.5
 
            To find a value without printing the table, type the name. For example, to exam the value of nsg, type
 
                r.nsg
 
            To check the settings type
 
                r.printInstanceParameterTable()   -- prints in tabular form the parameters used in subsequent calls to the method
                                                     function rgrd.
                or

                printStoredParameters()           -- prints the parameters in memory which may differ from the above if the user
                                                     has made more than one instance of the Cssgrid class.
 
            STEP 2. 

            cssgrid is restricted to two dimensions on the sphere. Consequently, it is the user's responsibility to reduce the
            processing of higher dimensional data to a sequence of calls using only two dimensional data.
 
            There are three basic computations provided by the single method function: regridding data on a sphere using Delaunay 
            triangulations and Voronoi polygons, calculating Delaunay triangulations only and calculating Voronoi polygons only.
            Consequently, the user has the following three choices:
 
            1. Compute the interpolation
 
                To interpolate the random data, dataIn, associated with the list arrays (loni, lati) to the rectilinear grid
                (lono, lato), type
 
                    dataOut = r.rgrd(dataIn)
 
                        where dataOut is the interpolated data on the (lono, lato) grid.
 
            2. Compute the Delaunay triangulations
 
                To find the triangulation associated with the grid (loni, lati), type
 
                    ntri = r.rgrd(compType = 'triangles' ) where 
 
                        ntri -- a 2D integer array containing the nodes of the triangles in the triangulation. The nodes
                        for the jth triangle are in the triple composed of ntri(j,1), ntri(j,2) and ntri(j,3). These indices 
                        reference the sequence in the input coordinate grid. For example, if the triple (5,1,2) were in the 
                        list of triples, it would describe the triangle having vertices at (loni(5), lati(5)), (loni(1), lati(1))
                        and(loni(2),lati(2)). 
 
            3. Compute the Voronoi polygons
 
                To find the polygons associated with the grid (loni, lati), type
 
                       latV, lonV, rc, nv = r.rgrd(compType = 'polygons', indexVoro = ni, firstCall = nf)
 
 
                   where the meaning of the input arguments are as follows:
 
                       ni -- the index of the input coordinate for which you want to determine the Voronoi polygon. The lowest
                             value is 0.  
 
                       nf -- a flag indicating if this is the first call to retrieve Voronoi polygons for this dataset(1=yes,0=no).
                             Calls subsequent to the first call for a given dataset are much faster than the first call.
 
                   where the meaning of the output tuple is as follows:
 
                       latV -- an array of latitude values in degrees for the Voronoi indices. ( These are circumcenters of
                               circles passing through the Delaunay triangles. If a coordinate is a boundary point, then the
                               circle may pass through certain "pseudo points" that have been added to the original dataset in 
                               order to complete the Voronoi polygon.) 
 
                       lonV -- an array of longitude values in degrees for the Voronoi indices. 
 
                       rc -- Array containing the arc length (in degrees) of the angle between a circumcenter and its associated
                             triangle vertices.
 
                       nv -- an array containing  indices for the Voronoi polygon enclosing the coordinate (loni(ni), lati(ni)).
                             The indices returned in this array refer to the coordinates returned in latV and lonV.  For example,
                             if the integer "j" is an element of the nv array, then (lonV(j),latV(j)) is a vertex of the Voronoi
                             polygon enclosing (loni(ni), lati(ni)). The indices in nv list out the vertices of the Voronoi polygon
                             in counter-clockwise order.
 
 
            In all three cases, a call to rgrd automatically sets the values of the control parameters for use in the computation
            to those set in the instance either as the defaults or by explicit choice by the user. 


        -------------------  ORIGINAL FUNCTION APPROACH -----------------
 
            The module cssgridmodule.so exports the following functions to Python from the original Fortran library:
        
                    Single precision procedures:                                                    
                        cssgrid -- interpolation on a sphere
                        cssgtri -- calculates a Delaunay triangulation
                        csvoro  -- calculates Voronoi polygons
                        csseti -- sets integer parameter values 
                        csgeti -- retrieves values for integer parameter values 
                        cssetr -- sets real parameter values 
                        csgetr -- retrieves values for real parameter values 
        
                    Double precision procedures:                                                    
                        cssgridd -- interpolation on a sphere
                        cssgtrid -- calculates a Delaunay triangulation
                        csvorod -- calculates Voronoi polygons
                        cssetd -- sets double precision parameter values 
                        csgetd -- retrieves values for double precision parameter values 
 
                    In addition it contains:
                        css2c -- converts lat/lon to Cartesian coordinates
                        csc2s -- converts Cartesian to lat/lon coordinates
 
            Information on the use of the routines is available by importing cssgridmodule and printing the docstring
            of interest. For example, documentation for the routine cssgrid is obtained by typing
 
                import cssgridmodule
                print cssgridmodule.cssgrid.__doc__
 
            This same information is available in the help package.
 
            A description of the control parameters is not in the cssgridmodule documentation. It can be found by typing
 
               import css
               css.printParameterTable()          -- printing the dedault parameter table
               css.printStoredParameters()        -- printing the values as seen by the Fortran code
 
 
            The documentation associated with the cssgridmodule.so, such as the doctrings, describe the Fortran code. The
            indices start with 1 (not 0 as in the Python interface) and the arrays are transposes as nomenclature. For
            example, the Fortran array dimensioned as (number of latitudes,  number of longitudes) is the same physically 
            as the Python or C array dimensioned as (number of longitudes, number of latitudes). Since the calls are from
            Python the array indices start with 0. It is only in a request for a specific index submitted to Fortran that
            that the indices start at 1 . 

            The long and the short of this is as follows. If you use the interface, indices start at 0. If you call the routines
            directly, in specific requests indices start at 1. In either case, data is passed with the latitude index moving
            faster than the longitude index. 
 
 
   DOCUMENTATION
  
        Documentation is provided through Python's docstrings, essentially Python style program comments. A help package
        provides instructions on the use of cssgrid. A table of contents is printed to the screen by typing
  
                css.help()
  
        after importing css.
  
        A hard copy of this documentation is written to the file cssgridmodule.doc after import cssgridtest by typing 
  
               cssgridtest.document() 
  
   TESTING 
 
       To run a test of the interpolation, the Delaunay triangulations and the computation of the Voronoi polygons, in single
       precision and double precision, and to get a copy of this documentation, type
                       
           cdat cssgridtest.py
 
--------------------------------------------------------------------------------------------------------------"""

import string, sys, numpy, cssgridmodule, math

#cssgridmodule.set_pyfort_option(cssgridmodule.MIRROR) # leftover from pyfort

usefilled = 'yes'
try:
    import numpy.ma
except ImportError:
    print 'Can not convert from numpy.ma array to numpy array without module numpy.ma'
    usefilled = 'no'

debug = 0

class Cssgrid:
    #-------------------------------------------------------------------------------------
    # 
    #       Contents of Cssgrid class
    #                __init__(self, lati, loni, lato = None, lono = None): 
    #                rgrd(self, dataIn = None, compType = None):
    #                
    #-------------------------------------------------------------------------------------
    def __init__(self, lati, loni, lato = None, lono = None): 
        """    --------------------------------------------------------------------------------------------------------
         routine:  __init__ for class Cssgrid
     
         purpose:  init makes an instance of the Cssgrid class while performing the following:
     
                       1. checks the argument list for the correct types.
                       2. selects single or double precision computation.
                       3. assigns the coordinate grid arrays to self data.
                       4. assigns default control parameter values from the parameter dictionary.
     
     
         usage:    r = css.Cssgrid(lati, loni, lato, lono) -- for interpolation on the sphere
     
                   or

                   r = css.Cssgrid(lati, loni) -- for Delauany triangles or Voronoi polygons

                   where lati,loni are the input grid coordinate arrays organized in a list format. The size of the lati
                   array and the loni array are necessarily equal. For example, if here are n input data points randomly
                   spaced on the sphere, there are n latitudes and n longitudes. The output grid coordinate arrays (lono, lato)
                   describe a rectangular grid on the sphere.
     
         definition: __init__(self, lati, loni, lato = None, lono = None) 
    --------------------------------------------------------------------------------------------------------""" 
        # the number of arrays is either two or four

        if (lato is None and lono is not None) or (lato is not None and lono is None): 
            msg = 'CANNOT CREATE INSTANCE - The number of arrays is either two or four' 
            raise ValueError, msg
        
        if (lato is not None):                                    # output grid is part of the initialization 
            # check the argument list
            try: 
                size = len(lati)
                if size < 4:
                    msg = 'CANNOT CREATE INSTANCE - The length of the input lat coordinate grid must be greater than 3'
                    raise ValueError, msg
            except:                        
                msg = 'CANNOT CREATE INSTANCE - Input latitude must be an array'
                raise TypeError, msg
            try: 
                size = len(loni)
                if size < 4:
                    msg = 'CANNOT CREATE INSTANCE - The length of the input lon coordinate grid must be greater than 3'
                    raise ValueError, msg
            except:                        
                msg = 'CANNOT CREATE INSTANCE - Input longitude must be an array'
                raise TypeError, msg
            try:
                size = len(lato)
            except:                        
                msg = 'CANNOT CREATE INSTANCE - Output latitude must be an array. Can have length of 1'
                raise TypeError, msg
            try: 
                size = len(lono)
            except:                        
                msg = 'CANNOT CREATE INSTANCE - Output longitude must be an array. Can have length of 1'
                raise TypeError, msg

            # make sure that the arrays are numpy arrays

            if usefilled == 'yes':
                lati = numpy.ma.filled(lati)
                loni = numpy.ma.filled(loni)
                lato = numpy.ma.filled(lato)
                lono = numpy.ma.filled(lono)

            # select the interpolation routines from the single or the double precision group - majority rules here

            numberSingles = 0
            numberDoubles = 0

            if lati.dtype.char == 'f':
                numberSingles = numberSingles + 1
            else:
                numberDoubles = numberDoubles + 1
            if loni.dtype.char == 'f':
                numberSingles = numberSingles + 1
            else:
                numberDoubles = numberDoubles + 1

            if lato.dtype.char == 'f':
                numberSingles = numberSingles + 1
            else:
                numberDoubles = numberDoubles + 1
            if lono.dtype.char == 'f':
                numberSingles = numberSingles + 1
            else:
                numberDoubles = numberDoubles + 1

            if debug == 1:
                print 'number Singles and Doubles : ', numberSingles, numberDoubles

            if numberSingles >= numberDoubles:
                self.group = 'single' 
                if numberSingles < 4:
                    print 'Changing all the coordinate grid types to float32'
                    lati = lati.astype(numpy.float32)
                    loni = loni.astype(numpy.float32)
                    lato = lato.astype(numpy.float32)
                    lono = lono.astype(numpy.float32)
            else:
                self.group = 'double' 
                if numberDoubles < 4:
                    print 'Changing all the coordinate grid types to float64'
                    lati = lati.astype(numpy.float64)
                    loni = loni.astype(numpy.float64)
                    lato = lato.astype(numpy.float64)
                    lono = lono.astype(numpy.float64)

            if debug == 1:
                print 'self.group: ', self.group
        
            # set the self data for the input grid

            nlati = len(lati)
            nloni = len(loni)
            if nlati != nloni:
                msg = 'CANNOT CREATE INSTANCE - The input grid must be in a list form -- here the array lengths area not equal'
                raise ValueError, msg
            else:
                self.ni = nlati

            self.lati = lati
            self.loni = loni

            # set the self data for the output grid

            self.nlato = len(lato)
            self.nlono = len(lono)
            self.lato = lato
            self.lono = lono

        else:                                    # input grid is only part of the initialization 
            # check the argument list
            try: 
                size = len(lati)
                if size < 4:
                    msg = 'CANNOT CREATE INSTANCE - The length of the input lat corrrdindate grid must be greater than 3'
                    raise ValueError, msg
            except:                        
                msg = 'CANNOT CREATE INSTANCE - Input latitude must be an array'
                raise TypeError, msg
            try: 
                size = len(loni)
                if size < 4:
                    msg = 'CANNOT CREATE INSTANCE - The length of the input lon coordindate grid must be greater than 3'
                    raise ValueError, msg
            except:                        
                msg = 'CANNOT CREATE INSTANCE - Input longitude must be an array'
                raise TypeError, msg

            # make sure that the arrays are numpy arrays

            if usefilled == 'yes':
                lati = numpy.ma.filled(lati)
                loni = numpy.ma.filled(loni)

            # select the interpolation routines from the single or the double precision group - majority rules here

            numberSingles = 0
            numberDoubles = 0

            if lati.dtype.char == 'f':
                numberSingles = numberSingles + 1
            else:
                numberDoubles = numberDoubles + 1
            if loni.dtype.char == 'f':
                numberSingles = numberSingles + 1
            else:
                numberDoubles = numberDoubles + 1

            if debug == 1:
                print 'number Singles and Doubles : ', numberSingles, numberDoubles

            if numberSingles >= numberDoubles:
                self.group = 'single' 
                if numberSingles < 2:
                    print 'Changing all the coordinate grid types to float32'
                    lati = lati.astype(numpy.float32)
                    loni = loni.astype(numpy.float32)
            else:
                self.group = 'double' 
                if numberDoubles < 2:
                    print 'Changing all the coordinate grid types to float64'
                    lati = lati.astype(numpy.float64)
                    loni = loni.astype(numpy.float64)
        
            # set the self data for the input grid

            nlati = len(lati)
            nloni = len(loni)
            if nlati != nloni:
                msg = 'CANNOT CREATE INSTANCE - The input grid must be in a list form -- here the array lengths area not equal'
                raise ValueError, msg
            else:
                self.ni = nlati

            self.lati = lati
            self.loni = loni

        # set the parameter instance data to the default values

        if debug == 1:
            printStoredParameters()

        defaultDict = Cssgrid.makeDefaultParameterTable(self)

        self.sig  = eval(defaultDict['sig'][2])
        self.tol  = eval(defaultDict['tol'][2])
        self.ttf  = eval(defaultDict['ttf'][2])
        self.nls  = eval(defaultDict['nls'][2])
        self.nsg  = eval(defaultDict['nsg'][2])
        self.isg  = eval(defaultDict['isg'][2])
        self.igr  = eval(defaultDict['igr'][2])
        self.mvl  = eval(defaultDict['mvl'][2])

        if debug == 1:
            Cssgrid.printInstanceParameters(self)

    def rgrd(self, dataIn = None, compType = None, indexVoro = None, firstCall = None):
        """    --------------------------------------------------------------------------------------------------------
         routine: rgrd
     
         purpose: perform one of the following:
                      1. interpolation 
                      2. calculation of the Delaunay triangles 
                      3. calculation of the Voronoi polygons 
     
     
         usage:    Ater making an instance, r, of the Cssgrid class the following procedure accesses the functionality:

                   For interpolation from dataIn to dataOut use the statement

                        dataOut = r.rgrd(dataIn)  where dataIn has the shape (lat, lon)
     
                   To compute the triangles use the statement

                        ntri = r.rgrd(compType = 'triangles' ) where 
 
                        ntri -- a 2D integer array containing the nodes of the triangles in the triangulation. The nodes
                        for the jth triangle are in the triple composed of ntri(j,1), ntri(j,2) and ntri(j,3). These indices 
                        reference the sequence in the input coordinate grid. For example, if the triple (5,1,2) were in the 
                        list of triples, it would describe the triangle having vertices at (loni(5), lati(5)), (loni(1), lati(1))
                        and(loni(2),lati(2)). 
     
                   To compute the polygons use the statement

                       latV, lonV, rc, nv = r.rgrd(compType = 'polygons', indexVoro = ni, firstCall = nf)
 
 
                   where the meaning of the input arguments are as follows:
 
                       ni -- the index of the input coordinate for which you want to determine the Voronoi polygon. The lowest
                             value is 0.  
 
                       nf -- a flag indicating if this is the first call to retrieve Voronoi polygons for this dataset(1=yes,0=no).
                             Calls subsequent to the first call for a given dataset are much faster than the first call.
 
                   where the meaning of the output tuple is as follows:
 
                       latV -- an array of latitude values in degrees for the Voronoi indices. ( These are circumcenters of
                               circles passing through the Delaunay triangles. If a coordinate is a boundary point, then the
                               circle may pass through certain "pseudo points" that have been added to the original dataset in 
                               order to complete the Voronoi polygon.) 
 
                       lonV -- an array of longitude values in degrees for the Voronoi indices. 
 
                       rc -- Array containing the arc length (in degrees) of the angle between a circumcenter and its associated
                             triangle vertices.
 
                       nv -- an array containing  indices for the Voronoi polygon enclosing the coordinate (loni(ni), lati(ni)).
                             The indices returned in this array refer to the coordinates returned in latV and lonV.  For example,
                             if the integer "j" is an element of the nv array, then (lonV(j),latV(j)) is a vertex of the Voronoi
                             polygon enclosing (loni(ni), lati(ni)). The indices in nv list out the vertices of the Voronoi polygon
                             in counter-clockwise order.

     
                   Note: the index in the data associated with latitude varies the fastest.
     
     
         definition: rgrd(self, dataIn = None, compType = None, indexVoro = None, firstCall = None):
    --------------------------------------------------------------------------------------------------------""" 
        # Evaluate the agrument list

        if (dataIn is None and compType is None): 
            msg = 'CANNOT CARRY OUT COMPUTATION - Must pass either dataIn or compType' 
            raise ValueError, msg

        if (dataIn is not None and compType is not None): 
            msg = 'CANNOT CARRY OUT COMPUTATION - Passing dataIn and compType is inconsistent' 
            raise ValueError, msg

        if dataIn is None:
            if compType != 'triangles' and compType != 'polygons': 
                msg = 'CANNOT CARRY OUT COMPUTATION - compType must be triangles or polygons'
                raise ValueError, msg

        else:
            if len(dataIn) != self.ni:
                msg = 'CANNOT INTERPOLATE - The length of the input grids and the length of data must agree'
                raise ValueError, msg
            if usefilled == 'yes':
                dataIn = numpy.ma.filled(dataIn)

        # set the instance values of the parameters in the c code
        Cssgrid.setInstanceParameters(self)

        if dataIn is not None:                                                       #---------- check type match ----------
            if self.group == 'single':                 # single precision
                if dataIn.dtype.char != 'f':
                    self.group = 'double'
                    self.lati = self.lati.astype(numpy.float64)
                    self.loni = self.loni.astype(numpy.float64)
                    self.lato = self.lato.astype(numpy.float64)
                    self.lono = self.lono.astype(numpy.float64)
            else:                                       # double precision
                if dataIn.dtype.char != 'd':
                    self.group = 'single'
                    self.lati = self.lati.astype(numpy.float32)
                    self.loni = self.loni.astype(numpy.float32)
                    self.lato = self.lato.astype(numpy.float32)
                    self.lono = self.lono.astype(numpy.float32)

        if dataIn is not None:                                                       #---------- perform interpolation ----------
            tmp1 = numpy.zeros((27*self.ni),'i')
            tmp2 = numpy.zeros((13*self.ni),'d')
            if self.group == 'single':                 # single precision
                if debug == 1:
                    print '********** in rgrd with single precision regrid  ********'
                
                dataOut, ier = cssgridmodule.cssgrid(self.lati, self.loni, dataIn,
                                                     self.lato, self.lono, tmp1, tmp2)
            else:
                dataOut, ier = cssgridmodule.cssgridd(self.lati, self.loni, dataIn,
                                                      self.lato, self.lono, tmp1, tmp2)
            dataOut = numpy.transpose(dataOut) # replaces the pyfort mirror thing

            if ier != 0:
                msg = 'Error in return from cssgrid call with -- ' + Cssgrid.errorTable(self)[ier] 
                raise ValueError, msg

            return dataOut 

        elif compType == 'triangles':                                                #---------- calculate triangles ----------
            tmp1 = numpy.zeros((27*self.ni),'i')
            tmp2 = numpy.zeros((13*self.ni),'d')
            if self.group == 'single':                 # single precision
                if debug == 1:
                    print '********** in rgrd with single precision triangles  ********'

                nt, ntri, ier = cssgridmodule.csstri(self.lati, self.loni, tmp1, tmp2)

                if ier != 0:
                    msg = 'Error in return from csstri call with -- ' + Cssgrid.errorTable(self)[ier] 
                    raise ValueError, msg

            else:                                       # double precision
                if debug == 1:
                    print '********** in rgrd with double precision triangles  ********'

                nt, ntri, ier = cssgridmodule.csstrid(self.lati, self.loni, tmp1, tmp2)

                if ier != 0:
                    msg = 'Error in return from csstrid call with -- ' + Cssgrid.errorTable(self)[ier] 
                    raise ValueError, msg
            ntri = numpy.transpose(ntri) # mirror
            ntriTruncated = ntri[:nt,:] 
            del ntri
            ntriTruncated = ntriTruncated  - 1

            return ntriTruncated

        elif compType == 'polygons':                                                 #---------- calculate polygons ----------
            tmp1 = numpy.zeros((27*self.ni),'i')
            tmp2 = numpy.zeros((9*self.ni),'d')
            if indexVoro is None or firstCall is None:
                msg = 'CANNOT CALCULATE VERONOI POLYGONS - indexVoro and firstCall must be set. Their is no default'
                raise ValueError, msg

            indexVoro = indexVoro + 1                 # change to Fortran 1 based index
            if indexVoro >= self.ni:
                msg = 'CANNOT CALCULATE VERONOI POLYGONS - indexVoro is is too large'
                raise ValueError, msg

            if self.group == 'single':                 # single precision
                if debug == 1:
                    print '********** in rgrd with single precision polygons  ********'

                nc = 2*self.ni

                latV, lonV, rc, nca, numv, nv, ier = cssgridmodule.csvoro(self.lati, self.loni, indexVoro, firstCall, tmp1,tmp2, nc)
                if ier != 0:
                    msg = 'Error in return from csvoro call with -- ' + Cssgrid.errorTable(self)[ier] 
                    raise ValueError, msg
            else:                                       # double precision
                if debug == 1:
                    print '********** in rgrd with double precision polygons  ********'

                nc = 2*self.ni

                latV, lonV, rc, nca, numv, nv, ier = cssgridmodule.csvorod(self.lati, self.loni, indexVoro, firstCall, tmp1, tmp2, nc)
                if ier != 0:
                    msg = 'Error in return from csvorod call with -- ' + Cssgrid.errorTable(self)[ier] 
                    raise ValueError, msg

            # nca is not calculated properly in csvoro and csvorod but the number -5.729e06 in latV can be used to fix it.

            for i in range(len(latV)):
                if latV[i] < -5.0e06:
                    nca = i
                    break

            latTruncated = latV[:nca] 
            del latV
            lonTruncated = lonV[:nca]
            del lonV
            rcTruncated = rc[:nca]
            del rc

            # truncate nv to the pertinent values only and change reference to 0-based Python
            nvTruncated = nv[:numv]
            nvTruncated = nvTruncated - 1
            del nv

            return  (latTruncated, lonTruncated, rcTruncated, nvTruncated)

    #---------------------------------------------------------------------------------
    # ******************* control parameter manipulation functions *******************
    #---------------------------------------------------------------------------------

    def parameterNames(self):
        #--------------------------------------------------------------------------------
        #                                      
        #     purpose: produce a list of the cssgrid parameters
        #
        #     usage:    parameters = parameterNames(self)
        #
        #     passed:   self
        #
        #     returned: parameters
        #
        #--------------------------------------------------------------------------------

        parameters = ['name', '----', 'sig', 'tol', 'ttf', 'nls', 'nsg', 'isg', 'igr', 'mvl']

        return parameters 

    def parameterType(self):
        #--------------------------------------------------------------------------------
        #                                      
        #     purpose:  produce a dictionary connecting cssgrid parameter names and their data types 
        #
        #     usage:    typeDict = parameterType(self):
        #
        #     passed:   self
        #
        #     returned: typeDict
        #
        #--------------------------------------------------------------------------------
        typeDict = { 'sig':'float', 'tol':'float', 'ttf':'float', 'nls':'int',
                                        'nsg':'int', 'isg':'int', 'igr':'int', 'mvl':'float' }

        return typeDict 

    def makeDefaultParameterTable(self):
        #--------------------------------------------------------------------------------
        #                                      
        #     purpose:  construct the dictionary which is the cssgrid default control parameter table
        #
        #     usage:    parameterDict= makeDefaultParameterTable(self)
        #
        #     passed:   self
        #
        #     returned: parameterDict
        #
        #--------------------------------------------------------------------------------

        parameterDict = {
  'name': ('type ','legal values','default','                  description                                                '),
  '----': ('-----','-----------','------','-------------------------------------------------------------------------------'),
  'sig': ('float','>= 0.  ','1.0   ','value of tension factor for splines (0. is for cubic)                               '),
  'tol': ('float','> 0.   ','0.01  ','tolerance in making gradient differences to terminate iteration for global gradients'),
  'ttf': ('float','> 0.   ','0.01  ','tolerance in determining accuracy of tension factor to approximate optimum value    '),
  'nls': ('int  ','>= 4   ','10    ','number of nodes to use in the least squares fit                                     '),
  'nsg': ('int  ','>= 2   ','10    ','max number of iterations to use in computing automatic tension factors              '),
  'isg': ('int  ','any    ','0     ','0 to revert to calculating automatic tension factors rather than using a constant   '),
  'igr': ('int  ','any    ','1     ','flags use of global or local gradients (1 for global, anything else for local)      '),
  'mvl': ('float','any    ','-8.0  ','used by NCL functions                                                               ')}

        return parameterDict      

    def makeInstanceParameterTable(self):
        #--------------------------------------------------------------------------------
        #                                      
        #     purpose: construct the dictionary which is the instance control parameters table
        #
        #     usage: parameterDict =  makeInstanceParameterTable(self)
        #
        #     passed:   self
        #
        #     returned: parameterDict
        #
        #--------------------------------------------------------------------------------

        parameterDict = {
  'name': ('type ','legal values','value',    '                       description                                               '),
  '----': ('-----','---------','------',      '---------------------------------------------------------------------------------'),
  'sig': ('float','>= 0.',eval('self.sig'),'value of tension factor for splines (0. is for cubic)                               '),
  'tol': ('float','> 0. ',eval('self.tol'),'tolerance in making gradient differences to terminate iteration for global gradients'),
  'ttf': ('float','> 0. ',eval('self.ttf'),'tolerance in determining accuracy of tension factor to approximate optimum value    '),
  'nls': ('int  ','>= 4 ',eval('self.nls'),'number of nodes to use in the least squares fit                                     '),
  'nsg': ('int  ','>= 2 ',eval('self.nsg'),'max number of iterations to use in computing automatic tension factors              '),
  'isg': ('int  ','any  ',eval('self.isg'),'0 to revert to calculating automatic tension factors rather than using a constant   '),
  'igr': ('int  ','any  ',eval('self.igr'),'flags use of global or local gradients (1 for global, anything else for local)      '),
  'mvl': ('float','any  ',eval('self.mvl'),'used by NCL functions                                                               ')}

        return parameterDict      

    def printDefaultParameterTable(self):
        """    --------------------------------------------------------------------------------------------------------
         routine: printDefaultParameterTable
     
         purpose: after making an instance of the Cssgrid class, print the control parameter table using the default values
     
         usage:  r.printDefaultParameterTable()
                 where r is an instance of the Cssgrid class
     
         returned: None
     
         definition: printDefaultParameterTable(self):
    --------------------------------------------------------------------------------------------------------""" 

        names = Cssgrid.parameterNames(self)

        parameterDict = Cssgrid.makeDefaultParameterTable(self)
        for item in names:
            items = (item, parameterDict[item][0], parameterDict[item][1], parameterDict[item][2], parameterDict[item][3])  
            print '%-7.7s  %-6.6s  %-12.12s   %-7.7s  %s' % items

        return 

    def printInstanceParameterTable(self):
        """    --------------------------------------------------------------------------------------------------------
         routine: printInstanceParameterTable
     
         purpose: after making an instance of the Cssgrid class, print the control parameter table using the instance values
     
         usage:  r.printInstanceParameterTable()
                 where r is an instance of the Cssgrid class
     
         returned: None
     
         definition: printInstanceParameterTable(self):
    --------------------------------------------------------------------------------------------------------""" 

        names = Cssgrid.parameterNames(self)

        parameterDict = Cssgrid.makeInstanceParameterTable(self)
        for item in names:
            items = (item, parameterDict[item][0], parameterDict[item][1], parameterDict[item][2], parameterDict[item][3])  
            print '%-7.7s  %-6.6s  %-12.12s   %-7.7s  %s' % items

        return 

    def printInstanceParameters(self):
        """    --------------------------------------------------------------------------------------------------------
         routine: printInstanceParameterTable
     
         purpose: after making an instance of the Cssgrid class, print the control parameters using the instance values
     
         usage:  r.printInstanceParameters()
                 where r is an instance of the Cssgrid class
     
         returned: None
     
         definition: printInstanceParameters(self):
    --------------------------------------------------------------------------------------------------------""" 

        names = Cssgrid.parameterNames(self)
        names = names[2:]

        typeDict = Cssgrid.parameterType(self)

        for name in names:
            if typeDict[name] == 'int':
                print 'Currently, %s = %d' % (name, eval('self.' + name))
            elif typeDict[name] == 'float':
                print 'Currently, %s = %f' % (name, eval('self.' + name))

        return None      

    def setInstanceParameters(self):
        #-------------------------------------------------------------------------------
        #                                      
        #     purpose: set the instance values of the current cssgrid control parameters in c code
        #
        #     usage:  setInstanceParameters(self) 
        #
        #
        #     returned: None
        #
        #-------------------------------------------------------------------------------

        names = Cssgrid.parameterNames(self)
        names = names[2:]

        typeDict = Cssgrid.parameterType(self)

        # set the current values for the cssgrid control parameters


        for name in names:
            if typeDict[name] == 'int':
                cssgridmodule.csseti(name, eval('self.' + name))
            elif typeDict[name] == 'float':
                cssgridmodule.cssetr(name, eval('self.' + name))
            elif typeDict[name] == 'double':
                cssgridmodule.cssetrd(name, eval('self.' + name))

        return None      

    #---------------------------------------------------------------------------------
    # ***************************** Error Table  *************************************
    #---------------------------------------------------------------------------------
    def errorTable(self):
        #-------------------------------------------------------------------------------
        #                                      
        #     purpose: construct the dictionary which provides access to error messages
        #
        #     usage:  errorTable()
        #
        #     returned: errorDict
        #
        #-------------------------------------------------------------------------------
        errorDict = {
          0: 'No error',
          1: 'Invalid number of input points (must be greater than 3)',
          2: 'Invalid dimension for latitudes in the output grid',
          3: 'Invalid dimension for longitudes in the output grid',
          4: 'First three nodes of the input array are colinear',
          5: 'Extrapolation failed due to the uniform grid extending too far beyond the triangulation boundary',
          6: 'Internal algorithm error - please report this ',
          7: 'Vertex of a triangle containing an interpolation point is outside its valid range',
          8: 'The angular distance between an interpolated point and the nearest point of the trangulation is at least 90 degrees',
          9: 'Not enough points to calculate a gradient',
         10:  'Insufficient space for the triangulation (must be >= number of boundary nodes minus 2)',
         12: 'Degenerate triangle (two verices lie on same geodesic',
          L: 'Coordinates L and M coincide for some M > L >= 1 (coordinate numbering starting at 1)'}

        return errorDict      

    #---------------------------------------------------------------------------------
    # ****************  magic functions follow  *************************
    #---------------------------------------------------------------------------------

    def __setattr__(self, name, value):
        #-------------------------------------------------------------------------------
        #
        #   purpose:  '__setattr__' is called on every assignment to an instance attribute.
        #             Consequently, it must put the value in through the __dict__ to avoid
        #             calling itself and setting up an infinite recursion loop.It sets the
        #             attribute called name to value in two steps.
        #                 One -- set the global C code control parameter  
        #                 Two -- set the instance self data control parameter  
        #
        #   usage:    x.name = value   
        #
        #   passed :  name and value
        #
        #   returned: None
        #
        #-------------------------------------------------------------------------------
        typeDict = Cssgrid.parameterType(self)

        if name in typeDict.keys():
            if typeDict[name] == 'int':
                cssgridmodule.csseti(name, value)
                self.__dict__[name] = value
            elif typeDict[name] == 'float':
                cssgridmodule.cssetr(name, value)
                self.__dict__[name] = value
            elif typeDict[name] == 'double':
                cssgridmodule.cssetrd(name, value)
                self.__dict__[name] = value

        else:
            self.__dict__[name] = value

        return None

    def __getattr__(self, name):
        #-------------------------------------------------------------------------------
        #
        #   purpose:  '__getattr__' is called only if a referenced attribute can not be found
        #             in the instance. It gets the attribute from cssgridmodule if possoble.
        #
        #   usage:    x.name  -- name is the oject and not a string repr   
        #
        #   passed :  name 
        #
        #   returned: x.name 
        #
        #-------------------------------------------------------------------------------
        typeDict = Cssgrid.parameterType(self)

        if name in typeDict.keys():
            if typeDict[name] == 'int':
                value = cssgridmodule.csgeti(name)
            elif typeDict[name] == 'float':
                value = cssgridmodule.csgetr(name)[0]
            elif typeDict[name] == 'double':
                value = cssgridmodule.csgetrd(name)[0]

        else:
            raise AttributeError, name

        return value

    
    #---------------------------------------------------------------------------------
    # ****************  end of magic functions **************************
    #---------------------------------------------------------------------------------

def printParameterTable():
    """    --------------------------------------------------------------------------------------------------------
     routine:  printParameterTable
     
     purpose:  print the control parameter table using the default values from outside the Cssgrid class
     
     usage:    import css
               css.printParameterTable()
     
     passed:   nothing 
     
     returned: None
     
     definition: printParameterTable():
--------------------------------------------------------------------------------------------------------""" 

    parameterDict = {
  'name': ('type ','legal values','default','                  description                                                    '),
  '----': ('-----','-----------','------','-----------------------------------------------------------------------------------'),
  'sig': ('float','>= 0.  ','1.0   ','value of tension factor for splines (0. is for cubic)                               '),
  'tol': ('float','> 0.   ','0.01  ','tolerance in making gradient differences to terminate iteration for global gradients'),
  'ttf': ('float','> 0.   ','0.01  ','tolerance in determining accuracy of tension factor to approximate optimum value    '),
  'nls': ('int  ','>= 4   ','10    ','number of nodes to use in the least squares fit                                     '),
  'nsg': ('int  ','>= 2   ','10    ','max number of iterations to use in computing automatic tension factors              '),
  'isg': ('int  ','any    ','0     ','0 to revert to calculating automatic tension factors rather than using a constant   '),
  'igr': ('int  ','any    ','1     ','flags use of global or local gradients (1 for global, anything else for local)      '),
  'mvl': ('float','any    ','-8.0  ','used by NCL functions                                                               ')}

    names = ['name', '----', 'sig', 'tol', 'ttf', 'nls', 'nsg', 'isg', 'igr', 'mvl']

    for item in names:
        items = (item, parameterDict[item][0], parameterDict[item][1], parameterDict[item][2], parameterDict[item][3])  
        print '%-7.7s  %-6.6s  %-12.12s   %-7.7s  %s' % items

    return None      


def printStoredParameters():
    """    --------------------------------------------------------------------------------------------------------
     routine:  printStoredParameters
     
     purpose:  print the current control parameter as seen by the Fortran code. The call to the method function rgrd will
               change them to the instance values.
     
     usage:    import css
               css.printStoredParameters()
     
     passed:   nothing 
     
     returned: None
     
     definition: def printStoredParameters():
--------------------------------------------------------------------------------------------------------""" 

    names = ['name', '----', 'sig', 'tol', 'ttf', 'nls', 'nsg', 'isg', 'igr', 'mvl']
    names = names[2:]

    typeDict = { 'sig':'float', 'tol':'float', 'ttf':'float', 'nls':'int',
                                'nsg':'int', 'isg':'int', 'igr':'int', 'mvl':'float' }

    for item in names:
        if typeDict[item] == 'int':
            print 'Currently, %s = %d' % (item, cssgridmodule.csgeti(item))
        elif typeDict[item] == 'float':
            print 'Currently, %s = %f' % (item, cssgridmodule.csgetr(item)[0])
        elif typeDict[item] == 'double':
            print 'Currently, %s = %f' % (item, cssgridmodule.csgetrd(item)[0])

    return None      

def help(choice = None):
    import css

    if choice is None:                               # get instructions for use of help
        print """     ----------------------------------------------------------------------------------------
      
      INSTRUCTIONS ON USE THE OBJECT ORIENTED INTERFACE TO THE CSSGRID PACKAGE FROM NGMATH 
     
      This module is built as one class, Cssgrid, which sports a single method called rgrd.
     
      To get instructions on making an instance of Cssgrid, type
     
              css.help('Cssgrid')
     
      To get instructions on using the control parameters, type
     
              css.help('parameters')
     
      To print the table describing the control parameters, type
     
              css.help('table')
     
      To get instructions on performing an interpolation, type
     
              css.help('interpolate')
      
      To get instructions on calculating Delauanay triangles, type
     
              css.help('triangles')
      
      To get instructions on calculating Voronoi polygons, type
     
              css.help('polygons')
      
     
      INSTRUCTIONS ON USE OF ORIGINAL CSSGRID PACKAGE FROM NGMATH 
     
      This module is built as an interface to cssgridmodule.so which exports the following functions:
                                                                                                    
                        Single precision procedures:                                                    
                                                                                                    
                            cssgrid -- interpolation on a sphere
                            cssgtri -- calculates a Delaunay triangulation
                            csvoro  -- calculates Voronoi polygons
                            csseti -- sets integer parameter values 
                            csgeti -- retrieves values for integer parameter values 
                            cssetr -- sets real parameter values 
                            csgetr -- retrieves values for real parameter values 
                                                                                                    
                        Double precision procedures:                                                    
                                                                                                    
                            cssgridd -- interpolation on a sphere
                            cssgtrid -- calculates a Delaunay triangulation
                            csvorod -- calculates Voronoi polygons
                            cssetd -- sets double precision parameter values 
                            csgetd -- retrieves values for double precision parameter values 
                                                                                                    
                        In addition it contains:
                                                                                                    
                            css2c -- converts lat/lon to Cartesian coordinates
                            csc2s -- converts Cartesian to lat/lon coordinates
                                                                                                    
          It is feasible to use these functions directly without this module. Information is available
          through their docstrings. For example, to get the docstring for the routine cssgrid, follow this 
          procedure at the Python prompt:
     
                import cssgridmodule
     
                print cssgridmodule.cssgrid.__doc__
            
                or simply type
     
                css.help('cssgrid')
 
          The information obtained from the documentation and, in particular, the docstrings describe the calls from Python.
          Consequenlty, indices used in these Python calls start at 0 while those in the Fortran based calls start at 1. In 
          addition, the two dimenensional arrays are transposed in thier description even though they are physically identical.
          For example, an array dimensioned in the order (lat,lon) in Fortran  implies that the lat dimension varies the
          fastest.  The same data described in Python or C is dimensioned (lon,lat) which implies that the lat dimension varies 
          fastest.
          The long and the short of this is as follows. If you use the interface, indices start at 0. If you call the routines
          directly, indices atart at 1. In either case, data is passed with the latitude index moving faster than the longitude
          index. 
     
     ------------------------------------------------------------------------------------------------------"""

    elif choice == 'Cssgrid': 
        print """     ----------------------------------------------------------------------------------------
                                           
     
            To make an instance, r, type:
     
                import css
     
                r = css.Cssgrid(lati, loni, lato, lono) 

                or

                r = css.Cssgrid(lati, loni)
     
                    where lati,loni and lato,lono are the input and output grid coordinate arrays. The input grid must be
                    organized in a list format. The size of the lati array and the loni array are necessarily equal. For
                    example, if here are n input data points randomly spaced on the sphere, there are n latitudes and n 
                    longitudes. The output grid coordinate arrays describe a rectangular grid on the sphere.

                    The choice between the two examples is made according to requirements in subseqent calls to the method
                    functions. The first choice is necessary for the interpolation from the input to the output grid. The
                    latter choice is sufficient if the subsequent request is for Delauanay triangles or Voronoi polygons
                    which require only an input grid.

                    The grid coordinate arrays may be single or double precision. The choice determines whether the subsequent
                    computations are single or double precision.
                                           
     --------------------------------------------------------------------------------------------------------------------"""

    elif choice == 'parameters': 
        print """     ----------------------------------------------------------------------------------------
                                           
             In the absence of an instance of the class Cssgrid, a description of the control parameters can be found 
             by typing
     
               import css
               css.printParameterTable()
     
     
            The control parameters are easily available within the class. First make an instance with:
     
                import css
     
                r = css.Cssgrid(lati, loni, lato, lono) 

                or

                r = css.Cssgrid(lati, loni)
     
     
            Then to look at the default settings for the control parameters, type
     
                r.printDefaultParameterTable()
     
            To change a setting type the new value. For example, to set sig to 0.5, type
     
                r.sig = 0.5
     
            To find a value without printing the table, type the name. For example, to exam the value of nsg, type
     
                r.nsg
     
            To check the settings type
     
                r.printInstanceParameterTable() -- prints the table with values and a description of the parameters
                                                   used in subsequent calls to the method function rgrd
                or

                r.printInstanceParameters()     -- prints a list of the parameters values used in subsequent calls to the 
                                                   the rgrd method

                css. printStoredParameters()    -- prints the parameters in memory which may differ from the above if the
                                                   user has made more than one instance of the Cssgrid class.
     --------------------------------------------------------------------------------------------------------------------"""

    elif choice == 'table': 
        printParameterTable()
     
    #-----------------------------------------------------------------------------------------------------

    elif choice == 'interpolate':  
        print """     ----------------------------------------------------------------------------------------
     
            Make an instance, r, by typing:
     
                import css
     
                r = css.Cssgrid(lati, loni, lato, lono) 
     
                    where:

                        lati and loni are the input 'grid' arrays in list format. The format where the size of the lati
                        array and the loni array are necessarily equal. For example, if here are n input data points randomly
                        spaced on the sphere, there are n latitudes and n longitudes. 

                        lato and lono are arrays describing the output rectilinear grid
     
            Organize the randomly spaced data on the sphere, dataIn, so the the latitude index varies the fastest.
            Interpolate dataIn, associated with the list arrays (loni, lati) to the rectilinear grid (lono, lato) with 
     
                dataOut = r.rgrd(dataIn)
     
                    where dataOut is the interpolated data on the output grid.
     
     --------------------------------------------------------------------------------------------------------------------"""

    elif choice == 'triangles':  
        print """     ----------------------------------------------------------------------------------------
     
            Make an instance, r, by typing:
     
                import css
     
                r = css.Cssgrid(lati, loni) 
     
                    where
                        lati and loni are the input 'grid' arrays in list format. The format where the size of the lati
                        array and the loni array are necessarily equal. For example, if here are n input data points randomly
                        spaced on the sphere, there are n latitudes and n longitudes. 
     
            Find the triangulation associated with the grid (loni,lati) with
     
                ntri = r.rgrd(compType = 'triangles' )

                    where 
     
                        ntri -- a 2D integer array containing the nodes of the triangles in the triangulation. The nodes
                                for the jth triangle are in the triple composed of ntri(j,1), ntri(j,2) and ntri(j,3). These
                                indices reference the sequence in the input coordinate grid. For example, if the triple (5,1,2)
                                were in the list of triples, it would describe the triangle having vertices at (loni(5), lati(5)),
                                (loni(1), lati(1)) and (loni(2),lati(2)). 
     
     --------------------------------------------------------------------------------------------------------------------"""

    elif choice == 'polygons':  
        print """     ----------------------------------------------------------------------------------------
     
            Make an instance, r, by typing:
     
                import css
     
                r = css.Cssgrid(lati, loni) 
     
                    where
                        lati and loni are the input 'grid' arrays in list format. The format where the size of the lati
                        array and the loni array are necessarily equal. For example, if here are n input data points randomly
                        spaced on the sphere, there are n latitudes and n longitudes. 
     
            Find the polygons associated with the grid (loni,lati) with
     
                latV, lonV, rc, nv = r.rgrd(compType = 'polygons', indexVoro = ni, firstCall = nf)
     
                       where the meaning of the input arguments are as follows:
     
                           ni -- the index of the input coordinate for which you want to determine the Voronoi polygon. This is
                                 a pass to Fortran. The lowest value is 1 -- not 0.  
     
                           nf -- a flag indicating if this is the first call to retrieve Voronoi polygons for this dataset 
                                 (1=yes, 0=no). Calls subsequent to the first call for a given dataset are much faster than the
                                 first call.
     
                       where the meaning of the output tuple is as follows:
     
                           latV -- an array of latitude values in degrees for the Voronoi indices. ( These are circumcenters
                                   of circles passing through the Delaunay triangles.  If a coordinate is a boundary point,
                                   then the circle may pass through certain "pseudo points" that have been added to the original
                                   dataset in order to complete the Voronoi polygon.) 
     
                           lonV -- an array of longitude values in degrees for the Voronoi indices. 
     
                           rc -- Array containing the arc length (in degrees) of the angle between a circumcenter and its
                                 associated triangle vertices.
     
                           nv -- an array containing indices for the Voronoi polygon enclosing the coordinate (loni(ni),
                                 lati(ni)). The indices returned in this array refer to the coordinates returned in latV and
                                 lonV.  For example, if the integer "j" is an element of the nv array, then (lonV(j),latV(j))
                                 is a vertex of the Voronoi polygon enclosing (loni(ni), lati(ni)). The indices in nv list out
                                 the vertices of the Voronoi polygon in counter- clockwise order.
     
     --------------------------------------------------------------------------------------------------------------------"""

    elif choice == 'cssgtri':
        print cssgridmodule.cssgtri.__doc__
    elif choice == 'csvoro':
        print cssgridmodule.csvoro.__doc__
    elif choice == 'csseti':
        print cssgridmodule.csseti.__doc__
    elif choice == 'csgeti':
        print cssgridmodule.csgeti.__doc__
    elif choice == 'cssetr':
        print cssgridmodule.cssetr.__doc__
    elif choice == 'csgetr':
        print cssgridmodule.csgetr.__doc__
    elif choice == 'cssgtrid':
        print cssgridmodule.cssgtrid.__doc__
    elif choice == 'csvorod':
        print cssgridmodule.csvorod.__doc__
    elif choice == 'cssetd':
        print cssgridmodule.cssetd.__doc__
    elif choice == 'csgetd':
        print cssgridmodule.csgetd.__doc__
    elif choice == 'css2c':
        print cssgridmodule.css2c.__doc__
    elif choice == 'csc2s':
        print cssgridmodule.csc2s.__doc__
    else:
        print 'Your request is not in help.   The help choices are: ' 
        print 'Cssgrid, parameters, interpolate,triangles, polygons,cssgrid, cssgtri, csvoro, csseti, csgeti, cssetr, csgetr, cssgtrid, csvorod, cssetd, csgetd, css2c, csc2s'

    return None

