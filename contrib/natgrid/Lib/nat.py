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
  
   NATGRID PACKAGE
  
        natgrid implements a natural neighbor interpolation method. The input for the interpolation is a set
        of randomly spaced two-dimensional coordinates with functional values at those coordinates; the output is a
        set of interpolated values at coordinates in a user specified rectangular grid. The coordinates in the output
        grid must be monotonic in each coordinate direction, but need not be evenly spaced. It is also possible to
        interpolate at a single point.
  
        natgrid uses a weighted average method that is much more sophisticated than the inverse distance weighted
        average used by dsgrid. One distinguishing quality of natural neighbor interpolation is the way in which
        a set of neighboring points (the natural neighbor) is selected to use for interpolating at a point. The
        natural neighbor selection process avoids the problems common to methods based on choosing a fixed number 
        of neighboring points, or all points within a fixed distance. Another distinguishing quality of natural
        neighbor interpolation is the way that the weights are calculated for the functional values at the natural
        neighbor coordinates. These weights are based on proportionate area, rather than distances.
  
        The method of finding the natural neighbors and calculating area-based weights to produce interpolated
        values is called natural neighbor linear interpolation. This produces an interpolation surface that has a
        continous slope at all points, except at the original input points. The result of natural neighbor linear
        interpolation can be visualized as producing a snugly fit sheet stretched over all of the input points.
  
        The interpolation method in natgrid also allows for natural neighbor linear interpolation augmented by
        blending in gradient estimates. This is called natural neighbor nonlinear interpolation. It produces an
        interpolation surface that has a continuous slope at all locations; two tautness parameters can be set by
        the user to control the apparent smoothness of the output surface.
 
   NATGRID CONTENTS
  
        Access through Python to the natgrid package from NCAR's ngmath distribution is provided directly through the module
        natgridmodule.so which was generated as a Python C language extension in order to export the natgrid functions 
        from the original C language library to Python.
        
   REQUIRED FILE
 
        natgridmodule.so -- the Python interface to the ngmath natgrid package.
 
   USEFUL FILES
 
        nat.py -- the object oriented interface including a general help package.
        natgridtest.py -- the code to test nat.py and to write documentation.

   USAGE
 
        This module is designed to use in two ways. One is through the use of the object oriented interface to the underlying
        functions. This approach is recommended for users not already familiar with the original natgrid distribtution because
        it simplifies the calls to the routines. The other method uses the original functions calling them directly from Python. 
 
        -------------------  OBJECT ORIENTED APPROACH ----------------
 
            The nat module contains the Natgrid class and its single method, rgrd, which provides access to all the natgrid 
            functions. The object oriented approach has been organized as a two step process.
 
            STEP 1. 
 
            To make an instance, r, type:
 
                import nat
 
                r = nat.Natgrid(xi, yi, xo, yo)
                or 
                r = nat.Natgrid(xi, yi, xo, yo, listOutput = 'yes')
 
                    where xi, yi and xo, yo are the input and output grid coordinate arrays. The optional listOutput must
                    set to anything except 'no' if xo, yo are in list format as explained below. It is the responsibility
                    of the user to set listOutput if the output is in the list form.

                The input grid must be organized in a list format always. The size of the xi array and the yi array are 
                necessarily equal. For example, if there are n randomly spaced input data points, there
                are n values in xi and n values in yi. 

                There are two possible formats for the output grid. The output grid coordinate arrays may be a list like 
                the input array or it may be a rectangular grid.  The choice between the two posibilities is made according
                to requirements in subseqent calls to the method function. The first choice is required if the subsequent
                call is to the single point mode interpolation. The list can have one or more points. Of course, the list
                could describe a rectangular grid. For example, a rectangular grid with 10 x values and 20 y values can be
                rewrtten in list form with 200 x value and 200 y values. However, this form requires calling the slower 
                single point interpolator. The second choice is most efficient for the basic interpolation to a rectangular
                output grid. The output grid must be monotonic but need not be equally spced.
                 
                The grid coordinate arrays can be single precision (numpy.float32) or double precision (numpy.float64). The
                decision on whether to call for a single or a double precision computation subsequently is made by looking at
                the type of these arrays. 
 
            To look at the default settings for the control parameters and a brief description of thier properties, type
 
                r.printDefaultParameterTable()
 
            To change a setting type the new value. For example, to set igr to 1, type
 
                r.igr = 1
 
            To find a value without printing the table, type the name. For example, to exam the value of hor, type
 
                r.hor
 
            To check the settings type
 
                r.printInstanceParameterTable()   -- prints in tabular form the parameters used in subsequent calls to the method
                                                     function rgrd.
                or

                printStoredParameters()           -- prints the parameters in memory which may differ from the above if the user
                                                     has made more than one instance of the Natgrid class.
 
            STEP 2. 

            natgrid is restricted to two dimensions . Consequently, it is the user's responsibility to reduce the processing of
            higher dimensional data to a sequence of calls using only two dimensional data.

            The computations are divided into two groups depending on whether the output arrays are in list form or in rectilinear
            grid form. If they are in list format the single point mode is called to interpolate to those individual points. This is
            the only process possible. On the other hand, if the output goes to a rectangular grid there are more choices. In
            addition to carrying out linear and nonlinear interpolations, it is possible to request aspects and slopes. The aspect
            at a point on the interpolated surface is the direction of steepest descend. The slope is the value of the partial
            derivative taken in the direction of the aspect. The slope is measured as an angle that is zero in a horizonal surface
            and positive below the horizontal. 

            The following examples cover the basic computations. They start with a indication of the appropriate STEP 1.
 
            Example 1: the basic natural neighbor linear interpolation

                As STEP 1 make an instance, r, with:
 
                     import nat
 
                     r = nat.Natgrid(xi, yi, xo, yo)

                     where the xo, yo grid is rectilinear as explained above in STEP 1.
 
                Then call the primary interpolation computation to regrid the input data, dataIn, on the grid (xi, yi) to
                the output data, dataOut, on the grid (xo, yo), with
 
                    dataOut = r.rgrd( dataIn )
 
                The computation is either single or double precision as determined by the precision submitted in the grid
                description in STEP 1.

                It is also possible to request a wrap in the input grid and the input data in the longitude direction, assumed
                to be the yi grid coordinate, by adding a keyword as 
 
                     dataOut = r.rgrd( dataIn, wrap = 'yes' )

 
            Example 2: natural neighbor linear interpolation returning the aspect and the slope.

                As STEP 1 make an instance, r, with:
 
                     import nat
 
                     r = nat.Natgrid(xi, yi, xo, yo)

                     where the xo, yo grid is rectilinear as explained above in STEP 1.
 
                Then call the primary interpolation computation to regrid the input data, dataIn, on the grid (xi, yi) to
                the output data, dataOut, on the grid (xo, yo), while asking for the aspect and the slope on this output grid, with
 
                     dataOut, a, s = r.rgrd( dataIn, aspectSlope = 'yes' )
 
                     where a is the aspect, the direction of the steepest descent in degrees measured from 'north' and s is the
                     slope in degrees measured from the horizontal. Necessarily, these are arrays aligned with the rectilinear
                     output grid, xo, yo.

                The computation is either single or double precision as determined by the precision submitted in the grid
                description in STEP 1.

                It is also possible to request a wrap in the input grid and the input data in the longitude direction, assumed
                to be the yi grid coordinate, by adding a keyword as 
 
                     dataOut, a, s = r.rgrd( dataIn, aspectSlope = 'yes', wrap = 'yes' )


            Example 3: the basic natural neighbor nonlinear interpolation

                The procedure for the nonlinear interpolation differs from the linear case in the need to set the control
                parameter igr. Follow Example 1 and insert the following statament after making the instance, r.
 
                     r.igr = 1 
 
            Example 4: natural neighbor nonlinear interpolation returning the aspect and the slope.

                The procedure for the nonlinear interpolation differs from the linear case in the need to set the control
                parameter igr. Follow Example 2 and insert the following statament after making the instance, r.
 
                     r.igr = 1 
 
            Example 5: single point mode natural neighbor linear interpolation
 
                As STEP 1 make an instance, r, with:
 
                     import nat
 
                     r = nat.Natgrid(xi, yi, xo, yo, listOutput = 'yes')

                     where the xo, yo output grid is in the list form (not a rectangular output grid) as explained above in
                     STEP 1.

                To call the single point mode interpolation computation to regrid the input data, dataIn, on the grid (xi, yi)
                to the output data, dataOut, on the grid (xo, yo), type
 
                    dataOut = r.rgrd( dataIn )

                The computation is either single or double precision as determined by the precision submitted in the grid
                description in STEP 1. In the single point mode it is not possible to request the aspect and the slope.
 

            Example 6: single point mode natural neighbor nonlinear interpolation
 
                The procedure for the nonlinear interpolation differs from the linear case in the need to set the control
                parameter igr. Follow Example 5 and insert the following statament after making the instance, r.
 
                     r.igr = 1 

        -------------------  ORIGINAL FUNCTION APPROACH -----------------
 
            The module natgridmodule.so exports the following functions to Python from the original ngmath C  library:
        
                    Single precision procedures:                                                    
                                                                                                
                        natgrids     - primary function for gridding.                         
                        seti         - set int parameter values.                                
                        geti         - retrieve values for int parameters.                      
                        setr         - set float parameter values.                              
                        getr         - retrieve values for float parameters                     
                        setc         - set char parameter values.                                
                        getc         - retrieve values for char parameters.                     
                        getaspects   - get aspect values, if calculated by setting sdi = 1.                        
                        getslopes    - get slope values, if calculated by setting sdi = 1.                         
                        pntinits     - initiate single point mode.                              
                        pnts         - interpolate at a single point.                           
                        pntend       - terminate single point mode.                             
                                                                                                
                                                                                                
                    Double precision procedures:                                                    
                                                                                                
                        natgridd     - primary function for gridding.                         
                        setrd        - set float parameter values.                              
                        getrd        - retrieve values for float parameters                     
                        getaspectd   - get aspect values, if calculated by setting sdi = 1.                        
                        getsloped    - get slope values, if calculated by setting sdi = 1.                         
                        pntinitd     - initiate single point mode.                              
                        pntd         - interpolate at a single point.                           
                        pntendd      - terminate single point mode.                             
 



            Information on the use of the routines is available by importing natgridmodule and printing the docstring
            of interest. For example, documentation for the routine natgrids is obtained by typing
 
                import natgridmodule
                print natgridmodule.natgrids.__doc__
 
            This same information is available in the help package.
 
            A description of the control parameters is not in the natgridmodule documentation. It can be found by typing
 
               import nat
               nat.printParameterTable()
 
 
            The documentation associated with the natgridmodule.so, such as the doctrings, describe the C code. 
 
   DOCUMENTATION
  
        Documentation is provided through Python's docstrings, essentially Python style program
        comments. A help package provides instructions on the use of the natgrid module. A table of contents
        is printed to the screen by typing
  
                nat.help()
  
        after importing nat.
  
        A hard copy of all the pertinent 'docstring' documentation written to the file natgridmodule.doc can 
        be produced by typing 
  
               nat.document() 
  
        
        As an alternate to using the help package, online documentation for the natgrids function, for example,
        is available directly from the natgrids doctring by typing
  
            import natgridmodule
 
            print natgridmodule.natgrids.__doc__
  
  
   TESTING 
 
       To run a test of the natgrid computations and to get a copy of this documentation, type
                       
           cdat natgridtest.py
 
--------------------------------------------------------------------------------------------------------------"""

import string, math, sys, numpy, cdms2, natgridmodule

writeTestcase = 'yes'
try:
    import cdms2
except ImportError:
    print 'Can not write test case results to netCDF files without module cdms'
    writeTestcase = 'no'

usefilled = 'yes'
try:
    import numpy.ma
except ImportError:
    print 'Can not convert from numpy.ma array to numpy array without module numpy.ma'
    usefilled = 'no'

debug = 0

class Natgrid:

    #-------------------------------------------------------------------------------------------------------------
    # 
    #       Contents of Natgrid class
    #
    #
    #    Natgrid class
    #        __init__              --  initialization
    #        rgrd                  --  the regridder called from Python
    #
    #        rgrdPrimary           -- called by rgrd if the output grid is montonically increasing
    #        rgrdSinglePoint       -- called by rgrd if the output grid is random or single point mode is selected
    #        setInstanceParameters -- sets the C values to the instance values
    #
    #---------------------------------------------------------------------------------------------------------------

    def __init__(self, xi, yi, xo, yo, listOutput = 'no'): 
        """    --------------------------------------------------------------------------------------------------------
     
         routine:  __init__ for class Natgrid
     
         purpose:  init makes an instance of the Natgrid class while performing the following:
     
                       1. checks the argument list for the correct types.
                       2. selects single or double precision computation.
                       3. assigns the coordinate grid arrays to self data.
                       4. assigns default control parameter values from the parameter dictionary.
     
         usage:     r = nat.Natgrid(xi, yi, xo, yo)
                    or 
                    r = nat.Natgrid(xi, yi, xo, yo, listOutput = 'yes')
 
                        where xi, yi and xo, yo are the input and output grid coordinate arrays. The optional listOutput is
                        set to anything except 'no' if xo, yo are in list format as explained below.

                    The input grid must be organized in a list format always. The size of the xi array and the yi array are 
                    necessarily equal. For example, if there are n randomly spaced input data points, there
                    are n values in xi and n values in yi. 

                    There are two possible formats for the output grid. The output grid coordinate arrays may be a list like 
                    the input array or it may be a rectangular grid.  The choice between the two posibilities is made according
                    to requirements in subseqent calls to the method function. The first choice is required if the subsequent
                    call is to the single point mode interpolation. The list can have one or more points. Of course, the list
                    could describe a rectangular grid. For example, a rectangular grid with 10 x values and 20 y values can be
                    rewrtten in list form with 200 x value and 200 y values. However, this form requires calling the slower 
                    single point interpolator. The second choice is most efficient for the basic interpolation to a rectangular
                    output grid. The output grid must be monotonic but need not be equally spced.
         
                   Note: the index in the data associated with y varies the fastest.
         
         definition: __init__(self, xi, yi, xo, yo, listOutput = 'no'): 
    --------------------------------------------------------------------------------------------------------""" 

        # ---- check the input grid argument list 

        try: 
            size = len(xi)
        except:                        
            msg = 'CANNOT CREATE INSTANCE - The first argument must be an array'
            raise TypeError, msg
        if size < 4:
            msg = 'CANNOT CREATE INSTANCE - The length of the input x coordindate grid must be greater than 3'
            raise ValueError, msg

        try: 
            size = len(yi)
        except:                        
            msg = 'CANNOT CREATE INSTANCE - The third argument must be an array'
            raise TypeError, msg
        if size < 4:
            msg = 'CANNOT CREATE INSTANCE - The length of the input y coordindate grid must be greater than 3'
            raise ValueError, msg

        # set the self data for the input grid

        self.nxi = len(xi)
        self.nyi = len(yi)
        if self.nxi != self.nyi:
            msg = 'CANNOT CREATE INSTANCE - The length of the input x and y coordindate grids must be equal'
            raise ValueError, msg

        self.xi = xi
        self.yi = yi

        
        # ---- check the output grid argument list 

        try:
            size = len(xo)
        except:                        
            msg = 'CANNOT CREATE INSTANCE - The second argument must be an array'
            raise TypeError, msg
        try: 
            size = len(yo)
        except:                        
            msg = 'CANNOT CREATE INSTANCE - The fourth argument must be an array'
            raise TypeError, msg

        # set the self data for the output grid

        self.nxo = len(xo)
        self.nyo = len(yo)

        if listOutput == 'no':
            self.xo, self.yo, monotonic, self.xreverse, self.yreverse = checkdim(xo, yo)  # monotonicity check

            if monotonic == 'no':
                msg = 'CANNOT CREATE INSTANCE - Rectangular output grid must be monotonic'
                raise ValueError, msg
            self.listOutput = 'no'
        else:
            if self.nxo != self.nyo:
                msg = 'CANNOT CREATE INSTANCE - The list type output arrays must have the same length'
                raise ValueError, msg
            else:
                self.xo = xo
                self.yo = yo
                self.xreverse = 'no'
                self.yreverse = 'no'
                self.listOutput = 'yes'

        # select the interpolation routines from the single or the double precision group - majority rules here

        numberSingles = 0
        numberDoubles = 0

        if xi.dtype.char == 'f':
            numberSingles = numberSingles + 1
        else:
            numberDoubles = numberDoubles + 1
        if xo.dtype.char == 'f':
            numberSingles = numberSingles + 1
        else:
            numberDoubles = numberDoubles + 1

        if yi.dtype.char == 'f':
            numberSingles = numberSingles + 1
        else:
            numberDoubles = numberDoubles + 1
        if yo.dtype.char == 'f':
            numberSingles = numberSingles + 1
        else:
            numberDoubles = numberDoubles + 1
        
        if debug == 1:
            print 'number Singles and Doubles : ', numberSingles, numberDoubles

        if numberSingles >= numberDoubles:
            self.group = 'single' 
            if numberSingles < 4:
                sendmsg('Changing all the coordinate grid types to float32')
                xi = xi.astype(numpy.float32)
                xo = xo.astype(numpy.float32)
                yi = yi.astype(numpy.float32)
                yo = yo.astype(numpy.float32)
        else:
            self.group = 'double' 
            if numberDoubles < 4:
                sendmsg('Changing all the coordinate grid types to float64')
                xi = xi.astype(numpy.float64)
                xo = xo.astype(numpy.float64)
                yi = yi.astype(numpy.float64)
                yo = yo.astype(numpy.float64)
        
        # set the parameter instance data to the default values

        defaultDict = Natgrid.makeDefaultParameterTable(self)

        self.adf  = eval(defaultDict['adf'][2])
        self.alg  = eval(defaultDict['alg'][2])
        self.asc  = eval(defaultDict['asc'][2])
        self.bI   = eval(defaultDict['bI'][2])
        self.bJ   = eval(defaultDict['bJ'][2])
        self.dup  = eval(defaultDict['dup'][2])
        self.ext  = eval(defaultDict['ext'][2])
        self.hor  = eval(defaultDict['hor'][2])
        self.igr  = eval(defaultDict['igr'][2])
        self.magx = eval(defaultDict['magx'][2])
        self.magy = eval(defaultDict['magy'][2])
        self.magz = eval(defaultDict['magz'][2])
        self.non  = eval(defaultDict['non'][2])
        self.nul  = eval(defaultDict['nul'][2])
        self.rad  = eval(defaultDict['rad'][2])
        self.sdi  = eval(defaultDict['sdi'][2])
        self.upd  = eval(defaultDict['upd'][2])
        self.ver  = eval(defaultDict['ver'][2])

    def rgrd(self, dataIn, aspectSlope = 'no', wrap = 'no'):
        """    --------------------------------------------------------------------------------------------------------
         routine: rgrd 
     
         purpose: Perform one of the following:
                      1. natural neighbor linear interpolation to a rectilinear grid
                      2. natural neighbor linear interpolation to a rectilinear grid returning aspects and slopes
                      3. natural neighbor linear interpolation to a list of points in the single point mode
                      4. natural neighbor nonlinear interpolation to a rectilinear grid
                      5. natural neighbor nonlinear interpolation to a rectilinear grid returning aspects and slopes
                      6. natural neighbor nonlinear interpolation to a list of points in the single point mode
     
                  Each of the computations can be single or double precison. The choice is made by examing the precision
                  in the grid coordinate arrays. In addition, the choice of the single point mode is determined by the
                  set of the listOuput parameter in creating an instance of the Natgrid class.

                  Assuming that the instance, r, has been constructed, the choice between a linear or a nonlinear 
                  computation is made with the control parameter igr. The default calls for a linear calculation. To
                  call for a nonlinear one, type

                      r.igr = 1
     
         usage:  To interpolate the input data, dataIn, to the output data, dataOut, on the output grid, type

                     dataOut = r.rgrd(dataIn)
     
                 If the output grid is rectangular, it is possible to request the associated aspects and slopes with

                     dataOut, aspect, slope = r.rgrd(dataIn, aspectSlope = 'yes')
     
                 For global latitude-longitude grids, it is also possible to request a wrap in the input grid and the input
                 data in the longitude direction, assumed to be the yi grid coordinate, (with or without associated aspects
                 and slopes) with

                     dataOut, aspect, slope = r.rgrd(dataIn, wrap = 'yes')
                     or
                     dataOut, aspect, slope = r.rgrd(dataIn, aspectSlope = 'yes', wrap = 'yes')
         
         definition: rgrd(self, dataIn, aspectSlope = 'no', wrap = 'no'):
    --------------------------------------------------------------------------------------------------------""" 
        if self.nxi != len(dataIn):
            msg = 'CANNOT CREATE INSTANCE - The length of the input coordindate grids and the data must be equal'
            raise ValueError, msg

        if usefilled == 'yes':
            dataIn = numpy.ma.filled(dataIn)

        # set the instance values of the parameters in the c code
        Natgrid.setInstanceParameters(self)

        if wrap == 'yes':
            self.xi, self.yi, dataIn = Natgrid.wrapAll(self, self.xi, self.yi, dataIn)
            self.nxi = len(self.xi)
            self.nyi = len(self.yi)

        if dataIn.dtype.char == 'f':                                                 # single precision
            if self.group == 'double':                                               # change the grid type to match dataIn
                self.group = 'single'                                                # change the grid type to match dataIn
                self.xi = self.xi.astype(numpy.float32)
                self.xo = self.xo.astype(numpy.float32)
                self.yi = self.yi.astype(numpy.float32)
                self.yo = self.yo.astype(numpy.float32)
        else:                                                                      # double precision
            if self.group == 'single':                                              # change the grid type to match dataIn
                self.group = 'double'                                               # change the grid type to match dataIn
                self.xi = self.xi.astype(numpy.float64)
                self.xo = self.xo.astype(numpy.float64)
                self.yi = self.yi.astype(numpy.float64)
                self.yo = self.yo.astype(numpy.float64)

        if self.listOutput == 'no':                                      # output grid is rectangular
            t = Natgrid.rgrdPrimary(self, dataIn, aspectSlope)

        else:                                                            # output grid is a list
            t = Natgrid.rgrdSinglePoint(self, dataIn)

        return t

    def rgrdPrimary(self, dataIn, aspectSlope):
        """        #-------------------------------------------------------------------
        #
        #
        #-------------------------------------------------------------------------"""
        if aspectSlope != 'no':
            self.sdi = 1                                       # calculate aspects and slopes 

        # set the instance values of the parameters in the c code
        #Natgrid.setInstanceParameters(self)

        if dataIn.dtype.char == 'f':                                                 # single precision
            if debug == 1:
                print 'In rgrdPrimary calling natgrids'

            dataOut, ier = natgridmodule.natgrids(self.nxi, self.xi, self.yi, dataIn, self.nxo, self.nyo, self.xo, self.yo)

            if ier != 0:
                msg = 'Error in return from natgrids call with -- ' + Natgrid.errorTable(self)[ier] 
                raise ValueError, msg

            if aspectSlope != 'no':

                nxo = self.nxo
                nyo = self.nyo
                a = numpy.zeros((nxo, nyo), numpy.float32)

                for i in range(nxo):
                    for j in range(nyo):
                        uvtemp, ier = natgridmodule.getaspects(i, j)
                        if ier != 0:
                            msg = 'Error in return from getaspects call with -- ' + Natgrid.errorTable(self)[ier] 
                            raise ValueError, msg
                        a[i,j] = uvtemp                                            # return aspect in degrees

                s = numpy.zeros((nxo, nyo), numpy.float32)

                for i in range(nxo):
                    for j in range(nyo):
                        uvtemp, ier = natgridmodule.getslopes(i, j)
                        if ier != 0:
                            msg = 'Error in return from getslopes call with -- ' + Natgrid.errorTable(self)[ier] 
                            raise ValueError, msg
                        s[i,j] = uvtemp                                            # return slope in degrees

        else:                                                                      # double precision
            if debug == 1:
                print 'In rgrdPrimary calling natgridd'

            dataOut, ier = natgridmodule.natgridd(self.nxi, self.xi, self.yi, dataIn, self.nxo, self.nyo, self.xo, self.yo)
            if ier != 0:
                msg = 'Error in return from natgridd call with -- ' + Natgrid.errorTable(self)[ier] 
                raise ValueError, msg

            if aspectSlope != 'no':

                nxo = self.nxo
                nyo = self.nyo
                a = numpy.zeros((nxo, nyo), numpy.float64)

                for i in range(nxo):
                    for j in range(nyo):
                        uvtemp, ier = natgridmodule.getsloped(i, j)
                        if ier != 0:
                            msg = 'Error in return from getaspectd call with -- ' + Natgrid.errorTable(self)[ier] 
                            raise ValueError, msg
                        a[i,j] = uvtemp                                            # return aspect in degrees

                s = numpy.zeros((nxo, nyo), numpy.float64)

                for i in range(nxo):
                    for j in range(nyo):
                        s[i,j], ier = natgridmodule.getsloped(i, j)
                        if ier != 0:
                            msg = 'Error in return from getsloped call with -- ' + Natgrid.errorTable(self)[ier] 
                            raise ValueError, msg
                        s[i,j] = uvtemp                                     # return slope in degrees

        # is a reverse the order in the returned arrays necessary

        if (self.xreverse == 'yes') or (self.yreverse == 'yes'):
            needReverse = 'yes'
        else:
            needReverse = 'no'

        # construct the tuple for the return of what was calculated

        if aspectSlope != 'no':
            if needReverse == 'yes':
                dataOut = Natgrid.reverseData(self, dataOut)
                a = Natgrid.reverseData(self, a)
                s = Natgrid.reverseData(self, s)
        
            returnList = [dataOut]
            returnList.append(a)
            returnList.append(s)

            return tuple(returnList)
        else:

            if needReverse == 'yes':
                dataOut = Natgrid.reverseData(self, dataOut)

            return dataOut

    def rgrdSinglePoint(self, dataIn):
        """        #-------------------------------------------------------------------
        #
        #
        #-------------------------------------------------------------------------"""
        self.sdi = 0                                       # turn off calculaton of aspect and slope 

        if dataIn.dtype.char == 'f':                                                 # single precision
            if debug == 1:
                print 'In rgrdSinglePoint using single precision computation'

            natgridmodule.pntinits(self.nxi, self.xi, self.yi, dataIn)

            dataOut = numpy.zeros((self.nxo), numpy.float32)
            for i in range(self.nxo):
                dataOut[i] = natgridmodule.pnts(self.xo[i], self.yo[i])

            natgridmodule.pntend()

        else:                                                                      # double precision
            if debug == 1:
                print 'In rgrdSinglePoint using double precision computation'

            natgridmodule.pntinitd(self.nxi, self.xi, self.yi, dataIn)

            dataOut = numpy.zeros((self.nxo), numpy.float64)
            for i in range(self.nxo):
                dataOut[i] = natgridmodule.pntd(self.xo[i], self.yo[i])

            natgridmodule.pntendd()

        return dataOut 


    def reverseData(self, data):
        #------------------------------------------------------------------------------
        #                                      
        #     purpose: reverse the order of th data if outgrid submitted was not increasing
        #
        #     usage:  
        #
        #     returned: parameters
        #
        #------------------------------------------------------------------------------

        if self.xreverse == 'yes':
            data = data[::-1,:]
        if self.yreverse == 'yes':
            data = data[:, ::-1]

        return data 

    def wrapAll(self, lat, lon, data):
        #------------------------------------------------------------------------------
        #                                      
        #     purpose: Adds much wrap in longitude to the linear form of the input data 
        #
        #     usage:  
        #
        #     passed:   lat -- the latitude array
        #               lon -- the longitude arraywhich requires a large wrap for natgrid
        #               data -- the data at the associated linear set of points
        #
        #     returned: lat, lon and data differing fom th input by the wrap
        #
        #     
        #------------------------------------------------------------------------------
        if debug == 1:
            print 'entering wrapAll with array lengths: ', len(lat)

        # Make a wrapped grid and wrapped data

        lonList = list(lon)                                        # make Python lists as intermediate step
        latList = list(lat)
        dataList = list(data)

        maxlon = max(lonList)                                      # set up the wrap ranges in longitude
        minlon = min(lonList)
        distance = (maxlon - minlon)/4.                            # wrap first and last quarter of points

        minlonLow = minlon
        minlonHigh = minlon + distance
        maxlonLow = maxlon - distance
        maxlonHigh = maxlon

        for i in range(len(lonList)):                              # wrap the Python lists
            value = lonList[i] 
            if (value >= minlonLow) and (value < minlonHigh):
                lonList.append(value + 360.)
                latList.append(latList[i])
                dataList.append(dataList[i])
            elif (value > maxlonLow) and (value <= maxlonHigh):
                lonList.append(value - 360.)
                latList.append(latList[i])
                dataList.append(dataList[i])

        if self.group == 'single':                                                 # single precision
            lon = numpy.array(lonList, numpy.float32)                          # convert to numpy arrays      
            lat = numpy.array(latList, numpy.float32)
            data = numpy.array(dataList, numpy.float32)
        else:                                                                      # double precision
            lon = numpy.array(lonList, numpy.float64)                          # convert to numpy arrays      
            lat = numpy.array(latList, numpy.float64)
            data = numpy.array(dataList, numpy.float64)

        if debug == 1:
            print 'leaving wrapAll with array lengths: ', len(lat)

        return lat, lon, data
 
    #---------------------------------------------------------------------------------
    # ****************  Control parameter manipulation functions  ********************
    #---------------------------------------------------------------------------------

    def parameterNames(self):
        #------------------------------------------------------------------------------
        #                                      
        #     purpose:  produce a list of the natgrid parameters
        #
        #     usage:    parameters = parameterNames(self)
        #
        #     passed:   self
        #
        #     returned: parameters
        #
        #------------------------------------------------------------------------------

        parameters = ['name', '----', 'adf', 'alg', 'asc', 'bI', 'bJ', 'dup', 'ext', 'hor', 'igr', 'magx', 
                      'magy', 'magz', 'non', 'nul', 'rad', 'sdi', 'upd', 'ver', 'xas', 'yas', 'zas' ]

        return parameters 

    def parameterType(self):
        #--------------------------------------------------------------------------------
        #                                      
        #     purpose: produce a dictionary connecting parameter names and their data types 
        #
        #     usage:  typeDict =  parameterType(self)
        #
        #     passed:   self
        #
        #     returned: typeDict
        #
        #---------------------------------------------------------------------------------
        typeDict = { 
           'adf':'int', 'alg':'char', 'asc':'int', 'bI':'float', 'bJ':'float', 'dup':'int', 'ext':'int',
           'hor':'float', 'igr':'int', 'magx':'float', 'magy':'float', 'magz':'float', 'non':'int', 'nul':'float',
           'rad':'int', 'sdi':'int', 'upd':'int', 'ver':'float', 'xas':'float', 'yas':'float', 'zas':'float' }

        return typeDict 

    def makeDefaultParameterTable(self):
        #-----------------------------------------------------------------------------------
        #                                      
        #     purpose: construct the dictionary which is the default control parameters table
        #
        #     usage:  makeDefaultParameterTable()
        #
        #     passed:   self
        #
        #     returned: parameterDict
        #
        #----------------------------------------------------------------------------------

        parameterDict = {
  'name':('type ', ' legal values       ','  default values ','            description                                     '),
  '----':('-----', '--------------------','-----------------','------------------------------------------------------------'),
  'adf': ('int  ','0 = no or 1 = yes   ',' 0                ','produce data file of algoritmic info for display? (see alg) '),
  'alg': ('char ','any file name       ',' "nnalg.dat"      ','file name for algoritmic display tool (see adf)             '),
  'asc': ('int  ','0 = no or 1 = yes   ',' 1                ','is automatic scaling is allowed?                            '),
  'bI':  ('float','>= 1.               ',' 1.5              ','tautness increasing effect of the gradients by increasing bI'),
  'bJ':  ('float','>= 1.               ',' 7.0              ','tautness decreasing breadth of region affected by gradients '),
  'dup': ('int  ','0 = yes or 1 = no   ',' 1                ','are duplicate input coordinates are allowed?                '),
  'ext': ('int  ','0 = no or 1 = yes   ',' 1                ','is extrapolation allowed outside the convex hull?           '),
  'hor': ('float','>= 0.               ',' -1.0             ','amount of horizontal overlap from outside current region    '),
  'igr': ('int  ','0 = no or 1 = yes   ',' 0                ','are gradients are to be computed?                           '),
  'magx':('float','> 0.                ',' 1.0              ','scale factor for x coordinate values                        '),
  'magy':('float','> 0.                ',' 1.0              ','scale factor for y coordinate values                        '),
  'magz':('float','> 0.                ',' 1.0              ','scale factor for z coordinate values                        '),
  'non': ('int  ','0 = yes or 1 = no   ',' 0                ','are interpolated values are allowed to be negative?         '),
  'nul': ('float','any float           ',' 0.0              ','value for points outside the convex hull if no extrapolation'),
  'rad': ('int  ','0 = rad or 1 = deg  ',' 0                ','are slopes and aspects are returned in radians or degrees?  '),
  'sdi': ('int  ','0 = no or 1 = yes   ',' 0                ','are slopes and aspects to be computed?                      '),
  'upd': ('int  ','0=N to S or 1=S to N',' 1                ','does output array from giving N to S or S to N?             '),
  'ver': ('float','>= 0.               ',' -1.0             ','amount of vertical overlap from outside current region      '),
  'xas': ('float','> 0.                ',' 0.0              ','scale used by automatic scaling of x in last interpolation  '),
  'yas': ('float','> 0.                ',' 0.0              ','scale used by automatic scaling of y in last interpolation  '),
  'zas': ('float','> 0.                ',' 0.0              ','scale used by automatic scaling of z in last interpolation  ') }

        return parameterDict      


    def makeInstanceParameterTable(self):
        #----------------------------------------------------------------------------------
        #                                      
        #     purpose:  construct the dictionary which is the instance control parameters table
        #
        #     usage:    makeInstanceParameterTable(self)
        #
        #     passed:   self
        #
        #     returned: parameterDict
        #
        #----------------------------------------------------------------------------------

        parameterDict = {
  'name':('type ', ' legal values      ','    Values      ','            description                                     '),
  '----':('-----', '-------------------','----------------','------------------------------------------------------------'),
  'adf': ('int  ','0 = no or 1 = yes   ', eval('self.adf') ,'produce data file of algoritmic info for display? (see alg) '),
  'alg': ('char ','any file name       ', eval('self.alg') ,'file name for algoritmic display tool (see adf)             '),
  'asc': ('int  ','0 = no or 1 = yes   ', eval('self.asc') ,'is automatic scaling is allowed?                            '),
  'bI':  ('float','>= 1.               ', eval('self.bI')  ,'tautness increasing effect of the gradients by increasing bI'),
  'bJ':  ('float','>= 1.               ', eval('self.bJ')  ,'tautness decreasing breadth of region affected by gradients '),
  'dup': ('int  ','0 = yes or 1 = no   ', eval('self.dup') ,'are duplicate input coordinates are allowed?                '),
  'ext': ('int  ','0 = no or 1 = yes   ', eval('self.ext') ,'is extrapolation allowed outside the convex hull?           '),
  'hor': ('float','>= 0.               ', eval('self.hor') ,'amount of horizontal overlap from outside current region    '),
  'igr': ('int  ','0 = no or 1 = yes   ', eval('self.igr') ,'are gradients are to be computed?                           '),
  'magx':('float','> 0.                ', eval('self.magx'),'scale factor for x coordinate values                        '),
  'magy':('float','> 0.                ', eval('self.magy'),'scale factor for y coordinate values                        '),
  'magz':('float','> 0.                ', eval('self.magz'),'scale factor for z coordinate values                        '),
  'non': ('int  ','0 = yes or 1 = no   ', eval('self.non') ,'are interpolated values are allowed to be negative?         '),
  'nul': ('float','any float           ', eval('self.nul') ,'value for points outside the convex hull if no extrapolation'),
  'rad': ('int  ','0 = rad or 1 = deg  ', eval('self.rad') ,'are slopes and aspects are returned in radians or degrees?  '),
  'sdi': ('int  ','0 = no or 1 = yes   ', eval('self.sdi') ,'are slopes and aspects to be computed?                      '),
  'upd': ('int  ','0=N to S or 1=S to N', eval('self.upd') ,'does output array from giving N to S or S to N?             '),
  'ver': ('float','>= 0.               ', eval('self.ver') ,'amount of vertical overlap from outside current region      '),
  'xas': ('float','> 0.                ',' 0.0              ','scale used by automatic scaling of x in last interpolation'),
  'yas': ('float','> 0.                ',' 0.0              ','scale used by automatic scaling of y in last interpolation'),
  'zas': ('float','> 0.                ',' 0.0              ','scale used by automatic scaling of z in last interpolation') }

        return parameterDict      

    def printDefaultParameterTable(self):
        """    --------------------------------------------------------------------------------------------------------
                                               
              purpose: print the value of all the parameters
         
              usage:    r.printDefaultParameterTable()
         
                        where r is an instance of Natgrid
         
              passed:   self
         
              returned: None
         
    --------------------------------------------------------------------------------------------------------""" 
        names = Natgrid.parameterNames(self)
        names = names[2:]

        parameterDict = Natgrid.makeDefaultParameterTable(self)
        for item in names:
            items = (item, parameterDict[item][0], parameterDict[item][1], parameterDict[item][2], parameterDict[item][3])  
            print '%-7.7s  %-6.6s  %-12.12s   %-15.15s  %s' % items

        return 

    def printInstanceParameterTable(self):
        """    --------------------------------------------------------------------------------------------------------
                                               
              purpose: print the value of all the parameters
         
              usage:  r.printInstanceParameterTable()
         
                      where r is an instance of Natgrid
         
              passed:   self
         
              returned: None
         
    --------------------------------------------------------------------------------------------------------""" 
        names = Natgrid.parameterNames(self)
        names = names[2:]

        parameterDict = Natgrid.makeInstanceParameterTable(self)
        for item in names:
            items = (item, parameterDict[item][0], parameterDict[item][1], parameterDict[item][2], parameterDict[item][3])  
            print '%-7.7s  %-6.6s  %-12.12s   %-7.7s  %s' % items

        return 
    def printInstanceParameters(self):
        """    --------------------------------------------------------------------------------------------------------
                                               
              purpose: print the values of the current natgrid control parameters in c code
         
              usage:    r. printInstanceParameters()
         
                      where r is an instance of Natgrid
         
              passed:   self
         
              returned: None
         
    --------------------------------------------------------------------------------------------------------""" 

        names = Natgrid.parameterNames(self)
        names = names[2:]

        typeDict = Natgrid.parameterType(self)

        for name in names:
            if typeDict[name] == 'int':
                print 'Currently,  %s = %d' % (name, eval('self.' + name))
            elif typeDict[name] == 'char':
                print 'Currently,  %s = %s' % (name, eval('self.' + name))
            elif typeDict[name] == 'float':
                print 'Currently,  %s = %f' % (name, eval('self.' + name))
            elif typeDict[name] == 'double':
                print 'Currently,  %s = %f' % (name, eval('self.' + name))

        return None      

    def setInstanceParameters(self):
        #---------------------------------------------------------------------------
        #                                      
        #     purpose: set the instance values of the current natgrid control parameters in c code
        #
        #     usage:  r.setInstanceParameters() 
        #
        #             where r is an instance of Natgrid
        #
        #     passed:   self
        #
        #     returned: None
        #
        #----------------------------------------------------------------------------

        names = Natgrid.parameterNames(self)
        names = names[2:-3]                                                # the -3 eliminates the nonsettable xas, yas and zas

        typeDict = Natgrid.parameterType(self)

        # set the current values for the natgrid control parameters


        for name in names:
            if typeDict[name] == 'int':
                natgridmodule.seti(name, eval('self.' + name))
            elif typeDict[name] == 'char':
                natgridmodule.setc(name, eval('self.' + name))
            elif typeDict[name] == 'float':
                natgridmodule.setr(name, eval('self.' + name))
            elif typeDict[name] == 'double':
                natgridmodule.setrd(name, eval('self.' + name))

        return None      

    #---------------------------------------------------------------------------------
    # *****************************  Error Table  ************************************
    #---------------------------------------------------------------------------------
    def errorTable(self):

        """    --------------------------------------------------------------------------------------------------------
                                             
            purpose: construct the dictionary which provides access to error messages
         
            usage:  errorDict = r.errorTable()
         
                    where r is an instance of Natgrid
         
            returned: errorDict
         
    --------------------------------------------------------------------------------------------------------""" 

        errorDict = {
          1:  'Insufficient data in gridded region to triangulate',
          2:  'Dulpicate input data coordinates are not allowed',
          3:  'Unable to open file for writing algorithmic',
          4:  'WARNING: The ratio of vertical to horizontal scales too large for gradients. Rescale if gradients required',
          5:  'WARNING: The ratio of vertical to horizontal scales too small for gradients. Rescale if gradients required',
          6:  'WARNING: The ratio of x to y-axis breath too extreme. Change proportions or rescale. Gradients disabled',
          7:  'Unable to allocate storage for ivector',
          8:  'Unable to allocate storage for dvector',
          9:  'Unable to allocate storage for **imatrix',
          10: 'Unable to allocate storage for imatrix[]',
          11: 'Unable to allocate storage for **fmatrix',
          12: 'Unable to allocate storage for fmatrix[]',
          13: 'Unable to allocate storage for **dmatrix',
          14: 'Unable to allocate storage for dmatrix[]',
          15: 'Unable to allocate storage for raw data',
          16: 'Unable to allocate storage for a simplex',
          17: 'Unable to allocate storage for temp',
          18: 'Unable to allocate storage for neig',
          19: 'Slopes have not been computed, set sdip',
          20: 'Row argument out of range',
          21: 'Column argument out of range',
          22: 'Aspects have not been computed, set sdip',
          23: 'Parameter name not known',
          24: 'Can not open error file',
          25: 'Automatic scaling done - distorted aspects not returned. Rescale data or set magx, magy and magz appropriately',
          26: 'Automatic scaling done - distorted slopes not returned. Rescale data or set magx, magy and magz appropriately',
          27: 'Coordinate is outside the gridded region for a single point interpolation',
          28: 'Can not compute aspects and slopes in conjunction with single point interpolation mode',
          29: 'Fortran DOUBLE PRECISION entries not supported on UNICOS',
          30: 'Error number out of range' }

        return errorDict      

    #---------------------------------------------------------------------------------
    # ***************************  magic functions   *********************************
    #---------------------------------------------------------------------------------

    def __setattr__(self, name, value):
        #---------------------------------------------------------------------------------
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
        #---------------------------------------------------------------------------------
        typeDict = Natgrid.parameterType(self)

        if name in typeDict.keys():
            if typeDict[name] == 'int':
                natgridmodule.seti(name, value)
                self.__dict__[name] = value
            elif typeDict[name] == 'char':
                natgridmodule.setc(name, value)
                self.__dict__[name] = value
            elif typeDict[name] == 'float':
                natgridmodule.setr(name, value)
                self.__dict__[name] = value
            elif typeDict[name] == 'double':
                natgridmodule.setrd(name, value)
                self.__dict__[name] = value

        else:
            self.__dict__[name] = value

        return None

    def __getattr__(self, name):
        #---------------------------------------------------------------------------------
        #
        #   purpose:  '__getattr__' is called only if a referenced attribute can not be found
        #             in the instance. It gets the attribute from natgridmodule if possible.
        #
        #   usage:    x.name  -- name is the oject and not a string repr   
        #
        #   passed :  name 
        #
        #   returned: x.name 
        #
        #---------------------------------------------------------------------------------
        typeDict = Natgrid.parameterType(self)

        if name in typeDict.keys():
            if typeDict[name] == 'int':
                value = natgridmodule.geti(name)
            elif typeDict[name] == 'char':
                value = natgridmodule.getc(name)
            elif typeDict[name] == 'float':
                value = natgridmodule.getr(name)
            elif typeDict[name] == 'double':
                value = natgridmodule.getrd(name)

        else:
            raise AttributeError, name

        return value

    #---------------------------------------------------------------------------------
    # *******************************************************************
    # ****************  end of magic functions **************************
    # *******************************************************************
    #---------------------------------------------------------------------------------

def printParameterTable():
    """    --------------------------------------------------------------------------------------------------------
     routine:  printParameterTable
     
     purpose:  print the control parameter table using the default values from outside the Natgrid class
     
     usage:    import nat
               nat.printParameterTable()
     
     passed:   nothing 
     
     returned: None
     
     definition: printParameterTable():
--------------------------------------------------------------------------------------------------------""" 

    names = ['name', '----', 'adf', 'alg', 'asc', 'bI', 'bJ', 'dup', 'ext', 'hor', 'igr', 'magx', 
                      'magy', 'magz', 'non', 'nul', 'rad', 'sdi', 'upd', 'ver', 'xas', 'yas', 'zas' ]

    parameterDict = {
  'name':('type ', ' legal values       ','  default values ','            description                                     '),
  '----':('-----', '--------------------','-----------------','------------------------------------------------------------'),
  'adf': ('int  ','0 = no or 1 = yes   ',' 0                ','produce data file of algoritmic info for display? (see alg) '),
  'alg': ('char ','any file name       ',' "nnalg.dat"      ','file name for algoritmic display tool (see adf)             '),
  'asc': ('int  ','0 = no or 1 = yes   ',' 1                ','is automatic scaling is allowed?                            '),
  'bI':  ('float','>= 1.               ',' 1.5              ','tautness increasing effect of the gradients by increasing bI'),
  'bJ':  ('float','>= 1.               ',' 7.0              ','tautness decreasing breadth of region affected by gradients '),
  'dup': ('int  ','0 = yes or 1 = no   ',' 1                ','are duplicate input coordinates are allowed?                '),
  'ext': ('int  ','0 = no or 1 = yes   ',' 1                ','is extrapolation allowed outside the convex hull?           '),
  'hor': ('float','>= 0.               ',' -1.0             ','amount of horizontal overlap from outside current region    '),
  'igr': ('int  ','0 = no or 1 = yes   ',' 0                ','are gradients are to be computed?                           '),
  'magx':('float','> 0.                ',' 1.0              ','scale factor for x coordinate values                        '),
  'magy':('float','> 0.                ',' 1.0              ','scale factor for y coordinate values                        '),
  'magz':('float','> 0.                ',' 1.0              ','scale factor for z coordinate values                        '),
  'non': ('int  ','0 = yes or 1 = no   ',' 0                ','are interpolated values are allowed to be negative?         '),
  'nul': ('float','any float           ',' 0.0              ','value for points outside the convex hull if no extrapolation'),
  'rad': ('int  ','0 = rad or 1 = deg  ',' 0                ','are slopes and aspects are returned in radians or degrees?  '),
  'sdi': ('int  ','0 = no or 1 = yes   ',' 0                ','are slopes and aspects to be computed?                      '),
  'upd': ('int  ','0=N to S or 1=S to N',' 1                ','does output array from giving N to S or S to N?             '),
  'ver': ('float','>= 0.               ',' -1.0             ','amount of vertical overlap from outside current region      '),
  'xas': ('float','> 0.                ',' 0.0              ','scale used by automatic scaling of x in last interpolation  '),
  'yas': ('float','> 0.                ',' 0.0              ','scale used by automatic scaling of y in last interpolation  '),
  'zas': ('float','> 0.                ',' 0.0              ','scale used by automatic scaling of z in last interpolation  ') }

    for item in names:
        items = (item, parameterDict[item][0], parameterDict[item][1], parameterDict[item][2], parameterDict[item][3])  
        print '%-7.7s  %-6.6s  %-12.12s   %-15.15s  %s' % items

    return 

def printStoredParameters():
    """    --------------------------------------------------------------------------------------------------------
     routine:  printStoredParameters
     
     purpose: print the values of the current natgrid control parameters in c code. The call
                   to the method function rgrd will change them to the instance values.
     
     usage:    import nat
               nat.printStoredParameters() 
     
     passed:   nothing 
     
     returned: None
     
     definition: printStoredParameters():
--------------------------------------------------------------------------------------------------------""" 

    names = ['name', '----', 'adf', 'alg', 'asc', 'bI', 'bJ', 'dup', 'ext', 'hor', 'igr', 'magx', 
                      'magy', 'magz', 'non', 'nul', 'rad', 'sdi', 'upd', 'ver', 'xas', 'yas', 'zas' ]
    names = names[2:]

    typeDict = { 
           'adf':'int', 'alg':'char', 'asc':'int', 'bI':'float', 'bJ':'float', 'dup':'int', 'ext':'int',
           'hor':'float', 'igr':'int', 'magx':'float', 'magy':'float', 'magz':'float', 'non':'int', 'nul':'float',
           'rad':'int', 'sdi':'int', 'upd':'int', 'ver':'float', 'xas':'float', 'yas':'float', 'zas':'float' }

    for item in names:
        if typeDict[item] == 'int':
            print '   %s = %d' % (item, natgridmodule.geti(item))
        elif typeDict[item] == 'char':
            print '   %s = %s' % (item, natgridmodule.getc(item))
        elif typeDict[item] == 'float':
            print '   %s = %f' % (item, natgridmodule.getr(item))
        elif typeDict[item] == 'double':
            print '   %s = %f' % (item, natgridmodule.getrd(item))

    return None      


def checkdim(x, y):
    #------------------------------------------------------------------------------------------
    #                                      
    #     purpose: determine whether the coordinate grid is random or monotonically increasing
    #
    #     usage:  
    #
    #     returned: x, y, monotonic, xreverse, yreverse
    #
    #-------------------------------------------------------------------------------------------
    xsize = len(x)

    if x[0] > x[xsize - 1]:
        x = x[::-1]
        xreverse = 'yes'
    else:
        xreverse = 'no'


    xmonotonic = 'yes'                                  # monotonic and possibly reversed to make it montonically increasing
    for n in range(1, xsize):
        if x[n] < x[n - 1]:
            xmonotonic = 'no'                           # not monotonic so return the original grid

    ysize = len(y)

    if y[0] > y[ysize - 1]:
        y = y[::-1]
        yreverse = 'yes'
    else:
        yreverse = 'no'


    ymonotonic = 'yes'                                  # monotonic and possibly reversed to make it montonically increasing
    for n in range(1, ysize):
        if y[n] < y[n - 1]:
            ymonotonic = 'no'                           # not monotonic so return the original grid

    if xmonotonic == 'yes' and ymonotonic == 'yes':      # if both are monotonic the grid is monotonic
        monotonic = 'yes'
    else:
        monotonic = 'no'
        if xreverse == 'yes':                           # return vectors to thier original state
            x = x[::-1]
            xreverse = 'no'
        if yreverse == 'yes':
            y = y[::-1]
            yreverse = 'no'

    # note that x and y may be returned reversed as necessary only if monotonic is set to yes

    return x, y, monotonic, xreverse, yreverse

#---------------------------------------------------------------------------------
# ********************************************************************************
# ********************************************************************************
#---------------------------------------------------------------------------------

 

def sendOutput(output, msg, value = None):
    """        #---------------------------------------------------------------------------------
    #
    #    purpose: send the same message to the screen and to a file
    # 
    #    passed :  msg - the string
    #             
    #    returned: return
    #
    #---------------------------------------------------------------------------------"""
    if value is None:
        print msg
        output.write(msg + '\n')
    else:
        print msg, `value`
        output.write(msg + ' %15.11e\n' % (value,))

    return None

def document():
    """        #-------------------------------------------------------------------------
    #
    #    purpose:   'docstrings' writes the doc strings contained in the regrid module
    #                to a file as documentation for the user
    #
    #    usage:     import regrid2 as regrid
    #               regrid.document()   
    #    
    #    passed :   nothing
    #
    #    returned:  nothing
    #
    #-------------------------------------------------------------------------"""
    import nat

    std = sys.stdout                                             # save sys.stout to allow reassigning later
    sys.stdout = open( 'natgrid.doc', 'w')

    print '**********************************************************************************************\n'  
    print '**************************** Overview of the CDAT interface to natgrid ***********************\n'
    print '**********************************************************************************************\n'  
    print nat.__doc__
    print
    print

    print '    ******************** Instructions for use of the natgrids function **************************'
    print natgridmodule.natgrids.__doc__
    print

    print '    ******************** Instructions for use of the seti function **************************'
    print natgridmodule.seti.__doc__
    print

    print '    ******************** Instructions for use of the geti function **************************'
    print natgridmodule.geti.__doc__
    print

    print '    ******************** Instructions for use of the setr function **************************'
    print natgridmodule.setr.__doc__
    print

    print '    ******************** Instructions for use of the getr function **************************'
    print natgridmodule.getr.__doc__
    print

    print '    ******************** Instructions for use of the setc function **************************'
    print natgridmodule.setc.__doc__
    print

    print '    ******************** Instructions for use of the getc function **************************'
    print natgridmodule.getc.__doc__
    print

    print '    ******************** Instructions for use of the getaspects function **************************'
    print natgridmodule.getaspects.__doc__
    print

    print '    ******************** Instructions for use of the getslopes function **************************'
    print natgridmodule.getslopes.__doc__
    print

    print '    ******************** Instructions for use of the pntinits function **************************'
    print natgridmodule.pntinits.__doc__
    print

    print '    ******************** Instructions for use of the pnts function **************************'
    print natgridmodule.pnts.__doc__
    print

    print '    ******************** Instructions for use of the pntend function **************************'
    print natgridmodule.pntend.__doc__
    print

    print '    ******************** Instructions for use of the natgridd function **************************'
    print natgridmodule.natgridd.__doc__
    print

    print '    ******************** Instructions for use of the setrd function **************************'
    print natgridmodule.setrd.__doc__
    print

    print '    ******************** Instructions for use of the getrd function **************************'
    print natgridmodule.getrd.__doc__
    print

    print '    ******************** Instructions for use of the getaspectd function **************************'
    print natgridmodule.getaspectd.__doc__
    print

    print '    ******************** Instructions for use of the getsloped function **************************'
    print natgridmodule.getsloped.__doc__
    print

    print '    ******************** Instructions for use of the pntinitd function **************************'
    print natgridmodule.pntinitd.__doc__
    print

    print '    ******************** Instructions for use of the pntd function **************************'
    print natgridmodule.pntd.__doc__
    print

    print '    ******************** Instructions for use of the pntendd function **************************'
    print natgridmodule.pntendd.__doc__
    print



    sys.stdout = std

    return None

def sendmsg(msg, value1 = None, value2 = None):
    """        #---------------------------------------------------------------------------------
    #
    #    purpose: send the same message to the screen 
    # 
    #    passed :  msg - the string
    #              value - the number associated with the string
    #             
    #    returned: return
    #
    #---------------------------------------------------------------------------------"""

    print '*******************************************************************'
    if value1 is None:
        print msg
    elif value2 is None:
        print msg, value1
    else:
        print msg, value1, value2
    print '*******************************************************************'

    return None


def help(choice = None):
    import nat

    if choice is None:                               # get instructions for use of help
        print """     ----------------------------------------------------------------------------------------
      
      INSTRUCTIONS ON USE THE OBJECT ORIENTED INTERFACE TO THE NATGRID PACKAGE FROM NGMATH 
     
      This module is built as one class, Natgrid, which sports a single method called rgrd.
     
      To get instructions on making an instance of Natgrid, type
     
              nat.help('Natgrid')
     
      To get instructions on using the control parameters, type
     
              nat.help('parameters')
     
      To print the table describing the control parameters, type
     
              nat.help('table')
     
      To get instructions on performing a regridding, type
     
              nat.help('regrid')
      
      To get instructions on calculating slopes and aspects, type
     
              nat.help('aspectSlope')
      
      To get instructions using the single point computational mode, type
     
              nat.help('singlePoint')
      
     
      INSTRUCTIONS ON USE OF ORIGINAL NATGRID PACKAGE FROM NGMATH 
     
      This module is built as an interface to natgridmodule.so which exports the following functions:
                                                                                                    
                                                                                                    
                        Single precision procedures:                                                    
                                                                                                    
                            natgrids     - primary function for gridding.                         
                            seti         - set int parameter values.                                
                            geti         - retrieve values for int parameters.                      
                            setr         - set float parameter values.                              
                            getr         - retrieve values for float parameters                     
                            setc         - set char parameter values.                                
                            getc         - retrieve values for char parameters.                     
                            getaspects   - get aspect values, if calculated.                        
                            getslopes    - get slope values, if calculated.                         
                            pntinits     - initiate single point mode.                              
                            pnts         - interpolate at a single point.                           
                            pntend       _ terminate single point mode.                             
                                                                                                    
                                                                                                    
                        Double precision procedures:                                                    
                                                                                                    
                            natgridd       - primary function for gridding.                         
                            setrd        - set float parameter values.                              
                            getrd        - retrieve values for float parameters                     
                            getaspectd   - get aspect values, if calculated.                        
                            getsloped    - get slope values, if calculated.                         
                            pntinitd     - initiate single point mode.                              
                            pntd         - interpolate at a single point.                           
                            pntendd      _ terminate single point mode.                             
                                                                                                    
                                                                                                    
          It is feasible to use these functions directly without this module. Information is available
          through their docstrings. For example, to get the docstring for the routine natgrids, follow this 
          procedure at the Python prompt:
     
                import natgridmodule
     
                print natgridmodule.natgrids.__doc__
            
                or simply type
     
                nat.help('natgrids')
 
     ------------------------------------------------------------------------------------------------------"""

    elif choice == 'Natgrid': 
        print """     ----------------------------------------------------------------------------------------
                                           
            To make an instance, r, type:
 
                import nat
 
                r = nat.Natgrid(xi, yi, xo, yo)
                or 
                r = nat.Natgrid(xi, yi, xo, yo, listOutput = 'yes')
 
                    where xi, yi and xo, yo are the input and output grid coordinate arrays. The optional listOutput is
                    set to anything except 'no' if xo, yo are in list format as explained below.

                The input grid must be organized in a list format always. The size of the xi array and the yi array are 
                necessarily equal. For example, if there are n randomly spaced input data points, there
                are n values in xi and n values in yi. 

                There are two possible formats for the output grid. The output grid coordinate arrays may be a list like 
                the input array or it may be a rectangular grid.  The choice between the two posibilities is made according
                to requirements in subseqent calls to the method function. The first choice is required if the subsequent
                call is to the single point mode interpolation. The list can have one or more points. Of course, the list
                could describe a rectangular grid. For example, a rectangular grid with 10 x values and 20 y values can be
                rewrtten in list form with 200 x value and 200 y values. However, this form requires calling the slower 
                single point interpolator. The second choice is most efficient for the basic interpolation to a rectangular
                output grid. The output grid must be monotonic but need not be equally spced.
                 
                The grid coordinate arrays can be single precision (numpy.float32) or double precision (numpy.float64). The
                decision on whether to call for a single or a double precision computation subsequently is made by looking at
                the type of these arrays. 

     --------------------------------------------------------------------------------------------------------------------"""
 

    elif choice == 'parameters': 
        print """     ----------------------------------------------------------------------------------------
                                           
             In the absence of an instance of the class Natgrid, a description of the control parameters can be found 
             by typing
     
               import nat
               nat.printParameterTable()
     
     
            The control parameters are easily available within the class. First make an instance, r, type:
     
                import nat
 
                r = nat.Natgrid(xi, yi, xo, yo)

 
            To change a setting type the new value. For example, to set igr to 1, type
 
                r.igr = 1
 
            To find an individual value,  type the name. For example, to exam the value of hor, type
 
                r.hor
     
            To check the settings type
     
                r.printInstanceParameterTable() -- prints the table with values and a description of the parameters
                                                   used in subsequent calls to the method function rgrd
                or

                r.printInstanceParameters()     -- prints a list of the parameters values used in subsequent calls to the 
                                                   the rgrd method

                nat. printStoredParameters()    -- prints the parameters in memory which may differ from the above if the
                                                   user has made more than one instance of the Natgrid class.
     --------------------------------------------------------------------------------------------------------------------"""

    elif choice == 'table': 
        printParameterTable()
     
    #-----------------------------------------------------------------------------------------------------

    elif choice == 'regrid':  
        print """     ----------------------------------------------------------------------------------------

            natgrid is restricted to two dimensions . Consequently, it is the user's responsibility to reduce the processing 
            of higher dimensional data to a sequence of calls using only two dimensional data. A description of the basic
            natural neighbor linear interpolation and nonlinear interpolations follow.

                Make an instance, r, with:
 
                     import nat
 
                     r = nat.Natgrid(xi, yi, xo, yo)

                     where the xo, yo grid is rectilinear as explained in the help choice 'Natgrid'.
 
                     r.igr = 1  -- in order to set up the computation for nonlinear interpolation. The default value
                                   for igr calls for a linear interpolation.
 
                Then call the primary interpolation computation to regrid the input data, dataIn, on the grid (xi, yi) to
                the output data, dataOut, on the grid (xo, yo), with
 
                    dataOut = r.rgrd( dataIn )

                When dealing with global data described on a latitude-longitude grid, it is also possible to request a wrap
                in the input grid and the input data in the longitude direction, assumed to be the yi grid coordinate, with

                     dataOut = r.rgrd(dataIn, wrap = 'yes')
 
                The computation is either single or double precision as determined by the precision submitted in making
                the instance.
     --------------------------------------------------------------------------------------------------------------------"""

    elif choice == 'aspectSlope':  
        print """     ----------------------------------------------------------------------------------------

            natgrid is restricted to two dimensions . Consequently, it is the user's responsibility to reduce the processing 
            of higher dimensional data to a sequence of calls using only two dimensional data. A description of the basic
            natural neighbor linear and nonlinear interpolations returning the aspect and the slope at the output grid
            points follows.

                First make an instance, r, with:
 
                     import nat
 
                     r = nat.Natgrid(xi, yi, xo, yo)

                     where the xo, yo grid is rectilinear as explained in the help choice 'Natgrid'.
 
                     r.igr = 1  -- in order to set up the computation for nonlinear interpolation. The default value
                                   for igr calls for a linear interpolation.
 
                Then call the primary interpolation computation to regrid the input data, dataIn, on the grid (xi, yi) to
                the output data, dataOut, on the grid (xo, yo), while asking for the aspect and the slope on this output grid, with
 
                     dataOut, a, s = r.rgrd( dataIn, aspectSlope = 'yes' )
 
                     where a is the aspect, the direction of the steepest descent in degrees measured from 'north' and s is the
                     slope in degrees measured from the horizontal. Necessarily, these are arrays aligned with the rectilinear
                     output grid, xo, yo.

                It is also possible to request a wrap in the input grid and the input data in the longitude direction, assumed
                to be the yi grid coordinate, by adding a keyword as 
 
                     dataOut, a, s = r.rgrd( dataIn, aspectSlope = 'yes', wrap = 'yes' )
 
                The computation is either single or double precision as determined by the precision submitted in making
                the instance.

     --------------------------------------------------------------------------------------------------------------------"""

    elif choice == 'singlePoint':  
        print """     ----------------------------------------------------------------------------------------
 
            natgrid is restricted to two dimensions . Consequently, it is the user's responsibility to reduce the processing 
            of higher dimensional data to a sequence of calls using only two dimensional data. A description of the single
            point natural neighbor linear and nonlinear interpolations follows.  

                First make an instance, r, with:
 
                     import nat
 
                     r = nat.Natgrid(xi, yi, xo, yo, listOutput)

                     where the xo, yo output grid is in the list  form (not a rectangular output grid) as explained
                     in the help choice 'Natgrid'.
 
                     r.igr = 1  -- in order to set up the computation for nonlinear interpolation. The default value
                                   for igr calls for a linear interpolation.

                Then  call the single point mode interpolation computation to regrid the input data, dataIn, on the grid (xi, yi)
                to the output data, dataOut, on the grid (xo, yo), type
 
                    dataOut = r.rgrd( dataIn )

                The single point mode is slow but it provides a choice where the interpolation is to one or more points
                rather than to a complete rectangular grid..
 
                The computation is either single or double precision as determined by the precision submitted in making
                the instance.
     
     --------------------------------------------------------------------------------------------------------------------"""

    elif choice == 'natgrids':
        print natgridmodule.natgrids.__doc__
    elif choice == 'seti':
        print natgridmodule.seti.__doc__
    elif choice == 'geti':
        print natgridmodule.geti.__doc__
    elif choice == 'setr':
        print natgridmodule.setr.__doc__
    elif choice == 'getr':
        print natgridmodule.getr.__doc__
    elif choice == 'setc':
        print natgridmodule.setc.__doc__
    elif choice == 'getc':
        print natgridmodule.getc.__doc__
    elif choice == 'getaspects':
        print natgridmodule.getaspects.__doc__
    elif choice == 'getslopes':
        print natgridmodule.getslopes.__doc__
    elif choice == 'pntinits':
        print natgridmodule.pntinits.__doc__
    elif choice == 'pnts':
        print natgridmodule.pnts.__doc__
    elif choice == 'pntend':
        print natgridmodule.pntend.__doc__
    elif choice == 'natgridd':
        print natgridmodule.natgridd.__doc__
    elif choice == 'setrd':
        print natgridmodule.setrd.__doc__
    elif choice == 'getrd':
        print natgridmodule.getrd.__doc__
    elif choice == 'getaspectd':
        print natgridmodule.getaspectd.__doc__
    elif choice == 'getsloped':
        print natgridmodule.getsloped.__doc__
    elif choice == 'pntinitd':
        print natgridmodule.pntinitd.__doc__
    elif choice == 'pntd':
        print natgridmodule.pntd.__doc__
    elif choice == 'pntendd':
        print natgridmodule.pntendd.__doc__

    else:
        print 'Your request is not in help.   The help choices are: ' 
        print 'Natgrid, parameters, table, regrid, aspectSlope, singlePoint, natgrids, seti, geti, setr, getr, setc, getc, getaspects, getslopes, pntinits, pnts, pntend, natgridd, setrd, getrd, getaspectd, getsloped, pntinitd, pntd, pntendd'                               

    return None

