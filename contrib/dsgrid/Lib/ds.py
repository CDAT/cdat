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
  
   DSGRID PACKAGE
  
        dsgrid implements a simple weighted average interpolation algorithm. The input for the interpolation is a set
        of randomly spaced two or three-dimensional coordinates with functional values at those coordinates; the output
        is a set of interpolated values at the user specified coordinates. The coordinates may be in a list or a gridded
        format. The coordinates in the output grid must be monotonically increasing in each coordinate direction, but need
        not be evenly spaced.
  
        dsgrid uses a simple weighted average method to do its interpolation, the value of the weights being inversely 
        proportional to the distance form an input coordinate to the point where the interpolated value is desired. As
        the default, the influence varies as the the inverse cube of the distance. The sum of the weights is normalized
        to unity and an interpolated value is the sum of the the products of the known functional values and the
        calculated weights. The capability exits to vary the power of the distances used to compute the weights. For 
        powers greater than one, the interpolated surface is flat at the input data: the size of the spot increases with
        increasing values of the power. If the power of the distances is one, the weights are computed as the linear
        inverse distances and the areas around input data are cone shaped. If the power of the distances is less than one,
        the areas around input data form a cusp at the data points.
  
 
   DSGRID CONTENTS
  
        Access through Python to the dsgrid package from NCAR's ngmath distribution is provided directly through the module
        dsgridmodule.so which was generated as a Python C language extension in order to export the functions from the
        original C language library
 
   REQUIRED FILE
 
        dsgridmodule.so -- the Python interface to the ngmath dsgrid package.
 
   USEFUL FILES
 
        ds.py         -- the object oriented interface including a general help package.
        dsgridtest.py -- the code to test ds.py and to write documentation.

   USAGE
 
        This module is designed to use in two ways. One is through the use of the object oriented interface to the underlying
        functions. This approach is recommended for users not already familiar with the original dsgrid distribution because
        it simplifies the calls to the routines. The other method uses the original functions calling them directly from Python. 
 
                    --------------------------------------------------------------
                    -------------------  OBJECT ORIENTED APPROACH ----------------
 
            The ds module contains the Dsgrid class and the single method rgrd
 
            STEP 1. 

            Rectangular Gridded Output Coordinates

                To make an instance, r, type:
 
                    import ds

                    r = ds.Dsgrid(xi, xo, yi, yo)

                    or

                    r = ds.Dsgrid(xi, xo, yi, yo, zi, zo)
        
                       where xi, yi and zi are the input coordinate arrays in list format
                       while xo, yo and zo are the and output grid coordinate arrays which
                       must be increasing but not necessarily evenly spaced.
        
            List Format Output Coordinates

                To make an instance, r, type:
 
                    import ds

                    r = ds.Dsgrid(xi, xo, yi, yo, griddedOutput = 'no')

                    or

                    r = ds.Dsgrid(xi, xo, yi, yo, zi, zo, griddedOutput = 'no')
        
                       where xi, yi and zi are the input coordinate arrays in list format
                       and xo, yo and zo are the output coordinate arrays in list format.
         
            DSGRID has regridding functions which carry out their respective interpolations in single
            or double precision. The choice is determined by the type of the coordinate arrays submitted
            in makking the instance. 

            STEP 2. 

                Gridded output or list output is accessed with the same call to the method function, rgrd.
                The choice is made in the creation of an instance of the Dsgrid class in STEP 1.

                Type
                    dataOut = r.rgrd(dataIn) where

                        dataIn  -- input data in list form 
                        dataOut -- output data in gridded or list form


                    --------------------------------------------------------------
                    -------------------  ORIGINAL FUNCTION APPROACH -----------------

            The module dsgridmodule.so exports the following functions to Python from the original C library:
        
                    Single precision procedures:                                                    
                                                                                                
                        grid2s       - primary function for gridding 2D data.                   
                        grid3s       - primary function for gridding 3D data.                   
                        seti         - set int parameter values.                                
                        geti         - retrieve values for int parameters.                      
                        setr         - set float parameter values.                              
                        getr         - retrieve values for float parameters                     
                        setc         - set char parameter values.                                
                        getc         - retrieve values for char parameters.                     
                        pnt2s        - interpolate 2D data at specified individual points.      
                        pnt3s        - interpolate 3D data at specified individual points.      
                                                                                                
                                                                                                
                    Double precision procedures:                                                    
                                                                                                
                        grid2d       - primary function for gridding 2D data.                   
                        grid3d       - primary function for gridding 3D data.                   
                        setrd        - set float parameter values.                              
                        getrd        - retrieve values for float parameters                     
                        pnt2d        - interpolate 2D data at specified individual points.      
                        pnt3d        - interpolate 3D data at specified individual points.      
                                                                                                
            Information on the use of the routines is available by importing dsgridmodule and printing the docstring
            of interest. For example, documentation for the routine grid2s is obtained by typing
 
                import dsgridmodule

                print dsgridmodule.grid2s.__doc__
 
            This same information is available in the help package.
 
                import ds

                print ds.help('grid2s')
 
            A description of the control parameters is not in the dsgridmodule documentation. It can be found by typing
 
                import ds
                ds.printParameterTable()
  
        A hard copy of the full documentation on the use of each of the routines is written to the file dsgrid.doc after
        importing  ds by typing 
  
               ds.document() 
 
   DOCUMENTATION
  
        Documentation is provided through Python's docstrings, essentially Python style program comments. A help package
        provides instructions on the use of dsgrid. A table of contents is printed to the screen by typing
  
                ds.help()
  
        after importing ds.
  
        A hard copy of this documentation is written to the file dsgridmodule.doc after import dsgridtest by typing 
  
               dsgridtest.document() 
  
   TESTING 
 
       To run a test of the 2D and 3D interpolations and to get a copy of this documentation, type
                       
           cdat dsgridtest.py
 
--------------------------------------------------------------------------------------------------------------"""
import string, sys, numpy, cdms2, dsgridmodule

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

class Dsgrid:

    #---------------------------------------------------------------------------------
    # 
    #       Contents of Dsgrid class
    #           __init__
    #           rgrd     
    #                
    #---------------------------------------------------------------------------------

    def __init__(self, xi, xo, yi, yo, zi = None, zo = None, griddedOutput = 'yes'): 
        """    --------------------------------------------------------------------------------------------------------
             ROUTINE:  __init__
         
             PURPOSE:  init makes an instance of the Dsgrid class while performing the following:
         
                           checks the argument list for the correct types.
                           selects single or double precision computation
                           assigns the coordinate grid arrays to self data
                           assigns default control paramter values from the parameter dictionary
         
             USAGE: 

                 Rectangular Gridded Output Coordinates

                     To make an instance, r, type:
 
                         import ds

                         r = ds.Dsgrid(xi, xo, yi, yo)

                         or

                         r = ds.Dsgrid(xi, xo, yi, yo, zi, zo)
        
                            where xi, yi and zi are the input coordinate arrays in list format
                            while xo, yo and zo are the and output grid coordinate arrays which
                            must be increasing but not necessarily evenly spaced.
        
                 List Format Output Coordinates

                     To make an instance, r, type:
 
                         import ds

                         r = ds.Dsgrid(xi, xo, yi, yo, griddedOutput = 'no')

                         or

                         r = ds.Dsgrid(xi, xo, yi, yo, zi, zo, griddedOutput = 'no')
        
                            where xi, yi and zi are the input coordinate arrays in list format
                            and xo, yo and zo are the output coordinate arrays in list format.
         
                 DSGRID has regridding functions which carry out their respective interpolations in single
                 or double precision. The choice is determined by the type of the coordinate arrays submitted
                 in makking the instance. 
         
             DEFINITION: __init__(self, xi, xo, yi, yo, zi = None, zo = None) 
         
    --------------------------------------------------------------------------------------------------------""" 
        self.griddedOutput = griddedOutput

        # set flag for 2D or 3D computation

        if zi  is None and zo  is None:
            self.is3D = 'no'
        elif zi is not None and zo is not None:
            self.is3D = 'yes'
        elif zi is not None and zo  is None:
            msg = 'CANNOT CREATE INSTANCE - The z coordinate must have an input and an output array'
            raise ValueError, msg
        elif zi  is None and zo is not None:
            msg = 'CANNOT CREATE INSTANCE - The z coordinate must have an input and an output array'
            raise ValueError, msg
        
        # make sure that the arrays are numpy arrays

        if usefilled == 'yes':
            xi = numpy.ma.filled(xi)
            xo = numpy.ma.filled(xo)
            yi = numpy.ma.filled(yi)
            yo = numpy.ma.filled(yo)
            if self.is3D == 'yes':
                zi = numpy.ma.filled(zi)
                zo = numpy.ma.filled(zo)

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

        if self.is3D == 'yes':

            if zi.dtype.char == 'f':
                numberSingles = numberSingles + 1
            else:
                numberDoubles = numberDoubles + 1
            if zo.dtype.char == 'f':
                numberSingles = numberSingles + 1
            else:
                numberDoubles = numberDoubles + 1
        
        if numberSingles >= numberDoubles:
            self.group = 'single' 
            if numberSingles < 4:
                sendmsg('Changing all the coordinate grid types to float32')
                xi = xi.astype(numpy.float32)
                xo = xo.astype(numpy.float32)
                yi = yi.astype(numpy.float32)
                yo = yo.astype(numpy.float32)
                if self.is3D == 'yes':
                    zi = zi.astype(numpy.float32)
                    zo = zo.astype(numpy.float32)
        else:
            self.group = 'double' 
            if numberDoubles < 4:
                sendmsg('Changing all the coordinate grid types to float64')
                xi = xi.astype(numpy.float64)
                yi = yi.astype(numpy.float64)
                xo = xo.astype(numpy.float64)
                yo = yo.astype(numpy.float64)
                if self.is3D == 'yes':
                    zi = zi.astype(numpy.float64)
                    zo = zo.astype(numpy.float64)
        
        # check the argument list
        try: 
            size = len(xi)
        except:                        
            msg = 'CANNOT CREATE INSTANCE - The first argument must be an array'
            raise TypeError, msg
        if size < 3:
            msg = 'CANNOT CREATE INSTANCE - The length of the input x coordindate grid must be greater than 2'
            raise ValueError, msg

        try:
            size = len(xo)
        except:                        
            msg = 'CANNOT CREATE INSTANCE - The second argument must be an array'
            raise TypeError, msg

        try: 
            size = len(yi)
        except:                        
            msg = 'CANNOT CREATE INSTANCE - The third argument must be an array'
            raise TypeError, msg
        if size < 3:
            msg = 'CANNOT CREATE INSTANCE - The length of the input y coordindate grid must be greater than 2'
            raise ValueError, msg

        try: 
            size = len(yo)
        except:                        
            msg = 'CANNOT CREATE INSTANCE - The fourth argument must be an array'
            raise TypeError, msg

        if self.is3D == 'yes':
            try: 
                size = len(zi)
            except:                        
                msg = 'CANNOT CREATE INSTANCE - The third argument must be an array'
                raise TypeError, msg
            if size < 3:
                msg = 'CANNOT CREATE INSTANCE - The length of the input z coordindate grid must be greater than 2'
                raise ValueError, msg

            try: 
                size = len(zo)
            except:                        
                msg = 'CANNOT CREATE INSTANCE - The fourth argument must be an array'
                raise TypeError, msg

        # set the self data for the input grid

        self.nxi = len(xi)
        self.nyi = len(yi)
        if self.nxi != self.nyi:
            msg = 'CANNOT CREATE INSTANCE - The input coordinates must all have the same length'
            raise ValueError, msg
        self.xi = xi
        self.yi = yi
        if self.is3D == 'yes':
            self.nzi = len(zi)
            self.zi = zi
            if self.nzi != self.nxi:
                msg = 'CANNOT CREATE INSTANCE - The input coordinates must all have the same length'
                raise ValueError, msg

        # set the self data for the input grid

        self.nxo = len(xo)
        self.nyo = len(yo)

        if griddedOutput == 'yes':           # gridded output

            if self.is3D == 'yes':                                                            # 3D gridded output
                self.nzo = len(zo)
                self.xo, self.yo, self.zo, self.monotonic, self.xreverse, self.yreverse, self.zreverse = checkdim(xo, yo, zo) 

            else:                                                                             # 2D gridded output
                self.xo, self.yo, self.monotonic, self.xreverse, self.yreverse = checkdim(xo, yo)

            if self.monotonic == 'no':
                msg = 'CANNOT CREATE INSTANCE - The gridded output coordinates must be arranged monotonically'
                raise ValueError, msg


        else:                                # listed output
            if self.nxo != self.nyo:
                msg = 'CANNOT CREATE INSTANCE - The listed output coordinates must all have the same length'
                raise ValueError, msg
            self.xo = xo
            self.yo = yo
            self.monotonic = None
            self.xreverse = None
            self.yreverse = None
            self.zreverse = None

            if self.is3D == 'yes':                                                            # 3D listed output
                self.nzo = len(zo)
                if self.nzo != self.nyo:
                    msg = 'CANNOT CREATE INSTANCE - The listed output coordinates must all have the same length'
                    raise ValueError, msg
                self.zo= zo


        # set the parameter instance data to the default values

        defaultDict = Dsgrid.makeDefaultParameterTable(self)

        self.dmv  = eval(defaultDict['dmv'][2])
        self.dmx  = eval(defaultDict['dmx'][2])
        self.erf  = eval(defaultDict['erf'][2])
        self.exp  = eval(defaultDict['exp'][2])
        self.shd  = eval(defaultDict['shd'][2])

    def rgrd(self, dataIn):
        """    --------------------------------------------------------------------------------------------------------
             ROUTINE:  rgrd
         
             PURPOSE:  rgrd calls the primary regridders to regrid to a ractangular grid or it calls  the single
                       point regridders to accomodate a list format output coordinate grid
         
             USAGE: 

                     After making an instance, r , of the Dsgrid class
 

                         dataOut = r.rgrd(dataIn)

                            where dataIn is the randomly spaced input data which is interpolated to dataOut, the result
                            on the output coordinate grid which may be rectangular or a list.
         
             DEFINITION: rgrd(self, dataIn)

    --------------------------------------------------------------------------------------------------------""" 
        # check the input data

        if dataIn.dtype.char == 'f':                                                 # single precision
            if self.group == 'double':                                               # change the grid type to match dataIn
                print 'converting grid to single to match data'
                self.group = 'single'                                               # change the grid type to match dataIn
                self.xi = self.xi.astype(numpy.float32)
                self.xo = self.xo.astype(numpy.float32)
                self.yi = self.yi.astype(numpy.float32)
                self.yo = self.yo.astype(numpy.float32)
                if self.is3D == 'yes':
                    self.zi = self.zi.astype(numpy.float32)
                    self.zo = self.zo.astype(numpy.float32)


        else:                                                                      # double precision
            if self.group == 'single':                                              # change the grid type to match dataIn
                print 'converting grid to double to match data'
                self.group = 'double'                                               # change the grid type to match dataIn
                self.xi = self.xi.astype(numpy.float64)
                self.xo = self.xo.astype(numpy.float64)
                self.yi = self.yi.astype(numpy.float64)
                self.yo = self.yo.astype(numpy.float64)
                if self.is3D == 'yes':
                    self.zi = self.zi.astype(numpy.float64)
                    self.zo = self.zo.astype(numpy.float64)

        if self.nxi != len(dataIn):
            msg = 'CANNOT INTERPOLATE - The length of the both input grids and the length of data must agree'
            raise ValueError, msg

        if usefilled == 'yes':
            dataIn = numpy.ma.filled(dataIn)

        # set the instance values of the parameters in the c code
        Dsgrid.setInstanceParameters(self)

        # select the rgrd method 

        if self.griddedOutput == 'yes': 
            dataOut = Dsgrid.rgrdPrimary(self, dataIn)
        else:
            dataOut = Dsgrid.rgrdSinglePoint(self, dataIn)

        return dataOut

    def rgrdPrimary(self, dataIn):
        #-------------------------------------------------------------------
        #
        #
        #--------------------------------------------------------------------

        if self.group == 'single':                                                                       # single precision
            if self.is3D == 'yes':
                if debug == 1:
                    print 'In rgrdPrimary using single precision 3D computation'

                dataOut, ier = dsgridmodule.grid3s(self.nxi, self.xi, self.yi, self.zi, dataIn,          # 3D
                               self.nxo, self.nyo, self.nzo, self.xo, self.yo, self.zo)
                if ier != 0:
                    msg = 'Error in return from grid3s call with -- ' + Dsgrid.errorTable(self)[ier]
                    raise ValueError,msg
            else:
                if debug == 1:
                    print 'In rgrdPrimary using single precision 2D computation'
                dataOut, ier = dsgridmodule.grid2s(self.nxi, self.xi, self.yi, dataIn,                   # 2D
                                         self.nxo, self.nyo, self.xo, self.yo)
                if ier != 0:
                    msg = 'Error in return from grid2s call with -- ' + Dsgrid.errorTable(self)[ier]
                    raise ValueError, msg

        else:                                                                                            # double precision
            if self.is3D == 'yes':
                if debug == 1:
                    print 'In rgrdPrimary using double precision 3D computation'
                dataOut, ier = dsgridmodule.grid3d(self.nxi, self.xi, self.yi, self.zi, dataIn,          # 3D
                               self.nxo, self.nyo, self.nzo, self.xo, self.yo, self.zo)
                if ier != 0:
                    msg = 'Error in return from grid3d call with -- ' + Dsgrid.errorTable(self)[ier]
                    raise ValueError, msg
            else:
                if debug == 1:
                    print 'In rgrdPrimary using double precision 2D computation'
                dataOut, ier = dsgridmodule.grid2d(self.nxi, self.xi, self.yi, dataIn,                   # 2D
                                         self.nxo, self.nyo, self.xo, self.yo)
                if ier != 0:
                    msg = 'Error in return from grid2d call with -- ' + Dsgrid.errorTable(self)[ier]
                    raise ValueError, msg

        return dataOut

    def rgrdSinglePoint(self, dataIn):
        #-------------------------------------------------------------------
        #
        #
        #------------------------------------------------------------------------

        if self.group == 'single':                                                 # single precision

            if self.is3D == 'yes':                                                 # 3D
                if debug == 1:
                    print 'In rgrdSinglePoint using single precision 3D computation'
                dataOut, ier = dsgridmodule.pnt3s(self.nxi, self.xi, self.yi, self.zi, dataIn, self.nxo, self.xo, self.yo, self.zo)
                if ier != 0:
                    msg = 'Error in return from pnt3s call with -- ' + Dsgrid.errorTable(self)[ier]
                    raise ValueError, msg
            else:                                                                  # 2D
                if debug == 1:
                    print 'In rgrdSinglePoint using single precision 2D computation'
                dataOut, ier = dsgridmodule.pnt2s(self.nxi, self.xi, self.yi, dataIn, self.nxo, self.xo, self.yo)
                if ier != 0:
                    msg = 'Error in return from pnt2s call with -- ' + Dsgrid.errorTable(self)[ier]
                    raise ValueError, msg

        else:                                                                      # double precision

            if self.is3D == 'yes':                                                 # 3D
                if debug == 1:
                    print 'In rgrdSinglePoint using double precision 3D computation'
                dataOut, ier = dsgridmodule.pnt3d(self.nxi, self.xi, self.yi, self.zi, dataIn, self.nxo, self.xo, self.yo, self.zo)
                if ier != 0:
                    msg = 'Error in return from pnt3d call with -- ' + Dsgrid.errorTable(self)[ier]
                    raise ValueError, msg
            else:                                                                  # 2D
                if debug == 1:
                    print 'In rgrdSinglePoint using double precision 2D computation'
                dataOut, ier = dsgridmodule.pnt2d(self.nxi, self.xi, self.yi, dataIn, self.nxo, self.xo, self.yo)
                if ier != 0:
                    msg = 'Error in return from pnt2d call with -- ' + Dsgrid.errorTable(self)[ier]
                    raise ValueError, msg

        return dataOut 

    #---------------------------------------------------------------------------------
    # ****************  Control parameter manipulation functions  ********************
    #---------------------------------------------------------------------------------

    def parameterNames(self):
        #------------------------------------------------------------------------------
        #                                      
        #     purpose:  produce a list of the dsgrid parameters
        #
        #     usage:    parameters = parameterNames(self)
        #
        #     passed:   self
        #
        #     returned: parameters
        #
        #------------------------------------------------------------------------------

        parameters = ['name', '----', 'dmv', 'dmx', 'erf', 'exp', 'shd']

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

        typeDict = { 'dmv':'float', 'dmx':'float', 'erf':'char', 'exp':'float', 'shd':'int' }

        return typeDict 

    def makeDefaultParameterTable(self):
        #-----------------------------------------------------------------------------------
        #                                      
        #     purpose: construct the dictionary which is the default control parameters table
        #
        #     usage:   makeDefaultParameterTable(self)
        #
        #     passed:   self
        #
        #     returned: parameterDict
        #
        #----------------------------------------------------------------------------------

        parameterDict = {
      'name': ('type ', '  legal values   ',  '  default values ', '                 description                           '),
      '----': ('-----', '-----------------',  '-----------------', '-------------------------------------------------------'),
      'dmv': (' float', 'any              ',  '-9999.           ', 'special value for data init and use with dmx parameter '),
      'dmx': (' float', '> 0.             ',  '1.0e20           ', 'for  weights, use only input data within distance dmx  '), 
      'erf': (' char ', 'any file name    ',  '"sterror"        ', 'error file                                             '),
      'exp': (' float', '> 0.             ',  '3.0              ', 'power to use for inverse distances in computing weights'),
      'shd': (' int  ', '0 = no or 1 = yes',  '0                ', 'controls whether the shadowing feature is on           ') }

        return parameterDict      

    def makeInstanceParameterTable(self):
        #----------------------------------------------------------------------------------
        #                                      
        #     purpose:  construct the dictionary which is the instance parameters table
        #
        #     usage:    makeInstanceParameterTable(self)
        #
        #     passed:   self
        #
        #     returned: parameterDict
        #
        #----------------------------------------------------------------------------------

        parameterDict = {
      'name':('type ','  legal values ','  Values      ','                 description                           '),
      '----':('-----','---------------','--------------','-------------------------------------------------------'),
      'dmv': ('float','any            ',eval('self.dmv'),'special value for data init and use with dmx parameter '),
      'dmx': ('float','> 0.           ',eval('self.dmx'),'for  weights, use only input data within distance dmx  '), 
      'erf': ('char ','any file name  ',eval('self.erf'),'error file                                             '),
      'exp': ('float','> 0.           ',eval('self.exp'),'power to use for inverse distances in computing weights'),
      'shd': ('int  ','0 = no, 1 = yes',eval('self.shd'),'controls whether the shadowing feature is on           ') }

        return parameterDict      

    def printDefaultParameterTable(self):
        """    --------------------------------------------------------------------------------------------------------
                                               
              purpose: print the value of all the parameters
         
              usage:    r.printDefaultParameterTable()
         
                        where r is an instance of Dsgrid
         
              passed:   self
         
              returned: None
         
    --------------------------------------------------------------------------------------------------------""" 

        names = Dsgrid.parameterNames(self)
        names = names[2:]

        parameterDict = Dsgrid.makeDefaultParameterTable(self)
        for item in names:
            items = (item, parameterDict[item][0], parameterDict[item][1], parameterDict[item][2], parameterDict[item][3])  
            print '%-7.7s  %-6.6s  %-14.14s   %-9.9s  %s' % items

        return 


    def printInstanceParameterTable(self):
        """    --------------------------------------------------------------------------------------------------------
                                               
              purpose: print the value of all the parameters
         
              usage:  r.printInstanceParameterTable()
         
                      where r is an instance of Dsgrid
         
              passed:   self
         
              returned: None
         
    --------------------------------------------------------------------------------------------------------""" 

        names = Dsgrid.parameterNames(self)
        names = names[2:]

        parameterDict = Dsgrid.makeInstanceParameterTable(self)
        for item in names:
            items = (item, parameterDict[item][0], parameterDict[item][1], parameterDict[item][2], parameterDict[item][3])  
            print '%-7.7s  %-6.6s  %-12.12s   %-7.7s  %s' % items

        return 

    def printInstanceParameters(self):
        """    --------------------------------------------------------------------------------------------------------
                                               
              purpose: print the values of the current dsgrid control parameters in c code
         
              usage:    r. printInstanceParameters()
         
                      where r is an instance of Dsgrid
         
              passed:   self
         
              returned: None
         
    --------------------------------------------------------------------------------------------------------""" 

        names = Dsgrid.parameterNames(self)
        names = names[2:]

        typeDict = Dsgrid.parameterType(self)

        for name in names:
            if typeDict[name] == 'int':
                print 'Currently,  %s = %d' % (name, eval('self.' + name))
            elif typeDict[name] == 'char':
                print 'Currently,  %s = %s' % (name, eval('self.' + name))
            elif typeDict[name] == 'float':
                print 'Currently,  %s = %f' % (name, eval('self.' + name))

        return None      

    def setInstanceParameters(self):
        #---------------------------------------------------------------------------
        #                                      
        #     purpose: set the instance values of the current dsgrid control parameters in c code
        #
        #     usage:  r.setInstanceParameters() 
        #
        #             where r is an instance of Dsgrid
        #
        #     passed:   self
        #
        #     returned: None
        #
        #----------------------------------------------------------------------------

        names = Dsgrid.parameterNames(self)
        names = names[2:]

        typeDict = Dsgrid.parameterType(self)

        # set the current values for the dsgrid control parameters


        for name in names:
            if typeDict[name] == 'int':
                dsgridmodule.seti(name, eval('self.' + name))
            elif typeDict[name] == 'char':
                dsgridmodule.setc(name, eval('self.' + name))
            elif typeDict[name] == 'float':
                dsgridmodule.setr(name, eval('self.' + name))

        return None      

    #---------------------------------------------------------------------------------
    # *****************************  Error Table  ************************************
    #---------------------------------------------------------------------------------
    def errorTable(self):

        """    --------------------------------------------------------------------------------------------------------
                                             
            purpose: construct the dictionary which provides access to error messages
         
            usage:  errorDict = r.errorTable()
         
                    where r is an instance of Dsgrid
         
            returned: errorDict
         
    --------------------------------------------------------------------------------------------------------""" 

        errorDict = {
            1:  'Error number out of range',
            2:  'Insufficient data in gridded region to triangulate',
            3:  'Array dimension out of range',
            4:  'Parameter name not known',
            5:  'Cannot open error file',
            6:  'Error allocating memory for input points',
            7:  'Fortran DOUBLE PRECISION entries are not supported on UNICOS',
            9:  'Error allocating memory for array of distances between input points',
            10: 'Error allocating memory for weights',
            11: 'Error allocating memory for distance ordering vector',
            12: 'Error allocating memory for output array',
            13: 'Number of input points must be greater than 2',
            14: 'No original data values within the specified distance - interpolated value set to missing value' }

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

        typeDict = Dsgrid.parameterType(self)

        if name in typeDict.keys():
            if typeDict[name] == 'int':
                dsgridmodule.seti(name, value)
                self.__dict__[name] = value
            elif typeDict[name] == 'char':
                dsgridmodule.setc(name, value)
                self.__dict__[name] = value
            elif typeDict[name] == 'float':
                dsgridmodule.setr(name, value)
                self.__dict__[name] = value
            elif typeDict[name] == 'double':
                dsgridmodule.setrd(name, value)
                self.__dict__[name] = value

        else:
            self.__dict__[name] = value

        return None

    def __getattr__(self, name):
        #---------------------------------------------------------------------------------
        #
        #   purpose:  '__getattr__' is called only if a referenced attribute can not be found
        #             in the instance. It gets the attribute from dsgridmodule if possible.
        #
        #   usage:    x.name  -- name is the oject and not a string repr   
        #
        #   passed :  name 
        #
        #   returned: x.name 
        #
        #---------------------------------------------------------------------------------

        typeDict = Dsgrid.parameterType(self)

        if name in typeDict.keys():
            if typeDict[name] == 'int':
                value = dsgridmodule.geti(name)
            elif typeDict[name] == 'char':
                value = dsgridmodule.getc(name)
            elif typeDict[name] == 'float':
                value = dsgridmodule.getr(name)
            elif typeDict[name] == 'double':
                value = dsgridmodule.getrd(name)

        else:
            value = self.__dict__[name]

        return value

    #---------------------------------------------------------------------------------
    # *******************************************************************
    # ****************  end of magic functions **************************
    # *******************************************************************
    #---------------------------------------------------------------------------------

def printParameterTable():
    """    --------------------------------------------------------------------------------------------------------
     routine:  printParameterTable
     
     purpose:  print the control parameter table using the default values from outside the Dsgrid class
     
     usage:    import ds
               ds.printParameterTable()
     
     passed:   nothing 
     
     returned: None
     
     definition: printParameterTable():
--------------------------------------------------------------------------------------------------------""" 

    names = ['name', '----', 'dmv', 'dmx', 'erf', 'exp', 'shd']

    parameterDict = {
      'name': ('type ', '  legal values   ',  '  default values ', '                 description                           '),
      '----': ('-----', '-----------------',  '-----------------', '-------------------------------------------------------'),
      'dmv': (' float', 'any              ',  '-9999.           ', 'special value for data init and use with dmx parameter '),
      'dmx': (' float', '> 0.             ',  '1.0e20           ', 'for  weights, use only input data within distance dmx  '), 
      'erf': (' char ', 'any file name    ',  '"sterror"        ', 'error file                                             '),
      'exp': (' float', '> 0.             ',  '3.0              ', 'power to use for inverse distances in computing weights'),
      'shd': (' int  ', '0 = no or 1 = yes',  '0                ', 'controls whether the shadowing feature is on           ') }

    for item in names:
        items = (item, parameterDict[item][0], parameterDict[item][1], parameterDict[item][2], parameterDict[item][3])  
        print '%-7.7s  %-6.6s  %-14.14s   %-9.9s  %s' % items

    return 

def printStoredParameters():
    """    --------------------------------------------------------------------------------------------------------
     routine:  printStoredParameters
     
     purpose: print the values of the current dsgrid control parameters in c code. The call
                   to the method function rgrd will change them to the instance values.
     
     usage:    import ds
               ds.printStoredParameters() 
     
     passed:   nothing 
     
     returned: None
     
     definition: printStoredParameters():
--------------------------------------------------------------------------------------------------------""" 

    names = ['dmv', 'dmx', 'erf', 'exp', 'shd']

    typeDict = { 'dmv':'float', 'dmx':'float', 'erf':'char', 'exp':'float', 'shd':'int' }

    for item in names:
        if typeDict[item] == 'int':
            print '   %s = %d' % (item, dsgridmodule.geti(item))
        elif typeDict[item] == 'char':
            print '   %s = %s' % (item, dsgridmodule.getc(item))
        elif typeDict[item] == 'float':
            print '   %s = %f' % (item, dsgridmodule.getr(item))
        elif typeDict[item] == 'double':
            print '   %s = %f' % (item, dsgridmodule.getrd(item))

    return None      


def checkdim(x, y, z = None):
    """    #-------------------------------------------------------------------
    #                                      
    #     purpose: determine whether the coordinate grid is random or monotonically increasing
    #
    #     usage:  
    #
    #     returned: x, y, monotonic, xreverse, yreverse
    #                       or
    #               x, y, z, monotonic, xreverse, yreverse, zreverse
    #
    #------------------------------------------------------------------------"""
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

    if z  is None:

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

    else:
        zsize = len(z)
        if z[0] > z[zsize - 1]:
            z = z[::-1]
            zreverse = 'yes'
        else:
            zreverse = 'no'

        zmonotonic = 'yes'                               # monotonic and possibly reversed to make it montonically increasing
        for n in range(1, zsize):
            if z[n] < z[n - 1]:
                zmonotonic = 'no'                        # not monotonic so return the original grid

    if xmonotonic == 'yes' and ymonotonic == 'yes' and zmonotonic == 'yes':  # if all are monotonic the grid is monotonic
        monotonic = 'yes'
    else:
        monotonic = 'no'
        if xreverse == 'yes':                           # return vectors to thier original state
            x = x[::-1]
            xreverse = 'no'
        if yreverse == 'yes':
            y = y[::-1]
            yreverse = 'no'
        if zreverse == 'yes':
            z = z[::-1]
            zreverse = 'no'

        # note that x, y and z may be returned reversed as necessary only if monotonic is set to yes

    return x, y, z, monotonic, xreverse, yreverse, zreverse
#---------------------------------------------------------------------------------
# ********************************************************************************
# ********************************************************************************
#---------------------------------------------------------------------------------

def document():
    """        #-------------------------------------------------------------------------
    #
    #    purpose:   'document' writes documentation for the user to a file 
    #
    #    usage:     import ds
    #               ds.document()   
    #    
    #    passed :   nothing
    #
    #    returned:  nothing
    #
    #-------------------------------------------------------------------------"""
    import ds

    std = sys.stdout                                             # save sys.stout to allow reassigning later
    sys.stdout = open( 'dsgrid.doc', 'w')

    print '**********************************************************************************************\n'  
    print '**************************** Overview of the CDAT interface to dsgrid ************************\n'
    print '**********************************************************************************************\n'  
    print ds.__doc__
    print
    print

    print '  ******************** Instructions for use of the grid2s function *****************************'
    print dsgridmodule.grid2s.__doc__
    print

    print '  ******************** Instructions for use of the grid3s function *****************************'
    print dsgridmodule.grid3s.__doc__
    print

    print '  ******************** Instructions for use of the seti function *****************************'
    print dsgridmodule.seti.__doc__
    print

    print '  ******************** Instructions for use of the geti function *****************************'
    print dsgridmodule.geti.__doc__
    print

    print '  ******************** Instructions for use of the setr function *****************************'
    print dsgridmodule.setr.__doc__
    print

    print '  ******************** Instructions for use of the getr function *****************************'
    print dsgridmodule.getr.__doc__
    print

    print '  ******************** Instructions for use of the setc function *****************************'
    print dsgridmodule.setc.__doc__
    print

    print '  ******************** Instructions for use of the getc function *****************************'
    print dsgridmodule.getc.__doc__
    print

    print '  ******************** Instructions for use of the pnt2s function *****************************'
    print dsgridmodule.pnt2s.__doc__
    print

    print '  ******************** Instructions for use of the pnt3s function *****************************'
    print dsgridmodule.pnt3s.__doc__
    print

    print '  ******************** Instructions for use of the grid2d function *****************************'
    print dsgridmodule.grid2d.__doc__
    print

    print '  ******************** Instructions for use of the grid3d function *****************************'
    print dsgridmodule.grid3d.__doc__
    print

    print '  ******************** Instructions for use of the setrd function *****************************'
    print dsgridmodule.setrd.__doc__
    print

    print '  ******************** Instructions for use of the getrd function *****************************'
    print dsgridmodule.getrd.__doc__
    print

    print '  ******************** Instructions for use of the pnt2d function *****************************'
    print dsgridmodule.pnt2d.__doc__
    print

    print '  ******************** Instructions for use of the pnt3d function *****************************'
    print dsgridmodule.pnt3d.__doc__
    print

    sys.stdout = std

    return None

def help(choice = None):
    import ds

    if choice is None:                               # get instructions for use of help
        print """     ----------------------------------------------------------------------------------------
      
      INSTRUCTIONS ON USE THE OBJECT ORIENTED INTERFACE TO THE DSGRID PACKAGE FROM NGMATH 
     
      This module is built as one class, Dsgrid, which sports a single method called rgrd.
     
      To get instructions on making an instance of Dsgrid, type
     
              ds.help('Dsgrid')
     
      To get instructions on using the control parameters, type
     
              ds.help('parameters')
     
      To print the table describing the control parameters, type
     
              ds.help('table')
     
      To get instructions on performing interpolations, type
     
              ds.help('interpolations')
     
     
      INSTRUCTIONS ON USE OF ORIGINAL DSGRID PACKAGE FROM NGMATH 
     

      The module dsgridmodule.so exports the following functions to Python from the original C library:
        
                Single precision procedures:                                                    
                                                                                                
                    grid2s       - primary function for gridding 2D data.                   
                    grid3s       - primary function for gridding 3D data.                   
                    seti         - set int parameter values.                                
                    geti         - retrieve values for int parameters.                      
                    setr         - set float parameter values.                              
                    getr         - retrieve values for float parameters                     
                    setc         - set char parameter values.                                
                    getc         - retrieve values for char parameters.                     
                    pnt2s        - interpolate 2D data at specified individual points.      
                    pnt3s        - interpolate 3D data at specified individual points.      
                                                                                                
                                                                                                
                Double precision procedures:                                                    
                                                                                                
                    grid2d       - primary function for gridding 2D data.                   
                    grid3d       - primary function for gridding 3D data.                   
                    setrd        - set float parameter values.                              
                    getrd        - retrieve values for float parameters                     
                    pnt2d        - interpolate 2D data at specified individual points.      
                    pnt3d        - interpolate 3D data at specified individual points.      
                                                                                                
      Information on the use of the routines is available by importing dsgridmodule and printing the docstring
      of interest. For example, documentation for the routine grid2s is obtained by typing
 
            import dsgridmodule

            print dsgridmodule.grid2s.__doc__
 
      This same information is available in the help package. For example, to get the docstring for grid2s, type
 
            import ds

            print ds.help('grid2s')
 
      A description of the control parameters is not in the dsgridmodule documentation. It can be found by typing
 
            import ds
            ds.printParameterTable()
 
      This same information is available in the help package.
 
     ------------------------------------------------------------------------------------------------------"""

    elif choice == 'Dsgrid': 
        print """     ----------------------------------------------------------------------------------------

            Rectangular Gridded Output Coordinates

                To make an instance, r, type:
 
                    import ds

                    r = ds.Dsgrid(xi, xo, yi, yo)

                    or

                    r = ds.Dsgrid(xi, xo, yi, yo, zi, zo)
        
                       where xi, yi and zi are the input coordinate arrays in list format
                       while xo, yo and zo are the and output grid coordinate arrays which
                       must be increasing but not necessarily evenly spaced.
        
            List Format Output Coordinates

                To make an instance, r, type:
 
                    import ds

                    r = ds.Dsgrid(xi, xo, yi, yo, griddedOutput = 'no')

                    or

                    r = ds.Dsgrid(xi, xo, yi, yo, zi, zo, griddedOutput = 'no')
        
                       where xi, yi and zi are the input coordinate arrays in list format
                       and xo, yo and zo are the output coordinate arrays in list format.
         
            DSGRID has regridding functions which carry out their respective interpolations in single
            or double precision. The choice is determined by the type of the coordinate arrays submitted
            in makking the instance. 
 
     --------------------------------------------------------------------------------------------------------------------"""

    elif choice == 'parameters': 
        print """     ----------------------------------------------------------------------------------------
                                           
             In the absence of an instance of the class Dsgrid, a description of the control parameters can be found 
             by typing
     
               import ds
               ds.printParameterTable()
     
     
            The control parameters are easily available within the class. First make an instance, r. To get instructions
            on making an instance, type
     
                import ds

                ds.help('Dsgrid')

            Then to look at the default settings for the control parameters, type
     
                r.printDefaultParameterTable()
     
            To change a setting type the new value. For example, to set exp to 5.0, type
 
                r.exp = 5.0
 
            To find a value without printing the table, type the name. For example, to exam the value of shd, type
 
                r.shd
     
            To check the settings type
     
                r.printInstanceParameterTable() -- prints the table with values and a description of the parameters
                                                   used in subsequent calls to the method function rgrd
                or

                r.printInstanceParameters()     -- prints a list of the parameters values used in subsequent calls to the 
                                                   the rgrd method

     --------------------------------------------------------------------------------------------------------------------"""

    elif choice == 'table': 
        printParameterTable()
     
    #-----------------------------------------------------------------------------------------------------

    elif choice == 'interpolations':  
        print """     ----------------------------------------------------------------------------------------

            First make an instance, r. To get instructions on making an instance, type
     
                import ds

                ds.help('Dsgrid')


     --------------------------------------------------------------------------------------------------------------------"""

    elif choice == 'grid2s':
        print dsgridmodule.grid2s.__doc__
    elif choice == 'grid3s':
        print dsgridmodule.grid3s.__doc__
    elif choice == 'grid2d':
        print dsgridmodule.grid2d.__doc__
    elif choice == 'grid3d':
        print dsgridmodule.grid3d.__doc__
    elif choice == 'seti':
        print dsgridmodule.seti.__doc__
    elif choice == 'geti':
        print dsgridmodule.geti.__doc__
    elif choice == 'setr':
        print dsgridmodule.setr.__doc__
    elif choice == 'getr':
        print dsgridmodule.getr.__doc__
    elif choice == 'setc':
        print dsgridmodule.setc.__doc__
    elif choice == 'getc':
        print dsgridmodule.getc.__doc__
    elif choice == 'pnt2s':
        print dsgridmodule.pnt2s.__doc__
    elif choice == 'pnt3s':
        print dsgridmodule.pnt3s.__doc__
    elif choice == 'pnt2d':
        print dsgridmodule.pnt2d.__doc__
    elif choice == 'pnt3d':
        print dsgridmodule.pnt3d.__doc__
    else:
        print 'Your request is not in help.   The help choices are the strings: Dsgrid, parameters, table, interpolations, grid2s, grid2d, grid3s, grid3d,  seti, geti, setr, getr, setrd, getrd, setc, getc, pnt2s, pnt3s, pnt2d and pnt3d' 

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
    if value1  is None:
        print msg
    elif value2  is None:
        print msg, value1
    else:
        print msg, value1, value2
    print '*******************************************************************'

    return None

