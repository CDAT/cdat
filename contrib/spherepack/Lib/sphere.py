# Adapted for numpy/ma/cdms2 by convertcdms.py
"""Documentation for module sphere: an interface to spherepack
  
   INTRODUCTION
  
        This module provides access through Python to the collection of Fortran programs in
        SPHEREPACK 3.0, which is a collection of programs produced at the National Center for 
        Atmospheric Research by John C. Adams and Paul N. Swarztrauber for computing certain 
        common differential operators and performing related manipulations on a sphere. It provides
        solutions via the spectral method that uses both scalar and vector harmonic transforms.
        Since scalar and vector quantities are fundamentially different on the sphere due to 
        the multiple valued and discontinuous nature of vectors at the poles, separate
        functions are provided for scalar and vector quantities. 
  
   RESTRICTIONS
  
        Spherepack is not for everyone. Spherepack manipulates data defined strictly on the global
        sphere. Accordingly, the following restrictions apply:
  
               The longitude vector consists of unique evenly spaced points spanning the globe. 
               The latitude vector can be either gaussian or evenly spaced including the poles.
               Missing data is not allowed. 
  
   THE SHORT STORY 
 
        To save time in coming up to speed in the use of this python wrapper around SPHEREPACK 3.0,
        skip to section GENERAL EXAMPLE. To run some test cases using analytically generated data
        type
                    python sphere.py
  
        It also puts out this documentation in the file spheremodule.doc and a copy of the information
        written to the screen as screen.asc for what it is worth.
  
   ORGANIZATION 
 
        This module is object oriented for simplicity. It is organized as three classes reflecting
        the three functional groups in SPHEREPACK 3.0 which perform vector analysis computations,
        regridding and grid shifting. The vector analysis computations are contained in the Sphere
        class, the regridding in the Regrid class and the grid shifting in the Shiftgrid class. The
        class names begin with a capital letter. Python is case sensitive. Access to the functions 
        housed in the classes is a simple two step process. The first step is making an instance of 
        the class which hosts the desired functionality. The second step is calling the actual function
        or functions of interest.
 
        Making an instance requires passing the grid vectors and possibly a computaional parameter 
        selecting the storage treatment of the Legendre polynomials. This information is used internally
        to select the Fortran function call. For example, to compute the divergence of a vector function,
        it is necessary to choose one of four Fortran functions according to whether the grid is evenly
        spaced or gaussian and the Legendre polynomials are stored or computed. These bookkeeping details
        are done automatically for the user based on the argument list passed in the instance statement.
 
        Having set up the machinery with the instance call, the class method functions are called by
        qualification using the dot operator. It is only necessary to submit the data and accept the return
        of the answer.
 
        As a preview, here is the two step process for calculating the divergence and the vorticity of a 
        2D vector field (u, v)
 
                x = sphere.Sphere(longitudeVector, latitudeVector)
                divergence = x.div(u, v)
                vorticity = x.vrt(u, v)
 
        which demonstrates the advantage of the object oriented approach. The instance can be reused for
        multiple computations and the argument list is simple and intuitive. Having calculated the 
        divergence and the vorticity, the user may want do view smoother fields. To truncate the fields
        at T16, write
                divergence_T16 = x.truncation(16, divergence)
                vorticity_T16 = x.truncation(16, vorticity)
 
   DOCUMENTATION
  
        Documentation written to the file spheremodule.doc can be obtained after importing the spheretest module by typing 
  
               spheretest.document() 
  
        A brief view of the documentation consisting of the overview can be written to the file spheremodule.doc after
        importing the sphere module by typing 
  
               spheretest.document(brief = 'yes') 
  
        Online documentation for individual classes or method functions is available from the module doctring and the
        help package. As examples:
  
               print sphere.__doc__       --  four page overview of the package
               sphere.help()              --  contents of the help function
               sphere.help('div')         --  documentation for the Sphere class div method 
               sphere.Sphere.div.__doc__  --  documentation for the Sphere class div method 
  
        For the utilities type for example
  
               sphere.help('gridGenerator')  -- grid vector generation 
 
   CONTENTS
 
        This module provides access to the Fortran library in terms of three functional groups
        which perform vector analysis computations, regridding and grid shifting. 
 
        Vector Analysis and Truncation -- contained in Sphere class
 
            The functions for computing differential operations and their inverses on scalar
            and vector functions in spherical coordinates on a global grid are:
        
                div       -- computes the divergence of a vector function                
                idiv      -- inverts the divergence creating an irrotational vector function            
                vrt       -- the vorticity of a vector function                
                ivrt      -- inverts the vorticity creating a divergence_free vector function      
                idvt      -- inverts the divergence and the vorticity creating a vector function
                 
                vts       -- computes the derivative of the vector function with respect to latitude 
                grad      -- computes the gradient of a scalar function
                igrad     -- inverts the gradient creating a scalar function 
                slap      -- computes the Laplacian of a scalar function
                islap     -- inverts the Laplacian of a scalar function
                vlap      -- computes the Laplacian of a vector function
                ivlap     -- inverts the Laplacian of a vector function
                sfvp      -- computes the stream function and the velocity potential of a vector function
                isfvp     -- inverts the stream function and the velocity potential of a vector function
     
            One additional function, not part of the basic library, has been added to perform triangular 
            truncation with or withhout tapering:
             
                truncation--  truncates scalar or vector data at specified total wavenumber                  
                             
            The basic functions for spectral analysis and synthesis directly accessible from python are: 
            
                sha       -- computes the spherical harmonic analysis of a scalar function
                shs       -- computes the spherical harmonic synthesis of a scalar function
                vha       -- computes the spherical harmonic analysis of a vector function
                vhs       -- computes the spherical harmonic synthesis of a vector function
                                                
        Regridding -- contained in Regrid class
                         
            The two functions for regridding are:
             
             regridScalar --  transfers scalar data from one global grid to another                  
             regridVector --  transfers vector data from one global grid to another                  
             
        Shifting -- contained in Shiftgrid class
     
            The two functions for shifting an evenly spaced grid by half an increment in longitude
            and latitude are:
             
             shiftScalar  --  transfers scalar data between an evenly spaced regular and an offset grid       
             shiftVector  --  transfers vector data between an evenly spaced regular and an offset grid       
             
             where the regular grid is defined as one which includes the poles.
             
             
        Utilities not part of the overall scheme but still of possible interest
             
             gridGenerator  --  generates the longitude and latitude vectors      
             truncate       --  provides truncation at the spectral coefficient level     
         
   PROCEDURE 
 
        Access to the Spherepack Fortran library is provided through the module spherepackmodule.so
        which has been constructed using the Pyfort utility. A wrapper around the functions in this
        spherepackmodule.so has been created as sphere.py. Assuming path access to spherepackmodule.so
        and sphere.py, to use Spherpack3.0 from the National Center for Atmospheric Research, it is
        only necessary to import the sphere.py module. It contains the classes Sphere, Regrid, and 
        Shiftgrid designed for the user. This is done by typing at the python prompt 
       
            import sphere 
 
        Use of spherepack is a two step process. The first step is the creation of an instance of the 
        appropriate class for the computation at hand. For this step there are three classes as a choice:
        Sphere, Regrid and Shiftgrid. The second step is the selection of the class method of interest by 
        qualification with the dot operator.
 
        As notation related to the display of the argument lists in the calls, the required ones use the
        keyword name only and the optional ones are written as keyword entries. 

        In practise there are lots of choices allowed by Python. At one extreme all arguments can be
        submitted using keyword assignments while setting them to None for optional unused  vectors. In
        the class Sphere, the computational scheme default 'computed' can be changed to 'stored'. At the
        other extreme all arguments can be submitted in the correct position. The examples will clarify
        this. 
 
        Vector Analysis and Truncation
 
            As the first step for the spherical differential operation calculations, the instance x of the 
            Sphere class is made with the statement typed to the python prompt
     
                x = sphere.Sphere(lonArray , latArray, numberLevels = nlev, numberTimes = ntime,
     
                                                                            computed_stored = 'computed') 
     
                where nlev and ntime are the actual number of levels and times respectively and the keywords
                are
     
                lonArray = longitude vector (required)
                latArray = latitude vector (required)
                numberLevels = number of levels (optional)
                numberTimes = number of times (optional)
                computed_stored (optional) : 'computed' -- computed Legendre polynomials 
                                              'stored' -- stored Legendre polynomials
                                              This choice involves a 30% storage/speed tradeoff
     
             The instance request uses computed_stored and the grid vectors to associate the actual Fortran
             calls with the requested calculation. This association is automatic and need not concern the user.
             If the longitude and latitude grid vectors are not available, prior to the instance creation
             they can be constructed by calling the utility function girdGenerator.
     
             As a second step, the desired method is selected. As a concrete example, the divergence of the
             the vector pair u and v is found with
     
                 div = x.div(u, v)
                       or
                 div = x.div(u, v, missingValue) - with the request to check for missing data
             
             This div function calls all the spherepack functions needed to return the divergence. The second form
             looks for the presence of missing data and returns an error if found. It uses the exact value so it
             must be the value in the data file.        
             
        Regridding
                             
            As the first step for regridding make an instance x of the Regrid class with
     
                x = sphere.Regrid(lonArrayOut, latArrayOut, lonArrayIn, latArrayIn, numberLevels = nlev, 
                                                                                      numberTimes = ntime) 
     
                where nlev and ntime are the actual number of levels and times respectively and the keywords
                are
     
                lonArrayOut = output grid longitude vector (required)
                latArrayOut = output grid latitude vector (required)
                lonArrayIn  = input grid longitude vector (required)
                latArrayIn  = input grid latitude vector (required)
                numberLevels  = input grid number of levels (optional)
                numberTimes = input grid number of times (optional)
     
            As an example of the second step, regridding a scalar function sf is accomplished with
     
                sf = x.regridScalar(sf)                 
                       or
                sf = x.regridScalar(sf, missingValue)                 
 
             The second form looks for the presence of missing data and returns an error if found. It uses the
             exact value so it must be the value in the data file.        
     
        Shifting
                             
            For the grid shifting as step one make an instance x of the Shiftgrid class with
     
                x = sphere.Shiftgrid(lonArray, latArray, numberLevels = nlev, numberTimes = ntime) 
     
                where nlev and ntime are the actual number of levels and times respectively and the keywords
                are
     
                lonArray = longitude vector (required)
                latArray = latitude vector (required)
                numberLevels = number of levels (optional)
                numberTimes = number of times (optional)
     
            As an example of the second step, shifting a scalar function sf is accomplished with
     
                sf = x.shiftScalar(sf)
                       or
                sf = x.shiftScalar(sf, missingValue)
                         
 
             The second form looks for the presence of missing data and returns an error if found. It uses the
             exact value so it must be the value in the data file.        
         
   GENERAL EXAMPLE 
         
        Step 1.  Type
                     import sphere
         
        Step 2.  From this documentation determine the class which offers the desired computation. There are only 
                 three choices: the Sphere class, Regrid  class and Shiftgrid class to use in ClassName below.
                 Instructions on making an instance is obtained by typing
         
                     sphere.help('ClassName')
         
        Step 3.  Make an instance, x, of the specific class ClassName using the statement 
         
                     x = sphere.ClassName(argument1, argument2, .........) 
 
         
        Step 4.  Perform the actual computation using a specific function named functionName, which has been
                 identified from this documentation.
         
                     returned values = x.functionName(argument1, argument2, .........)         
 
                 To get information about the agrument list type
 
                     sphere.help('functionName')
         
         
   TESTING 
 
        Typing 
         
            cdat spheretest.py
         
        generates some testing of the spheremodule using analytical functions as fields.
         
        For additional testing using real geophysical data, you might try the following exercise. 
         
            Step 1. Get winds u and v and their grid vectors, longitude values (lonvals) and 
                    latitude values (latvals), from somewhere.This example uses 2D fields for 
                    simplicity. The fields must be global without missing values. 
                    
         
            Step 2. Make an instance of the Sphere class, x, as
 
                        x = sphere.Sphere(lonvals, latvals)
 
            Step 3. Compute the streamfunction, sf, and the velocity potential, vp, using 
 
                        sf, vp = x.sfvp(u, v)
 
            Step 4. Compute the source for the streamfunction, sf_source, and the velocity potential, vp_source,
                    using the scalar Laplacian
 
                        sf_source = x.slap(sf)
                        vp_source = x.slap(vp)
 
            Step 5. Compute the source for the streamfunction, vort, and the velocity potential, div, directly 
                    using the divergence and the vorticity
 
                        vort = x.vrt(u, v)
                        div = x.div(u, v)
 
            Step 6. Compare the results for equality, sf_source with vort and vp_source with div. If the comparison
                    fails, please complain about it.
         
"""

import sys, string
import spherepack, numpy, math
#spherepack.set_pyfort_option(spherepack.MIRROR)
debug = 0                                           # set to 1 for debug prints
radius = 6.37122e06

usefilled = 'yes'
try:
    import numpy.ma
except ImportError:
    print 'Can not convert from numpy.ma array to numpy array without module numpy.ma'
    usefilled = 'no'

class Sphere:

    #-------------------------------------------------------------------------------------------------------
    # 
    #       Contents of Sphere class
    #
    #           The functions for computing differential operations and their inverses on scalar
    #           and vector functions in spherical coordinates on a global grid are:
    #       
    #               div       -- computes the divergence of a vector function                
    #               idiv      -- inverts the divergence creating an irrotational vector function            
    #               vrt       -- the vorticity of a vector function                
    #               ivrt      -- inverts the vorticity creating a divergence_free vector function      
    #               idvt      -- inverts the divergence and the vorticity creating a vector function
    #                
    #               vts       -- computes the derivative of the vector function with respect to latitude 
    #               grad      -- computes the gradient of a scalar function
    #               igrad     -- inverts the gradient creating a scalar function 
    #               slap      -- computes the Laplacian of a scalar function
    #               islap     -- inverts the Laplacian of a scalar function
    #               vlap      -- computes the Laplacian of a vector function
    #               ivlap     -- inverts the Laplacian of a vector function
    #               sfvp      -- computes the stream function and the velocity potential of a vector function
    #               isfvp     -- inverts the stream function and the velocity potential of a vector function
    #    
    #           One additional function, not part of the basic library, has been added to perform triangular 
    #           truncation with or withhout tapering:
    #            
    #               truncation--  truncates scalar or vector data at specified total wavenumber                  
    #                            
    #           The basic functions for spectral analysis and synthesis directly accessible from python are: 
    #           
    #               sha       -- computes the spherical harmonic analysis of a scalar function
    #               shs       -- computes the spherical harmonic synthesis of a scalar function
    #               vha       -- computes the spherical harmonic analysis of a vector function
    #               vhs       -- computes the spherical harmonic synthesis of a vector function
    #                
    #-------------------------------------------------------------------------------------------------------

    def __init__(self, lonArray, latArray, numberLevels = None, numberTimes = None, computed_stored = 'computed'): 
        """    --------------------------------------------------------------------------------------------------------
         routine:  __init__ for class Sphere
         
         purpose:  'init' assigns values to the instance data which are the dimensions lengths,
                    the latitude direction, the latitude type as gaussian or even and the
                    computational scheme computed_stored.
          
         usage:     x = sphere.Sphere(lonvals, latvals)                                        2D data
                    x = sphere.Sphere(lonArray = lonvals, latArray = latvals) 
                    x = sphere.Sphere(lonArray, latArray, 'stored') 
          
                    x = sphere.Sphere(lonvals, latvals,  numberTimes = ntime)                  3D data 
                    x = sphere.Sphere(lonArray = lonvals, latArray = latvals, numberTimes = ntime) 
                    x = sphere.Sphere(lonvals, latvals, numberTimes = ntime, computed_stored = 'stored') 
          
                    x = sphere.Sphere(lonvals, latvals, numberLevels, numberTimes)              4D data 
                    x = sphere.Sphere(lonArray = lonvals, latArray = latvals, numberLevels = nlev,
                                                                                   numberTimes = ntime) 
                    x = sphere.Sphere(lonvals, latvals, numberLevels, numberTimes, 'stored')  
         
                    where nlev and ntime are the actual number of levels and times respectively.
         
         passed:    lonArray = longitude vector
                    latArray = latitude vector
                    numberLevels = number of levels (optional)
                    numberTimes = number of times (optional)
                    computed_stored (optional) : 'computed' -- computed Legendre polynomials 
                                                  'stored' -- stored Legendre polynomials
                                                  This choice involves a 30% storage/speed tradeoff
         
         returned:  x -- an instance of the Sphere class to qualify using dot notation to choose a computation
        
         definition: __init__(self, lonArray, latArray, numberLevels = None, numberTimes = None,
                                                                      computed_stored = 'computed'): 
         
    --------------------------------------------------------------------------------------------------------""" 
        self.inverseOrder = None     # set in (v)sha for use in routines that contain mathtogeo only

        self.lon = len(lonArray)
        self.lat = len(latArray)

        if latArray[len(latArray)-1] > latArray[0]:
            self.reverseLatitude = 'mathyes'
        else:
            self.reverseLatitude = 'no'

        dimlist = [self.lat, self.lon]

        self.lev = numberLevels
        if numberLevels is not None and  numberLevels != 0:
            dimlist.append(self.lev)

        self.tme = numberTimes
        if numberTimes is not None and  numberTimes != 0:
            dimlist.append(self.tme)
 
        dimlist.reverse()
        self.standardShape = tuple(dimlist)                                   # math order (ntme,nlev,nlon,nlat)

        # check the shape for a unique number of longitudes and a unique number of latitudes

        if self.lon in [self.tme, self.lev, self.lat]:
            print 'Warning - number of longitudes in duplicated in the shape. The geotomath shape \
                   transform will not work unless it differs from the number of latitudes and it \
                   is one of the last two entiries in the shape'
        if self.lat in [self.tme, self.lev, self.lon]:
            print 'Warning - number of latitudes in duplicated in the shape. The geotomath shape \
                   transform will not work unless it differs from the number of longitudes and it \
                   is one of the last two entiries in the shape'

        # check grids and get grid_type as 'even' or 'gaussian'

        grid_type = check_lonlat(lonArray, latArray)

        # pick the class of functions used in the calculation by setting self.gridComp as a class instance. In
        # each of the Sphere class functions self.gridComp is called gridComp to simplify the notation. The
        # choices are:
        #              gc - gaussian latitudes with computed coefficients
        #              ec - evenly spaced latitudes with computed coefficients
        #              gs - gaussian latitudes with stored coefficients
        #              es - evenly spaced latitudes with stored coefficients

        if computed_stored == 'computed':
            if grid_type == 'gaussian':
                self.gridComp = Wrapgc()
            else:
                self.gridComp = Wrapec()
        elif computed_stored == 'stored':
            if grid_type == 'gaussian':
                self.gridComp = Wrapgs()
            else:
                self.gridComp = Wrapes()
        else:
            msg = 'CANNOT PROCESS THE DATA - The computation scheme for the coefficients must be either computed or stored'
            raise ValueError, msg

    def div(self, u, v, missingValue = None):
        """    --------------------------------------------------------------------------------------------------------
         routine:   div 
         
         purpose:   computes the divergence of a vector function 
         
         usage:     div = x.div( u, v, missingValue)  where x is an instance of Sphere
         
         passed:    u -- zonal vector function on a global grid 
                    v -- meridional vector function on a global grid 
                    missingValue -- an optional number requesting a check for missing data
         
         returned:  div --  the divergence of the vector function 
         
         definition: div(self, u, v, missingValue = None):
    --------------------------------------------------------------------------------------------------------""" 
        gridComp = self.gridComp
        nlon = self.lon
        nlat = self.lat
        standardShape = self.standardShape 
        reverseLatitude =  self.reverseLatitude 

        if usefilled == 'yes':
            u = numpy.ma.filled(u)
            v = numpy.ma.filled(v)

        nt, inverseOrder, u, v = geotomath(missingValue, reverseLatitude, standardShape, u, v)

        # --------------------- Vector Harmonic Analysis ----------------------------

        wvha, lvha = gridComp.vhai(nlat, nlon)

        br, bi, cr, ci = gridComp.vha(nlat, nlon, nt, lvha, wvha, u, v)

        # ------------------------ Scalar Harmonic Synthesis --------------------------------

        wshs, lshs = gridComp.shsi(nlat, nlon)

        div = gridComp.div(nlat, nlon, nt, br, bi, lshs, wshs)

        div = mathtogeo(reverseLatitude, standardShape, inverseOrder, div)

        scale = 1.0/radius                                            # scale to radius of the earth
        div = geoscale(scale, div)

        return div 

    def grad(self, sf, missingValue = None):
        """    --------------------------------------------------------------------------------------------------------
         routine:   grad 
                                               
         purpose:   computes the gradient of a scalar function 
         
         usage:     u, v = x.grad(sf, missingValue)
         
         passed:    sf -- scalar function on a global grid 
         
         returned:  u -- zonal vector function 
                    v -- meridional vector function 
                    missingValue -- an optional number requesting a check for missing data
         
         definition: grad(self, sf, missingValue = None):
         
    --------------------------------------------------------------------------------------------------------""" 
        gridComp = self.gridComp
        nlon = self.lon
        nlat = self.lat
        standardShape = self.standardShape 
        reverseLatitude =  self.reverseLatitude 

        if usefilled == 'yes':
            sf = numpy.ma.filled(sf)

        nt, inverseOrder, sf = geotomath(missingValue, reverseLatitude, standardShape, sf)

        # --------------------- Scalar Harmonic Analysis ----------------------------

        wsha, lsha = gridComp.shai(nlat, nlon)

        a, b = gridComp.sha(nlat, nlon, nt, lsha, wsha, sf)


        # --------------------- Vector Harmonic Synthesis ----------------------------

        wvhs, lvhs = gridComp.vhsi(nlat, nlon)

        u, v = gridComp.grad(nlat, nlon, nt, a, b, lvhs, wvhs)

        u, v = mathtogeo(reverseLatitude, standardShape, inverseOrder, u, v)

        scale = 1.0/radius                                     # scale to radius of the earth
        u, v = geoscale(scale, u, v)

        v = -1.0*v                                     # make + derivatives for function increasing in south to north direction
        v = v.astype(numpy.float32)

        return u, v 

    def idiv(self, div, missingValue = None):
        """    --------------------------------------------------------------------------------------------------------
         routine:   idiv 
                                               
         purpose:   computes an irrotational vector function with given divergence 
         
         usage:     u, v = x.idiv(div, missingValue)
         
         passed:    div -- divergence function on a global grid 
                    missingValue -- an optional number requesting a check for missing data
         
         returned:  u -- zonal vector function 
                    v -- meridional vector function 
         
         definition: idiv(self, div, missingValue = None):
         
    --------------------------------------------------------------------------------------------------------""" 
        gridComp = self.gridComp
        nlon = self.lon
        nlat = self.lat
        standardShape = self.standardShape 
        reverseLatitude =  self.reverseLatitude 

        if usefilled == 'yes':
            div = numpy.ma.filled(div)

        nt, inverseOrder, div = geotomath(missingValue, reverseLatitude, standardShape, div)

        scale = radius                                     # scale to unit radius of the earth
        div = geoscale(scale, div)
        # --------------------- Scalar Harmonic Analysis ----------------------------

        wsha, lsha = gridComp.shai(nlat, nlon)

        a, b = gridComp.sha(nlat, nlon, nt, lsha, wsha, div)


        # --------------------- Vector Harmonic Synthesis ----------------------------

        wvhs, lvhs = gridComp.vhsi(nlat, nlon)

        u, v = gridComp.idiv(nlat, nlon, nt, a, b, lvhs, wvhs)

        u, v = mathtogeo(reverseLatitude, standardShape, inverseOrder, u, v)

        return u, v 

    def idvt(self, div, vort, missingValue = None):
        """    --------------------------------------------------------------------------------------------------------
         routine:   idvt 
                                               
         purpose:   computes a vector function with given divergence and vorticity
         
         usage:     u, v = x.idvt(div, vort, missingValue)
         
         passed:    div -- divergence function on a global grid 
                    vort -- vorticity function on a global grid 
                    missingValue -- an optional number requesting a check for missing data
         
         returned:  u -- zonal vector function  
                    v -- meridional vector function 
         
         definition: idvt(self, div, vort, missingValue = None):
         
    --------------------------------------------------------------------------------------------------------""" 
        gridComp = self.gridComp
        nlon = self.lon
        nlat = self.lat
        standardShape = self.standardShape 
        reverseLatitude =  self.reverseLatitude 

        if usefilled == 'yes':
            div = numpy.ma.filled(div)
            vort = numpy.ma.filled(vort)

        nt, inverseOrder, div = geotomath(missingValue, reverseLatitude, standardShape, div)

        scale = radius                                     # scale to unit radius of the earth
        div = geoscale(scale, div)

        nt, inverseOrder, vort = geotomath(missingValue, reverseLatitude, standardShape, vort)

        scale = radius                                     # scale to unit radius of the earth
        vort = geoscale(scale, vort)

        # --------------------- Scalar Harmonic Analysis ----------------------------

        wsha, lsha = gridComp.shai(nlat, nlon)

        ad, bd = gridComp.sha(nlat, nlon, nt, lsha, wsha, div)

        wsha, lsha = gridComp.shai(nlat, nlon)

        av, bv = gridComp.sha(nlat, nlon, nt, lsha, wsha, vort)


        # --------------------- Vector Harmonic Synthesis ----------------------------

        wvhs, lvhs = gridComp.vhsi(nlat, nlon)

        u, v = gridComp.idvt(nlat, nlon, nt, ad, bd, av, bv, lvhs, wvhs)

        u, v = mathtogeo(reverseLatitude, standardShape, inverseOrder, u, v)

        return u, v 

    def igrad(self, u, v, missingValue = None):
        """    --------------------------------------------------------------------------------------------------------
         routine:   igrad 
                                               
         purpose:   computes a scalar function whose gradient is a given vector
                    function 
         
         usage:     sf = x.igrad(u, v, missingValue)
         
         passed:    u -- zonal vector function
                    v -- meridional vector function 
                    missingValue -- an optional number requesting a check for missing data
         
         returned:  sf --  a scalar function 
         
         definition: igrad(self, u, v, missingValue = None):
         
    --------------------------------------------------------------------------------------------------------""" 
        gridComp = self.gridComp
        nlon = self.lon
        nlat = self.lat
        standardShape = self.standardShape 
        reverseLatitude =  self.reverseLatitude 

        if usefilled == 'yes':
            u = numpy.ma.filled(u)
            v = numpy.ma.filled(v)

        nt, inverseOrder, u, v = geotomath(missingValue, reverseLatitude, standardShape, u, v)

        # --------------------- Vector Harmonic Analysis ----------------------------

        v = -1.0*v                                     # make + derivatives for function increasing in south to north direction
        v = v.astype(numpy.float32)

        wvha, lvha = gridComp.vhai(nlat, nlon)

        br, bi, cr, ci = gridComp.vha(nlat, nlon, nt, lvha, wvha, u, v)

        # ------------------------ Scalar Harmonic Synthesis --------------------------------

        wshs, lshs = gridComp.shsi(nlat, nlon)

        sf = gridComp.igrad(nlat, nlon, nt, lshs, wshs, br, bi)

        sf = mathtogeo(reverseLatitude, standardShape, inverseOrder, sf)

        scale = radius                                     # scale to radius of the earth
        sf = geoscale(scale, sf)

        return sf 

    def isfvp(self, sf, vp, missingValue = None):
        """    --------------------------------------------------------------------------------------------------------
         routine:   isfvp 
                                               
         purpose:   computes a vector function with a given stream function and
                    velocity potential 
         
         usage:     u, v = x.isfvp(sf, vp, missingValue):
         
         passed:    sf -- stream function on a global grid 
                    vp -- velocity potential on a global grid 
                    missingValue -- an optional number requesting a check for missing data
         
         returned:  u -- zonal vector function 
                    v -- meridional vector function 
         
         definition: isfvp(self, sf, vp, missingValue = None):
         
    --------------------------------------------------------------------------------------------------------""" 
        gridComp = self.gridComp
        nlon = self.lon
        nlat = self.lat
        standardShape = self.standardShape 
        reverseLatitude =  self.reverseLatitude 

        if usefilled == 'yes':
            sf = numpy.ma.filled(sf)
            vp = numpy.ma.filled(vp)

        nt, inverseOrder, sf = geotomath(missingValue, reverseLatitude, standardShape, sf)

        scale = 1.0/radius                                     # scale to unit radius of the earth
        sf = geoscale(scale, sf)

        nt, inverseOrder, vp = geotomath(missingValue, reverseLatitude, standardShape, vp)

        scale = 1.0/radius                                     # scale to unit radius of the earth
        vp = geoscale(scale, vp)

        # --------------------- Scalar Harmonic Analysis ----------------------------

        wsha, lsha = gridComp.shai(nlat, nlon)

        as_, bs = gridComp.sha(nlat, nlon, nt, lsha, wsha, sf)

        wsha, lsha = gridComp.shai(nlat, nlon)

        av, bv = gridComp.sha(nlat, nlon, nt, lsha, wsha, vp)

        # --------------------- Vector Harmonic Synthesis ----------------------------

        wvhs, lvhs = gridComp.vhsi(nlat, nlon)

        u, v = gridComp.isfvp(nlat, nlon, nt, as_, bs, av, bv, lvhs, wvhs)

        u, v = mathtogeo(reverseLatitude, standardShape, inverseOrder, u, v)

        return u, v 

    def islap(self, slap, missingValue = None):
        """    --------------------------------------------------------------------------------------------------------
         routine:   islap 
                                               
         purpose:   computes a scalar function whose scalar Laplacian is given 
         
         usage:     sf, ierror = x.islap(slap, missingValue):
         
         passed:    slap -- scalar Laplacian on a global grid 
                    missingValue -- an optional number requesting a check for missing data
         
         returned:  sf --  a scalar function 
         
        definition: islap(self, slap, missingValue = None):
         
    --------------------------------------------------------------------------------------------------------""" 
        gridComp = self.gridComp
        nlon = self.lon
        nlat = self.lat
        standardShape = self.standardShape 
        reverseLatitude =  self.reverseLatitude 

        if usefilled == 'yes':
            slap = numpy.ma.filled(slap)

        nt, inverseOrder, slap = geotomath(missingValue, reverseLatitude, standardShape, slap)

        scale = radius*radius                                     # scale to unit radius of the earth
        slap = geoscale(scale, slap)

        # --------------------- Scalar Harmonic Analysis ----------------------------

        wsha, lsha = gridComp.shai(nlat, nlon)

        a, b = gridComp.sha(nlat, nlon, nt, lsha, wsha, slap)

        # ------------------------ Scalar Harmonic Synthesis --------------------------------

        wshs, lshs = gridComp.shsi(nlat, nlon)

        sf = gridComp.islap( nlat, nlon, nt, lshs, wshs, a, b)

        sf = mathtogeo(reverseLatitude, standardShape, inverseOrder, sf)

        return sf 

    def ivlap(self, ulap, vlap, missingValue = None):
        """    --------------------------------------------------------------------------------------------------------
         routine:   ivlap 
                                               
         purpose:   computes a vector function whose Laplacian is a given vector
                    vector function 
         
         usage:     u, v = x.ivlap(ulap, vlap, missingValue)
                    missingValue -- an optional number requesting a check for missing data
         
         passed:    ulap -- zonal Laplacian vector function on a global grid 
                    vlap -- meridional Laplacian vector function on a global grid 
         
         returned:  u -- zonal vector function 
                    v -- meridional vector function 
         
         definition: ivlap(self, ulap, vlap, missingValue = None):
         
    --------------------------------------------------------------------------------------------------------""" 
        gridComp = self.gridComp
        nlon = self.lon
        nlat = self.lat
        standardShape = self.standardShape 
        reverseLatitude =  self.reverseLatitude 

        if usefilled == 'yes':
            ulap = numpy.ma.filled(ulap)
            vlap = numpy.ma.filled(vlap)

        nt, inverseOrder, ulap, vlap = geotomath(missingValue, reverseLatitude, standardShape, ulap, vlap)

        scale = radius*radius                                     # scale to unit radius of the earth
        ulap = geoscale(scale, ulap)
        vlap = geoscale(scale, vlap)

        # --------------------- Vector Harmonic Analysis ----------------------------

        wvha, lvha = gridComp.vhai(nlat, nlon)

        br, bi, cr, ci = gridComp.vha(nlat, nlon, nt, lvha, wvha, ulap, vlap)

        # --------------------- Vector Harmonic Synthesis ----------------------------

        wvhs, lvhs = gridComp.vhsi(nlat, nlon)

        u, v = gridComp.ivlap( nlat, nlon, nt, br, bi, cr, ci, lvhs, wvhs)

        u, v = mathtogeo(reverseLatitude, standardShape, inverseOrder, u, v)


        return u, v 

    def ivrt(self, vort, missingValue = None):
        """    --------------------------------------------------------------------------------------------------------
         routine:   ivrt 
                                               
         purpose:   computes a divergence-free vector function whose vorticity is
                    given 
         
         usage:     u, v = x.ivrt(vort, missingValue)
                    missingValue -- an optional number requesting a check for missing data
         
         passed:    vort -- vorticity on a global grid 
         
         returned:  u -- zonal vector function 
                    v -- meridional vector function 
         
         definition: ivrt(self, vort, missingValue = None):
         
    --------------------------------------------------------------------------------------------------------""" 
        gridComp = self.gridComp
        nlon = self.lon
        nlat = self.lat
        standardShape = self.standardShape 
        reverseLatitude =  self.reverseLatitude 

        if usefilled == 'yes':
            vort = numpy.ma.filled(vort)

        nt, inverseOrder, vort = geotomath(missingValue, reverseLatitude, standardShape, vort)

        scale = radius                                     # scale to unit radius of the earth
        vort = geoscale(scale, vort)

        # --------------------- Scalar Harmonic Analysis ----------------------------

        wsha, lsha = gridComp.shai(nlat, nlon)

        a, b = gridComp.sha(nlat, nlon, nt, lsha, wsha, vort)

        # --------------------- Vector Harmonic Synthesis ----------------------------

        wvhs, lvhs = gridComp.vhsi(nlat, nlon)

        u, v = gridComp.ivrt( nlat, nlon, nt, a, b, lvhs, wvhs)

        u, v = mathtogeo(reverseLatitude, standardShape, inverseOrder, u, v)

        return u, v 

    def sfvp(self, u, v, missingValue = None):
        """    --------------------------------------------------------------------------------------------------------
         routine:   sfvp 
                                               
         purpose:   computes the stream function and the velocity potential of a 
                    vector function 
         
         usage:     sf, vp = x.sfvp(u, v, missingValue)
         
         passed:    u -- zonal vector function on a global grid 
                    v -- meridional vector function on a global grid 
                    missingValue -- an optional number requesting a check for missing data
        
         returned:  sf -- stream function  
                    vp -- velocity potential  
         
        definition: sfvp(self, u, v, missingValue = None):
         
    --------------------------------------------------------------------------------------------------------""" 
        gridComp = self.gridComp
        nlon = self.lon
        nlat = self.lat
        standardShape = self.standardShape 
        reverseLatitude =  self.reverseLatitude 

        if usefilled == 'yes':
            u = numpy.ma.filled(u)
            v = numpy.ma.filled(v)

        nt, inverseOrder, u, v = geotomath(missingValue, reverseLatitude, standardShape, u, v)

        # --------------------- Vector Harmonic Analysis ----------------------------

        wvha, lvha = gridComp.vhai(nlat, nlon)

        br, bi, cr, ci = gridComp.vha(nlat, nlon, nt, lvha, wvha, u, v)

        # ------------------------ Scalar Harmonic Synthesis --------------------------------

        wshs, lshs = gridComp.shsi(nlat, nlon)

        sf, vp = gridComp.sfvp( nlat, nlon, nt, br, bi, cr, ci, lshs, wshs)

        sf = mathtogeo(reverseLatitude, standardShape, inverseOrder, sf)

        vp = mathtogeo(reverseLatitude, standardShape, inverseOrder, vp)

        scale = radius                                     # scale to radius of the earth
        sf, vp = geoscale(scale, sf, vp)

        return sf, vp 

    def sha(self, sf, missingValue = None):
        """    --------------------------------------------------------------------------------------------------------
         routine:   sha 
                                               
         purpose:   computes analysis coefficients for a scalar function
         
         usage:     a, b = x.sha(sf, missingValue)
                    missingValue -- an optional number requesting a check for missing data
         
         passed:    sf -- scalar function on global grid 
         
         returned:  a -- coefficients 
                    b -- coefficients 
         
         definition: sha(self, sf, missingValue = None):
         
   --------------------------------------------------------------------------------------------------------""" 
        gridComp = self.gridComp
        nlon = self.lon
        nlat = self.lat
        standardShape = self.standardShape 
        reverseLatitude =  self.reverseLatitude 

        if usefilled == 'yes':
            sf = numpy.ma.filled(sf)

        nt, inverseOrder, sf = geotomath(missingValue, reverseLatitude, standardShape, sf)
        self.inverseOrder = inverseOrder

        # ------------------------ Scalar Analysis --------------------------------

        wsha, lsha = gridComp.shai(nlat, nlon)

        a, b = gridComp.sha( nlat, nlon, nt, lsha, wsha, sf)

        if a.shape[0] == 1:
            a = numpy.reshape(a, (a.shape[1], a.shape[2]))
            b = numpy.reshape(b, (b.shape[1], b.shape[2]))

        return a, b 

    def shs(self, a, b):
        """    --------------------------------------------------------------------------------------------------------
         routine:   shs 
                                               
         purpose:   computes a scalar function from the coefficients
         
         usage:     sf = x.shs(a, b)
         
         passed:    a -- coefficients 
                    b -- coefficients 
         
         returned:  sf -- scalar function  
         
         definition: shs(self, a, b):
         
    --------------------------------------------------------------------------------------------------------""" 
        gridComp = self.gridComp
        nlon = self.lon
        nlat = self.lat
        standardShape = self.standardShape 
        reverseLatitude =  self.reverseLatitude 

        # ------------------------ Scalar Harmonic Synthesis --------------------------------

        wshs, lshs = gridComp.shsi(nlat, nlon)

        if len(a.shape) == 2:
            a = numpy.reshape(a, (1, a.shape[0], a.shape[1]))
            b = numpy.reshape(a, (1, b.shape[0], b.shape[1]))

        nt = a.shape[0]
        sf = gridComp.shs(nlat, nlon, nt, lshs, wshs, a, b)

        inverseOrder = self.inverseOrder 
        sf = mathtogeo(reverseLatitude, standardShape, inverseOrder, sf)

        return sf 

    def slap(self, sf, missingValue = None):
        """    --------------------------------------------------------------------------------------------------------
             routine:   slap 
                                               
             purpose:   computes a scalar Laplacian of a scalar function 
         
             usage:     slap = x.slap(self, sf, missingValue)
                        missingValue -- an optional number requesting a check for missing data
         
             passed:    sf -- scalar function on a global grid 
         
             returned:  slap -- scalar function 
         
             definition: slap(self, sf, missingValue = None):
         
    --------------------------------------------------------------------------------------------------------""" 
        gridComp = self.gridComp
        nlon = self.lon
        nlat = self.lat
        standardShape = self.standardShape 
        reverseLatitude =  self.reverseLatitude 

        if usefilled == 'yes':
            sf = numpy.ma.filled(sf)

        nt, inverseOrder, sf = geotomath(missingValue, reverseLatitude, standardShape, sf)

        # --------------------- Scalar Harmonic Analysis ----------------------------

        wsha, lsha = gridComp.shai(nlat, nlon)

        a, b = gridComp.sha(nlat, nlon, nt, lsha, wsha, sf)

        # ------------------------ Scalar Harmonic Synthesis --------------------------------

        wshs, lshs = gridComp.shsi(nlat, nlon)

        slap = gridComp.slap( nlat, nlon, nt, lshs, wshs, a, b)

        slap = mathtogeo(reverseLatitude, standardShape, inverseOrder, slap)

        scale = 1.0/(radius*radius)                                     # scale to radius of the earth
        slap = geoscale(scale, slap)

        return slap 


    def truncation(self, wave, u, v = None, taper = 'yes', missingValue = None):
        """    --------------------------------------------------------------------------------------------------------
         routine:  truncation 
         
         purpose:  performs a triangular truncation of a scalar or a vector function with 
                   or without tapering. For example, a request for T42 entails eliminating all
                   values for the total wavenumber above 42. The remaining values are tapered
                   by default.
         
         usage:    u, v = truncation(42, u, v)
                   u, v = truncation(wave, u, v)
                   u, v = truncation(wave, u, v, 'no', missingValue):
                            or
                   sf = truncation(42, sf):
                   sf = truncation(wave, sf):
                   sf = truncation(wave, sf, v, 'no', missingValue):
         
         passed:    wave - the truncation wave number. For example, a request for T42 is wave set
                               to 42 whick entails eliminating all values for the total wavenumber above 42. 
                        u -- zonal vector function on a global grid 
                        v -- meridional vector function on a global grid 
                            or
                        sf   -- a scalar with v = None  instead of u, v
                        taper - (optional) the values remaining after truncation are tapered if the default 'yes'
                                is not changed to 'no'.
                        missingValue -- an optional number requesting a check for missing data
                        
         returned:  u, v or sf
         
         definition: truncation(self, wave, u, v = None, taper = 'yes', missingValue = None):
         
    --------------------------------------------------------------------------------------------------------""" 
        gridComp = self.gridComp
        nlon = self.lon
        nlat = self.lat
        standardShape = self.standardShape 
        reverseLatitude =  self.reverseLatitude 


        if v is None:
            # --------------------- Scalar Harmonic Analysis ----------------------------

            if usefilled == 'yes':
                u = numpy.ma.filled(u)

            nt, inverseOrder, sf = geotomath(missingValue, reverseLatitude, standardShape, u)

            wsha, lsha = gridComp.shai(nlat, nlon)

            a, b = gridComp.sha( nlat, nlon, nt, lsha, wsha, sf)
    
            a, b = truncate(wave, a, b, taper)

            # ------------------------ Scalar Harmonic Synthesis --------------------------------

            wshs, lshs = gridComp.shsi(nlat, nlon)

            sf = gridComp.shs(nlat, nlon, nt, lshs, wshs, a, b)

            sf = mathtogeo(reverseLatitude, standardShape, inverseOrder, sf)

            return sf 

        else:
            # --------------------- Vector Harmonic Analysis ----------------------------

            if usefilled == 'yes':
                u = numpy.ma.filled(u)
                v = numpy.ma.filled(v)

            nt, inverseOrder, u, v = geotomath(missingValue, reverseLatitude, standardShape, u, v)

            wvha, lvha = gridComp.vhai(nlat, nlon)

            br, bi, cr, ci = gridComp.vha(nlat, nlon, nt, lvha, wvha, u, v)

            br, bi = truncate(wave, br, bi, taper)
            cr, ci = truncate(wave, cr, ci, taper)
            # --------------------- Vector Harmonic Synthesis ----------------------------

            wvhs, lvhs = gridComp.vhsi(nlat, nlon)

            u, v = gridComp.vhs( nlat, nlon, nt, br, bi, cr, ci, lvhs, wvhs)

            u, v = mathtogeo(reverseLatitude, standardShape, inverseOrder, u, v)

            return u, v 

    def vha(self, u, v, missingValue = None):
        """   --------------------------------------------------------------------------------------------------------
         routine:   vha 
                                               
         purpose: computes the vector harmonic analysis
         
         usage:   br, bi, cr, ci = x.vha(u, v, missingValue)
         
         passed:    u -- zonal vector function on a global grid 
                    v -- meridional vector function on a global grid 
                    missingValue -- an optional number requesting a check for missing data
         
         returned:  br -- coefficients 
                    bi -- coefficients 
                    cr -- coefficients 
                    ci -- coefficients 
         
         definition: vha(self, u, v, missingValue = None):
         
    --------------------------------------------------------------------------------------------------------""" 
        gridComp = self.gridComp
        nlon = self.lon
        nlat = self.lat
        standardShape = self.standardShape 
        reverseLatitude =  self.reverseLatitude 

        if usefilled == 'yes':
            u = numpy.ma.filled(u)
            v = numpy.ma.filled(v)

        nt, inverseOrder, u, v = geotomath(missingValue, reverseLatitude, standardShape, u, v)
        self.inverseOrder = inverseOrder

        # --------------------- Vector Harmonic Analysis ----------------------------

        wvha, lvha = gridComp.vhai(nlat, nlon)

        br, bi, cr, ci = gridComp.vha(nlat, nlon, nt, lvha, wvha, u, v)

        if br.shape[0] == 1:
            br = numpy.reshape(br, (br.shape[1], br.shape[2]))
            bi = numpy.reshape(bi, (bi.shape[1], bi.shape[2]))
            cr = numpy.reshape(cr, (cr.shape[1], cr.shape[2]))
            ci = numpy.reshape(ci, (ci.shape[1], ci.shape[2]))

        return br, bi, cr, ci 

    def vhs(self, br, bi, cr, ci):
        """    --------------------------------------------------------------------------------------------------------
         routine:   vhs 
                                               
         purpose: computes the vector harmonic synthesis 
         
         usage:   u, v = x.vhs(br, bi, cr, ci)
         
         passed:    br -- coefficients 
                    bi -- coefficients 
                    cr -- coefficients 
                    ci -- coefficients 
         
         returned:  u -- zonal vector function 
                    v -- meridional vector function 
         
         definition: vhs(self, br, bi, cr, ci):
         
    --------------------------------------------------------------------------------------------------------""" 
        gridComp = self.gridComp
        nlon = self.lon
        nlat = self.lat
        standardShape = self.standardShape 
        reverseLatitude =  self.reverseLatitude 

        # --------------------- Vector Harmonic Synthesis ----------------------------

        wvhs, lvhs = gridComp.vhsi(nlat, nlon)

        if len(br.shape) == 2:
            br = numpy.reshape(br, (1, br.shape[0], br.shape[1]))
            bi = numpy.reshape(bi, (1, bi.shape[0], bi.shape[1]))
            cr = numpy.reshape(cr, (1, cr.shape[0], cr.shape[1]))
            ci = numpy.reshape(ci, (1, ci.shape[0], ci.shape[1]))

        nt = br.shape[0]
        u, v = gridComp.vhs( nlat, nlon, nt, br, bi, cr, ci, lvhs, wvhs)

        inverseOrder = self.inverseOrder 
        u, v = mathtogeo(reverseLatitude, standardShape, inverseOrder, u, v)

        return u, v 


    def vlap(self, u, v, missingValue = None):
        """    --------------------------------------------------------------------------------------------------------
         routine: vlap 
                                              
         purpose: computes the vector Laplacian of a given vector function
         
         usage:   ulap, vlap = x.vlap(u, v, missingValue)
         
         passed:    u -- zonal vector function on a global grid 
                    v -- meridional vector function on a global grid 
                    missingValue -- an optional number requesting a check for missing data
         
         returned:  ulap -- zonal vector Laplacian function 
                    vlap -- meridional vector Laplacian function 
         
         definition: vlap(self, u, v, missingValue = None):
         
    --------------------------------------------------------------------------------------------------------""" 
        gridComp = self.gridComp
        nlon = self.lon
        nlat = self.lat
        standardShape = self.standardShape 
        reverseLatitude =  self.reverseLatitude 

        if usefilled == 'yes':
            u = numpy.ma.filled(u)
            v = numpy.ma.filled(v)

        nt, inverseOrder, u, v = geotomath(missingValue, reverseLatitude, standardShape, u, v)

        # --------------------- Vector Harmonic Analysis ----------------------------

        wvha, lvha = gridComp.vhai(nlat, nlon)

        br, bi, cr, ci = gridComp.vha(nlat, nlon, nt, lvha, wvha, u, v)

        # --------------------- Vector Harmonic Synthesis ----------------------------

        wvhs, lvhs = gridComp.vhsi(nlat, nlon)

        ulap, vlap = gridComp.vlap(nlat, nlon, nt, br, bi, cr, ci, lvhs, wvhs)

        ulap, vlap = mathtogeo(reverseLatitude, standardShape, inverseOrder, ulap, vlap)

        scale = 1.0/(radius*radius)                                     # scale to radius of the earth
        ulap, vlap = geoscale(scale, ulap, vlap)

        return ulap, vlap 

    def vrt(self, u, v, missingValue = None):
        """    --------------------------------------------------------------------------------------------------------
         routine:   vrt 
                                               
         purpose:   computes the scalar vorticity of a vector function 
         
         usage:     vort = x.vrt(u, v, missingValue) 
         
         passed:    u -- zonal vector function on a global grid 
                    v -- meridional vector function on a global grid 
                    missingValue -- an optional number requesting a check for missing data
         
         returned:  vort --  the vorticity of the vector function 
         
        definition: vrt(self, u, v, missingValue = None):
         
    --------------------------------------------------------------------------------------------------------""" 
        gridComp = self.gridComp
        nlon = self.lon
        nlat = self.lat
        standardShape = self.standardShape 
        reverseLatitude =  self.reverseLatitude 

        if usefilled == 'yes':
            u = numpy.ma.filled(u)
            v = numpy.ma.filled(v)

        nt, inverseOrder, u, v = geotomath(missingValue, reverseLatitude, standardShape, u, v)

        # --------------------- Vector Harmonic Analysis ----------------------------

        wvha, lvha = gridComp.vhai(nlat, nlon)

        br, bi, cr, ci = gridComp.vha(nlat, nlon, nt, lvha, wvha, u, v)

        # ------------------------ Scalar Harmonic Synthesis --------------------------------

        wshs, lshs = gridComp.shsi(nlat, nlon)

        vort = gridComp.vrt(nlat, nlon, nt, cr, ci, lshs, wshs)

        vort = mathtogeo(reverseLatitude, standardShape, inverseOrder, vort)

        scale = 1.0/radius                                     # scale to radius of the earth
        vort = geoscale(scale, vort)

        return vort 

    def vtsi(self):
        """    --------------------------------------------------------------------------------------------------------
         routine:   vtsi 
                                              
         purpose:   call vtsi to initailize work spaces for vts
         
         usage:     wvts, lwvts = x.vtsi()
         
         passed:    nothing 
         
         returned:  wvts -- work space array  
                    lvts -- length of work space
        --------------------------------------------------------------------------------------------------------""" 
        gridComp = self.gridComp
        nlon = self.lon
        nlat = self.lat

        # ------------------------ Vector Initialization --------------------------------

        wvts, lwvts = gridComp.vtsi(nlat, nlon)

        return wvts, lwvts 

    def vts(self, u, v, missingValue = None):
        """    --------------------------------------------------------------------------------------------------------
         routine:   vts 
                                               
         purpose:   computes the derivative of the vector function with respect
                    to latitude 
         
         usage:     ud, vd = x.vrt(u, v, missingValue) 
         
         passed:    u -- zonal vector function on a global grid 
                    v -- meridional vector function on a global grid 
                    missingValue -- an optional number requesting a check for missing data
         
         returned:  ud -- zonal vector function 
                    vd -- meridional vector function 
         
         definition: vts(self, u, v, missingValue = None):
         
    --------------------------------------------------------------------------------------------------------""" 
        gridComp = self.gridComp
        nlon = self.lon
        nlat = self.lat
        standardShape = self.standardShape 
        reverseLatitude =  self.reverseLatitude 

        if usefilled == 'yes':
            u = numpy.ma.filled(u)
            v = numpy.ma.filled(v)

        nt, inverseOrder, u, v = geotomath(missingValue, reverseLatitude, standardShape, u, v)

        # --------------------- Vector Harmonic Analysis ----------------------------

        wvha, lvha = gridComp.vhai(nlat, nlon)

        br, bi, cr, ci = gridComp.vha(nlat, nlon, nt, lvha, wvha, u, v)

        # --------------------- Vector Harmonic Synthesis ----------------------------

        wvts, lwvts = gridComp.vtsi(nlat, nlon)

        u, v = gridComp.vts( nlat, nlon, nt, br, bi, cr, ci, lwvts, wvts)

        u, v = mathtogeo(reverseLatitude, standardShape, inverseOrder, u, v)

        scale = 1.0/radius                                     # scale to radius of the earth
        u, v = geoscale(scale, u, v)

        u = -1.0*u                                  # make + derivatives for function increasing in south to north direction
        u = u.astype(numpy.float32)
        v = -1.0*v
        v = v.astype(numpy.float32)

        return u, v 

# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# +++++++++++++++++++++++++++++++ Equally spaced Case ++++++++++++++++++++++++++++++++++++++++++
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

class Wrapec:

    #-----------------------------------------------------------------------------
    #                                      
    #     purpose: provides a first layer 'gridComp' wrapper to assign the array
    #              the sizes. The final layer in class Sphere, seen by user, will call 
    #              associated intializations and preliminary functions and then
    #              return the result expected from the name of the call.
    #
    #     usage:  x  = Wrapec()       --  makes an instance
    #
    #-----------------------------------------------------------------------------


    def div(self, nlat, nlon, nt, br, bi, lshsec, wshsec):
        #-----------------------------------------------------------------------------
        #                                      
        #     purpose: computes the divergence of a vector function on a equally spaced
        #              grid
        #
        #     usage:   dv = x.div(nlat, nlon, nt, br, bi, wshsec, lshsec)
        #
        #-----------------------------------------------------------------------------

        if nlon%2:                              # nlon is odd
            n1 = min(nlat, (nlon + 1)/2)
        else:
            n1 = min(nlat, (nlon + 2)/2)

        if nlat%2:                              # nlat is odd
            n2 = (nlat + 1)/2
        else:
            n2 = nlat/2

        lwork = nlat*(nlon*nt + max(3*n2, nlon) + 2*nt*n1 + 1)

        isym = 0

        idv = nlat
        jdv = nlon

        if nlon%2:                              # nlon is odd
            n1 = min(nlat, (nlon + 1)/2)
        else:
            n1 = min(nlat, nlon/2)

        mdb = n1
        ndb = nlat

        work = numpy.zeros((lwork,),'f')
        dv, ierror = spherepack.divec(nlat, nlon, isym, idv, jdv,
                                      numpy.transpose(br),
                                      numpy.transpose(bi),
                                      wshsec, work)
        dv = numpy.transpose(dv)


        if ierror != 0 or debug == 1:
            print ' '
            print 'pass to divec'
            print 'nlon = ', nlon
            print 'nlat = ', nlat
            print 'isym = ', isym
            print 'nt = ', nt
            print 'idv = ', idv
            print 'jdv = ', jdv
            print 'mdb = ', mdb
            print 'ndb = ', ndb
            print 'lshsec = ', lshsec
            print 'lwork = ', lwork
            print 'return from divec with div'

        if ierror != 0:
            msg = 'In return from call to shaeci ierror =  %d' % (ierror,)
            raise ValueError, msg

        return dv 

    def grad(self, nlat, nlon, nt, a, b, lvhsec, wvhsec):
        #-----------------------------------------------------------------------------
        #                                      
        #     purpose: computes the gradient of a scalar function on a equally spaced grid
        #
        #     usage:   u, v = x.grad(nlat, nlon, nt, a, b, wvhsec, lvhsec)
        #
        #-----------------------------------------------------------------------------

        if nlon%2:                              # nlon is odd
            n1 = min(nlat, (nlon + 1)/2)
        else:
            n1 = min(nlat, nlon/2)

        if nlat%2:                              # nlat is odd
            n2 = (nlat + 1)/2
        else:
            n2 = nlat/2

        lwork = nlat*(2*nt*nlon + max(6*n2, nlon)) + nlat*(2*n1*nt + 1)

        isym = 0 

        idvw = nlat
        jdvw = nlon

        if nlon%2:                              # nlon is odd
            n1 = min(nlat, (nlon + 1)/2)
        else:
            n1 = min(nlat, (nlon + 2)/2)

        mdab = n1
        ndab = nlat

        # ---  call gradec ----
        work = numpy.zeros((lwork,),'f')
        v, w, ierror = spherepack.gradec(nlat, nlon, isym, idvw, jdvw,
                                         numpy.transpose(a),
                                         numpy.transpose(b),
                                         wvhsec, work)
        v = numpy.transpose(v)
        w = numpy.transpose(w)

        if ierror != 0 or debug == 1:
            print ' '
            print 'pass to gradec'
            print 'nlon = ', nlon
            print 'nlat = ', nlat
            print 'isym = ', isym
            print 'nt = ', nt
            print 'idvw = ', idvw
            print 'jdvw = ', jdvw
            print 'mdab = ', mdab
            print 'ndab = ', ndab
            print 'lvhsec = ', lvhsec
            print 'lwork = ', lwork
            print 'return from gradec with u, v'

        if ierror != 0:
            msg = 'In return from call to gradec ierror =  %d' % (ierror,)
            raise ValueError, msg

        return w, v 

    def idiv(self, nlat, nlon, nt, a, b, lvhsec, wvhsec):
        #-----------------------------------------------------------------------------
        #                                      
        #     purpose: computes an irrotational vector function whose divergence is
        #              given on a equally spaced grid.
        #
        #     usage:   u, v = x.idiv(nlat, nlon, nt, a, b, wvhsec, lvhsec)
        #
        #-----------------------------------------------------------------------------

        if nlon%2:                              # nlon is odd
            n1 = min(nlat, (nlon + 1)/2)
        else:
            n1 = min(nlat, nlon/2)

        if nlat%2:                              # nlat is odd
            n2 = (nlat + 1)/2
        else:
            n2 = nlat/2

        lwork = nlat*(2*nt*nlon + max(6*n2, nlon) + 2*nt*n1 + 1)

        isym = 0

        idvw = nlat
        jdvw = nlon

        if nlon%2:                              # nlon is odd
            n1 = min(nlat, (nlon + 1)/2)
        else:
            n1 = min(nlat, (nlon + 2)/2)

        mdab = n1
        ndab = nlat

        work = numpy.zeros((lwork,),'f')
        v, w, pertrb, ierror = spherepack.idivec(nlat, nlon, isym, idvw, jdvw,
                                                 numpy.transpose(a),
                                                 numpy.transpose(b),
                                                 wvhsec,
                                                 work)
        v = numpy.transpose(v)
        w = numpy.transpose(w)


        if ierror != 0 or debug == 1:
            print ' '
            print 'pass to idivec'
            print 'nlon = ', nlon
            print 'nlat = ', nlat
            print 'isym = ', isym
            print 'nt = ', nt
            print 'idvw = ', idvw
            print 'jdvw = ', jdvw
            print 'mdab = ', mdab
            print 'ndab = ', ndab
            print 'lvhsec = ', lvhsec
            print 'lwork = ', lwork
            print 'return from idivec with u, v'

        if ierror != 0:
            msg = 'In return from call to idivec ierror =  %d' % (ierror,)
            raise ValueError, msg

        return w, v 

    def idvt(self, nlat, nlon, nt, ad, bd, av, bv, lvhsec, wvhsec):
        #-----------------------------------------------------------------------------
        #                                      
        #     purpose: computes a vector function with given divergence and vorticity
        #              on a equally spaced grid.
        #
        #     usage:   u, v = x.idvt(nlat, nlon, nt, ad, bd, av, bv, wvhsec, lvhsec)
        #
        #-----------------------------------------------------------------------------

        if nlon%2:                              # nlon is odd
            n1 = min(nlat, (nlon + 1)/2)
        else:
            n1 = min(nlat, nlon/2)

        if nlat%2:                              # nlat is odd
            n2 = (nlat + 1)/2
        else:
            n2 = nlat/2

        lwork = nlat*(2*nt*nlon + max(6*n2, nlon)) + nlat*(4*n1*nt + 1)

        isym = 0

        idvw = nlat
        jdvw = nlon

        if nlon%2:                              # nlon is odd
            n1 = min(nlat, (nlon + 1)/2)
        else:
            n1 = min(nlat, (nlon + 2)/2)

        mdab = n1
        ndab = nlat

        work = numpy.zeros((lwork,),'f')
        v, w, pertbd, pertbv, ierror = spherepack.idvtec(nlat, nlon, isym, idvw, jdvw,
                                                         numpy.transpose(ad),
                                                         numpy.transpose(bd),
                                                         numpy.transpose(av),
                                                         numpy.transpose(bv),
                                                         wvhsec,work)
        v = numpy.transpose(v)
        w = numpy.transpose(w)

        if ierror != 0 or debug == 1:
            print ' '
            print 'pass to idvtec'
            print 'nlon = ', nlon
            print 'nlat = ', nlat
            print 'isym = ', isym
            print 'nt = ', nt
            print 'idvw = ', idvw
            print 'jdvw = ', jdvw
            print 'mdab = ', mdab
            print 'ndab = ', ndab
            print 'lvhsec = ', lvhsec
            print 'lwork = ', lwork
            print 'return from idvtec with u and v'

        if ierror != 0:
            msg = 'In return from call to idvtec ierror =  %d' % (ierror,)
            raise ValueError, msg

        return w, v 

    def igrad(self, nlat, nlon, nt, lshsec, wshsec, br, bi):
        #-----------------------------------------------------------------------------
        #                                      
        #     purpose: computes a scalar function whose gradient is a given vector
        #              function on a equally spaced grid
        #
        #     usage:   sf = x.igrad(nlat, nlon, nt, lshsec, wshsec, br, bi)
        #
        #-----------------------------------------------------------------------------

        if nlon%2:                              # nlon is odd
            n1 = min(nlat, (nlon + 1)/2)
        else:
            n1 = min(nlat, (nlon + 2)/2)

        if nlat%2:                              # nlat is odd
            n2 = (nlat + 1)/2
        else:
            n2 = nlat/2

        lwork = nlat*(nlon*nt + max(3*n2, nlon) + 2*nt*n1 + 1)

        isym = 0 

        isf = nlat
        jsf = nlon

        if nlon%2:                              # nlon is odd
            n1 = min(nlat, (nlon + 1)/2)
        else:
            n1 = min(nlat, nlon/2)

        mdb = n1
        ndb = nlat

        # ---  call gradec ----
        work = numpy.zeros((lwork,),'f')
        sf, ierror = spherepack.igradec(nlat, nlon, isym, isf, jsf,
                                        numpy.transpose(br),
                                        numpy.transpose(bi),
                                        wshsec, work)
        sf = numpy.transpose(sf)

        if ierror != 0 or debug == 1:
            print ' '
            print 'pass to igradec'
            print 'nlon = ', nlon
            print 'nlat = ', nlat
            print 'isym = ', isym
            print 'nt = ', nt
            print 'isf = ', isf
            print 'jsf = ', jsf
            print 'mdb = ', mdb
            print 'ndb = ', ndb
            print 'lshsec = ', lshsec
            print 'lwork = ', lwork
            print 'return from igradec with sf'

        if ierror != 0:
            msg = 'In return from call to igradec ierror =  %d' % (ierror,)
            raise ValueError, msg

        return sf 

    def isfvp(self, nlat, nlon, nt, as_, bs, av, bv, lvhsec, wvhsec):
        #-----------------------------------------------------------------------------
        #                                      
        #     purpose: computes a vector function with a given stream function and
        #              velocity potential on a equally spaced grid
        #
        #     usage:   u, v = x.isfvp(nlat, nlon, nt, as_, bs, av, bv, wvhsec, lvhsec)
        #
        #-----------------------------------------------------------------------------

        if nlon%2:                              # nlon is odd
            n1 = min(nlat, (nlon + 1)/2)
        else:
            n1 = min(nlat, nlon/2)

        if nlat%2:                              # nlat is odd
            n2 = (nlat + 1)/2
        else:
            n2 = nlat/2

        lwork = nlat*(2*nt*nlon + max(6*n2, nlon) + 4*n1*nt + 1)

        isym = 0

        idv = nlat
        jdv = nlon

        if nlon%2:                              # nlon is odd
            n1 = min(nlat, (nlon + 1)/2)
        else:
            n1 = min(nlat, (nlon + 2)/2)

        mdb = n1
        ndb = nlat

        work = numpy.zeros((lwork,),'f')
        v, w, ierror = spherepack.isfvpec(nlat, nlon, isym, idv, jdv,
                                          numpy.transpose(as_),
                                          numpy.transpose(bs),
                                          numpy.transpose(av),
                                          numpy.transpose(bv),
                                          wvhsec, work)
        v = numpy.transpose(v)
        w = numpy.transpose(w)

        if ierror != 0 or debug == 1:
            print ' '
            print 'pass to isfvpec'
            print 'nlon = ', nlon
            print 'nlat = ', nlat
            print 'isym = ', isym
            print 'nt = ', nt
            print 'idv = ', idv
            print 'jdv = ', jdv
            print 'mdb = ', mdb
            print 'ndb = ', ndb
            print 'lvhsec = ', lvhsec
            print 'lwork = ', lwork
            print 'return from isfvpec with u and v'

        if ierror != 0:
            msg = 'In return from call to isfvpec ierror =  %d' % (ierror,)
            raise ValueError, msg

        return w, v 

    def islap(self, nlat, nlon, nt, lshsec, wshsec, a, b):
        #-----------------------------------------------------------------------------
        #                                      
        #     purpose: computes a scalar function whose scalar Laplacian is given on
        #              a equally spaced grid
        #
        #     usage:   sf = x.islap(nlat, nlon, nt, lshsec, wshsec, a, b)
        #
        #-----------------------------------------------------------------------------

        if nlon%2:                              # nlon is odd
            n1 = min(nlat, (nlon + 1)/2)
        else:
            n1 = min(nlat, (nlon + 2)/2)

        if nlat%2:                              # nlat is odd
            n2 = (nlat + 1)/2
        else:
            n2 = nlat/2

        lwork = nlat*(2*nt*nlon + max(6*n2, nlon) + 2*nt*n1 + 1)

        isym = 0 

        ids = nlat
        jds = nlon

        mdab = n1
        ndab = nlat

        xlmbda = numpy.array( [0.0]*nt, numpy.float32)      # call for Poisson rather than Helmholtz

        # ---  call islapec ----
        work = numpy.zeros((lwork,),'f')
        sf, pertrb, ierror = spherepack.islapec(nlat, nlon, isym, xlmbda, ids, jds,
                                                numpy.transpose(a),
                                                numpy.transpose(b),
                                                wshsec, work)
        sf = numpy.transpose(sf)

        if ierror != 0 or debug == 1:
            print ' '
            print 'pass to islapec'
            print 'nlon = ', nlon
            print 'nlat = ', nlat
            print 'isym = ', isym
            print 'nt = ', nt
            print 'ids = ', ids
            print 'jds = ', jds
            print 'mdab = ', mdab
            print 'ndab = ', ndab
            print 'lshsec = ', lshsec
            print 'lwork = ', lwork
            print 'return from islapec with sf'

        if ierror != 0:
            msg = 'In return from call to islapec ierror =  %d' % (ierror,)
            raise ValueError, msg

        return sf 

    def ivlap(self, nlat, nlon, nt, br, bi, cr, ci, lvhsec, wvhsec):
        #-----------------------------------------------------------------------------
        #                                      
        #     purpose: computes a vector function whose Laplacian is a given vector
        #              vector function on a equally spaced grid
        #
        #     usage:   u, v = x.ivlap(nlat, nlon, nt, br, bi, cr, ci, wvhsec, lvhsec)
        #
        #-----------------------------------------------------------------------------

        if nlon%2:                              # nlon is odd
            n1 = min(nlat, (nlon + 1)/2)
        else:
            n1 = min(nlat, nlon/2)

        if nlat%2:                              # nlat is odd
            n2 = (nlat + 1)/2
        else:
            n2 = nlat/2

        lwork = nlat*(2*nt*nlon + max(6*n2, nlon)) + nlat*(4*nt*n1 + 1)

        ityp = 0

        idvw = nlat
        jdvw = nlon

        mdbc = n1
        ndbc = nlat

        work = numpy.zeros((lwork,),'f')
        v, w, ierror = spherepack.ivlapec(nlat, nlon, ityp, idvw, jdvw,
                                          numpy.transpose(br),
                                          numpy.transpose(bi),
                                          numpy.transpose(cr),
                                          numpy.transpose(ci),
                                          wvhsec, work)
        v = numpy.transpose(v)
        w = numpy.transpose(w)

        if ierror != 0 or debug == 1:
            print ' '
            print 'pass to ivlapec'
            print 'nlon = ', nlon
            print 'nlat = ', nlat
            print 'ityp = ', ityp
            print 'nt = ', nt
            print 'idvw = ', idvw
            print 'jdvw = ', jdvw
            print 'mdbc = ', mdbc
            print 'ndbc = ', ndbc
            print 'lvhsec = ', lvhsec
            print 'lwork = ', lwork
            print 'return from ivlapec with u and v'

        if ierror != 0:
            msg = 'In return from call to ivlapec ierror =  %d' % (ierror,)
            raise ValueError, msg

        return w, v 

    def ivrt(self, nlat, nlon, nt, a, b, lvhsec, wvhsec):
        #-----------------------------------------------------------------------------
        #                                      
        #     purpose: computes a divergence-free vector function whose vorticity is
        #              given on a equally spaced grid
        #
        #     usage:   w, v = x.ivrt(nlat, nlon, nt, a, b, wvhsec, lvhsec)
        #
        #-----------------------------------------------------------------------------

        if nlon%2:                              # nlon is odd
            n1 = min(nlat, (nlon + 1)/2)
        else:
            n1 = min(nlat, nlon/2)

        if nlat%2:                              # nlat is odd
            n2 = (nlat + 1)/2
        else:
            n2 = nlat/2

        lwork = nlat*(2*nt*nlon + max(6*n2, nlon) + 2*nt*n1 + 1)

        isym = 0

        idvw = nlat
        jdvw = nlon

        if nlon%2:                              # nlon is odd
            n1 = min(nlat, (nlon + 1)/2)
        else:
            n1 = min(nlat, (nlon + 2)/2)

        mdab = n1
        ndab = nlat

        work = numpy.zeros((lwork,),'f')
        v, w, pertrb, ierror = spherepack.ivrtec(nlat, nlon, isym, idvw, jdvw,
                                                 numpy.transpose(a),
                                                 numpy.transpose(b),
                                                 wvhsec, work)
        v = numpy.transpose(v)
        w = numpy.transpose(w)

        if ierror != 0 or debug == 1:
            print ' '
            print 'pass to ivrtec'
            print 'nlon = ', nlon
            print 'nlat = ', nlat
            print 'isym = ', isym
            print 'nt = ', nt
            print 'idvw = ', idvw
            print 'jdvw = ', jdvw
            print 'mdab = ', mdab
            print 'ndab = ', ndab
            print 'lvhsec = ', lvhsec
            print 'lwork = ', lwork
            print 'return from ivrtec with u, v'

        if ierror != 0:
            msg = 'In return from call to ivrtec ierror =  %d' % (ierror,)
            raise ValueError, msg

        return w, v 

    def sfvp(self, nlat, nlon, nt, br, bi, cr, ci, lshsec, wshsec):
        #-----------------------------------------------------------------------------
        #                                      
        #     purpose: computes the stream function and the velocity potential of a 
        #              vector function on a equally spaced grid
        #
        #     usage:   sf, vp = x.sfvp(nlat, nlon, nt, br, bi, cr, ci, wshsec, lshsec)
        #
        #-----------------------------------------------------------------------------

        if nlon%2:                              # nlon is odd
            n1 = min(nlat, (nlon + 1)/2)
        else:
            n1 = min(nlat, (nlon + 2)/2)

        if nlat%2:                              # nlat is odd
            n2 = (nlat + 1)/2
        else:
            n2 = nlat/2

        lwork = nlat*(nt*nlon + max(3*n2, nlon) + 2*n1*nt + 1)

        isym = 0

        idv = nlat
        jdv = nlon

        if nlon%2:                              # nlon is odd
            n1 = min(nlat, (nlon + 1)/2)
        else:
            n1 = min(nlat, nlon/2)

        mdb = n1
        ndb = nlat

        work = numpy.zeros((lwork,),'f')
        sf, vp, ierror = spherepack.sfvpec(nlat, nlon, isym, idv, jdv,
                                           numpy.transpose(br),
                                           numpy.transpose(bi),
                                           numpy.transpose(cr),
                                           numpy.transpose(ci),
                                           wshsec, work)
        sf = numpy.transpose(sf)
        vp = numpy.transpose(vp)

        if ierror != 0 or debug == 1:
            print ' '
            print 'pass to sfvpec'
            print 'nlon = ', nlon
            print 'nlat = ', nlat
            print 'isym = ', isym
            print 'nt = ', nt
            print 'idv = ', idv
            print 'jdv = ', jdv
            print 'mdb = ', mdb
            print 'ndb = ', ndb
            print 'lshsec = ', lshsec
            print 'lwork = ', lwork
            print 'return from sfvpec with sf and vp'

        if ierror != 0:
            msg = 'In return from call to sfvpec ierror =  %d' % (ierror,)
            raise ValueError, msg

        return sf, vp 

    def shai(self, nlat,nlon):
        #-----------------------------------------------------------------------------
        #                                      
        #     purpose:  call shai for wshaec
        #
        #     usage:  wsha, lsha = x.shaeci(nlat, nlon)
        #
        #-----------------------------------------------------------------------------

        if nlon%2:                              # nlon is odd
            n1 = min(nlat, (nlon + 1)/2)
        else:
            n1 = min(nlat, (nlon + 2)/2)


        if nlat%2:                              # nlat is odd
            n2 = (nlat + 1)/2
        else:
            n2 = nlat/2

        lshaec = 2*nlat*n2 + 3*((n1 -2)*(nlat + nlat - n1 -1))/2 + nlon + 15
        ldwork = nlat + 1

        # ---  call shaeci ----
        work = numpy.zeros((ldwork,),'d')
        wshaec, ierror = spherepack.shaeci(nlat, nlon, lshaec, work)

        if ierror != 0 or debug == 1:
            print ' '
            print 'pass to shaeci'
            print 'nlon = ', nlon
            print 'nlat = ', nlat
            print 'lshaec = ', lshaec
            print 'ldwork = ', ldwork
            print 'return from shaeci with wshaec and lshaec'

        if ierror != 0:
            msg = 'In return from call to shaeci ierror =  %d' % (ierror,)
            raise ValueError, msg

        return wshaec, lshaec 

    def sha(self, nlat, nlon, nt, lshaec, wshaec, g):
        #-----------------------------------------------------------------------------
        #                                      
        #     purpose:  call sha for coefficients
        #
        #     usage:  a, b = x.sha(nlat, nlon, lshaec, wshaec, g)
        #
        #-----------------------------------------------------------------------------

        if nlon%2:                              # nlon is odd
            n1 = min(nlat, (nlon + 1)/2)
        else:
            n1 = min(nlat, (nlon + 2)/2)

        if nlat%2:                              # nlat is odd
            n2 = (nlat + 1)/2
        else:
            n2 = nlat/2

        lwork = nlat*(nt*nlon + max(3*n2, nlon))

        isym = 0

        idg = nlat
        jdg = nlon

        mdab = n1
        ndab = nlat

        # ---  call shaec ----
        work = numpy.zeros((lwork,),'f')
        a, b, ierror = spherepack.shaec(nlat, nlon, isym,
                                        numpy.transpose(g),
                                        mdab, ndab,
                                        wshaec, work)
        a = numpy.transpose(a)
        b = numpy.transpose(b)
        

        if ierror != 0 or debug == 1:
            print ' '
            print 'pass to shaec'
            print 'nlon = ', nlon
            print 'nlat = ', nlat
            print 'isym = ', isym
            print 'nt = ', nt
            print 'idg = ', idg
            print 'jdg = ', jdg
            print 'mdab = ', mdab
            print 'ndab = ', ndab
            print 'lshaec = ', lshaec
            print 'lwork = ', lwork
            print 'return from shaec with a, b'

        if ierror != 0:
            msg = 'In return from call to shaec ierror =  %d' % (ierror,)
            raise ValueError, msg

        return a, b 

    def shsi(self, nlat, nlon):
        #-----------------------------------------------------------------------------
        #                                      
        #     purpose:  call shsi for wshsec
        #
        #     usage:  wshs, lshs = x.shsi(nlat, nlon)
        #
        #-----------------------------------------------------------------------------

        if nlon%2:                              # nlon is odd
            n1 = min(nlat, (nlon + 1)/2)
        else:
            n1 = min(nlat, (nlon + 2)/2)

        if nlat%2:                              # nlat is odd
            n2 = (nlat + 1)/2
        else:
            n2 = nlat/2


        lshsec = 2*nlat*n2 + 3*((n1 -2)*(nlat + nlat - n1 -1))/2 + nlon + 15

        ldwork = nlat + 1

        # ---  call shseci ----
        work = numpy.zeros((ldwork,),'d')
        wshsec, ierror = spherepack.shseci(nlat, nlon, lshsec, work)

        if ierror != 0 or debug == 1:
            print ' '
            print 'pass to shseci'
            print 'nlon = ', nlon
            print 'nlat = ', nlat
            print 'lshsec = ', lshsec
            print 'ldwork = ', ldwork
            print 'return from shseci with wshsec and lshsec'

        if ierror != 0:
            msg = 'In return from call to shseci ierror =  %d' % (ierror,)
            raise ValueError, msg

        return wshsec, lshsec 

    def shs(self, nlat, nlon, nt, lshsec, wshsec, a, b):
        #-----------------------------------------------------------------------------
        #                                      
        #     purpose: computes the spherical harmonic synthesis on an evenly spaced grid
        #
        #     usage:   g = x.shs(nlat,nlon, nt, lshsec, wshsec, a, b)
        #
        #-----------------------------------------------------------------------------

        if nlon%2:                              # nlon is odd
            n1 = min(nlat, (nlon + 1)/2)
        else:
            n1 = min(nlat, (nlon + 2)/2)

        if nlat%2:                              # nlat is odd
            n2 = (nlat + 1)/2
        else:
            n2 = nlat/2

        lwork = nlat*(nt*nlon + max(3*n2, nlon))

        isym = 0 

        idg = nlat
        jdg = nlon

        mdab = n1
        ndab = nlat

        # ---  call shsec ----
        work = numpy.zeros((lwork,),'f')
        g, ierror = spherepack.shsec(nlat, nlon, isym, idg, jdg,
                                     numpy.transpose(a),
                                     numpy.transpose(b),
                                     wshsec, work)
        g = numpy.transpose(g)

        if ierror != 0 or debug == 1:
            print ' '
            print 'pass to shsec'
            print 'nlon = ', nlon
            print 'nlat = ', nlat
            print 'isym = ', isym
            print 'nt = ', nt
            print 'idg = ', idg
            print 'jdg = ', jdg
            print 'mdab = ', mdab
            print 'ndab = ', ndab
            print 'lshsec = ', lshsec
            print 'lwork = ', lwork
            print 'return from shsec with g'

        if ierror != 0:
            msg = 'In return from call to shsec ierror =  %d' % (ierror,)
            raise ValueError, msg

        return g 

    def slap(self, nlat, nlon, nt, lshsec, wshsec, a, b):
        #-----------------------------------------------------------------------------
        #                                      
        #     purpose: computes a scalar Laplacian of a scalar function on a equally spaced
        #              grid
        #
        #     usage:   slap = x.slap(nlat, nlon, nt, lshsec, wshsec, a, b)
        #
        #-----------------------------------------------------------------------------

        if nlon%2:                              # nlon is odd
            n1 = min(nlat, (nlon + 1)/2)
        else:
            n1 = min(nlat, (nlon + 2)/2)

        if nlat%2:                              # nlat is odd
            n2 = (nlat + 1)/2
        else:
            n2 = nlat/2

        lwork = nlat*(2*nt*nlon + max(6*n2, nlon) + 2*nt*n1 + 1)

        isym = 0 

        ids = nlat
        jds = nlon

        mdab = n1
        ndab = nlat

        # ---  call slapec ----
        work = numpy.zeros((lwork,),'f')
        slap, ierror = spherepack.slapec(nlat, nlon, isym, ids, jds,
                                         numpy.transpose(a),
                                         numpy.transpose(b),
                                         wshsec, work)
        slap = numpy.transpose(slap)
        

        if ierror != 0 or debug == 1:
            print ' '
            print 'pass to slapec'
            print 'nlon = ', nlon
            print 'nlat = ', nlat
            print 'isym = ', isym
            print 'nt = ', nt
            print 'ids = ', ids
            print 'jds = ', jds
            print 'mdab = ', mdab
            print 'ndab = ', ndab
            print 'lshsec = ', lshsec
            print 'lwork = ', lwork
            print 'return from slapec with slap'

        if ierror != 0:
            msg = 'In return from call to slapec ierror =  %d' % (ierror,)
            raise ValueError, msg

        return slap 

    def vhai(self, nlat, nlon):
        #-----------------------------------------------------------------------------
        #                                      
        #     purpose:  call vhai for wvhaec
        #
        #     usage:  wvha, lvha = x.vhai(nlat, nlon)
        #
        #
        #-----------------------------------------------------------------------------

        if nlon%2:                              # nlon is odd
            n1 = min(nlat, (nlon + 1)/2)
        else:
            n1 = min(nlat, nlon/2)

        if nlat%2:                              # nlat is odd
            n2 = (nlat + 1)/2
        else:
            n2 = nlat/2

        lvhaec = 4*nlat*n2 + 3*max(n1 - 2, 0)*(2*nlat - n1 - 1) + nlon + 15
        ldwork = 2*(nlat + 2)

        # ---  call vhaeci ----
        work = numpy.zeros((ldwork,),'d')
        wvhaec, ierror = spherepack.vhaeci(nlat, nlon, lvhaec, work)

        if ierror != 0 or debug == 1:
            print ' '
            print 'pass to vhaeci'
            print 'nlon = ', nlon
            print 'nlat = ', nlat
            print 'lvhaec = ', lvhaec
            print 'ldwork = ', ldwork
            print 'return from vhaeci with wvhaec and lvhaec'

        if ierror != 0:
            msg = 'In return from call to vhaeci ierror =  %d' % (ierror,)
            raise ValueError, msg

        return wvhaec, lvhaec 

    def vha(self, nlat, nlon, nt, lvhaec, wvhaec, w, v):
        #-----------------------------------------------------------------------------
        #                                      
        #     purpose: computes the vector harmonic analysis on a equally spaced grid
        #
        #     usage:   br, bi, cr, ci  = x.vha(nlat, nlon, nt, lvhaec, wvhaec, w, v)
        #
        #-----------------------------------------------------------------------------

        if nlon%2:                              # nlon is odd
            n1 = min(nlat, (nlon + 1)/2)
        else:
            n1 = min(nlat, nlon/2)

        if nlat%2:                              # nlat is odd
            n2 = (nlat + 1)/2
        else:
            n2 = nlat/2

        lwork = nlat*(2*nt*nlon + max(6*n2,nlon))

        ityp = 0

        idvw = nlat
        jdvw = nlon

        mdab = n1
        ndab = nlat

        # ---  call vhaec ----
        work = numpy.zeros((lwork,),'f')
        br, bi, cr, ci, ierror = spherepack.vhaec(nlat, nlon, ityp,
                                                  numpy.transpose(v),
                                                  numpy.transpose(w),
                                                  mdab, ndab,
                                                  wvhaec, work)
        br = numpy.transpose(br)
        bi = numpy.transpose(bi)
        cr = numpy.transpose(cr)
        ci = numpy.transpose(ci)

        if ierror != 0 or debug == 1:
            print ' '
            print 'pass to vhaec'
            print 'nlon = ', nlon
            print 'nlat = ', nlat
            print 'ityp = ', ityp
            print 'nt = ', nt
            print 'idvw = ', idvw
            print 'jdvw = ', jdvw
            print 'mdab = ', mdab
            print 'ndab = ', ndab
            print 'lvhaec = ', lvhaec
            print 'lwork = ', lwork
            print 'return from vhaec with br,bi,cr,ci'

        if ierror != 0:
            msg = 'In return from call to vhaec ierror =  %d' % (ierror,)
            raise ValueError, msg

        return br, bi, cr, ci 

    def vhsi(self, nlat,nlon):
        #-----------------------------------------------------------------------------
        #                                      
        #     purpose:  call vhseci for wvhsec
        #
        #     usage:  wvhs, lvhs = x.vhsi(nlat, nlon)
        #
        #-----------------------------------------------------------------------------

        if nlon%2:                              # nlon is odd
            n1 = min(nlat, (nlon + 1)/2)
        else:
            n1 = min(nlat, nlon/2)

        if nlat%2:                              # nlat is odd
            n2 = (nlat + 1)/2
        else:
            n2 = nlat/2

        lvhsec = 4*nlat*n2 + 3*max(n1 - 2, 0)*(2*nlat - n1 -1) + nlon + 15
        ldwork = 2*(nlat + 2)

        # ---  call vhseci ----
        work = numpy.zeros((ldwork,),'d')
        wvhsec, ierror = spherepack.vhseci(nlat, nlon, lvhsec, work)

        if ierror != 0 or debug == 1:
            print ' '
            print 'pass to vhseci'
            print 'nlon = ', nlon
            print 'nlat = ', nlat
            print 'lvhsec = ', lvhsec
            print 'ldwork = ', ldwork
            print 'return from vhseci with wvhsec and lvhsec'

        if ierror != 0:
            msg = 'In return from call to vhseci ierror =  %d' % (ierror,)
            raise ValueError, msg

        return wvhsec, lvhsec 

    def vhs(self, nlat, nlon, nt, br, bi, cr, ci, lvhsec, wvhsec):
        #-----------------------------------------------------------------------------
        #                                      
        #     purpose: computes the vector harmonic synthesis on a equally spaced grid
        #
        #     usage:   w, v  = x.vhs(nlat, nlon, nt, br, bi, cr, ci, wvhsec, lvhsec)
        #
        #-----------------------------------------------------------------------------

        if nlon%2:                              # nlon is odd
            n1 = min(nlat, (nlon + 1)/2)
        else:
            n1 = min(nlat, nlon/2)

        if nlat%2:                              # nlat is odd
            n2 = (nlat + 1)/2
        else:
            n2 = nlat/2

        lwork = nlat*(2*nt*nlon + max(6*n2, nlon))

        ityp = 0

        idvw = nlat
        jdvw = nlon

        mdab = n1
        ndab = nlat

        work = numpy.zeros((lwork,),'f')
        v, w, ierror = spherepack.vhsec(nlat, nlon, ityp, idvw, jdvw,
                                        numpy.transpose(br),
                                        numpy.transpose(bi),
                                        numpy.transpose(cr),
                                        numpy.transpose(ci),
                                        wvhsec, work)

        v = numpy.transpose(v)
        w = numpy.transpose(w)

        if ierror != 0 or debug == 1:
            print ' '
            print 'pass to vhsec'
            print 'nlon = ', nlon
            print 'nlat = ', nlat
            print 'ityp = ', ityp
            print 'nt = ', nt
            print 'idvw = ', idvw
            print 'jdvw = ', jdvw
            print 'mdab = ', mdab
            print 'ndab = ', ndab
            print 'lvhsec = ', lvhsec
            print 'lwork = ', lwork
            print 'return from vhsec with u and v'

        if ierror != 0:
            msg = 'In return from call to vhsec ierror =  %d' % (ierror,)
            raise ValueError, msg

        return w, v 

    def vlap(self, nlat, nlon, nt, br, bi, cr, ci, lvhsec, wvhsec):
        #-----------------------------------------------------------------------------
        #                                      
        #     purpose: computes the vector Laplacian of a given vector function
        #              on a equally spaced grid
        #
        #     usage:   u, v = x.vlap(nlat, nlon, nt, br, bi, cr, ci, lvhsec, wvhsec)
        #
        #-----------------------------------------------------------------------------


        if nlon%2:                              # nlon is odd
            n1 = min(nlat, (nlon + 1)/2)
        else:
            n1 = min(nlat, (nlon + 2)/2)

        if nlat%2:                              # nlat is odd
            n2 = (nlat + 1)/2
        else:
            n2 = nlat/2

        lwork = nlat*(2*nt*nlon + max(6*n2, nlon)) + nlat*(4*nt*n1 + 1)

        ityp = 0

        idvw = nlat
        jdvw = nlon

        if nlon%2:                              # nlon is odd
            n1 = min(nlat, (nlon + 1)/2)
        else:
            n1 = min(nlat, nlon/2)

        mdbc = n1
        ndbc = nlat

        work = numpy.zeros((lwork,),'f')
        v, w, ierror = spherepack.vlapec(nlat, nlon, ityp, idvw, jdvw,
                                         numpy.transpose(br),
                                         numpy.transpose(bi),
                                         numpy.transpose(cr),
                                         numpy.transpose(ci),
                                         wvhsec, work)
        v = numpy.transpose(v)
        w = numpy.transpose(w)


        if ierror != 0 or debug == 1:
            print ' '
            print 'pass to vlapec'
            print 'nlon = ', nlon
            print 'nlat = ', nlat
            print 'ityp = ', ityp
            print 'nt = ', nt
            print 'idvw = ', idvw
            print 'jdvw = ', jdvw
            print 'mdbc = ', mdbc
            print 'ndbc = ', ndbc
            print 'lvhsec = ', lvhsec
            print 'lwork = ', lwork
            print 'return from vlapec with u and v'

        if ierror != 0:
            msg = 'In return from call to vlapec ierror =  %d' % (ierror,)
            raise ValueError, msg

        return w, v 

    def vrt(self, nlat, nlon, nt, cr, ci, lshsec, wshsec):
        #-----------------------------------------------------------------------------
        #                                      
        #     purpose: computes the scalar vorticity of a vector function on an
        #              equally spaced grid
        #
        #     usage:   vort = x.vrt(nlat, nlon, nt, cr, ci, lshsec, wshsec)
        #
        #-----------------------------------------------------------------------------

        if nlon%2:                              # nlon is odd
            n1 = min(nlat, (nlon + 1)/2)
        else:
            n1 = min(nlat, nlon/2)

        if nlat%2:                              # nlat is odd
            n2 = (nlat + 1)/2
        else:
            n2 = nlat/2

        isym = 0

        ivrt = nlat
        jvrt = nlon

        mdc = n1
        ndc = nlat

        n1 = min(nlat, (nlon + 2)/2)                           # note: error in sizes in vrtec.f comment section 
        n2 = (nlat + 1)/2
        lwork = nlat*(nt*nlon + max(3*n2, nlon) + 2*nt*n1 + 1)

        work = numpy.zeros((lwork,),'f')
        vort, ierror = spherepack.vrtec(nlat, nlon, isym, ivrt, jvrt,
                                        numpy.transpose(cr),
                                        numpy.transpose(ci),
                                        wshsec, work)
        vort = numpy.transpose(vort)

        if ierror != 0 or debug == 1:
            print ' '
            print 'pass to vortec'
            print 'nlon = ', nlon
            print 'nlat = ', nlat
            print 'isym = ', isym
            print 'nt = ', nt
            print 'ivrt = ', ivrt
            print 'jvrt = ', jvrt
            print 'mdc = ', mdc
            print 'ndc = ', ndc
            print 'lshsec = ', lshsec
            print 'lwork = ', lwork
            print 'return from vrtec with vort'

        if ierror != 0:
            msg = 'In return from call to vrtec ierror =  %d' % (ierror,)
            raise ValueError, msg

        return vort 

    def vtsi(self, nlat, nlon):
        #-----------------------------------------------------------------------------
        #                                      
        #     purpose: initializes wvts for vtsec
        #
        #     usage:   wvts, lwvts = x.vtsi(nlat, nlon)
        #
        #-----------------------------------------------------------------------------

        if nlon%2:                              # nlon is odd
            n1 = min(nlat, (nlon + 1)/2)
        else:
            n1 = min(nlat, nlon/2)

        if nlat%2:                              # nlat is odd
            n2 = (nlat + 1)/2
        else:
            n2 = nlat/2

        lwvts = 4*nlat*n2 + 3*max(n1 - 2, 0)*(nlat + nlat - n1 - 1) + nlon + 15
        ldwork = nlat*(nlat + 4) 

        # ---  call vtseci ----
        work = numpy.zeros((ldwork,),'d')
        wvts, ierror = spherepack.vtseci(nlat, nlon, lwvts, work)

        if ierror != 0 or debug == 1:
            print ' '
            print 'pass to vtseci'
            print 'nlon = ', nlon
            print 'nlat = ', nlat
            print 'lwvts = ', lwvts
            print 'ldwork = ', ldwork
            print 'return from vtseci with wvts and lwvts'

        if ierror != 0:
            msg = 'In return from call to vtseci ierror =  %d' % (ierror,)
            raise ValueError, msg

        return wvts, lwvts 

    def vts(self, nlat, nlon, nt, br, bi, cr, ci, lwvts, wvts):
        #-----------------------------------------------------------------------------
        #                                      
        #     purpose: computes the derivative of the vector function with respect
        #              to latitude on a equally spaced  grid
        #
        #     usage:   u, v  = x.vts(nlat, nlon, nt, br, bi, cr, ci, lwvts, wvts)
        #
        #-----------------------------------------------------------------------------

        if nlon%2:                              # nlon is odd
            n1 = min(nlat, (nlon + 1)/2)
        else:
            n1 = min(nlat, nlon/2)

        if nlat%2:                              # nlat is odd
            n2 = (nlat + 1)/2
        else:
            n2 = nlat/2

        lwork = nlat*(2*nt*nlon + max(6*n2, nlon))

        ityp = 0

        idvw = nlat
        jdvw = nlon

        mdab = n1
        ndab = nlat

        work = numpy.zeros((lwork,),'f')
        v, w, ierror = spherepack.vtsec(nlat, nlon, ityp, idvw, jdvw,
                                        numpy.transpose(br),
                                        numpy.transpose(bi),
                                        numpy.transpose(cr),
                                        numpy.transpose(ci),
                                        wvts, work)
        v = numpy.transpose(v)
        w = numpy.transpose(w)

        if ierror != 0 or debug == 1:
            print ' '
            print 'pass to vtsec'
            print 'nlon = ', nlon
            print 'nlat = ', nlat
            print 'ityp = ', ityp
            print 'nt = ', nt
            print 'idvw = ', idvw
            print 'jdvw = ', jdvw
            print 'mdab = ', mdab
            print 'ndab = ', ndab
            print 'lwvts = ', lwvts
            print 'lwork = ', lwork
            print 'return from vtsec with u and v'

        if ierror != 0:
            msg = 'In return from call to vtsec ierror =  %d' % (ierror,)
            raise ValueError, msg

        return w, v 

# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# +++++++++++++++++++++++++++++++ Gaussian Case ++++++++++++++++++++++++++++++++++++++++++
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

class Wrapgc:
    #----------------------------------------------------------------------------------
    #                                      
    #     purpose: provides a first layer 'gridComp' wrapper to assign the array
    #              sizes. The final layer in class Sphere, seen by user,  will call 
    #              associated intializations and preliminary functions and then
    #              return the result expected from the name of the call.
    #
    #     usage:  g = Wrapgc()       --  makes an instance
    #
    #----------------------------------------------------------------------------------

    def div(self, nlat, nlon, nt, br, bi, lshsgc, wshsgc):
        #-----------------------------------------------------------------------------
        #                                      
        #     purpose: computes the divergence of a vector function on a gaussian
        #              grid
        #
        #     usage:   dv = g.div(nlat, nlon, nt, br, bi, lshsgc, wshsgc)
        #
        #-----------------------------------------------------------------------------

        if nlon%2:                              # nlon is odd
            n1 = min(nlat, (nlon + 1)/2)
        else:
            n1 = min(nlat, (nlon + 2)/2)

        if nlat%2:                              # nlat is odd
            n2 = (nlat + 1)/2
        else:
            n2 = nlat/2

        lwork = nlat*(nlon*nt + max(3*n2, nlon) + 2*nt*n1 + 1)

        isym = 0

        idv = nlat
        jdv = nlon

        if nlon%2:                              # nlon is odd
            n1 = min(nlat, (nlon + 1)/2)
        else:
            n1 = min(nlat, nlon/2)

        mdb = n1
        ndb = nlat
        
        work = numpy.zeros((lwork,),'f')
        dv, ierror = spherepack.divgc(nlat, nlon, isym, idv, jdv,
                                      numpy.transpose(br),
                                      numpy.transpose(bi),
                                      wshsgc,work)
        dv = numpy.transpose(dv)


        if ierror != 0 or debug == 1:
            print ' '
            print 'pass to divgc'
            print 'nlon = ', nlon
            print 'nlat = ', nlat
            print 'isym = ', isym
            print 'nt = ', nt
            print 'idv = ', idv
            print 'jdv = ', jdv
            print 'mdb = ', mdb
            print 'ndb = ', ndb
            print 'lshsgc = ', lshsgc
            print 'lwork = ', lwork
            print 'return from divgc with div'

        if ierror != 0:
            msg = 'In return from call to divgc ierror =  %d' % (ierror,)
            raise ValueError, msg

        return dv 

    def grad(self, nlat, nlon, nt, a, b, lvhsgc, wvhsgc):
        #-----------------------------------------------------------------------------
        #                                      
        #     purpose: computes the gradient of a scalar function on a gaussian grid
        #
        #     usage:   u, v = g.grad(nlat, nlon, nt, a, b, lvhsgc, wvhsgc)
        #
        #-----------------------------------------------------------------------------

        if nlon%2:                              # nlon is odd
            n1 = min(nlat, (nlon + 1)/2)
        else:
            n1 = min(nlat, nlon/2)

        if nlat%2:                              # nlat is odd
            n2 = (nlat + 1)/2
        else:
            n2 = nlat/2

        lwork = nlat*(2*nt*nlon + max(6*n2, nlon)) + nlat*(2*n1*nt + 1)

        isym = 0 

        idvw = nlat
        jdvw = nlon

        if nlon%2:                              # nlon is odd
            n1 = min(nlat, (nlon + 1)/2)
        else:
            n1 = min(nlat, (nlon + 2)/2)

        mdab = n1
        ndab = nlat

        # ---  call gradgc ----
        work = numpy.zeros((lwork,),'f')
        v, w, ierror = spherepack.gradgc(nlat, nlon, isym, idvw, jdvw,
                                         numpy.transpose(a),
                                         numpy.transpose(b),
                                         wvhsgc, work)
        v = numpy.transpose(v)
        w = numpy.transpose(w)

        if ierror != 0 or debug == 1:
            print ' '
            print 'pass to gradgc'
            print 'nlon = ', nlon
            print 'nlat = ', nlat
            print 'isym = ', isym
            print 'nt = ', nt
            print 'idvw = ', idvw
            print 'jdvw = ', jdvw
            print 'mdab = ', mdab
            print 'ndab = ', ndab
            print 'lvhsgc = ', lvhsgc
            print 'lwork = ', lwork
            print 'return from gradgc with u, v'

        if ierror != 0:
            msg = 'In return from call to gradgc ierror =  %d' % (ierror,)
            raise ValueError, msg

        return w, v 

    def idiv(self, nlat, nlon, nt, a, b, lvhsgc, wvhsgc):
        #-----------------------------------------------------------------------------
        #                                      
        #     purpose: computes an irrotational vector function whose divergence is
        #              given on a gaussian grid.
        #
        #     usage:   u, v = g.idiv(nlat, nlon, nt, a, b, wvhsgc, lvhsgc)
        #
        #-----------------------------------------------------------------------------

        if nlon%2:                              # nlon is odd
            n1 = min(nlat, (nlon + 1)/2)
        else:
            n1 = min(nlat, nlon/2)

        if nlat%2:                              # nlat is odd
            n2 = (nlat + 1)/2
        else:
            n2 = nlat/2

        lwork = nlat*(2*nt*nlon + max(6*n2, nlon) + 2*nt*n1 + 1)

        isym = 0

        idvw = nlat
        jdvw = nlon

        if nlon%2:                              # nlon is odd
            n1 = min(nlat, (nlon + 1)/2)
        else:
            n1 = min(nlat, (nlon + 2)/2)

        mdab = n1
        ndab = nlat

        work = numpy.zeros((lwork,),'f')
        v, w, pertrb, ierror = spherepack.idivgc(nlat, nlon, isym, idvw, jdvw,
                                                 numpy.transpose(a),
                                                 numpy.transpose(b),
                                                 wvhsgc, work)
        v = numpy.transpose(v)
        w = numpy.transpose(w)

        if ierror != 0 or debug == 1:
            print ' '
            print 'pass to idivgc'
            print 'nlon = ', nlon
            print 'nlat = ', nlat
            print 'isym = ', isym
            print 'nt = ', nt
            print 'idvw = ', idvw
            print 'jdvw = ', jdvw
            print 'mdab = ', mdab
            print 'ndab = ', ndab
            print 'lvhsgc = ', lvhsgc
            print 'lwork = ', lwork
            print 'return from idivgc with u, v'

        if ierror != 0:
            msg = 'In return from call to idivgc ierror =  %d' % (ierror,)
            raise ValueError, msg

        return w, v 

    def idvt(self, nlat, nlon, nt, ad, bd, av, bv, lvhsgc, wvhsgc):
        #-----------------------------------------------------------------------------
        #                                      
        #     purpose: computes a vector function with given divergence and vorticity
        #              on a gaussian grid.
        #
        #     usage:   u, v = g.idvt(nlat, nlon, nt, ad, bd, av, bv, wvhsgc, lvhsgc)
        #
        #-----------------------------------------------------------------------------

        if nlon%2:                              # nlon is odd
            n1 = min(nlat, (nlon + 1)/2)
        else:
            n1 = min(nlat, nlon/2)

        if nlat%2:                              # nlat is odd
            n2 = (nlat + 1)/2
        else:
            n2 = nlat/2

        lwork = nlat*(2*nt*nlon + max(6*n2, nlon) + 4*nt*n1 + 1)

        isym = 0

        idvw = nlat
        jdvw = nlon

        if nlon%2:                              # nlon is odd
            n1 = min(nlat, (nlon + 1)/2)
        else:
            n1 = min(nlat, (nlon + 2)/2)

        mdab = n1
        ndab = nlat

        work = numpy.zeros((lwork,),'f')
        v, w, pertbd, pertbv, ierror = spherepack.idvtgc(nlat, nlon, isym, idvw, jdvw,
                                                         numpy.transpose(ad),
                                                         numpy.transpose(bd),
                                                         numpy.transpose(av),
                                                         numpy.transpose(bv),
                                                         wvhsgc, work)
        v = numpy.transpose(v)
        w = numpy.transpose(w)

        if ierror != 0 or debug == 1:
            print ' '
            print 'pass to idvtgc'
            print 'nlon = ', nlon
            print 'nlat = ', nlat
            print 'isym = ', isym
            print 'nt = ', nt
            print 'idvw = ', idvw
            print 'jdvw = ', jdvw
            print 'mdab = ', mdab
            print 'ndab = ', ndab
            print 'lvhsgc = ', lvhsgc
            print 'lwork = ', lwork
            print 'return from idvtgc with w and v'

        if ierror != 0:
            msg = 'In return from call to idvtgc ierror =  %d' % (ierror,)
            raise ValueError, msg

        return w, v 

    def igrad(self, nlat, nlon, nt, lshsgc, wshsgc, br, bi):
        #-----------------------------------------------------------------------------
        #                                      
        #     purpose: computes a scalar function whose gradient is a given vector
        #              function on a gaussian grid
        #
        #     usage:   sf = g.igrad(nlat, nlon, nt, lshsgc, wshsgc, br, bi)
        #
        #-----------------------------------------------------------------------------

        if nlon%2:                              # nlon is odd
            n1 = min(nlat, (nlon + 1)/2)
        else:
            n1 = min(nlat, (nlon + 2)/2)

        if nlat%2:                              # nlat is odd
            n2 = (nlat + 1)/2
        else:
            n2 = nlat/2

        lwork = nlat*(nlon*nt + max(3*n2, nlon) + 2*nt*n1 + 1)

        isym = 0 

        isf = nlat
        jsf = nlon

        if nlon%2:                              # nlon is odd
            n1 = min(nlat, (nlon + 1)/2)
        else:
            n1 = min(nlat, nlon/2)

        mdb = n1
        ndb = nlat

        # ---  call gradgc ----
        work = numpy.zeros((lwork,),'f')
        sf, ierror = spherepack.igradgc(nlat, nlon, isym, isf, jsf,
                                        numpy.transpose(br),
                                        numpy.transpose(bi),
                                        wshsgc, work)
        sf = numpy.transpose(sf)

        if ierror != 0 or debug == 1:
            print ' '
            print 'pass to igradgc'
            print 'nlon = ', nlon
            print 'nlat = ', nlat
            print 'isym = ', isym
            print 'nt = ', nt
            print 'isf = ', isf
            print 'jsf = ', jsf
            print 'mdb = ', mdb
            print 'ndb = ', ndb
            print 'lshsgc = ', lshsgc
            print 'lwork = ', lwork
            print 'return from igradgc with sf'

        if ierror != 0:
            msg = 'In return from call to igradgc ierror =  %d' % (ierror,)
            raise ValueError, msg

        return sf 

    def isfvp(self, nlat, nlon, nt, as_, bs, av, bv, lvhsgc, wvhsgc):
        #-----------------------------------------------------------------------------
        #                                      
        #     purpose: computes a vector function with a given stream function and
        #              velocity potential on a gaussian grid
        #
        #     usage:   u, v = g.isfvp(nlat, nlon, nt, as_, bs, av, bv, wvhsgc, lvhsgc)
        #
        #-----------------------------------------------------------------------------

        if nlon%2:                              # nlon is odd
            n1 = min(nlat, (nlon + 1)/2)
        else:
            n1 = min(nlat, nlon/2)

        if nlat%2:                              # nlat is odd
            n2 = (nlat + 1)/2
        else:
            n2 = nlat/2

        lwork = nlat*(2*nt*nlon + max(6*n2, nlon) + 4*n1*nt + 1)

        isym = 0

        idv = nlat
        jdv = nlon

        mdb = n1
        ndb = nlat

        work = numpy.zeros((lwork,),'f')
        v, w, ierror = spherepack.isfvpgc(nlat, nlon, isym, idv, jdv, 
                                          numpy.transpose(as_),
                                          numpy.transpose(bs),
                                          numpy.transpose(av),
                                          numpy.transpose(bv),
                                          wvhsgc, work)
        v = numpy.transpose(v)
        w = numpy.transpose(w)

        if ierror != 0 or debug == 1:
            print ' '
            print 'pass to isfvpgc'
            print 'nlon = ', nlon
            print 'nlat = ', nlat
            print 'isym = ', isym
            print 'nt = ', nt
            print 'idv = ', idv
            print 'jdv = ', jdv
            print 'mdb = ', mdb
            print 'ndb = ', ndb
            print 'lvhsgc = ', lvhsgc
            print 'lwork = ', lwork
            print 'return from isfvpgc with u and v'

        if ierror != 0:
            msg = 'In return from call to isfvpgc ierror =  %d' % (ierror,)
            raise ValueError, msg

        return w, v 

    def islap(self, nlat, nlon, nt, lshsgc, wshsgc, a, b):
        #-----------------------------------------------------------------------------
        #                                      
        #     purpose: computes a scalar function whose scalar Laplacian is given on
        #              a gaussian grid
        #
        #     usage:   sf = g.islap(nlat, nlon, nt, lshsgc, wshsgc, a, b)
        #
        #-----------------------------------------------------------------------------

        if nlon%2:                              # nlon is odd
            n1 = min(nlat, (nlon + 1)/2)
        else:
            n1 = min(nlat, (nlon + 2)/2)

        if nlat%2:                              # nlat is odd
            n2 = (nlat + 1)/2
        else:
            n2 = nlat/2

        lwork = nlat*(2*nt*nlon + max(6*n2, nlon) + 2*nt*n1 + 1)

        isym = 0 

        ids = nlat
        jds = nlon

        mdab = n1
        ndab = nlat

        xlmbda = numpy.array( [0.0]*nt, numpy.float32)      # call for Poisson rather than Helmholtz

        # ---  call islapgc ----
        work = numpy.zeros((lwork,),'f')
        sf, pertrb, ierror = spherepack.islapgc(nlat, nlon, isym, xlmbda, ids, jds,
                                                numpy.transpose(a),
                                                numpy.transpose(b),
                                                wshsgc, work)
        sf = numpy.transpose(sf)

        if ierror != 0 or debug == 1:
            print ' '
            print 'pass to islapgc'
            print 'nlon = ', nlon
            print 'nlat = ', nlat
            print 'isym = ', isym
            print 'nt = ', nt
            print 'ids = ', ids
            print 'jds = ', jds
            print 'mdab = ', mdab
            print 'ndab = ', ndab
            print 'lshsgc = ', lshsgc
            print 'lwork = ', lwork
            print 'return from islapgc with sf'

        if ierror != 0:
            msg = 'In return from call to islapgc ierror =  %d' % (ierror,)
            raise ValueError, msg

        return sf 

    def ivlap(self, nlat, nlon, nt, br, bi, cr, ci, lvhsgc, wvhsgc):
        #-----------------------------------------------------------------------------
        #                                      
        #     purpose: computes a vector function whose Laplacian is a given vector
        #              vector function on a gaussian grid
        #
        #     usage:   u, v = g.ivlap(nlat, nlon, nt, br, bi, cr, ci, wvhsgc, lvhsgc)
        #
        #-----------------------------------------------------------------------------

        if nlon%2:                              # nlon is odd
            n1 = min(nlat, (nlon + 1)/2)
        else:
            n1 = min(nlat, nlon/2)

        if nlat%2:                              # nlat is odd
            n2 = (nlat + 1)/2
        else:
            n2 = nlat/2

        lwork = nlat*(2*nt*nlon + max(6*n2, nlon)) + nlat*(4*nt*n1 + 1)

        ityp = 0

        idvw = nlat
        jdvw = nlon

        mdbc = n1
        ndbc = nlat


        work = numpy.zeros((lwork,),'f')
        v, w, ierror = spherepack.ivlapgc(nlat, nlon, ityp, nt, idvw, jdvw, br, bi, cr, ci, mdbc, ndbc, wvhsgc, work)

        work = numpy.zeros((lwork,),'f')
        v, w, ierror = spherepack.ivlapgc(nlat, nlon, ityp, idvw, jdvw,
                                          numpy.transpose(br),
                                          numpy.transpose(bi),
                                          numpy.transpose(cr),
                                          numpy.transpose(ci),
                                          wvhsgc, work)
        v = numpy.transpose(v)
        w = numpy.transpose(w)

        if ierror != 0 or debug == 1:
            print ' '
            print 'pass to ivlapgc'
            print 'nlon = ', nlon
            print 'nlat = ', nlat
            print 'ityp = ', ityp
            print 'nt = ', nt
            print 'idvw = ', idvw
            print 'jdvw = ', jdvw
            print 'mdbc = ', mdbc
            print 'ndbc = ', ndbc
            print 'lvhsgc = ', lvhsgc
            print 'lwork = ', lwork
            print 'return from ivlapgc with u and v'

        if ierror != 0:
            msg = 'In return from call to ivlapgc ierror =  %d' % (ierror,)
            raise ValueError, msg

        return w, v 

    def ivrt(self, nlat, nlon, nt, a, b, lvhsgc, wvhsgc):
        #-----------------------------------------------------------------------------
        #                                      
        #     purpose: computes a divergence-free vector function whose vorticity is
        #              given on a gaussian grid
        #
        #     usage:   w, v, ierror = g.ivrt(nlat, nlon, nt, a, b, wvhsgc, lvhsgc)
        #
        #-----------------------------------------------------------------------------


        if nlon%2:                              # nlon is odd
            n1 = min(nlat, (nlon + 1)/2)
        else:
            n1 = min(nlat, nlon/2)

        if nlat%2:                              # nlat is odd
            n2 = (nlat + 1)/2
        else:
            n2 = nlat/2

        lwork = nlat*(2*nt*nlon + max(6*n2, nlon) + 2*nt*n1 + 1)

        isym = 0

        idvw = nlat
        jdvw = nlon

        if nlon%2:                              # nlon is odd
            n1 = min(nlat, (nlon + 1)/2)
        else:
            n1 = min(nlat, (nlon + 2)/2)

        mdab = n1
        ndab = nlat

        work = numpy.zeros((lwork,),'f')
        v, w, pertrb, ierror = spherepack.ivrtgc(nlat, nlon, isym, idvw, jdvw,
                                                 numpy.transpose(a),
                                                 numpy.transpose(b),
                                                 wvhsgc, work)
        v = numpy.transpose(v)
        w = numpy.transpose(w)


        if ierror != 0 or debug == 1:
            print ' '
            print 'pass to ivrtgc'
            print 'nlon = ', nlon
            print 'nlat = ', nlat
            print 'isym = ', isym
            print 'nt = ', nt
            print 'idvw = ', idvw
            print 'jdvw = ', jdvw
            print 'mdab = ', mdab
            print 'ndab = ', ndab
            print 'lvhsgc = ', lvhsgc
            print 'lwork = ', lwork
            print 'return from ivrtgc with u, v'

        if ierror != 0:
            msg = 'In return from call to ivrtgc ierror =  %d' % (ierror,)
            raise ValueError, msg

        return w, v 

    def sfvp(self, nlat, nlon, nt, br, bi, cr, ci, lshsgc, wshsgc):
        #-----------------------------------------------------------------------------
        #                                      
        #     purpose: computes the stream function and the velocity potential of a 
        #              vector function on a gaussian grid
        #
        #     usage:   sf, vp = g.sfvp(nlat, nlon, nt, br, bi, cr, ci, wshsgc, lshsgc)
        #
        #-----------------------------------------------------------------------------


        if nlon%2:                              # nlon is odd
            n1 = min(nlat, (nlon + 1)/2)
        else:
            n1 = min(nlat, (nlon + 2)/2)

        if nlat%2:                              # nlat is odd
            n2 = (nlat + 1)/2
        else:
            n2 = nlat/2

        lwork = nlat*(nt*nlon + max(3*n2, nlon) + 2*n1*nt + 1)

        isym = 0

        idv = nlat
        jdv = nlon

        if nlon%2:                              # nlon is odd
            n1 = min(nlat, (nlon + 1)/2)
        else:
            n1 = min(nlat, nlon/2)

        mdb = n1
        ndb = nlat

        work = numpy.zeros((lwork,),'f')
        sf, vp, ierror = spherepack.sfvpgc(nlat, nlon, isym, idv, jdv,
                                           numpy.transpose(br),
                                           numpy.transpose(bi),
                                           numpy.transpose(cr),
                                           numpy.transpose(ci),
                                           wshsgc, work)
        sf = numpy.transpose(sf)
        vp = numpy.transpose(vp)

        if ierror != 0 or debug == 1:
            print ' '
            print 'pass to sfvpgc'
            print 'nlon = ', nlon
            print 'nlat = ', nlat
            print 'isym = ', isym
            print 'nt = ', nt
            print 'idv = ', idv
            print 'jdv = ', jdv
            print 'mdb = ', mdb
            print 'ndb = ', ndb
            print 'lshsgc = ', lshsgc
            print 'lwork = ', lwork
            print 'return from sfvpgc with sf and vp'

        if ierror != 0:
            msg = 'In return from call to sfvpgc ierror =  %d' % (ierror,)
            raise ValueError, msg

        return sf, vp 

    def shai(self, nlat, nlon):
        #-----------------------------------------------------------------------------
        #                                      
        #     purpose:  call shai for wshagc and lshagc
        #
        #     usage:  wshagc, lshagc = g.shai(nlat, nlon)
        #
        #-----------------------------------------------------------------------------

        if nlon%2:                              # nlon is odd
            n1 = min(nlat, (nlon + 1)/2)
        else:
            n1 = min(nlat, (nlon + 2)/2)

        if nlat%2:                              # nlat is odd
            n2 = (nlat + 1)/2
        else:
            n2 = nlat/2

        lshagc = nlat*(2*n2 + 3*n1 - 2) + 3*n1*max(1 - n1, 0)/2 + nlon + 15
        ldwork = nlat*(nlat + 4)

        # ---  call shagci ----
        work = numpy.zeros((ldwork,),'d')
        wshagc, ierror = spherepack.shagci(nlat, nlon, lshagc, work)

        if ierror != 0 or debug == 1:
            print ' '
            print 'pass to shagci'
            print 'nlon = ', nlon
            print 'nlat = ', nlat
            print 'lshagc = ', lshagc
            print 'ldwork = ', ldwork
            print 'return from shagci with wshagc and lshagc'

        if ierror != 0:
            msg = 'In return from call to shagci ierror =  %d' % (ierror,)
            raise ValueError, msg

        return wshagc, lshagc

    def sha(self, nlat, nlon, nt, lshagc, wshagc, g):
        #-----------------------------------------------------------------------------
        #                                      
        #     purpose: computes the spherical harmonic analysis on a gaussian grid
        #
        #     usage:   a, b = g.sha(nlat, nlon, nt, lshagc, wshagc, g)
        #
        #-----------------------------------------------------------------------------

        if nlon%2:                              # nlon is odd
            n1 = min(nlat, (nlon + 1)/2)
        else:
            n1 = min(nlat, (nlon + 2)/2)

        if nlat%2:                              # nlat is odd
            n2 = (nlat + 1)/2
        else:
            n2 = nlat/2

        lwork = nlat*(nlon*nt + max(3*n2, nlon))

        isym = 0

        idg = nlat
        jdg = nlon

        mdab = n1
        ndab = nlat


        # ---  call shagc ----
        work = numpy.zeros((lwork,),'f')
        a, b, ierror = spherepack.shagc(nlat, nlon, isym,
                                        numpy.transpose(g),
                                        mdab, ndab,
                                        wshagc, work)
        a = numpy.transpose(a)
        b = numpy.transpose(b)

        if ierror != 0 or debug == 1:
            print ' '
            print 'pass to shagc'
            print 'nlon = ', nlon
            print 'nlat = ', nlat
            print 'isym = ', isym
            print 'nt = ', nt
            print 'idg = ', idg
            print 'jdg = ', jdg
            print 'mdab = ', mdab
            print 'ndab = ', ndab
            print 'lshagc = ', lshagc
            print 'lwork = ', lwork
            print 'return from shagc with a, b'

        if ierror != 0:
            msg = 'In return from call to shagc ierror =  %d' % (ierror,)
            raise ValueError, msg

        return a, b 

    def shsi(self, nlat, nlon):
        #-----------------------------------------------------------------------------
        #                                      
        #     purpose:  call shsgci for wshsgc and lshsgc
        #
        #     usage:  wshsgc, lshsgc = g.shsi(nlat, nlon)
        #
        #-----------------------------------------------------------------------------

        if nlon%2:                              # nlon is odd
            n1 = min(nlat, (nlon + 1)/2)
        else:
            n1 = min(nlat, (nlon + 2)/2)

        if nlat%2:                              # nlat is odd
            n2 = (nlat + 1)/2
        else:
            n2 = nlat/2

        lshsgc = nlat*(2*n2 + 3*n1 - 2) + 3*n1*max(1 - n1, 0)/2 + nlon + 15
        ldwork = nlat*(nlat + 4)

        # ---  call shsgci ----
        work = numpy.zeros((ldwork,),'d')
        wshsgc, ierror = spherepack.shsgci(nlat, nlon, lshsgc, work)

        if ierror != 0 or debug == 1:
            print ' '
            print 'pass to shsgci'
            print 'nlon = ', nlon
            print 'nlat = ', nlat
            print 'lshsgc = ', lshsgc
            print 'ldwork = ', ldwork
            print 'return from shsgci with wshsgc and lshsgc'

        if ierror != 0:
            msg = 'In return from call to shsgci ierror =  %d' % (ierror,)
            raise ValueError, msg

        return wshsgc, lshsgc 

    def shs(self, nlat, nlon, nt, lshsgc, wshsgc, a, b):
        #-----------------------------------------------------------------------------
        #                                      
        #     purpose: computes the spherical harmonic synthesis on a gaussian grid
        #
        #     usage:   g = g.shs(nlat,nlon, nt, lshsgc, wshsgc, a, b)
        #
        #-----------------------------------------------------------------------------

        if nlon%2:                              # nlon is odd
            n1 = min(nlat, (nlon + 1)/2)
        else:
            n1 = min(nlat, (nlon + 2)/2)

        if nlat%2:                              # nlat is odd
            n2 = (nlat + 1)/2
        else:
            n2 = nlat/2

        lwork = nlat*(nlon*nt + max(3*n2, nlon) )

        mode = 0 

        idg = nlat
        jdg = nlon

        mdab = n1
        ndab = nlat

        # ---  call shsgc ----
        work = numpy.zeros((lwork,),'f')
        g, ierror = spherepack.shsgc(nlat, nlon, mode, idg, jdg,
                                        numpy.transpose(a),
                                        numpy.transpose(b),
                                        wshsgc, work)
        g = numpy.transpose(g)
        if ierror != 0 or debug == 1:
            print ' '
            print 'pass to shsgc'
            print 'nlon = ', nlon
            print 'nlat = ', nlat
            print 'mode = ', mode
            print 'nt = ', nt
            print 'idg = ', idg
            print 'jdg = ', jdg
            print 'mdab = ', mdab
            print 'ndab = ', ndab
            print 'lshsgc = ', lshsgc
            print 'lwork = ', lwork
            print 'return from shsgc with g'

        if ierror != 0:
            msg = 'In return from call to shsgc ierror =  %d' % (ierror,)
            raise ValueError, msg

        return g 

    def slap(self, nlat, nlon, nt, lshsgc, wshsgc, a, b):
        #-----------------------------------------------------------------------------
        #                                      
        #     purpose: computes a scalar Laplacian of a scalar function on a gaussian
        #              grid
        #
        #     usage:   slap = g.slap(nlat, nlon, nt, lshsgc, wshsgc, a, b)
        #
        #-----------------------------------------------------------------------------

        if nlon%2:                              # nlon is odd
            n1 = min(nlat, (nlon + 1)/2)
        else:
            n1 = min(nlat, (nlon + 2)/2)

        if nlat%2:                              # nlat is odd
            n2 = (nlat + 1)/2
        else:
            n2 = nlat/2

        lwork = nlat*(2*nt*nlon + max(6*n2, nlon) + 2*nt*n1 + 1)

        isym = 0 

        ids = nlat
        jds = nlon

        mdab = n1
        ndab = nlat

        # ---  call slapgc ----
        work = numpy.zeros((lwork,),'f')
        slap, ierror = spherepack.slapgc(nlat, nlon, isym, ids, jds,
                                         numpy.transpose(a),
                                         numpy.transpose(b),
                                         wshsgc, work)
        slap = numpy.transpose(slap)

        if ierror != 0 or debug == 1:
            print ' '
            print 'pass to slapgc'
            print 'nlon = ', nlon
            print 'nlat = ', nlat
            print 'isym = ', isym
            print 'nt = ', nt
            print 'ids = ', ids
            print 'jds = ', jds
            print 'mdab = ', mdab
            print 'ndab = ', ndab
            print 'lshsgc = ', lshsgc
            print 'lwork = ', lwork
            print 'return from slapgc with slap'

        if ierror != 0:
            msg = 'In return from call to slapgc ierror =  %d' % (ierror,)
            raise ValueError, msg

        return slap 

    def vhai(self, nlat, nlon):
        #-----------------------------------------------------------------------------
        #                                      
        #     purpose:  call vhai for wvhagc and lvhagc
        #
        #     usage:  wvhagc, lvhagc = g.vhai(nlat, nlon)
        #
        #
        #-----------------------------------------------------------------------------

        if nlon%2:                              # nlon is odd
            n1 = min(nlat, (nlon + 1)/2)
        else:
            n1 = min(nlat, nlon/2)


        if nlat%2:                              # nlat is odd
            n2 = (nlat + 1)/2
        else:
            n2 = nlat/2

        lvhagc = 4*nlat*n2 + 3*max(n1 - 2, 0)*(2*nlat - n1 - 1) + nlon + n2 + 15
        ldwork = 2*nlat*(nlat + 1) + 1

        # ---  call vhagci ----
        work = numpy.zeros((ldwork,),'d')
        wvhagc, ierror = spherepack.vhagci(nlat, nlon, lvhagc, work)

        if ierror != 0 or debug == 1:
            print ' '
            print 'pass to vhagci'
            print 'nlon = ', nlon
            print 'nlat = ', nlat
            print 'lvhagc = ', lvhagc
            print 'ldwork = ', ldwork
            print 'return from vhagci with wvhagc'

        if ierror != 0:
            msg = 'In return from call to vhagci ierror =  %d' % (ierror,)
            raise ValueError, msg

        return wvhagc, lvhagc 

    def vha(self, nlat, nlon, nt, lvhagc, wvhagc, w, v):
        #-----------------------------------------------------------------------------
        #                                      
        #     purpose: computes the vector harmonic analysis on a gaussian grid
        #
        #     usage:   br, bi, cr, ci = g.vhagc(nlat, nlon, nt, lvhagc, wvhagc, w, v)
        #
        #-----------------------------------------------------------------------------

        if nlon%2:                              # nlon is odd
            n1 = min(nlat, (nlon + 1)/2)
        else:
            n1 = min(nlat, nlon/2)

        if nlat%2:                              # nlat is odd
            n2 = (nlat + 1)/2
        else:
            n2 = nlat/2

        lwork = 2*nlat*(2*nlon*nt + 3*n2)

        ityp = 0

        idvw = nlat
        jdvw = nlon

        mdab = n1
        ndab = nlat

        # ---  call vhagc ----
        work = numpy.zeros((lwork,),'f')
        br, bi, cr, ci, ierror = spherepack.vhagc(nlat, nlon, ityp,
                                                  numpy.transpose(v),
                                                  numpy.transpose(w),
                                                  mdab, ndab,
                                                  wvhagc, work)
        br = numpy.transpose(br)
        bi = numpy.transpose(bi)
        cr = numpy.transpose(cr)
        ci = numpy.transpose(ci)

        if ierror != 0 or debug == 1:
            print ' '
            print 'pass to vhagc'
            print 'nlon = ', nlon
            print 'nlat = ', nlat
            print 'ityp = ', ityp
            print 'nt = ', nt
            print 'idvw = ', idvw
            print 'jdvw = ', jdvw
            print 'mdab = ', mdab
            print 'ndab = ', ndab
            print 'lvhagc = ', lvhagc
            print 'lwork = ', lwork
            print 'return from vhagc with br,bi,cr,ci'

        if ierror != 0:
            msg = 'In return from call to vhagc ierror =  %d' % (ierror,)
            raise ValueError, msg

        return br, bi, cr, ci 

    def vhsi(self, nlat, nlon):
        #-----------------------------------------------------------------------------
        #                                      
        #     purpose:  call vhsgci for wvhsgc
        #
        #     usage:  wvhsgc, lvhsgc = g.vhsi(nlat, nlon)
        #
        #-----------------------------------------------------------------------------

        if nlon%2:                              # nlon is odd
            n1 = min(nlat, (nlon + 1)/2)
        else:
            n1 = min(nlat, nlon/2)

        if nlat%2:                              # nlat is odd
            n2 = (nlat + 1)/2
        else:
            n2 = nlat/2

        lvhsgc = 4*nlat*n2 + 3*max(n1 - 2, 0)*(2*nlat - n1 -1) + nlon + 15
        ldwork = 2*nlat*(nlat + 1) + 1

        # ---  call vhsgci ----
        work = numpy.zeros((ldwork,),'d')
        wvhsgc, ierror = spherepack.vhsgci(nlat, nlon, lvhsgc, work)

        if ierror != 0 or debug == 1:
            print ' '
            print 'pass to vhsgci'
            print 'nlon = ', nlon
            print 'nlat = ', nlat
            print 'lvhsgc = ', lvhsgc
            print 'ldwork = ', ldwork
            print 'return from vhsgci with wvhsgc'

        if ierror != 0:
            msg = 'In return from call to vhsgci ierror =  %d' % (ierror,)
            raise ValueError, msg

        return wvhsgc, lvhsgc 

    def vhs(self, nlat, nlon, nt, br, bi, cr, ci, lvhsgc, wvhsgc):
        #-----------------------------------------------------------------------------
        #                                      
        #     purpose: computes the vector harmonic synthesis on a gaussian grid
        #
        #     usage:   u, v = g.vhs(nlat, nlon, nt, br, bi, cr, ci, wvhsgc, lvhsgc)
        #
        #-----------------------------------------------------------------------------

        if nlon%2:                              # nlon is odd
            n1 = min(nlat, (nlon + 1)/2)
        else:
            n1 = min(nlat, nlon/2)

        if nlat%2:                              # nlat is odd
            n2 = (nlat + 1)/2
        else:
            n2 = nlat/2

        lwork = nlat*(2*nt*nlon + max(6*n2, nlon))

        ityp = 0

        idvw = nlat
        jdvw = nlon

        mdab = n1
        ndab = nlat

        work = numpy.zeros((lwork,),'f')
        v, w, ierror = spherepack.vhsgc(nlat, nlon, ityp, idvw, jdvw,
                                        numpy.transpose(br),
                                        numpy.transpose(bi),
                                        numpy.transpose(cr),
                                        numpy.transpose(ci),
                                        wvhsgc, work)
        v = numpy.transpose(v)
        w = numpy.transpose(w)

        if ierror != 0 or debug == 1:
            print ' '
            print 'pass to vhsgc'
            print 'nlon = ', nlon
            print 'nlat = ', nlat
            print 'ityp = ', ityp
            print 'nt = ', nt
            print 'idvw = ', idvw
            print 'jdvw = ', jdvw
            print 'mdab = ', mdab
            print 'ndab = ', ndab
            print 'lvhsgc = ', lvhsgc
            print 'lwork = ', lwork
            print 'return from vhsgc with u and v'

        if ierror != 0:
            msg = 'In return from call to vhsgc ierror =  %d' % (ierror,)
            raise ValueError, msg

        return w, v 

    def vlap(self, nlat, nlon, nt, br, bi, cr, ci, lvhsgc, wvhsgc):
        #-----------------------------------------------------------------------------
        #                                      
        #     purpose: computes the vector Laplacian of a given vector function
        #              on a gaussian grid
        #
        #     usage:   u, v = g.vlap(nlat, nlon, nt, br, bi, cr, ci, wvhsgc, lvhsgc)
        #
        #-----------------------------------------------------------------------------

        if nlon%2:                              # nlon is odd
            n1 = min(nlat, (nlon + 1)/2)
        else:
            n1 = min(nlat, (nlon + 2)/2)

        if nlat%2:                              # nlat is odd
            n2 = (nlat + 1)/2
        else:
            n2 = nlat/2

        lwork = nlat*(2*nt*nlon + max(6*n2, nlon)) + nlat*(4*nt*n1 + 1)

        ityp = 0

        idvw = nlat
        jdvw = nlon

        if nlon%2:                              # nlon is odd
            n1 = min(nlat, (nlon + 1)/2)
        else:
            n1 = min(nlat, nlon/2)

        mdbc = n1
        ndbc = nlat

        work = numpy.zeros((lwork,),'f')
        v, w, ierror = spherepack.vlapgc(nlat, nlon, ityp, idvw, jdvw,
                                         numpy.transpose(br),
                                         numpy.transpose(bi),
                                         numpy.transpose(cr),
                                         numpy.transpose(ci),
                                         wvhsgc, work)

        v = numpy.transpose(v)
        w = numpy.transpose(w)

        if ierror != 0 or debug == 1:
            print ' '
            print 'pass to vlapgc'
            print 'nlon = ', nlon
            print 'nlat = ', nlat
            print 'ityp = ', ityp
            print 'nt = ', nt
            print 'idvw = ', idvw
            print 'jdvw = ', jdvw
            print 'mdbc = ', mdbc
            print 'ndbc = ', ndbc
            print 'lvhsgc = ', lvhsgc
            print 'lwork = ', lwork
            print 'return from vlapgc with u and v'

        if ierror != 0:
            msg = 'In return from call to vlapgc ierror =  %d' % (ierror,)
            raise ValueError, msg

        return w, v 

    def vrt(self, nlat, nlon, nt, cr, ci, lshsgc, wshsgc):
        #-----------------------------------------------------------------------------
        #                                      
        #     purpose: computes the scalar vorticity of a vector function on a
        #              gaussian grid
        #
        #     usage:   vort = g.vrt(nlat, nlon, nt, cr, ci, wshsgc, lshsgc)
        #
        #-----------------------------------------------------------------------------

        if nlon%2:                              # nlon is odd
            n1 = min(nlat, (nlon + 1)/2)
        else:
            n1 = min(nlat, nlon/2)

        if nlat%2:                              # nlat is odd
            n2 = (nlat + 1)/2
        else:
            n2 = nlat/2

        lwork = nlat*(nlon*nt + max(3*n2, nlon) + 2*nt*n1 + 1)

        isym = 0

        ivrt = nlat
        jvrt = nlon

        mdc = n1
        ndc = nlat

        work = numpy.zeros((lwork,),'f')
        vort, ierror = spherepack.vrtgc(nlat, nlon, isym, ivrt, jvrt,
                                        numpy.transpose(cr),
                                        numpy.transpose(ci),
                                        wshsgc, work)
        vort = numpy.transpose(vort)

        if ierror != 0 or debug == 1:
            print ' '
            print 'pass to vortgc'
            print 'nlon = ', nlon
            print 'nlat = ', nlat
            print 'isym = ', isym
            print 'nt = ', nt
            print 'ivrt = ', ivrt
            print 'jvrt = ', jvrt
            print 'mdc = ', mdc
            print 'ndc = ', ndc
            print 'lshsgc = ', lshsgc
            print 'lwork = ', lwork
            print 'return from vrtgc with vort'

        if ierror != 0:
            msg = 'In return from call to vrtgc ierror =  %d' % (ierror,)
            raise ValueError, msg

        return vort 

    def vtsi(self, nlat, nlon):
        #-----------------------------------------------------------------------------
        #                                      
        #     purpose: initializes wvts for vtsgc
        #
        #     usage:   wvts, lwvts = g.vtsi(nlat, nlon)
        #
        #-----------------------------------------------------------------------------

        if nlon%2:                              # nlon is odd
            n1 = min(nlat, (nlon + 1)/2)
        else:
            n1 = min(nlat, nlon/2)

        if nlat%2:                              # nlat is odd
            n2 = (nlat + 1)/2
        else:
            n2 = nlat/2

        lwvts = 4*nlat*n2 + 3*max(n1 - 2, 0)*(nlat + nlat - n1 - 1) + nlon + 15
        ldwork = nlat*(nlat + 4) 

        # ---  call vtsgci ----
        work = numpy.zeros((ldwork,),'d')
        wvts, ierror = spherepack.vtsgci(nlat, nlon, lwvts, work)

        if ierror != 0 or debug == 1:
            print ' '
            print 'nlon = ', nlon
            print 'nlat = ', nlat
            print 'lwvts = ', lwvts
            print 'ldwork = ', ldwork
            print 'return from vtsgci with wvts and lwvts'

        if ierror != 0:
            msg = 'In return from call to vtsi ierror =  %d' % (ierror,)
            raise ValueError, msg

        return wvts, lwvts 

    def vts(self, nlat, nlon, nt, br, bi, cr, ci, lwvts, wvts):
        #-----------------------------------------------------------------------------
        #                                      
        #     purpose: computes the derivative of the vector function with respect
        #              to latitude on a gaussian grid
        #
        #     usage:   w, v = g.vts(nlat, nlon, nt, br, bi, cr, ci, wvts, lwvts)
        #
        #-----------------------------------------------------------------------------

        if nlon%2:                              # nlon is odd
            n1 = min(nlat, (nlon + 1)/2)
        else:
            n1 = min(nlat, nlon/2)

        if nlat%2:                              # nlat is odd
            n2 = (nlat + 1)/2
        else:
            n2 = nlat/2

        lwork = nlat*(2*nt*nlon + max(6*n2, nlon))

        ityp = 0

        idvw = nlat
        jdvw = nlon

        mdab = n1
        ndab = nlat

        work = numpy.zeros((lwork,),'f')
        v, w, ierror = spherepack.vtsgc(nlat, nlon, ityp, idvw, jdvw,
                                        numpy.transpose(br),
                                        numpy.transpose(bi),
                                        numpy.transpose(cr),
                                        numpy.transpose(ci),
                                        wvts, work)
        v = numpy.transpose(v)
        w = numpy.transpose(w)

        if ierror != 0 or debug == 1:
            print ' '
            print 'pass to vtsgc'
            print 'nlon = ', nlon
            print 'nlat = ', nlat
            print 'ityp = ', ityp
            print 'nt = ', nt
            print 'idvw = ', idvw
            print 'jdvw = ', jdvw
            print 'mdab = ', mdab
            print 'ndab = ', ndab
            print 'lwvts = ', lwvts
            print 'lwork = ', lwork
            print 'return from vtsgc with u and v'

        if ierror != 0:
            msg = 'In return from call to vtsgc ierror =  %d' % (ierror,)
            raise ValueError, msg

        return w, v 

# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# ++++++++++++++++++++++++++++ Equally Spaced Stored Case ++++++++++++++++++++++++++++
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

class Wrapes:
    #-----------------------------------------------------------------------------------
    #                                      
    #     purpose: provides a first layer 'gridComp' wrapper to assign the array
    #              sizes. The final layer in class Sphere, seen by user,  will call the
    #              associated intializations and preliminary functions and then
    #              return the result expected from the name of the call.
    #
    #     usage:  x  = Wrapes()       --  makes an instance
    #
    #-----------------------------------------------------------------------------------

    def div(self, nlat, nlon, nt, br, bi, lshses, wshses):
        #-----------------------------------------------------------------------------
        #                                      
        #     purpose: computes the divergence of a vector function on a equally spaced
        #              grid
        #
        #     usage:   dv = .div(nlat, nlon, nt, br, bi, wshses, lshses)
        #
        #-----------------------------------------------------------------------------

        if nlon%2:                              # nlon is odd
            n1 = min(nlat, (nlon + 1)/2)
        else:
            n1 = min(nlat, (nlon + 2)/2)

        if nlat%2:                              # nlat is odd
            n2 = (nlat + 1)/2
        else:
            n2 = nlat/2

        lwork = nlat*((nt + 1)*nlon + 2*nt*n1 + 1)

        isym = 0

        idv = nlat
        jdv = nlon

        if nlon%2:                              # nlon is odd
            n1 = min(nlat, (nlon + 1)/2)
        else:
            n1 = min(nlat, nlon/2)

        mdb = n1
        ndb = nlat

        work = numpy.zeros((lwork,),'f')
        dv, ierror = spherepack.dives(nlat, nlon, isym, idv, jdv,
                                      numpy.transpose(br),
                                      numpy.transpose(bi),
                                      wshses, work)
        dv = numpy.transpose(dv)

        if ierror != 0 or debug == 1:
            print ' '
            print 'pass to dives'
            print 'nlon = ', nlon
            print 'nlat = ', nlat
            print 'isym = ', isym
            print 'nt = ', nt
            print 'idv = ', idv
            print 'jdv = ', jdv
            print 'mdb = ', mdb
            print 'ndb = ', ndb
            print 'lshses = ', lshses
            print 'lwork = ', lwork
            print 'return from dives with div'

        if ierror != 0:
            msg = 'In return from call to dives ierror =  %d' % (ierror,)
            raise ValueError, msg

        return dv 

    def grad(self, nlat, nlon, nt, a, b, lvhses, wvhses):
        #-----------------------------------------------------------------------------
        #                                      
        #     purpose: computes the gradient of a scalar function on a equally spaced grid
        #
        #     usage:   u, v = x.grad(nlat, nlon, nt, a, b, wvhses, lvhses)
        #
        #-----------------------------------------------------------------------------

        if nlon%2:                              # nlon is odd
            n1 = min(nlat, (nlon + 1)/2)
        else:
            n1 = min(nlat, nlon/2)

        if nlat%2:                              # nlat is odd
            n2 = (nlat + 1)/2
        else:
            n2 = nlat/2

        lwork = nlat*((2*nt + 1)*nlon +  2*n1*nt + 1)

        isym = 0 

        idvw = nlat
        jdvw = nlon

        if nlon%2:                              # nlon is odd
            n1 = min(nlat, (nlon + 1)/2)
        else:
            n1 = min(nlat, (nlon + 2)/2)

        mdab = n1
        ndab = nlat

        # ---  call grades ----
        work = numpy.zeros((lwork,),'f')
        v, w, ierror = spherepack.grades(nlat, nlon, isym, idvw, jdvw,
                                         numpy.transpose(a),
                                         numpy.transpose(b),
                                         wvhses, work)
        v = numpy.transpose(v)
        w = numpy.transpose(w)

        if ierror != 0 or debug == 1:
            print ' '
            print 'pass to grades'
            print 'nlon = ', nlon
            print 'nlat = ', nlat
            print 'isym = ', isym
            print 'nt = ', nt
            print 'idvw = ', idvw
            print 'jdvw = ', jdvw
            print 'mdab = ', mdab
            print 'ndab = ', ndab
            print 'lvhses = ', lvhses
            print 'lwork = ', lwork
            print 'return from grades with u, v'

        if ierror != 0:
            msg = 'In return from call to grades ierror =  %d' % (ierror,)
            raise ValueError, msg

        return w, v 

    def idiv(self, nlat, nlon, nt, a, b, lvhses, wvhses):
        #-----------------------------------------------------------------------------
        #                                      
        #     purpose: computes an irrotational vector function whose divergence is
        #              given on a equally spaced grid.
        #
        #     usage:   u, v = x.idiv(nlat, nlon, nt, a, b, wvhses, lvhses)
        #
        #-----------------------------------------------------------------------------

        if nlon%2:                              # nlon is odd
            n1 = min(nlat, (nlon + 1)/2)
        else:
            n1 = min(nlat, nlon/2)

        if nlat%2:                              # nlat is odd
            n2 = (nlat + 1)/2
        else:
            n2 = nlat/2

        lwork = nlat*((2*nt + 1)*nlon  + 2*nt*n1 + 1)

        isym = 0

        idvw = nlat
        jdvw = nlon

        if nlon%2:                              # nlon is odd
            n1 = min(nlat, (nlon + 1)/2)
        else:
            n1 = min(nlat, (nlon + 2)/2)

        mdab = n1
        ndab = nlat

        work = numpy.zeros((lwork,),'f')
        v, w, pertrb, ierror = spherepack.idives(nlat, nlon, isym, idvw, jdvw,
                                                 numpy.transpose(a),
                                                 numpy.transpose(b),
                                                 wvhses, work)
        v = numpy.transpose(v)
        w = numpy.transpose(w)

        if ierror != 0 or debug == 1:
            print ' '
            print 'pass to idives'
            print 'nlon = ', nlon
            print 'nlat = ', nlat
            print 'isym = ', isym
            print 'nt = ', nt
            print 'idvw = ', idvw
            print 'jdvw = ', jdvw
            print 'mdab = ', mdab
            print 'ndab = ', ndab
            print 'lvhses = ', lvhses
            print 'lwork = ', lwork
            print 'return from idives with u, v'

        if ierror != 0:
            msg = 'In return from call to idives ierror =  %d' % (ierror,)
            raise ValueError, msg

        return w, v 

    def idvt(self, nlat, nlon, nt, ad, bd, av, bv, lvhses, wvhses):
        #-----------------------------------------------------------------------------
        #                                      
        #     purpose: computes a vector function with given divergence and vorticity
        #              on a equally spaced grid.
        #
        #     usage:   u, v = x.idvt(nlat, nlon, nt, ad, bd, av, bv, wvhses, lvhses)
        #
        #-----------------------------------------------------------------------------

        if nlon%2:                              # nlon is odd
            n1 = min(nlat, (nlon + 1)/2)
        else:
            n1 = min(nlat, nlon/2)

        if nlat%2:                              # nlat is odd
            n2 = (nlat + 1)/2
        else:
            n2 = nlat/2

        lwork = nlat*((2*nt + 1)*nlon + 4*nt*n1 + 1)

        isym = 0

        idvw = nlat
        jdvw = nlon

        if nlon%2:                              # nlon is odd
            n1 = min(nlat, (nlon + 1)/2)
        else:
            n1 = min(nlat, (nlon + 2)/2)

        mdab = n1
        ndab = nlat

        work = numpy.zeros((lwork,),'f')
        v, w, pertbd, pertbv, ierror = spherepack.idvtes(nlat, nlon, isym, idvw, jdvw,
                                                         numpy.transpose(ad),
                                                         numpy.transpose(bd),
                                                         numpy.transpose(av),
                                                         numpy.transpose(bv),
                                                         wvhses, work)
        v = numpy.transpose(v)
        w = numpy.transpose(w)

        if ierror != 0 or debug == 1:
            print ' '
            print 'pass to idvtes'
            print 'nlon = ', nlon
            print 'nlat = ', nlat
            print 'isym = ', isym
            print 'nt = ', nt
            print 'idvw = ', idvw
            print 'jdvw = ', jdvw
            print 'mdab = ', mdab
            print 'ndab = ', ndab
            print 'lvhses = ', lvhses
            print 'lwork = ', lwork
            print 'return from idvtes with u and v'

        if ierror != 0:
            msg = 'In return from call to idvtes ierror =  %d' % (ierror,)
            raise ValueError, msg

        return w, v 

    def igrad(self, nlat, nlon, nt, lshses, wshses, br, bi):
        #-----------------------------------------------------------------------------
        #                                      
        #     purpose: computes a scalar function whose gradient is a given vector
        #              function on a equally spaced grid
        #
        #     usage:   sf = x.igrad(nlat, nlon, nt, lshses, wshses, br, bi)
        #
        #-----------------------------------------------------------------------------

        if nlon%2:                              # nlon is odd
            n1 = min(nlat, (nlon + 1)/2)
        else:
            n1 = min(nlat, (nlon + 2)/2)

        if nlat%2:                              # nlat is odd
            n2 = (nlat + 1)/2
        else:
            n2 = nlat/2

        lwork = nlat*((nt + 1)*nlon + 2*nt*n1 + 1)

        isym = 0 

        isf = nlat
        jsf = nlon

        if nlon%2:                              # nlon is odd
            n1 = min(nlat, (nlon + 1)/2)
        else:
            n1 = min(nlat, nlon/2)

        mdb = n1
        ndb = nlat

        # ---  call grades ----
        work = numpy.zeros((lwork,),'f')
        sf, ierror = spherepack.igrades(nlat, nlon, isym, isf, jsf,
                                        numpy.transpose(br),
                                        numpy.transpose(bi),
                                        wshses, work)
        sf = numpy.transpose(sf)
        if ierror != 0 or debug == 1:
            print ' '
            print 'pass to igrades'
            print 'nlon = ', nlon
            print 'nlat = ', nlat
            print 'isym = ', isym
            print 'nt = ', nt
            print 'isf = ', isf
            print 'jsf = ', jsf
            print 'mdb = ', mdb
            print 'ndb = ', ndb
            print 'lshses = ', lshses
            print 'lwork = ', lwork
            print 'return from igrades with sf'

        if ierror != 0:
            msg = 'In return from call to igrades ierror =  %d' % (ierror,)
            raise ValueError, msg

        return sf 

    def isfvp(self, nlat, nlon, nt, as_, bs, av, bv, lvhses, wvhses):
        #-----------------------------------------------------------------------------
        #                                      
        #     purpose: computes a vector function with a given stream function and
        #              velocity potential on a equally spaced grid
        #
        #     usage:   u, v = x.isfvp(nlat, nlon, nt, as_, bs, av, bv, wvhses, lvhses)
        #
        #-----------------------------------------------------------------------------

        n1 = min(nlat, (nlon + 2)/2)

        lwork = nlat*((2*nt + 1)*nlon + 4*n1*nt + 1)

        isym = 0

        idv = nlat
        jdv = nlon

        if nlon%2:                              # nlon is odd
            n1 = min(nlat, (nlon + 1)/2)
        else:
            n1 = min(nlat, (nlon + 2)/2)

        mdb = n1
        ndb = nlat

        work = numpy.zeros((lwork,),'f')
        v, w, ierror = spherepack.isfvpes(nlat, nlon, isym, idv, jdv,
                                          numpy.transpose(as_),
                                          numpy.transpose(bis),
                                          numpy.transpose(av),
                                          numpy.transpose(bv),
                                          wvhses, work)

        v = numpy.transpose(v)
        w = numpy.transpose(w)

        if ierror != 0 or debug == 1:
            print ' '
            print 'pass to isfvpes'
            print 'nlon = ', nlon
            print 'nlat = ', nlat
            print 'isym = ', isym
            print 'nt = ', nt
            print 'idv = ', idv
            print 'jdv = ', jdv
            print 'mdb = ', mdb
            print 'ndb = ', ndb
            print 'lvhses = ', lvhses
            print 'lwork = ', lwork
            print 'return from isfvpes with u and v'

        if ierror != 0:
            msg = 'In return from call to isfvpes ierror =  %d' % (ierror,)
            raise ValueError, msg

        return w, v 

    def islap(self, nlat, nlon, nt, lshses, wshses, a, b):
        #-----------------------------------------------------------------------------
        #                                      
        #     purpose: computes a scalar function whose scalar Laplacian is given on
        #              a equally spaced grid
        #
        #     usage:   sf = x.islap(nlat, nlon, nt, lshses, wshses, a, b)
        #
        #-----------------------------------------------------------------------------

        if nlon%2:                              # nlon is odd
            n1 = min(nlat, (nlon + 1)/2)
        else:
            n1 = min(nlat, (nlon + 2)/2)

        if nlat%2:                              # nlat is odd
            n2 = (nlat + 1)/2
        else:
            n2 = nlat/2

        lwork = nlat*((nt + 1)*nlon + 2*nt*n1 + 1)

        isym = 0 

        ids = nlat
        jds = nlon

        mdab = n1
        ndab = nlat

        xlmbda = numpy.array( [0.0]*nt, numpy.float32)      # call for Poisson rather than Helmholtz

        # ---  call islapes ----
        work = numpy.zeros((lwork,),'f')
        sf, pertrb, ierror = spherepack.islapes(nlat, nlon, isym, xlmbda, ids, jds,
                                                numpy.transpose(a),
                                                numpy.transpose(b),
                                                wshses, work)
        sf = numpy.transpose(sf)

        if ierror != 0 or debug == 1:
            print ' '
            print 'pass to islapes'
            print 'nlon = ', nlon
            print 'nlat = ', nlat
            print 'isym = ', isym
            print 'nt = ', nt
            print 'ids = ', ids
            print 'jds = ', jds
            print 'mdab = ', mdab
            print 'ndab = ', ndab
            print 'lshses = ', lshses
            print 'lwork = ', lwork
            print 'return from islapes with sf'

        if ierror != 0:
            msg = 'In return from call to islapes ierror =  %d' % (ierror,)
            raise ValueError, msg

        return sf 

    def ivlap(self, nlat, nlon, nt, br, bi, cr, ci, lvhses, wvhses):
        #-----------------------------------------------------------------------------
        #                                      
        #     purpose: computes a vector function whose Laplacian is a given vector
        #              vector function on a equally spaced grid
        #
        #     usage:   u, v = x.ivlap(nlat, nlon, nt, br, bi, cr, ci, wvhses, lvhses)
        #
        #-----------------------------------------------------------------------------

        if nlon%2:                              # nlon is odd
            n1 = min(nlat, (nlon + 1)/2)
        else:
            n1 = min(nlat, nlon/2)

        if nlat%2:                              # nlat is odd
            n2 = (nlat + 1)/2
        else:
            n2 = nlat/2

        lwork = nlat*((2*nt + 1)*nlon + 4*nt*n1 + 1)

        ityp = 0

        idvw = nlat
        jdvw = nlon

        mdbc = n1
        ndbc = nlat

        work = numpy.zeros((lwork,),'f')
        v, w, ierror = spherepack.ivlapes(nlat, nlon, ityp, idvw, jdvw,
                                          numpy.transpose(br),
                                          numpy.transpose(bi),
                                          numpy.transpose(cr),
                                          numpy.transpose(ci),
                                          wvhses, work)
        v = numpy.transpose(v)
        w = numpy.transpose(w)

        if ierror != 0 or debug == 1:
            print ' '
            print 'pass to ivlapes'
            print 'nlon = ', nlon
            print 'nlat = ', nlat
            print 'ityp = ', ityp
            print 'nt = ', nt
            print 'idvw = ', idvw
            print 'jdvw = ', jdvw
            print 'mdbc = ', mdbc
            print 'ndbc = ', ndbc
            print 'lvhses = ', lvhses
            print 'lwork = ', lwork
            print 'return from ivlapes with u and v'

        if ierror != 0:
            msg = 'In return from call to ivlapes ierror =  %d' % (ierror,)
            raise ValueError, msg

        return w, v 

    def ivrt(self, nlat, nlon, nt, a, b, lvhses, wvhses):
        #-----------------------------------------------------------------------------
        #                                      
        #     purpose: computes a divergence-free vector function whose vorticity is
        #              given on a equally spaced grid
        #
        #     usage:   w, v = x.ivrt(nlat, nlon, nt, a, b, wvhses, lvhses)
        #
        #-----------------------------------------------------------------------------


        if nlon%2:                              # nlon is odd
            n1 = min(nlat, (nlon + 1)/2)
        else:
            n1 = min(nlat, nlon/2)

        if nlat%2:                              # nlat is odd
            n2 = (nlat + 1)/2
        else:
            n2 = nlat/2

        lwork = nlat*((2*nt + 1)*nlon + 2*nt*n1 + 1)

        isym = 0

        idvw = nlat
        jdvw = nlon

        if nlon%2:                              # nlon is odd
            n1 = min(nlat, (nlon + 1)/2)
        else:
            n1 = min(nlat, (nlon + 2)/2)

        mdab = n1
        ndab = nlat

        work = numpy.zeros((lwork,),'f')
        v, w, pertrb, ierror = spherepack.ivrtes(nlat, nlon, isym, idvw, jdvw,
                                                 numpy.transpose(a),
                                                 numpy.transpose(b),
                                                 wvhses, work)

        v = numpy.transpose(v)
        w = numpy.transpose(w)
        if ierror != 0 or debug == 1:
            print ' '
            print 'pass to ivrtes'
            print 'nlon = ', nlon
            print 'nlat = ', nlat
            print 'isym = ', isym
            print 'nt = ', nt
            print 'idvw = ', idvw
            print 'jdvw = ', jdvw
            print 'mdab = ', mdab
            print 'ndab = ', ndab
            print 'lvhses = ', lvhses
            print 'lwork = ', lwork
            print 'return from ivrtes with u, v'

        if ierror != 0:
            msg = 'In return from call to ivrtes ierror =  %d' % (ierror,)
            raise ValueError, msg

        return w, v 

    def sfvp(self, nlat, nlon, nt, br, bi, cr, ci, lshses, wshses):
        #-----------------------------------------------------------------------------
        #                                      
        #     purpose: computes the stream function and the velocity potential of a 
        #              vector function on a equally spaced grid
        #
        #     usage:   sf, vp = x.sfvp(nlat, nlon, nt, br, bi, cr, ci, wshses, lshses)
        #
        #-----------------------------------------------------------------------------

        # lwork size changed to use n1 - not n2 as in the fortran code

        if nlon%2:                              # nlon is odd
            n1 = min(nlat, (nlon + 1)/2)
        else:
            n1 = min(nlat, (nlon + 2)/2)

        lwork = nlat*((nt + 1)*nlon + 2*n1*nt + 1)

        isym = 0

        idv = nlat
        jdv = nlon

        if nlon%2:                              # nlon is odd
            n1 = min(nlat, (nlon + 1)/2)
        else:
            n1 = min(nlat, nlon/2)

        mdb = n1
        ndb = nlat

        work = numpy.zeros((lwork,),'f')
        sf, vp, ierror = spherepack.sfvpes(nlat, nlon, isym, idv, jdv,
                                           numpy.transpose(br),
                                           numpy.transpose(bi),
                                           numpy.transpose(cr),
                                           numpy.transpose(ci),
                                           wshses, work)
        sf = numpy.transpose(sf)
        vp = numpy.transpose(vp)
        

        if ierror != 0 or debug == 1:
            print ' '
            print 'pass to sfvpes'
            print 'nlon = ', nlon
            print 'nlat = ', nlat
            print 'isym = ', isym
            print 'nt = ', nt
            print 'idv = ', idv
            print 'jdv = ', jdv
            print 'mdb = ', mdb
            print 'ndb = ', ndb
            print 'lshses = ', lshses
            print 'lwork = ', lwork
            print 'return from sfvpes with sf and vp'

        if ierror != 0:
            msg = 'In return from call to sfvpes ierror =  %d' % (ierror,)
            raise ValueError, msg

        return sf, vp 

    def shai(self, nlat, nlon):
        #-----------------------------------------------------------------------------
        #                                      
        #     purpose:  call shai for wshaes
        #
        #     usage:  wsha, lsha = x.shai(nlat, nlon)
        #
        #-----------------------------------------------------------------------------

        if nlon%2:                              # nlon is odd
            n1 = min(nlat, (nlon + 1)/2)
        else:
            n1 = min(nlat, (nlon + 2)/2)


        if nlat%2:                              # nlat is odd
            n2 = (nlat + 1)/2
        else:
            n2 = nlat/2

        lshaes = (n1*n2*(nlat + nlat - n1 + 1))/2 + nlon + 15
        lwork = 5*nlat*n2 + 3*((n1 - 2)*(nlat + nlat - n1 -1))/2 
        ldwork = nlat + 1

        # ---  call shaesi ----
        work = numpy.zeros((lwork,),'f')
        dwork = numpy.zeros((ldwork,),'d')
        wshaes, ierror = spherepack.shaesi(nlat, nlon, lshaes, work, dwork)

        if ierror != 0 or debug == 1:
            print ' '
            print 'pass to shaesi'
            print 'nlon = ', nlon
            print 'nlat = ', nlat
            print 'lshaes = ', lshaes
            print 'lwork = ', lwork
            print 'ldwork = ', ldwork
            print 'return from shaesi with wshaes and lshaes'

        if ierror != 0:
            msg = 'In return from call to shaies ierror =  %d' % (ierror,)
            raise ValueError, msg

        return wshaes, lshaes 

    def sha(self, nlat, nlon, nt, lshaes, wshaes, g):
        #-----------------------------------------------------------------------------
        #                                      
        #     purpose:  call sha for coefficients
        #
        #     usage:  a, b = x.sha(nlat, nlon, lshaes, wshaes, g)
        #
        #-----------------------------------------------------------------------------

        if nlon%2:                              # nlon is odd
            n1 = min(nlat, (nlon + 1)/2)
        else:
            n1 = min(nlat, (nlon + 2)/2)

        if nlat%2:                              # nlat is odd
            n2 = (nlat + 1)/2
        else:
            n2 = nlat/2

        lwork = (nt + 1)*nlat*nlon 

        isym = 0

        idg = nlat
        jdg = nlon

        mdab = n1
        ndab = nlat


        # ---  call shaes ----
        work = numpy.zeros((lwork,),'f')
        a, b, ierror = spherepack.shaes(nlat, nlon, isym,
                                        numpy.transpose(g),
                                        mdab, ndab,
                                        wshaes, work)
        a = numpy.transpose(a)
        b = numpy.transpose(b)
        

        if ierror != 0 or debug == 1:
            print ' '
            print 'pass to shaes'
            print 'nlon = ', nlon
            print 'nlat = ', nlat
            print 'isym = ', isym
            print 'nt = ', nt
            print 'idg = ', idg
            print 'jdg = ', jdg
            print 'mdab = ', mdab
            print 'ndab = ', ndab
            print 'lshaes = ', lshaes
            print 'lwork = ', lwork
            print 'return from shaes with a, b'

        if ierror != 0:
            msg = 'In return from call to shaes ierror =  %d' % (ierror,)
            raise ValueError, msg

        return a, b 

    def shsi(self, nlat, nlon):
        #-----------------------------------------------------------------------------
        #                                      
        #     purpose:  call shsi for wshses
        #
        #     usage:  wshs, lshs = x.shsi(nlat, nlon)
        #
        #-----------------------------------------------------------------------------

        if nlon%2:                              # nlon is odd
            n1 = min(nlat, (nlon + 1)/2)
        else:
            n1 = min(nlat, (nlon + 2)/2)

        if nlat%2:                              # nlat is odd
            n2 = (nlat + 1)/2
        else:
            n2 = nlat/2

        lshses = (n1*n2*(nlat + nlat - n1 +1))/2 + nlon + 15
        lwork = 5*nlat*n2 + 3*((n1 - 2)*(nlat + nlat - n1 -1))/2
        ldwork = nlat + 1

        # ---  call shsesi ----
        dwork = numpy.zeros((ldwork,),'d')
        work = numpy.zeros((lwork,),'f')
        wshses, ierror = spherepack.shsesi(nlat, nlon, lshses, work, dwork)

        if ierror != 0 or debug == 1:
            print ' '
            print 'pass to shsesi'
            print 'nlon = ', nlon
            print 'nlat = ', nlat
            print 'lshses = ', lshses
            print 'lwork = ', lwork
            print 'ldwork = ', ldwork
            print 'return from shsesi with wshses and lshses'

        if ierror != 0:
            msg = 'In return from call to shsesi ierror =  %d' % (ierror,)
            raise ValueError, msg

        return wshses, lshses 

    def shs(self, nlat, nlon, nt, lshses, wshses, a, b):
        #-----------------------------------------------------------------------------
        #                                      
        #     purpose: computes the spherical harmonic synthesis on an evenly spaced grid
        #
        #     usage:   g = x.shs(nlat,nlon, nt, lshses, wshses, a, b)
        #
        #-----------------------------------------------------------------------------

        if nlon%2:                              # nlon is odd
            n1 = min(nlat, (nlon + 1)/2)
        else:
            n1 = min(nlat, (nlon + 2)/2)

        if nlat%2:                              # nlat is odd
            n2 = (nlat + 1)/2
        else:
            n2 = nlat/2

        lwork = (nt +1)*nlat*nlon

        isym = 0 

        idg = nlat
        jdg = nlon

        mdab = n1
        ndab = nlat

        # ---  call shses ----
        work = numpy.zeros((lwork,),'f')
        g, ierror = spherepack.shses(nlat, nlon, isym, idg, jdg,
                                     numpy.transpose(a),
                                     numpy.transpose(b),
                                     wshses, work)
        g = numpy.transpose(g)
        

        if ierror != 0 or debug == 1:
            print ' '
            print 'pass to shses'
            print 'nlon = ', nlon
            print 'nlat = ', nlat
            print 'isym = ', isym
            print 'nt = ', nt
            print 'idg = ', idg
            print 'jdg = ', jdg
            print 'mdab = ', mdab
            print 'ndab = ', ndab
            print 'lshses = ', lshses
            print 'lwork = ', lwork
            print 'return from shses with g'

        if ierror != 0:
            msg = 'In return from call to shses ierror =  %d' % (ierror,)
            raise ValueError, msg

        return g 

    def slap(self, nlat, nlon, nt, lshses, wshses, a, b):
        #-----------------------------------------------------------------------------
        #                                      
        #     purpose: computes a scalar Laplacian of a scalar function on a equally spaced
        #              grid
        #
        #     usage:   slap = x.slap(nlat, nlon, nt, lshses, wshses, a, b)
        #
        #-----------------------------------------------------------------------------

        if nlon%2:                              # nlon is odd
            n1 = min(nlat, (nlon + 1)/2)
        else:
            n1 = min(nlat, (nlon + 2)/2)

        if nlat%2:                              # nlat is odd
            n2 = (nlat + 1)/2
        else:
            n2 = nlat/2

        lwork = (nt + 1)*nlat*nlon + nlat*(2*nt*n1 + 1)

        isym = 0 

        ids = nlat
        jds = nlon

        mdab = n1
        ndab = nlat

        # ---  call slapes ----
        
        work = numpy.zeros((lwork,),'f')
        slap, ierror = spherepack.slapes(nlat, nlon, isym, ids, jds,
                                         numpy.transpose(a),
                                         numpy.transpose(b),
                                         wshses, work)
        slap = numpy.transpose(slap)
        

        if ierror != 0 or debug == 1:
            print ' '
            print 'pass to slapes'
            print 'nlon = ', nlon
            print 'nlat = ', nlat
            print 'isym = ', isym
            print 'nt = ', nt
            print 'ids = ', ids
            print 'jds = ', jds
            print 'mdab = ', mdab
            print 'ndab = ', ndab
            print 'lshses = ', lshses
            print 'lwork = ', lwork
            print 'return from slapes with slap'

        if ierror != 0:
            msg = 'In return from call to slapes ierror =  %d' % (ierror,)
            raise ValueError, msg

        return slap 

    def vhai(self, nlat, nlon):
        #-----------------------------------------------------------------------------
        #                                      
        #     purpose:  call vhasi for wvhaes
        #
        #     usage:  wvha, lvha = x.vhai(nlat, nlon)
        #
        #-----------------------------------------------------------------------------

        if nlon%2:                              # nlon is odd
            n1 = min(nlat, (nlon + 1)/2)
        else:
            n1 = min(nlat, nlon/2)

        if nlat%2:                              # nlat is odd
            n2 = (nlat + 1)/2
        else:
            n2 = nlat/2

        lvhaes = n1*n2*(nlat + nlat - n1 + 1) + nlon + 15
        lwork = 3*(max(n1 -2,0)*(nlat + nlat - n1 - 1))/2 + 5*n2*nlat
        ldwork = 2*(nlat + 1)

        # ---  call vhaesi ----
        work = numpy.zeros((lwork,),'f')
        dwork = numpy.zeros((ldwork,),'d')
        wvhaes, ierror = spherepack.vhaesi(nlat, nlon, lvhaes, work, dwork)

        if ierror != 0 or debug == 1:
            print ' '
            print 'pass to vhaesi'
            print 'nlon = ', nlon
            print 'nlat = ', nlat
            print 'lvhaes = ', lvhaes
            print 'lwork = ', lwork
            print 'ldwork = ', ldwork
            print 'return from vhaesi with wvhaes and lvhaes'

        if ierror != 0:
            msg = 'In return from call to vhaesi ierror =  %d' % (ierror,)
            raise ValueError, msg

        return wvhaes, lvhaes 

    def vha(self, nlat, nlon, nt, lvhaes, wvhaes, w, v):
        #-----------------------------------------------------------------------------
        #                                      
        #     purpose: computes the vector harmonic analysis on a equally spaced grid
        #
        #     usage:   br, bi, cr, ci  = x.vha(nlat, nlon, nt, lvhaes, wvhaes, w, v)
        #
        #-----------------------------------------------------------------------------


        if nlon%2:                              # nlon is odd
            n1 = min(nlat, (nlon + 1)/2)
        else:
            n1 = min(nlat, nlon/2)

        if nlat%2:                              # nlat is odd
            n2 = (nlat + 1)/2
        else:
            n2 = nlat/2

        lwork = (2*nt + 1)*nlat*nlon

        ityp = 0

        idvw = nlat
        jdvw = nlon

        mdab = n1
        ndab = nlat

        # ---  call vhaes ----
        work = numpy.zeros((lwork,),'f')
        br, bi, cr, ci, ierror = spherepack.vhaes(nlat, nlon, ityp,
                                                  numpy.transpose(v),
                                                  numpy.transpose(w),
                                                  mdab, ndab,
                                                  wvhaes, work)
        br = numpy.transpose(br)
        bi = numpy.transpose(bi)
        cr = numpy.transpose(cr)
        ci = numpy.transpose(ci)
        

        if ierror != 0 or debug == 1:
            print ' '
            print 'pass to vhaes'
            print 'nlon = ', nlon
            print 'nlat = ', nlat
            print 'ityp = ', ityp
            print 'nt = ', nt
            print 'idvw = ', idvw
            print 'jdvw = ', jdvw
            print 'mdab = ', mdab
            print 'ndab = ', ndab
            print 'lvhaes = ', lvhaes
            print 'lwork = ', lwork
            print 'return from vhaes with br,bi,cr,ci'

        if ierror != 0:
            msg = 'In return from call to vhaes ierror =  %d' % (ierror,)
            raise ValueError, msg

        return br, bi, cr, ci 

    def vhsi(self, nlat,nlon):
        #-----------------------------------------------------------------------------
        #                                      
        #     purpose:  call vhsesi for wvhses
        #
        #     usage:  wvhs, lvhs = x.vhsi(nlat, nlon)
        #
        #-----------------------------------------------------------------------------

        if nlon%2:                              # nlon is odd
            n1 = min(nlat, (nlon + 1)/2)
        else:
            n1 = min(nlat, nlon/2)

        if nlat%2:                              # nlat is odd
            n2 = (nlat + 1)/2
        else:
            n2 = nlat/2

        lwork = 3*(max(n1 - 2,0)*(nlat + nlat - n1 -1))/2 + 5*n2*nlat
        ldwork = 2*(nlat + 1)

        n1 = min(nlat, (nlon + 2)/2)
        lvhses = n1*n2*(nlat + nlat - n1 + 1) + nlon + 15

        # ---  call vhsesi ----
        work = numpy.zeros((lwork,),'f')
        dwork = numpy.zeros((ldwork,),'d')
        wvhses, ierror = spherepack.vhsesi(nlat, nlon, lvhses, work, dwork)

        if ierror != 0 or debug == 1:
            print ' '
            print 'pass to vhsesi'
            print 'nlon = ', nlon
            print 'nlat = ', nlat
            print 'lvhses = ', lvhses
            print 'lwork = ', lwork
            print 'ldwork = ', ldwork
            print 'return from vhsesi with wvhses and lvhses'

        if ierror != 0:
            msg = 'In return from call to vhsies ierror =  %d' % (ierror,)
            raise ValueError, msg

        return wvhses, lvhses 

    def vhs(self, nlat, nlon, nt, br, bi, cr, ci, lvhses, wvhses):
        #-----------------------------------------------------------------------------
        #                                      
        #     purpose: computes the vector harmonic synthesis on a equally spaced grid
        #
        #     usage:   w, v  = x.vhs(nlat, nlon, nt, br, bi, cr, ci, wvhses, lvhses)
        #
        #-----------------------------------------------------------------------------

        if nlon%2:                              # nlon is odd
            n1 = min(nlat, (nlon + 1)/2)
        else:
            n1 = min(nlat, nlon/2)

        if nlat%2:                              # nlat is odd
            n2 = (nlat + 1)/2
        else:
            n2 = nlat/2

        lwork = (2*nt + 1)*nlat*nlon

        ityp = 0

        idvw = nlat
        jdvw = nlon

        mdab = n1
        ndab = nlat

        work = numpy.zeros((lwork,),'f')
        v, w, ierror = spherepack.vhses(nlat, nlon, ityp, idvw, jdvw,
                                        numpy.transpose(br),
                                        numpy.transpose(bi),
                                        numpy.transpose(cr),
                                        numpy.transpose(ci),
                                        wvhses, work)
        v = numpy.transpose(v)
        w = numpy.transpose(w)

        if ierror != 0 or debug == 1:
            print ' '
            print 'pass to vhses'
            print 'nlon = ', nlon
            print 'nlat = ', nlat
            print 'ityp = ', ityp
            print 'nt = ', nt
            print 'idvw = ', idvw
            print 'jdvw = ', jdvw
            print 'mdab = ', mdab
            print 'ndab = ', ndab
            print 'lvhses = ', lvhses
            print 'lwork = ', lwork
            print 'return from vhses with u and v'

        if ierror != 0:
            msg = 'In return from call to vhses ierror =  %d' % (ierror,)
            raise ValueError, msg

        return w, v 

    def vlap(self, nlat, nlon, nt, br, bi, cr, ci, lvhses, wvhses):
        #-----------------------------------------------------------------------------
        #                                      
        #     purpose: computes the vector Laplacian of a given vector function
        #              on a equally spaced grid
        #
        #     usage:   u, v = x.vlap(nlat, nlon, nt, br, bi, cr, ci, wvhses, lvhses)
        #
        #-----------------------------------------------------------------------------

        if nlon%2:                              # nlon is odd
            n1 = min(nlat, (nlon + 1)/2)
        else:
            n1 = min(nlat, (nlon + 2)/2)

        if nlat%2:                              # nlat is odd
            n2 = (nlat + 1)/2
        else:
            n2 = nlat/2

        lwork = (2*nt + 1)*nlat*nlon + nlat*(4*nt*n1 + 1)

        ityp = 0

        idvw = nlat
        jdvw = nlon

        if nlon%2:                              # nlon is odd
            n1 = min(nlat, (nlon + 1)/2)
        else:
            n1 = min(nlat, nlon/2)

        mdbc = n1
        ndbc = nlat

        work = numpy.zeros((lwork,),'f')
        v, w, ierror = spherepack.vlapes(nlat, nlon, ityp, idvw, jdvw,
                                         numpy.transpose(br),
                                         numpy.transpose(bi),
                                         numpy.transpose(cr),
                                         numpy.transpose(ci),
                                         wvhses, work)
        v = numpy.transpose(v)
        w = numpy.transpose(w)

        if ierror != 0 or debug == 1:
            print ' '
            print 'pass to vlapes'
            print 'nlon = ', nlon
            print 'nlat = ', nlat
            print 'ityp = ', ityp
            print 'nt = ', nt
            print 'idvw = ', idvw
            print 'jdvw = ', jdvw
            print 'mdbc = ', mdbc
            print 'ndbc = ', ndbc
            print 'lvhses = ', lvhses
            print 'lwork = ', lwork
            print 'return from vlapes with u and v'

        if ierror != 0:
            msg = 'In return from call to vlapes ierror =  %d' % (ierror,)
            raise ValueError, msg

        return w, v 

    def vrt(self, nlat, nlon, nt, cr, ci, lshses, wshses):
        #-----------------------------------------------------------------------------
        #                                      
        #     purpose: computes the scalar vorticity of a vector function on a
        #              on a equally spaced grid
        #
        #     usage:   vort = x.vrt(nlat, nlon, nt, cr, ci, wshses, lshses)
        #
        #-----------------------------------------------------------------------------

        if nlon%2:                              # nlon is odd
            n1 = min(nlat, (nlon + 1)/2)
        else:
            n1 = min(nlat, nlon/2)

        if nlat%2:                              # nlat is odd
            n2 = (nlat + 1)/2
        else:
            n2 = nlat/2

        isym = 0

        ivrt = nlat
        jvrt = nlon

        mdc = n1
        ndc = nlat

        n1 = min(nlat, nlon/2 + 1)              # from the code -- not the instructions
        lwork = nlat*((nt + 1)*nlon + 2*nt*n1 + 1)

        work = numpy.zeros((lwork,),'f')
        vort, ierror = spherepack.vrtes(nlat, nlon, isym, ivrt, jvrt,
                                        numpy.transpose(cr),
                                        numpy.transpose(ci),
                                        wshses, work)
        vort = numpy.transpose(vort)
        

        if ierror != 0 or debug == 1:
            print ' '
            print 'pass to vortes'
            print 'nlon = ', nlon
            print 'nlat = ', nlat
            print 'isym = ', isym
            print 'nt = ', nt
            print 'ivrt = ', ivrt
            print 'jvrt = ', jvrt
            print 'mdc = ', mdc
            print 'ndc = ', ndc
            print 'lshses = ', lshses
            print 'lwork = ', lwork
            print 'return from vrtes with vort'

        if ierror != 0:
            msg = 'In return from call to vrtes ierror =  %d' % (ierror,)
            raise ValueError, msg

        return vort 

    def vtsi(self, nlat, nlon):
        #-----------------------------------------------------------------------------
        #                                      
        #     purpose: initializes wvts for vtses
        #
        #     usage:   wvts, lwvts = x.vtsi(nlat, nlon)
        #
        #-----------------------------------------------------------------------------

        if nlon%2:                              # nlon is odd
            n1 = min(nlat, (nlon + 1)/2)
        else:
            n1 = min(nlat, nlon/2)

        if nlat%2:                              # nlat is odd
            n2 = (nlat + 1)/2
        else:
            n2 = nlat/2

        lwvts = n1*n2*(nlat + nlat - n1 + 1) + nlon + 15
        lwork = 3*(max(n1 - 2,0)*(nlat + nlat - n1 - 1))/2 + 5*n2*nlat
        ldwork = 2*(nlat + 1) 

        # ---  call vtsesi ----
        work = numpy.zeros((lwork,),'f')
        dwork = numpy.zeros((ldwork,),'d')
        wvts, ierror = spherepack.vtsesi(nlat, nlon, lwvts, work, dwork)

        if ierror != 0 or debug == 1:
            print ' '
            print 'pass to vtsesi'
            print 'nlon = ', nlon
            print 'nlat = ', nlat
            print 'lwvts = ', lwvts
            print 'lwork = ', lwork
            print 'ldwork = ', ldwork
            print 'return from vtsesi with wvts and lwvts'

        if ierror != 0:
            msg = 'In return from call to vtsesi ierror =  %d' % (ierror,)
            raise ValueError, msg

        return wvts, lwvts 

    def vts(self, nlat, nlon, nt, br, bi, cr, ci, lwvts, wvts):
        #-----------------------------------------------------------------------------
        #                                      
        #     purpose: computes the derivative of the vector function with respest
        #              to latitude on an evenly spaced grid
        #
        #     usage:   u, v  = x.vts(nlat, nlon, nt, br, bi, cr, ci, wvts, lwvts)
        #
        #-----------------------------------------------------------------------------

        if nlon%2:                              # nlon is odd
            n1 = min(nlat, (nlon + 1)/2)
        else:
            n1 = min(nlat, nlon/2)

        if nlat%2:                              # nlat is odd
            n2 = (nlat + 1)/2
        else:
            n2 = nlat/2

        lwork = (2*nt + 1)*nlat*nlon

        ityp = 0

        idvw = nlat
        jdvw = nlon

        mdab = n1
        ndab = nlat

        work = numpy.zeros((lwork,),'f')
        v, w, ierror = spherepack.vtses(nlat, nlon, ityp, idvw, jdvw,
                                        numpy.transpose(br),
                                        numpy.transpose(bi),
                                        numpy.transpose(cr),
                                        numpy.transpose(ci),
                                        wvts, work)
        v = numpy.transpose(v)
        w = numpy.transpose(w)

        if ierror != 0 or debug == 1:
            print ' '
            print 'pass to vtses'
            print 'nlon = ', nlon
            print 'nlat = ', nlat
            print 'ityp = ', ityp
            print 'nt = ', nt
            print 'idvw = ', idvw
            print 'jdvw = ', jdvw
            print 'mdab = ', mdab
            print 'ndab = ', ndab
            print 'lwvts = ', lwvts
            print 'lwork = ', lwork
            print 'return from vtses with u and v'

        if ierror != 0:
            msg = 'In return from call to vtses ierror =  %d' % (ierror,)
            raise ValueError, msg

        return w, v 


# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# +++++++++++++++++++++++++ Gaussian Stored Case +++++++++++++++++++++++++++++++++++++
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

class Wrapgs:
    #---------------------------------------------------------------------------------
    #                                      
    #     purpose: provides a first layer 'gridComp' wrapper to assign the array
    #              sizes. The final layer in class Sphere, seen by user,  will call
    #              associated intializations and preliminary functions and then
    #              return the result expected from the name of the call.
    #
    #     usage:  g = Wrapgs()       --  makes an instance
    #
    #---------------------------------------------------------------------------------

    def div(self, nlat, nlon, nt, br, bi, lshsgs, wshsgs):
        #-----------------------------------------------------------------------------
        #                                      
        #     purpose: computes the divergence of a vector function on a gaussian
        #              grid
        #
        #     usage:   dv = g.div(nlat, nlon, nt, br, bi, wshsgs, lshsgs)
        #
        #-----------------------------------------------------------------------------

        if nlon%2:                              # nlon is odd
            n1 = min(nlat, (nlon + 1)/2)
        else:
            n1 = min(nlat, (nlon + 2)/2)

        if nlat%2:                              # nlat is odd
            n2 = (nlat + 1)/2
        else:
            n2 = nlat/2

        lwork = nlat*((nt + 1)*nlon + 2*nt*n1 + 1)

        isym = 0

        idiv = nlat
        jdiv = nlon

        if nlon%2:                              # nlon is odd
            n1 = min(nlat, (nlon + 1)/2)
        else:
            n1 = min(nlat, nlon/2)

        mdb = n1
        ndb = nlat

        work = numpy.zeros((lwork,),'f')
        dv, ierror = spherepack.divgs(nlat, nlon, isym, idiv, jdiv,
                                      numpy.transpose(br),
                                      numpy.transpose(bi),
                                      wshsgs, work)
        dv = numpy.transpose(dv)


        if ierror != 0 or debug == 1:
            print ' '
            print 'pass to divgs'
            print 'nlon = ', nlon
            print 'nlat = ', nlat
            print 'isym = ', isym
            print 'nt = ', nt
            print 'idiv = ', idiv
            print 'jdiv = ', jdiv
            print 'mdb = ', mdb
            print 'ndb = ', ndb
            print 'lshsgs = ', lshsgs
            print 'lwork = ', lwork
            print 'return from divgs with div'

        if ierror != 0:
            msg = 'In return from call to divgs ierror =  %d' % (ierror,)
            raise ValueError, msg

        return dv 

    def grad(self, nlat, nlon, nt, a, b, lvhsgs, wvhsgs):
        #-----------------------------------------------------------------------------
        #                                      
        #     purpose: computes the gradient of a scalar function on a gaussian grid
        #
        #     usage:   u, v = g.grad(nlat, nlon, nt, a, b, wvhsgs, lvhsgs)
        #
        #-----------------------------------------------------------------------------

        if nlon%2:                              # nlon is odd
            n1 = min(nlat, (nlon + 1)/2)
        else:
            n1 = min(nlat, nlon/2)

        if nlat%2:                              # nlat is odd
            n2 = (nlat + 1)/2
        else:
            n2 = nlat/2

        lwork = nlat*((2*nt + 1)*nlon + 2*n1*nt + 1)

        isym = 0 

        idvw = nlat
        jdvw = nlon

        if nlon%2:                              # nlon is odd
            n1 = min(nlat, (nlon + 1)/2)
        else:
            n1 = min(nlat, (nlon + 2)/2)

        mdab = n1
        ndab = nlat

        # ---  call gradgs ----
        work = numpy.zeros((lwork,),'f')
        v, w, ierror = spherepack.gradgs(nlat, nlon, isym, idvw, jdvw,
                                         numpy.transpose(a),
                                         numpy.transpose(b),
                                         wvhsgs, work)
        v = numpy.transpose(v)
        w = numpy.transpose(W)

        if ierror != 0 or debug == 1:
            print ' '
            print 'pass to gradgs'
            print 'nlon = ', nlon
            print 'nlat = ', nlat
            print 'isym = ', isym
            print 'nt = ', nt
            print 'idvw = ', idvw
            print 'jdvw = ', jdvw
            print 'mdab = ', mdab
            print 'ndab = ', ndab
            print 'lvhsgs = ', lvhsgs
            print 'lwork = ', lwork
            print 'return from gradgs with u, v'

        if ierror != 0:
            msg = 'In return from call to gradgs ierror =  %d' % (ierror,)
            raise ValueError, msg

        return w, v 

    def idiv(self, nlat, nlon, nt, a, b, lvhsgs, wvhsgs):
        #-----------------------------------------------------------------------------
        #                                      
        #     purpose: computes an irrotational vector function whose divergence is
        #              given on a gaussian grid.
        #
        #     usage:   u, v = g.idiv(nlat, nlon, nt, a, b, wvhsgs, lvhsgs)
        #
        #-----------------------------------------------------------------------------

        if nlon%2:                              # nlon is odd
            n1 = min(nlat, (nlon + 1)/2)
        else:
            n1 = min(nlat, nlon/2)

        if nlat%2:                              # nlat is odd
            n2 = (nlat + 1)/2
        else:
            n2 = nlat/2

        lwork = (2*nt + 1)*nlat*nlon + nlat*(2*nt*n1 + 1)

        isym = 0

        idvw = nlat
        jdvw = nlon

        if nlon%2:                              # nlon is odd
            n1 = min(nlat, (nlon + 1)/2)
        else:
            n1 = min(nlat, (nlon + 2)/2)

        mdab = n1
        ndab = nlat
        
        work = numpy.zeros((lwork,),'f')
        v, w, pertrb, ierror = spherepack.idivgs(nlat, nlon, isym, idvw, jdvw,
                                                 numpy.transpose(a),
                                                 numpy.transpose(b),
                                                 wvhsgs, work)
        v = numpy.transpose(v)
        w = numpy.transpose(w)

        if ierror != 0 or debug == 1:
            print ' '
            print 'pass to idivgs'
            print 'nlon = ', nlon
            print 'nlat = ', nlat
            print 'isym = ', isym
            print 'nt = ', nt
            print 'idvw = ', idvw
            print 'jdvw = ', jdvw
            print 'mdab = ', mdab
            print 'ndab = ', ndab
            print 'lvhsgs = ', lvhsgs
            print 'lwork = ', lwork
            print 'return from idivgs with u, v'

        if ierror != 0:
            msg = 'In return from call to idivgs ierror =  %d' % (ierror,)
            raise ValueError, msg

        return w, v 

    def idvt(self, nlat, nlon, nt, ad, bd, av, bv, lvhsgs, wvhsgs):
        #-----------------------------------------------------------------------------
        #                                      
        #     purpose: computes a vector function with given divergence and vorticity
        #              on a gaussian grid.
        #
        #     usage:   u, v = g.idvt(nlat, nlon, nt, ad, bd, av, bv, wvhsgs, lvhsgs)
        #
        #-----------------------------------------------------------------------------

        if nlon%2:                              # nlon is odd
            n1 = min(nlat, (nlon + 1)/2)
        else:
            n1 = min(nlat, nlon/2)

        if nlat%2:                              # nlat is odd
            n2 = (nlat + 1)/2
        else:
            n2 = nlat/2

        lwork = nlat*((2*nt + 1)*nlon + 4*nt*n1 + 1)

        isym = 0

        idvw = nlat
        jdvw = nlon

        if nlon%2:                              # nlon is odd
            n1 = min(nlat, (nlon + 1)/2)
        else:
            n1 = min(nlat, (nlon + 2)/2)

        mdab = n1
        ndab = nlat
        
        work = numpy.zeros((lwork,),'f')
        v, w, pertbd, pertbv, ierror = spherepack.idvtgs(nlat, nlon, isym, idvw, jdvw,
                                                         numpy.transpose(ad),
                                                         numpy.transpose(bd),
                                                         numpy.transpose(av),
                                                         numpy.transpose(bv),
                                                         wvhsgs, work)
        v = numpy.transpose(v)
        w = numpy.transpose(w)
        
        if ierror != 0 or debug == 1:
            print ' '
            print 'pass to idvtgs'
            print 'nlon = ', nlon
            print 'nlat = ', nlat
            print 'isym = ', isym
            print 'nt = ', nt
            print 'idvw = ', idvw
            print 'jdvw = ', jdvw
            print 'mdab = ', mdab
            print 'ndab = ', ndab
            print 'lvhsgs = ', lvhsgs
            print 'lwork = ', lwork
            print 'return from idvtgs with u and v'

        if ierror != 0:
            msg = 'In return from call to idvtgs ierror =  %d' % (ierror,)
            raise ValueError, msg

        return w, v 

    def igrad(self, nlat, nlon, nt, lshsgs, wshsgs, br, bi):
        #-----------------------------------------------------------------------------
        #                                      
        #     purpose: computes a scalar function whose gradient is a given vector
        #              function on a gaussian grid
        #
        #     usage:   sf = g.igrad(nlat, nlon, nt, lshsgs, wshsgs, br, bi)
        #
        #-----------------------------------------------------------------------------

        if nlon%2:                              # nlon is odd
            n1 = min(nlat, (nlon + 1)/2)
        else:
            n1 = min(nlat, (nlon + 2)/2)

        if nlat%2:                              # nlat is odd
            n2 = (nlat + 1)/2
        else:
            n2 = nlat/2

        lwork = nlat*((nt + 1)*nlon + 2*nt*n1 + 1)

        isym = 0 

        isf = nlat
        jsf = nlon

        if nlon%2:                              # nlon is odd
            n1 = min(nlat, (nlon + 1)/2)
        else:
            n1 = min(nlat, nlon/2)

        mdb = n1
        ndb = nlat

        # ---  call gradgs ----
        work = numpy.zeros((lwork,),'f')
        sf, ierror = spherepack.igradgs(nlat, nlon, isym, isf, jsf,
                                        numpy.transpose(br),
                                        numpy.transpose(bi),
                                        wshsgs, work)
        sf = numpy.transpose(sf)
        

        if ierror != 0 or debug == 1:
            print ' '
            print 'pass to igradgs'
            print 'nlon = ', nlon
            print 'nlat = ', nlat
            print 'isym = ', isym
            print 'nt = ', nt
            print 'isf = ', isf
            print 'jsf = ', jsf
            print 'mdb = ', mdb
            print 'ndb = ', ndb
            print 'lshsgs = ', lshsgs
            print 'lwork = ', lwork
            print 'return from igradgs with sf'

        if ierror != 0:
            msg = 'In return from call to igradgs ierror =  %d' % (ierror,)
            raise ValueError, msg

        return sf 

    def isfvp(self, nlat, nlon, nt, as_, bs, av, bv, lvhsgs, wvhsgs):
        #-----------------------------------------------------------------------------
        #                                      
        #     purpose: computes a vector function with a given stream function and
        #              velocity potential on a gaussian grid
        #
        #     usage:   u, v, ierror = g.isfvp(nlat, nlon, nt, as_, bs, av, bv, wvhsgs, lvhsgs)
        #
        #-----------------------------------------------------------------------------

        if nlon%2:                              # nlon is odd
            n1 = min(nlat, (nlon + 1)/2)
        else:
            n1 = min(nlat, nlon/2)

        if nlat%2:                              # nlat is odd
            n2 = (nlat + 1)/2
        else:
            n2 = nlat/2

        lwork = nlat*((2*nt + 1)*nlon + 4*n1*nt + 1)

        isym = 0

        idv = nlat
        jdv = nlon

        mdb = n1
        ndb = nlat

        work = numpy.zeros((lwork,),'f')
        v, w, ierror = spherepack.isfvpgs(nlat, nlon, isym, idv, jdv,
                                          numpy.transpose(as_),
                                          numpy.transpose(bs),
                                          numpy.transpose(av),
                                          numpy.transpose(bv),
                                          wvhsgs, work)
        v = numpy.transpose(v)
        w = numpy.transpose(w)
        
        if ierror != 0 or debug == 1:
            print ' '
            print 'pass to isfvpgs'
            print 'nlon = ', nlon
            print 'nlat = ', nlat
            print 'isym = ', isym
            print 'nt = ', nt
            print 'idv = ', idv
            print 'jdv = ', jdv
            print 'mdb = ', mdb
            print 'ndb = ', ndb
            print 'lvhsgs = ', lvhsgs
            print 'lwork = ', lwork
            print 'return from isfvpgs with u and v'

        if ierror != 0:
            msg = 'In return from call to isfvpgs ierror =  %d' % (ierror,)
            raise ValueError, msg

        return w, v 

    def islap(self, nlat, nlon, nt, lshsgs, wshsgs, a, b):
        #-----------------------------------------------------------------------------
        #                                      
        #     purpose: computes a scalar function whose scalar Laplacian is given on
        #              a gaussian grid
        #
        #     usage:   sf = g.islap(nlat, nlon, nt, lshsgs, wshsgs, a, b)
        #
        #-----------------------------------------------------------------------------

        if nlon%2:                              # nlon is odd
            n1 = min(nlat, (nlon + 1)/2)
        else:
            n1 = min(nlat, (nlon + 2)/2)

        if nlat%2:                              # nlat is odd
            n2 = (nlat + 1)/2
        else:
            n2 = nlat/2

        lwork = (nt + 1)*nlat*nlon + nlat*(2*nt*n1 + 1)

        isym = 0 

        ids = nlat
        jds = nlon

        mdab = n1
        ndab = nlat

        xlmbda = numpy.array( [0.0]*nt, numpy.float32)      # call for Poisson rather than Helmholtz

        # ---  call islapgs ----
        work = numpy.zeros((lwork,),'f')
        sf, pertrb, ierror = spherepack.islapgs(nlat, nlon, isym, xlmbda, ids, jds,
                                                numpy.transpose(a),
                                                numpy.transpose(b),
                                                wshsgs, work)
        sf = numpy.transpose(sf)
        
        if ierror != 0 or debug == 1:
            print ' '
            print 'pass to islapgs'
            print 'nlon = ', nlon
            print 'nlat = ', nlat
            print 'isym = ', isym
            print 'nt = ', nt
            print 'ids = ', ids
            print 'jds = ', jds
            print 'mdab = ', mdab
            print 'ndab = ', ndab
            print 'lshsgs = ', lshsgs
            print 'lwork = ', lwork
            print 'return from islapgs with sf'

        if ierror != 0:
            msg = 'In return from call to islapgs ierror =  %d' % (ierror,)
            raise ValueError, msg

        return sf 

    def ivlap(self, nlat, nlon, nt, br, bi, cr, ci, lvhsgs, wvhsgs):
        #-----------------------------------------------------------------------------
        #                                      
        #     purpose: computes a vector function whose Laplacian is a given vector
        #              vector function on a gaussian grid
        #
        #     usage:   u, v = g.ivlap(nlat, nlon, nt, br, bi, cr, ci, wvhsgs, lvhsgs)
        #
        #-----------------------------------------------------------------------------

        if nlon%2:                              # nlon is odd
            n1 = min(nlat, (nlon + 1)/2)
        else:
            n1 = min(nlat, nlon/2)

        if nlat%2:                              # nlat is odd
            n2 = (nlat + 1)/2
        else:
            n2 = nlat/2

        lwork = (2*nt + 1)*nlat*nlon + nlat*(4*nt*n1 + 1)

        ityp = 0

        idvw = nlat
        jdvw = nlon

        mdbc = n1
        ndbc = nlat

        work = numpy.zeros((lwork,),'f')
        v, w, ierror = spherepack.ivlapgs(nlat, nlon, ityp, idvw, jdvw,
                                          numpy.transpose(br),
                                          numpy.transpose(bi),
                                          numpy.transpose(cr),
                                          numpy.transpose(ci),
                                          wvhsgs, work)
        v = numpy.transpose(v)
        w = numpy.transpose(w)
        
        if ierror != 0 or debug == 1:
            print ' '
            print 'pass to ivlapgs'
            print 'nlon = ', nlon
            print 'nlat = ', nlat
            print 'ityp = ', ityp
            print 'nt = ', nt
            print 'idvw = ', idvw
            print 'jdvw = ', jdvw
            print 'mdbc = ', mdbc
            print 'ndbc = ', ndbc
            print 'lvhsgs = ', lvhsgs
            print 'lwork = ', lwork
            print 'return from ivlapgs with u and v'

        if ierror != 0:
            msg = 'In return from call to ivlapgs ierror =  %d' % (ierror,)
            raise ValueError, msg

        return w, v 

    def ivrt(self, nlat, nlon, nt, a, b, lvhsgs, wvhsgs):
        #-----------------------------------------------------------------------------
        #                                      
        #     purpose: computes a divergence-free vector function whose vorticity is
        #              given on a gaussian grid
        #
        #     usage:   w, v, ierror = g.ivrt(nlat, nlon, nt, a, b, wvhsgs, lvhsgs)
        #
        #-----------------------------------------------------------------------------

        if nlon%2:                              # nlon is odd
            n1 = min(nlat, (nlon + 1)/2)
        else:
            n1 = min(nlat, nlon/2)

        if nlat%2:                              # nlat is odd
            n2 = (nlat + 1)/2
        else:
            n2 = nlat/2

        lwork = (2*nt + 1)*nlat*nlon + nlat*(2*nt*n1 + 1)

        isym = 0

        idvw = nlat
        jdvw = nlon

        if nlon%2:                              # nlon is odd
            n1 = min(nlat, (nlon + 1)/2)
        else:
            n1 = min(nlat, (nlon + 2)/2)

        mdab = n1
        ndab = nlat

        work = numpy.zeros((lwork,),'f')
        v, w, pertrb, ierror = spherepack.ivrtgs(nlat, nlon, isym, idvw, jdvw,
                                                 numpy.transpose(a),
                                                 numpy.transpose(b),
                                                 wvhsgs, work)
        v = numpy.transpose(v)
        w = numpy.transpose(w)
        

        if ierror != 0 or debug == 1:
            print ' '
            print 'pass to ivrtgs'
            print 'nlon = ', nlon
            print 'nlat = ', nlat
            print 'isym = ', isym
            print 'nt = ', nt
            print 'idvw = ', idvw
            print 'jdvw = ', jdvw
            print 'mdab = ', mdab
            print 'ndab = ', ndab
            print 'lvhsgs = ', lvhsgs
            print 'lwork = ', lwork
            print 'return from ivrtgs with u, v'

        if ierror != 0:
            msg = 'In return from call to ivrtgs ierror =  %d' % (ierror,)
            raise ValueError, msg

        return w, v 

    def sfvp(self, nlat, nlon, nt, br, bi, cr, ci, lshsgs, wshsgs):
        #-----------------------------------------------------------------------------
        #                                      
        #     purpose: computes the stream function and the velocity potential of a 
        #              vector function on a gaussian grid
        #
        #     usage:   sf, vp = g.sfvp(nlat, nlon, nt, br, bi, cr, ci, wshsgs, lshsgs)
        #
        #-----------------------------------------------------------------------------


        if nlon%2:                              # nlon is odd
            n1 = min(nlat, (nlon + 1)/2)
        else:
            n1 = min(nlat, (nlon + 2)/2)

        if nlat%2:                              # nlat is odd
            n2 = (nlat + 1)/2
        else:
            n2 = nlat/2

        lwork = nlat*nlon*(nt + 1) + nlat*(2*n1*nt + 1)

        isym = 0

        idv = nlat
        jdv = nlon

        if nlon%2:                              # nlon is odd
            n1 = min(nlat, (nlon + 1)/2)
        else:
            n1 = min(nlat, nlon/2)

        mdb = n1
        ndb = nlat

        work = numpy.zeros((lwork,),'f')
        sf, vp, ierror = spherepack.sfvpgs(nlat, nlon, isym, idv, jdv,
                                           numpy.transpose(br),
                                           numpy.transpose(bi),
                                           numpy.transpose(cr),
                                           numpy.transpose(ci),
                                           wshsgs, work)
        sf = numpy.transpose(sp)
        vp = numpy.transpose(vp)
        

        if ierror != 0 or debug == 1:
            print ' '
            print 'pass to sfvpgs'
            print 'nlon = ', nlon
            print 'nlat = ', nlat
            print 'isym = ', isym
            print 'nt = ', nt
            print 'idv = ', idv
            print 'jdv = ', jdv
            print 'mdb = ', mdb
            print 'ndb = ', ndb
            print 'lshsgs = ', lshsgs
            print 'lwork = ', lwork
            print 'return from sfvpgs with sf and vp'

        if ierror != 0:
            msg = 'In return from call to sfvpgs ierror =  %d' % (ierror,)
            raise ValueError, msg

        return sf, vp 

    def shai(self, nlat, nlon):
        #-----------------------------------------------------------------------------
        #                                      
        #     purpose:  call shai for wshags and lshags
        #
        #     usage:  wshags, lshags = g.shai(nlat, nlon)
        #
        #-----------------------------------------------------------------------------

        if nlon%2:                              # nlon is odd
            n1 = min(nlat, (nlon + 1)/2)
        else:
            n1 = min(nlat, (nlon + 2)/2)

        if nlat%2:                              # nlat is odd
            n2 = (nlat + 1)/2
        else:
            n2 = nlat/2

        lshags = nlat*(3*(n1 + n2) - 2) + (n1 - 1)*(n2*(2*nlat - n1) - 3*n1)/2 + nlon + 15
        lwork = 4*nlat*(nlat + 2) + 2
        ldwork = nlat*(nlat + 4)


        # ---  call shagsi ----
        work = numpy.zeros((lwork,),'f')
        dwork = numpy.zeros((ldwork,),'d')
        wshags, ierror = spherepack.shagsi(nlat, nlon, lshags, work, dwork)

        if ierror != 0 or debug == 1:
            print ' '
            print 'pass to shagsi'
            print 'nlon = ', nlon
            print 'nlat = ', nlat
            print 'lshags = ', lshags
            print 'lwork = ', lwork
            print 'ldwork = ', ldwork
            print 'return from shagsi with wshags and lshags'

        if ierror != 0:
            msg = 'In return from call to shagsi ierror =  %d' % (ierror,)
            raise ValueError, msg

        return wshags, lshags

    def sha(self, nlat, nlon, nt, lshags, wshags, g):
        #-----------------------------------------------------------------------------
        #                                      
        #     purpose: computes the spherical harmonic analysis on a gaussian grid
        #
        #     usage:   a, b = g.sha(nlat, nlon, nt, lshags, wshags, g)
        #
        #-----------------------------------------------------------------------------

        if nlon%2:                              # nlon is odd
            n1 = min(nlat, (nlon + 1)/2)
        else:
            n1 = min(nlat, (nlon + 2)/2)

        if nlat%2:                              # nlat is odd
            n2 = (nlat + 1)/2
        else:
            n2 = nlat/2

        lwork = nlat*nlon*(nt + 1)

        isym = 0

        idg = nlat
        jdg = nlon

        mdab = n1
        ndab = nlat

        # ---  call shags ----
        work = numpy.zeros((lwork,),'f')
        a, b, ierror = spherepack.shags(nlat, nlon, isym,
                                        numpy.transpose(g),
                                        mdab, ndab,
                                        wshags, work)
        a = numpy.transpose(a)
        b = numpy.transpose(b)
        

        if ierror != 0 or debug == 1:
            print ' '
            print 'pass to shags'
            print 'nlon = ', nlon
            print 'nlat = ', nlat
            print 'isym = ', isym
            print 'nt = ', nt
            print 'idg = ', idg
            print 'jdg = ', jdg
            print 'mdab = ', mdab
            print 'ndab = ', ndab
            print 'lshags = ', lshags
            print 'lwork = ', lwork
            print 'return from shags with a, b'

        if ierror != 0:
            msg = 'In return from call to shags ierror =  %d' % (ierror,)
            raise ValueError, msg

        return a, b 

    def shsi(self, nlat, nlon):
        #-----------------------------------------------------------------------------
        #                                      
        #     purpose:  call shsgsi for wshsgs and lshsgs
        #
        #     usage:  wshsgs, lshsgs = g.shsi(nlat, nlon)
        #
        #-----------------------------------------------------------------------------

        if nlon%2:                              # nlon is odd
            n1 = min(nlat, (nlon + 1)/2)
        else:
            n1 = min(nlat, (nlon + 2)/2)

        if nlat%2:                              # nlat is odd
            n2 = (nlat + 1)/2
        else:
            n2 = nlat/2

        lshsgs = nlat*(3*(n1 + n2) -2) + (n1 - 1)*(n2*(2*nlat - n1) - 3*n1)/2 + nlon + 15
        lwork = 4*nlat*(nlat + 2) + 2 
        ldwork = nlat*(nlat + 4)

        # ---  call shsgsi ----
        work = numpy.zeros((lwork,),'f')
        dwork = numpy.zeros((ldwork,),'d')
        wshsgs, ierror = spherepack.shsgsi(nlat, nlon, lshsgs, work, dwork)

        if ierror != 0 or debug == 1:
            print ' '
            print 'pass to shsgsi'
            print 'nlon = ', nlon
            print 'nlat = ', nlat
            print 'lshsgs = ', lshsgs
            print 'lwork = ', lwork
            print 'ldwork = ', ldwork
            print 'return from shsgsi with wshsgs and lshsgs'

        if ierror != 0:
            msg = 'In return from call to shsgsi ierror =  %d' % (ierror,)
            raise ValueError, msg

        return wshsgs, lshsgs 

    def shs(self, nlat, nlon, nt, lshsgs, wshsgs, a, b):
        #-----------------------------------------------------------------------------
        #                                      
        #     purpose: computes the spherical harmonic synthesis on a gaussian grid
        #
        #     usage:   g = g.shs(nlat,nlon, nt, lshsgs, wshsgs, a, b)
        #
        #-----------------------------------------------------------------------------

        if nlon%2:                              # nlon is odd
            n1 = min(nlat, (nlon + 1)/2)
        else:
            n1 = min(nlat, (nlon + 2)/2)

        if nlat%2:                              # nlat is odd
            n2 = (nlat + 1)/2
        else:
            n2 = nlat/2

        lwork = nlat*nlon*(nt + 1) 

        mode = 0 

        idg = nlat
        jdg = nlon

        mdab = n1
        ndab = nlat

        # ---  call shsgs ----
        work = numpy.zeros((lwork,),'f')
        g, ierror = spherepack.shsgs(nlat, nlon, mode, idg, jdg,
                                     numpy.transpose(a),
                                     numpy.transpose(b),
                                     wshsgs, work)

        if ierror != 0 or debug == 1:
            print ' '
            print 'pass to shsgs'
            print 'nlon = ', nlon
            print 'nlat = ', nlat
            print 'mode = ', mode
            print 'nt = ', nt
            print 'idg = ', idg
            print 'jdg = ', jdg
            print 'mdab = ', mdab
            print 'ndab = ', ndab
            print 'lshsgs = ', lshsgs
            print 'lwork = ', lwork
            print 'return from shsgs with g'

        if ierror != 0:
            msg = 'In return from call to shsgs ierror =  %d' % (ierror,)
            raise ValueError, msg

        return g 

    def slap(self, nlat, nlon, nt, lshsgs, wshsgs, a, b):
        #-----------------------------------------------------------------------------
        #                                      
        #     purpose: computes a scalar Laplacian of a scalar function on a gaussian
        #              grid
        #
        #     usage:   slap = g.slap(nlat, nlon, nt, lshsgs, wshsgs, a, b)
        #
        #-----------------------------------------------------------------------------

        if nlon%2:                              # nlon is odd
            n1 = min(nlat, (nlon + 1)/2)
        else:
            n1 = min(nlat, (nlon + 2)/2)

        if nlat%2:                              # nlat is odd
            n2 = (nlat + 1)/2
        else:
            n2 = nlat/2

        lwork = (nt + 1)*nlat*nlon + nlat*(2*nt*n1 + 1)

        isym = 0 

        ids = nlat
        jds = nlon

        mdab = n1
        ndab = nlat

        # ---  call slapgs ----
        work = numpy.zeros((lwork,),'f')
        slap, ierror = spherepack.slapgs(nlat, nlon, isym, ids, jds,
                                         numpy.transpose(a),
                                         numpy.transpose(b),
                                         wshsgs, work)

        if ierror != 0 or debug == 1:
            print ' '
            print 'pass to slapgs'
            print 'nlon = ', nlon
            print 'nlat = ', nlat
            print 'isym = ', isym
            print 'nt = ', nt
            print 'ids = ', ids
            print 'jds = ', jds
            print 'mdab = ', mdab
            print 'ndab = ', ndab
            print 'lshsgs = ', lshsgs
            print 'lwork = ', lwork
            print 'return from slapgs with slap'

        if ierror != 0:
            msg = 'In return from call to slapgs ierror =  %d' % (ierror,)
            raise ValueError, msg

        return slap 

    def vhai(self, nlat, nlon):
        #-----------------------------------------------------------------------------
        #                                      
        #     purpose:  call vhagsi for wvhags and lvhags
        #
        #     usage:  wvhags, lvhags = g.vhai(nlat, nlon)
        #
        #
        #-----------------------------------------------------------------------------

        if nlon%2:                              # nlon is odd
            n1 = min(nlat, (nlon + 1)/2)
        else:
            n1 = min(nlat, nlon/2)


        if nlat%2:                              # nlat is odd
            n2 = (nlat + 1)/2
        else:
            n2 = nlat/2

        lvhags = (nlat +1)*(nlat + 1)*nlat/2 +nlon + 15
        ldwork = (3*nlat*(nlat + 3) + 2)/2

        # ---  call vhagsi ----
        work = numpy.zeros((ldwork,),'d')
        wvhags, ierror = spherepack.vhagsi(nlat, nlon, lvhags, work)

        if ierror != 0 or debug == 1:
            print ' '
            print 'pass to vhagsi'
            print 'nlon = ', nlon
            print 'nlat = ', nlat
            print 'lvhags = ', lvhags
            print 'ldwork = ', ldwork
            print 'return from vhagsi with wvhags'

        if ierror != 0:
            msg = 'In return from call to vhagsi ierror =  %d' % (ierror,)
            raise ValueError, msg

        return wvhags, lvhags 

    def vha(self, nlat, nlon, nt, lvhags, wvhags, w, v):
        #-----------------------------------------------------------------------------
        #                                      
        #     purpose: computes the vector harmonic analysis on a gaussian grid
        #
        #     usage:   br, bi, cr, ci = g.vhags(nlat, nlon, nt, lvhags, wvhags, v, w)
        #
        #-----------------------------------------------------------------------------


        if nlon%2:                              # nlon is odd
            n1 = min(nlat, (nlon + 1)/2)
        else:
            n1 = min(nlat, nlon/2)

        if nlat%2:                              # nlat is odd
            n2 = (nlat + 1)/2
        else:
            n2 = nlat/2

        lwork = (max( (3*nlat*(nlat + 1) + 2), (2*nt + 1)*nlat*nlon) )

        ityp = 0

        idvw = nlat
        jdvw = nlon

        mdab = n1
        ndab = nlat

        # ---  call vhags ----
        work = numpy.zeros((lwork,),'f')
        br, bi, cr, ci, ierror = spherepack.vhags(nlat, nlon, ityp,
                                                  numpy.transpose(v),
                                                  numpy.transpose(w),
                                                  mdab, ndab,
                                                  wvhags, work)

        if ierror != 0 or debug == 1:
            print ' '
            print 'pass to vhags'
            print 'nlon = ', nlon
            print 'nlat = ', nlat
            print 'ityp = ', ityp
            print 'nt = ', nt
            print 'idvw = ', idvw
            print 'jdvw = ', jdvw
            print 'mdab = ', mdab
            print 'ndab = ', ndab
            print 'lvhags = ', lvhags
            print 'lwork = ', lwork
            print 'return from vhags with br,bi,cr,ci'

        if ierror != 0:
            msg = 'In return from call to vhags ierror =  %d' % (ierror,)
            raise ValueError, msg

        return br, bi, cr, ci 

    def vhsi(self, nlat, nlon):
        #-----------------------------------------------------------------------------
        #                                      
        #     purpose:  call vhsgsi for wvhsgs
        #
        #     usage:  wvhsgs, lvhsgs = g.vhsi(nlat, nlon)
        #
        #-----------------------------------------------------------------------------

        if nlon%2:                              # nlon is odd
            n1 = min(nlat, (nlon + 1)/2)
        else:
            n1 = min(nlat, nlon/2)

        if nlat%2:                              # nlat is odd
            n2 = (nlat + 1)/2
        else:
            n2 = nlat/2

        lvhsgs = n1*n2*(nlat + nlat -n1 +1) + nlon + 15 + 2*nlat
        ldwork = (3*nlat*(nlat + 3) + 2)/2 

        # ---  call vhsgsi ----
        work = numpy.zeros((ldwork,),'d')
        wvhsgs, ierror = spherepack.vhsgsi(nlat, nlon, lvhsgs, work)

        if ierror != 0 or debug == 1:
            print ' '
            print 'pass to vhsgsi'
            print 'nlon = ', nlon
            print 'nlat = ', nlat
            print 'lvhsgs = ', lvhsgs
            print 'ldwork = ', ldwork
            print 'return from vhsgsi with wvhsgs'

        if ierror != 0:
            msg = 'In return from call to vhsigs ierror =  %d' % (ierror,)
            raise ValueError, msg

        return wvhsgs, lvhsgs 

    def vhs(self, nlat, nlon, nt, br, bi, cr, ci, lvhsgs, wvhsgs):
        #-----------------------------------------------------------------------------
        #                                      
        #     purpose: computes the vector harmonic synthesis on a gaussian grid
        #
        #     usage:   u, v = g.vhs(nlat, nlon, nt, br, bi, cr, ci, wvhsgs, lvhsgs)
        #
        #-----------------------------------------------------------------------------

        if nlon%2:                              # nlon is odd
            n1 = min(nlat, (nlon + 1)/2)
        else:
            n1 = min(nlat, nlon/2)

        if nlat%2:                              # nlat is odd
            n2 = (nlat + 1)/2
        else:
            n2 = nlat/2

        lwork = (2*nt + 1)*nlat*nlon 

        ityp = 0

        idvw = nlat
        jdvw = nlon

        mdab = n1
        ndab = nlat

        work = numpy.zeros((lwork,),'f')
        v, w, ierror = spherepack.vhsgs(nlat, nlon, ityp, idvw, jdvw,
                                        numpy.transpose(br),
                                        numpy.transpose(bi),
                                        numpy.transpose(cr),
                                        numpy.transpose(ci),
                                        wvhsgs, work)
        v = numpy.transpose(v)
        w = numpy.transpose(w)

        if ierror != 0 or debug == 1:
            print ' '
            print 'pass to vhsgs'
            print 'nlon = ', nlon
            print 'nlat = ', nlat
            print 'ityp = ', ityp
            print 'nt = ', nt
            print 'idvw = ', idvw
            print 'jdvw = ', jdvw
            print 'mdab = ', mdab
            print 'ndab = ', ndab
            print 'lvhsgs = ', lvhsgs
            print 'lwork = ', lwork
            print 'return from vhsgs with u and v'

        if ierror != 0:
            msg = 'In return from call to vhsgs ierror =  %d' % (ierror,)
            raise ValueError, msg

        return w, v 

    def vlap(self, nlat, nlon, nt, br, bi, cr, ci, lvhsgs, wvhsgs):
        #-----------------------------------------------------------------------------
        #                                      
        #     purpose: computes the vector Laplacian of a given vector function
        #              on a gaussian grid
        #
        #     usage:   u, v = g.vlap(nlat, nlon, nt, br, bi, cr, ci, wvhsgs, lvhsgs)
        #
        #-----------------------------------------------------------------------------

        if nlon%2:                              # nlon is odd
            n1 = min(nlat, (nlon + 1)/2)
        else:
            n1 = min(nlat, (nlon + 2)/2)

        if nlat%2:                              # nlat is odd
            n2 = (nlat + 1)/2
        else:
            n2 = nlat/2

        lwork = (2*nt + 1)*nlat*nlon + nlat*(4*nt*n1 + 1)

        ityp = 0

        idvw = nlat
        jdvw = nlon

        if nlon%2:                              # nlon is odd
            n1 = min(nlat, (nlon + 1)/2)
        else:
            n1 = min(nlat, nlon/2)

        mdbc = n1
        ndbc = nlat


        work = numpy.zeros((lwork,),'f')
        v, w, ierror = spherepack.vlapgs(nlat, nlon, ityp, idvw, jdvw,
                                         numpy.transpose(br),
                                         numpy.transpose(bi),
                                         numpy.transpose(cr),
                                         numpy.transpose(ci),
                                         wvhsgs, work)
        v = numpy.transpose(v)
        w = numpy.transpose(w)


        if ierror != 0 or debug == 1:
            print ' '
            print 'pass to vlapgs'
            print 'nlon = ', nlon
            print 'nlat = ', nlat
            print 'ityp = ', ityp
            print 'nt = ', nt
            print 'idvw = ', idvw
            print 'jdvw = ', jdvw
            print 'mdbc = ', mdbc
            print 'ndbc = ', ndbc
            print 'lvhsgs = ', lvhsgs
            print 'lwork = ', lwork
            print 'return from vlapgs with u and v'

        if ierror != 0:
            msg = 'In return from call to vlapgs ierror =  %d' % (ierror,)
            raise ValueError, msg

        return w, v 

    def vrt(self, nlat, nlon, nt, cr, ci, lshsgs, wshsgs):
        #-----------------------------------------------------------------------------
        #                                      
        #     purpose: computes the scalar vorticity of a vector function on a
        #              gaussian grid
        #
        #     usage:   vort = g.vrt(nlat, nlon, nt, cr, ci, wshsgs, lshsgs)
        #
        #-----------------------------------------------------------------------------

        if nlon%2:                              # nlon is odd
            n1 = min(nlat, (nlon + 1)/2)
        else:
            n1 = min(nlat, nlon/2)

        if nlat%2:                              # nlat is odd
            n2 = (nlat + 1)/2
        else:
            n2 = nlat/2

        lwork = nlat*((nt + 1)*nlon + 2*nt*n1 + 1)

        isym = 0

        ivrt = nlat
        jvrt = nlon

        mdc = n1
        ndc = nlat

        work = numpy.zeros((lwork,),'f')
        vort, ierror = spherepack.vrtgs(nlat, nlon, isym, ivrt, jvrt,
                                        numpy.transpose(cr),
                                        numpy.transpose(ci),
                                        wshsgs, work)
        vort = numpy.transpose(vort)
        
        if ierror != 0 or debug == 1:
            print ' '
            print 'pass to vortgs'
            print 'nlon = ', nlon
            print 'nlat = ', nlat
            print 'isym = ', isym
            print 'nt = ', nt
            print 'ivrt = ', ivrt
            print 'jvrt = ', jvrt
            print 'mdc = ', mdc
            print 'ndc = ', ndc
            print 'lshsgs = ', lshsgs
            print 'lwork = ', lwork
            print 'return from vrtgs with vort'

        if ierror != 0:
            msg = 'In return from call to vrtgs ierror =  %d' % (ierror,)
            raise ValueError, msg

        return vort 

    def vtsi(self, nlat, nlon):
        #-----------------------------------------------------------------------------
        #                                      
        #     purpose: initializes wvts for vtsgs
        #
        #     usage:   wvts, lwvts = g.vtsi(nlat, nlon)
        #
        #-----------------------------------------------------------------------------

        if nlon%2:                              # nlon is odd
            n1 = min(nlat, (nlon + 1)/2)
        else:
            n1 = min(nlat, nlon/2)

        if nlat%2:                              # nlat is odd
            n2 = (nlat + 1)/2
        else:
            n2 = nlat/2

        lwvts = n1*n2*(nlat + nlat - n1 - 1) + nlon + 15
        lwork = 3*(max(n1 - 2, 0)*(nlat + nlat - n1 - 1))/2 + (5*n2 + 2)*nlat
        ldwork = nlat*(nlat + 2) 

        # ---  call vtsgsi ----
        work = numpy.zeros((lwork,),'f')
        dwork = numpy.zeros((ldwork,),'d')
        wvts, ierror = spherepack.vtsgsi(nlat, nlon, lwvts, work, dwork)

        if ierror != 0 or debug == 1:
            print ' '
            print 'pass to vtsgsi'
            print 'nlon = ', nlon
            print 'nlat = ', nlat
            print 'lwvts = ', lwvts
            print 'ldwork = ', ldwork
            print 'return from vtsgsi with wvts and lwvts'

        if ierror != 0:
            msg = 'In return from call to vtsgsi ierror =  %d' % (ierror,)
            raise ValueError, msg

        return wvts, lwvts 

    def vts(self, nlat, nlon, nt, br, bi, cr, ci, lwvts, wvts):
        #-----------------------------------------------------------------------------
        #                                      
        #     purpose: computes the derivative of the vector function with respect
        #              to latitude on a gaussian grid
        #
        #     usage:   w, v = g.vts(nlat, nlon, nt, br, bi, cr, ci, wvts, lwvts)
        #
        #-----------------------------------------------------------------------------

        if nlon%2:                              # nlon is odd
            n1 = min(nlat, (nlon + 1)/2)
        else:
            n1 = min(nlat, nlon/2)

        if nlat%2:                              # nlat is odd
            n2 = (nlat + 1)/2
        else:
            n2 = nlat/2

        lwork = (2*nt + 1)*nlat*nlon 

        ityp = 0

        idvw = nlat
        jdvw = nlon

        mdab = n1
        ndab = nlat

        work = numpy.zeros((lwork,),'f')
        v, w, ierror = spherepack.vtsgs(nlat, nlon, ityp, idvw, jdvw,
                                        numpy.transpose(br),
                                        numpy.transpose(bi),
                                        numpy.transpose(cr),
                                        numpy.transpose(ci),
                                        wvts, work)

        if ierror != 0 or debug == 1:
            print ' '
            print 'pass to vtsgs'
            print 'nlon = ', nlon
            print 'nlat = ', nlat
            print 'ityp = ', ityp
            print 'nt = ', nt
            print 'idvw = ', idvw
            print 'jdvw = ', jdvw
            print 'mdab = ', mdab
            print 'ndab = ', ndab
            print 'lwvts = ', lwvts
            print 'lwork = ', lwork
            print 'return from vtsgs with u and v'

        if ierror != 0:
            msg = 'In return from call to vtsgs ierror =  %d' % (ierror,)
            raise ValueError, msg

        return w, v 


# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# +++++++++++++++++++++++++++++ Regrid class +++++++++++++++++++++++++++++++++++++++++
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

class Regrid:
    #---------------------------------------------------------------------------------------
    # 
    #       Contents of the Regrid class
    #                        
    #           The two functions for regridding are:
    #            
    #            regridScalar --  transfers scalar data from one global grid to another                  
    #            regridVector --  transfers vector data from one global grid to another                  
    #                
    #---------------------------------------------------------------------------------------

    def __init__(self, lonArrayOut, latArrayOut, lonArrayIn, latArrayIn, numberLevels = None,
                                                                               numberTimes = None): 
        """    --------------------------------------------------------------------------------------------------------
         
         purpose:  'init' for class Regrid assigns values to the instance data which are the dimensions
                    lengths, the latitude direction and the latitude type as gaussian or even.
          
         usage:     x = sphere.Regrid(lonArrayOut, latArrayOut, lonArrayIn, latArrayIn, nlev, ntime)
         
                    where nlev and ntime are the actual number of levels and times respectively.
         
         passed:    lonArrayOut, lonArrayIn = longitude vectors
                    latArrayOut, latArrayIn = latitude vectors
                    numberLevels = number of levels (optional)
                    numberTimes = number of times (optional)
         
         returned:  x instance of Regrid
         
         definition: __init__(self, lonArrayOut, latArrayOut, lonArrayIn, latArrayIn, numberLevels = None,
                                                                                            numberTimes = None): 
    --------------------------------------------------------------------------------------------------------""" 
        self.lonIn = len(lonArrayIn)
        self.latIn = len(latArrayIn)

        self.lonOut = len(lonArrayOut)
        self.latOut = len(latArrayOut)

        self.reverseLatitude = 'no'           # regrid handles latitude north-south or south_north

        # ------ determine the standardShape for the input and output data for subsequent use ------   

        dimlistIn = [self.latIn, self.lonIn]                    # math order
        dimlistOut = [self.latOut, self.lonOut]

        self.levIn = numberLevels
        if numberLevels is not None and  numberLevels != 0:
            dimlistIn.append(self.levIn)
            dimlistOut.append(self.levIn)

        self.tmeIn = numberTimes
        if numberTimes is not None and  numberTimes != 0:
            dimlistIn.append(self.tmeIn)
            dimlistOut.append(self.tmeIn)
      
        dimlistIn.reverse()
        self.standardShapeIn = tuple(dimlistIn)                  # math order (ntme,nlev,nlon,nlat)
        dimlistOut.reverse()
        self.standardShapeOut = tuple(dimlistOut)   

        # ------ check the shape for a unique number of longitudes and a unique number of latitudes ------  

        if self.lonIn in [self.tmeIn, self.levIn, self.latIn]:
            print 'Warning - number of longitudes in duplicated in the shape. The geotomath shape \
                   transform will not work unless it differs from the number of latitudes and it \
                   is one of the last two entiries in the shape'
        if self.latIn in [self.tmeIn, self.levIn, self.lonIn]:
            print 'Warning - number of latitudes in duplicated in the shape. The geotomath shape \
                   transform will not work unless it differs from the number of longitudes and it \
                   is one of the last two entiries in the shape'


        # ------  set self data for igridIn and igridOut ------

        grid_typeIn = check_lonlat(lonArrayIn, latArrayIn)                   # 'even' or 'gaussian'
        grid_typeOut = check_lonlat(lonArrayOut, latArrayOut)

        if latArrayIn[0] > latArrayIn[self.latIn -1]:     # get latitude(not colatitude) direction 
            directionIn = 'north_south'
        else:
            directionIn = 'south_north'
        if latArrayOut[0] > latArrayOut[self.latOut -1]:
            directionOut = 'north_south'
        else:
            directionOut = 'south_north'

        self.igridIn = numpy.zeros((2,))
        self.igridOut = numpy.zeros((2,))

        if grid_typeIn == 'even': 
            if directionIn == 'north_south':
                self.igridIn[0] = -1
            else:
                self.igridIn[0] = +1
        elif grid_typeIn == 'gaussian':  
            if directionIn == 'north_south':
                self.igridIn[0] = -2
            else:
                self.igridIn[0] = +2
        else:                                              # gaussian
            msg =  'CANNOT PROCESS THE DATA - Grid maust be even or gaussian'
            raise ValueError, msg
            return

        if grid_typeOut == 'even': 
            if directionOut == 'north_south':
                self.igridOut[0] = -1
            else:
                self.igridOut[0] = +1
        elif grid_typeOut == 'gaussian':  
            if directionOut == 'north_south':
                self.igridOut[0] = -2
            else:
                self.igridOut[0] = +2
        else:                                              # gaussian
            msg = 'CANNOT PROCESS THE DATA - Grid maust be even or gaussian'
            raise ValueError, msg
            return

        self.igridIn[1] = 1                             # for choice nlat x nlon built into .pyf file
        self.igridOut[1] = 1

    def regridScalar(self, sf, missingValue = None):
        """    --------------------------------------------------------------------------------------------------------
         routine:   regridScalar
                                               
         purpose:   transfers scalar data from one global spherical grid to 
                    another. The grids may be gaussian or equally spaced.
         
         usage:     sfregrid= x.regridScalar(sf) 
         
         passed:    sf -- scalar function on a global grid 
         
         returned:  sfregrid -- regridded scalar function 
         
         definition: regridScalar(self, sf, missingValue = None):
    --------------------------------------------------------------------------------------------------------""" 
        # ------------------ Set Parameters --------------------

        igrida = self.igridIn
        nlona = self.lonIn
        nlata = self.latIn
        igridb = self.igridOut
        nlonb = self.lonOut
        nlatb = self.latOut

        standardShapea = self.standardShapeIn 
        standardShapeb = self.standardShapeOut 
        reverseLatitude =  self.reverseLatitude 

        # ** calculate lsave and malloc wsave**
        igrda = abs(igrida[0])
        igrdb = abs(igridb[0])
        na1 = min(nlata, (nlona + 2)/2)
        na2 = (nlata + 1)/2
        nb1 = min(nlatb, (nlonb + 2)/2)
        nb2 = (nlatb + 1)/2

        if igrda == 1:                                                           # even grid
            nwa = 2*nlata*na2 + 3*((na1 - 2)*(2*nlata - na1 - 1))/2 + nlona + 15
        else:                                                                    # gaussian grid
            nwa = nlata*(2*na2 + 3*na1 - 2) + 3*na1*(1 - na1)/2 + nlona + 15
        if igrdb == 1:                                                           # even grid
            nwb = 2*nlatb*nb2 + 3*((nb1 - 2)*(2*nlatb - nb1 - 1))/2 + nlonb + 15
        else:                                                                    # gaussian grid
            nwb = nlatb*(2*nb2 + 3*nb1 - 2) + 3*nb1*(1 - nb1)/2 + nlonb + 15

        lsave = nwa + nwb
        wsave = numpy.zeros((lsave,), numpy.float32)

        # ** calculate lwork **
        nlat = max(nlata, nlatb)
        nlon = max(nlona, nlonb)
        n1 = min(nlat, (nlon + 2)/2)
        n2 = (nlat + 1)/2
        lwork = nlat*(4*n1 + nlon + 2*nlat + 4) + 3*((n1 - 2)*2*(2*nlat - n1 - 1))/2

        # ** calculate ldwork **
        ldwork = nlat*(nlat + 4)

        # ------------------------------------------------------

        # ** transform to math order **
        nt, inverseOrder, sf = geotomath(missingValue, reverseLatitude, standardShapea, sf)

        # ** malloc for array b **
        db = numpy.zeros((nt, nlonb, nlatb), numpy.float32)

        # ---  call trssph one lon-lat slice at a time ----
        intl = 0

        work = numpy.zeros((lwork,),'f')
        dwork = numpy.zeros((ldwork,),'d')
        for i in range(nt): 
            dummy, lsvmin, lwkmin, ierror = spherepack.trssph(intl, igrida,
                                                                  numpy.transpose(sf[i,:,:]),
                                                                  igridb,
                                                                  nlonb, nlatb,
                                                                  wsave, work, dwork)
            db[i,:,:] = numpy.transpose(dummy)
            

            if ierror != 0:
                msg = 'In return from call to trssph ierror =  %d and call number = %d' % (ierror,i)
                raise ValueError, msg

        if ierror != 0 or debug == 1:
            print ' '
            print 'pass to trssph'
            print 'igrida = ', igrida
            print 'nlona = ', nlona
            print 'nlata = ', nlata
            print 'igridb = ', igridb
            print 'nlonb = ', nlonb
            print 'nlatb = ', nlatb
            print 'lsave = ', lsave
            print 'lwork = ', lwork
            print 'ldwork = ', ldwork

            print 'return from trssph with db'
            print 'lsvmin = ', lsvmin
            print 'lsave = ', lsave
            print 'lwkmin = ', lwkmin
            print 'lwork = ', lwork


        # ** transform to geo order **
        db = mathtogeo(reverseLatitude, standardShapeb, inverseOrder, db)

        return db 

    def regridVector(self, u, v, missingValue = None):
        """    --------------------------------------------------------------------------------------------------------
         routine:   regridVector
                                               
         purpose:   transfers vector data from one global spherical grid to another. 
                    The grids can be gaussian or equally spaced.
         
         usage:   uregrid, vregrid = x.regridVector(u, v) 
         
         passed:    u -- zonal vector function on a global grid 
                    v -- meridional vector function on a global grid 
         
         returned:  uregrid -- zonal regridded vector function 
                    vregrid -- meridional regridded vector function 
         
         definition: regridVector(self, u, v, missingValue = None):
    --------------------------------------------------------------------------------------------------------""" 
        # ------------------ Set Parameters --------------------
        iveca = 1
        ivecb = 1

        igrida = self.igridIn
        nlona = self.lonIn
        nlata = self.latIn
        igridb = self.igridOut
        nlonb = self.lonOut
        nlatb = self.latOut

        standardShapea = self.standardShapeIn 
        standardShapeb = self.standardShapeOut 
        reverseLatitude =  self.reverseLatitude 

        # ** calculate lsave and malloc wsave**

        na1 = min(nlata, (nlona + 1)/2)
        na2 = (nlata + 1)/2
        nb1 = min(nlatb, (nlonb + 1)/2)
        nb2 = (nlatb + 1)/2

        nwa = 4*nlata*na2 + 3*max(na1 - 2, 0)*(2*nlata - na1 - 1) + na2 + nlona + 15
        nwb = 4*nlatb*nb2 + 3*max(nb1 - 2, 0)*(2*nlatb - nb1 - 1) + nb2 + nlonb + 15
        lsave = nwa + nwb

        wsave = numpy.zeros((lsave,), numpy.float32)

        # ** calculate lwork **

        nlat = max(nlata, nlatb)
        nlon = max(nlona, nlonb)
        n1 = min(nlat, (nlon + 2)/2)
        lwork = 2*nlat*(8*n1 + 4*nlon + 3)

        # ** calculate ldwork **

        ldwork = 2*nlat*(nlat + 1) + 1

        # ------------------------------------------------------

        # ** transform to math order **
        nt, inverseOrder, u, v = geotomath(missingValue, reverseLatitude, standardShapea, u, v)

        # ** malloc for array b **
        ub = numpy.zeros((nt, nlonb, nlatb), numpy.float32)
        vb = numpy.zeros((nt, nlonb, nlatb), numpy.float32)

        # ---  call trvsph one lon-lat slice at a time ----
        intl = 0
        work = numpy.zeros((lwork,),'f')
        dwork = numpy.zeros((ldwork,),'d')
        for i in range(nt): 
            dummy1, dummy2, lsvmin, lwkmin, ierror = spherepack.trvsph(intl, igrida,
                                                                       iveca,
                                                                       numpy.transpose(u[i,:,:]),
                                                                       numpy.transpose(v[i,:,:]),
                                                                       igridb,
                                                                       nlonb, nlatb,
                                                                       ivecb,
                                                                       wsave, work, dwork)
            ub[i,:,:] = numpy.transpose(dummy1)
            vb[i,:,:] = numpy.transpose(dummy2)

            if ierror != 0:
                msg = 'In return from call to trvsph ierror =  %d and call number = %d' % (ierror,i)
                raise ValueError, msg

        if ierror != 0 or debug == 1:
            print ' '
            print 'pass to trvsph'
            print 'igrida = ', igrida
            print 'iveca = ', iveca
            print 'nlona = ', nlona
            print 'nlata = ', nlata
            print 'igridb = ', igridb
            print 'ivecb = ', ivecb
            print 'nlonb = ', nlonb
            print 'nlatb = ', nlatb
            print 'lsave = ', lsave
            print 'lwork = ', lwork
            print 'ldwork = ', ldwork

            print 'return from trvsph with ub, vb, lsvmin, lwkmin'
            print 'lsvmin = ', lsvmin
            print 'lsave = ', lsave
            print 'lwkmin = ', lwkmin
            print 'lwork = ', lwork

        # ** transform to geo order **
        ub, vb = mathtogeo(reverseLatitude, standardShapeb, inverseOrder, ub, vb)

        return ub, vb 

# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# +++++++++++++++++++++++++++++ Shiftgrid class +++++++++++++++++++++++++++++++++++++++++
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

class Shiftgrid:
    #-----------------------------------------------------------------------------------------
    #            
    #       Shifting -- contained in Shiftgrid class
    #    
    #           The two functions for shifting an evenly spaced grid by half an increment in longitude
    #           and latitude are:
    #            
    #            shiftScalar  --  transfers scalar data between an equally spaced regular and an offset grid       
    #            shfitVector  --  transfers vector data between an equally spaced regular and an offset grid       
    #            
    #-----------------------------------------------------------------------------------------

    def __init__(self, lonArray, latArray, numberLevels = None, numberTimes = None): 
        """    --------------------------------------------------------------------------------------------------------
         
         purpose:  'init' for class Shiftgrid assigns values to the instance data which are the dimensions
                    lengths, the latitude direction and the grid type as regular (evenly spaced including
                    the poles) or offset from regular by a half grid point in two directions.
          
         usage:     x = sphere.Shiftgrid(lonArray = lonvals, latArray = latvals, nlev, ntime) 
       
                    where nlev and ntime are the actual number of levels and times respectively.
         
         passed:    lonArray = longitude vector
                    latArray = latitude vector
                    numberLevels = number of levels (optional)
                    numberTimes = number of times (optional)
         
         
         caution:   Grid must be evenly spaced. If it includes the poles in is shifted
                    a half increment from the poles. If it excludes the poles in is shifted
                    a half increment to include the poles. There are only two possible input
                    longitude_latitude grids.
         
         definition: __init__(self, lonArray, latArray, numberLevels = None, numberTimes = None): 
    --------------------------------------------------------------------------------------------------------""" 
        #    caution:   In this class the geo order (nlat,nlon) is used

        self.grid_type = check_shiftgrids(lonArray, latArray)      # get grid type as 'regular' or 'offset'

        self.lon = len(lonArray)

        if self.grid_type == 'regular':                            # self.lat is the size of the offset latitudes
            self.lat = len(latArray) - 1
        else:
            self.lat = len(latArray)

        if latArray[0] > latArray[len(latArray)-1]:                # the shift routines want latitude south to north
            self.reverseLatitude = 'geoyes'
        else:
            self.reverseLatitude = 'no'


        dimlist = [self.lon, self.lat]                 # note the start of geo order here -- note list reverse later
        dimlistp = [self.lon, self.lat + 1]            # note the start of geo order here -- note list reverse later

        self.lev = numberLevels
        if numberLevels is not None and  numberLevels != 0:
            dimlist.append(self.lev)
            dimlistp.append(self.lev)

        self.tme = numberTimes
        if numberTimes is not None and  numberTimes != 0:
            dimlist.append(self.tme)
            dimlistp.append(self.tme)
      
        dimlist.reverse()
        dimlistp.reverse()
        self.standardShapeGeo = tuple(dimlist)                  # geo order (ntme,nlev,nlat,nlon)
        self.standardShapeGeop = tuple(dimlistp)                  # geo order (ntme,nlev,nlat+1,nlon)

        # check the shape for a unique number of longitudes and a unique number of latitudes

        if self.lon in [self.tme, self.lev, self.lat]:
            print 'Warning - number of longitudes in duplicated in the shape. The geotomath shape \
                   transform will not work unless it differs from the number of latitudes and it \
                   is one of the last two entiries in the shape'
        if self.lat in [self.tme, self.lev, self.lon]:
            print 'Warning - number of latitudes in duplicated in the shape. The geotomath shape \
                   transform will not work unless it differs from the number of longitudes and it \
                   is one of the last two entiries in the shape'

    def shiftScalar(self, sf, missingValue = None):
        """    --------------------------------------------------------------------------------------------------------
         routine:   shiftScalar
         
         purpose:   transfers scalar data on the sphere between an equally spaced
                    grid that includes the poles and a grid which is offset by a 
                    half grid increment in both longitude and latitude (which 
                    excludes the poles) 
         
        usage:     sfshift = x.shiftScalar(sf)
         
        passed:    sf -- an evenly spaced scalar function on a global grid 
         
        returned:  sfshift --  the shifted evenly spaced scalar function 
         
       definition: shiftScalar(self, sf, missingValue = None):
    --------------------------------------------------------------------------------------------------------""" 

        # ---- Set parameters and sizes ----
        nlon = self.lon 
        nlat = self.lat 
        standardShapeGeo = self.standardShapeGeo 
        standardShapeGeop = self.standardShapeGeop 
        reverseLatitude =  self.reverseLatitude 

        if self.grid_type == 'regular':                            # sf passed is a regular grid
            ioff = 1
        else:
            ioff = 0

        lsav = 2*(2*nlat + nlon + 16)

        if nlon%2:                              # nlon is odd
            lwrk = nlon*(5*nlat + 1)
        else:
            lwrk = 2*nlon*(nlat + 1)


        # ---  call sshifti ----
        wsav, ierror = spherepack.sshifti(ioff, nlon, nlat, lsav)
        if ierror != 0:
            msg = 'In return from call to sshifti ierror =  %d' % (ierror,)
            raise ValueError, msg

        # ** transform to standard geo order **
        if ioff == 1:                                                            # regular grid passed in
            nt, inverseOrder, sf = geotomath(missingValue, reverseLatitude, standardShapeGeop, sf)
        else:                                                                    # offset grid passed in
            nt, inverseOrder, sf = geotomath(missingValue, reverseLatitude, standardShapeGeo, sf)


        if nt > 1:                                                                # call sshifte one slice at a time 

            if ioff == 1:                                                         # regular grid passed in

                goff_return = numpy.zeros((nt, nlat, nlon), numpy.float32)     # malloc for array goff_return

                for i in range(nt): 
                    greg = sf[i,:,:]           

                    # ---  call sshifte ----
                    goff = numpy.zeros((nlat, nlon), numpy.float32)            # malloc for inout array goff
                    wrk = numpy.zeros((lwrk,),'f')
                    goff = numpy.transpose(goff)
                    greg = numpy.transpose(greg)
                    ierror = spherepack.sshifte(ioff,
                                                goff,
                                                greg,
                                                wsav, wrk)
                    goff = numpy.transpose(goff)
                    greg = numpy.transpose(greg)
                    

                    if ierror != 0 or debug == 1:
                        print ' '
                        print 'pass to sshifte'
                        print 'nlon = ', nlon
                        print 'nlat = ', nlat
                        print 'lsav = ', lsav
                        print 'lwrk = ', lwrk
                        print 'return from sshifte'

                    if ierror != 0:
                        msg = 'In return from call to sshifte ierror =  %d' % (ierror,)
                        raise ValueError, msg

                    goff_return[i,:,:] = goff

                # ** transform to original geo order **
                goff_return = mathtogeo(reverseLatitude, standardShapeGeo, inverseOrder, goff_return)

                return goff_return 

            else:                                                                     # offset grid passed in

                greg_return = numpy.zeros((nt, nlat + 1, nlon), numpy.float32)     # malloc for array greg_return **

                for i in range(nt): 
                    goff = sf[i,:,:]           

                    # ---  call sshifte ----
                    greg = numpy.zeros((nlat + 1, nlon), numpy.float32)            # malloc for inout array greg
                    wrk = numpy.zeros((lwrk,),'f')
                    goff = numpy.transpose(goff)
                    greg = numpy.transpose(greg)
                    ierror = spherepack.sshifte(ioff,
                                                goff,
                                                greg,
                                                wsav, wrk)
                    goff = numpy.transpose(goff)
                    greg = numpy.transpose(greg)
                    if ierror != 0 or debug == 1:
                        print ' '
                        print 'pass to sshifte'
                        print 'nlon = ', nlon
                        print 'nlat = ', nlat
                        print 'lsav = ', lsav
                        print 'lwrk = ', lwrk
                        print 'return from sshifte'

                    if ierror != 0:
                        msg = 'In return from call to sshifte ierror =  %d' % (ierror,)
                        raise ValueError, msg

                    greg_return[i,:,:] = greg

                # ** transform to original geo order **
                greg_return = mathtogeo(reverseLatitude, standardShapeGeop, inverseOrder, greg_return)

                return greg_return 


        else:                                                                     # single section only

            sf = numpy.reshape(sf, sf.shape[1:])                                # remove dummy dimension

            if ioff == 1:                                                         # regular grid passed in
                greg = sf

                # ---  call sshifte ----
                goff = numpy.zeros((nlat, nlon), numpy.float32)            # malloc for inout array goff
                wrk = numpy.zeros((lwrk,),'f')
                goff = numpy.transpose(goff)
                greg = numpy.transpose(greg)
                ierror = spherepack.sshifte(ioff,
                                            goff,
                                            greg,
                                            wsav, wrk)
                goff = numpy.transpose(goff)
                greg = numpy.transpose(greg)
                
                if ierror != 0 or debug == 1:
                    print ' '
                    print 'pass to sshifte'
                    print 'nlon = ', nlon
                    print 'nlat = ', nlat
                    print 'lsav = ', lsav
                    print 'lwrk = ', lwrk
                    print 'return from sshifte'

                if ierror != 0:
                    msg = 'In return from call to sshifte ierror =  %d' % (ierror,)
                    raise ValueError, msg

                goff = numpy.reshape(goff, (1, goff.shape[0], goff.shape[1]))      # restore dummy dimension

                # ** transform to original geo order **
                goff = mathtogeo(reverseLatitude, standardShapeGeo, inverseOrder, goff)

                return goff 


            else:                                                                 # offset grid passed in
                goff = sf

                # ---  call sshifte ----
                greg = numpy.zeros((nlat + 1, nlon), numpy.float32)            # malloc for inout array goff
                wrk = numpy.zeros((lwrk,),'f')
                goff = numpy.transpose(goff)
                greg = numpy.transpose(greg)
                ierror = spherepack.sshifte(ioff,
                                            goff,
                                            greg,
                                            wsav, wrk)
                goff = numpy.transpose(goff)
                greg = numpy.transpose(greg)

                if ierror != 0 or debug == 1:
                    print ' '
                    print 'pass to sshifte'
                    print 'nlon = ', nlon
                    print 'nlat = ', nlat
                    print 'lsav = ', lsav
                    print 'lwrk = ', lwrk
                    print 'return from sshifte'

                if ierror != 0:
                    msg = 'In return from call to sshifte ierror =  %d' % (ierror,)
                    raise ValueError, msg

                greg = numpy.reshape(greg, (1, greg.shape[0], greg.shape[1]))      # restore dummy dimension

                # ** transform to original geo order **
                greg = mathtogeo(reverseLatitude, standardShapeGeop, inverseOrder, greg)

                return greg 

    def shiftVector(self, u, v, missingValue = None):
        """    --------------------------------------------------------------------------------------------------------
         routine:   shiftVector
         
         purpose:   transfers vector data on the sphere between an equally spaced
                    grid that includes the poles and a grid which is offset by a 
                    half grid increment in both longitude and latitude (which 
                    excludes the poles) 
         
         usage:     ushift, vshift = x.shiftVector(u,v)
         
         passed:    u -- zonal evenly spaced vector function on a global grid 
                    v -- meridional evenly spaced vector function on a global grid 
         
         returned:  ushift -- zonal evenly spaced vector function 
                    vshift -- meridional evenly spaced vector function 
         
         definition: shiftVector(self, u, v, missingValue = None):
    --------------------------------------------------------------------------------------------------------""" 

        # ---- Set parameters and sizes ----
        nlon = self.lon 
        nlat = self.lat 
        standardShapeGeo = self.standardShapeGeo 
        standardShapeGeop = self.standardShapeGeop 
        reverseLatitude =  self.reverseLatitude 

        if self.grid_type == 'regular':                            # sf passed is a regular grid
            ioff = 1
        else:
            ioff = 0

        lsav = 2*(2*nlat + nlon + 16)

        if nlon%2:                              # nlon is odd
            lwrk = nlon*(5*nlat + 1)
        else:
            lwrk = 2*nlon*(nlat + 1)


        # ---  call vshifti ----
        wsav, ierror = spherepack.vshifti(ioff, nlon, nlat, lsav)
        if ierror != 0:
            msg = 'In return from call to vshifti ierror =  %d' % (ierror,)
            raise ValueError, msg

        # ** transform to standard geo order **
        if ioff == 1:                                                            # regular grid passed in
            nt, inverseOrder, u, v = geotomath(missingValue, reverseLatitude, standardShapeGeop, u, v)
            v = -1.0*v                             # undo the colatitude conversion in geotomath
            v = numpy.array(v.astype(numpy.float32), numpy.float32)
        else:                                                                    # offset grid passed in
            nt, inverseOrder, u, v = geotomath(missingValue, reverseLatitude, standardShapeGeo, u, v)
            v = -1.0*v                             # undo the colatitude conversion in geotomath
            v = numpy.array(v.astype(numpy.float32), numpy.float32)

        if nt > 1:                                                                # call vshifte one slice at a time 

            if ioff == 1:                                                         # regular grid passed in

                uoff_return = numpy.zeros((nt, nlat, nlon), numpy.float32)     # malloc for array uoff_return **
                voff_return = numpy.zeros((nt, nlat, nlon), numpy.float32)     # malloc for array voff_return **

                for i in range(nt): 
                    ureg = u[i,:,:]           
                    vreg = v[i,:,:]           

                    # ---  call vshift2e ----
                    uoff = numpy.zeros((nlat, nlon), numpy.float32)            # malloc for inout array uoff
                    voff = numpy.zeros((nlat, nlon), numpy.float32)
                    wrk = numpy.zeros((lwrk,),'f')
                    uoff = numpy.transpose(uoff)
                    voff = numpy.transpose(voff)
                    ureg = numpy.transpose(ureg)
                    vreg = numpy.transpose(vreg)
                    ierror = spherepack.vshifte(ioff,
                                                uoff,
                                                voff,
                                                ureg,
                                                vreg,
                                                wsav, wrk)
                    uoff = numpy.transpose(uoff)
                    voff = numpy.transpose(voff)
                    ureg = numpy.transpose(ureg)
                    vreg = numpy.transpose(vreg)


                    if ierror != 0 or debug == 1:
                        print ' '
                        print 'pass to sshifte'
                        print 'nlon = ', nlon
                        print 'nlat = ', nlat
                        print 'lsav = ', lsav
                        print 'lwrk = ', lwrk
                        print 'return from vshifte'

                    if ierror != 0:
                        msg = 'In return from call to vshifte ierror =  %d' % (ierror,)
                        raise ValueError, msg

                    uoff_return[i,:,:] = uoff
                    voff_return[i,:,:] = voff

                # ** transform to original geo order **
                uoff_return = mathtogeo(reverseLatitude, standardShapeGeo, inverseOrder, uoff_return)
                voff_return = mathtogeo(reverseLatitude, standardShapeGeo, inverseOrder, voff_return)

                return uoff_return, voff_return

            else:                                                                     # offset grid passed in

                ureg_return = numpy.zeros((nt, nlat + 1, nlon), numpy.float32)     # malloc for array ureg_return **
                vreg_return = numpy.zeros((nt, nlat + 1, nlon), numpy.float32)     # malloc for array vreg_return **

                for i in range(nt): 
                    uoff = u[i,:,:]           
                    voff = v[i,:,:]           

                    # ---  call vshifte ----
                    ureg = numpy.zeros((nlat + 1, nlon), numpy.float32)            # malloc for inout array uoff
                    vreg = numpy.zeros((nlat + 1, nlon), numpy.float32)  
                    wrk = numpy.zeros((lwrk,),'f')
                    uoff = numpy.transpose(uoff)
                    voff = numpy.transpose(voff)
                    ureg = numpy.transpose(ureg)
                    vreg = numpy.transpose(vreg)
                    ierror = spherepack.vshifte(ioff,
                                                uoff,
                                                voff,
                                                ureg,
                                                vreg,
                                                wsav, wrk)
                    uoff = numpy.transpose(uoff)
                    voff = numpy.transpose(voff)
                    ureg = numpy.transpose(ureg)
                    vreg = numpy.transpose(vreg)

                    if ierror != 0 or debug == 1:
                        print ' '
                        print 'pass to vshifte'
                        print 'nlon = ', nlon
                        print 'nlat = ', nlat
                        print 'lsav = ', lsav
                        print 'lwrk = ', lwrk
                        print 'return from sshifte'

                    if ierror != 0:
                        msg = 'In return from call to vshifte ierror =  %d' % (ierror,)
                        raise ValueError, msg

                    ureg_return[i,:,:] = ureg
                    vreg_return[i,:,:] = vreg

                # ** transform to original geo order **
                ureg_return, vreg_return = mathtogeo(reverseLatitude, standardShapeGeop, inverseOrder, ureg_return, vreg_return)
                vreg_return = -1.0*vreg_return
                vreg_return = numpy.array(vreg_return.astype(numpy.float32), numpy.float32)

                return ureg_return, vreg_return


        else:                                                                     # single section only

            u = numpy.reshape(u, u.shape[1:])                                   # remove dummy dimension
            v = numpy.reshape(v, v.shape[1:])                                   # remove dummy dimension

            if ioff == 1:                                                         # regular grid passed in
                ureg = u
                vreg = v

                # ---  call vshifte ----
                uoff = numpy.zeros((nlat, nlon), numpy.float32)            # malloc for inout array uoff
                voff = numpy.zeros((nlat, nlon), numpy.float32)            # malloc for inout array uoff
                wrk = numpy.zeros((lwrk,),'f')
                uoff = numpy.asfortranarray(numpy.transpose(uoff))
                voff = numpy.asfortranarray(numpy.transpose(voff))
                ureg = numpy.asfortranarray(numpy.transpose(ureg))
                vreg = numpy.asfortranarray(numpy.transpose(vreg))
                ierror = spherepack.vshifte(ioff,
                                            uoff,
                                            voff,
                                            ureg,
                                            vreg,
                                            wsav, wrk)
                uoff = numpy.transpose(uoff)
                voff = numpy.transpose(voff)
                ureg = numpy.transpose(ureg)
                vreg = numpy.transpose(vreg)

                if ierror != 0 or debug == 1:
                    print ' '
                    print 'pass to vshifte'
                    print 'nlon = ', nlon
                    print 'nlat = ', nlat
                    print 'lsav = ', lsav
                    print 'lwrk = ', lwrk
                    print 'return from vshifte'

                if ierror != 0:
                    msg = 'In return from call to vshifte ierror =  %d' % (ierror,)
                    raise ValueError, msg

                uoff = numpy.reshape(uoff, (1, uoff.shape[0], uoff.shape[1]))      # restore dummy dimension
                voff = numpy.reshape(voff, (1, voff.shape[0], voff.shape[1]))      # restore dummy dimension

                # ** transform to original geo order **
                uoff, voff = mathtogeo(reverseLatitude, standardShapeGeo, inverseOrder, uoff, voff)
                voff = -1.0*voff
                voff = numpy.array(voff.astype(numpy.float32), numpy.float32)

                return uoff, voff 


            else:                                                                 # offset grid passed in
                uoff = u
                voff = v

                # ---  call vshifte ----
                ureg = numpy.zeros((nlat + 1, nlon), numpy.float32)            # malloc for inout array uoff
                vreg = numpy.zeros((nlat + 1, nlon), numpy.float32)  
                wrk = numpy.zeros((lwrk,),'f')
                uoff = numpy.transpose(uoff)
                voff = numpy.transpose(voff)
                ureg = numpy.transpose(ureg)
                vreg = numpy.transpose(vreg)
                ierror = spherepack.vshifte(ioff,
                                            uoff,
                                            voff,
                                            ureg,
                                            vreg,
                                            wsav, wrk)
                uoff = numpy.transpose(uoff)
                voff = numpy.transpose(voff)
                ureg = numpy.transpose(ureg)
                vreg = numpy.transpose(vreg)

                if ierror != 0 or debug == 1:
                    print ' '
                    print 'pass to vshifte'
                    print 'nlon = ', nlon
                    print 'nlat = ', nlat
                    print 'lsav = ', lsav
                    print 'lwrk = ', lwrk
                    print 'return from vshifte'

                if ierror != 0:
                    msg = 'In return from call to sshifte ierror =  %d' % (ierror,)
                    raise ValueError, msg

                ureg = numpy.reshape(ureg, (1, ureg.shape[0], ureg.shape[1]))      # restore dummy dimension
                vreg = numpy.reshape(vreg, (1, vreg.shape[0], vreg.shape[1]))      # restore dummy dimension

                # ** transform to original geo order **
                ureg, vreg = mathtogeo(reverseLatitude, standardShapeGeop, inverseOrder, ureg, vreg)
                vreg = -1.0*vreg
                vreg = numpy.array(vreg.astype(numpy.float32), numpy.float32)

                return ureg, vreg 



# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# ++++++++++++++++++++++++++++++++ Utility Functions +++++++++++++++++++++++++++++++++
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++


def gaussian_pts_wts_bnds(nlat):
    #-------------------------------------------------------------------------------
    #                                      
    #     routine: gaussian_pts_wts_bnds
    #                                      
    #     purpose: compute the double precision gaussian grid points, weights and
    #              bounds using the spherepack function gaqd 
    #
    #     usage:   points, weights, bounds = gaussian_pts_wts_bnds(nlat) 
    #
    #              where nlat is the number of latitudes
    #
    #-------------------------------------------------------------------------------

    rad2deg = 180.0/math.pi

    # get the gaussian points and weights from spherepack

    ldwork = nlat*(nlat + 2)
    work = numpy.zeros((ldwork,),'d')
    points, wts, ierror = spherepack.gaqd(nlat, work)

    if ierror != 0:
        msg = 'In return from call to gaqd ierror =  %d' % (ierror,)
        raise ValueError, msg

    # convert points to geophysical format

    colatlist = list(points) 
    latlist = map( (lambda x: 90.0 - x*180.0/math.pi), colatlist)


    # calculate the bounds

    sinb = [0.0]*(nlat + 1)                                                  # allocate memory
    bndslist = [0.0]*(nlat + 1) 

    sinb[0] = 1.0
    sinb[nlat] = -1.0

    for i in range(1,nlat):
        sinb[i] = sinb[i-1] - wts[i-1]

    for i in range(nlat + 1):
        bndslist[i] = rad2deg*math.asin(sinb[i])
        
    # convert lists to double precision arrays

    pts =  numpy.array(latlist, numpy.float64)
    bnds =  numpy.array(bndslist, numpy.float64)

    return pts, wts, bnds

def check_lonlat(checklonpass, checklatpass):
    #-------------------------------------------------------------------------------
    #                                      
    #     routine: check_lonlat
    #                                      
    #     purpose: compare the passed checklat and checklon array with the correct
    #              geophysical ones calculated here. The latitudes must cover the 
    #              full sphere for either a evenly spaced (including the poles) or
    #              a gaussian grid. The longitudes must cover full sphere without a wrap. 
    #
    #     usage:   laterror, lonerror = check_lonlat(checklon, checklat)
    #              where checklat and checklon is a grid to check 
    #
    #    return:   grid_type as 'even' or 'gaussian' 
    #
    #-------------------------------------------------------------------------------
    small = 0.001                     # use as tolerance in checking values

    nlat = len(checklatpass) 
    nlon = len(checklonpass) 

    checklon = checklonpass

    if checklatpass[0] < checklatpass[nlat-1]:                         # need a copy?
        checklat = numpy.array(checklatpass, numpy.float64)
        checklat = checklat[::-1]
    else:
        checklat = checklatpass

    # check the pass for evenly spaced latitude including the poles


    firstdelta = abs(checklat[0] - checklat[1])
    maxdiff = 0.0
    for i in range(1, nlat - 1):
        diff = abs(firstdelta - (checklat[i] - checklat[i+1]))
        if diff > maxdiff:
            maxdiff = diff

    if maxdiff < small:
        if abs(90. - checklat[0]) > small or abs(-90. - checklat[nlat-1]) > small:
            print '***************************************************************************'
            print 'CANNOT PROCESS THE DATA - Evenly spaced grids must include the pole points'
            print '***************************************************************************'
            raise ValueError
            return
        else:
            grid_type = 'even'
    else:
        grid_type = 'gaussian'

    # check the pass for evenly spaced longitudes without a wrap

    if checklon[0] > checklon[nlon-1]: 
        print '****************************************************************'
        print 'CANNOT PROCESS THE DATA - Longitudes must run from west to east'
        print '****************************************************************'
        raise ValueError
        return

    delta = 360./nlon
    if (checklon[nlon-1] - checklon[0]) > (360.0 -(delta - small)) : 
        print '**************************************************'
        print 'CANNOT PROCESS THE DATA - Longitudes can not wrap'
        print '**************************************************'
        raise ValueError
        return


    # generate the correct latitude geophysical grid points

    if grid_type == 'even':

        latlist = []
        delta = 180./(nlat - 1)

        for i in range(nlat):
            value = 90. - i*delta
            latlist.append(value)

        latvals =  numpy.array(latlist, numpy.float64)

    else:

        latvals, wts, bnds = gaussian_pts_wts_bnds(nlat)


    # check lon and lat values
   
    delta = 360./nlon

    maxdiff = 0.0                                                   # max difference between delta and actual increments
    for i in range(0, nlon - 1):
        diff = abs(delta - (checklon[i+1] - checklon[i]))
        if diff > maxdiff:
            maxdiff = diff

    if maxdiff > small:
        print '***********************************************************'
        print 'CANNOT PROCESS THE DATA - Longitudes are not evenly spaced'
        print '***********************************************************'
        raise ValueError
        return

    laterror = max( abs(latvals - checklat) )
    if laterror > small:
        print '********************************************************'
        print 'CANNOT PROCESS THE DATA - Latitude values are incorrect'
        print '********************************************************'
        raise ValueError

    return  grid_type 


def check_shiftgrids(checklon, checklat):
    #-------------------------------------------------------------------------------
    #                                      
    #     routine: check_shiftgrids
    #                                      
    #     purpose: check the passed checklat and checklon arrays for conformity
    #              
    #              For 'reg' grids:
    #                             latitudes must be evenly spaced and include the poles 
    #                             longitudes must start at 0 without a wrap. 
    #              For 'off' grids:
    #                             latitudes must be evenly spaced and exclude the poles 
    #                             longitudes must start at 0 + delta/2  without a wrap. 
    #
    #     usage:   grid_type = check_shiftgrids(checklat, checklon)
    #              where checklat and checklon is a grid to check 
    #
    #    return:   grid_type as 'regular' or 'offset' 
    #
    #-------------------------------------------------------------------------------
    small = 0.001                     # use as tolerance in checking values

    nlon = len(checklon)
    nlat = len(checklat)

    # check the pass for evenly spaced latitudes and determine the grid type as 'regular' or 'offset'

    if checklat[0] < checklat[nlat-1]: 
        checklat = checklat[::-1]

    firstdelta = abs(checklat[0] - checklat[1])
    maxdiff = 0.0
    for i in range(1, nlat - 1):
        diff = abs(firstdelta - (checklat[i] - checklat[i+1]))
        if diff > maxdiff:
            maxdiff = diff

    if maxdiff < small:
        if abs(90. - checklat[0]) < small and abs(-90. - checklat[nlat-1]) < small:            # are poles present?
            grid_type = 'regular'
        else:
            grid_type = 'offset'
    else:
        print '*********************************************************'
        print 'CANNOT PROCESS THE DATA - Latitudes are not evenly spaced'
        print '*********************************************************'
        raise ValueError
        return
 
    # check the pass for evenly spaced longitudes and conformity to  the grid type as 'regular' or 'offset'

    if checklon[0] > checklon[nlon-1]: 
        print '***************************************************************'
        print 'CANNOT PROCESS THE DATA - Longitudes must run from west to east'
        print '***************************************************************'
        raise ValueError
        return

    delta = 360./nlon
    if (checklon[nlon-1] - checklon[0]) > (360.0 -(delta - small)) : 
        print '*************************************************'
        print 'CANNOT PROCESS THE DATA - Longitudes can not wrap'
        print '*************************************************'
        raise ValueError
        return

    maxdiff = 0.0                                                   # max difference between delta and actual increments
    for i in range(0, nlon - 1):
        diff = abs(delta - (checklon[i+1] - checklon[i]))
        if diff > maxdiff:
            maxdiff = diff

    if maxdiff > small:
        print '**********************************************************'
        print 'CANNOT PROCESS THE DATA - Longitudes are not evenly spaced'
        print '**********************************************************'
        raise ValueError
        return
    else:
        if grid_type == 'regular':
            if abs(0. - checklon[0]) > small or abs(360. - delta - checklon[nlon-1]) > small: 
                print '********************************************************************'
                print 'WARNING - Longitude end points do not conform to regular grid values'
                print 'Expected longitude to start at 0 degrees'
                print '********************************************************************'
        else:
            if abs(0. + delta/2. - checklon[0]) > small or abs(360. - delta/2. - checklon[nlon-1]) > small: 
                print '*********************************************************************'
                print 'WARNING -  Longitude end points do not conform to offset grid values'
                print 'Expected longitude to start offset from 0 degrees by half grid spacing'
                print '**********************************************************************'

    return  grid_type 

def geoscale(scale, u, v = None):
    #-------------------------------------------------------------------------------
    #                                      
    #     routine: geoscale
    #
    #    purpose: scale geophysical data 
    # 
    #    passed : scale - the the multiplier
    #             u,v - vector functions to scale
    #             or 
    #             u and None - u is the scalar function to scale 
    #             
    #    returned:  u and v 
    #             or 
    #               u - the scalar function 
    #
    #-------------------------------------------------------------------------------

    if v is None:                                                     # scalar function case
        u = scale*u  
        u = numpy.array(u.astype(numpy.float32), numpy.float32)
        return u
    else:                                                             # vectorscalar function case
        u = scale*u  
        u = numpy.array(u.astype(numpy.float32), numpy.float32)
        v = scale*v  
        v = numpy.array(v.astype(numpy.float32), numpy.float32)
        return u, v

def geotomath(missingValue, reverseLatitude, standardShape, u, v = None):
    #-------------------------------------------------------------------------------
    #                                      
    #     routine: geotomath
    #
    #    purpose: transform geophysical data to spherepack math format 
    # 
    #    passed : standardShape - the standard math order (ntme, nlev, nlon, nlat)
    #             u,v - vector functions to transform to standard math shape 
    #             or 
    #             u and None - u is the scalar function to transform to standard math shape 
    #             
    #    returned: inverseOrder, u and v (inverseOrder is needed in mathtogeo)
    #             or 
    #              inverseOrder, u - the scalar function 
    #             
    #    caution: a correct result is certain only if the shape makes a unique list
    #
    #-------------------------------------------------------------------------------
    # ----- Check data type and change to float if necessary -------

    if u.dtype.char != 'f':
        print '*******************************************'
        print 'WARNING - data will be converted to Float32'
        print '*******************************************'
        u = u.astype(numpy.float32)
   
    if v is not None:
        if v.dtype.char != 'f':
            print '*******************************************'
            print 'WARNING - data will be converted to Float32'
            print '*******************************************'
            v = v.astype(numpy.float32)

    # ----- Check for missing data -------

    if missingValue is not None and  usefilled == 'yes':
        um = numpy.ma.masked_where(u, missingValue)
        if um.mask is not numpy.ma.nomask:
            print '************************************************'
            print 'CANNOT PROCESS THE DATA - field has missing data'
            print '************************************************'
            raise ValueError
            return
        if v is not None:
            vm = numpy.ma.masked_where(v, missingValue)
            if vm.mask is not numpy.ma.nomask:
                print '************************************************'
                print 'CANNOT PROCESS THE DATA - field has missing data'
                print '************************************************'
                raise ValueError
                return

    # ----- Perform preliminary checks -------

    if v is not None:
        if u.shape != v.shape:
            print '***************************************************************************'
            print 'CANNOT PROCESS THE DATA - Error in the data - u and v have different shapes'
            print '***************************************************************************'
            raise 'IndexError'
            return

    origShape = u.shape                                  # u is the scalar function
    
    if len(standardShape) != len(origShape):
        print '***********************************'
        print 'CANNOT PROCESS THE DATA'
        print 'Shapes are not the same length'
        print 'standardShape is : ', standardShape
        print 'origShape is : ', origShape
        print '***********************************'
        raise IndexError
        return

    # ----- Determine the new order -----  

    size = len(standardShape)
    newOrderlist = [None]*size                             # malloc

    for i in range(size):                                  # make tuple to transpose original data to standard order
        test = standardShape[i]
        for j in range(size):
            if test == origShape[j]:
                if j not in newOrderlist:                  # all numbers in newOrderlist must be different
                    newOrderlist[i] = j
                    break                                  # use first found if there are duplicates

    newOrder = tuple(newOrderlist)

    # -----  Determine the inverse to this new order for use in mathtogeo -----  

    xform = []
    for i in range(len(newOrder)):
        xform.append( [newOrder[i], i] )
    xform.sort()

    inverse_shapelist = []
    for item in xform:
        inverse_shapelist.append(item[1])
    inverseOrder = tuple(inverse_shapelist)


    # -----  Determine nt for the triple (nt, nlon, nlat) -----  

    if size == 4:
       nt = standardShape[0]*standardShape[1]
    elif size == 3:
        nt = standardShape[0]
    elif size == 2:
        nt = 1
    else:
        print '**************************************************************'
        print 'CANNOT PROCESS THE DATA - size of data array must be 2, 3 or 4'
        print '**************************************************************'
        raise IndexError
        return

    triple = (nt, standardShape[size - 2], standardShape[size - 1])
 
    u = numpy.transpose(u, newOrder)                                # transpose data to standard math
    u = numpy.array(u.astype(numpy.float32), numpy.float32)                             # make contiguous 
    u = numpy.reshape(u, triple)                                    # reshape to form for spherepack

    if reverseLatitude != 'no':
        if reverseLatitude == 'mathyes':
            u = u[:,:,::-1]
        elif reverseLatitude == 'geoyes':
            u = u[:,::-1,:]
        else:
            print 'Only choices for reverseLatitude are strings no, mathyes or geoyes'
            raise ValueError

    if v is None:                                                     # scalar function case
        return nt, inverseOrder, u
    else:
        v = numpy.transpose(v, newOrder)
        v = -1.0*v
        v = numpy.array(v.astype(numpy.float32), numpy.float32)       # make contiguous 
        v = numpy.reshape(v, triple)

        if reverseLatitude != 'no':
            if reverseLatitude == 'mathyes':
                v = v[:,:,::-1]
            elif reverseLatitude == 'geoyes':
                v = v[:,::-1,:]
            else:
                print 'Only choices for reverseLatitude are strings no, mathyes or geoyes'
                raise ValueError

 
        return nt, inverseOrder, u, v

def mathtogeo(reverseLatitude, standardShape, inverseOrder, u, v = None):
    #-------------------------------------------------------------------------------
    #                                      
    #    routine: mathtogeo
    #
    #    purpose: transform spherepack math format to geophysical order
    # 
    #    passed : u,v which must be math order or u alone as a scalar function
    #             
    #    returned: u, v or only u in original data  order
    #
    #-------------------------------------------------------------------------------

    if v is not None:                                                            # vector case
        # Restore the standard time and level shape in the vector data

        if reverseLatitude != 'no':
            if reverseLatitude == 'mathyes':
                u = u[:,:,::-1]
                v = v[:,:,::-1]
            elif reverseLatitude == 'geoyes':
                u = u[:,::-1,:]
                v = v[:,::-1,:]
            else:
                print 'Only choices for reverseLatitude are strings no, mathyes or geoyes'
                raise ValueError

        u = numpy.reshape(u, standardShape)
        u = numpy.array(u.astype(numpy.float32), numpy.float32)       # make contiguous 
        v = numpy.reshape(v, standardShape)
        v = numpy.array(v.astype(numpy.float32), numpy.float32)       # make contiguous 
        
        # Restore the shape of the data to conform to that of the original data 

        u = numpy.transpose(u, inverseOrder)
        u = numpy.array(u.astype(numpy.float32), numpy.float32)       # make contiguous 

        v = numpy.transpose(v, inverseOrder)
        v = -1.0*v
        v = numpy.array(v.astype(numpy.float32), numpy.float32)  

        return  u, v  

    else:                                                                   # scalar case
        # Restore the standard time and level shape in the scalar data

        if reverseLatitude != 'no':
            if reverseLatitude == 'mathyes':
                u = u[:,:,::-1]
            elif reverseLatitude == 'geoyes':
                u = u[:,::-1,:]
            else:
                print 'Only choices for reverseLatitude are strings no, mathyes or geoyes'
                raise ValueError

        u = numpy.reshape(u, standardShape)
        u = numpy.array(u.astype(numpy.float32), numpy.float32)       # make contiguous 
        
        # Restore the shape of the data to conform to that of the original data 

        u = numpy.transpose(u, inverseOrder)
        u = numpy.array(u.astype(numpy.float32), numpy.float32)       # make contiguous 

        return  u  

def gridGenerator(nlon, nlat, firstLongitude, typeLatitudes, directionLatitudes):
    #--------------------------------------------------------------------------------------------
    #                                      
    #     routine: gridGenerator 
    #                                      
    #     purpose: generate the grid vectors
    #
    #     usage:   lonvals, latvals = sphere.gridGenerator(nlon, nlat, firstLongitude,
    #                                                 typeLatitudes, directionLatitudes)
    #
    #     passed:  nlon - size of longitude vector
    #              nlat - size of latitude vector
    #              firstLongitude -- first vector element
    #              typeLatitudes -- 'even' or 'gaussian'
    #              directionLatitudes -- 'north_to_south' or 'south_to_north'
    #
    #     return:  lonvals, latvals -  the double precision grid vectors
    #
    #     definition: gridGenerator(nlon, nlat, firstLongitude, typeLatitudes, directionLatitudes):
    #
    #--------------------------------------------------------------------------------------------

    if typeLatitudes != 'even' and typeLatitudes != 'gaussian':          # check ltitude request
        print '****************************************************************'
        print 'CANNOT PROCESS THE DATA - typeLatitudes must be even or gaussian'
        print '****************************************************************'
        raise ValueError
        return

    if directionLatitudes != 'north_to_south' and directionLatitudes != 'south_to_north': 
        print '*************************************************************************************'
        print 'CANNOT PROCESS THE DATA - directionLatitudes must be north_to_south or south_to_north'
        print '*************************************************************************************'
        raise ValueError
        return

    delta = 360./nlon                                                    # generate the longitude vector
    lonlist = []

    for i in range(nlon):
            value = firstLongitude + i*delta
            lonlist.append(value)
    lonvals =  numpy.array(lonlist, numpy.float64)



    if typeLatitudes == 'even':                                              # generate latitude vector

        latlist = []
        delta = 180./(nlat - 1)

        for i in range(nlat):
            value = 90. - i*delta
            latlist.append(value)

        latvals =  numpy.array(latlist, numpy.float64)

    else:

        latvals, wts, bnds = gaussian_pts_wts_bnds(nlat)

    if directionLatitudes == 'south_to_north':
        latvals = latvals[::-1]


    return lonvals, latvals

def truncate(wave, a, b, taper = 'yes'):
    #--------------------------------------------------------------------------------------------
    #                                      
    #     routine: truncate 
    #
    #     purpose: perform a triangular truncation of the coefficients in the arrays a and b with
    #              or without tapering. For example, a request for T42 entails eliminating all
    #              values for the total wavenumber above 42. If taper is not None, the remaining
    #              values are tapered.
    # 
    #     usage:    a,b = truncate(wave, a, b)                     -- use tapering
    #               a,b = truncate(wave, a, b, taper = 'no')       -- turn off tapering
    #
    #     passed:  a, b - the arrays
    #              wave - the truncation wavenumber
    #              taper - request for tapering the coefficient values
    #             
    #     returned: a, b - the truncated coefficient arrays 
    #
    #     definition: truncate(wave, a, b, taper = 'yes'):
    #             
    #     note: a, b have indices (nt, n, m)  
    #             
    #     note:  the formula for the exponential tapering was taken from John C. Adams. It is described 
    #            in Sardeshmukh P. D. and Hoskins B. J., 1984, Spatial Smoothing on the Sphere. Mon. Wea. 
    #            Rev., 112, 2524-2529.
    #
    #--------------------------------------------------------------------------------------------

    ashape = a.shape                                                         # -- Preliminary error checks --
    bshape = b.shape
    if ashape != bshape:
        print 'In truncate -- the shape of the two coefficient arrays passed are not the same '
        raise IndexError

    if len(ashape) != 3:
        print 'In truncate -- the coefficient arrays must be 3D'
        raise IndexError

    nb = ashape[1] 
    if wave + 1 > nb:
        print 'In truncate -- the wave number for the truncation is too large'
        raise IndexError

    t = wave + 1                                                            # -- Perform triangular truncation --
    a[:, t:, :] = 0.0 
    b[:, t:, :] = 0.0 


    if taper == 'yes':                                                       # --  Perform exponential tapering also --
        twgt = numpy.zeros(nb, numpy.float32)  
        iw = wave/10
        jp = max(iw, 1)
        jw = 10.0*jp

        con = 1.0/(jw*(jw + 1))

        for j in range(wave + 1):                                            # last value is j = wave
            x = j*(j+1)*con
            value = math.pow(x, jp)
            twgt[j] =math.exp(-value)

        a = numpy.transpose(a, (0,2,1))
        a = a*twgt                                                           # multipy by trianglar weights
        a = numpy.transpose(a, (0,2,1))

        b = numpy.transpose(b, (0,2,1))
        b = b*twgt                                                           # multipy by trianglar weights
        b = numpy.transpose(b, (0,2,1))

        a = numpy.array(a.astype(numpy.float32), numpy.float32)
        b = numpy.array(b.astype(numpy.float32), numpy.float32)

    return  a, b  
 
def help(choice = None):

    import sphere

    if choice is None:
        print """-------------------------------------------------------------------------------------------

        To get an overview of the sphere module, type
             sphere.help('overview')

       CLASS CONTENTS
           Sphere class  --   Vector Analysis and Truncation 
     
                To get information on making an instance type
                    sphere.help('Sphere')
     
                To get information on using a function type
                    sphere.help('functionName')
     
                where functionName is one of the following: 
            
                    div       -- computes the divergence of a vector function                
                    idiv      -- inverts the divergence creating an irrotational vector function            
                    vrt       -- the vorticity of a vector function                
                    ivrt      -- inverts the vorticity creating a divergence_free vector function      
                    idvt      -- inverts the divergence and the vorticity creating a vector function
                    vts       -- computes the derivative of the vector function with respect to latitude 
                    grad      -- computes the gradient of a scalar function
                    igrad     -- inverts the gradient creating a scalar function 
                    slap      -- computes the Laplacian of a scalar function
                    islap     -- inverts the Laplacian of a scalar function
                    vlap      -- computes the Laplacian of a vector function
                    ivlap     -- inverts the Laplacian of a vector function
                    sfvp      -- computes the stream function and the velocity potential of a vector function
                    isfvp     -- inverts the stream function and the velocity potential of a vector function
                    truncation-- truncates scalar or vector data at specified total wavenumber
                    sha       -- computes the spherical harmonic analysis of a scalar function
                    shs       -- computes the spherical harmonic synthesis of a scalar function
                    vha       -- computes the spherical harmonic analysis of a vector function
                    vhs       -- computes the spherical harmonic synthesis of a vector function
                                                    
           Regrid class  --   Regridding 
                                                    
                To get information on making an instance type
                    sphere.help('Regrid')
     
                To get information on using a function type
                    sphere.help('functionName')
     
                where functionName is one of the following: 
                 
                 regridScalar --  transfers scalar data from one global grid to another                  
                 regridVector --  transfers vector data from one global grid to another                  
                 
           Shiftgrid class  --   shifting 
                                                    
                To get information on making an instance type
                    sphere.help('Shiftgrid')
     
                To get information on using a function type
                    sphere.help('functionName')
     
                where functionName is one of the following: 
                 
                 shiftScalar  --  transfers scalar data between an evenly spaced regular and an offset grid    
                 shiftVector  --  transfers vector data between an evenly spaced regular and an offset grid    
                 
                 where the regular grid is defined as one which includes the poles.
                 
       UTILITIES
           Utilities not part of the overall scheme but still of possible interest
                 
                gridGenerator  --  generates the longitude and latitude vectors      
                truncate       --  provides truncation at the spectral coefficient level     
                 
                To get information on their use type
                    sphere.help('gridGenerator')
                    sphere.help('truncate')
                 
       EXAMPLES
                To get a general example type
                    sphere.help('GeneralExample')
                                                    
                To get a suggestion for an example of the the use of the Sphere class which has an
                an answer which can be verified  type
                    sphere.help('SphereTest')
    ----------------------------------------------------------------------------------------------------------------"""
    elif choice == 'overview':                          # look at the whole package
        print sphere.__doc__

    elif choice == 'Sphere':                           # how to make an instance of a class
        print """    --------------------------------------------------------------------------------------
                                           
      To make an instance x of the Sphere class type
     
          x = sphere.Sphere(lonArray , latArray, numberLevels = nlev, numberTimes = ntime,
                                                                     computed_stored = 'computed') 
         
          where nlev and ntime are the actual number of levels and times respectively and the
          keywords are
         
          lonArray = longitude vector (required)
          latArray = latitude vector (required)
          numberLevels = number of levels (optional)
          numberTimes = number of times (optional)
          computed_stored (optional) : 'computed' -- computed Legendre polynomials 
                                         'stored' -- stored Legendre polynomials
                                          This choice involves a 30% storage/speed tradeoff
     
      As an  example, for a 2D field using 'computed Legendre polynomials' type
     
          x = sphere.Sphere(lonArray , latArray)
     
      As an  example, for a 4D field  with  3 levels, 120 times using 'stored Legendre polynomials' type
     
          x = sphere.Sphere(lonArray , latArray, 3, 120, 'stored')
     
          or using the keywords explicitly
     
          x = sphere.Sphere(lonArray , latArray, numberLevels = 3, numberTimes = 120,
                                                                         computed_stored = 'stored') 
          where the order of the keyword entries is immaterial.
                                           
    -----------------------------------------------------------------------------------"""

    elif choice == 'Regrid':
        print """    --------------------------------------------------------------------------------------
                                 
      To make an instance x of the Regrid class type
         
          x = sphere.Regrid(lonArrayOut, latArrayOut, lonArrayIn, latArrayIn, numberLevels = nlev, 
                                                                                    numberTimes = ntime) 
         
          where nlev and ntime are the actual number of levels and times respectively and the keywords are
         
          lonArrayOut = output grid longitude vector (required)
          latArrayOut = output grid latitude vector (required)
          lonArrayIn  = input grid longitude vector (required)
          latArrayIn  = input grid latitude vector (required)
          numberLevels  = input grid number of levels (optional)
          numberTimes = input grid number of times (optional)
         
    -----------------------------------------------------------------------------------"""

    elif choice == 'Shiftgrid':
        print """    --------------------------------------------------------------------------------------
                                           
                                 
      To make an instance x of the Shiftgrid class type
         
          x = sphere.Shiftgrid(lonArray, latArray, numberLevels = nlev, numberTimes = ntime) 
         
          where nlev and ntime are the actual number of levels and times respectively and the keywords are
         
          lonArray = longitude vector (required)
          latArray = latitude vector (required)
          numberLevels = number of levels (optional)
          numberTimes = number of times (optional)
         
    -----------------------------------------------------------------------------------"""

    elif choice == 'GeneralExample':                    # example and a suggestion
        print """    --------------------------------------------------------------------------------------
                                           
             
        Step 1.  Type
                     import sphere
         
        Step 2.  From this documentation determine the class which offers the desired computation. You can avoid
                 reading this documentation by noting that there are only three choices: the Sphere class, Regrid 
                 class and Shiftgrid class to use in ClassName below. A list of the functions in a particular
                 class is obtained by typing
         
                     sphere.ClassName.__doc__
         
        Step 3.  Make an instance, x, of the specific class ClassName using the statement 
         
                     x = sphere.ClassName(argument1, argument2, .........) 
     
                 To get information on and examples of the argument list type
         
                     sphere.ClassName.__init__.__doc__  
         
                 where Classname is Sphere, Regrid or Shiftgrid.
         
        Step 4.  Perform the actual computation using a specific function named functionName, which has been
                 identified in Step 2 by writing
         
                     returned values = x.functionName(argument1, argument2, .........)         
     
                 To get information on the argument list and the returned values type
         
                     sphere.Clasname.functionName.__doc__ 
         
    -----------------------------------------------------------------------------------"""

    elif choice == 'SphereTest':
        print """    --------------------------------------------------------------------------------------
                                       
         
        Typing 
         
            cdat sphere.py
         
        generates some testing of the spheremodule using analytical functions as fields.
         
        For additional testing using real geophysical data, you might try the following exercise. 
         
            Step 1. Get winds u and v and their grid vectors, longitude values (lonvals) and 
            latitude values,(latvals)  from somewhere. This example uses 2D fields for 
            simplicity. The fields must be global without missing values. 
                    
         
            Step 2. Make an instance of the Sphere class, x, as
     
                        x = sphere.Sphere(lonvals, latvals)
     
            Step 3. Compute the streamfunction, sf, and the velocity potential, vp, using 
     
                        sf, vp = x.sfvp(u, v)
     
            Step 4. Compute the source for the streamfunction, sf_source, and the velocity potential, vp_source,
                    using the scalar Laplacian
     
                        sf_source = x.slap(sf)
                        vp_source = x.slap(vp)
     
            Step 5. Compute the source for the streamfunction, vort, and the velocity potential, div, directly 
                    using the divergence and the vorticity
     
                        vort = x.vrt(u, v)
                        div = x.div(u, v)
     
            Step 6. Compare the results for equality, sf_source with vort and vp_source with div. If the comparison
                    fails, please complain about it.
         
    -----------------------------------------------------------------------------------"""

    elif choice == 'gridGenerator':                     # utilities
        print """    -----------------------------------------------------------------------------
                                           
          routine: gridGenerator 
                                           
          purpose: generate the grid vectors
     
          usage:   lonvals, latvals = sphere.gridGenerator(nlon, nlat, firstLongitude,
                                                      typeLatitudes, directionLatitudes)
     
          passed:  nlon - size of longitude vector
                   nlat - size of latitude vector
                   firstLongitude -- first vector element
                   typeLatitudes -- 'even' or 'gaussian'
                   directionLatitudes -- 'north_to_south' or 'south_to_north'
     
          return:  lonvals, latvals -  the double precision grid vectors
     
          definition: gridGenerator(nlon, nlat, firstLongitude, typeLatitudes, directionLatitudes):
     
    -----------------------------------------------------------------------------------"""

    elif choice == 'truncate':
        print """    -------------------------------------------------------------------------------------------
                                           
          routine: truncate 
     
          purpose: perform a triangular truncation of the coefficients in the arrays a and b with
                   or without tapering. For example, a request for T42 entails eliminating all
                   values for the total wavenumber above 42. If taper is not None, the remaining
                   values are tapered.
      
          usage:    a,b = truncate(wave, a, b)                     -- use tapering
                    a,b = truncate(wave, a, b, taper = 'no')       -- turn off tapering
     
          passed:  a, b - the arrays
                   wave - the truncation wavenumber
                   taper - request for tapering the coefficient values
                  
          returned: a, b - the truncated coefficient arrays 
     
          definition: truncate(wave, a, b, taper = 'yes'):
                  
          note: a, b have indices (nt, n, m)  
                  
          note:  the formula for the exponential tapering was taken from John C. Adams. It is described 
                 in Sardeshmukh P. D. and Hoskins B. J., 1984, Spatial Smoothing on the Sphere. Mon. Wea. 
                 Rev., 112, 2524-2529.
     
    -------------------------------------------------------------------------------------------"""

    elif choice == 'div':                               # Sphere class method functions
        print sphere.Sphere.div.__doc__

    elif choice == 'idiv':
        print sphere.Sphere.idiv.__doc__

    elif choice == 'vrt':
        print sphere.Sphere.vrt.__doc__

    elif choice == 'ivrt':
        print sphere.Sphere.ivrt.__doc__

    elif choice == 'idvt':
        print sphere.Sphere.idvt.__doc__

    elif choice == 'vts':
        print sphere.Sphere.vts.__doc__

    elif choice == 'grad':
        print sphere.Sphere.grad.__doc__

    elif choice == 'igrad':
        print sphere.Sphere.igrad.__doc__

    elif choice == 'slap':
        print sphere.Sphere.slap.__doc__

    elif choice == 'islap':
        print sphere.Sphere.islap.__doc__

    elif choice == 'vlap':
        print sphere.Sphere.vlap.__doc__

    elif choice == 'ivlap':
        print sphere.Sphere.ivlap.__doc__
    elif choice == 'sfvp':
        print sphere.Sphere.sfvp.__doc__

    elif choice == 'isfvp':
        print sphere.Sphere.isfvp.__doc__

    elif choice == 'truncation':
        print sphere.Sphere.truncation.__doc__

    elif choice == 'sha':
        print sphere.Sphere.sha.__doc__

    elif choice == 'shs':
        print sphere.Sphere.shs.__doc__

    elif choice == 'vha':
        print sphere.Sphere.vha.__doc__

    elif choice == 'vhs':
        print sphere.Sphere.vhs.__doc__

    elif choice == 'regridScalar':                               # Regrid class method functions
        print sphere.Regrid.regridScalar.__doc__

    elif choice == 'regridVector':
        print sphere.Regrid.regridVector.__doc__

    elif choice == 'shiftScalar':                               # Regrid class method functions
        print sphere.Shiftgrid.shiftScalar.__doc__

    elif choice == 'shiftVector':
        print sphere.Shiftgrid.shiftVector.__doc__

    else:
        print 'Unknown Request - cannot provide help for ', choice 

    return None

