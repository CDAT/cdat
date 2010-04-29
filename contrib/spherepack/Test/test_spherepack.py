# Adapted for numpy/ma/cdms2 by convertcdms.py
"""Documentation for module spheretest: an automatic test for sphere, an interface to spherepack
  
   TESTING 
 
        Typing 
         
            cdat spheretest.py
         
        generates some testing of the spheremodule using analytically generated winds as the input fields. There are 3 tests.
         
        test 1
             Starting with analytically generated winds, it calculates the stream function and velocity potential using 
             Spherepack and compares with an analytically generated stream function and velocity potential.
         
        test 2
             Starting with analytically generated winds on a regular grid, one which includes the poles, it calculates the
             winds on a grid offset by half a grid point in longitude and latitude,
         
        test 3
             Starting with analytically generated winds on an evenly spaced grid, it calculates the winds on a gaussian grid. 

        For each test an rms error is calculated and written to the screen. In addition, the output to the screen is placed
        in the file screen.asc. The numerical results are written to a series of Netcdf files. The contents of these files
        are described in the screen output.
  
        After completing the tests, documentation is written to the file spheremodule.doc.
 
   DOCUMENTATION
  
        Documentation written to the file spheremodule.doc can be obtained without running the tests after importing the
        spheretest module by typing 
  
               spheretest.document() 
  
        A brief view of the documentation consisting of the overview can be written to the file spheremodule.doc after
        importing the sphere module by typing 
  
               spheretest.document(brief = 'yes') 
"""

import sys, string, sphere
import spherepack, numpy, math
#spherepack.set_pyfort_option(spherepack.MIRROR)
debug = 0                                           # set to 1 for debug prints
radius = 6.37122e06

writeTestcase = 'yes'
try:
    import cdms2
except ImportError:
    print 'Can not write test case results to netCDF files without module cdms2'
    writeTestcase = 'no'

def document(brief = 'no'):
    #----------------------------------------------------------------------------------------
    #
    #    purpose:   'document' writes the doc strings contained in the sphere module
    #                to a file as documentation for the user
    #
    #    usage:     import sphere
    #               sphere.document()   
    #    
    #    passed :   nothing
    #
    #    returned:  nothing
    #
    #----------------------------------------------------------------------------------------

    std = sys.stdout 
    sys.stdout = open('spheremodule.doc', 'w') 

    print '**********************************************************************************************\n'  
    print '****************** Overview of the CDAT interface to the NCAR SPHEREPACK 3.0 *****************\n'
    print '**********************************************************************************************\n'  
    print sphere.__doc__
    print
    print
    if brief != 'no':
        return None

    print '    ************************************************************************************\n'  
    print '    ************************** Sphere class documentation ******************************\n'
    print '    ************************************************************************************\n'  
    sphere.help('Sphere')                           # how to make an instance of a class
    print

    print '    **************************** Sphere class functions ********************************\n'
    SphereList = ['div', 'idiv', 'vrt', 'ivrt', 'idvt', 'vts', 'grad', 'igrad', 'slap',
                   'islap', 'vlap', 'ivlap', 'sfvp', 'isfvp', 'truncation', 'sha', 'shs', 'vha', 'vhs']  
    for name in SphereList:
        command = "sphere.help(" + "name" + ")"
        exec command 
        print

    print '    ************************************************************************************\n'  
    print '    **************************** Regrid class documentation ****************************\n'
    print '    ************************************************************************************\n'  
    sphere.help('Regrid')                           # how to make an instance of a class
    print
    print '    ****************************** Regrid class functions ******************************\n'
    sphere.help('regridScalar') 
    print
    sphere.help('regridVector') 
    print

    print '    ************************************************************************************\n'  
    print '    *************************** Shiftgrid class documentation **************************\n'
    print '    ************************************************************************************\n'  
    sphere.help('Shiftgrid')                           # how to make an instance of a class
    print
    print '    ***************************** Shiftgrid class functions ****************************\n'
    sphere.help('shiftScalar') 
    print
    sphere.help('shiftVector') 
    print

    print '    ************************************************************************************\n'  
    print '    **************************** Utility documentation *********************************\n'
    print '    ************************************************************************************\n'  
    sphere.help('gridGenerator')                           # how to make an instance of a class
    print
    sphere.help('truncate') 
    print

    sys.stdout = std

    return None
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# ++++++++++++++++++++++++++++++++ Autotest Functions +++++++++++++++++++++++++++++++++
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

def sfvp():
    #-----------------------------------------------------------------------------------
    #                                      
    #     purpose:  starting with analytically generated winds, calculate the stream
    #               function and velocity potential using Spherepack and compare with
    #               analytically generated stream function and velocity potential.
    #
    #     usage:    sfvp() 
    #
    #     passed :  nothing  
    #
    #     returned: nothing 
    #
    #-----------------------------------------------------------------------------------
    sendmsg('**************** calculate the stream function and velocity potential on gaussian grid  *****************')
    sendmsg(' ')

    testError = 0
    comp = 'computed'
    nlon = 128
    nlat = 64
    lonvals, latvals, timevals, u, v, sfexact, vpexact = sphere_test(nlon, nlat, 'v', 'gaussian')

    nt = len(timevals)
    x = sphere.Sphere(lonvals, latvals, numberLevels = 0, numberTimes = nt, computed_stored = comp)

    sfcal, vpcal = x.sfvp(u, v)

    scale = radius                                            # scale exact functions to radius for the earth
    sfexact = sphere.geoscale(scale, sfexact)
    vpexact = sphere.geoscale(scale, vpexact)

    sfexact = remove_offset(sfcal, sfexact)                      # subtract the offset
    vpexact = remove_offset(vpcal, vpexact)                     

    sendmsg('******* compare results')
    rms = rmserror(sfcal, sfexact)                              # stream function rms error
    sendmsg('expected normalized rms error in stream function computation is less than 1.e-05')
    sendmsg('calculated normalized rms error in stream function computation =', rms)
    sendmsg(' ')
    if rms > 1.e-05:
        testError = testError + 1

    rms = rmserror(vpcal, vpexact)                              # velocity potential rms error
    sendmsg( 'expected normalized rms error in velocity potential computation is less than 1.e-05')
    sendmsg( 'calculated normalized rms error in velocity potential computation =', rms)
    sendmsg(' ')
    if rms > 1.e-05:
        testError = testError + 1


    if writeTestcase == 'yes':
        sendmsg('******* write data')
        sendmsg( 'calculated stream function written to sfcal.nc')     # write netcdf file
        writeField(lonvals, latvals, timevals, 'sfcal', sfcal) 
        sendmsg( 'calculated velocity potential written to vpcal.nc') 
        writeField(lonvals, latvals, timevals, 'vpcal', vpcal) 
        sendmsg(' ')

        sendmsg( 'exact stream function written to sfexact.nc')     # write netcdf file
        writeField(lonvals, latvals, timevals, 'sfexact', sfexact) 
        sendmsg( 'exact velocity potential written to vpexact.nc') 
        writeField(lonvals, latvals, timevals, 'vpexact', vpexact) 
        sendmsg(' ')
        sendmsg(' ')

    return testError
 
def shift():
    #-----------------------------------------------------------------------------------
    #                                      
    #     purpose:  starting with analytically generate winds on a regular grid, one
    #               which includes the poles, calculate the winds on a grid offset by
    #               half a grid point in longitude and latitude,
    #
    #     usage:    shift() 
    #
    #     passed :  nothing  
    #
    #     returned:  nothing 
    #
    #-----------------------------------------------------------------------------------
    sendmsg('************ vector shift from a regular evenly spaced grid  and a offset evenly spaced grid ************')
    sendmsg(' ')

    testError = 0
    comp = 'computed'
    nlon = 144
    nlat = 72

    lonvals, latvals, timevals, u, v = vshift_testfunction(nlon, nlat)
    offlonvals, offlatvals, timevals, uexact, vexact = vshift_testfunction(nlon, nlat, grid_type = 'offset')

    nt = len(timevals)
    x = sphere.Shiftgrid(lonvals, latvals, numberTimes = nt)

    ucal, vcal = x.shiftVector(u,v)

    sendmsg('******* compare results')
    rms = rmserror(ucal, uexact)                                          # zonal wind rms error
    sendmsg( 'expected normalized  error in zonal wind computation is less than 1.e-06')
    sendmsg( 'calculated normalized rms error in zonal wind computation =', rms)
    sendmsg( ' ')
    if rms > 1.e-06:
        testError = testError + 1

    rms = rmserror(vcal, vexact)
    sendmsg( 'expected normalized rms error in meridional wind computation is less than 1.e-06')
    sendmsg( 'calculated normalized rms error in meridional wind computation =', rms)
    sendmsg(' ')
    if rms > 1.e-06:
        testError = testError + 1


    if writeTestcase == 'yes':
        sendmsg( '******* write data')
        sendmsg( 'calculated zonal wind on the offset grid written to uoffcal.nc')     # write netcdf file
        writeField(offlonvals, offlatvals, timevals, 'uoffcal', ucal) 
        sendmsg( 'calculated meridional wind on the offset grid wriiten to voffcal.nc') 
        writeField(offlonvals, offlatvals, timevals, 'voffcal', vcal) 
        sendmsg(' ')

        sendmsg( 'exact zonal wind on the offset grid written to uoffexact.nc')          # write netcdf file
        writeField(offlonvals, offlatvals, timevals, 'uoffexact', uexact) 
        sendmsg( 'exact meritional wind on the offset grid written to voffexact.nc')
        writeField(offlonvals, offlatvals, timevals, 'voffexact', vexact) 
        sendmsg(' ')
        sendmsg(' ')

    return testError

def regrid():
    #-----------------------------------------------------------------------------------
    #                                      
    #     purpose:  starting with analytically generate winds on an evenly spaced grid, 
    #               calculate the winds on a gaussian grid. 
    #
    #     usage:    regrid() 
    #
    #     passed :  nothing  
    #
    #     returned:  nothing 
    #
    #-----------------------------------------------------------------------------------
    sendmsg('*******************************  vector regridding on a sphere  ***********************************')
    sendmsg(' ')

    testError = 0
    nlone = 36
    nlate = 19
    lonArrayIn, latArrayIn, timevals, uIn, vIn = regrid_testfunction(nlone, nlate, 'v', 'even')

    nlong = 128
    nlatg = 64
    lonArrayOut, latArrayOut, timevals, uOut, vOut = regrid_testfunction(nlong, nlatg, 'v', 'gaussian')

    nt = len(timevals)
    x = sphere.Regrid(lonArrayOut, latArrayOut, lonArrayIn, latArrayIn, numberTimes = nt) 

    ucal, vcal = x.regridVector(uIn, vIn)

    sendmsg('******* compare results')
    rms = rmserror(uOut, ucal)
    sendmsg( 'expected normalized rms error in regridded zonal wind is less than 1.e-06')
    sendmsg( 'calculated normalized rms error in regridded zonal wind =', rms)
    sendmsg(' ')
    if rms > 1.e-06:
        testError = testError + 1


    rms = rmserror(vOut, vcal)
    sendmsg( 'expected normalized rms error in regridded meridonal wind is less than 1.e-06')
    sendmsg( 'calculted normalized rms error in regridded meridonal wind =', rms)
    sendmsg(' ')
    if rms > 1.e-06:
        testError = testError + 1


    if writeTestcase == 'yes':
        sendmsg('******* write data')
        sendmsg( 'calculated zonal wind on the new grid written to uregridcal.nc')     # write netcdf file
        writeField(lonArrayOut, latArrayOut, timevals, 'uregridcal', ucal) 
        sendmsg( 'calculated meridional wind on the new grid written to vregridcal.nc') 
        writeField(lonArrayOut, latArrayOut, timevals, 'vregridcal', vcal) 
        sendmsg(' ')

        sendmsg( 'exact zonal wind on the new grid written to uregridexact.nc')     # write netcdf file
        writeField(lonArrayOut, latArrayOut, timevals, 'uregridexact', uOut) 
        sendmsg( 'exact meridional wind on the new grid written to vregridexact.nc') 
        writeField(lonArrayOut, latArrayOut, timevals, 'vregridexact', vOut) 
        sendmsg(' ')

        sendmsg( 'original zonal wind on the grid written to uorig.nc')    
        writeField(lonArrayIn, latArrayIn, timevals, 'uorig', uIn) 
        sendmsg( 'original meridional wind on the grid written to vorig.nc')
        writeField(lonArrayIn, latArrayIn, timevals, 'vorig', vIn) 

    return testError

def writeField(lons, lats, tmes, varname, dataField): 
    #-----------------------------------------------------------------------------------
    #                                      
    #     purpose: write an output field 
    #
    #     usage:  
    #
    #     passed :  lons, lats, tmes, filename, varname, dataField 
    #
    #     returned: None 
    #
    #-----------------------------------------------------------------------------------
    fileObj = cdms2.createDataset(varname + '.nc')  

    lon_axis = fileObj.createAxis('longitude', lons)  
    lon_axis.units = "degrees_east"  
    lat_axis = fileObj.createAxis('latitude', lats)  
    lat_axis.units = "degrees_north"  
    tme_axis = fileObj.createAxis('time', tmes)  
    tme_axis.units = "months"  

    var = fileObj.createVariable(varname, numpy.float32, (tme_axis, lat_axis, lon_axis))   # variable without data

    var[:] = dataField                                                                       # copy in the data

    fileObj.close() 

    return None

def writeasc(name, a):
    #-----------------------------------------------------------------------------------
    #
    #    purpose: write the data into an ascii file
    # 
    #    passed : name - filename prefix 
    #             a - data
    #             
    #    returned: return
    #
    #-----------------------------------------------------------------------------------

    r = numpy.ravel(a)

    output = open(name + '.asc', 'w')
    Format = '%12.4E'

    count = 0
    for item in r:
        output.write(Format % (item,))
        count = count + 1 
        if count == 8:
            output.write('\n')
            count = 0

    output.close()

    return None

def sendmsg(msg, value = None, screen = 'no'):
    #------------------------------------------------------------------------------
    #
    #    purpose: send the same message to the screen and to a file
    # 
    #    passed :  msg - the string
    #             
    #    returned: return
    #
    #------------------------------------------------------------------------------
    if value is None:
        if screen != 'no':
            print msg
        output.write(msg + '\n')
    else:
        if screen != 'no':
            print msg, `value`
        output.write(msg + ' %15.11e\n' % (value,))

    return None

def remove_offset(cal, test):
    #-----------------------------------------------------------------------------------
    #
    #    purpose: shift the array so that the test array has an approximate zero mean to agree
    #             with the calculated one.
    # 
    #    passed : the two data sets 
    #             
    #    returned: test
    #
    #-----------------------------------------------------------------------------------

    dif = numpy.ravel(cal) - numpy.ravel(test)        # remove offset between the test and calculated arrays
    offset = numpy.sum(dif)/len(dif)
    test = test + offset
    test = test.astype(numpy.float32)

    return  test


def rmserror(data1, data2):
    #-----------------------------------------------------------------------------------
    #
    #    purpose: compute the rms error for two data sets having the same shape
    # 
    #    passed : the two data sets 
    #             
    #    returned: rms error
    #
    #-----------------------------------------------------------------------------------

    if data1.shape != data2.shape:
        print 'Error in shape in rmserror'
        raise ValueError

    d1 = numpy.ravel(data1)
    d2 = numpy.ravel(data2)

    sq = d1*d1                                         # find average magnitude
    avg = numpy.sqrt( numpy.sum(sq)/len(d1))

    sq = (d1 - d2)*(d1 - d2)
    error = numpy.sum(sq)/len(d1)
    rmserror =  (numpy.sqrt(error))/avg

    return  rmserror

def sphere_test(nlon, nlat, sorv = 's',  grid_choice = 'even'):
    #-----------------------------------------------------------------------------------
    #                                      
    #     purpose: compute the scalar and vector test functions used in testrssph.f
    #              and testtrvsph.f in geophysical coordinates
    #
    #-----------------------------------------------------------------------------------

    if sorv != 's' and sorv != 'v':
        print 'Must pass s or v to select scalar or vector'
        raise ValueError
        return

    # generate the correct geophysical grid points

    lonlistrad = []                             # longitudes in radians
    delta = 2.*math.pi/nlon

    for i in range(nlon):
        value = i*delta
        lonlistrad.append(value)

    lons =  numpy.array(lonlistrad, numpy.float64)                # in radians for computation

    lonlist = map( (lambda x: (180./math.pi)*x), lonlistrad)
    lonvals =  numpy.array(lonlist, numpy.float64)                # in degrees for return

    if grid_choice == 'even':

        latlistrad = []                                               # latitudes in radians
        delta = math.pi/(nlat - 1)

        for i in range(nlat):
            value = math.pi/2. - i*delta
            latlistrad.append(value)

        lats =  numpy.array(latlistrad, numpy.float64)             # in radians for computation  

        latlist = map( (lambda x: (180./math.pi)*x), latlistrad)
        latvals =  numpy.array(latlist, numpy.float64)             # array of latitudes in degrees


    elif grid_choice == 'gaussian':

        ldwork = nlat*(nlat + 2)
        work = numpy.zeros((ldwork,),'d')
        points, wts, ierror = spherepack.gaqd(nlat, work)            # get colatitudes from gaqd
        if ierror != 0:
            print 'In return from call to gaqd ierror = ', ierror
            raise ValueError

        # convert points to latitudes

        colatlistrad = list(points)                                   
        latlistrad = map( (lambda x: math.pi/2. - x), colatlistrad)    # convert to latitudes
        lats =  numpy.array(latlistrad, numpy.float64)             # in radians for computation  

        latlist = map( (lambda x: (180./math.pi)*x), latlistrad)
        latvals =  numpy.array(latlist, numpy.float64)

    else:
        print 'grid_choice must be even or gaussian'
        raise ValueError
        return

    # generate the data on geophysical grid points

    timevals = numpy.array( [0.0], numpy.float)

    if sorv == 's':

        sf = numpy.zeros((1,nlat,nlon), numpy.float)           # malloc in c order

        for i in range(len(lons)):                                 # calculate scalar test function
            p = lons[i]
            cosp = math.cos(p)
            sinp = math.sin(p)

            for j in range(len(lats)):
                t = lats[j]
                cost = math.cos(t)
                sint = math.sin(t)
                x = cost*cosp
                y = cost*sinp
                z = sint
                sf[0,j,i] = math.exp(x*y*z)                           # c order
            
        sf = sf.astype(numpy.float32)

        return lonvals, latvals, timevals, sf

    else:

        u = numpy.zeros((1,nlat,nlon), numpy.float)                  # malloc in c order
        v = numpy.zeros((1,nlat,nlon), numpy.float)
        sf = numpy.zeros((1,nlat,nlon), numpy.float)
        vp = numpy.zeros((1,nlat,nlon), numpy.float)

        for i in range(len(lons)):                                     # calculate vector test functions
            p = lons[i]
            cosp = math.cos(p)
            sinp = math.sin(p)

            for j in range(len(lats)):
                t = lats[j]
                cost = math.cos(t)
                sint = math.sin(t)
                x = cost*cosp
                y = cost*sinp
                z = sint
                ex = math.exp(x)
                ey = math.exp(y)
                ez = math.exp(z)
                emz = math.exp(-z)
                u[0,j,i] = -ex*sinp + ey*sint*sinp + emz*cost               # c order
                v[0,j,i] = -( ex*sint*cosp - ey*cosp - ez*cost ) 
                sf[0,j,i] = ey + emz   
                vp[0,j,i] = ex + ez   

        u = u.astype(numpy.float32)
        v = v.astype(numpy.float32)
        sf = sf.astype(numpy.float32)
        vp = vp.astype(numpy.float32)

        return lonvals, latvals, timevals, u, v, sf, vp

def regrid_testfunction(nlon, nlat, sorv = 'h',  grid_choice = 'even'):
    #-----------------------------------------------------------------------------------
    #                                      
    #     purpose: compute the scalar and vector test functions used in testrssph.f
    #              and testtrvsph.f in geophysical coordinates
    #
    #     usage:  
    #
    #
    #-----------------------------------------------------------------------------------

    if sorv != 's' and sorv != 'v':
        print 'Must pass s or v to select scalar or vector'
        raise ValueError
        return

    # generate the correct geophysical grid points

    lonlistrad = []                             # longitudes in radians
    delta = 2.*math.pi/nlon

    for i in range(nlon):
        value = i*delta
        lonlistrad.append(value)

    lons =  numpy.array(lonlistrad, numpy.float64)                # in radians for computation

    lonlist = map( (lambda x: (180./math.pi)*x), lonlistrad)
    lonvals =  numpy.array(lonlist, numpy.float64)                # in degrees for return

    if grid_choice == 'even':

        latlistrad = []                                               # latitudes in radians
        delta = math.pi/(nlat - 1)

        for i in range(nlat):
            value = math.pi/2. - i*delta
            latlistrad.append(value)

        lats =  numpy.array(latlistrad, numpy.float64)             # in radians for computation  

        latlist = map( (lambda x: (180./math.pi)*x), latlistrad)
        latvals =  numpy.array(latlist, numpy.float64)             # array of latitudes in degrees


    elif grid_choice == 'gaussian':

        ldwork = nlat*(nlat + 2)
        work = numpy.zeros((ldwork,),'d')
        points, wts, ierror = spherepack.gaqd(nlat, work)            # get colatitudes from gaqd
        if ierror != 0:
            print 'In return from call to gaqd ierror = ', ierror
            raise ValueError

        # convert points to latitudes

        colatlistrad = list(points)                                   
        latlistrad = map( (lambda x: math.pi/2. - x), colatlistrad)    # convert to latitudes
        lats =  numpy.array(latlistrad, numpy.float64)             # in radians for computation  

        latlist = map( (lambda x: (180./math.pi)*x), latlistrad)
        latvals =  numpy.array(latlist, numpy.float64)

    else:
        print 'grid_choice must be even or gaussian'
        raise ValueError
        return

    # generate the data on geophysical grid points

    timevals = numpy.array( [0.0], numpy.float)

    if sorv == 's':

        sf = numpy.zeros((1,nlat,nlon), numpy.float)           # malloc in c order

        for i in range(len(lons)):                                 # calculate scalar test function
            p = lons[i]
            cosp = math.cos(p)
            sinp = math.sin(p)

            for j in range(len(lats)):
                t = lats[j]
                cost = math.cos(t)
                sint = math.sin(t)
                x = cost*cosp
                y = cost*sinp
                z = sint
                sf[0,j,i] = math.exp(x*y*z)                           # c order
            
        sf = sf.astype(numpy.float32)

        return lonvals, latvals, timevals, sf

    else:

        u = numpy.zeros((1,nlat,nlon), numpy.float)                  # malloc in c order
        v = numpy.zeros((1,nlat,nlon), numpy.float)

        for i in range(len(lons)):                                     # calculate vector test functions
            p = lons[i]
            cosp = math.cos(p)
            sinp = math.sin(p)

            for j in range(len(lats)):
                t = lats[j]
                cost = math.cos(t)
                sint = math.sin(t)
                x = cost*cosp
                y = cost*sinp
                z = sint
                ex = math.exp(x)
                ey = math.exp(y)
                ez = math.exp(z)
                emz = math.exp(-z)
                u[0,j,i] = -ex*sinp + ey*sint*sinp + emz*cost               # c order
                v[0,j,i] = -( ex*sint*cosp - ey*cosp - ez*cost ) 

        u = u.astype(numpy.float32)
        v = v.astype(numpy.float32)

        return lonvals, latvals, timevals, u, v


def sshift_testfunction(nlon, nlat, grid_type = 'regular'):
    #-----------------------------------------------------------------------------------
    #                                      
    #     purpose: compute the scalar test function used in testsshifte.f
    #              in geophysical coordinates
    #
    #     usage:  
    #
    #
    #-----------------------------------------------------------------------------------

    if grid_type != 'regular' and grid_type != 'offset':
        print 'Must pass regular or offset to select grid from sshift_testfunction'
        raise ValueError
        return

    # ------- generate the longitude grid points

    reg_lonlistrad = []                                            # regular longitudes in radians
    off_lonlistrad = []                                            # offset longitudes in radians
    delta = 2.*math.pi/nlon
    shiftlon = delta/2.

    for i in range(nlon):
        value = i*delta
        svalue = shiftlon + value
        reg_lonlistrad.append(value)
        off_lonlistrad.append(svalue)

    reglons =  numpy.array(reg_lonlistrad, numpy.float64)         # for use in computation
    offlons =  numpy.array(off_lonlistrad, numpy.float64)         # for use in computation

    lonlist = map( (lambda x: (180./math.pi)*x), reg_lonlistrad)  # degrees for return
    reglonvals =  numpy.array(lonlist, numpy.float64)

    lonlist = map( (lambda x: (180./math.pi)*x), off_lonlistrad)
    offlonvals =  numpy.array(lonlist, numpy.float64)


    # generate the data on geophysical grid points

    timevals = numpy.array( [0.0], numpy.float)

    if grid_type == 'regular':

        # -------- generate the regular latitude grid points
        latlistrad = []                                                  # latitudes in radians
        delta = math.pi/nlat

        for j in range(nlat + 1):
            value = math.pi/2. - j*delta
            latlistrad.append(value)

        lats =  numpy.array(latlistrad, numpy.float64)              # latitudes for computation  

        latlist = map( (lambda x: (180./math.pi)*x), latlistrad)
        latvals =  numpy.array(latlist, numpy.float64)              # array of latitudes in degrees

        sf = numpy.zeros((1,nlat + 1,nlon), numpy.float32)                # malloc in c order

        for i in range(len(reglons)):                                      # calculate scalar test function
            p = reglons[i]
            cosp = math.cos(p)
            sinp = math.sin(p)

            for j in range(len(lats)):
                t = lats[j]
                cost = math.cos(t)
                sint = math.sin(t)
                x = cost*cosp
                y = cost*sinp
                z = sint
                sf[0,j,i] = math.exp(x + y + z)                           # c order
            
        sf = sf.astype(numpy.float32)

        return reglonvals, latvals, timevals, sf

    else:

        # -------- generate the offset latitude grid points
        latlistrad = []                                                   # latitudes in radians
        delta = math.pi/nlat
        shiftlat = delta/2.

        for j in range(nlat):
            value = math.pi/2. - shiftlat  - j*delta
            latlistrad.append(value)

        lats =  numpy.array(latlistrad, numpy.float64)                 # latitudes for computation  

        latlist = map( (lambda x: (180./math.pi)*x), latlistrad)
        latvals =  numpy.array(latlist, numpy.float64)                 # array of latitudes in degrees

        sf = numpy.zeros((1,nlat,nlon), numpy.float32)                   # malloc in c order

        for i in range(len(offlons)):                                      # calculate scalar test function
            p = offlons[i]
            cosp = math.cos(p)
            sinp = math.sin(p)

            for j in range(len(lats)):
                t = lats[j]
                cost = math.cos(t)
                sint = math.sin(t)
                x = cost*cosp
                y = cost*sinp
                z = sint
                sf[0,j,i] = math.exp(x + y + z)                            # c order
            
        sf = sf.astype(numpy.float32)

        return offlonvals, latvals, timevals, sf


def vshift_testfunction(nlon, nlat, grid_type = 'regular'):
    #-----------------------------------------------------------------------------------
    #                                      
    #     purpose: compute the vector test function used in testvshifte.f
    #              in geophysical coordinates
    #
    #     usage:  
    #
    #
    #-----------------------------------------------------------------------------------

    if grid_type != 'regular' and grid_type != 'offset':
        print 'Must pass regular or offset to select grid from vshift_testfunction'
        raise ValueError
        return

    # ------- generate the longitude grid points

    reg_lonlistrad = []                                            # regular longitudes in radians
    off_lonlistrad = []                                            # offset longitudes in radians
    delta = 2.*math.pi/nlon
    shiftlon = delta/2.

    for i in range(nlon):
        value = i*delta
        svalue = shiftlon + value
        reg_lonlistrad.append(value)
        off_lonlistrad.append(svalue)

    reglons =  numpy.array(reg_lonlistrad, numpy.float64)         # for use in computation
    offlons =  numpy.array(off_lonlistrad, numpy.float64)         # for use in computation

    lonlist = map( (lambda x: (180./math.pi)*x), reg_lonlistrad)  # degrees for return
    reglonvals =  numpy.array(lonlist, numpy.float64)

    lonlist = map( (lambda x: (180./math.pi)*x), off_lonlistrad)
    offlonvals =  numpy.array(lonlist, numpy.float64)


    # generate the data on geophysical grid points

    timevals = numpy.array( [0.0], numpy.float)

    if grid_type == 'regular':

        # -------- generate the regular latitude grid points

        latlistrad = []                                                  # latitudes in radians
        delta = math.pi/nlat

        for j in range(nlat + 1):
            value = math.pi/2. - j*delta
            latlistrad.append(value)

        lats =  numpy.array(latlistrad, numpy.float64)              # latitudes for computation  

        latlist = map( (lambda x: (180./math.pi)*x), latlistrad)
        latvals =  numpy.array(latlist, numpy.float64)              # array of latitudes in degrees

        u = numpy.zeros((1,nlat + 1,nlon), numpy.float32)                  # malloc in c order
        v = numpy.zeros((1,nlat + 1,nlon), numpy.float32)

        for i in range(len(reglons)):                                      # calculate scalar test function
            p = reglons[i]
            cosp = math.cos(p)
            sinp = math.sin(p)

            for j in range(len(lats)):
                t = lats[j]
                cost = math.cos(t)
                sint = math.sin(t)
                x = cost*cosp
                y = cost*sinp
                z = sint
                ex = math.exp(x)
                ey = math.exp(y)
                ez = math.exp(z)
                emz = math.exp(-z)
                u[0,j,i] = -ex*sinp + ey*sint*sinp + emz*cost               # c order
                v[0,j,i] = -ex*sint*cosp + ey*cosp + ez*cost 
            
        u = u.astype(numpy.float32)
        v = v.astype(numpy.float32)

        return reglonvals, latvals, timevals, u, v

    else:

        # -------- generate the offset latitude grid points

        latlistrad = []                                                   # latitudes in radians
        delta = math.pi/nlat
        shiftlat = delta/2.

        for j in range(nlat):
            value = math.pi/2. - shiftlat  - j*delta
            latlistrad.append(value)

        lats =  numpy.array(latlistrad, numpy.float64)                 # latitudes for computation  

        latlist = map( (lambda x: (180./math.pi)*x), latlistrad)
        latvals =  numpy.array(latlist, numpy.float64)                 # array of latitudes in degrees

        u = numpy.zeros((1,nlat,nlon), numpy.float32)                  # malloc in c order
        v = numpy.zeros((1,nlat,nlon), numpy.float32)

        for i in range(len(offlons)):                                      # calculate scalar test function
            p = offlons[i]
            cosp = math.cos(p)
            sinp = math.sin(p)

            for j in range(len(lats)):
                t = lats[j]
                cost = math.cos(t)
                sint = math.sin(t)
                x = cost*cosp
                y = cost*sinp
                z = sint
                ex = math.exp(x)
                ey = math.exp(y)
                ez = math.exp(z)
                emz = math.exp(-z)
                u[0,j,i] = -ex*sinp + ey*sint*sinp + emz*cost               # c order
                v[0,j,i] = -ex*sint*cosp + ey*cosp + ez*cost 
            
        u = u.astype(numpy.float32)
        v = v.astype(numpy.float32)

        return offlonvals, latvals, timevals, u, v

if __name__ == "__main__":
    output = open('test.asc', 'w')               # global file name

    print 'Running the test computations'
    te1 = sfvp() 
    te2 = shift()
    te3 = regrid()
    testError =  te1 + te2 + te3
    write = document()

    sendmsg(' ')
    sendmsg('*********')
    sendmsg('General information on the use of SPHERPACK has been written to the file spheremodule.doc.')
    sendmsg('*********')
    sendmsg(' ')

    if testError == 0:
        print 'Testing Completed Successfully'
    else:
        print 'Testing Completed But It May Have Problems'
    print 'Some details on the testing have been written to the file test.asc.'
    print 'General information on the use of SPHEREPACK has been written to the file spheremodule.doc.'

    output.close()
