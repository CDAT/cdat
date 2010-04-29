
"""Documentation for module cssgridtest: an automatic test for cssgrid, an interface to the ngmath CSSGRID
  
   TESTING 
 
        Typing 
         
            cdat cssgridtest.py
         
        generates some testing of the cssgridmodule using analytical functions as fields. It also writes a 
        hard copy of the documentation to the file cssgridmodule.doc and a copy of the information describing 
        the nature of the tests to test.asc. For the single and the double precision interpolations from
        randomly spaced data to a rectangular grid on a sphere, the numerical results are written to netCDF files
        if there is access to the module cdms2.
         
   DOCUMENTATION
  
        Without conducting the tests, documentation written to the file cssgridmodule.doc can be produced after
        importing the cssgridtest module by typing 
  
               cssgridtest.document() 
  
"""
import sys, numpy, math, random, css, cssgridmodule

writeTestcase = 'yes'
try:
    import cdms2
except ImportError:
    print 'Can not write test case results to netCDF files without module cdms2'
    writeTestcase = 'no'

def document():
    #-------------------------------------------------------------------------------
    #
    #    purpose:   'document' writes documentation for the user to a file 
    #
    #    usage:     import cssgridtest
    #               cssgridtest.document()
    #    
    #    passed :   nothing
    #
    #    returned:  nothing
    #
    #-------------------------------------------------------------------------------
    import css

    std = sys.stdout                                             # save sys.stout to allow reassigning later
    sys.stdout = open( 'cssgridmodule.doc', 'w')

    print '**********************************************************************************************\n'  
    print '*************************** Overview of the CDAT interface to cssgrid ************************\n'
    print '**********************************************************************************************\n'  
    print css.__doc__
    print
    print
    print '   HELP PACKAGE EXAMPLE \n'
    print '    ************************ Default Parameter Table **********************\n'
    print '    -----------------------------------------------------------------------------------------------------'
    css.help('table')
    print

    sys.stdout = std

    return None

def sendOutput(msg, value = None, screen = 'no'):
    #------------------------------------------------------------------------------
    #
    #    purpose: send a message and optionally a value a file and if screen is not 'no'
    #             send the same thing to the screen 
    # 
    #    usage:   sendOutput(msg, value = number, screen = 'yes')
    # 
    #    passed :  msg    - the string to write to the output media
    #              value  - a number
    #              screen - a string set to something different from 'no' if the output also 
    #                       goes to the screen
    #             
    #    returned: None
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

# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# ++++++++++++++++++++++++++++++++++ Autotest Calls ++++++++++++++++++++++++++++++++++
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
 
def runtests():
    #-----------------------------------------------------------------------------
    #                                      
    #     purpose: call test cases
    #
    #-----------------------------------------------------------------------------
    sendOutput('############################################################################################')
    sendOutput('################## Here are the results of running analytical test cases #####################')
    sendOutput('############################################################################################')

    testError = 0
    for n in range(1,7):
       err = choose(n)
       if err != 0:
           #print 'test number with error :',n,err
           testError = testError + 1
    return testError

def choose(case):
    #-------------------------------------------------------------------------------
    #                                      
    #     purpose:  check out cssgrid 
    #
    #     case 1: a simple 2D interpolation using y32 -- single precision 
    #---------------------------------------------------------------------------------
    err = 0

    if case == 1:   
        sendOutput('\n******* a simple 2D interpolation using y32 spherical harmonic -- single precision *****\n')

        # Make a random 2D grid and associated y32 data for the input fields

        nlati = 60
        nloni = 120
        latiSort, lati = randomGrid(nlati, 90.0, -90.0)             # latisort has random numbers monotonically increasing
        loniSort, loni = randomGrid(nloni, 360.0, 0.0)

        y32 = YData(loni, lati)                                     # y32(lati[i], loni[j]) format

        # Convert grids and data to the linear form where lati, loni and y32 have the same dimension with lat the fastest

        newOrder = (1,0)
        y32 = numpy.transpose(y32, newOrder)                     # new order needed for grid is (lon, lat) in C notation

        lonList, latList, y32List = c2Dto1D(loni, lati, y32)       # lati varies the fastest

        # ------------------------------------------------------------------------------

        # Make a uniform output grid

        nlato = 71
        nlono = 145
        lato = uniformGrid(nlato, -87.5, 87.5)      # start at 87.5
        lono = uniformGrid(nlono, 357.5, 0.0)       # start at 0.

        dataCheck = YData(lono, lato)                          # longitude varies the fastest
        sendOutput('*** writing exact anwser for test case to the netCDF file SingleDataCheck.nc')
        write1D_4DField('SingleDataCheck', dataCheck, lato, lono)   # lono varies the fastest. Shape is(nlati, nloni)
        # ------------------------------------------------------------------------------

        r = css.Cssgrid(latList, lonList, lato, lono)              # make instance

        dataOut = r.rgrd(y32List)                              # call regrid method

        newOrder = (1,0)                                        # transpose data to standard (lat, lon) lon the fastest
        dataOut = numpy.transpose(dataOut, newOrder)

        sendOutput('*** writing interpolated field for test case to the netCDF file SingleDataOut.nc')
        write1D_4DField('SingleDataOut', dataOut, lato, lono)      # loni varies the fastest. Shape is(nlati, nloni)

        error = rmserror(dataOut, dataCheck)                                 # find the rms error
        sendOutput('\n******* compare results\n')
        sendOutput('*** the 2D test case rms error is usually less than 0.008')
        sendOutput('*** calculated 2D test case 1D rms error = ', error)

        if error > .01:
            err = 1

        return err

    elif case == 2:   
        sendOutput('\n******* a simple 2D interpolation using y32 spherical harmonic -- double precision *****\n')

        # Make a random 2D grid and associated y32 data for the input fields

        nlati = 60
        nloni = 120
        latiSort, lati = randomGrid(nlati, 90.0, -90.0)             # latisort has random numbers monotonically increasing
        loniSort, loni = randomGrid(nloni, 360.0, 0.0)

        y32 = YData(loni, lati)                                     # y32(lati[i], loni[j]) format

        # Convert grids and data to the linear form where lati, loni and y32 have the same dimension with lat the fastest

        newOrder = (1,0)
        y32 = numpy.transpose(y32, newOrder)                     # new order needed for grid is (lon, lat) in C notation

        lonList, latList, y32List = c2Dto1D(loni, lati, y32)       # lati varies the fastest

        # ------------------------------------------------------------------------------

        # Make a uniform output grid

        nlato = 71
        nlono = 145
        lato = uniformGrid(nlato, -87.5, 87.5)      # start at 87.5
        lono = uniformGrid(nlono, 357.5, 0.0)       # start at 0.

        dataCheck = YData(lono, lato)                          # longitude varies the fastest
        sendOutput('*** writing exact anwser for test case to the netCDF file DoubleDataCheck.nc')
        write1D_4DField('DoubleDataCheck', dataCheck, lato, lono)   # lono varies the fastest. Shape is(nlati, nloni)
        # ------------------------------------------------------------------------------
        # make arrays double precision
        latList = latList.astype(numpy.float64)
        lonList = lonList.astype(numpy.float64)
        lato = lato.astype(numpy.float64)
        lono = lono.astype(numpy.float64)
        y32List = y32List.astype(numpy.float64)


        r = css.Cssgrid(latList, lonList, lato, lono)              # make instance

        dataOut = r.rgrd(y32List)                              # call regrid method

        # make arrays single precision
        lato = lato.astype(numpy.float64)
        lono = lono.astype(numpy.float64)
        dataOut = dataOut.astype(numpy.float32)

        newOrder = (1,0)                                    # transpose data to standard (lat, lon) lon the fastest
        dataOut = numpy.transpose(dataOut, newOrder)

        sendOutput('*** writing interpolated field for test case to the netCDF file DoubleDataOut.nc')
        write1D_4DField('DoubleDataOut', dataOut, lato, lono)      # loni varies the fastest. Shape is(nlati, nloni)

        error = rmserror(dataOut, dataCheck)                                 # find the rms error
        sendOutput('\n******* compare results\n')
        sendOutput('*** the 2D test case rms error is usually less than 0.008')
        sendOutput('*** calculated 2D test case 1D rms error = ', error)

        if error > .01:
            err = 1

        return err

    elif case == 3:   
        sendOutput('\n******* checking the Delauany triangulation *****\n')
        # Read a random 2D list formatted grid  and the triangles associated with this grid

        latiSort, loniSort, tri = storedListGrids()

        r = css.Cssgrid(latiSort, loniSort)  

        ntri = r.rgrd(compType = 'triangles') 

        tricheck = numpy.ravel(ntri)
        check = tri - tricheck
        if numpy.add.reduce(check) == 0:
            sendOutput('*** single precision Delauany triangulation is correct')
        else:
            err = 1

        return err

    elif case == 4:   
        # Read a random 2D list formatted grid  and the triangles associated with this grid

        latiSort, loniSort, tri = storedListGrids()

        # ------------------------------------------------------------------------------
        # make arrays double precision
        latiSort = latiSort.astype(numpy.float64)
        loniSort = loniSort.astype(numpy.float64)

        r = css.Cssgrid(latiSort, loniSort)  

        ntri = r.rgrd(compType = 'triangles') 

        ntri = ntri.astype(numpy.float32)

        tricheck = numpy.ravel(ntri)
        check = tri - tricheck
        if numpy.add.reduce(check) == 0:
            sendOutput('*** double precision Delauany triangulation is correct')
        else:
            err = 1

        return err

    elif case == 5:  
        sendOutput('\n******* checking the Voronoi polygon computation *****\n')
        # Read a random 2D list formatted grid  and the triangles associated with this grid

        latiSort, loniSort, tri = storedListGrids()

        # Read the anticipated returns from the call for polygons

        latVCheck, lonVCheck, rcCheck, nvCheck =  storedPolygons()
        # ------------------------------------------------------------------------------

        r = css.Cssgrid(latiSort, loniSort)  

        latV, lonV, rc, nv = r.rgrd(compType = 'polygons', indexVoro = 6, firstCall = 1)

        checkNumber = 0

        check = latV - latVCheck
        if abs(numpy.add.reduce(check)) < .007:
            checkNumber = checkNumber + 1

        check = lonV - lonVCheck
        if abs(numpy.add.reduce(check)) < .007:
            checkNumber = checkNumber + 1

        check = rc - rcCheck
        if abs(numpy.add.reduce(check)) < .007:
            checkNumber = checkNumber + 1

        check = nv - nvCheck
        if numpy.add.reduce(check) == 0:
            checkNumber = checkNumber + 1

        if checkNumber == 4:
            sendOutput('*** single precision computation of the Voronoi polygons is correct')
        else:
            err = 1

        return err


    elif case == 6:   
        # Read a random 2D list formatted grid  and the triangles associated with this grid

        latiSort, loniSort, tri = storedListGrids()

        # Read the anticipated returns from the call for polygons

        latVCheck, lonVCheck, rcCheck, nvCheck =  storedPolygons()
        # ------------------------------------------------------------------------------
        # make arrays double precision
        latiSort = latiSort.astype(numpy.float64)
        loniSort = loniSort.astype(numpy.float64)

        r = css.Cssgrid(latiSort, loniSort)  

        latV, lonV, rc, nv = r.rgrd(compType = 'polygons', indexVoro = 6, firstCall = 1)

        # make arrays single precision
        latV = latV.astype(numpy.float32)
        lonV = lonV.astype(numpy.float32)
        rc = rc.astype(numpy.float32)

        checkNumber = 0

        check = latV - latVCheck
        if abs(numpy.add.reduce(check)) < .007:
            checkNumber = checkNumber + 1

        check = lonV - lonVCheck
        if abs(numpy.add.reduce(check)) < .007:
            checkNumber = checkNumber + 1

        check = rc - rcCheck
        if abs(numpy.add.reduce(check)) < .007:
            checkNumber = checkNumber + 1

        check = nv - nvCheck
        if numpy.add.reduce(check) == 0:
            checkNumber = checkNumber + 1

        if checkNumber == 4:
            sendOutput('*** double precision computation of the Voronoi polygons is correct')
        else:
            err = 1

        return err

# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# ++++++++++++++++++++++++++++++++ Autotest Utilities +++++++++++++++++++++++++++++++++
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

def randomGrid(number, vmax, vmin):
    #----------------------------------------------------------------------------------------
    #                                      
    #     purpose: to construct a grid coordinate which is random but monotonically increasing
    #
    #     usage: vsort, vraw = randomGrid(number, vmax, vmin)
    #
    #     passed:  number - the size of the array
    #              vmax   - the largest possible value
    #              vmin   - the smallest possible value
    #
    #     returned: vsort - a numpy array sorted to be monotonically increasing
    #               vraw  - the same array as vsort without the sort into a monotonically
    #                       increasing values
    #
    #-----------------------------------------------------------------------------------------
    listNumbers = []                                              # generate random numbers
    vrange = vmax - vmin
    for i in range(number):
        listNumbers.append(vmin + vrange*random.random() )

    vraw = numpy.array(listNumbers, numpy.float32)            # make array of raw list of random numbers

    listNumbers.sort()                                            # make array of sorted list of random numbers
    listNumbers.reverse()                                         # make array of sorted list of random numbers
    vsort = numpy.array(listNumbers, numpy.float32)

    return vsort, vraw


def storedListGrids():
    #----------------------------------------------------------------------------------------
    #                                      
    #     purpose: to construct a grid coordinate which is random 
    #
    #     usage: lat, lon, tri = storedListGrids()
    #
    #     passed : nothing 
    #
    #     returned: lat - unevenly spaced latitude grid sorted to be montonically decreasing 
    #               lon - unevenly spaced longitude grid sorted to be montonically increasing 
    #               tri - the Delauany triangulation corresponding to this grid
    #
    #----------------------------------------------------------------------------------------
    latList = [
     -8.8095E+01, -8.6207E+01, -8.0798E+01, -7.7410E+01, -7.7048E+01, -7.0519E+01, -6.8764E+01, -6.8032E+01,
     -6.5928E+01, -5.3831E+01, -4.9518E+01, -3.8735E+01, -3.2771E+01, -3.0599E+01, -2.9237E+01, -2.4956E+01,
     -2.1248E+01, -2.0653E+01, -1.2058E+01, -1.2047E+01, -9.5890E+00, -3.0644E+00,  5.3169E-01,  3.4947E+00,
      9.0233E+00,  1.1813E+01,  1.5011E+01,  1.6174E+01,  1.7403E+01,  1.9631E+01,  3.6576E+01,  4.9099E+01,
      5.9614E+01,  6.5706E+01,  7.0717E+01,  7.1773E+01,  7.2613E+01,  7.8654E+01,  8.4144E+01,  8.4721E+01]

    latList.reverse()

    lonList = [
      2.6791E+00,  5.9523E+00,  3.1383E+01,  3.6397E+01,  6.3481E+01,  6.6528E+01,  7.5814E+01,  8.9036E+01,
      1.0822E+02,  1.2677E+02,  1.4488E+02,  1.4742E+02,  1.4756E+02,  1.5059E+02,  1.5762E+02,  1.6374E+02,
      1.6601E+02,  1.6945E+02,  1.8289E+02,  1.9781E+02,  2.0467E+02,  2.0961E+02,  2.1030E+02,  2.1110E+02,
      2.1491E+02,  2.1828E+02,  2.3473E+02,  2.5195E+02,  2.5752E+02,  2.7220E+02,  2.7235E+02,  2.7327E+02,
      2.7884E+02,  3.0400E+02,  3.1299E+02,  3.2419E+02,  3.2468E+02,  3.2833E+02,  3.5308E+02,  3.5781E+02]

    ntriList = [ 0, 27, 1, 0, 1, 2, 0, 2, 4, 0, 4, 5, 0, 5, 7, 0, 7, 8, 0, 8, 9, 0, 9, 15, 0, 15, 18, 
                  0, 18, 19, 0, 19, 21, 0, 21, 27, 1, 27, 28, 1, 28, 29, 1, 29, 3, 1, 3, 2, 2, 3, 4, 3, 33, 34, 
                  3, 34, 35, 3, 35, 6, 3, 6, 5, 3, 5, 4, 3, 29, 33, 5, 6, 7, 6, 35, 36, 6, 36, 7, 7, 36, 38, 
                  7, 38, 8, 8, 38, 9, 9, 38, 39, 9, 39, 10, 9, 10, 14, 9, 14, 15, 10, 39, 12, 10, 12, 11, 10, 11, 14, 
                  11, 12, 13, 11, 13, 14, 12, 39, 16, 12, 16, 13, 13, 16, 14, 14, 16, 15, 15, 16, 17, 15, 17, 18, 
                  16, 39, 17, 17, 39, 18, 18, 39, 25, 18, 25, 22, 18, 22, 19, 19, 22, 20, 19, 20, 21, 20, 22, 21, 
                  21, 22, 23, 21, 23, 24, 21, 24, 26, 21, 26, 27, 22, 25, 24, 22, 24, 23, 24, 25, 26, 25, 39, 31, 
                  25, 31, 26, 26, 31, 28, 26, 28, 27, 28, 31, 30, 28, 30, 29, 29, 30, 33, 30, 31, 32, 30, 32, 33, 
                  31, 39, 32, 32, 39, 37, 32, 37, 36, 32, 36, 35, 32, 35, 33, 33, 35, 34, 36, 37, 38, 37, 39, 38] 

    lat = numpy.array((latList), numpy.float32)
    lon = numpy.array((lonList), numpy.float32)
    tri = numpy.array((ntriList))

    return lat, lon, tri

def storedPolygons():
    #----------------------------------------------------------------------------------------
    #                                      
    #     purpose: to return the answer to the request for polygons for the specific grid obtained
    #              from the routine storedListGrids
    #
    #     usage: latV, lonV, rc, nv = storedPolygons()
    #
    #     passed : nothing 
    #
    #     returned: latV - an array with the latitudes of the Voronoi indices 
    #               lonV - an array with the longitudes of the Voronoi indices 
    #               rc   -  an array containing arc lengths of the sizes of the circles
    #               nv   -  an array with indices for the Voronoi polygon
    #
    #----------------------------------------------------------------------------------------
    latVList = [
      2.9219E+01,  1.6655E+01,  8.1120E+01,  8.0311E+01,  7.5356E+01,  6.9470E+01,  6.6253E+01,  5.9799E+01,
      4.9657E+01,  4.3222E+01,  4.1152E+01,  4.1004E+01,  1.5902E+01,  1.4720E+01,  1.1056E+01,  4.8387E+01,
      7.5854E+01,  5.0247E+00,  4.5976E+00, -4.8670E+00, -6.2166E+00,  5.6012E+01,  6.3147E+01,  7.0926E+01,
     -7.3762E+00, -7.9309E+00, -9.2611E+00, -1.1967E+01, -1.7137E+01, -1.7960E+01, -2.1847E+01,  4.8924E+01,
      4.9507E+01, -2.3414E+01,  1.6410E+01,  3.6262E+01,  1.7004E+01,  3.2561E+01, -3.4623E+01, -2.1443E+01,
     -1.1544E+01,  3.1343E+00,  1.0162E+01,  1.3426E+01, -3.6605E+01, -4.5326E+01, -4.5664E+01, -4.5365E+01,
     -3.9528E+01, -2.2280E+01,  5.6469E+00, -1.6545E+01, -1.5942E+01, -1.4586E+01, -1.0902E+01,  1.5632E+01,
     -3.9450E+01, -3.4969E+01, -1.9433E+01, -5.9195E+01, -5.5309E+01, -5.4048E+01, -4.2576E+01, -5.4274E+01,
     -5.0408E+01, -4.5893E+01, -5.8309E+01, -5.7662E+01, -7.4223E+01, -7.8573E+01, -7.7273E+01, -7.6749E+01,
     -7.5711E+01, -7.5335E+01, -2.1790E+01, -8.3217E+01]

    lonVList = [
     -1.0208E+02, -5.5274E+01,  9.0156E+01,  9.6028E+01,  1.3123E+02,  1.4507E+02,  1.5291E+02,  1.6867E+02,
     -1.7141E+02, -1.5184E+02, -1.4435E+02, -1.4351E+02, -5.3634E+01, -5.0637E+01, -3.7740E+01, -2.7712E+01,
      5.1629E+01, -2.4801E+01, -2.3006E+01,  2.0751E+01,  3.1212E+01,  4.6281E+01,  4.8055E+01,  1.2907E+02,
      3.5772E+01,  3.8059E+01,  4.2286E+01,  5.6068E+01,  7.5310E+01,  7.7417E+01,  8.9451E+01, -1.7439E+02,
     -1.7194E+02,  9.2545E+01,  1.4413E+02,  1.6791E+02,  1.4967E+02,  1.6472E+02,  1.2165E+02,  1.3434E+02,
      1.4262E+02,  1.5732E+02,  1.7488E+02, -1.7982E+02,  1.2622E+02,  1.6076E+02,  1.6252E+02,  1.6322E+02,
      1.7190E+02, -1.6328E+02, -1.5282E+02, -1.5287E+02, -1.4520E+02, -1.3798E+02, -1.2986E+02, -1.1022E+02,
     -1.7166E+02, -1.6342E+02, -1.3246E+02, -1.6049E+02, -1.3767E+02, -1.2899E+02, -1.1890E+02, -1.2601E+02,
     -1.0647E+02, -5.3905E+01, -6.9560E+01, -6.7093E+01, -1.3254E+02, -1.0130E+02, -7.8597E+01, -7.5661E+01,
     -7.1041E+01, -6.5195E+01,  4.0465E+01, -7.7111E+01]

    rcList = [
      6.2249E+01,  7.0599E+01,  1.0117E+01,  1.1290E+01,  1.8392E+01,  2.4912E+01,  2.8444E+01,  3.5344E+01,
      4.5597E+01,  5.1581E+01,  5.3335E+01,  5.3442E+01,  7.1201E+01,  7.2115E+01,  7.4741E+01,  3.6856E+01,
      5.2310E+00,  7.6880E+01,  7.6814E+01,  7.8127E+01,  7.8901E+01,  1.7091E+01,  1.0392E+01,  1.9637E+01,
      7.8735E+01,  7.8673E+01,  7.8274E+01,  7.6333E+01,  7.2388E+01,  7.1707E+01,  6.8220E+01,  4.3759E+01,
      4.5275E+01,  6.6757E+01,  3.2983E+00,  2.6151E+01,  2.1820E+00,  2.1749E+01,  5.6452E+01,  3.9787E+01,
      2.7705E+01,  8.6838E+00,  1.1039E+01,  1.6709E+01,  5.4593E+01,  4.6498E+01,  4.6176E+01,  4.5678E+01,
      3.7805E+01,  1.2734E+01,  1.7869E+01,  5.0896E+00,  6.3620E+00,  1.2332E+01,  2.0152E+01,  4.8447E+01,
      2.6586E+01,  1.8728E+01,  1.2920E+01,  3.2582E+01,  2.6233E+01,  2.3602E+01,  1.3012E+01,  2.2247E+01,
      1.2009E+01,  2.2896E+01,  1.1002E+01,  1.2132E+01,  1.7070E+01,  1.1876E+01,  9.2707E+00,  8.8654E+00,
      8.2723E+00,  7.1367E+00,  6.5671E+01,  6.5495E+00]

    nvList = [ 20, 24, 25, 23, 21, 20] 


    latV = numpy.array((latVList), numpy.float32)
    lonV = numpy.array((lonVList), numpy.float32)
    rc = numpy.array((rcList), numpy.float32)
    nv = numpy.array((nvList))

    return latV, lonV, rc, nv
 
def uniformGrid(number, vend, vstart):
    #----------------------------------------------------------------------------
    #                                      
    #     purpose: to construct a grid coordinate which is uniform
    #
    #     usage:  v = uniformGrid(number, vend, vstart)
    #
    #     passed:  number - the size of the array
    #              vend   - the last value
    #              vstart - the first value
    #
    #     returned: v - a float32 numpy array with values from vstart to v end
    #
    #-----------------------------------------------------------------------------
    v = numpy.zeros((number,), numpy.float32)

    vinc = (vend - vstart)/(number - 1)
    for n in range(number):
        v[n] = vstart + n*vinc
    return v

def c2Dto1D(x, y, z):
    #---------------------------------------------------------------------------------------------------
    #                                      
    #     purpose: to construct 1D zn[n] from 2D z[i,j] format
    #
    #     usage: xn, yn, zn  = c2Dto1D(x, y, z)
    #
    #     passed: x - the array which describes the rectangular grid associated with the first z index 
    #             y - the array which describes the rectangular grid associated with the second z index 
    #             z - the 2D data associated with the x, y grid
    #
    #     returned: xn  - a list form of the x array
    #               yn  - a list form of the y array
    #               zn  - a list form of the data array (this array has the same length as xn and yn
    #
    #---------------------------------------------------------------------------------------------------
 
    numberx = len(x)
    numbery = len(y)
    size =numberx*numbery
    xn = numpy.zeros(size, numpy.float32)
    yn = numpy.zeros(size, numpy.float32)

    for i in range(numberx):
        for j in range(numbery):
            n = j + i*numbery
            xn[n] = x[i]
            yn[n] = y[j]

    zn = numpy.ravel(z)

    return (xn, yn, zn)

def write1D_4DField(varname, dataField, x, y = None, z = None, t = None): 

    #------------------------------------------------------------------------------
    #                                      
    #     purpose: write an output field which may be 1D, 2D, 3D or 4D to a NetCDF file
    #
    #     usage:  write1D_4DField(varname, dataField, x, y, z = None, t = None) for a 2D write 
    #
    #     passed :  varname   - name of the variable and the file id
    #               x,y,z,t   - dimension vectors
    #               dataField - the data
    #
    #     returned: None 
    #
    #-------------------------------------------------------------------------------
    import cdms2

    fileObj = cdms2.createDataset(varname + '.nc')  

    # construct the axis tuple

    x = x.astype(numpy.float64)
    x_axis = fileObj.createAxis('x', x)  
    axisList = [x_axis]

    if y is not None:
        y = y.astype(numpy.float64)
        y_axis = fileObj.createAxis('y', y)  
        axisList.append(y_axis)

    if z is not None:
        z = z.astype(numpy.float64)
        z_axis = fileObj.createAxis('z', z)  
        axisList.append(z_axis)

    if t is not None:
        t = t.astype(numpy.float64)
        t_axis = fileObj.createAxis('t', t)  
        axisList.append(t_axis)
  
    if len(axisList) == 1:
        axisTuple = (x_axis,)
    else:
        axisTuple = tuple(axisList)

    # write the data to the file

    var = fileObj.createVariable(varname, numpy.float32, axisTuple)                    # variable without data

    var[:] = dataField                                                                   # copy in the data

    fileObj.close() 

    return None

#-----------------------------------------------------------------

def YData(lonvals, latvals, data_name = 'Y32'):
    #----------------------------------------------------------------------------
    #                                      
    #     purpose: construct Y33, Y32, Y31 or Y30 data 
    #
    #     usage:   data = YData(lonvals, latvals, data_name = 'Y32'):
    #
    #     passed :   lonvals -- longitude vactor 
    #                latvals -- latitude vactor 
    #
    #     returned:  data 
    #-----------------------------------------------------------------------------

    if data_name[:3] == 'Y33':
        data = Y33(lonvals, latvals)
    elif data_name[:3]  == 'Y32':
        data = Y32(lonvals, latvals)
    elif data_name[:3]  == 'Y31':
        data = Y31(lonvals, latvals)
    elif data_name[:3]  == 'Y30':
        data = Y30(lonvals, latvals)
    else:
        msg = 'Must choose Y33, Y32, Y31 or Y30'
        raise ValueError, msg
        return

    return data

def Y33(lonvals, latvals):
    #------------------------------------------------------------------------------
    #                                      
    #     purpose: construct Y33 data 
    #
    #     usage:   y33 = Y33(lonvals, latvals)
    #
    #     passed :   lonvals -- longitude vactor 
    #                latvals -- latitude vactor 
    #
    #     returned:  data 
    #------------------------------------------------------------------------------

    nlon = len(lonvals)
    nlat = len(latvals)
    phi = (math.pi/180.)*lonvals  
    theta = (math.pi/180.)*latvals
  
    y33 = numpy.zeros( (nlat,nlon), numpy.float32)     # memory

    fac = -(1./4.)*math.sqrt( (35./(4.*math.pi)) )
    fac = 1.0

    for i in range(nlon):
        for j in range(nlat):
            y33[j,i] = fac*(math.sin(theta[j])**3)*math.cos(3.*phi[i])

    return  y33


def Y32(lonvals, latvals):
    #-------------------------------------------------------------------------------
    #                                      
    #     purpose: construct Y32 data 
    #
    #     usage:   y32 = Y32(lonvals, latvals)
    #
    #     passed :   lonvals -- longitude vactor 
    #                latvals -- latitude vactor 
    #
    #     returned:  data 
    #-------------------------------------------------------------------------------

    nlon = len(lonvals)
    nlat = len(latvals)
    phi = (math.pi/180.)*lonvals  
    theta = (math.pi/180.)*latvals
  
    y32 = numpy.zeros( (nlat,nlon), numpy.float32)     # memory

    fac = (1./4.)*math.sqrt( (105./(4.*math.pi)) )
    fac = 1.0

    for i in range(nlon):
        for j in range(nlat):
            y32[j,i] = fac*(math.sin(theta[j])**2)*math.cos(theta[j])*math.cos(2.*phi[i])

    return  y32


def Y31(lonvals, latvals):
    #--------------------------------------------------------------------------------
    #                                      
    #     purpose: construct Y31 data 
    #
    #     usage:   y31 = Y31(lonvals, latvals)
    #
    #     passed :   lonvals -- longitude vactor 
    #                latvals -- latitude vactor 
    #
    #     returned:  data 
    #--------------------------------------------------------------------------------

    nlon = len(lonvals)
    nlat = len(latvals)
    phi = (math.pi/180.)*lonvals  
    theta = (math.pi/180.)*latvals
  
    y31 = numpy.zeros( (nlat,nlon), numpy.float32)     # memory

    fac = -(1./4.)*math.sqrt( (21./(4.*math.pi)) )
    fac = 1.0

    for i in range(nlon):
        for j in range(nlat):
            y31[j,i] = fac*math.sin(theta[j])*(5.*math.cos(theta[j])**2 - 1.)*math.cos(phi[i])

    return  y31


def Y30(lonvals, latvals):
    #----------------------------------------------------------------------------------
    #                                      
    #     purpose: construct Y30 data 
    #
    #     usage:   y30 = Y30(lonvals, latvals)
    #
    #     passed :   lonvals -- longitude vactor 
    #                latvals -- latitude vactor 
    #
    #     returned:  data 
    #-----------------------------------------------------------------------------------

    nlon = len(lonvals)
    nlat = len(latvals)
    phi = (math.pi/180.)*lonvals  
    theta = (math.pi/180.)*latvals

    lonvals = makelon(nlon)
    phi = lonvals  
    phi = (math.pi/180.)*lonvals  

    latvals, colatvals = makelat(nlat, grid_type)
    latvals, colatvals = makelat(nlat)
    theta = (math.pi/180.)*colatvals
  
    y30 = numpy.zeros( (nlat,nlon), numpy.float32)     # memory

    fac = math.sqrt( (7./(4.*math.pi)) )
    fac = 1.0

    for i in range(nlon):
        for j in range(nlat):
            y30[j,i] = fac*( (5./2.)*math.cos(theta[j])**3 - (3./2.)*math.cos(theta[j]) )

    return  y30

#-----------------------------------------------------------------

def rmserror(data1, data2):
    #---------------------------------------------------------------------------------
    #
    #    purpose: compute the rms error for two data sets having the same shape
    # 
    #    passed : the two data sets 
    #             
    #    returned: rms error
    #
    #---------------------------------------------------------------------------------

    if data1.shape != data2.shape:
        print 'Error in shape in rmserror'
        print 'data1 shape = ', data1.shape
        print 'data2 shape = ', data2.shape
        raise ValueError

    d1 = numpy.ravel(data1)
    d2 = numpy.ravel(data2)

    sq = (d1 - d2)*(d1 - d2)
    error = numpy.sum(sq)/len(d1)
    rmserror =  numpy.sqrt(error)

    return  rmserror


if __name__ == "__main__":
    output = open('test.asc', 'w')               # global file name

    print 'Running the test computations'
    testError = runtests() 
    write = document()

    sendOutput(' ')
    sendOutput('*********')
    sendOutput('General information on the use of CSSGRID has been written to the file cssgridmodule.doc.')
    sendOutput('*********')
    sendOutput(' ')

    if testError == 0:
        print 'Testing Completed Successfully'
    else:
        print 'Testing completed but it may have problems. The test generates a randomly spaced grid which may be a difficult one.'
        print 'Look at the results or run the test again in order to use a different randomly generated grid.'

    print 'Some details on the testing have been written to the file test.asc.'
    print 'General information on the use of CSSGRID has been written to the file cssgridmodule.doc.'

    output.close()
