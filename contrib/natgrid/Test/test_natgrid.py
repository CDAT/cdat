# Adapted for numpy/ma/cdms2 by convertcdms.py

"""Documentation for module natgridtest: an automatic test for natgrid, an interface to the ngmath NATGRID
  
   TESTING 
 
        Typing 
         
            cdat natgridtest.py
         
        generates some testing of the natgridmodule using analytical functions as fields. It also writes a 
        hard copy of the documentation to the file natgridmodule.doc and a copy of the information describing 
        the nature of the tests to test.asc. For the single and the double precision interpolations from
        randomly spaced data to a rectangular grid on a sphere, the numerical results are written to netCDF files
        if there is access to the module cdms.
         
   DOCUMENTATION
  
        Without conducting the tests, documentation written to the file natgridmodule.doc can be produced after
        importing the natgridtest module by typing 
  
               natgridtest.document() 
  
"""
import sys, numpy, math, random, nat, natgridmodule

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
    #    usage:     import natgridtest
    #               natgridtest.document()
    #    
    #    passed :   nothing
    #
    #    returned:  nothing
    #
    #-------------------------------------------------------------------------------
    import nat

    std = sys.stdout                                             # save sys.stout to allow reassigning later
    sys.stdout = open( 'natgridmodule.doc', 'w')

    print '**********************************************************************************************\n'  
    print '*************************** Overview of the CDAT interface to natgrid ************************\n'
    print '**********************************************************************************************\n'  
    print nat.__doc__
    print
    print
    print '   HELP PACKAGE EXAMPLE \n'
    print '    ************************ Default Parameter Table **********************\n'
    print '    -----------------------------------------------------------------------------------------------------'
    nat.help('table')
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
    for n in range(1,8):
       err = choose(n)
       if err != 0:
           #print 'test number with error :',n,err
           testError = testError + 1
    return testError

def choose(case):
    #-------------------------------------------------------------------------------
    #                                      
    #     purpose:  check out natgrid 
    #
    #     case 1: a simple 2D interpolation using y32 -- single precision 
    #---------------------------------------------------------------------------------
    err = 0

    if case == 1:   
        sendOutput('\n******* natural neighbor linear interpolation -- single precision *****\n')

        # array dimensions
        ni = 6                                  # size of xi, yi, and dataIn
        nxo = 21
        nyo = 21

        # input arrays and data 

        xiList = [0.00, 1.00, 0.00, 1.00,  0.40, 0.75]
        yiList = [0.00, 0.00, 1.00, 1.00,  0.20, 0.65]
        dataInList = [0.00, 0.00, 0.00, 0.00, 1.25, 0.80]

        xi = numpy.array(xiList, numpy.float32)
        yi = numpy.array(yiList, numpy.float32)
        dataIn = numpy.array(dataInList, numpy.float32)

        # output array 

        xo = uniformGrid(nxo, 1.0, 0.0)
        yo = uniformGrid(nyo, 1.0, 0.0)

        r = nat.Natgrid(xi, yi, xo, yo)  

        dataOut = r.rgrd(dataIn) 

        sendOutput('*** writing single precision linear interpolation test case to the netCDF file SingleLinearRegrid.nc')
        write1D_4DField('SingleLinearRegrid', dataOut, xo, yo) 

        dataCheck = storedAnswers('linearRegrid')
        dataCheck = numpy.reshape(dataCheck, (nxo,nyo))

        error = rmserror(dataOut, dataCheck)                                 # find the rms error
        sendOutput('\n******* compare results\n')
        sendOutput('*** the linear interpolation test case rms error is usually less than 1.e-05')
        sendOutput('*** the linear interpolation test case rms error = ', error)

        if error > .0001:
            err = 1

        return err

    elif case == 2:   
        sendOutput('\n******* natural neighbor linear interpolation -- double precision *****\n')

        # array dimensions
        ni = 6                                  # size 0f xi, yi, and dataIn
        nxo = 21
        nyo = 21

        # input arrays and data 

        xiList = [0.00, 1.00, 0.00, 1.00,  0.40, 0.75]
        yiList = [0.00, 0.00, 1.00, 1.00,  0.20, 0.65]
        dataInList = [0.00, 0.00, 0.00, 0.00, 1.25, 0.80]

        xi = numpy.array(xiList, numpy.float64)
        yi = numpy.array(yiList, numpy.float64)
        dataIn = numpy.array(dataInList, numpy.float64)

        # output array 

        xo = uniformGrid(nxo, 1.0, 0.0)
        yo = uniformGrid(nyo, 1.0, 0.0)

        xo = xo.astype(numpy.float64)
        yo = yo.astype(numpy.float64)

        r = nat.Natgrid(xi, yi, xo, yo)  

        dataOut = r.rgrd(dataIn) 

        xo = xo.astype(numpy.float32)                            # convert back to single precision
        yo = yo.astype(numpy.float32)
        dataOut = dataOut.astype(numpy.float32)

        sendOutput('*** writing double precision linear interpolation test case to the netCDF file DoubleLinearRegrid.nc')
        write1D_4DField('DoubleLinearRegrid', dataOut, xo, yo) 

        dataCheck = storedAnswers('linearRegrid')
        dataCheck = numpy.reshape(dataCheck, (nxo,nyo))

        error = rmserror(dataOut, dataCheck)                                 # find the rms error
        sendOutput('\n******* compare results\n')
        sendOutput('*** the linear interpolation test case rms error is usually less than 1.e-05')
        sendOutput('*** the linear interpolation test case rms error = ', error)

        if error > .0001:
            err = 1

        return err

    elif case == 3:   
        sendOutput('\n******* natural neighbor nonlinear interpolation -- single precision *****\n')

        # array dimensions
        ni = 6                                  # size of xi, yi, and dataIn
        nxo = 21
        nyo = 21

        # input arrays and data 

        xiList = [0.00, 1.00, 0.00, 1.00,  0.40, 0.75]
        yiList = [0.00, 0.00, 1.00, 1.00,  0.20, 0.65]
        dataInList = [0.00, 0.00, 0.00, 0.00, 1.25, 0.80]

        xi = numpy.array(xiList, numpy.float32)
        yi = numpy.array(yiList, numpy.float32)
        dataIn = numpy.array(dataInList, numpy.float32)

        # output array 

        xo = uniformGrid(nxo, 1.0, 0.0)
        yo = uniformGrid(nyo, 1.0, 0.0)

        r = nat.Natgrid(xi, yi, xo, yo)  
        r.igr = 1                                       # choose nonlinear interpolation

        dataOut = r.rgrd(dataIn) 

        sendOutput('*** writing single precision nonlinear interpolation test case to the netCDF file SingleNonlinearRegrid.nc')
        write1D_4DField('SingleNonlinearRegrid', dataOut, xo, yo) 

        dataCheck = storedAnswers('nonlinearRegrid')
        dataCheck = numpy.reshape(dataCheck, (nxo,nyo))

        error = rmserror(dataOut, dataCheck)                                 # find the rms error
        sendOutput('\n******* compare results\n')
        sendOutput('*** the nonlinear interpolation test case rms error is usually less than 1.e-05')
        sendOutput('*** the nonlinear interpolation test case rms error = ', error)

        if error > .0001:
            err = 1

        return err

    elif case == 4:   
        sendOutput('\n******* natural neighbor nonlinear interpolation -- double precision *****\n')

        # array dimensions
        ni = 6                                  # size 0f xi, yi, and dataIn
        nxo = 21
        nyo = 21

        # input arrays and data 

        xiList = [0.00, 1.00, 0.00, 1.00,  0.40, 0.75]
        yiList = [0.00, 0.00, 1.00, 1.00,  0.20, 0.65]
        dataInList = [0.00, 0.00, 0.00, 0.00, 1.25, 0.80]

        xi = numpy.array(xiList, numpy.float64)
        yi = numpy.array(yiList, numpy.float64)
        dataIn = numpy.array(dataInList, numpy.float64)

        # output array 

        xo = uniformGrid(nxo, 1.0, 0.0)
        yo = uniformGrid(nyo, 1.0, 0.0)

        xo = xo.astype(numpy.float64)
        yo = yo.astype(numpy.float64)

        r = nat.Natgrid(xi, yi, xo, yo)  
        r.igr = 1                                       # choose nonlinear interpolation

        dataOut = r.rgrd(dataIn) 

        xo = xo.astype(numpy.float32)                            # convert back to single precision
        yo = yo.astype(numpy.float32)
        dataOut = dataOut.astype(numpy.float32)

        sendOutput('*** writing double precision nonlinear interpolation test case to the netCDF file DoubleNonlinearRegrid.nc')
        write1D_4DField('DoubleNonlinearRegrid', dataOut, xo, yo) 

        dataCheck = storedAnswers('nonlinearRegrid')
        dataCheck = numpy.reshape(dataCheck, (nxo,nyo))

        error = rmserror(dataOut, dataCheck)                                 # find the rms error
        sendOutput('\n******* compare results\n')
        sendOutput('*** the nonlinear interpolation test case rms error is usually less than 1.e-05')
        sendOutput('*** the nonlinear interpolation test case rms error = ', error)

        if error > .0001:
            err = 1

        return err


    elif case == 5:   
        sendOutput('\n******* interpolation and computation of aspects and slopes -- single precision *******\n')

        # array dimensions
        ni = 800                                  # size of xi, yi, and dataIn
        nxo = 21
        nyo = 21

        # input array and data 

        xisort, xi = randomGrid(ni, 1.2, -0.2)                            # xisort has random numbers monotonically increasing
        yisort, yi = randomGrid(ni, 1.2, -0.2)

        dataIn = numpy.zeros((ni,), numpy.float32)
        for i in range(ni):
            dataIn[i] = (xi[i] - 0.25)*(xi[i] - 0.25) + (yi[i] - 0.50)*(yi[i] - 0.50)

        # output array 

        xo = uniformGrid(nxo, 1.0, 0.0)
        yo = uniformGrid(nyo, 1.0, 0.0)

        r = nat.Natgrid(xi, yi, xo, yo)  

        dataOut, aspect, slope = r.rgrd(dataIn, aspectSlope = 'yes') 


        sendOutput('*** writing single precision linear interpolation test case to the netCDF file AspectSlopeRegrid.nc')
        write1D_4DField('AspectSlopeRegrid', dataOut, xo, yo) 

        # Calculate the exact answer
        dataCheck = numpy.zeros((nxo, nyo), numpy.float32)
        for i in range(nxo):
            for j in range(nyo):
                dataCheck[i,j] = (xo[i] - 0.25)*(xo[i] - 0.25) + (yo[j] - 0.50)*(yo[j] - 0.50)

        sendOutput('*** writing exact answer to single precision interpolation test case to the netCDF file AspectSlopeExact.nc')
        write1D_4DField('AspectSlopeExact', dataOut, xo, yo) 

        error = rmserror(dataOut, dataCheck)                                 # find the rms error
        sendOutput('\n******* compare results\n')
        sendOutput('*** the linear interpolation test case rms error is usually about 1.e-03')
        sendOutput('*** the linear interpolation test case rms error = ', error)

        # Calculate the x and y aspects

        u = numpy.zeros((nxo, nyo), numpy.float32)
        v = numpy.zeros((nxo, nyo), numpy.float32)
        for i in range(nxo):
            for j in range(nyo):
                uvtemp = (math.pi/180.)*aspect[i,j]
                u[i,j] = math.cos(uvtemp)
                v[i,j] = math.sin(uvtemp)

        sendOutput('*** writing the cosine of the aspect to xaspect.nc')
        sendOutput('*** writing the sine of the aspect to yaspect.nc')
        write1D_4DField('xaspect', u, xo, yo) 
        write1D_4DField('yaspect', v, xo, yo) 

        if error > .01:
            err = 1

        return err

    elif case == 6:   
        sendOutput('\n******* single point mode -- single precision *****\n')

        # array dimensions
        ni = 171                                  # size of xi, yi, and dataIn
        nxo = 21
        nyo = 21

        # input arrays and data 

        xisort, xi = randomGrid(ni, 1.2, -0.2)                 # xisort has random numbers monotonically increasing
        yisort, yi = randomGrid(ni, 1.2, -0.2)

        dataIn = numpy.zeros((ni,), numpy.float32)
        for i in range(ni):
            dataIn[i] = (xi[i] - 0.25)*(xi[i] - 0.25) + (yi[i] - 0.50)*(yi[i] - 0.50)

        # output array 
        xo = uniformGrid(nxo, 1.0, 0.0)
        yo = uniformGrid(nyo, 1.0, 0.0)
        xn, yn = grid2Dto1D(xo, yo)

        r = nat.Natgrid(xi, yi, xn, yn, listOutput = 'yes')  
        r.igr = 1                                       # choose nonlinear interpolation

        zn = r.rgrd(dataIn) 
        xo, yo, dataOut = c1Dto2D(nxo, nyo, xn, yn, zn)

        sendOutput('*** writing single precision single point mode test case to the netCDF file SinglePointMode.nc')
        write1D_4DField('SinglePointMode', dataOut, xo, yo) 

        dataCheck = numpy.zeros((nxo,nyo), numpy.float32)
        for i in range(nxo):
            for j in range(nyo):
                dataCheck[i,j] = (xo[i] - 0.25)*(xo[i] - 0.25) + (yo[j] - 0.50)*(yo[j] - 0.50)

        sendOutput('*** writing exact answer to single precision single point mode test case to the netCDF file SinglePointExact.nc')
        write1D_4DField('SinglePointExact', dataOut, xo, yo) 

        error = rmserror(dataOut, dataCheck)                                 # find the rms error
        sendOutput('\n******* compare results\n')
        sendOutput('*** the nonlinear single point mode test case rms error is usually less than 1.e-02')
        sendOutput('*** the nonlinear single point test case rms error = ', error)

        if error > .01:
            err = 1

        return err


    elif case == 7:   
        sendOutput('\n******* nonlinear interpolation of y32 with a wrap -- single precision *****\n')

        # input arrays and data 

        lati,latiSort,loni,loniSort  = storedGrids()

        y32 = YData(loni, lati)                                      # y32(lati[i], loni[j]) format

        newOrder = (1,0)
        y32 = numpy.transpose(y32, newOrder)
        lonLinear, latLinear, y32Linear = c2Dto1D(loni, lati, y32)           # change to the linear list format 


        # output array 

        nlato = 71
        nlono = 144
        lato = uniformGrid(nlato, 87.5, -87.5)              # start at - 87.5
        lono = uniformGrid(nlono, 357.5, 0.0)               # start at 0.


        r = nat.Natgrid(latLinear, lonLinear, lato, lono)  
        #r.igr = 1                                                        # choose nonlinear interpolation

        dataOut = r.rgrd(y32Linear, wrap = 'yes') 

        dataCheck = YData(lono, lato)                                      # longitude varies the fastest
        sendOutput('*** writing exact answer to single precision y32 interpolatiion test case to the netCDF file y32Exact.nc')
        write1D_4DField('y32Exact', dataCheck, lato, lono)               # lono varies the fastest. Shape is(nlati, nloni)

        sendOutput('*** writing single precision y32 interpolation test case to the netCDF file y32Regrid.nc')
        write1D_4DField('y32Regrid', dataOut, lato, lono) 

        error = rmserror(dataOut, dataCheck)                               # find the rms error
        sendOutput('\n******* compare results\n')
        sendOutput('*** the nonlinear interpolation test case rms error is usually less than 1.e-02')
        sendOutput('*** the nonlinear interpolation test case rms error = ', error)

        if error > .01:
            err = 1

        return err



        dataCheck = YData(lono, lato)                       # longitude varies the fastest
        write1D_4DField('data_Check', dataCheck, lato, lono)        # lono varies the fastest. Shape is(nlati, nloni)

        # ------------------------------------------------------------------------------
        # Call the interpolator

        print 'making instance for case 8'

        r = nat.Natgrid(latLinear, lonLinear, lato, lono)  

        print 'call rgrd method for case 8'

        dataOut = r.rgrd(y32Linear, wrap = 'yes') 

        print 'returning from call rgrd method for case 8'

        write1D_4DField('wrapdata_Out', dataOut, lato, lono)           # loni varies the fastest. Shape is(nlati, nloni)

        print 'dataOut and dataCheck shapes before call to rmserror', dataOut.shape, dataCheck.shape
        error = rmserror(dataOut, dataCheck)                                 # find the rms error
        print 'case 1 rms error = ', error

        return None

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

def storedAnswers(choice):
    #----------------------------------------------------------------------------------------
    #                                      
    #     purpose: to store the answers to selected test cases
    #
    #     usage: data = storedAnswers(choice)
    #
    #     passed : choice -- a string idetifying the desired data 
    #
    #     returned:  data
    #
    #----------------------------------------------------------------------------------------

    if choice == 'linearRegrid':
        linearRegridList = [
         8.4993E-07,  3.7050E-03,  6.9907E-03,  9.8621E-03,  1.2324E-02,  1.4383E-02,  1.6037E-02,  1.7102E-02,
         1.7583E-02,  1.7586E-02,  1.7189E-02,  1.6454E-02,  1.5428E-02,  1.4147E-02,  1.2642E-02,  1.0937E-02,
         9.0519E-03,  7.0031E-03,  4.8043E-03,  2.4671E-03,  4.3879E-06,  7.2811E-03,  1.5625E-01,  1.5625E-01,
         1.5625E-01,  1.5625E-01,  1.5625E-01,  1.5476E-01,  1.5088E-01,  1.4598E-01,  1.4069E-01,  1.3534E-01,
         1.3005E-01,  1.2487E-01,  1.1980E-01,  1.1476E-01,  1.0963E-01,  1.0414E-01,  9.7756E-02,  8.8053E-02,
         6.6249E-02,  8.4452E-04,  1.3750E-02,  2.6209E-01,  3.1250E-01,  3.1250E-01,  3.1250E-01,  3.1158E-01,
         3.0454E-01,  2.9412E-01,  2.8246E-01,  2.7050E-01,  2.5865E-01,  2.4703E-01,  2.3563E-01,  2.2431E-01,
         2.1283E-01,  2.0076E-01,  1.8705E-01,  1.6770E-01,  1.3780E-01,  8.7170E-02,  1.6137E-03,  1.9411E-02,
         2.9894E-01,  4.5966E-01,  4.6875E-01,  4.6875E-01,  4.6336E-01,  4.4829E-01,  4.2966E-01,  4.1005E-01,
         3.9052E-01,  3.7142E-01,  3.5281E-01,  3.3451E-01,  3.1623E-01,  2.9748E-01,  2.7694E-01,  2.5040E-01,
         2.1500E-01,  1.6605E-01,  9.5877E-02,  2.3095E-03,  2.4271E-02,  3.1159E-01,  5.5232E-01,  6.2500E-01,
         6.2490E-01,  6.1118E-01,  5.8621E-01,  5.5806E-01,  5.2959E-01,  5.0180E-01,  4.7493E-01,  4.4884E-01,
         4.2320E-01,  3.9749E-01,  3.7065E-01,  3.3827E-01,  2.9815E-01,  2.4785E-01,  1.8341E-01,  1.0017E-01,
         2.9305E-03,  2.8341E-02,  3.1249E-01,  6.0503E-01,  7.7272E-01,  7.8001E-01,  7.5547E-01,  7.1872E-01,
         6.7979E-01,  6.4164E-01,  6.0504E-01,  5.6997E-01,  5.3607E-01,  5.0277E-01,  4.6931E-01,  4.3199E-01,
         3.8746E-01,  3.3460E-01,  2.7123E-01,  1.9442E-01,  1.0243E-01,  3.4747E-03,  3.1627E-02,  3.1249E-01,
         6.2499E-01,  8.7816E-01,  9.3479E-01,  8.9655E-01,  8.4565E-01,  7.9466E-01,  7.4613E-01,  7.0030E-01,
         6.5677E-01,  6.1484E-01,  5.7368E-01,  5.3151E-01,  4.8291E-01,  4.2690E-01,  3.6244E-01,  2.8776E-01,
         2.0100E-01,  1.0437E-01,  3.9397E-03,  3.4140E-02,  3.1249E-01,  6.2500E-01,  9.3601E-01,  1.0908E+00,
         1.0330E+00,  9.6510E-01,  9.0112E-01,  8.4192E-01,  7.8681E-01,  7.3480E-01,  6.8485E-01,  6.3583E-01,
         5.8391E-01,  5.2465E-01,  4.5798E-01,  3.8304E-01,  2.9858E-01,  2.0549E-01,  1.0620E-01,  4.3220E-03,
         3.5887E-02,  3.1250E-01,  6.2500E-01,  9.3750E-01,  1.2500E+00,  1.1529E+00,  1.0692E+00,  9.9449E-01,
         9.2588E-01,  8.6236E-01,  8.0251E-01,  7.4497E-01,  6.8836E-01,  6.2670E-01,  5.5756E-01,  4.8112E-01,
         3.9679E-01,  3.0586E-01,  2.0979E-01,  1.0798E-01,  4.6174E-03,  3.6877E-02,  3.1250E-01,  6.2396E-01,
         9.1734E-01,  1.1479E+00,  1.1546E+00,  1.1088E+00,  1.0471E+00,  9.8424E-01,  9.2126E-01,  8.5666E-01,
         7.9286E-01,  7.2938E-01,  6.5911E-01,  5.8108E-01,  4.9636E-01,  4.0694E-01,  3.1293E-01,  2.1400E-01,
         1.0972E-01,  4.8200E-03,  3.7117E-02,  3.1097E-01,  6.0588E-01,  8.6639E-01,  1.0526E+00,  1.0806E+00,
         1.0693E+00,  1.0360E+00,  9.9296E-01,  9.4356E-01,  8.8783E-01,  8.2419E-01,  7.5644E-01,  6.8069E-01,
         5.9785E-01,  5.0976E-01,  4.1712E-01,  3.1998E-01,  2.1814E-01,  1.1139E-01,  4.9216E-03,  3.6509E-02,
         3.0353E-01,  5.7902E-01,  8.1072E-01,  9.6003E-01,  9.8676E-01,  1.0031E+00,  9.9376E-01,  9.6959E-01,
         9.3602E-01,  8.9422E-01,  8.4272E-01,  7.7847E-01,  7.0090E-01,  6.1499E-01,  5.2351E-01,  4.2744E-01,
         3.2700E-01,  2.2214E-01,  1.1290E-01,  4.9105E-03,  3.4793E-02,  2.9338E-01,  5.4929E-01,  7.5334E-01,
         8.6617E-01,  8.9081E-01,  9.1544E-01,  9.3057E-01,  9.2741E-01,  9.1235E-01,  8.8750E-01,  8.5160E-01,
         8.0010E-01,  7.2228E-01,  6.3305E-01,  5.3775E-01,  4.3786E-01,  3.3380E-01,  2.2572E-01,  1.1400E-01,
         4.7721E-03,  3.2230E-02,  2.8206E-01,  5.1778E-01,  6.9308E-01,  7.7022E-01,  7.9485E-01,  8.1948E-01,
         8.4411E-01,  8.6324E-01,  8.6898E-01,  8.6426E-01,  8.4838E-01,  8.1552E-01,  7.4542E-01,  6.5225E-01,
         5.5230E-01,  4.4790E-01,  3.3973E-01,  2.2820E-01,  1.1429E-01,  4.5258E-03,  2.9007E-02,  2.6994E-01,
         4.8377E-01,  6.2700E-01,  6.7426E-01,  6.9889E-01,  7.2353E-01,  7.4816E-01,  7.7279E-01,  7.9709E-01,
         8.1464E-01,  8.2358E-01,  8.1914E-01,  7.7103E-01,  6.7221E-01,  5.6584E-01,  4.5576E-01,  3.4286E-01,
         2.2858E-01,  1.1429E-01,  4.1797E-03,  2.5235E-02,  2.5676E-01,  4.4512E-01,  5.5023E-01,  5.7831E-01,
         6.0294E-01,  6.2757E-01,  6.5220E-01,  6.7608E-01,  6.9914E-01,  7.2227E-01,  7.4625E-01,  7.7187E-01,
         7.9999E-01,  6.8572E-01,  5.7143E-01,  4.5715E-01,  3.4286E-01,  2.2858E-01,  1.1429E-01,  3.7338E-03,
         2.0987E-02,  2.4149E-01,  3.9756E-01,  4.5772E-01,  4.8235E-01,  5.0624E-01,  5.2689E-01,  5.4531E-01,
         5.6257E-01,  5.7940E-01,  5.9628E-01,  6.1336E-01,  6.2992E-01,  6.3999E-01,  6.3186E-01,  5.6427E-01,
         4.5715E-01,  3.4286E-01,  2.2858E-01,  1.1429E-01,  3.1880E-03,  1.6310E-02,  2.2155E-01,  3.3264E-01,
         3.6166E-01,  3.8203E-01,  3.9797E-01,  4.1139E-01,  4.2346E-01,  4.3486E-01,  4.4598E-01,  4.5695E-01,
         4.6754E-01,  4.7658E-01,  4.7999E-01,  4.7999E-01,  4.7245E-01,  4.2657E-01,  3.4218E-01,  2.2858E-01,
         1.1429E-01,  2.5420E-03,  1.1239E-02,  1.8956E-01,  2.4052E-01,  2.5713E-01,  2.6835E-01,  2.7702E-01,
         2.8442E-01,  2.9121E-01,  2.9769E-01,  3.0399E-01,  3.1006E-01,  3.1554E-01,  3.1941E-01,  3.1999E-01,
         3.1999E-01,  3.1999E-01,  3.1737E-01,  2.9044E-01,  2.2479E-01,  1.1429E-01,  1.7957E-03,  5.7978E-03,
         1.1982E-01,  1.3170E-01,  1.3741E-01,  1.4126E-01,  1.4439E-01,  1.4721E-01,  1.4990E-01,  1.5251E-01,
         1.5503E-01,  1.5735E-01,  1.5920E-01,  1.6000E-01,  1.6000E-01,  1.6000E-01,  1.6000E-01,  1.6000E-01,
         1.6000E-01,  1.5335E-01,  1.1016E-01,  9.4906E-04,  7.6115E-08,  1.3378E-03,  2.5593E-03,  3.6624E-03,
         4.6450E-03,  5.5041E-03,  6.2360E-03,  6.8355E-03,  7.2956E-03,  7.6065E-03,  7.7542E-03,  7.7177E-03,
         7.4883E-03,  7.1010E-03,  6.5572E-03,  5.8568E-03,  4.9997E-03,  3.9857E-03,  2.8145E-03,  1.4861E-03,
         9.4486E-07]

        return numpy.array((linearRegridList), numpy.float32)

    elif choice == 'nonlinearRegrid':
        nonlinearRegridList = [
         1.4061E-07,  5.9856E-04,  3.3025E-03,  7.1219E-03,  1.1544E-02,  1.6204E-02,  2.0812E-02,  2.5066E-02,
         2.8761E-02,  3.1773E-02,  3.4004E-02,  3.5371E-02,  3.5807E-02,  3.5254E-02,  3.3662E-02,  3.0987E-02,
         2.7190E-02,  2.2235E-02,  1.6086E-02,  8.6904E-03,  9.3576E-07, -1.6475E-03,  4.0636E-02,  5.1891E-02,
         6.2648E-02,  7.2691E-02,  8.1856E-02,  8.8773E-02,  9.2560E-02,  9.4276E-02,  9.4444E-02,  9.3304E-02,
         9.0947E-02,  8.7384E-02,  8.2576E-02,  7.6457E-02,  6.8940E-02,  5.9917E-02,  4.9275E-02,  3.6498E-02,
         2.0278E-02,  1.0188E-04, -2.3704E-03,  1.1213E-01,  1.6880E-01,  1.8774E-01,  2.0453E-01,  2.1804E-01,
         2.2176E-01,  2.1920E-01,  2.1323E-01,  2.0517E-01,  1.9562E-01,  1.8480E-01,  1.7278E-01,  1.5950E-01,
         1.4489E-01,  1.2884E-01,  1.1104E-01,  8.9636E-02,  6.3626E-02,  3.2008E-02, -8.7618E-05, -2.3976E-03,
         1.5408E-01,  3.2600E-01,  3.6266E-01,  3.8633E-01,  3.9820E-01,  3.9203E-01,  3.7764E-01,  3.5940E-01,
         3.3920E-01,  3.1785E-01,  2.9562E-01,  2.7254E-01,  2.4856E-01,  2.2356E-01,  1.9698E-01,  1.6591E-01,
         1.2967E-01,  8.7819E-02,  4.0778E-02, -4.2476E-04, -1.9720E-03,  1.7771E-01,  4.5068E-01,  5.6699E-01,
         5.9747E-01,  6.0036E-01,  5.8014E-01,  5.5077E-01,  5.1774E-01,  4.8328E-01,  4.4829E-01,  4.1306E-01,
         3.7759E-01,  3.4182E-01,  3.0540E-01,  2.6448E-01,  2.1798E-01,  1.6583E-01,  1.0820E-01,  4.7477E-02,
        -8.5668E-04, -1.2987E-03,  1.8918E-01,  5.3532E-01,  7.6919E-01,  8.1425E-01,  8.0561E-01,  7.7032E-01,
         7.2542E-01,  6.7709E-01,  6.2785E-01,  5.7870E-01,  5.2997E-01,  4.8176E-01,  4.3409E-01,  3.8373E-01,
         3.2738E-01,  2.6522E-01,  1.9749E-01,  1.2515E-01,  5.2692E-02, -1.3503E-03, -5.4612E-04,  1.9741E-01,
         5.7911E-01,  9.1567E-01,  1.0141E+00,  9.9646E-01,  9.4885E-01,  8.9058E-01,  8.2845E-01,  7.6536E-01,
         7.0264E-01,  6.4088E-01,  5.8039E-01,  5.2051E-01,  4.5538E-01,  3.8409E-01,  3.0698E-01,  2.2467E-01,
         1.3902E-01,  5.7014E-02, -1.8810E-03,  1.5272E-04,  2.0341E-01,  5.9346E-01,  9.9962E-01,  1.1721E+00,
         1.1542E+00,  1.1021E+00,  1.0360E+00,  9.6363E-01,  8.8911E-01,  8.1453E-01,  7.4107E-01,  6.6956E-01,
         5.9736E-01,  5.1877E-01,  4.3381E-01,  3.4303E-01,  2.4748E-01,  1.5066E-01,  6.0634E-02, -2.4281E-03,
         6.9861E-04,  2.0731E-01,  6.0225E-01,  1.0160E+00,  1.2500E+00,  1.2555E+00,  1.2148E+00,  1.1508E+00,
         1.0743E+00,  9.9241E-01,  9.0888E-01,  8.2606E-01,  7.4565E-01,  6.6273E-01,  5.7280E-01,  4.7605E-01,
         3.7323E-01,  2.6681E-01,  1.6090E-01,  6.3627E-02, -2.9730E-03,  1.0247E-03,  2.0918E-01,  6.0431E-01,
         9.9699E-01,  1.2241E+00,  1.2631E+00,  1.2530E+00,  1.2085E+00,  1.1440E+00,  1.0666E+00,  9.7985E-01,
         8.9131E-01,  8.0517E-01,  7.1501E-01,  6.1671E-01,  5.1081E-01,  3.9933E-01,  2.8423E-01,  1.6982E-01,
         6.6003E-02, -3.4978E-03,  1.0951E-03,  2.0736E-01,  5.7738E-01,  9.3136E-01,  1.1486E+00,  1.2035E+00,
         1.2158E+00,  1.1947E+00,  1.1512E+00,  1.0908E+00,  1.0171E+00,  9.3223E-01,  8.4588E-01,  7.5422E-01,
         6.5262E-01,  5.4169E-01,  4.2310E-01,  2.9972E-01,  1.7733E-01,  6.7690E-02, -3.9847E-03,  8.6025E-04,
         1.9721E-01,  5.3426E-01,  8.4983E-01,  1.0425E+00,  1.0970E+00,  1.1360E+00,  1.1387E+00,  1.1160E+00,
         1.0752E+00,  1.0199E+00,  9.5238E-01,  8.7484E-01,  7.8660E-01,  6.8440E-01,  5.6941E-01,  4.4420E-01,
         3.1296E-01,  1.8312E-01,  6.8501E-02, -4.4155E-03,  2.4226E-04,  1.8267E-01,  4.8442E-01,  7.5872E-01,
         9.1395E-01,  9.6583E-01,  1.0156E+00,  1.0482E+00,  1.0516E+00,  1.0343E+00,  1.0007E+00,  9.5303E-01,
         8.9252E-01,  8.1160E-01,  7.1103E-01,  5.9312E-01,  4.6183E-01,  3.2305E-01,  1.8641E-01,  6.8047E-02,
        -4.7698E-03, -5.8461E-04,  1.6578E-01,  4.3042E-01,  6.5949E-01,  7.7110E-01,  8.2280E-01,  8.7431E-01,
         9.2200E-01,  9.5600E-01,  9.6629E-01,  9.5780E-01,  9.3276E-01,  8.9167E-01,  8.2627E-01,  7.3058E-01,
         6.1102E-01,  4.7395E-01,  3.2792E-01,  1.8550E-01,  6.5685E-02, -5.0087E-03, -1.4594E-03,  1.4726E-01,
         3.7257E-01,  5.5181E-01,  6.2548E-01,  6.7771E-01,  7.3133E-01,  7.8294E-01,  8.2931E-01,  8.6682E-01,
         8.8664E-01,  8.8759E-01,  8.6922E-01,  8.2572E-01,  7.3918E-01,  6.1846E-01,  4.7522E-01,  3.2240E-01,
         1.7822E-01,  6.2363E-02, -5.0958E-03, -2.2396E-03,  1.2721E-01,  3.1004E-01,  4.3490E-01,  4.8592E-01,
         5.3958E-01,  5.9600E-01,  6.5193E-01,  7.0330E-01,  7.4713E-01,  7.8192E-01,  8.0552E-01,  8.1423E-01,
         8.0000E-01,  7.2297E-01,  5.9985E-01,  4.5521E-01,  3.0629E-01,  1.6880E-01,  5.8577E-02, -4.9947E-03,
        -2.7897E-03,  1.0526E-01,  2.4121E-01,  3.1212E-01,  3.6074E-01,  4.1573E-01,  4.6891E-01,  5.1792E-01,
         5.6226E-01,  6.0169E-01,  6.3568E-01,  6.6300E-01,  6.8064E-01,  6.7757E-01,  6.4324E-01,  5.5705E-01,
         4.2785E-01,  2.8778E-01,  1.5805E-01,  5.4318E-02, -4.6651E-03, -2.9748E-03,  8.0481E-02,  1.6467E-01,
         2.0663E-01,  2.5106E-01,  2.9319E-01,  3.3190E-01,  3.6737E-01,  3.9985E-01,  4.2931E-01,  4.5529E-01,
         4.7648E-01,  4.8955E-01,  4.8514E-01,  4.6828E-01,  4.3856E-01,  3.6952E-01,  2.6597E-01,  1.4587E-01,
         4.9559E-02, -4.0612E-03, -2.6588E-03,  5.0914E-02,  8.7792E-02,  1.1716E-01,  1.4398E-01,  1.6823E-01,
         1.9056E-01,  2.1134E-01,  2.3069E-01,  2.4842E-01,  2.6399E-01,  2.7621E-01,  2.8241E-01,  2.7877E-01,
         2.7097E-01,  2.5980E-01,  2.4222E-01,  2.0011E-01,  1.2931E-01,  4.4238E-02, -3.1307E-03, -1.6950E-03,
         1.5529E-02,  2.6578E-02,  3.5795E-02,  4.4418E-02,  5.2999E-02,  6.1652E-02,  7.0306E-02,  7.8788E-02,
         8.6822E-02,  9.3985E-02,  9.9597E-02,  1.0247E-01,  1.0283E-01,  1.0170E-01,  9.8775E-02,  9.3695E-02,
         8.5980E-02,  7.1050E-02,  3.6490E-02, -1.8052E-03, -9.6657E-07, -8.0903E-03, -1.3190E-02, -1.6109E-02,
        -1.7301E-02, -1.7095E-02, -1.5765E-02, -1.3557E-02, -1.0694E-02, -7.3887E-03, -3.8441E-03, -2.5851E-04,
         3.1767E-03,  6.2818E-03,  8.8673E-03,  1.0736E-02,  1.1677E-02,  1.1452E-02,  9.7757E-03,  6.2375E-03,
         4.3809E-07]

        return numpy.array((nonlinearRegridList), numpy.float32)
    else:
        print 'unknown option in call for data in storedAnswers'
        return None

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

def storedGrids():
    """    #-------------------------------------------------------------------
    #                                      
    #     purpose: to construct a grid coordinate which is random 
    #
    #     passed : nothing 
    #
    #     returned: lati --  a 60 element latitude grid from -90. to +90. degrees
    #               latiSort -- lati sorted to be montonically decreasing 
    #               loni --  a 120 element longitude grid from 0. to 360. degrees
    #               loniSort -- loni sorted to be montonically increasing 
    #
    #------------------------------------------------------------------------"""
    latiList = [
      1.3092E+01,  7.1081E+01,  3.2199E+01,  2.6314E+01, -7.5665E+01, -7.2182E+00, -2.1963E+01, -8.3351E+01,
      4.8161E+01,  8.6379E+01, -5.6722E+01, -3.3604E+01,  3.4670E-01, -5.9393E+00, -1.7894E+01,  1.7068E+01,
     -1.0846E+01, -6.0505E+00, -4.9974E+01,  7.1796E+01,  3.3333E+01,  8.0870E+01,  2.7362E+00,  2.6315E+00,
     -3.9012E+01,  5.2667E+00, -8.1956E+01,  8.8042E+01,  8.0710E+00, -5.3203E+01, -6.5512E+00,  5.0851E+01,
      2.2580E+00, -2.2110E+01,  5.3739E+01, -8.7512E+01,  6.7964E+01,  3.9599E+01,  1.2495E+01, -1.1603E+01,
     -1.3217E+01,  3.0072E+01, -6.2477E+01,  8.9158E+01,  6.1896E+01,  3.5624E+01, -3.5438E+01,  6.2368E+01,
     -3.2040E+01,  7.2130E+01, -7.9999E+01,  6.4780E+01,  5.3882E+01,  6.9012E+01,  7.9715E+01, -7.2460E+01,
      7.5047E+00, -1.5061E+01,  2.5178E+01,  6.9948E+00]

    latiSortList = [
     -8.7512E+01, -8.3351E+01, -8.1956E+01, -7.9999E+01, -7.5665E+01, -7.2460E+01, -6.2477E+01, -5.6722E+01,
     -5.3203E+01, -4.9974E+01, -3.9012E+01, -3.5438E+01, -3.3604E+01, -3.2040E+01, -2.2110E+01, -2.1963E+01,
     -1.7894E+01, -1.5061E+01, -1.3217E+01, -1.1603E+01, -1.0846E+01, -7.2182E+00, -6.5512E+00, -6.0505E+00,
     -5.9393E+00,  3.4670E-01,  2.2580E+00,  2.6315E+00,  2.7362E+00,  5.2667E+00,  6.9948E+00,  7.5047E+00,
      8.0710E+00,  1.2495E+01,  1.3092E+01,  1.7068E+01,  2.5178E+01,  2.6314E+01,  3.0072E+01,  3.2199E+01,
      3.3333E+01,  3.5624E+01,  3.9599E+01,  4.8161E+01,  5.0851E+01,  5.3739E+01,  5.3882E+01,  6.1896E+01,
      6.2368E+01,  6.4780E+01,  6.7964E+01,  6.9012E+01,  7.1081E+01,  7.1796E+01,  7.2130E+01,  7.9715E+01,
      8.0870E+01,  8.6379E+01,  8.8042E+01,  8.9158E+01]
    latiSortList.reverse()

    loniList = [
      1.0950E+02,  3.1987E+02,  1.6087E+02,  2.2737E+02,  1.4790E+02,  6.2704E+01,  6.2566E+01,  2.4556E+02,
      2.4902E+01,  9.1912E+01,  1.2039E+02,  1.6807E+02,  1.8303E+02,  2.4495E+02,  1.1643E+01,  9.5821E+01,
      1.6826E+02,  2.3723E+02,  1.4022E+01,  2.6537E+02,  3.4034E+01,  1.0511E+02,  2.4025E+02,  1.0651E+02,
      8.4892E+01,  3.4940E+02,  1.6315E+02,  1.1100E+02,  1.4735E+02,  1.7356E+02,  7.5067E+01,  2.9491E+02,
      1.3526E+02,  3.4038E+02,  3.1191E+02,  2.4636E+02,  1.0361E+02,  3.1934E+02,  2.5720E+02,  3.5403E+02,
      1.8194E+02,  2.8795E+02,  9.0098E+01,  2.7536E+02,  4.1070E+01,  3.7064E+01,  1.5244E+02,  8.5413E+01,
      1.3328E+02,  3.2401E+02,  2.7889E+01,  1.3045E+02,  2.3126E+01,  2.2804E+02,  1.2270E+02,  1.5981E+02,
      2.1705E+02,  2.2611E+02,  2.9517E+02,  3.5181E+02,  3.0866E+02,  1.0522E+01,  2.2290E+01,  1.2809E+02,
      3.1070E+01,  2.3676E+02,  1.6915E+01,  3.2640E+02,  7.1367E+01,  1.9983E+02,  1.0566E+02,  2.7452E+02,
      1.3069E+02,  2.5578E+02,  2.2619E+02,  3.5151E+02,  3.3032E+01,  1.2169E+02,  1.4333E+02,  8.3669E+01,
      3.3945E-01,  2.8520E+02,  9.7079E+01,  3.1794E+02,  1.7400E+02,  3.1042E+02,  1.2403E+02,  2.8891E+02,
      2.5776E+02,  1.5096E+02,  4.0489E+01,  2.1803E+02,  2.6891E+02,  2.5970E+02,  2.3404E+02,  3.2476E+01,
      6.4254E+01,  2.9157E+02,  4.8417E+00,  2.7701E+02,  7.5394E+01,  1.5646E+02,  4.3079E+01,  1.6228E+02,
      3.3645E+02,  2.8462E+02,  3.4708E+02,  1.8942E+02,  1.4303E+02,  1.8721E+00,  1.3013E+02,  1.9077E+02,
      1.8328E+02,  3.5694E+02,  3.5559E+02,  1.4661E+01,  8.7624E+01,  2.0111E+02,  1.5145E+02,  1.8391E+02]

    loniSortList = [
      3.3945E-01,  1.8721E+00,  4.8417E+00,  1.0522E+01,  1.1643E+01,  1.4022E+01,  1.4661E+01,  1.6915E+01,
      2.2290E+01,  2.3126E+01,  2.4902E+01,  2.7889E+01,  3.1070E+01,  3.2476E+01,  3.3032E+01,  3.4034E+01,
      3.7064E+01,  4.0489E+01,  4.1070E+01,  4.3079E+01,  6.2566E+01,  6.2704E+01,  6.4254E+01,  7.1367E+01,
      7.5067E+01,  7.5394E+01,  8.3669E+01,  8.4892E+01,  8.5413E+01,  8.7624E+01,  9.0098E+01,  9.1912E+01,
      9.5821E+01,  9.7079E+01,  1.0361E+02,  1.0511E+02,  1.0566E+02,  1.0651E+02,  1.0950E+02,  1.1100E+02,
      1.2039E+02,  1.2169E+02,  1.2270E+02,  1.2403E+02,  1.2809E+02,  1.3013E+02,  1.3045E+02,  1.3069E+02,
      1.3328E+02,  1.3526E+02,  1.4303E+02,  1.4333E+02,  1.4735E+02,  1.4790E+02,  1.5096E+02,  1.5145E+02,
      1.5244E+02,  1.5646E+02,  1.5981E+02,  1.6087E+02,  1.6228E+02,  1.6315E+02,  1.6807E+02,  1.6826E+02,
      1.7356E+02,  1.7400E+02,  1.8194E+02,  1.8303E+02,  1.8328E+02,  1.8391E+02,  1.8942E+02,  1.9077E+02,
      1.9983E+02,  2.0111E+02,  2.1705E+02,  2.1803E+02,  2.2611E+02,  2.2619E+02,  2.2737E+02,  2.2804E+02,
      2.3404E+02,  2.3676E+02,  2.3723E+02,  2.4025E+02,  2.4495E+02,  2.4556E+02,  2.4636E+02,  2.5578E+02,
      2.5720E+02,  2.5776E+02,  2.5970E+02,  2.6537E+02,  2.6891E+02,  2.7452E+02,  2.7536E+02,  2.7701E+02,
      2.8462E+02,  2.8520E+02,  2.8795E+02,  2.8891E+02,  2.9157E+02,  2.9491E+02,  2.9517E+02,  3.0866E+02,
      3.1042E+02,  3.1191E+02,  3.1794E+02,  3.1934E+02,  3.1987E+02,  3.2401E+02,  3.2640E+02,  3.3645E+02,
      3.4038E+02,  3.4708E+02,  3.4940E+02,  3.5151E+02,  3.5181E+02,  3.5403E+02,  3.5559E+02,  3.5694E+02]

    lati = numpy.array((latiList), numpy.float32)
    latiSort = numpy.array((latiSortList), numpy.float32)
    loni = numpy.array((loniList), numpy.float32)
    loniSort = numpy.array((loniSortList), numpy.float32)

    return lati, latiSort, loni, loniSort

def grid2Dto1D(x, y):
    """    #-------------------------------------------------------------------
    #                                      
    #     purpose: to construct a linear grid from a rectangular one
    #
    #     passed : x[i] and y[j] 
    #
    #     returned: xn[n], yn[n]
    #
    #------------------------------------------------------------------------"""
 
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

    return (xn, yn)

def c1Dto2D(numberx, numbery, xn, yn, zn):
    """    #-------------------------------------------------------------------
    #                                      
    #     purpose: to construct  2D z[i,j] 1D zn[n] format
    #
    #     passed: xn[n], yn[n], zn[n]
    #
    #     returned : x[i], y[j] and z[i,j] 
    #
    #------------------------------------------------------------------------"""
 
    x = numpy.zeros(numberx, numpy.float32)
    y = numpy.zeros(numbery, numpy.float32)

    for i in range(numberx):
        x[i] = xn[i*numbery]

    for j in range(numbery):
        y[j] = yn[j]

    z = numpy.reshape(zn, (numberx, numbery))

    return (x, y, z)
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
    sendOutput('General information on the use of NATGRID has been written to the file natgridmodule.doc.')
    sendOutput('*********')
    sendOutput(' ')

    if testError == 0:
        print 'Testing Completed Successfully'
    else:
        print 'Testing completed but it may have problems. Look at test.asc for an explanation'

    print 'Some details on the testing have been written to the file test.asc.'
    print 'General information on the use of NATGRID has been written to the file natgridmodule.doc.'

    output.close()
