# Adapted for numpy/ma/cdms2 by convertcdms.py

"""Documentation for module dsgridtest: an automatic test for dsgrid, an interface to the ngmath DSGRID
  
   TESTING 
 
        Typing 
         
            cdat dsgridtest.py
         
        generates some testing of the dsgridmodule using analytical functions as fields. It also writes a 
        hard copy of the documentation to the file dsgridmodule.doc and a copy of the information describing 
        the nature of the tests to test.asc. For the single and the double precision interpolations from
        randomly spaced data to a rectangular grid on a sphere, the numerical results are written to netCDF files
        if there is access to the module cdms.
         
   DOCUMENTATION
  
        Without conducting the tests, documentation written to the file dsgridmodule.doc can be produced after
        importing the dsgridtest module by typing 
  
               dsgridtest.document() 
  
"""
import sys, numpy, math, random, ds, dsgridmodule

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
    #    usage:     import dsgridtest
    #               dsgridtest.document()
    #    
    #    passed :   nothing
    #
    #    returned:  nothing
    #
    #-------------------------------------------------------------------------------
    import ds

    std = sys.stdout                                             # save sys.stout to allow reassigning later
    sys.stdout = open( 'dsgridmodule.doc', 'w')

    print '**********************************************************************************************\n'  
    print '*************************** Overview of the CDAT interface to dsgrid ************************\n'
    print '**********************************************************************************************\n'  
    print ds.__doc__
    print
    print
    print '   HELP PACKAGE EXAMPLE \n'
    print '    ************************ Default Parameter Table **********************\n'
    print '    -----------------------------------------------------------------------------------------------------'
    ds.help('table')
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
    for n in range(1,11):
       err = choose(n)
       if err != 0:
           #print 'test number with error :',n,err
           testError = testError + 1
    return testError

def choose(case):
    #-------------------------------------------------------------------------------
    #                                      
    #     purpose:  verify the operation of dsgrid 
    #
    #---------------------------------------------------------------------------------
    err = 0

    if case == 1:   
        sendOutput('\n******* 3D interpolation -- gridded output, single precision *****\n')

        ni = 1000                                   # size of xi, yi, zi and dataIn
        nxo = 21
        nyo = 23
        nzo = 25

        xmin = -2.0
        xmax = 2.0
        ymin = -3.0
        ymax = 3.0
        zmin = -4.0
        zmax = 4.0

        # input arrays and data 
        xisort, xi = randomGrid(ni, xmax, xmin)                            # xisort has random numbers monotonically increasing
        yisort, yi = randomGrid(ni, ymax, ymin)                            # xisort has random numbers monotonically increasing
        zisort, zi = randomGrid(ni, zmax, zmin)                            # xisort has random numbers monotonically increasing

        dataIn = numpy.zeros((ni,), numpy.float32)
        for i in range(ni):
            dataIn[i] = xi[i]*xi[i] + yi[i]*yi[i] + zi[i]*zi[i]

        # output data 
        xo = uniformGrid(nxo, xmax, xmin)
        yo = uniformGrid(nyo, ymax, ymin)
        zo = uniformGrid(nzo, zmax, zmin)

        r = ds.Dsgrid(xi, xo, yi, yo, zi, zo)  

        dataOut = r.rgrd(dataIn) 

        sendOutput('*** writing interpolated field for test case to the netCDF file grid3Ds.nc')
        write1D_4DField('grid3Ds', dataOut, xo, yo, zo) 

        dataCheck = numpy.zeros((nxo,nyo,nzo), numpy.float32)
        for i in range(nxo):
            for j in range(nyo):
                for k in range(nzo):
                    dataCheck[i,j,k] = xo[i]*xo[i] + yo[j]*yo[j] + zo[k]*zo[k]


        sendOutput('*** writing exact answer for test case to the netCDF file grid3DsExact.nc')
        write1D_4DField('grid3DsExact', dataCheck, xo, yo, zo) 

        error = rmserror(dataOut, dataCheck)                                    # find the rms error
        sendOutput('******* compare results')
        sendOutput('*** the 3D interpolation test case rms error is usually less than 3.0')
        sendOutput('*** the 3D interpolation test case rms error = ', error)

        if error > 3.0:
            err = 1
        return err

    elif case == 2:   
        sendOutput('\n*******  3D interpolation -- gridded output, double precision *******\n')

        ni = 1000                                   # size of xi, yi, zi and dataIn
        nxo = 21
        nyo = 23
        nzo = 25

        xmin = -2.0
        xmax = 2.0
        ymin = -3.0
        ymax = 3.0
        zmin = -4.0
        zmax = 4.0

        # input arrays and data 
        xisort, xi = randomGrid(ni, xmax, xmin)                       # xisort has random numbers monotonically increasing
        yisort, yi = randomGrid(ni, ymax, ymin)                       # xisort has random numbers monotonically increasing
        zisort, zi = randomGrid(ni, zmax, zmin)                       # xisort has random numbers monotonically increasing

        xi = xi.astype(numpy.float64)
        yi = yi.astype(numpy.float64)
        zi = zi.astype(numpy.float64)

        dataIn = numpy.zeros((ni,), numpy.float64)
        for i in range(ni):
            dataIn[i] = xi[i]*xi[i] + yi[i]*yi[i] + zi[i]*zi[i]

        # output data 
        xo = uniformGrid(nxo, xmax, xmin)
        yo = uniformGrid(nyo, ymax, ymin)
        zo = uniformGrid(nzo, zmax, zmin)

        xo = xo.astype(numpy.float64)
        yo = yo.astype(numpy.float64)
        zo = zo.astype(numpy.float64)

        r = ds.Dsgrid(xi, xo, yi, yo, zi, zo)  

        dataOut = r.rgrd(dataIn) 

        xo = xo.astype(numpy.float32)
        yo = yo.astype(numpy.float32)
        zo = zo.astype(numpy.float32)
        dataOut = dataOut.astype(numpy.float32)

        sendOutput('*** writing interpolated field for test case to the netCDF file grid3Dd.nc')
        write1D_4DField('grid3Dd', dataOut, xo, yo, zo) 

        dataCheck = numpy.zeros((nxo,nyo,nzo), numpy.float32)
        for i in range(nxo):
            for j in range(nyo):
                for k in range(nzo):
                    dataCheck[i,j,k] = xo[i]*xo[i] + yo[j]*yo[j] + zo[k]*zo[k]

        sendOutput('*** writing exact answer for test case to the netCDF file grid3DdExact.nc')
        write1D_4DField('grid3DdExact', dataCheck, xo, yo, zo) 

        error = rmserror(dataOut, dataCheck)                                    # find the rms error
        sendOutput('******* compare results')
        sendOutput('*** the 3D interpolation test case rms error is usually less than 3.0')
        sendOutput('*** the 3D interpolation test case rms error = ', error)

        if error > 3.0:
            err = 1

        return err

    elif case == 3:   
        sendOutput('\n*******  2D interpolation -- gridded output, single precision *******\n')

        ni = 171                                  # size of xi, yi, and dataIn
        nxo = 21
        nyo = 23

        # input grids 
        xisort, xi = randomGrid(ni, 1.2, -0.2)                            # xisort has random numbers monotonically increasing
        yisort, yi = randomGrid(ni, 1.2, -0.2)

        # input data 
        dataIn = numpy.zeros((ni,), numpy.float32)
        for i in range(ni):
            dataIn[i] = (xi[i] - 0.25)*(xi[i] - 0.25) + (yi[i] - 0.50)*(yi[i] - 0.50)

        # output array 
        xo = uniformGrid(nxo, 2.0, 0.0)
        yo = uniformGrid(nyo, 1.0, 0.0)

        r = ds.Dsgrid(xi, xo, yi, yo)  

        dataOut = r.rgrd(dataIn) 

        sendOutput('*** writing interpolated field for test case to the netCDF file grid2Ds.nc')
        write1D_4DField('grid2Ds', dataOut, xo, yo) 

        # check the result

        dataCheck = numpy.zeros((nxo,nyo), numpy.float32)
        for i in range(nxo):
            for j in range(nyo):
                dataCheck[i,j] = (xo[i] - 0.25)*(xo[i] - 0.25) + (yo[j] - 0.50)*(yo[j] - 0.50)

        sendOutput('*** writing exact answer for test case to the netCDF file grid2DsExact.nc')
        write1D_4DField('grid2DsExact', dataCheck, xo, yo) 

        error = rmserror(dataOut, dataCheck)                                    # find the rms error
        sendOutput('******* compare results')
        sendOutput('*** the 2D interpolation test case rms error is usually less than 2.0')
        sendOutput('*** the 2D interpolation test case rms error = ', error)

        if error > 2.0:
            err = 1

        return err

    elif case == 4:   
        sendOutput('\n*******  2D interpolation -- gridded output, double precision *******\n')

        ni = 171                                  # size of xi, yi, and dataIn
        nxo = 21
        nyo = 21

        # input grids 
        xisort, xi = randomGrid(ni, 1.2, -0.2)                            # xisort has random numbers monotonically increasing
        yisort, yi = randomGrid(ni, 1.2, -0.2)
        xi = xi.astype(numpy.float64)
        yi = yi.astype(numpy.float64)

        # input data 
        dataIn = numpy.zeros((ni,), numpy.float64)
        for i in range(ni):
            dataIn[i] = (xi[i] - 0.25)*(xi[i] - 0.25) + (yi[i] - 0.50)*(yi[i] - 0.50)

        # output array 
        xo = uniformGrid(nxo, 2.0, 0.0)
        yo = uniformGrid(nyo, 1.0, 0.0)
        xo = xo.astype(numpy.float64)
        yo = yo.astype(numpy.float64)

        r = ds.Dsgrid(xi, xo, yi, yo)  

        dataOut = r.rgrd(dataIn) 

        xo = xo.astype(numpy.float32)
        yo = yo.astype(numpy.float32)
        dataOut = dataOut.astype(numpy.float32)

        sendOutput('*** writing interpolated field for test case to the netCDF file grid2Dd.nc')
        write1D_4DField('grid2Dd', dataOut, xo, yo) 

        # check the result

        dataCheck = numpy.zeros((nxo,nyo), numpy.float32)
        for i in range(nxo):
            for j in range(nyo):
                dataCheck[i,j] = (xo[i] - 0.25)*(xo[i] - 0.25) + (yo[j] - 0.50)*(yo[j] - 0.50)

        sendOutput('*** writing exact answer for test case to the netCDF file grid2DdExact.nc')
        write1D_4DField('grid2DdExact', dataCheck, xo, yo) 

        error = rmserror(dataOut, dataCheck)                                    # find the rms error
        sendOutput('******* compare results')
        sendOutput('*** the 2D interpolation test case rms error is usually less than 3.0')
        sendOutput('*** the 2D interpolation test case rms error = ', error)

        if error > 3.0:
            err = 1

        return err

    elif case == 5:   
        sendOutput('\n*******  3D interpolation -- comparing listed output to gridded output, single precision ******\n')

        ni = 1000                                   # size of xi, yi, zi and dataIn
        nxo = 21
        nyo = 23
        nzo = 25

        xmin = -2.0
        xmax = 2.0
        ymin = -3.0
        ymax = 3.0
        zmin = -4.0
        zmax = 4.0

        # input arrays and data 
        xisort, xi = randomGrid(ni, xmax, xmin)                            # xisort has random numbers monotonically increasing
        yisort, yi = randomGrid(ni, ymax, ymin)                            # xisort has random numbers monotonically increasing
        zisort, zi = randomGrid(ni, zmax, zmin)                            # xisort has random numbers monotonically increasing

        dataIn = numpy.zeros((ni,), numpy.float32)
        for i in range(ni):
            dataIn[i] = xi[i]*xi[i] + yi[i]*yi[i] +zi[i]*zi[i]

        # output data 
        xo = uniformGrid(nxo, xmax, xmin)
        yo = uniformGrid(nyo, ymax, ymin)
        zo = uniformGrid(nzo, zmax, zmin)

        # ----------- get the answer from calling grid3s

        r = ds.Dsgrid(xi, xo, yi, yo, zi, zo)  
        ans = r.rgrd(dataIn) 

        # ----------- get the answer from calling pnt3s

        xL, yL, zL = CtolistXYZ(xo, yo, zo)                                # get the grid in list format

        r = ds.Dsgrid(xi, xL, yi, yL, zi, zL, griddedOutput = 'no')  

        dataOut = r.rgrd(dataIn) 

        ans = numpy.ravel(ans)
        error = rmserror(dataOut, ans)                                    # find the rms error
        sendOutput('******* compare results')
        sendOutput('*** the 3D interpolation test case rms error is usually less than 1.0e-05')
        sendOutput('*** the 3D interpolation test case rms error = ', error)

        if error > 1.0e-05:
            err = 1

        return err

    elif case == 6:   
        sendOutput('\n*******  3D interpolation -- comparing listed output to gridded output, double precision ******\n')

        ni = 1000                                   # size of xi, yi, zi and dataIn
        nxo = 21
        nyo = 23
        nzo = 25

        xmin = -2.0
        xmax = 2.0
        ymin = -3.0
        ymax = 3.0
        zmin = -4.0
        zmax = 4.0

        # input arrays and data 
        xisort, xi = randomGrid(ni, xmax, xmin)                    # xisort has random numbers monotonically increasing
        yisort, yi = randomGrid(ni, ymax, ymin)                    # xisort has random numbers monotonically increasing
        zisort, zi = randomGrid(ni, zmax, zmin)                    # xisort has random numbers monotonically increasing

        xi = xi.astype(numpy.float64)
        yi = yi.astype(numpy.float64)
        zi = zi.astype(numpy.float64)

        dataIn = numpy.zeros((ni,), numpy.float64)
        for i in range(ni):
            dataIn[i] = xi[i]*xi[i] + yi[i]*yi[i] + zi[i]*zi[i]

        # output data 
        xo = uniformGrid(nxo, xmax, xmin)
        yo = uniformGrid(nyo, ymax, ymin)
        zo = uniformGrid(nzo, zmax, zmin)

        xo = xo.astype(numpy.float64)
        yo = yo.astype(numpy.float64)
        zo = zo.astype(numpy.float64)

        # ----------- get the answer from calling grid3d

        r = ds.Dsgrid(xi, xo, yi, yo, zi, zo)  
        ans = r.rgrd(dataIn) 

        # ----------- get the answer from calling pnt3d

        xL, yL, zL = CtolistXYZ(xo, yo, zo)                                # get the grid in list format
        xL = xL.astype(numpy.float64)
        yL = yL.astype(numpy.float64)
        zL = zL.astype(numpy.float64)

        r = ds.Dsgrid(xi, xL, yi, yL, zi, zL, griddedOutput = 'no')  

        dataOut = r.rgrd(dataIn) 

        xo = xo.astype(numpy.float32)
        yo = yo.astype(numpy.float32)
        zo = zo.astype(numpy.float32)
        ans = ans.astype(numpy.float32)

        ans = ans.astype(numpy.float64)
        ans = numpy.ravel(ans)
        error = rmserror(dataOut, ans)                                    # find the rms error
        sendOutput('******* compare results')
        sendOutput('*** the 3D interpolation test case rms error is usually less than 1.0e-06')
        sendOutput('*** the 3D interpolation test case rms error = ', error)

        if error > 1.0e-06:
            err = 1

        return err

    elif case == 7:   
        sendOutput('\n*******  2D interpolation -- comparing listed output to gridded output, single precision ******\n')

        ni = 1000                                  # size of xi, yi, and dataIn
        nxo = 21
        nyo = 23

        # input grids 
        xisort, xi = randomGrid(ni, 1.2, -0.2)                            # xisort has random numbers monotonically increasing
        yisort, yi = randomGrid(ni, 1.2, -0.2)

        # input data 
        dataIn = numpy.zeros((ni,), numpy.float32)
        for i in range(ni):
            dataIn[i] = (xi[i] - 0.25)*(xi[i] - 0.25) + (yi[i] - 0.50)*(yi[i] - 0.50)

        # output array 
        xo = uniformGrid(nxo, 2.0, 0.0)
        yo = uniformGrid(nyo, 1.0, 0.0)

        # ----------- get the answer from calling grid2s

        r = ds.Dsgrid(xi, xo, yi, yo)  
        ans = r.rgrd(dataIn) 

        # ----------- get the answer from calling pnt2s

        xL, yL = CtolistXY(xo, yo)                                # get the grid in list format

        r = ds.Dsgrid(xi, xL, yi, yL, griddedOutput = 'no')  

        dataOut = r.rgrd(dataIn) 

        ans = numpy.ravel(ans)
        error = rmserror(dataOut, ans)                                    # find the rms error
        sendOutput('******* compare results')
        sendOutput('*** the 2D interpolation test case rms error is usually less than 1.0e-06')
        sendOutput('*** the 2D interpolation test case rms error = ', error)

        if error > 1.0e-06:
            err = 1

        return err

    elif case == 8:   
        sendOutput('\n*******  2D interpolation -- comparing listed output to gridded output, single precision ******\n')

        ni = 1000                                  # size of xi, yi, and dataIn
        nxo = 21
        nyo = 23

        # input grids 
        xisort, xi = randomGrid(ni, 1.2, -0.2)                            # xisort has random numbers monotonically increasing
        yisort, yi = randomGrid(ni, 1.2, -0.2)
        xi = xi.astype(numpy.float64)
        yi = yi.astype(numpy.float64)

        # input data 
        dataIn = numpy.zeros((ni,), numpy.float64)
        for i in range(ni):
            dataIn[i] = (xi[i] - 0.25)*(xi[i] - 0.25) + (yi[i] - 0.50)*(yi[i] - 0.50)

        # output array 
        xo = uniformGrid(nxo, 2.0, 0.0)
        yo = uniformGrid(nyo, 1.0, 0.0)
        xo = xo.astype(numpy.float64)
        yo = yo.astype(numpy.float64)

        # ----------- get the answer from calling grid2d

        r = ds.Dsgrid(xi, xo, yi, yo)  
        ans = r.rgrd(dataIn) 

        # ----------- get the answer from calling pnt3d

        xL, yL = CtolistXY(xo, yo)                                # get the grid in list format
        xL = xL.astype(numpy.float64)
        yL = yL.astype(numpy.float64)

        r = ds.Dsgrid(xi, xL, yi, yL, griddedOutput = 'no')  

        dataOut = r.rgrd(dataIn) 

        xo = xo.astype(numpy.float32)
        yo = yo.astype(numpy.float32)
        ans = ans.astype(numpy.float32)

        ans = ans.astype(numpy.float64)
        ans = numpy.ravel(ans)
        error = rmserror(dataOut, ans)                                    # find the rms error
        sendOutput('******* compare results')
        sendOutput('*** the 2D interpolation test case rms error is usually less than 1.0e-06')
        sendOutput('*** the 2D interpolation test case rms error = ', error)

        if error > 1.0e-06:
            err = 1

        return err

    elif case == 9:   
        sendOutput('\n******* 2D example varying exponent of distances, single precision ******\n')

        nxo = 61
        nyo = 61

        # input arrays 

        xiList     = [0.00, 1.00, 0.00, 1.00, 0.40, 0.75]
        yiList     = [0.00, 0.00, 1.00, 1.00, 0.20, 0.65]
        dataInList = [0.00, 0.00, 0.00, 0.00, 1.25, 0.80]

        xi = numpy.array(xiList, numpy.float32)
        yi = numpy.array(yiList, numpy.float32)
        dataIn = numpy.array(dataInList, numpy.float32)

        # output array 

        xo = uniformGrid(nxo, 1.0, 0.0)
        yo = uniformGrid(nyo, 1.0, 0.0)

        r = ds.Dsgrid(xi, xo, yi, yo)  

        r.exp = 0.5
        dataOut = r.rgrd(dataIn) 
        sendOutput('*** writing interpolated field with exp control parameter set to 0.5 to the netCDF file exp_5.nc')
        write1D_4DField('exp_5', dataOut, xo, yo) 

        r.exp = 1.0
        dataOut = r.rgrd(dataIn) 
        sendOutput('*** writing interpolated field with exp control parameter set to 1.0 to the netCDF file exp1.nc')
        write1D_4DField('exp1', dataOut, xo, yo) 

        r.exp = 5.0
        dataOut = r.rgrd(dataIn) 
        sendOutput('*** writing interpolated field with exp control parameter set to 5.0 to the netCDF file exp5.nc')
        write1D_4DField('exp5', dataOut, xo, yo) 

        return err

    elif case == 10:   
        sendOutput('\n******* 2D example with the shadow feature off and on, single precision ******\n')

        nxo = 21
        nyo = 21

        # input arrays 
        xiList     = [0.00, 1.00, 0.00, 1.00, 0.30, 0.30, 0.30, 0.69, 0.71, 0.71, 0.69, 0.70, 0.70, 0.70, 0.69, 0.71]
        yiList     = [0.00, 0.00, 1.00, 1.00, 0.70, 0.30, 0.70, 0.69, 0.71, 0.71, 0.69, 0.70, 0.70, 0.70, 0.69, 0.71]
        dataInList = [0.00, 0.00, 0.00, 0.50, 0.50, 0.50, 0.50, 1.00, 1.00, 1.00, 1.00, 1.00, 1.00, 1.00, 1.00, 1.00]

        xi = numpy.array(xiList, numpy.float32)
        yi = numpy.array(yiList, numpy.float32)
        dataIn = numpy.array(dataInList, numpy.float32)

        # output array 
        xo = uniformGrid(nxo, 1.0, 0.0)
        yo = uniformGrid(nyo, 1.0, 0.0)

        r = ds.Dsgrid(xi, xo, yi, yo)  

        dataOut = r.rgrd(dataIn) 

        sendOutput('*** writing interpolated field with shd control parameter set to 0 to the netCDF file shd0.nc')
        write1D_4DField('shd0', dataOut, xo, yo) 

        r.shd = 1
        dataOut = r.rgrd(dataIn) 

        sendOutput('*** writing interpolated field with shd control parameter set to 1 to the netCDF file shd1.nc')
        write1D_4DField('shd1', dataOut, xo, yo) 

        return err

# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# ++++++++++++++++++++++++++++++++ Autotest Utilities +++++++++++++++++++++++++++++++++
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

def randomGrid(number, vmax, vmin):
    """    #-------------------------------------------------------------------
    #                                      
    #     purpose: to construct a grid coordinate which is random but monotonic
    #
    #     passed :  number, vmax, vmin
    #
    #     returned: vsort[i], vraw[i]
    #
    #------------------------------------------------------------------------"""
    listNumbers = []                                              # generate random numbers
    vrange = vmax - vmin
    for i in range(number):
        listNumbers.append(vmin + vrange*random.random() )

    vraw = numpy.array(listNumbers, numpy.float32)            # make array of raw list of random numbers

    listNumbers.sort()                                            # make array of sorted list of random numbers
    vsort = numpy.array(listNumbers, numpy.float32)

    return vsort, vraw

 
def uniformGrid(number, vmax, vmin):
    """    #-------------------------------------------------------------------
    #                                      
    #     purpose: to construct a grid coordinate which is uniform
    #
    #     passed :  number, vmax, vmin
    #
    #     returned: v[i]
    #
    #------------------------------------------------------------------------"""
    v = numpy.zeros((number,), numpy.float32)

    vinc = (vmax - vmin)/(number - 1)
    for n in range(number):
        v[n] = vmin + n*vinc
    return v

def write1D_4DField(varname, dataField, x, y = None, z = None, t = None): 

    """    #-------------------------------------------------------------------
    #                                      
    #     purpose: write an output field which may be 1D, 2D, 3D or 4D
    #
    #     usage:  
    #
    #     passed :  dimension vectors, varname, dataField 
    #
    #     returned: None 
    #
    #------------------------------------------------------------------------"""
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

def CtolistXY(x, y):
    """    #-------------------------------------------------------------------
    #                                      
    #     purpose: to construct 1D xn and yn from grid form x, y in C order
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
 
def CtolistXYZ(x, y, z):
    """    #-------------------------------------------------------------------
    #                                      
    #     purpose: to construct 1D xn, yn and zn from grid form x, y,z in C order
    #
    #     passed : x[i], y[j], z[k] 
    #
    #     returned: xn[n], yn[n], zn[n]
    #
    #------------------------------------------------------------------------"""
 
    numberx = len(x)
    numbery = len(y)
    numberz = len(z)
    size =numberx*numbery*numberz
    xn = numpy.zeros(size, numpy.float32)
    yn = numpy.zeros(size, numpy.float32)
    zn = numpy.zeros(size, numpy.float32)

    for i in range(numberx):
        for j in range(numbery):
            for k in range(numberz):
                n = k + j*numberz + i*numbery*numberz
                xn[n] = x[i]
                yn[n] = y[j]
                zn[n] = z[k]

    return (xn, yn, zn)
 
def writeasc(name, a):

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

def rmserror(data1, data2):
    """        #---------------------------------------------------------------------------------
    #
    #    purpose: compute the rms error for two data sets having the same shape
    # 
    #    passed : the two data sets 
    #             
    #    returned: rms error
    #
    #---------------------------------------------------------------------------------"""

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

    print 'Running the test computations. This will take a few minutes.'
    testError = runtests() 
    write = document()

    sendOutput(' ')
    sendOutput('*********')
    sendOutput('General information on the use of DSGRID has been written to the file dsgridmodule.doc.')
    sendOutput('*********')
    sendOutput(' ')

    if testError == 0:
        print 'Testing Completed Successfully'
    else:
        print 'Testing completed but it may have problems. Look at test.asc for an explanation'

    print 'Some details on the testing have been written to the file test.asc.'
    print 'General information on the use of NATGRID has been written to the file dsgridmodule.doc.'

    output.close()
