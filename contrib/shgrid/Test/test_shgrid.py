
"""Documentation for module shgridtest: an automatic test for shgrid, an interface to the ngmath SHGRID
  
   TESTING 
 
        Typing 
         
            cdat shgridtest.py
         
        generates some testing of the shgridmodule using analytical functions as fields. It also writes a 
        hard copy of the documentation to the file shgridmodule.doc and a copy of the information describing 
        the nature of the tests to test.asc. For the single precision interpolations from randomly spaced data
        in 3-space to a 3D cartesian grid, the numerical results are written to netCDF files if there is access
        to the module cdms.
         
   DOCUMENTATION
  
        Without conducting the tests, documentation written to the file shgridmodule.doc can be produced after
        importing the shgridtest module by typing 
  
               shgridtest.document() 
  
"""
import sys, numpy, math, random, sh, shgridmodule

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
    #    usage:     import shgridtest
    #               shgridtest.document()
    #    
    #    passed :   nothing
    #
    #    returned:  nothing
    #
    #-------------------------------------------------------------------------------
    import sh

    std = sys.stdout                                             # save sys.stout to allow reassigning later
    sys.stdout = open( 'shgridmodule.doc', 'w')

    print '**********************************************************************************************\n'  
    print '*************************** Overview of the CDAT interface to shgrid ************************\n'
    print '**********************************************************************************************\n'  
    print sh.__doc__
    print
    print
    print '   HELP PACKAGE EXAMPLE \n'
    print '    ************************ Default Parameter Table **********************\n'
    print '    -----------------------------------------------------------------------------------------------------'
    sh.help('table')
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
    for n in range(1,4):
       err = choose(n)
       if err != 0:
           print 'test number with error :',n,err
           testError = testError + 1
    return testError

def choose(case):
    #-------------------------------------------------------------------------------
    #                                      
    #     purpose:  verify the operation of shgrid 
    #
    #---------------------------------------------------------------------------------
    err = 0

    if case == 1:   
        sendOutput('\n******* find the nearest point to the given point (.5,.5,.5) in 3-space *****\n')

        ni = 1331

        # input grid 
        xi, x = randomGrid(ni, 1.0, 0.0)
        yi, y = randomGrid(ni, 1.0, 0.0)
        zi, z = randomGrid(ni, 1.0, 0.0)

        # output point 
        xo = 0.5
        yo = 0.5
        zo = 0.5

        r = sh.Shgrid(xi, yi, zi, xo, yo, zo) 
        numberPoints = 5
        np = r.rgrd(numberPoints)            

        sendOutput('******* For this particular randomly selected grid, the point nearest has :')
        sendOutput('*** x coordinate = ', xi[np[0]])
        sendOutput('*** y coordinate = ', yi[np[0]])
        sendOutput('*** z coordinate = ', zi[np[0]])

        return err

    elif case == 2:   
        sendOutput('\n******* create an ellipsoidal isosurface *****\n')

        ni = 1000
        nxo = 21 
        nyo = 21 
        nzo = 21 

        xmin = -2.0
        xmax = 2.0
        ymin = -2.0
        ymax = 2.0
        zmin = -2.0
        zmax = 2.0

        # input arrays 

        x, xi = randomGrid(ni, xmax, xmin)
        y, yi = randomGrid(ni, ymax, ymin)
        z, zi = randomGrid(ni, zmax, zmin)


        dataIn = numpy.zeros((ni,), numpy.float32)
        for i in range(ni):
            dataIn[i] = 0.5*xi[i]*xi[i] + yi[i]*yi[i] + zi[i]*zi[i]

        # output arrays 

        xo = uniformGrid(nxo, xmax, xmin)
        yo = uniformGrid(nyo, ymax, ymin)
        zo = uniformGrid(nzo, zmax, zmin)

        dataCheck = numpy.zeros((nzo, nyo, nxo), numpy.float32)
        for k in range(nzo):
            for j in range(nyo):
                for i in range(nxo):
                    dataCheck[k,j,i] = 0.5*xo[i]*xo[i] + yo[j]*yo[j] + zo[k]*zo[k]

        r = sh.Shgrid(xi, yi, zi, xo, yo, zo) 

        dataOut = r.rgrd(dataIn)            

        sendOutput('*** writing exact ellipsoidal surface to ellipsoidExact.nc')
        write1D_4DField('ellipsoidExact', dataCheck, zo, yo, xo) 
        sendOutput('*** writing interpolated ellipsoidal surface to ellipsoidInterp.nc')
        write1D_4DField('ellipsoidInterp', dataOut, zo, yo, xo) 

        error = rmserror(dataOut, dataCheck)                                 # find the rms error
        sendOutput('\n******* compare results\n')
        sendOutput('*** the ellipsoidalal surface interpolation test case rms error is usually less than 1.e-05')
        sendOutput('*** the ellipsoidalal surface interpolation test case rms error = ', error)

        if error > .0001:
            err = 1

        return err

    elif case == 3:   
        sendOutput('\n******* create a tube-shaped isosurface *****\n')

        ni = 1000
        nxo = 21 
        nyo = 31 
        nzo = 41 

        xmin = -2.0
        xmax = 2.0
        ymin = -2.0
        ymax = 2.0
        zmin = -2.0
        zmax = 2.0

        # input arrays 

        x, xi = randomGrid(ni, xmax, xmin)
        y, yi = randomGrid(ni, ymax, ymin)
        z, zi = randomGrid(ni, zmax, zmin)

        dataIn = numpy.zeros((ni,), numpy.float32)
        for i in range(ni):
            dataIn[i] = 0.75*xi[i]*xi[i] - 1.6*yi[i]*yi[i] + 2.0*zi[i]*zi[i]

        # output arrays 

        xo = uniformGrid(nxo, xmax, xmin)
        yo = uniformGrid(nyo, ymax, ymin)
        zo = uniformGrid(nzo, zmax, zmin)

        dataCheck = numpy.zeros((nzo, nyo, nxo), numpy.float32)
        for k in range(nzo):
            for j in range(nyo):
                for i in range(nxo):
                    dataCheck[k,j,i] = 0.75*xo[i]*xo[i] - 1.6*yo[j]*yo[j] + 2.0*zo[k]*zo[k]

        r = sh.Shgrid(xi, yi, zi, xo, yo, zo) 
        dataOut = r.rgrd(dataIn)            

        sendOutput('*** writing exact tube-shape surface to tubeExact.nc')
        write1D_4DField('tubeExact', dataCheck, zo, yo, xo) 
        sendOutput('*** writing interpolated tube-shape surface to tubeInterp.nc')
        write1D_4DField('tubeInterp', dataOut, zo, yo, xo) 

        error = rmserror(dataOut, dataCheck)                                 # find the rms error
        sendOutput('\n******* compare results\n')
        sendOutput('*** the tube-shape surface interpolation test case rms error is usually less than 1.e-05')
        sendOutput('*** the tube-shape surface interpolation test case rms error = ', error)

        if error > .0001:
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

    print 'Running the test computations.'
    testError = runtests() 
    write = document()

    sendOutput(' ')
    sendOutput('*********')
    sendOutput('General information on the use of SHGRID has been written to the file shgridmodule.doc.')
    sendOutput('*********')
    sendOutput(' ')

    if testError == 0:
        print 'Testing Completed Successfully'
    else:
        print 'Testing completed but it may have problems. Look at test.asc for an explanation'

    print 'Some details on the testing have been written to the file test.asc.'
    print 'General information on the use of SHGRID has been written to the file shgridmodule.doc.'

    output.close()
