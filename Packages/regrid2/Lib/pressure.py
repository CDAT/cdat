## Automatically adapted for numpy.oldnumeric Aug 02, 2007 by 

import cdms2
import numpy, copy, string, _regrid
from error import RegridError

class PressureRegridder:
    """    #-----------------------------------------------------------------------------------------------
    # 
    #    PURPOSE: To perform all the tasks required to regrid the input data into the ouput data along
    #             the pressure dimension only.
    #
    #    PROCEDURE: Step One:
    #                  Make an instance of class PressureRegridder passing it input and output grid information
    #               Step Two:
    #                  Pass the input data with some descriptive parameters and get the output data 
    #                  in return
    #                
    #------------------------------------------------------------------------------------------------"""


    def __init__(self, axisIn, axisOut):
        """        #-----------------------------------------------------------------------------------------------
        # 
        #    PURPOSE: To make an instance which entails setting up the input and output grids 
        # 
        #    DEFINITION:
        #
        #             def __init__(self, levIn, levOut):
        # 
        #    PROCEDURE: 
        # 
        #        The user must assemble two pieces of information: 
        #
        #            axisIn -  the input level axis
        #
        #            axisOut -  the output level axis
        #
        #    USAGE: 
        #                
        #          To  make an instance preparing for a regrid along the level dimension pnly, type                     
        #                
        #             r = PressureRegridder(levIn, levOut)
        #                
        #------------------------------------------------------------------------------------------------"""

        # ---  set the instance grid data attributes used to describe input and output grid sizes

        self.axisIn = axisIn
        self.axisOut = axisOut
        self.nlevi = len(axisIn)
        self.nlevo = len(axisOut)

    def __call__(self, ar, missing=None, order=None, method="log"):
        """
        Call the pressure regridder function.
        ar is the input array, a variable, masked array, or numpy array.
        missing is the missing data value, if any. It defaults to the missing/fill value
          defined for the input array, if any.
        order is of the form "tzyx", "tyx", etc.
        method is either 'log' to interpolate in the log of pressure, or 'linear' for linear interpolation.
        """

        import cdms2
        from cdms2.avariable import AbstractVariable
        from cdms2.tvariable import TransientVariable

        # Save Variable metadata for output
        if isinstance(ar, AbstractVariable):
            attrs = copy.copy(ar.attributes)
            varid = ar.id
            axislist = ar.getAxisList()
            inputIsVariable = 1
            if order is None:
                order = ar.getOrder()
            #this expects contiguous arrays
            if isinstance(ar, TransientVariable) and ar.iscontiguous() is False:
                ar = ar.ascontiguous()
        else:
            inputIsVariable = 0

        # Turn ar into a numpy array.
        if numpy.ma.isMaskedArray(ar):
            armiss = ar.fill_value
            ar = numpy.ma.filled(ar)
        elif isinstance(ar, AbstractVariable):
            tempar = ar.getValue(squeeze=0)
            armiss = ar.getMissing()
            ar = numpy.ma.filled(tempar)
        elif isinstance(ar, numpy.ndarray):
            armiss = None
        else:
            raise RegridError, "Input array is not a Variable, numpy.ma, or numpy array"
        
        # Set missing value
        if missing is None:
            missing = armiss
        if isinstance(missing, numpy.ndarray):
            missing = missing[0]

        rank = len(ar.shape)
        assert 3<=rank<=4, 'Array rank is %i, must be  3 or 4'%rank

        # Set the default order to match the input grid order
        if order is None:
            if rank==3:
                order = "zyx"
            elif rank==4:
                order = "tzyx"
        
        assert rank==len(order), 'Order must be same length as array rank: %i'%len(ar.shape)

        order = string.lower(order)

        # Map order to positionIn
        positionIn = [None]*4
        for i in range(len(order)):
            if order[i]=='x':
                positionIn[0]=i
            elif order[i]=='y':
                positionIn[1]=i
            elif order[i]=='z':
                positionIn[2]=i
                if inputIsVariable:
                    axislist[i] = self.axisOut
            else:
                positionIn[3]=i

        # Regrid
        if method=='log':
            logYes = 'yes'
        else:
            logYes = 'no'
        outar = self.rgrd(ar, missing, 'greater', logYes, positionIn)

        # Reconstruct the same class as on input
        if inputIsVariable==1:
            result = cdms2.createVariable(outar, fill_value = missing,
                                         axes = axislist, attributes = attrs, id = varid)
        else:
            result = numpy.ma.masked_array(outar, fill_value = missing)

        return result

    def rgrd(self, dataIn, missingValueIn, missingMatch, logYes = 'yes', positionIn = None, missingValueOut = None):

        """        #---------------------------------------------------------------------------------
        #
        #    PURPOSE: To perform all the tasks required to regrid the input data, dataIn, into the ouput data,
        #             dataout along the level dimension only.
        #
        #    DEFINITION:
        #
        #             def rgrd(self, dataIn, missingValueIn, missingMatch, positionIn = None, missingValueOut = None):
        # 
        # 
        #    PASSED :  dataIn -- data to regrid
        #
        #             missingValueIn -- the missing data value to use in setting missing in the mask. It is required
        #                               and there are two choices:
        #                                     None -- there is no missing data
        #                                     A number -- the value to use in the search for possible missing data.
        #                               The presence of missing data at a grid point leads to recording 0.0 in the mask.
        #
        #             missingMatch -- the comparison scheme used in searching for missing data in dataIn using the value passed
        #                             in as missingValueIn. The choices are:
        #                                  None -- used if None is the entry for missingValueIn
        #                                  exact -- used if missingValue is the exact value from the file
        #                                  greater -- the missing data value is equal to or greater than missingValueIn
        #                                  less -- the missing data value is equal to or less than missingValueIn
        #
        #             logYes -- choose the level regrid as linear in log of level or linear in level. Set to 
        #                       'yes' for log. Anything else is linear in level.
        #         
        #
        #
        #             positionIn -- a tuple with the numerical position of the dimensions
        #                           in C or Python order specified in the sequence longitude,
        #                           latitude, level and time. Longitude, latitude and level are
        #                           required. If time is missing submit None in its slot in the 
        #                           tuple. Notice that the length of the tuple is always four.
        #
        #                           Explicitly, in terms of the shape of dataIn as returned by Python's shape function
        #
        #                                positionIn[0] contains the position of longitude in dataIn      
        #                                positionIn[1] contains the position of latitude in dataIn      
        #                                positionIn[2] contains the position of level in dataIn or None      
        #                                positionIn[3] contains the position of time in dataIn or None      
        #
        #                           As  examples:
        #                                If the C order shape of 4D data is
        #                                    (number of longitudes, number of times, number of levels, number of latitudes)
        #                                submit
        #                                     (0, 3, 2, 1) 
        #
        #                                If the C order shape of 3D data is 
        #                                    (number of longitudes, number of times, number oflatitudes)
        #                                submit
        #                                    (0, 2, 1, None) 
        #
        #                           Send in None if the shape is a subset of (time, level,
        #                           latitude, longitude) which is evaluated as follows:
        #                              3D -- code assumes (2,1,0,None)
        #                              4D -- code assumes (3,2,1,0)
        #
        #              missingValueOut -- the value for the missing data used in writing the output data. If left at the
        #                                 default entry, None, the code uses missingValueIn if present or as a last resort
        #                                 1.0e20
        #
        # 
        #    RETURNED : dataOut -- the regridded data
        #
        #                
        #    USAGE: 
        #                
        #          Example 1.  To regrid dataIn into dataOut using all the defaults where None, None signifies no
        #                      missing data.                   
        #              dataOut = x.rgrd(dataIn, None, None)    
        #
        #          Example 2.  To regrid dataIn into dataOut using 1.0e20 and greater as the missing data
        #                
        #                      dataOut = x.rgrd(dataIn, 1.e20, 'greater')    
        #
        #---------------------------------------------------------------------------------------------------------------------"""

        # check the required input -- dataIn, missingValueIn and  missingMatch

        # make sure that dataIn is an array

        try:
            z = len(dataIn)
        except TypeError:
            sendmsg('Error in calling the rgrd method -- dataIn must be an array')
            raise TypeError

        # check the missingValueIn pass

        if missingValueIn != None:
            try:
                z = abs(missingValueIn)
            except TypeError:
                sendmsg('Error in calling the rgrd method -- missingvalueIn must be None or a number. Now it is  ', missingValueIn)
                raise TypeError

        # check the missingMatch pass

        missingPossibilities = ['greater', 'equal', 'less', None]
        if missingMatch not in missingPossibilities:
            msg = 'Error in missingMatch -- it must be None or the string greater, equal, or less. Now it is '
            sendmsg(msg, missingMatch)
            raise ValueError

        # --- Check data type and change to float if necessary ----

        if dataIn.dtype.char != 'f':
            dataIn = dataIn.astype(numpy.float32)

        dataShape = dataIn.shape
        numberDim = len(dataShape)

        if numberDim < 2: 
            msg = 'Error in call to rgrd -- data must have at least 2 dimensions'
            sendmsg(msg)
            raise TypeError

        # --- evaluate positionIn ----
        
        # --- make standard positionIn as a check----
        positionList =[]
        for n in range(numberDim):              # insert a sequence of numbers
            positionList.append(n)
        positionList.reverse()

        for n in range(numberDim, 4):            # fill end of list with Nones
            positionList.append(None)

        positionCheck = tuple(positionList)      


        standardPosition = 0                            # transpose required

        if positionIn == None:                          # construct the default positionIn tuple
            positionIn = positionCheck
            standardPosition = 1                        # no need for a transpose with this data
        else:
            if positionIn == positionCheck:             # compare to the standard
                standardPosition = 1                    # no need for a transpose with this data

        if len(positionIn) != 4: 
            msg = 'Error in call to rgrd -- positionIn must be a tuple of length 4'
            sendmsg(msg)
            raise TypeError

        if standardPosition == 0:                        # transpose data to the standard order (t,z,y,x)

            newOrder, inverseOrder = checkorder(positionIn)

            dataIn = numpy.transpose(dataIn, newOrder)                    # transpose data to standard order (t,z,y,x)
            dataIn = numpy.array(dataIn.astype(numpy.float32), numpy.float32)       # make contiguous 


        # set dimension sizes and check for consistency 

        if positionIn[0] != None: 
            self.nlon = (dataShape[ positionIn[0] ]) 
        else:
            self.nlon = 0 
        if positionIn[1] != None: 
            self.nlat = (dataShape[ positionIn[1] ]) 
        else:
            self.nlat = 0 
        if positionIn[2] != None: 
            if self.nlevi != (dataShape[ positionIn[2] ]): 
                msg = 'Level size is inconsistent with input data'
                sendmsg(msg)
                raise ValueError
        if positionIn[3] != None: 
            self.ntime = (dataShape[ positionIn[3] ]) 
        else:
            self.ntime = 0 

        # allocate memory for dataOut -- the array with new number of levels

        outList = list(dataIn.shape)

        for i in range(len(outList)):
            if outList[i] == self.nlevi:
                outList[i] = self.nlevo
                break

        dataOut = numpy.zeros(tuple(outList), numpy.float32)                      # memory for aout


        if missingMatch == None:                                                # if no missing do not pass None
            missingMatch = 'none'

        if missingValueIn == None:                                                # if no missing do not pass None
            missingValueIn = 1.333e33

        if logYes != 'yes':
            logYes = 'no'

        levIn = self.axisIn[:].astype(numpy.float64)
        levOut = self.axisOut[:].astype(numpy.float64)
        _regrid.rgdpressure(self.nlevi, self.nlevo, self.nlat, self.nlon, self.ntime, missingValueIn, missingMatch, logYes, levIn, levOut, dataIn, dataOut)  

        if missingMatch == 'none':                                                # if no missing do not pass None
            missingMatch = None
        if missingValueIn == 1.333e33:              
            missingValueIn = None

        if standardPosition == 0:
            dataOut = numpy.transpose(dataOut, inverseOrder)                                   # transpose data to original order
            dataOut = numpy.array(dataOut.astype(numpy.float32), numpy.float32)            # make contiguous 

        if missingValueOut != None:                # set the missing value in data to missingValueOut

            if missingMatch == 'greater': 
                if missingValueIn > 0.0: 
                    missing = 0.99*missingValueIn
                else: 
                    missing = 1.01*missingValueIn

                dataOut = numpy.where(numpy.greater(dataOut,missing), missingValueOut, dataOut)

            elif missingMatch == 'equal': 
                missing = missingValueIn
                dataOut = numpy.where(numpy.equal(dataOut,missing), missingValueOut, dataOut)

            elif missingMatch == 'less': 
                if missingValueIn < 0.0: 
                    missing = 0.99*missingValueIn
                else: 
                    missing = 1.01*missingValueIn

                dataOut = numpy.where(numpy.less(dataOut,missing), missingValueOut, dataOut)

        return dataOut 

def checkorder(positionIn): 

    """    #-----------------------------------------------------------------------------------------
    #                                      
    #     purpose: construct the tuples for transposing the data to standard dimension order and the
    #              inverse for transposing it back to the original dimension order
    #
    #     usage:   newOrder, inverseOrder = checkorder(positionIn)
    #
    #     passed:  positionIn -- array with location of longitude, latitude. level and time respectively
    #                            in the sense of the python shape of the data
    #
    #     returned: newOrder -- tuple to transpose data to the order (t,z,y,x) 
    #               inverseOrder -- tuple to transpose data to back to the original order 
    #
    #----------------------------------------------------------------------------------------------"""

    # remove the None values from positionIn and reverse the order

    reducedPosition = [ ]
    for item in positionIn:
        if item != None:
            reducedPosition.append(item)
    reducedPosition.reverse()

    # make the newOrder tuple

    newOrder = tuple(reducedPosition)

    # -----  Determine the inverse to this new order for use in mathtogeo -----  

    xform = []
    for i in range(len(newOrder)):
        xform.append( [newOrder[i], i] )
    xform.sort()

    inverse_shapelist = []
    for item in xform:
        inverse_shapelist.append(item[1])
    inverseOrder = tuple(inverse_shapelist)


    return newOrder, inverseOrder

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
    if value1 == None:
        print msg
    elif value2 == None:
        print msg, value1
    else:
        print msg, value1, value2
    print '*******************************************************************'

    return None

