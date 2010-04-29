########################################################################
#
# Copyright (c) 2008, Lawrence Livermore National Security, LLC.  
# Produced at the Lawrence Livermore National Laboratory  
# Written by bremer5@llnl.gov,pascucci@sci.utah.edu.  
# LLNL-CODE-406031.  
# All rights reserved.  
#   
# This file is part of "Simple and Flexible Scene Graph Version 2.0."
# Please also read BSD_ADDITIONAL.txt.
#   
# Redistribution and use in source and binary forms, with or without
# modification, are permitted provided that the following conditions are
# met:
#   
# @ Redistributions of source code must retain the above copyright
#   notice, this list of conditions and the disclaimer below.
# @ Redistributions in binary form must reproduce the above copyright
#   notice, this list of conditions and the disclaimer (as noted below) in
#   the documentation and/or other materials provided with the
#   distribution.
# @ Neither the name of the LLNS/LLNL nor the names of its contributors
#   may be used to endorse or promote products derived from this software
#   without specific prior written permission.
#   
#  
# THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
# "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
# LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
# A PARTICULAR PURPOSE ARE DISCLAIMED.  IN NO EVENT SHALL LAWRENCE
# LIVERMORE NATIONAL SECURITY, LLC, THE U.S. DEPARTMENT OF ENERGY OR
# CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL,
# EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO,
# PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR
# PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF
# LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING
# NEGLIGENCE OR OTHERWISE) ARISING
#
########################################################################

from pyvisus.core import *
from pyvisus.data import *
from pyvisus.extract import VisusAxisAlignedExtractor
import cdtime
from cdms2 import tvariable
import numpy
from pyvisus.numpyconversion import *

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
# Try to automatically extract the data attributes from the data descriptor
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
def extractDataAttributes(data,attributes=dict()):

  """

  This function extracts as much information about the given data set
  as possible and store it in the attributes array. However, the
  function will not override existing attributes assuming that the
  caller had access to additional information. Possible attributes are

  name     : Verbal description of the data set or variable
  var_name : Short name of the variable  
  domain   : Physical domain of the data set represented as
             [[min_x,max_x],[min_y,max_y],[min_z,[max_z]]. If
             the data is only two- or one-dimensional the extra
             dimensions will be [0,0]
  samples  : The number of samples in each dimension represented
             as [samples_x, samples_y, samples_z, samples_t]. For
             lower dimensional data the additional dimensions will
             be set to 1, e.g. a time-dependent 2D array would be
             [dim_x,dim_y,1,dim_t]
  units    : VisusUnit identfier describing the natural coordinate
             system of the data (e.g. cartesian length, polar
             coordinates, etc.) All possible values can be found in
             VisusUnits.h
  range    : Local function range of the data. Note, that percentages
             and fractions will automatically get a range of [0,100]
             and [0,1] respectively.
  time     : The cdtime.Comptime of the first time step in data
  """

  # Try to determine the name of the data
  if "name" not in attributes: # unless the name is pre-defined
    try:
      name = data.standard_name # try the standard name
    except AttributeError:
      try:
        name = data.long_name # then the long
      except AttributeError:
        try:
          name = data.name # finally the short name
        except AttributeError:
          name = "unknown" # or indicate we could not find a name
    attributes["name"] = name


  # Try to determine the short name of the variable
  if "var_name" not in attributes:    
    try:
      attributes["var_name"] = data.name
    except:
      attributes["var_name"] = "unkown"

  # Try to determine the domain size
  if "domain" not in attributes:
    try:
      axis = data.getDomain() # get the description of the axis

      # For each axis get the first and last data value. Note that these
      #are not necessarily order. An axis could be given inverted
      domain = [[a[0].getData()[0],a[0].getData()[-1]] for a in axis]
      domain.reverse() # reverse the ordering to align with Visus ordering
      if axis[0][0].isTime(): # #if the first axis is a time axis
        domain[-1] = [0,0] # set a default value instead
        
      domain +=  [[0,0]] * (3-len(domain)) # add empty dimensions if necessary
      attributes["domain"] = domain
    except AttributeError:
      pass

  # Try to determine the number of samples in x,y,z, and time
  if "samples" not in attributes:
    shape = list(data.shape) # make a copy of the shape array
    shape.reverse() # reverse the ordering to align with Visus ordering

    try: # try whether we can check for a time axis
      if data.getAxis(0).isTime(): #if the first axis was a time axis
        shape = shape[:-1] + [1]*(4-len(shape)) + [shape[-1]]
      else:
        shape = shape + [1]*(4-len(shape))
    except:
        shape = shape + [1]*(4-len(shape))
        
    attributes["samples"] = shape

  # Try to determine the physical units of the domain
  if "units" not in attributes:

    try:
      x_axis = data.getAxisList()[-1] # Get the last axis which is the x-axis


      #print "Units ", x_axis.units
      # If the units description contains the word "degrees" assume the data
      # is in lat,long coordinates 
      if "degrees" in x_axis.units:
        unit = VISUS_POLAR_DEGREES
      else: # Otherwise we assume we are in standard length scale
        unit = VISUS_METERS
    except:
      unit = VISUS_METERS

    attributes["units"] = unit
        

  # Try to determine the range of the field
  if "range" not in attributes:

    try:
      if data.units in ["percent","%"]:
        range = [0,100]
      elif data.units == "fraction":
        range = [0,1]
      else:
        range = [data.min(),data.max()]
      attributes["range"] = range
    except:
      pass
      
  # Try to determine a date for the first time step
  if "time" not in attributes:

    try:
      if data.getAxis(0).isTime():
        
        time = data.getAxis(0).getData()
        
        attributes["time"] = [time[0],time[1]]
    except:
      pass

      
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
# Create a description of the data
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
def constructDataDescriptor(data,attributes = dict(),**keywords):

  attributes.update(keywords)
  
  # Try to complete the data attributes to the best of our knowlege
  extractDataAttributes(data,attributes)

  # Create a view of the data that conforms to the shape Visus expects
  new_shape = list(attributes["samples"])
  new_shape.reverse()
  data_view = data.reshape(new_shape)

  if isinstance(data_view,tvariable.TransientVariable):
    data_view = data_view.data
  

  # If we were not given a domain
  if "domain" in attributes:

    # Sometimes the domain might describe the data in reverse order,
    # for example, the height coordinate is often given this
    # way. However, Visus only deals with data ordered "normally."
    # Therefore, we check for possible inversions here and if
    # necessary invert the data
    domain = attributes["domain"]
    if domain[0][0] > domain[0][1]:
      domain[0].reverse()
      data_view = data_view[::-1]
      
    if domain[1][0] > domain[1][1]:
      domain[1].reverse()
      data_view = data_view[:,::-1]
      
    if domain[2][0] > domain[2][1]:
      domain[2].reverse()
      data_view = data_view[:,:,::-1]

    attributes["domain"] = domain

  data_descriptor = "Incore: "

  samples = attributes["samples"]
  data_descriptor += "%d %d %d %d " % (samples[0],samples[1],samples[2],samples[3])
  
  if "units" in attributes:
    data_descriptor += "%d " % attributes["units"]
  else:
    data_descriptor += "0 "
    
  if "domain" in attributes:
    domain = attributes["domain"]
    domain[2][1] = domain[2][1] * 50
    data_descriptor += "domain %f %f %f %f %f %f " % (domain[0][0],domain[1][0],domain[2][0],
                                                      domain[0][1],domain[1][1],domain[2][1])

  if "time" in attributes:
    time = attributes["time"]
    data_descriptor += "time %d %f %f " % (0,time[0],time[1])
    
  if data_view.dtype in [numpy.int8,numpy.uint8,numpy.byte,numpy.ubyte]:
    size = 1
    type = PV_UCHAR
  elif data.dtype in [numpy.float64]:
    size = 8
    type = PV_FLOAT64
  elif data.dtype in [numpy.float32]:
    size = 4
    type = PV_FLOAT32
  else:
    raise ValueError

  # If the array is not a C-style array we have no choice but to copy the data
  if not isCArray(data_view):
    data_view = numpy.array(data_view)
    if not isCArray(data_view):
      raise ValueError

  # Extract the pointer to the C-array
  pointer = arrayPointer(data_view)
  if pointer is 0:
    raise ValueError, "Could not encode array as pointer"
    
  field = " { "

  if "name" in attributes:
    field += "%s " % attributes["name"].replace(" ","_")
  else:
    field += "unknown "

  field += "%d %d %d " % (pointer,type,size)

  if "range" in attributes:
    field += "range %f %f " % (attributes["range"][0],attributes["range"][1])


  if "mask" in attributes:

    mask = attributes["mask"] 

    #new_shape = list(attributes["samples"])
    #new_shape.reverse()
    #print mask
    #print data_view[0]
    #print mask.shape,data_view.shape

    # We only need one mask not one maks per time so the mask can skip
    # the first dimension. Note that this of course does not work for
    # 2D + time yet but it is not clear how to distinguish this from
    # 3D without time. For now we just assume the user always gives us
    # 4D cooridnates some of which may be 1
    mask_shape = data_view.shape
    if len(mask_shape) > 3:
      mask_shape = mask_shape[1:]

    # This is a hack for a 2D mask applied to a 3D Data set
    if len(mask.shape) < len(mask_shape):
      mask = numpy.expand_dims(mask,axis=0)
      mask = numpy.tile(mask,[mask_shape[0],1,1])
    
    
    mask_view = mask.reshape(mask_shape)
    if not isCArray(mask):
      mask_view = numpy.array(mask_view)
      #mask = mask.ascontiguousarray()
      
    pointer = arrayPointer(mask_view)
    if pointer is 0:
      raise ValueError, "Could not encode array as pointer"

    field += " mask %d " % pointer


  field += "} "
  

  data_descriptor += field
    

  if not "mask" in attributes:
    #print data_descriptor
    return VisusDataDescription(data_descriptor,data_view)
  else:
    #data_descriptor += " mask 1 "
    #print data_descriptor
    return VisusDataDescription(data_descriptor,data_view,mask_view)


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
# Create a description of the CDMS data
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#    
def constructCDMSDescription(file,var,attributes = dict(),**keywords):

  attributes.update(keywords)
  
  variable = file.getVariable(var)

  # First we extract the very first time step
  if variable.getAxis(0).isTime():
    data = variable[0]
    bounds = variable.getAxis(0).getBounds()
    if not 'time' in attributes:
      tbounds = [bounds[0][0],bounds[-1][1]]
      attributes['time'] = tbounds
  else:
    data = file(var)

  
  # Try to complete the data attributes to the best of our knowlege
  extractDataAttributes(data,attributes)

  # Create a view of the data that conforms to the shape Visus expects
  new_shape = list(attributes["samples"])
  new_shape.reverse()
  data_view = data.reshape(new_shape)

  if isinstance(data_view,tvariable.TransientVariable):
    data_view = data_view.data
  

  data_descriptor = "Managed: "

  loader = VisusCDMSLoader()
  dir(loader)
  data_descriptor += "%d " % loader.objectPointer()
  data_descriptor += "%d " % variable.getAxis(0).shape[0]
  data_descriptor += "0 %f %f " % (tbounds[0],tbounds[1])


  # If we were not given a domain
  if "domain" in attributes:

    # Sometimes the domain might describe the data in reverse order,
    # for example, the height coordinate is often given this
    # way. However, Visus only deals with data ordered "normally."
    # Therefore, we check for possible inversions here and if
    # necessary invert the data
    domain = attributes["domain"]
    if domain[0][0] > domain[0][1]:
      domain[0].reverse()
      data_view = data_view[::-1]
      
    if domain[1][0] > domain[1][1]:
      domain[1].reverse()
      data_view = data_view[:,::-1]
      
    if domain[2][0] > domain[2][1]:
      domain[2].reverse()
      data_view = data_view[:,:,::-1]

    attributes["domain"] = domain

  samples = attributes["samples"]
  data_descriptor += "%d %d %d %d " % (samples[0],samples[1],samples[2],samples[3])
  
  if "units" in attributes:
    data_descriptor += "%d " % attributes["units"]
  else:
    data_descriptor += "0 "
    
  if "domain" in attributes:
    domain = attributes["domain"]
    data_descriptor += "domain %f %f %f %f %f %f " % (domain[0][0],domain[1][0],domain[2][0],
                                                      domain[0][1],domain[1][1],domain[2][1])

  if "time" in attributes:
    time = attributes["time"]
    data_descriptor += "time %d %f %f " % (0,time[0],time[1])
    
  if data_view.dtype in [numpy.int8,numpy.uint8,numpy.byte,numpy.ubyte]:
    size = 1
    type = PV_UCHAR
  elif data.dtype in [numpy.float64]:
    size = 8
    type = PV_FLOAT64
  elif data.dtype in [numpy.float32]:
    size = 4
    type = PV_FLOAT32
  else:
    raise ValueError

  # If the array is not a C-style array we have no choice but to copy the data
  if not isCArray(data_view):
    data_view = numpy.array(data_view)
    if not isCArray(data_view):
      raise ValueError

  # Extract the pointer to the C-array
  pointer = arrayPointer(data_view)
  if pointer is 0:
    raise ValueError, "Could not encode array as pointer"
    
  field = " { "

  if "name" in attributes:
    field += "%s " % attributes["name"].replace(" ","_")
  else:
    field += "unknown "

  field += "%d %d %d " % (pointer,type,size)

  if "range" in attributes:
    field += "range %f %f " % (attributes["range"][0],attributes["range"][1])


  if "mask" in attributes:

    mask = attributes["mask"] 

    #new_shape = list(attributes["samples"])
    #new_shape.reverse()
    #print "MASK"
    #print mask.shape,data_view.shape

    # We only need one mask not one maks per time so the mask can skip
    # the first dimension. Note that this of course does not work for
    # 2D + time yet but it is not clear how to distinguish this from
    # 3D without time. For now we just assume the user always gives us
    # 4D cooridnates some of which may be 1
    mask_shape = data_view.shape
    if len(mask_shape) > 3:
      mask_shape = mask_shape[1:]
    
    mask_view = mask.reshape(mask_shape)
    if not isCArray(mask):
      mask_view = numpy.array(mask_view)
      #mask = mask.ascontiguousarray()
      
    pointer = arrayPointer(mask_view)
    if pointer is 0:
      raise ValueError, "Could not encode array as pointer"

    field += " mask %d " % pointer


  field += "} "
  

  data_descriptor += field
    

  if not "mask" in attributes:
    #print data_descriptor
    return VisusDataDescription(data_descriptor,data_view)
  else:
    #data_descriptor += " mask 1 "
    #print data_descriptor
    return VisusDataDescription(data_descriptor,data_view,mask_view)
    

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
def constructSliceExtractor(data_description):
  
  left = VectorDouble(3)
  right= VectorDouble(3)
  
  source = VisusDataSourceFactory.make(data_description)
  if not source.isValid():
    raise RuntimeError("Problem loading data set")

  source.domainBoundingBox(left,right)
  bbox = VisusBoundingBox()
  bbox.set(left,right)
           
  request = VisusDataRequest()

  extent = VectorDouble(3)  
  extent[0] = (right[0] - left[0]) 
  extent[1] = (right[1] - left[1])  
  extent[2] = 0  

  request.extent(extent)

  matrix = translationMatrix((left[0]+right[0])/2,
                             (left[1]+right[1])/2,
                             (left[2]+right[2])/2,
                             )
  request.transformation(matrix)
  request.requestType(VISUS_2D_REQUEST);

  start = [ 1, 1, 1 ]
  end   = [ 1, 1, 1 ]
  request.setStrides(start,end)
  
  extractor = VisusAxisAlignedExtractor.construct()

  extractor.setValue(data_description)
  extractor.setValue(request)
  extractor.setValue(bbox) 
  extractor.drawBoundingBox(False)
  extractor.visible(False)

  return extractor

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
# Create the 3d data extractor
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
def create3dExtractor(data,attributes=dict()):

  # Try to infer as many data attributes as you can and return a
  # string describing/defining the data in the ViSUS internal format
  data_description = constructDataDescriptor(data,attributes)

  #print "data: ", attributes["units"]

  # Create a ViSUS data source based on the data description created earlier
  source = VisusDataSourceFactory.make(data_description)
  if not source.isValid():
    raise RuntimeError("Problem loading data set")
    
  # Get the corners of the bounding box
  left = VectorDouble(3)
  right = VectorDouble(3)
  
  # Get the bounding box of the data
  source.domainBoundingBox(left, right)
  bbox = VisusBoundingBox()
  bbox.set(left, right)

  # Create a data request (empty)
  request = VisusDataRequest()
  
  # Create the extent of the data request
  extent = VectorDouble(3)  
  extent[0] = (right[0] - left[0]) # We want to see all of the x-coordinate
  extent[1] = (right[1] - left[1]) # and all of the y-coordinate 
  extent[2] = (right[2] - left[2]) + 10
   
  #print 'extent[0]=', extent[0]
  #print 'extent[1]=', extent[1]
  #print 'extent[2]=', extent[2]
 # But we want to look at a plane so the z extend is 0

  # Pass the extent information into the request
  request.extent(extent)
  
  # Translate the extent request to the middle of the bbox
  matrix = translationMatrix((left[0]+right[0])/2,
                             (left[1]+right[1])/2,
                             (left[2]+right[2])/2, 
                             )

  # And pass this matrix to the request
  request.transformation(matrix)

  # Set the request to be a 3D slice
  request.requestType(VISUS_3D_REQUEST);

  # Set the resolution of the data
  start = [ 2, 2, 2 ]
  end = [ 2, 2, 2 ]
  request.setStrides(start, end)

  # Create a node that will extract the data
  extractor = VisusAxisAlignedExtractor.construct()
  
  # Set the data to extract, the request and the bbox
  extractor.setValue(data_description)
  extractor.setValue(request) 
  extractor.setValue(bbox)
  extractor.drawBoundingBox(False)
  extractor.visible(False)
  
  return extractor, bbox, data_description 

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
# Create the data extractor
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
def createExtractor(data,attributes=dict()):

  # Try to infer as many data attributes as you can and return a
  # string describing/defining the data in the ViSUS internal format
  data_description = constructDataDescriptor(data,attributes)
  
  # Create a ViSUS data source based on the data description created earlier
  source = VisusDataSourceFactory.make(data_description)
  if not source.isValid():
    raise RuntimeError("Problem loading data set")
    
  # Get the corners of the bounding box
  left = VectorDouble(3)
  right = VectorDouble(3)
  
  # Get the bounding box of the data
  source.domainBoundingBox(left, right)
  bbox = VisusBoundingBox()
  bbox.set(left, right)

  # Create a data request (empty)
  request = VisusDataRequest()
  
  # Create the extent of the data request
  extent = VectorDouble(3)  
  extent[0] = (right[0] - left[0]) # We want to see all of the x-coordinate
  extent[1] = (right[1] - left[1]) # and all of the y-coordinate 
  extent[2] = 0  # But we want to look at a plane so the z extend is 0

  # Pass the extent information into the request
  request.extent(extent)
  
  # Translate the extent request to the middle of the bbox
  matrix = translationMatrix((left[0]+right[0])/2,
                             (left[1]+right[1])/2,
                             (left[2]+right[2])/2, 
                             )

  # And pass this matrix to the request
  request.transformation(matrix)

  # Set the request to be a 2D slice
  request.requestType(VISUS_2D_REQUEST);

  # Set the resolution of the data
  start = [ 1, 1, 1 ]
  end = [ 1, 1, 1 ]
  request.setStrides(start, end)

  # Create a node that will extract the data
  extractor = VisusAxisAlignedExtractor.construct()
  
  # Set the data to extract, the request and the bbox
  extractor.setValue(data_description)
  extractor.setValue(request) 
  extractor.setValue(bbox)
  extractor.drawBoundingBox(False)
  extractor.visible(False)
  
  return extractor, bbox, attributes, data_description


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
# Create the height data extractor
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
def createHeightExtractor(data, heightdata, attributes=dict(), heightattri=dict()):

  # Try to infer as many data attributes as you can and return a
  # string describing/defining the data in the ViSUS internal format
  data_description = constructDataDescriptor(data,attributes)
  heightdata_description = constructDataDescriptor(heightdata,heightattri)
  
  #print "data: ", attributes["units"]
  #print "attributes", attributes
  # Create ViSUS data sources based on the data description created earlier
  source = VisusDataSourceFactory.make(data_description)
  heightsource = VisusDataSourceFactory.make(heightdata_description)
  
  if not source.isValid():
    raise RuntimeError("Problem loading data set")
    
  # Get the corners of the bounding box
  left = VectorDouble(3)
  right = VectorDouble(3)
  
  # Get the bounding box of the data
  source.domainBoundingBox(left, right)
  bbox = VisusBoundingBox()
  bbox.set(left, right)

  # Create a data request (empty)
  request = VisusDataRequest()
  
  # Create the extent of the data request
  extent = VectorDouble(3)  
  extent[0] = (right[0] - left[0]) # We want to see all of the x-coordinate
  extent[1] = (right[1] - left[1]) # and all of the y-coordinate 
  extent[2] = 0  # But we want to look at a plane so the z extend is 0

  # Pass the extent information into the request
  request.extent(extent)
  
  # Translate the extent request to the middle of the bbox
  matrix = translationMatrix((left[0]+right[0])/2,
                             (left[1]+right[1])/2,
                             (left[2]+right[2])/2, 
                             )

  # And pass this matrix to the request
  request.transformation(matrix)

  # Set the request to be a 2D slice
  request.requestType(VISUS_2D_REQUEST);

  # Set the resolution of the data
  start = [ 1, 1, 1 ]
  end = [ 1, 1, 1 ]
  request.setStrides(start, end)

  # Create nodes that will extract the texture and height data
  extractor1 = VisusAxisAlignedExtractor.construct()
  extractor2 = VisusAxisAlignedExtractor.construct()
  
  # Set the texture data to extract, the request and the bbox
  extractor1.setValue(heightdata_description)
  extractor1.setValue(request) 
  extractor1.setValue(bbox)
  extractor1.drawBoundingBox(False)
  extractor1.visible(False)
  
  # Set the height data to extract, the request and the bbox
  extractor2.setValue(data_description)
  extractor2.setValue(request) 
  extractor2.setValue(bbox)
  extractor2.drawBoundingBox(False)
  extractor2.visible(False)
  
  return extractor1, extractor2, bbox, attributes, heightattri, data_description, heightdata_description
  
  
