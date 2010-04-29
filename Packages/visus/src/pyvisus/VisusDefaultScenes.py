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


from os.path import join

import pyvisus
from pyvisus.gui import *
from pyvisus.core import *
from pyvisus.component import *
from pyvisus.data import *
from pyvisus.display import VisusOrthogonalSlice,VisusEarthNode,VisusIndexedDisplay
from pyvisus.extract import VisusAxisAlignedExtractor
from pyvisus.shared import VisusSharedDataRequest
from pyvisus.numpyconversion import *

import cdms2
import cdtime
import numpy
import time
import threading
from cdms2 import tvariable

def printBox(label, box):
  print label," box: ", box[0], box[1], box[2], box[3], box[4], box[5]
  return

def constructWorldBox(data_box):
  box = VisusBoundingBox()

  #printBox("Input",data_box)

  scale_factor = 10e34

  for i in xrange(0,3):
    if data_box[i] != data_box[i+3]:
      tmp = 10.0 / (data_box[i+3] - data_box[i])
      if tmp < scale_factor:
        scale_factor = tmp

  if scale_factor == 10e34:
    scale_factor = 1


  for i in xrange(3): 
    box[i]   = -(data_box[i+3] - data_box[i]) * scale_factor / 2.0
    box[i+3] = +(data_box[i+3] - data_box[i]) * scale_factor / 2.0 

  #printBox("Output",box)

  return box;

def loadWorldMap():
  import cdutil.worldmap as worldmap

  outlines = []
  for area in dir(worldmap):
    
    if area[:4]=='farc':
      exec('area = worldmap.'+area)

      line = []
      n=len(area)
      for i in range(0,n,2):
        line += [float(area[i+1])/100.,float(area[i])/100.]
        if i!=0:
          if line[-2] < 0 and line[-4] > 177.5:
            line[-2] += 360
            
            outlines.append(line)
            line = list([line[-2]-360,line[-1]])
            
          elif line[-4] < 0 and line[-2] > 177.5:
            line[-2] -= 360
            
            outlines.append(line)
            line = list([line[-2]+360,line[-1]])
            

            
      outlines.append(line)

  return outlines


def guiDefinedData():
    """
    This function attempts to grab all information the user has entered
    into the vcdat gui. If succesful the function will open the file
    and return the approriate array as cdms2 variable.
    """

    try:
        import gui_support
        root = gui_support.root()
        panel = root.panelDM        
    except:
        print "Could not find gui root. VCDAT seems not to be running"
        raise
    
    
    import browser.gui_formulate

    return browser.gui_formulate.data(root,d_name = panel.var3)
    

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
        
        time = data.getAxis(0).getData()[0]
        unit = data.getAxis(0).units
        time = cdtime.reltime(time,unit)
        time = time.tocomp()
        attributes["time"] = time
    except:
      raise

      
  
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
    data_descriptor += "domain %f %f %f %f %f %f " % (domain[0][0],domain[1][0],domain[2][0],
                                                      domain[0][1],domain[1][1],domain[2][1])
    
    
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

  field += "} "
  
  data_descriptor += field

  return VisusDataDescription(data_descriptor,data_view)

def sliceHierarchy(data,**keywords):
    
  attributes = keywords
  if "units" not in attributes:
    attributes["units"] = VISUS_METERS

  # Try to infer as many data attributes as you can and return a
  # string describing/defining the data in the ViSUS internal format
  data_description = constructDataDescriptor(data,attributes)

  # Construct a root node for the scene graph hierarchy. This node is
  # mainly used for the global transformation (e.g. rotating the
  # complete scene) and is a convinient way to collect independent
  # nodes (nodes that should have no ancestor-descendend
  # relationship. Note, the use of the construct function rather than
  # a standard "constructor." ViSUS uses a smart pointer / reference
  # counting scheme which requires this.
  root = VisusGroup.construct()

  # To have a convinient reference point we want to display the
  # bounding box of the root node which will automatically adjust to
  # include the bounding box of all child nodes.  
  root.drawBoundingBox(True)

  # Create two points that will form the left lower and right upper
  # corner of the data bounding box
  left = VectorDouble(3)
  right= VectorDouble(3)

  # Create a ViSUS data source based on the data description created earlier
  source = VisusDataSourceFactory.make(data_description)
  if not source.isValid():
    raise RuntimeError("Problem loading data set")

  # Get the bounding box of the data whose value may or may not depend
  # on data attributes such as units. In particular, if this
  # information was available the bounding box will be given in
  # pyhsical units rather than in index space
  source.domainBoundingBox(left,right)
  bbox = VisusBoundingBox()
  bbox.set(left,right);
  
  # The ViSUS system is based on data "requests" that describe a
  # particular piece of data that should be displayed. The data
  # requests can be manipulated through the user interface or
  # directly. However, here we want to create a sensible default
  # request to see a portion of our data.
  
  # First, we create an empty request 
  request = VisusDataRequest()

  # Second, we construct an array that describes the length in each
  # dimension of the box of data we want to extract in whatever units
  # the data source uses (typically physical space).
  extent = VectorDouble(3)  
  extent[0] = (right[0] - left[0]) # We want to see all of the x-coordinate
  extent[1] = (right[1] - left[1]) # and all of the y-coordinate 
  extent[2] = 0  # But we want to look at a plane so the z extend is 0

  # And we pass the extent information into the request
  request.extent(extent)

  # Second, we need to position our extent box in the data
  # set. Initially, the extent will be centered around the origin. To
  # see the middle of the data we want translate our request to the
  # center of the data's bounding box

  # So we create the corresponding transformation matrix
  matrix = translationMatrix((left[0]+right[0])/2,
                             (left[1]+right[1])/2,
                             (left[2]+right[2])/2,
                             )
  # And pass this matrix to the request
  request.transformation(matrix)
  
  # Third, we want to indicate to the ViSUS system that we are
  # really interested in a 2D slice of the data (rather than a
  # degenerate volume) as slices are a special case and implement some
  # special user interactions
  request.requestType(VISUS_2D_REQUEST);

  # Finally, we describe the resolution at which we want to see our
  # data. The resolution is defined in terms ofo sub-sampling
  # "strides" where a stride of 1 defines the full resolution data a
  # stride of 2, the data sub-sampled by a factor of 2 etc.. Strides
  # must be powers of two but can be independently set for each
  # dimension. Furthermore, for large data it can be useful to
  # progressively refine the data starting at some higher strides /
  # coarser resolution, ending at some lower strides / finer
  # resolution. This preserves interactivity while still providing
  # high resolution data if the request remains unchanged long enough

  # Since, for default scenes we expect the data to be small we only
  # want to see the highest resolution
  start = [ 1, 1, 1 ]
  end   = [ 1, 1, 1 ]
  request.setStrides(start,end)
  

  # Armed with the description of the data and the request we just
  # constructed we create a scene graph node that will extract the
  # data for us. This node, will continuously test its current request
  # and update the data accordingly. Therefore, one could adapt the
  # scene by simply passing different requests to this node (which is
  # what the graphical user interface does on mouse movements or
  # keyboard commands)
  
  # Create a producer node specialized to extract and axis aligned
  # bounding box from a data set. As before with the root node the
  # node must be explicitly constructed.
  extractor = VisusAxisAlignedExtractor.construct()

  # Set the data to extract our data from
  extractor.setValue(data_description)
  extractor.setValue(request) # Our default request
  extractor.setValue(bbox); # And the bounding box 
  extractor.drawBoundingBox(False);


  # Now that we have a node that will extract the data we want to see
  # we construct another node to visualize the slice

  # We want to view an axis aligned slice of the data an the
  # appropriate visualization node is one for orthogonal slices
  slice = VisusOrthogonalSlice.construct()

  # Now we connect the output of the extractor to the input of the
  # visualization node
  if not slice.connectInput(extractor):
    raise RuntimeError("unable to connect extractor as slice input")

  # For reasons internal to the underpinnings of the ViSUS system the
  # visualization node must know about the current data request. The
  # simplest way of achieving this is to attach the slice node as
  # child to the extractor 
  extractor.attachSubTree(slice,True)

  # and to inherit/share the request 
  slice.inherit(VisusSharedDataRequest.typeIndex(),True)

  # In this configuration the slice node will naturally draw the data
  # at the position of the data request which is the natural
  # place. However, by manipulating the transformation matrix of the
  # slice node we could draw the data anywhere in space / on the screen
   
  # Instead of the default greyscale color map we want to use a
  # typical red-to-blue map
  color_map = banded()
  slice.setValue(color_map)

  # In order to see the slice even if it contains no data (if the
  # request has been moved outside the data set, or the data is
  # extremly large) we draw the bounding box of the slice
  slice.drawBoundingBox(True);

  # Finally, we attach the whole subtree that renders the data plus
  # annotation to the root node
  root.attachSubTree(extractor,True)

  # Using the current bounding box of the scene we adapt a default box
  # to the aspect ratios of of our scene and use it as our "world
  # coordinate system"  
  world_box = constructWorldBox(bbox)
  root.setValue(bbox)

  # Now we make sure that the data is drawn in our standard coordinate
  # system
  root.mapToWorldBox(world_box)

  return root

def cartesianSlice(data=None,**keywords):

  """

    This function constructs a default ViSUS scene displaying a single
    z-slice through a regular block of data. The user can either pass a
    cdms2 array using the data attribute or enter the necessary
    information into the vcdat gui. If no data attribute is present the
    function will try to extract the necessary information from the gui
    and if that fails an empty scene will be displayed.

    The scene contains multiple \"nodes\" each of which can be
    manipulated using the keyboard and mouse. Som basic commands are

    Tab : Switch node
    Middle Mouse button : Pan scene
    Left Mouse button : Rotate scene
    Right Mouse button : Zoom in and out


    Usage: cartesianSlice(DataArray, keyword=value,...) where DataArray
    can be a standard numpy array, a cdms type data set, or
    \"None\". The function will attempt to extract a number of
    attributes from the given data or the gui each of which can be
    overridden by the user by passing it explicitly. Possible keywords
    are:

    name     : Verbal description of the data set or variable (used in the title)
    var_name : Short name of the variable  
    domain   : Physical domain of the data set represented as
               [[min_x,max_x],[min_y,max_y],[min_z,max_z]]. If
               the data is only two- or one-dimensional the extra
               dimensions will be assumed to be [0,0]
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
  
  # If the user did not pass in an array try to extract one from the
  # gui
  if data is None:
    data = guiDefinedData()
    
  attributes = keywords
  try:
    
    # Unless the user has explicitly specified different units we
    # treat the domain as a length box
    if "units" not in attributes:
      attributes["units"] = VISUS_METERS

    # Try to infer as many data attributes as you can and return a
    # string describing/defining the data in the ViSUS internal format
    data_description = constructDataDescriptor(data,attributes)
  except:
    print "Could not parse data. Unknown format"
    raise

  # Create a font to be used in all the nodes that render text
  font = VisusFont()
  font.fontSize(6)
  
  # Construct a root node for the scene graph hierarchy. This node is
  # mainly used for the global transformation (e.g. rotating the
  # complete scene) and is a convinient way to collect independent
  # nodes (nodes that should have no ancestor-descendend
  # relationship. Note, the use of the construct function rather than
  # a standard "constructor." ViSUS uses a smart pointer / reference
  # counting scheme which requires this.
  root = VisusGroup.construct()

  # To have a convinient reference point we want to display the
  # bounding box of the root node which will automatically adjust to
  # include the bounding box of all child nodes.  
  root.drawBoundingBox(True)

  # Create two points that will form the left lower and right upper
  # corner of the data bounding box
  left = VectorDouble(3)
  right= VectorDouble(3)

  # Create a ViSUS data source based on the data description created earlier
  source = VisusDataSourceFactory.make(data_description)
  if not source.isValid():
    raise RuntimeError("Problem loading data set")

  # Get the bounding box of the data whose value may or may not depend
  # on data attributes such as units. In particular, if this
  # information was available the bounding box will be given in
  # pyhsical units rather than in index space
  source.domainBoundingBox(left,right)
  bbox = VisusBoundingBox()
  bbox.set(left,right);
  
  # The ViSUS system is based on data "requests" that describe a
  # particular piece of data that should be displayed. The data
  # requests can be manipulated through the user interface or
  # directly. However, here we want to create a sensible default
  # request to see a portion of our data.
  
  # First, we create an empty request 
  request = VisusDataRequest()

  # Second, we construct an array that describes the length in each
  # dimension of the box of data we want to extract in whatever units
  # the data source uses (typically physical space).
  extent = VectorDouble(3)  
  extent[0] = (right[0] - left[0]) # We want to see all of the x-coordinate
  extent[1] = (right[1] - left[1]) # and all of the y-coordinate 
  extent[2] = 0  # But we want to look at a plane so the z extend is 0

  # And we pass the extent information into the request
  request.extent(extent)

  # Second, we need to position our extent box in the data
  # set. Initially, the extent will be centered around the origin. To
  # see the middle of the data we want translate our request to the
  # center of the data's bounding box

  # So we create the corresponding transformation matrix
  matrix = translationMatrix((left[0]+right[0])/2,
                             (left[1]+right[1])/2,
                             (left[2]+right[2])/2,
                             )
  # And pass this matrix to the request
  request.transformation(matrix)
  
  # Third, we want to indicate to the ViSUS system that we are
  # really interested in a 2D slice of the data (rather than a
  # degenerate volume) as slices are a special case and implement some
  # special user interactions
  request.requestType(VISUS_2D_REQUEST);

  # Finally, we describe the resolution at which we want to see our
  # data. The resolution is defined in terms ofo sub-sampling
  # "strides" where a stride of 1 defines the full resolution data a
  # stride of 2, the data sub-sampled by a factor of 2 etc.. Strides
  # must be powers of two but can be independently set for each
  # dimension. Furthermore, for large data it can be useful to
  # progressively refine the data starting at some higher strides /
  # coarser resolution, ending at some lower strides / finer
  # resolution. This preserves interactivity while still providing
  # high resolution data if the request remains unchanged long enough

  # Since, for default scenes we expect the data to be small we only
  # want to see the highest resolution
  start = [ 1, 1, 1 ]
  end   = [ 1, 1, 1 ]
  request.setStrides(start,end)
  

  # Armed with the description of the data and the request we just
  # constructed we create a scene graph node that will extract the
  # data for us. This node, will continuously test its current request
  # and update the data accordingly. Therefore, one could adapt the
  # scene by simply passing different requests to this node (which is
  # what the graphical user interface does on mouse movements or
  # keyboard commands)
  
  # Create a producer node specialized to extract and axis aligned
  # bounding box from a data set. As before with the root node the
  # node must be explicitly constructed.
  extractor = VisusAxisAlignedExtractor.construct()

  # Set the data to extract our data from
  extractor.setValue(data_description)
  extractor.setValue(request) # Our default request
  extractor.setValue(bbox); # And the bounding box 
  extractor.drawBoundingBox(False);


  # Now that we have a node that will extract the data we want to see
  # we construct another node to visualize the slice

  # We want to view an axis aligned slice of the data an the
  # appropriate visualization node is one for orthogonal slices
  slice = VisusOrthogonalSlice.construct()

  # Now we connect the output of the extractor to the input of the
  # visualization node
  if not slice.connectInput(extractor):
    raise RuntimeError("unable to connect extractor as slice input")

  # For reasons internal to the underpinnings of the ViSUS system the
  # visualization node must know about the current data request. The
  # simplest way of achieving this is to attach the slice node as
  # child to the extractor 
  extractor.attachSubTree(slice,True)

  # and to inherit/share the request 
  slice.inherit(VisusSharedDataRequest.typeIndex(),True)

  # In this configuration the slice node will naturally draw the data
  # at the position of the data request which is the natural
  # place. However, by manipulating the transformation matrix of the
  # slice node we could draw the data anywhere in space / on the screen
   
  # Instead of the default greyscale color map we want to use a
  # typical red-to-blue map
  color_map = banded()
  slice.setValue(color_map)

  # In order to see the slice even if it contains no data (if the
  # request has been moved outside the data set, or the data is
  # extremly large) we draw the bounding box of the slice
  slice.drawBoundingBox(True);

  # Now, we want to create labeled tickmarks to give the user a notion
  # of the data range. 

  # Create a tickmark node that by default will create tickmarks along
  # the x-axis of its bounding box
  xTickMarks = VisusTickMarks.construct()

  # ViSUS uses "axis" to control the labels of images, graphs, and
  # also tickmarks. Here we get the current axis 
  axis = xTickMarks.axis()
  axis.legendText("longitude") # Set the label 
  axis.labelFont(font)         # and font
  xTickMarks.axis(axis)        # and update the axis

  # Finally, we attach the tickmarks node as a child to the
  # extractor. By default a tickmarks node inherits the bounding box
  # of its parent and thus this node will now annotate the x-axis of
  # our data set
  extractor.attachSubTree(xTickMarks,True)
  
  # Create tickmarks for the y-axis in an identical fashion
  yTickMarks = VisusTickMarks.construct()
  
  # Except that now we must name the axis explicitly 
  yTickMarks.direction(TM_Y_AXIS)
  axis = yTickMarks.axis() 
  axis.legendText("latitude")
  axis.labelFont(font)
  yTickMarks.axis(axis)
  extractor.attachSubTree(yTickMarks,True)
  
  # Finally, we attach the whole subtree that renders the data plus
  # annotation to the root node
  root.attachSubTree(extractor,True)


  # To further enhance the scene we want to add the continental
  # outlines on top of the slice

  # First, we load the cdat internal world map 
  world_map = loadWorldMap()

  # And parse it as a collection of polylines. A VisusIndexedData
  # object stores an array of n-dimensional points (n being 2 in this
  # case) and a set of elements defined as ordered lists of indices
  # into this array. Therefore, each contour of the world map is
  # parsed as a single element
  outlines = VisusIndexedData(2)
  for m in world_map:
    outlines.constructElement(m)

  # Once the data is constructed we create a display node designed to visualize it
  map = VisusIndexedDisplay.construct()

  # And load the data. 
  map.loadData(outlines,0)

  # The cdat world map is given within (-180,-90)x(180,90). However,
  # in order to display the normal lat,long, elev data easily in
  # cartesian space we told ViSUS the data units are meters (meaning
  # general distance) which will draw (0,0)x(360,180). To adjust the
  # map to the slice we therefore must shift it
  #map.getValue(matrix);
  #matrix = matrix * translationMatrix(180,90,0.01)
  #map.setValue(matrix)

  # Attach the map as child of the extractor. Note, that from this moment
  # on the map will inherit all transformations from the extractor and
  # thus "stick" to the extractor if it is moved or rotated
  extractor.attachSubTree(map)
  
  # Lastly, we attach more annotations to the scene

  # A VisusLabelNode is a general one line text of arbitrary font and
  # size. First, we create a title string using the variable name if
  # we found one
  label_node = VisusLabelNode.construct()
  title = "Variable: " + attributes["name"].replace("_"," ")
  label_node.text(title)
  label_node.setValue(font)

  # Positions of labels are given relative to the screen size
  label_node.position(-1,+0.8)
  root.attachSubTree(label_node,True)

  # If our data set is time-dependent we attach a label that displays
  # the current time step
  if "time" in attributes:
    time_node = VisusLabelNode.construct()
    t = attributes["time"]
    title = "Date: %02d/%02d/%04d" % (t.month,t.day,t.year)
    time_node.text(title)
    time_node.setValue(font)
    time_node.position(0.6,-0.95)
    root.attachSubTree(time_node,True)

  # A VisusColorBar displays a bar with a color map on the inside and
  # allows us to attach tickmarks and labels
  color_bar = VisusColorBar.construct()

  # As before fopr the tickmark node we modify the labels via the Axis construct
  axis = color_bar.axis()

  # If we found a variable name we use it as label
  if "var_name" in attributes:
    axis.legendText(attributes["var_name"])
    
  # If we found a range we use it as well. 
  if "range" in attributes:
    axis.minValue(float(attributes["range"][0]))
    axis.maxValue(float(attributes["range"][1]))
    
  font.fontSize(4)
  axis.legendFont(font)
  axis.labelFont(font)
  color_bar.position(-1.2,-0.8)
  color_bar.axis(axis)

  # Finally, attach the color bar to the slice and make sure it
  # inherits its colormap from there
  slice.attachSubTree(color_bar,True)
  #color_bar.inherit(VisusSharedColorMap.typeIndex(),True)
  color_bar.inherit(8,True)


  # Lastly we must make sure that we actually see the data on the
  # screen which means we need to make sure that the data is scaled
  # and translated appropriately. (Alternatively, once can think of
  # this step as setting up the viewpoint of our virtual camera.

  # Using the current bounding box of the scene we adapt a default box
  # to the aspect ratios of of our scene and use it as our "world
  # coordinate system"  
  world_box = constructWorldBox(bbox)
  root.setValue(bbox)

  # Now we make sure that the data is drawn in our standard coordinate
  # system
  root.mapToWorldBox(world_box)

  # Finally, most climate data contains significantly more samples in
  # x/y than in z. Since, for the cartesian slice we effecticely draw
  # the data in index space since is inconvinient as the volume wille
  # very "flat." To compensate for this we simply scale the z
  # coordinate of our system
  root.getValue(matrix)
  matrix = matrix * scaleMatrix(1,1,10)
  root.setValue(matrix)

  # As the final step we create ourselfs a window to display the scene
  # graph hanging under the root node
  createWindow(root)
  
  return root,data_description


def sphereSlice(data=None,**keywords):

  """

    This function constructs a default ViSUS scene displaying a single
    z-slice through a regular block of data mapped onto the earth. The
    user can either pass a cdms2 array using the data attribute or
    enter the necessary information into the vcdat gui. If no data
    attribute is present the function will try to extract the
    necessary information from the gui and if that fails an empty
    scene will be displayed.

    The scene contains multiple \"nodes\" each of which can be
    manipulated using the keyboard and mouse. Som basic commands are

    Tab : Switch node
    Middle Mouse button : Pan scene
    Left Mouse button : Rotate scene
    Right Mouse button : Zoom in and out


    Usage: cartesianSlice(DataArray, keyword=value,...) where DataArray
    can be a standard numpy array, a cdms type data set, or
    \"None\". The function will attempt to extract a number of
    attributes from the given data or the gui each of which can be
    overridden by the user by passing it explicitly. Possible keywords
    are:

    name     : Verbal description of the data set or variable (used in the title)
    var_name : Short name of the variable  
    domain   : Physical domain of the data set represented as
               [[min_x,max_x],[min_y,max_y],[min_z,max_z]]. If
               the data is only two- or one-dimensional the extra
               dimensions will be assumed to be [0,0]
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
  
  # If the user did not pass in an array try to extract one from the
  # gui
  if data is None:
    data = guiDefinedData()

  attributes = keywords
  try:
    # Try to infer as many data attributes as you can and return a
    # string describing/defining the data in the ViSUS internal format
    data_description = constructDataDescriptor(data,attributes)
  except:
    print "Could not parse data. Unknown format"
    raise
  
  # Create a font to be used in all the nodes that render text
  font = VisusFont()
  font.fontSize(6)
  
  # Construct a root node for the scene graph hierarchy. This node is
  # mainly used for the global transformation (e.g. rotating the
  # complete scene) and is a convinient way to collect independent
  # nodes (nodes that should have no ancestor-descendend
  # relationship. Note, the use of the construct function rather than
  # a standard "constructor." ViSUS uses a smart pointer / reference
  # counting scheme which requires this.
  root = VisusGroup.construct()

  # Create two points that will form the left lower and right upper
  # corner of the data bounding box
  left = VectorDouble(3)
  right= VectorDouble(3)

  # Create a ViSUS data source based on the data description created earlier
  source = VisusDataSourceFactory.make(data_description)
  if not source.isValid():
    raise RuntimeError("Problem loading data set")

  # Get the bounding box of the data whose value may or may not depend
  # on data attributes such as units. In particular, if this
  # information was available the bounding box will be given in
  # pyhsical units rather than in index space
  source.domainBoundingBox(left,right)
  bbox = VisusBoundingBox()
  bbox.set(left,right);
  
  # The ViSUS system is based on data "requests" that describe a
  # particular piece of data that should be displayed. The data
  # requests can be manipulated through the user interface or
  # directly. However, here we want to create a sensible default
  # request to see a portion of our data.
  
  # First, we create an empty request 
  request = VisusDataRequest()

  # Second, we construct an array that describes the length in each
  # dimension of the box of data we want to extract in whatever units
  # the data source uses (typically physical space).
  extent = VectorDouble(3)  
  extent[0] = (right[0] - left[0]) # We want to see all of the x-coordinate
  extent[1] = (right[1] - left[1]) # and all of the y-coordinate 
  extent[2] = 0  # But we want to look at a plane so the z extend is 0

  # And we pass the extent information into the request
  request.extent(extent)

  # Second, we need to position our extent box in the data
  # set. Initially, the extent will be centered around the origin. To
  # see the middle of the data we want translate our request to the
  # center of the data's bounding box

  # So we create the corresponding transformation matrix
  matrix = translationMatrix((left[0]+right[0])/2,
                             (left[1]+right[1])/2,
                             (left[2]+right[2])/2,
                             )
  # And pass this matrix to the request
  request.transformation(matrix)
  
  # Third, we want to indicate to the ViSUS system that we are
  # really interested in a 2D slice of the data (rather than a
  # degenerate volume) as slices are a special case and implement some
  # special user interactions
  request.requestType(VISUS_2D_REQUEST);

  # Finally, we describe the resolution at which we want to see our
  # data. The resolution is defined in terms ofo sub-sampling
  # "strides" where a stride of 1 defines the full resolution data a
  # stride of 2, the data sub-sampled by a factor of 2 etc.. Strides
  # must be powers of two but can be independently set for each
  # dimension. Furthermore, for large data it can be useful to
  # progressively refine the data starting at some higher strides /
  # coarser resolution, ending at some lower strides / finer
  # resolution. This preserves interactivity while still providing
  # high resolution data if the request remains unchanged long enough

  # Since, for default scenes we expect the data to be small we only
  # want to see the highest resolution
  start = [ 1, 1, 1 ]
  end   = [ 1, 1, 1 ]
  request.setStrides(start,end)
  
  # Armed with the description of the data and the request we just
  # constructed we create a scene graph node that will extract the
  # data for us. This node, will continuously test its current request
  # and update the data accordingly. Therefore, one could adapt the
  # scene by simply passing different requests to this node (which is
  # what the graphical user interface does on mouse movements or
  # keyboard commands)
  
  # Create a producer node specialized to extract and axis aligned
  # bounding box from a data set. As before with the root node the
  # node must be explicitly constructed.
  extractor = VisusAxisAlignedExtractor.construct()

  # Set the data to extract our data from
  extractor.setValue(data_description)
  extractor.setValue(request) # Our default request
  extractor.setValue(bbox); # And the bounding box 
  extractor.drawBoundingBox(False);
  extractor.visible(False)

  # A VisusEarthNode behaves much like a VisusOrthogonalSlice (see
  # cartesianSlice) except that it expects the data to be
  # angle+elevation and maps the slice onto the sphere.
  earth = VisusEarthNode.construct()

  # Now we connect the output of the extractor to the input of the
  # visualization node
  if not earth.connectTexture(extractor):
    raise RuntimeError("unable to connect slice as earth input")

  # For reasons internal to the underpinnings of the ViSUS system the
  # visualization node must know about the current data request. The
  # simplest way of achieving this is to attach the slice node as
  # child to the extractor 
  extractor.attachSubTree(earth)

  # and to inherit/share the request 
  earth.inherit(VisusSharedDataRequest.typeIndex(),True)

  # The default view displays the box [-10,-10,-10]x[10,10,10] so we
  # set the radius to 3 to get a reasonably size sphere. 
  radius = VisusEarthRadius()
  radius.set(3.0)
  earth.setValue(radius)
  
  # Instead of the default greyscale color map we want to use a
  # typical red-to-blue map
  color_map = banded()
  earth.setValue(color_map)

  earth.periodic(True)

  # Finally, we attach the whole subtree that renders the data to the
  # root node
  root.attachSubTree(extractor)
  
  # Lastly, we attach more annotations to the scene

  # A VisusLabelNode is a general one line text of arbitrary font and
  # size. First, we create a title string using the variable name if
  # we found one
  label_node = VisusLabelNode.construct()
  title = "Variable: " + attributes["name"].replace("_"," ")
  label_node.text(title)
  label_node.setValue(font)

  # Positions of labels are given relative to the screen size
  label_node.position(-1,+0.8)
  root.attachSubTree(label_node,True)

  # If our data set is time-dependent we attach a label that displays
  # the current time step
  if "time" in attributes:
    time_node = VisusLabelNode.construct()
    t = attributes["time"]
    title = "Date: %02d/%02d/%04d" % (t.month,t.day,t.year)
    time_node.text(title)
    time_node.setValue(font)
    time_node.position(0.6,-0.95)
    root.attachSubTree(time_node,True)

  # A VisusColorBar displays a bar with a color map on the inside and
  # allows us to attach tickmarks and labels
  color_bar = VisusColorBar.construct()

  # As before fopr the tickmark node we modify the labels via the Axis construct
  axis = color_bar.axis()

  # If we found a variable name we use it as label
  if "var_name" in attributes:
    axis.legendText(attributes["var_name"])
    
  # If we found a range we use it as well. 
  if "range" in attributes:
    axis.minValue(float(attributes["range"][0]))
    axis.maxValue(float(attributes["range"][1]))
    
  font.fontSize(4)
  axis.legendFont(font)
  axis.labelFont(font)
  color_bar.position(-1.2,-0.8)
  color_bar.axis(axis)

  # Finally, attach the color bar to the slice and make sure it
  # inherits its colormap from there  
  earth.attachSubTree(color_bar)
  #color_bar.inherit(VisusSharedColorMap.typeIndex(),True)
  color_bar.inherit(8,True)

  createWindow(root)

  return root, data_description


if __name__ == "__main__":

  from sys import executable as exe
  from os.path import join,isdir,split

  sample_dir = join(split(exe)[0],"..","sample_data")

  if isdir(sample_dir):
    fid2 = cdms2.open( join(sample_dir,'clt.nc'))
    d=fid2( 'clt', time = ('1979-1-1 0:0:0.0', '1981-9-1 0:0:0.0'), latitude = (-90, 90), longitude = (-180, 180), squeeze = 0, order = '012' )

    root, earth_data = sphereSlice(d)
    #root, slice_data = cartesianSlice(d)

  #import time
  #for i in xrange(0,50):

  #  earth.rotate(0.1,0)
  #  time.sleep(0.1)
  #a = VisusDataDescription()
  #b = VisusDataDescription()
  #a = b
