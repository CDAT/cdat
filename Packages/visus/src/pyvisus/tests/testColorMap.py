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


from pyvisus.VisusDefaultScenes import *

  
    

if __name__ == "__main__":

  from sys import executable as exe
  from os.path import join,isdir,split

  sample_dir = join(split(exe)[0],"..","sample_data")

  if isdir(sample_dir):
    fid2 = cdms2.open( join(sample_dir,'clt.nc'))
    d=fid2( 'clt', time = ('1979-1-1 0:0:0.0', '1981-9-1 0:0:0.0'), latitude = (-90, 90), longitude = (-180, 175), squeeze = 0, order = '012' )


  # This creates a minimal hierarchy consisting only of the root and
  # extractor and a slice node
  root = sliceHierarchy(d)

  scene_number = 1;
  
  if scene_number == 1:

    # This options creates a custom color map consisting of only 5
    # colors evenly spaced

    # Create a color map with 4 entries each entry by default consist
    # of 4 floats namely 'R', 'G', 'B', 'Alpha'. Furthermore, the
    # resolution must be a power of two to allow texture based drawing
    color_map = VisusColorMap(4)

    # The first color is red. Note that the alpha is 1 by default
    color_map[0] = 1
    color_map[1] = 0
    color_map[2] = 0

    # The second color is yellow
    color_map[4] = 1
    color_map[5] = 1
    color_map[6] = 0

    # The third color is green
    color_map[8] = 0
    color_map[9] = 1
    color_map[10] = 0

    # The fourth color is purple
    color_map[12] = 0
    color_map[13] = 1
    color_map[14] = 1


    # Now we get the slice node that draws the data 
    slice = root.child(0).child(0)

    # And replace its color map by the new one
    slice.setValue(color_map)

  elif scene_number == 2:

    # This option creates a compressed color map that only colors the
    # range [10,60] and uses custom colors outside this rang

    slice = root.child(0).child(0)

    # Create a grey scale color map (which is the default map
    color_map = VisusColorMap()

    # Set the new range
    color_map.setBounds(10,90)

    # Set everything under 10 to turquoise
    color_map.setFloorColor(0,1,1)

    # And everything above 90 to orange
    color_map.setCeilingColor(1,0.5,0)

    slice.setValue(color_map)

  elif scene_number == 3:

    # This option uses the standard color map but scales the range from [0,60]
    # logarithmically to enhance low values

    slice = root.child(0).child(0)

    color_map = VisusColorMap()
    slice.getValue(color_map)

    color_map.logarithmicMap(0,60)
    
    slice.setValue(color_map)
   
  elif scene_number == 4:

    # This option uses the standard color map but scales the range from [40,100]
    # exponentially to enhance high values

    slice = root.child(0).child(0)

    color_map = VisusColorMap()
    slice.getValue(color_map)

    color_map.exponentialMap(40,100)
    
    slice.setValue(color_map)

  createWindow(root)

