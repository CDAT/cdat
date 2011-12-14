#!/usr/bin/env python

"""
Abstract class for writing data into file
Alex Pletzer, Tech-X Corp. (2011)
This code is provided with the hope that it will be useful. 
No guarantee is provided whatsoever. Use at your own risk.
"""

import mvSphereMesh

class BaseWriter:

    def __init__(self, var, sphereRadius=1.0, maxElev=0.1):
        """
        Constructor
        @param var a cdms2 variable
        @param sphereRadius radius of the sphere upon which the grid will 
                            be projected
        @param maxElev max elevation/depth normalized to the sphere radius
        """
        self.var = var
        sphere_mesh = mvSphereMesh.SphereMesh(var, maxElev)

        self.shape = sphere_mesh.shape

        # there is currently a bug in vizSchema which causes 
        # visit to crash if the leading index is 1, this is 
        # a workaround the problem
        if self.shape[0] == 1:
            self.shape = list(sphere_mesh.shape[1:]) + [1,]
        
        self.mesh = sphere_mesh.getXYZCoords(sphereRadius)

    def write(self, filename):
        """
        Write data to file. This method is overloaded.
        @param filename file name
        """
        raise NotImplementedError, \
            'write method not implemented in derived class'
