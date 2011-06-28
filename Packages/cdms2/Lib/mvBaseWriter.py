#!/usr/bin/env python

"""
Abstract class for writing data into file
Alex Pletzer, Tech-X Corp.
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
        sm = mvSphereMesh.SphereMesh(var)
        self.shape = sm.shape
        self.mesh = sm.getXYZCoords(sphereRadius, maxElev)

    def write(self, filename):
        """
        Write data to file. This method is overloaded.
        @param filename file name
        """
        raise NotImplementedError, \
            'write method not implemented in derived class'
