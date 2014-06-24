'''
Created on Jun 18, 2014

@author: tpmaxwel
'''

import Canvas
import VCS_validation_functions
import AutoAPI
import xmldocs
import cdtime
import vcs


class Gfdv3d(object,AutoAPI.AutoAPI):

    __slots__ = [
         '__doc__',
         'name',
         'axes'
         ]
    
    def _getname(self):
        return self._name    
    def _setname(self,value):
        value=VCS_validation_functions.checkname(self,'name',value)
        if value is not None:
            self._name=value            
    name=property(_getname,_setname)
    
    def _getaxes(self):
        return self._axes
    def _setaxes(self,value):
        value=VCS_validation_functions.checkOnOff(self,'axes',value)
        self._axes=value
    axes=property(_getaxes,_setaxes)
    
    def __init__(self, Gfdv3d_name, Gfdv3d_name_src='default'):
    #                                                         #
        ###########################################################
    # Initialize the meshfill class and its members            #
        #                              #
    # The getGfmmember function retrieves the values of the   #
        # meshfill members in the C structure and passes back the  #
    # appropriate Python Object.                              #
        ###########################################################


        if not isinstance(Gfdv3d_name,str):
            raise ValueError,"DV3D name must be a string"
        if Gfdv3d_name in vcs.elements["dv3d"].keys():
            raise ValueError,"DV3D graphic method '%s' already exists" % Gfdv3d_name
        self._name = Gfdv3d_name
        self.g_name='Gfdv3d'
            
        if Gfdv3d_name=="default": 
            self._axes="xyz"
        elif Gfdv3d_name=="hovmuller": 
            self._axes="xyt"
            
        vcs.elements["dv3d"][Gfdv3d_name]=self

    def list(self):
        print ' ---------- DV3D (Gfdv3d) member (attribute) listings ---------'
        print 'name =',self.name
        print 'axes =',self.axes
