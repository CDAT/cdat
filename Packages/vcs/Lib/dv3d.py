'''
Created on Jun 18, 2014

@author: tpmaxwel
'''

import Canvas
import VCS_validation_functions
import AutoAPI
import xmldocs
import cdtime, multiprocessing
import vcs
import time
import DV3D
from DV3D.ConfigurationFunctions import ConfigManager

class Gfdv3d(object,AutoAPI.AutoAPI):

    __slots__ = [
         '__doc__',
         'name',
         'axes',
         'g_name',
         'ncores',
         'plot_attributes'
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
#        value=VCS_validation_functions.checkOnOff(self,'axes',value)
        self._axes=value
    axes=property(_getaxes,_setaxes)

    def _getNumCores(self):
        return self.ncores
    def _setNumCores(self, nc ):
        self.ncores = nc
    NumCores=property(_getNumCores,_setNumCores)
    
    def script(self, script_filename=None, mode=None):
        if (script_filename == None):
          raise ValueError, 'Error - Must provide an output script file name.'

        if (mode == None):
           mode = 'a'
        elif (mode not in ('w', 'a')):
          raise ValueError, 'Error - Mode can only be "w" for replace or "a" for append.'

        # By default, save file in json
        scr_type = script_filename.split(".")
        if len(scr_type)==1 or len(scr_type[-1])>5:
          scr_type= "json"
          if script_filename!="initial.attributes":
            script_filename+=".json"
        else:
          scr_type = scr_type[-1]
        if scr_type == '.scr':
           raise DeprecationWarning("scr script are no longer generated")
        elif scr_type==".py":
           mode = mode + '+'
           py_type = script_filename[len(script_filename)-3:len(script_filename)]
           if (py_type != '.py'):
              script_filename = script_filename + '.py'

           # Write to file
           fp = open(script_filename,mode)
                      
           if (fp.tell() == 0): # Must be a new file, so include below
              fp.write("#####################################\n")
              fp.write("#                                 #\n")
              fp.write("# Import and Initialize VCS     #\n")
              fp.write("#                             #\n")
              fp.write("#############################\n")
              fp.write("import vcs\n")
              fp.write("v=vcs.init()\n\n")
              
           gtype = 'xyt' if (self._axes=="xyt") else 'default' 
           unique_name = 'gm3d_%s' % str( time.time() % 1 )[2:]
           if self.g_name=='3d_scalar': fp.write( '%s = vcs.get3d_scalar( %s )\n' % ( unique_name, gtype ) )
           if self.g_name=='3d_vector': ffp.write( '%s = vcs.get3d_vector( %s )\n' % ( unique_name, gtype ))       
           for param_name in self.parameter_names:
               fp.write( '%s.%s = %s\n' % ( unique_name, param_name, self.cfgManager.getParameterValue( param_name ) ) )
        else:
          #Json type
          mode+="+"
          f = open(script_filename,mode)
          vcs.utils.dumpToJson(self,f)
          f.close()

    
    def __init__(self, Gfdv3d_name, Gfdv3d_name_src='default'):
        if not isinstance(Gfdv3d_name,str):
            raise ValueError,"DV3D name must be a string"
        if Gfdv3d_name in vcs.elements[self.g_name].keys():
            raise ValueError,"DV3D graphic method '%s' already exists" % Gfdv3d_name
        self._name = Gfdv3d_name
        self.plot_attributes = {}
        self.projection = 'default' 
        self.provenanceHandler = None
                
        if Gfdv3d_name=="xyt": 
            self._axes="xyt"
        else:
            self._axes="xyz"

        self.cfgManager = ConfigManager()  
        self.ncores = multiprocessing.cpu_count()           
        self.addParameters()
            
        vcs.elements[self.g_name][Gfdv3d_name]=self
#        print "Adding VCS element: %s %s " % ( self.g_name, Gfdv3d_name )
        
    def setProvenanceHandler(self, provenanceHandler ):
        self.provenanceHandler = provenanceHandler
                
    def getStateData(self):
        return self.cfgManager.getStateData()

    def getConfigurationData( self, **args ):
        return self.cfgManager.getConfigurationData( **args )

    def getConfigurationParms(self, **args): 
        return self.cfgManager.getConfigurationParms( **args )
    
    def getConfigurationState( self, pname, **args ):
        return self.cfgManager.getConfigurationState( pname, **args )

    def add_property(self, name ):
        fget = lambda self: self.getParameter(name)
        fset = lambda self, value: self.setParameter(name, value)
        setattr(self.__class__, name, property(fget, fset))
        if not name in Gfdv3d.__slots__:
            Gfdv3d.__slots__.append( name )

    def addPlotAttribute(self, name, value ):
        self.plot_attributes[ name ] = value 

    def getPlotAttribute(self, name ):
        return self.plot_attributes.get( name, None )

    def getPlotAttributes( self ):
        return self.plot_attributes
    
    @staticmethod
    def getParameterList():
        cfgManager = ConfigManager()
        parameterList = cfgManager.getParameterList()
        return parameterList
                
    def addParameters( self ):
        self.parameter_names = []
        for pname in self.getParameterList():
            self.add_property( pname )
            self.parameter_names.append( pname )
#            print "  ------------->> Adding parameter: ", pname
            
    def getParameter(self, param_name, **args ):
        return self.cfgManager.getParameterValue( param_name, **args )

    def setParameter(self, param_name, data, **args ):
        self.cfgManager.setParameter( param_name, data, **args )
        
    def restoreState(self):
        self.cfgManager.restoreState()

    def initDefaultState(self):
        self.cfgManager.initDefaultState()
                
    def list(self):
        print ' ---------- DV3D (Gfdv3d) member (attribute) listings ---------'
        print 'name =',self.name
        print 'axes =',self.axes
        for pname in self.parameter_names:
            print ' = '.join( [ pname, self.getParameter( pname ) ] )

class Gf3Dvector(Gfdv3d):

    def __init__(self, Gfdv3d_name, Gfdv3d_name_src='default'):        
        self.g_name='3d_vector'                        
        Gfdv3d.__init__(self, Gfdv3d_name, Gfdv3d_name_src='default')

class Gf3Dscalar(Gfdv3d):

    def __init__(self, Gfdv3d_name, Gfdv3d_name_src='default'):        
        self.g_name='3d_scalar'                        
        Gfdv3d.__init__(self, Gfdv3d_name, Gfdv3d_name_src='default')
        self.VectorDisplay = Gfdv3d_name

if __name__ == '__main__':
    dv3d = vcs.get3d_scalar()
    dv3d.script( '/tmp/test.json' )
    
    
    