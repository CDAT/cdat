'''
Created on May 9, 2014

@author: tpmaxwel
'''
import sys, vtk, cdms2, traceback, os, cdtime, cPickle, copy 
from StringIO import StringIO
import numpy as np
import inspect
from weakref import WeakSet, WeakKeyDictionary

SLICE_WIDTH_LR_COMP = [ 'xlrwidth', 'ylrwidth', 'zlrwidth' ]
SLICE_WIDTH_HR_COMP = [ 'xhrwidth', 'yhrwidth', 'zhrwidth' ]

packagePath = os.path.dirname( __file__ ) 
DataDir = os.path.join( packagePath, 'data' ) 
defaultMapDir = DataDir 
defaultOutlineMapFile = os.path.join( defaultMapDir,  'political_map.png' )

packagePath = os.path.dirname( __file__ )  
defaultMapDir = os.path.join( packagePath, 'data' )
defaultLogoFile = os.path.join( defaultMapDir,  'uvcdat.jpg' )
defaultMapFile = os.path.join( defaultMapDir,  'earth2k.jpg' )
defaultMapCut = -180
SLIDER_MAX_VALUE = 100
MAX_IMAGE_SIZE = 1000000

class CDMSDataType:
    Volume = 1
    Slice = 2
    Vector = 3
    Hoffmuller = 4
    ChartData = 5
    VariableSpace = 6
    Points = 7

class DataCache():
    
    def __init__(self):
        self.data = {}
        self.cells = set()
        
def getDatatypeString( scalar_dtype ):
    if scalar_dtype == np.ushort:
        return 'UShort' 
    if scalar_dtype == np.ubyte:
        return 'UByte' 
    if scalar_dtype == np.float32:
        return 'Float' 
    if scalar_dtype == np.float64:
        return 'Double' 
    return None

def getBool( val ):
    if isinstance( val, str ):
        if( val.lower()[0] == 't' ): return True
        if( val.lower()[0] == 'f' ): return False
        try:    val = int(val)
        except: pass
    return bool( val )

def makeList( obj, minSize = 1 ):
    if obj == None: return None
    if isinstance( obj, tuple ): obj = list( obj ) 
    if not isinstance( obj, list ): obj = [ obj ]
    if ( len( obj ) == 1 ) and minSize > 1:
        obj = obj*minSize
#    assert len(obj) == minSize, "Wrong number of elements in list" 
    return obj

def deserialize_value( sval ):
    if isinstance( sval, float ): 
        return sval
    try:
        return int(sval)
    except ValueError:
        try:
            return float(sval)
        except ValueError:
            return sval

def deserialize_str( sval ):
    try:
        return int(sval)
    except ValueError:
        try:
            return float(sval)
        except ValueError:
            return sval

def deserialize( data_obj ):
    if isinstance( data_obj, tuple ) or isinstance( data_obj, list ):
        return [ deserialize(item) for item in data_obj ]
    if isinstance( data_obj, dict ):
        rv = {}
        for item in data_obj.items(): rv[ deserialize(item[0]) ] = deserialize(item[0])
        return rv
    if isinstance( data_obj, str ):
        return deserialize_str( data_obj )
    return data_obj

def get_value_decl( val ):
    if isinstance( val, bool ): return "bool"
    if isinstance( val, int ): return "int"
    if isinstance( val, float ): return "float"
    return "str"

def getMaxScalarValue( scalar_dtype ):
    if scalar_dtype == np.ushort:
        return 65535.0
    if scalar_dtype == np.ubyte:
        return 255.0 
    if scalar_dtype == np.float32:
        f = np.finfo(np.float32) 
        return f.max
    if scalar_dtype == np.float64:
        f = np.finfo(np.float64) 
        return f.max
    return None

class PlotType:
    Planar = 0
    Spherical = 1
    List = 0
    Grid = 1
    LevelAliases = [ 'isobaric', "layers", "interfaces" ]
    
    @classmethod
    def validCoords( cls, lat, lon ):
        return ( id(lat) <> id(None) ) and ( id(lon) <> id(None) )
    
    @classmethod
    def isLevelAxis( cls, pid ):
        lname = pid.lower()
        if ( lname.find('lev')  >= 0 ): return True
        if ( lname.find('bottom') >= 0 ) and ( lname.find('top') >= 0 ): return True
        if pid in cls.LevelAliases: return True
        if lname in cls.LevelAliases: return True
        return False    

    @classmethod
    def getPointsLayout( cls, grid ):
        if grid <> None:
            if (grid.__class__.__name__ in ( "RectGrid", "TransientRectGrid", "FileRectGrid") ): 
                return cls.Grid
        return cls.List  

def isDesignated( axis ):
    return ( axis.isLatitude() or axis.isLongitude() or axis.isLevel() or axis.isTime() )

def matchesAxisType( axis, axis_attr, axis_aliases ):
    matches = False
    aname = axis.id.lower()
    axis_attribute = axis.attributes.get('axis',None)
    if axis_attribute and ( axis_attribute.lower() in axis_attr ):
        matches = True
    else:
        for axis_alias in axis_aliases:
            if ( aname.find( axis_alias ) >= 0): 
                matches = True
                break
    return matches
class SIGNAL(object):
    
    def __init__( self, name = None ):
        self._functions = WeakSet()
        self._methods = WeakKeyDictionary()
        self._name = name

    def __call__(self, *args, **kargs):
        # Call handler functions
        for func in self._functions:
            func(*args, **kargs)

        # Call handler methods
        for obj, funcs in self._methods.items():
            for func in funcs:
                func(obj, *args, **kargs)

    def connect(self, slot):
        if inspect.ismethod(slot):
            if slot.__self__ not in self._methods:
                self._methods[slot.__self__] = set()

            self._methods[slot.__self__].add(slot.__func__)

        else:
            self._functions.add(slot)

    def disconnect(self, slot):
        if inspect.ismethod(slot):
            if slot.__self__ in self._methods:
                self._methods[slot.__self__].remove(slot.__func__)
        else:
            if slot in self._functions:
                self._functions.remove(slot)

    def clear(self):
        self._functions.clear()
        self._methods.clear()

class ConfigManager:
    
    
    def __init__( self,  **args ): 
        self.ConfigCmd = SIGNAL("ConfigCmd")
        self.cfgFile = os.path.join( DataDir, 'parameters.txt' )
        self.stateFile = os.path.join( DataDir, 'state.txt' )
        self.config_params = {}
        self.iCatIndex = 0
        self.cats = {}
        self.cfgDir = None
        self.metadata = args
        self.configurableFunctions = {}
        self.parameters = {}
        self.initialized = False

    def getParameter( self, param_name, **args ):
        cparm = self.parameters.get( param_name, None )
        if cparm == None:
            cparm = ConfigParameter( param_name, **args )
            self.parameters[ param_name ] = cparm
        return cparm
            
      
    def setParameter(self, param_name, data, **args ):
        param = self.getParameter( param_name, **args )
#        pdata = data if hasattr( data, '__iter__' ) else [ data ]
        param.setInitValue( data )
#        print '  <<---------------------------------------------------->> Set Parameter: ', param_name, " = ", str( data )

    def getParameterValue(self, param_name, **args ):
        param = self.getParameter( param_name, **args )
        return None if ( param == None ) else param.getValues()
        
    def getConfigurableFunction(self, name, **args ):
        rv = self.configurableFunctions.get( name, None )
        if rv == None:
            type = args.get( 'type', ConfigurableFunction.Default )
            if type == ConfigurableFunction.Default:  rv = ConfigurableFunction( name, **args )
            elif type == ConfigurableFunction.Slider: rv = ConfigurableSliderFunction( name, **args )
            else:
                print>>sys.stderr, "Error, Unknown Configurable Function Type: ", str(type)
                return None
            self.configurableFunctions[name] = rv
        return rv
        
    def getMetadata(self, key=None ):
        return self.metadata.get( key, None ) if key else self.metadata

    def addParam(self, key ,cparm ):
        self.config_params[ key ] = cparm
#        print "Add param[%s]" % key
                     
    def saveConfig( self ):
        try:
            f = open( self.cfgFile, 'w' )
            for config_item in self.config_params.items():
                cfg_str = " %s = %s " % ( config_item[0], config_item[1].serialize() )
                f.write( cfg_str )
            f.close()
        except IOError:
            print>>sys.stderr, "Can't open config file: %s" % self.cfgFile

    def addParameter( self, config_name, **args ):
#        print '  <<---------------------------------------------------->> Add Parameter: ', config_name, " = ", str( args )
        cparm = self.getParameter( config_name, **args )
        categoryName = args.get('category', None )
        varname = args.get('varname', None )
        key_tok = [] 
        if categoryName: key_tok.append( categoryName )
        key_tok.append( config_name )
        if varname: key_tok.append( varname )
        self.addParam( ':'.join( key_tok ), cparm )
        return cparm

    def readConfig( self ):
        try:
            f = open( self.cfgFile, 'r' )
            while( True ):
                config_str = f.readline()
                if not config_str: break
                cfg_tok = config_str.split('=')
                parm = self.config_params.get( cfg_tok[0].strip(), None )
                if parm: parm.initialize( cfg_tok[1] )
        except IOError:
            print>>sys.stderr, "Can't open config file: %s" % self.cfgFile                       

    def saveParameterMetadata( self ):
        try:
            parameter_file = open( self.cfgFile, "w")
            for cf in self.configurableFunctions.values():
                parameter_file.write( cf.getParameterMetadata() + '\n' )        
            parameter_file.close()
            print " saved Parameter Metadata to file ", self.cfgFile

        except Exception, err:
            print>>sys.stderr, "Can't save parameter metadata: ", str(err)
            
    def saveState(self):
        try:
            state_file = open( self.stateFile, "w")
            for cf in self.configurableFunctions.values():
                state_data = cf.serializeState()
                if state_data:
                    state_file.write( state_data + '\n' )        
            state_file.close()
            print " saved state data to file ", state_file

        except Exception, err:
            print>>sys.stderr, "Can't save state data: ", str(err)

    def restoreState( self ):
        try:
            state_file = open( self.stateFile, "r")
            while True:
                line = state_file.readline()
                if line == "": break
                serializedState = line.split('=')
                print '  <<---------------------------------------------------->> Restore State: ', serializedState[0], " = ", str( serializedState[1] )
                cp = self.getParameter( serializedState[0] )
                cp.restoreState( serializedState[1].strip() )
   
            state_file.close()
            self.initialized = True

        except Exception, err:
            print>>sys.stderr, "Can't read state data: ", str(err)
            
    def initDefaultState(self):
        cp = self.getParameter( 'XSlider' )
        cp.restoreState( { 'state': 1 } )
        cp = self.getParameter( 'YSlider' )
        cp.restoreState( { 'state': 1 } )
        cp = self.getParameter( 'ZSlider' )
        cp.restoreState( { 'state': 1 } )
                  
    def getParameterMetadata( self ):
        try:
            parameter_mdata = [ ]
            parameter_file = open( self.cfgFile, "r")
            while True:
                line = parameter_file.readline()
                if line == "": break
                parameter_mdata.append( line.split(','))
   
            parameter_file.close()

        except Exception, err:
            print>>sys.stderr, "Can't read parameter metadata: ", str(err)
            
        return parameter_mdata
        
    def initParameters(self):
        if not self.cfgDir:
            self.cfgDir = os.path.join( os.path.expanduser( "~" ), ".cpc" )
            if not os.path.exists(self.cfgDir): 
                os.mkdir(  self.cfgDir )
        if not self.cfgFile:
            self.cfgFile = os.path.join( self.cfgDir, "cpcConfig.txt" )
        else:
            self.readConfig()            
        for config_item in self.config_params.items():
            self.ConfigCmd( ( "InitParm",  config_item[0], config_item[1] ) )

    def getParameterPersistenceList(self):
        plist = []
        for cfg_item in self.config_params.items():
            key = cfg_item[0]
            cfg_spec = cfg_item[1].pack()
            plist.append( ( key, cfg_spec[1] ) )
        return plist

    def initialize( self, parm_name, parm_values ):
        if not ( isinstance(parm_values,list) or isinstance(parm_values,tuple) ):
            parm_values = [ parm_values ]
        cfg_parm = self.config_params.get( parm_name, None )
        if cfg_parm: cfg_parm.unpack( parm_values )

    def getPersistentParameterSpecs(self):
        plist = []
        for cfg_item in self.config_params.items():
            key = cfg_item[0]
            values_decl = cfg_item[1].values_decl()
            plist.append( ( key, values_decl ) )
        return plist
    
    def addCategory(self, cat_name ):
        self.iCatIndex = self.iCatIndex + 1
        self.cats[ self.iCatIndex ] = cat_name
        return self.iCatIndex

class ConfigParameter:
    
    def __init__(self, name, **args ):
        self.name = name 
        self.ValueChanged = SIGNAL( 'ValueChanged' )
        self.varname = args.get( 'varname', name ) 
        self.ptype = args.get( 'ptype', name ) 
        self.values = args
        self.valueKeyList = list( args.keys() )
        self.stateKeyList = []
#        self.scaling_bounds = None
            
    def serializeState( self, **args ):
        if len( self.stateKeyList ) == 0:
            return None
        state_parms = {}
        for key in self.stateKeyList:
            state_parms[key] = self.values[key]
        state_parms.update( **args )
        return str( state_parms ) 

    def restoreState( self, stateData ) :
        if type( stateData ) == str:
            state = eval( stateData )
        elif type( stateData ) == dict:
            state = stateData
        else:
            print>>sys.stderr, "Unrecognized stateData type: ", str( type( stateData ) )
            return
        self.values.update( state )
        self.values[ 'init' ] = self.getValues()
        print " --> Restore state [%s] : %s " % ( self.name, stateData )
                                    
    def __str__(self):
        return " ConfigParameter[%s]: %s " % ( self.name, str( self.values ) )
   
    def addValueKey( self, key ):
        if not (key in self.stateKeyList) or (key in self.valueKeyList):
            self.stateKeyList.append( key ) 
                                
    def pack( self ):
        try:
            return ( self.ptype, [ str( self.values[key] ) for key in self.valueKeyList ] )
        except KeyError:
            print "Error packing parameter %s%s. Values = %s " % ( self.name, str(self.valueKeyList), str(self.values))

    def unpack( self, value_strs ):
        if len( value_strs ) <> len( self.values.keys() ): 
            print>>sys.stderr, " Error: parameter structure mismatch in %s ( %d vs %d )" % ( self.name,  len( value_strs ), len( self.values.keys() ) ); sys.stderr.flush()
        for ( key, str_val ) in zip( self.valueKeyList, value_strs ):
            self.values[key] = deserialize_value( str_val ) 
#        print " && Unpack parameter %s: %s " % ( self.name, str( self.values ) ); sys.stdout.flush()
            
    def __len__(self):
        return len(self.values)

    def __getitem__(self, key):
        return self.values.get( key, None )

    def __setitem__(self, key, value ):
        self.values[key] = value 
        self.addValueKey( key )

    def __call__(self, **args ):
        self.values.update( args )
        args1 = [ self.ptype ]
        for item in args.items():
            args1.extend( list(item) )
            self.addValueKey( item[0] )
        args1.append( self.name )
        self.ValueChanged( args1 )
         
    def getName(self):
        return self.name

    def getVarName(self):
        return self.varname

    def getParameterType(self):
        return self.ptype
    
    def initialize( self, config_str ):
        self.values = eval( config_str )
        self.sort()

    def serialize( self ):
        return str( self.values )

    def getValue( self, key=0, default_value=None ):
        return self.values.get( key, default_value )

    def getInitValue( self, default_value=None ):
        return self.values.get( 'init', default_value )

    def setInitValue( self, value, update = False ):
        self.setValue( 'init', value, update )

    def setValue( self, key, val, update=False  ):
        self.values[ key ] = val
        self.addValueKey( key )
        if update: 
            args1 = [  self.ptype, key, val, self.name]
            self.ValueChanged( args1 )

    def setValues( self, values, update=False  ):
        for key,value in enumerate( values ):
            self.setValue( key, value )

    def getValues( self ):
        vals = []
        for index in range( 0, 100 ):
            val = self.getValue( index )
            if val == None: break
            vals.append( val )
        return vals

    def incrementValue( self, key, inc = 1 ):
        inc_val = self.values[ key ] + inc
        self.values[ key ] = inc_val
        return inc_val
        
    @property
    def rmin(self):
        return self['rmin']

    @rmin.setter
    def rmin(self, value):
        self['rmin'] = value
        
    @property
    def rmax(self):
        return self['rmax']

    @rmax.setter
    def rmax(self, value):
        self['rmax'] = value
        
#     def setScalingBounds( self, sbounds ):
#         self.scaling_bounds = sbounds
#         
#     def getScaledRange(self):
#         if self.scaling_bounds:
#             ds = self.scaling_bounds[1] - self.scaling_bounds[0]
#             return ( self.scaling_bounds[0] + self.rmin * ds, self.scaling_bounds[0] + self.rmax * ds )
#         else:
#             return self.getRange()

    def setRange(self, range ):
        self.rmin = range[0] # min( max( range[0], 0.0 ), 1.0 )
        self.rmax = range[1] # max( min( range[1], 1.0 ), 0.0 )
        
    def getRange( self ):
        return ( self.rmin, self.rmax )

CfgManager = ConfigManager() 
  
class ConfigurableFunction:

    Default = 0
    Slider = 1    
    ConfigurableFunctions = {}    
    
    def __init__( self, name, **args ):
        self.name = name
        self.persist = args.get( 'persist', True )
#         if name == 'XSlider':
#             print "."
        self.value = CfgManager.addParameter( name, **args )
        self.type = 'generic'
        self.kwargs = args
        self.cfg_state = None
        self.label = args.get( 'label', self.name )
        self.units = args.get( 'units', '' ).strip().lower()
        self.persist = bool( args.get( 'persist', True ) )
        self.key = args.get( 'key', None )
        ival = self.value.getValue( 'init' )
        if ival <> None:
            self.initial_value = ival if hasattr( ival, '__iter__' ) else [ ival ]
        else:    
            self.initial_value = makeList( args.get( 'initValue', None ), self.getValueLength() )
#        self.group = args.get( 'group', ConfigGroup.Display )  
        self.active = args.get( 'active', True )
        self.group = args.get( 'group', None )
        self._persisted = True
        self.interactionHandler = args.get( 'interactionHandler', None )
        
    def getState(self):
        return self.value.getValue('state')

    def setState(self,value):
        return self.value.setValue('state',value)
        
    def serializeState(self):
        state_data = self.value.serializeState() 
        return None if ( (state_data == None) or not self.persist ) else "=".join( [ self.name, state_data ] ) 

    def restoreState( self, stateData ):
        self.value.restoreState( stateData )
     
    def sameGroup( self, config_fn ): 
        if id( self ) == id( config_fn ): return False
        if self.name == config_fn.name: return True
        if (self.group <> None)  and (self.group == config_fn.group): return True
        return False
          
    def getPosition(self):
        return None
        
    def processStateChangeEvent( self, state ):
        args = [ "InitConfig", state ]
        self.interactionHandler( args, self )

    @classmethod
    def activate( cls ):
        CfgManager.initParameters()
        
    def getValueLength(self):
        return 1
        
    def open(self, cfg_state ):
        self.cfg_state = cfg_state

    def start( self, interactionState, x, y ) :
        pass
           
    def close(self) :
        self.processInteractionEvent( [ "Close" ] )
        
    def processInteractionEvent( self, args ):
        if self.interactionHandler:
            self.interactionHandler( args, self )
            
    def init( self, **args ):
        if self.interactionHandler:
            init_args = [ "Init", args ]
            self.interactionHandler( init_args, self )
                        
    def get_persisted(self):
        return self._persisted if self.persist else True
    
    def updateWindow( self ):
        pass
     
    def set_persisted(self, value):
        self._persisted = value
#        
    persisted = property(get_persisted, set_persisted) 

    def isValid(self):
        return True
    
    def hasDataUnits(self):
        return ( self.units == 'data' )
    
    def isCompatible( self, config_fn ):
        if config_fn and self.matchUnits:
            if self.units <> config_fn.units:
                return False           
        return True

    def setValue( self, new_parameter_value ):
        self.value.setValue( new_parameter_value )
        
    def postInstructions( self, message ):
        print "\n ----- %s -------\n" % message
             
    def matches( self, key ):
        return self.active and ( self.key == key )
    
    def applyParameter( self, **args ):
        pass
            
    def getHelpText( self ):
        return "<tr>   <td>%s</td>  <td>%s</td> <td>%s</td> </tr>\n" % ( self.key, self.label, self.type )
        
class ConfigurableSliderFunction( ConfigurableFunction ):

    def __init__( self, name, **args ):
        self.sliderLabels = makeList( args.get( 'sliderLabels', [ 'Range Min', 'Range Max' ] ) )
        ConfigurableFunction.__init__( self, name, **args  )
        self.StartSlidingSignal =SIGNAL('startSliding')
        self.UpdateSlidingSignal =SIGNAL('updateSliding')
        self.type = 'slider'
        self._range_bounds = args.get( 'range_bounds', None )
        self._initial_range = None
        self.position = args.get( 'position', None )
        if self.initial_value <> None:
            for index, value in enumerate( self.initial_value ):
                self.value.setValue( index, value )
                
    def getPosition(self):
        return self.position[0] if self.position else None

    def scaleRange( self, scale_factor ):
        if self._initial_range == None: 
            self._initial_range = self._range_bounds
        if self._initial_range <> None:
            self._range_bounds = [ irv * scale_factor for irv in self._initial_range ]

    def getValueLength(self):
        return len( self.sliderLabels )
        
    def getRangeBounds(self):
        return copy.copy( self._range_bounds )

    def setRangeBounds(self, value):
        self._range_bounds = copy.copy( value )
         
def getTitle( dsid, name, attributes, showUnits=False ):
    long_name = attributes.get( 'long_name', attributes.get( 'standard_name', name ) )
    if not showUnits: return "%s:%s" % ( dsid, long_name )
    units = attributes.get( 'units', 'unitless' )
    return  "%s:%s (%s)" % ( dsid, long_name, units )

def getNewVtkDataArray( scalar_dtype ):
    if scalar_dtype == np.ushort:
        return vtk.vtkUnsignedShortArray() 
    if scalar_dtype == np.ubyte:
        return vtk.vtkUnsignedCharArray() 
    if scalar_dtype == np.float32:
        return vtk.vtkFloatArray() 
    if scalar_dtype == np.float64:
        return vtk.vtkDoubleArray() 
    return None

def getFloatStr( val ):
    if ( type(val) == type(' ') ): return val
    return "%.1f" % val

def getStringDataArray( name, values = [] ):
    array = vtk.vtkStringArray()
    array.SetName( name )
    for value in values:
        array.InsertNextValue( value )
    return array

def encodeToString( obj ):
    rv = None
    try:
        buffer = StringIO()
        pickler = cPickle.Pickler( buffer )
        pickler.dump( obj )
        rv = buffer.getvalue()
        buffer.close()
    except Exception, err:
        print>>sys.stderr, "Error pickling object %s: %s" % ( str(obj), str(err) )
    return rv

def decodeFromString( string_value, default_value=None ):
    obj = default_value
    try:
        buffer = StringIO( string_value )
        pickler = cPickle.Unpickler( buffer )
        obj = pickler.load()
        buffer.close()
    except Exception, err:
        print>>sys.stderr, "Error unpickling string %s: %s" % ( string_value, str(err) )
    return obj
  
class InputSpecs:
    
    def __init__( self, **args ):
        self.units = ''
        self.scalarRange = None
        self.seriesScalarRange = None
        self.rangeBounds = None
        self.referenceTimeUnits = None
        self.metadata = None
        self._input = None
        self.fieldData = None
        self.datasetId = None
        self.clipper = None
        self.dtype = None
        
    def isFloat(self):
        return self.dtype == "Float"

#     def selectInputArray( self, raw_input, plotIndex ):
#         self.updateMetadata( plotIndex )
#         old_point_data = raw_input.GetPointData()  
#         nArrays = old_point_data.GetNumberOfArrays() 
#         if nArrays == 1: return raw_input  
#         image_data = vtk.vtkImageData()
#         image_data.ShallowCopy( raw_input )
#         new_point_data = image_data.GetPointData()        
#         array_index = plotIndex if plotIndex < nArrays else 0
#         inputVarList = self.metadata.get( 'inputVarList', [] )
#         if array_index < len( inputVarList ):
#             aname = inputVarList[ array_index ] 
#             new_point_data.SetActiveScalars( aname )
# #            print "Selecting scalars array %s for input %d" % ( aname, array_index )
#         else:
#             print>>sys.stderr, "Error, can't find scalars array for input %d" % array_index
# #        print "Selecting %s (array-%d) for plot index %d" % ( aname, array_index, plotIndex)
#         return image_data
 
    def initializeInput( self, imageData, fieldData, plotIndex=0 ): 
        self._input =  imageData 
        self.fieldData = fieldData                          
        self.updateMetadata( plotIndex )
        
    def input( self ):
        if self.clipper:
            input = self.clipper.GetOutput()
            input.Update()
            return input
        return self._input
        
    def clipInput( self, extent ):
        self.clipper = vtk.vtkImageClip()
        self.clipper.AddInput( self._input )
        self.clipper.SetOutputWholeExtent( extent )

    def getWorldCoords( self, image_coords ):
        plotType = self.metadata[ 'plotType' ]                   
        world_coords = None
        try:
            if plotType == 'zyt':
                lat = self.metadata[ 'lat' ]
                lon = self.metadata[ 'lon' ]
                timeAxis = self.metadata[ 'time' ]
                tval = timeAxis[ image_coords[2] ]
                relTimeValue = cdtime.reltime( float( tval ), timeAxis.units ) 
                timeValue = str( relTimeValue.tocomp() )          
                world_coords = [ getFloatStr(lon[ image_coords[0] ]), getFloatStr(lat[ image_coords[1] ]), timeValue ]   
            else:         
                lat = self.metadata[ 'lat' ]
                lon = self.metadata[ 'lon' ]
                lev = self.metadata[ 'lev' ]
                world_coords = [ getFloatStr(lon[ image_coords[0] ]), getFloatStr(lat[ image_coords[1] ]), getFloatStr(lev[ image_coords[2] ]) ]   
        except:
            gridSpacing = self.input().GetSpacing()
            gridOrigin = self.input().GetOrigin()
            world_coords = [ getFloatStr(gridOrigin[i] + image_coords[i]*gridSpacing[i]) for i in range(3) ]
        return world_coords

    def getWorldCoordsAsFloat( self, image_coords ):
        plotType = self.metadata[ 'plotType' ]                   
        world_coords = None
        try:
            if plotType == 'zyt':
                lat = self.metadata[ 'lat' ]
                lon = self.metadata[ 'lon' ]
                timeAxis = self.metadata[ 'time' ]
                tval = timeAxis[ image_coords[2] ]
                relTimeValue = cdtime.reltime( float( tval ), timeAxis.units ) 
                timeValue = str( relTimeValue.tocomp() )          
                world_coords = [ lon[ image_coords[0] ], lat[ image_coords[1] ], timeValue ]   
            else:         
                lat = self.metadata[ 'lat' ]
                lon = self.metadata[ 'lon' ]
                lev = self.metadata[ 'lev' ]
                world_coords = [ lon[ image_coords[0] ], lat[ image_coords[1] ], lev[ image_coords[2] ] ]   
        except:
            gridSpacing = self.input().GetSpacing()
            gridOrigin = self.input().GetOrigin()
            world_coords = [ gridOrigin[i] + image_coords[i]*gridSpacing[i] for i in range(3) ]
        return world_coords
    
    def getWorldCoord( self, image_coord, iAxis, latLonGrid  ):
        plotType = self.metadata[ 'plotType' ] 
        if plotType == 'zyt':                  
            axisNames = [ 'Longitude', 'Latitude', 'Time' ] if latLonGrid else [ 'X', 'Y', 'Time' ]
        else:
            axisNames =  [ 'Longitude', 'Latitude', 'Level' ] if latLonGrid else [ 'X', 'Y', 'Level' ]
        try:
            axes = [ 'lon', 'lat', 'time' ] if plotType == 'zyt'  else [ 'lon', 'lat', 'lev' ]
            world_coord = self.metadata[ axes[iAxis] ][ image_coord ]
            if ( plotType == 'zyt') and  ( iAxis == 2 ):
                timeAxis = self.metadata[ 'time' ]     
                timeValue = cdtime.reltime( float( world_coord ), timeAxis.units ) 
                world_coord = str( timeValue.tocomp() )          
            return axisNames[iAxis], getFloatStr( world_coord )
        except:
            if (plotType == 'zyx') or (iAxis < 2):
                gridSpacing = self.input().GetSpacing()
                gridOrigin = self.input().GetOrigin()
                return axes[iAxis], getFloatStr( gridOrigin[iAxis] + image_coord*gridSpacing[iAxis] ) 
            return axes[iAxis], ""

    def getRangeBounds( self ):
        if self.dtype == "Float": 
            return self.scalarRange
        return self.rangeBounds  
        
    def getDataRangeBounds(self):
        if self.dtype == "Float":
            return self.scalarRange
        if self.rangeBounds:
            srange = self.getDataValues( self.rangeBounds[0:2] ) 
            if ( len( self.rangeBounds ) > 2 ): srange.append( self.rangeBounds[2] ) 
            else:                               srange.append( 0 )
        else: srange = [ 0, 0, 0 ]
        return srange
        
    def raiseModuleError( self, msg ):
        print>>sys.stderr, msg
        raise Exception( msg )

    def getDataValue( self, image_value):
        if self.isFloat(): return image_value
        if not self.scalarRange: 
            self.raiseModuleError( "ERROR: no variable selected in dataset input to module %s" % getClassName( self ) )
        valueRange = self.scalarRange
        sval = ( float(image_value) - self.rangeBounds[0] ) / ( self.rangeBounds[1] - self.rangeBounds[0] )
        dataValue = valueRange[0] + sval * ( valueRange[1] - valueRange[0] ) 
#        print " GetDataValue(%.3G): valueRange = %s " % ( sval, str( valueRange ) )
        return dataValue
                
    def getDataValues( self, image_value_list ):
        if self.isFloat(): return image_value_list
        if not self.scalarRange: 
            self.raiseModuleError( "ERROR: no variable selected in dataset input to module %s" % getClassName( self ) )
        valueRange = self.scalarRange
        dr = ( self.rangeBounds[1] - self.rangeBounds[0] )
        data_values = []
        for image_value in image_value_list:
            sval = 0.0 if ( dr == 0.0 ) else ( image_value - self.rangeBounds[0] ) / dr
            dataValue = valueRange[0] + sval * ( valueRange[1] - valueRange[0] ) 
            data_values.append( dataValue )
        return data_values

    def getImageValue( self, data_value ):
        if not self.scalarRange: 
            self.raiseModuleError( "ERROR: no variable selected in dataset input to module %s" % getClassName( self ) )
        valueRange = self.scalarRange
        dv = ( valueRange[1] - valueRange[0] )
        sval = 0.0 if ( dv == 0.0 ) else ( data_value - valueRange[0] ) / dv 
        imageValue = self.rangeBounds[0] + sval * ( self.rangeBounds[1] - self.rangeBounds[0] ) 
        return imageValue

    def getImageValues( self, data_value_list ):
        if self.isFloat(): return data_value_list
        if not self.scalarRange: 
            self.raiseModuleError( "ERROR: no variable selected in dataset input to module %s" % getClassName( self ) )
        valueRange = self.scalarRange
        dv = ( valueRange[1] - valueRange[0] )
        imageValues = []
        for data_value in data_value_list:
            sval = 0.0 if ( dv == 0.0 ) else ( data_value - valueRange[0] ) / dv
            imageValue = self.rangeBounds[0] + sval * ( self.rangeBounds[1] - self.rangeBounds[0] ) 
            imageValues.append( imageValue )
#        print "\n *****************  GetImageValues: data_values = %s, range = %s, imageValues = %s **************** \n" % ( str(data_value_list), str(self.scalarRange), str(imageValues) )
        return imageValues

    def scaleToImage( self, data_value ):
        if self.isFloat(): return data_value
        if not self.scalarRange: 
            self.raiseModuleError( "ERROR: no variable selected in dataset input to module %s" % getClassName( self ) )
        dv = ( self.scalarRange[1] - self.scalarRange[0] )
        sval = 0.0 if ( dv == 0.0 ) else data_value / dv
        imageScaledValue =  sval * ( self.rangeBounds[1] - self.rangeBounds[0] ) 
        return imageScaledValue

    def getMetadata( self, key = None ):
        return self.metadata.get( key, None ) if ( key and self.metadata )  else self.metadata
  
    def getFieldData( self ):
        if self.fieldData == None:
            print>>sys.stderr, ' Uninitialized field data being accessed in ispec[%x]  ' % id(self)  
            self.initializeMetadata()
        return self.fieldData  
    
    def updateMetadata( self, plotIndex ):
        if self.metadata == None:
            scalars = None
             
#            arr_names = [] 
#            na = self.fieldData.GetNumberOfArrays()
#            for iF in range( na ):
#                arr_names.append( self.fieldData.GetArrayName(iF) )
#            print " updateMetadata: getFieldData, arrays = ", str( arr_names ) ; sys.stdout.flush()
            
            if self.fieldData == None:
                print>>sys.stderr,  ' NULL field data in updateMetadata: ispec[%x]  ' % id(self)  
                self.initializeMetadata() 
    
            self.metadata = self.computeMetadata( plotIndex )
            
            if self.metadata <> None:
                self.rangeBounds = None              
                self.datasetId = self.metadata.get( 'datasetId', None )                
                tval = self.metadata.get( 'timeValue', 0.0 )
                self.referenceTimeUnits = self.metadata.get( 'timeUnits', None )
                self.timeValue = cdtime.reltime( float( tval ), self.referenceTimeUnits ) if tval else None              
                self.dtype =  self.metadata.get( 'datatype', None )
                scalars =  self.metadata.get( 'scalars', None )
                self.rangeBounds = getRangeBounds( self.dtype )
                title = self.metadata.get( 'title', None )
                if title:
                    targs = title.split(':')
                    if len( targs ) == 1:
                        self.titleBuffer = "\n%s" % ( title )
                    elif len( targs ) > 1:
                        self.titleBuffer = "%s\n%s" % ( targs[1], targs[0] )
                else: self.titleBuffer = "" 
                attributes = self.metadata.get( 'attributes' , None )
                if attributes:
                    self.units = attributes.get( 'units' , '' )
                    srange = attributes.get( 'range', None )
                    if srange: 
        #                print "\n ***************** ScalarRange = %s, md[%d], var_md[%d] *****************  \n" % ( str(range), id(metadata), id(var_md) )
                        self.scalarRange = list( srange )
                        self.scalarRange.append( 1 )
                        if not self.seriesScalarRange:
                            self.seriesScalarRange = list(srange)
                        else:
                            if self.seriesScalarRange[0] > srange[0]:
                                self.seriesScalarRange[0] = srange[0] 
                            if self.seriesScalarRange[1] < srange[1]:
                                self.seriesScalarRange[1] = srange[1] 

    def getUnits(self):
        return self.units
    
    def getLayerList(self):
        layerList = []
        pointData = self.input().GetPointData()
        for iA in range( pointData.GetNumberOfArrays() ):
            array_name = pointData.GetArrayName(iA)
            if array_name: layerList.append( array_name )
        return layerList
    
    def computeMetadata( self, plotIndex=0 ):
        if not self.fieldData: self.initializeMetadata() 
        if self.fieldData:
            mdList = extractMetadata( self.fieldData )
            if plotIndex < len(mdList):
                return mdList[ plotIndex ]
            else:
                try: return mdList[ 0 ]
                except: pass               
        print>>sys.stderr, "[%s]: Error, Metadata for input %d not found in ispec[%x]  "  % ( self.__class__.__name__,  plotIndex, id(self) )
        return {}
        
    def addMetadataObserver( self, caller, event ):
        fd = caller.GetOutput().GetFieldData()
        fd.ShallowCopy( self.fieldData )
        pass

    def initializeMetadata( self ):
        try:
            self.fieldData = vtk.vtkDataSetAttributes()
            mdarray = getStringDataArray( 'metadata' )
            self.fieldData.AddArray( mdarray )
#            diagnosticWriter.log( self, ' initialize field data in ispec[%x]  ' % id(self) )  
        except Exception, err:
            print>>sys.stderr, "Error initializing metadata"

    def addMetadata( self, metadata ):
        dataVector = self.fieldData.GetAbstractArray( 'metadata' ) 
        if dataVector == None:
            cname = getClassName( self ) 
            if cname <> "InputSpecs": print " Can't get Metadata for class %s " % cname
        else:
            enc_mdata = encodeToString( metadata )
            dataVector.InsertNextValue( enc_mdata  )
            
def getClassName( instance ):
    return instance.__class__.__name__ if ( instance <> None ) else "None" 

def bound( val, bounds ): return max( min( val, bounds[1] ), bounds[0] )

def getRangeBounds( type_str ):
    if type_str == 'UShort':
        return [ 0, 65535, 1 ]
    if type_str == 'UByte':
        return [ 0, 255, 1 ] 
    if type_str == 'Float':
        f = np.finfo(float) 
        return [ -f.max, f.max, 1 ]
    return None

def extractMetadata( fieldData ):
    mdList = []
    inputVarList = []
    varlist = fieldData.GetAbstractArray( 'varlist' ) 
    if varlist == None:   # module.getFieldData() 
        print>>sys.stderr, " Can't get Metadata!" 
    else: 
        nvar = varlist.GetNumberOfValues()
        for vid in range(nvar):
            varName = str( varlist.GetValue(vid) )
            inputVarList.append( varName )
            dataVector = fieldData.GetAbstractArray( 'metadata:%s' % varName ) 
            if dataVector == None:  
                print>>sys.stderr, " Can't get Metadata for var %s!" % varName 
            else: 
                metadata = {}
                nval = dataVector.GetNumberOfValues()
                for id in range(nval):
                    enc_mdata = str( dataVector.GetValue(id) )
                    md = decodeFromString( enc_mdata )
                    metadata.update( md )
                mdList.append( metadata )
        for md in mdList: md['inputVarList'] = inputVarList
    return mdList 