'''
Created on Aug 28, 2014

@author: tpmaxwel
'''


import cdms2, cdutil, genutil
import vcs, os, sys

DefaultSampleFile = "geos5-sample.nc"
DefaultSampleVar = "uwnd"

class TestManager:
    
    DefinedTests = []
    
    def __init__( self ):
        pass
    
    def reviewTests(self):
        for test in TestManager.DefinedTests:
            test.show()
            line = sys.stdin.readline()
            if line[0] == 'q': break
            if line[0] <> 'n': test.update_image() 
        print " Finished reviewing tests, update CMakeLists? (y/n)" 
        line = sys.stdin.readline() 
        if line[0] == 'y': self.writeCMakeLists()
        
    def writeCMakeLists(self):
        f = open( 'CMakeLists', 'w' )
        for test in TestManager.DefinedTests:
            test.writeCMakeDef( f )
        f.close()
                 
class vcsTest:
        
    def __init__( self, name, **args ):
        self.name = name
        self.image_name = '.'.join( [ self.name, 'png' ] )
        self.filename = args.get( 'file', DefaultSampleFile )
        self.varnames = args.get( 'vars', [ DefaultSampleVar ] )
        self.file = cdms2.open( os.path.join( sys.prefix, "sample_data", self.filename ) )
        self.roi =  args.get( 'roi', None )
        self.ptype = args.get( 'type', 'scalar' )
        self.template = args.get( 'template', 'default' )
        self.parameters = args.get( 'parameters', {} )
        self.interactions = args.get( 'interactions', None )
        TestManager.DefinedTests.append( self )
        
    def build(self):
        plot_args = []
        for varname in self.varnames:
            var = self.file[varname] 
            var = var( lon=( self.roi[0], self.roi[1] ), lat=( self.roi[2], self.roi[3] ), squeeze=1, ) if self.roi else var(  squeeze=1, )
            plot_args.append( var )
            
        self.canvas = vcs.init()
        self.gm = vcs.get3d_scalar( self.template ) if ( self.ptype == 'scalar' ) else vcs.get3d_vector( self.template )        
        for pitem in self.parameters.items():
            self.gm.setParameter( pitem[0], pitem[1] )
        plot_args.append( self.gm )
            
        plot_kwargs = { 'cdmsfile': self.file.id }
        self.canvas.plot( *plot_args, **plot_kwargs )
        self.plot = self.canvas.backend.plotApps[ self.gm ]
        if self.interactions: self.interactions(self)
        
    def show(self):
        self.build()
#        self.canvas.interact()
        
    def test( self, interactive=False ):        
        import checkimage
        test_image = '.'.join( [ self.name, 'test', 'png' ] )
        self.canvas.png( test_image )
        ret = checkimage.check_result_image( self.image_name, test_image, 0.05 )
        print " Image Test returned:  %d " % ret
        if not interactive: sys.exit(ret)
        
    def update_image(self):
        print "Saving reference image to %s " % self.image_name       
        self.canvas.png( self.image_name )
        
    def writeCMakeDef( self, f ):
        f.write( "add_test(%s\n" % self.name )
        f.write( "  ${CMAKE_INSTALL_PREFIX}/bin/python\n"  )
        f.write( "  ${cdat_SOURCE_DIR}/testing/dv3d/%s.py\n" % self.name )
        currdir = os.path.dirname(__file__)
        source_file = os.path.join( currdir, "%s.py" % self.name )
        f1 = open( source_file, 'w' )
        f1.write( "from TestManager import %s\n" % self.__class__.__name__ )
        f1.write( "%s.test()\n" % self.__class__.__name__ )
        f1.close()
        
        



        
    