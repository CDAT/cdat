'''
Created on Aug 28, 2014

@author: tpmaxwel
'''


import cdms2, cdutil, genutil
import vcs, os, sys, shutil, collections
pth = os.path.join(os.path.dirname(__file__),"..")
sys.path.append(pth)
import checkimage

DefaultSampleFile = "geos5-sample.nc"
DefaultSampleVar = "uwnd"

class TestManager:
    
    DefinedTests = collections.OrderedDict()
    
    def __init__( self ):
        pass
    
    def reviewTests(self):
        for (testName, test) in TestManager.DefinedTests.items():
            self.reviewTest(testName)
        print " Finished reviewing tests, update CMakeLists? (y/n)" 
        line = sys.stdin.readline() 
        if line[0] == 'y': self.writeCMakeLists()


    def reviewTest(self, testName ):
        print "Running test: ", testName
        os.system("python %s.py -i" % testName ) 
        
    def writeCMakeLists(self):
        f = open( 'CMakeLists.txt', 'w' )
        for (testName, test) in TestManager.DefinedTests.items():
            test.writeCMakeDef( f )
        f.close()
        
    def runTest(self, testName, interactive=False, baselinedir=None ):
        test = TestManager.DefinedTests.get( testName, None )
        if test == None:
            print>>sys.stderr, "Can't find test named %s" % testName
            return -1
        if baselinedir is not None:
            test.test_dir = baselinedir
            test.image_name = os.path.join( test.test_dir, 'images', '.'.join( [ test.name, 'png' ] )  )
        test.test( interactive )

    def showTest(self, testName ):
        test = TestManager.DefinedTests.get( testName, None )
        if test == None:
            print>>sys.stderr, "Can't find test named %s" % testName
            return -1
        test.show()
        line = sys.stdin.readline()

    def runTests( self ):  
        for test in TestManager.DefinedTests.keys():
            self.runTest( test, True )      
                   
class vcsTest:
        
    def __init__( self, name, **args ):
        self.name = name
        self.test_dir = os.path.dirname(__file__)
        parent_dir = os.path.join( self.test_dir, ".."  )
        sys.path.append( parent_dir )
        self.image_name = os.path.join( self.test_dir, 'images', '.'.join( [ self.name, 'png' ] )  )
        filename = args.get( 'file', DefaultSampleFile )
        self.varnames = args.get( 'vars', [ DefaultSampleVar ] )
        self.file_path = os.path.join( sys.prefix, "sample_data", filename )
        self.file = cdms2.open( self.file_path )
        self.roi =  args.get( 'roi', None )
        self.ptype = args.get( 'type', 'scalar' )
        self.template = args.get( 'template', 'default' )
        self.parameters = args.get( 'parameters', {} )
        self.actions = args.get( 'actions', [ 'test' ] )
        TestManager.DefinedTests[ name ] = self
        
    def build(self):
        print "Processing vars %s from file %s" % ( str(self.varnames), self.file_path )
        plot_args = []
        for varname in self.varnames:
            var = self.file[varname] 
            var = var( lon=( self.roi[0], self.roi[1] ), lat=( self.roi[2], self.roi[3] ), squeeze=1, ) if self.roi else var(  squeeze=1, )
            plot_args.append( var )
            
        self.canvas = vcs.init()
        self.canvas.drawlogooff()
        self.gm = vcs.get3d_scalar( self.template ) if ( self.ptype == 'scalar' ) else vcs.get3d_vector( self.template )        
        for pitem in self.parameters.items():
            self.gm.setParameter( pitem[0], pitem[1] )
        plot_args.append( self.gm )
            
        plot_kwargs = { 'cdmsfile': self.file.id, 'window_size': (900,600) }
        self.canvas.setantialiasing(False)
        self.canvas.plot( *plot_args, **plot_kwargs )
        self.plot = self.canvas.backend.plotApps[ self.gm ]
#        self.applyActions()
        
    def applyActions(self):
        for action in self.actions:
            self.plot.applyAction( action )
        
    def show(self):
        self.build()
#        self.canvas.interact()

    def interact(self):
        self.build()
        self.canvas.interact()
        
    def test( self, interactive=False ):      
        self.build()
#        test_image = os.path.join( self.test_dir, 'images', '.'.join( [ self.name, 'png' ] ) )
        test_image = '.'.join( [ self.name, 'test', 'png' ] )
        ref_image  = '.'.join( [ self.name, 'png' ] )
        self.canvas.png( test_image )
        try:
            shutil.copy( self.image_name, ref_image )
            print "Copying ref image %s to %s in %s " % ( self.image_name, ref_image, os.path.abspath('.') )
            ret = checkimage.check_result_image( ref_image, test_image, checkimage.defaultThreshold )
        except IOError:
            print "No ref image found."
            ret = 0
        if  interactive: 
            print "Type <Enter> to continue and update ref image ( type 'n' to skip update )." 
            sys.stdout.flush()
            line = sys.stdin.readline()
            if line[0] <> 'n':  self.update_image() 
        sys.exit(ret)
        
    def update_image(self):
        print "Saving reference image to %s " % self.image_name       
        self.canvas.png( self.image_name )
        
    def writeCMakeDef( self, f ):
        f.write( "add_test(%s\n" % self.name )
        f.write( "  ${CMAKE_INSTALL_PREFIX}/bin/python\n"  )
        f.write( "  ${cdat_SOURCE_DIR}/testing/dv3d/%s.py\n" % self.name )
#        f.write( "  ${cdat_SOURCE_DIR}/testing/dv3d/images/%s.png\n" % self.name )
        f.write( ")\n\n\n")
        source_file = os.path.join( self.test_dir, "%s.py" % self.name )
        f1 = open( source_file, 'w' )
        f1.write( "import sys\n")
        f1.write( "from TestDefinitions import testManager\n"  )
        f1.write( "interactive = ( len(sys.argv) > 1 ) and ( sys.argv[1] == '-i' )\n")
        f1.write( "testManager.runTest( '%s', interactive )\n" % self.name )
        f1.close()
        
        
if __name__ == '__main__':
    from TestDefinitions import testManager    
#    testManager.runTests()
    testManager.runTest( 'dv3d_hovmoller_test' )
#    testManager.showTest( 'dv3d_constituents_test' )
