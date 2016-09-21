'''
Created on Aug 28, 2014

@author: tpmaxwel
'''


import cdms2, cdutil, genutil, ast
import vcs, os, sys, shutil, collections, subprocess
TestingDir=os.path.dirname(__file__)
pth = os.path.join(TestingDir,"..")
sys.path.append(pth)
import vcs.testing.regression as regression

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
        os.system('python dv3d_execute_test.py %s True' % testName )

    def writeCMakeLists(self):
        f = open( 'CMakeLists.txt', 'w' )
        f.write('set(BASELINE_DIR "${UVCDAT_GIT_TESTDATA_DIR}/baselines/dv3d")\n\n')
        for (testName, test) in TestManager.DefinedTests.items():
            test.writeCMakeDef( f )
        f.close()

    def runTest(self, testName, interactive=False, baselinedir=None ):
        test = TestManager.DefinedTests.get( testName, None )
        if test is None:
            print>>sys.stderr, "Can't find test named '%s'" % testName
            print>>sys.stderr, " Defined Tests = %s " % str( TestManager.DefinedTests.items() )
            return -1
        if baselinedir is not None:
            test.test_dir = baselinedir
            test.image_name = os.path.join( test.test_dir, 'images',\
                '.'.join( [ test.name, 'png' ] )  )
        print "Running test %s, interactive= %s, baselinedir= %s" % ( testName,\
                str(interactive), str(baselinedir) )
        test.test( ast.literal_eval(interactive) )


    def showTest(self, testName ):
        test = TestManager.DefinedTests.get( testName, None )
        if test is None:
            print>>sys.stderr, "Can't find test named %s" % testName
            print>>sys.stderr, " Defined Tests = %s " % str( TestManager.DefinedTests.items() )
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
        self.file_path = os.path.join( vcs.sample_data, filename )
        self.file = cdms2.open( self.file_path )
        self.roi =  args.get( 'roi', None )
        self.ptype = args.get( 'type', '3d_scalar' )
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
        self.gm = vcs.get3d_scalar( self.template ) if ( self.ptype == '3d_scalar' ) else vcs.get3d_vector( self.template )
        for pitem in self.parameters.items():
            self.gm.setParameter( pitem[0], pitem[1] )
        plot_args.append( self.gm )

        plot_kwargs = { 'cdmsfile': self.file.id, 'window_size': (900,600) }
        self.canvas.setantialiasing(False)
        display = self.canvas.plot( *plot_args, **plot_kwargs )
        self.plot = self.canvas.backend.plotApps[ vcs.elements[display.g_type][display.g_name] ]

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
        self.canvas.png( test_image, width = 900, height = 600 )

        ret = regression.check_result_image( test_image, self.image_name,\
                regression.defaultThreshold+3. )

        if  interactive:
            print "Type <Enter> to continue and update ref image ( type 'n' to skip update )."
            sys.stdout.flush()
            line = sys.stdin.readline()
            if line[0] <> 'n':
                self.update_image()
        sys.exit(ret)

    def update_image(self):
        self.update_baseline_repo()
        self.canvas.png( self.image_name, width = 900, height = 600 )
        baseline_dir = os.path.dirname( self.image_name  )
        ref_image =os.path.basename( self.image_name  )
        git_cmd = " cd %s; git add %s; git commit -a -m 'Adding ref image %s'; git push origin HEAD" %  ( baseline_dir, ref_image, ref_image )
        print "Saving reference image to %s, git cmd = '%s' " % ( self.image_name, git_cmd )
#        subprocess.call( git_cmd )

    def update_baseline_repo(self):
        repo_dir = os.path.dirname( os.path.dirname( self.test_dir ) )
        config_file = os.path.join( repo_dir, '.git', 'config' )
        cfile = open( config_file, 'r')
        redefine_repo = False
        for line in cfile:
            toks =  line.split('=')
            if (len(toks) > 1) and toks[0].find('url') != -1:
               if toks[1].find('git://') != -1:
                    redefine_repo = True
               break
        if redefine_repo:
            redef_cmd = "cd %s; rm -rf uvcdat-testdata; git clone https://github.com/UV-CDAT/uvcdat-testdata.git uvcdat-testdata" % os.path.dirname( repo_dir )
            print 'Executing: ', redef_cmd
            os.system( redef_cmd )

    def writeCMakeDef( self, f ):
        f.write( "add_test(%s\n" % self.name )
        f.write( "  \"${PYTHON_EXECUTABLE}\"\n"  )
        f.write( "  ${cdat_SOURCE_DIR}/testing/dv3d/dv3d_execute_test.py\n" )
        f.write( "  %s\n" % self.name )
        f.write( "  False\n" )
        f.write( "  ${BASELINE_DIR}\n" )
        f.write( ")\n\n\n")
