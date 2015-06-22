'''
Created on Aug 28, 2014

@author: tpmaxwel
'''


import cdms2, vcs, os, sys

DefaultSampleFile = "geos5-sample.nc"
DefaultSampleVar = "uwnd"

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

    def run( self ):
        self.build()
        print "Type <Enter> to exit."
        sys.stdout.flush()
        line = sys.stdin.readline()
        sys.exit(0)
