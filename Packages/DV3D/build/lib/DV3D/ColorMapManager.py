'''
Created on Dec 3, 2010
Hacked from the Enthought MayaVi2 lut_manager
@author: tpmaxwel
'''

import os.path
import sys, vtk, copy
import cPickle

pkl_path = os.path.join( os.path.dirname( __file__ ), 'data', 'colormaps.pkl' )
colormap_file = open( pkl_path, 'rb' )
colormaps = cPickle.load( colormap_file )
colormap_file.close()

VTK_BACKGROUND_COLOR =  ( 1.0, 1.0, 1.0 ) # ( 0.0, 0.0, 0.0 )
VTK_FOREGROUND_COLOR =  ( 0.0, 0.0, 0.0 ) # ( 1.0, 1.0, 1.0 )

class AlphaManager():
    
    def __init__( self ): 
        self.graph_data = None
        self.alpha_range = [ 0.0, 1.0 ]
        self.ascale = None
        self.n_col = None
        self.graphEnabled = False
        self.currentGraphNode = 0

    def setNumberOfColors( self, n_col ):
        self.n_col = n_col
        self.ascale = ( self.alpha_range[1] - self.alpha_range[0] ) / ( n_col - 1 )

    def getAlphaRange(self ):
        return self.alpha_range
        
    def setAlphaRange(self, arange ):
        self.alpha_range = arange
        if self.n_col: self.ascale = ( self.alpha_range[1] - self.alpha_range[0] ) / ( self.n_col - 1 )
        self.graphEnabled = False
        
    def enableGraph( self, enable ):
        self.graphEnabled = enable
        
    def setGraphData( self, data ):
        print " set Graph Data: ", str(data); sys.stdout.flush()
        self.graph_data = data
        self.graphEnabled = True
        
    def getAlphaValue( self, iCol ):
        assert self.n_col, "Must call setNumberOfColors" 
        if self.graphEnabled:
            dval = iCol / float( self.n_col - 1 )
            if iCol == 0: 
                self.currentGraphNode = 0
                gn0 = self.graph_data[0] 
                return gn0[1]
            elif iCol == ( self.n_col - 1 ): 
                self.currentGraphNode = 0
                gn0 = self.graph_data[-1] 
                return gn0[1]
#            print " getAlphaValue [%d/%d]: %f, ig = %d, ng = %d: %s " % ( iCol, self.n_col, dval, self.currentGraphNode, len(self.graph_data), str(self.graph_data) ); sys.stdout.flush()
            gn1 = self.graph_data[self.currentGraphNode+1]
            if dval >= gn1[0]:
                self.currentGraphNode = self.currentGraphNode+1
                gn1 = self.graph_data[self.currentGraphNode+1]
            gn0 = self.graph_data[self.currentGraphNode]            
            s = ( gn1[1] - gn0[1] ) / ( gn1[0] - gn0[0] )
            return gn0[1] + s * ( dval - gn0[0] )
        else:
            return self.alpha_range[0] + iCol*self.ascale

class ColorMapManager():
    
    def __init__(self, lut, display_lut = None, **args ): 
        self.lut = lut   
        self.display_lut = vtk.vtkLookupTable() 
        self.number_of_colors =  args.get('ncolors',256)
        self.alphaManager = AlphaManager()
        self.colormapName = 'Spectral'
        self.colorBarActor = None
        self.invertColormap = 1
        self.smoothColormap = 1

    def setColorbarVisibility( self, isVisible ):
        if self.colorBarActor:
            if  isVisible:  self.colorBarActor.VisibilityOn()  
            else:           self.colorBarActor.VisibilityOff() 

    def toggleColorbarVisibility(self, **args ):
        if self.colorBarActor:
            makeVisible = args.get( 'state', not self.colorBarActor.GetVisibility() )
            if  makeVisible:    self.colorBarActor.VisibilityOn()  
            else:               self.colorBarActor.VisibilityOff() 

    def createActor( self, **args ):
        if self.colorBarActor == None:
            pos = args.get( 'pos', [ 0.9, 0.2 ] )
            title = args.get( 'title', '' )
            self.colorBarActor = vtk.vtkScalarBarActor()
#            self.colorBarActor.SetMaximumWidthInPixels( 50 )
            self.colorBarActor.SetNumberOfLabels(9)
            labelFormat = vtk.vtkTextProperty()
            labelFormat.SetFontSize( 160 )
            labelFormat.SetColor(  VTK_FOREGROUND_COLOR[0], VTK_FOREGROUND_COLOR[1], VTK_FOREGROUND_COLOR[2] ) 
            titleFormat = vtk.vtkTextProperty()
            titleFormat.SetFontSize( 160 )
            titleFormat.SetColor(  VTK_FOREGROUND_COLOR[0], VTK_FOREGROUND_COLOR[1], VTK_FOREGROUND_COLOR[2]  ) 
#            titleFormat.SetVerticalJustificationToTop ()
#            titleFormat.BoldOn()
            self.colorBarActor.SetPosition( pos[0], pos[1] )    
            self.colorBarActor.SetLabelTextProperty( labelFormat )
            self.colorBarActor.SetTitleTextProperty( titleFormat )
            self.colorBarActor.SetTitle( title )
            self.colorBarActor.SetLookupTable( self.getDisplayLookupTable() )
            self.colorBarActor.SetVisibility(0)
            self.colorBarActor.SetMaximumWidthInPixels(75)
        else:
            self.colorBarActor.SetLookupTable( self.getDisplayLookupTable() )
            self.colorBarActor.Modified() 
        return self.colorBarActor
       
    def getAlphaRange( self ):
        return self.alphaManager.getAlphaRange()
        
    def setAlphaRange( self, arange ):
        self.alphaManager.setAlphaRange(arange) 
        self.load_lut()

    def setAlphaGraph(self, data ):
        self.alphaManager.setGraphData( data ) 
        self.load_lut()
     
    @staticmethod
    def getColormaps():
        return colormaps

    @staticmethod
    def getColormapNames():
        return colormaps.keys()
    
    def getDisplayLookupTable(self):
        return self.display_lut
    
    def getImageScale(self):
        return self.lut.GetTableRange()
    
    def setScale( self, imageRange, displayRange  ):
        self.lut.SetTableRange( imageRange[0], imageRange[1] ) 
        self.lut.Modified()
        self.setDisplayRange( displayRange )
  
    def setDisplayRange( self, dataRange ):
        self.display_lut.SetTableRange( dataRange[0], dataRange[1] )
        self.display_lut.Modified()

    def getDisplayRange( self ):
        return self.display_lut.GetTableRange()

    def matchDisplayRange( self, range ):
        trange = self.display_lut.GetTableRange()
        return ( trange[0] == range[0] ) and ( trange[1] == range[1] )
   
    def set_lut(self, vtk_lut, lut_lst):
        """Setup the vtkLookupTable (`vtk_lut`) using the passed list of
        lut values."""
        n_col = len(lut_lst)
        vtk_lut.SetNumberOfColors( n_col )
        vtk_lut.Build()
        self.alphaManager.setNumberOfColors( n_col )
        for i in range(0, n_col):
            lt = lut_lst[i]
            alpha = self.alphaManager.getAlphaValue( i )
            vtk_lut.SetTableValue(i, lt[0], lt[1], lt[2], alpha )
    
    def check_lut_first_line(self, line, file_name=''):
        """Check the line to see if this is a valid LUT file."""
        first = line.split()
        if first[0] != "LOOKUP_TABLE":
            errmsg = "Error: The input data file \"%s\"\n"%(file_name)
            errmsg = errmsg+ "is not a proper lookup table file."\
                     " No LOOKUP_TABLE tag in first line. Try again."
            raise IOError, errmsg
        try:
            n_color = first[2]
        except:
            
            raise IOError, "Error: No size for LookupTable specified."
        else:
            return n_color
    
    def parse_lut_file(self, file_name):
        """Parse the file specified by its name `file_name` for a LUT and
        return the list of parsed values."""
        
        input = open(file_name, "r")
    
        line = input.readline()
        n_color = self.check_lut_first_line(line, file_name)
    
        lut = []
        for line in input.readlines():
            entr = line.split()
            if len(entr) != 4:
                errmsg="Error: insufficient or too much data in line "\
                        "-- \"%s\""%(entr)
                raise IOError, errmsg
    
            tmp = []
            for color in entr:
                try:
                    tmp.append(float(color))
                except:
                    raise IOError, \
                          "Unknown entry '%s'in lookup table input."%color
            lut.append(tmp)
    
        return lut
    
    def load_lut_from_file(self, file_name):
        lut_list = []
        if len(file_name) > 0:
            try:
                f = open(file_name, 'r')
            except IOError:
                msg = "Cannot open Lookup Table file: %s\n"%file_name
                print>>sys.stderr, msg
            else:
                f.close()
                try:
                    lut_list = self.parse_lut_file(file_name)
                except IOError, err_msg:
                    msg = "Sorry could not parse LUT file: %s\n" % file_name
                    msg += str( err_msg )
                    raise IOError, msg
                else:
                    if self.invertColormap:
                        lut_list.reverse()
                    self.lut = self.set_lut(self.lut, lut_list)
                    
    def load_lut_from_list(self, list):
        self.set_lut(self.lut, list) 
        self.lut.Modified()  
                
    def getColor( self, dval ):
        color = [ 0, 0, 0 ]
        self.lut.GetColor( dval, color )  
        return color           

    def load_lut(self, value=None):
        if( value <> None ): self.colormapName = str( value )       
        if self.colormapName == 'file':
            if self.file_name:
                self.load_lut_from_file(self.file_name)        
        elif self.colormapName in colormaps:
            lut = self.load_array()
            self.load_lut_from_list(lut.tolist())
        else:
            print>>sys.stderr, "Error-- Unrecognized colormap: %s" % self.colormapName
            
        self.display_lut.SetTable( self.lut.GetTable() )
        self.display_lut.SetValueRange( self.lut.GetValueRange() )
        self.display_lut.Modified()

    def load_array(self, name=None):
        if( name <> None ): 
            self.colormapName = str( name )
        reverse = self.invertColormap
        if self.colormapName in colormaps:
            lut = colormaps[self.colormapName]
            if reverse:
                lut = lut[::-1, :]
            n_total = len(lut)
            n_color = self.number_of_colors
            if not n_color >= n_total:
                lut = lut[::round(n_total/float(n_color))]
        else:
            print>>sys.stderr, "Error-- Unrecognized colormap: %s" % self.colormapName  
            return None         
        return lut

 