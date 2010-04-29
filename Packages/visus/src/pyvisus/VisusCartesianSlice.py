import pyvisus
from pyvisus.core import *
from pyvisus.gui import *
from pyvisus.data import *
from pyvisus.component import * 
from pyvisus.display import VisusOrthogonalSlice, VisusIndexedDisplay, VisusMeshDisplay
from pyvisus.shared import VisusSharedDataRequest
from pyvisus.extract import VisusIsoSurface
import VisusFunctions

#~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~#
# Class to create a cartesian slice 
#~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~#
class CartesianSlice:
  """
  Class to colormap, displace, isosurface, etc on the cartesian slice
  """
  _interface_info = """
Select a variable which for to
see the slice
"""

  _min_variables = 1
  _max_variables = 1
  
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
  # Construct the slice by passing in a dataArray, optional attributes, 
  # an optional root, and a flag to create a new fltk window 
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
  def __init__( self, dataArray, title=None, usrAttribs=dict(), root=None,
                newWindow = True):

    # Creating the default root within the __init__ call somehow
    # causes the root variable to persist and only a single root to be
    # created for multiple scenes. To avoid this we use the explicit
    # None initialization
    if root == None:
      root = VisusGroup.construct()
      #root = VisusSceneNode.construct()
    
    # Create a root node
    self._root = root
   # self._root.drawBoundingBox(False)  
    
    # Create the attributes
    self._attribs = dict()

    # Create the extractor, data description and update attribs 
    self.setData(dataArray, usrAttribs)

    # Attach the extractor to the root
    self._root.attachSubTree(self._extractor, True)

    self._extractor.propagateUpwards(VISUS_GLOBALTIME_TYPEID);

    # Create the drawing window
    if newWindow:
      print "Creating new window of root ", root
      createWindow(self._root)

    # Create the node for the slice of data
    self._orthoSlice = VisusOrthogonalSlice.construct()

    # Connect the output of the extractor to the input of each slice node
    if not self._orthoSlice.connectInput(self._extractor):
      raise RuntimeError("unable to connect extractor1 as slice input")
  
    # Attach the ortho slice to the root as a subtree
    self._extractor.attachSubTree(self._orthoSlice)

    # Inherit/Share the data request
    # Now that we have positioned the slices where we want them we want
    # to freeze their position. To do this we simply uninherit the data
    self._orthoSlice.inherit(VisusSharedDataRequest.typeIndex(),True)

    # This should be done autmoatically in the SliceNode
    # Set the correct bounding box for the slice
    # request = VisusDataRequest()
    # self._orthoSlice.getValue(request)
    # self._orthoSlice.setValue(request.queryRegion())
  
    # Set the colormap
    #self.setColorMap(ice())
    self.setColorMap(bgry())
    
    # Draw the bounding boxes of the slice
    self._orthoSlice.drawBoundingBox(True) 

    # Set up the annotations for the colorbar etc
    self.setAnnote()

    # Add the world map
    world_map = self.loadWorldMap()

    # And parse it as a collection of polylines. A VisusIndexedData
    # object stores an array of n-dimensional points (n being 2 in this
    # case) and a set of elements defined as ordered lists of indices
    # into this array. Therefore, each contour of the world map is
    # parsed as a single element
    outlines = VisusIndexedData(2)
    for m in world_map:
      outlines.constructElement(m)
      
    # Once the data is constructed we create a display node designed to visualize it
    self._map = VisusIndexedDisplay.construct()

    # And load the data. 
    self._map.loadData(outlines,0)
    
    # Move the map to the top of the data set so that is it always above the slice
    matrix = translationMatrix(0,0,0)
    self._map.getValue(matrix)
    matrix = matrix * translationMatrix(0,0,0.01)

    # This is a really ugly hack to adjust for the [0,360] [-180,180] ambiguity
    if self._bbox[0] > -90:
      matrix = matrix * translationMatrix(180,0,0)

    self._map.setValue(matrix)

    # Attach the map as child of the extractor. Note, that from this moment
    # on the map will inherit all transformations from the extractor and
    # thus "stick" to the extractor if it is moved or rotated
    self._orthoSlice.attachSubTree(self._map)
    #self._extractor.attachSubTree(self._map)

    # Add a title to the scene
    if(title==None):
      if "name" in self._attribs:
        title = self._attribs["name"].replace("_", " ")
      else:
        title = "Title"

    font = VisusFont()
    font.fontSize(12);
    self._title = VisusLabelNode.construct()
    self._title.text(title)
    self._title.setValue(font)
    self._title.position( 0, 0.92)
    self._root.attachSubTree(self._title,True)


  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
  #  Get the title
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
  def getTitle( self ):
    return self._title

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
  #  Set the title
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
  def setTitle( self, mTitle):
    self._title = mTitle

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
  # Take in the data and some attributes and get the extractor etc
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
  def setData( self,dataArray,usrAttribs=dict() ):
    
    # If the user passed in attributes, use those
    if(usrAttribs):
      self._attribs = usrAttribs
        
    # Create the extractor, description and update the attributes 
    self._extractor, self._bbox, self._attribs, self._descrip = VisusFunctions.createExtractor(dataArray,
                                                                                               self._attribs)
    
    
    # Set the bounding box of the root
    self._root.setValue(self._bbox)
    

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
  #  Change the colormap
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
  def setColorMap( self, colormap ):
    self._colorMap = colormap
    self._orthoSlice.setValue(self._colorMap)


  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
  # Set the labels of the x and y axes of the slice
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
  def setAnnote( self ):
    #  ----- Create a font ----
    font = VisusFont()
    font.fontSize(6)

    # Create Tick Marks
    xTickMarks = VisusTickMarks.construct()
    axis = xTickMarks.axis()
    axis.minValue(-180)
    axis.maxValue(180)
    axis.legendText("Longitude")
    axis.labelFont(font)
    axis.legendFont(font)
    axis.drawLegend(False)
    xTickMarks.axis(axis)
    self._extractor.attachSubTree(xTickMarks)
    
    # Create Tick Marks
    yTickMarks = VisusTickMarks.construct()
    yTickMarks.direction(TM_Y_AXIS)
    axis = yTickMarks.axis()
    axis.legendText("Latitude")
    axis.minValue(-80)
    axis.maxValue(80)
    axis.labelFont(font)
    axis.legendFont(font)
    axis.drawLegend(False)
    yTickMarks.axis(axis)
    self._extractor.attachSubTree(yTickMarks)
 

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
  # Load a map of the world as lines
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
  def loadWorldMap( self ):
    import cdutil.worldmap as worldmap

    outlines = []
    for area in dir(worldmap):
      
      if area[:4]=='farc':
        exec('area = worldmap.'+area)

        line = []
        n=len(area)
        for i in range(0,n,2):
          line += [float(area[i+1])/100.,float(area[i])/100.]
          if i!=0:
            if line[-2] < 0 and line[-4] > 177.5:
              line[-2] += 360
              
              outlines.append(line)
              line = list([line[-2]-360,line[-1]])
              
            elif line[-4] < 0 and line[-2] > 177.5:
              line[-2] -= 360
              
              outlines.append(line)
              line = list([line[-2]+360,line[-1]])
                            
        outlines.append(line)
              
    return outlines

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
  # Return the root of the 
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
  def getRoot( self ):
    return self._root


  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
  # Return the data attributes
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
  def getAttribs( self ):
    return self._attribs


  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
  # Return the data extractor
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
  def getExtractor( self ):
    return self._extractor


  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
  # Return the data description
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
  def getDescription( self ):
    return self._descrip


  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
  # Return the bounding box of the data
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
  def getBoundingBox( self ):
    return self._bbox
 

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
  # Return the Orthographic Slice Node
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
  def getOrthoSlice( self ):
    return self._orthoSlice


  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
  # Get the colormap
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
  def getColorMap( self ):
    return self._colorMap
  

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
  # Scale the entire slice (top-most node)
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
  def scaleSlice( self, x, y ):
    self._extractor.scale(x, y)
   # self._orthoSlice.scale(x, y)


  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
  # Translate the entire slice (top-most node)
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
  def translateSlice( self, x, y ):
    self._extractor.translate(x, y)
   # self._orthoSlice.translate(x, y)


  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
  # Move the data request of the slice (to show different z-values!)
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
  def moveDataRequest( self, z ):
    request = VisusDataRequest()
    self._extractor.getValue(request)
    
    print self._attribs

    matrix = request.transformation()

    # Move the request higher to get a different piece of data
    matrix = matrix * translationMatrix(0,0,z) 
    request.transformation(matrix)
    self._extractor.setValue(request)
    #self._isoExtract.setValue(request)
     
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
  # Add an isosurface
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
  def addIsoSurface( self, isoData, isoValue=0 ):
    
    print "Add Isosurface"

    # Create the 3D extractor
    self._isoExtract, bbox, self._iso_description = VisusFunctions.create3dExtractor( isoData )

    # Attach the extractor to the passed in slice
    self._root.attachSubTree(self._isoExtract, True)

    # Create the isosurface
    surface = VisusIsoSurface.construct()

    # Create the mesh display
    display = VisusMeshDisplay.construct()
    display.normalIndex(3)

    self._isoExtract.attachSubTree(surface)
    surface.attachSubTree(display)
    
    # Connect the output of the extractor to the input of each surface node
    if not surface.connectIso(self._isoExtract):
      raise RuntimeError("unable to connect extractor1 as surface input")
    
    if not display.connectInput(surface):
      raise RuntimeError("unable to connect surface as display input")

    # Set the isosurface
    iso = VisusIsoValue(isoValue)
    surface.setValue(iso)

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
  # Add a label for the slice
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
  def addLabel( self, labelText, font = VisusFont() ):
    label = VisusLabelNode.construct()
    label.text(labelText)
    font.fontSize(16)
    label.setValue(font)
    #label.position(-0.75, 0.65)
    self._root.attachSubTree(label,True)

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
  def freeze( self ):
    self._extractor.freeze()
