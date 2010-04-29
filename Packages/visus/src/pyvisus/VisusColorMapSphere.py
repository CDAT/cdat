import pyvisus
from pyvisus.core import *
from pyvisus.gui import *
from pyvisus.data import *
from pyvisus.component import VisusColorBar, VisusLabelNode
from pyvisus.display import VisusEarthNode, VisusMeshDisplay
from pyvisus.shared import VisusSharedDataRequest, VisusSharedEarthRadius
from pyvisus.extract import VisusIsoSurface, VisusProjector
from pyvisus.VisusFunctions import *
from numpy import uint8
#import Image
import time
import cdms2

#~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~#
# Class to create an earth node with a colormap, and a colorbar
#~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~#
class ColorMapSphere:
  """
  Class to colormap data onto a sphere and the colorbar associated with the colormap
  """

  _interface_info = """
Select up to 3 variables they will define

Variable 1: Color map on the earth
Variable 2: Height data on the earth
Variable 3: Iso-surface around the earth
"""
  _min_variables = 1
  _max_variables = 3
  
  
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
  # Construct the colormapped sphere
  # Mask is an optional argument: 0 no mask, 1 if by land, 2 if by sea...
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
  def __init__( self, dataArray, heightDataArray=[], isoDataArray=[], mask=0, title=None, usrAttribs=dict(), 
                root=None, newWindow = True):
    mask = 1
    # Creating the default root within the __init__ call somehow
    # causes the root variable to persist and only a single root to be
    # created for multiple scenes. To avoid this we use the explicit
    # None initialization
    if root == None:
      root = VisusGroup.construct()

    # Create a root node (or get one from the user)
    self._root = root
    self._root.drawBoundingBox(False)  
    
    # Create the attributes
    self._attribs = dict()
    
    # Create the heightattributes
    self._heightattribs = dict()
    
    # Create the iso attributes
    self._isoattribs = dict()

    # Set whether we mask the ocean, the land, or nothing
    self._mask = mask
    
    # Create a radius object
    self._radius = VisusEarthRadius()
    
    # Create an earth node 
    self._earth = VisusEarthNode.construct()
    
    # Create a node for the background image
    self._bgEarth = VisusEarthNode.construct()
    
    # Set the radius of the earth  
    self.setEarthRadius(100.0)

    # Set the data that we have
    self._haveHeightData = True
    if(len(heightDataArray) == 0):
      self._haveHeightData = False
    self._haveIsoData = True
    if(len(isoDataArray)== 0):
      self._haveIsoData = False

    # Create the extractor, data description and update attribs 
    self.setData(dataArray, heightDataArray, isoDataArray, usrAttribs)
    
    # Attach the texture extractor to the earth
    self._earth.attachSubTree(self._extractortexture, True) 

    # Add the isosurface
    if(self._haveIsoData):
      self._isosurface = self.addIso(self._extractoriso)
    
      # Set the isoValue
      self._isoValue = VisusIsoValue(40)
      self._isosurface.setValue(self._isoValue)
    
      # Attach the iso extractor to the earth
      self._earth.attachSubTree(self._extractoriso, True)
     
    # Attach the height extractor to the earth
    if(self._haveHeightData):
      self._earth.attachSubTree(self._extractorheight, True)

    # Finally attach the earth node to the root
    self._root.attachSubTree(self._earth,True);
        
    # Create the drawing window
    if newWindow:
      createWindow(self._root)

    # If masking is enabled, set it up
    if self._mask != 0:
      self.setUpMasking()

    # Connect the extractor which is a VisusProducer 
    # (which is really a VisusAxisAlignedExtractor)
    if not self._earth.connectTexture(self._extractortexture):
      raise RuntimeError("Unable to connect extractor as EarthNode input")

    # Connect the height extractor which is a VisusProducer 
    # (which is really a VisusAxisAlignedExtractor) 
    if(self._haveHeightData):
      if not self._earth.connectHeight(self._extractorheight):
        raise RuntimeError("Unable to connect extractor as EarthNode input")
    
    # Set the earth as periodic
    self._earth.periodic(True)

    # Rotate the hole to the top
    self.rotateEarth(0.0, -1.0)
    
    # Set the colormap
    self.setColorMap(banded())

    # Create the colorbar for the colormap
    self._colorBar = VisusColorBar.construct()

    # Set the annotations for the scene
    self.setAnnote()
        
    self._earth.attachSubTree(self._colorBar, True)
    self._colorBar.inherit(8, True)

    # Position the colorbar
    self._colorBar.position(-.5,-0.8)

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
    self._earth.attachSubTree(self._title)

    self._extractortexture.propagateUpwards(VISUS_GLOBALTIME_TYPEID);

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
  # Add a projected Isosurface
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
  def addIso(self, extractor):

    # Create the nodes for surface of data
    surface = VisusIsoSurface.construct()

     # Create Projector and Projection
    proj = VisusSphereProjection()
    projector = VisusProjector.construct()
    projector.declareParameter(11) # projector->declareParameter(VisusSharedEarthRadius::sTypeIndex);
    projector.projection(proj)

    # set radius
    ShareRadius = VisusSharedEarthRadius()
    radius = self._radius.get()*1.25
    ShareRadius.set(radius)
    projector.setValue(ShareRadius)
 
    # Create Display
    display = VisusMeshDisplay.construct()
    display.normalIndex(3)
    
    # Attach the surface as sub trees of the extractors
    extractor.attachSubTree(surface, True)
    surface.attachSubTree(projector, True) 
    projector.attachSubTree(display, True)
    #surface.attachSubTree(display, True) 

    # Connect the output of the extractor to the input of each surface node
    if not surface.connectIso(extractor):
      raise RuntimeError("unable to connect extractor as surface input")
    
    # Connect the output of the projector to the input of each extractor node
    if not projector.connectInput(surface):
      raise RuntimeError("unable to connect surface as projector input")
    
    # Connect the output of the display to the input of each surface node
    if not display.connectInput(projector):
      raise RuntimeError("unable to connect projector as display input")
  
    # Draw the bounding boxes of the surface
    surface.drawBoundingBox(False) 

    return surface

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
  # Take in the data and some attributes and get the extractor etc
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
  def setData( self,dataArray, heightDataArray, isoDataArray, usrAttribs=dict(),filename=None):
    
    # If the user passed in attributes, use those
    if(usrAttribs):
      self._attribs = usrAttribs
    
    # If we are masking, get the data to mask
    if self._mask != 0:

      if filename == None:
        from sys import executable
        from os.path import split,join

      filename = join(split(executable)[0],"..","sample_data","sftlf_visus.nc")
       # filename = join(split(executable)[0],"..","sample_data","sftlf_dnm.nc")
        #filename = 'sftlf_A1.20C3M.CCSM.atmm.nc'

      longitude = dataArray.getLongitude().getData();
      longitude = [longitude[0],longitude[-1]]
        
      mask_file = cdms2.open(filename)
      mask_data = mask_file('sftlf',longitude = longitude).ascontiguousarray()
      mask_data = (mask_data/100).round() # Make the values between 0 1
    
      if self._mask == 1:
        mask_data = 255*(1-mask_data) # mask ocean (data on land)
      elif self._mask == 2:
        mask_data = 255*(mask_data)   # mask land (data on ocean)

      mask_data = mask_data.astype(uint8)
      self._attribs.update(mask=mask_data)

    # Create the extractor for the colormap
    self._extractortexture, self._bbox, self._attribs, self._descrip = createExtractor(dataArray, self._attribs)
    
    # Create the extractor for the height
    if(self._haveHeightData):
      self._extractorheight, self._heightbbox, self._heightattribs, self._heightsdescrip = createExtractor(heightDataArray, self._heightattribs)
    else:
      self._extractorheight = None
      
    # Create the extractor for textutre and height, description and update the attributes 
    if(self._haveIsoData):
      self._extractoriso, self._isobbox, self._isodescrip = create3dExtractor(isoDataArray, self._isoattribs)
    else:
      self._extratoriso = None
        
    # Create the extractor, description and update the attributes
    # self._extractortexture, self._bbox, self.attribs, self._descrip =  createExtractor(dataArray, self._attribs)                                                
    # Set the bounding box of the root
    self._bbox[0] = -100
    self._bbox[1] = -100
    self._bbox[2] = -100
    self._bbox[3] = 100
    self._bbox[4] = 100
    self._bbox[5] = 100
    self._root.setValue(self._bbox)


  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
  # Set the labels of the colorbar
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
  def setAnnote( self ):
    
    # Get the colorbar axis
    axis = self._colorBar.axis()

    # Set up the text of the colorbar
    font = VisusFont()
    font.fontSize(8)
    axis.legendFont(font)
    axis.labelFont(font)

    # If we found a variable name we use it as label
    if "name" in self._attribs:
      axis.legendText(self._attribs["name"].replace("_", " ")+ " (K)")
      
    # If we found a range we use it as well. 
    if "range" in self._attribs:
      axis.minValue(float(self._attribs["range"][0]))
      axis.maxValue(float(self._attribs["range"][1]))

    self._colorBar.axis(axis)


  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
  # Set the radius of the earth
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
  def setEarthRadius( self, radius):
    self._radius.set(radius)
    self._earth.setValue(self._radius)
    if self._mask != 0:
      bgRadius = VisusEarthRadius()
      bgRadius.set(radius*0.99)
      self._bgEarth.setValue(bgRadius)


  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
  #  Change the colormap
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
  def setColorMap( self, colormap ):
    self._colorMap = colormap
    self._earth.setValue(self._colorMap)
    
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
  #  Change the isoValue
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
  def setIsovalue( self, isovalue ):
    if(self._haveIsoData):
      self._isoValue = isovalue
      self._isosurface.setValue(self._isoValue)
    
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
  #  Change the GeometryScaling
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
  def setGeometryScaling( self, mGeometryScaling):
    self._geoScaling = mGeometryScaling
    self._earth.setGeometryScaling(self._geoScaling)
    
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
  # Return the isovalue
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
  def getIsovalue( self ):
    return self._isoValue  


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
  # Return the EarthNode
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
  def getEarth( self ):
    return self._earth


  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
  # Get the radius of the earth
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  def getEarthRadius( self ):
    return self._earth.getValue(self._radius)
  

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
  # Get the colormap
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
  def getColorMap( self ):
    return self._colorMap


  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
  # Get the colorbar
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
  def getColorBar( self ):
    return self._colorBar

  def setColorBar( self, colorbar ):
    self._colorbar = colorbar


  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
  # Rotate the earth by x, y
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
  def rotateEarth( self, x, y ):
    self._earth.rotate(x, y)
    if(self._bgEarth):
      self._bgEarth.rotate(x, y)

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
  # Set the scaling of the height
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
  def setGeometryScaling( self, scale ):
    self._earth.setGeometryScaling(scale);

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
  # Translate the earth by  x, y
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
  def translateEarthNode( self, x, y ):
    self._earth.translate(x, y)
    if(self._bgEarth):
      self._bgEarth.translate(x, y)
    #self._colorBar.translate(x,y)


  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
  # Set up the image masking
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
  def setUpMasking( self,image=None):

    if image == None:
      from sys import executable
      from os.path import split,join
      
      image = join(split(executable)[0],"..","sample_data","BlueMarble.ppm")
    
    # Open the default or passed in image
    #maskPic = Image.open(image)
    
    #maskPic = maskPic.transpose(Image.FLIP_TOP_BOTTOM)
    #pic = pic.offset(512,0)
    
    # Save the image to a ppm
    #maskPic.save("background.ppm","ppm")
    
    texture = VisusTexture()
    texture.loadPPM(image)
    #texture.loadPPM("background.ppm")
    left = VectorDouble(3)
    right = VectorDouble(3)
    
    left[0] = -180
    left[1] = -90
    left[2] = 0
    right[0] = 180
    right[1] = 90
    right[2] = 0
    
    texture.setDomainBoundingBox(left,right)
    texture.unit(VISUS_POLAR_DEGREES)
    
    self._bgEarth.loadData(texture,1)
    
    self._root.attachSubTree(self._bgEarth,True)

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
  # Animate the data on the sphere by updating the time
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
  def animate( self, i, t = VisusGlobalTime(), t2 = VisusGlobalTime() ):
    print "i:", i 
    self._extractortexture.getValue(t)
    self._extractorheight.getValue(t2)
    self._extractoriso.getValue(t2)
    t.inc()
    t2.inc()
    self._extractortexture.setValue(t)
    if(self._haveHeightData):
      self._extractorheight.setValue(t2)
    if(self._haveIsoData):
      self._extractoriso.setValue(t2)
