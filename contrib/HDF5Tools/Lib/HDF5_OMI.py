# Adapted for numpy/ma/cdms2 by convertcdms.py
import cdms2,numpy,HDF5Tools,os

# Surclass HDF5_Variable class
class HDF5_Variable_OMI(HDF5Tools.HDF5_Variable):
   def __init__(self,hdf5file,variable,coordinates=None,h5dump=None):
      HDF5Tools.HDF5_Variable.__init__(self,hdf5file,variable,h5dump=h5dump)
      # Sets the coordinates attribute if needed to link to dimensions
      if not hasattr(self,'coordinates'):
         self.coordinates = coordinates
         self._attributes['coordinates']=coordinates

   def get(self,coordinates=None,resolution_lon=None,resolution_lat=None,grid=None):
      """ Retrieves the variable
      Usage:
      V.get(), where V is an HDF5_Variable_OMI

      Keywords:
      grid: can represent grid onto which apply the data, otherwise set to None, in conjonction with the variable "coordinates" and "grid" attributes, will retrieve coordinates info and map onto a grid.
      TODO: slicing
      """
      if coordinates is None and hasattr(self,'coordinates'):
         coordinates = self.coordinates
      if resolution_lat is None and hasattr(self,'resolution_lat'):
         resolution_lat = self.resolution_lat
      if resolution_lon is None and hasattr(self,'resolution_lon'):
         resolution_lon = self.resolution_lon
      if grid is None and hasattr(self,'grid'):
         grid = self.grid 
      data = HDF5Tools.HDF5_Variable.get(self)

      if hasattr(self,'grid') and grid is None:
         if coordinates is not None:
            coord = coordinates.split()
            res = numpy.array(resolution_lat/2.,'f')
            # Creates grid
            # Latitudes
            centerLat = self.file.variables[coord[0]].get()
            # Creates the grid mask
            mask = centerLat.mask
            centerLat = numpy.ma.where(numpy.ma.less(centerLat,-90),-90.,centerLat)
            centerLat = numpy.ma.where(numpy.ma.greater(centerLat,90),90.,centerLat)
            # Now create the corners for latitude
            sh = list(centerLat.shape)+[4,]
            cornerLat = numpy.ma.ones(sh,'f')
            cornerLat[...,0] = centerLat-res
            cornerLat[...,1] = centerLat-res
            cornerLat[...,2] = centerLat+res
            cornerLat[...,3] = centerLat+res
            cornerLat = numpy.ma.where(numpy.ma.less(cornerLat,-90),-90.,cornerLat)
            cornerLat = numpy.ma.where(numpy.ma.greater(cornerLat,90),90.,cornerLat)

            # Prepare the i,j axes
            from cdms2.coord import TransientAxis2D, TransientVirtualAxis
            from cdms2.hgrid import TransientCurveGrid
            iaxis = TransientVirtualAxis("i",centerLat.shape[0])
            jaxis = TransientVirtualAxis("j",centerLat.shape[1])

            res = numpy.array(resolution_lon/2.,'f')
            # Longitudes
            centerLon = self.file.variables[coord[1]].get()

            # Intersect the existing mask with longitudes one
            mask = numpy.logical_and(mask,centerLon.mask)
            # Now create the corners for latitude
            sh = list(centerLon.shape)+[4,]
            cornerLon = numpy.ones(sh,'f')
            cornerLon[...,0] = centerLon-res
            cornerLon[...,1] = centerLon+res
            cornerLon[...,2] = centerLon+res
            cornerLon[...,3] = centerLon-res

            # Now create the grid
            lataxis = TransientAxis2D(centerLat, axes=(iaxis, jaxis),
                                      bounds=cornerLat,
                                      attributes={'units':'degrees_north'},
                                      id="latitude")
            lonaxis = TransientAxis2D(centerLon, axes=(iaxis, jaxis),
                                      bounds=cornerLon,
                                      attributes={'units':'degrees_east'},
                                      id="longitude")
            self.grid = TransientCurveGrid(lataxis, lonaxis, id='grid', tempmask=mask)
            # Now applies all that
            data.setAxis(0,iaxis)
            data.setAxis(1,jaxis)
            data.setGrid(self.grid)

      elif grid is not None:
         axes = grid.getAxisList()
         for i in range(len(axes)): # this loop to arrange for curvilinear or generic grids
            data.setAxis(i,axes[i])
         data.setGrid(grid)

      return data

   __call__=get

# Class HDF5_OMI surclass HDF5 with OMI/CDAT specific
class HDF5_OMI(HDF5Tools.HDF5):
   def __init__(self,file,dimension_kw=None,coordinates=None,grid=None,resolution_lon=.5,resolution_lat=.25,h5dump=None):
      """Creates an HDF5 file for ingestion into cdat, with OMI specific features
      such as coordinates parsing
      these default respectively to:
      'Latitude Longitude'
      grid: A grid object, for now None or Curvilinear or Generic grids only
      resolution_lon/_lat: if grid is None then will use coordinates to retrieve grid centers and resolution to create box of half that value around it (default values in lon/lat: .5/.25 degrees.
      """
      if h5dump is None:
         self.h5dump = os.environ.get("H5DUMP","h5dump")
      else:
         self.h5dump = h5dump
         
      sin,sout,serr = os.popen3('%s -h' % self.h5dump)
      err = serr.read()
      if len(err.strip())>0:
         raise RuntimeError, "h5dump binary cannot be found, HDF5 module will not function w/o it, you can pass its (full) path at init time via h5dump keyword or by setting environement variable H5DUMP"

      self.file = file
      if dimension_kw is None:
         self.dimension_kw = 'Geolocation Fields'
      else:
         self.dimension_kw = dimension_kw
      self.dimensions = []
      self.open(var_class=HDF5_Variable_OMI)
      # Ok now that it opened the file and scanned it
      # we need to apply OMI specific attributes such as grid info
      if coordinates is None:
         coordinates = 'Latitude Longitude'
      # Now checks that coordinates defined are actually in file
      # Assumes that they all have the same shape
      shape = None
      for c in coordinates.split():
         if not c in self.variables.keys():
            raise ValueError,'Error for coordinates attribute, variable %s has been declared as coordinates variable but is not present in file' % c
         if shape is None:
            shape = self.variables[c].shape
         elif shape!=self.variables[c].shape:
            raise ValueError,'Error for coordinates attribute (%s), variable %s has been declared as coordinates variable but its shape %s is not consistent with one of previous coordinates %s' % (coordinates,c,self.variables[c].shape,shape)
      # Applies the coordinates attribute
      for v in self.listvariables():
         V=self.variables[v]
         if V.shape[:len(shape)]==shape:
            V.coordinates = coordinates
            V.grid = grid
            V.resolution_lat = resolution_lat
            V.resolution_lon = resolution_lon
