import vtk
from patterns import pattern_list


def make_patterned_polydata(inputContours, fillareastyle=None,
                            fillareaindex=None, fillareacolors=None,
                            fillareaopacity=None, size=None):
    if inputContours is None or fillareastyle == 'solid':
        return None
    if inputContours.GetNumberOfCells() == 0:
        return None
    if fillareaindex is None:
        fillareaindex = 1
    if fillareaopacity is None:
        fillareaopacity = 100

    # Create the plane that will be textured with the pattern
    # The bounds of the plane match the bounds of the input polydata
    bounds = inputContours.GetBounds()

    patternPolyData = vtk.vtkPolyData()
    patternPts = vtk.vtkPoints()
    patternPolyData.SetPoints(patternPts)

    xBounds = bounds[1] - bounds[0]
    yBounds = bounds[3] - bounds[2]

    if xBounds <= 1 and yBounds <= 1 and size is not None:
        xBounds *= size[0]/2
        yBounds *= size[1]/2

    xres = int(xBounds/2)
    yres = int(yBounds/2)
#    if size is not None and len(size) == 2:
#        if size[0] > size[1]:
#            yres = 20
#            xres = size[0] * 20 / size[1]
#        else:
#            xres = 20
#            yres = size[1] * 20 / size[0]
#    else:
#        xres = 5
#        yres = 5

    numPts = (xres+1) * (yres+1)
    patternPts.Allocate(numPts)
    normals = vtk.vtkFloatArray()
    normals.SetName("Normals")
    normals.SetNumberOfComponents(3)
    normals.Allocate(3*numPts)
    tcoords = vtk.vtkFloatArray()
    tcoords.SetName("TextureCoordinates")
    tcoords.SetNumberOfComponents(2)
    tcoords.Allocate(2*numPts)

    x = [0.0, 0.0, 0.0]
    tc = [0.0, 0.0]
    v1 = [0.0, bounds[3] - bounds[2]]
    v2 = [bounds[1] - bounds[0], 0.0]
    normal = [0.0, 0.0, 1.0]
    numPt = 0
    for i in range(yres+1):
        tc[0] = i * 1.0 / yres
        for j in range(xres+1):
            tc[1] = j * 1.0 / xres
            for ii in range(2):
                x[ii] = bounds[2*ii] + tc[0] * v1[ii] + tc[1] * v2[ii]
            patternPts.InsertPoint(numPt, x)
            tcoords.InsertTuple(numPt, tc)
            normals.InsertTuple(numPt, normal)
            numPt += 1
    patternPolyData.GetPointData().SetNormals(normals)
    patternPolyData.GetPointData().SetTCoords(tcoords)

#    patternPlane = vtk.vtkPlaneSource()
#    patternPlane.SetOrigin(bounds[0], bounds[2], 0.0)
#    patternPlane.SetPoint1(bounds[0], bounds[3], 0.0)
#    patternPlane.SetPoint2(bounds[1], bounds[2], 0.0)
#    if size is not None and len(size) == 2:
#        patternPlane.SetXResolution(size[0])
#        patternPlane.SetYResolution(size[1])
#
#    patternPlane.Update()
#    patternPolyData = patternPlane.GetOutput()
    # Create the pattern
    create_pattern(patternPolyData, xres, yres,
                   fillareastyle, fillareaindex,
                   fillareacolors, fillareaopacity)
#    # Generate texture coordinates for the plane
#    textureMap = vtk.vtkTextureMapToPlane()
#    textureMap.SetInputConnection(patternPlane.GetOutputPort())
#
#    # Create the pattern image of the size of the input polydata
#    # and type defined by fillareaindex
#    xBounds = bounds[1] - bounds[0]
#    yBounds = bounds[3] - bounds[2]
#
#    if xBounds <= 1 and yBounds <= 1 and size is not None:
#        xBounds *= size[0]
#        yBounds *= size[1]
#        xres, yres = int(xBounds), int(yBounds)
#
#    xres = int(4.0*xBounds)
#    yres = int(4.0*yBounds)
#
#    # Handle the case when the bounds are less than 1 in physical dimensions
#
#    patternImage = create_pattern(xres, yres, num_pixels, fillareastyle,
#                                  fillareaindex, fillareacolors,
#                                  fillareaopacity)
#    if patternImage is None:
#        return None
#
    # Extrude the contour since vtkPolyDataToImageStencil
    # requires 3D polydata
#    extruder = vtk.vtkLinearExtrusionFilter()
#    extruder.SetInputData(inputContours)
#    extruder.SetScaleFactor(1.0)
#    extruder.SetVector(0, 0, 1)
#    extruder.SetExtrusionTypeToNormalExtrusion()
#    extruder.Update()
#
#    # Create a binary image mask from the extruded polydata
#    pol2stenc = vtk.vtkPolyDataToImageStencil()
#    pol2stenc.SetTolerance(0)
#    pol2stenc.SetInputConnection(extruder.GetOutputPort())
#    pol2stenc.SetOutputOrigin(bounds[0], bounds[2], 0.0)
#    pol2stenc.SetOutputSpacing((bounds[1] - bounds[0]) / xres,
#                               (bounds[3] - bounds[2]) / yres,
#                               0.0)
#    pol2stenc.SetOutputWholeExtent(patternImage.GetExtent())
#
#    # Stencil out the fillarea from the pattern image
#    stenc = vtk.vtkImageStencil()
#    stenc.SetInputData(patternImage)
#    stenc.SetStencilConnection(pol2stenc.GetOutputPort())
#    stenc.ReverseStencilOff()
#    stenc.SetBackgroundColor(0, 0, 0, 0)
#    stenc.Update()
#    patternImage = stenc.GetOutput()
#
#    # Create the texture using the stenciled pattern
#    patternTexture = vtk.vtkTexture()
#    patternTexture.SetInputData(patternImage)
#    patternTexture.InterpolateOn()
#    patternTexture.RepeatOn()

    ctp = vtk.vtkCellDataToPointData()
    ctp.SetInputData(inputContours)
    ctp.Update()
    # Now that the polydata has been created,
    # clip it using the input contour
    implicitFn = vtk.vtkImplicitDataSet()
    implicitFn.SetDataSet(ctp.GetOutput())
    clipFilter = vtk.vtkClipPolyData()
    clipFilter.SetInputData(patternPolyData)
    clipFilter.SetClipFunction(implicitFn)
#    clipFilter.Update()

#    w = vtk.vtkXMLPolyDataWriter()
#    w.SetFileName("Test.vtp")
#    w.SetInputConnection(clipFilter.GetOutputPort())
#    w.Write()

    mapper = vtk.vtkPolyDataMapper()
    mapper.SetInputConnection(clipFilter.GetOutputPort())
    actor = vtk.vtkActor()
    actor.SetMapper(mapper)
    # actor.SetTexture(patternTexture)
    return actor


def create_pattern(patternPolyData, xres, yres,
                   fillareastyle=None, fillareaindex=None,
                   fillareacolors=None, fillareaopacity=None):
    if fillareastyle == 'solid':
        return None

    if fillareaindex is None:
        fillareaindex = 1

    if fillareacolors is None:
        fillareacolors = [0, 0, 0]

    if fillareaopacity is None:
        fillareaopacity = 100

    # Create a pattern source image of the given size
    pattern = pattern_list[fillareaindex](patternPolyData, xres, yres,
                                          fillareacolors, fillareastyle,
                                          fillareaopacity)
    return pattern.render()
