import vtk


def make_patterned_polydata(inputContours, fillareastyle='pattern',
                            fillareaindex=0, fillareacolors=[]):
    if inputContours is None or fillareastyle == 'solid':
        return []

    # Create the plane that will be textured with the pattern
    # The bounds of the plane match the bounds of the input polydata
    bounds = inputContours.GetBounds()
    patternPlane = vtk.vtkPlaneSource()
    patternPlane.SetOrigin(bounds[0], bounds[2], 0.0)
    patternPlane.SetPoint1(bounds[0], bounds[3], 0.0)
    patternPlane.SetPoint2(bounds[1], bounds[2], 0.0)
    # Generate texture coordinates for the plane
    textureMap = vtk.vtkTextureMapToPlane()
    textureMap.SetInputConnection(patternPlane.GetOutputPort())
    textureMap.Update()

    # Create the pattern image of the size of the input polydata
    # and type defined by fillareaindex
    xres = 2*int(bounds[1] - bounds[0])
    yres = 2*int(bounds[3] - bounds[2])
    patternImage = create_pattern(xres, yres, fillareastyle,
                                  fillareaindex, fillareacolors)
    if patternImage is None:
        return []

    # Extrude the contour since vtkPolyDataToImageStencil
    # requires 3D polydata
    extruder = vtk.vtkLinearExtrusionFilter()
    extruder.SetInputData(inputContours)
    extruder.SetScaleFactor(1.0)
    extruder.SetVector(0, 0, 1)
    extruder.SetExtrusionTypeToNormalExtrusion()

    # Create a binary image mask from the extruded polydata
    pol2stenc = vtk.vtkPolyDataToImageStencil()
    pol2stenc.SetTolerance(0)
    pol2stenc.SetInputConnection(extruder.GetOutputPort())
    pol2stenc.SetOutputOrigin(bounds[0], bounds[2], 0.0)
    pol2stenc.SetOutputSpacing((bounds[1] - bounds[0]) / xres,
                               (bounds[3] - bounds[2]) / yres,
                               0.0)
    pol2stenc.SetOutputWholeExtent(patternImage.GetExtent())

    # Stencil out the fillarea from the pattern image
    stenc = vtk.vtkImageStencil()
    stenc.SetInputData(patternImage)
    stenc.SetStencilConnection(pol2stenc.GetOutputPort())
    stenc.ReverseStencilOff()
    stenc.SetBackgroundColor(0, 0, 0, 0)

    # Create the texture using the stenciled pattern
    patternTexture = vtk.vtkTexture()
    patternTexture.SetInputConnection(stenc.GetOutputPort())
    return [textureMap.GetOutput(), patternTexture]


def create_pattern(width, height, fillareastyle,
                   fillareaindex=0, fillareacolors=[]):
    if fillareastyle == 'solid':
        return None

    if not fillareacolors:
        fillareacolors = [0, 0, 0]

    patternSource = vtk.vtkImageCanvasSource2D()
    patternSource.SetScalarTypeToUnsignedChar()
    patternSource.SetExtent(0, width, 0, height, 0, 0)
    patternSource.SetNumberOfScalarComponents(4)
    patternSource.SetDrawColor(255, 255, 255, 0)
    patternSource.FillBox(0, width, 0, height)
    if fillareastyle == 'hatch':
        patternSource.SetDrawColor(fillareacolors[0],
                                   fillareacolors[1],
                                   fillareacolors[2],
                                   255)
    else:
        patternSource.SetDrawColor(0, 0, 0, 255)

    if fillareaindex == 0:
        return pattern1(patternSource, width, height)
    else:
        return None


def pattern1(patternSource, width, height):
    if patternSource is None:
        return None
    patternLevels = range(0, max(width, height) + min(width, height), 10)
    for lev in patternLevels:
        patternSource.DrawSegment(0, lev, lev, 0)
    patternSource.Update()
    return patternSource.GetOutput()
