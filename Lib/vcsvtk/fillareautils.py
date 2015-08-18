import vtk

#TODO: Need to add opacity control for patterns/hatches
#TODO: counter is a DEBUG thing and needs to be removed later.
counter = 0

# number of pixels per individual pattern block
NUM_PIXELS = 12


def make_patterned_polydata(inputContours, fillareastyle=None,
                            fillareaindex=None, fillareacolors=None):
    if inputContours is None or fillareastyle == 'solid':
        return None, None
    if fillareaindex is None:
        fillareaindex = 1

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

    global counter
#    wp = vtk.vtkXMLPolyDataWriter()
#    wp.SetInputConnection(textureMap.GetOutputPort())
#    sw = "plane_" + str(counter) + ".vtp"
#    wp.SetFileName(sw)
#    wp.Write()

    # Create the pattern image of the size of the input polydata
    # and type defined by fillareaindex
    # Scaled the size to 3 times to make the pattern image of a finer resolution
    xres = 3*int(bounds[1] - bounds[0])
    yres = 3*int(bounds[3] - bounds[2])
    patternImage = create_pattern(xres, yres, fillareastyle,
                                  fillareaindex, fillareacolors)
    if patternImage is None:
        return []
    ww = vtk.vtkPNGWriter()
    swt = "pattern_" + str(counter) + ".png"
    ww.SetFileName(swt)
    ww.SetInputData(patternImage)
    ww.Write()

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

#    w = vtk.vtkPNGWriter()
#    st = "stencil_" + str(counter) + ".png"
#    w.SetFileName(st)
#    w.SetInputConnection(stenc.GetOutputPort())
#    w.Write()
    counter = counter + 1

    # Create the texture using the stenciled pattern
    patternTexture = vtk.vtkTexture()
    patternTexture.SetInputConnection(stenc.GetOutputPort())
    return textureMap.GetOutput(), patternTexture


def create_pattern(width, height, fillareastyle=None,
                   fillareaindex=None, fillareacolors=None):
    if fillareastyle == 'solid':
        return None

    if fillareaindex is None:
        fillareaindex = 1

    if fillareacolors is None:
        fillareacolors = [0, 0, 0]

    # Create a pattern source image of the given size
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

    if fillareaindex == 1:
        pattern1(patternSource, width, height)
    elif fillareaindex == 2:
        pattern2(patternSource, width, height)
    elif fillareaindex == 3:
        pattern3(patternSource, width, height)
    elif fillareaindex == 4:
        pattern4(patternSource, width, height)

    # Update the pipeline and return the image
    patternSource.Update()
    return patternSource.GetOutput()


def pattern1(patternSource, width, height):
    if patternSource is None:
        return None
    global NUM_PIXELS
    for x in xrange(0, width, NUM_PIXELS):
        for y in xrange(0, height, NUM_PIXELS):
            patternSource.FillTriangle(x, y,
                                       x + NUM_PIXELS, y,
                                       x, y + NUM_PIXELS)


def pattern2(patternSource, width, height):
    if patternSource is None:
        return None
    global NUM_PIXELS
    for x in xrange(0, width, NUM_PIXELS):
        for y in xrange(0, height, NUM_PIXELS):
            patternSource.FillBox(x, x + NUM_PIXELS/2,
                                  y, y + NUM_PIXELS/2)


def pattern3(patternSource, width, height):
    if patternSource is None:
        return None
    global NUM_PIXELS
    for y in xrange(0, height, NUM_PIXELS):
        patternSource.FillBox(0, width,
                              y, y + NUM_PIXELS/2)
    for x in xrange(0, width, NUM_PIXELS):
        patternSource.FillBox(x, x + NUM_PIXELS/2,
                              0, height)


def pattern4(patternSource, width, height):
    if patternSource is None:
        return None
    global NUM_PIXELS
    patternLevels = range(0, max(width, height) + min(width, height), NUM_PIXELS)
    for lev in patternLevels:
        patternSource.DrawSegment(0, lev, lev, 0)
