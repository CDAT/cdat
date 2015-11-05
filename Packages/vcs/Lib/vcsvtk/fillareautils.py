import vtk
from patterns import pattern_list
import math


def num_pixels_for_size(size):
    # Select the largest dimension available
    dim = max(size)
    log4 = math.log(dim, 4)
    return 2 ** (int(log4))


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
        fillareaopacity = 255
    num_pixels = num_pixels_for_size(size)
    print num_pixels

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

    # Create the pattern image of the size of the input polydata
    # and type defined by fillareaindex
    # Scaled the size to 2 times to make the pattern image of a finer resolution
    xBounds = bounds[1] - bounds[0]
    yBounds = bounds[3] - bounds[2]
    xres = int(4.0*xBounds)
    yres = int(4.0*yBounds)
    # Handle the case when the bounds are less than 1 in physical dimensions
    if xBounds <= 1 or yBounds <= 1:
        boundsAspect = xBounds / yBounds
        if boundsAspect > 1.0:
            yres = num_pixels * 2
            xres = int(yres * boundsAspect)
        else:
            xres = num_pixels * 2
            yres = int(xres / boundsAspect)
    patternImage = create_pattern(xres, yres, num_pixels, fillareastyle,
                                  fillareaindex, fillareacolors,
                                  fillareaopacity)
    print patternImage.GetDimensions(), xres, yres, num_pixels, xBounds, yBounds
    if patternImage is None:
        return None

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
    print patternImage.GetDimensions()
    stenc.SetStencilConnection(pol2stenc.GetOutputPort())
    stenc.ReverseStencilOff()
    stenc.SetBackgroundColor(0, 0, 0, 0)
    stenc.Update()
    patternImage = stenc.GetOutput()
    print patternImage.GetDimensions()

    # Create the texture using the stenciled pattern
    patternTexture = vtk.vtkTexture()
    patternTexture.SetInputData(patternImage)
    patternTexture.InterpolateOn()
    patternTexture.RepeatOn()
    mapper = vtk.vtkPolyDataMapper()
    mapper.SetInputConnection(textureMap.GetOutputPort())
    actor = vtk.vtkActor()
    actor.SetMapper(mapper)
    actor.SetTexture(patternTexture)
    return actor


def create_pattern(width, height, num_pixels, fillareastyle=None,
                   fillareaindex=None, fillareacolors=None, fillareaopacity=None):
    if fillareastyle == 'solid':
        return None

    if fillareaindex is None:
        fillareaindex = 1

    if fillareacolors is None:
        fillareacolors = [0, 0, 0]

    if fillareaopacity is None:
        fillareaopacity = 255

    # Create a pattern source image of the given size
    pattern = pattern_list[fillareaindex](width, height, num_pixels, fillareacolors, fillareastyle, fillareaopacity)
    return pattern.render()
