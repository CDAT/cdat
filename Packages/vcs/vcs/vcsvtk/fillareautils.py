import vtk
from patterns import pattern_list


def num_pixels_for_size(size):
    # Select the largest dimension available
    dim = max(size)
    return int(round(dim / 20))


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
    num_pixels = num_pixels_for_size(size)

    # Create the plane that will be textured with the pattern
    # The bounds of the plane match the bounds of the input polydata
    bounds = inputContours.GetBounds()

    # Create the pattern image of the size of the input polydata
    # and type defined by fillareaindex
    xBounds = bounds[1] - bounds[0]
    yBounds = bounds[3] - bounds[2]

    if xBounds <= 1 and yBounds <= 1 and size is not None:
        xBounds *= size[0]
        yBounds *= size[1]
        xres, yres = int(xBounds), int(yBounds)

    xres = int(4.0*xBounds)
    yres = int(4.0*yBounds)

    # Handle the case when the bounds are less than 1 in physical dimensions

    patternImage = create_pattern(xres, yres, num_pixels, fillareastyle,
                                  fillareaindex, fillareacolors,
                                  fillareaopacity)
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
    stenc.SetStencilConnection(pol2stenc.GetOutputPort())
    stenc.ReverseStencilOff()
    stenc.SetBackgroundColor(0, 0, 0, 0)
    stenc.Update()
    patternImage = stenc.GetOutput()

    # Create a polydata from the pattern image
    image2poly = vtk.vtkImageDataGeometryFilter()
    image2poly.SetInputConnection(stenc.GetOutputPort())
    image2poly.Update()
    patternPolyData = image2poly.GetOutput()
    polybounds = patternPolyData.GetBounds()
    polyCenter = patternPolyData.GetCenter()
    center = inputContours.GetCenter()

    # Transform the generated polydata to get the right bounds
    tf = vtk.vtkTransform()
    tf.PostMultiply()
    tf.Translate(-polyCenter[0], -polyCenter[1], -polyCenter[2])
    tf.Scale(
        (bounds[1] - bounds[0])/(polybounds[1] - polybounds[0]),
        (bounds[3] - bounds[2])/(polybounds[3] - polybounds[2]),
        1.0)
    tf.Translate(center[0], center[1], center[2])
    transformPatternPolyData = vtk.vtkTransformPolyDataFilter()
    transformPatternPolyData.SetInputConnection(image2poly.GetOutputPort())
    transformPatternPolyData.SetTransform(tf)

    # Remove transparent cells
    # This is done to reduce the overhead of large number of transparent
    # triangles
    cleanPoly = vtk.vtkCleanPolyData()
    cleanPoly.SetInputConnection(transformPatternPolyData.GetOutputPort())
    cleanPoly.Update()
    patternPoly = cleanPoly.GetOutput()
    numberOfPoints = patternPoly.GetNumberOfPoints()
    imgScalars = patternPoly.GetPointData().GetScalars("ImageScalars")
    nonTransparentPoints = vtk.vtkIdTypeArray()
    nonTransparentPoints.SetNumberOfComponents(1)
    if imgScalars:
        for i in range(numberOfPoints):
            if imgScalars.GetTuple(i)[3] != 0.0:
                nonTransparentPoints.InsertNextValue(i)
    selNode = vtk.vtkSelectionNode()
    selNode.SetFieldType(vtk.vtkSelectionNode.POINT)
    selNode.SetContentType(vtk.vtkSelectionNode.INDICES)
    selNode.SetSelectionList(nonTransparentPoints)
    selNode.GetProperties().Set(vtk.vtkSelectionNode.CONTAINING_CELLS(), 1)

    selection = vtk.vtkSelection()
    selection.AddNode(selNode)

    extractSel = vtk.vtkExtractSelection()
    extractSel.SetInputConnection(0, cleanPoly.GetOutputPort())
    extractSel.SetInputData(1, selection)

    # Now that we have the pattern polydata, map it to the scene
    mapper = vtk.vtkDataSetMapper()
    mapper.SetInputConnection(extractSel.GetOutputPort())
    actor = vtk.vtkActor()
    actor.SetMapper(mapper)
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
        fillareaopacity = 100

    # Create a pattern source image of the given size
    pattern = pattern_list[fillareaindex](width, height, num_pixels, fillareacolors, fillareastyle, fillareaopacity)
    return pattern.render()
