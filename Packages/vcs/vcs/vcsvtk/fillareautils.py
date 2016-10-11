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

    # Create a point set laid out on a plane that will be glyphed with the
    # pattern / hatch
    # The bounds of the plane match the bounds of the input polydata
    bounds = inputContours.GetBounds()

    patternPolyData = vtk.vtkPolyData()
    patternPts = vtk.vtkPoints()
    patternPolyData.SetPoints(patternPts)

    xBounds = bounds[1] - bounds[0]
    yBounds = bounds[3] - bounds[2]

    if xBounds <= 1 and yBounds <= 1 and size is not None:
        xBounds *= size[0] / 2
        yBounds *= size[1] / 2

    xres = int(xBounds / 2)
    yres = int(yBounds / 2)

    numPts = (xres - 1) * (yres - 1)
    patternPts.Allocate(numPts)
    normals = vtk.vtkFloatArray()
    normals.SetName("Normals")
    normals.SetNumberOfComponents(3)
    normals.Allocate(3 * numPts)
    tcoords = vtk.vtkFloatArray()
    tcoords.SetName("TextureCoordinates")
    tcoords.SetNumberOfComponents(2)
    tcoords.Allocate(2 * numPts)

    x = [0.0, 0.0, 0.0]
    tc = [0.0, 0.0]
    v1 = [0.0, bounds[3] - bounds[2]]
    v2 = [bounds[1] - bounds[0], 0.0]
    normal = [0.0, 0.0, 1.0]
    numPt = 0
    for i in range(1, yres):
        tc[0] = i * 1.0 / yres
        for j in range(1, xres):
            tc[1] = j * 1.0 / xres
            for ii in range(2):
                x[ii] = bounds[2 * ii] + tc[0] * v1[ii] + tc[1] * v2[ii]
            patternPts.InsertPoint(numPt, x)
            tcoords.InsertTuple(numPt, tc)
            normals.InsertTuple(numPt, normal)
            numPt += 1
    patternPolyData.GetPointData().SetNormals(normals)
    patternPolyData.GetPointData().SetTCoords(tcoords)

    # Create the pattern
    create_pattern(patternPolyData, xres, yres,
                   fillareastyle, fillareaindex,
                   fillareacolors, fillareaopacity)

    # Handle special case of legend
    # Here there are no contours, so no point data
    p = vtk.vtkPolyData()
    p.DeepCopy(inputContours)
    if inputContours.GetPointData().GetNumberOfArrays() == 0:
        ctp = vtk.vtkCellDataToPointData()
        ctp.SetInputData(inputContours)
        ctp.Update()
        p.DeepCopy(ctp.GetOutput())
    # Now that the polydata has been created,
    # clip it using the input contour
    implicitFn = vtk.vtkImplicitDataSet()
    implicitFn.SetDataSet(p)
    implicitFn.SetOutValue(-1.0)
    clipFilter = vtk.vtkClipPolyData()
    clipFilter.SetInputData(patternPolyData)
    clipFilter.SetClipFunction(implicitFn)

    mapper = vtk.vtkPolyDataMapper()
    mapper.SetInputConnection(clipFilter.GetOutputPort())
    actor = vtk.vtkActor()
    actor.SetMapper(mapper)
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
