# This module contains some convenience function from vcs2vtk
import vcs
import vtk
import numpy
import json
import os
import math
import meshfill
from vtk.util import numpy_support as VN
import cdms2
import warnings
from projection import round_projections, no_over_proj4_parameter_projections
from vcsvtk import fillareautils
import sys
import numbers

f = open(os.path.join(vcs.prefix, "share", "vcs", "wmo_symbols.json"))
wmo = json.load(f)

projNames = [
    "linear",
    "utm",
    "state",
    "aea",
    "lcc",
    "merc",
    "stere",
    "poly",
    "eqdc",
    "tmerc",
    "stere",
    "lcca",
    "azi",
    "gnom",
    "ortho",
    "vertnearper",
    "sinu",
    "eqc",
    "mill",
    "vandg",
    "omerc",
    "robin",
    "somerc",
    "alsk",
    "goode",
    "moll",
    "imoll",
    "hammer",
    "wag4",
    "wag7",
    "oea"]
projDict = {"polar stereographic": "stere",
            -3: "aeqd",
            }
for i in range(len(projNames)):
    projDict[i] = projNames[i]


def applyAttributesFromVCStmpl(tmpl, tmplattribute, txtobj=None):
    tatt = getattr(tmpl, tmplattribute)
    if txtobj is None:
        txtobj = vcs.createtext(
            To_source=tatt.textorientation,
            Tt_source=tatt.texttable)
    for att in ["x", "y", "priority"]:
        setattr(txtobj, att, getattr(tatt, att))
    return txtobj


def numpy_to_vtk_wrapper(numpyArray, deep=False, array_type=None):
    result = VN.numpy_to_vtk(numpyArray, deep, array_type)
    # Prevent garbage collection on shallow copied data:
    if not deep:
        result.numpyArray = numpyArray
    return result


# Adds 'array' to 'grid' as cell or point attribute based on 'isCellData'
# It also sets it as the active scalar if 'isScalars'.
# If the grid has pedigree ids (it was wrapped) we use them to set the array.
def setArray(grid, array, arrayName, isCellData, isScalars):
    attributes = grid.GetCellData() if isCellData else grid.GetPointData()
    pedigreeId = attributes.GetPedigreeIds()
    if (pedigreeId):
        vtkarray = attributes.GetArray(arrayName)
        for i in range(0, vtkarray.GetNumberOfTuples()):
            vtkarray.SetValue(i, array[pedigreeId.GetValue(i)])
    else:
        vtkarray = numpy_to_vtk_wrapper(array, deep=False)
        vtkarray.SetName(arrayName)
        attributes.AddArray(vtkarray)
    if (isScalars):
        attributes.SetActiveScalars(arrayName)


def putMaskOnVTKGrid(data, grid, actorColor=None, cellData=True, deep=True):
    # Ok now looking
    msk = data.mask
    mapper = None
    if msk is not numpy.ma.nomask and not numpy.allclose(msk, False):
        if actorColor is not None:
            flatIMask = msk.astype(numpy.int).flat
            if grid.IsA("vtkStructuredGrid"):
                grid2 = vtk.vtkStructuredGrid()
                vtkmask = numpy_to_vtk_wrapper(flatIMask, deep=deep)
                attributes2 = grid2.GetCellData() if cellData else grid2.GetPointData()
            else:
                grid2 = vtk.vtkUnstructuredGrid()
                if (cellData):
                    attributes2 = grid2.GetCellData()
                    attributes = grid.GetCellData()
                else:
                    attributes2 = grid2.GetPointData()
                    attributes = grid.GetPointData()
                if (attributes.GetPedigreeIds()):
                    attributes2.SetPedigreeIds(attributes.GetPedigreeIds())
                    vtkmask = vtk.vtkIntArray()
                    vtkmask.SetNumberOfTuples(attributes2.GetPedigreeIds().GetNumberOfTuples())
                else:
                    # the unstructured grid is not wrapped
                    vtkmask = numpy_to_vtk_wrapper(flatIMask, deep=deep)
            vtkmask.SetName("scalar")
            attributes2.RemoveArray(vtk.vtkDataSetAttributes.GhostArrayName())
            attributes2.SetScalars(vtkmask)
            grid2.CopyStructure(grid)
            setArray(grid2, flatIMask, "scalar", isCellData=cellData,
                     isScalars=True)
            geoFilter = vtk.vtkDataSetSurfaceFilter()
            lut = vtk.vtkLookupTable()
            r, g, b, a = actorColor
            lut.SetNumberOfTableValues(2)
            geoFilter.SetInputData(grid2)
            if not cellData:
                lut.SetTableValue(0, r / 100., g / 100., b / 100., a / 100.)
                lut.SetTableValue(1, r / 100., g / 100., b / 100., a / 100.)
            else:
                lut.SetTableValue(0, r / 100., g / 100., b / 100., 0.)
                lut.SetTableValue(1, r / 100., g / 100., b / 100., 1.)
            geoFilter.Update()
            mapper = vtk.vtkPolyDataMapper()
            mapper.SetInputData(geoFilter.GetOutput())
            mapper.SetLookupTable(lut)
            mapper.SetScalarRange(0, 1)

        # The ghost array now stores information about hidden (blanked)
        # points/cells. Setting an array entry to the bitwise value
        # `vtkDataSetAttributes.HIDDEN(CELL|POINT)` will blank the cell/point.
        flatMask = msk.flat
        ghost = numpy.zeros(len(flatMask), dtype=numpy.uint8)
        invalidMaskValue = vtk.vtkDataSetAttributes.HIDDENCELL if cellData else \
            vtk.vtkDataSetAttributes.HIDDENPOINT
        for i, isInvalid in enumerate(flatMask):
            if isInvalid:
                ghost[i] = invalidMaskValue
        attributes = grid.GetCellData() if cellData else grid.GetPointData()
        pedigreeIds = attributes.GetPedigreeIds()
        if (pedigreeIds):
            # we need to create the ghost array because setArray does not create it in this case
            vtkghost = vtk.vtkUnsignedCharArray()
            vtkghost.SetNumberOfTuples(pedigreeIds.GetNumberOfTuples())
            vtkghost.SetName(vtk.vtkDataSetAttributes.GhostArrayName())
            attributes.AddArray(vtkghost)
        setArray(grid, ghost, vtk.vtkDataSetAttributes.GhostArrayName(),
                 cellData, isScalars=False)
    return mapper


def handleProjectionEdgeCases(projection, data):
    # For mercator projection, latitude values of -90 or 90
    # transformation result in infinity values. We chose -85, 85
    # as that's the typical limit used by the community.
    ptype = projDict.get(projection._type, projection.type)
    if (ptype.lower() == "merc"):
        lat = data.getLatitude()
        if isinstance(lat, cdms2.axis.TransientAxis):
            lat = lat[:]
            # Reverse the latitudes incase the starting latitude is greater
            # than the ending one
            if lat[-1] < lat[0]:
                lat = lat[::-1]
        data = data(latitude=(max(-85, lat.min()), min(85, lat.max())))
    return data


def getBoundsList(axis, hasCellData, dualGrid):
    '''
    Returns the bounds list for 'axis'. If axis has n elements the
    bounds list will have n+1 elements
    If there are not explicit bounds in the file we return None
    '''
    needsCellData = (hasCellData != dualGrid)
    axisBounds = axis.getBoundsForDualGrid(dualGrid)
    # we still have to generate bounds for non lon-lat axes, because
    # the default in axis.py is 2 (generate bounds only for lat/lon axis)
    # this is used for non lon-lat plots - by default numpy arrays are POINT data
    if (not axis.isLatitude() and not axis.isLongitude() and needsCellData):
        axisBounds = axis.genGenericBounds()
    if (axisBounds is not None):
        bounds = numpy.zeros(len(axis) + 1)
        if (axis[0] < axis[-1]):
            # axis is increasing
            if (axisBounds[0][0] < axisBounds[0][1]):
                # interval is increasing
                bounds[:len(axis)] = axisBounds[:, 0]
                bounds[len(axis)] = axisBounds[-1, 1]
            else:
                # interval is decreasing
                bounds[:len(axis)] = axisBounds[:, 1]
                bounds[len(axis)] = axisBounds[-1, 0]
        else:
            # axis is decreasing
            if (axisBounds[0][0] < axisBounds[0][1]):
                # interval is increasing
                bounds[:len(axis)] = axisBounds[:, 1]
                bounds[len(axis)] = axisBounds[-1, 0]
            else:
                # interval is decreasing
                bounds[:len(axis)] = axisBounds[:, 0]
                bounds[len(axis)] = axisBounds[-1, 1]
        return bounds
    else:
        return None


def setInfToValid(geoPoints, ghost):
    '''
    Set infinity points to a point that already exists in the list.
    If a ghost array is passed, we also hide infinity points.
    We return true if any points are infinity
    '''
    anyInfinity = False
    validPoint = [0, 0, 0]
    for i in range(geoPoints.GetNumberOfPoints()):
        point = geoPoints.GetPoint(i)
        if (not math.isinf(point[0]) and not math.isinf(point[1])):
            validPoint[0] = point[0]
            validPoint[1] = point[1]
            break
    for i in range(geoPoints.GetNumberOfPoints()):
        point = geoPoints.GetPoint(i)
        if (math.isinf(point[0]) or math.isinf(point[1])):
            anyInfinity = True
            newPoint = list(point)
            if (math.isinf(point[0])):
                newPoint[0] = validPoint[0]
            if (math.isinf(point[1])):
                newPoint[1] = validPoint[1]
            geoPoints.SetPoint(i, newPoint)
            ghost.SetValue(i, vtk.vtkDataSetAttributes.HIDDENPOINT)
    return anyInfinity


def genGrid(data1, data2, gm, deep=True, grid=None, geo=None, genVectors=False,
            dualGrid=False):
    continents = False
    wrap = None
    m3 = None
    g = None
    cellData = True
    xm, xM, ym, yM = None, None, None, None
    projection = vcs.elements["projection"][gm.projection]

    data1 = handleProjectionEdgeCases(projection, data1)
    if data2 is not None:
        data2 = handleProjectionEdgeCases(projection, data2)

    try:  # First try to see if we can get a mesh out of this
        g = data1.getGrid()
        # Ok need unstructured grid
        if isinstance(g, cdms2.gengrid.AbstractGenericGrid):
            continents = True
            wrap = [0., 360.]
            if grid is None:
                m = g.getMesh()
                xm = m[:, 1].min()
                xM = m[:, 1].max()
                ym = m[:, 0].min()
                yM = m[:, 0].max()
                numberOfCells = m.shape[0]
                # For vtk we need to reorder things
                m2 = numpy.ascontiguousarray(numpy.transpose(m, (0, 2, 1)))
                m2.resize((m2.shape[0] * m2.shape[1], m2.shape[2]))
                m2 = m2[..., ::-1]
                # here we add dummy levels, might want to reconsider converting
                # "trimData" to "reOrderData" and use actual levels?
                m3 = numpy.concatenate(
                    (m2, numpy.zeros(
                        (m2.shape[0], 1))), axis=1)
        # Could still be meshfill with mesh data
        # Ok probably should do a test for hgrid before sending data2
        if isinstance(gm, meshfill.Gfm) and data2 is not None:
            wrap = gm.wrap
            if gm.wrap[1] == 360.:
                continents = True
            if grid is None:
                xm = data2[:, 1].min()
                xM = data2[:, 1].max()
                ym = data2[:, 0].min()
                yM = data2[:, 0].max()
                numberOfCells = data2.shape[0]
                data2 = data2.filled(numpy.nan)
                m2 = numpy.ascontiguousarray(numpy.transpose(data2, (0, 2, 1)))
                nVertices = m2.shape[-2]
                m2.resize((m2.shape[0] * m2.shape[1], m2.shape[2]))
                m2 = m2[..., ::-1]
                # here we add dummy levels, might want to reconsider converting
                # "trimData" to "reOrderData" and use actual levels?
                m3 = numpy.concatenate(
                    (m2, numpy.zeros(
                        (m2.shape[0], 1))), axis=1)
    except Exception:  # Ok no mesh on file, will do with lat/lon
        pass
    if m3 is not None:
        # Create unstructured grid points
        vg = vtk.vtkUnstructuredGrid()
        for i in range(numberOfCells):
            pt_ids = []
            for j in range(nVertices):
                indx = i * nVertices + j
                if not numpy.isnan(m3[indx][0]):  # missing value means skip vertex
                    pt_ids.append(indx)
            vg.InsertNextCell(vtk.VTK_POLYGON,
                              len(pt_ids),
                              pt_ids)
    else:
        # Ok a simple structured grid is enough
        if grid is None:
            vg = vtk.vtkStructuredGrid()
        hasCellData = data1.hasCellData()
        if g is not None:
            # Ok we have grid
            continents = True
            wrap = [0., 360.]
            if grid is None:
                lat = g.getLatitude()
                lon = g.getLongitude()
            if isinstance(g, cdms2.hgrid.AbstractCurveGrid):
                # Ok in this case it is a structured grid we
                # have no bounds, using ponitData
                # ??? We should probably revist and see if we
                # can use the "bounds" attribute
                # to generate cell instead of points
                cellData = False
            elif grid is None:
                lon = data1.getAxis(-1)
                lat = data1.getAxis(-2)
                # Ok let's try to get the bounds
                lon2 = getBoundsList(lon, hasCellData, dualGrid)
                lat2 = getBoundsList(lat, hasCellData, dualGrid)
                if (lon2 is not None and lat2 is not None):
                    lon3 = lon2
                    lat3 = lat2
                else:
                    lon3 = lon
                    lat3 = lat
                    cellData = False
                # Note that m,M is min,max for an increasing list
                # and max,min for a decreasing list
                xm = lon3[0]
                xM = lon3[-1]
                ym = lat3[0]
                yM = lat3[-1]

                lat = lat3[:, numpy.newaxis] * numpy.ones(lon3.shape)[numpy.newaxis, :]
                lon = lon3[numpy.newaxis, :] * numpy.ones(lat3.shape)[:, numpy.newaxis]
        elif grid is None:
            # No grid info from data, making one up
            data1 = cdms2.asVariable(data1)
            lon = data1.getAxis(-1)
            lat = data1.getAxis(-2)
            # Ok let's try to get the bounds
            lon2 = getBoundsList(lon, hasCellData, dualGrid)
            lat2 = getBoundsList(lat, hasCellData, dualGrid)
            if (lon2 is not None and lat2 is not None):
                lon3 = lon2
                lat3 = lat2
            else:
                lon3 = lon
                lat3 = lat
                cellData = False
            # Note that m,M is min,max for an increasing list
            # and max,min for a decreasing list
            xm = lon3[0]
            xM = lon3[-1]
            ym = lat3[0]
            yM = lat3[-1]
            lat = lat3[:, numpy.newaxis] * \
                numpy.ones(lon3.shape)[numpy.newaxis, :]
            lon = lon3[numpy.newaxis, :] * \
                numpy.ones(lat3.shape)[:, numpy.newaxis]
        if grid is None:
            vg.SetDimensions(lat.shape[1], lat.shape[0], 1)
            lon = numpy.ma.ravel(lon)
            lat = numpy.ma.ravel(lat)
            sh = list(lat.shape)
            sh.append(1)
            lon = numpy.ma.reshape(lon, sh)
            lat = numpy.ma.reshape(lat, sh)
            z = numpy.zeros(lon.shape)
            m3 = numpy.concatenate((lon, lat), axis=1)
            m3 = numpy.concatenate((m3, z), axis=1)
            if xm is None:
                try:
                    xm = lon[0]
                    xM = lon[-1]
                    ym = lat[0]
                    yM = lat[-1]
                except:
                    xm = lon.min()
                    xM = lon.max()
                    ym = lat.min()
                    yM = lat.max()

    # attribute data
    gridForAttribute = grid if grid else vg
    if genVectors:
        attribute = generateVectorArray(data1, data2, gridForAttribute)
    else:
        attribute = numpy_to_vtk_wrapper(data1.filled(0.).flat,
                                         deep=False)
        attribute.SetName("scalar")
    if cellData:
        attributes = gridForAttribute.GetCellData()
    else:
        attributes = gridForAttribute.GetPointData()
    if genVectors:
        attributes.SetVectors(attribute)
    else:
        attributes.SetScalars(attribute)

    if grid is None:
        # First create the points/vertices (in vcs terms)
        pts = vtk.vtkPoints()
        # Convert nupmy array to vtk ones
        ppV = numpy_to_vtk_wrapper(m3, deep=deep)
        pts.SetData(ppV)
        ptsBounds = pts.GetBounds()
        xRange = ptsBounds[1] - ptsBounds[0]
        xm, xM, ym, yM, tmp, tmp2 = pts.GetBounds()
        if (isinstance(g, cdms2.hgrid.TransientCurveGrid) and
                xRange > 360 and not numpy.isclose(xRange, 360)):
            vg.SetPoints(pts)
            # index into the scalar array. Used for upgrading
            # the scalar after wrapping. Note this will work
            # correctly only for cell data. For point data
            # the indexes for points on the border will be incorrect after
            # wrapping
            pedigreeId = vtk.vtkIntArray()
            pedigreeId.SetName("PedigreeIds")
            pedigreeId.SetNumberOfTuples(attribute.GetNumberOfTuples())
            for i in range(0, attribute.GetNumberOfTuples()):
                pedigreeId.SetValue(i, i)
            if cellData:
                vg.GetCellData().SetPedigreeIds(pedigreeId)
            else:
                vg.GetPointData().SetPedigreeIds(pedigreeId)
            vg = wrapDataSetX(vg)
            pts = vg.GetPoints()
            xm, xM, ym, yM, tmp, tmp2 = vg.GetPoints().GetBounds()
    else:
        xm, xM, ym, yM, tmp, tmp2 = grid.GetPoints().GetBounds()
    projection = vcs.elements["projection"][gm.projection]
    if grid is None:
        vg.SetPoints(pts)
        # We use the zooming feature for linear and polar projections
        # We use plotting coordinates for doing the projection
        # such that parameters such that central meridian are set correctly
        if (gm.g_name == 'Gfm'):
            # axes are not lon/lat for meshfill
            wc = [gm.datawc_x1, gm.datawc_x2, gm.datawc_y1, gm.datawc_y2]
        else:
            wc = vcs.utils.getworldcoordinates(gm,
                                               data1.getAxis(-1),
                                               data1.getAxis(-2))
        geo, geopts = project(pts, projection, getWrappedBounds(
            wc, [xm, xM, ym, yM], wrap))
        # proj4 returns inf for points that are not visible. Set those to a valid point
        # and hide them.
        ghost = vg.AllocatePointGhostArray()
        if (setInfToValid(geopts, ghost)):
            # if there are hidden points, we recompute the bounds
            xm = ym = sys.float_info.max
            xM = yM = - sys.float_info.max
            for i in range(pts.GetNumberOfPoints()):
                if (ghost.GetValue(i) & vtk.vtkDataSetAttributes.HIDDENPOINT == 0):
                    # point not hidden
                    p = pts.GetPoint(i)
                    if (p[0] < xm):
                        xm = p[0]
                    if (p[0] > xM):
                        xM = p[0]
                    if (p[1] < ym):
                        ym = p[1]
                    if (p[1] > yM):
                        yM = p[1]
        # Sets the vertics into the grid
        vg.SetPoints(geopts)
    else:
        vg = grid
    # Add a GlobalIds array to keep track of cell ids throughout the pipeline
    globalIds = numpy_to_vtk_wrapper(numpy.arange(0, vg.GetNumberOfCells()), deep=True)
    globalIds.SetName('GlobalIds')
    vg.GetCellData().SetGlobalIds(globalIds)
    out = {"vtk_backend_grid": vg,
           "xm": xm,
           "xM": xM,
           "ym": ym,
           "yM": yM,
           "continents": continents,
           "wrap": wrap,
           "geo": geo,
           "cellData": cellData,
           "data": data1,
           "data2": data2
           }
    return out

# Continents first
# Try to save time and memorize these continents
vcsContinents = {}


def prepContinents(fnm):
    """ This converts vcs continents files to vtkpolydata
    Author: Charles Doutriaux
    Input: vcs continent file name
    """
    if fnm in vcsContinents:
        return vcsContinents[fnm]
    poly = vtk.vtkPolyData()
    cells = vtk.vtkCellArray()
    pts = vtk.vtkPoints()
    f = open(fnm)
    ln = f.readline()
    while ln.strip().split() != ["-99", "-99"]:
        # Many lines, need to know number of points
        N = int(ln.split()[0])
        # Now create and store these points
        n = 0
        npts = pts.GetNumberOfPoints()
        while n < N:
            ln = f.readline()
            sp = ln.split()
            sn = len(sp)
            didIt = False
            if sn % 2 == 0:
                try:
                    spts = []
                    for i in range(sn / 2):
                        l, L = float(sp[i * 2]), float(sp[i * 2 + 1])
                        spts.append([l, L])
                    for p in spts:
                        pts.InsertNextPoint(p[1], p[0], 0.)
                    n += sn
                    didIt = True
                except:
                    didIt = False
            if didIt is False:
                while len(ln) > 2:
                    l, L = float(ln[:8]), float(ln[8:16])
                    pts.InsertNextPoint(L, l, 0.)
                    ln = ln[16:]
                    n += 2
        ln = vtk.vtkPolyLine()
        ln.GetPointIds().SetNumberOfIds(N / 2)
        for i in range(N / 2):
            ln.GetPointIds().SetId(i, i + npts)
        cells.InsertNextCell(ln)
        ln = f.readline()
    poly.SetPoints(pts)
    poly.SetLines(cells)

    # The dataset has some duplicate lines that extend
    # outside of x=[-180, 180],
    # which will cause wrapping artifacts for
    # certain projections (e.g.
    # Robinson). Clip out the duplicate data:
    box = vtk.vtkBox()
    box.SetXMin(-180., -90., 0.)
    box.SetXMax(180., 90., 1.)
    clipper = vtk.vtkClipPolyData()
    clipper.SetInputData(poly)
    clipper.InsideOutOn()
    clipper.SetClipFunction(box)
    clipper.Update()
    poly = clipper.GetOutput()

    vcsContinents[fnm] = poly
    return poly


def apply_proj_parameters(pd, projection, x1, x2, y1, y2):
    pname = projDict.get(projection._type, projection.type)
    projName = pname
    pd.SetName(projName)
    if projection.type == "polar (non gctp)":
        if (y1 < y2):
            pd.SetOptionalParameter("lat_0", "-90.")
            pd.SetCentralMeridian(x1)
        else:
            pd.SetOptionalParameter("lat_0", "90.")
            pd.SetCentralMeridian(x1 + 180.)
    else:
        if projection.type not in no_over_proj4_parameter_projections:
            pd.SetOptionalParameter("over", "true")
        else:
            pd.SetOptionalParameter("over", "false")
            setProjectionParameters(pd, projection)
        if (hasattr(projection, 'centralmeridian')):
            if (numpy.allclose(projection.centralmeridian, 1e+20)):
                centralmeridian = float(x1 + x2) / 2.0
            else:
                centralmeridian = projection.centralmeridian
            pd.SetCentralMeridian(centralmeridian)
        if (hasattr(projection, 'centerlongitude')):
            if (numpy.allclose(projection.centerlongitude, 1e+20)):
                centerlongitude = float(x1 + x2) / 2.0
            else:
                centerlongitude = projection.centerlongitude
            pd.SetOptionalParameter("lon_0", str(centerlongitude))
        if (hasattr(projection, 'originlatitude')):
            if (numpy.allclose(projection.originlatitude, 1e+20)):
                originlatitude = float(y1 + y2) / 2.0
            else:
                originlatitude = projection.originlatitude
            pd.SetOptionalParameter("lat_0", str(originlatitude))
        if (hasattr(projection, 'centerlatitude')):
            if (numpy.allclose(projection.centerlatitude, 1e+20)):
                centerlatitude = float(y1 + y2) / 2.0
            else:
                centerlatitude = projection.centerlatitude
            pd.SetOptionalParameter("lat_0", str(centerlatitude))
        if (hasattr(projection, 'standardparallel1')):
            if (numpy.allclose(projection.standardparallel1, 1.e20)):
                standardparallel1 = min(y1, y2)
            else:
                standardparallel1 = projection.standardparallel1
            pd.SetOptionalParameter('lat_1', str(standardparallel1))
        if (hasattr(projection, 'standardparallel2')):
            if (numpy.allclose(projection.standardparallel2, 1.e20)):
                standardparallel2 = max(y1, y2)
            else:
                standardparallel2 = projection.standardparallel2
            pd.SetOptionalParameter('lat_2', str(standardparallel2))


def projectArray(w, projection, wc, geo=None):
    x1, x2, y1, y2 = wc
    if isinstance(projection, (str, unicode)):
        projection = vcs.elements["projection"][projection]
    if projection.type == "linear":
        return None, w

    if geo is None:
        geo = vtk.vtkGeoTransform()
        ps = vtk.vtkGeoProjection()
        pd = vtk.vtkGeoProjection()

        apply_proj_parameters(pd, projection, x1, x2, y1, y2)

        geo.SetSourceProjection(ps)
        geo.SetDestinationProjection(pd)

    for i in range(0, w.GetNumberOfTuples()):
        tuple = [0, 0, 0]
        w.GetTupleValue(i, tuple)
        geo.TransformPoint(tuple, tuple)
        w.SetTupleValue(i, tuple)


# Geo projection
def project(pts, projection, wc, geo=None):
    x1, x2, y1, y2 = wc
    if isinstance(projection, (str, unicode)):
        projection = vcs.elements["projection"][projection]
    if projection.type == "linear":
        return None, pts
    if geo is None:
        geo = vtk.vtkGeoTransform()
        ps = vtk.vtkGeoProjection()
        pd = vtk.vtkGeoProjection()

        apply_proj_parameters(pd, projection, x1, x2, y1, y2)

        geo.SetSourceProjection(ps)
        geo.SetDestinationProjection(pd)
    geopts = vtk.vtkPoints()
    geo.TransformPoints(pts, geopts)
    return geo, geopts


def setProjectionParameters(pd, proj):
    if proj._type > 200:
        proj4 = proj.parameters
        if numpy.allclose(proj4, 1.e20):
            proj4 = {}
    else:
        p = proj.parameters
        proj4 = {}
        if proj._type in [3, 4]:
            proj4["a"] = proj.smajor
            proj4["b"] = proj.sminor
            proj4["lat_1"] = proj.standardparallel1
            proj4["lat_2"] = proj.standardparallel2
            proj4["lon_0"] = proj.centralmeridian
            proj4["lat_0"] = proj.originlatitude
            proj4["x_0"] = proj.falseeasting
            proj4["y_0"] = proj.falsenorthing
        elif proj._type == 5:
            proj4["a"] = proj.smajor
            proj4["b"] = proj.sminor
            proj4["lon_0"] = proj.centralmeridian
            proj4["lat_ts"] = proj.truescale
            proj4["x_0"] = proj.falseeasting
            proj4["y_0"] = proj.falsenorthing
        elif proj._type == 6:
            proj4["a"] = proj.smajor
            proj4["b"] = proj.sminor
            proj4["lon_wrap"] = proj.centerlongitude  # MAP NAME ?????
            proj4["lat_ts"] = proj.truescale
            proj4["x_0"] = proj.falseeasting
            proj4["y_0"] = proj.falsenorthing
        elif proj._type == 7:
            proj4["a"] = proj.smajor
            proj4["b"] = proj.sminor
            proj4["lon_0"] = proj.centralmeridian
            proj4["lat_0"] = proj.originlatitude
            proj4["x_0"] = proj.falseeasting
            proj4["y_0"] = proj.falsenorthing
        elif proj._type == 8:
            proj4["a"] = proj.smajor
            proj4["b"] = proj.sminor
            proj4["lon_0"] = proj.centralmeridian
            proj4["lat_0"] = proj.originlatitude
            proj4["x_0"] = proj.falseeasting
            proj4["y_0"] = proj.falsenorthing
            if (p[8] == 0 or p[8] > 9.9E19):
                proj4["subtype"] = proj.subtype = 0
                proj4["lat_1"] = proj.standardparallel  # MAP NAME ?????
                proj4["lat_2"] = proj.standardparallel  # MAP NAME ?????
                proj4["lat_ts"] = proj.standardparallel  # MAP NAME ?????
            else:
                proj4["subtype"] = proj.subtype = 1
                proj4["lat_1"] = proj.standardparallel1
                proj4["lat_2"] = proj.standardparallel2
        elif proj._type == 9:
            proj4["a"] = proj.smajor
            proj4["b"] = proj.sminor
            proj4["k_0"] = proj.factor
            proj4["lon_0"] = proj.centralmeridian
            proj4["lat_0"] = proj.originlatitude
            proj4["x_0"] = proj.falseeasting
            proj4["y_0"] = proj.falsenorthing
        elif proj._type in [10, 11, 12, 13, 14]:
            proj4["a"] = proj.sphere  # MAP NAME ?????
            proj4["b"] = proj.sphere  # MAP NAME ?????
            proj4["lon_0"] = proj.centerlongitude
            proj4["lat_0"] = proj.centerlatitude
            proj4["x_0"] = proj.falseeasting
            proj4["y_0"] = proj.falsenorthing
        elif proj._type == 15:
            proj4["a"] = proj.sphere  # MAP NAME ?????
            proj4["b"] = proj.sphere  # MAP NAME ?????
            proj4["height"] = proj.height  # MAP NAME ?????
            proj4["lon_0"] = proj.centerlongitude
            proj4["lat_0"] = proj.centerlatitude
            proj4["x_0"] = proj.falseeasting
            proj4["y_0"] = proj.falsenorthing
        elif proj._type in [16, 18, 21, 25, 27, 28, 29]:
            proj4["a"] = proj.sphere  # MAP NAME ?????
            proj4["b"] = proj.sphere  # MAP NAME ?????
            proj4["lon_0"] = proj.centralmeridian
            proj4["x_0"] = proj.falseeasting
            proj4["y_0"] = proj.falsenorthing
        elif proj._type == 17:
            proj4["a"] = proj.sphere
            proj4["b"] = proj.sphere
            proj4["lon_0"] = proj.centralmeridian
            proj4["lat_ts"] = proj.truescale
            proj4["x_0"] = proj.falseeasting
            proj4["y_0"] = proj.falsenorthing
        elif proj._type == 19:
            proj4["a"] = proj.sphere  # MAP NAME ?????
            proj4["b"] = proj.sphere  # MAP NAME ?????
            proj4["lon_0"] = proj.centralmeridian
            proj4["lat_0"] = proj.originlatitude
            proj4["x_0"] = proj.falseeasting
            proj4["y_0"] = proj.falsenorthing
        elif proj._type == 20:
            proj4["a"] = proj.smajor
            proj4["b"] = proj.sminor
            proj4["k_0"] = proj.factor
            proj4["lat_0"] = proj.originlatitude
            proj4["x_0"] = proj.falseeasting
            proj4["y_0"] = proj.falsenorthing
            if (p[12] == 0 or p[12] > 9.9E19):
                proj4["subtype"] = proj.subtype
                proj4["lon_0"] = proj.longitude1  # MAP NAME ?????
                proj4["lat_1"] = proj.latitude1
                proj4["lonc"] = proj.longitude2  # MAP NAME ?????
                proj4["lat_2"] = proj.latitude2
            else:
                proj4["subtype"] = proj.subtype
                proj4["azi"] = proj.azimuthalangle  # MAP NAME ?????
                proj4["lon_0"] = proj.azimuthallongitude  # MAP NAME ?????
        elif proj._type == 22:
            proj4["a"] = proj.smajor
            proj4["b"] = proj.sminor
            proj4["x_0"] = proj.falseeasting
            proj4["y_0"] = proj.falsenorthing
            if (p[12] == 0 or p[12] > 9.9E19):
                proj4["subtype"] = proj.subtype
                proj4["???"] = proj.orbitinclination  # MAP NAME ?????
                proj4["???"] = proj.orbitlongitude  # MAP NAME ?????
                proj4["???"] = proj.satelliterevolutionperiod  # MAP NAME ?????
                proj4["???"] = proj.landsatcompensationratio  # MAP NAME ?????
                proj4["???"] = proj.pathflag  # MAP NAME ?????
            else:
                proj4["subtype"] = proj.subtype
                proj4["???"] = proj.satellite  # MAP NAME ?????
                proj4["???"] = proj.path  # MAP NAME ?????
        elif proj._type == 23:
            proj4["a"] = proj.smajor
            proj4["b"] = proj.sminor
            proj4["x_0"] = proj.falseeasting
            proj4["y_0"] = proj.falsenorthing
        elif proj._type in [24, 26]:
            proj4["a"] = proj.sphere  # MAP NAME ?????
            proj4["b"] = proj.sphere  # MAP NAME ?????
        elif proj._type == 30:
            proj4["a"] = proj.sphere  # MAP NAME ?????
            proj4["b"] = proj.sphere  # MAP NAME ?????
            proj4["???"] = proj.shapem  # MAP NAME ?????
            proj4["???"] = proj.shapen  # MAP NAME ?????
            proj4["lon_0"] = proj.centerlongitude
            proj4["lat_0"] = proj.centerlatitude
            proj4["x_0"] = proj.falseeasting
            proj4["y_0"] = proj.falsenorthing

    if proj._type == 6:
        pd.SetOptionalParameter("lat_0", "90")
    for k in proj4:
        if not numpy.allclose(proj4[k], 1.e20):
            if k == "lon_0":
                pd.SetCentralMeridian(proj4[k])
            elif k != "???":
                pd.SetOptionalParameter(k, str(proj4[k]))

# Vtk dump
dumps = {}


def dump2VTK(obj, fnm=None):
    global dumps
    if fnm is None:
        fnm = "foo.vtk" % dumps
    if fnm[:-4].lower() != ".vtk":
        fnm += ".vtk"
    if fnm in dumps:
        dumps[fnm] += 1
        fnm = fnm[:-4] + "%.3i.vtk" % dumps[fnm]
    else:
        dumps[fnm] = 0
    dsw = vtk.vtkDataSetWriter()
    dsw.SetFileName(fnm)
    try:
        dsw.SetInputData(obj)
    except:
        dsw.SetInputConnection(obj.GetOutputPort())

    dsw.Write()


# Wrapping around
def doWrap(Act, wc, wrap=[0., 360], fastClip=True):
    if wrap is None:
        return Act
    Mapper = Act.GetMapper()
    Mapper.Update()
    data = Mapper.GetInput()
    # insure that GLOBALIDS are not removed by the append filter
    attributes = data.GetCellData()
    attributes.SetActiveAttribute(-1, attributes.GLOBALIDS)
    xmn = min(wc[0], wc[1])
    xmx = max(wc[0], wc[1])
    if numpy.allclose(xmn, 1.e20) or numpy.allclose(xmx, 1.e20):
        xmx = abs(wrap[1])
        xmn = -wrap[1]
    ymn = min(wc[2], wc[3])
    ymx = max(wc[2], wc[3])
    if numpy.allclose(ymn, 1.e20) or numpy.allclose(ymx, 1.e20):
        ymx = abs(wrap[0])
        ymn = -wrap[0]

    appendFilter = vtk.vtkAppendPolyData()
    appendFilter.AddInputData(data)
    appendFilter.Update()
    # X axis wrappping
    Amn, Amx = Act.GetXRange()
    if wrap[1] != 0.:
        i = 0
        while Amn > xmn:
            i += 1
            Amn -= wrap[1]
            Tpf = vtk.vtkTransformPolyDataFilter()
            Tpf.SetInputData(data)
            T = vtk.vtkTransform()
            T.Translate(-i * wrap[1], 0, 0)
            Tpf.SetTransform(T)
            Tpf.Update()
            appendFilter.AddInputData(Tpf.GetOutput())
            appendFilter.Update()
        i = 0
        while Amx < xmx:
            i += 1
            Amx += wrap[1]
            Tpf = vtk.vtkTransformPolyDataFilter()
            Tpf.SetInputData(data)
            T = vtk.vtkTransform()
            T.Translate(i * wrap[1], 0, 0)
            Tpf.SetTransform(T)
            Tpf.Update()
            appendFilter.AddInputData(Tpf.GetOutput())
            appendFilter.Update()

    # Y axis wrapping
    Amn, Amx = Act.GetYRange()
    if wrap[0] != 0.:
        i = 0
        while Amn > ymn:
            i += 1
            Amn -= wrap[0]
            Tpf = vtk.vtkTransformPolyDataFilter()
            Tpf.SetInputData(data)
            T = vtk.vtkTransform()
            T.Translate(0, i * wrap[0], 0)
            Tpf.SetTransform(T)
            Tpf.Update()
            appendFilter.AddInputData(Tpf.GetOutput())
            appendFilter.Update()
        i = 0
        while Amx < ymx:
            i += 1
            Amx += wrap[0]
            Tpf = vtk.vtkTransformPolyDataFilter()
            Tpf.SetInputData(data)
            T = vtk.vtkTransform()
            T.Translate(0, -i * wrap[0], 0)
            Tpf.SetTransform(T)
            Tpf.Update()
            appendFilter.AddInputData(Tpf.GetOutput())
            appendFilter.Update()

    # Clip the data to the final window:
    clipBox = vtk.vtkBox()
    clipBox.SetXMin(xmn, ymn, -1.0)
    clipBox.SetXMax(xmx, ymx, 1.0)
    if fastClip:
        clipper = vtk.vtkExtractPolyDataGeometry()
        clipper.ExtractInsideOn()
        clipper.SetImplicitFunction(clipBox)
        clipper.ExtractBoundaryCellsOn()
        clipper.PassPointsOff()
    else:
        clipper = vtk.vtkClipPolyData()
        clipper.InsideOutOn()
        clipper.SetClipFunction(clipBox)
    clipper.SetInputConnection(appendFilter.GetOutputPort())
    clipper.Update()
    # set globalids attribute
    attributes = clipper.GetOutput().GetCellData()
    globalIdsIndex = vtk.mutable(-1)
    attributes.GetArray("GlobalIds", globalIdsIndex)
    attributes.SetActiveAttribute(globalIdsIndex, attributes.GLOBALIDS)

    Mapper.SetInputData(clipper.GetOutput())
    return Act


# Wrap grid in interval minX, minX + 360
# minX is the minimum x value for 'grid'
def wrapDataSetX(grid):
    # Clip the dataset into 2 pieces: left and right
    bounds = grid.GetBounds()
    minX = bounds[0]
    intervalX = 360
    maxX = minX + intervalX

    plane = vtk.vtkPlane()
    plane.SetOrigin(maxX, 0, 0)
    plane.SetNormal(1, 0, 0)

    clipRight = vtk.vtkClipDataSet()
    clipRight.SetClipFunction(plane)
    clipRight.SetInputData(grid)
    clipRight.Update()
    right = clipRight.GetOutputDataObject(0)

    plane.SetNormal(-1, 0, 0)
    clipLeft = vtk.vtkClipDataSet()
    clipLeft.SetClipFunction(plane)
    clipLeft.SetInputData(grid)
    clipLeft.Update()
    left = clipLeft.GetOutputDataObject(0)

    # translate the right piece
    tl = vtk.vtkTransform()
    tl.Translate(-intervalX, 0, 0)
    translateLeft = vtk.vtkTransformFilter()
    translateLeft.SetTransform(tl)
    translateLeft.SetInputData(right)
    translateLeft.Update()
    right = translateLeft.GetOutput()

    # append the pieces together
    append = vtk.vtkAppendFilter()
    append.AddInputData(left)
    append.AddInputData(right)
    append.MergePointsOn()
    append.Update()
    whole = append.GetOutput()

    return whole


def setClipPlanes(mapper, xmin, xmax, ymin, ymax):
    clipPlaneCollection = vtk.vtkPlaneCollection()

    if xmin != xmax:
        clipPlaneXMin = vtk.vtkPlane()
        clipPlaneXMin.SetOrigin(xmin, 0.0, 0.0)
        clipPlaneXMin.SetNormal(1.0, 0.0, 0.0)

        clipPlaneXMax = vtk.vtkPlane()
        clipPlaneXMax.SetOrigin(xmax, 0.0, 0.0)
        clipPlaneXMax.SetNormal(-1.0, 0.0, 0.0)

        clipPlaneCollection.AddItem(clipPlaneXMin)
        clipPlaneCollection.AddItem(clipPlaneXMax)

    if ymin != ymax:
        clipPlaneYMin = vtk.vtkPlane()
        clipPlaneYMin.SetOrigin(0.0, ymin, 0.0)
        clipPlaneYMin.SetNormal(0.0, 1.0, 0.0)

        clipPlaneYMax = vtk.vtkPlane()
        clipPlaneYMax.SetOrigin(0.0, ymax, 0.0)
        clipPlaneYMax.SetNormal(0.0, -1.0, 0.0)

        clipPlaneCollection.AddItem(clipPlaneYMin)
        clipPlaneCollection.AddItem(clipPlaneYMax)

    if clipPlaneCollection.GetNumberOfItems() > 0:
        mapper.SetClippingPlanes(clipPlaneCollection)

# The code is replaced by the setClipPlanes above
# def doClip(data, xmin,xmax,ymin,ymax):
#   if xmin!=xmax:
#     xminClip = doClip1(data,xmin,1,0)
#     xfullClip = doClip1(xminClip,xmax,-1,0)
#   else:
#     xfullClip = data
#   if ymin!=ymax:
#     yminClip  = doClip1(xfullClip,ymin,1,1)
#     xyClip  = doClip1(yminClip,ymax,-1,1)
#   else:
#     xyClip = xfullClip
#   return xyClip

# def doClip1(data,value,normal,axis=0):
#     return data
# We have the actor, do clipping
#     clpf = vtk.vtkPlane()
#     if axis == 0:
#       clpf.SetOrigin(value,0,0)
#       clpf.SetNormal(normal,0,0)
#     else:
#       clpf.SetOrigin(0,value,0)
#       clpf.SetNormal(0,normal,0)
#     clp = vtk.vtkClipPolyData()
#     clp.SetClipFunction(clpf)
#     clp.SetInputData(data)
#     clp.Update()
#     return clp.GetOutput()


def prepTextProperty(p, winSize, to="default", tt="default", cmap=None,
                     overrideColorIndex=None):
    if isinstance(to, str):
        to = vcs.elements["textorientation"][to]
    if isinstance(tt, str):
        tt = vcs.elements["texttable"][tt]

    if tt.colormap is not None:
        cmap = tt.colormap
    elif cmap is None:
        cmap = vcs._colorMap
    if isinstance(cmap, str):
        cmap = vcs.elements["colormap"][cmap]
    colorIndex = overrideColorIndex if overrideColorIndex else tt.color
    if isinstance(colorIndex, int):
        c = cmap.index[colorIndex]
    else:
        c = colorIndex
    p.SetColor([C / 100. for C in c[:3]])
    p.SetOpacity(c[-1])
    bcolorIndex = tt.backgroundcolor if tt.backgroundcolor else 255
    if isinstance(bcolorIndex, int):
        bc = cmap.index[bcolorIndex]
    else:
        bc = bcolorIndex
    p.SetBackgroundColor([C / 100. for C in bc[:3]])
    bopacity = (tt.backgroundopacity / 100.) if tt.backgroundopacity else 0
    p.SetBackgroundOpacity(bopacity)
    if to.halign in [0, 'left']:
        p.SetJustificationToLeft()
    elif to.halign in [2, 'right']:
        p.SetJustificationToRight()
    elif to.halign in [1, 'center']:
        p.SetJustificationToCentered()

    p.SetOrientation(-to.angle)

    if to.valign in [0, 'top']:
        p.SetVerticalJustificationToTop()
    elif to.valign in [2, 'half']:
        p.SetVerticalJustificationToCentered()
    elif to.valign in [4, 'bottom']:
        p.SetVerticalJustificationToBottom()
    elif to.valign in [1, 'cap']:
        warnings.warn("VTK does not support 'cap' align, using 'top'")
        p.SetVerticalJustificationToTop()
    elif to.valign in [3, 'base']:
        warnings.warn("VTK does not support 'base' align, using 'bottom'")
        p.SetVerticalJustificationToBottom()
    p.SetFontFamily(vtk.VTK_FONT_FILE)
    p.SetFontFile(vcs.elements["font"][vcs.elements["fontNumber"][tt.font]])
    p.SetFontSize(int(to.height * winSize[1] / 800.))


def genTextActor(renderer, string=None, x=None, y=None,
                 to='default', tt='default', cmap=None, geoBounds=None, geo=None):
    if isinstance(to, str):
        to = vcs.elements["textorientation"][to]
    if isinstance(tt, str):
        tt = vcs.elements["texttable"][tt]
    if tt.priority == 0:
        return []
    if string is None:
        string = tt.string
    if x is None:
        x = tt.x
    if y is None:
        y = tt.y
    if x is None or y is None or string in [['', ], []]:
        return []

    n = max(len(x), len(y), len(string))
    for a in [x, y, string]:
        while len(a) < n:
            a.append(a[-1])

    sz = renderer.GetRenderWindow().GetSize()
    actors = []
    pts = vtk.vtkPoints()
    if vcs.elements["projection"][tt.projection].type != "linear":
        wc = geoBounds[:4]
        # renderer.SetViewport(tt.viewport[0],tt.viewport[2],tt.viewport[1],tt.viewport[3])
        renderer.SetWorldPoint(wc)

    for i in range(n):
        t = vtk.vtkTextActor()
        p = t.GetTextProperty()
        prepTextProperty(p, sz, to, tt, cmap)
        pts = vtk.vtkPoints()
        pts.InsertNextPoint(x[i], y[i], 0.)
        if vcs.elements["projection"][tt.projection].type != "linear":
            _, pts = project(pts, tt.projection, tt.worldcoordinate, geo=geo)
            X, Y, tz = pts.GetPoint(0)
            X, Y = world2Renderer(renderer, X, Y, tt.viewport, wc)
        else:
            X, Y = world2Renderer(
                renderer, x[i], y[i], tt.viewport, tt.worldcoordinate)
        t.SetPosition(X, Y)
        t.SetInput(string[i])
        # T=vtk.vtkTransform()
        # T.Scale(1.,sz[1]/606.,1.)
        # T.RotateY(to.angle)
        # t.SetUserTransform(T)
        renderer.AddActor(t)
        actors.append(t)
    return actors


def prepPrimitive(prim):
    if prim.x is None or prim.y is None:
        return 0
    if not isinstance(prim.x[0], (list, tuple)):
        prim.x = [prim.x, ]
    if not isinstance(prim.y[0], (list, tuple)):
        prim.y = [prim.y, ]
    if vcs.isfillarea(prim):
        atts = ["x", "y", "color", "style", "index"]
    elif vcs.ismarker(prim):
        atts = ["x", "y", "color", "size", "type"]
    elif vcs.isline(prim):
        atts = ["x", "y", "color", "width", "type"]
    n = 0
    for a in atts:
        n = max(n, len(getattr(prim, a)))
    for a in atts:
        v = getattr(prim, a)
        while len(v) < n:
            v.append(v[-1])
        setattr(prim, a, v)

    # Handle fillarea opacity case, where the default will depend on the style
    if vcs.isfillarea(prim):
        o = getattr(prim, "opacity")
        s = getattr(prim, "style")
        assert(len(s) == n)
        if not o:
            o = []
        while len(o) < n:
            lastind = len(o) - 1
            if s[lastind] == "pattern":
                o.append(0)
            else:
                o.append(100)
    return n


def __build_pd__():
    pts = vtk.vtkPoints()
    polygons = vtk.vtkCellArray()
    polygonPolyData = vtk.vtkPolyData()
    polygonPolyData.SetPoints(pts)
    polygonPolyData.SetPolys(polygons)
    return pts, polygons, polygonPolyData


def prepFillarea(renWin, farea, cmap=None):
    n = prepPrimitive(farea)
    if n == 0:
        return []
    actors = []

    # Find color map:
    if farea.colormap is not None:
        cmap = farea.colormap
    elif cmap is None:
        cmap = vcs._colorMap
    if isinstance(cmap, str):
        cmap = vcs.elements["colormap"][cmap]

    # Create data structures
    pts, polygons, polygonPolyData = __build_pd__()
    colors = vtk.vtkUnsignedCharArray()
    colors.SetNumberOfComponents(4)
    colors.SetNumberOfTuples(n)
    polygonPolyData.GetCellData().SetScalars(colors)

    # Iterate through polygons:
    for i in range(n):
        x = farea.x[i]
        y = farea.y[i]
        st = farea.style[i]
        if st == "pattern":
            c = 241
        else:
            c = farea.color[i]

        if st == "solid":
            points, polys, pd, color_arr = pts, polygons, polygonPolyData, colors
        else:
            points, polys, pd = __build_pd__()
            color_arr = vtk.vtkUnsignedCharArray()
            color_arr.SetNumberOfComponents(4)
            color_arr.SetNumberOfTuples(1)
            colors.SetNumberOfTuples(colors.GetNumberOfTuples() - 1)
            pd.GetCellData().SetScalars(color_arr)

        idx = farea.index[i]
        N = max(len(x), len(y))

        for a in [x, y]:
            assert(len(a) == N)

        polygon = vtk.vtkPolygon()
        # Add current polygon
        pid = polygon.GetPointIds()
        pid.SetNumberOfIds(N)

        for j in range(N):
            pid.SetId(j, points.InsertNextPoint(x[j], y[j], 0.))
        cellId = polys.InsertNextCell(polygon)

        if isinstance(c, int):
            color = [C for C in cmap.index[c]]
        else:
            color = [C for C in c]
        if len(farea.opacity) > i:
            opacity = farea.opacity[i]
            if opacity is not None:
                opacity = farea.opacity[i]
        else:
            opacity = None
        # Draw colored background for solid
        # transparent/white background for hatches/patterns
        if st == 'solid':
            # Add the color to the color array:
            if opacity is not None:
                color[-1] = opacity
            color = [int(C / 100. * 255) for C in color]
            colors.SetTupleValue(cellId, color)
        else:
            color_arr.SetTupleValue(cellId, [255, 255, 255, 0])

        if st != "solid":
            # Patterns/hatches support
            geo, proj_points = project(
                points, farea.projection, farea.worldcoordinate)
            pd.SetPoints(proj_points)
            act = fillareautils.make_patterned_polydata(pd,
                                                        st,
                                                        idx,
                                                        color,
                                                        opacity,
                                                        renWin.GetSize())
            if act is not None:
                if (st == "pattern" and opacity > 0) or st == "hatch":
                    m = vtk.vtkPolyDataMapper()
                    m.SetInputData(pd)
                    a = vtk.vtkActor()
                    a.SetMapper(m)
                    actors.append((a, geo))
                actors.append((act, geo))

    # Transform points
    geo, pts = project(pts, farea.projection, farea.worldcoordinate)
    polygonPolyData.SetPoints(pts)
    # Setup rendering
    m = vtk.vtkPolyDataMapper()
    m.SetInputData(polygonPolyData)
    a = vtk.vtkActor()
    a.SetMapper(m)
    actors.append((a, geo))

    return actors


def genPoly(coords, pts, filled=True):
    N = pts.GetNumberOfPoints()
    if filled:
        poly = vtk.vtkPolygon()
    else:
        poly = vtk.vtkPolyLine()
    pid = poly.GetPointIds()
    n = len(coords)
    pid.SetNumberOfIds(n)
    for j in range(n):
        c = list(coords[j])
        if len(c) == 2:
            c.append(0)
        pts.InsertNextPoint(*c)
        pid.SetId(j, j + N)
    return poly


def prepGlyph(g, marker, index=0):
    t, s = marker.type[index], marker.size[index] * .5
    gs = vtk.vtkGlyphSource2D()
    pd = None

    if t == 'dot':
        gs.SetGlyphTypeToCircle()
        gs.FilledOn()
        s *= numpy.pi
    elif t == 'circle':
        gs.SetGlyphTypeToCircle()
        gs.FilledOff()
    elif t == 'plus':
        gs.SetGlyphTypeToCross()
        gs.FilledOff()
    elif t == 'cross':
        gs.SetGlyphTypeToCross()
        gs.SetRotationAngle(45)
        gs.FilledOff()
    elif t[:6] == 'square':
        gs.SetGlyphTypeToSquare()
        gs.FilledOff()
    elif t[:7] == 'diamond':
        gs.SetGlyphTypeToDiamond()
        gs.FilledOff()
    elif t[:8] == 'triangle':
        gs.SetGlyphTypeToTriangle()
        gs.FilledOff()
        if t[9] == "d":
            gs.SetRotationAngle(180)
        elif t[9] == "l":
            gs.SetRotationAngle(90)
        elif t[9] == "r":
            gs.SetRotationAngle(-90)
        elif t[9] == "u":
            gs.SetRotationAngle(0)
    elif t == "hurricane":
        s = s / 5.
        ds = vtk.vtkDiskSource()
        ds.SetInnerRadius(.55 * s)
        ds.SetOuterRadius(1.01 * s)
        ds.SetCircumferentialResolution(90)
        ds.SetRadialResolution(30)
        gf = vtk.vtkGeometryFilter()
        gf.SetInputConnection(ds.GetOutputPort())
        gf.Update()
        pd1 = gf.GetOutput()
        apd = vtk.vtkAppendPolyData()
        apd.AddInputData(pd1)
        pts = vtk.vtkPoints()
        pd = vtk.vtkPolyData()
        polygons = vtk.vtkCellArray()
        add_angle = numpy.pi / 360.
        coords = []
        angle1 = .6 * numpy.pi
        angle2 = .88 * numpy.pi
        while angle1 <= angle2:
            coords.append(
                [s * 2 + 2 * s * numpy.cos(angle1), 2 * s * numpy.sin(angle1)])
            angle1 += add_angle
        angle1 = .79 * numpy.pi
        angle2 = .6 * numpy.pi
        while angle1 >= angle2:
            coords.append([s *
                           2.25 +
                           s *
                           4 *
                           numpy.cos(angle1), -
                           s *
                           2 +
                           s *
                           4 *
                           numpy.sin(angle1)])
            angle1 -= add_angle
        poly = genPoly(coords, pts, filled=True)
        polygons.InsertNextCell(poly)
        coords = []
        angle1 = 1.6 * numpy.pi
        angle2 = 1.9 * numpy.pi
        while angle1 <= angle2:
            coords.append([-
                           s *
                           2 +
                           s *
                           2 *
                           numpy.cos(angle1), s *
                           2 *
                           numpy.sin(angle1)])
            angle1 += add_angle
        angle1 = 1.8 * numpy.pi
        angle2 = 1.6 * numpy.pi
        while angle1 >= angle2:
            coords.append([-
                           s *
                           2.27 +
                           s *
                           4 *
                           numpy.cos(angle1), s *
                           2 +
                           s *
                           4 *
                           numpy.sin(angle1)])
            angle1 -= add_angle
        poly = genPoly(coords, pts, filled=True)
        polygons.InsertNextCell(poly)
        pd.SetPoints(pts)
        pd.SetPolys(polygons)
        apd.AddInputData(pd)
        apd.Update()
        g.SetSourceData(apd.GetOutput())
    elif t[:4] == "star":
        np = 5
        points = starPoints(.001 * s, 0, 0, np)

        pts = vtk.vtkPoints()
        # Add all perimeter points
        for point in points:
            pts.InsertNextPoint((point[0], point[1], 0))

        center_id = len(points)

        # Add center point
        pts.InsertNextPoint((0, 0, 0))

        polygons = vtk.vtkCellArray()
        for ind in range(0, np * 2, 2):
            poly = vtk.vtkPolygon()
            pid = poly.GetPointIds()
            pid.SetNumberOfIds(4)
            pid.SetId(0, ind)
            pid.SetId(1, (ind - 1) % len(points))
            pid.SetId(2, center_id)
            pid.SetId(3, (ind + 1) % len(points))
            polygons.InsertNextCell(poly)

        pd = vtk.vtkPolyData()

        pd.SetPoints(pts)
        pd.SetPolys(polygons)

        g.SetSourceData(pd)
    elif t in ["w%.2i" % x for x in range(203)]:
        # WMO marker
        params = wmo[t]
        pts = vtk.vtkPoints()
        pd = vtk.vtkPolyData()
        polys = vtk.vtkCellArray()
        lines = vtk.vtkCellArray()
        s *= 3
        # Lines first
        for l in params["line"]:
            coords = numpy.array(zip(*l)) * s / 30.
            line = genPoly(coords.tolist(), pts, filled=False)
            lines.InsertNextCell(line)
        for l in params["poly"]:
            coords = numpy.array(zip(*l)) * s / 30.
            line = genPoly(coords.tolist(), pts, filled=True)
            polys.InsertNextCell(line)
        geo, pts = project(pts, marker.projection, marker.worldcoordinate)
        pd.SetPoints(pts)
        pd.SetPolys(polys)
        pd.SetLines(lines)
        g.SetSourceData(pd)
    else:
        warnings.warn("unknown marker type: %s, using dot" % t)
        gs.SetGlyphTypeToCircle()
        gs.FilledOn()
    if t[-5:] == "_fill":
        gs.FilledOn()

    if pd is None:
        # Use the difference in x to scale the point, as later we'll use the
        # x range to correct the aspect ratio:
        dx = marker.worldcoordinate[1] - marker.worldcoordinate[0]
        s *= abs(float(dx)) / 500.
        gs.SetScale(s)
        gs.Update()
        g.SetSourceConnection(gs.GetOutputPort())
    return gs, pd


def setMarkerColor(p, marker, c, cmap=None):
    # Color
    if marker.colormap is not None:
        cmap = marker.colormap
    elif cmap is None:
        cmap = vcs._colorMap
    if isinstance(cmap, str):
        cmap = vcs.elements["colormap"][cmap]
    if isinstance(c, int):
        color = cmap.index[c]
    else:
        color = c
    p.SetColor([C / 100. for C in color[:3]])
    p.SetOpacity(color[-1])


def scaleMarkerGlyph(g, gs, pd, a):
    # Invert the scale of the actor's transform.
    glyphTransform = vtk.vtkTransform()
    scale = a.GetUserTransform().GetScale()
    xComp = scale[0]
    scale = [xComp / float(val) for val in scale]
    glyphTransform.Scale(scale)

    glyphFixer = vtk.vtkTransformPolyDataFilter()
    glyphFixer.SetTransform(glyphTransform)

    if pd is None:
        glyphFixer.SetInputConnection(gs.GetOutputPort())
    else:
        glyphFixer.SetInputData(pd)
        g.SetSourceData(None)
    g.SetSourceConnection(glyphFixer.GetOutputPort())


def prepMarker(renWin, marker, cmap=None):
    n = prepPrimitive(marker)
    if n == 0:
        return []
    actors = []
    for i in range(n):
        g = vtk.vtkGlyph2D()
        markers = vtk.vtkPolyData()
        x = marker.x[i]
        y = marker.y[i]
        c = marker.color[i]
        N = max(len(x), len(y))
        for a in [x, y]:
            while len(a) < n:
                a.append(a[-1])
        pts = vtk.vtkPoints()
        for j in range(N):
            pts.InsertNextPoint(x[j], y[j], 0.)
        geo, pts = project(pts, marker.projection, marker.worldcoordinate)
        markers.SetPoints(pts)

        #  Type
        # Ok at this point generates the source for glpyh
        gs, pd = prepGlyph(g, marker, index=i)
        g.SetInputData(markers)

        a = vtk.vtkActor()
        m = vtk.vtkPolyDataMapper()
        m.SetInputConnection(g.GetOutputPort())
        m.Update()
        a.SetMapper(m)
        p = a.GetProperty()
        setMarkerColor(p, marker, c, cmap)
        actors.append((g, gs, pd, a, geo))

    return actors


def __build_ld__():
    pts = vtk.vtkPoints()
    lines = vtk.vtkCellArray()
    linePolyData = vtk.vtkPolyData()
    linePolyData.SetPoints(pts)
    linePolyData.SetLines(lines)
    colors = vtk.vtkUnsignedCharArray()
    colors.SetNumberOfComponents(4)
    colors.SetName("Colors")
    return pts, lines, linePolyData, colors


def stippleLine(prop, line_type):
    if line_type == 'long-dash':
        prop.SetLineStipplePattern(int('0000111111111111', 2))
        prop.SetLineStippleRepeatFactor(1)
    elif line_type == 'dot':
        prop.SetLineStipplePattern(int('0101010101010101', 2))
        prop.SetLineStippleRepeatFactor(1)
    elif line_type == 'dash':
        prop.SetLineStipplePattern(int('0001111100011111', 2))
        prop.SetLineStippleRepeatFactor(1)
    elif line_type == 'dash-dot':
        prop.SetLineStipplePattern(int('0101111101011111', 2))
        prop.SetLineStippleRepeatFactor(1)
    elif line_type == 'solid':
        prop.SetLineStipplePattern(int('1111111111111111', 2))
        prop.SetLineStippleRepeatFactor(1)
    else:
        raise Exception("Unknown line type: '%s'" % line_type)


def prepLine(renWin, line, cmap=None):
    number_lines = prepPrimitive(line)
    if number_lines == 0:
        return []

    actors = []

    line_data = {}

    if line.colormap is not None:
        cmap = line.colormap
    elif cmap is None:
        cmap = vcs._colorMap

    if isinstance(cmap, str):
        cmap = vcs.elements["colormap"][cmap]

    for i in range(number_lines):

        x = line.x[i]
        y = line.y[i]
        if isinstance(line.color[i], int):
            c = cmap.index[line.color[i]]
        else:
            c = line.color[i]
        w = line.width[i]
        t = line.type[i]

        if (t, w) not in line_data:
            line_data[(t, w)] = __build_ld__()
        pts, lines, linesPoly, colors = line_data[(t, w)]
        vtk_color = [int(component / 100. * 255) for component in c]

        number_points = max(len(x), len(y))

        point_offset = pts.GetNumberOfPoints()
        # Extend x or y to the length of the other by duplicating the last
        # coord.
        for a in [x, y]:
            while len(a) < number_points:
                a.append(a[-1])

        if vcs.elements["projection"][line.projection].type == "linear":
            for j in range(number_points):
                pts.InsertNextPoint(x[j], y[j], 0.)
            n2 = number_points - 1
        else:
            pts.InsertNextPoint(x[0], y[0], 0.)
            n2 = 0
            for j in range(1, number_points):
                if vcs.elements["projection"][
                        line.projection].type in round_projections:
                    NPointsInterp = 50
                else:
                    NPointsInterp = 25
                for i in range(1, NPointsInterp + 1):
                    if x[j] != x[j - 1]:
                        tmpx = x[j - 1] + \
                            float(i) / NPointsInterp * (x[j] - x[j - 1])
                    else:
                        tmpx = x[j]
                    if y[j] != y[j - 1]:
                        tmpy = y[j - 1] + \
                            float(i) / NPointsInterp * (y[j] - y[j - 1])
                    else:
                        tmpy = y[j]
                    pts.InsertNextPoint(tmpx, tmpy, 0.)
                    n2 += 1
        for j in range(n2):
            colors.InsertNextTupleValue(vtk_color)
            l = vtk.vtkLine()
            l.GetPointIds().SetId(0, j + point_offset)
            l.GetPointIds().SetId(1, j + point_offset + 1)
            lines.InsertNextCell(l)

    for t, w in line_data:
        pts, _, linesPoly, colors = line_data[(t, w)]

        linesPoly.GetCellData().SetScalars(colors)
        geo, pts = project(pts, line.projection, line.worldcoordinate)
        linesPoly.SetPoints(pts)

        a = vtk.vtkActor()
        m = vtk.vtkPolyDataMapper()
        m.SetInputData(linesPoly)
        a.SetMapper(m)

        p = a.GetProperty()
        p.SetLineWidth(w)

        stippleLine(p, t)
        actors.append((a, geo))

    return actors


def getRendererCorners(Renderer, vp=[0., 1., 0., 1.]):
    sz = Renderer.GetSize()
    origin = Renderer.GetOrigin()
    opposite = origin[0] + sz[0] * vp[1], origin[1] + sz[1] * vp[3]
    origin2 = origin[0] + sz[0] * vp[0], origin[1] + sz[1] * vp[2]
    return origin2, opposite


def world2Renderer(ren, x, y, vp=[0., 1., 0., 1.], wc=[0., 1., 0., 1.]):
    origin, opposite = getRendererCorners(ren, vp)
    X = origin[0] + (opposite[0] - origin[0]) * (x - wc[0]) / (wc[1] - wc[0])
    Y = origin[1] + (opposite[1] - origin[1]) * (y - wc[2]) / (wc[3] - wc[2])
    return X, Y


def vtkWorld2Renderer(ren, x, y):
    ren.SetWorldPoint(x, y, 0, 0)
    ren.WorldToDisplay()
    renpts = ren.GetDisplayPoint()
    return renpts

p = vtk.vtkGeoProjection()
vtkProjections = [
    p.GetProjectionName(i) for i in range(
        p.GetNumberOfProjections())]


def checkProjType(self, name, value):
    if value in vtkProjections:
        warnings.warn(
            "%s is a VTK backend specific projection, it might "
            "not work if you are not using the VTK backend" %
            value)
        return 200 + vtkProjections.index(value)
    raise Exception("%s is not a known VTK projection" % value)


def checkProjParameters(self, name, value):
    if not isinstance(value, dict):
        raise ValueError(
            "VTK specific projections parameters"
            "attribute needs to be a dictionary")
    return value


def getProjType(value):
    return vtkProjections[value - 200]


def starPoints(radius_outer, x, y, number_points=5):
    """Defines coordinate points for an arbitrary star"""
    # Point at the top of the star
    theta = numpy.pi / 2.0

    # Angular distance from one point to the next
    delta_theta = 2 * numpy.pi / float(number_points)

    # Size of the interior circle
    radius_inner = radius_outer / 2.0

    points = []
    for point_index in range(number_points * 2):
        # Outer point
        dx = radius_outer * numpy.cos(theta)
        dy = radius_outer * numpy.sin(theta)

        points.append((x + dx, y + dy))

        # Inner point
        dx = radius_inner * numpy.cos(theta + delta_theta / 2.0)
        dy = radius_inner * numpy.sin(theta + delta_theta / 2.0)
        points.append((x + dx, y + dy))

        theta += delta_theta
    return points


def generateVectorArray(data1, data2, vtk_grid):
    u = numpy.ma.ravel(data1)
    v = numpy.ma.ravel(data2)
    sh = list(u.shape)
    sh.append(1)
    u = numpy.reshape(u, sh)
    v = numpy.reshape(v, sh)
    z = numpy.zeros(u.shape)
    w = numpy.concatenate((u, v), axis=1)
    w = numpy.concatenate((w, z), axis=1)

    w = numpy_to_vtk_wrapper(w, deep=False)
    w.SetName("vector")
    return w


def vtkIterate(iterator):
    iterator.InitTraversal()
    obj = iterator.GetNextItem()
    while obj is not None:
        yield obj
        obj = iterator.GetNextItem()


def getPlottingBounds(gmbounds, databounds, geo):
    """
    Returns databounds for geographic projection
    else returns gmbounds if gmbounds are different than 1.e20
    """
    x1gm, x2gm, y1gm, y2gm = gmbounds[:4]
    x1, x2, y1, y2 = databounds[:4]
    if geo:
        return [x1, x2, y1, y2]
    assert (x1 < x2 and y1 < y2)
    if not numpy.allclose([x1gm, x2gm], 1.e20):
        x1, x2 = [x1gm, x2gm]
    if (isinstance(y1gm, numbers.Number) and not numpy.allclose([y1gm, y2gm], 1.e20)):
        y1, y2 = [y1gm, y2gm]
    return [x1, x2, y1, y2]


def switchAndTranslate(gm1, gm2, v1, v2, wrapModulo):
    """
    Transforms [v1,v2] and returns it
    such that it is in the same order
    and has the same middle interval as [gm1, gm2]
    """
    assert(v1 < v2)
    # keep the same middle of the interval
    if (wrapModulo):
        gmMiddle = float(gm1 + gm2) / 2.0
        half = float(v2 - v1) / 2.0
        v1 = gmMiddle - half
        v2 = gmMiddle + half
    # if gm margins are increasing and dataset bounds are decreasing
    # or the other way around switch them
    if ((gm1 - gm2) * (v1 - v2) < 0):
        v1, v2 = v2, v1
    return [v1, v2]


def getWrappedBounds(gmbounds, databounds, wrapModulo):
    """
    Returns bounds with the same interval size as databounds
    but in the same order and with the same middle interval
    as gmbounds. The middle and the order are used for
    plotting. wrapModule has YWrap, XWrap in degrees, 0 means no wrap
    """
    x1gm, x2gm, y1gm, y2gm = gmbounds[:4]
    x1, x2, y1, y2 = databounds[:4]
    assert (x1 < x2 and y1 < y2)
    if not numpy.allclose([x1gm, x2gm], 1.e20):
        x1, x2 = switchAndTranslate(x1gm, x2gm, x1, x2, wrapModulo[1] if wrapModulo else None)
    if (isinstance(y1gm, numbers.Number) and not numpy.allclose([y1gm, y2gm], 1.e20)):
        y1, y2 = switchAndTranslate(y1gm, y2gm, y1, y2, wrapModulo[0] if wrapModulo else None)
    return [x1, x2, y1, y2]
