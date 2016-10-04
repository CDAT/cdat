import vtk


class Pattern(object):
    def __init__(self, patternPolyData, xres, yres, colors, style, opacity):
        self.patternPolyData = patternPolyData
        self.size = [xres, yres]
        self.colors = [int(c / 100. * 255) for c in colors]
        self.style = style
        if self.style != "hatch":
            self.colors = [0, 0, 0]
        if self.style in ["hatch", "pattern"]:
            self.opacity = int(opacity / 100. * 255)
        else:
            self.opacity = 255
        self.rgba = self.colorTuple()

    def render(self):
        """
        Returns vtkImageData for pattern
        """
        cells = vtk.vtkCellArray()
        self.patternPolyData.SetPolys(cells)
        colors = vtk.vtkUnsignedCharArray()
        colors.SetNumberOfComponents(4)
        colors.SetName("Colors")
        self.patternPolyData.GetCellData().SetScalars(colors)
        self.paint()

    def paint(self):
        raise NotImplementedError(
            "paint() not implemented for %s" % str(
                type(self)))

    def colorTuple(self):
        """
        Returns a 4 component color tuple (RGBA)
        """
        color = self.colors[0:3]
        color.append(self.opacity)
        return color


class BottomLeftTri(Pattern):

    def paint(self):
        if self.patternPolyData is None:
            return None
        cells = self.patternPolyData.GetPolys()
        cells.Allocate(cells.EstimateSize(self.size[0] * self.size[1], 3))
        colors = self.patternPolyData.GetCellData().GetScalars("Colors")
        cell = [0, 0, 0]
        for i in range(0, self.size[1], 2):
            for j in range(0, self.size[0], 2):
                cell[0] = j + i * (self.size[0] + 1)
                cell[1] = cell[0] + 1
                cell[2] = cell[0] + self.size[0] + 1
                cells.InsertNextCell(3, cell)
                colors.InsertNextTypedTuple(self.rgba)


class TopRightTri(Pattern):

    def paint(self):
        if self.patternPolyData is None:
            return None
        cells = self.patternPolyData.GetPolys()
        cells.Allocate(cells.EstimateSize(self.size[0] * self.size[1], 3))
        colors = self.patternPolyData.GetCellData().GetScalars("Colors")
        cell = [0, 0, 0]
        for i in range(0, self.size[1], 2):
            for j in range(0, self.size[0], 2):
                cell[0] = j + i * (self.size[0] + 1)
                cell[1] = cell[0] + self.size[0] + 1
                cell[2] = cell[1] + 1
                cells.InsertNextCell(3, cell)
                colors.InsertNextTypedTuple(self.rgba)


class SmallRectDot(Pattern):

    def paint(self):
        if self.patternPolyData is None:
            return None
        cells = self.patternPolyData.GetPolys()
        cells.Allocate(cells.EstimateSize(self.size[0] * self.size[1], 4))
        colors = self.patternPolyData.GetCellData().GetScalars("Colors")
        cell = [0, 0, 0, 0]
        for i in range(0, self.size[1], 2):
            for j in range(0, self.size[0], 2):
                cell[0] = j + i * (self.size[0] + 1)
                cell[1] = cell[0] + 1
                cell[2] = cell[0] + self.size[0] + 2
                cell[3] = cell[2] - 1
                cells.InsertNextCell(4, cell)
                colors.InsertNextTypedTuple(self.rgba)


class HorizStripe(Pattern):

    def paint(self):
        if self.patternPolyData is None:
            return None
        cells = self.patternPolyData.GetPolys()
        cells.Allocate(cells.EstimateSize(self.size[0] * self.size[1], 4))
        colors = self.patternPolyData.GetCellData().GetScalars("Colors")
        cell = [0, 0, 0, 0]
        for i in range(0, self.size[1], 2):
            for j in range(0, self.size[0], 1):
                cell[0] = j + i * (self.size[0] + 1)
                cell[1] = cell[0] + 1
                cell[2] = cell[1] + self.size[0] + 1
                cell[3] = cell[2] - 1
                cells.InsertNextCell(4, cell)
                colors.InsertNextTypedTuple(self.rgba)


class VertStripe(Pattern):

    def paint(self):
        if self.patternPolyData is None:
            return None
        cells = self.patternPolyData.GetPolys()
        cells.Allocate(cells.EstimateSize(self.size[0] * self.size[1], 4))
        colors = self.patternPolyData.GetCellData().GetScalars("Colors")
        cell = [0, 0, 0, 0]
        for i in range(0, self.size[1], 1):
            for j in range(0, self.size[0], 2):
                cell[0] = j + i * (self.size[0] + 1)
                cell[1] = cell[0] + 1
                cell[2] = cell[1] + self.size[0] + 1
                cell[3] = cell[2] - 1
                cells.InsertNextCell(4, cell)
                colors.InsertNextTypedTuple(self.rgba)


class HorizDash(Pattern):

    def paint(self):
        if self.patternPolyData is None:
            return None
        r = vtk.vtkRegularPolygonSource()
        g = vtk.vtkGlyph2D()
        g.SetInputData(self.patternPolyData)
        g.SetSourceConnection(r.GetOutputPort())
        g.Update()
        self.patternPolyData = g.GetOutput()
#        colors = self.patternPolyData.GetPointData().GetScalars("Colors")
#        if colors is None:
#            colors = vtk.vtkUnsignedCharArray()
#            colors.SetName("Colors")
#            colors.SetNumberOfComponents(4)
#            self.patternPolyData.GetPointData().SetScalars(colors)
#        for i in range(self.patternPolyData.GetNumberOfPoints()):
#            colors.InsertNextTypedTuple(self.rgba)

#        cells = self.patternPolyData.GetPolys()
#        cells.Allocate(cells.EstimateSize(self.size[0] * self.size[1], 4))
#        colors = self.patternPolyData.GetCellData().GetScalars("Colors")
#        cell = [0, 0, 0, 0]
#        for i in range(0, self.size[1], 2):
#            for j in range(0, self.size[0], 3):
#                cell[0] = j + i * (self.size[0] + 1)
#                cell[1] = cell[0] + 2
#                cell[2] = cell[1] + self.size[0] + 1
#                cell[3] = cell[2] - 2
#                cells.InsertNextCell(4, cell)
#                colors.InsertNextTypedTuple(self.rgba)


class VertDash(Pattern):

    def paint(self, patternSource):
        if patternSource is None:
            return None
        for x in xrange(0, self.width, self.num_pixels):
            for y in xrange(0, self.height, self.num_pixels):
                patternSource.FillBox(x + self.num_pixels / 4, x + self.num_pixels / 2,
                                      y, y + self.num_pixels * 3 / 4)
                patternSource.FillBox(x + self.num_pixels * 3 / 4, x + self.num_pixels,
                                      y, y + self.num_pixels * 3 / 4)


class XDash(Pattern):

    def render(self):
        """
        Returns vtkImageData for pattern
        """
        # XDash is drawn and then rotated (for the sake of performance)
        rot_square = int(((self.width + self.height) * 2 ** .5) / 2)

        o_width, o_height = self.width, self.height
        self.width, self.height = rot_square, rot_square

        image = super(XDash, self).render()

        slicer = vtk.vtkImageReslice()
        slicer.SetInputData(image)

        raw_bounds = image.GetBounds()
        raw_center = ((raw_bounds[0] + raw_bounds[1]) / 2.,
                      (raw_bounds[2] + raw_bounds[3]) / 2.,
                      (raw_bounds[4] + raw_bounds[5]) / 2.)

        transform = vtk.vtkTransform()
        transform.Translate(*raw_center)
        transform.RotateWXYZ(45., 0, 0, 1)
        transform.Translate(*[-1 * p for p in raw_center])

        slicer.SetResliceTransform(transform)
        slicer.SetInterpolationModeToCubic()
        slicer.SetOutputSpacing(image.GetSpacing())
        slicer.SetOutputExtent(0, o_width, 0, o_height, 0, 0)

        x_origin = (rot_square - o_width) / 2
        y_origin = (rot_square - o_height) / 2

        slicer.SetOutputOrigin(x_origin, y_origin, 0)
        slicer.Update()

        self.width, self.height = o_width, o_height
        return slicer.GetOutput()

    def paint(self, patternSource):
        if patternSource is None:
            return None

        thickness = self.num_pixels / 8
        for x in range(thickness, self.width, self.num_pixels):
            patternSource.FillBox(x - thickness, x + thickness, 0, self.height)

        for y in range(thickness, self.height, self.num_pixels):
            patternSource.FillBox(0, self.width, y - thickness, y + thickness)
            patternSource.SetDrawColor(255, 255, 255, 0)
            for x in range(thickness, self.width, self.num_pixels):
                patternSource.FillBox(x - thickness, x + thickness, y - thickness, y + thickness)
            patternSource.SetDrawColor(self.colors[0], self.colors[1], self.colors[2], self.opacity)


class ThinDiagDownRight(Pattern):

    def paint(self, patternSource):
        if patternSource is None:
            return None
        patternLevels = range(0, max(self.width, self.height) + min(self.width, self.height), self.num_pixels)
        for lev in patternLevels:
            patternSource.FillTube(0, lev, lev, 0, self.num_pixels / 8)


class ThickDiagRownRight(Pattern):

    def paint(self, patternSource):
        if patternSource is None:
            return None
        patternLevels = range(0, max(self.width, self.height) + min(self.width, self.height), self.num_pixels)
        for lev in patternLevels:
            patternSource.FillTube(0, lev, lev, 0, self.num_pixels / 4)


class ThinDiagUpRight(Pattern):

    def paint(self, patternSource):
        if patternSource is None:
            return None
        patternLevels = range(0, max(self.width, self.height) + min(self.width, self.height), self.num_pixels)
        for lev in patternLevels:
            patternSource.FillTube(lev, self.height, 0, self.height - lev, self.num_pixels / 8)


class ThickDiagUpRight(Pattern):

    def paint(self, patternSource):
        if patternSource is None:
            return None
        patternLevels = range(0, max(self.width, self.height) + min(self.width, self.height), self.num_pixels)
        for lev in patternLevels:
            patternSource.FillTube(lev, self.height, 0, self.height - lev, self.num_pixels / 4)


class ThickThinVertStripe(Pattern):

    def paint(self, patternSource):
        if patternSource is None:
            return None
        patternLevels = range(0, self.width, self.num_pixels)
        for lev in patternLevels:
            patternSource.FillBox(lev + self.num_pixels / 8, lev + self.num_pixels * 1 / 2, 0, self.height)
            patternSource.FillBox(lev + self.num_pixels * 3 / 4, lev + self.num_pixels * 7 / 8, 0, self.height)


class ThickThinHorizStripe(Pattern):

    def paint(self, patternSource):
        if patternSource is None:
            return None
        patternLevels = range(0, self.height, self.num_pixels)
        for lev in patternLevels:
            patternSource.FillBox(0, self.width, lev + self.num_pixels / 8, lev + self.num_pixels * 1 / 2)
            patternSource.FillBox(0, self.width, lev + self.num_pixels * 3 / 4, lev + self.num_pixels * 7 / 8)


class LargeRectDot(Pattern):

    def paint(self, patternSource):
        if patternSource is None:
            return None
        for x in xrange(0, self.width, self.num_pixels):
            for y in xrange(0, self.height, self.num_pixels):
                patternSource.FillBox(x + self.num_pixels * 1 / 8, x + self.num_pixels * 7 / 8,
                                      y + self.num_pixels * 1 / 8, y + self.num_pixels * 7 / 8)


class Diamond(Pattern):

    def paint(self, patternSource):
        if patternSource is None:
            return None
        for x in xrange(0, self.width, self.num_pixels):
            for y in xrange(0, self.height, self.num_pixels):
                patternSource.FillTriangle(x, y,
                                           x + self.num_pixels / 4, y,
                                           x, y + self.num_pixels / 4)
                patternSource.FillTriangle(x, y + self.num_pixels,
                                           x + self.num_pixels / 4, y + self.num_pixels,
                                           x, y + self.num_pixels * 3 / 4)
                patternSource.FillTriangle(x + self.num_pixels, y + self.num_pixels,
                                           x + self.num_pixels * 3 / 4, y + self.num_pixels,
                                           x + self.num_pixels, y + self.num_pixels * 3 / 4)
                patternSource.FillTriangle(x + self.num_pixels, y,
                                           x + self.num_pixels * 3 / 4, y,
                                           x + self.num_pixels, y + self.num_pixels / 4)
                patternSource.FillTriangle(x, y + self.num_pixels / 2,
                                           x + self.num_pixels / 2, y + self.num_pixels,
                                           x + self.num_pixels / 2, y)
                patternSource.FillTriangle(x + self.num_pixels, y + self.num_pixels / 2,
                                           x + self.num_pixels / 2, y + self.num_pixels,
                                           x + self.num_pixels / 2, y)


class Bubble(Pattern):

    def paint(self, patternSource):
        if patternSource is None:
            return None
        for x in xrange(0, self.width, self.num_pixels):
            for y in xrange(0, self.height, self.num_pixels):
                patternSource.FillBox(x, x + self.num_pixels / 8,
                                      y, y + self.num_pixels / 8)
                patternSource.FillBox(x + self.num_pixels / 2, x + self.num_pixels * 5 / 8,
                                      y, y + self.num_pixels / 8)
                patternSource.FillBox(x, x + self.num_pixels / 8,
                                      y + self.num_pixels / 2, y + self.num_pixels * 5 / 8)
                patternSource.FillBox(x + self.num_pixels / 8, x + self.num_pixels / 2,
                                      y + self.num_pixels / 8, y + self.num_pixels / 2)
                patternSource.FillBox(x + self.num_pixels * 5 / 8, x + self.num_pixels,
                                      y + self.num_pixels * 5 / 8, y + self.num_pixels)
                patternSource.FillBox(x + self.num_pixels / 8, x + self.num_pixels * 3 / 8,
                                      y + self.num_pixels * 5 / 8, y + self.num_pixels * 3/4)
                patternSource.FillBox(x + self.num_pixels * 5 / 8, x + self.num_pixels * 7 / 8,
                                      y + self.num_pixels / 8, y + self.num_pixels * 1 / 4)


class Snake(Pattern):

    def paint(self, patternSource):
        if patternSource is None:
            return None
        for x in xrange(0, self.width, self.num_pixels):
            for y in xrange(0, self.height, self.num_pixels):
                patternSource.FillBox(x, x + self.num_pixels / 3,
                                      y, y + self.num_pixels / 3)
                patternSource.FillBox(x + self.num_pixels * 2 / 3, x + self.num_pixels,
                                      y, y + self.num_pixels / 3)
                patternSource.FillBox(x + self.num_pixels / 6, x + self.num_pixels * 5 / 6,
                                      y + self.num_pixels / 3, y + self.num_pixels * 2 / 3)


class EmptyCircle(Pattern):

    def paint(self, patternSource):
        if patternSource is None:
            return None
        for x in xrange(0, self.width, self.num_pixels):
            for y in xrange(0, self.height, self.num_pixels):
                # Draw 1/3 or the radius as border by making several smaller circles
                for r in range(self.num_pixels / 8):
                    patternSource.DrawCircle(x + self.num_pixels / 2, y + self.num_pixels / 2, self.num_pixels / 4. - r)


# Patterns are 1-indexed, so we always skip the 0th element in this list
pattern_list = [Pattern, BottomLeftTri, TopRightTri, SmallRectDot,
                HorizStripe, VertStripe, HorizDash, VertDash, XDash, ThinDiagDownRight,
                ThickDiagRownRight, ThinDiagUpRight, ThickDiagUpRight, ThickThinVertStripe,
                ThickThinHorizStripe, LargeRectDot, Diamond, Bubble, Snake, EmptyCircle]
