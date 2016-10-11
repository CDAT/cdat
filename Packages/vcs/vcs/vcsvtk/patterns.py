import vtk


class Pattern(object):
    def __init__(self, patternPolyData, xres, yres, colors, style, opacity):
        self.patternPolyData = patternPolyData
        self.size = [xres, yres]
        self.style = style
        self.colors = colors
        self.opacity = opacity
        self.glyph = None

    def render(self):
        """
        Glyphs the input polydata points with the requested shape and
        replaces the input polydata with glyphed output polydata with
        colored cells
        """
        bounds = self.patternPolyData.GetBounds()
        xb = bounds[1] - bounds[0]
        yb = bounds[3] - bounds[2]
        xscale = xb / self.size[0]
        yscale = yb / self.size[1]
        self.scale = (xscale + yscale) / 2.0
        self.glyph = vtk.vtkGlyphSource2D()
        self.glyph.SetGlyphTypeToSquare()
        self.glyph.SetScale(self.scale)
        self.glyph.FilledOff()
        self.glyph.DashOff()
        self.glyph.CrossOff()

        self.paint()

        self.glyph2D = vtk.vtkGlyph2D()
        self.glyph2D.ScalingOff()
        self.glyph2D.SetInputData(self.patternPolyData)
        self.glyph2D.SetSourceConnection(self.glyph.GetOutputPort())
        self.glyph2D.Update()
        self.patternPolyData.DeepCopy(self.glyph2D.GetOutput())

        self.map_colors()

    def paint(self):
        raise NotImplementedError(
            "paint() not implemented for %s" % str(
                type(self)))

    def color_tuple(self):
        """
        Returns a 4 component color tuple (RGBA)
        """
        if self.style != "hatch":
            color = [0, 0, 0]
        else:
            color = [int(c / 100. * 255) for c in self.colors[:3]]
        if self.style in ["hatch", "pattern"]:
            opacity = int(self.opacity / 100. * 255)
        else:
            opacity = 255
        color.append(opacity)
        return color

    def map_colors(self):
        colors = vtk.vtkUnsignedCharArray()
        colors.SetNumberOfComponents(4)
        colors.SetName("Colors")
        self.patternPolyData.GetCellData().SetScalars(colors)
        for i in range(self.patternPolyData.GetNumberOfCells()):
            colors.InsertNextTypedTuple(self.color_tuple())


class Triangle(Pattern):

    def paint(self):
        self.glyph.SetGlyphTypeToTriangle()


class FilledTriangle(Triangle):

    def paint(self):
        Triangle.paint(self)
        self.glyph.FilledOn()


class Dot(Pattern):

    def paint(self):
        self.glyph.SetGlyphTypeToCircle()


class FilledDot(Dot):

    def paint(self):
        Dot.paint(self)
        self.glyph.FilledOn()


class HorizStripe(Pattern):

    def paint(self):
        self.glyph.SetGlyphTypeToDash()
        self.glyph.FilledOn()
        self.glyph.SetScale(self.scale * 1.5)


class VertStripe(HorizStripe):

    def paint(self):
        HorizStripe.paint(self)
        self.glyph.SetRotationAngle(90)


class DiagStripe(HorizStripe):

    def paint(self):
        HorizStripe.paint(self)
        self.glyph.SetRotationAngle(45)


class ReverseDiagStripe(DiagStripe):

    def paint(self):
        DiagStripe.paint(self)
        self.glyph.SetRotationAngle(-45)


class HorizDash(Pattern):

    def paint(self):
        self.glyph.SetGlyphTypeToDash()
        self.glyph.FilledOn()
        self.glyph.SetScale(self.scale * 3.0 / 4.0)


class VertDash(HorizDash):

    def paint(self):
        HorizDash.paint(self)
        self.glyph.SetRotationAngle(90)


class Cross(Pattern):

    def paint(self):
        self.glyph.SetGlyphTypeToThickCross()
        self.glyph.SetScale(self.scale * 3.0 / 4.0)


class FilledCross(Cross):

    def paint(self):
        Cross.paint(self)
        self.glyph.FilledOn()


class XCross(Cross):

    def paint(self):
        Cross.paint(self)
        self.glyph.SetScale(self.scale * 2.0)
        self.glyph.SetRotationAngle(45.0)


class Diamond(Pattern):

    def paint(self):
        self.glyph.SetGlyphTypeToDiamond()


class FilledDiamond(Diamond):

    def paint(self):
        Diamond.paint(self)
        self.glyph.FilledOn()


class Square(Pattern):

    def paint(self):
        self.glyph.SetGlyphTypeToSquare()
        self.glyph.SetScale(self.scale * 3.0 / 4.0)


class FilledSquare(Square):

    def paint(self):
        Square.paint(self)
        self.glyph.FilledOn()


class Arrow(Pattern):

    def paint(self):
        self.glyph.SetGlyphTypeToThickArrow()
        self.glyph.SetScale(self.scale * 3.0 / 4.0)


class CircleCross(Pattern):

    def paint(self):
        self.glyph.SetGlyphTypeToCircle()
        self.glyph.SetScale(self.scale * 0.5)
        self.glyph.SetScale2(2.5)
        self.glyph.CrossOn()


class EdgeArrow(Pattern):

    def paint(self):
        self.glyph.SetGlyphTypeToEdgeArrow()
        self.glyph.SetScale(self.scale * 0.75)


# Patterns are 1-indexed, so we always skip the 0th element in this list
pattern_list = [Pattern, Triangle, FilledTriangle, Dot, FilledDot,
                HorizStripe, VertStripe, HorizDash, VertDash,
                DiagStripe, ReverseDiagStripe,
                Cross, FilledCross, XCross, Diamond, FilledDiamond,
                Square, FilledSquare, Arrow, CircleCross, EdgeArrow]
