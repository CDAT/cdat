import vtk

# number of pixels per individual pattern block
NUM_PIXELS = 16


def make_patterned_polydata(inputContours, fillareastyle=None,
                            fillareaindex=None, fillareacolors=None,
                            fillareaopacity=None):
    if inputContours is None or fillareastyle == 'solid':
        return None
    if inputContours.GetNumberOfCells() == 0:
        return None
    if fillareaindex is None:
        fillareaindex = 1
    if fillareaopacity is None:
        fillareaopacity = 255

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
    if xBounds < 1 or yBounds < 1:
        boundsAspect = xBounds / yBounds
        global NUM_PIXELS
        if boundsAspect > 1.0:
            yres = 2 * NUM_PIXELS
            xres = int(boundsAspect * yres)
        else:
            xres = 2 * NUM_PIXELS
            yres = int(xres / boundsAspect)
    patternImage = create_pattern(xres, yres, fillareastyle,
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

    # Create the texture using the stenciled pattern
    patternTexture = vtk.vtkTexture()
    patternTexture.SetInputData(patternImage)
    patternTexture.InterpolateOn()
    mapper = vtk.vtkPolyDataMapper()
    mapper.SetInputConnection(textureMap.GetOutputPort())
    actor = vtk.vtkActor()
    actor.SetMapper(mapper)
    actor.SetTexture(patternTexture)
    return actor


def create_pattern(width, height, fillareastyle=None,
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
    pattern = patterns[fillareaindex](width, height, fillareacolors, fillareastyle, fillareaopacity)
    return pattern.render()


class Pattern(object):
    def __init__(self, width, height, colors, style, opacity):
        self.width = width
        self.height = height
        self.colors = colors
        self.style = style
        self.opacity = opacity

    def render(self):
        """
        Returns vtkImageData for pattern
        """
        patternSource = vtk.vtkImageCanvasSource2D()
        patternSource.SetScalarTypeToUnsignedChar()
        patternSource.SetExtent(0, self.width, 0, self.height, 0, 0)
        patternSource.SetNumberOfScalarComponents(4)
        patternSource.SetDrawColor(255, 255, 255, 0)
        patternSource.FillBox(0, self.width, 0, self.height)
        if self.style == 'hatch':
            patternSource.SetDrawColor(self.colors[0],
                                       self.colors[1],
                                       self.colors[2],
                                       self.opacity)
        else:
            patternSource.SetDrawColor(0, 0, 0, 255)

        self.draw(patternSource)
        patternSource.Update()
        return patternSource.GetOutput()

    def draw(self, pattern):
        raise NotImplementedError("draw() not implemented for %s" % str(type(self)))


class BottomLeftTri(Pattern):
    def draw(self, patternSource):
        if patternSource is None:
            return None
        global NUM_PIXELS
        for x in xrange(0, self.width, NUM_PIXELS):
            for y in xrange(0, self.height, NUM_PIXELS):
                patternSource.FillTriangle(x, y,
                                           x + NUM_PIXELS * 3 / 4, y,
                                           x, y + NUM_PIXELS * 3 / 4)


class TopRightTri(Pattern):
    def draw(self, patternSource):
        if patternSource is None:
            return None
        global NUM_PIXELS
        for x in xrange(0, self.width, NUM_PIXELS):
            for y in xrange(0, self.height, NUM_PIXELS):
                patternSource.FillTriangle(x + NUM_PIXELS, y + NUM_PIXELS,
                                           x + NUM_PIXELS / 4, y + NUM_PIXELS,
                                           x + NUM_PIXELS, y + NUM_PIXELS * 1 / 4)


class SmallRectDot(Pattern):
    def draw(self, patternSource):
        if patternSource is None:
            return None
        global NUM_PIXELS
        for x in xrange(0, self.width, NUM_PIXELS):
            for y in xrange(0, self.height, NUM_PIXELS):
                patternSource.FillBox(x + NUM_PIXELS * 3 / 8, x + NUM_PIXELS * 5 / 8,
                                      y + NUM_PIXELS * 3 / 8, y + NUM_PIXELS * 5 / 8)


class CheckerBoard(Pattern):
    def draw(self, patternSource):
        if patternSource is None:
            return None
        global NUM_PIXELS
        for x in xrange(0, self.width, NUM_PIXELS):
            for y in xrange(0, self.height, NUM_PIXELS):
                patternSource.FillBox(x, x + NUM_PIXELS / 2,
                                      y, y + NUM_PIXELS / 2)
                patternSource.FillBox(x + NUM_PIXELS / 2, x + NUM_PIXELS,
                                      y + NUM_PIXELS / 2, y + NUM_PIXELS)


class HorizStripe(Pattern):
    def draw(self, patternSource):
        if patternSource is None:
            return None
        global NUM_PIXELS
        patternLevels = range(0, self.height, NUM_PIXELS)
        for lev in patternLevels:
            patternSource.FillBox(0, self.width, lev + NUM_PIXELS / 4, lev + NUM_PIXELS * 3 / 4)


class VertStripe(Pattern):
    def draw(self, patternSource):
        if patternSource is None:
            return None
        global NUM_PIXELS
        patternLevels = range(0, self.width, NUM_PIXELS)
        for lev in patternLevels:
            patternSource.FillBox(lev + NUM_PIXELS / 4, lev + NUM_PIXELS * 3 / 4, 0, self.height)


class HorizDash(Pattern):
    def draw(self, patternSource):
        if patternSource is None:
            return None
        global NUM_PIXELS
        for x in xrange(0, self.width, NUM_PIXELS):
            for y in xrange(0, self.height, NUM_PIXELS):
                patternSource.FillBox(x, x + NUM_PIXELS * 3 / 4,
                                      y, y + NUM_PIXELS / 4)


class VertDash(Pattern):
    def draw(self, patternSource):
        if patternSource is None:
            return None
        global NUM_PIXELS
        for x in xrange(0, self.width, NUM_PIXELS):
            for y in xrange(0, self.height, NUM_PIXELS):
                patternSource.FillBox(x + NUM_PIXELS / 4, x + NUM_PIXELS / 2,
                                      y, y + NUM_PIXELS * 3 / 4)
                patternSource.FillBox(x + NUM_PIXELS * 3 / 4, x + NUM_PIXELS,
                                      y, y + NUM_PIXELS * 3 / 4)


class XDash(Pattern):
    def draw(self, patternSource):
        if patternSource is None:
            return None
        global NUM_PIXELS
        for x in xrange(0, self.width, NUM_PIXELS):
            for y in xrange(0, self.height, NUM_PIXELS):
                patternSource.FillTube(x + NUM_PIXELS / 8, y + NUM_PIXELS / 8,
                                       x + NUM_PIXELS * 3 / 8, y + NUM_PIXELS * 3 / 8,
                                       NUM_PIXELS / 8)
                patternSource.FillTube(x + NUM_PIXELS * 5 / 8, y + NUM_PIXELS * 5 / 8,
                                       x + NUM_PIXELS * 7 / 8, y + NUM_PIXELS * 7 / 8,
                                       NUM_PIXELS / 8)
                patternSource.FillTube(x + NUM_PIXELS / 8, y + NUM_PIXELS * 7 / 8,
                                       x + NUM_PIXELS * 3 / 8, y + NUM_PIXELS * 5 / 8,
                                       NUM_PIXELS / 8)
                patternSource.FillTube(x + NUM_PIXELS * 5 / 8, y + NUM_PIXELS * 3 / 8,
                                       x + NUM_PIXELS * 7 / 8, y + NUM_PIXELS / 8,
                                       NUM_PIXELS / 8)


class ThinDiagDownRight(Pattern):
    def draw(self, patternSource):
        if patternSource is None:
            return None
        global NUM_PIXELS
        patternLevels = range(0, max(self.width, self.height) + min(self.width, self.height), NUM_PIXELS)
        for lev in patternLevels:
            patternSource.FillTube(0, lev, lev, 0, NUM_PIXELS / 8)


class ThickDiagRownRight(Pattern):
    def draw(self, patternSource):
        if patternSource is None:
            return None
        global NUM_PIXELS
        patternLevels = range(0, max(self.width, self.height) + min(self.width, self.height), NUM_PIXELS)
        for lev in patternLevels:
            patternSource.FillTube(0, lev, lev, 0, NUM_PIXELS / 4)


class ThinDiagUpRight(Pattern):
    def draw(self, patternSource):
        if patternSource is None:
            return None
        global NUM_PIXELS
        patternLevels = range(0, max(self.width, self.height) + min(self.width, self.height), NUM_PIXELS)
        for lev in patternLevels:
            patternSource.FillTube(lev, self.height, 0, self.height - lev, NUM_PIXELS / 8)


class ThickDiagUpRight(Pattern):
    def draw(self, patternSource):
        if patternSource is None:
            return None
        global NUM_PIXELS
        patternLevels = range(0, max(self.width, self.height) + min(self.width, self.height), NUM_PIXELS)
        for lev in patternLevels:
            patternSource.FillTube(lev, self.height, 0, self.height - lev, NUM_PIXELS / 4)


class ThickThinVertStripe(Pattern):
    def draw(self, patternSource):
        if patternSource is None:
            return None
        global NUM_PIXELS
        patternLevels = range(0, self.width, NUM_PIXELS)
        for lev in patternLevels:
            patternSource.FillBox(lev + NUM_PIXELS / 8, lev + NUM_PIXELS * 1 / 2, 0, self.height)
            patternSource.FillBox(lev + NUM_PIXELS * 3 / 4, lev + NUM_PIXELS * 7 / 8, 0, self.height)


class ThickThinHorizStripe(Pattern):
    def draw(self, patternSource):
        if patternSource is None:
            return None
        global NUM_PIXELS
        patternLevels = range(0, self.height, NUM_PIXELS)
        for lev in patternLevels:
            patternSource.FillBox(0, self.width, lev + NUM_PIXELS / 8, lev + NUM_PIXELS * 1 / 2)
            patternSource.FillBox(0, self.width, lev + NUM_PIXELS * 3 / 4, lev + NUM_PIXELS * 7 / 8)


class LargeRectDot(Pattern):
    def draw(self, patternSource):
        if patternSource is None:
            return None
        global NUM_PIXELS
        for x in xrange(0, self.width, NUM_PIXELS):
            for y in xrange(0, self.height, NUM_PIXELS):
                patternSource.FillBox(x + NUM_PIXELS * 1 / 8, x + NUM_PIXELS * 7 / 8,
                                      y + NUM_PIXELS * 1 / 8, y + NUM_PIXELS * 7 / 8)


class Diamond(Pattern):
    def draw(self, patternSource):
        if patternSource is None:
            return None
        global NUM_PIXELS
        for x in xrange(0, self.width, NUM_PIXELS):
            for y in xrange(0, self.height, NUM_PIXELS):
                patternSource.FillTriangle(x, y,
                                           x + NUM_PIXELS / 4, y,
                                           x, y + NUM_PIXELS / 4)
                patternSource.FillTriangle(x, y + NUM_PIXELS,
                                           x + NUM_PIXELS / 4, y + NUM_PIXELS,
                                           x, y + NUM_PIXELS * 3 / 4)
                patternSource.FillTriangle(x + NUM_PIXELS, y + NUM_PIXELS,
                                           x + NUM_PIXELS * 3 / 4, y + NUM_PIXELS,
                                           x + NUM_PIXELS, y + NUM_PIXELS * 3 / 4)
                patternSource.FillTriangle(x + NUM_PIXELS, y,
                                           x + NUM_PIXELS * 3 / 4, y,
                                           x + NUM_PIXELS, y + NUM_PIXELS / 4)
                patternSource.FillTriangle(x, y + NUM_PIXELS / 2,
                                           x + NUM_PIXELS / 2, y + NUM_PIXELS,
                                           x + NUM_PIXELS / 2, y)
                patternSource.FillTriangle(x + NUM_PIXELS, y + NUM_PIXELS / 2,
                                           x + NUM_PIXELS / 2, y + NUM_PIXELS,
                                           x + NUM_PIXELS / 2, y)


class AbstractArt(Pattern):
    def draw(self, patternSource):
        if patternSource is None:
            return None
        global NUM_PIXELS
        for x in xrange(0, self.width, NUM_PIXELS):
            for y in xrange(0, self.height, NUM_PIXELS):
                patternSource.FillBox(x, x + NUM_PIXELS / 8,
                                      y, y + NUM_PIXELS / 8)
                patternSource.FillBox(x + NUM_PIXELS / 2, x + NUM_PIXELS * 5 / 8,
                                      y, y + NUM_PIXELS / 8)
                patternSource.FillBox(x, x + NUM_PIXELS / 8,
                                      y + NUM_PIXELS / 2, y + NUM_PIXELS * 5 / 8)
                patternSource.FillBox(x + NUM_PIXELS / 8, x + NUM_PIXELS / 2,
                                      y + NUM_PIXELS / 8, y + NUM_PIXELS / 2)
                patternSource.FillBox(x + NUM_PIXELS * 5 / 8, x + NUM_PIXELS,
                                      y + NUM_PIXELS * 5 / 8, y + NUM_PIXELS)
                patternSource.FillBox(x + NUM_PIXELS / 8, x + NUM_PIXELS * 3 / 8,
                                      y + NUM_PIXELS * 5 / 8, y + NUM_PIXELS * 3/4)
                patternSource.FillBox(x + NUM_PIXELS * 5 / 8, x + NUM_PIXELS * 7 / 8,
                                      y + NUM_PIXELS / 8, y + NUM_PIXELS * 1 / 4)


class Snake(Pattern):
    def draw(self, patternSource):
        if patternSource is None:
            return None
        global NUM_PIXELS
        for x in xrange(0, self.width, NUM_PIXELS):
            for y in xrange(0, self.height, NUM_PIXELS):
                patternSource.FillBox(x, x + NUM_PIXELS * 3 / 8,
                                      y, y + NUM_PIXELS / 4)
                patternSource.FillBox(x + NUM_PIXELS * 5 / 8, x + NUM_PIXELS,
                                      y, y + NUM_PIXELS / 4)
                patternSource.FillBox(x + NUM_PIXELS / 4, x + NUM_PIXELS * 3 / 4,
                                      y + NUM_PIXELS / 3, y + NUM_PIXELS * 2 / 3)


class EmptyCircle(Pattern):
    def draw(self, patternSource):
        if patternSource is None:
            return None
        global NUM_PIXELS
        for x in xrange(0, self.width, NUM_PIXELS):
            for y in xrange(0, self.height, NUM_PIXELS):
                patternSource.DrawCircle(x + NUM_PIXELS / 2, y + NUM_PIXELS / 2, NUM_PIXELS / 4.)


patterns = [Pattern, BottomLeftTri, TopRightTri, SmallRectDot, CheckerBoard,
            HorizStripe, VertStripe, HorizDash, VertDash, XDash, ThinDiagDownRight,
            ThickDiagRownRight, ThinDiagUpRight, ThickDiagUpRight, ThickThinVertStripe,
            ThickThinHorizStripe, LargeRectDot, Diamond, AbstractArt, Snake, EmptyCircle]
