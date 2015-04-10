import vtk

class Line(object):
  def __init__(self, point_1, point_2, color=(0,0,0), width=1, renderer=None):
    self.line = vtk.vtkLineSource()
    mapper = vtk.vtkPolyDataMapper2D()
    mapper.SetInputConnection(self.line.GetOutputPort())
    self.actor = vtk.vtkActor2D()
    self.actor.SetMapper(mapper)
    self._renderer = None

    # Default to hidden
    self.hide()

    # Actor and everything need to be set up before magic properties work
    self.point_1 = point_1
    self.point_2 = point_2
    self.color = color
    self.width = width
    self.renderer = renderer

  def hide(self):
    self.actor.VisibilityOff()

  def show(self):
    self.actor.VisibilityOn()

  @property
  def showing(self):
    return self.actor.GetVisibility() == 1

  @property
  def point_1(self):
    return self.line.GetPoint1()[:2]

  @point_1.setter
  def point_1(self, value):
    self.line.SetPoint1(value[0], value[1], 10)

  @property
  def point_2(self):
    return self.line.GetPoint2()[:2]

  @point_2.setter
  def point_2(self, value):
    self.line.SetPoint2(value[0], value[1], 10)

  @property
  def x1(self):
    return self.point_1[0]

  @x1.setter
  def x1(self, value):
    p1 = self.point_1
    self.point_1 = (value, p1[1], 10)

  @property
  def x2(self):
    return self.point_2[0]

  @x2.setter
  def x2(self, value):
    p = self.point_2
    self.point_2 = (value, p[1], 10)

  @property
  def y1(self):
    return self.point_1[1]

  @y1.setter
  def y1(self, value):
    p = self.point_1
    self.point_1 = (p[0], value, 10)

  @property
  def y2(self):
    return self.point_2[1]

  @y2.setter
  def y2(self, value):
    p = self.point_2
    self.point_2 = (p[0], value, 10)

  @property
  def width(self):
    return self.actor.GetProperty().GetLineWidth()

  @width.setter
  def width(self, value):
    self.actor.GetProperty().SetLineWidth(value)

  @property
  def color(self):
    return self.actor.GetProperty().GetColor()

  @color.setter
  def color(self, value):
    self.actor.GetProperty().SetColor(value)

  @property
  def renderer(self):
    return self._renderer

  @renderer.setter
  def renderer(self, value):
    if self._renderer:
      self._renderer.RemoveActor(self.actor)
    self._renderer = value
    if value is not None:
      self._renderer.AddActor(self.actor)

  def detach(self):
    self.renderer = None
    del self.actor
    del self.line