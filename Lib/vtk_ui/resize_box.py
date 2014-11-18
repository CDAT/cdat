from handle import Handle

class ResizeBox(object):
    def __init__(self, interactor, point_1, point_2, color=(0, 0, 0), set_points=None, normalize=False):
        self.interactor = interactor

        self.normalize = normalize

        self.x1, self.y1 = point_1
        self.x2, self.y2 = point_2

        self.color = color

        handle_kwargs = {
            "color": self.color,
            "dragged": self.dragged,
            "clicked": self.clicked,
            "released": self.released
        }

        self.set_points = set_points

        self.top_left = Handle(interactor, (self.x1, self.y1), **handle_kwargs)
        self.top_right = Handle(interactor, (self.x2, self.y1), **handle_kwargs)
        self.bottom_left = Handle(interactor, (self.x1, self.y2), **handle_kwargs)
        self.bottom_right = Handle(interactor, (self.x2, self.y2), **handle_kwargs)
        self.center = Handle(interactor, self.get_center(), **handle_kwargs)


        self.handles = [self.top_left, self.top_right, self.bottom_left, self.bottom_right, self.center]

        self.place()
    
    def get_dimensions(self):
        return abs(self.x1 - self.x2), abs(self.y1 - self.y2)

    def get_center(self):
        w, h = self.get_dimensions()
        
        if self.normalize:
            sw, sh = self.interactor.GetRenderWindow().GetSize()
        else:
            sw, sh = 1

        return (w / 2.0 + min(self.x1, self.x2)) * sw, (h / 2.0 + min(self.y1, self.y2)) * sh

    def get_points(self):

        return (min(self.x1, self.x2), min(self.y1, self.y2)), (max(self.x1, self.x2), max(self.y1, self.y2))

    def place(self):

        if self.normalize:
            sw, sh = self.interactor.GetRenderWindow().GetSize()
        else:
            sw, sh = 1

        x1, y1 = sw * self.x1, sh * self.y1
        x2, y2 = sw * self.x2, sh * self.y2

        self.top_left.x = x1
        self.top_left.y = y1

        self.top_right.x = x2
        self.top_right.y = y1

        self.bottom_left.x = x1
        self.bottom_left.y = y2

        self.bottom_right.x = x2
        self.bottom_right.y = y2

        self.center.x, self.center.y = self.get_center()
        
        for handle in self.handles:
            handle.place()

    def show(self):
        self.place()
        for handle in self.handles:
            handle.show()
    
    def hide(self):
        for handle in self.handles:
            handle.hide()

    def clicked(self, handle):
        pass

    def released(self, handle):
        if self.set_points:
            # Don't let silly things happen, like reversing images. Just normalize to sane coordinates.
            self.set_points(self, self.get_points())

    def dragged(self, handle, x, y):
        if self.normalize:
            sw, sh = self.interactor.GetRenderWindow().GetSize()
            x, y = x / float(sw), y / float(sh)

        if handle == self.top_left:
            self.x1, self.y1 = x, y
        elif handle == self.top_right:
            self.x2, self.y1 = x, y
        elif handle == self.bottom_left:
            self.x1, self.y2 = x, y
        elif handle == self.bottom_right:
            self.x2, self.y2 = x, y
        elif handle == self.center:
            w, h = self.get_dimensions()
            self.x1 = x - w / 2.0
            self.y1 = y - h / 2.0
            self.x2 = self.x1 + w
            self.y2 = self.y1 + h
        
        self.place()