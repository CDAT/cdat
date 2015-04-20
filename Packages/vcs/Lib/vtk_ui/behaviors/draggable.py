from behavior import Behavior
import datetime


class DraggableMixin(Behavior):

    def __init__(self):
        super(DraggableMixin, self).__init__()
        self.drag_origin = None
        self.drag_position = None
        self.drag_started = None
        self.drag_buffer = 10  # in pixels
        self.add_event_handler("LeftButtonPressEvent", self.drag_clicked)
        self.add_event_handler("MouseMoveEvent", self.drag_moved)
        self.add_event_handler("LeftButtonReleaseEvent", self.drag_released)

    def drag_start(self):
        """Implement in subclass"""
        pass

    def drag_move(self, delta_x, delta_y):
        """Implement in subclass"""
        pass

    def drag_stop(self):
        """Implement in subclass"""
        pass

    def drag_clicked(self, obj, event):

        x, y = self.event_position()
        if self.in_bounds(x, y):
            self.drag_origin = self.event_position()
            self.drag_position = None
            self.drag_started = datetime.datetime.now()
            self.drag_start()

    def drag_moved(self, obj, event):
        x, y = self.event_position()

        if self.drag_origin is None or datetime.datetime.now() - self.drag_started < datetime.timedelta(0, .25):
            return

        if self.drag_position is None:
            x0, y0 = self.drag_origin

            w, h = self.interactor.GetRenderWindow().GetSize()
            x_buff = self.drag_buffer / float(w)
            y_buff = self.drag_buffer / float(h)
            if x0 + x_buff < x or x0 - x_buff > x or y0 + y_buff < y or y0 - y_buff > y:
                self.drag_position = self.drag_origin
            else:
                return

        old_x, old_y = self.drag_position

        self.drag_move(x - old_x, y - old_y)

        self.drag_position = (x, y)

        return

    def drag_released(self, obj, event):
        if self.drag_origin is not None or self.drag_position is not None and datetime.datetime.now() - self.drag_started > datetime.timedelta(0, .25):
            self.drag_stop()
            self.drag_origin = None
            self.drag_position = None
