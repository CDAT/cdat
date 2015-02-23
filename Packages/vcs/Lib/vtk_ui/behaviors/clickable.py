from behavior import Behavior

class ClickableMixin(Behavior):

    def __init__(self):
        # Used to track double clicks
        self.click_timer = None
        self.release_timer = None

        # Used to identify double clicks (in milliseconds)
        self.double_click_duration = 200
        self.clicked = False
        super(ClickableMixin, self).__init__()

        self.add_event_handler("LeftButtonPressEvent", self.click_pressed)
        self.add_event_handler("LeftButtonReleaseEvent", self.click_released)
        self.add_event_handler("RightButtonPressEvent", self.right_pressed)
        self.add_event_handler("RightButtonReleaseEvent", self.right_released)

        # If they're moving the mouse, it's probably not a double click.
        self.add_event_handler("MouseMoveEvent", self.timer_elapsed)
        self.add_event_handler("TimerEvent", self.timer_elapsed)


    def click_press(self):
        """Implement in subclass, called on mouse down"""
        pass

    def click_release(self):
        """Implement in subclass, called on mouse up"""
        pass

    def double_press(self):
        """Implement in subclass, called on double mouse down"""
        pass

    def double_release(self):
        """Implement in subclass, called on double mouse up"""
        pass

    def right_press(self):
        """Implement in subclass, called on right mouse down"""
        pass

    def right_release(self):
        """Implement in subclass, called on right mouse up"""
        pass

    def right_pressed(self, obj, event):
        self.right_press()

    def right_released(self, obj, event):
        self.right_release()

    def timer_elapsed(self, obj, event):
        if self.click_timer is not None:
            self.interactor.DestroyTimer(self.click_timer)
            self.click_timer = None
            self.click_press()

        if self.release_timer is not None:
            self.interactor.DestroyTimer(self.release_timer)
            self.release_timer = None
            self.click_release()

    def click_pressed(self, obj, event):
        self.clicked = True
        if self.click_timer is None:
            self.click_timer = self.interactor.CreateOneShotTimer(self.double_click_duration)
        else:
            self.interactor.DestroyTimer(self.click_timer)
            self.click_timer = None
            self.double_press()

    def click_released(self, obj, event):
        # A weird spam event was coming through on object initialization
        if self.clicked == False:
            return
        else:
            self.clicked = False

        if self.release_timer is None:
            self.release_timer = self.interactor.CreateOneShotTimer(self.double_click_duration)
        else:
            self.interactor.DestroyTimer(self.release_timer)
            self.release_timer = None
            self.double_release()