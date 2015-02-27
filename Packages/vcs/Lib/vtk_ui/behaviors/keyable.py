from behavior import Behavior

class KeyableMixin(Behavior):

    def __init__(self):
        super(KeyableMixin, self).__init__()

        self.add_event_handler("KeyPressEvent", self.key_press)
        self.add_event_handler("KeyReleaseEvent", self.key_release)

    def current_modifiers(self):
        alt = self.interactor.GetAltKey()
        shift = self.interactor.GetShiftKey()
        control = self.interactor.GetControlKey()
        return {"alt":alt == 1, "shift":shift == 1, "control":control == 1}

    def key_pressed(self, key, shift=False, alt=False, control=False):
        return

    def key_released(self, key, shift=False, alt=False, control=False):
        return

    def key_release(self, obj, event):
        key = self.fetch_pressed()
        self.key_released(key, **self.current_modifiers())

    def key_press(self, obj, event):
        key = self.fetch_pressed()
        self.key_pressed(key, **self.current_modifiers())

    def fetch_pressed(self):
        code = self.interactor.GetKeyCode()
        if len(code) == 0 or ord(code) > 127:
            code = self.interactor.GetKeySym()
        return code
