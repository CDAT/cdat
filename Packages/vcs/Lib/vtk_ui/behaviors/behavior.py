class Behavior(object):
    """
    Base class; requires subclasses to have an interactor property
    """

    def __init__(self):

        self.events = {}
        self.listeners = {}
        super(Behavior, self).__init__()

    def event_position(self, normalized=True):
        point = self.interactor.GetEventPosition()
        if normalized:
            w, h = self.interactor.GetRenderWindow().GetSize()

            if w == 0 or h == 0:
                return 0, 0

            x, y = point
            x = x / float(w)
            y = y / float(h)
            point = (x, y)
        return point

    def in_bounds(self, x, y):
        raise NotImplementedError("Bounds Check not implemented")

    def add_event_handler(self, event, action):
        if event in self.events:
            self.events[event].append(action)
        else:
            self.events[event] = [action]

    def register(self):
        for event in self.events:
            def call_in_order(obj, event):
                for action in self.events[event]:
                    action(obj, event)

            self.subscribe(event, call_in_order)

    def unregister(self):
        for event in self.listeners.keys():
            self.unsubscribe(event)
        self.listeners = {}

    def subscribe(self, event, action):
        self.unsubscribe(event)
        self.listeners[event] = self.interactor.AddObserver(event, action)

    def unsubscribe(self, event):
        if event in self.listeners:
            self.interactor.RemoveObserver(self.listeners[event])
            del self.listeners[event]