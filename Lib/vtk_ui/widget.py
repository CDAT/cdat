from manager import get_manager

class Widget(object):
    def __init__(self, interactor, widget):
        self.interactor = interactor
        self.widget = widget
        self.widget.SetInteractor(interactor)
        self.repr = widget.GetRepresentation()
        self.subscriptions = {}
        self.manager = get_manager(interactor)
        self.manager.add_widget(self)
        super(Widget, self).__init__()

    def log(self, message):
        print repr(self), message

    def subscribe(self, event, action):
        if event in self.subscriptions:
            raise Exception("%s already subscribed to %s's %s event." % (action, self.widget, event))

        self.subscriptions[event] = self.widget.AddObserver(event, action)

    def unsubscribe(self, *events):
        """
        Will unsubscribe from all or none events. If any events are not subscribed to, there will be an exception.
        """

        for event in events:
            if event not in self.subscriptions:
                raise Exception("%s not subscribed to %s's %s event." % (self, self.widget, event))

        for event in events:
            self.widget.RemoveObserver(self.subscriptions[event])
            del self.subscriptions[event]

    def showing(self):
        return self.widget.GetEnabled() == 1

    def show(self):
        if not self.showing():
            self.widget.SetEnabled(True)
            self.place()
            self.manager.queue_render()

    def hide(self):
        if self.showing():
            self.widget.SetEnabled(False)
            self.widget.SetCurrentRenderer(self.manager.renderer)
            self.manager.queue_render()

    def detach(self):
        render = self.widget.GetCurrentRenderer()
        if render is None:
            return

        if render.HasViewProp(self.repr):
            render.RemoveViewProp(self.repr)
        self.unsubscribe(*self.subscriptions.keys())
        self.widget.SetCurrentRenderer(None)
        self.widget.SetInteractor(None)
        self.widget.Off()
        get_manager(self.interactor).remove_widget(self)
        self.interactor = None

class WidgetReprShim(object):
    """
    Used to substitute for a vtkWidget and vtkWidgetRepresentation when using actors directly
    """
    def __init__(self, interactor, actor):
        self._inter = interactor
        self._actor = actor
        self._ren = None

    def SetPosition(self, x, y):
        self._actor.SetPosition((x, y))

    def GetPosition(self):
        return self._actor.GetPosition()

    def GetEnabled(self):
        return 1 if self._actor.GetVisibility() else 0

    def GetRenderer(self):
        return self._ren

    def GetCurrentRenderer(self):
        return self._ren

    def SetCurrentRenderer(self, renderer):
        """
        The main cause of all this hubbub; need to use the actor_renderer.
        """
        man = get_manager(self._inter)
        self._ren = man.actor_renderer
        self._ren.AddActor(self._actor)

    def AddObserver(self, event, action):
        """
        Passes through to interactor
        """
        return self._inter.AddObserver(event, action)

    def RemoveObserver(self, e):
        """
        Passes through to interactor
        """
        self._inter.RemoveObserver(e)

    def SetInteractor(self, interactor):
        self._inter = interactor

    def GetRepresentation(self):
        """
        This object is also a shim for representations
        """
        return self