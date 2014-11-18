
class Widget(object):
    def __init__(self, interactor, widget):
        self.interactor = interactor
        self.widget = widget
        self.widget.SetInteractor(interactor)
        self.repr = widget.GetRepresentation()
        self.subscriptions = {}

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