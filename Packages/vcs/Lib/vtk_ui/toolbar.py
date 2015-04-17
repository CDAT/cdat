from button import ToggleButton, SliderButton, ButtonState, Button

class Toolbar(object):

    def __init__(self, interactor, label, vertical=True, left=10, top=10, open_label="Open", on_open=None, close_label="Close", button_margin=5, parent=None, save=None):

        self.save = save
        self.interactor = interactor
        self.margin = button_margin
        # Left is automatically modified by the margin; was running into issues with updating left at a later date, this was the easiest solution.
        self.left = left
        self.top = top
        self.label = ToggleButton(self.interactor, label, on=self.__on__, off=self.__off__, on_prefix=open_label, off_prefix=close_label, left=self.left - self.margin, top=self.top)
        self.on_open = on_open
        self.vertical = vertical

        self.open = False

        # Increment this as widgets are added
        self.width, self.height = self.label.get_dimensions()
        self.height += button_margin

        self.widgets = []
        self.bars = {}
        self.parent = parent

        self.__placing__ = False

    def get_text(self):
        return self.label.get_text()

    def in_toolbar(self, x, y):
        width, top = self.interactor.GetRenderWindow().GetSize()
        if x > self.left and x < self.left + self.width and y < top and y > top - self.height:
            return True
        else:
            return False

    # Left property definition
    def __getleft(self):
        return self.__left
    def __setleft(self, left):
        self.__left = left + self.margin
    def __delleft(self):
        del self.__left
    left = property(__getleft, __setleft, __delleft, "The Left property, modified by the margin")

    def __on__(self):
        self.open = True
        if self.on_open is not None:
            self.on_open()
        self.show_widgets()

    def __off__(self):
        self.open = False
        self.hide_widgets()

    def copy(self, interactor):
        t = Toolbar(interactor, self.label.label, vertical=self.vertical, left=self.left - self.margin, top=self.top, button_margin=self.margin)

        for widget in self.widgets:
            t.widgets.append(widget.copy(interactor))

        if self.label.get_state() == 1:
            t.show_widgets()
            t.label.set_state(1)

        return t

    def place(self):

        if self.parent and self.__placing__ == False:
            # Navigate up the tree, work from the root
            self.parent.place()
            return

        self.label.left = self.left - self.margin
        self.label.top = self.top
        self.label.place()


        self.width, self.height = self.label.get_dimensions()

        if self.vertical:
            self.height += self.margin
        else:
            self.width += self.margin

        for widget in self.widgets:

            widget_width, widget_height = widget.get_dimensions()

            if self.vertical:
                widget.left = self.left
                widget.top = self.top + self.height
            else:
                widget.left = self.left + self.width
                widget.top = self.top

            widget.__placing__ = True
            widget.place()
            widget.__placing__ = False

            widget_width, widget_height = widget.get_dimensions()

            if self.vertical:
                self.height += widget_height + self.margin
            else:
                self.width += widget_width + self.margin

    def get_dimensions(self):
        """
        Widget shim; makes this behave the same as a button with regards to being layed out.
        """
        if self.open:
            # Children are visible, width/height are correct
            return self.width, self.height
        else:
            # Children are invisible, use label dimensions
            return self.label.get_dimensions()

    def show(self):
        """
        Widget shim; makes this behave the same as a button with regards to being layed out.
        """
        self.place()
        self.label.show()
        if self.open:
            self.show_widgets()

    def hide(self):
        """
        Widget shim; makes this behave the same as a button with regards to being layed out.
        """
        self.label.hide()
        self.hide_widgets()

    def show_widgets(self):
        for widget in self.widgets:
            widget.show()

        self.place()

    def hide_widgets(self):
        """
        Hides all widgets in this toolbar

        Triggers a reflow of the toolbar tree
        """
        for button in self.widgets:
            button.hide()

        self.place()

    def detach(self):
        self.label.detach()
        for w in self.widgets:
            w.detach()

    def get_toolbar(self, label):
        """
        Retrieve a toolbar by label.

        Should probably use some other identifier, but this is the only one available right now.
        """
        return self.bars.get(label, None)

    """
    Convenience functions to add managed widgets to the toolbar.
    """

    def add_button(self, labels, **kwargs):

        states = [ButtonState(label=label) for label in labels]
        kwargs["states"] = states

        if self.vertical:
            kwargs["left"] = self.left
            kwargs["top"] = self.top + self.height + self.margin
        else:
            kwargs["left"] = self.left + self.width + self.margin
            kwargs["top"] = self.top

        b = Button(self.interactor, **kwargs)

        if self.open:
            b.show()

        if self.vertical:
            self.height = kwargs["top"] + b.get_dimensions()[1]
        else:
            self.width = kwargs["left"] + b.get_dimensions()[0]

        self.widgets.append(b)
        return b



    def add_toolbar(self, label, **kwargs):
        """
        Adds a toolbar using the same configs as this one has
        """
        kwargs["parent"] = self
        if self.vertical:
            kwargs["top"] = self.top + self.height + self.margin
            kwargs["left"] = self.left
        else:
            kwargs["top"] = self.top
            kwargs["left"] = self.left + self.width + self.margin
        kwargs["button_margin"] = self.margin

        if "vertical" not in kwargs:
            kwargs["vertical"] = self.vertical

        toolbar = Toolbar(self.interactor, label, **kwargs)

        if self.open:
            toolbar.label.show()

        if self.vertical:
            self.height += self.margin + toolbar.get_dimensions()[1]
        else:
            self.width += self.margin + toolbar.get_dimensions()[0]

        self.widgets.append(toolbar)
        self.bars[label] = toolbar
        return toolbar

    def add_toggle_button(self, label, **kwargs):

        _off = kwargs.get("off", None)
        _on  = kwargs.get("on", None)

        def off():
            if _off:
                _off()
            if self.save:
                self.save()

        def on():
            if _on:
                _on()

        kwargs['off'] = off
        kwargs['on'] = on

        if self.vertical:
            kwargs["left"] = self.left
            kwargs["top"] = self.top + self.height + self.margin
        else:
            kwargs["left"] = self.left + self.width + self.margin
            kwargs["top"] = self.top

        b = ToggleButton(self.interactor, label, **kwargs)

        if self.open:
            b.show()

        if self.vertical:
            self.height = kwargs["top"] + b.get_dimensions()[1]
        else:
            self.width = kwargs["left"] + b.get_dimensions()[0]

        self.widgets.append(b)
        return b

    def add_slider_button(self, value, min_val, max_val, label, **kwargs):

        _end = kwargs.get("end", None)
        def end(val):

            if _end:
                _end(val)

            if self.save:
                self.save()

        _on_show = kwargs.get("on_show_slider", None)
        def on_show():
            self.hide_sliders()
            if _on_show:
                _on_show()

        kwargs["on_show_slider"] = on_show
        kwargs["end"] = end

        if self.vertical:
            kwargs["left"] = self.left
            kwargs["top"] = self.top + self.height + self.margin
        else:
            kwargs["left"] = self.left + self.width + self.margin
            kwargs["top"] = self.top

        b = SliderButton(self.interactor, value, min_val, max_val, label, **kwargs)

        if self.open:
            b.show()

        if self.vertical:
            self.height = kwargs["top"] + b.get_dimensions()[1]
        else:
            self.width = kwargs["left"] + b.get_dimensions()[0]

        self.widgets.append(b)
        return b

    def hide_sliders(self):
        for w in self.widgets:
            if type(w) == SliderButton:
                if w.slider.showing():
                    w.set_state(0)
                    w.off()
            if type(w) == Toolbar:
                w.hide_sliders()
