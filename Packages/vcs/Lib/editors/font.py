import vcs


class FontEditor(object):

    def __init__(self, toolbar, font_setter, current_font="default"):

        self.toolbar = toolbar
        self.set_font = font_setter

        self.fonts = sorted(vcs.elements["font"].keys())
        if current_font not in self.fonts:
            current_font = "default"
        self.current_font = current_font

        def make_font_setter(font):
            def font_on():
                # Set current font button to "off"
                self.buttons[self.fonts.index(self.current_font)].set_state(0)
                self.set_font(font)
                self.current_font = font

            def font_off():
                # Set a default font to "on"
                self.buttons[self.fonts.index("default")].set_state(1)
                self.set_font("default")
                self.current_font = "default"
            return font_on, font_off

        font_bar = self.toolbar.add_toolbar("Fonts")
        self.buttons = []
        for font in self.fonts:
            on, off = make_font_setter(font)
            b = font_bar.add_toggle_button(font, on=on, off=off,
                                           on_prefix="Use", off_prefix="Using")
            self.buttons.append(b)
            if font == current_font:
                b.set_state(1)
