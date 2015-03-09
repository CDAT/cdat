import vcs

class FontEditor(object):
    def __init__(self, toolbar, font_setter, current_font="default"):

        self.toolbar = toolbar
        self.set_font = font_setter

        self.fonts = sorted(vcs.elements["font"].keys())

        self.font_button = self.toolbar.add_button(self.fonts, action=self.font_state)
        self.font_button.set_state(self.fonts.index(current_font))

    def font_state(self, state):
        font = self.fonts[state]
        self.set_font(font)