import vcs, sys

passing = False


def save_clicked(colormap, color):
    if color == 135:
        global passing
        passing = True


picker = vcs.colorpicker.ColorPicker(500, 500, None, None, on_save=save_clicked)

interactor = picker.render_window.GetInteractor()
interactor.SetEventInformation(250, 260)
picker.clickEvent(None, None)
picker.save(0)

sys.exit(0 if passing else 1)
