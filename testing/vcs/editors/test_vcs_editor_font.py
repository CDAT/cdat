import vcs
from vcs import vtk_ui, editors
import sys
import vtk

w = vtk.vtkRenderWindow()
i = vtk.vtkRenderWindowInteractor()
i.SetRenderWindow(w)
w.SetOffScreenRendering(True)
w.SetSize(200, 1000)

initial_font = "default"

font_set_to = [initial_font]


def set_font(f):
    font_set_to.append(f)

# Init vcs to get fonts loaded
x = vcs.init(backend=w)

# Create toolbar on the window for passing into the font editor
tb = vtk_ui.Toolbar(i, "Test")
# Create the font editor
fe = editors.font.FontEditor(tb, set_font, current_font=initial_font)

assert len(tb.widgets) > 0, "FontEditor not added to toolbar"
assert len(tb.widgets) == 1, "FontEditor added to wrong toolbar"
assert len(tb.bars) == 1, "FontEditor didn't set up toolbar correctly"

f_tb = tb.bars["Fonts"]
previous_buttons = []

for ind, button in enumerate(f_tb.widgets):

    if fe.fonts[ind] == font_set_to[-1]:
        # Should set to default
        new_font = "default"
    else:
        new_font = fe.fonts[ind]

    button.__advance__(None)
    print new_font, fe.fonts[ind], font_set_to[-1]
    assert new_font == font_set_to[-1], "Did not set font correctly"

    for i, b in enumerate(previous_buttons):
        if new_font == fe.fonts[i]:
            assert b.get_state() == 1, "FontEditor didn't toggle on correct font button"
        else:
            assert b.get_state() == 0, "FontEditor didn't toggle off disabled fonts"

    previous_buttons.append(button)

sys.exit(0)
