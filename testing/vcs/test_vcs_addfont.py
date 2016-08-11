import vcs, os

x = vcs.init()
font_dir = os.path.dirname(vcs.elements["font"]["Adelon"])
default_fonts = 0

for font in vcs.elements["font"].values():
    if font.startswith(font_dir):
        default_fonts += 1

# Test adding a single font.
font_path = os.path.join(font_dir, "Adelon_Regular.ttf")
new_font_name = x.addfont(font_path)
assert new_font_name == "Adelon_Regular"
