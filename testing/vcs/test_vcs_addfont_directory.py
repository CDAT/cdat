import vcs, os

x = vcs.init()
font_dir = os.path.dirname(vcs.elements["font"]["Adelon"])

orig_fonts = set()

for font in vcs.elements["font"].values():
    if font.startswith(font_dir):
        orig_fonts.add(font)

# Test adding a directory of fonts.
all_the_new_fonts = set(x.addfont(font_dir))
assert len(all_the_new_fonts) == len(orig_fonts)
