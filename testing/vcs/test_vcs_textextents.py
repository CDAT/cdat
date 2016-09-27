import os, sys, numpy, cdms2, MV2, vcs, vcs.testing.regression as regression

# We have to specify the geometry to make sure that the size of the canvas doesn't change between the init and the plot functions
x = regression.init(bg=True, geometry=(1200,1091))
text = x.createtext()
text.string = ["A very very very very long string", "A\nmult-line\nstring", "Short"]
# Use any value for initial; then we'll manually "right align" using the text extents
text.x = [.1]
text.y = [.1, .5, .9]

# This function only gets the extents for the *current* size
extents = x.gettextextent(text)
# Now we'll manually populate this with the desired values
text.x = []
for min_x, max_x, min_y, max_y in extents:
    w = max_x - min_x
    #h = max_y - min_y
    text.x.append(1 - w)

x.plot(text, bg=1)
regression.run(x, "test_textextents.png")
