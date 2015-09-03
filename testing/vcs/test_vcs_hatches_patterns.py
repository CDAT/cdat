import os
import sys
import vcs

pth = os.path.join(os.path.dirname(__file__), "..")
sys.path.append(pth)
import checkimage

baselineImage = sys.argv[1]

# Initialize the graphics canvas
x = vcs.init()
x.setantialiasing(0)
x.setbgoutputdimensions(1200, 1091, units="pixels")

# Create a test plot for listing all the hatches and patterns
style_list = []
index_list = []
col_cycle = [40, 220, 115, 201, 160, 80, 233, 20, 141, 243, 248, 252, 255]
nb_cols = len(col_cycle)
color_list = []
x_list = []
y_list = []
txt_x_list = []
txt_y_list = []
txt_str_list = []

for j, style in enumerate(['pattern', 'hatch']):
    slide_y = j * .4
    for i in range(20):
        slide_x = i * 0.04
        x1, y1 = (.05 + slide_x, .25 + slide_y)
        x2, y2 = (.08 + slide_x, .45 + slide_y)

        # Add rectangles to the list of positions
        # NOTE: no need to close the fill area. Giving 4 vertices
        #       for getting a filled rectangle is enough
        x_list.append([x1, x2, x2, x1])
        y_list.append([y1, y1, y2, y2])

        style_list.append(style)
        # Hatches/Patterns indices have to be in 1-20 range
        index_list.append(i+1)
        col_idx = col_cycle[i % nb_cols]
        color_list.append(col_idx)

        # Annotations
        txt_x_list.append(x1 + 0.015)
        txt_y_list.append(y1 - 0.015)
        txt_str_list.append('%s = %i  -  Color = %i' %
                            (style, i+1, col_idx))

# Create the fill area and the text annotations
fill_test = x.createfillarea('fill_hatches_patterns')
fill_test.style = style_list
fill_test.index = index_list
fill_test.color = color_list
fill_test.x = x_list
fill_test.y = y_list

fill_info = x.createtext('fill_info')
fill_info.angle = 45
fill_info.height = 12
fill_info.color = 241  # Black
fill_info.string = txt_str_list
fill_info.x = txt_x_list
fill_info.y = txt_y_list

# Create a title
plot_title = x.createtext('plot_title')
plot_title.height = 40
plot_title.string = ['Testing hatches and patterns in VCS/CDAT']
plot_title.x = [.01]
plot_title.y = [.9]

# Initialize and use a second graphics canvas
x.plot(plot_title, bg=1)
x.plot(fill_test, bg=1)
x.plot(fill_info, bg=1)

testImage = os.path.abspath("test_vcs_hatches_patterns.png")
x.png(testImage)

ret = checkimage.check_result_image(testImage, baselineImage,
                                    checkimage.defaultThreshold)

sys.exit(ret)
