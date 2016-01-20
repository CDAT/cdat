import cdms2
import os
import sys
import vcs
import EzTemplate

# Load the clt data:
dataFile = cdms2.open(os.path.join(vcs.sample_data, "clt.nc"))
clt = dataFile("clt", time="1979-1-1", squeeze=1)


# Zero out the array so we can see the continents clearly
clt[:] = 0

# Initialize canvas:
canvas = vcs.init()
canvas.setantialiasing(0)
canvas.setbgoutputdimensions(1200, 1091, units="pixels")
canvas.drawlogooff()

# Create and plot quick boxfill with default settings:
boxfill = canvas.createboxfill()
# Change the type
boxfill.boxfill_type = 'custom'
# Set levels to ignore 0
boxfill.levels = [1, 100]
# Pick a color, any color
boxfill.fillareacolors = [242]

dataonly = vcs.createtemplate()
dataonly.blank()
dataonly.data.priority = 1

multitemplate = EzTemplate.Multi(template=dataonly, rows=4, columns=3)

line_styles = ['long-dash', 'dot', 'dash', 'dash-dot', 'solid']


for i in range(12):
    cont_index = i % 6 + 1
    cont_line = vcs.createline()
    cont_line.width = i % 3 + 1
    cont_line.type = line_styles[i % 5]
    cont_line.color = i + 200
    template = multitemplate.get(i)
    if cont_index != 3:
        canvas.plot(clt, template, boxfill, continents=cont_index, continents_line=cont_line, bg=1)
    else:
        canvas.setcontinentsline(cont_line)
        canvas.setcontinentstype(3)
        canvas.plot(clt, template, boxfill, bg=1)

# Load the image testing module:
testingDir = os.path.join(os.path.dirname(__file__), "..")
sys.path.append(testingDir)
import checkimage

# Create the test image and compare:
baseline = sys.argv[1]
testFile = "test_continents.png"
canvas.png(testFile)
ret = checkimage.check_result_image(testFile, baseline,
                                    25)
sys.exit(ret)
