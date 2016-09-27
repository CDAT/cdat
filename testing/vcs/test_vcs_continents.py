import os, sys, EzTemplate, cdms2, vcs, vcs.testing.regression as regression

# Load the clt data:
dataFile = cdms2.open(os.path.join(vcs.sample_data, "clt.nc"))
clt = dataFile("clt", time="1979-1-1", squeeze=1)

# Zero out the array so we can see the continents clearly
clt[:] = 0

# Initialize canvas:
canvas = regression.init()

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
    print "Cont_line_rtype:",line_styles[i % 5]
    cont_line.color = i + 200
    template = multitemplate.get(i)
    if cont_index != 3 and i != 4 and i != 11:
        canvas.plot(clt, template, boxfill, continents=cont_index, continents_line=cont_line, bg=1)
    elif cont_index == 3:
        canvas.setcontinentsline(cont_line)
        canvas.setcontinentstype(3)
        canvas.plot(clt, template, boxfill, bg=1)
    elif i == 4:
        canvas.setcontinentstype(0)
        # Make sure absolute path works
        path = os.path.join(vcs.prefix, "share", "vcs", "data_continent_political")
        canvas.plot(clt, template, boxfill, continents=path, continents_line=cont_line, bg=1)
    elif i == 11:
        # Make sure the dotdirectory other* works
        dotdir = vcs.getdotdirectory()
        current_dotdir = os.environ.get(dotdir[1], dotdir[0])
        os.environ["UVCDAT_DIR"] = os.path.join(vcs.prefix, "share", "vcs")
        # Should pick up the other7 continents
        canvas.plot(clt, template, boxfill, continents=7, continents_line=cont_line, bg=1)
        os.environ["UVCDAT_DIR"] = current_dotdir

regression.run(canvas, "test_continents.png")