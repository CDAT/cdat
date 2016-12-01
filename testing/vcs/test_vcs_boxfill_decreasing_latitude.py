#!/usr/bin/env python
import cdms2, cdutil, os, sys, vcs, vcs.testing.regression as regression

f = cdms2.open(sys.argv[2])
ice = f("variable_6")
x = regression.init()

gm = x.createboxfill()
gm.boxfill_type = "custom"

tmpl = x.createtemplate()
#tmpl.blank()
tmpl.data.priority = 1
tmpl.data.x1 = .05
tmpl.data.x2 = .95
tmpl.data.y1 = .05
tmpl.data.y2 = .90
tmpl.title.priority = 1
tmpl.box2.priority = 1
tmpl.box2.x1 = .23
tmpl.box2.x2 = .815
tmpl.box2.y1 = .11
tmpl.box2.y2 = .895
tmpl.title.x = .5
tmpl.title.y = .95

tmpl.legend.priority = 1
tmpl.legend.x1 = tmpl.box2.x1 - .05
tmpl.legend.x2 = tmpl.box2.x2 + .05
tmpl.legend.y1 = .03
tmpl.legend.y2 = .055
tmpl.max.priority = 1

txt = x.createtext()
txt.height = 20
txt.valign = "half"
txt.halign = "center"

tmpl.title.textorientation = txt.To_name
tmpl.title.texttable = txt.Tt_name

ice.long_name = "September sea ice fraction: 4xCO2 climate, no ocean albedo alteration "
levs = vcs.mkscale(ice.min(), ice.max())

cols = vcs.getcolors(levs)
cols[0] = 'white'

gm.levels = levs
gm.fillareacolors = cols
#gm.projection="polar"
gm.datawc_y2 = 30
gm.datawc_y1 = 90

x.plot(ice, gm, tmpl, bg=1)
fnm = os.path.split(__file__)[1][:-3] + ".png"
regression.run(x, fnm)
