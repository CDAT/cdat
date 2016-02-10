import vcs
import sys

x=vcs.init()

Ns = {}
for k in vcs.elements.keys():
    Ns[k] = len(vcs.elements[k].keys())
x.scriptrun(sys.argv[1])
Ns2 = {}
for k in vcs.elements.keys():
    Ns2[k] = len(vcs.elements[k].keys())
diffs = {'projection': 0, 'colormap': 53, 'isofill': 187, 'marker': 0, '3d_dual_scalar': 0, 'texttable': 4, '3d_scalar': 0, 'fillarea': 234, 'font': 0, '3d_vector': 0, '1d': 9, 'template': 43, 'textcombined': 0, 'textorientation': 3, 'xvsy': 0, 'xyvsy': 0, 'isoline': 113, 'boxfill': 239, 'fontNumber': 0, 'line': 21, 'meshfill': 0, 'yxvsx': 9, 'taylordiagram': 0, 'list': 26, 'display': 0, 'vector': 55, 'scatter': 0}
for k in vcs.elements.keys():
    assert(diffs[k] == Ns2[k]-Ns[k])

gm = x.getisofill("pr_time_lat_1")
assert(gm.ymtics1=="lat5")
assert(gm.ext_2 is True)
assert(gm.fillareacolors ==  [240, 240, 240, 28, 27, 26, 25, 23, 22, 21, 20, 19, 18, 16])
gm = x.getboxfill("lon_lat_mjop05")
assert(gm.xmtics1=="lon5")
assert(gm.yticlabels1=="lat20")
assert(gm.datawc_x1 == 30)
assert(gm.datawc_x2 == 210.)
assert(gm.datawc_y1 == -30)
assert(gm.datawc_y2 == 30.)
assert(gm.level_1==-0.05)
assert(gm.level_2==0.05)
assert(gm.color_1==18)
assert(gm.color_2==219)
gm = x.getline("red_solid")
assert(gm.type == ['solid'])
assert(gm.color == [242])
assert(gm.width == [2.0])

gm = x.getyxvsx("pr_lsfit_lat")
assert(gm.xmtics1=="lat5")
assert(gm.linecolor==242)
assert(gm.linewidth==2.)
assert(gm.datawc_x1 == 30)
assert(gm.datawc_x2 == -30.)
assert(gm.datawc_y1 == -5.)
assert(gm.datawc_y2 == 5.)
gm = x.getisoline("div_anom")
assert(gm.xmtics1=="lon5")
assert(gm.xticlabels1=="lon15")
assert(gm.line == ['dash', 'dash', 'dash', 'dash', 'solid', 'dash', 'dash', 'dash', 'solid', 'solid', 'solid', 'solid', 'solid', 'solid', 'solid', 'solid', 'solid'])
assert(gm.linecolors ==  [241, 241, 241, 241, 242, 241, 241, 241, 1, 1, 1, 1, 1, 1, 1, 1, 1])
assert(gm.linewidths ==  [1.0, 1.0, 1.0, 1.0, 2.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0])
gm = x.getvector("lon_lat_IO_5")
assert(gm.xmtics1=="lon5")
assert(gm.xticlabels1=="lon20")
assert(gm.linecolor==242)
assert(gm.linewidth==2.)
assert(gm.scale==3)
assert(gm.reference==5)
