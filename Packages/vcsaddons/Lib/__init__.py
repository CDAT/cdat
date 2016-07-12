gms = {}
import histograms
import polar
import EzTemplate
import yxvsxfill
import continents
import vcs


def createyxvsxfill(name=None,source='default',x=None,template=None):
    return yxvsxfill.Gyf(name,source=source,x=x,template=template)


def createhistogram(name=None,source='default',x=None,template=None):
    return histograms.Ghg(name,source=source,x=x,template=template)


def createusercontinents(name=None,source="default",x=None,template=None):
    return continents.Guc(name,source=source,x=x,template=template)


def createpolar(name=None, source="default", x=None, template=None):
    if "polar_oned" not in gms:
        init_polar()
    return polar.Gpo(name, source=source, x=x, template=template)


def getpolar(name=None):
    if "polar_oned" not in gms:
        init_polar()
    if name in gms["polar_oned"]:
        return gms["polar_oned"][name]
    raise KeyError("No Polar GM exists with name '%s'" % name)


def init_polar():
    # Create nice polar template
    try:
        t = vcs.createtemplate("polar_oned")
        t.data.x1 = .2
        t.data.x2 = .8
        t.data.y1 = .2
        t.data.y2 = .8

        t.legend.x1 = .85
        t.legend.x2 = 1
        t.legend.y1 = .15
        t.legend.y2 = .85

        dash = vcs.createline()
        dash.type = "dash"
        dot = vcs.createline()
        dot.type = "dot"
        t.xtic1.line = dash
        t.ytic1.line = dot

        left_aligned = vcs.createtextorientation()
        left_aligned.halign = "left"
        left_aligned.valign = "half"
        t.legend.textorientation = left_aligned
    except vcs.vcsError:
        # Template already exists
        pass
    # Create some nice default polar GMs
    degree_polar = polar.Gpo("degrees", template="polar_oned")
    degree_polar.datawc_x1 = 0
    degree_polar.datawc_x2 = 360
    degree_polar.xticlabels1 = {
        i: str(i) for i in range(0, 360, 45)
    }

    clock_24 = polar.Gpo("diurnal", template="polar_oned")
    clock_24.datawc_x1 = 0
    clock_24.datawc_x2 = 24
    clock_24.clockwise = True
    # 6 AM on the right
    clock_24.theta_offset = -6
    clock_24.xticlabels1 = {
        i: str(i) for i in range(0, 24, 3)
    }

    clock_24_meridiem = polar.Gpo("diurnal_12_hour", source="diurnal", template="polar_oned")
    clock_24_meridiem.xticlabels1 = {
        0: "12 AM",
        3: "3 AM",
        6: "6 AM",
        9: "9 AM",
        12: "12 PM",
        15: "3 PM",
        18: "6 PM",
        21: "9 PM"
    }

    clock_12 = polar.Gpo("semidiurnal", source="diurnal", template="polar_oned")
    clock_12.datawc_x2 = 12
    clock_12.xticlabels1 = {
        i: str(i) for i in range(3, 13, 3)
    }
    # 3 on the right
    clock_12.theta_offset = -3

    annual_cycle = polar.Gpo("annual_cycle", template="polar_oned")
    annual_cycle.datawc_x1 = 1
    annual_cycle.datawc_x2 = 13
    annual_cycle.clockwise = True
    annual_cycle.xticlabels1 = {
        1: "Jan",
        2: "Feb",
        3: "Mar",
        4: "Apr",
        5: "May",
        6: "Jun",
        7: "Jul",
        8: "Aug",
        9: "Sep",
        10: "Oct",
        11: "Nov",
        12: "Dec"
    }
    # Put December on the top
    annual_cycle.theta_offset = -2

    seasonal = polar.Gpo("seasonal", template="polar_oned")
    seasonal.datawc_x1 = 0
    seasonal.datawc_x2 = 4
    seasonal.xticlabels1 = {0: "DJF", 1: "MAM", 2: "JJA", 3: "SON"}
    seasonal.clockwise = True
    # DJF on top
    seasonal.theta_offset = -1
