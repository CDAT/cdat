#############################################################################
#  File:    geoparse.py                                                     #
#  Author:  Velimir Mlaker, mlaker1@llnl.gov                                #
#  Date:    05-Aug-2005                                                     #
#  Desc:    Parsers for geometry string.                                    #
#           KNOWN BUG: Only works on form wxh+x+y, i.e. with '+' for x and  #
#                      y coords. It will fail if using '-' for x and y.     #
#############################################################################

# Extract width from the geometry string.
def get_w (geo):
    return geo [:geo.find('x')]

# Extract height from the geometry string.
def get_h (geo):
    return geo [geo.find('x')+1 : geo.find('+')]

# Extract X and Y from the geometry string.
def get_xy (geo):
    return geo [geo.find('+')+1 : len(geo)]

# Extract X from the geometry string.
def get_x (geo):
    xy = get_xy(geo)
    return xy [:xy.find('+')]

# Extract Y from the geometry string.
def get_y (geo):
    xy = get_xy(geo)
    return xy [xy.find('+')+1 : len(xy)]
