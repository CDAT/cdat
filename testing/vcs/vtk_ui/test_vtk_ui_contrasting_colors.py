"""
Test contrasting colors
"""
from vcs.vtk_ui.text import contrasting_color

from vtk_ui_test import vtk_ui_test

def test(fg):
    bg = contrasting_color(*fg)
    print "\tTesting", fg, "vs", bg
    return passes_w3c_contrast_ratio(fg, bg)

def w3c_lum_normalize(component):
    if component <= .03928:
        return component / 12.92
    else:
        return ((component + .055) / 1.055) ** 2.4

def w3c_luminance(color):
    r, g, b = [w3c_lum_normalize(c) for c in color]
    lum = .2126 * r + .7152 * g + .0722 * b
    return lum

def w3c_contrast_ratio(fg, bg):
    lum_fg = w3c_luminance(fg)
    lum_bg = w3c_luminance(bg)
    l1 = max(lum_fg, lum_bg)
    l2 = min(lum_fg, lum_bg)

    return (l1 + .05) / (l2 + .05)

def passes_w3c_contrast_ratio(fg, bg):
    ratio = w3c_contrast_ratio(fg, bg)
    print "\tContrast ratio is", ratio
    if ratio > 4.5:
        return True
    else:
        return False



class test_vtk_ui_contrasting_colors(vtk_ui_test):
    def do_test(self):
        # Value tests
        midtone = (.5, .5, .5)
        light = (1, 1, 1)
        dark = (0, 0, 0)

        print "Testing contrasting values"
        passes = True
        if test(midtone) == False:
            print "\tFails midtone"
            passes = False
        else:
            print "\tPasses midtone"

        if test(light) == False:
            print "\tFails light"
            passes = False
        else:
            print "\tPasses light"

        if test(dark) == False:
            print "\tFails dark"
            passes = False
        else:
            print "\tPasses dark"
        print "%s value tests" % ("Passed" if passes else "Failed")
        self.passed = passes
        # Hue tests
        red = (1, 0, 0)
        blue = (0, 0, 1)
        green = (0, 1, 0)
        yellow = (1, 1, 0)
        cyan = (0, 1, 1)
        magenta = (1, 0, 1)
        passes = True
        print "Testing contrasting hues"
        if test(red) == False:
            print "\tFails red"
            passes = False
        else:
            print "\tPasses red"
        if test(blue) == False:
            print "\tFails blue"
            passes = False
        else:
            print "\tPasses blue"
        if test(green) == False:
            print "\tFails green"
            passes = False
        else:
            print "\tPasses green"
        if test(yellow) == False:
            print "\tFails yellow"
            passes = False
        else:
            print "\tPasses yellow"
        if test(cyan) == False:
            print "\tFails cyan"
            passes = False
        else:
            print "\tPasses cyan"
        if test(magenta) == False:
            print "\tFails magenta"
            passes = False
        else:
            print "\tPasses magenta"
        print "%s hue tests" % ("Passed" if passes else "Failed")
        self.passed = self.passed and passes

        # Saturation tests
        # Fully saturated handled by hue tests
        desaturated = (1, .8, .8)
        midsaturated = (1, .5, .5)
        passes = True
        print "Testing contrasting saturation"
        if test(desaturated) == False:
            print "\tFails desaturated"
            passes = False
        else:
            print "\tPasses desaturated"
        if test(midsaturated) == False:
            print "\tFails midsaturated"
            passes = False
        else:
            print "\tPasses midsaturated"
        print "%s saturation tests" % ("Passed" if passes else "Failed")
        self.passed = self.passed and passes
        if self.passed:
            self.passed = 0
        else:
            self.passed = 1

test_vtk_ui_contrasting_colors().test()
