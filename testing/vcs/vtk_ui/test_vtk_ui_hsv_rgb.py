"""
Test hsv/rgb conversions
"""
from vcs.vtk_ui.text import hsv_to_rgb, rgb_to_hsv

from vtk_ui_test import vtk_ui_test


def equal(tup1, tup2, margin=.01):
    if len(tup1) != len(tup2):
        return False

    for ind in range(len(tup1)):
        if tup1[ind] - margin <= tup2[ind] and tup1[ind] + margin >= tup2[ind]:
            continue
        else:
            return False

    return True


def truncate_float(f, to_position=2):
    """
    Chops off parts after to_position
    """
    string_repr = str(f)
    leading, point, trailing = string_repr.partition(".")
    return float(leading + point + trailing[:to_position])


class test_vtk_ui_hsv_rgb(vtk_ui_test):
    def do_test(self):
        hsvs = [
            (229, 1, .5),
            (0, 0, 0),
            (0, 0, 1),
            (0, 0, .5),
            (98, .68, 1),
            (279, .22, .53)
        ]
        rgbs = [
            (0, truncate_float(23/255.), truncate_float(127/255.)),
            (0, 0, 0),
            (1, 1, 1),
            (.5, .5, .5),
            (truncate_float(145 / 255.), 1, truncate_float(81 / 255.)),
            (truncate_float(124 / 255.), truncate_float(105 / 255.), truncate_float(135 / 255.)),
        ]

        passing = True
        for ind in range(len(hsvs)):
            rgb = hsv_to_rgb(*hsvs[ind])
            rgb = tuple((truncate_float(part) for part in rgb))

            hsv = rgb_to_hsv(*rgbs[ind])
            hsv = tuple((truncate_float(part) for part in hsv))

            if not equal(rgb, rgbs[ind]):
                print "hsv_to_rgb failed to convert", hsvs[ind], "expected", rgbs[ind], "received", rgb
                passing = False
            if not equal(hsv, hsvs[ind]):
                if equal(hsv[:1], hsvs[ind][:1], margin=1) and equal(hsv[1:], hsvs[ind][1:]):
                    # The hue can have a larger variance, since it's not in the 0-1 range
                    continue
                print "rgb_to_hsv failed to convert", rgbs[ind], "expected", hsvs[ind], "received", hsv
                passing = False

        if passing:
            self.passed = 0

if __name__ == "__main__":
    test_vtk_ui_hsv_rgb().test()
