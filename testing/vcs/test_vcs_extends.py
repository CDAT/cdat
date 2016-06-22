import vcs
import numpy

box = vcs.createboxfill()

box.ext_1 = True
assert numpy.allclose(box.levels, [1e20] * 2)

box.ext_2 = True
assert numpy.allclose(box.levels, [1e20] * 2)

box.levels = [1, 10, 20, 30, 40, 50, 60, 70, 80, 90, 100]
assert box.ext_1 == False
assert box.ext_1 == False

box.ext_1 = True
assert box.levels[0] < -9e19

box.ext_2 = True
assert box.levels[-1] > 9e19

box.ext_1 = False
assert box.levels[0] > -9e19

box.ext_2 = False
assert box.levels[-1] < 9e19



