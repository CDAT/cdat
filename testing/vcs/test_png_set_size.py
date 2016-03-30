import vcs
import sys
import os

# This test checks that png size is indeed controled by user
import struct


def get_image_info(fnm):
    data = open(fnm,"rb").read()
    w, h = struct.unpack('>LL', data[16:24])
    width = int(w)
    height = int(h)
    return width, height


x=vcs.init()
x.drawlogooff()
x.setantialiasing(0)

x.plot([1,2,3,4,5,4,3,2,1],bg=1)

fnm = "test_png_set_size.png"
x.png(fnm,width=15)
print get_image_info(fnm)
assert(get_image_info(fnm) == (15,11))
x.png(fnm,height=16)
print get_image_info(fnm)
assert(get_image_info(fnm) == (20,16))
x.png(fnm,width=15,height=12)
print get_image_info(fnm)
assert(get_image_info(fnm) == (15,12))
os.remove(fnm)
