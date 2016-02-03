# Check if text chunks are saved correctly in a PNG file
import vcs
import os

x=vcs.init()
x.drawlogooff()
x.setantialiasing(0)

x.plot([1,2,3,4,5,4,3,2,1],bg=1)

fnm = "test_png_metadata.png"
m = {'one':'value one','two':'value two'}
x.png(fnm, width=15, metadata=m)
assert(vcs.png_read_metadata(fnm) == m)
os.remove(fnm)
