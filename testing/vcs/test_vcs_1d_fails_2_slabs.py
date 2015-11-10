import vcs
x = vcs.init()
try:
    x.xvsy([1,2,3,4],[1,2,3,4,5],bg=True)
    failed = True
except:
    failed = False

if failed is False:
    raise RuntimeError("plotting 1d with two slabs should have failed")
