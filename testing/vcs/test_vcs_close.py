import os, sys, vcs, cdms2

pth = os.path.join(os.path.dirname(__file__),"..")
sys.path.append(pth)
cdmsfile = cdms2.open(vcs.sample_data+"/clt.nc")
data = cdmsfile('clt')
x = vcs.init()
x.plot(data, bg=1)
x.close()
sys.exit(0)
