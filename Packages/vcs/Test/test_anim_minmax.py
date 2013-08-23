import sys,cdms2,vcs
x=vcs.init()
f=cdms2.open(sys.prefix+"/sample_data/clt.nc")
s=f("clt",time=slice(0,3))

x.plot(s)
x.animate.create(thread_it=False,min=20,max=80)
x.animate.run()
raw_input("Press enter to end")

