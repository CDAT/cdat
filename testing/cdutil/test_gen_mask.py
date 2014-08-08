import cdms2,sys,cdutil,os

f=cdms2.open(os.path.join(sys.prefix,"sample_data","navy_land.nc"))
navy_frac = f("sftlf")/100.

target = cdms2.open(os.path.join(sys.prefix,'sample_data','clt.nc'))("clt",slice(0,1)).getGrid()
mask = cdutil.generateLandSeaMask(target,navy_frac)
target = cdms2.open(os.path.join(sys.prefix,'sample_data','clt.nc'))("clt",slice(0,1))
mask = cdutil.generateLandSeaMask(target,navy_frac)
target=cdms2.createGaussianGrid(64)
mask = cdutil.generateLandSeaMask(target)
target = cdms2.open(os.path.join(sys.prefix,'sample_data','clt.nc'))("clt",slice(0,1),latitude=(15,85),longitude=(-175,-65)).getGrid()
mask = cdutil.generateLandSeaMask(target)

#import vcs
#x=vcs.init()
#x.plot(mask)
#raw_input()
