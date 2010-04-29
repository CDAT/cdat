import os,sys
if os.path.exists('../src/build/lib'):
	sys.path.insert(0,'../src/build/lib')
import spanlib,cdms2 as cdms,MV2 as MV,numpy as npy,pylab as P

##var = npy.random.rand(50,100).astype('f')
var = npy.zeros((150,100),'f')
nt,nx = var.shape

xx = npy.arange(nx,dtype='f')
tt = npy.arange(nt,dtype='f')

xfreq = 2
tfreq = 3
for ifreq in .5,3:
	tvar = (npy.cos(2*npy.pi * tfreq * tt/(nt-1))*tt/(nt-1)).reshape((nt,1))
	xvar = npy.cos(2*npy.pi * xfreq*ifreq * xx/(nx-1)).reshape((1,nx))
	var += npy.multiply(tvar,xvar)
for i in xrange(nt):
	for j in xrange(nx):
		var[i,j] += 3*(i+j)/(nx+nt)
#var*=1.e10
var-=var.mean(axis=0)

vv = var.copy()

##cov = npy.dot(var.T, var)/len(var)
#cov = npy.cov(var.T, bias=1)
##print '- xcov',cov[0,:3]
#txev,txeof = npy.linalg.eigh(cov)
#isort = npy.argsort(txev)[::-1]
#xev = txev[isort]
#xeof = txeof[:, isort]
#print '- xev[:3]:',xev[:3]
#print "- diag(Et.COV.E)[:3]", npy.diag(npy.dot(xeof.T,npy.dot(cov,xeof)))[:3]
##print '- xeof**2',[(xeof[:,i]**2).sum() for i in xrange(3)]
#print '- xeof[0, :3]:', xeof[0, :3]
#print '- max,min eof:',npy.max(xeof[:,0]),npy.min(xeof[:,0])
#print '- ev sums',xev.sum(),cov.trace()
#
## EOF[:, i] = ieme EOF  (EOF(ns,npca))

print '-'*70
lon = cdms.createAxis(xx,id='lon')
lon.units = 'degrees_east'
lon.designateLongitude()

time = cdms.createAxis(tt,id='time')
time.units = 'months since 2000'
time.designateTime()

var = MV.array(var,copy=0,id='var')
var.units = 'm'
var.long_name = 'My variable'
var.setAxisList([time,lon])

f=cdms.open('var.nc','w')
f.write(var)
f.close()

nmode = 10
span = spanlib.SpAn(var,npca=nmode)
eof = span.pca_eof()
pc = span.pca_pc()
ev = span.pca_ev()
rec= span.pca_rec(imode=-2)

print '+ ev[:3]:',ev[:3]
#print '+ eof sums:',[(thiseof.filled()**2).sum() for thiseof in eof]
print '+ max,min eof:',npy.max(eof[0]),npy.min(eof[0])
print '+ ev sum:',span.pca_ev(sum=True)
print '+ eof[0, :3]:', eof[0, :3]


f=cdms.open(__file__[:-2]+'nc','w')
f.write(var,extend=False)
f.write(eof)
f.write(pc)
f.write(ev)
f.write(rec)
f.close()

P.rc('font', size=9)

P.figure(figsize=(6,8))
P.subplots_adjust(hspace=.5, bottom=.03, top=.96)
for im in xrange(nmode):
	P.subplot(nmode,2,im*2+1)
	P.plot(eof[im].filled())
	P.title('EOF %i'%(im+1))
	P.subplot(nmode,2,im*2+2)
	P.plot(pc[im].filled())
	P.title('PC %i'%(im+1))
P.savefig(__file__[:-2]+'out.png')

P.figure(figsize=(6,8))
P.subplots_adjust()
P.subplot(211)
P.pcolor(var.filled()-var.filled().mean(0))
P.colorbar()
P.title('Original')
P.subplot(212)
P.pcolor(rec.filled())
P.colorbar()
P.title('Rec%i'%nmode)
P.savefig(__file__[:-2]+'outrec.png')


#P.show()



