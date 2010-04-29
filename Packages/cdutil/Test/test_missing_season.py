import cdms2,cdutil,sys,MV2,numpy,os

f=cdms2.open(os.path.join(cdms2.__path__[0],'..','..','..','..','sample_data','clt.nc'))
s=f("clt")
cdutil.setTimeBoundsMonthly(s)

print 'Getting JJA, which should be inexistant in data'

if cdutil.JJA(s[:5]) is not None:
    raise RuntimeError, "data w/o season did not return None"

## Create a year worth of data w/o JJA
s1 = s[:5]
s2 = s[8:12]

s3 = MV2.concatenate((s1,s2))
t = MV2.concatenate((s1.getTime()[:],s2.getTime()[:]))
t = cdms2.createAxis(t,id='time')
t.units=s.getTime().units
t.designateTime()

s3.setAxis(0,t)
cdutil.setTimeBoundsMonthly(s3)
if cdutil.JJA(s3) is not None:
    raise RuntimeError, "data w/o season did not return None"
if cdutil.JJA.departures(s3) is not None:
    raise RuntimeError, "data w/o season did not return None for dep"
if cdutil.JJA.climatology(s3) is not None:
    raise RuntimeError, "data w/o season did not return None for clim"

# Now gets seasonal cycle, should have JJA all missing
print 'Testing seasonal cycle on 1 year data w/o JJA should work'
a = cdutil.SEASONALCYCLE(s3)
print a.shape,a.getTime()[:],a.getTime().getBounds()
if a.shape!=(4,46,72):
    raise "Error returned data with wrong shape"
if not numpy.equal(a.getTime()[:],[  0.,   3.,   9.,  12.]).all():
    raise "Error time are not valid"

if not numpy.equal(a.getTime().getBounds()[:],[[ -1.,   2.], [  2.,   5.], [  8.,  11.], [ 11.,  14.]]).all():
    raise "Error bound time are not valid"
d = cdutil.SEASONALCYCLE.departures(s3)
c = cdutil.SEASONALCYCLE.climatology(s3)
## Create 2 year worth of data w/o JJA
s1 = s[:5]
s2 = s[8:17]
s3 = s[20:24]

s4 = MV2.concatenate((s1,s2))
s5 = MV2.concatenate((s4,s3))
t = MV2.concatenate((s1.getTime()[:],s2.getTime()[:]))
t2 = MV2.concatenate((t,s3.getTime()[:]))
t = cdms2.createAxis(t2,id='time')
t.units=s.getTime().units
t.designateTime()

s5.setAxis(0,t)
cdutil.setTimeBoundsMonthly(s5)
d = cdutil.SEASONALCYCLE.departures(s5)
c = cdutil.SEASONALCYCLE.climatology(s5)
if cdutil.JJA(s5) is not None:
    raise RuntimeError, "data w/o season did not return None"
# Now gets seasonal cycle, should have JJA all missing
print 'Testing seasonal cycle on 2 years data w/o JJA should work'
a = cdutil.SEASONALCYCLE(s5)
if a.shape!=(7,46,72):
    raise "Error returned data with wrong shape"

## Create 2 years worth of data w/o 1st JJA
s1 = s[:5]
s2 = s[8:24]

s3 = MV2.concatenate((s1,s2))
t = MV2.concatenate((s1.getTime()[:],s2.getTime()[:]))
t = cdms2.createAxis(t,id='time')
t.units=s.getTime().units
t.designateTime()

s3.setAxis(0,t)
cdutil.setTimeBoundsMonthly(s3)
a = cdutil.JJA(s3)
if a is None:
    raise RuntimeError, "data w/o 1st season returned None"

# Now gets seasonal cycle, should have JJA all missing
print 'Testing seasonal cycle on 2 years data w/o 1st JJA should work'
a = cdutil.SEASONALCYCLE(s3)
d = cdutil.SEASONALCYCLE.departures(s3)
c = cdutil.SEASONALCYCLE.climatology(s3)
if a.shape!=(8,46,72):
    raise "Error returned data with wrong shape"
if not numpy.equal(a.getTime()[:],[  0.,   3.,   9.,  12., 15.,18.,21,24]).all():
    raise "Error time are not valid"

if not numpy.equal(a.getTime().getBounds()[:],[[ -1.,   2.], [  2.,   5.], [  8.,  11.], [ 11.,  14.], [ 14.,  17.], [ 17.,  20.], [ 20.,  23.], [ 23.,  26.]]).all():
    raise "Error bound time are not valid"


print " Ok we test the filling part"
print " this should add month '6' as all missing"
b= cdutil.times.insert_monthly_seasons(a,['JJA',])
if b.shape!=(9,46,72):
    raise "Error returned data with wrong shape"
if not numpy.equal(b.getTime()[:],[  0.,   3.,6,   9.,  12., 15.,18.,21,24]).all():
    raise "Error time are not valid"

if not numpy.equal(b.getTime().getBounds()[:],[[ -1.,   2.], [  2.,   5.], [5,8],[  8.,  11.], [ 11.,  14.], [ 14.,  17.], [ 17.,  20.], [ 20.,  23.], [ 23.,  26.]]).all():
    raise "Error bound time are not valid"

if not b[2].count() == 0:
    raise "Error not all times missing in added spot"





# Now gets seasonal cycle, should have JJA all missing
print 'Testing seasonal cycle on 2 years data w/o JJA should work'
a = cdutil.SEASONALCYCLE(s5)
if a.shape!=(7,46,72):
    raise "Error returned data with wrong shape"

## Creates data with big gap in years
s1 = s[:15]
s2 = s[68:]
s3 = MV2.concatenate((s1,s2))
t = MV2.concatenate((s1.getTime()[:],s2.getTime()[:]))
t = cdms2.createAxis(t,id='time')
t.units=s.getTime().units
t.designateTime()

s3.setAxis(0,t)
cdutil.setTimeBoundsMonthly(s3)
a = cdutil.JJA(s3)
if a is None:
    raise RuntimeError, "data with gap returned None"

# Now gets seasonal cycle, should have JJA all missing
print 'Testing seasonal cycle on data with years of gap should work'
a = cdutil.SEASONALCYCLE(s3)
d = cdutil.SEASONALCYCLE.departures(s3)
c = cdutil.SEASONALCYCLE.climatology(s3)
print s3.shape,a.shape
print a.getTime()[:]
if a.shape!=(24,46,72):
    raise "Error returned data with wrong shape"


print " Ok we test the filling part"
print " this should add month '6' as all missing"
b= cdutil.times.insert_monthly_seasons(a,cdutil.times.SEASONALCYCLE.seasons)
print b.shape
if b.shape!=(41,46,72):
    raise "Error returned data with wrong shape"
print b.getTime()[:]


print cdutil.SEASONALCYCLE.departures(s3).shape,a.shape,cdutil.SEASONALCYCLE.climatology(s3).shape






## c = cdutil.SEASONALCYCLE.climatology(s5)
## print d.shape
