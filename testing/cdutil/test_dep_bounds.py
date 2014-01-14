import cdtime
import cdms2
import MV2
import numpy
import cdutil

def mk_time(offset=0,len=120,units="months since 1800"):
    t=cdms2.createAxis(numpy.arange(offset,offset+len))
    t.designateTime()
    t.id='time'
    t.units=units
    data= MV2.array(numpy.random.random((len)))
    data.setAxis(0,t)
    cdutil.setTimeBoundsMonthly(t)
    return data,t,t.asComponentTime()

def change_units(t,units):
    t.toRelativeTime(units)

def testit(offset=0,midunits="months since 1801",units="months since 1800"):
    data,t,tc=mk_time(offset,units=units)
    units = t.units
    t1,t2 = tc[0],tc[-1]
    dep = cdutil.times.ANNUALCYCLE.departures(data)
    tc = dep.getTime().asComponentTime()
    print t1,t2,tc[0],tc[-1]
    assert tc[0] == t1
    assert tc[-1] == t2
    assert data.getTime().units == units
    assert dep.getTime().units == units

testit()
testit(-200)
testit(-60)
testit(10)



