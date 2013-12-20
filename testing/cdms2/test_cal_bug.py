import cdms2,cdtime
t=cdms2.createAxis([1,2,3,4])
t.designateTime()
t.setCalendar(cdtime.ClimCalendar)

try:
    t.setCalendar(3421)
except cdms2.CDMSError:
    print "Excepted as expected"
else:
    raise "Unexpected error"
