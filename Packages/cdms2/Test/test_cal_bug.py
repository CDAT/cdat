import cdms2,cdtime
t=cdms2.createAxis([1,2,3,4])
t.designateTime()
t.setCalendar(cdtime.ClimCalendar)

print t.calendar

