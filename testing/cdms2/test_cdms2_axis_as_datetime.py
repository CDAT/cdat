import cdms2,datetime

ax = cdms2.createAxis([10.813224335543,],id="time")
ax.units="seconds since 2014-10-06 10:12:13"
ax.designateTime()

dt = ax.asdatetime()

assert(dt[0]==datetime.datetime(2014, 10, 6, 10, 12, 23, 813))
