import sys,cdms2,vcs
f=cdms2.open("Test/celine.nc")
s=f("data")
x=vcs.init()
x.scriptrun("Test/celine.json")
i=x.getisofill("celine")
x.plot(s,i)
#x.png("bad")
y=vcs.init()
b=vcs.createboxfill()
b.levels=i.levels
b.fillareacolors=i.fillareacolors
b.boxfill_type="custom"
y.plot(s,b)
b=vcs.createboxfill()
z=vcs.init()
#z.plot(s,b)

y.interact()

#raw_input("press enter")

