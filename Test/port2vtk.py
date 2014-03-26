import vcs
x=vcs.init()
b=x.createboxfill()
#b.list()
print x.listelements("boxfill")
b=x.getboxfill("mercator")
#b.list()
print x.listelements("projection")
f=x.getfillarea("GEN_seaice_7")
print f.list()

