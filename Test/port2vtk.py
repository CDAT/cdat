import vcs
x=vcs.init()
b=x.createboxfill()
#b.list()
print x.listelements("boxfill")
b=x.getboxfill("mercator")
#b.list()
print x.listelements("projection")

