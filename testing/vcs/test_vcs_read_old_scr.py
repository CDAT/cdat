import vcs
import sys

x=vcs.init()

boxes = x.listelements("boxfill")

x.scriptrun(sys.argv[1])

boxes2 = x.listelements("boxfill")

new = []

for e in boxes2:
    if not e in boxes:
        new.append(e)

print "new boxfills:",new
