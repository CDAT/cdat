import vcs
import sys

json = sys.argv[1]

for tp in ["boxfill","meshfill","isofill","isoline","template","oned"]:
    b4 = vcs.listelements(tp)
    assert (not "Charles.Doutriaux" in b4)
vcs.scriptrun(json)
for tp in ["boxfill","meshfill","isofill","isoline","template","oned"]:
    after = vcs.listelements(tp)
    assert ("Charles.Doutriaux" in after)
    gm = vcs.elements[tp]["Charles.Doutriaux"]



