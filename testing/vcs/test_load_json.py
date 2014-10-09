import vcs
import sys

json = sys.argv[1]

for tp in ["boxfill","meshfill","isofill","isoline","template","1d"]:
    b4 = vcs.listelements(tp)
    assert (not "Charles.Doutriaux" in b4)
vcs.scriptrun(json)
for tp in ["boxfill","meshfill","isofill","isoline","template","1d"]:
    after = vcs.listelements(tp)
    assert ("Charles.Doutriaux" in after)
    gm = vcs.elements[tp]["Charles.Doutriaux"]



