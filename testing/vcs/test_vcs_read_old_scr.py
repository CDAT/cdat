import vcs
import sys

x=vcs.init()

boxes = x.listelements("boxfill")
isofs = x.listelements("isofill")

x.scriptrun(sys.argv[1])

boxes2 = x.listelements("boxfill")
isofs2 = x.listelements("isofill")

tested = [(boxes,boxes2),(isofs,isofs2)]
tested = [(isofs,isofs2)]
for gm,gm2 in [(boxes,boxes2),(isofs,isofs2)]:
    new = []
    for e in gm2:
        if not e in gm:
            new.append(e)

#    print "new ",new


gm = x.getisofill("pr_time_lat_1")
gm.list()
"""
n=linear,xticlabels#1=*,xticlabels#2=*,yticlabels#1=*,
   yticlabels#2=*,
   datawc(-3,-3,3,3),
   missing=1e+20,
   range
   (id=1,level1=0,level2=0.001,Tf=AMIP30)
   (id=2,level1=0.001,level2=0.002,Tf=AMIP29)
   (id=3,level1=0.002,level2=0.003,Tf=AMIP27)
   (id=4,level1=0.003,level2=0.004,Tf=AMIP25)
   (id=5,level1=0.004,level2=0.005,Tf=AMIP23)
   (id=6,level1=0.005,level2=0.006,Tf=AMIP21)
   (id=7,level1=0.006,level2=0.007,Tf=AMIP19)
   (id=8,level1=0.007,level2=0.008,Tf=AMIP17)  )
"""
