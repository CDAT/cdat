import cdms2
import rlcompleter
import readline
import sys,os
readline.parse_and_bind("tab: complete")

myNode = cdms2.esgNodeConnection("esg-datanode.jpl.nasa.gov")
#datasets =  myNode.search(stringType=True,variable="ta")
datasets =  myNode.search(stringType=False,variable="ta")
print datasets
for d in datasets[::-1]:
    print "---------- Dataset  ----------" 
    print d
print d.files
for f in d.files:
    print f
    
F = cdms2.open(f["OPENDAP"])

print F.variables.keys()

ta=F['ta']
print ta.shape
ta=ta[0,0]
import vcs
x=vcs.init()
x.plot(ta)
raw_input("Press enter")
