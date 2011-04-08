import cdms2
import rlcompleter
import readline
import sys,os
readline.parse_and_bind("tab: complete")

myNode = cdms2.esgNodeConnection("esg-datanode.jpl.nasa.gov")
datasets =  myNode.search(stringType=False,variable="ta")
d0=datasets[0]
#print d0.files[:]
print d0.search(stringType=True)
print d0.url
print d0.title
print d0.timestamp
print d0.id
print d0.files
