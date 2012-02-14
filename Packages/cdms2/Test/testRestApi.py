import cdms2
import rlcompleter
import readline
import sys,os
readline.parse_and_bind("tab: complete")
restPath=None
test = "Luca"
test="Gavin"
#test="Luca-new"
datasetids=None
if test == "Luca":
    gateway = "esg-datanode.jpl.nasa.gov"
    #gateway = "test-datanode.jpl.nasa.gov"
    #restPath = "/esg-search/ws/rest/search"
    datasetids = "%(project).%(obs_structure).%(realm).%(instrument).%(time_frequency)"
    datasetids = "%(project).%(crappy)"
    datasetids=None
    fileids="%(datasetid).%(version).%(variable)_%(crap).nc"
    fileids="%(datasetid).%(variable)_%(crap)"
    mapping="%(project).%(variable)"
    #mapping="%(datasetid).%(version)"
    mapping=None
    stringType= False
elif test == "Gavin":
    gateway = "pcmdi9.llnl.gov"
    #datasetids = "%(project).%(product).%(institution).%(model).%(experiment).%(time_frequency).%(realm).%(MIPTable).%(ensemble)"
    #fileids="%(datasetid).%(version).%(variable)_%(MIPTable)_%(model)_%(experiment)_%(ensemble)_%(timespan).nc"
    #mapping="%(datasetid).%(variable)"
    #stringType= True
elif test== "Luca-new":
    gateway = "uv-cdat"
    datasetids = "%(project).%(obs_structure).%(realm).%(instrument_type).%(time_frequency)"
    fileids="%(datasetid).%(variable)_%(crap).nc"
    mapping="%(datasetid).%(version).%(variable)"
    stringType= True
    stringType= False
    
#mapping="%(project).%(variable)"
#fileids=None
#datasetids=None
#datasetids = "%(project).%(product).%(institute).%(model).%(experiment).%(time_frequency).%(realm).%(MIPTable).%(ensemble)"
fileids=None
#mapping="%(datasetid).%(variable)"
stringType= False
mapping=None
print gateway,mapping,datasetids,restPath
myGateway = cdms2.esgfConnection(gateway,mapping=mapping,datasetids=datasetids,fileids=fileids,restPath=restPath)
stringType=False
datasets =  myGateway.searchDatasets(stringType=stringType,variable="hus")
print datasets
#sys.exit()
if stringType:
    #print datasets
    pass
else:
    ## for f in dataset[-1].files:
    ##     print f
    
    print datasets[0].id
    print myGateway.keys()

    ## print datasets[0].files
    i=0
    for d in datasets:
        print i,d.id
        i+=1
        #search1 = d.search()
        #print len(search1)
    #print d.search(stringType=True)
    #sys.exit()
    #print datasets[0]
    #print datasets[0].files[:]
    i=5
    print "Looking at:",datasets[i].id,datasets[i].mapping
    search1 = datasets[i].search()#searchString=True)
    print search1
    print len(search1)
    search1.remap()
    print search1.mapped#["cmip5"]["output1"]["INM"]["inmcm4"]["amip"]["day"]["atmos"]["day"]["r1i1p1"].keys()
    print search1.mapped["CMIP5"]["output1"]["CNRM-CERFACS"]["CNRM-CM5"]["historical"]["mon"]["atmos"]["Amon"]["r4i1p1"]
    f1=search1[0]
    print f1
    print f1.OPENDAP
    f=cdms2.open(f1.OPENDAP)
    print f.listvariables()
#    print search1.mapped['obs4cmip5']['NASA-JPL']['AURA']['MLS']['mon']['files'][0].OPENDAP
    #print search1.mapped['obs4MIPs']['NASA-JPL']['AIRS']['mon']['files'][0].services
    #print search1.mapped['obs4MIPs']['NASA-JPL']['AIRS']['mon']['files'][0].OPENDAP
    #print search1.parent["MIPTable"]
        #search2 = datasets[i].search(variable="crap")
        #print len(search2)
    ## f=cdms2.open(datasets[1].files[0]["OPENDAP"])
    ## print f.variables.keys()
