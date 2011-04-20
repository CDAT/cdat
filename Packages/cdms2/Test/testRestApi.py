import cdms2
import rlcompleter
import readline
import sys,os
readline.parse_and_bind("tab: complete")
restPath=None
test = "Luca"
test="Gavin"
#test="Luca-new"
if test == "Luca":
    gateway = "esg-datanode.jpl.nasa.gov"
    #gateway = "test-datanode.jpl.nasa.gov"
    restPath = "/esg-search2/ws/rest/search"
    datasetids = "%(project).%(obs_structure).%(realm).%(instrument_type).%(time_frequency)"
    fileids="%(datasetid).%(version).%(variable)_%(crap).nc"
    mapping="%(datasetid).%(version).%(variable)"
    #mapping="%(datasetid).%(version)"
    stringType= False
elif test == "Gavin":
    gateway = "esgf-node1.llnl.gov"
    datasetids = "%(project).%(product).%(institution).%(model).%(experiment).%(time_frequency).%(realm).%(MIPTable).%(ensemble)"
    fileids="%(datasetid).%(version).%(variable)_%(MIPTable)_%(model)_%(experiment)_%(ensemble)_%(timespan).nc"
    mapping="%(datasetid).%(variable)"
    stringType= False
    #stringType= True
elif test== "Luca-new":
    gateway = "uv-cdat"
    datasetids = "%(project).%(obs_structure).%(realm).%(instrument_type).%(time_frequency)"
    fileids="%(datasetid).%(variable)_%(crap).nc"
    mapping="%(datasetid).%(version).%(variable)"
    stringType= True
    stringType= False
    
myGateway = cdms2.esgfConnection(gateway,mapping=mapping,datasetids=datasetids,fileids=fileids,restPath=restPath)

datasets =  myGateway.search(stringType=stringType,variable="ta")
#print datasets[0]
if stringType:
    print datasets
else:
    ## for f in dataset[-1].files:
    ##     print f

    print datasets[0].id
    print myGateway.keys()

    ## print datasets[0].files
    for d in datasets:
        print d.id,len(d.files)
    #print datasets[0]
    #print datasets[0].files[:]
    if test[:4]=="Luca":
        print datasets[0].mapping.keys()
        print datasets[0].mapped#["obs4cmip5"]["NASA-JPL"]["AQUA"]["AIRS"]["mon"]
    elif test=="Gavin":
        i=0
        print datasets[i].id
        #print datasets[i].search(variable="ta",stringType=True)
        sys.exit()
        print datasets[0].mapped["cmip5"]["output1"]["INM"]["inmcm4"]["1pctCO2"]["mon"]["atmos"]["Amon"]["r1i1p1"].keys()
    ## f=cdms2.open(datasets[1].files[0]["OPENDAP"])
    ## print f.variables.keys()
