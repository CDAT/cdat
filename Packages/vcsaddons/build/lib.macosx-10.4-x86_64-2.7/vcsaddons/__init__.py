gms = {}
import histograms
import EzTemplate
import yxvsxfill
import continents

def createyxvsxfill(name=None,source='default',x=None,template=None):
    return yxvsxfill.Gyf(name,source=source,x=x,template=template)
def createhistogram(name=None,source='default',x=None,template=None):
    return histograms.Ghg(name,source=source,x=x,template=template)
def createusercontinents(name=None,source="default",x=None,template=None):
    return continents.Guc(name,source=source,x=x,template=template)
