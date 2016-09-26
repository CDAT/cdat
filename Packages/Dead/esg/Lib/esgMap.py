#!/usr/bin/env python
# Adapted for numpy/ma/cdms2 by convertcdms.py
"""
Mapping between ESG, CDMS, and other schemas.
"""

import esg, cdms2 as cdms, cdtime, os

def toEsg(cddataset, id, paramlistParent, generatedBy, ofType=None, isPartOf=None, convention=None, timeCoverage=None, spaceCoverage=None, format=None, calendar=None, taxis=None, xaxis=None, yaxis=None, zaxis=None, fileMapper=None, mapperArg=None):
    """
    Translate a cdms.cdmsNode.DatasetNode into an ESG Metadata node. The result node
    contains a dataset, parameterlist, and parameters.

    cddataset:       input cdms.cdmsNode.DatasetNode to translate
    id:              identifier of the resulting Dataset node.
    paramlistParent: string identifier of the parent activity that owns the parameter list.
    generatedBy:     string identifier of the activity that generated the dataset.
    ofType:	     string distinguishing characteristic within an investigation, e.g. "monthly means"
    isPartOf:	     Dataset that contains this dataset, if any.
    convention:	     String metadata convention ID, e.g. "CF-1.0"
    timeCoverage:    esg.TimeRegion instance. Overrides taxis.
    spaceCoverage:   esg.SpaceRegion instance. Overrides xaxis, yaxis, and zaxis.
    format:	     esg.Format instance.
    calendar:	     CDMS calendar.
    taxis:	     name of time axis.
    xaxis:	     name of X axis.
    yaxis:	     name of Y axis.
    zaxis:	     name of Z axis.
    fileMapper:      function mapping datafile path to logical fileid and name. fileMapper has the form
                     fileid,filename = fileMapper(path, optarg=mapperArg), where path may be relative or absolute.
                     If None, don't translate filenames.
    mapperArg:       Optional argument passed to fileMapper
    """

    # Create initial tree: Metadata, Dataset, and ParameterList
    esgmetadata = esg.Metadata()
    esgdataset = esg.Dataset(id, esg.ObjRef(generatedBy), ofType=ofType, isPartOf=isPartOf, convention=convention)
    esgmetadata.setDataset(esgdataset)
    parmlistid = paramlistParent+'.parameters'
    esgparmlist = esg.ParameterList(parmlistid)
    esgdataset.addParameterList(esg.ObjRef(esgparmlist.id))
    esgmetadata.setParameterList(esgparmlist)

    # Create a CDMS Dataset from the DatasetNode
    direc = cddataset.getExternalAttr('directory')
    if direc is not None and os.path.isabs(direc):
        dpath = direc                   # If .directory is absolute, use it as datapath for bounds, etc.
    else:
        dpath = None
    cdmsdataset = cdms.dataset.Dataset('<None>','r',cddataset,datapath=dpath)

    # Generate Parameters
    for var in cdmsdataset.variables.values():
        paramid = paramlistParent+'.param.'+var.id
        desc = var.attributes.get('long_name',None)
        parm = esg.Parameter(paramid, var.id, description=desc)
        esgmetadata.setParameter(parm)
        esgparmlist.addParamRef(esg.ObjRef(parm.id))

    # Determine time coverage from first variable having a time dimension
    if timeCoverage is not None:
        esgdataset.setTimeCoverage(timeCoverage)
    else:
        for var in cdmsdataset.variables.values():
            if taxis is None:
                timevar = var.getTime()
            else:
                timevar = cdmsdataset[taxis]
            if timevar is not None:
                if calendar is None:
                    calendar = timevar.getCalendar()
                time0 = cdtime.reltime(timevar[0], timevar.units).tocomp(calendar)
                timen = cdtime.reltime(timevar[-1], timevar.units).tocomp(calendar)
                timeregion = esg.TimeRegion(start=str(time0), stop=str(timen))
                if calendar is not None:
                    timeregion.calendar = cdms.coord.calendarToTag[calendar]
                esgdataset.setTimeCoverage(timeregion)
                break

    # Determine spatial coverage from first variable(s) having x|y|z dimension
    xrange = yrange = zrange = None
    for var in cdmsdataset.variables.values():
        if xaxis is None:
            xvar = var.getLongitude()
        else:
            xvar = cdmsdataset[xaxis]
        if yaxis is None:
            yvar = var.getLatitude()
        else:
            yvar = cdmsdataset[yaxis]
        if zaxis is None:
            zvar = var.getLevel()
        else:
            zvar = cdmsdataset[zaxis]
        if xrange is None and xvar is not None:
            try:
                xbounds = xvar.getBounds()
                startx = min(xbounds[0,0], xbounds[0,1])
                stopx = max(xbounds[-1,0], xbounds[-1,1])
                xrange = (startx, stopx, xvar.units)
            except:
                xrange = (min(xvar[0],xvar[-1]), max(xvar[0],xvar[-1]), xvar.units)
        if yrange is None and yvar is not None:
            try:
                ybounds = yvar.getBounds()
                starty = min(ybounds[0,0], ybounds[0,1])
                stopy = max(ybounds[-1,0], ybounds[-1,1])
                yrange = (starty, stopy, yvar.units)
            except:
                yrange = (min(yvar[0],yvar[-1]), max(yvar[0],yvar[-1]), yvar.units)
        if zrange is None and zvar is not None:
            zrange = (min(zvar[0],zvar[-1]), max(zvar[0],zvar[-1]), zvar.units)
        if xrange is not None and yrange is not None and zrange is not None:
            break

    spaceregion = esg.SpaceRegion(xrange=xrange, yrange=yrange, zrange=zrange)
    if spaceCoverage is None:
        esgdataset.setSpaceCoverage(spaceregion)
    else:
        esgdataset.setSpaceCoverage(spaceCoverage)

    if format is not None:
        esgdataset.format = format

    # Set metadata convention if present
    if hasattr(cdmsdataset, 'Conventions'):
        esgdataset.convention = cdmsdataset.Conventions

    # Add files to output dataset if fileMapper is set
    if fileMapper is not None:
        paths = cdmsdataset.getPaths()
        for path in paths:
            fileid, filename = fileMapper(path, optarg=mapperArg)
            fileobj = esg.File(fileid, isPartOf=esg.ObjRef(id), name=filename)
            esgmetadata.setFile(fileobj)

    return esgmetadata


