# Module:      Python LATS parameter table module
#
# Copyright:	1997, Regents of the University of California
#		This software may not be distributed to others without
#		permission of the author.
#
# Author:      Bob Drach, Lawrence Livermore National Laboratory
#              drach@llnl.gov
#
# Version:     $Id$
#
# Revision History:
#
# $Log: latsParmTab.py,v $
# Revision 1.1.1.1  1998/09/01 22:17:59  drach
# Imported from ~williams/devel/cdat/python
#
# Revision 1.2  1998/04/07 16:52:26  drach
# - Added synonyms
#
# Revision 1.1.1.1  1998/01/21 17:54:59  drach
# Initial repository version
#
#
#

import string, slats

# Parse modes
NoMode=0
VariableMode=1
VertDimMode=2
CenterMode=3
QCMode=4

DuplicateEntryError = "Duplicate entry "
ParmTabOpenError = "Error opening parameter table"
InvalidParmTabError = "No section delimiter found; is this a parameter table?"
ParseError = "Error parsing parameter table"

# Parse a variable table entry

vartabId=0
vartabTitle=1
vartabUnits=2
vartabDatatype=3
vartabLeveltype=4
vartabScaleFac=5
vartabPrecision=6
vartabComment1=7
vartabComment2=8

def varParse(entry):
    id=0
    title='None'
    units='None'
    datatype=slats.LatsFloat
    leveltype=''
    scale_fac=-999
    precision=-999
    comment1=''
    comment2=''
    nentry = len(entry)
    if nentry>1: id = string.atoi(entry[1])
    if nentry>2: title = string.strip(entry[2])
    if nentry>3: units = string.strip(entry[3])
    if nentry>4: 
	if string.lower(string.strip(entry[4]))=='float':
	    datatype = slats.LatsFloat
	else:
	    datatype = slats.LatsInt
    if nentry>5: leveltype = string.strip(entry[5])
    if nentry>6: scale_fac = string.atoi(entry[6])
    if nentry>7: precision = string.atoi(entry[7])
    if nentry>8: comment1 = string.strip(entry[8])
    if nentry>9: comment2 = string.strip(entry[9])
    return [id,title,units,datatype,leveltype,scale_fac,precision,comment1,comment2]

# Parse a vertical dimension table entry

verttabDescription=0
verttabUnits=1
verttabVerticality=2
verttabPositive=3
verttabGribId=4
verttabGribP1=5
verttabGribP2=6
verttabGribP3=7

def vertDimParse(entry):
    description = 'None'
    units = 'None'
    verticality = slats.SingleLevel
    positive = slats.UpDirection
    grib_id = 0
    grib_p1 = 0
    grib_p2 = 0
    grib_p3 = 0
    nentry = len(entry)
    if nentry>1: description = string.strip(entry[1])
    if nentry>2: units = string.strip(entry[2])
    if nentry>3:
	if string.lower(string.strip(entry[3]))=='single':
	    verticality = slats.SingleLevel
	else:
	    verticality = slats.MultiLevel
    if nentry>4:
	if string.lower(string.strip(entry[4]))=='up':
	    positive = slats.UpDirection
	else:
	    positive = slats.DownDirection
    if nentry>5: grib_id = string.atoi(entry[5])
    if nentry>6: grib_p1 = string.atoi(entry[6])
    if nentry>7: grib_p2 = string.atoi(entry[7])
    if nentry>8: grib_p3 = string.atoi(entry[8])
    return [description, units, verticality, positive, grib_id, grib_p1, grib_p2, grib_p3]

# Parse a center table entry

def centerParse(entry):
    grib_process = 0
    grib_center = 0
    grib_subcenter = 0
    nentry = len(entry)
    if nentry>1: grib_process = string.atoi(entry[1])
    if nentry>2: grib_center = string.atoi(entry[2])
    if nentry>3: grib_subcenter = string.atoi(entry[3])
    return [grib_process, grib_center, grib_subcenter]

# Parse a quality control table entry

def qcParse(entry):
    mean = 0.0
    std = 0.0
    tolerance = 0.0
    range = 0.0
    rangetol = 0.0
    nentry = len(entry)
    if nentry>3: mean = string.atof(entry[3])
    if nentry>4: std = string.atof(entry[4])
    if nentry>5: tolerance = string.atof(entry[5])
    if nentry>6: range = string.atof(entry[6])
    if nentry>7: rangetol = string.atof(entry[7])
    return [mean,std,tolerance,range,rangetol]
    
# Parameter table
#   vars:      variable dictionary
#   verts:     vertical dimension dictionary
#   centers:   modeling center dictionary
#   qcs:       quality control marks dictionary
#   path:      ASCII parameter file path, or None if not persistent

class ParmTab:

    def __init__(self, vars=None, verts=None, centers=None, qcs=None):
	if not vars: self.vars = {}
	if not verts: self.verts = {}
	if not centers: self.centers = {}
	if not qcs: self.qcs = {}
	self.path = None

    # Read a parameter table from a file 'path'

    def read(self, path):
	tabs = {VariableMode:self.vars, VertDimMode:self.verts, CenterMode:self.centers, QCMode:self.qcs}
	mode=NoMode
	linenum=0
	try:
	    lines = open(path,'r').readlines()
	except IOError, msg:
	    raise ParmTabOpenError, msg[1]+' '+path
	    
	for line in lines:
	    linenum = linenum+1
	    token = string.split(line)
	    if len(token)>0:
		token0 = string.lower(token[0])

		# If variable section
		if token0=='#!variable':
		    mode = VariableMode

		# else if vertical dimension section
		elif token0=='#!vert':
		    mode = VertDimMode

		# else if modeling center section
		elif token0=='#!center':
		    mode = CenterMode

		# else if quality control section
		elif token0=='#!qc':
		    mode = QCMode

		# else if a section delimiter
		elif token0=='#!' and len(token)>1:
		    token1=string.lower(token[1])
		    if token1=='variable':
			mode = VariableMode
		    elif token1=='vert':
			mode = VertDimMode
		    elif token1=='center':
			mode = CenterMode
		    elif token1=='qc':
			mode = QCMode

		# else if not a comment, parse according to the current mode
		elif token0[0] != '#':
                    try:
                        if mode==NoMode:
                            raise InvalidParmTabError, path
                        entry=string.splitfields(line,'|')
                        if mode==VariableMode:
                            cleanEntry=varParse(entry)
                            tabkey = string.strip(entry[0])
                        elif mode==VertDimMode:
                            cleanEntry = vertDimParse(entry)
                            tabkey = string.strip(entry[0])
                        elif mode==CenterMode:
                            cleanEntry = centerParse(entry)
                            tabkey = string.strip(entry[0])
                        elif mode==QCMode:
                            cleanEntry = qcParse(entry)
                            if string.strip(entry[2])=='':
                                entry[2]='0.0'
                            tabkey = (string.strip(entry[0]),string.strip(entry[1]),string.atof(entry[2]))
                        tab = tabs[mode]
                        if tab.has_key(tabkey):
                            print 'Warning: Duplicate entry ',tabkey
                        tab[tabkey] = cleanEntry
                    except:
                        if entry[2]=='': print 'null entry'
                        raise ParseError, '%s, line %d: %s'%(path,linenum,line)
	self.path = path

    # Write a parameter table to a file 'path'

    def write(self, path):
	parmfile = open(path,'w')

	# Variable section
	
	varkeys = self.vars.keys()
	varkeys.sort()
	parmfile.write("#!variable\n")
	for name in varkeys:
	    entry = self.vars[name]
	    id = entry[0]
	    title = entry[1]
	    units = entry[2]
	    if entry[3]==slats.LatsFloat:
		datatype="float"
	    else:
		datatype="int"
	    leveltype = entry[4]
	    scale_fac = entry[5]
	    precision = entry[6]
	    comment1 = entry[7]
	    comment2 = entry[8]
	    parmfile.write("%s\t |%d\t |%s |%s |%s |%s |%d |%d |%s |%s |\n"%(name,id,title,units,datatype,leveltype,scale_fac,precision,comment1,comment2))
	
	# Vertical dimension section

	vertkeys = self.verts.keys()
	vertkeys.sort()
	parmfile.write("#!vert\n")
	for leveltype in vertkeys:
	    entry = self.verts[leveltype]
	    description = entry[0]
	    units = entry[1]
	    if entry[2]==slats.SingleLevel:
		verticality = "single"
	    else:
		verticality = "multi"
	    if entry[3]==slats.UpDirection:
		positive = "up"
	    else:
		positive = "down"
	    grib_id = entry[4]
	    grib_p1 = entry[5]
	    grib_p2 = entry[6]
	    grib_p3 = entry[7]
	    parmfile.write("%s\t |%s |%s |%s |%s |%d |%d |%d |%d |\n"%(leveltype,description,units,verticality,positive,grib_id,grib_p1,grib_p2,grib_p3))
	
	# Center section

	centerkeys = self.centers.keys()
	centerkeys.sort()
	parmfile.write("#!center\n")
	for center in centerkeys:
	    entry = self.centers[center]
	    grib_process = entry[0]
	    grib_center = entry[1]
	    grib_subcenter = entry[2]
	    parmfile.write("%s\t |%d\t |%d\t |%d\t |\n"%(center,grib_process,grib_center,grib_subcenter))

	# Quality control section

	qckeys = self.qcs.keys()
	qckeys.sort()
	parmfile.write("#!qc\n")
	for (variable,leveltype,level) in qckeys:
	    entry = self.qcs[(variable,leveltype,level)]
	    mean = entry[0]
	    std = entry[1]
	    tolerance = entry[2]
	    range = entry[3]
	    rangetol = entry[4]
	    parmfile.write("%s\t |%s\t |%f\t |%f |%f |%f |%f |%f |\n"%(variable,leveltype,level,mean,std,tolerance,range,rangetol))

	parmfile.close()
	self.path = path

# Synonyms

ParameterTable = ParmTab
