#!/usr/bin/env python
# Adapted for numpy/ma/cdms2 by convertcdms.py

import cdms2 as cdms
import sys
import os
import numpy

file=sys.argv[1]
if sys.argv[1]=='-h':
    print 'Usage: cddump [-d] [-v] [-h] <control-file-path> [<data-file-path>]'
    print '-d option prints all dimensions values'
    print '-v option prints all variables values'
    print '-h option prints help message'
    sys.exit()
    
if file in [ '-d', '-h', '-v'] : file=sys.argv[2]


fnm=os.path.split(file)[1]
f=cdms.open(file)

dims=f.listdimension()
vars=f.variables.keys()
vars_dims=[]
icont=1
while icont:
    icont=0
    for i in range(len(vars)):
        v=vars[i]
        if v in dims:
            icont=1
            vars_dims.append(vars.pop(i))
            break

vars=vars_dims+vars
print 'cddump: '+file
print 'netcdf '+fnm+' {'
print
print 'dimensions:'
for d in dims:
    ax=f.getAxis(d)
    print '\t'+d+' = '+str(len(ax))+';',
    if ax.units!='':
        print '\t// units ="'+ax.units+'";'
    else:
        print
print
print 'variables:'
for v in vars:
    V=f[v]
    t=V.dtype.char
    if t=='d':
        t='double'
    elif t=='f':
        t='float'
    elif t=='b':
        t='byte'
    elif t=='h':
        t='short'
    elif t=='l':
        t='int'
    elif t=='q':
        t='long'
    elif t=='D':
        t='Complex'
    axs=V.getAxisIds()
    d='('
    for a in axs:
        d+=a+', '
    d=d[:-2]+');'
    print '\t'+t+' '+V.id+d
    for a in V.listattributes():
        print '\t\t'+V.id+':'+a+'\t\t= "'+repr(getattr(V,a))+'";'
print '  \\\\global attributes'
for a in f.attributes.keys():
    print '\t:'+a+'\t= "'+repr(getattr(f,a))+'";'
if sys.argv[1]=='-d':
    print 'data:'
    for d in dims:
        print '\t'+d+' = ',
        a=f.getAxis(d)
        for v in a[:]:
            print '%.2f,' % v,
        print ';'
if sys.argv[1]=='-v':
    print 'data:'
    for v in vars:
        print '\t'+v+' = ',
        a=cdms.MV2.ravel(f(v)).filled()
        for val in a[:]:
            print '%.4g,' % val,
        print ';'
print '}'
