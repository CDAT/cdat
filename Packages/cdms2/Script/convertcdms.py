#!/usr/bin/env python

# Convert to cdms2 / numpy modules

import sys
import os
import re
import getopt
import glob
from numpy.oldnumeric.alter_code1 import fixtypechars, fixistesting, changeimports, replaceattr, replaceother, convertsrc

usage = """Usage:
    convertcdms.py [options] path

      Convert scripts that use the Numeric, MA, cdms, and regrid modules to numpy, ma, cdms2, and regrid2.

    -or-
    
    convertcdms.py [options] -r direc

      Convert all Numeric/MA/cdms Python scripts and C source code in directory 'direc' to numpy/ma/cdms2.

      Use this form to convert C source code.
    
Arguments:

    path: A .py file to convert
    direc: A directory: convert all .py, .c, and .h files

Options:

    -a:           Disable 'aggressive' conversion (see Note 1)
    -c:           Clobber the original file(s). The default is to move the original to <name>.orig
    -h:           Print a help message
    -n:           Do not write '# Adapted by...' comment at the top of file. 
    -r:           Recursively convert all .py and .c/.h files in directory direc
    -s direc:     Skip the directory. This option may be used more than once.
                    For example, '-s .svn' skips all subversion subdirectories.

Notes:

    1) The -a option turns off the following translations:

        - MA.Float => Numeric.Float, similarly for MA.Int, MA.NewAxis
        - MA.Numeric => Numeric
        - The 'typecode=' argument in MA functions is changed to 'dtype='
        - XX.mask() => XX.mask
        - XX.mask is None => ((XX.mask is None) or (XX.mask is MV2.nomask))
        - A keyword argument 'axis=0' is added to MA.sum, MA.average, MA.product, and MA.repeat

    2) By default the following translations are made:

        - import cdms => import cdms2 as cdms
        - import regrid => import regrid2 as regrid
        - import MV => import MV2 as MV
        - import Numeric => import numpy as Numeric
        - import MA => import numpy.ma as MA
        - import cdms.MV => import cdms2.MV2
        - from cdms import XX => from cdms2 import XX (similarly for regrid, MV, MA, and Numeric)
        - from cdms.XX import YY => from cdms2.XX import YY (similarly for regrid, MV, MA, and Numeric)
        - import cdms as XX => import cdms2 as XX (similarly for regrid, MV, MA, and Numeric)
        - import cdms.XX => import cdms2.XX (similarly for regrid, MV, MA, and Numeric)
        - import XX, cdms, YY => import XX, cdms2 as cdms, YY (similarly for regrid, MV, MA, and Numeric)
        - the translations in Note (1)
        - the translations in numpy.oldnumeric.alter_code1. This module is used for most Numeric and MA-related translations.
"""

isnottest_re = {}
_types = ['float', 'int', 'complex', 'ArrayType', 'FloatType',
          'IntType', 'ComplexType']
for name in _types:
    _astr = r'type\s*[(]([^)]*)[)]\s+(?:is not|!=)\s+(.*?%s)'%name
    isnottest_re[name] = re.compile(_astr)

def fixisnottesting(astr):
    for name in _types:
        astr = isnottest_re[name].sub(r'(not isinstance(\1, \2))', astr)
    return astr

def fromstr(filestr):
    filestr = fixtypechars(filestr)
    filestr = fixisnottesting(filestr)
    filestr = fixistesting(filestr)
    filestr, fromall1 = changeimports(filestr, 'Numeric', 'numpy')
    filestr, fromall1 = changeimports(filestr, 'multiarray','numpy')
    filestr, fromall1 = changeimports(filestr, 'umath', 'numpy')
    filestr, fromall1 = changeimports(filestr, 'Precision', 'numpy.oldnumeric.precision')
    filestr, fromall1 = changeimports(filestr, 'UserArray', 'numpy.oldnumeric.user_array')
    filestr, fromall1 = changeimports(filestr, 'ArrayPrinter', 'numpy.oldnumeric.array_printer')
    filestr, fromall2 = changeimports(filestr, 'numerix', 'numpy.oldnumeric')
    filestr, fromall3 = changeimports(filestr, 'scipy_base', 'numpy.oldnumeric')
    filestr, fromall3 = changeimports(filestr, 'Matrix', 'numpy.oldnumeric.matrix')
    filestr, fromall3 = changeimports(filestr, 'MLab', 'numpy.oldnumeric.mlab')
    filestr, fromall3 = changeimports(filestr, 'LinearAlgebra', 'numpy.oldnumeric.linear_algebra')
    filestr, fromall3 = changeimports(filestr, 'RNG', 'numpy.oldnumeric.rng')
    filestr, fromall3 = changeimports(filestr, 'RNG.Statistics', 'numpy.oldnumeric.rng_stats')
    filestr, fromall3 = changeimports(filestr, 'RandomArray', 'numpy.oldnumeric.random_array')
    filestr, fromall3 = changeimports(filestr, 'FFT', 'numpy.fft')
    filestr, fromall3 = changeimports(filestr, 'MA', 'numpy.ma')
    filestr, fromall3 = changeimports(filestr, 'numpy.oldnumeric', 'numpy')
    fromall = fromall1 or fromall2 or fromall3
    filestr = replaceattr(filestr)
    filestr = replaceother(filestr)
    return filestr

def change_cdimports(fstr, name, newname):

    # from name import xyz => from newname import xyz
    fstr = re.sub(r'(from\s+)%s(\s+import)'%name, r'\1%s\2'%newname, fstr)

    # from name.submodule import xyz => from newname.submodule import xyz
    fstr = re.sub(r'(from\s+)%s(\..*\s+import)'%name, r'\1%s\2'%newname, fstr)

    # import name as alias => import newname as aliase
    fstr = re.sub(r'(import.*[, ])%s(\s+as\s+)'%name, r'\1%s\2'%newname, fstr)

    # import name => import newname as name
    fstr = re.sub(r'(import \s*)%s(?P<ending>[ ,\n\r]|$)'%name, r'\1%s as %s\g<ending>'%(newname, name), fstr)
    
    # import ..., name => import ..., newname as name
    fstr = re.sub(r'import (.*),([ ]*)%s([ ,\n\r]|$)'%name, r'import \1,\2%s as %s\3'%(newname, name), fstr)
    
    # import name.submodule => import newname.submodule
    fstr = re.sub(r'(import.*[, ])%s(\.)'%name, r'\1%s\2'%newname, fstr)

    return fstr

def test():

    teststr = """
import x,y, cdms, z
import x,  y as Y,  cdms as CD
import x, cdms
import cdms
import cdms as CD
import cdms.MV
import cdms.MV as MV2
from cdms import MV
from cdms import MV as mv
from cdms import axis, MV
from cdms.axis import xyz
from cdms.axis import xyz as abc
import regrid
import regrid._regrid
import MV
from MV import concatenate
if type(x) is ArrayType: foo
if type(x) is not ArrayType: foo


def foo():
    import x,y, cdms, z
    import x,y,cdms as CD,z
    import cdms
    from cdms import MV

    x = 2

# These shouldn't change
import x,y, cdms2 as cdms, z
import x,  y as Y,  cdms2 as CD
import x, cdms2 as cdms
import cdms2 as cdms
import cdms2 as CD
import cdms2.MV
import cdms2.MV as MV2
from cdms2 import MV2 as MV
from cdms2 import MV2 as mv
from cdms2.axis import xyz
from cdms2.axis import xyz as abc
import regrid2 as regrid
import regrid2._regrid
import MV2 as MV
from MV2 import concatenate
import x,y, cdms2, z
import x,  y ,  cdms2 as CD
import cdms2
from cdms2 import MV
import MV2
from ncml import _copyObjectAttributes, NumericToNCType
"""

    result = fromstr(teststr)
    result = translate(result)
    result, extra = aggressive_translate(result)
    print result

def translate(fstr):
    fstr = change_cdimports(fstr, 'cdms', 'cdms2')
    fstr = change_cdimports(fstr, 'regrid', 'regrid2')
    fstr = change_cdimports(fstr, 'MV', 'MV2')
    fstr = change_cdimports(fstr, 'Numeric', 'numpy.oldnumeric')
    fstr = change_cdimports(fstr, 'MA', 'numpy.oldnumeric.ma')

    # import cdms2.MV => cdms2.MV2
    fstr = re.sub(r'cdms2\.MV([^2]|$)', r'cdms2.MV2\1', fstr)

    # import cdms.MV => cdms.MV2
    fstr = re.sub(r'cdms\.MV([^2]|$)', r'cdms.MV2\1', fstr)

    return fstr

def aggressive_translate(fstr):
    extra = ''
    savestr = fstr
    fstr = fstr.replace('MA.Float', 'Numeric.Float')
    fstr = fstr.replace('MA.Int', 'Numeric.Int')
    fstr = fstr.replace('MA.Numeric', 'Numeric')
    fstr = fstr.replace('MA.NewAxis', 'Numeric.NewAxis')
    if savestr!=fstr:
        extra = 'import numpy.oldnumeric as Numeric\n'
    fstr = fstr.replace('.mask()', '.mask')
    fstr = fstr.replace('.raw_data()', '.data')

    # MA.XX(..., typecode= => MA.XX(..., dtype=
    fstr = re.sub(r'(MA\..*,\s*)typecode(\s*=)', r'\1dtype\2', fstr)

    # XX.mask is None => ((XX.mask is None) or (XX.mask is MV2.nomask))
    savestr = fstr
    fstr = re.sub(r' (\S*\.mask)\s*is\s*None', r' ((\1 is None) or (\1 is MV2.nomask))', fstr)
    if savestr!=fstr:
        extra += 'import MV2\n'

    # MA.sub(...) => MA.sub(..., axis=0), etc.
    fstr = re.sub(r'(MA.sum\([^,\(\)\n\r]*)\)', r'\1, axis=0)', fstr)
    fstr = re.sub(r'(MA.average\([^,\(\)\n\r]*)\)', r'\1, axis=0)', fstr)
    fstr = re.sub(r'(MA.product\([^,\(\)\n\r]*)\)', r'\1, axis=0)', fstr)
    fstr = re.sub(r'(MA.repeat\([^,\(\)\n\r]*)\)', r'\1, axis=0)', fstr)

    return fstr, extra

def convertfile(path, **args):

    aggressive = args.get('aggressive', False)
    clobber = args.get('clobber', False)
    nocomment = args.get('nocomment', False)
    stats = os.stat(path)
    f = open(path)
    fstr = f.read()
    f.close()
    result = fromstr(fstr)
    result = translate(result)
    if not aggressive:
        result, extra = aggressive_translate(result)
    else:
        extra = ''
    if fstr!=result:
        if result[0:2]=='#!':
            splice = result.find('\n')+1
        else:
            splice = 0

        addin = extra
        if not nocomment:
            addin = '# Adapted for numpy/ma/cdms2 by convertcdms.py\n'+addin
        result = result[0:splice]+addin+result[splice:]

        if not clobber:
            base, ext = os.path.splitext(path)
            newpath = base+".orig"
            if not os.path.exists(newpath):
                os.rename(path, newpath)
        else:
            os.remove(path)
        fid = file(path, 'w')
        fid.write(result)
        fid.close()
        os.chmod(path,stats.st_mode)

def convertall(direc, args):
    """Convert all .py files to use numpy/ma/cdms2 in the directory given

    For each changed file, a backup of <usesnumeric>.py is made as
    <usesnumeric>.py.orig.  A new file named <usesnumeric>.py
    is then written with the updated code.
    """
    files = glob.glob(os.path.join(direc,'*.py'))
    for afile in files:
        if afile[-8:] == 'setup.py': continue # skip these
        convertfile(afile, **args)

def _func(args, dirname, fnames):
    clobber = args.get('clobber', False)
    skiplist = args.get('skip', [])
    dirlist = os.path.abspath(dirname).split(os.sep)
    for path in skiplist:
        if path in dirlist:
            break
    else:
        convertall(dirname, args)
        convertsrc(dirname, ext=['h','c'], orig=(not clobber))

def converttree(direc, **args):
    """Convert all .py files and source code files in the tree given
    """
    os.path.walk(direc, _func, args)

def main(argv):

    try:
        args, lastargs = getopt.getopt(argv, "acdhnrs:")
    except getopt.error:
        print sys.exc_value
        print usage
        sys.exit(0)

    aggressive = False
    clobber = False
    nocomment = False
    recursive = False
    skip = []
    for flag, arg in args:
        if flag=='-a':
            aggressive = True
        elif flag=='-c':
            clobber = True
        elif flag=='-d':
            test()
            sys.exit(0)
        elif flag=='-h':
            print usage
            sys.exit(0)
        elif flag=='-n':
            nocomment = True
        elif flag=='-r':
            recursive = True
        elif flag=='-s':
            skip.append(arg)

    if len(lastargs)!=1:
        print usage
        sys.exit(0)
        
    path = lastargs[0]
    if not recursive:
        convertfile(path, aggressive=aggressive, clobber=clobber, nocomment=nocomment)
    else:
        converttree(path, aggressive=aggressive, clobber=clobber, nocomment=nocomment, skip=['.svn']+skip)

if __name__=='__main__':
    main(sys.argv[1:])
