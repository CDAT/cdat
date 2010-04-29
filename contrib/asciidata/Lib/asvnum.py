# Adapted for numpy/ma/cdms2 by convertcdms.py
import ASV
import numpy, string

def get_array(asv, name, tc=None):
    """Return the array whose field name is name, using typecode tc
    """
    return numpy.array(map(string.atof, [r[name] for r in asv]), tc)

def get_arrays(asv, omit=None, typecodes=None):
    """Return a dictionary of arrays from an ASV object, omitting names in the list omit,
       and using typecodes in the list typecodes.
    """
    f = asv.get_field_names()
    if omit is None: omit=[]
    if typecodes is None:
        typecodes=[None]*len(f)
    result = {}
    for i in range(len(f)):
        name = f[i]
        tc = typecodes[i]
        if name not in omit:
            result[name] = get_array(asv, name, tc)
    return result

def tab_delimited (filename, omit=None, typecodes=None):
    """Returns a dictionary of arrays that are the columns of the data in the file.
    """
    asv = ASV.ASV()
    asv.input_from_file(filename, ASV.TSV(), has_field_names = 1)
    return get_arrays(asv, omit=omit, typecodes=typecodes)

def comma_delimited (filename, omit=None, typecodes=None):
    """Returns a tuple of dictionary of arrays that are the columns of the data in the file.
    """
    asv = ASV.ASV()
    asv.input_from_file(filename, ASV.CSV(), has_field_names = 1)
    return get_arrays(asv, omit=omit, typecodes=typecodes)


