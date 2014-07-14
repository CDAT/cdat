# Adapted for numpy/ma/cdms2 by convertcdms.py
import MV2,numpy,cdms2,genutil

def get_parenthesis_content(code):
  opened = 0
  closed = 0
  for i,s in enumerate(code):
    if s=="(":
      opened+=1
    elif s==")":
      closed+=1
    if opened!=0 and closed==opened:
      return code[code.find("(")+1:i]
  return ""

def make_var(lap,id=None,shape=None):
    lap=MV2.array(lap)
    if shape is not None:
        lap=MV2.reshape(lap,shape)
    if id is not None:
        lap.id=id
    return lap

def readAscii( text_file ,header=0, ids=None, shape=None, next='------',separators=[';',',',':']):
    """Reads data from an ascii file
    Usage :::
    vars = genutil.ASCII.readAscii( text_file ,header=0, ids=None, shape=None, next='------',separators=[';',',',':'])
    :::

    Options :::
        text_file  :: ASCII File to read from.
        header     :: (0) Number of header lines, these lines will be skipped.
        ids        :: (None) use the values in this list as variable ids (1 per variable returned)
        shape      :: (None) use the tuple/list in this list as the final shape of the variable read.
        next       :: ('------') character string marking separation between variables
        separators :: ([';',',', ':']) List of character recognized as column separator
    Output :::
        vars       :: List containing transient(s) variable(s) possibly named after ids and reshaped from the 'shape' option.
    
    """
    sep=[]
    if isinstance(separators,str):
        separators=separators.split()
    for s in separators:
        sep.append(s)
        
    f=open( text_file )
    lst = f.readlines( )
    f.close( )
    if not isinstance(ids,(tuple,list)):
        ids=[ids]
    vars=[]
    lap=[]
    for l in lst[header:]:
        for s in sep:
            l=l.replace(s,' ')
        for s in l.split():
            if s==next:
                if len(vars)>len(ids)-1:
                    Id=None
                else:
                    Id=ids[len(vars)]
                if shape is None or len(vars)>len(shape):
                    sh = None
                else:
                    sh =shape[len(vars)]
                if lap!=[]:
                    vars.append(make_var(lap,shape=sh,id=Id))
                    lap=[]
            else:
                if s!='':
                    lap.append(float(s))
    if len(vars)>len(ids)-1:
        Id=None
    else:
        Id=ids[len(vars)]
    if lap!=[]:
        vars.append(make_var(lap,shape=shape,id=Id))
    if len(vars)>1:
        return vars
    else:
        return vars[0]


def read_col( text_file ,header=0, cskip=0, cskip_type='columns', axis=False, ids=None, idrow=0, separators=[';',',', ':']):
    """ Reads column-stored data from ASCII files
    Usage:::
     vars = genutil.ASCII.read_col( text_file ,header=0, cskip=0, cskip_type='columns', axis=False, ids=None, idrow=False, separators=[';',',', ':'])

    Options :::
        text_file  :: ASCII File to read from.
        header     :: (0) Number of header lines, these lines will be skipped.
        cskip      :: (0) Number of 'column'/'character' to skip (dummy column)
        cskip_type :: ('column') is 'cskip'  a number of 'column' or 'character' to skip?
        axis       :: (False)  Use as the values for the first column as variable axis (x values in y(x))
        idrow      :: (False) Is the first row representing the ids of var generated ?
        ids        :: (None) use the values in this list as variable ids (1 per column returned)
        separators :: ([';',',', ':']) List of character recognized as column separator
    Output :::
        vars       :: List containing 1 transient varialbe per column in the files.
                      Variable ids are optionaly determined by first row.
                      Variable axis may be the first column
    """

    sep=[]
    if isinstance(separators,str):
        separators=separators.split()
    for s in separators:
        sep.append(s)
        
    f=open( text_file )
    lst = f.readlines( )
    f.close( )
    lst=lst[header:]
    if not isinstance(ids,(tuple,list)):
        ids=[ids]        
    vars=None
    for l in lst:
        if cskip_type=='characters':
            l=l[cskip:]
        for s in sep:
            l=l.replace(s,' ')
        sp=l.split()
        if cskip_type=='columns':
            sp=sp[cskip:]
        if vars is None:
            nvars=len(sp)
            vars=[]
            for i in range(nvars):
                vars.append([])
        if idrow:
            ids=sp
            idrow=0
        else:
            for i in range(nvars):
                vars[i].append(float(sp[i]))
    for i in range(nvars):
        vars[i]=MV2.array(vars[i])
        if ids!=[None]:
            vars[i].id=ids[i]
    if axis:
        id=vars[0].id
        axval=vars.pop(0).filled()
        axindices=numpy.argsort(axval)
        ax=cdms2.createAxis(numpy.sort(axval))
        ax.id=id
        for i in range(nvars-1):
            tmp=MV2.array(genutil.arrayindexing.get(vars[i],axindices))
            tmp.id=vars[i].id
            tmp.setAxis(0,ax)
            vars[i]=tmp
    if len(vars)>1:
        return vars
    else:
        return vars[0]
    
readAsciiCols = read_col
