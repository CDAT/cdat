# Adapted for numpy/ma/cdms2 by convertcdms.py
# PJD  7 Jan 2013 - Edits to 'get' function as non-string type missing_value attributes are causing problems (Aquarius data)

import MV2,os,numpy

def inbetween(string,open='(',close=')'):
   i0=string.find(open)
   string = string[i0:]
   sp= string.split(open)
   n=-1
   stout=''
   for s in sp:
      n+=1
      if s.find(close)>-1:
         sp2=s.split(close)
         nclose = len(sp2)-1
         n-=nclose
         if n==0:
            stout+=open+close.join(sp2[:-1])+close
            break
         else:
            stout+=open+close.join(sp2)
      else:
         stout+=open+s
   return stout[2*len(open):-len(close)]

class HDF5_Variable:
   def __init__(self,hdf5file,variable,h5dump=None):
      if h5dump is None:
         self.h5dump = os.environ.get("H5DUMP","h5dump")
      else:
         self.h5dump = h5dump
         
      sin,sout,serr = os.popen3('%s -h' % self.h5dump)
      err = serr.read()
      if len(err.strip())>0:
         raise RuntimeError, "h5dump binary cannot be found, HDF5 module will not function w/o it, you can pass its (full) path at init time via h5dump keyword or by setting environment variable H5DUMP"

      self.file = hdf5file
      if variable in self.file.listvariables()+self.file.listdimension():
         self.variable = variable
      else:
         raise 'Variable %s not present in file %s' % (variable, self.file.file)

      # Figures out the header for h5dump commands
      self._attributes={'missing_value':numpy.array(1.E20,'f')}
      for l in self.file.description:
         n=len(self.variable)+1
         l=l.strip()
         if l[-n:]=='/'+self.variable:
            self._group = '/'+l.split('/',1)[1].strip()
            self._header=self._group.replace(' ','\ ')
            self._group = '/'.join(self._group.split('/')[:-1])
      # Now figures out the attributes
      self._scan_attributes()

      # Figures out the shape
      cmd = '%s -H -d %s %s' % (self.h5dump, self._header, self.file.file)
      for l in os.popen(cmd).xreadlines():
         l=l.strip()
         sp=l.split()
         if sp[0].upper()=='DATASPACE':
            if not sp[1].upper() in ['SCALAR']:
               # Found the line with shape and extract it, then reverse it for C/Python order
               self.shape = tuple(eval('[%s]' % inbetween(l.strip())))
               break
            else:
               self.shape=()
               break

   def _scan_attributes(self):
      cmd = '%s -A -d %s %s' % (self.h5dump, self._header, self.file.file)
      att = os.popen(cmd).read()
      att = inbetween(att,'{','}') # remove garbage HDF5 file
      att = inbetween(att,'{','}') # Removes dataset brackets
      while len(att)>0:
         i0=att.find('{')
         info = inbetween(att,'{','}')
         linfo=len(info)
         if att[:9].upper()=='ATTRIBUTE': # Ok we are dealing with an attribute
            nm=att[9:i0].strip()[1:-1] # removes "
            inf0=info.find('{')
            data=inbetween(info,'{','}')
            while info[inf0-5:inf0]!='DATA ' and len(info.strip())!=0:
               data=inbetween(info,'{','}')
               info = info[inf0+len(data)+2:]
               inf0=info.find('{')

            value = ':'.join(inbetween(info,'{','}').split(':')[1:]).strip()
            try:
               setattr(self,nm,eval(value))
            except:
               setattr(self,nm,value)
            if nm.lower() in ['missing','missingvalue']:
               self.missing_value = numpy.array(getattr(self,nm),'f')
               
            self._attributes[nm]=getattr(self,nm)
         att = att[i0+linfo+2:].strip()
      
            
   def get(self):
      """ retrieves the variable data """
      cmd = '%s -y -d %s %s' % (self.h5dump, self._header, self.file.file)
      att = os.popen(cmd).read()
      i0=att.find('DATA {')
      att=att[i0:]
      att = inbetween(att,'{','}') # remove garbage HDF5 file and add trailing comma
      size=1
      for l in self.shape:
         size*=l
         
      data = numpy.ones(size,'f')
      ielement=0
      for v in att.split(','):
           data[ielement]=float(v)
           ielement+=1

      if ielement!=size:
         raise RuntimeError, 'got wrong number of data %s, expected %s' % (ielement,size)

      # In case of quirky missing_value attribute - AQUARIUS data (Paul Durack - 130107)
      try:
          data = numpy.ma.masked_equal(data,self.missing_value)
      except:
          data = numpy.ma.masked_equal(data,self._attributes['missing_value'])
          
      # Now also mask very low and very high values
      data = numpy.ma.masked_less(data,-1.E100)
      data = numpy.ma.masked_greater(data,1.E100)

      # Finally gives it the right shape
      data = MV2.reshape(data,self.shape)

      # And sets the attributes back on
      for a in self._attributes:
	  # Catch instance where type of missing_value attribute is not string
          if 'missing_value' in a and type(self._attributes['missing_value']) is not str:
              setattr(data,a,str(self._attributes['missing_value']))
	  # Else process attributes as string
          else:
              setattr(data,a,getattr(self,a))
      data.id = self.variable
      
      return data

   __call__ = get

class HDF5:
   def __init__(self,file,dimension_kw=None,h5dump=None):
      if h5dump is None:
         self.h5dump = os.environ.get("H5DUMP","h5dump")
      else:
         self.h5dump = h5dump
         
      sin,sout,serr = os.popen3('%s -h' % self.h5dump)
      err = serr.read()
      if len(err.strip())>0:
         raise RuntimeError, "h5dump binary cannot be found, HDF5 module will not function w/o it, you can pass its (full) path at init time via h5dump keyword or by setting environement variable H5DUMP" 

      self.file = file
      self.dimension_kw=dimension_kw
      self.dimensions = []
      self.open()

   def get(self,variable,*args,**kargs):
      if not variable in self.variables.keys():
         raise ValueError, 'Variable %s not present in file'
      return apply(self.variables[variable],args,kargs)

   __call__ = get
   
   def _scan_description(self):
      vout = []
      for l in self.description:
         sp=l.strip().split(' ',1)
         if sp[0]=='dataset':
            sp = sp[1].split('/')
            var = sp[-1]
            vout.append(var)
      return vout

   def open(self,file=None,var_class=HDF5_Variable):
      if file is not None:
         self.file = file

      # Test if it is a valid hdf5 file
      if not isinstance(self.file,str):
         raise 'Error file must be a string'
      else:
         self.file=self.file.strip()

      cmd = '%s -n %s' % (self.h5dump,self.file)

      i,o,e = os.popen3(cmd)
      self.description = o.readlines()
      err = e.readlines()
      if len(err)>0:
         raise 'Error opening file',self.file

      self.Variables = self._scan_description()
      self.variables={}
      for v in self.Variables:
         self.variables[v] = var_class(self,v,h5dump=self.h5dump)

      if self.dimension_kw is not None: # We defined a keyword
         for k in self.variables.keys():
            v = self.variables[k]
            if v._group.find(self.dimension_kw)!=-1:
               self.dimensions.append(k)
               self.Variables.remove(k)

   def listvariables(self):
      return self.Variables
   
   def listdimension(self):
      return self.dimensions
