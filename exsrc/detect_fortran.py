#!/usr/bin/env python
import os,sys

def detect_fortran_compiler(full_path=True):


  fortrans = """
g77
gfortran
f90
f95
g95
xlf90
fort77
pgf77
pgf90
cf77
xlf
ghf77
"""
  if os.environ.has_key('FC'):
    return os.environ['FC']

  for f in fortrans.split():
    i,o=os.popen4('which '+f)
    ln=o.readlines()
    o.close()
    i.close()
    if (ln!=[]) and (not 'no' in ln[0].lower().split()) and (not 'not' in ln[0].lower().split()) :
      if full_path :
        return ln[0].strip()
      else:
        return f

if __name__=="__main__":
  print detect_fortran_compiler()
