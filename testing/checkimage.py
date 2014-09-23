"""
This script compares the png test image given in the second argument against
the known good baseline given in the third. Alternate known good images
(with _number.png in the filename) are also compared against.
"""

import math
import numpy
import vtk
import os
import os.path
import sys
import logging

from vtk.util.vtkImageExportToArray import vtkImageExportToArray
from vtk.util.vtkImageImportFromArray import vtkImageImportFromArray

defaultThreshold=0.09

def compare_imgs(adata, bdata):
    adata = adata[0]/255.
    bdata = bdata[0]/255.
    adata=adata.flatten().astype("f")
    bdata=bdata.flatten().astype("f")
    try:
        rms = math.sqrt(numpy.sum((adata-bdata)**2)/adata.size)
    except:
	logging.exception('')
        return -1
    return rms

def dump_image_to_file(fname,img):
  print "Writing to:",fname
  wr = vtk.vtkPNGWriter()
  wr.SetFileName(fname)
  imp = vtkImageImportFromArray()
  imp.SetArray((img+255/510))
  wr.SetInputConnection(imp.GetOutputPort())
  wr.Update()
  wr.Write()

def gen_diff_array(a,b):
  diff = abs(a-b)
  return diff

def image_from_file(fname):
    try:
      rd = vtk.vtkPNGReader()
      rd.SetFileName(fname)
      rd.Update()
      exp = vtkImageExportToArray()
      exp.SetInputConnection(rd.GetOutputPort())
      areader = exp.GetArray()
    except Exception,err:
      print "Problem opening file",err
      return None
    #adata = areader.flatten().astype("f")
    return areader

def find_alternates(fname):
    dirname = os.path.dirname(fname)
    prefix, ext = os.path.splitext(os.path.split(fname)[1])
    files = os.listdir(dirname)
    results = [fname]
    for i in files:
        if i.startswith(prefix) and i.endswith(ext) and i != prefix+ext:
            results.append(os.path.join(dirname, i))
    return results

def check_result_image(fname, baselinefname, threshold, baseline = False, cleanup=True):
    resultimg = image_from_file(fname)
    if resultimg is None:
        print "no result image, failed test"
        return -1

    if baseline:
        baselinefnames = find_alternates(baselinefname)
    else:
        baselinefnames = [baselinefname,]
    print "baselines:"
    print baselinefnames

    bestfile = None
    bestresult = None
    bestimg = None	 
    for x in baselinefnames:
        print "comparing " + fname + " against " + x
        nextbimage = image_from_file(x)
        if nextbimage is None:
            continue
        res = compare_imgs(resultimg, nextbimage)
        if res >= 0:
            if bestresult is None or res < bestresult:
                print "new best"
                besresult = res
                bestfile = x
                bestimg = nextbimage
            if res > threshold:
                print "image fails comparison threshold " + str(res) + ">" + str(threshold)
            else:
                print "images are close enough " + str(res)
                if cleanup:
                  os.remove(fname)
                return 0

    if bestimg is None:
	print "no baseline image found"
	return -1
    print "no baseline images matched"
    ## Ok now we are saving the diff
    diff = gen_diff_array(resultimg,bestimg)
    sp = fname.split(".")
    fnmdiff = ".".join(sp[:-1])+"_diff."+sp[-1]
    dump_image_to_file(fnmdiff,diff)
    return -1

def main():
    if len(sys.argv) != 4:
        print "Error:"
        print "Called with: " + str(sys.argv)
        print "Call with " + sys.argv[0] + " test_directory/test_filename.png  baseline_directory/baseline_filename.png threshold"
        sys.exit(-1)

    ret = check_result_image(sys.argv[1], sys.argv[2], float(sys.argv[3]))
    sys.exit(ret)

if __name__ == "__main__":
    main()
