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
from vtk.util.vtkImageExportToArray import vtkImageExportToArray

def compare_imgs(adata, bdata):
    try:
        rms = math.sqrt(numpy.sum((adata-bdata)**2)/adata.size)
    except:
        print "images are not compatible"
        return -1
    return rms

def image_from_file(fname):
    try:
      rd = vtk.vtkPNGReader()
      rd.SetFileName(fname)
      rd.Update()
      exp = vtkImageExportToArray()
      exp.SetInputConnection(rd.GetOutputPort())
      areader = exp.GetArray()[0]/255.
    except Exception,err:
      print "Problem opening file",err
      return None
    adata = areader.flatten().astype("f")
    return adata

def find_alternates(fname):
    dirname = os.path.dirname(fname)
    prefix, ext = os.path.splitext(os.path.split(fname)[1])
    files = os.listdir(dirname)
    results = [fname]
    for i in files:
        if i.startswith(prefix) and i.endswith(ext) and i != prefix+ext:
            results.append(os.path.join(dirname, i))
    return results

def check_result_image(fname, baselinefname, threshold, baseline = False):
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
            if res > threshold:
                print "image fails comparison threshold " + str(res) + ">" + str(threshold)
            else:
                print "images are close enough " + str(res)
                return 0

    print "no baseline images matched"
    if bestfile is not None:
        print bestfile + " is closest"
        #TODO: save difference image for verification, something like
        #bestbimage = image_from_file(bestfile)
        #diffimg = (resultimg-bestbimg)**2
        #mpimg.imsave("filename.png", diffimg) #should work but it doesn't

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
