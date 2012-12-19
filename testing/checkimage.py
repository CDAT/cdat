"""
This script compares the png test image given in the second argument against
the known good baseline given in the third. Alternate known good images
(with _number.png in the filename) are also compared against.
"""

import math
import numpy
import matplotlib.image as mpimg
import os
import os.path
import sys

def compare_imgs(adata, bdata):
    try:
        rms = math.sqrt(numpy.sum((adata-bdata)**2)/adata.size)
    except:
        print "images are not compatible"
        return -1
    return rms

def image_from_file(fname):
    try:
      areader = mpimg.imread(fname)
    except:
      print "Problem opening file"
      return None
    adata = areader.flatten()
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

def check_result_image(fname, baselinefname, threshold):
    resultimg = image_from_file(fname)
    if resultimg == None:
        print "no result image, failed test"
        return -1

    baselineimg = image_from_file(baselinefname)
    if baselineimg == None:
        print "no baseline image yet"
        print "Check in " + fname + " as a new baseline"
        return -1

    baselinefnames = find_alternates(baselinefname)
    print "baselines:"
    print baselinefnames

    bestfile = None
    bestresult = None
    for x in baselinefnames:
        print "comparing " + fname + " against " + x
        nextbimage = image_from_file(x)
        if nextbimage == None:
            continue
        res = compare_imgs(resultimg, nextbimage)
        print "result " + str(res)
        if res >= 0:
            if bestresult == None or res < bestresult:
                print "new best"
                besresult = res
                bestfile = x
            if res > threshold:
                print "image fails comparison threshold " + str(res) + ">" + str(threshold)
            else:
                print "images are close enough " + str(res)
                return 0

    print "no baseline images matched"
    if bestfile != None:
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

    ret = check_result_image(sys.argv[1], sys.argv[2], sys.argv[3])
    sys.exit(ret)

if __name__ == "__main__":
    main()
