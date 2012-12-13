"""
This script compares the png test image given in the second argument against
the known good baseline given in the third. Alternate known good images
(with _number.png in the filename) are also compared against.
"""

import sys
import os
import vtk
import vtk.test
import vtk.test.Testing

if len(sys.argv) != 3:
    print "Error:"
    print "Called with: " + str(sys.argv)
    print "Call with " + sys.argv[0] + " test_directory/test_filename.png " + " " + " baseline_directory/baseline_filename.png"
    sys.exit(-1)

try:
    freader = vtk.vtkPNGReader()
    freader.SetFileName(sys.argv[1])

    #vtkImageDifference assumes RGB, so strip alpha
    removealpha = vtk.vtkImageExtractComponents()
    removealpha.SetComponents(0,1,2)
    removealpha.SetInputConnection(freader.GetOutputPort())
    removealpha.Update()

    if not os.path.isfile(sys.argv[2]):
        #make a new image if no baseline yet
        #when making a new test, copy and rename this into the baseline
        #dir and rerun before checking it in
        print("No regression image yet, creating one")
        writer = vtk.vtkPNGWriter()
        writer.SetInputConnection(removealpha.GetOutputPort())
        writer.SetFileName(sys.argv[1]+"stripped.png")
        writer.Write();
        sys.exit(-1)

    #do the image comparison
    vtk.test.Testing.compareImageWithSavedImage(
        removealpha.GetOutput(), sys.argv[2], 0.1)

except AssertionError:
    #test comparison failed
    sys.exit(-2)

#test comparison passed
sys.exit(0)
