import cdms2
import os
import sys
import vcs

baselineFilename = sys.argv[1]
checkImagePath = os.path.join(os.path.dirname(baselineFilename),"..")
sys.path.append(checkImagePath)
import checkimage

cdmsfile = cdms2.open(os.path.join(sys.prefix,"sample_data","clt.nc"))
data = cdmsfile('clt')

x=vcs.init()
x.setbgoutputdimensions(1200,1091,units="pixels")

t=x.gettemplate('default')
x.plot(data, t, bg=True)

# This should force the image to update
x.setcolormap('bl_to_drkorang')

testFilename = "test_vcs_setcolormap.png"
x.png(testFilename)

ret = checkimage.check_result_image(testFilename,
                                    baselineFilename,
                                    checkimage.defaultThreshold)
sys.exit(ret)
