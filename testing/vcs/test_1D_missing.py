import vcs,numpy,cdms2,MV2,os,sys
src=sys.argv[1]
pth = os.path.join(os.path.dirname(src),"..")
sys.path.append(pth)
import checkimage

x=vcs.init()
x.setbgoutputdimensions(1200,1091,units="pixels")
yx =x.createyxvsx()

data = """-11.14902417  -9.17390922  -7.29515002  
-7.51774549  -8.63608171
  -10.4827395   -9.93859485  -7.3394366   -5.39241468  -5.74825567
     -6.74967902  -7.09622319  -5.93836983  -4.04592997  -2.65591499
        -1.68180032  -0.86935245  -0.40114047  -0.54273785  -1.36178957
           -2.67488251  -3.87524401  -4.84708491  -5.49186142  -5.28618944
              -4.30557389  -2.89804038  -1.53825408  -1.84771029  -2.74948361
                 -2.23517037  -1.73306118  -0.71200646   0.76416785   1.51511193
                    -0.04018418  -1.54564706  -1.88664877  -0.43751604   0.89988184
                        0.33437949  -1.70341844  -3.79880014  -4.03570169  -4.7740073
                           -5.04626101  -3.77609961  -3.18667176  -2.21038272  -1.3666902
                              -0.54267951  -0.16472441  -0.52871418  -0.83520848  -0.90315403
                                 -0.21747426   0.01922666   0.89621996   1.75691927   3.12657503
                                     4.55749531   6.04921304   7.20744489   7.65294958""".split()
data = numpy.array(data,dtype=numpy.float)
data = MV2.array(data)

data=MV2.masked_where(MV2.logical_and(data>-4,data<-2),data)

#yx.datawc_x1 = 0
#yx.datawc_x2 = 80
##yx.datawc_y1 =-12 
#yx.datawc_y2 = 12 


x.plot(data,yx,bg=1)
fnm = "test_vcs_1d_missing.png"
x.png(fnm)

print "fnm:",fnm
print "src:",src
ret = checkimage.check_result_image(fnm,src,checkimage.defaultThreshold)
sys.exit(ret)
