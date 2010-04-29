dset ^testgrib.grb
title LATS test
undef 1e+20
dtype grib
index ^testgrib.gmp
options yrev
xdef 64 linear 0.000000 5.625000
ydef 32 levels
-85.761 -80.269 -74.745 -69.213 -63.679 -58.143 -52.607 -47.070 -41.532 -35.995 
-30.458 -24.920 -19.382 -13.844  -8.307  -2.769   2.769   8.307  13.844  19.382 
 24.920  30.458  35.995  41.532  47.070  52.607  58.143  63.679  69.213  74.745 
 80.269  85.761 
zdef 3 levels
1000.00  925.00  850.00 
tdef 16 linear 6Z30dec1978  6hr
vars 5
var5      0  153,  1,  0,  0 Test variable 5 [test]
prc       0  144,  1,  0,  0 Total precipitation [mm/day]
var3      0  151,  1,  0,  0 Test variable 3 [test]
var2      0   58,  1,  0,  0 Test variable 2 [test]
var1      3  150,100 Test variable 1 [test]
endvars
