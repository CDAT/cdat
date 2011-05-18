dset ^testgrib2.grib2
index ^testgrib2.idx
undef 9.999E+20
title testgrib2.grib2
*  produced by g2ctl v0.0.4o
* griddef=1:0:(10 x 20):grid_template=0:winds(N/S) lat-lon grid:(10 x 20) units 1e-06 input WE:SN output WE:SN res 48 lat -63.000000 to -53.500000 by 0.500000 lon 4.500000 to 9.000000 by 0.500000 #points=200:winds(N/S)

dtype grib2
ydef 20 linear -63.000000 0.5
xdef 10 linear 4.500000 0.500000
tdef 15 linear 12Z23sep2009 3hr
zdef 1 linear 1 1
vars 16
DIRPWsfc   0,1,1   10,0,10 ** surface Primary Wave Direction [deg]
HTSGWsfc   0,1,1   10,0,3 ** surface Significant Height of Combined Wind Waves and Swell [m]
PERPWsfc   0,1,1   10,0,11 ** surface Primary Wave Mean Period [s]
SWDIR_1   0,241,1   10,0,7 ** 1 in sequence Direction of Swell Waves [deg]
SWDIR_2   0,241,2   10,0,7 ** 2 in sequence Direction of Swell Waves [deg]
SWELL_1   0,241,1   10,0,8 ** 1 in sequence Significant Height of Swell Waves [m]
SWELL_2   0,241,2   10,0,8 ** 2 in sequence Significant Height of Swell Waves [m]
SWPER_1   0,241,1   10,0,9 ** 1 in sequence Mean Period of Swell Waves [s]
SWPER_2   0,241,2   10,0,9 ** 2 in sequence Mean Period of Swell Waves [s]
UGRDsfc   0,1,1   0,2,2 ** surface U-Component of Wind [m/s]
VGRDsfc   0,1,1   0,2,3 ** surface V-Component of Wind [m/s]
WDIRsfc   0,1,1   0,2,0 ** surface Wind Direction (from which blowing) [deg]
WINDsfc   0,1,1   0,2,1 ** surface Wind Speed [m/s]
WVDIRsfc   0,1,1   10,0,4 ** surface Direction of Wind Waves [deg]
WVHGTsfc   0,1,1   10,0,5 ** surface Significant Height of Wind Waves [m]
WVPERsfc   0,1,1   10,0,6 ** surface Mean Period of Wind Waves [s]
ENDVARS
