dset ^gfs.t12z.master.grbf%f2.10m.uv.grib2
index ^gfs_master_12z.ctl.idx
undef 9.999E+20
title Global 0.5 wind %Y%m%d_%Hz
*  produced by grib2ctl v0.9.12.5p34b, modified for grib2
dtype grib2
options template
options yrev
ydef 361 linear -90.000000 0.5
xdef 720 linear 0.000000 0.500000
tdef 4 linear 12Z16sep2010 3hr
zdef 1 linear 1 1
vars 2
UGRD10m  0,103,10 0,2,2 ** 10 m u wind [m/s]
VGRD10m  0,103,10 0,2,3 ** 10 m v wind [m/s]
ENDVARS
