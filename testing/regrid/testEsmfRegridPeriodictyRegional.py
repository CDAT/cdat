import os, sys, vcs, cdms2, EzTemplate, testing.regression as regression

data = sys.argv[1]
png = sys.argv[2]

f=cdms2.open(data)
s=f("swcre")

lon_dest = cdms2.createAxis([ 135., 137., 139., 141., 143., 145., 147., 149., 151.,
    153., 155., 157., 159., 161., 163., 165., 167., 169.,
    171., 173., 175., 177., 179., 181., 183., 185., 187.,
    189., 191., 193., 195., 197., 199., 201., 203., 205.,
    207., 209., 211., 213., 215., 217., 219., 221., 223.,
    225., 227., 229., 231., 233., 235.])
lon_dest.designateLongitude()
lon_dest.id="longitude"
lon_dest.units="degrees_east"

lat_dest = cdms2.createAxis([-29., -27., -25., -23., -21., -19., -17., -15., -13., -11., -9.,
    -7., -5., -3., -1., 1., 3., 5., 7., 9., 11., 13.,
    15., 17., 19., 21., 23., 25., 27., 29.])
lat_dest.designateLatitude()
lat_dest.units="degrees_north"
dummy = cdms2.MV2.ones((len(lat_dest),len(lon_dest)))
dummy.setAxisList((lat_dest,lon_dest))

grid_dest=dummy.getGrid()
s.id="orig"
s_regrid2 = s.regrid(grid_dest,regridTool="regrid2")
s_regrid2.id="regrid2"
s_esmf_lin = s.regrid(grid_dest)
s_esmf_lin.id = "ESMF Linear"
s_esmf_con = s.regrid(grid_dest,regridTool="esmf",regridMethod="conservative")
s_esmf_lin.id = "ESMF Conservative"

x=regression.init()
t=x.createtemplate()
t.blank()
t.data.priority=1
t.legend.priority=1
t.dataname.priority=1
t.dataname.y=t.dataname.y*.95
M=EzTemplate.Multi(template=t,x=x,rows=2,columns=2)
gm=x.createboxfill()
levels= vcs.mkscale(*vcs.minmax(s))
cols = vcs.getcolors(levels)
gm.boxfill_type = "custom"
gm.fillareacolors = cols
gm.levels = levels
x.plot(s,M.get(),gm,bg=1)
x.plot(s_regrid2,M.get(),gm,bg=1)
x.plot(s_esmf_lin,M.get(),gm,bg=1)
x.plot(s_esmf_con,M.get(),gm,bg=1)

ret = regression.run(x, "esmf_issue_1125.png", png)
