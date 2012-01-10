import numpy
import cmor

print 'Done importing'
try:
    import cdms2
except:
    print "This test code needs cdms2 interface for i/0"
    import sys
    sys.exit()
import os
pth = os.path.split(os.path.realpath(os.curdir))
if pth[-1]=='Test':
    ipth = opth = '.'
    dpth = '../data'
else:
    ipth = opth = 'Test'
    dpth='data'

myaxes=numpy.zeros(9,dtype='i')
myaxes2=numpy.zeros(9,dtype='i')
myvars=numpy.zeros(9,dtype='i')

def read_input(var,order=None):
    f=cdms2.open(os.path.join(dpth,"%s_sample.nc" % var))
    ok = f(var)
    if order is None:
        s = f(var)
    else:
        s = f(var,order=order)
    s.units=f[var].units
    s.id=var
    f.close()
    return s,ok

def prep_var(data):
    rk = data.rank()
    axes=[]
    for i in range(rk):
        ax = data.getAxis(i)
        if ax.isLongitude():
            id=cmor.axis(table_entry='longitude',units=ax.units,coord_vals=ax[:],cell_bounds=ax.getBounds())
        elif ax.isLatitude():
            id=cmor.axis(table_entry='latitude',units=ax.units,coord_vals=ax[:],cell_bounds=ax.getBounds())
        else:
            id=cmor.axis(table_entry=str(ax.id),units=ax.units,coord_vals=ax[:],cell_bounds=ax.getBounds())
            print i,'units:',ax.units, ax[0]
        axes.append(id)
    var = cmor.variable(table_entry = data.id,
                        units = data.units,
                        axis_ids = numpy.array(axes),
                        missing_value = data.missing_value,
                        history = "rewrote by cmor via python script")
    return var



def prep_cmor():
    cmor.setup(inpath=ipth,set_verbosity=cmor.CMOR_QUIET, netcdf_file_action = cmor.CMOR_REPLACE, exit_control = cmor.CMOR_EXIT_ON_MAJOR);
    cmor.dataset(
        outpath = opth,
        experiment_id = "lgm",
        institution = "GICC (Generic International Climate Center, Geneva, Switzerland)",
        source = "GICCM1 (2002): atmosphere:  GICAM3 (gicam_0_brnchT_itea_2, T63L32); ocean: MOM (mom3_ver_3.5.2, 2x3L15); sea ice: GISIM4; land: GILSM2.5",
        calendar = "standard",
        realization = 1,
        contact = "Rusty Koder (koder@middle_earth.net)",
        history = "Output from archive/giccm_03_std_2xCO2_2256.",
        comment = "Equilibrium reached after 30-year spin-up after which data were output starting with nominal date of January 2030",
        references = "Model described by Koder and Tolkien (J. Geophys. Res., 2001, 576-591).  Also see http://www.GICC.su/giccm/doc/index.html  2XCO2 simulation described in Dorkey et al. '(Clim. Dyn., 2003, 323-357.)",
        leap_year=0,
        leap_month=0,
        institute_id="PCMDI",
        month_lengths=None,model_id="GICCM1",forcing="Nat",
        parent_experiment_id="N/A",
        parent_experiment_rip="N/A",
        branch_time=0.)
    
    tables=[]
    a = cmor.load_table("Tables/CMIP5_Omon")
    tables.append(a)
    tables.append(cmor.load_table("Tables/CMIP5_Amon"))
    return


for var in ['tas',]:
    print 'Testing var:',var
    orders = ['tyx...','txy...','ytx...','yxt...','xyt...','xty...',]
    for o in orders:
        print '\tordering:',o
        data,data_ordered = read_input(var,order=o)
        prep_cmor()
        print data.shape
        var_id = prep_var(data)
        df = data.filled(data.missing_value)
        cmor.write(var_id,df)
        cmor.close()
        f=cdms2.open(opth+'/CMIP5/output/PCMDI/GICCM1/lgm/mon/atmos/%s/r1i1p1/%s_Amon_GICCM1_lgm_r1i1p1_186810-186812.nc'  % (var,var))
        s=f(var)
        if not numpy.allclose(s,data_ordered):
            raise "Error reordering: %s"%o
        else:
            print 'order: %s, passed' % o
        f.close()
print 'Done'
#cmor.close()
print 'Finito'
