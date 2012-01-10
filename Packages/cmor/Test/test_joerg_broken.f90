program joerg

  USE cmor_users_functions
  IMPLICIT NONE

  integer axes(2),error_flag,ntables(2)

  integer,parameter ::  nlon = 122, nlat=101, ntim=1
  integer tim_id,i,j,grid_id,var_ids

  real alon(nlon,nlat),blon(nlon,nlat,4),xii(nlon)
  real alat(nlon,nlat),blat(nlon,nlat,4),yii(nlat)


  real mydata(nlon,nlat,1)

  real anum

  double precision bt
  double precision times(1)
  double precision time_bounds(2,1)

  bt=0.
  do i =1,nlon 
     xii(i)=i
     do j = 1,nlat
        anum = i+j+i*j
        mydata(i,j,1) = sin(anum)
     enddo
  end do
  do i =1,nlat
     yii(i)=i
  end do


  open(10,file="joerg.txt")

  read(10,*) alat
  read(10,*) alon
  read(10,*) blat
  read(10,*) blon

  close(10)
  error_flag = cmor_setup(inpath='Test', netcdf_file_action='replace')

  error_flag = cmor_dataset(                                    &
       outpath='Test',                                          &
       experiment_id='abrupt 4XCO2',                            &
       institution=                                             &
       'GICC (Generic International Climate Center, ' //        &
       'Geneva, Switzerland)',                                  &
       source='GICCM1 (2002): ' //                              &
       'atmosphere:  GICAM3 (gicam_0_brnchT_itea_2, T63L32); '//&
       'ocean: MOM (mom3_ver_3.5.2, 2x3L15); '             //   &
       'sea ice: GISIM4; land: GILSM2.5',                       &
       calendar='noleap',                                       &
       realization=1,                                           &
       history='Output from archive/giccm_03_std_2xCO2_2256.',  &
       institute_id = 'PCMDI',                                  &
       comment='Equilibrium reached after 30-year spin-up ' //  &
       'after which data were output starting with nominal '//  &
       'date of January 2030',                                  &
       references='Model described by Koder and Tolkien ' //    &
       '(J. Geophys. Res., 2001, 576-591).  Also '        //    &
       'see http://www.GICC.su/giccm/doc/index.html '     //    &
       ' 2XCO2 simulation described in Dorkey et al. '    //    &
       '(Clim. Dyn., 2003, 323-357.)',                          &
       model_id='GICCM1',forcing='TO',contact="Barry Bonds",    &
       parent_experiment_rip="N/A",parent_experiment_id="N/A",branch_time=bt)

  ntables(1) =  cmor_load_table('/git/cmip5-cmor-tables/Tables/CMIP5_OImon')
  ntables(2) =  cmor_load_table('/git/cmip5-cmor-tables/Tables/CMIP5_grids')

  call cmor_set_table(table_id=ntables(2))

  axes(1) = cmor_axis(                                          &
       table_entry        = 'i_index',                          &
       length             = nlon,                               &
       coord_vals         = xii,                                &
       units              = '1')
  
  axes(2) = cmor_axis(                                          &
       table_entry        = 'j_index',                          &
       length             = nlat,                               &
       coord_vals         = yii,                                &
       units              = '1')
  
  print*, 'Axes:',axes
  grid_id = cmor_grid(                                          &
       axis_ids           = axes,                               &
       latitude           = alat,                               &
       longitude          = alon,                               &
       latitude_vertices  = blat,                               &
       longitude_vertices = blon)
  
  call cmor_set_table(table_id=ntables(1))
 
  tim_id = cmor_axis( &
       table_entry = "time", &
       units = "months since 2010")

  var_ids              = cmor_variable(                         &
       table_entry        = "sic",                              &
       units              = "%",                                &
       axis_ids           = (/ grid_id, tim_id /),              &
       missing_value      = 1.e20 )
  
  times(1)=1.
  time_bounds(1,1)=1.
  time_bounds(2,1)=2.
  error_flag = cmor_write(                                      &
       var_id            = var_ids,                             &
       data              = mydata,                              &
       file_suffix       = "",                                  &
       ntimes_passed     = ntim,                                &
       time_vals         = times,                              &
       time_bnds         = time_bounds)

  error_flag = cmor_close()

end program joerg
