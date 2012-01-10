MODULE local_subs

  USE cmor_users_functions
!!$  PRIVATE
!!$  PUBLIC read_coords, read_time, read_3d_input_files, read_2d_input_files
CONTAINS
  
  SUBROUTINE read_coords(alats, alons, plevs, bnds_lat, bnds_lon)

    IMPLICIT NONE
    
    DOUBLE PRECISION, INTENT(OUT), DIMENSION(:) :: alats
    DOUBLE PRECISION, INTENT(OUT), DIMENSION(:) :: alons
    DOUBLE PRECISION, INTENT(OUT), DIMENSION(:) :: plevs
    DOUBLE PRECISION, INTENT(OUT), DIMENSION(:,:) :: bnds_lat
    DOUBLE PRECISION, INTENT(OUT), DIMENSION(:,:) :: bnds_lon
    
    INTEGER :: i
    
    DO i = 1, SIZE(alons)
       alons(i) = (i-1)*360./SIZE(alons)
       bnds_lon(1,i) = (i - 1.5)*360./SIZE(alons)
       bnds_lon(2,i) = (i - 0.5)*360./SIZE(alons)
    END DO
    
    DO i = 1, SIZE(alats)
       alats(i) = (size(alats)+1-i)*10
       bnds_lat(1,i) = (size(alats)+1-i)*10 + 5.
       bnds_lat(2,i) = (size(alats)+1-i)*10 - 5.
    END DO
  
    DO i = 1, SIZE(plevs)
       plevs(i) = i*1.0e4
    END DO
          plevs = (/100000., 92500., 85000., 70000.,&
       60000., 50000., 40000., 30000., 25000., 20000.,&
       15000., 10000., 7000., 5000., 3000., 2000., 1000. /)
  

    RETURN
  END SUBROUTINE read_coords

  SUBROUTINE read_time(it, time, time_bnds)
    
    IMPLICIT NONE
    
    INTEGER, INTENT(IN) :: it
    DOUBLE PRECISION, INTENT(OUT) :: time
    DOUBLE PRECISION, INTENT(OUT), DIMENSION(2,1) :: time_bnds
    
    time = (it-0.5)*30.
    time_bnds(1,1) = (it-1)*30.
    time_bnds(2,1) = it*30.
    
    RETURN
  END SUBROUTINE read_time
  
include "reader_2D_3D.f90"

END MODULE local_subs

program testing
  use cmor_users_functions
  use local_subs
    implicit none
    integer error_flag
  INTEGER, PARAMETER :: ntimes = 2    ! number of time samples to process
  INTEGER, PARAMETER :: lon = 4       ! number of longitude grid cells  
  INTEGER, PARAMETER :: lat = 3       ! number of latitude grid cells
  INTEGER, PARAMETER :: lev = 5       ! number of standard pressure levels
  INTEGER, PARAMETER :: lev2 = 17       ! number of standard pressure levels
  INTEGER, PARAMETER :: n2d = 4       ! number of IPCC Table A1a fields to be
                                      !     output.
  INTEGER, PARAMETER :: n3d = 3       ! number of IPCC Table A1c fields to 
                                      !     be output.  
                                ! My variable names for IPCC Table A1c fields
  CHARACTER (LEN=5), DIMENSION(n3d) :: &
                                 varin3d=(/'CLOUD', 'U    ', 'T    '/)

                                ! Units appropriate to my data
  CHARACTER (LEN=6), DIMENSION(n3d) :: &
                                  units3d=(/ '%     ', 'm s-1 ',   'K     ' /)

                     ! Corresponding IPCC Table A1c entry (variable name) 
  CHARACTER (LEN=2), DIMENSION(n3d) :: entry3d = (/ 'cl', 'ua', 'ta' /)

                                ! My variable names for IPCC Table A1a fields
  CHARACTER (LEN=8), DIMENSION(n2d) :: &
                  varin2d=(/ 'LATENT  ', 'TSURF   ', 'SOIL_WET', 'PSURF   ' /)

                                ! Units appropriate to my data
   CHARACTER (LEN=6), DIMENSION(n2d) :: &
                          units2d=(/ 'W m-2 ', 'K     ', 'kg m-2', 'Pa    ' /)

   CHARACTER (LEN=4), DIMENSION(n2d) :: &
                      positive2d= (/  'down',  '    ', '    ', '    '  /)

                     ! Corresponding IPCC Table A1a entry (variable name) 
  CHARACTER (LEN=5), DIMENSION(n2d) :: &
                        entry2d = (/ 'hfls ', 'tas  ', 'mrsos', 'ps   ' /)

!  uninitialized variables used in communicating with CMOR:
!  ---------------------------------------------------------

  INTEGER :: znondim_id, zfactor_id
  INTEGER, DIMENSION(n2d) :: var2d_ids
  INTEGER, DIMENSION(n3d) :: var3d_ids
  REAL, DIMENSION(lon,lat) :: data2d
  real, DIMENSION(lon*lat) ::  data1dtest
  REAL, DIMENSION(lon,lat,lev2) :: data3d
  DOUBLE PRECISION, DIMENSION(lat) :: alats
  DOUBLE PRECISION, DIMENSION(lon) :: alons
  DOUBLE PRECISION, DIMENSION(lev2) :: plevs
  DOUBLE PRECISION, DIMENSION(1) :: time
  DOUBLE PRECISION, DIMENSION(2,1):: bnds_time
  DOUBLE PRECISION, DIMENSION(2,lat) :: bnds_lat
  DOUBLE PRECISION, DIMENSION(2,lon) :: bnds_lon
  DOUBLE PRECISION, DIMENSION(lev) :: zlevs
  DOUBLE PRECISION, DIMENSION(lev+1) :: zlev_bnds
  real, DIMENSION(lev) :: a_coeff
  DOUBLE PRECISION, DIMENSION(lev) :: b_coeff
  DOUBLE PRECISION :: p0,bt
  DOUBLE PRECISION :: p0array(1)
  real, DIMENSION(lev+1) :: a_coeff_bnds
  DOUBLE PRECISION, DIMENSION(lev+1) :: b_coeff_bnds
  INTEGER :: ilon, ilat, ipres, ilev, itim,i,ilon2,ilat2,itim2
  real :: missing

  !  Other variables:
  !  ---------------------
  
  INTEGER :: it, m, j,k

  bt=0.
  j = CMOR_REPLACE
  k = CMOR_EXIT_ON_MAJOR
  error_flag = cmor_setup(inpath='Test', netcdf_file_action=j,&
       exit_control=k)
  print*,'Test code: done'
  error_flag = cmor_dataset(                                   &
       outpath='Test',                                         &
       experiment_id='abrupt 4XCO2',           &
       institution=                                            &
       'GICC (Generic International Climate Center, ' //       &
       'Geneva, Switzerland)',                                 &
       source='GICCM1 (2002): ' //                             &
       'atmosphere:  GICAM3 (gicam_0_brnchT_itea_2, T63L32); '// &
       'ocean: MOM (mom3_ver_3.5.2, 2x3L15); '             //  &
       'sea ice: GISIM4; land: GILSM2.5',                      &
       calendar='360_day',                                      &
       realization=1,                                          &
       history='Output from archive/giccm_03_std_2xCO2_2256.', &
       comment='Equilibrium reached after 30-year spin-up ' // &
       'after which data were output starting with nominal '// &
       'date of January 2030',                                 &
       references='Model described by Koder and Tolkien ' //   &
       '(J. Geophys. Res., 2001, 576-591).  Also '        //   &
       'see http://www.GICC.su/giccm/doc/index.html '     //   &
       ' 2XCO2 simulation described in Dorkey et al. '    //   &
       '(Clim. Dyn., 2003, 323-357.)', model_id="GICCM1", &
       forcing='TO',contact="Barry Bonds",institute_id="PCMDI",&
       parent_experiment_rip="N/A",parent_experiment_id="N/A",branch_time=bt)
  print*, 'Test code: done 2 lalala'
  
  call read_coords(alats, alons, plevs, bnds_lat, bnds_lon)
  

  ilat2 = cmor_axis(  &
       table='Tables/CMIP5_Lmon',        &
       table_entry='latitude',       &
       units='degrees_north',        &  
       length=lat,                   &
       coord_vals=alats,             & 
       cell_bounds=bnds_lat)        
      
  ilon2 = cmor_axis(  &
       table='Tables/CMIP5_Lmon',        &
       table_entry='longitude',      &
       length=lon,                   &
       units='degrees_east',         &
       coord_vals=alons,             &
       cell_bounds=bnds_lon)      
        
  itim2 = cmor_axis(  &
       table='Tables/CMIP5_Lmon',        &
       table_entry='time',           &
       units='days since 2030-1-1',  &
       length=ntimes,                &
       interval='1 month')

  print*, 'Test code: ok calling axis stuff lat'
  ilat = cmor_axis(  &
       table='Tables/CMIP5_Amon',    &
       table_entry='latitude',       &
       units='degrees_north',        &  
       length=lat,                   &
       coord_vals=alats,             & 
       cell_bounds=bnds_lat)        
      
  print*, 'Test code: ok calling axis stuff lon',ilat
  ilon = cmor_axis(  &
       table='Tables/CMIP5_Amon',    &
       table_entry='longitude',      &
       length=lon,                   &
       units='degrees_east',         &
       coord_vals=alons,             &
       cell_bounds=bnds_lon)      
        
  print*, 'Test code: ok calling axis stuff pressure',ilon
  ipres = cmor_axis(  &
       table='Tables/CMIP5_Amon',    &
       table_entry='plevs',       &
       units='Pa',                   &
       length=lev2,                   &
       coord_vals=plevs)

  !   note that the time axis is defined next, but the time coordinate 
  !   values and bounds will be passed to cmor through function 
  !   cmor_write (later, below).

  print*, 'Test code: ok calling axis stuff time',ipres
  itim = cmor_axis(  &
       table='Tables/CMIP5_Amon',    &
       table_entry='time',           &
       units='days since 2030-1-1',  &
       length=ntimes,                &
       interval='30 days')

  !  define model eta levels (although these must be provided, they will
  !    actually be replaced by a+b before writing the netCDF file)
  zlevs = (/ 0.1, 0.3, 0.55, 0.7, 0.9 /)
  zlev_bnds=(/ 0.,.2, .42, .62, .8, 1. /)

  print*, 'Test code: ok calling axis stuff lev2',itim
  ilev = cmor_axis(  &
       table='Tables/CMIP5_Amon',    &
       table_entry='standard_hybrid_sigma',       &
       units="1",   &
       length=lev,                   &
       coord_vals=zlevs,             &
       cell_bounds=zlev_bnds)

  print*, 'Test code: ok called stuff lev2',ilev
  !   define z-factors needed to transform from model level to pressure
  p0 = 1.e5
  p0array(1)=p0
  a_coeff = (/ 0.1, 0.2, 0.3, 0.22, 0.1 /)
  b_coeff = (/ 0.0, 0.1, 0.2, 0.5, 0.8 /)

  a_coeff_bnds=(/0.,.15, .25, .25, .16, 0./)
  b_coeff_bnds=(/0.,.05, .15, .35, .65, 1./)

  print*, 'Test code: zfactor p0'
  error_flag = cmor_zfactor(  &
       zaxis_id=ilev,                      &
       zfactor_name='p0',                  &
       units='Pa',                         &
       zfactor_values = p0)

  print*, 'Test code: result',error_flag
  print*, 'Test code: zfactor b'
  error_flag = cmor_zfactor(  &
       zaxis_id=ilev,                       & 
       zfactor_name='b',                    &
       axis_ids= (/ ilev /),                &
       zfactor_values = b_coeff,            &
       zfactor_bounds = b_coeff_bnds  )

  print*, 'Test code: result',error_flag
  print*, 'Test code: zfactor a'
  error_flag = cmor_zfactor(  &
       zaxis_id=ilev,                       &
       zfactor_name='a',                    &
       axis_ids= (/ ilev /),                &
       zfactor_values = a_coeff,            &
       zfactor_bounds = a_coeff_bnds )

  print*, 'Test code: result',error_flag
  print*, 'Test code: zfactor ps'
  zfactor_id = cmor_zfactor(  &
       zaxis_id=ilev,                         &
       zfactor_name='ps',                     &
       axis_ids=(/ ilon, ilat, itim /),       &
       units='Pa' )

  print*, 'Test code: result',zfactor_id
  print*, 'Test code: var3d'
  missing = 1.e28
  var3d_ids(1) = cmor_variable(    &
       table='Tables/CMIP5_Amon',  &
       table_entry=entry3d(1),     &
       units=units3d(1),           &
       axis_ids=(/ ilon, ilat, ilev, itim /),  &
       missing_value=missing, &
       original_name=varin3d(1))
  
  !  Define variables appearing in IPCC table A1c that are a function of pressure
  !         (3-d variables)
  
  print*, 'Test code: result',var3d_ids(1)
  DO m=2,n3d
     print*, 'Test code: var: ',entry3d(m)
     var3d_ids(m) = cmor_variable(    &
          table='Tables/CMIP5_Amon',  &
          table_entry=entry3d(m),     &
          units=units3d(m),           &
          axis_ids=(/ ilon, ilat, ipres, itim /), &
          missing_value=missing,       &
          original_name=varin3d(m))
     print*, 'Test code: result',var3d_ids(m)
  ENDDO
  

  !  Define variables appearing in IPCC table A1a (2-d variables)
  
  DO m=1,n2d
     print*, 'Test code: var: ',entry2d(m)
     if (m.ne.3) then
     var2d_ids(m) = cmor_variable(    &
          table='Tables/CMIP5_Amon',      &
          table_entry=entry2d(m),     & 
          units=units2d(m),           & 
          axis_ids=(/ ilon, ilat, itim /), &
          missing_value=1.0e28,       &
          positive=positive2d(m),     &
          original_name=varin2d(m))   
  else
     var2d_ids(m) = cmor_variable(    &
          table='Tables/CMIP5_Lmon',      &
          table_entry=entry2d(m),     & 
          units=units2d(m),           & 
          axis_ids=(/ ilon2, ilat2, itim2 /), &
          missing_value=1.0e28,       &
          positive=positive2d(m),     &
          original_name=varin2d(m)) 
  endif
     print*, 'Test code: result',var2d_ids(m)
  ENDDO

  PRINT*, 'Test code:  '
  PRINT*, 'Test code: completed everything up to writing output fields '
  PRINT*, 'Test code:  '
  time_loop: DO it=1, ntimes
     
     ! In the following loops over the 3d and 2d fields, the user-written    
     ! subroutines (read_3d_input_files and read_2d_input_files) retrieve 
     ! the requested IPCC table A1c and table A1a fields and store them in 
     ! data3d and data2d, respectively.  In addition a user-written code 
     ! (read_time) retrieves the time and time-bounds associated with the 
     ! time sample (in units of 'days since 1970-1-1', consistent with the 
     ! axis definitions above).  The bounds are set to the beginning and 
     ! the end of the month retrieved, indicating the averaging period.
     
     ! The user must write a code to obtain the times and time-bounds for
     !   the time slice.  The following line is simply a place-holder for
     !   the user's code, which should replace it.
     
    call read_time(it, time(1), bnds_time)

    call read_3d_input_files(it, varin3d(1), data3d)

    error_flag = cmor_write(                                  &
         var_id        = var3d_ids(1),                        &
         data          = data3d,                              &
         ntimes_passed = 1,                                   &
         time_vals     = time,                                &
         time_bnds     = bnds_time   )

    call read_2d_input_files(it, varin2d(4), data2d)                  

    error_flag = cmor_write(                                  &
         var_id        = zfactor_id,                          &
         data          = data2d,                              &
         ntimes_passed = 1,                                   &
         time_vals     = time,                                &
         time_bnds     = bnds_time,                           &
         store_with    = var3d_ids(1) )

    print*, 'Test code: result',error_flag
   ! Cycle through the 3-d fields (stored on pressure levels), 
    ! and retrieve the requested variable and append each to the 
    ! appropriate netCDF file.

    DO m=2,n3d
        
        ! The user must write the code that fills the arrays of data
        ! that will be passed to CMOR.  The following line is simply a
        ! a place-holder for the user's code, which should replace it.

        call read_3d_input_files(it, varin3d(m), data3d)
       
        ! append a single time sample of data for a single field to 
        ! the appropriate netCDF file.
        
        error_flag = cmor_write(                                  &
             var_id        = var3d_ids(m),                        &
             data          = data3d,                              &
             ntimes_passed = 1,                                   &
             time_vals     = time,                                &
             time_bnds     = bnds_time  )
        
        IF (error_flag < 0) THEN
           ! write diagnostic messages to standard output device
           write(*,*) 'Test code:  Error encountered writing IPCC Table A1c ' &
                // 'field ', entry3d(m), ', which I call ', varin3d(m)
           write(*,*) 'Test code:  Was processing time sample: ', time
                      
        END IF

     END DO
!!$     
!!$     ! Cycle through the 2-d fields, retrieve the requested variable and 
!!$     ! append each to the appropriate netCDF file.
!!$     
     DO m=1,n2d
        
        ! The user must write the code that fills the arrays of data
        ! that will be passed to CMOR.  The following line is simply a
        ! a place-holder for the user's code, which should replace it.
        
        call read_2d_input_files(it, varin2d(m), data2d)                  

        ! append a single time sample of data for a single field to 
        ! the appropriate netCDF file.

        error_flag = cmor_write(                                  &
             var_id        = var2d_ids(m),                        &
             data          = data2d,                              &
             ntimes_passed = 1,                                   &
             time_vals     = time,                                &
             time_bnds     = bnds_time  )
        
       IF (error_flag < 0) THEN
           ! write diagnostic messages to standard output device
           write(*,*) 'Test code:  Error encountered writing IPCC Table A1a ' &
                // 'field ', entry2d(m), ', which I call ', varin2d(m)
           write(*,*) 'Test code:  Was processing time sample: ', time 
                      
        END IF
        
     END DO
     
  END DO time_loop

  error_flag = cmor_close()

end program testing
