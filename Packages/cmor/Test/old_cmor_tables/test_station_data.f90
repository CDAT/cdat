MODULE local_subs

  USE cmor_users_functions
!!$  PRIVATE
!!$  PUBLIC read_coords, read_time, read_3d_input_files, read_2d_input_files
CONTAINS
  
  SUBROUTINE read_coords(alats, alons, plevs, bnds_lat, bnds_lon, station, st_lons, st_lats)

    IMPLICIT NONE
    
    DOUBLE PRECISION, INTENT(OUT), DIMENSION(:) :: alats
    DOUBLE PRECISION, INTENT(OUT), DIMENSION(:) :: alons
    DOUBLE PRECISION, INTENT(OUT), DIMENSION(:) :: plevs
    DOUBLE PRECISION, INTENT(OUT), DIMENSION(:,:) :: bnds_lat
    DOUBLE PRECISION, INTENT(OUT), DIMENSION(:,:) :: bnds_lon
    INTEGER, INTENT(OUT), DIMENSION(:) ::  station
    DOUBLE PRECISION, INTENT(OUT), DIMENSION(:) :: st_lats
    DOUBLE PRECISION, INTENT(OUT), DIMENSION(:) :: st_lons
    
    INTEGER :: i, j, k
    
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
    
    k = 1
    DO i = 1, SIZE(alons)
      DO j = 1, SIZE(alats)
        station(k) = k
        st_lons(k) = alons(i)
        st_lats(k) = alats(j)
        k = k+1
      END DO   
    END DO 
    
    
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
  
  !------------------------------------------------
  SUBROUTINE read_3d_input_files(it, varname, field)

    IMPLICIT NONE
    
    INTEGER, INTENT(IN) :: it
    CHARACTER(len=*), INTENT(IN) :: varname
    REAL, INTENT(OUT), DIMENSION(:,:,:) :: field
    
    INTEGER :: i, j, k
    REAL :: factor, offset
    CHARACTER(len=LEN(varname)) :: tmp
    
    tmp = TRIM(ADJUSTL(varname))
    SELECT CASE (tmp)
    CASE ('CLOUD')  
       factor = 0.1
       offset = -50.
    CASE ('U')  
       factor = 1.
       offset = 100.
    CASE ('T')
       factor = 0.5
       offset = -150.
    END SELECT
    
    DO k=1,SIZE(field, 3)
       DO j=1,SIZE(field, 2)
          DO i=1,SIZE(field, 1)
             field(i,j,k) = ((k-1)*64 + (j-1)*16 + (i-1)*4 + it)*factor - offset
          END DO
       END DO
    END DO
    
  END SUBROUTINE read_3d_input_files
  
  !------------------------------------------------
  SUBROUTINE read_2d_input_files(it, varname, field)

    IMPLICIT NONE
    
    INTEGER, INTENT(IN) :: it
    CHARACTER(len=*), INTENT(IN) :: varname
    REAL, INTENT(OUT), DIMENSION(:,:) :: field
    
    INTEGER :: i, j
    REAL :: factor, offset
    CHARACTER(len=LEN(varname)) :: tmp
    
    tmp = TRIM(ADJUSTL(varname))
    SELECT CASE (tmp)
    CASE ('LATENT')  
       
       factor = 1.
       offset = 20.
    CASE ('TSURF')
       factor = 2.0
       offset = -220.
    CASE ('SOIL_WET')
       factor = 10.
       offset = 0.
    CASE ('PSURF')
       factor = 100.
       offset = -9.7e4
    END SELECT
    
    DO j=1,SIZE(field, 2)
       DO i=1,SIZE(field, 1)
          field(i,size(field,2)+1-j) = ((j-1)*16 + (i-1)*4 + it)*factor - offset
       END DO
    END DO

  END SUBROUTINE read_2d_input_files

END MODULE local_subs


!=======================================================
program testing
  use cmor_users_functions
  use local_subs
    implicit none
    integer error_flag
  INTEGER, PARAMETER :: ntimes = 2    ! number of time samples to process
  INTEGER, PARAMETER :: lon = 4       ! number of longitude grid cells  
  INTEGER, PARAMETER :: lat = 3       ! number of latitude grid cells
  INTEGER, PARAMETER :: lev = 5       ! number of standard pressure levels
  INTEGER, PARAMETER :: nst = 12      ! number of stations
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

  INTEGER igrid;
                          
!  uninitialized variables used in communicating with CMOR:
!  ---------------------------------------------------------

  INTEGER :: znondim_id, zfactor_id
  INTEGER, DIMENSION(n2d) :: var2d_ids
  INTEGER, DIMENSION(n3d) :: var3d_ids
  REAL, DIMENSION(lon,lat) :: data2d
  real, DIMENSION(lon*lat) ::  data1dtest
  REAL, DIMENSION(lon,lat,lev) :: data3d
  REAL, DIMENSION(lon*lat,lev) :: data2d_st
  DOUBLE PRECISION, DIMENSION(lat) :: alats
  DOUBLE PRECISION, DIMENSION(lon) :: alons
  DOUBLE PRECISION, DIMENSION(lev) :: plevs
  INTEGER, DIMENSION(lon*lat) :: station
  DOUBLE PRECISION, DIMENSION(lon*lat) :: st_lons
  DOUBLE PRECISION, DIMENSION(lon*lat) :: st_lats
  DOUBLE PRECISION, DIMENSION(1) :: time
  DOUBLE PRECISION, DIMENSION(2,1):: bnds_time
  DOUBLE PRECISION, DIMENSION(2,lat) :: bnds_lat
  DOUBLE PRECISION, DIMENSION(2,lon) :: bnds_lon
  DOUBLE PRECISION, DIMENSION(lev) :: zlevs
  DOUBLE PRECISION, DIMENSION(lev+1) :: zlev_bnds
  real, DIMENSION(lev) :: a_coeff
  DOUBLE PRECISION, DIMENSION(lev) :: b_coeff
  DOUBLE PRECISION :: p0
  DOUBLE PRECISION :: p0array(1)
  real, DIMENSION(lev+1) :: a_coeff_bnds
  DOUBLE PRECISION, DIMENSION(lev+1) :: b_coeff_bnds
  INTEGER :: ilon, ilat, ipres, ilev, itim, i, ist
  real :: missing

  !  Other variables:
  !  ---------------------
  
  INTEGER :: it, m, j,k

  j = CMOR_REPLACE
  k = CMOR_EXIT_ON_MAJOR
  error_flag = cmor_setup(inpath='Test', netcdf_file_action=j,&
       exit_control=k)
  print*,'Test code: done'
  error_flag = cmor_dataset(                                   &
       outpath='Test',                                         &
       experiment_id='2xCO2 equilibrium experiment',           &
       institution=                                            &
       'GICC (Generic International Climate Center, ' //       &
       'Geneva, Switzerland)',                                 &
       source='GICCM1 (2002): ' //                             &
       'atmosphere:  GICAM3 (gicam_0_brnchT_itea_2, T63L32); '// &
       'ocean: MOM (mom3_ver_3.5.2, 2x3L15); '             //  &
       'sea ice: GISIM4; land: GILSM2.5',                      &
       calendar='360_day',                                      &
       realization=1,                                          &
       contact = 'Rusty Koder (koder@middle_earth.net) ',      &
       history='Output from archive/giccm_03_std_2xCO2_2256.', &
       comment='Equilibrium reached after 30-year spin-up ' // &
       'after which data were output starting with nominal '// &
       'date of January 2030',                                 &
       references='Model described by Koder and Tolkien ' //   &
       '(J. Geophys. Res., 2001, 576-591).  Also '        //   &
       'see http://www.GICC.su/giccm/doc/index.html '     //   &
       ' 2XCO2 simulation described in Dorkey et al. '    //   &
       '(Clim. Dyn., 2003, 323-357.)', model_id="pcmdi-08a", &
       forcing="CO2")
  print*, 'Test code: done 2 lalala'
  
  call read_coords(alats, alons, plevs, bnds_lat, bnds_lon, station, st_lons, st_lats)
  

  print*, 'Test code: ok calling axis stuff station'
  ist = cmor_axis(  &
       table='IPCC_test_table_S',    &
       table_entry='station',       &
       units=' ',        &  
       length=nst,                   &
       coord_vals=station )      
       
   
  print*, 'Test code: ok calling axis stuff pressure',ist
  ipres = cmor_axis(  &
       table='IPCC_test_table_S',    &
       table_entry='pressure',       &
       units='Pa',                   &
       length=lev,                   &
       coord_vals=plevs)

  !   note that the time axis is defined next, but the time coordinate 
  !   values and bounds will be passed to cmor through function 
  !   cmor_write (later, below).

  print*, 'Test code: ok calling axis stuff time',ipres
  itim = cmor_axis(  &
       table='IPCC_test_table_S',    &
       table_entry='time',           &
       units='days since 2030-1-1',  &
       length=ntimes,                &
       interval='1 month')

  
  ! to make it a station data, we need to define the grid with lon/lat 
  ! information. CMOR will then add the 'longitude' and 'latitude'
  ! variables as assosiated to the station data.
  ! You do not set up lon/lat as axis variables for the station data     
  ! note - the first parameter has ot be an array (of dim=1 in this example)
  igrid = cmor_grid((/ist/), st_lats, st_lons)       
       
  write(*,'(a, 12f6.1)') 'lons: ',st_lons
  write(*,'(a, 12f6.1)') 'lats: ',st_lats
  
  !===============================================================
  print*, ' '
   

  !  Define variables appearing in IPCC table A1a (2-d variables)
  DO m=1,4
     print*, 'Test code: var:  ',entry2d(m)
     var2d_ids(m) = cmor_variable(    &
          table='IPCC_test_table_S',  &
          table_entry=entry2d(m),     & 
          units=units2d(m),           & 
          axis_ids=(/ igrid, itim /), &
          missing_value=missing,       &
          positive=positive2d(m),     &
          original_name=varin2d(m))   
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

    
!!$     
!!$     ! Cycle through the 2-d fields, retrieve the requested variable and 
!!$     ! append each to the appropriate netCDF file.
!!$     
     DO m=1,n2d
        
        ! The user must write the code that fills the arrays of data
        ! that will be passed to CMOR.  The following line is simply a
        ! a place-holder for the user's code, which should replace it.
        
        call read_2d_input_files(it, varin2d(m), data2d)    
        k = 1  
        DO j = 1, lat
          data1dtest(k:k+lat) = data2d(:,j) 
          k = k+lat+1 
        END DO   
        !write(*,'(a, 12(f8.1,1x))'), entry2d(m), data1dtest            

        ! append a single time sample of data for a single field to 
        ! the appropriate netCDF file.

        error_flag = cmor_write(                                  &
             var_id        = var2d_ids(m),                        &
             data          = data1dtest,                          &
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
