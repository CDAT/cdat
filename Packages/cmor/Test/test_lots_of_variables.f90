!!$pgf90 -I/work/NetCDF/5.1/include -L/work/NetCDF/5.1/lib -l netcdf -L. -l cmor Test/test1.f90 -IModules -o cmor_test
!!$pgf90 -g -I/pcmdi/charles_work/NetCDF//include -L/pcmdi/charles_work/NetCDF//lib -lnetcdf -module Modules -IModules -L. -lcmor Test/test1.f90 -o cmor_test

MODULE local_subs

  PRIVATE
  PUBLIC read_coords, read_time, read_3d_input_files, read_2d_input_files
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
       alats(i) = i*10
       bnds_lat(1,i) = i*10. - 5.
       bnds_lat(2,i) = i*10. + 5.
    END DO
    
    DO i = 1, SIZE(plevs)
       plevs(i) = i*1.0e4
    END DO
    
    RETURN
  END SUBROUTINE read_coords

  SUBROUTINE read_time(it, time, time_bnds)
    
    IMPLICIT NONE
    
    INTEGER, INTENT(IN) :: it
    DOUBLE PRECISION, INTENT(OUT) :: time
    DOUBLE PRECISION, INTENT(OUT), DIMENSION(2) :: time_bnds
    
    time = (it-0.5)*30.
    time_bnds(1) = (it-1)*30.
    time_bnds(2) = it*30.
    
    RETURN
  END SUBROUTINE read_time
    
  SUBROUTINE read_2d_input_files(it, varname, field)                  
    
    INTEGER, INTENT(IN) :: it
    CHARACTER(len=*), INTENT(IN) :: varname
    REAL, INTENT(OUT), DIMENSION(:,:) :: field
    
    INTEGER :: i, j
    REAL :: factor, offset
    
    
    SELECT CASE (TRIM(ADJUSTL(varname)))
    CASE ('LATENT')  
       
       factor = 1.5
       offset = 20.
    CASE ('TSURF')
       factor = 2.2
       offset = -220.
    CASE ('SOIL_WET')
       factor = 10.
       offset = 0.
    END SELECT
    
    DO j=1,SIZE(field, 1)
       DO i=1,SIZE(field, 2)
          field(j,i) = ((j-1)*16 + (i-1)*4 + it)*factor - offset
       END DO
    END DO

  END SUBROUTINE read_2d_input_files

END MODULE local_subs


PROGRAM mip_contribution

! include module that contains the user-accessible cmor functions.
  USE cmor_users_functions
  USE local_subs

  IMPLICIT NONE

  !   dimension parameters:
  ! ---------------------------------
  INTEGER, PARAMETER :: ntimes = 2    ! number of time samples to process
  INTEGER, PARAMETER :: lon = 4       ! number of longitude grid cells  
  INTEGER, PARAMETER :: lat = 3       ! number of latitude grid cells
  INTEGER, PARAMETER :: n2d = 1       ! number of AMIP Table 2 fields to be
                                      !     output.

  !   Tables associating the user's variables with AMIP standard output 
  !   variables.  The user may choose to make this association in a 
  !   different way (e.g., by defining values of pointers that allow him 
  !   to directly retrieve data from a data record containing many 
  !   different variables), but in some way the user will need to map his 
  !   model output onto the Tables specifying the MIP standard output.

  ! ----------------------------------

                                ! My variable names for Table 1a fields
                                ! My variable names for Table 2 fields
  CHARACTER (LEN=8), DIMENSION(n2d) :: &
                          varin2d=(/ 'LATENT  ' /)
                                ! Units appropriate to my data
   CHARACTER (LEN=6), DIMENSION(n2d) :: &
                          units2d=(/ 'W m-2 ' /)
   CHARACTER (LEN=4), DIMENSION(n2d) :: &
                      positive2d= (/  'down' /)
                     ! Corresponding AMIP Table 2 entry (variable name) 
  CHARACTER (LEN=5), DIMENSION(n2d) :: &
                        entry2d = (/ 'hfls ' /)

!  uninitialized variables used in communicating with CMOR:
!  ---------------------------------------------------------

  INTEGER :: error_flag
  INTEGER, PARAMETER :: ntest=400
  INTEGER, DIMENSION(3) :: axis2d_ids
  INTEGER, DIMENSION(n2d+ntest) :: var2d_ids
  REAL, DIMENSION(lat,lon) :: data2d
  DOUBLE PRECISION, DIMENSION(lat) :: alats
  DOUBLE PRECISION, DIMENSION(lon) :: alons
  DOUBLE PRECISION, DIMENSION(5) :: plevs
  DOUBLE PRECISION :: time,bt
  DOUBLE PRECISION, DIMENSION(2):: bnds_time
  DOUBLE PRECISION, DIMENSION(2,lat) :: bnds_lat
  DOUBLE PRECISION, DIMENSION(2,lon) :: bnds_lon

!!$  REAL, DIMENSION(lat*lon*lev) :: tmp3d
!!$  REAL, DIMENSION(lat*lon) :: tmp2d

  !  Other variables:
  !  ---------------------
  
  INTEGER :: it, m
  
    character (len=200) :: msg,msg2
    bt=0.
  ! ================================
  !  Execution begins here:
  ! ================================
  
  ! Read coordinate information from model into arrays that will be passed 
  !   to CMOR.
  ! Read latitude, longitude, and pressure coordinate values into 
  !   alats, alons, and plevs, respectively.  Also generate latitude and 
  !   longitude bounds, and store in bnds_lat and bnds_lon, respectively.
  
  !   The user must write the subroutine that fills the coordinate arrays 
  !   and their bounds with actual data.  The following line is simply a
  !   a place-holder for the user's code, which should replace it.
  
  !  *** possible user-written call ***	
  
  print*, 'calling read_coords '

  call read_coords(alats, alons, plevs, bnds_lat, bnds_lon)

  print*, 'returned from read_coords'
  
  ! Specify path where tables can be found, indicate that existing netCDF 
  !    files should not be overwritten, and instruct CMOR to error exit on 
  !    encountering errors of any severity.
  
  error_flag = cmor_setup(inpath='Test',   &
       netcdf_file_action='replace',                                       &
       set_verbosity=1,                                                    &
       exit_control=1)
  
  ! Define dataset as output from the GICC model (first member of an
  !   ensemble of simulations) runcmor_write under IPCC 2xCO2 equilibrium experiment 
  !   conditions, and provide information to be included as attributes in 
  !   all CF-netCDF files written as part of this dataset.

  print*, 'calling cmor_dataset'
  error_flag = cmor_dataset(                                   &
       outpath='Test',         &
       experiment_id='abrupt 4XCO2',           &
       institution=                                            &
       'GICC (Generic International Climate Center, ' //       &
       ' Geneva, Switzerland)',                                &
       source='GICCM1  2002(giccm_0_brnchT_itea_2, T63L32)',    &
       calendar='360_day',                                      &
       realization=1,                                          &
       history='Output from archive/giccm_03_std_2xCO2_2256.', &
       comment='Equilibrium reached after 30-year spin-up ' // &
       'after which data were output starting with nominal '// &
       'date of January 2030',                                 &
       references='Model described by Koder and Tolkien ' //   &
       '(J. Geophys. Res., 2001, 576-591).  Also ' //          &
       'see http://www.GICC.su/giccm/doc/index.html '  //      &
       ' 2XCO2 simulation described in Dorkey et al. '//       &
       '(Clim. Dyn., 2003, 323-357.)',model_id="GICCM1", &
       forcing='TO',contact="Barry Bonds",institute_id="PCMDI",&
       parent_experiment_rip="N/A",parent_experiment_id="N/A",branch_time=bt)

  !error_flag  = cmor_set_cur_dataset_attribute("initialization_method","4")
  !error_flag  = cmor_set_cur_dataset_attribute("physics_version","6")

  print*, 'returned from cmor_dataset'

  !  Define axes for 3-d fields

  print*, 'defining axes'
  
  axis2d_ids(1) = cmor_axis(  &
       table='CMIP5_Amons',    &
       table_entry='latitude',       &
       units='degrees_north',        &  
       length=lat,                   &
       coord_vals=alats,             & 
       cell_bounds=bnds_lat)              
  axis2d_ids(2) = cmor_axis(  &
       table='CMIP5_Amons',    &
       table_entry='longitude',      &
       length=lon,                   &
       units='degrees_east',         &
       coord_vals=alons,             &
       cell_bounds=bnds_lon)              
  !   note that the time axis is defined next, but the time coordinate 
  !   values and bounds will be passed to cmor through function 
  !   cmor_write (below).
print*, 'before time '
  axis2d_ids(3) = cmor_axis(  &
       table='CMIP5_Amons',    &
       table_entry='time',           &
       units='days since 1979-1-1',  &
       length=ntimes,                &
       interval='1 month')
  
  print*, 'finished defining axes'

  

  !  Define variables found in AMIP table 2a (2-d variables)
  
  DO m=1,ntest
     write(msg,*)  m-1
     msg2 = trim(entry2d(1)(1:4)//adjustl(msg))
     print*,'Test Code: defining variable: :',msg2
     var2d_ids(m) = cmor_variable(    &
          table='CMIP5_Amons',  & 
          table_entry=msg2,     & 
!!$          file_suffix='1979-2001',    &
          units=units2d(1),           & 
          axis_ids=axis2d_ids,        &
          missing_value=1.0e20,       &
          positive=positive2d(1),     &
          original_name=varin2d(1))   
  ENDDO

  print*, 'completed everything up to writing output fields '
  
  !  Loop through history files (each containing several different fields, 
  !       but only a single month of data, averaged over the month).  Then 
  !       extract fields of interest and write these to netCDF files (with 
  !       one field per file, but all months included in the loop).
  
  DO m=1,ntest
     time_loop: DO it=1, ntimes
        
        ! In the following loops over the 3d and 2d fields, the user-written    
        ! subroutines (read_3d_input_files and read_2d_input_files) retrieve 
        ! the requested AMIP table 1a and table 2 fields and store them in 
        ! data3d and data2d, respectively.  In addition a user-written code 
        ! (read_time) retrieves the time and time-bounds associated with the 
        ! time sample (in units of 'days since 1970-1-1', consistent with the 
        ! axis definitions above).  The bounds are set to the beginning and 
        ! the end of the month retrieved, indicating the averaging period.
        
        ! The user must write a code to obtain the times and time-bounds for
        !   the time slice.  The following line is simply a place-holder for
        !   the user's code, which should replace it.
        
        call read_time(it, time, bnds_time)
        
        ! Cycle through the 3-d fields, retrieve the requested variable and 
        ! append each to the appropriate netCDF file.
        
        
        
        
        ! Cycle through the 2-d fields, retrieve the requested variable and 
        ! append each to the appropriate netCDF file.
        
        
        ! The user must write the code that fills the arrays of data
        ! that will be passed to CMOR.  The following line is simply a
        ! a place-holder for the user's code, which should replace it.
        
        call read_2d_input_files(it, varin2d(1), data2d)                  
        
        ! append a single time sample of data for a single field to 
        ! the appropriate netCDF file.
        
        print*, RESHAPE(bnds_time, (/ 2,1 /))
        error_flag = cmor_write(var_id =        var2d_ids(m),   &
             data =          RESHAPE(data2d, (/ lat*lon /)),        &
             ntimes_passed = 1,              &
             time_vals =     (/ time /),           &
             time_bnds =     RESHAPE(bnds_time, (/ 2,1 /)))
        
        print*, 'after writing variable, ', var2d_ids(m)
        IF (error_flag < 0) THEN
           ! write diagnostic messages to standard output device
           write(*,*) ' Error encountered writing AMIP Table 2 ' &
                // 'field ', entry2d(m), ', which I call ', varin2d(m)
           write(*,*) ' Was processing time sample: ', time 
           
!!$            error_flag = cmor_error_trace(device=0)
           
        END IF
        
     END DO time_loop
     error_flag = cmor_close(var2d_ids(m))
  END DO
  
  !   Close all files opened by CMOR.
  
  error_flag = cmor_close()  

print*, '******************************'
print*, ' '
print*, 'CMOR COMPLETED SUCCESSFULLY '   
print*, ' '
print*, '******************************'

END PROGRAM mip_contribution

