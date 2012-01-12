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
    
    INTEGER :: i, irev, ioff
    
    DO i = 1, SIZE(alons)
       irev = SIZE(alons)+1-i
       ioff = irev +2
       if (ioff>SIZE(alons)) ioff = ioff - SIZE(alons)
       alons(ioff) = (i-1)*360./SIZE(alons)
       bnds_lon(1,ioff) = (i - 1.5)*360./SIZE(alons)
       bnds_lon(2,ioff) = (i - 0.5)*360./SIZE(alons)
    END DO
    
    DO i = 1, SIZE(alats)
       alats(i) = i*10
       bnds_lat(1,i) = i*10. - 5.
       bnds_lat(2,i) = i*10. + 5.
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
    DOUBLE PRECISION, INTENT(OUT), DIMENSION(2) :: time_bnds
    
    time = (it-0.5)*30.
    time_bnds(1) = (it-1)*30.
    time_bnds(2) = it*30.
    
    RETURN
  END SUBROUTINE read_time
  
INCLUDE "reader_2D_3D.f90"

END MODULE local_subs


PROGRAM mip_contribution
!
!   Purpose:   To serve as a generic example of an application that
!       uses the "Climate Model Output Rewriter" (CMOR)

!    CMOR writes CF-compliant netCDF files.
!    Its use is required by many community-coordinated standard 
!       climate model experiments (e.g., AMIP, CMIP, CFMIP, APE, and
!       IPCC scenario runs)
!
!   Background information for this sample code:
!
!      Standard output requested by AMIP is listed in 6 different 
!   tables.  This sample code processes only 2 variables listed in AMIP 
!   Table 1a ("3-d" fields, containing monthly mean data that are a 
!   function of longitude, latitude, pressure and time) and only 3 
!   variables in AMIP Table 2 ("2-d" fields, containing monthly mean 
!   data that are a function of longitude, latitude, and time).  The 
!   extension to many more fields is trivial.
!
!      For this example, the user must fill in the sections of code that 
!   extract the 3-d and 2-d fields from his monthly mean "history" 
!   files (which usually contain many variables but only a single time 
!   slice).  The CMOR code will write each field in a separate file, but 
!   many monthly mean time-samples will be stored together.  These 
!   constraints partially determine the structure of the code.
!
!   Record of revisions:

!       Date        Programmer(s)           Description of change
!       ====        ==========              =====================
!      10/22/03     Rusty Koder              Original code
!       1/28/04     Les R. Koder             Revised to be consistent
!                                            with evolving code design

! include module that contains the user-accessible cmor functions.
  USE cmor_users_functions
  USE local_subs

  IMPLICIT NONE

  !   dimension parameters:
  ! ---------------------------------
  INTEGER, PARAMETER :: ntimes = 2    ! number of time samples to process
  INTEGER, PARAMETER :: lon = 4       ! number of longitude grid cells  
  INTEGER, PARAMETER :: lat = 3       ! number of latitude grid cells
  INTEGER, PARAMETER :: lev = 17       ! number of standard pressure levels
  INTEGER, PARAMETER :: n2d = 3       ! number of AMIP Table 2 fields to be
                                      !     output.
  INTEGER, PARAMETER :: n3d = 2       ! number of AMIP Table 1a fields to 
                                      !     be output.  

  !   Tables associating the user's variables with AMIP standard output 
  !   variables.  The user may choose to make this association in a 
  !   different way (e.g., by defining values of pointers that allow him 
  !   to directly retrieve data from a data record containing many 
  !   different variables), but in some way the user will need to map his 
  !   model output onto the Tables specifying the MIP standard output.

  ! ----------------------------------

                                ! My variable names for Table 1a fields
  CHARACTER (LEN=2), DIMENSION(n3d) :: varin3d=(/ 'U', 'T'/)
                                ! Units appropriate to my data
  CHARACTER (LEN=5), DIMENSION(n3d) :: &
                                  units3d=(/ 'm s-1',   'K    '  /)
  CHARACTER (LEN=4), DIMENSION(n3d) ::  &
                              positive3d= (/   '    ',       '    ' /)
                     ! Corresponding AMIP Table 1a entry (variable name) 
  CHARACTER (LEN=2), DIMENSION(n3d) :: entry3d = (/ 'ua', 'ta' /)

                                ! My variable names for Table 2 fields
  CHARACTER (LEN=8), DIMENSION(n2d) :: &
                          varin2d=(/ 'LATENT  ', 'TSURF   ', 'SOIL_WET' /)
                                ! Units appropriate to my data
   CHARACTER (LEN=6), DIMENSION(n2d) :: &
                          units2d=(/ 'W m-2 ', 'K     ', 'kg m-2' /)
   CHARACTER (LEN=4), DIMENSION(n2d) :: &
                      positive2d= (/  'down',  '    ',       '    ' /)
                     ! Corresponding AMIP Table 2 entry (variable name) 
  CHARACTER (LEN=5), DIMENSION(n2d) :: &
                        entry2d = (/ 'hfls ', 'tas  ', 'mrsos' /)

!  uninitialized variables used in communicating with CMOR:
!  ---------------------------------------------------------

  INTEGER :: error_flag
  INTEGER, DIMENSION(3) :: axis2d_ids
  INTEGER, DIMENSION(3) :: axis2d_ids2
  INTEGER, DIMENSION(4) :: axis3d_ids
  INTEGER, DIMENSION(n2d) :: var2d_ids
  INTEGER, DIMENSION(n3d) :: var3d_ids
  REAL, DIMENSION(lat,lon) :: data2d
  REAL, DIMENSION(lev,lon,lat) :: data3d
  DOUBLE PRECISION, DIMENSION(lat) :: alats
  DOUBLE PRECISION, DIMENSION(lon) :: alons
  DOUBLE PRECISION, DIMENSION(lev) :: plevs
  DOUBLE PRECISION :: time,bt
  DOUBLE PRECISION, DIMENSION(2):: bnds_time
  DOUBLE PRECISION, DIMENSION(2,lat) :: bnds_lat
  DOUBLE PRECISION, DIMENSION(2,lon) :: bnds_lon

!!$  REAL, DIMENSION(lat*lon*lev) :: tmp3d
!!$  REAL, DIMENSION(lat*lon) :: tmp2d

  !  Other variables:
  !  ---------------------
  
  INTEGER :: it, m
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
       '(Clim. Dyn., 2003, 323-357.)' , &
       model_id="GICCM1",forcing='TO',contact="Barry Bonds",&
       parent_experiment_rip="N/A",parent_experiment_id="N/A",branch_time=bt,institute_id="PCMDI");
  
  print*, 'returned from cmor_dataset'

  !  Define axes for 3-d fields

  print*, 'defining 3-d axes'
  
  axis2d_ids2(1) = cmor_axis(  &
       table='Tables/CMIP5_Lmon',    &
       table_entry='latitude',       &
       units='degrees_north',        &  
       length=lat,                   &
       coord_vals=alats,             & 
       cell_bounds=bnds_lat)              
  axis2d_ids2(2) = cmor_axis(  &
       table='Tables/CMIP5_Lmon',    &
       table_entry='longitude',      &
       length=lon,                   &
       units='degrees_east',         &
       coord_vals=alons,             &
       cell_bounds=bnds_lon)              
  axis2d_ids2(3) = cmor_axis(  &
       table='Tables/CMIP5_Lmon',    &
       table_entry='time',           &
       units='days since 1979-1-1',  &
       length=ntimes,                &
       interval='1 month')
  axis3d_ids(3) = cmor_axis(  &
       table='Tables/CMIP5_Amon',    &
       table_entry='latitude',       &
       units='degrees_north',        &  
       length=lat,                   &
       coord_vals=alats,             & 
       cell_bounds=bnds_lat)              
  axis3d_ids(2) = cmor_axis(  &
       table='Tables/CMIP5_Amon',    &
       table_entry='longitude',      &
       length=lon,                   &
       units='degrees_east',         &
       coord_vals=alons,             &
       cell_bounds=bnds_lon)           
  axis3d_ids(1) = cmor_axis(  &
       table='Tables/CMIP5_Amon',    &
       table_entry='plevs',       &
       units='Pa',                   &
       length=lev,                   &
       coord_vals=plevs)
  !   note that the time axis is defined next, but the time coordinate 
  !   values and bounds will be passed to cmor through function 
  !   cmor_write (below).
print*, 'before time '
  axis3d_ids(4) = cmor_axis(  &
       table='Tables/CMIP5_Amon',    &
       table_entry='time',           &
       units='days since 1979-1-1',  &
       length=ntimes,                &
       interval='30 days')
  
  print*, 'finished defining 3-d axes'

  !  Define axes for 2-d fields
  
  print*, 'defining 2-d axes'

  axis2d_ids(1) = axis3d_ids(3)          
  axis2d_ids(2) = axis3d_ids(2)           
  axis2d_ids(3) = axis3d_ids(4)
  
print*, 'finished defining 2-d axes'

  !  Define variables found in AMIP table 1a (3-d variables)
  
  DO m=1,n3d
     var3d_ids(m) = cmor_variable(    &   
          table='Tables/CMIP5_Amon',  &
          table_entry=entry3d(m),     &
          units=units3d(m),           &
          axis_ids=axis3d_ids,        &
          missing_value=1.0e20,       &
          positive=positive3d(m),     &
          original_name=varin3d(m))
  ENDDO
  

  !  Define variables found in AMIP table 2a (2-d variables)
  
  DO m=1,n2d
     if (m.eq.3) then
        var2d_ids(m) = cmor_variable(    &
             table='Tables/CMIP5_Lmon',  & 
             table_entry=entry2d(m),     & 
!!$          file_suffix='1979-2001',    &
             units=units2d(m),           & 
             axis_ids=axis2d_ids2,        &
             missing_value=1.0e20,       &
             positive=positive2d(m),     &
             original_name=varin2d(m))  
     else 
     var2d_ids(m) = cmor_variable(    &
          table='Tables/CMIP5_Amon',  & 
          table_entry=entry2d(m),     & 
          units=units2d(m),           & 
          axis_ids=axis2d_ids,        &
          missing_value=1.0e20,       &
          positive=positive2d(m),     &
          original_name=varin2d(m))   
  endif
  ENDDO

print*, 'completed everything up to writing output fields '
  
  !  Loop through history files (each containing several different fields, 
  !       but only a single month of data, averaged over the month).  Then 
  !       extract fields of interest and write these to netCDF files (with 
  !       one field per file, but all months included in the loop).
  
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
    
 

    DO m=1,n3d
        
        ! The user must write the code that fills the arrays of data
        ! that will be passed to CMOR.  The following line is simply a
        ! a place-holder for the user's code, which should replace it.

        call read_3d_input_files(it, varin3d(m), data3d)

       
        ! append a single time sample of data for a single field to 
        ! the appropriate netCDF file.
        
        error_flag = cmor_write(var_id =        var3d_ids(m),   &
                                data =          RESHAPE(data3d, (/ lat*lon*lev /)),          &
                                ntimes_passed = 1,              &
                                time_vals =     (/ time /),           &
                                time_bnds =     RESHAPE(bnds_time, (/ 2,1 /)))

print*, 'after writing variable, ', var3d_ids(m)
print*, '    error flag = ', error_flag

        IF (error_flag < 0) THEN
           ! write diagnostic messages to standard output device
           write(*,*) ' Error encountered writing AMIP Table 1a ' &
                // 'field ', entry3d(m), ', which I call ', varin3d(m)
           write(*,*) ' Was processing time sample: ', time
           
!!$            error_flag = cmor_error_trace(device=0)
           
        END IF

     END DO
     
     ! Cycle through the 2-d fields, retrieve the requested variable and 
     ! append each to the appropriate netCDF file.
     
     DO m=1,n2d
        
        ! The user must write the code that fills the arrays of data
        ! that will be passed to CMOR.  The following line is simply a
        ! a place-holder for the user's code, which should replace it.
        
        call read_2d_input_files(it, varin2d(m), data2d)                  

        ! append a single time sample of data for a single field to 
        ! the appropriate netCDF file.

        error_flag = cmor_write(var_id =        var2d_ids(m),   &
                                data =          RESHAPE(data2d, (/ lat*lon /)),        &
                                ntimes_passed = 1,              &
                                time_vals =     (/ time /),           &
                                time_bnds =     RESHAPE(bnds_time, (/ 2,1 /)))
        
print*, 'after writing variable, ', var2d_ids(m)
print*, '    error flag = ', error_flag
       IF (error_flag < 0) THEN
           ! write diagnostic messages to standard output device
           write(*,*) ' Error encountered writing AMIP Table 2 ' &
                // 'field ', entry2d(m), ', which I call ', varin2d(m)
           write(*,*) ' Was processing time sample: ', time 
           
!!$            error_flag = cmor_error_trace(device=0)
           
        END IF
        
     END DO
     
  END DO time_loop
  
  !   Close all files opened by CMOR.
  
  error_flag = cmor_close()  

print*, '******************************'
print*, ' '
print*, 'CMOR COMPLETED SUCCESSFULLY '   
print*, ' '
print*, '******************************'

END PROGRAM mip_contribution

