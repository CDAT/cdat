!!$pgf90 -I/work/NetCDF/5.1/include -L/work/NetCDF/5.1/lib -l netcdf -L. -l cmor Test/test_dimensionless.f90 -IModules -o cmor_test
!!$pgf90 -g -I/pcmdi/charles_work/NetCDF/include -L/pcmdi/charles_work/NetCDF/lib -lnetcdf -module Modules -IModules -L. -lcmor -I/pcmdi/charles_work/Unidata/include -L/pcmdi/charles_work/Unidata/lib -ludunits Test/test_dimensionless.f90 -o cmor_test

MODULE local_subs

  USE cmor_users_functions
  PRIVATE
  PUBLIC read_coords, read_time, read_1d_input_files
CONTAINS
  
  SUBROUTINE read_coords(alats, bnds_lat)

    IMPLICIT NONE
    
    DOUBLE PRECISION, INTENT(OUT), DIMENSION(:) :: alats
    DOUBLE PRECISION, INTENT(OUT), DIMENSION(:,:) :: bnds_lat
    
    INTEGER :: i
    
    DO i = 1, SIZE(alats)
       alats(i) = i*10
       bnds_lat(1,i) = i*10. - 5.
       bnds_lat(2,i) = i*10. + 5.
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
  
include "reader_2D_3D.f90"

END MODULE local_subs


PROGRAM test_region
!
!   Purpose:   To serve as a generic example of an application that
!       uses the "Climate Model Output Rewriter" (CMOR)

!    CMOR writes CF-compliant netCDF files.
!    Its use is strongly encouraged by the IPCC and is intended for use 
!       by those participating in many community-coordinated standard 
!       climate model experiments (e.g., AMIP, CMIP, CFMIP, PMIP, APE,
!       etc.)
!
!   Background information for this sample code:
!
!      Atmospheric standard output requested by IPCC are listed in 
!   tables available on the web.  Monthly mean output is found in
!   tables A1a and A1c.  This sample code processes only two 3-d 
!   variables listed in table A1c ("monthly mean atmosphere 3-D data" 
!   and only four 2-d variables listed in table A1a ("monthly mean 
!   atmosphere + land surface 2-D (latitude, longitude) data").  The 
!   extension to many more fields is trivial.
!
!      For this example, the user must fill in the sections of code that 
!   extract the 3-d and 2-d fields from his monthly mean "history" 
!   files (which usually contain many variables but only a single time 
!   slice).  The CMOR code will write each field in a separate file, but 
!   many monthly mean time-samples will be stored together.  These 
!   constraints partially determine the structure of the code.
!
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
  INTEGER, PARAMETER :: reg = 4       ! number of regions 
  INTEGER, PARAMETER :: lat = 3       ! number of latitude grid cells
  INTEGER, PARAMETER :: n1d = 1       ! number of IPCC Table O1 fields to be
                                      !     output.
  !
  !   Tables associating the user's variables with IPCC standard output 
  !   variables.  The user may choose to make this association in a 
  !   different way (e.g., by defining values of pointers that allow him 
  !   to directly retrieve data from a data record containing many 
  !   different variables), but in some way the user will need to map his 
  !   model output onto the Tables specifying the MIP standard output.

  ! ----------------------------------

                                ! My variable names for IPCC Table O1a fields
  CHARACTER (LEN=5), DIMENSION(n1d) :: &
                                 varin1d=(/ 'OFLUX' /)

                                ! Units appropriate to my data
  CHARACTER (LEN=1), DIMENSION(n1d) :: &
                                  units1d=(/ 'W' /)

                     ! Corresponding IPCC Table O1a entry (variable name) 
  CHARACTER (LEN=8), DIMENSION(n1d) :: entry1d = (/ 'htovgyre' /)


!  uninitialized variables used in communicating with CMOR:
!  ---------------------------------------------------------

  INTEGER :: error_flag
  INTEGER, DIMENSION(n1d) :: var1d_ids
  REAL, DIMENSION(lat,reg) :: data1d
  DOUBLE PRECISION, DIMENSION(lat) :: alats
  DOUBLE PRECISION, DIMENSION(1) :: time
  DOUBLE PRECISION, DIMENSION(2,1):: bnds_time
  DOUBLE PRECISION, DIMENSION(2,lat) :: bnds_lat
  INTEGER :: ilat, ireg, itim
  double precision bt

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
  !   Note that all variable names in this code can be freely chosen by
  !   the user.

  !   The user must write the subroutine that fills the coordinate arrays 
  !   and their bounds with actual data.  The following line is simply a
  !   a place-holder for the user's code, which should replace it.
  
  !  *** possible user-written call ***
  
  call read_coords(alats, bnds_lat)
  
  ! Specify path where tables can be found and indicate that existing 
  !    netCDF files should not be overwritten.
  
  error_flag = cmor_setup(inpath='Test', netcdf_file_action='replace')
  
  ! Define dataset as output from the GICC model (first member of an
  !   ensemble of simulations) run under IPCC 2xCO2 equilibrium
  !   experiment conditions, and provide information to be included as 
  !   attributes in all CF-netCDF files written as part of this dataset.

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
       '(Clim. Dyn., 2003, 323-357.)' , model_id="GICCM1", &
       forcing='TO',contact="Barry Bonds",institute_id="PCMDI",&
       parent_experiment_rip="N/A",parent_experiment_id="N/A",branch_time=bt)
    
  !  Define all axes that will be needed
  
  ilat = cmor_axis(  &
       table='Tables/CMIP5_Omon',    &
       table_entry='latitude',       &
       units='degrees_north',        &  
       length=lat,                   &
       coord_vals=alats,             & 
       cell_bounds=bnds_lat)        
      
  ireg = cmor_axis(  &
       table='Tables/CMIP5_Omon',        &
       table_entry='basin',         &
       length=reg,                   &
       units='none',                 &
       coord_vals= (/ "atlantic_arctic_ocean", "indian_pacific_ocean ", "pacific_ocean        ", &
       "global_ocean         " /) )   
        
  !   note that the time axis is defined next, but the time coordinate 
  !   values and bounds will be passed to cmor through function 
  !   cmor_write (later, below).

  itim = cmor_axis(  &
       table='Tables/CMIP5_Omon',        &
       table_entry='time',           &
       units='days since 2030-1-1',  &
       length=ntimes,                &
       interval='1 month')
  

  !  Define variables appearing in IPCC table O1a (functions of lat and reg)
  
  DO m=1,n1d
     var1d_ids(m) = cmor_variable(    &
          table='Tables/CMIP5_Omon',      &
          table_entry=entry1d(m),     &
          units=units1d(m),           &
          axis_ids=(/ ilat, ireg, itim /), &
          missing_value=1.0e28,       &
          original_name=varin1d(m))
  ENDDO
  

  PRINT*, ' '
  PRINT*, 'completed everything up to writing output fields '
  PRINT*, ' '

  !  Loop through history files (each containing several different fields, 
  !       but only a single month of data, averaged over the month).  Then 
  !       extract fields of interest and write these to netCDF files (with 
  !       one field per file, but all months included in the loop).
  
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

    ! Cycle through the 1-d fields (functions of lat and region), 
    ! and retrieve the requested variable and append each to the 
    ! appropriate netCDF file.

    DO m=1,n1d
        
        ! The user must write the code that fills the arrays of data
        ! that will be passed to CMOR.  The following line is simply a
        ! a place-holder for the user's code, which should replace it.

        call read_1d_input_files(it, varin1d(m), data1d)
       
        ! append a single time sample of data for a single field to 
        ! the appropriate netCDF file.
        
        error_flag = cmor_write(                                  &
             var_id        = var1d_ids(m),                        &
             data          = data1d,                              &
             ntimes_passed = 1,                                   &
             time_vals     = time,                                &
             time_bnds     = bnds_time  )
        
        IF (error_flag < 0) THEN
           ! write diagnostic messages to standard output device
           write(*,*) ' Error encountered writing IPCC Table O ' &
                // 'field ', entry1d(m), ', which I call ', varin1d(m)
           write(*,*) ' Was processing time sample: ', time
                      
        END IF

     END DO
     
     
  END DO time_loop
  
  !   Close all files opened by CMOR.
 
  print*, 'Ok wrote everythin' 
  error_flag = cmor_close()  

  print*, ' '
  print*, '******************************'
  print*, ' '
  print*, 'ipcc_test_code executed to completion '   
  print*, ' '
  print*, '******************************'
  
END PROGRAM test_region

