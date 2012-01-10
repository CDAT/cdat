!!$pgf90 -I/work/NetCDF/5.1/include -L/work/NetCDF/5.1/lib -l netcdf -L. -l cmor Test/test_dimensionless.f90 -IModules -o cmor_test
!!$pgf90 -g -I/pcmdi/charles_work/NetCDF/include -L/pcmdi/charles_work/NetCDF/lib -lnetcdf -module Modules -IModules -L. -lcmor -I/pcmdi/charles_work/Unidata/include -L/pcmdi/charles_work/Unidata/lib -ludunits Test/test_dimensionless.f90 -o cmor_test

MODULE local_subs

  USE cmor_users_functions
  PRIVATE
  PUBLIC read_coords, read_time, read_3d_input_files, read_2d_input_files
CONTAINS
  
  SUBROUTINE read_coords(alats, alons, bnds_lat, bnds_lon)

    IMPLICIT NONE
    
    DOUBLE PRECISION, INTENT(OUT), DIMENSION(:) :: alats
    DOUBLE PRECISION, INTENT(OUT), DIMENSION(:) :: alons
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
      
    RETURN
  END SUBROUTINE read_coords

  SUBROUTINE read_time(it, time, time_bnds)
    
    IMPLICIT NONE
    
    INTEGER, INTENT(IN) :: it
    DOUBLE PRECISION, INTENT(OUT), DIMENSION(:) :: time
    DOUBLE PRECISION, INTENT(OUT), DIMENSION(:,:) :: time_bnds

    INTEGER :: i
    
    DO i=1,SIZE(time)
       time(i) = (it-1)*30.+ (i-.5)/SIZE(time)
       time_bnds(1,i) = (it-1)*30.+ (i-1)*1.0/SIZE(time)
       time_bnds(2,i) = (it-1)*30.+ i*1.0/SIZE(time) + 29.
       print*, i,time_bnds(1,i),time(i),time_bnds(2,i)
    END DO
    
    RETURN
  END SUBROUTINE read_time
  

  SUBROUTINE read_2d_input_files(it, varname, field)

    IMPLICIT NONE
    
    INTEGER, INTENT(IN) :: it
    CHARACTER(len=*), INTENT(IN) :: varname
    REAL, INTENT(OUT), DIMENSION(:,:,:) :: field
    
    INTEGER :: i, j, k
    REAL :: factor, offset
    CHARACTER(len=LEN(varname)) :: tmp
    
    tmp = TRIM(ADJUSTL(varname))

    factor = 2.2
    offset = -220.
    
    DO k=1,SIZE(field,3)
       DO j=1,SIZE(field, 2)
          DO i=1,SIZE(field, 1)
             field(i,size(field,2)+1-j,k) = ((k-1)*0.005 + (j-1)*16 + (i-1)*4 + it)*factor - offset
          END DO
       END DO
    END DO

  END SUBROUTINE read_2d_input_files

END MODULE local_subs


PROGRAM ipcc_test_code
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
  INTEGER, PARAMETER :: lon = 4       ! number of longitude grid cells  
  INTEGER, PARAMETER :: lat = 3       ! number of latitude grid cells
  INTEGER, PARAMETER :: n2d = 1       ! number of IPCC Table A1a fields to be
                                      !     output.
  !   Tables associating the user's variables with IPCC standard output 
  !   variables.  The user may choose to make this association in a 
  !   different way (e.g., by defining values of pointers that allow him 
  !   to directly retrieve data from a data record containing many 
  !   different variables), but in some way the user will need to map his 
  !   model output onto the Tables specifying the MIP standard output.

  ! ----------------------------------

                                ! My variable names for IPCC Table A1a fields
  CHARACTER (LEN=4), DIMENSION(n2d) :: &
                  varin2d=(/  'TSURF' /)

                                ! Units appropriate to my data
   CHARACTER (LEN=1), DIMENSION(n2d) :: &
                          units2d=(/ 'K'/)

   CHARACTER (LEN=4), DIMENSION(n2d) :: &
                      positive2d= (/  '    ' /)

                     ! Corresponding IPCC Table A1a entry (variable name) 
  CHARACTER (LEN=10), DIMENSION(n2d) :: &
                        entry2d = (/ 'tasDiurnal' /)

!  uninitialized variables used in communicating with CMOR:
!  ---------------------------------------------------------

  INTEGER :: error_flag
  INTEGER, DIMENSION(n2d) :: var2d_ids
  REAL, DIMENSION(lon,lat,24) :: data2d
  DOUBLE PRECISION, DIMENSION(lat) :: alats
  DOUBLE PRECISION, DIMENSION(lon) :: alons
  DOUBLE PRECISION, DIMENSION(24) :: time
  DOUBLE PRECISION, DIMENSION(2,24):: bnds_time
  DOUBLE PRECISION, DIMENSION(2,lat) :: bnds_lat
  DOUBLE PRECISION, DIMENSION(2,lon) :: bnds_lon
  INTEGER :: ilon, ilat, ipres, itim


  !  Other variables:
  !  ---------------------
  
  INTEGER :: it, m  
  
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
  
  call read_coords(alats, alons, bnds_lat, bnds_lon)
  
  ! Specify path where tables can be found and indicate that existing 
  !    netCDF files should not be overwritten.
  
  error_flag = cmor_setup(inpath='Test', netcdf_file_action='replace',logfile="climatology_test_code.LOG")
  
  ! Define dataset as output from the GICC model (first member of an
  !   ensemble of simulations) run under IPCC 2xCO2 equilibrium
  !   experiment conditions, and provide information to be included as 
  !   attributes in all CF-netCDF files written as part of this dataset.

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
       '(Clim. Dyn., 2003, 323-357.)', &
       model_id = 'pcmdi-01', &
       forcing = 'atm')
  
  !  Define all axes that will be needed

  ilat = cmor_axis(  &
       table='climatology_test_table_A',    &
       table_entry='latitude',       &
       units='degrees_north',        &  
       length=lat,                   &
       coord_vals=alats,             & 
       cell_bounds=bnds_lat)        
      
  ilon = cmor_axis(  &
       table='climatology_test_table_A',    &
       table_entry='longitude',      &
       length=lon,                   &
       units='degrees_east',         &
       coord_vals=alons,             &
       cell_bounds=bnds_lon)      
        
  !   note that the time axis is defined next, but the time coordinate 
  !   values and bounds will be passed to cmor through function 
  !   cmor_write (later, below).

  itim = cmor_axis(  &
       table='climatology_test_table_A',    &
       table_entry='time',           &
       units='days since 2030-1-1',  &
       length=ntimes*24                &
       )
  

  !  Define variables appearing in IPCC table A1a (2-d variables)
  
  DO m=1,n2d
     var2d_ids(m) = cmor_variable(    &
          table='climatology_test_table_A',  &
          table_entry=entry2d(m),     & 
          units=units2d(m),           & 
          axis_ids=(/ ilon, ilat, itim /), &
          missing_value=1.0e28,       &
          positive=positive2d(m),     &
          original_name=varin2d(m))   
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
     
     call read_time(it, time, bnds_time)

     
     ! Cycle through the 2-d fields, retrieve the requested variable and 
     ! append each to the appropriate netCDF file.
     
     DO m=1,n2d
        
        ! The user must write the code that fills the arrays of data
        ! that will be passed to CMOR.  The following line is simply a
        ! a place-holder for the user's code, which should replace it.
        
        call read_2d_input_files(it, varin2d(m), data2d)                  

        ! append a single time sample of data for a single field to 
        ! the appropriate netCDF file.

        print*, 'CALLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLL'
        print*, 'times:',time(1),time(2)
        print*, 'btimes:',bnds_time(1,1),bnds_time(2,1),bnds_time(1,2),bnds_time(2,2)
        error_flag = cmor_write(                                  &
             var_id        = var2d_ids(m),                        &
             data          = data2d,                              &
             ntimes_passed = 24,                                   &
             time_vals     = time,                                &
             time_bnds     = bnds_time  )
        
       IF (error_flag < 0) THEN
           ! write diagnostic messages to standard output device
           write(*,*) ' Error encountered writing climatology test Table A ' &
                // 'field ', entry2d(m), ', which I call ', varin2d(m)
           write(*,*) ' Was processing time sample: ', time 
                      
        END IF
        
     END DO
     
  END DO time_loop
  
  !   Close all files opened by CMOR.
  
  error_flag = cmor_close()  

  print*, ' '
  print*, '******************************'
  print*, ' '
  print*, 'ipcc_test_code executed to completion '   
  print*, ' '
  print*, '******************************'
  
END PROGRAM ipcc_test_code

