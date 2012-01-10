      program rewrite_harvardf_data

!  Use CMOR routines to rewrite Harvard Forest obs data into CF-standard netCDF.
!  Start with data in easy-to-read ASCII text (Mathematica input) format, which
!  was produced by the script read_harvard_forest_obs_dat.py.

!  Note: first line of each input data file is ASCII text to be skipped over.

!		Curt Covey		January 2006

USE cmor_users_functions

      character(len = 25) :: input_file   = 'Test/tas_harvardf.txt'
      character(len = 20) :: output_file  = 'tas_harvardf.nc'
      character(len = 20) :: units_string = 'K'
!     character(len = 20) :: input_table  = 'IPCC_test_table_A'
!!$      character(len = 20) :: input_table  = 'IPCC_table_A2'
      character(len = 20) :: input_table  = 'Test/Curts_table'

      character :: first_line    ! throwaway

      integer   :: iaxis(3)      ! identifier for axes (time, latitude, longitude)

!!$      integer, parameter :: ndatalines = 4383 ! # lines in file that contain data
      integer, parameter :: ndatalines = 10 ! # lines in file that contain data
                                              ! ( = total # lines in file - 1)
      integer, parameter :: np1 = ndatalines + 1

      double precision      :: lat(1)        ! single-member array to hold latitude
      double precision      :: lon(1)        ! single-member array to hold longitude
      double precision      :: t(ndatalines) ! array to hold time coordinate
      double precision      :: tbounds(np1)  ! array to hold time coordinate cell bounds
      real      :: x(ndatalines,1,1) ! array to hold data from each input data file
!    -Harvard Forest is located at 42.5 deg N, 72.2 deg W ...
      lat(1) = 42.5d0
      lon(1) = -72.2d0
!     ... and is 3000 acres ~ a circle with radius of 0.02 deg lat/lon:
      latlonrad = 0.02
      do i = 1, ndatalines
         t(i)       = i - 1.0  ! time coord  = "days since" =  0,       1, ...
         tbounds(i) = i - 1.5  ! time bounds =           -0.5,    +0.5,    ...
      enddo
      tbounds(np1) = ndatalines - 0.5

      print *, 'Opening ', input_file, '...'
      open(10, file = input_file)
      read(10, '(A)') first_line
      print    '(A)', 'First line begins with: ', first_line
      do i = 1, ndatalines
         read(10, '(F6.2)') x(i,1,1)
!        print *, x(i,1,1)
      enddo

      print *, 'Initializing CMOR ...'
      ierrorflag = cmor_setup(netcdf_file_action = 'replace')

      print *, 'Identifying output data sets for CMOR ...'
      ierrorflag = cmor_dataset(                               &
                      outpath       = 'Test',                    &
                      experiment_id =                          &
      'climate of the 20th Century experiment (20C3M)',        &
                      institution   =                          &
      'Berkeley CAS (Center for Atmospheric Science)',         &
                      source        =                          &
      'ftp://ftp.as.harvard.edu/pub/nigec/HU_Wofsy/hf_data',   &
                      calendar      = 'gregorian',             &
                      contact       =                          &
      'Jasmin John, Berkeley CAS <jjohn@berkeley.edu>')

!     print *, ' ** Time coordinate vector:', t
!     print *, ' ** Time bounds vector:',     tbounds

      print *, 'Defining coordinates for CMOR output data ...',t
      iaxis(1)   = cmor_axis(                                  &
                      table       = input_table,               &
                      table_entry = 'time',                    &
                      units       = 'days since 1992-1-1',     &
                      length      = ndatalines,                &
                      coord_vals  = t,                         &
                      cell_bounds = tbounds)

      iaxis(2)   = cmor_axis(                                  &
                      table       = input_table,               &
                      table_entry = 'latitude',                &
                      units       = 'degrees_north',           &
                      length      = 1,                         &
                      coord_vals  = lat)                       

      iaxis(3)   = cmor_axis(                                  &
                      table       = input_table,               &
                      table_entry = 'longitude',               &
                      units       = 'degrees_east',            &
                      length      = 1,                         &
                      coord_vals  = lon)

      print *, 'Defining CMOR output data variables ...'
      ivarb     =  cmor_variable(                              &
                      table       = input_table,               &
                      table_entry = 'tas',                     &
                      units       = units_string,              &
                      axis_ids    = iaxis)

!!$      print *, ' ** Data vector:', x

      print *, 'Writing CMOR output ...',shape(x)
      ierrorflag = cmor_write(                                 &
                      var_id    = ivarb,                       &
                      data      = x(:,1,1))

      print *, 'Closing file(s) created by CMOR ...'
      ierrorflag = cmor_close()

      end program rewrite_harvardf_data
