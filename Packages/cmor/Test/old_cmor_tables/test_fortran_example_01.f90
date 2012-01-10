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

program testing
  use cmor_users_functions
  use local_subs
    implicit none
    integer error_flag
  INTEGER, PARAMETER :: nvert = 4    ! number of time samples to process
  INTEGER, PARAMETER :: ntimes = 2    ! number of time samples to process
  INTEGER, PARAMETER :: lon = 3       ! number of longitude grid cells  
  INTEGER, PARAMETER :: lat = 4       ! number of latitude grid cells
  INTEGER, PARAMETER :: lev = 5       ! number of standard pressure levels
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
  REAL, DIMENSION(lon,lat,lev) :: data3d
  DOUBLE PRECISION, DIMENSION(lat) :: alats
  DOUBLE PRECISION, DIMENSION(lon) :: alons
  DOUBLE PRECISION, DIMENSION(lev) :: plevs
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
  INTEGER :: ilon, ilat, ipres, ilev, itim,i
  real :: missing
  character(1024) inpath,mapnm
  character(len=30),dimension(6) ::params =(/ "standard_parallel1            ",&
       "longitude_of_central_meridian ","latitude_of_projection_origin ",&
       "false_easting                 ","false_northing                ", &
       "standard_parallel2            " /)
  character(len=5) ,dimension(6) :: punits = (/ "     ","     ","     ","     ","     ","     " /)
  double precision,dimension(6) :: pvalues = (/-20.,175.,13.,8.,0.,20. /)
  !  Other variables:
  !  ---------------------
  
  INTEGER :: it, m, j,k,tables(2),vars(10),axes(10)

  integer pass_axes(2)

  integer :: grid_id


  real lon_coords(lon,lat),lat_coords(lon,lat)
  real lon_vertices(nvert,lon,lat),lat_vertices(nvert,lon,lat)
  real area(lon,lat)
  real x(lon),y(lat)
  
  real lon0,lat0,delta_lon,delta_lat
  
  lon0 = -120.
  lat0=0.;
  delta_lon = 10.;
  delta_lat = 10.;
  
!!$  /* first construct grid lon/lat */
  do j=1,lat
     y(j)=j
     do i=1,lon
        x(i)=i
        lon_coords(i,j) = lon0+delta_lon*(j+i-1);
        lat_coords(i,j) = lat0+delta_lat*(j-i-2);
!!$      /* vertices lon*/
!!$      k = i*4+j*lon*4+0;
!!$      printf('i,j,k: %i, %i, %i\n',i,j,k);
        lon_vertices(1,i,j) = lon_coords(i,j)-delta_lon;
        lon_vertices(2,i,j) = lon_coords(i,j);
        lon_vertices(3,i,j) = lon_coords(i,j)+delta_lon;
        lon_vertices(4,i,j) = lon_coords(i,j);
!!$      /* vertices lat */
        lat_vertices(1,i,j) = lat_coords(i,j);
        lat_vertices(2,i,j) = lat_coords(i,j)-delta_lat;
        lat_vertices(3,i,j) = lat_coords(i,j);
        lat_vertices(4,i,j) = lat_coords(i,j)+delta_lat;
     end do
  end do

  inpath ='Test'
  j = CMOR_REPLACE
  k = CMOR_EXIT_ON_MAJOR
  error_flag = cmor_setup(inpath='Test', netcdf_file_action=j,&
       exit_control=k)
  
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
       '(Clim. Dyn., 2003, 323-357.)',model_id="pcmdi-08a", &
       forcing="CO2")

    tables(1) = cmor_load_table('IPCC_test_table_A')
    tables(2) = cmor_load_table('IPCC_test_table_Grids')

    axes(1) = cmor_axis(table_entry = 'x', &
                        units = '1', &
                        length = lon, &
                        coord_vals = x)
    axes(2) = cmor_axis(table_entry = 'y', &
                        units = '1', &
                        length = lat, &
                        coord_vals = y)
                        
    grid_id = cmor_grid(axis_ids = axes, &
                        latitude = lat_coords, &
                        longitude = lon_coords, &
                        latitude_vertices = lat_vertices, &
                        longitude_vertices = lon_vertices)
    print*, 'Got grid id: ',grid_id
    
    mapnm = 'lambert_conformal_conic'
    error_flag = cmor_set_grid_mapping(grid_id,&
         mapnm,params,pvalues,punits)

    call cmor_set_table(table_id=tables(2))

     
    axes(3) = cmor_axis(table = 'IPCC_test_table_A',&
                        table_entry = 'time',&
                        units = 'days since 1980',&
                        length = 2 &
                        )
    
    
    pass_axes(2) = axes(3)
    pass_axes(1) = grid_id
    
    vars(1) = cmor_variable(table = 'IPCC_test_table_A',&
                            table_entry = 'hfls',&
                            units = 'W m-2',&
                            axis_ids = pass_axes,&
                            positive = 'down',&
                            original_name = 'HFLS',&
                            history = 'no history',&
                            comment = 'no future'&
                            )
    do i=1,ntimes
       call read_time(i, time(1), bnds_time)
       print*, 'Test code: writing time:',i,'of',ntimes,time(1)
       call read_2d_input_files(i, 'LATENT', data2d)
       error_flag = cmor_write(var_id = vars(1) ,&
                               data =data2d,&
                               file_suffix='ftn-01',&
                               ntimes_passed = 1,&
                               time_vals = time,&
                               time_bnds = bnds_time)
    end do
   error_flag = cmor_close()
    
  end program testing
  
