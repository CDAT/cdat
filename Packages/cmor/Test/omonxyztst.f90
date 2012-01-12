
MODULE local_subs

  PRIVATE
  PUBLIC  read_time, read_2d_input_files, read_3d_input_files, read_eta_input_files
CONTAINS
  

  SUBROUTINE read_time(it, time, time_bnds)
    
    IMPLICIT NONE
    
    INTEGER, INTENT(IN) :: it
    DOUBLE PRECISION, INTENT(OUT) :: time
    DOUBLE PRECISION, INTENT(OUT), DIMENSION(2) :: time_bnds
    INTEGER :: nm, nyr
    INTEGER, DIMENSION(0:12) :: &
     mon=(/0,31,59,90,120,151,181,212,243,273,304,334,365/)    
    nm = mod(it,12)
    if(nm.eq.0) then
       nm=12
    end if
    nyr=(it-nm)/12
    time_bnds(1) = mon(nm-1)+nyr*365.
    time_bnds(2) = mon(nm)+nyr*365.
    time = (time_bnds(1)+time_bnds(2))*0.5 
    
    RETURN
  END SUBROUTINE read_time
  
  SUBROUTINE read_3d_input_files(it,field)                  
    
    REAL, INTENT(OUT), DIMENSION(:,:,:) :: field
    
    INTEGER, INTENT(IN) :: it
    INTEGER :: i, j, k

       DO k=1,SIZE(field, 1)
          DO j=1,SIZE(field, 3)
             DO i=1,SIZE(field, 2)
               field(k,i,j)= 15.0+0.001*(i+j+k+it)
             END DO
          END DO
       END DO

  END SUBROUTINE read_3d_input_files

  
  SUBROUTINE read_2d_input_files(it, field)                  
    
    INTEGER, INTENT(IN) :: it
    REAL, INTENT(OUT), DIMENSION(:,:) :: field
    INTEGER :: i, j
    
    DO j=1,SIZE(field, 2)
       DO i=1,SIZE(field, 1)
           field(i,j)=0.001*(i-180)+0.002*(j-170)
        END DO
     END DO

  END SUBROUTINE read_2d_input_files

  SUBROUTINE read_eta_input_files(it, field)                  
    
    INTEGER, INTENT(IN) :: it
    REAL, INTENT(OUT), DIMENSION(:,:) :: field
    INTEGER :: i, j
    
    DO j=1,SIZE(field, 2)
       DO i=1,SIZE(field, 1)
           field(i,j)=0.001*(i-180)+0.002*(j-170)
        END DO
     END DO

   END SUBROUTINE read_eta_input_files

END MODULE local_subs


PROGRAM mip_contribution


! include module that contains the user-accessible cmor functions.
  USE cmor_users_functions
  USE local_subs

  IMPLICIT NONE

  !   dimension parameters:
  ! ---------------------------------
  INTEGER, PARAMETER :: nt = 12    ! number of time samples to process
  INTEGER, PARAMETER :: lon = 360       ! number of longitude grid cells  
  INTEGER, PARAMETER :: lat = 340       ! number of latitude grid cells
  INTEGER, PARAMETER :: lev = 40       ! number of latitude grid cells
  INTEGER, PARAMETER :: n3d = 2       ! number of AMIP Table 2 fields to be
  INTEGER, PARAMETER :: n2d = 1       ! number of AMIP Table 2 fields to be
                                      !     output.

  !   Tables associating the user's variables with AMIP standard output 
  !   variables.  The user may choose to make this association in a 
  !   different way (e.g., by defining values of pointers that allow him 
  !   to directly retrieve data from a data record containing many 
  !   different variables), but in some way the user will need to map his 
  !   model output onto the Tables specifying the MIP standard output.

  ! ----------------------------------


                                ! My variable names for Table 2 fields
  CHARACTER (LEN=10), DIMENSION(n3d) :: &
   varin3d=(/ 'thetao    ','so        '/)
                                ! Units appropriate to my data
   CHARACTER (LEN=10), DIMENSION(n3d) :: &
   units3d=(/ 'K         ','psu       '/)
   CHARACTER (LEN=4), DIMENSION(n3d) :: &
 positive3d= (/'    ','    '/)
  CHARACTER (LEN=10), DIMENSION(n3d) :: &
   entry3d=(/ 'thetao    ','so        '/)


  CHARACTER (LEN=10), DIMENSION(n2d) :: varin2d=(/'zos'/) 
  CHARACTER (LEN=10), DIMENSION(n2d) :: units2d=(/'m'/) 
  CHARACTER (LEN=4), DIMENSION(n2d) :: positive2d=(/'    '/) 
  CHARACTER (LEN=10), DIMENSION(n2d) :: entry2d=(/'zos'/) 

!  uninitialized variables used in communicating with CMOR:
!  ---------------------------------------------------------

  INTEGER :: error_flag, i, j, k, grid_ids, zfactor_id,itim,ilev,ilat,ilon
  INTEGER, DIMENSION(5) :: myaxes 
  INTEGER, DIMENSION(n3d) :: var3d_ids
  INTEGER, DIMENSION(n2d) :: var2d_ids
  INTEGER, DIMENSION(1) :: vard_ids
  REAL, DIMENSION(lev,lon,lat) :: data3d
  REAL, DIMENSION(lon,lat) :: data2d 
  DOUBLE PRECISION, DIMENSION (lon,lat) :: rdepth
  DOUBLE PRECISION, DIMENSION(lon,lat) :: alat_coords
  DOUBLE PRECISION, DIMENSION(lon,lat) :: alon_coords
  DOUBLE PRECISION, DIMENSION(lon,lat,4) :: alat_vertices
  DOUBLE PRECISION, DIMENSION(lon,lat,4) :: alon_vertices
  DOUBLE PRECISION :: time
  DOUBLE PRECISION, DIMENSION(2):: bnds_time
  REAL, DIMENSION(lon,lat) :: w
  REAL, DIMENSION(lon) :: rlon 
  REAL, DIMENSION(lat) :: rlat
  DOUBLE PRECISION, DIMENSION(lev) :: slev
  DOUBLE PRECISION, DIMENSION(lev+1) :: slev_bnds
  DOUBLE PRECISION, DIMENSION(3) :: pvalues 
  DOUBLE PRECISION :: bt

  !  Other variables:
  !  ---------------------
  
  INTEGER :: it, m
  

  bt=0.
  
  ! ================================
  !  Execution begins here:
  ! ================================
  
  ! definition of true longitude  
   do j=1,lat
      do i=1,lon
         alon_coords(i,j)=(i-1)*1.0
         alon_vertices(i,j,2)=alon_coords(i,j)
         alon_vertices(i,j,4)=alon_coords(i,j)
      end do
   end do
   do j=1,lat
      do i=2,lon
         alon_vertices(i,j,1)=alon_coords(i-1,j)
         alon_vertices(i,j,1)=alon_coords(i-1,j)
      end do
      alon_vertices(1,j,1)=alon_coords(lon,j)
      do i=1,lon-1 
         alon_vertices(i,j,3)=alon_coords(i+1,j)
      end do
      alon_vertices(lon,j,3)=alon_coords(1,j)
   end do
  ! definition of true latitude   
   do j=1,lat
      do i=1,lon
         alat_coords(i,j)=-85.25+(j-1)*0.5
         alat_vertices(i,j,2)=alat_coords(i,j)
         alat_vertices(i,j,4)=alat_coords(i,j)
      end do
   end do
   do j=2,lat
      do i=1,lon
         alat_vertices(i,j,1)=alat_coords(i,j-1)
      end do
   end do
   do j=1,lat-1
      do i=1,lon
         alat_vertices(i,j,3)=alat_coords(i,j+1)
      end do
   end do
   do i=1,lon
      alat_vertices(i,1,1)=alat_coords(i,1)
      alat_vertices(i,lat,3)=alat_coords(i,lat)
   end do
 !definition of longitude and latitude in rotated coordinate system
   do i=1,lon
      rlon(i)=0.5+(i-1)*1.0
   end do
   do j=1,lat
      rlat(j)=-85.25+(j-1)*0.5
   end do
 
 !definition of depth
   do j=1,lat
      do i=1,lon
         rdepth(i,j)=3000.+(j-170+i-180)
      end do
   end do
  ! definition of sigma levels
   do k=1,lev
      slev(k)=-(k-0.5)/lev
      slev_bnds(k)=-(k-1.0)/lev
   end do
   slev_bnds(lev+1)=-1.0
  ! Specify path where tables can be found, indicate that existing netCDF 
  !    files should not be overwritten, and instruct CMOR to error exit on 
  !    encountering errors of any severity.
  
  error_flag = cmor_setup(inpath='Tables',   &
       netcdf_file_action='replace',                                       &
       set_verbosity=1,                                                    &
       exit_control=1)
  
  ! Define dataset as output from the INM model 

  print*, 'calling cmor_dataset'
  error_flag = cmor_dataset(                                   &
       outpath='Test',         &
       contact='Evgeny Volodin, volodin@inm.ras.ru,' //  & 
       'INM RAS, Gubkina 8, Moscow, 119333 Russia,' // &
       '+7-495-9383904',            &
       experiment_id='piControl',        &
       institution='INM (Institute for Numerical Mathematics, ' //       &
       ' Moscow, Russia)',                                &
       institute_id='INM',       &
       source='inmcm4.0 (2009)',    &
       calendar='365_day',                                      &
       realization=1,                                          &
       initialization_method=1,           &
       history='Output from /data5/volodin/PICNTL', &
       comment='no comments',  &
       references='Volodin, Diansky, Gusev 2010. Climate ' //   &
       'model INMCM4.0. Izvestia RAS. Atmospheric and ' //          &
       'oceanic physics, V.46, N4, in print.',          &
       model_id="inmcm4", &
       !modeling_realm="ocean",  &
       parent_experiment_rip="N/A",parent_experiment_id="N/A",   &
       physics_version=1,      &
       branch_time=bt, &
      ! frequency='mon',                           &
       forcing="N/A")
  
  print*, 'returned from cmor_dataset'


  print*, 'defining axes'
  
!  definition of grid longitude
  myaxes(1) = cmor_axis(  &
       table='CMIP5_grids',    &
       table_entry='grid_longitude',      &
       length=lon,                   &
       units='degrees',         &
       coord_vals=rlon)

!  definition of grid latitude
  myaxes(2) = cmor_axis(  &
       table='CMIP5_grids',    &
       table_entry='grid_latitude',       &
       units='degrees',        &  
       length=lat,                   &
       coord_vals=rlat)              

!   definition of horizontal grid
   grid_ids = cmor_grid(axis_ids = (/myaxes(1),myaxes(2)/),  &
            latitude = alat_coords,     &
            longitude = alon_coords,    & 
            latitude_vertices = alat_vertices,    &
            longitude_vertices = alon_vertices) 

  myaxes(3) = grid_ids
  !   note that the time axis is defined next, but the time coordinate 
  !   values and bounds will be passed to cmor through function 
  !   cmor_write (below).
   print*, 'before time '
  myaxes(4) = cmor_axis(  &
       table='CMIP5_Omon',    &
       table_entry='time',           &
       units='days since 1850-1-1',  &
       length=nt,                &
       interval='1 month')
  itim=myaxes(4)

! definition of vertical coordinate
  myaxes(5) = cmor_axis(  &
       table='CMIP5_Omon',        &
       table_entry='ocean_sigma',       &
       units='1',&
       length=lev,                   &
       coord_vals=slev,             &
       cell_bounds=slev_bnds)
  ilev=myaxes(5)

  error_flag = cmor_zfactor(  &
       zaxis_id=ilev,                     &
       zfactor_name='sigma',              &
       axis_ids= (/ilev/),                    &
       zfactor_values = slev)!,             &
!       zfactor_bounds = slev_bnds)

  error_flag = cmor_zfactor(  &
       zaxis_id=ilev,                     &
       zfactor_name='depth',              &
       axis_ids= (/grid_ids/),            &
       zfactor_values = rdepth,            &
       units='m')      
       write(*,*) '1.7',error_flag       

  zfactor_id = cmor_zfactor(  &
       zaxis_id=ilev,                         &
       zfactor_name='eta',                     &
       axis_ids=(/ grid_ids, itim /),       &
       units='m' )
print*, 'finished defining axes', zfactor_id
    pvalues(1)=90.
    pvalues(2)=0.
    pvalues(3)=0.

!!$    error_flag = cmor_set_grid_mapping(grid_id=grid_ids, &
!!$     mapping_name='rotated_latitude_longitude', &
!!$     parameter_names=(/'grid_north_pole_latitude ','grid_north_pole_longitude','north_pole_grid_longitude'/), &
!!$     parameter_values=pvalues,  &
!!$     parameter_units=(/'degrees_north','degrees_east ','degrees      '/))
     write(*,*) 'after grid_mapping',error_flag

  
  DO m=1,n3d
     write(*,*) 'm =',m,ilev,grid_ids,itim
! IN THIS LINE I HAVE SEGMENTATION FAULT
     var3d_ids(m) = cmor_variable(    &
          table='CMIP5_Omon',  & 
          table_entry=entry3d(m),     & 
          units=units3d(m),           & 
          axis_ids=(/ilev,grid_ids,itim/),        &
          missing_value=1.0e20,       &
          positive=positive3d(m),     &
          original_name=varin3d(m))  
          write(*,*) 'var3d_ids =',var3d_ids(m) 
  ENDDO
  DO m=1,n2d
     var2d_ids(m) = cmor_variable(    &
          table='CMIP5_Omon',  & 
          table_entry=entry2d(m),     & 
          units=units2d(m),           & 
          axis_ids=(/grid_ids,itim/),        &
          missing_value=1.0e20,       &
          positive=positive2d(m),     &
          original_name=varin2d(m))   
         write(*,*) 'var2d_ids =',var2d_ids(m) 
  ENDDO
! commenting this out, depth is a zfactor not an actual var in this code
!     vard_ids(1) = cmor_variable(    &
!          table='CMIP5_Omon',  & 
!          table_entry='depth',     & 
!          units='m',           & 
!          axis_ids=(/grid_ids/),        &
!          missing_value=1.0e20,       &
!          original_name='depth')   

print*, 'completed everything up to writing output fields '
  
  !  Loop through history files (each containing several different fields, 
  !       but only a single month of data, averaged over the month).  Then 
  !       extract fields of interest and write these to netCDF files (with 
  !       one field per file, but all months included in the loop).
  
     
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
     
     
     ! Cycle through the 2-d fields, retrieve the requested variable and 
     ! append each to the appropriate netCDF file.
     
time_loop: DO it=1,nt 
   call read_time(it, time, bnds_time)
   DO m=1,n3d
      
      ! The user must write the code that fills the arrays of data
      ! that will be passed to CMOR.  The following line is simply a
      ! a place-holder for the user's code, which should replace it.
      
      call read_3d_input_files(it,data3d) 
      write(*,*) '3d file was read'  
      
      ! append a single time sample of data for a single field to 
      ! the appropriate netCDF file.
      
      !print*, RESHAPE(bnds_time, (/ 2,1 /))
      write(*,*) '3d var writing: ',var3d_ids(m)
      error_flag = cmor_write(var_id =        var3d_ids(m),   &
           data =          RESHAPE(data3d, (/ lat*lon*lev /)),        &
           ntimes_passed = 1,              &
           time_vals =     (/ time /),           &
           time_bnds =     RESHAPE(bnds_time, (/ 2,1 /)))
      write(*,*) '3d file was written, error_flag =',error_flag
      !/* editing this out, depth is zfactor should be written with zfactor but eta needs to be */
      !and it needs to be done at each time 
      write(*,*) "reading in eta"
      call read_eta_input_files(it,data2d) 
      write(*,*) "writing in eta"
      error_flag = cmor_write(var_id =       zfactor_id,   &
           data =          data2d,        &
           ntimes_passed = 1,    &
           time_vals =     (/ time /),           &
           time_bnds =     RESHAPE(bnds_time, (/ 2,1 /)), &
           store_with = var3d_ids(m)) 
   enddo
   DO m=1,n2d
      call read_2d_input_files(it,data2d)   
      write(*,*) '2d var writing with: ',var2d_ids(1)
      error_flag = cmor_write(var_id =        var2d_ids(1),   &
           data =          RESHAPE(data2d, (/ lat*lon /)),        &
           ntimes_passed = 1,              &
           time_vals =     (/ time /),           &
           time_bnds =     RESHAPE(bnds_time, (/ 2,1 /)))
      print*, 'after writing variable, ', var2d_ids(m)
      if(error_flag.ne.0) print*, '    error flag = ', error_flag
      IF (error_flag < 0) THEN
         ! write diagnostic messages to standard output device
         write(*,*) ' Error encountered writing AMIP Table 2 ' &
              // 'field ', entry3d(m), ', which I call ', varin3d(m)
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

