PROGRAM test_cmor_grid
   USE cmor_users_functions

   IMPLICIT NONE

   INTEGER, PARAMETER :: n_sections = 3
   INTEGER, PARAMETER :: n_points = 10
   INTEGER, PARAMETER :: n_lev = 40
   INTEGER, PARAMETER :: n_dbz = 15
   real, parameter :: R_UNDEF = -1.0E+30
   INTEGER :: i, j, k, l, v2_id,v3_id,option
!    REAL :: x(n_lev,n_points,n_sections)
   REAL :: y(n_points,n_lev,n_sections)
   REAL :: x(n_points,n_sections)
   double precision :: z_ax(n_lev),z_bounds(2,n_lev)
   double precision :: dbz_ax(n_dbz),dbz_bounds(2,n_dbz)
   double precision :: profile_ax(n_points),section_ax(n_sections)
   real :: lat(n_points,n_sections),lon(n_points,n_sections)
   real :: lat_bounds(4,n_points,n_sections),lon_bounds(4,n_points,n_sections)
   character(len=128) :: table='CMIP5_cf3hr'
   character(len=32) :: sec_units='days since 2000-01-01'
   integer :: error_flag,height_axid,time_axid,profile_axid,grid_id,section_axid,dbz_axid
   real :: lat_step,lon_step
   integer latvar_id,lonvar_id
   integer blatvar_id,blonvar_id,table_grid_id

   print *, 'Option (0,1)?'
   read(*,*) option
   
   
   !+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   ! Fill in variables with some data
   !+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
     do k=1,n_sections
         do i=1,n_points
            x(i,k) = k*10.0 + 0.1*i
         enddo
     enddo
!    do l=1,n_dbz
     do k=1,n_sections
       do j=1,n_lev
         do i=1,n_points
            y(i,j,k) = k*10.0 + 1.0*j + 0.1*i
         enddo
       enddo
     enddo
!    enddo
   
   ! Values for section axis (time of each orbit)
   do i=1,n_sections
     section_ax(i) = float(i)/8.0 ! 3hr
   enddo

   ! Values for profile axis
   do i=1,n_points
     profile_ax(i) = float(i)
   enddo
      
   ! Height of vertical levels
   do i=1,n_lev
     z_ax(i) = 240.D0 + 480.D0*(i-1)
     z_bounds(1,i) = z_ax(i) - 240.D0
     z_bounds(2,i) = z_ax(i) + 240.D0
   enddo
    
   ! dBZ bins
   do i=1,n_dbz
     dbz_ax(i) = -47.5D0 + 5.D0*(i-1)
     dbz_bounds(1,i) = dbz_ax(i) - 2.5D0
     dbz_bounds(2,i) = dbz_ax(i) + 2.5D0
   enddo
   
   lat_step = 180.0/(n_points+2)
   lon_step = 360.0/(n_points+2)
   do i=1,n_points
      lat(i,:) = -90.0 + lat_step*0.5 + lat_step*i
      lon(i,:) =   0.0 + lon_step*0.5 + lon_step*i
       lat_bounds(1,i,:) = lat(i,:) - lat_step*0.5
       lat_bounds(2,i,:) = lat(i,:) - lat_step*0.5
       lat_bounds(3,i,:) = lat(i,:) + lat_step*0.5
       lat_bounds(4,i,:) = lat(i,:) + lat_step*0.5
       lon_bounds(1,i,:) = lon(i,:) - lon_step*0.5
       lon_bounds(2,i,:) = lon(i,:) + lon_step*0.5
       lon_bounds(3,i,:) = lon(i,:) - lon_step*0.5
       lon_bounds(4,i,:) = lon(i,:) + lon_step*0.5
   enddo

   
   !+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   ! Specify path for tables and set up other CMOR options
   !+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   error_flag = cmor_setup(inpath='Tables/', &
                           netcdf_file_action="replace",create_subdirectories=0)

   print *, '---------------Define dataset'
   !+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   ! Define dataset as output from COSP, and other model details
   !+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   error_flag = cmor_dataset(outpath='./',experiment_id='AMIP',institution='Met Office Hadley Centre', &
                    source='MetUM',calendar='360_day',realization=1,contact='alejandro.bodas@metoffice.gov.uk', &
                    history='history',comment='En un lugar de la Mancha de cuyo nombre no quiero acordarme', &
                    references='references',model_id='MetUM',forcing='N/A',parent_experiment_id='N/A', &
                    branch_time=0.d0, institute_id='MOHC',parent_experiment_rip='prip')
   
   print *, '---------------Define axis'
   !+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   ! Define axis
   !+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   profile_axid = cmor_axis(table=table, table_entry='location', units='1', &
                                                       length=n_points, coord_vals=profile_ax)
   height_axid  = cmor_axis(table=table, table_entry='alt40', units='m', &
                                                       length=n_lev, coord_vals=z_ax,cell_bounds=z_bounds)
   dbz_axid     = cmor_axis(table=table, table_entry='dbze', units='dBZ', &
                                                       length=15, coord_vals=dbz_ax,cell_bounds=dbz_bounds)
   section_axid  = cmor_axis(table=table, table_entry='time1', units=sec_units, &
                                                       length=n_sections, coord_vals=section_ax)

   print *, '---------------Define grid'
   !+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   ! Define grid
   !+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
!    table_grid_id = cmor_load_table(table_grids)
!    call cmor_set_table(table_grid_id)
   print*, "AXES FOR GRID: ",(/profile_axid, section_axid/)
   grid_id = cmor_grid((/profile_axid, section_axid/))
!    lat,lon,lat_bounds,lon_bounds,
   print *, ' -- grid_id: ', grid_id
   latvar_id = cmor_time_varying_grid_coordinate(grid_id,'latitude','degrees_north',missing=R_UNDEF)
   print*, ' -- latvar_id :',latvar_id
   lonvar_id = cmor_time_varying_grid_coordinate(grid_id,'longitude','degrees_east',missing=R_UNDEF)
   print*, ' -- lonvar_id:',lonvar_id
   if (grid_id > 0) then
        print *,  '*********** Error, grid_id: ', grid_id
        stop
   endif
   
   print *, '---------------Define variables'
   !+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   ! Define variables
   !+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   v3_id = cmor_variable(table=table, table_entry='clcalipso', units='%', &
                                 axis_ids=(/grid_id,height_axid/), missing_value=R_UNDEF)
   v2_id = cmor_variable(table=table, table_entry='cllcalipso', units='%', &
                                 axis_ids=(/grid_id/), missing_value=R_UNDEF)
   
   !+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   ! Write variables to file
   !+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   if (option == 0) then ! Variable clcalipso
      print *, '---------------Write clcalipso'
      error_flag = cmor_write(var_id=v3_id, data=reshape(y,(/n_points,n_sections,n_lev/),order=(/1,3,2/)))
      print *, '---------------Write clcalipso:lat'
      error_flag = cmor_write(var_id=latvar_id, data=lat, store_with=v3_id)
      print *, '---------------Write clcalipso:lon'
      error_flag = cmor_write(var_id=lonvar_id, data=lon, store_with=v3_id)
   endif
   if (option == 1) then ! Variable cllcalipso
      print *, '---------------Write cllcalipso'
      error_flag = cmor_write(var_id=v2_id, data=x)
      print *, '---------------Write cllcalipso:lat'
      error_flag = cmor_write(var_id=latvar_id, data=lat, store_with=v2_id)
      print *, '---------------Write cllcalipso:lon'
      error_flag = cmor_write(var_id=lonvar_id, data=lon, store_with=v2_id)
   endif
   
   !+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   ! Close files
   !+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   error_flag = cmor_close()
                               
   
 END
