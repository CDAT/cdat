PROGRAM test_cmor_grid
   USE cmor_users_functions

   IMPLICIT NONE

   INTEGER, PARAMETER :: n_sections = 3
   INTEGER, PARAMETER :: n_points = 10
   INTEGER, PARAMETER :: n_lev = 4
   real, parameter :: R_UNDEF = -1.0E+30
   INTEGER :: i, j, k, var_id,option
!    REAL :: x(n_lev,n_points,n_sections)
   REAL :: x(n_points,n_sections,n_lev)
   real :: z_ax(n_lev),z_bounds(2,n_points)
   double precision :: profile_ax(n_points),section_ax(n_sections)
   real :: lat(n_points,n_sections),lon(n_points,n_sections)
   real :: lat_bounds(4,n_points,n_sections),lon_bounds(4,n_points,n_sections)
   character(len=128) :: table='Test/CMIP5_cf3hr.txt'
   character(len=32) :: sec_units='days since 2000-01-01'
   integer :: error_flag,height_axid,time_axid,profile_axid,grid_id,section_axid
   real :: lat_step,lon_step
   integer latvar_id,lonvar_id
   integer blatvar_id,blonvar_id

   print *, 'Option (0,1,other)?'
   read(*,*) option

   if (option == 0) then ! 1D grid, no time dimension
      table='Test/CMIP5_cf3hr_0.txt'
      sec_units='1'
   endif
   if (option == 1) then ! 1D grid, section as time dimension
      table='Test/CMIP5_cf3hr_1.txt'
      sec_units='days since 2000-01-01'
   endif
   
   !+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   ! Fill in variables with some data
   !+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   do k=1,n_sections
     do j=1,n_points
       do i=1,n_lev
          x(j,k,i) = k*1000000 + 1000*j + i
       enddo
     enddo
   enddo

   ! Values for section axis
   do i=1,n_sections
     section_ax(i) = float(i)
   enddo

   ! Values for profile axis
   do i=1,n_points
     profile_ax(i) = float(i)
   enddo
      
   ! Height of vertical levels
   do i=1,n_lev
     z_ax(i) = 240.0 + 480.0*(i-1)
     z_bounds(1,i) = z_ax(i) - 240.0
     z_bounds(2,i) = z_ax(i) + 240.0
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
   error_flag = cmor_setup(inpath='./',netcdf_file_action="replace",create_subdirectories=0)

   print *, '---------------Define dataset'
   !+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   ! Define dataset as output from COSP, and other model details
   !+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   error_flag = cmor_dataset(outpath='Test/',experiment_id='AMIP',institution='Met Office', &
                    source='source',calendar='360_day',realization=1,contact='alejandro.bodas@metoffice.gov.uk', &
                    history='history',comment='En un lugar de la Mancha de cuyo nombre no quiero acordarme', &
                    references='references',forcing="CH4",model_id="mymod-10a")
   
   print *, '---------------Define axis'
   !+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   ! Define axis
   !+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   profile_axid = cmor_axis(table=table, table_entry='location', units='1', &
                                                       length=n_points, coord_vals=profile_ax)
   height_axid  = cmor_axis(table=table, table_entry='height40', units='m', &
                                                       length=n_lev, coord_vals=z_ax,cell_bounds=z_bounds)
   section_axid  = cmor_axis(table=table, table_entry='section', units=sec_units, &
                                                       length=n_sections, coord_vals=section_ax)
   
   print *, '---------------Define grid'
   !+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   ! Define grid
   !+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   if ((option == 0).or.(option == 1)) then
     grid_id = cmor_grid((/profile_axid/), lat(:,1), lon(:,1))
   else
      print*, "AXES FOR GRID: ",(/profile_axid, section_axid/)
     grid_id = cmor_grid((/profile_axid, section_axid/),nvertices=4)
     print*, 'got grid:',grid_id
     !need to add code for defining the lat lon var here
     latvar_id = cmor_time_varying_grid_coordinate(grid_id,table_entry='latitude',units='degrees_north')
     lonvar_id = cmor_time_varying_grid_coordinate(grid_id,table_entry='longitude',units='degrees_east')
     blatvar_id = cmor_time_varying_grid_coordinate(grid_id,table_entry='vertices_latitude',units='degrees_north')
     blonvar_id = cmor_time_varying_grid_coordinate(grid_id,table_entry='vertices_longitude',units='degrees_east')
   endif
   if (grid_id > 0) then
        print *,  '*********** Error, grid_id: ', grid_id
        stop
   endif
   
   print *, '---------------Define variables'
   !+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   ! Define variables. Fill in dimensions table first if needed
   !+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   if ((option == 0).or.(option == 1)) then
     var_id = cmor_variable(table=table, table_entry='curtain', units='1', &
                                 axis_ids=(/grid_id,section_axid,height_axid/), missing_value=R_UNDEF)
   else
     var_id = cmor_variable(table=table, table_entry='curtain', units='1', &
                                 axis_ids=(/grid_id,height_axid/), missing_value=R_UNDEF)
   endif
   
   print *, '---------------Write variables'
   !+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   ! Write variables to file
   !+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   if ((option == 0).or.(option == 1)) then
     error_flag = cmor_write(var_id=var_id, data=x)
   else
     error_flag = cmor_write(var_id=var_id, data=x)
     error_flag = cmor_write(var_id=latvar_id, data=lat, store_with=var_id)
     error_flag = cmor_write(var_id=lonvar_id, data=lon, store_with=var_id)
     error_flag = cmor_write(var_id=latvar_id, data=lat, store_with=var_id)
     error_flag = cmor_write(var_id=lonvar_id, data=lon, store_with=var_id)
     print*, 'writing blat',blatvar_id
     error_flag = cmor_write(var_id=blatvar_id, data=lat_bounds, store_with=var_id)
     print*, 'writing blon',blonvar_id
     error_flag = cmor_write(var_id=blonvar_id, data=lon_bounds, store_with=var_id)
   endif
   if (error_flag /= 0) then
        print *,  '*********** Error writing variable: ', error_flag
        stop
   endif
   
   !+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   ! Close files
   !+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   error_flag = cmor_close()
                               
   
 END
