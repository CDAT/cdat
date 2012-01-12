
subroutine read_time(it, time, time_bnds,ntimes)
     integer it,ntimes
     double precision,intent(out), dimension(ntimes) :: time
     double precision,intent(out), dimension(2,ntimes):: time_bnds
  time(1) = (it-0.5)*30.
  time_bnds(1,1) = (it-1)*30.
  time_bnds(2,1) = it*30.

  time(1)=it
  time_bnds(1,1) = it
  time_bnds(2,1) = it+1

end subroutine read_time
  
subroutine read_2d_input_files(data2d,nlon,nlat)
  double precision,intent(out):: data2d(nlon,nlat)
  integer i,j
  do i = 1,nlon
     do j=1,nlat
        data2d(i,j)=52.+i-j
        !print*,i,j,data2d(i,j)
     enddo
  enddo
end subroutine read_2d_input_files

program test_grid

  use cmor_users_functions

  integer, parameter ::   ntimes = 2
  integer, parameter ::   lon =3 
  integer, parameter ::   lat=4
  integer, parameter ::   lev=5
  integer, parameter ::   nvert=6

  double precision, dimension(lon) :: x
  double precision, dimension(lat) :: y
  double precision lon_coords(lon,lat)
  double precision lat_coords(lon,lat)
  double precision lon_vertices(nvert,lon,lat)
  double precision lat_vertices(nvert,lon,lat)
 
  double precision data2d(lon,lat)

  integer myaxes(10)
  integer mygrids(10)
  integer myvars(10)
  integer tables(4)
  integer axes_ids(CMOR_MAX_DIMENSIONS)
  integer i,j,k,ierr;

  double precision , dimension(ntimes)::Time
  double precision, dimension(2,ntimes)::  bnds_time
  double precision tolerance
  double precision lon0 
  double precision lat0
  double precision delta_lon
  double precision delta_lat
  character(256) id
  double precision tmpf

  integer exit_mode
  integer tmpmo(12)
  
  tolerance=1.e-4
  lon0 = 280.
  lat0=0.
  delta_lon = 10.
  delta_lat = 10.
  tmpf=0.

  do j=1,lat
    y(j)=j;
    do i=1,lon
      x(i)=i;
      lon_coords(i,j) = lon0+delta_lon*(j+i-1)
      lat_coords(i,j) = lat0+delta_lat*(j-i+1)
!      /* vertices lon*/
      k = i*nvert+j*lon*nvert+0;
      print*, "i,j,k: %i, %i, %i\n",i,j,k
      if (nvert.eq.6) then
	lon_vertices(1,i,j) = lon_coords(i,j)
	lon_vertices(2,i,j) = lon_coords(i,j)+delta_lon
	lon_vertices(3,i,j) = lon_coords(i,j)+delta_lon
	lon_vertices(4,i,j) = lon_coords(i,j)+delta_lon/5.
	lon_vertices(5,i,j) = lon_coords(i,j)+delta_lon/5.
	lon_vertices(6,i,j) = lon_coords(i,j)

	lat_vertices(1,i,j) = lat_coords(i,j)
	lat_vertices(2,i,j) = lat_coords(i,j)
	lat_vertices(3,i,j) = lat_coords(i,j)+2.*delta_lat/3.
	lat_vertices(4,i,j) = lat_coords(i,j)+2.*delta_lat/3.
	lat_vertices(5,i,j) = lat_coords(i,j)+delta_lat
	lat_vertices(6,i,j) = lat_coords(i,j)+delta_lat

      else 
!!$	lon_vertices[i*4+j*lon*4+0] = lon_coords[i+j*lon]-delta_lon;
!!$	lon_vertices[i*4+j*lon*4+1] = lon_coords[i+j*lon];
!!$	lon_vertices[i*4+j*lon*4+2] = lon_coords[i+j*lon]+delta_lon;
!!$	lon_vertices[i*4+j*lon*4+3] = lon_coords[i+j*lon];
!!$	lat_vertices[i*4+j*lon*4+0] = lat_coords[i+j*lon];
!!$	lat_vertices[i*4+j*lon*4+1] = lat_coords[i+j*lon]-delta_lat;
!!$	lat_vertices[i*4+j*lon*4+2] = lat_coords[i+j*lon];
!!$	lat_vertices[i*4+j*lon*4+3] = lat_coords[i+j*lon]+delta_lat;
      endif
   enddo
enddo

  exit_mode = CMOR_EXIT_ON_MAJOR;
  j = CMOR_REPLACE;
  ierr = cmor_setup(netcdf_file_action=j,exit_control=exit_mode);
  ierr = cmor_dataset( &
       "Test",&
       "amip",&
       "GICC (Generic International Climate Center, Geneva, Switzerland)",&
       "GICCM1 (2002): atmosphere:  GICAM3 (gicam_0_brnchT_itea_2, T63L32); ",&
       "standard",&
       1,&
       "Rusty Koder (koder@middle_earth.net)",&
       "Output from archive/giccm_03_std_2xCO2_2256.",&
       "Equilibrium reached after 30-year ",&
       "Model described by Koder and Tolkien (J. Geophys. Res., 2001, 576-591).",&
       0,&
       0,&
       tmpmo,&
       "GICCM1","N/A",0,0,"GICC","N/A",tmpf,"r1i1p1")
  !ierr = cmor_load_table("Tables/CMIP5_Amon",tables(2))
  !ierr = cmor_load_table("Tables/CMIP5_grids",tables(1))
  !  cmor_set_table(tables(1))

  myaxes(1) = cmor_axis(table="Tables/CMIP5_grids",table_entry="x",units="m",coord_vals=x)
  myaxes(2) = cmor_axis(table="Tables/CMIP5_grids",table_entry="y",units="m",coord_vals=y)

  axes_ids(1) = myaxes(1)
  axes_ids(2) = myaxes(2)
  mygrids = cmor_grid((/axes_ids(1),axes_ids(2)/),&
       lat_coords,lon_coords,lat_vertices,lon_vertices)






  do i=1, ntimes
     call read_time(i-1, Time(i), bnds_time(1,i),1);
  enddo
  print*, 'Time:',Time
  myaxes(3) = cmor_axis(table="Tables/CMIP5_Amon",table_entry="time",&
       units="months since 1980",coord_vals=Time,cell_bounds=bnds_time)

  axes_ids(1)=myaxes(3)
  axes_ids(2)=mygrids(1)


  myvars(1) = cmor_variable(table="Tables/CMIP5_Amon",table_entry="hfls",&
       units="W m-2",axis_ids=(/axes_ids(1),axes_ids(2)/),&
       tolerance=tolerance,positive="down")
  
  do i=1, ntimes
     print*, i
    call read_2d_input_files(data2d,lon,lat)
    ierr = cmor_write(var_id=myvars(1),data=data2d,ntimes_passed=1)
 enddo
  ierr = cmor_close();
end program test_grid
