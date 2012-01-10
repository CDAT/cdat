module my_subs

contains

subroutine levrein(mlev, alev, blev, zlev, alevb, blevb, zlevb, p0)

integer                             :: mlev
double precision, intent(INOUT),dimension(mlev)   :: zlev
double precision, intent(INOUT),dimension(mlev+1) :: zlevb
real,             dimension(mlev)   :: slev
real,             intent(INOUT),dimension(mlev)   :: alev, blev
real,             intent(INOUT),dimension(mlev+1) :: alevb, blevb
real , intent(inout) :: p0
p0=101325.0

slev=(/ &
    0.00000000000000000,&
 2000.00000000000000000,&
 4000.00000000000000000,&
 6000.00000000000000000,&
 8000.00000000000000000,&
 9976.13671875000000000,&
11820.53906250000000000,&
13431.39453125000000000,&
14736.35546875000000000,&
15689.20703125000000000,&
16266.60937500000000000,&
16465.00390625000000000,&
16297.62109375000000000,&
15791.59765625000000000,&
14985.26953125000000000,&
13925.51953125000000000,&
12665.29296875000000000,&
11261.23046875000000000,&
 9771.40625000000000000,&
 8253.21093750000000000,&
 6761.33984375000000000,&
 5345.91406250000000000,&
 4050.71777343750000000,&
 2911.56933593750000000,&
 1954.80517578125000000,&
 1195.88989257812500000,&
  638.14892578125000000,&
  271.62646484375000000,&
   72.06358337402343750,&
    0.00000000000000000,&
    0.00000000000000000,&
    0.00000000000000000 /)

do i=1,mlev
  alev(i)=slev(i)/p0
end do
blev=(/ &
0.00000000000000000,&
0.00000000000000000,&
0.00000000000000000,&
0.00000000000000000,&
0.00000000000000000,&
0.00039085815660655,&
0.00291970069520175,&
0.00919413194060326,&
0.02031915634870529,&
0.03697485849261284,&
0.05948764085769653,&
0.08789497613906860,&
0.12200361490249634,&
0.16144150495529175,&
0.20570325851440430,&
0.25418859720230103,&
0.30623537302017212,&
0.36114501953125000,&
0.41820228099822998,&
0.47668814659118652,&
0.53588658571243286,&
0.59508424997329712,&
0.65356457233428955,&
0.71059441566467285,&
0.76540523767471313,&
0.81716698408126831,&
0.86495584249496460,&
0.90771585702896118,&
0.94421321153640747,&
0.97298520803451538,&
0.99228149652481079,&
1.00000000000000000 /)

do i=1,mlev
   zlev(i)=alev(i)+blev(i)
end do
do i=2,mlev
   alevb(i)=abs((alev(i)+alev(i-1))/2)
end do
alevb(1)=0.
alevb(mlev+1)=0.

do i=2,mlev
   blevb(i)=abs((blev(i)+blev(i-1))/2)
end do
blevb(1)=0.
blevb(mlev+1)=1.0

do i=2,mlev
   zlevb(i)=abs((zlev(i)+zlev(i-1))/2)
end do
zlevb(1)=0.
zlevb(mlev+1)=1.0
print*, 'done levrein going out' 
end subroutine levrein

subroutine latrein(lat, lon, latd, lond, bndslat, bndslon)

double precision, dimension(lon)   :: lond
double precision, dimension(lat)   :: latd
double precision, dimension(2,lon) :: bndslon
double precision, dimension(2,lat) :: bndslat
double precision, dimension(17)    :: plevs

integer   :: i

lond(1)=0.0
bndslon(1,1)=-0.9375
bndslon(2,1)=0.9375
do i=1,191
   lond(i+1)     =lond(i)+1.875000
   bndslon(1,i+1)=lond(i+1)-0.9375
   bndslon(2,i+1)=lond(i+1)+0.9375
end do

latd=(/ &
 -88.572,-86.723,-84.862,-82.999,-81.135,-79.271,-77.406,-75.541,-73.676,-71.811, &
 -69.946,-68.081,-66.216,-64.351,-62.486,-60.620,-58.755,-56.890,-55.025,-53.160, &
 -51.294,-49.429,-47.564,-45.699,-43.833,-41.968,-40.103,-38.238,-36.372,-34.507, &
 -32.642,-30.777,-28.911,-27.046,-25.181,-23.316,-21.450,-19.585,-17.720,-15.855, &
 -13.989,-12.124,-10.259,-8.394,-6.528,-4.663,-2.798,-0.933, 0.933, 2.798, &
 4.663, 6.528, 8.394, 10.259, 12.124, 13.989, 15.855, 17.720, 19.585, 21.450, &
 23.316,25.181,27.046,28.911,30.777,32.642,34.507,36.372,38.238,40.103, &
 41.968,43.833,45.699,47.564,49.429,51.294,53.160,55.025,56.890,58.755, &
 60.620,62.486,64.351,66.216,68.081,69.946,71.811,73.676,75.541,77.406, &
 79.271,81.135,82.999,84.862,86.723,88.572 /)

do i=1,95
   bndslat(2,i)=latd(i)-((latd(i)-(latd(i+1)))/2.)
   bndslat(1,i+1)=bndslat(2,i)
end do
bndslat(1,1)=-90.0
bndslat(2,96)=90.0

end subroutine latrein


subroutine timdatrein(nrec, lev, lat, lon, time, bndstime, data2, data3)

double precision, dimension(nrec) :: time
double precision, dimension(2,nrec) :: bndstime

real, dimension(lon*lat)         :: dummy2, dummy3
real, dimension(lon,lat,nrec)    :: data2
real, dimension(lon,lat,lev,nrec):: data3
integer                          :: jtime, icode, ilevel, nsize
integer                          :: i, nyear, ihalf, nrec
integer                          :: iyear, imon, iday, im, id, itime
integer                          :: jd1860, jdactu
character (len=50)               :: filearg

! filenamen als argument fuer program einlesen:
! filearg 1 ist 2d feld
! filearg 2 ist 3d feld
!
call getarg(1, filearg)
open(1,file=filearg, form='unformatted')
call getarg(2, filearg)
open(2,file=filearg, form='unformatted')

! Zeit seit 1860 01 01 12
! Julian date 1859 12 31 12
!
jd1860 = 31-32075+1461*(1859+4800+(12-14)/12)/4+367*(12-2-(12-14)/12*12) &
         /12-3*((1859+4900+(12-14)/12)/100)/4

do i=1,nrec
   read(1) jtime, icode, ilevel,nsize
   read(1) dummy2

   iyear = int(jtime/10000)
   im = iyear*100
   imon = int(jtime/100)-im
   id = imon*100

   monlen: select case (imon)
   case(4,6,9,11)
      iday = 30
   case(2)
      if(mod(iyear,4).eq.0) then
       if (mod(iyear,100).eq.0) then
          iday = 28
       else
          iday = 29
       endif
       if(mod(iyear,400).eq.0) iday=29
      else
          iday = 28
      endif
   case default
     iday = 31
   end select monlen
   ihalf=15
   print*, itime, iyear, imon, iday
      jdactu = ihalf-32075+1461*(iyear+4800+(imon-14)/12)/4+367*(imon-2-(imon-14)/12*12) &
               /12-3*((iyear+4900+(imon-14)/12)/100)/4
      time(i) = jdactu - jd1860
      bndstime(1,i) = time(i) - ihalf
      bndstime(2,i) = time(i) + (iday-ihalf)
   print*, bndstime(1,i), time(i), bndstime(2,i)

   do j=1,lat
     do k=1,lon
       data2(k,j,i)= dummy2(lon*(j-1) + k)
     end do
   end do
  
   do l=1,lev
      read(2) jtime, icode, ilevel, nsize
      read(2) dummy3
      do j=1,lat
        do k=1,lon
          data3(k,j,l,i)= dummy3(lon*(j-1) + k)
        end do
      end do
   end do

end do
   print*, time

end subroutine timdatrein

end module my_subs

program wegner
  !
  use cmor_users_functions

  use my_subs

  implicit none

  !   dimension parameters:
  ! ---------------------------------

  INTEGER, PARAMETER                 :: nti = 24  
  INTEGER, PARAMETER                 :: lon = 192       
  INTEGER, PARAMETER                 :: lat = 96     
  INTEGER, PARAMETER                 :: lev = 32    

  ! My variable names for IPCC Table A1a fields
  CHARACTER (LEN=50)                 :: TABLE = 'Test/IPCC_table_A1'
  CHARACTER (LEN=50)                 :: OUTDIR = 'Test'
  CHARACTER (LEN=50)                 :: TABELLE = 'Test/IPCC_table_A1'
  CHARACTER (LEN=5)                  :: varin2d = 'MSLP '
  CHARACTER (LEN=5)                  :: units2d = 'Pa   '
  CHARACTER (LEN=5)                  :: posit2d = '     '
  CHARACTER (LEN=5)                  :: entry2d = 'ps   '
  CHARACTER (LEN=5)                  :: varin3d = 'CLOUD'
  CHARACTER (LEN=5)                  :: units3d = '%    '
  CHARACTER (LEN=5)                  :: entry3d = 'cl   '

  
  !  uninitialized variables used in communicating with CMOR:
  !  ---------------------------------------------------------

  INTEGER                            :: error_flag, j,k
  INTEGER                            :: znondim_id, zfactor_id=1
  INTEGER                            :: var2d_ids
  INTEGER                            :: var3d_ids
  REAL, DIMENSION(lon,lat,nti)       :: data2d
  REAL, DIMENSION(lon,lat,lev,nti)   :: data3d
  REAL, DIMENSION(lon,lat)           :: data2d2
  DOUBLE PRECISION, DIMENSION(lat)   :: alats
  DOUBLE PRECISION, DIMENSION(lon)   :: alons
  DOUBLE PRECISION, DIMENSION(nti)   :: time
  DOUBLE PRECISION, DIMENSION(2,nti) :: bnds_time
  DOUBLE PRECISION, DIMENSION(2,lat) :: bnds_lat
  DOUBLE PRECISION, DIMENSION(2,lon) :: bnds_lon
  DOUBLE PRECISION, DIMENSION(lev)   :: zlevs
  DOUBLE PRECISION, DIMENSION(lev+1) :: zlev_bnds
  REAL, DIMENSION(lev)               :: a_coeff, b_coeff
  REAL, DIMENSION(lev+1)             :: a_coeff_bnds, b_coeff_bnds
  REAL                               :: p0
  INTEGER                            :: ilon, ilat, ipres, ilev, itim

  !  Other variables:
  !  ---------------------
  
  INTEGER                            :: it
  
  ! read the grid
  ! --------------------
  
  call latrein(lat, lon, alats, alons, bnds_lat, bnds_lon)
  
  ! Specify path where tables can be found and indicate that existing 
  !    netCDF files should not be overwritten.
  
  error_flag = cmor_setup(inpath='TABLE',                      &
               netcdf_file_action='replace',                   &
               exit_control=1)
  
  ! Define dataset as output from  ECHAM5-OM1
  ! -----------------------------------------
  error_flag         = cmor_dataset(                           &
       outpath       = OUTDIR,                                 &
       experiment_id ='pre-industrial control experiment',     &
       institution   ='MPI (Max Planck Institute for Meteorology,'  // &
       'Hamburg, Germany)',                                    &
       source        ='ECHAM5/MPI-OM(2004):'//                 &
       'atmosphere:  ECHAM5 (T63L32);'//                       &
       'ocean:       OM (1x1L41); '//                          &
       'sea ice:     ECHAM5',                                  &
       calendar      ='gregorian',                             &
       realization   =1,                                       &
       contact = 'Joerg Wegner (wegner@dkrz.de) ',             &
       history='Output from CERA Database/EH5_OM_20C_1_TEMP2', &
       comment='anthropogenic forcing only',                   &
       references='ECHAM5: E. Roeckner et. all, 2003,' //      &
       'The atmospheric general circulation model ECHAM5' //   & 
       'Report No. 349' //                                     &
       'OM: Marsland et. all, 2003,' //                        &
       'The Max-Planck-Institute global ocean/sea ice model'// &
       'with orthogonal curvelinear coordinates' //            &
       'Ocean Modell., 5, 91-127.' )
  
  !  Define all axes that will be needed

  ilat = cmor_axis(  &
       table=TABELLE,                &
       table_entry='latitude',       &
       units='degrees_north',        &  
       length=lat,                   &
       coord_vals=alats,             & 
       cell_bounds=bnds_lat)        
      
  ilon = cmor_axis(  &
       table=TABELLE,                &
       table_entry='longitude',      &
       length=lon,                   &
       units='degrees_east',         &
       coord_vals=alons,             &
       cell_bounds=bnds_lon)      
        
  !   note that the time axis is defined next, but the time coordinate 
  !   values and bounds will be passed to cmor through function 
  !   cmor_write (later, below).

  itim = cmor_axis(  &
       table=TABELLE,                &
       table_entry='time',           &
       units='days since 1860-1-1',  &
       length=nti,                   &
       interval='12 minutes')
 
  call levrein(lev, a_coeff, b_coeff, zlevs, a_coeff_bnds, b_coeff_bnds, zlev_bnds, p0)
  print*, 'done levrein'
  print*, 'sure ?'
  ilev = cmor_axis(  &
       table=TABELLE,                &
       table_entry='standard_hybrid_sigma',&
       length=lev,                   &
       units = '1',&
       coord_vals=zlevs,             &
       cell_bounds=zlev_bnds)

  error_flag = cmor_zfactor(               &
       zaxis_id=ilev,                      &
       zfactor_name='p0',                  &
       units='Pa',                         &
       zfactor_values = p0)

  error_flag = cmor_zfactor(               &
       zaxis_id=ilev,                      & 
       zfactor_name='b',                   &
       axis_ids= (/ ilev /),               &
       zfactor_values = b_coeff,           &
       zfactor_bounds = b_coeff_bnds  )

  error_flag = cmor_zfactor(               &
       zaxis_id=ilev,                      &
       zfactor_name='a',                   &
       axis_ids= (/ ilev /),               &
       zfactor_values = a_coeff,           &
       zfactor_bounds = a_coeff_bnds )

  zfactor_id = cmor_zfactor(               &
       zaxis_id=ilev,                      &
       zfactor_name='ps',                  &
       axis_ids=(/ ilon, ilat, itim /),    &
       units='Pa' )

  
     var2d_ids = cmor_variable(            &
          table=TABELLE,                   &
          table_entry=entry2d,             & 
          units=units2d,                   & 
          axis_ids=(/ ilon, ilat, itim /), &
          missing_value=1.0e20,            &
          positive=posit2d,                &
          original_name=varin2d)   

     var3d_ids = cmor_variable(            &
          table=TABELLE,                   &
          table_entry=entry3d,             &
          units=units3d,                   &
          axis_ids=(/ ilon, ilat, ilev, itim /),  &
          missing_value=1.0e20,            &
          original_name=varin3d)

  print*, ' '
  print*, 'completed everything up to writing output fields '
  print*, ' '
  
  call timdatrein(nti, lev, lat, lon, time, bnds_time, data2d, data3d)

        
     error_flag = cmor_write(                               &
           var_id        = var3d_ids,                       &
           data          = data3d,                          &
           ntimes_passed = nti,                             &
           time_vals     = time,                            &
           time_bnds     = bnds_time )

     error_flag = cmor_write(                               &
           var_id        = var2d_ids,                       &
           data          = data2d,                          &
           ntimes_passed = nti,                             &
           time_vals     = time,                            &
           time_bnds     = bnds_time,                       &
           store_with    = var3d_ids )

  if (error_flag < 0) then
 ! write diagnostic messages to standard output device
           write(*,*) ' Error encountered writing IPCC Table A1a ' &
               // 'field ', entry2d, ', which I call ', varin2d
           write(*,*) ' Was processing time sample: ', time 
  end if
  !   Close all files opened by CMOR.
  !   -------------------------------
  error_flag = cmor_close()  
 
end program wegner
