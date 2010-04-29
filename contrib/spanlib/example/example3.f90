! File: example3.f90
!
! This file is part of the SpanLib library.
! Copyright (C) 2006  Stephane Raynaud
! Contact: stephane dot raynaud at gmail dot com
!
! This library is free software; you can redistribute it and/or
! modify it under the terms of the GNU Lesser General Public
! License as published by the Free Software Foundation; either
! version 2.1 of the License, or (at your option) any later version.
!
! This library is distributed in the hope that it will be useful,
! but WITHOUT ANY WARRANTY; without even the implied warranty of
! MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
! Lesser General Public License for more details.
!
! You should have received a copy of the GNU Lesser General Public
! License along with this library; if not, write to the Free Software
! Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301  USA

program example3

	! This example shows how to use SVD statistical model.
	!
	! The same dataset as in previous f90 example is used,
	! and the same geographical regions as in example2.f90
	! are analysed using SVD.
	!
	! The goal is to build a SVD model based on the relationship
	! between these two regions. The eastern region is set as the
	! predictor, and will be used to estimate the values in these
	! western region (the predictand).
	!
	! Note:
	!	This example should run only a few seconds.
	!	If it is not the case, your BLAS/LAPACK librairy is not optimized.


	use spanlib
	use netcdf

	implicit none

	! Parameters
	! ----------
	integer,parameter :: pcaNkeep=15, svdNkeep=8,&
		& lons1(2)=(/70,150/),lats1(2)=(/16,45/),& ! East = predictor
		& lons2(2)=(/10,40/),lats2(2)=(/12,49/)    ! West = predictand
	real(wp), parameter ::new_missing_value=-999.d0
	character(len=20), parameter :: input_nc_file="data2.cdf", &
		& output_nc_file="output_fortran3.nc", var_name='ssta'

	! Other declarations
	! ------------------
	real(wp), allocatable :: lon1(:), lat1(:), &
		& lon2(:), lat2(:), time(:), &
		& sst1(:,:,:), sst2(:,:,:)
	logical, allocatable :: mask1(:,:),mask2(:,:)
	real(wp), allocatable :: packed_sst1(:,:),packed_sst2(:,:),&
		& packed_sstNow1(:),packed_svdSstNow2(:)
	real(wp), allocatable :: &
		& pcaEofs1(:,:),pcaEofs2(:,:),pcaPcs1(:,:),pcaPcs2(:,:), &
		& svdEofs1(:,:),svdEofs2(:,:),svdPcs1(:,:),svdPcs2(:,:)
	real(wp), allocatable :: l2r(:), packed_svdSst2(:,:), &
		& packed_pcaSst1(:,:), packed_pcaSst2(:,:), &
		& pcaSst1(:,:,:), pcaSst2(:,:,:), svdSst2(:,:,:)
	character(len=20) :: &
		& lon_units, lat_units, var_units, &
		&	lon_name, lat_name, time_name, time_units
	integer :: ncid, dimids(6), varids(6), sstids(5)
	integer(kind=4) :: i,nlon1,nlat1,nlon2,nlat2,ns1,ns2,nt
	real(wp) :: missing_value

	! Precision
	! ---------
	if(wp==8)then
		print*,'Using double precision'
	else
		print*,'Using simple precision'
	end if

	! Get the initial sst field from the netcdf file
	! ----------------------------------------------
	print*,'Reading inputs...'
	call err(nf90_open(input_nc_file, nf90_nowrite, ncid))
	call err(nf90_inq_varid(ncid, var_name, sstids(1)))
	! Dimensions
	nlon1 = lons1(2)-lons1(1)+1 ;	nlat1 = lats1(2)-lats1(1)+1
	nlon2 = lons2(2)-lons2(1)+1 ; nlat2 = lats2(2)-lats2(1)+1
	call err(nf90_inquire_variable(ncid, sstids(1), dimids=dimids(1:3)))
	call err(nf90_inquire_dimension(ncid,dimids(1),name=lon_name))
	call err(nf90_inquire_dimension(ncid,dimids(2),name=lat_name))
	call err(nf90_inquire_dimension(ncid, dimids(3), &
		&	name=time_name, len=nt))
	! Allocations
	allocate(sst1(nlon1,nlat1,nt),sst2(nlon2,nlat2,nt))
	allocate(mask1(nlon1,nlat1),mask2(nlon2,nlat2))
	allocate(lon1(nlon1),lat1(nlat1))
	allocate(lon2(nlon2),lat2(nlat2))
	allocate(time(nt))
	! SST boxes and attributes
	call err(nf90_get_var(ncid, sstids(1), sst1,&
		& start=(/lons1(1),lats1(1),1/), &
		& count=(/lons1(2)-lons1(1)+1,lats1(2)-lats1(1)+1,nt/)))
	call err(nf90_get_var(ncid, sstids(1), sst2,&
		& start=(/lons2(1),lats2(1),1/), &
		& count=(/lons2(2)-lons2(1)+1,lats2(2)-lats2(1)+1,nt/)))
	call err(nf90_get_att(ncid,sstids(1),'missing_value',missing_value))
	call err(nf90_get_att(ncid,sstids(1),'units',var_units))
	! Longitudes
	call err(nf90_inq_varid(ncid, lon_name, varids(1)))
	call err(nf90_get_var(ncid, varids(1), lon1, &
		& start=(/lons1(1)/), count=(/lons1(2)-lons1(1)+1/)))
	call err(nf90_get_att(ncid, varids(1), 'units', lon_units))
	call err(nf90_get_var(ncid, varids(1), lon2, &
		& start=(/lons2(1)/), count=(/lons2(2)-lons2(1)+1/)))
	! Latitudes
	call err(nf90_inq_varid(ncid, lat_name, varids(1)))
	call err(nf90_get_var(ncid, varids(1), lat1, &
		& start=(/lats1(1)/), count=(/lats1(2)-lats1(1)+1/)))
	call err(nf90_get_att(ncid, varids(1), 'units', lat_units))
	call err(nf90_get_var(ncid, varids(1), lat2, &
		& start=(/lats2(1)/), count=(/lats2(2)-lats2(1)+1/)))
	! Time
	call err(nf90_inq_varid(ncid, time_name, varids(1)))
	call err(nf90_get_var(ncid, varids(1), time))
	call err(nf90_get_att(ncid, varids(1), 'units', time_units))
	call err(nf90_close(ncid))


	! Format (pack) data to have only one space dimension
	! ---------------------------------------------------
	print*,'Packing...'
	mask1 = (sst1(:,:,1) /= missing_value)
	mask2 = (sst2(:,:,1) /= missing_value)
	ns1 = count(mask1) ; ns2 = count(mask2)
	allocate(packed_sst1(ns1, nt))
	allocate(packed_sst2(ns2, nt))
	do i=1, nt
		packed_sst1(:,i) = pack(sst1(:,:,i), mask1)
		packed_sst2(:,i) = pack(sst2(:,:,i), mask2)
	end do
	where(sst1==missing_value)sst1 = new_missing_value
	where(sst2==missing_value)sst2 = new_missing_value

	! First, we build the model
	! -------------------------
	print*,'[sl_svd_model_build] Seting up the SVD model...'
	allocate(pcaEofs1(ns1, pcaNkeep),pcaPcs1(nt,pcaNkeep))
	allocate(pcaEofs2(ns2, pcaNkeep),pcaPcs2(nt,pcaNkeep))
	allocate(svdEofs1(pcaNkeep,svdNkeep))
	allocate(svdEofs2(pcaNkeep,svdNkeep))
	allocate(svdPcs1(nt,svdNkeep),svdPcs2(nt,svdNkeep))
	allocate(l2r(svdNkeep))
	call sl_svd_model_setup(packed_sst1,packed_sst2,&
		& pcaEofs1,pcaEofs2,svdEofs1,svdEofs2,l2r, &
		& lPcaPc = pcaPcs1, rPcaPc = pcaPcs2,&
		& lSvdPc = svdPcs1, rSvdPc = svdPcs2)

	! Second, we loop on time to estimate sst2 from sst1
	! --------------------------------------------------
	print*,'[sl_svd_model_use] Running the SVD model at each time step...'
	allocate(packed_svdSst2(ns2,nt),packed_sstNow1(ns1),packed_svdSstNow2(ns2))
	do i = 1, nt
		packed_sstNow1 = packed_sst1(:,i)
		call sl_svd_model_run(packed_sstNow1,packed_svdSstNow2,&
			& pcaEofs1,pcaEofs2,svdEofs1,svdEofs2,l2r)
		packed_svdSst2(:,i) = packed_svdSstNow2
	end do
	deallocate(svdEofs1,svdEofs2,l2r,packed_sstNow1,packed_svdSstNow2)

	! Rebuild the pre-PCA filtered field for comparisons
	! --------------------------------------------------
	print*,'[sl_pca_rec] Rebuilding after pre-pca'
	allocate(packed_pcaSst1(ns1,nt),packed_pcaSst2(ns2,nt))
	call sl_pca_rec(pcaEofs1,pcaPcs1,packed_pcaSst1)
	call sl_pca_rec(pcaEofs2,pcaPcs2,packed_pcaSst2)
	deallocate(pcaEofs1,pcaEofs2)

	! Unpacking
	! ---------
	print*,'Unpacking...'
	allocate(svdSst2(nlon2,nlat2,nt))
	allocate(pcaSst1(nlon1,nlat1,nt),pcaSst2(nlon2,nlat2,nt))
	do i=1, nt
		svdSst2(:,:,i) = unpack(packed_svdSst2(:,i), &
			& mask2, new_missing_value)
		pcaSst1(:,:,i) = unpack(packed_pcaSst1(:,i), &
			& mask1, new_missing_value)
		pcaSst2(:,:,i) = unpack(packed_pcaSst2(:,i), &
			& mask2, new_missing_value)
		where(.not.mask2)svdSst2(:,:,i) = new_missing_value
		where(.not.mask1)pcaSst1(:,:,i) = new_missing_value
		where(.not.mask2)pcaSst2(:,:,i) = new_missing_value
	end do


	! Write out the phase composites of the first oscillation
	! -------------------------------------------------------
	print*,'Writing out...'
	! File
	call err(nf90_create(output_nc_file, nf90_write, ncid))
	! Dimensions
	call err(nf90_def_dim(ncid, 'lon_box1', nlon1, dimids(1)))
	call err(nf90_def_dim(ncid, 'lat_box1', nlat1, dimids(2)))
	call err(nf90_def_dim(ncid, 'lon_box2', nlon2, dimids(3)))
	call err(nf90_def_dim(ncid, 'lat_box2', nlat2, dimids(4)))
	call err(nf90_def_dim(ncid, 'time', nt, dimids(5)))
	call err(nf90_def_dim(ncid, 'mode', svdNkeep, dimids(6)))
	! Box 1
	call err(nf90_def_var(ncid, 'lon_box1', nf90_float, dimids(1), &
		& varids(1)))
	call err(nf90_put_att(ncid, varids(1), 'long_name', &
		& 'Longitude'))
	call err(nf90_put_att(ncid, varids(1), 'units', lon_units))
	call err(nf90_def_var(ncid, 'lat_box1', nf90_float, dimids(2), &
		& varids(2)))
	call err(nf90_put_att(ncid, varids(2), 'long_name', &
		& 'Latitude'))
	call err(nf90_put_att(ncid, varids(2), 'units', lat_units))
	! Box 2
	call err(nf90_def_var(ncid, 'lon_box2', nf90_float, dimids(3), &
		& varids(3)))
	call err(nf90_put_att(ncid, varids(3), 'long_name', &
		& 'Longitude'))
	call err(nf90_put_att(ncid, varids(3), 'units', lon_units))
	call err(nf90_def_var(ncid, 'lat_box2', nf90_float, dimids(4), &
		& varids(4)))
	call err(nf90_put_att(ncid, varids(4), 'long_name', &
		& 'Latitude'))
	call err(nf90_put_att(ncid, varids(4), 'units', lat_units))
	! Time
	call err(nf90_def_var(ncid, 'time', nf90_float, dimids(5), &
		& varids(5)))
	call err(nf90_put_att(ncid, varids(5), 'long_name', 'Time'))
	call err(nf90_put_att(ncid, varids(5), 'units', time_units))
	! Mode
	call err(nf90_def_var(ncid, 'mode', nf90_float, dimids(6), &
		& varids(6)))
	call err(nf90_put_att(ncid, varids(6), 'long_name', 'Mode'))
	call err(nf90_put_att(ncid, varids(6), 'units', 'level'))
	! Original fields
	! * box1
	call err(nf90_def_var(ncid, 'sst_box1', nf90_float, &
		& (/dimids(1),dimids(2),dimids(5)/), sstids(1)))
	call err(nf90_put_att(ncid, sstids(1), 'long_name', &
		& 'SST anomaly / original field / box 1'))
	call err(nf90_put_att(ncid, sstids(1), 'units', var_units))
	call err(nf90_put_att(ncid, sstids(1), 'missing_value', &
		& new_missing_value))
	! * box2
	call err(nf90_def_var(ncid, 'sst_box2', nf90_float, &
	 & (/dimids(3),dimids(4),dimids(5)/), sstids(2)))
	call err(nf90_put_att(ncid, sstids(2), 'long_name', &
		& 'SST anomaly / original field / box 2'))
	call err(nf90_put_att(ncid, sstids(2), 'units', var_units))
	call err(nf90_put_att(ncid, sstids(2), 'missing_value', &
		& new_missing_value))
	! PCA SST
	! * box1
	call err(nf90_def_var(ncid, 'sst_pca_box1', nf90_float, &
	 & (/dimids(1),dimids(2),dimids(5)/),sstids(3)))
	call err(nf90_put_att(ncid, sstids(3), 'long_name', &
		& 'SST anomaly rec. from PCA / box 1'))
	call err(nf90_put_att(ncid, sstids(3), 'missing_value', &
		& new_missing_value))
	! * box2
	call err(nf90_def_var(ncid, 'sst_pca_box2', nf90_float, &
	 & (/dimids(3),dimids(4),dimids(5)/),sstids(4)))
	call err(nf90_put_att(ncid, sstids(4), 'long_name', &
		& 'SST anomaly rec. from PCA / box 2'))
	call err(nf90_put_att(ncid, sstids(4), 'missing_value', &
		& new_missing_value))
	! SST computed by model
	! * Box 2
	call err(nf90_def_var(ncid, 'sst_model_box2', nf90_float, &
		& (/dimids(3),dimids(4),dimids(5)/),sstids(5)))
	call err(nf90_put_att(ncid, sstids(5), 'long_name', &
		& 'SST anomaly computed from box1 by model / box 2'))
	call err(nf90_put_att(ncid, sstids(5), 'units', var_units))
	call err(nf90_put_att(ncid, sstids(5), 'missing_value', &
		& new_missing_value))

	! Values
	call err(nf90_enddef(ncid))
	call err(nf90_put_var(ncid, varids(1), lon1))
	call err(nf90_put_var(ncid, varids(2), lat1))
	call err(nf90_put_var(ncid, varids(3), lon2))
	call err(nf90_put_var(ncid, varids(4), lat2))
	call err(nf90_put_var(ncid, varids(5), time))
	call err(nf90_put_var(ncid, varids(6), &
		& float((/(i,i=1,svdNkeep)/))))
	call err(nf90_put_var(ncid, sstids(1), sst1))
	call err(nf90_put_var(ncid, sstids(2), sst2))
	call err(nf90_put_var(ncid, sstids(3), pcaSst1))
	call err(nf90_put_var(ncid, sstids(4), pcaSst2))
	call err(nf90_put_var(ncid, sstids(5), svdSst2))

	call err(nf90_close(ncid))

end program example3

subroutine err(jstatus)

	use netcdf

	integer :: jstatus

	if (jstatus .ne. nf90_noerr) then
		print *, trim(nf90_strerror(jstatus))
		stop
	end if

end subroutine err

