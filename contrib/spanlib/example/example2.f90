! File: example2.f90
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

program example2

	! This simple example shows how to use PCA and SVD from this package.
	! Warning: it requires netcdf for in/outputs.
	!
	! This example works in a very similar way to the first fortran example,
	! with the same initial dataset.
	!
	! To mimics the use of two different datasets, two geographical
	! regions are decomposed using SVD. As for MSSA, a pre-PCA is
	! performed (independantly for the two regions).
	! One region is situated in the east of the basin, the other
	! one is on the west.
	! The dominant EOFs from the SVD decomposition are reconstructed
	! back to the physical space and stored in the output netcdf file
	! along with their PCs.
	!
	! Note:
	!	This example should run only a few seconds.
	!	If it is not the case, your BLAS/LAPACK librairy is not optimized.


	use spanlib
	use netcdf

	implicit none

	! Parameters
	! ----------
	integer,parameter :: pcaNkeep=10, svdNkeep=5,&
		& lons1(2)=(/70,150/),lats1(2)=(/16,45/),& ! East
		& lons2(2)=(/10,40/),lats2(2)=(/12,49/)    ! West
	real(wp), parameter ::new_missing_value=-999.d0
	character(len=20), parameter :: input_nc_file="data2.cdf", &
		& output_nc_file="output_fortran2.nc", var_name='ssta'

	! Other declarations
	! ------------------
	real(wp), allocatable :: lon1(:), lat1(:), &
		& lon2(:), lat2(:), time(:), sst1(:,:,:), sst2(:,:,:)
	logical, allocatable :: mask1(:,:),mask2(:,:)
	real(wp), allocatable :: packed_sst1(:,:),packed_sst2(:,:),&
		& pcaSstRec1(:,:,:),pcaSstRec2(:,:,:)
	real(wp), allocatable :: svdEv(:), &
		& pcaEofs1(:,:),pcaEofs2(:,:),pcaPcs1(:,:),pcaPcs2(:,:), &
		& svdEofs1(:,:),svdEofs2(:,:),svdPcs1(:,:),svdPcs2(:,:)
	real(wp), allocatable :: &
		& svdEofsRec1(:,:,:), svdEofsRec2(:,:,:), &
		& packed_svdEofsRec1(:,:), packed_svdEofsRec2(:,:), &
		& packed_svdSstRec1(:,:), packed_svdSstRec2(:,:), &
		& packed_pcaSstRec1(:,:), packed_pcaSstRec2(:,:)
	character(len=20) :: lon_units, lat_units, var_units, &
		&	lon_name, lat_name, time_name, time_units
	integer :: ncid, dimids(6), varids(6), sstids(9)
	integer(kind=4) :: i,nlon1,nlat1,nlon2,nlat2,ntime,ns1,ns2
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
		&	name=time_name, len=ntime))
	! Allocations
	allocate(sst1(nlon1,nlat1,ntime),sst2(nlon2,nlat2,ntime))
	allocate(mask1(nlon1,nlat1),mask2(nlon2,nlat2))
	allocate(lon1(nlon1),lat1(nlat1))
	allocate(lon2(nlon2),lat2(nlat2))
	allocate(time(ntime))
	! SST boxes and attributes
	call err(nf90_get_var(ncid, sstids(1), sst1,&
		& start=(/lons1(1),lats1(1),1/), &
		& count=(/lons1(2)-lons1(1)+1,lats1(2)-lats1(1)+1,ntime/)))
	call err(nf90_get_var(ncid, sstids(1), sst2,&
		& start=(/lons2(1),lats2(1),1/), &
		& count=(/lons2(2)-lons2(1)+1,lats2(2)-lats2(1)+1,ntime/)))
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
	allocate(packed_sst1(ns1, ntime))
	allocate(packed_sst2(ns2, ntime))
	do i=1, ntime
		packed_sst1(:,i) = pack(sst1(:,:,i), mask1)
		packed_sst2(:,i) = pack(sst2(:,:,i), mask2)
	end do
	where(sst1==missing_value)sst1 = new_missing_value
	where(sst2==missing_value)sst2 = new_missing_value

	! Perform a PCA to reduce the d.o.f
	! ---------------------------------
	print*,'[sl_pca] Pre-PCA ...'
	allocate(pcaEofs1(ns1, pcaNkeep))
	allocate(pcaPcs1(ntime,pcaNkeep))
	call sl_pca(packed_sst1, pcaNkeep, xeof=pcaEofs1, pc=pcaPcs1)
	allocate(pcaEofs2(ns2, pcaNkeep))
	allocate(pcaPcs2(ntime,pcaNkeep))
	call sl_pca(packed_sst2, pcaNkeep, xeof=pcaEofs2, pc=pcaPcs2)
	deallocate(packed_sst1,packed_sst2)


	! Perform a SVD on previous PCs
	! -----------------------------
	print*,'[sl_svd] SVD...'
	allocate(svdEofs1(pcaNkeep,svdNkeep),svdPcs1(ntime,svdNkeep))
	allocate(svdEofs2(pcaNkeep,svdNkeep),svdPcs2(ntime,svdNkeep))
	allocate(svdEv(svdNkeep))
	call sl_svd(transpose(pcaPcs1),transpose(pcaPcs2),&
		& svdNkeep,leof=svdEofs1,reof=svdEofs2,&
		& lpc=svdPcs1,rpc=svdPcs2,ev=svdEv)
	deallocate(pcaPcs1,pcaPcs2)

	! Swicth SVD EOFs to the physical space (!)
	! -----------------------------------------
	print*,'[sl_pca_rec] Back EOF to the physical space ...'
	allocate(packed_svdEofsRec1(ns1,svdNkeep))
	allocate(packed_svdEofsRec2(ns2,svdNkeep))
	call sl_pca_rec(pcaEofs1, transpose(svdEofs1), packed_svdEofsRec1)
	call sl_pca_rec(pcaEofs2, transpose(svdEofs2), packed_svdEofsRec2)

	! Switch SST to the physical space
	! --------------------------------
	! SVD to pre-PCA
	print*,'[sl_pca_rec] Back SST to the physical space ...'
	allocate(packed_svdSstRec1(pcaNkeep,ntime))
	allocate(packed_svdSstRec2(pcaNkeep,ntime))
	call sl_pca_rec(svdEofs1,svdPcs1,packed_svdSstRec1)
	call sl_pca_rec(svdEofs2,svdPcs2,packed_svdSstRec2)
	! Pre-PCA to physical
	allocate(packed_pcaSstRec1(ns1,ntime),packed_pcaSstRec2(ns2,ntime))
	call sl_pca_rec(pcaEofs1,transpose(packed_svdSstRec1),&
		& packed_pcaSstRec1)
	call sl_pca_rec(pcaEofs2,transpose(packed_svdSstRec2),&
		& packed_pcaSstRec2)
	deallocate(pcaEofs1,svdEofs1,pcaEofs2,svdEofs2)
	deallocate(packed_svdSstRec1,packed_svdSstRec2)

	! Unpacking
	! ---------
	print*,'Unpacking...'
	allocate(svdEofsRec1(nlon1,nlat1,svdNkeep))
	allocate(svdEofsRec2(nlon2,nlat2,svdNkeep))
	allocate(pcaSstRec1(nlon1,nlat1,ntime))
	allocate(pcaSstRec2(nlon2,nlat2,ntime))
	! Eofs
	do i=1, svdNkeep
		svdEofsRec1(:,:,i) = unpack(packed_svdEofsRec1(:,i), &
			& mask1, new_missing_value)
		svdEofsRec2(:,:,i) = unpack(packed_svdEofsRec2(:,i), &
			& mask2, new_missing_value)
		where(.not.mask1)svdEofsRec1(:,:,i) = new_missing_value
		where(.not.mask2)svdEofsRec2(:,:,i) = new_missing_value
	end do
	! SST
	do i=1, ntime
		pcaSstRec1(:,:,i) = unpack(packed_pcaSstRec1(:,i), &
			& mask1, new_missing_value)
		pcaSstRec2(:,:,i) = unpack(packed_pcaSstRec2(:,i), &
			& mask2, new_missing_value)
		where(.not.mask1)pcaSstRec1(:,:,i) = new_missing_value
		where(.not.mask2)pcaSstRec2(:,:,i) = new_missing_value
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
	call err(nf90_def_dim(ncid, 'time', ntime, dimids(5)))
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
	! SVD EOFs
	! * box1
	call err(nf90_def_var(ncid, 'eofs_box1', nf90_float, &
	 & (/dimids(1),dimids(2),dimids(6)/),sstids(3)))
	call err(nf90_put_att(ncid, sstids(3), 'long_name', &
		& 'SVD EOFs of SST anomaly / box 1'))
	call err(nf90_put_att(ncid, sstids(3), 'missing_value', &
		& new_missing_value))
	! * box2
	call err(nf90_def_var(ncid, 'eofs_box2', nf90_float, &
	 & (/dimids(3),dimids(4),dimids(6)/),sstids(4)))
	call err(nf90_put_att(ncid, sstids(4), 'long_name', &
		& 'SVD EOFs of SST anomaly / box 2'))
	call err(nf90_put_att(ncid, sstids(4), 'missing_value', &
		& new_missing_value))
	! SVD PCs
	! * Box 1
	call err(nf90_def_var(ncid, 'pcs_box1', nf90_float, &
		& (/dimids(5),dimids(6)/),sstids(5)))
	call err(nf90_put_att(ncid, sstids(5), 'long_name', &
		& 'SVD EOFs of SST anomaly / box 1'))
	call err(nf90_put_att(ncid, sstids(5), 'units', var_units))
	call err(nf90_put_att(ncid, sstids(5), 'missing_value', &
		& new_missing_value))
	! * Box 2
	call err(nf90_def_var(ncid, 'pcs_box2', nf90_float, &
		& (/dimids(5),dimids(6)/),sstids(6)))
	call err(nf90_put_att(ncid, sstids(6), 'long_name', &
		& 'SVD EOFs of SST anomaly / box 2'))
	call err(nf90_put_att(ncid, sstids(6), 'units', var_units))
	call err(nf90_put_att(ncid, sstids(6), 'missing_value', &
		& new_missing_value))
	! SVD eigen values
	call err(nf90_def_var(ncid, 'ev', nf90_float, &
		& dimids(6),sstids(7)))
	call err(nf90_put_att(ncid, sstids(7), 'long_name', &
		& 'Eigen values'))
	! Reconstructed SST
	! * box1
	call err(nf90_def_var(ncid, 'sstrec_box1', nf90_float, &
	 & (/dimids(1),dimids(2),dimids(5)/),sstids(8)))
	call err(nf90_put_att(ncid, sstids(8), 'long_name', &
		& 'Reconstructed SST anomaly / box 1'))
	call err(nf90_put_att(ncid, sstids(8), 'missing_value', &
		& new_missing_value))
	! * box2
	call err(nf90_def_var(ncid, 'sstrec_box2', nf90_float, &
	 & (/dimids(3),dimids(4),dimids(5)/),sstids(9)))
	call err(nf90_put_att(ncid, sstids(9), 'long_name', &
		& 'Reconstructed SST anomaly / box 2'))
	call err(nf90_put_att(ncid, sstids(9), 'missing_value', &
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
	call err(nf90_put_var(ncid, sstids(3), svdEofsRec1))
	call err(nf90_put_var(ncid, sstids(4), svdEofsRec2))
	call err(nf90_put_var(ncid, sstids(5), svdPcs1))
	call err(nf90_put_var(ncid, sstids(6), svdPcs2))
	call err(nf90_put_var(ncid, sstids(7), svdEv))
	call err(nf90_put_var(ncid, sstids(8), pcaSstRec1))
	call err(nf90_put_var(ncid, sstids(9), pcaSstRec2))

	call err(nf90_close(ncid))

end program example2

subroutine err(jstatus)

	use netcdf

	integer :: jstatus

	if (jstatus .ne. nf90_noerr) then
		print *, trim(nf90_strerror(jstatus))
		stop
	end if

end subroutine err

