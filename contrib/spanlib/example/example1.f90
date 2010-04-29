! File: example1.f90
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

program example

	! This simple example shows how to PCA and MSSA subroutines from this package.
	! Warning: it requires netcdf for in/outputs.
	!
	! We start from longitude/latitude/time value of Pacific Sea Surface Temperature
	! that include the El Nino Southern Oscillation signal.
	! Input is the netcdf file data2.cdf.
	! We remove land points from the initial array according
	! to the netcdf missing_value attribute of the analysed variable (data are "packed").
	! A PCA is used to reduce the degrees of freedoom before MSSA analysis.
	! Weights for PCA are computed as a fonction of latitude.
	! Then, we assume that we have already identified an oscillation (after tests).
	! This oscillation, given by a pair of MSSA modes, is then
	! reconstructed from the MSSA and PCA spaces.
	! Finally, phase composites are computed from this reconstructed oscillation.
	! The oscillation is outputed in a netcdf file (pair_1.cdf).
	! MSSA eigenvalue are stored in the output netcdf file. This shows
	! you pairs due to oscillatory modes.
	!
	! Initial data set (data2.cdf):
	! - origin: updated Reynolds and Smith (1996) SST (netcdf file)
	! - origin url: data selector from http://iridl.ldeo.columbia.edu
	! - how to get it [10Mb]: http://stefdeperou.free.fr/pub/data.cdf
	! - area of study: tropical pacific [130.5E:75.5W, 29.5S:29.5N] (155x60 grid points)
	! - period of study: Jan1982:Dec2005 (288 time steps)
	!
	! Parameters:
	! - Only the first 20 PCs are retainedand given to the MSSA
	! - A window of 7 years (84 months) is chosen for the MSSA
	! - Phase composites use 8 phases
	! - An offset of 0.4 and is used for composites
	! - The first phase of composites is set at 180 degrees (minimal value)
	!
	! Note:
	!	This example should run only a few seconds.
	!	If it is not the case, your BLAS/LAPACK librairy is not optimized.

	use spanlib
	use netcdf

	implicit none

	! Parameters
	! ----------
	integer,parameter :: nkeep_pca=10, nkeep_mssa=6, nwindow=84, &
		& first_mode=1, nphases=8
	real(wp), parameter :: offset=0.d0, first_phase=180.d0, &
		& new_missing_value=-999.d0
	character(len=20), parameter :: input_nc_file="data2.cdf", &
		& output_nc_file="output_fortran1.nc", var_name='ssta'

	! Other declarations
	! ------------------
	real(wp), allocatable :: field(:,:,:), weights(:,:), &
		& lat(:), lon(:), time(:)
	real(wp), allocatable :: reco(:,:,:), phasecomps(:,:,:)
	logical, allocatable :: mask(:,:)
	real(wp), allocatable :: packed_field(:,:), &
		& packed_weights(:), packed_phasecomps(:,:), stphasecomps(:,:)
	real(wp), allocatable :: eof(:,:), pc(:,:), &
		& stpair(:,:), pair(:,:)
	real(wp), allocatable :: steof(:,:),stpc(:,:),stev(:)
	character(len=20) :: lon_units, lat_units, var_units, &
		&	lon_name, lat_name, time_name, time_units
	integer :: ncid, dimids(5), varid, lonid, latid, phaseid, &
		& timeid, phcoid, recoid, origid, modeid, evid
	integer(kind=4) :: i, nspace, nlon, nlat, ntime
	real(wp) :: pi, missing_value

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
	call err(nf90_inq_varid(ncid, var_name, varid))
	call err(nf90_inquire_variable(ncid, varid, dimids=dimids(1:3)))
	call err(nf90_inquire_dimension(ncid, dimids(1), &
		&	name=lon_name, len=nlon))
	call err(nf90_inquire_dimension(ncid, dimids(2), &
		&	name=lat_name, len=nlat))
	call err(nf90_inquire_dimension(ncid, dimids(3), &
		&	name=time_name, len=ntime))
	allocate(field(nlon,nlat,ntime))
	allocate(mask(nlon,nlat))
	allocate(weights(nlon,nlat))
	allocate(lon(nlon))
	allocate(lat(nlat))
	allocate(time(ntime))
	call err(nf90_get_var(ncid, varid, field))
	call err(nf90_get_att(ncid, varid, 'missing_value', &
		& missing_value))
	call err(nf90_get_att(ncid, varid, 'units', var_units))
	call err(nf90_inq_varid(ncid, lon_name, varid))
	call err(nf90_get_var(ncid, varid, lon))
	call err(nf90_get_att(ncid, varid, 'units', lon_units))
	call err(nf90_inq_varid(ncid, lat_name, varid))
	call err(nf90_get_var(ncid, varid, lat))
	call err(nf90_get_att(ncid, varid, 'units', lat_units))
	call err(nf90_inq_varid(ncid, time_name, varid))
	call err(nf90_get_var(ncid, varid, time))
	call err(nf90_get_att(ncid, varid, 'units', time_units))
	call err(nf90_close(ncid))


	! Format (pack) data to have only one space dimension
	! ---------------------------------------------------
	print*,'Packing...'

	! Compute weights proportional to grid point area
	pi = cos(-1.)
	do i=1,nlat
		weights(:,i) = cos(lat(i)*pi/180.)
	end do

	! Now pack
	mask = (field(:,:,1) /= missing_value)
	allocate(packed_field(count(mask), ntime))
	do i=1, ntime
		packed_field(:,i) = pack(field(:,:,i), mask)
	end do
	nspace = count(mask)
	allocate(packed_weights(nspace))
	packed_weights = pack(weights, mask)


	! Perform a PCA to reduce the d.o.f
	! ---------------------------------
	print*,'[sl_pca] Pre-PCA...'
	allocate(eof(nspace, nkeep_pca))
	allocate(pc(ntime,   nkeep_pca))
	call sl_pca(packed_field, nkeep_pca, xeof=eof, &
		&	pc=pc, weights=packed_weights)
	deallocate(packed_field)

	! We send results from PCA to MSSA
	! --------------------------------
	print*,'[sl_mssa] MSSA...'
	allocate(steof(nkeep_pca*nwindow, nkeep_mssa))
	allocate(stpc(ntime-nwindow+1,    nkeep_mssa))
	allocate(stev(                    nkeep_mssa))
	call sl_mssa(transpose(pc), nwindow, nkeep_mssa, &
		&	steof=steof, stpc=stpc, ev=stev)

	! We reconstruct modes [first_mode + first_mode+1] of MSSA
	! --------------------------------------------------------
	print*,'[sl_mssa_rec] MSSA reconstruction...'
	allocate(stpair(nkeep_pca, ntime))
	call sl_mssa_rec(steof(:,first_mode:first_mode+1), &
		& stpc(:,first_mode:first_mode+1), nwindow, stpair)
	deallocate(steof, stpc)

	! We compute phases composites for the reconstructed oscillation
	! ---------------------------------------------------------------
	print*,'[sl_phasecomp] Phase composites...'
	allocate(stphasecomps(nkeep_pca, nphases))
	call sl_phasecomp(stpair, nphases, stphasecomps, &
		&	offset=offset, firstphase=first_phase)

	! We go back to the physical space for
	! the full oscillation AND its composites
	! ---------------------------------------
	print*,'[sl_pca_rec] Back to the physical space...'
	allocate(pair(nspace, ntime))
	call sl_pca_rec(eof, transpose(stpair), pair)
	allocate(packed_phasecomps(nspace, nphases))
	call sl_pca_rec(eof, transpose(stphasecomps), packed_phasecomps)
	deallocate(stpair, eof, stphasecomps)

	! Unpacking
	! ---------
	print*,'Unpacking...'
	allocate(reco(nlon,nlat,ntime))
	do i=1, ntime
		reco(:,:,i) = unpack(pair(:,i), mask, new_missing_value)
		where(.not.mask)field(:,:,i) = new_missing_value
	end do
	allocate(phasecomps(nlon,nlat,nphases))
	do i=1, nphases
		phasecomps(:,:,i) = unpack(packed_phasecomps(:,i), mask, &
		 & new_missing_value)
	end do

	! Write out the phase composites of the first oscillation
	! -------------------------------------------------------
	print*,'Writing out...'
	! File
	call err(nf90_create(output_nc_file, nf90_write, ncid))
	! Dimensions
	call err(nf90_def_dim(ncid, 'lon', nlon, dimids(1)))
	call err(nf90_def_dim(ncid, 'lat', nlat, dimids(2)))
	call err(nf90_def_dim(ncid, 'time', ntime, dimids(3)))
	call err(nf90_def_dim(ncid, 'phase', nphases, dimids(4)))
	call err(nf90_def_dim(ncid, 'mode', nkeep_mssa, dimids(5)))
	! Variables
	call err(nf90_def_var(ncid, 'lon', nf90_float, dimids(1), lonid))
	call err(nf90_put_att(ncid, lonid, 'long_name', 'Longitude'))
	call err(nf90_put_att(ncid, lonid, 'units', lon_units))
	call err(nf90_def_var(ncid, 'lat', nf90_float, dimids(2), latid))
	call err(nf90_put_att(ncid, latid, 'long_name', 'Latitude'))
	call err(nf90_put_att(ncid, latid, 'units', lat_units))
	call err(nf90_def_var(ncid, 'time', nf90_float, dimids(3), &
		& timeid))
	call err(nf90_put_att(ncid, timeid, 'long_name', 'Time'))
	call err(nf90_put_att(ncid, timeid, 'units', time_units))
	call err(nf90_def_var(ncid, 'phase', nf90_float, dimids(4), &
		& phaseid))
	call err(nf90_put_att(ncid, phaseid, 'long_name', 'Phase'))
	call err(nf90_put_att(ncid, phaseid, 'units', 'level'))
	call err(nf90_def_var(ncid, 'mode', nf90_float, dimids(5), &
		& modeid))
	call err(nf90_put_att(ncid, modeid, 'long_name', 'Mode'))
	call err(nf90_put_att(ncid, modeid, 'units', 'level'))
	call err(nf90_def_var(ncid, 'orig', nf90_float, dimids(1:3), &
	 & origid))
	call err(nf90_put_att(ncid, origid, 'long_name', &
		& 'SST anomaly / original field'))
	call err(nf90_put_att(ncid, origid, 'units', var_units))
	call err(nf90_put_att(ncid, origid, 'missing_value', &
		& new_missing_value))
	call err(nf90_def_var(ncid, 'reco1', nf90_float, dimids(1:3), &
		& recoid))
	call err(nf90_put_att(ncid, recoid, 'long_name', &
		& 'SST anomaly / reconstruction of first pair'))
	call err(nf90_put_att(ncid, recoid, 'units', var_units))
	call err(nf90_put_att(ncid, recoid, 'missing_value', &
		& new_missing_value))
	call err(nf90_def_var(ncid, 'pair1', nf90_float, &
		& (/dimids(1),dimids(2),dimids(4)/), phcoid))
	call err(nf90_put_att(ncid, phcoid, 'long_name', &
		&'SST anomaly / phase composite of first pair'))
	call err(nf90_put_att(ncid, phcoid, 'units', var_units))
	call err(nf90_put_att(ncid, phcoid, 'missing_value', &
		& new_missing_value))
	call err(nf90_def_var(ncid, 'ev', nf90_float, dimids(5), evid))
	call err(nf90_put_att(ncid, evid, 'long_name', &
		&'MSSA eigen values'))
	! Values
	call err(nf90_enddef(ncid))
	call err(nf90_put_var(ncid, lonid, lon))
	call err(nf90_put_var(ncid, latid, lat))
	call err(nf90_put_var(ncid, timeid, time))
	call err(nf90_put_var(ncid, phaseid, float((/(i,i=1,nphases)/))))
	call err(nf90_put_var(ncid, modeid, &
		& float((/(i,i=1,nkeep_mssa)/))))
	call err(nf90_put_var(ncid, origid, field))
	call err(nf90_put_var(ncid, recoid, reco))
	call err(nf90_put_var(ncid, phcoid, phasecomps))
	call err(nf90_put_var(ncid, evid, stev))
	call err(nf90_close(ncid))

end program example

subroutine err(jstatus)

	use netcdf

	integer :: jstatus

	if (jstatus .ne. nf90_noerr) then
		print *, trim(nf90_strerror(jstatus))
		stop
	end if

end subroutine err

