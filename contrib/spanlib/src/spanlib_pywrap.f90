! File: spanlib_pywrap.f90
!
! This file is part of the SpanLib library.
! Copyright (C) 2006  Stephane Raynaud
! Contact: stephane dot raynaud at gmail dot com
!
! This library is free software; you can redistribute it and/or
! modify it under the terms of the GNU Lesser General Public
! License as published by the Free Software Foundation; either
! version 2.1 of the License, or (at your option) any later version.

! This library is distributed in the hope that it will be useful,
! but WITHOUT ANY WARRANTY; without even the implied warranty of
! MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
! Lesser General Public License for more details.
!
! You should have received a copy of the GNU Lesser General Public
! License along with this library; if not, write to the Free Software
! Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301  USA

! Interface to f90
! ================

subroutine pca(var, ns, nt, nkeep, xeof, pc, ev, ev_sum, weights, useteof)

	use spanlib, only: sl_pca

	implicit none

	! External
	! --------
	integer, intent(in)  :: ns,nt
	real,    intent(in)  :: var(ns,nt)
	integer, intent(in)  :: nkeep
	real,    intent(out) :: pc(nt,nkeep), xeof(ns,nkeep), ev(nkeep)
	real,    intent(in)  :: weights(ns)
	integer, intent(in)  :: useteof
	real,    intent(out) :: ev_sum

	! Call to original subroutine
	! ---------------------------
	call sl_pca(var, nkeep, xeof=xeof, pc=pc, ev=ev, ev_sum=ev_sum,&
	 & weights=weights, useteof=useteof)

end subroutine pca

subroutine pca_getec(var, xeof, ns, nt, nkept, ec, weights)

	use spanlib, only: sl_pca_getec

	implicit none

	! External
	! --------
	integer, intent(in)  :: ns,nt,nkept
	real,    intent(in)  :: var(ns,nt), xeof(ns,nkept),weights(ns)
	real,    intent(out) :: ec(nt,nkept)


	! Call to original subroutine
	! ---------------------------
	call sl_pca_getec(var, xeof, ec, weights=weights)

end subroutine pca_getec


subroutine pca_rec(xeof, pc, ns, nt, nkept, varrec, istart, iend)

	use spanlib, only: sl_pca_rec

	implicit none

	! External
	! --------
	integer, intent(in)  :: ns, nt, nkept, istart, iend
	real,    intent(in)  :: xeof(ns,nkept), pc(nt,nkept)
	real,   intent(out) :: varrec(ns,nt)

	! Call to original subroutine
	! ---------------------------
	call sl_pca_rec(xeof, pc, varrec=varrec, istart=istart, iend=iend)

end subroutine pca_rec



subroutine mssa(var, nchan, nt, nwindow, nkeep, steof, stpc, ev, ev_sum)

	use spanlib, only: sl_mssa

	implicit none

	! External
	! --------
	integer, intent(in)  :: nchan, nt, nwindow, nkeep
	real,    intent(in)  :: var(nchan,nt)
	real,    intent(out) :: steof(nchan*nwindow,nkeep), &
	 & stpc(nt-nwindow+1,nkeep), ev(nkeep)
	real,    intent(out) :: ev_sum

	! Call to original subroutine
	! ---------------------------
	call sl_mssa(var, nwindow, nkeep, steof=steof, stpc=stpc, ev=ev, &
	 & ev_sum=ev_sum)

end subroutine mssa

subroutine stcov(var, cov, nchan, nt, nwindow)

	use spanlib, only: sl_stcov
	
	implicit none
	
	! External
	! --------
	integer, intent(in)  :: nchan, nt, nwindow
	real,    intent(in)  :: var(nchan,nt)
	real,    intent(out) :: cov(nchan*nwindow,nchan*nwindow)

	! Call to original subroutine
	! ---------------------------
	call sl_stcov(var, cov)

end subroutine stcov


subroutine mssa_getec(var, steof, nchan, nt, nkept, nwindow, stec)

	use spanlib, only: sl_mssa_getec

	implicit none

	! External
	! --------
	integer, intent(in) :: nchan, nt, nwindow, nkept
	real,    intent(in) :: var(nchan,nt), &
	 & steof(nchan*nwindow,nkept)
	real,    intent(out)  :: stec(nt-nwindow+1, nkept)

	! Call to original subroutine
	! ---------------------------
	call sl_mssa_getec(var, steof, nwindow, stec)

end subroutine mssa_getec

subroutine mssa_rec(steof, stpc, nchan, nt, nkeep, nwindow, &
  & varrec, istart, iend)

	use spanlib, only: sl_mssa_rec

	implicit none

	! External
	! --------
	integer, intent(in)  :: nchan, nt, nwindow, nkeep
	integer, intent(in)  :: istart, iend
	real,    intent(in)  :: steof(nchan*nwindow,nkeep), &
	 & stpc(nt-nwindow+1,nkeep)
	real,    intent(out) :: varrec(nchan,nt)

	! Call to original subroutine
	! ---------------------------
	call sl_mssa_rec(steof, stpc, nwindow, varrec=varrec, &
	 & istart=istart, iend=iend)

end subroutine mssa_rec



subroutine phasecomp(varrec, ns, nt, np,  phases, weights, &
  & offset, firstphase)

	use spanlib, only: sl_phasecomp

	implicit none

	! External
	! --------
	integer, intent(in)  :: ns, nt, np
	real,    intent(in)  :: varrec(ns,nt)
	real,    intent(in)  :: weights(ns)
	real,    intent(in)  :: offset, firstphase
	real    ,intent(out) :: phases(ns, np)

	! Call to original subroutine
	! ---------------------------
	call sl_phasecomp(varrec, np, phases, weights=weights, &
	 & offset=offset, firstphase=firstphase)

end subroutine phasecomp



subroutine svd(ll, nsl, rr, nsr, nt, nkeep, leof, reof, lpc, rpc, ev, ev_sum, lweights, rweights, usecorr, largematrix, info)

	use spanlib, only: sl_svd

	implicit none

	! External
	! --------
	integer, intent(in)  :: nsl,nsr,nt
	real,    intent(in)  :: ll(nsl,nt),rr(nsr,nt)
	integer, intent(in)  :: nkeep
	real,    intent(out) :: lpc(nt,nkeep), leof(nsl,nkeep)
	real,    intent(out) :: rpc(nt,nkeep), reof(nsr,nkeep), ev(nkeep)
	real,    intent(in)  :: lweights(nsl),rweights(nsr)
	logical, intent(in)  :: usecorr,largematrix
	real,    intent(out) :: ev_sum
	integer, intent(out) :: info
	
	! Internal
	! --------

	! Call to original subroutine
	! ---------------------------
	call sl_svd(ll,rr,nkeep,leof,reof,lpc,rpc,ev,ev_sum,lweights,rweights,usecorr,largematrix,info)

end subroutine svd


! Utilities
! =========

subroutine chan_pack(varNd, mask, nstot, nt, var2d, ns)

	implicit none

	! External
	! --------
	integer, intent(in)  :: nstot, nt, ns
	real,    intent(in)  :: varNd(nt,nstot)
	integer, intent(in)  :: mask(nstot)
	real,    intent(out) :: var2d(ns,nt)

	! Internal
	! --------
	integer :: it

	! Call to pack
	! ------------
	do it = 1, nt
		var2d(:,it) = pack(varNd(it,:), mask==1)
	end do

end subroutine chan_pack


subroutine chan_unpack(varNd, mask, nstot, nt, var2d, ns, &
  & missing_value)

	implicit none

	! External
	! --------
	integer, intent(in)  :: nstot, nt, ns
	real,    intent(out) :: varNd(nt,nstot)
	integer, intent(in)  :: mask(nstot)
	real,    intent(in)  :: var2d(ns,nt),missing_value

	! Internal
	! --------
	integer :: it

	! Call to pack
	! ------------
	do it = 1, nt
		varNd(it,:) = unpack(var2d(:,it), mask==1, missing_value)
	end do

end subroutine chan_unpack

