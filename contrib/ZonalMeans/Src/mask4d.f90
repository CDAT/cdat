!_ ---------------------------------------------------------------------
!_ RCS lines preceded by "c_ "
!_ ---------------------------------------------------------------------
!_
!_ $Source: /home/geocmip/users/orr/gap/fortran/zm/RCS/mask4d.f90,v $ 
!_ $Revision: 1.3 $
!_ $Date: 2005/07/05 09:13:07 $   ;  $State: Exp $
!_ $Author: orr $ ;  $Locker:  $
!_
!_ ---------------------------------------------------------------------
!_ $Log: mask4d.f90,v $
!_ Revision 1.3  2005/07/05 09:13:07  orr
!_ Got rid of imasktab array (fixed to work with month=-1 option)
!_
!_ Revision 1.2  2005/03/03 17:03:35  orr
!_ Replaced WHERE statement with IF in DO loops
!_ because it caused a run-time error
!_ when treating 3-D arrays.
!_
!_ Revision 1.1  2000/06/27 09:31:49  orr
!_ Initial revision
!_
!_ ---------------------------------------------------------------------
!_ 
MODULE mpr_mask4d
CONTAINS
  SUBROUTINE mask4d(tab,mask,imt,jmt,kmt,nt,miss_val)
!
!       Routine to mask values with miss_val
!       for up to 4-D array, (3-D in space, 1-D in time)
!
!       J. Orr, LSCE/CEA, IPSL, France, 21 March 2000
!
    IMPLICIT NONE
    INTEGER, INTENT(in) :: imt, jmt, kmt, nt
    REAL*4, DIMENSION(imt,jmt,kmt), intent(in) :: mask
    REAL*4, DIMENSION(imt,jmt,kmt,nt), intent(inout) :: tab
    REAL*4, INTENT(in) :: miss_val

!   Local variables
    INTEGER i,j,k,n
    
!!$    print *, "MASK4D: nt =", nt
!!$    print *, "MASK4D: miss_val =", miss_val

    do n = 1,nt
      do k = 1,kmt
        do j = 1,jmt
          do i = 1,imt
             if (mask(i,j,k) .lt. 0.5) tab(i,j,k,n) = miss_val
          end do
        end do
      end do
    end do

!   print *, "MASK4D: tab set to missing values"

    RETURN
  END SUBROUTINE mask4d
END MODULE mpr_mask4d
