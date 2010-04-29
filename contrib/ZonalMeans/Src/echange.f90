!_ ---------------------------------------------------------------------
!_ RCS lines preceded by "c_ "
!_ ---------------------------------------------------------------------
!_
!_ $Source: /home/geocmip/users/orr/gap/fortran/zm/RCS/echange.f90,v $ 
!_ $Revision: 1.1 $
!_ $Date: 2000/06/27 09:31:49 $   ;  $State: Exp $
!_ $Author: orr $ ;  $Locker:  $
!_
!_ ---------------------------------------------------------------------
!_ $Log: echange.f90,v $
!_ Revision 1.1  2000/06/27  09:31:49  orr
!_ Initial revision
!_
!_ ---------------------------------------------------------------------
!_ 
MODULE mpr_echange
CONTAINS
  SUBROUTINE echange(i,j,x,y)
    
    IMPLICIT NONE
    
    INTEGER :: i, j
    REAL :: buffer_x, buffer_y
    REAL, DIMENSION(4) :: x, y
    
    buffer_x = x(i)
    buffer_y = y(i)
    x(i) = x(j)
    y(i) = y(j)
    x(j) = buffer_x
    y(j) = buffer_y
    
    RETURN
  END SUBROUTINE echange
END MODULE mpr_echange



