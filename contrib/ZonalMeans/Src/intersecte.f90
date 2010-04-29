!_ ---------------------------------------------------------------------
!_ RCS lines preceded by "c_ "
!_ ---------------------------------------------------------------------
!_
!_ $Source: /home/geocmip/users/orr/gap/fortran/zm/RCS/intersecte.f90,v $ 
!_ $Revision: 1.1 $
!_ $Date: 2000/06/27 09:31:49 $   ;  $State: Exp $
!_ $Author: orr $ ;  $Locker:  $
!_
!_ ---------------------------------------------------------------------
!_ $Log: intersecte.f90,v $
!_ Revision 1.1  2000/06/27  09:31:49  orr
!_ Initial revision
!_
!_ ---------------------------------------------------------------------
!_ 
MODULE mpr_intersecte  
CONTAINS
  FUNCTION intersecte(xseg1,yseg1,xseg2,yseg2,latitude,xint,yint)
!====================================================================
!     sous programme de calcul de l'intersection d'un segment avec une 
!     ligne de latitude cte
!
!     variables d'entree :
!                segment: xseg1,yseg1,xseg2,yseg2
!                ligne de latitude: latitude
!
!     variable de sortie
!                intersecte :  1 si intersection
!                              0 si non intersection
!                              2 si confondu
!                xint,yint
!======================================================================

    IMPLICIT NONE
    
    REAL xseg1,yseg1,xseg2,yseg2,latitude
    REAL xint,yint
!   integer intersecte
    INTEGER intersecte
    
    IF (((yseg1.GT.latitude).AND.(yseg2.GT.latitude)).OR.       &
        &    ((yseg1.LT.latitude).AND.(yseg2.LT.latitude))) THEN
        intersecte = 0
    ELSE
        IF (((yseg1.EQ.latitude).AND.(yseg2.EQ.latitude))) THEN
            intersecte = 2
        ELSE
            xint=((xseg1-xseg2)/(yseg1-yseg2))*latitude+          &
            (((xseg2*yseg1)-(xseg1*yseg2))/(yseg1-yseg2))
            yint=latitude
            intersecte=1
        ENDIF
    ENDIF
    
    RETURN
  END FUNCTION intersecte
END MODULE mpr_intersecte









