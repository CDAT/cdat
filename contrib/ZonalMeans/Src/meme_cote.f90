!_ ---------------------------------------------------------------------
!_ RCS lines preceded by "c_ "
!_ ---------------------------------------------------------------------
!_
!_ $Source: /home/geocmip/users/orr/gap/fortran/zm/RCS/meme_cote.f90,v $ 
!_ $Revision: 1.1 $
!_ $Date: 2000/06/27 09:31:49 $   ;  $State: Exp $
!_ $Author: orr $ ;  $Locker:  $
!_
!_ ---------------------------------------------------------------------
!_ $Log: meme_cote.f90,v $
!_ Revision 1.1  2000/06/27  09:31:49  orr
!_ Initial revision
!_
!_ ---------------------------------------------------------------------
!_ 
MODULE mpr_meme_cote
CONTAINS
  FUNCTION meme_cote(x,y)
!==================================================================
!     Fonction de test
!     M1 et M2sont ils du meme cote
!         1 oui   0 non 2 M1 M2 alignes avec M3 ou M4
!
!==================================================================

    IMPLICIT NONE

    REAL x(4),y(4)
    INTEGER meme_cote
    REAL xdroite2,xdroite3,xseg1,xseg4,yseg1,yseg4
    
    xseg1=x(1)
    xseg4=x(4) 
    yseg1=y(1)
    yseg4=y(4)
    
    xdroite2=((xseg1-xseg4)/(yseg1-yseg4))*y(2)+                 &
    &            (((xseg4*yseg1)-(xseg1*yseg4))/(yseg1-yseg4))
    xdroite3=((xseg1-xseg4)/(yseg1-yseg4))*y(3)+                 &  
    &            (((xseg4*yseg1)-(xseg1*yseg4))/(yseg1-yseg4))
    
    IF (((xdroite3-x(3))*(xdroite2-x(2))).EQ.0) THEN
        meme_cote=2
    ELSE
        IF  (((xdroite3-x(3))*(xdroite2-x(2))).GT.0) THEN
            meme_cote=1
        ELSE 
            meme_cote=0
        ENDIF
    ENDIF
    RETURN
  END FUNCTION meme_cote
END MODULE mpr_meme_cote














