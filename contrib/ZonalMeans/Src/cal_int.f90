!_ ---------------------------------------------------------------------
!_ RCS lines preceded by "c_ "
!_ ---------------------------------------------------------------------
!_
!_ $Source: /home/geocmip/users/orr/gap/fortran/zm/RCS/cal_int.f90,v $ 
!_ $Revision: 1.1 $
!_ $Date: 2000/06/27 09:31:49 $   ;  $State: Exp $
!_ $Author: orr $ ;  $Locker:  $
!_
!_ ---------------------------------------------------------------------
!_ $Log: cal_int.f90,v $
!_ Revision 1.1  2000/06/27  09:31:49  orr
!_ Initial revision
!_
!_ ---------------------------------------------------------------------
!_ 
MODULE mpr_cal_int
CONTAINS
  SUBROUTINE cal_int(x,y,rlatmax,rlatmin,latitude           &
     &     ,haut,x_sortie,y_sortie,info)
!====================================================================== 
!     calculs des points d'intersection
!
!     variables d'entree     x(4),y(4),rlatmax,rlatmin
!                            latitude
!                            haut=Intersection avec le demi plan 
!                            0 : inferieur 1 : superieur
!     variables de sortie    x_sortie(2),y_sortie(2)
!
!====================================================================

      USE mpr_meme_cote
      USE mpr_intersecte
      USE mpr_echange

        IMPLICIT NONE

        REAL rlatmax,rlatmin,x_sortie(5),y_sortie(5)
        REAL x(4),y(4)
        REAL latitude 
        
        INTEGER info,inc
        INTEGER haut
!        INTEGER meme_cote, intersecte
        REAL xint, yint
        
!---------------positionement des 4 points de la boite-----------
        
latmax: DO inc=1,4
          IF (y(inc).EQ.rlatmax) THEN
              CALL echange(inc,1,x,y)
              exit latmax
          ENDIF
        END DO latmax
        
latmin: DO inc=2,4
          IF (y(inc).EQ.rlatmin) THEN 
              CALL echange(inc,4,x,y)
              exit latmin
          ENDIF
        END DO latmin
        
        IF ((y(1).EQ.y(2)).AND.(x(1).LT.x(2))) THEN
            CALL echange(1,2,x,y)
        ENDIF
        
        IF ((y(1).EQ.y(3)).AND.(x(1).LT.x(3))) THEN
            CALL echange(1,3,x,y) 
        ENDIF
        
        IF ((y(4).EQ.y(2)).AND.(x(4).GT.x(2))) THEN 
            CALL echange(4,2,x,y)
        ENDIF
        
        IF ((y(4).EQ.y(3)).AND.(x(4).GT.x(3))) THEN 
            CALL echange(4,3,x,y)
        ENDIF
        
!-----------------------------------------------------------------------
        IF (meme_cote(x,y).EQ.0) THEN
            IF (intersecte(x(1),y(1),x(2),y(2),latitude,xint,yint).NE.0)THEN 
            IF (intersecte(x(1),y(1),x(2),y(2),latitude,xint,yint) .EQ. 2) THEN
                IF (haut.EQ.1) THEN
                    info=0
                    RETURN
                ELSE
                    x_sortie(1)=x(1)
                    y_sortie(1)=y(1)
                    x_sortie(2)=x(2)
                    y_sortie(2)=y(2)
                    x_sortie(3)=x(4)
                    y_sortie(3)=y(4)
                    x_sortie(4)=x(3)
                    y_sortie(4)=y(3)
                    info =4
                    RETURN
                ENDIF
            ELSE
                IF (haut.EQ.1) THEN
                    x_sortie(1)=xint
                    y_sortie(1)=yint
                    x_sortie(2)=x(1)
                    y_sortie(2)=y(1)
                    info=2 
                ELSE
                    x_sortie(1)=xint
                    y_sortie(1)=yint
                    x_sortie(2)=x(2)
                    y_sortie(2)=y(2)
                    x_sortie(3)=x(4)
                    y_sortie(3)=y(4)
                    info =3 
                ENDIF
            ENDIF
        ELSE 
            IF (intersecte(x(2),y(2),x(4),y(4),latitude,xint,yint) .NE. 0) THEN
                IF (intersecte(x(2),y(2),x(4),y(4),latitude,xint,yint).EQ.2) THEN
                    IF (haut.EQ.1) THEN
                        x_sortie(1)=x(1)
                        y_sortie(1)=y(1)
                        x_sortie(2)=x(2)
                        y_sortie(2)=y(2)
                        x_sortie(3)=x(4)
                        y_sortie(3)=y(4)
                        x_sortie(4)=x(3)
                        y_sortie(4)=y(3)
                        info=4
                        RETURN
                    ELSE
                        info = 0
                        RETURN
                    ENDIF
                ELSE
                    IF (haut.EQ.1) THEN
                        x_sortie(1)=x(1)
                        y_sortie(1)=y(1)
                        x_sortie(2)=x(2)
                        y_sortie(2)=y(2)
                        x_sortie(3)=xint
                        y_sortie(3)=yint
                        info=3
                    ELSE
                        x_sortie(1)=xint
                        y_sortie(1)=yint
                        x_sortie(2)=x(4)
                        y_sortie(2)=y(4)
                        info=2  
                    ENDIF
                ENDIF
            ENDIF
        ENDIF
        
        IF (intersecte(x(1),y(1),x(3),y(3),latitude,xint,yint).NE.0)THEN
            IF (intersecte(x(1),y(1),x(3),y(3),latitude,xint,yint).EQ.2) THEN
                
                IF (haut.EQ.1) THEN
                    info=0
                    RETURN
                ELSE
                    x_sortie(1)=x(1)
                    y_sortie(1)=y(1)
                    x_sortie(2)=x(2)
                    y_sortie(2)=y(2)
                    x_sortie(3)=x(4)
                    y_sortie(3)=y(4)
                    x_sortie(4)=x(3)
                    y_sortie(4)=y(3)
                    info=4
                    RETURN
                ENDIF
            ELSE
                IF (haut.EQ.1) THEN
                    info=info+1
                    x_sortie(info)=xint
                    y_sortie(info)=yint
                ELSE
                    x_sortie(info+1)=x(3)
                    y_sortie(info+1)=y(3)
                    x_sortie(info+2)=xint
                    y_sortie(info+2)=yint
                    info=info+2
                ENDIF
            ENDIF
        ELSE 
            IF (intersecte(x(3),y(3),x(4),y(4),latitude,xint,yint).NE.0) THEN
                IF (intersecte(x(3),y(3),x(4),y(4),latitude,xint,yint).EQ.2) THEN
                    IF (haut.EQ.1) THEN
                        x_sortie(1)=x(1)
                        y_sortie(1)=y(1)
                        x_sortie(2)=x(2)
                        y_sortie(2)=y(2)
                        x_sortie(3)=x(4)
                        y_sortie(3)=y(4)
                        x_sortie(4)=x(3)
                        y_sortie(4)=y(3)
                        info=4
                        RETURN
                    ELSE
                        info=0
                        RETURN
                    ENDIF
                ELSE
                    IF (haut.EQ.1) THEN
                        x_sortie(info+1)=x(3)
                        y_sortie(info+1)=y(3)
                        x_sortie(info+2)=xint
                        y_sortie(info+2)=yint
                        info=info+2
                    ELSE
                        info=info+1
                        x_sortie(info)=xint
                        y_sortie(info)=yint
                    ENDIF
                ENDIF
            ENDIF
        ENDIF
    ELSE
!-----------------point M1 M2 du meme cote----------------------
        IF (y(3).GT.y(2)) THEN 
            CALL echange(2,3,x,y)
        ENDIF
        IF (intersecte(x(1),y(1),x(2),y(2),latitude,xint,yint).NE.0) THEN
            IF (intersecte(x(1),y(1),x(2),y(2),latitude,xint,yint).EQ.2) THEN
                IF (haut.EQ.1) THEN
                    info=0
                    RETURN
                ELSE
                    x_sortie(1)=x(1)
                    y_sortie(1)=y(1)
                    x_sortie(2)=x(2)
                    y_sortie(2)=y(2)
                    x_sortie(3)=x(3)
                    y_sortie(3)=y(3)
                    x_sortie(4)=x(4)
                    y_sortie(4)=y(4)
                    info=4
                    RETURN
                ENDIF
            ELSE 
                IF (haut.EQ.1) THEN
                    x_sortie(1)=xint
                    y_sortie(1)=yint
                    x_sortie(2)=x(1)
                    y_sortie(2)=y(1)
                    info=3
                ELSE
                    x_sortie(1)=xint
                    y_sortie(1)=yint
                    x_sortie(2)=x(2)
                    y_sortie(2)=y(2)
                    x_sortie(3)=x(3)
                    y_sortie(3)=y(3)
                    x_sortie(4)=x(4)
                    y_sortie(4)=y(4)
                    info=5
                ENDIF
            ENDIF
        ENDIF
        IF (intersecte(x(2),y(2),x(3),y(3),latitude,xint,yint).NE.0) THEN
            IF (intersecte(x(2),y(2),x(3),y(3),latitude,xint,yint).EQ.2) THEN
                IF (haut.EQ.1) THEN
                    x_sortie(1)=x(1)
                    y_sortie(1)=y(1)
                    x_sortie(2)=x(2)
                    y_sortie(2)=y(2)
                    info=3
                ELSE
                    x_sortie(1)=x(4)
                    y_sortie(1)=y(4)
                    x_sortie(2)=x(3)
                    y_sortie(2)=y(3)
                    info=3
                ENDIF
            ELSE 
                IF (haut.EQ.1) THEN
                    x_sortie(1)=xint
                    y_sortie(1)=yint
                    x_sortie(2)=x(2)
                    y_sortie(2)=y(2)
                    x_sortie(3)=x(1)
                    y_sortie(3)=y(1)
                    info=4
                ELSE
                    x_sortie(1)=xint
                    y_sortie(1)=yint
                    x_sortie(2)=x(3)
                    y_sortie(2)=y(3)
                    x_sortie(3)=x(4)
                    y_sortie(3)=y(4)
                    info=4
                ENDIF
            ENDIF
        ENDIF
        IF (intersecte(x(3),y(3),x(4),y(4),latitude,xint,yint).NE.0) THEN
            IF (intersecte(x(3),y(3),x(4),y(4),latitude,xint,yint).EQ.2) THEN
                IF (haut.EQ.1) THEN
                    x_sortie(1)=x(1)
                    y_sortie(1)=y(1)
                    x_sortie(2)=x(2)
                    y_sortie(2)=y(2)
                    x_sortie(3)=x(3)
                    y_sortie(3)=y(3)
                    x_sortie(4)=x(4)
                    y_sortie(4)=y(4)
                    info=4
                    RETURN
                ELSE
                    info=0
                    RETURN
                ENDIF
            ELSE
                IF (haut.EQ.1) THEN
                    x_sortie(1)=xint
                    y_sortie(1)=yint
                    x_sortie(2)=x(3)
                    y_sortie(2)=y(3)
                    x_sortie(3)=x(2)
                    y_sortie(3)=y(2)
                    x_sortie(4)=x(1)
                    y_sortie(4)=y(1)
                    info=5
                ELSE
                    x_sortie(1)=xint
                    y_sortie(1)=yint
                    x_sortie(2)=x(4)
                    y_sortie(2)=y(4)
                    info=3
                ENDIF
            ENDIF
        ENDIF
        IF (intersecte(x(1),y(1),x(4),y(4),latitude,xint,yint).NE.0) THEN
            x_sortie(info)=xint
            y_sortie(info)=yint
        ENDIF
    ENDIF
    RETURN
  END SUBROUTINE cal_int
END MODULE mpr_cal_int











