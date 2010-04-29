!_ ---------------------------------------------------------------------
!_ RCS lines preceded by "c_ "
!_ ---------------------------------------------------------------------
!_
!_ $Source: /home/geocmip/users/orr/gap/fortran/zm/RCS/cfraction.f90,v $
!_ $Revision: 1.1 $
!_ $Date: 2000/06/27 09:31:49 $   ;  $State: Exp $
!_ $Author: orr $ ;  $Locker:  $
!_
!_ ---------------------------------------------------------------------
!_ $Log: cfraction.f90,v $
!_ Revision 1.1  2000/06/27  09:31:49  orr
!_ Initial revision
!_
!_ ---------------------------------------------------------------------
!_
MODULE mpr_cfraction
  CONTAINS
      SUBROUTINE cfraction(imt,jmt,nband,xlon,ylat,bandlat,fareaz)
!
!-----------------------------------------------------------------------
!     Ce programme effectue un decoupage d'une grille quelconque en
!     bandes de latitude
!-----------------------------------------------------------------------
!
!     Yannick Peysson , LMCE, CEA, 7 Juillet 1993
!
!     MODIFICATIONS:
!     -------------
!     8 October 2003 (Jean-Marie Epitalon)
!     Changed method to compute area of polygon from planar to spherical
!     Replaced calls to parea with calls to SphericalPolyArea
!
!     28 August 1993 (James Orr)
!     Modified commentary as well as how input variables enter this
!     routine, i.e., I moved xlon, ylat, and fareaz to common, in order
!     to allow the calling program to share memory and thus save space.
!     -------------
!     
!
!-----------------------------------------------------------------------
!
!     Input:
!            bandlat = array of nb+1 members, each member defines a line
!                      that together separate the nb latitudinal bands
!            nband      = number of latitudinal bands
!            --------------------
!            In common (zmeancom)
!            --------------------
!            xlon(i,j,4) = Longitudes of the 4 corners of each box
!            ylat (i,j,4) = Latitudes  of the 4 corners of each box
!
!     Note: number of bands specified (nb) cannot exceed the
!           parameter nbndmax.  Change nbndmax if desirable, but beware
!           that very large arrays may result, depending upon the
!           variables imt and jmt, and this must be taken into account
!           when
!
!
!     Output:
!            --------------------
!            In common (zmeancom)
!            --------------------
!            fareaz(i,j,n) gives the fractional area of box i,j in band n
!
!

      USE mpr_meme_cote
      USE mpr_intersecte
      USE mpr_SphericalPolyArea
      USE mpr_cal_int
      USE mpr_echange

      IMPLICIT none

!     INCLUDE 'zonmaster.inc'

      INTEGER, parameter :: nbndmax = 180

      INTEGER, INTENT(in) :: imt, jmt
      INTEGER, INTENT(in) :: nband
      REAL*4, DIMENSION (imt,jmt,4), INTENT(in) :: xlon, ylat
      REAL*4, DIMENSION(nband+1), INTENT(in) :: bandlat
!      REAL*4, DIMENSION(nbndmax), INTENT(in) :: bandlat

      REAL*4, DIMENSION (imt,jmt,nband), INTENT(out) :: fareaz

!     Local arrays
      INTEGER i, j, k, n, ic, icompt
      INTEGER iflag1, ita
      REAL*4, DIMENSION (imt,jmt) ::  rlatmin, rlatmax
      INTEGER, DIMENSION (imt,jmt) ::  ibndmin, ibndmax

      REAL, DIMENSION(6) :: x, y, xsup, ysup

!     Following two lines are defined as real*8 on asterix;
!     however that is the default on the cray
      real aire_p(10)
      real aire_t

!
!!      PRINT *,'CFRACTION: imt, jmt, nband = ', imt, jmt, nband
!
!-----Initialisation du tableau------------------
!
      fareaz(:,:,:) = 0.0

!
!-----Calcul du point de latitude max et min---------------------
!
!     Initialize max and min
      rlatmax(:,:) = -1000.
      rlatmin(:,:) = 1000.
!
!-----Calculation of max and min
!!$        PRINT *,'CFRACTION: Calculation of max and min'
!
      DO j=1,jmt
         DO i=1,imt
            DO ic=1,4
               IF (ylat(i,j,ic) .GE. rlatmax(i,j)) THEN
                   rlatmax(i,j)=ylat(i,j,ic)
               ENDIF
               IF (ylat(i,j,ic) .LE. rlatmin(i,j)) THEN
                   rlatmin(i,j)=ylat(i,j,ic)
               ENDIF
             END DO
         END DO
       END DO
!
!-----Cherche la bande de ibndmax et ibndmin--------------
!!$        PRINT *,'CFRACTION: Cherche la bande de ibndmax et ibndmin'
!
      DO j=1,jmt
!         PRINT *,'j =', j
         DO i=1,imt
            DO n=1,nband
!              Debugging IF block
!!$               IF (n.eq.nband .and. i.eq.1 .and. j.EQ.jmt) THEN
!!$                 PRINT *,&
!!$                 'n, rlatmin(i,j), rlatmax(i,j), bandlat(n), bandlat(n+1) = ',&
!!$                  n, rlatmin(i,j), rlatmax(i,j), bandlat(n), bandlat(n+1)
!!$               ENDIF

               IF (rlatmax(i,j).GT.bandlat(n) .AND.          &
&                  rlatmax(i,j).LE.bandlat(n+1))     THEN
                  ibndmax(i,j) = n
!                 IF (j.EQ.jmt) PRINT *,'n = ', n
              ENDIF
               IF (rlatmin(i,j).GE.bandlat(n) .AND.          &
&                  rlatmin(i,j).LT.bandlat(n+1))     THEN
                  ibndmin(i,j) = n
              ENDIF
            END DO
          END DO
        END DO
!!$         PRINT *,' n = ', (n, n=1,nband+1)
!!$         PRINT *,' bandlat(n) = ', (bandlat(n), n=1,nband+1)
!!$         PRINT *, 'ibndmin(:,jmt) :', ibndmin(:,jmt)
!!$         PRINT *, 'ibndmax(:,jmt) :', ibndmax(:,jmt)

!
!------calcul de l'aire de chaque boite dans sa bande----------
!!$        PRINT *,'CFRACTION: calcul de l''aire de chaque boite dans sa bande'
        DO j=1,jmt
!!$          PRINT *,'j = ', j
          DO i=1,imt
!!$            PRINT *,'i, j ibnd(min, max) = ', i, j, ibndmin(i,j), ibndmax(i,j)

            icompt = 1
            DO k=1,4
              x(k)=xlon(i,j,k)
              y(k)=ylat(i,j,k)
            END DO
            IF((x(1).EQ.x(2) .AND. x(2).EQ.x(3) .AND. x(3).EQ.x(4))          &
               &        .OR.                                                 &
               &        (y(1).EQ.y(2) .AND. y(2).EQ.y(3) .AND. y(3).EQ.y(4)) &
               &       )THEN
                fareaz(i,j,:) = 0.
            elseIF (ibndmin(i,j).EQ.ibndmax(i,j)) THEN
                aire_t=1.0
                fareaz(i,j,ibndmin(i,j))=aire_t
            ELSE

                DO n=ibndmin(i,j),ibndmax(i,j)

                  IF (n.EQ.ibndmin(i,j)) THEN
                      DO k=1,4
                        x(k)=xlon(i,j,k)
                        y(k)=ylat(i,j,k)
                      END DO
                      if((x(1).eq.x(2) .and. x(2).eq.x(3) .and. x(3).eq.x(4)) &
                &        .or.                                                 &
                &        (y(1).eq.y(2) .and. y(2).eq.y(3) .and. y(3).eq.y(4)) &
                &       )then
                          aire_p(icompt)=0

                       else
                           CALL cal_int(x,y,rlatmax(i,j),rlatmin(i,j),       &
                              &         bandlat(n+1),0,xsup,ysup,iflag1)
                           xsup(iflag1+1)=xsup(1)
                           ysup(iflag1+1)=ysup(1)
                           aire_p(icompt)=SphericalPolyArea(xsup,ysup,iflag1+1)

                       endif
                       icompt=icompt+1
                  ELSE
                      IF (n.EQ.ibndmax(i,j)) THEN
                          DO k=1,4
                            x(k)=xlon(i,j,k)
                            y(k)=ylat(i,j,k)
                          END DO
                          IF((x(1).EQ.x(2) .AND. x(2).EQ.x(3) .AND. x(3).EQ.x(4)) &
                &            .or.                                                 &
                &            (y(1).eq.y(2) .and. y(2).eq.y(3) .and. y(3).eq.y(4)) &
                &           )then
                              aire_p(icompt)=0
                              aire_t = 0
                          ELSE
                              x(5)=x(1)
                              y(5)=y(1)
                              aire_t = SphericalPolyArea(x,y,5)
                              aire_p(icompt) = aire_t
                          ENDIF
                      ELSE
                          DO k=1,4
                            x(k)=xlon(i,j,k)
                            y(k)=ylat(i,j,k)
                          END DO
                          IF((x(1).EQ.x(2) .AND. x(2).EQ.x(3) .AND. x(3).EQ.x(4)) &
                   &        .OR.                                                 &
                   &         (y(1).EQ.y(2) .AND. y(2).EQ.y(3) .AND. y(3).EQ.y(4)) &
                   &        )THEN
                              aire_p(icompt)=0
                          ELSE
                              CALL cal_int(x,y,rlatmax(i,j),rlatmin(i,j),  &
                                 bandlat(n+1),0,xsup,ysup,iflag1)
                              xsup(iflag1+1)=xsup(1)
                              ysup(iflag1+1)=ysup(1)
                              aire_p(icompt)=SphericalPolyArea(xsup,ysup,iflag1+1)
                          ENDIF
                          icompt=icompt+1
                      ENDIF
                  ENDIF
                END DO
                
                DO n=icompt,2,-1
                  aire_p(n)=aire_p(n)-aire_p(n-1)
                END DO

                ita=1
                DO n=ibndmin(i,j),ibndmax(i,j)
!                 IF (aire_t .EQ. 0) PRINT *, 'aire_t = 0'
!                 If block below added for PIUB;
!                 Other models only require ELSE clasue
                  IF (aire_t .EQ. 0) THEN
!!$                      PRINT *, 'aire_t = 0', '  i, j, n =',i,j,n, aire_p(ita+1)
                      fareaz(i,j,n) = 0.
                  ELSE
                      fareaz(i,j,n) = (aire_p(ita)/aire_t)
                  END IF
                  ita=ita+1
                END DO

            ENDIF

          END DO
        END DO
        RETURN
      END SUBROUTINE cfraction
END MODULE mpr_cfraction
