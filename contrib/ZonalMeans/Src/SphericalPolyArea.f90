!_ ---------------------------------------------------------------------
!_ RCS lines preceded by "c_ "
!_ ---------------------------------------------------------------------
!_
!_ $Source: /home/geocmip/users/orr/gap/fortran/zm/RCS/SphericalPolyArea.f90,v $
!_ $Revision: 1.1 $
!_ $Date: 2000/06/27 09:31:49 $   ;  $State: Exp $
!_ $Author: orr $ ;  $Locker:  $
!_
!_ ---------------------------------------------------------------------
!_ $Log: SphericalPolyArea.f90,v $
!_ Revision 1.1  2003/10/08  09:31:49  orr
!_ Initial revision
!_
!_ ---------------------------------------------------------------------
!_
MODULE mpr_Hav
CONTAINS
    DOUBLE PRECISION FUNCTION Hav(X)
!    Haversine function: hav(x)= (1-cos(x))/2.

        IMPLICIT NONE
        DOUBLE PRECISION :: X

        Hav = (1.0 -cos(X))/2.0
        RETURN
    END FUNCTION Hav
END MODULE mpr_Hav

MODULE mpr_SphericalPolyArea
CONTAINS
    REAL FUNCTION SphericalPolyArea (Lon, Lat, N)
!
!*****************************************************************
!
!   Given a sequence of N points (Lon(I),Lat(I)) in degrees,
! compute the area bounded bu the closed polygonal curve which
! passes through the points in the order that they are indexed.
! Result is in spherical deegrees.
! Area bounds are great circles on the sphere.
!
! The final point of the curve is assumed to be the first point
! given. Therefore, it need not be listed at the end of Lat and
! Lon.
!
!  from the article
!  "Computing the Area of a Spherical Polygon"
!  by Robert D. Miller
!  in "Graphics Gems IV", Academic Press, 1994
!
!  Converted from C to Fortran 90 by JM Epitalon (8 Oct 2003)
!
!*****************************************************************



!     Returns the area of a spherical polygon in spherical degrees,
!     given the latitudes and longitudes in Lat and Lon, respectively.
!     The N data points have indices which range from 1 to N.
!
        USE mpr_Hav

        IMPLICIT NONE

        INTEGER, INTENT(in) :: N
        REAL, DIMENSION (N), INTENT(in) :: Lat, Lon

        INTEGER :: J, K, NbPoints
        DOUBLE PRECISION :: Lam, Lam1, Lam2, Beta1, Beta2, &
        &                   CosB1, CosB2, HavA,            &
        &                   T, A, B, C, S, Sum, Excess
        DOUBLE PRECISION :: HalfPi, Degree
        REAL, DIMENSION (N) :: X, Y


! PRINT *, "SphericalPolyArea:"
! WRITE (*,*) 'xlon : ',Lon(:)
! WRITE (*,*) 'ylat : ',Lat(:)

        NbPoints = N
        IF (Lat(1) == Lat(n) .AND. Lon(1) == Lon(n)) NbPoints = NbPoints - 1

        HalfPi= 1.5707963267948966192313
!       degrees per radian
        Degree= 57.295779513082320876798

!       Convert from degrees to radians
        DO J = 1, NbPoints
            X (J) = Lon (J) / Degree
            Y (J) = Lat (J) / Degree
        END DO

        Sum= 0.
        DO J = 1, NbPoints

            IF (J == 1) THEN

                Lam1= X (J)
                Beta1= Y (J)
                Lam2= X (J+1)
                Beta2= Y (J+1)
                CosB1= cos(Beta1)
                CosB2= cos(Beta2)

            ELSE

                K = J + 1
                IF (K == NbPoints + 1) K = 1

                Lam1= Lam2
                Beta1= Beta2
                Lam2= X (K)
                Beta2= Y (K)
                CosB1= CosB2
                CosB2= cos(Beta2)
            ENDIF

! PRINT *, "J, Lam1, Lam2: ", J, Lam1, Lam2

            IF (Lam1 .NE. Lam2) THEN

                HavA= Hav(Beta2-Beta1) +CosB1*CosB2*Hav(Lam2-Lam1)
                A= 2*asin(sqrt(HavA))
                B= HalfPi -Beta2
                C= HalfPi -Beta1
                S= 0.5*(A+B+C)
                T= tan(S/2) * tan((S-A)/2) * tan((S-B)/2) * tan((S-C)/2)

                Excess= ABS(4*atan(sqrt(ABS(T))))*Degree
                IF (Lam2 > Lam1)  THEN
                    Lam = Lam2 - Lam1
                ELSE
                    Lam = Lam2 - Lam1 + 4*HalfPi
                ENDIF
                IF (Lam > 2*HalfPi) THEN
                    Excess= -Excess
                ENDIF
! PRINT *, "Excess: ", Excess

                Sum= Sum + Excess
            ENDIF
        END DO
        SphericalPolyArea = ABS(Sum)
        RETURN

    END FUNCTION SphericalPolyArea

END MODULE mpr_SphericalPolyArea
