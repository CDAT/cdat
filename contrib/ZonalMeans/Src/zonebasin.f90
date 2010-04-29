!_ ---------------------------------------------------------------------
!_ RCS lines preceded by "c_ "
!_ ---------------------------------------------------------------------
!_
!_ $Source: /home/geocmip/users/orr/gap/fortran/zm/RCS/zonebasin.f90,v $
!_ $Revision: 1.3 $
!_ $Date: 2000/12/07 13:14:14 $   ;  $State: Exp $
!_ $Author: orr $ ;  $Locker: orr $
!_
!_ ---------------------------------------------------------------------
!_ $Log: zonebasin.f90,v $
!_ Revision 2.0  2003/10/20  13:14:14  epitalon
!_ Striped out all I/O operations. Made callable from Python program
!
!_ Revision 1.5  2005/07/05 09:15:47  orr
!_ Added casenomask option
!_
!_ Revision 1.4  2005/03/03 16:32:15  orr
!_ Added multiple zrite statements
!_
!_ Revision 1.3  2000/12/07  13:14:14  orr
!_ Protected from 'divide-by-zero' error (area=0) in ratio_vol calc.
!_
!_ Revision 1.2  2000/12/05  13:19:38  orr
!_ Changed "basindir" to input variable  (-d basindir on command line
!_ overides default basindir (see o_zm.f90)
!_
!_ Revision 1.1  2000/06/29  06:29:45  orr
!_ Initial revision
!_
!_ Revision 1.1  2000/06/27  09:31:49  orr
!_ Initial revision
!_
!_ ---------------------------------------------------------------------
!_

    SUBROUTINE zonebasin(var, imt, jmt, kmt, nt, kmt_grid               &
      &                       , iomax                                   &
      &                       , bounds_lon, bounds_lat, bounds_depth                &
      &                       , mask, basin_masks                       &
      &                       , area, vol_length, volume                &
      &                       , bandlat, nb                             &
      &                       , areaband, zonxbar, zoninv, zonmask )
!!$      &                       , fazname                                 &
!!$      &                       , casenomask                              &

!**********************************************************************
!
! PURPOSE
! -------
! Calculates the zonal mean (zonxbar) of the array var over
! latitude band n (contained between bandlat(n) and bandlat(n+1)).
! Calculate "global" zonal means and Basin global means
! (Pacific, Atlantic, Indian, Antartic, Marginal seas)
!
! INPUT
! -----
!                 * var         : 3-D spatial array from which zonal mean
!                                 is calculated (N.B. use kmt=1 for 2-D)
!
!                 * imt,jmt,kmt : dimensions of var (longitude, latitude and
!                                 depth indices)
!                 * nt          : dimension of time (e.g., nt=1 for annual
!                                 mean or month; nt=12 for 12 months)
!                 * bounds_lon  : longitudes at the 4 corners of each grid box
!                 * bounds_lat  : latitudes  at the 4 corners of each grid box
!
!                (4)---------(3)
!                 |           |    latitude  : ylat(i,j,4)
!                 |           |    longitude : xlon(i,j,4)
!                 |           |
!                 |           |
!                (1)---------(2)
!
!                 * bounds_depth : vertical bounds in meters of grid layers
!                 * mask          [ra] : mask, where 1=ocean and 0=land
!                 * basin_masks   [ia] : 9 masks : 5 basins and 4 combined basins
!                 * area(imt,jmt) [ra] : area of the grid box
!                 * vol_length    [i]  : 0 if volume data unavailable
!                 * volume        [ra] : 3D array, volume of grid cells
!                 * nb            [i]  : number of latitude bands
!                 * bandlat       [ra] : latitudes of the sequence of lines
!                                        that defines the lat. bands
!                                        (nb+1 members)
!                 * fazname       [c]  : filename where fract. area info is stored
!
!                 * casenomask    [i]  : if .true. don't call mask4d
! OUTPUT
! ------
!                * areaband       [ra] : area (m^2) of zonal band
!                * zonxbar        [ra] : zonal mean of var(imt,jmt,kmt)
!                * zoninv         [ra] : zonal cumulative inventory
!                                        of var(imt,jmt,kmt)
!                * zonmask        [ra] : zonal mask
!                                        (0 if no ocean area in band;
!                                         otherwise 1)
!
!----------------------------------------------------------------------
!       J. Orr, LSCE, 27 March 2000
!**********************************************************************

      USE mpr_cfraction
      USE mpr_mask4d

      IMPLICIT NONE

      REAL, parameter :: missing_value = -1.E+34

!     Input arguments:
      INTEGER, INTENT(in) :: imt, jmt, kmt, nt, kmt_grid, iomax
      REAL, DIMENSION (imt,jmt,kmt,nt), INTENT(inout)  :: var
!     REAL, DIMENSION (imt,jmt), INTENT(in)        :: lon, lat, area
      REAL, DIMENSION (4,imt,jmt), INTENT(in)    :: bounds_lon, bounds_lat
      REAL, DIMENSION (kmt,2)                      :: bounds_depth
      REAL, DIMENSION (imt,jmt), INTENT(in)        :: area
      INTEGER, INTENT(in)                          :: vol_length
      REAL, DIMENSION (imt,jmt,vol_length), INTENT(in)   :: volume
      REAL, DIMENSION (imt,jmt,kmt_grid), INTENT(in)     :: mask
      REAL, DIMENSION (imt,jmt,iomax), INTENT(in)  :: basin_masks

      INTEGER, INTENT(in) :: nb
      REAL, DIMENSION(nb+1), INTENT(in)  :: bandlat

!!$      CHARACTER*(80) ::  fazname
!!$      INTEGER, INTENT(in) ::  casenomask

!     Output arguments:
      REAL, DIMENSION(nb,kmt,iomax), INTENT(out) :: areaband, zonmask
      REAL, DIMENSION(nb,kmt,nt,iomax), INTENT(out) :: zonxbar, zoninv

!     Local definitions
      REAL, DIMENSION (imt,jmt)     :: avol
      REAL, DIMENSION (imt,jmt,kmt) :: ratio_vol
      REAL, DIMENSION (imt,jmt,4)   :: xlon, ylat
      REAL, DIMENSION (imt,jmt,nb)  :: fareaz

      INTEGER :: i, j, k, l, n, io
      INTEGER :: mismatch


!     ------------------------------------------------------------------
!     Make the array (for zonmean) that defines the corners
!     of each grid box. Points move counter clockwise, starting from
!     lower, left INDEX (1, 1) to upper left (1,2)
!                (4)---------(3)
!                 |           |    latitude  : ylat(i,j,4)
!                 |           |    longitude : xlon(i,j,4)
!                 |           |
!                 |           |
!                (1)---------(2)
!
!     ------------------------------------------------------------------
!     Four corners:
!     -------------
      xlon(:,:,1) = bounds_lon(1,:,:)
      xlon(:,:,2) = bounds_lon(2,:,:)
      xlon(:,:,3) = bounds_lon(3,:,:)
      xlon(:,:,4) = bounds_lon(4,:,:)

      ylat(:,:,1) = bounds_lat(1,:,:)
      ylat(:,:,2) = bounds_lat(2,:,:)
      ylat(:,:,3) = bounds_lat(3,:,:)
      ylat(:,:,4) = bounds_lat(4,:,:)

!!$      do i =1,2
!!$         print*, xlon(i,1,:),'*********************************************************8'
!!$         print*, ylat(i,1,:),'------------------------------------------------------------'
!!$      enddo
!
!     Center of each grid box:
!     ------------------------
!     xlon(:,:,5)=lon(:,:)
!     ylat(:,:,5)=lat(:,:)

!     ------------------------------------------------------------------
!     Open file with band areas for each grid box.
!     If it does not already exist, create array fareaz with a call to
!     cfraction; then write out that result.
!     --
!     That is, this computation is CPU intensive!  So to avoid needless
!     repetiton, build filename in which fractional area info
!     (for each non-zero i,j-box for each band) is stored (if it
!     was never previously created). Conversely, if file was
!     previously created, read in appropriate results
!     ---------------------------------------------------------------

!!$      write(*,'(a)')'ZONEBASIN: fazname = ', fazname
!!$      open(unit=55,file=fazname,form='unformatted',status='old',err=90)

!     ------------------------------------------------------------------
!     Read in fractional area of each grid box in each band
!     (Note: to save space this is done using individual writes for each
!            point; this way one avoids many superflous writes--hopefully
!            this approach is much shorter than the very long time
!            required each time)
!     ------------------------------------------------------------------
!     Initialize all values to zero:
      fareaz(:,:,:) = 0.0e+0

!     Read in nonzero values (those written with frac > 1e-06, see below):
!!$      nrw = 0
!!$      numrec_max = imt*jmt*nb
!!$      DO icnt = 1,numrec_max
!!$        READ(55,END=91) i, j, n, fareaz(i,j,n)
!!$        nrw = nrw + 1
!!$      END DO
!!$
!!$!     Should not arrive here
!!$      GO TO 91
!!$      print *, '>>>> PROBLEM -> Logic ERROR when reading :', trim(fazname)
!!$!     If problem, need to uncomment "go to" statement just above
!!$
!!$!     ------------------------------------------------------------------
!!$!     IF above FILE does not already exist
!!$!     Compute fractional area of each grid box within each band
!!$!     (this is time consuming; thus needless repetition is avoided)
!!$!     ------------------------------------------------------------------
!!$90    print *,'ZONEBASIN: Compute fractional areas (Calling cfraction subroutine):'
!!$      PRINT *,'ZONEBASIN: imt, jmt, nb =', imt, jmt, nb
!!$      WRITE (*,*) 'ZONEBASIN: xlon : ',xlon(25,:,1)
!!$      WRITE (*,*) 'ZONEBASIN: ylat : ',ylat(25,:,1)
!!$      WRITE (*,*) 'ZONEBASIN: bandlat: ', (bandlat(n), n=1,nb)

      CALL cfraction(imt,jmt,nb,xlon,ylat,bandlat,fareaz)

!!$      fazname='faz.txt'
!!$      PRINT *,'ZONEBASIN:  Store fractional areas in file: ', TRIM(fazname)
!!$      open(unit=55,file=fazname,form='formatted',status='new')
!!$      nrw = 0
!!$wf:   DO n=1,nb
!!$        DO j=1,jmt
!!$          DO i=1,imt
!!$            IF (fareaz(i,j,n) > 1.0e-6)THEN
!!$                WRITE(55,*)i,j,n,fareaz(i,j,n)
!!$                nrw = nrw + 1
!!$            ENDIF
!!$          END DO
!!$        END DO
!!$      END DO wf
!!$
91    CONTINUE
!!$      PRINT *,'ZONEBASIN: number of records in ',trim(fazname),' = ', nrw
!!$      close(55)

!     ------------------------------------------------------------------
!     If it exists, read volume and calculate real/ideal volume ratio:
!     Compute ratio of real volume vs. volume of depth x area
!     the two should be equal except for partially masked grid cells
!     (AWI, MIT, and MPIM) models
!     Attempt to read volume from special volume file
!     ------------------------------------------------------------------
      IF ((kmt.NE.1) .AND. (vol_length.NE.0)) THEN

          WHERE (mask(:,:,1) == 0.0)
              avol = 1.
          ELSEWHERE
              avol = area
          END WHERE

    rvol: DO k=1,kmt
            DO j=1,jmt
              DO i=1,imt
                    ratio_vol(i,j,k) = volume(i,j,k) /   &
                   &       (avol(i,j) * (bounds_depth(k,2) - bounds_depth(k,1)) )
              END DO
            END DO
          END DO rvol

          WHERE (mask == 0.0) ratio_vol = 0.

      ELSE
!         If no level dimension or volume file does not exist, the ratio is 1
!         -------------------------------------------------------------------
          ratio_vol(:,:,:) = 1.
      ENDIF

!     ------------------------------------------------------------------
!     Initialize arrays used to accumulate band areas, and zonal tracer conc.
!     ------------------------------------------------------------------
      zonxbar(:,:,:,:)  = 0.  ;  zoninv(:,:,:,:)  = 0.  ;  areaband(:,:,:) = 0.

!     ------------------------------------------------------------------
!!$      print *,' Determine zonal means (areas and tracer conc''s)'
!     ------------------------------------------------------------------
karea: DO k=1,kmt
!!$        PRINT *,'ZONEBASIN: Depth Level = ',k, ' of', kmt
!       Keep track of each basin)
!       Sum area in each band in each basin (for each depth)
!       ----------------------------------------------------------------
!!$        PRINT*,'ZONEBASIN: Calculate Zonal areas'
  ar:   DO io=1,iomax
          DO n=1,nb
            DO j=1,jmt
              DO i=1,imt
                areaband(n,k,io) = areaband(n,k,io)                           &
                   &                               + area(i,j)                &
                   &                               * fareaz(i,j,n)            &
                   &                               * mask(i,j,k)              &
                   &                               * basin_masks(i,j,io)      &
                   &                               * ratio_vol(i,j,k)
              END DO
            END DO
          END DO
        END DO ar
      END DO karea

!       Mask all missing values as zero (to avoid problems with summing below)
!       Problem noticed when passing to j=2 in do loop below
!       Fixed with "mask4d" approach now below
!!$        PRINT *, "ZONEBASIN: Mask var"
!!$        print *, "ZONEBASIN: imt, jmt, kmt, nt =", imt, jmt, kmt, nt
!!$        IF ( casenomask.ne.1) THEN
          call mask4d(var,mask,imt,jmt,kmt,nt,0.0)
!!$          PRINT *, "ZONEBASIN: Mask var (completed)"
!!$        ENDIF

!       Sum tracer in each band in each ocean (for each depth)
!       to get integrated quantity [units of mass or mass/time]
!       ----------------------------------------------------------------
!!$        PRINT*,'ZONEBASIN: Calculate Zonal Integrals'
!!$        PRINT *, 'nt, kmt, nb, jmt, imt =', &
!!$                  nt, kmt, nb, jmt, imt

        zoninv = 0.
        mismatch = 0
 binv:  DO io=1,iomax
  cumul:  DO l=1,nt
      kinv: DO k=1,kmt
              DO n=1,nb
                DO j=1,jmt
                  DO i=1,imt

!                 Following IF block needed for IPSL.DM1
!                 (for DICi in IPSL.DM1_Abiotic_hist_1989_2D.nc)
                    IF (var(i,j,k,l).EQ.missing_value .AND. &
                       &mask(i,j,k) .EQ. 1) THEN
                        var(i,j,k,l) = 0.0
                        mismatch = mismatch +1
                    END IF

                    zoninv(n,k,l,io) = zoninv(n,k,l,io)                       &
                       &                         + var(i,j,k,l)               &
                       &                           * area(i,j)                &
                       &                           * fareaz(i,j,n)            &
                       &                           * mask(i,j,k)              &
                       &                           * basin_masks(i,j,io)      &
                       &                           * ratio_vol(i,j,k)
                  END DO
                END DO
              END DO
            END DO kinv
          END DO cumul
        END DO binv

      IF (mismatch > 0) &
         & PRINT *,'ZONEBASIN: >> WARNING -> Mask vs. variable mismatches = '&
         &        , mismatch

!     Compute zonal mean mask
!     -----------------------
!!$      PRINT*,'ZONEBASIN: Build Zonal Mask'
      WHERE (areaband == 0.0)
          zonmask = 0.
      ELSEWHERE
          zonmask = 1.
      END WHERE

!     Divide Sum by Area to get at zonal means per band for each level k
!     ------------------------------------------------------------------
!!$      PRINT*,'ZONEBASIN: Calculate Zonal Mean'
bbar: DO io=1,iomax
  xbar: DO l=1,nt
          DO k=1,kmt
            DO n=1,nb
              IF (zonmask(n,k,io) .EQ. 1.) THEN
                  zonxbar(n,k,l,io) = zoninv(n,k,l,io) / areaband (n,k,io)
              ELSE
                  zonxbar(n,k,l,io) = missing_value
                  zoninv (n,k,l,io) = missing_value
              END IF
            END DO
          END DO
        END DO xbar
      END DO bbar

      RETURN
    END SUBROUTINE zonebasin


