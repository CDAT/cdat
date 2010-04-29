      subroutine read1f(filename, maxsta, nvarbs, nlevels,
     &                  lon, lat, data, nr)
c
c  Read one file of Oort radiosonde data.  Each file contains data for all stations 
c  for one month.  The number of stations changes from month to month.
c
c  This program is a modified version of the test-read code "read.f" (GFDL program 
c  reado) provided by NCAR.
c
c  Input variables:
c
      character*(*) filename	! name of the file to be read
      integer maxsta 		! max number of stations (soundings) possible
      integer nvarbs, nlevels   ! number of variables and P-levels in each sounding
c
c  Output variables:
c
      real lon(maxsta), lat(maxsta)	  ! longitudes / latitudes of the stations
      real data(nvarbs, nlevels, maxsta)  ! sounding data
      integer nr			  ! actual number of stations with data
c
c
c                          *** Documentation ***
c
c  Oort radiosonde data (NCAR Data Support Section dataset DS431.0)
c
c  From: jrl@GFDL.GOV (John Lanzante) via joseph@ncar.ucar.edu (Dennis Joseph)
c
c _______________________________________________________________________________
c  Datasets:             Monthly Rawinsonde Station Covariances - 11 terms
c  _______________________________________________________________________________
c  Variables:            Su Sv ST Sz Sq Srh Nu NT Nz Nq Nrh
c                        The variables are u, v, T, z, q, and rh.
c                        S = sum and N = number of observations.
c                        NOTE:  Nv is not needed since Nv=Nu.
c                        NOTE:  z is given as the geopotential height departure
c                        (z - zREF) from NMC standard atmosphere (zREF) in gpm.
c  _______________________________________________________________________________
c  Time Period:          May 1958 to December1989
c  _______________________________________________________________________________
c  File Structure:       Files named yyyy.mm.ttz where yyyy=four digit year,
c                        mm=two digit month (leading zeros) and tt=time (either
c                        00,06,12 or 18 GMT); z indicates GMT.
c  _______________________________________________________________________________
c  Units:                m/s m/s C m g/kg % num num num num num
c  _______________________________________________________________________________
c  
c  Original Source:      GFDL, Bram Oort (retired)
c  _______________________________________________________________________________
c  Bibliography:
c         Oort, A., 1983:  Global Atmospheric Circulation Statistics, 1958-1973,
c         NOAA Professional Paper 14, U.S. Department of Commerce [data set
c         has been updated since this publication].
c  _______________________________________________________________________________
c  GFDL Contact:         John Lanzante or Gabriel Lau
c  _______________________________________________________________________________
c
      dimension s(11, 27)
      character*5 wmo
      character*8 clon
      open(10, file = filename, form = "unformatted")
      read(10) wmo,flat,flon,clon,elev,ihr,imo,iyr,s,ist,kst
c where wmo=wmo number (generally numbers, but a few weather ships
c containing letters. Please note that there may be more than one name
c for a ship, e.g., for Ship P: "P", "4YP" or "C7P").
c flat,flon=latitude, longitude of station using the conventional
c notation flat > 0 for NH; flat < 0 for SH; flon > 0 for eastern
c hemisphere; flon < 0 for western hemisphere.
c clon = "E" or "W" to make longitude extra clear.
c elev = elevation in m (often missing or unreliable).
c ihr = hour 00, 06, 12 or 18 GMT.
c imo = month 1, 2, 3,...12 for Jan, Feb, March,...December.
c iyr = year 1958...1989.
c sum(11,27) = array with data.
c     i = 1,2...11 sums and numbers of observations for the month.
c     j = 1,2,...27; levels: surface, 1000 mb,...7mb (generally no
c     data above 10 mb).
c     When there is no data for a certain term, the sum and N are both
c     equal to zero.
c     ist = 1,2...nst, where nst is the total number of stations for
c     a particular month.
c     kst = cross reference number to dictionary list (do not use, may
c     not be reliable since dictionary list has been updated).
c ieee assign statement for Cray systems
c assign -F f77.nonvax::393216 -N ieee fort.1
C
C    -Loop over number of data records (= number of stations):
      nr=0
   2  read(10,end=90) wmo,flat,flon,clon,elev,ihr,imo,iyr,s,ist,kst
      nr=nr+1
c*cc      write(*,*) wmo,flat,flon,clon,elev,ihr,imo,iyr,ist,kst
c*cc      do 10 j=1,27
c*cc      write(*,*) "Level = ",j
c*cc      write(*,7) (i,S(i,j),i=1,11)
c*cc   7  format(5(i3,e13.5))
c*cc  10  continue
c*cc      write(*,*) " "
      if (nr .gt. maxsta) then
         write(*, *) "read1f: Number of soundings exceeds maxsta."
         write(*, *) "** nr = ", nr, "  maxsta = ", maxsta
         write(*, *) "** Stopping execution."
         stop
      endif
      lon(nr) = flon
      lat(nr) = flat
      do j = 1, nlevels
         do i = 1, nvarbs
            data(i, j, nr) = s(i, j)
         enddo
      enddo
      go to 2
  90  continue
      write(*, "(a21, a, i8)") 
     &     filename, ": number of soundings read = ", nr
      close(10)
      return 
      end
