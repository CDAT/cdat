c     Test cddrs/FORTRAN/GRIB

#include "drsdef.h"
#include "fcddrs.h"
      integer T21_NLON, T21_NLAT
      parameter (T21_NLON = 64)
      parameter (T21_NLAT = 32)

      dimension t21_lats(T21_NLAT)
      data t21_lats /
     $     0.85760587120445E+02, 0.80268779072251E+02,
     $     0.74744540368636E+02, 0.69212976169371E+02,
     $     0.63678635561097E+02, 0.58142954049203E+02,
     $     0.52606526034345E+02, 0.47069642059688E+02,
     $     0.41532461246656E+02, 0.35995078411272E+02,
     $     0.30457553961152E+02, 0.24919928629948E+02,
     $     0.19382231346434E+02, 0.13844483734385E+02,
     $     0.83067028565188E+01, 0.27689030077360E+01,
     $     -0.27689030077360E+01,-0.83067028565188E+01,
     $     -0.13844483734385E+02,-0.19382231346434E+02,
     $     -0.24919928629948E+02,-0.30457553961152E+02,
     $     -0.35995078411272E+02,-0.41532461246656E+02,
     $     -0.47069642059688E+02,-0.52606526034345E+02,
     $     -0.58142954049203E+02,-0.63678635561097E+02,
     $     -0.69212976169371E+02,-0.74744540368636E+02,
     $     -0.80268779072251E+02,-0.85760587120445E+02/
      real prc(2,5), prc_copy(5,2,4)
      data prc /0.8563904,  0.7936714,
     $     0.7409869,  0.4776548,
     $     0.7068663,  0.4638551,
     $     0.277178,  0.1619728,
     $     1.560012,   1.812692/
      dimension t21_lats_copy(T21_NLAT)
      character*120 source
      character*16 name
      character*80 title
      character*40 units
      character*8 time, date, etype
      integer cw_aslun, cw_cluvdb, cw_setname, cw_inqdict,
     $     cw_getcdim, cw_cllun, cw_getdat, cw_getname,
     $     cw_setdim

      ierr = cw_aslun(7,'../../cdunif/test/testgrib.ctl',
     $     8,' ',IDRS_READ)

c     Get the latitudes, compare

      ierr = cw_cluvdb()
      ierr = cw_setname(' ','prc',' ',' ',' ')
      ierr = cw_inqdict(7,IDRS_GETFIRSTVAR)
      ierr = cw_getcdim(2,source,name,title,units,itype,
     $     T21_NLAT,t21_lats_copy,iactlen)
      do 10 i=1,T21_NLAT
         if(abs(t21_lats(T21_NLAT-i+1)-t21_lats_copy(i))
     $        .gt.1.e-3) then
            print *,'t21_lats_copy(',i,')=',t21_lats_copy(i),
     $           ', should be ',t21_lats(T21_NLAT-i+1)
            call exit(1)
         endif
 10   continue
      
c     Get a hyperslab, compare

      ierr = cw_cluvdb()
      ierr = cw_setname(' ','prc',' ',' ',' ')
      ierr = cw_setdim(1,'latitude',' ',5,60.0,90.0)
      ierr = cw_setdim(2,'longitude',' ',2,0.0,6.0)
      ierr = cw_setdim(3,'time',' ',4,-1.0,19.0)
      ierr = cw_getdat(7,prc_copy,IDRS_BYTES_PER_WORD*40)
      ierr = cw_getname(source, name, title, units, date,
     $     time, etype, ndim)
      
      do 40 itime=1,4
         do 30 ilon=1,2
            do 20 ilat=1,5
               if(abs(prc(ilon,6-ilat)-prc_copy(ilat,ilon,itime))
     $              .gt.4.e-5) then
                  print *,'prc_copy(',ilat,',',ilon,',',itime,') is ',
     $                 prc_copy(ilat,ilon,itime),', should be ',
     $                 prc(ilon,6-ilat)
                  call exit(1)
               endif
 20         continue
 30      continue
 40   continue
      
      if(title.ne.'Total precipitation [mm/day]') then
         print *,'Comments = ',title,
     $        ', should be Total precipitation [mm/day]'
         call exit(1)
      endif
      
      ierr = cw_cllun(7)
      end
