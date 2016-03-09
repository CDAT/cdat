          program pcmdi
              implicit none
              include "lats.inc"
              integer nlon,nlat,nmon
              parameter (nlon=144,nlat=72,nmon=456)
              real adata(nlon,nlat,nmon)
              real w(nlon,nlat,nmon), alon(nlon), alat(nlat)
              integer lons,lats,mons,i,j

              integer latsconv,latscal,latsfreq,latsdelta
              integer id_fil,iyr0,imo0,ida0,ihr0,id_grd,id_var
              integer ierr,imo,iyr,icounter
              character*20 center,model,var
              data center /'pcmdi'/
              data model /'lats'/
              data var /'tas'/
              integer latscreate,latsgrid,latsvar,latswrite,latsclose
              integer latsparmtab,id_parmtab
c  Read in DRS data using EzGet
              call initget
              call defmisc('input missing value','real',1.e20)
              call defvar(1,'tf2',
     &      'tf2_RSS_v08jan16.20c3m_run1_mm_xy_fw_r0000_0000.dic')
              call defdim(1,1,'Longitude','width','nearest',
     &               -180.,180.,360.)
              call defdim(1,2,'Latitude','cosine','range',-90.,90.,0.)
              call defdim(1,3,'Time','unit','nearest',2148.,2603.,0.)
              lons = 0
              lats = 0
              mons = 0
              i = 0
              j = 0
              call getdata(1,nlon,nlat,nmon,j,lons,lats,mons,i,w,adata)
              call getcoord(1,1,alon)
              call getcoord(1,2,alat)
              call closeget
c Now writes out the data using LATS
              id_parmtab=
     $   latsparmtab(
     $   "/lgm/uvcdat/2016-03-08/Externals/lib/lats/amip2.parms")

              latsconv=LATS_PCMDI
              latscal=LATS_STANDARD
              latsfreq=LATS_MONTHLY
              latsdelta=1
              id_fil = latscreate('latsout',
     $       latsconv,
     $       latscal,
     $       latsfreq,latsdelta,center,
     $       model,'LATS netcdf test')
             iyr0 = 2016
             imo0=1
             ida0=1
             ihr0=12
             call latsbasetime(id_fil,iyr0,imo0,ida0,ihr0)
              print*, alat
              print*, alon
              print *,nlon,nlat
             id_grd=latsgrid("t42", LATS_GENERIC, nlon, alon, nlat, 
     $       alat)
             id_var=latsvar(id_fil,var,LATS_FLOAT,LATS_AVERAGE,id_grd,
     $     0, 'sfc variable')
             icounter = 0
             iyr = iyr0
             do imo=1,nmon
               ierr=latswrite(id_fil,id_var,0,iyr,imo,ida0,ihr0,
     $                 adata(1,1,imo))
               icounter = icounter+1
               if (icounter.eq.12) then
                   icounter = 0
                   iyr = iyr+1
               endif
           enddo
           ierr = latsclose(id_fil)
          end

