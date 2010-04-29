c to build it
c pgf77 -c -C compall_lib.F
c rm libmycompall.a
c ar crv libmycompall.a compall_lib.o
c ranlib libmycompall.a
c pyfort part
c pyfort -e compall_2.pfp

c **********************************************************************
c ----------------------------------------------------------------------
c
      subroutine fillout(sumwt, suma, sumb, sumaa, sumbb, sumab, sumrms,
     +       wts, avga, avgb, vara, varb, correl, rms)
c
c ----------------------------------------------------------------------
c ********************************************************************** 

      implicit none
      double precision sumwt, suma, sumb, sumaa, sumbb, sumab, sumrms
      real wts, avga, avgb, vara, varb, correl, rms

      if (sumwt .gt. 0.) then
        wts = sumwt
        suma = suma/sumwt
        sumb = sumb/sumwt
        sumaa = sumaa/sumwt - suma*suma
        sumbb = sumbb/sumwt - sumb*sumb
        if (sumaa .lt. 0.) sumaa = 0.
        if (sumbb .lt. 0.) sumbb = 0.
        if ((sumaa .gt. 0.) .and. (sumbb .gt. 0.)) then
          correl = (sumab/sumwt - suma*sumb)/sqrt(sumaa*sumbb)
        else
          correl = 0.0
        endif
        rms = sumrms/sumwt - (suma-sumb)*(suma-sumb)
        if (rms .gt. 0.) then 
          rms = sqrt(rms)
        else
          rms = 0.0
        endif
 
        avga = suma
        avgb = sumb
        vara = sumaa
        varb = sumbb
      endif

      return
      end




c **********************************************************************
c ----------------------------------------------------------------------
c
      subroutine mkmask(icalndr, lenreg, lentim, itimpyr, mosea1, 
     &        minyr, fracmin, amask, bmask)
c
c ----------------------------------------------------------------------
c ********************************************************************** 
c
c   This subroutine generates an output mask given 2 input masks.
c   The output mask is returned in amask.  bmask is not altered
c    by this subroutine
c
c   If amask is used to compute a climatology of months (or seasons) 
c      (i.e., mean over all years), then each month or season will
c      be weighted by the number of days in the month (season).  For 
c      interannual variability, years with more data will be weighted
c      more heavily.
c
c   For no missing data, the sum of weights over all time and grid
c      points = the fraction of the globe occupied by the domain
c      More generally, sum of weights over all time and grid points = 
c         the avg. fraction of globe with data 
c
c   input:  amask(i,j,m,n) > 0. if test data exists at i,j,m,n
c                          i = longitude
c                          j = latitude
c                          m = month or season
c                          n = year
c
c           bmask(i,j,m,n) = weights of reference data cells (0.0 if
c                              data missing)
c
c   create output mask: 
c
c        1. if (amask(i,j,m,n) > 0.) then 
c                 set amask(i,j,m,n) = bmask(i,j,m,n)
c        
c        for each grid cell (i,j):
c          2. determine how many years of data are available
c          3. set na(m) = number of years for month (season) m where  
c             amask > 0 at a given grid cell
c          4. if mpy > 1, then
c               for a given grid cell, if for each of the 12 months (or
c               4 seasons), m:
c                  if ((na(m) .gt. minyr) .or. 
c                       (na(m) .ge. fracmin*nyrs))  then
c                     amask(m,n) = 
c                         ((amask(m,n)/mpy) / sum_over_n(amask(m,n)))*
c                         (sum_over_m,n(amask(m,n)/na(m))/mpy)*wttime(m) 
c                  else
c                     amask(all m, all n) = 0.0
c                  endif
c             endif 
c
c             where mpy = 12, 4, or 1, depending on whether the data are
c                          monthly, seasonal, or annual (or single
c                          months or seasons), and
c                 wttime(m) = for monthly data, 
c                             (12./365.)*(the number of days in month m)
c                           = for seasonal data, 
c                             (4./365.)*(the number of days in season m)
c                           = for individual months, seasons, or annual
c                                mean,
c                              1.0
c
c             else (mpy = 1) 
c               amask = amask/lentim
c             endif
c                             
c
c    Note:  The above implies that
c 
c        The sum of the weights over all years at grid cell i,j
c              is proportional to the number of days in a given month
c              (i.e., very nearly independent of month)
c
c             this means that climatological seasons and months can
c             be computed in a straight-forward way.


      implicit none
      integer lenreg, lentim, itimpyr, mosea1, icalndr, minyr
      real fracmin, amask(lenreg,lentim), bmask(lenreg,lentim)

c     local variables

      real wttime(12,3,2), fac
      double precision sn, smn
      integer mcnt, ncnt, j, m, n, iw, k, nyrs

      data wttime / 
     &      31., 28., 31., 30., 31., 30., 31., 31., 30., 31., 30., 31.,
     &      90., 92., 92., 91., 8*0.,
     &     365., 11*0.,
     &      12*30., 4*90., 8*0., 360., 11*0. /

c     copy bmask to amask and normalize by the number of times

      do n=1,lentim
        do j=1,lenreg
          if (amask(j,n) .gt. 0.0) amask(j,n) = bmask(j,n)/lentim
        enddo
      enddo

      if (itimpyr .eq. 1) return
        
      nyrs = lentim/itimpyr
      if (itimpyr .eq. 12) then
        iw = 1
      elseif (itimpyr .eq. 4) then
        iw = 2
      else
        iw = 3
      endif

      fac = float(lentim)/(wttime(1,3,icalndr)*float(itimpyr))

      do 100 j=1,lenreg
 
          mcnt = 0
          smn = 0.0

          do 80 m=1,itimpyr
 
c           check whether there is enough data at the grid cell to 
c              compute climatological monthly average

            ncnt = 0
            sn = 0.0

            do 70 n=m,lentim,itimpyr
              if (amask(j,n) .gt. 0.) then 
                ncnt = ncnt + 1
                sn = sn + amask(j,n)
              endif
   70       continue

            if ((ncnt .gt. minyr) .or. (ncnt .ge. nyrs*fracmin)) then

c               count for how many months of year we can compute
c                  a climatological average:
                mcnt = mcnt + 1

c               accumulate in order to compute an annual mean of the
c                  climatological monthly weights:
                smn = smn + sn/ncnt

c               compute fraction of climatological average contributed 
c                  by year n:
                do 75 n=m,lentim,itimpyr
                  amask(j,n) = amask(j,n)/sn
   75           continue

            endif
            
   80     continue

          if (mcnt .lt. itimpyr) then

c           if not all months have enough data to compute a 
c              climatological average, then mask this cell:

            do 85 n=1,lentim
                amask(j,n) = 0.0
   85       continue

          else

c           calculate climatological annual mean weight

            k = mosea1 - 1
            do 88 m=1,itimpyr

c             find out what calendar month it is
              k = k + 1
              if (k .gt. itimpyr) k = 1

              do 87 n=m,lentim,itimpyr 
                amask(j,n) = amask(j,n)*smn*wttime(k,iw,icalndr)*fac
   87         continue

   88       continue

          endif
            
  100 continue

      return
      end





c **********************************************************************
c ----------------------------------------------------------------------
c
      subroutine mkoutfil(template, modl, pthd, shrtname, longname, 
     &         filename)
c
c ----------------------------------------------------------------------
c **********************************************************************

      implicit none
      character*(*) template, modl, filename, pthd, shrtname, longname

c      local variables:

      integer i,j,k,m

      if (index(template, 'MOD') .eq. 0) then
        i = index(template, ' ') - 1
        IF (i.eq.-1) then
            filename=template
        ELSE
            filename = template(1:i)
        endif
      else
 
        if (index(modl, '-') .ne. 0) then

c             this should be a PCMDI standard model short name:
           call modltabl(modl, 'standard', shrtname, longname)

        elseif (index(pthd, 'amip') .ne. 0) then

           if (index(pthd, 'amip2') .eq. 0) then 
c              this should be an AMIP 1 model:
              call modltabl(modl, 'amip1', shrtname, longname)
           else
c              this should be an AMIP 2 model:
              call modltabl(modl, 'amip2', shrtname, longname)
           endif

        elseif (index(pthd, 'pmip') .ne. 0) then
   
           if (index(pthd, 'fix') .ne. 0) then
c              this should be a PMIP fixed SST model
              call modltabl(modl, 'pmipfix', shrtname, longname)
           else
c              this should be a PMIP cal SST model
              call modltabl(modl, 'pmipcal', shrtname, longname)
           endif

        else

c           this should be a CMIP model
           call modltabl(modl, 'cmip', shrtname, longname)

        endif

        i = index(template, 'MOD') - 1
        j = index(shrtname, ' ') - 1
        k = i + 4
        m = index(template, ' ') - 1
        filename = template(1:i)//shrtname(1:j)//template(k:m)//'_rslv'

      endif

      return
      end
          




c **********************************************************************
c ----------------------------------------------------------------------
c
      subroutine modltabl(modl, expt, shrtname, longname)
c
c ----------------------------------------------------------------------
c **********************************************************************

      implicit none
      character*(*) modl, expt, shrtname, longname
      character*80 amip2(3,3)
c      character*80 amip1(3,40), pmipfix(3,20), pmipcal(3,10), 
c     &  cmip(3,15)

c    table of models, acronyms, etc.
c    alias, short name, long name

      data amip2/
     & 'bmr'        , 'BMRC_A1'     ,            '???'              ,
     & 'bmra'       , 'BMRC_A1a'    ,            '???'              ,
     & 'bmrb' ,       '???', '???'
     & /


      if (expt(1:8) .eq. 'standard') then
c       search tables for standard names:

        shrtname  = modl
        longname = '???'

      elseif (expt(1:5) .eq. 'amip1') then
c       search amip1 table for standard names:

        shrtname = '???'
        longname = '???'

      elseif (expt(1:5) .eq. 'amip2') then
c       search amip2 table for standard names:

        shrtname = '???'
        longname = '???'

      elseif (expt(1:5) .eq. 'pmipfix') then
c       search pmipfix table for standard names:

        shrtname = '???'
        longname = '???'

      elseif (expt(1:5) .eq. 'pmipcal') then
c       search pmipcal table for standard names:

        shrtname = '???'
        longname = '???'

      elseif (expt(1:5) .eq. 'cmip') then
c       search cmip table for standard names:

        shrtname = '???'
        longname = '???'

      endif

      return
      end

c **********************************************************************
c ----------------------------------------------------------------------
c
      subroutine resolve(nlon, nlat, mpy, maxyr, lentim, idoclim,
     &     adata, bdata, wt, wts, avga, avgb, 
     &     vara, varb, correl, rms,
     &     a2, a3, a4, a5, a6, 
     &     b2, b3, b4, b5, b6, 
     &     wt2, wt3, wt4, wt5, wt6,
     &     ai1, ai2, ai3, ai4, ai5, 
     &     bi1, bi2, bi3, bi4, bi5, 
     &     wi1, wi2, wi3, wi4, wi5, 
     &     siwyr, siayr, sibyr)
c
c ----------------------------------------------------------------------
c ********************************************************************** 

c    resolve into climatological ( annual mean (global mean + zonal dev
c      + long dev) 
c      + deviation from annual mean (global mean + zonal + dev) +
c      + interannual variability
c
c
      implicit none

      integer nlon, nlat, mpy, maxyr, lentim, idoclim
      real adata(nlon,nlat,lentim), bdata(nlon,nlat,lentim),
     +     wt(nlon,nlat,lentim), wts(28), avga(28), avgb(28), vara(28),
     +     varb(28), correl(28), rms(28)
    
c     local variables

      double precision a1, b1, wt1, ai, bi, wi, aa, bb, ww

      integer i, j, m, n, nn, iwtsum
      integer icomp(6)
      double precision suma, sumb, sumaa, sumbb, sumab, sumrms, ssuma, 
     +     ssumb, sumwt, ssumwt, sia, sib, siw, siaa, sibb, siab, sirms
      real wt6(nlon,nlat,mpy), a2(mpy), a3(nlat), a4(nlat,mpy)
      real a5(nlon,nlat), a6(nlon,nlat,mpy), b2(mpy), b3(nlat)
      real b4(nlat,mpy), b5(nlon,nlat), b6(nlon,nlat,mpy)
      real wt2(mpy), wt3(nlat), wt4(nlat,mpy), wt5(nlon,nlat)
      real ai1(maxyr), ai2(lentim), ai3(nlat,maxyr), ai4(nlat,lentim)
      real ai5(nlon,nlat,maxyr)
      real bi1(maxyr), bi2(lentim), bi3(nlat,maxyr)
      real bi4(nlat,lentim), bi5(nlon,nlat,maxyr)
      real wi1(maxyr), wi2(lentim)
      real wi3(nlat,maxyr), wi4(nlat,lentim), wi5(nlon,nlat,maxyr)
      real siwyr(maxyr*2), siayr(maxyr*2), sibyr(maxyr*2)

          
      do 50 i=1,28
        wts(i) = 0.0
        avga(i) = 0.0
        avgb(i) = 0.0
        vara(i) = 0.0
        varb(i) = 0.0
        correl(i) = 0.0
        rms(i) = 0.0
   50 continue

      if (maxyr .eq. 1) then
c        no interannual variability:

        do 70 nn=1,maxyr
          wi1(nn) = 0.0
          ai1(nn) = 0.0
          bi1(nn) = 0.0
          do 60 j=1,nlat
            wi3(j,nn) = 0.0
            ai3(j,nn) = 0.0
            bi3(j,nn) = 0.0
            do 55 i=1,nlon
              wi5(i,j,nn) = 0.0
              ai5(i,j,nn) = 0.0
              bi5(i,j,nn) = 0.0
   55       continue
   60     continue
   70   continue

        do 90 n=1,lentim
          wi2(n) = 0.0
          ai2(n) = 0.0
          bi2(n) = 0.0
          do 80 j=1,nlat
            wi4(j,n) = 0.0
            ai4(j,n) = 0.0
            bi4(j,n) = 0.0
   80     continue
   90   continue

      endif

c   compute climatologically averaged fields

      iwtsum=0
      do 200 j=1,nlat
        do 190 i=1,nlon
            do 180 m=1,mpy
              suma = 0.0
              sumb = 0.0
              sumwt = 0.0
              do 170 n=m,lentim,mpy
                  sumwt = sumwt + wt(i,j,n)
                  suma = suma + wt(i,j,n)*adata(i,j,n)
                  sumb = sumb + wt(i,j,n)*bdata(i,j,n)
  170         continue
              wt6(i,j,m) = sumwt
              if (sumwt .gt. 0.) then
c                 monthly climatology
                a6(i,j,m) = suma/sumwt
                b6(i,j,m) = sumb/sumwt
              else
                a6(i,j,m) = 0.0
                b6(i,j,m) = 0.0
              endif
  180       continue
  190   continue
  200 continue

c   compute climatological annual average field 

      do 300 j=1,nlat
        do 290 i=1,nlon

            sumwt = 0.0
            suma = 0.0
            sumb = 0.0
            do 260 nn=1,maxyr
              siwyr(nn) = 0.0
              siayr(nn) = 0.0
              sibyr(nn) = 0.0
  260       continue
            do 280 m=1,mpy
              do 270 n=m,lentim,mpy
                nn = (n-1)/mpy + 1
                sumwt = sumwt + wt(i,j,n)
                suma = suma + wt(i,j,n)*adata(i,j,n)
                sumb = sumb + wt(i,j,n)*bdata(i,j,n)
                siwyr(nn) = siwyr(nn) + wt(i,j,n)
                siayr(nn) = siayr(nn) + 
     &                              wt(i,j,n)*(adata(i,j,n)-a6(i,j,m))
                sibyr(nn) = sibyr(nn) + 
     &                              wt(i,j,n)*(bdata(i,j,n)-b6(i,j,m))
  270         continue
  280       continue
            if (maxyr .gt. 1) then
              do 285 nn=1,maxyr
                wi5(i,j,nn) = siwyr(nn)
                if (siwyr(nn) .gt. 0.) then
c                annual mean anomaly (anomaly, for mpy=1)
                  ai5(i,j,nn) = siayr(nn)/siwyr(nn)
                  bi5(i,j,nn) = sibyr(nn)/siwyr(nn)
                else
                  ai5(i,j,nn) = 0.0
                  bi5(i,j,nn) = 0.0
                endif
  285         continue
            endif
            wt5(i,j) = sumwt
            if (sumwt .gt. 0.) then
c               annual mean climatology 
c                      (=a6, b6, i.e., climatology, for mpy=1)
              a5(i,j) = suma/sumwt
              b5(i,j) = sumb/sumwt
            else
              a5(i,j) = 0.0
              b5(i,j) = 0.0
            endif
  290   continue
  300 continue
c
c    compute climatological and zonal average field 
             
c ??? if (mpy .gt. 1) then   
      do 400 j=1,nlat
        do 390 m=1,mpy
          sumwt = 0.0
          suma = 0.0
          sumb = 0.0
          do 380 n=m,lentim,mpy
              siw = 0.0
              sia = 0.0
              sib = 0.0
              do 370 i=1,nlon
                    sumwt = sumwt + wt(i,j,n)
                    suma = suma + wt(i,j,n)*adata(i,j,n)
                    sumb = sumb + wt(i,j,n)*bdata(i,j,n)
                    siw = siw + wt(i,j,n)
                    sia = sia + wt(i,j,n)*(adata(i,j,n)-a6(i,j,m))
                    sib = sib + wt(i,j,n)*(bdata(i,j,n)-b6(i,j,m))
  370         continue
              if (maxyr .gt. 1) then
                wi4(j,n) = siw
                if (siw .gt. 0.) then
c                 zonal mean monthly anomaly
c                  (= a3, b3, i.e. zonal mean anomaly, if mpy=1)
                  ai4(j,n) = sia/siw
                  bi4(j,n) = sib/siw
                else
                  ai4(j,n) = 0.0
                  bi4(j,n) = 0.0  
                endif
              endif
  380     continue
          wt4(j,m) = sumwt
          if (sumwt .gt. 0.) then
c            zonal mean monthly climatology
c              (= a3, b3, i.e. zonal mean climatology, if mpy=1)
            a4(j,m) = suma/sumwt
            b4(j,m) = sumb/sumwt
          else
            a4(j,m) = 0.0
            b4(j,m) = 0.0
          endif
  390   continue
  400 continue
c ??? endif

c    compute climatological, zonal, annual average field

      do 500 j=1,nlat
        sumwt = 0.0
        suma = 0.0
        sumb = 0.0
        do 460 nn=1,maxyr
          siwyr(nn) = 0.0
          siayr(nn) = 0.0
          sibyr(nn) = 0.0
  460   continue
        do 490 m=1,mpy
          siw = 0.0
          sia = 0.0
          sib = 0.0
          do 480 n=m,lentim,mpy
              nn = (n-1)/mpy + 1
              do 470 i=1,nlon
                  sumwt = sumwt + wt(i,j,n)
                  suma = suma + wt(i,j,n)*adata(i,j,n)
                  sumb = sumb + wt(i,j,n)*bdata(i,j,n)
                  siwyr(nn) = siwyr(nn) + wt(i,j,n)
                  siayr(nn) = siayr(nn) + 
     &                           wt(i,j,n)*(adata(i,j,n)-a6(i,j,m))
                  sibyr(nn) = sibyr(nn) + 
     &                           wt(i,j,n)*(bdata(i,j,n)-b6(i,j,m))
  470         continue
  480     continue
  490   continue
        if (maxyr .gt. 1) then
          do 495 nn=1,maxyr
            wi3(j,nn) = siwyr(nn)
            if (siwyr(nn) .gt. 0.0) then
              ai3(j,nn) = siayr(nn)/siwyr(nn)
              bi3(j,nn) = sibyr(nn)/siwyr(nn)
            else
              ai3(j,nn) = 0.0
              bi3(j,nn) = 0.0
            endif
  495     continue
        endif     
c          zonal, annual mean climatology 
c             (=a4, b4, i.e., zonal mean climatology, for mpy=1)
        wt3(j) = sumwt
        if (sumwt .gt. 0.0) then
          a3(j) = suma/sumwt
          b3(j) = sumb/sumwt
        else
          a3(j) = 0.0
          b3(j) = 0.0
        endif
  500 continue

c    compute climatological, global average field
c     and also clim., global, annual mean field

      sumwt = 0.0
      suma = 0.0
      sumb = 0.0

      do 510 nn=1,maxyr
        wi1(nn) = 0.0
        ai1(nn) = 0.0
        bi1(nn) = 0.0
  510 continue

      do 600 m=1,mpy
        ssumwt = 0.0
        ssuma = 0.0
        ssumb = 0.0
        do 590 n=m,lentim,mpy
          nn = (n-1)/mpy + 1
          siw = 0.0
          sia = 0.0
          sib = 0.0
          do 580 j=1,nlat
            do 570 i=1,nlon
                  ssumwt = ssumwt + wt(i,j,n)
                  ssuma = ssuma + wt(i,j,n)*adata(i,j,n)
                  ssumb = ssumb + wt(i,j,n)*bdata(i,j,n)
                  siw = siw + wt(i,j,n)
                  sia = sia + wt(i,j,n)*(adata(i,j,n)-a6(i,j,m))
                  sib = sib + wt(i,j,n)*(bdata(i,j,n)-b6(i,j,m))
  570       continue
  580     continue
          wi2(n) = siw
          if (siw .gt. 0.0) then
c            global mean monthly anomaly
c             (=ai1, bi1, i.e., global mean anomaly, if mpy=1)
            ai2(n) = sia/siw
            bi2(n) = sib/siw
          else
            ai2(n) = 0.0
            bi2(n) = 0.0
          endif
          wi1(nn) = wi1(nn) + siw
          ai1(nn) = ai1(nn) + sia 
          bi1(nn) = bi1(nn) + sib 
  590   continue
        wt2(m) = ssumwt
        if (ssumwt .gt. 0.0) then
c            global mean monthly climatology
c             (=a1, b1, i.e., global mean anomaly, if mpy=1)
          a2(m) = ssuma/ssumwt
          b2(m) = ssumb/ssumwt
        else
          a2(m) = 0.0
          b2(m) = 0.0
        endif
        sumwt = sumwt + ssumwt
        suma = suma + ssuma
        sumb = sumb + ssumb
  600 continue
      wt1 = sumwt
      if (sumwt .gt. 0.0) then
c          global mean annual climatology 
c            (global mean climatology if mpy=1)
        a1 = suma/sumwt
        b1 = sumb/sumwt
      else
        a1 = 0.0
        b1 = 0.0
      endif

      if (maxyr .gt. 1) then
        do 605 nn=1,maxyr
          if (wi1(nn) .gt. 0.0) then
c          global mean annual anomaly 
c            (global mean anomaly if mpy=1)
            ai1(nn) = ai1(nn)/wi1(nn)
            bi1(nn) = bi1(nn)/wi1(nn)
          endif
  605   continue

c       compute statistics for field 1:

        siw = 0.0
        sia = 0.0
        sib = 0.0
        siaa = 0.0
        sibb = 0.0
        siab = 0.0
        sirms = 0.0

        do 625 nn=1,maxyr

          ai = ai1(nn)
          bi = bi1(nn)
          wi = wi1(nn)

          siw = siw + wi
          sia = sia + ai*wi
          siaa = siaa + ai*ai*wi
          sib = sib + bi*wi
          sibb = sibb + bi*bi*wi
          siab = siab + ai*bi*wi
          sirms = sirms + (ai-bi)*(ai-bi)*wi

  625   continue

        call fillout(siw, sia, sib, siaa, sibb, siab, sirms,
     +   wts(8), avga(8), avgb(8), vara(8), varb(8), correl(8), rms(8))

      endif

      if (mpy .gt. 1) then
c   compute statistics for field 2:
c    should be zero if mpy=1

      sumwt = 0.
      suma = 0.
      sumb = 0.
      sumaa = 0.0
      sumbb = 0.0
      sumab = 0.0
      sumrms = 0.0
      siw = 0.0
      sia = 0.0
      sib = 0.0
      siaa = 0.0
      sibb = 0.0
      siab = 0.0
      sirms = 0.0

      do 700 m=1,mpy

        aa = a2(m) - a1
        bb = b2(m) - b1
        ww = wt2(m)

        sumwt = sumwt + ww
        suma = suma + aa*ww
        sumaa = sumaa + aa*aa*ww
        sumb = sumb + bb*ww
        sumbb = sumbb + bb*bb*ww
        sumab = sumab + aa*bb*ww
        sumrms = sumrms + (aa-bb)*(aa-bb)*ww

        if (maxyr .gt. 1) then
          do 675 n=m,lentim,mpy

            nn = (n-1)/mpy + 1
            ai = ai2(n) - ai1(nn)
            bi = bi2(n) - bi1(nn)
            wi = wi2(n)

            siw = siw + wi
            sia = sia + ai*wi
            siaa = siaa + ai*ai*wi
            sib = sib + bi*wi
            sibb = sibb + bi*bi*wi
            siab = siab + ai*bi*wi
            sirms = sirms + (ai-bi)*(ai-bi)*wi

  675     continue
        endif

  700 continue 

      call fillout(sumwt, suma, sumb, sumaa, sumbb, sumab, sumrms,
     +   wts(2), avga(2), avgb(2), vara(2), varb(2), correl(2), rms(2))
      call fillout(siw, sia, sib, siaa, sibb, siab, sirms,
     +   wts(9), avga(9), avgb(9), vara(9), varb(9), correl(9), rms(9))

      endif

c   compute statistics for field 3:

      sumwt = 0.
      suma = 0.
      sumb = 0.
      sumaa = 0.0
      sumbb = 0.0
      sumab = 0.0
      sumrms = 0.0
      siw = 0.0
      sia = 0.0
      sib = 0.0
      siaa = 0.0
      sibb = 0.0
      siab = 0.0
      sirms = 0.0

      do 800 j=1,nlat

        aa = a3(j) - a1 
        bb = b3(j) - b1 
        ww = wt3(j)

        sumwt = sumwt + ww
        suma = suma + aa*ww
        sumaa = sumaa + aa*aa*ww
        sumb = sumb + bb*ww
        sumbb = sumbb + bb*bb*ww
        sumab = sumab + aa*bb*ww
        sumrms = sumrms + (aa-bb)*(aa-bb)*ww

        if (maxyr .gt. 1) then
          do 775 nn=1,maxyr

            ai = ai3(j,nn) - ai1(nn)
            bi = bi3(j,nn) - bi1(nn)
            wi = wi3(j,nn)

            siw = siw + wi
            sia = sia + ai*wi
            siaa = siaa + ai*ai*wi
            sib = sib + bi*wi
            sibb = sibb + bi*bi*wi
            siab = siab + ai*bi*wi
            sirms = sirms + (ai-bi)*(ai-bi)*wi

  775     continue
        endif

  800 continue 

      call fillout(sumwt, suma, sumb, sumaa, sumbb, sumab, sumrms,
     +   wts(3), avga(3), avgb(3), vara(3), varb(3), correl(3), rms(3))
      call fillout(siw, sia, sib, siaa, sibb, siab, sirms, wts(10),
     + avga(10), avgb(10), vara(10), varb(10), correl(10), rms(10))

      if (mpy .gt. 1) then
c   compute statistics for field 4:
c    should be zero if mpy=1

      sumwt = 0.
      suma = 0.
      sumb = 0.
      sumaa = 0.0
      sumbb = 0.0
      sumab = 0.0
      sumrms = 0.0
      siw = 0.0
      sia = 0.0
      sib = 0.0
      siaa = 0.0
      sibb = 0.0
      siab = 0.0
      sirms = 0.0

      do 900 m=1,mpy
        do 890 j=1,nlat

          aa = a4(j,m) - a1 - (a2(m)-a1) - (a3(j)-a1)
          bb = b4(j,m) - b1 - (b2(m)-b1) - (b3(j)-b1)
          ww = wt4(j,m)

          sumwt = sumwt + ww
          suma = suma + aa*ww
          sumaa = sumaa + aa*aa*ww
          sumb = sumb + bb*ww
          sumbb = sumbb + bb*bb*ww
          sumab = sumab + aa*bb*ww
          sumrms = sumrms + (aa-bb)*(aa-bb)*ww

          if (maxyr .gt. 1) then
            do 875 n=m,lentim,mpy

              nn = (n-1)/mpy + 1
              ai = ai4(j,n)-ai1(nn)-(ai2(n)-ai1(nn))-(ai3(j,nn)-ai1(nn))
              bi = bi4(j,n)-bi1(nn)-(bi2(n)-bi1(nn))-(bi3(j,nn)-bi1(nn))
              wi = wi4(j,n)

              siw = siw + wi
              sia = sia + ai*wi
              siaa = siaa + ai*ai*wi
              sib = sib + bi*wi
              sibb = sibb + bi*bi*wi
              siab = siab + ai*bi*wi
              sirms = sirms + (ai-bi)*(ai-bi)*wi

  875       continue
          endif

  890   continue

  900 continue 

      call fillout(sumwt, suma, sumb, sumaa, sumbb, sumab, sumrms,
     +   wts(4), avga(4), avgb(4), vara(4), varb(4), correl(4), rms(4))
      call fillout(siw, sia, sib, siaa, sibb, siab, sirms, wts(11),
     + avga(11), avgb(11), vara(11), varb(11), correl(11), rms(11))

      endif
c   compute statistics for field 5:

      sumwt = 0.
      suma = 0.
      sumb = 0.
      sumaa = 0.0
      sumbb = 0.0
      sumab = 0.0
      sumrms = 0.0
      siw = 0.0
      sia = 0.0
      sib = 0.0
      siaa = 0.0
      sibb = 0.0
      siab = 0.0
      sirms = 0.0

      do 1000 j=1,nlat
        do 990 i=1,nlon

            aa = a5(i,j) - a3(j)
            bb = b5(i,j) - b3(j)
            ww = wt5(i,j)

            sumwt = sumwt + ww
            suma = suma + aa*ww
            sumaa = sumaa + aa*aa*ww
            sumb = sumb + bb*ww
            sumbb = sumbb + bb*bb*ww
            sumab = sumab + aa*bb*ww
            sumrms = sumrms + (aa-bb)*(aa-bb)*ww

            if (maxyr .gt. 1) then
              do 975 nn=1,maxyr

                ai = ai5(i,j,nn) - ai3(j,nn)
                bi = bi5(i,j,nn) - bi3(j,nn)
                wi = wi5(i,j,nn)
        
                siw = siw + wi
                sia = sia + ai*wi
                siaa = siaa + ai*ai*wi
                sib = sib + bi*wi
                sibb = sibb + bi*bi*wi
                siab = siab + ai*bi*wi
                sirms = sirms + (ai-bi)*(ai-bi)*wi
 
  975         continue
            endif

  990   continue

 1000 continue 

      call fillout(sumwt, suma, sumb, sumaa, sumbb, sumab, sumrms,
     +   wts(5), avga(5), avgb(5), vara(5), varb(5), correl(5), rms(5))
      call fillout(siw, sia, sib, siaa, sibb, siab, sirms, wts(12),
     + avga(12), avgb(12), vara(12), varb(12), correl(12), rms(12))

      if (mpy .gt. 1) then
c   compute statistics for field 6:
c    should be zero if mpy=1

      sumwt = 0.
      suma = 0.
      sumb = 0.
      sumaa = 0.0
      sumbb = 0.0
      sumab = 0.0
      sumrms = 0.0
      siw = 0.0
      sia = 0.0
      sib = 0.0
      siaa = 0.0
      sibb = 0.0
      siab = 0.0
      sirms = 0.0

      do 1100 j=1,nlat
        do 1090 i=1,nlon

            do 1080 m=1,mpy

              aa = a6(i,j,m) - (a5(i,j) - a3(j)) - a4(j,m)
              bb = b6(i,j,m) - (b5(i,j) - b3(j)) - b4(j,m)
              ww = wt6(i,j,m)

              sumwt = sumwt + ww
              suma = suma + aa*ww
              sumaa = sumaa + aa*aa*ww
              sumb = sumb + bb*ww
              sumbb = sumbb + bb*bb*ww
              sumab = sumab + aa*bb*ww
              sumrms = sumrms + (aa-bb)*(aa-bb)*ww

              if (maxyr .gt. 1) then
                do 1075 n=m,lentim,mpy

                  nn = (n-1)/mpy + 1
                  ai = adata(i,j,n) - a6(i,j,m) -
     +                       (ai5(i,j,nn) - ai3(j,nn)) - ai4(j,n)
                  bi = bdata(i,j,n) - b6(i,j,m) -
     +                       (bi5(i,j,nn) - bi3(j,nn)) - bi4(j,n)
                  wi = wt(i,j,n)

                  siw = siw + wi
                  sia = sia + ai*wi
                  siaa = siaa + ai*ai*wi
                  sib = sib + bi*wi
                  sibb = sibb + bi*bi*wi
                  siab = siab + ai*bi*wi
                  sirms = sirms + (ai-bi)*(ai-bi)*wi

 1075           continue
              endif

 1080       continue

 1090   continue

 1100 continue 

      call fillout(sumwt, suma, sumb, sumaa, sumbb, sumab, sumrms,
     +   wts(6), avga(6), avgb(6), vara(6), varb(6), correl(6), rms(6))
      call fillout(siw, sia, sib, siaa, sibb, siab, sirms, wts(13),
     + avga(13), avgb(13), vara(13), varb(13), correl(13), rms(13))

      endif

c   compute statistics for field 7:

      sumwt = 0.
      suma = 0.
      sumb = 0.
      sumaa = 0.0
      sumbb = 0.0
      sumab = 0.0
      sumrms = 0.0

      if (maxyr .gt. 1) then
        do 1200 j=1,nlat
          do 1190 i=1,nlon

            do 1180 m=1,mpy

              do 1170 n=m,lentim,mpy
    
                  ww = wt(i,j,n)
                  aa = adata(i,j,n) - a6(i,j,m)
                  bb = bdata(i,j,n) - b6(i,j,m)

                  sumwt = sumwt + ww
                  suma = suma + ww*aa
                  sumaa = sumaa + ww*aa*aa
                  sumb = sumb + ww*bb
                  sumbb = sumbb + ww*bb*bb
                  sumab = sumab + ww*aa*bb
                  sumrms = sumrms + ww*(aa-bb)*(aa-bb)

 1170         continue

 1180       continue

 1190     continue

 1200   continue 

        call fillout(sumwt, suma, sumb, sumaa, sumbb, sumab, sumrms,
     +    wts(7), avga(7), avgb(7), vara(7), varb(7), correl(7), rms(7))

      endif

      avga(1) = a1
      avgb(1) = b1
      wts(1) = wt1
      
 
c   compute statistics of full fields
      
      sumwt = 0.0
      suma = 0.0
      sumb = 0.0
      sumaa = 0.0
      sumbb = 0.0
      sumab = 0.0
      sumrms = 0.0

      do 1500 j=1,nlat
        do 1400 i=1,nlon

          do 1300 n=1,lentim

              ww = wt(i,j,n)
              aa = adata(i,j,n)
              bb = bdata(i,j,n)

              sumwt = sumwt + ww
              suma = suma + ww*aa
              sumb = sumb + ww*bb
              sumaa = sumaa + ww*aa*aa
              sumbb = sumbb + ww*bb*bb
              sumab = sumab + ww*aa*bb
              sumrms = sumrms + ww*(aa-bb)*(aa-bb)

 1300     continue
            
 1400   continue

 1500 continue

      call fillout(sumwt, suma, sumb, sumaa, sumbb, sumab, sumrms,
     +   wts(28), avga(28), avgb(28), vara(28), varb(28), correl(28),
     +   rms(28))


      icomp(1) = 3
      icomp(2) = 5

      call sumsq(2, 14, icomp, wts, avga, avgb, vara, varb, 
     +      rms, correl)

      icomp(1) = 2
      icomp(2) = 4

      call sumsq(2, 15, icomp, wts, avga, avgb, vara, varb, 
     +      rms, correl)

      icomp(1) = 2
      icomp(2) = 4
      icomp(3) = 6

      call sumsq(3, 16, icomp, wts, avga, avgb, vara, varb, 
     +      rms, correl)

      if (mpy .gt. 1) then

        icomp(1) = 2
        icomp(2) = 3
        icomp(3) = 4

        call sumsq(3, 17, icomp, wts, avga, avgb, vara, varb, 
     +      rms, correl)

        icomp(1) = 5
        icomp(2) = 6

        call sumsq(2, 18, icomp, wts, avga, avgb, vara, varb, 
     +      rms, correl)

        icomp(1) = 2
        icomp(2) = 3
        icomp(3) = 4
        icomp(4) = 5
        icomp(5) = 6

        call sumsq(5, 19, icomp, wts, avga, avgb, vara, varb, 
     +      rms, correl)

        icomp(1) = 2
        icomp(2) = 3
        icomp(3) = 4
        icomp(4) = 5
        icomp(5) = 6
        n = 5
        if (idoclim .eq. 0) then
          n = 6
          icomp(6) = 7
        endif

        call sumsq(n, 20, icomp, wts, avga, avgb, vara, varb, 
     +      rms, correl)

      else

        icomp(1) = 3

        call sumsq(1, 17, icomp, wts, avga, avgb, vara, varb, 
     +      rms, correl)

        icomp(1) = 5

        call sumsq(1, 18, icomp, wts, avga, avgb, vara, varb, 
     +      rms, correl)

        icomp(1) = 3
        icomp(2) = 5

        call sumsq(2, 19, icomp, wts, avga, avgb, vara, varb, 
     +      rms, correl)

        icomp(1) = 3
        icomp(2) = 5
        n = 2
        if (idoclim .eq. 0) then
          n = 3
          icomp(3) = 7
        endif

        call sumsq(n, 20, icomp, wts, avga, avgb, vara, varb, 
     +      rms, correl)


      endif

      if (idoclim .eq. 0) then

        icomp(1) = 9
        icomp(2) = 11

        call sumsq(2, 21, icomp, wts, avga, avgb, vara, varb, 
     +      rms, correl)

        icomp(1) = 9
        icomp(2) = 11
        icomp(3) = 13

        call sumsq(3, 22, icomp, wts, avga, avgb, vara, varb, 
     +      rms, correl)

        icomp(1) = 8
        icomp(2) = 10

        call sumsq(2, 23, icomp, wts, avga, avgb, vara, varb, 
     +      rms, correl)

        icomp(1) = 8
        icomp(2) = 10
        icomp(3) = 12

        call sumsq(3, 24, icomp, wts, avga, avgb, vara, varb, 
     +      rms, correl)

        if (mpy .gt. 1) then

          icomp(1) = 8
          icomp(2) = 9
          icomp(3) = 10
          icomp(4) = 11

          call sumsq(4, 25, icomp, wts, avga, avgb, vara, varb, 
     +      rms, correl)

          icomp(1) = 12
          icomp(2) = 13

          call sumsq(2, 26, icomp, wts, avga, avgb, vara, varb, 
     +      rms, correl)

          icomp(1) = 8
          icomp(2) = 9
          icomp(3) = 10
          icomp(4) = 11
          icomp(5) = 12
          icomp(6) = 13

          call sumsq(6, 27, icomp, wts, avga, avgb, vara, varb, 
     +      rms, correl)

        else

          icomp(1) = 8
          icomp(2) = 10

          call sumsq(2, 25, icomp, wts, avga, avgb, vara, varb, 
     +      rms, correl)

          icomp(1) = 12

          call sumsq(1, 26, icomp, wts, avga, avgb, vara, varb, 
     +      rms, correl)

          icomp(1) = 8
          icomp(2) = 10
          icomp(3) = 12

          call sumsq(3, 27, icomp, wts, avga, avgb, vara, varb, 
     +      rms, correl)


        endif

      endif

        
      return
      end


c **********************************************************************
c ----------------------------------------------------------------------
c
      subroutine sumsq(nn, io, icomp, wts, avga, avgb, vara, varb, 
     +      rms, correl)
c
c ----------------------------------------------------------------------
c ********************************************************************** 

      implicit none
      integer icomp(*), nn, io
      double precision sw, a, b, aa, bb, rmsl
      real wts(*), avga(*), avgb(*), vara(*), varb(*), correl(*), rms(*)
      integer n, i

      sw = 0.0
      a = 0.0
      b = 0.0
      aa = 0.0
      bb = 0.0
      rmsl = 0.0

      do 100 n=1,nn
        i = icomp(n)
        sw = sw + wts(i)
        a = a + avga(i)
        b = b + avgb(i)
        aa = aa + vara(i)
        bb = bb + varb(i)
        rmsl = rmsl + rms(i)*rms(i)
  100 continue
      
      wts(io) = sw/nn
      avga(io) = a/nn
      avgb(io) = b/nn
      vara(io) = aa
      varb(io) = bb
      if (rmsl .gt. 0.) then
        rms(io) = sqrt(rmsl)
      else
        rms(io) = 0.0
      endif
      if ((aa .gt. 0.) .and. (bb .gt. 0.)) then
        correl(io) = (aa + bb - rmsl)/(2.*sqrt(aa*bb))
      else 
        correl(io) = 0.0 
      endif

      return
      end  


C      SUBROUTINE set_n(value)
C      IMPLICIT NONE
C      SAVE n
C      INTEGER n,value
C      data n/0/
C      n=value
C      WRITE(*,*) 'n set to:',n
C      return
C      END

c **********************************************************************
c ----------------------------------------------------------------------
c
      subroutine wrtstats(nn, icall, vars1, modl1, pthd1, pthmask1,  
     +       vars2, modl2, pthd2, pthmask2, region, 
     &       minyr, fracmin, months, years, alon0, alon1, 
     +       alat0, alat1, ncomps, mons10, mons11, 
     +       mons20, mons21, l1i, l2i, l1f, l2f,
     +       itarg, tmodl, tpth, tvar, tpthmask, alon, mlon, dellon, 
     +       alat, mlat, 
     +       dellat, mvar, mmodl, mpth, mpthmask, mmons0, mmons1,
     +       wts, avga, avgb, vara, varb, correl, rms, template,
     &       retrlev1, retrlev2, retrlevm, units1, sloptst,
     &       offsett, mskvar1, l1o, units2, slopref, 
     &       offsetr, mskvar2, l2o, mmskvar, mi,
     &       mo, tmskvar,levunits1,levunits2,levunits3, 
     &       idoclim, ifulla, imona, iseasa, ianna, rundate, 
     &       runtime, alev0, alev10, alev20, reset_n)
c
c 
c 
c ----------------------------------------------------------------------
c ********************************************************************** 

      implicit none

      save n

c     arguments passed to subroutine

      integer nn, l1i(5), l1f(5), l2i(5), l2f(5), mlon, mlat,
     &     ncomps, mons10, mons11, mons20, mons21, mmons0, mmons1, 
     &     itarg, icall, minyr, months(2,3), years(2,3), l1o(5), 
     &     l2o(5), mi(5), mo(5), idoclim, ifulla, imona,
     &     iseasa, ianna, reset_n

      real alon0, alon1, alat0, alat1, dellon, dellat, retrlev1, 
     &     retrlev2, retrlevm, fracmin, slopref, offsetr, sloptst,
     &     offsett, alev0, alev10, alev20, alon, alat


      real wts(28), avga(28), avgb(28), vara(28), varb(28), correl(28),
     +      rms(28)

      character*(*) vars1, modl1, pthd1, vars2, modl2, pthd2, region,
     &     template, pthmask1, pthmask2, tmodl, tpth, tpthmask, 
     &     mvar, mmodl, mpth, mpthmask, units1, mskvar1,
     &     units2, mskvar2, mmskvar,
     &     tmskvar, levunits1, levunits2, levunits3,
     &     tvar, rundate, runtime

c     local variables:
   
      integer n, i, m, j, k, kunit, mm
      parameter (mm=19*40)

      real avgal(28,mm), avgbl(28,mm), sda(28,mm), sdb(28,mm), 
     +      rmsl(28,mm), correll(28,mm), pvara(28,mm), pvarb(28,mm),
     +      pmsdiff(28,mm), rank(28,mm), wtl(28,mm), ipt(mm), 
     +      pvar1a(28,mm)
      real aavga(28), aavgb(28), asda(28), asdb(28), arms(28), awt(28),
     +     acorrel(28), apvara(28), apvarb(28), apmsdiff(28), arank(28),
     +     apvar1a(28)

      character*5 alist(mm), amodel(mm)
      character*16 shrtname, tablname(19)
      character*240 longname, ket1file, ket1text,strip

      data n/0/
      data alist/mm*'     '/, amodel/mm*'     '/
      data tablname/'January', 'February', 'March', 'April', 'May',
     &       'June', 'July', 'August', 'September', 'October', 
     &       'November', 'December', 'DJF', 'MAM', 'JJA', 'SON',
     &       'annual mean', 'all seasons',  'all months'/

c     generate file name:

      template=strip(template)
      
c      print*, 'nn',nn
c      print*, 'icall:',icall
c      print*, 'vars1', vars1
c      print*, 'modl1:', modl1
c      print *, 'pthd1', pthd1
c      print*, 'pthmask1',  pthmask1 
c      print*, 'vars2', vars2
c      print*, 'modl2', modl2
c      print*, 'pthd2', pthd2
c      print*, 'pthmask2', pthmask2
c      print*, 'region', region
c      print*, 'minyr', minyr
c      print*, 'fracmin', fracmin
c      print*, 'months', months
c      print*, 'years', years
c      print*, 'alon0', alon0
c      print*, 'alon1', alon1
c      print*, 'alat0', alat0
c      print*, 'alat1', alat1
c      print*, 'ncomps', ncomps
c      print*, 'mons10', mons10
c      print*, 'mons11', mons11
c      print*, 'mons20', mons20
c      print*, 'mons21', mons21
c      print*, 'l1i', l1i
c      print*, 'l2i', l2i
c      print*, 'l1f', l1f
c      print*, 'l2f',l2f
c      print*, 'itarg', itarg
c      print*, 'tmodl', tmodl
c      print*, 'tpth', tpth
c      print*, 'tvar', tvar
c      print*, 'tpthmask',tpthmask 
c      print*, 'alon', alon
c      print*, 'mlon', mlon
c      print*, 'dellon', dellon
c      print*, 'alat', alat
c      print*, 'mlat', mlat
c      print*, 'dellat', dellat
c      print*, 'mvar', mvar
c      print*, 'mmodl', mmodl
c      print*, 'mpth', mpth
c      print*, 'mpthmask', mpthmask
c      print*, 'mmons0', mmons0
c      print*, 'mmons1',mmons1
c      print*, 'wts', wts
c      print*, 'avga', avga
c      print*, 'avgb', avgb
c      print*, 'vara', vara
c      print*, 'varb', varb
c      print*, 'correl', correl
c      print*, 'rms', rms
c      print *,'template',template
c      print*, 'retrlev1',retrlev1
c      print*, 'retrlev2',retrlev2
c      print*, 'retrlevm',retrlevm
c      print*, 'units1',units1
c      print*, 'sloptst',sloptst
c      print*, 'offsett',offsett
c      print*, 'mskvar1', mskvar1
c      print*, 'l1o', l1o
c      print*, 'units2', units2
c      print*, 'slopref', slopref
c      print*, 'offsetr', offsetr
c      print*, 'mskvar2', mskvar2
c      print*, 'l2o',l2o
c      print*, 'mmskvar',mmskvar
c      print*, 'mi',mi
c      print*, 'mo', mo
c      print*, 'tmskvar',tmskvar
c      print*, 'levunits1',levunits1
c      print*, 'levunits2',levunits2
c      print*, 'levunits3', levunits3
c      print*, 'idoclim', idoclim
c      print*, 'ifulla', ifulla
c      print*, 'imona', imona
c      print*, 'iseasa', iseasa
c      print*, 'ianna', ianna
c      print*, 'rundate', rundate
c      print*, 'runtime',runtime
c      print*, 'alev0', alev0
c      print*, 'alev10', alev10
c      print*, 'alev20', alev20
c      print*, 'reset_n',reset_n
      
      call mkoutfil(template, modl1, pthd1, shrtname, longname,
     &        ket1file)

      IF (reset_n.eq.1) n=0

      if (n .eq. 0) then


        kunit = 89
        ket1text = ket1file(1:index(ket1file,' ')-1)

        open(unit=kunit, file=ket1text, form='formatted', status='new')


          write(kunit, 9000)
          write(kunit, 9001)
c
          write(kunit, 9010) strip(rundate)
          write(kunit, 9020) strip(runtime)
          write(kunit, 9100) 

          write(kunit, 9110) ncomps

          write(kunit, 9200)
          write(kunit, 9210) strip(pthd1)
          write(kunit, 9215) strip(modl1)
          write(kunit, 9216) 
          write(kunit, 9220) strip(vars1)
          write(kunit, 9221) strip(units1)
          if (alev10 .ne. 0.0) then
             write(kunit, 9225) alev10, retrlev1, strip(levunits1)
          else
             write(kunit, 9226)  
          endif
          write(kunit, 9230) mons10, mons11
          if (months(1,1) .ne. 0) then
             write(kunit, 9235) months(1,1), years(1,1), months(2,1),
     &           years(2,1) 
          else
             write(kunit, 9236) 
          endif
          write(kunit, 9240) 
          write(kunit, 9245) l1i(1), l1i(2), l1i(3)
          write(kunit, 9250) l1o(1), l1o(2), l1o(3)
          write(kunit, 9255) l1f(1), l1f(2), l1f(3)
          write(kunit, 9260) sloptst
          write(kunit, 9265) offsett
          if (region .ne. 'all') then
            write(kunit, 9270) strip(pthmask1)
            write(kunit, 9275) strip(mskvar1)
          else
            write(kunit, 9271)
          endif

          write(kunit, 9300)

          write(kunit, 9310) strip(pthd2)
          write(kunit, 9315) strip(modl2)
          write(kunit, 9316) 
          write(kunit, 9320) strip(vars2)
          write(kunit, 9321) strip(units2)
          if (alev20 .ne. 0.0) then
             write(kunit, 9325)  alev20, retrlev2, strip(levunits2)
          else
             write(kunit, 9326)  
          endif
          write(kunit, 9330) mons20, mons21
          if (months(1,2) .ne. 0) then
             write(kunit, 9335) months(1,2), years(1,2), months(2,2),
     &           years(2,2) 
          else
             write(kunit, 9336) 
          endif
          write(kunit, 9340)  
          write(kunit, 9345) l2i(1), l2i(2), l2i(3)
          write(kunit, 9350) l2o(1), l2o(2), l2o(3)
          write(kunit, 9355) l2f(1), l2f(2), l2f(3)
          write(kunit, 9360) slopref
          write(kunit, 9365) offsetr
          if (region .ne. 'all') then
            write(kunit, 9370) strip(pthmask2)
            write(kunit, 9375) strip(mskvar2)
          else
            write(kunit, 9371)
          endif

          write(kunit,9400) 
          write(kunit, 9410) alat0, alat1
          write(kunit, 9420) alon0, alon1

          write(kunit, 9500)
          write(kunit, 9510) minyr
          write(kunit, 9511) fracmin

          if (region .eq. 'all') then
            write(kunit, 9520)
          else
            write(kunit, 9521) strip(region)
          endif

          write(kunit, 9530) 
          if (idoclim .gt. 0) then
            write(kunit, 9540) 
          else
            write(kunit, 9541) 
          endif

          if (ifulla .eq. 0) then
            write(kunit, 9545)
          elseif (ifulla .eq. 1) then
            write(kunit, 9546)
          elseif (ifulla .eq. 10) then
            write(kunit, 9547)
          else
            write(kunit, 9548)
          endif

          if (imona .eq. 0) then
             write(kunit, 9550)
          elseif (imona .lt. 13) then
             write(kunit, 9551) imona
          elseif (imona .eq. 13) then
             write(kunit, 9552)
          else
             write(kunit, 9553)
          endif

          if (iseasa .eq. 0) then
             write(kunit, 9560)
          elseif (iseasa .lt. 5) then
             write(kunit, 9561) iseasa
          elseif (iseasa .eq. 5) then
             write(kunit, 9562)
          else
             write(kunit, 9563)
          endif

          if (ianna .eq. 0) then
             write(kunit, 9570)
          else
             write(kunit, 9571) 
          endif
c

          write(kunit, 9600)

          if (itarg .le. 0) then

            write(kunit, 9611)

          else

            write(kunit, 9610) strip(mpth)
            write(kunit, 9615) strip(mmodl)
            write(kunit, 9616) 
            write(kunit, 9620) strip(mvar)
            if (alev0 .ne. 0.0) then
               write(kunit, 9625) alev0, retrlevm, strip(levunits3)
            else
               write(kunit, 9626)  
            endif
            write(kunit, 9630) mmons0, mmons1
          if (months(1,3) .ne. 0) then
             write(kunit, 9635) months(1,3), years(1,3), months(2,3),
     &              years(2,3)
          else
             write(kunit, 9636) 
          endif
            write(kunit, 9640)
            write(kunit, 9645) mi(1), mi(2), mi(3)
            write(kunit, 9650) mo(1), mo(2), mo(3)
            if (region .ne. 'all') then
              write(kunit, 9670) strip(mpthmask)
              write(kunit, 9675) strip(mmskvar)
            else
              write(kunit, 9671)
            endif

          endif

c
          write(kunit, 9701)
c          write(kunit, 9710) itarg
          write(kunit, 9710) 

c          if (iabs(itarg) .eq. 3) then
          if (iabs(itarg) .eq. 2) then

            write(kunit, 9720) strip(tpth)
            write(kunit, 9725) strip(tmodl)
            write(kunit, 9726) 
            write(kunit, 9730) strip(tvar)
            write(kunit, 9740)

            if (region .ne. 'all') then
              write(kunit, 9742) strip(tpthmask)
              write(kunit, 9744) strip(tmskvar)
            else
              write(kunit, 9743)
            endif
          else

            write(kunit, 9721)

          endif

c          if ((iabs(itarg) .eq. 2) .or. (itarg .eq. 5)) then
          if (mlon .ne. -999) then

            write(kunit, 9750) 
            write(kunit, 9760) mlon, alon, dellon
            write(kunit, 9770) mlat, alat, dellat
 
          else
 
            write(kunit, 9751)

          endif

        write(kunit, 9990)

        write(kunit, '(1x)')
        write(kunit, '(1x)')
c

 9000 format(t2,'NEW RESULTS FROM PROGRAM RESOLVE')
 9001 format(t2,' 1. First and Second Order Statistics')
c
 9010 format(t2,' 2. Analysis ', a20)
 9020 format(t2,' 3. Analysis ', a20)
 9100 format(t2,' 4. ')
 9110 format(t2,' 5. Total number of data set comparisons: ', i3 )
 9200 format(t2,' 6. ')
c                              full path
 9210 format(t2,' 7. Test data set (1)     : ', a100 )
c                             (e.g., bmrc-a)
 9215 format(t2,' 8.    Data-set I.D.: ', a16)
c                           "source" read in along with i.d.
 9216 format(t2,' 9.    Source: unavailable')
 9220 format(t2,'10.    Variable name: ', a16 )
 9221 format(t2,'11.    Original units: ', a24 )
 9225 format(t2,'12.    Level requested and extracted: ', 1pe12.4,
     &                    1pe12.4, 1x, a15)
 9226 format(t2,'12.    Single level field analyzed')
 9230 format(t2,'13.    Times selected: ', i7,  ' to ', i7 )
 9235 format(t2,'14.    Calendar dates selected:  ', i4, '/', i4, 
     &        ' - ', i2, '/', i4)  
 9236 format(t2,'14.    Calendar dates unavailable or inapplicable')
 9240 format(t2,'15.    Grid type: unavailable' )
 9245 format(t2,'16.    Original (lon lat time) dimensions:  ', 3i7 )   
 9250 format(t2,'17.    Dimensions before final regridding:  ', 3i7 )
 9255 format(t2,'18.    Final dimensions                  :  ', 3i7 )
 9260 format(t2,'19.    Scale-factor applied: ', 1pe13.5 )
 9265 format(t2,'20.    Offset factor added: ', 1pe13.5 )
 9270 format(t2,'21.    Geography mask from: ', a120 )
 9271 format(t2,'21.    No geography mask applied' / t2, '22.')
 9275 format(t2,'22.    Name of masking variable: ', a16)
     
 9300 format(t2,'23. ')
c                              full path
 9310 format(t2,'24. Reference data set (2)     : ', a100 )
c                             (e.g., bmrc-a)
 9315 format(t2,'25.    Data-set I.D.: ', a16)
c                           "source" read in along with i.d.
 9316 format(t2,'26.    Source: unavailable')
 9320 format(t2,'27.    Variable name: ', a16 )
 9321 format(t2,'28.    Original units: ', a24 )
 9325 format(t2,'29.    Level requested and extracted: ', 1pe12.4,
     &                    1pe12.4, 1x, a15)
 9326 format(t2,'30.    Single level field analyzed')
 9330 format(t2,'30.    Times selected: ', i7,  ' to ', i7 )
 9335 format(t2,'31.    Calendar dates selected:  ', i4, '/', i4, 
     &        ' - ', i2, '/', i4)  
 9336 format(t2,'31.    Calendar dates unavailable or inapplicable')
 9340 format(t2,'32.    Grid type: unavailable' )
 9345 format(t2,'33.    Original (lon lat time) dimensions:  ', 3i7 )   
 9350 format(t2,'34.    Dimensions before final regridding:  ', 3i7 )
 9355 format(t2,'35.    Final dimensions                  :  ', 3i7 )
 9360 format(t2,'36.    Scale-factor applied: ', 1pe13.5 )
 9365 format(t2,'37.    Offset factor added: ', 1pe13.5 )    
 9370 format(t2,'38.    Geography mask from: ', a120 )
 9371 format(t2,'38.    No geography mask applied' / t2, '39.')
 9375 format(t2,'39.    Name of masking variable: ', a16)

 9400 format(t2,'40. ')
 9410 format(t2,'41. Selected latitude boundaries of analysis ',
     &'domain:  ', f6.1, ' to ', f6.1 )
 9420 format(t2,'42. Selected longitude boundaries of analysis ',
     &'domain: ', f6.1, ' to ', f6.1 )

 9500 format(t2,'43. ')
 9510 format(t2,'44. Minimum number of years required for analysis ',
     &          '(unless minimum fraction exceeded): ', i6)
 9511 format(t2,
     &'45. Minimum fraction of years required for analysis ',
     &          '(unless minimum number of years exceeded): ', f10.6)
 9520 format(t2,'46. All geographical regions included ' )
 9521 format(t2,'46. All regions masked except: ', a120 )
 9530 format(t2,'47. Integer describing input data: unavailable')

 9540 format(t2,'48. Only climatology analyzed')
 9541 format(t2,
     &   '48. Both climatology and interannual anomalies analyzed')

 9545 format(t2,'49. Full space-time analysis skipped')
 9546 format(t2,'49. Full space-time monthly analysis performed')
 9547 format(t2,'49. Full space-time seasonal analysis performed')
 9548 format(t2,'49. Full space-time monthly and seasonal analyses ',
     &        'performed')

 9550 format(t2,'50. Analysis of individual months skipped')
 9551 format(t2,'50. Analysis of month ', i2, ' performed')
 9552 format(t2,'50. Analysis of January and July performed')
 9553 format(t2,'50. Analysis of each individual month performed')

 9560 format(t2,'51. Analysis of individual seasons skipped')
 9561 format(t2,'51. Analysis of season ', i2, ' performed')
 9562 format(t2,'51. Analysis of DJF and JJA performed')
 9563 format(t2,'51. Analysis of each individual season performed')

 9570 format(t2,'52. Analysis of annual mean skipped')
 9571 format(t2,'52. Analysis of annual mean performed')



 9600 format(t2,'53. ')
 9610 format(t2,'54. External data mask data set: ', a100 )
 9611 format(t2,'54. No external data mask was used.' /
     &      t2, '55.'/ t2, '56.'/ t2, '57.'/ t2, '58.'/
     &      t2, '59.'/ t2, '60.'/ t2, '61.'/ t2, '62.'/ 
     &      t2, '63.'/ t2, '64.'/ t2, '65.')
 9615 format(t2,'55.    Data-set I.D.: ', a16)
 9616 format(t2,'56.    Source: unavailable')
 9620 format(t2,'57.    Variable name: ', a16 )
 9625 format(t2,'58.   Level requested and extracted: ', 1pe12.4,
     &                    1pe12.4, 1x, a15)
 9626 format(t2,'58.    Single level field analyzed')
 9630 format(t2,'59.    Times selected: ', i7,  ' to ', i7 )
 9635 format(t2,'60.    Calendar dates selected:  ', i4, '/', i4, 
     &        ' - ', i2, '/', i4)  
 9636 format(t2,'60.    Calendar dates unavailable or inapplicable')
 9640 format(t2,'61.    Grid type: unavailable' )
 9645 format(t2,'62.    Original (lon lat time) dimensions:  ', 3i7 )  
 9650 format(t2,'63.    Dimensions after regridding:  ', 3i7 )
 9670 format(t2,'64.    Geography mask from: ', a120 )
 9671 format(t2,'64.    No geography mask applied' / t2, '65.')
 9675 format(t2,'65.    Name of masking variable: ', a16)

 9701 format(t2,'66. ')
 9710 format(t2, '67. Regridding option selected: unavailable')
 9720 format(t2, '68. Target grid obtained from: ', a100)
 9721 format(t2, '68. No stored target grid was used.' /
     &       t2, '69. ' / t2, '70. ', / , t2, '71.',/,t2, '72.',/
     &       t2, '73. ' / t2, '74. ' )
 9725 format(t2,'69.    Data-set I.D.: ', a16)
 9726 format(t2,'70.    Target grid source: unavailable')
 9730 format(t2, '71.   Target grid variable name: ', a16)
 9740 format(t2, '72.   Target grid type: unavailable')

 9742 format(t2,'73.    Geography mask from: ', a120 )
 9743 format(t2,'73.    No geography mask applied' / t2, '74.')
 9744 format(t2,'74.    Name of masking variable: ', a16)

 9750 format(t2, 
     &   '75. Target grid generated from user-defined parameters')
 9751 format(t2, '75. No user-defined target grid was used.', /,
     &       t2, '76. ', /, t2, '77. ')
 9760 format(t2, '76.   User-defined grid for longitude: ', i4, 4x, 
     &        f10.6, 4x, f10.6)
 9770 format(t2, '77.   User-defined grid for latitude:  ', i4, 4x, 
     &        f10.6, 4x, f10.6)
 9990 format(t2, 'LAST HEADER LINE')
c



        write(89,10)
        write(89,11)
        write(89,12)
        write(89,13)
        write(89,14)
        write(89,15)
        write(89,16)
        write(89,17)
        write(89,18)
        write(89,19)
        write(89,20)
        write(89,21)
        write(89,22)
        write(89,23)
        write(89,24)
        write(89,25)
        write(89,26)
        write(89,27)
        write(89,28)
        write(89,29)
        write(89,30)
        write(89,31)
        write(89,32)
        write(89,33)
        write(89,34)
        write(89,35)
        write(89,36)
        write(89,37)
        write(89,38)

        write(89, 7)

      endif

      if (nn .gt. 0) then

        n = n + 1

        do 100 i=1,28

          wtl(i,n) = wts(i)
          avgal(i,n) = avga(i)
          avgbl(i,n) = avgb(i)
          rmsl(i,n) = rms(i)
          correll(i,n) = correl(i)

          if (vara(28) .gt. 0.) then
            pvara(i,n) = 100.*vara(i)/vara(28)
            sda(i,n) = sqrt(vara(i))
          endif
          
          if (varb(28) .gt. 0.) then
            pvarb(i,n) = 100.*varb(i)/varb(28)
            pvar1a(i,n) = 100.*vara(i)/varb(28)
            sdb(i,n) = sqrt(varb(i))
          endif

          if (rms(28) .gt. 0.) then
            pmsdiff(i,n) = 100.*rms(i)*rms(i)/(rms(28)*rms(28))
          endif

          if ((sda(i,n) .gt. 0.0) .and. (sdb(i,n) .gt. 0.0)) then
            rank(i,n) = 100.*rms(i)/sqrt(sda(i,n)*sdb(i,n))
          endif

 100    continue
        IF (((mons21-mons20).EQ.11).OR.(pvarb(7,n).EQ.0.)) THEN
          DO i=7,13
           rank(i,n)=0.
           sda(i,n)=0.
           sdb(i,n)=0.
           rms(i)=0.
           rmsl(i,n)=0.
           correl(i)=0.
           correll(i,n)=0.
           pvara(i,n)=0.
           pvar1a(i,n)=0.
           pvarb(i,n)=0.
           pmsdiff(i,n)=0.
          ENDDO
          DO i=21,27
           rank(i,n)=0.
           sda(i,n)=0.
           sdb(i,n)=0.
           rms(i)=0.
           rmsl(i,n)=0.
           correl(i)=0.
           correll(i,n)=0.
           pvara(i,n)=0.
           pvar1a(i,n)=0.
           pvarb(i,n)=0.
           pmsdiff(i,n)=0.
          ENDDO
        ENDIF


        amodel(n) = modl1
          
C       if (n .le. 1) then
C        write(*,7)
C        write(*,'(" time domain: ", a16)') tablname(icall) 
C        write(*,7) 
C        write(*,1) vars1, modl1, pthd1
C        write(*,1) vars2, modl2, pthd2
C        write(*,2) region 
C        write(*,3)  alon0, alon1, alat0, alat1
C        write(*,4) wts(1), avga(1), avgb(1)
C        write(*,8)  (i, wts(i), avga(i), avgb(i), i=2,28)
C        write(*,5)
C       endif 

        write(89,7) 
        write(89,'(" time domain: ", a16)') tablname(icall) 
c        write(89,1) vars1, modl1, pthd1
c        write(89,1) vars2, modl2, pthd2
c        write(89,2) region 
c        write(89,3)  alon0, alon1, alat0, alat1
        write(89,4) wts(1), avga(1), avgb(1)
        write(89,5) 

C        write(*,6) (i, rank(i,n), sda(i,n), sdb(i,n), rms(i), 
C     +        correl(i), pvara(i,n), pvar1a(i,n), pvarb(i,n), 
C     +        pmsdiff(i,n), i=2,28)
        write(89,6) (i, rank(i,n), sda(i,n), sdb(i,n), rms(i), 
     +        correl(i), pvara(i,n), pvar1a(i,n), pvarb(i,n), 
     +        pmsdiff(i,n), i=2,28)

    7 format(1x)
c    1 format( a16, a16, a70)
c    2 format(a80)
c    3 format(4f10.2)
    4 format( / 'spatial and temporal mean: ' /
     +  '       weight', ' test: avg1  ', ' ref.: avg2' /
     +    3x, f10.6, 2(1pe13.5))
c    8 format((i3, 0pf10.6, 2(1pe13.5)))
    5 format(/ 5x, ' rank ', '   s.d. 1  ', '   s.d. 2 ', 'pattern rms',
     +       ' correl ', ' %var 1 ', ' %var 1a ', ' %var 2 ',  
     +       '%ms-diff')
    6 format(6(i2, 0pf8.3, 3(1pe11.3), 0pf8.4, 4(0pf8.3) /) /
     +       6(i2, 0pf8.3, 3(1pe11.3), 0pf8.4, 4(0pf8.3) /) /
     +       7(i2, 0pf8.3, 3(1pe11.3), 0pf8.4, 4(0pf8.3) /) /
     +       6(i2, 0pf8.3, 3(1pe11.3), 0pf8.4, 4(0pf8.3) /) /
     +       2(i2, 0pf8.3, 3(1pe11.3), 0pf8.4, 4(0pf8.3) /))

   10 format(5x, 'Component Key:')
   11 format('1 = spatial and temporal mean')
   12 format('2 = climatological, spatial-mean, seasonal cycle')
   13 format('3 = climatological, time-mean, zonal-average')
   14 format('4 = climatological, seasonal cycle of zonal mean'/
     +       '      (with spatial-mean seasonal cycle removed)')
   15 format('5 = climatological, time-mean deviations from ',
     +    'the zonal mean')
   16 format('6 = climatological, seasonal cycle of deviations from ',
     +    'the zonal mean')
   17 format('7 = interannual variations')
   18 format('8 = interannual variations of spatial and annual mean')
   19 format('9 = interannual variations of spatial-mean, seasonal ',
     +     'cycle')
   20 format('10 = interannual variations of annual-mean, ' ,
     +     'zonal-average (with spatial mean removed)')
   21 format('11 = interannual variations of seasonal cycle of zonal ' ,
     +     'mean'/ 
     +       '      (with spatial-mean seasonal cycle removed)')
   22 format('12 = interannual variations of annual-mean deviations ' ,
     +    'from the zonal mean')
   23 format('13 = interannual variations of seasonal cycle of ' ,
     +     'deviations from the zonal mean')
   24 format('14 = 3 + 5  (climatological time-mean component)')
   25 format('15 = 2 + 4  (climatological zonal mean seasonal cycle ',
     +        'component)')
   26 format('16 = 2 + 4 + 6 (climatological seasonal cycle component')
   27 format('17 = 2 + 3 + 4  (climatological zonal-mean component')
   28 format('18 = 5 + 6  (climatological, deviations from the zonal ', 
     +      'mean)')
   29 format('19 = 2 + 3 + 4 + 5 + 6  (climatological component)')
   30 format('20 = 2 + 3 + 4 + 5 + 6 + 7 (total space-time field)')
   31 format('21 = 9 + 11 (interannual variations of seasonal cycle of',
     +        ' zonal mean)')
   32 format('22 = 9 + 11 + 13  (interannual variations of seasonal ',
     +        'cycle')
   33 format('23 = 8 + 10  (interannual variations of annual mean, ',
     +     'zonal average)')
   34 format('24 = 8 + 10 + 12 (interannual variations of annual mean)')
   35 format('25 = 8 + 9 + 10 + 11 (interannual variations of zonal ',
     +      'mean)')
   36 format('26 = 12 + 13 (interannual variations of deviations from ',
     +     'the zonal mean)')
   37 format('27 = same as 7')
   38 format('28 = same as 20')
 
c
c-------------------------------------------------------------------------------
c     ** 11.1 **
c
c     Format Statements.
c-------------------------------------------------------------------------------
c
c
c
c


      elseif (n .gt. 1) then
c       put models in order: best to worst

        ipt(1) = 1
        do 300 m = 2,n
          i = m
          do 200 j=1,m-1

            if (rank(28,m) .lt. rank(28,j)) then
              ipt(j) = ipt(j) + 1
              i = i - 1
            endif

  200     continue

            ipt(m) = i

  300   continue

        do 350 m=1,n
          k = ipt(m)
          alist(k) = amodel(m)
  350   continue

c        calculate mean statistics for all models

        do 490 i=1,28
            awt(i) = 0.0
            aavga(i) = 0.0
            aavgb(i) = 0.0
            asda(i) = 0.0
            asdb(i) = 0.0
            arms(i) = 0.0
            acorrel(i) = 0.0 
            apvara(i) = 0.0
            apvar1a(i) = 0.0
            apvarb(i) = 0.0
            apmsdiff(i) = 0.0
            arank(i) = 0.0
  490   continue

 
        do 500 i=1,28

          do 400 m=1,n

            awt(i) = awt(i) + wtl(i,m)/n
            aavga(i) = aavga(i) + avgal(i,m)/n
            aavgb(i) = aavgb(i) + avgbl(i,m)/n
            asda(i) = asda(i) + sda(i,m)*sda(i,m)/n
            asdb(i) = asdb(i) + sdb(i,m)*sdb(i,m)/n
            arms(i) = arms(i) + rmsl(i,m)*rmsl(i,m)/n
            apvara(i) = apvara(i) + pvara(i,m)/n
            apvar1a(i) = apvar1a(i) + pvar1a(i,m)/n
            apvarb(i) = apvarb(i) + pvarb(i,m)/n
            apmsdiff(i) = apmsdiff(i) + pmsdiff(i,m)/n

  400     continue

          if (asda(i) .gt. 0.) asda(i) = sqrt(asda(i))
          if (asdb(i) .gt. 0.) asdb(i) = sqrt(asdb(i))
          if (arms(i) .gt. 0.) arms(i) = sqrt(arms(i))

          if ((asda(i) .gt. 0.) .and. (asdb(i) .gt. 0.)) then
            acorrel(i) = -(arms(i)*arms(i) - asda(i)*asda(i) - 
     +                     asdb(i)*asdb(i))/(2.*asda(i)*asdb(i))
            arank(i) =  100.*arms(i)/sqrt(asda(i)*asdb(i))
          endif

  500   continue
 
C        write(*,7) 
C        write(*,9) vars1, (alist(i), i=1,15)
C        write(*,9) vars2, (alist(i), i=16,30)
C        write(*,2) region 
C        write(*,3)  alon0, alon1, alat0, alat1
C        write(*,8)  (i, awt(i), aavga(i), aavgb(i), i=2,28)
C        write(*,4)  awt(1), aavga(1), aavgb(1)
C        write(*,5) 

        write(89,7) 
        write(89,9) vars1, (alist(i), i=1,15)
        write(89,9) vars2, (alist(i), i=16,30)
c        write(89,2) region 
c        write(89,3) alon0, alon1, alat0, alat1
        write(89,4) awt(1), aavga(1), aavgb(1)
        write(89,5) 

C        write(*,6) (i, arank(i), asda(i), asdb(i), arms(i), 
C     +        acorrel(i), apvara(i), apvar1a(i), apvarb(i), apmsdiff(i),
C     +        i=2,28)
        write(89,6) (i, arank(i), asda(i), asdb(i), arms(i), 
     +        acorrel(i), apvara(i), apvar1a(i), apvarb(i), apmsdiff(i),
     +        i=2,28)

    9   format (16a5)

        close(89)

      endif

      return 
      end


c ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c
c     logical function caseindp
c     -------
c
c     Description:
c     -----------
c    This function tests for character string equivalence
c
c     Usage:
c     ------
c
c      if (caseindp(a, b)) then
c
c     ------
c     Date: 9/17/94
c     ----
c
c ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

      CHARACTER*(*) function strip(txt)

      IMPLICIT none
      CHARACTER*(*) txt
      INTEGER i,j,j1,j2
c      PRINT*, 'Stripping:',txt,'---'
      j=len(txt)
      j1=0
      do 10 i=1,j
        if (txt(i:i) .ne. ' ') go to 20
        j1 = j1 + 1
   10 continue
   20 continue
      j2=j1
      do 30 i=j1,j
        if (txt(i:i) .eq. ' ') go to 40
        j2 = j2 + 1
   30 continue
   40 CONTINUE
c      PRINT*, j1,j1+1,j2
      strip=txt(j1+1:j2)//' '
C      PRINT*, 'Returning FORM strip'
      return
      end

      logical function caseindp(a, b)

c    This function tests for character string equivalence
c
c    compare what follows any leading blanks
c    check for equivalence of strings ignoring case.
c    only check through the last character of the shorter string 
c    (where "shorter" is computed after removing leading blanks)
c    The only exception to this is if 1 string is null or completely
c    blank; then equivalence requires both strings to be null or
c    completely blank.   

c      implicit none
      character*(*) a, b
      character*1 c, d
      integer i, j, k, m, n, j1, k1, ii, j2, k2

c     look for leading blanks and neglect

      j = len(a)
      k = len(b)

c     check whether both strings are length zero

      if ((j .eq. 0) .or. (k .eq. 0)) then
        if (j .eq. k) then
          caseindp = .true.
        else
          caseindp = .false.
        endif
        return
      endif


      j1 = 0
      do 10 i=1,j
        if (a(i:i) .ne. ' ') go to 20
        j1 = j1 + 1
   10 continue
   20 continue

      k1 = 0
      do 30 i=1,k
        if (b(i:i) .ne. ' ') go to 40
        k1 = k1 + 1
   30 continue
   40 continue

c   check whether either string is completely blank

      if ((j1 .eq. j) .or. (k1 .eq. k)) then
        if ((j1 .eq. j) .and. (k1 .eq. k)) then
          caseindp = .true.
        else
          caseindp = .false.
        endif
        return
      endif


c   look for trailing blanks and neglect
      
      j2 = j
      do 50 i=1,j
        if (a(j-i+1:j-i+1) .ne. ' ') go to 60
        j2 = j2 - 1
   50 continue
   60 continue

      k2 = k
      do 70 i=1,k
        if (b(k-i+1:k-i+1) .ne. ' ') go to 80
        k2 = k2 - 1
   70 continue
   80 continue

      j = j2 - j1
      k = k2 - k1
      ii = min0(j, k)
 
      do 100 i=1,ii
        m = i + j1
        n = i + k1
        c = a(m:m)
        if (lge(c,'A') .and. lle(c,'Z')) c = char(ichar(c)+32)
        d = b(n:n)
        if (lge(d,'A') .and. lle(d,'Z')) d = char(ichar(d)+32)
        if (c .ne. d) then
           caseindp = .false.
           return
        endif
  100 continue

      caseindp = .true. 
      return
      end


c **********************************************************************
c ----------------------------------------------------------------------

      subroutine mksubset(icall, icalndr, idoclim, lenreg, lentim,  
     &           inptfreq, mosea1, testmsk, testdata, refmsk, refdata,
     &           a1, awt1, a2, awt2, lentim2,nmx)

c ----------------------------------------------------------------------
c **********************************************************************

            
c        ido(1) = Jan
c        ido(2) = Feb 
c          .
c          .
c          .
c        ido(12) = Dec

c        ido(13) = DJF
c        ido(14) = MAM
c        ido(15) = JJA
c        ido(16) = SON

c        ido(17) = annual

c        ido(18) = space-time (seasonal)
c        ido(19) = space-time (monthly)

c
c      omit seasons/annual means with if any contributing months are
c          missing.
     
      implicit NONE
      INTEGER lentim2,nmx
      integer icall, lenreg, lentim, inptfreq, mosea1, icalndr, idoclim
      real testmsk(lenreg,lentim), testdata(lenreg,lentim),
     &     refmsk(lenreg,lentim), refdata(lenreg,lentim),
     &     awt1(lenreg,nmx), a1(lenreg,nmx),
     &     awt2(lenreg,nmx), a2(lenreg,nmx)
     
c     local variables

      integer j, k, m, n, m1, k1, k2, k3, mm, mn1, mn2, kkk(12), 
     &   im, is, iw, i, i1, i2, i3, is0, iii
      real ww, wttime(12,2,2)
      data wttime/ 
     &   31., 28., 31., 30., 31., 30., 31., 31., 30., 31., 30., 31,  
     &   90., 92., 92., 91., 8*0.,
     &   12*30., 4*90., 8*0. /

      if (icall .le. 12) then
c        extract individual month

          if (inptfreq .ne. 12) then
            print*, 'You cannot compute statistics for individual '
            print*, 'months because input data is not monthly.'
            stop
          endif


          m1 = icall - mosea1 + 1
          if (m1 .le. 0) m1 = m1 + 12

          n = 0
          do m=m1,lentim,12
            n = n + 1
            do j=1,lenreg
              a1(j,n) = testdata(j,m)
              a2(j,n) = refdata(j,m)
              awt1(j,n) = testmsk(j,m)
              awt2(j,n) = refmsk(j,m)
            enddo
          enddo

          lentim = n

      elseif (icall .le. 16) then
c        extract individual season

        if (inptfreq .eq. 12) then
c               compute seasonal mean, weighted by length of months

            is0 = icall-12

c           first month from input to contribute:
            m1 = 3*(is0-1)
            if (m1 .eq. 0) m1 = 12
            if (idoclim .eq. 0) then
              mm = lentim-2
              if (mosea1 .le. m1) then
                m1 = m1 - mosea1 + 1
              else
                m1 = m1 - mosea1 + 13
              endif
              i2 = 1
              i3 = 2
            else
              mm = m1
              if (m1 .eq. 12) then
                i2 = -11
                i3 = -10
              else
                i2 = 1
                i3 = 2
              endif
            endif
        
c             calendar month extracted
            k1 = 3*(is0-1)
            k2 = k1 + 1
            k3 = k1 + 2
            if (k1 .eq. 0) k1 = 12

            ww = wttime(k1,1,icalndr) + wttime(k2,1,icalndr) +
     &                 wttime(k3,1,icalndr)

            n = 0

            do i=m1,mm,12
              n = n + 1
              do j=1,lenreg
             
                if (testmsk(j,i)*testmsk(j,i+i2)*testmsk(j,i+i3) .ne. 
     &               0.0) then
                  awt1(j,n) = (testmsk(j,i)*wttime(k1,1,icalndr) + 
     &                         testmsk(j,i+i2)*wttime(k2,1,icalndr) + 
     &                         testmsk(j,i+i3)*wttime(k3,1,icalndr))/ww
                  a1(j,n) = (testdata(j,i)*wttime(k1,1,icalndr) + 
     &                       testdata(j,i+i2)*wttime(k2,1,icalndr) +
     &                       testdata(j,i+i3)*wttime(k3,1,icalndr))/ww
                else
                  awt1(j,n) = 0.0
                  a1(j,n) = 0.0
                endif
                
                if (refmsk(j,i)*refmsk(j,i+i2)*refmsk(j,i+i3) .ne. 
     &               0.0) then
                  awt2(j,n) = (refmsk(j,i)*wttime(k1,1,icalndr) + 
     &                         refmsk(j,i+i2)*wttime(k2,1,icalndr) + 
     &                         refmsk(j,i+i3)*wttime(k3,1,icalndr))/ww
                  a2(j,n) = (refdata(j,i)*wttime(k1,1,icalndr) + 
     &                       refdata(j,i+i2)*wttime(k2,1,icalndr) +
     &                       refdata(j,i+i3)*wttime(k3,1,icalndr))/ww
                else
                  awt2(j,n) = 0.0
                  a2(j,n) = 0.0
                endif
                
              enddo

            enddo

        else

c         extract season of interest

            if (inptfreq .ne. 4) then
              print*, 'You cannot compute statistics for individual '
              print*, 'seasons because input data is not seasonal '
              print*, 'or monthly.'
              stop
            endif

            m1 = icall - 12 - mosea1 + 1
            if (m1 .le. 0) m1 = m1 + 4

            n = 0

            do m=m1,lentim,4
              n = n + 1
              do j=1,lenreg
                a1(j,n) = testdata(j,m)
                a2(j,n) = refdata(j,m)
                awt1(j,n) = testmsk(j,m)
                awt2(j,n) = refmsk(j,m)
              enddo
            enddo

        endif

        lentim = n

      elseif (icall .eq. 17) then
c        extract annual data
        if (inptfreq .eq. 12) then
          iw=1
        elseif (inptfreq .eq. 4) then
          iw=2
        endif

        if ((inptfreq .eq. 12) .or. (inptfreq .eq. 4)) then
c         compute annual mean from monthly or seasonal data

c           first month/season from input to contribute:
        
c             calendar month extracted

          ww = 0.0
          do k=1,inptfreq
            kkk(k) = k + mosea1 - 1
            if (kkk(k) .gt. inptfreq) kkk(k) = kkk(k) - inptfreq
            ww = ww + wttime(k,iw,icalndr)
          enddo

          n = 0
          iii = lentim+1-inptfreq
          do i=1,iii,inptfreq
            n = n + 1
            do j=1,lenreg
             
              a1(j,n) = 0.0
              a2(j,n) = 0.0
              awt1(j,n) = 0.0
              awt2(j,n) = 0.0

              mn1 = 0
              mn2 = 0
              im = i - 1

              do m=1,inptfreq
                k = kkk(m)
                if (testmsk(j,im+m) .ne. 0.0) then
                  mn1 = mn1 + 1
                  awt1(j,n) = awt1(j,n) +
     &                            testmsk(j,im+m)*wttime(k,iw,icalndr) 
                  a1(j,n) = a1(j,n) +
     &                            testdata(j,im+m)*wttime(k,iw,icalndr)
                endif
                if (refmsk(j,im+m) .ne. 0.0) then
                  mn2 = mn2 + 1
                  awt2(j,n) = awt2(j,n) + 
     &                             refmsk(j,im+m)*wttime(k,iw,icalndr) 
                  a2(j,n) = a2(j,n) +
     &                             refdata(j,im+m)*wttime(k,iw,icalndr)
                endif
              enddo

              if (mn1 .eq. inptfreq) then
                awt1(j,n) = awt1(j,n)/ww
                a1(j,n) = a1(j,n)/ww
              else
                awt1(j,n) = 0.0
                a1(j,n) = 0.0
              endif
                
              if (mn2 .eq. inptfreq) then
                awt2(j,n) = awt2(j,n)/ww
                a2(j,n) = a2(j,n)/ww
              else
                awt2(j,n) = 0.0
                a2(j,n) = 0.0
              endif
                                
            enddo

          enddo

          lentim = n

        else
c         copy annual data

            if (inptfreq .ne. 1) then
              print*, 'You cannot compute statistics for individual '
              print*, 'years because input data is not annual,  '
              print*, 'seasonal or monthly.'
              stop
            endif

          do n=1,lentim
            do j=1,lenreg
              a1(j,n) = testdata(j,n)
              a2(j,n) = refdata(j,n)
              awt1(j,n) = testmsk(j,n)
              awt2(j,n) = refmsk(j,n)
            enddo
          enddo

        endif

      elseif (icall .eq. 18) then
c        extract all 4 seasons

        if (inptfreq .eq. 12) then
c           compute seasonal means, weighted by length of months

c         find out which will be season immediately preceeding first
c              season to be treated

          if (idoclim .eq. 0) then
            is0 = (mosea1+2)/3 
            m1 = 3 - mod((mosea1+2), 3)
            mosea1 = is0 + 1
            if (mosea1 .gt. 4) mosea1 = 1
          else
            is0 = 0
            m1 = 0
          endif

          n = 0

          do i=m1, lentim-2, 3

            n = n + 1
            is0 = is0 + 1
            if (is0 .gt. 4) is0 = 1

            i1 = 0
            i2 = 1
            i3 = 2
            if ((idoclim .eq. 1) .and. (i .eq. 0)) i1 = 12
        
c             calendar month extracted
            k1 = 3*(is0-1)
            k2 = k1 + 1
            k3 = k1 + 2
            if (k1 .eq. 0) k1 = 12

            ww = wttime(k1,1,icalndr) + wttime(k2,1,icalndr) + 
     &                wttime(k3,1,icalndr)

              do j=1,lenreg
             
                if (testmsk(j,i+i1)*testmsk(j,i+i2)*testmsk(j,i+i3) .ne.
     &               0.0) then
                  awt1(j,n) = (testmsk(j,i+i1)*wttime(k1,1,icalndr) + 
     &                         testmsk(j,i+i2)*wttime(k2,1,icalndr) + 
     &                         testmsk(j,i+i3)*wttime(k3,1,icalndr))/ww
                  a1(j,n) = (testdata(j,i+i1)*wttime(k1,1,icalndr) + 
     &                       testdata(j,i+i2)*wttime(k2,1,icalndr) +
     &                       testdata(j,i+i3)*wttime(k3,1,icalndr))/ww
                else
                  awt1(j,n) = 0.0
                  a1(j,n) = 0.0
                endif
                
                if (refmsk(j,i+i1)*refmsk(j,i+i2)*refmsk(j,i+i3) .ne. 
     &               0.0) then
                  awt2(j,n) = (refmsk(j,i+i1)*wttime(k1,1,icalndr) + 
     &                         refmsk(j,i+i2)*wttime(k2,1,icalndr) + 
     &                         refmsk(j,i+i3)*wttime(k3,1,icalndr))/ww
                  a2(j,n) = (refdata(j,i+i1)*wttime(k1,1,icalndr) + 
     &                       refdata(j,i+i2)*wttime(k2,1,icalndr) +
     &                       refdata(j,i+i3)*wttime(k3,1,icalndr))/ww
                else
                  awt2(j,n) = 0.0
                  a2(j,n) = 0.0
                endif
                
              enddo


          enddo

          lentim = n

        else

            if (inptfreq .ne. 4) then
              print*, 'You cannot compute statistics for individual '
              print*, 'seasons because input data is not seasonal '
              print*, 'or monthly.'
              stop
            endif

c         copy seasons from seasonal data

          do is=1,lentim

              do j=1,lenreg
                a1(j,is) = testdata(j,is)
                a2(j,is) = refdata(j,is)
                awt1(j,is) = testmsk(j,is)
                awt2(j,is) = refmsk(j,is)
              enddo

          enddo

        endif

      endif

      lentim2=lentim
      return
      end
