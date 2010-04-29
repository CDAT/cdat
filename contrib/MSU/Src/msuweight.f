c-------------------------------------------------------------------------------
c-------------------------------------------------------------------------------
c     SUBROUTINE MSUWEIGHT3.F
c
c     Date:    24-JAN-2005.
c     Author:  Ben Santer and Charles Doutriaux, LLNL, Livermore.
c     Version: # 1.0 / Single precision.
c
c
c
c     This subroutine processes temperature data at discrete pressure levels,
c     and computes from this data an "equivalent MSU" temperature. The MSU
c     weighting function is assumed to be in pressure coordinates (with 5 mb
c     vertical resolution). 
c
c
c
      subroutine msuweight3(nhp, nhm, indx, heightp, heightm, weightm,
     &wtnorm1, ta1, ta2, bogus, critwt, sumw)
c
c
c
      implicit real (o-z, a-h)
c
c     ** INPUT **
c     integer nhp                  No. of pressure layers (target data)
c     integer nhm                  No. of pressure layers (MSU wt funct)
c     integer indx                 Index .gt. 1 = no diagnostics          
c     dimension heightp(nhp)       Pressure coordinates (target data)
c     dimension heightm(nhm)       Pressure coordinates (MSU wt funct)
c     dimension weightm(nhm)       Input MSU weighting function        
c     dimension ta1(nhp)           Input temperature data
c     real bogus                   Missing data code
c     real critwt                  Threshold fraction wts 
c
c     ** OUTPUT **
c     dimension wtnorm1(nhp)       Normalized weight (target data)
c     real ta2                     Output equivalent MSU temperature
c     real sumw                    Sum of valid weights
c
c     ** INTERNAL **
c     dimension boundp1(250)       Lower edge of pressure layer (target data)
c     dimension boundp2(250)       Upper edge of pressure layer (target data)
c     dimension boundm1(250)       Lower edge of pressure layer (MSU wt funct)
c     dimension boundm2(250)       Upper edge of pressure layer (MSU wt funct)
c     dimension pwt(250)           Pressure weights (target data)
c     dimension totwt(250)         MSU weight for layer of target data
c
c
c 
      integer nhp, nhm, indx
      dimension heightp(nhp), heightm(nhm), weightm(nhm)
      dimension ta1(nhp), wtnorm1(nhp)
      dimension boundp1(250), boundp2(250), boundm1(250), boundm2(250)
      dimension pwt(250), totwt(250)
      real bogus, critwt, ta2
c
c
c
c-------------------------------------------------------------------------------
c-------------------------------------------------------------------------------
c
c
c
c-------------------------------------------------------------------------------
c     ** 1.0 **
c
c     First compute weights and boundaries for pressure levels of target data
c     set.
c-------------------------------------------------------------------------------
c
c
c
        if (indx .le. 1) then
           print*, 'nhp, nhm, indx',nhp, nhm, indx
        endif

      edge2 = heightp(1) - ((heightp(2) - heightp(1)) / 2.0)
c
      do 100 k = 2, nhp
        edge1 = edge2
        edge2 = heightp(k - 1) + ((heightp(k) - heightp(k - 1)) / 2.0)
        pwt(k - 1) = edge2 - edge1
c
        if (indx .le. 1) then
          write(*, 101) k - 1, edge1, edge2, heightp(k - 1), pwt(k - 1) 
          print*, k,ta1(k)       
        end if
c
        boundp1(k - 1) = edge1
        boundp2(k - 1) = edge2
  100 continue
c
      edge1 = edge2
      edge2 = heightp(nhp) + ((heightp(nhp) - heightp(nhp - 1)) / 2.0)
      pwt(nhp) = edge2 - edge1
c
      if (indx .le. 1) then
        write(*, 101) nhp, edge1, edge2, heightp(nhp), pwt(nhp)
      end if
c
      boundp1(nhp) = edge1
      boundp2(nhp) = edge2
c
  101 format('IN MSUWEIGHT3: DATA LAYER', i4, 2x, 'EDGE1 = ', f8.2, 2x,
     &'EDGE2 = ', f8.2, 2x, 'HEIGHT = ', f8.2, 2x, 'PWT = ', f8.2)
c
c
c
c-------------------------------------------------------------------------------
c     ** 1.1 **
c
c     Compute boundaries for MSU pressure levels (no pressure weight is 
c     required, since layers are of equal thickness).
c-------------------------------------------------------------------------------
c
c
c
      edge2 = heightm(1) - ((heightm(2) - heightm(1)) / 2.0)
c
      do 110 k = 2, nhm
        edge1 = edge2
        edge2 = heightm(k - 1) + ((heightm(k) - heightm(k - 1)) / 2.0)
c
        if (indx .le. 1) then
          write(*, 111) k - 1, edge1, edge2, heightm(k - 1), 
     &    weightm(k - 1)
        end if
c
        boundm1(k - 1) = edge1
        boundm2(k - 1) = edge2
  110 continue
c
      edge1 = edge2
      edge2 = heightm(nhm) + ((heightm(nhm) - heightm(nhm - 1)) / 2.0)
c
      if (indx .le. 1) then
        write(*, 111) nhm, edge1, edge2, heightm(nhm), weightm(nhm)
      end if
c
      boundm1(nhm) = edge1
      boundm2(nhm) = edge2
c
  111 format('IN MSUWEIGHT3: MSU LAYER', i4, 2x, 'EDGE1 = ', f8.2, 2x,
     &'EDGE2 = ', f8.2, 2x, 'HEIGHT = ', f8.2, 2x, 'WEIGHT = ', f8.5)
c
c
c
c-------------------------------------------------------------------------------
c     ** 1.2 **
c   
c     Sum the total MSU weights over the layers of the target data set, 
c     multiplying by the pressure weights for individual MSU layers (and for
c     fractions of individual layers) contained within each target layer. 
c-------------------------------------------------------------------------------
c
c
c
      sum2 = 0.0
      do 121 k = 1, nhp
        s1     = boundp1(k)
        s2     = boundp2(k)
        sum1   = 0.0
        rcount = 0.0
        do 120 m = 1, nhm
          rm1 = boundm1(m)
          rm2 = boundm2(m)
c         print *, 's1, s2, rm1, rm2 = ', s1, s2, rm1, rm2
          if ((s1 .ge. rm1) .and. (s1 .le. rm2)) then
            wtemp = (min(s2, rm2) - s1) / (rm2 - rm1)
            sum1   = sum1 + wtemp * weightm(m)
            rcount = rcount + wtemp
          else if ((s2 .ge. rm1) .and. (s2 .le. rm2)) then
            wtemp = ((s2 - rm1) / (rm2 - rm1))
            sum1   = sum1 + wtemp * weightm(m)
            rcount = rcount + wtemp
          else if ((rm1 .ge. s1) .and. (rm2 .le. s2)) then
            sum1   = sum1 + weightm(m)
            rcount = rcount + 1
          end if
  120   continue
        totwt(k) = sum1 
c
        if (indx .le. 1) then
          write(*, 122) k, s1, s2, heightp(k), totwt(k), rcount
        end if
c
        sum2 = sum2 + totwt(k)
  121 continue
c
  122 format('TARGET LAYER', i4, 2x, 'EDGE1 = ', f8.2, 2x, 
     &'EDGE2 = ', f8.2, 2x, 'HEIGHT = ', f8.2, 2x,
     &'TOT. WT = ', f10.6, 2x, 'RCOUNT = ', f8.2)
c
      sum3 = 0.0
      do 123 k = 1, nhp
        wtnorm1(k) = totwt(k) / sum2
        if (indx .le. 1) then
          print *, 'HEIGHT = ', heightp(k), 'NORMALIZED WT = ',
     &    wtnorm1(k)
        end if
        sum3 = sum3 + wtnorm1(k)
  123 continue
      if (indx .le. 1) then
        print *, 'SUM OF WEIGHTS = ', sum3
      end if
c
c
c
c-------------------------------------------------------------------------------
c     ** 1.3 **
c
c     Compute the product of the temperature at a discrete pressure level times 
c     the MSU weight. Sum over all discrete pressure levels with valid data!
c-------------------------------------------------------------------------------
c
c
c
      sum1   = 0.0
      sumw   = 0.0
c
      do 130 k = 1, nhp
        term1 = ta1(k)
        if (term1 .ne. bogus) then
          sum1 = sum1 + (term1 * totwt(k))
          sumw = sumw + totwt(k)
        end if
  130 continue
c
      if (sumw .gt. critwt) then
        ta2 = sum1 / sumw
      else
        ta2 = bogus        
      end if
c
      if (indx .le. 1) then
        print *, 'Equivalent MSU temp = ', ta2 
      end if
c
      return
      end
c
      subroutine loop_thru_dims(nmsu,nlev,nic,nextra,
     &                          critw,
     &                          arrayin,weights,arrayout,
     &                          heightp,heightm,
     &                          temp,tempout,tempw)
      implicit none
      integer i,j,k,nmsu,nextra,nic,nlev
      real arrayin(nlev,nextra),arrayout(nic,nextra)
      real heightp(nlev),heightm(nmsu)
      real weights(nmsu,nic)
      real temp(nlev),tempw(nmsu),tempout(nlev)
      real equiv
      real bogus
      real critw
      real sumw,tempval

      bogus=1.e20

c$$$      print*, 'citeria:',critw,nic,bogus
      do i = 1, nextra
         do j = 1, nlev
            temp(j)=arrayin(j,i)
c$$$            if (i.le.2) then
c$$$               print *, i,j,' i-i ', temp(j)
c$$$            endif
         enddo
         do j = 1, nic
            do k = 1, nmsu
               tempw(k)=weights(k,j)
c$$$               if (i.le.2) then
c$$$                  print *, i,k,' w-w', tempw(k)
c$$$               endif

            enddo
c$$$      subroutine msuweight3(nhp, nhm, indx, heightp, heightm, weightm,
c$$$     &wtnorm1, ta1, ta2, bogus, critwt, sumw)

            call msuweight3(nlev, nmsu, i+1, heightp, heightm, 
     &                      tempw,
     &                      tempout, temp, 
     &                      tempval,
     &                      bogus, critw, sumw)
            arrayout(j,i)=tempval
         enddo
      enddo

      return
      end
