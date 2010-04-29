      subroutine binout1 (iunit, x, n)
      integer iunit, n, i
      real x(n)
      write(iunit) (x(i), i = 1, n)
      return
      end

      subroutine binout2 (iunit, x, n, m)
      integer iunit, n, m, i
      real x(n, m)
      write(iunit) ((x(i, j), i = 1, n), j = 1, m)
      return
      end

      subroutine binout3 (iunit, x, n1, n2, n3)
      integer iunit, n1, n2, n3, i, j, k
      real x(n1, n2, n3)
      write(iunit) (((x(i, j, k), i = 1, n1), j = 1, n2), k=1, n3)
      return
      end

      subroutine binout4 (iunit, x, n1, n2, n3, n4)
      integer iunit, n1, n2, n3, n4, i, j, k, l
      real x(n1, n2, n3, n4)
      write(iunit) ((((x(i, j, k, l), 
     & i = 1, n1), j = 1, n2), k=1, n3), l=1, n4)
      return
      end

      subroutine binin1 (iunit, x, n)
      integer iunit, n, i
      real x(n)
      read(iunit) (x(i), i = 1, n)
      return
      end

      subroutine binin2 (iunit, x, n, m)
      integer iunit, n, m, i
      real x(n, m)
      read(iunit) ((x(i, j), i = 1, n), j = 1, m)
      return
      end

      subroutine binin3 (iunit, x, n1, n2, n3)
      integer iunit, n1, n2, n3, i, j, k
      real x(n1, n2, n3)
      read(iunit) (((x(i, j, k), i = 1, n1), j = 1, n2), k=1, n3)
      return
      end

      subroutine binin4 (iunit, x, n1, n2, n3, n4)
      integer iunit, n1, n2, n3, n4, i, j, k, l
      real x(n1, n2, n3, n4)
      read(iunit) ((((x(i, j, k, l), 
     & i = 1, n1), j = 1, n2), k=1, n3), l=1, n4)
      return
      end

      function bincreate (f)
      integer bincreate, binfree
      character*(*) f
      integer iunit
      iunit = binfree()
      open(unit=iunit, file=f, access='sequential', form='unformatted',
     &     status='new')
      bincreate = iunit
      return
      end

      function binopen (f)
      integer binopen, binfree
      character*(*) f
      integer iunit
      iunit = binfree()
      open(unit=iunit, file=f, access='sequential', form='unformatted',
     &     status='old')
      binopen = iunit
      return
      end

      function binfree()
      integer binfree
      integer units(10), base
      integer j
      parameter(base=70)
      common /binfr/ units
      save /binfr/
      data units /0,0,0,0,0,0,0,0,0,0/
      do 100 j = 1, 10
         if (units(j) .eq. 0) then
             units(j) = 1
             binfree = base + j
             return
         endif
 100     continue
      stop "binfree out of units"
      end
     
      subroutine binclose (iunit)
      integer units(10), base
      parameter(base=70)
      common /binfr/ units
      save /binfr/
      integer iunit
      close(iunit)
      units(iunit-base) = 0
      return
      end










