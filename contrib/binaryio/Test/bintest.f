      real x(10), y(10)
      integer i, iunit, bincreate
      do 100 i=1, 10
         x(i) = i
 100     continue
      iunit = bincreate('testbin')
      call binout1(iunit, x, 10)
      call binclose(iunit)

      open(unit=7, file='testbin', status='old', access='sequential',
     & form='unformatted')
      read(7) (y(i), i = 1, 10)
      print *, (y(i), i=1,10)
      close(7)
      end
