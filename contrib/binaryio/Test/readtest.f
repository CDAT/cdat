      real y(10)
      integer i, iunit
      open(unit=7, file='testout', status='old', access='sequential',
     & form='unformatted')
      read(7) (y(i), i = 1, 10)
      print *, (y(i), i=1,10)
      close(7)
      end
