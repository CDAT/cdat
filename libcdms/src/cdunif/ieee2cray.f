      subroutine IEEE2CRAY(ai,a,n)
      dimension ai(n),a(n)
      ierr=ieg2cray(2,n,ai,0,a)
      return
      end
