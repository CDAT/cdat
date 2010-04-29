c
c  . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .
c  .                                                             .
c  .                  copyright (c) 1998 by ucar                 .
c  .                                                             .
c  .       university corporation for atmospheric research       .
c  .                                                             .
c  .                      all rights reserved                    .
c  .                                                             .
c  .                                                             .
c  .                         spherepack3.0                       .
c  .                                                             .
c  . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .
c
c
c
c ... file gaqd.f
c
c     this file includes documentation and code for subrooutine gaqd.
c
c ... required files
c
c     none
c
c     subroutine gaqd(nlat,theta,wts,dwork,ldwork,ierror)
c
c     subroutine gaqd computes the nlat gaussian colatitudes and weights
c     in double precision. the colatitudes are in radians and lie in the
c     in the interval (0,pi).
c
c     input parameters
c
c     nlat    the number of gaussian colatitudes in the interval (0,pi)
c             (between the two poles).  nlat must be greater than zero.
c
c     dwork   a temporary work space
c
c     ldwork  the length of the work space  in the routine calling gaqd
c             ldwork must be at least nlat*(nlat+2).
c
c     output parameters
c
c     theta   a double precision vector of length nlat containing the
c             nlat gaussian colatitudes on the sphere in increasing radians
c             in the interval (o,pi).
c
c     wts     a double precision vector of length nlat containing the
c             nlat gaussian weights.
c
c     ierror = 0 no errors
c            = 1 if ldwork.lt.nlat*(nlat+2)
c            = 2 if nlat.le.0
c            = 3 if unable to compute gaussian points
c                (failure in in eigenvalue routine)
c
c  *****************************************************************
      subroutine gaqd(nlat,theta,wts,dwork,ldwork,ierror)
      double precision theta(nlat),wts(nlat),dwork(ldwork),x
      n = nlat
      ierror = 1
c     check work space length
      if (ldwork .lt. n*(n+2)) return
      ierror = 2
      if (n.le.0) return
      ierror = 0
      if (n.gt.2) then
c
c     partition dwork space for double precision eigenvalue(vector computation)
c
      i1 = 1
      i2 = i1+n
      i3 = i2+n
      call gaqd1(n,theta,wts,dwork(i1),dwork(i2),dwork(i3),ierror)
      if (ierror.ne.0) then
      ierror = 3
      return
      end if
      return
      else if (n.eq.1) then
      wts(1) = 2.0d0
      theta(1) = dacos(0.0d0)
      else if (n.eq.2) then
c     compute weights and points analytically when n=2
      wts(1) = 1.0d0
      wts(2) = 1.0d0
      x = dsqrt(1.0d0/3.0d0)
      theta(1) = dacos(x)
      theta(2) = dacos(-x)
      return
      end if
      end
      subroutine gaqd1(n,theta,wts,w,e,z,ier)
      dimension theta(n),wts(n),w(n),e(n),z(n,n)
      double precision theta,wts,temp,w,e,z
c     set symmetric tridiagnonal matrix subdiagonal and diagonal
c     coefficients for matrix coming from coefficients in the
c     recursion formula for legendre polynomials
c     a(n)*p(n-1)+b(n)*p(n)+c(n)*p(n+1) = 0.
      w(1)=0.d0
      e(1) = 0.d0
      do 100 j=2,n
      e(j)= (j-1.d0)/dsqrt((2.d0*j-1.d0)*(2.d0*j-3.d0))
      w(j) = 0.d0
  100 continue
c
c     compute eigenvalues and eigenvectors
c
      matz = 1
      call drst(n,n,w,e,matz,z,ier)
      if (ier.ne.0) return
c
c     compute gaussian weights and points
c
      do 101 j=1,n
      theta(j) = dacos(w(j))
c
c     set gaussian weights as 1st components of eigenvectors squared
c 
      wts(j) = 2.0d0*z(1,j)**2
  101 continue
c
c     reverse order of gaussian points to be
c     monotonic increasing in radians
c
      n2 = n/2
      do 102 i=1,n2
      temp = theta(i)
      theta(i) = theta(n-i+1)
      theta(n-i+1) = temp
  102 continue
      return
      end
      subroutine drst(nm,n,w,e,matz,z,ierr)
c     drst is a double precision modification of rst off eispack
c     to be used  to compute gaussian points and weights

c
      integer i,j,n,nm,ierr,matz
      double precision w(n),e(n),z(nm,n)

c
c     .......... find both eigenvalues and eigenvectors ..........
   20 do 40 i = 1, n
c
	 do 30 j = 1, n
	    z(j,i) = 0.0d0
   30    continue
c
	 z(i,i) = 1.0d0
   40 continue
c
      call  dintql(nm,n,w,e,z,ierr)
      return
      end
      subroutine dintql(nm,n,d,e,z,ierr)
c     dintql is a double precision modification of intql2 off
c     eispack to be used by gaqd in spherepack for computing
c     gaussian weights and points
c
      integer i,j,k,l,m,n,ii,nm,mml,ierr
      double precision d(n),e(n),z(nm,n)
      double precision b,c,f,g,p,r,s,tst1,tst2,dpytha
      ierr = 0
      if (n .eq. 1) go to 1001
c
      do 100 i = 2, n
  100 e(i-1) = e(i)
c
      e(n) = 0.0d0
c
      do 240 l = 1, n
	 j = 0
c     .......... look for small sub-diagonal element ..........
c
  105    nm1 = n-1     
         if(l .gt. nm1) go to 111
         do 110 mdo = l, nm1
            m = mdo
	    tst1 = dabs(d(m)) + dabs(d(m+1))
	    tst2 = tst1 + dabs(e(m))
	    if (tst2 .eq. tst1) go to 120
  110    continue
  111    m = n
c
  120    p = d(l)
	 if (m .eq. l) go to 240
	 if (j .eq. 30) go to 1000
	 j = j + 1
c     .......... form shift ..........
	 g = (d(l+1) - p) / (2.0d0 * e(l))
	 r = dpytha(g,1.0d0)
	 g = d(m) - p + e(l) / (g + sign(r,g))
	 s = 1.0d0
	 c = 1.0d0
	 p = 0.0d0
	 mml = m - l
c     .......... for i=m-1 step -1 until l do -- ..........
	 do 200 ii = 1, mml
	    i = m - ii
	    f = s * e(i)
	    b = c * e(i)
	    r = dpytha(f,g)
	    e(i+1) = r
	    if (r .eq. 0.0d0) go to 210
	    s = f / r
	    c = g / r
	    g = d(i+1) - p
	    r = (d(i) - g) * s + 2.0d0 * c * b
	    p = s * r
	    d(i+1) = g + p
	    g = c * r - b
c     .......... form vector ..........
	    do 180 k = 1, n
	       f = z(k,i+1)
	       z(k,i+1) = s * z(k,i) + c * f
	       z(k,i) = c * z(k,i) - s * f
  180       continue
c
  200    continue
c
	 d(l) = d(l) - p
	 e(l) = g
	 e(m) = 0.0d0
	 go to 105
c     .......... recover from underflow ..........
  210    d(i+1) = d(i+1) - p
	 e(m) = 0.0d0
	 go to 105
  240 continue
c     .......... order eigenvalues and eigenvectors ..........
      do 300 ii = 2, n
	 i = ii - 1
	 k = i
	 p = d(i)
c
	 do 260 j = ii, n
	    if (d(j) .ge. p) go to 260
	    k = j
	    p = d(j)
  260    continue
c
	 if (k .eq. i) go to 300
	 d(k) = d(i)
	 d(i) = p
c
	 do 280 j = 1, n
	    p = z(j,i)
	    z(j,i) = z(j,k)
	    z(j,k) = p
  280    continue
c
  300 continue
c
      go to 1001
c     .......... set error -- no convergence to an
c                eigenvalue after 30 iterations ..........
 1000 ierr = l
 1001 return
      end
      double precision function dpytha(a,b)
      double precision a,b
c     dpytha is a double precision modification of pythag off eispack
c     for use by dimtql

c
c     finds sqrt(a**2+b**2) without overflow or destructive underflow
c
      double precision p,r,s,t,u
      p = dabs(a)
      if (dabs(b).ge.dabs(a)) p = dabs(b)
      if (p .eq. 0.0d0) go to 20
      r = (dabs(a)/p)**2
      if (dabs(b).lt.dabs(a)) r = (dabs(b)/p)**2
   10 continue
	 t = 4.0d0 + r
	 if (t .eq. 4.0d0) go to 20
	 s = r/t
	 u = 1.0d0 + 2.0d0*s
	 p = u*p
	 r = (s/u)**2 * r
      go to 10
   20 dpytha = p
      return
      end
