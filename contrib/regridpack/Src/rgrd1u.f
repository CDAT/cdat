c
c
c ... file rgrd1u.f
c
c     this file contains documentation followed by fortran code for
c     subroutine rgrd1u and additional subroutines.
c
c ... author
c
c     John C. Adams (NCAR 1999)
c
c ... subroutine rgrd1u(nx,p,mx,q,intpol,w,lw,iw,liw,ier)
c
c ... purpose
c
c     subroutine rgrd1u interpolates the nx vector p onto
c     the mx vector q. it is assumed that p and q are
c     values on uniform nx and mx grids which subdivide
c     the same interval (INCLUDING END POINTS).  if p and
c     q are values on nonuniform grids and/or if q is defined
c     on a grid which lies within the p grid then subroutine
c     rgrd1 (see file rgrd1.f) should be used.
c
c ... language
c
c     coded in portable FORTRAN77 and FORTRAN90
c
c ... test program
c
c     file trgrd1u.f on regridpack includes a test program for subroutine rgrd1u
c
c ... method
c
c     linear or cubic interpolation (see intpol) is used when the
c     mx uniform grid is not a subgrid of the nx uniform grid (i.e.,
c     whenever mx-1 does not divide nx-1).  q is set directly from
c     p in the subgrid case.
c
c ... required files
c
c     none
c
c ... efficiency
c
c     inner most loops in regridpack software vectorize.
c
c
c *** input arguments ***
c
c
c ... nx
c
c     the integer dimension of p.  nx > 1 if intpol = 1 or
c     nx > 3 if intpol = 3 is required (see ier = 2).
c
c ... p
c
c     a real nx dimensioned vector of given values
c
c ... mx
c
c     the integer dimension of q.  mx > 1 is required (see ier = 1)
c
c
c ... intpol
c
c     an integer which sets linear or cubic interpolation as follows:
c
c        intpol = 1 sets linear interpolation
c        intpol = 3 sets cubic interpolation
c
c     values other than 1 or 3 in intpol are not allowed (ier = 4).
c
c ... w
c
c     a real work space of length lw.
c
c ... lw
c
c     the integer length of the work space w in the routine calling rgrd1u.
c     if mx-1 divides nx-1 then the mx uniform grid is a subgrid of
c     the nx uniform grid.  in this case let lwmin = 1.  otherwise
c     let lwmin = mx if intpol = 1 or lwmin = mx if intpol = 3.
c     then lw must be greater than or equal to lwmin (see ier=4).
c
c ... iw
c
c     an integer work space of length liw
c
c ... liw
c
c     the integer length of the integer work space iw in the routine calling rgrd1u.
c     liw must be greater than or equal to mx.
c
c
c *** output arguments ***
c
c
c ... q
c
c     a real mx dimensioned vector of values which are interpolated from p.
c
c ... ier
c
c     an integer error flag set as follows:
c
c     ier = 0 if no errors in input arguments are detected
c
c     ier = 1 if  mx < 2
c
c     ier = 2 if nx < 2 when intpol=1 or nx < 4 when intpol=3.
c
c     ier = 3 if intpol is not equal to 1 or 3
c
c     ier = 4 if lw or liw is too small (insufficient work space)
c
c
c ************************************************************************
c
c     end of rgrd1u documentation, fortran code follows:
c
c ************************************************************************
c
c
      subroutine rgrd1u(nx,p,mx,q,intpol,w,lw,iw,liw,ier)
      implicit none
      integer intpol,liw,iw(liw),lw,nx,mx,ier,inmx,isubx
      integer i2,i3,i4,i5,lwmin
      real p(nx),q(mx),w(lw)
c
c     check input arguments
c
      ier = 1
c
c     check mx
c
      if (mx .lt. 2) return
c
c     check intpol
c
      ier = 3
      if (intpol.ne.1 .and. intpol.ne.3) return
c
c     check nx
c
      ier = 2
      if (intpol.eq.1 .and. nx.lt.2) return
      if (intpol.eq.3 .and. nx.lt.4) return
c
c     set subgrid integer indicator
c
      inmx = (nx-1)/(mx-1)
      isubx = nx - inmx*(mx-1)
c
c     set minimum and check work space
c
      ier = 4
      if (isubx.ne.1) then
	if (intpol.eq.1) lwmin = mx
	if (intpol.eq.3) lwmin = 4*mx
      else
	lwmin = 1
      end if
      if (lw .lt. lwmin) return
      if (liw .lt. mx) return
c
c     input arguments o.k.
c
      ier = 0
c
c     preset pointers
c
      i2 = 1
      i3 = 1
      i4 = 1
      i5 = 1
      if (intpol .eq. 1) then
c
c     linear interpolation in x
c
	if (isubx .ne. 1) then
	  call linmxu(nx,mx,iw,w)
	end if
	call lint1u(nx,p,mx,q,iw,w,inmx,isubx)
	return
      else
c
c     cubic interpolation in x
c
	if (isubx .ne. 1) then
	  i2 = 1
	  i3 = i2+mx
	  i4 = i3+mx
	  i5 = i4+mx
	  call cubnmxu(nx,mx,iw,w(i2),w(i3),w(i4),w(i5))
	end if
	call cubt1u(nx,p,mx,q,iw,w(i2),w(i3),w(i4),w(i5),inmx,isubx)
      return
      end if
      end

      subroutine lint1u(nx,p,mx,q,ix,dx,inmx,isubx)
      implicit none
      integer nx,mx,ix(mx),inmx,isubx,i,ii
      real p(nx),q(mx),dx(mx)
      if (isubx .eq. 1) then
c
c     mx grid is subset of nx grid so q can be set directly
c
	do ii=1,mx
	  i = inmx*(ii-1)+1
	  q(ii) = p(i)
	end do
	return
      else
c
c     linearly interpolate
c
	do ii=1,mx
	  i = ix(ii)
	  q(ii) = p(i)+dx(ii)*(p(i+1)-p(i))
	end do
	return
      end if
      end

      subroutine cubt1u(nx,p,mx,q,ix,dxm,dx,dxp,dxpp,inmx,isubx)
      implicit none
      integer nx,mx,ix(mx),inmx,isubx,i,ii
      real p(nx),q(mx)
      real dxm(mx),dx(mx),dxp(mx),dxpp(mx)

      if (isubx .eq. 1) then
c
c     mx grid is subset of nx grid so q can be set directly
c
	do ii=1,mx
	  i = inmx*(ii-1)+1
	  q(ii) = p(i)
	end do
	return

      else
c
c     cubically interpolate on uniform grid
c
	do ii=1,mx
	  i = ix(ii)
	  q(ii)=(dxm(ii)*p(i-1)+dx(ii)*p(i)+dxp(ii)*p(i+1)+
     +           dxpp(ii)*p(i+2))
	end do
	return
      end if
      end

      subroutine linmxu(nx,mx,ix,dx)
c
c     set linear interpolation terms
c
      implicit none
      integer nx,mx,ix(mx),i,ii
      real dx(mx),dnx,dmx,x,xx
c
c     set "virtual" uniform increments
c
      dnx = 1.0/(nx-1)
      dmx = 1.0/(mx-1)
c
c     set ix(ii) = i  s.t. i,i+1 can interpolate for ii
c
      do ii=1,mx
	xx = (ii-1)*dmx
	ix(ii) = min0(int(xx/dnx)+1,nx-1)
c
c     set scale term for linear
c
	i = ix(ii)
	x = (i-1)*dnx
	dx(ii) = (xx-x)/dnx
      end do
      return
      end

      subroutine cubnmxu(nx,mx,ix,dxm,dx,dxp,dxpp)
c
c     set cubic interpolation terms
c
      implicit none
      integer nx,mx,ix(mx),i,ii
      real dxm(mx),dx(mx),dxp(mx),dxpp(mx),dnx,dmx,odnx3
      real xx,xim,xi,xip,xipp
c
c     set "virtual" uniform increments
c
      dnx = 1.0/(nx-1)
      dmx = 1.0/(mx-1)
      odnx3 = 1.0/(6.*dnx*dnx*dnx)
c
c     set i=ix(ii) in [2,nx-2] such that
c     i-1,i,i+1,i+2 can be used to interpolate at ii
c
      do ii=1,mx
	xx = (ii-1)*dmx
	ix(ii) = min0(max0(int(xx/dnx)+1,2),nx-2)
	i = ix(ii)
c
c     set scale terms for cubic
c
	xi = (i-1)*dnx
	xim = xi-dnx
	xip = xi+dnx
	xipp = xip+dnx
	dxm(ii) = -(xx-xi)*(xx-xip)*(xx-xipp)*odnx3
	dx(ii) = 3.*(xx-xim)*(xx-xip)*(xx-xipp)*odnx3
	dxp(ii) = -3.*(xx-xim)*(xx-xi)*(xx-xipp)*odnx3
	dxpp(ii) = (xx-xim)*(xx-xi)*(xx-xip)*odnx3
      end do
      return
      end
