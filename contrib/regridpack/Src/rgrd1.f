c
c
c ... file rgrd1.f
c
c     this file contains documentation for subroutine rgrd1 followed by
c     fortran code for rgrd1 and additional subroutines.
c
c ... author
c
c     John C. Adams (NCAR 1999)
c
c ... subroutine rgrd1(nx,x,p,mx,xx,q,intpol,w,lw,iw,liw,ier)
c
c ... purpose
c
c     subroutine rgrd1 interpolates the values p(i) on the grid x(i)
c     for i=1,...,nx onto q(ii) on the grid xx(ii),ii=1,...,mx.
c
c ... language
c
c     coded in portable FORTRAN90 and FORTRAN77
c
c ... test program
c
c     file trgrd1.f on regridpack includes a test program for subroutine rgrd1
c
c ... method
c
c     Linear interpolation (intpol=1)
c
c       Given ii in the integer interval [1,mx], let i be the
c       largest integer in the integer interval [1,nx-1] such
c       that x(i) is less than or equal to xx(ii).  Then q(ii)
c       at xx(ii) is linearly interpolated from p(i),p(i+1) at
c       x(i),x(i+1).
c
c     Cubic interpolation (intpol=3)
c
c       Given ii in the integer interval [1,mx], let i be the
c       largest integer in the integer interval [2,nx-2] such
c       that x(i) is less than or equal to xx(ii).  Then q(ii)
c       at xx(ii) is cubically interpolated from p(i-1),p(i),
c       p(i+1),p(i+2) at x(i-1),x(i),x(i+1),x(i+2).
c
c ... required files
c
c     none
c
c ... requirements
c
c     x must be a strictly increasing grid and xx must be an increasing
c     grid (see ier = 4).  in addition the interval
c
c          [xx(1),xx(mx)]
c
c     must lie within the interval
c
c          [x(1),x(nx)].
c
c     extrapolation is not allowed (see ier=3).  if these intervals
c     are identical and the x and xx grids are UNIFORM then subroutine
c     rgrd1u (see file rgrd1u.f) should be used in place of rgrd1.
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
c *** input arguments
c
c
c ... nx
c
c     the integer dimension of the grid vector x and the dimension of p.
c     nx > 1 if intpol = 1 or nx > 3 if intpol = 3 is required.
c
c ... x
c
c     a real nx vector of strictly increasing values which defines the x
c     grid on which p is given.
c
c
c ... p
c
c     a real nx vector of values given on the x grid
c
c ... mx
c
c     the integer dimension of the grid vector xx and the dimension of q.
c     mx > 0 is required.
c
c ... xx
c
c     a real mx vector of increasing values which defines the
c     grid on which q is defined.  xx(1) < x(1) or xx(mx) > x(nx)
c     is not allowed (see ier = 3)
c
c ... intpol
c
c     an integer which sets linear or cubic
c     interpolation as follows:
c
c        intpol = 1 sets linear interpolation
c        intpol = 3 sets cubic interpolation
c
c     values other than 1 or 3 in intpol are not allowed (ier = 6).
c
c ... w
c
c     a real work space of length at least lw which must be provided in the
c     routine calling rgrd1
c
c
c ... lw
c
c     the integer length of the real work space w.  let
c
c          lwmin =   mx            if intpol(1) = 1
c          lwmin = 4*mx            if intpol(1) = 3
c
c     then lw must be greater than or equal to lwmin
c
c ... iw
c
c     an integer work space of length at least liw which must be provided in the
c     routine calling rgrd1
c
c ... liw
c
c     tne length of the integer work space iw. liw must be greater than or equal to mx.
c
c
c *** output arguments
c
c
c ... q
c
c     a real mx vector of values on the xx grid which are
c     interpolated from p on the x grid
c
c ... ier
c
c     an integer error flag set as follows:
c
c     ier = 0 if no errors in input arguments are detected
c
c     ier = 1 if  mx < 1
c
c     ier = 2 if nx < 2 when intpol=1 or nx < 4 when intpol=3
c
c     ier = 3 if xx(1) < x(1) or x(nx) < xx(mx)
c
c *** to avoid this flag when end points are intended to be the
c     same but may differ slightly due to roundoff error, they
c     should be set exactly in the calling routine (e.g., if both
c     grids have the same x boundaries then xx(1)=x(1) and xx(mx)=x(nx)
c     should be set before calling rgrd1)
c
c     ier = 4 if the x grid is not strictly monotonically increasing
c             or if the xx grid is not montonically increasing.  more
c             precisely if:
c
c             x(i+1) <= x(i) for some i such that 1 <= i < nx (or)
c
c             xx(ii+1) < xx(ii) for some ii such that 1 <= ii < mx
c
c     ier = 5 if lw or liw is too small (insufficient work space)
c
c     ier = 6 if intpol is not equal to 1 or 3
c
c ************************************************************************
c
c     end of rgrd1 documentation, fortran code follows:
c
c ************************************************************************
c
      subroutine rgrd1(nx,x,p,mx,xx,q,intpol,w,lw,iw,liw,ier)
c     dimension x(nx),p(nx),xx(mx),q(mx),w(lw)
      implicit none
      real x(*),p(*),xx(*),q(*),w(*)
      integer iw(*)
      integer nx,mx,ier,intpol,lw,liw,i,ii,i1,i2,i3,i4
c
c     check arguments for errors
c
      ier = 1
c
c     check xx grid resolution
c
      if (mx .lt. 1) return
c
c     check intpol
c
      ier = 6
      if (intpol.ne.1 .and. intpol.ne.3) return
c
c     check x grid resolution
c
      ier = 2
      if (intpol.eq.1 .and. nx.lt.2) return
      if (intpol.eq.3 .and. nx.lt.4) return
c
c     check xx grid contained in x grid
c
      ier = 3
      if (xx(1).lt.x(1) .or. xx(mx).gt.x(nx)) return
c
c     check montonicity of grids
c
      do i=2,nx
	if (x(i-1).ge.x(i)) then
	  ier = 4
	  return
	end if
      end do
      do ii=2,mx
	if (xx(ii-1).gt.xx(ii)) then
	  ier = 4
	  return
	end if
      end do
c
c     check minimum work space lengths
c
      if (intpol.eq.1) then
	if (lw .lt. mx) return
      else
	if (lw .lt. 4*mx) return
      end if
      if (liw .lt. mx) return
c
c     arguments o.k.
c
      ier = 0

      if (intpol.eq.1) then
c
c     linear interpolation in x
c
      i1 = 1
      i2 = i1+mx
      call linmx(nx,x,mx,xx,iw,w)
      call lint1(nx,p,mx,q,iw,w)
      return
      else
c
c     cubic interpolation in x
c
      i1 = 1
      i2 = i1+mx
      i3 = i2+mx
      i4 = i3+mx
      call cubnmx(nx,x,mx,xx,iw,w(i1),w(i2),w(i3),w(i4))
      call cubt1(nx,p,mx,q,iw,w(i1),w(i2),w(i3),w(i4))
      return
      end if
      end

      subroutine lint1(nx,p,mx,q,ix,dx)
c     dimension p(nx),q(mx),ix(mx),dx(mx)
      implicit none
      integer mx,ix(mx),nx,ii,i
      real p(nx),q(mx),dx(mx)
c
c     linearly interpolate p on x onto q on xx
c
      do ii=1,mx
	i = ix(ii)
	q(ii) = p(i)+dx(ii)*(p(i+1)-p(i))
      end do
      return
      end

      subroutine cubt1(nx,p,mx,q,ix,dxm,dx,dxp,dxpp)
      implicit none
      integer mx,ix(mx),nx,i,ii
      real p(nx),q(mx),dxm(mx),dx(mx),dxp(mx),dxpp(mx)
c
c     cubically interpolate p on x to q on xx
c
      do ii=1,mx
	i = ix(ii)
	q(ii) = dxm(ii)*p(i-1)+dx(ii)*p(i)+dxp(ii)*p(i+1)+dxpp(ii)*p(i+2)
      end do
      return
      end

      subroutine linmx(nx,x,mx,xx,ix,dx)
c
c     set x grid pointers for xx grid and interpolation scale terms
c
      implicit none
      real x(*),xx(*),dx(*)
      integer ix(*),isrt,ii,i,nx,mx
      isrt = 1
      do ii=1,mx
c
c     find x(i) s.t. x(i) < xx(ii) <= x(i+1)
c
	do i=isrt,nx-1
	  if (x(i+1) .ge. xx(ii)) then
	    isrt = i
	    ix(ii) = i
	    go to 3
	  end if
	end do
    3   continue
      end do
c
c     set linear scale term
c
      do ii=1,mx
	i = ix(ii)
	dx(ii) = (xx(ii)-x(i))/(x(i+1)-x(i))
      end do
      return
      end

      subroutine cubnmx(nx,x,mx,xx,ix,dxm,dx,dxp,dxpp)
      implicit none
      real x(*),xx(*),dxm(*),dx(*),dxp(*),dxpp(*)
      integer ix(*),mx,nx,i,ii,isrt

      isrt = 1
      do ii=1,mx
c
c     set i in [2,nx-2] closest s.t.
c     x(i-1),x(i),x(i+1),x(i+2) can interpolate xx(ii)
c
	do i=isrt,nx-1
	  if (x(i+1) .ge. xx(ii)) then
	    ix(ii) = min0(nx-2,max0(2,i))
	    isrt = ix(ii)
	    go to 3
	  end if
	end do
    3   continue
      end do
c
c     set cubic scale terms
c
      do ii=1,mx
	i = ix(ii)
	dxm(ii) = (xx(ii)-x(i))*(xx(ii)-x(i+1))*(xx(ii)-x(i+2))/
     +          ((x(i-1)-x(i))*(x(i-1)-x(i+1))*(x(i-1)-x(i+2)))
	dx(ii) = (xx(ii)-x(i-1))*(xx(ii)-x(i+1))*(xx(ii)-x(i+2))/
     +          ((x(i)-x(i-1))*(x(i)-x(i+1))*(x(i)-x(i+2)))
	dxp(ii) = (xx(ii)-x(i-1))*(xx(ii)-x(i))*(xx(ii)-x(i+2))/
     +          ((x(i+1)-x(i-1))*(x(i+1)-x(i))*(x(i+1)-x(i+2)))
	dxpp(ii) = (xx(ii)-x(i-1))*(xx(ii)-x(i))*(xx(ii)-x(i+1))/
     +          ((x(i+2)-x(i-1))*(x(i+2)-x(i))*(x(i+2)-x(i+1)))
      end do
      return
      end

