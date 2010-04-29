c
c
c ... file rgrd2.f
c
c     this file contains documentation for subroutine rgrd2 followed by
c     fortran code for rgrd2 and additional subroutines.
c
c ... author
c
c     John C. Adams (NCAR 1999)
c
c ... subroutine rgrd2(nx,ny,x,y,p,mx,my,xx,yy,q,intpol,w,lw,iw,liw,ier)
c
c ... purpose
c
c     subroutine rgrd2 interpolates the values p(i,j) on the orthogonal
c     grid (x(i),y(j)) for i=1,...,nx and j=1,...,ny onto q(ii,jj) on the
c     orthogonal grid (xx(ii),yy(jj)) for ii=1,...,mx and jj=1,...,my.
c
c ... language
c
c     coded in portable FORTRAN77 and FORTRAN90
c
c ... test program
c
c     file trgrd2.f on regridpack includes a test program for subroutine rgrd2
c
c ... method
c
c     linear or cubic interpolation is used (independently) in
c     each direction (see argument intpol).
c
c ... required files
c
c     file rgrd1.f must be loaded with rgrd2.f.  it includes
c     subroutines called by the routines in rgrd2.f
c
c ... requirements
c
c     each of the x,y grids must be strictly montonically increasing
c     and each of the xx,yy grids must be montonically increasing (see
c     ier = 4).  in addition the (X,Y) region
c
c          [xx(1),xx(mx)] X [yy(1),yy(my)]
c
c     must lie within the (X,Y) region
c
c          [x(1),x(nx)] X [y(1),y(ny)].
c
c     extrapolation is not allowed (see ier=3).  if these (X,Y)
c     regions are identical and the orthogonal grids are UNIFORM
c     in each direction then subroutine rgrd2u (see file rgrd2u.f)
c     should be used instead of rgrd2.
c
c ... efficiency
c
c     inner most loops in regridpack software vectorize.
c     If the arguments mx,my (see below) have different values, optimal
c     vectorization will be achieved if mx > my.
c
c
c *** input argument
c
c
c ... nx
c
c     the integer dimension of the grid vector x and the first dimension
c     of p.  nx > 1 if intpol(1) = 1 or nx > 3 if intpol(1) = 3 is required.
c
c ... ny
c
c     the integer dimension of the grid vector y and the second dimension
c     of p.  ny > 1 if intpol(2) = 1 or ny > 3 if intpol(2) = 3 is required.
c
c ... x
c
c     a real nx vector of strictly increasing values which defines the x
c     portion of the orthogonal grid on which p is given
c
c ... y
c
c     a real ny vector of strictly increasing values which defines the y
c     portion of the orthogonal grid on which p is given
c
c ... p
c
c     a real nx by ny array of values given on the orthogonal (x,y) grid
c
c ... mx
c
c     the integer dimension of the grid vector xx and the first dimension
c     of q.  mx > 0 is required.
c
c ... my
c
c     the integer dimension of the grid vector yy and the second dimension
c     of q.  my > 0 is required.
c
c ... xx
c
c     a real mx vector of increasing values which defines the x portion of the
c     orthogonal grid on which q is defined.  xx(1) < x(1) or xx(mx) > x(nx)
c     is not allowed (see ier = 3)
c
c ... yy
c
c     a real my vector of increasing values which defines the y portion of the
c     orthogonal grid on which q is defined.  yy(1) < y(1) or yy(my) > y(ny)
c     is not allowed (see ier = 3)
c
c ... intpol
c
c     an integer vector of dimension 2 which sets linear or cubic
c     interpolation in the x,y directions as follows:
c
c        intpol(1) = 1 sets linear interpolation in the x direction
c        intpol(1) = 3 sets cubic interpolation in the x direction.
c
c        intpol(2) = 1 sets linear interpolation in the y direction
c        intpol(2) = 3 sets cubic interpolation in the y direction.
c
c     values other than 1 or 3 in intpol are not allowed (ier = 5).
c
c ... w
c
c     a real work space of length at least lw which must be provided in the
c     routine calling rgrd2
c
c ... lw
c
c     the integer length of the real work space w.  let
c
c          lwx =   mx            if intpol(1) = 1
c          lwx = 4*mx            if intpol(1) = 3
c
c          lwy = my+2*mx         if intpol(2) = 1
c          lwy = 4*(mx+my)       if intpol(2) = 3
c
c     then lw must be greater than or equal to lwx+lwy
c
c ... iw
c
c     an integer work space of length at least liw which must be provided in the
c     routine calling rgrd2
c
c ... liw
c
c     the integer length of the integer work space iw.  liw must be at least mx+my
c
c *** output arguments
c
c
c ... q
c
c     a real mx by my array of values on the (xx,yy) grid which are
c     interpolated from p on the (x,y) grid
c
c ... ier
c
c     an integer error flag set as follows:
c
c     ier = 0 if no errors in input arguments are detected
c
c     ier = 1 if  min0(mx,my) < 1
c
c     ier = 2 if nx < 2 when intpol(1)=1 or nx < 4 when intpol(1)=3 (or)
c                ny < 2 when intpol(2)=1 or ny < 4 when intpol(2)=3
c
c     ier = 3 if xx(1) < x(1) or x(nx) < xx(mx) (or)
c                yy(1) < y(1) or y(ny) < yy(my) (or)
c
c *** to avoid this flag when end points are intended to be the
c     same but may differ slightly due to roundoff error, they
c     should be set exactly in the calling routine (e.g., if both
c     grids have the same y boundaries then yy(1)=y(1) and yy(my)=y(ny)
c     should be set before calling rgrd2)
c
c     ier = 4 if one of the grids x,y is not strictly monotonically
c             increasing or if one of the grids xx,yy is not
c             montonically increasing.  more precisely if:
c
c             x(i+1) <= x(i) for some i such that 1 <= i < nx (or)
c
c             y(j+1) <= y(j) for some j such that 1 <= j < ny (or)
c
c             xx(ii+1) < xx(ii) for some ii such that 1 <= ii < mx (or)
c
c             yy(jj+1) < yy(jj) for some jj such that 1 <= jj < my
c
c     ier = 5 if lw or liw is to small (insufficient work space)
c
c     ier = 6 if intpol(1) or intpol(2) is not equal to 1 or 3
c
c ************************************************************************
c
c     end of rgrd2 documentation, fortran code follows:
c
c ************************************************************************
c
      subroutine rgrd2(nx,ny,x,y,p,mx,my,xx,yy,q,intpol,w,lw,iw,liw,ier)
      implicit none
      integer nx,ny,mx,my,lw,liw,ier
      integer intpol(2),iw(liw)
      real x(nx),y(ny),p(nx,ny),xx(mx),yy(my),q(mx,my),w(lw)
      integer i,ii,j,jj,j2,j3,j4,j5,j6,j7,j8,j9,i2,i3,i4,i5
      integer jy,lwx,lwy
c
c     check input arguments
c
      ier = 1
c
c     check (xx,yy) grid resolution
c
      if (min0(mx,my) .lt. 1) return
c
c     check intpol
c
      ier = 6
      if (intpol(1).ne.1 .and. intpol(1).ne.3) return
      if (intpol(2).ne.1 .and. intpol(2).ne.3) return
c
c     check (x,y) grid resolution
c
      ier = 2
      if (intpol(1).eq.1 .and. nx.lt.2) return
      if (intpol(1).eq.3 .and. nx.lt.4) return
      if (intpol(2).eq.1 .and. ny.lt.2) return
      if (intpol(2).eq.3 .and. ny.lt.4) return
c
c     check work space lengths
c
      ier = 5
      if (intpol(1).eq.1) then
      lwx = mx
      else
      lwx = 4*mx
      end if
      if (intpol(2).eq.1) then
      lwy = my+2*mx
      else
      lwy = 4*(mx+my)
      end if
      if (lw .lt. lwx+lwy) return
      if (liw .lt. mx+my) return
c
c     check (xx,yy) grid contained in (x,y) grid
c
      ier = 3
      if (xx(1).lt.x(1) .or. xx(mx).gt.x(nx)) return
      if (yy(1).lt.y(1) .or. yy(my).gt.y(ny)) return
c
c     check montonicity of grids
c
      ier = 4
      do i=2,nx
	if (x(i-1).ge.x(i)) return
      end do
      do j=2,ny
	if (y(j-1).ge.y(j)) return
      end do
      do ii=2,mx
	if (xx(ii-1).gt.xx(ii)) return
      end do
      do jj=2,my
	if (yy(jj-1).gt.yy(jj)) return
      end do
c
c     arguments o.k.
c
      ier = 0
c
c     set pointer in integer work space
c
      jy = mx+1
      if (intpol(2) .eq.1) then
c
c     linearly interpolate in y
c
      j2 = 1
      j3 = j2
      j4 = j3+my
      j5 = j4
      j6 = j5
      j7 = j6
      j8 = j7+mx
      j9 = j8+mx
c
c     set y interpolation indices and scales and linearly interpolate
c
      call linmx(ny,y,my,yy,iw(jy),w(j3))
      i2 = j9
c
c     set work space portion and indices which depend on x interpolation
c
      if (intpol(1) .eq. 1) then
      i3 = i2
      i4 = i3
      i5 = i4
      call linmx(nx,x,mx,xx,iw,w(i3))
      else
      i3 = i2+mx
      i4 = i3+mx
      i5 = i4+mx
      call cubnmx(nx,x,mx,xx,iw,w(i2),w(i3),w(i4),w(i5))
      end if
      call lint2(nx,ny,p,mx,my,q,intpol,iw(jy),w(j3),
     +            w(j7),w(j8),iw,w(i2),w(i3),w(i4),w(i5))
      return

      else
c
c     cubically interpolate in y, set indice pointers
c
      j2 = 1
      j3 = j2+my
      j4 = j3+my
      j5 = j4+my
      j6 = j5+my
      j7 = j6+mx
      j8 = j7+mx
      j9 = j8+mx
      call cubnmx(ny,y,my,yy,iw(jy),w(j2),w(j3),w(j4),w(j5))
      i2 =  j9+mx
c
c     set work space portion and indices which depend on x interpolation
c
      if (intpol(1) .eq. 1) then
      i3 = i2
      i4 = i3
      i5 = i4
      call linmx(nx,x,mx,xx,iw,w(i3))
      else
      i3 = i2+mx
      i4 = i3+mx
      i5 = i4+mx
      call cubnmx(nx,x,mx,xx,iw,w(i2),w(i3),w(i4),w(i5))
      end if
      call cubt2(nx,ny,p,mx,my,q,intpol,iw(jy),w(j2),w(j3),
     +w(j4),w(j5),w(j6),w(j7),w(j8),w(j9),iw,w(i2),w(i3),w(i4),w(i5))
      return
      end if
      end

      subroutine lint2(nx,ny,p,mx,my,q,intpol,jy,dy,pj,pjp,
     +                 ix,dxm,dx,dxp,dxpp)
      implicit none
      integer nx,ny,mx,my,intpol(2),jy(my),ix(mx)
      integer jsave,j,jj,ii
      real p(nx,ny),q(mx,my)
      real pj(mx),pjp(mx),dy(my)
      real dxm(mx),dx(mx),dxp(mx),dxpp(mx)
c
c     linearly interpolate in y
c
      if (intpol(1).eq.1) then
c
c     linear in x
c
      jsave = -1
      do jj=1,my
	j = jy(jj)
	if (j.eq.jsave) then
c
c       j pointer has not moved since last pass (no updates or interpolation)
c
	else if (j.eq.jsave+1) then
c
c       update j and interpolate j+1
c
	  do ii=1,mx
	    pj(ii) = pjp(ii)
	  end do
	  call lint1(nx,p(1,j+1),mx,pjp,ix,dx)
	else
c
c       interpolate j,j+1in pj,pjp on xx mesh
c
	call lint1(nx,p(1,j),mx,pj,ix,dx)
	call lint1(nx,p(1,j+1),mx,pjp,ix,dx)
	end if
c
c       save j pointer for next pass
c
	jsave = j
c
c       linearly interpolate q(ii,jj) from pjp,pj in y direction
c
	do ii=1,mx
	  q(ii,jj) = pj(ii)+dy(jj)*(pjp(ii)-pj(ii))
	end do
      end do

      else
c
c     cubic in x
c
      jsave = -1
      do jj=1,my
	j = jy(jj)
	if (j.eq.jsave) then
c
c       j pointer has not moved since last pass (no updates or interpolation)
c
	else if (j.eq.jsave+1) then
c
c       update j and interpolate j+1
c
	  do ii=1,mx
	    pj(ii) = pjp(ii)
	  end do
	  call cubt1(nx,p(1,j+1),mx,pjp,ix,dxm,dx,dxp,dxpp)
	else
c
c       interpolate j,j+1 in pj,pjp on xx mesh
c
	  call cubt1(nx,p(1,j),mx,pj,ix,dxm,dx,dxp,dxpp)
	  call cubt1(nx,p(1,j+1),mx,pjp,ix,dxm,dx,dxp,dxpp)
	end if
c
c       save j pointer for next pass
c
	jsave = j
c
c       linearly interpolate q(ii,jj) from pjp,pj in y direction
c
	do ii=1,mx
	  q(ii,jj) = pj(ii)+dy(jj)*(pjp(ii)-pj(ii))
	end do
      end do
      return
      end if
      end

      subroutine cubt2(nx,ny,p,mx,my,q,intpol,jy,dym,dy,dyp,
     +dypp,pjm,pj,pjp,pjpp,ix,dxm,dx,dxp,dxpp)
      implicit none
      integer nx,ny,mx,my,intpol(2),jy(my),ix(mx)
      integer jsave,j,jj,ii
      real p(nx,ny),q(mx,my)
      real pjm(mx),pj(mx),pjp(mx),pjpp(mx)
      real dym(my),dy(my),dyp(my),dypp(my)
      real dxm(mx),dx(mx),dxp(mx),dxpp(mx)
      if (intpol(1).eq.1) then
c
c     linear in x
c
      jsave = -3
      do jj=1,my
c
c       load closest four j lines containing interpolate on xx mesh
c       for j-1,j,j+1,j+2 in pjm,pj,pjp,pjpp
c
	j = jy(jj)
	if (j.eq.jsave) then
c
c       j pointer has not moved since last pass (no updates or interpolation)
c
	else if (j.eq.jsave+1) then
c
c       update j-1,j,j+1 and interpolate j+2
c
	  do ii=1,mx
	    pjm(ii) = pj(ii)
	    pj(ii) = pjp(ii)
	    pjp(ii) = pjpp(ii)
	  end do
	  call lint1(nx,p(1,j+2),mx,pjpp,ix,dx)
	else if (j.eq.jsave+2) then
c
c     update j-1,j and interpolate j+1,j+2
c
	  do ii=1,mx
	    pjm(ii) = pjp(ii)
	    pj(ii) = pjpp(ii)
	  end do
	  call lint1(nx,p(1,j+1),mx,pjp,ix,dx)
	  call lint1(nx,p(1,j+2),mx,pjpp,ix,dx)
	else if (j.eq.jsave+3) then
c
c       update j-1 and interpolate j,j+1,j+2
c
	  do ii=1,mx
	    pjm(ii) = pjpp(ii)
	  end do
	  call lint1(nx,p(1,j),mx,pj,ix,dx)
	  call lint1(nx,p(1,j+1),mx,pjp,ix,dx)
	  call lint1(nx,p(1,j+2),mx,pjpp,ix,dx)
	else
c
c       interpolate all four j-1,j,j+1,j+2
c
	  call lint1(nx,p(1,j-1),mx,pjm,ix,dx)
	  call lint1(nx,p(1,j),mx,pj,ix,dx)
	  call lint1(nx,p(1,j+1),mx,pjp,ix,dx)
	  call lint1(nx,p(1,j+2),mx,pjpp,ix,dx)
	end if
c
c     save j pointer for next pass
c
	jsave = j
c
c     cubically interpolate q(ii,jj) from pjm,pj,pjp,pjpp in y direction
c
	do ii=1,mx
	  q(ii,jj) = dym(jj)*pjm(ii)+dy(jj)*pj(ii)+dyp(jj)*pjp(ii)+
     +               dypp(jj)*pjpp(ii)
	end do
      end do
      return

      else
c
c     cubic in x
c
	jsave = -3
	do jj=1,my
c
c       load closest four j lines containing interpolate on xx mesh
c       for j-1,j,j+1,j+2 in pjm,pj,pjp,pjpp
c
	  j = jy(jj)
	  if (j.eq.jsave) then
c
c         j pointer has not moved since last pass (no updates or interpolation)
c
	  else if (j.eq.jsave+1) then
c
c         update j-1,j,j+1 and interpolate j+2
c
	    do ii=1,mx
	      pjm(ii) = pj(ii)
	      pj(ii) = pjp(ii)
	      pjp(ii) = pjpp(ii)
	    end do
	    call cubt1(nx,p(1,j+2),mx,pjpp,ix,dxm,dx,dxp,dxpp)
	  else if (j.eq.jsave+2) then
c
c         update j-1,j and interpolate j+1,j+2
c
	    do ii=1,mx
	      pjm(ii) = pjp(ii)
	      pj(ii) = pjpp(ii)
	    end do
	    call cubt1(nx,p(1,j+1),mx,pjp,ix,dxm,dx,dxp,dxpp)
	    call cubt1(nx,p(1,j+2),mx,pjpp,ix,dxm,dx,dxp,dxpp)
	  else if (j.eq.jsave+3) then
c
c         update j-1 and interpolate j,j+1,j+2
c
	    do ii=1,mx
	      pjm(ii) = pjpp(ii)
	    end do
	    call cubt1(nx,p(1,j),mx,pj,ix,dxm,dx,dxp,dxpp)
	    call cubt1(nx,p(1,j+1),mx,pjp,ix,dxm,dx,dxp,dxpp)
	    call cubt1(nx,p(1,j+2),mx,pjpp,ix,dxm,dx,dxp,dxpp)
	  else
c
c         interpolate all four j-1,j,j+1,j+2
c
	    call cubt1(nx,p(1,j-1),mx,pjm,ix,dxm,dx,dxp,dxpp)
	    call cubt1(nx,p(1,j),mx,pj,ix,dxm,dx,dxp,dxpp)
	    call cubt1(nx,p(1,j+1),mx,pjp,ix,dxm,dx,dxp,dxpp)
	    call cubt1(nx,p(1,j+2),mx,pjpp,ix,dxm,dx,dxp,dxpp)
	  end if
c
c       save j pointer for next pass
c
	jsave = j
c
c       cubically interpolate q(ii,jj) from pjm,pj,pjp,pjpp in y direction
c
	do ii=1,mx
	  q(ii,jj) = dym(jj)*pjm(ii)+dy(jj)*pj(ii)+dyp(jj)*pjp(ii)+
     +               dypp(jj)*pjpp(ii)
	end do
      end do
      return
      end if
      end
