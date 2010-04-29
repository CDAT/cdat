c
c
c ... file rgrd4.f
c
c     this file contains documentation for subroutine rgrd4 followed by
c     fortran code for rgrd4 and additional subroutines.
c
c ... author
c
c     John C. Adams (NCAR 1999)
c
c ... subroutine rgrd4(nx,ny,nz,nt,x,y,z,t,p,mx,my,mz,mt,xx,yy,zz,tt,q,
c    +                 intpol,w,lw,iw,liw,ier)
c
c ... purpose
c
c     subroutine rgrd4 interpolates the values p(i,j,k,l) on the orthogonal
c     grid
c
c       (x(i),y(j),z(k),t(l))
c
c       for i=1,...,nx;j=1,...,ny;k=1,...,nz;l=1,...,nt
c
c
c     onto q(ii,jj,kk,ll) on the orthogonal grid
c
c
c       (xx(ii),yy(jj),zz(kk),tt(ll))
c
c       for ii=1,...,mx;jj=1,...,my;kk=1,...,mz;ll=1,...,mt
c
c ... language
c
c     coded in portable FORTRAN77 and FORTRAN90
c
c ... test program
c
c     file trgrd4.f on regridpack includes a test program for subroutine rgrd4
c
c ... method
c
c     linear or cubic interpolation is used (independently) in
c     each direction (see argument intpol).
c
c ... required files
c
c     files rgrd3.f,rgrd2.f and rgrd1.f must be loaded with rgrd4.f.  they
c     include subroutines called by the routines in rgrd4.f
c
c ... requirements
c
c     each of the x,y,z,t grids must be strictly montonically increasing
c     and each of the xx,yy,zz,tt grids must be montonically increasing
c     (see ier = 4).  in addition the (X,Y,Z,T) region of the q grid
c
c      [xx(1),xx(mx)] X [yy(1),yy(my)] X [zz(1),zz(mz)] X [tt(1),tt(my)]
c
c     must lie within the (X,Y,Z,T) region of the p grid
c
c      [x(1),x(nx)] X [y(1),y(ny)] X [z(1),z(nz)] X [t(1),t(nt)].
c
c     extrapolation is not allowed (see ier=3).  if these (X,Y,Z,T)
c     regions are identical and the orthogonal grids are UNIFORM
c     in each direction then subroutine rgrd4u (see file rgrd4u.f)
c     should be used instead of rgrd4.
c
c ... efficiency
c
c     inner most loops in regridpack software vectorize. If
c     the integer arguments mx,my,mz,mt (see below) have different values,
c     optimal vectorization will be achieved if they are arranged so that
c     mx > my > mz > mt.
c
c *** input arguments
c
c ... nx
c
c     the integer dimension of the grid vector x and the first dimension of p.
c     nx > 1 if intpol(1) = 1 or nx > 3 if intpol(1) = 3 is required.
c
c ... ny
c
c     the integer dimension of the grid vector y and the second dimension of p.
c     ny > 1 if intpol(2) = 1 or ny > 3 if intpol(2) = 3 is required.
c
c ... nz
c
c     the integer dimension of the grid vector z and the third dimension of p.
c     nz > 1 if intpol(3) = 1 or nz > 3 if intpol(3) = 3 is required.
c
c ... nt
c
c     the integer dimension of the grid vector t and the fourth dimension of p.
c     nt > 1 if intpol(4) = 1 or nt > 3 if intpol(4) = 3 is required.
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
c ... z
c
c     a real nz vector of strictly increasing values which defines the z
c     portion of the orthogonal grid on which p is given
c
c ... t
c
c     a real nt vector of strictly increasing values which defines the t
c     portion of the orthogonal grid on which p is given
c
c ... p
c
c     a real nx by ny by nz by nt array of values given on the (x,y,z,t) grid
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
c ... mz
c
c     the integer dimension of the grid vector zz and the third dimension
c     of q.  mz > 0 is required.
c
c ... mt
c
c     the integer dimension of the grid vector tt and the fourth dimension
c     of q.  mt > 0 is required.
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
c ... zz
c
c     a real mz vector of increasing values which defines the z portion of the
c     orthogonal grid on which q is defined.  zz(1) < z(1) or zz(mz) > z(nz)
c     is not allowed (see ier = 3)
c
c ... tt
c
c     a real mt vector of increasing values which defines the t portion of the
c     orthogonal grid on which q is defined.  tt(1) < t(1) or tt(mt) > t(nt)
c     is not allowed (see ier = 3)
c
c ... intpol
c
c     an integer vector of dimension 4 which sets linear or cubic
c     interpolation in each of the x,y,z,t directions as follows:
c
c        intpol(1) = 1 sets linear interpolation in the x direction
c        intpol(1) = 3 sets cubic interpolation in the x direction.
c
c        intpol(2) = 1 sets linear interpolation in the y direction
c        intpol(2) = 3 sets cubic interpolation in the y direction.
c
c        intpol(3) = 1 sets linear interpolation in the z direction
c        intpol(3) = 3 sets cubic interpolation in the z direction.
c
c        intpol(4) = 1 sets linear interpolation in the t direction
c        intpol(4) = 3 sets cubic interpolation in the t direction.
c
c     values other than 1 or 3 in intpol are not allowed (ier = 6).
c
c ... w
c
c     a real work space of length at least lw which must be provided in the
c     routine calling rgrd4
c
c ... lw
c
c     the integer length of the real work space w.  let
c
c          lwx =   mx            if intpol(1) = 1
c          lwx = 4*mx            if intpol(1) = 3
c
c          lwy = my+2*mx         if intpol(2) = 1
c          lwy = 4*(my+mx)       if intpol(2) = 3
c
c          lwz = 2*mx*my+mz      if intpol(3) = 1
c          lwz = 4*(mx*my+mz)    if intpol(3) = 3
c
c          lwt = 2*mx*my*mz+mt   if intpol(4) = 1
c          lwt = 4*(mx*my*mz+mt) if intpol(4) = 3
c
c     then lw must be greater than or equal to lwx+lwy+lwz+lwt
c
c ... iw
c
c     an integer work space of length at least liw which must be provided in the
c     routine calling rgrd4
c
c ... liw
c
c     the integer length of the integer work space iw.  liw must be at least mx+my+mz+mt
c
c
c *** output arguments
c
c
c ... q
c
c     a real mx by my by mz by mt array of values on the (xx,yy,zz,tt) grid
c     which are interpolated from p on the (x,y,z,t) grid
c
c ... ier
c
c     an integer error flag set as follows:
c
c     ier = 0 if no errors in input arguments are detected
c
c     ier = 1 if  min0(mx,my,mz,mt) < 1
c
c     ier = 2 if nx < 2 when intpol(1)=1 or nx < 4 when intpol(1)=3 (or)
c                ny < 2 when intpol(2)=1 or ny < 4 when intpol(2)=3 (or)
c                nz < 2 when intpol(3)=1 or nz < 4 when intpol(3)=3 (or)
c                nt < 2 when intpol(4)=1 or nt < 4 when intpol(4)=3
c
c     ier = 3 if xx(1) < x(1) or x(nx) < xx(mx) (or)
c                yy(1) < y(1) or y(ny) < yy(my) (or)
c                zz(1) < z(1) or z(nz) < zz(mz) (or)
c                tt(1) < t(1) or t(nt) < tt(mt)
c
c *** to avoid this flag when end points are intended to be the
c     same but may differ slightly due to roundoff error, they
c     should be set exactly in the calling routine (e.g., if both
c     grids have the same y boundaries then yy(1)=y(1) and yy(my)=y(ny)
c     should be set before calling rgrd4)
c
c     ier = 4 if one of the grids x,y,z,t is not strictly monotonically
c             increasing or if one of the grids xx,yy,zz,tt is not
c             montonically increasing.  more precisely if:
c
c             x(i+1) <= x(i) for some i such that 1 <= i < nx (or)
c
c             y(j+1) <= y(j) for some j such that 1 <= j < ny (or)
c
c             z(k+1) <= z(k) for some k such that 1 <= k < nz (or)
c
c             t(l+1) <= t(l) for some l such that 1 <= l < nt (or)
c
c             xx(ii+1) < xx(ii) for some ii such that 1 <= ii < mx (or)
c
c             yy(jj+1) < yy(jj) for some jj such that 1 <= jj < my (or)
c
c             zz(kk+1) < zz(k)  for some kk such that 1 <= kk < mz (or)
c
c             tt(ll+1) < tt(l)  for some ll such that 1 <= ll < mt
c
c     ier = 5 if lw or liw is too small (insufficient work space)
c
c     ier = 6 if any of intpol(1),intpol(2),intpol(3),intpol(4)
c             is not equal to 1 or 3
c
c ************************************************************************
c
c     end of rgrd4 documentation, fortran code follows:
c
c ************************************************************************
c
      subroutine rgrd4(nx,ny,nz,nt,x,y,z,t,p,mx,my,mz,mt,xx,yy,zz,
     +                 tt,q,intpol,w,lw,iw,liw,ier)
      implicit none
      integer nx,ny,nz,nt,mx,my,mz,mt,lw,liw,ier
      integer iw(liw),intpol(4)
      real x(nx),y(ny),z(nz),t(nt),p(nx,ny,nz,nt),w(lw)
      real xx(mx),yy(my),zz(mz),tt(mt),q(mx,my,mz,mt)
      integer l2,l3,l4,l5,l6,l7,l8,l9
      integer k2,k3,k4,k5,k6,k7,k8,k9
      integer j2,j3,j4,j5,j6,j7,j8,j9
      integer i2,i3,i4,i5
      integer lwx,lwy,lwz,lwt,mxmy,mxmymz
      integer ii,jj,kk,ll,i,j,k,l
      integer jy,kz,lt
c
c     check input arguments
c
      ier = 1
c
c     check (xx,yy,zz,tt) grid resolution
c
      if (min0(mx,my,mz,mt) .lt. 1) return
c
c     check intpol
c
      ier = 6
      if (intpol(1).ne.1 .and. intpol(1).ne.3) return
      if (intpol(2).ne.1 .and. intpol(2).ne.3) return
      if (intpol(3).ne.1 .and. intpol(3).ne.3) return
      if (intpol(4).ne.1 .and. intpol(4).ne.3) return
c
c     check (x,y,z,t) grid resolution
c
      ier = 2
      if (intpol(1).eq.1 .and. nx.lt.2) return
      if (intpol(1).eq.3 .and. nx.lt.4) return
      if (intpol(2).eq.1 .and. ny.lt.2) return
      if (intpol(2).eq.3 .and. ny.lt.4) return
      if (intpol(3).eq.1 .and. nz.lt.2) return
      if (intpol(3).eq.3 .and. nz.lt.4) return
      if (intpol(4).eq.1 .and. nt.lt.2) return
      if (intpol(4).eq.3 .and. nt.lt.4) return
c
c     check work space length input and set minimum
c
      ier = 5
      mxmy = mx*my
      mxmymz = mxmy*mz
      if (intpol(1).eq.1) then
	lwx = mx
      else
	lwx = 4*mx
      end if
      if (intpol(2).eq.1) then
	lwy = (my+2*mx)
      else
	lwy = 4*(mx+my)
      end if
      if (intpol(3).eq.1) then
	lwz = (2*mxmy+mz)
      else
	lwz = 4*(mxmy+mz)
      end if
      if (intpol(4).eq.1) then
	lwt = (2*mxmymz+mt)
      else
	lwt = 4*(mxmymz+mt)
      end if
      if (lw .lt. lwx+lwy+lwz+lwt) return
      if (liw .lt. mx+my+mz+mt) return
c
c     check (xx,yy,zz,tt) grid contained in (x,y,z,t) grid
c
      ier = 3
      if (xx(1).lt.x(1) .or. xx(mx).gt.x(nx)) return
      if (yy(1).lt.y(1) .or. yy(my).gt.y(ny)) return
      if (zz(1).lt.z(1) .or. zz(mz).gt.z(nz)) return
      if (tt(1).lt.t(1) .or. tt(mt).gt.t(nt)) return
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
      do k=2,nz
	if (z(k-1).ge.z(k)) return
      end do
      do l=2,nt
	if (t(l-1).ge.t(l)) return
      end do
      do ii=2,mx
	if (xx(ii-1).gt.xx(ii)) return
      end do
      do jj=2,my
	if (yy(jj-1).gt.yy(jj)) return
      end do
      do kk=2,mz
	if (zz(kk-1).gt.zz(kk)) return
      end do
      do ll=2,mt
	if (tt(ll-1).gt.tt(ll)) return
      end do
c
c     arguments o.k.
c
      ier = 0
c
c     set pointers for integer work space iw
c
      jy = mx+1
      kz = mx+my+1
      lt = mx+my+mz+1

      if (intpol(4).eq.1) then
c
c     linearly interpolate in nt, set work space pointers and scales
c
	l2 = 1
	l3 = l2
	l4 = l3+mt
	l5 = l4
	l6 = l5
	l7 = l6
	l8 = l7+mxmymz
	l9 = l8+mxmymz
	call linmx(nt,t,mt,tt,iw(lt),w(l3))
	k2 = l9

	if (intpol(3).eq.1) then
c     linear in z
	  k3 = k2
	  k4 = k3+mz
	  k5 = k4
	  k6 = k5
	  k7 = k6
	  k8 = k7+mxmy
	  k9 = k8+mxmy
	  call linmx(nz,z,mz,zz,iw(kz),w(k3))
	  j2 = k9
	else
c     cubic in z
	  k3 = k2+mz
	  k4 = k3+mz
	  k5 = k4+mz
	  k6 = k5+mz
	  k7 = k6+mxmy
	  k8 = k7+mxmy
	  k9 = k8+mxmy
	  call cubnmx(nz,z,mz,zz,iw(kz),w(k2),w(k3),w(k4),w(k5))
	  j2 = k9+mxmy
	end if

	if (intpol(2) .eq. 1) then
c     linear in y
	  j3 = j2
	  j4 = j3+my
	  j5 = j4
	  j6 = j5
	  j7 = j6
	  j8 = j7+mx
	  j9 = j8+mx
	  call linmx(ny,y,my,yy,iw(jy),w(j3))
	  i2 = j9
	else
c     cubic in y
	  j3 = j2+my
	  j4 = j3+my
	  j5 = j4+my
	  j6 = j5+my
	  j7 = j6+mx
	  j8 = j7+mx
	  j9 = j8+mx
	  call cubnmx(ny,y,my,yy,iw(jy),w(j2),w(j3),w(j4),w(j5))
	  i2 = j9+mx
	end if

	if (intpol(1) .eq. 1) then
c     linear in x
	  i3 = i2
	  i4 = i3
	  i5 = i4
	  call linmx(nx,x,mx,xx,iw,w(i3))
	else
c     cubic in x
	  i3 = i2+mx
	  i4 = i3+mx
	  i5 = i4+mx
	  call cubnmx(nx,x,mx,xx,iw,w(i2),w(i3),w(i4),w(i5))
	end if
c
c     linearly interpolate in t
c
	call lint4(nx,ny,nz,nt,p,mx,my,mz,mt,mxmy,mxmymz,q,intpol,
     +  iw(lt),w(l3),w(l7),w(l8),
     +  iw(kz),w(k2),w(k3),w(k4),w(k5),w(k6),w(k7),w(k8),w(k9),
     +  iw(jy),w(j2),w(j3),w(j4),w(j5),w(j6),w(j7),w(j8),w(j9),
     +  iw,w(i2),w(i3),w(i4),w(i5))
	return

      else
c
c     cubically interpolate in t
c
	l2 = 1
	l3 = l2+mt
	l4 = l3+mt
	l5 = l4+mt
	l6 = l5+mt
	l7 = l6+mxmymz
	l8 = l7+mxmymz
	l9 = l8+mxmymz
	call cubnmx(nt,t,mt,tt,iw(lt),w(l2),w(l3),w(l4),w(l5))
	k2 = l9+mxmymz

	if (intpol(3).eq.1) then
c     linear in z
	  k3 = k2
	  k4 = k3+mz
	  k5 = k4
	  k6 = k5
	  k7 = k6
	  k8 = k7+mxmy
	  k9 = k8+mxmy
	  call linmx(nz,z,mz,zz,iw(kz),w(k3))
	  j2 = k9
	else
c     cubic in z
	  k3 = k2+mz
	  k4 = k3+mz
	  k5 = k4+mz
	  k6 = k5+mz
	  k7 = k6+mxmy
	  k8 = k7+mxmy
	  k9 = k8+mxmy
	  call cubnmx(nz,z,mz,zz,iw(kz),w(k2),w(k3),w(k4),w(k5))
	  j2 = k9+mxmy
	end if

	if (intpol(2) .eq. 1) then
	  j3 = j2
	  j4 = j3+my
	  j5 = j4
	  j6 = j5
	  j7 = j6
	  j8 = j7+mx
	  j9 = j8+mx
	  call linmx(ny,y,my,yy,iw(jy),w(j3))
	  i2 = j9
	else
	  j3 = j2+my
	  j4 = j3+my
	  j5 = j4+my
	  j6 = j5+my
	  j7 = j6+mx
	  j8 = j7+mx
	  j9 = j8+mx
	  call cubnmx(ny,y,my,yy,iw(jy),w(j2),w(j3),w(j4),w(j5))
	  i2 = j9+mx
	end if
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
c
c     cubically interpolate in t
c
	call cubt4(nx,ny,nz,nt,p,mx,my,mz,mt,mxmy,mxmymz,q,intpol,
     +  iw(lt),w(l2),w(l3),w(l4),w(l5),w(l6),w(l7),w(l8),w(l9),
     +  iw(kz),w(k2),w(k3),w(k4),w(k5),w(k6),w(k7),w(k8),w(k9),
     +  iw(jy),w(j2),w(j3),w(j4),w(j5),w(j6),w(j7),w(j8),w(j9),
     +  iw,w(i2),w(i3),w(i4),w(i5))
	return

      end if
      end

      subroutine lint4(nx,ny,nz,nt,p,mx,my,mz,mt,mxmy,mxmymz,q,intpol,
     +lt,dt,pt,ptp,kz,dzm,dz,dzp,dzpp,pkm,pk,pkp,pkpp,
     +jy,dym,dy,dyp,dypp,pjm,pj,pjp,pjpp,ix,dxm,dx,dxp,dxpp)
c
c     linearly interpolate in t direction
c
      implicit none
      integer nx,ny,nz,nt,mx,my,mz,mt,mxmy,mxmymz,lsave,ll,l,iijjkk
      integer lt(mt),kz(mz),jy(my),ix(mx),intpol(4)
      real p(nx,ny,nz,nt),q(mxmymz,mt)
      real dt(mt),pt(mxmymz),ptp(mxmymz)
      real dzm(mz),dz(mz),dzp(mz),dzpp(mz)
      real pkm(mxmy),pk(mxmy),pkp(mxmy),pkpp(mxmy)
      real dym(my),dy(my),dyp(my),dypp(my)
      real pjm(mx),pj(mx),pjp(mx),pjpp(mx)
      real dxm(mx),dx(mx),dxp(mx),dxpp(mx)

      if (intpol(3) .eq. 1) then
c
c     linear in z
c
      lsave = -1
      do ll=1,mt
	l = lt(ll)
	if (l.eq.lsave) then
c
c     l pointer has not moved since last pass (no updates or interpolation)
c
	else if (l.eq.lsave+1) then
c
c     update l and interpolate l+1
c
	  do iijjkk=1,mxmymz
	    pt(iijjkk) = ptp(iijjkk)
	  end do
	  call lint3(nx,ny,nz,p(1,1,1,l+1),mx,my,mxmy,mz,ptp,intpol,
     +    kz,dz,pk,pkp,jy,dym,dy,dyp,dypp,pjm,pj,pjp,pjpp,ix,
     +    dxm,dx,dxp,dxpp)
	else
c
c     interpolate l,l+1 in pt,ptp on xx,yy,zz mesh
c
      call lint3(nx,ny,nz,p(1,1,1,l),mx,my,mxmy,mz,pt,intpol,kz,
     +dz,pk,pkp,jy,dym,dy,dyp,dypp,pjm,pj,pjp,pjpp,ix,dxm,dx,dxp,dxpp)
      call lint3(nx,ny,nz,p(1,1,1,l+1),mx,my,mxmy,mz,ptp,intpol,kz,
     +dz,pk,pkp,jy,dym,dy,dyp,dypp,pjm,pj,pjp,pjpp,ix,dxm,dx,dxp,dxpp)
	end if
c
c     save l pointer for next pass
c
      lsave = l
c
c     linearly interpolate q(ii,jj,,kk,ll) from pt,ptp in t direction
c
	do iijjkk=1,mxmymz
	  q(iijjkk,ll) = pt(iijjkk)+dt(ll)*(ptp(iijjkk)-pt(iijjkk))
	end do
      end do
      return

      else
c
c     cubic in z
c
      lsave = -1
      do ll=1,mt
	l = lt(ll)
	if (l.eq.lsave) then
c
c     l pointer has not moved since last pass (no updates or interpolation)
c
	else if (l.eq.lsave+1) then
c
c     update l and interpolate l+1
c
	do iijjkk=1,mxmymz
	  pt(iijjkk) = ptp(iijjkk)
	end do
	call cubt3(nx,ny,nt,p(1,1,1,l+1),mx,my,mxmy,mz,ptp,intpol,
     +  kz,dzm,dz,dzp,dzpp,pkm,pk,pkp,pkpp,
     +  jy,dym,dy,dyp,dypp,pjm,pj,pjp,pjpp,ix,dxm,dx,dxp,dxpp)
	else
c
c     interpolate l,l+1 in pt,ptp on xx,yy,zz mesh
c
	call cubt3(nx,ny,nt,p(1,1,1,l),mx,my,mxmy,mz,pt,intpol,
     +  kz,dzm,dz,dzp,dzpp,pkm,pk,pkp,pkpp,
     +  jy,dym,dy,dyp,dypp,pjm,pj,pjp,pjpp,ix,dxm,dx,dxp,dxpp)
	call cubt3(nx,ny,nt,p(1,1,1,l+1),mx,my,mxmy,mz,ptp,intpol,
     +  kz,dzm,dz,dzp,dzpp,pkm,pk,pkp,pkpp,
     +  jy,dym,dy,dyp,dypp,pjm,pj,pjp,pjpp,ix,dxm,dx,dxp,dxpp)
	end if
c
c     save l pointer for next pass
c
	lsave = l
c
c     linearly interpolate q(ii,jj,kk,ll) from pt,ptp in t direction
c
	do iijjkk=1,mxmymz
	  q(iijjkk,ll) = pt(iijjkk)+dt(ll)*(ptp(iijjkk)-pt(iijjkk))
	end do

      end do
      return

      end if
      end

      subroutine cubt4(nx,ny,nz,nt,p,mx,my,mz,mt,mxmy,mxmymz,q,intpol,
     +lt,dtm,dt,dtp,dtpp,ptm,pt,ptp,ptpp,
     +kz,dzm,dz,dzp,dzpp,pkm,pk,pkp,pkpp,
     +jy,dym,dy,dyp,dypp,pjm,pj,pjp,pjpp,
     +ix,dxm,dx,dxp,dxpp)
c
c     cubically interpolate in t
c
      implicit none
      integer nx,ny,nz,nt,mx,my,mz,mt,mxmy,mxmymz,lsave,ll,l,iijjkk
      integer lt(mt),kz(mz),jy(my),ix(mx),intpol(4)
      real p(nx,ny,nz,nt),q(mxmymz,mt)
      real dtm(mt),dt(mt),dtp(mt),dtpp(mt)
      real ptm(mxmymz),pt(mxmymz),ptp(mxmymz),ptpp(mxmymz)
      real dzm(mz),dz(mz),dzp(mz),dzpp(mz)
      real pkm(mxmy),pk(mxmy),pkp(mxmy),pkpp(mxmy)
      real dym(my),dy(my),dyp(my),dypp(my)
      real pjm(mx),pj(mx),pjp(mx),pjpp(mx)
      real dxm(mx),dx(mx),dxp(mx),dxpp(mx)
      if (intpol(3) .eq. 1) then
c
c     linear in z
c
      lsave = -3
      do ll=1,mt
	l = lt(ll)
	if (l.eq.lsave) then
c
c     l pointer has not moved since last pass (no updates or interpolation)
c
	else if (l.eq.lsave+1) then
c
c     update l-1,l,l+1 and interpolate l+2
c
	do iijjkk=1,mxmymz
	  ptm(iijjkk) = pt(iijjkk)
	  pt(iijjkk) = ptp(iijjkk)
	  ptp(iijjkk) = ptpp(iijjkk)
	end do
	call lint3(nx,ny,nz,p(1,1,1,l+2),mx,my,mxmy,mz,ptpp,intpol,kz,dz,
     +  pk,pkp,jy,dym,dy,dyp,dypp,pjm,pj,pjp,pjpp,ix,dxm,dx,dxp,dxpp)
	else if (l.eq.lsave+2) then
c
c     update l-1,l and interpolate l+1,l+2
c
	do iijjkk=1,mxmymz
	  ptm(iijjkk) = ptp(iijjkk)
	  pt(iijjkk) = ptpp(iijjkk)
	end do
	call lint3(nx,ny,nz,p(1,1,1,l+1),mx,my,mxmy,mz,ptp,intpol,kz,dz,
     +  pk,pkp,jy,dym,dy,dyp,dypp,pjm,pj,pjp,pjpp,ix,dxm,dx,dxp,dxpp)
	call lint3(nx,ny,nz,p(1,1,1,l+2),mx,my,mxmy,mz,ptpp,intpol,kz,dz,
     +  pk,pkp,jy,dym,dy,dyp,dypp,pjm,pj,pjp,pjpp,ix,dxm,dx,dxp,dxpp)
      else if (l.eq.lsave+3) then
c
c     update l-1 and interpolate l,l+1,l+2
c
	do iijjkk=1,mxmymz
	  ptm(iijjkk) = ptpp(iijjkk)
	end do
	call lint3(nx,ny,nz,p(1,1,1,l),mx,my,mxmy,mz,pt,intpol,kz,dz,
     +  pk,pkp,jy,dym,dy,dyp,dypp,pjm,pj,pjp,pjpp,ix,dxm,dx,dxp,dxpp)
	call lint3(nx,ny,nz,p(1,1,1,l+1),mx,my,mxmy,mz,ptp,intpol,kz,dz,
     +  pk,pkp,jy,dym,dy,dyp,dypp,pjm,pj,pjp,pjpp,ix,dxm,dx,dxp,dxpp)
	call lint3(nx,ny,nz,p(1,1,1,l+2),mx,my,mxmy,mz,ptpp,intpol,kz,dz,
     +  pk,pkp,jy,dym,dy,dyp,dypp,pjm,pj,pjp,pjpp,ix,dxm,dx,dxp,dxpp)
	else
c
c     interpolate all four l-1,l,l+1,l+2
c
	call lint3(nx,ny,nz,p(1,1,1,l-1),mx,my,mxmy,mz,ptm,intpol,kz,dz,
     +  pk,pkp,jy,dym,dy,dyp,dypp,pjm,pj,pjp,pjpp,ix,dxm,dx,dxp,dxpp)
	call lint3(nx,ny,nz,p(1,1,1,l),mx,my,mxmy,mz,pt,intpol,kz,dz,
     +  pk,pkp,jy,dym,dy,dyp,dypp,pjm,pj,pjp,pjpp,ix,dxm,dx,dxp,dxpp)
	call lint3(nx,ny,nz,p(1,1,1,l+1),mx,my,mxmy,mz,ptp,intpol,kz,dz,
     +  pk,pkp,jy,dym,dy,dyp,dypp,pjm,pj,pjp,pjpp,ix,dxm,dx,dxp,dxpp)
	call lint3(nx,ny,nz,p(1,1,1,l+2),mx,my,mxmy,mz,ptpp,intpol,kz,dz,
     +  pk,pkp,jy,dym,dy,dyp,dypp,pjm,pj,pjp,pjpp,ix,dxm,dx,dxp,dxpp)
	end if
c
c     save l pointer for next pass
c
	lsave = l
c
c     cubically interpolate q(ii,jj,kk,ll) from ptm,pt,ptp,ptpp in t direction
c
	do iijjkk=1,mxmymz
	  q(iijjkk,ll) = dtm(ll)*ptm(iijjkk) + dt(ll)*pt(iijjkk) +
     +                   dtp(ll)*ptp(iijjkk) + dtpp(ll)*ptpp(iijjkk)
	end do
      end do
      return

      else
c
c     cubic in z
c
      lsave = -3
      do ll=1,mt
	l = lt(ll)
	if (l.eq.lsave) then
c
c     l pointer has not moved since last pass (no updates or interpolation)
c
	else if (l.eq.lsave+1) then
c
c     update l-1,l,l+1 and interpolate l+2
c
	do iijjkk=1,mxmymz
	  ptm(iijjkk) = pt(iijjkk)
	  pt(iijjkk) = ptp(iijjkk)
	  ptp(iijjkk) = ptpp(iijjkk)
	end do
      call cubt3(nx,ny,nz,p(1,1,1,l+2),mx,my,mxmy,mz,ptpp,intpol,kz,dzm,
     +dz,dzp,dzpp,pkm,pk,pkp,pkpp,jy,dym,dy,dyp,dypp,pjm,pj,pjp,pjpp,
     +ix,dxm,dx,dxp,dxpp)
	else if (l.eq.lsave+2) then
c
c     update l-1,l and interpolate l+1,l+2
c
	do iijjkk=1,mxmymz
	  ptm(iijjkk) = ptp(iijjkk)
	  pt(iijjkk) = ptpp(iijjkk)
	end do
      call cubt3(nx,ny,nz,p(1,1,1,l+1),mx,my,mxmy,mz,ptp,intpol,kz,dzm,
     +dz,dzp,dzpp,pkm,pk,pkp,pkpp,jy,dym,dy,dyp,dypp,pjm,pj,pjp,pjpp,
     +ix,dxm,dx,dxp,dxpp)
      call cubt3(nx,ny,nz,p(1,1,1,l+2),mx,my,mxmy,mz,ptpp,intpol,kz,dzm,
     +dz,dzp,dzpp,pkm,pk,pkp,pkpp,jy,dym,dy,dyp,dypp,pjm,pj,pjp,pjpp,
     +ix,dxm,dx,dxp,dxpp)
	else if (l.eq.lsave+3) then
c
c     update l-1 and interpolate l,l+1,l+2
c
	do iijjkk=1,mxmymz
	  ptm(iijjkk) = ptpp(iijjkk)
	end do
      call cubt3(nx,ny,nz,p(1,1,1,l),mx,my,mxmy,mz,pt,intpol,kz,dzm,
     +dz,dzp,dzpp,pkm,pk,pkp,pkpp,jy,dym,dy,dyp,dypp,pjm,pj,pjp,pjpp,
     +ix,dxm,dx,dxp,dxpp)
      call cubt3(nx,ny,nz,p(1,1,1,l+1),mx,my,mxmy,mz,ptp,intpol,kz,dzm,
     +dz,dzp,dzpp,pkm,pk,pkp,pkpp,jy,dym,dy,dyp,dypp,pjm,pj,pjp,pjpp,
     +ix,dxm,dx,dxp,dxpp)
      call cubt3(nx,ny,nz,p(1,1,1,l+2),mx,my,mxmy,mz,ptpp,intpol,kz,dzm,
     +dz,dzp,dzpp,pkm,pk,pkp,pkpp,jy,dym,dy,dyp,dypp,pjm,pj,pjp,pjpp,
     +ix,dxm,dx,dxp,dxpp)
	else
c
c     interpolate all four l-1,l,l+1,l+2
c
      call cubt3(nx,ny,nz,p(1,1,1,l-1),mx,my,mxmy,mz,ptm,intpol,kz,dzm,
     +dz,dzp,dzpp,pkm,pk,pkp,pkpp,jy,dym,dy,dyp,dypp,pjm,pj,pjp,pjpp,
     +ix,dxm,dx,dxp,dxpp)
      call cubt3(nx,ny,nz,p(1,1,1,l),mx,my,mxmy,mz,pt,intpol,kz,dzm,
     +dz,dzp,dzpp,pkm,pk,pkp,pkpp,jy,dym,dy,dyp,dypp,pjm,pj,pjp,pjpp,
     +ix,dxm,dx,dxp,dxpp)
      call cubt3(nx,ny,nz,p(1,1,1,l+1),mx,my,mxmy,mz,ptp,intpol,kz,dzm,
     +dz,dzp,dzpp,pkm,pk,pkp,pkpp,jy,dym,dy,dyp,dypp,pjm,pj,pjp,pjpp,
     +ix,dxm,dx,dxp,dxpp)
      call cubt3(nx,ny,nz,p(1,1,1,l+2),mx,my,mxmy,mz,ptpp,intpol,kz,dzm,
     +dz,dzp,dzpp,pkm,pk,pkp,pkpp,jy,dym,dy,dyp,dypp,pjm,pj,pjp,pjpp,
     +ix,dxm,dx,dxp,dxpp)
	end if
c
c     save l pointer for next pass
c
	lsave = l
c
c     cubically interpolate q(ii,jj,kk,ll) from ptm,pt,ptp,ptpp in t direction
c
	do iijjkk=1,mxmymz
	  q(iijjkk,ll) = dtm(ll)*ptm(iijjkk) + dt(ll)*pt(iijjkk) +
     +                   dtp(ll)*ptp(iijjkk) + dtpp(ll)*ptpp(iijjkk)
	end do
      end do
      return
      end if

      end
