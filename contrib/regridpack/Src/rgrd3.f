c
c
c ... file rgrd3.f
c
c     this file contains documentation for subroutine rgrd3 followed by
c     fortran code for rgrd3 and additional subroutines.
c
c ... author
c
c     John C. Adams (NCAR 1999)
c
c ... subroutine rgrd3(nx,ny,nz,x,y,z,p,mx,my,mz,xx,yy,zz,q,intpol,
c    +                 w,lw,iw,liw,ier)
c
c ... purpose
c
c     subroutine rgrd3 interpolates the values p(i,j,k) on the orthogonal
c     grid (x(i),y(j),z(k)) for i=1,...,nx; j=1,...,ny; k=1,...,nz
c     onto q(ii,jj,kk) on the orthogonal grid (xx(ii),yy(jj),zz(kk)) for
c     ii=1,...,mx; jj=1,...,my; kk=1,...,mz.
c
c ... language
c
c     coded in portable FORTRAN77 and FORTRAN90
c
c ... test program
c
c     file trgrd3.f on regridpack includes a test program for subroutine rgrd3
c
c ... method
c
c     linear or cubic interpolation is used (independently) in
c     each direction (see argument intpol).
c
c ... required files
c
c     files rgrd2.f and rgrd1.f must be loaded with rgrd3.f.  they
c     include subroutines called by the routines in rgrd3.f
c
c ... requirements
c
c     each of the x,y,z grids must be strictly montonically increasing
c     and each of the xx,yy,zz grids must be montonically increasing
c     (see ier = 4).  in addition the (X,Y,Z) region
c
c          [xx(1),xx(mx)] X [yy(1),yy(my)] X [zz(1),zz(mz)]
c
c     must lie within the (X,Y,Z) region
c
c          [x(1),x(nx)] X [y(1),y(ny)] X [z(1),z(nz)].
c
c     extrapolation is not allowed (see ier=3).  if these (X,Y,Z)
c     regions are identical and the orthogonal grids are UNIFORM
c     in each direction then subroutine rgrd3u (see file rgrd3u.f)
c     should be used instead of rgrd3.
c
c ... efficiency
c
c     inner most loops in regridpack software vectorize.  if
c     the integer arguments mx,my,mz (see below) have different values,
c     optimal vectorization will be achieved if mx > my > mz.
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
c ... p
c
c     a real nx by ny by nz array of values given on the (x,y,z) grid
c
c ... mx
c
c     the integer dimension of the grid vector xx and the first dimension of q.
c     mx > 0 is required.
c
c ... my
c
c     the integer dimension of the grid vector yy and the second dimension of q.
c     my > 0 is required.
c
c ... mz
c
c     the integer dimension of the grid vector zz and the third dimension of q.
c     mz > 0 is required.
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
c ... intpol
c
c     an integer vector of dimension 3 which sets linear or cubic
c     interpolation in each of the x,y,z directions as follows:
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
c     values other than 1 or 3 in intpol are not allowed (ier = 5).
c
c ... w
c
c     a real work space of length at least lw which must be provided in the
c     routine calling rgrd3
c
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
c          lwz = 2*mx*my+mz      if intpol(3) = 1
c          lwz = 4*(mx*my+mz)    if intpol(3) = 3
c
c     then lw must be greater than or equal to lwx+lwy+lwz
c
c ... iw
c
c     an integer work space of length at least liw which must be provided in the
c     routine calling rgrd3
c
c ... liw
c
c     the integer length of the integer work space iw.  liw must be at least mx+my+mz
c
c *** output arguments
c
c
c ... q
c
c     a real mx by my by mz array of values on the (xx,yy,zz) grid which are
c     interpolated from p on the (x,y,z) grid
c
c ... ier
c
c     an integer error flag set as follows:
c
c     ier = 0 if no errors in input arguments are detected
c
c     ier = 1 if  min0(mx,my,mz) < 1
c
c     ier = 2 if nx < 2 when intpol(1)=1 or nx < 4 when intpol(1)=3 (or)
c                ny < 2 when intpol(2)=1 or ny < 4 when intpol(2)=3 (or)
c                nz < 2 when intpol(3)=1 or nz < 4 when intpol(3)=3.
c
c     ier = 3 if xx(1) < x(1) or x(nx) < xx(mx) (or)
c                yy(1) < y(1) or y(ny) < yy(my) (or)
c                zz(1) < z(1) or z(nz) < zz(mz)
c
c *** to avoid this flag when end points are intended to be the
c     same but may differ slightly due to roundoff error, they
c     should be set exactly in the calling routine (e.g., if both
c     grids have the same y boundaries then yy(1)=y(1) and yy(my)=y(ny)
c     should be set before calling rgrd3)
c
c     ier = 4 if one of the grids x,y,z is not strictly monotonically
c             increasing or if one of the grids xx,yy,zz is not
c             montonically increasing.  more precisely if:
c
c             x(i+1) <= x(i) for some i such that 1 <= i < nx (or)
c
c             y(j+1) <= y(j) for some j such that 1 <= j < ny (or)
c
c             z(k+1) <= z(k) for some k such that 1 <= k < nz (or)
c
c             xx(ii+1) < xx(ii) for some ii such that 1 <= ii < mx (or)
c
c             yy(jj+1) < yy(jj) for some jj such that 1 <= jj < my (or)
c
c             zz(kk+1) < zz(k)  for some kk such that 1 <= kk < mz
c
c     ier = 5 if lw or liw is too small (insufficient work space)
c
c     ier = 6 if any of intpol(1),intpol(2),intpol(3) is not equal to 1 or 3
c
c ************************************************************************
c
c     end of rgrd3 documentation, fortran code follows:
c
c ************************************************************************
c
      subroutine rgrd3(nx,ny,nz,x,y,z,p,mx,my,mz,xx,yy,zz,q,intpol,
     +                 w,lw,iw,liw,ier)
      implicit none
      integer nx,ny,nz,mx,my,mz,lw,liw,ier
      integer lwx,lwy,lwz,jy,kz,mxmy
      real x(nx),y(ny),z(nz),p(nx,ny,nz)
      real xx(mx),yy(my),zz(mz),q(mx,my,mz)
      real w(lw)
      integer intpol(3),iw(liw)
      integer i,ii,j,jj,k,kk
      integer i2,i3,i4,i5
      integer j2,j3,j4,j5,j6,j7,j8,j9
      integer k2,k3,k4,k5,k6,k7,k8,k9
c
c     check input arguments
c
      ier = 1
c
c     check (xx,yy,zz) grid resolution
c
      if (min0(mx,my,mz) .lt. 1) return
c
c     check intpol
c
      ier = 6
      if (intpol(1).ne.1 .and. intpol(1).ne.3) return
      if (intpol(2).ne.1 .and. intpol(2).ne.3) return
      if (intpol(3).ne.1 .and. intpol(3).ne.3) return
c
c     check (x,y,z) grid resolution
c
      ier = 2
      if (intpol(1).eq.1 .and. nx.lt.2) return
      if (intpol(1).eq.3 .and. nx.lt.4) return
      if (intpol(2).eq.1 .and. ny.lt.2) return
      if (intpol(2).eq.3 .and. ny.lt.4) return
      if (intpol(3).eq.1 .and. nz.lt.2) return
      if (intpol(3).eq.3 .and. nz.lt.4) return
c
c     check work space length input and set minimum
c
      ier = 5
      mxmy = mx*my
      if (intpol(1).eq.1) then
	lwx = mx
      else
	lwx = 4*mx
      end if
      if (intpol(2).eq.1) then
	lwy = (my+2*mx)
      else
	lwy = 4*(my+mx)
      end if
      if (intpol(3).eq.1) then
	lwz = (2*mxmy+mz)
      else
	lwz = 4*(mxmy+mz)
      end if
      if (lw .lt. lwx+lwy+lwz) return
      if (liw .lt. mx+my+mz) return
c
c     check (xx,yy,zz) grid contained in (x,y,z) grid
c
      ier = 3
      if (xx(1).lt.x(1) .or. xx(mx).gt.x(nx)) return
      if (yy(1).lt.y(1) .or. yy(my).gt.y(ny)) return
      if (zz(1).lt.z(1) .or. zz(mz).gt.z(nz)) return
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
      do ii=2,mx
	if (xx(ii-1).gt.xx(ii)) return
      end do
      do jj=2,my
	if (yy(jj-1).gt.yy(jj)) return
      end do
      do kk=2,mz
	if (zz(kk-1).gt.zz(kk)) return
      end do
c
c     arguments o.k.
c
      ier = 0
      jy = mx+1
      kz = mx+my+1
      if (intpol(3).eq.1) then
c
c     linearly interpolate in nz, set work space pointers and scales
c
	k2 = 1
	k3 = k2
	k4 = k3+mz
	k5 = k4
	k6 = k5
	k7 = k6
	k8 = k7+mxmy
	k9 = k8+mxmy
	call linmx(nz,z,mz,zz,iw(kz),w(k3))
	j2 = k9
c
c     set indices and scales which depend on y interpolation
c
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
c
c     set indices and scales which depend on x interpolation
c
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
	call lint3(nx,ny,nz,p,mx,my,mxmy,mz,q,intpol,iw(kz),
     +  w(k3),w(k7),w(k8),iw(jy),w(j2),w(j3),w(j4),w(j5),w(j6),
     +  w(j7),w(j8),w(j9),iw,w(i2),w(i3),w(i4),w(i5))
	return
      else
c
c     cubically interpolate in z
c
	k2 = 1
	k3 = k2+mz
	k4 = k3+mz
	k5 = k4+mz
	k6 = k5+mz
	k7 = k6+mxmy
	k8 = k7+mxmy
	k9 = k8+mxmy
	call cubnmx(nz,z,mz,zz,iw(kz),w(k2),w(k3),w(k4),w(k5))
	j2 = k9+mxmy
c
c     set indices which depend on y interpolation
c
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
	call cubt3(nx,ny,nz,p,mx,my,mxmy,mz,q,intpol,
     +  iw(kz),w(k2),w(k3),w(k4),w(k5),w(k6),w(k7),w(k8),w(k9),
     +  iw(jy),w(j2),w(j3),w(j4),w(j5),w(j6),w(j7),w(j8),w(j9),
     +  iw,w(i2),w(i3),w(i4),w(i5))
	return

      end if

      end

      subroutine lint3(nx,ny,nz,p,mx,my,mxmy,mz,q,intpol,kz,
     +dz,pk,pkp,jy,dym,dy,dyp,dypp,pjm,pj,pjp,pjpp,ix,dxm,dx,dxp,dxpp)
c
c     linearly interpolate in z direction
c
      implicit none
      integer nx,ny,nz,mx,my,mz,mxmy
      real p(nx,ny,nz),q(mxmy,mz)
      real dz(mz),pk(mxmy),pkp(mxmy)
      real dym(my),dy(my),dyp(my),dypp(my)
      real pjm(mx),pj(mx),pjp(mx),pjpp(mx)
      real dxm(mx),dx(mx),dxp(mx),dxpp(mx)
      integer intpol(3),kz(mz),jy(my),ix(mx)
      integer k,kk,iijj,ksave
      if (intpol(2) .eq. 1) then
c
c     linear in y
c
	ksave = -1
	do kk=1,mz
	  k = kz(kk)
	  if (k.eq.ksave) then
c
c     k pointer has not moved since last pass (no updates or interpolation)
c
	  else if (k.eq.ksave+1) then
c
c     update k and interpolate k+1
c
	  do iijj=1,mxmy
	    pk(iijj) = pkp(iijj)
	  end do
	  call lint2(nx,ny,p(1,1,k+1),mx,my,pkp,intpol,jy,dy,
     +               pj,pjp,ix,dxm,dx,dxp,dxpp)
	  else
c
c     interpolate k,k+1 in pk,pkp on xx,yy mesh
c
	    call lint2(nx,ny,p(1,1,k),mx,my,pk,intpol,jy,dy,
     +                 pj,pjp,ix,dxm,dx,dxp,dxpp)
	    call lint2(nx,ny,p(1,1,k+1),mx,my,pkp,intpol,jy,dy,
     +                 pj,pjp,ix,dxm,dx,dxp,dxpp)
	  end if
c
c     save k pointer for next pass
c
	  ksave = k
c
c     linearly interpolate q(ii,jj,k) from pk,pkp in z direction
c
	  do iijj=1,mxmy
	    q(iijj,kk) = pk(iijj)+dz(kk)*(pkp(iijj)-pk(iijj))
	  end do
	end do
	return

      else
c
c     cubic in y
c
	ksave = -1
	do kk=1,mz
	  k = kz(kk)
	  if (k.eq.ksave) then
c
c     k pointer has not moved since last pass (no updates or interpolation)
c
	  else if (k.eq.ksave+1) then
c
c     update k and interpolate k+1
c
	  do iijj=1,mxmy
	    pk(iijj) = pkp(iijj)
	  end do
	  call cubt2(nx,ny,p(1,1,k+1),mx,my,pkp,intpol,
     +    jy,dym,dy,dyp,dypp,pjm,pj,pjp,pjpp,ix,dxm,dx,dxp,dxpp)
	  else
c
c     interpolate k,k+1 in pk,pkp on xx,yy mesh
c
	    call cubt2(nx,ny,p(1,1,k),mx,my,pk,intpol,
     +      jy,dym,dy,dyp,dypp,pjm,pj,pjp,pjpp,ix,dxm,dx,dxp,dxpp)
	    call cubt2(nx,ny,p(1,1,k+1),mx,my,pkp,intpol,
     +      jy,dym,dy,dyp,dypp,pjm,pj,pjp,pjpp,ix,dxm,dx,dxp,dxpp)
	  end if
c
c     save k pointer for next pass
c
	  ksave = k
c
c     linearly interpolate q(ii,jj,k) from pk,pkp in z direction
c
	  do iijj=1,mxmy
	    q(iijj,kk) = pk(iijj)+dz(kk)*(pkp(iijj)-pk(iijj))
	  end do
	end do
	return

      end if
      end

      subroutine cubt3(nx,ny,nz,p,mx,my,mxmy,mz,q,intpol,
     +kz,dzm,dz,dzp,dzpp,pkm,pk,pkp,pkpp,jy,dym,dy,dyp,dypp,pjm,pj,
     +pjp,pjpp,ix,dxm,dx,dxp,dxpp)
c
c     cubically interpolate in z
c
      implicit none
      integer nx,ny,nz,mx,my,mxmy,mz,k,kk,ksave,iijj
      real p(nx,ny,nz),q(mxmy,mz)
      real pkm(mxmy),pk(mxmy),pkp(mxmy),pkpp(mxmy)
      real pjm(mx),pj(mx),pjp(mx),pjpp(mx)
      real dzm(mz),dz(mz),dzp(mz),dzpp(mz)
      real dym(my),dy(my),dyp(my),dypp(my)
      real dxm(mx),dx(mx),dxp(mx),dxpp(mx)
      integer intpol(3),kz(mz),jy(my),ix(mx)
      if (intpol(2) .eq. 1) then
c
c       linear in y
c
	ksave = -3
	do kk=1,mz
	  k = kz(kk)
	  if (k.eq.ksave) then
c
c       k pointer has not moved since last pass (no updates or interpolation)
c
	  else if (k.eq.ksave+1) then
c
c       update k-1,k,k+1 and interpolate k+2
c
	  do iijj=1,mxmy
	    pkm(iijj) = pk(iijj)
	    pk(iijj) = pkp(iijj)
	    pkp(iijj) = pkpp(iijj)
	  end do
	  call lint2(nx,ny,p(1,1,k+2),mx,my,pkpp,
     +              intpol,jy,dy,pj,pjp,ix,dxm,dx,dxp,dxpp)
	  else if (k.eq.ksave+2) then
c
c       update k-1,k and interpolate k+1,k+2
c
	  do iijj=1,mxmy
	    pkm(iijj) = pkp(iijj)
	    pk(iijj) = pkpp(iijj)
	  end do
	  call lint2(nx,ny,p(1,1,k+1),mx,my,pkp,
     +               intpol,jy,dy,pj,pjp,ix,dxm,dx,dxp,dxpp)
	  call lint2(nx,ny,p(1,1,k+2),mx,my,pkpp,
     +               intpol,jy,dy,pj,pjp,ix,dxm,dx,dxp,dxpp)
	  else if (k.eq.ksave+3) then
c
c       update k-1 and interpolate k,k+1,k+2
c
	  do iijj=1,mxmy
	    pkm(iijj) = pkpp(iijj)
	  end do
	  call lint2(nx,ny,p(1,1,k),mx,my,pk,
     +               intpol,jy,dy,pj,pjp,ix,dxm,dx,dxp,dxpp)
	  call lint2(nx,ny,p(1,1,k+1),mx,my,pkp,
     +               intpol,jy,dy,pj,pjp,ix,dxm,dx,dxp,dxpp)
	  call lint2(nx,ny,p(1,1,k+2),mx,my,pkpp,
     +               intpol,jy,dy,pj,pjp,ix,dxm,dx,dxp,dxpp)
	  else
c
c       interpolate all four k-1,k,k+1,k+2
c
	    call lint2(nx,ny,p(1,1,k-1),mx,my,pkm,
     +                 intpol,jy,dy,pj,pjp,ix,dxm,dx,dxp,dxpp)
	    call lint2(nx,ny,p(1,1,k),mx,my,pk,
     +                 intpol,jy,dy,pj,pjp,ix,dxm,dx,dxp,dxpp)
	    call lint2(nx,ny,p(1,1,k+1),mx,my,pkp,
     +           intpol,jy,dy,pj,pjp,ix,dxm,dx,dxp,dxpp)
	    call lint2(nx,ny,p(1,1,k+2),mx,my,pkpp,
     +                 intpol,jy,dy,pj,pjp,ix,dxm,dx,dxp,dxpp)
	  end if
c
c       save k pointer for next pass
c
	  ksave = k
c
c       cubically interpolate q(ii,jj,kk) from pkm,pk,pkp,pkpp in z direction
c
	  do iijj=1,mxmy
	    q(iijj,kk) = dzm(kk)*pkm(iijj) + dz(kk)*pk(iijj) +
     +                   dzp(kk)*pkp(iijj) + dzpp(kk)*pkpp(iijj)
	  end do
	end do
	return

      else
c
c       cubic in y
c
	ksave = -3
	do kk=1,mz
	  k = kz(kk)
	  if (k.eq.ksave) then
c
c       k pointer has not moved since last pass (no updates or interpolation)
c
	  else if (k.eq.ksave+1) then
c
c       update k-1,k,k+1 and interpolate k+2
c
	  do iijj=1,mxmy
	    pkm(iijj) = pk(iijj)
	    pk(iijj) = pkp(iijj)
	    pkp(iijj) = pkpp(iijj)
	  end do
	  call cubt2(nx,ny,p(1,1,k+2),mx,my,pkpp,intpol,jy,dym,dy,
     +               dyp,dypp,pjm,pj,pjp,pjpp,ix,dxm,dx,dxp,dxpp)
	  else if (k.eq.ksave+2) then
c
c       update k-1,k and interpolate k+1,k+2
c
	    do iijj=1,mxmy
	      pkm(iijj) = pkp(iijj)
	      pk(iijj) = pkpp(iijj)
	    end do
	    call cubt2(nx,ny,p(1,1,k+1),mx,my,pkp,intpol,jy,dym,dy,
     +                 dyp,dypp,pjm,pj,pjp,pjpp,ix,dxm,dx,dxp,dxpp)
	    call cubt2(nx,ny,p(1,1,k+2),mx,my,pkpp,intpol,jy,dym,dy,
     +                 dyp,dypp,pjm,pj,pjp,pjpp,ix,dxm,dx,dxp,dxpp)
	  else if (k.eq.ksave+3) then
c
c       update k-1 and interpolate k,k+1,k+2
c
	    do iijj=1,mxmy
	      pkm(iijj) = pkpp(iijj)
	    end do
	    call cubt2(nx,ny,p(1,1,k),mx,my,pk,intpol,
     +      jy,dym,dy,dyp,dypp,pjm,pj,pjp,pjpp,ix,dxm,dx,dxp,dxpp)
	    call cubt2(nx,ny,p(1,1,k+1),mx,my,pkp,intpol,
     +      jy,dym,dy,dyp,dypp,pjm,pj,pjp,pjpp,ix,dxm,dx,dxp,dxpp)
	    call cubt2(nx,ny,p(1,1,k+2),mx,my,pkpp,intpol,
     +      jy,dym,dy,dyp,dypp,pjm,pj,pjp,pjpp,ix,dxm,dx,dxp,dxpp)
	  else
c
c     interpolate all four k-1,k,k+1,k+2
c
	    call cubt2(nx,ny,p(1,1,k-1),mx,my,pkm,intpol,
     +      jy,dym,dy,dyp,dypp,pjm,pj,pjp,pjpp,ix,dxm,dx,dxp,dxpp)
	    call cubt2(nx,ny,p(1,1,k),mx,my,pk,intpol,
     +      jy,dym,dy,dyp,dypp,pjm,pj,pjp,pjpp,ix,dxm,dx,dxp,dxpp)
	    call cubt2(nx,ny,p(1,1,k+1),mx,my,pkp,intpol,
     +      jy,dym,dy,dyp,dypp,pjm,pj,pjp,pjpp,ix,dxm,dx,dxp,dxpp)
	    call cubt2(nx,ny,p(1,1,k+2),mx,my,pkpp,intpol,
     +      jy,dym,dy,dyp,dypp,pjm,pj,pjp,pjpp,ix,dxm,dx,dxp,dxpp)

	  end if
c
c       save k pointer for next pass
c
	  ksave = k
c
c       cubically interpolate q(ii,jj,kk) from pkm,pk,pkp,pkpp in z direction
c
	  do iijj=1,mxmy
	    q(iijj,kk) = dzm(kk)*pkm(iijj) + dz(kk)*pk(iijj) +
     +                   dzp(kk)*pkp(iijj) + dzpp(kk)*pkpp(iijj)
	  end do
	end do
	return
      end if

      end
