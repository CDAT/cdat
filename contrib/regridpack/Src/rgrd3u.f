c
c
c ... file rgrd3u.f
c
c     this file contains documentation for subroutine rgrd3u followed by
c     fortran code for rgrd3u and additional subroutines.
c
c ... author
c
c     John C. Adams (NCAR 1999)
c
c ... subroutine rgrd3u(nx,ny,nz,p,mx,my,mz,q,intpol,w,lw,iw,liw,ier)
c
c ... purpose
c
c     subroutine rgrd3u interpolates the nx by ny by nz array p onto
c     the mx by my by mz array q.  it is assumed that p and q are
c     values on uniform nx by ny by nz and mx by my by mz grids which
c     are superimposed on the same box region (INCLUDING BOUNDARIES).
c     if p and q are values on nonuniform orthogonal grids and/or
c     if the grid on which q is defined lies within the p grid then
c     subroutine rgrd3 (see file rgrd3.f) should be used.
c
c ... language
c
c     coded in portable FORTRAN77 and FORTRAN90
c
c ... test program
c
c     file trgrd3u.f on regridpack includes a test program for subroutine rgrd3u
c
c ... method
c
c     linear or cubic interpolation (see intpol) is used in each
c     direction for which the q grid is not a subgrid of the p grid.
c     [the mx (my,mz) uniform grid is a subgrid of the nx (ny,nz) uniform
c     grid if and only if mx-1 (my-1,nz-1) divides nx-1 (ny-1,nz-1)].
c     Values are set directly without (the need for) interpolation
c     in subgrid directions.
c
c
c ... required files
c
c     files rgrd2u.f and rgrd1u.f must be loaded with rgrd3u.f.  they
c     include subroutines called by the routines in rgrd3u.f
c
c ... efficiency
c
c     inner most loops in regridpack software vectorize.  If the
c     arguments mx,my,mz (see below) have different values, optimal
c     vectorization will be achieved if mx > my > mz.
c
c
c *** input arguments ***
c
c
c ... nx
c
c     the integer first dimension of p.  nx > 1 if intpol(1) = 1 or
c     nx > 3 if intpol(1) = 3 is required (see ier = 2).
c
c ... ny
c
c     the integer second dimension of p.  ny > 1 if intpol(2) = 1 or
c     ny > 3 if intpol(2) = 3 is required (see ier = 2).
c
c
c ... nz
c
c     the integer third dimension of p.  nz > 1 if intpol(3) = 1 or
c     nz > 3 if intpol(3) = 3 is required (see ier = 2)
c
c ... p
c
c     a real nx by ny by nz array of given values
c
c ... mx
c
c     the integer first dimension of q.  mx > 1 is required (see ier = 1)
c
c ... my
c
c     the integer second dimension of q. my > 1 is required (see ier = 1)
c
c ... mz
c
c     the integer third dimension of q. mz > 1 is required (see ier = 1)
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
c     values other than 1 or 3 in intpol are not allowed (ier = 3).
c
c ... w
c
c     a real work space of length at least lw which must be provided in the
c     routine calling rgrd3u
c
c ... lw
c
c     the integer length of the real work space w.
c
c          let lwx = 1 if mx-1 divides nx-1; otherwise
c          let lwx =   mx if intpol(1) = 1 or
c          let lwx = 4*mx if intpol(1) = 3
c
c          let lwy = 0 if my-1 divides ny-1; otherwise
c          let lwy = my+2*mx if intpol(2) = 1 or
c          let lwy = 4*(mx+my) if intpol(2) = 3
c
c          let lwz = 0 if mz-1 divides nz-1; otherwise
c          let lwz = 2*mx*my+mz if intpol(3) = 1 or
c          let lwz = 4*(mx*my+mz) if intpol(3) = 3
c
c     then lw must be greater than or equal to lwx+lwy+lwz
c
c ... iw
c
c     an integer work space of length at least liw which must be provided in the
c     routine calling rgrd3u
c
c ... liw
c
c     the integer length of the integer work space iw.  liw must be greater than
c     or equal to mx+my+mz
c
c
c *** output arguments ***
c
c
c ... q
c
c     a real mx by my by mz array of values which are interpolated from p.
c
c ... ier
c
c     an integer error flag set as follows:
c
c     ier = 0 if no errors in input arguments are detected
c
c     ier = 1 if  min0(mx,my,mz) < 2
c
c     ier = 2 if nx < 2 when intpol(1)=1 or nx < 4 when intpol(1)=3 (or)
c                ny < 2 when intpol(2)=1 or ny < 4 when intpol(2)=3 (or)
c                nz < 2 when intpol(3)=1 or nz < 4 when intpol(3)=3.
c
c     ier = 3 if any of intpol(1),intpol(2),intpol(3) is not equal to 1 or 3
c
c     ier = 4 if lw or liw is too small (insufficient work space)
c
c
c ************************************************************************
c
c     end of rgrd3u documentation, fortran code follows:
c
c ************************************************************************
c
c
      subroutine rgrd3u(nx,ny,nz,p,mx,my,mz,q,intpol,w,lw,iw,liw,ier)
      implicit none
      integer nx,ny,nz,mx,my,mz,intpol(3),lw,liw,iw(liw),ier
      real p(nx,ny,nz),q(mx,my,mz),w(lw)
      integer inmx,jnmy,knmz,isubx,jsuby,ksubz
      integer lwx,lwy,lwz,mxmy,jy,kz
      integer i2,i3,i4,i5
      integer j2,j3,j4,j5,j6,j7,j8,j9
      integer k2,k3,k4,k5,k6,k7,k8,k9
c
c     check input arguments
c
      ier = 1
c
c     check mx,my,mz
c
      if (min0(mx,my,mz) .lt. 1) return
c
c     check intpol
c
      ier = 3
      if (intpol(1).ne.1 .and. intpol(1).ne.3) return
      if (intpol(2).ne.1 .and. intpol(2).ne.3) return
      if (intpol(3).ne.1 .and. intpol(3).ne.3) return
c
c     check nx,ny,nz
c
      ier = 2
      if (intpol(1).eq.1 .and. nx.lt.2) return
      if (intpol(1).eq.3 .and. nx.lt.4) return
      if (intpol(2).eq.1 .and. ny.lt.2) return
      if (intpol(2).eq.3 .and. ny.lt.4) return
      if (intpol(3).eq.1 .and. nz.lt.2) return
      if (intpol(3).eq.3 .and. nz.lt.4) return
c
c     set subgrid indicators
c
      inmx = (nx-1)/(mx-1)
      jnmy = (ny-1)/(my-1)
      knmz = (nz-1)/(mz-1)
      isubx = nx - inmx*(mx-1)
      jsuby = ny - jnmy*(my-1)
      ksubz = nz - knmz*(mz-1)
c
c     check work space lengths
c
      ier = 4
      mxmy = mx*my
      lwx = 1
      if (isubx.ne.1) then
	if (intpol(1).eq.1) then
	  lwx = mx
	else
	  lwx = 4*mx
	end if
      end if
      lwy = 0
      if (jsuby.ne.1) then
	if (intpol(2).eq.1) then
	  lwy = (2*mx+my)
	else
	  lwy = 4*(mx+my)
	end if
      end if
      lwz = 0
      if (ksubz.ne.1) then
	if (intpol(3).eq.1) then
	  lwz = (2*mxmy+mz)
	else
	  lwz = 4*(mxmy+mz)
	end if
      end if
      if (lw .lt. lwx+lwy+lwz) return
      if (liw .lt. mx+my+mz) return
c
c     arguments o.k.
c
      ier = 0
      jy = mx+1
      kz = mx+my+1
c
c     preset work space pointers
c
      k2 = 1
      k3 = 1
      k4 = 1
      k5 = 1
      k6 = 1
      k7 = 1
      k8 = 1
      k9 = 1
      j2 = 1
      j3 = 1
      j4 = 1
      j5 = 1
      j6 = 1
      j7 = 1
      j8 = 1
      j9 = 1
      i2 = 1
      i3 = 1
      i4 = 1
      i5 = 1

      if (intpol(3).eq.1) then
	if (ksubz.ne.1) then
c
c     linearly interpolate in nz, set work space pointers
c
	  k2 = 1
	  k3 = k2
	  k4 = k3+mz
	  k5 = k4
	  k6 = k5
	  k7 = k6
	  k8 = k7+mxmy
	  k9 = k8+mxmy
c
c     set z interpolation indices and scales
c
	  call linmxu(nz,mz,iw(kz),w(k3))
	  j2 = k9
	  i2 = k9
	end if

	if (jsuby.ne.1) then
	  if (intpol(2) .eq. 1) then
c     linear in y
	    j3 = j2
	    j4 = j3+my
	    j5 = j4
	    j6 = j5
	    j7 = j6
	    j8 = j7+mx
	    j9 = j8+mx
	    call linmxu(ny,my,iw(jy),w(j3))
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
	    call cubnmxu(ny,my,iw(jy),w(j2),w(j3),w(j4),w(j5))
	    i2 = j9+mx
	  end if
	end if

	if (isubx.ne.1) then
	  if (intpol(1) .eq. 1) then
c     linear in x
	    i3 = i2
	    i4 = i3
	    i5 = i4
	    call linmxu(nx,mx,iw,w(i3))
	  else
c     cubic in x
	    i3 = i2+mx
	    i4 = i3+mx
	    i5 = i4+mx
	    call cubnmxu(nx,mx,iw,w(i2),w(i3),w(i4),w(i5))
	  end if
	end if
c
c     linearly interpolate p onto q in z
c
	call lint3u(nx,ny,nz,p,mx,my,mxmy,mz,q,intpol,iw(kz),w(k3),
     +  w(k7),w(k8),iw(jy),w(j2),w(j3),w(j4),w(j5),w(j6),w(j7),
     +  w(j8),w(j9),iw,w(i2),w(i3),w(i4),w(i5),
     +  inmx,jnmy,knmz,isubx,jsuby,ksubz)
	return

      else
c
c     cubically interpolate in z
c
	if (ksubz.ne.1) then
	  k2 = 1
	  k3 = k2+mz
	  k4 = k3+mz
	  k5 = k4+mz
	  k6 = k5+mz
	  k7 = k6+mxmy
	  k8 = k7+mxmy
	  k9 = k8+mxmy
	  call cubnmxu(nz,mz,iw(kz),w(k2),w(k3),w(k4),w(k5))
	  j2 = k9+mxmy
	  i2 = j2
	end if

	if (jsuby.ne.1) then
	  if (intpol(2) .eq. 1) then
	    j3 = j2
	    j4 = j3+my
	    j5 = j4
	    j6 = j5
	    j7 = j6
	    j8 = j7+mx
	    j9 = j8+mx
	    call linmxu(ny,my,iw(jy),w(j3))
	    i2 = j9
	  else
	    j3 = j2+my
	    j4 = j3+my
	    j5 = j4+my
	    j6 = j5+my
	    j7 = j6+mx
	    j8 = j7+mx
	    j9 = j8+mx
	    call cubnmxu(ny,my,iw(jy),w(j2),w(j3),w(j4),w(j5))
	    i2 = j9+mx
	  end if
	end if

	if (isubx.ne.1) then
	  if (intpol(1) .eq. 1) then
	    i3 = i2
	    i4 = i3
	    i5 = i4
	    call linmxu(nx,mx,iw,w(i3))
	  else
	    i3 = i2+mx
	    i4 = i3+mx
	    i5 = i4+mx
	    call cubnmxu(nx,mx,iw,w(i2),w(i3),w(i4),w(i5))
	  end if
	end if
c
c     cubically interpolate p onto q in z
c
	call cubt3u(nx,ny,nz,p,mx,my,mxmy,mz,q,intpol,
     +  iw(kz),w(k2),w(k3),w(k4),w(k5),w(k6),w(k7),w(k8),w(k9),
     +  iw(jy),w(j2),w(j3),w(j4),w(j5),w(j6),w(j7),w(j8),w(j9),
     +  iw,w(i2),w(i3),w(i4),w(i5),
     +  inmx,jnmy,knmz,isubx,jsuby,ksubz)

	return
      end if
      end

      subroutine lint3u(nx,ny,nz,p,mx,my,mxmy,mz,q,intpol,kz,
     +dz,pk,pkp,jy,dym,dy,dyp,dypp,pjm,pj,pjp,pjpp,ix,dxm,dx,dxp,dxpp,
     +inmx,jnmy,knmz,isubx,jsuby,ksubz)
      implicit none
c
c     linearly interpolate in z direction
c
      integer nx,ny,nz,mx,my,mz,mxmy,intpol(3),kz(mz),jy(my),ix(mx)
      integer inmx,jnmy,knmz,isubx,jsuby,ksubz
      integer kk,k,iijj,ksave
      real p(nx,ny,nz),q(mxmy,mz)
      real dz(mz),pk(mxmy),pkp(mxmy)
      real dym(my),dy(my),dyp(my),dypp(my)
      real pjm(mx),pj(mx),pjp(mx),pjpp(mx)
      real dxm(mx),dx(mx),dxp(mx),dxpp(mx)

      if (intpol(2) .eq. 1) then
c
c     linear in y
c
	if (ksubz .eq. 1) then
c
c     mz grid is subset of nz grid
c
	  do kk=1,mz
	    k = knmz*(kk-1)+1
	    call lint2u(nx,ny,p(1,1,k),mx,my,q(1,kk),intpol,jy,dy,
     +      pj,pjp,ix,dxm,dx,dxp,dxpp,inmx,jnmy,isubx,jsuby)
	  end do
	  return
	end if

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
	    call lint2u(nx,ny,p(1,1,k+1),mx,my,pkp,intpol,jy,dy,
     +      pj,pjp,ix,dxm,dx,dxp,dxpp,inmx,jnmy,isubx,jsuby)
	  else
c
c     interpolate k,k+1 in pk,pkp
c
	    call lint2u(nx,ny,p(1,1,k),mx,my,pk,intpol,jy,dy,
     +      pj,pjp,ix,dxm,dx,dxp,dxpp,inmx,jnmy,isubx,jsuby)
	    call lint2u(nx,ny,p(1,1,k+1),mx,my,pkp,intpol,jy,dy,
     +      pj,pjp,ix,dxm,dx,dxp,dxpp,inmx,jnmy,isubx,jsuby)
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
	if (ksubz .eq. 1) then
c
c     mz grid is subset of nz grid
c
	  do kk=1,mz
	    k = knmz*(kk-1)+1
	    call cubt2u(nx,ny,p(1,1,k),mx,my,q(1,kk),intpol,
     +      jy,dym,dy,dyp,dypp,pjm,pj,pjp,pjpp,ix,dxm,dx,dxp,dxpp,
     +      inmx,jnmy,isubx,jsuby)
	  end do
	  return
	end if

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
	    call cubt2u(nx,ny,p(1,1,k+1),mx,my,pkp,intpol,
     +      jy,dym,dy,dyp,dypp,pjm,pj,pjp,pjpp,ix,dxm,dx,dxp,dxpp,
     +      inmx,jnmy,isubx,jsuby)
	  else
c
c     interpolate k,k+1 in pk,pkp
c
	    call cubt2u(nx,ny,p(1,1,k),mx,my,pk,intpol,
     +      jy,dym,dy,dyp,dypp,pjm,pj,pjp,pjpp,ix,dxm,dx,dxp,dxpp,
     +      inmx,jnmy,isubx,jsuby)
	    call cubt2u(nx,ny,p(1,1,k+1),mx,my,pkp,intpol,
     +      jy,dym,dy,dyp,dypp,pjm,pj,pjp,pjpp,ix,dxm,dx,dxp,dxpp,
     +      inmx,jnmy,isubx,jsuby)
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

      subroutine cubt3u(nx,ny,nz,p,mx,my,mxmy,mz,q,intpol,
     +kz,dzm,dz,dzp,dzpp,pkm,pk,pkp,pkpp,jy,dym,dy,dyp,dypp,pjm,pj,
     +pjp,pjpp,ix,dxm,dx,dxp,dxpp,
     +inmx,jnmy,knmz,isubx,jsuby,ksubz)
      implicit none
c
c     cubically interpolate in z
c
      integer nx,ny,nz,mx,my,mz,mxmy,intpol(3),kz(mz),jy(my),ix(mx)
      integer inmx,jnmy,knmz,isubx,jsuby,ksubz
      integer kk,k,iijj,ksave
      real p(nx,ny,nz),q(mxmy,mz)
      real dzm(mz),dz(mz),dzp(mz),dzpp(mz)
      real pkm(mxmy),pk(mxmy),pkp(mxmy),pkpp(mxmy)
      real dym(my),dy(my),dyp(my),dypp(my)
      real pjm(mx),pj(mx),pjp(mx),pjpp(mx)
      real dxm(mx),dx(mx),dxp(mx),dxpp(mx)

      if (intpol(2) .eq. 1) then
c
c     linear in y
c
	if (ksubz .eq. 1) then
c
c     mz grid is subset of nz grid
c
	  do kk=1,mz
	    k = knmz*(kk-1)+1
	    call lint2u(nx,ny,p(1,1,k),mx,my,q(1,kk),intpol,jy,dy,
     +      pj,pjp,ix,dxm,dx,dxp,dxpp,inmx,jnmy,isubx,jsuby)
	  end do
	  return
	end if
c
c     mz not a subgrid of nz
c
	ksave = -3
	do kk=1,mz
	  k = kz(kk)
	  if (k.eq.ksave) then
c
c     k pointer has not moved since last pass (no updates or interpolation)
c
	  else if (k.eq.ksave+1) then
c
c     update k-1,k,k+1 and interpolate k+2
c
	    do iijj=1,mxmy
	      pkm(iijj) = pk(iijj)
	      pk(iijj) = pkp(iijj)
	      pkp(iijj) = pkpp(iijj)
	    end do
	    call lint2u(nx,ny,p(1,1,k+2),mx,my,pkpp,intpol,jy,
     +      dy,pj,pjp,ix,dxm,dx,dxp,dxpp,inmx,jnmy,isubx,jsuby)
	  else if (k.eq.ksave+2) then
c
c     update k-1,k and interpolate k+1,k+2
c
	    do iijj=1,mxmy
	      pkm(iijj) = pkp(iijj)
	      pk(iijj) = pkpp(iijj)
	    end do
	    call lint2u(nx,ny,p(1,1,k+1),mx,my,pkp,intpol,jy,
     +      dy,pj,pjp,ix,dxm,dx,dxp,dxpp,inmx,jnmy,isubx,jsuby)
	    call lint2u(nx,ny,p(1,1,k+2),mx,my,pkpp,intpol,jy,
     +      dy,pj,pjp,ix,dxm,dx,dxp,dxpp,inmx,jnmy,isubx,jsuby)
	  else if (k.eq.ksave+3) then
c
c     update k-1 and interpolate k,k+1,k+2
c
	    do iijj=1,mxmy
	      pkm(iijj) = pkpp(iijj)
	    end do
	    call lint2u(nx,ny,p(1,1,k),mx,my,pk,intpol,jy,
     +      dy,pj,pjp,ix,dxm,dx,dxp,dxpp,inmx,jnmy,isubx,jsuby)
	    call lint2u(nx,ny,p(1,1,k+1),mx,my,pkp,intpol,jy,
     +      dy,pj,pjp,ix,dxm,dx,dxp,dxpp,inmx,jnmy,isubx,jsuby)
	    call lint2u(nx,ny,p(1,1,k+2),mx,my,pkpp,intpol,jy,
     +      dy,pj,pjp,ix,dxm,dx,dxp,dxpp,inmx,jnmy,isubx,jsuby)
	  else
c
c     interpolate all four k-1,k,k+1,k+2
c
	    call lint2u(nx,ny,p(1,1,k-1),mx,my,pkm,intpol,jy,
     +      dy,pj,pjp,ix,dxm,dx,dxp,dxpp,inmx,jnmy,isubx,jsuby)
	    call lint2u(nx,ny,p(1,1,k),mx,my,pk,intpol,jy,
     +      dy,pj,pjp,ix,dxm,dx,dxp,dxpp,inmx,jnmy,isubx,jsuby)
	    call lint2u(nx,ny,p(1,1,k+1),mx,my,pkp,intpol,jy,
     +      dy,pj,pjp,ix,dxm,dx,dxp,dxpp,inmx,jnmy,isubx,jsuby)
	    call lint2u(nx,ny,p(1,1,k+2),mx,my,pkpp,intpol,jy,
     +      dy,pj,pjp,ix,dxm,dx,dxp,dxpp,inmx,jnmy,isubx,jsuby)

	  end if
c
c     save k pointer for next pass
c
	  ksave = k
c
c     cubically interpolate q(ii,jj,kk) from pkm,pk,pkp,pkpp in z direction
c
	  do iijj=1,mxmy
	    q(iijj,kk) = dzm(kk)*pkm(iijj) + dz(kk)*pk(iijj) +
     +                   dzp(kk)*pkp(iijj) + dzpp(kk)*pkpp(iijj)
	  end do
	end do
	return

      else
c
c     cubic in y
c
	if (ksubz .eq. 1) then
c
c     mz grid is subset of nz grid
c
	  do kk=1,mz
	    k = knmz*(kk-1)+1
	    call cubt2u(nx,ny,p(1,1,k),mx,my,q(1,kk),intpol,jy,dym,dy,
     +      dyp,dypp,pjm,pj,pjp,pjpp,ix,dxm,dx,dxp,dxpp,
     +      inmx,jnmy,isubx,jsuby)

	  end do
	  return
	end if

	ksave = -3
	do kk=1,mz
	  k = kz(kk)
	  if (k.eq.ksave) then
c
c     k pointer has not moved since last pass (no updates or interpolation)
c
	  else if (k.eq.ksave+1) then
c
c     update k-1,k,k+1 and interpolate k+2
c
	    do iijj=1,mxmy
	      pkm(iijj) = pk(iijj)
	      pk(iijj) = pkp(iijj)
	      pkp(iijj) = pkpp(iijj)
	    end do
	    call cubt2u(nx,ny,p(1,1,k+2),mx,my,pkpp,intpol,
     +      jy,dym,dy,dyp,dypp,pjm,pj,pjp,pjpp,ix,dxm,dx,dxp,dxpp,
     +      inmx,jnmy,isubx,jsuby)

	  else if (k.eq.ksave+2) then
c
c     update k-1,k and interpolate k+1,k+2
c
	    do iijj=1,mxmy
	      pkm(iijj) = pkp(iijj)
	      pk(iijj) = pkpp(iijj)
	    end do
	    call cubt2u(nx,ny,p(1,1,k+1),mx,my,pkp,intpol,
     +      jy,dym,dy,dyp,dypp,pjm,pj,pjp,pjpp,ix,dxm,dx,dxp,dxpp,
     +      inmx,jnmy,isubx,jsuby)
	    call cubt2u(nx,ny,p(1,1,k+2),mx,my,pkpp,intpol,
     +      jy,dym,dy,dyp,dypp,pjm,pj,pjp,pjpp,ix,dxm,dx,dxp,dxpp,
     +      inmx,jnmy,isubx,jsuby)
	  else if (k.eq.ksave+3) then
c
c     update k-1 and interpolate k,k+1,k+2
c
	    do iijj=1,mxmy
	      pkm(iijj) = pkpp(iijj)
	    end do
	    call cubt2u(nx,ny,p(1,1,k),mx,my,pk,intpol,
     +      jy,dym,dy,dyp,dypp,pjm,pj,pjp,pjpp,ix,dxm,dx,dxp,dxpp,
     +      inmx,jnmy,isubx,jsuby)
	    call cubt2u(nx,ny,p(1,1,k+1),mx,my,pkp,intpol,
     +      jy,dym,dy,dyp,dypp,pjm,pj,pjp,pjpp,ix,dxm,dx,dxp,dxpp,
     +      inmx,jnmy,isubx,jsuby)
	    call cubt2u(nx,ny,p(1,1,k+2),mx,my,pkpp,intpol,
     +      jy,dym,dy,dyp,dypp,pjm,pj,pjp,pjpp,ix,dxm,dx,dxp,dxpp,
     +      inmx,jnmy,isubx,jsuby)
	  else
c
c     interpolate all four k-1,k,k+1,k+2
c
	    call cubt2u(nx,ny,p(1,1,k-1),mx,my,pkm,intpol,
     +      jy,dym,dy,dyp,dypp,pjm,pj,pjp,pjpp,ix,dxm,dx,dxp,dxpp,
     +      inmx,jnmy,isubx,jsuby)
	    call cubt2u(nx,ny,p(1,1,k),mx,my,pk,intpol,
     +      jy,dym,dy,dyp,dypp,pjm,pj,pjp,pjpp,ix,dxm,dx,dxp,dxpp,
     +      inmx,jnmy,isubx,jsuby)
	    call cubt2u(nx,ny,p(1,1,k+1),mx,my,pkp,intpol,
     +      jy,dym,dy,dyp,dypp,pjm,pj,pjp,pjpp,ix,dxm,dx,dxp,dxpp,
     +      inmx,jnmy,isubx,jsuby)
	    call cubt2u(nx,ny,p(1,1,k+2),mx,my,pkpp,intpol,
     +      jy,dym,dy,dyp,dypp,pjm,pj,pjp,pjpp,ix,dxm,dx,dxp,dxpp,
     +      inmx,jnmy,isubx,jsuby)
	  end if
c
c     save k pointer for next pass
c
	  ksave = k
c
c     cubically interpolate q(ii,jj,kk) from pkm,pk,pkp,pkpp in z direction
c
	  do iijj=1,mxmy
	    q(iijj,kk) = dzm(kk)*pkm(iijj) + dz(kk)*pk(iijj) +
     +                   dzp(kk)*pkp(iijj) + dzpp(kk)*pkpp(iijj)
	  end do
	end do
	return
      end if

      end
