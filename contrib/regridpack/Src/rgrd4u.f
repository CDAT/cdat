c
c
c ... file rgrd4u.f
c
c     this file contains documentation for subroutine rgrd4u followed by
c     fortran code for rgrid4u and additional subroutines.
c
c ... author
c
c     John C. Adams (NCAR 1999)
c
c ... subroutine rgrd4u(nx,ny,nz,nt,p,mx,my,mz,mt,q,intpol,w,lw,iw,liw,ier)
c
c ... purpose
c
c     subroutine rgrd4u interpolates the nx by ny by nz by nt array p onto
c     the mx by my by mz by mt array q.  it is assumed that p and q are
c     values on uniform nx by ny by nz by nt and mx by my by mz by mt grids
c     which are superimposed on the same box region (INCLUDING BOUNDARIES).
c     if p and q are values on nonuniform orthogonal grids and/or if the grid
c     on which q is defined lies within the p grid then subroutine rgrd4
c     (see file rgrd4.f) should be used.
c
c ... language
c
c     coded in portable FORTRAN77 and FORTRAN90
c
c ... test program
c
c     file trgrd4u.f on regridpack includes a test program for subroutine rgrd4u
c
c ... method
c
c     linear or cubic interpolation (see intpol) is used in each
c     direction for which the q grid is not a subgrid of the p grid.
c     [the mx (my,mz,mt) uniform grid is a subgrid of the nx (ny,nz,nt)
c     uniform grid if and only if mx-1 (my-1,nz-1,nt-1) divides nx-1
c     (ny-1,nz-1,nt-1)].  Values are set directly without (the need for)
c     interpolation in subgrid directions.
c
c ... required files
c
c     files rgrd3u.f,rgrd2u.f and rgrd1u.f must be loaded with rgrd4u.f.  they
c     include subroutines called by the routines in rgrd4u.f
c
c ... efficiency
c
c     inner most loops in regridpack software vectorize.  If the
c     integer arguments mx,my,mz,mt (see below) have different values,
c     optimal vectorization will be achieved if they are arranged so that
c     mx > my > mz > mt.
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
c ... nt
c
c     the integer fourth dimension of p.  nt > 1 if intpol(4) = 1 or
c     nt > 3 if intpol(4) = 3 is required (see ier=2)
c
c ... p
c
c     a real nx by ny by nz by nt array of given values
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

c ... mt
c
c     the integer fourth dimension of q. mt > 1 is required (see ier = 1)
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
c     values other than 1 or 3 in intpol are not allowed (ier = 3).
c
c ... w
c
c     a real work space of length at least lw which must be provided in the
c     routine calling rgrd4u
c
c ... lw
c
c     the integer length of the work space w.
c
c          let lwx = 1 if mx-1 divides nx-1; otherwise
c          let lwx = mx if intpol(1) = 1 or
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
c          let lwt = 0 if mt-1 divides nt-1; otherwise
c          let lwt = 2*mx*my*mz+mt if intpol(4) = 1 or
c          let lwt = 4*(mx*my*mz+mt) if intpol(4) = 3
c
c     then lw must be greater than or equal to lwx+lwy+lwz+lwt
c
c ... iw
c
c     an integer work space of length at least liw which must be provided in the
c     routine calling rgrd4u
c
c ... liw
c
c     the integer length of the integer work space iw.  liw must be at least
c     mx+my+mz+mt
c
c
c *** output arguments ***
c
c
c ... q
c
c     a real mx by my by mz by mt array of values which are interpolated
c     from p.
c
c ... ier
c
c     an integer error flag set as follows:
c
c     ier = 0 if no errors in input arguments are detected
c
c     ier = 1 if  min0(mx,my,mz,mt) < 2
c
c     ier = 2 if nx < 2 when intpol(1)=1 or nx < 4 when intpol(1)=3 (or)
c                ny < 2 when intpol(2)=1 or ny < 4 when intpol(2)=3 (or)
c                nz < 2 when intpol(3)=1 or nz < 4 when intpol(3)=3 (or)
c                nt < 2 when intpol(4)=1 or nt < 4 when intpol(4)=3.
c
c     ier = 3 if any of intpol(1),intpol(2),intpol(3),intpol(4)  is not
c             equal to 1 or 3.
c
c     ier = 4 if lw or liw is too small (insufficient work space)
c
c ************************************************************************
c
c     end of rgrd4u documentation, fortran code follows:
c
c ************************************************************************
c
      subroutine rgrd4u(nx,ny,nz,nt,p,mx,my,mz,mt,q,intpol,
     +                  w,lw,iw,liw,ier)
      implicit none
      integer nx,ny,nz,nt,mx,my,mz,mt,intpol(4),liw,iw(liw),lw,ier
      real p(nx,ny,nz,nt),q(mx,my,mz,mt),w(lw)
      integer inmx,jnmy,knmz,lnmt,isubx,jsuby,ksubz,lsubt
      integer mxmy,mxmymz,lwx,lwy,lwz,lwt,jy,kz,lt
      integer i2,i3,i4,i5
      integer j2,j3,j4,j5,j6,j7,j8,j9
      integer k2,k3,k4,k5,k6,k7,k8,k9
      integer l2,l3,l4,l5,l6,l7,l8,l9
c
c
c     check input arguments
c
      ier = 1
c
c     check mx,my,mz,mt
c
      if (min0(mx,my,mz,mt) .lt. 1) return
c
c     check intpol
c
      ier = 3
      if (intpol(1).ne.1 .and. intpol(1).ne.3) return
      if (intpol(2).ne.1 .and. intpol(2).ne.3) return
      if (intpol(3).ne.1 .and. intpol(3).ne.3) return
      if (intpol(4).ne.1 .and. intpol(4).ne.3) return
c
c     check nx,ny,nz,nt
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
c     set subgrid indicators
c
      inmx = (nx-1)/(mx-1)
      jnmy = (ny-1)/(my-1)
      knmz = (nz-1)/(mz-1)
      lnmt = (nt-1)/(mt-1)
      isubx = nx - inmx*(mx-1)
      jsuby = ny - jnmy*(my-1)
      ksubz = nz - knmz*(mz-1)
      lsubt = nt - lnmt*(mt-1)
c
c     check work space length input
c
      ier = 4
      mxmy = mx*my
      mxmymz = mxmy*mz
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
	  lwy = 4*my+4*mx
	end if
      end if
      lwz = 0
      if (ksubz.ne.1) then
	if (intpol(3).eq.1) then
	  lwz = (2*mxmy+mz)
	else
	  lwz = 4*mxmy+4*mz
	end if
      end if
      lwt = 0
      if (lsubt.ne.1) then
	if (intpol(4).eq.1) then
	  lwt = (2*mxmymz+mt)
	else
	  lwt = 4*mxmymz+4*mt
	end if
      end if

      if (lw .lt. lwx+lwy+lwz+lwt) return
      if (liw .lt. mx+my+mz+mt) return
c
c     arguments o.k.
c
      ier = 0
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
	call linmxu(nt,mt,iw(lt),w(l3))
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
	  call linmxu(nz,mz,iw(kz),w(k3))
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
	  call cubnmxu(nz,mz,iw(kz),w(k2),w(k3),w(k4),w(k5))
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
c
c     linearly interpolate in t
c
	call lint4u(nx,ny,nz,nt,p,mx,my,mz,mt,mxmy,mxmymz,q,intpol,
     +  iw(lt),w(l3),w(l7),w(l8),
     +  iw(kz),w(k2),w(k3),w(k4),w(k5),w(k6),w(k7),w(k8),w(k9),
     +  iw(jy),w(j2),w(j3),w(j4),w(j5),w(j6),w(j7),w(j8),w(j9),
     +  iw,w(i2),w(i3),w(i4),w(i5),
     +  inmx,jnmy,knmz,lnmt,isubx,jsuby,ksubz,lsubt)
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
	call cubnmxu(nt,mt,iw(lt),w(l2),w(l3),w(l4),w(l5))
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
	  call linmxu(nz,mz,iw(kz),w(k3))
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
	  call cubnmxu(nz,mz,iw(kz),w(k2),w(k3),w(k4),w(k5))
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
c
c     set work space portion and indices which depend on x interpolation
c
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
c
c     cubically interpolate in t
c
	call cubt4u(nx,ny,nz,nt,p,mx,my,mz,mt,mxmy,mxmymz,q,intpol,
     +  iw(lt),w(l2),w(l3),w(l4),w(l5),w(l6),w(l7),w(l8),w(l9),
     +  iw(kz),w(k2),w(k3),w(k4),w(k5),w(k6),w(k7),w(k8),w(k9),
     +  iw(jy),w(j2),w(j3),w(j4),w(j5),w(j6),w(j7),w(j8),w(j9),
     +  iw,w(i2),w(i3),w(i4),w(i5),
     +  inmx,jnmy,knmz,lnmt,isubx,jsuby,ksubz,lsubt)
	return
      end if
      end

      subroutine lint4u(nx,ny,nz,nt,p,mx,my,mz,mt,mxmy,mxmymz,q,intpol,
     +lt,dt,pt,ptp,kz,dzm,dz,dzp,dzpp,pkm,pk,pkp,pkpp,
     +jy,dym,dy,dyp,dypp,pjm,pj,pjp,pjpp,ix,dxm,dx,dxp,dxpp,
     +inmx,jnmy,knmz,lnmt,isubx,jsuby,ksubz,lsubt)
c
c     linearly interpolate in t direction
c
      implicit none
      integer nx,ny,nz,nt,mx,my,mz,mt
      integer mxmy,mxmymz
      real p(nx,ny,nz,nt),q(mxmymz,mt)
      integer inmx,jnmy,knmz,lnmt,isubx,jsuby,ksubz,lsubt
      real dt(mt),pt(mxmymz),ptp(mxmymz)
      real dzm(mz),dz(mz),dzp(mz),dzpp(mz)
      real pkm(mxmy),pk(mxmy),pkp(mxmy),pkpp(mxmy)
      real dym(my),dy(my),dyp(my),dypp(my)
      real pjm(mx),pj(mx),pjp(mx),pjpp(mx)
      real dxm(mx),dx(mx),dxp(mx),dxpp(mx)
      integer lt(mt),kz(mz),jy(my),ix(mx),intpol(4)
      integer l,ll,lsave,iijjkk
      if (intpol(3) .eq. 1) then
c
c     linear in z
c
      if (lsubt .eq. 1) then
c
c     mt grid is subset of nt grid
c
       do ll=1,mt
	l = lnmt*(ll-1)+1
	call lint3u(nx,ny,nz,p(1,1,1,l),mx,my,mxmy,mz,q(1,ll),intpol,kz,
     +  dz,pk,pkp,jy,dym,dy,dyp,dypp,pjm,pj,pjp,pjpp,ix,dxm,dx,dxp,dxpp,
     +  inmx,jnmy,knmz,isubx,jsuby,ksubz)
       end do
       return
       end if

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
	 call lint3u(nx,ny,nz,p(1,1,1,l+1),mx,my,mxmy,mz,ptp,intpol,kz,
     +   dz,pk,pkp,jy,dym,dy,dyp,dypp,pjm,pj,pjp,pjpp,ix,dxm,dx,dxp,dxpp
     +   ,inmx,jnmy,knmz,isubx,jsuby,ksubz)
	else
c
c     interpolate l,l+1 in pt,ptp on xx,yy,zz mesh
c
	 call lint3u(nx,ny,nz,p(1,1,1,l),mx,my,mxmy,mz,pt,intpol,kz,dz,
     +   pk,pkp,jy,dym,dy,dyp,dypp,pjm,pj,pjp,pjpp,ix,dxm,dx,dxp,dxpp,
     +   inmx,jnmy,knmz,isubx,jsuby,ksubz)
	 call lint3u(nx,ny,nz,p(1,1,1,l+1),mx,my,mxmy,mz,ptp,intpol,kz,
     +   dz,pk,pkp,jy,dym,dy,dyp,dypp,pjm,pj,pjp,pjpp,ix,dxm,dx,dxp,dxpp
     +   ,inmx,jnmy,knmz,isubx,jsuby,ksubz)
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
       if (lsubt .eq. 1) then
c
c     mt grid is subset of nt grid
c
	do ll=1,mt
	 l = lnmt*(ll-1)+1
	 call cubt3u(nx,ny,nz,p(1,1,1,l),mx,my,mxmy,mz,q(1,ll),intpol,
     +   kz,dzm,dz,dzp,dzpp,pkm,pk,pkp,pkpp,jy,dym,dy,dyp,dypp,
     +   pjm,pj,pjp,pjpp,ix,dxm,dx,dxp,dxpp,
     +   inmx,jnmy,knmz,isubx,jsuby,ksubz)
	end do
	return
       end if
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
       call cubt3u(nx,ny,nt,p(1,1,1,l+1),mx,my,mxmy,mz,ptp,intpol,
     + kz,dzm,dz,dzp,dzpp,pkm,pk,pkp,pkpp,
     + jy,dym,dy,dyp,dypp,pjm,pj,pjp,pjpp,ix,dxm,dx,dxp,dxpp,
     + inmx,jnmy,knmz,isubx,jsuby,ksubz)
       else
c
c     interpolate l,l+1 in pt,ptp on xx,yy,zz mesh
c
       call cubt3u(nx,ny,nt,p(1,1,1,l),mx,my,mxmy,mz,pt,intpol,
     + kz,dzm,dz,dzp,dzpp,pkm,pk,pkp,pkpp,
     + jy,dym,dy,dyp,dypp,pjm,pj,pjp,pjpp,ix,dxm,dx,dxp,dxpp,
     + inmx,jnmy,knmz,isubx,jsuby,ksubz)
       call cubt3u(nx,ny,nt,p(1,1,1,l+1),mx,my,mxmy,mz,ptp,intpol,
     + kz,dzm,dz,dzp,dzpp,pkm,pk,pkp,pkpp,
     + jy,dym,dy,dyp,dypp,pjm,pj,pjp,pjpp,ix,dxm,dx,dxp,dxpp,
     + inmx,jnmy,knmz,isubx,jsuby,ksubz)
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

      subroutine cubt4u(nx,ny,nz,nt,p,mx,my,mz,mt,mxmy,mxmymz,q,intpol,
     +lt,dtm,dt,dtp,dtpp,ptm,pt,ptp,ptpp,
     +kz,dzm,dz,dzp,dzpp,pkm,pk,pkp,pkpp,
     +jy,dym,dy,dyp,dypp,pjm,pj,pjp,pjpp,
     +ix,dxm,dx,dxp,dxpp,
     +inmx,jnmy,knmz,lnmt,isubx,jsuby,ksubz,lsubt)
c
c
c     cubically interpolate in t
c
      implicit none
      integer nx,ny,nz,nt,mx,my,mz,mt
      integer inmx,jnmy,knmz,lnmt,isubx,jsuby,ksubz,lsubt
      integer mxmy,mxmymz
      real p(nx,ny,nz,nt),q(mxmymz,mt)
      real ptm(mxmymz),pt(mxmymz),ptp(mxmymz),ptpp(mxmymz)
      real dtm(mt),dt(mt),dtp(mt),dtpp(mt)
      real dzm(mz),dz(mz),dzp(mz),dzpp(mz)
      real pkm(mxmy),pk(mxmy),pkp(mxmy),pkpp(mxmy)
      real dym(my),dy(my),dyp(my),dypp(my)
      real pjm(mx),pj(mx),pjp(mx),pjpp(mx)
      real dxm(mx),dx(mx),dxp(mx),dxpp(mx)
      integer lt(mt),kz(mz),jy(my),ix(mx),intpol(4)
      integer l,ll,iijjkk,lsave
      if (intpol(3) .eq. 1) then
c
c     linear in z
c
      if (lsubt .eq. 1) then
c
c     mt grid is subset of nt grid
c
       do ll=1,mt
	l = lnmt*(ll-1)+1
	call lint3u(nx,ny,nz,p(1,1,1,l),mx,my,mxmy,mz,q(1,ll),intpol,kz,
     +  dz,pk,pkp,jy,dym,dy,dyp,dypp,pjm,pj,pjp,pjpp,ix,dxm,dx,dxp,dxpp,
     +  inmx,jnmy,knmz,isubx,jsuby,ksubz)
       end do
       return
      end if
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
	call lint3u(nx,ny,nz,p(1,1,1,l+2),mx,my,mxmy,mz,ptpp,intpol,kz,
     +  dz,pk,pkp,jy,dym,dy,dyp,dypp,pjm,pj,pjp,pjpp,ix,dxm,dx,dxp,dxpp,
     +  inmx,jnmy,knmz,isubx,jsuby,ksubz)
       else if (l.eq.lsave+2) then
c
c     update l-1,l and interpolate l+1,l+2
c
	do iijjkk=1,mxmymz
	 ptm(iijjkk) = ptp(iijjkk)
	 pt(iijjkk) = ptpp(iijjkk)
	end do
	call lint3u(nx,ny,nz,p(1,1,1,l+1),mx,my,mxmy,mz,ptp,intpol,kz,
     +  dz,pk,pkp,jy,dym,dy,dyp,dypp,pjm,pj,pjp,pjpp,ix,dxm,dx,dxp,dxpp,
     +  inmx,jnmy,knmz,isubx,jsuby,ksubz)
	call lint3u(nx,ny,nz,p(1,1,1,l+2),mx,my,mxmy,mz,ptpp,intpol,kz,
     +  dz,pk,pkp,jy,dym,dy,dyp,dypp,pjm,pj,pjp,pjpp,ix,dxm,dx,dxp,dxpp,
     +  inmx,jnmy,knmz,isubx,jsuby,ksubz)
       else if (l.eq.lsave+3) then
c
c     update l-1 and interpolate l,l+1,l+2
c
	do iijjkk=1,mxmymz
	 ptm(iijjkk) = ptpp(iijjkk)
	end do
	call lint3u(nx,ny,nz,p(1,1,1,l),mx,my,mxmy,mz,pt,intpol,kz,dz,
     +  pk,pkp,jy,dym,dy,dyp,dypp,pjm,pj,pjp,pjpp,ix,dxm,dx,dxp,dxpp,
     +  inmx,jnmy,knmz,isubx,jsuby,ksubz)
	call lint3u(nx,ny,nz,p(1,1,1,l+1),mx,my,mxmy,mz,ptp,intpol,kz,
     +  dz,pk,pkp,jy,dym,dy,dyp,dypp,pjm,pj,pjp,pjpp,ix,dxm,dx,dxp,dxpp,
     +  inmx,jnmy,knmz,isubx,jsuby,ksubz)
	call lint3u(nx,ny,nz,p(1,1,1,l+2),mx,my,mxmy,mz,ptpp,intpol,kz,
     +  dz,pk,pkp,jy,dym,dy,dyp,dypp,pjm,pj,pjp,pjpp,ix,dxm,dx,dxp,dxpp,
     +  inmx,jnmy,knmz,isubx,jsuby,ksubz)
       else
c
c     interpolate all four l-1,l,l+1,l+2
c
	call lint3u(nx,ny,nz,p(1,1,1,l-1),mx,my,mxmy,mz,ptm,intpol,kz,
     +  dz,pk,pkp,jy,dym,dy,dyp,dypp,pjm,pj,pjp,pjpp,ix,dxm,dx,dxp,dxpp,
     +  inmx,jnmy,knmz,isubx,jsuby,ksubz)
	call lint3u(nx,ny,nz,p(1,1,1,l),mx,my,mxmy,mz,pt,intpol,kz,
     +  dz,pk,pkp,jy,dym,dy,dyp,dypp,pjm,pj,pjp,pjpp,ix,dxm,dx,dxp,dxpp,
     +  inmx,jnmy,knmz,isubx,jsuby,ksubz)
	call lint3u(nx,ny,nz,p(1,1,1,l+1),mx,my,mxmy,mz,ptp,intpol,kz,
     +  dz,pk,pkp,jy,dym,dy,dyp,dypp,pjm,pj,pjp,pjpp,ix,dxm,dx,dxp,dxpp,
     +  inmx,jnmy,knmz,isubx,jsuby,ksubz)
	call lint3u(nx,ny,nz,p(1,1,1,l+2),mx,my,mxmy,mz,ptpp,intpol,kz,
     +  dz,pk,pkp,jy,dym,dy,dyp,dypp,pjm,pj,pjp,pjpp,ix,dxm,dx,dxp,dxpp,
     +  inmx,jnmy,knmz,isubx,jsuby,ksubz)
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
     +                 dtp(ll)*ptp(iijjkk) + dtpp(ll)*ptpp(iijjkk)
       end do
      end do
      return

      else
c
c     cubic in z
c
      if (lsubt .eq. 1) then
c
c     mt grid is subset of nt grid
c
       do ll=1,mt
	l = lnmt*(ll-1)+1
	call cubt3u(nx,ny,nz,p(1,1,1,l),mx,my,mxmy,mz,q(1,ll),intpol,
     +  kz,dzm,dz,dzp,dzpp,pkm,pk,pkp,pkpp,jy,dym,dy,dyp,dypp,
     +  pjm,pj,pjp,pjpp,ix,dxm,dx,dxp,dxpp,
     +  inmx,jnmy,knmz,isubx,jsuby,ksubz)
       end do
       return
      end if
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
	call cubt3u(nx,ny,nz,p(1,1,1,l+2),mx,my,mxmy,mz,ptpp,intpol,kz,
     +dzm,dz,dzp,dzpp,pkm,pk,pkp,pkpp,jy,dym,dy,dyp,dypp,pjm,pj,pjp,pjpp
     +,ix,dxm,dx,dxp,dxpp,
     +inmx,jnmy,knmz,isubx,jsuby,ksubz)
       else if (l.eq.lsave+2) then
c
c     update l-1,l and interpolate l+1,l+2
c
	do iijjkk=1,mxmymz
	 ptm(iijjkk) = ptp(iijjkk)
	 pt(iijjkk) = ptpp(iijjkk)
	end do
	call cubt3u(nx,ny,nz,p(1,1,1,l+1),mx,my,mxmy,mz,ptp,intpol,kz,
     +dzm,dz,dzp,dzpp,pkm,pk,pkp,pkpp,jy,dym,dy,dyp,dypp,pjm,pj,pjp,pjpp
     +,ix,dxm,dx,dxp,dxpp,
     +inmx,jnmy,knmz,isubx,jsuby,ksubz)
	call cubt3u(nx,ny,nz,p(1,1,1,l+2),mx,my,mxmy,mz,ptpp,intpol,kz,
     +dzm,dz,dzp,dzpp,pkm,pk,pkp,pkpp,jy,dym,dy,dyp,dypp,pjm,pj,pjp,pjpp
     +,ix,dxm,dx,dxp,dxpp,
     +inmx,jnmy,knmz,isubx,jsuby,ksubz)
       else if (l.eq.lsave+3) then
c
c     update l-1 and interpolate l,l+1,l+2
c
	do iijjkk=1,mxmymz
	 ptm(iijjkk) = ptpp(iijjkk)
	end do
	call cubt3u(nx,ny,nz,p(1,1,1,l),mx,my,mxmy,mz,pt,intpol,kz,
     +dzm,dz,dzp,dzpp,pkm,pk,pkp,pkpp,jy,dym,dy,dyp,dypp,pjm,pj,pjp,pjpp
     +,ix,dxm,dx,dxp,dxpp,
     +inmx,jnmy,knmz,isubx,jsuby,ksubz)
	call cubt3u(nx,ny,nz,p(1,1,1,l+1),mx,my,mxmy,mz,ptp,intpol,kz,
     +dzm,dz,dzp,dzpp,pkm,pk,pkp,pkpp,jy,dym,dy,dyp,dypp,pjm,pj,pjp,pjpp
     +,ix,dxm,dx,dxp,dxpp,
     +inmx,jnmy,knmz,isubx,jsuby,ksubz)
	call cubt3u(nx,ny,nz,p(1,1,1,l+2),mx,my,mxmy,mz,ptpp,intpol,kz,
     +dzm,dz,dzp,dzpp,pkm,pk,pkp,pkpp,jy,dym,dy,dyp,dypp,pjm,pj,pjp,pjpp
     +,ix,dxm,dx,dxp,dxpp,
     +inmx,jnmy,knmz,isubx,jsuby,ksubz)
       else
c
c     interpolate all four l-1,l,l+1,l+2
c
	call cubt3u(nx,ny,nz,p(1,1,1,l-1),mx,my,mxmy,mz,ptm,intpol,kz,
     +dzm,dz,dzp,dzpp,pkm,pk,pkp,pkpp,jy,dym,dy,dyp,dypp,pjm,pj,pjp,pjpp
     +,ix,dxm,dx,dxp,dxpp,
     +inmx,jnmy,knmz,isubx,jsuby,ksubz)
	call cubt3u(nx,ny,nz,p(1,1,1,l),mx,my,mxmy,mz,pt,intpol,kz,
     +dzm,dz,dzp,dzpp,pkm,pk,pkp,pkpp,jy,dym,dy,dyp,dypp,pjm,pj,pjp,pjpp
     +,ix,dxm,dx,dxp,dxpp,
     +inmx,jnmy,knmz,isubx,jsuby,ksubz)
	call cubt3u(nx,ny,nz,p(1,1,1,l+1),mx,my,mxmy,mz,ptp,intpol,kz,
     +dzm,dz,dzp,dzpp,pkm,pk,pkp,pkpp,jy,dym,dy,dyp,dypp,pjm,pj,pjp,pjpp
     +,ix,dxm,dx,dxp,dxpp,
     +inmx,jnmy,knmz,isubx,jsuby,ksubz)
	call cubt3u(nx,ny,nz,p(1,1,1,l+2),mx,my,mxmy,mz,ptpp,intpol,kz,
     +dzm,dz,dzp,dzpp,pkm,pk,pkp,pkpp,jy,dym,dy,dyp,dypp,pjm,pj,pjp,pjpp
     +,ix,dxm,dx,dxp,dxpp,
     +inmx,jnmy,knmz,isubx,jsuby,ksubz)
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
     +                 dtp(ll)*ptp(iijjkk) + dtpp(ll)*ptpp(iijjkk)
       end do
      end do
      return
      end if

      end
