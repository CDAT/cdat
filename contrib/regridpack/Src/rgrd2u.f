c
c
c ... file rgrd2u.f
c
c     this file contains documentation followed by fortran code for
c     subroutine rgrd2u and additional subroutines.
c
c ... author
c
c     John C. Adams (NCAR 1999)
c
c ... subroutine rgrd2u(nx,ny,p,mx,my,q,intpol,w,lw,iw,liw,ier)
c
c ... purpose
c
c     subroutine rgrd2u interpolates the nx by ny array p onto
c     the mx by my array q.  linear or cubic interpolation is
c     used in each direction (see aargumentintpol).  it is assumed
c     that p and q are values on uniform nx by ny and mx by my grids
c     superimposed on the same rectangle (INCLUDING BOUNDARIES).
c     if p and q are values on nonuniform orthogonal grids and/or
c     if the grid on which q is defined lies within the p grid
c     then subroutine rgrd2 (see file rgrd2.f) should be used.
c
c ... language
c
c     coded in portable FORTRAN77 and FORTRAN90
c
c ... test program
c
c     file trgrd2u.f on regridpack includes a test program for subroutine rgrd2u
c
c
c ... method
c
c     linear or cubic interpolation (see intpol) is used in each
c     direction for which the q grid is not a subgrid of the p grid.
c     [the mx (my) uniform grid is a subgrid of the nx (ny) uniform
c     grid if and only if mx-1 (my-1) divides nx-1 (ny-1)].
c     values are set directly without (the need for) interpolation
c     in subgrid directions.
c
c ... required files
c
c     file rgrd1u.f must be loaded with rgrd2u.f.  it includes
c     subroutines called by the routines in rgrd2u.f
c
c ... efficiency
c
c     inner most loops in regridpack software vectorize.  If
c     the arguments mx,my (see below) have different values, optimal
c     vectorization will be achieved if mx > my.
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
c ... p
c
c     a real nx by ny array of given values
c
c ... mx
c
c     the integer first dimension of q.  mx > 1 is required (see ier = 1)
c
c ... my
c
c     the integer second dimension of q. my > 1 is required (see ier = 1)
c
c ... intpol
c
c     an integer vector of dimension 2 which sets linear or cubic
c     interpolation in each of the x,y directions as follows:
c
c        intpol(1) = 1 sets linear interpolation in the x direction
c        intpol(1) = 3 sets cubic interpolation in the x direction.
c
c        intpol(2) = 1 sets linear interpolation in the y direction
c        intpol(2) = 3 sets cubic interpolation in the y direction.
c
c     values other than 1 or 3 in intpol are not allowed (ier = 3).
c
c ... w
c
c     a real work space of length at least lw which must be provided in the
c     routine calling rgrd2u
c
c ... lw
c
c     the integer length of the work space w.
c
c          let lwx = 1 if mx-1 divides nx-1; otherwise
c          let lwx =   mx if intpol(1) = 1 or
c          let lwx = 4*mx if intpol(1) = 3
c
c          let lwy = 0 if my-1 divides ny-1; otherwise
c          let lwy = 2*mx+my if intpol(2) = 1 or
c          let lwy = 4*(mx+my)  if intpol(2) = 3
c
c     then lw must be greater than or equal to lwx+lwy
c
c ... iw
c
c     an integer work space of length at least liw which must be provided in the
c     routine calling rgrd2u
c
c ... liw
c
c     the integer length of the integer work space iw.  liw must be greater than
c     or equal to mx+my.
c
c *** output arguments ***
c
c
c ... q
c
c     a real mx by my array of values which are interpolated from p.
c
c ... ier
c
c     an integer error flag set as follows:
c
c     ier = 0 if no errors in input arguments are detected
c
c     ier = 1 if  min0(mx,my) < 2
c
c     ier = 2 if nx < 2 when intpol(1)=1 or nx < 4 when intpol(1)=3 (or)
c                ny < 2 when intpol(2)=1 or ny < 4 when intpol(2)=3.
c
c     ier = 3 if intpol(1) or intpol(2) is not equal to 1 or 3
c
c     ier = 4 if lw or liw is to small (insufficient work space)
c
c ************************************************************************
c
c     end of rgrd2u documentation, fortran code follows:
c
c ************************************************************************
c
      subroutine rgrd2u(nx,ny,p,mx,my,q,intpol,w,lw,iw,liw,ier)
c
c     two dimensional linear or cubic interpolation of p onto q
c     assuming p and q lie on nx by ny and and mx by my uniform  grids
c     which subdivide the same rectangle (including boundaries)
c
      implicit none
      integer nx,ny,mx,my,intpol(2),lw,liw,ier
      real p(nx,ny),q(mx,my),w(lw)
      integer inmx,jnmy,isubx,jsuby,lwx,lwy,jy,iw(liw)
      integer j2,j3,j4,j5,j6,j7,j8,j9,i2,i3,i4,i5
c
c     check input aarguments
c
      ier = 1
c
c     check mx,my
c
      if (min0(mx,my) .lt. 2) return
c
c     check intpol
c
      ier = 3
      if (intpol(1).ne.1 .and. intpol(1).ne.3) return
      if (intpol(2).ne.1 .and. intpol(2).ne.3) return
c
c     check nx,ny
c
      ier = 2
      if (intpol(1).eq.1 .and. nx.lt.2) return
      if (intpol(1).eq.3 .and. nx.lt.4) return
      if (intpol(2).eq.1 .and. ny.lt.2) return
      if (intpol(2).eq.3 .and. ny.lt.4) return
c
c     set subgrid indicators
c
      inmx = (nx-1)/(mx-1)
      jnmy = (ny-1)/(my-1)
      isubx = nx - inmx*(mx-1)
      jsuby = ny - jnmy*(my-1)
c
c     check work space length input
c
      ier = 4
      lwx = 1
      lwy = 0
      if (isubx.ne.1) then
      if (intpol(1).eq.1) then
      lwx = mx
      else
      lwx = mx
      end if
      end if
      if (jsuby.ne.1) then
      if (intpol(2).eq.1) then
      lwy = (my+2*mx)
      else
      lwy = 4*(mx+my)
      end if
      end if
      if (lw .lt. lwx+lwy) return
      if (liw .lt. mx+my) return
c
c     input arguments o.k.
c
      ier = 0
      jy = mx+1
c
c     preset work space pointers
c
      j2 = 1
      j3 = j2
      j4 = j3
      j5 = j4
      j6 = j5
      j7 = j6
      j8 = j7
      j9 = j8
      i2 = j9
      i3 = i2
      i4 = i3
      i5 = i4

      if (intpol(2) .eq.1) then
c
c     linearly interpolate in y
c
	if (jsuby.ne.1) then
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
	  call linmxu(ny,my,iw(jy),w(j3))
	  i2 = j9
	end if
c
c     set work space portion and indices which depend on x interpolation
c
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
	call lint2u(nx,ny,p,mx,my,q,intpol,iw(jy),w(j3),w(j7),
     +  w(j8),iw,w(i2),w(i3),w(i4),w(i5),inmx,jnmy,isubx,jsuby)
	return

      else
c
c     cubically interpolate in y, set indice pointers
c
	if (jsuby.ne.1) then
	  j2 = 1
	  j3 = j2+my
	  j4 = j3+my
	  j5 = j4+my
	  j6 = j5+my
	  j7 = j6+mx
	  j8 = j7+mx
	  j9 = j8+mx
c
c     set y interpolation indices and scales and cubically interpolate in y
c
	  call cubnmxu(ny,my,iw(jy),w(j2),w(j3),w(j4),w(j5))
	  i2 =  j9+mx
	end if
c
c     set work space portion and indices which depend on x interpolation
c
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
	call cubt2u(nx,ny,p,mx,my,q,intpol,iw(jy),w(j2),w(j3),w(j4),
     +  w(j5),w(j6),w(j7),w(j8),w(j9),iw,w(i2),w(i3),w(i4),w(i5),
     +  inmx,jnmy,isubx,jsuby)
	return
      end if
      end

      subroutine lint2u(nx,ny,p,mx,my,q,intpol,jy,dy,pj,pjp,ix,dxm,dx,
     +                  dxp,dxpp,inmx,jnmy,isubx,jsuby)
      implicit none
      integer nx,ny,mx,my,intpol(2),jy(my),ix(mx),inmx,jnmy,isubx,jsuby
      integer j,jj,ii,jsave
      real p(nx,ny),q(mx,my)
      real dy(my),pj(mx),pjp(mx)
      real dxm(mx),dx(mx),dxp(mx),dxpp(mx)
c
c     linearly interpolate p onto q in y
c
      if (intpol(1) .eq. 1) then
c
c     linear in x
c
	if (jsuby .eq. 1) then
c
c     my grid is subset of ny grid
c
	do jj=1,my
	  j = jnmy*(jj-1)+1
	  call lint1u(nx,p(1,j),mx,q(1,jj),ix,dx,inmx,isubx)
	end do
	return
	end if

	jsave = -1
	do jj=1,my
	j = jy(jj)
	if (j .eq. jsave) then
c
c     pointer has not moved, no interpolation in pj,pjp necessary
c
	else if (j .eq. jsave+1) then
	  do ii=1,mx
	    pj(ii) = pjp(ii)
	  end do
	  call lint1u(nx,p(1,j+1),mx,pjp,ix,dx,inmx,isubx)
	else
	  call lint1u(nx,p(1,j),mx,pj,ix,dx,inmx,isubx)
	  call lint1u(nx,p(1,j+1),mx,pjp,ix,dx,inmx,isubx)
	end if
c
c     update pointer
c
	jsave = j
	do ii=1,mx
	  q(ii,jj) = pj(ii)+dy(jj)*(pjp(ii)-pj(ii))
	  end do
	end do

	return

      else
c
c     cubic in x
c
	if (jsuby .eq. 1) then
c
c     my grid is subset of ny grid
c
	  do jj=1,my
	    j = jnmy*(jj-1)+1
	    call cubt1u(nx,p(1,j),mx,q(1,jj),ix,dxm,dx,dxp,dxpp,
     +           inmx,isubx)
	  end do
	  return
	end if

	jsave = -1
	do jj=1,my
	  j = jy(jj)
	  if (j .eq. jsave) then
c
c     no interpolation in pj,pjp necessary
c
	  else if (j .eq. jsave+1) then
	    do ii=1,mx
	      pj(ii) = pjp(ii)
	    end do
	    call cubt1u(nx,p(1,j+1),mx,pjp,ix,dxm,dx,dxp,dxpp,
     +                  inmx,isubx)
	  else
	    call cubt1u(nx,p(1,j),mx,pj,ix,dxm,dx,dxp,dxpp,
     +                  inmx,isubx)
	   call cubt1u(nx,p(1,j+1),mx,pjp,ix,dxm,dx,dxp,dxpp,
     +                 inmx,isubx)
	  end if
c
c     update  pointer
c
	  jsave = j
	  do ii=1,mx
	    q(ii,jj) = pj(ii)+dy(jj)*(pjp(ii)-pj(ii))
	  end do
	end do
	return

      end if

      end

      subroutine cubt2u(nx,ny,p,mx,my,q,intpol,jy,dym,dy,dyp,dypp,
     +pjm,pj,pjp,pjpp,ix,dxm,dx,dxp,dxpp,inmx,jnmy,isubx,jsuby)
      implicit none
      integer nx,ny,mx,my,intpol(2),jy(my),ix(mx),inmx,jnmy,isubx,jsuby
      integer j,jj,ii,jsave
      real p(nx,ny),q(mx,my)
      real dym(my),dy(my),dyp(my),dypp(my)
      real pjm(mx),pj(mx),pjp(mx),pjpp(mx)
      real dxm(mx),dx(mx),dxp(mx),dxpp(mx)
c
c     cubically interpolate p onto q in y
c
      if (intpol(1) .eq. 1) then
c
c     linear in x
c
	if (jsuby .eq. 1) then
c
c     my grid is subset of ny grid
c
	  do jj=1,my
	    j = jnmy*(jj-1)+1
	    call lint1u(nx,p(1,j),mx,q(1,jj),ix,dx,inmx,isubx)
	  end do
	  return
	end if

	jsave = -3
	do jj=1,my
	  j = jy(jj)
c
c     load pjm,pj,pjp,pjpp
c
	  if (j.eq.jsave) then
c     no updates or x interpolation necessary
	  else if (j.eq.jsave+1) then
	    do ii=1,mx
	      pjm(ii) = pj(ii)
	      pj(ii) = pjp(ii)
	      pjp(ii) = pjpp(ii)
	    end do
	    call lint1u(nx,p(1,j+2),mx,pjpp,ix,dx,inmx,isubx)
	  else if (j.eq.jsave+2) then
	    do ii=1,mx
	      pjm(ii) = pjp(ii)
	     pj(ii) = pjpp(ii)
	    end do
	    call lint1u(nx,p(1,j+1),mx,pjp,ix,dx,inmx,isubx)
	    call lint1u(nx,p(1,j+2),mx,pjpp,ix,dx,inmx,isubx)
	  else if (j.eq.jsave+3) then
	    do ii=1,mx
	      pjm(ii) = pjpp(ii)
	    end do
	    call lint1u(nx,p(1,j),mx,pj,ix,dx,inmx,isubx)
	    call lint1u(nx,p(1,j+1),mx,pjp,ix,dx,inmx,isubx)
	    call lint1u(nx,p(1,j+2),mx,pjpp,ix,dx,inmx,isubx)
	  else
c     load all four (no updates)
	    call lint1u(nx,p(1,j-1),mx,pjm,ix,dx,inmx,isubx)
	    call lint1u(nx,p(1,j),mx,pj,ix,dx,inmx,isubx)
	    call lint1u(nx,p(1,j+1),mx,pjp,ix,dx,inmx,isubx)
	    call lint1u(nx,p(1,j+2),mx,pjpp,ix,dx,inmx,isubx)
	  end if
c
c     update pointer
c
	  jsave = j
	  do ii=1,mx
	    q(ii,jj) = dym(jj)*pjm(ii) + dy(jj)*pj(ii) +
     +                 dyp(jj)*pjp(ii) + dypp(jj)*pjpp(ii)
	  end do
	end do
	return

      else
c
c     cubic in x
c

	if (jsuby .eq. 1) then
c
c     my grid is subset of ny grid
c
	  do jj=1,my
	    j = jnmy*(jj-1)+1
	    call cubt1u(nx,p(1,j),mx,q(1,jj),ix,dxm,dx,dxp,dxpp,
     +                  inmx,isubx)
	  end do
	  return
	end if

	jsave = -3
	do jj=1,my
	  j = jy(jj)
c
c     load pjm,pj,pjp,pjpp
c
	  if (j.eq.jsave) then
c     no updates or x interpolation necessary
	  else if (j.eq.jsave+1) then
	    do ii=1,mx
	      pjm(ii) = pj(ii)
	      pj(ii) = pjp(ii)
	      pjp(ii) = pjpp(ii)
	    end do
	    call cubt1u(nx,p(1,j+2),mx,pjpp,ix,dxm,dx,dxp,dxpp,
     +                  inmx,isubx)
	  else if (j.eq.jsave+2) then
	    do ii=1,mx
	      pjm(ii) = pjp(ii)
	      pj(ii) = pjpp(ii)
	    end do
	    call cubt1u(nx,p(1,j+1),mx,pjp,ix,dxm,dx,dxp,dxpp,
     +                  inmx,isubx)
	    call cubt1u(nx,p(1,j+2),mx,pjpp,ix,dxm,dx,dxp,dxpp,
     +                  inmx,isubx)
	  else if (j.eq.jsave+3) then
	    do ii=1,mx
	      pjm(ii) = pjpp(ii)
	    end do
	    call cubt1u(nx,p(1,j),mx,pj,ix,dxm,dx,dxp,dxpp,
     +                  inmx,isubx)
	    call cubt1u(nx,p(1,j+1),mx,pjp,ix,dxm,dx,dxp,dxpp,
     +                  inmx,isubx)
	    call cubt1u(nx,p(1,j+2),mx,pjpp,ix,dxm,dx,dxp,dxpp,
     +                  inmx,isubx)
	  else
c     load all four (no updates)
	    call cubt1u(nx,p(1,j-1),mx,pjm,ix,dxm,dx,dxp,dxpp,
     +                  inmx,isubx)
	    call cubt1u(nx,p(1,j),mx,pj,ix,dxm,dx,dxp,dxpp,
     +                  inmx,isubx)
	    call cubt1u(nx,p(1,j+1),mx,pjp,ix,dxm,dx,dxp,dxpp,
     +                  inmx,isubx)
	    call cubt1u(nx,p(1,j+2),mx,pjpp,ix,dxm,dx,dxp,dxpp,
     +                  inmx,isubx)
	  end if
c
c     update pointer
c
	  jsave = j
	  do ii=1,mx
	    q(ii,jj) = dym(jj)*pjm(ii) + dy(jj)*pj(ii) +
     +                 dyp(jj)*pjp(ii) + dypp(jj)*pjpp(ii)
	  end do
	end do
      return

      end if

      end

