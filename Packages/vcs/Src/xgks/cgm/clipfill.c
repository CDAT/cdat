/*		Clip fill area polygons.				*/
/*		Prior to sending the metafile.				*/

#define MAXPT 1000000

    int clip_fill(mf_cgmo **cgmo, /*	Metafile out structure pointer	*/
		int num,	/*	Pointer to metafile out structure*/
		int id,		/*	identify the function.		*/
		int np,		/*	Number of points.		*/
		Gpoint pt[]	/*	Pointer to points.		*/
		)
      {
       int i,j,k,ixm,ix,iym,iy;
       float xmx,ymx,xmn,ymn,x,y,xm,ym;
       float xx,yy,xx2,yy2;
       Gpoint p[MAXPT];

       if (np <= 0 || np > MAXPT -10)
	 {
	  fprintf (stderr,"Error - clipper, point count wrong (%d).\n",np);
	  return 0;
	 }
       cgm_init();

/*		Loop over the points.					*/

       xmn=(*cgmo)->ws->clip.xmin;
       xmx=(*cgmo)->ws->clip.xmax;
       ymn=(*cgmo)->ws->clip.ymin;
       ymx=(*cgmo)->ws->clip.ymax;

/*fprintf(stdout,"num = %d, id = %d, np = %d, limits = x(%g,%g) y(%g,%g)\n",
						num,id,np,xmn,xmx,ymn,ymx);*/
       x=y=1.e20;
       ix=ixm=iy=iym=0;

       for (i=j=0; i <= np; i++)
	 {
	  xm=x;
	  ym=y;
	  ixm=ix;
	  iym=iy;

	  x=pt[i].x;
	  y=pt[i].y;
	  if (i == np)
	    {
	     x=pt[0].x;
	     y=pt[0].y;
	    }

/*			Set ix and iy.  The center space is the
			only drawable space (i.e. ix=iy=0).

				|	|
				| iy=1	|
			------------------------
				|	|
			ix=-1	|ix=iy=0| ix=1
				|	|
			------------------------
				|	|
				| iy=-1	|
*/
	  if (x >= xmn)
	    {
	     if (x <= xmx) ix=0;
	     else ix=1;
	    }
	  else
	    {
	     ix=-1;
	    }
	  if (y >= ymn)
	    {
	     if (y <= ymx) iy=0;
	     else iy=1;
	    }
	  else
	    {
	     iy=-1;
	    }
/*			If the point is inside.			*/

	  if (iy == 0 && ix == 0)
	    {
	     if (i > 0)
	       {
		if (ixm == 0)
		  {
/*				This is the most frequent case.		*/
/*				The previous point was saved, now
				save this point.			*/
		   if (iym == 0)
		     {
		     }
		   else if (iym == -1)
		     {
		      p[j].x=x-((y-ymn)/(y-ym))*(x-xm);
		      p[j].y=ymn;
		      j++;
		     }
		   else if (iym == 1)
		     {
		      p[j].x=x-((y-ymx)/(y-ym))*(x-xm);
		      p[j].y=ymx;
		      j++;
		     }
		  }	/*  End ixm == 0			*/
		else if (ixm == -1)
		  {
		   if (iym == 0)
		     {
		      p[j].x=xmn;
		      p[j].y=y-((x-xmn)/(x-xm))*(y-ym);
		      j++;
		     }
		   else if (iym == -1)
		     {
		      if ( (xx=x-((y-ymn)/(y-ym))*(x-xm)) >= xmn)
			{
			 p[j].x=xx;
			 p[j].y=ymn;
		         j++;
			}
		      else
			{
			 p[j].x=xmn;
			 p[j].y=y-((x-xmn)/(x-xm))*(y-ym);
		         j++;
			}
		     }
		   else if (iym == 1)
		     {
		      if ( (xx=x-((y-ymx)/(y-ym))*(x-xm)) >= xmn)
			{
			 p[j].x=xx;
			 p[j].y=ymx;
		         j++;
			}
		      else
			{
			 p[j].x=xmn;
			 p[j].y=y-((x-xmn)/(x-xm))*(y-ym);
		         j++;
			}
		     }
		  }	/*  End ixm == -1			*/
		else if (ixm == 1)
		  {
		   if (iym == 0)
		     {
		      p[j].x=xmx;
		      p[j].y=y-((x-xmx)/(x-xm))*(y-ym);
		      j++;
		     }
		   else if (iym == -1)
		     {
		      if ( (xx=x-((y-ymn)/(y-ym))*(x-xm)) < xmx)
			{
			 p[j].x=xx;
			 p[j].y=ymn;
		         j++;
			}
		      else
			{
			 p[j].x=xmx;
			 p[j].y=y-((x-xmx)/(x-xm))*(y-ym);
		         j++;
			}
		     }
		   else if (iym == 1)
		     {
		      if ( (xx=x-((y-ymx)/(y-ym))*(x-xm)) < xmx)
			{
			 p[j].x=xx;
			 p[j].y=ymx;
		         j++;
			}
		      else
			{
			 p[j].x=xmx;
			 p[j].y=y-((x-xmx)/(x-xm))*(y-ym);
		         j++;
			}
		     }
		  }	/*  End ixm == 1			*/
	       }	/*  End i > 0				*/

	     p[j].x = x;
	     p[j].y = y;
	     j++;
	     continue;
	    }	/*  End ix == 0 && iy == 0			*/
 
/*		The point is outside the clip rectangle.	*/

/*			Save a point on the periphery and continue
			if this is first point, or it is in the same
			exterior section as before.		*/
/*			It should have been saved if it's inside.*/
	
	  if (i == 0 || (ix == ixm && iy == iym))
	    {
	     if (x<xmn) p[j].x=xmn; else if (x>xmx) p[j].x=xmx; else p[j].x=x;
	     if (y<ymn) p[j].y=ymn; else if (y>ymx) p[j].y=ymx; else p[j].y=y;
	     j++;
	     continue;
	    }

/*			Pre-compute xx, xx2, yy, yy2			*/
	  if (y != ym)
	    {
	     xx=x-((y-ymn)/(y-ym))*(x-xm);
	     xx2=x-((y-ymx)/(y-ym))*(x-xm);
	    }
	  else xx=xx2=1.e20;
	  if (x != xm)
	    {
	     yy=y-((x-xmn)/(x-xm))*(y-ym);
	     yy2=y-((x-xmx)/(x-xm))*(y-ym);
	    }
	  else yy=yy2=1.e20;

/*			Current point on left side.			*/

	  if (ix == -1)
	    {
/*			Current point at bottom left.			*/

	     if (iy == -1)
	       {
		if (ixm == 0)
	          {
		   if (iym == 0)
		     {
		      if (xx > xmn) {	p[j].x=xx;  p[j].y=ymn; j++;}
		      else {		p[j].x=xmn; p[j].y=yy; j++;}
		     }
		   else if (iym == 1)
		     {
		      if (xx2 > xmn)
			{
					p[j].x=xx2; p[j].y=ymx; j++;
		         if (xx > xmn) {p[j].x=xx;  p[j].y=ymn; j++;}
		         else {		p[j].x=xmn; p[j].y=yy;  j++;}
			}
		      else { p[j].x=xmn; p[j].y=ymx; j++; }
		     }
		  }	/*  End ixm == 0			*/
		else if (ixm == 1)
	          {
		   if (iym == 0)
		     {
		      if (xx < xmx)
			{
					p[j].x=xmx; p[j].y=yy2; j++;
			 if (yy < ymn) {p[j].x=xx;  p[j].y=ymn; j++;}
			 else {		p[j].x=xmn; p[j].y=yy;  j++;}
			}
		      else { p[j].x=xmx; p[j].y=ymn; j++;}
		     }
		   else if (iym == 1)
		     {
		      if (xx < xmx && yy < ymx)
		        {
			 if (xx2 > xmx) {   p[j].x=xmx; p[j].y=yy2; j++;}
			 else {		    p[j].x=xx2; p[j].y=ymx; j++;}
			 if (xx > xmn)     {p[j].x=xx;  p[j].y=ymn; j++;}
			 else if (yy > ymn){p[j].x=xmn; p[j].y=yy;  j++;}
			}
		      else if (x >= xmx) {p[j].x=xmx; p[j].y=ymn; j++;}
		      else {p[j].x=xmn; p[j].y=ymx; j++;}
		     }
		  }	/*  End ixm == 1			*/
		p[j].x=xmn; p[j].y=ymn; j++;
	       }	/*  End iy == -1			*/

	     else if (iy == 0)
	       {
		if (ixm == 0)
	          {
		   if (iym == -1)
		     {
		      if (xx > xmn) {	p[j].x=xx;  p[j].y=ymn; j++;
					p[j].x=xmn; p[j].y=yy;  j++;}
		      else {p[j].x=xmn; p[j].y=ymn; j++;}
		     }
		   else if (iym == 0) { p[j].x=xmn; p[j].y=yy;  j++;}
		   else if (iym == 1)
		     {
		      if (xx2 > xmn) {	p[j].x=xx2; p[j].y=ymx; j++;
					p[j].x=xmn; p[j].y=yy;  j++;}
		      else {p[j].x=xmn; p[j].y=ymx; j++;}
		     }
		  }	/*  End ixm == 0			*/
		else if (ixm == 1)
	          {
		   if (iym == -1)
		     {
		      if (yy > ymn)
			{
			 if (xx < xmx) {p[j].x=xx;  p[j].y=ymn; j++;}
			 else {		p[j].x=xmx; p[j].y=yy2; j++;}
					p[j].x=xmn; p[j].y=yy;  j++;
			}
		      else {p[j].x=xmn; p[j].y=ymn; j++;}
		     }
		   else if (iym == 0)  {p[j].x=xmx; p[j].y=yy2; j++;
					p[j].x=xmn; p[j].y=yy;  j++;}
		   else if (iym == 1)
		     {
		      if (yy < ymx)
		        {
			 if (xx2 > xmx) {p[j].x=xmx; p[j].y=yy2; j++;}
			 else {		 p[j].x=xx2; p[j].y=ymx; j++;}
					 p[j].x=xmn; p[j].y=yy;  j++;
			}
		      else {p[j].x=xmn; p[j].y=ymx; j++;}
		     }
		  }	/*  End ixm == 1			*/
		p[j].x=xmn; p[j].y=y; j++;
	       }	/*  End iy == 0				*/

	     else if (iy == 1)
	       {
		if (ixm == 0)
	          {
		   if (iym == -1)
		     {
		      if (xx > xmn)
			{
					p[j].x=xx;  p[j].y=ymn; j++;
		         if (yy < ymx) {p[j].x=xmn; p[j].y=yy;  j++;}
		         else {		p[j].x=xx2; p[j].y=ymx; j++;}
			}
		      else {p[j].x=xmn; p[j].y=ymn; j++;]
		     }
		   else if (iym == 0)
		     {
		      if (yy > ymx) {	p[j].x=xx2; p[j].y=ymx; j++;}
		      else {		p[j].x=xmn; p[j].y=yy;  j++;}
		     }
		  }	/*  End ixm == 0			*/
		else if (ixm == 1)
	          {
		   if (iym == -1)
		     {
		      if (yy > ymn && xx2 < xmx)
			{
			 if (xx > xmx) {p[j].x=xmx; p[j].y=yy2; j++;}
			 else {		p[j].x=xx;  p[j].y=ymn; j++;}
			 if (yy < ymx) {p[j].x=xmn; p[j].y=yy;  j++;}
			 else {		p[j].x=xx2; p[j].y=ymx; j++;}
			}
		      else if (yy <= ymn) {p[j].x=xmn; p[j].y=ymn; j++;}
		      else {p[j].x=xmx; p[j].y=ymx; j++;}
		     }
		   else if (iym == 0)
		     {
		      if (yy2 < ymx)
			{
			 		p[j].x=xmx; p[j].y=yy2; j++;
			 if (yy < ymx) {p[j].x=xmn; p[j].y=yy;  j++;}
			 else {		p[j].x=xx2; p[j].y=ymx; j++;}
			}
		     }
		  }	/*  End ixm == 1			*/
		p[j].x=xmn; p[j].y=ymx; j++;
	       }	/*  End iy == 1				*/

	    }	/*  End ix == -1				*/

/*			Current point in middle (horizontal).		*/

	  else if (ix == 0)
	    {
/*			Current point at bottom middle.			*/

	     if (iy == -1)
	       {
		if (ixm == -1)
	          {
		   if (iym == 0)
		     {
		      if (xx > xmn) {	p[j].x=xmn; p[j].y=yy;  j++;
					p[j].x=xx;  p[j].y=ymn; j++;}
		      else {p[j].x=xmn; p[j].y=ymn; j++;
		     }
		   else if (iym == 1)
		     {
		      if (yy > ymn)
			{
		         if (xx2 > xmn) {p[j].x=xx2; p[j].y=ymx; j++;}
		         else {		 p[j].x=xmn; p[j].y=yy;  j++;}
					 p[j].x=xx;  p[j].y=ymn; j++;
			}
		      else {p[j].x=xmn; p[j].y=ymn; j++;}
		     }
		  }	/*  End ixm == -1			*/
		else if (ixm == 0)
		  {
		   if (iym == 0) {	p[j].x=xx;   p[j].y=ymn; j++;}
		   else if (iym == 1) {	p[j].x=xx2;  p[j].y=ymx; j++;
					p[j].x=xx;   p[j].y=ymn; j++;}
		  }
		else if (ixm == 1)
	          {
		   if (iym == 0)
		     {
		      if (xx < xmx) {	p[j].x=xmx; p[j].y=yy2; j++;
					p[j].x=xx;  p[j].y=ymn; j++;}
		      else {p[j].x=xmx; p[j].y=ymn; j++;}
		     }
		   else if (iym == 1)
		     {
		      if (xx < xmx)
		        {
			 if (yy2 < ymx) {p[j].x=xmx; p[j].y=yy2; j++;}
			 else {		 p[j].x=xx2; p[j].y=ymx; j++;}
					 p[j].x=xx;  p[j].y=ymn; j++;
			}
		      else {p[j].x=xmx; p[j].y=ymn; j++;}
		     }
		  }	/*  End ixm == 1			*/
		p[j].x=x; p[j].y=ymn; j++;
	       }	/*  End iy == -1			*/

	     else if (iy == 1)
	       {
		if (ixm == -1)
	          {
		   if (iym == -1)
		     {
		      if (yy < ymx)
			{
		         if (xx < xmn) {p[j].x=xmn; p[j].y=yy;  j++;}
		         else {		p[j].x=xx;  p[j].y=ymn; j++;}
					p[j].x=xx2; p[j].y=ymx; j++;
			}
		      else {p[j].x=xmn; p[j].y=ymx; j++;}
		     }
		   else if (iym == 0)
		     {
		      if (xx2 > xmn) {	p[j].x=xmn; p[j].y=yy;  j++;
					p[j].x=xx2; p[j].y=ymx; j++;}
		      else {p[j].x=xmn; p[j].y=ymx; j++;}
		     }
		  }	/*  End ixm == -1			*/
		else if (ixm == 0)
		  {
		   if (iym == -1) {	p[j].x=xx;  p[j].y=ymn; j++;
					p[j].x=xx2; p[j].y=ymx; j++;}
		   else if (iym == 0) {	p[j].x=xx2; p[j].y=ymx; j++;}
		  }
		else if (ixm == 1)
	          {
		   if (iym == -1)
		     {
		      if (yy2 < ymx)
			{
			 if (xx < xmx) { p[j].x=xx;  p[j].y=ymn; j++;}
			 else {		 p[j].x=xmn; p[j].y=yy2; j++;}
			    		 p[j].x=xx2; p[j].y=ymx; j++;
			}
		      else {p[j].x=xmx; p[j].y=ymx; j++;}
		     }
		   else if (iym == 0)
		     {
		      if (xx2 < xmx) {	p[j].x=xmx; p[j].y=yy2; j++;
					p[j].x=xx2; p[j].y=ymx; j++;}
		      else {p[j].x=xmx; p[j].y=ymx; j++;}
		     }
		  }	/*  End ixm == 1			*/
		p[j].x=x; p[j].y=ymx; j++;}
	       }	/*  End iy == 1				*/
	    }	/*  End ix == 0				*/

/*			Current point on right side.			*/

	  else if (ix == 1)
	    {
/*			Current point at bottom right.			*/

	     if (iy == -1)
	       {
		if (ixm == -1)
	          {
		   if (iym == 0)
		     {
		      if (xx > xmn)
		        {
					p[j].x=xmn; p[j].y=yy;  j++;
		         if (xx < xmx) {p[j].x=xx;  p[j].y=ymn; j++;}
		         else {		p[j].x=xmx; p[j].y=yy2; j++;}
		        }
		      else {p[j].x=xmn; p[j].y=ymn; j++;}
		     }
		   else if (iym == 1)
		     {
		      if (xx > xmn && xx2 < xmx)
			{
			 if (xx2 > xmn){p[j].x=xx2; p[j].y=ymx; j++;}
			 else {		p[j].x=xmn; p[j].y=yy;  j++;}
		         if (xx > xmx) {p[j].x=xmx; p[j].y=yy2; j++;}
		         else {		p[j].x=xx;  p[j].y=ymn; j++;}
			}
		      else if (x <= xmn) {p[j].x=xmn; p[j].y=ymn; j++;}
		     }
		  }	/*  End ixm == -1			*/
		else if (ixm == 0)
	          {
		   if (iym == 0)
		     {
		      if (xx < xmx)	{p[j].x=xx;  p[j].y=ymn; j++;}
		      else {		 p[j].x=xmx; p[j].y=yy2;  j++;}
		     }
		   else if (iym == 1)
		     {
		      if (xx2 < xmx)
		        {
					p[j].x=xx2; p[j].y=ymx; j++;
			 if (xx < xmx) {p[j].x=xx;  p[j].y=ymn; j++;}
			 else	{	p[j].x=xmx; p[j].y=yy2; j++;}
			}
		      else {p[j].x=xmx; p[j].y=ymx; j++;}
		     }
		  }	/*  End ixm == 0			*/
		p[j].x=xmx; p[j].y=ymn; j++;
	       }	/*  End iy == -1			*/

	     else if (iy == 0)
	       {
		if (ixm == -1)
	          {
		   if (iym == -1)
		     {
		      if (xx < xmx)
			{
			 if (xx > xmn)	{p[j].x=xx;  p[j].y=ymn; j++;}
			 else	{	 p[j].x=xmn; p[j].y=yy;  j++;}
					 p[j].x=xmx; p[j].y=yy2; j++;
			}
		      else {p[j].x=xmx; p[j].y=ymn; j++;}
		     }
		   else if (iym == 0) { p[j].x=xmn; p[j].y=yy;  j++;
					p[j].x=xmx; p[j].y=yy2; j++;}
		   else if (iym == 1)
		     {
		      if (xx2 < xmx)
			{
			 if (xx2 > xmn) {p[j].x=xx2; p[j].y=ymx; j++;}
			 else	{	 p[j].x=xmn; p[j].y=yy;  j++;}
					 p[j].x=xmx; p[j].y=yy2;  j++;
			}
		      else {p[j].x=xmx; p[j].y=ymx; j++;}
		     }
		  }	/*  End ixm == -1			*/
		else if (ixm == 0)
	          {
		   if (iym == -1)
		     {
		      if (xx < xmx) {	p[j].x=xx;  p[j].y=ymn; j++;
					p[j].x=xmx; p[j].y=yy2; j++;}
		      else {p[j].x=xmx; p[j].y=ymn; j++;}
		     }
		   else if (iym == 0){	p[j].x=xmx; p[j].y=yy2; j++;}
		   else if (iym == 1)
		     {
		      if (xx2 < xmx) {	p[j].x=xx2; p[j].y=ymx; j++;
					p[j].x=xmx; p[j].y=yy2; j++;}
		      else {p[j].x=xmx; p[j].y=ymx; j++;}
		     }
		  }	/*  End ixm == 0			*/
		p[j].x=xmx; p[j].y=y; j++;
	       }	/*  End iy == 0				*/

	     else if (iy == 1)
	       {
		if (ixm == -1)
	          {
		   if (iym == -1)
		     {
		      if (xx < xmx && yy < ymx)
			{
			 if (xx > xmn) {p[j].x=xx;  p[j].y=ymn; j++;}
			 else {		p[j].x=xmn; p[j].y=yy;  j++;}
			 if (yy2 < ymx){p[j].x=xmx; p[j].y=yy2; j++;}
		         else	       {p[j].x=xx2; p[j].y=ymx; j++;}
			}
		      else if (xx >= xmx) {p[j].x=xmx; p[j].y=ymn; j++;}
		      else if (yy >= ymx) {p[j].x=xmn; p[j].y=ymx; j++;}
		     }
		   else if (iym == 0)
		     {
		      if (xx2 > xmn)
			{
					  p[j].x=xmn; p[j].y=yy;  j++;
		         if (yy2 > ymx) { p[j].x=xx2; p[j].y=ymx; j++;}
		         else {		  p[j].x=xmx; p[j].y=yy2;  j++;}
			}
		      else {p[j].x=xmn; p[j].y=ymx; j++;}
		     }
		  }	/*  End ixm == -1			*/
		else if (ixm == 0)
	          {
		   if (iym == -1)
		     {
		      if (xx < xmx)
			{
			    		 p[j].x=xx; p[j].y=ymn;  j++;
			 if (xx2 > xmx) {p[j].x=xmx; p[j].y=yy2; j++;}
			 else {		 p[j].x=xx2;  p[j].y=ymx; j++;}
			}
		      else {p[j].x=xmx; p[j].y=ymn; j++;}
		     }
		   else if (iym == 0)
		     {
		      if (yy2 < ymx) {p[j].x=xmx; p[j].y=yy2;  j++;}
		      else {	      p[j].x=xx2; p[j].y=ymx; j++;}
		     }
		  }	/*  End ixm == 0			*/
	       }	/*  End iy == 1				*/
	     p[j].x=xmx; p[j].y=ymx; j++;
	    }	/*  End ix == 1				*/

	 }	/*  End for i					*/
       if (j > 0)
             cgm_points(cgmo,num,id,p,j);
       cgm_flushit(cgmo,num,id);
       return 1;
      }
 
    int cgm_init()
      {
       markit=fillit=0;
       return OK;
      }
    int cgm_points(mf_cgmo **cgmo,int num,int id,Gpoint *pos,Gint num_pt)
      {
       if (num_pt <= 0) return 0;
       if (id == LINE_ID)
	 {
	  if (num_pt < 2) return OK;
          mo_insure_in_body(cgmo, num);
          mo_header(PRIMITIVE_CL, id);
          mo_points(cgmo, num, (char*)pos, num_pt);
          mo_flush(cgmo, num, 0);
          mo_mode(cgmo, num, CGMO_NOT_EMPTY);
	 }
       else if (id == MARKER_ID)
	 {
	  if (!markit)
	    {
	     mo_insure_in_body(cgmo, num);
	     mo_header(PRIMITIVE_CL, id);
	     markit=1;
	    }
	  mo_points(cgmo, num, (char*)pos, num_pt);
	 }
       else if (id == POLYGON_ID)
	 {
	  if (!fillit)
	    {
	     mo_insure_in_body(cgmo, num);
	     mo_header(PRIMITIVE_CL, id);
	     fillit=1;
	    }
	  mo_points(cgmo, num, (char*)pos, num_pt);
	 }
       return OK;
      }
    int cgm_flushit(mf_cgmo **cgmo,int num,int id)
      {
       if (markit || fillit)
	 {
          mo_flush(cgmo, num, 0);
          mo_mode(cgmo, num, CGMO_NOT_EMPTY);
	 }
       return OK;
      }
