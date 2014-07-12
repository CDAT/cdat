/* #include "Python.h" */
#include <stdio.h>
#include <ctype.h>
#include <string.h>
#include <math.h>
#include "array.h"
#include "list.h"
#include "project.h"
#include "picture.h"
#include "graph.h"
#include "display.h"
#include "workstations.h"
#include "vcs_legacy_marker.h"
#include "gks.h"
#include "gksshort.h"

extern Gconid_X_drawable connect_id;
extern struct workstations Wkst[];
extern struct display_tab D_tab;
extern int update_ind;
extern struct table_fill Tf_tab;
extern struct default_continents Dc;

extern FILE *fpin,*fpout,*fperr;

int nice15(float a,float b,float *dr,int *pw10,float *center);
extern int generic_world (float x1,float y1,float x2,float y2, char *lb,
			  char *trans_xaxis, char *trans_yaxis);
extern int generic_world2 (float x1,float y1,float x2,float y2, char *lb,
			   char *trans_xaxis, char *trans_yaxis);

float getArrayValueAsFloat( struct a_attr *array,int index)
{
  float value;
  if (array->mask[index]!=1){
    value=1.e20;}
  else
    value=array->un.data[index];
/*   printf("values is: %f, %d\n",value,index); */
  return value;
}

int isInsidePolygon(float X, float Y, int n,float *XS,float *YS)
     /* determine if the angles of a domain are inside a polygon */
{
  int i,j;
  int cuts,result;
  float a,b,xx;
  cuts=0;
  result=0;
/*   printf("X/Y,n: %f,%f,%i\n",X,Y,n); */
/*   for (i=0;i<n;i++)  printf("i,x,y: %d, %f, %f\n",i,XS[i],YS[i]); */

  for (i=0;i<n;i++)
    {
      j=i+1;
      if (j==n) j=0;
      if (XS[i]!=XS[j]) /* not a vertical line */
	{
	  a=(YS[i]-YS[j])/(XS[i]-XS[j]);
	  b=YS[i]-a*XS[i];
	  if (a!=0) /*not an horizontal line */
	    {
	      xx=(Y-b)/a;
	      if (xx>=X)
		{
		  if ((YS[i]>YS[j])&&((Y<=YS[i])&&(Y>=YS[j]))) cuts++;
		  else if ((YS[j]>YS[i])&&((Y<=YS[j])&&(Y>=YS[i]))) cuts++;
		}
	    }
	} 
      else if (X<XS[i])
	{
	  if ((YS[i]>YS[j])&&((Y<=YS[i])&&(Y>=YS[j]))) cuts++;
	  else if ((YS[j]>YS[i])&&((Y<=YS[j])&&(Y>=YS[i]))) cuts++;
	}
    }
/*   printf("cuts:%d\n",cuts); */
  if ((cuts%2)==1) result=1;
/*   if (result==1) { */
/*     printf("Succes for: %f,%f which is inside:\n",X,Y); */
/*     for (i=0;i<n;i++){ */
/*     printf("%i: %f,%f\n",i,XS[i],YS[i]);}} */
  return result;
}

/* void showxy(Gpoint *xy,int n) */
/* { */
/*   int i; */
/*   for (i=0;i<n;i++) printf("x, y: %d, %d\n",xy[i].x,xy[i].y); */
/* } */
  
void drawPolygon(float value, int i, struct a_attr *pmesh,
		 float xoff,float yoff, int n2,struct gfm_attr *pGfm)
{
  int j,n1;
  Gpoint *xy;
  float tmp,tmp2,X1,X2,Y1,Y2;
  struct table_fill *pf;
  struct fill_range *po;
  extern int trans_continent();

/*   printf("drawing:%d\n",value); */
  X1=pGfm->dsp[0];
  Y1=pGfm->dsp[1];
  X2=pGfm->dsp[2];
  Y2=pGfm->dsp[3];
  j=n2+1;
  xy=malloc(j*sizeof(Gpoint));
  for (j=0;j<n2;j=j+1)
    {
      n1=i*(pmesh->XS[1][0]*pmesh->XS[0][0])+j;
      tmp=getArrayValueAsFloat(pmesh,n1);
      xy[j].y=tmp+yoff;
      tmp=getArrayValueAsFloat(pmesh,n1+pmesh->XS[0][0]);
      xy[j].x=tmp+xoff;
    }
  xy[n2].x=xy[0].x;
  xy[n2].y=xy[0].y;
  if (( ((strcmp("linear",pGfm->xat) != 0) &&
	(strcmp("",pGfm->xat) != 0)) &&
       (strcmp("linear",pGfm->proj) == 0) ) 
  
  || ( ((strcmp("linear",pGfm->yat) != 0) &&
	(strcmp("",pGfm->yat) != 0)) &&
       (strcmp("linear",pGfm->proj) == 0) )) {
    trans_continent(n2+1, xy, pGfm->xat, pGfm->yat);
      }
  if (value<1.E20)
    {
      for (po=pGfm->line; po!=NULL; po=po->next)
   	{
	  tmp=po->lev1;
	  tmp2=po->lev2;
	  if (tmp>tmp2) {
	    if (tmp<9.9E19) {
	    tmp=tmp2;
	    tmp2=po->lev1;
	    }
	    else {
	      tmp=-1.e20;
	    }
	  }
	  if (tmp>.99E20 && tmp2>.99E20) tmp=-.99E20;
/* 	  printf("BOUNDARIES: %d,%d, %d\n",tmp,tmp2,value); */
	  if ((tmp<value) && (tmp2>=value))
	    {
	      /* Set up the fill area definitions. */
	      
	      for (pf=&Tf_tab;pf!=NULL;pf=pf->next)
		if (strcmp(pf->name,po->fill_name) == 0)
		  {
/* 		    printf("%d,%d,%d\n",pf->faci[0],pf->fais[0],pf->fasi[0]); */
		    gsfaci(pf->faci[0]);
		    gsfais(pf->fais[0]);
		    gsfasi(pf->fasi[0]);
		  }
	      proj_gfa(n2+1,xy);	
	    }
	}
    }
  else
    { 
      if ((int)pGfm->missing!=240)
	{
	  /* Set up the fill area definitions. */
/* 	  printf("Missing color:%d\n",(int)pGfm->missing); */
	  gsfaci((int)(pGfm->missing));
	  gsfais(1);
	  gsfasi(1);
	  /* 		    gsfais(pf->fais[0]); */
	  /* 		    gsfasi(pf->fasi[0]); */
	  proj_gfa(n2+1,xy);
	}
    }	
  if (pGfm->mesh==1) 
    {
      gsln(1); /* solid line */
      gsplci(241); /* color of the mesh line : black*/
      proj_pl(n2+1,xy);
    }
free(xy);
}

int meshfill(pD,pGfm,pP,pdata,pmesh)
     struct display_tab *pD;
     struct gfm_attr *pGfm;
     struct p_tab *pP;
     struct a_attr *pdata,*pmesh; 
{
  int i,j,iw,jw,ix,erret,n,k1,k2;
  int tmp,ncells;
    /* int ndraw;*/
  float x,y,value,ftmp;
  float X1,X2,Y1,Y2,wc[4],dsp[4];
  float Xmin=1.E23,Ymin=1.E23,Xmax=-1.E23,Ymax=-1.E23;
  float xwrap,ywrap;
  float xoff,yoff;
  int n1,n2,nxwrap,nywrap;
  float Xcorners[4];
  float Ycorners[4];
  float *xpoly,*ypoly;
  char proj[256],tmpchar[256];
  extern int segment_num;
  Glimit pvp;
  float dr,del,center;
  struct fill_range *pl,*p1,*pl1,*plg,*plsave;
  struct table_fill *pt,*ptt;
  extern int trans_coord();
  extern struct fill_range *generate_auto_fill_range(float min,float max);
  struct l_tab *pltab=NULL;
  extern struct l_tab L_tab[2];
	int savecontinents;

  erret=0;
  pdata->min=1.E21;
  pdata->max=-1.E21;
  /* Dimensions */
  n1=pdata->XS[0][0]; /* number of cell to plot */
  n2=pmesh->XS[0][0]; /* number of vertices for each cell */
  xwrap=pGfm->xwrap;
  ywrap=pGfm->ywrap;

  /* save the world coordinates to put back at the end*/
  wc[0]=pGfm->dsp[0];
  wc[1]=pGfm->dsp[1];
  wc[2]=pGfm->dsp[2];
  wc[3]=pGfm->dsp[3];

  plsave=pl=plg=pGfm->line;
  /* now figures out if it needs to reset the world coordinates */
  if (( wc[0]>=1.E20 || wc[1]>=1.E20 || wc[2]>=1.E20 || wc[3]>=1.E20 ) ||
      (pl == NULL || (pl->lev1 >= 0.99e20 && pl->lev2 >= 0.99e20 &&
		      pl->next == NULL))){
    X1= 1.E30; /* xmin for mesh */
    X2=-1.E30; /* xmax for mesh */
    Y1= 1.E30; /* ymin for mesh */
    Y2=-1.E30; /* ymax for mesh */
    for (i=0;i<n1;i++){
      for (j=0;j<n2;j++){
	k1=i*pmesh->XS[0][0]*pmesh->XS[1][0]+j;
	y=(float)getArrayValueAsFloat(pmesh,k1);
	x=(float)getArrayValueAsFloat(pmesh,k1+pmesh->XS[0][0]);
	if (x>X2 && x<9.9E19) {X2=x;}
	if (x<X1 && x<9.9E19) {X1=x;}
	if (y>Y2 && y<9.9E19) {Y2=y;}
	if (y<Y1 && y<9.9E19) {Y1=y;}
      }
      /* max and min values */
      x=getArrayValueAsFloat(pdata,i);
      if ( pdata->min>x && x<1.E20 ) {pdata->min=x;}
      if ( pdata->max<x && x<1.E20 ) {pdata->max=x;}
    }
    if ( wc[0]>=1.E20 ) {pGfm->dsp[0]=X1;}
    if ( wc[1]>=1.E20 ) {pGfm->dsp[1]=Y1;}
    if ( wc[2]>=1.E20 ) {
      if (xwrap==0.) {
	pGfm->dsp[2]=X2;}
      else if ((X2-X1)<=xwrap){
	pGfm->dsp[2]=X2;}
      else{
	pGfm->dsp[2]=pGfm->dsp[0]+xwrap;
      }}
    if ( wc[3]>=1.E20 ) {
      if (ywrap==0.) {
	pGfm->dsp[3]=Y2;}
      else if ((Y2-Y1)<=ywrap){
	pGfm->dsp[3]=Y2;}
      else{
	pGfm->dsp[3]=pGfm->dsp[1]+ywrap;
      }}
  }

  /* Now sets the levels if needed and assume reference at 0.0.	*/
  if (pl == NULL || (pl->lev1 >= 0.99e20 && pl->lev2 >= 0.99e20 &&
		     pl->next == NULL) )
    {
      plg=generate_auto_fill_range(pdata->min,pdata->max);
      if (plg==NULL)
	{
	  err_warn(1,fperr,
		   "Error - no memory to compute meshfill ranges (%s)\n",
		   pD->name);
	  pD->off=2;
	  pD->dsp_seg[1]=0;
	  pD->dsp_seg[2]=0;
	  pD->dsp_seg[3]=0;
	  erret++;
	}
    }
  pGfm->line=plg;

  
  if (pD->leg_seg[3] > 0)
    {
      gcrsg(pD->leg_seg[0]=++segment_num);
      gssgp(pD->leg_seg[0],(pP->leg.p+pD->pri)/1000.0);
      
      /* Check to see if it is a List or a string of numbers.	*/
      if (pGfm->legend != NULL) {
	pltab=&L_tab[0];
	while ((pltab != NULL) && (strcmp(pltab->name,pGfm->legend) != 0))
	  pltab=pltab->next;
      }
      
      if (legendGfi(pGfm->line,pP,pD, pltab) > 0)
	{
	  gclsg();
	  pD->leg_seg[1]=1;
	  pD->leg_seg[2]=1;
	  pD->leg_seg[3]=0;
	}
      else
	{
	  gclsg();
	  gdsg(pD->leg_seg[0]);
	  pD->leg_seg[0]=0;
	  pD->leg_seg[1]=0;
	  pD->leg_seg[2]=0;
	  pD->leg_seg[3]=0;
			erret++;
	}
    }

  /* world coordinates */
  /* preserve the world coordinate as "real" (for use later algorithm, stuff like isinsidepolygon,etc...)*/
  X1=pGfm->dsp[0];
  Y1=pGfm->dsp[1];
  X2=pGfm->dsp[2];
  Y2=pGfm->dsp[3];
/*                              Transform the coordinate axes.         */
  if ( ((strcmp("linear",pGfm->xat) != 0) &&
	(strcmp("",pGfm->xat) != 0)) &&
       (strcmp("linear",pGfm->proj) == 0) ) {
    trans_coord(&pGfm->dsp[0], &pGfm->dsp[2], pGfm->xat, NULL, "x");}
  
  if ( ((strcmp("linear",pGfm->yat) != 0) &&
	(strcmp("",pGfm->yat) != 0)) &&
       (strcmp("linear",pGfm->proj) == 0) ) {
    trans_coord(&pGfm->dsp[1], &pGfm->dsp[3], pGfm->yat, NULL, "y");}
  
  /* Now does the projections and sets the display to rember dsp and projection actually used*/
  pD->dsp_used[0]=dsp[0]=pGfm->dsp[0];
  pD->dsp_used[1]=dsp[1]=pGfm->dsp[1];
  pD->dsp_used[2]=dsp[2]=pGfm->dsp[2];
  pD->dsp_used[3]=dsp[3]=pGfm->dsp[3];
  strcpy(pD->proj_name,pGfm->proj);
  strcpy(proj,pGfm->proj);
  
  if (set_projection(proj,pP->dsp,pGfm->dsp,dsp) == 0)
    {
      err_warn(1,fperr,"Error - in projection for MESHFILLS.\n");
      erret++;
    }

  /*			Check NDC space is greater than zero.		*/
  
  if (pP->dsp.x2-pP->dsp.x1 < .001 || pP->dsp.y2-pP->dsp.y1 < .001)
    {
      err_warn(1,fperr,
	       "Error - NDC display space is nil for MESHFILL.\n");
      pD->dsp_seg[1]=0;
      pD->off=2;
      erret++;
    }
  
  /* viewport */
  pvp.xmin = pP->dsp.x1;
  pvp.xmax = pP->dsp.x2;
  pvp.ymin = pP->dsp.y1;
  pvp.ymax = pP->dsp.y2;
  /*printf("viewport: %d,%d,%d,%d\n",vp[0],vp[1],vp[2],vp[3]);*/
  gsvp(2,&pvp);
  gswn(2,&pvp);
  gselnt(2);
  gsclip(GCLIP);

  
  
  if (pD->dsp_seg[0] > 0 && pD->dsp_seg[3] > 0)
    {
      gdsg(pD->dsp_seg[0]);
      pD->dsp_seg[0]=0;
    }

  /* store the corners of the plot area*/
  Xcorners[0]=X1;
  Ycorners[0]=Y1;

  Xcorners[1]=X2;
  Ycorners[1]=Y1;

  Xcorners[2]=X2;
  Ycorners[2]=Y2;

  Xcorners[3]=X1;
  Ycorners[3]=Y2;

  /* Create the segment */
  gcrsg(pD->dsp_seg[0]=++segment_num) ; 
  /* Set the priority */
  /*printf("pD->pri is : %d,%d\n",pD->pri,pP->dsp.p);*/
  gssgp(pD->dsp_seg[0],(pP->dsp.p+pD->pri)/1000.0); 
  /*ndraw=0;*/  
  /* Now loop through the mesh */
  for (i=0;i<n1;i++){
/*     if (i==0) printf("First index is: %d\n",i*pmesh->XS[0][0]*pmesh->XS[1][0]); */
    ftmp=getArrayValueAsFloat(pmesh,i*pmesh->XS[0][0]*pmesh->XS[1][0]);
    if ( ftmp<1.E20 ) {
/*       printf("in there !\n"); */
      xoff=0.;
      yoff=0.;
      nxwrap=1;
      nywrap=1;
      ncells=0;
      for (j=0;j<n2;j++)
	{
	  k1=i*pmesh->XS[0][0]*pmesh->XS[1][0]+j;
	  y=(float)getArrayValueAsFloat(pmesh,k1);
	  if (y<1.E20) ncells=ncells+1;
	}
      if ((xwrap!=0.)||(ywrap!=0.))
	{
	  Xmin=1.E23;
	  Xmax=-1.E23;
          Ymin=1.E23;
          Ymax=-1.E23;
	  /* first determine the max and min X and Y of the polygon (for wrapping) */
	  for (j=0;j<ncells;j++)
	    {
	      k1=i*pmesh->XS[0][0]*pmesh->XS[1][0]+j;
	      y=(float)getArrayValueAsFloat(pmesh,k1);
	      x=(float)getArrayValueAsFloat(pmesh,k1+pmesh->XS[0][0]);
	      if (x>Xmax && x<1.E20) Xmax=x;
	      if (x<Xmin && x<1.E20) Xmin=x;
	      if (y>Ymax && y<1.E20) Ymax=y;
	      if (y<Ymin && y<1.E20) Ymin=y;
	    }
	  /* Now set the x/y offset in order to be left/lower and X1/Y1 */
	  /* and count how many wrap to do in each direction  */
	  if (xwrap!=0.) 
	    {
	      while (Xmax>X1) { Xmin=Xmin-xwrap;Xmax=Xmax-xwrap;xoff=xoff-xwrap;}
	      while((Xmin+xwrap)<=X2) {Xmin=Xmin+xwrap;nxwrap=nxwrap+1;}
	    }
	  if (ywrap!=0.)
	    {
	      while (Ymax>Y1) { Ymin=Ymin-ywrap;Ymax=Ymax-ywrap;yoff=yoff-ywrap;}
	      while((Ymin+ywrap)<=Y2) {Ymin=Ymin+ywrap;nywrap=nywrap+1;}
	    }
	}
      /*Now do the wraping thing  */
      for (iw=0;iw<nywrap;iw++) /*y wrap */
	{
	  for (jw=0;jw<nxwrap;jw++)
	    {
	      tmp=0;
	      for (j=0;j<ncells;j++)
		{
		  /* construct the array of the corners */
		  k1=i*pmesh->XS[0][0]*pmesh->XS[1][0]+j;
		  y=(float)getArrayValueAsFloat(pmesh,k1)+yoff;
		  x=(float)getArrayValueAsFloat(pmesh,k1+pmesh->XS[0][0])+xoff;
		  /* Is it in the domain ?  */
		  if (isInsidePolygon(x,y,4,Xcorners,Ycorners))
		    {
		      /*ndraw=ndraw+1;*/
		      value=getArrayValueAsFloat(pdata,i);
		      drawPolygon(value, i, pmesh,xoff,yoff,ncells,pGfm);
		      if (value>pdata->max && value<1.E20) { pdata->max=value;}
		      if (value<pdata->min && value<1.E20) { pdata->min=value;}
		      tmp=1;
		      break;
		    }
		}
	      
	      if (tmp==0)
		{
		  /* creating the polygon coordinates */
		  if ((xpoly=(float *) malloc(ncells*sizeof(float))) == NULL)
		  { printf("OOOPS !!! X ERROR !!!:%d\n",ncells);}
		  if ((ypoly=(float *) malloc(ncells*sizeof(float))) == NULL)
		  { printf("OOOPS !!! Y ERROR !!!: %d\n",ncells);}
		  for (j=0;j<ncells;j++)
		    {
		      ix=i*pmesh->XS[0][0]*pmesh->XS[1][0]+j;
		      ypoly[j]=(float)getArrayValueAsFloat(pmesh,ix);
		      xpoly[j]=(float)getArrayValueAsFloat(pmesh,ix+pmesh->XS[1][0]);
		    }
		  if (isInsidePolygon(X1-xoff,Y1-yoff,ncells,xpoly,ypoly))
		    {
		      tmp=1;
		    }
		  else if (isInsidePolygon(X2-xoff,Y1-yoff,ncells,xpoly,ypoly))
		    {
		      tmp=1;
		    }
		  else if (isInsidePolygon(X2-xoff,Y2-yoff,ncells,xpoly,ypoly))
		    {
		      tmp=1;
		    }	      
		  else if (isInsidePolygon(X1-xoff,Y2-yoff,ncells,xpoly,ypoly))
		    {
		      tmp=1;
		    }
		  free(xpoly);
		  free(ypoly);
		  if (tmp==1) {
		    value=getArrayValueAsFloat(pdata,i);
		    /*ndraw=ndraw+1;*/
		    drawPolygon(value, i, pmesh,xoff,yoff,ncells,pGfm);
		    if (value>pdata->max && value<1.E20) { pdata->max=value;}
		    if (value<pdata->min && value<1.E20) { pdata->min=value;}
		  }
		}
	      xoff=xoff+xwrap; /* ok next set of points let's reset xoff... */
	    }
	  for (jw=0;jw<nxwrap;jw++) xoff=xoff-xwrap ;
	  yoff=yoff+ywrap;
	}
    } /* end of mesh isn't missing */
  } /* end of for i in n1 */
  /*printf("ndraw: %d,%d\n",ndraw,n1);*/
  savecontinents = Dc.selected;
  if (pD->continents==-1) pD->continents = Dc.selected;
  else Dc.selected = pD->continents;
  if (pD->continents > 0)
    {
  X1=pGfm->dsp[0];
  Y1=pGfm->dsp[1];
  X2=pGfm->dsp[2];
  Y2=pGfm->dsp[3];
      /*    generic_world coordinate values */
      if (pD->continents  < 3)
	generic_world(X1,Y1,X2,Y2,Dc.lb,pGfm->xat,pGfm->yat);
      else
	generic_world2(X1,Y1,X2,Y2,Dc.lb,pGfm->xat,pGfm->yat);
    }
  Dc.selected = savecontinents;
  gclsg();
  pD->dsp_seg[1]=1;
  pD->dsp_seg[2]=1;
  pD->dsp_seg[3]=0;
  gselnt(0);
  gsclip(GNOCLIP);

  k1=pP->mean.p;
  pP->mean.p=0;
  pict_elem(pdata,pP,pD,pGfm->proj,
	    pGfm->xtl1,pGfm->xtl2,pGfm->xmt1,pGfm->xmt2,
	    pGfm->ytl1,pGfm->ytl2,pGfm->ymt1,pGfm->ymt2,
	    pGfm->dsp);
  pP->mean.p=k1;
  /* Reset the world coordinates */
  for (i=0;i<4;i++) {  pGfm->dsp[i]=wc[i] ;}
  /* Reset the levels */
  pGfm->line=plsave;
  killA_tmp();
  return 1;
}
