
// $Id: compute.h,v 1.3 2003/11/23 02:00:07 scorzell Exp $

/* signature computation */
/* modified by Fan:
   add new signatures: */
#ifndef COMPUTE_H
#define COMPUTE_H

#ifndef WIN32
#include <unistd.h>
#else
#pragma warning (disable:4244)
#pragma warning (disable:4018)
#pragma warning (disable:4390)
#endif

#include "basic.h"

inline void triSurfIntegral(double *x1, double *x2, double *x3, float v1, float v2,
                      float v3, float *fx, float *val, int nbucket, float min,
                      float max, float scaling)
{
   double *_t;
   float mid[2];
   float dist[2];
   float ival;
   float maxlen;
   float t;
   u_int first;

   if (v3 < v2) {
      _t = x3;
      x3 = x2;
      x2 = _t;

      t = v3;
      v3 = v2;
      v2 = t;
   }
   if (v2 < v1) {
      _t = x2;
      x2 = x1;
      x1 = _t;

      t = v2;
      v2 = v1;
      v1 = t;
   }
   if (v3 < v2) {
      _t = x3;
      x3 = x2;
      x2 = _t;

      t = v3;
      v3 = v2;
      v2 = t;
   }

   // need to compute point along v[0]->v[2]

   // a cell with constant value will not contribute to the length
   if (v3 == v1)
      return;

   ival = (v3-v2)/(v3-v1);
   mid[0] = (1.0-ival)*x3[0] + (ival)*x1[0];
   mid[1] = (1.0-ival)*x3[1] + (ival)*x1[1];

   dist[0] = mid[0]-x2[0];
   dist[1] = mid[1]-x2[1];
   maxlen = sqrt(sqr(dist[0]) + sqr(dist[1])) * scaling;

   first=ceil(((nbucket-1) * (v1-min))/(max-min));

   // compute the
   for (; fx[first] < v2; first++) {
      if (v1 == v2)
        val[first] += maxlen;
      else
        val[first]+=((fx[first]-v1)/(float)(v2-v1)) * maxlen;
   }
   for (; fx[first] < v3; first++) {
      if (v3==v2)
         val[first] += maxlen;
      else
         val[first]+=((v3-fx[first])/(float)(v3-v2)) * maxlen;
   }
}


inline void triVolIntegral(double *x1, double *x2, double *x3, float v1, float v2,
                      float v3, float *fx, float *val, float *cum, int nbucket,
                      float min, float max, float scaling)
{
   double *_t;
   float mid[2];
   float dist[2], dist2[2], dist3[2];
   float ival;
   float midarea, maxarea;
   float t;
   u_int first;

   if (v3 < v2) {
      _t = x3;
      x3 = x2;
      x2 = _t;

      t = v3;
      v3 = v2;
      v2 = t;
   }
   if (v2 < v1) {
      _t = x2;
      x2 = x1;
      x1 = _t;

      t = v2;
      v2 = v1;
      v1 = t;
   }
   if (v3 < v2) {
      _t = x3;
      x3 = x2;
      x2 = _t;

      t = v3;
      v3 = v2;
      v2 = t;
   }

   dist[0] = x2[0]-x1[0];
   dist[1] = x2[1]-x1[1];
   dist2[0] = x3[0]-x1[0];
   dist2[1] = x3[1]-x1[1];

   if (v3 == v1)
      midarea = maxarea = 0.5 * fabs(dist2[0]*dist[1]-dist2[1]*dist[0]);
   else {
      ival = (v3-v2)/(v3-v1);
      mid[0] = (1.0-ival)*x3[0] + (ival)*x1[0];
      mid[1] = (1.0-ival)*x3[1] + (ival)*x1[1];

      dist3[0] = mid[0]-x1[0];
      dist3[1] = mid[1]-x1[1];
      midarea = 0.5 * fabs(dist3[0]*dist[1]-dist3[1]*dist[0]);
      maxarea = 0.5 * fabs(dist2[0]*dist[1]-dist2[1]*dist[0]);
   }

   first=ceil(((nbucket-1) * (v1-min))/(max-min));

   // compute the
   for (; fx[first] < v2; first++) {
      if (v1 == v2)
        val[first] += midarea;
      else
        val[first]+=sqr((fx[first]-v1)/(float)(v2-v1)) * midarea;
   }
   for (; fx[first] < v3; first++) {
      if (v3==v2)
         val[first] += maxarea;
      else
         val[first]+= midarea + (1.0-sqr((v3-fx[first])/(float)(v3-v2))) * (maxarea-midarea);
   }

   if (first < nbucket);
      cum[first] += maxarea;
}

inline void tetSurfIntegral(float *x1, float *x2, float *x3, float *x4, float v1, float v2,
                      float v3, float v4, float *fx, float *val, int nbucket, float min,
                      float max, float scaling)
{
   float *_t;
   float mid[3], mid2[3];
   float vec1[3], vec2[3], vec3[3];
   float ival;
   float s;
   float area1, area2, midarea, volume;
   float t;
   float cp[3];
   u_int first;

   if (v4 < v3) {
      _t = x4; x4 = x3; x3 = _t;

      t = v4; v4 = v3; v3 = t;
   }
   if (v3 < v2) {
      _t = x3; x3 = x2; x2 = _t;

      t = v3; v3 = v2; v2 = t;
   }
   if (v2 < v1) {
      _t = x2; x2 = x1; x1 = _t;

      t = v2; v2 = v1; v1 = t;
   }
   if (v4 < v3) {
      _t = x4; x4 = x3; x3 = _t;

      t = v4; v4 = v3; v3 = t;
   }
   if (v3 < v2) {
      _t = x3; x3 = x2; x2 = _t;

      t = v3; v3 = v2; v2 = t;
   }
   if (v4 < v3) {
      _t = x4; x4 = x3; x3 = _t;

      t = v4; v4 = v3; v3 = t;
   }

   // need to compute point along v[0]->v[2]

   // a cell with constant value will not contribute to the length
   if (v4 == v1)
      return;


   // compute the first area
   if (v1 != v3)
      ival = (v3-v2)/(v3-v1);
   else
      ival = 0.0;
   mid[0] = (1.0-ival)*x3[0] + (ival)*x1[0];
   mid[1] = (1.0-ival)*x3[1] + (ival)*x1[1];
   mid[2] = (1.0-ival)*x3[2] + (ival)*x1[2];
   if (v1 != v4)
      ival = (v4-v2)/(v4-v1);
   else
      ival = 0.0;
   mid2[0] = (1.0-ival)*x4[0] + (ival)*x1[0];
   mid2[1] = (1.0-ival)*x4[1] + (ival)*x1[1];
   mid2[2] = (1.0-ival)*x4[2] + (ival)*x1[2];

   vec1[0] = mid[0]-x2[0];
   vec1[1] = mid[1]-x2[1];
   vec1[2] = mid[2]-x2[2];
   vec2[0] = mid2[0]-x2[0];
   vec2[1] = mid2[1]-x2[1];
   vec2[2] = mid2[2]-x2[2];
   cp[0] = vec1[1]*vec2[2]-vec1[2]*vec2[1];
   cp[1] = vec1[2]*vec2[0]-vec1[0]*vec2[2];
   cp[2] = vec1[0]*vec2[1]-vec1[1]*vec2[0];
   area1 = 0.5 * fabs(sqrt(sqr(cp[0]) + sqr(cp[1]) + sqr(cp[2]))) * scaling;


   // compute the second area
   if (v2 != v4)
      ival = (v4-v3)/(v4-v2);
   else
      ival = 0.0;
   mid[0] = (1.0-ival)*x4[0] + (ival)*x2[0];
   mid[1] = (1.0-ival)*x4[1] + (ival)*x2[1];
   mid[2] = (1.0-ival)*x4[2] + (ival)*x2[2];
   if (v4 != v1)
      ival = (v4-v3)/(v4-v1);
   else
      ival = 0.0;
   mid2[0] = (1.0-ival)*x4[0] + (ival)*x1[0];
   mid2[1] = (1.0-ival)*x4[1] + (ival)*x1[1];
   mid2[2] = (1.0-ival)*x4[2] + (ival)*x1[2];

   vec1[0] = mid[0]-x3[0];
   vec1[1] = mid[1]-x3[1];
   vec1[2] = mid[2]-x3[2];
   vec2[0] = mid2[0]-x3[0];
   vec2[1] = mid2[1]-x3[1];
   vec2[2] = mid2[2]-x3[2];
   cp[0] = vec1[1]*vec2[2]-vec1[2]*vec2[1];
   cp[1] = vec1[2]*vec2[0]-vec1[0]*vec2[2];
   cp[2] = vec1[0]*vec2[1]-vec1[1]*vec2[0];
   area2 = 0.5 * fabs(sqrt(sqr(cp[0]) + sqr(cp[1]) + sqr(cp[2]))) * scaling;

   if (v2-v1 != 0.0)
      midarea = area1 * (1.0+(v3-v2)/(v2-v1));
   else if (v4-v3 != 0.0)
      midarea = area2 * (1.0+(v3-v2)/(v4-v3));
   else {
      // have to compute the midarea
      vec1[0] = x2[0]-x1[0];
      vec1[1] = x2[1]-x1[1];
      vec1[2] = x2[2]-x1[2];
      vec2[0] = x3[0]-x1[0];
      vec2[1] = x3[1]-x1[1];
      vec2[2] = x3[2]-x1[2];
      vec3[0] = x4[0]-x1[0];
      vec3[1] = x4[1]-x1[1];
      vec3[2] = x4[2]-x1[2];
      cp[0] = vec3[0]*(vec1[1]*vec2[2]-vec1[2]*vec2[1]);
      cp[1] = vec3[1]*(vec1[2]*vec2[0]-vec1[0]*vec2[2]);
      cp[2] = vec3[2]*(vec1[0]*vec2[1]-vec1[1]*vec2[0]);
      volume = fabs(sqrt(sqr(cp[0]) + sqr(cp[1]) + sqr(cp[2])))/6.0;
      midarea  = volume * 3.0;
#ifdef DEBUG_VOLSURF
printf("1: %f 2: %f mid: %f\n", area1, area2, midarea);
printf("v1: %f v2: %f v3: %f v4: %f\n", v1, v2, v3, v4);
//return;
#endif
   }

//if (midarea > 3) {
//printf("1: %f 2: %f mid: %f\n", area1, area2, midarea);
//printf("v1: %f v2: %f v3: %f v4: %f\n", v1, v2, v3, v4);
//   sleep(5);
//}

   first=ceil(((nbucket-1) * (v1-min))/(max-min));

#ifdef DEBUG_VOLSURF
printf("1: %f 2: %f mid: %f\n", area1, area2, midarea);
printf("v1: %f v2: %f v3: %f v4: %f\n", v1, v2, v3, v4);
#endif

   // compute the
   for (; fx[first] < v2; first++) {
      if (v1 == v3)
        val[first] += area1;
      else {
        s = (fx[first]-v1)/(v2-v1);
        val[first] += s*s * area1;
#ifdef DEBUG_VOLSURF
printf("val[%d] (%f) += %f\n", first, fx[first], s*s*area1);
#endif
      }
   }
   for (; fx[first] < v3; first++) {
//      val[first]+=sqr(1.0-((v4-fx[first])/(float)(v4-v2))) * maxlen;

      s = (fx[first]-v2)/(float)(v3-v2);
      val[first] += (1-s)*(1-s)*area1 + s*(1-s)*midarea + s*s*area2;
#ifdef DEBUG_VOLSURF
printf("val[%d] (%f) += %f\n", first, fx[first], (1-s)*(1-s)*area1 + s*(1-s)*midarea + s*s*area2);
#endif

   }
   for (; fx[first] < v4; first++) {
      if (v4==v2)
         val[first] += area2;
      else {
         s = (fx[first]-v3)/(float)(v4-v3);
         val[first]+=(1.0-s)*(1.0-s) * area2;
#ifdef DEBUG_VOLSURF
printf("val[%d] (%f) += %f\n", first, fx[first], (1.0-s)*(1.0-s) * area2);
#endif
      }
   }
}

inline void tetVolIntegral(float *x1, float *x2, float *x3, float *x4, float v1, float v2,
                      float v3, float v4, float *fx, float *val, float *cum, int nbucket,
                      float min, float max, float scaling)
{
   float *_t;
   float mid[3], mid2[3];
   float vec1[3], vec2[3], vec3[3];
   float ival;
   float s, s2, s3;
   float area1, area2, midarea, volume;
   float cum1;
   float t;
   float cp[3];
   u_int first;

   if (v4 < v3) {
      _t = x4; x4 = x3; x3 = _t;

      t = v4; v4 = v3; v3 = t;
   }
   if (v3 < v2) {
      _t = x3; x3 = x2; x2 = _t;

      t = v3; v3 = v2; v2 = t;
   }
   if (v2 < v1) {
      _t = x2; x2 = x1; x1 = _t;

      t = v2; v2 = v1; v1 = t;
   }
   if (v4 < v3) {
      _t = x4; x4 = x3; x3 = _t;

      t = v4; v4 = v3; v3 = t;
   }
   if (v3 < v2) {
      _t = x3; x3 = x2; x2 = _t;

      t = v3; v3 = v2; v2 = t;
   }
   if (v4 < v3) {
      _t = x4; x4 = x3; x3 = _t;

      t = v4; v4 = v3; v3 = t;
   }

   // need to compute point along v[0]->v[2]

   // a cell with constant value will not contribute to the length
   if (v4 == v1)
      return;


   // compute the first area
   if (v1 != v3)
      ival = (v3-v2)/(v3-v1);
   else
      ival = 0.0;
   mid[0] = (1.0-ival)*x3[0] + (ival)*x1[0];
   mid[1] = (1.0-ival)*x3[1] + (ival)*x1[1];
   mid[2] = (1.0-ival)*x3[2] + (ival)*x1[2];
   if (v1 != v4)
      ival = (v4-v2)/(v4-v1);
   else
      ival = 0.0;
   mid2[0] = (1.0-ival)*x4[0] + (ival)*x1[0];
   mid2[1] = (1.0-ival)*x4[1] + (ival)*x1[1];
   mid2[2] = (1.0-ival)*x4[2] + (ival)*x1[2];

   vec1[0] = mid[0]-x2[0];
   vec1[1] = mid[1]-x2[1];
   vec1[2] = mid[2]-x2[2];
   vec2[0] = mid2[0]-x2[0];
   vec2[1] = mid2[1]-x2[1];
   vec2[2] = mid2[2]-x2[2];
   cp[0] = vec1[1]*vec2[2]-vec1[2]*vec2[1];
   cp[1] = vec1[2]*vec2[0]-vec1[0]*vec2[2];
   cp[2] = vec1[0]*vec2[1]-vec1[1]*vec2[0];
   area1 = 0.5 * fabs(sqrt(sqr(cp[0]) + sqr(cp[1]) + sqr(cp[2]))) * scaling;


   // compute the second area
   if (v2 != v4)
      ival = (v4-v3)/(v4-v2);
   else
      ival = 0.0;
   mid[0] = (1.0-ival)*x4[0] + (ival)*x2[0];
   mid[1] = (1.0-ival)*x4[1] + (ival)*x2[1];
   mid[2] = (1.0-ival)*x4[2] + (ival)*x2[2];
   if (v4 != v1)
      ival = (v4-v3)/(v4-v1);
   else
      ival = 0.0;
   mid2[0] = (1.0-ival)*x4[0] + (ival)*x1[0];
   mid2[1] = (1.0-ival)*x4[1] + (ival)*x1[1];
   mid2[2] = (1.0-ival)*x4[2] + (ival)*x1[2];

   vec1[0] = mid[0]-x3[0];
   vec1[1] = mid[1]-x3[1];
   vec1[2] = mid[2]-x3[2];
   vec2[0] = mid2[0]-x3[0];
   vec2[1] = mid2[1]-x3[1];
   vec2[2] = mid2[2]-x3[2];
   cp[0] = vec1[1]*vec2[2]-vec1[2]*vec2[1];
   cp[1] = vec1[2]*vec2[0]-vec1[0]*vec2[2];
   cp[2] = vec1[0]*vec2[1]-vec1[1]*vec2[0];
   area2 = 0.5 * fabs(sqrt(sqr(cp[0]) + sqr(cp[1]) + sqr(cp[2]))) * scaling;

   if (v2-v1 != 0.0)
      midarea = area1 * (1.0+(v3-v2)/(v2-v1));
   else if (v4-v3 != 0.0)
      midarea = area2 * (1.0+(v3-v2)/(v4-v3));
   else {
      // have to compute the midarea
      vec1[0] = x2[0]-x1[0];
      vec1[1] = x2[1]-x1[1];
      vec1[2] = x2[2]-x1[2];
      vec2[0] = x3[0]-x1[0];
      vec2[1] = x3[1]-x1[1];
      vec2[2] = x3[2]-x1[2];
      vec3[0] = x4[0]-x1[0];
      vec3[1] = x4[1]-x1[1];
      vec3[2] = x4[2]-x1[2];
      cp[0] = vec3[0]*(vec1[1]*vec2[2]-vec1[2]*vec2[1]);
      cp[1] = vec3[1]*(vec1[2]*vec2[0]-vec1[0]*vec2[2]);
      cp[2] = vec3[2]*(vec1[0]*vec2[1]-vec1[1]*vec2[0]);
      volume = fabs(sqrt(sqr(cp[0]) + sqr(cp[1]) + sqr(cp[2])))/6.0;
      midarea  = volume * 3.0;
#ifdef DEBUG_VOLVOL
printf("1: %f 2: %f mid: %f\n", area1, area2, midarea);
printf("v1: %f v2: %f v3: %f v4: %f\n", v1, v2, v3, v4);
//return;
#endif
   }

//if (midarea > 3) {
//printf("1: %f 2: %f mid: %f\n", area1, area2, midarea);
//printf("v1: %f v2: %f v3: %f v4: %f\n", v1, v2, v3, v4);
//   sleep(5);
//}

   first=ceil(((nbucket-1) * (v1-min))/(max-min));

#ifdef DEBUG_VOLVOL
printf("1: %f 2: %f mid: %f\n", area1, area2, midarea);
printf("v1: %f v2: %f v3: %f v4: %f\n", v1, v2, v3, v4);
#endif

   // compute the
   for (; fx[first] < v2; first++) {
      if (v1 == v3)
        val[first] += area1;
      else {
        s = (fx[first]-v1)/(v2-v1);
        val[first] += s*s*s*area1*(v2-v1)/3.0;
      }
   }

   cum1 = area1*(v2-v1)/3.0;

   for (; fx[first] < v3; first++) {
      s = (fx[first]-v2)/(float)(v3-v2);
      s2 = sqr(s);
      s3 = s2*s;
      val[first] += cum1 + (area1*(s - s2 + s3/3.0)
                          + midarea*(s2/2.0 - s3/3.0)
                          + area2*(s3/3.0)) * (v3-v2);
   }

   cum1 += (area1/3.0 + midarea/6.0 + area2/3.0) * (v3-v2);

   for (; fx[first] < v4; first++) {
      if (v4==v2)
         val[first] += area2;
      else {
         s = (fx[first]-v3)/(float)(v4-v3);
         s2 = sqr(s);
         s3 = s2*s;
         val[first] += cum1 + (area2*(s - s2 + s3/3.0)) * (v4-v3);
#ifdef DEBUG_VOLVOL
printf("val[%d] (%f) += %f\n", first, fx[first], (1.0-s)*(1.0-s) * area2);
#endif
      }
   }

   cum1 += area2/3.0 * (v4-v3);

   if (first < nbucket);
      cum[first] += cum1;
}

#endif

/* add by Fan:
signature: volume of the region where two variables are intercepted
with area ..... */
inline void intVolIntegral(float **p,
			   float *u, float *v,
			   float fx1[], float **val1,float fx2[], float **val2,
			   int nbucket,
			   float min1, float max1,
			   float min2, float max2, float scaling)
{
  
   u_int first1, first2;
   //float alph0, beta0;
   float alph, beta;
   int i,j;

   alph = 0.; beta = 0.;
   for(i=0; i<8; i++){
     alph = alph + u[i];
     beta = beta + v[i];
   }
   alph = alph/8.0;
   beta = beta/8.0;

#if 1     
   first1 = ceil(((nbucket-1) * (alph-min1))/(max1-min1));
   first2 = ceil(((nbucket-1) * (beta-min2))/(max2-min2));
   
   /* for alph > alph0 and beta > beta0 */		  
   for(i=first1; i<nbucket; i++){
     for(j=first2; j<nbucket; j++){
       val1[i][j] += 1.0;
     }
   }
   /* for alph < alph0 and beta < beta0 */		  
   for(i=first1-1; i>=0; i--){ 
     for(j=first2-1; j>=0; j--){
       val2[i][j] += 1.0;
     }
   }

#endif
   /* this part will be handeled later */
#if 0
   /* for alph > alph0 and beta < beta0 */		  

   for(i=first1; i<nbucket; i++){
     for(j=first2-1; j>=0; j--){
       val1[i][j] -= 1.0;
       val2[i][j] -= 1.0;

     }
   } 

   /* for alph < alph0 and beta > beta0 */		  
   for(i=first1; i>=0; i--){
     for(j=first2; j<nbucket; j++){
       val1[i][j] -= 1.0;
       val2[i][j] -= 1.0;

     }
   }
#endif

}
