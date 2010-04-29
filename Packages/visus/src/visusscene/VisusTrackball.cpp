/***********************************************************************
*
* Copyright (c) 2008, Lawrence Livermore National Security, LLC.  
* Produced at the Lawrence Livermore National Laboratory  
* Written by bremer5@llnl.gov,pascucci@sci.utah.edu.  
* LLNL-CODE-406031.  
* All rights reserved.  
*   
* This file is part of "Simple and Flexible Scene Graph Version 2.0."
* Please also read BSD_ADDITIONAL.txt.
*   
* Redistribution and use in source and binary forms, with or without
* modification, are permitted provided that the following conditions are
* met:
*   
* @ Redistributions of source code must retain the above copyright
*   notice, this list of conditions and the disclaimer below.
* @ Redistributions in binary form must reproduce the above copyright
*   notice, this list of conditions and the disclaimer (as noted below) in
*   the documentation and/or other materials provided with the
*   distribution.
* @ Neither the name of the LLNS/LLNL nor the names of its contributors
*   may be used to endorse or promote products derived from this software
*   without specific prior written permission.
*   
*  
* THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
* "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
* LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
* A PARTICULAR PURPOSE ARE DISCLAIMED.  IN NO EVENT SHALL LAWRENCE
* LIVERMORE NATIONAL SECURITY, LLC, THE U.S. DEPARTMENT OF ENERGY OR
* CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL,
* EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO,
* PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR
* PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF
* LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING
* NEGLIGENCE OR OTHERWISE) ARISING
*
***********************************************************************/


#include <stdio.h>
#include <stdlib.h>
#include <string.h>

/*
 * (c) Copyright 1993, 1994, Silicon Graphics, Inc.
 * ALL RIGHTS RESERVED
 * Permission to use, copy, modify, and distribute this software for
 * any purpose and without fee is hereby granted, provided that the above
 * copyright notice appear in all copies and that both the copyright notice
 * and this permission notice appear in supporting documentation, and that
 * the name of Silicon Graphics, Inc. not be used in advertising
 * or publicity pertaining to distribution of the software without specific,
 * written prior permission.
 *
 * THE MATERIAL EMBODIED ON THIS SOFTWARE IS PROVIDED TO YOU "AS-IS"
 * AND WITHOUT WARRANTY OF ANY KIND, EXPRESS, IMPLIED OR OTHERWISE,
 * INCLUDING WITHOUT LIMITATION, ANY WARRANTY OF MERCHANTABILITY OR
 * FITNESS FOR A PARTICULAR PURPOSE.  IN NO EVENT SHALL SILICON
 * GRAPHICS, INC.  BE LIABLE TO YOU OR ANYONE ELSE FOR ANY DIRECT,
 * SPECIAL, INCIDENTAL, INDIRECT OR CONSEQUENTIAL DAMAGES OF ANY
 * KIND, OR ANY DAMAGES WHATSOEVER, INCLUDING WITHOUT LIMITATION,
 * LOSS OF PROFIT, LOSS OF USE, SAVINGS OR REVENUE, OR THE CLAIMS OF
 * THIRD PARTIES, WHETHER OR NOT SILICON GRAPHICS, INC.  HAS BEEN
 * ADVISED OF THE POSSIBILITY OF SUCH LOSS, HOWEVER CAUSED AND ON
 * ANY THEORY OF LIABILITY, ARISING OUT OF OR IN CONNECTION WITH THE
 * POSSESSION, USE OR PERFORMANCE OF THIS SOFTWARE.
 *
 * US Government Users Restricted Rights
 * Use, duplication, or disclosure by the Government is subject to
 * restrictions set forth in FAR 52.227.19(c)(2) or subparagraph
 * (c)(1)(ii) of the Rights in Technical Data and Computer Software
 * clause at DFARS 252.227-7013 and/or in similar or successor
 * clauses in the FAR or the DOD or NASA FAR Supplement.
 * Unpublished-- rights reserved under the copyright laws of the
 * United States.  Contractor/manufacturer is Silicon Graphics,
 * Inc., 2011 N.  Shoreline Blvd., Mountain View, CA 94039-7311.
 *
 * OpenGL(TM) is a trademark of Silicon Graphics, Inc.
 */
/*
 * Trackball code:
 *
 * Implementation of a virtual trackball.
 * Implemented by Gavin Bell, lots of ideas from Thant Tessman and
 *   the August '88 issue of Siggraph's "Computer Graphics," pp. 121-129.
 *
 * Vector manip code:
 *
 * Original code from:
 * David M. Ciemiewicz, Mark Grossman, Henry Moreton, and Paul Haeberli
 *
 * Much mucking with by:
 * Gavin Bell
 */
#if defined(_WIN32)
#pragma warning (disable:4244)          /* disable bogus conversion warnings */
#endif
#include <math.h>
#include "VisusTrackball.h"

/*
 * This size should really be based on the distance from the center of
 * rotation to the point on the object underneath the mouse.  That
 * point would then track the mouse as closely as possible.  This is a
 * simple example, though, so that is left as an Exercise for the
 * Programmer.
 */
#define TRACKBALLSIZE  (0.8f)

/*
 * Local function prototypes (not defined in trackball.h)
 */
static float tb_project_to_sphere(float, float, float);
static void normalize_quat(float [4]);

void
vzero(float *v)
{
    v[0] = 0.0;
    v[1] = 0.0;
    v[2] = 0.0;
}

void
vset(float *v, float x, float y, float z)
{
    v[0] = x;
    v[1] = y;
    v[2] = z;
}

void
vsub(const float *src1, const float *src2, float *dst)
{
    dst[0] = src1[0] - src2[0];
    dst[1] = src1[1] - src2[1];
    dst[2] = src1[2] - src2[2];
}

void
vcopy(const float *v1, float *v2)
{
    register int i;
    for (i = 0 ; i < 3 ; i++)
        v2[i] = v1[i];
}

void
vcross(const float *v1, const float *v2, float *cross)
{
    float temp[3];

    temp[0] = (v1[1] * v2[2]) - (v1[2] * v2[1]);
    temp[1] = (v1[2] * v2[0]) - (v1[0] * v2[2]);
    temp[2] = (v1[0] * v2[1]) - (v1[1] * v2[0]);
    vcopy(temp, cross);
}

float
vlength(const float *v)
{
    return sqrt(v[0] * v[0] + v[1] * v[1] + v[2] * v[2]);
}

void
vscale(float *v, float div)
{
    v[0] *= div;
    v[1] *= div;
    v[2] *= div;
}

void
vnormal(float *v)
{
    vscale(v,1.0/vlength(v));
}

float
vdot(const float *v1, const float *v2)
{
    return v1[0]*v2[0] + v1[1]*v2[1] + v1[2]*v2[2];
}

void
vadd(const float *src1, const float *src2, float *dst)
{
    dst[0] = src1[0] + src2[0];
    dst[1] = src1[1] + src2[1];
    dst[2] = src1[2] + src2[2];
}

/*
 * Ok, simulate a track-ball.  Project the points onto the virtual
 * trackball, then figure out the axis of rotation, which is the cross
 * product of P1 P2 and O P1 (O is the center of the ball, 0,0,0)
 * Note:  This is a deformed trackball-- is a trackball in the center,
 * but is deformed into a hyperbolic sheet of rotation away from the
 * center.  This particular function was chosen after trying out
 * several variations.
 *
 * It is assumed that the arguments to this routine are in the range
 * (-1.0 ... 1.0)
 */
void
trackball(float q[4], float p1x, float p1y, float p2x, float p2y)
{
    float a[3]; /* Axis of rotation */
    float phi;  /* how much to rotate about axis */
    float p1[3], p2[3], d[3];
    float t;

    if (p1x == p2x && p1y == p2y) {
        /* Zero rotation */
        vzero(q);
        q[3] = 1.0;
        return;
    }

    /*
     * First, figure out z-coordinates for projection of P1 and P2 to
     * deformed sphere
     */
    vset(p1,p1x,p1y,tb_project_to_sphere(TRACKBALLSIZE,p1x,p1y));
    vset(p2,p2x,p2y,tb_project_to_sphere(TRACKBALLSIZE,p2x,p2y));

    /*
     *  Now, we want the cross product of P1 and P2
     */
    vcross(p2,p1,a);

    /*
     *  Figure out how much to rotate around that axis.
     */
    vsub(p1,p2,d);
    t = vlength(d) / (2.0*TRACKBALLSIZE);

    /*
     * Avoid problems with out-of-control values...
     */
    if (t > 1.0) t = 1.0;
    if (t < -1.0) t = -1.0;
    phi = 2.0 * asin(t);

    axis_to_quat(a,phi,q);
}

/*
 *  Given an axis and angle, compute quaternion.
 */
void
axis_to_quat(float a[3], float phi, float q[4])
{
    vnormal(a);
    vcopy(a,q);
    vscale(q,sin(phi/2.0));
    q[3] = cos(phi/2.0);
}

/*
 * Multiply two quaternions (can be done in place).
 */
void
quat_mult(float q1[4], float q2[4], float dest[4])
{
    float tmp[4];
    tmp[0] = q1[3]*q2[0] + q1[0]*q2[3] + q1[1]*q2[2] - q1[2]*q2[1];
    tmp[1] = q1[3]*q2[1] - q1[0]*q2[2] + q1[1]*q2[3] + q1[2]*q2[0];
    tmp[2] = q1[3]*q2[2] + q1[0]*q2[1] - q1[1]*q2[0] + q1[2]*q2[3];
    tmp[3] = q1[3]*q2[3] - q1[0]*q2[0] - q1[1]*q2[1] - q1[2]*q2[2];
    vcopy(tmp,dest);
}

/*
 * Project an x,y pair onto a sphere of radius r OR a hyperbolic sheet
 * if we are away from the center of the sphere.
 */
static float
tb_project_to_sphere(float r, float x, float y)
{
    float d, t, z;

    d = sqrt(x*x + y*y);
    if (d < r * 0.70710678118654752440) {    /* Inside sphere */
        z = sqrt(r*r - d*d);
    } else {           /* On hyperbola */
        t = r / 1.41421356237309504880;
        z = t*t / d;
    }
    return z;
}

/*
 * Given two rotations, e1 and e2, expressed as quaternion rotations,
 * figure out the equivalent single rotation and stuff it into dest.
 *
 * This routine also normalizes the result every RENORMCOUNT times it is
 * called, to keep error from creeping in.
 *
 * NOTE: This routine is written so that q1 or q2 may be the same
 * as dest (or each other).
 */

#define RENORMCOUNT 97

void
add_quats(float q1[4], float q2[4], float dest[4])
{
    static int count=0;
    float t1[4], t2[4], t3[4];
    float tf[4];

#if 0
printf("q1 = %f %f %f %f\n", q1[0], q1[1], q1[2], q1[3]);
printf("q2 = %f %f %f %f\n", q2[0], q2[1], q2[2], q2[3]);
#endif

    vcopy(q1,t1);
    vscale(t1,q2[3]);

    vcopy(q2,t2);
    vscale(t2,q1[3]);

    vcross(q2,q1,t3);
    vadd(t1,t2,tf);
    vadd(t3,tf,tf);
    tf[3] = q1[3] * q2[3] - vdot(q1,q2);

#if 0
printf("tf = %f %f %f %f\n", tf[0], tf[1], tf[2], tf[3]);
#endif

    dest[0] = tf[0];
    dest[1] = tf[1];
    dest[2] = tf[2];
    dest[3] = tf[3];

    if (++count > RENORMCOUNT) {
        count = 0;
        normalize_quat(dest);
    }
}

/*
 * Quaternions always obey:  a^2 + b^2 + c^2 + d^2 = 1.0
 * If they don't add up to 1.0, dividing by their magnitued will
 * renormalize them.
 *
 * Note: See the following for more information on quaternions:
 *
 * - Shoemake, K., Animating rotation with quaternion curves, Computer
 *   Graphics 19, No 3 (Proc. SIGGRAPH'85), 245-254, 1985.
 * - Pletinckx, D., Quaternion calculus as a basic tool in computer
 *   graphics, The Visual Computer 5, 2-13, 1989.
 */
static void
normalize_quat(float q[4])
{
    int i;
    float mag;

    mag = sqrt(q[0]*q[0] + q[1]*q[1] + q[2]*q[2] + q[3]*q[3]);
    for (i = 0; i < 4; i++) q[i] /= mag;
}

/*
 * Build a rotation matrix, given a quaternion rotation.
 *
 */
void
build_rotmatrix(float m[4][4], float q[4])
{
    m[0][0] = 1.0 - 2.0 * (q[1] * q[1] + q[2] * q[2]);
    m[0][1] = 2.0 * (q[0] * q[1] - q[2] * q[3]);
    m[0][2] = 2.0 * (q[2] * q[0] + q[1] * q[3]);
    m[0][3] = 0.0;

    m[1][0] = 2.0 * (q[0] * q[1] + q[2] * q[3]);
    m[1][1]= 1.0 - 2.0 * (q[2] * q[2] + q[0] * q[0]);
    m[1][2] = 2.0 * (q[1] * q[2] - q[0] * q[3]);
    m[1][3] = 0.0;

    m[2][0] = 2.0 * (q[2] * q[0] - q[1] * q[3]);
    m[2][1] = 2.0 * (q[1] * q[2] + q[0] * q[3]);
    m[2][2] = 1.0 - 2.0 * (q[1] * q[1] + q[0] * q[0]);
    m[2][3] = 0.0;

    m[3][0] = 0.0;
    m[3][1] = 0.0;
    m[3][2] = 0.0;
    m[3][3] = 1.0;
}

void matxmat(float out[4][4],float m0[4][4],float m1[4][4])
{
    float	r[4][4];
    int		i,j,k;

    for(i=0;i<4;i++) {
        for(j=0;j<4;j++) {
            r[i][j] = 0;
	    for(k=0;k<4;k++) {
	        r[i][j] += m0[i][k]*m1[k][j];
            }
        }
    }
    matcopy(out,r);
    return;
}

void matident(float m[4][4])
{
	int 	i,j;
	for(i=0;i<4;i++) {
		for(j=0;j<4;j++) {
			m[i][j] = (i == j)?1:0;
		}
	}
	return;
}

void matmult(float *in,float *out,float mat[4][4])
{
        float tmp[3];
        tmp[0] = (in[0]*mat[0][0] + in[1]*mat[0][1] +
                 in[2]*mat[0][2]);
        tmp[1] = (in[0]*mat[1][0] + in[1]*mat[1][1] +
                 in[2]*mat[1][2]);
        tmp[2] = (in[0]*mat[2][0] + in[1]*mat[2][1] +
                 in[2]*mat[2][2]);
	vcopy(tmp,out);
	return;
}

void matinvert(float A[4][4])
{
/*
 *         a b c a b
 *         d e f d e
 *         g h i g h
 *         a b c a b
 *         d e f d e
 */
        double  a,b,c,d,e,f,g,h,i;
        double  Da,Db,Dc,Dd,De,Df,Dg,Dh,Di;
        double  detA;
        long int        k,j;

/* assign temp vars */
        a = A[0][0];
        b = A[0][1];
        c = A[0][2];
        d = A[1][0];
        e = A[1][1];
        f = A[1][2];
        g = A[2][0];
        h = A[2][1];
        i = A[2][2];

/* calc determinat of 3x3 A */
	detA = (a * e * i) + (d * h * c) + (b * f * g);
        detA = detA - (c * e * g) - (a * f * h) - (b * d * i);

/* check for singular matrix */
        if (detA == 0.0) {
                fprintf(stderr,"Warning, singular matrix detected.\n");
	        return;
	 }

/* calc sub (2x2) determinats if Dx row and column is eliminated */
/* and mult by (-1)^i+j */
         Da =  ((e * i) - (f * h));
	 Db = ((f * g) - (d * i));
	 Dc =  ((d * h) - (g * e));
	 Dd = ((h * c) - (b * i));
	 De =  ((i * a) - (c * g));
	 Df = ((g * b) - (a * h));
	 Dg =  ((b * f) - (e * c));
	 Dh = ((c * d) - (a * f));
	 Di =  ((a * e) - (b * d));

/* build the transpose matrix from the sub determinats */
	 A[0][0] = Da;
	 A[1][0] = Db;
         A[2][0] = Dc;
	 A[0][1] = Dd;
	 A[1][1] = De;
	 A[2][1] = Df;
	 A[0][2] = Dg;
	 A[1][2] = Dh;
	 A[2][2] = Di;

/* devide by detA */
	 for (k=0; k<3; k++) {
	 	for (j=0; j<3; j++) A[k][j] = A[k][j]/detA;
	 }
	 return;
}

void matcopy(float a[4][4],float b[4][4])
{
	memcpy(a,b,16*sizeof(float));
}

void mattrans(float a[4][4])
{
	float	r[4][4];
	int	i,j;
	for(i=0;i<4;i++) {
		for(j=0;j<4;j++) {
			r[i][j] = a[j][i];
		}
	}
	matcopy(a,r);
}


void matprint(float a[4][4],char *s)
{
    int i;

    if (s) printf("Matrix:%s\n",s);
    for(i=0;i<4;i++) {
	printf("%6.3f %6.3f %6.3f %6.3f\n",a[0][i],a[1][i],a[2][i],a[3][i]);
    }
}

void matinv4x4(float A[4][4])
{
	float	r[4][4];
	int	i,j;

	float det = matadjoint(r,A);

	if (det == 0.0) return;

	for(i=0;i<4;i++) {
		for(j=0;j<4;j++) {
			r[i][j] /= det;
		}
	}

	matcopy(A,r);

	return;
}

static float det2x2( float a1, float b1,
                float a2, float b2 )
{
  return (a1*b2 - b1*a2);
}

static float det3x3( float a1, float b1, float c1,
                float a2, float b2, float c2,
                float a3, float b3, float c3 )
{
    return (   a1*det2x2(b2,c2,
                         b3,c3)
             - b1*det2x2(a2,c2,
                         a3,c3)
             + c1*det2x2(a2,b2,
                         a3,b3) );
}

float matadjoint(float r[4][4],float _data[4][4])
{
    float a1=_data[0][0],b1=_data[0][1],c1=_data[0][2],d1=_data[0][3],
          a2=_data[1][0],b2=_data[1][1],c2=_data[1][2],d2=_data[1][3],
          a3=_data[2][0],b3=_data[2][1],c3=_data[2][2],d3=_data[2][3],
          a4=_data[3][0],b4=_data[3][1],c4=_data[3][2],d4=_data[3][3];

    r[0][0] =  det3x3(b2,c2,d2, b3,c3,d3, b4,c4,d4);
    r[1][0] = -det3x3(a2,c2,d2, a3,c3,d3, a4,c4,d4);
    r[2][0] =  det3x3(a2,b2,d2, a3,b3,d3, a4,b4,d4);
    r[3][0] = -det3x3(a2,b2,c2, a3,b3,c3, a4,b4,c4);

    r[0][1] = -det3x3(b1,c1,d1, b3,c3,d3, b4,c4,d4);
    r[1][1] =  det3x3(a1,c1,d1, a3,c3,d3, a4,c4,d4);
    r[2][1] = -det3x3(a1,b1,d1, a3,b3,d3, a4,b4,d4);
    r[3][1] =  det3x3(a1,b1,c1, a3,b3,c3, a4,b4,c4);

    r[0][2] =  det3x3(b1,c1,d1, b2,c2,d2, b4,c4,d4);
    r[1][2] = -det3x3(a1,c1,d1, a2,c2,d2, a4,c4,d4);
    r[2][2] =  det3x3(a1,b1,d1, a2,b2,d2, a4,b4,d4);
    r[3][2] = -det3x3(a1,b1,c1, a2,b2,c2, a4,b4,c4);

    r[0][3] = -det3x3(b1,c1,d1, b2,c2,d2, b3,c3,d3);
    r[1][3] =  det3x3(a1,c1,d1, a2,c2,d2, a3,c3,d3);
    r[2][3] = -det3x3(a1,b1,d1, a2,b2,d2, a3,b3,d3);
    r[3][3] =  det3x3(a1,b1,c1, a2,b2,c2, a3,b3,c3);

    float outDet =   a1 * r[0][0]
                   + b1 * r[1][0]
                   + c1 * r[2][0]
                   + d1 * r[3][0];

    return(outDet);
}


