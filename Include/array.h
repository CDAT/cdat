
#define NDS 4
#define A_TABMAX 100
#define MAXARGS 4

/*       Declare the structure for data array attributes                */

struct a_attr
	{
	int notok;	/* Indicates validity of the attributes		*/

/*		Character string attributes				*/

	char *F;	/* Filename (s<=1k)				*/

	char *f;	/* Assignment function (rhs) (s<=1k)		*/

        char *lmask;	/* Logical mask assignment (s<=1k)		*/

        char *trnf;	/* List of transforms to use (s<=1k)	*/

	char *S;	/* Actual data source description (s<= 120)	*/
	char *N;	/* Actual name of the data  (s<= 16)		*/
	char *TI;	/* Actual title for the data (s<=80)		*/
	char *U;        /* Actual units of the data (s<=40)		*/
	char *TY;	/* Actual type of data (s<=8) `R*n','I*n','C*n'	*/
	char *CRD;	/* Actual creation date of the data  (s<=8)	*/
	char *CRT;	/* Actual creation time of the data (s<=8)	*/

	char *s;	/* Substitute data source description (s<= 120)	*/
	char *n;	/* Substitute name of the data  (s<= 16)	*/
	char *ti;	/* Substitute title for the data (s<=80)	*/
	char *u;        /* Substitute units of the data (s<=40)		*/
	char *ty;	/* Substitute type of data (s<=8)		*/
	char *crd;	/* Substitute creation date of the data  (s<=8)	*/
	char *crt;	/* Substitute creation time of the data (s<=8)	*/

	char *com1;	/* Comment line, number 1 (s<=120)		*/
	char *com2;	/* Comment line, number 2 (s<=120)		*/
	char *com3;	/* Comment line, number 3 (s<=120)		*/
	char *com4;	/* Comment line, number 4 (s<=120)		*/

        char *XN[NDS];	/* Actual dimension name (s<=16)		*/
	char *XU[NDS];	/* Actual dimension units (s<=40)		*/

        char *xn[NDS];	/* Substitute dimension name (s<=16)		*/
	char *xu[NDS];	/* Substitute dimension units (s<=40)		*/

	char *aF;	/* Assign filename				*/
 
	char *af;	/* Assign assignment function (rhs)		*/
        char *almask;	/* Assign logical mask assignment		*/
        char *atrnf;	/* Assign list of transforms to use		*/

	char *aS;	/* Assign actual data source description	*/
	char *aN;	/* Assign actual name of the data		*/
	char *aTI;	/* Assign actual title for the data		*/
	char *aU;       /* Assign actual units of the data		*/
	char *aTY;	/* Assign actual type of data			*/
	char *aCRD;	/* Assign actual creation date of the data	*/
	char *aCRT;	/* Assign actual creation time of the data	*/

	char *as;	/* Assign substitute data source description	*/
	char *an;	/* Assign substitute name of the data		*/
	char *ati;	/* Assign substitute title for the data		*/
	char *au;       /* Assign substitute units of the data		*/
	char *aty;	/* Assign substitute type of data		*/
	char *acrd;	/* Assign substitute creation date of the data	*/
	char *acrt;	/* Assign substitute creation time of the data	*/

	char *acom1;	/* Assign comment line number 1			*/
	char *acom2;	/* Assign comment line number 2			*/
	char *acom3;	/* Assign comment line number 3			*/
	char *acom4;	/* Assign comment line number 4			*/

        char *aXN[NDS];	/* Assign actual dimension name			*/
	char *aXU[NDS];	/* Assign actual dimension units		*/

        char *axn[NDS];	/* Assign substitute dimension name		*/
	char *axu[NDS];	/* Assign substitute dimension units		*/

/*		Integer attributes					*/

	int *XS[NDS];	/* Actual dimension size (s=1)			*/
        int *XK[NDS];	/* Actual dimension wrap coeff. limits (s=2)	*/

	int *xs[NDS];	/* Substitute dimension size (s=1)		*/
	int *xj[NDS];	/* Dimension jump interval (s=1)		*/
	int *xi[NDS];	/* Dimension interpolation indicator (s=1)	*/

	char *aXS[NDS];	/* Assign actual dimension size			*/
	char *aXK[NDS];	/* Assign actual wrap coefficient limits	*/

	char *axs[NDS];	/* Assign substitute dimension size		*/
	char *axj[NDS];	/* Assign dimension jump interval		*/
	char *axi[NDS]; /* Assign interpolation indicator 		*/

/*		Computed integer.					*/

	int ND;		/* Number of dimensions of the variable.	*/

/*		Scalar floating point attributes			*/

	float *XC[NDS];	/* Actual dimension wrap cycle length	(s=1)	*/

	float *XF[NDS];	/* Actual dimension first value (s=1)		*/
	float *XL[NDS];	/* Actual dimension last value (s=1)		*/

	float *xf[NDS];	/* Substitute dimension first value (s=1)	*/
	float *xl[NDS];	/* Substitute dimension last value (s=1)	*/

	char *aXC[NDS];	/* Assign actual dimension wrap cycle length	*/

	char *aXF[NDS];	/* Assign actual dimension first value		*/
	char *aXL[NDS];	/* Assign actual dimension last value		*/

	char *axf[NDS];	/* Assign substitute dimension first value	*/
	char *axl[NDS];	/* Assign substitute dimension last value	*/

/*		Vector floating point attributes			*/

	float *XV[NDS];	/* Actual dimension values (s=XS)		*/
	float *XB[NDS];	/* Actual dimension grid box limits (s=XS+1)	*/
	float *XW[NDS];	/* Actual dimension area weights (s=XS)		*/

	float *xv[NDS];	/* Substitute dimension values (s=xs)		*/
	float *xb[NDS];	/* Substitute dimension grid box limits (s=xs+1)*/
	float *xw[NDS];	/* Substitute dimension area weights (s=xs)	*/

	char *aXV[NDS];	/* Assign actual dimension values		*/
	char *aXB[NDS];	/* Assign actual grid box limits		*/
	char *aXW[NDS];	/* Assign actual dimension area weights		*/

	char *axv[NDS];	/* Assign substitute dimension values		*/
	char *axb[NDS];	/* Assign substitute dimension grid box limits	*/
	char *axw[NDS];	/* Assign substitute dimension area weights	*/

/*		Attributes computed and/or filled at access time	*/

	float mean;	/* weighted mean of (non-duplicate) data values	*/
	float min;	/* minimum of data values			*/
	float max;	/* maximum of data values			*/

        short *mask;	/* logical mask array (s=xs*ys*zs*ts)		*/
/* 	/\* added by C. Doutriaux, so i don't have to copy the DATA ! *\/ */
/* 	/\* POINTER to array of dim ND describing the number of element in each dim *\/ */
/* 	int *dimensions;  */
/* 	char *data; */
/* 	int *data_strides; */
/*         char *amask; */
/* 	int *mask_strides; */
/* 	char *Numpy_data_address; */
/* 	char *Numpy_mask_address; */
/* 	int data_position_ct; /\* Offset to have the correct position in the anim *\/ */
/* 	int mask_position_ct; /\* Offset to have the correct position in the anim *\/ */
/* 	/\* END of addition by C. Doutriaux *\/ */
/* 	int type_num; */
	union
	  {
	   float *data;	/* the data array (s=xs*ys*zs*ts)		*/
	   int *idata;	/* the data array (s=xs*ys*zs*ts)		*/
	  } un;

	 };

/*  The following is a table of array attribute names and locations of
    their structure of values.						*/

	struct a_tab
	  {
	   char name[17];	/* name of the array attributes		*/
           int FROM_CDAT; /* indicate if the array was generated from CDAT*/
	   struct a_attr *pA_attr;/* pointer to array attribute structure*/
	   struct a_tab *next;	/* pointer to the next table entry	*/
	  } ;

    struct parsed
      {
       char str[19];
       int tok1;
       int tok2;
       float v;
      };

/*  Structures for assignment of array.					*/

    struct argmnt
	{
	 char name[17];
	 int ok;
	 struct a_attr *paa;
	 float v;
	 char *s;
	 struct data_flo *pdf;
	};

    struct data_flo
	{
	 struct data_flo *prev; /*  Pointer to previous structure.	*/
	 struct data_flo *next; /*  Pointer to next structure.		*/
	 struct argmnt rtrn;	 /*  Return argument for function.	*/
	 char func[17];		 /*  Function name.			*/
	 struct argmnt argm[MAXARGS];/*  Given argument(s) for function.*/
	};

    struct parslog
      {
       char tok1[3];
       char str[20];
       char tok2[3];
       float v;
      };

