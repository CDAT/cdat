#define PRJ_TYPES 4	/* Total number of projection types modelled.	*/
#include "vcs_legacy_names_length.h"
	/*	Projection parameters are defined in the following structures.	*/

	struct projection_attr
	{
	char name[VCS_MX_NM_LEN];	/* Active projection name.		*/
	
	
	/*                                 GCTP stuff                           */
	long proj_type;
	double parm[15];      /* parameters for projection gctp code  */
	
	struct projection_attr *next;
	};


	struct project_attr
	{
	char name[VCS_MX_NM_LEN];	/* Active projection name.		*/
	int projno;		/* Projection number.			*/
	
	/*		Display space in Normalized Display Coordinates.	*/
	float x1_NDC;	/* x coordinate of lower left corner.	*/
	float y1_NDC;	/* y coordinate of lower left corner.	*/
	float x2_NDC;	/* x coordinate of upper right corner.	*/
	float y2_NDC;	/* y coordinate of upper right corner.	*/
	
	/*		Coordinates in user space for linear projection.	*/
	
	float X1;		/* x coordinate of lower left corner.	*/
	float Y1;		/* y coordinate of lower left corner.	*/
	float X2;		/* x coordinate of upper right corner.	*/
	float Y2;		/* y coordinate of upper right corner.	*/
	
	
/*                                 GCTP stuff   */
	long proj_type;                        
	double parm[15];      /* parameters for projection gctp code  */
	
	/*                              Scaling stuff                           */
	float cX, sX, cY, sY;
	
	struct project_attr *next;
	};
	
