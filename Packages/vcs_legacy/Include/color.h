/*  The following is a table of color maps and their structure.	*/

	struct c_val
	  {
	   float red;	/* percent red to be used.		*/
	   float green;	/* percent green to be used.		*/
	   float blue;	/* percent blue to be used.		*/
	  };
	struct color_table
	  {
	   char name[17];	/* name of the color table	*/
	   struct c_val cval[240];/* the color table		*/
	   struct color_table *next; 	/* pointer - next color table	*/
	  };

