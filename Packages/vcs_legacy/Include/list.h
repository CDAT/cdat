/*  The following is a table of list names and locations of
    their structure of values.						*/

	struct l_val
	  {
	   float data;		/* floating point data part of the list	*/
	   char *str;		/* pointer to string part of the list	*/
	   struct l_val *next;	/* pointer to next entry		*/
	  };
	struct l_tab
	  {
	   char name[17];	/* name of the list of values		*/
	   int count;		/* count of the number of list entries	*/
	   struct l_val *val;	/* pointer to the first list entry	*/
	   struct l_tab *next;  /* pointer to the next table element	*/
	  };

