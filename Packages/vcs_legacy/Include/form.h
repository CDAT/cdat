
    struct form
      {
       char s_name[17];	 /* Name - must appear in the format request.	*/
       char s_units[41]; /* Units - must appear in the format request.	*/
       char format[121]; /* Format - describes formatting for value.	*/
       struct form *next;
      };
    struct table_form
      {
       char name[17];		/* Name of the assemblage of formats.	*/
       struct form *variety;	/* Points to the assemblage of formats.	*/
       struct table_form *next;
      };

/*		The format should contain:

		%n - to display the name of the value being formatted.
		%u - to display the units of the value being formatted.
		%g - to format the value, or
		%t - to format time as defined in a following set of
		     bracketed units definitions, as [h:m:s] or [d/M/y]
		     or perhaps [d/M/c+1979].

		Where:	1. s - second
			2. m - minute
			3. h - hour
			4. day - day
			5. M - month number
			   mon - three character month designator (no capital)
			   Mon - three character month designator (1st capital)
			   MON - three character month designator (all capital)
			   month - full month name (no capital)
			   Month - full month name (1st capital)
			   MONTH - full month name (all capital)
			6. S - three character season designator
			       (i.e. 1 - DJF, 2 - MAM, 3 - JJA, 4 - SON)
			   Sea - three character/month season designator
			         (i.e. Dec-Jan-Feb)
			   SEA - three character season designator in caps
			         (i.e. DEC-JAN-FEB)
			   season - three month of the season in full
				    (i.e. december-january-february)
			   Season - three months of the season capitalized in
				    full (i.e. December-January-February)
			   SEASON - three month of the season in full caps
				    (i.e. DECEMBER-JANUARY-FEBRUARY)
			7. y - year last two digits (i.e. no century)
			   c - year all four digits

		The %t and bracketed format designators should only
		be used when the variable's name includes "time" and
		units are "month" or "hour" etc..
									*/
