
/* Gmktype - additional VCS marker types */
/* The other Gmktype - marker types (listed below) are defined 
   in the gks.h file.
#define GMK_POINT       1
#define GMK_PLUS        2
#define GMK_STAR        3
#define GMK_O           4
#define GMK_X           5
*/

#define GMK_DIAMOND    		 6
#define GMK_TRIANGLEUP  	 7
#define GMK_TRIANGLEDOWN	 8
#define GMK_TRIANGLELEFT	 9
#define GMK_TRIANGLERIGHT	10
#define GMK_SQUARE     		11
#define GMK_DIAMOND_FILL	12
#define GMK_TRIANGLEUP_FILL  	13
#define GMK_TRIANGLEDOWN_FILL	14
#define GMK_TRIANGLELEFT_FILL	15
#define GMK_TRIANGLERIGHT_FILL	16
#define GMK_SQUARE_FILL		17
#define GMK_HURRICANE	        18
#define GMK_NONE		19


/*		A structure for VCS marker attributes.	*/
/*		Values are initialized in main.				*/

  struct vcs_marker
    {
     int type;			/* The marker type.			*/
     float size;		/* The size of the marker.		*/
     int colour;		/* The marker color.			*/
    };
