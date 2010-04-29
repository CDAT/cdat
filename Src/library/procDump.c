#include "gks.h"
#include "gksshort.h"
#ifdef USEX11
#include <X11/Xlib.h>
#include <X11/Xutil.h>
#endif
#define STRMAX 256
#include <stdio.h>
#include <ctype.h>
#include <string.h>
#include <math.h>
#include "array.h"
#include "list.h"
#include "picture.h"
#include "color.h"
#include "graph.h"
#include "display.h"
#include "workstations.h"
#include "project.h"

    extern FILE *fpin,*fpout,*fperr;

    extern struct a_tab A_tab;
    extern struct l_tab L_tab[2];
    extern struct color_table C_tab;
    extern struct p_tab Pic_tab_dud;
    extern struct gi_tab Gi_tab;
    extern struct go_tab Go_tab;
    extern struct gfi_tab Gfi_tab;
    extern struct gfm_tab Gfm_tab;
    extern struct gfo_tab Gfo_tab;
    extern struct gfb_tab Gfb_tab;
    extern struct gv_tab Gv_tab;
    extern struct gXy_tab GXy_tab;
    extern struct gYx_tab GYx_tab;
    extern struct gXY_tab GXY_tab;
    extern struct gSp_tab GSp_tab;
    extern struct gcon_tab Gcon_tab;
    extern struct display_tab D_tab;
    extern struct table_text Tt_tab;
    extern struct table_chorn To_tab4;
    extern struct table_line Tl_tab;
    extern struct table_fill Tf_tab;
    extern struct table_mark Tm_tab;
    extern struct table_form Th_tab;
    extern struct table_pattern Pat_tab;
    extern struct displays d_type[NTYPES];

    extern struct workstations Wkst[];
    extern struct orientation Page;

    extern char active_colors[];

    extern char cgm_file[1024];

    extern struct projection_attr p_PRJ_list;


/*		Process a Dump command.					*/

    int procDump(str,tok)

      char str[];
      int *tok;

      {
	int tokm;
	int j,c;

	char strm[1024];

	FILE *fp;

	fp=NULL;

	if (*tok != '(')
	  {
	   err_warn(1,fperr,
		"Error - not a proper token (%s%c).\n",str,*tok);
	   return 0;
	  }
	c=getsttk(strm,&tokm);
	if (tokm == ')')
	  {
	   if (c > 0)
	     {
	      if ( (j=strlen(strm)) < 4 || strcmp(".scr",&strm[j-4]) != 0)
		{
		 strcat(strm,".scr");
		}
	      if ((fp=fopen(strm,"w")) == NULL)
	        {
		 err_warn(1,fperr,
			"Error - opening file (%s) - no dump made.\n",strm);
		 return 0;
		}
	     }
	   else
	     {
	      err_warn(1,fperr,
		"Error - no Dump file given - no dump made.\n");
	      return 0;
	     }
	   if (dump(fp) == 0) return 0;
	  } 
	else
	  {
	   err_warn(1,fperr,
	      "Error - not a proper token (%s%c%s%c) - no dump made.\n",
			str,*tok,strm,tokm);
	   return 0;
	  }

	return 1;
      }



/*		Dump attribute sets to a file.				*/

    int dump (fp)
      FILE *fp;
      {
       if (fp == NULL)
	 {
	  err_warn(1,fperr,"Error - Dump command with no file name.\n");
	  return 0;
	 }

       dmpInd(fp);
       dmpHints(fp);
       dmpControl(fp);
       dmpOvly(fp);
       dmpPage(fp);
       dmpWks(fp);
       dmpL_(fp);
       dmpTt_attr(fp);
       dmpTo_attr(fp);
       dmpTl_attr(fp);
       dmpTf_attr(fp);
       dmpTm_attr(fp);
       dmpPat_attr(fp);
       dmpTh_attr(fp);
       dmpC_attr(fp);
       dmpColor(fp);
       dmpP_attr(fp);
       dmpGi_attr(fp);
       dmpGo_attr(fp);
       dmpGcon_attr(fp);
       dmpGfi_attr(fp);
       dmpGfo_attr(fp);
       dmpGfb_attr(fp);
       dmpGv_attr(fp);
       dmpGXy_attr(fp);
       dmpGYx_attr(fp);
       dmpGXY_attr(fp);
       dmpGSp_attr(fp);
       dmpA_attr(fp);
       dmp_display(fp);
       return 1;
      }

/*		Replace initial.attributes file.			*/

    int replace_init (fp)
      FILE *fp;
      {
       if (fp == NULL)
	 {
	  err_warn(1,fperr,"Error - Dump command with no file name.\n");
	  return 0;
	 }

       dmpInd(fp);
       dmpHints(fp);
       dmpControl(fp);
       dmpOvly(fp);
       dmpPage(fp);
       dmpL_(fp);
       dmpTt_attr(fp);
       dmpTo_attr(fp);
       dmpTl_attr(fp);
       dmpTf_attr(fp);
       dmpTm_attr(fp);
       dmpPat_attr(fp);
       dmpTh_attr(fp);
       dmpC_attr(fp);
       dmpColor(fp);
       dmpP_attr(fp);
       dmpGi_attr(fp);
       dmpGo_attr(fp);
       dmpGcon_attr(fp);
       dmpGfi_attr(fp);
       dmpGfm_attr(fp);
       dmpGfo_attr(fp);
       dmpGfb_attr(fp);
       dmpGv_attr(fp);
       dmpGXy_attr(fp);
       dmpGYx_attr(fp);
       dmpGXY_attr(fp);
       dmpGSp_attr(fp);
       dmpProj_attr(fp);
       return 1;
      }

/*		Print the indices.					*/

    int dmpInd(fp)

      FILE *fp;

      {
	prtInd(fp);
	return 1;
      }

/*		Print the Hints switch.					*/

    int dmpHints(fp)

      FILE *fp;

      {
	prtHints(fp);
	return 1;
      }


/*		Print the control_panel switch.				*/

    int dmpControl(fp)

      FILE *fp;

      {
	prtControl(fp);
	return 1;
      }


/*		Print the Continents Overlay.				*/

    int dmpOvly(fp)
      FILE *fp;

      {
	prtOvly(fp);
	return 1;
      }


/*		Print the page orientation.				*/

    int dmpPage(fp)

      FILE *fp;

      {
	prtPage(fp);
	return 1;
      }


/*		Print Canvas command if the
		workstation is defined.					*/

    int dmpWks(fp)

      FILE *fp;

      {
	int i;
	int cvid;
        Gintlist wsid;
        struct workstations *w;
	int *pws;

	cvid=0;
	w=Wkst;
	while (w->id != 0)
	  {
	   if (cmpncs(w->type,"X_WIN") == 0) cvid=w->id;
	   w++;
	  }
	if (cvid != 0)
	  {
           wsid.number = 0;
           wsid.integers = NULL;
           gqopwk(&wsid);
	   for (i=0,pws=wsid.integers;i<wsid.number;pws++,i++)
	     {
	      if (*pws == cvid)
	        {
	         fprintf (fp,"Canvas(open)\n");
	        }
	     }
	   if (wsid.number > 0 && wsid.integers != NULL)
	       free((char *)wsid.integers);
	  }
	return 1;
      }


/*		Print the defined lists.				*/

    int dmpL_(fp)

      FILE *fp;

      {

        struct l_tab *ptab;

	for (ptab=L_tab[1].next; ptab != NULL; ptab=ptab->next)
		prtL(fp,ptab);
	return 1;
      }

    int dump_single_list(fp,attribute_name)

      FILE *fp;
      char *attribute_name;

      {

        struct l_tab *ptab;

        for (ptab=L_tab[1].next; (ptab != NULL) && 
            (strcmp(ptab->name, attribute_name) != 0); ptab=ptab->next) {}

	if (ptab != NULL)
           prtL(fp,ptab);
        return 1;
      }


/*		Print the defined array attributes.			*/

    int dmpA_attr(fp)

      FILE *fp;

      {
        struct a_tab *ptab;

	for (ptab=&A_tab; ptab != NULL; ptab=ptab->next)
	   prtA(fp,ptab);
	
        return 1;
      }

    int dump_single_array(fp,attribute_name)

      FILE *fp;
      char *attribute_name;

      {

        struct a_tab *ptab;
	void save_data_secondary_elements();

        for (ptab=&A_tab; (ptab != NULL) &&
            (strcmp(ptab->name, attribute_name) != 0); ptab=ptab->next) {}

	if (ptab != NULL) {
           prtA(fp,ptab);
	   save_data_secondary_elements(fp,ptab);
	}
        return 1;
      }

/*	Print the picture element attributes that are defined.		*/

    int dmpP_attr(fp)

      FILE *fp;

      {
        struct p_tab *ptab;

	for (ptab=Pic_tab_dud.next; ptab != NULL; ptab=ptab->next)
        /* Changed this so that only templates that do not start with
         * a '.' will be saved. -Jen =) */
        if (ptab->name[0] != '.')
	        prtP(fp,ptab);
        return 1;
      }

    int dump_single_template(fp,attribute_name)

      FILE *fp;
      char *attribute_name;

      {

        struct p_tab *ptab;
	void save_secondary_elements();

        for (ptab=Pic_tab_dud.next; (ptab != NULL) &&
            (strcmp(ptab->name, attribute_name) != 0); ptab=ptab->next) {}

	if (ptab != NULL) {
	   prtP(fp,ptab);
	   save_secondary_elements(fp,ptab);
	}
        return 1;
      }

/*		Print the text bundle attributes that are defined.	*/

    int dmpTt_attr(fp)

      FILE *fp;

      {
        struct table_text *ptab;

	for (ptab=Tt_tab.next; ptab != NULL; ptab=ptab->next)
		prtTt(fp,ptab);
	return 1;
      }

    int dump_single_textt(fp,attribute_name)

      FILE *fp;
      char *attribute_name;

      {

        struct table_text *ptab;

        for (ptab=Tt_tab.next; (ptab != NULL) &&   
            (strcmp(ptab->name, attribute_name) != 0); ptab=ptab->next) {}

	if (ptab != NULL)
           prtTt(fp,ptab);
        return 1;
      }

/*	Print the text orientation attributes that are defined.		*/

    int dmpTo_attr(fp)

      FILE *fp;

      {
        struct table_chorn *ptab;

	for (ptab=To_tab4.next; ptab != NULL; ptab=ptab->next)
		prtTo(fp,ptab);
        return 1;
      }

    int dump_single_texto(fp,attribute_name)

      FILE *fp;
      char *attribute_name;

      {

        struct table_chorn *ptab;

        for (ptab=To_tab4.next; (ptab != NULL) &&   
            (strcmp(ptab->name, attribute_name) != 0); ptab=ptab->next) {}

	if (ptab != NULL)
           prtTo(fp,ptab);
        return 1;
      }

/*	Print the line bundle attributes that are defined.		*/

    int dmpTl_attr(fp)

      FILE *fp;

      {
        struct table_line *ptab;

	for (ptab=Tl_tab.next; ptab != NULL; ptab=ptab->next)
		prtTl(fp,ptab);
        return 1;
      }

    int dump_single_line(fp,attribute_name)

      FILE *fp;
      char *attribute_name;

      {

        struct table_line *ptab;

        for (ptab=Tl_tab.next; (ptab != NULL) && 
            (strcmp(ptab->name, attribute_name) != 0); ptab=ptab->next) {}

	if (ptab != NULL)
           prtTl(fp,ptab);
        return 1;
      }

/*	Print the fill area bundle attributes that are defined.		*/

    int dmpTf_attr(fp)

      FILE *fp;

      {
        struct table_fill *ptab;

	for (ptab=Tf_tab.next; ptab != NULL; ptab=ptab->next)
		prtTf(fp,ptab);
        return 1;
      }

    int dump_single_fillarea(fp,attribute_name)

      FILE *fp;
      char *attribute_name;

      {

        struct table_fill *ptab;

        for (ptab=Tf_tab.next; (ptab != NULL) &&   
            (strcmp(ptab->name, attribute_name) != 0); ptab=ptab->next) {}

	if (ptab != NULL)
	  {
	    prtTf(fp,ptab);
	  }
        return 1;
      }

/*	Print the marker bundle attributes that are defined.		*/

    int dmpTm_attr(fp)

      FILE *fp;

      {
        struct table_mark *ptab;

	for (ptab=Tm_tab.next; ptab != NULL; ptab=ptab->next)
		prtTm(fp,ptab);
        return 1;
      }

    int dump_single_marker(fp,attribute_name)

      FILE *fp;
      char *attribute_name;

      {

        struct table_mark *ptab;

        for (ptab=Tm_tab.next; (ptab != NULL) &&   
            (strcmp(ptab->name, attribute_name) != 0); ptab=ptab->next) {}

	if (ptab != NULL)
           prtTm(fp,ptab);
        return 1;
      }

/*	Print the pattern bundle attributes that are defined.		*/

    int dmpPat_attr(fp)

      FILE *fp;

      {
        struct table_pattern *ptab;

	for (ptab=Pat_tab.next;ptab != NULL; ptab=ptab->next)
		prtPat(fp,ptab);
        return 1;
      }
/*	Print the format attributes that are defined.			*/

    int dmpTh_attr(fp)

      FILE *fp;

      {
        struct table_form *ptab;

	for (ptab=Th_tab.next;ptab != NULL; ptab=ptab->next)
		prtTh(fp,ptab);
        return 1;
      }

    int dump_single_format(fp,attribute_name)

      FILE *fp;
      char *attribute_name;

      {

        struct table_form *ptab;

        for (ptab=Th_tab.next; (ptab != NULL) &&   
            (strcmp(ptab->name, attribute_name) != 0); ptab=ptab->next) {}

	if (ptab != NULL)
           prtTh(fp,ptab);
        return 1;
      }


/*	Print the colormaps that are defined.				*/

    int dmpC_attr(fp)

      FILE *fp;

      {
        struct color_table *ptab;

	for (ptab=C_tab.next;ptab != NULL; ptab=ptab->next)
		prtC(fp,ptab);
	return 1;
      }

    int dump_single_colormap(fp,attribute_name)

      FILE *fp;
      char *attribute_name;

      {

        struct color_table *ptab;

        for (ptab=C_tab.next; (ptab != NULL) &&
            (strcmp(ptab->name, attribute_name) != 0); ptab=ptab->next) {}

	if (ptab != NULL)
           prtC(fp,ptab);
        return 1;
      }

/*			Dump the active colormap name.			*/
    int dmpColor(fp)

      FILE *fp;
      {
	prtColor(fp);
	return 1;
      }

/*		Print the isoline graph attributes that are defined.	*/

    int dmpGi_attr(fp)

      FILE *fp;

      {
        struct gi_tab *ptab;

	for (ptab=Gi_tab.next; ptab != NULL; ptab=ptab->next)
		prtGi(fp,ptab);
	return 1;
      }

    int dump_single_isoline(fp,attribute_name)

      FILE *fp;
      char *attribute_name;

      {
        struct gi_tab *ptab;
	void save_isoline_secondary_elements();

        for (ptab=Gi_tab.next; (ptab != NULL) &&
            (strcmp(ptab->name, attribute_name) != 0); ptab=ptab->next) {}

	if (ptab != NULL) {
           prtGi(fp,ptab);
	   save_isoline_secondary_elements(fp,ptab);
	}
        return 1;
      }

/*		Print the outline graph attributes that are defined.	*/

    int dmpGo_attr(fp)

      FILE *fp;

      {
        struct go_tab *ptab;

	for (ptab=Go_tab.next; ptab != NULL; ptab=ptab->next)
		prtGo(fp,ptab);
	return 1;
      }

    int dump_single_outline(fp,attribute_name)

      FILE *fp;
      char *attribute_name;

      {
        struct go_tab *ptab;
	void save_outline_secondary_elements();

        for (ptab=Go_tab.next; (ptab != NULL) &&
            (strcmp(ptab->name, attribute_name) != 0); ptab=ptab->next) {}

	if (ptab != NULL) {
           prtGo(fp,ptab);
	   save_outline_secondary_elements(fp,ptab);
	}
        return 1;
      }

/*		Print the continents attributes that are defined.	*/

    int dmpGcon_attr(fp)

      FILE *fp;

      {
        struct gcon_tab *ptab;

	for (ptab=Gcon_tab.next; ptab != NULL; ptab=ptab->next)
		prtGcon(fp,ptab);
	return 1;
      }

    int dump_single_continents(fp,attribute_name)

      FILE *fp;
      char *attribute_name;

      {
        struct gcon_tab *ptab;
        void save_continents_secondary_elements();

        for (ptab=Gcon_tab.next; (ptab != NULL) &&
            (strcmp(ptab->name, attribute_name) != 0); ptab=ptab->next) {}

	if (ptab != NULL) {
           prtGcon(fp,ptab);
           save_continents_secondary_elements(fp,ptab);
	}
        return 1;
      }

/*		Print the isofill graph attributes that are defined.	*/

    int dmpGfi_attr(fp)

      FILE *fp;

      {
        struct gfi_tab *ptab;

	for (ptab=Gfi_tab.next; ptab != NULL; ptab=ptab->next)
		prtGfi(fp,ptab);
	return 1;
      }

    int dump_single_isofill(fp,attribute_name)

      FILE *fp;
      char *attribute_name;

      {
        struct gfi_tab *ptab;
        void save_isofill_secondary_elements();

        for (ptab=Gfi_tab.next; (ptab != NULL) &&
            (strcmp(ptab->name, attribute_name) != 0); ptab=ptab->next) {}

	if (ptab != NULL) {
           prtGfi(fp,ptab);
           save_isofill_secondary_elements(fp,ptab);
	}
        return 1;
      }

/*		Print the meshfill graph attributes that are defined.	*/

    int dmpGfm_attr(fp)

      FILE *fp;

      {
        struct gfm_tab *ptab;

	for (ptab=Gfm_tab.next; ptab != NULL; ptab=ptab->next)
		prtGfm(fp,ptab);
	return 1;
      }

    int dump_single_meshfill(fp,attribute_name)

      FILE *fp;
      char *attribute_name;

      {
        struct gfm_tab *ptab;
        void save_meshfill_secondary_elements();

        for (ptab=Gfm_tab.next; (ptab != NULL) &&
            (strcmp(ptab->name, attribute_name) != 0); ptab=ptab->next) {}

	if (ptab != NULL) {
           prtGfm(fp,ptab);
           save_meshfill_secondary_elements(fp,ptab);
	}
        return 1;
      }

/*		Print the projection attributes that are defined.	*/

    int dmpProj_attr(fp)

      FILE *fp;

      {
        struct projection_attr *ptab;
	int i;

	for (i=0,ptab=&p_PRJ_list; i<9; i++,ptab=ptab->next ) {}
	for (; ptab != NULL; ptab=ptab->next)
	  {
	    prtProj(fp,ptab);
	  }
	return 1;
      }

    int dump_single_projection(fp,attribute_name)

      FILE *fp;
      char *attribute_name;

      {
        struct projection_attr *ptab;
	void save_projection_secondary_elements();
	int i;

	for (i=0,ptab=&p_PRJ_list; i<9; i++,ptab=ptab->next );
        for (; (ptab != NULL) &&
            (strcmp(ptab->name, attribute_name) != 0); ptab=ptab->next) {}

	if (ptab != NULL) {
           prtProj(fp,ptab);
/* 	   save_projection_secondary_elements(fp,ptab); */
	}
        return 1;
      }

/*	Print the outline fill graph attributes that are defined.	*/

    int dmpGfo_attr(fp)

      FILE *fp;

      {
        struct gfo_tab *ptab;

	for (ptab=Gfo_tab.next; ptab != NULL; ptab=ptab->next)
		prtGfo(fp,ptab);
	return 1;
      }

    int dump_single_outfill(fp,attribute_name)

      FILE *fp;
      char *attribute_name;

      {
        struct gfo_tab *ptab;
        void save_outfill_secondary_elements();

        for (ptab=Gfo_tab.next; (ptab != NULL) &&
            (strcmp(ptab->name, attribute_name) != 0); ptab=ptab->next) {}

	if (ptab != NULL) {
           prtGfo(fp,ptab);
           save_outfill_secondary_elements(fp,ptab);
	}
        return 1;
      }

/*	Print the box fill graph attributes that are defined.	*/

    int dmpGfb_attr(fp)

      FILE *fp;

      {
        struct gfb_tab *ptab;

	for (ptab=Gfb_tab.next; ptab != NULL; ptab=ptab->next)
		prtGfb(fp,ptab);
	return 1;
      }

    int dump_single_boxfill(fp,attribute_name)

      FILE *fp;
      char *attribute_name;

      {
        struct gfb_tab *ptab;
	void save_boxfill_secondary_elements();

        for (ptab=Gfb_tab.next; (ptab != NULL) &&
            (strcmp(ptab->name, attribute_name) != 0); ptab=ptab->next) {}

	if (ptab != NULL) {
           prtGfb(fp,ptab);
	   save_boxfill_secondary_elements(fp,ptab);
	}
        return 1;
      }

/*	Print the vector graph attributes that are defined.	*/

    int dmpGv_attr(fp)

      FILE *fp;

      {
        struct gv_tab *ptab;

	for (ptab=Gv_tab.next; ptab != NULL; ptab=ptab->next)
		prtGv(fp,ptab);
	return 1;
      }

    int dump_single_vector(fp,attribute_name)

      FILE *fp;
      char *attribute_name;

      {
        struct gv_tab *ptab;
        void save_vector_secondary_elements();

        for (ptab=Gv_tab.next; (ptab != NULL) &&
            (strcmp(ptab->name, attribute_name) != 0); ptab=ptab->next) {}

	if (ptab != NULL) {
           prtGv(fp,ptab);
           save_vector_secondary_elements(fp,ptab);
	}
        return 1;
      }

/*	Print the X(y) vs y graph attributes that are defined.	*/

    int dmpGXy_attr(fp)

      FILE *fp;

      {
        struct gXy_tab *ptab;

	for (ptab=GXy_tab.next; ptab != NULL; ptab=ptab->next)
		prtGXy(fp,ptab);
	return 1;
      }

    int dump_single_xyvsy(fp,attribute_name)

      FILE *fp;
      char *attribute_name;

      {
        struct gXy_tab *ptab;
	void save_xyvsy_secondary_elements();

        for (ptab=GXy_tab.next; (ptab != NULL) &&
            (strcmp(ptab->name, attribute_name) != 0); ptab=ptab->next) {}

	if (ptab != NULL) {
           prtGXy(fp,ptab);
	   save_xyvsy_secondary_elements(fp,ptab);
	}
        return 1;
      }

/*	Print the Y(x) vs x graph attributes that are defined.	*/

    int dmpGYx_attr(fp)

      FILE *fp;

      {
        struct gYx_tab *ptab;

	for (ptab=GYx_tab.next; ptab != NULL; ptab=ptab->next)
		prtGYx(fp,ptab);
	return 1;
      }

    int dump_single_yxvsx(fp,attribute_name)

      FILE *fp;
      char *attribute_name;

      {
        struct gYx_tab *ptab;
	void save_yxvsx_secondary_elements();

        for (ptab=GYx_tab.next; (ptab != NULL) &&
            (strcmp(ptab->name, attribute_name) != 0); ptab=ptab->next) {}

	if (ptab != NULL) {
           prtGYx(fp,ptab);
	   save_yxvsx_secondary_elements(fp,ptab);
	}
        return 1;
      }

/*	Print the X(t) vs Y(t) graph attributes that are defined.	*/

    int dmpGXY_attr(fp)

      FILE *fp;

      {
        struct gXY_tab *ptab;

	for (ptab=GXY_tab.next; ptab != NULL; ptab=ptab->next)
		prtGXY(fp,ptab);
	return 1;
      }


    int dump_single_xvsy(fp,attribute_name)

      FILE *fp;
      char *attribute_name;

      {
        struct gXY_tab *ptab;
	void save_xvsy_secondary_elements();

        for (ptab=GXY_tab.next; (ptab != NULL) &&
            (strcmp(ptab->name, attribute_name) != 0); ptab=ptab->next) {}

	if (ptab != NULL) {
           prtGXY(fp,ptab);
	   save_xvsy_secondary_elements(fp,ptab);
	}
        return 1;
      }

/*	Print the X(...) vs Y(...) ScatterPlot graph attributes that are
							 defined.	*/

    int dmpGSp_attr(fp)

      FILE *fp;

      {
        struct gSp_tab *ptab;

	for (ptab=GSp_tab.next; ptab != NULL; ptab=ptab->next)
		prtGSp(fp,ptab);
	return 1;
      }

    int dump_single_scatter(fp,attribute_name)

      FILE *fp;
      char *attribute_name;

      {
        struct gSp_tab *ptab;
	void save_scatter_secondary_elements();

        for (ptab=GSp_tab.next; (ptab != NULL) &&
            (strcmp(ptab->name, attribute_name) != 0); ptab=ptab->next) {}

	if (ptab != NULL) {
           prtGSp(fp,ptab);
	   save_scatter_secondary_elements(fp,ptab);
	}
        return 1;
      }

/*		Print the display attributes that are defined.		*/

    int dmp_display(fp)

      FILE *fp;

      {
        struct display_tab *dtab;

	for (dtab=&D_tab; dtab != NULL; dtab=dtab->next)
		prtDisp(fp,dtab);
	return 1;
      }

     void save_secondary_elements(FILE *fp,struct p_tab *ptab)
      {
	int i;
	struct pe_text *ptx;
	struct pe_form *pfm;
	struct pe_dsp *pds;
	struct pe_leg *plg;
	struct pe_x_tic *pxt;
	struct pe_y_tic *pyt;
	struct pe_x_lab *pxl;
	struct pe_y_lab *pyl;
	struct pe_box *pbx;
        int dump_single_textt();
        int dump_single_texto();
        int dump_single_line();

	for (i=0,ptx=&(ptab->F); i < 22; i++,ptx++) {
	   if (strcmp(ptx->tb, "default") != 0)
              dump_single_textt(fp, ptx->tb);
	   if (strcmp(ptx->to, "default") != 0)
              dump_single_texto(fp, ptx->to);
        }
	for (i=0,pfm=&(ptab->xv); i < 7; i++,pfm++) {
	   if (strcmp(pfm->tb, "default") != 0)
              dump_single_textt(fp, pfm->tb);
	   if (strcmp(pfm->to, "default") != 0)
              dump_single_texto(fp, pfm->to);
	   if (strcmp(pfm->fmt, "default") != 0)
              dump_single_format(fp, pfm->fmt);
	}
        for (i=0,pxt=&(ptab->xt1); i < 4; i++,pxt++) {
	   if (strcmp(pxt->ln, "default") != 0)
              dump_single_line(fp, pxt->ln);
	}
	for (i=0,pyt=&(ptab->yt1); i < 4; i++,pyt++) {
	   if (strcmp(pyt->ln, "default") != 0)
              dump_single_line(fp, pyt->ln);
	}
	for (i=0,pxl=&(ptab->xl1); i < 2; i++,pxl++) {
	   if (strcmp(pxl->tb, "default") != 0)
              dump_single_textt(fp, pxl->tb);
	   if (strcmp(pxl->to, "default") != 0)
              dump_single_texto(fp, pxl->to);
	}
	for (i=0,pyl=&(ptab->yl1); i < 2; i++,pyl++) {
	   if (strcmp(pyl->tb, "default") != 0)
              dump_single_textt(fp, pyl->tb);
	   if (strcmp(pyl->to, "default") != 0)
              dump_single_texto(fp, pyl->to);
	}
	for (i=0,pbx=&(ptab->b1); i < 8; i++,pbx++) {
	   if (strcmp(pbx->ln, "default") != 0)
              dump_single_line(fp, pbx->ln);
	}
	plg=&(ptab->leg);
	if (strcmp(plg->tb, "default") != 0)
              dump_single_textt(fp, plg->tb);
	if (strcmp(plg->to, "default") != 0)
              dump_single_texto(fp, plg->to);
	if (strcmp(plg->ln, "default") != 0)
              dump_single_line(fp, plg->ln);
      }

     void save_boxfill_secondary_elements(FILE *fp,struct gfb_tab *ptab)
      {
        struct fill_range *pfro;
        int dump_single_list();

	if ( (ptab->pGfb_attr->xtl1[0] != '\0') &&
             (ptab->pGfb_attr->xtl1[0] != '*' ) )
           dump_single_list(fp,ptab->pGfb_attr->xtl1);
	if ( (ptab->pGfb_attr->xtl2[0] != '\0') &&
             (ptab->pGfb_attr->xtl2[0] != '*' ) )
           dump_single_list(fp,ptab->pGfb_attr->xtl2);
	if ( (ptab->pGfb_attr->xmt1[0] != '\0') &&
             (ptab->pGfb_attr->xmt1[0] != '*' ) )
           dump_single_list(fp,ptab->pGfb_attr->xmt1);
	if ( (ptab->pGfb_attr->xmt2[0] != '\0') &&
             (ptab->pGfb_attr->xmt2[0] != '*' ) )
           dump_single_list(fp,ptab->pGfb_attr->xmt2);
	if ( (ptab->pGfb_attr->ytl1[0] != '\0') &&
             (ptab->pGfb_attr->ytl1[0] != '*' ) )
           dump_single_list(fp,ptab->pGfb_attr->ytl1);
	if ( (ptab->pGfb_attr->ytl2[0] != '\0') &&
             (ptab->pGfb_attr->ytl2[0] != '*' ) )
           dump_single_list(fp,ptab->pGfb_attr->ytl2);
	if ( (ptab->pGfb_attr->ymt1[0] != '\0') &&
             (ptab->pGfb_attr->ymt1[0] != '*' ) )
           dump_single_list(fp,ptab->pGfb_attr->ymt1);
	if ( (ptab->pGfb_attr->ymt2[0] != '\0') &&
             (ptab->pGfb_attr->ymt2[0] != '*' ) )
           dump_single_list(fp,ptab->pGfb_attr->ymt2);
        if (ptab->pGfb_attr->legend != NULL)
           dump_single_list(fp,ptab->pGfb_attr->legend);

	if ((pfro=ptab->pGfb_attr->line) != NULL) {
           for (;pfro != NULL; pfro=pfro->next) {
	       if (strcmp(pfro->fill_name, "default") != 0)
                   dump_single_fillarea(fp, pfro->fill_name);
           }
        }
      }

     void save_projection_secondary_elements(FILE *fp,struct projection_attr *ptab)
      {
        int dump_single_list();

	dump_single_list(fp,ptab->parm);
      }

     void save_isoline_secondary_elements(FILE *fp, struct gi_tab *ptab) 
      {
	struct iso *piso;
        int dump_single_list();
        int dump_single_line();
        int dump_single_textt();
        int dump_single_texto();

        if ( (ptab->pGi_attr->xtl1[0] != '\0') &&
             (ptab->pGi_attr->xtl1[0] != '*' ) )
           dump_single_list(fp,ptab->pGi_attr->xtl1);
        if ( (ptab->pGi_attr->xtl2[0] != '\0') &&
             (ptab->pGi_attr->xtl2[0] != '*' ) )
           dump_single_list(fp,ptab->pGi_attr->xtl2);
        if ( (ptab->pGi_attr->xmt1[0] != '\0') &&
             (ptab->pGi_attr->xmt1[0] != '*' ) )
           dump_single_list(fp,ptab->pGi_attr->xmt1);
        if ( (ptab->pGi_attr->xmt2[0] != '\0') &&
             (ptab->pGi_attr->xmt2[0] != '*' ) )
           dump_single_list(fp,ptab->pGi_attr->xmt2);
        if ( (ptab->pGi_attr->ytl1[0] != '\0') &&
             (ptab->pGi_attr->ytl1[0] != '*' ) )
           dump_single_list(fp,ptab->pGi_attr->ytl1);
        if ( (ptab->pGi_attr->ytl2[0] != '\0') &&
             (ptab->pGi_attr->ytl2[0] != '*' ) )
           dump_single_list(fp,ptab->pGi_attr->ytl2);
        if ( (ptab->pGi_attr->ymt1[0] != '\0') &&
             (ptab->pGi_attr->ymt1[0] != '*' ) )
           dump_single_list(fp,ptab->pGi_attr->ymt1);
        if ( (ptab->pGi_attr->ymt2[0] != '\0') &&
             (ptab->pGi_attr->ymt2[0] != '*' ) )
           dump_single_list(fp,ptab->pGi_attr->ymt2);

	if ((piso=ptab->pGi_attr->line) != NULL) {
           for (;piso != NULL; piso=piso->next) {
	      if (strcmp(piso->tb, "default") != 0)
                    dump_single_textt(fp, piso->tb);
	      if (strcmp(piso->to, "default") != 0)
                    dump_single_texto(fp, piso->to);
	      if (strcmp(piso->lb, "default") != 0)
                    dump_single_line(fp, piso->lb);
           }
	}
      }

     void save_continents_secondary_elements(FILE *fp,struct gcon_tab *ptab)
      {
        int dump_single_list();
        int dump_single_line();

        if ( (ptab->pGcon_attr->xtl1[0] != '\0') &&
             (ptab->pGcon_attr->xtl1[0] != '*' ) )
           dump_single_list(fp,ptab->pGcon_attr->xtl1);
        if ( (ptab->pGcon_attr->xtl2[0] != '\0') &&
             (ptab->pGcon_attr->xtl2[0] != '*' ) )
           dump_single_list(fp,ptab->pGcon_attr->xtl2);
        if ( (ptab->pGcon_attr->xmt1[0] != '\0') &&
             (ptab->pGcon_attr->xmt1[0] != '*' ) )
           dump_single_list(fp,ptab->pGcon_attr->xmt1);
        if ( (ptab->pGcon_attr->xmt2[0] != '\0') &&
             (ptab->pGcon_attr->xmt2[0] != '*' ) )
           dump_single_list(fp,ptab->pGcon_attr->xmt2);
        if ( (ptab->pGcon_attr->ytl1[0] != '\0') &&
             (ptab->pGcon_attr->ytl1[0] != '*' ) )
           dump_single_list(fp,ptab->pGcon_attr->ytl1);
        if ( (ptab->pGcon_attr->ytl2[0] != '\0') &&
             (ptab->pGcon_attr->ytl2[0] != '*' ) )
           dump_single_list(fp,ptab->pGcon_attr->ytl2);
        if ( (ptab->pGcon_attr->ymt1[0] != '\0') &&
             (ptab->pGcon_attr->ymt1[0] != '*' ) )
           dump_single_list(fp,ptab->pGcon_attr->ymt1);
        if ( (ptab->pGcon_attr->ymt2[0] != '\0') &&
             (ptab->pGcon_attr->ymt2[0] != '*' ) )
           dump_single_list(fp,ptab->pGcon_attr->ymt2);

	if (strcmp(ptab->pGcon_attr->lb, "default") != 0)
           dump_single_line(fp, ptab->pGcon_attr->lb);
      }

     void save_outline_secondary_elements(FILE *fp, struct go_tab *ptab)
      {
        int dump_single_list();
        int dump_single_line();

        if ( (ptab->pGo_attr->xtl1[0] != '\0') &&
             (ptab->pGo_attr->xtl1[0] != '*' ) )
           dump_single_list(fp,ptab->pGo_attr->xtl1);
        if ( (ptab->pGo_attr->xtl2[0] != '\0') &&
             (ptab->pGo_attr->xtl2[0] != '*' ) )
           dump_single_list(fp,ptab->pGo_attr->xtl2);
        if ( (ptab->pGo_attr->xmt1[0] != '\0') &&
             (ptab->pGo_attr->xmt1[0] != '*' ) )
           dump_single_list(fp,ptab->pGo_attr->xmt1);
        if ( (ptab->pGo_attr->xmt2[0] != '\0') &&
             (ptab->pGo_attr->xmt2[0] != '*' ) )
           dump_single_list(fp,ptab->pGo_attr->xmt2);
        if ( (ptab->pGo_attr->ytl1[0] != '\0') &&
             (ptab->pGo_attr->ytl1[0] != '*' ) )
           dump_single_list(fp,ptab->pGo_attr->ytl1);
        if ( (ptab->pGo_attr->ytl2[0] != '\0') &&
             (ptab->pGo_attr->ytl2[0] != '*' ) )
           dump_single_list(fp,ptab->pGo_attr->ytl2);
        if ( (ptab->pGo_attr->ymt1[0] != '\0') &&
             (ptab->pGo_attr->ymt1[0] != '*' ) )
           dump_single_list(fp,ptab->pGo_attr->ymt1);
        if ( (ptab->pGo_attr->ymt2[0] != '\0') &&
             (ptab->pGo_attr->ymt2[0] != '*' ) )
           dump_single_list(fp,ptab->pGo_attr->ymt2);

	if (strcmp(ptab->pGo_attr->lb, "default") != 0)
           dump_single_line(fp, ptab->pGo_attr->lb);
      }

     void save_isofill_secondary_elements(FILE *fp,struct gfi_tab *ptab)
      {
	struct fill_range *pfro;
        int dump_single_list();

        if ( (ptab->pGfi_attr->xtl1[0] != '\0') &&
             (ptab->pGfi_attr->xtl1[0] != '*' ) )
           dump_single_list(fp,ptab->pGfi_attr->xtl1);
        if ( (ptab->pGfi_attr->xtl2[0] != '\0') &&
             (ptab->pGfi_attr->xtl2[0] != '*' ) )
           dump_single_list(fp,ptab->pGfi_attr->xtl2);
        if ( (ptab->pGfi_attr->xmt1[0] != '\0') &&
             (ptab->pGfi_attr->xmt1[0] != '*' ) )
           dump_single_list(fp,ptab->pGfi_attr->xmt1);
        if ( (ptab->pGfi_attr->xmt2[0] != '\0') &&
             (ptab->pGfi_attr->xmt2[0] != '*' ) )
           dump_single_list(fp,ptab->pGfi_attr->xmt2);
        if ( (ptab->pGfi_attr->ytl1[0] != '\0') &&
             (ptab->pGfi_attr->ytl1[0] != '*' ) )
           dump_single_list(fp,ptab->pGfi_attr->ytl1);
        if ( (ptab->pGfi_attr->ytl2[0] != '\0') &&
             (ptab->pGfi_attr->ytl2[0] != '*' ) )
           dump_single_list(fp,ptab->pGfi_attr->ytl2);
        if ( (ptab->pGfi_attr->ymt1[0] != '\0') &&
             (ptab->pGfi_attr->ymt1[0] != '*' ) )
           dump_single_list(fp,ptab->pGfi_attr->ymt1);
        if ( (ptab->pGfi_attr->ymt2[0] != '\0') &&
             (ptab->pGfi_attr->ymt2[0] != '*' ) )
           dump_single_list(fp,ptab->pGfi_attr->ymt2);
        if (ptab->pGfi_attr->legend != NULL)
           dump_single_list(fp,ptab->pGfi_attr->legend);

	if ((pfro=ptab->pGfi_attr->line) != NULL) {
           for (;pfro != NULL; pfro=pfro->next) {
	       if (strcmp(pfro->fill_name, "default") != 0)
                   dump_single_fillarea(fp, pfro->fill_name);
           }
        }
      }

     void save_meshfill_secondary_elements(FILE *fp,struct gfm_tab *ptab)
      {
	struct fill_range *pfro;
        int dump_single_list();

        if ( (ptab->pGfm_attr->xtl1[0] != '\0') &&
             (ptab->pGfm_attr->xtl1[0] != '*' ) )
           dump_single_list(fp,ptab->pGfm_attr->xtl1);
        if ( (ptab->pGfm_attr->xtl2[0] != '\0') &&
             (ptab->pGfm_attr->xtl2[0] != '*' ) )
           dump_single_list(fp,ptab->pGfm_attr->xtl2);
        if ( (ptab->pGfm_attr->xmt1[0] != '\0') &&
             (ptab->pGfm_attr->xmt1[0] != '*' ) )
           dump_single_list(fp,ptab->pGfm_attr->xmt1);
        if ( (ptab->pGfm_attr->xmt2[0] != '\0') &&
             (ptab->pGfm_attr->xmt2[0] != '*' ) )
           dump_single_list(fp,ptab->pGfm_attr->xmt2);
        if ( (ptab->pGfm_attr->ytl1[0] != '\0') &&
             (ptab->pGfm_attr->ytl1[0] != '*' ) )
           dump_single_list(fp,ptab->pGfm_attr->ytl1);
        if ( (ptab->pGfm_attr->ytl2[0] != '\0') &&
             (ptab->pGfm_attr->ytl2[0] != '*' ) )
           dump_single_list(fp,ptab->pGfm_attr->ytl2);
        if ( (ptab->pGfm_attr->ymt1[0] != '\0') &&
             (ptab->pGfm_attr->ymt1[0] != '*' ) )
           dump_single_list(fp,ptab->pGfm_attr->ymt1);
        if ( (ptab->pGfm_attr->ymt2[0] != '\0') &&
             (ptab->pGfm_attr->ymt2[0] != '*' ) )
           dump_single_list(fp,ptab->pGfm_attr->ymt2);

	if ((pfro=ptab->pGfm_attr->line) != NULL) {
           for (;pfro != NULL; pfro=pfro->next) {
	       if (strcmp(pfro->fill_name, "default") != 0)
                   dump_single_fillarea(fp, pfro->fill_name);
           }
        }
      }

     void save_outfill_secondary_elements(FILE *fp, struct gfo_tab *ptab)
      {
        int dump_single_list();
        int dump_single_fillarea();

        if ( (ptab->pGfo_attr->xtl1[0] != '\0') &&
             (ptab->pGfo_attr->xtl1[0] != '*' ) )
           dump_single_list(fp,ptab->pGfo_attr->xtl1);
        if ( (ptab->pGfo_attr->xtl2[0] != '\0') &&
             (ptab->pGfo_attr->xtl2[0] != '*' ) )
           dump_single_list(fp,ptab->pGfo_attr->xtl2);
        if ( (ptab->pGfo_attr->xmt1[0] != '\0') &&
             (ptab->pGfo_attr->xmt1[0] != '*' ) )
           dump_single_list(fp,ptab->pGfo_attr->xmt1);
        if ( (ptab->pGfo_attr->xmt2[0] != '\0') &&
             (ptab->pGfo_attr->xmt2[0] != '*' ) )
           dump_single_list(fp,ptab->pGfo_attr->xmt2);
        if ( (ptab->pGfo_attr->ytl1[0] != '\0') &&
             (ptab->pGfo_attr->ytl1[0] != '*' ) )
           dump_single_list(fp,ptab->pGfo_attr->ytl1);
        if ( (ptab->pGfo_attr->ytl2[0] != '\0') &&
             (ptab->pGfo_attr->ytl2[0] != '*' ) )
           dump_single_list(fp,ptab->pGfo_attr->ytl2);
        if ( (ptab->pGfo_attr->ymt1[0] != '\0') &&
             (ptab->pGfo_attr->ymt1[0] != '*' ) )
           dump_single_list(fp,ptab->pGfo_attr->ymt1);
        if ( (ptab->pGfo_attr->ymt2[0] != '\0') &&
             (ptab->pGfo_attr->ymt2[0] != '*' ) )
           dump_single_list(fp,ptab->pGfo_attr->ymt2);

        if (strcmp(ptab->pGfo_attr->f, "default") != 0)
            dump_single_fillarea(fp, ptab->pGfo_attr->f);
      }

     void   save_vector_secondary_elements(FILE *fp, struct gv_tab *ptab)
      {
        int dump_single_list();
        int dump_single_line();

        if ( (ptab->pGv_attr->xtl1[0] != '\0') &&
             (ptab->pGv_attr->xtl1[0] != '*' ) )
           dump_single_list(fp,ptab->pGv_attr->xtl1);
        if ( (ptab->pGv_attr->xtl2[0] != '\0') &&
             (ptab->pGv_attr->xtl2[0] != '*' ) )
           dump_single_list(fp,ptab->pGv_attr->xtl2);
        if ( (ptab->pGv_attr->xmt1[0] != '\0') &&
             (ptab->pGv_attr->xmt1[0] != '*' ) )
           dump_single_list(fp,ptab->pGv_attr->xmt1);
        if ( (ptab->pGv_attr->xmt2[0] != '\0') &&
             (ptab->pGv_attr->xmt2[0] != '*' ) )
           dump_single_list(fp,ptab->pGv_attr->xmt2);
        if ( (ptab->pGv_attr->ytl1[0] != '\0') &&
             (ptab->pGv_attr->ytl1[0] != '*' ) )
           dump_single_list(fp,ptab->pGv_attr->ytl1);
        if ( (ptab->pGv_attr->ytl2[0] != '\0') &&
             (ptab->pGv_attr->ytl2[0] != '*' ) )
           dump_single_list(fp,ptab->pGv_attr->ytl2);
        if ( (ptab->pGv_attr->ymt1[0] != '\0') &&
             (ptab->pGv_attr->ymt1[0] != '*' ) )
           dump_single_list(fp,ptab->pGv_attr->ymt1);
        if ( (ptab->pGv_attr->ymt2[0] != '\0') &&
             (ptab->pGv_attr->ymt2[0] != '*' ) )
           dump_single_list(fp,ptab->pGv_attr->ymt2);

        if (strcmp(ptab->pGv_attr->lb, "default") != 0)
            dump_single_line(fp, ptab->pGv_attr->lb);
      }

     void save_xyvsy_secondary_elements(FILE *fp, struct gXy_tab *ptab)
      {
        int dump_single_list();
        int dump_single_line();
        int dump_single_marker();

        if ( (ptab->pGXy_attr->xtl1[0] != '\0') &&
             (ptab->pGXy_attr->xtl1[0] != '*' ) )
           dump_single_list(fp,ptab->pGXy_attr->xtl1);
        if ( (ptab->pGXy_attr->xtl2[0] != '\0') &&
             (ptab->pGXy_attr->xtl2[0] != '*' ) )
           dump_single_list(fp,ptab->pGXy_attr->xtl2);
        if ( (ptab->pGXy_attr->xmt1[0] != '\0') &&
             (ptab->pGXy_attr->xmt1[0] != '*' ) )
           dump_single_list(fp,ptab->pGXy_attr->xmt1);
        if ( (ptab->pGXy_attr->xmt2[0] != '\0') &&
             (ptab->pGXy_attr->xmt2[0] != '*' ) )
           dump_single_list(fp,ptab->pGXy_attr->xmt2);
        if ( (ptab->pGXy_attr->ytl1[0] != '\0') &&
             (ptab->pGXy_attr->ytl1[0] != '*' ) )
           dump_single_list(fp,ptab->pGXy_attr->ytl1);
        if ( (ptab->pGXy_attr->ytl2[0] != '\0') &&
             (ptab->pGXy_attr->ytl2[0] != '*' ) )
           dump_single_list(fp,ptab->pGXy_attr->ytl2);
        if ( (ptab->pGXy_attr->ymt1[0] != '\0') &&
             (ptab->pGXy_attr->ymt1[0] != '*' ) )
           dump_single_list(fp,ptab->pGXy_attr->ymt1);
        if ( (ptab->pGXy_attr->ymt2[0] != '\0') &&
             (ptab->pGXy_attr->ymt2[0] != '*' ) )
           dump_single_list(fp,ptab->pGXy_attr->ymt2);

        if (strcmp(ptab->pGXy_attr->lb, "default") != 0)
            dump_single_line(fp, ptab->pGXy_attr->lb);
        if (strcmp(ptab->pGXy_attr->mb, "default") != 0)
            dump_single_marker(fp, ptab->pGXy_attr->mb);
      }

     void save_yxvsx_secondary_elements(FILE *fp, struct gYx_tab *ptab)
      {
        int dump_single_list();
        int dump_single_line();
        int dump_single_marker();

        if ( (ptab->pGYx_attr->xtl1[0] != '\0') &&
             (ptab->pGYx_attr->xtl1[0] != '*' ) )
           dump_single_list(fp,ptab->pGYx_attr->xtl1);
        if ( (ptab->pGYx_attr->xtl2[0] != '\0') &&
             (ptab->pGYx_attr->xtl2[0] != '*' ) )
           dump_single_list(fp,ptab->pGYx_attr->xtl2);
        if ( (ptab->pGYx_attr->xmt1[0] != '\0') &&
             (ptab->pGYx_attr->xmt1[0] != '*' ) )
           dump_single_list(fp,ptab->pGYx_attr->xmt1);
        if ( (ptab->pGYx_attr->xmt2[0] != '\0') &&
             (ptab->pGYx_attr->xmt2[0] != '*' ) )
           dump_single_list(fp,ptab->pGYx_attr->xmt2);
        if ( (ptab->pGYx_attr->ytl1[0] != '\0') &&
             (ptab->pGYx_attr->ytl1[0] != '*' ) )
           dump_single_list(fp,ptab->pGYx_attr->ytl1);
        if ( (ptab->pGYx_attr->ytl2[0] != '\0') &&
             (ptab->pGYx_attr->ytl2[0] != '*' ) )
           dump_single_list(fp,ptab->pGYx_attr->ytl2);
        if ( (ptab->pGYx_attr->ymt1[0] != '\0') &&
             (ptab->pGYx_attr->ymt1[0] != '*' ) )
           dump_single_list(fp,ptab->pGYx_attr->ymt1);
        if ( (ptab->pGYx_attr->ymt2[0] != '\0') &&
             (ptab->pGYx_attr->ymt2[0] != '*' ) )
           dump_single_list(fp,ptab->pGYx_attr->ymt2);

        if (strcmp(ptab->pGYx_attr->lb, "default") != 0)
            dump_single_line(fp, ptab->pGYx_attr->lb);
        if (strcmp(ptab->pGYx_attr->mb, "default") != 0)
            dump_single_marker(fp, ptab->pGYx_attr->mb);
      }

     void save_xvsy_secondary_elements(FILE *fp, struct gXY_tab *ptab)
      {
        int dump_single_list();
        int dump_single_line();
        int dump_single_marker();

        if ( (ptab->pGXY_attr->xtl1[0] != '\0') &&
             (ptab->pGXY_attr->xtl1[0] != '*' ) )
           dump_single_list(fp,ptab->pGXY_attr->xtl1);
        if ( (ptab->pGXY_attr->xtl2[0] != '\0') &&
             (ptab->pGXY_attr->xtl2[0] != '*' ) )
           dump_single_list(fp,ptab->pGXY_attr->xtl2);
        if ( (ptab->pGXY_attr->xmt1[0] != '\0') &&
             (ptab->pGXY_attr->xmt1[0] != '*' ) )
           dump_single_list(fp,ptab->pGXY_attr->xmt1);
        if ( (ptab->pGXY_attr->xmt2[0] != '\0') &&
             (ptab->pGXY_attr->xmt2[0] != '*' ) )
           dump_single_list(fp,ptab->pGXY_attr->xmt2);
        if ( (ptab->pGXY_attr->ytl1[0] != '\0') &&
             (ptab->pGXY_attr->ytl1[0] != '*' ) )
           dump_single_list(fp,ptab->pGXY_attr->ytl1);
        if ( (ptab->pGXY_attr->ytl2[0] != '\0') &&
             (ptab->pGXY_attr->ytl2[0] != '*' ) )
           dump_single_list(fp,ptab->pGXY_attr->ytl2);
        if ( (ptab->pGXY_attr->ymt1[0] != '\0') &&
             (ptab->pGXY_attr->ymt1[0] != '*' ) )
           dump_single_list(fp,ptab->pGXY_attr->ymt1);
        if ( (ptab->pGXY_attr->ymt2[0] != '\0') &&
             (ptab->pGXY_attr->ymt2[0] != '*' ) )
           dump_single_list(fp,ptab->pGXY_attr->ymt2);

        if (strcmp(ptab->pGXY_attr->lb, "default") != 0)
            dump_single_line(fp, ptab->pGXY_attr->lb);
        if (strcmp(ptab->pGXY_attr->mb, "default") != 0)
            dump_single_marker(fp, ptab->pGXY_attr->mb);
      }

     void save_scatter_secondary_elements(FILE *fp, struct gSp_tab *ptab)
      {
        int dump_single_list();
        int dump_single_marker();

        if ( (ptab->pGSp_attr->xtl1[0] != '\0') &&
             (ptab->pGSp_attr->xtl1[0] != '*' ) )
           dump_single_list(fp,ptab->pGSp_attr->xtl1);
        if ( (ptab->pGSp_attr->xtl2[0] != '\0') &&
             (ptab->pGSp_attr->xtl2[0] != '*' ) )
           dump_single_list(fp,ptab->pGSp_attr->xtl2);
        if ( (ptab->pGSp_attr->xmt1[0] != '\0') &&
             (ptab->pGSp_attr->xmt1[0] != '*' ) )
           dump_single_list(fp,ptab->pGSp_attr->xmt1);
        if ( (ptab->pGSp_attr->xmt2[0] != '\0') &&
             (ptab->pGSp_attr->xmt2[0] != '*' ) )
           dump_single_list(fp,ptab->pGSp_attr->xmt2);
        if ( (ptab->pGSp_attr->ytl1[0] != '\0') &&
             (ptab->pGSp_attr->ytl1[0] != '*' ) )
           dump_single_list(fp,ptab->pGSp_attr->ytl1);
        if ( (ptab->pGSp_attr->ytl2[0] != '\0') &&
             (ptab->pGSp_attr->ytl2[0] != '*' ) )
           dump_single_list(fp,ptab->pGSp_attr->ytl2);
        if ( (ptab->pGSp_attr->ymt1[0] != '\0') &&
             (ptab->pGSp_attr->ymt1[0] != '*' ) )
           dump_single_list(fp,ptab->pGSp_attr->ymt1);
        if ( (ptab->pGSp_attr->ymt2[0] != '\0') &&
             (ptab->pGSp_attr->ymt2[0] != '*' ) )
           dump_single_list(fp,ptab->pGSp_attr->ymt2);

        if (strcmp(ptab->pGSp_attr->mb, "default") != 0)
            dump_single_marker(fp, ptab->pGSp_attr->mb);
      }

     void save_data_secondary_elements(FILE *fp,struct a_tab *ptab)
      {
	struct a_attr           *pA_;
	int			i;
        int 			dump_single_list();

	pA_=ptab->pA_attr;

        if ( (pA_->aF!=NULL)&& (pA_->aF[0]!='\0')&&(pA_->aF[0]!='*'))
           dump_single_list(fp,pA_->aF);
        if ( (pA_->af!=NULL)&&(pA_->af[0]!='\0')&&(pA_->af[0]!='*'))
           dump_single_list(fp,pA_->af);
        if ((pA_->almask!=NULL)&&(pA_->almask[0]!='\0')&&(pA_->almask[0]!='*'))
           dump_single_list(fp,pA_->almask);
        if ( (pA_->atrnf!=NULL)&&(pA_->atrnf[0]!='\0')&&(pA_->atrnf[0]!='*'))
           dump_single_list(fp,pA_->atrnf);
        if ( (pA_->aS!=NULL)&&(pA_->aS[0]!='\0')&&(pA_->aS[0]!='*'))
           dump_single_list(fp,pA_->aS);
        if ( (pA_->aN!=NULL)&&(pA_->aN[0]!='\0')&&(pA_->aN[0]!='*'))
           dump_single_list(fp,pA_->aN);
        if ( (pA_->aTI!=NULL)&&(pA_->aTI[0]!='\0')&&(pA_->aTI[0]!='*'))
           dump_single_list(fp,pA_->aTI);
        if ( (pA_->aU!=NULL)&&(pA_->aU[0]!='\0')&&(pA_->aU[0]!='*'))
           dump_single_list(fp,pA_->aU);
        if ( (pA_->aTY!=NULL)&&(pA_->aTY[0]!='\0')&&(pA_->aTY[0]!='*'))
           dump_single_list(fp,pA_->aTY);
        if ( (pA_->aCRD!=NULL)&&(pA_->aCRD[0]!='\0')&&(pA_->aCRD[0]!='*'))
           dump_single_list(fp,pA_->aCRD);
        if ( (pA_->aCRT!=NULL)&&(pA_->aCRT[0]!='\0')&&(pA_->aCRT[0]!='*'))
           dump_single_list(fp,pA_->aCRT);

        if ( (pA_->as!=NULL) && (pA_->as[0]!='\0')&&(pA_->as[0]!='*' ) )
           dump_single_list(fp,pA_->as);
        if ( (pA_->an!=NULL) && (pA_->an[0]!='\0')&&(pA_->an[0]!='*' ) )
           dump_single_list(fp,pA_->an);
        if ( (pA_->ati!=NULL) && (pA_->ati[0]!='\0')&&(pA_->ati[0]!='*' ) )
           dump_single_list(fp,pA_->ati);
        if ( (pA_->au!=NULL) && (pA_->au[0]!='\0')&&(pA_->au[0]!='*' ) )
           dump_single_list(fp,pA_->au);
        if ( (pA_->aty!=NULL) && (pA_->aty[0]!='\0')&&(pA_->aty[0]!='*' ) )
           dump_single_list(fp,pA_->aty);
        if ( (pA_->acrd!=NULL) && (pA_->acrd[0]!='\0')&&(pA_->acrd[0]!='*'))
           dump_single_list(fp,pA_->acrd);
        if ( (pA_->acrt!=NULL) && (pA_->acrt[0]!='\0')&&(pA_->acrt[0]!='*'))
           dump_single_list(fp,pA_->acrt);
        if ( (pA_->acom1!=NULL) && (pA_->acom1[0]!='\0')&&(pA_->acom1[0]!='*'))
           dump_single_list(fp,pA_->acom1);
        if ( (pA_->acom2!=NULL) && (pA_->acom2[0]!='\0')&&(pA_->acom2[0]!='*'))
           dump_single_list(fp,pA_->acom2);
        if ( (pA_->acom3!=NULL)&&(pA_->acom3[0]!='\0')&&(pA_->acom3[0]!='*'))
           dump_single_list(fp,pA_->acom3);
        if ((pA_->acom4!=NULL)&&(pA_->acom4[0]!='\0')&&(pA_->acom4[0]!='*'))
           dump_single_list(fp,pA_->acom4);
	
	for (i = 0; i < pA_->ND; ++i) {
           if ((pA_->axn[i]!=NULL)&&(pA_->axn[i][0]!='\0')&&
               (pA_->axn[i][0]!='*'))
              dump_single_list(fp,pA_->axn[i]);
           if ((pA_->axu[i]!=NULL)&&(pA_->axu[i][0]!='\0')&&
               (pA_->axu[i][0]!='*'))
              dump_single_list(fp,pA_->axu[i]);
           if ((pA_->axf[i]!=NULL)&&(pA_->axf[i][0]!='\0')&&
               (pA_->axf[i][0]!='*'))
              dump_single_list(fp,pA_->axf[i]);
           if ((pA_->axl[i]!=NULL)&&(pA_->axl[i][0]!='\0')&&
               (pA_->axl[i][0]!='*'))
              dump_single_list(fp,pA_->axl[i]);
	}
      }
