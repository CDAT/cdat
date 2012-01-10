/* psql structs, pointers, defines */

#ifndef _PSQL_H
#define _PSQL_H

#include <stdio.h>
#include <math.h>
#include <stdarg.h>
#include <cdms.h>
#include <string.h>
#include <dirent.h>
#include <sys/stat.h>
#include <stdlib.h>
#include <netcdf.h>
#include <pwd.h>
#include <grp.h>
#include <sys/types.h>
#include <time.h>
#include <ctype.h>


/*--------------------------------------------------------------------
 *                Integer ID numbers for CDMS tree nodes
 *-------------------------------------------------------------------*/


typedef enum {
        id_cdNone = 4321,
        id_cdHd,
        id_cdTmp,
        id_cdPql,
        id_cdDb,
        id_cdDset,
        id_cdVar,
        id_cdDim,
        id_cdAtt
} cdms_Id;


/*--------------------------------------------------------------------
 *                PQL keywords and enumerations
 *-------------------------------------------------------------------*/


#define L_pql 42             /* number of pql keywords */
#define L_pql_node_type 5    /* number of pql tree node identifiers */

enum { i_database,  i_dataset,   i_variable,  i_dimension,  i_attribute,
       i_select,    i_distinct,  i_at,        i_below,      i_above,
       i_name,      i_value,     i_length,    i_where,      i_union,
       i_show,      i_id,        i_from,      i_dirtree,    i_end,
       i_readmeta,  i_writemeta, i_pwd,       i_tempmount,  i_group,
       i_release,   i_read,      i_cd,        i_virtual,    i_ndim,
       i_ttycopy,   i_with,      i_for,       i_filename,   i_readcheck,
       i_type,      i_check,     i_alter,     i_readalter,  i_subfgroup,
       i_writeds,   i_lenreadbuf };

#ifndef PSQL
char *pqlkey[L_pql] = {
       "database",  "dataset",   "variable",  "dimension",  "attribute",
       "select",    "distinct",  "at",        "below",      "above",
       "name",      "value",     "length",    "where",      "union",
       "show",      "id",        "from",      "dirtree",    "end",
       "readmeta",  "writemeta", "pwd",       "tempmount",  "group",
       "release",   "read",      "cd",        "virtual",    "ndim",
       "ttycopy",   "with",      "for",       "filename",   "readcheck",
       "type",      "check",     "alter",     "readalter",  "subfgroup",
       "writeds",   "lenreadbuf" };

cdms_Id pqlnode[L_pql_node_type] = {
       id_cdDb,     id_cdDset,   id_cdVar,    id_cdDim,     id_cdAtt };
#else
extern char *pqlkey[];
extern cdms_Id pqlnode[];
#endif


/**********************************************************************/
                             /* Database */
/**********************************************************************/


typedef struct cdDb_n {
        cdms_Id            id;        /* id identifier of this struct */
        cdms_Id            id_above;  /* id struct above this struct */
        void               *above;    /* struct above this struct */
        char               *name;     /* Database name */
        struct cdDset_n    *dsets;    /* Datasets */
        struct cdVar_n     *vars;     /* Variables */
        struct cdDim_n     *dims;     /* Dimensions */
        struct cdAtt_n     *atts;     /* Attributes */
        struct cdPql_n     *pqls;     /* Pql lists */
        struct cdDb_n      *next;     /* Next database */
        struct cdCheck_n   *ckdefs;   /* Check Var,Dim,Att def. */
        struct cdDbFlag_n  *f;        /* Global scalars, flags */
} cdDb_new;


/**********************************************************************/
                             /* Dataset */
/**********************************************************************/


typedef struct cdDset_n {
        cdms_Id          id;         /* id identifier of this struct */
        cdms_Id          id_above;   /* id struct above this struct */
        void             *above;     /* struct above this struct */
        char             *name;      /* Dataset name */
        struct cdVar_n   *vars;      /* Variables */
        struct cdDim_n   *dims;      /* Dimensions */
        struct cdAtt_n   *atts;      /* Attributes */
        struct cdDset_n  *next;      /* Next dataset */
} cdDset_new;


/**********************************************************************/
                             /* Variable */
/**********************************************************************/


typedef struct cdVar_n {
        cdms_Id          id;           /* id identifier of this struct */
        cdms_Id          id_above;     /* id struct above this struct */
        void             *above;       /* struct above this struct */
        char             *name;        /* Parameter name (official) */
        struct cdAtt_n   *atts;        /* Attributes */
        int              ndims;        /* Number of dimensions */
        struct cdTmp_n   *dim;         /* Dimension IDs (in dim order) */
        cdType           datatype;     /* cdChar, cdInt, etc. */
        long             length;       /* Number of elements */
        void             *data;        /* Data for implicit definition */
        struct cdVar_n   *next;        /* Next Variable */
} cdVar_new;


/**********************************************************************/
                             /* Dimension */
/**********************************************************************/


typedef struct cdDim_n {
        cdms_Id         id;            /* id identifier of this struct */
        cdms_Id         id_above;      /* id struct above this struct */
        void            *above;        /* struct above this struct */
        char            *name;         /* Dimension name */
        struct cdAtt_n  *atts;         /* Attributes */
        char            *units;        /* Units*/
        cdType          datatype;      /* Dimension datatype */
        long            length;        /* Number of elements */
        void            *data;         /* coordinates if not cdLinear */
        struct cdDim_n  *next;         /* Next Dimension */
} cdDim_new;


/**********************************************************************/
                             /* Attribute */
/**********************************************************************/


typedef struct cdAtt_n {
        cdms_Id         id;        /* id identifier of this struct */
        cdms_Id         id_above;  /* id struct above this struct */
        void            *above;    /* struct above this struct */
        char            *name;     /* Attribute name */
        cdType          datatype;  /* Datatype of the attribute */
        long            length;    /* Number of elements (NOT bytes) */
        void            *values;   /* Attribute values */
        struct cdAtt_n  *next;     /* Next Attribute */
} cdAtt_new;


/**********************************************************************/
                           /* Struct Header */
/**********************************************************************/


typedef struct cdHd_n {
        cdms_Id         id;        /* id identifier of this struct */
        cdms_Id         id_above;  /* id struct above this struct */
        void            *above;    /* struct above this struct */
} cdHd;


/**********************************************************************/
      /* Substitution Struct -- placeholder pointer of desired struct */
/**********************************************************************/


typedef struct cdTmp_n {
        cdms_Id         id;        /* id identifier of this struct */
        cdms_Id         id_above;  /* id struct above this struct */
        void            *above;    /* struct above this struct */
        struct cdTmp_n  *next;     /* next Tempory */
        cdms_Id         id_want;   /* id of wanted struct */
        char            *nam_want; /* name of wanted struct */
        void            *want;     /* address wanted struct */
} cdTmp;


/**********************************************************************/
      /* PQL List Selection Struct -- list of tree struct addresses */
/**********************************************************************/


typedef struct cdPql_n {
        cdms_Id         id;        /* id identifier of this struct */
        cdms_Id         id_above;  /* id struct above this struct */
        void            *above;    /* struct above this struct */
        struct cdPql_n  *next;     /* next List */
        char            *name;     /* Pql name */
        char            *pqlmsg;   /* user's input pql instruction */
        long            length;    /* length of user's list */
        long            *list;     /* list as user-ID's */
} cdPql;


/**********************************************************************/
          /* internal -- PQL read in sub-set of dataset variable */
/**********************************************************************/


typedef struct var_pql_read_n {
        struct cdVar_n  *var;     /* Variable struct */
        void            *data;    /* data array */
        char            *path;    /* input file directory */
        char            *file;    /* input file */

       /*------- Input file Variable info -------*/
        int             ndims;    /* Variable number of dimensions */
        cdType          vtyp;     /* Variable datatype */
        long            vlen;     /* Variable length */
        int             *dorder;  /* Variable dim transposing */

       /*------- Input file per-dimension Variable info -------*/
        struct cdDim_n  **dims;   /* Dimension struct */
        char            ***plst;  /* Pathlist attribute or NULL */
        char            ***flst;  /* Filelist attribute or NULL */
        int             **ilst;   /* Poslist attribute or NULL */
        int             **fpt;    /* Filepoint attribute or NULL */

        long            *index;   /* User want index */
        long            *count;   /* User want count */

        long            *rdidx;   /* Read file index */
        long            *rdcnt;   /* Read file count */
        long            *memidx;  /* Read file to memory array index */

       /* REMOVE FROM STRUCT   per-dimension info */
        long            *len;     /* Coordinate length */
        cdType          *typ;     /* Coordinate datatype */
        void            **cord;   /* Coordinate array */
} var_pql_read;


/**********************************************************************/
          /* internal -- PQL read in sub-set of dataset variable */
/**********************************************************************/


typedef struct dim_pql_read_n {
        struct cdDim_n  *dim;     /* Dimension struct */
        void            *data;    /* data array */

       /*------- Input file Dimension info -------*/
        cdType          dtyp;     /* Dimension datatype */
        long            index;    /* User want index */
        long            count;    /* User want count */
} dim_pql_read;


/**********************************************************************/
          /* internal -- PQL list of struct CDMS-tree-node addresses */
/**********************************************************************/


typedef struct cdms_pql_list_n {
        long      L_list;     /* physical size of list */
        long      length;     /* used length of list */
        void      **list;     /* list of struct addresses */


       /*------- where clause, input from user card -------*/
        int       flg_sid;    /* 1 =, 2 !=, 3 <, 4 <=, 5 >, 6 >= */
        long      tst_sid;    /* length test arg. */

        int       flg_nam;    /* 1 =, 2 !=, 3 <, 4 <=, 5 >, 6 >= */
        char      *tst_nam;   /* name test arg. */

        int       flg_len;    /* 1 =, 2 !=, 3 <, 4 <=, 5 >, 6 >= */
        long      tst_len;    /* length test arg. */

        int       flg_typ;    /* 1 =, 2 !=, 3 <, 4 <=, 5 >, 6 >= */
        char      *tst_typ;   /* type test arg. */

        int       flg_val;    /* 1 =, 2 !=, 3 <, 4 <=, 5 >, 6 >= */
        cdType    tst_vtyp;   /* type of the value test arg. */
        char      *tst_a_val; /* ascii value test */
        long      tst_i_val;  /* integer value test */
        double    tst_f_val;  /* floating value test */

        int       flg_dim;    /* 1 =, 2 !=, 3 <, 4 <=, 5 >, 6 >= */
        long      tst_dim;    /* ndim test arg. */

        int       flg_v1d;    /* 1 =, 2 !=, 3 <, 4 <=, 5 >, 6 >= */
                              /* variable name = dimension name test */

        int       flg_grp;    /* 1 =, 2 !=, 3 <, 4 <=, 5 >, 6 >= */
                              /* dataset used in f-spanning-group */


       /*------- where clause, input from tree-node -------*/
        long      nod_sid;    /* length test arg. */

        char      *nod_nam;   /* name test arg. */

        long      nod_len;    /* length test arg. */

        cdType    nod_typ;    /* type test arg. */

        cdType    nod_vtyp;   /* type of the value test arg. */
        void      *nod_val;   /* ascii value test */

        int       nod_dim;    /* variable number of dimensions */

        int       nod_v1d;    /* flag for variable==dimension */

        int       nod_grp;    /* flag for dataset==group */


       /*------- show section edit flags -------*/
        int       pflg;       /* (0,1) print title lines */
        int       cflg;       /* (0,1) print converted dim. values */
} cdms_pql_list;


/**********************************************************************/
          /* internal -- ascii metafile line, and line symbols */
/**********************************************************************/


typedef struct cdms_card_n {
        FILE   *fp;        /* file pointer */
        char   *eof;       /* return value from fgets() */
        int    len_line;   /* string length of ascii line */
        char   *asc_line;  /* ascii line */
        int    num_sym;    /* number of symbols */
        int    *idx_sym;   /* char-index of symbol */
        int    *len_sym;   /* char-length of symbol */
        int    *cls_sym;   /* classification of symbol */
        int    L_asc_line; /* memory size of asc_line */
        int    L_idx_sym;  /* memory size of idx_sym */
       /*---------------------------------
        * cls_sym[] -- classification flag identifying symbol
                    -- (integer of a letter)
             ( ) < > { } , : ; = " -- special letters
             'a' -- symbol, ascii name
             'i' -- symbol, integer number
             'f' -- symbol, floating number
             'q' -- ascii string contained within quotes "..."
        * idx_sym[] -- index of symbol in ascii input line
        * len_sym[] -- character length of symbol
        *---------------------------------*/
} cdms_card;


/**********************************************************************/
     /* internal -- User-Meta-Disc Variable correspondence */
/**********************************************************************/


#define D_max 8

typedef struct cdms_rd_var_n {
       /*--------------- disc_to_meta correspondence ---------------*/
        long            f_siz[D_max];    /* disc dimension sizes */
        long            f_len[D_max];    /* length index arrays */
        long            *f_out[D_max];   /* to_meta index arrays */

       /*--------------- user_to_meta correspondence ---------------*/
        long            u_pos[D_max];    /* user position in meta */
        long            u_siz[D_max];    /* user dimension sizes */
        long            u_len[D_max];    /* length index arrays */
        long            *u_in[D_max];    /* to_meta index arrays */
        struct cdDim_n  *u_dim[D_max];   /* user sub-dimensions */

       /*--------------- disc_to_user correspondence ---------------*/
        long            rd_len[D_max];   /* length index arrays */
        long            *rd_in[D_max];   /* read-in index array */
        long            *rd_out[D_max];  /* read-out array */

       /*--------------- general info                ---------------*/
        struct cdVar_n  *var;            /* meta variable struct */
        long            ndims;           /* meta var-dim count */
        long            m_siz[D_max];    /* meta var-dim sizes */
        struct cdDim_n  *m_dim[D_max];   /* meta var-dim structs */
        char            *rd_dir;         /* directory path */
        char            *rd_list;        /* list of filenames */
        long            nfil;            /* number of filenames */
        long            ifil;            /* idx current file */
        int             f_id;            /* cdunif file id */
        int             f_recdim;        /* cdunif file recdim */
        int             f_id_var;        /* cdunif file variable id */
        int             f_ndims;         /* cdunif file var dim count */
        int             f_dim[D_max];    /* cdunif file var dim id's */
        int             free_flg[8];     /* memory cleanup flags */
                     /* free_flg: 0 f_in, 1 f_out, 2 u_in, 3 u_out,
                        4 rd_in, 5 rd_out, 6 rd_list, 7 rd_dir */    
} cdms_rd_var;


/**********************************************************************/
      /* internal -- user directory-to-cdms-tree structure */
/**********************************************************************/


typedef struct cdms_dir_n {
        int             db_n;      /* number for db names */
        int             ds_n;      /* number for dset names */
        char            *aaflag;   /* ascii key for db,dset names */
        int             dirflag;   /* 0 dir -- db, 1 dir -- dset */
        int             numflag;   /* max no. levels to expand */
        int             scnflag;   /* 0 -- open file, 1 -- don't */
        int             ownflag;   /* file owner flag (0 no, 1 yes) */
        int             conflag;   /* 0 -- stop on bad file, 1 -- no */
        int             vmxflag;   /* get var. min-max (0 no, 1 yes) */
        int             tmxflag;   /* get time bgn-end (0 no, 1 yes) */
} cdms_dir;


/**********************************************************************/
      /* internal -- add sub-data-file to spanning file declaration */
/**********************************************************************/


typedef struct span_list_n {
        struct cdDb_n   *dbs;      /* db of first spaned file */
        int             ct;        /* calendar first file */

       /*--------------- dimension card info         ---------------*/
        char            *dname;    /* name spaning dimension */
        int             flg_dtyp;  /* 1 time '1980-1-1 0:0' on cards */
        int             select;    /* 0--pt1pt2 cord axis subset, */
                                   /* 1--idxidx, 2--idxcnt, */
                                   /* 3--offoff, 4--offcnt */
        char            *altf;     /* NULL or alterfile */
        int             lvlst;     /* length of vlst (from dim card) */
        char            **vlst;    /* NULL or variable list */

       /*--------------- filelist size arrays        ---------------*/
        int             L_flist;   /* size of per-file lists */
        int             n_flist;   /* number of files */
        char            **flist;   /* list of file names */
        char            **plist;   /* list of file paths */
        int             *fdoff;    /* list of file-dim offsets */

       /*--------------- coordinate size arrays      ---------------*/
        int             L_cord;    /* size of coord. arrays */
        int             n_cord;    /* number of coordinates */
        double          *scord;    /* span coordinate array */
        int             *fpoint;   /* filepoint span attribute */

       /*--------------- units,delta card info       ---------------*/
        int             flg_del;   /* flag, 1 if delta .cdms format */
        char            *uni;      /* units first file */
        int             flg_uni;   /* 1,2,3 mo,da,6h time flag */
        double          delt;      /* delta between coordinates */
} span_list;


/**********************************************************************/
      /* internal -- pql group instruction */
/**********************************************************************/


typedef struct cdms_group_n {
        cdVar_new       **vlist;   /* list of datasets */
        cdDim_new       **dlist;   /* list of dimensions */
        int             *index;    /* index to coordinates */
        int             *count;    /* number of coordinates */
        int             *order;    /* sort order position */
        double          *coord;    /* coordinates */
} cdms_group;


/**********************************************************************/
                          /* Check Var,Dim,Att Struct */
/**********************************************************************/


typedef struct cdCheck_n {
        cdms_Id          id;          /* id identifier of this struct */
        struct cdCheck_n *next;       /* Next Check struct */
        char             *name;       /* official name */
        int              L_alias;     /* size of **alias */
        int              nalias;      /* number of alias */
        char             **alias;     /* list of alias names */
        char             *tagname;    /* unique identifer name */
        int              L_card;      /* size of **card */
        int              ncard;       /* number of cards */
        char             **card;      /* list of cards */
        int              L_fstruct;   /* size of **fstruct */
        int              nfstruct;    /* number of struct */
        cdHd             **fstruct;   /* list of VAR,DIM,DSET struct */
} cdCheck;


/**********************************************************************/
                          /* Key Global per Database */
/**********************************************************************/


typedef struct cdDbFlag_n {
        cdms_Id     id;                    /* id identifier */
        int         L_multi;               /* size of arrays */
        int         *multi_ioc;            /* multi-file ioc's */
        cdDset_new  **multi_ds;            /* multi-file 1st var. */
        FILE        *FPT_OUT;              /* NULL or ioc of file */
        int         FPT_TTY;               /* flag: print to tty */
        int         ERR_no;                /* global error flag */
        char        *TempMount;            /* tempory mount point */
        int         EDIT_STRUCT_DATA_FLG;  /* edit struct or data */
        int         FPT_PY;                /* flag: print to python */
        int         qlerrflg;              /* flag: 'ql' routines */
        char        *PSQL_MSG;             /* message to print */
        char        *PY_PSQL_MSG;          /* message to python */
        int         l_PY_PSQL_MSG;         /* length of PY_PSQL_MSG */
        int         L_PY_PSQL_MSG;         /* size of PY_PSQL_MSG */
        int         l_ALT_PAT;             /* length of ALT_PAT */
        char        *ALT_PAT;              /* alter replace patern */
} cdDbFlag;


/**********************************************************************/
              /* Global scalars, arrays, statement functions */
/**********************************************************************/


#ifndef PSQL
cdDb_new   *DB_ROOT_ADR;           /* tree root address */
FILE       *FPT_OUT;               /* NULL or ioc of file */
int        FPT_TTY;                /* flag to print to tty */
int        ERR_no;                 /* global error flag */
char       *TempMount;             /* ascii tempory mount returned by pwd */
int        EDIT_STRUCT_DATA_FLG;   /* edit struct or data flag */
int        FPT_PY;                 /* flag: print to python */
char       *PSQL_MSG;              /* psql message to print */
char       *PY_PSQL_MSG;           /* psql message to send to python */
int        l_PY_PSQL_MSG;          /* length of PY_PSQL_MSG */
int        L_PY_PSQL_MSG;          /* size of PY_PSQL_MSG array */
#else
extern cdDb_new   *DB_ROOT_ADR;
extern FILE       *FPT_OUT;
extern int        FPT_TTY;
extern int        ERR_no;
extern char       *TempMount;
extern int        EDIT_STRUCT_DATA_FLG;
extern int        FPT_PY;
extern            *PSQL_MSG;
extern            *PY_PSQL_MSG;
extern            l_PY_PSQL_MSG;
extern            L_PY_PSQL_MSG;
#endif

#define pt_to_long(p,l) { void **v; v = (void *) &l; *v = (void *) p; }
#define long_to_pt(l,p) { void **v; v = (void *) &l, p = *v; }
#define err_x(a) { err_m(a); ERR_no = 1; return; }
#define err_t(a,b) { if( a ) err_x(b); }
#define err_r(void) { if( ERR_no ) return; }
#define err_i(a) { ERR_no = a; }
#define qlerr_x(a) { err_m(a); ERR_no = 0; return -1; }
#define qlerr_r(void) { if( ERR_no ) { ERR_no = 0; return -1; } }

/*.......error macros for functions of type long int.......*/
#define err_xl(a) { err_m(a); ERR_no = 1; return (long) 0; }
#define err_rl(void) { if( ERR_no ) return (long) 0; }
#define err_tl(a,b) { if( a ) err_xl(b); }

/*.......error macros for functions of type void* .......*/
#define err_xv(a) { err_m(a); ERR_no = 1; return NULL; }
#define err_rv(void) { if( ERR_no ) return NULL; }
#define err_tv(a,b) { if( a ) err_xv(b); }

/*.......err macros for functions of type int.......*/
#define err_xi(a) { err_m(a); ERR_no = 1; return 0; }
#define err_ri(void) { if( ERR_no ) return 0; }
#define err_ti(a,b) { if( a ) err_xi(b); }


/**********************************************************************/
                         /* Function prototypes */
/**********************************************************************/


/*..........bgn_extern..........*/
extern void alt_addatt( cdms_card *line,   
                 cdHd      *hd,     
                 int       idx,     
                 int       flg );
extern void alt_adddim( cdms_card *line,   
                 cdHd      *hd,     
                 int       flg );
extern void alt_addvar( cdms_card *line,   
                 cdHd      *hd,     
                 int       flg );
extern void alt_arg_pat( char *out,   
                  char *in );
extern void alt_att_concat( cdms_card *line,   
                     cdHd      *hd,     
                     int       idx );
extern void alt_delatt( cdms_card *line,   
                 cdHd      *hd );
extern void alt_del_node( cdAtt_new *att );
extern void alter_dataset( cdHd *cur );
extern void alt_midmonth( cdHd *hd,      
                   char *nmod,    
                   char *ncal,    
                   char *ntim );
extern void alt_mod_shape( cdAtt_new *att );
extern void alt_newtim( cdms_card *line,   
                 cdDim_new *dim,    
                 int       idx );
extern void alt_one_dim( cdDim_new *dim,   
                  cdCheck   *ck );
extern void alt_one_ds( cdDset_new *ds,    
                 cdCheck    *ck );
extern void alt_one_var( cdVar_new *var,   
                  cdCheck   *ck );
extern void alt_tim6h( cdHd *hd,      
                char *nmod,    
                char *ncal,    
                char *ntim );
extern long alt_values( cdms_card *line,   
                 int       idx,     
                 cdHd      *hd,     
                 long      len );
extern void alt_wrt_cards( cdDset_new *dset,    
                    char       *name );
extern void ary_min_max( cdType typ,     
                  long   len,     
                  void   *ary,    
                  double *min,    
                  double *max );
extern void *ary_off( cdType typ,     
               long   idx,     
               void   *ary );
extern void *ary_trans( cdType typ,        
                 long   len,        
                 void   *ary,       
                 cdType out_typ );
extern void att_to_dgv( cdDim_new  *dim,    
                 cdVar_new  *var );
extern void cdPqltty(void);
extern void check_dataset( cdHd *cur,     
                    int  pflg );
extern int check_filename( char *name );
extern int ck_lnk_list_att( cdAtt_new *att0,   
                     int       pflg );
extern int ck_lnk_list_dim( cdDim_new *dim0,   
                     int       pflg );
extern int ck_lnk_list_var( cdVar_new *var0,   
                     int       pflg );
extern void ck_nam_fnd( cdms_Id idwant,       
                 char    name[],       
                 char    tagname[],    
                 int     *num,         
                 cdCheck ***cklist );
extern int ck_one_att( cdAtt_new *att0,    
                cdCheck   *ck,      
                int        pflg );
extern int ck_one_dim( cdDim_new *dim,     
                cdCheck   *ck,      
                int        pflg );
extern int ck_one_var( cdVar_new *var,     
                cdCheck   *ck,      
                int        pflg );
extern void *copy_struct( cdHd *cur,    
                   cdHd *abv );
extern char *cr_asc_cpy( char *in );
extern void cr_att_cpy( cdAtt_new *atta,   
                 cdHd      *cur );
extern void *cre_struct( cdms_Id idwant,   
                  cdHd    *cur );
extern void delete_struct( cdHd *cur );
extern int dim_f1( int       idx,    
            cdms_card *line,  
            cdDim_new *dim );
extern int dim_f2( int       idx,      
            cdms_card *line,    
            cdDim_new *dim );
extern int dim_f3( int       idx,      
            cdms_card *line,    
            cdDim_new *dim );
extern void dir_file( cdms_dir *dir,    
               char     *path,   
               char     *name,   
               cdHd     *cur );
extern void dir_file_dim( cdms_dir   *dir,      
                   char       *path,     
                   char       *name,     
                   int        f_id,      
                   int        f_ndims,   
                   cdDset_new *ds );
extern void dir_file_noscan( cdms_dir *dir,    
                      char     *path,   
                      char     *name,   
                      cdHd     *cur );
extern cdHd *dir_struct( cdms_dir *dir,    
                  char     *path,   
                  cdHd     *hd,     
                  int      flg );
extern void dir_to_tree( cdms_dir *dir,    
                  int      lev,     
                  cdHd     *cur );
extern void edit_struct( cdHd *cur );
extern void edit_struct_att( cdAtt_new *att );
extern void edit_struct_dim( cdDim_new *dim,    
                      int       dedit );
extern void edit_struct_hdr( char name[] );
extern void edit_struct_num( char   name[],   
                      cdType typ,      
                      long   len,      
                      void   *ary );
extern void edit_struct_var( cdVar_new *var,    
                      int       dedit );
extern void *empty_struct( cdHd *cur );
extern int ending_quote( char *asc_line,   
                  int       idx1,   
                  int       idx2 );
extern int ending_star( char *asc_line,   
                 int       idx1,   
                 int       idx2 );
extern void err_cdms( char *in,   
               int  n );
extern void err_m( char *msg );
extern void fnd_dir_list( char ***nam,   
                   int  **flg,    
                   int  *num,     
                   int  scnflg );
extern void **fnd_lnk_list( cdms_Id idwant,   
                     cdHd    *cur );
extern void *get_coord( cdDim_new *dim,    
                 cdType    typ,     
                 long      *len );
extern char *get_cur_dir( void );
extern char *get_path_of_file( char *filename,   
                        char *mpath );
extern cdDb_new *init_user_db( void );
extern void ins_asym_eos( cdms_card  *line );
extern void load_ds_arrays( cdDset_new *dset );
extern void mem_cdms_card( cdms_card  **out1 );
extern void mem_cdms_pql_list( cdms_pql_list **out2 );
extern cdCheck *mem_check( cdCheck *cka,   
                    int     ncd,    
                    int     nal,    
                    int     nfs );
extern span_list *mem_span_list( span_list *spa,   
                          int       nfl,    
                          int       nsc );
extern void *mem_struct( cdms_Id idwant,   
                  cdHd    *cur );
extern void *meta_after_eq( int       idx,    
                     cdms_card *line,  
                     cdType    *typ,   
                     long      *num );
extern void *meta_att( cdms_card *line,   
                cdHd      *mas );
extern int meta_str_sym( cdms_card *line,  
                  int       idx,    
                  int       len );
extern void *nam_fnd( char    name[],   
               cdms_Id idwant,   
               cdHd    *cur );
extern void *old_att_name( cdHd *cur );
extern void old_py_name( cdms_card  *line );
extern int pql_alter( cdms_card     *line,   
               cdms_pql_list *pql,    
               int           idx );
extern int pql_check( cdms_card     *line,   
               cdms_pql_list *pql,    
               int           idx );
extern void pql_compress( cdms_pql_list *pql,      
                   int           idx,       
                   cdms_Id       idwant );
extern int pql_dirtree( cdms_card     *line,   
                 cdms_pql_list *pql,    
                 int           idx );
extern void pql_execute( char msg_ln[] );
extern int pql_filename( cdms_card     *line,   
                  cdms_pql_list *pql,    
                  int           idx );
extern cdPql *pql_fld1( cdms_card  *line,      
                 int        idx,        
                 int        *outidx );
extern int pql_fld2( cdms_card *line,      
              int       idx );
extern cdHd *pql_fld3( cdms_card *line,      
                int       idx,        
                int       *outidx );
extern int pql_fld4( cdms_card     *line,   
              cdms_pql_list *pql,    
              int           idx );
extern int pql_fld5( cdms_card *line,      
              int       idx,        
              int       *idx_w );
extern int pql_fld6( cdms_card     *line,   
              cdms_pql_list *pql,    
              int           idx );
extern int pql_gen_list( cdms_card     *line,   
                  cdms_pql_list *pql,    
                  int           i2,      
                  int           i3 );
extern int pql_group( cdms_card     *line,   
               cdms_pql_list *pql,    
               int           idx );
extern void pql_key_sym( cdms_card  *line );
extern int pql_lenreadbuf( cdms_card     *line,   
                    cdms_pql_list *pql,    
                    int           idx );
extern void pql_one_level( cdms_pql_list *pql,    
                    cdHd          *cur );
extern void pql_one_level_all( cdms_pql_list *pql,    
                        cdHd          *cur );
extern void pql_one_mem( cdms_pql_list *pql,   
                  int           num );
extern char *pql_rd_line( cdms_card *line,       
                   char      msg_ln[] );
extern int pql_read( cdms_card     *line,   
              cdms_pql_list *pql,    
              int           idx );
extern int pql_readalter( cdms_card     *line,   
                   cdms_pql_list *pql,    
                   int           idx );
extern int pql_readcheck( cdms_card     *line,   
                   cdms_pql_list *pql,    
                   int           idx );
extern void pql_read_cmp( var_pql_read *rd,     
                   void         *ary1,   
                   void         *ary2,   
                   cdType       typ );
extern void pql_read_dmem( var_pql_read *rd );
extern void pql_read_exp( var_pql_read *rd,     
                   void         *ary1,   
                   void         *ary2,   
                   cdType       typ );
extern void pql_read_fil( var_pql_read *rd );
extern long *pql_read_idx( var_pql_read *rd );
extern var_pql_read *pql_read_mem( cdVar_new *var );
extern int pql_readmeta( cdms_card     *line,   
                  cdms_pql_list *pql,    
                  int           idx );
extern int pql_read_sec( cdms_card     *line,    
                  cdms_pql_list *pql,     
                  var_pql_read  **outrd,  
                  int           idx );
extern void pql_read_var( var_pql_read *rd );
extern void pql_read_wrt( var_pql_read *rd,     
                   char         *nam );
extern int pql_release( cdms_card     *line,   
                 cdms_pql_list *pql,    
                 int           idx );
extern int pql_select( cdms_card     *line,   
                cdms_pql_list *pql,    
                int           idx );
extern int pql_show( cdms_card     *line,   
              cdms_pql_list *pql,    
              int           idx );
extern void pql_show_ary( cdType typ,      
                   long   len,      
                   void   *ary );
extern void pql_show_ed( int           cnt,     
                  cdms_card     *line,   
                  int           idxl1,   
                  int           idxl2,   
                  int           idxl3,   
                  cdms_pql_list *pql,    
                  int           idx1,    
                  int           idx2 );
extern int pql_show_sec( cdms_card     *line,   
                  cdms_pql_list *pql,    
                  int           idx );
extern void pql_show_value( cdms_pql_list *pql,   
                     int           idx );
extern int pql_subfgroup( cdms_card     *line,   
                   cdms_pql_list *pql,    
                   int           idx );
extern void pql_sublist( cdms_card     *line,    
                  cdms_pql_list *pql,     
                  int           idx,      
                  int           idxl1,    
                  int           idxl2 );
extern void pql_tree( cdms_pql_list *pql,     
               cdms_Id       idwant,   
               int           tflg,     
               cdHd          *cur );
extern int pql_union( cdms_card     *line,   
               cdms_pql_list *pql,    
               int           idx );
extern int pql_virtual( cdms_card     *line,   
                 cdms_pql_list *pql,    
                 int           idx );
extern int pql_where( cdms_card     *line,   
               cdms_pql_list *pql,    
               int           idx );
extern int pql_whr_1test( cdms_pql_list *pql );
extern int pql_whr_dtest( cdms_pql_list *pql );
extern int pql_whr_gtest( cdms_pql_list *pql );
extern int pql_whr_itest( cdms_pql_list *pql );
extern int pql_whr_ltest( cdms_pql_list *pql );
extern void pql_whr_node( cdms_pql_list *pql,    
                   int           idx );
extern int pql_whr_ntest( cdms_pql_list *pql );
extern int pql_whr_test( cdms_pql_list *pql,     
                  int           idx1,     
                  int           idx2 );
extern int pql_whr_ttest( cdms_pql_list *pql );
extern int pql_whr_vtest( cdms_pql_list *pql );
extern int pql_with( cdms_card     *line,   
               cdms_pql_list *pql,    
               int           idx );
extern int pql_writeds( cdms_card     *line,   
                 cdms_pql_list *pql,    
                 int           idx );
extern int pql_writemeta( cdms_card     *line,   
                   cdms_pql_list *pql,    
                   int           idx );
extern void pro_data_lines( cdms_card *line,   
                     cdHd      *mas );
extern cdDb_new *pro_db_lines( cdms_card *line,   
                        cdHd      *mas );
extern void pro_dim_lines( cdms_card  *line,   
                    cdHd       *mas );
extern cdDset_new *pro_dset_lines( cdms_card *line,   
                            cdHd      *mas );
extern void pro_tmp_att( void );
extern void pro_var_lines( cdms_card *line,   
                    cdHd      *mas );
extern char *py_psql_execute( long ldb,         
                       char msg_ln[] );
extern long py_psql_init( void );
extern int qlattget( int   fileid,     
              int   varid,      
              char  *name,      
              void  *values );
extern int qlattinq( int     fileid,      
              int     varid,       
              char    *name,       
              CuType  *datatype,   
              int     *len );
extern int qlattname( int   fileid,   
               int   varid,    
               int   attnum,   
               char  *name );
extern int qlclose( int  fileid );
extern cdDim_new *ql_dim_fnd( int  fileid,    
                       int  varid,     
                       int  dimid,     
                       char *name,     
                       int  *n_dim );
extern int qldimget( int   fileid,     
              int   dimid,      
              void  *values );
extern int qldimid( int   fileid,   
             int   varid,    
             char  *name );
extern int qldiminq( int        fileid,      
              int        dimid,       
              char       *dimname,    
              char       *dimunits,   
              CuType     *datatype,   
              CuDimType  *dimtype,    
              int        *varid,      
              long       *length );
extern int qlinquire( int fileid,     
               int *ndims,     
               int *nvars,     
               int *natts,     
               int *recdim );
extern int qlintr( char msg_ln[] );
extern int qlopenread( int   fileid,        
                char *controlpath,   
                char *datapath );
extern int qlseterropts( int  err_flg );
extern cdVar_new *ql_var_fnd( int  fileid,    
                       int  varid,     
                       char *name,     
                       int  *n_var );
extern int qlvarget( int   fileid,    
              int   varid,     
              long  start[],   
              long  count[],   
              void  *value );
extern int qlvarid( int  fileid,   
             char *name );
extern int qlvarinq( int     fileid,      
              int     varid,       
              char    *name,       
              CuType  *datatype,   
              int     *ndims,      
              int     dimids[],    
              int     *natts );
extern void rd_alt_dim_mod( dim_pql_read *rd,      
                     char         *name );
extern void rd_alt_dim_tran( dim_pql_read *rd,     
                      long         *idx );
extern int rd_alter_file( char *filename );
extern void rd_alt_var_mod( var_pql_read *rd,      
                     char         *name );
extern long *rd_alt_var_ridx( var_pql_read *rd );
extern long *rd_alt_var_tidx( var_pql_read *rd );
extern void rd_alt_var_tran( var_pql_read *rd,     
                      long         *idx );
extern int rd_check_file( char *filename );
extern void *rd_dim_array( cdDim_new *dim,     
                    long      start,    
                    long      count );
extern void *rd_dim_coord( dim_pql_read *rd );
extern int rd_meta_file( char *filename );
extern int rd_meta_line( cdms_card *line );
extern void rd_small_dim_card( cdms_card *line,    
                        span_list *span );
extern void rd_small_file( cdms_card *line,    
                    char      *path,    
                    char      *fall );
extern void *rd_var_array( cdVar_new *var,       
                    long      start[],    
                    long      count[] );
extern void rel_cdms_card( cdms_card  *line );
extern void rel_cdms_pql_list( cdms_pql_list *pql );
extern void rel_check( cdCheck  *cka );
extern void *scn_lnk_list( char    name[],   
                    cdms_Id idwant,   
                    cdHd    *cur );
extern void scn_small_alterf( cdDset_new *ds,       
                       char       *altf,     
                       char       *mpath );
extern void scn_small_db( span_list *span,   
                   cdDb_new  *db,     
                   char      *nam,    
                   char      *pth );
extern void scn_small_file( cdms_card *line,     
                     span_list *span,     
                     char      *mpath );
extern char *typ_as_ascii( cdType  dtype );
extern cdType typ_from_ascii( char *aa );
extern void typ_from_cdunif( CuType dtype,   
                      cdType *typ,    
                      int    *len );
extern int typ_of_calendar( char *aa );
extern int typ_time_dim( char *dnam );
extern void typ_to_cdunif( cdType dtype,    
                    CuType *typ,     
                    int    *len );
extern void typ_to_netcdf( cdType  dtype,    
                    nc_type *typ,     
                    int     *len );
extern void wrt_ds_to_netcdf( cdDset_new *dset,   
                       char       *nam,    
                       char       *tim );
extern void wrt_msg( void );
extern void wrt_small_file( cdHd *hd,      
                     char *unam );
/*..........end_extern..........*/
#endif
