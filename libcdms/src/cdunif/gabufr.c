/* Copyright (C) 1988-2010 by Brian Doty and the 
   Institute of Global Environment and Society (IGES).  
   See file COPYRIGHT for more information.   */

/* Authored by Joe Wielgosz */

#include <stdio.h>
#include <stdlib.h>
#include <errno.h>
#include <string.h>
#include <limits.h>
#include <float.h>

#include "gabufr.h"
#include "grads.h"

#ifndef GABUFR_DEBUG
#define GABUFR_DEBUG 0
#endif

#define GABUFR_NO_PARSE_DATA 0
#define GABUFR_PARSE_DATA 1

typedef struct {
  gabufr_msg * msg; /* msg being parsed */
  gabufr_varid * descpos; /* current descriptor */
  gabufr_varid * parentpos; /* descriptor in next level out (needed
			       for NCEP use of a sequence to indicate
			       replication of next descriptor */
  unsigned char * datapos; /* position in raw data */
  gaint databits;   /* bit offset in raw data */
  gaint z;  /* current replication count in outermost level */
  gaint sub; /* current subset */
  gaint delrep; /* flag set if previous descriptor was delayed
		 replication (F=2,Y=0) */
  gabufr_val * curval; /* end of list of parsed data values */
} gabufr_parseinf;


void gabufr_free_val(gabufr_val * val) {
  if (val->sval) {
    free(val->sval);
  }
  free(val);
}
  

void gabufr_free_msg(gabufr_msg * msg) {
  gabufr_val * val, * nextval;
  gaint i;
  if (msg->subs) {
    for (i = 0; i < msg->subcnt; i++) {
      if (msg->subs[i]) {
	for (val = msg->subs[i]; val != NULL; val = nextval) {
	  nextval = val->next;
	  gabufr_free_val(val);
	}
      }
    }
    free(msg->subs);
  }
  free(msg);
}  
 
void gabufr_close(gabufr_dset * dset) {
  gabufr_msg * msg, * nextmsg;

  for (msg = dset->msgs; msg != NULL; msg = nextmsg) {
    nextmsg = msg->next;
    gabufr_free_msg(msg);
 }

  if (dset->buf) {
    printf("should have been done already");
    free(dset->buf);
  }

  free(dset);
}

/* loads file contents into a memory block */
gaint gabufr_load2mem(const char * path, gabufr_dset * dset) {
  off_t bytesread, bytesleft, filesize;
  FILE * datafile;
  unsigned char * filebuf;
  unsigned char * pos;

  if ((datafile = fopen(path, "r")) == NULL) {
      printf("Can't open BUFR file %s: %s\n", path, strerror(errno));
      return GABUFR_ERR;
  }

  fseeko(datafile, 0, SEEK_END);
  bytesleft = filesize = ftello(datafile);
  fseeko(datafile, 0, SEEK_SET);
  if (GABUFR_DEBUG) printf("filesize is %lld\n", filesize);

  pos = filebuf = malloc(filesize);
  if (filebuf == NULL) {
    printf("Couldn't allocate memory for file parsing\n");
    return GABUFR_ERR;
  }

  while (bytesleft > 0) {
    bytesread = fread(pos, 1, bytesleft, datafile);
    if (GABUFR_DEBUG) printf("read %lld bytes\n", bytesleft);
    if (ferror(datafile)) {
      printf("Low level read error on BUFR file %s\n", path);
      free(filebuf);
      fclose(datafile);
      return GABUFR_ERR;
    }
    
    if (feof(datafile) && bytesleft) {
      if (GABUFR_DEBUG) printf("Ran out of data in BUFR file %s!\n", path);
      free(filebuf);
      fclose(datafile);
      return GABUFR_ERR;
    }

    bytesleft -= bytesread;
    pos += bytesread;
  }
  fclose(datafile);
  dset->buf = filebuf;
  dset->len = filesize;
  dset->msgs = NULL;
  return GABUFR_OK;
}  

/* prints a full description of a varid, expanding sequence contents*/
void gabufr_print_varid (gabufr_varid *varid, gaint indent) {
  gabufr_varid * seq_varid;
  gabufr_varinf * varinf;
  gaint i;
  static gaint delrep = 0;

  for (i = 0; i < indent; i++) {
    printf(" ");
  }

  printf("%d %.2d %.3d  ", 
	 varid->f, varid->x, varid->y);
  
  if (delrep) {
    printf("(replication count)\n");
    delrep = 0;
    return;
  }
  switch (varid->f) {

  case 0:
    varinf = gabufr_get_varinf(varid->x, varid->y);
    printf("(%s) %s\n",
	   (varinf->datatype == GABUFR_STR_TYPE) ? "text" : "numeric",
	   varinf->description);
    break;

  case 1:
    printf("(replicate next %d", varid->x);
    if (varid->y == 0) {
      printf(", not including replication count)\n");
      delrep = 1;
    } else {
      printf(", %d times)\n", varid->y);
    }
      
    break;

  case 2:
    printf("(operator)\n");
    break;

  case 3:
    printf("(sequence)\n");
    for (seq_varid = gabufr_get_seq(varid->x, varid->y); 
	 seq_varid; 
	 seq_varid = seq_varid->next) {
      gabufr_print_varid(seq_varid, indent + 2);
    }
    break;
  }


}

/* builds a list of varid's from the message header */
gabufr_varid * gabufr_extract_msg_desc(gabufr_msg * msg, gaint parse_data) {
  unsigned char * pos, * start, * end;
  gabufr_varid * head, * current, * next;
  gabufr_varinf * varinf;

  if (msg == NULL) {
    return NULL;
  }

  if (GABUFR_DEBUG || parse_data == GABUFR_NO_PARSE_DATA)  {
    printf("\n\n\n>>> start of message\n");
  }

  head = current = NULL;
  start = msg->section3 + 7;
  end = msg->section4;
  for (pos = start; pos < end; pos += 2) {
    next = (gabufr_varid *) malloc(sizeof(gabufr_varid));
    if (next == NULL) {
      printf("Memory allocation failed during parsing\n");
      gabufr_free_varids(head);
      return NULL;
    }
      
    next->f = gagbb(pos, 0, 2);
    next->x = gagbb(pos, 2, 6);
    next->y = gagbb(pos, 8, 8);
    
    if (next->f == 0 && next->x == 0 && next->y == 0) {
      free(next);
      continue;
    }

    next->next = NULL;

    if (!gabufr_valid_varid(next->f, next->x, next->y)) {
      printf("error: corrupt message (contains invalid FXY %d-%.2d-%.3d)\n", 
	     next->f, next->x, next->y);
      free(next);
      gabufr_free_varids(head);
      return NULL;
    }

    if (next->f == 0) {
      varinf = gabufr_get_varinf(next->x, next->y);
      if (varinf->width == 0) {
	printf("error: no table information for FXY %d-%.2d-%.3d\n", 
	       next->f, next->x, next->y); 
	free(next);
	gabufr_free_varids(head);
	return NULL;
      }
    }

    if (GABUFR_DEBUG || parse_data == GABUFR_NO_PARSE_DATA) gabufr_print_varid(next, 0);

    if (head) {
      current->next = next;
    } else {
      head = next;
    }
    current = next;
  }

  if (GABUFR_DEBUG || parse_data == GABUFR_NO_PARSE_DATA)  {
    printf("\n<<< end of message");
  }

  return head;
}

void gabufr_seekbits(gabufr_parseinf *inf, gaint bits) {
  inf->databits += bits;
  inf->datapos += (inf->databits / 8);
  inf->databits %= 8;
  /* if (GABUFR_DEBUG) printf("set position to %p (offset %d)\n", 
     inf->datapos, inf->databits); */
}  

gaint gabufr_all_ones(gaint bitcnt) {
  return (1 << bitcnt) - 1;
}

long gabufr_readbits2num(unsigned char * pos, gaint offset, gaint bitcnt) {
  long retval;
  if (bitcnt <= 0 || bitcnt > sizeof(long) * 8) {
    printf("warning: can't read %d-bit data value; max is %d\n",
	   bitcnt, (gaint) (sizeof(long) * 8));
    return gabufr_all_ones(bitcnt);
  } else {
    retval = gagbb(pos, offset, bitcnt);
    if (GABUFR_DEBUG) printf("read %2d bits at (%p + %d):  %ld\n", 
			     bitcnt, pos, offset, retval);
    return retval;
  }
}

char * gabufr_readbits2str(unsigned char * pos, gaint offset, gaint bitcnt) {
  gaint i, bytecnt;
  char * retval;
  if (bitcnt < 0 || bitcnt % 8) {
    printf("error: invalid bit count for string: %d\n", 
	   bitcnt);
    return NULL;
  }
  bytecnt = bitcnt / 8;

  if (GABUFR_DEBUG) printf("read %2d chars at (%p + %d): [", 
	 bytecnt, pos, offset);
  retval = (char *) malloc(bytecnt+1);
  if (retval == NULL) {
    printf("Memory allocation failed during parsing\n");
    return NULL;
  }
  if (offset) {
    for (i = 0; i < bytecnt; i++) {
      retval[i] = (char) gagbb(pos + i, offset, 8);
    }
  } else {
    memcpy(retval, pos, bytecnt);
  }
  retval[bytecnt] = '\0';
  if (GABUFR_DEBUG) printf("%s]\n", retval);
  return retval;
}

double gabufr_exp10(double mant, gaint exp) {
  gaint i;
  if (exp > 0) {
    for (i = 0; i < exp; i++) {
      mant /= 10;
    }
  } else {
    for (i = 0; i > exp; i--) {
      mant *= 10;
    }
  }    
  return mant;
}

gaint gabufr_parseval(gabufr_parseinf *inf, gaint x, gaint y, gabufr_val * val) {
  gabufr_varinf * varinf;
  long packedval;

  varinf = gabufr_get_varinf(x, y);
  if (varinf->width == 0) {
    printf("error: no entry for descriptor (0, %d, %d)\n", x, y);
    return GABUFR_ERR;
  }

  if (varinf->datatype == GABUFR_STR_TYPE) {
    val->sval = 
      gabufr_readbits2str(inf->datapos, inf->databits, varinf->width);
    val->val = DBL_MIN;
  } else {
    packedval = 
      (double) gabufr_readbits2num(inf->datapos, inf->databits, varinf->width);

    if (packedval == gabufr_all_ones(varinf->width)) {
	val->undef = GABUFR_UNDEF;
	if (GABUFR_DEBUG) printf("missing data flag set\n");
      } else {
	val->undef = GABUFR_DEF;
      }

    val->val = gabufr_exp10(packedval + varinf->offset, varinf->scale);
    if (GABUFR_DEBUG) printf("unpacking: ( %d + %d ) / 10^%d -> %g\n",
			     (gaint) packedval, 
			     varinf->offset, 
			     varinf->scale, 
			     val->val);
    val->sval = NULL;
  } 
  
  val->x = x;
  val->y = y;
  val->z = inf->z;
  gabufr_seekbits(inf, varinf->width);
  return GABUFR_OK;
}

/* adds a new value to the current linked list, creating it if necessary */
void gabufr_addval(gabufr_parseinf *inf, gabufr_val *val) {
  if (! inf->curval) {
    inf->msg->subs[inf->sub] = val;
  } else {
    inf->curval->next = val;
  }    
  val->next = NULL;
  inf->curval = val;
}

gaint gabufr_parsedesc(gabufr_parseinf * inf, gaint f, gaint x, gaint y); 

/* reads data associated with a list of (f,x,y) descriptors */
gaint gabufr_parselist(gabufr_parseinf * inf, gabufr_varid * list) {
  gabufr_varid * saved;
  saved = inf->descpos;
  for(inf->descpos = list; 
      inf->descpos; 
      inf->descpos = inf->descpos->next) {
    if (gabufr_parsedesc(inf, inf->descpos->f, inf->descpos->x, 
			 inf->descpos->y) == GABUFR_ERR) {
      return GABUFR_ERR;
    }
  }
  inf->descpos = saved;
  return GABUFR_OK;
}  

/* performs replication - parses the next numdesc descriptors in the list, numreps times */
gaint gabufr_replicate(gabufr_parseinf * inf, gaint numdesc, gaint numreps) {
  gaint i, z, nestedrep;
  gabufr_varid * base, * pos, * end, ** pos_addr;

  if (GABUFR_DEBUG) printf("**** replicating %d descriptors %d times\n", 
			   numdesc, numreps);

    /* NCEP has sequences that just contain a replication factor,
     * which are supposed to apply to the id that follows after that
     * sequence.  so we may be inside a sequence with no more id's in
     * the list.  Thus, we either increment the pointer for the
     * current list, or the id pointer for the parent list, depending.
     */
  if (inf->descpos->next) {
    if (GABUFR_DEBUG) printf("using descpos (currently %p)\n", inf->descpos);
    pos_addr = &inf->descpos;
  } else {
    if (GABUFR_DEBUG) printf("using parentpos (currently %p)\n", inf->descpos);
    pos_addr = &inf->parentpos;
  }

  pos = base = (*pos_addr);
  for (i = 0; i < numdesc; i++) {
      pos = pos->next;
      if (!pos) {
	printf("error: ran out of descriptors to replicate!\n");
	return GABUFR_ERR;
      }
  }
  end = pos;

  nestedrep = (inf->z >= 0);
  if (nestedrep) {
    /* we handle nested rep fine, except that we don't print out the replication counts */
    if (GABUFR_DEBUG) printf("warning: nested replication in dataset\n");
  }

  for (z = 0; z < numreps; z++) {
    if (GABUFR_DEBUG) printf("\n** rep = %d of %d\n", z, numreps);

    /* increment global var during looping, so that nested loops start 
     *  from the right place */
    (*pos_addr) = base;

    if (!nestedrep) {
      inf->z = z;
    }

    while ((*pos_addr) != end) {
      (*pos_addr) = (*pos_addr)->next;
      if (GABUFR_DEBUG) printf("descpos=%d-%d-%d\n", 
			       (*pos_addr)->f, (*pos_addr)->x, (*pos_addr)->y);
      if (gabufr_parsedesc(inf, (*pos_addr)->f, (*pos_addr)->x, 
			   (*pos_addr)->y) == GABUFR_ERR) {
	return GABUFR_ERR;
      }
    }
  }

  if (!nestedrep) {
    inf->z = -1;
  }

  /* Move pointer to end of replicated descriptors */ 
  (*pos_addr) = end;

  if (GABUFR_DEBUG) printf("**** done replicating %d descriptors %d times\n\n",
			   numdesc, numreps);
  return GABUFR_OK;
}

gaint gabufr_parse_f0(gabufr_parseinf * inf, gaint x, gaint y) {
  gabufr_val * val;
  gabufr_val delrepval;
  gabufr_varinf * varinf;
  gaint numdesc, numreps;

  if (x == 0 && y == 0) {
    if (GABUFR_DEBUG) printf("null descriptor\n");
    return GABUFR_OK;
  } else {
    varinf = gabufr_get_varinf(x, y);
    if (GABUFR_DEBUG) printf("\t%s\n", varinf->description);
  }

  if (inf->delrep) {
    if ( x != 31 ) {
      printf("error: expected F=0 Y=31 X=... for delayed replication\n");
      return GABUFR_ERR;
    }
    if (gabufr_parseval(inf, x, y, &delrepval) == GABUFR_ERR) {
      return GABUFR_ERR;
    }
    numreps = delrepval.val;
    numdesc = inf->delrep;
    inf->delrep = 0;
    if (gabufr_replicate(inf, numdesc, numreps) == GABUFR_ERR) {
      return GABUFR_ERR;
    }
  } else {
    val = (gabufr_val *) malloc(sizeof(gabufr_val));
    if (val == NULL) {
      printf("Memory allocation failed during parsing\n");
      return GABUFR_ERR;
    }
    if (gabufr_parseval(inf, x, y, val) == GABUFR_ERR) {
      return GABUFR_ERR;
    }
    gabufr_addval(inf, val);
  }
  return GABUFR_OK;
}

gaint gabufr_parse_f1(gabufr_parseinf * inf, gaint x, gaint y) {
  gaint numdesc, numreps;

  if (GABUFR_DEBUG) printf("\n\n");
  numdesc = x;
  if (y > 0) {
    if (GABUFR_DEBUG) printf("**** normal replication\n");
    numreps = y;
    if (gabufr_replicate(inf, numdesc, numreps) == GABUFR_ERR) {
      return GABUFR_ERR;
    }
  } else {
    if (GABUFR_DEBUG) printf("**** delayed replication\n");
    inf->delrep = x;
  }
  return GABUFR_OK;
}


gaint gabufr_parse_f2(gabufr_parseinf * inf, gaint x, gaint y) {
  gabufr_val * val;
  /*  gaint width; if we decide to skip local fields */

  switch (x) {
  case 4: /* associated field */
    if (GABUFR_DEBUG) printf("reading %d-bit associated field\n", y);
    val = (gabufr_val *) malloc(sizeof(gabufr_val));
    if (val == NULL) {
      printf("Memory allocation failed during parsing\n");
      return GABUFR_ERR;
    }
    val->x = -1;
    val->y = -1;
    val->z = inf->z;
    val->val = gabufr_readbits2num(inf->datapos, inf->databits, y);
    val->sval = NULL;
    gabufr_seekbits(inf, y);
    gabufr_addval(inf, val);
    break;

  case 5: /* associated string */
    if (GABUFR_DEBUG) printf("reading %d-byte associated string\n", y);
    val = (gabufr_val *) malloc(sizeof(gabufr_val));
    if (val == NULL) {
      printf("Memory allocation failed during parsing\n");
      return GABUFR_ERR;
    }
    val->x = -1;
    val->y = -1;
    val->z = inf->z;
    val->sval = gabufr_readbits2str(inf->datapos, inf->databits, y * 8);
    if (val->sval == NULL) {
      return GABUFR_ERR;
    }
    val->val = DBL_MIN;
    gabufr_seekbits(inf, y * 8);
    gabufr_addval(inf, val);
    break;

  case 6: /* local field length */
    /*
      width = y;
    inf->descpos = inf->descpos->next;
    if (GABUFR_DEBUG) printf("skipping %d-bit local field F=%d X=%d Y=%d\n",
	   y, inf->descpos->f, inf->descpos->x, inf->descpos->y);
    gabufr_seekbits(inf, y);
    */
    break;

  default:
    printf("warning: ignoring unsupported operator F=2 X=%d Y=%d\n", x, y);
  }

  return GABUFR_OK;

}

gaint gabufr_parse_f3(gabufr_parseinf * inf, gaint x, gaint y) {
  gabufr_varid * saved;
  if (GABUFR_DEBUG) 
    printf("\n==== recursing into table entry for F=3 X=%d Y=%d\n", x, y);
  /*
  table_d_entry = gabufr_get_seq(x, y);
  if (table_d_entry->f == 1 && 
      table_d_entry->y == 0 &&
      table_d_entry->next->next == NULL) {
  */
    
  saved = inf->parentpos;
  inf->parentpos = inf->descpos;
  for (inf->descpos = gabufr_get_seq(x, y); 
       inf->descpos; 
       inf->descpos = inf->descpos->next) {
    if (gabufr_parsedesc(inf, inf->descpos->f, inf->descpos->x, 
			 inf->descpos->y) == GABUFR_ERR) {
      return GABUFR_ERR;
    }
  }
  if (GABUFR_DEBUG) printf("==== finished F=3 X=%d Y=%d\n\n", x, y);
  inf->descpos = inf->parentpos;
  inf->parentpos = saved;

  return GABUFR_OK;
}

gaint gabufr_parsedesc(gabufr_parseinf * inf, gaint f, gaint x, gaint y) {
  gaint rc = GABUFR_OK;
  if (GABUFR_DEBUG) printf("descriptor: (%d, %d, %d)\n", f, x, y);
  switch (f) {
  case 0: 
    rc = gabufr_parse_f0(inf, x, y); 
    break;
  case 1: 
    rc = gabufr_parse_f1(inf, x, y); 
    break;
  case 2: 
    rc = gabufr_parse_f2(inf, x, y); 
    break;
  case 3: 
    rc = gabufr_parse_f3(inf, x, y); 
    break;
  }
  return rc;
}


/* parses the raw data for a msg into a list of val structures */
gaint gabufr_parsevals(gabufr_msg * msg, gaint parse_data) {
  gabufr_parseinf inf;
  gabufr_varid * msg_descs;
  gaint extra;

  inf.delrep = 0;
  inf.z = -1;
  inf.datapos = msg->section4 + 4;  
  inf.databits = 0;
  inf.msg = msg;
  msg_descs = gabufr_extract_msg_desc(msg, parse_data);
  if (msg_descs == NULL) {
    return GABUFR_ERR;
  }

  if (parse_data == GABUFR_PARSE_DATA || msg->is_new_tbl) {
    for (inf.sub = 0; inf.sub < msg->subcnt; inf.sub++) {
      if (GABUFR_DEBUG) printf("\n\n@@@ parsing subset %d @@@\n", inf.sub);
      inf.parentpos = NULL;
      inf.curval = NULL;
      if (gabufr_parselist(&inf, msg_descs) == GABUFR_ERR) {
	return GABUFR_ERR;
      }
    }
    if (GABUFR_DEBUG) printf("data position is (%p + %d); end of data is %p\n", 
			     inf.datapos, inf.databits, inf.msg->end);
    
    extra = inf.msg->end - inf.datapos;
    if (extra > 1) {
      printf("Corrupt message: %d extra bytes in data section\n", extra);
      return GABUFR_ERR;
    }
  } else {
    return GABUFR_ERR;
  }

  gabufr_free_varids(msg_descs);
  return GABUFR_OK;
}

/* reads header data (not the descriptor list but the one-off required
   fields) and creates a new message structure */
gabufr_msg * gabufr_parsehdr(unsigned char * section0) {
  gaint section2flag, century;
  unsigned char *section2;
  gabufr_msg * msg;

  msg = (gabufr_msg *) malloc(sizeof(gabufr_msg));
  if (msg == NULL) {
    printf("Memory allocation failed during parsing\n");
    return NULL;
  }
  
  msg->next = NULL;

  msg->section0 = section0;
  
  msg->section1 = msg->section0 + 8;
  section2flag = gagbb(msg->section1+7, 0, 1);
  if (section2flag) {
    if (GABUFR_DEBUG) printf("found msg->section 2\n");
    section2 = msg->section1 + gagby(msg->section1, 0, 3);
    msg->section3 = section2 + gagby(section2, 0, 3);
  } else {
    if (GABUFR_DEBUG) printf("no msg->section 2\n");
    msg->section3 = msg->section1 + gagby(msg->section1, 0, 3);
  }
 msg->section4 = msg->section3 + gagby(msg->section3, 0, 3);
 msg->end = msg->section4 + gagby(msg->section4, 0, 3);
 
 if (GABUFR_DEBUG) printf("sections: %p / %p / %p / %p (end %p)\n",
			  msg->section0, 
			  msg->section1, 
			  msg->section3, 
			  msg->section4,
			  msg->end); 
 
 if (GABUFR_DEBUG) printf("lengths: %d / %d / %d / %d (total %d)\n",
			  msg->section1 - msg->section0, 
			  msg->section3 - msg->section1, 
			  msg->section4 - msg->section3, 
			  msg->end      - msg->section4,
			  msg->end      - msg->section0); 

  msg->tbl_inf.bufr_edition = gagby(msg->section0, 7, 1);
  msg->tbl_inf.master_tbl_num = gagby(msg->section1, 3, 1);
  msg->tbl_inf.master_tbl_version = gagby(msg->section1, 10, 1);
  msg->tbl_inf.local_tbl_version = gagby(msg->section1, 11, 1);

  if (GABUFR_DEBUG) 
    printf ("edition: %d; master #: %d; master v: %d; local v: %d\n",
	    msg->tbl_inf.bufr_edition, 
	    msg->tbl_inf.master_tbl_num, 
	    msg->tbl_inf.master_tbl_version, 
	    msg->tbl_inf.local_tbl_version);

  msg->is_new_tbl = (gagby(msg->section1, 8, 1) == 11);

/* 
   Per Jack Woollen, the message section 1 date 
   has the year broken into two separate bytes. 
   Byte #13 contains the year of the century.
   Byte #18 contains the number of the century. 
   For 1999, the century would be 20, and the year of the century 99
   For 2000, the century would be 20, and the year of the century 100
   For 2003, the century would be 21, and the year of the century 1
*/

  msg->year = gagby(msg->section1, 12, 1);   
  msg->month = gagby(msg->section1, 13, 1);
  msg->day = gagby(msg->section1, 14, 1);
  msg->hour = gagby(msg->section1, 15, 1);
  msg->min = gagby(msg->section1, 16, 1);
  msg->subcnt = gagby(msg->section3, 4, 2);

  century = gagby(msg->section1,17,1);     
  msg->year = msg->year + ((century-1)*100);

  if (msg->subcnt) {
    msg->subs = (gabufr_val **) calloc(msg->subcnt, sizeof(gabufr_val *));
    if (msg->subs == NULL) {
      printf("Memory allocation failed during parsing\n");
      free(msg);
      return NULL;
    }
  } else {
    msg->subs = NULL;
  }
  if (GABUFR_DEBUG) printf("date: %.2d:%.2d %.2d-%.2d-%.2d   subsets: %d\n", 
			   msg->hour, msg->min, msg->month, msg->day, 
			   msg->year, msg->subcnt);


  return msg;
}

/* not currently used */
gaint gabufr_countmsgs(gabufr_dset *dset) {
  unsigned char * start, * end, * pos, * endofmsg;
  gaint msglen;
  gaint msgcnt;
  gabufr_msg * current, * next;

  msgcnt = 0;
  current = next = NULL;
  pos = start = dset->buf;
  end = (start + dset->len) - 4; /* stop 4 characters early so
				      * memcmp() doesn't run off the edge */
  while (pos < end) {

    /* search for next "BUFR" string */
    if( memcmp(pos, "BUFR", 4) == 0 ) {

      msglen = gagby(pos, 4, 3);
      /* if (GABUFR_DEBUG) 
	 printf("\n\n\n\nfound 'BUFR' at %p followed by length %d; ", 
	 pos, msglen); */
      endofmsg = (pos + msglen) - 4;

      if (memcmp(endofmsg, "7777", 4) == 0) {

	/* if (GABUFR_DEBUG) printf("confirmed end of message.\n"); */
	msgcnt++;
	pos = endofmsg + 4;
      } else {
	/* if (GABUFR_DEBUG) printf("no end of message! got %4c instead\n", 
	   endofmsg); */
      }
    }

    pos++;

  }
  printf("found %d messages in file\n", msgcnt);
  return msgcnt;
}

/* reads data out of a file into a series of message structures */
gaint gabufr_decode(gabufr_dset *dset, gaint parse_data) {
  unsigned char * start, * end, * pos, * endofmsg;
  gaint msglen;
  gabufr_msg * current, * next;

  /*  gabufr_countmsgs(dset); */

  dset->msgcnt = 0;
  current = next = NULL;
  pos = start = dset->buf;
  end = (start + dset->len) - 4; /* stop 4 characters early so
				      * memcmp() doesn't run off the edge */
  while (pos < end) {

    /* search for next "BUFR" string */
    if( memcmp(pos, "BUFR", 4) == 0 ) {

      msglen = gagby(pos, 4, 3);
      if (GABUFR_DEBUG) 
	printf("\n\n\n\nFound 'BUFR' at %p followed by length %d; ", 
	     pos, msglen);
      endofmsg = (pos + msglen) - 4;
      
      if (memcmp(endofmsg, "7777", 4) == 0) {

	if (GABUFR_DEBUG) printf("confirmed end of message.\n");
	next = gabufr_parsehdr(pos);
	if (next) {
	  next->fileindex = dset->msgcnt;

	  if (next->is_new_tbl) {
	    if (GABUFR_DEBUG) printf("msg %d contains a new BUFR table\n", 
				     dset->msgcnt);
	  } else {
	    if (GABUFR_DEBUG) printf("msg %d contains data\n",
				     dset->msgcnt);
	  }
	  
	  if (!gabufr_have_tbl(&next->tbl_inf)) {
	    if (gabufr_read_tbls(&next->tbl_inf) == GABUFR_ERR) {
	      return GABUFR_ERR;
	    }
	  }
	  
	  if (GABUFR_DEBUG) printf("%%%%%%%%%%%%%% processing message %d..\n", 
				   dset->msgcnt);
	  if (parse_data == GABUFR_NO_PARSE_DATA)  {
	    printf("\n\n\n>>> processing message %d\n", dset->msgcnt);
	  }
	  
	  if (gabufr_parsevals(next, parse_data) == GABUFR_OK) {

	    if (next->is_new_tbl) {
	      gabufr_update_ncep_tbl(dset, next);
	    }
	    
	    if (! current) {
	      dset->msgs = next;
	    } else {
	      current->next = next;
	    }
	    
	    current = next;
	    dset->msgcnt++;

	  } else {
	    gabufr_free_msg(next);
	  }

	}

	pos = endofmsg + 4;

      } else {
	if (GABUFR_DEBUG) printf("no end of message! got %c%c%c%c instead\n", 
				 *endofmsg,
				 *(endofmsg+1),
				 *(endofmsg+2),
				 *(endofmsg+3));
      }
    }

    pos++;

  }

  return GABUFR_OK;
}

gabufr_dset * gabufr_open(const char * path) {
  gabufr_dset * dset;

  dset = (gabufr_dset *) malloc(sizeof(gabufr_dset));
  if (dset == NULL) {
    printf("Memory allocation failed during parsing\n");
    return NULL;
  }
  gabufr_reset_tbls();
  if (gabufr_load2mem(path, dset) == GABUFR_ERR) {
    return NULL;
  }
  if (gabufr_decode(dset, GABUFR_PARSE_DATA) == GABUFR_ERR) {
    free(dset->buf);
    free(dset);    
    return NULL;
  }
  free(dset->buf);
  dset->buf = NULL;
  return dset;
}

gabufr_dset * gabufr_scan(const char * path) {
  gabufr_dset * dset;

  dset = (gabufr_dset *) malloc(sizeof(gabufr_dset));
  if (dset == NULL) {
    printf("Memory allocation failed during parsing\n");
    return NULL;
  }
  gabufr_reset_tbls();
  if (gabufr_load2mem(path, dset) == GABUFR_ERR) {
    return NULL;
  }
  if (gabufr_decode(dset, GABUFR_NO_PARSE_DATA) == GABUFR_ERR) {
    free(dset->buf);
    free(dset);    
    return NULL;
  }
  free(dset->buf);
  dset->buf = NULL;
  return dset;
}
