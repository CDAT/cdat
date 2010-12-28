/*  Copyright (C) 1988-2010 by Brian Doty and the 
    Institute of Global Environment and Society (IGES).  
    See file COPYRIGHT for more information.   */

/* Authored by Joe Wielgosz */

#include <stdio.h>
#include <errno.h>
#include <string.h>
#include <stdlib.h>
#include <limits.h>
#include "gabufr.h"

#ifndef GABUFR_TBL_DEBUG
#define GABUFR_TBL_DEBUG 0
#endif

const char* base_path;

/* Size of buffer for each line of the table text file */
#define GABUFR_MAX_LINE_LEN 4096

/* Used in parsing */
#define GABUFR_FIRST_PASS  0
#define GABUFR_SECOND_PASS 1

/* Length of a filename for a MEL-style plain text BUFR table */
#define GABUFR_TBL_NAME_LEN 14

/* Static storage for currently loaded tables B and D */
gabufr_varinf * tbl_b = NULL;
gabufr_varid ** tbl_d_entries = NULL;

void gabufr_set_tbl_base_path(const char * path) {
  base_path = path;
}

gaint gabufr_valid_varid(gaint f, gaint x, gaint y) {
  return ((f >= 0 && f <= 3)
	  && (x >= 0 && x < 64)
	  && (y >= 0 && y < 256));
}

gaint gabufr_tbl_index(gaint x, gaint y) {
  return (x << GABUFR_Y_BITS) +  y;
}

gabufr_varinf * gabufr_get_varinf(gaint x, gaint y) {
  if (!tbl_b) {
    return NULL;
  } else {
    return &tbl_b[gabufr_tbl_index(x, y)]; 
  }
}

gabufr_varid * gabufr_get_seq(gaint x, gaint y) {
  return tbl_d_entries[gabufr_tbl_index(x, y)]; 
}

gaint gabufr_have_tbl(gabufr_tbl_inf * tbl_inf) {
  return (tbl_b && tbl_d_entries);
}

void gabufr_free_varids(gabufr_varid * entry) {
  gabufr_varid * next;
  while (entry) {
    next = entry->next;
    free(entry);
    entry = next;
  }
}

void gabufr_reset_tbls() {
  gaint i;

  if (tbl_b != NULL) {
    free(tbl_b);
  }
  if (tbl_d_entries != NULL) {
    for (i = 0; i < GABUFR_TBL_SIZE; i++) {
      gabufr_free_varids(tbl_d_entries[i]);
    }
    free(tbl_d_entries);
  }
  tbl_b = NULL;
  tbl_d_entries = NULL;
}

gaint gabufr_entry_is_text(const char *entry) {
  gaint i;
  const char * pos, * endpos, * found;
  pos = entry;
  for (i = 0; i < 6; i++) {
    pos = strchr(pos, ';');
    pos++;
  }
  endpos = strchr(pos, ';');
  found = strstr(pos, "CCITT_IA5");
  if (found && 
      (found < endpos)) {
    return GABUFR_STR_TYPE;
  } else {
    return GABUFR_NUM_TYPE;
  }
}

char * gabufr_copy_desc(const char *entry) {
  gaint i, len;
  const char * pos, * endpos;
  char * retval;
  pos = entry;
  for (i = 0; i < 7; i++) {
    pos = strchr(pos, ';');
    pos++;
  }
  endpos = strchr(pos, '\n');
  len = endpos - pos;
  retval = (char *) malloc(len + 1);
  if (retval == NULL) {
    printf("Memory allocation failed during parsing\n");
    return NULL;
  }  
  strncpy(retval, pos, len);
  retval[len] = '\0';
  if (GABUFR_TBL_DEBUG) printf("description: %s\n", retval);
  return retval;
}

gaint gabufr_read_tbl_b(const char * tbl_b_path) { 
  FILE *tbl_b_file;  
  char line[GABUFR_MAX_LINE_LEN];
  gaint f, x, y, scale, offset, width;
  gabufr_varinf * entry;
  
  /* allocate memory and initialize to zero */
  tbl_b = (gabufr_varinf *) calloc(GABUFR_TBL_SIZE, sizeof(gabufr_varinf));
  if (tbl_b == NULL) {
    printf("Memory error loading table B\n");
    return GABUFR_ERR;
  }
  
  /* open file */
  tbl_b_file = fopen(tbl_b_path, "r");
  if (tbl_b_file == NULL) {
    printf ("Error opening table B file (%s): %s\n", 
	    tbl_b_path, strerror(errno));
    return GABUFR_ERR;
  }
  
  /* read entries into table array */
  while ( fgets(line, GABUFR_MAX_LINE_LEN, tbl_b_file) ) {
    if (GABUFR_TBL_DEBUG) printf("line: %s", line);
    if (line[0] == '#' || strlen(line) < 2) {
      continue;
    }
    sscanf(line, "%d;%d;%d;%d;%d;%d",
	   &f, &x, &y, &scale, &offset, &width);

    entry = gabufr_get_varinf(x, y);
    entry->scale = scale;
    entry->offset = offset;
    entry->width = width;
    entry->datatype = gabufr_entry_is_text(line);

    entry->description = gabufr_copy_desc(line);

    if (GABUFR_TBL_DEBUG) printf("(%d,%d,%d): (val+%d)*%d - %d bits of %s data\n",
			  f, x, y, offset, scale, width,
			  (entry->datatype == GABUFR_STR_TYPE) ? 
			  "text" : "numeric");    
  }
  fclose(tbl_b_file);

  return GABUFR_OK;
} 

gaint gabufr_read_tbl_d(const char * tbl_d_path) { 
  FILE *tbl_d_file; 
  gabufr_varid * head, * next;
  char line[GABUFR_MAX_LINE_LEN];
  gaint f, x, y, tbl_x, tbl_y;
  gaint entry_index = 0;
  gaint line_mode = 0; /* 0 for tbl D index; 
			1 for list of FXY's;
		     */
  
  head = NULL;
  tbl_x = tbl_y = 0;

  /* allocate memory and initialize to zero */
  if( ! (tbl_d_entries = 
	 (gabufr_varid **) calloc(GABUFR_TBL_SIZE, 
				  sizeof(gabufr_varid *)))) {
    printf("Memory error loading table D\n");
    return GABUFR_ERR;
  }
  
  
  /* open file */
  tbl_d_file = fopen(tbl_d_path, "r");
  if (tbl_d_file == NULL) {
    printf ("Error opening table D file (%s): %s\n", 
	    tbl_d_path, strerror(errno));
    return GABUFR_ERR;
  }

  /* read entries into table array */
  while ( fgets(line, GABUFR_MAX_LINE_LEN, tbl_d_file) ) {
    if (line[0] == '#') {
      continue;
    }
    sscanf(line, "%d %d %d", &f, &x, &y);
    if (GABUFR_TBL_DEBUG) printf("(%d,%d,%d): ", f, x, y);
    if (line_mode == 0) {
      if (f == 3) {
	tbl_x = x;
	tbl_y = y;
	head = NULL;
	line_mode = 1;
      } else {
	if (GABUFR_TBL_DEBUG) printf("\n");
      }
    } else {
      if (f >= 0) {
	next = (gabufr_varid *) calloc(sizeof(gabufr_varid), 1);
	if (next == NULL) {
	  printf("Memory allocation failed during parsing\n");
	  fclose(tbl_d_file);
	  return GABUFR_ERR;
	}
	next->f = f;
	next->x = x;
	next->y = y;
	if (head) {
	  head->next = next;
	} else {
	  tbl_d_entries[gabufr_tbl_index(tbl_x, tbl_y)] = next;	  
	}
	head = next;
	if (GABUFR_TBL_DEBUG) printf("\t adding (%d, %d) at (%d, %d) [%d]\n", 
			      x, y, tbl_x, tbl_y, entry_index);
      } else {
	if (GABUFR_TBL_DEBUG) printf("finished entry (%d, %d) at %d\n", 
			      x,y,entry_index);       	
	line_mode = 0;
      }
      entry_index++;
    }
  }

  if (GABUFR_TBL_DEBUG) printf("done\n");
  fclose(tbl_d_file);
  return GABUFR_OK;
} 



gaint gabufr_read_tbls(gabufr_tbl_inf * tbl_inf) {
  gaint base_path_len;
  char * tbl_b_path, * tbl_d_path;

  gabufr_reset_tbls();
  
  base_path_len = strlen(base_path);

  tbl_b_path = (char *) malloc(base_path_len + GABUFR_TBL_NAME_LEN + 1);
  if (tbl_b_path == NULL) {
    printf("Memory allocation failed during parsing\n");
    return GABUFR_ERR;
  }
  strncpy(tbl_b_path, base_path, base_path_len);
  tbl_b_path[base_path_len] = '/';
  sprintf((tbl_b_path + base_path_len + 1), "B%dM-%.3d-%.3d-B", 
	  tbl_inf->bufr_edition, 
	  tbl_inf->master_tbl_num,
	  tbl_inf->master_tbl_version);
  if (GABUFR_TBL_DEBUG) printf("reading from table B file %s\n", tbl_b_path);

  tbl_d_path = (char *) malloc(strlen(base_path) + GABUFR_TBL_NAME_LEN + 1);
  if (tbl_d_path == NULL) {
    printf("Memory allocation failed during parsing\n");
    return GABUFR_ERR;
  }
  strncpy(tbl_d_path, base_path, base_path_len);
  tbl_d_path[base_path_len] = '/';
  sprintf((tbl_d_path + base_path_len + 1), "B%dM-%.3d-%.3d-D", 
	  tbl_inf->bufr_edition, 
	  tbl_inf->master_tbl_num,
	  tbl_inf->master_tbl_version);
  if (GABUFR_TBL_DEBUG) printf("reading from table D file %s\n", tbl_d_path);

  if (gabufr_read_tbl_b(tbl_b_path) == GABUFR_ERR
      || gabufr_read_tbl_d(tbl_d_path) == GABUFR_ERR) {
    gabufr_reset_tbls();
    return GABUFR_ERR;
  }

  free(tbl_b_path);
  free(tbl_d_path);
  return GABUFR_OK;
}

/* order of events for NCEP encoded BUFR tables:
 
   Table A:

   1-3-0 delayed rep of three descriptors
   0-31-1 8-bit delayed rep count
   0-0-1 table A entry
   0-0-2 table A desc line 1
   0-0-3 table A desc line 2

   Table B:

   1-1-0 delayed rep of one descriptor
   0-31-1 8-bit delayed rep count
   3-0-4
    3-0-3
     0-0-10 F descriptor to be added or defined
     0-0-11 X descriptor to be added or defined
     0-0-12 Y descriptor to be added or defined
    0-0-13 Element name, line 1
    0-0-14 Element name, line 2
    0-0-15 Units name
    0-0-16 Units scale sign
    0-0-17 Units scale
    0-0-18 Units reference sign
    0-0-19 Units reference value
    0-0-20 Element data width

   Table D:

   1-5-0 delayed rep of five descriptors
   0-31-1 8-bit delayed rep count
   3-0-3
     0-0-10 F descriptor to be added or defined
     0-0-11 X descriptor to be added or defined
     0-0-12 Y descriptor to be added or defined
   2-5-64 Add 64-byte associated character field
   1-1-0 delayed rep of one descriptor
   0-31-1 8-bit delayed rep count
   0-0-30 Descriptor defining sequence

   0-0-0  ignore
*/

gabufr_val * gabufr_update_ncep_tbl_b(gabufr_dset * file, gabufr_msg * msg, gabufr_val * pos) {
  gabufr_varinf new_entry;
  gaint new_x, new_y;
  gaint y_expected, z_expected;
  char description[65];
  gabufr_varinf * entry_to_replace;
 
  new_x = new_y = 0;

  description[64] = '\0';
  z_expected = pos->z;
  for (y_expected = 11; y_expected <= 20; y_expected++) {
    if (!pos) {
      printf("Ran out of data in middle of entry!\n");
      return pos;
    }
    if (pos->y != y_expected) {
      printf("Expected y = %d; got y = %d\n", y_expected, pos->y);
      return pos;
    }
    if (pos->z != z_expected) {
      printf("Expected z = %d; got z = %d\n", z_expected, pos->z);
      return pos;
    }
    if (!pos->sval) {
      printf("Expected string data!\n");
      return pos;
    }      

    switch (pos->y) {
    case 11:
      new_x = strtol(pos->sval, NULL, 10);
      break;
    case 12:
      new_y = strtol(pos->sval, NULL, 10);
      break;
    case 13:
      memcpy(description, pos->sval, 32);
      break;
    case 14:
      memcpy(description + 32, pos->sval, 32);
      break;
    case 15:
      if (strstr(pos->sval, "CCITT_IA5") 
	  || strstr(pos->sval, "CCITT IA5")) {
	new_entry.datatype = GABUFR_STR_TYPE;
      } else {
	new_entry.datatype = GABUFR_NUM_TYPE;
      }
      break;
    case 16:
      if (strchr(pos->sval, '-')) {
	new_entry.scale = -1;
      } else if (strchr(pos->sval, '+')) {
	new_entry.scale = 1;
      } else {
	printf("invalid scale sign string: %s\n", pos->sval);
      }
      break;
    case 17:
      new_entry.scale *= strtol(pos->sval, NULL, 10);
      break;
    case 18: 
      if (strchr(pos->sval, '-')) {
	new_entry.offset = -1;
      } else if (strchr(pos->sval, '+')) {
	new_entry.offset = 1;
      } else {
	printf("invalid offset sign string: %s\n", pos->sval);
      }
      break;
    case 19:
      new_entry.offset *= strtol(pos->sval, NULL, 10);
      break;
    case 20:
      new_entry.width = strtol(pos->sval, NULL, 10);
      break;
    } 
    pos = pos->next;
  }

  if (GABUFR_TBL_DEBUG) 
    printf("updated entry: (%d,%d,%d): (val+%d)*%d - %d bits of %s data\n",
	 0, new_x, new_y, 
	 new_entry.offset, 
	 new_entry.scale, 
	 new_entry.width,
	 (new_entry.datatype == GABUFR_STR_TYPE) ? 
	 "text" : "numeric");

  new_entry.description = (char *) malloc(65);
  if (new_entry.description == NULL) {
    printf("Memory allocation failed during parsing\n");
    return NULL;
  }
  strcpy(new_entry.description, description);

  if (GABUFR_TBL_DEBUG) printf("\tdescription: %s\n", 
				 new_entry.description);    


  /* copy entry into table */
  entry_to_replace = gabufr_get_varinf(new_x, new_y);
  memcpy(entry_to_replace, &new_entry, sizeof(gabufr_varinf));


  return pos;
 
}



gabufr_val * gabufr_update_ncep_tbl_d(gabufr_dset * file, gabufr_msg * msg, 
				      gabufr_val * pos) {
  gaint new_x, new_y;
  gaint y_expected, z_expected;
  char fstr[2], xstr[3], ystr[4];
  gabufr_varid * head, * next;
  gabufr_varid ** entry_ptr;

  new_x = new_y = 0;

  head = NULL;
  z_expected = pos->z;
  for (y_expected = 11; y_expected <= 12; y_expected++) {
    if (!pos) {
      printf("ran out of data in middle of entry!\n");
      return pos;
    }
    if (pos->y != y_expected) {
      printf("expected y = %d; got y = %d\n", y_expected, pos->y);
      return pos;
    }
    if (pos->z != z_expected) {
      printf("expected z = %d; got z = %d\n", z_expected, pos->z);
      return pos;
    }
    if (!pos->sval) {
      printf("expected string data!\n");
      return pos;
    }      

    switch (pos->y) {
    case 11:
      new_x = strtol(pos->sval, NULL, 10);
      break;
    case 12:
      new_y = strtol(pos->sval, NULL, 10);
      break;
    }
    pos = pos->next;
  }
    
  if (GABUFR_TBL_DEBUG) printf("new table D entry is (3, %d, %d)\n", 
				 new_x, new_y);
  entry_ptr = &tbl_d_entries[gabufr_tbl_index(new_x, new_y)];
  gabufr_free_varids(*entry_ptr);
  head = *entry_ptr = NULL;


  while (pos) {
    if (pos->x == -1 && pos->y == -1) {
      if (GABUFR_TBL_DEBUG) printf("sequence description: [%s]\n", 
				     pos->sval);
    } else if (pos->x == 0 && pos->y == 30) {
      next = (gabufr_varid *) calloc(sizeof(gabufr_varid), 1);
      if (next == NULL) {
	printf("Memory allocation failed during parsing\n");
	return NULL;
      }

      memcpy(fstr, pos->sval, 1);
      fstr[1] = '\0';
      memcpy(xstr, pos->sval+1, 2);
      xstr[2] = '\0';
      memcpy(ystr, pos->sval+3, 3);
      ystr[3] = '\0';

      next->f = strtol(fstr, NULL, 10);
      next->x = strtol(xstr, NULL, 10);
      next->y = strtol(ystr, NULL, 10);
      
      if (GABUFR_TBL_DEBUG) printf("\tadding (%d, %d, %d) to sequence\n", 
	     next->f, next->x, next->y);

      if (head) {
	head->next = next;
      } else {
	*entry_ptr = next;
      }
      head = next;
      
    } else {
      break;
    }
    pos = pos->next;
  }
  if (GABUFR_TBL_DEBUG) printf("\n");

  return pos;
}



void gabufr_update_ncep_tbl(gabufr_dset * file, gabufr_msg * msg) {
  gabufr_val * pos;
  gaint i;
  long f;

  for (i = 0; i < msg->subcnt; i++) {
    pos = msg->subs[i];
    while (pos) {
      if (pos->x == 0 && pos->y == 10 && pos->sval!=NULL) {
	f = strtol(pos->sval, NULL, 10);
	switch (f) {
	case 0:
	  pos = gabufr_update_ncep_tbl_b(file, msg, pos->next);
	  break;
	case 3:
	  pos = gabufr_update_ncep_tbl_d(file, msg, pos->next);
	  break;
	default:
	  printf("warning: invalid table definition, f = %ld\n", f);
	}
      } else {
	pos = pos->next;
      }
    }
    
  }
}

#ifdef GABUFR_TBL_STANDALONE
void gabufr_print_tbl_b() {
  gaint x, y;
  gabufr_varinf * entry;
  for (x = 0; x < (1<<6); x++) {
    for (y = 0; y < (1<<8); y++) {
      entry = gabufr_get_varinf(x, y);
      if (entry->width > 0) {
	printf("(%d,%d,%d): ", 0, x, y);
	if (entry->datatype == GABUFR_STR_TYPE) {
	  printf("text: ");
	} else {
	  printf("numeric ((%d bits + %d) * 10^%d): ",
		 entry->width,
		 entry->offset, 
		 entry->scale);
	}
	printf(entry->description);
	printf("\n");
      }
    }
  }
}

gaint main (gaint argc, char *argv[]) {
  if (argc > 1) {
    gabufr_read_tbl_b(argv[1]);
    gabufr_print_tbl_b();
  }
  return 0;
}
#endif /* GABUFR_TBL_STANDALONE */
