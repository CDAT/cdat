/*
 *
 *    Copyright (C) 2004-2006 NERC DataGrid
 *    This software may be distributed under the terms of the
 *    CCLRC Licence for CCLRC Software
 * <CDATDIR>/External_License/CCLRC_CDAT_License.txt 
 *
 */
#ifdef HAVE_PP
#include "cdunifpp.h"

void pp_dump_header(const PPhdr *hdr)
{
#ifdef PP_STORE_LBYR
  printf("LBYR=%d\n",hdr->LBYR);
#endif
#ifdef PP_STORE_LBMON
  printf("LBMON=%d\n",hdr->LBMON);
#endif
#ifdef PP_STORE_LBDAT
  printf("LBDAT=%d\n",hdr->LBDAT);
#endif
#ifdef PP_STORE_LBHR
  printf("LBHR=%d\n",hdr->LBHR);
#endif
#ifdef PP_STORE_LBMIN
  printf("LBMIN=%d\n",hdr->LBMIN);
#endif
#ifdef PP_STORE_LBDAY
  printf("LBDAY=%d\n",hdr->LBDAY);
#endif
#ifdef PP_STORE_LBYRD
  printf("LBYRD=%d\n",hdr->LBYRD);
#endif
#ifdef PP_STORE_LBMOND
  printf("LBMOND=%d\n",hdr->LBMOND);
#endif
#ifdef PP_STORE_LBDATD
  printf("LBDATD=%d\n",hdr->LBDATD);
#endif
#ifdef PP_STORE_LBHRD
  printf("LBHRD=%d\n",hdr->LBHRD);
#endif
#ifdef PP_STORE_LBMIND
  printf("LBMIND=%d\n",hdr->LBMIND);
#endif
#ifdef PP_STORE_LBDAYD
  printf("LBDAYD=%d\n",hdr->LBDAYD);
#endif
#ifdef PP_STORE_LBTIM
  printf("LBTIM=%d\n",hdr->LBTIM);
#endif
#ifdef PP_STORE_LBFT
  printf("LBFT=%d\n",  hdr->LBFT);
#endif
#ifdef PP_STORE_LBLREC
  printf("LBLREC=%d\n",  hdr->LBLREC);
#endif
#ifdef PP_STORE_LBCODE
  printf("LBCODE=%d\n",  hdr->LBCODE);
#endif
#ifdef PP_STORE_LBHEM
  printf("LBHEM=%d\n",  hdr->LBHEM);
#endif
#ifdef PP_STORE_LBROW
  printf("LBROW=%d\n",  hdr->LBROW);
#endif
#ifdef PP_STORE_LBNPT
  printf("LBNPT=%d\n",  hdr->LBNPT);
#endif
#ifdef PP_STORE_LBEXT
  printf("LBEXT=%d\n",  hdr->LBEXT);
#endif
#ifdef PP_STORE_LBPACK
  printf("LBPACK=%d\n",  hdr->LBPACK);
#endif
#ifdef PP_STORE_LBREL
  printf("LBREL=%d\n",  hdr->LBREL);
#endif
#ifdef PP_STORE_LBFC
  printf("LBFC=%d\n",  hdr->LBFC);
#endif
#ifdef PP_STORE_LBCFC
  printf("LBCFC=%d\n",  hdr->LBCFC);
#endif
#ifdef PP_STORE_LBPROC
  printf("LBPROC=%d\n",  hdr->LBPROC);
#endif
#ifdef PP_STORE_LBVC
  printf("LBVC=%d\n",  hdr->LBVC);
#endif
#ifdef PP_STORE_LBRVC
  printf("LBRVC=%d\n",  hdr->LBRVC);
#endif
#ifdef PP_STORE_LBEXP
  printf("LBEXP=%d\n",  hdr->LBEXP);
#endif
#ifdef PP_STORE_LBBEGIN
  printf("LBBEGIN=%d\n",  hdr->LBBEGIN);
#endif
#ifdef PP_STORE_LBNREC
  printf("LBNREC=%d\n",  hdr->LBNREC);
#endif
#ifdef PP_STORE_LBPROJ
  printf("LBPROJ=%d\n",  hdr->LBPROJ);
#endif
#ifdef PP_STORE_LBTYP
  printf("LBTYP=%d\n",  hdr->LBTYP);
#endif
#ifdef PP_STORE_LBLEV
  printf("LBLEV=%d\n",  hdr->LBLEV);
#endif
#ifdef PP_STORE_LBRSVD1
  printf("LBRSVD1=%d\n",  hdr->LBRSVD1);
#endif
#ifdef PP_STORE_LBRSVD2
  printf("LBRSVD2=%d\n",  hdr->LBRSVD2);
#endif
#ifdef PP_STORE_LBRSVD3
  printf("LBRSVD3=%d\n",  hdr->LBRSVD3);
#endif
#ifdef PP_STORE_LBRSVD4
  printf("LBRSVD4=%d\n",  hdr->LBRSVD4);
#endif
#ifdef PP_STORE_LBSRCE
  printf("LBSRCE=%d\n",  hdr->LBSRCE);
#endif
#ifdef PP_STORE_LBUSER1
  printf("LBUSER1=%d\n",  hdr->LBUSER1);
#endif
#ifdef PP_STORE_LBUSER2
  printf("LBUSER2=%d\n",  hdr->LBUSER2);
#endif
#ifdef PP_STORE_LBUSER3
  printf("LBUSER3=%d\n",  hdr->LBUSER3);
#endif
#ifdef PP_STORE_LBUSER4
  printf("LBUSER4=%d\n",  hdr->LBUSER4);
#endif
#ifdef PP_STORE_LBUSER5
  printf("LBUSER5=%d\n",  hdr->LBUSER5);
#endif
#ifdef PP_STORE_LBUSER6
  printf("LBUSER6=%d\n",  hdr->LBUSER6);
#endif
#ifdef PP_STORE_LBUSER7
  printf("LBUSER7=%d\n",  hdr->LBUSER7);
#endif

#ifdef PP_STORE_BULEV
  printf("BULEV=%e\n",  hdr->BULEV);
#endif
#ifdef PP_STORE_BRSVD2
  printf("BHULEV=%e\n",  hdr->BHULEV);
#endif
#ifdef PP_STORE_BRSVD3
  printf("BRSVD3=%e\n",  hdr->BRSVD3);
#endif
#ifdef PP_STORE_BRSVD4
  printf("BRSVD4=%e\n",  hdr->BRSVD4);
#endif
#ifdef PP_STORE_BDATUM
  printf("BDATUM=%e\n",  hdr->BDATUM);
#endif
#ifdef PP_STORE_BACC
  printf("BACC=%e\n",  hdr->BACC);
#endif
#ifdef PP_STORE_BLEV
  printf("BLEV=%e\n",  hdr->BLEV);
#endif
#ifdef PP_STORE_BRLEV
  printf("BRLEV=%e\n",  hdr->BRLEV);
#endif
#ifdef PP_STORE_BHLEV
  printf("BHLEV=%e\n",  hdr->BHLEV);
#endif
#ifdef PP_STORE_BHRLEV
  printf("BHRLEV=%e\n",  hdr->BHRLEV);
#endif
#ifdef PP_STORE_BPLAT
  printf("BPLAT=%e\n",  hdr->BPLAT);
#endif
#ifdef PP_STORE_BPLON
  printf("BPLON=%e\n",  hdr->BPLON);
#endif
#ifdef PP_STORE_BGOR
  printf("BGOR=%e\n",  hdr->BGOR);
#endif
#ifdef PP_STORE_BZY
  printf("BZY=%e\n",  hdr->BZY);
#endif
#ifdef PP_STORE_BDY
  printf("BDY=%e\n",  hdr->BDY);
#endif
#ifdef PP_STORE_BZX
  printf("BZX=%e\n",  hdr->BZX);
#endif
#ifdef PP_STORE_BDX
  printf("BDX=%e\n",  hdr->BDX);
#endif
#ifdef PP_STORE_BMDI
  printf("BMDI=%e\n",  hdr->BMDI);
#endif
#ifdef PP_STORE_BMKS
  printf("BMKS=%e\n",  hdr->BMKS);
#endif

  fflush(stdout);
}

#endif

void pp_dump_date(PPdate *date) {
  printf ("%d-%d-%d %d:%d:%d\n",
	  date->year, date->month, date->day, date->hour, date->minute, date->second);
}

void pp_dump_time(PPtime *t) {
  printf("     Time at %p\n", t);
  printf("       Time1: ");
  pp_dump_date(&t->time1);
  printf("       Time2: ");
  pp_dump_date(&t->time2);
}


void pp_dump_list(PPlist *list, void (func)(void *)) {
  PPlisthandle handle;
  void *value;

  printf("    %d values on list\n", list->n);
  pp_list_startwalk(list,&handle);
  while ((value=pp_list_walk(&handle,0))!=NULL)
    func(value);
}

void pp_dump_taxis(PPgenaxis *taxis) {
  PPtaxis *t;

  t = (PPtaxis*) taxis->axis;

  printf("PP_DUMP_TAXIS\n");
  printf("  taxis = %p axis = %p\n", taxis, t);

  printf("  pp_dump_taxis orig: ");
  pp_dump_date(&t->time_orig);
  printf("  pp_dump_taxis Values: \n");
  pp_dump_list(t->values, pp_dump_time);
  puts("----------------------");
  fflush(stdout);
};

