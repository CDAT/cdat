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

/*
 * for use as default action in switch statements which shouldn't need a default 
 */

static int verbose;

static const int debug=0;

int pp_switch_bug(const char *routine)
{
  /* CuError(CU_EINTERN,"CDUINF_PP: SWITCH Error in %s",routine); */
  pp_error("no match in switch statement in routine; may indicate coding bug in CDUNIFPP or unexpected header value");
  return 0;
}

int pp_error(const char *routine)
{
  if (verbose || debug)
    CuError(CU_EINTERN,"CDUNIF_PP: error condition detected in routine %s",routine);
  verbose=0;
  return 0;
}

int pp_error_mesg(const char *routine, const char *mess)
{
  if (verbose || debug)
    CuError(CU_EINTERN,"CDUNIF_PP: error condition detected in routine %s: %s",routine,mess);
  verbose=0;
  return 0;
}

int pp_errorhandle_init()
{
  /* init sets verbose -- called at start of each of the interface routines --
   * then first call to pp_error will cause a diagnostic to be printed,
   * but then unsets verbose to avoid series of knock-on messages
   */
  verbose=1;
  return 0;
}

#endif
