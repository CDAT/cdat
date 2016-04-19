"""
udunits -- Python wrapping for UDUNITS package developped by UNIDATA
(C) Copyright 1992,1995 UCAR/Unidata


Permission to use, copy, modify, and distribute this software and its
documentation for any purpose without fee is hereby granted, provided that
the above copyright notice appears in all copies, that both that copyright
notice and this permission notice appear in supporting documentation, and
that the name of UCAR/Unidata not be used in advertising or publicity
pertaining to distribution of the software without specific, written prior
permission.  UCAR makes no representations about the suitability of this
software for any purpose.  It is provided "as is" without express or
implied warranty.  It is provided with no support and without obligation on
the part of UCAR or Unidata, to assist in its use, correction,
modification, or enhancement.

"""
import os
import sys
from udunits import udunits, addBaseUnit, addDimensionlessUnit, addScaledUnit  # noqa
from udunits import addOffsettedUnit, addMultipliedUnits, addInvertedUnit, addDividedUnits  # noqa
udunits_init = 0  # noqa

xml_pth = os.path.join(sys.prefix,"share","udunits","udunits2.xml")
if os.path.exists(xml_pth):
    os.environ["UDUNITS2_XML_PATH"] = xml_pth
else:
    try:
        import cdat_info
        xml_pth = os.path.join(cdat_info.externals,"share","udunits","udunits2.xml")
        if os.path.exists(xml_pth):
            os.environ["UDUNITS2_XML_PATH"] = xml_pth
    except:
        pass
