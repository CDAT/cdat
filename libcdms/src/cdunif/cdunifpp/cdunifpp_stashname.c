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

/* returns name corresponding to stash codes, or NULL if not found. */

/* NOTE: the code of this function was largely auto-generated from the stashmaster files
 * from UM version 4.5.  See below for perl script.
 */

char *pp_stashname(int model, int section, int item)
{
  switch (model) {
  case 1:
    switch (section) {
    case 0:
      switch (item) {
      case 1: return "PSTAR AFTER TIMESTEP";
      case 2: return "U COMPNT OF WIND AFTER TIMESTEP";
      case 3: return "V COMPNT OF WIND AFTER TIMESTEP";
      case 4: return "THETA AFTER TIMESTEP";
      case 5: return "THETAL IN THE EXTERNAL DUMP";
      case 9: return "SOIL MOISTURE CONTENT IN A LAYER";
      case 10: return "SPECIFIC HUMIDITY AFTER TIMESTEP";
      case 11: return "QT IN THE EXTERNAL DUMP";
      case 12: return "QCF IN THE EXTERNAL DUMP";
      case 13: return "CONV CLOUD AMOUNT AFTER TIMESTEP";
      case 14: return "CONV CLOUD BASE LEVEL NO. AFTER TS";
      case 15: return "CONV CLOUD TOP LEVEL NO. AFTER TS";
      case 16: return "CONV CLOUD LIQUID WATER PATH";
      case 17: return "SILHOUETTE OROGRAPHIC ROUGHNESS";
      case 18: return "HALF OF  (PEAK TO TROUGH HT OF OROG)";
      case 20: return "DEEP SOIL TEMP AFTER TIMESTEP";
      case 21: return "SOIL MOISTURE CONTENT AFTER TS KG/M2";
      case 22: return "CANOPY WATER AFTER TIMESTEP  KG/M2";
      case 23: return "SNOW AMOUNT AFTER TIMESTEP  KG/M2";
      case 24: return "SURFACE TEMPERATURE AFTER TIMESTEP";
      case 25: return "BOUNDARY LAYER DEPTH AFTER TIMESTEP";
      case 26: return "ROUGHNESS LENGTH AFTER TIMESTEP";
      case 27: return "SNOW EDGE AFTER TIMESTEP  **";
      case 28: return "SURFACE ZONAL CURRENT AFTER TIMESTEP";
      case 29: return "SURFACE MERID CURRENT AFTER TIMESTEP";
      case 30: return "LAND MASK (LOGICAL: LAND=TRUE)";
      case 31: return "SEA ICE FRACTION AFTER TIMESTEP";
      case 32: return "SEA ICE DEPTH (MEAN OVER ICE)  M";
      case 33: return "OROGRAPHY (/STRAT LOWER BC)";
      case 34: return "STANDARD DEVIATION OF OROGRAPHY";
      case 35: return "OROGRAPHIC GRADIENT XX COMPONENT";
      case 36: return "OROGRAPHIC GRADIENT XY COMPONENT";
      case 37: return "OROGRAPHIC GRADIENT YY COMPONENT";
      case 38: return "ICE EDGE  IN THE ANCILLARY FILE ONLY";
      case 39: return "SST ANOMALY";
      case 40: return "VOL SMC AT WILTING AFTER TIMESTEP";
      case 41: return "VOL SMC AT CRIT PT AFTER TIMESTEP";
      case 42: return "VOL SMC AT FIELD CAP AFTER TIMESTEP";
      case 43: return "VOL SMC AT SATURATION AFTER TIMESTEP";
      case 44: return "SAT SOIL CONDUCTIVITY AFTER TIMESTEP";
      case 45: return "EAGLESON'S EXPONENT AFTER TIMESTEP";
      case 46: return "THERMAL CAPACITY AFTER TIMESTEP";
      case 47: return "THERMAL CONDUCTIVITY AFTER TIMESTEP";
      case 48: return "SATURATED SOIL WATER SUCTION  **";
      case 49: return "SEA-ICE TEMPERATURE AFTER TIMESTEP";
      case 50: return "VEGETATION FRACTION AFTER TIMESTEP";
      case 51: return "ROOT DEPTH AFTER TIMESTEP";
      case 52: return "SNOW-FREE SURFACE ALBEDO AFTER TS";
      case 53: return "DEEP SNOW SURFACE ALBEDO AFTER TS";
      case 54: return "SURFACE RESISTANCE TO EVAP AFTER TS";
      case 55: return "SURFACE CAPACITY AFTER TIMESTEP";
      case 56: return "INFILTRATION FACTOR AFTER TIMESTEP";
      case 57: return "TOTAL AEROSOL EMISSIONS (FOR VIS)";
      case 58: return "SULPHUR DIOXIDE EMISSIONS";
      case 59: return "DIMETHYL SULPHIDE EMISSIONS";
      case 60: return "OZONE  **";
      case 61: return "ATM TRACER  1 (CONVEN O3  )AFTER TS";
      case 62: return "ATM TRACER  2 (CONVEN H2O  )AFTER TS";
      case 63: return "ATM TRACER  3 (CONVEN CO  )AFTER TS";
      case 64: return "ATM TRACER  4 (CONVEN CH4  )AFTER TS";
      case 65: return "ATM TRACER  5 (CONVEN N2O  )AFTER TS";
      case 66: return "ATM TRACER  6 (CONVEN NO  )AFTER TS";
      case 67: return "ATM TRACER  7 (CONVEN NO2  )AFTER TS";
      case 68: return "ATM TRACER  8 (CONVEN HNO3 )AFTER TS";
      case 69: return "ATM TRACER  9 (CONVEN N2O5 )AFTER TS";
      case 70: return "ATM TRACER 10 (CONVN CLONO2)AFTER TS";
      case 71: return "ATM TRACER 11 (CONVEN CLO  )AFTER TS";
      case 72: return "ATM TRACER 12 (CONVEN HCL  )AFTER TS";
      case 73: return "ATM TRACER 13 (CONVN CF2CL2)AFTER TS";
      case 74: return "ATM TRACER 14 (CONVEN CFCL3)AFTER TS";
      case 75: return "ATM TRACER 15 (CONVEN HF  )AFTER TS";
      case 76: return "ATM TRACER 16 (CONVEN -----)AFTER TS";
      case 77: return "ATM TRACER 17 (CONVEN -----)AFTER TS";
      case 78: return "ATM TRACER 18 (CONVEN -----)AFTER TS";
      case 79: return "ATM TRACER 19 (CONVEN ---- )AFTER TS";
      case 80: return "ATM TRACER 20 (CONVEN ---- )AFTER TS";
      case 81: return "ATM TRACER 21 (CONVEN H2S  )AFTER TS";
      case 82: return "ATM TRACER 22 (CONV WATER-SOL)AFT TS";
      case 83: return "ATM TRACER 23 (CONV \"DUST\" )AFTER TS";
      case 84: return "ATM TRACER 24 (CONV OCEANIC)AFTER TS";
      case 85: return "ATM TRACER 25 (CONVEN SOOT )AFTER TS";
      case 86: return "ATM TRACER 26 (CONV VOLC ASH) AFT TS";
      case 87: return "ATM TRACER 27 (CONVEN H2SO4)AFTER TS";
      case 88: return "ATM TRACER 28 (NH4)2SO4  AFTER TS";
      case 89: return "ATM TRACER 29 (CONV MINERAL)AFTER TS";
      case 90: return "TOTAL AEROSOL (FOR VISIBILITY)";
      case 93: return "RUNOFF COASTAL OUTFLOW POINT";
      case 96: return "HORIZ BOUNDARY TENDS: UNAVAILABLE";
      case 97: return "HORIZ BOUNDARY VALUES: UNAVAILABLE";
      case 98: return "LOWER BOUNDARY TENDENCIES (OROG)";
      case 101: return "SO2 MASS MIXING RATIO  AFTER TSTEP";
      case 102: return "DIMETHYL SULPHIDE MIX RAT AFTER TS";
      case 103: return "SO4 AITKEN MODE AEROSOL AFTER TSTEP";
      case 104: return "SO4 ACCUM. MODE AEROSOL AFTER TSTEP";
      case 105: return "SO4 DISSOLVED AEROSOL AFTER TSTEP";
      case 106: return "H2O2 MASS MIXING RATIO AFTER TSTEP";
      case 107: return "NH3 MASS MIXING RATIO  AFTER TSTEP";
      case 108: return "FRESH SOOT MASS MIX RAT AFTER TSTEP";
      case 109: return "AGED SOOT MASS MIX RAT  AFTER TSTEP";
      case 110: return "CLOUD SOOT MASS MIX RAT AFTER TSTEP";
      case 121: return "3D NATURAL SO2 EMISSIONS KG/M2/S";
      case 122: return "3D OH CONCENTRATIONS IN MCULES/CC";
      case 123: return "3D HO2 CONCENTRATIONS IN MCULES/CC";
      case 124: return "3D H2O2 MIX RATIO LIMIT FIELD";
      case 125: return "3D OZONE MIX RAT FOR SULPHUR CYCLE";
      case 126: return "HIGH LEVEL  SO2 EMISSIONS KG/M2/S";
      case 127: return "AMMONIA GAS  EMISSIONS KG/M2/S";
      case 128: return "FRESH SOOT SURF EMISS  KG/M2/S";
      case 129: return "FRESH SOOT HI LEV EMISS KG/M2/S";
      case 150: return "W COMP OF WIND C-P TH LEVS:VAR DUMMY";
      case 151: return "PRESSURE  C-P RHO LEVS:VAR DUMMY";
      case 152: return "DENSITY*R*R  C-P RHO LEVS:VAR DUMMY";
      case 153: return "U COMP OF WIND C-P RHO LEVS:VAR DUM";
      case 154: return "V COMP OF WIND C-P RHO LEVS:VAR DUM";
      case 160: return "HADCM2 SULPHATE LOADING PATTERNS";
      case 201: return "PSTAR PERTURBATION- DUMMY";
      case 202: return "U COMPNT PERTURBATION- DUMMY";
      case 203: return "V COMPNT PERTURBATION- DUMMY";
      case 204: return "THETA PERTURBATION- DUMMY";
      case 205: return "FRACTIONAL LAND COVER";
      case 207: return "CLAPP-HORNBERGER \"B\" COEFFICIENT";
      case 208: return "LEAF AREA INDEX OF VEG FRACTION";
      case 209: return "CANOPY HEIGHT OF VEGETATED FRACTION";
      case 211: return "CCA with anvil after timestep";
      case 213: return "CANOPY CONDUCTANCE AFTER TIMESTEP";
      case 214: return "UNFROZEN SOIL MOISTURE FRAC AFTER TS";
      case 215: return "FROZEN SOIL MOISTURE FRAC AFTER TS";
      case 216: return "FRACTIONS OF SURFACE TYPES";
      case 217: return "LEAF AREA INDEX OF PLANT FUNC TYPES";
      case 218: return "CANOPY HEIGHT OF PLANT FUNC TYPES M";
      case 219: return "DISTURBED FRACTION OF VEGETATION";
      case 220: return "SNOW-FREE ALBEDO OF SOIL";
      case 221: return "SNOW SOOT CONTENT";
      case 222: return "NET ENERGY CHANGE THIS PERIOD J/M**2";
      case 223: return "SOIL CARBON CONTENT  KG C / M2";
      case 224: return "ACCUMULATED NPP ON PLANT FUNC TYPES";
      case 225: return "ACCUMULATED LEAF TURNOVER RATE PFTS";
      case 226: return "ACCUMULATED PHENOL LEAF TRNVR PFTS";
      case 227: return "ACCUMULATED WOOD RESPIRATION PFTS";
      case 228: return "ACCUMULATED SOIL RESPIRATION";
      case 229: return "CANOPY WATER ON NON-ICE TILES  KG/M2";
      case 230: return "CANOPY CAPACITY NON-ICE TILES  KG/M2";
      case 231: return "SNOW GRAIN SIZE  MICRONS";
      case 232: return "SNOW TEMPERATURE  K";
      case 233: return "SURFACE TEMP ON SNOW-ADJSTD TILES  K";
      case 234: return "ROUGHNESS LENGTH ON TILES  M";
      case 250: return "CO2 OCEAN FLUX  KG/M**2/S";
      case 251: return "CO2 SURFACE EMISSIONS  KG/M**2/S";
      case 252: return "CO2 3D TRACER  MASS MIXING RATIO";
      case 401: return "P_EXNER IN D1 FOR ADDRESSING ONLY";
      case 402: return " QCL  IN D1 FOR ADDRESSING ONLY";
      case 403: return " QCF  IN D1 FOR ADDRESSING ONLY";
      case 404: return "RHCRIT IN D1 FOR ADDRESSING ONLY";
      case 405: return "SOIL MOISTURE CONTENT IN D1 FOR ADD";
      }
      break;
    case 1:
      switch (item) {
      case 4: return "TEMPERATURE AFTER SW RAD INCREMENTS";
      case 201: return "NET DOWN SURFACE SW FLUX: SW TS ONLY";
      case 202: return "NET DOWN SW FLUX: SOLID SURF: ALL TS";
      case 203: return "NET DOWN SW RAD FLUX: OPEN SEA";
      case 204: return "NET DOWN SURFACE SW FLUX BELOW 690NM";
      case 205: return "SNOW-FREE SURFACE ALBEDO (ANCILLARY)";
      case 206: return "DEEP SNOW SURFACE ALBEDO (ANCILLARY)";
      case 207: return "INCOMING SW RAD FLUX (TOA): ALL TSS";
      case 208: return "OUTGOING SW RAD FLUX (TOA)";
      case 209: return "CLEAR-SKY (II) UPWARD SW FLUX (TOA)";
      case 210: return "CLEAR-SKY (II) DOWN SURFACE SW FLUX";
      case 211: return "CLEAR-SKY (II) UP SURFACE SW FLUX";
      case 212: return "LAYER CLOUD WEIGHTS IN SWRAD";
      case 213: return "CONVECTIVE CLOUD WEIGHTS IN SWRAD";
      case 214: return "LAYER CLOUD DIRECT ALBEDO * AMOUNT";
      case 215: return "LAYER CLOUD DIFFUSE ALBEDO * AMOUNT";
      case 216: return "CONV CLOUD DIRECT ALBEDO * AMOUNT";
      case 217: return "CONV CLOUD DIFFUSE ALBEDO * AMOUNT";
      case 218: return "LAYER CLOUD REDUCED TO 3 LAYERS - SW";
      case 219: return "TOTAL CLOUD AMOUNT IN SW RADIATION";
      case 220: return "CONV CLD LIQ RE * CONV CLD AMOUNT";
      case 221: return "LAYER CLD LIQ RE * LAYER CLD AMOUNT";
      case 222: return "CONV CLD AMT IN SWRAD (MICROPHYSICS)";
      case 223: return "LYR CLD AMT IN SWRAD (MICROPHYSICS)";
      case 224: return "LAYER CLD CONDENSED WATER PATH * AMT";
      case 225: return "CONV CLD LIQ RE * CONV CLD WEIGHT";
      case 226: return "CONV CLD WGT IN SWRAD (MPHY ALL LYR)";
      case 232: return "SW HEATING RATES: ALL TIMESTEPS";
      case 233: return "CLEAR-SKY SW HEATING RATES";
      case 234: return "HADCM2 SULPHATE LOADING (KG/M2)";
      case 235: return "TOTAL DOWNWARD SURFACE SW FLUX";
      case 236: return "HADCM2 SULPHATE FORCING (W/M2)";
      case 237: return "NET DOWNWARD SW FLUX AT THE TROP.";
      case 238: return "UPWARD SW FLUX AT THE TROP.";
      case 241: return "DROPLET NUMBER CONC * CLOUD AMOUNT";
      case 242: return "LAYER CLOUD LWC * CLOUD AMOUNT";
      case 243: return "SO4 CCN MASS CONC * COND SAMP WEIGHT";
      case 244: return "CONDITIONAL SAMPLING WEIGHT";
      case 245: return "2-D EFFECTIVE RADIUS * 2-D RE WEIGHT";
      case 246: return "WEIGHT FOR 2-D EFFECTIVE RADIUS";
      }
      break;
    case 2:
      switch (item) {
      case 4: return "TEMPERATURE AFTER LW RAD INCREMENTS";
      case 201: return "NET DOWN SURFACE LW RAD FLUX";
      case 202: return "NET DOWN LW RAD FLUX: SOLID SURFACE";
      case 203: return "NET DOWN LW RAD FLUX: OPEN SEA";
      case 204: return "TOTAL CLOUD AMOUNT IN LW RADIATION";
      case 205: return "OUTGOING LW RAD FLUX (TOA)";
      case 206: return "CLEAR-SKY (II) UPWARD LW FLUX (TOA)";
      case 207: return "DOWNWARD LW RAD FLUX: SURFACE";
      case 208: return "CLEAR-SKY (II) DOWN SURFACE LW FLUX";
      case 232: return "LW HEATING RATES";
      case 233: return "CLEAR-SKY LW HEATING RATES";
      case 237: return "NET DOWNWARD LW FLUX AT THE TROP.";
      case 238: return "TOTAL DOWNWARD LW FLUX AT THE TROP.";
      }
      break;
    case 3:
      switch (item) {
      case 2: return "U COMPNT OF WIND AFTER B.LAYER";
      case 3: return "V COMPNT OF WIND AFTER B.LAYER";
      case 4: return "TEMPERATURE AFTER B.LAYER";
      case 10: return "SPECIFIC HUMIDITY AFTER B.LAYER";
      case 24: return "SURFACE TEMPERATURE AFTER B.LAYER";
      case 25: return "BOUNDARY LAYER DEPTH AFTER B.LAYER";
      case 26: return "ROUGHNESS LEN. AFTER B.L. (SEE DOC)";
      case 49: return "SEA-ICE TEMPERATURE AFTER B. LAYER";
      case 100: return "FLUX OF TRACER 1 IN BL";
      case 101: return "FLUX OF TRACER 2 IN BL";
      case 102: return "FLUX OF TRACER 3 IN BL";
      case 103: return "FLUX OF TRACER 4 IN BL";
      case 104: return "FLUX OF TRACER 5 IN BL";
      case 105: return "FLUX OF TRACER 6 IN BL";
      case 106: return "FLUX OF TRACER 7 IN BL";
      case 107: return "FLUX OF TRACER 8 IN BL";
      case 108: return "FLUX OF TRACER 9 IN BL";
      case 109: return "FLUX OF TRACER 10 IN BL";
      case 110: return "FLUX OF TRACER 11 IN BL";
      case 111: return "FLUX OF TRACER 12 IN BL";
      case 112: return "FLUX OF TRACER 13 IN BL";
      case 113: return "FLUX OF TRACER 14 IN BL";
      case 114: return "FLUX OF TRACER 15 IN BL";
      case 115: return "FLUX OF TRACER 16 IN BL";
      case 116: return "FLUX OF TRACER 17 IN BL";
      case 117: return "FLUX OF TRACER 18 IN BL";
      case 118: return "FLUX OF TRACER 19 IN BL";
      case 119: return "FLUX OF TRACER 20 IN BL";
      case 120: return "FLUX OF TRACER 21 IN BL";
      case 121: return "FLUX OF TRACER 22 IN BL";
      case 122: return "FLUX OF TRACER 23 IN BL";
      case 123: return "FLUX OF TRACER 24 IN BL";
      case 124: return "FLUX OF TRACER 25 IN BL";
      case 125: return "FLUX OF TRACER 26 IN BL";
      case 126: return "FLUX OF TRACER 27 IN BL";
      case 127: return "FLUX OF TRACER 28 IN BL";
      case 128: return "FLUX OF TRACER 29 IN BL";
      case 129: return "FLUX OF TOTAL AEROSOL IN BL";
      case 201: return "HEAT FLUX THROUGH SEA ICE (GBM) W/M2";
      case 202: return "HT FLUX FROM SURF TO DEEP SOIL LEV 1";
      case 203: return "CD";
      case 204: return "CH";
      case 205: return "SURFACE LAYER WIND SHEAR";
      case 206: return "RHOSTAR * CD * SURF_LAYER_WIND_SHEAR";
      case 207: return "RHOSTAR * CH * SURF_LAYER_WIND_SHEAR";
      case 208: return "LOWEST LAYER BULK RICHARDSON NO. RIB";
      case 217: return "SURFACE & B.LAYER HEAT FLUXES  W/M2";
      case 219: return "X-COMP OF SURF & BL WIND STRESS N/M2";
      case 220: return "Y-COMP OF SURF & BL WIND STRESS N/M2";
      case 223: return "SURF & BL TOTL MOISTURE FLUX KG/M2/S";
      case 224: return "WIND MIXING EN'GY FLUX INTO SEA W/M2";
      case 225: return "10 METRE WIND U-COMP";
      case 226: return "10 METRE WIND V-COMP";
      case 228: return "SURFACE SH FLUX FROM SEA (GBM)  W/M2";
      case 229: return "EVAP FROM SOIL SURF -AMOUNT KG/M2/TS";
      case 230: return "EVAP FROM CANOPY - AMOUNT  KG/M2/TS";
      case 231: return "SUBLIM. FROM SURFACE (GBM)  KG/M2/TS";
      case 232: return "EVAPORATION FROM SEA (GBM)  KG/M2/S";
      case 234: return "SURFACE LATENT HEAT FLUX  W/M2";
      case 235: return "SEAICE TOP MELTING LH FLUX(GBM) W/M2";
      case 236: return "TEMPERATURE AT 1.5M";
      case 237: return "SPECIFIC HUMIDITY  AT 1.5M";
      case 238: return "DEEP SOIL TEMPERATURE AFTER B.LAYER";
      case 239: return "CLOUD LIQUID WATER AFTER B.LAYER";
      case 240: return "CLOUD ICE CONTENT AFTER B.LAYER";
      case 241: return "TOTAL SURF MOIST FLUX PER TIMESTEP";
      case 242: return "CLOUD FRACTION AT 1.5M";
      case 243: return "CLOUD LIQUID WATER AT 1.5M";
      case 244: return "CLOUD ICE CONTENT AT 1.5M";
      case 245: return "RELATIVE HUMIDITY AT 1.5M";
      case 247: return "VISIBILITY AT 1.5M  M";
      case 248: return "FOG FRACTION AT 1.5 M";
      case 249: return "10 METRE WIND SPEED  M/S";
      case 250: return "DEWPOINT AT 1.5M (K)";
      case 251: return "SILHOUETTE OROGRAPHIC ROUGHNESS";
      case 252: return "HALF OF  (PEAK TO TROUGH HT OF OROG)";
      case 253: return "PROBABILITY OF VIS LESS THAN 5 KM";
      case 254: return "TL AT 1.5M";
      case 255: return "QT AT 1.5M";
      case 256: return "RHO_CD_MODV1";
      case 257: return "RHO_KM";
      case 258: return "SURFACE SNOWMELT HEAT FLUX  W/M2";
      case 259: return "CANOPY CONDUCTANCE M/S";
      case 260: return "TRANSPIRATION  KG/M2/TS";
      case 261: return "GROSS PRIMARY PRODUCTIVITY KG C/M2/S";
      case 262: return "NET PRIMARY PRODUCTIVITY KG C/M2/S";
      case 263: return "PLANT RESPIRATION KG/M2/S";
      case 264: return "LEAF AREA INDEX OF VEG FRACTION";
      case 265: return "CANOPY HEIGHT OF VEGETATED FRACTION";
      case 270: return "SO2 SURFACE DRY DEP FLUX KG/M2/S";
      case 271: return "SO4 AIT SURF DRY DEP FLUX KG/M2/S";
      case 272: return "SO4 ACC SURF DRY DEP FLUX KG/M2/S";
      case 273: return "SO4 DIS SURF DRY DEP FLUX KG/M2/S";
      case 274: return "RESIST B FOR SO2  AFTER TS  S/M";
      case 275: return "RESIST B FOR SO4 AIT AFTER TS S/M";
      case 276: return "RESIST B FOR SO4 ACC AFTER TS S/M";
      case 277: return "RESIST B FOR SO4 DIS AFTER TS S/M";
      case 278: return "RESIST S FOR SO2 AFTER TS  S/M";
      case 279: return "RESIST S FOR SO4 AIT AFTER TS S/M";
      case 280: return "RESIST S FOR SO4 ACC AFTER TS S/M";
      case 281: return "RESIST S FOR SO4 DIS AFTER TS S/M";
      case 282: return "DRY DEP VEL FOR SO2  AFTER TS M/S";
      case 283: return "DRY DEP VEL FOR SO4 AIT AFTER TS M/S";
      case 284: return "DRY DEP VEL FOR SO4 ACC AFTER TS M/S";
      case 285: return "DRY DEP VEL FOR SO4 DIS AFTER TS M/S";
      case 286: return "AERODYNAMIC RESISTANCE  AFTER TS";
      case 287: return "CANOPY EVAPORATION ON NON-ICE TILES";
      case 288: return "TRANSPIRATION+SOIL EVP NON-ICE TILES";
      case 289: return "GROSS PRIMARY PRODUCTIVITY ON PFTS";
      case 290: return "SURFACE SENSIBLE HEAT FLUX ON TILES";
      case 291: return "NET PRIMARY PRODUCTIVITY ON PFTS";
      case 292: return "PLANT RESPIRATION ON PFTS  KG C/M2/S";
      case 293: return "SOIL RESPIRATION  KG C/M2/S";
      case 294: return "BULK RICHARDSON NUMBER ON TILES";
      case 295: return "FRACTIONAL SNOW COVER";
      case 296: return "EVAP FROM SOIL SURF : RATE  KG/M2/S";
      case 297: return "EVAP FROM CANOPY : RATE  KG/M2/S";
      case 298: return "SUBLIM. SURFACE (GBM) : RATE KG/M2/S";
      case 299: return "TRANSPIRATION RATE  KG/M2/S";
      case 300: return "NH3 SURFACE DRY DEP FLUX KG/M2/S";
      case 301: return "FRESH SOOT DRY DEPN FLUX KG/M2/S";
      case 302: return "AGED SOOT DRY DEPN FLUX KG/M2/S";
      case 303: return "SOOT IN CLOUD (OCCULT) DEPN KG/M2/S";
      case 304: return "TURBULENT MIXING HT AFTER B.LAYER m";
      case 305: return "STABLE BL INDICATOR";
      case 306: return "STRATOCUM. OVER STABLE BL INDICATOR";
      case 307: return "WELL_MIXED BL INDICATOR";
      case 308: return "DECOUPLED SC. NOT OVER CU. INDICATOR";
      case 309: return "DECOUPLED SC. OVER CU. INDICATOR";
      case 310: return "CUMULUS-CAPPED BL INDICATOR";
      case 311: return "POTENTIAL EVAPORATION AMNT KG/M2/TS";
      case 312: return "POTENTIAL EVAPORATION RATE  KG/M2/S";
      case 313: return "SOIL MOISTURE AVAILABILITY FACTOR";
      case 314: return "SURFACE NET RADIATION";
      case 315: return "SNOW-ADJUSTED TILE FRACTIONS";
      case 316: return "SURFACE TEMP ON SNOW-ADJSTD TILES  K";
      case 317: return "TILE FRACTIONS (B.LAYER)";
      case 318: return "LEAF AREA INDEX ON PFTS (B.LAYER)";
      case 319: return "CANOPY HEIGHT ON PFTS (B.LAYER)  M";
      case 320: return "SOIL CARBON CONTENT (B.LAYER) KGC/M2";
      case 321: return "CANOPY WATER ON NON-ICE TILES  KG/M2";
      case 322: return "CANOPY CAPACITY NON-ICE TILES  KG/M2";
      case 323: return "SNOW TEMPERATURE  K";
      case 324: return "ROUGHNESS LENGTH ON TILES  M";
      case 325: return "LEAF TURNOVER RATE ON PFTS";
      case 326: return "CO2 LAND SURFACE FLUX  KG/M**2/S";
      case 327: return "CO2 TOTAL FLUX TO ATMOS  KG/M**2/S";
      }
      break;
    case 4:
      switch (item) {
      case 4: return "TEMPERATURE AFTER LARGE SCALE PRECIP";
      case 10: return "SPECIFIC HUMIDITY AFTER LS PRECIP";
      case 201: return "LARGE SCALE RAIN AMOUNT  KG/M2/TS";
      case 202: return "LARGE SCALE SNOW AMOUNT  KG/M2/TS";
      case 203: return "LARGE SCALE RAINFALL RATE  KG/M2/S";
      case 204: return "LARGE SCALE SNOWFALL RATE  KG/M2/S";
      case 205: return "CLOUD LIQUID WATER AFTER LS PRECIP";
      case 206: return "CLOUD ICE CONTENT AFTER LS PRECIP";
      case 207: return "RELATIVE HUMIDITY AFTER LS PRECIP";
      case 208: return "VISIBILITY AFTER LS PREC IN LEVEL  M";
      case 211: return "SO2 SCAVENGED BY LS PPN KG/M2/TS";
      case 212: return "SO4 AIT SCAVNGD BY LS PPN KG/M2/TS";
      case 213: return "SO4 ACC SCAVNGD BY LS PPN KG/M2/TS";
      case 214: return "SO4 DIS SCAVNGD BY LS PPN KG/M2/TS";
      case 215: return "NH3 SCAVENGED BY LS PPN KG/M2/S";
      case 216: return "SO2 SCAVENGED BY LS PPN KG/M2/S";
      case 217: return "SO4 AIT SCAVNGD BY LS PPN KG/M2/S";
      case 218: return "SO4 ACC SCAVNGD BY LS PPN KG/M2/S";
      case 219: return "SO4 DIS SCAVNGD BY LS PPN KG/M2/S";
      case 220: return "SOOT LS RAINOUT KG/M2/S";
      case 221: return "SOOT LS WASHOUT KG/M2/S";
      case 222: return "RAINFALL RATE OUT OF MODEL LEVELS";
      case 223: return "SNOWFALL RATE OUT OF MODEL LEVELS";
      case 224: return "SUPERCOOLED LIQUID WATER CONTENT";
      case 225: return "SUPERCOOLED RAIN OUT OF MODEL LEVELS";
      }
      break;
    case 5:
      switch (item) {
      case 4: return "THETA  AFTER CONVECTION INCREMENT";
      case 10: return "SPECIFIC HUMIDITY  AFTER CONVECTION";
      case 13: return "CONV CLOUD AMOUNT  AFTER CONVECTION";
      case 201: return "CONVECTIVE RAIN AMOUNT  KG/M2/TS";
      case 202: return "CONVECTIVE SNOW AMOUNT  KG/M2/TS";
      case 203: return "THETA INCREMENT FROM CONVECTION";
      case 204: return "SPECIFIC HUMID INCRMNT FROM CONVECTN";
      case 205: return "CONVECTIVE RAINFALL RATE  KG/M2/S";
      case 206: return "CONVECTIVE SNOWFALL RATE  KG/M2/S";
      case 207: return "PRESSURE AT CONVECTIVE CLOUD BASE";
      case 208: return "PRESSURE AT CONVECTIVE CLOUD TOP";
      case 209: return "TEMPERATURE AFTER CONVECTION";
      case 210: return "ICAO HT OF CONVECTIVE CLOUD BASE";
      case 211: return "ICAO HT OF CONVECTIVE CLOUD TOP";
      case 212: return "CONV. CLOUD AMOUNT ON EACH MODEL LEV";
      case 213: return "CONV CLOUD CONDENSED WATER  KG/KG";
      case 214: return "TOTAL RAINFALL RATE: LS+CONV KG/M2/S";
      case 215: return "TOTAL SNOWFALL RATE: LS+CONV KG/M2/S";
      case 216: return "TOTAL PRECIPITATION RATE  KG/M2/S";
      case 217: return "DILUTE CONVECTIVELY AVAIL POT E J/KG";
      case 218: return "LOWEST CONV CLOUD BASE LEVEL NO.";
      case 219: return "LOWEST CONV CLOUD TOP LEVEL NO.";
      case 220: return "LOWEST CONV CLOUD AMOUNT AFTER CONV";
      case 221: return "LOWEST CONV CLOUD LIQUID WATER PATH";
      case 222: return "PRESSURE AT LOWEST CONV CLOUD BASE";
      case 223: return "PRESSURE AT LOWEST CONV CLOUD TOP";
      case 224: return "ICAO HT OF LOWEST CONV CLOUD BASE";
      case 225: return "ICAO HT OF LOWEST CONV CLOUD TOP";
      case 226: return "TOTAL PRECIPITATION AMOUNT  KG/M2/TS";
      case 227: return "SO2 SCAVENGED BY CONV PPN KG/M2/TS";
      case 228: return "SO4 AIT SCAVNGD BY CONV PPN KG/M2/TS";
      case 229: return "SO4 ACC SCAVNGD BY CONV PPN KG/M2/TS";
      case 230: return "SO4 DIS SCAVNGD BY CONV PPN KG/M2/TS";
      case 231: return "CONV. CLOUD BASE PRESSURE * CCA";
      case 232: return "CONV. CLOUD TOP PRESSURE * CCA";
      case 233: return "GRIDBOX MEAN CONVECTIVE CLOUD WATER";
      case 234: return "GRIDBOX MEAN CONV. CLOUD WATER PATH";
      case 235: return "U compnt of wind after convection";
      case 236: return "V compnt of wind after convection";
      case 237: return "NH3 SCAVENGED BY CONV PPN KG/M2/S";
      case 238: return "SO2 SCAVENGED BY CONV PPN KG/M2/SEC";
      case 239: return "SO4 AIT SCAVNGD BY CONV PPN KG/M2/S";
      case 240: return "SO4 ACC SCAVNGD BY CONV PPN KG/M2/S";
      case 241: return "SO4 DIS SCAVNGD BY CONV PPN KG/M2/S";
      case 242: return "CONVECTIVELY SCAVENGED SOOT KG/M2/S";
      case 250: return "UPDRAUGHT MASS FLUX (PA/S)";
      case 251: return "DOWNDRAUGHT MASS FLUX (PA/S)";
      case 252: return "UPDRAUGHT ENTRAINMENT RATE (S-1)";
      case 253: return "UPDRAUGHT DETRAINMENT RATE (S-1)";
      case 254: return "DOWNDRAUGHT ENTRAINMENT RATE (S-1)";
      case 255: return "DOWNDRAUGHT DETRAINMENT RATE (S-1)";
      case 256: return "U INCREMENT MS-2 ( P GRID)";
      case 257: return "V INCREMENT MS-2  ( P GRID)";
      }
      break;
    case 6:
      switch (item) {
      case 2: return "U COMPNT OF WIND AFTER G.WAVE DRAG";
      case 3: return "V COMPNT OF WIND AFTER G.WAVE DRAG";
      case 201: return "U COMPONENT OF GRAVITY WAVE STRESS";
      case 202: return "V COMPONENT OF GRAVITY WAVE STRESS";
      case 203: return "STANDARD DEVIATION OF OROGRAPHY";
      case 204: return "OROGRAPHIC GRADIENT XX COMPONENT";
      case 205: return "OROGRAPHIC GRADIENT XY COMPONENT";
      case 206: return "OROGRAPHIC GRADIENT YY COMPONENT";
      case 207: return "U-ACCEL FROM SATURATED STRESS";
      case 208: return "V-ACCEL FROM SATURATED STRESS";
      case 209: return "U-ACCEL FROM HYDRAULIC JUMP";
      case 210: return "V-ACCEL FROM HYDRAULIC JUMP";
      case 211: return "U-ACCEL FROM TRAPPED LEE WAVES";
      case 212: return "V-ACCEL FROM TRAPPED LEE WAVES";
      case 213: return "VERTICAL TRANSMISSION COEFFICIENT";
      }
      break;
    case 7:
      switch (item) {
      case 2: return "U COMPNT OF WIND AFTER VERT DIF'SION";
      case 3: return "V COMPNT OF WIND AFTER VERT DIF'SION";
      case 201: return "VERT FLUX OF U MOMENTUM FROM V.DIFFN";
      case 202: return "VERT FLUX OF V MOMENTUM FROM V.DIFFN";
      }
      break;
    case 8:
      switch (item) {
      case 23: return "SNOW MASS AFTER HYDROLOGY  KG/M2";
      case 24: return "SURFACE TEMPERATURE,AFTER HYDROLOGY";
      case 201: return "LAND SNOW MELT AMOUNT  KG/M2/TS";
      case 202: return "LAND SNOW MELT HEAT FLUX  W/M2";
      case 203: return "CANOPY THROUGHFALL AMOUNT  KG/M2/TS";
      case 204: return "SURFACE RUNOFF AMOUNT  KG/M2/TS";
      case 205: return "SUB-SURFACE RUNOFF AMOUNT  KG/M2/TS";
      case 206: return "SOIL HYDROLOGY PARAMETER \"BS\"";
      case 207: return "VEG MODIFIED INFILTRATION RATE";
      case 208: return "SOIL MOISTURE CONTENT";
      case 209: return "CANOPY WATER CONTENT";
      case 210: return "VOL SMC AT WILTING";
      case 211: return "VOL SMC AT CRIT PT";
      case 212: return "VOL SMC AT FLD CAP";
      case 213: return "VOL SMC AT SATURATION";
      case 214: return "SATURATED SOIL CONDUCTIVITY";
      case 215: return "EAGLESON'S EXPONENT";
      case 216: return "THERMAL CAPACITY";
      case 217: return "THERMAL CONDUCTIVITY";
      case 218: return "VEGETATION FRACTION";
      case 219: return "ROOT DEPTH";
      case 220: return "SURFACE RESISTANCE TO EVAPORATION";
      case 221: return "SURFACE CAPACITY";
      case 222: return "INFILTRATION FACTOR";
      case 223: return "SOIL MOISTURE CONTENT IN A LAYER";
      case 224: return "SATURATED SOIL WATER SUCTION";
      case 225: return "DEEP SOIL TEMP. AFTER HYDROLOGY DEGK";
      case 226: return "DEEP SNOWMELT HEAT FLUX  W/M2";
      case 228: return "CLAPP-HORNBERGER \"B\" COEFFICIENT";
      case 229: return "UNFROZEN SOIL MOISTURE FRACTION";
      case 230: return "FROZEN SOIL MOISTURE FRACTION";
      case 231: return "LAND SNOW MELT RATE  KG/M2/S";
      case 233: return "CANOPY THROUGHFALL RATE  KG/M2/S";
      case 234: return "SURFACE RUNOFF RATE  KG/M2/S";
      case 235: return "SUB-SURFACE RUNOFF RATE  KG/M2/S";
      }
      break;
    case 9:
      switch (item) {
      case 4: return "TEMPERATURE AFTER DYNAMIC CLOUD";
      case 10: return "SPEC. HUMIDITY AFTER DYNAMIC CLOUD";
      case 201: return "LAYER CLOUD AMOUNT IN EACH LAYER";
      case 202: return "VERY LOW CLOUD AMOUNT";
      case 203: return "LOW CLOUD AMOUNT";
      case 204: return "MEDIUM CLOUD AMOUNT";
      case 205: return "HIGH CLOUD AMOUNT";
      case 206: return "CLOUD LIQUID WATER AFTER DYN CLOUD";
      case 207: return "CLOUD ICE CONTENT AFTER DYNAM CLOUD";
      case 208: return "CLOUD BASE FOR COVER.GT.0.1 OCTA KFT";
      case 209: return "CLOUD BASE FOR COVER.GT.1.5 OCTA KFT";
      case 210: return "CLOUD BASE FOR COVER.GT.2.5 OCTA KFT";
      case 211: return "CLOUD BASE FOR COVER.GT.3.5 OCTA KFT";
      case 212: return "CLOUD BASE FOR COVER.GT.4.5 OCTA KFT";
      case 213: return "CLOUD BASE FOR COVER.GT.5.5 OCTA KFT";
      case 214: return "CLOUD BASE FOR COVER.GT.6.5 OCTA KFT";
      case 215: return "CLOUD BASE FOR COVER.GT.7.9 OCTA KFT";
      case 216: return "TOTAL CLOUD AMOUNT - RANDOM OVERLAP";
      case 217: return "TOTAL CLOUD AMOUNT MAX/RANDOM OVERLP";
      case 218: return "CLOUD FRACTION BELOW 1000 FT ASL";
      case 219: return "LOW CLOUD BASE (FT ASL)";
      case 220: return "LOW CLOUD TOP (FT ASL)";
      case 221: return "WET BULB FREEZING LEV HEIGHT (M)";
      case 222: return "WET BULB TEMPERATURE (K)";
      case 223: return "TOTAL CLOUD TOP HEIGHT (KFT)";
      case 224: return "LAYER LIQUID CLOUD AMOUNT IN LAYERS";
      case 225: return "LAYER FROZEN CLOUD AMOUNT IN LAYERS";
      case 226: return "LAYER CLOUD FREQUENCY IN EACH LAYER";
      case 227: return "LAYER CLOUD AREA IN LAYER";
      case 228: return "CRITICAL RELATIVE HUMIDITY";
      }
      break;
    case 10:
      switch (item) {
      case 1: return "PSTAR AFTER ADJUSTMENT";
      case 2: return "U COMPNT OF WIND AFTER ADJUSTMENT";
      case 3: return "V COMPNT OF WIND AFTER ADJUSTMENT";
      case 4: return "THETA AFTER ADJUSTMENT";
      case 10: return "SPEC HUMID TOT WATER CONTENT:ADJUST";
      case 201: return "MEAN U OVER ADJ*LAYER THICK=MASS FLX";
      case 202: return "MEAN V OVER ADJ*LAYER THICK=MASS FLX";
      case 203: return "PSEUDO-RADIUS AT MODEL LEVELS";
      case 204: return "ETADOT FROM THE ADJUSTMENT STEP";
      case 205: return "RATE OF CHANGE OF PSTAR";
      case 206: return "GEOPOTENTIAL";
      case 207: return "MEAN U OVER ADJ * LAYER THICK * TEMP";
      case 208: return "MEAN V OVER ADJ * LAYER THICK * TEMP";
      case 209: return "MEAN U OVER ADJ * LAYER THICK *HUMID";
      case 210: return "MEAN V OVER ADJ * LAYER THICK *HUMID";
      case 211: return "MEAN U ADJ *LAYER THICK* LIQ WATER T";
      case 212: return "MEAN V ADJ *LAYER THICK* LIQ WATER T";
      case 213: return "MEAN U ADJ *LAYER THICK* TOTAL WATER";
      case 214: return "MEAN V ADJ *LAYER THICK* TOTAL WATER";
      case 215: return "MEAN U ADJ * LAYER THICK * U";
      case 216: return "MEAN V ADJ * LAYER THICK * U";
      case 217: return "MEAN U ADJ * LAYER THICK * V";
      case 218: return "MEAN V ADJ * LAYER THICK * V";
      case 219: return "MEAN U * LAYER THICK * GEOPOTENTIAL";
      case 220: return "MEAN V * LAYER THICK * GEOPOTENTIAL";
      case 221: return "MN U *LYR THICK* MOIST STATIC ENERGY";
      case 222: return "MN V *LYR THICK* MOIST STATIC ENERGY";
      case 223: return "ANGULAR MOMENTUM M1  KG M2/S X10-24";
      case 224: return "ANGULAR MOMENTUM M2  KG M2/S X10-24";
      case 225: return "ANGULAR MOMENTUM M3  KG M2/S X10-24";
      case 226: return "ANGULAR MOMENTUM W1  KG M2/S X10-24";
      case 227: return "ANGULAR MOMENTUM W2  KG M2/S X10-24";
      case 228: return "ANGULAR MOMENTUM W3  KG M2/S X10-24";
      case 229: return "CLOUD LIQUID WATER BEFORE DYNAMICS";
      case 230: return "CLOUD ICE CONTENT  BEFORE DYNAMICS";
      }
      break;
    case 12:
      switch (item) {
      case 2: return "U AFTER PRIMARY FIELD ADVECTION";
      case 3: return "V AFTER PRIMARY FIELD ADVECTION";
      case 4: return "THETAL AFTER PRIMARY FIELD ADVECTION";
      case 10: return "QT AFTER PRIMARY FIELD ADVECTION";
      case 201: return "OMEGA ON MODEL LEVELS";
      case 202: return "OMEGA ON PRESSURE LEVELS";
      }
      break;
    case 13:
      switch (item) {
      case 2: return "U AFTER FILTERING AND DIFFUSION";
      case 3: return "V AFTER FILTERING AND DIFFUSION";
      case 4: return "THETAL AFTER FILTERING AND DIFFUSION";
      case 10: return "QT AFTER FILTERING AND DIFFUSION";
      case 201: return "QT SOURCE/SINK IN QT_POS  KG/M2/S";
      }
      break;
    case 14:
      switch (item) {
      case 201: return "ATMOS ENERGY CORR'N IN COLUMN  W/M2";
      }
      break;
    case 15:
      switch (item) {
      case 201: return "U COMPNT OF WIND ON PRESSURE LEVELS";
      case 202: return "V COMPNT OF WIND ON PRESSURE LEVELS";
      case 203: return "MAX CAT PROBABILITY. NEED MAX WIND";
      case 204: return "MAX CAT PROB LEVEL. NEED MAX WIND";
      case 205: return "CAT PROB @ P LEVELS. NEED MAX WIND";
      case 206: return "HEIGHT OF MAX WIND LEVEL- NOT AVALBL";
      case 207: return "ICAO HT OF MAX WIND LEV-NEED MAX UV&";
      case 208: return "PRESSURE OF MAX WIND LEV-NEED MAX U,";
      case 209: return "U COMPONENT OF MAX WIND-NEED MAX V&P";
      case 210: return "V COMPONENT OF MAX WIND-NEED MAX U&P";
      case 211: return "CAT PROB AVE 300 250 200MB NEED MAX";
      case 212: return "50 METRE WIND U-COMPONENT";
      case 213: return "50 METRE WIND V-COMPONENT";
      case 214: return "ERTEL POTENTIAL VORTICITY(THETA LEV)";
      case 215: return "UV ON PRESSURE LEVS.  USE MACRO";
      case 216: return "T ON PRESSURE LEVS U GRID. USE MACRO";
      case 217: return "UT ON PRESS LEVS U GRID.  USE MACRO";
      case 218: return "VT ON PRESS LEVS U GRID.  USE MACRO";
      case 219: return "T**2 ON PRESS LEVS U GRID. USE MACRO";
      case 220: return "U**2 ON PRESS LEVS U GRID. USE MACRO";
      case 221: return "V**2 ON PRESS LEVS U GRID. USE MACRO";
      case 222: return "OMEGA ON PRESS LEVS U GRID.USE MACRO";
      case 223: return "OMEGA*T ON P LEVS U GRID.  USE MACRO";
      case 224: return "OMEGA*U ON P LEVS U GRID.  USE MACRO";
      case 225: return "OMEGA*V ON P LEVS U GRID.  USE MACRO";
      case 226: return "SPECIF HUM;P LEVS;U GRID.  USE MACRO";
      case 227: return "Q*U ON PRESS LEVS U GRID.  USE MACRO";
      case 228: return "Q*V ON PRESS LEVS U GRID.  USE MACRO";
      case 229: return "POTENTIAL VORTICITY ON PRESSURE LEVS";
      case 230: return "THETA ON POT VORT LEVS-SEE CON.B.5.F";
      case 231: return "TEST FIELD ON UV GRID-SINGLE LEVEL";
      case 232: return "TEST FIELD ON TH GRID-SINGLE LEVEL";
      case 233: return "TEST FIELD ON TH GRID-PRESS LEVELS";
      case 234: return "TEST FIELD ON TH GRID-MODEL LEVELS";
      case 235: return "q*w on press levs, u grid";
      case 236: return "Fract of time pres lev above surface";
      case 237: return "Total KE per unit area  x10e-6 J/m2";
      case 238: return "Geopotential height, u grid";
      case 239: return "U*Geopotential height, u grid x1.e6";
      case 240: return "v*Geopotential height, u grid x1.e6";
      case 241: return " mountain torque per unit area  N/m";
      }
      break;
    case 16:
      switch (item) {
      case 201: return "GEOPOTENTIAL HEIGHT:LAYER BOUNDARIES";
      case 202: return "GEOPOTENTIAL HEIGHT: PRESSURE LEVELS";
      case 203: return "TEMPERATURE ON PRESSURE LEVELS";
      case 204: return "RELATIVE HUMIDITY ON PRESSURE LEVELS";
      case 205: return "WET BULB POTENTIAL TEMPERATURE";
      case 206: return "SURFACE SNOW PROBABILITY";
      case 207: return "ICAO HT OF -20 DEG ISO- NEED P,HT";
      case 208: return "PRESSURE OF -20 DEG ISO- NEED HT";
      case 209: return "ICAO HT OF FREEZING LEV- NEED P,HT";
      case 210: return "HEIGHT OF FREEZING LEV- NEED PRESS";
      case 211: return "PRESSURE OF FREEZING LEV- NEED HT";
      case 212: return "HT OF CONTRAIL LOWER LIM- NEED UPPER";
      case 213: return "HT OF CONTRAIL UPPER LIM- NEED LOWER";
      case 214: return "PRESSURE AT TROP LEV- NEED HT,TEMP";
      case 215: return "TEMP AT TROP LEVEL- NEED HT,PRESS";
      case 216: return "HEIGHT OF TROP- NEED TEMP, PRESS";
      case 217: return "ICAO HT OF TROP- NEED HT,TEMP,PRESS";
      case 218: return "HEIGHT OF -20 DEG ISO- NEED PRESSURE";
      case 219: return "THERMAL ADVECTION ON PRESSURE LEVELS";
      case 220: return "RADIO DUCT HEIGHT- NEED INTENSITY";
      case 221: return "RADIO DUCT INTENSITY- NEED HEIGHT";
      case 222: return "PRESSURE AT MEAN SEA LEVEL";
      case 223: return "THERMAL ADVECT AVE OVER 850 700 500";
      case 224: return "HEIGHT**2 ON PRESS LEVS.  USE MACRO";
      case 225: return "GEOPOTENTIAL HT OF MODEL LEVEL  M";
      case 226: return "ATM TRACER  1 (O3  ) ON PRESS LEVS";
      case 227: return "ATM TRACER  2 (H2O  ) ON PRESS LEVS";
      case 228: return "ATM TRACER  3 (CO  ) ON PRESS LEVS";
      case 229: return "ATM TRACER  4 (CH4  ) ON PRESS LEVS";
      case 230: return "ATM TRACER  5 (N2O  ) ON PRESS LEVS";
      case 231: return "ATM TRACER  6 (NO  ) ON PRESS LEVS";
      case 232: return "ATM TRACER  7 (NO2  ) ON PRESS LEVS";
      case 233: return "ATM TRACER  8 (HNO3  ) ON PRESS LEVS";
      case 234: return "ATM TRACER  9 (N2O5  ) ON PRESS LEVS";
      case 235: return "ATM TRACER 10 (CLONO2) ON PRESS LEVS";
      case 236: return "ATM TRACER 11 (CLO  ) ON PRESS LEVS";
      case 237: return "ATM TRACER 12 (HCL  ) ON PRESS LEVS";
      case 238: return "ATM TRACER 13 (CF2CL2) ON PRESS LEVS";
      case 239: return "ATM TRACER 14 (CFCL3 ) ON PRESS LEVS";
      case 240: return "ATM TRACER 15 (HF  ) ON PRESS LEVS";
      case 241: return "ATM TRACER 16 (------) ON PRESS LEVS";
      case 242: return "ATM TRACER 17 (------) ON PRESS LEVS";
      case 243: return "ATM TRACER 18 (------) ON PRESS LEVS";
      case 244: return "ATM TRACER 19 (------) ON PRESS LEVS";
      case 245: return "ATM TRACER 20 (------) ON PRESS LEVS";
      case 246: return "ATM TRACER 21 (H2S  ) ON PRESS LEVS";
      case 247: return "ATM TRACER 22 (WATER-SOL) PRESS LEVS";
      case 248: return "ATM TRACER 23 (\"DUST\") ON PRESS LEVS";
      case 249: return "ATM TRACER 24 (OCEANIC)ON PRESS LEVS";
      case 250: return "ATM TRACER 25 (SOOT  ) ON PRESS LEVS";
      case 251: return "ATM TRACER 26 (VOLC ASH)  PRESS LEVS";
      case 252: return "ATM TRACER 27 (H2SO4 ) ON PRESS LEVS";
      case 253: return "ATM TRACER 28 ((NH4)2SO4) PRESS LEVS";
      case 254: return "ATM TRACER 29 (MINERAL)ON PRESS LEVS";
      }
      break;
    case 17:
      switch (item) {
      case 201: return "SO4 AIT SURF SETTLEMENT FLUX KG/M2/S";
      case 202: return "SO4 ACC SURF SETTLEMENT FLUX KG/M2/S";
      case 203: return "MSA MASS MIXING RATIO  AFTER TSTEP";
      case 204: return "NH3 DEPLETION KG/KG  AFTER TSTEP";
      }
      break;
    case 18:
      switch (item) {
      case 1: return "P* AFTER ASSIMILATION INCREMENTS";
      case 2: return "U AFTER ASSIMILATION INCREMENTS";
      case 3: return "V AFTER ASSIMILATION INCREMENTS";
      case 4: return "THETA AFTER ASSIMILATION INCREMENTS";
      case 10: return "Q AFTER ASSIMILATION INCREMENTS";
      case 201: return "P* ASSIMILATION WEIGHTS";
      case 202: return "THETA ASSIMILATION WEIGHTS";
      case 203: return "WIND ASSIMILATION WEIGHTS";
      case 204: return "RH ASSIMILATION WEIGHTS";
      case 205: return "PRECIPITATION RATE ASSIM WEIGHTS";
      case 209: return "VISIBILITY ASSIMILATION WEIGHTS";
      case 211: return "P* ASSIMILATION INCREMENT";
      case 212: return "THETA ASSIMILATION INCREMENT";
      case 213: return "U  ASSIMILATION INCREMENT";
      case 214: return "RH ASSIMILATION INCREMENT";
      case 215: return "PRECIPITATION RATE ASSIM INCREMENTS";
      case 219: return "VISIBILITY ASSIMILATION INCREMENTS";
      case 223: return "V  ASSIMILATION INCREMENT";
      case 231: return "HYDROSTATIC THETA ASSM INCR(FROM P*)";
      case 241: return "GEOSTROPHIC U ASSM INCR (FROM P*)";
      case 242: return "GEOSTROPHIC U ASSM INCR (FROM THETA)";
      case 251: return "GEOSTROPHIC V ASSM INCR (FROM P*)";
      case 252: return "GEOSTROPHIC V ASSM INCR (FROM THETA)";
      case 261: return "P* INCREMENTS FROM WIND BALANCE";
      case 262: return "THETA INCREMENTS FROM WIND BALANCE";
      case 271: return "THETA INCR FROM LS LATENT HEATING";
      case 272: return "THETA INCR FROM PRECIP DATA (LHN)";
      }
      break;
    case 19:
      switch (item) {
      case 1: return "VEGETATION CARBON ON PFTS  KG C/M2";
      case 2: return "GRIDBOX MEAN VEG CARBON  KG C/M2";
      case 3: return "PHENOLOGICAL LEAF TURNOVER RATE PFTS";
      case 4: return "LITTER CARBON ON PFTS  KG C/M2/YEAR";
      case 5: return "GRIDBOX MEAN LITTER CARBN KGC/M2/YR";
      case 6: return "MEAN LEAF TRNVR RATE PFTS FOR PHENOL";
      case 7: return "LEAF AREA INDEX PFTS AFTER PHENOLOGY";
      case 8: return "MN LEAF TRNVR RATE PFTS FOR TRIFFID";
      case 9: return "MEAN NPP ON PFTS FOR TRIFFID";
      case 10: return "MEAN WOOD RESP ON PFTS FOR TRIFFID";
      case 11: return "MEAN SOIL RESPIRATION FOR TRIFFID";
      case 12: return "DISTURBED FRACTION OF VEGETATION";
      case 13: return "SURFACE TYPE FRACTIONS AFTER TRIFFID";
      case 14: return "LEAF AREA INDEX PFTS AFTER TRIFFID";
      case 15: return "CANOPY HEIGHT ON PFTS AFTER TRIFFID";
      case 16: return "SOIL CARBON CONTENT AFTER TRIFFID";
      }
      break;
    case 21:
      switch (item) {
      case 1: return "CM1: PSTAR AFTER TIMESTEP";
      case 2: return "CM1: U COMPNT OF WIND AFTER TIMESTEP";
      case 3: return "CM1: V COMPNT OF WIND AFTER TIMESTEP";
      case 4: return "CM1: THETA AFTER TIMESTEP";
      case 10: return "CM1: SPECIFIC HUMIDITY AFTER TSTEP";
      case 13: return "CM1: CONV CLOUD AMOUNT AFTER TSTEP";
      case 14: return "CM1: CONV CLOUD BASE LEV NO AFTER TS";
      case 15: return "CM1: CONV CLOUD TOP LEV NO AFTER TS";
      case 16: return "CM1: CONV CLOUD LIQUID WATER PATH";
      case 23: return "CM1: SNOW AMOUNT AFTER TSTEP  KG/M2";
      case 24: return "CM1: SURFACE TEMPERATURE AFTER TS";
      case 25: return "CM1: BOUNDARY LAYER DEPTH AFTER TS";
      case 26: return "CM1: ROUGHNESS LENGTH AFTER TIMESTEP";
      case 28: return "CM1: SURFACE ZONAL CURRENT AFTER TS";
      case 29: return "CM1: SURFACE MERID CURRENT AFTER TS";
      case 31: return "CM1: SEA ICE FRACTION AFTER TIMESTEP";
      case 32: return "CM1: SEA ICE DEPTH (MEAN OVER ICE) M";
      case 61: return "CM1: ATM TRACER  1  (CONVEN  O3  )";
      case 62: return "CM1: ATM TRACER  2  (CONVEN H2O  )";
      case 63: return "CM1: ATM TRACER  3  (CONVEN  CO  )";
      case 64: return "CM1: ATM TRACER  4  (CONVEN CH4  )";
      case 65: return "CM1: ATM TRACER  5  (CONVEN N2O  )";
      case 66: return "CM1: ATM TRACER  6  (CONVEN  NO  )";
      case 67: return "CM1: ATM TRACER  7  (CONVEN  NO2  )";
      case 68: return "CM1: ATM TRACER  8  (CONVEN HNO3  )";
      case 69: return "CM1: ATM TRACER  9  (CONVEN N2O5  )";
      case 70: return "CM1: ATM TRACER 10  (CONVEN CLONO2)";
      case 71: return "CM1: ATM TRACER 11  (CONVEN CLO  )";
      case 72: return "CM1: ATM TRACER 12  (CONVEN HCL  )";
      case 73: return "CM1: ATM TRACER 13  (CONVEN CF2CL2)";
      case 74: return "CM1: ATM TRACER 14  (CONVEN CFCL3 )";
      case 75: return "CM1: ATM TRACER 15  (CONVEN  HF  )";
      case 76: return "CM1: ATM TRACER 16  (CONVEN ----- )";
      case 77: return "CM1: ATM TRACER 17  (CONVEN ----- )";
      case 78: return "CM1: ATM TRACER 18  (CONVEN ----- )";
      case 79: return "CM1: ATM TRACER 19  (CONVEN S02  )";
      case 80: return "CM1: ATM TRACER 20  (CONVEN DMS  )";
      case 81: return "CM1: ATM TRACER 21  (CONVEN H2S  )";
      case 82: return "CM1: ATM TRACER 22 (CONV WATER-SOL )";
      case 83: return "CM1: ATM TRACER 23 (CONV DUST-LIKE )";
      case 84: return "CM1: ATM TRACER 24 (CONV OCEANIC  )";
      case 85: return "CM1: ATM TRACER 25 (CONV SOOT  )";
      case 86: return "CM1: ATM TRACER 26 (CONV VOLC ASH  )";
      case 87: return "CM1: ATM TRACER 27 (CONV H2SO4  - )";
      case 88: return "CM1: ATM TRACER 28 (CONV (NH4)2SO4 )";
      case 89: return "CM1: ATM TRACER 29 (CONV MINERAL  )";
      case 201: return "CM1: U COMP OF WIND ON PRESSURE LEV.";
      case 202: return "CM1: V COMP OF WIND ON PRESSURE LEV.";
      case 203: return "CM1: VERT VEL.(OMEGA): PRESSURE LEV.";
      case 205: return "CM1: CAT PROBABILITY";
      case 206: return "CM1: HEIGHT OF THE MAX WIND LEVEL";
      case 207: return "CM1: PRESSURE OF THE MAX WIND LEVEL";
      case 208: return "CM1: U COMPONENT OF THE MAX WIND";
      case 209: return "CM1: V COMPONENT OF THE MAX WIND";
      case 210: return "CM1: GEOPOTENTIAL HT. MODEL HALF LEV";
      case 211: return "CM1: GEOPOTENTIAL HT. PRESSURE LEV.";
      case 212: return "CM1: TEMPERATURE ON PRESSURE LEVELS";
      case 213: return "CM1: RELATIVE HUMIDITY ON PRESS LEVS";
      case 214: return "CM1: WET BULB POTENTIAL TEMPERATURE";
      case 215: return "CM1: SURFACE SNOW PROBABILITY";
      case 216: return "CM1: HEIGHT OF THE FREEZING LEVEL";
      case 217: return "CM1: PRESSURE OF THE FREEZING LEVEL";
      case 218: return "CM1: PRESSURE AT THE TROPOPAUSE LEV.";
      case 219: return "CM1: TEMPERATURE AT THE TROP. LEVEL";
      case 220: return "CM1: HEIGHT OF THE TROPOPAUSE";
      case 221: return "CM1: HIGH CLOUD AMOUNT";
      case 222: return "CM1: MEDIUM CLOUD AMOUNT";
      case 223: return "CM1: LOW CLOUD AMOUNT";
      case 224: return "CM1: PRESSURE AT MEAN SEA LEVEL";
      }
      break;
    case 22:
      switch (item) {
      case 1: return "CM2: PSTAR AFTER TIMESTEP";
      case 2: return "CM2: U COMPNT OF WIND AFTER TIMESTEP";
      case 3: return "CM2: V COMPNT OF WIND AFTER TIMESTEP";
      case 4: return "CM2: THETA AFTER TIMESTEP";
      case 10: return "CM2: SPECIFIC HUMIDITY AFTER TSTEP";
      case 13: return "CM2: CONV CLOUD AMOUNT AFTER TSTEP";
      case 14: return "CM2: CONV CLOUD BASE LEV NO AFTER TS";
      case 15: return "CM2: CONV CLOUD TOP LEV NO AFTER TS";
      case 16: return "CM2: CONV CLOUD LIQUID WATER PATH";
      case 23: return "CM2: SNOW AMOUNT AFTER TSTEP  KG/M2";
      case 24: return "CM2: SURFACE TEMPERATURE AFTER TS";
      case 25: return "CM2: BOUNDARY LAYER DEPTH AFTER TS";
      case 26: return "CM2: ROUGHNESS LENGTH AFTER TIMESTEP";
      case 28: return "CM2: SURFACE ZONAL CURRENT AFTER TS";
      case 29: return "CM2: SURFACE MERID CURRENT AFTER TS";
      case 31: return "CM2: SEA ICE FRACTION AFTER TIMESTEP";
      case 32: return "CM2: SEA ICE DEPTH (MEAN OVER ICE) M";
      case 61: return "CM2: ATM TRACER  1  (CONVEN  O3  )";
      case 62: return "CM2: ATM TRACER  2  (CONVEN H2O  )";
      case 63: return "CM2: ATM TRACER  3  (CONVEN  CO  )";
      case 64: return "CM2: ATM TRACER  4  (CONVEN CH4  )";
      case 65: return "CM2: ATM TRACER  5  (CONVEN N2O  )";
      case 66: return "CM2: ATM TRACER  6  (CONVEN  NO  )";
      case 67: return "CM2: ATM TRACER  7  (CONVEN  NO2  )";
      case 68: return "CM2: ATM TRACER  8  (CONVEN HNO3  )";
      case 69: return "CM2: ATM TRACER  9  (CONVEN N2O5  )";
      case 70: return "CM2: ATM TRACER 10  (CONVEN CLONO2)";
      case 71: return "CM2: ATM TRACER 11  (CONVEN CLO  )";
      case 72: return "CM2: ATM TRACER 12  (CONVEN HCL  )";
      case 73: return "CM2: ATM TRACER 13  (CONVEN CF2CL2)";
      case 74: return "CM2: ATM TRACER 14  (CONVEN CFCL3 )";
      case 75: return "CM2: ATM TRACER 15  (CONVEN  HF  )";
      case 76: return "CM2: ATM TRACER 16  (CONVEN ----- )";
      case 77: return "CM2: ATM TRACER 17  (CONVEN ----- )";
      case 78: return "CM2: ATM TRACER 18  (CONVEN ----- )";
      case 79: return "CM2: ATM TRACER 19  (CONVEN ----  )";
      case 80: return "CM2: ATM TRACER 20  (CONVEN ----- )";
      case 81: return "CM2: ATM TRACER 21  (CONVEN H2S  )";
      case 82: return "CM2: ATM TRACER 22 (CONV WATER-SOL )";
      case 83: return "CM2: ATM TRACER 23 (CONV DUST-LIKE )";
      case 84: return "CM2: ATM TRACER 24 (CONV OCEANIC  )";
      case 85: return "CM2: ATM TRACER 25 (CONV SOOT  )";
      case 86: return "CM2: ATM TRACER 26 (CONV VOLC ASH  )";
      case 87: return "CM2: ATM TRACER 27 (CONV H2SO4  )";
      case 88: return "CM2: ATM TRACER 28 (CONV (NH4)2SO4 )";
      case 89: return "CM2: ATM TRACER 29 (MINERAL  )";
      case 201: return "CM2: U COMP OF WIND ON PRESSURE LEV.";
      case 202: return "CM2: V COMP OF WIND ON PRESSURE LEV.";
      case 203: return "CM2: VERT VEL.(OMEGA): PRESSURE LEV.";
      case 205: return "CM2: CAT PROBABILITY";
      case 206: return "CM2: HEIGHT OF THE MAX WIND LEVEL";
      case 207: return "CM2: PRESSURE OF THE MAX WIND LEVEL";
      case 208: return "CM2: U COMPONENT OF THE MAX WIND";
      case 209: return "CM2: V COMPONENT OF THE MAX WIND";
      case 210: return "CM2: GEOPOTENTIAL HT. MODEL HALF LEV";
      case 211: return "CM2: GEOPOTENTIAL HT. PRESSURE LEV.";
      case 212: return "CM2: TEMPERATURE ON PRESSURE LEVELS";
      case 213: return "CM2: RELATIVE HUMIDITY ON PRESS LEVS";
      case 214: return "CM2: WET BULB POTENTIAL TEMPERATURE";
      case 215: return "CM2: SURFACE SNOW PROBABILITY";
      case 216: return "CM2: HEIGHT OF THE FREEZING LEVEL";
      case 217: return "CM2: PRESSURE OF THE FREEZING LEVEL";
      case 218: return "CM2: PRESSURE AT THE TROPOPAUSE LEV.";
      case 219: return "CM2: TEMPERATURE AT THE TROP. LEVEL";
      case 220: return "CM2: HEIGHT OF THE TROPOPAUSE";
      case 221: return "CM2: HIGH CLOUD AMOUNT";
      case 222: return "CM2: MEDIUM CLOUD AMOUNT";
      case 223: return "CM2: LOW CLOUD AMOUNT";
      case 224: return "CM2: PRESSURE AT MEAN SEA LEVEL";
      }
      break;
    case 23:
      switch (item) {
      case 1: return "CM3: PSTAR AFTER TIMESTEP";
      case 2: return "CM3: U COMPNT OF WIND AFTER TIMESTEP";
      case 3: return "CM3: V COMPNT OF WIND AFTER TIMESTEP";
      case 4: return "CM3: THETA AFTER TIMESTEP";
      case 10: return "CM3: SPECIFIC HUMIDITY AFTER TSTEP";
      case 13: return "CM3: CONV CLOUD AMOUNT AFTER TSTEP";
      case 14: return "CM3: CONV CLOUD BASE LEV NO AFTER TS";
      case 15: return "CM3: CONV CLOUD TOP LEV NO AFTER TS";
      case 16: return "CM3: CONV CLOUD LIQUID WATER PATH";
      case 23: return "CM3: SNOW AMOUNT AFTER TSTEP  KG/M2";
      case 24: return "CM3: SURFACE TEMPERATURE AFTER TS";
      case 25: return "CM3: BOUNDARY LAYER DEPTH AFTER TS";
      case 26: return "CM3: ROUGHNESS LENGTH AFTER TIMESTEP";
      case 28: return "CM3: SURFACE ZONAL CURRENT AFTER TS";
      case 29: return "CM3: SURFACE MERID CURRENT AFTER TS";
      case 31: return "CM3: SEA ICE FRACTION AFTER TIMESTEP";
      case 32: return "CM3: SEA ICE DEPTH (MEAN OVER ICE) M";
      case 61: return "CM3: ATM TRACER  1  (CONVEN  O3  )";
      case 62: return "CM3: ATM TRACER  2  (CONVEN H2O  )";
      case 63: return "CM3: ATM TRACER  3  (CONVEN  CO  )";
      case 64: return "CM3: ATM TRACER  4  (CONVEN CH4  )";
      case 65: return "CM3: ATM TRACER  5  (CONVEN N2O  )";
      case 66: return "CM3: ATM TRACER  6  (CONVEN  NO  )";
      case 67: return "CM3: ATM TRACER  7  (CONVEN  NO2  )";
      case 68: return "CM3: ATM TRACER  8  (CONVEN HNO3  )";
      case 69: return "CM3: ATM TRACER  9  (CONVEN N2O5  )";
      case 70: return "CM3: ATM TRACER 10  (CONVEN CLONO2)";
      case 71: return "CM3: ATM TRACER 11  (CONVEN CLO  )";
      case 72: return "CM3: ATM TRACER 12  (CONVEN HCL  )";
      case 73: return "CM3: ATM TRACER 13  (CONVEN CF2CL2)";
      case 74: return "CM3: ATM TRACER 14  (CONVEN CFCL3 )";
      case 75: return "CM3: ATM TRACER 15  (CONVEN  HF  )";
      case 76: return "CM3: ATM TRACER 16  (CONVEN ----- )";
      case 77: return "CM3: ATM TRACER 17  (CONVEN ----- )";
      case 78: return "CM3: ATM TRACER 18  (CONVEN ----- )";
      case 79: return "CM3: ATM TRACER 19  (CONVEN ----  )";
      case 80: return "CM3: ATM TRACER 20  (CONVEN ----  )";
      case 81: return "CM3: ATM TRACER 21  (CONVEN H2S  )";
      case 82: return "CM3: ATM TRACER 22 (CONV WATER-SOL )";
      case 83: return "CM3: ATM TRACER 23 (CONV DUST-LIKE )";
      case 84: return "CM3: ATM TRACER 24 (CONV OCEANIC  )";
      case 85: return "CM3: ATM TRACER 25 (CONV SOOT  )";
      case 86: return "CM3: ATM TRACER 26 (CONV VOLC ASH  )";
      case 87: return "CM3: ATM TRACER 27 (CONV H2SO4  )";
      case 88: return "CM3: ATM TRACER 28 (CONV (NH4)2SO4 )";
      case 89: return "CM3: ATM TRACER 29 (CONV MINERAL  )";
      case 201: return "CM3: U COMP OF WIND ON PRESSURE LEV.";
      case 202: return "CM3: V COMP OF WIND ON PRESSURE LEV.";
      case 203: return "CM3: VERT VEL.(OMEGA): PRESSURE LEV.";
      case 205: return "CM3: CAT PROBABILITY";
      case 206: return "CM3: HEIGHT OF THE MAX WIND LEVEL";
      case 207: return "CM3: PRESSURE OF THE MAX WIND LEVEL";
      case 208: return "CM3: U COMPONENT OF THE MAX WIND";
      case 209: return "CM3: V COMPONENT OF THE MAX WIND";
      case 210: return "CM3: GEOPOTENTIAL HT. MODEL HALF LEV";
      case 211: return "CM3: GEOPOTENTIAL HT. PRESSURE LEV.";
      case 212: return "CM3: TEMPERATURE ON PRESSURE LEVELS";
      case 213: return "CM3: RELATIVE HUMIDITY ON PRESS LEVS";
      case 214: return "CM3: WET BULB POTENTIAL TEMPERATURE";
      case 215: return "CM3: SURFACE SNOW PROBABILITY";
      case 216: return "CM3: HEIGHT OF THE FREEZING LEVEL";
      case 217: return "CM3: PRESSURE OF THE FREEZING LEVEL";
      case 218: return "CM3: PRESSURE AT THE TROPOPAUSE LEV.";
      case 219: return "CM3: TEMPERATURE AT THE TROP. LEVEL";
      case 220: return "CM3: HEIGHT OF THE TROPOPAUSE";
      case 221: return "CM3: HIGH CLOUD AMOUNT";
      case 222: return "CM3: MEDIUM CLOUD AMOUNT";
      case 223: return "CM3: LOW CLOUD AMOUNT";
      case 224: return "CM3: PRESSURE AT MEAN SEA LEVEL";
      }
      break;
    case 24:
      switch (item) {
      case 1: return "CM4: PSTAR AFTER TIMESTEP";
      case 2: return "CM4: U COMPNT OF WIND AFTER TIMESTEP";
      case 3: return "CM4: V COMPNT OF WIND AFTER TIMESTEP";
      case 4: return "CM4: THETA AFTER TIMESTEP";
      case 10: return "CM4: SPECIFIC HUMIDITY AFTER TSTEP";
      case 13: return "CM4: CONV CLOUD AMOUNT AFTER TSTEP";
      case 14: return "CM4: CONV CLOUD BASE LEV NO AFTER TS";
      case 15: return "CM4: CONV CLOUD TOP LEV NO AFTER TS";
      case 16: return "CM4: CONV CLOUD LIQUID WATER PATH";
      case 23: return "CM4: SNOW AMOUNT AFTER TSTEP  KG/M2";
      case 24: return "CM4: SURFACE TEMPERATURE AFTER TS";
      case 25: return "CM4: BOUNDARY LAYER DEPTH AFTER TS";
      case 26: return "CM4: ROUGHNESS LENGTH AFTER TIMESTEP";
      case 28: return "CM4: SURFACE ZONAL CURRENT AFTER TS";
      case 29: return "CM4: SURFACE MERID CURRENT AFTER TS";
      case 31: return "CM4: SEA ICE FRACTION AFTER TIMESTEP";
      case 32: return "CM4: SEA ICE DEPTH (MEAN OVER ICE) M";
      case 61: return "CM4: ATM TRACER  1  (CONVEN  O3  )";
      case 62: return "CM4: ATM TRACER  2  (CONVEN H2O  )";
      case 63: return "CM4: ATM TRACER  3  (CONVEN  CO  )";
      case 64: return "CM4: ATM TRACER  4  (CONVEN CH4  )";
      case 65: return "CM4: ATM TRACER  5  (CONVEN N2O  )";
      case 66: return "CM4: ATM TRACER  6  (CONVEN  NO  )";
      case 67: return "CM4: ATM TRACER  7  (CONVEN  NO2  )";
      case 68: return "CM4: ATM TRACER  8  (CONVEN HNO3  )";
      case 69: return "CM4: ATM TRACER  9  (CONVEN N2O5  )";
      case 70: return "CM4: ATM TRACER 10  (CONVEN CLONO2)";
      case 71: return "CM4: ATM TRACER 11  (CONVEN CLO  )";
      case 72: return "CM4: ATM TRACER 12  (CONVEN HCL  )";
      case 73: return "CM4: ATM TRACER 13  (CONVEN CF2CL2)";
      case 74: return "CM4: ATM TRACER 14  (CONVEN CFCL3 )";
      case 75: return "CM4: ATM TRACER 15  (CONVEN  HF  )";
      case 76: return "CM4: ATM TRACER 16  (CONVEN ----- )";
      case 77: return "CM4: ATM TRACER 17  (CONVEN ----- )";
      case 78: return "CM4: ATM TRACER 18  (CONVEN ----- )";
      case 79: return "CM4: ATM TRACER 19  (CONVEN ----  )";
      case 80: return "CM4: ATM TRACER 20  (CONVEN ----  )";
      case 81: return "CM4: ATM TRACER 21  (CONVEN H2S  )";
      case 82: return "CM4: ATM TRACER 22 (CONV WATER-SOL )";
      case 83: return "CM4: ATM TRACER 23 (CONV DUST-LIKE )";
      case 84: return "CM4: ATM TRACER 24 (CONV OCEANIC  )";
      case 85: return "CM4: ATM TRACER 25 (CONV SOOT  )";
      case 86: return "CM4: ATM TRACER 26 (CONV VOLC ASH  )";
      case 87: return "CM4: ATM TRACER 27 (CONV H2SO4  )";
      case 88: return "CM4: ATM TRACER 28 (CONV (NH4)2SO4 )";
      case 89: return "CM4: ATM TRACER 29 (MINERAL  )";
      case 201: return "CM4: U COMP OF WIND ON PRESSURE LEV.";
      case 202: return "CM4: V COMP OF WIND ON PRESSURE LEV.";
      case 203: return "CM4: VERT VEL.(OMEGA): PRESSURE LEV.";
      case 205: return "CM4: CAT PROBABILITY";
      case 206: return "CM4: HEIGHT OF THE MAX WIND LEVEL";
      case 207: return "CM4: PRESSURE OF THE MAX WIND LEVEL";
      case 208: return "CM4: U COMPONENT OF THE MAX WIND";
      case 209: return "CM4: V COMPONENT OF THE MAX WIND";
      case 210: return "CM4: GEOPOTENTIAL HT. MODEL HALF LEV";
      case 211: return "CM4: GEOPOTENTIAL HT. PRESSURE LEV.";
      case 212: return "CM4: TEMPERATURE ON PRESSURE LEVELS";
      case 213: return "CM4: RELATIVE HUMIDITY ON PRESS LEVS";
      case 214: return "CM4: WET BULB POTENTIAL TEMPERATURE";
      case 215: return "CM4: SURFACE SNOW PROBABILITY";
      case 216: return "CM4: HEIGHT OF THE FREEZING LEVEL";
      case 217: return "CM4: PRESSURE OF THE FREEZING LEVEL";
      case 218: return "CM4: PRESSURE AT THE TROPOPAUSE LEV.";
      case 219: return "CM4: TEMPERATURE AT THE TROP. LEVEL";
      case 220: return "CM4: HEIGHT OF THE TROPOPAUSE";
      case 221: return "CM4: HIGH CLOUD AMOUNT";
      case 222: return "CM4: MEDIUM CLOUD AMOUNT";
      case 223: return "CM4: LOW CLOUD AMOUNT";
      case 224: return "CM4: PRESSURE AT MEAN SEA LEVEL";
      }
      break;
    }
    break;
  case 2:
    switch (section) {
    case 0:
      switch (item) {
      case 101: return "POTENTIAL TEMPERATURE (OCEAN)  DEG.C";
      case 102: return "SALINITY (OCEAN)  (PSU-35)/1000";
      case 103: return "OCN EXTRACER  1: CONVEN TCO2";
      case 104: return "OCN EXTRACER  2: CONVEN ALAKLINITY";
      case 105: return "OCN EXTRACER  3: CONVEN NUTRIENT";
      case 106: return "OCN EXTRACER  4: CONVEN PHYTOPLNKTN";
      case 107: return "OCN EXTRACER  5: CONVEN ZOOPLNKTN";
      case 108: return "OCN EXTRACER  6: CONVEN DETRITUS";
      case 109: return "OCN EXTRACER  7: CONVEN TRITIUM";
      case 110: return "OCN EXTRACER  8: CONVEN 3H+3HE";
      case 111: return "OCN EXTRACER  9: CONVEN CFC11";
      case 112: return "OCN EXTRACER 10: CONVEN CFC12";
      case 113: return "OCN EXTRACER 11: CONVEN CFC13";
      case 114: return "OCN EXTRACER 12: CONVEN CARBON14";
      case 115: return "OCN EXTRACER 13: CONVEN --------";
      case 116: return "OCN EXTRACER 14: CONVEN --------";
      case 117: return "OCN EXTRACER 15: CONVEN -------";
      case 118: return "OCN EXTRACER 16: CONVEN -------";
      case 119: return "OCN EXTRACER 17: CONVEN -------";
      case 120: return "OCN EXTRACER 18: CONVEN -------";
      case 121: return "BAROCLINIC U_VELOCITY (OCEAN)  CM/S";
      case 122: return "BAROCLINIC V_VELOCITY (OCEAN)  CM/S";
      case 130: return "STREAMFUNCTION (OCEAN)  CM3/S";
      case 131: return "STREAMFUNCTION (T-1) (OCEAN)  CM3/S";
      case 132: return "STREAMFN TENDENCY (OCEAN)  CM3/S/TS";
      case 133: return "STREAMFN TENDENCY(T-1)(OCN) CM3/S/TS";
      case 134: return "SURFACE ELEVATION (OCEAN)  CM";
      case 135: return "BAROTROPIC U_VELOCITY (OCEAN)  CM/S";
      case 136: return "BAROTROPIC V_VELOCITY (OCEAN)  CM/S";
      case 137: return "MIXED LAYER DEPTH (OCEAN)  M";
      case 139: return "ISX  X OCEAN/ICE STRESS (ICE)  N/M2";
      case 140: return "ISY  Y OCEAN/ICE STRESS  (ICE) N/M2";
      case 141: return "SNOW DEPTH (OCEAN)  M";
      case 142: return "GBM CARYHEAT MISC HEAT FLX(ICE) W/M2";
      case 143: return "GBM HEAT FLUX:OCEAN TO ICE(OCN) W/M2";
      case 144: return "RATE OF SALINITY CHANGE (ICE)  PSU/S";
      case 145: return "ICY : TRUE IF BOX CONTAINS ICE";
      case 146: return "AICE : ICE CONCENTRATION";
      case 147: return "HICE: MEAN ICE DEPTH OVER GRIDBOX  M";
      case 148: return "ICE U_VELOCITY  M/S";
      case 149: return "ICE V_VELOCITY  M/S";
      case 150: return "TAUX: X_WINDSTRESS  N/M2  A";
      case 151: return "TAUY: Y_WINDSTRESS  N/M2  A";
      case 152: return "WME: WIND MIXING ENERGY FLUX W/M2  A";
      case 161: return "SOL: PEN.SOLAR*LF INTO OCEAN W/M2  A";
      case 162: return "HTN:NONPEN.HT.FLX*LF INTO OCN W/M2 A";
      case 165: return "PLE:PRECIP-EVAP INTO OCEAN KG/M2/S A";
      case 166: return "RIVER OUTFLOW INTO OCEAN  KG/M2/S  A";
      case 167: return "WATER TYPE  A";
      case 170: return "SOLAR RADIATION OVER ICE  W/M2  A";
      case 171: return "SNOWFALL INTO OCN/ONTO ICE KG/M2/S A";
      case 172: return "SUBLIMATION FROM SEAICE  KG/M2/S A";
      case 175: return "LAT B'DARY CONDITIONS (BOUNDARY)";
      case 176: return "LAT B'DARY TENDENCIES (BOUNDARY)";
      case 180: return "REF. SEA SURF. TEMPERATURE  DEG.C  A";
      case 181: return "REF.SEA SURF.SALINITY(PSU-35)/1000 A";
      case 182: return "CLIM. AIR TEMPERATURE  DEG.C  A";
      case 183: return "CLIMATOLOGICAL ICE DEPTH  M  A";
      case 185: return "OCEAN HEAT FLUX CORRECTION  W/M2  A";
      case 186: return "P-E FLUX CORRECTION  KG/M2/S  A";
      case 190: return "GBM SEAICE TOPMELT HEAT FLUX W/M2  A";
      case 191: return "GBM SEAICE BOTMELT HEAT FLUX W/M2  A";
      case 192: return "CONJUGATE GRADIENT RESIDUAL (TS)";
      case 193: return "CONJUGATE GRADIENT RESIDUAL (TS-1)";
      case 194: return "THICKNESS DIFF COEFF (OCEAN) CM2/S";
      case 195: return "SURFACE ELEVATION(DTBT-1)(OCEAN)  CM";
      case 196: return "B'TROPIC U_FLUX (DTBT-1)(OCEAN)CM2/S";
      case 197: return "B'TROPIC V_FLUX (DTBT-1)(OCEAN)CM2/S";
      case 198: return "B'TROPIC U_FLUX (DTBC-1)(OCEAN)CM2/S";
      case 199: return "B'TROPIC V_FLUX (DTBC-1)(OCEAN)CM2/S";
      case 200: return "CO2 ATMOS LEVEL 1 CONC  ppmv";
      }
      break;
    case 30:
      switch (item) {
      case 201: return "VERT.VEL. ON OCEAN HALF LEVELS  CM/S";
      case 202: return "MIXED LAYER DEPTH  M";
      case 203: return "ANOMALOUS HEAT FLUX  W/M2";
      case 204: return "ANOMALOUS SALINITY FLUX  KG/M2/S";
      case 205: return "ANOMALOUS SEA ICE HEAT FLUX  W/M2";
      case 206: return "GBM HTN INTO OCEAN BUDGET  W/M**2";
      case 207: return "SNOWRATE WHERE NO ICE KG M**-2 S**-1";
      case 208: return "CARYHEAT AFTER ROW CALCULATION  W/M2";
      case 210: return "DTHETA/DT IN TOTAL  K/Gs";
      case 211: return "MEAD DIAGNOSTICS: TEMPERATURE  W";
      case 212: return "MEAD DIAGNOSTICS: SALINITY  KG/S";
      case 213: return "MEAD DIAGNOSTICS: EXTRA TRACER 1";
      case 214: return "MEAD DIAGNOSTICS: EXTRA TRACER 2";
      case 215: return "MEAD DIAGNOSTICS: EXTRA TRACER 3";
      case 216: return "MEAD DIAGNOSTICS: EXTRA TRACER 4";
      case 217: return "MEAD DIAGNOSTICS: EXTRA TRACER 5";
      case 218: return "MEAD DIAGNOSTICS: EXTRA TRACER 6";
      case 219: return "MEAD DIAGNOSTICS: EXTRA TRACER 7";
      case 220: return "MEAD DIAGNOSTICS: EXTRA TRACER 8";
      case 221: return "MEAD DIAGNOSTICS: EXTRA TRACER 9";
      case 222: return "MEAD DIAGNOSTICS: EXTRA TRACER 10";
      case 223: return "MEAD DIAGNOSTICS: EXTRA TRACER 11";
      case 224: return "MEAD DIAGNOSTICS: EXTRA TRACER 12";
      case 225: return "MEAD DIAGNOSTICS: EXTRA TRACER 13";
      case 226: return "MEAD DIAGNOSTICS: EXTRA TRACER 14";
      case 227: return "MEAD DIAGNOSTICS: EXTRA TRACER 15";
      case 228: return "MEAD DIAGNOSTICS: EXTRA TRACER 16";
      case 229: return "MEAD DIAGNOSTICS: EXTRA TRACER 17";
      case 230: return "MEAD DIAGNOSTICS: EXTRA TRACER 18";
      case 231: return "DTHETA/DT FROM X-ADVECTION  K/Gs";
      case 232: return "DTHETA/DT FROM Y-ADVECTION  K/Gs";
      case 233: return "DTHETA/DT FROM Z-ADVECTION  K/Gs";
      case 234: return "DTHETA/DT FROM X-DIFFUSION  K/Gs";
      case 235: return "DTHETA/DT FROM Y-DIFFUSION  K/Gs";
      case 236: return "DTHETA/DT FROM Z-DIFFUSION  K/Gs";
      case 237: return "DTHETA/DT FROM SFC. FLUXES  K/Gs";
      case 238: return "DTHETA/DT FROM PEN. SOLAR  K/Gs";
      case 239: return "DTHETA/DT FROM ICE PHYSICS  K/Gs";
      case 240: return "DTHETA/DT FROM ML PHYSICS  K/Gs";
      case 241: return "DTHETA/DT FROM CONVECTION  K/Gs";
      case 242: return "DTHETA/DT FROM ADVECTION  K/Gs";
      case 243: return "DTHETA/DT FROM FOURIER FILT  K/Gs";
      case 244: return "DTHETA/DT FROM ROBERT FILT  K/Gs";
      case 245: return "DTHETA/DT FROM MED. OUTFLOW  K/Gs";
      case 246: return "BAROCLINIC X-ACCN (ZUN)  CM/S**2";
      case 247: return "BAROCLINIC Y-ACCN (ZVN)  CM/S**2";
      case 248: return "PCO2 (OCEAN)";
      case 249: return "CO2 FLUX (AIR-TO-SEA) (MOLE-C/M2/YR)";
      case 250: return "CO2 INVASION RATE (MOLE-C/M2/YR)";
      case 251: return "CO2 EVASION RATE (MOLE-C/M2/YR)";
      case 252: return "PRIMARY PRODUCTION (GC/M2/DAY)";
      case 253: return "ZOOPLTN PRODUCTION (GC/M2/DAY)";
      case 254: return "PHYTO SPECIFIC GROWTH RATE (1/DAY)";
      case 255: return "PHYTO SPECIFIC GRAZING RATE (1/DAY)";
      case 256: return "PHYTO SPECIFIC MORTALITY (1/DAY)";
      case 257: return "NITRATE GAIN-EXCRETION (MMOL-N/M2/D)";
      case 258: return "NITRATE LOSS - GROWTH (MMOL-N/M2/D)";
      case 259: return "NITRATE GAIN-PHY MORT (MMOL-N/M2/D)";
      case 260: return "NITRATE GAIN-ZOO MORT (MMOL-N/M2/D)";
      case 261: return "NITRATE GAIN-PHY RESP (MMOL-N/M2/D)";
      case 262: return "NITRATE GAIN-REMIN  (MMOL-N/M2/D)";
      case 263: return "NUTRIENT LIMITATION";
      case 264: return "LIGHT LIMITATION";
      case 265: return "TEMPERATURE LIMITATION";
      case 266: return "DETRITUS FLUX  (MMOL-N/M2/D)";
      case 267: return "VERTICAL NITRATE FLUX  (MMOL-N/M2/D)";
      case 268: return "HORIZ NITRATE ADVECT RATE(MMOL/M3/D)";
      case 269: return "VERT NITRATE ADVECTN RATE(MMOL/M3/D)";
      case 270: return "HORIZ NITRATE DIFFUSION  (MMOL/M3/D)";
      case 271: return "VERT NITRATE DIFFUSION  (MMOL/M3/D)";
      case 272: return "NITRATE MIXING DUE TO MLM(MMOL/M3/D)";
      case 273: return "NITRATE CONVECTION  (MMOL/M3/D)";
      case 274: return "NITRATE CHANGE - BIOLOGY (MMOL/M3/D)";
      case 275: return "NITRATE CHANGE-RESETTING (MMOL/M3/D)";
      case 276: return "HORIZ PHYTO ADVECT RATE(MMOL-N/M3/D)";
      case 277: return "HORIZ ZOO  ADVECT RATE(MMOL-N/M3/D)";
      case 278: return "HORIZ DETRI ADVECT RATE(MMOL-N/M3/D)";
      case 279: return "ANOM. HEAT \"SINK\" AT OCN FLOOR  W/M2";
      case 280: return "WATER_FLUX*SALINITY/DENSITY m Gs**-1";
      case 281: return "GM EDDY U VELOCITY (OCEAN)";
      case 282: return "GM EDDY V VELOCITY (N FACE) (OCEAN)";
      case 283: return "GM EDDY W VEL (TOP FACE) (OCEAN)";
      case 284: return "DTHETA/DT FROM G&MCW SCHEME  K/Gs";
      case 285: return "RIGID-LID PRESSURE/PA";
      case 286: return "OLD- DO NOT USE: KM";
      case 287: return "OLD- DO NOT USE: KT";
      case 288: return "OLD- DO NOT USE: RIM";
      case 289: return "OLD- DO NOT USE: RIT";
      case 290: return "HM MAX DEPTH LARGE SCHEME MOMENTUM";
      case 291: return "HT MAX DEPTH LARGE SCHEME TRACER";
      case 292: return "OLD- DO NOT USE: VIRT A-S FLUX CO2";
      case 293: return "OLD- DO NOT USE: VIRT A-S FLUX ALK";
      case 294: return "RICHARDSON NO CALC FROM MLD ROUTINE";
      case 296: return "KM VERT MOM DIFF COEFF";
      case 297: return "KT VERT TRACER DIFF COEFF";
      case 298: return "RIM RICHARDSON NUMBER MOMENTUM";
      case 299: return "RIT RICHARDSON NUMBER TRACER";
      case 301: return "TEMPERATURE (OCEAN)  DEG.C";
      case 302: return "LM MONIN OBUKHOV LENGTH MOMENTUM";
      case 303: return "LT MONIN OBUKHOV LENGTH TRACER";
      case 306: return "DS/DT FROM X-ADVECTION  (OCN) Gs**-1";
      case 307: return "DS/DT FROM Y-ADVECTION  (OCN) Gs**-1";
      case 308: return "DS/DT FROM Z-ADVECTION  (OCN) Gs**-1";
      case 309: return "DS/DT FROM X-DIFFUSION  (OCN) Gs**-1";
      case 310: return "DS/DT FROM Y-DIFFUSION  (OCN) Gs**-1";
      case 311: return "DS/DT FROM Z-DIFFUSION  (OCN) Gs**-1";
      case 312: return "DS/DT FROM SFC. FLUXES  (OCN) Gs**-1";
      case 313: return "DS/DT FROM ICE PHYSICS  (OCN) Gs**-1";
      case 314: return "DS/DT FROM ML PHYSICS  (OCN) Gs**-1";
      case 315: return "DS/DT FROM CONVECTION  (OCN) Gs**-1";
      case 316: return "DS/DT FROM ADVECTION  Gs**-1";
      case 317: return "DS/DT FROM FOURIER FILT (OCN) Gs**-1";
      case 318: return "DS/DT FROM ROBERT FILT  (OCN) Gs**-1";
      case 319: return "DS/DT FROM MED. OUTFLOW (OCN) Gs**-1";
      case 320: return "TOTAL OCEAN U-VELOCITY  CM S**-1";
      case 321: return "TOTAL OCEAN V-VELOCITY  CM S**-1";
      case 322: return "DS/DT FROM G&MCW SCHEME  Gs**-1";
      }
      break;
    case 31:
      switch (item) {
      case 201: return "HR: RECIP.OF DEPTH AT UV POINTS 1/CM";
      case 202: return "CHANGE IN VORTICITY ACROSS TSTEP:ZTD";
      case 211: return "ZMN VORTY FORCING: ADVECTION  S*-2";
      case 212: return "ZMN VORTY FORCING: HOR DIFFN  S*-2";
      case 213: return "ZMN VORTY FORCING: VRT DIFFN  S*-2";
      case 214: return "ZMN VORTY FORCING: CORIOLIS  S*-2";
      case 215: return "ZMN VORTY FORCING: PRESSURE  S*-2";
      case 216: return "INT VORTY FORCING: ADVECTION CM S*-2";
      case 217: return "INT VORTY FORCING: HOR DIFFN CM S*-2";
      case 218: return "INT VORTY FORCING: VRT DIFFN CM S*-2";
      case 219: return "INT VORTY FORCING: CORIOLIS  CM S*-2";
      case 220: return "INT VORTY FORCING: BOTTOM P  CM S*-2";
      }
      break;
    case 32:
      switch (item) {
      case 201: return "AICE INC. DUE TO DYNAMICS FRACT/TS";
      case 202: return "HICE INC. DUE TO DYNAMICS  M/TS";
      case 203: return "GBM SNOWDEPTH DYNAMIC INC  M/TS";
      case 204: return "HICE INC. DUE TO DIFFUSION  M/TS";
      case 209: return "U COMPONENT OF ICE VELOCITY (M.S-1)";
      case 210: return "V COMPONENT OF ICE VELOCITY (M.S-1)";
      case 211: return "AICE INC. (THERMODYNAMIC) FRACT/TS";
      case 212: return "HICE INC. (THERMODYNAMIC)  M/TS";
      case 213: return "GBM SNOWDEPTH THERMODYNAMIC INC M/TS";
      case 214: return "GBM HTN INTO ICE BUDGET  W/M**2";
      case 215: return "SNOWRATE WHERE ICY  KG M**-2 S**-1";
      case 216: return "OCN TOP-LEVEL TEMPERATURE  K";
      case 217: return "GBM HTN INTO OCN WHERE ICY  W/M**2";
      case 218: return "GBM SNOWDEPTH ON SEA-ICE  M";
      case 219: return "U CPT OF OCEAN STRESS ON ICE  Pa";
      case 220: return "V CPT OF OCEAN STRESS ON ICE  Pa";
      case 221: return "U CPT OF CORIOLIS STRESS ON ICE  Pa";
      case 222: return "V CPT OF CORIOLIS STRESS ON ICE  Pa";
      case 223: return "d/dt AICE DYNAMICS  s-1";
      case 224: return "d/dt HICE DYNAMICS  m s-1";
      case 225: return "d/dt GBM SNOWDEPTH DYNAMICS  m s-1";
      case 226: return "d/dt HICE DIFFUSION  m s-1";
      case 227: return "d/dt AICE THERMODYN  s-1";
      case 228: return "d/dt HICE THERMODYN  m s-1";
      case 229: return "d/dt GBM SNOWDEPTH THERMODYN  m s-1";
      case 230: return "U CPT OF INTERNAL ICE STRESS  Pa";
      case 231: return "V CPT OF INTERNAL ICE STRESS  Pa";
      }
      break;
    case 35:
      switch (item) {
      case 101: return "POTENTIAL TEMPERATURE (OCEAN)  DEG.C";
      case 102: return "SALINITY (OCEAN)  (PSU-35)/1000";
      case 121: return "BAROCLINIC U_VELOCITY (OCEAN)  CM/S";
      case 122: return "BAROCLINIC V_VELOCITY (OCEAN)  CM/S";
      case 130: return "STREAMFUNCTION (OCEAN)  CM3/S";
      case 134: return "SURFACE ELEVATION (OCEAN)  CM";
      case 135: return "BAROTROPIC U_VELOCITY (OCEAN)  CM/S";
      case 136: return "BAROTROPIC V_VELOCITY (OCEAN)  CM/S";
      case 137: return "MIXED LAYER DEPTH (OCEAN)  M";
      case 201: return "OCEAN ASSIM SRFC. HEIGHT WEIGHTS";
      case 202: return "OCEAN ASSIM MIXED LAYER DEPTH WTS";
      case 203: return "OCEAN ASSIM SRFC. TEMP. WEIGHTS";
      case 204: return "OCEAN ASSIM POT. TEMP. WEIGHTS";
      case 205: return "OCEAN ASSIM SALINE WEIGHTS";
      case 206: return "OCEAN ASSIM VELOCITY WEIGHTS";
      case 211: return "OCEAN ASSIM SRFC. HEIGHT INCREMENTS";
      case 213: return "OCEAN ASSIM SRFC. TEMP. INCREMENTS";
      case 214: return "OCEAN ASSIM POT. TEMP. INCREMENTS";
      case 215: return "OCEAN ASSIM SALINE INCREMENTS";
      case 221: return "OCN ASSM MERID VEL. INCS AFTER SHGHT";
      case 224: return "OCN ASSM MERID VEL. INCS AFTER THRML";
      case 225: return "OCN ASSM MERID VEL. INCS AFTER SALIN";
      case 231: return "OCN ASSM ZONAL VEL. INCS AFTER SHGHT";
      case 234: return "OCN ASSM ZONAL VEL. INCS AFTER THRML";
      case 235: return "OCN ASSM ZONAL VEL. INCS AFTER SALIN";
      case 241: return "OCN ASSM PRESS. INCS AFTER SHGHT ANA";
      case 244: return "OCN ASSM PRESS. INCS AFTER THRML ANA";
      case 245: return "OCN ASSM PRESS. INCS AFTER SALIN ANA";
      case 251: return "OCN ASSM PTM INCR AFTER SRF HGT ANAL";
      case 254: return "OCN ASSM SAL INCR AFTER SRF HGT ANAL";
      case 255: return "OCN ASSM VRT DISP AFTER SRF HGT ANAL";
      }
      break;
    case 41:
      switch (item) {
      case 101: return "CM1: POTENTIAL TEMP. (OCEAN)  DEG.C";
      case 102: return "CM1: SALINITY (OCEAN) (PSU-35)/1000";
      case 103: return "CM1: OCN EXTRACER  1: CON TCO2";
      case 104: return "CM1: OCN EXTRACER  2: CON ALKALINITY";
      case 105: return "CM1: OCN EXTRACER  3: CON NUTRIENT";
      case 106: return "CM1: OCN EXTRACER  4: CON PHYTOPLNKT";
      case 107: return "CM1: OCN EXTRACER  5: CON ZOOPLNKTN";
      case 108: return "CM1: OCN EXTRACER  6: CON DETRITUS";
      case 109: return "CM1: OCN EXTRACER  7: CON TRITIUM";
      case 110: return "CM1: OCN EXTRACER  8: CON 3H+3HE";
      case 111: return "CM1: OCN EXTRACER  9: CON CFC11";
      case 112: return "CM1: OCN EXTRACER 10: CON CFC12";
      case 113: return "CM1: OCN EXTRACER 11: CON CFC13";
      case 114: return "CM1: OCN EXTRACER 12: CON CARBON14";
      case 115: return "CM1: OCN EXTRACER 13: CON -------";
      case 116: return "CM1: OCN EXTRACER 14: CON -------";
      case 117: return "CM1: OCN EXTRACER 15: CON -------";
      case 118: return "CM1: OCN EXTRACER 16: CON -------";
      case 119: return "CM1: OCN EXTRACER 17: CON -------";
      case 120: return "CM1: OCN EXTRACER 18: CON -------";
      case 121: return "CM1: BAROCLINIC U_VEL (OCEAN) CM/S";
      case 122: return "CM1: BAROCLINIC V_VEL (OCEAN) CM/S";
      case 130: return "CM1: STREAMFUNCTION (OCEAN)  CM3/S";
      case 131: return "CM1: STREAMFTN (T-1) (OCEAN)  CM3/S";
      case 132: return "CM1: STREAMFTN TENDENCY(OC) CM3/S/TS";
      case 133: return "CM1: STREAMFTN TENDENCY(T-1)(OCEAN)";
      case 134: return "CM1: SURFACE ELEVATION (OCEAN)  CM";
      case 135: return "CM1: BAROTROPIC U_VEL (OCEAN) CM/S";
      case 136: return "CM1: BAROTROPIC V_VEL (OCEAN) CM/S";
      case 137: return "CM1: MIXED LAYER DEPTH (OCEAN)  M";
      case 139: return "CM1: ISX OCEAN/ICE STRESS(ICE)  N/M2";
      case 140: return "CM1: ISY OCEAN/ICE STRESS(ICE)  N/M2";
      case 141: return "CM1: SNOW DEPTH (OCEAN)  M";
      case 142: return "CM1: GBM CARYHEAT MISC HEAT FLX(ICE)";
      case 143: return "CM1: GBM HEAT FLUX:OCEAN TO ICE(OCN)";
      case 144: return "CM1: RATE OF SAL CHANGE (ICE)  PSU/S";
      case 145: return "CM1: ICY : TRUE IF SOME ICE";
      case 146: return "CM1: AICE : ICE CONCENTRATION";
      case 147: return "CM1: HICE : MEAN ICE DEPTH  M";
      case 148: return "CM1: ICE U_VELOCITY  M/S";
      case 149: return "CM1: ICE V_VELOCITY  M/S";
      case 150: return "CM1: TAUX: X_WINDSTRESS  N/M2  A";
      case 151: return "CM1: TAUY: Y_WINDSTRESS  N/M2  A";
      case 152: return "CM1: WME: WIND MIXING EN FLUX W/M2 A";
      case 161: return "CM1: SOL:PEN.SOLAR*LF INTO OC W/M2 A";
      case 162: return "CM1: HTN:NONPEN.HT.FLX*LF INTO OCN A";
      case 165: return "CM1: PLE:PRECIP-EVAP INTO OCEAN  A";
      case 166: return "CM1: RIVER OUTFLOW INTO OCEAN  A";
      case 167: return "CM1: WATER TYPE  A";
      case 170: return "CM1: SOLAR RADIATION OVER ICE W/M2 A";
      case 171: return "CM1: SNOWFALL INTO OCEAN/ONTO ICE  A";
      case 172: return "CM1: SUBLIMATION FROM SEAICE  A";
      case 180: return "CM1: REF. SEA SURFACE TEMP. DEG.C  A";
      case 181: return "CM1: REF. SEA SURFACE SALINITY  A";
      case 182: return "CM1: CLIM. AIR TEMPERATURE DEG.C  A";
      case 183: return "CM1: CLIMATOLOGICAL ICE DEPTH M  A";
      case 185: return "CM1: OCEAN HEAT FLUX CORRN. W/M2  A";
      case 186: return "CM1: P-E FLUX CORRECTION KG/M2/S  A";
      case 187: return "CM1: S-ICE HT FLUX CORRN: DIABLED";
      case 190: return "CM1: GBM SEAICE TOPMELT HEAT FLUX  A";
      case 191: return "CM1: GBM SEAICE BOTMELT HEAT FLUX  A";
      }
      break;
    case 42:
      switch (item) {
      case 101: return "CM2: POTENTIAL TEMP. (OCEAN)  DEG.C";
      case 102: return "CM2: SALINITY (OCEAN) (PSU-35)/1000";
      case 103: return "CM2: OCN EXTRACER  1: CON TCO2";
      case 104: return "CM2: OCN EXTRACER  2: CON ALKALINITY";
      case 105: return "CM2: OCN EXTRACER  3: CON NUTRIENT";
      case 106: return "CM2: OCN EXTRACER  4: CON PHYTOPLNKT";
      case 107: return "CM2: OCN EXTRACER  5: CON ZOOPLNKTN";
      case 108: return "CM2: OCN EXTRACER  6: CON DETRIUS";
      case 109: return "CM2: OCN EXTRACER  7: CON TRITIUM";
      case 110: return "CM2: OCN EXTRACER  8: CON 3H+3HE";
      case 111: return "CM2: OCN EXTRACER  9: CON CFC11";
      case 112: return "CM2: OCN EXTRACER 10: CON CFC12";
      case 113: return "CM2: OCN EXTRACER 11: CON CFC13";
      case 114: return "CM2: OCN EXTRACER 12: CON CARBON14";
      case 115: return "CM2: OCN EXTRACER 13: CON -------";
      case 116: return "CM2: OCN EXTRACER 14: CON -------";
      case 117: return "CM2: OCN EXTRACER 15: CON -------";
      case 118: return "CM2: OCN EXTRACER 16: CON -------";
      case 119: return "CM2: OCN EXTRACER 17: CON -------";
      case 120: return "CM2: OCN EXTRACER 18: CON -------";
      case 121: return "CM2: BAROCLINIC U_VEL (OCEAN) CM/S";
      case 122: return "CM2: BAROCLINIC V_VEL (OCEAN) CM/S";
      case 130: return "CM2: STREAMFUNCTION (OCEAN)  CM3/S";
      case 131: return "CM2: STREAMFTN (T-1) (OCEAN)  CM3/S";
      case 132: return "CM2: STREAMFTN TENDENCY(OC) CM3/S/TS";
      case 133: return "CM2: STREAMFTN TENDENCY(T-1)(OCEAN)";
      case 134: return "CM2: SURFACE ELEVATION (OCEAN)  CM";
      case 135: return "CM2: BAROTROPIC U_VEL (OCEAN) CM/S";
      case 136: return "CM2: BAROTROPIC V_VEL (OCEAN) CM/S";
      case 137: return "CM2: MIXED LAYER DEPTH (OCEAN)  M";
      case 139: return "CM2: ISX OCEAN/ICE STRESS(ICE)  N/M2";
      case 140: return "CM2: ISY OCEAN/ICE STRESS(ICE)  N/M2";
      case 141: return "CM2: SNOW DEPTH (OCEAN)  M";
      case 142: return "CM2: GBM CARYHEAT MISC HEAT FLX(ICE)";
      case 143: return "CM2: GBM HEAT FLUX:OCEAN TO ICE(OCN)";
      case 144: return "CM2: RATE OF SAL CHANGE (ICE)  PSU/S";
      case 145: return "CM2: ICY : TRUE IF SOME ICE";
      case 146: return "CM2: AICE : ICE CONCENTRATION";
      case 147: return "CM2: HICE : MEAN ICE DEPTH  M";
      case 148: return "CM2: ICE U_VELOCITY  M/S";
      case 149: return "CM2: ICE V_VELOCITY  M/S";
      case 150: return "CM2: TAUX: X_WINDSTRESS  N/M2  A";
      case 151: return "CM2: TAUY: Y_WINDSTRESS  N/M2  A";
      case 152: return "CM2: WME: WIND MIXING EN FLUX W/M2 A";
      case 161: return "CM2: SOL:PEN.SOLAR*LF INTO OC W/M2 A";
      case 162: return "CM2: HTN:NONPEN.HT.FLX*LF INTO OCN A";
      case 165: return "CM2: PLE:PRECIP-EVAP INTO OCEAN  A";
      case 166: return "CM2: RIVER OUTFLOW INTO OCEAN  A";
      case 167: return "CM2: WATER TYPE  A";
      case 170: return "CM2: SOLAR RADIATION OVER ICE W/M2 A";
      case 171: return "CM2: SNOWFALL INTO OCEAN/ONTO ICE  A";
      case 172: return "CM2: SUBLIMATION FROM SEAICE  A";
      case 180: return "CM2: REF. SEA SURFACE TEMP. DEG.C  A";
      case 181: return "CM2: REF. SEA SURFACE SALINITY  A";
      case 182: return "CM2: CLIM. AIR TEMPERATURE DEG.C  A";
      case 183: return "CM2: CLIMATOLOGICAL ICE DEPTH M  A";
      case 185: return "CM2: OCEAN HEAT FLUX CORRN. W/M2  A";
      case 186: return "CM2: P-E FLUX CORRECTION KG/M2/S  A";
      case 187: return "CM2: S-ICE HT FLUX CORRN. DIABLED";
      case 190: return "CM2: GBM SEAICE TOPMELT HEAT FLUX  A";
      case 191: return "CM2: GBM SEAICE BOTMELT HEAT FLUX  A";
      }
      break;
    case 43:
      switch (item) {
      case 101: return "CM3: POTENTIAL TEMP. (OCEAN)  DEG.C";
      case 102: return "CM3: SALINITY (OCEAN) (PSU-35)/1000";
      case 103: return "CM3: OCN EXTRACER  1: CON TCO2";
      case 104: return "CM3: OCN EXTRACER  2: CON ALKALINITY";
      case 105: return "CM3: OCN EXTRACER  3: CON NUTRIENT";
      case 106: return "CM3: OCN EXTRACER  4: CON PHYTOPLNKT";
      case 107: return "CM3: OCN EXTRACER  5: CON ZOOPLNKTN";
      case 108: return "CM3: OCN EXTRACER  6: CON DETRITUS";
      case 109: return "CM3: OCN EXTRACER  7: CON TRITIUM";
      case 110: return "CM3: OCN EXTRACER  8: CON 3H+3HE";
      case 111: return "CM3: OCN EXTRACER  9: CON CFC11";
      case 112: return "CM3: OCN EXTRACER 10: CON CFC12";
      case 113: return "CM3: OCN EXTRACER 11: CON CFC13";
      case 114: return "CM3: OCN EXTRACER 12: CON CARBON14";
      case 115: return "CM3: OCN EXTRACER 13: CON -------";
      case 116: return "CM3: OCN EXTRACER 14: CON -------";
      case 117: return "CM3: OCN EXTRACER 15: CON -------";
      case 118: return "CM3: OCN EXTRACER 16: CON -------";
      case 119: return "CM3: OCN EXTRACER 17: CON -------";
      case 120: return "CM3: OCN EXTRACER 18: CON -------";
      case 121: return "CM3: BAROCLINIC U_VEL (OCEAN) CM/S";
      case 122: return "CM3: BAROCLINIC V_VEL (OCEAN) CM/S";
      case 130: return "CM3: STREAMFUNCTION (OCEAN)  CM3/S";
      case 131: return "CM3: STREAMFTN (T-1) (OCEAN)  CM3/S";
      case 132: return "CM3: STREAMFTN TENDENCY(OC) CM3/S/TS";
      case 133: return "CM3: STREAMFTN TENDENCY(T-1)(OCEAN)";
      case 134: return "CM3: SURFACE ELEVATION (OCEAN)  CM";
      case 135: return "CM3: BAROTROPIC U_VEL (OCEAN) CM/S";
      case 136: return "CM3: BAROTROPIC V_VEL (OCEAN) CM/S";
      case 137: return "CM3: MIXED LAYER DEPTH (OCEAN)  M";
      case 139: return "CM3: ISX OCEAN/ICE STRESS(ICE)  N/M2";
      case 140: return "CM3: ISY OCEAN/ICE STRESS(ICE)  N/M2";
      case 141: return "CM3: SNOW DEPTH (OCEAN)  M";
      case 142: return "CM3: GBM CARYHEAT MISC HEAT FLX(ICE)";
      case 143: return "CM3: GBM HEAT FLUX:OCEAN TO ICE(OCN)";
      case 144: return "CM3: RATE OF SAL CHANGE (ICE)  PSU/S";
      case 145: return "CM3: ICY : TRUE IF SOME ICE";
      case 146: return "CM3: AICE : ICE CONCENTRATION";
      case 147: return "CM3: HICE : MEAN ICE DEPTH  M";
      case 148: return "CM3: ICE U_VELOCITY  M/S";
      case 149: return "CM3: ICE V_VELOCITY  M/S";
      case 150: return "CM3: TAUX: X_WINDSTRESS  N/M2  A";
      case 151: return "CM3: TAUY: Y_WINDSTRESS  N/M2  A";
      case 152: return "CM3: WME: WIND MIXING EN FLUX W/M2 A";
      case 161: return "CM3: SOL:PEN.SOLAR*LF INTO OC W/M2 A";
      case 162: return "CM3: HTN:NONPEN.HT.FLX*LF INTO OCN A";
      case 165: return "CM3: PLE:PRECIP-EVAP INTO OCEAN  A";
      case 166: return "CM3: RIVER OUTFLOW INTO OCEAN  A";
      case 167: return "CM3: WATER TYPE  A";
      case 170: return "CM3: SOLAR RADIATION OVER ICE W/M2 A";
      case 171: return "CM3: SNOWFALL INTO OCEAN/ONTO ICE  A";
      case 172: return "CM3: SUBLIMATION FROM SEAICE  A";
      case 180: return "CM3: REF. SEA SURFACE TEMP. DEG.C  A";
      case 181: return "CM3: REF. SEA SURFACE SALINITY  A";
      case 182: return "CM3: CLIM. AIR TEMPERATURE DEG.C  A";
      case 183: return "CM3: CLIMATOLOGICAL ICE DEPTH M  A";
      case 185: return "CM3: OCEAN HEAT FLUX CORRN. W/M2  A";
      case 186: return "CM3: P-E FLUX CORRECTION KG/M2/S  A";
      case 187: return "CM3: S-ICE HT FLUX CORRN. DISABLED";
      case 190: return "CM3: GBM SEAICE TOPMELT HEAT FLUX  A";
      case 191: return "CM3: GBM SEAICE BOTMELT HEAT FLUX  A";
      }
      break;
    case 44:
      switch (item) {
      case 101: return "CM4: POTENTIAL TEMP. (OCEAN)  DEG.C";
      case 102: return "CM4: SALINITY (OCEAN) (PSU-35)/1000";
      case 103: return "CM4: OCN EXTRACER  1: CON TCO2";
      case 104: return "CM4: OCN EXTRACER  2: CON ALKALINITY";
      case 105: return "CM4: OCN EXTRACER  3: CON NUTRIENT";
      case 106: return "CM4: OCN EXTRACER  4: CON PHYTOPLNKT";
      case 107: return "CM4: OCN EXTRACER  5: CON ZOOPLNKTN";
      case 108: return "CM4: OCN EXTRACER  6: CON DETRITUS";
      case 109: return "CM4: OCN EXTRACER  7: CON TRITIUM";
      case 110: return "CM4: OCN EXTRACER  8: CON CFC11";
      case 111: return "CM4: OCN EXTRACER  9: CON CFC12";
      case 112: return "CM4: OCN EXTRACER 10: CON CFC13";
      case 113: return "CM4: OCN EXTRACER 11: CON CARBON14";
      case 114: return "CM4: OCN EXTRACER 12: CON -------";
      case 115: return "CM4: OCN EXTRACER 13: CON -------";
      case 116: return "CM4: OCN EXTRACER 14: CON -------";
      case 117: return "CM4: OCN EXTRACER 15: CON -------";
      case 118: return "CM4: OCN EXTRACER 16: CON -------";
      case 119: return "CM4: OCN EXTRACER 17: CON -------";
      case 120: return "CM4: OCN EXTRACER 18: CON -------";
      case 121: return "CM4: BAROCLINIC U_VEL (OCEAN) CM/S";
      case 122: return "CM4: BAROCLINIC V_VEL (OCEAN) CM/S";
      case 130: return "CM4: STREAMFUNCTION (OCEAN)  CM3/S";
      case 131: return "CM4: STREAMFTN (T-1) (OCEAN)  CM3/S";
      case 132: return "CM4: STREAMFTN TENDENCY(OC) CM3/S/TS";
      case 133: return "CM4: STREAMFTN TENDENCY(T-1)(OCEAN)";
      case 134: return "CM4: SURFACE ELEVATION (OCEAN)  CM";
      case 135: return "CM4: BAROTROPIC U_VEL (OCEAN) CM/S";
      case 136: return "CM4: BAROTROPIC V_VEL (OCEAN) CM/S";
      case 137: return "CM4: MIXED LAYER DEPTH (OCEAN)  M";
      case 139: return "CM4: ISX OCEAN/ICE STRESS(ICE)  N/M2";
      case 140: return "CM4: ISY OCEAN/ICE STRESS(ICE)  N/M2";
      case 141: return "CM4: SNOW DEPTH (OCEAN)  M";
      case 142: return "CM4: GBM CARYHEAT MISC HEAT FLX(ICE)";
      case 143: return "CM4: GBM HEAT FLUX:OCEAN TO ICE(OCN)";
      case 144: return "CM4: RATE OF SAL CHANGE (ICE)  PSU/S";
      case 145: return "CM4: ICY : TRUE IF SOME ICE";
      case 146: return "CM4: AICE : ICE CONCENTRATION";
      case 147: return "CM4: HICE : MEAN ICE DEPTH  M";
      case 148: return "CM4: ICE U_VELOCITY  M/S";
      case 149: return "CM4: ICE V_VELOCITY  M/S";
      case 150: return "CM4: TAUX: X_WINDSTRESS  N/M2  A";
      case 151: return "CM4: TAUY: Y_WINDSTRESS  N/M2  A";
      case 152: return "CM4: WME: WIND MIXING EN FLUX W/M2 A";
      case 161: return "CM4: SOL:PEN.SOLAR*LF INTO OC W/M2 A";
      case 162: return "CM4: HTN:NONPEN.HT.FLX*LF INTO OCN A";
      case 165: return "CM4: PLE:PRECIP-EVAP INTO OCEAN  A";
      case 166: return "CM4: RIVER OUTFLOW INTO OCEAN  A";
      case 167: return "CM4: WATER TYPE  A";
      case 170: return "CM4: SOLAR RADIATION OVER ICE W/M2 A";
      case 171: return "CM4: SNOWFALL INTO OCEAN/ONTO ICE  A";
      case 172: return "CM4: SUBLIMATION FROM SEAICE  A";
      case 180: return "CM4: REF. SEA SURFACE TEMP. DEG.C  A";
      case 181: return "CM4: REF. SEA SURFACE SALINITY  A";
      case 182: return "CM4: CLIM. AIR TEMPERATURE DEG.C  A";
      case 183: return "CM4: CLIMATOLOGICAL ICE DEPTH M  A";
      case 185: return "CM4: OCEAN HEAT FLUX CORRN. W/M2  A";
      case 186: return "CM4: P-E FLUX CORRECTION KG/M2/S  A";
      case 187: return "CM4: S-ICE HT FLUX CORRN. DISABLED";
      case 190: return "CM4: GBM SEAICE TOPMELT HEAT FLUX  A";
      case 191: return "CM4: GBM SEAICE BOTMELT HEAT FLUX  A";
      }
      break;
    }
    break;
  case 3:
    switch (section) {
    case 0:
      switch (item) {
      case 177: return "HEAT CONVERGENCE (SLAB MODEL) W/M2 A";
      case 178: return "REF. SEA SURF TEMP (SLAB MODEL) K  A";
      case 179: return "CLIM. SEAICE DEPTH (SLAB MODEL) M  A";
      case 210: return "SLAB TEMPERATURE AFTER TSTEP  SLAB C";
      case 211: return "UICE ICE VELOCITY  SLAB M/S";
      case 212: return "VICE ICE VELOCITY  SLAB M/S";
      }
      break;
    case 21:
      switch (item) {
      case 177: return "CM1: HEAT CONVERGENCE ( SLAB) W/M2 A";
      case 178: return "CM1: REF. SEA SURF TEMP ( SLAB) K  A";
      case 179: return "CM1: CLIM. SEAICE DEPTH ( SLAB) M  A";
      case 225: return "CM1:  SLAB TEMP AFTER TSTEP  SLAB C";
      case 226: return "CM1: UICE ICE VELOCITY  SLAB M/S";
      case 227: return "CM1: VICE ICE VELOCITY  SLAB M/S";
      }
      break;
    case 22:
      switch (item) {
      case 177: return "CM2: HEAT CONVERGENCE (SLAB ) W/M2 A";
      case 178: return "CM2: REF. SEA SURF TEMP (SLAB ) K  A";
      case 179: return "CM2: CLIM. SEAICE DEPTH (SLAB ) M  A";
      case 225: return "CM2:  SLAB TEMP AFTER TSTEP  SLAB C";
      case 226: return "CM2: UICE ICE VELOCITY  SLAB M/S";
      case 227: return "CM2: VICE ICE VELOCITY  SLAB M/S";
      }
      break;
    case 23:
      switch (item) {
      case 177: return "CM3: HEAT CONVERGENCE (SLAB ) W/M2 A";
      case 178: return "CM3: REF. SEA SURF TEMP (SLAB ) K  A";
      case 179: return "CM3: CLIM. SEAICE DEPTH (SLAB ) M  A";
      case 225: return "CM3:  SLAB TEMP AFTER TSTEP  SLAB C";
      case 226: return "CM3: UICE ICE VELOCITY  SLAB M/S";
      case 227: return "CM3: VICE ICE VELOCITY  SLAB M/S";
      }
      break;
    case 24:
      switch (item) {
      case 177: return "CM4: HEAT CONVERGENCE (SLAB ) W/M2 A";
      case 178: return "CM4: REF. SEA SURF TEMP (SLAB ) K  A";
      case 179: return "CM4: CLIM. SEAICE DEPTH (SLAB ) M  A";
      case 225: return "CM4:  SLAB TEMPERATURE  SLAB C";
      case 226: return "CM4: UICE ICE VELOCITY  SLAB M/S";
      case 227: return "CM4: VICE ICE VELOCITY  SLAB M/S";
      }
      break;
    case 40:
      switch (item) {
      case 23: return "SNOW AMOUNT AFTER SLAB  KG/M2";
      case 24: return "SURFACE TEMPERATURE AFTER SLAB  K";
      case 31: return "SEA ICE FRACTION AFTER SLAB";
      case 32: return "SEAICE DEPTH(MN OVR ICE)AFTER SLAB M";
      case 177: return "HEAT CONVERGENCE AFTER SLAB  W/M2 A";
      case 178: return "REF. SEA SURF TEMP AFTER SLAB  K  A";
      case 179: return "CLIM. SEAICE DEPTH AFTER SLAB  M  A";
      case 201: return "ANOMALOUS HEAT CONVRGENCE(SLAB) W/M2";
      case 202: return "REDISTRIBUTED HEAT CONVERGENCE SLAB";
      case 203: return "SEAICE DEPTH(GRD BOX MN)AFTER SLAB M";
      case 204: return "ICE FRACTION INC.  (DYNAMICS) SLAB";
      case 205: return "ICE DEPTH INC.  (DYNAMICS) SLAB";
      case 206: return "ICE DEPTH INC.  (DIFFUSION)  SLAB";
      case 207: return "SNOW DEPTH INC.*AICE (DYNAMICS) SLAB";
      case 208: return "ICE FRACTION INC.  (THERMO) SLAB";
      case 209: return "ICE DEPTH INC.  (THERMO) SLAB";
      case 210: return "SNOW DEPTH INC.*AICE  (THERMO) SLAB";
      case 211: return "OCEAN TO ICE HEAT FLUX (W.M-2) SLAB";
      case 212: return "INTERNAL ICE PRESSURE  (N.M-2) SLAB";
      case 213: return "ICE STRENGTH  (N.M-2) SLAB";
      case 214: return "NET SLAB FLUX THRO LDS (W.M-2) SLAB";
      case 215: return "NET ICE  FLUX THRO LDS (W.M-2) SLAB";
      case 216: return "W X SLABTEMP AT SURFACE  AFTER SLAB";
      case 217: return "W X SLABTEMP AT BASE  AFTER SLAB";
      case 218: return "SLAB HEATING RATE (ADV) K/S  SLAB";
      case 219: return "SLAB HEATING RATE (DIFF) K/S  SLAB";
      case 220: return "SLAB CARYHEAT  (W M-2)  SLAB";
      case 221: return "SLAB HEATING RATE (ICE)  K/S  SLAB";
      case 222: return "SNOWFALL INTO SLAB (KG/M2/S)  SLAB";
      case 223: return "SNOWFALL INTO LEADS (KG/M2/S)  SLAB";
      case 224: return "ICE DEPTH INC (ADV) (M/TS)  SLAB";
      case 225: return "SNOW DEPTH INC*AICE (ADV M/TS) SLAB";
      case 226: return "GRID BOX AREAS (M2)  SLAB";
      }
      break;
    }
    break;
  case 4:
    switch (section) {
    case 0:
      switch (item) {
      case 1: return "WAVE ENERGY SPECTRUM (FIELDS)";
      case 2: return "LAND/SEA MASK (LOGICAL: LAND=TRUE)";
      case 3: return "WATER DEPTH";
      case 4: return "10 METRE WIND U COMPONENT";
      case 5: return "10 METRE WIND V COMPONENT";
      case 6: return "WAVE DEPENDENT USTAR U COMPONENT";
      case 7: return "WAVE DEPENDENT USTAR V COMPONENT";
      case 8: return "SEA ICE FRACTION AFTER TIMESTEP";
      case 9: return "WAVE INDUCED SURFACE STRESS X COMPT";
      case 10: return "WAVE INDUCED SURFACE STRESS Y COMPT";
      }
      break;
    case 1:
      switch (item) {
      case 1: return "ENERGY INCREMENTS AFTER PROPAGATION";
      }
      break;
    case 2:
      switch (item) {
      case 1: return "WAVE DEPENDENT ROUGHNESS LENGTH";
      case 2: return "WAVE DEPENDENT USTAR X COMPONENT";
      case 3: return "WAVE DEPENDENT USTAR Y COMPONENT";
      case 4: return "WAVE INDUCED SURFACE STRESS X COMPT";
      case 5: return "WAVE INDUCED SURFACE STRESS Y COMPT";
      case 6: return "WAVE INDUCED SURF STRESS HF CONTRIBN";
      case 7: return "ENERGY INCRTS AFTER INPUT SRCE TERM";
      }
      break;
    case 3:
      switch (item) {
      case 1: return "ENERGY INCRTS AFTER NONLIN TRANSFER";
      }
      break;
    case 4:
      switch (item) {
      case 1: return "ENERGY INCRTS AFTER DISSIPATION";
      }
      break;
    case 5:
      switch (item) {
      case 1: return "ENERGY INCRTS AFTER BOTTOM FRICTION";
      }
      break;
    case 6:
      switch (item) {
      case 1: return "TOTAL WAVE HEIGHT";
      case 2: return "TOTAL WAVE UPCROSSING PERIOD";
      case 3: return "TOTAL WAVE PRINCIPAL DIRECTION";
      case 4: return "TOTAL WAVE PEAK PERIOD";
      case 5: return "WINDSEA HEIGHT";
      case 6: return "WINDSEA UPCROSSING PERIOD";
      case 7: return "WINDSEA PRINCIPAL DIRECTION";
      case 8: return "SWELL HEIGHT";
      case 9: return "SWELL UPCROSSING PERIOD";
      case 10: return "SWELL PRINCIPAL DIRECTION";
      case 11: return "SWELL PEAK PERIOD";
      case 12: return "WAVETRAIN WAVE-HEIGHT";
      case 13: return "WAVETRAIN CROSSING PERIOD";
      case 14: return "WAVETRAIN MEAN DIRECTION";
      case 15: return "MEAN ENERGY AT A GIVEN FREQUENCY";
      case 16: return "MEAN DIRECTION AT A GIVEN FREQUENCY";
      case 17: return "10 METRE WIND SPEED";
      case 18: return "10 METRE WIND DIRECTION";
      }
      break;
    case 7:
      switch (item) {
      case 1: return "ENERGY INCRTS AFTER ASSIMILATION";
      case 2: return "ASSIMILATION INCRTS FOR WAVE HEIGHT";
      case 3: return "ASSIMILATION INCRTS FOR WINDSPEED";
      }
    }
  }

  return NULL;
}


/*-------------------------------------------------------------------------------
 * PERL CODE USED TO GENERATE THE ABOVE SWITCH STATEMENT FROM STASHMASTER FILE(S)
 * ==============================================================================
 *
 *  #!/usr/bin/perl
 *  
 *  $Sm="  switch (model) {\n";
 *  $Em="  }\n";
 *  $sm="  case %d:\n";
 *  $em="    break;\n";
 *  
 *  $Ss="    switch (section) {\n";
 *  $Es="    }\n";
 *  $ss="    case %d:\n";
 *  $es="      break;\n";
 *  
 *  $Si="      switch (item) {\n";
 *  $Ei="      }\n";
 *  $si="      case %d: ";
 *  #$ei="      break;\n";
 *  $ei="\n";
 *  
 *  
 *  $first=1;
 *  $lm=$ls=-1;
 *  while (<>){
 *      if (/^1\|\s*([0-9]+)\s*\|\s*([0-9]+)\s*\|\s*([0-9]+)\s*\|(.*)\|\s*$/) {
 *          ($m,$s,$i,$name)=($1,$2,$3,$4);
 *          
 *          if ($first) {
 *              print $Sm;
 *              printf $sm,$m;
 *              print $Ss;
 *              printf $ss,$s;
 *              print $Si;
 *          } else {
 *  
 *              if ($s != $ls) {
 *                  print $Ei;
 *                  print $es;
 *              }
 *              if ($m != $lm) {
 *                  print $Es;
 *                  print $em;
 *                  printf $sm,$m;
 *                  print $Ss;              
 *              }
 *              if ($s != $ls) {
 *                  printf $ss,$s;
 *                  print $Si;
 *              }
 *          }
 *  
 *          $name =~ s/  +/  /g;
 *          $name =~ s/\s+$//;
 *          $name =~ s/\"/\\\"/g;
 *  
 *          printf $si,$i;
 *          print "return \"$name\";";
 *          print $ei;
 *  
 *          $first=0;
 *          ($lm,$ls)=($m,$s);
 *      }
 *  }
 *  
 *  print $Ei;
 *  print $Es;
 *  print $Em;
 *  
 */

#endif
