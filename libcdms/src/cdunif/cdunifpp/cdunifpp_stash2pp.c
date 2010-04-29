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
 *  NOTE: It turns out that as currently implemented, pp_stashpp() is NOT USED,
 *  ====  because PP codes are taken directly from the header rather than
 *        being lookup up from the stash code.  But this routine is potentially
 *        useful, so it is being left in the source tree for possible later 
 *        use.
 */


/* returns PP code corresponding to stash codes, or -1 if not found. */

/* NOTE: the code of this function was largely auto-generated from the stashmaster files
 * from UM version 4.5.  See below for perl script.
 */

int pp_stashpp(int model, int section, int item)
{
  switch (model) {
  case 1:
    switch (section) {
    case 0:
      switch (item) {
      case 1: return 8;
      case 2: return 56;
      case 3: return 57;
      case 4: return 19;
      case 5: return 19;
      case 9: return 122;
      case 10: return 95;
      case 11: return 95;
      case 12: return 78;
      case 13: return 34;
      case 14: return 222;
      case 15: return 223;
      case 16: return 219;
      case 17: return 174;
      case 18: return 175;
      case 20: return 23;
      case 21: return 106;
      case 22: return 271;
      case 23: return 93;
      case 24: return 16;
      case 25: return 5;
      case 26: return 324;
      case 27: return 0;
      case 28: return 701;
      case 29: return 702;
      case 30: return 395;
      case 31: return 37;
      case 32: return 687;
      case 33: return 1;
      case 34: return 150;
      case 35: return 152;
      case 36: return 153;
      case 37: return 154;
      case 38: return 0;
      case 39: return 0;
      case 40: return 329;
      case 41: return 330;
      case 42: return 331;
      case 43: return 332;
      case 44: return 333;
      case 45: return 334;
      case 46: return 335;
      case 47: return 336;
      case 48: return 342;
      case 49: return 209;
      case 50: return 326;
      case 51: return 321;
      case 52: return 322;
      case 53: return 328;
      case 54: return 323;
      case 55: return 325;
      case 56: return 327;
      case 57: return 287;
      case 58: return 569;
      case 59: return 570;
      case 60: return 453;
      case 61: return 501;
      case 62: return 502;
      case 63: return 503;
      case 64: return 504;
      case 65: return 505;
      case 66: return 506;
      case 67: return 507;
      case 68: return 508;
      case 69: return 509;
      case 70: return 510;
      case 71: return 511;
      case 72: return 512;
      case 73: return 513;
      case 74: return 514;
      case 75: return 515;
      case 76: return 516;
      case 77: return 517;
      case 78: return 518;
      case 79: return 519;
      case 80: return 520;
      case 81: return 521;
      case 82: return 522;
      case 83: return 523;
      case 84: return 524;
      case 85: return 525;
      case 86: return 526;
      case 87: return 527;
      case 88: return 528;
      case 89: return 529;
      case 90: return 286;
      case 93: return 0;
      case 96: return 0;
      case 97: return 0;
      case 98: return 0;
      case 101: return 1374;
      case 102: return 1373;
      case 103: return 1370;
      case 104: return 1371;
      case 105: return 1372;
      case 106: return 581;
      case 107: return 1379;
      case 108: return 1491;
      case 109: return 1492;
      case 110: return 1493;
      case 121: return 569;
      case 122: return 580;
      case 123: return 600;
      case 124: return 581;
      case 125: return 60;
      case 126: return 569;
      case 127: return 572;
      case 128: return 573;
      case 129: return 573;
      case 150: return 42;
      case 151: return 8;
      case 152: return 999;
      case 153: return 56;
      case 154: return 57;
      case 160: return 1376;
      case 201: return 8;
      case 202: return 56;
      case 203: return 57;
      case 204: return 19;
      case 205: return 36;
      case 207: return 1381;
      case 208: return 1382;
      case 209: return 1383;
      case 211: return 218;
      case 213: return 1384;
      case 214: return 1385;
      case 215: return 1386;
      case 216: return 1391;
      case 217: return 1392;
      case 218: return 1393;
      case 219: return 1394;
      case 220: return 1395;
      case 221: return 1396;
      case 222: return 259;
      case 223: return 1397;
      case 224: return 1398;
      case 225: return 1500;
      case 226: return 1501;
      case 227: return 1502;
      case 228: return 1503;
      case 229: return 1504;
      case 230: return 1505;
      case 231: return 1507;
      case 232: return 1508;
      case 233: return 1510;
      case 234: return 1511;
      case 250: return 1560;
      case 251: return 1561;
      case 252: return 1564;
      case 401: return 7;
      case 402: return 79;
      case 403: return 78;
      case 404: return 1731;
      case 405: return 106;
      }
      break;
    case 1:
      switch (item) {
      case 4: return 16;
      case 201: return 186;
      case 202: return 186;
      case 203: return 186;
      case 204: return 186;
      case 205: return 322;
      case 206: return 328;
      case 207: return 200;
      case 208: return 201;
      case 209: return 207;
      case 210: return 208;
      case 211: return 207;
      case 212: return 220;
      case 213: return 34;
      case 214: return 169;
      case 215: return 169;
      case 216: return 169;
      case 217: return 169;
      case 218: return 220;
      case 219: return 30;
      case 220: return 163;
      case 221: return 164;
      case 222: return 165;
      case 223: return 166;
      case 224: return 167;
      case 225: return 163;
      case 226: return 165;
      case 232: return 251;
      case 233: return 252;
      case 234: return 1376;
      case 235: return 203;
      case 236: return 1377;
      case 237: return 186;
      case 238: return 201;
      case 241: return 1464;
      case 242: return 1465;
      case 243: return 1466;
      case 244: return 1467;
      case 245: return 1468;
      case 246: return 1469;
      }
      break;
    case 2:
      switch (item) {
      case 4: return 16;
      case 201: return 187;
      case 202: return 187;
      case 203: return 187;
      case 204: return 30;
      case 205: return 206;
      case 206: return 210;
      case 207: return 205;
      case 208: return 211;
      case 232: return 253;
      case 233: return 254;
      case 237: return 187;
      case 238: return 205;
      }
      break;
    case 3:
      switch (item) {
      case 2: return 56;
      case 3: return 57;
      case 4: return 16;
      case 10: return 95;
      case 24: return 16;
      case 25: return 5;
      case 26: return 324;
      case 49: return 209;
      case 100: return 1301;
      case 101: return 1302;
      case 102: return 1303;
      case 103: return 1304;
      case 104: return 1305;
      case 105: return 1306;
      case 106: return 1307;
      case 107: return 1308;
      case 108: return 1309;
      case 109: return 1310;
      case 110: return 1311;
      case 111: return 1312;
      case 112: return 1313;
      case 113: return 1314;
      case 114: return 1315;
      case 115: return 1316;
      case 116: return 1317;
      case 117: return 1318;
      case 118: return 1319;
      case 119: return 1320;
      case 120: return 1321;
      case 121: return 1322;
      case 122: return 1323;
      case 123: return 1324;
      case 124: return 1325;
      case 125: return 1326;
      case 126: return 1327;
      case 127: return 1328;
      case 128: return 1329;
      case 129: return 1330;
      case 201: return 261;
      case 202: return 179;
      case 203: return 160;
      case 204: return 183;
      case 205: return 52;
      case 206: return 172;
      case 207: return 171;
      case 208: return 181;
      case 217: return 178;
      case 219: return 61;
      case 220: return 62;
      case 223: return 184;
      case 224: return 182;
      case 225: return 56;
      case 226: return 57;
      case 228: return 178;
      case 229: return 115;
      case 230: return 273;
      case 231: return 107;
      case 232: return 285;
      case 234: return 180;
      case 235: return 260;
      case 236: return 16;
      case 237: return 95;
      case 238: return 23;
      case 239: return 79;
      case 240: return 78;
      case 241: return 184;
      case 242: return 220;
      case 243: return 79;
      case 244: return 78;
      case 245: return 88;
      case 247: return 25;
      case 248: return 220;
      case 249: return 50;
      case 250: return 17;
      case 251: return 174;
      case 252: return 175;
      case 253: return 173;
      case 254: return 19;
      case 255: return 95;
      case 256: return 257;
      case 257: return 258;
      case 258: return 141;
      case 259: return 1384;
      case 260: return 1387;
      case 261: return 1388;
      case 262: return 1389;
      case 263: return 1390;
      case 264: return 1382;
      case 265: return 1383;
      case 270: return 1400;
      case 271: return 1401;
      case 272: return 1402;
      case 273: return 1403;
      case 274: return 1404;
      case 275: return 1405;
      case 276: return 1406;
      case 277: return 1407;
      case 278: return 1408;
      case 279: return 1409;
      case 280: return 1410;
      case 281: return 1411;
      case 282: return 1412;
      case 283: return 1413;
      case 284: return 1414;
      case 285: return 1415;
      case 286: return 1416;
      case 287: return 1517;
      case 288: return 1518;
      case 289: return 1519;
      case 290: return 1520;
      case 291: return 1521;
      case 292: return 1522;
      case 293: return 1523;
      case 294: return 1524;
      case 295: return 1525;
      case 296: return 1526;
      case 297: return 1527;
      case 298: return 1528;
      case 299: return 1529;
      case 300: return 1541;
      case 301: return 1542;
      case 302: return 1543;
      case 303: return 1544;
      case 304: return 1534;
      case 305: return 1535;
      case 306: return 1536;
      case 307: return 1537;
      case 308: return 1538;
      case 309: return 1539;
      case 310: return 1540;
      case 311: return 1557;
      case 312: return 1558;
      case 313: return 1559;
      case 314: return 202;
      case 315: return 1391;
      case 316: return 1510;
      case 317: return 1391;
      case 318: return 1392;
      case 319: return 1393;
      case 320: return 1397;
      case 321: return 1504;
      case 322: return 1505;
      case 323: return 1508;
      case 324: return 1511;
      case 325: return 1499;
      case 326: return 1562;
      case 327: return 1563;
      }
      break;
    case 4:
      switch (item) {
      case 4: return 16;
      case 10: return 95;
      case 201: return 102;
      case 202: return 116;
      case 203: return 99;
      case 204: return 118;
      case 205: return 79;
      case 206: return 78;
      case 207: return 88;
      case 208: return 25;
      case 211: return 1421;
      case 212: return 1422;
      case 213: return 1423;
      case 214: return 1424;
      case 215: return 1545;
      case 216: return 1547;
      case 217: return 1548;
      case 218: return 1549;
      case 219: return 1550;
      case 220: return 1546;
      case 221: return 1546;
      case 222: return 78;
      case 223: return 78;
      case 224: return 78;
      case 225: return 78;
      }
      break;
    case 5:
      switch (item) {
      case 4: return 19;
      case 10: return 95;
      case 13: return 34;
      case 201: return 94;
      case 202: return 117;
      case 203: return 24;
      case 204: return 95;
      case 205: return 98;
      case 206: return 119;
      case 207: return 34;
      case 208: return 34;
      case 209: return 16;
      case 210: return 34;
      case 211: return 34;
      case 212: return 34;
      case 213: return 218;
      case 214: return 97;
      case 215: return 108;
      case 216: return 90;
      case 217: return 213;
      case 218: return 222;
      case 219: return 223;
      case 220: return 343;
      case 221: return 219;
      case 222: return 344;
      case 223: return 345;
      case 224: return 346;
      case 225: return 347;
      case 226: return 77;
      case 227: return 1417;
      case 228: return 1418;
      case 229: return 1419;
      case 230: return 1420;
      case 231: return 344;
      case 232: return 345;
      case 233: return 218;
      case 234: return 219;
      case 235: return 56;
      case 236: return 57;
      case 237: return 1551;
      case 238: return 1553;
      case 239: return 1554;
      case 240: return 1555;
      case 241: return 1556;
      case 242: return 1552;
      case 250: return 40;
      case 251: return 40;
      case 252: return 74;
      case 253: return 74;
      case 254: return 74;
      case 255: return 74;
      case 256: return 318;
      case 257: return 319;
      }
      break;
    case 6:
      switch (item) {
      case 2: return 56;
      case 3: return 57;
      case 201: return 61;
      case 202: return 62;
      case 203: return 150;
      case 204: return 152;
      case 205: return 153;
      case 206: return 154;
      case 207: return 68;
      case 208: return 69;
      case 209: return 70;
      case 210: return 71;
      case 211: return 124;
      case 212: return 124;
      case 213: return 125;
      }
      break;
    case 7:
      switch (item) {
      case 2: return 56;
      case 3: return 57;
      case 201: return 195;
      case 202: return 196;
      }
      break;
    case 8:
      switch (item) {
      case 23: return 93;
      case 24: return 16;
      case 201: return 110;
      case 202: return 197;
      case 203: return 274;
      case 204: return 111;
      case 205: return 112;
      case 206: return 337;
      case 207: return 338;
      case 208: return 106;
      case 209: return 271;
      case 210: return 329;
      case 211: return 330;
      case 212: return 331;
      case 213: return 332;
      case 214: return 333;
      case 215: return 334;
      case 216: return 335;
      case 217: return 336;
      case 218: return 326;
      case 219: return 321;
      case 220: return 323;
      case 221: return 325;
      case 222: return 327;
      case 223: return 122;
      case 224: return 342;
      case 225: return 23;
      case 226: return 141;
      case 228: return 1381;
      case 229: return 1385;
      case 230: return 1386;
      case 231: return 1530;
      case 233: return 1531;
      case 234: return 1532;
      case 235: return 1533;
      }
      break;
    case 9:
      switch (item) {
      case 4: return 16;
      case 10: return 95;
      case 201: return 220;
      case 202: return 33;
      case 203: return 33;
      case 204: return 32;
      case 205: return 31;
      case 206: return 79;
      case 207: return 78;
      case 208: return 136;
      case 209: return 136;
      case 210: return 136;
      case 211: return 136;
      case 212: return 136;
      case 213: return 136;
      case 214: return 136;
      case 215: return 136;
      case 216: return 30;
      case 217: return 30;
      case 218: return 29;
      case 219: return 75;
      case 220: return 76;
      case 221: return 139;
      case 222: return 41;
      case 223: return 216;
      case 224: return 1425;
      case 225: return 1426;
      case 226: return 1427;
      case 227: return 1729;
      case 228: return 1730;
      }
      break;
    case 10:
      switch (item) {
      case 1: return 8;
      case 2: return 56;
      case 3: return 57;
      case 4: return 19;
      case 10: return 95;
      case 201: return 224;
      case 202: return 225;
      case 203: return 151;
      case 204: return 43;
      case 205: return 28;
      case 206: return 3;
      case 207: return 229;
      case 208: return 230;
      case 209: return 231;
      case 210: return 232;
      case 211: return 233;
      case 212: return 234;
      case 213: return 235;
      case 214: return 236;
      case 215: return 263;
      case 216: return 264;
      case 217: return 265;
      case 218: return 266;
      case 219: return 267;
      case 220: return 268;
      case 221: return 269;
      case 222: return 270;
      case 223: return 56;
      case 224: return 57;
      case 225: return 88;
      case 226: return 56;
      case 227: return 57;
      case 228: return 88;
      case 229: return 79;
      case 230: return 78;
      }
      break;
    case 12:
      switch (item) {
      case 2: return 56;
      case 3: return 57;
      case 4: return 19;
      case 10: return 95;
      case 201: return 40;
      case 202: return 40;
      }
      break;
    case 13:
      switch (item) {
      case 2: return 56;
      case 3: return 57;
      case 4: return 19;
      case 10: return 95;
      case 201: return 184;
      }
      break;
    case 14:
      switch (item) {
      case 201: return 259;
      }
      break;
    case 15:
      switch (item) {
      case 201: return 56;
      case 202: return 57;
      case 203: return 190;
      case 204: return 8;
      case 205: return 190;
      case 206: return 1;
      case 207: return 4;
      case 208: return 8;
      case 209: return 56;
      case 210: return 57;
      case 211: return 190;
      case 212: return 56;
      case 213: return 57;
      case 214: return 82;
      case 215: return 60;
      case 216: return 16;
      case 217: return 11;
      case 218: return 12;
      case 219: return 13;
      case 220: return 58;
      case 221: return 59;
      case 222: return 40;
      case 223: return 14;
      case 224: return 53;
      case 225: return 54;
      case 226: return 95;
      case 227: return 46;
      case 228: return 47;
      case 229: return 82;
      case 230: return 19;
      case 231: return 144;
      case 232: return 145;
      case 233: return 146;
      case 234: return 147;
      case 235: return 1334;
      case 236: return 1335;
      case 237: return 63;
      case 238: return 1;
      case 239: return 1334;
      case 240: return 1334;
      case 241: return 1399;
      }
      break;
    case 16:
      switch (item) {
      case 201: return 1;
      case 202: return 1;
      case 203: return 16;
      case 204: return 88;
      case 205: return 22;
      case 206: return 191;
      case 207: return 4;
      case 208: return 8;
      case 209: return 4;
      case 210: return 1;
      case 211: return 8;
      case 212: return 35;
      case 213: return 35;
      case 214: return 8;
      case 215: return 16;
      case 216: return 1;
      case 217: return 4;
      case 218: return 1;
      case 219: return 189;
      case 220: return 1;
      case 221: return 199;
      case 222: return 8;
      case 223: return 189;
      case 224: return 15;
      case 225: return 1;
      case 226: return 501;
      case 227: return 502;
      case 228: return 503;
      case 229: return 504;
      case 230: return 505;
      case 231: return 506;
      case 232: return 507;
      case 233: return 508;
      case 234: return 509;
      case 235: return 510;
      case 236: return 511;
      case 237: return 512;
      case 238: return 513;
      case 239: return 514;
      case 240: return 515;
      case 241: return 516;
      case 242: return 517;
      case 243: return 518;
      case 244: return 519;
      case 245: return 520;
      case 246: return 521;
      case 247: return 522;
      case 248: return 523;
      case 249: return 524;
      case 250: return 525;
      case 251: return 526;
      case 252: return 527;
      case 253: return 528;
      case 254: return 529;
      }
      break;
    case 17:
      switch (item) {
      case 201: return 1494;
      case 202: return 1495;
      case 203: return 1496;
      case 204: return 1497;
      }
      break;
    case 18:
      switch (item) {
      case 1: return 8;
      case 2: return 56;
      case 3: return 57;
      case 4: return 19;
      case 10: return 95;
      case 201: return 290;
      case 202: return 291;
      case 203: return 292;
      case 204: return 294;
      case 205: return 295;
      case 209: return 288;
      case 211: return 303;
      case 212: return 304;
      case 213: return 305;
      case 214: return 309;
      case 215: return 310;
      case 219: return 289;
      case 223: return 306;
      case 231: return 304;
      case 241: return 305;
      case 242: return 305;
      case 251: return 306;
      case 252: return 306;
      case 261: return 303;
      case 262: return 304;
      case 271: return 24;
      case 272: return 304;
      }
      break;
    case 19:
      switch (item) {
      case 1: return 1512;
      case 2: return 1513;
      case 3: return 1514;
      case 4: return 1515;
      case 5: return 1516;
      case 6: return 1500;
      case 7: return 1392;
      case 8: return 1500;
      case 9: return 1398;
      case 10: return 1502;
      case 11: return 1503;
      case 12: return 1394;
      case 13: return 1391;
      case 14: return 1392;
      case 15: return 1393;
      case 16: return 1397;
      }
      break;
    case 21:
      switch (item) {
      case 1: return 8;
      case 2: return 56;
      case 3: return 57;
      case 4: return 19;
      case 10: return 95;
      case 13: return 34;
      case 14: return 222;
      case 15: return 223;
      case 16: return 219;
      case 23: return 93;
      case 24: return 16;
      case 25: return 5;
      case 26: return 324;
      case 28: return 701;
      case 29: return 702;
      case 31: return 37;
      case 32: return 687;
      case 61: return 501;
      case 62: return 502;
      case 63: return 503;
      case 64: return 504;
      case 65: return 505;
      case 66: return 506;
      case 67: return 507;
      case 68: return 508;
      case 69: return 509;
      case 70: return 510;
      case 71: return 511;
      case 72: return 512;
      case 73: return 513;
      case 74: return 514;
      case 75: return 515;
      case 76: return 516;
      case 77: return 517;
      case 78: return 518;
      case 79: return 519;
      case 80: return 520;
      case 81: return 521;
      case 82: return 522;
      case 83: return 523;
      case 84: return 524;
      case 85: return 525;
      case 86: return 526;
      case 87: return 527;
      case 88: return 528;
      case 89: return 529;
      case 201: return 56;
      case 202: return 57;
      case 203: return 40;
      case 205: return 190;
      case 206: return 1;
      case 207: return 8;
      case 208: return 56;
      case 209: return 57;
      case 210: return 1;
      case 211: return 1;
      case 212: return 16;
      case 213: return 88;
      case 214: return 22;
      case 215: return 191;
      case 216: return 1;
      case 217: return 8;
      case 218: return 8;
      case 219: return 16;
      case 220: return 1;
      case 221: return 31;
      case 222: return 32;
      case 223: return 33;
      case 224: return 8;
      }
      break;
    case 22:
      switch (item) {
      case 1: return 8;
      case 2: return 56;
      case 3: return 57;
      case 4: return 19;
      case 10: return 95;
      case 13: return 34;
      case 14: return 222;
      case 15: return 223;
      case 16: return 219;
      case 23: return 93;
      case 24: return 16;
      case 25: return 5;
      case 26: return 324;
      case 28: return 701;
      case 29: return 702;
      case 31: return 37;
      case 32: return 687;
      case 61: return 501;
      case 62: return 502;
      case 63: return 503;
      case 64: return 504;
      case 65: return 505;
      case 66: return 506;
      case 67: return 507;
      case 68: return 508;
      case 69: return 509;
      case 70: return 510;
      case 71: return 511;
      case 72: return 512;
      case 73: return 513;
      case 74: return 514;
      case 75: return 515;
      case 76: return 516;
      case 77: return 517;
      case 78: return 518;
      case 79: return 519;
      case 80: return 520;
      case 81: return 521;
      case 82: return 522;
      case 83: return 523;
      case 84: return 524;
      case 85: return 525;
      case 86: return 526;
      case 87: return 527;
      case 88: return 528;
      case 89: return 529;
      case 201: return 56;
      case 202: return 57;
      case 203: return 40;
      case 205: return 190;
      case 206: return 1;
      case 207: return 8;
      case 208: return 56;
      case 209: return 57;
      case 210: return 1;
      case 211: return 1;
      case 212: return 16;
      case 213: return 88;
      case 214: return 22;
      case 215: return 191;
      case 216: return 1;
      case 217: return 8;
      case 218: return 8;
      case 219: return 16;
      case 220: return 1;
      case 221: return 31;
      case 222: return 32;
      case 223: return 33;
      case 224: return 8;
      }
      break;
    case 23:
      switch (item) {
      case 1: return 8;
      case 2: return 56;
      case 3: return 57;
      case 4: return 19;
      case 10: return 95;
      case 13: return 34;
      case 14: return 222;
      case 15: return 223;
      case 16: return 219;
      case 23: return 93;
      case 24: return 16;
      case 25: return 5;
      case 26: return 324;
      case 28: return 701;
      case 29: return 702;
      case 31: return 37;
      case 32: return 687;
      case 61: return 501;
      case 62: return 502;
      case 63: return 503;
      case 64: return 504;
      case 65: return 505;
      case 66: return 506;
      case 67: return 507;
      case 68: return 508;
      case 69: return 509;
      case 70: return 510;
      case 71: return 511;
      case 72: return 512;
      case 73: return 513;
      case 74: return 514;
      case 75: return 515;
      case 76: return 516;
      case 77: return 517;
      case 78: return 518;
      case 79: return 519;
      case 80: return 520;
      case 81: return 521;
      case 82: return 522;
      case 83: return 523;
      case 84: return 524;
      case 85: return 525;
      case 86: return 526;
      case 87: return 527;
      case 88: return 528;
      case 89: return 529;
      case 201: return 56;
      case 202: return 57;
      case 203: return 40;
      case 205: return 190;
      case 206: return 1;
      case 207: return 8;
      case 208: return 56;
      case 209: return 57;
      case 210: return 1;
      case 211: return 1;
      case 212: return 16;
      case 213: return 88;
      case 214: return 22;
      case 215: return 191;
      case 216: return 1;
      case 217: return 8;
      case 218: return 8;
      case 219: return 16;
      case 220: return 1;
      case 221: return 31;
      case 222: return 32;
      case 223: return 33;
      case 224: return 8;
      }
      break;
    case 24:
      switch (item) {
      case 1: return 8;
      case 2: return 56;
      case 3: return 57;
      case 4: return 19;
      case 10: return 95;
      case 13: return 34;
      case 14: return 222;
      case 15: return 223;
      case 16: return 219;
      case 23: return 93;
      case 24: return 16;
      case 25: return 5;
      case 26: return 324;
      case 28: return 701;
      case 29: return 702;
      case 31: return 37;
      case 32: return 687;
      case 61: return 501;
      case 62: return 502;
      case 63: return 503;
      case 64: return 504;
      case 65: return 505;
      case 66: return 506;
      case 67: return 507;
      case 68: return 508;
      case 69: return 509;
      case 70: return 510;
      case 71: return 511;
      case 72: return 512;
      case 73: return 513;
      case 74: return 514;
      case 75: return 515;
      case 76: return 516;
      case 77: return 517;
      case 78: return 518;
      case 79: return 519;
      case 80: return 520;
      case 81: return 521;
      case 82: return 522;
      case 83: return 523;
      case 84: return 524;
      case 85: return 525;
      case 86: return 526;
      case 87: return 527;
      case 88: return 528;
      case 89: return 529;
      case 201: return 56;
      case 202: return 57;
      case 203: return 40;
      case 205: return 190;
      case 206: return 1;
      case 207: return 8;
      case 208: return 56;
      case 209: return 57;
      case 210: return 1;
      case 211: return 1;
      case 212: return 16;
      case 213: return 88;
      case 214: return 22;
      case 215: return 191;
      case 216: return 1;
      case 217: return 8;
      case 218: return 8;
      case 219: return 16;
      case 220: return 1;
      case 221: return 31;
      case 222: return 32;
      case 223: return 33;
      case 224: return 8;
      }
      break;
    }
    break;
  case 2:
    switch (section) {
    case 0:
      switch (item) {
      case 101: return 601;
      case 102: return 602;
      case 103: return 801;
      case 104: return 802;
      case 105: return 803;
      case 106: return 804;
      case 107: return 805;
      case 108: return 806;
      case 109: return 807;
      case 110: return 808;
      case 111: return 809;
      case 112: return 810;
      case 113: return 811;
      case 114: return 812;
      case 115: return 813;
      case 116: return 814;
      case 117: return 815;
      case 118: return 816;
      case 119: return 817;
      case 120: return 818;
      case 121: return 701;
      case 122: return 702;
      case 130: return 611;
      case 131: return 612;
      case 132: return 613;
      case 133: return 614;
      case 134: return 608;
      case 135: return 711;
      case 136: return 712;
      case 137: return 653;
      case 139: return 733;
      case 140: return 734;
      case 141: return 688;
      case 142: return 685;
      case 143: return 684;
      case 144: return 686;
      case 145: return 0;
      case 146: return 683;
      case 147: return 687;
      case 148: return 728;
      case 149: return 729;
      case 150: return 721;
      case 151: return 722;
      case 152: return 627;
      case 161: return 625;
      case 162: return 626;
      case 165: return 629;
      case 166: return 631;
      case 167: return 0;
      case 170: return 698;
      case 171: return 623;
      case 172: return 624;
      case 175: return 670;
      case 176: return 670;
      case 180: return 650;
      case 181: return 649;
      case 182: return 0;
      case 183: return 675;
      case 185: return 671;
      case 186: return 672;
      case 190: return 681;
      case 191: return 682;
      case 192: return 615;
      case 193: return 616;
      case 194: return 658;
      case 195: return 609;
      case 196: return 713;
      case 197: return 714;
      case 198: return 715;
      case 199: return 716;
      case 200: return 838;
      }
      break;
    case 30:
      switch (item) {
      case 201: return 680;
      case 202: return 653;
      case 203: return 671;
      case 204: return 672;
      case 205: return 678;
      case 206: return 626;
      case 207: return 623;
      case 208: return 685;
      case 210: return 647;
      case 211: return 740;
      case 212: return 740;
      case 213: return 740;
      case 214: return 740;
      case 215: return 740;
      case 216: return 740;
      case 217: return 740;
      case 218: return 740;
      case 219: return 740;
      case 220: return 740;
      case 221: return 740;
      case 222: return 740;
      case 223: return 740;
      case 224: return 740;
      case 225: return 740;
      case 226: return 740;
      case 227: return 740;
      case 228: return 740;
      case 229: return 740;
      case 230: return 740;
      case 231: return 801;
      case 232: return 802;
      case 233: return 803;
      case 234: return 804;
      case 235: return 805;
      case 236: return 806;
      case 237: return 807;
      case 238: return 808;
      case 239: return 809;
      case 240: return 810;
      case 241: return 811;
      case 242: return 642;
      case 243: return 813;
      case 244: return 814;
      case 245: return 815;
      case 246: return 713;
      case 247: return 714;
      case 248: return 645;
      case 249: return 646;
      case 250: return 647;
      case 251: return 648;
      case 252: return 891;
      case 253: return 892;
      case 254: return 893;
      case 255: return 894;
      case 256: return 895;
      case 257: return 896;
      case 258: return 897;
      case 259: return 898;
      case 260: return 899;
      case 261: return 900;
      case 262: return 901;
      case 263: return 902;
      case 264: return 903;
      case 265: return 904;
      case 266: return 905;
      case 267: return 906;
      case 268: return 907;
      case 269: return 908;
      case 270: return 909;
      case 271: return 910;
      case 272: return 911;
      case 273: return 912;
      case 274: return 913;
      case 275: return 914;
      case 276: return 915;
      case 277: return 916;
      case 278: return 917;
      case 279: return 678;
      case 280: return 632;
      case 281: return 831;
      case 282: return 832;
      case 283: return 833;
      case 284: return 834;
      case 285: return 617;
      case 286: return 795;
      case 287: return 695;
      case 288: return 796;
      case 289: return 696;
      case 290: return 797;
      case 291: return 697;
      case 292: return 673;
      case 293: return 674;
      case 294: return 867;
      case 296: return 795;
      case 297: return 695;
      case 298: return 796;
      case 299: return 696;
      case 301: return 637;
      case 302: return 865;
      case 303: return 866;
      case 306: return 648;
      case 307: return 648;
      case 308: return 648;
      case 309: return 648;
      case 310: return 648;
      case 311: return 648;
      case 312: return 648;
      case 313: return 648;
      case 314: return 648;
      case 315: return 648;
      case 316: return 648;
      case 317: return 648;
      case 318: return 648;
      case 319: return 648;
      case 320: return 703;
      case 321: return 704;
      case 322: return 648;
      }
      break;
    case 31:
      switch (item) {
      case 201: return 715;
      case 202: return 618;
      case 211: return 660;
      case 212: return 661;
      case 213: return 662;
      case 214: return 663;
      case 215: return 664;
      case 216: return 665;
      case 217: return 666;
      case 218: return 667;
      case 219: return 668;
      case 220: return 669;
      }
      break;
    case 32:
      switch (item) {
      case 201: return 683;
      case 202: return 687;
      case 203: return 688;
      case 204: return 687;
      case 209: return 731;
      case 210: return 732;
      case 211: return 683;
      case 212: return 687;
      case 213: return 688;
      case 214: return 626;
      case 215: return 623;
      case 216: return 601;
      case 217: return 626;
      case 218: return 626;
      case 219: return 733;
      case 220: return 734;
      case 221: return 735;
      case 222: return 736;
      case 223: return 918;
      case 224: return 919;
      case 225: return 920;
      case 226: return 919;
      case 227: return 918;
      case 228: return 919;
      case 229: return 920;
      case 230: return 737;
      case 231: return 738;
      }
      break;
    case 35:
      switch (item) {
      case 101: return 601;
      case 102: return 602;
      case 121: return 701;
      case 122: return 702;
      case 130: return 611;
      case 134: return 608;
      case 135: return 711;
      case 136: return 712;
      case 137: return 653;
      case 201: return 850;
      case 202: return 851;
      case 203: return 852;
      case 204: return 853;
      case 205: return 854;
      case 206: return 855;
      case 211: return 860;
      case 213: return 862;
      case 214: return 863;
      case 215: return 864;
      case 221: return 871;
      case 224: return 872;
      case 225: return 873;
      case 231: return 876;
      case 234: return 877;
      case 235: return 878;
      case 241: return 880;
      case 244: return 881;
      case 245: return 882;
      case 251: return 885;
      case 254: return 888;
      case 255: return 884;
      }
      break;
    case 41:
      switch (item) {
      case 101: return 601;
      case 102: return 602;
      case 103: return 801;
      case 104: return 802;
      case 105: return 803;
      case 106: return 804;
      case 107: return 805;
      case 108: return 806;
      case 109: return 807;
      case 110: return 808;
      case 111: return 809;
      case 112: return 810;
      case 113: return 811;
      case 114: return 812;
      case 115: return 813;
      case 116: return 814;
      case 117: return 815;
      case 118: return 816;
      case 119: return 817;
      case 120: return 818;
      case 121: return 701;
      case 122: return 702;
      case 130: return 611;
      case 131: return 612;
      case 132: return 613;
      case 133: return 614;
      case 134: return 608;
      case 135: return 711;
      case 136: return 712;
      case 137: return 653;
      case 139: return 633;
      case 140: return 634;
      case 141: return 688;
      case 142: return 685;
      case 143: return 684;
      case 144: return 686;
      case 145: return 0;
      case 146: return 683;
      case 147: return 687;
      case 148: return 728;
      case 149: return 729;
      case 150: return 721;
      case 151: return 722;
      case 152: return 627;
      case 161: return 625;
      case 162: return 626;
      case 165: return 629;
      case 166: return 631;
      case 167: return 0;
      case 170: return 698;
      case 171: return 623;
      case 172: return 624;
      case 180: return 650;
      case 181: return 649;
      case 182: return 0;
      case 183: return 675;
      case 185: return 671;
      case 186: return 672;
      case 187: return 678;
      case 190: return 681;
      case 191: return 682;
      }
      break;
    case 42:
      switch (item) {
      case 101: return 601;
      case 102: return 602;
      case 103: return 801;
      case 104: return 802;
      case 105: return 803;
      case 106: return 804;
      case 107: return 805;
      case 108: return 806;
      case 109: return 807;
      case 110: return 808;
      case 111: return 809;
      case 112: return 810;
      case 113: return 811;
      case 114: return 812;
      case 115: return 813;
      case 116: return 814;
      case 117: return 815;
      case 118: return 816;
      case 119: return 817;
      case 120: return 818;
      case 121: return 701;
      case 122: return 702;
      case 130: return 611;
      case 131: return 612;
      case 132: return 613;
      case 133: return 614;
      case 134: return 608;
      case 135: return 711;
      case 136: return 712;
      case 137: return 653;
      case 139: return 633;
      case 140: return 634;
      case 141: return 688;
      case 142: return 685;
      case 143: return 684;
      case 144: return 686;
      case 145: return 0;
      case 146: return 683;
      case 147: return 687;
      case 148: return 728;
      case 149: return 729;
      case 150: return 721;
      case 151: return 722;
      case 152: return 627;
      case 161: return 625;
      case 162: return 626;
      case 165: return 629;
      case 166: return 631;
      case 167: return 0;
      case 170: return 698;
      case 171: return 623;
      case 172: return 624;
      case 180: return 650;
      case 181: return 649;
      case 182: return 0;
      case 183: return 675;
      case 185: return 671;
      case 186: return 672;
      case 187: return 678;
      case 190: return 681;
      case 191: return 682;
      }
      break;
    case 43:
      switch (item) {
      case 101: return 601;
      case 102: return 602;
      case 103: return 801;
      case 104: return 802;
      case 105: return 803;
      case 106: return 804;
      case 107: return 805;
      case 108: return 806;
      case 109: return 807;
      case 110: return 808;
      case 111: return 809;
      case 112: return 810;
      case 113: return 811;
      case 114: return 812;
      case 115: return 813;
      case 116: return 814;
      case 117: return 815;
      case 118: return 816;
      case 119: return 817;
      case 120: return 818;
      case 121: return 701;
      case 122: return 702;
      case 130: return 611;
      case 131: return 612;
      case 132: return 613;
      case 133: return 614;
      case 134: return 608;
      case 135: return 711;
      case 136: return 712;
      case 137: return 653;
      case 139: return 633;
      case 140: return 634;
      case 141: return 688;
      case 142: return 685;
      case 143: return 684;
      case 144: return 686;
      case 145: return 0;
      case 146: return 683;
      case 147: return 687;
      case 148: return 728;
      case 149: return 729;
      case 150: return 721;
      case 151: return 722;
      case 152: return 627;
      case 161: return 625;
      case 162: return 626;
      case 165: return 629;
      case 166: return 631;
      case 167: return 0;
      case 170: return 698;
      case 171: return 623;
      case 172: return 624;
      case 180: return 650;
      case 181: return 649;
      case 182: return 0;
      case 183: return 675;
      case 185: return 671;
      case 186: return 672;
      case 187: return 678;
      case 190: return 681;
      case 191: return 682;
      }
      break;
    case 44:
      switch (item) {
      case 101: return 601;
      case 102: return 602;
      case 103: return 801;
      case 104: return 802;
      case 105: return 803;
      case 106: return 804;
      case 107: return 805;
      case 108: return 806;
      case 109: return 807;
      case 110: return 808;
      case 111: return 809;
      case 112: return 810;
      case 113: return 811;
      case 114: return 812;
      case 115: return 813;
      case 116: return 814;
      case 117: return 815;
      case 118: return 816;
      case 119: return 817;
      case 120: return 818;
      case 121: return 701;
      case 122: return 702;
      case 130: return 611;
      case 131: return 612;
      case 132: return 613;
      case 133: return 614;
      case 134: return 608;
      case 135: return 711;
      case 136: return 712;
      case 137: return 653;
      case 139: return 633;
      case 140: return 634;
      case 141: return 688;
      case 142: return 685;
      case 143: return 684;
      case 144: return 686;
      case 145: return 0;
      case 146: return 683;
      case 147: return 687;
      case 148: return 728;
      case 149: return 729;
      case 150: return 721;
      case 151: return 722;
      case 152: return 627;
      case 161: return 625;
      case 162: return 626;
      case 165: return 629;
      case 166: return 631;
      case 167: return 0;
      case 170: return 698;
      case 171: return 623;
      case 172: return 624;
      case 180: return 650;
      case 181: return 649;
      case 182: return 0;
      case 183: return 675;
      case 185: return 671;
      case 186: return 672;
      case 187: return 678;
      case 190: return 681;
      case 191: return 682;
      }
      break;
    }
    break;
  case 3:
    switch (section) {
    case 0:
      switch (item) {
      case 177: return 620;
      case 178: return 650;
      case 179: return 675;
      case 210: return 16;
      case 211: return 728;
      case 212: return 729;
      }
      break;
    case 21:
      switch (item) {
      case 177: return 620;
      case 178: return 650;
      case 179: return 675;
      case 225: return 16;
      case 226: return 728;
      case 227: return 729;
      }
      break;
    case 22:
      switch (item) {
      case 177: return 620;
      case 178: return 650;
      case 179: return 675;
      case 225: return 16;
      case 226: return 728;
      case 227: return 729;
      }
      break;
    case 23:
      switch (item) {
      case 177: return 620;
      case 178: return 650;
      case 179: return 675;
      case 225: return 16;
      case 226: return 728;
      case 227: return 729;
      }
      break;
    case 24:
      switch (item) {
      case 177: return 620;
      case 178: return 650;
      case 179: return 675;
      case 225: return 16;
      case 226: return 728;
      case 227: return 729;
      }
      break;
    case 40:
      switch (item) {
      case 23: return 93;
      case 24: return 16;
      case 31: return 37;
      case 32: return 687;
      case 177: return 620;
      case 178: return 650;
      case 179: return 675;
      case 201: return 620;
      case 202: return 620;
      case 203: return 687;
      case 204: return 683;
      case 205: return 687;
      case 206: return 687;
      case 207: return 688;
      case 208: return 683;
      case 209: return 687;
      case 210: return 688;
      case 211: return 684;
      case 212: return 689;
      case 213: return 690;
      case 214: return 188;
      case 215: return 188;
      case 216: return 14;
      case 217: return 14;
      case 218: return 910;
      case 219: return 911;
      case 220: return 685;
      case 221: return 912;
      case 222: return 108;
      case 223: return 108;
      case 224: return 687;
      case 225: return 688;
      case 226: return 15;
      }
      break;
    }
    break;
  case 4:
    switch (section) {
    case 0:
      switch (item) {
      case 1: return 351;
      case 2: return 38;
      case 3: return 2;
      case 4: return 56;
      case 5: return 57;
      case 6: return 61;
      case 7: return 62;
      case 8: return 37;
      case 9: return 364;
      case 10: return 365;
      }
      break;
    case 1:
      switch (item) {
      case 1: return 353;
      }
      break;
    case 2:
      switch (item) {
      case 1: return 367;
      case 2: return 61;
      case 3: return 62;
      case 4: return 364;
      case 5: return 365;
      case 6: return 366;
      case 7: return 354;
      }
      break;
    case 3:
      switch (item) {
      case 1: return 355;
      }
      break;
    case 4:
      switch (item) {
      case 1: return 356;
      }
      break;
    case 5:
      switch (item) {
      case 1: return 357;
      }
      break;
    case 6:
      switch (item) {
      case 1: return 387;
      case 2: return 393;
      case 3: return 394;
      case 4: return 392;
      case 5: return 385;
      case 6: return 388;
      case 7: return 389;
      case 8: return 386;
      case 9: return 390;
      case 10: return 391;
      case 11: return 398;
      case 12: return 381;
      case 13: return 382;
      case 14: return 383;
      case 15: return 399;
      case 16: return 400;
      case 17: return 50;
      case 18: return 55;
      }
      break;
    case 7:
      switch (item) {
      case 1: return 358;
      case 2: return 359;
      case 3: return 360;
      }
    }
  }
  return -1;  
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
 *  $open=0;
 *  while (<>){
 *      if (/^1\|\s*([0-9]+)\s*\|\s*([0-9]+)\s*\|\s*([0-9]+)\s*\|/) {
 *          ($m,$s,$i)=($1,$2,$3);
 *          $open=1;
 *      }
 *      elsif ($open && /^5\|\s*([0-9]+)\s*\|\s*([0-9]+)\s*\|/) {
 *          $open=0;
 *          $ppcode=$2;
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
 *          print "return $ppcode;";
 *          print $ei;
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
