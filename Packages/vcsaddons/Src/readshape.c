#include <Python.h>
#include "shapefil.h"


static PyObject *readdbffile(     PyObject *self, PyObject *args)
{
    char *file=NULL;
    DBFHandle	hDBF;
    int		*panWidth, i, iRecord;
    char	szFormat[32], *pszFilename = NULL;
    int		nWidth, nDecimals;
    int		bHeader = 0;
    int		bRaw = 0;
    int		bMultiLine = 0;
    char	szTitle[12];

    PyObject *outdict;
    PyObject *list;


    if (!PyArg_ParseTuple(args,"s",&file))
      return NULL;

           if (file == NULL) {
                 PyErr_SetString(PyExc_TypeError, "Error - Must provide a dbf file name.");
	         return NULL;
           }

    hDBF = DBFOpen( file, "rb" );
    if( hDBF == NULL )
    {
      PyErr_SetString(PyExc_TypeError,"DBFOpen failed" );
      return NULL;
    }

    outdict = PyDict_New();
    if( DBFGetFieldCount(hDBF) == 0 )
    {
      return outdict;
    }

    /* At this point creates the keys for dictionary with empty list in */
    for( i = 0; i < DBFGetFieldCount(hDBF) && !bMultiLine; i++ )
    {
	DBFFieldType	eType;

	eType = DBFGetFieldInfo( hDBF, i, szTitle, &nWidth, &nDecimals );
	PyDict_SetItemString(outdict,szTitle,PyList_New(0));
    }
/* -------------------------------------------------------------------- */
/*	Read all the records 						*/
/* -------------------------------------------------------------------- */
    for( iRecord = 0; iRecord < DBFGetRecordCount(hDBF); iRecord++ )
    {
        
	for( i = 0; i < DBFGetFieldCount(hDBF); i++ )
	{
            DBFFieldType	eType;
            
            eType = DBFGetFieldInfo( hDBF, i, szTitle, &nWidth, &nDecimals );
	    
	    list = PyDict_GetItemString(outdict,szTitle);
	    switch( eType )
	      {
	      case FTString:
		PyList_Append(list,PyString_FromString(DBFReadStringAttribute( hDBF, iRecord, i )));
		break;
                
	      case FTInteger:
		PyList_Append(list,PyInt_FromLong((long)DBFReadIntegerAttribute( hDBF, iRecord, i )));
		break;
                
	      case FTDouble:
		PyList_Append(list,PyInt_FromLong((long)DBFReadDoubleAttribute( hDBF, iRecord, i )));
		break;
                
	      default:
		printf("crap\n");
		break;
	      }
	}
    }
    DBFClose( hDBF );
    return outdict;
}

static PyObject *readshapefile(     PyObject *self, PyObject *args)

{
    SHPHandle	hSHP = NULL;
    char *file=NULL;
    int		nShapeType, nEntities, i, iPart;
    const char 	*pszPlus;
    double 	adfMinBound[4], adfMaxBound[4];

    PyObject *outlist;
    PyObject *list;
    PyObject *list2;
    PyObject *list3;


    if (!PyArg_ParseTuple(args,"s",&file))
      return NULL;

           if (file == NULL) {
                 PyErr_SetString(PyExc_TypeError, "Error - Must provide a shape file name.");
	         Py_INCREF ((PyObject *)Py_None); return Py_None;
           }

    hSHP = SHPOpen( file, "rb" );
     if( hSHP == NULL )
    {
                 PyErr_SetString(PyExc_TypeError, "Error - Unable to open file.");
	         Py_INCREF ((PyObject *)Py_None); return NULL;
    }
/*      Print out the file bounds.                                      */
/* -------------------------------------------------------------------- */
    SHPGetInfo( hSHP, &nEntities, &nShapeType, adfMinBound, adfMaxBound );

    
    outlist = PyList_New(nEntities+3);
    PyList_SetItem(outlist,0,PyInt_FromLong((long)nShapeType));
    PyList_SetItem(outlist,1,PyInt_FromLong((long)nEntities));
    PyList_SetItem(outlist,2,PyInt_FromLong((long)nShapeType));
    PyList_SetItem(outlist,0,PyInt_FromLong((long)nShapeType));
    PyList_SetItem(outlist,1,PyInt_FromLong((long)nEntities));
    list = PyList_New(8);
    PyList_SetItem(list,0,PyFloat_FromDouble((double)adfMinBound[0]));
    PyList_SetItem(list,1,PyFloat_FromDouble((double)adfMinBound[1]));
    PyList_SetItem(list,2,PyFloat_FromDouble((double)adfMinBound[2]));
    PyList_SetItem(list,3,PyFloat_FromDouble((double)adfMinBound[3]));
    PyList_SetItem(list,4,PyFloat_FromDouble((double)adfMaxBound[0]));
    PyList_SetItem(list,5,PyFloat_FromDouble((double)adfMaxBound[1]));
    PyList_SetItem(list,6,PyFloat_FromDouble((double)adfMaxBound[2]));
    PyList_SetItem(list,7,PyFloat_FromDouble((double)adfMaxBound[3]));
    PyList_SetItem(outlist,2,list);

/* -------------------------------------------------------------------- */
/*	Skim over the list of shapes, printing all the vertices.	*/
/* -------------------------------------------------------------------- */
    for( i = 0; i < nEntities; i++ )
    {
	int		j;
        SHPObject	*psShape;


	psShape = SHPReadObject( hSHP, i );
	list2 = PyList_New(psShape->nParts+3);
	PyList_SetItem(list2,0,PyInt_FromLong((long)psShape->nSHPType));
	PyList_SetItem(list2,1,PyInt_FromLong((long)psShape->nParts));
	list3 = PyList_New(8);
	PyList_SetItem(list3,0,PyFloat_FromDouble((double)psShape->dfXMin));
	PyList_SetItem(list3,1,PyFloat_FromDouble((double)psShape->dfYMin));
	PyList_SetItem(list3,2,PyFloat_FromDouble((double)psShape->dfZMin));
	PyList_SetItem(list3,3,PyFloat_FromDouble((double)psShape->dfMMin));
	PyList_SetItem(list3,4,PyFloat_FromDouble((double)psShape->dfXMax));
	PyList_SetItem(list3,5,PyFloat_FromDouble((double)psShape->dfYMax));
	PyList_SetItem(list3,6,PyFloat_FromDouble((double)psShape->dfZMax));
	PyList_SetItem(list3,7,PyFloat_FromDouble((double)psShape->dfMMax));
	PyList_SetItem(list2,2,list3);
	Py_XINCREF(list3);
/* 	printf( "\nShape:%d (%s)  nVertices=%d, nParts=%d\n" */
/*                 "  Bounds:(%12.3f,%12.3f, %g, %g)\n" */
/*                 "      to (%12.3f,%12.3f, %g, %g)\n", */
/* 	        i, SHPTypeName(psShape->nSHPType), */
/*                 psShape->nVertices, psShape->nParts, */
/*                 psShape->dfXMin, psShape->dfYMin, */
/*                 psShape->dfZMin, psShape->dfMMin, */
/*                 psShape->dfXMax, psShape->dfYMax, */
/*                 psShape->dfZMax, psShape->dfMMax ); */
	list3 = PyList_New(0);
	for( j = 0, iPart = 1; j < psShape->nVertices; j++ )
	{
            const char	*pszPartType = "";

            if( j == 0 && psShape->nParts > 0 )
                pszPartType = SHPPartTypeName( psShape->panPartType[0] );
            
	    if( iPart < psShape->nParts
                && psShape->panPartStart[iPart] == j )
	    {
		PyList_SetItem(list2,iPart+2,list3);
                pszPartType = SHPPartTypeName( psShape->panPartType[iPart] );
		iPart++;

		//pszPlus = "+";
		list3 = PyList_New(0);
	    }
	    else
	        pszPlus = " ";
	    PyList_Append(list3,PyFloat_FromDouble((double)psShape->padfX[j]));
	    PyList_Append(list3,PyFloat_FromDouble((double)psShape->padfY[j]));
	    PyList_Append(list3,PyFloat_FromDouble((double)psShape->padfZ[j]));
	    PyList_Append(list3,PyFloat_FromDouble((double)psShape->padfM[j]));
/* 	    printf("   %s (%12.3f,%12.3f, %g, %g) %s \n", */
/*                    pszPlus, */
/*                    psShape->padfX[j], */
/*                    psShape->padfY[j], */
/*                    psShape->padfZ[j], */
/*                    psShape->padfM[j], */
/*                    pszPartType ); */
	}
	PyList_SetItem(list2,iPart+2,list3);
        SHPDestroyObject( psShape );
	PyList_SetItem(outlist,i+3,list2);
    }

    SHPClose( hSHP );
   return outlist;
}

static PyMethodDef MyExtractMethods[]= {
  {"readshapefile", readshapefile , METH_VARARGS},
  {"readdbffile", readdbffile , METH_VARARGS},
  {NULL, NULL} /*sentinel */
};

void 
init_gis()
{
  (void) Py_InitModule("_gis", MyExtractMethods);  
}

int main(int argc,char **argv)
{
  Py_SetProgramName(argv[0]);
  Py_Initialize();
  init_gis();}

