macro(strappend VAR STR)
  set(${VAR} "${${VAR}}|${STR}")
endmacro()

#build up a list of things to look for in logs that indicate problems
set(FAIL_EXPRESSIONS "ERROR")
strappend(FAIL_EXPRESSIONS "Error")
strappend(FAIL_EXPRESSIONS "FAIL")
strappend(FAIL_EXPRESSIONS "Fail")
strappend(FAIL_EXPRESSIONS "CRITICAL")

#build up a list of exceptions to the above
set(OK_EXPRESSIONS "vtk*Error*.cxx") #ParaView
strappend(OK_EXPRESSIONS "xmlFailures") #libXML2
strappend(OK_EXPRESSIONS "TIFFError.3tiff") #TIFF
strappend(OK_EXPRESSIONS " xml*ErrorFunc ")
strappend(OK_EXPRESSIONS "tstLastError")
strappend(OK_EXPRESSIONS "Failed to build these modules:") #Python
strappend(OK_EXPRESSIONS "ErrorHandling.py") #Python
strappend(OK_EXPRESSIONS "Errors.py") #Python
strappend(OK_EXPRESSIONS "SyntaxError") #Python
strappend(OK_EXPRESSIONS "cairo-Error-handling") #Cairo
strappend(OK_EXPRESSIONS "AtlasNotFoundError") #NUMPY
strappend(OK_EXPRESSIONS "PTHREAD_MUTEX_ERRORCHECK") #OPENMPI
strappend(OK_EXPRESSIONS "MPI_Error_class|MPI_Error_string") #OPENMPI
strappend(OK_EXPRESSIONS "TopologyValidationError") #GEOS
strappend(OK_EXPRESSIONS "AssertionFailedException") #GEOS
strappend(OK_EXPRESSIONS "AtlasNotFoundError") #scikits
strappend(OK_EXPRESSIONS "BlasNotFoundError") #scikits
strappend(OK_EXPRESSIONS "SWIG_Python_AddErrorMsg") #Visit
strappend(OK_EXPRESSIONS "GetPluginErrorsRPC.C") #Visit
strappend(OK_EXPRESSIONS "avtMatErrorExpression") #Visit
