%module error
%{
#include "grid_storage_client.h"
#include "grid_storage_file.h"
#include "grid_storage_www.h"
%}
%inline %{
int isError(globus_result_t result){
	return (result!=0);
}
char *getError(globus_result_t result){
        globus_object_t * obj;
        char * str;
	static char sstr[256];

	obj = globus_error_get(result);
	str = globus_object_printable_to_string(obj);
	strncpy(sstr,str,256);		     /* Copy to static, so that globus string can be freed: */
					     /* avoids a memory leak. Python will make a copy of the string. */
	globus_object_free(obj);
	globus_free(str);
	return sstr;
}
%}
