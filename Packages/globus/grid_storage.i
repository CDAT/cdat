%module grid_storage
%include error.i
%{
#include "grid_storage_client.h"
#if defined(HAVE_FILE_DRIVER)
#include "grid_storage_file.h"
#endif
#if defined(HAVE_WWW_DRIVER)
#include "grid_storage_www.h"
#endif
#if defined(HAVE_DPSS_DRIVER)
#include "grid_storage_dpss.h"
#endif
%}

#define GRID_STORAGE_ATTRIBUTE_CREATE_MODE      "ATTRIBUTE::CREATE_MODE"
#define GRID_STORAGE_ATTRIBUTE_CRED_HANDLE      "ATTRIBUTE::CRED_HANDLE"
#define GRID_STORAGE_ATTRIBUTE_LOG_READ         "ATTRIBUTE::LOG_READ"
#define GRID_STORAGE_ATTRIBUTE_LOG_STRING       "ATTRIBUTE::LOG_STRING"
#define GRID_STORAGE_ATTRIBUTE_LOG_WRITE        "ATTRIBUTE::LOG_WRITE"
#define GRID_STORAGE_ATTRIBUTE_NETLOGGER_FILE   "ATTRIBUTE::NETLOGGER_FILE"
#define GRID_STORAGE_ATTRIBUTE_NETLOGGER_HOST   "ATTRIBUTE::NETLOGGER_HOST"
#define GRID_STORAGE_ATTRIBUTE_NETLOGGER_PORT   "ATTRIBUTE::NETLOGGER_PORT"
#define GRID_STORAGE_ATTRIBUTE_OPEN_FLAGS       "ATTRIBUTE::OPEN_FLAGS"
#define GRID_STORAGE_ATTRIBUTE_REMOTE_RANDOM_ACCESS     "ATTRIBUTE::REMOTE_RANDOM_ACCESS"
#define GRID_STORAGE_ATTRIBUTE_SEQUENTIAL       "ATTRIBUTE::SEQUENTIAL"
#define GRID_STORAGE_ATTRIBUTE_TRANSFER_SERVER  "ATTRIBUTE::TRANSFER_SERVER"
#define GRID_STORAGE_ATTRIBUTE_VALUE_FALSE      "FALSE"
#define GRID_STORAGE_ATTRIBUTE_VALUE_TRUE       "TRUE"

typedef char grid_storage_url_t;

extern globus_result_t
grid_storage_add_attribute(
	char *				attribute_name,
	char *				attribute_value,
	grid_storage_attribute_t *	attribute_set);

globus_result_t
grid_storage_destroy_attribute_set(
	grid_storage_attribute_t *	attribute_set);

extern globus_result_t grid_storage_transfer(grid_storage_url_t *source_url, grid_storage_url_t *destination_url, grid_storage_attribute_t *attributes);
%inline %{
grid_storage_attribute_t *create_attribute_set(){
	grid_storage_attribute_t *attr = (grid_storage_attribute_t *)malloc(sizeof(grid_storage_attribute_t));
	*attr = GRID_STORAGE_ATTRIBUTE_INITIALIZER;
	return attr;
}
void delete_attribute_set(grid_storage_attribute_t *attr){
	free(attr);
}
%}
%init %{
#if defined(HAVE_FILE_DRIVER)
	globus_module_activate(GRID_STORAGE_FILE_MODULE);
#endif
#if defined(HAVE_WWW_DRIVER)
	globus_module_activate(GRID_STORAGE_WWW_MODULE);
#endif
#if defined(HAVE_DPSS_DRIVER)
	globus_module_activate(GRID_STORAGE_DPSS_MODULE);
#endif
%}
