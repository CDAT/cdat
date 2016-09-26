from globus import grid_storage

##

MustBeStringError = "Attribute value must be a string"

CREATE_MODE = grid_storage.GRID_STORAGE_ATTRIBUTE_CREATE_MODE
CRED_HANDLE = grid_storage.GRID_STORAGE_ATTRIBUTE_CRED_HANDLE
LOG_READ = grid_storage.GRID_STORAGE_ATTRIBUTE_LOG_READ
LOG_STRING = grid_storage.GRID_STORAGE_ATTRIBUTE_LOG_STRING
LOG_WRITE = grid_storage.GRID_STORAGE_ATTRIBUTE_LOG_WRITE
NETLOGGER_FILE = grid_storage.GRID_STORAGE_ATTRIBUTE_NETLOGGER_FILE
NETLOGGER_HOST = grid_storage.GRID_STORAGE_ATTRIBUTE_NETLOGGER_HOST
NETLOGGER_PORT = grid_storage.GRID_STORAGE_ATTRIBUTE_NETLOGGER_PORT
OPEN_FLAGS = grid_storage.GRID_STORAGE_ATTRIBUTE_OPEN_FLAGS
REMOTE_RANDOM_ACCESS = grid_storage.GRID_STORAGE_ATTRIBUTE_REMOTE_RANDOM_ACCESS
SEQUENTIAL = grid_storage.GRID_STORAGE_ATTRIBUTE_SEQUENTIAL
TRANSFER_SERVER = grid_storage.GRID_STORAGE_ATTRIBUTE_TRANSFER_SERVER
VALUE_FALSE = grid_storage.GRID_STORAGE_ATTRIBUTE_VALUE_FALSE
VALUE_TRUE = grid_storage.GRID_STORAGE_ATTRIBUTE_VALUE_TRUE

def transfer(source_url, destination_url, attributes={}):
    """
    transfer(source_url, destination_url, attributes={})

    Copy file source_url to file destination_url.

    source_url: string URL of source file
    destination_url: string URL of destination file. Local files must have 'file:/' prefix
    attributes: Dictionary of attribute-value pairs used to customize the behaviour
      of the function. Attribute values must be strings. Predefined attributes include:

      LOG_READ : ??? (VALUE_TRUE | VALUE_FALSE)
      LOG_STRING : Log string to write (e.g., "GSID=PCMDI FILE=http://www-pcmdi.llnl.gov/index.html"
      LOG_WRITE : Write to log (VALUE_TRUE | VALUE_FALSE)
      NETLOGGER_FILE : ???
      NETLOGGER_HOST : URL of NetLogger host machine (e.g., "pitcairn.mcs.anl.gov")
      NETLOGGER_PORT : port number on which NetLogger is running (e.g., "14830")
      TRANSFER_SERVER : ???

    """
    attrset = grid_storage.create_attribute_set()
    for attname in attributes.keys():
        attval = attributes[attname]
        if type(attval)!=type(""):
            raise MustBeStringError,attname+"="+`attval`
        result = grid_storage.grid_storage_add_attribute(attname,attributes[attname],attrset)
        errString = grid_storage.getError(result)
        if grid_storage.isError(result):
            errString = grid_storage.getError(result)
            raise errString, 'Adding attribute %s=%s'%(attname,attributes[attname])
        
    result = grid_storage.grid_storage_transfer(source_url, destination_url, attrset)
    grid_storage.grid_storage_destroy_attribute_set(attrset)
    
    if grid_storage.isError(result):
        errString = grid_storage.getError(result)
        raise errString, 'transferring %s to %s'%(source_url, destination_url)
