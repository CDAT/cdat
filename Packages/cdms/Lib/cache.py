"""
CDMS cache management and file movement objects
"""
import cdurllib, urlparse, tempfile, os, time, cdmsobj, sys, errno, shelve
from error import CDMSError
MethodNotImplemented = "Method not yet implemented"
SchemeNotSupported = "Scheme not supported: "
LockError = "Lock error:"
TimeOutError = "Wait for read completion timed out:"
GlobusNotSupported = "Globus interface not supported"
RequestManagerNotSupported = "Request manager interface not supported (module reqm not found)"

_lock_max_tries = 10                    # Number of tries for a lock
_lock_naptime = 1                       # Seconds between lock tries
_cache_tempdir = None                   # Default temporary directory

def lock(filename):
    """
    Acquire a file-based lock with the given name.
    Usage: lock(filename)
    If the function returns, the lock was acquired successfully.
    Note: This function is UNIX-specific.
    Note: It is important to delete the lock via unlock() if the process
      is interrupted, otherwise subsequent locks will fail.
    """

    path = lockpath(filename)

    # Try to acquire a file-based lock
    success = 0
    tries = 0
    while (not success) and (tries < _lock_max_tries):
        try:
            if cdmsobj._debug:
                print 'Process %d: Trying to acquire lock %s'%(os.getpid(),path)
            fd = os.open(path, os.O_CREAT | os.O_WRONLY | os.O_EXCL, 0666)

        # If the open failed because the file already exists, keep trying, otherwise
        # reraise the error
        except OSError:
            if sys.exc_value.errno!=errno.EEXIST:
                raise
            tries = tries + 1
        else:
            if cdmsobj._debug:
                print 'Process %d: Acquired lock %s after %d tries'%(os.getpid(),path,tries)
            success = 1
            break

        # Sleep until next retry
        if cdmsobj._debug:
            print 'Process %d: Failed to acquire lock %s, sleeping'%(os.getpid(),path)
        time.sleep(_lock_naptime)

    # Error if the lock could not be acquired
    if not success:
        raise CDMSError, LockError + 'Could not acquire a lock on %s'%path

    # The lock succeeded, so just close the file - we don't need to write
    # anything here
    else:
        os.close(fd)

def unlock(filename):
    """
    Delete a file-based lock with the given name.
    Usage:unlock(filename) 
    If the function returns, the lock was successfully deleted.
    Note: This function is UNIX-specific.
    """

    path = lockpath(filename)
    if cdmsobj._debug:
        print 'Process %d: Unlocking %s'%(os.getpid(),path)
    os.unlink(path)

def lockpath(filename):
    """
    Generate the pathname of a lock. Creates the directory containing the lock
    if necessary.
    Usage: lockpath(filename)
    """
    global _cache_tempdir

    if not _cache_tempdir:
        tempfile.mktemp()
        _cache_tempdir = os.path.join(tempfile.tempdir,'cdms')
        if not os.path.isdir(_cache_tempdir):
            if cdmsobj._debug:
                print 'Process %d: Creating cache directory %s'%(os.getpid(),_cache_tempdir)
            os.mkdir(_cache_tempdir,0777)
    return os.path.join(_cache_tempdir,filename)

_useWindow = 0                          # If true, use a progress dialog
_pythonTransfer = 0
_globusTransfer = 1
_requestManagerTransfer = 2
_transferMethod = _pythonTransfer       # Method of transferring files

def useWindow():
    """
    Specify that dialog windows should be used if possible. Do not call this directly, use
    gui.setProgressParent instead. See useTTY.
    """
    global _useWindow
    _useWindow = 1

def useTTY():
    """
    Informational messages such as FTP status should be sent to the terminal. See useWindow.
    """
    global _useWindow
    _useWindow = 0

def useGlobusTransfer():
    """
    Specify that file transfers should use the Globus storage API (SC-API). See usePythonTransfer.
    """
    global _transferMethod
    _transferMethod = _globusTransfer

def usePythonTransfer():
    """
    Specify that file transfers should use the Python libraries urllib, ftplib. See useGlobusTransfer.
    """
    global _transferMethod
    _transferMethod = _pythonTransfer

def useRequestManagerTransfer():
    try:
        import reqm
    except ImportError:
        raise CDMSError, RequestManagerNotSupported
    global _transferMethod
    _transferMethod = _requestManagerTransfer

def copyFile(fromURL, toURL, callback=None, lcpath=None, userid=None, useReplica=1):
    """
    Copy file <fromURL> to local file <toURL>. For FTP transfers, if cache._useWindow is true,
    display a progress dialog, otherwise just print progress messages.

    For request manager transfers, <lcpath> is the logical collection distinguished name,
    <userid> is the string user ID, <useReplica> is true iff the request manager should
    search the replica catalog for the actual file to transfer.
    """
    if callback is None:
        if _useWindow:
            import gui
            dialogParent = gui.getProgressParent()
            dialog = gui.CdProgressDialog(dialogParent, fromURL)
            callback = gui.updateProgressGui
        else:
            callback = cdurllib.sampleReportHook
    (scheme,netloc,path,parameters,query,fragment)=urlparse.urlparse(fromURL)
    if scheme=='ftp':
        if _transferMethod==_pythonTransfer:
            urlopener = cdurllib.CDURLopener()

            # In window environment, attach the dialog to the opener. This will
            # be passed back to the callback function.
            if _useWindow:
                urlopener.setUserObject(dialog)
            try:
                fname, headers = urlopener.retrieve(fromURL, toURL, callback)
            except KeyboardInterrupt:
                raise                       # Window or keyboard interrupt: re-raise
            except:
                if _useWindow:
                    dialog.Destroy()
                raise 
        elif _transferMethod==_globusTransfer: # Transfer via Globus SC-API
            try:
                import globus.storage
            except ImportError:
                raise CDMSError, GlobusNotSupported

            globus.storage.transfer(fromURL, "file:"+toURL)
        else:
            raise CDMSError, SchemeNotSupported + scheme
        return
    elif _transferMethod==_requestManagerTransfer: # Request manager gransfer
        import reqm, signal

        # Define an alarm handler, to poll the request manager
        def handler(signum, frame):
            pass

        # Obtain server reference from environment variable ESG_REQM_REF if present
        serverRef = os.environ.get('ESG_REQM_REF', '/tmp/esg_rqm.ref')
        server = reqm.RequestManager(iorFile=serverRef)
        result, token = server.requestFile(userid, lcpath, path, toURL, useReplica)
        server.execute(token)

        # Poll the request manager for completion, signalled by estim<=0.0
        while 1:
            signal.signal(signal.SIGALRM, handler)
            estim = server.estimate(token)
            print 'Estimate: ',estim
            if estim<=0.0: break
            signal.alarm(3)             # Number of seconds between polls
            signal.pause()

        #!!!! Remove this when gsincftp uses the right target name !!!
            
##         oldpath = os.path.join(os.path.dirname(toURL),path)
##         os.rename(oldpath,toURL)

        #!!!!
        
        return
    else:
        raise CDMSError, SchemeNotSupported + scheme

# A simple data cache
class Cache:

    indexpath = None                    # Path of data cache index

    def __init__(self):
        if self.indexpath is None:
            self.indexpath = lockpath(".index")
            # This is a kluge to handle the very real possibility that
            # a lock was left over from an aborted process. Unfortunately,
            # this might also screw up a transfer in progress ...
            try:
                unlock("index_lock")
            except:
                pass
            lock("index_lock")
            self.index = shelve.open(self.indexpath) # Persistent cache index
            try:
                os.chmod(self.indexpath,0666) # Make index file world writeable
            except:
                pass
            self.index.close()
            unlock("index_lock")
            # Clean up pending read notifications in the cache. This will also
            # mess up tranfers in progress...
            self.clean()
            self.direc = os.path.dirname(self.indexpath) # Cache directory

    def get(self, filekey):
        """
        Get the path associated with <filekey>, or None if not present.
        """
        filekey = str(filekey)
        lock("index_lock")
        try:
            self.index = shelve.open(self.indexpath)
            value = self.index[filekey]
        except KeyError:
            value = None
        except:
            self.index.close()
            unlock("index_lock")
            raise
        self.index.close()
        unlock("index_lock")
        return value

    def put(self, filekey, path):
        """
        cache[filekey] = path
        """

        filekey = str(filekey)

        # Create a semaphore
        lock("index_lock")
        try:
            if cdmsobj._debug:
                print 'Process %d: Adding cache file %s,\n   key %s'%(os.getpid(),path,filekey)
            self.index = shelve.open(self.indexpath)
            self.index[filekey] = path
        except:
            self.index.close()
            unlock("index_lock")
            raise
        self.index.close()
        unlock("index_lock")

    def deleteEntry(self, filekey):
        """
        Delete a cache index entry.
        """
        filekey = str(filekey)

        # Create a semaphore
        lock("index_lock")
        self.index = shelve.open(self.indexpath)
        try:
            del self.index[filekey]
        except:
            pass
        unlock("index_lock")

    def copyFile(self, fromURL, filekey, lcpath=None, userid=None, useReplica=None):
        """
        Copy the file <fromURL> into the cache. Return the result path.

        For request manager transfers, lcpath is the logical collection path,
        <userid> is the string user ID, <useReplica> is true iff the request manager should
        search the replica catalog for the actual file to transfer.
        """
        
        # Put a notification into the cache, that this file is being read.
        self.put(filekey,"__READ_PENDING__")

        # Get a temporary file in the cache
        tempdir = tempfile.tempdir
        tempfile.tempdir = self.direc
        toPath = tempfile.mktemp()
        tempfile.tempdir = tempdir

        # Copy to the temporary file
        try:
            copyFile(fromURL, toPath, lcpath=lcpath, userid=userid, useReplica=useReplica)
            os.chmod(toPath,0666)           # Make cache files world writeable
        except:
            # Remove the notification on error, and the temp file, then re-raise
            self.deleteEntry(filekey)
            if os.path.isfile(toPath):
                os.unlink(toPath)
            raise

        # Add to the cache index
        self.put(filekey,toPath)

        return toPath

    def getFile(self, fromURL, filekey, naptime=5, maxtries=60, lcpath=None, userid=None, useReplica=None):
        """
        Get the file with <fileURL>. If the file is in the cache, read it.
        If another process is transferring it into the cache, wait for the
        transfer to complete. <naptime> is the number of seconds between
        retries, <maxtries> is the maximum number of retries.
        Otherwise, copy it from the remote file.

        <filekey> is the cache index key. A good choice is (datasetDN, filename)
        where datasetDN is the distinguished name of the dataset, and filename
        is the name of the file within the dataset.

        For request manager transfers, <lcpath> is the logical collection path,
        <userid> is the user string ID, <useReplica> is true iff the request manager should
        search the replica catalog for the actual file to transfer.

        Returns the path of a file in the cache.

        Note: The function does not guarantee that the file is still in the cache
        by the time it returns.
        """
        # If the file is being read into the cache, just wait for it
        tempname = self.get(filekey)
        # Note: This is not bulletproof: another process could set the cache at this point
        if tempname is None:
            fpath = self.copyFile(fromURL,filekey,lcpath=lcpath,userid=userid,useReplica=useReplica)
        elif tempname=="__READ_PENDING__":
            success = 0
            for i in range(maxtries):
                if cdmsobj._debug:
                    print 'Process %d: Waiting for read completion, %s'%(os.getpid(),`filekey`)
                time.sleep(naptime)
                tempname = self.get(filekey)

                # The read failed, or the entry was deleted.
                if tempname is None:
                    fpath = self.copyFile(fromURL,filekey,lcpath=lcpath,userid=userid,useReplica=useReplica)

                # The read is not yet complete
                elif tempname=="__READ_PENDING__":
                    continue

                # The read is finished.
                else:
                    fpath = tempname
                    success = 1
                    break
            if not success:
                raise CDMSError, TimeOutError +`filekey`

        else:
            fpath = tempname

        if cdmsobj._debug:
            print 'Process %d: Got file %s from cache %s'%(os.getpid(),fromURL,fpath)
        return fpath

    def delete(self):
        """
        Delete the cache.
        """
        if self.indexpath is not None:
            lock("index_lock")
            self.index = shelve.open(self.indexpath)
            for key in self.index.keys():
                path = self.index[key]
                if path=="__READ_PENDING__": continue # Don't remove read-pending notifications
                try:
                    if cdmsobj._debug:
                        print 'Process %d: Deleting cache file %s'%(os.getpid(),path)
                    os.unlink(path)
                except:
                    pass

                del self.index[key]
            self.index.close()
            unlock("index_lock")
            self.indexpath = None

    def clean(self):
        """
        Clean pending read notifications.
        """
        lock("index_lock")
        self.index = shelve.open(self.indexpath)
        for key in self.index.keys():
            path = self.index[key]
            if path=="__READ_PENDING__":
                del self.index[key]
        self.index.close()
        unlock("index_lock")
