"""Customized URLopener"""

import urllib, getpass, socket, string, sys

MAXFTPCACHE = 10        # Trim the ftp cache beyond this size

class CDURLopener(urllib.URLopener):

    def __init__(self, proxies=None):
        urllib.URLopener.__init__(self,proxies)
        self._userObject = None

    # Attach an object to be returned with callbacks
    def setUserObject(self, userObject):
        self._userObject = userObject

    # Use FTP protocol
    def open_ftp(self, url):
        host, path = urllib.splithost(url)
        if not host: raise IOError, ('ftp error', 'no host given')
        host, port = urllib.splitport(host)
        user, host = urllib.splituser(host)
        # if user: user, passwd = splitpasswd(user)
        if user: passwd = getpass.getpass()
        else: passwd = None
        host = urllib.unquote(host)
        user = urllib.unquote(user or '')
        passwd = urllib.unquote(passwd or '')
        host = socket.gethostbyname(host)
        if not port:
            import ftplib
            port = ftplib.FTP_PORT
        else:
            port = int(port)
        path, attrs = urllib.splitattr(path)
        path = urllib.unquote(path)
        dirs = string.splitfields(path, '/')
        dirs, file = dirs[:-1], dirs[-1]
        if dirs and not dirs[0]: dirs = dirs[1:]
        key = (user, host, port, string.joinfields(dirs, '/'))
        # XXX thread unsafe!
        if len(self.ftpcache) > MAXFTPCACHE:
            # Prune the cache, rather arbitrarily
            for k in self.ftpcache.keys():
                if k != key:
                    v = self.ftpcache[k]
                    del self.ftpcache[k]
                    v.close()
        try:
            if not self.ftpcache.has_key(key):
                print 'Creating ftpwrapper: ',user,host,port,dirs
                self.ftpcache[key] = \
                    urllib.ftpwrapper(user, passwd, host, port, dirs)
            if not file: type = 'D'
            else: type = 'I'
            for attr in attrs:
                attr, value = urllib.splitvalue(attr)
                if string.lower(attr) == 'type' and \
                   value in ('a', 'A', 'i', 'I', 'd', 'D'):
                    type = string.upper(value)
            (fp, retrlen) = self.ftpcache[key].retrfile(file, type)
            if retrlen is not None and retrlen >= 0:
                import mimetools, StringIO
                headers = mimetools.Message(StringIO.StringIO(
                    'Content-Length: %d\n' % retrlen))
            else:
                headers = noheaders()
            return urllib.addinfourl(fp, headers, "ftp:" + url)
        except urllib.ftperrors(), msg:
            raise IOError, ('ftp error', msg), sys.exc_info()[2]

    def retrieve(self, url, filename=None, reporthook=None, blocksize=262144):
        url = urllib.unwrap(url)
        if self.tempcache and self.tempcache.has_key(url):
            return self.tempcache[url]
        type, url1 = urllib.splittype(url)
        if not filename and (not type or type == 'file'):
            try:
                fp = self.open_local_file(url1)
                hdrs = fp.info()
                del fp
                return url2pathname(urllib.splithost(url1)[1]), hdrs
            except IOError, msg:
                pass
        fp = self.open(url)
        headers = fp.info()
        if not filename:
            import tempfile
            garbage, path = urllib.splittype(url)
            garbage, path = urllib.splithost(path or "")
            path, garbage = urllib.splitquery(path or "")
            path, garbage = urllib.splitattr(path or "")
            suffix = os.path.splitext(path)[1]
            filename = tempfile.mktemp(suffix)
            self.__tempfiles.append(filename)
        result = filename, headers
        if self.tempcache is not None:
            self.tempcache[url] = result
        tfp = open(filename, 'wb')
        bs = blocksize
        size = -1
        blocknum = 1
        if reporthook:
            if headers.has_key("content-length"):
                size = int(headers["Content-Length"])
            stayopen = reporthook(0, bs, size, self._userObject)
            if stayopen==0:
                raise KeyboardInterrupt
        bytesread = 0
        block = fp.read(bs)
        if reporthook:
            stayopen = reporthook(1, bs, size, self._userObject)
            if stayopen==0:
                raise KeyboardInterrupt
        while block:
            tfp.write(block)
            bytesread = bytesread + len(block)
##             print blocknum, bytesread, size,
##             if blocknum*blocksize!=bytesread:
##                 print ' (*)'
##             else:
##                 print
            if block and reporthook:
                stayopen = reporthook(blocknum, bs, size, self._userObject)
                if stayopen==0:
                    raise KeyboardInterrupt
            blocknum = blocknum + 1
            block = fp.read(bs)
        # fp.close()
        tfp.close()
        del fp
        del tfp
        return result

def sampleReportHook(blocknum, blocksize, size, userObj):
    sizekb = size/1024
    percent = min(100,int(100.0*float(blocknum*blocksize)/float(size)))
    print "Read: %3d%% of %dK"%(percent,sizekb)
    return 1

if __name__ == '__main__':

    import sys
    if len(sys.argv)!=4:
        print 'Usage: cdurllib.py URL filename blocksize'
        sys.exit(1)

    url = sys.argv[1]
    filename = sys.argv[2]
    blocksize = int(sys.argv[3])
    
    urlopener = CDURLopener()
    fname, headers = urlopener.retrieve(url, filename, sampleReportHook, blocksize)
    print fname, 'written'
