#
# ESG request manager Python interface
# 
from Fnorb.orb import CORBA
RequestManagerUnavailable = 'Request manager is unavailable.'
InvalidRequestManager = 'Invalid request manager!'

class RequestManager:
    """
    ESG request manager singleton class. The first instance initializes CORBA
    and creates the server proxy object. Subsequent instances call the cached
    server object.
    """

    server = None

    def __init__(self, iorFile = 'Server.ref', orbArgs = []):
        if self.server==None:

            # Initialise the ORB.
            orb = CORBA.ORB_init(orbArgs, CORBA.ORB_ID)
            
            # Read the server's stringified IOR from a file (this is just a quick and
            # dirty way of locating the server - a better method is to use
            # the naming or trader services).
            try:
                stringified_ior = open(iorFile).read()
            except:
                raise RequestManagerUnavailable

            # Convert the stringified IOR into an active object reference.
            server = orb.string_to_object(stringified_ior)

            # Make sure that the server is not a 'nil object reference' (represented
            # in Python by the value 'None').
            if server is None:
                raise 'Nil object reference!'

            # Make sure that the object implements the expected interface!
            try:
                server_check = server._is_a('IDL:RequestManager:1.0')
            except:
                raise RequestManagerUnavailable
                
            if not server_check:
                raise InvalidRequestManager

            RequestManager.server = server

    def request(self, userid, requestList):
        """
        server.request(userid, requestList)

        creates a request for files to be transferred. The function returns a tuple
        (result, token), where <result> is an error indicator, and <token> is the
        string request token.

        <userid> is a string user ID.
        <requestList> is a list of REQUESTs, where:

        A REQUEST is created by REQUEST(source, target, spec, flag);
        <source> and target are FILE_LOCATIONs: FILE_LOCATION(dataset, file);
        <dataset> and <file> are strings;
        <spec> is a SLABSPEC: SLABSPEC(variableName, datatype, selection);
        <variableName> and <datatype> are strings;
        <selection> is a list of TUPLES: [TUPLE(start, stop, stride), TUPLE(...), ...]
        <flag> is true (1) iff a replica catalog should be searched for the actual source file.

        Example:

            lc = "lc=B04.10.atm, rc=PCMDI Replica Catalog, o=Argonne National Laboratory, o=Globus, c=US"
            targetdir = "file:/pcmdi/drach/ngi"
            source1 = FILE_LOCATION(lc, "B04.10.atm.0049.nc")
            target1 = FILE_LOCATION(targetdir, "B04.10.atm.0049.nc")
            selection1 = [TUPLE(0,12,1),TUPLE(0,18,1),TUPLE(0,64,1),TUPLE(0,128,1)]
            specs11 = SLABSPEC("U", "Float", selection1)
            specs12 = SLABSPEC("V", "Float", selection1)
            specs1 = [specs11, specs12]
            req1 = REQUEST(source1, target1, specs1, 1)

            source2 = FILE_LOCATION(lc, "B04.10.atm.0050.nc")
            target2 = FILE_LOCATION(targetdir, "B04.10.atm.0050.nc")
            selection2 = [TUPLE(0,12,1),TUPLE(0,18,1),TUPLE(0,64,1),TUPLE(0,128,1)]
            specs21 = SLABSPEC("U", "Float", selection2)
            specs22 = SLABSPEC("V", "Float", selection2)
            specs2 = [specs21, specs22]
            req2 = REQUEST(source2, target2, specs2, 1)

            requests = [req1, req2]
            result, token = server.request("u17374", requests)
        
        """
        return self.server.request(userid, requestList)

    def requestFile(self, userid, dataset, sourcefile, targetfile, useReplica=1):
        """
        server.requestFile(self, dataset, sourcefile, targetfile, useReplica=1)

        creates a request for a file to be tranferred. The function returns a tuple
        (result, token), where <result> is an error indicator, and <token> is the
        string request token.

        <dataset> is the string name of the source dataset.
        <sourcefile> is the path of the source file.
        <targetfile> is the path of the target file.
        <useReplica> is true, iff <dataset> is a logical collection name in
          the replica catalog. If false, <dataset> is a URL.
          
        """
        from _GlobalIDL import FILE_LOCATION, REQUEST

        source = FILE_LOCATION(dataset, sourcefile)
        target = FILE_LOCATION("", targetfile)
        requests = [REQUEST(source, target, [], useReplica)]
        return self.request(userid, requests)

    def execute(self, token):
        """
        server.execute(token)

        executes a request.

        <token> is the request token returned by a request call.
        """
        return self.server.execute(token)

    def cancel(self, token):
        """
        server.cancel(token)

        cancels a request.

        <token> is the request token returned by a request call.
        """

        return self.server.cancel(token)

    def estimate(self, token):
        """
        server.estimate(token)

        returns an estimate of time remaining.

        <token> is the request token returned by a request call.
        """

        return self.server.estimate(token)

    def status(self, token):
        """
        server.status(token)

        returns the status of a file transfer, as a list of FILE_STATUS objects.

        <token> is the request token returned by a request call.
        """

        return self.server.status(token)

    def staged(self, token, filename):
        """
        server.staged(token, filename)

        return true iff the file has been staged.

        <token> is the request token returned by a request call.
        """
        return self.server.staged(token, filename)

    def stageFailed(self, token, filename):
        """
        server.stageFailed(token, filename)

        return true iff the file stage operation failed.

        <token> is the request token returned by a request call.
        """
        return self.server.stageFailed(token, filename)

