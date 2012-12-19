"""
$Id: testMpi.py 2200 2012-05-31 16:28:28Z pletzer $

Unit MPI

"""

from mpi4py import MPI
import numpy
import unittest

class Test(unittest.TestCase):
    def setUp(self):
        pass

    def test_gather(self):
        comm = MPI.COMM_WORLD
        pe = comm.Get_rank()
        nprocs = comm.Get_size()
        n = 10
        sendBuf = numpy.ones((n,), numpy.int32) * pe
        recvBuf = comm.gather(sendBuf, root=0)
        if pe == 0:
            for i in range(nprocs):
                data = numpy.ones((n,), numpy.int32) * i
                res = numpy.sum(data - recvBuf[i])
                self.assertEqual(res, 0)

    def test_gatherv(self):
        comm = MPI.COMM_WORLD
        pe = comm.Get_rank()
        nprocs = comm.Get_size()
        # variable size
        n = 10
        sendBuf = numpy.ones((n + pe,), numpy.int32) * pe
        recvBuf = comm.gather(sendBuf, root=0)
        if pe == 0:
            for i in range(nprocs):
                data = numpy.ones((n + i,), numpy.int32) * i
                res = numpy.sum(data - recvBuf[i])
                self.assertEqual(res, 0)

        
if __name__ == '__main__':
    print ""
    suite = unittest.TestLoader().loadTestsFromTestCase(Test)
    unittest.TextTestRunner(verbosity = 1).run(suite)

