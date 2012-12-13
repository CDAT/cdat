import distarray
import numpy
import unittest
from mpi4py import MPI

class TestGhostedDistArray(unittest.TestCase):
    """
    Test distributed array
    """

    def setUp(self):
        pass

    def test_test0(self):
        """
        Test constructors
        """
        da = distarray.ghZeros( (2,3), numpy.float64 )
        da.free()
        da = distarray.ghOnes( (2,3), numpy.float64 )
        da.free()
        da = distarray.ghArray( [1,2,3] )
        da.free()

    def test_test1(self):
        """
        1D, float64
        """

        dtyp = numpy.float64

        # MPI stuff
        comm = MPI.COMM_WORLD
        rk = comm.Get_rank()
        sz = comm.Get_size()

        # create the dist array
        n = 10
        da = distarray.ghZeros( (n,), dtyp, ghostWidth=1 )
        # set data
        da[:] = 100*rk + numpy.array([i for i in range(n)], dtyp)
        # access remote data
        leftRk = (rk - 1) % sz
        print 'proc %d tries to access data from %d' % (rk, leftRk)
        leftData = da.get(pe=leftRk, winID=(1,))
        print 'leftData for rank %d = %s' % (rk, str(leftData))
        # check
        if leftRk < rk:
            self.assertEqual(leftData[0], da[-1] - 100)
        else:
            self.assertEqual(leftData[0], da[-1] + 100*(sz-1))
        # free
        da.free()

    def test_test2(self):
        """
        1D, float32
        """

        dtyp = numpy.float64

        # MPI stuff
        comm = MPI.COMM_WORLD
        rk = comm.Get_rank()
        sz = comm.Get_size()

        # create the dist array
        n = 10
        da = distarray.ghZeros( (n,), dtyp, ghostWidth=1 )
        # set data
        da[:] = 100*rk + numpy.array([i for i in range(n)], dtyp)
        # access remote data
        leftRk = (rk - 1) % sz
        print 'proc %d tries to access data from %d' % (rk, leftRk)
        leftData = da.get(pe=leftRk, winID=(1,))
        print 'leftData for rank %d = %s' % (rk, str(leftData))
        # check
        if leftRk < rk:
            self.assertEqual(leftData[0], da[-1] - 100)
        else:
            self.assertEqual(leftData[0], da[-1] + 100*(sz-1))
        # free
        da.free()

    def test_test3(self):
        """
        1D, int
        """

        dtyp = numpy.int64

        # MPI stuff
        comm = MPI.COMM_WORLD
        rk = comm.Get_rank()
        sz = comm.Get_size()

        # create the dist array
        n = 10
        da = distarray.ghZeros( (n,), dtyp, ghostWidth=1 )
        # set data
        da[:] = 100*rk + numpy.array([i for i in range(n)], dtyp)
        # access remote data
        leftRk = (rk - 1) % sz
        print 'proc %d tries to access data from %d' % (rk, leftRk)
        leftData = da.get(pe=leftRk, winID=(1,))
        print 'leftData for rank %d = %s' % (rk, str(leftData))
        # check
        if leftRk < rk:
            self.assertEqual(leftData[0], da[-1] - 100)
        else:
            self.assertEqual(leftData[0], da[-1] + 100*(sz-1))
        # free
        da.free()
        
        
if __name__ == '__main__':
    print "" # Spacer 
    suite = unittest.TestLoader().loadTestsFromTestCase(TestGhostedDistArray)
    unittest.TextTestRunner(verbosity = 1).run(suite)
    MPI.Finalize()
    
