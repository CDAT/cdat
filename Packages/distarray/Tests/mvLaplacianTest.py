#/usr/bin/env python

"""
Example of distributed array test 

This code is provided with the hope that it will be useful. 
No guarantee is provided whatsoever. Use at your own risk.

Alex Pletzer, Tech-X Corp. (2012)
"""

__version__ = "1.0"

import numpy
import distarray
import time

class LaplacianTest:

    def __init__(self, n=10,):
        """
        Constructor
        @param n number of cells per PE domain
        """
        self.n = n
        self.ndim = 2

    def setFunction(self, funct):
        """
        Set the function that defines the input
        @param funct a user defined function taking a list of arguments (x,y,z,..)
        """
        self.funct = funct

    def setDecomp(self, comm, decomp):
        """
        Set the domain decomposition
        @param comm communicator
        @param decomp domain decomposition (number of procs in each direction)
        """
        self.comm = comm
        self.myid = comm.Get_rank()
        self.decomp = decomp
        # total number of procs
        self.npes = reduce(lambda x,y: x*y, decomp)

        # {dim: {'-': pe}, {'+':pe}, ...}
        # a dict assigning neighboring pes on the lo and hi sides of a given
        # PE domain. The last index varies fastest.
        self.neighbors = {
            0: {'-': self.myid - decomp[1],
                '+': self.myid + decomp[1]},
            1: {'-': self.myid - 1,
                '+': self.myid + 1,}, 
            }
        

    def getLaplacian(self, x0s, ls, plot = False):
        """
        Compute the discretized Laplacian
        @param x0s lower corner coordinates along each dimension
        @param ls pe domain sizes along each dimension
        @param plot True if want to show plot
        @return discretized Laplacian in local pe domain
        """

        self.hs = [ls[i]/float(self.n) for i in range(self.ndim)]
        self.ls = ls
        self.x0s = x0s

        # set input data values
        shp = (self.n, self.n)
        data = numpy.zeros(shp, numpy.float64)
        for j in range(self.n):
            y = x0s[0] + ls[0]*(j + 0.5)/float(self.n)
            for i in range(self.n):
                x = x0s[1] + ls[1]*(i + 0.5)/float(self.n)
                data[j, i] = self.funct(x, y)

        # create MPI windows and attach data
        inData = distarray.ghZeros(shp, numpy.float64, ghostWidth=1)
        # set local domain values
        inData[:] = data

        # compute the Laplacian
        res = numpy.zeros(shp, numpy.float64)
        for dim in range(self.ndim):
            # iterate over dimensions (or windows)
            
            # inner nodes, no communication necessary
            c = inData.getSlab(dim, slice(1, self.n-1)) # center
            p = inData.getSlab(dim, slice(2, self.n  )) # plus
            m = inData.getSlab(dim, slice(0, self.n-2)) # minus
            res[c] += inData[p] + inData[m] - 2.0*inData[c]

            # slabs that require communication

            # lo
            c = inData.getSlab(dim, slice(0, 1))
            p = inData.getSlab(dim, slice(1, 2))
            neighPe = self.neighbors[dim]['-']
            outsidePhysDom = False
            if neighPe < 0:
                # no data transfer if out of physical domain
                neighPe = self.myid
                outsidePhysDom = True
            # collective (all pes)
            winIndex = [0 for i in range(self.ndim)]
            winIndex[dim] = +1 # hi side for neighbor
            remoteData = inData.get(neighPe, winID=tuple(winIndex))
            remoteData = numpy.reshape(remoteData, inData[p].shape)
            if not outsidePhysDom:
                res[c] += inData[p] + remoteData - 2.0*inData[c]
            
            # hi
            c = inData.getSlab(dim, slice(self.n-1, self.n  ))
            m = inData.getSlab(dim, slice(self.n-2, self.n-1))
            neighPe = self.neighbors[dim]['+']
            outsidePhysDom = False
            if neighPe >= self.npes:
                # no data transfer if out of physical domain
                neighPe = self.myid
                outsidePhysDom = True
            # collective (all pes)
            winIndex = [0 for i in range(self.ndim)]
            winIndex[dim] = -1 # lo side for neighbor
            remoteData = inData.get(neighPe, winID=tuple(winIndex))
            remoteData = numpy.reshape(remoteData, inData[m].shape)
            if not outsidePhysDom:
                res[c] += remoteData + inData[m] - 2.0*inData[c]

        inData.free() # must explicitly free windows

        if plot:
            import re
            import os
            # save data in file, one per pe, then combine data
            fname = 'mvLaplacianTest_rk%d.txt' % self.myid
            numpy.savetxt(fname, res, fmt = '%12.6g')
            self.comm.barrier()
            if self.myid == 0:
                self.show('mvLaplacianTest_rk*.txt')
                
        return res
            
    def show(self, rootFilename):
        """
        Plot the solution
        @param rootFilename a wild card expression, eg 'XXX_rk%d.txt'
        """
        import glob
        from matplotlib import pylab
        import re
        import os
        
        dataWindows = {}
        for f in glob.glob(rootFilename):
            pe = int(re.search(r'_rk(\d+)\.txt', f).group(1))
            dataWindows[pe] = numpy.loadtxt(f, unpack=False)
        
        nBigSizes = (self.decomp[0]*self.n,
                     self.decomp[1]*self.n)
        bigData = numpy.zeros(nBigSizes, numpy.float64)
        for j in range(self.decomp[0]):
            begJ = j*self.n
            endJ = begJ + self.n
            for i in range(self.decomp[1]):
                pe = j*self.decomp[1] + i
                begI = i*self.n
                endI = begI + self.n
                minVal = min(dataWindows[pe].flat)
                maxVal = max(dataWindows[pe].flat)
                bigData[begJ:endJ, begI:endI] = dataWindows[pe]

        xs = self.x0s[1] + numpy.array([(i + 0.5)*self.hs[1] for i in \
                          range(1, self.decomp[1]*self.n-1)])
        ys = self.x0s[0] + numpy.array([(j + 0.5)*self.hs[0] for j in \
                          range(1, self.decomp[0]*self.n-1)])
        pylab.contour(xs, ys, bigData[1:-1,1:-1], 21)
        pylab.colorbar()
        # plot the domain decomp
        for m in range(1, self.decomp[0]):
            yVal = self.x0s[0] + m * self.ls[0] - self.hs[0] / 2.0
            pylab.plot([xs[0], xs[-1]], [yVal, yVal], 'k--')
            yVal += self.hs[0]
            pylab.plot([xs[0], xs[-1]], [yVal, yVal], 'k--')
        for n in range(1, self.decomp[1]):
            xVal = self.x0s[1] + n * self.ls[1] - self.hs[1] / 2.0
            pylab.plot([xVal, xVal], [ys[0], ys[-1]], 'k--')
            xVal += self.hs[1]
            pylab.plot([xVal, xVal], [ys[0], ys[-1]], 'k--')
        pylab.show()
        fname = re.sub(r'_rk\*.txt', '', rootFilename) + '.png'
        print 'saving colorplot in file ' + fname
        pylab.savefig(fname)

######################################################################

def test():

    import sys
    from mpi4py import MPI
    from math import exp

    # parallel stuff
    comm = MPI.COMM_WORLD
    rk = comm.Get_rank()
    sz = comm.Get_size()

    def getOptimalDecomp():
        n = 1
        res = [n, sz]
        srf = res[0] + res[1]
        n = 2
        while n <= sz//2:
            m = (sz//n)
            vol = n * m
            newSrf = n + m
            if vol == sz and newSrf < srf:
                res[0] = n
                res[1] = m
                srf = newSrf
            n += 1
        return res

    decomp = getOptimalDecomp()
    print '[%d] decomp = %s' % (rk, str(decomp))

    # physical domain sizes
    Ly, Lx = decomp[0]*1.0, decomp[1]*1.0

    # pe domain sizes
    ly = Ly / float(decomp[0])
    lx = Lx / float(decomp[1])

    n = rk // decomp[1]
    m = rk % decomp[1]

    y0 = n * ly
    x0 = m * lx
    
    def fcn(*args):
        x0, y0 = Lx/2.0, Ly/2.0
        x, y = args
        #return y
        return exp( -(x-x0)**2/0.2 - (y-y0)**2/0.2 )
        #return (x**3 + y**3)
        #return ((x - x0)**3 + (y - y0)**3) / 2.0


    # number of cells per pe domain
    nCells = 20 
    if len(sys.argv) > 1: nCells = int(sys.argv[1])

    tic = time.time()
    lp = LaplacianTest(n = nCells)
    lp.setFunction(fcn)
    lp.setDecomp(comm, decomp)
    lp.getLaplacian(x0s=(y0, x0), ls=(lx, ly), plot=True)
    toc = time.time()
    if rk == 0:
        print 'time to compute Laplacian: %g sec' % (toc - tic)
    
if __name__ == '__main__': test()
