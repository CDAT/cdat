from numpy import *
import MLab, LinearAlgebra
import rs
from numpy import *
Error = "EOF Error."
def eigsort (d, v, nr):
    "Sort the eigenvalues and vectors, return first nr of them"
    np = len(d)
    result = array(d)
    ires = range(np)
    for i in range(np):
        for j in range(i+1, np):
            if result[j] > result[i]:
                t = result[i]
                result[i] = result[j]
                result[j] = t
                t = ires[i]
                ires[i] = ires[j]
                ires[j] = t
    return result[0:nr], take(v, ires, 1)[:, 0:nr]

class Analyzer:
    def analyze(self, z, nr=0, space=None):
        assert space is None or space == "sample" or space == "state"
        assert nr >= 0
        nt, np = shape (z)
        zt = transpose(z)
        mr = min(nt-1, np)
        if nr == 0:
            nr = mr
        else:
            nr = min(nr, mr)

        if space is None:  #sample or state space?
            if nt >= np: space = "state"
            else: space="sample"
        self.space = space

        if space == "state":
            s = matrix(zt) * matrix(z) / nt
            self.eval, self.evec, self.ierr = rs.rs (s, 1)
            if self.ierr:
                raise Error, "Eigenvector routine error " + str (self.ierr)
            total_eval = add.reduce(self.eval)
            self.eval, self.evec = eigsort (self.eval, self.evec, nr)
        else:
            t = matrix(z)*matrix(zt) / np
            m, f, self.ierr = rs.rs (t, 1)
            if self.ierr:
                raise Error, "Eigenvector routine error " + str (self.ierr)
            total_eval = add.reduce(m)
            m, f = eigsort(m, f, nr)
            self.eval = m
            self.evec = matrix(zt)*matrix(f)
            self.evec = self.evec / sqrt(self.eval)
            
        # make unique spaces for eignevectors
        for i in range (nr):
            for j in range (nt):
                t = self.evec[j,i]
                if t < 0.0:
                    self.evec[:,i] = -self.evec[:,i]
                    break
                elif t > 0.0:
                    break

        self.a = matrix(z)*matrix(self.evec)
        self.pct = 100.0 * self.eval / total_eval

if __name__ == "__main__":
    a = Analyzer ()
    xxx=array ([[1.,2.,3.,4.,5.],
             [2.,3.2, 2.9, 4., 5.1],
             [3.,4.,5., 5.5, 6.5],
             [2., 3.1, 2.95, 4.1, 4.9]])
    xmean = add.reduce (xxx) / len (xxx[:,0])
    y = xxx - xmean
    print y
    a.analyze (y)
    print "eval", a.space
    print a.eval
    print "evec"
    print a.evec
    print "a"
    print a.a
    print "pct"
    print a.pct
    a.analyze (y, space='state')
    print "eval", a.space
    print a.eval
    print "evec"
    print a.evec
    print "a"
    print a.a
    print "pct"
    print a.pct
