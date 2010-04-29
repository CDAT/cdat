# Emulate array_indexing

import array_indexing,numpy

def extract(a,b):
    b=numpy.where(numpy.equal(b,-1),0,b) # -1 means all missing so 0 is fine
    return b.choose(a)

rank = array_indexing.rank
set = array_indexing.set
