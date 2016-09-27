import os, sys, numpy, vcs, vcs.testing.regression as regression

x = regression.init()

data_values = [ 25, 45, 55.]
data_lon = [ 5., 10., 15.]
data_lat = [ 5., 10., 15.]

data_lon_vert = [
    # Triangle (last one missing because traingle has only 3 vertices
    [2.5,7.5,5.,1.e20],
    # Square
    [7.5,12.5,12.5,7.5],
    # Diamond
    [15.,17.5,15,12.5],]

data_lat_vert = [
        # triangle
        [2.5,2.5,7.5,1.e20],
        # square
        [7.5,7.5,12.5,12.5],
        # diamond
        [12.5,15,17.5,15],]

mesh = numpy.array([data_lat_vert,data_lon_vert])
print "MESH SHAPE:",mesh.shape,mesh.dtype
mesh = numpy.transpose(mesh,(1,0,2))
print "MESH SHAPE:",mesh.shape
mesh = numpy.ma.masked_greater(mesh,1.e19)

"""
print "Triangle lats:"
print mesh[0][0]
print "Triangle lons:"
print mesh[0][1]
"""
m = x.createmeshfill()
m.levels = [20,30,50,70,80]
m.mesh = True

x.plot(numpy.array(data_values,),mesh,m,bg=True)
regression.run(x, "test_vcs_meshfill_vertices_issue.png")