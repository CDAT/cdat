#########################
# Simple netcdf plotter #
#########################

# Needed modules
import vcs, sys, cdms

# Arguments
if len(sys.argv) < 3:
	print 'Usage: python quickview.py <filename> <varname>'
	sys.exit(1)
filename = sys.argv[1]
varname = sys.argv[2]

# Open netcdf file
f=cdms.open(filename)

# Read our variable
s=f(varname)

# Create vcs canvas
x=vcs.init()

# Plot it
x.plot(s)