# Adapted for numpy/ma/cdms2 by convertcdms.py
# Let's import the necessary modules
import cdms2 as cdms,MV2 as MV

# Let's create the variable first
rain= MV.array([2.,15.,3.5,4.])
rain.id='pr'
rain.units='cm'

# Now we need to create the associated "time" axis, let's use days' since 2005 as units
# First the axis itslef with its values
# values can be passed as a list, an array or a variable
time=cdms.createAxis([2,4,16,22]) # Remember Jan 1st, is 0 days since Jan 1st!
# Now let's name it
time.id='time'
# Let's give it some units
time.units='days since 2005-1-1'

# Another very important attribute are the bounds, indeed we need to know the "extend" of each value
# Here we will assume that the data span a full day everytime, but they could as well spend a few hours
# of these days only

bounds = MV.array ([[2.,3.],
                    [4.,5.],
                    [16.,17.],
                    [22.,23.]])

time.setBounds(bounds)

# Now we have to link the variable dimension to this axes object

rain.setAxis(0,time)

rain.info()
