# Adapted for numpy/ma/cdms2 by convertcdms.py
import cdms2, MV2

# Let's start by creating data by hand, but you could achieve this via a script,
# reading data from a file, etc..

my_data=[
     [1,2,3],
      [4,5,6],
      [7,8,9],
      [10,11,12],
     ]

# It is really easy to convert this 2 dimensional list into an array
# that CDAT will be able to process further for statistical analysis or simply to disply it

my_array=MV2.array(my_data)
print my_array.shape

# Done, that was easy, wasn't it ?
# Now we can fine tune this array, since it has been created with default values

# First its name
my_array.id='My_Array'

# Second it's type, since everytihng in the list was integer it is of type integer
# But we can change this
my_array=my_array.astype('f')

# Here we changed the typecode to  'f' which is float
# Accessible values are 'f': float, 'd': double, 'i': int , 'l': long

# Ok now we are adding "descriptive" attributes that would be useful to remember
my_array.history='first i created a list and then converted to MV2 and changed name and type'

# At this point we should also change the axis but this is for another tutorials

# We will  quickly save it into a new file for future use
f=cdms2.open('results.nc','w')
f.write(my_array)
f.close()
