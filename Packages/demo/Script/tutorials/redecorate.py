# Adapted for numpy/ma/cdms2 by convertcdms.py
# import needed modules
import cdms2, vcs, cdutil, genutil, cdtime, sys, os
# get an xml file or just a netcdf file
file = os.path.join(vcs.prefix,'sample_data/tas_ccsr-95a.xml')
a=cdms2.open(file)

# get the data and print out it's shape
data=a('tas')
print data.shape
#(12, 1, 32, 64)
#
# get the list of global attributes and put them into a dictionary
list_file=a.attributes.keys()
file_dic={}
for i in range(0,len(list_file)):
  file_dic[i]=list_file[i],a.attributes[list_file[i] ]

# see what is in a list and a dictionary
print list_file
print file_dic

# now get the variable 'data' attributes and put into another dictionary
list_data=data.attributes.keys()
data_dic={}
for i in range(0,len(list_data)):
  data_dic[i]=list_data[i],data.attributes[list_data[i] ]

# print the list and the dictionary
print list_data
print data_dic


#
# calculate Annual Cycle
#
cdutil.setTimeBoundsMonthly(data)
start_time = data.getTime().asComponentTime()[0]
end_time = data.getTime().asComponentTime()[-1]

# print the time extent of the data:
print 'start_time :',start_time,'  end_time: ',end_time

# calculate annualcycle climatology
ac=cdutil.ANNUALCYCLE.climatology(data(time=(start_time, end_time, 'cob')))
for i in range(0,len(data_dic)):
  dm=data_dic[i]
  setattr(ac,dm[0],dm[1])


#
# write out file and add global attributes to file
#
o=cdms2.open('output.nc','w')
o.write(ac)
for i in range(0,len(file_dic)):
 dm=file_dic[i]
 setattr(o,dm[0],dm[1])

o.close()
a.close()
