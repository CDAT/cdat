import cdms2, vcs,sys
f=cdms2.open('rv_T_ANN_ft1_obs_data__ANN.nc')
T=f('rv_T_ANN_ft1_obs_data')
v = vcs.init()
#v.open()
# This will display a blank window:
v.plot(T)

# This will write a blank plot to a file:
v.png("nonblankplot")
