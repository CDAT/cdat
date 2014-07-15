# Adapted for numpy/ma/cdms2 by convertcdms.py
import vcs_legacy, MV2 as MV, support


bg=support.bg

#
# First create some sample data
#
data = MV.array([[-0.50428531,-0.8505522 ,],
 [ 0.70056821,-0.27235352,],
 [ 0.05106154, 0.23012322,],
 [-0.26478429, 0.11950427,],
 [ 0.85760801,-0.08336641,],
 [ 1.14083397,-0.78326507,]])

x=vcs_legacy.init()

td=x.createtaylordiagram('new')

td.quadrans = 2
x.plot(data,td,skill = td.defaultSkillFunction,bg=bg)
support.check_plot(x)
