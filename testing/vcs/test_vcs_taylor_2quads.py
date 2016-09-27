
import sys, os, vcs, MV2, vcs.testing.regression as regression

data = MV2.array([[-0.50428531,-0.8505522 ,],
 [ 0.70056821,-0.27235352,],
 [ 0.05106154, 0.23012322,],
 [-0.26478429, 0.11950427,],
 [ 0.85760801,-0.08336641,],
 [ 1.14083397,-0.78326507,]])

x = regression.init()
td = x.createtaylordiagram('new')
td.quadrans = 2
x.plot(data, td, skill = td.defaultSkillFunction, bg=1)
regression.run(x, "test_vcs_taylor_2quads.png")
