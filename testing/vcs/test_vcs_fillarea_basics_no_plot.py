import vcs
import numpy
import cdtime

from vcs_test_common import *

x=vcs.init()

f=x.createfillarea()
assert(vcs.queries.isfillarea(f))

test_values_setting(f,"style",[f,0,1,2,3,"hatch","pattern","hallow"],[-1,4,"foo",[],{},(),None])
test_values_setting(f,"index",[None,f,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,],[0,21,"foo",[],(),{}])
test_values_setting(f,"color",range(256))
test_values_setting(f,"color",[f,"red",[2,3,4],None],[-1,256,[[2,3,4],]])
test_values_setting(f,["x","y"],[None,[1,2,3],],[1,"sdf",[1,2,"3"],[[1,2,3],2]])
b = x.createfillarea("test_f_ok",f.name)
assert(b.name == "test_f_ok")
assert(b.style == ["hallow",])
assert(b.index == [20,])
assert(b.color == [241])
assert(f.x == [1,2,3])
assert(f.y == [1,2,3])
