# Create Python wrapper for globus grid_storage module

all:	grid_storage_wrap.c

grid_storage_wrap.c:	grid_storage.i error.i
	swig -python grid_storage.i
