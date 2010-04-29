#!/usr/bin/env python
"""
Generate a .pyf file from a properly formated fortran file

Usage: genpyf.py <fortran_file> <pyf_file>
"""
import sys, re

fw = open(sys.argv[1])
fp = open(sys.argv[2],'w')
def add_nextline(ntab=0,nnl=1):
	global i
	fline = ntab*'\t'+lines[i]
	while lines[i].endswith('&'):
		i+=1
		fline += lines[i]
	fline = fline.replace('&','')+'\n'*nnl
	fp.write(fline)
	i+=1
redec = re.compile('(real|logical|integer)')
fp.write("! -*- Mode: f90 -*-\n\npython module spanlib_fort\ninterface\n")
lines = [l[:-1].strip() for l in fw.xreadlines()]
i = 0
while i < len(lines):
	# Find subroutine blocks
	if "subroutine" in lines[i].lower():
		# Subroutine declaration
		add_nextline()
		# Variales
		while redec.search(lines[i]) is None: i+=1
		while len(lines[i]): add_nextline(ntab=1)
		# End subroutine
		while 'end subroutine' not in lines[i].lower(): i+=1
		add_nextline(nnl=2)
	i+=1
fw.close()
fp.write("end interface\nend python module\n\n")
fp.close()
		