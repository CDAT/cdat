import re,sys

r = re.compile("^-[li]",re.I)
l = []
for arg in sys.argv[1:]:
	for lib in re.split("\s+",arg):
		if lib != "":
			l.append(r.sub("",lib))

print re.sub("\s+","",str(l))
	
