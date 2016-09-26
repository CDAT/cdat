# Create Python wrapper for ESG Request Manager

all:	_GlobalIDL

_GlobalIDL: esgdefs.idl reqm.idl
	fnidl -I. reqm.idl
	touch _GlobalIDL
