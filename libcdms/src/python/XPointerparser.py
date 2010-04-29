#!/usr/local/bin/python

# Parse an XPointer

import re
import string

_opS = '[ \t\r\n]*'
op=re.compile('(?P<op>child|descendant)\(')
id=re.compile('id\(')
name=re.compile('(?P<name>[a-zA-Z_:][-a-zA-Z0-9._:]*)')
number=re.compile('(?P<number>[0-9]+)')
value=re.compile('(?P<value>[^\)]*)')
lparen=re.compile('\(')
rparen=re.compile('\)')
comma=re.compile(',')
starparen=re.compile('\*\)')

_Any = 0

class XPointerParser:

	def __init__(self,verbose=0):
		self.verbose=verbose
		self.reset()

	def reset(self):
		self.xpointer=''
		
	def parse(self,xpointer):
		self.xpointer=xpointer
		i=0
		n=len(xpointer)
		while i<n:
			k=self.parse_one(i,n)
			if k==n:
				break
			elif k==i:
				raise RuntimeError, 'invalid syntax: %s' % xpointer[k]
			elif xpointer[k]=='.':
				i=k+1
				continue
			else:
				raise RuntimeError, 'expected a period: %s' % xpointer[k]
		
	def parse_one(self,i,n):
		xpointer=self.xpointer
		while i<n:
			matchObj = id.match(xpointer,i)
			if matchObj:
				k=self.parse_id(matchObj,i)
				if self.verbose: print i,k,n
				break
			matchObj = op.match(xpointer,i)
			if matchObj:
				k=self.parse_op(matchObj,i)
				if self.verbose: print i,k,n
				break
			matchObj = name.match(xpointer,i)
			if matchObj:
				k=self.parse_name(matchObj,i)
				if self.verbose: print i,k,n
				break
			k=i
		return k
				
	def parse_id(self,matchObj,i):
		xpointer = self.xpointer
		i=matchObj.end()
		matchObj = name.match(xpointer,i)
		if not matchObj:
			raise RuntimeError, 'expected a name'
		target = matchObj.group('name')
		if self.verbose: print 'Id target:',target
		i=matchObj.end()
		matchObj = rparen.match(xpointer,i)
		if not matchObj:
			raise RuntimeError, 'expected a right paren'
		self.handle_id(target)
		return matchObj.end()
		
	def parse_name(self,matchObj,i):
		xpointer = self.xpointer
		target=matchObj.group('name')
		self.handle_id(target)
		return matchObj.end()
		
	def parse_op(self,matchObj,i):
		xpointer = self.xpointer
		opname = matchObj.group('op')
		i=matchObj.end()
		matchObj = number.match(xpointer,i)
		if not matchObj:
			raise RuntimeError, 'expected a number'
		position = string.atoi(matchObj.group('number'))
		i=matchObj.end()
		matchObj = comma.match(xpointer,i)
		if not matchObj:
			raise RuntimeError, 'expected a comma'
		i=matchObj.end()
		matchObj = name.match(xpointer,i)
		if matchObj:
			eltype=matchObj.group('name')
			i=matchObj.end()
		elif xpointer[i]=='.':
			eltype=_Any
			i=i+1
		else:
			raise RuntimeError, 'expected a name or period'
		
		attname=attval=None	
		matchObj = comma.match(xpointer,i)
		if matchObj:
			i=matchObj.end()
			matchObj=name.match(xpointer,i)
			if matchObj:
				attname=matchObj.group('name')
				i=matchObj.end()
			elif xpointer[i]=='*':
				attname=_Any
				i=i+1
			else:
				raise RuntimeError, 'expected an attribute name or "*"'
			matchObj = comma.match(xpointer,i)
			if not matchObj:
				raise RuntimeError, 'expected a comma'
			i=matchObj.end()
			matchObj = starparen.match(xpointer,i)
			if matchObj:
				attval=_Any
				i=matchObj.end()-1
			else:
				matchObj = value.match(xpointer,i)
				if matchObj:
					attval=matchObj.group('value')
					i=matchObj.end()
				else:
					raise RuntimeError, 'expected an attribute value or "*"'
			if self.verbose: print 'value=',value
			
		matchObj = rparen.match(xpointer,i)
		if not matchObj:
			raise RuntimeError, 'expected a right paren'
		if opname=='child':
			self.handle_child(position,eltype,attname,attval)
		elif opname=='descendant':
			self.handle_descendant(position,eltype,attname,attval)
		return matchObj.end()
							
	def handle_id(self,target):
		print 'id('+target+')'
		
	def handle_child(self,position,eltype,attname,attval):
		print 'child(',position,eltype,')'
		if attname is not None: print '  where %s==%s' % (attname,attval)
		
	def handle_descendant(self,position,eltype,attname,attval):
		print 'descendant(',position,eltype,')'
		if attname is not None: print '  where %s==%s' % (attname,attval)

		
if __name__=='__main__':
	p=XPointerParser()
	p.parse('child(1,dataset)')
	p.parse('descendant(2,variable,units,W/M^2)')
	p.parse('id(hfss).descendant(1,domElem,name,time)')
	p.parse('child(1,.)')
	p.parse('descendant(2,variable,*,W/M^2)')
	p.parse('descendant(2,variable,units,*)')



