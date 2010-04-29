# Adapted for numpy/ma/cdms2 by convertcdms.py
# ASV 0.2  2 November 2000  Laurence Tratt  laurie@tratt.net
#
# Homepage: http://tratt.net/~laurie/python/asv/
#
# Copyright (c) 2000 Laurence Tratt.  All rights reserved.

# Redistribution and use in source and binary forms, with or without
# modification, are permitted provided that the following conditions
# are met:
#
# 1. Redistributions of source code must retain the above copyright
#    notice, this list of conditions and the following disclaimer.
# 2. Redistributions in binary form must reproduce the above copyright
#    notice, this list of conditions and the following disclaimer in the
#    documentation and/or other materials provided with the distribution.
# 3. All advertising materials mentioning features or use of this software
#    must display the following acknowledgement:
#      This product includes software developed by Laurence Tratt.
# 4. The name of the Laurence Tratt may not be used to endorse or promote
#    products derived from this software without specific prior written 
#    permission.
# 
# THIS SOFTWARE IS PROVIDED BY LAURENCE TRATT ``AS IS'' AND
# ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
# IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
# ARE DISCLAIMED.  IN NO EVENT SHALL LAURENCE TRATT BE LIABLE
# FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
# DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS
# OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION)
# HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
# LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY
# OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF
# SUCH DAMAGE.

# History:
#   ASV 0.1  29 October 2000
#
#   ASV 0.2  2 November 2000
#
#   ASV 0.3  1 March 2001
#     First public release
#
#   ASV 0.4
#     * Fixed bug where __setitem__ ignored the field_names paramater
#     * Added code to make ASV `executeable' with command line parameters etc






import sys, types
from UserList import UserList






class ASVError(Exception): pass
class Mismatched_Field_Names_Error(ASVError): pass
class Too_Many_Fields_Error(ASVError): pass






class ASV(UserList):


	def __init__(self):
	
		UserList.__init__(self)
		
		self._field_names = None
	
	
	
	
	def __getitem__(self, x):
	
		"""Return row x as an instance of Row from this ASV instance"""
	
		return Row(self.data[x], self._field_names)
	
	
	
	
	def __setitem__(self, x, row, field_names = None):
	
		"""Set item x to row
		
		row can be either a Row instance or a list. field_names is a list of field names that each
		element in the row correnponds with and only makes sense if row is a list since a Row
		instance already has field names"""
	
		# XXX This method of doing things is awful
	
		self.append(row, field_names)
		self.data[x] = self.data[-1]
		del self.data[-1]
	
	
	
	
	def append(self, row, field_names = None):
	
		"""Append row to this ASV instance
		
		row can be either a Row instance or a list. field_names is a list of field names that each
		element in the row correnponds with and only makes sense if row is a list since a Row
		instance already has field names"""
	
		# If we've been given a 'Row' to append, it doesn't make sense to specify field names,
		#   since the row itself contains field names
	
		if isinstance(row, Row) and field_names:
				raise ASVError("Can't specify row of type Row and field_names since a Row object has its own field_names")
		
		if isinstance(row, Row) or field_names:
			# If we've been given a 'Row', or 'field_names' has been specified when 'row' is a list,
			#   then we need to check the field names we've been given against those of our instance.
			#   The names *don't* need to be in the same order in each list
			
			if isinstance(row, Row):
				field_names = row.get_field_names()
			
			# As a quick speed hack, we check to see if the 'field_names' object is the *same* object
			#   as our own field names. If they both point to the same object, we don't have to
			#   bother checking that each field name occurs in both lists
			
			if field_names is not self._field_names:
			
				# If we're being given more names in 'field_names' than we know of in 
				#   'self._field_names' then self evidently the caller has gone wrong
			
				if len(field_names) > len(self._field_names):
					raise Mismatched_Field_Names_Error("Too many field names from row to ASV instance")
					
				# Iterate over the field names we've been given checking that each name occurs in
				#   'self._field_names'; this works because len(field_names) must be 
				#   <= len(self._field_names) due to a previous check
					
				for field_name in field_names:
					try:
						self._field_names.index(field_name)
					except ValueError:
						raise Mismatched_Field_Names_Error("Field '%s' not found in ASV instance" % field_name)
			
			# Construct a row that has the fields from the input row in the same order as
			#   'self._field_names' so that we can safely append it to our own data
			
			row_as_list = []
			for field_name in self._field_names:
				if field_names.count(field_name) == 0 or field_names.index(field_name) >= len(row):
					row_as_list.append("")
				else:
					row_as_list.append(row[field_names.index(field_name)])
			
			self.data.append(row_as_list)
		else:
			# Check that if we have 'self._field_names', the row doesn't exceed that lists length
			
			if self._field_names is not None and len(row) > len(self._field_names):
				raise Too_Many_Fields_Error("'%s' contains more than the %s field names this ASV instance knows about" % (row, len(self._field_names)))
			
			# If we have 'self._field_names' and len(row) is < len(self._field_names), pad the row out
			#   with blank items out to bring it up to the same length
			# Otherwise simply append the row
			
			if self._field_names is not None and len(row) < len(self._field_names):
				self.data.append(row + [""] * (len(self._field_names) - len(row)))
			else:			
				self.data.append(row)
	
	
	
	def extend(self, rows, field_names = None):
	
		"""Append list rows to this ASV instance
		
		rows must be a list. Elements in rows should either all be Row instances or lists themselves;
		if this is not the case the result of the extend is undefined.
		
		row can be either a Row instance or a list. field_names is a list of field names that each
		element in the row correnponds with and only makes sense if row is a list since a Row
		instance already has field names."""
	
		for x in rows:
			self.append(x, field_names)
	
	
	
	
	def input(self, data, input_class, *args, **kwargs):
	
		"""
		Process input data using input_class
		
		Although the input_class can specify what type data should be, in general it should be a
		string.
		
		See 'input and output classes' in the main documentation for more discussion of what
		input_class should refer to.
		"""
	
		apply(input_class.input, (self, data) + args, kwargs)
	
	
	
	
	def input_from_file(self, input_file, input_class, *args, **kwargs):
	
		"""
		Process input data from a named file
		
		This is a convenience method. input_file should be the name of a readable file; see the
		input method for details of the other arguments
		"""
	
		file = open(input_file, "rb")
		data = file.read(-1)
		file.close()
		apply(self.input, (data, input_class) + args, kwargs)
	
	
	
	
	def output(self, output_class, *args, **kwargs):
	
		"""
		Create output data using output_class
		"""
	
		return apply(output_class.output, (self, ) + args, kwargs)
	
	
	
	
	def output_to_file(self, output_file, output_class, *args, **kwargs):
	
		"""
		Output data straight to a named file
		
		This is a convenience method for the output method
		"""
	
		output = apply(self.output, (output_class, ) + args, kwargs)
		file = open(output_file, "wb")
		file.write(output)
		file.close()
	
	
	
	
	def set_field_names(self, field_names):
	
		"""
		Set the field names for this ASV instance
		
		field_names must be a list of strings.
		
		You can not set field names if you they have already been set (either explicitly be using
		this method or indirectly by another method such as input) or if this ASV instance already
		holds data.
		"""
	
		if self._field_names is not None:
			raise ASVError("Field names can not be changed once set")
		
		if len(self.data) > 0:
			raise ASVError("Field names can not be set once data has already been stored")
			
		self._field_names = field_names[ : ]
	
	
	
	
	def get_field_names(self):
	
		"""
		Return this ASV instances field names
		
		Returns None if this ASV instance does not have any field names
		"""
	
		if self._field_names is None:
			return None
		else:
			return self._field_names[ : ]




class SimpleSV:


	_SPEECH_MARKS = ['"', "'"]
	_ESCAPE_CHARS = ["\\"]
	_NEWLINES = ["\n", "\r\n", "\r", "\n\r"]
	_COMMENTS = ["#"]
	_DOUBLE_SPEECH_IS_ESCAPE = 1
	
	_OUTPUT_ESCAPE_CHAR = "\\"
	_OUTPUT_SPEECH_CHAR = "\""
	_OUTPUT_DOUBLE_SPEECH_CHAR_FOR_ESCAPE = 0


	def input(self, asv, input_data, has_field_names = 0, remove_field_names = 0):
	
		if not has_field_names and remove_field_names:
			raise ASVError("remove_field_names makes no sense if has_field_names is not on")
	
		rows = []
		input_data_pos = 0
		
		# The length of 'input_data' never changes so create a variable with that value rather than
		#   repeatedly calling len(input_data)
		
		input_data_len = len(input_data)
		
		# Set up an inital row to append items to
		# 'row' is updated every time a newline is found which is why there isn't the inner while
		#   loop one might otherwise expect
		
		row = []
		
		# Create a variable so we know where the start of a line is. We use this later to weed
		#   out blank lines
		# As with 'row', this variable is updated each time a newline is found
		
		input_data_start_of_line_pos = input_data_pos		
		while input_data_pos < input_data_len:
		
			# Skip whitespace at the beginning of the item
			
			input_data_pos = self._skip_whitespace(input_data, input_data_pos)
			
			# Strip out any comments
			
			stripped_a_comment = 0    # Know if we stripped a comment so we can start the while loop
			                          #   again. Oh, for named loops...
			for comment in self._COMMENTS:
				if type(comment) == types.StringType:
					# With comments of type string, we look for the comment character and then ignore
					#   everything until the end of the line or end of the file, whichever one comes
					#   first
					
					if input_data.startswith(comment, input_data_pos):
						while input_data_pos < input_data_len:
							input_data_new_pos = self._skip_newline(input_data, input_data_pos)
							if input_data_new_pos != input_data_pos:
							
								# In theory, not only should we update
								#   'input_data_start_of_line_pos', but also 'row' as we have come
								#   across a newline. As this line has been a comment and therefore
								#   contained no data, 'row' is empty so it's not really worth
								#   making the effort to create a new list for 'row' when the one it
								#   has is so far unused
								
								input_data_start_of_line_pos = input_data_pos = input_data_new_pos
								break
							input_data_pos += 1
						stripped_a_comment = 1
						break
				else:
					raise ASVError("Unsupported comment type")
			if stripped_a_comment:
				continue
			
			# See if this data item begins with a speech mark
			# Set 'speech_mark' to the speech mark found or None if no speech mark found
			
			for speech_mark in self._SPEECH_MARKS:
				if input_data.startswith(speech_mark, input_data_pos):
					input_data_pos += len(speech_mark)
					break
			else:
				speech_mark = None
			
			# Process an item (or cell or whatever you want to call it)
			# The item is stored in a list and is made up solely of strings which we later join
			#   together to create a single string. This is purely an efficiency measure as
			#   new_string = string1 + string2 is so slow in Python
			
			item = []
			while input_data_pos < input_data_len:
				escaped_a_char = 0    # Know if we escaped a character so we can break out of the
				                      #   while loop rather than the for loop
				for escape_char in self._ESCAPE_CHARS:
					if input_data.startswith(escape_char, input_data_pos):
						if input_data_pos + 1 >=  input_data_len:
							# No point looking for an escaped character if this is the last character
							#   in the file: what could it be escaping? Append the escape character
							#   itself to the item

							item.append(escape_char)
							input_data_pos += 1
						else:
							item.append(input_data[input_data_pos + 1])
							input_data_pos += 2
						escaped_a_char = 1
						break
				if escaped_a_char:
					continue
				
				# If this item started with a speech mark, see if we've reached the closing speech
				#   mark
				
				if speech_mark and input_data.startswith(speech_mark, input_data_pos):
					input_data_pos += len(speech_mark)

					if self._DOUBLE_SPEECH_IS_ESCAPE and input_data_pos < input_data_len and input_data.startswith(speech_mark, input_data_pos):
						item.append(speech_mark)
						input_data_pos += len(speech_mark)
						continue
						
					# After a closing speech mark we ignore *everything* until we either come across
					#   a seperator or a newline or we reach the end of the input
			
					input_data_pos = self._skip_whitespace(input_data, input_data_pos)
					while input_data_pos < input_data_len:
						input_data_new_pos = self._skip_newline(input_data, input_data_pos)
						if input_data_new_pos != input_data_pos:
							break
						input_data_new_pos = self._skip_seperator(input_data, input_data_pos)
						if input_data_new_pos != input_data_pos:
							input_data_pos = input_data_new_pos
							break
						input_data_pos += 1
					break
				
				# If the item is not wrapped in speech marks, check to see if we've come across a
				#   seperator to mark the end of the item
				
				if not speech_mark:
					input_data_new_pos = self._skip_seperator(input_data, input_data_pos)
					if input_data_new_pos != input_data_pos:
						input_data_pos = input_data_new_pos
						break
				
				# At the end of a line?
				
				input_data_new_pos = self._skip_newline(input_data, input_data_pos)
				if input_data_new_pos != input_data_pos:
					break
				
				item.append(input_data[input_data_pos])
				input_data_pos += 1

			# If the item wasn't wrapped in speech marks, strip off the trailing whitespace (the
			#   leading whitespace has already been stripped)

			if not speech_mark:
			
				# Since we already have a method to skip off the leading whitespace we reverse the
				#   item, strip the leading whitespace and then reverse it back
				
				item.reverse()
				item = item[self._skip_whitespace(item, 0) : ]
				item.reverse()
			
			row.append("".join(item))
			
			# If we're at the end of a line, or the end of the input data, append the current row to
			#   the list of rows and create a new row
			
			input_data_new_pos = self._skip_newline(input_data, input_data_pos)
			if input_data_new_pos != input_data_pos or input_data_pos == input_data_len:
			
				# This is where we weed out blank lines. We do this by using the _skip_whitespace
				#   function to see if when you take the text from the beginning of the line up to
				#   (but not including) the newline you are left with a null string or not
			
				if self._skip_whitespace(input_data[input_data_start_of_line_pos : input_data_new_pos], 0) != input_data_pos - input_data_start_of_line_pos:
					rows.append(row)
				#else:
				#	print row, input_data[input_data_start_of_line_pos : input_data_new_pos]
				row = []
				
				input_data_start_of_line_pos = input_data_pos = input_data_new_pos
		
		# If this file has field names, set the relevant attribute and then peel those off 'rows'
		
		if has_field_names:
			field_names = rows[0]
			rows = rows[1 : ]
			if remove_field_names:
				asv.extend(rows)
			else:
				if asv.get_field_names() is None:
					asv.set_field_names(field_names)
				asv.extend(rows, field_names)
		else:
			asv.extend(rows)




	def _skip_whitespace(self, input_data, input_data_pos):
	
		while input_data_pos < len(input_data) and input_data[input_data_pos] in self._WHITESPACE:
			input_data_pos += 1
		
		return input_data_pos




	def _skip_newline(self, input_data, input_data_pos):
	
		for newline in self._NEWLINES:
			if input_data.startswith(newline, input_data_pos):
				input_data_pos += len(newline)
				break

		return input_data_pos




	def _skip_seperator(self, input_data, input_data_pos):
	
		for seperator in self._SEPERATORS:
			if input_data.startswith(seperator, input_data_pos):
				input_data_pos += len(seperator)
				break

		return input_data_pos
	
	
	
	
	def output(self, asv, newline = "\n", keep_it_simple = 0, quotes_around_everything = 0):
	
		output = []
		
		if asv.get_field_names() is not None:
			output.append(self._construct_line(asv.get_field_names(), keep_it_simple, quotes_around_everything))
			output.append(newline)
			
			# If the mode isn't 'keep_it_simple', we output a newline after the field headings
			
			if not keep_it_simple:
				output.append(newline)
		
		for row in asv:
			output.append(self._construct_line(row, keep_it_simple, quotes_around_everything))
			output.append(newline)
		
		# Output a new line at the end of the file which tends to keep badly written programs happy
		
		output.append(newline)
		
		return "".join(output)



	def _construct_line(self, items, keep_it_simple, quotes_around_everything):
	
		# Any items that have trailing or leading space or that have '_OUTPUT_SEPERATOR' in them are
		#   quoted in speech marks
		# Escape any speech mark characters, and escape any escape characters in the item itself
	
		output_items = []
		for item in items:
			for escape_char in self._ESCAPE_CHARS:
				item = item.replace(escape_char, escape_char * 2)
			if self._OUTPUT_DOUBLE_SPEECH_CHAR_FOR_ESCAPE:
				item = item.replace(self._OUTPUT_SPEECH_CHAR, "%s%s" % (self._OUTPUT_SPEECH_CHAR, self._OUTPUT_SPEECH_CHAR))
			else:
				for speech_mark in self._SPEECH_MARKS:
					item = item.replace(speech_mark, "%s%s" % (self._OUTPUT_ESCAPE_CHAR, speech_mark))
			if quotes_around_everything or item.strip() != item or item.find(self._OUTPUT_SEPERATOR) != -1 or item.find(self._OUTPUT_SPEECH_CHAR) != -1:
				output_items.append('%s%s%s' % (self._OUTPUT_SPEECH_CHAR, item, self._OUTPUT_SPEECH_CHAR))
			else:
				output_items.append(item)
		
		# If the mode is 'keep_it_simple', we output the item seperator on its own without a space
		#   directly after it
		
		if keep_it_simple:
			return ("%s" % self._OUTPUT_SEPERATOR[0]).join(output_items)
		else:
			return ("%s " % self._OUTPUT_SEPERATOR[0]).join(output_items)





class CSV(SimpleSV):

	"Standard CSV (Comma Seperated Values)"


	_SEPERATORS = [","]
	_WHITESPACE = " \t"
	
	_OUTPUT_SEPERATOR = ","






class WinCSV(CSV):

	"As standard CSV, but outputs speech characters as \"\" rather than \\\" for compatability with Windows programs"
	
	_OUTPUT_DOUBLE_SPEECH_CHAR_FOR_ESCAPE = 1





class TSV(SimpleSV):

	"Standard TSV (Tab Seperated Values)"


	_SEPERATORS = ["\t"]
	_WHITESPACE = " "
	
	_OUTPUT_SEPERATOR = "\t"



	def _construct_line(self, items, keep_it_simple, quotes_around_everything):
	
		# We need our own '_construct_line' for TSV because empty items need to be wrapped in speech
		#   marks or they are likely to get gobbled up by over eager whitespace munchers which
		#   don't interpret TAB TAB correctly

		# Any items that have trailing or leading space or that have '_OUTPUT_SEPERATOR' in them are
		#   quoted in speech marks
		# Escape any speech mark characters, and escape any escape characters in the item itself
	
		output_items = []
		for item in items:
			for escape_char in self._ESCAPE_CHARS:
				item = item.replace(escape_char, escape_char * 2)
			if self._OUTPUT_DOUBLE_SPEECH_CHAR_FOR_ESCAPE:
				item = item.replace(self._OUTPUT_SPEECH_CHAR, "%s%s" % (self._OUTPUT_SPEECH_CHAR, self._OUTPUT_SPEECH_CHAR))
			else:
				for speech_mark in self._SPEECH_MARKS:
					item = item.replace(speech_mark, "%s%s" % (self._OUTPUT_ESCAPE_CHAR, speech_mark))
			if quotes_around_everything or item.strip() != item or item.find(self._OUTPUT_SEPERATOR) != -1 or item.find(self._OUTPUT_SPEECH_CHAR) != -1 or item.strip() == "":
				output_items.append('%s%s%s' % (self._OUTPUT_SPEECH_CHAR, item, self._OUTPUT_SPEECH_CHAR))
			else:
				output_items.append(item)
		
		# If the mode is 'keep_it_simple', we output the item seperator on its own without a space
		#   directly after it
		
		if keep_it_simple:
			return ("%s" % self._OUTPUT_SEPERATOR[0]).join(output_items)
		else:
			return ("%s " % self._OUTPUT_SEPERATOR[0]).join(output_items)




class WinTSV(TSV):

	"As standard TSV, but outputs speech characters as \"\" rather than \\\" for compatability with Windows programs"
	
	_OUTPUT_DOUBLE_SPEECH_CHAR_FOR_ESCAPE = 1




class ColonSV(SimpleSV):

	"Colon Seperated Values"


	_SEPERATORS = [";"]
	_WHITESPACE = " \t"
	
	_OUTPUT_SEPERATOR = ";"





class Row(UserList):


	def __init__(self, items, field_names = None):

		self.data = items
		self._field_names = field_names




	def __getitem__(self, x):
	
		if isinstance(x, types.IntType):
			return self.data[x]
		else:
			return self.data[self._field_names.index(x)]
		
	
	
	def __setitem__(self, x, item):
	
		if isinstance(x, types.IntType):
			self.data[x] == item
		else:
			self.data[self._field_names.index(x)] = item




	def get_field_names(self):
	
		if self._field_names is None:
			return None
		else:
			return self._field_names[ : ]




	def set_field_names(self, field_names):
	
		if self._field_names is not None:
			raise ASVError("Field names can not be changed once set")
		
		if len(self.data) > 0:
			raise ASVError("Field names can not be set once data has already been stored")
			
		self._field_names = field_names[ : ]
	
	



if __name__ == "__main__":

	builders = []
	builder_names = []
	for thing in globals().values():
		if type(thing) == types.ClassType:
			# Flatten the super classes of this class ie we get a list of this classes super
			#   classes, and all of the super classes classes etc
			
			super_classes = list(thing.__bases__)
			super_classes_pos = 0
			while super_classes_pos < len(super_classes):
				for super_class in super_classes[super_classes_pos].__bases__:
					if super_class not in super_classes:
						super_classes.append(super_class)
				super_classes_pos += 1
			
			for super_class in super_classes:
				if super_class.__name__ in ["SimpleSV"]:
					builders.append(thing)
					builder_names.append(thing.__name__.lower())
	
	# Sort the builder names (and the builders) into alphabetical value
	# This assumes that each entry in builder_names is simply builder[pos].__name__ to ensure that
	#   both builders and builder_names remain in sync
	
	builder_names.sort()
	builders.sort(lambda x, y : cmp(x.__name__.lower(), y.__name__.lower()))
	
	has_valid_arguments = 1
	if len(sys.argv) == 5:
		# Process the input file arguments
		
		if sys.argv[1] == "-c" and len(sys.argv[2]) > 0 and sys.argv[2][1 : ].lower() in builder_names:
			input_file_name = None
			input_builder_name = sys.argv[2][1 : ]
		elif len(sys.argv[1]) > 0 and sys.argv[1][1 : ] in builder_names:
			input_file_name = sys.argv[2]
			input_builder_name = sys.argv[1][1 : ]
		else:
			has_valid_arguments = 0
		
		# Process the output file arguments
		
		if sys.argv[3] == "-c" and len(sys.argv[4]) > 0 and sys.argv[4][1 : ].lower() in builder_names:
			output_file_name = None
			output_builder_name = sys.argv[4][1 : ]
		elif len(sys.argv[3]) > 0 and sys.argv[3][1 : ] in builder_names:
			output_file_name = sys.argv[4]
			output_builder_name = sys.argv[3][1 : ]
		else:
			has_valid_arguments = 0
	else:
		has_valid_arguments = 0
	
	if not has_valid_arguments:
		print """asv [-c] -<input builder> [<input file name>] [-c] -<output builder> [<output file name>]

Possible builders are:
"""

		for builder in builders:
			print "   ", builder.__name__.lower(),
			if builder.__doc__:
				print ":", builder.__doc__,
			print
		
		print """
-c can be used instead of the input/output file name to enable reading from stdin/stdout.

Example:

  asv -tsv old_data.tsv -c -csv
  Will read in a TSV file, convert it to CSV and output the result to screen"""

		sys.exit(0)

	if input_file_name is None:
		input_file = sys.stdin
	else:
		try:
			input_file = open(input_file_name, "rb")
		except IOError, e:
			print "Unable to open file '%s' for reading" % input_file_name
			sys.exit(0)
			
	if output_file_name is None:
		output_file = sys.stdout
	else:
		try:
			output_file = open(output_file_name, "wb")
		except IOError, e:
			print "Unable to open file '%s' for writing" % output_file_name
			sys.exit(0)
	
	for builder in builders:
		if builder.__name__.lower() == input_builder_name.lower():
			input_builder = builder()
		if builder.__name__.lower() == output_builder_name.lower():
			output_builder = builder()
	
	asv = ASV()
	asv.input(input_file.read(-1), input_builder)
	output_file.write(asv.output(output_builder))
