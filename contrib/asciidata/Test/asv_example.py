import ASV
# Create an ASV instance

asv = ASV.ASV()

# Load our test data

asv.input_from_file("example_data.csv", ASV.CSV(), has_field_names = 1)

# Print out the ASV instance (just for fun)

print asv

print

# Iterate over the ASV instance and print out the first cell of each row
#   and the cell from the column named Address even though we're unsure
#   of its exact number - ASV works it out for us

for row in asv:
	print row[0], row["Address"]

print

# Add a row of data; notice our input file has 4 columns yet we're only
#   providing 3 bits of data. ASV will automatically fill in a blank value
#   for the row we haven't specified

asv.append(["Laurie", "01234 567890", "Somerset"])

# Convert the ASV instance to TSV and print it out to screen

print asv.output(ASV.TSV())
