# List maps to use  for each
maps = (
	('Si'),
	('Cl'),
	('Ca'),
	('Ti'),
	('Cr'),
	('Mn'),
	('Fe'),
	('Ni'),
	('Cu'),
	('Zn'),
	('Br'),
	('Kr'),
	('Rb'),
	('Sr'),
	('Zr'),
	)

print '\n'
#define image width
width = 500
#define header height
header = 4

for files in maps:
	TextImage = open("T164_%s.txt" % files, 'w')
	counter = 0
	data = open("t164_mapRot-1_001_%s Ka.dat" % files)
	for line in data:
		#Get rid of all the lines in the header
		counter = counter + 1
		if counter < header + 1:
			print counter
		#once outside the header process the data and arrange to textimage standards
		if counter > header:
			a = line.split()
			TextImage.write('%s ' % a[2])
			if (counter - header) % width == 0:
				TextImage.write('\n')


