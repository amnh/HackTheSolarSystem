import Image

#s is number of vertical slices
s = 669
elements = (
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
file = open("T164_linesSum.txt" , 'w')
file.write('T164 Line Sums\n')

for items in elements:
	file.write('%s, ' % items)
file.write('\n')

for slices in range(s):
	frame = Image.open("T164_XRF_R%03i.tif" % slices)
	for y in range(15):
		#print y
		count = 0
		tot = 0
		for x in range(296):
			#print x
			pixV = frame.getpixel((x,y))
			if pixV != 0:
				tot = tot +pixV
				count = count + 1
		file.write('%i, ' % tot)
	file.write('%i, \n' % count)
