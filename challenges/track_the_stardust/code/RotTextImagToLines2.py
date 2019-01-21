maps = (
	('Si'),
	('Zr'),
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
	)


file = open("T164_linesSum.txt" , 'w')	
for files in maps:
	data = open("T164_%s.txt" % files)
	lineNum = 0
	for line in data:
		a = line.split()
		sum = 0.0
		nPts = 0
		counter = 0
		for x in a:
			sum = sum + float(a[counter])
			if float(a[counter]) > 0.001:
				nPts +=1
			counter += 1
		lineNum += 1
		file.write('%.3f, ' %  sum)
	file.write('\n')
file.close
	
data3 = open("T164_Si.txt")
num2 = 0
for line in data3:
	a = line.split()
	sum = 0.0
	nPts = 0
	counter = 0
	for x in a:
		sum = sum + float(a[counter])
		if float(a[counter]) > 0.001:
			nPts +=1
		counter += 1
	lineNum += 1
	file.write('%i, ' %  nPts)
file. write('\n')
data3.close
	
data2 = open("T164_Si.txt")
num = 0
for line in data2:
	num += 1
	file.write('%i, ' % num)
file. write('\n')
data2.close	