import Image


w = int(raw_input("How many images are in this series?: "))
for slices in range(w):
	im = Image.open("Shape%i.tif" % )

L = im.mode

C = []

dim = im.size

xsize = dim[0]
ysize = dim[1]

pts = int(raw_input("How many points would you like to sample on each side?: "))


s = (xsize/pts)

for x in range(pts):
	for y in range(ysize):
		val = im.getpixel(((x*s),y))
		if val < 10:
			C.append(((x*s),y))
			break
for x in range(pts):
	for y in range(ysize):
		val = im.getpixel(((x*s),ysize-1-y))
		if val < 10:
			C.append(((x*s),ysize-1-y))
			break	
for y in range(pts):
	for x in range(xsize):
		val = im.getpixel((x,(y*s)))
		if val < 10:
			C.append((x,(y*s)))
			break	
for y in range(pts):
	for x in range(xsize):
		val = im.getpixel((xsize-1-x,(y*s)))
		if val < 10:
			C.append((xsize-1-x,(y*s)))
			break				
print C

im = Image.new(L, (xsize, ysize), 255)
for i in range(len(C)):
	im.putpixel(C.pop(), 0)
for slices in range(w):
	im.save("Newshape%i.tif" % slices)