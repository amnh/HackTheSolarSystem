# This is the completed, working outlining code.

import Image
frames = int(raw_input("How many frames are in the series?: "))
testval = int(raw_input("Which pixel values do you wish to detect?: "))
s = int(raw_input("Step size?: "))
# This imports functions and asks for the variable parameters.

	
for slices in range(frames):
	if slices >= 100:
		im = Image.open("T152_map_6_%i.tif" % slices)
	if 10 <= slices < 100:
		im = Image.open("T152_map_6_0%i.tif" % slices)
	if slices < 10:
		im = Image.open("T152_map_6_00%i.tif" % slices)
	J = im.mode
	xsize = im.size[0]
	ysize = im.size[1]
	# Opens the the images in the sequence to perform the functions on,
	# and finds the image-specific parameters.

	X = []
	Y = []
	sX = []
	sY = []
	# Creates coordinate lists.

	xpts = int(xsize/s)
	ypts = int(ysize/s)
	# The number of points to be sampled in X and Y directions.

	for x in range(xpts): # This finds the border of the shape and adds its
                              # coordinates to the appropriate lists.
		for y in range(ysize):
			val = im.getpixel(((x*s),y))
			if val > testval:
				X.append(x*s)
				sX.append(x*s)
				Y.append(y)
				sY.append(y)
				break
	for x in range(xpts):
		for y in range(ysize):
			val = im.getpixel(((x*s),ysize-1-y))
			if val > testval:
				X.append(x*s)
				sX.append(x*s)
				Y.append(ysize-1-y)
				sY.append(ysize-1-y)
				break	
	test = 0
	t = []
	for y in range(ypts):
		for x in range(xsize):
			val = im.getpixel((x,(y*s)))
			if val > testval:
				X.append(x)
				sX.append(x)
				Y.append(y*s)
				sY.append(y*s)
				if test == 0:
					t.append((x, y*s))
					test = 1
				break	
	for y in range(ypts):
		for x in range(xsize):
			val = im.getpixel((xsize-1-x,(y*s)))
			if val > testval:
				X.append(xsize-1-x)
				sX.append(xsize-1-x)
				Y.append(y*s)
				sY.append(y*s)
				break				


	sX.sort()
	sY.sort()
	# Sorts two lists so that minimum and maximum values can be extracted

	im = Image.new("L", (xsize, ysize), 255)

	def drawline((x1, y1), (x2, y2)): # The drawline function from line8.
		im.putpixel((x1, y1), 0)
		im.putpixel((x2, y2), 0)
		mid = ((x1+(x2-x1)/2), (y1+(y2-y1)/2))
		im.putpixel(mid, 0)
		if y2-y1 > x2-x1:
			for i in range((y2-y1)/2):
				return drawline(mid, (x2,y2)), drawline((x1, y1), mid)
		if x2-x1 > y2-y1:
			for i in range((x2-x1)/2):
				return drawline(mid, (x2,y2)), drawline((x1, y1), mid)
		if x2-x1 == y2-y1:
			for i in range(x2-x1):
				im.putpixel((x1+i, y1+i), 0)

	def BinarySearch(list, value, low, high): # The standard BinarySearch
		mid = low + ((high - low) / 2)
		if mid > value:
			return BinarySearch(list, value, low, mid-1)
		if mid < value:
			return BinarySearch(list, value, mid+1, high)
		if mid == value and mid in(list):
			return list.index(mid)
		else:
			pass
	yes = 0
	if len(sX) > 0:
        # Sets the minimum and maximum X and Y vals using the sorted lists
		minx = sX.pop(0)
		maxx = sX.pop()
		miny = sY.pop(0)
		maxy = sY.pop()
		yes = 1
	check = 0
	x2, y2 = x, y
	def connect((x, y), xlist, ylist): # The standard connect function from line8.
		global minx, maxx, miny, maxy, check, x2, y2
		im.putpixel((x, y), 0)
		if y < maxy and check == 0 or check == 2:
			for i in range(1, maxy):
				index = BinarySearch(ylist, y+i, miny, maxy)
				if index >= 0:
					x2 = xlist.pop(index)
					y2 = ylist.pop(index)
					d = ((x2-x)*(x2-x)+(y2-y)*(y2-y))/(((x2-x)*(x2-x)+(y2-y)*(y2-y))*((x2-x)*(x2-x)+(y2-y)*(y2-y)))
					if d < 100:
						drawline((x, y), (x2, y2))
						break
					else:
						return connect((x2, y2), xlist, ylist)
					
					
		if y == maxy:		
			check = 1
		if y > miny and check == 1:
			for i in range(1, (maxy - miny)):
				index = BinarySearch(ylist, y-i, miny, maxy)
				if index >= 0:
					x2 = xlist.pop(index)
					y2 = ylist.pop(index)
					d = ((x2-x)*(x2-x)+(y2-y)*(y2-y))/(((x2-x)*(x2-x)+(y2-y)*(y2-y))*((x2-x)*(x2-x)+(y2-y)*(y2-y)))
					if d < 100:
						drawline((x, y), (x2, y2))
						break
					else:
						return connect((x2, y2), xlist, ylist)
		if y == miny:
			check = 2
		try:
			return connect((x2, y2), xlist, ylist)
		except RuntimeError:
			pass
			
	if yes == 1 and len(t) > 0:		# Performs connect and fills the outline if there are pts to be connected and filled.
		connect(t.pop(), X, Y)
		Rec = []
		Rec2 = []
		for x in range(xsize):
			for y in range(ysize):
				val1 = im.getpixel((x, y))
				if val1 < 20:
					Rec.append(x)
					Rec.append(y)
					break
			for y in range(ysize):		
				val2 = im.getpixel((x, ysize-1-y))
				if val2 < 20:
					Rec2.append(x)
					Rec2.append(ysize-1-y)
					break
		if len(Rec) > 1:
			for i in range(len(Rec)/2):
				x1 = Rec.pop(0)
				y1 = Rec.pop(0)
				x2 = Rec2.pop(0)
				y2 = Rec2.pop(0)
				for i in range(y2-y1):
					im.putpixel((x1, y1+i), 0)
					
	im.save("T152_map_6_outlined_%i.tif" % slices) # Saves as a sequence
	print "%i" % slices # Reports progress
