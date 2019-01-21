import Image

volume = 0
x_pix = 5
y_pix = 5
s = 669
#im = Image.open("Stack1.tif")


for slices in range(s):
	im = Image.open("T163_XRF_R%i3.tif" % slices)
	print "stack%i.tif" % slices
	for x in range(x_pix):
		for y in range(y_pix):
			val = im.getpixel((x,y))
			print x , y , val
			if val < 100:
				volume = volume + 1

print volume