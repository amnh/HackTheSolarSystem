pro IDL_stack_test
; M.Greenberg Adapted from:
; D.S.Ebel
; Requires getting the right threshold values from ImageJ.
; TO DO: Modify for 16-bit tiff stacks.
; Modify to input the volume file directly


slices = 119
xres = 2048
yres = 2048       							;number of images in stack
arr = readtiffstack('D:\MGreenberg\flight_20x_map_rotated.tif')
tot =0L
grandTot = 0L										; set variables / initial zero counters for total pxls (set LONG ???)
totArr = intarr(xres)
Index = indgen(2048)

												; 0.0242 cubic microns/voxel element
for i=0, xres-1 DO BEGIN
	for j=0, slices-1 DO BEGIN
		for k=0, yres-1 DO BEGIN
			if (arr[k,i,j] eq 255) then tot=tot+1
		ENDFOR
	ENDFOR
	grandTot = grandTot+tot
	totArr[i] = tot
	tot = 0
ENDFOR
	print, grandTot
	print, totArr[2040]
	PLOT, Index, totarr, $
   	TITLE='Track Area vs distance', XTITLE='Distance', $
   	YTITLE='Cross-sectional Area', XRANGE=[0,2100]

end
