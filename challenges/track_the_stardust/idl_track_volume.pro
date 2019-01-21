pro IDL_track_volume
; M.Greenberg Adapted from:
; D.S.Ebel
; Requires getting the right threshold values from ImageJ.
; TO DO: Modify for 16-bit tiff stacks.
; Modify to input the volume file directly

;inputted Conditions
slices = 101
xres = 2046
yres = 439
arr = readtiffstack('D:\MGreenberg\Track 82\T82_outlines_Final.tif')

;variables used in code
tot =0L
grandTot = 0L
totArr = fltarr(xres)
Index = indgen(xres)

; 0.0242 cubic microns/voxel element

; 3 For loops: loops over row N on all slices and sums to cross sectional
; area for that slice. Adds that to a grand total of voxel elements
; It clears tot each time through so it is reused.
for i=xres-1, 0, -1 DO BEGIN
	for j=0, slices-1 DO BEGIN
		for k=0, yres-1 DO BEGIN
			if (arr[k,i,j] eq 255) then tot=tot+1
		ENDFOR
	ENDFOR
	grandTot = long(grandTot)+long(tot)
	totArr[i] = grandtot
	tot = 0
ENDFOR

print, grandTot		;Prints grand total of voxels

WINDOW, XSIZE = 2000, YSIZE = 600

;Plots profile of track
PLOT, Index, totarr, $
XRANGE = [0.0,1900.0], $
THICK = 3.5, $
XTHICK =2, $
YTHICK = 2, $
CHARTHICK = 1.5, $
CHARSIZE = 2, $
COLOR = 0, $
BACKGROUND = 16777214
END
