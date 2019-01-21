function extract_spectra_scans, roi_template_file, nx, ny, first_file, rois, $
         background_width=background_width
;+
; NAME:
;	EXTRACT_SPECTRA_SCANS
;
; PURPOSE:
;	This function extracts the net counts in regions of interest from spectra scan
;   files.
;
; CATEGORY:
;	IDL MCA utilities
;
; CALLING SEQUENCE:
;
;	Result = EXTRACT_SPECTRA_SCANS(Roi_template_file, Nx, Ny, First_file, Rois, $
;                                  Background_width=Background_width)
;
; INPUTS:
;	Roi_template_file:
; 	 	The name of an MCA file that contains the ROI definitions to be
;       used when extracting the data. This is typically one of the spectra files that
;       has been mofified using MCA_DISPLAY to define the ROIs to be extracted, and
;       then perhaps saved under a different name.
;  Nx:
;       The number of points in the fast scan direction.
;  Ny:
;       The number of points in the slow scan direction.  May be 1 for a 1-D scan.
;  First_file:
;       The full name of the first file in the scan.  Subsequent files are assumed to be
;       constructed from this file name with "increment_filename".
;
; KEYWORD PARAMETERS:
;	BACKGROUND_WIDTH:
;       The number of points of background on each side of the ROIs to be used in
;       calculating	the net counts in each ROI.
;
; OUTPUTS:
;	This function returns a floating point array [Nx, Ny, Nrois] containing the net
;   counts in each ROI at each spectrum in the scan. Nrois is the  number of ROIs
;   defined in the Roi_template_file.
;
; OPTIONAL OUTPUTS:
;   Rois:
;       An array of structures of type MCA_ROI[Nrois].  These structures can be used,
;       for example to get the labels to put on plots of the
;
; EXAMPLE:
;   IDL> data = extract_spectra_scans('roi_template.dat', 33, 1, '501.49', back=1, rois)
;   IDL> plot, data[*,0,4], psym=-1, title=rois[4].label
;
; MODIFICATION HISTORY:
; 	Written by:	Mark Rivers, June 29, 2001
;-


   mca = obj_new('mca')

   ; Read the template file to get the ROI definitions
   mca->read_file, roi_template_file
   rois = mca->get_rois(roi_info)
   nrois = roi_info.nrois
   data = fltarr(nx, ny, nrois)
   file = first_file

   for i=0, ny-1 do begin
      for j=0, nx-1 do begin
         mca->read_file, file
         mca->set_rois, rois
         mca->get_roi_counts, total, net, background_width=background_width
         for k=0, nrois-1 do data[j, i, k] = net[k]
         file = increment_filename(file)
      endfor
   endfor
   return, data
end