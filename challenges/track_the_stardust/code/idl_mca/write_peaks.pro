pro write_peaks, peaks, file, background
;+
; NAME:
;       WRITE_PEAKS
;
; PURPOSE:
;       This procedure writes an array of structures of type
;       {MCA_PEAK} to a disk file.  If the Background parameter is present is also
;       writes the background structure to the file.
;
; CATEGORY:
;       MCA data analysis
;
; CALLING SEQUENCE:
;       WRITE_PEAKS, Peaks, File, Background
;
; INPUTS:
;       Peaks:
;           An array of structures of type {MCA_PEAK}.
;       File:
;           The name of a disk file containing the peak definitions.
;
; OPTIONAL INPUTS:
;       Background:
;           A structure of type {MCA_BACKGROUND}.
;
; EXAMPLE:
;       Peaks = READ_PEAKS('my_peaks.pks')
;       Peaks[1].initial_energy = 6.4
;       WRITE_PEAKS, Peaks, 'mypeaks.pks'
;
; MODIFICATION HISTORY:
;       Written by:     Mark Rivers, November 1, 1998
;       9-Jan-2001, MLR, Added background parameter for saving background structure
;-

    openw, lun, file, /get
    if (n_elements(background) ne 0) then begin
        printf, lun, 'Background_exponent,', background.exponent
        printf, lun, 'Background_top_width,', background.top_width
        printf, lun, 'Background_bottom_width,', background.bottom_width
        printf, lun, 'Background_tangent,', background.tangent
        printf, lun, 'Background_compress,', background.compress
    endif
    for i=0, n_elements(peaks)-1 do begin
        printf, lun, strtrim(peaks[i].initial_energy, 2) + ', ' + $
                     strtrim(peaks[i].energy_flag, 2)    + ', ' + $
                     strtrim(peaks[i].initial_fwhm, 2)   + ', ' + $
                     strtrim(peaks[i].fwhm_flag, 2)      + ', ' + $
                     strtrim(peaks[i].ampl_factor, 2)    + ', ' + $
                     strtrim(peaks[i].label, 2)
    endfor
    free_lun, lun
end
