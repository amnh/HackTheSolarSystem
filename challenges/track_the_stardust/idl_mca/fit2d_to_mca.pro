pro fit2d_to_mca, infile, outfile, energy
;+
;
; NAME:
;       FIT2D_TO_MCA
;
; PURPOSE:
;       This procedure converts an output file from FIT2D, which contains
;       data points in (angle, intensity) at a fixed wavelength, to a file
;       in MCA format, with fixed energy spacing at a fixed angle.  The purpose
;       of the procedure is to be able to look at FIT2D output in the MCA
;       program, particularly to use the JCPDS feature to compare known
;       diffraction lines.
;
; CATEGORY:
;       IDL diffraction data analysis.
;
; CALLING SEQUENCE:
;       FIT2D_TO_MCA, Infile, Outfile, Energy
;
; INPUTS:
;       Infile:   The name of the input file, in FIT2D format.
;
;       Outfile:  The name of the output file, in MCA format.
;
;       Energy:   The energy at which the angle-dispersive data were collected
;
; OUTPUTS:
;       None.  Creates a new disk file, Outfile.
;
; RESTRICTIONS:
;       This procedure fixes 2-theta of the "fake" energy-dispersive data at 10
;       degrees.  This simply sets the pseudo-energy scale of the output data.
;
; PROCEDURE:
;       This procedure does the following:
;       - Reads the input file (angle, intensity)
;       - Converts the angle array to an "d" spacing array, using the actual
;         energy at which the input data were collected
;       - Converts this "d" spacing array to an energy array, assuming 10
;         degrees 2-theta for the energy-dispersive diffraction.  This energy
;         array is NOT evenly spaced in energy.
;       - Rebins the data to evenly spaced points in energy, using a cubic
;         spline.
;       - Writes the output to a file using MCA::WRITE_FILE.  It sets the
;         energy calibration coefficients correctly.  It also multiplies the
;         intensities by 100, since the input is floating point, and the output
;         must be an integer.
;
; EXAMPLE:
;       FIT2D_TO_MCA, 'nacl_2.chi', 'nacl_2.mca', 29.1440
;
; MODIFICATION HISTORY:
;       Written by:     Mark Rivers, June 13, 2000
;-

    if (n_params() ne 3) then $
            message, 'Usage: fit2d_to_mca, infile, outfile, energy'

    mca = obj_new('mca')
    openr, /get, lun, infile
    s = ""
    ; Skip 3 lines
    readf, lun, s
    readf, lun, s
    readf, lun, s

    ; Read number of points
    npoints = 0L
    readf, lun, npoints

    temp = fltarr(2, npoints)
    ; Read data
    readf, lun, temp
    free_lun, lun
    theta = reform(temp[0,*])/2.
    intensity = reform(temp[1,*])

    ; Convert from angle-dispersive coordinate system to energy dispersive
    ; Assume 10 degree 2-theta
    ; First convert from angle to D spacing
    d = 12.398/energy/2./sin(theta*!dtor)
    e = 12.398 / (2. * d * sin(10*!dtor/2.))

    ; We now have "e" array, of equivalent energies.  However, we need to spline
    ; this to get equal spaced energies
    efit = e[0] + findgen(npoints)/(npoints-1)*(e[npoints-1]-e[0])
    ifit = spline(e, intensity, efit, .01)

    cal = mca->get_calibration()
    cal.offset = efit[0]
    cal.slope = (efit[npoints-1] - efit[0])/(npoints-1)
    cal.quad = 0.
    mca->set_calibration, cal
    mca->set_data, long(ifit*100.)
    mca->write_file, outfile
end
