;*****************************************************************************
;+
; NAME:
; Release notes
;   Version 3:
;       Everything prior to 5/16/99 when I started making release notes!
;       Prior to this date there are release notes for each routine but no
;       global release notes for the MCA class, and there are no
;       version numbers
;  Version 4.1
;       May 16, 1999  Mark Rivers
;           Added .read_time field to MCA_ELAPSED
;  Version 4.2
;       Dec. 2, 1999 Mark Rivers
;           Added .dwell, .channel_advance, .prescale to MCA_PRESETS
;       Dec. 3, 1999 Mark Rivers
;           Fixed WRITE_FILE so environment information is retrieved for
;           EPICS_MED.
;       Feb. 10, 2000 Mark Rivers
;           Added WRITE_STANDARD_FILE and READ_STANDARD_FILE, made
;           WRITE_FILE and READ_FILE call these routines.  This makes it easy
;           to replace WRITE_FILE and READ_FILE with site-specific routines
;           but also retain the standard routines.
;       Feb. 20, 2000 Mark Rivers
;           Fixed bug in ENERGY_TO_CHAN which caused it to use the wrong root
;           of the quadratic equation if the quadratic term of the energy
;           calibration was positive.
;           Modified WRITE_FILE to use exponential notation for energy
;           calibration coefficients to preserve precision.
;       Jan. 4, 2001 Mark Rivers
;           Added spreadsheet compatible output to fit_peaks_report.
;       Jan. 4, 2001 Mark Rivers
;           Modified spreadsheet output slightly.
;       Jan. 18, 2001 Mark Rivers
;           Added energy and FWHM to spreadsheet output
;       Feb. 01, 2001 Mark Rivers
;           Changed fit_peaks.pro (not an mca method, but called by mca::fit_peaks)
;           to make it so that if a peak amplitude is constrained then the area
;           rather than the amplitude is actually what is constrained to the specified
;           value.
;       Mar. 20, 2001 Mark Rivers
;           Increased MAX_CHANS from 4096 to 8192 to handle X17 multiplexor (4x2048) data.
;       June 29, 2001 Mark Rivers
;           Added method mca::get_roi_counts to retrieve total and net counts
;           in each ROI.  Added utility routine extract_spectra_scans.pro.
;       July 14, 2001 Mark Rivers
;           Minor documentation fix for mca::get_environment().
;       July 21, 2001 Mark Rivers
;           Increased MAX_ROIS from 10 to 32 to take advantage of new EPICS
;           record limit.
;       Sept. 21, 2001 Mark Rivers
;           Added GET_NAME and SET_NAME methods.
;       Sept. 29, 2001 Mark Rivers
;          Added support for netCDF file format.
;          Renamed MCA::WRITE_FILE to MCA::WRITE_ASCII_FILE, created
;          MCA::WRITE_NETCDF_FILE, new version of MCA::WRITE_FILE that calls
;          one or the other of these.
;          Renamed MCA_READ_FILE to MCA_READ_ASCII_FILE, created
;          MCA_READ_NETCDF_FILE, new version of MCA_READ_FILE that calls
;          one or the other of these.
;          Took the EPICS specific code to get environment out of the
;          WRITE_FILE routines, added call to GET_ENVIRONMENT()
;      August 28, 2002 Mark Rivers
;          Fixed bug in background calculation in FIT_PEAKS_REPORT, hi was
;          one channel to large.
;-
;

function mca::copy
;+
; NAME:
;       MCA::COPY
;
; PURPOSE:
;       This functions returns a copy of the MCA object.
;
; CATEGORY:
;       MCA object library
;
; CALLING SEQUENCE:
;       Result = mca->COPY()
;
; PROCEDURE:
;       This function simply makes a copy of the MCA object and returns it.
;       It would be nice if IDL had a universal way to make a copy of an
;       object, preserving all of the data in that object, but there is
;       currently no such functionality in IDL.
;
; EXAMPLE:
;       mca = obj_new('MCA')
;       mca->read_file, 'mca.001'
;       new = mca->COPY()
;
; MODIFICATION HISTORY:
;       Written by:     Mark Rivers, October 23, 1998
;-
    out = obj_new('mca')
    out->set_calibration, self->get_calibration()
    out->set_elapsed, self->get_elapsed()
    out->set_presets, self->get_presets()
    out->set_rois, self->get_rois()
    out->set_data, self->get_data()
    return, out
end


pro mca::plot, data, _EXTRA=extra
;+
; NAME:
;       MCA::PLOT
;
; PURPOSE:
;       This procedure provides a quick and convenient way to plot an
;       MCA spectrum.
;
; CATEGORY:
;       MCA object library.
;
; CALLING SEQUENCE:
;       mca->PLOT, Data
;
; OPTIONAL INPUTS:
;       Data: The data to be plotted.  The default is the entire spectrum in
;       the MCA object, i.e. data = mca->GET_DATA().
;
; KEYWORD PARAMETERS:
;       All keywords accepted by the IDL "PLOT" procedure.  Such keywords
;       are passed to PLOT via the _EXTRA mechanism.
;
; PROCEDURE:
;       This routine simply does the following:
;           1) Sets Data = self->GET_DATA if Data is not defined
;           2) Sets Energy = self->chan_to_energy(findgen(n_elements(Data)))
;           3) Executes PLOT, Energy, Data, _EXTRA=extra.
;
; EXAMPLE:
;       mca = obj_new('MCA')
;       mca->read_file, 'mca.001'
;       mca->PLOT
;
; MODIFICATION HISTORY:
;       Written by:     Mark Rivers, October 23, 1998
;-
    if (n_elements(data) eq 0) then data = self->get_data()
    energy = self->chan_to_energy(findgen(n_elements(data)))
    plot, energy, data, _EXTRA=extra
end


pro mca::oplot, data, _EXTRA=extra
;+
; NAME:
;       MCA::OPLOT
;
; PURPOSE:
;       This procedure provides a quick and convenient way to overplot an
;       MCA spectrum.
;
; CATEGORY:
;       MCA object library.
;
; CALLING SEQUENCE:
;       mca->OPLOT, Data
;
; OPTIONAL INPUTS:
;       Data: The data to be oplotted.  The default is the entire spectrum in
;       the MCA object, i.e. data = mca->GET_DATA().
;
; KEYWORD PARAMETERS:
;       All keywords accepted by the IDL "OPLOT" procedure.  Such keywords
;       are passed to OPLOT via the _EXTRA mechanism.
;
; PROCEDURE:
;       This routine simply does the following:
;           1) Sets Data = self->GET_DATA if Data is not defined
;           2) Sets Energy = self->chan_to_energy(findgen(n_elements(Data)))
;           3) Executes OPLOT, Energy, Data, _EXTRA=extra.
;
; EXAMPLE:
;       mca = obj_new('MCA')
;       mca->read_file, 'mca.001'
;       mca->PLOT
;       mca->read_file, 'mca.002'
;       mca->OPLOT
;
; MODIFICATION HISTORY:
;       Written by:     Mark Rivers, October 23, 1998
;-
    if (n_elements(data) eq 0) then data = self->get_data()
    energy = self->chan_to_energy(findgen(n_elements(data)))
    oplot, energy, data, _EXTRA=extra
end


function mca::get_energy
;+
; NAME:
;       MCA::GET_ENERGY
;
; PURPOSE:
;       This function returns an array containing the energy of each channel in
;       the MCA spectrum.
;
; CATEGORY:
;       MCA object library.
;
; CALLING SEQUENCE:
;       Result = mca->GET_ENERGY()
;
; PROCEDURE:
;       This routine simply returns mca->chan_to_energy(indgen(self.nchans))
;
; EXAMPLE:
;       mca = obj_new('MCA')
;       mca->read_file, 'mca.001'
;       energy = mca->GET_ENERGY()
;
; MODIFICATION HISTORY:
;       Written by:     Mark Rivers, October 23, 1998
;-
    return, self->chan_to_energy(indgen(self.nchans))
end


function mca::fit_background, _EXTRA=extra
;+
; NAME:
;       MCA::FIT_BACKGROUND
;
; PURPOSE:
;       This function fits the background to the MCA spectrum.
;
; CATEGORY:
;       MCA object library.
;
; CALLING SEQUENCE:
;       Result = mca->FIT_BACKGROUND()
;
; KEYWORD PARAMETERS:
;       All keywords accepted by the <A HREF="mca_utility_routines.html#FIT_BACKGROUND">FIT_BACKGROUND</A> function.
;
; OUTPUTS:
;       This function returns an MCA object which is identical to the calling
;       object, except that the data have been replaced by the background fit.
;
; PROCEDURE:
;       This routine provides a convenient interface to the
;       <A HREF="mca_utility_routines.html#FIT_BACKGROUND">FIT_BACKGROUND</A> function.
;
;       The function extracts the data and calibration slope from the input
;       MCA object, and passes them to FIT_BACKGROUND().  It creates a new
;       MCA object using MCA::COPY() and stores the output of FIT_BACKGROUND()
;       in this new object with MCA::SET_DATA.  It then returns this new MCA
;       object as the function return value.
;
; EXAMPLE:
;       mca = obj_new('MCA')
;       mca->read_file, 'mca.001'
;       bgd = mca->FIT_BACKGROUND(bottom=6, exponent=4)
;       mca->plot
;       bgd->oplot
;
; MODIFICATION HISTORY:
;       Written by:     Mark Rivers, October 23, 1998
;-
    data = self->get_data()
    cal = self->get_calibration()
    slope = cal.slope
;;    print, '<<  MCA ->  FIT BACKGROUND'
    back = fit_background(data, slope, _EXTRA=extra)
;;    print, '  FIT BACKGROUND >>'
    bgd = self->copy()
    bgd->set_data, back
    return, bgd
end


function mca::fit_peaks, peaks, fit=fit, background=background, $
                         output=output, spreadsheet=spreadsheet, $
                         append=append, _EXTRA=extra
;+
; NAME:
;       MCA::FIT_PEAKS
;
; PURPOSE:
;       This function fits the peaks in the MCA spectrum. It provides a
;       convenient interface to the <A HREF="mca_utility_routines.html#FIT_PEAKS">FIT_PEAKS</A> function.
;
; CATEGORY:
;       MCA object library.
;
; CALLING SEQUENCE:
;       Result = mca->FIT_PEAKS(Peaks)
;
; INPUTS:
;       Peaks:  An array of structures of type {MCA_PEAKS}.  See <A HREF="mca_utility_routines.html#FIT_PEAKS">FIT_PEAKS</A> and <A HREF="mca_utility_routines.html#READ_PEAKS">READ_PEAKS</A>
;               for more information.
;
; KEYWORD PARAMETERS:
;       FIT:
;           A structure of type {MCA_FIT} which can be used to control the
;           peak fitting.  If this keyword is omitted then the Fit structure
;           is created with mca->FIT_INITIALIZE()
;       BACKGROUND:
;           An MCA object containing the fitted background.  If this keyword
;           is omitted then this function will call mca->FIT_BACKGROUND before
;           calling FIT_PEAKS.
;       OUTPUT:
;           The name of an output file to receive the ASCII printout of the
;           fit results.  This keyword is simply passed to <A HREF="#MCA::FIT_PEAKS_REPORT">MCA::FIT_PEAKS_REPORT</A>.
;       SPREADSHEET:
;           The name of an output file to receive the ASCII output of the
;           fit results in spreadsheet format.  This keyword is simply passed to <A HREF="#MCA::FIT_PEAKS_REPORT">MCA::FIT_PEAKS_REPORT</A>.
;       APPEND:
;           Flag indicating whether the output and spreadsheet files should be
;           appended to or overwritten.  This keyword is simply passed to <A HREF="#MCA::FIT_PEAKS_REPORT">MCA::FIT_PEAKS_REPORT</A>.
;
;       In addition to these keywords, all keywords accepted by the <A HREF="mca_utility_routines.html#FIT_BACKGROUND">FIT_BACKGROUND</A>
;       function are accepted if the Background keyword is not present, i.e.
;       if this function will be calling FIT_BACKGROUND().
;
; OUTPUTS:
;       This function returns an MCA object which is identical to the calling
;       object, except that the data have been replaced by the peak fit.
;
; PROCEDURE:
;       The function does the following:
;           - Creates the Fit structure with mca->FIT_INITIALIZE() if Fit
;             was not passed as a keyword parameter.
;           - Fits the background using MCA::FIT_BACKGROUND if Background
;             was not passed as a keyword parameter.
;           - Extracts the data from the input spectrum and the background
;             spectrum.
;           - Calls the <A HREF="mca_utility_routines.html#FIT_PEAKS">FIT_PEAKS</A>
;             function with the background subtracted data.
;           - Calls <A HREF="#MCA::FIT_PEAKS_REPORT">MCA::FIT_PEAKS_REPORT</A>
;           - Creates a new MCA object using MCA::COPY() and stores the output
;             of FIT_PEAKS() in this new object with MCA::SET_DATA.  It then
;             returns this new MCA object as the function return value.
;
; EXAMPLE:
;       mca = obj_new('MCA')
;       mca->read_file, 'mca.001'
;       peaks = read_peaks('mypeaks.pks')
;       fit = mca->FIT_PEAKS(peaks, bottom=6, exponent=4)
;       mca->plot
;       fit->oplot
;
; MODIFICATION HISTORY:
;       Written by:     Mark Rivers, October 23, 1998
;       Nov. 1, 1998.  Mark Rivers. Added APPEND keyword
;       Jan. 4, 2001.  Mark Rivers. Added SPREADHSHEET keyword.
;-

;   If fit is not a structure of type {MCA_FIT} then initialize it
    tname = size(fit, /tname)
    if (tname ne 'STRUCT') then begin
        fit = self->fit_initialize()
    endif else begin
        struct = tag_names(fit, /structure_name)
        if (struct ne 'MCA_FIT') then begin
            fit = self->fit_initialize()
        endif
    endelse


;   If background is not an object of type MCA then initialize it
    if (not obj_valid(background)) then begin
        background = self->fit_background(_EXTRA=extra)
    endif else if (not obj_isa(background, 'mca')) then begin
        background = self->fit_background(_EXTRA=extra)
    endif

    fit.npeaks = n_elements(peaks)
    observed_counts = self->get_data()
    background_counts = background->get_data()
    fit_counts = fit_peaks(fit, observed_counts - background_counts, peaks)
    fit_counts = fit_counts + background_counts
    self->fit_peaks_report, fit, peaks, background, output=output, $
                            spreadsheet=spreadsheet, append=append
    fit_mca = self->copy()
    fit_mca->set_data, fit_counts
    return, fit_mca
end


pro mca::fit_peaks_report, fit, peaks, background, $
                           output=output, spreadsheet=spreadsheet, append=append
;+
; NAME:
;       MCA::FIT_PEAKS_REPORT
;
; PURPOSE:
;       This procedure prints out the results from <A HREF="mca_utility_routines.html#FIT_PEAKS">FIT_PEAKS</A>
;
; CATEGORY:
;       MCA object library.
;
; CALLING SEQUENCE:
;       mca->FIT_PEAKS_REPORT, Fit, Peaks, Background
;
; INPUTS:
;       Fit:  A structure of type {MCA_FIT}.
;
;       Peaks:  An array of structures of type {MCA_PEAK}.
;
;       (See <A HREF="mca_utility_routines.html#FIT_PEAKS">FIT_PEAKS</A> for more information on Fit and Peaks)
;
;       Background:  An MCA object containing the fitted background spectrum.
;
; KEYWORD PARAMETERS:
;       OUTPUT:
;           The name of an output file to receive the ASCII printout of the
;           fit results.  If this keyword is omitted then the output will be
;           written to stdout, i.e. the IDL output window.  If the Output file
;           already exists then the new information will (by default) be appended
;           to the file.
;
;       SPREADSHEET:
;           The name of an output file to receive the ASCII output of the
;           fit results in a format easily imported into a spreadsheet.  If this
;           keyword is omitted then no spreadsheet output will be generated.
;           written to stdout, i.e. the IDL output window.
;           If the spreadhseet file already exists then the new information will
;           (by default) be appended to the file.
;
;       APPEND:
;           Set this keyword to 0 to overwrite the output and spreadsheet files
;           rather than to append to them, which is the default behavior.
;
; EXAMPLE:
;       mca = obj_new('MCA')
;       mca->read_file, 'mca.001'
;       peaks = read_peaks('mypeaks.pks')
;       fit = mca->fit_peaks(peaks, fit=fit, background=background, $
;                            bottom=6, exponent=4)
;       mca->FIT_PEAKS_REPORT, fit, peaks, background, output='mca.001_out'
;
; MODIFICATION HISTORY:
;       Written by:     Mark Rivers, October 23, 1998
;       Nov. 1, 1998.  Mark Rivers. Added APPEND keyword
;       Nov. 17, 1998  Mark Rivers. Added error string, initial energy.
;       Jan. 4, 2001   Mark Rivers. Added spreadsheet file output.
;       Jan. 9, 2001   Mark Rivers. Modified spreadsheet output slightly
;       Jan. 18, 2001  Mark Rivers. Added energy and FWHM to spreadsheet output
;-

    if (n_elements(append) eq 0) then append=1
    if (n_elements(output) ne 0) then begin
        openw, out_lun, output, /get, append=append
    endif else begin
        out_lun = -1
    endelse
    if (n_elements(spreadsheet) ne 0) then begin
        openw, spread_lun, spreadsheet, /get, append=append
    endif else begin
        spread_lun = 0
    endelse

    SIGMA_TO_FWHM = 2.*sqrt(2.*alog(2.))

    ; Compute backgrounds
    background_counts = background->get_data()
    for i=0, fit.npeaks-1 do begin
        low = background->energy_to_chan((peaks(i).energy - $
                2.*peaks(i).fwhm / SIGMA_TO_FWHM))
        low = (low > 0) < (self.nchans-3)
        hi  = background->energy_to_chan(peaks(i).energy + $
                2.*peaks(i).fwhm / SIGMA_TO_FWHM)
        hi = (hi > (low+1)) < (self.nchans-1)
        peaks(i).bgd = total(background_counts(low:hi))
    endfor

    printf, out_lun
    printf, out_lun, '*******************************************************'
    printf, out_lun, '    Fit of ', self.name
    printf, out_lun
    elapsed = self->get_elapsed()
    printf, out_lun, format='("Real time (seconds):            ", f10.2)', $
                 elapsed.real_time
    printf, out_lun, format='("Live time (seconds):            ", f10.2)', $
                 elapsed.live_time
    printf, out_lun, format='("Initial FWHM offset, slope:     ", 2f10.6)', $
                 fit.initial_fwhm_offset, fit.initial_fwhm_slope
    printf, out_lun, format='("Optimized FWHM offset, slope:   ", 2f10.6)', $
                 fit.fwhm_offset, fit.fwhm_slope
    printf, out_lun, format='("Initial energy offset, slope:   ", 2f10.6)', $
                 fit.initial_energy_offset, fit.initial_energy_slope
    printf, out_lun, format='("Optimized energy offset, slope: ", 2f10.6)', $
                fit.energy_offset, fit.energy_slope
    printf, out_lun, format='("# Iterations, function evals:   ", i10, i10)', $
                 fit.n_iter, fit.n_eval
    printf, out_lun, format='("Chi squared:                    ", e14.6)', $
                 fit.chisqr
    printf, out_lun, format='("Status code:                    ", i3)', $
                 fit.status
    printf, out_lun, format='("Error message:                  ", a)', $
                 fit.err_string
    printf, out_lun
    format = '(9x,"Peak",7x,"Energy",4x,"FWHM",6x,"Area",5x,' + $
             '"Background",3x,"Area/MDL",3x,"Area/Bkg")'
    printf, out_lun, format=format
    printf, out_lun

    for i=0, fit.npeaks-1 do begin
        if (peaks(i).energy_flag eq 0) then esym=' ' else esym='*'
        if (peaks(i).fwhm_flag eq 0) then fsym=' ' else fsym='*'
        if (peaks(i).ampl_factor eq 0.) then asym=' ' else asym='*'
        printf, out_lun, format='(a15,f10.3,a,f10.4,a,f10.1,a,f10.1,f10.1,f10.1)', $
            peaks(i).label, $
            peaks(i).energy, esym, $
            peaks(i).fwhm, fsym,$
            peaks(i).area, asym,$
            peaks(i).bgd, $
            peaks(i).area/((3*sqrt(peaks(i).bgd)) > 1.0),$
            peaks(i).area/(peaks(i).bgd > 1.0)
    endfor

    if (out_lun gt 0) then free_lun, out_lun

    if (spread_lun gt 0) then begin
        printf, spread_lun, format='(a,a,$)', self.name, $
                       '#Labels#Live time#Real time#'
        for i=0, fit.npeaks-1 do begin
            printf, spread_lun, format='(a,$)', peaks(i).label + '#'
        endfor
        printf, spread_lun
        printf, spread_lun, format='(a,a,f10.3,a,f10.3,a,$)', self.name, $
                '#Energy#', elapsed.live_time, '#', elapsed.real_time, '#'
        for i=0, fit.npeaks-1 do begin
            printf, spread_lun, format='(f10.3,a,$)', peaks(i).energy, '#'
        endfor
        printf, spread_lun
        printf, spread_lun, format='(a,a,f10.3,a,f10.3,a,$)', self.name, $
                '#FWHM#', elapsed.live_time, '#', elapsed.real_time, '#'
        for i=0, fit.npeaks-1 do begin
            printf, spread_lun, format='(f10.3,a,$)', peaks(i).fwhm, '#'
        endfor
        printf, spread_lun
        printf, spread_lun, format='(a,a,f10.3,a,f10.3,a,$)', self.name, $
                '#Area#', elapsed.live_time, '#', elapsed.real_time, '#'
        for i=0, fit.npeaks-1 do begin
            printf, spread_lun, format='(f10.1,a,$)', peaks(i).area, '#'
        endfor
        printf, spread_lun
        printf, spread_lun, format='(a,a,f10.3,a,f10.3,a,$)', self.name, $
                '#Background#', elapsed.live_time, '#', elapsed.real_time, '#'
        for i=0, fit.npeaks-1 do begin
            printf, spread_lun, format='(f10.1,a,$)', peaks(i).bgd, '#'
        endfor
        printf, spread_lun
        free_lun, spread_lun
    endif

end


function mca::fit_initialize
;+
; NAME:
;       MCA::FIT_INITIALIZE
;
; PURPOSE:
;       This procedure initializes a structure of type {MCA_FIT}.  It is
;       typically called before calling <A HREF="mca_utility_routines.html#FIT_PEAKS">FIT_PEAKS</A>
;
; CATEGORY:
;       MCA object library.
;
; CALLING SEQUENCE:
;       Result = mca->FIT_INTIIALIZE()
;
; OUTPUT:
;       This function returns a structure of type {MCA_FIT} with a number
;       of fields initialized to appropriate values.
;
; PROCEDURE:
;       This function initializes the fields in the {MCA_FIT} structure as
;       follows:
;           .first_chan = 0
;           .last_chan = self.nchans-1
;           .nchans = self.nchans
;           .initial_energy_offset = self.calibration.offset
;           .initial_energy_slope =  self.calibration.slope
;           .energy_flag = 1
;           .fwhm_flag = 1
;           .initial_fwhm_offset = .15  ; 150 eV constant
;           .initial_fwhm_slope = 0.
;           .chi_exp = 0.
;           .max_iter = 20
;           .tolerance = 1.e-4
;           .debug = 0
;
;       These are reasonable default values for fitting x-ray spectra collected
;       with Si(Li) or Ge detectors.
;
; EXAMPLE:
;       mca = obj_new('MCA')
;       mca->read_file, 'mca.001'
;       fit = mca->FIT_INITIALIZE()
;
; MODIFICATION HISTORY:
;       Written by:     Mark Rivers, October 23, 1998
;-
    calibration = self->get_calibration()
    fit = {mca_fit}
    fit.first_chan = 0
    fit.last_chan = self.nchans-1
    fit.nchans = self.nchans
    fit.initial_energy_offset = calibration.offset
    fit.initial_energy_slope = calibration.slope
    fit.energy_flag = 1
    fit.fwhm_flag = 1
    fit.initial_fwhm_offset = .15  ; 150 eV constant
    fit.initial_fwhm_slope = 0.
    fit.chi_exp = 0.
    fit.max_iter = 20
    fit.tolerance = 1.e-4
    fit.debug = 0
    return, fit
end


function mca::background_initialize
;+
; NAME:
;       MCA::BACKGROUND_INITIALIZE
;
; PURPOSE:
;       This procedure initializes a structure of type {MCA_BACKGROUND}.  It is
;       typically called before calling <A HREF="mca_utility_routines.html#FIT_BACKGROUND">FIT_BACKGROUND</A>
;
; CATEGORY:
;       MCA object library.
;
; CALLING SEQUENCE:
;       Result = mca->BACKGROUND_INITIALIZE()
;
; OUTPUT:
;       This function returns a structure of type {MCA_BACKGROUND} with a number
;       of fields initialized to appropriate values.
;
; PROCEDURE:
;       This function initializes the fields in the {MCA_BACKGROUND} structure as
;       follows:
;           .exponent = 2
;           .top_width = 0.
;           .bottom_width = 4.
;           .tangent = 0
;           .compress = 4
;
;       These are reasonable default values for fitting x-ray spectra collected
;       with Si(Li) or Ge detectors.
;
; EXAMPLE:
;       mca = obj_new('MCA')
;       mca->read_file, 'mca.001'
;       fit = mca->BACKGROUND_INITIALIZE()
;
; MODIFICATION HISTORY:
;       Written by:     Mark Rivers, November 1, 1998
;-
    background = {mca_background}
    background.exponent = 2
    background.top_width = 0.
    background.bottom_width = 4.
    background.tangent = 0
    background.compress = 4
    return, background
end


;*****************************************************************************
pro mca::initial_calibration, energy
;+
; NAME:
;       MCA::INITIAL_CALIBRATION
;
; PURPOSE:
;       This procedure performs an initial coarse energy calibration of the
;       MCA, setting only the slope, and setting the offset parameter to 0.
;
; CATEGORY:
;       IDL device class library.
;
; CALLING SEQUENCE:
;
;       Result = mca->INITIAL_CALIBRATION, Energy
;
; INPUTS:
;       Energy: The energy of the biggest peak in the MCA spectrum
;
; PROCEDURE:
;       This routine does the following:
;           1) Sets the offset coefficient to 0.0
;           2) Sets the quadratic coefficient to 0.0
;           3) Determines which channel contains the most counts, PEAK_CHAN
;           4) Sets the slope equal to the input energy divided by PEAK_CHAN
;
; EXAMPLE:
;       mca = obj_new('MCA')
;       mca->read_file, 'mca.001'
;       mca->INITIAL_CALIBRATION, 20.1
;
; MODIFICATION HISTORY:
;       Written by:     Mark Rivers, Sept. 13, 1998
;-
    data = self->get_data()
    peak = max(data, peak_chan)
    calibration = self->get_calibration()
    calibration.offset = 0.
    calibration.slope = float(energy)/(peak_chan > 1)
    calibration.quad = 0.
    self->set_calibration, calibration
end


;*****************************************************************************
pro mca::final_calibration, peaks, _EXTRA=extra
;+
; NAME:
;       MCA::FINAL_CALIBRATION
;
; PURPOSE:
;       This procedure performs a final fine energy calibration of the
;       MCA, setting both the offset and slope parameters.
;
; CATEGORY:
;       IDL device class library.
;
; CALLING SEQUENCE:
;
;       mca->FINAL_CALIBRATION, Peaks
;
; INPUTS:
;       Peaks: The array of MCA_PEAKS structures to be used by MCA::FIT_PEAKS.
;
; OUTPUTS:
;       None:
;
; PROCEDURE:
;       This routine does the following:
;           1) Fits the background
;           2) Fits the peaks, determining the best-fit energy offset and
;              slope.
;           3) Sets the offset and slope of the energy calibration to the
;              values from the fit
;
; EXAMPLE:
;       mca = obj_new('MCA')
;       mca->read_file, 'mca.001'
;       peaks = read_peaks('calib.pks')
;       mca->FINAL_CALIBRATION, peaks
;
; MODIFICATION HISTORY:
;       Written by:     Mark Rivers, Sept. 15, 1998
;-
    predicted = self->fit_peaks(peaks, fit=fit, _EXTRA=extra)
    calibration = self->get_calibration()
    calibration.slope = fit.energy_slope
    calibration.offset = fit.energy_offset
    self->set_calibration, calibration
end


;*****************************************************************************
function mca::get_calibration
;+
; NAME:
;       MCA::GET_CALIBRATION
;
; PURPOSE:
;       This function returns the calibration parameters for the MCA.
;       The calibration information is contained in a structure of type
;       MCA_CALIBRATION.
;
; CATEGORY:
;       IDL device class library.
;
; CALLING SEQUENCE:
;
;       Result = mca->GET_CALIBRATION()
;
; INPUTS:
;       None:
;
; OUTPUTS:
;       This function returns the calibration information for the MCA.
;       The calibration information is contained in a structure of type
;       MCA_CALIBRATION.  This structure is currently defined as follows:
;           {mca_calibration, $
;               offset:     0., $
;               slope:      0., $
;               quad:       0., $
;               units:      '', $
;               two_theta:  0.  $
;           }
;       The calibration equation is:
;           energy = .offset + .slope*channel + .quad*channel^2
;       where channel 0 is the first channel and offset is thus the energy
;       of the first channel.
;
;       The function MCA::CHANNEL_TO_ENERGY or MCA::ENERGY_TO_CHANNEL
;       should always be used rather than implementing the above equation
;       directly in case additional calibration parameters are implemented in
;       the future.
;
;       .units is a string, typically something like "keV".
;       .two_theta is the angle of the detector from the incident beam.  This
;       is typically used in energy-dispersive diffraction.
;
;       NOTE: The exact definition of the MCA_CALIBRATION structure is subject
;       to change.  What will not change is the name and function of the fields
;       described here.  However, additional fields may be added and the order
;       of the fields could change.  Users should not write code which assumes
;       anything about the exact definition of this structure.
;
; EXAMPLE:
;       mca = obj_new('MCA')
;       mca->read_file, 'mca.001'
;       calibration = mca->GET_CALIBRATION()
;
; MODIFICATION HISTORY:
;       Written by:     Mark Rivers, October 1, 1997
;-
    return, self.calibration
end


;*****************************************************************************
pro mca::set_calibration, calibration
;+
; NAME:
;       MCA::SET_CALIBRATION
;
; PURPOSE:
;       This procedure sets the calibration parameters for the MCA.
;       The calibration information is contained in a structure of type
;       MCA_CALIBRATION.
;
; CATEGORY:
;       IDL device class library.
;
; CALLING SEQUENCE:
;
;       mca->SET_CALIBRATION, Calibration
;
; INPUTS:
;       Calibration:  A structure of type MCA_CALIBRATION.  The current
;                     definition of this  structure is described in
;                     <A HREF="#MCA::GET_CALIBRATION">MCA::GET_CALIBRATION()</A>.
;
;       NOTE: The exact definition of the MCA_CALIBRATION structure is subject
;       to change.  What will not change is the name and function of the fields
;       described here.  However, additional fields may be added and the order
;       of the fields could change.  Users should not write code which assumes
;       anything about the exact definition of this structure.
;
; OUTPUTS:
;       None
;
; EXAMPLE:
;       mca = obj_new('MCA')
;       mca->read_file, 'mca.001'
;       calibration = mca->GET_CALIBRATION()
;       calibration.offset = .014
;       mca->SET_CALIBRATION, calibration
;
; MODIFICATION HISTORY:
;       Written by:     Mark Rivers, October 1, 1997
;-
    self.calibration = calibration
end


;*****************************************************************************
function mca::get_name
;+
; NAME:
;       MCA::GET_NAME
;
; PURPOSE:
;       This function returns the name of this MCA object.
;
; CATEGORY:
;       IDL device class library.
;
; CALLING SEQUENCE:
;
;       Result = mca->GET_NAME()
;
; INPUTS:
;       None:
;
; OUTPUTS:
;       This function returns the name of the MCA object.  This is typically either
;       the hardware name of the detector or the name of the disk file that contained
;       the MCA data.
;
; EXAMPLE:
;       mca = obj_new('MCA')
;       mca->read_file, 'mca.001'
;       name = mca->GET_NAME()
;
; MODIFICATION HISTORY:
;       Written by:     Mark Rivers, September 21, 2001
;-
    return, self.name
end


;*****************************************************************************
pro mca::set_name, name
;+
; NAME:
;       MCA::SET_NAME
;
; PURPOSE:
;       This procedure sets the name of the MCA.
;
; CATEGORY:
;       IDL device class library.
;
; CALLING SEQUENCE:
;
;       mca->SET_NAME, Name
;
; INPUTS:
;       Name:  A string giving the descriptive name of the MCA
;
; OUTPUTS:
;       None
;
; EXAMPLE:
;       mca = obj_new('MCA')
;       mca->read_file, 'mca.001'
;       mca->SET_NAME, 'My MCA'
;
; MODIFICATION HISTORY:
;       Written by:     Mark Rivers, September 21, 2001
;-
    self.name = name
end


;*****************************************************************************
pro mca::set_environment, environment
;+
; NAME:
;       MCA::SET_ENVIRONMENT
;
; PURPOSE:
;       This procedure sets the environment parameters for the MCA.
;       The calibration information is contained in an array of structures of
;       type MCA_ENVIRONMENT.
;
; CATEGORY:
;       IDL device class library.
;
; CALLING SEQUENCE:
;
;       mca->SET_ENVIRONMENT, Environment
;
; INPUTS:
;       Environment:  An array of structures of type MCA_ENVIRONMENT.  The current
;                     definition of this  structure is described in
;                     <A HREF="#MCA::GET_ENVIRONMENT">MCA::GET_ENVIRONMENT()</A>.
;
;       NOTE: The exact definition of the MCA_ENVIRONMENT structure is subject
;       to change.  What will not change is the name and function of the fields
;       described here.  However, additional fields may be added and the order
;       of the fields could change.  Users should not write code which assumes
;       anything about the exact definition of this structure.
;
; OUTPUTS:
;       None
;
; EXAMPLE:
;       mca = obj_new('MCA')
;       mca->read_file, 'mca.001'
;       env = mca->get_environment()
;       env[0].description = 'Test'
;       mca->SET_ENVIRONMENT, env
;
; MODIFICATION HISTORY:
;       Written by:     Mark Rivers, October 30, 1998
;-
    if (n_elements(environment) gt 0) then $
        self.environment = ptr_new(environment)
end


;*****************************************************************************
function mca::get_environment, count, name=name, description=description
;+
; NAME:
;       MCA::GET_ENVIRONMENT
;
; PURPOSE:
;       This function gets the environment parameters for the MCA.
;       The environment information is contained in an array of structures
;       of type MCA_ENVIRONMENT.
;
; CATEGORY:
;       IDL device class library.
;
; CALLING SEQUENCE:
;       Result = mca->GET_ENVIRONMENT()
;
; KEYWORD PARAMETERS:
;       NAME :
;           This keyword can be used to define a search string.  Only
;           environment array elements whose .NAME fields contain this
;           string will be returned.  The match is done using STRPOS and
;           the match does not have to be complete, i.e. NAME can be a
;           substring of the .NAME field.
;
;       DESCRIPTION:
;           This keyword can be used to define a search string.  Only
;           environment array elements whose .DESCRIPTION fields contain this
;           string will be returned.  The match is done using STRPOS and
;           the match does not have to be complete, i.e. DESCRIPTION can be a
;           substring of the .DESCRIPTION field.
;
;       One can specify either the NAME or DESCRIPTION keywords, but not both.
;       If neither is specified then the function returns all of the
;       environment array elements.
;
; OUTPUTS:
;       This function returns the environment information for the MCA.
;       The environment information is contained in an array of structures of
;       type MCA_ENVIRONMENT.  This structure is currently defined as follows:
;           {mca_environment, $
;               name:        '', $
;               value:       '', $
;               description: ''  $
;           }
;       NOTE: The exact definition of the MCA_ENVIRONMENT structure is subject
;       to change.  What will not change is the name and function of the fields
;       described here.  However, additional fields may be added and the order
;       of the fields could change.  Users should not write code which assumes
;       anything about the exact definition of this structure.
;
; OPTIONAL OUTPUTS:
;       Count:  The size of the {MCA_ENVIRONMENT} array being returned.  This
;               will be <=0 if there are no environment variables in the MCA
;               or if no entries matched the NAME or DESCRIPTION criteria.
;
; EXAMPLE:
;       mca = obj_new('MCA')
;       mca->read_file, 'mca.001'
;       env = mca->GET_ENVIRONMENT(count)
;       ; Get all environment variables
;       help, /structure, env[0]
;       ; Get all of the sample information
;       env = mca->GET_ENVIRONMENT(DESCRIPTION='Sample', count)
;       for i=0, count-1 do print, env[i].name, '=', env[i].value
;
; MODIFICATION HISTORY:
;       Written by:     Mark Rivers, October 31, 1998
;-
    blank = {MCA_ENVIRONMENT}
    count = 0
    if (not ptr_valid(self.environment)) then return, blank
    env = *self.environment
    if (n_elements(name) ne 0) then begin
        index = where((strpos(env.name, name) ne -1), count)
        if (count gt 0) then return, env[index] else return, blank
    endif
    if (n_elements(description) ne 0) then begin
        index = where((strpos(env.description, description) ne -1), count)
        if (count gt 0) then return, env[index] else return, blank
    endif
    count = n_elements(env)
    return, env
end


;*****************************************************************************
pro mca::set_elapsed, elapsed
;+
; NAME:
;       MCA::SET_ELAPSED
;
; PURPOSE:
;       This procedure sets the elapsed parameters for the MCA.
;       The elapsed information is contained in a structure of type
;       MCA_ELAPSED.
;
; CATEGORY:
;       IDL device class library.
;
; CALLING SEQUENCE:
;       mca->SET_ELAPSED, Elapsed
;
; INPUTS:
;       Elapsed:  A structure of type MCA_ELAPSED.
;
; MODIFICATION HISTORY:
;       Written by:     Mark Rivers, Sept. 25, 1998
;-
    self.elapsed = elapsed
end


;*****************************************************************************
function mca::get_elapsed
;+
; NAME:
;       MCA::GET_ELAPSED
;
; PURPOSE:
;       This function returns the elapsed parameters for the MCA.
;       The elapsed information is contained in a structure of type
;       MCA_ELAPSED.
;
; CATEGORY:
;       IDL device class library.
;
; CALLING SEQUENCE:
;
;       Result = mca->GET_ELAPSED()
;
; INPUTS:
;       None:
;
; OUTPUTS:
;       This function returns the elapsed information for the MCA.
;       The elapsed information is contained in a structure of type
;       MCA_ELAPSED.  This structure is currently defined as follows:
;           {mca_elapsed,  $
;               start_time:     '', $
;               live_time:      0., $
;               real_time:      0., $
;               read_time:      0.D0, $
;               total_counts:   0.  $
;           }
;
;       .start_time is a string, containing the date and time when acquisition
;                   started
;       .live_time is the elapsed live time in seconds.
;       .real_time is the elapsed real time in seconds.
;       .total_counts is the total counts between the preset start and end
;                   channels
;
;       NOTE: The exact definition of the MCA_ELAPSED structure is subject
;       to change.  What will not change is the name and function of the fields
;       described here.  However, additional fields may be added and the order
;       of the fields could change.  Users should not write code which assumes
;       anything about the exact definition of this structure.
;
; EXAMPLE:
;       mca = obj_new('MCA')
;       mca->read_file, 'mca.001'
;       elapsed = mca->GET_ELAPSED()
;
; MODIFICATION HISTORY:
;       Written by:     Mark Rivers, October 1, 1997
;-
    return, self.elapsed
end


;*****************************************************************************
function mca::get_presets
;+
; NAME:
;       MCA::GET_PRESETS
;
; PURPOSE:
;       This function returns the preset parameters for the MCA.
;       The preset information is contained in a structure of type
;       MCA_PRESETS.
;
; CATEGORY:
;       IDL device class library.
;
; CALLING SEQUENCE:
;
;       Result = mca->GET_PRESETS()
;
; INPUTS:
;       None:
;
; OUTPUTS:
;       This function returns the preset information for the MCA.
;       The preset information is contained in a structure of type
;       MCA_PRESETS.  This structure is currently defined as follows:
;           {mca_presets, $
;               live_time:      0., $
;               real_time:      0., $
;               total_counts:   0., $
;               start_channel:  0L, $
;               end_channel:    0L, $
;               dwell:          0.,  $
;               channel_advance: 0L,  $
;               prescale:       0L  $
;           }
;
;       .live_time is the preset live time in seconds.
;       .real_time is the preset real time in seconds.
;       .total_counts is the total counts between the preset start and end
;                   channels
;       .start_channel is the first channel for the preset count region
;       .end_channel is the last channel for the preset count region
;       .dwell is the dwell time in seconds for MCS mode
;       .channel_advance is 0 for internal channel advance, 1 for external in
;        MCS mode
;       .prescale is the external prescale factor in MCS mode
;
;       NOTE: The exact definition of the MCA_PRESETS structure is subject
;       to change.  What will not change is the name and function of the fields
;       described here.  However, additional fields may be added and the order
;       of the fields could change.  Users should not write code which assumes
;       anything about the exact definition of this structure.
;
; EXAMPLE:
;       mca = obj_new('MCA')
;       mca->read_file, 'mca.001'
;       presets = mca->GET_PRESETS()
;
; MODIFICATION HISTORY:
;       Written by:     Mark Rivers, October 1, 1997
;-
    return, self.presets
end


;*****************************************************************************
pro mca::set_presets, presets
;+
; NAME:
;       MCA::SET_PRESETS
;
; PURPOSE:
;       This procedure sets the preset parameters for the MCA.
;
; CATEGORY:
;       IDL device class library.
;
; CALLING SEQUENCE:
;
;       mca->SET_PRESETS, Presets
;
; INPUTS:
;       Presets:  A structure of type MCA_PRESETS.  The current definition of
;                 this  structure is described in <A HREF="#MCA::GET_PRESETS">MCA::GET_PRESETS()</A>.
;
;       NOTE: The exact definition of the MCA_PRESETS structure is subject
;       to change.  What will not change is the name and function of the fields
;       described here.  However, additional fields may be added and the order
;       of the fields could change.  Users should not write code which assumes
;       anything about the exact definition of this structure.
;
; OUTPUTS:
;       None
;
; EXAMPLE:
;       mca = obj_new('MCA')
;       mca->read_file, 'mca.001'
;       presets = mca->GET_PRESETS()
;       presets.live_time = 0.5
;       mca->SET_PRESETS, presets
;
; MODIFICATION HISTORY:
;       Written by:     Mark Rivers, October 1, 1997
;-
    self.presets = presets
end


;*****************************************************************************
function mca::get_acquire_status
;+
; NAME:
;       MCA::GET_ACQUIRE_STATUS
;
; PURPOSE:
;       This function returns the acquisition status of the MCA.
;
; CATEGORY:
;       IDL device class library.
;
; CALLING SEQUENCE:
;
;       Result = mca->GET_ACQUIRE_STATUS()
;
; INPUTS:
;       None:
;
; OUTPUTS:
;       This function returns the acquisition status of the MCA.
;       The function returns 0 if the device is not acquiring and 1 if it
;       is acquiring.
;
; EXAMPLE:
;       mca = obj_new('MCA')
;       mca->read_file, 'mca.001'
;       acq = mca->GET_ACQUIRE_STATUS()
;
; MODIFICATION HISTORY:
;       Written by:     Mark Rivers, October 1, 1997
;-
    return, self.acquiring
end


;*****************************************************************************
function mca::get_nchans
;+
; NAME:
;       MCA::GET_NCHANS
;
; PURPOSE:
;       This function returns the number of channels in the MCA
;
; CATEGORY:
;       IDL device class library.
;
; CALLING SEQUENCE:
;
;       Result = mca->GET_NCHANS()
;
; INPUTS:
;       None:
;
; OUTPUTS:
;       This function returns the number of channels in the MCA.
;
; EXAMPLE:
;       mca = obj_new('MCA')
;       mca->read_file, 'mca.001'
;       nchans = mca->GET_NCHANS()
;
; MODIFICATION HISTORY:
;       Written by:     Mark Rivers, October 1, 1997
;-
    return, self.nchans
end


;*****************************************************************************
function mca::get_data
;+
; NAME:
;       MCA::GET_DATA
;
; PURPOSE:
;       This function returns the data for the MCA.
;
; CATEGORY:
;       IDL device class library.
;
; CALLING SEQUENCE:
;
;       Result = mca->GET_DATA()
;
; INPUTS:
;       None:
;
; OUTPUTS:
;       This function returns the data for the MCA.  This is an array whose
;       length is the same as that returned by the function MCA::GET_NCHANS().
;
; EXAMPLE:
;       mca = obj_new('MCA')
;       mca->read_file, 'mca.001'
;       data = mca->GET_DATA()
;
; MODIFICATION HISTORY:
;       Written by:     Mark Rivers, October 1, 1997
;-
    return, self.data(0:self.nchans-1)
end


;*****************************************************************************
pro mca::set_data, data
;+
; NAME:
;       MCA::SET_DATA
;
; PURPOSE:
;       This procedure writes the data for the MCA.
;
; CATEGORY:
;       IDL device class library.
;
; CALLING SEQUENCE:
;
;       mca->SET_DATA, Data
;
; INPUTS:
;       Data:  An array of data which is stored in the MCA object.
;
; OUTPUTS:
;       None.
;
; EXAMPLE:
;       mca = obj_new('MCA')
;       mca->read_file, 'mca.001'
;       data = findgen(mca->GET_NCHANS())
;       mca->SET_DATA(), data
;
; MODIFICATION HISTORY:
;       Written by:     Mark Rivers, October 1, 1997
;-
    self.data = data
    self.nchans = n_elements(data)
end


;*****************************************************************************
function mca::get_rois, roi_info, energy=energy
;+
; NAME:
;       MCA::GET_ROIS
;
; PURPOSE:
;       This function returns the region-of-interest parameters for the MCA.
;       This information is contained in an array of structures of type
;       MCA_ROI.
;
; CATEGORY:
;       IDL device class library.
;
; CALLING SEQUENCE:
;
;       Result = mca->GET_ROIS(ROI_Info, /ENERGY)
;
; INPUTS:
;       None:
;
; KEYWORD PARAMETERS:
;       ENERGY:  Set this keyword to return the ROI .left and .right fields
;                in energy rather than in channels.
;
; OUTPUTS:
;       This function returns the region-of-interest parameters for the MCA.
;       This information is contained in an array of structures of type
;       MCA_ROI. This structure is currently defined as follows:
;       {mca_roi,       $
;           left:           0., $
;           right:          0., $
;           centroid:       0., $
;           bgd_width:      0L, $
;           use:            0L, $
;           preset:         0., $
;           label:          '', $
;           d_spacing:      0., $
;           energy:         0.  $
;       }
;
;       .left is the left channel (or energy) of the region of interest
;       .right is the right channel (or energy) of the region of interest
;       .centroid is the channel centroid of the region of interest.
;       Note that this value is NOT presently computed by the MCA class.
;       .bgd_width is the number of channels of background outwide the ROI
;       to be used in computing net counts in the ROI
;       .use is a flag indicating whether this ROI should be used in
;       energy-calibration.
;       .preset is the preset counts in the ROI
;       .label is a string label to identify the ROI
;       .d_spacing is the lattice spacing for this ROI when it is a diffraction
;        peak
;       .energy is the energy of the centroid of the ROI
;
;       NOTE: The exact definition of the MCA_ROI structure is subject
;       to change.  What will not change is the name and function of the fields
;       described here.  However, additional fields may be added and the order
;       of the fields could change.  Users should not write code which assumes
;       anything about the exact definition of this structure.
;
; OPTIONAL OUTPUTS:
;       Roi_Info:  This is a structure which returns some additional
;                  information about the regions-of-interest.  This structure
;                  is presently defined as follows:
;                       {max_rois: 0L, $
;                        nrois:    0L}
;                  .max_rois is the maximum number of ROIs which can be
;                       defined.
;                  .nrois is the number of ROIs presently defined.  This is the
;                       number of elements in the function return value.
;                       If there are no ROIs presently defined then this number
;                       is zero, and the function return value will be a single
;                       structure of type MCA_ROI which should be ignorred.
;
; EXAMPLE:
;       mca = obj_new('MCA')
;       mca->read_file, 'mca.001'
;       rois = mca->GET_ROIS(roi_info)
;       if (roi_info.nrois gt 0) then begin
;           print, rois[0].left
;           ...
;
; MODIFICATION HISTORY:
;       Written by:     Mark Rivers, October 1, 1997
;       Mark Rivers, Sept. 25, 1998  Added ENERGY keyword
;-
    roi_info = {mca_roi_info}
    roi_info.max_rois = self.max_rois
    roi_info.nrois = self.nrois
    if (self.nrois eq 0) then roi=self.roi(0) $
    else roi = self.roi(0:self.nrois-1)
    if (keyword_set(energy)) then begin
        roi.left = self->chan_to_energy(roi.left)
        roi.right = self->chan_to_energy(roi.right)
    endif
    return, roi
end


;*****************************************************************************
pro mca::get_roi_counts, total, net, background_width=background_width
;+
; NAME:
;       MCA::GET_ROI_COUNTS
;
; PURPOSE:
;       This procedures returns the net and total counts of each
;       region-of-interest in the MCA.
;
; CATEGORY:
;       IDL device class library.
;
; CALLING SEQUENCE:
;
;       mca->GET_ROI_COUNTS, Total, Net
;
; INPUTS:
;       None
;
; KEYWORD PARAMETERS:
;       BACKGROUND_WIDTH :
;           Set this keyword to set the width of the background region on either
;           side of the peaks when computing net counts.  The default is 1.
;
; OUTPUTS:
;       Total:  The total counts in each ROI.
;       Net:    The net counts in each ROI.
;
;       The dimension of each array is NROIS, where NROIS
;       is the number of currently defined ROIs for this MCA.  It returns
;       zero for both if NROIS is zero.  Users should call
;       MCA::GET_ROIS() to check the number of ROIS.
;
; EXAMPLE:
;       mca = obj_new('MCA')
;       mca->read_file, 'mca.001'
;       mca->GET_ROI_COUNTS, total, net, background_width=3
;       print, 'Net counts = ', net
;
; MODIFICATION HISTORY:
;       Written by:     Mark Rivers, June 29, 2001
;-
    nrois = self.nrois > 1
    total = fltarr(nrois)
    net = fltarr(nrois)
    if (n_elements(background_width) eq 0) then background_width=1
    for i=0, self.nrois-1 do begin
        left = self.roi[i].left
        ll = (left-background_width+1) > 0
        if (background_width gt 0) then $
            bgd_left = total(self.data[ll:left]) / (left-ll+1) $
        else bgd_left = 0.
        right = self.roi[i].right
        rr = (right+background_width-1) < self.nchans
        if (background_width gt 0) then $
            bgd_right = total(self.data[right:rr]) / (rr-right+1) $
        else bgd_right = 0.
        total_counts = self.data[left:right]
        total[i] = total(total_counts)
        n_sel        = right - left + 1L
        bgd_counts   = bgd_left + findgen(n_sel)/(n_sel-1) * $
                                 (bgd_right - bgd_left)
        net_counts   = total_counts - bgd_counts
        net[i]       = total(net_counts)
    endfor

end


;*****************************************************************************
pro mca::set_rois, rois, energy=energy
;+
; NAME:
;       MCA::SET_ROIS
;
; PURPOSE:
;       This procedure sets the region-of-interest parameters for the MCA.
;       The rois information is contained in a structure of type
;       MCA_ROIS.
;
; CATEGORY:
;       IDL device class library.
;
; CALLING SEQUENCE:
;
;       mca->SET_ROIS, Rois
;
; INPUTS:
;       Rois:  A structure of type MCA_ROIS.  The current definition of this
;              structure is described in <A HREF="#MCA::GET_ROIS">MCA::GET_ROIS()</A>.
;
;       NOTE: The exact definition of the MCA_ROIS structure is subject
;       to change.  What will not change is the name and function of the fields
;       described here.  However, additional fields may be added and the order
;       of the fields could change.  Users should not write code which assumes
;       anything about the exact definition of this structure.
;
; KEYWORD PARAMETERS:
;       ENERGY:     Set this flag to indicate that the .left and .right fields
;                   of Rois are in units of energy rather than channel number.
;
; OUTPUTS:
;       None
;
; EXAMPLE:
;       mca = obj_new('MCA')
;       mca->read_file, 'mca.001'
;       rois = mca->GET_ROIS()
;       rois[0].left = 1012
;       rois[0].right = 1050
;       mca->SET_ROIS, rois
;
; MODIFICATION HISTORY:
;       Written by:     Mark Rivers, October 1, 1997
;       21-Mar-1998     Mark Rivers.  Changed logic to modify nrois
;-
    if (n_elements(rois) eq 0) then begin
        self.nrois = 0
    endif else begin
        self.nrois = n_elements(rois)
        temp = rois
        if (keyword_set(energy)) then begin
            temp.left = (self->energy_to_chan(temp.left) > 0) < self.nchans
            temp.right = (self->energy_to_chan(temp.right) > 0) < self.nchans
        endif
        self.roi = temp
    endelse
end


;*****************************************************************************
function mca::add_roi, roi, energy=energy
;+
; NAME:
;       MCA::ADD_ROI
;
; PURPOSE:
;       This procedure adds a new region-of-interest to the MCA.  If the
;       maximum number of ROIs has been exceeded then this routine returns -1
;       else it returns 0.
;
; CATEGORY:
;       IDL device class library.
;
; CALLING SEQUENCE:
;       status = mca->ADD_ROI(roi)
;
; INPUTS:
;       Roi:        A structure of type MCA_ROI.
;
; KEYWORD PARAMETERS:
;       ENERGY:     Set this flag to indicate that the .left and .right fields
;                   of Roi are in units of energy rather than channel number.
;
; FUNCTION RETURNS:
;       0 if the ROI was added successfully, -1 if there are too many ROIs.
;
; EXAMPLE:
;       mca = obj_new('MCA')
;       mca->read_file, 'mca.001'
;       roi = {MCA_ROI}
;       roi.left = 500
;       roi.right = 600
;       roi.label = 'Fe Ka'
;       mca->ADD_ROI, roi
;
; MODIFICATION HISTORY:
;       Written by:     Mark Rivers, October 1, 1997
;       23-Mar-1998     Mark Rivers, changed the input to be an MCA_ROI
;                       structure from left, right, label, bgd_width
;       11-Sep-1998     Mark Rivers.  Added ENERGY keyword
;-
    if (self.nrois eq self.max_rois) then return, -1
    temp = roi
    if (keyword_set(energy)) then begin
        temp.left = (self->energy_to_chan(temp.left) > 0) < self.nchans
        temp.right = (self->energy_to_chan(temp.right) > 0) < self.nchans
    endif
    self.roi(self.nrois) = temp

    n = self.nrois
    self.nrois = self.nrois + 1

    ; Sort ROIs
    sort = sort(self.roi(0:n).left)
    self.roi(0:n)  = self.roi(sort)
    return, 0
end



;*****************************************************************************
function mca::find_roi, left, right, energy=energy
;+
; NAME:
;       MCA::FIND_ROI
;
; PURPOSE:
;       This procedure finds the index number of the ROI with a specified
;       left and right channel number.
;
; CATEGORY:
;       IDL device class library.
;
; CALLING SEQUENCE:
;       index = mca->FIND_ROI(Left, Right)
;
; INPUTS:
;       Left:        Left channel number of this ROI
;       Right:       Right channel number of this ROI
;
; KEYWORD PARAMETERS:
;       ENERGY:     Set this flag to indicate that Left and Right are in units
;                   of energy rather than channel number.
;
; FUNCTION RETURNS:
;       Index of the specified ROI, -1 if the ROI was not found.
;
; EXAMPLE:
;       mca = obj_new('MCA')
;       mca->read_file, 'mca.001'
;       index = mca->FIND_ROI(left, right)
;
; MODIFICATION HISTORY:
;       Written by:     Mark Rivers, March 23, 1998
;-
    l = left
    r = right
    if (keyword_set(energy)) then begin
        l = (self->energy_to_chan(l) > 0) < self.nchans
        r = (self->energy_to_chan(r) > 0) < self.nchans
    endif
    last = self.nrois-1
    if (last lt 0) then return, -1
    index = where( self.roi(0:last).left eq l and $
                   self.roi(0:last).right eq r, count)
    if (count eq 0L) then return, -1 else return, index(0)
end

;*****************************************************************************
pro mca::del_roi, index
;+
; NAME:
;       MCA::DEL_ROI
;
; PURPOSE:
;       This procedure deletes the specified region-of-interest from the MCA.
;
; CATEGORY:
;       IDL device class library.
;
; CALLING SEQUENCE:
;
;       mca->DEL_ROI, index
;
; INPUTS:
;       Index:      The index of the ROI to be deleted, range 0 to nrois-1
;
; OUTPUTS:
;       None
;
; EXAMPLE:
;       mca = obj_new('MCA')
;       mca->read_file, 'mca.001'
;       mca->DEL_ROI, 2
;
; MODIFICATION HISTORY:
;       Written by:     Mark Rivers, October 1, 1997
;       23-Mar-1998:    Mark Rivers, changed routine to use ROI index, rather
;                       than left and right channel numbers.
;                       Function MCA::FIND_ROI is typically used to find the
;                       index number before calling this routine.
;-
    if ((self.nrois eq 0) or $
        (index lt 0) or $
        (index gt self.nrois)) then return
    for i=index, self.nrois-2 do self.roi(i)  = self.roi(i+1)
    self.nrois = self.nrois - 1
end


;*****************************************************************************
function mca::chan_to_energy, chan
;+
; NAME:
;       MCA::CHAN_TO_ENERGY
;
; PURPOSE:
;       This function converts channels to energy using the current
;       calibration values for the MCA.  This routine can convert a single
;       channel number or an array of channel numbers.  Users are strongly
;       encouraged to use this function rather than implement the conversion
;       calculation themselves, since it will be updated if additional
;       calibration parameters (cubic, etc.) are added.
;
; CATEGORY:
;       IDL device class library.
;
; CALLING SEQUENCE:
;
;       Result = mca->CHAN_TO_ENERGY(Channels)
;
; INPUTS:
;       Channels:  The channel numbers to be converted to energy.  This can be
;                  a single number or an array of channel numbers.
;
; OUTPUTS:
;       This function returns the equivalent energy for the input channels.
;
; EXAMPLE:
;       mca = obj_new('MCA')
;       mca->read_file, 'mca.001'
;       channels = findgen(mca->GET_NCHANS())  ; All channel numbers
;       energy = mca->CHAN_TO_ENERGY(channels) ; Get the energy of these
;
; MODIFICATION HISTORY:
;       Written by:     Mark Rivers, October 1, 1997
;-
   ; This works with chan either scaler or array
   return, self.calibration.offset + $
           self.calibration.slope*chan + $
           self.calibration.quad*chan^2
end


;*****************************************************************************
function mca::chan_to_d, chan
;+
; NAME:
;       MCA::CHAN_TO_D
;
; PURPOSE:
;       This function converts channels to "d-spacing" using the current
;       calibration values for the MCA.  This routine can convert a single
;       channel number or an array of channel numbers.  Users are strongly
;       encouraged to use this function rather than implement the conversion
;       calculation themselves, since it will be updated if additional
;       calibration parameters are added.  This routine is useful for energy
;       dispersive diffraction experiments.  It uses both the energy calibration
;       parameters and the "two-theta" calibration parameter.
;
; CATEGORY:
;       IDL device class library.
;
; CALLING SEQUENCE:
;
;       Result = mca->CHAN_TO_D(Channels)
;
; INPUTS:
;       Channels:  The channel numbers to be converted to "d-spacing".
;                  This can be a single number or an array of channel numbers.
;
; OUTPUTS:
;       This function returns the equivalent "d-spacing" for the input channels.
;       The output units are in Angstroms.
;
; RESTRICTIONS:
;       This function assumes that the units of the energy calibration are keV
;       and that the units of "two-theta" are degrees.
;
; EXAMPLE:
;       mca = obj_new('MCA')
;       mca->read_file, 'mca.001'
;       channels = findgen(mca->GET_NCHANS())  ; All channel numbers
;       d = mca->CHAN_TO_D(channels)           ; Get the "d-spacing" of these
;
; MODIFICATION HISTORY:
;       Written by:     Mark Rivers, October 1, 1997
;-
    e = self->chan_to_energy(chan)
    return, 12.398 / (2. * e * sin(self.calibration.two_theta*!dtor/2.))
end

;*****************************************************************************
function mca::energy_to_chan, energy
;+
; NAME:
;       MCA::ENERGY_TO_CHAN
;
; PURPOSE:
;       This function converts energy to channels using the current
;       calibration values for the MCA.  This routine can convert a single
;       energy or an array of energy values.  Users are strongly
;       encouraged to use this function rather than implement the conversion
;       calculation themselves, since it will be updated if additional
;       calibration parameters are added.
;
; CATEGORY:
;       IDL device class library.
;
; CALLING SEQUENCE:
;
;       Result = mca->ENERGY_TO_CHAN(Energy)
;
; INPUTS:
;       Energy:  The energy values to be converted to channels. This can be a
;                single number or an array of energy values.
;
; OUTPUTS:
;       This function returns the closest equivalent channel for the input
;       energy.  Note that it does not generate an error if the channel number
;       is outside the range 0 to (nchans-1), which will happen if the energy
;       is outside the range for the calibration values of the MCA.
;
; EXAMPLE:
;       mca = obj_new('MCA')
;       mca->read_file, 'mca.001'
;       channel = mca->ENERGY_TO_CHAN(5.985)
;
; MODIFICATION HISTORY:
;       Written by:     Mark Rivers, October 1, 1997
;
;       Mark Rivers, November 4, 1997
;           Fixed bug when converting an array of energies with non-linear
;           calibration coefficients
;       Mark Rivers, February 20, 2000
;           Fixed bug in choosing the correct root returned by fz_roots
;-
   if (self.calibration.quad eq 0.0) then begin
      return, round((energy-self.calibration.offset) / self.calibration.slope)
   endif else begin
      npts = n_elements(energy)
      chan = energy
      for i=0,npts-1 do begin
         t = fz_roots([self.calibration.offset-energy[i], $
                    self.calibration.slope, $
                    self.calibration.quad])
         ; There are 2 roots.  Which one is correct depends on the sign of
         ; the quadratic term
         if (self.calibration.quad lt 0) then chan[i]=t[0] else chan[i]=t[1]
      endfor
      return, round(chan)
   endelse
end

;*****************************************************************************
function mca::d_to_chan, d
;+
; NAME:
;       MCA::D_TO_CHAN
;
; PURPOSE:
;       This function converts "d-spacing" to channels using the current
;       calibration values for the MCA.  This routine can convert a single
;       "d-spacing" or an array of "d-spacings".  Users are strongly
;       encouraged to use this function rather than implement the conversion
;       calculation themselves, since it will be updated if additional
;       calibration parameters are added.    This routine is useful for energy
;       dispersive diffraction experiments.  It uses both the energy calibration
;       parameters and the "two-theta" calibration parameter.
;
; CATEGORY:
;       IDL device class library.
;
; CALLING SEQUENCE:
;
;       Result = mca->D_TO_CHAN(D_spacing)
;
; INPUTS:
;       D_spacing:  The "d-spacing" values to be converted to channels.
;                   This can be a single number or an array of values.
;
; OUTPUTS:
;       This function returns the closest equivalent channel for the input
;       "d-spacing". Note that it does not generate an error if the channel
;       number is outside the range 0 to (nchans-1), which will happen if the
;       "d-spacing" is outside the range for the calibration values of the MCA.
;
; EXAMPLE:
;       mca = obj_new('MCA')
;       mca->read_file, 'mca.001'
;       channel = mca->D_TO_CHAN(1.598)
;
; MODIFICATION HISTORY:
;       Written by:     Mark Rivers, October 1, 1997
;-
    e = 12.398 / (2. * d * sin(self.calibration.two_theta*!dtor/2.))
    return, self->energy_to_chan(e)
end



;*****************************************************************************
pro mca::write_file, file, netcdf=netcdf ; write data to a file
;+
; NAME:
;       MCA::WRITE_FILE
;
; PURPOSE:
;       This procedure writes MCA or MED objects to a disk file.
;       It calls MCA::WRITE_NETCDF_FILE if the /NETCDF keyword is specified,
;       otherwise it calls MCA::WRITE_ASCII_FILE.
;
;       Note that users who want to read such files with IDL are strongly
;       encouraged to use MCA::READ_FILE.
;
; CATEGORY:
;       IDL device class library.
;
; CALLING SEQUENCE:
;       mca->WRITE_FILE, File
;
; INPUTS:
;       File:  The name of the disk file to write.
;
; KEYWORD PARAMETERS:
;       NETCDF:  Set this flag to write the file in netCDF format, otherwise
;                the file is written in ASCII format.  See the documentation
;                for MCA::WRITE_ASCII_FILE and MCA::WRITE_NETCDF_FILE for
;                information on the formats.
;
; EXAMPLE:
;       mca = obj_new('MCA')
;       mca->write_file, 'mca.001', /netcdf
;
; MODIFICATION HISTORY:
;       Written by:     Mark Rivers, September 29, 2001.  The previous
;                       version of MCA::WRITE_FILE was renamed to MCA::
;                       WRITE_ASCII_FILE, and the MCA::WRITE_NETCDF_FILE was
;                       added.
;-
    if (keyword_set(netcdf)) then begin
        self->write_netcdf_file, file
    endif else begin
        self->write_ascii_file, file
    endelse
end

;*****************************************************************************
pro mca::write_ascii_file, file
;+
; NAME:
;       MCA::WRITE_ASCII_FILE
;
; PURPOSE:
;       This procedure writes MCA or MED objects to a disk file.  The file format
;       is a tagged ASCII format.  The file contains the information from the
;       MCA object which it makes sense to store permanently, but does not
;       contain all of the internal state information for the MCA.  Files
;       written with this routine can be read with <A HREF="#MCA::READ_FILE">MCA::READ_FILE</A>.
;
;       Note that users who want to read such files with IDL are strongly
;       encouraged to use MCA::READ_FILE.  For reading files in other languages
;       users should use the tags to interpret the data and NOT rely on the
;       position of lines in the data.  Additional tags may be added in the
;       future, but existing tags will not be changed.
;
;       This procedure is typically not called directly, but is called
;       by MCA::WRITE_FILE if the /NETCDF keyword is not used.
;
;       The following shows part of a file written with this procedure for a
;       13 element MED:
;       ...
;
; CATEGORY:
;       IDL device class library.
;
; CALLING SEQUENCE:
;       mca->WRITE_ASCII_FILE, File
;
; INPUTS:
;       File:  The name of the disk file to write.
;
; EXAMPLE:
;       mca = obj_new('MCA')
;       ; Note - we don't call mca->write_ascii_file directly, but rather
;       ; call mca->write_file without the /netcdf flag
;       mca->write_file, 'mca.001'
;
; MODIFICATION HISTORY:
;       Written by:     Mark Rivers, October 1, 1997
;       28-SEP-1998  MLR  Merged code for MCA and MED classes into a single
;                         WRITE_FILE routine.  The major changes are that
;                         the file version number was changed from 3.0 to 3.1
;                         and ROI labels now have an '&' character after them,
;                         needed as a separator when storing MED data.  The
;                         files can now contain data for multiple MCAs.
;       30-OCT-1998 MLR   Fixed a bug.  The ELEMENTS: tag must be the first
;                         tag after VERSION:, since controls the dimension
;                         of arrays in READ_FILE.  Previous version wrote
;                         DATE: tag before ELEMENTS:
;       20-FEB-2000 MLR   Changed format of calibration coefficients from F
;                         to E to preserve precision.
;       31-MAY-2000 MLR   Added ",x" to formats to compensate for a bug in IDL
;                         (up to 5.3?) which caused no space in floating
;                         numbers printed in "e" format if negative.
;       29-SEP-2001 MLR   Renamed this procedure from MCA::WRITE_FILE to
;                         MCA::WRITE_ASCII_FILE.
;-

    elapsed = self->get_elapsed()
    calibration = self->get_calibration()
    rois = self->get_rois(roi_info)
    data = self->get_data()
    environment = self->get_environment(n_env)

    n_detectors = n_elements(elapsed)
    fformat = '(' + strtrim(n_detectors,2) + '(f,x))'
    eformat = '(' + strtrim(n_detectors,2) + '(e,x))'
    iformat = '(' + strtrim(n_detectors,2) + '(i,x))'
    sformat = '(' + strtrim(n_detectors,2) + '(a,x))'
    nchans = n_elements(data[*,0])
    nrois = max(roi_info.nrois)

    openw, unit, file, /get_lun, width=1000
    printf, unit, 'VERSION:    ', '3.1'
    printf, unit, 'ELEMENTS:   ', n_detectors
    printf, unit, 'DATE:       ', elapsed[0].start_time
    printf, unit, 'CHANNELS:   ', nchans

    line = strcompress(string(roi_info.nrois, format=iformat, /print))
    printf, unit, 'ROIS:       ', line
    line = strcompress(string(elapsed.real_time, format=fformat, /print))
    printf, unit, 'REAL_TIME:  ', line
    line = strcompress(string(elapsed.live_time, format=fformat, /print))
    printf, unit, 'LIVE_TIME:  ', line
    line = strcompress(string(calibration.offset, format=eformat, /print))
    printf, unit, 'CAL_OFFSET: ', line
    line = strcompress(string(calibration.slope, format=eformat, /print))
    printf, unit, 'CAL_SLOPE:  ', line
    line = strcompress(string(calibration.quad, format=eformat, /print))
    printf, unit, 'CAL_QUAD:   ', line
    line = strcompress(string(calibration.two_theta, format=fformat, /print))
    printf, unit, 'TWO_THETA:  ', line

    for i=0, nrois-1 do begin
        num = strtrim(i, 2)
        printf, unit, 'ROI_'+num+'_LEFT:  ', $
                    strcompress(string(rois[i,*].left, format=iformat, /print))
        printf, unit, 'ROI_'+num+'_RIGHT:  ', $
                    strcompress(string(rois[i,*].right, format=iformat, /print))
        printf, unit, 'ROI_'+num+'_LABEL: ', $
                    string((rois[i,*].label + ' & '), format=sformat, /print)
    endfor
    for i=0, n_env-1 do begin
        printf, unit, 'ENVIRONMENT: ', environment[i].name, '="', $
                                       environment[i].value, $
                                '" (', environment[i].description, ')'
    endfor
    printf, unit, 'DATA: '
    for i=0, nchans-1 do begin
        temp = reform(data[i,*])
        line = strcompress(string(temp, format=iformat, /print))
        printf, unit, line
    endfor
    free_lun, unit
end

;*****************************************************************************
pro mca::write_netcdf_file, file
;+
; NAME:
;       MCA::WRITE_FILE
;
; PURPOSE:
;       This procedure writes MCA or MED objects to a disk file in netCDF
;       format.  netCDF is a portable, self-describing binary format, and there
;       is software to read such files in many data analysis and data
;       visualization programs.  It is much more efficient, in speed and disk
;       space, than the ASCII format. The file contains the information from the
;       MCA object which it makes sense to store permanently, but does not
;       contain all of the internal state information for the MCA.  Files
;       written with this routine can be read with <A HREF="#MCA::READ_FILE">MCA::READ_FILE</A>.
;
;       This procedure is typically not called directly, but is called
;       by MCA::WRITE_FILE if the /NETCDF keyword is used.
;
;       Note that users who want to read such files with IDL are strongly
;       encouraged to use MCA::READ_FILE.  Files can be read in other languages
;       by using the netCDF library which is availble from XXXXX
;
; CATEGORY:
;       IDL device class library.
;
; CALLING SEQUENCE:
;       mca->WRITE_NETCDF_FILE, File
;
; INPUTS:
;       File:  The name of the disk file to write.
;
; EXAMPLE:
;       mca = obj_new('MCA')
;       ; Note - we don't call mca->write_netcdf_file directly, but rather
;       ; use the /netcdf flag to mca->write_file
;       mca->write_file, 'mca.001', /netcdf
;
; MODIFICATION HISTORY:
;       Written by:     Mark Rivers, September 29, 2001.
;-

    elapsed = self->get_elapsed()
    calibration = self->get_calibration()
    rois = self->get_rois(roi_info)
    environment = self->get_environment(n_env)
    data = self->get_data()

    n_detectors = n_elements(elapsed)
    nchans = n_elements(data[*,0])
    max_rois = max(roi_info.nrois)

    MAX_STRING=80

    ; netCDF file format
    ; Create netCDF file
    file_id = ncdf_create(file, /clobber)
    ncdf_control, file_id, fill=0

    ; Create dimensions
    n_detectors_id = ncdf_dimdef(file_id, 'N_DETECTORS', n_detectors)
    nchans_id = ncdf_dimdef(file_id, 'N_CHANS', nchans)
    max_string_id = ncdf_dimdef(file_id, 'MAX_STRING', MAX_STRING)
    if (max_rois gt 0) then $
        max_rois_id = ncdf_dimdef(file_id, 'MAX_ROIS', max_rois)
    if (n_env gt 0) then $
        n_env_id = ncdf_dimdef(file_id, 'N_ENVIRONMENT', n_env)

    ; Create variables
    data_id = ncdf_vardef(file_id, 'DATA', [nchans_id, n_detectors_id], /LONG)
    nrois_id = ncdf_vardef(file_id, 'N_ROIS', [n_detectors_id], /LONG)
    real_time_id = ncdf_vardef(file_id, 'REAL_TIME', [n_detectors_id], /FLOAT)
    live_time_id = ncdf_vardef(file_id, 'LIVE_TIME', [n_detectors_id], /FLOAT)
    cal_offset_id = ncdf_vardef(file_id, 'CAL_OFFSET', [n_detectors_id], /FLOAT)
    cal_slope_id = ncdf_vardef(file_id, 'CAL_SLOPE', [n_detectors_id], /FLOAT)
    cal_quad_id = ncdf_vardef(file_id, 'CAL_QUAD', [n_detectors_id], /FLOAT)
    two_theta_id = ncdf_vardef(file_id, 'TWO_THETA', [n_detectors_id], /FLOAT)
    if (max_rois gt 0) then begin
        roi_left_id = ncdf_vardef(file_id, 'ROI_LEFT', $
                            [max_rois_id, n_detectors_id], /LONG)
        roi_right_id = ncdf_vardef(file_id, 'ROI_RIGHT', $
                            [max_rois_id, n_detectors_id], /LONG)
        roi_label_id = ncdf_vardef(file_id, 'ROI_LABEL', $
                            [max_string_id, max_rois_id, n_detectors_id], /CHAR)
    endif
    if (n_env gt 0) then begin
        env_name_id = ncdf_vardef(file_id, 'ENV_NAME', $
                            [max_string_id, n_env_id], /CHAR)
        env_descr_id = ncdf_vardef(file_id, 'ENV_DESCRIPTION', $
                            [max_string_id, n_env_id], /CHAR)
        env_value_id = ncdf_vardef(file_id, 'ENV_VALUE', $
                            [max_string_id, n_env_id], /CHAR)
    endif

    ; Create attributes.  Replace null strings with a blank.
    ncdf_attput, file_id, /GLOBAL, 'VERSION', '3.1'
    date = elapsed[0].start_time
    if (strlen(date) eq 0) then date=' '
    ncdf_attput, file_id, /GLOBAL, 'DATE', date

    ; Put the file into data mode.
    ncdf_control, file_id, /endef

    ; Write variables to the file
    ncdf_varput, file_id, nrois_id, roi_info.nrois
    ncdf_varput, file_id, real_time_id, elapsed.real_time
    ncdf_varput, file_id, live_time_id, elapsed.live_time
    ncdf_varput, file_id, cal_offset_id, calibration.offset
    ncdf_varput, file_id, cal_slope_id, calibration.slope
    ncdf_varput, file_id, cal_quad_id, calibration.quad
    ncdf_varput, file_id, two_theta_id, calibration.two_theta
    if (max_rois gt 0) then begin
        ncdf_varput, file_id, roi_left_id, rois.left
        ncdf_varput, file_id, roi_right_id, rois.right
        ncdf_varput, file_id, roi_label_id, rois.label
    endif
    if (n_env gt 0) then begin
        ncdf_varput, file_id, env_name_id, environment.name
        ncdf_varput, file_id, env_descr_id, environment.description
        ncdf_varput, file_id, env_value_id, environment.value
    endif
    ncdf_varput, file_id, data_id, data[0:nchans-1,*]

    ; Close the file
    ncdf_close, file_id
end


;*****************************************************************************
pro mca::read_file, file
;+
; NAME:
;       MCA::READ_FILE
;
; PURPOSE:
;       This procedure reads a disk file into an MCA object.
;       It simply calls MCA::READ_STANDARD_FILE.
;       The reason for having this routine is that it can easily be overridden
;       by compiling a replacement version after compiling this file.  A
;       replacement version might, for example, first try to read a file in
;       a site-specific format and if that fails try to read the file using
;       MCA::READ_STANDARD_FILE.
;
; CATEGORY:
;       IDL device class library.
;
; CALLING SEQUENCE:
;       mca->READ_FILE, File
;
; INPUTS:
;       File:  The name of the disk file to read.
;
; OUTPUTS:
;       None
;
; EXAMPLE:
;       mca = obj_new('MCA')
;       mca->read_file, 'mca.001'
;
; MODIFICATION HISTORY:
;       Written by:     Mark Rivers, October 1997.
;       10-FEB-2000     Split into READ_FILE and READ_STANDARD_FILE
;-

    self->read_standard_file, file
end


;*****************************************************************************
pro mca::read_standard_file, file
;+
; NAME:
;       MCA::READ_STANDARD_FILE
;
; PURPOSE:
;       This procedure reads a disk file into an MCA object.  The file format
;       is a tagged ASCII format.  The file contains the information from the
;       MCA object which it makes sense to store permanently, but does not
;       contain all of the internal state information for the MCA.  This
;       procedure reads files written with <A HREF="#MCA::WRITE_FILE">MCA::WRITE_FILE</A>.
;
; CATEGORY:
;       IDL device class library.
;
; CALLING SEQUENCE:
;
;       mca->READ_STANDARD_FILE, File
;
; INPUTS:
;       File:  The name of the disk file to read.
;
; OUTPUTS:
;       None
;
; EXAMPLE:
;       mca = obj_new('MCA')
;       mca->read_standard_file, 'mca.001'
;
; MODIFICATION HISTORY:
;       Written by:     Mark Rivers, Feb. 10, 2000.  Split READ_FILE into
;                       READ_FILE and READ_STANDARD_FILE.
;-

    temp = self->get_rois(roi_info)
    mca_read_file, file, elapsed, calibration, rois, roi_info, $
                       environment, data
    self.name = file
    self.nchans = n_elements(data[*,0])
    self->set_elapsed, elapsed[0]
    self->set_calibration, calibration[0]
    nrois = roi_info[0].nrois
    if (nrois gt 0) then begin
        self->set_rois, rois[0:nrois-1, 0]
    endif else begin
        self->set_rois
    endelse
    if (n_elements(environment) gt 0) then self->set_environment, environment
    self->set_data, data[*,0]
end


;*****************************************************************************
pro mca_read_file, file, elapsed, calibration, rois, roi_info, $
                         environment, data
;+
; NAME:
;       MCA_READ_FILE
;
; PURPOSE:
;       This procedure reads a disk file into an MCA object.  This procedure
;       reads either ASCII or netCDF files.  It does not read into the
;       object itself, because the dimensions in the disk file may be
;       inconsistent with those of the object.  The object procedures call
;       this procedure.
;
; CATEGORY:
;       IDL device class library.
;
; CALLING SEQUENCE:
;
;       MCA_READ_FILE, File, Elapsed, Calibration, Rois, Roi_info, $
;                      Environment, Data
;
; INPUTS:
;       File:  The name of the disk file to read.
;
; OUTPUTS:
;       The elapsed, calibration, roi, environment and data information
;       in the disk file
;
; EXAMPLE:
;       mca_read_file, 'mca.001', Elapsed, Calibration, Rois, Roi_info,
;                                 Environment, Data
;
; MODIFICATION HISTORY:
;       Written by:     Mark Rivers, September 29, 2001. Previous version
;                       renamed to MCA_READ_ASCII_FILE

    on_ioerror, ignore_error
    ncdf_control, 0, /noverbose
    file_id = ncdf_open(file, /nowrite)
ignore_error:
    if (n_elements(file_id) eq 0) then begin
        on_ioerror, null
        ; This is not a netCDF file, it is an ASCII file
        mca_read_ascii_file, file, elapsed, calibration, rois, roi_info, $
                       environment, data
    endif else begin
        ; This is a netCDF file
        ; Close the netCDF file that was opened in the test above
        ncdf_close, file_id
        mca_read_netcdf_file, file, elapsed, calibration, rois, roi_info, $
                       environment, data
    endelse
end

;*****************************************************************************
pro mca_read_ascii_file, file, elapsed, calibration, rois, roi_info, $
                         environment, data
;+
; NAME:
;       MCA_READ_ASCII_FILE
;
; PURPOSE:
;       This procedure reads a disk file into an MCA object.  The file format
;       is a tagged ASCII format.  The file contains the information from the
;       MCA object which it makes sense to store permanently, but does not
;       contain all of the internal state information for the MCA.  This
;       procedure reads files written with <A HREF="#MCA::WRITE_FILE">MCA::WRITE_FILE</A>.
;
; CATEGORY:
;       IDL device class library.
;
; CALLING SEQUENCE:
;
;       mca->READ_FILE, File, Elapsed, Calibration, Rois, Data
;
; INPUTS:
;       File:  The name of the disk file to read.
;
; OUTPUTS:
;       None
;
; EXAMPLE:
;       mca_read_ascii_file, 'mca.001', Elapsed, Calibration, Rois, Roi_info, Data
;
; MODIFICATION HISTORY:
;       Written by:     Mark Rivers, October 1, 1997
;       28-SEP-1998  MLR  Made common READ_FILE routine for MCA and MED data
;                         Old files can still be read fine.  The new version
;                         can read files containing multiple MCAs, e.g. MED
;                         objects.
;       30-OCT-1998  MLR  Added code to correctly read files written with
;                         DATE: tag before ELEMENTS: tag.  This was a bug in
;                         WRITE_FILE which is also fixed.
;       31-MAY-2000  MLR  Modified slightly to handle lines without a trailing
;                         blank, as happens when some text editors modify them.
;       29-SEP-2001  MLR  Renamed to MCA_READ_ASCII_FILE, added
;                         MCA_READ_NETCDF_FILE
;-

    openr, unit, file, /get_lun
    line = ''
    elapsed = {MCA_ELAPSED}
    calibration = {MCA_CALIBRATION}
    max_rois = roi_info[0].max_rois
    rois = replicate({MCA_ROI}, roi_info[0].max_rois)
    roi_info = {MCA_ROI_INFO}
    nrois = 0
    nenv = 0
    n_detectors = 1  ; Assume single element data
    while (not eof(unit)) do begin
        readf, unit, line
        pos = strpos(line, ' ')
        if (pos eq -1) then pos = strlen(line)
        tag = strmid(line, 0, pos)
        value = strtrim(strmid(line, pos, 1000), 2)
        case tag of
            'VERSION:'   :  t = 0; No-op
            'DATE:'      :  elapsed.start_time = value
            'ELEMENTS:'  :  begin
                                n_detectors  = long(value)
                                elapsed = $
                                   replicate(elapsed, n_detectors)
                                calibration = $
                                   replicate({MCA_CALIBRATION}, n_detectors)
                                rois = replicate({MCA_ROI}, $
                                                   max_rois, n_detectors)
                                roi_info = $
                                   replicate({MCA_ROI_INFO}, n_detectors)
                            end
            'CHANNELS:'  :  nchans = long(value)
            'ROIS:'      :  begin
                                nrois = lonarr(n_detectors)
                                reads, value, nrois
                                roi_info.nrois = nrois
                                max_rois = max(nrois)
                            end
            'REAL_TIME:' :  begin
                                temp = fltarr(n_detectors)
                                reads, value, temp
                                elapsed.real_time = temp
                            end
            'LIVE_TIME:' :  begin
                                temp = fltarr(n_detectors)
                                reads, value, temp
                                elapsed.live_time = temp
                            end
            'CAL_OFFSET:':  begin
                                temp = fltarr(n_detectors)
                                reads, value, temp
                                calibration.offset = temp
                            end
            'CAL_SLOPE:' :  begin
                                temp = fltarr(n_detectors)
                                reads, value, temp
                                calibration.slope = temp
                            end
            'CAL_QUAD:'  :  begin
                                temp = fltarr(n_detectors)
                                reads, value, temp
                                calibration.quad = temp
                            end
            'TWO_THETA:' :  begin
                                temp = fltarr(n_detectors)
                                reads, value, temp
                                calibration.two_theta = temp
                            end
            'ENVIRONMENT:': begin
                                env = {MCA_ENVIRONMENT}
                                p1 = strpos(value, '=')
                                env.name = strmid(value, 0, p1)
                                p2 = strpos(value, '"', p1+2)
                                env.value = strmid(value, p1+2, p2-p1-2)
                                desc = strmid(value, p2+3, 1000)
                                if (strlen(desc) gt 0) then begin
                                    desc = strmid(desc, 0, strlen(desc)-1)
                                endif
                                env.description = desc
                                if (n_elements(environment) eq 0) then $
                                    environment = env $
                                    else environment = [environment, env]
                            end
            'DATA:'      :  begin
                                data = lonarr(nchans, n_detectors)
                                counts = lonarr(n_detectors)
                                for i=0, nchans-1 do begin
                                    readf, unit, counts
                                    data(i,*) = counts
                                endfor
                            end
            else         :  begin
                            for i=0, max_rois-1 do begin
                                roi = 'ROI_'+strtrim(i, 2)+'_'
                                if (tag eq roi+'LEFT:') then begin
                                    temp = fltarr(n_detectors)
                                    reads, value, temp
                                    rois(i,*).left = transpose(temp)
                                    goto, found_roi
                                endif else if (tag eq roi+'RIGHT:') then begin
                                    temp = fltarr(n_detectors)
                                    reads, value, temp
                                    rois(i,*).right = transpose(temp)
                                    goto, found_roi
                                endif else if (tag eq roi+'LABEL:') then begin
                                    temp = str_sep(value, '&', /trim)
                                    temp = temp[0:n_detectors-1]
                                    rois(i,*).label = transpose(temp)
                                    goto, found_roi
                                endif
                            endfor
                            message, 'Unknown tag = '+tag+' in file: ' $
                                        + file + '.'
                            found_roi:
                            end
        endcase
    endwhile
    ; Make sure DATA array is define, else this was not a valid data file
    if (n_elements(data) eq 0) then message, 'Not a valid data file: ' + file + '.'
    free_lun, unit
end


;*****************************************************************************
pro mca_read_netcdf_file, file, elapsed, calibration, rois, roi_info, $
                         environment, data
;+
; NAME:
;       MCA_READ_NETCDF_FILE
;
; PURPOSE:
;       This procedure reads a disk file into an MCA object.  The file format
;       is netCDF.  The file contains the information from the
;       MCA object which it makes sense to store permanently, but does not
;       contain all of the internal state information for the MCA.  This
;       procedure reads files written with <A HREF="#MCA::WRITE_FILE">MCA::WRITE_FILE, /NETCDF</A>.
;
; CATEGORY:
;       IDL device class library.
;
; CALLING SEQUENCE:
;
;       mca->READ_FILE, File, Elapsed, Calibration, Rois, Data
;
; INPUTS:
;       File:  The name of the disk file to read.
;
; OUTPUTS:
;       None
;
; EXAMPLE:
;       mca_read_netdf_file, 'mca.001', Elapsed, Calibration, Rois, Roi_info, Data
;
; MODIFICATION HISTORY:
;       Written by:     Mark Rivers, Sept. 29, 2001
;-

    file_id = ncdf_open(file, /nowrite)

    ; Get the dimension ids
    n_detectors_id = ncdf_dimid(file_id, 'N_DETECTORS')
    ncdf_diminq, file_id, n_detectors_id, name, n_detectors
    max_rois_id = ncdf_dimid(file_id, 'MAX_ROIS')
    if (max_rois_id eq -1) then max_rois=0 $
    else ncdf_diminq, file_id, max_rois_id, name, max_rois
    n_env_id = ncdf_dimid(file_id, 'N_ENVIRONMENT')
    if (n_env_id eq -1) then n_env=0 $
    else ncdf_diminq, file_id, n_env_id, name, n_env

    elapsed = replicate({MCA_ELAPSED}, n_detectors)
    calibration = replicate({MCA_CALIBRATION}, n_detectors)
    roi_info = replicate({MCA_ROI_INFO}, n_detectors)

    ; Process the global attributes
    status = ncdf_inquire(file_id)
    for i=0, status.ngatts-1 do begin
        name = ncdf_attname(file_id, /global, i)
        ncdf_attget, file_id, /global, name, value
        value = string(value)
        case name of
                'VERSION'   :  t = 0; No-op
                'DATE'      :  elapsed.start_time = value
        endcase
    endfor

    ; Get the variable ids - they are -1 if not defined
    data_id = ncdf_varid(file_id, 'DATA')
    nrois_id = ncdf_varid(file_id, 'N_ROIS')
    real_time_id = ncdf_varid(file_id, 'REAL_TIME')
    live_time_id = ncdf_varid(file_id, 'LIVE_TIME')
    cal_offset_id = ncdf_varid(file_id, 'CAL_OFFSET')
    cal_slope_id = ncdf_varid(file_id, 'CAL_SLOPE')
    cal_quad_id = ncdf_varid(file_id, 'CAL_QUAD')
    two_theta_id = ncdf_varid(file_id, 'TWO_THETA')
    if (max_rois gt 0) then begin
        rois = replicate({MCA_ROI}, max_rois, n_detectors)
        roi_left_id = ncdf_varid(file_id, 'ROI_LEFT')
        roi_right_id = ncdf_varid(file_id, 'ROI_RIGHT')
        roi_label_id = ncdf_varid(file_id, 'ROI_LABEL')
    endif
    if (n_env gt 0) then begin
        env_name_id = ncdf_varid(file_id, 'ENV_NAME')
        env_descr_id = ncdf_varid(file_id, 'ENV_DESCRIPTION')
        env_value_id = ncdf_varid(file_id, 'ENV_VALUE')
    endif

    ; Read variables from the file
    ncdf_varget, file_id, nrois_id, temp
    roi_info.nrois = temp
    ncdf_varget, file_id, real_time_id, temp
    elapsed.real_time = temp
    ncdf_varget, file_id, live_time_id, temp
    elapsed.live_time = temp
    ncdf_varget, file_id, cal_offset_id, temp
    calibration.offset = temp
    ncdf_varget, file_id, cal_slope_id, temp
    calibration.slope = temp
    ncdf_varget, file_id, cal_quad_id, temp
    calibration.quad = temp
    ncdf_varget, file_id, two_theta_id, temp
    calibration.two_theta = temp
    if (max_rois gt 0) then begin
        ncdf_varget, file_id, roi_left_id, temp
        rois.left = temp
        ncdf_varget, file_id, roi_right_id, temp
        rois.right = temp
        ncdf_varget, file_id, roi_label_id, temp
        rois.label = string(temp)
    endif
    if (n_env gt 0) then begin
        environment = replicate({MCA_ENVIRONMENT}, n_env)
        ncdf_varget, file_id, env_name_id, temp
        environment.name = string(temp)
        ncdf_varget, file_id, env_descr_id, temp
        environment.description = string(temp)
        ncdf_varget, file_id, env_value_id, temp
        environment.value = string(temp)
    endif
    ncdf_varget, file_id, data_id, data

    ; Close the netCDF file
    ncdf_close, file_id

end


;*****************************************************************************
function mca::init
;+
; NAME:
;       MCA::INIT
;
; PURPOSE:
;       This is the initialization code which is invoked when a new object of
;       type MCA is created.  It cannot be called directly, but only
;       indirectly by the IDL OBJ_NEW() function.
;
; CATEGORY:
;       IDL device class library.
;
; CALLING SEQUENCE:
;       Result = OBJ_NEW('MCA')
;
; INPUTS:
;       None
;
; OUTPUTS:
;       This function always returns success, since it is device independent
;       and always able to create a new object.
;
; RESTRICTIONS:
;       This routine cannot be called directly.  It is called indirectly when
;       creating a new object of class MCA by the IDL OBJ_NEW() function.
;
; EXAMPLE:
;       mca = obj_new('MCA')
;       mca->READ_FILE, 'mca.001'
;
; MODIFICATION HISTORY:
;       Written by:     Mark Rivers, October 1, 1997
;-
   MAX_ROIS = 32
   MAX_CHANS = 8192
   self.calibration.slope = 1.0
   self.calibration.units = 'keV'
   self.max_rois = MAX_ROIS
   self.max_chans = MAX_CHANS
   self.nchans = MAX_CHANS
   self.calibration.two_theta = 10.
   return, 1
end



;*****************************************************************************
pro mca__define
;+
; NAME:
;       MCA__DEFINE
;
; PURPOSE:
;       This is the definition code which is invoked when a new object of
;       type MCA is created.  It cannot be called directly, but only
;       indirectly by the IDL OBJ_NEW() function.  It defines the data
;       structures used for the MCA class.
;
; CATEGORY:
;       IDL device class library.
;
; CALLING SEQUENCE:
;       Result = OBJ_NEW('MCA')
;
; INPUTS:
;       None
;
; OUTPUTS:
;       None
;
; RESTRICTIONS:
;       This routine cannot be called directly.  It is called indirectly when
;       creating a new object of class MCA by the IDL OBJ_NEW()
;       function.
;
; EXAMPLE:
;       mca = obj_new('MCA')
;       mca->READ_FILE, 'mca.001'
;
; MODIFICATION HISTORY:
;       Written by:     Mark Rivers, October 1, 1997
;       23-Mar-1998:    Mark Rivers.  Added .d_spacing to MCA_ROI structure.
;       16-May-1999:    Mark Rivers.  Added .read_time to MCA_ELAPSED structure.
;-
   MAX_ROIS = 32
   MAX_CHANS = 8192
   roi = {mca_roi,       $
      left:           0., $
      right:          0., $
      centroid:       0., $
      bgd_width:      0L, $
      use:            0L, $
      preset:         0., $
      label:          '', $
      d_spacing:      0., $
      energy:         0.  $
   }

   roi_info = {mca_roi_info, $
      nrois:          0L, $
      max_rois:       0l  $
   }

    elapsed = {mca_elapsed,  $
        start_time:     '', $
        live_time:      0., $
        real_time:      0., $
        read_time:      0.D0, $
        total_counts:   0.  $
    }

    calibration = {mca_calibration, $
        offset:     0., $
        slope:      0., $
        quad:       0., $
        units:      '', $
        two_theta:  0.  $
    }

    presets = {mca_presets, $
        live_time:      0., $
        real_time:      0., $
        total_counts:   0., $
        start_channel:  0L, $
        end_channel:    0L, $
        dwell:          0., $
        channel_advance: 0L,  $
        prescale:       0L  $
    }


    environment = {mca_environment, $
            name: '', $
            value: '', $
            description: ''}

    mca = {mca,           $
        name:           '', $
        max_chans:      0L, $
        nchans:         0L, $
        data:        lonarr(MAX_CHANS), $
        max_rois:       0L, $
        nrois:          0L, $
        acquiring:      0L, $
        roi:          replicate(roi, MAX_ROIS), $
        calibration:    calibration, $
        elapsed:        elapsed, $
        presets:        presets, $
        environment:    ptr_new() $
   }
end
