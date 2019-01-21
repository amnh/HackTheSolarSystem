function mpfit_peaks, x, parameters
    common fit_peaks_common, fit, peak, observed, weights, predicted
    copy_fit_params, parameters
    predict_gaussian_spectrum
    return, predicted
end


pro copy_fit_params, parameters

    ; Copies the fit parameters to the "fit" and "peak" structures
    common fit_peaks_common

    np = 0

    fit.energy_offset = parameters[np]
    np = np + 1
    fit.energy_slope = parameters[np]
    np = np + 1

    fit.fwhm_offset = parameters[np]
    np = np + 1
    fit.fwhm_slope = parameters[np]
    np = np + 1

    for i=0, fit.npeaks-1 do begin
        peak[i].energy = parameters[np]
        np = np + 1
        peak[i].fwhm = parameters[np]
        np = np + 1
        if (peak[i].fwhm_flag eq 0) then begin
            peak[i].fwhm = fit.fwhm_offset + $
                    fit.fwhm_slope*sqrt(peak[i].energy)
        endif
        peak[i].ampl = parameters[np]
        np = np + 1
        if (peak[i].ampl_factor eq 0.) then begin
            last_opt_peak = i
        endif else if (peak[i].ampl_factor gt 0.) then begin
            peak[i].ampl = peak[last_opt_peak].ampl * $
                        peak[i].ampl_factor * $
                        peak[last_opt_peak].fwhm / (peak[i].fwhm > .001)
        endif else if (peak[i].ampl_factor eq -1.) then begin
            peak[i].ampl = 0.
        endif
    endfor
end


pro predict_gaussian_spectrum
    common fit_peaks_common

    MAX_SIGMA=5.
    SIGMA_TO_FWHM = 2.35482
    ;fit.n_eval = fit.n_eval + 1L
    predicted = observed*0.D
    sigma = peak.fwhm/SIGMA_TO_FWHM
    ; Compute first channel where each peak makes a "significant" contribution
    first_chan = fix(((peak.energy - sigma*MAX_SIGMA - fit.energy_offset) / $
                fit.energy_slope) > 0 < (fit.nchans-1))
    ; channel where each peak makes a "significant" contribution
    nchans = (2.*sigma*MAX_SIGMA/fit.energy_slope) > 1
    last_chan = (first_chan + nchans) > 0 < (fit.nchans-1)
    nchans = (last_chan - first_chan + 1) > 1
    ;help, /structure, fit
    ;for i=0, fit.npeaks-1 do print, peak[i]
    for i=0, fit.npeaks-1 do begin
        energy = fit.energy_offset + first_chan[i]*fit.energy_slope + $
                    findgen(nchans[i])*fit.energy_slope
        counts = abs(peak[i].ampl) $
                    * exp(-((energy - peak[i].energy)^2 / $
                    (2. * sigma[i]^2)))
        peak[i].area = total(counts)
        predicted(first_chan[i]:last_chan[i]) = $
                predicted(first_chan[i]:last_chan[i]) + counts
        ;print, i, peak[i].energy, peak[i].fwhm, peak[i].area
    endfor
end




function fit_peaks, input_fit, source, input_peaks
;+
; NAME:
;       FIT_PEAKS
;
; PURPOSE:
;       This function fits spectra with a set of Gaussian peaks.
;
; CATEGORY:
;       Spectral data fitting
;
; CALLING SEQUENCE:
;       Result = FIT_PEAKS(Fit, Data, Peaks)
;
; INPUTS:
;       Fit:    
;           A structure of type {MCA_FIT}.  This structure is used to control 
;           the global fitting parameters and options. The exact definition of 
;           this structure is subject to change.  However, the following 
;           "input" fields will always exist and can be used to control the fit
;           process.  Function MCA::FIT_INITIALIZE() can be used to create
;           this structure with reasonable default values for each field.
;           Further information on many of these fields can be found in the
;           procedure description below.
;               .initial_energy_offset ; The initial energy calibration offset. 
;                                      ; FIT_INITIALIZE sets this to the 
;                                      ; calibration offset for the MCA object
;               .initial_energy_slope  ; The initial energy calibration slope. 
;                                      ; FIT_INITIALIZE sets this to the 
;                                      ; calibration slope for the MCA object
;               .energy_flag           ; Energy flag
;                                      ; 0 = Fix energy calibration coeffs
;                                      ; 1 = Optimize energy calibration coeffs
;                                      ; FIT_INITIALIZE sets this to 1
;               .initial_fwhm_offset   ; The initial FWHM calibration offset
;                                      ; FIT_INITIALIZE sets this to 150 eV
;               .initial_fwhm_slope    ; The initial FWHM calibration slope
;                                      ; FIT_INITIALIZE sets this to 0.
;               .fwhm_flag             ; FWHM flag
;                                      ;   0 = Fix FWHM coefficients
;                                      ;   1 = Optimize FWHM coefficients
;                                      ; FIT_INITIALIZE sets this to 1
;               .chi_exp               ; Exponent of chi
;                                      ; FIT_INITIALIZE sets this to 0.
;               .max_eval              ; Maximum # function evaluations
;                                      ; FIT_INITIALIZE sets this to 0 which
;                                      ; does not limit the number of function 
;                                      ; evaluations
;               .max_iter              ; Maximum number of iterations
;                                      ; FIT_INITIALIZE sets this to 20
;               .tolerance             ; Convergence tolerance. The fitting 
;                                      ; process will stop when the value of 
;                                      ; chi^2 changes by a relative amount
;                                      ; less than tolerance on two successive 
;                                      ; iterations. 
;                                      ; FIT_INITIALIZE sets this to 1.e-4
;
;       Data:   
;           The input spectrum to be fit.  Note that this spectrum typically 
;           will have previously had the background fitted using 
;           <A HREF="#FIT_BACKGROUND">FIT_BACKGROUND</A> and this background 
;           subtracted from Data before passing it to this function.
;
;       Peaks:  
;           An array of structures of type {MCA_PEAKS} which contains the
;           parameters for each peak to be fitted. The exact definition of 
;           this structure is subject to change.  However, the following 
;           "input" fields will always exist and can be used to control the fit
;           process.  Function <A HREF="#READ_PEAKS">READ_PEAKS</A> can be used 
;           to read a disk file into this structure.
;           Further information on many of these fields can be found in the
;           procedure description below.
;               .label          ; A string describing the peak
;               .energy_flag    ; Flag for fitting energy of this peak
;                                   ; 0 = Fix energy 
;                                   ; 1 = Optimize energy
;               .fwhm_flag      ; Flag for fitting FWHM of this peak
;                                   ; 0 = Fix FWHM to global curve
;                                   ; 1 = Optimize FWHM
;                                   ; 2 = Fix FWHM to input value
;               .ampl_factor    ; Flag for fitting amplitude of this peak
;                                   ; 0.0  = Optimize amplitude of this peak
;                                   ; >0.0 = Fix amplitude to this value 
;                                   ;        relative to amplitude of 
;                                   ;        previous unconstrained peak
;                                   ; -1.0 = Fix amplitude at 0.0
;               .initial_energy ; Initial value for peak energy
;               .initial_fwhm   ; Initial value for FWHM.  This can be zero if
;                                   ;   .fwhm_flag is 0
;               .initial_ampl   ; Initial value of peak amplitude.  
;                                   ; If .ampl_factor is 0.0 then this function
;                                   ; will automaticailly determine a value for
;                                   ; .initial_ampl
;
; OUTPUTS:
;       This function returns the fitted spectrum as the function return value.
;       It also returns output information in the Fit and Peaks parameters.
;
;       Fit:    
;           A structure of type {MCA_FIT} which contains the global fit 
;           parameters. This is the same structure which is also an input 
;           parameter.  The exact definition of this structure is subject to 
;           change.  However, the following "output" fields will always exist 
;           and contain the results of the fit. Further information on many 
;           of these fields can be found in the procedure description below.
;               .energy_offset  ; Fitted energy calibration offset
;               .energy_slope   ; Fitted energy calibration slope
;               .fwhm_offset    ; Fitted FWHM offset
;               .fwhm_slope     ; FWHM slope
;               .n_eval         ; Actual number of function evalutions
;               .n_iter         ; Actual number of iterations
;               .chisqr         ; Chi-squared on output
;
;       Peaks:  
;           An array of structures of type {MCA_PEAKS} which contains the
;           parameters for each peak to be fitted. This is the same array which
;           is also an input parameter.  The exact definition of 
;           this structure is subject to change.  However, the following 
;           "output" fields will always exist and contain the results of the
;           fit. Further information on many of these fields can be found in the
;           procedure description below.
;               .energy         ; The fitted peak energy
;               .fwhm           ; The fitted peak FWHM
;               .ampl           ; The fitted peak amplitude
;               .area           ; The fitted area of the peak
;
; COMMON BLOCKS:
;       FIT_PEAKS_COMMON:  This common block is used to communicate between the
;               various routines in this file.  It is required because of the
;               way MPFITFUN calls the procedure to evaluate the residuals.
;
; RESTRICTIONS:
;       This function is presently limited to fitting Gaussian peaks.  It may
;       be extended in the future to fit other peak shapes.
;
; PROCEDURE:
;       In general a Gaussian peak has 3 adjustable parameters: position 
;       (or energy), sigma (or FWHM), and amplitude (or area).  For many
;       applications, however, not all of these parameters should be
;       adjustable during the fit.  For example, in XRF analysis the energy of
;       the peaks is known, and should not be optimized.  However, the overall
;       energy calibration coefficients for the entire spectrum, which relate
;       channel number to energy, might well be optimized during the fit.
;       Similarly, the FWHM of XRF peaks are not independent, but rather
;       typically follow a predictable detector response function:
;           FWHM = A + B*sqrt(energy)
;       Finally, even the amplitude of an XRF peak might not be a free
;       parameter, since, for example one might want to constrain the K-beta 
;       peak to be a fixed fraction of the K-alpha.  Such constraints allow 
;       one to fit overlapping K-alpha/K-beta peaks with much better accuracy.
;
;       This procedure is designed to be very flexible in terms of which
;       parameters are fixed and which ones are optimized.  The constraints are
;       communicated via the Fit and Peaks structures.
;
;       The energy of each channel is assumed to obey the relation: 
;           energy = energy_offset + (channel * energy_slope)
;
;       These parameters control the fit for peaks whose energy is fixed, 
;       rather than being a fit parameter.
;       If Fit.energy_flag is 1 then these energy calibration coefficients
;       will be optimized during the fitting process. If it is 0 then these
;       energy calibration coefficients are assumed to be correct and are not 
;       optimized.  Not optimizing the energy calibration coefficients can 
;       both speed up the fitting process and lead to more stable results when 
;       fitting small peaks.  This function does a sanity check and will not
;       optimize these energy calibration coefficients unless at least 2 peaks
;       have their .energy_flag field set to 0, so that they use these global
;       calibration coefficients.
;
;       The FWHM of the peaks is assumed to obey the relation:
;           fwhm = fwhm_offset + (fwhm_slope * sqrt(energy))
;       These parameters control the fit for peaks whose FWHM is neither fixed 
;       nor a fit parameter.
;       If Fit.fwhm_flag is 1 then these coefficients will be optimized during
;       the fitting process. If it is 0 then the specified coefficients are 
;       assumed to be correct and are not optimized. Not optimizing the FWHM
;       coeffcients can both speed up the fitting process and lead to more 
;       stable results when fitting very small peaks. This function does a 
;       sanity check and will not optimize these FWHM calibration coefficients 
;       unless at least 2 peaks have their .fwhm_flag field set to 0, so that 
;       they use these global calibration coefficients.
;
;       This function also optimizes the following parameters:
;           - The amplitudes of all peaks whose .ampl_factor field is 0
;           - The energies of all peaks whose .energy_flag field is 1
;           - The FWHM of all peaks whose .fwhm_flag field is 1
;
;       The parameter which is the minimized during the fitting process is 
;       chi^2, defined as:
;                                                    2
;          2            y_obs[i]    -     y_pred[i]
;       chi  = sum (  ---------------------------- )
;               i              sigma[i]
;
;       where y_obs[i] is the observed counts in channel i, y_pred is the
;       predicted counts in channel i, and sigma[i] is the standard deviation
;       of y_obs[i].
;
;       This function assumes that:
;
;       sigma[i] = y_obs[i] ** chi_exponent
;
;       e.g. that the standard deviation in each channel is equal to the counts
;       in the channel to some power. For photon counting spectra where Poisson
;       statistics apply chi_exponent=0.5, and this is the default. Setting
;       chi_exponent=0. will set all of the sigma[i] values to 1., and the fit
;       would then be minimizing the sum of the squares of the residuals. This
;       should tend to result in a better fit for the large peaks in a spectrum
;       and a poorer fit for the smaller peaks. Setting chi_exponent=1.0 will
;       result in a minimization of the sum of the squares of the relative error
;       in each channel. This should tend to weight the fit more strongly toward
;       the small peaks.
;
;       If .ampl_factor for a peak is 0., then the amplitude of the peak is a 
;       fit parameter. If the amplitude_factor is non-zero then the amplitude 
;       of this peak is not a fit parameter, but rather is constrained to
;       be equal to the amplitude of the last previous peak in the array which 
;       had an amplitude factor of zero, times the amplitude_factor. This can 
;       be used, for instance, fit K-alpha and K-beta x-ray lines when the 
;       alpha/beta ratio is known, and one wants to add this known constraint 
;       to the fitting process.
;       For example:
;           peaks = replicate({mca_peak}, 3)
;           ; Fe Ka is the "reference" peak
;           peaks[0].initial_energy=6.40 & peaks[0].ampl_factor=0.0 
;           ; Si-Ka escape peak is 3% of Fe Ka at 4.66 keV
;           peaks[1].initial_energy=4.66 & peaks[1].ampl_factor=0.03
;           ; Fe-Kb is 23% of Fe Ka
;           peaks[2].initial_energy=7.06 & peaks[2].ampl_factor=0.23
;       In this example the amplitude of the Fe-Ka peak will be fitted, but the
;       amplitudes of the escape peak and the Fe-Kb peak are constrained to
;       be fixed fractions of the Fe-Ka peak.  The reference peak is always the
;       closest preceding peak in the array for which ampl_factor is 0.
;
; EXAMPLE:
;       mca = obj_new('mca')
;       mca->read_file, 'myspect.dat'
;       data = mca->get_data()
;       cal = mca->get_calibration()
;       slope = cal.slope
;       back = fit_background(data, slope)
;       peaks = read_peaks('mypeaks.pks')
;       fit = fit_initialize()
;       result = fit_peaks(fit, data-back, peaks)
;       plot, data
;       oplot, back
;       oplot, result+back
;
;       Note:  This example is provided for reference.  In practice it is
;              simpler to use the MCA object methods MCA::FIT_BACKGROUND and
;              MCA::FIT_PEAKS, because they handle much of the tedious
;              bookkeeping the above example.  However, it is important to
;              recognize that FIT_PEAKS is independent of the MCA class, and
;              can be used without the MCA class library
;
; MODIFICATION HISTORY:
;       Written by:     Mark Rivers, October 21, 1998.  This is the latest
;                       re-write of a routine which has a long history, begun 
;                       at X-26 at the NSLS.  The original version was written 
;                       in a program called SPCALC, and was then ported to IDL.
;                       These early versions used IMSL for the least-squares
;                       routine.  The port to CURVEFIT, so that no external
;                       software package is required, was done in 1998.
;       Mark Rivers, Nov. 9, 1998.  Added sanity check for nchans
;       Mark Rivers, Nov. 12, 1998.  Significant re-write to use MPFITFUN
;                                    in place of CURVEFIT
;       Mark Rivers, Feb. 1, 2001.  Changed amplitude ratio calculation so that the
;                                   AREA of the two peaks has the specified ratio,
;                                   rather than the AMPLITUDE.  This is done by
;                                   adjusting the constrained ratio by the relative
;                                   peak widths.
;-

    common fit_peaks_common

;   Copy initial guesses to fit parameters
    input_fit.energy_offset = input_fit.initial_energy_offset
    input_fit.energy_slope = input_fit.initial_energy_slope
    input_fit.fwhm_offset = input_fit.initial_fwhm_offset
    input_fit.fwhm_slope = input_fit.initial_fwhm_slope
    for i=0, n_elements(input_peaks)-1 do begin
        input_peaks[i].energy = input_peaks[i].initial_energy
        input_peaks[i].fwhm = input_peaks[i].initial_fwhm
        input_peaks[i].ampl = input_peaks[i].initial_ampl
    endfor

    ; Copy input parameters to COMMON block
    fit = input_fit
    peak = input_peaks
    observed = source

    ; Do some sanity checks
    ; Don't fit global FWHM parameters if no peaks use these
    t = where(peak.fwhm_flag eq 0, count)
    if (count lt 2) then fit.fwhm_flag = 0
    ; Don't fit global energy parameters if no peaks use these
    t = where(peak.energy_flag eq 0, count)
    if (count lt 2) then fit.energy_flag = 0
    ; Make max channels check
    fit.nchans = fit.nchans < n_elements(observed)
    ; Don't fit peaks outside the energy range of the data
    for i=0, fit.npeaks-1 do begin
        chan = ((peak[i].energy - $
                    fit.energy_offset)/fit.energy_slope)
        if ((chan lt 0) or (chan gt fit.nchans-1)) then begin
            peak[i].ampl_factor = -1.
            peak[i].energy_flag = 0
            peak[i].fwhm_flag = 2
        endif
    endfor

    ; Maximum number of parameters
    max_params=fit.npeaks*3 + 4

    ; Parameter info structure for initial guesses and constraints
    parinfo = {value:0.D, fixed:0, limited:[0,0], limits:[0.D, 0.D], step:0.D}
    parinfo = replicate(parinfo, max_params)

    ; Compute sigma of observations to computed weighted residuals
    ; Treat special cases of fit.chi_exp=0, .5, 1.
    case fit.chi_exp of
        0.0: weights   = observed*0.0 + 1.0
        0.5: weights   = 1./sqrt(abs(observed>1.))
        1.0: weights   = 1./abs(observed>1.)
        default: weights = 1./(abs(observed>1.)^fit.chi_exp)
    endcase

    ; Copy initial guesses of peak parameters to parameters vector
    np = 0
    parinfo[np].value = fit.energy_offset
    if (fit.energy_flag eq 0) then parinfo[np].fixed=1
    np = np+1
    parinfo[np].value = fit.energy_slope
    if (fit.energy_flag eq 0) then parinfo[np].fixed=1
    np = np+1

    parinfo[np].value = fit.fwhm_offset
    if (fit.fwhm_flag eq 0) then parinfo[np].fixed=1
    np = np+1
    parinfo[np].value = fit.fwhm_slope
    if (fit.fwhm_flag eq 0) then parinfo[np].fixed=1
    np = np+1

    for i=0, fit.npeaks-1 do begin
        parinfo[np].value = peak[i].energy
        if (peak[i].energy_flag eq 0) then parinfo[np].fixed=1
        np = np+1
        parinfo[np].value = peak[i].fwhm
        if (peak[i].fwhm_flag ne 1) then parinfo[np].fixed=1 else begin
            ; Limit the FWHM to .1 to 10 times initial guess
            parinfo[np].limited=[1,1]
            parinfo[np].limits=[peak[i].fwhm/10., peak[i].fwhm*10.]
        endelse
        np = np+1
        if (peak[i].ampl_factor eq 0.) then begin
            chan = ((peak[i].energy - $
                        fit.energy_offset)/fit.energy_slope) $
                    > 0 < (fit.nchans-1)
            peak[i].ampl = observed(chan) > 0.
            ; Limit the amplitude to non-negative values
            parinfo[np].limited=[1,0]
            parinfo[np].limits=[0.,0.]
            last_opt_peak = i
        endif else if (peak[i].ampl_factor gt 0.) then begin
            ; Don't correct for FWHM here, this is just initial value
            peak[i].ampl = peak[last_opt_peak].ampl * $
                        peak[i].ampl_factor
            parinfo[np].fixed=1
        endif else if (peak[i].ampl_factor eq -1.) then begin
            peak[i].ampl = 0.
            parinfo[np].fixed=1
        endif
        parinfo[np].value = peak[i].ampl
        np = np + 1
    endfor

    ; Number of fit parameters and degrees of freedom
    fit.nparams = np
    ; Truncate parameters() to the actual number of fitted parameters
    parinfo = parinfo(0:np-1)
    ;fit.n_eval=0
    ; Call the non-linear least-squares routine
    parameters = mpfitfun('mpfit_peaks', findgen(fit.nchans), observed, $
                weights, /quiet, $
                chi2=fmin,  parinfo=parinfo, $
                xtol=fit.tolerance, maxiter=fit.max_iter, $
                nfev=nfev, perror=perror, bestnorm=bestnorm, status=status, $
                errmsg=errmsg)
    ; Copy optimized results back
    copy_fit_params, parameters
    predict_gaussian_spectrum
    fit.n_iter = 0
    fit.n_eval = nfev
    fit.chisqr = bestnorm
    fit.status = status
    fit.err_string = errmsg
    input_fit = fit
    input_peaks = peak
    return, predicted
end
