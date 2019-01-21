pro mca_fit__define
    fit = {mca_fit, $
        npeaks:      0L, $ ; Number of peaks to fit
        first_chan:  0L, $ ; First channel to fit
        last_chan:   0L, $ ; Last channel to fit
        nchans:      0L, $ ; Number of channels 
        nparams:     0L, $ ; Number of fit parameters
        initial_energy_offset: 0.D, $
        initial_energy_slope:  0.D, $
        energy_offset: 0.D, $ ; Energy calibration offset
        energy_slope:  0.D, $ ; Energy calibration slope
        energy_flag: 0L, $ ; Energy flag
                           ;   0 = Fix energy calibration coefficients
                           ;   1 = Optimize energy calibration coefficients
        initial_fwhm_offset:   0.D, $
        initial_fwhm_slope:    0.D, $
        fwhm_offset:   0.D, $ ; FWHM offset
        fwhm_slope:    0.D, $ ; FWHM slope
        fwhm_flag:   0L, $ ; Fwhm flag
                           ;   0 = Fix FWHM coefficients
                           ;   1 = Optimize FWHM coefficients
        chi_exp:     0.D, $ ; Exponent of chi
        max_eval:    0L, $ ; Maximum # function evaluations
        n_eval:      0L, $ ; Actual number of function evalutions
        max_iter:    0L, $ ; Maximum number of iterations
        n_iter:      0L, $ ; Actual number of iterations
        tolerance:   0.D, $ ; Convergence tolerance
        chisqr:      0.D, $ ; Chi-squared on output
        status:      0L, $ ; Output status code
        err_string:  '', $ ; Output error string
        debug:       0L  $ ; 0 (no debug) or 1
    }
end
