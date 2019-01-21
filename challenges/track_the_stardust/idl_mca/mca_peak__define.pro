pro mca_peak__define
    peak = {mca_peak, $
        label:       "", $ ; Peak label
        energy_flag: 0L, $ ; Flag for fitting energy
                           ;   0 = Fix energy 
                           ;   1 = Optimize energy
        fwhm_flag:   0L, $ ; Flag for fitting FWHM
                           ;   0 = Fix FWHM to global curve
                           ;   1 = Optimize FWHM
                           ;   2 = Fix FWHM to input value
        ampl_factor: 0.D, $ ; Fixed amplitude ratio to previous peak
                           ;   0.D0  = Optimize amplitude of this peak
                           ;   >0.D0 = Fix amplitude to this value relative
                           ;          to amplitude of previous free peak
                           ;   -1.0 = Fix amplitude at 0.D0
        initial_energy: 0.D, $ ; Peak energy
        energy:         0.D, $ ; Peak energy
        initial_fwhm:   0.D, $ ; Peak FWHM
        fwhm:           0.D, $ ; Peak FWHM
        initial_ampl:   0.D, $ ; Peak amplitude
        ampl:           0.D, $ ; Peak amplitude
        area:           0.D, $ ; Area of peak
        bgd:            0.D  $ ; Background under peak
    }
end
