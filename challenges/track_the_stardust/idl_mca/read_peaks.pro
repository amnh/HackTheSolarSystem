function read_peaks, file, background
;+
; NAME:
;       READ_PEAKS
;
; PURPOSE:
;       This function reads a disk file into an array of structures of type 
;       {MCA_PEAK}.  This routine is typically called before calling 
;       <A HREF="#FIT_PEAKS">FIT_PEAKS</A> or 
;       <A HREF="mca_class.html#FIT_PEAKS">MCA::FIT_PEAKS</A>.
;       The routine also returns a structure of type {MCA_BACKGROUND}.  This structure
;       may or may not actually be defined in the file (older peaks files lacked it),
;       but a reasonable default value will always be returned.
;
; CATEGORY:
;       MCA data analysis
;
; CALLING SEQUENCE:
;       Result = READ_PEAKS(File, Background)
;
; INPUTS:
;       File:  
;           The name of a disk file containing the peak definitions.
;
; OUTPUTS:
;       This function returns an array of structures of type {MCA_PEAK}.
;
;       Background:
;           A structure of type {MCA_BACKGROUND} containing the background fitting
;           parameters.
;
; PROCEDURE:
;       The input file is used to construct an array of structures of type
;       {MCA_PEAK}, which contains the parameters for peaks to be fitted. 
;       The exact definition of this structure is subject to change.  However, 
;       the following fields will always exist and are the fields which are set
;       by this function. 
;       Further information on many of these fields can be found in the
;       documentation for <A HREF="#FIT_PEAKS">FIT_PEAKS</A>.
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
;       The format of the disk file is as follows:
;           - Lines containing the parameters for the background (fields in
;             {MCA_BACKGROUND} structure) have the following format:
;                   Background_exponent, 4
;                   Background_top_width, 0
;                   Background_bottom_width, 4
;                   Background_tangent, 0
;                   Background_compress, 8
;
;           - There is one line in the file for each peak
;
;           - Each line consists of the following fields, separated by commas:
;               energy, energy_flag, fwhm, fwhm_flag, ampl_factor, label
;
;           - All fields except the first, "energy", are optional and default
;             values of 0 or blank.
;           - Field descriptions
;               - energy:       This field can either be an energy in keV or a
;                               string which can be parsed by 
;                                <A HREF="#LOOKUP_XRF_LINE">LOOKUP_XRF_LINE</A> 
;                               or <A HREF="#LOOKUP_GAMMA_LINE">LOOKUP_GAMMA_LINE</A>.
;                               The energy in keV of the peak is put in the 
;                               .energy value for the peak.
;               - energy_flag:  The .energy_flag value for the peak. 0 or 1.
;               - fwhm:         The .initial_fwhm value for the peak.  This 
;                               can be 0 if the .fwhm_flag is 0.
;               - fwhm_flag:    The .fwhm_flag value for the peak. 0, 1 or 2.
;               - ampl_factor:  The .ampl_factor for the peak.
;               - label:        The .label string for the peak.  If the energy
;                               field is a string, and the label field is blank
;                               then the energy string will be put in .label.
;
;       The following is an example peak file. Most peaks in this example use 
;       the default (0) values for the energy_flag, fwhm_flag and 
;       amplitude_factor fields.
;
;           4.660,,,,,Fe escape  ! This peak uses a numeric energy value
;           Fe Ka                ! This peak uses a string energy value
;           Fe Kb,,,,.5          ! Fe Kb is constrained to be 0.5 of Fe Ka
;           Ni Ka                ! These peaks energies are found with
;           Ni Kb                !  LOOKUP_XRF_LINE
;           Co57 G1              ! These peak energies are found with
;           Cd109 G2             !  LOOKUP_GAMMA_LINE
;           15.9,1,.3,1,,Diffraction ! Fit both the energy and fwhm of this peak
;           17.443,1,,,,Unknown  ! Fit the energy, but not fwhm, of this peak
;
;
;
; EXAMPLE:
;       Peaks = READ_PEAKS('mypeaks.pks')
;
; MODIFICATION HISTORY:
;       Written by:     Mark Rivers, October 23, 1998
;       Feb 17, 2000:   Matt Newville.  Separated parse_peaks function to
;                       separate file -- I wanted direct access to that function
;       Jan 9, 2001:    Mark Rivers.  Added Background output parameter since files
;                       can now contain background fitting parameters as well as
;                       peaks.
;-

    openr, lun, file, /get
    on_ioerror, finish
    background = {MCA_BACKGROUND}
    background.exponent=2
    background.top_width=0
    background.bottom_width=4
    background.tangent=0
    background.compress=4
    while (1) do begin
        string = ""
        readf, lun, string
        q = strpos(string, ',')
        if (q gt 0) then begin
            label = strupcase(strmid(string, 0, q))
            case label of
                "BACKGROUND_EXPONENT": background.exponent = strmid(string, q+1, 100)
                "BACKGROUND_TOP_WIDTH": background.top_width = strmid(string, q+1, 100)
                "BACKGROUND_BOTTOM_WIDTH": $
                                     background.bottom_width = strmid(string, q+1, 100)
                "BACKGROUND_TANGENT": background.tangent = strmid(string, q+1, 100)
                "BACKGROUND_COMPRESS": background.compress = strmid(string, q+1, 100)
                else: begin
                    peak = parse_peak(string)
                    if (n_elements(peaks) eq 0) then $
                                    peaks = peak else peaks=[peaks, peak]
                end
            endcase
        endif
    endwhile
    finish:
    free_lun, lun
    return, peaks
end
