function lookup_gamma_line, in_string

;+
; NAME:
;       LOOKUP_GAMMA_LINE
;
; PURPOSE:
;       This function returns the energy in keV for a particular gamma
;       emmission line.
;
; CATEGORY:
;       X-ray analysis routines
;
; CALLING SEQUENCE:
;       Result = LOOKUP_GAMMA_LINE(Gamma_Line)
;
; INPUTS:
;       Gamma_Line: A string of the form 'Isotope Line', where Isotope is a
;       the symbol for a radioactive isotope, and Line is an index of the form
;       g1, g2, ... gn.
;       Both Isotope and Line are case insensitive.  There must be a space
;       between Isotope and Line.
;
;       Examples of Gamma_Line:
;           'Cd109 g1'  - 88 keV line of Cd-109
;           'co57 g2'   - 122 keV line of Co-57
;
; OUTPUTS:
;       This function returns the gamma energy of the specified line.
;       If the input is invalid, e.g. non-existent isotope or line, then the
;       function returns 0.
;
; RESTRICTIONS:
;       This function only knows about a few isotopes at present.  It is
;       intended for use with common radioactive check sources.  It is easy to
;       add additional isotopes and gamma lines to this function as needed.
;       The current library is:
;           'CO57 G1' = 14.413
;           'CO57 G2' = 122.0614
;           'CO57 G3' = 136.4743
;           'CD109 G1'= 88.04
;
; EXAMPLE:
;       energy = LOOKUP_GAMMA_LINE('Co57 g1')  ; Look up 14 keV line of Co-57
;
; MODIFICATION HISTORY:
;       Written by:     Mark Rivers.  October 10, 1998
;-

    temp = {gamma_line_struct, $
            isotope: '', $
            line:    '', $
            energy:  0. $
           }

    all_lines = [{gamma_line_struct, 'CO57',  'G1', 14.413}, $
                 {gamma_line_struct, 'CO57',  'G2', 122.0614}, $
                 {gamma_line_struct, 'CO57',  'G3', 136.4743}, $
                 {gamma_line_struct, 'CD109', 'G1', 88.04} $
                ]

    temp = str_sep(in_string, ' ', /trim)
    if (n_elements(temp) lt 2) then return, 0.
    isotope = strupcase(temp[0])
    line = strupcase(temp[1])

    for i=0, n_elements(all_lines)-1 do begin
        if (isotope eq all_lines[i].isotope and $
            line eq all_lines[i].line) then return, all_lines[i].energy
    endfor
    return, 0.
end
