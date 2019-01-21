function lookup_jcpds_line, in_string, $
                            pressure=pressure, $
                            temperature=temperature, $
                            path = path
;+
; NAME:
;       LOOKUP_JCPDS_LINE
;
; PURPOSE:
;       This function returns the d-spacing in Angstroms for a particular
;       lattice plane.
;
; CATEGORY:
;       X-ray analysis routines
;
; CALLING SEQUENCE:
;       Result = LOOKUP_JCPDS_LINE(Diffraction_plane)
;
; INPUTS:
;       Diffaction_plane: A string of the form 'Compound HKL', where Compound
;       is the name of a material (e.g. 'gold', and HKL is the diffraction
;       plane (e.g. 220).
;       There must be a space between Compound and HKL.
;
;       Examples of Diffraction_plane:
;           'gold 111' - Gold 111 plane
;           'si 220'   - Silicon 220 plane
;
; KEYWORD PARAMETERS:
;       PATH:
;           The path in which to look for the file 'Compound.jcpds'.  The
;           default is to search in the directory pointed to by the
;           environment variable JCPDS_PATH.
;
;       PRESSURE:
;           The pressure at which to compute the d-spacing.  Not yet
;           implemented, zero pressure d-spacing is always returned.
;
;       TEMPERATURE:
;           The temperature at which to compute the d-spacing.  Not yet
;           implemented.  Room-temperature d-spacing is always returned.
;
; OUTPUTS:
;       This function returns the d-spacing of the specified lattice plane.
;       If the input is invalid, e.g. non-existent compound or plane, then the
;       function returns 0.
;
; RESTRICTIONS:
;       This function attempts to locate the file 'Compound.jcpds', where
;       'Compound' is the name of the material specified in the input parameter
;       'Diffraction_plane'.  For example:
;           d = LOOKUP_JCPDS_LINE('gold 220')
;       will look for the file gold.jcpds.  It will either look in the file
;       specified in the PATH keyword parameter to this function, or in the
;       the directory pointed to by the environtment variable JCPDS_PATH
;       if the PATH keyword is not specified.  Note that the filename will be
;       case sensitive on Unix systems, but not on Windows or VMS systems.
;
;       This function is currently only able to handle HKL values from 0-9.
;       The parser will need to be improved to handle 2-digit values of H,
;       K or L.
;
; PROCEDURE:
;       This function calls READ_JCPDS and searches for the specified HKL plane
;       and returns its d-spacing;
;
; EXAMPLE:
;       d = LOOKUP_JCPDS_LINE('gold 111')  ; Look up gold 111 line
;       d = LOOKUP_JCPDS_LINE('quartz 220') ; Look up the quartz 220 line
;
; MODIFICATION HISTORY:
;       Written by:     Mark Rivers.  October 10, 1998
;-
    if (n_elements(path) eq 0) then path = getenv('JCPDS_PATH')
    temp = str_sep(in_string, ' ', /trim)
    if (n_elements(temp) lt 2) then return, 0.
    file = temp[0]
;    nums = str_sep(temp[1], [ '(', ')', ' ', ',', '[', '[' ] )
    nums = str_sep(temp[1],  ' ')
    case n_elements(nums) of
        1: begin
            if (strlen(nums[0]) eq 3) then begin
                hkl = long(byte(nums[0])) - (byte('0'))[0]
            endif else return, 0.
           end
        3: begin
            hkl = long(nums)
           end
        else: return, 0.
    endcase

    full_file = path + file + '.jcpds'
    catch, error
    if (error ne 0) then return, 0.
    jcpds = obj_new('JCPDS')
    jcpds->read_file, full_file
    r = jcpds->get_reflections()
    for i=0, n_elements(r)-1 do begin
        if (r[i].h eq hkl[0]  and $
            r[i].k eq hkl[1]  and $
            r[i].l eq hkl[2]) then return, r[i].d0
    endfor
    return, 0.
end
