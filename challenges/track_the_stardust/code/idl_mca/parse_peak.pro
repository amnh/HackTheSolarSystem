function parse_peak, string
;
; MN Feb 17 2000: separated from read_peaks.pro file --
; I wanted direct access to this function
    all_done=0
    peak = {MCA_PEAK}
    peak.label=' '
    q = strpos(string, ',')
    if q eq -1 then begin
        q=100
        all_done=1
    endif
    label = strmid(string, 0, q)
    on_ioerror, not_energy
    ; peak.energy = float(label)
    ; Note, there is a bug which causes an accesss violation on the above statement
    ; orks OK if a temporary variable is used instead
    ; There is another bug which causes error if the label string begins with
    ; E or D. It removes the first character from the string. Make a copy instead,
    temp = label
    temp = float(temp)
    peak.initial_energy = temp
    goto, finish_parse

    not_energy:
    on_ioerror, NULL
    peak.label = strtrim(label,2)
    peak.initial_energy = lookup_xrf_line(label)

    finish_parse:
    if all_done eq 1 then goto, all_done
    string = strmid(string, q+1, 100)
    t = strpos(string, ',')
    if t eq -1 then begin
        t=100
        all_done=1
    endif
    temp = strmid(string, 0, t)
    peak.energy_flag = float(temp)
    string =strmid(string, t+1, 100)
    if all_done eq 1 then goto, all_done

    t = strpos(string, ',')
    if t eq -1 then begin
        t=100
        all_done=1
    endif
    temp = strmid(string, 0, t)
    peak.initial_fwhm = float(temp)
    string =strmid(string, t+1, 100)
    if all_done eq 1 then goto, all_done

    t = strpos(string, ',')
    if t eq -1 then begin
    t=100
    all_done=1
    endif
    temp = strmid(string, 0, t)
    peak.fwhm_flag = float(temp)
    string =strmid(string, t+1, 100)
    if all_done eq 1 then goto, all_done

    t = strpos(string, ',')
    if t eq -1 then begin
        t=100
        all_done=1
    endif
    temp = strmid(string, 0, t)
    peak.ampl_factor = float(temp)
    if all_done eq 1 then goto, all_done

    string = strmid(string, t+1, 100)
    if (peak.label eq ' ') then peak.label = strtrim(string,2)

    all_done:
    return, peak
end
