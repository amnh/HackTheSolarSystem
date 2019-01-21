function get_font_name, $
        helvetica=helvetica, times=times, courier=courier, $
        tiny=tiny, small=small, medium=medium, large=large, huge=huge, $
        size=size, $
        bold=bold, italic=italic, $
        dpi75=dpi75, dpi100=dpi100

; Returns the name of the font with the specified characteristics

; This routine should be Window system specific.  For now these are Motif fonts

if (!version.os_family eq 'Windows') then begin
    font = ''
    if keyword_set(helvetica) then font = font + 'Helvetica' else $
    if keyword_set(times)     then font = font + 'Times' else $
    if keyword_set(courier)   then font = font + 'Courier New' else $
    font = font + 'MS San Serif'

    if keyword_set(bold) then font = font + '*Bold'
    if keyword_set(italic) then font = font + '*Italic'
    if keyword_set(tiny)   then size=0
    if keyword_set(small)  then size=1
    if keyword_set(medium) then size=2
    if keyword_set(large)  then size=3
    if keyword_set(huge)   then size=4
    if (n_elements(size) eq 0) then size=2
    font_size_strings = ['12', '14', '16', '18', '20']
    size = (size > 0) < (n_elements(font_size_strings)-1)
    font = font + '*' + font_size_strings(size)
    return, font
endif else if (!version.os_family eq 'Mac') then begin
   font='Helvetica'
   return, font
endif else begin  ; Assume Motif
    font = '-adobe-'
    if keyword_set(helvetica) then font = font + 'helvetica-' else $
    if keyword_set(times)     then font = font + 'times-' else $
    if keyword_set(courier)   then font = font + 'courier-' else $
                                   font = font + 'helvetica-'

    if keyword_set(bold) then font = font + 'bold-' else font = font + 'medium-'
    if keyword_set(italic) then font = font + 'o-' else font = font + 'r-'
    font = font + 'normal--*-'

    if keyword_set(tiny)   then size=0
    if keyword_set(small)  then size=1
    if keyword_set(medium) then size=2
    if keyword_set(large)  then size=3
    if keyword_set(huge)   then size=4
    if (n_elements(size) eq 0) then size=2
    font_size_strings = ['80-', '100-', '120-', '140-', '180-']
    size = (size > 0) < (n_elements(font_size_strings)-1)
    font = font + font_size_strings(size)

    if keyword_set(dpi100) then font = font + '100-100-' else $
    if keyword_set(dpi75) then font = font + '75-75-' else $
    font = font + '*-*-'
    font = font + '*-*-iso8859-1'
return, font
endelse

end

