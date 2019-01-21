function lookup_xrf_line, in_str

;+
; NAME:
;       LOOKUP_XRF_LINE
;
; PURPOSE:
;       This function returns the energy in keV for a particular x-ray
;       fluorescence line.
;
; CATEGORY:
;       X-ray analysis routines
;
; CALLING SEQUENCE:
;       Result = LOOKUP_XRF_LINE(XRF_Line)
;
; INPUTS:
;       XRF_LINE: A string of the form 'Element Line', where Element is an
;       atomic symbol, and Line is an acronym for a fluorescence line.
;       Both Element and Line are case insensitive.  There must be a space
;       between Element and Line.
;       The valid lines are
;           ka  - K-alpha (weighted average of ka1 and ka2)
;           ka1 - K-alpha 1
;           ka2 - K-alpha 2
;           kb  - K-beta (weighted average of kb1 and kb2)
;           kb1 - K-beta 1
;           kb2 - K-beta 2
;           la1 - L-alpha 1
;           lb1 - L-beta 1
;           lb2 - L-beta 2
;           lg1 - L-gamma 1
;           lg2 - L-gamma 2
;           lg3 - L-gamma 3
;           lg4 - L-gamma 4
;           ll  - L-eta
;
;       Examples of XRF_Line:
;           'Fe Ka'  - Fe k-alpha
;           'sr kb2' - Sr K-beta 2
;           'pb lg2' - Pb L-gamma 2
;
; OUTPUTS:
;       This function returns the fluoresence energy of the specified line.
;       If the input is invalid, e.g. non-existent element or line, then the
;       function returns 0.
;
; COMMON BLOCKS:
;       LOOKUP_XRF_COM: This common block is used to store the element line
;       table after reading it in.
;
; RESTRICTIONS:
;       This function uses the environment variable XRF_PEAK_LIBRARY to locate
;       the file containing the database of XRF lines.  This environment
;       variable must be defined to use this function.  On Unix systems
;       this is typically done with a command like
;       setenv XRF_PEAK_LIBRARY
;                   /usr/local/idl_user/epics/mca/xrf_peak_library.txt
;       On Windows NT systems this is typically done with the
;               Settings/Control Panel/System/Environment control.
;
; PROCEDURE:
;       The first time this function is called it reads in the table from the
;       file pointed to by the environment variable.  On all calls it looks
;       up the element using function ATOMIC_NUMBER and searches for the
;       specified line in the table.  It returns 0. if the element or line
;       are invalid.
;
; EXAMPLE:
;       energy = LOOKUP_XRF_LINE('Fe Ka')  ; Look up iron k-alpha line
;       energy = LOOKUP_XRF_LINE('Pb lb1') ; Look up lead l-beta 1 line
;
; MODIFICATION HISTORY:
;       Written by:     Mark Rivers.  October 10, 1998
;-

common lookup_xrf_com, eng_arr, line_list, elem_list

str = strtrim(in_str, 2)

nels = 100   ; Number of elements in table
nlines = 14  ; Number of x-ray lines in table

on_error, 2
; Read in the data table if it has not already been done
if n_elements(eng_arr) eq 0 then begin
  dummy1 = ' '
  dummy2 = ' '
  temp = fltarr(nlines)
  line_list = strarr(nlines)
  elem_list = strarr(nels)
  temp = fltarr(nlines)
  eng_arr = fltarr(nlines, nels)

  get_lun, table_lun
  file = getenv('XRF_PEAK_LIBRARY')
  openr, table_lun, file
  readf, table_lun, format = '(16A8)', dummy1, dummy2, line_list
  line_list = strupcase(strtrim(line_list,2))
  for i=0, nels-1 do begin
    readf, table_lun, format = '(2A8,14f8.0)',dummy1,dummy2,temp
    elem_list(i) = strupcase(strtrim(dummy2,2))
    eng_arr(0,i) = temp
  endfor
endif

; Parse the input string

pos = strpos(str, ' ')
if (pos le 0) then return, 0.0  ; No blank in string
symbol = strmid(str, 0, pos)
z = atomic_number(symbol)
if (z le 0) then return, 0.0    ; Not a valid atomic symbol
line = strmid(str, pos+1, 100)

line_index = where((strupcase(strtrim(line,2)) eq line_list), count)
if (count le 0) then return, 0.0
line_index = line_index(0)

return, eng_arr(line_index, z-1)

end
