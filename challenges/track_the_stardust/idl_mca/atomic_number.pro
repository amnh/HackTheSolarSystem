function atomic_number, symbol

;+
; NAME:
;       ATOMIC_NUMBER
;
; PURPOSE:
;       This function returns the atomic number of an element 
;
; CATEGORY:
;       IDL data analysis
;
; CALLING SEQUENCE:
;       Result = ATOMIC_NUMBER(Symbol)
;
; INPUTS:
;       Symbol: The atomic symbol of the element whose atomic number is being
;               requested.  This is a 1 or 2 character string, e.g. 'H', 'Si',
;               etc.  It is case insensitive and leading or trailing blanks
;               are ignorred.
;
; OUTPUTS:
;       This function returns the atomic number of the input element.  If an
;       Symbol is an invalid atomic symbol then the function returns 0.
;
; COMMON BLOCKS:
;       ATOMIC_SYMBOL_COMMON:  Holds an array of atomic_symbols.  Initialized
;       the first time this function or function ATOMIC_SYMBOL is called.
;
; EXAMPLE:
;       IDL> print, atomic_number('ag')
;       IDL>      47
;
; MODIFICATION HISTORY:
;       Written by:     Mark Rivers, October 23, 1998
;-

common atomic_symbol_common, atomic_symbols

; If the symbol table has not yet been initialized, then force it to be
if (n_elements(atomic_symbols) eq 0) then t = atomic_symbol(10)

str = strupcase(strtrim(symbol, 2))
pos = strpos(str, ' ')
if (pos gt 0) then str = strmid(str, 0, pos)

z = where((str eq strupcase(strtrim(atomic_symbols,2))), count)
if (count le 0) then return, 0 else return, z(0)+1
end
