function atomic_symbol, z

;+
; NAME:
;       ATOMIC_SYMBOL
;
; PURPOSE:
;       This function returns the atomic symbol of an element 
;
; CATEGORY:
;       IDL data analysis
;
; CALLING SEQUENCE:
;       Result = ATOMIC_SYMBOL(Z)
;
; INPUTS:
;       Z:  The atomic number of the element whose atomic symbol is being
;           requested.  
;
; OUTPUTS:
;       This function returns the atomic symbol of the input element as a
;       string.  If Z is an invalid atomic number then the function returns 
;       a null string.
;
; COMMON BLOCKS:
;       ATOMIC_SYMBOL_COMMON:  Holds an array of atomic_symbols.  Initialized
;       the first time this function or function ATOMIC_NUMBER is called.
;
; EXAMPLE:
;       IDL> print, atomic_symbol(47)
;       IDL> Ag
;
; MODIFICATION HISTORY:
;       Written by:     Mark Rivers, October 23, 1998
;-

common atomic_symbol_common, atomic_symbols

if (n_elements(atomic_symbols) eq 0) then begin
  atomic_symbols = [                                                           $
 'H',  'He', 'Li', 'Be', 'B',  'C',  'N',  'O',  'F ', 'Ne', 'Na', 'Mg', 'Al', $
 'Si', 'P',  'S',  'Cl', 'Ar', 'K',  'Ca', 'Sc', 'Ti', 'V',  'Cr', 'Mn', 'Fe', $
 'Co', 'Ni', 'Cu', 'Zn', 'Ga', 'Ge', 'As', 'Se', 'Br', 'Kr', 'Rb', 'Sr', 'Y',  $
 'Zr', 'Nb', 'Mo', 'Tc', 'Ru', 'Rh', 'Pd', 'Ag', 'Cd', 'In', 'Sn', 'Sb', 'Te']
  atomic_symbols = [atomic_symbols,                                            $
 'I',  'Xe', 'Cs', 'Ba', 'La', 'Ce', 'Pr', 'Nd', 'Pm', 'Sm', 'Eu', 'Gd', 'Tb', $
 'Dy', 'Ho', 'Er', 'Tm', 'Yb', 'Lu', 'Hf', 'Ta', 'W',  'Re', 'Os', 'Ir', 'Pt', $
 'Au', 'Hg', 'Tl', 'Pb', 'Bi', 'Po', 'At', 'Rn', 'Fr', 'Ra', 'Ac', 'Th', 'Pa', $
 'U',  'Np', 'Pu', 'Am', 'Cm', 'Bk', 'Cf', 'Es', 'Fm']
endif
if ((z lt 1) or (z gt n_elements(atomic_symbols))) then return, ''
return, atomic_symbols(z-1)
end
