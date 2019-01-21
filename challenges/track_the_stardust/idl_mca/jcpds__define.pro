pro jcpds::read_file, file
;+
; NAME:
;       JCPDS::READ_FILE
;
; PURPOSE:
;       This procedure reads a JCPDS file into the JCPDS object.
;
; CATEGORY:
;       MCA object library
;
; CALLING SEQUENCE:
;       jcpds->READ_FILE, file
;
; INPUTS:
;       File:  The name of the file to read.
;
; OUTPUTS:
;       None.  The file is read into the JCPDS object itself.
;
; PROCEDURE:
;       This procedure read the JCPDS file.  There are several versions of the
;       formats used for JCPDS files.  Versions 1, 2 and 3 used a fixed
;       format, where a particular entry had to be in a specific location on
;       a specific line.  Versions 2 and 3 were used only by Dan Shim.
;       This routine can read these old files, but no new files should be
;       created in this format, they should be converted to Version 4.
;       Version 4 is a "keyword" driven format.  Each line in the file is of
;       the form:
;       KEYWORD: value
;       The order of the lines is not important, except that the first line of
;       the file must be "VERSION: 4".
;       The following keywords are currently supported:
;           COMMENT:    Any information describing the material, literature
;                       references, etc.  There can be multiple comment lines
;                       per file.
;           K0:         The bulk modulus in GPa.
;           K0P:        The change in K0 with pressure, for Birch-Murnaghan
;                       equation of state.  Dimensionless.
;           DK0DT:      The temperature derivative of K0, GPa/K.
;           DK0PDT:     The temperature derivative of K0P, 1/K.
;           SYMMETRY:   One of CUBIC, TETRAGONAL, HEXAGONAL, RHOMBOHEDRAL,
;                       ORTHORHOMBIC, MONOCLINIC or TRICLINIC
;           A:          The unit cell dimension A
;           B:          The unit cell dimension B
;           C:          The unit cell dimension C
;           ALPHA:      The unit cell angle ALPHA
;           BETA:       The unit cell angle BETA
;           GAMMA:      The unit cell angle GAMMA
;           VOLUME:     The unit cell volume
;           ALPHAT:     The thermal expansion coefficient, 1/K
;           DALPHADT:   The temperature derivative of the thermal expansion
;                       coefficient, 1/K^2
;           DIHKL:      For each reflection, the D spacing in Angstrom, the
;                       relative intensity (0-100), and the H, K, L indices.
;
;       This procedure calculates the D spacing of each relfection, using the
;       symmetry and unit cell parameters from the file.  It compares the
;       calculated D spacing with the input D spacing for each line.  If they
;       disagree by more than 0.1% then a warning message is printed.
;       The following is an example JCPDS file in the Version 4 format:
;           VERSION:  4
;           COMMENT: Alumina (JCPDS 0-173, EOS n/a)
;           K0:          194.000
;           K0P:           5.000
;           SYMMETRY: HEXAGONAL
;           A:            4.758
;           C:            12.99
;           VOLUME:        22.0640
;           ALPHAT:    2.000e-6
;           DIHKL:        3.4790      75.0   0   1   2
;           DIHKL:        2.5520      90.0   1   0   4
;           DIHKL:        2.3790      40.0   1   1   0
;           DIHKL:        2.0850     100.0   1   1   3
;           DIHKL:        1.7400      45.0   0   2   4
;           DIHKL:        1.6010      80.0   1   1   6
;           DIHKL:        1.4040      30.0   2   1   4
;           DIHKL:        1.3740      50.0   3   0   0
;           DIHKL:        1.2390      16.0   1   0  10
;
;       Note that B and ALPHA, BETA and GAMMA are not present, since they are
;       not needed for a hexagonal material, and will be simple ignorred if
;       they are present.
;
; EXAMPLE:
;       jcpds = obj_new('JCPDS')
;       jcpds->read_file, 'alumina.jcpds'
;       print, jcpds->get_reflections()
;
; MODIFICATION HISTORY:
;       Written by:     Mark Rivers, April 8, 2000.
;       16-May-2000 MLR  Increased maximum number of lines from 100 to 1000
;       27-Feb-2001 MLR  Added DK0PDT and DALPHADT.
;-
    ; Initialize variables
    self.file = file
    line = ''
    version = 0.
    ncomments = 0
    comment = strarr(100)
    nd = 0
    reflection = replicate({JCPDS_REFLECTION}, 1000)
    dtemp = fltarr(5)

    ; Determine what version JCPDS file this is
    ; In current files have the first line starts with the string VERSION:
    openr, lun, file, /get_lun
    readf, lun, line
    pos = strpos(line, ' ')
    tag = strupcase(strmid(line, 0, pos))
    value = strtrim(strmid(line, pos, 1000), 2)
    if (tag eq 'VERSION:') then begin
        self.version = value
        ; This is the current, keyword based version of JCPDS file
        while (not eof(lun)) do begin
            readf, lun, line
            pos = strpos(line, ' ')
            tag = strupcase(strmid(line, 0, pos))
            value = strtrim(strmid(line, pos, 1000), 2)
            case tag of
                'COMMENT:'  :  begin
                    comment[ncomments] = value
                    ncomments = ncomments + 1
                 end
                'K0:'  :  self.k0 = value
                'K0P:' :  self.k0p = value
                'DK0DT:'  :  self.dk0dt = value
                'DK0PDT:' :  self.dk0pdt = value
                'SYMMETRY:' : self.symmetry = strupcase(value)
                'A:'      :  self.a0 = value
                'B:'      :  self.b0 = value
                'C:'      :  self.c0 = value
                'ALPHA:'  :  self.alpha0 = value
                'BETA:'   :  self.beta0 = value
                'GAMMA:'  :  self.gamma0 = value
                'VOLUME:' :  self.v0 = value
                'ALPHAT:' :  self.alphat = value
                'DALPHADT:' :  self.dalphadt = value
                'DIHKL:'  :  begin
                    reads, value, dtemp
                    reflection[nd].d0 = dtemp[0]
                    reflection[nd].inten = dtemp[1]
                    reflection[nd].h = dtemp[2]
                    reflection[nd].k = dtemp[3]
                    reflection[nd].l = dtemp[4]
                    nd = nd + 1
                end
            endcase
        endwhile
    endif else begin

        ; This is an old format JCPDS file
        self.version = 1.
        header = ''
        temp = fltarr(5)
        comment[0] = line ; Read above
        readf, lun, temp
        ; The symmetry codes are as follows:
        ;   1 -- cubic
        ;   2 -- hexagonal
        if (temp[0] eq 1) then self.symmetry = 'CUBIC'
        if (temp[0] eq 2) then self.symmetry = 'HEXAGONAL'
        self.a0 = temp[1]
        self.k0 = temp[2]
        self.k0p = temp[3]
        c0a0 = temp[4]
        self.c0 = self.a0 * c0a0
        readf, lun, line  ; Ignore, just column labels
        ncomments = 1

        while (not eof(lun)) do begin
            readf, lun, dtemp
            reflection[nd].d0 = dtemp[0]
            reflection[nd].inten = dtemp[1]
            reflection[nd].h = dtemp[2]
            reflection[nd].k = dtemp[3]
            reflection[nd].l = dtemp[4]
            nd=nd+1
        endwhile
    endelse

    free_lun, lun
    comment = comment[0:ncomments-1]
    reflection = reflection[0:nd-1]

    self->compute_v0
    self.pcomment =  ptr_new(comment)
    self.a = self.a0
    self.b = self.b0
    self.c = self.c0
    self.alpha = self.alpha0
    self.beta = self.beta0
    self.gamma = self.gamma0
    self.v = self.v0
    self.preflection = ptr_new(reflection)
    ; Compute D spacings, make sure they are consistent with the input values
    self->compute_d
    r = self->get_reflections()
    diff = abs(r.d0 - r.d) / r.d0
    for i=0, n_elements(diff)-1 do begin
        if (diff[i] gt .001) then begin
            print, 'Reflection ' + $
                    strtrim(r[i].h,2) + ' ' + $
                    strtrim(r[i].k,2) + ' ' + $
                    strtrim(r[i].l,2) + $
                   ': calculated D (' + strtrim(r[i].d,2) + $
                   ') differs by more than 0.1% from input D (' + $
                    strtrim(r[i].d0,2) + ')'
        endif
    endfor
end


pro jcpds::write_file, file
;+
; NAME:
;       JCPDS::WRITE_FILE
;
; PURPOSE:
;       This procedure writes a JCPDS file from the JCPDS object.
;
; CATEGORY:
;       MCA object library
;
; CALLING SEQUENCE:
;       jcpds->WRITE_FILE, file
;
; INPUTS:
;       File:  The name of the file to written.
;
; OUTPUTS:
;       None.  The file is written from the information in the JCPDS object.
;
; PROCEDURE:
;       This procedure writes a JCPDS file.  It always writes files in the
;       current, keyword-driven format (Version 4).  See the documentation for
;       <A HREF="#JCPDS::READ_FILE">JCPDS::READ_FILE</A> for information on
;       the file format.
;
; EXAMPLE:
;       This reads an old format file, writes a new format file.
;       jcpds = obj_new('JCPDS')
;       jcpds->read_file,  'alumina_old.jcpds'
;       jcpds->write_file, 'alumina_new.jcpds'
;
; MODIFICATION HISTORY:
;       Written by:     Mark Rivers, April 8, 2000.
;-
    openw, lun, file, /get_lun
    printf, lun, format='(a, t11, i1)', 'VERSION:', 4
    comment = *self.pcomment
    for i=0, n_elements(comment)-1 do begin
        printf, lun, 'COMMENT: ', comment[i]
    endfor
    printf, lun, format='(a, t11, f10.3)', 'K0:', self.k0
    printf, lun, format='(a, t11, f10.3)', 'K0P:', self.k0p
    printf, lun, format='(a, t11, f10.3)', 'DK0DT:', self.dk0dt
    printf, lun, format='(a, t11, e10.3)', 'DK0PDT:', self.dk0pdt
    printf, lun, format='(a, t11, a)', 'SYMMETRY:', self.symmetry
    printf, lun, format='(a, t11, f10.4)', 'A:', self.a0
    printf, lun, format='(a, t11, f10.4)', 'B:', self.b0
    printf, lun, format='(a, t11, f10.4)', 'C:', self.c0
    printf, lun, format='(a, t11, f10.4)', 'ALPHA:', self.alpha0
    printf, lun, format='(a, t11, f10.4)', 'BETA:', self.beta0
    printf, lun, format='(a, t11, f10.4)', 'GAMMA:', self.gamma0
    printf, lun, format='(a, t11, f12.4)', 'VOLUME:', self.v0
    printf, lun, format='(a, t11, e10.3)', 'ALPHAT:', self.alphat
    printf, lun, format='(a, t11, e10.3)', 'DALPHADT:', self.dalphadt
    r = self->get_reflections()
    for i=0, n_elements(r)-1 do begin
        printf, lun, 'DIHKL:', format='(a, t11, f10.4, f10.1, i4, i4, i4)', $
                               r[i].d0, r[i].inten, r[i].h, r[i].k, r[i].l
    endfor
    free_lun, lun
end

pro jcpds::compute_v0

;+
; NAME:
;       JCPDS::COMPUTE_V0
;
; PURPOSE:
;       This procedure computes the unit cell volume of the material at zero
;       pressure and temperature from the unit cell parameters.
;
; CATEGORY:
;       MCA object library
;
; CALLING SEQUENCE:
;       jcpds->COMPUTE_V0
;
; OUTPUTS:
;       None.  The V0 information in the JCPDS object is calculated.
;
; PROCEDURE:
;       This procedure computes the unit cell volume from the unit cell
;       parameters.
;
; EXAMPLE:
;       Compute the zero pressure and temperature unit cell volume of alumina
;       jcpds = obj_new('JCPDS')
;       jcpds->read_file,  'alumina.jcpds'
;       jcpds->compute_V0
;
; MODIFICATION HISTORY:
;       Written by:     Mark Rivers, May 22, 2000
;-

    case self.symmetry of
        'CUBIC': begin
            self.b0 = self.a0
            self.c0 = self.a0
            self.alpha0 = 90.
            self.beta0 = 90.
            self.gamma0 = 90.
        end

        'TETRAGONAL': begin
            self.b0 = self.a0
            self.alpha0 = 90.
            self.beta0 = 90.
            self.gamma0 = 90.
        end

        'ORTHORHOMBIC': begin
            self.alpha0 = 90.
            self.beta0 = 90.
            self.gamma0 = 90.
        end

        'HEXAGONAL': begin
            self.b0 = self.a0
            self.alpha0 = 90.
            self.beta0 = 90.
            self.gamma0 = 120.
        end

        'RHOMBOHEDRAL':   begin
            self.b0 = self.a0
            self.c0 = self.a0
            self.beta0 = self.alpha0
            self.gamma0 = self.alpha0
        end

        'MONOCLINIC': begin
            self.alpha0 = 90.
            self.gamma0 = 90.
        end

        'TRICLINIC': begin
            dummy = self.alpha0
        end

    endcase
    self.v0 = self.a0 * self.b0 * self.c0 * $
                sqrt(1. - $
                cos(self.alpha0*!dtor)^2 - $
                cos(self.beta0*!dtor)^2  - $
                cos(self.gamma0*!dtor)^2 + $
                2.* cos(self.alpha0*!dtor) * $
                cos(self.beta0*!dtor) * $
                cos(self.gamma0*!dtor))
end

pro jcpds::compute_volume, pressure, temperature

;+
; NAME:
;       JCPDS::COMPUTE_VOLUME
;
; PURPOSE:
;       This procedure computes the unit cell volume of the material.
;       It can compute volumes at different pressures and temperatures.
;
; CATEGORY:
;       MCA object library
;
; CALLING SEQUENCE:
;       jcpds->COMPUTE_VOLUME, pressure, temperature
;
; OPTIONAL INPUTS:
;       Pressure:    The pressure in GPa.  If not present then the pressure is
;                    assumed to be 0.
;       Temperature: The temperature in K.  If not present or zero, then the
;                    temperature is assumed to be 298K, i.e. room temperature.
;
; OUTPUTS:
;       None.  The volume information in the JCPDS object is calculated.
;
; PROCEDURE:
;       This procedure computes the unit cell volume.  It starts with the
;       volume read from the JCPDS file or computed from the zero-pressure,
;       room temperature lattice constants.  It does the following:
;           1) Corrects K0 for temperature if DK0DT is non-zero.
;           2) Computes volume at zero-pressure and the specified temperature
;              if ALPHAT is non-zero.
;           3) Computes the volume at the specified pressure if K0 is non-zero.
;              The routine uses the IDL function FX_ROOT to solve the third
;              order Birch-Murnaghan equation of state. 
;
; EXAMPLE:
;       Compute the unit cell volume of alumina at 100 GPa and 2500 K.
;       jcpds = obj_new('JCPDS')
;       jcpds->read_file,  'alumina.jcpds'
;       jcpds->compute_volume, 100, 2500
;
; MODIFICATION HISTORY:
;       Written by:     Mark Rivers, April 8, 2000.  Original work by Tom
;                       Duffy.  Ideas from Dan Shim.
;       31-May-2000 MLR Issue warning if K0 is 0, but return 0 pressure volume
;       06-Sep-2000 MLR Change to Anderson thermal pressure EOS for high P+T
;       27-Feb-2001 MLR Add support for dk0pdt and dalphadt.
;       13-Mar-2001 MLR Put addtional sanity check (f ge 0.) in BM solver.  
;       17-Mar-2001 MLR Replaced home-grown solver for BM equation of state
;                       with IDL FX_ROOT function.  It is much faster, 
;                       more accurate, and more robust.
;-
    common bm3_common, mod_pressure, k0, k0p

    if (n_elements(pressure) eq 0) then pressure=0.
    if (n_elements(temperature) eq 0) then temperature=0.
    ; Assume 0 K really means room T
    if (temperature eq 0) then temperature=298.
    ; Compute values of K0, K0P and alphat at this temperature
    k0 = self.k0
    alphat = self.alphat + self.dalphadt*(temperature-298.)
    k0p = self.k0p + self.dk0pdt*(temperature-298.)

    if (pressure eq 0.) then begin
    	self.v = self.v0 * (1 + alphat*(temperature-298.))
    endif else begin
        if (k0 le 0.) then begin
            s = dialog_message('K0 is zero, computing zero pressure volume')
            self.v = self.v0
        endif else begin
            mod_pressure = pressure - alphat*k0*(temperature-298.)
            v0_v = fx_root([0.0,0.5,1.0], 'jcpds_bm3_inverse')
            self.v = self.v0/v0_v
        endelse
    endelse
end

function jcpds_bm3_inverse, v0_v
;+
; NAME:
;       JCPDS_BM3_INVERSE
;
; PURPOSE:
;       This function returns the value of the third order Birch-Murnaghan
;       equation minus pressure.  It is used to solve for V0/V for a given
;       P, K0 and K0'.
;
; CATEGORY:
;       MCA object library
;
; CALLING SEQUENCE:
;       result = JCPDS_BM3_INVERSE(V0_v, Pressure)
;
; INPUTS:
;       V0_v:   The ratio of the zero pressure volume to the high pressure
;               volume
;
; OUTPUTS:
;       This function returns the value of the third order Birch-Murnaghan
;       equation minus pressure.  
;
; COMMON BLOCKS:
;       The common block BM3_COMMON contains MOD_PRESSURE, K0 and K0P. The 
;       use of a common block is required to pass this information, because
;       FX_ROOT does not call the user function with any additional parameters.
;
; PROCEDURE:
;       This procedure simply computes the pressure using V0/V, K0 and K0',
;       and then subtracts the input pressure.
;
; EXAMPLE:
;       Compute the difference of the calculated pressure and 100 GPa for
;       V0/V=1.3 for alumina
;       jcpds = obj_new('JCPDS')
;       jcpds->read_file,  'alumina.jcpds'
;       common bm3_common mod_pressure, k0, k0p
;       mod_pressure=100
;       k0 = 100
;       k0p = 4.
;       diff = jcpds_bm3_inverse(1.3)
;
; MODIFICATION HISTORY:
;       Written by:     Mark Rivers, March 17, 2001.
;-
    common bm3_common, mod_pressure, k0, k0p

    return, 1.5*k0*(v0_v^(7./3.) - v0_v^(5./3.)) * $
            (1 + 0.75*(k0p - 4.) * (v0_v^(2./3.) - 1.0)) - mod_pressure
end


pro jcpds::compute_D, pressure, temperature
;+
; NAME:
;       JCPDS::COMPUTE_D
;
; PURPOSE:
;       This procedure computes the D spacings of the material.
;       It can compute D spacings at different pressures and temperatures.
;
; CATEGORY:
;       MCA object library
;
; CALLING SEQUENCE:
;       jcpds->COMPUTE_D, pressure, temperature
;
; OPTIONAL INPUTS:
;       Pressure:    The pressure in GPa.  If not present then the pressure is
;                    assumed to be 0.
;       Temperature: The temperature in K.  If not present or zero, then the
;                    temperature is assumed to be 298K, i.e. room temperature.
;
; OUTPUTS:
;       None.  The D spacing information in the JCPDS object is calculated.
;
; PROCEDURE:
;       This procedure first calls 
;       <A HREF="#JCPDS::COMPUTE_VOLUME">JCPDS::COMPUTE_VOLUME</A>.
;       It then assumes that each lattice dimension fractionally changes by
;       the cube root of the fractional change in the volume.
;       Using the equations for the each symmetry class it then computes the
;       change in D spacing of each reflection.
;
; EXAMPLE:
;       Compute the D spacings of alumina at 100 GPa and 2500 K.
;       jcpds = obj_new('JCPDS')
;       jcpds->read_file,  'alumina.jcpds'
;       jcpds->compute_d, 100, 2500
;       r = jcpds->get_reflections()
;       Print out the D spacings at ambient conditions
;       print, r.d0
;       Print out the D spacings at high pressure and temperature
;       print, r.d
;
; MODIFICATION HISTORY:
;       Written by:     Mark Rivers, April 8, 2000.
;-
    self->compute_volume, pressure, temperature

    ; Assumme each cell dimension changes by the same fractional amount = cube
    ; root of volume change ratio
    ratio = (self.v / self.v0)^(1.0/3.0)
    self.a = self.a0 * ratio
    self.b = self.b0 * ratio
    self.c = self.c0 * ratio

    a = self.a
    b = self.b
    c = self.c
    alpha = self.alpha * !dtor
    beta = self.beta * !dtor
    gamma = self.gamma * !dtor
    r = self->get_reflections()
    for i=0, n_elements(r)-1 do begin
        h = float(r[i].h)
        k = float(r[i].k)
        l = float(r[i].l)
        case self.symmetry of
        'CUBIC': d2inv = (h^2 + k^2 + l^2) / a^2

        'TETRAGONAL': d2inv = (h^2 + k^2) / a^2 + l^2 / c^2

        'ORTHORHOMBIC': d2inv = h^2 / a^2 + k^2 / b^2 + l^2 / c^2

        'HEXAGONAL': d2inv = (h^2 + h*k + k^2)*4./3./a^2 + l^2/c^2

        'RHOMBOHEDRAL': d2inv = ((1. + cos(alpha)) * ((h^2 + k^2 + l^2) - $
                                (1 - tan(0.5*alpha)^2)*(h*k + k*l + l*h))) / $
                                (a^2 * (1 + cos(alpha) - 2*cos(alpha)^2))

        'MONOCLINIC': d2inv = h^2 / sin(beta)^2 / a^2 + $
                              k^2 / b^2 + $
                              l^2 / sin(beta)^2 / c^2 + $
                              2 * h * l * cos(beta) / (a * c * sin(beta)^2)

        'TRICLINIC': begin
                        V = a^2 * b^2 * c^2 * $
                            (1. - cos(alpha)^2 - cos(beta)^2 - cos(gamma)^2 + $
                            2 * cos(alpha) * cos(beta) * cos(gamma))
                        s11 = b^2 * c^2 * sin(alpha)^2
                        s22 = a^2 * c^2 * sin(beta)^2
                        s33 = a^2 * b^2 * sin(gamma)^2
                        s12 = a * b * c^2 * $
                                (cos(alpha) * cos(beta) - cos(gamma))
                        s23 = a^2 * b * c * $
                                (cos(beta) * cos(gamma) - cos(alpha))
                        s31 = a * b^2 * c * $
                                (cos(gamma) * cos(alpha) - cos(beta))
                        d2inv = (s11 * h^2 + s22 * k^2 + s33 * l^2 + $
                             2.*s12*h*k + 2.*s23*k*l + 2.*s31*l*h) / V^2
                 end

        else:   message, 'Unknown crystal symmetry = ' + self.symmetry
        endcase
        (*self.preflection)(i).d = sqrt(1./d2inv)
    endfor
end


function jcpds::get_reflections
;+
; NAME:
;       JCPDS::GET_REFLECTIONS
;
; PURPOSE:
;       This function returns the information for each reflection for the
;       material.  This information is an array of structures of type
;       {JCPDS_REFLECTION}.
;
; CATEGORY:
;       MCA object library
;
; CALLING SEQUENCE:
;       r = jcpds->GET_REFLECTIONS()
;
; OUTPUTS:
;       This function returns an array of structures of type
;       {JCPDS_REFLECTION}.  The current definition of this structure is:
;       {JCPDS_REFLECTION, $
;           d0:    0., $  ; The lattice spacing at 0 pressure and 298K
;           d:     0., $  ; The lattice spacing computed by JCPDS::COMPUTE_D,
;                         ; at pressure and temperature.
;           inten: 0., $  ; The relative intensity of this reflection
;           h:     0, $   ; The lattice index H
;           k:     0, $   ; The lattice index K
;           l:     0 $    ; The lattice index L
;       }
;       Note that the definition of this structure may change in the future,
;       but these fields are guaranteed to always exist.
;
;       If there are no reflections present in this JCPDS object (for example,
;       if JCPDS::READ_FILE has not yet been called) then this function returns
;       -1.
;
; PROCEDURE:
;       This function simply returns the array of JCPDS_REFLECTION structures
;       from the JCPDS object.
;
; EXAMPLE:
;       Compute the D spacings of alumina at 100 GPa and 2500 K.
;       jcpds = obj_new('JCPDS')
;       jcpds->read_file,  'alumina.jcpds'
;       jcpds->compute_d, 100, 2500
;       r = jcpds->get_reflections()
;       Print out the D spacings at ambient conditions
;       print, r.d0
;       Print out the D spacings at high pressure and temperature
;       print, r.d
;
; MODIFICATION HISTORY:
;       Written by:     Mark Rivers, April 8, 2000.
;-
    if (ptr_valid(self.preflection)) then begin
        return, *self.preflection
    endif else begin
        return, -1
    endelse
end

pro jcpds__define
;+
; NAME:
;       JCPDS__DEFINE
;
; PURPOSE:
;       This function defines an object of type JCPDS.  It cannot be called
;       directly, but is invoked when an object of type JCPDS is created by
;       IDL.
;
; CATEGORY:
;       MCA object library
;
; CALLING SEQUENCE:
;       jcpds = obj_new('JCPDS')
;
; OUTPUTS:
;       OBJ_NEW creates and returns an object of type JCPDS.
;
; SIDE EFFECTS:
;       This procedure defines structures of type JCPDS and JCPDS_REFLECTION.
;
; EXAMPLE:
;       ; Create a JCPDS object and read a file into it.
;       jcpds = obj_new('JCPDS')
;       jcpds->read_file,  'alumina.jcpds'
;
; MODIFICATION HISTORY:
;       Written by:         Mark Rivers, April 8, 2000.
;       May 22, 2000 MLR    Added JCPDS::COMPUTE_V0 method
;-
    jcpds_reflection = {jcpds_reflection, $
        d0:    0., $
        d:     0., $
        inten: 0., $
        h:     0, $
        k:     0, $
        l:     0 $
    }
    jcpds = {jcpds, $
        file:     ' ', $
        version:  0., $
        pcomment: ptr_new(), $
        symmetry: '', $
        k0:       0., $
        k0p:      0., $
        dk0dt:    0., $
        dk0pdt:   0., $
        alphat:   0., $
        dalphadt: 0., $
        a0:       0., $
        b0:       0., $
        c0:       0., $
        alpha0:   0., $
        beta0:    0., $
        gamma0:   0., $
        v0:       0., $
        a:        0., $
        b:        0., $
        c:        0., $
        alpha:    0., $
        beta:     0., $
        gamma:    0., $
        v:        0., $
        preflection: ptr_new() $
    }
end
