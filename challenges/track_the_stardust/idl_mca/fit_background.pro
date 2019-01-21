function fit_background, input_data, input_slope, $
        bottom_width = bottom_width, $
        top_width = top_width, $
        exponent = exponent, $
        tangent = tangent, $
        compress = compress

;+
; NAME:
;       FIT_BACKGROUND
;
; PURPOSE:
;       This function fits a background to an MCA spectrum.
;       The background is fitted using an enhanced version of the algorithm
;       published by Kajfosz, J. and Kwiatek, W .M. (1987)  "Non-polynomial
;       approximation of background in x-ray spectra." Nucl. Instrum. Methods
;       B22, 78-81.
;
; CATEGORY:
;       Spectra data fitting
;
; CALLING SEQUENCE:
;       Result = FIT_BACKGROUND(Data, Slope)
;
; INPUTS:
;       Data:   A 1-D array which contains the input spectrum to be fitted.
;
;       Slope:  The energy calibration slope, in user units/channel.  This
;               value is needed because the widths are specified in user units.
;
; KEYWORD PARAMETERS:
;       TOP_WIDTH:
;           Specifies the width of the polynomials which are concave upward.
;           The top_width is the full width in energy units at which the
;           magnitude of the polynomial is 100 counts. The default is 0, which
;           means that concave upward polynomials are not used.
;
;       BOTTOM_WIDTH:
;           Specifies the width of the polynomials which are concave downward.
;           The bottom_width is the full width in energy units at which the
;           magnitude of the polynomial is 100 counts. The default is 4.
;
;       EXPONENT:
;           Specifies the power of polynomial which is used. The power must be
;           an integer. The default is 2, i.e. parabolas. Higher exponents,
;           for example EXPONENT=4, results in polynomials with flatter tops
;           and steeper sides, which can better fit spectra with steeply
;           sloping backgrounds.
;
;       TANGENT
;           Specifies that the polynomials are to be tangent to the slope of the
;           spectrum. The default is vertical polynomials. This option works
;           best on steeply sloping spectra. It has trouble in spectra with
;           big peaks because the polynomials are very tilted up inside the
;           peaks.
;
;       COMPRESS:
;           Compression factor to apply before fitting the background.
;           Default=4, which means, for example, that a 2048 channel spectrum
;           will be rebinned to 512 channels before fitting.
;           The compression is done on a temporary copy of the input spectrum,
;           so the input spectrum itself is unchanged.
;           The algorithm works best if the spectrum is compressed before it
;           is fitted. There are two reasons for this. First, the background
;           is constrained to never be larger than the data itself. If the
;           spectrum has negative noise spikes they will cause the fit to be
;           too low. Compression will smooth out such noise spikes.
;           Second, the algorithm requires about 3*N^2 operations, so the time
;           required grows rapidly with the size of the input spectrum. On a
;           200 MHz Pentium it takes about 3 seconds to fit a 2048 channel
;           spectrum with COMPRESS=1 (no compression), but only 0.2 seconds
;           with COMPRESS=4 (the default).
;
; OUTPUTS:
;       This function returns a 1-D array which contains the background 
;       fitted to the input data.
;
; PROCEDURE:
;       1) At each channel "i" an n'th degree polynomial which is concave up
;       is fitted. Its equation is
;
;                                     n
;                        (e(i) - e(j))
;       f(j,i) = y(i) + --------------
;                                  n
;                         top_width
;
;       where f(j,i) is the fitted counts in channel j for the polynomial
;       centered in channel i. y(i) is the input counts in channel "i", e(i) is
;       the energy of channel i, e(j) is the energy of channel j, and
;       "top_width" and "n" are user-specified parameters. The background count
;       in channel "j", b(j) is defined as
;
;       b(j) = min ((f(j,i), y(j))
;               i
;
;       b(j) is thus the smallest fitted polynomial in channel j, or the raw
;       data, whichever is smaller.
;
;       2) After the concave up polynomials have been fitted, a series of
;       concave down polynomials are constructed. At each channel "i" an n'th
;       degree polynomial which is concave up is fitted. The polynomial is slid
;       up from below until it "just touches" some channel of the spectrum. Call
;       this channel "i". The maximum height of the polynomial is thus
;
;                                                n
;                                   (e(i) - e(j))
;       height(j) = max ( b(j) +  --------------  )
;                    i                          n
;                                   bottom_width
;
;       where bottom_width is a user_specified parameter.
;
;       3) Once the value of height(i) is known the polynomial is fitted. The
;       background counts in each channel are then determined from:
;
;                                                n
;                                   (e(i) - e(j))
;       bgd(j) = max ( height(i) + --------------
;                 i                             n
;                                   bottom_width
;
;       bgd(j) is thus the maximum counts for any of the concave down
;       polynomials passing though channel j.
;
;       Before the concave-down polynomials are fitted the spectrum at each
;       channel it is possible to subtract out a straight line which is
;       tangent to the spectrum at that channel. Use the /TANGENT qualifier to
;       do this. This is equivalent to fitting a "tilted" polynomial whose
;       apex is tangent to the spectrum at that channel. By fitting
;       polynomials which are tangent rather than vertical the background fit
;       is much improved on spectra with steep slopes.
;
; EXAMPLE:
;       mca = obj_new('MCA')
;       mca->read_file, 'my_file.dat'
;       data = mca->get_data()
;       cal = mca->get_calibration()
;       slope = cal.slope
;       back = fit_background(data, slope, bottom=6, exponent=4)
;       plot, data
;       oplot, back
;
;       Note:  This example is provided for reference.  In practice it is
;              simpler to use the MCA object method MCA::FIT_BACKGROUND.
;              because is handles much of the tedious bookkeeping the above 
;              example.  However, it is important to recognize that
;              FIT_BACKGROUND is independent of the MCA class, and
;              can be used without the MCA class library
;
; MODIFICATION HISTORY:
;       Written by:     Mark Rivers, October 19, 1998
;-

    REFERENCE_AMPL=100.
    TINY = 1.E-20
    HUGE = 1.E20
    MAX_TANGENT=2

    if (n_elements(tangent) eq 0) then tangent =  0
    if (n_elements(compress) eq 0) then compress = 4
    if (n_elements(exponent) eq 0) then exponent =  2.
    if (n_elements(top_width) eq 0) then top_width =  0.
    if (n_elements(bottom_width) eq 0) then bottom_width =  4.

    scratch = input_data
    nchans = n_elements(scratch)
    slope = input_slope

    ;   Compress scratch spectrum
    if (compress gt 1) then begin
        scratch = rebin(scratch, n_elements(scratch)/compress)
        slope = slope * compress
        nchans = nchans / compress
    endif

    ;   Copy scratch spectrum to background spectrum
    bckgnd = scratch

    ;   Find maximum counts in input spectrum. This information is used to
    ;   limit the size of the function lookup table
    max_counts = max(scratch)

    ;   Fit functions which come down from top
    if (top_width gt 0.) then begin
        ;   First make a lookup table of this FUNCTION
        chan_width = top_width / (2. * slope)
        denom = chan_width^exponent
        indices = findgen(nchans*2+1) - nchans
        power_funct = indices^exponent * (REFERENCE_AMPL / denom)
        good = where(power_funct le max_counts)
        power_funct = power_funct[good]
        max_index = n_elements(power_funct)/2 - 1

        for center_chan = 0, nchans-1 do begin
            first_chan = (center_chan - max_index) > 0
            last_chan = (center_chan + max_index) < (nchans-1)
            f = first_chan - center_chan + max_index
            l = last_chan - center_chan + max_index
            test = scratch[center_chan] + power_funct[f:l]
            bckgnd[first_chan:last_chan] = bckgnd[first_chan:last_chan] > test
        endfor
    endif

;   Copy this approximation of background to scratch
    scratch = bckgnd

;   Find maximum counts in scratch spectrum. This information is used to
;   limit the size of the function lookup table
    max_counts = max(scratch)

;   Fit functions which come up from below
    bckgnd = findgen(nchans) - HUGE

;   First make a lookup table of this function
    chan_width = bottom_width / (2. * slope)
    if (chan_width eq 0.) then denom = TINY else denom = chan_width^exponent
    indices = findgen(nchans*2+1) - nchans
    power_funct = indices^exponent  * (REFERENCE_AMPL / denom)
    good = where(power_funct le max_counts)
    power_funct = power_funct[good]
    max_index = n_elements(power_funct)/2 - 1

    for center_chan=0, nchans-2 do begin
        tangent_slope = 0.
        if (tangent) then begin
            ; Find slope of tangent to spectrum at this channel
            first_chan = (center_chan - MAX_TANGENT) > 0
            last_chan = (center_chan + MAX_TANGENT) < (nchans-1)
            denom = center_chan - findgen(last_chan - first_chan + 1)
            tangent_slope = (scratch[center_chan] - $
                             scratch[first_chan:last_chan]) / (denom>1)
            tangent_slope = total(tangent_slope) / (last_chan - first_chan)

        endif

        first_chan = (center_chan - max_index) > 0
        last_chan = (center_chan + max_index) < (nchans-1) 
        nc = last_chan - first_chan + 1
;{mn 14-apr-99: having problemswith med calibration -- sems that nc can be zero? 
;        nc = (last_chan - first_chan + 1) > 1 
;        print, 'Fit Background = ', last_chan, first_chan, $
;               center_chan, nc , max_index, nchans
; mn}
        last_chan = last_chan > first_chan
        nc        = last_chan - first_chan + 1
;;   print, ' Debug: nc , center, first, last ', $
;;        nc, center_chan, first_chan, last_chan
        lin_offset = scratch[center_chan] + $
                (findgen(nc) - nc/2) * tangent_slope

        ; Find the maximum height of a function centered on this channel
        ; such that it is never higher than the counts in any channel

        f = first_chan - center_chan + max_index
        l = last_chan - center_chan + max_index

        test = scratch[first_chan:last_chan] - lin_offset + power_funct[f:l]
        height = min(test)

        ; We now have the FUNCTION height. Set the background to the
        ; height of the maximum function amplitude at each channel

        test = height + lin_offset - power_funct(f:l)
        bckgnd[first_chan:last_chan] = bckgnd[first_chan:last_chan] > test
    endfor

    ; Expand spectrum
    if (compress gt 1) then begin
        bckgnd = rebin(bckgnd, nchans * compress)
    endif

    return, bckgnd
end



