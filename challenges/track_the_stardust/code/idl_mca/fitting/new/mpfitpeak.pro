;+
; NAME:
;   MPFITPEAK
;
; AUTHOR:
;   Craig B. Markwardt, NASA/GSFC Code 662, Greenbelt, MD 20770
;   craigm@lheamail.gsfc.nasa.gov
;   UPDATED VERSIONs can be found on my WEB PAGE: 
;      http://cow.physics.wisc.edu/~craigm/idl/idl.html
;
; PURPOSE:
;   Fit a gaussian, lorentzian or Moffat model to data
;
; MAJOR TOPICS:
;   Curve and Surface Fitting
;
; CALLING SEQUENCE:
;   yfit = MPFITPEAK(X, Y, A, NTERMS=nterms, ...)
;
; DESCRIPTION:
;
;   MPFITPEAK fits a gaussian, lorentzian or Moffat model using the
;   non-linear least squares fitter MPFIT.  MPFITPEAK is meant to be a
;   drop-in replacement for IDL's GAUSSFIT function (and requires
;   MPFIT and MPFITFUN).
;
;   The choice of the fitting function is determined by the keywords
;   GAUSSIAN, LORENTZIAN and MOFFAT.  By default the gaussian model
;   function is used.  [ The Moffat function is a modified Lorentzian
;   with variable power law index. (Moffat, A. F. J. 1969, Astronomy &
;   Astrophysics, v. 3, p. 455-461) ]
;
;   The functional form of the baseline is determined by NTERMS and
;   the function to be fitted.  NTERMS represents the total number of
;   parameters, A, to be fitted.  The functional forms and the
;   meanings of the parameters are described in this table:
;
;                 GAUSSIAN#          Lorentzian#         Moffat#
;
;   Model     A(0)*exp(-0.5*u^2)   A(0)/(u^2 + 1)   A(0)/(u^2 + 1)^A(3)
;
;   A(0)         Peak Value          Peak Value        Peak Value
;   A(1)        Peak Centroid       Peak Centroid     Peak Centroid
;   A(2)       Gaussian Sigma           HWHM%             HWHM%
;   A(3)         + A(3)    *          + A(3)   *      Moffat Index
;   A(4)         + A(4)*x  *          + A(4)*x *         + A(4)   *
;   A(5)                                                 + A(5)*x *
;
;   Notes: # u = (x - A(1))/A(2)
;          % Half-width at half maximum
;          * Optional depending on NTERMS
;
;   By default the initial starting values for the parameters A are
;   estimated from the data.  However, explicit starting values can be
;   supplied using the ESTIMATES keyword.  Also, error or weighting
;   values can optionally be provided; otherwise the fit is
;   unweighted.
;
;   MPFITPEAK fits the peak value of the curve.  The area under a
;   gaussian peak is A(0)*A(2)*SQRT(2*!DPI); the area under a
;   lorentzian peak is A(0)*A(2)*!DPI.
;
; RESTRICTIONS:
;
;   If no starting parameter ESTIMATES are provided, then MPFITPEAK
;   attempts to estimate them from the data.  This is not a perfect
;   science; however, the author believes that the technique
;   implemented here is more robust than the one used in IDL's
;   GAUSSFIT.  The author has tested cases of strong peaks, noisy
;   peaks and broad peaks, all with success.
;
;   Users should be aware that if the baseline term contains a strong
;   linear component then the automatic estimation may fail.  For
;   automatic estimation to work the peak amplitude should dominate
;   over the the maximum baseline.
;
; INPUTS:
;   X - Array of independent variable values, whose values should
;       monotonically increase.
;
;   Y - Array of "measured" dependent variable values.  Y should have
;       the same data type and dimension as X.
;
;
; OUTPUTS:
;   A - Upon return, an array of NTERMS best fit parameter values.
;       See the table above for the meanings of each parameter
;       element.
;
;
; RETURNS:
;
;   Returns the best fitting model function.
;
; KEYWORDS:
;
;   ** NOTE ** Additional keywords such as PARINFO, BESTNORM, and
;              STATUS are accepted by MPFITPEAK but not documented
;              here.  Please see the documentation for MPFIT for the
;              description of these advanced options.
;
;   ERROR - upon input, the measured 1-sigma uncertainties in the "Y"
;           values.  If no ERROR or WEIGHTS are given, then the fit is
;           unweighted.
;
;   ESTIMATES - Array of starting values for each parameter of the
;               model.  The number of parameters should at least be
;               three (four for Moffat), and if less than NTERMS, will
;               be extended with zeroes.
;               Default: parameter values are estimated from data.
;
;   GAUSSIAN - if set, fit a gaussian model function.  The Default.
;   LORENTZIAN - if set, fit a lorentzian model function.
;   MOFFAT - if set, fit a Moffat model function.
;
;   NEGATIVE / POSITIVE - if set, and ESTIMATES is not provided, then
;                         MPFITPEAK will assume that a
;                         negative/positive peak is present.
;                         Default: determined automatically
;
;   NTERMS - An integer describing the number of fitting terms.
;            NTERMS must have a minimum value, but can optionally be
;            larger depending on the desired baseline.  
;
;            For gaussian and lorentzian models, NTERMS must be three
;            (zero baseline), four (constant baseline) or five (linear
;            baseline).  Default: 4
;
;            For the Moffat model, NTERMS must be four (zero
;            baseline), five (constant baseline), or six (linear
;            baseline).  Default: 5
;
;   PERROR - upon return, the 1-sigma uncertainties of the parameter
;            values A.  These values are only meaningful if the ERRORS
;            or WEIGHTS keywords are specified properly.
;
;            If the fit is unweighted (i.e. no errors were given, or
;            the weights were uniformly set to unity), then PERROR
;            will probably not represent the true parameter
;            uncertainties.  
;
;            *If* you can assume that the true reduced chi-squared
;            value is unity -- meaning that the fit is implicitly
;            assumed to be of good quality -- then the estimated
;            parameter uncertainties can be computed by scaling PERROR
;            by the measured chi-squared value.
;
;              DOF     = N_ELEMENTS(X) - N_ELEMENTS(PARMS) ; deg of freedom
;              PCERROR = PERROR * SQRT(BESTNORM / DOF)   ; scaled uncertainties
;
;   QUIET - if set then diagnostic fitting messages are suppressed.
;           Default: QUIET=1 (i.e., no diagnostics)
;
;   WEIGHTS - Array of weights to be used in calculating the
;             chi-squared value.  If WEIGHTS is specified then the ERR
;             parameter is ignored.  The chi-squared value is computed
;             as follows:
;
;                CHISQ = TOTAL( (Y-MYFUNCT(X,P))^2 * ABS(WEIGHTS) )
;
;             Here are common values of WEIGHTS:
;
;                1D/ERR^2 - Normal weighting (ERR is the measurement error)
;                1D/Y     - Poisson weighting (counting statistics)
;                1D       - Unweighted
;
;             The ERROR keyword takes precedence over any WEIGHTS
;             keyword values.  If no ERROR or WEIGHTS are given, then
;             the fit is unweighted.
;
;
; EXAMPLE:
;
;   ; First, generate some synthetic data
;   npts = 200
;   x  = dindgen(npts) * 0.1 - 10.                  ; Independent variable 
;   yi = gauss1(x, [2.2D, 1.4, 3000.]) + 1000       ; "Ideal" Y variable
;   y  = yi + randomn(seed, npts) * sqrt(1000. + yi); Measured, w/ noise
;   sy = sqrt(1000.D + y)                           ; Poisson errors
;
;   ; Now fit a Gaussian to see how well we can recover the original
;   yfit = mpfitpeak(x, y, a, error=sy)
;   print, p
;
;   Generates a synthetic data set with a Gaussian peak, and Poisson
;   statistical uncertainty.  Then the same function is fitted to the
;   data.
;
; REFERENCES:
;
;   MINPACK-1, Jorge More', available from netlib (www.netlib.org).
;   "Optimization Software Guide," Jorge More' and Stephen Wright, 
;     SIAM, *Frontiers in Applied Mathematics*, Number 14.
;
; MODIFICATION HISTORY:
;
;   New algorithm for estimating starting values, CM, 31 Oct 1999
;   Documented, 02 Nov 1999
;   Small documentation fixes, 02 Nov 1999
;   Slight correction to calculation of dx, CM, 02 Nov 1999
;   Documented PERROR for unweighted fits, 03 Nov 1999, CM
;   Copying permission terms have been liberalized, 26 Mar 2000, CM
;   Change requirements on # elements in X and Y, 20 Jul 2000, CM
;      (thanks to David Schlegel <schlegel@astro.princeton.edu>)
;   Added documentation on area under curve, 29 Aug 2000, CM
;   Added POSITIVE and NEGATIVE keywords, 17 Nov 2000, CM
;   Added reference to Moffat paper, 10 Jan 2001, CM
;   Added usage message, 26 Jul 2001, CM
;   Documentation clarification, 05 Sep 2001, CM
;
;  $Id: mpfitpeak.pro,v 1.5 2001/09/18 00:12:39 craigm Exp $
;-
; Copyright (C) 1997-2001, Craig Markwardt
; This software is provided as is without any warranty whatsoever.
; Permission to use, copy, modify, and distribute modified or
; unmodified copies is granted, provided this copyright and disclaimer
; are included unchanged.
;-

forward_function mpfit, mpfitfun, mpfitpeak, mpfitpeak_gauss, $
  mpfitpeak_lorentz, mpfitpeak_moffat, mpfitpeak_u

function mpfitpeak_u, x, p
  wid = abs(p(2)) > 1e-20
  return, ((x-p(1))/wid)^2
end


; Gaussian Function
function mpfitpeak_gauss, x, p, _extra=extra
  sz = size(x)
  if sz(sz(0)+1) EQ 5 then smax = 26D else smax = 13.
  u = mpfitpeak_u(x, p)
  mask = u LT (smax^2)  ;; Prevents floating underflow
  if n_elements(p) GE 4 then f = p(3) else f = 0
  if n_elements(p) GE 5 then f = f + p(4)*x
  return,  f + p(0) * mask * exp(-0.5 * temporary(u) * mask)
end

; Lorentzian Function
function mpfitpeak_lorentz, x, p, _extra=extra
  u = mpfitpeak_u(x, p)
  if n_elements(p) GE 4 then f = p(3) else f = 0
  if n_elements(p) GE 5 then f = f + p(4)*x
  return, f + p(0) / (u + 1)
end

; Moffat Function
function mpfitpeak_moffat, x, p, _extra=extra
  u = mpfitpeak_u(x, p)
  if n_elements(p) GE 5 then f = p(4) else f = 0
  if n_elements(p) GE 6 then f = f + p(5)*x
  return, f + p(0) / (u + 1)^p(3)
end


function mpfitpeak, x, y, a, estimates=est, nterms=nterms, $
                    gaussian=gauss, lorentzian=lorentz, moffat=moffat, $
                    parinfo=parinfo, perror=perror, errmsg=errmsg,$
                    error=yerror, weights=weights, $
                    negative=neg, positive=pos, $
                    niter=iter, nfev=nfev, bestnorm=bestnorm, $
                    status=status, query=query, quiet=quiet, _extra=extra

  status = 0L
  errmsg = ''

  if n_params() EQ 0 then begin
      message, 'USAGE: yfit = MPFITPEAK(X, Y, A, ...)', /info
      return, !values.d_nan
  endif

  ;; Detect MPFIT and crash if it was not found
  catch, catcherror
  if catcherror NE 0 then begin
      MPFIT_NOTFOUND:
      catch, /cancel
      message, 'ERROR: the required functions MPFIT and MPFITFUN ' + $
        'must be in your IDL path', /info
      return, !values.d_nan
  endif
  if mpfit(/query)    NE 1 then goto, MPFIT_NOTFOUND
  if mpfitfun(/query) NE 1 then goto, MPFIT_NOTFOUND
  catch, /cancel
  if keyword_set(query) then return, 1

  ;; Check the number of parameter estimates
  if n_elements(quiet) EQ 0 then quiet=1
  if n_elements(nterms) EQ 0 then nterms = 4

  ;; Reject data vectors that are too simple
  if n_elements(x) LT nterms OR n_elements(y) LT nterms then begin
      message, 'ERROR: X and Y must have at least NTERMS elements', /info
      return, !values.d_nan
  endif

  ;; Compute the weighting factors to use
  if n_elements(yerror) EQ 0 AND n_elements(weights) EQ 0 then begin
      weights = x*0+1        ;; Unweighted by default
  endif else if n_elements(yerror) GT 0 then begin
      weights = yerror * 0   ;; Avoid division by zero
      wh = where(yerror NE 0, ct)
      if ct GT 0 then weights(wh) = 1./yerror(wh)^2
  endif

  if n_elements(est) EQ 0 then begin
      ;; Here is the secret - the width is estimated based on the area
      ;; above/below the average.  Thus, as the signal becomes more
      ;; noisy the width automatically broadens as it should.

      maxx = max(x, min=minx) & maxy = max(y, min=miny)
      nx = n_elements(x)
      dx = 0.5 * [x(1)-x(0), x(2:*) - x, x(nx-1) - x(nx-2)]
      totarea = total(dx*y)       ;; Total area under curve
      av = totarea/(maxx - minx)  ;; Average height

      ;; Compute the spread in values above and below average... we
      ;; take the narrowest one as the one with the peak
      wh1 = where(y GE av, ct1)
      sd1 = total(x(wh1)^2)/ct1 - (total(x(wh1))/ct1)^2
      wh2 = where(y LE av, ct2)
      sd2 = total(x(wh2)^2)/ct2 - (total(x(wh2))/ct2)^2
      
      ;; Compute area above/below average

      if keyword_set(pos) then goto, POS_PEAK
      if keyword_set(neg) then goto, NEG_PEAK
      if sd1 LT sd2 then begin  ;; This is a positive peak
          POS_PEAK:
          cent  = x(where(y EQ maxy)) & cent = cent(0)
          peak  = maxy - av
      endif else begin          ;; This is a negative peak
          NEG_PEAK:
          cent  = x(where(y EQ miny)) & cent = cent(0)
          peak  = miny - av
      endelse
      peakarea = totarea - total(dx*(y<av))
      width = peakarea / (2*abs(peak))

      est = [peak, cent, width, av]
      guess = 1
  endif

  ;; Parameter checking for individual function types
  np = 3
  if keyword_set(moffat) then begin               ;; MOFFAT
      fun = 'mpfitpeak_moffat'
      if keyword_set(guess) then est = [est(0:2), 1, est(3:*)]
      np = 4
  endif else if keyword_set(lorentz) then begin  ;; LORENTZIAN
      fun = 'mpfitpeak_lorentz'
  endif else begin                               ;; GAUSSIAN
      fun = 'mpfitpeak_gauss'
  endelse
  if n_elements(est) LT np then begin
      message, 'ERROR: parameter ESTIMATES must have at least '+strtrim(np,2)+$
        ' elements', /info
      return, !values.d_nan
  endif
  if nterms(0) LT np then begin
      message, 'ERROR: NTERMS must be at least '+strtrim(np,2), /info
      return, !values.d_nan
  endif
  p0 = replicate(est(0)*0, nterms(0) > n_elements(est))
  p0(0) = est

  ;; Function call
  a = mpfitfun(fun, x, y, 0, p0(0:nterms(0)-1), weights=weights, $
               bestnorm=bestnorm, nfev=nfev, status=status, $
               parinfo=parinfo, perror=perror, niter=iter, yfit=yfit, $
               quiet=quiet, errmsg=errmsg, _EXTRA=extra)

  ;; Print error message if there is one.
  if NOT keyword_set(quiet) AND errmsg NE '' then $
    message, errmsg, /info

  if status NE 0 then begin
      ;; Make sure the width is positive
      a(2) = abs(a(2))

      return, yfit
  endif

  return, !values.d_nan
end

