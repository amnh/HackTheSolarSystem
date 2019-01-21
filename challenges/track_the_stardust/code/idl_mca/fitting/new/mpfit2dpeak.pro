;+
; NAME:
;   MPFIT2DPEAK
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
;   yfit = MPFIT2DPEAK(Z, A [, X, Y, /TILT ...] )
;
; DESCRIPTION:
;
;   MPFIT2DPEAK fits a gaussian, lorentzian or Moffat model using the
;   non-linear least squares fitter MPFIT.  MPFIT2DPEAK is meant to be
;   a drop-in replacement for IDL's GAUSS2DFIT function (and requires
;   MPFIT and MPFIT2DFUN).
;
;   The choice of the fitting function is determined by the keywords
;   GAUSSIAN, LORENTZIAN and MOFFAT.  By default the gaussian model
;   function is used.  [ The Moffat function is a modified Lorentzian
;   with variable power law index. ]  The two-dimensional peak has
;   independent semimajor and semiminor axes, with an optional
;   rotation term activated by setting the TILT keyword.  The baseline
;   is assumed to be a constant.
;
;      GAUSSIAN      A(0) + A(1)*exp(-0.5*u)
;      LORENTZIAN    A(0) + A(1)/(u + 1)
;      MOFFAT        A(0) + A(1)/(u + 1)^A(7)
;
;      u = ( (x-A(4))/A(2) )^2 + ( (y-A(5))/A(3) )^2
;
;         where x and y are cartesian coordinates in rotated
;         coordinate system if TILT keyword is set.
;
;   The returned parameter array elements have the following meanings:
;
;      A(0)   Constant baseline level
;      A(1)   Peak value
;      A(2)   Peak half-width (x) -- gaussian sigma or half-width at half-max
;      A(3)   Peak half-width (y) -- gaussian sigma or half-width at half-max
;      A(4)   Peak centroid (x)
;      A(5)   Peak centroid (y)
;      A(6)   Rotation angle (radians) if TILT keyword set
;      A(7)   Moffat power law index if MOFFAT keyword set
;
;   By default the initial starting values for the parameters A are
;   estimated from the data.  However, explicit starting values can be
;   supplied using the ESTIMATES keyword.  Also, error or weighting
;   values can optionally be provided; otherwise the fit is
;   unweighted.
;
; RESTRICTIONS:
;
;   If no starting parameter ESTIMATES are provided, then MPFIT2DPEAK
;   attempts to estimate them from the data.  This is not a perfect
;   science; however, the author believes that the technique
;   implemented here is more robust than the one used in IDL's
;   GAUSS2DFIT.  The author has tested cases of strong peaks, noisy
;   peaks and broad peaks, all with success.
;
;
; INPUTS:
;
;   Z - Two dimensional array of "measured" dependent variable values.
;       Z should be of the same type and dimension as (X # Y).
;
;   X - Optional vector of x positions for a single row of Z.
;
;          X(i) should provide the x position of Z(i,*)
;
;       Default: X values are integer increments from 0 to NX-1
;
;   Y - Optional vector of y positions for a single column of Z.
;
;          Y(j) should provide the j position of Z(*,j)
;
;       Default: Y values are integer increments from 0 to NY-1
;
; OUTPUTS:
;   A - Upon return, an array of best fit parameter values.  See the
;       table above for the meanings of each parameter element.
;
;
; RETURNS:
;
;   Returns the best fitting model function as a 2D array.
;
; KEYWORDS:
;
;   ** NOTE ** Additional keywords such as PARINFO, BESTNORM, and
;              STATUS are accepted by MPFIT2DPEAK but not documented
;              here.  Please see the documentation for MPFIT for the
;              description of these advanced options.
;
;   ERROR - upon input, the measured 1-sigma uncertainties in the "Z"
;           values.  If no ERROR or WEIGHTS are given, then the fit is
;           unweighted.
;
;   ESTIMATES - Array of starting values for each parameter of the
;               model.
;               Default: parameter values are estimated from data.
;
;   GAUSSIAN - if set, fit a gaussian model function.  The Default.
;   LORENTZIAN - if set, fit a lorentzian model function.
;   MOFFAT - if set, fit a Moffat model function.
;
;   NEGATIVE - if set, and ESTIMATES is not provided, then MPFIT2DPEAK
;              will assume that a negative peak is present -- a
;              valley.  Specifying this keyword is not normally
;              required, since MPFIT2DPEAK can determine this
;              automatically.
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
;              DOF     = N_ELEMENTS(Z) - N_ELEMENTS(A)   ; deg of freedom
;              PCERROR = PERROR * SQRT(BESTNORM / DOF)   ; scaled uncertainties
;
;   QUIET - if set then diagnostic fitting messages are suppressed.
;           Default: QUIET=1 (i.e., no diagnostics)
;
;   CIRCULAR - if set, then the peak profile is assumed to be
;              azimuthally symmetric.  When set, the parameters A(2)
;              and A(3) will be identical and the TILT keyword will
;              have no effect.
;
;   TILT - if set, then the major and minor axes of the peak profile
;          are allowed to rotate with respect to the image axes.
;          Parameter A(6) will be set to the clockwise rotation angle
;          of the A(2) axis in radians.
;
;   WEIGHTS - Array of weights to be used in calculating the
;             chi-squared value.  If WEIGHTS is specified then the ERR
;             parameter is ignored.  The chi-squared value is computed
;             as follows:
;
;                CHISQ = TOTAL( (Z-MYFUNCT(X,Y,P))^2 * ABS(WEIGHTS) )
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
; ; Construct a sample gaussian surface in range [-5,5] centered at [2,-3]
;   x = findgen(100)*0.1 - 5. & y = x
;   xx = x # (y*0 + 1)
;   yy = (x*0 + 1) # y
;   rr = sqrt((xx-2.)^2 + (yy+3.)^2)
;
; ; Gaussian surface with sigma=0.5, peak value of 3, noise with sigma=0.2
;   z = 3.*exp(-(rr/0.5)^2) + randomn(seed,100,100)*.2
;
; ; Fit gaussian parameters A
;   zfit = mpfit2dpeak(z, a, x, y)
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
;   Documented PERROR for unweighted fits, 03 Nov 1999, CM
;   Copying permission terms have been liberalized, 26 Mar 2000, CM
;   Small cosmetic changes, 21 Sep 2000, CM
;   Corrected bug introduced by cosmetic changes, 11 Oct 2000, CM :-)
;   Added POSITIVE keyword, 17 Nov 2000, CM
;   Removed TILT in common, in favor of FUNCTARGS approach, 23 Nov
;     2000, CM
;   Added SYMMETRIC keyword, documentation for TILT, and an example,
;     24 Nov 2000, CM
;   Changed SYMMETRIC to CIRCULAR, 17 Dec 2000, CM
;
;  $Id: mpfit2dpeak.pro,v 1.2 2000/12/18 07:00:15 craigm Exp $
;-
; Copyright (C) 1997-2000, Craig Markwardt
; This software is provided as is without any warranty whatsoever.
; Permission to use, copy, modify, and distribute modified or
; unmodified copies is granted, provided this copyright and disclaimer
; are included unchanged.
;-

forward_function mpfit, mpfitfun, mpfit2dpeak, mpfit2dpeak_gauss, $
  mpfit2dpeak_lorentz, mpfit2dpeak_moffat, mpfit2dpeak_u

; Compute the "u" value = (x/a)^2 + (y/b)^2 with optional rotation
function mpfit2dpeak_u, x, y, p, tilt=tilt, symmetric=sym
  widx  = abs(p(2)) > 1e-20 & widy  = abs(p(3)) > 1e-20 
  if keyword_set(sym) then widy = widx
  xp    = x-p(4)            & yp    = y-p(5)
  theta = p(6)
  if keyword_set(tilt) AND theta NE 0 then begin
      c  = cos(theta) & s  = sin(theta)
      return, ( (xp * (c/widx) - yp * (s/widx))^2 + $
                (xp * (s/widy) + yp * (c/widy))^2 )
  endif else begin
      return, (xp/widx)^2 + (yp/widy)^2
  endelse

end

; Gaussian Function
function mpfit2dpeak_gauss, x, y, p, tilt=tilt, symmetric=sym, _extra=extra
  sz = size(x)
  if sz(sz(0)+1) EQ 5 then smax = 26D else smax = 13.

  u = mpfit2dpeak_u(x, y, p, tilt=keyword_set(tilt), symmetric=keyword_set(sym))
  mask = u LT (smax^2)  ;; Prevents floating underflow
  return, p(0) + p(1) * mask * exp(-0.5 * u * mask)
end

; Lorentzian Function
function mpfit2dpeak_lorentz, x, y, p, tilt=tilt, symmetric=sym, _extra=extra
  u = mpfit2dpeak_u(x, y, p, tilt=keyword_set(tilt), symmetric=keyword_set(sym))
  return, p(0) + p(1) / (u + 1)
end

; Moffat Function
function mpfit2dpeak_moffat, x, y, p, tilt=tilt, symmetric=sym, _extra=extra
  u = mpfit2dpeak_u(x, y, p, tilt=keyword_set(tilt), symmetric=keyword_set(sym))
  return, p(0) + p(1) / (u + 1)^p(7)
end

function mpfit2dpeak, z, a, x, y, estimates=est, tilt=tilt, $
                      gaussian=gauss, lorentzian=lorentz, moffat=moffat, $
                      parinfo=parinfo, perror=perror, errmsg=errmsg,$
                      error=zerror, weights=weights, symmetric=sym, $
                      niter=iter, nfev=nfev, bestnorm=bestnorm, $
                      status=status, query=query, quiet=quiet, _extra=extra

  status = 0L
  errmsg = ''

  ;; Detect MPFIT and crash if it was not found
  catch, catcherror
  if catcherror NE 0 then begin
      MPFIT_NOTFOUND:
      catch, /cancel
      message, 'ERROR: the required functions MPFIT and MPFIT2DFUN ' + $
        'must be in your IDL path', /info
      return, !values.d_nan
  endif
  if mpfit(/query)      NE 1 then goto, MPFIT_NOTFOUND
  if mpfit2dfun(/query) NE 1 then goto, MPFIT_NOTFOUND
  catch, /cancel
  if keyword_set(query) then return, 1

  ;; Reject too few data
  if n_elements(z) LT 8 then begin
      message, 'ERROR: array must have at least eight elements', /info
      return, !values.d_nan
  endif

  sz = size(z)
  if sz(0) LT 2 then begin
      message, 'ERROR: array must be 2-dimensional', /info
      return, !values.d_nan
  endif
  nx = sz(1)
  ny = sz(2)
  
  ;; Fill in the X and Y values if needed -- note clever promotion to
  ;;                                         double if needed
  if n_elements(x) EQ 0 then x = findgen(nx)*(z(0)*0+1)
  if n_elements(y) EQ 0 then y = findgen(ny)*(z(0)*0+1)
  if n_elements(x) LT nx then begin
      message, 'ERROR: X array was not large enough', /info
      return, !values.d_nan
  endif
  if n_elements(y) LT ny then begin
      message, 'ERROR: Y array was not large enough', /info
      return, !values.d_nan
  endif

  ;; Make 2D arrays of X and Y values -- if the user hasn't done it
  if n_elements(x) NE n_elements(z) then   xx = x # (y*0 + 1)   else xx = x
  if n_elements(y) NE n_elements(z) then   yy = (x*0 + 1) # y   else yy = y

  ;; Compute the weighting factors to use
  if n_elements(zerror) EQ 0 AND n_elements(weights) EQ 0 then begin
      weights = z*0+1        ;; Unweighted by default
  endif else if n_elements(zerror) GT 0 then begin
      weights = zerror * 0   ;; Avoid division by zero
      wh = where(zerror NE 0, ct)
      if ct GT 0 then weights(wh) = 1./zerror(wh)^2
  endif

  if n_elements(est) EQ 0 then begin
      ;; Here is the secret - the width is estimated based on the volume
      ;; above/below the average.  Thus, as the signal becomes more
      ;; noisy the width automatically broadens as it should.

      maxx = max(x, min=minx) & maxy = max(y, min=miny)
      maxz = max(z, whmax)    & minz = min(z, whmin)
      nx = n_elements(x)
      dx = 0.5 * [x(1)-x(0), x(2:*) - x, x(nx-1) - x(nx-2)]
      ny = n_elements(y)
      dy = 0.5 * [y(1)-y(0), y(2:*) - y, y(ny-1) - y(ny-2)]
      ;; Compute cell areas
      da = dx # dy

      totvol = total(da*z)               ;; Total volume under curve
      av = totvol/(total(dx)*total(dy))  ;; Average height

      ;; Compute the spread in values above and below average... we
      ;; take the narrowest one as the one with the peak
      wh   = where(z GE av, ct1)
      sdx1 = total(xx(wh)^2)/ct1 - (total(xx(wh))/ct1)^2
      sdy1 = total(yy(wh)^2)/ct1 - (total(yy(wh))/ct1)^2
      wh   = where(z LE av, ct2)
      sdx2 = total(xx(wh)^2)/ct2 - (total(xx(wh))/ct2)^2
      sdy2 = total(yy(wh)^2)/ct2 - (total(yy(wh))/ct2)^2
      wh   = 0  ;; conserve memory

      if keyword_set(pos) then goto, POS_PEAK
      if keyword_set(neg) then goto, NEG_PEAK

      ;; Compute volume above/below average
      if (sdx1 LT sdx2 AND sdy1 LT sdy2) then begin 
          ;; Positive peak
          POS_PEAK:
          centx = xx(whmax)
          centy = yy(whmax)
          peakz = maxz - av
      endif else if (sdx1 GT sdx2 AND sdy1 GT sdy2) then begin 
          ;; Negative peak
          NEG_PEAK:
          centx = xx(whmin)
          centy = yy(whmin)
          peakz = minz - av
      endif else begin
          ;; Ambiguous case
          if abs(maxz - av) GT (minz - av) then goto, POS_PEAK $
          else                                  goto, NEG_PEAK
      endelse
      peakvol = totvol - total(da*(z<av))
      width   = sqrt(peakvol / (6*abs(peakz)))

      est = [av, peakz, width, width, centx, centy, 0, 1]
      guess = 1
  endif

  ;; Check the number of parameter estimates
  if n_elements(quiet) EQ 0 then quiet=1

  np = 7
  ;; Parameter checking for individual function types
  if keyword_set(moffat) then begin               ;; MOFFAT
      fun = 'mpfit2dpeak_moffat'
      np = 8
  endif else if keyword_set(lorentz) then begin  ;; LORENTZIAN
      fun = 'mpfit2dpeak_lorentz'
  endif else begin                               ;; GAUSSIAN
      fun = 'mpfit2dpeak_gauss'
  endelse
  if n_elements(est) LT np then begin
      message, 'ERROR: parameter ESTIMATES must have at least '+strtrim(np,2)+$
        ' elements', /info
      return, !values.d_nan
  endif
  p0 = replicate(est(0)*0, np > n_elements(est))
  p0(0) = est

  ;; Function call
  fargs = {tilt: keyword_set(tilt), symmetric: keyword_set(sym)}
  a = mpfit2dfun(fun, xx, yy, z, 0, p0(0:np-1), weights=weights, $
                 bestnorm=bestnorm, nfev=nfev, status=status, $
                 parinfo=parinfo, perror=perror, niter=iter, yfit=yfit, $
                 quiet=quiet, errmsg=errmsg, $
                 functargs=fargs, _EXTRA=extra)

  ;; Print error message if there is one.
  if NOT keyword_set(quiet) AND errmsg NE '' then $
    message, errmsg, /info

  ;; Make sure the parameters are sane
  if status NE 0 then begin
      ;; Width is positive
      a(2) = abs(a(2))
      a(3) = abs(a(3))
      if keyword_set(sym) then a(3) = a(2)
      ;; Make sure that theta is in the range [0,pi]
      a(6) = ((a(6) MOD !dpi) + 2*!dpi) MOD !dpi
      a = a(0:np-1)

      if n_elements(yfit) EQ nx*ny then $
        return, reform(yfit, nx, ny, /overwrite)
  endif

  return, !values.d_nan
end

