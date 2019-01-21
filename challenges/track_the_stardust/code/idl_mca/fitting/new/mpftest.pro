;+
; NAME:
;   MPFTEST
;
; AUTHOR:
;   Craig B. Markwardt, NASA/GSFC Code 662, Greenbelt, MD 20770
;   craigm@lheamail.gsfc.nasa.gov
;   UPDATED VERSIONs can be found on my WEB PAGE: 
;      http://cow.physics.wisc.edu/~craigm/idl/idl.html
;
; PURPOSE:
;   Compute the probability of a given F value
;
; MAJOR TOPICS:
;   Curve and Surface Fitting, Statistics
;
; CALLING SEQUENCE:
;   PROB = MPFTEST(F, DOF1, DOF2, [/SIGMA, /CLEVEL, /SLEVEL ])
;
; DESCRIPTION:
;
;  The function MPFTEST() computes the probability for a value drawn
;  from the F-distribution to equal or exceed the given value of F.
;  This can be used for confidence testing of a measured value obeying
;  the F-distribution (i.e., for testing the ratio of variances, or
;  equivalently for the addition of parameters to a fitted model).
;
;    P_F(X > F; DOF1, DOF2) = PROB
;
;  In specifying the returned probability level the user has three
;  choices:
;
;    * return the confidence level when the /CLEVEL keyword is passed;
;      OR
;
;    * return the significance level (i.e., 1 - confidence level) when
;      the /SLEVEL keyword is passed (default); OR
;
;    * return the "sigma" of the probability (i.e., compute the
;      probability based on the normal distribution) when the /SIGMA
;      keyword is passed.
;
;  Note that /SLEVEL, /CLEVEL and /SIGMA are mutually exclusive.
;
;  For the ratio of variance test, the two variances, VAR1 and VAR2,
;  should be distributed according to the chi-squared distribution
;  with degrees of freedom DOF1 and DOF2 respectively.  The F-value is
;  computed as:
;
;     F = (VAR1/DOF1) / (VAR2/DOF2)
;
;  and then the probability is computed as:
;
;     PROB = MPFTEST(F, DOF1, DOF2, ... )
;
;
;  For the test of additional parameters in least squares fitting, the
;  user should perform two separate fits, and have two chi-squared
;  values.  One fit should be the "original" fit with no additional
;  parameters, and one fit should be the "new" fit with M additional
;  parameters.
;
;    CHI1 - chi-squared value for original fit
;
;    DOF1 - number of degrees of freedom of CHI1 (number of data
;           points minus number of original parameters)
;
;    CHI2 - chi-squared value for new fit
;
;    DOF2 - number of degrees of freedom of CHI2
;
;  Note that according to this formalism, the number of degrees of
;  freedom in the "new" fit, DOF2, should be less than the number of
;  degrees of freedom in the "original" fit, DOF1 (DOF2 < DOF1); and
;  also CHI2 < CHI1.
;
;  With the above definition, the F value is computed as:
;
;    F = ( (CHI1-CHI2)/(DOF1-DOF2) )   /  (CHI2/DOF2)
;
;  where DOF1-DOF2 is equal to M, and then the F-test probability is
;  computed as:
;
;     PROB = MPFTEST(F, DOF1-DOF2, DOF2, ... )
;
;  Note that this formalism assumes that the addition of the M
;  parameters is a small peturbation to the overall fit.  If the
;  additional parameters dramatically changes the character of the
;  model, then the first "ratio of variance" test is more appropriate,
;  where F = (CHI1/DOF1) / (CHI2/DOF2).
;
; INPUTS:
;
;   F - ratio of variances as defined above.
;
;   DOF1 - number of degrees of freedom in first variance component.
;
;   DOF2 - number of degrees of freedom in second variance component.
;
;
; RETURNS:
;
;  Returns a scalar or vector of probabilities, as described above,
;  and according to the /SLEVEL, /CLEVEL and /SIGMA keywords.
;
; KEYWORD PARAMETERS:
;
;   SLEVEL - if set, then PROB describes the significance level
;            (default).
;
;   CLEVEL - if set, then PROB describes the confidence level.
;
;   SIGMA - if set, then PROB is the number of "sigma" away from the
;           mean in the normal distribution.
;
; EXAMPLE:
;
;   chi1 = 62.3D & dof1 = 42d
;   chi2 = 54.6D & dof2 = 40d
;
;   f = ((chi1-chi2)/(dof1-dof2)) / (chi2/dof2)
;   print, mpftest(f, dof1-dof2, dof2)
;
;   This is a test for addition of parameters.  The "original"
;   chi-squared value was 62.3 with 42 degrees of freedom, and the
;   "new" chi-squared value was 54.6 with 40 degrees of freedom.
;   These values reflect the addition of 2 parameters and the
;   reduction of the chi-squared value by 7.7.  The significance of
;   this set of circumstances is 0.029568701.
;
; REFERENCES:
;
;   Algorithms taken from CEPHES special function library, by Stephen
;   Moshier. (http://www.netlib.org/cephes/)
;
; MODIFICATION HISTORY:
;   Completed, 1999, CM
;   Documented, 16 Nov 2001, CM
;   Reduced obtrusiveness of common block and math error handling, 18
;     Nov 2001, CM
;   Added documentation, 30 Dec 2001, CM
;   Documentation corrections (thanks W. Landsman), 17 Jan 2002, CM
;   Example docs were corrected (Thanks M. Perez-Torres), 17 Feb 2002, CM
;
;  $Id: mpftest.pro,v 1.6 2002/02/17 14:48:28 craigm Exp $
;-
; Copyright (C) 1997-2001, Craig Markwardt
; This software is provided as is without any warranty whatsoever.
; Permission to use, copy, modify, and distribute modified or
; unmodified copies is granted, provided this copyright and disclaimer
; are included unchanged.
;-

forward_function cephes_incbet, cephes_incbcf, cephes_incbd, cephes_pseries

;; Set machine constants, once for this session.  Double precision
;; only.
pro cephes_setmachar
  common cephes_machar, cephes_machar_vals
  if n_elements(cephes_machar_vals) GT 0 then return

  if (!version.release) LT 5 then dummy = check_math(1, 1)

  mch = machar(/double)
  machep = mch.eps
  maxnum = mch.xmax
  minnum = mch.xmin
  maxlog = alog(mch.xmax)
  minlog = alog(mch.xmin)
  maxgam = 171.624376956302725D

  cephes_machar_vals = {machep: machep, maxnum: maxnum, minnum: minnum, $
                        maxlog: maxlog, minlog: minlog, maxgam: maxgam}

  if (!version.release) LT 5 then dummy = check_math(0, 0)
  return
end

;							incbet.c
;   
;   	Incomplete beta integral
;   
;   
;    SYNOPSIS:
;   
;    double a, b, x, y, incbet();
;   
;    y = incbet( a, b, x );
;   
;   
;    DESCRIPTION:
;   
;    Returns incomplete beta integral of the arguments, evaluated
;    from zero to x.  The function is defined as
;   
;                     x
;        -            -
;       | (a+b)      | |  a-1     b-1
;     -----------    |   t   (1-t)   dt.
;      -     -     | |
;     | (a) | (b)   -
;                    0
;   
;    The domain of definition is 0 <= x <= 1.  In this
;    implementation a and b are restricted to positive values.
;    The integral from x to 1 may be obtained by the symmetry
;    relation
;   
;       1 - incbet( a, b, x )  =  incbet( b, a, 1-x ).
;   
;    The integral is evaluated by a continued fraction expansion
;    or, when b*x is small, by a power series.
;   
;    ACCURACY:
;   
;    Tested at uniformly distributed random points (a,b,x) with a and b
;    in "domain" and x between 0 and 1.
;                                           Relative error
;    arithmetic   domain     # trials      peak         rms
;       IEEE      0,5         10000       6.9e-15     4.5e-16
;       IEEE      0,85       250000       2.2e-13     1.7e-14
;       IEEE      0,1000      30000       5.3e-12     6.3e-13
;       IEEE      0,10000    250000       9.3e-11     7.1e-12
;       IEEE      0,100000    10000       8.7e-10     4.8e-11
;    Outputs smaller than the IEEE gradual underflow threshold
;    were excluded from these statistics.
;   
;    ERROR MESSAGES:
;      message         condition      value returned
;    incbet domain      x<0, x>1          0.0
;    incbet underflow                     0.0
function cephes_incbet, aa, bb, xx

  forward_function cephes_incbcf, cephes_incbd, cephes_pseries

  common cephes_machar, machvals
  MINLOG = machvals.minlog
  MAXLOG = machvals.maxlog
  MAXGAM = machvals.maxgam
  MACHEP = machvals.machep

  if aa LE 0. OR bb LE 0. then goto, DOMERR

  if xx LE 0. OR xx GE 1. then begin
      if xx EQ 0 then return, 0.D
      if xx EQ 1. then return, 1.D
      DOMERR:
      message, 'ERROR: domain', /info
      return, 0.D
  endif

  flag = 0
  if bb * xx LE 1. AND xx LE 0.95 then begin
      t = cephes_pseries(aa, bb, xx)
      goto, DONE
  endif

  w = 1.D - xx

  if xx GT aa/(aa+bb) then begin
      flag = 1
      a = bb
      b = aa
      xc = xx
      x = w
  endif else begin
      a = aa
      b = bb
      xc = w
      x = xx
  endelse

  if flag EQ 1 AND b*x LE 1. AND x LE 0.95 then begin
      t = cephes_pseries(a, b, x)
      goto, DONE
  endif

  ;; Choose expansion for better convergence
  y = x * (a+b-2.) - (a-1.)
  if y LT 0. then w = cephes_incbcf(a, b, x) $
  else            w = cephes_incbd(a, b, x) / xc

  ;; Multiply w by the factor
  ;;    a      b   _             _     _
  ;;   x  (1-x)   | (a+b) / ( a | (a) | (b) ) .   */
  y = a * alog(x)
  t = b * alog(xc)
  if (a+b) LT MAXGAM AND abs(y) LT MAXLOG AND abs(t) LT MAXLOG then begin
      t = ((xc^b) * (x^a)) * w * gamma(a+b) / ( a * gamma(a) * gamma(b) )
      goto, DONE
  endif

  ;; Resort to logarithms
  y = y + t + lngamma(a+b) - lngamma(a) - lngamma(b)
  y = y + alog(w/a)
  if y LT MINLOG then t = 0.D $
  else                t = exp(y)
  
  DONE:
  if flag EQ 1 then begin
      if t LE MACHEP then t = 1.D - MACHEP $
      else                t = 1.D - t
  endif
  
  return, t
end

;; Continued fraction expasion #1 for incomplete beta integral
function cephes_incbcf, a, b, x

  common cephes_machar, machvals
  MACHEP = machvals.machep
  
  big = 4.503599627370496D15
  biginv = 2.22044604925031308085D-16

  k1 = a
  k2 = a + b
  k3 = a
  k4 = a + 1.
  k5 = 1.
  k6 = b - 1.
  k7 = k4
  k8 = a + 2.

  pkm2 = 0.D
  qkm2 = 1.D
  pkm1 = 1.D
  qkm1 = 1.D
  ans = 1.D
  r = 1.D
  n = 0L
  thresh = 3.D * MACHEP
  
  repeat begin
      xk = - (x * k1 * k2 ) / (k3 * k4)
      pk = pkm1 + pkm2 * xk
      qk = qkm1 + qkm2 * xk
      pkm2 = pkm1
      pkm1 = pk
      qkm2 = qkm1
      qkm1 = qk

      xk = ( x * k5 * k6 ) / ( k7 * k8)
      pk = pkm1 + pkm2 * xk
      qk = qkm1 + qkm2 * xk
      pkm2 = pkm1
      pkm1 = pk
      qkm2 = qkm1
      qkm1 = qk

      if qk NE 0 then r = pk/qk
      if r NE 0 then begin
          t = abs( (ans-r)/r )
          ans = r
      endif else begin
          t = 1.D
      endelse

      if t LT thresh then goto, CDONE
      k1 = k1 + 1.
      k2 = k2 + 1.
      k3 = k3 + 2.
      k4 = k4 + 2.
      k5 = k5 + 1.
      k6 = k6 - 1.
      k7 = k7 + 2.
      k8 = k8 + 2.

      if abs(qk) + abs(pk) GT big then begin
          pkm2 = pkm2 * biginv
          pkm1 = pkm1 * biginv
          qkm2 = qkm2 * biginv
          qkm1 = qkm1 * biginv
      endif
      if abs(qk) LT biginv OR abs(pk) LT biginv then begin
          pkm2 = pkm2 * big
          pkm1 = pkm1 * big
          qkm2 = qkm2 * big
          qkm1 = qkm1 * big
      endif
      
      n = n + 1
  endrep until n GE 300

  CDONE:
  return, ans
end

;; Continued fraction expansion #2 for incomplete beta integral
function cephes_incbd, a, b, x

  common cephes_machar, machvals
  MACHEP = machvals.machep

  big = 4.503599627370496D15
  biginv = 2.22044604925031308085D-16

  k1 = a
  k2 = b - 1.
  k3 = a
  k4 = a + 1.
  k5 = 1.
  k6 = a + b
  k7 = a + 1.
  k8 = a + 2.
  
  pkm2 = 0.D
  qkm2 = 1.D
  pkm1 = 1.D
  qkm1 = 1.D
  z = x / (1.D - x)
  ans = 1.D
  r = 1.D
  n = 0L
  thresh = 3.D * MACHEP
  
  repeat begin
      xk = -(z * k1 * k2) / (k3 * k4)
      pk = pkm1 + pkm2 * xk
      qk = qkm1 + qkm2 * xk
      pkm2 = pkm1
      pkm1 = pk
      qkm2 = qkm1
      qkm1 = qk
      
      xk = (z * k5 * k6) / (k7 * k8)
      pk = pkm1 + pkm2 * xk
      qk = qkm1 + qkm2 * xk
      pkm2 = pkm1
      pkm1 = pk
      qkm2 = qkm1
      qkm1 = qk
      
      if qk NE 0 then r = pk/qk
      if r NE 0 then begin
          t = abs( (ans-r)/r )
          ans = r
      endif else begin
          t = 1.D
      endelse

      if t LT thresh then goto, CDONE
      
      k1 = k1 + 1.
      k2 = k2 - 1.
      k3 = k3 + 2.
      k4 = k4 + 2.
      k5 = k5 + 1.
      k6 = k6 + 1.
      k7 = k7 + 2.
      k8 = k8 + 2.

      if abs(qk) + abs(pk) GT big then begin
          pkm2 = pkm2 * biginv
          pkm1 = pkm1 * biginv
          qkm2 = qkm2 * biginv
          qkm1 = qkm1 * biginv
      endif
      if abs(qk) LT biginv OR abs(pk) LT biginv then begin
          pkm2 = pkm2 * big
          pkm1 = pkm1 * big
          qkm2 = qkm2 * big
          qkm1 = qkm1 * big
      endif
      
      n = n + 1
  endrep until n GE 300

  CDONE:
  return, ans
end

;; Power series for incomplete beta integral.
;; Use when b*x is small and x not too close to 1
function cephes_pseries, a, b, x

  common cephes_machar, machvals
  MINLOG = machvals.minlog
  MAXLOG = machvals.maxlog
  MAXGAM = machvals.maxgam
  MACHEP = machvals.machep

  ai = 1.D/a
  u = (1.D - b) * x
  v = u / (a + 1.D)
  t1 = v
  t = u
  n = 2.D
  s = 0.D
  z = MACHEP * ai

  while abs(v) GT z do begin
      u = (n-b) * x / n
      t = t * u
      v = t / (a+n)
      s = s + v
      n = n + 1.D
  endwhile
  s = s + t1 + ai

  u = a * alog(x)
  if (a+b) LT MAXGAM AND abs(u) LT MAXLOG then begin
      t = gamma(a+b)/(gamma(a)*gamma(b))
      s = s * t * x^a
  endif else begin
      t = lngamma(a+b) - lngamma(a) - lngamma(b) + u + alog(s)
      if t LT MINLOG then s = 0.D else s = exp(t)
  endelse

  return, s
end

; MPFTEST
;  Returns the significance level of a particular F-statistic.
;     P(x; nu1, nu2)  is probability for F to exceed x 
;  x - the F-ratio
;    For ratio of variance test:
;      x = (chi1sq/nu1) / (chi2sq/nu2)
;      p = mpftest(x, nu1, nu2)
;    For additional parameter test:
;      x = [ (chi1sq-chi2sq)/(nu1-nu2) ] / (chi2sq/nu2)
;      p = mpftest(x, nu1-nu2, nu2)
;
;  nu1 - number of DOF in chi1sq
;  nu2 - number of DOF in chi2sq   nu2 < nu1

function mpftest, x, nu1, nu2, slevel=slevel, clevel=clevel, sigma=sigma

  cephes_setmachar   ;; Set machine constants

  if nu1 LT 1 OR nu2 LT 1 OR x LT 0. then begin
      message, 'ERROR: domain', /info
      return, 0.D
  endif

  w = double(nu2) / (double(nu2) + double(nu1)*double(x))
  
  s = cephes_incbet(0.5D * nu2, 0.5D * nu1, w)
  ;; Return confidence level if requested
  if keyword_set(clevel) then return, 1D - s
  if keyword_set(sigma)  then return, mpnormlim(s, /slevel)

  ;; Return significance level otherwise.
  return, s
  
end

      
