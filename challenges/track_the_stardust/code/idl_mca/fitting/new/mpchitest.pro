;+
; NAME:
;   MPCHITEST
;
; AUTHOR:
;   Craig B. Markwardt, NASA/GSFC Code 662, Greenbelt, MD 20770
;   craigm@lheamail.gsfc.nasa.gov
;   UPDATED VERSIONs can be found on my WEB PAGE: 
;      http://cow.physics.wisc.edu/~craigm/idl/idl.html
;
; PURPOSE:
;   Compute the probability of a given chi-squared value
;
; MAJOR TOPICS:
;   Curve and Surface Fitting, Statistics
;
; CALLING SEQUENCE:
;   PROB = MPCHITEST(CHI, DOF, [/SIGMA, /CLEVEL, /SLEVEL ])
;
; DESCRIPTION:
;
;  The function MPCHITEST() computes the probability for a value drawn
;  from the chi-square distribution to equal or exceed the given value
;  CHI.  This can be used for confidence testing of a measured value
;  obeying the chi-squared distribution.
;
;    P_CHI(X > CHI; DOF) = PROB
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
; INPUTS:
;
;   CHI - chi-squared value to be tested.
;
;   DOF - scalar or vector number, giving the number of degrees of
;         freedom in the chi-square distribution.
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
; EXAMPLES:
;
;   print, mpchitest(1300d,1252d)
;
;   Print the probability for a chi-squared value with 1252 degrees of
;   freedom to exceed a value of 1300, as a confidence level.
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
;
;  $Id: mpchitest.pro,v 1.5 2001/11/18 12:59:16 craigm Exp $
;-
; Copyright (C) 1997-2001, Craig Markwardt
; This software is provided as is without any warranty whatsoever.
; Permission to use, copy, modify, and distribute modified or
; unmodified copies is granted, provided this copyright and disclaimer
; are included unchanged.
;-

forward_function cephes_igamc, cephes_igam

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

function cephes_igam, a, x
;   
;   	Incomplete gamma integral
;   
;   
;   
;    SYNOPSIS:
;   
;    double a, x, y, igam();
;   
;    y = igam( a, x );
;   
;    DESCRIPTION:
;   
;    The function is defined by
;   
;                              x
;                               -
;                      1       | |  -t  a-1
;     igam(a,x)  =   -----     |   e   t   dt.
;                     -      | |
;                    | (a)    -
;                              0
;   
;   
;    In this implementation both arguments must be positive.
;    The integral is evaluated by either a power series or
;    continued fraction expansion, depending on the relative
;    values of a and x.
;   
;    ACCURACY:
;   
;                         Relative error:
;    arithmetic   domain     # trials      peak         rms
;       IEEE      0,30       200000       3.6e-14     2.9e-15
;       IEEE      0,100      300000       9.9e-14     1.5e-14
  common cephes_machar, machvals
  MAXLOG = machvals.maxlog
  MACHEP = machvals.machep

  if x LE 0 OR a LE 0 then return, 0.D
  if x GT 1. AND x GT a then return, 1.D - cephes_igamc(a, x)
  
  ax = a * alog(x) - x - lngamma(a)
  if ax LT -MAXLOG then begin
;      message, 'WARNING: underflow', /info
      return, 0.D
  endif
  ax = exp(ax)
  r = a
  c = 1.D
  ans = 1.D
  
  repeat begin
      r = r + 1
      c = c * x/r
      ans = ans + c
  endrep until (c/ans LE MACHEP)

  return, ans*ax/a
end

function cephes_igamc, a, x
;   
;   	Complemented incomplete gamma integral
;   
;   
;   
;    SYNOPSIS:
;   
;    double a, x, y, igamc();
;   
;    y = igamc( a, x );
;   
;    DESCRIPTION:
;   
;    The function is defined by
;   
;   
;     igamc(a,x)   =   1 - igam(a,x)
;   
;                               inf.
;                                 -
;                        1       | |  -t  a-1
;                  =   -----     |   e   t   dt.
;                       -      | |
;                      | (a)    -
;                                x
;   
;   
;    In this implementation both arguments must be positive.
;    The integral is evaluated by either a power series or
;    continued fraction expansion, depending on the relative
;    values of a and x.
;   
;    ACCURACY:
;   
;    Tested at random a, x.
;                   a         x                      Relative error:
;    arithmetic   domain   domain     # trials      peak         rms
;       IEEE     0.5,100   0,100      200000       1.9e-14     1.7e-15
;       IEEE     0.01,0.5  0,100      200000       1.4e-13     1.6e-15

  common cephes_machar, machvals
  MAXLOG = machvals.maxlog
  MACHEP = machvals.machep

  big = 4.503599627370496D15
  biginv = 2.22044604925031308085D-16

  if x LE 0 OR a LE 0 then return, 1.D
  if x LT 1. OR x LT a then return, 1.D - cephes_igam(a, x)
  ax = a * alog(x) - x - lngamma(a)

  if ax LT -MAXLOG then begin
;      message, 'ERROR: underflow', /info
      return, 0.D
  endif
  
  ax = exp(ax)
  y = 1.D - a
  z = x + y + 1.D
  c = 0.D
  pkm2 = 1.D
  qkm2 = x
  pkm1 = x + 1.D
  qkm1 = z * x
  ans = pkm1 / qkm1

  repeat begin
      c = c + 1.D
      y = y + 1.D
      z = z + 2.D
      yc = y * c
      pk = pkm1 * z - pkm2 * yc
      qk = qkm1 * z - qkm2 * yc
      if qk NE 0 then begin
          r = pk/qk
          t = abs( (ans-r)/r )
          ans = r
      endif else begin
          t = 1.D
      endelse
      pkm2 = pkm1
      pkm1 = pk
      qkm2 = qkm1
      qkm1 = qk
      if abs(pk) GT big then begin
          pkm2 = pkm2 * biginv
          pkm1 = pkm1 * biginv
          qkm2 = qkm2 * biginv
          qkm1 = qkm1 * biginv
      endif
  endrep until t LE MACHEP

  return, ans * ax
end

; MPCHITEST
;  compute the probability for a chi-squared value to exceed x give
;  the number of degrees of freedom dof.
function mpchitest, x, dof, slevel=slevel, clevel=clevel, sigma=sigma

  cephes_setmachar   ;; Set machine constants
  
  p = double(x) * 0
  for i = 0, n_elements(x)-1 do begin
      p(i) = cephes_igamc(0.5D * dof, 0.5D * double(x(i)))
  endfor
  if keyword_set(clevel) then return, 1D - double(p)
  if keyword_set(sigma) then return, mpnormlim(p, /slevel)

  return, p
end

  
  
