;+
; NAME:
;   MPFITFUN
;
; AUTHOR:
;   Craig B. Markwardt, NASA/GSFC Code 662, Greenbelt, MD 20770
;   craigm@lheamail.gsfc.nasa.gov
;   UPDATED VERSIONs can be found on my WEB PAGE: 
;      http://astrog.physics.wisc.edu/~craigm/idl.html
;
; PURPOSE:
;   Perform Levenberg-Marquardt least-squares fit to IDL function
;
; MAJOR TOPICS:
;   Curve and Surface Fitting
;
; CALLING SEQUENCE:
;   parms = MPFIT(MYFUNCT, X, Y, ERR, start_parms, ...)
;
; DESCRIPTION:
;
;  MPFITFUN fits a user-supplied model -- in the form of an IDL
;  function -- to a set of user-supplied data.  MPFITFUN calls
;  MPFIT, the MINPACK-1 least-squares minimizer, to do the main
;  work.
;
;  Given the data and their uncertainties, MPFITFUN finds the best set
;  of model parameters which match the data (in a least-squares
;  sense) and returns them in an array.
;  
;  The user must supply the following items:
;   - An array of independent variable values ("X").
;   - An array of "measured" *dependent* variable values ("Y").
;   - An array of "measured" 1-sigma uncertainty values ("ERR").
;   - The name of an IDL function which computes Y given X ("MYFUNCT").
;
;  There are very few restrictions placed on X, Y or MYFUNCT.  Simply
;  put, MYFUNCT must map the "X" values into "Y" values given the
;  model parameters.  The "X" values may represent any independent
;  variable (not just Cartesian X), and indeed may be multidimensional
;  themselves.  For example, in the application of image fitting, X
;  may be a 2xN array of image positions.
;
;  MPFITFUN carefully avoids passing large arrays where possible to
;  improve performance.
;
;  See below for an example of usage.
;   
; INPUTS:
;   MYFUNCT - a string variable containing the name of an IDL function.
;             This function computes the "model" Y values given the
;             X values and model parameters.  It should be declared in
;             the following way:
;
;             FUNCTION MYFUNCT, X, parms
;               ; X are the independent variable values
;               ; parms are the parameter values
;               YMOD = ....
;               RETURN, YMOD
;             END
;
;             The returned array YMOD should be of the same size and
;             dimensions as the "measured" Y values.  
;
;   START_PARAMS - An array of starting values for each of the
;                  parameters of the model.  The number of parameters
;                  should be fewer than the number of measurements.
;                  Also, the parameters should have the same data type
;                  as the measurements (double is preferred).
;
;                  This parameter is optional if the PARINFO keyword
;                  is used (see MPFIT).  The PARINFO keyword provides
;                  a mechanism to fix or constrain individual
;                  parameters.  If both START_PARAMS and PARINFO are
;                  passed, then the starting *value* is taken from
;                  START_PARAMS, but the *constraints* are taken from
;                  PARINFO.
; 
; INPUT KEYWORD PARAMETERS:
;
;   FUNCTARGS - A structure which contains the parameters to be passed
;               to the user-supplied function specified by MYFUNCT via
;               the _EXTRA mechanism.  This is the way you can pass
;               additional data to your user-supplied function without
;               using common blocks.
;
;               By default, no extra parameters are passed to the
;               user-supplied function.
;
;   MAXITER - The maximum number of iterations to perform.  If the
;             number is exceeded, then the STATUS value is set to 5
;             and MPFIT returns.
;             Default: 200 iterations
;
;   FTOL - a nonnegative input variable. Termination occurs when both
;          the actual and predicted relative reductions in the sum of
;          squares are at most FTOL.  Therefore, FTOL measures the
;          relative error desired in the sum of squares.
;          Default: 1D-10
;
;   XTOL - a nonnegative input variable. Termination occurs when the
;          relative error between two consecutive iterates is at most
;          XTOL. therefore, XTOL measures the relative error desired
;          in the approximate solution.
;          Default: 1D-10
;
;   GTOL - a nonnegative input variable. Termination occurs when the
;          cosine of the angle between fvec and any column of the
;          jacobian is at most GTOL in absolute value. Therefore, GTOL
;          measures the orthogonality desired between the function
;          vector and the columns of the jacobian.
;          Default: 1D-10
;
;   ITERPROC - The name of a procedure to be called upon each NPRINT
;              iteration of the MPFIT routine.  It should be declared
;              in the following way:
;
;              PRO ITERPROC, MYFUNCT, p, iter, FUNCTARGS=fcnargs, $
;                PARINFO=parinfo, QUIET=quiet, ...
;                ; perform custom iteration update
;              END
;         
;              Where MYFUNCT is the user-supplied function to be
;              minimized, P is the current set of model parameters,
;              ITER is the iteration number, and FUNCTARGS are the
;              arguments to be passed to MYFUNCT.  QUIET is set when
;              no textual output should be printed.  See below for
;              documentation of PARINFO.
;
;              In implementation, ITERPROC, can perform updates to the
;              terminal or graphical user interface, to provide
;              feedback while the fit proceeds.  If the fit is to be
;              stopped for any reason, then ITERPROC should set the
;              system variable !ERR to a negative value.  In
;              principle, ITERPROC should probably not modify the
;              parameter values, because it may interfere with the
;              algorithm's stability.  In practice it is allowed.
;
;              Default: an internal routine is used to print the
;                       parameter values.
;
;   NPRINT - The frequency with which ITERPROC is called.  A value of
;            1 indicates that ITERPROC is called with every iteration,
;            while 2 indicates every other iteration, etc.  
;            Default value: 1
;
;   ITERARGS - The keyword arguments to be passed to ITERPROC via the
;              _EXTRA mechanism.  This should be a structure, and is
;              similar in operation to FUNCTARGS.
;              Default: no arguments are passed.
;
;   PARINFO - Provides a mechanism for more sophisticated constraints
;             to be placed on parameter values.  When PARINFO is not
;             passed, then it is assumed that all parameters are free
;             and unconstrained.  No values in PARINFO are modified
;             during the call to MPFIT.
;
;             PARINFO should be an array of structures, one for each
;             parameter.  Each parameter is associated with one
;             element of the array, in numerical order.  The structure
;             should have at least the following entries:
;
;               - VALUE - the starting parameter value (but see
;                         START_PARAMS above).
;
;               - FIXED - a boolean value, whether the parameter is to 
;                         be held fixed or not.  Fixed parameters are
;                         not varied by MPFIT, but are passed on to 
;                         MYFUNCT for evaluation.
;
;               - LIMITED - a two-element boolean array.  If the
;                 first/second element is set, then the parameter is
;                 bounded on the lower/upper side.  A parameter can be
;                 bounded on both sides.
;
;               - LIMITS - a two-element float or double array.  Gives
;                 the parameter limits on the lower and upper sides,
;                 respectively.  Zero, one or two of these values can
;                 be set, depending on the value of LIMITED.
;
;               - STEP - the step size to be used in calculating the
;                 numerical derivatives.  If set to zero, then the
;                 step size is computed automatically.
; 
;             Other tag values can also be given in the structure, but
;             they are ignored.
;
;             Example:
;             parinfo = replicate({value:0.D, fixed:0, $
;                           limited:[0,0], limits:[0.D,0], step:0.D}, 5)
;             parinfo(0).fixed = 1
;             parinfo(4).limited(0) = 1
;             parinfo(4).limits(0)  = 50.D
;             parinfo(*).value = [5.7D, 2.2, 500., 1.5, 2000.]
;
;             A total of 5 parameters, with starting values of 5.7,
;             2.2, 500, 1.5, and 2000 are given.  The first parameter
;             is fixed at a value of 5.7, and the last parameter is
;             constrained to be above 50.
;
;             Default value:  all parameters are free and unconstrained.
;
;   QUIET - set this keyword when no textual output should be printed
;           by MPFIT
;
;   NOCOVAR - set this keyword to prevent the calculation of the
;             covariance matrix before returning (see COVAR)
;
; RETURNS:
;
;   Returns the array of best-fit parameters.
;
; OUTPUT KEYWORD PARAMETERS:
;
;   Output keywords are the same as MPFIT.
;   
;   NFEV - the number of MYFUNCT function evaluations performed.
;
;   ERRMSG - a string error or warning message is returned.
;
;   BESTNORM - the value of the summed squared residuals for the
;              returned parameter values.
;
;   PERROR - The formal 1-sigma errors in each parameter.  If a
;            parameter is held fixed, or if it touches a boundary,
;            then the error is reported as zero.
;
;   COVAR - the covariance matrix for the set of parameters returned
;           by MPFIT.  The matrix is NxN where N is the number of
;           parameters.  The square root of the diagonal elements
;           gives the formal 1-sigma statistical errors on the
;           parameters IF errors were treated "properly" in MYFUNC.
;           Parameter errors are also returned in PERROR.
;
;           To compute the correlation matrix, PCOR, use this:
;           IDL> PCOR = COV * 0
;           IDL> FOR i = 0, n-1 DO FOR j = 0, n-1 DO $
;                PCOR(i,j) = COV(i,j)/sqrt(COV(i,i)*COV(j,j))
;
;           If NOCOVAR is set or MPFIT terminated abnormally, then
;           COVAR is set to a scalar with value !VALUES.D_NAN.
;
;   STATUS - an integer status code is returned.  It can have one of
;            the following values:
;
;	   0  improper input parameters.
;         
;	   1  both actual and predicted relative reductions
;	      in the sum of squares are at most FTOL.
;         
;	   2  relative error between two consecutive iterates
;	      is at most XTOL
;         
;	   3  conditions for STATUS = 1 and STATUS = 2 both hold.
;         
;	   4  the cosine of the angle between fvec and any
;	      column of the jacobian is at most GTOL in
;	      absolute value.
;         
;	   5  the maximum number of iterations has been reached
;         
;	   6  FTOL is too small. no further reduction in
;	      the sum of squares is possible.
;         
;	   7  XTOL is too small. no further improvement in
;	      the approximate solution x is possible.
;         
;	   8  GTOL is too small. fvec is orthogonal to the
;	      columns of the jacobian to machine precision.
;
; EXAMPLE:
;
;   ; First, generate some synthetic data
;   x  = dindgen(200) * 0.1 - 10.                   ; Independent variable 
;   yi = gauss1(x, [2.2D, 1.4, 3000.])              ; "Ideal" Y variable
;   y  = yi + randomn(seed, 200) * sqrt(1000. + yi) ; Measured, w/ noise
;   sy = sqrt(1000.D + y)                           ; Poisson errors
;
;   ; Now fit a Gaussian to see how well we can recover
;   p0 = [1.D, 1., 1000.]                           ; Initial guess
;   p = mpfitfun('GAUSS1', x, y, sy, p0)            ; Fit a function
;   print, p
;
;   Generates a synthetic data set with a Gaussian peak, and Poisson
;   statistical uncertainty.  Then the same function is fitted to the
;   data to see how close we can get.
;
; REFERENCES:
;
;   MINPACK-1, Jorge More', available from netlib (www.netlib.org).
;   "Optimization Software Guide," Jorge More' and Stephen Wright, 
;     SIAM, *Frontiers in Applied Mathematics*, Number 14.
;
; MODIFICATION HISTORY:
;   Written, Apr-Jul 1998, CM
;
;-

FORWARD_FUNCTION mpfitfun_eval, mpfitfun

function mpfitfun_eval, p, hx=hx, hy=hy, herr=he, hf=hf, fcn=fcn, $
                 FUNCTARGS=fcnargs, _EXTRA=extra

  ;; Retrieve X values
  handle_value, hx, x, /no_copy
  if handle_info(hf) then begin

      ;; Compute the Y model values
      handle_value, hf, fcnargs, /no_copy
      f = call_function(fcn, x, p, _EXTRA=fcnargs)
      handle_value, hf, fcnargs, /set, /no_copy
  endif else $
    f = call_function(fcn, x, p)
  handle_value, hx, x, /set, /no_copy

  ;; Do some error checking on the errors.
  use_err = 0
  handle_value, hy, y, /no_copy
  handle_value, he, err, /no_copy
  if n_elements(err) GT 0 then use_err = 1
  if n_elements(err) EQ 1 then $
    if err EQ 0 then use_err = 0

  ;; Compute the deviations
  if use_err then $
    result = (y-temporary(f))/err $
  else $
    result = y-temporary(f)
  handle_value, hy, y, /set, /no_copy
  handle_value, he, err, /set, /no_copy

  ;; Make sure the returned result is one-dimensional.
  result = reform(result, n_elements(result), /overwrite)
  return, result
  
end

function mpfitfun, fcn, x, y, err, p, FUNCTARGS=fa, BESTNORM=bestnorm, $
                   parinfo=parinfo, STATUS=info, nfev=nfev, quiet=quiet, $
                   covar=covar, perror=perror, ERRMSG=errmsg, _EXTRA=extra

  ;; Use handles, which prevent the passing large amounts of data back
  ;; and forth between the fitting routine and the function evaluator.
  ;; The no_copy keyword is used above to prevent data copying.
  hx = handle_create(value=x)
  hy = handle_create(value=y)
  he = handle_create(value=err)
  hf = -1L
  if n_elements(fa) GT 0 then $
    hf = handle_create(value=fa)

  result = mpfit('mpfitfun_eval', p, $
                 FUNCTARGS={hx:hx, hy:hy, herr:he, fcn:fcn, hf:hf}, $
                 parinfo=parinfo, STATUS=info, nfev=nfev, BESTNORM=bestnorm, $
                 covar=covar, perror=perror, ERRMSG=errmsg, quiet=quiet, $
                 _EXTRA=extra)

  handle_free, hx
  handle_free, hy
  handle_free, he
  if hf GE 0 then handle_free, hf

  ;; Print error message if there is one.
  if NOT keyword_set(quiet) AND errmsg NE '' then $
    message, errmsg, /info

  return, result

end
