;+
; NAME:
;   MPFITEXPR
;
; AUTHOR:
;   Craig B. Markwardt, NASA/GSFC Code 662, Greenbelt, MD 20770
;   craigm@lheamail.gsfc.nasa.gov
;   UPDATED VERSIONs can be found on my WEB PAGE: 
;      http://astrog.physics.wisc.edu/~craigm/idl.html
;
; PURPOSE:
;   Perform Levenberg-Marquardt least-squares fit to arbitrary expression
;
; MAJOR TOPICS:
;   Curve and Surface Fitting
;
; CALLING SEQUENCE:
;   parms = MPFIT('X*(1-X)+3', XVAL, YVAL, ERR, start_parms, ...)
;
; DESCRIPTION:
;
;  MPFITEXPR fits a user-supplied model -- in the form of an arbitrary IDL
;  expression -- to a set of user-supplied data.  MPFITEXPR calls
;  MPFIT, the MINPACK-1 least-squares minimizer, to do the main
;  work.
;
;  Given the data and their uncertainties, MPFITEXPR finds the best set
;  of model parameters which match the data (in a least-squares
;  sense) and returns them in an array.
;  
;  The user must supply the following items:
;   - An array of independent variable values ("X").
;   - An array of "measured" *dependent* variable values ("Y").
;   - An array of "measured" 1-sigma uncertainty values ("ERR").
;   - A text IDL expression which computes Y given X.
;
;  There are very few restrictions placed on X, Y or the expression of
;  the model.  Simply put, the expression must map the "X" values into
;  "Y" values given the model parameters.  The "X" values may
;  represent any independent variable (not just Cartesian X), and
;  indeed may be multidimensional themselves.  For example, in the
;  application of image fitting, X may be a 2xN array of image
;  positions.
;
;  Some rules must be obeyed in constructing the expression.  First,
;  the independent variable name *MUST* be "X" in the expression,
;  regardless of the name of the variable being passed to MPFITEXPR.
;  This is demonstrated in the above calling sequence, where the X
;  variable passed in is called "XVAL" but the expression still refers
;  to "X".  Second, parameter values must be referred to as an array
;  named "P".
;
;  If you do not pass in starting values for the model parameters,
;  MPFITEXPR will attempt to determine the number of parameters you
;  intend to have (it does this by looking for references to the array
;  variable named "P").  When no starting values are passed in, the
;  values are assumed to start at zero.
;
;  MPFITFUN carefully avoids passing large arrays where possible to
;  improve performance.
;
;  See below for an example of usage.
;   
; INPUTS:
;   MYFUNCT - a string variable containing an IDL expression.  The
;             only restriction is that the independent variable *must*
;             be referred to as "X" and model parameters *must* be
;             referred to as an array called "P".
;
;             The expression should calculate "model" Y values given
;             the X values and model parameters.  Using the vector
;             notation of IDL, this can be quite easy to do.  If your
;             expression gets complicated, you may wish to make an IDL
;             function which will improve performance and readibility.
;
;             The resulting array should be of the same size and
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
;                  If no parameters are given, then MPFITEXPR attempts
;                  to determine the number of parameters by scanning
;                  the expression.  Parameters determined this way
;                  are initialized to zero.
; 
; INPUT KEYWORD PARAMETERS:
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
;   yi = gauss1(x, [2.2D, 1.4, 3000.]) + 1000       ; "Ideal" Y variable
;   y  = yi + randomn(seed, 200) * sqrt(yi)         ; Measured, w/ noise
;   sy = sqrt(y)                                    ; Poisson errors
;
;   ; Now fit a Gaussian to see how well we can recover
;   p0 = [800.D, 1.D, 1., 500.]                     ; Initial guess
;   p = mpfitexpr('P(0) + GAUSS1(X, P(1:3))', $
;                 x, y, sy, p0)                     ; Fit the expression
;   print, p
;
;   Generates a synthetic data set with a Gaussian peak, and Poisson
;   statistical uncertainty.  Then a model consisting of a constant
;   plus Gaussian is fit to the data.
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

FORWARD_FUNCTION mpfitexpr_eval, mpfitexpr

function mpfitexpr_eval, p, hx=_hx, hy=_hy, herr=_he, $
                  expr=_expr, _EXTRA=extra

  handle_value, _hx, x, /no_copy
  _f = 0.D
  _cmd = '_f = '+_expr
  _err = execute(_cmd)
  handle_value, _hx, x, /set, /no_copy
  if _err EQ 0 then return, !values.d_nan

  _use_err = 0
  handle_value, _hy, y, /no_copy
  handle_value, _he, err, /no_copy
  if n_elements(err) GT 0 then _use_err = 1
  if n_elements(err) EQ 1 then $
    if err EQ 0 then _use_err = 0

  if _use_err then $
    result = (y-temporary(_f))/err $
  else $
    result = y-temporary(_f)

  handle_value, _hy, y, /set, /no_copy
  handle_value, _he, err, /set, /no_copy

  result = reform(result, n_elements(result), /overwrite)
  return, result
end

function mpfitexpr, expr, x, y, err, p, BESTNORM=bestnorm, $
                    parinfo=parinfo, $
                    STATUS=info, nfev=nfev, errmsg=errmsg, $
                    covar=covar, perror=perror, quiet=quiet, _EXTRA=extra

  ;; If no parameters are given, then parse the input expression,
  ;; and determine the number of parameters automatically.
  if (n_elements(parinfo) GT 0) AND (n_elements(p) EQ 0) then $
    p = parinfo(*).value
  if (n_elements(p) EQ 0) then begin
      pos = 0L
      nparams = 0L
      ee = strupcase(expr)
      ;; These are character constants representing the boundaries of
      ;; variable names.
      ca = (byte('A'))(0)
      cz = (byte('Z'))(0)
      c0 = (byte('0'))(0)
      c9 = (byte('9'))(0)
      c_ = (byte('_'))(0)  ;; Underscore can be in a variable name
      ll = strlen(ee)
      pnames = ['']

      ;; Now step through, looking for variables looking like p(0), etc.
      repeat begin
          i = [strpos(ee, 'P(', pos), strpos(ee, 'P[', pos)]
          wh = where(i GE 0, ct)
          if ct LE 0 then goto, DONE_PARAMS
          i = min(i(wh))

          ;; None found, finished
          if i LT 0 then goto, DONE_PARAMS
          ;; Too close to the end of the string
          if i GT ll-4 then goto, DONE_PARAMS

          ;; Have to be careful here, to be sure that this isn't just
          ;; a variable name ending in "p"
          maybe = 0
          ;; If this is the first character
          if i EQ 0 then maybe = 1 $
          else begin
              ;; Or if the preceding character is a non-variable character
              c = (byte(strmid(ee, i-1, 1)))(0)
              if NOT ( (c GE ca AND c LE cz) OR (c GE c0 AND c LE c9) $
                       OR c EQ c_ ) then maybe = 1
          endelse
          if maybe then begin
              ;; If we found one, then strip out the value inside the
              ;; parentheses.
              rest = strmid(ee, i+2, ll-i-2)
              next = str_sep(rest, ')', /trim)
              next = next(0)
              pnames = [pnames, next]
          endif
          pos = i+1
      endrep until pos GE ll

      DONE_PARAMS:
      if n_elements(pnames) EQ 1 then begin
          message, 'ERROR: no parameters to fit', /info
          return, !values.d_nan
      endif

      ;; Finally, we take the maximum parameter number
      pnames = pnames(1:*)
      nparams = max(long(pnames)) + 1
      if NOT keyword_set(quiet) then $
        message, '  Number of parameters: '+strtrim(nparams,2) $
        + ' (initialized to zero)', /info

      ;; Create a parameter vector, starting at zero
      p = dblarr(nparams)
  endif

  ;; Use handles, which prevent the passing large amounts of data back
  ;; and forth between the fitting routine and the function evaluator.
  ;; The no_copy keyword is used above to prevent data copying.
  hx = handle_create(value=x)
  hy = handle_create(value=y)
  he = handle_create(value=err)

  ;; Test out the function, as lmfit would call it, to see if it works
  ;; okay.  There is no sense in calling the fitter if the function
  ;; itself doesn't work.
  catch, catcherror
  if catcherror NE 0 then begin
      CATCH_ERROR:
      catch, /cancel
      message, 'ERROR: execution of "'+expr+'" failed.', /info
      message, '       check syntax and parameter usage', /info
      if n_elements(hx) GT 0 then handle_free, hx
      if n_elements(hy) GT 0 then handle_free, hy
      if n_elements(he) GT 0 then handle_free, he
      return, !values.d_nan
  endif

  ;; Initialize.  Function that is actually called is mpfitexpr_eval,
  ;; which is a wrapper that sets up the expression evaluation.
  fcn = 'mpfitexpr_eval'

  fcnargs = {hx:hx, hy:hy, herr:he, expr:expr}
  fvec = call_function(fcn, p, _EXTRA=fcnargs)
  if n_elements(fvec) EQ 1 then $
    if NOT finite(fvec(0)) then goto, CATCH_ERROR
  ;; No errors caught if reached this stage
  catch, /cancel

  if keyword_set(quiet) then iterproc = ''
  result = mpfit(fcn, p, FUNCTARGS=fcnargs, BESTNORM=bestnorm, $
                 parinfo=parinfo, STATUS=info, nfev=nfev, errmsg=errmsg, $
                 covar=covar, perror=perror, quiet=quiet, _EXTRA=extra)

  ;; Print error message if there is one.
  if NOT keyword_set(quiet) AND errmsg NE '' then $
    message, errmsg, /info

  return, result
end
