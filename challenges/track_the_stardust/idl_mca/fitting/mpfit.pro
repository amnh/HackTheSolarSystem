;+
; NAME:
;   MPFIT
;
; AUTHOR:
;   Craig B. Markwardt, NASA/GSFC Code 662, Greenbelt, MD 20770
;   craigm@lheamail.gsfc.nasa.gov
;   UPDATED VERSIONs can be found on my WEB PAGE: 
;      http://astrog.physics.wisc.edu/~craigm/idl.html
;
; PURPOSE:
;   Perform Levenberg-Marquardt least-squares minimization (MINPACK-1)
;
; MAJOR TOPICS:
;   Curve and Surface Fitting
;
; CALLING SEQUENCE:
;   parms = MPFIT(MYFUNCT, start_parms, FUNCTARGS=fcnargs, NFEV=nfev,
;                 MAXITER=maxiter, ERRMSG=errmsg, NPRINT=nprint, 
;                 FTOL=ftol, XTOL=xtol, GTOL=gtol, 
;                 STATUS=status, ITERPROC=iterproc, ITERARGS=iterargs,
;                 COVAR=covar, PERROR=perror,
;                 PARINFO=parinfo)
;
; DESCRIPTION:
;
;  MPFIT uses the Levenberg-Marquardt technique to solve the
;  least-squares problem.  In its typical use, MPFIT will be used to
;  fit a user-supplied function (the "model") to user-supplied data
;  points (the "data") by adjusting a set of parameters.
;
;  For example, a researcher may think that a set of observed data
;  points is best modelled with a Gaussian curve.  A Gaussian curve is
;  parameterized by its mean, standard deviation and normalization.
;  MPFIT will, within certain constraints, find the set of parameters
;  which best fits the data.  The fit is "best" in the least-squares
;  sense; that is, the sum of the weighted squared differences between
;  the model and data is minimumized.
;
;  The Levenberg-Marquardt technique is a particular strategy for
;  iteratively searching for the best fit.  This particular
;  implementation is drawn from MINPACK-1 (see NETLIB), and seems to
;  be more robust than routines provided with IDL.  This version
;  allows upper and lower bounding constraints to be placed on each
;  parameter, or the parameter can be held fixed.
;
;  The IDL user-supplied function should return an array of weighted
;  deviations between model and data.  In a typical scientific problem
;  the residuals should be weighted so that each deviate has a
;  gaussian sigma of 1.0.  If X represents values of the independent
;  variable, Y represents a measurement for each value of X, and ERR
;  represents the error in the measurements, then the deviates could
;  be calculated as follows:
;
;    DEVIATES = (Y - F(X)) / ERR
;
;  where F is the analytical function representing the model.  The
;  convenience functions MPFITFUN and MPFITEXPR calculate the deviates
;  for you.  If ERR are the 1-sigma uncertainties in Y, then
;
;    TOTAL( DEVIATES^2 ) 
;
;  will be the total chi-squared value.  MPFIT will minimize the
;  chi-square value.  The values of X, Y and ERR are passed through
;  MPFIT to the user-supplied function via the FUNCTARGS keyword.
;
;  MPFIT does not perform more general optimization tasks.  It is
;  customized, based on MINPACK-1, to the least-squares minimization
;  problem.
;
;  In the search for the best-fit solution, MPFIT calculates
;  derivatives numerically via a finite difference approximation.  The
;  user-supplied function need not calculate the derivatives
;  explicitly.
;   
; INPUTS:
;   MYFUNCT - a string variable containing the name of the function to
;             be minimized.  The function should return the weighted
;             deviations between the model and the data.  It should be
;             declared in the following way (or in some equivalent):
;
;             FUNCTION MYFUNCT, p, X=x, Y=y, ERR=err
;              ; Parameter values are passed in "p"
;              ; Calculation of deviations occurs here
;              model = F(x)
;              return, (y-model)/err
;             END
;
;             The keyword parameters X, Y, and ERR in the example 
;             above are suggestive but not required.  Any parameters
;             can be passed to MYFUNCT by using the FUNCTARGS keyword
;             to MPFIT.  Use MPFITFUN and MPFITEXPR if you need ideas
;             on how to do that.
;
;             In general there are no restrictions on the number of
;             dimensions in X, Y or ERR.  However the deviates *must*
;             be returned in a one-dimensional array, and must have
;             the same type (float or double) as the input arrays.
;
;   START_PARAMS - An array of starting values for each of the
;                  parameters of the model.  The number of parameters
;                  should be fewer than the number of measurements.
;                  Also, the parameters should have the same data type
;                  as the measurements (double is preferred).
;
;                  This parameter is optional if the PARINFO keyword
;                  is used.  See below.  The PARINFO keyword provides
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
;               data to your user-supplied function without using
;               common blocks.
;
;               Consider the following example:
;                if FUNCTARGS = { XVAL:[1.D,2,3], YVAL:[1.D,4,9],
;                                 ERRVAL:[1.D,1,1] }
;                then the user supplied function should be declared
;                like this:
;                PRO MYFUNCT, P, XVAL=x, YVAL=y, ERRVAL=err
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
;   p0 = [5.7D, 2.2, 500., 1.5, 2000.]
;   fa = {X:x, Y:y, ERR:err}
;   p = mpfit('MYFUNCT', p0, functargs=fa)
;
;   Minimizes sum of squares of MYFUNCT.  MYFUNCT is called with the X,
;   Y, and ERR keyword parameters that are given by FUNCTARGS.  The
;   resulting parameter values are returned in p.
;
; REFERENCES:
;
;   MINPACK-1, Jorge More', available from netlib (www.netlib.org).
;   "Optimization Software Guide," Jorge More' and Stephen Wright, 
;     SIAM, *Frontiers in Applied Mathematics*, Number 14.
;
; MODIFICATION HISTORY:
;   Translated from MINPACK-1 in FORTRAN, Apr-Jul 1998, CM
;   Fixed bug in parameter limits (x vs xnew), 04 Aug 1998, CM
;   Added PERROR keyword, 04 Aug 1998, CM
;
;-

FORWARD_FUNCTION mpfit_fdjac2, mpfit_enorm, mpfit_lmpar, mpfit_covar, mpfit

;  Things to do:
;    * optional derivative in user-supplied function

;  Documentation below is mostly original to the MINPACK-1 FORTRAN
;  routines.  Some documentation has been modified to refer to IDL
;  terminology, but not all.

;     **********
;
;     subroutine fdjac2
;
;     this subroutine computes a forward-difference approximation
;     to the m by n jacobian matrix associated with a specified
;     problem of m functions in n variables.
;
;     the subroutine statement is
;
;       PRO        FDJAC2,fcn, x, fvec, step, ulimited, ulimit, 
;                         iflag=iflag, epsfcn=epsfcn, nfev=nfev,
;                         fcnargs=fcnargs, xall=xall, ifree=ifree
;
;     where
;
;       fcn is a string containing the name of a user-supplied
;         function which calculates the m functions.  fcn should
;         be declared in the following way
;
;         FUNCTION fcn, x, ...
;           ; calculations of y values
;           RETURN, y
;         END
;         
;         Extra keyword arguments can be passed via the fcnargs
;         keyword.  The value of !err should not be changed by fcn
;         unless the user wants to terminate execution of fdjac2.  In
;         this case set !err to a negative integer.
;
;       fcnargs is an optional structure containing keyword arguments
;         to be passed to the function fcn, following the _EXTRA
;         convention.  If no extra keyword arguments are to be passed,
;         then fcnargs should remain undefined.
;
;	x is an input array
;
;       fvec is an input array which must contain the functions
;         evaluated at x.
;
;	fjac is an output m by n array which contains the
;	  approximation to the jacobian matrix evaluated at x.
;         m is the number of elements in fvec, n is then number of
;         elements in x.
;
;	iflag is an integer variable which can be used to terminate
;	  the execution of fdjac2. see description of fcn.
;
;	epsfcn is an input variable used in determining a suitable
;	  step length for the forward-difference approximation. this
;	  approximation assumes that the relative errors in the
;	  functions are of the order of epsfcn. if epsfcn is less
;	  than the machine precision, it is assumed that the relative
;	  errors in the functions are of the order of the machine
;	  precision.
;
;	wa is a work array of length m.
;
;     subprograms called
;
;	user-supplied ...... fcn
;
;	minpack-supplied ... dpmpar
;
;	fortran-supplied ... dabs,dmax1,dsqrt
;
;     argonne national laboratory. minpack project. march 1980.
;     burton s. garbow, kenneth e. hillstrom, jorge j. more
;

function mpfit_fdjac2, fcn, x, fvec, step, ulimited, ulimit, $
                 iflag=iflag, epsfcn=epsfcn, nfev=nfev, $
                 FUNCTARGS=fcnargs, xall=xall, ifree=ifree
  sz = size(x)
  mch = machar(double=(sz(sz(0)+1) EQ 5))
  MACHEP = mch.eps
  DWARF = mch.xmin
  if n_elements(epsfcn) EQ 0 then epsfcn = MACHEP
  if n_elements(nfev) EQ 0 then nfev = 0L
  if n_elements(xall) EQ 0 then xall = x
  if n_elements(ifree) EQ 0 then ifree = lindgen(n_elements(xall))
  if n_elements(step) EQ 0 then step = x * 0 

  eps = sqrt(max([epsfcn, MACHEP]));
  n = n_elements(x)
  m = n_elements(fvec)
  fjac = reform(replicate(fvec(0), m, n), m, n, /overwrite)

  h = eps * abs(x)
  wh = where(h EQ 0, ct)
  if ct GT 0 then h(wh) = eps

  ;; if STEP is given, use that
  ct = 0 & if n_elements(step) GT 0 then wh = where(step GT 0, ct)
  if ct GT 0 then h(wh) = step(wh)

  ;; if LIMITS specified, then respect those
  ct = 0 & if n_elements(ulimited) GT 0 AND n_elements(ulimit) GT 0 then $
    wh = where(ulimited AND (x GT ulimit-h), ct)
  if ct GT 0 then h(wh) = -h(wh)

  for j=0L, n-1 do begin
      xp = xall
      xp(ifree(j)) = xp(ifree(j)) + h(j)
      
      !err = 0
      fp = call_function(fcn, xp, _EXTRA=fcnargs)
      
      nfev = nfev + 1
      iflag = !err
      !err = 0
      if iflag LT 0 then return, !values.d_nan

      fjac(*,j) = (fp-fvec)/h(j)
  endfor

  return, fjac
end

;     **********
;
;     function enorm
;
;     given an n-vector x, this function calculates the
;     euclidean norm of x.
;
;     the euclidean norm is computed by accumulating the sum of
;     squares in three different sums. the sums of squares for the
;     small and large components are scaled so that no overflows
;     occur. non-destructive underflows are permitted. underflows
;     and overflows do not occur in the computation of the unscaled
;     sum of squares for the intermediate components.
;     the definitions of small, intermediate and large components
;     depend on two constants, rdwarf and rgiant. the main
;     restrictions on these constants are that rdwarf**2 not
;     underflow and rgiant**2 not overflow. the constants
;     given here are suitable for every known computer.
;
;     the function statement is
;
;	double precision function enorm(n,x)
;
;     where
;
;	n is a positive integer input variable.
;
;	x is an input array of length n.
;
;     subprograms called
;
;	fortran-supplied ... dabs,dsqrt
;
;     argonne national laboratory. minpack project. march 1980.
;     burton s. garbow, kenneth e. hillstrom, jorge j. more
;
function mpfit_enorm, vec, tricky=tricky
; Very simple-minded sum-of-squares
;  ans0 = sqrt(total(vec^2, 1))

  sz = size(vec)
  mch = machar(double=(sz(sz(0)+1) EQ 5))
  MACHEP = mch.eps
  rdwarf = sqrt(mch.xmin) * 0.9
  rgiant = sqrt(mch.xmax) * 0.9
  sz = size(vec)
  if sz(0) EQ 0 then return, abs(vec)
  if sz(0) EQ 1 then begin
      nr = 1L
      nc = sz(1)
  endif
  if sz(0) EQ 2 then begin
      nr = sz(2)
      nc = sz(1)
  endif
  if sz(0) EQ 2 AND (sz(1) EQ 1) then begin
      vec = vec(*)
      nr = 1L
      nc = n_elements(vec)
  endif
  ans = replicate(vec(0)*0, nr)
  zero = vec(0)*0
  if n_elements(ans) EQ 1 then ans = zero

  for j = 0, nr-1 do begin
      s1 = zero
      s2 = zero
      s3 = zero
      x1max = zero
      x3max = zero
      agiant = rgiant/float(nc)
      x = vec(*,j)
      xabs = abs(x)

      big = (xabs GE agiant)
      sml = (xabs LE rdwarf AND xabs GT 0)

      wh = where(NOT big AND NOT sml, ct)
      if ct GT 0 then s2 = total(xabs(wh)^2)

      wh = where(big, ct)
      if ct GT 0 then begin
          x1max = max(xabs(wh))
          s1 = total((xabs(wh)/x1max)^2)
      endif

      wh = where(sml, ct)
      if ct GT 0 then begin
          x3max = max(xabs(wh))
          s3 = total((xabs(wh)/x3max)^2)
      endif

      if s1 NE 0 then begin
          ans(j) = x1max*sqrt(s1 + (s2/x1max)/x1max)
      endif else if s2 NE 0 then begin
          if s2 GE x3max then $
            temp = s2*((x3max/s2)*(x3max*s3)+1) $
          else $
            temp = x3max*((s2/x3max)+(x3max*s3))
          ans(j) = sqrt(temp)
      endif else begin
          ans(j) = x3max*sqrt(s3)
      endelse
  endfor

  return, ans
end

;     **********
;
;     subroutine qrfac
;
;     this subroutine uses householder transformations with column
;     pivoting (optional) to compute a qr factorization of the
;     m by n matrix a. that is, qrfac determines an orthogonal
;     matrix q, a permutation matrix p, and an upper trapezoidal
;     matrix r with diagonal elements of nonincreasing magnitude,
;     such that a*p = q*r. the householder transformation for
;     column k, k = 1,2,...,min(m,n), is of the form
;
;			    t
;	    i - (1/u(k))*u*u
;
;     where u has zeros in the first k-1 positions. the form of
;     this transformation and the method of pivoting first
;     appeared in the corresponding linpack subroutine.
;
;     the subroutine statement is
;
;	subroutine qrfac(m,n,a,lda,pivot,ipvt,lipvt,rdiag,acnorm,wa)
;
;     where
;
;	m is a positive integer input variable set to the number
;	  of rows of a.
;
;	n is a positive integer input variable set to the number
;	  of columns of a.
;
;	a is an m by n array. on input a contains the matrix for
;	  which the qr factorization is to be computed. on output
;	  the strict upper trapezoidal part of a contains the strict
;	  upper trapezoidal part of r, and the lower trapezoidal
;	  part of a contains a factored form of q (the non-trivial
;	  elements of the u vectors described above).
;
;	lda is a positive integer input variable not less than m
;	  which specifies the leading dimension of the array a.
;
;	pivot is a logical input variable. if pivot is set true,
;	  then column pivoting is enforced. if pivot is set false,
;	  then no column pivoting is done.
;
;	ipvt is an integer output array of length lipvt. ipvt
;	  defines the permutation matrix p such that a*p = q*r.
;	  column j of p is column ipvt(j) of the identity matrix.
;	  if pivot is false, ipvt is not referenced.
;
;	lipvt is a positive integer input variable. if pivot is false,
;	  then lipvt may be as small as 1. if pivot is true, then
;	  lipvt must be at least n.
;
;	rdiag is an output array of length n which contains the
;	  diagonal elements of r.
;
;	acnorm is an output array of length n which contains the
;	  norms of the corresponding columns of the input matrix a.
;	  if this information is not needed, then acnorm can coincide
;	  with rdiag.
;
;	wa is a work array of length n. if pivot is false, then wa
;	  can coincide with rdiag.
;
;     subprograms called
;
;	minpack-supplied ... dpmpar,enorm
;
;	fortran-supplied ... dmax1,dsqrt,min0
;
;     argonne national laboratory. minpack project. march 1980.
;     burton s. garbow, kenneth e. hillstrom, jorge j. more
;
;     **********
pro mpfit_qrfac, a, ipvt, rdiag, acnorm, pivot=pivot

  sz = size(a)
  mch = machar(double=(sz(sz(0)+1) EQ 5))
  MACHEP = mch.eps
  DWARF = mch.xmin
  sz = size(a)
  m = sz(1)
  n = sz(2)
  
  ;; Compute the initial column norms and initialize arrays
  acnorm = mpfit_enorm(a)
  rdiag = acnorm
  wa = rdiag
  if keyword_set(pivot) then ipvt = lindgen(n)
  
  ;; Reduce a to r with householder transformations
  minmn = min([m,n])
  for j = 0L, minmn-1 do begin
      if NOT keyword_set(pivot) then goto, HOUSE1
      
      ;; Bring the column of largest norm into the pivot position
      rmax = max(rdiag(j:*))
      kmax = where(rdiag(j:*) EQ rmax, ct) + j
      if ct LE 0 then goto, HOUSE1
      kmax = kmax(0)
      
      ;; Exchange rows
      temp       = a(*,j)    & a(*,j)     = a(*,kmax)  & a(*,kmax)  = temp
      rdiag(kmax) = rdiag(j)
      wa(kmax)   = wa(j)
      temp       = ipvt(j)   & ipvt(j)    = ipvt(kmax) & ipvt(kmax) = temp
      
      HOUSE1:

      ;; Compute the householder transformation to reduce the jth
      ;; column of a to a multiple of the jth unit vector
      ajnorm = mpfit_enorm(a(j:*,j))
      if ajnorm EQ 0 then goto, NEXT_ROW
      if a(j,j) LT 0 then ajnorm = -ajnorm
      
      a(j:*,j) = a(j:*,j) / ajnorm
      a(j,j) = a(j,j) + 1
      
      ;; Apply the transformation to the remaining columns
      ;; and update the norms
      if j+1 LT n then begin
          for k=j+1, n-1 do begin
              sum = total(a(j:*,k)*a(j:*,j))
              temp = sum/a(j,j)
              a(j:*,k) = a(j:*,k) - temp * a(j:*,j)

              if keyword_set(pivot) AND rdiag(k) NE 0 then begin
                  temp = a(j,k)/rdiag(k)
                  rdiag(k) = rdiag(k) * sqrt(max([-temp*temp + 0, 1]))
                  temp = rdiag(k)/wa(k)
                  if 0.05D*temp*temp LE MACHEP then begin
                      rdiag(k) = mpfit_enorm(a((j+1):*,k))
                      wa(k) = rdiag(k)
                  endif
              endif
          endfor
      endif

      NEXT_ROW:
      rdiag(j) = -ajnorm
  endfor

  return
end

;     **********
;
;     subroutine qrsolv
;
;     given an m by n matrix a, an n by n diagonal matrix d,
;     and an m-vector b, the problem is to determine an x which
;     solves the system
;
;           a*x = b ,     d*x = 0 ,
;
;     in the least squares sense.
;
;     this subroutine completes the solution of the problem
;     if it is provided with the necessary information from the
;     qr factorization, with column pivoting, of a. that is, if
;     a*p = q*r, where p is a permutation matrix, q has orthogonal
;     columns, and r is an upper triangular matrix with diagonal
;     elements of nonincreasing magnitude, then qrsolv expects
;     the full upper triangle of r, the permutation matrix p,
;     and the first n components of (q transpose)*b. the system
;     a*x = b, d*x = 0, is then equivalent to
;
;                  t       t
;           r*z = q *b ,  p *d*p*z = 0 ,
;
;     where x = p*z. if this system does not have full rank,
;     then a least squares solution is obtained. on output qrsolv
;     also provides an upper triangular matrix s such that
;
;            t   t               t
;           p *(a *a + d*d)*p = s *s .
;
;     s is computed within qrsolv and may be of separate interest.
;
;     the subroutine statement is
;
;       subroutine qrsolv(n,r,ldr,ipvt,diag,qtb,x,sdiag,wa)
;
;     where
;
;       n is a positive integer input variable set to the order of r.
;
;       r is an n by n array. on input the full upper triangle
;         must contain the full upper triangle of the matrix r.
;         on output the full upper triangle is unaltered, and the
;         strict lower triangle contains the strict upper triangle
;         (transposed) of the upper triangular matrix s.
;
;       ldr is a positive integer input variable not less than n
;         which specifies the leading dimension of the array r.
;
;       ipvt is an integer input array of length n which defines the
;         permutation matrix p such that a*p = q*r. column j of p
;         is column ipvt(j) of the identity matrix.
;
;       diag is an input array of length n which must contain the
;         diagonal elements of the matrix d.
;
;       qtb is an input array of length n which must contain the first
;         n elements of the vector (q transpose)*b.
;
;       x is an output array of length n which contains the least
;         squares solution of the system a*x = b, d*x = 0.
;
;       sdiag is an output array of length n which contains the
;         diagonal elements of the upper triangular matrix s.
;
;       wa is a work array of length n.
;
;     subprograms called
;
;       fortran-supplied ... dabs,dsqrt
;
;     argonne national laboratory. minpack project. march 1980.
;     burton s. garbow, kenneth e. hillstrom, jorge j. more
;
pro mpfit_qrsolv, r, ipvt, diag, qtb, x, sdiag

  sz = size(r)
  m = sz(1)
  n = sz(2)
  delm = lindgen(n) * (m+1) ;; Diagonal elements of r

  ;; copy r and (q transpose)*b to preserve input and initialize s.
  ;; in particular, save the diagonal elements of r in x.

  for j = 0L, n-1 do $
    r(j:n-1,j) = r(j,j:n-1)
  x = r(delm)
  wa = qtb
  zero = qtb(0)*0
  half = zero + 0.5
  quart = zero + 0.25

  ;; Eliminate the diagonal matrix d using a givens rotation
  for j = 0L, n-1 do begin
      l = ipvt(j)
      if diag(l) EQ 0 then goto, STORE_RESTORE
      sdiag(j:*) = 0
      sdiag(j) = diag(l)

      ;; The transformations to eliminate the row of d modify only a
      ;; single element of (q transpose)*b beyond the first n, which
      ;; is initially zero.

      qtbpj = zero
      for k = j, n-1 do begin
          if sdiag(k) EQ 0 then goto, ELIM_NEXT_LOOP
          if abs(r(k,k)) LT abs(sdiag(k)) then begin
              cotan = r(k,k)/sdiag(k)
              sin = half/sqrt(quart + quart*cotan*cotan)
              cos = sin*cotan
          endif else begin
              tan = sdiag(k)/r(k,k)
              cos = half/sqrt(quart + quart*tan*tan)
              sin = cos*tan
          endelse
          
          ;; Compute the modified diagonal element of r and the
          ;; modified element of ((q transpose)*b,0).
          r(k,k) = cos*r(k,k) + sin*sdiag(k)
          temp = cos*wa(k) + sin*qtbpj
          qtbpj = -sin*wa(k) + cos*qtbpj
          wa(k) = temp

          ;; Accumulate the transformation in the row of s
          if n GT k+1 then begin
              temp = cos*r(k+1:n-1,k) + sin*sdiag(k+1:n-1)
              sdiag(k+1:n-1) = -sin*r(k+1:n-1,k) + cos*sdiag(k+1:n-1)
              r(k+1:n-1,k) = temp
          endif
ELIM_NEXT_LOOP:
      endfor

STORE_RESTORE:
      sdiag(j) = r(j,j)
      r(j,j) = x(j)
  endfor

  ;; Solve the triangular system for z.  If the system is singular
  ;; then obtain a least squares solution
  nsing = n
  wh = where(sdiag EQ 0, ct)
  if ct GT 0 then begin
      nsing = wh(0)
      wa(nsing:*) = 0
  endif

  if nsing GE 1 then begin
      wa(nsing-1) = wa(nsing-1)/sdiag(nsing-1) ;; Degenerate case
      for j=nsing-2,0,-1 do begin  ;; Reverse loop
          sum = total(r(j+1:nsing-1,j)*wa(j+1:nsing-1))
          wa(j) = (wa(j)-sum)/sdiag(j)
      endfor
  endif

  ;; Permute the components of z back to components of x
  x(ipvt) = wa

  return
end
      
  
;
;     subroutine lmpar
;
;     given an m by n matrix a, an n by n nonsingular diagonal
;     matrix d, an m-vector b, and a positive number delta,
;     the problem is to determine a value for the parameter
;     par such that if x solves the system
;
;	    a*x = b ,	  sqrt(par)*d*x = 0 ,
;
;     in the least squares sense, and dxnorm is the euclidean
;     norm of d*x, then either par is zero and
;
;	    (dxnorm-delta) .le. 0.1*delta ,
;
;     or par is positive and
;
;	    abs(dxnorm-delta) .le. 0.1*delta .
;
;     this subroutine completes the solution of the problem
;     if it is provided with the necessary information from the
;     qr factorization, with column pivoting, of a. that is, if
;     a*p = q*r, where p is a permutation matrix, q has orthogonal
;     columns, and r is an upper triangular matrix with diagonal
;     elements of nonincreasing magnitude, then lmpar expects
;     the full upper triangle of r, the permutation matrix p,
;     and the first n components of (q transpose)*b. on output
;     lmpar also provides an upper triangular matrix s such that
;
;	     t	 t		     t
;	    p *(a *a + par*d*d)*p = s *s .
;
;     s is employed within lmpar and may be of separate interest.
;
;     only a few iterations are generally needed for convergence
;     of the algorithm. if, however, the limit of 10 iterations
;     is reached, then the output par will contain the best
;     value obtained so far.
;
;     the subroutine statement is
;
;	subroutine lmpar(n,r,ldr,ipvt,diag,qtb,delta,par,x,sdiag,
;			 wa1,wa2)
;
;     where
;
;	n is a positive integer input variable set to the order of r.
;
;	r is an n by n array. on input the full upper triangle
;	  must contain the full upper triangle of the matrix r.
;	  on output the full upper triangle is unaltered, and the
;	  strict lower triangle contains the strict upper triangle
;	  (transposed) of the upper triangular matrix s.
;
;	ldr is a positive integer input variable not less than n
;	  which specifies the leading dimension of the array r.
;
;	ipvt is an integer input array of length n which defines the
;	  permutation matrix p such that a*p = q*r. column j of p
;	  is column ipvt(j) of the identity matrix.
;
;	diag is an input array of length n which must contain the
;	  diagonal elements of the matrix d.
;
;	qtb is an input array of length n which must contain the first
;	  n elements of the vector (q transpose)*b.
;
;	delta is a positive input variable which specifies an upper
;	  bound on the euclidean norm of d*x.
;
;	par is a nonnegative variable. on input par contains an
;	  initial estimate of the levenberg-marquardt parameter.
;	  on output par contains the final estimate.
;
;	x is an output array of length n which contains the least
;	  squares solution of the system a*x = b, sqrt(par)*d*x = 0,
;	  for the output par.
;
;	sdiag is an output array of length n which contains the
;	  diagonal elements of the upper triangular matrix s.
;
;	wa1 and wa2 are work arrays of length n.
;
;     subprograms called
;
;	minpack-supplied ... dpmpar,enorm,qrsolv
;
;	fortran-supplied ... dabs,dmax1,dmin1,dsqrt
;
;     argonne national laboratory. minpack project. march 1980.
;     burton s. garbow, kenneth e. hillstrom, jorge j. more
;
function mpfit_lmpar, r, ipvt, diag, qtb, delta, x, sdiag, par=par

  sz = size(r)
  mch = machar(double=(sz(sz(0)+1) EQ 5))
  MACHEP = mch.eps
  DWARF = mch.xmin
  sz = size(r)
  m = sz(1)
  n = sz(2)
  delm = lindgen(n) * (m+1) ;; Diagonal elements of r

  ;; Compute and store in x the gauss-newton direction.  If the
  ;; jacobian is rank-deficient, obtain a least-squares solution
  nsing = n
  wa1 = qtb
  wh = where(r(delm) EQ 0, ct)
  if ct GT 0 then begin
      nsing = wh(0)
      wa1(nsing:*) = 0
  endif

  if nsing GE 1 then begin
      for j=nsing-1,0,-1 do begin  ;; Reverse loop
          wa1(j) = wa1(j)/r(j,j)
          if (j-1 GE 0) then $
            wa1(0:(j-1)) = wa1(0:(j-1)) - r(0:(j-1),j)*wa1(j)
      endfor
  endif

  ;; Note: ipvt here is a permutation array
  x(ipvt) = wa1

  ;; Initialize the iteration counter.  Evaluate the function at the
  ;; origin, and test for acceptance of the gauss-newton direction
  iter = 0L
  wa2 = diag * x
  dxnorm = mpfit_enorm(wa2)
  fp = dxnorm - delta
  if fp LE 0.1 then goto, TERMINATE

  ;; If the jacobian is not rank deficient, the newton step provides a
  ;; lower bound, parl, for the zero of the function.  Otherwise set
  ;; this bound to zero.
  
  zero = wa2(0)*0
  parl = zero
  if nsing GE n then begin
      wa1 = diag(ipvt)*wa2(ipvt)/dxnorm

      wa1(0) = wa1(0) / r(0,0) ;; Degenerate case 
      for j=1L, n-1 do begin   ;; Note "1" here, not zero
          sum = total(r(0:(j-1),j)*wa1(0:(j-1)))
          wa1(j) = (wa1(j) - sum)/r(j,j)
      endfor

      temp = mpfit_enorm(wa1)
      parl = ((fp/delta)/temp)/temp
  endif

  ;; Calculate an upper bound, paru, for the zero of the function
  for j=0, n-1 do begin
      sum = total(r(0:j,j)*qtb(0:j))
      wa1(j) = sum/diag(ipvt(j))
  endfor
  gnorm = mpfit_enorm(wa1)
  paru  = gnorm/delta
  if paru EQ 0 then paru = DWARF/min([delta,0.1])

  ;; If the input par lies outside of the interval (parl,paru), set
  ;; par to the closer endpoint

  par = max([par,parl])
  par = min([par,paru])
  if par EQ 0 then par = gnorm/dxnorm

  ;; Beginning of an interation
  ITERATION:
  iter = iter + 1
  
  ;; Evaluate the function at the current value of par
  if par EQ 0 then par = max([DWARF, paru*0.001])
  temp = sqrt(par)
  wa1 = temp * diag
  mpfit_qrsolv, r, ipvt, wa1, qtb, x, sdiag
  wa2 = diag*x
  dxnorm = mpfit_enorm(wa2)
  temp = fp
  fp = dxnorm - delta

  if (abs(fp) LE 0.1D*delta) $
    OR ((parl EQ 0) AND (fp LE temp) AND (temp LT 0)) $
    OR (iter EQ 10) then goto, TERMINATE

  ;; Compute the newton correction
  wa1 = diag(ipvt)*wa2(ipvt)/dxnorm

  for j=0,n-2 do begin
      wa1(j) = wa1(j)/sdiag(j)
      wa1(j+1:n-1) = wa1(j+1:n-1) - r(j+1:n-1,j)*wa1(j)
  endfor
  wa1(n-1) = wa1(n-1)/sdiag(n-1) ;; Degenerate case

  temp = mpfit_enorm(wa1)
  parc = ((fp/delta)/temp)/temp

  ;; Depending on the sign of the function, update parl or paru
  if fp GT 0 then parl = max([parl,par])
  if fp LT 0 then paru = min([paru,par])

  ;; Compute an improved estimate for par
  par = max([parl, par+parc])

  ;; End of an iteration
  goto, ITERATION
  
TERMINATE:
  ;; Termination
  if iter EQ 0 then return, par(0)*0
  return, par
end

pro mpfit_defiter, fcn, x, iter, FUNCTARGS=fcnargs, fmt=fmt, $
         quiet=quiet, _EXTRA=iterargs

  if keyword_set(quiet) then return
  fvec = call_function(fcn, x, _EXTRA=fcnargs)
  fnorm = mpfit_enorm(fvec)

  print, iter, fnorm^2, $
    format='("Iter ",I6,"   CHI-SQUARE = ",G20.8)'
  if n_elements(fmt) GT 0 then begin
      print, p, format=fmt
  endif else begin
      p = '  P('+strtrim(lindgen(n_elements(x)),2)+') = ' $
        + strtrim(string(x,format='(G20.6)'),2) + '  '
      print, '         '+p, format='(A)'
  endelse
  
  return
end

;     **********
;
;     subroutine covar
;
;     given an m by n matrix a, the problem is to determine
;     the covariance matrix corresponding to a, defined as
;
;                    t
;           inverse(a *a) .
;
;     this subroutine completes the solution of the problem
;     if it is provided with the necessary information from the
;     qr factorization, with column pivoting, of a. that is, if
;     a*p = q*r, where p is a permutation matrix, q has orthogonal
;     columns, and r is an upper triangular matrix with diagonal
;     elements of nonincreasing magnitude, then covar expects
;     the full upper triangle of r and the permutation matrix p.
;     the covariance matrix is then computed as
;
;                      t     t
;           p*inverse(r *r)*p  .
;
;     if a is nearly rank deficient, it may be desirable to compute
;     the covariance matrix corresponding to the linearly independent
;     columns of a. to define the numerical rank of a, covar uses
;     the tolerance tol. if l is the largest integer such that
;
;           abs(r(l,l)) .gt. tol*abs(r(1,1)) ,
;
;     then covar computes the covariance matrix corresponding to
;     the first l columns of r. for k greater than l, column
;     and row ipvt(k) of the covariance matrix are set to zero.
;
;     the subroutine statement is
;
;       subroutine covar(n,r,ldr,ipvt,tol,wa)
;
;     where
;
;       n is a positive integer input variable set to the order of r.
;
;       r is an n by n array. on input the full upper triangle must
;         contain the full upper triangle of the matrix r. on output
;         r contains the square symmetric covariance matrix.
;
;       ldr is a positive integer input variable not less than n
;         which specifies the leading dimension of the array r.
;
;       ipvt is an integer input array of length n which defines the
;         permutation matrix p such that a*p = q*r. column j of p
;         is column ipvt(j) of the identity matrix.
;
;       tol is a nonnegative input variable used to define the
;         numerical rank of a in the manner described above.
;
;       wa is a work array of length n.
;
;     subprograms called
;
;       fortran-supplied ... dabs
;
;     argonne national laboratory. minpack project. august 1980.
;     burton s. garbow, kenneth e. hillstrom, jorge j. more
;
;     **********
function mpfit_covar, rr, ipvt, tol=tol

  if n_elements(tol) EQ 0 then tol = 1.D-14
  sz = size(rr)
  if sz(0) NE 2 then begin
      message, 'ERROR: r must be a two-dimensional matrix'
      return, -1L
  endif
  n = sz(1)
  if n NE sz(2) then begin
      message, 'ERROR: r must be a square matrix'
      return, -1L
  endif

  if n_elements(ipvt) EQ 0 then ipvt = lindgen(n)
  r = rr
  
  ;; For the inverse of r in the full upper triangle of r
  l = -1L
  tolr = tol * abs(r(0,0))
  zero = r(0,0) * 0.
  one  = zero + 1.
  for k = 0L, n-1 do begin
      if abs(r(k,k)) LE tolr then goto, INV_END_LOOP
      r(k,k) = one/r(k,k)
      for j = 0L, k-1 do begin
          temp = r(k,k) * r(j,k)
          r(j,k) = zero
          r(0:j,k) = r(0:j,k) - temp*r(0:j,j)
      endfor
      l = k
  endfor
  INV_END_LOOP:

  ;; Form the full upper triangle of the inverse of (r transpose)*r
  ;; in the full upper triangle of r
  if l GE 0 then $
    for k = 0L, l do begin
      for j = 0L, k-1 do begin
          temp = r(j,k)
          r(0:j,j) = r(0:j,j) + temp*r(0:j,k)
      endfor
      temp = r(k,k)
      r(0:k,k) = temp * r(0:k,k)
  endfor

  ;; For the full lower triangle of the covariance matrix
  ;; in the strict lower triangle or and in wa
  wa = replicate(r(0,0), n)
  for j = 0L, n-1 do begin
      jj = ipvt(j)
      sing = j GT l
      for i = 0L, j do begin
          if sing then r(i,j) = zero
          ii = ipvt(i)
          if ii GT jj then r(ii,jj) = r(i,j)
          if ii LT jj then r(jj,ii) = r(i,j)
      endfor
      wa(jj) = r(j,j)
  endfor

  ;; Symmetrize the covariance matrix in r
  for j = 0L, n-1 do begin
      r(0:j,j) = r(j,0:j)
      r(j,j) = wa(j)
  endfor

  return, r
end

;     **********
;
;     subroutine lmdif
;
;     the purpose of lmdif is to minimize the sum of the squares of
;     m nonlinear functions in n variables by a modification of
;     the levenberg-marquardt algorithm. the user must provide a
;     subroutine which calculates the functions. the jacobian is
;     then calculated by a forward-difference approximation.
;
;     the subroutine statement is
;
;	subroutine lmdif(fcn,m,n,x,fvec,ftol,xtol,gtol,maxfev,epsfcn,
;			 diag,mode,factor,nprint,info,nfev,fjac,
;			 ldfjac,ipvt,qtf,wa1,wa2,wa3,wa4)
;
;     where
;
;	fcn is the name of the user-supplied subroutine which
;	  calculates the functions. fcn must be declared
;	  in an external statement in the user calling
;	  program, and should be written as follows.
;
;	  subroutine fcn(m,n,x,fvec,iflag)
;	  integer m,n,iflag
;	  double precision x(n),fvec(m)
;	  ----------
;	  calculate the functions at x and
;	  return this vector in fvec.
;	  ----------
;	  return
;	  end
;
;	  the value of iflag should not be changed by fcn unless
;	  the user wants to terminate execution of lmdif.
;	  in this case set iflag to a negative integer.
;
;	m is a positive integer input variable set to the number
;	  of functions.
;
;	n is a positive integer input variable set to the number
;	  of variables. n must not exceed m.
;
;	x is an array of length n. on input x must contain
;	  an initial estimate of the solution vector. on output x
;	  contains the final estimate of the solution vector.
;
;	fvec is an output array of length m which contains
;	  the functions evaluated at the output x.
;
;	ftol is a nonnegative input variable. termination
;	  occurs when both the actual and predicted relative
;	  reductions in the sum of squares are at most ftol.
;	  therefore, ftol measures the relative error desired
;	  in the sum of squares.
;
;	xtol is a nonnegative input variable. termination
;	  occurs when the relative error between two consecutive
;	  iterates is at most xtol. therefore, xtol measures the
;	  relative error desired in the approximate solution.
;
;	gtol is a nonnegative input variable. termination
;	  occurs when the cosine of the angle between fvec and
;	  any column of the jacobian is at most gtol in absolute
;	  value. therefore, gtol measures the orthogonality
;	  desired between the function vector and the columns
;	  of the jacobian.
;
;	maxfev is a positive integer input variable. termination
;	  occurs when the number of calls to fcn is at least
;	  maxfev by the end of an iteration.
;
;	epsfcn is an input variable used in determining a suitable
;	  step length for the forward-difference approximation. this
;	  approximation assumes that the relative errors in the
;	  functions are of the order of epsfcn. if epsfcn is less
;	  than the machine precision, it is assumed that the relative
;	  errors in the functions are of the order of the machine
;	  precision.
;
;	diag is an array of length n. if mode = 1 (see
;	  below), diag is internally set. if mode = 2, diag
;	  must contain positive entries that serve as
;	  multiplicative scale factors for the variables.
;
;	mode is an integer input variable. if mode = 1, the
;	  variables will be scaled internally. if mode = 2,
;	  the scaling is specified by the input diag. other
;	  values of mode are equivalent to mode = 1.
;
;	factor is a positive input variable used in determining the
;	  initial step bound. this bound is set to the product of
;	  factor and the euclidean norm of diag*x if nonzero, or else
;	  to factor itself. in most cases factor should lie in the
;	  interval (.1,100.). 100. is a generally recommended value.
;
;	nprint is an integer input variable that enables controlled
;	  printing of iterates if it is positive. in this case,
;	  fcn is called with iflag = 0 at the beginning of the first
;	  iteration and every nprint iterations thereafter and
;	  immediately prior to return, with x and fvec available
;	  for printing. if nprint is not positive, no special calls
;	  of fcn with iflag = 0 are made.
;
;	info is an integer output variable. if the user has
;	  terminated execution, info is set to the (negative)
;	  value of iflag. see description of fcn. otherwise,
;	  info is set as follows.
;
;	  info = 0  improper input parameters.
;
;	  info = 1  both actual and predicted relative reductions
;		    in the sum of squares are at most ftol.
;
;	  info = 2  relative error between two consecutive iterates
;		    is at most xtol.
;
;	  info = 3  conditions for info = 1 and info = 2 both hold.
;
;	  info = 4  the cosine of the angle between fvec and any
;		    column of the jacobian is at most gtol in
;		    absolute value.
;
;	  info = 5  number of calls to fcn has reached or
;		    exceeded maxfev.
;
;	  info = 6  ftol is too small. no further reduction in
;		    the sum of squares is possible.
;
;	  info = 7  xtol is too small. no further improvement in
;		    the approximate solution x is possible.
;
;	  info = 8  gtol is too small. fvec is orthogonal to the
;		    columns of the jacobian to machine precision.
;
;	nfev is an integer output variable set to the number of
;	  calls to fcn.
;
;	fjac is an output m by n array. the upper n by n submatrix
;	  of fjac contains an upper triangular matrix r with
;	  diagonal elements of nonincreasing magnitude such that
;
;		 t     t	   t
;		p *(jac *jac)*p = r *r,
;
;	  where p is a permutation matrix and jac is the final
;	  calculated jacobian. column j of p is column ipvt(j)
;	  (see below) of the identity matrix. the lower trapezoidal
;	  part of fjac contains information generated during
;	  the computation of r.
;
;	ldfjac is a positive integer input variable not less than m
;	  which specifies the leading dimension of the array fjac.
;
;	ipvt is an integer output array of length n. ipvt
;	  defines a permutation matrix p such that jac*p = q*r,
;	  where jac is the final calculated jacobian, q is
;	  orthogonal (not stored), and r is upper triangular
;	  with diagonal elements of nonincreasing magnitude.
;	  column j of p is column ipvt(j) of the identity matrix.
;
;	qtf is an output array of length n which contains
;	  the first n elements of the vector (q transpose)*fvec.
;
;	wa1, wa2, and wa3 are work arrays of length n.
;
;	wa4 is a work array of length m.
;
;     subprograms called
;
;	user-supplied ...... fcn
;
;	minpack-supplied ... dpmpar,enorm,fdjac2,lmpar,qrfac
;
;	fortran-supplied ... dabs,dmax1,dmin1,dsqrt,mod
;
;     argonne national laboratory. minpack project. march 1980.
;     burton s. garbow, kenneth e. hillstrom, jorge j. more
;
;     **********
function mpfit, fcn, xall, FUNCTARGS=fcnargs, $
                ftol=ftol, xtol=xtol, gtol=gtol, epsfcn=epsfcn, $
                nfev=nfev, maxiter=maxiter, errmsg=errmsg, $
                factor=factor, nprint=nprint, STATUS=info, $
                iterproc=iterproc, iterargs=iterargs, $
                diag=diag, rescale=rescale, $
                perror=perror, covar=covar, nocovar=nocovar, bestnorm=fnorm, $
                parinfo=parinfo, quiet=quiet, nocatch=nocatch

  if n_elements(ftol) EQ 0 then ftol = 1.D-10
  if n_elements(xtol) EQ 0 then xtol = 1.D-10
  if n_elements(gtol) EQ 0 then gtol = 1.D-10
  if n_elements(factor) EQ 0 then factor = 100.
  if n_elements(nprint) EQ 0 then nprint = 1
  if n_elements(iterproc) EQ 0 then iterproc = 'mpfit_defiter'

  info = 0L
  iflag = 0L
  nfev = 0L
  errmsg = ''
  fnorm  = -1.D
  fnorm1 = -1.D

  ;; Handle error conditions gracefully
  if NOT keyword_set(nocatch) then begin
      catch, catcherror
      if catcherror NE 0 then begin
          catch, /cancel
          message, !err_string, /info
          message, 'Error condition detected. Returning to MAIN level.', /info
          return, !values.d_nan
      endif
  endif

  ;; Parinfo:
  ;; --------------- STARTING/CONFIG INFO (passed in to routine, not changed)
  ;; .value   - starting value for parameter
  ;; .fixed   - parameter is fixed
  ;; .limited - a two-element array, if parameter is bounded on
  ;;            lower/upper side
  ;; .limits - a two-element array, lower/upper parameter bounds, if
  ;;           limited vale is set
  ;; .step   - step size in Jacobian calc, if greater than zero

  ;; Parameters can either be stored in parinfo, or x.  Parinfo takes
  ;; precedence if it exists.
  if n_elements(xall) EQ 0 AND n_elements(parinfo) EQ 0 then begin
      errmsg = 'ERROR: must pass parameters in P or PARINFO'
      goto, TERMINATE
  endif
  if n_elements(xall) EQ 0 then begin
      xall = parinfo(*).value
      sz = size(xall)
      ;; Convert to double if parameters are not float or double
      if sz(sz(0)+1) NE 4 AND sz(sz(0)+1) NE 5 then $
        xall = double(xall)
  endif
  if n_elements(parinfo) EQ 0 then begin
      parinfo = replicate({value:0.D, fixed:0, $
                           limited:[0,0], limits:[0.D,0], step:0.D}, $
                           n_elements(xall))
      parinfo(*).value = xall
  endif
  ;; Error checking on parinfo
  wh = where( (parinfo(*).limited(0) AND xall LT parinfo(*).limits(0)) $
              OR (parinfo(*).limited(1) AND xall GT parinfo(*).limits(1)), ct)
  if ct GT 0 then begin
      errmsg = 'ERROR: parameters are not within PARINFO limits'
      goto, TERMINATE
  endif
  wh = where(parinfo(*).limited(0) AND parinfo(*).limited(1) $
             AND parinfo(*).limits(0) GE parinfo(*).limits(1) $
             AND NOT parinfo(*).fixed, ct)
  if ct GT 0 then begin
      errmsg = 'ERROR: PARINFO parameter limits are not consistent'
      goto, TERMINATE
  endif

  ;; Get freely adjustable parameters
  ifree = where(parinfo(*).fixed EQ 0, ct)
  if ct EQ 0 then begin
      errmsg = 'ERROR: no free parameters'
      goto, TERMINATE
  endif
  xnew = xall      ;; xnew is the set of parameters to be returned
  x = xnew(ifree)  ;; x is the set of free parameters
  qulim = parinfo(ifree).limited(1)
  ulim  = parinfo(ifree).limits(1)
  qllim = parinfo(ifree).limited(0)
  llim  = parinfo(ifree).limits(0)
  step  = parinfo(ifree).step

  n = n_elements(x)
  if n_elements(maxiter) EQ 0 then maxiter = 200L

  ;; Check input parameters for errors
  if (n LE 0) OR (ftol LT 0) OR (xtol LT 0) OR (gtol LT 0) $
    OR (maxiter LE 0) OR (factor LE 0) then begin
      errmsg = 'ERROR: input keywords are inconsistent'
      goto, TERMINATE
  endif
 
  if keyword_set(rescale) then begin
      errmsg = 'ERROR: DIAG parameter scales are inconsistent'
      if n_elements(diag) LT n then goto, TERMINATE
      wh = where(diag LE 0, ct)
      if ct GT 0 then goto, TERMINATE
      errmsg = ''
  endif

  !err = 1
  fvec = call_function(fcn, xnew, _EXTRA=fcnargs)
  nfev = 1
  iflag = !err
  !err = 0
  if iflag LT 0 then begin
      errmsg = 'ERROR: first call to "'+fcn+'" failed'
      goto, TERMINATE
  endif

  sz = size(fvec(0))
  mch = machar(double=(sz(sz(0)+1) EQ 5))
  MACHEP = mch.eps
  DWARF = mch.xmin
  szx = size(x)
  ;; The parameters and the squared deviations should have the same
  ;; type.  Otherwise the MACHAR-based evaluation will fail.
  if sz(sz(0)+1) EQ 5 AND szx(szx(0)+1) NE 5 then begin
      if NOT keyword_set(quiet) then begin
          message, 'WARNING: data is DOUBLE but parameters are FLOAT', /info
          message, '         (converting parameters to DOUBLE)', /info
      endif
      x = double(x)
      xnew = double(xnew)
  endif

  m = n_elements(fvec)
  if (m LT n) then begin
      errmsg = 'ERROR: number of parameters must not exceed data'
      goto, TERMINATE
  endif
  
  fnorm = mpfit_enorm(fvec)

  ;; Initialize Levelberg-Marquardt parameter and iteration counter

  par = x(0) * 0
  iter = 1L
  qtf = x * 0.

  ;; Beginning of the outer loop
  
  OUTER_LOOP:

  ;; If requested, call fcn to enable printing of iterates

  xnew(ifree) = x
  if nprint GT 0 AND iterproc NE '' then begin
      iflag = 0L
      if (iter-1) MOD nprint EQ 0 then begin
          call_procedure, iterproc, fcn, xnew, iter, $
            FUNCTARGS=fcnargs, parinfo=parinfo, quiet=quiet, _EXTRA=iterargs
          iflag = !err
          !err = 0
          if iflag LT 0 then begin
              errmsg = 'WARNING: premature termination by "'+iterproc+'"'
              goto, TERMINATE
          endif
      endif
  endif
  x = xnew(ifree)

  ;; Calculate the jacobian matrix
  iflag = 2
  fjac = mpfit_fdjac2(fcn, x, fvec, step, qulim, ulim, $
                iflag=iflag, epsfcn=epsfcn, nfev=nfev, $
                FUNCTARGS=fcnargs, ifree=ifree, xall=xnew)
  if iflag LT 0 then begin
      errmsg = 'WARNING: premature termination by FDJAC2'
      goto, TERMINATE
  endif

  ;; Determine if any of the parameters are pegged at the limits
  whlpeg = where(qllim AND (x EQ llim), nlpeg)
  whupeg = where(qulim AND (x EQ ulim), nupeg)
  
  ;; See if any "pegged" values should keep their derivatives
  if (nlpeg GT 0) then begin
      ;; Total derivative of sum wrt lower pegged parameters
      for i = 0, nlpeg-1 do begin
          sum = total(fvec * fjac(*,whlpeg(i)))
          if sum GT 0 then fjac(*,whlpeg(i)) = 0
      endfor
  endif
  if (nupeg GT 0) then begin
      ;; Total derivative of sum wrt upper pegged parameters
      for i = 0, nupeg-1 do begin
          sum = total(fvec * fjac(*,whupeg(i)))
          if sum LT 0 then fjac(*,whupeg(i)) = 0
      endfor
  endif

  ;; Compute the QR factorization of the jacobian
  mpfit_qrfac, fjac, ipvt, wa1, wa2, /pivot

  ;; On the first iteration if "diag" is unspecified, scale
  ;; according to the norms of the columns of the initial jacobian
  if (iter EQ 1) then begin

      if NOT keyword_set(rescale) OR (n_elements(diag) LT n) then begin
          diag = wa2
          wh = where (diag EQ 0, ct)
          if ct GT 0 then diag(wh) = 1.D
      endif
      
      ;; On the first iteration, calculate the norm of the scaled x
      ;; and initialize the step bound delta 
      wa3 = diag * x
      xnorm = mpfit_enorm(wa3)
      delta = factor*xnorm
      if delta EQ 0.D then delta = delta(0)*0 + factor
  endif

  ;; Form (q transpose)*fvec and store the first n components in qtf
  wa4 = fvec
  for j=0L, n-1 do begin
      temp3 = fjac(j,j)
      if temp3 NE 0 then begin
          sum = total(fjac(j:*,j)*wa4(j:*))
          temp = -sum/temp3
          wa4(j:*) = wa4(j:*) + fjac(j:*,j) * temp
      endif
      fjac(j,j) = wa1(j)
      qtf(j) = wa4(j)
  endfor

  ;; Compute the norm of the scaled gradient
  gnorm = wa2(0) * 0
  if fnorm NE 0 then begin
      for j=0L, n-1 do begin
          l = ipvt(j)
          if wa2(l) NE 0 then begin
              sum = total(fjac(0:j,j)*qtf(0:j)/fnorm)
              gnorm = max([gnorm,abs(sum/wa2(l))])
          endif
      endfor
  endif

  ;; Test for convergence of the gradient norm
  if gnorm LE gtol then info = 4
  if info NE 0 then goto, TERMINATE

  ;; Rescale if necessary
  if NOT keyword_set(rescale) then $
    diag = diag > wa2

  ;; Beginning of the inner loop
  INNER_LOOP:
  
  ;; Determine the levenberg-marquardt parameter
  par = mpfit_lmpar(fjac, ipvt, diag, qtf, delta, wa1, wa2, par=par)

  ;; Store the direction p and x+p. Calculate the norm of p
  wa1 = -wa1

  ;; Do not allow any steps out of bounds
  if nlpeg GT 0 then wa1(whlpeg) = wa1(whlpeg) > 0
  if nupeg GT 0 then wa1(whupeg) = wa1(whupeg) < 0

  ;; Respect the limits.  If a step were to go out of bounds, then
  ;; we should take a step in the same direction but shorter distance.
  ;; The step should take us right to the limit in that case.
  alpha = x(0)*0 + 1.
  whl = where((abs(wa1) GT MACHEP) AND qllim AND (x + wa1 LT llim), lct)
  if lct GT 0 then $
    alpha = min([alpha, (llim(whl)-x(whl))/wa1(whl)])
  whu = where((abs(wa1) GT MACHEP) AND qulim AND (x + wa1 GT ulim), uct)
  if uct GT 0 then $
    alpha = min([alpha, (ulim(whu)-x(whu))/wa1(whu)])

  ;; Adjust the step according to any boundaries
  wa1 = wa1 * alpha
  wa2 = x + wa1

  ;; If the step put us exactly on a boundary, make sure it is exact
  if lct GT 0 then wa2(whl) = llim(whl)
  if uct GT 0 then wa2(whu) = ulim(whu)

  wa3 = diag * wa1
  pnorm = mpfit_enorm(wa3)

  ;; On the first iteration, adjust the initial step bound
  if iter EQ 1 then delta = min([delta,pnorm])
  
  ;; Evaluate the function at x+p and calculate its norm
  !err = 1
  xnew(ifree) = wa2
  wa4 = call_function(fcn, xnew, _EXTRA=fcnargs)
  nfev = nfev +1
  iflag = !err
  !err = 0
  if iflag LT 0 then begin
      goto, TERMINATE
      errmsg = 'WARNING: premature termination by "'+fcn+'"'
  endif
  fnorm1 = mpfit_enorm(wa4)
  
  ;; Compute the scaled actual reduction
  actred = x(0)*0 - 1.
  if 0.1D * fnorm1 LT fnorm then actred = - (fnorm1/fnorm)^2 + 1.

  ;; Compute the scaled predicted reduction and the scaled directional
  ;; derivative
  for j = 0, n-1 do begin
      wa3(j) = 0;
      wa3(0:j) = wa3(0:j) + fjac(0:j,j)*wa1(ipvt(j))
  endfor

  ;; Remember, alpha is the fraction of the full LM step actually taken
  temp1 = mpfit_enorm(alpha*wa3)/fnorm
  temp2 = (sqrt(alpha*par)*pnorm)/fnorm
  half = temp1(0) * 0 + 0.5
  prered = temp1*temp1 + (temp2*temp2)/half
  dirder = -(temp1*temp1 + temp2*temp2)

  ;; Compute the ratio of the actual to the predicted reduction.
  ratio = half * 0
  tenth = half * 0 + 0.1
  if prered NE 0 then ratio = actred/prered

  ;; Update the step bound
  if ratio LE 0.25D then begin
      if actred GE 0 then temp = half $
      else temp = half*dirder/(dirder + half*actred)
      if ((0.1D*fnorm1) GE fnorm) OR (temp LT 0.1D) then temp = tenth
      delta = temp*min([delta,pnorm/tenth])
      par = par/temp
  endif else begin
      if (par EQ 0) OR (ratio GE 0.75) then begin
          delta = pnorm/half
          par = half*par
      endif
  endelse

  ;; Test for successful iteration
  if ratio GE 0.0001 then begin
      ;; Successful iteration.  Update x, fvec, and their norms
      x = wa2
      wa2 = diag * x

      fvec = wa4
      xnorm = mpfit_enorm(wa2)
      fnorm = fnorm1
      iter = iter + 1
  endif

  ;; Tests for convergence
  if (abs(actred) LE ftol) AND (prered LE ftol) $
    AND  (0.5D * ratio LE 1) then info = 1
  if delta LE xtol*xnorm then info = 2
  if (abs(actred) LE ftol) AND (prered LE ftol) $
    AND (0.5D * ratio LE 1) AND (info EQ 2) then info = 3
  if info NE 0 then goto, TERMINATE

  ;; Tests for termination and stringent tolerances
  if iter GE maxiter then info = 5
  if (abs(actred) LE MACHEP) AND (prered LE MACHEP) $
    AND (0.5*ratio LE 1) then info = 6
  if delta LE MACHEP*xnorm then info = 7
  if gnorm LE MACHEP then info = 8
  if info NE 0 then goto, TERMINATE

  ;; End of inner loop. Repeat if iteration unsuccessful
  if ratio LT 0.0001 then begin
      goto, INNER_LOOP
  endif

  ;; End of outer loop.
  goto, OUTER_LOOP

TERMINATE:
  ;; Termination, either normal or user imposed.
  if iflag LT 0 then info = iflag
  iflag = 0
  if n_elements(ifree) EQ 0 then xnew = xall else xnew(ifree) = x
  if nprint GT 0 then begin
      fvec = call_function(fcn, xnew, _EXTRA=fcnargs)
      fnorm = mpfit_enorm(fvec)
  endif

  fnorm = max([fnorm, fnorm1])
  fnorm = fnorm^2.

  covar = !values.d_nan
  ;; (very carefully) set the covariance matrix COVAR
  if info GT 0 AND NOT keyword_set(nocovar) $
    AND n_elements(n) GT 0 AND n_elements(fvec) GT 0 $
    AND n_elements(fjac) GT 0 AND n_elements(ipvt) GT 0 then begin
      sz = size(fjac)
      if n GT 0 AND sz(0) GT 1 AND sz(1) GE n AND sz(2) GE n $
        AND n_elements(ipvt) GE n then begin
          cv = mpfit_covar(fjac(0:n-1,0:n-1), ipvt(0:n-1))
          nn = n_elements(xall)
          
          ;; Fill in actual covariance matrix, accounting for fixed
          ;; parameters.
          covar = replicate(cv(0)*0, nn, nn)
          for i = 0L, n-1 do begin
              covar(ifree, ifree(i)) = cv(*,i)
          end
          
          ;; Compute errors in parameters
          i = lindgen(nn)
          perror = replicate(covar(0), nn)*0
          wh = where(covar(i,i) GE 0, ct)
          if ct GT 0 then $
            perror(wh) = sqrt(covar(wh, wh))
      endif
  endif
      
  return, xnew
end
