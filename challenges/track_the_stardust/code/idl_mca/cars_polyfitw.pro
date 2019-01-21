; $Id: cars_polyfitw.pro,v 1.1.1.1 2001/09/04 03:35:45 epics Exp $

FUNCTION CARS_POLYFITW,X,Y,W,NDEGREE,YFIT,YBAND,SIGMA,A
;+
; NAME:
;	POLYFITW
;
; PURPOSE:
;	Perform a least-square polynomial fit with optional error estimates.
;
; CATEGORY:
;	Curve fitting.
;
; CALLING SEQUENCE:
;	Result = POLYFITW(X, Y, W, NDegree [, Yfit, Yband, Sigma, A])
;
; INPUTS:
;	    X:	The independent variable vector.
;
;	    Y:	The dependent variable vector.  This vector should be the same 
;		length as X.
;
;	    W:	The vector of weights.  This vector should be same length as 
;		X and Y.
;
;     NDegree:	The degree of polynomial to fit.
;
; OUTPUTS:
;	POLYFITW returns a vector of coefficients of length NDegree+1.
;
; OPTIONAL OUTPUT PARAMETERS:
;	 Yfit:	The vector of calculated Y's.  Has an error of + or - Yband.
;
;	Yband:	Error estimate for each point = 1 sigma.
;
;	Sigma:	The standard deviation in Y units.
;
;	    A:	Correlation matrix of the coefficients.
;
; COMMON BLOCKS:
;	None.
;
; SIDE EFFECTS:
;	None.
;
; MODIFICATION HISTORY:
;	Written by: 	George Lawrence, LASP, University of Colorado,
;			December, 1981.
;
;	Adapted to VAX IDL by: David Stern, Jan, 1982.
;
;	Weights added, April, 1987,  G. Lawrence
;       Fixed bug with checking number of params, November, 1998, Mark Rivers
;
;-
ON_ERROR,2                      ;RETURN TO CALLER IF AN ERROR OCCURS
N = N_ELEMENTS(X) < N_ELEMENTS(Y) ; SIZE = SMALLER OF X,Y
M = NDEGREE + 1	; # OF ELEMENTS IN COEFF VEC
;
A = DBLARR(M,M) ; LEAST SQUARE MATRIX, WEIGHTED MATRIX
B = DBLARR(M)	; WILL CONTAIN SUM W*Y*X^J
Z = DBLARR(N) + 1.	; BASIS VECTOR FOR CONSTANT TERM
;
A[0,0] = TOTAL(W)
B[0] = TOTAL(W*Y)
;
FOR P = 1,2*NDEGREE DO BEGIN	; POWER LOOP
Z=Z*X	; Z IS NOW X^P
IF P LT M THEN B[P] = TOTAL(W*Y*Z)	; B IS SUM W*Y*X^J
SUM = TOTAL(W*Z)
FOR J = 0 > (P-NDEGREE), NDEGREE < P DO A[J,P-J] = SUM
	END ; END OF P LOOP, CONSTRUCTION OF A AND B
;
A = INVERT(A)
;
C = FLOAT(B # A)
;
; Mark Rivers.  Replaced LE 3 with LE 4 in following line.
IF ( N_PARAMS(0) LE 4) THEN RETURN,C	; EXIT IF NO ERROR ESTIMATES
;
; COMPUTE OPTIONAL OUTPUT PARAMETERS.
;
YFIT = FLTARR(N)+C[0]	; ONE-SIGMA ERROR ESTIMATES, INIT
FOR K = 1, NDEGREE DO YFIT = YFIT + C[K]*(X^K)	; SUM BASIS VECTORS
;
VAR = TOTAL((YFIT-Y)^2 )/(N-M)	; VARIANCE ESTIMATE, UNBIASED
;
;
SIGMA=SQRT(VAR)
YBAND = FLTARR(N) + A[0,0]
Z=FLTARR(N)+1.
FOR P=1,2*NDEGREE DO BEGIN	; COMPUTE CORRELATED ERROR ESTIMATES ON Y
	Z = Z*X		; Z IS NOW X^P
	SUM = 0.
	FOR J=0 > (P - NDEGREE), NDEGREE  < P DO SUM = SUM + A[J,P-J]
	YBAND = YBAND + SUM * Z	; ADD IN ALL THE ERROR SOURCES
ENDFOR	; END OF P LOOP
	YBAND = YBAND*VAR
	YBAND = SQRT( YBAND )
RETURN,C
END



