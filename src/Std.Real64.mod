(*
Module with operation on `REAL` type.

Ported from Oberon A2:
  ETH Oberon, Copyright 2001 ETH Zuerich Institut fuer Computersysteme, ETH Zentrum, CH-8092 Zuerich.
  Refer to the "General ETH Oberon System Source License" contract available at: http://www.oberon.ethz.ch/

Most functionality will eventually be replaced by porting code from Newlib/LibmCS.
*)
MODULE Real;

IMPORT SYSTEM;
IN Std IMPORT Const;

CONST
    FPZero*     = Const.FPZero;
    FPNormal*   = Const.FPNormal;
    FPSubnormal*= Const.FPSubnormal;
    FPInfinite* = Const.FPInfinite;
    FPNaN*      = Const.FPNaN;
    PI*   = 3.1415926535897932384626433832795028841972;
    E* = 2.7182818284590452353602874713526624977572;
	LN2* = 0.693147180559945309417232121458D0;
	EPS = 2.2D-16;

TYPE
    WORD = UNSIGNED32;

PROCEDURE GetWords(VAR high, low : WORD; real : REAL);
BEGIN
    SYSTEM.GET(SYSTEM.ADR(real) + SIZE(WORD), high);
    SYSTEM.GET(SYSTEM.ADR(real), low);
END GetWords;

PROCEDURE GetHighWord(real : REAL): WORD;
VAR ret : WORD;
BEGIN
    SYSTEM.GET(SYSTEM.ADR(real) + SIZE(WORD), ret);
    RETURN ret
END GetHighWord;

PROCEDURE SetHighWord(VAR real : REAL; high : WORD);
BEGIN SYSTEM.PUT(SYSTEM.ADR(real) + SIZE(WORD), high);
END SetHighWord;

PROCEDURE Expo(x: REAL): UNSIGNED32;
BEGIN RETURN UNSIGNED32(ASH(SYSTEM.VAL(UNSIGNED64, x), -52)) MOD 2048
END Expo;

PROCEDURE Mantissa(x: REAL): UNSIGNED64;
BEGIN RETURN SYSTEM.VAL(UNSIGNED64, SYSTEM.VAL(SET64, x) * {0 .. 51})
END Mantissa;

PROCEDURE Equal(x, y: REAL): BOOLEAN;
BEGIN
	IF x > y THEN x := x - y
	ELSE x := y - x END;
	RETURN x < EPS
END Equal;

(**
Categorizes floating point value.
Return either FPNormal, FPZero, FPNaN, FPInfinite or FPSubnormal.
*)
PROCEDURE FPClassify*(x : REAL): INTEGER;
VAR msw, lsw : UNSIGNED32;
BEGIN 
    GetWords(msw, lsw, x);
    msw := WORD(SET32(msw) * SET32(07FFFFFFFH));
    IF (msw = 000000000H) & (lsw = 000000000H) THEN
        RETURN FPZero
    ELSIF (msw >= 00100000H) & (msw <= 07FEFFFFFH) THEN
        RETURN FPNormal
    ELSIF msw <= 000FFFFFH THEN (* zero is already handled above *)
        RETURN FPSubnormal
    ELSIF (msw = 07FF00000H) & (lsw = 000000000H) THEN 
        RETURN FPInfinite
    ELSE
        RETURN FPNaN
    END;
END FPClassify;

(** Return `TRUE` if x is a NaN (not a number), and `FALSE` otherwise. *)
PROCEDURE IsNan*(x : REAL): BOOLEAN;
BEGIN RETURN FPClassify(x) = FPNaN
END IsNan;

(** Return `TRUE` if x is a positive or negative infinity, and `FALSE` otherwise. *)
PROCEDURE IsInf*(x : REAL): BOOLEAN;
BEGIN RETURN FPClassify(x) = FPInfinite
END IsInf;

(** Return `TRUE` if x is neither an infinity nor a NaN, and `FALSE` otherwise.*)
PROCEDURE IsFinite*(x : REAL): BOOLEAN;
BEGIN RETURN ~(IsNan(x) OR IsInf(x))
END IsFinite;

(** Return `TRUE` if x is neither an infinity nor a NaN or Zero, and `FALSE` otherwise.*)
PROCEDURE IsNormal*(x : REAL): BOOLEAN;
BEGIN RETURN FPClassify(x) = FPNormal
END IsNormal;

(** Return `TRUE` if sign bit is set. *)
PROCEDURE SignBit*(x : REAL): BOOLEAN;
VAR high : WORD;
BEGIN
    high := GetHighWord(x);
    RETURN SET32(high) * SET32(080000000H) # {}
END SignBit;

(** Return a `REAL` with the magnitude (absolute value) of x but the sign of y. *)
PROCEDURE CopySign*(x, y : LONGREAL): LONGREAL;
VAR hx, hy : WORD;
BEGIN
    hx := GetHighWord(x); hy := GetHighWord(y);
    SetHighWord(x, WORD((SET32(hx) * SET32(07FFFFFFFH)) + (SET32(hy) * SET32(080000000H))));
    RETURN x
END CopySign;

(** Return absolute value of x. *)
PROCEDURE Abs*(x : REAL): REAL;
VAR high : WORD;
BEGIN
    high := GetHighWord(x);
    SetHighWord(x, WORD(SET32(high) * SET32(07FFFFFFFH)));
    RETURN x
END Abs;

(** Computes the sine of the angle `REAL` x in radians *)
PROCEDURE Sin*(x: REAL): REAL;
VAR
	k: INTEGER;
	xk, prev, res: REAL;
BEGIN
	WHILE x >= 2 * PI DO x := x - 2*PI END;
	WHILE x < 0 DO x := x + 2*PI END;
	res := x;
	xk := x;
	k := 1;
	REPEAT
		prev := res;
		xk := -xk * x * x / (2 * k) / (2 * k + 1);
		res := res + xk;
		INC(k);
	UNTIL Equal(prev, res) OR (k = 5000);
	RETURN res
END Sin;

(** Computes the cosine of the angle `REAL` x in radians *)
PROCEDURE Cos*(x: REAL): REAL;
VAR
	k: INTEGER;
	prev, res, xk: REAL;
BEGIN
	WHILE x >= 2 * PI DO x := x - 2*PI END;
	WHILE x < 0 DO x := x + 2*PI END;
	res := 1.0;
	xk := 1.0;
	k := 1;
	REPEAT
		prev := res;
		xk := -xk * x * x / (2 * k - 1) / (2 * k);
		res := res + xk;
		INC(k, 1)
	UNTIL Equal(xk, 0.0) OR Equal(prev, res) OR (k = 5000);
	RETURN res
END Cos;

(** Computes the arc tangent of the value `REAL` x *)
PROCEDURE ArcTan(y:REAL):REAL;
VAR
	k: INTEGER;
	prev, res, term, yk: REAL;
BEGIN
	IF (y = 1) OR (y = -1) THEN
		RETURN y * PI / 4
	ELSIF (y > 1) OR (y < -1) THEN
		RETURN PI / 2 - ArcTan(1 / y)
	ELSE
		(* atan(y) = sum_k (-1)^(k) y^{2 k + 1} / (2 k + 1), |y| < 1 *)
		prev := PI / 2;
		res := 0.0;
		yk := y;
		k := 0;
		REPEAT
			prev := res;
			term := 1 / (2 * k + 1) * yk;
			IF ODD(k) THEN
				res := res - term
			ELSE
				res := res + term
			END;
			yk := yk * y * y;
			INC(k)
		UNTIL Equal(prev, res) OR (k = 50000);
		RETURN res
	END
END ArcTan;

(** Computes the arc tangent of the value `REAL` x/y using the sign to select the right quadrant *)
PROCEDURE ArcTan2*(y, x: REAL): REAL;
BEGIN
	IF (x>0) & (y>=0) THEN RETURN ArcTan(y/x)
	ELSIF (x>0) & (y<0) THEN RETURN ArcTan(y/x)
	ELSIF x<0 THEN RETURN ArcTan(y/x)-PI (*?*)
	ELSIF (x=0) & (y>0) THEN RETURN PI/2
	ELSIF (x=0) & (y<0) THEN RETURN -PI/2
	ELSE (*( x=0) & (y=0) *) RETURN 0 
	END
END ArcTan2;

(** Computes natural (e) logarithm of x *)
PROCEDURE Ln*(x: REAL): REAL;
VAR
	k: INTEGER;
	res, y, yk: REAL;
	mantissa: UNSIGNED64;
BEGIN
	IF x <= 0 THEN
		HALT(0);
	END;
	IF x < 1.0 THEN
		RETURN -Ln(1.0 / x)
	ELSIF x >= 2.0 THEN
		(*
			algorithm idea from http://stackoverflow.com/questions/10732034/how-are-logarithms-programmed
			and https://en.wikipedia.org/wiki/Natural_logarithm (Newton's method)
			ln(m * 2^e) = e ln(2) + ln(m)
		*)
		mantissa := Mantissa(x) + 3FF0000000000000H;
		RETURN (Expo(x) - 1023) * LN2 + Ln(SYSTEM.VAL(REAL, mantissa))
	ELSE
		(* ln(x) = 2 * sum_k 1/(2 k + 1) y^k, where y = (x - 1) / (x + 1), x real *)
		y := (x - 1) / (x + 1);
		yk := y;
		res := y;
		k := 1;
		REPEAT
			yk := yk * y * y;
			res := res + yk / (2 * k + 1);
			INC(k)
		UNTIL Equal(yk, 0.0) OR (k = 5000);
		RETURN 2.0 * res;
	END
END Ln;

(** Computes e raised to the power of x *)
PROCEDURE Exp*(x: REAL): REAL;
VAR
	k: INTEGER;
	prev, res, xk: REAL;
BEGIN
	IF x < 0.0 THEN
		RETURN 1.0 / Exp(-x)
	ELSE
		(* exp(x) = sum_k x^(k) / k! *)
		prev := 0.0;
		res := 1.0;
		k := 1;
		xk := 1;
		REPEAT
			prev := res;
			xk := xk / k * x;
			res := res + xk;
			INC(k, 1)
		UNTIL Equal(xk, 0.0) OR (k = 5000);
		RETURN res
	END
END Exp;

(** Computes the square root of the `REAL` x *)
PROCEDURE Sqrt*(x: REAL): REAL;
BEGIN
	IF x <= 0 THEN
		IF x = 0 THEN RETURN 0 ELSE HALT(0) END;
	END;
	RETURN Exp(0.5 * Ln(x));
END Sqrt;

BEGIN
    ASSERT(SIZE(REAL) = 8);
END Real.