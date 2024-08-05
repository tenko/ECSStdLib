(*
Module with operation on `REAL` type.

Ported from Oberon A2:
  ETH Oberon, Copyright 2001 ETH Zuerich Institut fuer Computersysteme, ETH Zentrum, CH-8092 Zuerich.
  Refer to the "General ETH Oberon System Source License" contract available at: http://www.oberon.ethz.ch/

Most functionality will eventually be replaced by porting code from Newlib/LibmCS.
*)
MODULE Real;

IMPORT SYSTEM;
IN Std IMPORT Const, Char, ArrayOfChar;

CONST
    FPZero*     = Const.FPZero;
    FPNormal*   = Const.FPNormal;
    FPSubnormal*= Const.FPSubnormal;
    FPInfinite* = Const.FPInfinite;
    FPNaN*      = Const.FPNaN;
    PI*   = 3.1415926535897932384626433832795028841972;
    E* = 2.7182818284590452353602874713526624977572;
    LN2* = 0.693147180559945309417232121458D0;
    EPS = 1.2E-7;

TYPE
    WORD = UNSIGNED32;

VAR
    Inf- : REAL;
    NaN- : REAL;

PROCEDURE Expo (x: REAL): UNSIGNED32;
BEGIN RETURN ASH(SYSTEM.VAL(UNSIGNED32, x), -23) MOD 256
END Expo;

PROCEDURE Mantissa (x: REAL): UNSIGNED32;
BEGIN RETURN SYSTEM.VAL(UNSIGNED32, SET32(x) * {0 .. 22})
END Mantissa;

PROCEDURE Equal (x, y: REAL): BOOLEAN;
BEGIN
    IF x > y THEN
        x := x - y
    ELSE
        x := y - x
    END;
    RETURN x < EPS
END Equal;

(**
Categorizes floating point value.
Return either FPNormal, FPZero, FPNaN, FPInfinite or FPSubnormal.
*)
PROCEDURE FPClassify*(x : REAL): INTEGER;
VAR w : WORD;
BEGIN
    w := WORD(SET32(x) * SET32(07FFFFFFFH));
    IF w = 000000000H THEN
        RETURN FPZero
    ELSIF (w > 000800000H) & (w < 07F7FFFFFH) THEN
        RETURN FPNormal
    ELSIF w <= 0007FFFFFH THEN
        RETURN FPSubnormal
    ELSIF w = 07F800000H THEN
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
BEGIN RETURN SET32(x) * SET32(080000000H) # {}
END SignBit;

(** Return a `REAL` with the magnitude (absolute value) of x but the sign of y. *)
PROCEDURE CopySign*(x, y : REAL): REAL;
BEGIN RETURN REAL((SET32(x) * SET32(07FFFFFFFH)) + (SET32(y) * SET32(080000000H)))
END CopySign;

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
        INC(k)
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
BEGIN
    IF x <= 0 THEN HALT(0) END;
    IF x < 1.0 THEN
        RETURN -Ln(1.0 / x)
    ELSIF x >= 2.0 THEN
        (*
            algorithm idea from http://stackoverflow.com/questions/10732034/how-are-logarithms-programmed
            and https://en.wikipedia.org/wiki/Natural_logarithm (Newton's method)

            ln(m * 2^e) = e ln(2) + ln(m)
        *)
        RETURN (Expo(x) - 127) * LN2 + Ln(REAL(Mantissa(x) + 3F800000H))
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
    IF x < 0.0 THEN RETURN 1.0 / Exp(-x)
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

PROCEDURE Ten( e: INTEGER ): REAL; 
VAR r: REAL;
BEGIN
    IF e < -38 THEN RETURN 0
    ELSIF 38 < e THEN RETURN INF END;
    r := 1;
    WHILE (e > 0) DO r := r * 10;  DEC( e );  END;
    WHILE (e < 0) DO r := r / 10;  INC( e );  END;
    RETURN r;
END Ten;

(**
Convert string `str` to `REAL` and `start` and `length` into `str`.

Return `TRUE` if success.
*)
PROCEDURE FromString*(VAR result: REAL; str-: ARRAY OF CHAR; start, length: INTEGER): BOOLEAN;
VAR
    y : REAL;
    e, i : INTEGER;
    j : LENGTH;
    ch : CHAR;
    neg, negE : BOOLEAN;
    PROCEDURE Next;
    BEGIN
        INC(i);
        IF i < LEN(str) THEN ch := str[i]
        ELSE ch := 00X END;
    END Next;
    PROCEDURE ScanFractionalPart;
    VAR g : REAL;
    BEGIN
        g := 1;
        WHILE Char.IsDigit(ch) DO
            g := g / 10;
            y := y + g * (ORD(ch) - ORD("0"));
            Next
        END;
    END ScanFractionalPart;
BEGIN
    y := 0; i := start; e := 0;
    j := length;
    IF i < 0 THEN RETURN FALSE END;
    IF j = 0 THEN j := ArrayOfChar.Length(str) END;
    ch := str[i];
    neg := FALSE; negE := FALSE;
	IF ch = "-" THEN  neg := TRUE; Next END;
    IF ch = "." THEN
        Next;
        IF Char.IsDigit(ch) THEN ScanFractionalPart;
        ELSE RETURN FALSE END
    ELSIF Char.IsDigit(ch) THEN
        WHILE (ch = "0") DO Next END;
        WHILE Char.IsDigit(ch) DO
            y := y * 10 + (ORD(ch) - ORD("0"));
            Next
        END;
        IF ch = "." THEN Next; ScanFractionalPart END
    ELSE
        IF ch = "I" THEN
            Next; IF ch # "N" THEN RETURN FALSE END;
            Next; IF ch # "F" THEN RETURN FALSE END;
            y := Inf
        ELSIF ch = "N" THEN
            Next; IF ch # "A" THEN RETURN FALSE END;
            Next; IF ch # "N" THEN RETURN FALSE END;
            IF neg THEN RETURN FALSE END;
            y := NaN
        ELSE RETURN FALSE END;
    END;
    IF (y # Inf) OR (y # NaN) THEN
        IF (ch = "d") OR (ch = "D") OR (ch = "e") OR (ch = "E") THEN
            Next;
            IF ch = "-" THEN negE := TRUE; Next END;
            IF ~Char.IsDigit(ch) THEN RETURN FALSE END;
            WHILE (ch = "0") DO Next END;
            WHILE ("0" <= ch) & (ch <= "9") DO
                e := e * 10 + (ORD(ch) - ORD("0"));
                Next
            END;
            IF negE THEN y := y / Ten(e)
            ELSE y := y * Ten(e)
            END
        END
    END;
    IF neg THEN y := -y  END;
    result := y;
    RETURN i = start + j
END StringToReal;

BEGIN
    ASSERT(SIZE(REAL) = 4);
    Inf := REAL(07FF00000H);
    NaN := REAL(07FF80000H);
END Real.