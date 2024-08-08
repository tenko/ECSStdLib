(*
Module with operation on `REAL` type.

Ported from Oberon System 3/4:
  ETH Oberon, Copyright 2001 ETH Zuerich Institut fuer Computersysteme, ETH Zentrum, CH-8092 Zuerich.
  Refer to the "General ETH Oberon System Source License" contract available at: http://www.oberon.ethz.ch/

Floating point format according to the IEEE standard:

Single precision: S EEEEEEEE MMMMMMMMMMMMMMMMMMMMMMM
    1 bit for the sign
    8 bits for the exponent
    23 bits for the mantissa
    32 bits = 4 bytes for one single precision floating point number
    
    The exponent is stored as an unbiased exponent, to get the real exponent (within range -126..127)
    you have to subtract 127 from the resulting number).
    The number 0 is represented as exponent = 0 and mantissa = 0.
    An exponent of 255 and a mantissa of 0 denotes infinity.
    An exponent of 255 and a mantissa of #0 denotes NaN.
*)
MODULE Real IN Std;

IMPORT SYSTEM;
IN Std IMPORT Const, Type, Char, ArrayOfChar;

CONST
    FPZero*     = Const.FPZero;
    FPNormal*   = Const.FPNormal;
    FPSubnormal*= Const.FPSubnormal;
    FPInfinite* = Const.FPInfinite;
    FPNaN*      = Const.FPNaN;
    E*          = 2.71828182845904523536;
    PI*         = 3.14159265358979323846;
    PIDIV2*     = 1.57079632679489661923;
    PIDIV4*     = 0.785398163397448309616;
    SQRT2*      = 1.41421356237309504880;
    EPS*        = 1.1920929E-7;
    MINIMUM*    = 1.175494351E-38;
    MAXIMUM*    = 3.402823466E+38;
    MaxRoundArray   = 8;
    MaxPowerArray   = 6;
    MaxFixedFloat   = 1.0E9;
    MinFixedFloat   = 1.0E-9;

TYPE
    WORD = UNSIGNED32;

VAR
    Inf- : REAL;
    NaN- : REAL;
    RoundValue : ARRAY MaxRoundArray OF REAL;
    Powers : ARRAY MaxPowerArray OF REAL;
    c11, c12, c13, c14, c15: REAL; (* sqrt *)
	c21, c22, c23, c24: REAL; (* exp *)
	c41, c42, c43, c44, c45: REAL; (* ln *)
	c31, p31, p32, p33, p30, q31, q32, q33: REAL; (* sin, cos *)
	c51, s51, c52, s52, p50, q52, q51, q50: REAL; (* atan *)

(**
Categorizes floating point value.
Return either FPNormal, FPZero, FPNaN, FPInfinite or FPSubnormal.
*)
PROCEDURE FPClassify*(x : REAL): INTEGER;
VAR w : WORD;
BEGIN
    w := WORD(SYSTEM.VAL(SET32, x) * SET32(07FFFFFFFH));
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

(** Return `TRUE` if x is Zero and `FALSE` otherwise.*)
PROCEDURE IsZero*(x : REAL): BOOLEAN;
BEGIN RETURN FPClassify(x) = FPZero
END IsZero;

(** Return `TRUE` if x is neither an infinity nor a NaN or Zero, and `FALSE` otherwise.*)
PROCEDURE IsNormal*(x : REAL): BOOLEAN;
BEGIN RETURN FPClassify(x) = FPNormal
END IsNormal;

(** Return `TRUE` if sign bit is set. *)
PROCEDURE SignBit*(x : REAL): BOOLEAN;
BEGIN RETURN SYSTEM.VAL(SET32, x) * SET32(080000000H) # {}
END SignBit;

(** Return a `REAL` with the magnitude (absolute value) of x but the sign of y. *)
PROCEDURE CopySign*(x, y : REAL): REAL;
BEGIN RETURN SYSTEM.VAL(REAL, (SYSTEM.VAL(SET32, x) * SET32(07FFFFFFFH)) + (SYSTEM.VAL(SET32, y) * SET32(080000000H)))
END CopySign;

(** Return absolute value of x. *)
PROCEDURE Abs*(x : REAL): REAL;
BEGIN RETURN SYSTEM.VAL(REAL, SYSTEM.VAL(SET32, x) * SET32(07FFFFFFFH)) 
END Abs;

(** Computes the largest integer value not greater than x *)
PROCEDURE Floor* (x: REAL) : REAL ;
BEGIN RETURN REAL(ENTIER(x))
END Floor;

(** Computes the nearest integer value to x, rounding halfway cases away from zero *)
PROCEDURE Round* (x: REAL) : REAL ;
BEGIN
    IF x = 0. THEN RETURN x;
    ELSIF x < 0. THEN RETURN -REAL(ENTIER(-x + 0.5));
    ELSE RETURN REAL(ENTIER(x + 0.5)) END
END Round;

(** Computes the sine of the angle `REAL` x in radians *)
PROCEDURE Sin*(x: REAL): REAL;
VAR n: SIGNED32; y, yy, f: REAL;
BEGIN 
    y := c31*x; n := ENTIER(y + 0.5);  (*c31 = 2/pi*)
    y := 2*(y-n); yy := y*y;
    IF ~ODD(n) THEN
        f := ((p33*yy + p32)*yy + p31) / (p30 + yy) * y
    ELSE
        f := ((q33*yy + q32)*yy + q31) / (q31 + yy)
    END ;
    IF ODD(n DIV 2) THEN f := -f END ;
    RETURN f
END Sin;

(** Computes the cosine of the angle `REAL` x in radians *)
PROCEDURE Cos*(x: REAL): REAL;
VAR n: SIGNED32; y, yy, f: REAL;
BEGIN 
    y := c31*x; n := ENTIER(y + 0.5);  (*c31 = 2/pi*)
    y := 2*(y-n); INC(n); yy := y*y;
    IF ~ODD(n) THEN
        f := ((p33*yy + p32)*yy + p31) / (p30 + yy) * y
    ELSE
        f := ((q33*yy + q32)*yy + q31) / (q31 + yy)
    END ;
    IF ODD(n DIV 2) THEN f := -f END ;
    RETURN f
END Cos;

(** Computes the arc tangent of the value `REAL` x *)
PROCEDURE ArcTan*(x: REAL): REAL;
VAR y, yy, s: REAL;
BEGIN 
    IF IsInf(x) THEN RETURN CopySign(1.0, x) END;
    y := ABS(x); s := 0;
    IF y > c51 THEN y := -1/y; s := s51
    ELSIF y > c52 THEN y := (y-1)/(y+1); s := s52
    END ;
    yy := y*y;
    y := p50 * y/(yy + q52 + q51/(yy + q50)) + s;
    IF x < 0 THEN y := -y END ;
    RETURN y
END ArcTan;

(** Computes the arc tangent of `y` / `x` using the signs of arguments to determine the correct quadrant. *)
PROCEDURE ArcTan2*(x, y: REAL): REAL;
BEGIN
    IF y # 0 THEN
        IF y > 0 THEN RETURN ArcTan(x/y)
        ELSE
            IF x < 0 THEN RETURN  ArcTan(x/y) - PI
            ELSE RETURN  ArcTan(x/y) - PI END
        END
    ELSE
        IF x > 0 THEN RETURN PI / 2
        ELSIF x < 0 THEN RETURN -PI / 2 END
    END;
    RETURN NaN
END ArcTan2;

(** Computes the square root of the `REAL` x *)
PROCEDURE Sqrt*(x: REAL): REAL;
VAR e: SIGNED32; a, s: REAL;
BEGIN 
	IF x <= 0 THEN
		IF x = 0 THEN RETURN 0 ELSE RETURN NaN END
	ELSE
		a := SYSTEM.VAL(REAL, SYSTEM.VAL(SET32, x) - {23..30} + {24..29});	(* expo(a) = 126 *)
		e := SYSTEM.LSH(SYSTEM.VAL(SIGNED32, x), -23) MOD 256 - 126;
		s := c11*a + c12 + c13/(c14 + a);
		s := (s + a/s)*0.5;
		IF ODD(e) THEN INC(e); s := c15*s END ;
		RETURN SYSTEM.VAL(REAL, SYSTEM.VAL(SIGNED32, s) + SYSTEM.LSH(e DIV 2, 23))
	END
END Sqrt;

(** Computes natural (e) logarithm of x *)
PROCEDURE Log*(x: REAL): REAL;
VAR e: SIGNED32; a: REAL;
BEGIN 
	IF x <= 0 THEN RETURN NaN
	ELSE
		a := SYSTEM.VAL(REAL, SYSTEM.VAL(SET32, x) - {23..30} + {24..29});	(* expo(a) = 126 *)
		e := SYSTEM.LSH(SYSTEM.VAL(SIGNED32, x), -23) MOD 256 - 126;
		IF a < c41 THEN a := 2*a; DEC(e) END ;
		a := (a-1)/(a+1);
		a := c42*e + a*(c43 + c44/(c45 - a*a));
		RETURN a
	END
END Log;

(** Computes e raised to the power of x *)
PROCEDURE Exp*(x: REAL): REAL;
VAR n: SIGNED32; p, y, yy: REAL;
BEGIN 
    y := c21*x;  (*1/ln(2)*)
    n := ENTIER(y + 0.5); y := y-n;
    IF y >= 0 THEN INC(n) END ;
    IF n < -128 THEN RETURN 0
    ELSIF n > 127 THEN RETURN Inf
    ELSE yy := y*y;
        p := (c22 + c23*yy)*y;
        p := p/(c24 + yy - p) + 0.5;
        IF y < 0 THEN p := 2*p END;
        RETURN SYSTEM.VAL(REAL, SYSTEM.VAL(SIGNED32, p) + SYSTEM.LSH(n, 23))
    END
END Exp;

(** Computes the tangent of the value `REAL` x in radians *)
PROCEDURE Tan* (x: REAL): REAL;
VAR neg: BOOLEAN; y, s1: REAL;
BEGIN
	y := x - ENTIER(x / PI) * PI;
	IF y > PI/2 THEN neg := TRUE ELSE neg := FALSE END;
	s1 := Sin(y);
	IF neg THEN RETURN -s1 / Sqrt(1 - s1 * s1) ELSE RETURN s1 / Sqrt(1 - s1 * s1) END
END Tan;

PROCEDURE FormatFix(VAR str : ARRAY OF CHAR; value : REAL; prec: INTEGER);
VAR
    i, digits, round, val : INTEGER;
BEGIN
    IF (prec <= 0) OR (prec > 9) THEN prec := 9 END;
    IF value < 0 THEN
        value := ABS(value);
    END;
    (* force real to less than 10 *)
    round := prec;
    digits := 1;
    WHILE value >= 10.0 DO
        IF value >= 1.0E4 THEN
            value := value / 1.0E4;
            INC(digits, 4);
        ELSIF value >= 1.0E2 THEN
            value := value / 1.0E2;
            INC(digits, 2);
        ELSE
            value := value / 10.0;
            INC(digits, 1);
        END;
    END;
    (* round off *)
    round := prec + (digits - 1);
    IF round < 0 THEN
        round := 0
    ELSIF round > MaxRoundArray - 1 THEN
        round := MaxRoundArray - 1;
    END;
    value := value + RoundValue[round];
    IF value >= 10.0 THEN
        value := value / 10.0;
        INC(digits);
    END;
    (* write digits *)
    FOR i := 1 TO digits DO
        val := ENTIER(value);
        IF i < 8 THEN
            ArrayOfChar.AppendChar(str, CHR(ORD('0') + val MOD 10)); 
        ELSE
            ArrayOfChar.AppendChar(str, '0');
        END;
        value := 10*(value - val);
    END;
    (* write fractional part *)
    IF prec >= 0 THEN
        ArrayOfChar.AppendChar(str, '.');
        INC(prec, digits);
        WHILE digits < prec DO
            val := ENTIER(value);
            IF i < 15 THEN
                ArrayOfChar.AppendChar(str, CHR(ORD('0') + val MOD 10)); 
            ELSE
                ArrayOfChar.AppendChar(str, '0');
            END;
            value := 10*(value - val);
            INC(digits);
        END;
    END;
END FormatFix;

PROCEDURE FormatSci(VAR str : ARRAY OF CHAR; value : REAL; prec: INTEGER);
VAR
    i, digits, exp, pow2, val : INTEGER;
BEGIN
    exp := 0; digits := 0;
    IF (prec <= 0) OR (prec > 9) THEN prec := 9 END;
    IF value < 0 THEN
        value := ABS(value);
    END;
    (* force the number between 0 and 10 *)
    IF value >= 10.0 THEN
        pow2 := 32;
        FOR i := MaxPowerArray - 1 TO 0 BY -1 DO
            IF value > Powers[i] THEN
                value := value / Powers[i];
                INC(exp, pow2)
            END;
            pow2 := pow2 DIV 2
        END;
    ELSIF value < 1.0 THEN
        pow2 := 32;
        FOR i := MaxPowerArray - 1 TO 0 BY -1 DO
            IF value <= (1.0 / Powers[i]) THEN
                value := value * Powers[i];
                DEC(exp, pow2);
            END;
            pow2 := pow2 DIV 2
        END;
        IF value < 1.0 THEN
            value := value * 10;
            DEC(exp)
        END;
    END;
    (* round off *)
    IF prec - 1 < MaxRoundArray THEN
        value := value + RoundValue[prec - 1]
    END;
    IF value >= 10.0 THEN
        value := value / 10.0;
        INC(exp);
    END;
    (* first digit *)
    val := ENTIER(value);
    ArrayOfChar.AppendChar(str, CHR(ORD('0') + val MOD 10));
    value := value - val;
    value := value * 10;
    digits := 1;
    (* fractional part *)
    IF prec > 1 THEN
        ArrayOfChar.AppendChar(str, ".");
        WHILE digits < prec DO
            val := ENTIER(value);
            ArrayOfChar.AppendChar(str, CHR(ORD('0') + val MOD 10));
            value := 10*(value - val);
            INC(digits)
        END;
    END;
    (* exponent *)
    ArrayOfChar.AppendChar(str, "E");
    IF exp < 0 THEN ArrayOfChar.AppendChar(str, "-")
    ELSE ArrayOfChar.AppendChar(str, "+") END;
    exp := ABS(exp);
    ArrayOfChar.AppendChar(str, CHR(ORD('0') + exp DIV 10));
    ArrayOfChar.AppendChar(str, CHR(ORD('0') + exp MOD 10));
END FormatSci;

(**
Format `REAL`.

* `prec` : Precision or zero for default value.
* `width` : Total field with. Can overflow if number is bigger.
* `flags` : `Exp` or `Fix` formatting supported. Defaults to `Fix`

The formatting flags defaults to `Right` alignment.
The `Spc` flag fills in a blank character for `+` if the number is positive.
The `Sign` flag fills in a `+` character if the number is positive.
If both `Spc` and `Sign` are given then `Sign` precedes.
*)
PROCEDURE Format*(VAR Writer : Type.Writer; value : REAL; prec: INTEGER; width: LENGTH; flags: SET);
VAR
    str : ARRAY 32 OF CHAR;
    len, left, right : LENGTH;
    class : INTEGER;
    neg : BOOLEAN;
BEGIN
    neg := value < 0;
    class := FPClassify(value);
    IF class = FPInfinite THEN
        str := "INF";
        len := 3
    ELSIF class = FPNaN THEN
        str := "NAN";
        len := 3
    ELSIF (class = FPZero) OR (class = FPSubnormal) THEN
        str := "0";
        len := 1
    ELSE
        IF (flags * Const.Exp # {}) OR ((ABS(value) > MaxFixedFloat) OR (ABS(value) < MinFixedFloat)) THEN
            FormatSci(str, value, prec);
        ELSE
            FormatFix(str, value, prec);
        END;
        len := ArrayOfChar.Length(str);
    END;
    (* sign *)
    IF class # FPNaN THEN
        IF neg THEN INC(len);
        ELSIF (flags * Const.Sign) # {} THEN INC(len)
        END;
    END;
    (* Alignment *)
    left := 0; right := 0;
    IF width > len THEN
        IF (flags * Const.Left) # {} THEN
            right := width - len
        ELSIF (flags * Const.Center) # {} THEN
            left := (width - len) DIV 2;
            right := (width - len) - left;
        ELSE (* Default to Right *)
            left := width - len
        END
    END;
    WHILE left > 0 DO Writer.WriteChar(' '); DEC(left) END;
    (* Sign *)
    IF class # FPNaN THEN
        IF neg THEN Writer.WriteChar('-');
        ELSIF (flags * Const.Sign) # {} THEN Writer.WriteChar('+')
        ELSIF (flags * Const.Spc) # {} THEN Writer.WriteChar(' ')
        END;
    END;
    Writer.WriteString(str);
    WHILE right > 0 DO Writer.WriteChar(' '); DEC(right) END
END Format;

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
    PROCEDURE Ten( e: INTEGER ): REAL; 
    VAR r: REAL;
    BEGIN
        IF e < -37 THEN RETURN 0
        ELSIF 38 < e THEN RETURN INF END;
        r := 1;
        WHILE (e > 0) DO r := r * 10;  DEC( e );  END;
        WHILE (e < 0) DO r := r / 10;  INC( e );  END;
        RETURN r;
    END Ten;
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
        IF ch = "." THEN Next; ScanFractionalPart END;
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
    IF IsFinite(y) THEN
        IF (ch = "d") OR (ch = "D") OR (ch = "e") OR (ch = "E") THEN
            Next;
            negE := FALSE; 
            IF ch = "-" THEN negE := TRUE; Next END;
            IF ch = "+" THEN Next END;
            IF ~Char.IsDigit(ch) THEN RETURN FALSE END;
            WHILE (ch = "0") DO Next END;
            WHILE ("0" <= ch) & (ch <= "9") DO
                e := e * 10 + (ORD(ch) - ORD("0"));
                Next
            END;
            IF negE THEN y := y / Ten(e)
            ELSE y := y * Ten(e)
            END;
        END
    END;
    IF neg THEN y := -y  END;
    result := y;
    RETURN i = start + j
END FromString;

BEGIN
    ASSERT(SIZE(REAL) = 4);
    RoundValue[0] := 0.5; RoundValue[1] := 5.0E-2 + 1.0E-7;
    RoundValue[2] := 5.0E-3 + 1.0E-7; RoundValue[3] := 5.0E-4 + 1.0E-7;
    RoundValue[4] := 5.0E-5 + 1.0E-7; RoundValue[5] := 5.0E-6 + 1.0E-7;
    RoundValue[6] := 5.0E-7; RoundValue[7] := 5.0E-8;
    Powers[0] := 1.0E1; Powers[1] := 1.0E2; Powers[2] := 1.0E4;
    Powers[3] := 1.0E8; Powers[4] := 1.0E16; Powers[5] := 1.0E32;
    c11 := SYSTEM.VAL(REAL, 03EAFBA81H); c12 := SYSTEM.VAL(REAL, 03F665222H);
    c13 := SYSTEM.VAL(REAL, 0BEBA6391H); c14 := SYSTEM.VAL(REAL, 03F00000EH);
    c15 := SYSTEM.VAL(REAL, 03F3504F3H);
	c21 := SYSTEM.VAL(REAL, 03FB8AA3BH); c22 := SYSTEM.VAL(REAL, 040E6E1ACH);
    c23 := SYSTEM.VAL(REAL, 03D6C5665H); c24 := SYSTEM.VAL(REAL, 041A68BBBH);
	c41 := SYSTEM.VAL(REAL, 03F3504F3H); c42 := SYSTEM.VAL(REAL, 03F317218H);
    c43 := SYSTEM.VAL(REAL, 03F654226H); c44 := SYSTEM.VAL(REAL, 03FEA3856H);
    c45 := SYSTEM.VAL(REAL, 03FD4114DH);
	c31 := SYSTEM.VAL(REAL, 03F22F983H); p31 := SYSTEM.VAL(REAL, 04253463FH);
    p32 := SYSTEM.VAL(REAL, 0C094A235H); p33 := SYSTEM.VAL(REAL, 03DB1AC59H);
    p30 := SYSTEM.VAL(REAL, 042868060H);
	q31 := SYSTEM.VAL(REAL, 0423EBFC9H); q32 := SYSTEM.VAL(REAL, 0C15B53F8H);
    q33 := SYSTEM.VAL(REAL, 03EE548F8H);
    c51 := SYSTEM.VAL(REAL, 0401A827AH); s51 := SYSTEM.VAL(REAL, 03FC90FDBH);
    c52 := SYSTEM.VAL(REAL, 03ED413CDH); s52 := SYSTEM.VAL(REAL, 03F490FDBH);
	p50 := SYSTEM.VAL(REAL, 040CBD065H); q52 := SYSTEM.VAL(REAL, 041099F6AH);
    q51 := SYSTEM.VAL(REAL, 0C08DFBCBH); q50 := SYSTEM.VAL(REAL, 03FFE6CB2H);
    Inf := SYSTEM.VAL(REAL, 07F800000H);
    NaN := SYSTEM.VAL(REAL, 07FF80000H);
END Real.