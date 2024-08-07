(*
Module with operation on `REAL` type.

Ported from Oberon System 3/4:
  ETH Oberon, Copyright 2001 ETH Zuerich Institut fuer Computersysteme, ETH Zentrum, CH-8092 Zuerich.
  Refer to the "General ETH Oberon System Source License" contract available at: http://www.oberon.ethz.ch/

Double precision: S EEEEEEEEEEE MMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMM
    1 bit for the sign
    11 bits for the exponent
    52 bits for the mantissa
    64 bits = 8 bytes for one double precision floating point number

    The exponent is stored as an unbiased exponent, to get the real exponent (within range -1022..1024)
    you have to subtract 1023 from the resulting number).
    The number 0 is represented as exponent = 0 and mantissa = 0.
    An exponent of 2047 and a mantissa of 0 denotes infinity.
    An exponent of 2047 and a mantissa of #0 denotes NaN.
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
    PI*   = 3.1415926535897932384626433832795028841972;
    E* = 2.7182818284590452353602874713526624977572;
    LN2* = 0.693147180559945309417232121458D0;
    MaxRoundArray   = 17;
    MaxPowerArray   = 9;
    MaxFixedFloat   = 1.0E9;
    MinFixedFloat   = 1.0E-9;

TYPE
    WORD = UNSIGNED32;

VAR
    Inf- : REAL;
    NaN- : REAL;
    RoundValue : ARRAY MaxRoundArray OF REAL;
    Powers : ARRAY MaxPowerArray OF REAL;
    c11, p10: REAL; (* sqrt *)
	c21, p22, p21, p20, q21, q20: REAL; (* exp *)
	c42, p42, p41, p40, q42, q41, q40: REAL; (* ln *)
	c31, p36, p35, p34, p33, p32, p31, p30, q36, q35, q34, q33, q32, q31, q30: REAL; (* sin, cos *)
	c51, s51, c52, s52, c53, s53, p53, p52, p51, p50, q52, q51, q50: REAL; (* atan *)

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
VAR n: SIGNED64; y, yy, f: REAL;
BEGIN 
    y := c31*x; n := ENTIER(y+0.5);  (*c31 = 2/pi*)
    y := 2*(y-n); yy := y*y;
    IF ODD(n) THEN
        f := (((((q36*yy + q35)*yy + q34)*yy + q33)*yy + q32)*yy + q31)*yy + q30
    ELSE
        f := ((((((p36*yy + p35)*yy + p34)*yy + p33)*yy + p32)*yy + p31)*yy + p30)*y
    END;
    IF ODD(n DIV 2) THEN f := -f END;
    RETURN f
END Sin;

(** Computes the cosine of the angle `REAL` x in radians *)
PROCEDURE Cos*(x: REAL): REAL;
VAR n: SIGNED64; y, yy, f: REAL;
BEGIN 
    y := c31*x; n := ENTIER(y+0.5);  (*c31 = 2/pi*)
    y := 2*(y-n); INC(n); yy := y*y;
    IF ODD(n) THEN
        f := (((((q36*yy + q35)*yy + q34)*yy + q33)*yy + q32)*yy + q31)*yy + q30
    ELSE
        f := ((((((p36*yy + p35)*yy + p34)*yy + p33)*yy + p32)*yy + p31)*yy + p30)*y
    END ;
    IF ODD(n DIV 2) THEN f := -f END;
    RETURN f
END Cos;

(** Computes the arc tangent of the value `REAL` x *)
PROCEDURE ArcTan*(x: REAL): REAL;
VAR y, yy, s: REAL;
BEGIN 
    y := ABS(x); s := 0;
    IF y > c51 THEN y := -1/y; s := s51
    ELSIF y > 1 THEN y := (y - c52)/(y*c52 + 1); s := s52
    ELSIF y > c53 THEN y := (y*c52 - 1)/(y + c52); s := s53
    END;
    yy := y*y;
    y := y*(((p53*yy + p52)*yy + p51)*yy + p50)/
        (((yy + q52)*yy + q51)*yy + q50) + s;
    IF x < 0 THEN y := -y END;
    RETURN y
END ArcTan;

(** Computes the arc tangent of `y` / `x` using the signs of arguments to determine the correct quadrant. *)
PROCEDURE ArcTan2*(x, y: REAL): REAL;
BEGIN
    IF x # 0 THEN
        IF x > 0 THEN RETURN ArcTan(y/x)
        ELSE
            IF y < 0 THEN RETURN  ArcTan(y/x) - PI
            ELSE RETURN  ArcTan(y/x) - PI END
        END
    ELSE
        IF y > 0 THEN RETURN PI / 2
        ELSIF y < 0 THEN RETURN -PI / 2 END
    END;
    RETURN NaN
END ArcTan2;

(** Computes the square root of the `REAL` x *)
PROCEDURE Sqrt*(x: REAL): REAL;
VAR h: SET64; e: SIGNED64; a, s: REAL;
BEGIN 
	IF x <= 0 THEN
		IF x = 0 THEN RETURN 0 ELSE RETURN NaN END
	ELSE
		SYSTEM.GET(SYSTEM.ADR(x)+4, h); SYSTEM.PUT(SYSTEM.ADR(x)+4, h - {20..30} + {21..29}); a := x; 	(* expo(a) = 1022 *)
		e := SYSTEM.LSH(SYSTEM.VAL(SIGNED64, h), -20) MOD 2048 - 1022;
		s := p10*(a + c11);  (*c11 = 1/sqrt(2)*)
		s := s + a/s;
		s := 0.25*s + a/s;
		s := 0.5*(s + a/s);
		IF ODD(e) THEN INC(e); s := c11*s END;
		x := s; SYSTEM.GET(SYSTEM.ADR(x)+4, h); SYSTEM.PUT(SYSTEM.ADR(x)+4, SYSTEM.VAL(SIGNED64, h) + SYSTEM.LSH(e DIV 2, 20));
		RETURN x
	END
END Sqrt;

(** Computes natural (e) logarithm of x *)
PROCEDURE Ln*(x: REAL): REAL;
VAR h: SET64; e: SIGNED64; a, aa, a1, a2: REAL;
BEGIN 
	IF x <= 0 THEN RETURN NaN
	ELSE
		SYSTEM.GET(SYSTEM.ADR(x)+4, h); SYSTEM.PUT(SYSTEM.ADR(x)+4, h - {20..30} + {21..29}); a := x; 	(* expo(a) = 1022 *)
		e := SYSTEM.LSH(SYSTEM.VAL(SIGNED64, h), -20) MOD 2048 - 1022;
		IF a < c11 THEN a := 2*a; DEC(e) END ;
		a := (a-1)/(a+1);
		aa := a*a;
		a1 := ((p42*aa + p41)*aa + p40);
		a2 := (((aa + q42)*aa + q41)*aa + q40);
		a := c42*e + a*a1/a2;
		RETURN a   (*c42 = ln(2)*)
	END
END Ln;

(** Computes e raised to the power of x *)
PROCEDURE Exp*(x: REAL): REAL;
VAR h: SET64; n: SIGNED64; p, y, yy: REAL;
BEGIN 
    y := c21*x;  (*c21 = 1/ln(2)*)
    n := ENTIER(y + 0.5); y := y-n;
    IF y >= 0 THEN INC(n) END ;
    IF n < -1023 THEN RETURN 0
    ELSIF n > 1024 THEN RETURN Inf
    ELSE yy := y*y;
        p := ((p22*yy + p21)*yy + p20)*y;
        p := p/((yy + q21)*yy + q20 - p) + 0.5;
        IF y < 0 THEN p := 2*p END ;
        x := p; SYSTEM.GET(SYSTEM.ADR(x)+4, h); SYSTEM.PUT(SYSTEM.ADR(x)+4, SYSTEM.VAL(SIGNED64, h) + SYSTEM.LSH(n, 20));
        RETURN x
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

(** Computes the arc sine of the value `REAL` x *)
PROCEDURE ArcSin* (x: REAL): REAL;
BEGIN RETURN ArcTan(x / Sqrt(1 - x * x))
END ArcSin;

(** Computes the arc cosine of the value `REAL` x *)
PROCEDURE ArcCos* (x: LONGREAL): LONGREAL;
BEGIN RETURN PI/2 - ArcSin(x)
END ArcCos;

PROCEDURE FormatFix(VAR str : ARRAY OF CHAR; value : REAL; prec: INTEGER);
VAR
    i, digits, round, val : INTEGER;
BEGIN
    IF (prec <= 0) OR (prec > 17) THEN prec := 17 END;
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
        IF i < 15 THEN
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
    IF (prec <= 0) OR (prec > 17) THEN prec := 17 END;
    IF value < 0 THEN
        value := ABS(value);
    END;
    (* force the number between 0 and 10 *)
    IF value >= 10.0 THEN
        pow2 := 256;
        FOR i := MaxPowerArray - 1 TO 0 BY -1 DO
            IF value > Powers[i] THEN
                value := value / Powers[i];
                INC(exp, pow2)
            END;
            pow2 := pow2 DIV 2
        END;
    ELSIF value < 1.0 THEN
        pow2 := 256;
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
    IF exp >= 100 THEN
        ArrayOfChar.AppendChar(str, CHR(ORD('0') + exp DIV 100))
    END;
    exp := exp MOD 100;
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
    str[0] := 00X;
    neg := value < 0;
    class := FPClassify(value);
    IF class = FPInfinite THEN
        str := "INF";
        len := 3
    ELSIF class = FPNaN THEN
        str := "NAN";
        len := 3
    ELSIF class = FPZero THEN
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
        IF e < -307 THEN RETURN 0
        ELSIF 308 < e THEN RETURN INF END;
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
END FromString;

BEGIN
    ASSERT(SIZE(REAL) = 8);
    RoundValue[0] := 0.5; RoundValue[1] := 5.0E-2 + 1.0E-16;
    RoundValue[2] := 5.0E-3 + 1.0E-16; RoundValue[3] := 5.0E-4 + 1.0E-16;
    RoundValue[4] := 5.0E-5 + 1.0E-16; RoundValue[5] := 5.0E-6 + 1.0E-16;
    RoundValue[6] := 5.0E-7 + 1.0E-16; RoundValue[7] := 5.0E-8 + 1.0E-16;
    RoundValue[8] := 5.0E-9 + 1.0E-16; RoundValue[9] := 5.0E-10 + 1.0E-16;
    RoundValue[10] := 5.0E-11 + 1.0E-16; RoundValue[11] := 5.0E-12 + 1.0E-16;
    RoundValue[12] := 5.0E-13 + 1.0E-16; RoundValue[13] := 5.0E-14 + 1.0E-16;
    RoundValue[14] := 5.0E-15 + 1.0E-16; RoundValue[15] := 5.0E-16;
    RoundValue[16] := 5.0E-17;
    Powers[0] := 1.0E1; Powers[1] := 1.0E2; Powers[2] := 1.0E4;
    Powers[3] := 1.0E8; Powers[4] := 1.0E16; Powers[5] := 1.0E32;
    Powers[6] := 1.0E64; Powers[7] := 1.0E128; Powers[8] := 1.0E256;
    c11 := SYSTEM.VAL(REAL, 03FE6A09E667F3BCDH); p10 := SYSTEM.VAL(REAL, 03FE2E29B8F2FCF79H);
	c21 := SYSTEM.VAL(REAL, 03FF71547652B82FEH); p22 := SYSTEM.VAL(REAL, 03F97A609AA5CD460H);
	p21 := SYSTEM.VAL(REAL, 0403433A29C957776H); p20 := SYSTEM.VAL(REAL, 04097A774E9C773D2H);
	q21 := SYSTEM.VAL(REAL, 0406D25B413B5FFD9H); q20 := SYSTEM.VAL(REAL, 040B11016B314DFB1H);
	c42 := SYSTEM.VAL(REAL, 03FE62E42FEFA39EFH); p42 := SYSTEM.VAL(REAL, 0C03253EF500DFE4AH);
	p41 := SYSTEM.VAL(REAL, 040575DB08C526AEEH); p40 := SYSTEM.VAL(REAL, 0C0568B2E25EFD645H);
	q42 := SYSTEM.VAL(REAL, 0C034BBC5DCDB3E86H); q41 := SYSTEM.VAL(REAL, 0404EE16A98F7C5C0H);
	q40 := SYSTEM.VAL(REAL, 0C0468B2E25EFD64BH); c31 := SYSTEM.VAL(REAL, 03FE45F306DC9C883H);
	p36 := SYSTEM.VAL(REAL, 03D9E3EED386DCB82H); p35 := SYSTEM.VAL(REAL, 0BE1E3006399B0141H);
	p34 := SYSTEM.VAL(REAL, 03E950782FCA31EF8H); p33 := SYSTEM.VAL(REAL, 0BF032D2CCE2D52FAH);
	p32 := SYSTEM.VAL(REAL, 03F6466BC677586FFH); p31 := SYSTEM.VAL(REAL, 0BFB4ABBCE625BE41H);
	p30 := SYSTEM.VAL(REAL, 03FE921FB54442D18H); q36 := SYSTEM.VAL(REAL, 03DDF3C92814ECDA3H);
	q35 := SYSTEM.VAL(REAL, 0BE5A6C98987E2CEFH); q34 := SYSTEM.VAL(REAL, 03ECE1F4FAE0F3ECCH);
	q33 := SYSTEM.VAL(REAL, 0BF355D3C7DB78384H); q32 := SYSTEM.VAL(REAL, 03F903C1F081AFFA4H);
	q31 := SYSTEM.VAL(REAL, 0BFD3BD3CC9BE4580H); q30 := SYSTEM.VAL(REAL, 03FEFFFFFFFFFFFFFH);
	c51 := SYSTEM.VAL(REAL, 0400DDB3D742C2655H); s51 := SYSTEM.VAL(REAL, 03FF921FB54442D18H);
	c52 := SYSTEM.VAL(REAL, 03FFBB67AE8584CAAH); s52 := SYSTEM.VAL(REAL, 03FF0C152382D7366H);
	c53 := SYSTEM.VAL(REAL, 03FD126145E9ECD56H); s53 := SYSTEM.VAL(REAL, 03FE0C152382D7366H);
	p53 := SYSTEM.VAL(REAL, 03FCA5162FC998D80H); p52 := SYSTEM.VAL(REAL, 04013B19B23C5E3D6H);
	p51 := SYSTEM.VAL(REAL, 040303E28A72936F6H); p50 := SYSTEM.VAL(REAL, 04029A55D3717FEF3H);
	q52 := SYSTEM.VAL(REAL, 04022655229F407DCH); q51 := SYSTEM.VAL(REAL, 040348462DB028A62H);
	q50 := SYSTEM.VAL(REAL, 04029A55D3717FEF3H);
    Inf := INF;
    NaN := REAL(07FF8000000000000H);
END Real.