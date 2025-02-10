(** Module with operation on `HUGECARD` (64bit) *)
MODULE Cardinal IN Std;

IMPORT SYSTEM;
IN Std IMPORT Const, Type, Char, OSHost;

VAR
    randomSeed* : HUGECARD;

(** Next psuedo random number *)
PROCEDURE Random* (): HUGECARD;
VAR z : HUGECARD;
BEGIN
    (* splitmix64 *)
    randomSeed := randomSeed + 09E3779B97F4A7C15H;
    z := randomSeed;
    z := HUGECARD(SET64(z) / SET64(SYSTEM.LSH(z, -30))) * 0BF58476D1CE4E5B9H;
    z := HUGECARD(SET64(z) / SET64(SYSTEM.LSH(z, -27))) * 094D049BB133111EBH;
    RETURN HUGECARD(SET64(z) / SET64(SYSTEM.LSH(z, -31)))
END Random;

(** Next psuedo random number *)
PROCEDURE RandomRange* (max : HUGECARD): HUGECARD;
VAR ret : HUGECARD;
BEGIN
    ret := Random();
    (* discard lower bits if possible *)
    IF max <= MAX(UNSIGNED32) THEN
        ret := SYSTEM.LSH(ret, -32);
    ELSIF max <= MAX(UNSIGNED16) THEN
        ret := SYSTEM.LSH(ret, -48);
    ELSIF max <= MAX(UNSIGNED8) THEN
        ret := SYSTEM.LSH(ret, -56);
    END;
    IF max < MAX(UNSIGNED64) THEN
        ret := ret MOD (max + 1);
    END;
    RETURN ret;  
END RandomRange;

(**
Format `HUGECARD`.

* `base` : Number base.
* `width` : Total field with. Can overflow if number is bigger.

The alignment formatting flags are `Left`, `Right` & `Center` .
The `Zero` flag fills with 0 of the formatting is right aligned.
The `Upper` flag the hex decimal letters are upper case.

The `Alt` flags prefix binary (base 2) numbers with `0b`,
octal numbers (base 8) with `0o` and hex decimal numbers
with either `0x` or `0X` depending on the `Upper` flag.
*)
PROCEDURE Format*(VAR Writer : Type.Stream; value : HUGECARD; base, width: INTEGER; flags: SET);
VAR
    x, bas: HUGECARD;
    i, digits, len, left, right : LENGTH;
    str : ARRAY 70 OF CHAR;
    DIGITS : ARRAY 17 OF CHAR;
BEGIN
    DIGITS := "0123456789abcdef";
    (* Check if base is valid *)
    bas := HUGECARD(base);
    IF (bas < 2) OR (bas > 16) THEN RETURN END;
    (* Number of digits*)
    len := 0; x := value;
    REPEAT INC(len); x := x DIV bas UNTIL x = 0;
    digits := len;
    (* alternative representation *)
    IF ((flags * Const.Alt) # {}) & ((bas = 2) OR (bas = 8) OR (bas = 16)) THEN
        INC(len, 2)
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
    IF (flags * Const.Zero) = {} THEN
        WHILE left > 0 DO Writer.WriteChar(' '); DEC(left) END;
    END;
    IF (flags * Const.Alt) # {} THEN
        IF bas = 2 THEN Writer.WriteChar('0'); Writer.WriteChar('b')
        ELSIF bas = 8 THEN Writer.WriteChar('0'); Writer.WriteChar('o')
        ELSIF bas = 16  THEN
            Writer.WriteChar('0');
            IF (flags * Const.Upper) # {} THEN Writer.WriteChar('X'); 
            ELSE Writer.WriteChar('x');
            END;
        ELSE RETURN END;
    END;
    IF (flags * Const.Zero) # {} THEN
        WHILE left > 0 DO Writer.WriteChar('0'); DEC(left) END
    END;
    i := digits;
    REPEAT
        DEC(i);
        IF (flags * Const.Upper) # {} THEN
            str[i] := Char.Upper(DIGITS[LENGTH(value MOD bas)]);
        ELSE
            str[i] := DIGITS[LENGTH(value MOD bas)];
        END;
        value := value DIV bas;
    UNTIL value = 0;
    i := 0;
    WHILE (i < digits) DO Writer.WriteChar(str[i]); INC(i) END;
    IF (flags * Const.Zero) = {} THEN
        WHILE right > 0 DO Writer.WriteChar(' '); DEC(right) END
    END;
END Format;

(**
Convert string `str` to `HUGECARD` with number `base` and with argument `start` position into `str`.
Return `FALSE` in case of an invalid `base` argument (2, 8, 10 or 16 is valid).
If `length` is > 0 then the number of converted characters is expected to be `length`.
If `length` is < 0 then the whole string is expected to be converted.
If an overflow is detected then `FALSE` is returned.
Return `TRUE` if success.
*)
PROCEDURE FromSubString* (VAR result : HUGECARD; str- : ARRAY OF CHAR; base : INTEGER; start : LENGTH ; length : LENGTH): BOOLEAN;
VAR
    i, j: LENGTH;
    val, bas: HUGECARD;
    c : CHAR;
    ret : BOOLEAN;
BEGIN
    i := 0; j := length;
    IF length <= 0 THEN RETURN FALSE END;
    IF start < 0 THEN RETURN FALSE END;
    IF (base # 2) & (base # 8) & (base # 10) & (base # 16) THEN RETURN FALSE END;
    bas := HUGECARD(base);
    result := 0;
    ret := TRUE;
    LOOP
        IF (i >= j) OR ~ret THEN EXIT END;
        c := Char.Upper(str[i + start]);
        IF Char.IsDigit(c) OR Char.IsAlpha(c) THEN
            IF Char.IsDigit(c) THEN val := ORD(c) - ORD("0");
            ELSE val := 10 + ORD(c) - ORD("A")
            END;
            IF val < bas THEN
                IF result <= (MAX(HUGECARD) - val) DIV bas THEN
                    result := result * bas + val;
                ELSE ret := FALSE END;
            ELSE ret := FALSE END;
        ELSIF c = Char.NUL THEN DEC(i); EXIT;
        ELSE ret := FALSE END;
        INC(i);
    END;
    IF ret THEN ret := i = length END;
    RETURN ret;
END FromSubString;

(**
Convert string `str` to `HUGECARD` with `base`.

Return `TRUE` if success.
*)
PROCEDURE FromString* (VAR result : HUGECARD; str- : ARRAY OF CHAR; base : INTEGER): BOOLEAN;
VAR
    i: LENGTH;
BEGIN
    i := 0;
    WHILE (i < LEN(str)) & (str[i] # 00X) DO INC(i) END;
    RETURN FromSubString(result, str, base, 0, i);
END FromString;

(* Initialize randomSeed with time *)
PROCEDURE Init;
VAR dt : OSHost.DateTime;
BEGIN
    OSHost.GetTime(dt);
    randomSeed := dt.msec + dt.sec * 1000 + dt.min * 60 * 1000 + dt.hour * 60 * 60 * 1000;
END Init;

BEGIN
    Init;
END Cardinal.