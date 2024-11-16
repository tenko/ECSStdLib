(** Module with operation on `HUGEINT` (64bit) *)
MODULE Integer IN Std;

IN Std IMPORT Const, Type, Char;

(**
Format `HUGEINT`.

* `width` : Total field with. Can overflow if number is bigger.

The alignment formatting flags are `Left`, `Right` & `Center` .
The `Zero` flag fills with 0 of the formatting is right aligned.
The `Spc` flag fills in a blank character for `+` if the number is positive.
The `Sign` flag fills in a `+` character if the number is positive.
If both `Spc` and `Sign` are given then `Sign` precedes.
*)
PROCEDURE Format*(VAR Writer : Type.Stream; value : HUGEINT; width: LENGTH; flags: SET);
VAR
    val, x : HUGECARD;
    i, len, digits, left, right : LENGTH;
    str : ARRAY 20 OF CHAR;
BEGIN
    IF value = MIN(HUGEINT) THEN (* -MIN(HUGEINT) does not exists *)
        val := 9223372036854775808
    ELSE
        val := ABS(value);
    END;
    (* Number of digits*)
    len := 0; x := val;
    REPEAT INC(len); x := x DIV 10 UNTIL x = 0;
    digits := len;
    IF value < 0 THEN INC(len);
    ELSIF (flags * Const.Sign) # {} THEN INC(len)
    ELSIF (flags * Const.Spc) # {} THEN INC(len)
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
    (* Sign *)
    IF value < 0 THEN Writer.WriteChar('-');
    ELSIF (flags * Const.Sign) # {} THEN Writer.WriteChar('+')
    ELSIF (flags * Const.Spc) # {} THEN Writer.WriteChar(' ')
    END;
    IF (flags * Const.Zero) # {} THEN
        WHILE left > 0 DO Writer.WriteChar('0'); DEC(left) END
    END;
    i := digits;
    REPEAT
        DEC(i);
        str[i] := CHR(ORD('0') + val MOD 10);
        val := val DIV 10;
    UNTIL val = 0;
    i := 0;
    WHILE (i < digits) DO Writer.WriteChar(str[i]); INC(i) END;
    IF (flags * Const.Zero) = {} THEN
        WHILE right > 0 DO Writer.WriteChar(' '); DEC(right) END
    END;
END Format;

(**
Convert string `str` to `HUGEINT` and with argument `start` position into `str`.
If `length` is > 0 then the number of converted characters is expected to be `length`.
If `length` is < 0 then the whole string is expected to be converted.
If an overflow is detected `FALSE` is returned.
Return `TRUE` if success.
*)
PROCEDURE FromSubString* (VAR result : HUGEINT; str- : ARRAY OF CHAR; start : LENGTH ; length : LENGTH): BOOLEAN;
VAR
    res, max : HUGECARD;
    i, j, val: LENGTH;
    c, sign : CHAR;
    ret : BOOLEAN;
BEGIN
    i := 0; j := length;
    IF length <= 0 THEN RETURN FALSE END;
    IF start < 0 THEN RETURN FALSE END;
    res := 0;
    max := HUGECARD(MAX(HUGEINT)) + 1;
    sign := 00X;
    ret := TRUE;
    LOOP
        IF (i >= j) OR ~ret THEN EXIT END;
        c := str[i + start];
        IF Char.IsDigit(c) THEN
            val := ORD(c) - ORD("0");
            IF res <= (max - HUGECARD(val)) DIV 10 THEN
                res := res * 10 + HUGECARD(val);
            ELSE
                ret := FALSE;
            END;
        ELSIF (i = 0) & ((c = '+') OR (c = '-')) THEN
            sign := c;
        ELSIF c = Char.NUL THEN DEC(i); EXIT;
        ELSE ret := FALSE END;
        INC(i);
    END;
    IF sign = '-' THEN
        IF res = max THEN result := MIN(HUGEINT)
        ELSE result := -HUGEINT(res) END;
    ELSE
        IF res < max THEN result := HUGEINT(res)
        ELSE ret := FALSE END;
    END;
    IF ret THEN
        ret := (i > LENGTH(sign # 00X)) & (i = length);
    END;
    RETURN ret;
END FromSubString;

(**
Convert string `str` to `HUGEINT`.

Return `TRUE` if success.
*)
PROCEDURE FromString* (VAR result : HUGEINT; str- : ARRAY OF CHAR): BOOLEAN;
VAR
    i: LENGTH;
BEGIN
    i := 0;
    WHILE (i < LEN(str)) & (str[i] # 00X) DO INC(i) END;
    RETURN FromSubString(result, str, 0, i);
END FromString;

END Integer.