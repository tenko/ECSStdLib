(**
Dynamic `STRING` type.
Strings are always `NUL` terminated and possible
resized to accomodate content.

For further operations on `STRING` type check :ref:`ArrayOfChar`.
*)
MODULE String IN Std;

IN Std IMPORT Char, Config, Type, ArrayOfChar, Integer, Cardinal, Real, DateTime;

TYPE
    STRING* = Type.STRING;
    (* StringWriter *)
    StringWriter = RECORD(Type.Stream)
        str* : STRING;
        pos* : LENGTH;
    END;

(** Reserve capacity.
If Copy is `TRUE` then the existing content is copied when resizing. *)
PROCEDURE Reserve* (VAR dst :STRING; capacity : LENGTH; Copy : BOOLEAN);
VAR tmp : STRING;
BEGIN
    IF capacity < 1 THEN capacity := 1 END;
    IF dst = NIL THEN
        NEW(dst, capacity + 1);
        dst^[0] := Char.NUL;
    ELSIF LEN(dst^) < capacity + 1 THEN
        NEW(tmp, capacity + 1);
        IF Copy THEN ArrayOfChar.Assign(tmp^, dst^)
        ELSE tmp^[capacity] := Char.NUL END;
        DISPOSE(dst);
        dst := tmp;
    END;
END Reserve;

(** Assign `src` to `dst`. *)
PROCEDURE Assign* (VAR dst :STRING; src- : ARRAY OF CHAR);
VAR len : LENGTH;
BEGIN
    len := ArrayOfChar.Length(src);
    Reserve(dst, len, FALSE);
    ArrayOfChar.Assign(dst^, src);
    dst^[len] := Char.NUL;
END Assign;

(** Append `ch` to `dst`. *)
PROCEDURE AppendChar* (VAR dst : STRING; ch : CHAR);
VAR n: LENGTH;
BEGIN
    IF dst = NIL THEN n := 0
    ELSE n := ArrayOfChar.Length(dst^)
    END;
    Reserve(dst, n + 1, TRUE);
    dst^[n + 0] := ch;
    dst^[n + 1] := Char.NUL;
END AppendChar ;

(** Append NewLine to `dst`. *)
PROCEDURE Ln*(VAR dst: STRING);
BEGIN
    AppendChar(dst, Config.NL[0]);
    IF Config.NL[1] # 00X THEN
        AppendChar(dst, Config.NL[1]);
    END
END Ln;

(** Append `src` to `dst`. *)
PROCEDURE Append* (VAR dst : STRING; src- : ARRAY OF CHAR);
VAR i, n, m: LENGTH ;
BEGIN
    IF dst = NIL THEN n := 0
    ELSE n := ArrayOfChar.Length(dst^)
    END;
    m := ArrayOfChar.Length(src);
    Reserve(dst, n + m, TRUE);
    FOR i := 0 TO m - 1 DO
        dst^[n + i] := src[i];
    END;
    dst^[n + m] := Char.NUL;
END Append ;

(** Extract substring from `src` starting at `start` and `count` length. *)
PROCEDURE Extract* (VAR dst : STRING; src- : ARRAY OF CHAR; start, count: LENGTH);
BEGIN
    Reserve(dst, count, FALSE);
    ArrayOfChar.Extract(dst^, src, start, count);
END Extract;

(**
Compare strings `left` and `right`.

* 0 if left = right
* -1 if left < right
* +1 if left > right
*)
PROCEDURE Compare* (left-, right- : STRING): INTEGER;
BEGIN RETURN ArrayOfChar.Compare(left^, right^);
END Compare;

(** Test if `left` and `right` is equal. *)
PROCEDURE Equal* (left-, right- : STRING): BOOLEAN;
BEGIN RETURN ArrayOfChar.Compare(left^, right^) = 0
END Equal;

(** Insert `src` into `dst` at `start` *)
PROCEDURE Insert* (VAR dst : STRING; src- : ARRAY OF CHAR; start: LENGTH);
VAR i, n, m: LENGTH ;
BEGIN
    IF dst = NIL THEN n := 0
    ELSE n := ArrayOfChar.Length(dst^)
    END;
    m := ArrayOfChar.Length(src);
    IF start < 0 THEN start := 0 END;
    IF ((start > n) OR (m = 0 )) THEN
        RETURN;
    END;
    Reserve(dst, n + m, TRUE);
    i := n;
    WHILE i > start DO
        dst^[i + m - 1] := dst^[i - 1];
        DEC(i);
    END;
    i := 0;
    WHILE (i < m) DO
        dst^[start + i] := src[i];
        INC(i);
    END;
    dst^[n + m] := Char.NUL;
END Insert;

(** Replace `old` string with `new` string starting at index `start` (default to 0) *)
PROCEDURE Replace* (VAR dst: STRING; old-, new-: ARRAY OF CHAR; start : LENGTH);
VAR i, ll : LENGTH;
BEGIN
    IF dst = NIL THEN RETURN END;
    ll := ArrayOfChar.Length(old);
    IF ll = 0 THEN RETURN END;
    i := ArrayOfChar.Index(old, dst^, start);
    IF i # -1 THEN
        ArrayOfChar.Delete(dst^, i, ll);
        Insert(dst, new, i);
    END;
END Replace;

(** Assign to a new string and return *)
PROCEDURE New* (str-: ARRAY OF CHAR): STRING;
VAR ret : STRING;
BEGIN
    Assign(ret, str);
    RETURN ret;
END New;

(** Assign to existing string and return reference *)
PROCEDURE S* (VAR dst: STRING; str-: ARRAY OF CHAR): STRING;
BEGIN
    Assign(dst, str);
    RETURN dst;
END S;

(** Duplicate string *)
PROCEDURE Duplicate* (VAR dst: STRING; src-: STRING);
BEGIN
    IF src # NIL THEN Assign(dst, src^)
    ELSE dst := NIL END;
END Duplicate;

(** Dispose string *)
PROCEDURE Dispose* (VAR str: STRING);
BEGIN IF str # NIL THEN DISPOSE(str); str := NIL; END;
END Dispose;

(* WriteChar method for StringWriter *)
PROCEDURE (VAR s : StringWriter) WriteChar(ch : CHAR);
BEGIN
    AppendChar(s.str, ch);
    INC(s.pos, 1);
END WriteChar;

(**
Format `ARRAY OF CHAR`. 

* `width` : Total field with. Can overflow if string is bigger.
* `prec` : The number of characters in string to add (if prec > 0)

The alignment formatting flags are `Left`, `Right` & `Center` .
The `Upper` flag will make the whole string upper case.
The `Alt` flag will capitalize the string.
*)
PROCEDURE FormatString*(VAR dst: STRING; str- : ARRAY OF CHAR; width, prec: INTEGER; flags: SET);
VAR writer : StringWriter;
BEGIN
    writer.str := dst;
    IF dst # NIL THEN writer.pos := ArrayOfChar.Length(dst^)
    ELSE writer.pos := 0;
    END;
    ArrayOfChar.Format(writer, str, width, prec, flags);
    dst := writer.str;
END FormatString;

(**
Format `HUGEINT`.

* `width` : Total field with. Can overflow if number is bigger.

The alignment formatting flags are `Left`, `Right` & `Center` .
The `Zero` flag fills with 0 of the formatting is right aligned.
The `Spc` flag fills in a blank character for `+` if the number is positive.
The `Sign` flag fills in a `+` character if the number is positive.
If both `Spc` and `Sign` are given then `Sign` precedes.
*)
PROCEDURE FormatInteger*(VAR dst: STRING; value : HUGEINT; width: LENGTH; flags: SET);
VAR writer : StringWriter;
BEGIN
    writer.str := dst;
    IF dst # NIL THEN writer.pos := ArrayOfChar.Length(dst^)
    ELSE writer.pos := 0;
    END;
    Integer.Format(writer, value, width, flags);
    dst := writer.str;
END FormatInteger;

(**
Format `REAL`.

* `prec` : Precision or zero for default value.
* `width` : Total field with. Can overflow if number is bigger.
* `flags` : `Exp` or `Fix` formatting supported.

The alignment formatting flags are `Left`, `Right` & `Center` .
The `Spc` flag fills in a blank character for `+` if the number is positive.
The `Sign` flag fills in a `+` character if the number is positive.
If both `Spc` and `Sign` are given then `Sign` precedes.
*)
PROCEDURE FormatReal*(VAR dst: STRING; value : REAL; prec: INTEGER; width: LENGTH; flags: SET);
VAR writer : StringWriter;
BEGIN
    writer.str := dst;
    IF dst # NIL THEN writer.pos := ArrayOfChar.Length(dst^)
    ELSE writer.pos := 0;
    END;
    Real.Format(writer, value, prec, width, flags);
    dst := writer.str;
END FormatReal;

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
PROCEDURE FormatCardinal*(VAR dst: STRING; value : HUGECARD; base, width: INTEGER; flags: SET);
VAR writer : StringWriter;
BEGIN
    writer.str := dst;
    IF dst # NIL THEN writer.pos := ArrayOfChar.Length(dst^)
    ELSE writer.pos := 0;
    END;
    Cardinal.Format(writer, value, base, width, flags);
    dst := writer.str;
END FormatCardinal;

(**
Format `DATETIME` according to format string arguments:

* `%a` : Weekday abbreviated name : Mon .. Sun
* `%A` : Weekday full name : Monday .. Sunday
* `%w` : Weekday as number : 0 .. 6
* `%b` : Month abbreviated name : Jan .. Des
* `%B` : Month full name : Januar .. Desember
* `%Y` : Year without century : 00 - 99
* `%y` : Year with century : 0000 - 9999
* `%m` : Month zero-padded : 00 - 12
* `%d` : Day of the month zero-padded : 01 - XX
* `%W` : Week of the year zero-padded : 01 - 53
* `%H` : Hour (24-hour clock) zero-padded : 00 - 23
* `%I` : Hour (12-hour clock) zero-padded : 1 - 12
* `%p` : AM or PM
* `%M` : Minute zero-padded : 00 - 59
* `%S` : Second zero-padded : 00 - 59
* `%f` : Milliseconds zero-padded : 000 - 999
* `%Z` : Timezone : UTC+/-
* `%%` : Literal `%` char

Other characters are copied to output.
*)
PROCEDURE FormatDateTime*(VAR dst: STRING; value : DateTime.DATETIME; fmt- : ARRAY OF CHAR);
VAR writer : StringWriter;
BEGIN
    writer.str := dst;
    IF dst # NIL THEN writer.pos := ArrayOfChar.Length(dst^)
    ELSE writer.pos := 0;
    END;
    DateTime.Format(writer, value, fmt);
    dst := writer.str;
END FormatDateTime;

(**  Hash value of array (64/32bit FNV-1a) *)
PROCEDURE Hash* (src- : STRING): LENGTH;
BEGIN RETURN ArrayOfChar.Hash(src^);
END Hash;

END String.