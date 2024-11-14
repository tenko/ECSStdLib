(** Operations on `ARRAY OF CHAR`. All operations sets the `NUL` termination if possible *)
MODULE ArrayOfChar IN Std;

IMPORT SYSTEM;
IN Std IMPORT Char, Const, Type;

TYPE
    WORD = SYSTEM.ADDRESS;
    WSET = SYSTEM.SET;
    
CONST
    WORDSIZE = SIZE(WORD);

(** Set length of string to 0 *)
PROCEDURE Clear* (VAR str : ARRAY OF CHAR);
BEGIN str[0] := 00X
END Clear;

(** Ensure string is `NUL` terminated *)
PROCEDURE NulTerminate* (VAR str : ARRAY OF CHAR);
BEGIN str[LEN(str) - 1] := 00X
END NulTerminate;

(** Return capacity of the array *)
PROCEDURE Capacity* (str- : ARRAY OF CHAR): LENGTH;
BEGIN RETURN LEN(str)
END Capacity;

(** Find length of C style `NUL` terminated string or length of array if not `NUL` terminated *)
PROCEDURE Length* (str-: ARRAY OF CHAR) : LENGTH;
VAR i: LENGTH;
BEGIN
    i := 0;
    WHILE (i < LEN(str)) & (str[i] # 00X) DO INC(i) END;
    RETURN i
END Length;

(** Assign `src` to `dst` *)
PROCEDURE Assign* (VAR dst : ARRAY OF CHAR; src- : ARRAY OF CHAR);
VAR i, max: LENGTH;
BEGIN
    i := 0;
    max := LEN(dst);
    IF LEN(src) < max THEN max := LEN(src) END;
    WHILE (i < max) & (src[i] # 00X) DO
        dst[i] := src[i];
        INC(i)
    END;
    IF i < LEN(dst) THEN dst[i] := 00X END
END Assign;

(** Fill string `dst` with char `chr` *)
PROCEDURE FillChar* (VAR dst : ARRAY OF CHAR; ch : CHAR);
VAR i: LENGTH;
BEGIN
    i := 0;
    WHILE (i < LEN(dst)) DO
        dst[i] := ch;
        INC(i)
    END
END FillChar;

(** Append `ch` to `dst` *)
PROCEDURE AppendChar* (VAR dst : ARRAY OF CHAR; ch : CHAR);
VAR n: LENGTH;
BEGIN
    n := Length(dst);
    IF LEN(dst) > n THEN dst[n] := ch END;
    IF LEN(dst) > n + 1 THEN dst[n + 1] := 00X END
END AppendChar;

(** Append `src` to `dst` *)
PROCEDURE Append* (VAR dst : ARRAY OF CHAR; src- : ARRAY OF CHAR);
VAR
    i, n: LENGTH;
    ch : CHAR;
BEGIN
    n := Length(dst);
    i := 0;
    ch := src[i];
    WHILE (ch # 00X) & (i + n < LEN(dst)) DO
        dst[i + n] := ch;
        INC(i);
        IF i < LEN(src) THEN ch := src[i] ELSE ch := 00X END
    END;
    IF i + n < LEN(dst) THEN dst[i + n] := 00X END
END Append;

(** Extract substring to `dst` from `src` from `start` position and `count` length. *)
PROCEDURE Extract* (VAR dst : ARRAY OF CHAR; src- : ARRAY OF CHAR; start, count: LENGTH);
VAR i: LENGTH;
BEGIN
    i := 0;
    WHILE (i < LEN(dst)) & (i < count ) & (start < LEN(src)) & (src[start] # 00X) DO
        dst[i] := src[start];
        INC(i);
        INC(start)
    END;
    IF i < LEN(dst) THEN dst[i] := 00X END
END Extract;

(**
Compare strings `left` and `right`.

* 0 if left = right
* -1 if left < right
* +1 if left > right
*)
PROCEDURE Compare* (left-, right- : ARRAY OF CHAR): INTEGER;
VAR
    i, high: LENGTH;
    cl, cr : CHAR;
BEGIN
    high := LEN(left);
    IF LEN(right) < high THEN high := LEN(right) END;
    i := 0;
    LOOP
        IF i < LEN(left) THEN cl := left[i] ELSE cl := 00X END;
        IF i < LEN(right) THEN cr := right[i] ELSE cr := 00X END;
        IF (cl # cr) OR (cl = 00X) THEN EXIT END;
        INC(i)
    END;
    IF cl > cr THEN RETURN 1
    ELSIF cl < cr THEN RETURN -1 END;
    RETURN 0
END Compare;

(** Test if `left` and `right` is equal. *)
PROCEDURE Equal* (left-, right- : ARRAY OF CHAR): BOOLEAN;
BEGIN RETURN Compare(left, right) = 0
END Equal;

(** Index of `char` in `str`. One based index with zero indicating `char` not found *)
PROCEDURE IndexChar* (ch : CHAR; str- : ARRAY OF CHAR; start: LENGTH): LENGTH;
VAR i: LENGTH;
BEGIN
    i := start;
    WHILE (i < LEN(str)) & (str[i] # 00X) DO
        IF str[i] = ch THEN RETURN i END;
        INC(i);
    END;
    RETURN -1;
END IndexChar;

(**
Index of `pattern` in `str`. -1 indicating pattern not found.
*)
PROCEDURE Index* (pattern-, str-: ARRAY OF CHAR; start : LENGTH): LENGTH;
VAR i, j, lp: LENGTH;
BEGIN
    lp := Length(pattern);
    IF lp = 0 THEN RETURN 0 END;
    i := start;
    IF lp <= LEN(str) - start THEN
        WHILE (i <= (LEN(str) - lp)) & (str[i] # Char.NUL) DO
            j := 0;
            WHILE (j < lp) & (pattern[j] = str[i + j]) DO
                INC(j);
                IF j = lp THEN RETURN i END;
            END;
            INC(i);
        END;
    END;
    RETURN -1;
END Index;

(** Delete `count` characters from `dst` starting from `start`. *)
PROCEDURE Delete* (VAR dst: ARRAY OF CHAR; start, count: LENGTH);
VAR
    len : LENGTH;
BEGIN
    len := Length(dst);
    WHILE start + count < len DO
        dst[start] := dst[start + count];
        INC(start)
    END;
    IF start < LEN(dst) THEN dst[start] := 00X END
END Delete;

(** Insert `src` into `dst` at `start`. *)
PROCEDURE Insert* (VAR dst : ARRAY OF CHAR; src- : ARRAY OF CHAR; start: LENGTH);
VAR
    i, n, m: LENGTH;
BEGIN
    n := Length(dst);
    m := Length(src);
    IF start < 0 THEN start := 0 END;
    IF ((start > n) OR (m = 0 )) OR (n + m > LEN(dst)) THEN RETURN END;
    i := n;
    WHILE i > start DO
        dst[i + m - 1] := dst[i - 1];
        DEC(i);
    END;
    i := 0;
    WHILE (i < m) DO
        dst[start + i] := src[i];
        INC(i);
    END;
    IF n + m < LEN(dst) THEN dst[n + m] := 00X END;
END Insert;

(** Replace `old` string with `new` string starting at index `start`. *)
PROCEDURE Replace* (VAR dst: ARRAY OF CHAR; old-, new-: ARRAY OF CHAR; start : LENGTH);
VAR
    i, ll : LENGTH;
BEGIN
    ll := Length(old);
    i := Index(old, dst, start);
    IF i # -1 THEN
        Delete(dst, i, ll);
        Insert(dst, new, i);
    END;
END Replace;

(** Remove white space & control characters from left side of string. *)
PROCEDURE LeftTrim* (VAR dst: ARRAY OF CHAR);
VAR
    i, cnt, len : LENGTH;
BEGIN
    i := 0;
    len := Length(dst);
    WHILE (i < len) & Char.IsControl(dst[i]) DO INC(i) END;
    IF i > 0 THEN
        cnt := i;
        i := 0;
        WHILE i + cnt < len DO
            dst[i] := dst[i + cnt];
            INC(i)
        END;
        IF i <= LEN(dst) - 1 THEN dst[i] := 00X END;
    END;
END LeftTrim;

(* Remove white space & special characters from right side of string. *)
PROCEDURE RightTrim* (VAR dst: ARRAY OF CHAR);
VAR
    i, len : LENGTH;
BEGIN
    len := Length(dst);
    i := len;
    WHILE (i > 0) & Char.IsControl(dst[i - 1]) DO DEC(i) END;
    IF i < len THEN dst[i] := 00X END;
END RightTrim;

(** Left justified of length `width` with `ch`. *)
PROCEDURE LeftPad* (VAR dst: ARRAY OF CHAR; width : LENGTH; ch : CHAR);
VAR
    str: ARRAY 1 OF CHAR;
    cnt: LENGTH;
BEGIN
    cnt := width - Length(dst);
    str[0] := ch;
    WHILE cnt > 0 DO Insert(dst, str, 0); DEC(cnt) END;
END LeftPad;

(** Right justified of length `width` with `ch`. *)
PROCEDURE RightPad* (VAR dst: ARRAY OF CHAR; width : LENGTH; ch : CHAR);
VAR
    str: ARRAY 1 OF CHAR;
    cnt: LENGTH;
BEGIN
    cnt := width - Length(dst);
    str[0] := ch;
    WHILE cnt > 0 DO Append(dst, str); DEC(cnt) END;
END RightPad;

(** Remove white space & special characters from right & left side of string. *)
PROCEDURE Trim* (VAR dst: ARRAY OF CHAR);
BEGIN RightTrim(dst); LeftTrim(dst);
END Trim;

(** Transform string inplace to lower case (Only takes into account the ASCII characters). *)
PROCEDURE LowerCase* (VAR dst: ARRAY OF CHAR);
VAR
    i : LENGTH;
BEGIN
    i := 0;
    WHILE (i < LEN(dst)) & (dst[i] # 00X) DO dst[i] := Char.Lower(dst[i]); INC(i) END;
END LowerCase;

(** Transform string inplace to upper case (Only takes into account the ASCII characters).*)
PROCEDURE UpperCase* (VAR dst: ARRAY OF CHAR);
VAR
    i : LENGTH;
BEGIN
    i := 0;
    WHILE (i < LEN(dst)) & (dst[i] # 00X) DO dst[i] := Char.Upper(dst[i]); INC(i) END;
END UpperCase;

(** Capitalize string inplace. (Only takes into account the ASCII characters). *)
PROCEDURE Capitalize* (VAR dst: ARRAY OF CHAR);
VAR
    i : LENGTH;
    ch : CHAR;
BEGIN
    i := 0;
    ch := dst[i];
    WHILE (i < LEN(dst)) & (ch # Char.NUL) DO
        IF ~Char.IsControl(ch) THEN
            dst[i] := Char.Upper(ch);
            ch := Char.NUL
        ELSE
            INC(i);
            ch := dst[i]
        END
    END
END Capitalize;

(** Check if string `str` starts with `prefix`. *)
PROCEDURE StartsWith* (str-, prefix- : ARRAY OF CHAR): BOOLEAN;
VAR
    i, ll : LENGTH;
BEGIN
    ll := Length(prefix);
    i := Length(str) - ll;
    RETURN (i >= 0 ) & ((ll = 0) OR (Index(prefix, str, 0) = 0));
END StartsWith;

(** Check if string `str` ends with `postfix`. *)
PROCEDURE EndsWith* (str-, postfix- : ARRAY OF CHAR): BOOLEAN;
VAR
    i, ll : LENGTH;
BEGIN
    ll := Length(postfix);
    i := Length(str) - ll;
    RETURN (i >= 0 ) & ((ll = 0) OR (Index(postfix, str, i) = i))
END EndsWith;

(*
Return `TRUE` if `patter` matches `str`.

* `?` mathches a single character
*  `*` mathches any sequence of characters including zero length
*)
PROCEDURE Match* (str- : ARRAY OF CHAR; pattern- : ARRAY OF CHAR; IgnoreCase : BOOLEAN): BOOLEAN;
    VAR
        lens, lenp : LENGTH;
    
    PROCEDURE IMatch(is, ip: LENGTH): BOOLEAN;
    BEGIN
        WHILE ip < lenp DO
            IF pattern[ip] = '*' THEN
                WHILE is <= lens DO (* check to end of string *)
                    IF IMatch(is, ip + 1) THEN RETURN TRUE END;
                    INC(is);
                END;
                RETURN FALSE;
            ELSIF is = lens THEN (* pattern not exhausted *)
                RETURN FALSE;
            ELSIF pattern[ip] = '?' THEN
                ;
            ELSIF ~IgnoreCase & (str[is] # pattern[ip]) THEN
                RETURN FALSE;
            ELSIF IgnoreCase & (Char.Upper(str[is]) # Char.Upper(pattern[ip])) THEN
                RETURN FALSE;
            END;
            INC(is); INC(ip);
        END;
        RETURN is = lens;
    END IMatch;
BEGIN
    lens := Length(str);
    lenp := Length(pattern);
    IF lenp = 0 THEN RETURN lens = 0 END;
    RETURN IMatch(0, 0); 
END Match;

(**
Format `ARRAY OF CHAR`. 

* `width` : Total field with. Can overflow if string is bigger.
* `prec` : The number of characters in string to add (if prec > 0)
* `flags` : The formatting flags defaults to `Left` alignment.

The `Upper` flag will make the whole string upper case.
The `Alt` flag will capitalize the string.
*)
PROCEDURE Format*(VAR Writer : Type.Stream; str- : ARRAY OF CHAR; width, prec: INTEGER; flags: SET);
VAR i, strlen, left, right: LENGTH;
BEGIN
    strlen := Length(str);
    IF (prec > 0) & (prec < strlen) THEN strlen := prec END;
    left := 0; right := 0;
    IF width > strlen THEN
        IF (flags * Const.Right) # {} THEN
            left := width - strlen
        ELSIF (flags * Const.Center) # {} THEN
            left := (width - strlen) DIV 2;
            right := (width - strlen) - left;
        ELSE (* Default to Left *)
            right := width - strlen
        END
    END;
    WHILE left > 0 DO Writer.WriteChar(' '); DEC(left) END;
    i := 0;
    IF (flags * Const.Alt) # {} THEN
        LOOP
            IF i >= strlen THEN EXIT END;
            IF ~Char.IsControl(str[i]) THEN
                Writer.WriteChar(Char.Upper(str[i]));
                INC(i);
                EXIT;
            ELSE
                Writer.WriteChar(str[i]);
            END;
            INC(i)
        END;
        WHILE (i < strlen) DO Writer.WriteChar(str[i]); INC(i) END;
    ELSIF (flags * Const.Upper) # {} THEN
        WHILE (i < strlen) DO Writer.WriteChar(Char.Upper(str[i])); INC(i) END;
    ELSE
        WHILE (i < strlen) DO Writer.WriteChar(str[i]); INC(i) END;
    END;
    WHILE right > 0 DO Writer.WriteChar(' '); DEC(right) END
END Format;

(**  Hash value of string (64/32bit FNV-1a) *)
PROCEDURE Hash* (src- : ARRAY OF CHAR): LENGTH;
CONST
    HSTART = SEL(WORDSIZE = 4, 0811C9DC5H, 0CBF29CE484222325H);
    HFACTOR = SEL(WORDSIZE = 4, 01000193H, 0100000001B3H);
VAR
    i : LENGTH;
    hash : LENGTH;
BEGIN
    hash := LENGTH(HSTART);
    i := 0;
    WHILE (i < LEN(src)) & (src[i] # 00X) DO
        hash := LENGTH(WSET(src[i]) / WSET(hash)); (* XOR *)
        hash := hash * LENGTH(HFACTOR);
        INC(i)
    END;
    RETURN hash
END Hash;

END ArrayOfChar.