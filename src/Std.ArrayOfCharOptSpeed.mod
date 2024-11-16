(** Operations on `ARRAY OF CHAR`. All operations sets the `NUL` termination if possible *)
MODULE ArrayOfChar IN Std;

IMPORT SYSTEM;
IN Std IMPORT Char, Const, Type;

TYPE
    WORD = SYSTEM.ADDRESS;
    WSET = SYSTEM.SET;

CONST
    WORDSIZE = SIZE(WORD);
    NULLMASK1 = SEL(WORDSIZE = 2, 0101H, SEL(WORDSIZE = 4, 01010101H, 0101010101010101H));
    NULLMASK2 = SEL(WORDSIZE = 2, 8080H, SEL(WORDSIZE = 4, 80808080H, 8080808080808080H));
    LOWER = SEL(WORDSIZE = 2, 101H, SEL(WORDSIZE = 4, 1010101H, 101010101010101H));
    UPPER = SEL(WORDSIZE = 2, 1010H, SEL(WORDSIZE = 4, 10101010H, 1010101010101010H));

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
VAR
    word: WORD;
    adr : SYSTEM.ADDRESS;
    i: LENGTH;
BEGIN
    i := 0;
    (* Check block by block *)
    adr := SYSTEM.ADR(str[0]);
    LOOP
        IF i > (LEN(str) - WORDSIZE) THEN EXIT END;
        SYSTEM.GET(adr + i, word);
        (*  Ref.: https://graphics.stanford.edu/~seander/bithacks.html#ZeroInWord *)
        IF (WSET(word - NULLMASK1) * (-WSET(word)) * WSET(NULLMASK2)) # {} THEN EXIT END;
        INC(i, WORDSIZE)
    END;
    (* Find position in last block *)
    WHILE (i < LEN(str)) & (str[i] # 00X) DO INC(i) END;
    RETURN i
END Length;

(** Assign `src` to `dst` *)
PROCEDURE Assign* (VAR dst : ARRAY OF CHAR; src- : ARRAY OF CHAR);
VAR
    word: WORD;
    sadr, dadr : SYSTEM.ADDRESS;
    i, max: LENGTH;
BEGIN
    i := 0;
    max := LEN(dst);
    IF LEN(src) < max THEN max := LEN(src) END;
    (* Check block by block *)
    sadr := SYSTEM.ADR(src[0]);
    dadr := SYSTEM.ADR(dst[0]);
    LOOP
        IF i > (max - WORDSIZE) THEN EXIT END;
        SYSTEM.GET(sadr + i, word);
        SYSTEM.PUT(dadr + i, word);
        (*  Ref.: https://graphics.stanford.edu/~seander/bithacks.html#ZeroInWord *)
        IF (WSET(word - NULLMASK1) * (-WSET(word)) * WSET(NULLMASK2)) # {} THEN EXIT END;
        INC(i, WORDSIZE)
    END;
    (* Process last partial block *)
    WHILE (i < max) & (src[i] # 00X) DO
        dst[i] := src[i];
        INC(i)
    END;
    IF i < LEN(dst) THEN dst[i] := 00X END
END Assign;

(** Fill string `dst` with char `chr` *)
PROCEDURE FillChar* (VAR dst : ARRAY OF CHAR; ch : CHAR);
VAR
    word: WORD;
    adr : SYSTEM.ADDRESS;
    i: LENGTH;
BEGIN
    (* Process block by block *)
    i := 0;
    adr := SYSTEM.ADR(dst[0]);
    word := LOWER * WORD(WSET(ch) * WSET(0FH))  + UPPER * WORD(WSET(UNSIGNED8(ch) DIV 16) * WSET(0FH));
    LOOP
        IF (i > (LEN(dst) - WORDSIZE)) THEN EXIT END;
        SYSTEM.PUT(adr + i, word);
        INC(i, WORDSIZE)
    END;
    (* Process last partial block *)
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
    lword, rword: WORD;
    ladr, radr : SYSTEM.ADDRESS;
    cl, cr : CHAR;
BEGIN
    high := LEN(left);
    IF LEN(right) < high THEN high := LEN(right) END;
    i := 0;
    (* Check block by block for difference *)
    ladr := SYSTEM.ADR(left[0]);
    radr := SYSTEM.ADR(right[0]);
    LOOP
        IF i > (high - WORDSIZE) THEN EXIT END;
        SYSTEM.GET(ladr + i, lword);
        SYSTEM.GET(radr + i, rword); 
        IF lword # rword THEN EXIT END;
        (*  Ref.: https://graphics.stanford.edu/~seander/bithacks.html#ZeroInWord *)
        IF (WSET(lword - NULLMASK1) * (-WSET(lword)) * WSET(NULLMASK2)) # {} THEN EXIT END;
        INC(i, WORDSIZE)
    END;
    (* Check last block *)
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

(** Index of `char` in `str`. zero based index with -1 indicating `char` not found *)
PROCEDURE IndexChar* (ch : CHAR; str- : ARRAY OF CHAR; start: LENGTH): LENGTH;
VAR
    i: LENGTH;
    word, mask: WORD;
    adr: SYSTEM.ADDRESS;
BEGIN
    i := start;
    (* Check block by block *)
    adr := SYSTEM.ADR(str[0]);
    mask := LOWER * WORD(WSET(ch) * WSET(0FH))  + UPPER * WORD(WSET(UNSIGNED8(ch) DIV 16) * WSET(0FH));
    LOOP
        IF i > (LEN(str) - WORDSIZE) THEN EXIT END;
        SYSTEM.GET(adr + i, word);
        IF (WSET(word - NULLMASK1) * (-WSET(word)) * WSET(NULLMASK2)) # {} THEN EXIT END;
        word := WORD(WSET(word) / WSET(mask)); (* XOR *)
        IF (WSET(word - NULLMASK1) * (-WSET(word)) * WSET(NULLMASK2)) # {} THEN EXIT END;
        INC(i, WORDSIZE);
    END;
    (* Find position in last block *)
    WHILE (i < LEN(str)) & (str[i] # 00X) DO
        IF str[i] = ch THEN RETURN i END;
        INC(i);
    END;
    RETURN -1;
END IndexChar;

(**
Index of `pattern` in str. -1 indicating `pattern` not found.

This is the TwoWay algorithm to avoid quadratic behaviour
 * http://monge.univ-mlv.fr/~mac/Articles-PDF/CP-1991-jacm.pdf
 * https://www-igm.univ-mlv.fr/~lecroq/string/node26.html
*)
PROCEDURE Index* (pattern-, str-: ARRAY OF CHAR; start : LENGTH): LENGTH;
VAR
    i, j, m, n: LENGTH;
    ell, memory, p, per, q: LENGTH;

    (* Computing of the maximal suffix for <= *)
    PROCEDURE MaxSuffixGEQ(VAR period : LENGTH; VAR max : LENGTH);
    VAR
        j, k : LENGTH;
        a, b : CHAR;
    BEGIN
        max := -1; j := 0;
        k := 1; period := 1;
        WHILE j + k < m DO
            a := pattern[j + k];
            b := pattern[max + k];
            IF a < b THEN
                j := j + k; k := 1;
                period := j - max;
            ELSE
                IF a = b THEN
                    IF k # period THEN INC(k)
                    ELSE j := j + period; k := 1 END;
                ELSE
                    max := j; j := max + 1;
                    k := 1; period := 1;
                END;
            END;
        END;
    END MaxSuffixGEQ;

    (* Computing of the maximal suffix for >= *)
    PROCEDURE MaxSuffixLEQ(VAR period : LENGTH; VAR max : LENGTH);
    VAR
        j, k : LENGTH;
        a, b : CHAR;
    BEGIN
        max := -1; j := 0;
        k := 1; period := 1;
        WHILE j + k < m DO
            a := pattern[j + k];
            b := pattern[max + k];
            IF a > b THEN
                j := j + k; k := 1;
                period := j - max;
            ELSE
                IF a = b THEN
                    IF k # period THEN INC(k)
                    ELSE j := j + period; k := 1 END;
                ELSE
                    max := j; j := max + 1;
                    k := 1; period := 1;
                END;
            END;
        END;
    END MaxSuffixLEQ;

    PROCEDURE Cmp(j, len : LENGTH): BOOLEAN;
    VAR i : LENGTH;
    BEGIN
        i := 0;
        WHILE (pattern[i] = pattern[i + j]) & (len > 0) DO
            INC(i); DEC(len)
        END;
        RETURN len = 1
    END Cmp;

BEGIN
    m := Length(pattern);
    IF m = 0 THEN RETURN 0 END;
    n := Length(str);
    IF (n = 0) OR (start >= n) THEN RETURN -1 END;
    IF m > n - start THEN RETURN -1 END;
    IF m = 1 THEN RETURN IndexChar(pattern[0], str, start) END;
    (* TODO : Special case with pattern length 2, 3 & 4 *)
    MaxSuffixGEQ(p, i);
    MaxSuffixLEQ(q, j);
    IF i > j THEN
        ell := i; per := p
    ELSE
        ell := j; per := q
    END;
    (* Searching *)
    IF Cmp(per, ell + 1) THEN
        j := 0; memory := -1;
        WHILE (j <= n - m) DO
            IF ell > memory THEN i := ell + 1
            ELSE i := memory + 1 END;
            WHILE (i < m) & (pattern[i] = str[start + i + j]) DO INC(i) END;
            IF i >= m THEN
                i := ell;
                WHILE (i > memory) & (pattern[i] = str[start + i + j]) DO DEC(i) END;
                IF i <= memory THEN RETURN j + start END;
                j := j + per;
                memory := m - per - 1;
            ELSE
                j := j + (i - ell);
                memory := -1;
            END
        END
    ELSE
        IF ell + 1 > m - ell - 1 THEN per := ell + 2
        ELSE per := m - ell END;
        j := 0;
        WHILE j <= n - m DO
            i := ell + 1;
            WHILE (i < m) & (pattern[i] = str[start + i + j]) DO INC(i) END;
            IF i >= m THEN
                i := ell;
                WHILE (i >= 0) & (pattern[i] = str[start + i + j]) DO DEC(i) END;
                IF i < 0 THEN RETURN j + start END;
                j := j + per
            ELSE
                j := j + (i - ell)
            END;
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

The alignment formatting flags are `Left`, `Right` & `Center` .
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
    word: WORD;
    adr : SYSTEM.ADDRESS;
    i : LENGTH;
    j : INTEGER;
    hash : LENGTH;
BEGIN
    hash := LENGTH(HSTART);
    i := 0;
    adr := SYSTEM.ADR(src[0]);
    (* Process block by block *)
    LOOP
        IF (i > (LEN(src) - WORDSIZE)) THEN EXIT END;
        SYSTEM.GET(adr + i, word);
        (*  Ref.: https://graphics.stanford.edu/~seander/bithacks.html#ZeroInWord *)
        IF (WSET(word - NULLMASK1) * (-WSET(word)) * WSET(NULLMASK2)) # {} THEN EXIT END;
        FOR j := 0 TO WORDSIZE - 1 DO
            hash := LENGTH((WSET(word) * WSET(0FFH)) / WSET(hash)); (* XOR *)
            hash := hash * HFACTOR;
            word := SYSTEM.LSH(word, -8);
        END;
        INC(i, WORDSIZE)
    END;
    (* Process last partial block *)
    WHILE (i < LEN(src)) & (src[i] # 00X) DO
        hash := LENGTH(WSET(src[i]) / WSET(hash)); (* XOR *)
        hash := hash * LENGTH(HFACTOR);
        INC(i)
    END;
    RETURN hash
END Hash;

END ArrayOfChar.