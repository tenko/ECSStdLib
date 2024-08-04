MODULE TestMod;

IMPORT SYSTEM, ArrayOfChar IN Std, O2Timing IN Std, Out IN OBL;

CONST
    LOOPS = 1000000;
    INNERLOOPS = 10;

VAR
  Str : ARRAY 255 OF CHAR;
  ch : CHAR;
  i, j : LENGTH;

(** Index of `char` in `str`. zero based index with -1 indicating `char` not found *)
PROCEDURE IndexChar(ch : CHAR; str- : ARRAY OF CHAR; start : LENGTH): LENGTH;
VAR
    i: LENGTH;
BEGIN
    i := start;
    WHILE (i < LEN(str)) & (str[i] # 00X) DO
        IF str[i] = ch THEN RETURN i END;
        INC(i);
    END;
    RETURN -1;
END IndexChar;

PROCEDURE Test1;
BEGIN
    IGNORE(IndexChar(ch, Str, 0))
END Test1;

PROCEDURE Test2;
BEGIN
    IGNORE(ArrayOfChar.IndexChar(ch, Str, 0))
END Test2;

BEGIN
    FOR i := 0 TO LEN(Str) - 1 DO
        Str[i] := CHR(i + 1);
    END;
    j := LEN(Str);
    FOR i := 0 TO 4 DO
        Str[j - 1] := 00X;
        ch := Str[j - 2];
        Out.String("ArrayOfChar.Length(Str) = "); Out.Integer(ArrayOfChar.Length(Str)); Out.Ln;
        O2Timing.StartTimer();
        O2Timing.Timing("IndexChar(ch, Str, 0)", Test1, LOOPS, INNERLOOPS);
        O2Timing.Timing("ArrayOfChar.IndexChar(ch, Str, 0)", Test2, LOOPS, INNERLOOPS);
        Out.Ln;
        j := j DIV 2;
    END;
END TestMod.