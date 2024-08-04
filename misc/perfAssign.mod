(* Test performance of string lenth optimization in ArrayOfChar *)
MODULE Test;

IMPORT ArrayOfChar IN Std, O2Timing IN Std, Out IN OBL;

CONST
    LOOPS = 1000000;
    INNERLOOPS = 10;

VAR
  Str : ARRAY 255 OF CHAR;
  Dst : ARRAY 255 OF CHAR;
  i, j : LENGTH;

PROCEDURE Assign(VAR dst : ARRAY OF CHAR; src- : ARRAY OF CHAR);
VAR
    i: LENGTH;
BEGIN
    i := 0;
    WHILE (i < LEN(dst)) & (i < LEN(src)) & (src[i] # 00X) DO
        dst[i] := src[i];
        INC(i)
    END;
    IF i < LEN(dst) THEN dst[i] := 00X END
END Assign;

PROCEDURE Test1;
BEGIN
    Assign(Dst, Str)
END Test1;

PROCEDURE Test2;
BEGIN
    ArrayOfChar.Assign(Dst, Str)
END Test2;

BEGIN
    FOR i := 0 TO LEN(Str) - 1 DO
        Str[i] := CHR(i + 1);
    END;
    j := LEN(Str);
    FOR i := 0 TO 4 DO
        Str[j - 1] := 00X;
        Out.String("ArrayOfChar.Length(Str) = "); Out.Integer(ArrayOfChar.Length(Str)); Out.Ln;
        O2Timing.StartTimer();
        O2Timing.Timing("Assign(Dst, Str)", Test1, LOOPS, INNERLOOPS);
        O2Timing.Timing("ArrayOfChar.Assign(Dst, Str)", Test2, LOOPS, INNERLOOPS);
        Out.Ln;
        j := j DIV 2;
    END;
END Test.