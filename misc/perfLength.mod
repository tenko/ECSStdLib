(* Test performance of string lenth optimization in ArrayOfChar *)
MODULE prefLength;

IMPORT ArrayOfChar IN Std, O2Timing IN Std, Out IN OBL;

CONST
    LOOPS = 1000000;
    INNERLOOPS = 10;

VAR
  Str : ARRAY 1024 OF CHAR;
  i, j : LENGTH;

PROCEDURE LengthSimple (str-: ARRAY OF CHAR) : LENGTH;
VAR
    i: LENGTH;
BEGIN
    i := 0;
    WHILE (i < LEN(str)) & (str[i] # 00X) DO INC(i) END;
    RETURN i
END LengthSimple;

PROCEDURE Test1;
VAR
  res : LENGTH;
BEGIN
    res := LengthSimple(Str);
END Test1;

PROCEDURE Test2;
VAR
  res : LENGTH;
BEGIN
    res := ArrayOfChar.Length(Str);
END Test2;

BEGIN
    FOR i:= 0 TO LEN(Str) - 1 DO
        Str[i] := CHR(ORD('a') + i MOD 20);
    END;
    j := LEN(Str);
    FOR i := 0 TO 6 DO
        Str[j - 1] := 00X;
        Out.String("ArrayOfChar.Length(Str) = "); Out.Integer(ArrayOfChar.Length(Str)); Out.Ln;
        O2Timing.StartTimer();
        O2Timing.Timing("Length(Str)", Test1, LOOPS, INNERLOOPS);
        O2Timing.Timing("ArrayOfChar.Length(Str)", Test2, LOOPS, INNERLOOPS);
        Out.Ln;
        j := j DIV 2;
    END;
END prefLength.