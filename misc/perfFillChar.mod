(* Test performance of string lenth optimization in ArrayOfChar *)
MODULE Test;

IMPORT ArrayOfChar IN Std, O2Timing IN Std, Out IN OBL;

CONST
    LOOPS = 1000000;
    INNERLOOPS = 10;

VAR
  Str1 : ARRAY 256 OF CHAR;
  Str2 : ARRAY 128 OF CHAR;
  Str3 : ARRAY 64 OF CHAR;
  Str4 : ARRAY 32 OF CHAR;
  Str5 : ARRAY 16 OF CHAR;

PROCEDURE FillChar(VAR dst : ARRAY OF CHAR; ch : CHAR);
VAR
    i: LENGTH;
BEGIN
    i := 0;
    WHILE (i < LEN(dst)) DO
        dst[i] := ch;
        INC(i)
    END
END FillChar;

PROCEDURE Test1;
BEGIN
    FillChar(Str1, 'A')
END Test1;

PROCEDURE Test11;
BEGIN
    ArrayOfChar.FillChar(Str1, 'A')
END Test11;

PROCEDURE Test2;
BEGIN
    FillChar(Str2, 'A')
END Test2;

PROCEDURE Test12;
BEGIN
    ArrayOfChar.FillChar(Str2, 'A')
END Test12;

PROCEDURE Test3;
BEGIN
    FillChar(Str3, 'A')
END Test3;

PROCEDURE Test13;
BEGIN
    ArrayOfChar.FillChar(Str3, 'A')
END Test13;

PROCEDURE Test4;
BEGIN
    FillChar(Str4, 'A')
END Test4;

PROCEDURE Test14;
BEGIN
    ArrayOfChar.FillChar(Str4, 'A')
END Test14;

PROCEDURE Test5;
BEGIN
    FillChar(Str5, 'A')
END Test5;

PROCEDURE Test15;
BEGIN
    ArrayOfChar.FillChar(Str5, 'A')
END Test15;

BEGIN
    Out.String("LEN(Str1) = "); Out.Integer(LEN(Str1)); Out.Ln;
    O2Timing.StartTimer();
    O2Timing.Timing("FillChar(Str1, 'A')", Test1, LOOPS, INNERLOOPS);
    O2Timing.Timing("ArrayOfChar.FillChar(Str1, 'A')", Test11, LOOPS, INNERLOOPS);
    Out.Ln;
    Out.String("LEN(Str2) = "); Out.Integer(LEN(Str2)); Out.Ln;
    O2Timing.StartTimer();
    O2Timing.Timing("FillChar(Str2, 'A')", Test2, LOOPS, INNERLOOPS);
    O2Timing.Timing("ArrayOfChar.FillChar(Str2, 'A')", Test12, LOOPS, INNERLOOPS);
    Out.Ln;
    Out.String("LEN(Str3) = "); Out.Integer(LEN(Str3)); Out.Ln;
    O2Timing.StartTimer();
    O2Timing.Timing("FillChar(Str3, 'A')", Test3, LOOPS, INNERLOOPS);
    O2Timing.Timing("ArrayOfChar.FillChar(Str3, 'A')", Test13, LOOPS, INNERLOOPS);
    Out.Ln;
    Out.String("LEN(Str4) = "); Out.Integer(LEN(Str4)); Out.Ln;
    O2Timing.StartTimer();
    O2Timing.Timing("FillChar(Str4, 'A')", Test4, LOOPS, INNERLOOPS);
    O2Timing.Timing("ArrayOfChar.FillChar(Str4, 'A')", Test14, LOOPS, INNERLOOPS);
    Out.Ln;
    Out.String("LEN(Str5) = "); Out.Integer(LEN(Str5)); Out.Ln;
    O2Timing.StartTimer();
    O2Timing.Timing("FillChar(Str5, 'A')", Test5, LOOPS, INNERLOOPS);
    O2Timing.Timing("ArrayOfChar.FillChar(Str5, 'A')", Test15, LOOPS, INNERLOOPS);
    Out.Ln;
END Test.