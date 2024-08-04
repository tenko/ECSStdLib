(* Test performance of string optimization in ArrayOfChar *)
MODULE Test;

IMPORT ArrayOfChar IN Std, O2Timing IN Std, Out IN OBL;

CONST
    LOOPS = 1000000;
    INNERLOOPS = 10;

VAR
  Str1 : ARRAY 255 OF CHAR;
  Str2 : ARRAY 128 OF CHAR;
  Str3 : ARRAY 64 OF CHAR;
  Str4 : ARRAY 32 OF CHAR;
  Str5 : ARRAY 16 OF CHAR;

  PROCEDURE CompareSimple(left-, right- : ARRAY OF CHAR): INTEGER;
  VAR
      i: LENGTH;
      cl, cr : CHAR;
  BEGIN
      i := 0;
      cl := left[i]; cr := right[i];
      WHILE (cl = cr) & (cl # 00X)  DO
          INC(i);
          IF i < LEN(left) THEN cl := left[i] ELSE cl := 00X END;
          IF i < LEN(right) THEN cr := right[i] ELSE cr := 00X END
      END;
      IF cl > cr THEN RETURN 1
      ELSIF cl < cr THEN RETURN -1 END;
      RETURN 0
  END CompareSimple;

PROCEDURE Test1;
BEGIN
    IGNORE(CompareSimple(Str1, Str1))
END Test1;

PROCEDURE Test11;
BEGIN
    IGNORE(ArrayOfChar.Compare(Str1, Str1))
END Test11;

PROCEDURE Test2;
BEGIN
    IGNORE(CompareSimple(Str2, Str2))
END Test2;

PROCEDURE Test12;
BEGIN
    IGNORE(ArrayOfChar.Compare(Str2, Str2))
END Test12;

PROCEDURE Test3;
BEGIN
    IGNORE(CompareSimple(Str3, Str3))
END Test3;

PROCEDURE Test13;
BEGIN
    IGNORE(ArrayOfChar.Compare(Str3, Str3))
END Test13;

PROCEDURE Test4;
BEGIN
    IGNORE(CompareSimple(Str4, Str4))
END Test4;

PROCEDURE Test14;
BEGIN
    IGNORE(ArrayOfChar.Compare(Str4, Str4))
END Test14;

PROCEDURE Test5;
BEGIN
    IGNORE(CompareSimple(Str5, Str5))
END Test5;

PROCEDURE Test15;
BEGIN
    IGNORE(ArrayOfChar.Compare(Str5, Str5))
END Test15;

PROCEDURE Assign(VAR str : ARRAY OF CHAR);
VAR
    i : LENGTH;
BEGIN
    FOR i := 0 TO LEN(str) - 1 DO
        str[i] := CHR(i + 1);
    END
END Assign;

BEGIN
    Assign(Str1);
    Out.String("ArrayOfChar.Length(Str1) = "); Out.Integer(ArrayOfChar.Length(Str1)); Out.Ln;
    O2Timing.StartTimer();
    O2Timing.Timing("CompareSimple(Str1, Str1)", Test1, LOOPS, INNERLOOPS);
    O2Timing.Timing("ArrayOfChar.Compare(Str1, Str1)", Test11, LOOPS, INNERLOOPS);
    Out.Ln;
    Assign(Str2);
    Out.String("ArrayOfChar.Length(Str2) = "); Out.Integer(ArrayOfChar.Length(Str2)); Out.Ln;
    O2Timing.StartTimer();
    O2Timing.Timing("CompareSimple(Str2, Str2)", Test2, LOOPS, INNERLOOPS);
    O2Timing.Timing("ArrayOfChar.Compare(Str2, Str2)", Test12, LOOPS, INNERLOOPS);
    Out.Ln;
    Assign(Str3);
    Out.String("ArrayOfChar.Length(Str3) = "); Out.Integer(ArrayOfChar.Length(Str3)); Out.Ln;
    O2Timing.StartTimer();
    O2Timing.Timing("CompareSimple(Str3, Str3)", Test3, LOOPS, INNERLOOPS);
    O2Timing.Timing("ArrayOfChar.Compare(Str3, Str3)", Test13, LOOPS, INNERLOOPS);
    Out.Ln;
    Assign(Str4);
    Out.String("ArrayOfChar.Length(Str4) = "); Out.Integer(ArrayOfChar.Length(Str4)); Out.Ln;
    O2Timing.StartTimer();
    O2Timing.Timing("CompareSimple(Str4, Str4)", Test4, LOOPS, INNERLOOPS);
    O2Timing.Timing("ArrayOfChar.Compare(Str4, Str4)", Test14, LOOPS, INNERLOOPS);
    Out.Ln;
    Assign(Str5);
    Out.String("ArrayOfChar.Length(Str5) = "); Out.Integer(ArrayOfChar.Length(Str5)); Out.Ln;
    O2Timing.StartTimer();
    O2Timing.Timing("CompareSimple(Str5, Str5)", Test5, LOOPS, INNERLOOPS);
    O2Timing.Timing("ArrayOfChar.Compare(Str5, Str5)", Test15, LOOPS, INNERLOOPS);
    Out.Ln;
END Test.