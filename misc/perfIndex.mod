(* Test performance of string index optimization in ArrayOfChar *)
MODULE prefIndex;

IMPORT ArrayOfChar IN Std, O2Timing IN Std, Out IN OBL;

CONST
    LOOPS = 1000000;
    INNERLOOPS = 10;
    STR = "aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaab";
VAR
  Pth : ARRAY 64 OF CHAR;

(* Algorithms and Data Structures by N. Wirth 1.9.2 The Knuth-Morris-Pratt String Search *)
PROCEDURE KMPIndexOf(p-, s-: ARRAY OF CHAR): LENGTH;
VAR
    ds : ARRAY 127 OF LENGTH;
    dp: POINTER TO ARRAY OF LENGTH;
    N, M: LENGTH;

    PROCEDURE IKMPIndexOf(d: ARRAY OF LENGTH): LENGTH;
        VAR
            i, j, k: LENGTH;
    BEGIN
        (* compute shifts *)
        d[0] := -1;
        IF p[0] # p[1] THEN d[1] := 0
        ELSE d[1] := -1 END;
        j := 1; k := 0;
        WHILE (j < M - 1) & (p[j] # 00X) DO
            IF (k >= 0) & (p[j] # p[k]) THEN k := d[k];
            ELSE
            INC(j); INC(k);
            IF p[j] # p[k] THEN d[j] := k
            ELSE d[j] := d[k] END;
            END;
        END;
        IF j <= M - 1 THEN M := j END;
        (* scan with shifts *)
        i := 0; j := 0;
        WHILE (j < M) & (i < N) & (s[i] # 00X) DO
            IF (j >= 0) & (s[i] # p[j]) THEN j := d[j];
            ELSE INC(i); INC(j) END;
        END;
        IF j = M THEN RETURN i - M END;
        RETURN -1
    END IKMPIndexOf;
BEGIN
    N := LEN(s);
    M := LEN(p);
    IF M < 128 THEN RETURN IKMPIndexOf(ds)
    ELSE NEW(dp, M); RETURN IKMPIndexOf(dp^) END;
END KMPIndexOf;

PROCEDURE SimpleSearch (pattern-, str-: ARRAY OF CHAR; start : LENGTH): LENGTH;
VAR
    i, j, lp: LENGTH;
BEGIN
    lp := ArrayOfChar.Length(pattern);
    IF lp = 0 THEN RETURN 0 END;
    i := start;
    IF lp <= LEN(str) - start THEN
        WHILE (i <= (LEN(str) - lp)) & (str[i] # 00X) DO
            j := 0;
            WHILE (j < lp) & (pattern[j] = str[i + j]) DO
                INC(j);
                IF j = lp THEN RETURN i END;
            END;
            INC(i);
        END;
    END;
    RETURN -1;
END SimpleSearch;

PROCEDURE Test1;
VAR ret : LENGTH;
VAR
BEGIN
    ret := KMPIndexOf(Pth, STR);
END Test1;

PROCEDURE Test2;
VAR ret : LENGTH;
BEGIN
    ret := ArrayOfChar.Index(Pth, STR, 0);
END Test2;

PROCEDURE Test3;
VAR ret : LENGTH;
BEGIN
    ret := SimpleSearch(Pth, STR, 0);
END Test3;

BEGIN
    (* a simple search win here *)
    Pth := "euismod vehicula";
    Out.String("ArrayOfChar.Length(STR) = "); Out.Integer(ArrayOfChar.Length(STR)); Out.Ln;
    Out.String("ArrayOfChar.Length(Pth) = "); Out.Integer(ArrayOfChar.Length(Pth)); Out.Ln;
    O2Timing.StartTimer();
    O2Timing.Timing("KMPIndexOf", Test1, LOOPS, INNERLOOPS);
    O2Timing.Timing("ArrayOfChar.Index", Test2, LOOPS, INNERLOOPS);
    O2Timing.Timing("SimpleSearch", Test3, LOOPS, INNERLOOPS);
    Out.Ln;

    (* Quadratic behaviour of simple algo. *)
    Pth := "aaaaaaaaaaaaaaaaaaaaaaaab";
    Out.String("ArrayOfChar.Length(Pth) = "); Out.Integer(ArrayOfChar.Length(Pth)); Out.Ln;
    O2Timing.StartTimer();
    O2Timing.Timing("KMPIndexOf", Test1, LOOPS, INNERLOOPS);
    O2Timing.Timing("ArrayOfChar.Index", Test2, LOOPS, INNERLOOPS);
    O2Timing.Timing("SimpleSearch", Test3, LOOPS, INNERLOOPS);
    Out.Ln;

END prefIndex.