MODULE test;

IMPORT SYSTEM;
IN Std IMPORT OSStream, Real;

PROCEDURE Epsilon;
VAR
    fh : OSStream.Std;
    eps: REAL;
    fmt, j, k: INTEGER;
BEGIN
    TRACE(fh.Open(OSStream.STDOUT));
    fh.WriteString ('SIZE(REAL) = ');
    fh.FormatInteger(SIZE(REAL), 0, {});
    fh.WriteNL;
    eps := 1.0;
    k := 0;
    WHILE  ((1.0 + 0.5 * eps) > 1.0) DO
	    DEC(k);
	    eps := 0.5 * eps
    END;
    fh.WriteString ('eps = ');
    fh.FormatReal(eps, 17, 0, {});
    fh.WriteNL;
    FOR j := 0 TO 17 DO
        fh.WriteString ('prec = ');
        fh.FormatInteger(j, 2, {});
        fh.WriteString ('  eps = ');
        fh.FormatReal(eps, j, 0, {});
        fh.WriteNL;
    END;
    fh.Close;
END Epsilon;

BEGIN
    Epsilon;
    (* Run; *)
END test.
