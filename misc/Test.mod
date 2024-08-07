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

PROCEDURE MachineEpsilon() : REAL;
VAR m, ret : REAL;
BEGIN
    m := 1.0; ret := 0.0;
    WHILE 1.0 + m # 1.0 DO
        ret := m;
        m := m / 2.0
    END;
    RETURN ret
END MachineEpsilon;

PROCEDURE Assert(b: BOOLEAN; id: INTEGER) ;
BEGIN IF ~b THEN TRACE("fail"); TRACE(id) END;
END Assert;

(* Check `x` against `exp` either within absolute tolerance `tol` *)
PROCEDURE AssertReal(x, exp, tol : REAL; id: INTEGER) ;
VAR fail : BOOLEAN;
BEGIN
    fail := TRUE;
    IF Real.IsNan(x) & Real.IsNan(exp) THEN
        fail := FALSE
    ELSIF Real.IsNan(x) OR Real.IsNan(exp) THEN
        (* FAIL *)
    ELSIF Real.IsInf(x) & Real.IsInf(exp) THEN
        fail := Real.SignBit(x) # Real.SignBit(exp)
    ELSIF Real.IsInf(x) OR Real.IsInf(exp) THEN
        (* FAIL *)
    ELSE
        IF Real.Abs(exp - x) <= tol THEN
           fail := FALSE
        END
    END;
    ASSERT(fail = FALSE)
END AssertReal;

PROCEDURE Run;
VAR   
    value, eps : REAL;
    ret : BOOLEAN;
BEGIN
    eps := MachineEpsilon();

    (* Constants *)
    Assert(Real.E = 2.71828182845904523536, 1);
    Assert(Real.PI = 3.14159265358979323846, 2);
    Assert(Real.PIDIV2 = 1.57079632679489661923, 3);
    Assert(Real.PIDIV4 = 0.785398163397448309616, 4);
    Assert(Real.SQRT2 = 1.41421356237309504880, 5);
    AssertReal(eps, Real.EPS, 1.0E-17, 6);

    (* FPClassify *)
    Assert(Real.FPClassify(Real.NaN) = Real.FPNaN, 7);
    Assert(Real.FPClassify(Real.Inf) = Real.FPInfinite, 8);
    Assert(Real.FPClassify(0.0) = Real.FPZero, 9);
    Assert(Real.FPClassify(SYSTEM.VAL(REAL, 1000000H)) = Real.FPSubnormal, 10); (* 8.289046E-317 *)
    Assert(Real.FPClassify(1.0) = Real.FPNormal, 11);

    (* IsNan *)
    Assert(Real.IsNan(Real.NaN) = TRUE, 12);
    Assert(Real.IsNan(Real.Inf*0.0) = TRUE, 13); (* This should pass *)
    Assert(Real.IsNan(0.0) = FALSE, 14);
    Assert(Real.IsNan(Real.Inf) = FALSE, 15);

    (* IsInf *)
    Assert(Real.IsInf(Real.NaN) = FALSE, 16);
    Assert(Real.IsInf(0.0) = FALSE, 17);
    Assert(Real.IsInf(Real.Inf) = TRUE, 18);
    Assert(Real.IsInf(Real.Inf + 7.0) = TRUE, 19);
    Assert(Real.IsInf(-1.5 * Real.Inf) = TRUE, 20);


END Run;

BEGIN
    (* Epsilon; *)
    Run;
END test.
