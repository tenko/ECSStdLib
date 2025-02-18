MODULE TestReal;
IN Std IMPORT Testing := O2Testing, Real;
IMPORT SYSTEM;

TYPE
    TEST = Testing.TEST;

CONST
    M = "TestReal";
    REALSIZE = SIZE(REAL);

PROCEDURE Run* (VAR test: TEST);
VAR
    value, eps, tol : REAL;
    ret : BOOLEAN;

    PROCEDURE Assert(b: BOOLEAN; id: LONGINT) ;
    BEGIN
        Testing.Assert(test, b, M, id);
    END Assert ;

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
        Assert(fail = FALSE, id)
    END AssertReal;

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

BEGIN
    Testing.Begin(test, M);

    eps := MachineEpsilon();
    IF REALSIZE = 8 THEN tol := 1.0E-9
    ELSE tol := 1.0E-6 END;

    (* Constants *)
    Assert(Real.E = 2.71828182845904523536, __LINE__);
    Assert(Real.PI = 3.14159265358979323846, __LINE__);
    Assert(Real.PIDIV2 = 1.57079632679489661923, __LINE__);
    Assert(Real.PIDIV4 = 0.785398163397448309616, __LINE__);
    Assert(Real.SQRT2 = 1.41421356237309504880, __LINE__);
    IF REALSIZE = 8 THEN
        AssertReal(eps, Real.EPS, 1.0E-17, __LINE__);
    ELSE
        AssertReal(eps, Real.EPS, 1.0E-9, __LINE__);
    END;

    (* FPClassify *)
    Assert(Real.FPClassify(Real.NaN) = Real.FPNaN, __LINE__);
    Assert(Real.FPClassify(Real.Inf) = Real.FPInfinite, __LINE__);
    Assert(Real.FPClassify(0.0) = Real.FPZero, __LINE__);
    IF REALSIZE = 8 THEN
        Assert(Real.FPClassify(SYSTEM.VAL(REAL, 1000000H)) = Real.FPSubnormal, __LINE__); (* 8.289046E-317 *)
    ELSE
        Assert(Real.FPClassify(SYSTEM.VAL(REAL, 01H)) = Real.FPSubnormal, __LINE__); (* 1.4012984643 × 10−45 *)
    END;
    Assert(Real.FPClassify(1.0) = Real.FPNormal, __LINE__);

    (* IsNan *)
    Assert(Real.IsNan(Real.NaN) = TRUE, __LINE__);
    Assert(Real.IsNan(0.0) = FALSE, __LINE__);
    Assert(Real.IsNan(Real.Inf) = FALSE, __LINE__);

    (* IsInf *)
    Assert(Real.IsInf(Real.NaN) = FALSE, __LINE__);
    Assert(Real.IsInf(0.0) = FALSE, __LINE__);
    Assert(Real.IsInf(Real.Inf) = TRUE, __LINE__);
    Assert(Real.IsInf(Real.Inf + 7.0) = TRUE, __LINE__);
    Assert(Real.IsInf(-1.5 * Real.Inf) = TRUE, __LINE__);

    (* IsZero *)
    Assert(Real.IsZero(Real.NaN) = FALSE, __LINE__);
    Assert(Real.IsZero(0.0) = TRUE, __LINE__);
    Assert(Real.IsZero(1.0) = FALSE, __LINE__);
    Assert(Real.IsZero(Real.Inf) = FALSE, __LINE__);

    (* IsNormal *)
    Assert(Real.IsNormal(Real.NaN) = FALSE, __LINE__);
    Assert(Real.IsNormal(0.0) = FALSE, __LINE__);
    Assert(Real.IsNormal(1.0) = TRUE, __LINE__);
    Assert(Real.IsNormal(Real.Inf) = FALSE, __LINE__);

    (* Abs *)
    Assert(Real.Abs(-1.0) = 1.0, __LINE__);
    Assert(Real.Abs(-0.0) = 0.0, __LINE__);
    Assert(Real.Abs(0.0) = 0.0, __LINE__);
    Assert(Real.Abs(1.0) = 1.0, __LINE__);
    Assert(Real.Abs(Real.Inf) = Real.Inf, __LINE__);
    Assert(Real.Abs(-Real.Inf) = Real.Inf, __LINE__);
    Assert(Real.Abs(Real.NaN) = Real.NaN, __LINE__);

    (* SignBit *)
    Assert(Real.SignBit(-1.0) = TRUE, __LINE__);
    Assert(Real.SignBit(-0.0) = TRUE, __LINE__);
    Assert(Real.SignBit(0.0) = FALSE, __LINE__);
    Assert(Real.SignBit(1.0) = FALSE, __LINE__);
    Assert(Real.SignBit(Real.Inf) = FALSE, __LINE__);
    Assert(Real.SignBit(-Real.Inf) = TRUE, __LINE__);
    Assert(Real.SignBit(Real.NaN) = FALSE, __LINE__);

    (* CopySign *)
    Assert(Real.CopySign(1.0, 25.0) = 1.0, __LINE__);
    Assert(Real.CopySign(0.0, 25.0) = 0.0, __LINE__);
    Assert(Real.CopySign(1.0, -25.0) = -1.0, __LINE__);
    Assert(Real.CopySign(1.0, -0.0) = -1.0, __LINE__);
    Assert(Real.CopySign(1.0, Real.Inf) = 1.0, __LINE__);
    Assert(Real.CopySign(1.0, -Real.Inf) = -1.0, __LINE__);
    Assert(Real.CopySign(Real.Inf, Real.Inf) = Real.Inf, __LINE__);
    Assert(Real.CopySign(Real.Inf, -Real.Inf) = -Real.Inf, __LINE__);

    (* Sin *)
    AssertReal(Real.Sin(0), 0.0, tol, __LINE__);
    AssertReal(Real.Sin(Real.PI), 0.0, tol, __LINE__);
    AssertReal(Real.Sin(Real.PIDIV2), 1.0, tol, __LINE__);
    AssertReal(Real.Sin(-Real.PIDIV2), -1.0, tol, __LINE__);

    (* Cos *)
    AssertReal(Real.Cos(0), 1.0, tol, __LINE__);
    AssertReal(Real.Cos(Real.PI), -1.0, tol, __LINE__);
    AssertReal(Real.Cos(Real.PIDIV2), 0.0, tol, __LINE__);
    AssertReal(Real.Cos(-Real.PIDIV2), 0.0, tol, __LINE__);

    (* Tan *)
    AssertReal(Real.Tan(0), 0.0, tol, __LINE__);
    AssertReal(Real.Tan(Real.PIDIV4), 1.0, tol, __LINE__);
    AssertReal(Real.Tan(-Real.PIDIV4), -1.0, 1000*tol, __LINE__);

    (* ArcTan *)
    AssertReal(Real.ArcTan(0), 0.0, tol, __LINE__);
    AssertReal(Real.ArcTan(1.0), Real.PIDIV4, tol, __LINE__);
    AssertReal(Real.ArcTan(-1.0), -Real.PIDIV4, tol, __LINE__);
    AssertReal(Real.ArcTan(Real.Inf), 1.0, tol, __LINE__);
    AssertReal(Real.ArcTan(-Real.Inf), -1.0, tol, __LINE__);

    (* ArcTan2 *)
    AssertReal(Real.ArcTan2(1.0, 1.0), Real.PIDIV4, tol, __LINE__);
    AssertReal(Real.ArcTan2(-1.0, 1.0), -Real.PIDIV4, tol, __LINE__);
    AssertReal(Real.ArcTan2(1.0, 0.0), Real.PIDIV2, tol, __LINE__);
    AssertReal(Real.ArcTan2(-1.0, 0.0), -Real.PIDIV2, tol, __LINE__);

    (* Sqrt *)
    AssertReal(Real.Sqrt(0.0), 0.0, tol, __LINE__);
    AssertReal(Real.Sqrt(2.0), Real.SQRT2, tol, __LINE__);
    AssertReal(Real.Sqrt(4.0), 2.0, tol, __LINE__);

    (* Exp *)
    AssertReal(Real.Exp(0.0), 1.0, tol, __LINE__);
    AssertReal(Real.Exp(1.0), Real.E, tol, __LINE__);

    (* Log *)
    AssertReal(Real.Log(1.0), 0.0, tol, __LINE__);
    AssertReal(Real.Log(Real.E), 1.0, tol, __LINE__);

    (* Floor *)
    AssertReal(Real.Floor(0.0), 0.0, tol, __LINE__);
    AssertReal(Real.Floor(0.5), 0.0, tol, __LINE__);
    AssertReal(Real.Floor(1.0), 1.0, tol, __LINE__);
    AssertReal(Real.Floor(1.5), 1.0, tol, __LINE__);
    AssertReal(Real.Floor(-0.5), -1.0, tol, __LINE__);
    AssertReal(Real.Floor(-1.0), -1.0, tol, __LINE__);
    AssertReal(Real.Floor(-1.5), -2.0, tol, __LINE__);

    (* Round *)
    AssertReal(Real.Round(0.0), 0.0, tol, __LINE__);
    AssertReal(Real.Round(2.3), 2.0, tol, __LINE__);
    AssertReal(Real.Round(2.5), 3.0, tol, __LINE__);
    AssertReal(Real.Round(-2.3), -2.0, tol, __LINE__);
    AssertReal(Real.Round(-2.5), -3.0, tol, __LINE__);

    (* FromString *)
    Assert(~Real.FromString(value, "", 0, 0), __LINE__);
    Assert(~Real.FromString(value, "x", 0, 0), __LINE__);
    Assert(~Real.FromString(value, "1..1", 0, 0), __LINE__);
    Assert(~Real.FromString(value, "..", 0, 0), __LINE__);
    ret := Real.FromString(value, "0", 0, 0);
    Assert(ret & (value = 0.), __LINE__);
    ret := Real.FromString(value, "-0", 0, 0);
    Assert(ret & (value = -0.), __LINE__);
    ret := Real.FromString(value, "0.", 0, 0);
    Assert(ret & (value = 0.), __LINE__);
    ret := Real.FromString(value, "123456789", 0, 0);
    AssertReal(value, 123456789., tol, __LINE__);
    ret := Real.FromString(value, "1e0", 0, 0);
    Assert(ret & (value = 1.), __LINE__);
    ret := Real.FromString(value, "1E0", 0, 0);
    Assert(ret & (value = 1.), __LINE__);
    IF REALSIZE = 8 THEN
        ret := Real.FromString(value, "1.7976931348623157e308", 0, 0);
        Assert(ret & (value = 1.7976931348623157E308), __LINE__);
        ret := Real.FromString(value, "5E-324", 0, 0);
        Assert(ret, __LINE__);
        ret := Real.FromString(value, "2.4e-324", 0, 0); (* Underflow *)
        Assert(ret & (value = 0.0), __LINE__);
        ret := Real.FromString(value, "2e308", 0, 0); (* Overflow *)
        Assert(ret & (value = Real.Inf), __LINE__);
    ELSE
        ret := Real.FromString(value, "3.40282E+38", 0, 0);
        Assert(ret, __LINE__);
        AssertReal(value, 3.40282E+38, tol, __LINE__); (* Strange error *)
    END;
    ret := Real.FromString(value, " 1234", 1, 4); (* start & length *)
    Assert(ret & (value = 1234.), __LINE__);

    Testing.End(test);
END Run;

END TestReal.