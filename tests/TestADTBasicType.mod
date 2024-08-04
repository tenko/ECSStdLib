MODULE TestADTBasicType;
IMPORT Testing := O2Testing IN Std, SYSTEM;
IMPORT Int := ADTBasicType(INTEGER) IN Std;

TYPE
    TEST = Testing.TEST;

CONST
    M = "TestADTBasicType";

PROCEDURE Run* (VAR test: TEST);
VAR
    a, b : INTEGER;

    PROCEDURE Assert(b: BOOLEAN; id: LONGINT) ;
    BEGIN
        Testing.Assert(test, b, M, id);
    END Assert ;

BEGIN
    Testing.Begin(test, M);

    (* Min *)
    Assert(Int.Min(-1,+1) = -1, __LINE__) ;
    Assert(Int.Min(+1,-1) = -1, __LINE__) ;
    (* Max *)
    Assert(Int.Max(-1,+1) = +1, __LINE__) ;
    Assert(Int.Max(+1,-1) = +1, __LINE__) ;
    (* Swap *)
    a := -1; b := 1;
    Int.Swap(a, b);
    Assert((a = 1) & (b = -1), __LINE__) ;
    (* Compare *)
    Assert(Int.Compare(-1,+1) = -1, __LINE__) ;
    Assert(Int.Compare(+1,-1) = +1, __LINE__) ;
    Assert(Int.Compare(+1,+1) = 0, __LINE__) ;
    (* Equal *)
    Assert(Int.Equal(-1,+1) = FALSE, __LINE__) ;
    Assert(Int.Equal(+1,+1) = TRUE, __LINE__) ;
    (* Hash *)
    IF SIZE(SYSTEM.ADDRESS) = 8 THEN
        Assert(Int.Hash(1) = 6238072747940578789, __LINE__) ;
    ELSE
        Assert(Int.Hash(1) = LENGTH(1364076727), __LINE__) ;
    END;

    Testing.End(test);
END Run;

END TestADTBasicType.