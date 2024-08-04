MODULE TestInteger;
IN Std IMPORT Testing := O2Testing, Integer;

TYPE
    TEST = Testing.TEST;

CONST
    M = "TestInteger";

PROCEDURE Run* (VAR test: TEST);
VAR
    value : HUGEINT;

    PROCEDURE Assert(b: BOOLEAN; id: LONGINT) ;
    BEGIN
        Testing.Assert(test, b, M, id);
    END Assert ;

BEGIN
    Testing.Begin(test, M);

    (* FromString *)
    Assert(Integer.FromString(value, "") = FALSE, __LINE__) ;
    Assert(Integer.FromString(value, "-") = FALSE, __LINE__) ;
    Assert(Integer.FromString(value, "+") = FALSE, __LINE__) ;
    Assert(Integer.FromString(value, "abc") = FALSE, __LINE__) ;
    Assert(Integer.FromString(value, "1") = TRUE, __LINE__) ;
    Assert(value = 1, __LINE__) ;
    Assert(Integer.FromString(value, "9") = TRUE, __LINE__) ;
    Assert(value = 9, __LINE__) ;
    Assert(Integer.FromString(value, " 1") = FALSE, __LINE__) ;
    Assert(Integer.FromString(value, "123 ") = FALSE, __LINE__) ;
    Assert(Integer.FromString(value, "123") = TRUE, __LINE__) ;
    Assert(value = 123, __LINE__) ;
    Assert(Integer.FromString(value, "+123") = TRUE, __LINE__) ;
    Assert(value = 123, __LINE__) ;
    Assert(Integer.FromString(value, "-123") = TRUE, __LINE__) ;
    Assert(value = -123, __LINE__) ;
    Assert(Integer.FromString(value, "9223372036854775808") = FALSE, __LINE__) ;
    Assert(Integer.FromString(value, "9223372036854775807") = TRUE, __LINE__) ;
    Assert(value = MAX(HUGEINT), __LINE__) ;
    Assert(Integer.FromString(value, "-9223372036854775809") = FALSE, __LINE__) ;
    Assert(Integer.FromString(value, "-9223372036854775808") = TRUE, __LINE__) ;
    Assert(value = MIN(HUGEINT), __LINE__) ;

    (* FromSubString *)
    Assert(Integer.FromSubString(value, " 123 ", 1, 3) = TRUE, __LINE__) ;
    Assert(value = 123, __LINE__) ;
    Assert(Integer.FromSubString(value, " 12. ", 1, 3) = FALSE, __LINE__) ;

    Testing.End(test);
END Run;

END TestInteger.