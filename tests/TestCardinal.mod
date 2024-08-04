MODULE TestCardinal;
IN Std IMPORT Testing := O2Testing, Cardinal;

TYPE
    TEST = Testing.TEST;

CONST
    M = "TestCardinal";

PROCEDURE Run* (VAR test: TEST);
VAR
    value : HUGECARD;

    PROCEDURE Assert(b: BOOLEAN; id: LONGINT) ;
    BEGIN
        Testing.Assert(test, b, M, id);
    END Assert ;

BEGIN
    Testing.Begin(test, M);

    (* FromString *)
    Assert(Cardinal.FromString(value, "1", 10) = TRUE, __LINE__);
    Assert(value = 1, __LINE__);
    Assert(Cardinal.FromString(value, "1", 9) = FALSE, __LINE__);
    Assert(Cardinal.FromString(value, "", 10) = FALSE, __LINE__);
    Assert(Cardinal.FromString(value, "-", 10) = FALSE, __LINE__);
    Assert(Cardinal.FromString(value, "abc", 10) = FALSE, __LINE__);
    Assert(Cardinal.FromString(value, "abc", 16) = TRUE, __LINE__);
    Assert(Cardinal.FromString(value, "ABC", 16) = TRUE, __LINE__);
    Assert(value = 0ABCH, __LINE__);
    Assert(Cardinal.FromString(value, "9", 10) = TRUE, __LINE__);
    Assert(value = 9, __LINE__);
    Assert(Cardinal.FromString(value, "A", 10) = FALSE, __LINE__);
    Assert(Cardinal.FromString(value, "A", 16) = TRUE, __LINE__);
    Assert(value = 0AH, __LINE__);
    Assert(Cardinal.FromString(value, "F", 10) = FALSE, __LINE__);
    Assert(Cardinal.FromString(value, "F", 16) = TRUE, __LINE__);
    Assert(value = 0FH, __LINE__);
    Assert(Cardinal.FromString(value, "1FFFFFFFFFFFFFFFF", 16) = FALSE, __LINE__);
    Assert(Cardinal.FromString(value, "0FFFFFFFFFFFFFFFF", 16) = TRUE, __LINE__);
    Assert(value = 0FFFFFFFFFFFFFFFFH, __LINE__);

    (* FromSubString *)
    Assert(Cardinal.FromSubString(value, " 123", 10, 1, 3) = TRUE, __LINE__);
    Assert(value = 123, __LINE__);
    Assert(Cardinal.FromSubString(value, " 123 ", 10, 1, 3) = TRUE, __LINE__);
    Assert(value = 123, __LINE__);
    Assert(Cardinal.FromSubString(value, "91238", 10, 1, 3) = TRUE, __LINE__);
    Assert(value = 123, __LINE__);

    Testing.End(test);
END Run;

END TestCardinal.