MODULE TestDataLZ4;

IMPORT SYSTEM;
IN Std IMPORT Testing := O2Testing, ArrayOfChar, DataLZ4;

TYPE
    TEST = Testing.TEST;

CONST
    TEXT = "Lorem ipsum dolor sit amet, consectetur adipiscing elit, sed do eiusmod tempor incididunt ut labore et dolore magna aliqua. Ut enim ad minim veniam, quis nostrud exercitation ullamco laboris nisi ut aliquip ex ea commodo consequat. Duis aute irure dolor in reprehenderit in voluptate velit esse cillum dolore eu fugiat nulla pariatur. Excepteur sint occaecat cupidatat non proident, sunt in culpa qui officia deserunt mollit anim id est laborum.";
    M = "TestDataLZ4";

VAR
    org, comp, dcomp : ARRAY 512 OF CHAR;

PROCEDURE Run* (VAR test: TEST);
VAR   
    len, res : LENGTH;

    PROCEDURE Assert(b: BOOLEAN; id: LONGINT) ;
    BEGIN
        Testing.Assert(test, b, M, id);
    END Assert ;
BEGIN
    Testing.Begin(test, M);

    len := ArrayOfChar.Length(TEXT);
    ArrayOfChar.Assign(org, TEXT);
    ArrayOfChar.FillChar(comp, 00X);
    ArrayOfChar.FillChar(dcomp, 00X);

    res := DataLZ4.BlockEncodeRaw(SYSTEM.ADR(comp[0]), LEN(comp), SYSTEM.ADR(org[0]), len + 1);
    Assert(res > 0, __LINE__);

    res := DataLZ4.BlockDecodeRaw(SYSTEM.ADR(dcomp[0]), LEN(dcomp), SYSTEM.ADR(comp[0]), res);
    Assert(res > 0, __LINE__);

    Assert(ArrayOfChar.Compare(org, dcomp) = 0, __LINE__);

    Testing.End(test);
END Run;

END TestDataLZ4.