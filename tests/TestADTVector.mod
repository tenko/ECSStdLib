MODULE TestADTVector;
IN Std IMPORT Testing := O2Testing, String;
IMPORT ADTVector(INTEGER) IN Std;

TYPE
    TEST = Testing.TEST;

CONST
    M = "TestADTVector";

PROCEDURE Compare(left-, right-: INTEGER): INTEGER;
BEGIN
    IF left < right THEN RETURN -1 END;
    IF left > right THEN RETURN 1 END;
    RETURN 0
END Compare;

PROCEDURE Run* (VAR test: TEST);
VAR
    vint : ADTVector.Vector;
    ival : INTEGER;
    ret : BOOLEAN;

    PROCEDURE Assert(b: BOOLEAN; id: LONGINT) ;
    BEGIN
        Testing.Assert(test, b, M, id);
    END Assert ;
BEGIN
    Testing.Begin(test, M);

    vint.Init(4);
    vint.Append(4); vint.Append(1); vint.Append(8); vint.Append(5); vint.Append(6);
    vint.Append(2); vint.Append(9); vint.Append(0); vint.Append(7); vint.Append(3);
    vint.Sort(Compare);
    ret := vint.Pop(ival); Assert(ret & (ival = 9), __LINE__);
    ret := vint.Pop(ival); Assert(ret & (ival = 8), __LINE__);
    ret := vint.Pop(ival); Assert(ret & (ival = 7), __LINE__);
    ret := vint.Pop(ival); Assert(ret & (ival = 6), __LINE__);
    ret := vint.Pop(ival); Assert(ret & (ival = 5), __LINE__);
    ret := vint.Pop(ival); Assert(ret & (ival = 4), __LINE__);
    ret := vint.Pop(ival); Assert(ret & (ival = 3), __LINE__);
    ret := vint.Pop(ival); Assert(ret & (ival = 2), __LINE__);
    ret := vint.Pop(ival); Assert(ret & (ival = 1), __LINE__);
    ret := vint.Pop(ival); Assert(ret & (ival = 0), __LINE__);
    ret := vint.Pop(ival); Assert(~ret, __LINE__);
    Assert(vint.Size() = 0, __LINE__);

    vint.Append(4); vint.Append(1); vint.Append(8); vint.Append(5); vint.Append(6);
    vint.Append(2); vint.Append(9); vint.Append(0); vint.Append(7); vint.Append(3);
    vint.Sort(Compare);
    Assert(vint.At(0) = 0, __LINE__);
    Assert(vint.At(9) = 9, __LINE__);
    Assert(vint.Find(Compare, 0) = 0, __LINE__);
    Assert(vint.Find(Compare, 9) = 9, __LINE__);
    Assert(vint.Find(Compare, -1) = -1, __LINE__);
    
    vint.Dispose();

    Testing.End(test);
END Run;

END TestADTVector.