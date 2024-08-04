MODULE TestADTSet;
IMPORT Testing := O2Testing IN Std;

IMPORT Int := ADTBasicType(INTEGER) IN Std;
IMPORT ADTSet(INTEGER, Int.Hash, Int.Equal) IN Std;

TYPE
    TEST = Testing.TEST;

CONST
    M = "TestADTSet";

PROCEDURE Run* (VAR test: TEST);
VAR
    set : ADTSet.Set;
    it : ADTSet.Iterator;
    elements : ADTSet.ElementVector;
    i : INTEGER;

    PROCEDURE Assert(b: BOOLEAN; id: LONGINT) ;
    BEGIN
        Testing.Assert(test, b, M, id);
    END Assert ;
BEGIN
    Testing.Begin(test, M);

    set.Init(2);

    set.Incl(1);
    set.Incl(-1);
    set.Incl(100);
    set.Incl(100);
    set.Incl(9999);
    
    Assert(~set.In(2), __LINE__);
    Assert(set.In(1), __LINE__);
    Assert(set.In(-1), __LINE__);
    Assert(set.In(100), __LINE__);
    Assert(set.In(9999), __LINE__);

    set.Excl(100);
    Assert(set.In(1), __LINE__);
    Assert(set.In(-1), __LINE__);
    Assert(~set.In(100), __LINE__);
    Assert(set.In(9999), __LINE__);

    set.Incl(100);

    set.First(it);
    WHILE it.Next(i) DO
        IF (i # 9999) & (i # 100) & (i # 1) & (i # -1) THEN
            Assert(FALSE, __LINE__);
        END;
    END;

    elements := set.Elements();
    elements.Sort(Int.Compare);
    Assert(elements.Pop(i), __LINE__);
    Assert(i = 9999, __LINE__);
    Assert(elements.Pop(i), __LINE__);
    Assert(i = 100, __LINE__);
    Assert(elements.Pop(i), __LINE__);
    Assert(i = 1, __LINE__);
    Assert(elements.Pop(i), __LINE__);
    Assert(i = -1, __LINE__);
    Assert(~elements.Pop(i), __LINE__);
    elements.Dispose();
    set.Dispose();

    Testing.End(test);
END Run;

END TestADTSet.