MODULE TestArrayOfSet;
IN Std IMPORT Testing := O2Testing, Set := ArrayOfSet;

CONST
    M = "TestArrayOfSet";
    N = 256 DIV Set.SETSIZE;

TYPE
    TEST = Testing.TEST;
    CHARSET = ARRAY N OF SET;

PROCEDURE Run* (VAR test: TEST);
VAR   
    digits, alpha, alphanum, s : CHARSET; 
    i : LENGTH;

    PROCEDURE Assert(b: BOOLEAN; id: LONGINT) ;
    BEGIN
        Testing.Assert(test, b, M, id);
    END Assert ;

BEGIN
    Testing.Begin(test, M);

    Set.Clear(s);
    Assert(Set.IsZero(s) = TRUE, __LINE__);

    Set.Clear(digits);
    FOR i := ORD('0') TO ORD('9') DO
        Set.Incl(digits, i)
    END;

    Assert(Set.Equal(s, digits) = FALSE, __LINE__);
    Set.Copy(s, digits);
    Assert(Set.Equal(s, digits) = TRUE, __LINE__);

    Assert(Set.In(digits, ORD('a')) = FALSE, __LINE__);
    Assert(Set.In(digits, ORD('0')) = TRUE, __LINE__);
    Assert(Set.In(digits, ORD('9')) = TRUE, __LINE__);

    Set.Clear(alpha);
    FOR i := ORD('a') TO ORD('z') DO
        Set.Incl(alpha, i)
    END;
    FOR i := ORD('A') TO ORD('Z') DO
        Set.Incl(alpha, i)
    END;

    Set.Union(alphanum, digits, alpha);
    Assert(Set.In(alphanum, ORD('a')) = TRUE, __LINE__);
    Assert(Set.In(alphanum, ORD('z')) = TRUE, __LINE__);
    Assert(Set.In(alphanum, ORD('A')) = TRUE, __LINE__);
    Assert(Set.In(alphanum, ORD('Z')) = TRUE, __LINE__);
    Assert(Set.In(alphanum, ORD('0')) = TRUE, __LINE__);
    Assert(Set.In(alphanum, ORD('9')) = TRUE, __LINE__);
    Assert(Set.IsSubset(digits, alphanum) = TRUE, __LINE__);
    Assert(Set.IsSubset(alpha, alphanum) = TRUE, __LINE__);

    Set.Copy(s, alphanum);
    Set.Invert(s);
    Assert(Set.IsSubset(s, alphanum) = FALSE, __LINE__);

    Set.Union(s, s, alphanum);
    Set.Invert(s);
    Assert(Set.IsZero(s) = TRUE, __LINE__);

    Testing.End(test);
END Run;

END TestArrayOfSet.