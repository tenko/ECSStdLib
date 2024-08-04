MODULE TestADTList;
IN Std IMPORT Testing := O2Testing, String;
IMPORT ADTList(String.STRING) IN Std;

TYPE
    TEST = Testing.TEST;

CONST
    M = "TestADTList";

PROCEDURE Run* (VAR test: TEST);
VAR
    S : String.STRING;
    List : ADTList.List;
    It : ADTList.Iterator;

    PROCEDURE Assert(b: BOOLEAN; id: LONGINT) ;
    BEGIN
        Testing.Assert(test, b, M, id);
    END Assert ;

BEGIN
    Testing.Begin(test, M);

    List.Init();
    Assert(List.IsEmpty() = TRUE, __LINE__) ;
    List.Append(String.New("123"));
    List.Append(String.New("456"));
    List.Append(String.New("789"));
    List.AppendHead(String.New("abc"));
    Assert(List.IsEmpty() = FALSE, __LINE__) ;
    Assert(List.Size() = 4, __LINE__) ;

    (* Pop *)
    IF List.Pop(S) THEN
        Assert(S^ = "789", __LINE__) ;
        String.Dispose(S);
    END;
    IF List.Pop(S) THEN
        Assert(S^ = "456", __LINE__) ;
        String.Dispose(S);
    END;
    IF List.Pop(S) THEN
        Assert(S^ = "123", __LINE__) ;
        String.Dispose(S);
    END;
    IF List.Pop(S) THEN
        Assert(S^ = "abc", __LINE__) ;
        String.Dispose(S);
    END;
    Assert(List.Pop(S) = FALSE, __LINE__) ;
    Assert(List.IsEmpty() = TRUE, __LINE__) ;
    Assert(List.Size() = 0, __LINE__) ;

    (* PopHead *)
    List.Append(String.New("123"));
    List.Append(String.New("456"));
    List.Append(String.New("789"));
    List.AppendHead(String.New("abc"));
    Assert(List.IsEmpty() = FALSE, __LINE__) ;
    Assert(List.Size() = 4, __LINE__) ;
    IF List.PopHead(S) THEN
        Assert(S^ = "abc", __LINE__) ;
        String.Dispose(S);
    END;
    IF List.PopHead(S) THEN
        Assert(S^ = "123", __LINE__) ;
        String.Dispose(S);
    END;
    IF List.PopHead(S) THEN
        Assert(S^ = "456", __LINE__) ;
        String.Dispose(S);
    END;
    IF List.PopHead(S) THEN
        Assert(S^ = "789", __LINE__) ;
        String.Dispose(S);
    END;
    Assert(List.PopHead(S) = FALSE, __LINE__) ;
    Assert(List.IsEmpty() = TRUE, __LINE__) ;
    Assert(List.Size() = 0, __LINE__) ;

    (* Iterator *)
    List.Append(String.New("123"));
    List.Append(String.New("456"));
    List.Append(String.New("789"));
    List.AppendHead(String.New("abc"));
    Assert(List.IsEmpty() = FALSE, __LINE__) ;
    Assert(List.Size() = 4, __LINE__) ;

    List.First(It);
    IF It.Next(S) THEN
        Assert(S^ = "abc", __LINE__) ;
    END;
    IF It.Next(S) THEN
        Assert(S^ = "123", __LINE__) ;
    END;
    IF It.Next(S) THEN
        Assert(S^ = "456", __LINE__) ;
    END;
    IF It.Next(S) THEN
        Assert(S^ = "789", __LINE__) ;
    END;
    Assert(It.Next(S) = FALSE, __LINE__) ;

    List.Last(It);
    IF It.Next(S) THEN
        Assert(S^ = "789", __LINE__) ;
    END;
    IF It.Next(S) THEN
        Assert(S^ = "456", __LINE__) ;
    END;
    IF It.Next(S) THEN
        Assert(S^ = "123", __LINE__) ;
    END;
    IF It.Next(S) THEN
        Assert(S^ = "abc", __LINE__) ;
    END;
    Assert(It.Next(S) = FALSE, __LINE__) ;

    (* Dispose *)
    List.Apply(String.Dispose);
    List.Dispose();
    Assert(List.IsEmpty() = TRUE, __LINE__) ;
    Assert(List.Size() = 0, __LINE__) ;

    Testing.End(test);
END Run;

END TestADTList.