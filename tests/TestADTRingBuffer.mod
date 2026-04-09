MODULE TestADTRingBuffer;

IMPORT Testing := O2Testing IN Std;
IMPORT IntBuffer := ADTRingBuffer(INTEGER, 8) IN Std;

TYPE
    TEST = Testing.TEST;

CONST
    M = "TestADTRingBuffer";

PROCEDURE Run* (VAR test: TEST);
VAR
    buf : IntBuffer.RingBuffer;
    i, val : INTEGER;
    ret : BOOLEAN;

    PROCEDURE Assert(b: BOOLEAN; id: LONGINT) ;
    BEGIN
        Testing.Assert(test, b, M, id);
    END Assert ;
BEGIN
    Testing.Begin(test, M);

    IntBuffer.Init(buf);
    
    Assert(IntBuffer.Size = 8, __LINE__);
    Assert(buf.Size() = 0, __LINE__);
    FOR i := 0 TO IntBuffer.Size - 2 DO
        IGNORE(buf.Push(i + 1));
    END;
    Assert(buf.Size() = IntBuffer.Size - 1, __LINE__);
    Assert(buf.Push(8) = FALSE, __LINE__);
    ret := buf.Pop(val);
    Assert((ret = TRUE) & (val = 1), __LINE__);
    ret := buf.Pop(val);
    Assert((ret = TRUE) & (val = 2), __LINE__);
    ret := buf.Pop(val);
    Assert((ret = TRUE) & (val = 3), __LINE__);
    FOR i := 0 TO 2 DO
        IGNORE(buf.Push(i + 1));
    END;
    Assert(buf.Size() = IntBuffer.Size - 1, __LINE__);
    ret := buf.Peek(0, val);
    Assert((ret = TRUE) & (val = 4), __LINE__);
    ret := buf.Peek(6, val);
    Assert((ret = TRUE) & (val = 3), __LINE__);
    ret := buf.Peek(7, val);
    Assert(ret = FALSE, __LINE__);
    ret := TRUE;
    FOR i := 0 TO 6 DO
        IF ~buf.Pop(val) THEN ret := FALSE END
    END;
    Assert(ret = TRUE, __LINE__);
    Assert(buf.Size() = 0, __LINE__);

    Testing.End(test);
END Run;

END TestADTRingBuffer.