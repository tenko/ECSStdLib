MODULE TestArrayOfByte;
IMPORT Testing := O2Testing IN Std, SYSTEM, ArrayOfByte IN Std;

TYPE
    TEST = Testing.TEST;
    BYTE = SYSTEM.BYTE;

CONST
    M = "TestArrayOfByte";

PROCEDURE Run* (VAR test: TEST);
VAR   
    s : ARRAY 64 OF UNSIGNED32;
    str : ARRAY 15 OF CHAR;
    bs, bd : ARRAY 64 OF BYTE;
    bss : ARRAY 32 OF BYTE;
    h1 : ARRAY 6 OF BYTE;
    i : LENGTH;
    ok : BOOLEAN;

    PROCEDURE Assert(b: BOOLEAN; id: LONGINT) ;
    BEGIN
        Testing.Assert(test, b, M, id);
    END Assert ;

BEGIN
    Testing.Begin(test, M);

    (* Zero *)
    ArrayOfByte.Zero(s);
    ok := TRUE;
    FOR i := 0 TO LEN(s) - 1 DO
        IF s[i] # 0 THEN ok := FALSE END;
    END;
    Assert(ok, __LINE__);

    (* Fill *)
    ok := TRUE;
    ArrayOfByte.Fill(s, 1);
    FOR i := 0 TO LEN(s) - 1 DO
        IF s[i] # 01010101H THEN ok := FALSE END;
    END;
    Assert(ok, __LINE__);

    (* Copy *)
    ArrayOfByte.Zero(bs);
    ArrayOfByte.Fill(bs, 1);
    ArrayOfByte.Copy(bd, bs, -1);
    ok := TRUE;
    FOR i := 0 TO LEN(bd) - 1 DO
        IF UNSIGNED8(bd[i]) # 01H THEN ok := FALSE END;
    END;
    Assert(ok, __LINE__);

    (* IsZero *)
    ArrayOfByte.Zero(bs);
    Assert(ArrayOfByte.IsZero(bs) = TRUE, __LINE__);
    bs[63] := 01X;
    Assert(ArrayOfByte.IsZero(bs) = FALSE, __LINE__);

    (* Compare *)
    ArrayOfByte.Fill(bs, 1);
    ArrayOfByte.Fill(bd, 1);
    Assert(ArrayOfByte.Equal(bs, bd) = TRUE, __LINE__);
    ArrayOfByte.Fill(bs, 1);
    ArrayOfByte.Fill(bd, 2);
    Assert(ArrayOfByte.Equal(bs, bd) = FALSE, __LINE__);
    ArrayOfByte.Fill(bs, 2);
    ArrayOfByte.Fill(bd, 1);
    Assert(ArrayOfByte.Equal(bs, bd) = FALSE, __LINE__);
    ArrayOfByte.Fill(bs, 1);
    ArrayOfByte.Fill(bss, 1);
    Assert(ArrayOfByte.Equal(bs, bss) = FALSE, __LINE__);

    (* Hash *)
    str := 'foobar';
    FOR i := 0 TO LEN(h1) - 1 DO h1[i] := str[i] END;
    IF SIZE(SYSTEM.ADDRESS) = 8 THEN
        Assert(ArrayOfByte.Hash(h1) = 085944171F73967E8H, __LINE__) ;
    ELSE
        Assert(ArrayOfByte.Hash(h1) = LENGTH(0BF9CF968H), __LINE__) ;
    END;

    Testing.End(test);
END Run;

END TestArrayOfByte.