MODULE TestADTStream;
IN Std IMPORT Testing := O2Testing, Const, ADTStream, Str := String;

TYPE
    TEST = Testing.TEST;

CONST
    M = "TestADTStream";

PROCEDURE Run* (VAR test: TEST);
VAR
    fn : ADTStream.NullStream;
    fm : ADTStream.MemoryStream;
    str : Str.STRING;

    PROCEDURE Assert(b: BOOLEAN; id: LONGINT) ;
    BEGIN
        Testing.Assert(test, b, M, id);
    END Assert ;
BEGIN
    Testing.Begin(test, M);

    (* NullStream *)
    fn.WriteString('test123'); fn.WriteNL();
    fn.WriteString('123test');
    Assert(fn.ReadLine(str) = TRUE, __LINE__);

    (* MemoryStream *)
    Assert(fm.Open(0) = TRUE, __LINE__);
    fm.WriteString('test123'); fm.WriteNL();
    fm.WriteString('123test');
    Assert(fm.Seek(0, Const.SeekSet) = 0, __LINE__);
    Assert(fm.Tell() = 0, __LINE__);
    Assert(fm.ReadLine(str) = TRUE, __LINE__);
    Assert(str^ = 'test123', __LINE__);
    Assert(fm.ReadLine(str) = TRUE, __LINE__);
    Assert(str^ = '123test', __LINE__);
    Assert(fm.ReadLine(str) = FALSE, __LINE__);
    fm.Close();

    Testing.End(test);
END Run;

END TestADTStream.