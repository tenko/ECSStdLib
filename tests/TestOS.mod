MODULE TestOS;
IMPORT Testing := O2Testing IN Std;

IN Std IMPORT Str := String, OSStream, OSPath, OSFile, OSDir;

TYPE
    TEST = Testing.TEST;

CONST
    M = "TestOS";

PROCEDURE Run* (VAR test: TEST);
VAR
    fh : OSStream.File;
    str : Str.STRING;

    PROCEDURE Assert(b: BOOLEAN; id: LONGINT) ;
    BEGIN
        Testing.Assert(test, b, M, id);
    END Assert ;
BEGIN
    Testing.Begin(test, M);

    OSPath.Join(str, 'tests', 'dummy');
    Assert(OSDir.Create(str^) = TRUE, __LINE__);
    Assert(OSFile.Exists(str^) = TRUE, __LINE__);
    Assert(OSDir.SetCurrent(str^) = TRUE, __LINE__);

    IF fh.Open("test.txt", OSStream.AccessWrite + OSStream.ModeNew) THEN
        fh.WriteString('test123'); fh.WriteNL();
        fh.WriteString('123test');
        fh.Close;
    END;

    IF fh.Open("test.txt", OSStream.AccessRead) THEN
        Assert(fh.ReadLine(str) = TRUE, __LINE__);
        Assert(str^ = 'test123', __LINE__);
        Assert(fh.ReadLine(str) = TRUE, __LINE__);
        Assert(str^ = '123test', __LINE__);
        Assert(fh.ReadLine(str) = FALSE, __LINE__);
        fh.Close;
    END;

    Assert(OSFile.Delete("test.txt") = TRUE, __LINE__);
    Assert(OSDir.SetCurrent('..') = TRUE, __LINE__);
    Assert(OSDir.Delete('dummy') = TRUE, __LINE__);

    Testing.End(test);
END Run;

END TestOS.