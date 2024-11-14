MODULE TestDataConfig;
IN Std IMPORT Testing := O2Testing, DataConfig, ADTStream, String;

TYPE
    TEST = Testing.TEST;

CONST
    ER1 = "Comment without proper start";
    ER2 = "[Section";
    ER3 = "[Section]
          [Section]";
    ER4 = "[]";
    OK1 = "# This is a valid comments";
    OK2 = "[Section]";
    OK3 = "[Section]
           key without value";
    OK4 = "[Section]
           key without value=";
    OK5 = "[Section]
           key=value";
    M = "TestDataConfig";
    
PROCEDURE Run* (VAR test: TEST);
VAR
    fh : ADTStream.MemoryStream;
    parser : DataConfig.Parser;
    value : String.STRING;
    ret : LENGTH;
    res : BOOLEAN;
    PROCEDURE ParseString(str- : ARRAY OF CHAR) : INTEGER;
    BEGIN
        IGNORE(fh.Truncate(0));
        IGNORE(fh.Seek(0, 0));
        fh.WriteString(str);
        IGNORE(fh.Seek(0, 0));
        parser.Clear();
        RETURN parser.Read(fh);
    END ParseString;
    PROCEDURE Assert(b: BOOLEAN; id: LONGINT) ;
    BEGIN
        Testing.Assert(test, b, M, id);
    END Assert ;
BEGIN
    Testing.Begin(test, M);

    DataConfig.InitParser(parser);
    IGNORE(fh.Open(0));
    
    ret := ParseString(ER1);
    Assert(ret # 0, __LINE__);

    ret := ParseString(ER2);
    Assert(ret # 0, __LINE__);

    ret := ParseString(ER3);
    Assert(ret # 0, __LINE__);

    ret := ParseString(ER4);
    Assert(ret # 0, __LINE__);

    ret := ParseString(OK1);
    Assert(ret = 0, __LINE__);

    ret := ParseString(OK2);
    Assert(ret = 0, __LINE__);

    ret := ParseString(OK3);
    Assert(ret = 0, __LINE__);

    ret := ParseString(OK4);
    Assert(ret = 0, __LINE__);

    ret := ParseString(OK5);
    Assert(ret = 0, __LINE__);

    Assert(parser.HasSection("Section"), __LINE__);
    res := parser.Get(value, "Section", "key");
    Assert(res & (value^ = "value"), __LINE__);

    parser.Dispose;
    String.Dispose(value);
    fh.Close;

    Testing.End(test);
END Run;

END TestDataConfig.