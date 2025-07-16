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

    IGNORE(fh.Truncate(0));
    fh.WriteString("# This is a valid comments"); fh.WriteNL;
    fh.WriteString("[Section]"); fh.WriteNL;
    fh.WriteString("key=value"); fh.WriteNL;
    fh.WriteString("Long Key=Long Value"); fh.WriteNL;
    fh.WriteString("emptyvalue="); fh.WriteNL;

    IGNORE(fh.Seek(ADTStream.SeekSet, 0));
    parser.Clear();
    Assert(parser.Read(fh) = 0, __LINE__);

    IGNORE(fh.Truncate(0));
    Assert(parser.Write(fh) = TRUE, __LINE__);
    IGNORE(fh.Seek(ADTStream.SeekSet,0));
    Assert(fh.ReadLine(value) = TRUE, __LINE__);
    Assert(value^ = "[section]", __LINE__);
    Assert(fh.ReadLine(value) = TRUE, __LINE__);
    Assert(value^ = "emptyvalue = ", __LINE__);
    Assert(fh.ReadLine(value) = TRUE, __LINE__);
    Assert(value^ = "key = value", __LINE__);
    Assert(fh.ReadLine(value) = TRUE, __LINE__);
    Assert(value^ = "long key = Long Value", __LINE__);
    Assert(fh.ReadLine(value) = FALSE, __LINE__);
    
    parser.Dispose;
    String.Dispose(value);
    fh.Close;

    Testing.End(test);
END Run;

END TestDataConfig.