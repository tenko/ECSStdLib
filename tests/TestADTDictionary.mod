MODULE TestADTDictionary;
IN Std IMPORT Testing := O2Testing, String;

IMPORT ADTDictionary(String.STRING, INTEGER, String.Hash, String.Equal) IN Std;

TYPE
    TEST = Testing.TEST;

CONST
    M = "TestADTDictionary";

PROCEDURE Run* (VAR test: TEST);
VAR
    s, key : String.STRING;
    dict : ADTDictionary.Dictionary;
    it : ADTDictionary.Iterator;
    values : ADTDictionary.ValueVector;
    keys : ADTDictionary.KeyVector;
    value : INTEGER;
    ret : BOOLEAN;

    PROCEDURE Assert(b: BOOLEAN; id: LONGINT) ;
    BEGIN
        Testing.Assert(test, b, M, id);
    END Assert ;
BEGIN
    Testing.Begin(test, M);

    dict.Init(2);
    (* dictionary handles copy of keys *)
    dict.duplicateKey := String.Duplicate;
    dict.disposeKey := String.Dispose;

    dict.Set(String.S(key, "test"), 1);
    dict.Set(String.S(key, "a"), 4);
    dict.Set(String.S(key, "c"), -1);

    value := 0;
    ret := dict.Get(String.S(key, "test"), value);
    Assert(ret & (value = 1), __LINE__);

    ret := dict.Get(String.S(key, "a"), value);
    Assert(ret & (value = 4), __LINE__);

    ret := dict.Get(String.S(key, "c"), value);
    Assert(ret & (value = -1), __LINE__);

    ret := dict.Get(String.S(key, "b"), value);
    Assert(~ret, __LINE__);

    dict.Discard(String.S(key, "a"));

    ret := dict.Get(String.S(key, "test"), value);
    Assert(ret & (value = 1), __LINE__);

    ret := dict.Get(String.S(key, "a"), value);
    Assert(~ret, __LINE__);

    ret := dict.Get(String.S(key, "c"), value);
    Assert(ret & (value = -1), __LINE__);

    ret := dict.Get(String.S(key, "b"), value);
    Assert(~ret, __LINE__);

    dict.Clear();
    Assert(~dict.PopItem(s, value), __LINE__);
    String.Dispose(s);
    
    dict.Set(String.S(key, "test"), 1);
    dict.Set(String.S(key, "a"), 4);
    dict.Set(String.S(key, "c"), -1);

    value := 0;
    ret := dict.Get(String.S(key, "test"), value);
    Assert(ret & (value = 1), __LINE__);

    ret := dict.Get(String.S(key, "a"), value);
    Assert(ret & (value = 4), __LINE__);

    ret := dict.Get(String.S(key, "c"), value);
    Assert(ret & (value = -1), __LINE__);

    ret := dict.Get(String.S(key, "b"), value);
    Assert(~ret, __LINE__);

    dict.First(it);
    WHILE it.NextItem(s, value) DO
        Assert((value = 1) OR (value = 4) OR (value = -1), __LINE__);
    END;

    (* Dispose strings *)
    String.Dispose(key);
    dict.Dispose();

    Testing.End(test);
END Run;

END TestADTDictionary.