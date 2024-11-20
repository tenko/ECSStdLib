(*
Testing the speed of the Dictionary implementation.

NOTE - words.txt is from here (public domain):
Link : https://github.com/dwyl/english-words/

The tests is comparable to Ben Hoyt's C and Go implementation.
Link : https://github.com/benhoyt/ht

On my computer I get results about 30% of the speed of Clang with O1 optimization
and about 20% of the speed of Go. This is expected as currently the ECS compiler
does very few optimzations.
*)
MODULE Test;

IN Std IMPORT Type, Const, Char, Str := ArrayOfChar, String;
IN Std IMPORT O2Timing, OSStream;
IN Std IMPORT DictStrInt := ADTDictionary(String.STRING, LENGTH, String.Hash, String.Equal);

PROCEDURE ReportTime(title- : ARRAY OF CHAR; time : SIGNED64);
BEGIN
    OSStream.StdOut.WriteString(title); OSStream.StdOut.WriteChar(" ");
    OSStream.StdOut.FormatInteger((time DIV 60000) MOD 60, 2, Const.Zero);
    OSStream.StdOut.WriteString("m:");
    OSStream.StdOut.FormatInteger((time DIV 1000) MOD 60, 2, Const.Zero);
    OSStream.StdOut.WriteString("s:");
    OSStream.StdOut.FormatInteger(time MOD 1000, 3, Const.Zero);
    OSStream.StdOut.WriteString("ms");
    OSStream.StdOut.WriteNL;
END ReportTime;

PROCEDURE ReadFile(VAR str : Type.STRING; filename- : ARRAY OF CHAR): BOOLEAN;
VAR
    fh : OSStream.File;
    len : LENGTH;
BEGIN
    IF ~fh.Open(filename, OSStream.AccessRead) THEN RETURN FALSE END;
    IGNORE(fh.Seek(0, Const.SeekEnd));
    len := fh.Tell();
    IF len <= 0 THEN fh.Close; RETURN FALSE END;
    IGNORE(fh.Seek(0, Const.SeekSet));
    String.Reserve(str, len, FALSE);
    IF fh.ReadBytes(str^, 0, len) # len THEN fh.Close; RETURN FALSE END;
    fh.Close;
    RETURN TRUE;
END ReadFile;

PROCEDURE PopulateDict(VAR dict : DictStrInt.Dictionary): BOOLEAN;
VAR
    str, word : String.STRING;
    ch : CHAR;
    starttime, time : SIGNED64;
    i, j, value : LENGTH;
    PROCEDURE Next;
    BEGIN IF i < LEN(str^) THEN ch := str^[i]; INC(i) ELSE ch := 00X END
    END Next;
    PROCEDURE Skip();
    BEGIN WHILE Char.IsSpace(ch) DO Next() END;
    END Skip;
BEGIN
    O2Timing.StartTimer();
    starttime := O2Timing.Elapsed();
    i := 0;
    String.Reserve(word, 64, FALSE);
    IF ~ReadFile(str, "words.txt") THEN
        String.Dispose(word);
        OSStream.StdOut.WriteString("failed to open 'words.txt'."); 
        OSStream.StdOut.WriteNL;
        RETURN FALSE
    END;
    LOOP
        Next; Skip;
        IF ch = 00X THEN EXIT END;
        Str.Clear(word^);
        j := 0;
        WHILE (ch # 00X) & ~Char.IsSpace(ch) DO
            String.AppendChar(word, ch);
            INC(j); Next;
        END;
        IF j > 0 THEN
            IF ~dict.Get(word, value) THEN value := 1 END;
            dict.Set(word, value)
        END;
    END;
    String.Dispose(str); String.Dispose(word);
    time := O2Timing.Elapsed() - starttime;
    ReportTime("PopulateDict", time);
    RETURN TRUE
END PopulateDict;

PROCEDURE TestGet(VAR dict : DictStrInt.Dictionary);
VAR
    keys : DictStrInt.KeyVector;
    key : String.STRING;
    starttime, time : SIGNED64;
    i, j, value : LENGTH;
BEGIN
    keys := dict.Keys();
    O2Timing.StartTimer();
    starttime := O2Timing.Elapsed();
    FOR i := 0 TO 9 DO
        FOR j := 0 TO keys.Size() - 1 DO
            key := keys.At(j);
            IGNORE(dict.Get(key, value));
        END;
    END;
    time := O2Timing.Elapsed() - starttime;
    ReportTime("TestGet", time);
    keys.Dispose();
END TestGet;

PROCEDURE TestSet(VAR dict : DictStrInt.Dictionary);
VAR
    d : DictStrInt.Dictionary;
    keys : DictStrInt.KeyVector;
    key : String.STRING;
    starttime, time : SIGNED64;
    i : LENGTH;
BEGIN
    d.Init(2);
    d.duplicateKey := String.Duplicate;
    d.disposeKey := String.Dispose;
    keys := dict.Keys();
    O2Timing.StartTimer();
    starttime := O2Timing.Elapsed();
    FOR i := 0 TO keys.Size() - 1 DO
        key := keys.At(i);
        d.Set(key, i);
    END;
    time := O2Timing.Elapsed() - starttime;
    ReportTime("TestSet", time);
    keys.Dispose();
    d.Dispose();
END TestSet;

PROCEDURE Test;
VAR
    dict : DictStrInt.Dictionary;
BEGIN
    dict.Init(1024);
    dict.duplicateKey := String.Duplicate;
    dict.disposeKey := String.Dispose;
    IGNORE(PopulateDict(dict));
    TestGet(dict);
    TestSet(dict);
    dict.Dispose();
END Test;

BEGIN
    Test;
END Test.
