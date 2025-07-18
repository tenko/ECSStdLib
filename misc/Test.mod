(** A showcase of library features implementing a top 10 word counter utility. *)
MODULE test;

IN Std IMPORT ArrayOfChar, String, StringPattern, OS, OSStream, ADTStream;
IN Std IMPORT PairStrInt := ADTPair(String.STRING, LENGTH);
IN Std IMPORT TreePair := ADTTree(PairStrInt.Pair, PairStrInt.Compare);
IN Std IMPORT DictStrInt := ADTDictionary(String.STRING, LENGTH, String.Hash, String.Equal);

PROCEDURE PairCompare(fl-, fr- : String.STRING; sl-, sr- : LENGTH): INTEGER;
BEGIN
    IF (sl = sr) OR (sl < sr) THEN RETURN -1 END; (* If we return 0, pairs would be overwritten *)
    RETURN 1;
END PairCompare;

PROCEDURE ProcessFile(VAR fh : ADTStream.ADTStream);
VAR
    dict : DictStrInt.Dictionary;
    dit : DictStrInt.Iterator;
    tree : TreePair.Tree;
    tit : TreePair.Iterator;
    pair : PairStrInt.Pair;
    pat : StringPattern.Pattern;
    s, line, word: String.STRING;
    idx, pos, len, cnt : LENGTH;
BEGIN
    PairStrInt.compare := PairCompare;
    dict.Init(256);
    dict.duplicateKey := String.Duplicate; (* Dictionary insert copies of strings *)
    dict.disposeKey := String.Dispose; (* Dictionary disposes of strings *)
    WHILE fh.ReadLine(line) DO
        idx := 0;
        WHILE pat.Find("%w+", line^, idx) # -1 DO
            IF pat.Capture(0, pos, len) THEN
                String.Extract(word, line^, pos, len);
                ArrayOfChar.LowerCase(word^);
                IF ~dict.Get(word, cnt) THEN cnt := 0 END;
                dict.Set(word, cnt + 1);
            END;
            idx := pos + len + 1;
        END;
    END;
    tree.Init();
    dict.First(dit);
    WHILE dit.NextItem(s, cnt) DO
        pair.first := s;
        pair.second := cnt;
        tree.Insert(pair);
    END;
    tree.Last(tit);
    cnt := 0;
    WHILE tit.Next(pair) & (cnt < 10) DO
        OSStream.StdOut.FormatInteger(pair.second, 3, {});
        OSStream.StdOut.WriteString(" : ");
        OSStream.StdOut.WriteString(pair.first^);
        OSStream.StdErr.WriteNL;
        INC(cnt);
    END;
    String.Dispose(line);
    String.Dispose(word);
    dict.Dispose();
    tree.Dispose();
END ProcessFile;

PROCEDURE Run;
VAR
    fh : OSStream.File;
    s : String.STRING;
BEGIN
    IF OS.Args() = 1 THEN
        ProcessFile(OSStream.StdIn);
    ELSIF OS.Args() = 2 THEN
        OS.Arg(s, 1);
        IF ~fh.Open(s^, OSStream.AccessRead) THEN
            OSStream.StdErr.WriteString("Failed to open file"); OSStream.StdErr.WriteNL;
            String.Dispose(s);
            OS.Exit(1);
        END;
        ProcessFile(fh);
        String.Dispose(s);
        fh.Close();
    ELSE
        OSStream.StdErr.WriteString("Usage : test [filename]"); OSStream.StdErr.WriteNL;
        OS.Exit(1);
    END;
END Run;

BEGIN
    Run;
END test.
