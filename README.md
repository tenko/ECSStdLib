# ECSStdLib

The **[ECS](https://ecs.openbrace.org/)** standard library defines modules supporting application development  
with the **[ECS Oberon](https://ecs.openbrace.org/manual/manualch7.html)** compiler, including:

 * Modules for basic types : [Char](https://tenko.github.io/ECSStdLib/src/Std.Char.mod.html), [Integer](https://tenko.github.io/ECSStdLib/src/Std.Integer.mod.html), [Cardinal](https://tenko.github.io/ECSStdLib/src/Std.Cardinal.mod.html), [Real](https://tenko.github.io/ECSStdLib/src/Std.Real.mod.html), [ArrayOfChar](https://tenko.github.io/ECSStdLib/src/Std.ArrayOfChar.mod.html), [ArrayOfByte](https://tenko.github.io/ECSStdLib/src/Std.ArrayOfByte.mod.html) & [ArrayOfSet](https://tenko.github.io/ECSStdLib/src/Std.ArrayOfSet.mod.html) .
 * Modules for string handling : [String](https://tenko.github.io/ECSStdLib/src/Std.String.mod.html) & [StringPattern](https://tenko.github.io/ECSStdLib/src/Std.StringPattern.mod.html).
 * Module for date and time support : [DateTime](https://tenko.github.io/ECSStdLib/src/Std.DateTime.mod.html).
 * Module for container and alogrithms : [ADTStream](https://tenko.github.io/ECSStdLib/src/Std.ADTStream.mod.html), [ADTDictionary](https://tenko.github.io/ECSStdLib/src/Std.ADTDictionary.mod.html), [ADTList](https://tenko.github.io/ECSStdLib/src/Std.ADTList.mod.html), [ADTSet](https://tenko.github.io/ECSStdLib/src/Std.ADTSet.mod.html), [ADTTree](https://tenko.github.io/ECSStdLib/src/Std.ADTTree.mod.html) & [ADTVector](https://tenko.github.io/ECSStdLib/src/Std.ADTVector.mod.html).
 * Module for cross platform basic OS support : [OS](https://tenko.github.io/ECSStdLib/src/Std.OS.mod.html), [OSDir](https://tenko.github.io/ECSStdLib/src/Std.OSDir.mod.html), [OSFile](https://tenko.github.io/ECSStdLib/src/Std.OSFile.mod.html), [OSPath](https://tenko.github.io/ECSStdLib/src/Std.OSPath.mod.html) & [OSStream](https://tenko.github.io/ECSStdLib/src/Std.OSStream.mod.html).
 * Module for testing & benchmark : [O2Testing](https://tenko.github.io/ECSStdLib/src/Std.O2Testing.mod.html) & [O2Timing](https://tenko.github.io/ECSStdLib/src/Std.O2Timing.mod.html).
 * Modules for data handling: [DataLZ4](https://tenko.github.io/ECSStdLib/src/Std.DataLZ4.mod.html) & [DataConfig](https://tenko.github.io/ECSStdLib/src/Std.DataConfig.mod.html) .
 * Module for memory allocation on embedded platforms : [SysMem](https://github.com/tenko/ECSStdLib/blob/main/src/Std.SysMem.mod).

The [Const](https://tenko.github.io/ECSStdLib/src/Std.Const.mod.html) module defines constants reused throughout the library.

The stream concept is used troughout the modules to support formatting, reading and writing of data.  
The module [Type](https://tenko.github.io/ECSStdLib/src/Std.Type.mod.html) defines the basic stream type interface. 

OS support is covered for the **Windows** and **Linux** platforms.  
In addition for embedded platform basic support is added for the [ARMv7M](https://github.com/tenko/ECSStdLib/blob/main/src/armv7mrun.asm) platform using  
the semihost interface to the host computer.

The library is tested with about 750 unit tests.  
These tests also can be inspected for basic usage where the documentation is not clear.  

The **[ECS Oberon](https://ecs.openbrace.org/manual/manualch7.html)** compiler is implemented according to the original
Oberon-2 [report](https://www.ssw.uni-linz.ac.at/Research/Papers/Oberon2.pdf) with modernizing extensions.

## Building & Running tests

Building

> make

Running tests (Linux)

> make TestMain

Running tests (Windows)

> make TestMain.exe

## Example showcasing of library features implementing a top 10 word counter utility.

Test.mod:

```modula-2
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

```

Running (Windows)

```
ecsd Test.mod std.lib /c/EigenCompilerSuite/runtime/win64api.obf
./test.exe README.md
 36 : https
 34 : std
 32 : github
 32 : ecsstdlib
 32 : tenko
 31 : mod
 30 : html
 30 : src
 29 : io
 24 : string
```

## Note

Complete API Documentation: [Link](https://tenko.github.io/ECSStdLib/)  
Currently a patched version of the **ECS** compiler is needed [Link](https://github.com/tenko/ECS)  
With the next release of the **ECS** compiler these patches should be included. 
