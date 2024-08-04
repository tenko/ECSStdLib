MODULE TestArrayOfChar;
IMPORT SYSTEM;
IN Std IMPORT Testing := O2Testing, Char, Str := ArrayOfChar;

TYPE
    TEST = Testing.TEST;

CONST
    M = "TestArrayOfChar";

PROCEDURE FillString(VAR s : ARRAY OF CHAR);
VAR   
   i : LENGTH;
BEGIN
   i := 0;
   WHILE i <= LEN(s) - 1 DO
      s[i] := CHR(i MOD 255 + 1);
      INC(i);
   END;
END FillString;

PROCEDURE Run* (VAR test: TEST);
VAR   
    str : ARRAY 257 OF CHAR;
    dst : ARRAY 10 OF CHAR;
    chr: ARRAY 1 OF CHAR;
    i : LONGINT;
    res : BOOLEAN;

    PROCEDURE Assert(b: BOOLEAN; id: LONGINT) ;
    BEGIN
        Testing.Assert(test, b, M, id);
    END Assert ;

BEGIN
    Testing.Begin(test, M);

    chr[0] := Char.NUL;
    FillString(str);

    (*Capacity *)
    Assert(Str.Capacity(str) = 257, __LINE__) ;
    Assert(Str.Capacity(chr) = 1, __LINE__) ;

    (* Length *)
    Assert(Str.Length(str) = 257, __LINE__) ; (* Not nul terminated, length of array returned *)
    str[256] := Char.NUL;
    Assert(Str.Length(str) = 256, __LINE__) ;
    str[80] := Char.NUL;
    Assert(Str.Length(str) = 80, __LINE__) ;
    Assert(Str.Length("abc") = 3, __LINE__) ;
    Assert(Str.Length("") = 0, __LINE__) ;
    Assert(Str.Length(chr) = 0, __LINE__) ;

    (*Clear *)
    Str.Clear(str);
    Assert(Str.Length(str) = 0, __LINE__) ;

    (* Assign *)
    Str.Assign(str, "abcde");
    Assert(Str.Length(str) = 5, __LINE__) ;
    Assert((str[0] = "a") & (str[1] = "b") & (str[2] = "c") &
            (str[3] = "d") & (str[4] = "e") & (str[5] = Char.NUL), __LINE__) ;
    Str.Assign(str, "");
    Assert(Str.Length(str) = 0, __LINE__) ;
    Assert(str[0] = Char.NUL, __LINE__) ;
    Str.Assign(chr, "a");
    Assert((Str.Length(chr) = 1) & (chr[0] = "a"), __LINE__);
    Str.Assign(chr, "ab");
    Assert((Str.Length(chr) = 1) & (chr[0] = "a"), __LINE__);

    (* FillChar *)
    chr[0] := Char.NUL;
    Str.FillChar(chr, 'a');
    Assert(chr[0] = "a", __LINE__);
    FillString(str);
    Str.FillChar(str, 'a');
    i := 0;
    res := TRUE;
    WHILE i < LEN(str) DO
        IF str[i] # 'a' THEN
            res := FALSE;
        END;
        INC(i);
    END ;
    Assert(res = TRUE, __LINE__);

    (* AppendChar *)
    str := "";

    Str.AppendChar(str, Char.NUL);
    Assert(Str.Length(str) = 0, __LINE__) ;
    Assert((str[0] = Char.NUL), __LINE__) ;

    Str.AppendChar(str, "a");
    Assert(Str.Length(str) = 1, __LINE__) ;
    Assert((str[0] = "a") & (str[1] = Char.NUL), __LINE__) ;

    Str.AppendChar(str, "b");
    Assert(Str.Length(str) = 2, __LINE__) ;
    Assert((str[0] = "a") & (str[1] = "b") & (str[2] = Char.NUL), __LINE__) ;

    (* Append *)
    str := "";
    Str.Append(str, "abc");
    Assert(Str.Length(str) = 3, __LINE__) ;
    Assert((str[0] = "a") & (str[1] = "b") & (str[2] = "c") & (str[3] = Char.NUL), __LINE__) ;
    Str.Append(str, "123");
    Assert(Str.Length(str) = 6, __LINE__) ;
    Assert((str[0] = "a") & (str[1] = "b") & (str[2] = "c") &
            (str[3] = "1") & (str[4] = "2") & (str[5] = "3") & (str[6] = Char.NUL), __LINE__) ;
    FillString(str);
    Str.Append(str, "abc");
    Assert(Str.Length(str) = 257, __LINE__) ;

    (* Extract *)
    str := "abcde";
    Str.Extract(dst, str, 0, 3);
    Assert(Str.Length(dst) = 3, __LINE__) ;
    Assert((dst[0] = "a") & (dst[1] = "b") & (dst[2] = "c") & (dst[3] = Char.NUL), __LINE__) ;
    Str.Extract(dst, str, 2, 3);
    Assert(Str.Length(dst) = 3, __LINE__) ;
    Assert((dst[0] = "c") & (dst[1] = "d") & (dst[2] = "e") & (dst[3] = Char.NUL), __LINE__) ;
    Str.Extract(dst, str, 5, 3);
    Assert(Str.Length(dst) = 0, __LINE__) ;
    Assert(dst[0] = Char.NUL, __LINE__) ;
    str := "";
    Str.Extract(dst, str, 0, 3);
    Assert(Str.Length(dst) = 0, __LINE__) ;
    chr[0] := Char.NUL;
    Str.Extract(dst, chr, 0, 1);
    Assert(Str.Length(dst) = 0, __LINE__) ;

    (* Compare *)
    str := "abc";
    Assert(Str.Compare(str, "abc") = 0, __LINE__) ;
    Assert(Str.Compare("abc", str) = 0, __LINE__) ;
    Assert(Str.Compare(str, "def") < 0, __LINE__) ;
    Assert(Str.Compare("def", str) > 0, __LINE__) ;
    Assert(Str.Compare(str, "") > 0, __LINE__) ;
    Assert(Str.Compare("", str) < 0, __LINE__) ;
    str := "abcde";
    Assert(Str.Compare(str, "abcde") = 0, __LINE__) ;
    Assert(Str.Compare("abcde", str) = 0, __LINE__) ;
    Assert(Str.Compare(str, "fghi") < 0, __LINE__) ;
    Assert(Str.Compare("fghi", str) > 0, __LINE__) ;
    Assert(Str.Compare(str, "") > 0, __LINE__) ;
    Assert(Str.Compare("", str) < 0, __LINE__) ;
    str := "";
    chr[0] := Char.NUL;
    Assert(Str.Compare(str, chr) = 0, __LINE__) ;
    str := "a";
    chr[0] := "b";
    Assert(Str.Compare(str, chr) < 0, __LINE__) ;

    (* IndexChar *)
    chr[0] := 'a';
    Assert(Str.IndexChar('a', chr, 0) = 0, __LINE__) ;
    Assert(Str.IndexChar('a', chr, 1) = -1, __LINE__) ;
    Assert(Str.IndexChar('b', chr, 0) = -1, __LINE__) ;
    Str.Assign(str, 'file.dat');
    Assert(Str.IndexChar('.', str, 0) = 4, __LINE__) ;
    Assert(Str.IndexChar('.', str, 5) = -1, __LINE__) ;
    Assert(Str.IndexChar('d', str, 0) = 5, __LINE__) ;
    Assert(Str.IndexChar('d', str, 1) = 5, __LINE__) ;
    Str.Assign(str, 'thisisaverylongfilename.dat');
    Assert(Str.IndexChar('.', str, 3) = 23, __LINE__) ;

    (* Index *)
    str := "abc";
    Assert(Str.Index(str, str, 0) = 0, __LINE__) ;
    Assert(Str.Index("abc", str, 0) = 0, __LINE__) ;
    Assert(Str.Index("def", str, 0) = -1, __LINE__) ;
    Assert(Str.Index("", str, 0) = 0, __LINE__) ;
    Assert(Str.Index("a", str, 0) = 0, __LINE__) ;
    Assert(Str.Index("b", str, 0) = 1, __LINE__) ;
    Assert(Str.Index("c", str, 0) = 2, __LINE__) ;
    Assert(Str.Index("bc", str, 0) = 1, __LINE__) ;
    str := "";
    Assert(Str.Index("abc", str, 0) = -1, __LINE__) ;
    Assert(Str.Index("", str, 0) = 0, __LINE__) ;
    chr[0] := "a";
    Assert(Str.Index("a", chr, 0) = 0, __LINE__) ;
    Assert(Str.Index("a", chr, 1) = -1, __LINE__) ;
    Assert(Str.Index("b", chr, 0) = -1, __LINE__) ;

    (* Delete *)
    str := "abc";
    Str.Delete(str, 0, 3);
    Assert(Str.Length(str) = 0, __LINE__) ;
    str := "abc";
    Str.Delete(str, 0, 1);
    Assert(Str.Length(str) = 2, __LINE__) ;
    Assert((str[0] = "b") & (str[1] = "c") & (str[2] = Char.NUL), __LINE__) ;
    str := "abc";
    Str.Delete(str, 1, 5);
    Assert(Str.Length(str) = 1, __LINE__) ;
    Assert((str[0] = "a") & (str[1] = Char.NUL), __LINE__) ;
    str := "abc";
    Str.Delete(str, 3, 1);
    Assert(Str.Length(str) = 3, __LINE__) ;
    Assert((str[0] = "a") & (str[1] = "b") & (str[2] = "c") & (str[3] = Char.NUL), __LINE__) ;
    chr[0] := "a";
    Str.Delete(chr, 0, 1);
    Assert(Str.Length(chr) = 0, __LINE__) ;
    
    (* Insert *)
    chr[0] := Char.NUL;
    Str.Insert(chr, 'a', 0);
    Assert(Str.Length(chr) = 1, __LINE__) ;
    Assert(chr[0] = "a", __LINE__) ;

    Str.Insert(chr, 'b', 0);
    Assert(Str.Length(chr) = 1, __LINE__) ;
    Assert(chr[0] = "a", __LINE__) ;

    str := "";
    Str.Insert(str, 'abc', 0);
    Assert(Str.Length(str) = 3, __LINE__) ;
    Assert((str[0] = "a") & (str[1] = "b") & (str[2] = "c") & (str[3] = Char.NUL), __LINE__) ;

    str := "abc";
    Str.Insert(str, "", 0);
    Assert(Str.Length(str) = 3, __LINE__) ;
    Assert((str[0] = "a") & (str[1] = "b") & (str[2] = "c") & (str[3] = Char.NUL), __LINE__) ;

    str := "abc";
    Str.Insert(str, " ", 0);
    Assert(Str.Length(str) = 4, __LINE__) ;
    Assert((str[0] = " ") & (str[1] = "a") & (str[2] = "b") & (str[3] = "c") & (str[4] = Char.NUL), __LINE__) ;

    (* Replace *)
    str := "abc";
    Str.Replace(str, "b", "d", 0);
    Assert(Str.Length(str) = 3, __LINE__) ;
    Assert((str[0] = "a") & (str[1] = "d") & (str[2] = "c") & (str[3] = Char.NUL), __LINE__) ;

    str := "abcd";
    Str.Replace(str, "cd", "ef", 0);
    Assert(Str.Length(str) = 4, __LINE__) ;
    Assert((str[0] = "a") & (str[1] = "b") & (str[2] = "e") & (str[3] = "f") & (str[4] = Char.NUL), __LINE__) ;

    str := "abc";
    Str.Replace(str, "def", "abc", 0);
    Assert(Str.Length(str) = 3, __LINE__) ;
    Assert((str[0] = "a") & (str[1] = "b") & (str[2] = "c") & (str[3] = Char.NUL), __LINE__) ;

    str := "abc";
    Str.Replace(str, "abc", "def", 3);
    Assert(Str.Length(str) = 3, __LINE__) ;
    Assert((str[0] = "a") & (str[1] = "b") & (str[2] = "c") & (str[3] = Char.NUL), __LINE__) ;
    
    chr[0] := "a";
    Str.Replace(chr, "a", "b", 0);
    Assert(Str.Length(chr) = 1, __LINE__) ;
    Assert(chr[0] = "b", __LINE__) ;

    chr[0] := "a";
    Str.Replace(chr, "b", "a", 0);
    Assert(Str.Length(chr) = 1, __LINE__) ;
    Assert(chr[0] = "a", __LINE__) ;

    chr[0] := Char.NUL;
    Str.Replace(chr, "a", "b", 0);
    Assert(Str.Length(chr) = 0, __LINE__) ;

    (* LeftTrim *)
    str := "abc";
    Str.LeftTrim(str);
    Assert((str[0] = "a") & (str[1] = "b") & (str[2] = "c") & (str[3] = Char.NUL), __LINE__) ;
    str := " abc";
    Str.LeftTrim(str);
    Assert((str[0] = "a") & (str[1] = "b") & (str[2] = "c") & (str[3] = Char.NUL), __LINE__) ;
    str := "    abc";
    Str.LeftTrim(str);
    Assert((str[0] = "a") & (str[1] = "b") & (str[2] = "c") & (str[3] = Char.NUL), __LINE__) ;
    str := "    ";
    Str.LeftTrim(str);
    Assert((str[0] =  Char.NUL), __LINE__) ;
    chr[0] := Char.NUL;
    Str.LeftTrim(chr);
    Assert(chr[0] =  Char.NUL, __LINE__) ;
    chr[0] := "a";
    Str.LeftTrim(chr);
    Assert(chr[0] = "a",  __LINE__) ;
    chr[0] := " ";
    Str.LeftTrim(chr);
    Assert(chr[0] =  Char.NUL, __LINE__) ;

    (* RightTrim *)
    str := "abc";
    Str.RightTrim(str);
    Assert((str[0] = "a") & (str[1] = "b") & (str[2] = "c") & (str[3] = Char.NUL), __LINE__) ;
    str := "abc ";
    Str.RightTrim(str);
    Assert((str[0] = "a") & (str[1] = "b") & (str[2] = "c") & (str[3] = Char.NUL), __LINE__) ;
    str := "abc   ";
    Str.RightTrim(str);
    Assert((str[0] = "a") & (str[1] = "b") & (str[2] = "c") & (str[3] = Char.NUL), __LINE__) ;
    str := "    ";
    Str.RightTrim(str);
    Assert(str[0] =  Char.NUL, __LINE__) ;
    chr[0] := Char.NUL;
    Str.RightTrim(chr);
    Assert(chr[0] =  Char.NUL, __LINE__) ;
    chr[0] := "a";
    Str.RightTrim(chr);
    Assert(chr[0] =  "a", __LINE__) ;
    chr[0] := " ";
    Str.RightTrim(chr);
    Assert(chr[0] =  Char.NUL, __LINE__) ;

    (* Trim *)
    str := "abc";
    Str.Trim(str);
    Assert((str[0] = "a") & (str[1] = "b") & (str[2] = "c") & (str[3] = Char.NUL), __LINE__) ;
    str := "abc ";
    Str.Trim(str);
    Assert((str[0] = "a") & (str[1] = "b") & (str[2] = "c") & (str[3] = Char.NUL), __LINE__) ;
    str := " abc   ";
    Str.Trim(str);
    Assert((str[0] = "a") & (str[1] = "b") & (str[2] = "c") & (str[3] = Char.NUL), __LINE__) ;
    str := "    ";
    Str.Trim(str);
    Assert(str[0] =  Char.NUL, __LINE__) ;
    chr[0] := Char.NUL;
    Str.Trim(chr);
    Assert(chr[0] =  Char.NUL, __LINE__) ;
    chr[0] := "a";
    Str.Trim(chr);
    Assert(chr[0] = "a", __LINE__) ;
    chr[0] := " ";
    Str.Trim(chr);
    Assert(chr[0] =  Char.NUL, __LINE__) ;

    (* LeftPad *)
    chr[0] := Char.NUL;
    Str.LeftPad(chr, 0, ' ');
    Assert(chr[0] = Char.NUL, __LINE__) ;
    Str.LeftPad(chr, 1, ' ');
    Assert(chr[0] = ' ', __LINE__) ;
    chr[0] := Char.NUL;
    Str.LeftPad(chr, 2, ' ');
    Assert(chr[0] = ' ', __LINE__) ;
    str := "abc";
    Str.LeftPad(str, 0, ' ');
    Assert((str[0] = "a") & (str[1] = "b") & (str[2] = "c") & (str[3] = Char.NUL), __LINE__) ;
    str := "abc";
    Str.LeftPad(str, 5, ' ');
    Assert((str[0] = " ") & (str[1] = " ") & (str[2] = "a") & (str[3] = "b") & (str[4] = "c") & (str[5] = Char.NUL), __LINE__) ;

    (* RightPad *)
    chr[0] := Char.NUL;
    Str.RightPad(chr, 0, ' ');
    Assert(chr[0] = Char.NUL, __LINE__) ;
    Str.RightPad(chr, 1, ' ');
    Assert(chr[0] = ' ', __LINE__) ;
    chr[0] := Char.NUL;
    Str.RightPad(chr, 2, ' ');
    Assert(chr[0] = ' ', __LINE__) ;
    str := "abc";
    Str.RightPad(str, 0, ' ');
    Assert((str[0] = "a") & (str[1] = "b") & (str[2] = "c") & (str[3] = Char.NUL), __LINE__) ;
    str := "abc";
    Str.RightPad(str, 5, ' ');
    Assert((str[0] = "a") & (str[1] = "b") & (str[2] = "c") & (str[3] = " ") & (str[4] = " ") &  (str[5] = Char.NUL), __LINE__) ;
    
    (* LowerCase *)
    str := "abc";
    Str.LowerCase(str);
    Assert((str[0] = "a") & (str[1] = "b") & (str[2] = "c") & (str[3] = Char.NUL), __LINE__) ;
    str := "ABC";
    Str.LowerCase(str);
    Assert((str[0] = "a") & (str[1] = "b") & (str[2] = "c") & (str[3] = Char.NUL), __LINE__) ;
    str := "AbC";
    Str.LowerCase(str);
    Assert((str[0] = "a") & (str[1] = "b") & (str[2] = "c") & (str[3] = Char.NUL), __LINE__) ;
    str := "";
    Str.LowerCase(str);
    Assert(str[0] =  Char.NUL, __LINE__) ;
    chr[0] := Char.NUL;
    Str.LowerCase(chr);
    Assert(chr[0] =  Char.NUL, __LINE__) ;
    chr[0] := "a";
    Str.LowerCase(chr);
    Assert(chr[0] =  "a", __LINE__) ;
    chr[0] := "A";
    Str.LowerCase(chr);
    Assert(chr[0] =  "a", __LINE__) ;

    (* UpperCase *)
    str := "abc";
    Str.UpperCase(str);
    Assert((str[0] = "A") & (str[1] = "B") & (str[2] = "C") & (str[3] = Char.NUL), __LINE__) ;
    str := "ABC";
    Str.UpperCase(str);
    Assert((str[0] = "A") & (str[1] = "B") & (str[2] = "C") & (str[3] = Char.NUL), __LINE__) ;
    str := "AbC";
    Str.UpperCase(str);
    Assert((str[0] = "A") & (str[1] = "B") & (str[2] = "C") & (str[3] = Char.NUL), __LINE__) ;
    str := "";
    Str.UpperCase(str);
    Assert(str[0] =  Char.NUL, __LINE__) ;
    chr[0] := Char.NUL;
    Str.UpperCase(chr);
    Assert(chr[0] =  Char.NUL, __LINE__) ;
    chr[0] := "a";
    Str.UpperCase(chr);
    Assert(chr[0] =  "A", __LINE__) ;
    chr[0] := "A";
    Str.UpperCase(chr);
    Assert(chr[0] =  "A", __LINE__) ;

    (* Capitalize *)
    str := "abc";
    Str.Capitalize(str);
    Assert((str[0] = "A") & (str[1] = "b") & (str[2] = "c") & (str[3] = Char.NUL), __LINE__) ;
    str := " abc";
    Str.Capitalize(str);
    Assert((str[0] = " ") & (str[1] = "A") & (str[2] = "b") & (str[3] = "c") & (str[4] = Char.NUL), __LINE__) ;
    str := "";
    Str.Capitalize(str);
    Assert(str[0] =  Char.NUL, __LINE__) ;
    str := "a";
    Str.Capitalize(str);
    Assert((str[0] = "A") & (str[1] = Char.NUL), __LINE__) ;

    (* StartsWith *)
    str := "abc";
    Assert(Str.StartsWith(str, 'a') = TRUE, __LINE__) ;
    Assert(Str.StartsWith(str, 'ab') = TRUE, __LINE__) ;
    Assert(Str.StartsWith(str, 'abc') = TRUE, __LINE__) ;
    Assert(Str.StartsWith(str, "") = TRUE, __LINE__) ;
    Assert(Str.StartsWith(str, 'b') = FALSE, __LINE__) ;

    (* EndsWith *)
    str := "abc";
    Assert(Str.EndsWith(str, 'c') = TRUE, __LINE__) ;
    Assert(Str.EndsWith(str, 'bc') = TRUE, __LINE__) ;
    Assert(Str.EndsWith(str, 'abc') = TRUE, __LINE__) ;
    Assert(Str.EndsWith(str, "") = TRUE, __LINE__) ;
    Assert(Str.EndsWith(str, 'a') = FALSE, __LINE__) ;

    (* Match *)
    Assert(Str.Match("", "", FALSE) = TRUE, __LINE__);
    Assert(Str.Match("", "*", FALSE) = TRUE, __LINE__);
    Assert(Str.Match("Filename.exe", "*.exe", FALSE) = TRUE, __LINE__);
    Assert(Str.Match("Filename.exe", "f*.exe", FALSE) = FALSE, __LINE__);
    Assert(Str.Match("Filename.exe", "f*.exe", TRUE) = TRUE, __LINE__);

    str := 'foobar';
    IF SIZE(SYSTEM.ADDRESS) = 8 THEN
        Assert(Str.Hash(str) = 085944171F73967E8H, __LINE__) ;
    ELSE
        Assert(Str.Hash(str) = LENGTH(0BF9CF968H), __LINE__) ;
    END;

    Testing.End(test);
END Run;

END TestArrayOfChar.