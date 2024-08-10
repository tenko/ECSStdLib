MODULE TestString;
IMPORT Testing := O2Testing IN Std;
IN Std IMPORT Const, Char, S := ArrayOfChar, Str := String, DateTime;

TYPE
    TEST = Testing.TEST;

CONST
    M = "TestString";

PROCEDURE Run* (VAR test: TEST);
VAR
    str, dst : Str.STRING;
    d : DateTime.DATETIME;

    PROCEDURE Assert(b: BOOLEAN; id: LONGINT) ;
    BEGIN
        Testing.Assert(test, b, M, id);
    END Assert ;

BEGIN
    Testing.Begin(test, M);

    
    (* Assign *)
    Str.Assign(str, "abcde");
    Assert(S.Length(str^) = 5, __LINE__) ;
    Assert((str^[0] = "a") & (str^[1] = "b") & (str^[2] = "c") &
            (str^[3] = "d") & (str^[4] = "e") & (str^[5] = Char.NUL), __LINE__) ;
    Str.Assign(str, "");
    Assert(S.Length(str^) = 0, __LINE__) ;
    Assert(str[0] = Char.NUL, __LINE__) ;
    
    (* AppendChar *)
    Str.Assign(str, "");

    Str.AppendChar(str, Char.NUL);
    Assert(S.Length(str^) = 0, __LINE__) ;
    Assert((str^[0] = Char.NUL), __LINE__) ;

    Str.AppendChar(str, "a");
    Assert(S.Length(str^) = 1, __LINE__) ;
    Assert((str^[0] = "a") & (str^[1] = Char.NUL), __LINE__) ;

    Str.AppendChar(str, "b");
    Assert(S.Length(str^) = 2, __LINE__) ;
    Assert((str^[0] = "a") & (str^[1] = "b") &  (str^[2] = Char.NUL), __LINE__) ;
    
    (* Append *)
    Str.Assign(str, "");
    Str.Append(str, "abc");
    Assert(S.Length(str^) = 3, __LINE__) ;
    Assert((str^[0] = "a") & (str^[1] = "b") & (str^[2] = "c") & (str^[3] = Char.NUL), __LINE__) ;
    Str.Append(str, "123");
    Assert(S.Length(str^) = 6, __LINE__) ;
    Assert((str^[0] = "a") & (str^[1] = "b") & (str^[2] = "c") &
           (str^[3] = "1") & (str^[4] = "2") & (str^[5] = "3") & (str^[6] = Char.NUL), __LINE__) ;

    (* Extract *)
    Str.Assign(str, "abcde");
    Str.Extract(dst, str^, 0, 3);
    Assert(S.Length(dst^) = 3, __LINE__) ;
    Assert((dst^[0] = "a") & (dst^[1] = "b") & (dst^[2] = "c") & (dst^[3] = Char.NUL), __LINE__) ;
    Str.Extract(dst, str^, 2, 3);
    Assert(S.Length(dst^) = 3, __LINE__) ;
    Assert((dst^[0] = "c") & (dst^[1] = "d") & (dst^[2] = "e") & (dst^[3] = Char.NUL), __LINE__) ;
    Str.Extract(dst, str^, 5, 3);
    Assert(S.Length(dst^) = 0, __LINE__) ;
    Assert(dst^[0] = Char.NUL, __LINE__) ;
    Str.Assign(str, "");
    Str.Extract(dst, str^, 0, 3);
    Assert(S.Length(dst^) = 0, __LINE__) ;
    
    (* Insert *)
    Str.Assign(str, "");
    Str.Insert(str, 'abc', 0);
    Assert(S.Length(str^) = 3, __LINE__) ;
    Assert((str^[0] = "a") & (str^[1] = "b") & (str^[2] = "c") & (str^[3] = Char.NUL), __LINE__) ;

    Str.Assign(str, "abc");
    Str.Insert(str, "", 0);
    Assert(S.Length(str^) = 3, __LINE__) ;
    Assert((str^[0] = "a") & (str^[1] = "b") & (str^[2] = "c") & (str^[3] = Char.NUL), __LINE__) ;

    Str.Assign(str, "abc");
    Str.Insert(str, ' ', 0);
    Assert(S.Length(str^) = 4, __LINE__) ;
    Assert((str^[0] = " ") & (str^[1] = "a") & (str^[2] = "b") & (str^[3] = "c") & (str^[4] = Char.NUL), __LINE__) ;
    
    (* Replace *)
    Str.Assign(str, "abc");
    Str.Replace(str, "b", "d", 0);
    Assert(S.Length(str^) = 3, __LINE__) ;
    Assert((str^[0] = "a") & (str^[1] = "d") & (str^[2] = "c") & (str^[3] = Char.NUL), __LINE__) ;

    Str.Assign(str, "abcd");
    Str.Replace(str, "cd", "ef", 1);
    Assert(S.Length(str^) = 4, __LINE__) ;
    Assert((str^[0] = "a") & (str^[1] = "b") & (str^[2] = "e") & (str^[3] = "f") & (str^[4] = Char.NUL), __LINE__) ;

    Str.Assign(str, "abc");
    Str.Replace(str, "def", "abc", 0);
    Assert(S.Length(str^) = 3, __LINE__) ;
    Assert((str^[0] = "a") & (str^[1] = "b") & (str^[2] = "c") & (str^[3] = Char.NUL), __LINE__) ;
    
    (* String formatting *)
    Str.Assign(str, "");
    Str.FormatString(str, "abc", 0, 0, {});
    Assert(str^ = "abc", __LINE__) ;

    Str.Assign(str, "");
    Str.FormatString(str, "abc", 0, 2, {});
    Assert(str^ = "ab", __LINE__) ;

    Str.Assign(str, "");
    Str.FormatString(str, "abc", 4, 0, {});
    Assert(str^ = "abc ", __LINE__) ;

    Str.Assign(str, "");
    Str.FormatString(str, "abc", 4, 2, {});
    Assert(str^ = "ab  ", __LINE__) ;

    Str.Assign(str, "");
    Str.FormatString(str, "abc", 4, 0, Const.Left);
    Assert(str^ = "abc ", __LINE__) ;

    Str.Assign(str, "");
    Str.FormatString(str, "abc", 4, 0, Const.Right);
    Assert(str^ = " abc", __LINE__) ;

    Str.Assign(str, "");
    Str.FormatString(str, "abc", 5, 0, Const.Center);
    Assert(str^ = " abc ", __LINE__) ;

    Str.Assign(str, "");
    Str.FormatString(str, "abc", 5, 0, Const.Center + Const.Upper);
    Assert(str^ = " ABC ", __LINE__) ;

    Str.Assign(str, "");
    Str.FormatString(str, " abc ", 7, 0, Const.Center + Const.Alt);
    Assert(str^ = "  Abc  ", __LINE__) ;
    
    (* Integer formatting *)
    Str.Assign(str, "");
    Str.FormatInteger(str, -123, 0, {});
    Assert(str^ = "-123", __LINE__) ;

    Str.Assign(str, "");
    Str.FormatInteger(str, -123, 5, {});
    Assert(str^ = " -123", __LINE__) ;

    Str.Assign(str, "");
    Str.FormatInteger(str, -123, 5, Const.Right);
    Assert(str^ = " -123", __LINE__) ;

    Str.Assign(str, "");
    Str.FormatInteger(str, -123, 5, Const.Left);
    Assert(str^ = "-123 ", __LINE__) ;

    Str.Assign(str, "");
    Str.FormatInteger(str, -123, 6, Const.Center);
    Assert(str^ = " -123 ", __LINE__) ;

    Str.Assign(str, "");
    Str.FormatInteger(str, 123, 0, {});
    Assert(str^ = "123", __LINE__) ;

    Str.Assign(str, "");
    Str.FormatInteger(str, 123, 4, {});
    Assert(str^ = " 123", __LINE__) ;

    Str.Assign(str, "");
    Str.FormatInteger(str, 123, 4, Const.Right);
    Assert(str^ = " 123", __LINE__) ;

    Str.Assign(str, "");
    Str.FormatInteger(str, 123, 4, Const.Left);
    Assert(str^ = "123 ", __LINE__) ;

    Str.Assign(str, "");
    Str.FormatInteger(str, 123, 5, Const.Center);
    Assert(str^ = " 123 ", __LINE__) ;

    Str.Assign(str, "");
    Str.FormatInteger(str, 123, 0, Const.Sign);
    Assert(str^ = "+123", __LINE__) ;

    Str.Assign(str, "");
    Str.FormatInteger(str, 123, 0, Const.Spc);
    Assert(str^ = " 123", __LINE__) ;

    Str.Assign(str, "");
    Str.FormatInteger(str, 123, 0, Const.Zero);
    Assert(str^ = "123", __LINE__) ;

    Str.Assign(str, "");
    Str.FormatInteger(str, 123, 4, Const.Zero);
    Assert(str^ = "0123", __LINE__) ;
    
    (* Cardinal formatting *)
    Str.Assign(str, "");
    Str.FormatCardinal(str, 123, 10, 0, {});
    Assert(str^ = "123", __LINE__) ;

    Str.Assign(str, "");
    Str.FormatCardinal(str, 123, 10, 5, {});
    Assert(str^ = "  123", __LINE__) ;

    Str.Assign(str, "");
    Str.FormatCardinal(str, 123, 10, 4, Const.Right);
    Assert(str^ = " 123", __LINE__) ;

    Str.Assign(str, "");
    Str.FormatCardinal(str, 123, 10, 4, Const.Left);
    Assert(str^ = "123 ", __LINE__) ;

    Str.Assign(str, "");
    Str.FormatCardinal(str, 123, 10, 5, Const.Center);
    Assert(str^ = " 123 ", __LINE__) ;
    
    (* Binary *)
    Str.Assign(str, "");
    Str.FormatCardinal(str, 123, 10, 5, Const.Zero);
    Assert(str^ = "00123", __LINE__) ;

    Str.Assign(str, "");
    Str.FormatCardinal(str, 5, 2, 0, {});
    Assert(str^ = "101", __LINE__) ;

    Str.Assign(str, "");
    Str.FormatCardinal(str, 5, 2, 5, {});
    Assert(str^ = "  101", __LINE__) ;

    Str.Assign(str, "");
    Str.FormatCardinal(str, 5, 2, 6, Const.Alt + Const.Zero);
    Assert(str^ = "0b0101", __LINE__) ;

    (* Octal *)
    Str.Assign(str, "");
    Str.FormatCardinal(str, 9, 8, 0, {});
    Assert(str^ = "11", __LINE__) ;

    Str.Assign(str, "");
    Str.FormatCardinal(str, 9, 8, 0, Const.Alt);
    Assert(str^ = "0o11", __LINE__) ;

    (* Hex *)
    Str.Assign(str, "");
    Str.FormatCardinal(str, 15, 16, 0, {});
    Assert(str^ = "f", __LINE__) ;

    Str.Assign(str, "");
    Str.FormatCardinal(str, 15, 16, 0, Const.Upper);
    Assert(str^ = "F", __LINE__) ;

    Str.Assign(str, "");
    Str.FormatCardinal(str, 15, 16, 0, Const.Alt + Const.Upper);
    Assert(str^ = "0XF", __LINE__) ;
    
    (* DateTime *)
    d := DateTime.EncodeDate(2011,11,18);
    Str.Assign(str, "");
    Str.FormatDateTime(str, d, 'Date : %y-%m-%d');
    Assert(str^ = 'Date : 2011-11-18', __LINE__);
    
    d := DateTime.EncodeDate(2011,11,18);
    Str.Assign(str, "");
    Str.FormatDateTime(str, d, '%a %d, %b, %y');
    Assert(str^ = 'Fri 18, Nov, 2011', __LINE__);
    
    d := DateTime.EncodeDateTime(2019,11,15,16,43,20,0);
    (*
    Str.Assign(str, "");
    Str.FormatDateTime(str, d, '%y-%m-%dT%H:%M:%S');
    Assert(str^ = '2019-11-15T16:43:20', __LINE__);
    *)
    
    DISPOSE(str); DISPOSE(dst);
    Testing.End(test);
END Run;

END TestString.