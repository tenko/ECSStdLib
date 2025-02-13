(* Tests are ported from Lua testing suite *)
MODULE TestStringPattern;
IMPORT Testing := O2Testing IN Std;
IN Std IMPORT ArrayOfChar, StringPattern;

TYPE
    TEST = Testing.TEST;

CONST
    M = "TestStringPattern";

PROCEDURE Run* (VAR test: TEST);
VAR
    str : ARRAY 64 OF CHAR;
	pat : StringPattern.Pattern;

    PROCEDURE Assert(b: BOOLEAN; id: LONGINT) ;
    BEGIN
        Testing.Assert(test, b, M, id);
    END Assert ;

    PROCEDURE Equal(check- : ARRAY OF CHAR; index : INTEGER): BOOLEAN;
    VAR start, len, i : LENGTH;
    BEGIN
        IF ~pat.Capture(index, start, len) THEN RETURN FALSE END;
        IF ArrayOfChar.Length(check) # len THEN RETURN FALSE END;
        i := 0;
        WHILE len > 0 DO
            IF check[i] # str[start] THEN RETURN FALSE END;
            INC(start);
            DEC(len);
            INC(i);
        END;
        RETURN TRUE
    END Equal;
BEGIN
    Testing.Begin(test, M);

    str := '';
	Assert(pat.Match("", str) = TRUE, __LINE__);
	Assert(pat.Find("", str, 0) = 0, __LINE__);
    
    str := 'alo';
    Assert(pat.Find("", str, 0) = 0, __LINE__);

    str := 'ao ao ao';
    Assert(pat.Find("c", str, 0) = -1, __LINE__);
    Assert(pat.Find("a", str, 0) = 0, __LINE__);
    Assert(pat.Find("ao", str, 1) = 3, __LINE__);
    Assert(pat.Find("ao", str, 6) = 6, __LINE__);
    
    str := 'ao ao ab';
    Assert(pat.Find("ab", str, 0) = 6, __LINE__);
    Assert(pat.Find("b", str, 0) = 7, __LINE__);

    str := 'alo123alo';
    Assert(pat.Find("12", str, 0) = 3, __LINE__);
    Assert(pat.Find("^12", str, 0) = -1, __LINE__);

    str := "aaab";
    Assert(pat.Match(".*b", str), __LINE__);
    
    Assert(Equal("aaab", 0), __LINE__);
    
    str := "aaa";
    Assert(pat.Match(".*a", str), __LINE__);
    Assert(Equal("aaa", 0), __LINE__);
    
    str := "b";
    Assert(pat.Match(".*b", str), __LINE__);
    Assert(Equal("b", 0), __LINE__);
    
    str := "aaa";
    Assert(pat.Match(".+a", str), __LINE__);
    Assert(Equal("aaa", 0), __LINE__);
    
    str := "b";
    Assert(~pat.Match(".+b", str), __LINE__);
    
    str := "aaab";
    Assert(pat.Match(".?b", str), __LINE__);
    Assert(Equal("ab", 0), __LINE__);

    str := "aaa";
    Assert(pat.Match(".?a", str), __LINE__);
    Assert(Equal("aa", 0), __LINE__);
    
    str := "b";
    Assert(pat.Match(".?b", str), __LINE__);
    Assert(Equal("b", 0), __LINE__);
    
    str := "aloALO";
    Assert(pat.Match("%l*", str), __LINE__);
    Assert(Equal("alo", 0), __LINE__);
    
    str := "aLo_ALO";
    Assert(pat.Match("%a*", str), __LINE__);
    Assert(Equal("aLo", 0), __LINE__);
    
    str := "aaab";
    Assert(pat.Match("a*", str), __LINE__);
    Assert(Equal("aaa", 0), __LINE__);
    
    str := "aaa";
    Assert(pat.Match("^.*$", str), __LINE__);
    Assert(Equal("aaa", 0), __LINE__);
    
    str := "aaa";
    Assert(pat.Match("b*", str), __LINE__);
    Assert(Equal("", 0), __LINE__);

    str := "aaa";
    Assert(pat.Match("ab*a", str), __LINE__);
    Assert(Equal("aa", 0), __LINE__);
    
    str := "aba";
    Assert(pat.Match("ab*a", str), __LINE__);
    Assert(Equal("aba", 0), __LINE__);

    str := "aaab";
    Assert(pat.Match("a+", str), __LINE__);
    Assert(Equal("aaa", 0), __LINE__);

    str := "aaa";
    Assert(pat.Match("^.+$", str), __LINE__);
    Assert(Equal("aaa", 0), __LINE__);

    str := "aaa";
    Assert(~pat.Match("b+", str), __LINE__);
    
    str := "aaa";
    Assert(~pat.Match("ab+a", str), __LINE__);

    str := "aba";
    Assert(pat.Match("ab+a", str), __LINE__);
    Assert(Equal("aba", 0), __LINE__);
    
    str := "a$a";
    Assert(pat.Match(".$", str), __LINE__);
    Assert(Equal("a", 0), __LINE__);
    
    str := "a$a";
    Assert(pat.Match(".%$", str), __LINE__);
    Assert(Equal("a$", 0), __LINE__);
    
    str := "a$a";
    Assert(pat.Match(".$.", str), __LINE__);
    Assert(Equal("a$a", 0), __LINE__);
    
    str := "a$a";
    Assert(~pat.Match("$$", str), __LINE__);
    
    str := "a$b";
    Assert(~pat.Match("a$", str), __LINE__);
    
    str := "a$a";
    Assert(pat.Match("$", str), __LINE__);
    Assert(Equal("", 0), __LINE__);

    str := "aaa";
    Assert(~pat.Match("bb*", str), __LINE__);

    str := "aaab";
    Assert(pat.Match("a-", str), __LINE__);
    Assert(Equal("", 0), __LINE__);
    
    str := "aaa";
    Assert(pat.Match("^.-$", str), __LINE__);
    Assert(Equal("aaa", 0), __LINE__);

    str := "aabaaabaaabaaaba";
    Assert(pat.Match("b.*b", str), __LINE__);
    Assert(Equal("baaabaaabaaab", 0), __LINE__);

    str := "aabaaabaaabaaaba";
    Assert(pat.Match("b.-b", str), __LINE__);
    Assert(Equal("baaab", 0), __LINE__);

    str := "alo xo";
    Assert(pat.Match(".o$", str), __LINE__);
    Assert(Equal("xo", 0), __LINE__);

    str := "alo xyzK";
    Assert(pat.Match("(%w+)K", str), __LINE__);
    Assert(Equal("xyz", 1), __LINE__);

    str := "254 K";
    Assert(pat.Match("(%d*)K", str), __LINE__);
    Assert(Equal("", 1), __LINE__);

    str := "alo ";
    Assert(pat.Match("(%w*)$", str), __LINE__);
    Assert(Equal("", 1), __LINE__);

    str := "alo ";
    Assert(~pat.Match("(%w+)$", str), __LINE__);

    str := "(9 ((8))(\0) 7) \0\0 a b ()(c)() a";
    Assert(pat.Match("%b()", str), __LINE__);
    Assert(Equal("(9 ((8))(\0) 7)", 0), __LINE__);
    
    str := "alo 'oi' alo";
    Assert(pat.Match("%b''", str), __LINE__);
    Assert(Equal("'oi'", 0), __LINE__);

    str := " alo aalo allo";
    Assert(pat.Match("%f[%S](.-%f[%s].-%f[%S])", str), __LINE__);
    Assert(Equal("alo ", 0), __LINE__);

    str := "a";
    Assert(~pat.Match("(.", str), __LINE__);
    Assert(pat.error = StringPattern.ERROR_INVALID_PATTERN_CAPTURE, __LINE__);
    Assert(~pat.Match(".)", str), __LINE__);
    Assert(pat.error = StringPattern.ERROR_INVALID_PATTERN_CAPTURE, __LINE__);
    Assert(~pat.Match("[a", str), __LINE__);
    Assert(pat.error = StringPattern.ERROR_MALFORMED_PATTERN, __LINE__);
    Assert(~pat.Match("[]", str), __LINE__);
    Assert(pat.error = StringPattern.ERROR_MALFORMED_PATTERN, __LINE__);
    Assert(~pat.Match("[^]", str), __LINE__);
    Assert(pat.error = StringPattern.ERROR_MALFORMED_PATTERN, __LINE__);
    Assert(~pat.Match("[a%]", str), __LINE__);
    Assert(pat.error = StringPattern.ERROR_MALFORMED_PATTERN, __LINE__);
    Assert(~pat.Match("[a%", str), __LINE__);
    Assert(pat.error = StringPattern.ERROR_MALFORMED_PATTERN, __LINE__);
    Assert(~pat.Match("%b", str), __LINE__);
    Assert(pat.error = StringPattern.ERROR_MALFORMED_PATTERN, __LINE__);
    Assert(~pat.Match("%ba", str), __LINE__);
    Assert(pat.error = StringPattern.ERROR_MALFORMED_PATTERN, __LINE__);
    Assert(~pat.Match("%", str), __LINE__);
    Assert(pat.error = StringPattern.ERROR_MALFORMED_PATTERN, __LINE__);
    Assert(~pat.Match("%f", str), __LINE__);
    Assert(pat.error = StringPattern.ERROR_MALFORMED_PATTERN, __LINE__);
    
    Testing.End(test);
END Run;

END TestStringPattern.