MODULE TestRegex;
IMPORT Testing := O2Testing IN Std;
IN Std IMPORT Re := Regex, S := ArrayOfChar;

TYPE
    TEST = Testing.TEST;

CONST
    M = "TestRegex";

PROCEDURE Run* (VAR test: TEST);
VAR str : ARRAY 10 OF CHAR;

    PROCEDURE Assert(b: BOOLEAN; id: LONGINT) ;
    BEGIN
        Testing.Assert(test, b, M, id);
    END Assert ;

    PROCEDURE T(regex-, str- : ARRAY OF CHAR; compile, result : BOOLEAN; id: LONGINT);
    VAR
        re : Re.Pattern;
        res : LONGINT;
        ret : BOOLEAN;
    BEGIN
        Re.Compile(re, regex, res);
        Assert((res > 0) = compile, id);
        IF (res > 0) & (S.Length(str) > 0) THEN
            ret := re.FullMatch(str);
            Assert(ret = result, id);
        END;
        Re.Dispose(re);
    END T;
BEGIN
    Testing.Begin(test, M);

    T("", "", FALSE, FALSE, __LINE__);
    T("aB", "aB", TRUE, TRUE, __LINE__);
    T("aB", "ab", TRUE, FALSE, __LINE__);
    T("(", "", FALSE, FALSE, __LINE__);
    T("(a)", "a", TRUE, TRUE, __LINE__);
    T("(aB)|(ab)", "aB", TRUE, TRUE, __LINE__);
    T("(aB)|(ab)", "ab", TRUE, TRUE, __LINE__);
    T("(aB)|(ab)", "Ab", TRUE, FALSE, __LINE__);
    T("[a-b", "", FALSE, FALSE, __LINE__);
    T("[b-a]", "", FALSE, FALSE, __LINE__);
    T("[a-b]", "a", TRUE, TRUE, __LINE__);
    T("[a-b]", "b", TRUE, TRUE, __LINE__);
    T("^[a-b]", "c", TRUE, TRUE, __LINE__);
    T("{a-b", "", FALSE, FALSE, __LINE__);
    T("{b-a}", "", FALSE, FALSE, __LINE__);
    T("{a-b}", "abba", TRUE, TRUE, __LINE__);
    T("^{a-b}", "c", TRUE, TRUE, __LINE__);
    T("*", "testing123.", TRUE, TRUE, __LINE__);
    T("T*t", "Test", TRUE, TRUE, __LINE__);
    T("T*t", "Tt", TRUE, TRUE, __LINE__);
    T("T*t.", "Test", TRUE, FALSE, __LINE__);
    str := "?";
    T(str, "a", TRUE, TRUE, __LINE__);
    T(str, "ab", TRUE, FALSE, __LINE__);
    S.AppendChar(str, "?");
    T(str, "a", TRUE, FALSE, __LINE__);
    T(str, "ab", TRUE, TRUE, __LINE__);
    str := "T??t";
    T(str, "Test", TRUE, TRUE, __LINE__);
    str := "^T??t";
    T(str, "Test", TRUE, FALSE, __LINE__);
    T(str, "TesT", TRUE, TRUE, __LINE__);
    str := "\d";
    T(str, "1", TRUE, TRUE, __LINE__);
    T(str, "a", TRUE, FALSE, __LINE__);
    S.Append(str, "\D");
    T(str, "1", TRUE, TRUE, __LINE__);
    T(str, "123", TRUE, TRUE, __LINE__);
    T(str, "a", TRUE, FALSE, __LINE__);
    str := "\w\W";
    T(str, "Test", TRUE, TRUE, __LINE__);
    T("?", "a", TRUE, TRUE, 0);
    T("??", "a", TRUE, FALSE, 0);
    T("??", "ab", TRUE, TRUE, 0);
    T("T??t", "Test", TRUE, TRUE, 0);
    T("^(T??t)", "Test", TRUE, FALSE, 0);
    T("^(T??t)", "TesT", TRUE, TRUE, 0);
    T("(\d\d-\d\d-\d\d\d\d)|(\d\d/\d\d/\d\d\d\d)", "01-01-2023", TRUE, TRUE, 0);
    T("(\d\d-\d\d-\d\d\d\d)|(\d\d/\d\d/\d\d\d\d)", "01/01/2023", TRUE, TRUE, 0);
    T("(\d\d-\d\d-\d\d\d\d)|(\d\d/\d\d/\d\d\d\d)", "01/01/202", TRUE, FALSE, 0);
    T("(\d\d-\d\d-\d\d\d\d)|(\d\d/\d\d/\d\d\d\d)", "01/0A/2023", TRUE, FALSE, 0);
    T("\w\W\s\w\W", "Test 123", TRUE, TRUE, 0);
    T("\w\W\s\w\W", "Test 123 Test", TRUE, FALSE, 0);
    Testing.End(test);
END Run;

END TestRegex.