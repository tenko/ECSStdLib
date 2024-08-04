(* 
ETH Oberon, Copyright 1990-2003 Computer Systems Institute, ETH Zurich, CH-8092 Zurich.
Refer to the license.txt file provided with this distribution. 
Modified by Tenko.
*)
MODULE TestMod;

IMPORT String IN Std, Re := StringRegex IN Std, S := ArrayOfChar IN Std, SYSTEM;

PROCEDURE Assert(b: BOOLEAN; id: LONGINT) ;
BEGIN
    IF ~b THEN TRACE("ASSERTION"); TRACE(id) END;
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
    T("", "", FALSE, FALSE, 0);
    T("aB", "aB", TRUE, TRUE, 0);
    T("aB", "ab", TRUE, FALSE, 0);
    T("(", "", FALSE, FALSE, 0);
    T("(a)", "a", TRUE, TRUE, 0);
    T("(aB)|(ab)", "aB", TRUE, TRUE, 0);
    T("(aB)|(ab)", "ab", TRUE, TRUE, 0);
    T("(aB)|(ab)", "Ab", TRUE, FALSE, 0);
    T("[a-b", "", FALSE, FALSE, 0);
    T("[b-a]", "", FALSE, FALSE, 0);
    T("[a-b]", "a", TRUE, TRUE, 0);
    T("[a-b]", "b", TRUE, TRUE, 0);
    T("^[a-b]", "c", TRUE, TRUE, 0);
    T("{a-b", "", FALSE, FALSE, 0);
    T("{b-a}", "", FALSE, FALSE, 0);
    T("{a-b}", "abba", TRUE, TRUE, 0);
    T("^{a-b}", "c", TRUE, TRUE, 0);
    T("*", "testing123.", TRUE, TRUE, 0);
    T("T*t", "Test", TRUE, TRUE, 0);
    T("T*t", "Tt", TRUE, TRUE, 0);
    T("T*t.", "Test", TRUE, FALSE, 0);
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
END TestMod.