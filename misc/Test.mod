MODULE test;

IMPORT SYSTEM, String IN Std, OS IN Std;

VAR
    s : String.STRING;
BEGIN
    TRACE(OS.Args());
    OS.ProgramName(s);
    TRACE(s^);
    OS.Arg(s, 1);
    TRACE(s^);
    OS.Arg(s, 2);
    TRACE(s^);
    OS.Arg(s, 3);
    TRACE(s^);
    OS.Arg(s, 4);
    TRACE(s^);
END test.