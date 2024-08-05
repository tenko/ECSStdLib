(**
Module for unit testing.
*)
MODULE O2Testing IN Std;

IMPORT SYSTEM;
IN Std IMPORT Config, ArrayOfChar;

TYPE
    TEST* = 
    RECORD
        title : ARRAY 64 OF CHAR;
        current : ARRAY 64 OF CHAR;
        tests : LONGINT;
        failed : LONGINT;
        local : LONGINT;
        localfailed : LONGINT
    END;

(* Printing procedures only using 'putchar' *)
PROCEDURE ^ Putchar ["putchar"] (character: INTEGER): INTEGER;

PROCEDURE Char(ch: CHAR);
BEGIN IGNORE(Putchar(ORD(ch)))
END Char;

PROCEDURE String(str-: ARRAY OF CHAR);
VAR i: LENGTH;
BEGIN i := 0; WHILE (i < LEN(str)) & (str[i] # 00X) DO Char(str[i]); INC(i) END
END String;

PROCEDURE Integer(value: LENGTH);
VAR i: LENGTH; buffer: ARRAY 20 OF CHAR;
BEGIN
	i := 0; REPEAT buffer[i] := CHR (value MOD 10 + ORD ('0')); value := value DIV 10; INC (i) UNTIL value = 0;
    IF value < 0 THEN Char('-') END;
	WHILE i # 0 DO DEC (i); Char(buffer[i]) END;
END Integer;

PROCEDURE Ln();
BEGIN
    Char(Config.NL[0]);
    IF Config.NL[1] # 00X THEN Char(Config.NL[1]) END;
END Ln;

(* Initialize global testing Record and output host, target and title *)
PROCEDURE Initialize* (VAR test: TEST; title: ARRAY OF CHAR);
BEGIN
    ArrayOfChar.Assign(test.title, title);
    test.tests := 0;
    test.failed := 0;
    String("START "); String(test.title); Ln
END Initialize;

(** Begin local module tests *)
PROCEDURE Begin* (VAR test: TEST; name: ARRAY OF CHAR);
BEGIN
    ArrayOfChar.Assign(test.current, name);
    test.local := 0;
    test.localfailed := 0
END Begin;

(** End local module tests and print out statistics *)
PROCEDURE End* (VAR test: TEST);
BEGIN
    INC(test.tests, test.local);
    INC(test.failed, test.localfailed);
    String("  "); String(test.current);
    String(", tests: "); Integer(test.local);
    String(", failed: "); Integer(test.localfailed);
    Ln
END End;

(** Finalize tests and print out total statistics *)
PROCEDURE Finalize* (VAR test: TEST);
BEGIN
    String("SUMMARY, tests: "); Integer(test.tests);
    String(", failed: "); Integer(test.failed); Ln;
    String("END"); Ln
END Finalize;

(**
Assert procedure.
*)
PROCEDURE Assert* (VAR test: TEST; b: BOOLEAN; file : ARRAY OF CHAR; id: INTEGER) ;
BEGIN
    INC(test.local);
    IF ~b THEN
        String("  "); String(test.current);
        String(" : LINE "); Integer(id);
        String(" : Assert failed"); Ln;
        INC(test.localfailed)
    END
END Assert;

END O2Testing.