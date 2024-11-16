(**
Module for unit testing.

Reference to tests folder for usage.
*)
MODULE O2Testing IN Std;

IMPORT ArrayOfChar IN Std, Out IN OBL;

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

(* Initialize global testing Record and output host, target and title *)
PROCEDURE Initialize* (VAR test: TEST; title: ARRAY OF CHAR);
BEGIN
    ArrayOfChar.Assign(test.title, title);
    test.tests := 0;
    test.failed := 0;
    Out.String("START "); Out.String(test.title); Out.Ln
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
    Out.String("  "); Out.String(test.current);
    Out.String(", tests: "); Out.Integer(test.local);
    Out.String(", failed: "); Out.Integer(test.localfailed);
    Out.Ln
END End;

(** Finalize tests and print out total statistics *)
PROCEDURE Finalize* (VAR test: TEST);
BEGIN
    Out.String("SUMMARY, tests: "); Out.Integer(test.tests);
    Out.String(", failed: "); Out.Integer(test.failed); Out.Ln;
    Out.String("END"); Out.Ln
END Finalize;

(**
Assert procedure.
*)
PROCEDURE Assert* (VAR test: TEST; b: BOOLEAN; file : ARRAY OF CHAR; id: INTEGER) ;
BEGIN
    INC(test.local);
    IF ~b THEN
        Out.String("  "); Out.String(test.current);
        Out.String(" : LINE "); Out.Integer(id);
        Out.String(" : Assert failed"); Out.Ln;
        INC(test.localfailed)
    END
END Assert;

END O2Testing.