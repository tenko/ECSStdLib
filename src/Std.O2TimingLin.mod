(**
Module for timing of procedure execution.

Reference files the misc folder for usage examples.
*)
MODULE O2Timing IN Std;

IMPORT SYSTEM, Linux IN API, Out IN OBL;

(** Setup timer *)
PROCEDURE StartTimer*;
BEGIN END StartTimer;

(** Elapsed time *)
PROCEDURE Elapsed* (): SIGNED64;
VAR res : Linux.Timespec;
BEGIN
    IGNORE(Linux.ClockGetTime(0, SYSTEM.ADR(res)));
    RETURN res.tv_sec * 1000 + res.tv_nsec DIV 1000000;
END Elapsed;

(** Run testproc and report statistics *)
PROCEDURE Timing* (name- : ARRAY OF CHAR; testproc : PROCEDURE; loops : LONGINT; outer : LONGINT);
    VAR
        totaltime, t : SIGNED64;
        hour, min, sec, msec : SIGNED64;
        i : LONGINT;

    PROCEDURE Run(loops : LONGINT; testproc : PROCEDURE): SIGNED64;
        VAR
            starttime, nulltime : SIGNED64;
            i : LONGINT;
    BEGIN
        starttime := Elapsed();
        FOR i := 1 TO loops DO END;
        nulltime := Elapsed() - starttime;

        starttime := Elapsed();
        FOR i := 1 TO loops DO testproc END;
        RETURN Elapsed() - starttime - nulltime;
    END Run;
BEGIN
    totaltime := MAX(SIGNED64);
    FOR i := 0 TO outer - 1 DO
        t := Run(loops, testproc);
        IF t < totaltime THEN totaltime := t END;
    END;
    hour := (totaltime DIV 3600000) MOD 24;
    min := (totaltime DIV 60000) MOD 60;
    sec := (totaltime DIV 1000) MOD 60;
    msec := totaltime MOD 1000;
    Out.String(name); Out.String(" : ");
    Out.Integer(hour); Out.String("h:");
    Out.Integer(min); Out.String("m:");
    Out.Integer(sec); Out.String("s:");
    Out.Integer(msec); Out.String("ms");
    Out.Ln;
END Timing;

END O2Timing.
