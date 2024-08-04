(**
Module for timing of procedure execution.

Reference files the perf directory for usage examples.
*)
MODULE O2Timing IN Std;

IMPORT Windows IN API, Out IN OBL;

VAR
    PCFreq : SIGNED64;
    CounterStart : SIGNED64;

(** Setup timer *)
PROCEDURE StartTimer*;
VAR res : Windows.LARGE_INTEGER;
BEGIN
    IF Windows.QueryPerformanceFrequency(res) = 0 THEN HALT(1) END;
    PCFreq := res;
    IGNORE(Windows.QueryPerformanceCounter(res));
	CounterStart := res;
END StartTimer;

(** Elapsed time *)
PROCEDURE Elapsed* (): SIGNED64;
VAR res : Windows.LARGE_INTEGER;
BEGIN
    IGNORE(Windows.QueryPerformanceCounter(res));
	RETURN (1000*(res - CounterStart)) DIV PCFreq;
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