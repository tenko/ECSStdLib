(**
Module for timing of procedure execution.

Reference files the misc folder for usage examples.
*)
MODULE O2Timing IN Std;

IMPORT SYSTEM;

(** Setup timer *)
PROCEDURE ^ HostStartTimer ["HostStartTimer"];
PROCEDURE StartTimer*;
BEGIN HostStartTimer END StartTimer;

(** Elapsed time *)
PROCEDURE ^ HostElapsed ["HostElapsed"](): SIGNED64;
PROCEDURE Elapsed* (): SIGNED64;
BEGIN RETURN HostElapsed()
END Elapsed;

(** Run testproc and report statistics *)
PROCEDURE ^ HostTiming ["HostTiming"];
PROCEDURE Timing* (name- : ARRAY OF CHAR; testproc : PROCEDURE; loops : LONGINT; outer : LONGINT);
BEGIN HostTiming
END Timing;

END O2Timing.
