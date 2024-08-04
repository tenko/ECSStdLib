(** Module with platform config. *)
MODULE Config IN Std;

IMPORT Const IN Std;

CONST
    PLATFORM* = Const.SysNone;
    SEP* = "/"; (* Path separator*)

VAR
    NL- : ARRAY 3 OF CHAR;

BEGIN;
    NL[0] := 0AX;
    NL[1] := 00X;
    NL[2] := 00X;
END Config.