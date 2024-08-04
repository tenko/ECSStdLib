(** Module with platform config. *)
MODULE Config IN Std;

IMPORT Const IN Std;

CONST
    PLATFORM* = Const.SysWindows;
    SEP* = "\"; (* Path separator*)

VAR
    NL- : ARRAY 3 OF CHAR;

BEGIN
    NL[0] := 0DX;
    NL[1] := 0AX;
    NL[2] := 00X;
END Config.