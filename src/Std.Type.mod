(** Module with common types. *)
MODULE Type IN Std;

IMPORT Config IN Std;

TYPE
    DATETIME* = HUGEINT;
    STRING* = POINTER TO ARRAY OF CHAR;
    Writer* = RECORD END;

(** Write `CHAR. *)
PROCEDURE (VAR w : Writer) WriteChar*(ch : CHAR);
BEGIN END WriteChar;

(**
Write `ARRAY OF CHAR` value to NULL byte or length of array.
*)
PROCEDURE (VAR w : Writer) WriteString*(str- : ARRAY OF CHAR);
VAR i: LENGTH;
BEGIN
    i := 0;
    WHILE (i < LEN(str)) & (str[i] # 00X) DO
        w.WriteChar(str[i]);
        INC(i);
    END;
END WriteString;

(** Write platforms newline value.*)
PROCEDURE (VAR w : Writer) WriteNL*();
BEGIN
    w.WriteChar(Config.NL[0]);
    IF Config.NL[1] # 00X THEN w.WriteChar(Config.NL[1]) END;
END WriteNL;

END Type.