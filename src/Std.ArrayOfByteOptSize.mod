(** Operations on ARRAY OF BYTE. *)
MODULE ArrayOfByte IN Std;

IMPORT SYSTEM;

TYPE
    BYTE = SYSTEM.BYTE;
    WORD = SYSTEM.ADDRESS;
    WSET = SYSTEM.SET;

CONST
    WORDSIZE = SIZE(WORD);

(** Fill array `dst` with byte `val` *)
PROCEDURE Fill* (VAR dst : ARRAY OF BYTE; val : BYTE);
VAR i: LENGTH;
BEGIN
    i := 0;
    WHILE (i < LEN(dst)) DO
        dst[i] := val;
        INC(i)
    END
END Fill;

(** Fill array with zeros *)
PROCEDURE Zero* (VAR dst : ARRAY OF BYTE);
BEGIN Fill(dst, 0)
END Zero;

(** Copy `src` to `dst` *)
PROCEDURE Copy* (VAR dst : ARRAY OF BYTE; VAR src- : ARRAY OF BYTE; cnt : LENGTH);
VAR i, max: LENGTH;
BEGIN
    i := 0;
    IF cnt > 0 THEN max := cnt;   
    ELSE max := MAX(LENGTH) END;
    IF LEN(dst) < max THEN max := LEN(dst) END;
    IF LEN(src) < max THEN max := LEN(src) END;
    WHILE i < max DO
        dst[i] := src[i];
        INC(i)
    END;
END Copy;

(** Test if array is all zeros *)
PROCEDURE IsZero* (VAR src- : ARRAY OF BYTE): BOOLEAN;
VAR i: LENGTH;
BEGIN
    i := 0;
    WHILE (i < LEN(src)) DO
        IF CHAR(src[i]) # 00X THEN RETURN FALSE END;
        INC(i)
    END;
    RETURN TRUE;
END IsZero;

(** Test if `left` and `right` is equal. *)
PROCEDURE Equal* (VAR left-, right- : ARRAY OF BYTE): BOOLEAN;
VAR i, len: LENGTH;
BEGIN
    i := 0; len := LEN(left);
    IF LEN(right) # len THEN RETURN FALSE END;
    WHILE i < len DO
        IF left[i] # right[i] THEN RETURN FALSE END;
        INC(i)
    END;
    RETURN TRUE
END Equal;

(**  Hash value of array (64/32bit FNV-1a) *)
PROCEDURE Hash* (VAR src- : ARRAY OF BYTE): LENGTH;
CONST
    HSTART = SEL(WORDSIZE = 4, 0811C9DC5H, 0CBF29CE484222325H);
    HFACTOR = SEL(WORDSIZE = 4, 01000193H, 0100000001B3H);
VAR
    i : LENGTH;
    hash : LENGTH;
BEGIN
    hash := LENGTH(HSTART);
    i := 0;
    WHILE i < LEN(src) DO
        hash := LENGTH(WSET(src[i]) / WSET(hash)); (* XOR *)
        hash := hash * LENGTH(HFACTOR);
        INC(i)
    END;
    RETURN hash
END Hash;

END ArrayOfByte.