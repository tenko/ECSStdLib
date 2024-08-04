(** Operations on ARRAY OF BYTE. *)
MODULE ArrayOfByte IN Std;

IMPORT SYSTEM;

TYPE
    BYTE = SYSTEM.BYTE;
    WORD = SYSTEM.ADDRESS;
    WSET = SYSTEM.SET;

CONST
    WORDSIZE = SIZE(WORD);
    LOWER = SEL(WORDSIZE = 2, 101H, SEL(WORDSIZE = 4, 1010101H, 101010101010101H));
    UPPER = SEL(WORDSIZE = 2, 1010H, SEL(WORDSIZE = 4, 10101010H, 1010101010101010H));

(** Fill array `dst` with byte `val` *)
PROCEDURE Fill* (VAR dst : ARRAY OF BYTE; val : BYTE);
VAR
    word: WORD;
    adr : SYSTEM.ADDRESS;
    i: LENGTH;
BEGIN
    (* Process block by block *)
    i := 0;
    adr := SYSTEM.ADR(dst[0]);
    word := LOWER * WORD(WSET(val) * WSET(0FH))  + UPPER * WORD(WSET(UNSIGNED8(val) DIV 16) * WSET(0FH));
    LOOP
        IF (i > (LEN(dst) - WORDSIZE)) THEN EXIT END;
        SYSTEM.PUT(adr + i, word);
        INC(i, WORDSIZE)
    END;
    (* Process last partial block *)
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
VAR
    word: WORD;
    sadr, dadr : SYSTEM.ADDRESS;
    i, max: LENGTH;
BEGIN
    i := 0;
    IF cnt > 0 THEN max := cnt;   
    ELSE max := MAX(LENGTH) END;
    IF LEN(dst) < max THEN max := LEN(dst) END;
    IF LEN(src) < max THEN max := LEN(src) END;
    (* Check block by block *)
    sadr := SYSTEM.ADR(src[0]);
    dadr := SYSTEM.ADR(dst[0]);
    LOOP
        IF i > (max - WORDSIZE) THEN EXIT END;
        SYSTEM.GET(sadr + i, word);
        SYSTEM.PUT(dadr + i, word);
        INC(i, WORDSIZE)
    END;
    (* Process last partial block *)
    WHILE i < max DO
        dst[i] := src[i];
        INC(i)
    END;
END Copy;

(** Test if array is all zeros *)
PROCEDURE IsZero* (VAR src- : ARRAY OF BYTE): BOOLEAN;
VAR
    word: WORD;
    adr : SYSTEM.ADDRESS;
    i: LENGTH;
BEGIN
    (* Process block by block *)
    i := 0;
    adr := SYSTEM.ADR(src[0]);
    LOOP
        IF (i > (LEN(src) - WORDSIZE)) THEN EXIT END;
        SYSTEM.GET(adr + i, word);
        IF word # WORD(00X) THEN RETURN FALSE END;
        INC(i, WORDSIZE)
    END;
    (* Process last partial block *)
    WHILE (i < LEN(src)) DO
        IF CHAR(src[i]) # 00X THEN RETURN FALSE END;
        INC(i)
    END;
    RETURN TRUE;
END IsZero;

(** Test if `left` and `right` is equal. *)
PROCEDURE Equal* (VAR left-, right- : ARRAY OF BYTE): BOOLEAN;
VAR
    rword, lword: WORD;
    ladr, radr : SYSTEM.ADDRESS;
    i, len: LENGTH;
BEGIN
    i := 0; len := LEN(left);
    IF LEN(right) # len THEN RETURN FALSE END;
    (* Check block by block *)
    ladr := SYSTEM.ADR(left[0]);
    radr := SYSTEM.ADR(right[0]);
    LOOP
        IF i > (len - WORDSIZE) THEN EXIT END;
        SYSTEM.GET(ladr + i, lword);
        SYSTEM.GET(radr + i, rword);
        IF lword # rword THEN RETURN FALSE END;
        INC(i, WORDSIZE)
    END;
    (* Process last partial block *)
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
    word: WORD;
    adr : SYSTEM.ADDRESS;
    i : LENGTH;
    j : INTEGER;
    hash : LENGTH;
BEGIN
    hash := LENGTH(HSTART);
    i := 0;
    adr := SYSTEM.ADR(src[0]);
    (* Process block by block *)
    LOOP
        IF (i > (LEN(src) - WORDSIZE)) THEN EXIT END;
        SYSTEM.GET(adr + i, word);
        FOR j := 0 TO WORDSIZE - 1 DO
            hash := LENGTH((WSET(word) * WSET(0FFH)) / WSET(hash)); (* XOR *)
            hash := hash * HFACTOR;
            word := SYSTEM.LSH(word, -8);
        END;
        INC(i, WORDSIZE)
    END;
    (* Process last partial block *)
    WHILE i < LEN(src) DO
        hash := LENGTH(WSET(src[i]) / WSET(hash)); (* XOR *)
        hash := hash * LENGTH(HFACTOR);
        INC(i)
    END;
    RETURN hash
END Hash;

END ArrayOfByte.