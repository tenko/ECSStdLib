(**
Common procedures for basic types (Integer, Cardinal, Set, Real)
*)
MODULE ADTBasicType (Type*) IN Std;

IMPORT SYSTEM;

TYPE
    WORD = SYSTEM.ADDRESS;
    WSET = SYSTEM.SET;

CONST
    WORDSIZE = SIZE(WORD);
    SHIFT1 = SEL(WORDSIZE = 8, 30, 16);
    SHIFT2 = SEL(WORDSIZE = 8, 27, 13);
    SHIFT3 = SEL(WORDSIZE = 8, 31, 16);
    C1 = SEL(WORDSIZE = 8, 0BF58476D1CE4E5B9H, 085EBCA6BH);
    C2 = SEL(WORDSIZE = 8, 094D049BB133111EBH, 0C2B2AE35H);

(** Return smallest of x & y *)
PROCEDURE Min* (x, y : Type) : Type;
BEGIN
    IF x < y THEN RETURN x;
    ELSE RETURN y END;
END Min;

(** Return largest of x & y *)
PROCEDURE Max* (x, y : Type) : Type;
BEGIN
    IF x > y THEN RETURN x;
    ELSE RETURN y END;
END Max;

(** Swap x & y *)
PROCEDURE Swap* (VAR x, y : Type);
VAR tmp : Type;
BEGIN tmp := x; x := y; y := tmp
END Swap;

(**
Compare `left` and `right`.

* 0 if left = right
* -1 if left < right
* +1 if left > right
*)
PROCEDURE Compare* (left-, right-: Type): INTEGER;
BEGIN
    IF left < right THEN RETURN -1 END;
    IF left > right THEN RETURN 1 END;
    RETURN 0
END Compare;

(** Test if `left` and `right` is equal. *)
PROCEDURE Equal* (left-, right-: Type): BOOLEAN;
BEGIN RETURN left = right
END Equal;

(**  Hash value (splitmix32/64) *)
PROCEDURE Hash* (src- : Type): LENGTH;
VAR x : WORD;
BEGIN
    x := WORD(src);
    x := WORD(WSET(x) / WSET(SYSTEM.LSH(x, -SHIFT1))) * WORD(C1);
    x := WORD(WSET(x) / WSET(SYSTEM.LSH(x, -SHIFT2))) * WORD(C2);
    RETURN LENGTH(WSET(x) / WSET(SYSTEM.LSH(x, -SHIFT3)))
END Hash;

END ADTBasicType.