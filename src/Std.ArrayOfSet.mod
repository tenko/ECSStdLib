(**
Treat ARRAY OF SET as a large set. Array lengths of must match and array
must be large enough to match the largest set memeber.

ETH Oberon, Copyright 1990-2003 Computer Systems Institute, ETH Zurich, CH-8092 Zurich.
Refer to the license.txt file provided with this distribution.
*)
MODULE ArrayOfSet IN Std;

IMPORT ArrayOfByte IN Std;

CONST SETSIZE* = SIZE(SET) * 8;

(* Clear content of set *)
PROCEDURE Clear*(VAR s: ARRAY OF SET);
BEGIN ArrayOfByte.Zero(s)
END Clear;

(* Fill content of set *)
PROCEDURE Fill*(VAR s: ARRAY OF SET);
BEGIN ArrayOfByte.Fill(s, 0FFX)
END Fill;

(* Copy src to dst *)
PROCEDURE Copy*(VAR dst: ARRAY OF SET; src-: ARRAY OF SET);
BEGIN ArrayOfByte.Copy(dst, src, LEN(dst))
END Copy;

(* Return TRUE if set contains x *)
PROCEDURE In*(s-: ARRAY OF SET; x: LENGTH): BOOLEAN;
BEGIN RETURN x MOD SETSIZE IN s[x DIV SETSIZE]
END In;

(** Test if set is all zeros *)
PROCEDURE IsZero* (VAR src- : ARRAY OF SET): BOOLEAN;
BEGIN RETURN ArrayOfByte.IsZero(src)
END IsZero;

(** Test if `left` and `right` is equal. *)
PROCEDURE Equal* (VAR left-, right- : ARRAY OF SET): BOOLEAN;
BEGIN RETURN ArrayOfByte.Equal(left, right)
END Equal;

(* Return TRUE if all elements in left is found in right *)
PROCEDURE IsSubset*(VAR left-, right- : ARRAY OF SET): BOOLEAN;
VAR i : LENGTH;
BEGIN
    FOR i := 0 TO LEN(left) - 1 DO
        IF left[i] * right[i] # left[i] THEN
            RETURN FALSE
        END
    END;
    RETURN TRUE
END IsSubset;

(* Include x in set *)
PROCEDURE Incl*(VAR s: ARRAY OF SET; x: LENGTH);
BEGIN INCL(s[x DIV SETSIZE], x MOD SETSIZE)
END Incl;

(* Exlude x from set *)
PROCEDURE Excl*(VAR s: ARRAY OF SET; x: LENGTH);
BEGIN EXCL(s[x DIV SETSIZE], x MOD SETSIZE)
END Excl;

(* Invert set *)
PROCEDURE Invert*(VAR dst: ARRAY OF SET);
VAR i : LENGTH;
BEGIN
    FOR i := 0 TO LEN(dst) - 1 DO
        dst[i] := dst[i] / {0..SETSIZE-1}
    END;
END Invert;

(* Set dst to elements both in x and y *)
PROCEDURE Union*(VAR dst: ARRAY OF SET; x-, y-: ARRAY OF SET);
VAR i : LENGTH;
BEGIN
    FOR i := 0 TO LEN(dst) - 1 DO
        dst[i] := x[i] + y[i]
    END;
END Union;

(* Set dst to element in x but not in y *)
PROCEDURE Difference*(VAR dst: ARRAY OF SET; x-, y-: ARRAY OF SET);
VAR i : LENGTH;
BEGIN
    FOR i := 0 TO LEN(dst) - 1 DO
        dst[i] := x[i] - y[i]
    END;
END Difference;

(* Set dst to element common to x and y *)
PROCEDURE Intersection*(VAR dst: ARRAY OF SET; x-, y-: ARRAY OF SET);
VAR i : LENGTH;
BEGIN
    FOR i := 0 TO LEN(dst) - 1 DO
        dst[i] := x[i] * y[i]
    END;
END Intersection;

(* Set dst to elements in either x or y, but not in both *)
PROCEDURE SymmetricDifference*(VAR dst: ARRAY OF SET; x-, y-: ARRAY OF SET);
VAR i : LENGTH;
BEGIN
    FOR i := 0 TO LEN(dst) - 1 DO
        dst[i] := x[i] / y[i]
    END;
END SymmetricDifference;

END ArrayOfSet.