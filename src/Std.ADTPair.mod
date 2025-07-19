(**
A Pair is a templated type which allows to store two types as a single unit.
*)
MODULE ADTPair (First*, Second*) IN Std;

TYPE
    HashProc* = PROCEDURE(first- : First; second- : Second): LENGTH;
    EqualProc* = PROCEDURE(fl-, fr- : First; sl-, sr- : Second): BOOLEAN;
    CompareProc* = PROCEDURE(fl-, fr- : First; sl-, sr- : Second): INTEGER;
    
    Pair* = RECORD-
        first* : First;
        second* : Second;
    END;
    
VAR
    hash* : HashProc;
    equal* : EqualProc;
    compare* : CompareProc;

(** Hash pair *)
PROCEDURE Hash*(pair- : Pair): LENGTH;
BEGIN RETURN hash(pair.first, pair.second)
END Hash;

(** Equal pair *)
PROCEDURE Equal*(left-, right- : Pair): BOOLEAN;
BEGIN RETURN equal(left.first, right.first, left.second, right.second)
END Equal;

(** Compare pair *)
PROCEDURE Compare*(left-, right- : Pair): INTEGER;
BEGIN RETURN compare(left.first, right.first, left.second, right.second)
END Compare;

END ADTPair.