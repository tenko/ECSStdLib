(**
Set type implementation based on a hash table.

If life time handling of allocated of elements is needed
the callback procedures duplicate and dispose can be used.

By default the set only takes references of elements and
the caller is responsible for keeping elements alive.
*)
MODULE ADTSet (Element*, Hash*, Equal*) IN Std;

IMPORT SYSTEM;
IMPORT ADTVector(Element) IN Std;

TYPE
    WSET = SYSTEM.SET;
    DuplicateElementProc = PROCEDURE(VAR dst: Element; src-: Element);
    DisposeElementProc = PROCEDURE(VAR dst: Element);
    ElementVector* = ADTVector.Vector;
    Entry = POINTER TO EntryDesc;
    EntryDesc = RECORD-
        element : Element;
        deleted : BOOLEAN;
    END;
    Storage = POINTER TO ARRAY OF Entry;
    Set* = RECORD-
        storage : Storage;
        capacity, size : LENGTH;
        duplicate* : DuplicateElementProc;
        dispose* : DisposeElementProc;
    END;
    Iterator* = RECORD-
        storage : Storage;
        duplicate* : DuplicateElementProc;
        index : LENGTH;
    END;

PROCEDURE DefaultDuplicateElement* (VAR dst: Element; src-: Element);
BEGIN dst := src
END DefaultDuplicateElement;

PROCEDURE DefaultDisposeElement* (VAR dst: Element);
BEGIN END DefaultDisposeElement;

PROCEDURE NewStorage(VAR storage: Storage; VAR capacity : LENGTH);
VAR i : LENGTH;
    (* http://graphics.stanford.edu/~seander/bithacks.html#RoundUpPowerOf2Float *)
    PROCEDURE RoundUpPow2(x : LENGTH): LENGTH;
    BEGIN
        x := x - 1;
        x := LENGTH(WSET(x) + WSET(SYSTEM.LSH(x, -1)));
        x := LENGTH(WSET(x) + WSET(SYSTEM.LSH(x, -2)));
        x := LENGTH(WSET(x) + WSET(SYSTEM.LSH(x, -4)));
        x := LENGTH(WSET(x) + WSET(SYSTEM.LSH(x, -8)));
        x := LENGTH(WSET(x) + WSET(SYSTEM.LSH(x, -8)));
        RETURN x + 1
    END RoundUpPow2;
BEGIN
    IF capacity < 2 THEN capacity := 2 END;
    capacity := RoundUpPow2(capacity);
    NEW(storage, capacity);
    IF storage = NIL THEN capacity := 0 END;
    FOR i := 0 TO capacity - 1 DO storage[i] := NIL END;
END NewStorage;

(**
Initialize set storage to given capacity.
Capacity will be rounded up to nearest exponent of 2 size. 
*)
PROCEDURE (VAR this : Set) Init*(capacity : LENGTH);
BEGIN
    NewStorage(this.storage, capacity);
    this.duplicate := DefaultDuplicateElement;
    this.dispose := DefaultDisposeElement;
    this.capacity := capacity;
    this.size := 0;
END Init;

(** Free set storage *)
PROCEDURE (VAR this : Set) Dispose*();
VAR
    current : Entry;
    i : LENGTH;
BEGIN
    IF this.storage = NIL THEN RETURN END;
    FOR i := 0 TO this.capacity - 1 DO
        current := this.storage[i];
        IF current # NIL THEN
            this.dispose(current.element);
            DISPOSE(current);
        END
    END; 
    DISPOSE(this.storage);
    this.storage := NIL;
    this.size := 0;
    this.capacity := 0
END Dispose;

(** Resize set storage. Called automatic *)
PROCEDURE (VAR this : Set) Resize(capacity : LENGTH): BOOLEAN;
VAR
    storage : Storage;
    current : Entry;
    i, len : LENGTH;

    PROCEDURE Add(VAR entry : Entry);
    VAR
        current : Entry;
        index, hash : LENGTH;
    BEGIN
        (* simplified as no duplicates or deleted entries expected *)
        hash := Hash(entry.element);
        index := LENGTH(WSET(hash) * WSET(this.capacity - 1));
        LOOP
            current := this.storage[index];
            IF current = NIL THEN EXIT END;
            index := LENGTH(WSET(index + 1) * WSET(this.capacity - 1));
        END;
        this.storage[index] := entry;
        INC(this.size);
    END Add;
BEGIN
    storage := this.storage;
    len := this.capacity;
    NewStorage(this.storage, capacity);
    IF capacity = 0 THEN
        this.storage := storage;
        RETURN FALSE
    END;
    this.size := 0;
    this.capacity := capacity;
    FOR i := 0 TO len - 1 DO
        current := storage[i];
        IF current # NIL THEN
            IF ~current.deleted THEN
                Add(current);
            ELSE
                this.dispose(current.element);
                DISPOSE(current)
            END
        END
    END;
    DISPOSE(storage);
    RETURN TRUE;
END Resize;

(** Return size of set *)
PROCEDURE (VAR this- : Set) Size*(): LENGTH;
VAR 
    i, ret : LENGTH;
    entry : Entry;
BEGIN
    ret := 0;
    FOR i := 0 TO this.capacity - 1 DO
        entry := this.storage[i];
        IF (entry # NIL) & ~entry.deleted THEN INC(ret) END
    END;
    RETURN ret
END Size;

(** Return TRUE if set has given element *)
PROCEDURE (VAR this- : Set) In*(element- : Element): BOOLEAN;
VAR
    current : Entry;
    index, hash : LENGTH;
BEGIN
    hash := Hash(element);
    index := LENGTH(WSET(hash) * WSET(this.capacity - 1));
    LOOP
        current := this.storage[index];
        IF current = NIL THEN EXIT
        ELSIF ~current.deleted THEN
            IF Equal(current.element, element) THEN RETURN TRUE END;
        END;
        index := LENGTH(WSET(index + 1) * WSET(this.capacity - 1));
    END;
    RETURN FALSE;
END In;

(** Add element to set *)
PROCEDURE (VAR this : Set) Incl* (element- : Element);
CONST SENTINEL = MIN(LENGTH);
VAR
    current : Entry;
    index, deleted, hash : LENGTH;
BEGIN
    (* Expand size if load factor is >= 0.75 *)
    IF this.size >= (this.capacity DIV 2 + this.capacity DIV 4) THEN
        IGNORE(this.Resize(this.capacity * 2))
    END;
    deleted := SENTINEL;
    hash := Hash(element);
    index := LENGTH(WSET(hash) * WSET(this.capacity - 1));
    LOOP
        current := this.storage[index];
        IF current = NIL THEN EXIT
        ELSIF current.deleted & (deleted = SENTINEL) THEN
            deleted := index
        ELSIF Equal(current.element, element) THEN
            RETURN
        END;
        index := LENGTH(WSET(index + 1) * WSET(this.capacity - 1));
    END;
    IF deleted = SENTINEL THEN NEW(this.storage[index])
    ELSE index := deleted END;
    current := this.storage[index];
    IF current = NIL THEN RETURN END;
    current.deleted := FALSE;
    this.duplicate(current.element, element);
    INC(this.size)
END Incl;

(** Remove element from set. *)
PROCEDURE (VAR this : Set) Excl*(element- : Element);
VAR
    current, tmp : Entry;
    i, j, k, hash : LENGTH;
BEGIN
    hash := Hash(element);
    i := LENGTH(WSET(hash) * WSET(this.capacity - 1));
    LOOP
        current := this.storage[i];
        IF (current = NIL) THEN RETURN END;
        IF ~current.deleted THEN
            IF Equal(current.element, element) THEN EXIT END;
        END;
        i := LENGTH(WSET(i + 1) * WSET(this.capacity - 1));
    END;
    j := i;
    LOOP
        (* Possible shift entries back to earlier position if we have a hash collision *)
        j := LENGTH(WSET(j + 1) * WSET(this.capacity - 1));
        current := this.storage[j];
        IF current = NIL THEN EXIT END;
        hash := Hash(current.element);
        k := LENGTH(WSET(hash) * WSET(this.capacity - 1));
        IF ((j > i) & ((k <= i) OR (k > j))) OR ((j < i) & ((k <= i) & (k > j))) THEN
            tmp := this.storage[i];
            this.storage[i] := this.storage[j];
            this.storage[j] := tmp;
            i := j;
        END;
    END;
    DEC(this.size);
    IF this.size < 0 THEN this.size := 0 END;
    this.storage[i].deleted := TRUE;
    IF this.size <= this.capacity DIV 10 THEN (* Resize if loadfactor is 0.1 or less *)
        IF ~this.Resize(this.capacity DIV 2) THEN RETURN END;
    END;
END Excl;

(** Clear set all elements without deallocation *)
PROCEDURE (VAR this : Set) Clear*();
VAR i : LENGTH;
BEGIN
    IF this.storage = NIL THEN RETURN END;
    FOR i := 0 TO this.capacity - 1 DO
        IF this.storage[i] # NIL THEN
            this.storage[i].deleted := TRUE
        END
    END;
    this.size := 0
END Clear;

(**
Remove arbitary element from set. Return FALSE if set is empty.
Note this may be duplicate the element and the caller would be
responsible for the lifetime of the element.
*)
PROCEDURE (VAR this : Set) Pop*(VAR element : Element): BOOLEAN;
VAR 
    i : LENGTH;
    entry : Entry;
BEGIN
    FOR i := 0 TO this.capacity - 1 DO
        entry := this.storage[i];
        IF (entry # NIL) & ~entry.deleted THEN
            this.duplicate(element, entry.element);
            DEC(this.size);
            IF this.size < 0 THEN this.size := 0 END;
            entry.deleted := TRUE;
            RETURN TRUE;
        END
    END;
    RETURN FALSE
END Pop;

(**
Return Vector of elements.
Note this may be duplicate the elements and the caller would be
responsible for the lifetime of the elements.
*)
PROCEDURE (VAR this- : Set) Elements*(): ElementVector;
VAR 
    i : LENGTH;
    entry : Entry;
    element : Element;
    ret : ElementVector;
BEGIN
    ret.Init(this.size);
    ret.dispose := this.dispose;
    ret.duplicate := this.duplicate;
    FOR i := 0 TO this.capacity - 1 DO
        entry := this.storage[i];
        IF (entry # NIL) & ~entry.deleted THEN
            this.duplicate(element, entry.element);
            ret.Append(element)
        END
    END;
    RETURN ret
END Elements;

(**
Return Vector of reference to elements.
*)
PROCEDURE (VAR this- : Set) ElementsRef*(): ElementVector;
VAR 
    i : LENGTH;
    entry : Entry;
    element : Element;
    ret : ElementVector;
BEGIN
    ret.Init(this.size);
    ret.dispose := DefaultDisposeElement;
    ret.duplicate := DefaultDuplicateElement;
    FOR i := 0 TO this.capacity - 1 DO
        entry := this.storage[i];
        IF (entry # NIL) & ~entry.deleted THEN
            ret.Append(entry.element)
        END
    END;
    RETURN ret
END ElementsRef;

(** Returns an iterator for the set. *)
PROCEDURE (VAR this- : Set) First* (VAR iterator: Iterator);
BEGIN
    iterator.storage := this.storage;
    iterator.duplicate := this.duplicate;
    iterator.index := 0;
END First;

(**
Advance iterator. Return `FALSE` if end is reached.
Note this may be duplicate the element and the caller would be
responsible for the lifetime of the element.
*)
PROCEDURE (VAR this : Iterator) Next*(VAR element : Element) : BOOLEAN;
VAR entry : Entry;
BEGIN
    WHILE this.index < LEN(this.storage^) DO
        entry := this.storage[this.index];
        INC(this.index);
        IF entry # NIL THEN
            IF ~entry.deleted THEN
                this.duplicate(element, entry.element);
                RETURN TRUE
            END
        END
    END;
    RETURN FALSE
END Next;

(** Reset iterator to start of set. *)
PROCEDURE (VAR this : Iterator) Reset*();
BEGIN this.index := 0 END Reset;

END ADTSet.