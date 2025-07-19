(**
Dictionary which implementes an hash table.

If life time handling of allocated keys and values is needed
the callback procedures duplicateKey/Value and disposeKey/Value can be used.

By default the dictionary only takes references of keys & elements and
the caller is responsible for keeping them alive.
*)
MODULE ADTDictionary (Key*, Value*, Hash*, Equal*) IN Std;

IMPORT SYSTEM;
IMPORT ADTKeyVector := ADTVector(Key) IN Std;
IMPORT ADTValueVector := ADTVector(Value) IN Std;

TYPE
    WSET = SYSTEM.SET;
    
    DuplicateKeyProc* = PROCEDURE(VAR dst: Key; src-: Key);
    DisposeKeyProc* = PROCEDURE(VAR dst: Key);
    DuplicateValueProc* = PROCEDURE(VAR dst: Value; src-: Value);
    DisposeValueProc* = PROCEDURE(VAR dst: Value);
    
    ValueVector* = ADTValueVector.Vector;
    KeyVector* = ADTKeyVector.Vector;
    Entry = POINTER TO EntryDesc;
    EntryDesc = RECORD-
        key : Key;
        value : Value;
        deleted : BOOLEAN;
    END;
    Storage = POINTER TO ARRAY OF Entry;
    Dictionary* = RECORD-
        storage : Storage;
        duplicateKey* : DuplicateKeyProc;
        disposeKey* : DisposeKeyProc;
        duplicateValue* : DuplicateValueProc;
        disposeValue* : DisposeValueProc;
        capacity, size : LENGTH;
    END;
    Iterator* = RECORD-
        storage : Storage;
        index : LENGTH;
    END;

(** defaults to assignment *)
PROCEDURE DefaultDuplicateKey* (VAR dst: Key; src-: Key);
BEGIN dst := src
END DefaultDuplicateKey;

(** defaults to no operation *)
PROCEDURE DefaultDisposeKey* (VAR dst: Key);
BEGIN END DefaultDisposeKey;

(** defaults to assignment *)
PROCEDURE DefaultDuplicateValue* (VAR dst: Value; src-: Value);
BEGIN dst := src
END DefaultDuplicateValue;

(** defaults to no operation *)
PROCEDURE DefaultDisposeValue* (VAR dst: Value);
BEGIN END DefaultDisposeValue;

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
PROCEDURE (VAR this : Dictionary) Init*(capacity : LENGTH);
BEGIN
    NewStorage(this.storage, capacity);
    this.duplicateKey := DefaultDuplicateKey;
    this.disposeKey := DefaultDisposeKey;
    this.duplicateValue := DefaultDuplicateValue;
    this.disposeValue := DefaultDisposeValue;
    this.capacity := capacity;
    this.size := 0;
END Init;

(** Free dictionary storage *)
PROCEDURE (VAR this : Dictionary) Dispose*();
VAR
    current : Entry;
    i : LENGTH;
BEGIN
    IF this.storage = NIL THEN RETURN END;
    FOR i := 0 TO this.capacity - 1 DO
        current := this.storage[i];
        IF current # NIL THEN
            this.disposeKey(current.key);
            this.disposeValue(current.value);
            DISPOSE(current);
        END
    END; 
    DISPOSE(this.storage);
    this.storage := NIL;
    this.size := 0;
    this.capacity := 0
END Dispose;

(** Resize dictionary storage. Called automatic *)
PROCEDURE (VAR this : Dictionary) Resize(capacity : LENGTH): BOOLEAN;
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
        hash := Hash(entry.key);
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
                this.disposeKey(current.key);
                this.disposeValue(current.value);
                DISPOSE(current)
            END
        END
    END;
    DISPOSE(storage);
    RETURN TRUE;
END Resize;

(** Return size of dictionary *)
PROCEDURE (VAR this- : Dictionary) Size*(): LENGTH;
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

(** Return TRUE if dictionary has item with given key *)
PROCEDURE (VAR this- : Dictionary) HasKey*(key- : Key): BOOLEAN;
VAR
    current : Entry;
    index, hash : LENGTH;
BEGIN
    hash := Hash(key);
    index := LENGTH(WSET(hash) * WSET(this.capacity - 1));
    LOOP
        current := this.storage[index];
        IF current = NIL THEN EXIT
        ELSIF ~current.deleted THEN
            IF Equal(current.key, key) THEN RETURN TRUE END;
        END;
        index := LENGTH(WSET(index + 1) * WSET(this.capacity - 1));
    END;
    RETURN FALSE;
END HasKey;

(** Set or update item with given key to value *)
PROCEDURE (VAR this : Dictionary) Set* (key- : Key; value- : Value);
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
    hash := Hash(key);
    index := LENGTH(WSET(hash) * WSET(this.capacity - 1));
    LOOP
        current := this.storage[index];
        IF current = NIL THEN EXIT
        ELSIF current.deleted & (deleted = SENTINEL) THEN
            deleted := index
        ELSIF Equal(current.key, key) THEN
            this.duplicateValue(current.value, value);
            RETURN
        END;
        index := LENGTH(WSET(index + 1) * WSET(this.capacity - 1));
    END;
    IF deleted = SENTINEL THEN NEW(this.storage[index])
    ELSE index := deleted END;
    current := this.storage[index];
    IF current = NIL THEN RETURN END;
    current.deleted := FALSE;
    this.duplicateKey(current.key, key);
    this.duplicateValue(current.value, value);
    INC(this.size)
END Set;

(**
Get value and return TRUE if dictionary has item with given key.
Note: this potentially return a reference to the value. 
*)
PROCEDURE (VAR this- : Dictionary) Get*(key- : Key; VAR value : Value): BOOLEAN;
VAR
    current : Entry;
    index, hash : LENGTH;
BEGIN
    hash := Hash(key);
    index := LENGTH(WSET(hash) * WSET(this.capacity - 1));
    LOOP
        current := this.storage[index];
        IF current = NIL THEN EXIT
        ELSIF ~current.deleted THEN
            IF Equal(current.key, key) THEN
                value := current.value;
                RETURN TRUE
            END;
        END;
        index := LENGTH(WSET(index + 1) * WSET(this.capacity - 1));
    END;
    RETURN FALSE;
END Get;

(* Remove item from dictionary. Return TRUE if item present and was removed *)
PROCEDURE (VAR this : Dictionary) Remove*(key- : Key): BOOLEAN;
VAR
    current, tmp : Entry;
    i, j, k, hash : LENGTH;
BEGIN
    hash := Hash(key);
    i := LENGTH(WSET(hash) * WSET(this.capacity - 1));
    LOOP
        current := this.storage[i];
        IF (current = NIL) THEN RETURN FALSE END;
        IF ~current.deleted THEN
            IF Equal(current.key, key) THEN EXIT END;
        END;
        i := LENGTH(WSET(i + 1) * WSET(this.capacity - 1));
    END;
    j := i;
    LOOP
        (* Possible shift entries back to earlier position if we have a hash collision *)
        j := LENGTH(WSET(j + 1) * WSET(this.capacity - 1));
        current := this.storage[j];
        IF current = NIL THEN EXIT END;
        hash := Hash(current.key);
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
        IF ~this.Resize(this.capacity DIV 2) THEN RETURN FALSE END;
    END;
    RETURN TRUE;
END Remove;

(** Remove item from dictionary if present *)
PROCEDURE (VAR this : Dictionary) Discard*(key- : Key);
BEGIN IGNORE(this.Remove(key))
END Discard;

(** Clears all items without deallocation *)
PROCEDURE (VAR this : Dictionary) Clear*();
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
Remove arbitary item from dictionary and set key.
Return FALSE if dictionary is empty.
Note: this potentially transfere key ownership to caller.
*)
PROCEDURE (VAR this : Dictionary) Pop*(VAR key : Key): BOOLEAN;
VAR 
    i : LENGTH;
    entry : Entry;
BEGIN
    FOR i := 0 TO this.capacity - 1 DO
        entry := this.storage[i];
        IF (entry # NIL) & ~entry.deleted THEN
            key := entry.key;
            DEC(this.size);
            IF this.size < 0 THEN this.size := 0 END;
            this.disposeValue(entry.value);
            DISPOSE(entry);
            this.storage[i] := NIL;
            RETURN TRUE;
        END
    END;
    RETURN FALSE
END Pop;

(**
Remove arbitary item from dictionary and set key and value to item.
Return FALSE if dictionary is empty.
Note: this potentially transfere key and value ownership to caller.
*)
PROCEDURE (VAR this : Dictionary) PopItem*(VAR key : Key; VAR value : Value): BOOLEAN;
VAR 
    i : LENGTH;
    entry : Entry;
BEGIN
    FOR i := 0 TO this.capacity - 1 DO
        entry := this.storage[i];
        IF (entry # NIL) & ~entry.deleted THEN
            key := entry.key;
            value := entry.value;
            DEC(this.size);
            IF this.size < 0 THEN this.size := 0 END;
            DISPOSE(entry);
            this.storage[i] := NIL;
            RETURN TRUE;
        END
    END;
    RETURN FALSE
END PopItem;

(**
Return Vector of keys.
Note: this potentially return a vector of reference to keys. 
*)
PROCEDURE (VAR this- : Dictionary) Keys*(): KeyVector;
VAR 
    i : LENGTH;
    entry : Entry;
    ret : KeyVector;
BEGIN
    ret.Init(this.size);
    FOR i := 0 TO this.capacity - 1 DO
        entry := this.storage[i];
        IF (entry # NIL) & ~entry.deleted THEN
            ret.Append(entry.key)
        END
    END;
    RETURN ret
END Keys;

(**
Return Vector of values.
Note: this potentially return a vector of reference to the values.
*)
PROCEDURE (VAR this- : Dictionary) Values*(): ValueVector;
VAR 
    i : LENGTH;
    entry : Entry;
    ret : ValueVector;
BEGIN
    ret.Init(this.size);
    FOR i := 0 TO this.capacity - 1 DO
        entry := this.storage[i];
        IF (entry # NIL) & ~entry.deleted THEN
            ret.Append(entry.value)
        END
    END;
    RETURN ret
END Values;

(** Returns an iterator for the dictionary. *)
PROCEDURE (VAR this- : Dictionary) First* (VAR iterator: Iterator);
BEGIN
    iterator.storage := this.storage;
    iterator.index := 0;
END First;

(**
Advance iterator and set key. Return `FALSE` if end is reached.
Note: this potentially set the key to a reference.
*)
PROCEDURE (VAR this : Iterator) Next*(VAR key : Key) : BOOLEAN;
VAR entry : Entry;
BEGIN
    WHILE this.index < LEN(this.storage^) DO
        entry := this.storage[this.index];
        INC(this.index);
        IF entry # NIL THEN
            IF ~entry.deleted THEN
                key := entry.key;
                RETURN TRUE
            END
        END
    END;
    RETURN FALSE
END Next;

(**
Advance iterator and set key and value. Return `FALSE` if end is reached.
Note: this potentially set the key and value to a reference.
*)
PROCEDURE (VAR this : Iterator) NextItem*(VAR key : Key; VAR value : Value) : BOOLEAN;
VAR entry : Entry;
BEGIN
    WHILE this.index < LEN(this.storage^) DO
        entry := this.storage[this.index];
        INC(this.index);
        IF entry # NIL THEN
            IF ~entry.deleted THEN
                key := entry.key;
                value := entry.value;
                RETURN TRUE
            END
        END
    END;
    RETURN FALSE
END NextItem;

(** Reset iterator to start of set. *)
PROCEDURE (VAR this : Iterator) Reset*();
BEGIN this.index := 0 END Reset;

END ADTDictionary.