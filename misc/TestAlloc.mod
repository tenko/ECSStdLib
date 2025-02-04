(**
Simple embedded free list allocator from
"The C Programming Language", Page 173.

Ref : https://stackoverflow.com/a/36512105/10830469
*)
MODULE test;

IMPORT SYSTEM;
IN Std IMPORT Cardinal, ADTVector(SYSTEM.ADDRESS);

CONST
    MAGIC = UNSIGNED32(0DEADBEEFH);
    NALLOC = 4;
    ADR = SYSTEM.ADR;

TYPE
    BYTE = SYSTEM.BYTE;
    ADDRESS = SYSTEM.ADDRESS;
    BYTES = POINTER TO ARRAY OF BYTE;
    NodePtr = POINTER TO Node;
    Node = RECORD-
        magic : UNSIGNED32;
        next : NodePtr;
        size : LENGTH;
    END;
VAR
    FreePtr : NodePtr;
    BasePtr : NodePtr;
    Base : Node;
    AllocSize : LENGTH;
    vec : ADTVector.Vector;

(* Calculate free memory size *)
PROCEDURE FreeMem(): LENGTH;
VAR
    cur : NodePtr;
    size : LENGTH;
    PROCEDURE Adr(node : NodePtr): ADDRESS;
    BEGIN RETURN SYSTEM.VAL(ADDRESS, node);
    END Adr;
BEGIN
    IF FreePtr = NIL THEN RETURN 0 END;
    cur := BasePtr;
    size := cur.size * SIZE(Node);
    WHILE (cur.next # NIL) & (Adr(cur.next) > Adr(cur)) DO
        cur := cur.next;
        INC(size, cur.size * SIZE(Node));
    END;
    RETURN size
END FreeMem;

(* Free memory at ptr and add to free list *)
PROCEDURE Dispose(ptr : ADDRESS);
VAR
    cur, ins, nxt : NodePtr;
    adrins, adrcur, anxt : ADDRESS;
    PROCEDURE Adr(node : NodePtr): ADDRESS;
    BEGIN RETURN SYSTEM.VAL(ADDRESS, node);
    END Adr;
BEGIN
    IF ptr = 0 THEN RETURN END;
    SYSTEM.PUT(ADR(ins), ptr - SIZE(Node));
    IF ins.magic # MAGIC THEN RETURN END;
    adrins := Adr(ins);
    cur := FreePtr;
    (* Step through the free list looking for the position *)
    LOOP
        adrcur := Adr(cur);
        anxt := Adr(cur.next);
        IF (adrcur >= anxt) & ((adrcur < adrins) OR (adrins < anxt)) THEN
            EXIT (* at one end or other *)
        END;
        IF (adrcur < adrins) & (adrins < anxt) THEN
            EXIT (* between two nodes *)
        END;
        cur := cur.next;
    END;
    IF (adrins + ins.size*SIZE(Node)) = anxt THEN
        (* the end of the insertion block is adjacent to the beginning of another block *)
        INC(ins.size, cur.next.size);
        ASSERT(cur.next.magic = MAGIC);
        ASSERT(cur.next.next.magic = MAGIC);
        (* cur.magic := 0; *)
        ins.next := cur.next.next;
    ELSE
        (* the insertion block is not left-adjacent to the beginning of another block *)
        ins.next := cur.next;
    END;
    IF (adrcur + cur.size*SIZE(Node)) = adrins THEN
        (* the end of another block of data is adjacent to the beginning of the insertion block *)
        INC(cur.size, ins.size);
        (* ins.magic := 0; *)
        cur.next := ins.next;
    ELSE
        (* the insertion block is not right-adjacent to the end of another block *)
        cur.next := ins;
    END;
    (* Set the free pointer list to start the block previous to the insertion block. *)
    FreePtr := cur;
END Dispose;

(* Allocate new memory from system *)
PROCEDURE MoreCore(nunits : LENGTH): NodePtr;
VAR
    node : NodePtr;
    arr : BYTES;
BEGIN
    IF nunits < NALLOC THEN nunits := NALLOC END;
    SYSTEM.NEW(arr, nunits * SIZE(Node));
    IF arr = NIL THEN RETURN NIL END;
    INC(AllocSize, nunits * SIZE(Node));
    SYSTEM.PUT(ADR(node), ADR(arr[0]));
    node.magic := MAGIC;
    node.next := NIL;
    node.size := nunits;
    (* insert node into freelist *)
    Dispose(ADR(arr[0]) + SIZE(Node));
    RETURN FreePtr
END MoreCore;

(* Allocate nbytes memory and return adddress. Return 0 on failure *)
PROCEDURE New(nbytes : LENGTH): ADDRESS;
VAR
    cur, prev : NodePtr;
    nunits : LENGTH;
    PROCEDURE Adr(node : NodePtr): ADDRESS;
    BEGIN RETURN SYSTEM.VAL(ADDRESS, node);
    END Adr;
BEGIN
    nunits := ((nbytes + SIZE(Node) - 1) DIV SIZE(Node)) + 1;
    IF FreePtr = NIL THEN
        (* Insert sentinentel node *)
        NEW(BasePtr);
        FreePtr := BasePtr;
        BasePtr.magic := MAGIC;
        BasePtr.next := FreePtr;
        BasePtr.size := 0;
    END;
    prev := FreePtr; cur := prev.next;
    LOOP
        IF cur.size >= nunits THEN
            (* found a block of memory large enough *)
            IF cur.size = nunits THEN
                (* exact match *)
                prev.next := cur.next;
            ELSE
                (* split block *)
                DEC(cur.size, nunits);
                SYSTEM.PUT(ADR(cur), Adr(cur) + cur.size*SIZE(Node));
                cur.magic := MAGIC;
                cur.size := nunits;
            END;
            FreePtr := prev;
            ASSERT(cur.magic = MAGIC);
            RETURN Adr(cur) + SIZE(Node)
        END;
        IF cur = FreePtr THEN
            (* Wrap around list, allocate new memory *)
            cur := MoreCore(nunits);
            IF cur = NIL THEN RETURN 0 END;
        END;
        prev := cur; cur := cur.next;
    END;
END New;

PROCEDURE Test;
VAR
    a1, a2, a3 : ADDRESS;
    i, size : LENGTH;
BEGIN
    AllocSize := 0;

    vec.Init(1000);
    FOR i := 0 TO 999 DO
        size := LENGTH(Cardinal.RandomRange(354)) + 1;
        a1 := New(size);
        IF a1 = 0 THEN TRACE("Alloc a1 failed"); HALT(0) END;
        vec.Append(a1);
    END;
    TRACE(FreeMem());
    TRACE(AllocSize);
    vec.Shuffle();

    FOR i := 0 TO 999 DO
        a1 := vec.At(i);
        Dispose(a1);
    END;

    TRACE(FreeMem());
    TRACE(AllocSize);

    vec.Dispose();
END Test;

BEGIN
    Test;
END test.
