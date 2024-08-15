(**
Simple allocator implemented as embedded free list
allocator from "The C Programming Language", Page 173.

Ref : https://stackoverflow.com/a/36512105/10830469

Import this module to replace runtime malloc & free.
*)
MODULE SysMem IN Std;
IMPORT SYSTEM;

CONST
    MAGIC = UNSIGNED32(0DEADBEEFH);
    NALLOC = 4;
    ADR = SYSTEM.ADR;

TYPE
    BYTE = SYSTEM.BYTE;
    ADDRESS = SYSTEM.ADDRESS;
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
    AllocSize- : LENGTH;
    Heap- : ADDRESS;

VAR ^ heapStart ["_trailer"]: SYSTEM.BYTE;

PROCEDURE Adr(node : NodePtr): ADDRESS;
BEGIN RETURN SYSTEM.VAL(ADDRESS, node);
END Adr;

(* Calculate free memory size *)
PROCEDURE FreeMem*(): LENGTH;
VAR
    cur : NodePtr;
    size : LENGTH;
BEGIN
    IF FreePtr = NIL THEN RETURN 0 END;
    cur := BasePtr;
    size := cur.size * SIZE(Node);
    WHILE Adr(cur.next) > Adr(cur) DO
        cur := cur.next;
        INC(size, cur.size * SIZE(Node));
    END;
    RETURN size
END FreeMem;

(* Free memory at ptr and add to free list *)
PROCEDURE Dispose* ["free"] (ptr : ADDRESS);
VAR
    cur, ins, nxt : NodePtr;
    adrins, adrcur, anxt : ADDRESS;
BEGIN
    IF ptr = 0 THEN  RETURN END;
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
        ins.next := cur.next.next;
    ELSE
        (* the insertion block is not left-adjacent to the beginning of another block *)
        ins.next := cur.next;
    END;
    IF (adrcur + cur.size*SIZE(Node)) = adrins THEN
        (* the end of another block of data is adjacent to the beginning of the insertion block *)
        INC(cur.size, ins.size);
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
    adr : ADDRESS;
BEGIN
    IF nunits < NALLOC THEN nunits := NALLOC END;
    SYSTEM.PUT(ADR(node), Heap);
    INC(Heap, nunits * SIZE(Node));
    INC(AllocSize, nunits * SIZE(Node));
    node.magic := MAGIC;
    node.next := NIL;
    node.size := nunits;
    (* insert node into freelist *)
    Dispose(Adr(node) + SIZE(Node));
    RETURN FreePtr
END MoreCore;

(* Allocate nbytes memory and return adddress. Return 0 on failure *)
PROCEDURE New* ["malloc"] (nbytes : LENGTH): ADDRESS;
VAR
    cur, prev : NodePtr;
    nunits, i : LENGTH;
BEGIN
    nunits := ((nbytes + SIZE(Node) - 1) DIV SIZE(Node)) + 1;
    IF FreePtr = NIL THEN
        (* Insert sentinentel node *)
        BasePtr := SYSTEM.VAL(NodePtr, ADR(Base));
        Base.magic := MAGIC;
        Base.next := BasePtr;
        Base.size := 0;
        FreePtr := BasePtr;
        Heap := ADR(heapStart);
        Heap := ADDRESS(SET32(Heap + 3) - SET32(1));
    END;
    prev := FreePtr; cur := prev.next; i := 0;
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

BEGIN
    AllocSize := 0;
    Heap := 0;
END SysMem.