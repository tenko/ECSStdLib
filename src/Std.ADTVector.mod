(**
The `Vector` module implements resizable container.
It is fast to append and remove at end of array.
and fast for random access to elements.

If life time handling of allocated elements is needed
the callback procedures duplicate and dispose can be used.

By default the vectors only takes references of elements and
the caller is responsible for keeping elements alive.
*)
MODULE ADTVector (Element*) IN Std;

IN Std IMPORT ArrayOfByte, Cardinal;

TYPE
    DuplicateElementProc* = PROCEDURE(VAR dst: Element; src-: Element);
    DisposeElementProc* = PROCEDURE(VAR dst: Element);
    VectorStorage = POINTER TO ARRAY OF Element;
    Vector* = RECORD-
        storage* : VectorStorage;
        duplicate* : DuplicateElementProc;
        dispose* : DisposeElementProc;
        last : LENGTH;
    END;
    Compare = PROCEDURE(left-, right- : Element): INTEGER;

CONST INIT_SIZE = 4;

(** defaults to assignment *)
PROCEDURE DefaultDuplicateElement* (VAR dst: Element; src-: Element);
BEGIN dst := src
END DefaultDuplicateElement;

(** defaults to no operation *)
PROCEDURE DefaultDisposeElement* (VAR dst: Element);
BEGIN END DefaultDisposeElement;

(** Initialize  *)
PROCEDURE (VAR this : Vector) Init*(size : LENGTH);
BEGIN
    ASSERT(size > 0);
    IF size < INIT_SIZE THEN size := INIT_SIZE END;
    NEW(this.storage, size);
    IF this.storage = NIL THEN HALT(0) END;
    this.duplicate :=  DefaultDuplicateElement;
    this.dispose := DefaultDisposeElement;
    this.last := 0;
END Init;

(** Length of vector *)
PROCEDURE (VAR this- : Vector) Size*(): LENGTH;
BEGIN RETURN this.last;
END Size;

(** Item capacity of vector *)
PROCEDURE (VAR this- : Vector) Capacity*(): LENGTH;
BEGIN RETURN LEN(this.storage^);
END Capacity;

(** Clear Vector to zero size *)
PROCEDURE (VAR this : Vector) Clear*();
VAR i : LENGTH;
BEGIN
    FOR i := 0 TO this.last - 1 DO
        this.dispose(this.storage[i])
    END;
    this.last := 0;
END Clear;

(** Free storage *)
PROCEDURE (VAR this : Vector) Dispose*();
VAR i : LENGTH;
BEGIN
    FOR i := 0 TO this.last - 1 DO
        this.dispose(this.storage[i])
    END;
    DISPOSE(this.storage);
    this.storage := NIL;
    this.last := 0;
END Dispose;

(** Resize storage to accomodate capacity *)
PROCEDURE (VAR this : Vector) Reserve*(capacity  : LENGTH);
VAR
    storage : VectorStorage;
    cap : LENGTH;
BEGIN
    ASSERT(capacity > 0);
    cap := this.Capacity();
    IF capacity > cap THEN
        WHILE cap < capacity DO cap := cap * 2 END;
        NEW(storage, cap);
        IF storage = NIL THEN HALT(0) END;
        IF this.last > 0 THEN
            ArrayOfByte.Copy(storage^, this.storage^, this.last * SIZE(Element))
        END;
        DISPOSE(this.storage);
        this.storage := storage
    END;
END Reserve;

(** Shrink storage *)
PROCEDURE (VAR this : Vector) Shrink*();
VAR
    storage : VectorStorage;
    cap : LENGTH;
BEGIN
    cap := this.Capacity();
    IF cap > this.last + 1 THEN
        WHILE (cap > this.last) & (cap > INIT_SIZE) DO cap := cap DIV 2 END;
        IF cap < this.last THEN cap := cap * 2 END;
        NEW(storage, cap);
        IF storage = NIL THEN HALT(0) END;
        IF this.last > 0 THEN
            ArrayOfByte.Copy(storage^, this.storage^, this.last * SIZE(Element))
        END;
        DISPOSE(this.storage);
        this.storage := storage
    END;
END Shrink;

(** Append Element to end of Vector *)
PROCEDURE (VAR this : Vector) Append*(value- : Element);
VAR capacity : LENGTH;
BEGIN
    capacity := this.Capacity();
    IF this.last >= capacity THEN
        this.Reserve(capacity + 1)
    END;
    this.duplicate(this.storage[this.last], value);
    INC(this.last)
END Append;

(**
Return value at idx.
Note this may return a reference to a element.
*)
PROCEDURE (VAR this- : Vector) At*(idx : LENGTH): Element;
BEGIN RETURN this.storage[idx]
END At;

(** Set value at idx *)
PROCEDURE (VAR this : Vector) Set*(idx : LENGTH; value- : Element);
BEGIN
    this.duplicate(this.storage[idx], value)
END Set;

(** Get value at idx.
Note: this potentially set the value to a reference.
*)
PROCEDURE (VAR this : Vector) Get*(idx : LENGTH; VAR value : Element);
BEGIN
    value := this.storage[idx];
END Get;

(**
Remove and return last element of vector.
Return FALSE if vector is empty.
Note: this potentially transfere key ownership to caller.
*)
PROCEDURE (VAR this : Vector) Pop*(VAR element : Element) : BOOLEAN;
BEGIN
    IF this.Size() = 0 THEN RETURN FALSE END;
    element := this.storage[this.last - 1];
    DEC(this.last);
    RETURN TRUE;
END Pop;

(** Swap array data at i and j. *)
PROCEDURE (VAR this : Vector) Swap(i, j: LENGTH);
VAR tmp: Element;
BEGIN
    tmp := this.storage[i];
    this.storage[i] := this.storage[j];
    this.storage[j] := tmp;
END Swap;

(** Reverse array in-place *)
PROCEDURE (VAR this : Vector) Reverse*();
VAR start, end: LENGTH;
BEGIN
    start := 0;
    end := this.Size() - 1;
    WHILE start < end DO
       this.Swap(start, end);
       INC(start); DEC(end)
    END;
END Reverse;

(** Random shuffle array in-place *)
PROCEDURE (VAR this : Vector) Shuffle*();
VAR i, j: LENGTH;
BEGIN
    i := this.Size() - 1;
    WHILE i >= 1 DO
        j := LENGTH(Cardinal.RandomRange(this.Size() - 1));
        this.Swap(i, j);
        DEC(i)
    END;
END Shuffle;

(** Sort array in-place (QuickSort) *)
PROCEDURE (VAR this : Vector) Sort* (Cmp : Compare);
VAR N: LENGTH;
    PROCEDURE ISort (l, r: LENGTH);
    VAR
        i, j: LENGTH;
        val : Element;
    BEGIN
        WHILE r > l DO
            i := l + 1;
            j := r;
            WHILE i <= j DO
                val := this.storage[l];
                WHILE (i <= j) & ~(Cmp(val, this.storage[i]) < 0) DO
                    INC(i);
                END;
                WHILE (i <= j) & (Cmp(val, this.storage[j]) < 0) DO
                    DEC(j);
                END;
                IF i <= j THEN
                    IF i # j THEN
                        this.Swap(i, j);
                    END;
                    INC(i);
                    DEC(j)
                END;
            END;
            IF j # l THEN
                this.Swap(j, l)
            END;
            IF j + j > r + l THEN
                ISort(j + 1, r);
                r := j - 1;
            ELSE
                ISort(l, j - 1);
                l := j + 1;
            END;
        END;
    END ISort;
BEGIN
    N := this.Size();
    IF N > 0 THEN
        ISort(0, N-1);
    END
END Sort;

(**
Find position in array. Expect array to be sorted in ascending order.
Return -1 if not found.
*)
PROCEDURE (VAR this- : Vector) Find* (Cmp : Compare; value- : Element): LENGTH;
VAR i, left, right: LENGTH;
BEGIN
    left := 0;
    right := this.Size() - 1;
    WHILE left <= right DO
        i := left + (right - left) DIV 2;
        CASE Cmp(this.storage[i], value) OF
            | -1 : left := i + 1;
            |  0 : RETURN i;
            | +1 : right := i - 1;
        ELSE RETURN -1 END
    END;
    RETURN -1
END Find;

(**
Find leftmost insertion position in array for value.
Expect array to be sorted in ascending order.
*)
PROCEDURE (VAR this- : Vector) BisectLeft* (Cmp : Compare; value- : Element): LENGTH;
VAR i, left, right: LENGTH;
BEGIN
    left := 0;
    right := this.Size();
    WHILE left < right DO
        i := left + (right - left) DIV 2;
        IF Cmp(this.storage[i], value) = -1 THEN
            left := i + 1;
        ELSE
            right := i;
        END;
    END;
    RETURN left
END BisectLeft;

(**
Find rightmost insertion position in array for value.
Expect array to be sorted in ascending order.
*)
PROCEDURE (VAR this- : Vector) BisectRight* (Cmp : Compare; value- : Element): LENGTH;
VAR i, left, right: LENGTH;
BEGIN
    left := 0;
    right := this.Size();
    WHILE left < right DO
        i := left + (right - left) DIV 2;
        IF Cmp(value, this.storage[i]) = -1 THEN
            right := i;
        ELSE
            left := i + 1;
        END;
    END;
    RETURN left
END BisectRight;

(* move a node down in the tree to restore heap relationship *)
PROCEDURE (VAR this : Vector) HeapSiftDown*(Cmp : Compare; i : LENGTH);
VAR size, largest, left, right : LENGTH;
BEGIN
    size := this.Size();
    largest := i;
    left := 2*i + 1;
    right := 2*i + 2;
    
    IF (left < size) & (Cmp(this.storage[left], this.storage[largest]) = 1) THEN
        largest := left;
    END;
    IF (right < size) & (Cmp(this.storage[right], this.storage[largest]) = 1) THEN
        largest := right;
    END;

    IF largest # i THEN
        this.Swap(i, largest);
        this.HeapSiftDown(Cmp, largest);
    END;
END HeapSiftDown;

(** Heapify array in-place *)
PROCEDURE (VAR this : Vector) Heapify*(Cmp : Compare);
VAR i, size : LENGTH;
BEGIN
    size := this.Size();
    IF size <= 1 THEN RETURN END;
    FOR i := (size - 1) DIV 2 TO 0 BY -1 DO
        this.HeapSiftDown(Cmp, i);
    END;
END Heapify;

(**
Remove first element of heap. Return FALSE if heap is empty.
Note: this potentially transfere key ownership to caller.
*)
PROCEDURE (VAR this : Vector) HeapPop*(Cmp : Compare; VAR element : Element) : BOOLEAN;
BEGIN
    IF this.Size() = 0 THEN RETURN FALSE END;
    IF this.Size() = 1 THEN RETURN this.Pop(element) END;

    element := this.storage[0];
    this.storage[0] := this.storage[this.last - 1];
    DEC(this.last);
    this.HeapSiftDown(Cmp, 0);
    RETURN TRUE;
END HeapPop;

(** Insert Element into Vector and update heap relationship *)
PROCEDURE (VAR this : Vector) HeapInsert*(Cmp : Compare; value : Element);
VAR i : LENGTH;
BEGIN
    this.Append(value);
    (* move node up in the tree to restore heap condition *)
    i := this.Size() - 1;
    WHILE (i > 0) & (Cmp(this.storage[(i - 1) DIV 2], this.storage[i]) = -1) DO
        this.Swap(i, (i - 1) DIV 2);
        i := (i - 1) DIV 2;
    END;
END HeapInsert;

END ADTVector.