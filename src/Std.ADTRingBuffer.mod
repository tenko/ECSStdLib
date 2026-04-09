(**
The `RingBuffer` module implements buffer with
with fixed size and type where data is pushed
to the tail and popped from the head.
This is therefore a FIFO (First in first out) queue.

The size of the buffer must be of power of two and
the available capacity is equal to the buffer size
minus one, due to the last element used as an end marker.
*)   
MODULE ADTRingBuffer(Type, Size*) IN Std;

CONST
    Mask = SET(Size - 1); (* Mask for fast modulo operation *)
    
TYPE
    RingBuffer* = RECORD
        data : ARRAY Size OF Type;
        head, tail : CARDINAL;
    END;

(** Initialize  *)
PROCEDURE Init*(VAR this : RingBuffer);
BEGIN
    ASSERT((Size > 0) & (SET(Size) * Mask = {})); (* Check that size is power of 2 *)
    ASSERT((Size <= MAX(CARDINAL)));
    this.head := 0;
    this.tail := 0;
END Init;

(** Return capacity of buffer  *)
PROCEDURE (VAR this : RingBuffer) Capacity*(): LENGTH;
BEGIN RETURN Size - 1;
END Capacity;

(** Return current size of buffer  *)
PROCEDURE (VAR this : RingBuffer) Size*(): LENGTH;
BEGIN RETURN LENGTH(SET(this.tail - this.head) * Mask);
END Size;

(** Clear content of buffer  *)
PROCEDURE (VAR this : RingBuffer) Clear*();
BEGIN
    this.head := 0;
    this.tail := 0;
END Clear;

(** Push value to tail. Return TRUE if the buffer was not full  *)
PROCEDURE (VAR this : RingBuffer) Push*(value- : Type): BOOLEAN;
VAR tail : CARDINAL;
BEGIN
    tail := this.tail;
    IF SET(tail + 1) * Mask = SET(this.head) THEN
        RETURN FALSE
    END;
    this.data[LENGTH(tail)] := value;
    this.tail := CARDINAL(SET(tail + 1) * Mask);
    RETURN TRUE;
END Push;

(** Pop value from head. Return TRUE if data was avaiable  *)
PROCEDURE (VAR this : RingBuffer) Pop*(VAR value : Type): BOOLEAN;
VAR head : CARDINAL;
BEGIN
    head := this.head;
    IF head = this.tail THEN
        RETURN FALSE
    END;
    value := this.data[LENGTH(head)];
    this.head := CARDINAL(SET(head + 1) * Mask);
    RETURN TRUE;
END Pop;

(** Peek value at index. Return TRUE if data was avaiable  *)
PROCEDURE (VAR this : RingBuffer) Peek*(index : CARDINAL; VAR value : Type): BOOLEAN;
VAR head, size : CARDINAL;
BEGIN
    head := this.head;
    size := CARDINAL(SET(this.tail - head) * Mask);
    IF (size = 0) OR (index >= size) THEN
        RETURN FALSE
    END;
    value := this.data[LENGTH(SET(head + index) * Mask)];
    RETURN TRUE;
END Peek;

END ADTRingBuffer.