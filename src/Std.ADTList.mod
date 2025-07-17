(** 
Double linked list class.
Fast for insertions and remove and head or tail.

If life time handling of allocated elements is needed
the callback procedures duplicate and dispose can be used.

By default the list only takes references of elements and
the caller is responsible for keeping elements alive.
*)
MODULE ADTList (Element*) IN Std;

TYPE
    DuplicateElementProc = PROCEDURE(VAR dst: Element; src-: Element);
    DisposeElementProc = PROCEDURE(VAR dst: Element);
    PROC = PROCEDURE(VAR element: Element);
    Node* = RECORD-
        element-: Element;
        next, prev: POINTER TO Node;
    END;
    List* = RECORD-
        first, last: POINTER TO Node;
        duplicate* : DuplicateElementProc;
        dispose* : DisposeElementProc;
        size: LENGTH;
    END;
    Iterator* = RECORD
        reverse : BOOLEAN;
        current : POINTER TO Node;
    END;

PROCEDURE DefaultDuplicateElement* (VAR dst: Element; src-: Element);
BEGIN dst := src
END DefaultDuplicateElement;

PROCEDURE DefaultDisposeElement* (VAR dst: Element);
BEGIN END DefaultDisposeElement;

(** Initialize  *)
PROCEDURE (VAR this : List) Init*;
BEGIN
    this.duplicate := DefaultDuplicateElement;
    this.dispose := DefaultDisposeElement;
    this.first := NIL;
    this.last := NIL;
    this.size := 0;
END Init;

(** Removes all elements from the list. *)
PROCEDURE (VAR this: List) Dispose*;
VAR node, next: POINTER TO Node;
BEGIN
    node := this.first;
    WHILE node # NIL DO
        next := node.next;
        this.dispose(node.element);
        DISPOSE (node);
        node := next
    END;
    this.first := NIL;
    this.last := NIL;
    this.size := 0;
END Dispose;

(** Returns the size of the list. *)
PROCEDURE (VAR this-: List) Size* (): LENGTH;
BEGIN
    IF this.first # NIL THEN RETURN this.size END;
    RETURN 0;
END Size;

(** Returns whether the list is empty. *)
PROCEDURE (VAR this-: List) IsEmpty* (): BOOLEAN;
BEGIN RETURN this.first = NIL;
END IsEmpty;

(** Apply procedure to all elements of the list. *)
PROCEDURE (VAR this: List) Apply* (proc : PROC);
VAR node, next: POINTER TO Node;
BEGIN
    node := this.first;
    WHILE node # NIL DO
        next := node.next;
        proc(node.element);
        node := next
    END;
END Apply;

(** Append element to tail of list *)
PROCEDURE (VAR this: List) Append* (element-: Element);
VAR node: POINTER TO Node;
BEGIN
    NEW(node);
    IF node = NIL THEN HALT(0) END;
    this.duplicate(node.element, element);
    node.next := NIL;
    node.prev := this.last;
    IF this.last # NIL THEN
        this.last.next := node;
        INC (this.size)
    ELSE
        this.first := node;
        this.size := 1
    END;
    this.last := node;
END Append;

(** Append element to head of list *)
PROCEDURE (VAR this: List) AppendHead* (element-: Element);
VAR node: POINTER TO Node;
BEGIN
    NEW(node);
    IF node = NIL THEN HALT(0) END;
    this.duplicate(node.element, element);
    node.next := this.first;
    node.prev := NIL;
    IF this.size = 0 THEN
        this.last := node;
    ELSE
        this.first.prev := node;
    END;
    this.first := node;
    INC(this.size);
END AppendHead;

(**
Remove and return element at tail of list-
Return FALSE if list is empty.
Note: this potentially transfere element ownership to caller.
*)
PROCEDURE (VAR this: List) Pop* (VAR element: Element): BOOLEAN;
VAR node: POINTER TO Node;
BEGIN
    IF this.last = NIL THEN RETURN FALSE END;
    node := this.last;
    element := node.element;
    IF this.size = 1 THEN
        this.first := NIL;
        this.last := NIL;
    ELSE
        this.last := this.last.prev;
        this.last.next := NIL;
    END;
    DISPOSE(node);
    DEC(this.size);
    RETURN TRUE
END Pop;

(**
Remove and return element at head of list
Return FALSE if list is empty.
Note: this potentially transfere element ownership to caller.
*)
PROCEDURE (VAR this: List) PopHead* (VAR element: Element): BOOLEAN;
VAR node: POINTER TO Node;
BEGIN
    IF this.first = NIL THEN RETURN FALSE END;
    node := this.first;
    element := node.element;
    IF this.size = 1 THEN
        this.first := NIL;
        this.last := NIL;
    ELSE;
        this.first := node.next;
    END;
    DISPOSE(node);
    DEC(this.size);
    RETURN TRUE; 
END PopHead;

(** Returns an forward iterator for the list. *)
PROCEDURE (VAR this-: List) First* (VAR iterator: Iterator);
BEGIN
    iterator.reverse := FALSE;
    iterator.current := this.first;
END First;

(** Returns an reverse iterator for the list. *)
PROCEDURE (VAR this-: List) Last* (VAR iterator: Iterator);
BEGIN
    iterator.reverse := TRUE;
    iterator.current := this.last;
END Last;

(**
Advance iterator. Return `FALSE` if end is reached.
Note: this potentially set the element to a reference.
*)
PROCEDURE (VAR this: Iterator) Next* (VAR element: Element): BOOLEAN;
BEGIN
    IF this.current # NIL THEN
        element := this.current.element;
        IF this.reverse THEN this.current := this.current.prev
        ELSE this.current := this.current.next END;
        RETURN TRUE
    END;
    RETURN FALSE;
END Next;

END ADTList.