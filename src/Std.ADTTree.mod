(** 
AVL tree (Adelson-Velsky and Landis) is a self-balancing binary
search tree.

AVL trees keep the order of nodes on insertion/delete and allow
for fast find operations (average and worst case O(log n)).

If life time handling of allocated elements is needed
the callback procedures duplicate and dispose can be used.

By default the tree only takes references of elements and
the caller is responsible for keeping elements alive.

Copyright (c) 1993 xTech Ltd, Russia. All Rights Reserved.
Tenko : Modified for inclusion in library

TODO : Find safe way to free all nodes.
*)
MODULE ADTTree (Element*, Compare*) IN Std;

TYPE
    DuplicateElementProc = PROCEDURE(VAR dst: Element; src-: Element);
    DisposeElementProc = PROCEDURE(VAR dst: Element);
    Node = POINTER TO NodeDesc;
    NodeDesc = RECORD-
        element: Element;
        left, right, up: Node;
        bal : INTEGER;
    END;
    Tree* = RECORD-
        up: Node;
        duplicate* : DuplicateElementProc;
        dispose* : DisposeElementProc;
        size: LENGTH;
    END;
    Iterator* = RECORD
        current : Node;
        duplicate : DuplicateElementProc;
        reverse : BOOLEAN;
    END;

PROCEDURE DefaultDuplicateElement (VAR dst: Element; src-: Element);
BEGIN dst := src
END DefaultDuplicateElement;

PROCEDURE DefaultDisposeElement (VAR dst: Element);
BEGIN END DefaultDisposeElement;

(* Return next node or NIL if last *)
PROCEDURE NextNode(current : Node) : Node;
VAR node, ret : Node;
BEGIN
    IF current = NIL THEN RETURN NIL END;
    IF current.right # NIL THEN
        ret := current.right;
        WHILE ret.left # NIL DO ret := ret.left END;
    ELSE
        node := current;
        ret := node.up;
        WHILE (ret # NIL) & (node = ret.right) DO
            node := ret;
            ret := ret.up;
        END;
    END;
    RETURN ret;
END NextNode;

(* Return previous node or NIL if first *)
PROCEDURE PrevNode(current : Node) : Node;
VAR node, ret : Node;
BEGIN
    IF current = NIL THEN RETURN NIL END;
    IF current.left # NIL THEN
        ret := current.left;
        WHILE ret.right # NIL DO ret := ret.right END;
    ELSE
        node := current;
        ret := node.up;
        WHILE (ret # NIL) & (node = ret.left) DO
            node := ret;
            ret := ret.up;
        END;
    END;
    RETURN ret;
END PrevNode;

(** Initialize  *)
PROCEDURE (VAR this : Tree) Init*;
BEGIN
    this.duplicate := DefaultDuplicateElement;
    this.dispose := DefaultDisposeElement;
    this.size := 0;
END Init;

(** Removes all elements from the tree. *)
PROCEDURE (VAR this: Tree) Dispose*;
VAR node, left, right, up : Node;
BEGIN
    node := this.up;
    up := NIL;
    WHILE node # NIL DO
        IF node.left # NIL THEN
            left := node.left;
            node.left := up;
            up := node;
            node := left;
        ELSIF node.right # NIL THEN
            right := node.right;
            node.left := up;
            node.right := NIL;
            up := node;
            node := right;
        ELSE
            IF up = NIL THEN
                this.dispose(node.element);
                DISPOSE(node);
            ELSE
                LOOP
                    IF up = NIL THEN EXIT END;
                    this.dispose(node.element);
                    DISPOSE(node);
                    IF up.right # NIL THEN
                        node := up.right;
                        up.right := NIL;
                        EXIT
                    ELSE
                        node := up;
                        up := up.left
                    END
                END;
            END;
        END;
    END;
    this.up := NIL;
    this.size := 0;
END Dispose;

(** Returns the size of the tree. *)
PROCEDURE (VAR this-: Tree) Size* (): LENGTH;
BEGIN
    IF this.up # NIL THEN RETURN this.size END;
    RETURN 0;
END Size;

(** Find node equal to element argument. Return NIL if no node is found. *)
PROCEDURE (VAR this-: Tree) FindNode*(element-: Element) : Node;
VAR
    node: Node;
    i : INTEGER;
BEGIN
    IF this.up = NIL THEN RETURN NIL END;
    node := this.up;
    WHILE node # NIL DO
        i := Compare(element, node.element);
        IF i = 0 THEN RETURN node
        ELSIF i > 0 THEN node := node.right
        ELSE node := node.left
        END;
    END;
    RETURN NIL
END FindNode;

(** Return TRUE if tree has element equal to given element *)
PROCEDURE (VAR this- : Tree) HasElement*(element- : Element): BOOLEAN;
BEGIN RETURN this.FindNode(element) # NIL
END HasElement;

(** Forward iterator for the tree. *)
PROCEDURE (VAR this-: Tree) First* (VAR iterator: Iterator);
VAR node: Node;
BEGIN
    iterator.reverse := FALSE;
    node := this.up;
    IF node # NIL THEN
        WHILE node.left # NIL DO node := node.left END
    END;
    iterator.current := node;
    iterator.duplicate := this.duplicate;
END First;

(** Reverse iterator for the tree. *)
PROCEDURE (VAR this-: Tree) Last* (VAR iterator: Iterator);
VAR node: Node;
BEGIN
    iterator.reverse := TRUE;
    iterator.duplicate := this.duplicate;
    node := this.up;
    IF node # NIL THEN
        WHILE node.right # NIL DO node := node.right END
    END;
    iterator.current := node;
END Last;

(** Forward iterator where element is found. *)
PROCEDURE (VAR this-: Tree) FindForward* (VAR iterator: Iterator; element- : Element);
BEGIN
    iterator.reverse := FALSE;
    iterator.duplicate := this.duplicate;
    iterator.current := this.FindNode(element);
END FindForward;

(** Reverse iterator where element is found. *)
PROCEDURE (VAR this-: Tree) FindReverse* (VAR iterator: Iterator; element- : Element);
BEGIN
    iterator.reverse := TRUE;
    iterator.duplicate := this.duplicate;
    iterator.current := this.FindNode(element);
END FindReverse;

(**
Advance iterator. Return `FALSE` if end is reached.
Note this may be duplicate the element and the caller would be
responsible for the lifetime of the element.
*)
PROCEDURE (VAR this: Iterator) Next* (VAR element: Element): BOOLEAN;
BEGIN
    IF this.current # NIL THEN
        this.duplicate(element, this.current.element);
        IF this.reverse THEN
            this.current := PrevNode(this.current)
        ELSE
            this.current := NextNode(this.current)
        END;
        RETURN TRUE
    END;
    RETURN FALSE;
END Next;

PROCEDURE (VAR this: Tree) SimpleInsert(e-: Element) : Node;
VAR
    p, h: Node;
    i : INTEGER;
    direction: BOOLEAN;
BEGIN
    IF this.up # NIL THEN
        h := this.up;
        REPEAT
            p := h;
            i := Compare(e, h.element);
            IF i = 0 THEN
                (* replace element *)
                this.dispose(h.element);
                this.duplicate(h.element, e);
                RETURN NIL
            ELSIF i > 0 THEN h := h.right; direction := TRUE
            ELSE h := h.left;  direction := FALSE
            END;
        UNTIL h = NIL;
        NEW(h);
        h.left:= NIL; h.right:= NIL; h.up:= p;
        this.duplicate(h.element, e);
        h.bal:= 0;
        IF direction THEN p.right:= h;
        ELSE p.left:= h;
        END;
        INC(this.size);
        RETURN h
    ELSE
        NEW(this.up);
        this.up.right := NIL;
        this.up.left := NIL;
        this.up.up := NIL;
        this.up.bal := 0;
        this.duplicate(this.up.element, e);
        this.size := 1;
        RETURN this.up;
    END;
END SimpleInsert;

(** Insert or replace element *)
PROCEDURE (VAR this: Tree) Insert*(element-: Element);
VAR cur, p, p2: Node;
BEGIN
    cur := this.SimpleInsert(element);
    WHILE ( cur # NIL ) & ( cur # this.up )  DO
        p:= cur.up;
        IF p.left = cur THEN (* left branch grows up   *)
            CASE p.bal OF
                1 : p.bal:= 0;  RETURN;
                |0 : p.bal:= -1;
                |-1: (* balance *)
                IF cur.bal = -1 THEN (* single LL turn *)
                    p.left:= cur.right;
                    IF cur.right # NIL THEN  cur.right.up:= p  END;
                    cur.right:= p; cur.up:= p.up; p.up:= cur;
                    p.bal:= 0;
                    IF p = this.up THEN this.up:= cur;
                    ELSIF cur.up.left = p THEN cur.up.left:= cur;
                    ELSE cur.up.right:= cur;
                    END;
                    p:= cur;
                ELSE (* double LR turn *)
                    p2:= cur.right;
                    cur.right:= p2.left;
                    IF p2.left # NIL THEN  p2.left.up:= cur  END;
                    p2.left:= cur; cur.up:= p2;
                    p.left:= p2.right;
                    IF p2.right # NIL THEN p2.right.up:= p   END;
                    p2.right:= p; p2.up:= p.up; p.up:= p2;
                    IF p2.bal = -1 THEN  p.bal:= 1     ELSE  p.bal:= 0    END;
                    IF p2.bal =  1 THEN  cur.bal:= -1  ELSE  cur.bal:= 0  END;
                    IF p = this.up THEN this.up:= p2
                    ELSIF p2.up.left = p THEN p2.up.left:= p2;
                    ELSE p2.up.right:= p2;
                    END;
                    p:= p2;
                END;
                p.bal:= 0;
                RETURN;
            END;
        ELSE  (* right branch grows up   *)
            CASE p.bal OF
                -1: p.bal:= 0;  RETURN;
                |0 : p.bal:= 1;
                |1 : (* balance *)
                IF cur.bal = 1 THEN  (* single RR turn *)
                    p.right:= cur.left;
                    IF cur.left # NIL THEN cur.left.up:= p END;
                    cur.left:= p; cur.up:= p.up; p.up:= cur;
                    p.bal:= 0;
                    IF p = this.up THEN this.up:= cur
                    ELSIF cur.up.left = p THEN cur.up.left:= cur;
                    ELSE  cur.up.right:= cur;
                    END;
                    p:= cur;
                ELSE  (* double RL turn *)
                    p2:= cur.left;
                    cur.left:= p2.right;
                    IF p2.right # NIL THEN  p2.right.up:= cur END;
                    p2.right:= cur; cur.up:= p2;
                    p.right:= p2.left;
                    IF p2.left # NIL THEN  p2.left.up:= p    END;
                    p2.left:= p; p2.up:= p.up; p.up:= p2;
                    IF p2.bal =  1 THEN  p.bal:= -1   ELSE  p.bal:= 0    END;
                    IF p2.bal = -1 THEN  cur.bal:= 1  ELSE  cur.bal:= 0  END;
                    IF p = this.up THEN this.up:= p2
                    ELSIF p2.up.left = p THEN p2.up.left:= p2;
                    ELSE p2.up.right:= p2;
                    END;
                    p:= p2;
                END;
                p.bal:= 0;
                RETURN;
            END;
        END;
        cur:= cur.up;
    END;
END Insert;

(* left branch grows down *)
PROCEDURE (VAR this: Tree) BalanceL(VAR p: Node; VAR h: BOOLEAN);
VAR p1, p2: Node;
    b1, b2: INTEGER;
BEGIN
    CASE p.bal OF
        -1: p.bal:= 0;
        |0 : p.bal:= 1;  h:= FALSE;
        |1 : (* balance *)
            p1:= p.right; b1:= p1.bal;
            IF b1 >= 0 THEN  (* single RR turn *)
                p.right:= p1.left;
                IF p1.left # NIL THEN p1.left.up:= p END;
                p1.left:= p; p1.up:= p.up; p.up:= p1;
                IF b1 = 0 THEN p.bal:= 1; p1.bal:= -1; h:= FALSE;
                ELSE p.bal:= 0; p1.bal:= 0;
                END;
                IF p = this.up THEN this.up:= p1;
                ELSIF p1.up.left = p THEN p1.up.left:= p1;
                ELSE  p1.up.right:= p1;
                END;
                p:= p1;
            ELSE  (* double RL turn *)
                p2:= p1.left;  b2:= p2.bal;
                p1.left:= p2.right;
                IF p2.right # NIL THEN  p2.right.up:= p1 END;
                p2.right:= p1; p1.up:= p2;
                p.right:= p2.left;
                IF p2.left # NIL THEN  p2.left.up:= p END;
                p2.left:= p; p2.up:= p.up; p.up:= p2;
                IF b2 =  1 THEN  p.bal:= -1   ELSE  p.bal:= 0    END;
                IF b2 = -1 THEN  p1.bal:= 1   ELSE  p1.bal:= 0   END;
                IF p = this.up THEN this.up:= p2
                ELSIF p2.up.left = p THEN p2.up.left:= p2;
                ELSE p2.up.right:= p2;
                END;
                p:= p2;
                p2.bal:= 0;
            END;
    END;
END BalanceL;

(* right branch grows down *)
PROCEDURE (VAR this: Tree) BalanceR(VAR p: Node; VAR h: BOOLEAN);
VAR p1, p2: Node;
    b1, b2: INTEGER;
BEGIN
    CASE p.bal OF
     1 : p.bal:= 0;
    |0 : p.bal:= -1; h:= FALSE;
    |-1: (* balance *)
        p1:= p.left; b1:= p1.bal;
        IF b1 <= 0 THEN (* single LL turn *)
            p.left:= p1.right;
            IF p1.right # NIL THEN p1.right.up:= p END;
            p1.right:= p; p1.up:= p.up; p.up:= p1;
            IF b1 = 0 THEN p.bal:= -1; p1.bal:= 1; h:= FALSE;
            ELSE p.bal:= 0; p1.bal:= 0;
            END;
            IF p = this.up THEN this.up:= p1;
            ELSIF p1.up.left = p THEN p1.up.left:= p1;
            ELSE p1.up.right:= p1;
            END;
            p:= p1;
        ELSE (* double LR turn *)
            p2:= p1.right; b2:= p2.bal;
            p1.right:= p2.left;
            IF p2.left # NIL THEN p2.left.up:= p1 END;
            p2.left:= p1; p1.up:= p2;
            p.left:= p2.right;
            IF p2.right # NIL THEN p2.right.up:= p END;
            p2.right:= p; p2.up:= p.up; p.up:= p2;
            IF b2 = -1 THEN  p.bal:= 1     ELSE  p.bal:= 0    END;
            IF b2 =  1 THEN  p1.bal:= -1   ELSE  p1.bal:= 0   END;
            IF p = this.up THEN this.up:= p2
            ELSIF p2.up.left = p THEN p2.up.left:= p2;
            ELSE p2.up.right:= p2;
            END;
            p:= p2;
            p2.bal:= 0;
        END;
    END;
END BalanceR;

(* Remove element from tree if found *)
PROCEDURE (VAR this: Tree) IRemove(element-: Element; VAR node : Node): BOOLEAN;
    CONST root  = 0;
          right = 1;
          left  = 2;
    VAR p: Node;
        h: BOOLEAN;
        i, direction: INTEGER;

    PROCEDURE Del(r: Node; VAR h: BOOLEAN);
    BEGIN
        WHILE r.right # NIL DO r:= r.right END;
        p.element := r.element;
        IF r.up = p THEN p.left:= r.left;
        ELSE r.up.right:= r.left;
        END;
        IF r.left # NIL THEN r.left.up:= r.up END;
        h:= TRUE;
        r:= r.up; (* balance *)
        WHILE ( h ) & ( r # p ) DO
            this.BalanceR( r, h );
            r:= r.up;
        END;
    END Del;
BEGIN
    IF this.up = NIL THEN RETURN FALSE END;
    node := this.up;
    direction:= root;
    LOOP
        IF node = NIL THEN RETURN FALSE END;
        i := Compare(element, node.element);
        IF i = 0 THEN EXIT
        ELSIF i > 0 THEN node := node.right; direction:= right;
        ELSE node:= node.left; direction:= left;
        END;
    END;
    p := node;
    IF this.up.up = p THEN this.up.up:= NIL END;
    IF p.right = NIL THEN
        h:= TRUE;
        IF p.left # NIL THEN p.left.up:= p.up  END;
        CASE direction OF
         root : this.up:= p.left;
                RETURN TRUE;
        |right: p.up.right:= p.left;
                p:= p.up;
                this.BalanceR(p, h);
        |left : p.up.left:= p.left;
                p:= p.up;
                this.BalanceL(p, h);
        END;
    ELSIF p.left = NIL THEN
        h:= TRUE;
        p.right.up:= p.up;
        CASE direction OF
        root : this.up:= p.right;
                RETURN TRUE;
        |right: p.up.right:= p.right;
                p:= p.up;
                this.BalanceR(p, h);
        |left : p.up.left:= p.right;
                p:= p.up;
                this.BalanceL(p, h);
        END;
    ELSE
        Del(p.left, h);
        IF h THEN this.BalanceL(p, h) END;
    END;
    (* balance *)
    WHILE (h) & (p # this.up) DO
        IF p.up.left = p THEN
            p:= p.up;
            this.BalanceL(p, h);
        ELSE
            p:= p.up;
            this.BalanceR(p, h);
        END;
    END;
    RETURN TRUE;
END IRemove;

(** Remove node from tree if found. Return TRUE if item present and was removed *)
PROCEDURE (VAR this : Tree) Remove*(element- : Element): BOOLEAN;
VAR node : Node;
BEGIN
    IF this.IRemove(element, node) THEN
        this.dispose(node.element);
        DISPOSE(node);
        DEC(this.size);
        RETURN TRUE
    END;
    RETURN FALSE
END Remove;

END ADTTree.