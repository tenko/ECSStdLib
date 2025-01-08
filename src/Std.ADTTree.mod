(**
A RBTree (red-black) is a variant of binary search tree.
The trees keep the order of nodes on insertion/delete and allow
for fast find operations. It has O(log n) worst-case time for each operation.

If life time handling of allocated elements is needed
the callback procedures duplicate and dispose can be used.

By default the tree only takes references of elements and
the caller is responsible for keeping elements alive.

Reference to Wikipedia page on the subject for implementation details.
Adapted from https://github.com/sakeven/RbTree License MIT.
*)
MODULE ADTTree (Element*, Compare*) IN Std;

CONST
	BLACK = FALSE;
	RED = TRUE;

TYPE
	Color = BOOLEAN;
    DuplicateElementProc = PROCEDURE(VAR dst: Element; src-: Element);
    DisposeElementProc = PROCEDURE(VAR dst: Element);
    
    Node = POINTER TO NodeDesc;
    NodeDesc = RECORD-
    	left, right, parent: Node;
        element: Element;
        color : Color;
    END;
    Tree* = RECORD-
        root: Node;
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

(** Initialize  *)
PROCEDURE (VAR this : Tree) Init*;
BEGIN
    this.duplicate := DefaultDuplicateElement;
    this.dispose := DefaultDisposeElement;
    this.size := 0
END Init;

(** Returns the size of the tree. *)
PROCEDURE (VAR this-: Tree) Size* (): LENGTH;
BEGIN
    IF this.root # NIL THEN RETURN this.size END;
    RETURN 0;
END Size;

(* Return lefmost node *)
PROCEDURE MinNode(x : Node) : Node;
BEGIN
	IF x = NIL THEN RETURN NIL END;
	WHILE x.left # NIL DO x := x.left END;
	RETURN x
END MinNode;

(* Return rightmost node *)
PROCEDURE MaxNode(x : Node) : Node;
BEGIN
	IF x = NIL THEN RETURN NIL END;
	WHILE x.right # NIL DO x := x.right END;
	RETURN x
END MaxNode;

(* Return next node or NIL if last *)
PROCEDURE NextNode(x : Node) : Node;
VAR y : Node;
BEGIN
    IF x = NIL THEN RETURN NIL END;
	IF x.right # NIL THEN
		RETURN MinNode(x.right)
	END;
	y := x.parent;
	WHILE (y # NIL) & (x = y.right) DO
		x := y;
		y := x.parent
	END;
	RETURN y
END NextNode;

(* Return previous node or NIL if last *)
PROCEDURE PrevNode(x : Node) : Node;
VAR y : Node;
BEGIN
    IF x = NIL THEN RETURN NIL END;
	IF x.left # NIL THEN
		RETURN MaxNode(x.left)
	END;
	y := x.parent;
	WHILE (y # NIL) & (x = y.left) DO
		x := y;
		y := x.parent
	END;
	RETURN y
END PrevNode;

(** Removes all elements from the tree. *)
PROCEDURE (VAR this: Tree) Dispose*;
VAR node, left, right, parent : Node;
BEGIN
	(* Ref : https://codegolf.stackexchange.com/questions/478/free-a-binary-tree *)
    node := this.root;
    parent := NIL;
    WHILE node # NIL DO
        IF node.left # NIL THEN
            left := node.left;
            node.left := parent;
            parent := node;
            node := left;
        ELSIF node.right # NIL THEN
            right := node.right;
            node.left := parent;
            node.right := NIL;
            parent := node;
            node := right;
        ELSE
            IF parent = NIL THEN
                this.dispose(node.element);
                DISPOSE(node);
            ELSE
                LOOP
                    IF parent = NIL THEN EXIT END;
                    this.dispose(node.element);
                    DISPOSE(node);
                    IF parent.right # NIL THEN
                        node := parent.right;
                        parent.right := NIL;
                        EXIT
                    ELSE
                        node := parent;
                        parent := parent.left
                    END
                END;
            END;
        END;
    END;
    this.root := NIL;
    this.size := 0;
END Dispose;

(** Find node equal to element argument. Return NIL if no node is found. *)
PROCEDURE (VAR this-: Tree) FindNode*(element-: Element) : Node;
VAR
	x : Node;
	i : INTEGER;
BEGIN
	x := this.root;
	WHILE x # NIL DO
		i := Compare(element, x.element);
		IF i = 0 THEN RETURN x
		ELSIF i < 0 THEN x := x.left
		ELSE x := x.right END;
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
    node := MinNode(this.root);
    iterator.current := node;
    iterator.duplicate := this.duplicate
END First;

(** Reverse iterator for the tree. *)
PROCEDURE (VAR this-: Tree) Last* (VAR iterator: Iterator);
VAR node: Node;
BEGIN
    iterator.reverse := TRUE;
    iterator.duplicate := this.duplicate;
    node := MaxNode(this.root);
    iterator.current := node
END Last;

(** Forward iterator where element is found. *)
PROCEDURE (VAR this-: Tree) FindForward* (VAR iterator: Iterator; element- : Element);
BEGIN
    iterator.reverse := FALSE;
    iterator.duplicate := this.duplicate;
    iterator.current := this.FindNode(element)
END FindForward;

(** Reverse iterator where element is found. *)
PROCEDURE (VAR this-: Tree) FindReverse* (VAR iterator: Iterator; element- : Element);
BEGIN
    iterator.reverse := TRUE;
    iterator.duplicate := this.duplicate;
    iterator.current := this.FindNode(element)
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
    RETURN FALSE
END Next;

PROCEDURE (VAR this : Tree) RotateLeft(x : Node);
VAR y : Node;
BEGIN
    ASSERT(x.right # NIL);
    y := x.right;
	x.right := y.left;
	IF y.left # NIL THEN
		y.left.parent := x
	END;
	y.parent := x.parent;
	IF x.parent = NIL THEN
		this.root := y
	ELSIF x = x.parent.left THEN
		x.parent.left := y
	ELSE
		x.parent.right := y
	END;
	y.left := x;
	x.parent := y
END RotateLeft;

PROCEDURE (VAR this : Tree) RotateRight(x : Node);
VAR y : Node;
BEGIN
    ASSERT(x.left # NIL);
	y := x.left;
	x.left := y.right;
	IF y.right # NIL THEN
		y.right.parent := x
	END;
	y.parent := x.parent;
    IF x.parent = NIL THEN
		this.root := y
	ELSIF x = x.parent.left THEN
		x.parent.left := y
	ELSE
		x.parent.right := y
	END;
	y.right := x;
	x.parent := y
END RotateRight;

PROCEDURE (VAR this : Tree) InsertFixup(z : Node);
VAR y : Node;
BEGIN
	WHILE (z # this.root) & (z.parent.color = RED) DO
		IF z.parent = z.parent.parent.left THEN
			y := z.parent.parent.right;
			IF (y # NIL) & (y.color = RED) THEN
				z.parent.color := BLACK;
				y.color := BLACK;
				z.parent.parent.color := RED;
				z := z.parent.parent
			ELSE
				IF z = z.parent.right THEN
					z := z.parent;
					this.RotateLeft(z)
				END;
				z.parent.color := BLACK;
				z.parent.parent.color := RED;
				this.RotateRight(z.parent.parent)
			END;
		ELSE
			y := z.parent.parent.left;
			IF (y # NIL) & (y.color = RED) THEN
				z.parent.color := BLACK;
				y.color := BLACK;
				z.parent.parent.color := RED;
				z := z.parent.parent
			ELSE
				IF z = z.parent.left THEN
					z := z.parent;
					this.RotateRight(z)
				END;
				z.parent.color := BLACK;
				z.parent.parent.color := RED;
				this.RotateLeft(z.parent.parent)
			END
		END;
		this.root.color := BLACK
	END
END InsertFixup;

(** Insert or replace element *)
PROCEDURE (VAR this : Tree) Insert*(element-: Element);
VAR
	x, y, z : Node;
	i : INTEGER;
BEGIN
	x := this.root;
	WHILE x # NIL DO
		y := x;
		i := Compare(element, x.element);
		IF i = 0 THEN
			this.duplicate(x.element, element);
			RETURN
		ELSIF i < 0 THEN x := x.left
		ELSE x := x.right END
	END;
	NEW(z);
	z.parent := y; z.color := RED;
	this.duplicate(z.element, element);
	IF y = NIL THEN
		z.color := BLACK;
		this.root := z;
		INC(this.size);
		RETURN
    END;
    i := Compare(z.element, y.element);
	IF i < 0 THEN (* z.element < y.element *)
		y.left := z
	ELSE
		y.right := z
	END;
	this.InsertFixup(z);
	INC(this.size)
END Insert;

PROCEDURE (VAR this : Tree) RemoveFixup(x, parent : Node);
VAR w : Node;
	PROCEDURE NodeColor(node : Node): Color;
	BEGIN RETURN SEL(node # NIL, node.color, BLACK)
	END NodeColor;
BEGIN
	WHILE (x # this.root) & (NodeColor(x) = BLACK) DO
		IF x # NIL THEN parent := x.parent END;
		IF x = parent.left THEN
			w := parent.right;
			IF w.color = RED THEN
				w.color := BLACK;
				parent.color := RED;
				this.RotateLeft(parent);
				w := parent.right
			END;
			IF (NodeColor(w.left) = BLACK) & (NodeColor(w.right) = BLACK) THEN
				w.color := RED;
				x := parent
			ELSE
				IF NodeColor(w.right) = BLACK THEN
					IF w.left # NIL THEN
						w.left.color := BLACK
					END;
					w.color := RED;
					this.RotateRight(w);
					w := parent.right
				END;
				w.color := parent.color;
				parent.color := BLACK;
				IF w.right # NIL THEN
					w.right.color := BLACK
				END;
				this.RotateLeft(parent);
				x := this.root
			END;		
		ELSE
			w := parent.left;
			IF w.color = RED THEN
				w.color := BLACK;
				parent.color := RED;
				this.RotateRight(parent);
				w := parent.left;
			END;
			IF (NodeColor(w.left) = BLACK) & (NodeColor(w.right) = BLACK) THEN
				w.color := RED;
				x := parent
			ELSE
				IF NodeColor(w.left) = BLACK THEN
					IF w.right # NIL THEN
						w.right.color := BLACK
					END;
					w.color := RED;
					this.RotateLeft(w);
					w := parent.left
				END;
				w.color := parent.color;
				parent.color := BLACK;
				IF w.left # NIL THEN
					w.left.color := BLACK
				END;
				this.RotateRight(parent);
				x := this.root
			END		
		END;
	END;
	IF x # NIL THEN x.color := BLACK END
END RemoveFixup;

(** Remove node from tree if found. Return TRUE if item present and it was removed *)
PROCEDURE (VAR this : Tree) Remove*(element- : Element): BOOLEAN;
VAR x, y, z, xparent : Node;
BEGIN
	IF this.root = NIL THEN RETURN FALSE END;
	z := this.FindNode(element);
	IF z = NIL THEN RETURN FALSE END;
	IF (z.left # NIL) & (z.right # NIL) THEN
		y := NextNode(z)
	ELSE
		y := z
	END;
	IF y.left # NIL THEN
		x := y.left
	ELSE
		x := y.right
	END;
	xparent := y.parent;
	IF x # NIL THEN x.parent := xparent END;
	IF y.parent = NIL THEN
		this.root := x
	ELSIF y = y.parent.left THEN
		y.parent.left := x
	ELSE
		y.parent.right := x
	END;
	this.dispose(z.element);
	IF y # z THEN
		z.element := y.element
	END;
	IF y.color = BLACK THEN
		this.RemoveFixup(x, xparent);
	END;
	IF y # z THEN DISPOSE(y)
	ELSE DISPOSE(z) END;
	DEC(this.size);
	RETURN TRUE
END Remove;

END ADTTree.
