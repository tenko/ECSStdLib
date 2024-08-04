MODULE TestADTTree;
IN Std IMPORT Testing := O2Testing, String, S := ArrayOfChar;

IMPORT ADTTree(String.STRING, String.Compare) IN Std;

TYPE
    TEST = Testing.TEST;

CONST
    M = "TestADTTree";

PROCEDURE Run* (VAR test: TEST);
VAR
    e : String.STRING;
    tree : ADTTree.Tree;
    it : ADTTree.Iterator;
    ret : BOOLEAN;

    PROCEDURE Assert(b: BOOLEAN; id: LONGINT) ;
    BEGIN
        Testing.Assert(test, b, M, id);
    END Assert ;
BEGIN
    Testing.Begin(test, M);

    tree.Init();
    (* dictionary handles copy of elements *)
    tree.duplicate := String.Duplicate;
    tree.dispose := String.Dispose;

    Assert(tree.Size() = 0, __LINE__);
    tree.Insert(String.S(e, "ab"));
    tree.Insert(String.S(e, "cd"));
    tree.Insert(String.S(e, "12"));
    Assert(tree.Size() = 3, __LINE__);
    Assert(tree.HasElement(String.S(e, "cd")), __LINE__);
    IGNORE(tree.Remove(String.S(e, "cd")));
    Assert(~tree.HasElement(String.S(e, "cd")), __LINE__);
    tree.Insert(String.S(e, "cd"));
    tree.First(it);
    ret := it.Next(e);
    Assert(ret & (S.Compare(e^, "12") = 0), __LINE__);
    ret := it.Next(e);
    Assert(ret & (S.Compare(e^, "ab") = 0), __LINE__);
    ret := it.Next(e);
    Assert(ret & (S.Compare(e^, "cd") = 0), __LINE__);
    ret := it.Next(e);
    Assert(~ret, __LINE__);
    tree.Last(it);
    ret := it.Next(e);
    Assert(ret & (S.Compare(e^, "cd") = 0), __LINE__);
    ret := it.Next(e);
    Assert(ret & (S.Compare(e^, "ab") = 0), __LINE__);
    ret := it.Next(e);
    Assert(ret & (S.Compare(e^, "12") = 0), __LINE__);
    ret := it.Next(e);
    Assert(~ret, __LINE__);
    String.Dispose(e);
    tree.Dispose();

    Testing.End(test);
END Run;

END TestADTTree.