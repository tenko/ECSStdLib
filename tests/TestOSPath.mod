MODULE TestOSPath;
IMPORT Testing := O2Testing IN Std;
IN Std IMPORT S := ArrayOfChar, Str := String, Pth := OSPath;

TYPE
    TEST = Testing.TEST;

CONST
    M = "TestOSPath";
    SEP = Pth.SEP;

PROCEDURE Run* (VAR test: TEST);
VAR
    s, pth : Str.STRING;
    left, right : ARRAY 32 OF CHAR;

    PROCEDURE Assert(b: BOOLEAN; id: LONGINT) ;
    BEGIN
        Testing.Assert(test, b, M, id);
    END Assert ;
    PROCEDURE A(str- : ARRAY OF CHAR);
    BEGIN Str.Append(s, str)
    END A;
BEGIN
    Testing.Begin(test, M);

    (* DirName *)
    Pth.DirName(pth, " ");
    Assert(pth^ = ".", __LINE__);
    Pth.DirName(pth, "a");
    Assert(pth^ = ".", __LINE__);
    Pth.DirName(pth, SEP);
    Assert(pth^ = SEP, __LINE__);
    Str.Assign(s, 'filename'); A(SEP);
    Pth.DirName(pth, s^);
    Assert(pth^ = ".", __LINE__);
    Str.Assign(s, SEP); A('filename'); A(SEP);
    Pth.DirName(pth, s^);
    Assert(pth^ = SEP, __LINE__);
    Str.Assign(s, SEP); A('filename');
    Pth.DirName(pth, s^);
    Assert(pth^ = SEP, __LINE__);
    Str.Assign(s, 'dir'); A(SEP); A('filename'); A(SEP);
    Pth.DirName(pth, s^);
    Assert(pth^ = 'dir', __LINE__);
    Str.Assign(s, 'dir'); A(SEP); A('filename');
    Pth.DirName(pth, s^);
    Assert(pth^ = 'dir', __LINE__);
    Str.Assign(s, SEP); A('dir'); A(SEP); A('filename');
    Pth.DirName(pth, s^);
    Str.Assign(s, SEP); A('dir');
    Assert(pth^ = s^, __LINE__);

    (* FileName *)
    Pth.FileName(pth, " ");
    Assert(pth^ = "", __LINE__);
    Pth.FileName(pth, "a");
    Assert(pth^ = "a", __LINE__);
    Pth.FileName(pth, "filename");
    Assert(pth^ = "filename", __LINE__);
    Pth.FileName(pth, SEP);
    Assert(pth^ = "", __LINE__);
    Str.Assign(s, 'filename'); A(SEP);
    Pth.FileName(pth, s^);
    Assert(pth^ = "", __LINE__);
    Str.Assign(s, SEP); A('filename'); A(SEP);
    Pth.FileName(pth, s^);
    Assert(pth^ = "", __LINE__);
    Str.Assign(s, SEP); A('filename');
    Pth.FileName(pth, s^);
    Assert(pth^ = "filename", __LINE__);
    Str.Assign(s, 'dir'); A(SEP); A('filename'); A(SEP);
    Pth.FileName(pth, s^);
    Assert(pth^ = "", __LINE__);
    Str.Assign(s, 'dir'); A(SEP); A('filename');
    Pth.FileName(pth, s^);
    Assert(pth^ = "filename", __LINE__);
    Str.Assign(s, SEP); A('dir'); A(SEP); A('filename');
    Pth.FileName(pth, s^);
    Assert(pth^ = "filename", __LINE__);

    (* Join *)
    left := 'path'; right := 'filename';
    Pth.Join(pth, left, right);
    Str.Assign(s, 'path'); A(SEP); A('filename');
    Assert(pth^ = s^, __LINE__);
    left := SEP; S.Append(left, 'path'); S.Append(left, SEP);
    right := SEP; S.Append(right, 'filename');
    Pth.Join(pth, left, right);
    Str.Assign(s, SEP); A('path'); A(SEP); A('filename');
    Assert(pth^ = s^, __LINE__);
    left := SEP; S.Append(left, 'path'); S.Append(left, SEP); S.Append(left, SEP);
    right := SEP; S.Append(right, SEP); S.Append(right, 'filename');
    Pth.Join(pth, left, right);
    Str.Assign(s, SEP); A('path'); A(SEP); A('filename');
    Assert(pth^ = s^, __LINE__);
    left := 'path'; right := "";
    Pth.Join(pth, left, right);
    Str.Assign(s, 'path'); A(SEP);
    Assert(pth^ = s^, __LINE__);
    left := ""; right := "";
    Pth.Join(pth, left, right);
    Assert(pth^ = SEP, __LINE__);

    (* Extension *)
    Str.Assign(s, SEP); A('dir'); A(SEP); A('filename.ext');
    Pth.Extension(pth, s^);
    Assert(pth^ = '.ext', __LINE__);
    Str.Assign(s, SEP); A('dir'); A(SEP); A('filename.');
    Pth.Extension(pth, s^);
    Assert(pth^ = "", __LINE__);
    Str.Assign(s, SEP); A('dir'); A(SEP); A('filename');
    Pth.Extension(pth, s^);
    Assert(pth^ = "", __LINE__);
    Pth.Extension(pth, '.');
    Assert(pth^ = "", __LINE__);
    Pth.Extension(pth, "");
    Assert(pth^ = "", __LINE__);

    (* Match *)
    Assert(Pth.Match('name.c', 'name.c') = TRUE, __LINE__);
    Assert(Pth.Match('name.c', 'name.d') = FALSE, __LINE__);
    Str.Assign(s, 'name.?');
    Assert(Pth.Match('name.c', s^) = TRUE, __LINE__);
    Str.Assign(s, 'dir'); A(SEP); A('name.c');
    Assert(Pth.Match(s^, '*.[ch]') = FALSE, __LINE__);
    right := '*'; S.Append(right, SEP); S.Append(right, '*.[ch]');
    Str.Assign(s, 'dir'); A(SEP); A('name.c');
    Assert(Pth.Match(s^, right) = TRUE, __LINE__);
    Str.Assign(s, 'dir'); A(SEP); A('name.h');
    Assert(Pth.Match(s^, right) = TRUE, __LINE__);
    Str.Assign(s, 'dir'); A(SEP); A('name.d');
    Assert(Pth.Match(s^, right) = FALSE, __LINE__);
    Assert(Pth.Match('name.c', '*.[!a-z]') = FALSE, __LINE__);
    Assert(Pth.Match('name.1', '*.[!a-z]') = TRUE, __LINE__);

    DISPOSE(s); DISPOSE(pth);
    Testing.End(test);
END Run;

END TestOSPath.