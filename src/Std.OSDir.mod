(** Module for operating on OS directories *)
MODULE OSDir IN Std;

IN Std IMPORT String, OSHost;

TYPE
    Dir* = RECORD (OSHost.DirEntry) END;

(** Open directory listing *)
PROCEDURE (VAR d : Dir) Open*(name- : ARRAY OF CHAR);
BEGIN OSHost.DirOpen(d, name)
END Open;

(** Close directory listing *)
PROCEDURE (VAR d : Dir) Close*();
BEGIN OSHost.DirClose(d)
END Close;

(** Return FALSE when end of file/directory listing is reached *)
PROCEDURE (VAR d : Dir) Next*(): BOOLEAN;
BEGIN RETURN OSHost.DirNext(d);
END Next;

(** Get current name *)
PROCEDURE (VAR d : Dir) Name*(VAR name : String.STRING);
BEGIN
    IF OSHost.DirNameLength(d) > 0 THEN
        String.Reserve(name, OSHost.DirNameLength(d) + 1, FALSE);
        OSHost.DirName(d, name^)
    ELSE
        String.Assign(name, '');
    END;
END Name;

(** Get current name *)
PROCEDURE (VAR d : Dir) IsDir*(): BOOLEAN;
BEGIN RETURN OSHost.DirIsDir(d)
END IsDir;

(** Get current directory name *)
PROCEDURE Current*(VAR name : String.STRING);
BEGIN
    IF OSHost.CDNameLength() = 0 THEN RETURN END;
    String.Reserve(name, OSHost.CDNameLength() + 1, FALSE);
    OSHost.CDName(name^, LEN(name^))
END Current;

(** Set current directory name *)
PROCEDURE SetCurrent*(name- : ARRAY OF CHAR): BOOLEAN;
BEGIN RETURN OSHost.SetCD(name)
END SetCurrent;

(** Try to create directory. Return `TRUE` on success *)
PROCEDURE Create*(name-: ARRAY OF CHAR): BOOLEAN;
BEGIN RETURN OSHost.CreateDirectory(name)
END Create;

(** Try to delete directory. Return `TRUE` on success *)
PROCEDURE Delete*(name-: ARRAY OF CHAR): BOOLEAN;
BEGIN RETURN OSHost.RemoveDirectory(name);
END Delete;

END OSDir.