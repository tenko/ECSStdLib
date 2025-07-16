(** OS host dependent procedures. *)
MODULE OSHost IN Std;

IMPORT API := Linux IN API, SYSTEM;
IN Std IMPORT Const, ArrayOfChar;

CONST
    INVALID_HANDLE* = -1;
    STDIN* = 0;
    STDOUT* = 1;
    STDERR* = 2;
    OFSRECLEN = 16;
    OFSTYPE = 18;
    OFSSTR = 19;

TYPE
    HANDLE* = INTEGER;
    DirEntry *= RECORD
        handle : HANDLE;
        data : POINTER TO ARRAY OF SYSTEM.BYTE;
        size, idx : LENGTH;
        adr : SYSTEM.ADDRESS;
    END;
    DateTime* = RECORD
        year*, month*, day*, hour*, min*, sec*, msec*: INTEGER;
    END;
    ArgStr = POINTER TO - ARRAY MAX(LENGTH) OF CHAR;
    ADDRESS = SYSTEM.ADDRESS;
    Stat = RECORD-
    	st_dev: LENGTH;
    	st_ino: LENGTH;
    	st_mode: INTEGER;
    	st_nlink: LENGTH;
    	st_uid: INTEGER;
    	st_gid: INTEGER;
    	st_rdev: LENGTH;
    	st_size: LENGTH;
    	st_blksize: LENGTH;
    	st_blocks: LENGTH;
    	st_atime: LENGTH;
    	st_mtime: LENGTH;
    	st_ctime: LENGTH;
    	pad : ARRAY 64 OF SYSTEM.BYTE;
    END;

VAR
    errno : INTEGER;

VAR ^ argc ["_argc"]: INTEGER;
VAR ^ argv ["_argv"]: POINTER TO - ARRAY MAX(LENGTH) OF ArgStr;
PROCEDURE ^ GetEnv ["getenv"] (addr: SYSTEM.ADDRESS): SYSTEM.ADDRESS;

PROCEDURE CheckForError(error : INTEGER);
BEGIN
    IF (error < 0) & (errno # 0) THEN
        errno := ABS(error)
    END;
END CheckForError;

(**
Get length of program name string
*)
PROCEDURE ProgramNameLength*(): LENGTH;
BEGIN
    IF (argv # NIL) & (argv[0] # NIL) THEN
        RETURN ArrayOfChar.Length(argv[0]^)
    END;
    RETURN 0
END ProgramNameLength;

(**
Get program name
*)
PROCEDURE ProgramName*(VAR name : ARRAY OF CHAR);
BEGIN
    IF (argv # NIL) & (argv[0] # NIL) THEN
        ArrayOfChar.Assign(name, argv[0]^)
    ELSE
        ArrayOfChar.Clear(name)
    END
END ProgramName;

(**
Get number of program arguments
*)
PROCEDURE Args*(): LENGTH;
BEGIN RETURN LENGTH(argc)
END Args;

(**
Get length of argument string
*)
PROCEDURE ArgLength*(n : LENGTH): LENGTH;
BEGIN
    IF (argv # NIL) & (n < argc) & (argv[n] # NIL) THEN
        RETURN ArrayOfChar.Length(argv[n]^)
    END;
    RETURN 0
END ArgLength;

(**
Get n-th argument
*)
PROCEDURE Arg*(VAR str : ARRAY OF CHAR; n : LENGTH);
BEGIN
    IF (argv # NIL) & (n < argc) & (argv[n] # NIL) THEN
        ArrayOfChar.Assign(str, argv[n]^)
    ELSE
        ArrayOfChar.Clear(str)
    END
END Arg;

(* Open one of the standard streams STDING, STDOUT or STDERR. Return TRUE on success.*)
PROCEDURE StdHandle*(VAR handle : HANDLE; type : INTEGER): BOOLEAN;
BEGIN
    IF (type # STDIN) &  (type # STDOUT) & (type # STDERR) THEN
        handle := INVALID_HANDLE;
        RETURN FALSE
    END;
    handle := type;
    RETURN TRUE
END StdHandle;

(* Open new or existing file with mode flags. Return TRUE on success.*)
PROCEDURE FileOpen*(VAR handle : HANDLE; filename- : ARRAY OF CHAR; mode : SET): BOOLEAN;
VAR
    flags, mode_t: INTEGER;
BEGIN
    flags := -1;
    mode_t := 0;
    IF mode = {} THEN
        flags := API.O_RDONLY;
    ELSE
        IF mode * Const.AccessWrite # {} THEN
            IF mode * Const.AccessRead # {} THEN
                flags := API.O_RDWR;
            ELSE
                flags := API.O_WRONLY;
            END;
        ELSIF mode * Const.AccessRead # {} THEN
            flags := API.O_RDONLY;
        END;
    END;
    IF flags = -1 THEN
        handle := INVALID_HANDLE;
        RETURN FALSE;
    END;
    IF mode * Const.AccessWrite # {} THEN
        flags := flags + API.O_CREAT;
        IF  mode * Const.ModeNew # {} THEN
            flags := flags + API.O_TRUNC;
        END;
        mode_t := 644O;
    END;
    handle := API.Open(SYSTEM.ADR(filename[0]), flags, mode_t);
    IF handle < 0 THEN
        CheckForError(handle);
        handle := INVALID_HANDLE;
        RETURN FALSE
    END;
    RETURN TRUE
END FileOpen;

(* Close file. Return TRUE if success *)
PROCEDURE FileClose*(handle : HANDLE): BOOLEAN;
VAR ret : INTEGER;
BEGIN
    ret := API.Close(handle);
    IF ret < 0 THEN
        CheckForError(ret);
        ret := -1
    END;
    RETURN ret >= 0
END FileClose;

(*
Read from file into buffer.
Return number of bytes actually read or -1 on failure.
*)
PROCEDURE FileRead*(handle : HANDLE; buffer : ADDRESS; len : LENGTH): LENGTH;
VAR ret : LENGTH;
BEGIN
    ret := API.Read(handle, buffer, len);
    IF ret < 0 THEN
        CheckForError(INTEGER(ret));
        ret := -1
    END;
    RETURN ret
END FileRead;

(*
Write from file into buffer.
Return number of bytes actually written or -1 on failure.
*)
PROCEDURE FileWrite*(handle : HANDLE; buffer : ADDRESS; len : LENGTH): LENGTH;
VAR ret : LENGTH;
BEGIN
    ret := API.Write(handle, buffer, len);
    IF ret < 0 THEN
        CheckForError(INTEGER(ret));
        ret := -1
    END;
    RETURN ret
END FileWrite;

(*
Write from from buffer to std file handle.
Return number of bytes actually written or -1 on failure.
*)
PROCEDURE FileStdWrite*(handle : HANDLE; buffer : ADDRESS; len : LENGTH): LENGTH;
VAR ret : LENGTH;
BEGIN
    ret := API.Write(handle, buffer, len);
    IF ret < 0 THEN
        CheckForError(INTEGER(ret));
        ret := -1
    END;
    RETURN ret
END FileStdWrite;

(**
Set byte position in file. Return new position or -1 in case of failure.
*)
PROCEDURE FileSeek*(handle : HANDLE; offset : LENGTH; mode : INTEGER): LENGTH;
VAR
    mod : INTEGER;
    ret : LENGTH;
BEGIN
    CASE mode OF
          Const.SeekSet  : mod := API.SEEK_SET;
        | Const.SeekCur  : mod := API.SEEK_CUR;
        | Const.SeekEnd  : mod := API.SEEK_END;
    ELSE
        RETURN -1
    END;
    ret := API.LSeek(handle, offset, mod);
    IF ret < 0 THEN
        CheckForError(INTEGER(ret));
        RETURN -1
    END;
    RETURN ret
END FileSeek;

(* Return byte position in file or -1 on failure. *)
PROCEDURE FileTell*(handle : HANDLE): LENGTH;
VAR ret : LENGTH;
BEGIN
    ret := API.LSeek(handle, 0, API.SEEK_CUR);
    IF ret < 0 THEN
        CheckForError(INTEGER(ret));
        RETURN -1
    END;
    RETURN ret
END FileTell;

(* Set end of file to current position. *)
PROCEDURE FileSetSize*(handle : HANDLE): BOOLEAN;
VAR size : LENGTH;
BEGIN
    size := FileTell(handle);
    IF size < 0 THEN
        CheckForError(INTEGER(size));
        RETURN FALSE
    END;
    IF API.FTruncate(handle, size) # 0 THEN RETURN FALSE END;
    RETURN TRUE
END FileSetSize;

(* Truncate file to given size *)
PROCEDURE FileTruncate*(handle : HANDLE; size : LENGTH): LENGTH;
VAR ret : INTEGER;
BEGIN
    ret := API.FTruncate(handle, size);
    IF ret < 0 THEN
        CheckForError(ret);
        RETURN -1
    END;
    IF ret # 0 THEN RETURN -1 END;
    RETURN size
END FileTruncate;

(*
Flush buffered write operations to disk.
Return TRUE on success.
*)
PROCEDURE FileFlush*(handle : HANDLE): BOOLEAN;
BEGIN RETURN TRUE
END FileFlush;

(** Check if file exists *)
PROCEDURE FileExists*(filename- : ARRAY OF CHAR): BOOLEAN;
VAR
    stat : Stat;
    ret : INTEGER;
BEGIN
    ret := API.Stat(SYSTEM.ADR(filename[0]), SYSTEM.ADR(stat));
    CheckForError(ret);
    RETURN ret = 0
END FileExists;

(** Try to remove file. Return `TRUE` on success *)
PROCEDURE FileRemove*(filename- : ARRAY OF CHAR): BOOLEAN;
VAR ret : INTEGER;
BEGIN
    ret := API.Unlink(SYSTEM.ADR(filename[0]));
    CheckForError(ret);
    RETURN ret = 0
END FileRemove;

(** Try to rename file. Return `TRUE` on success *)
PROCEDURE FileRename*(oldname-, newname-: ARRAY OF CHAR): BOOLEAN;
VAR ret : INTEGER;
BEGIN
    ret := API.Rename(SYSTEM.ADR(oldname[0]), SYSTEM.ADR(newname[0]));
    CheckForError(ret);
    RETURN ret = 0
END FileRename;

(** Try to get modification time for file. Return `TRUE` on success *)
PROCEDURE FileModificationTime*(VAR time : DateTime; VAR delta : HUGEINT; filename-: ARRAY OF CHAR): BOOLEAN;
VAR
    stat : Stat;
    ret : INTEGER;
BEGIN
    ret := API.Stat(SYSTEM.ADR(filename[0]), SYSTEM.ADR(stat));
    CheckForError(ret);
    IF ret # 0 THEN RETURN FALSE END;
    time.year := 1970;
    time.month := 1;
    time.day := 1;
    time.hour := 0;
    time.min := 0;
    time.sec := 0;
    time.msec := 0;
    delta := stat.st_mtime;
    RETURN TRUE
END FileModificationTime;

(** Open file/directory listing *)
PROCEDURE DirOpen*(VAR dir: DirEntry; name-: ARRAY OF CHAR);
VAR sname : ARRAY 2 OF CHAR;
BEGIN
    IF ArrayOfChar.Length(name) = 0 THEN
        sname := '.';
        dir.handle := API.Open(SYSTEM.ADR(sname[0]), API.O_RDONLY + API.O_DIRECTORY, 0);
    ELSE
        dir.handle := API.Open(SYSTEM.ADR(name[0]), API.O_RDONLY + API.O_DIRECTORY, 0);
    END;
    IF dir.handle < 0 THEN
        CheckForError(dir.handle);
        dir.handle := INVALID_HANDLE;
        RETURN
    END;
    NEW(dir.data, 4096);
    dir.adr := SYSTEM.ADR(dir.data[0]);
    dir.size := -1;
END DirOpen;

(** Close directory listing *)
PROCEDURE DirClose*(VAR dir: DirEntry);
VAR ret : INTEGER;
BEGIN
    IF dir.handle # INVALID_HANDLE THEN
        ret := API.Close(dir.handle);
        CheckForError(ret);
        dir.handle := INVALID_HANDLE
    END;
    IF dir.data # NIL THEN
        DISPOSE(dir.data);
    END;
    dir.size := -1;
END DirClose;

(** Return FALSE when end of file/directory listing is reached *)
PROCEDURE DirNext*(VAR dir: DirEntry): BOOLEAN;
VAR
    reclen : UNSIGNED16;
BEGIN
    IF dir.handle # INVALID_HANDLE THEN
        IF (dir.size = -1) OR (dir.idx >= dir.size) THEN
            dir.size := API.GetDents(dir.handle, dir.adr, 4096);
            IF (dir.size = -1) OR (dir.size = 0) THEN
                dir.size := -1;
                RETURN FALSE
            END;
            dir.idx := 0;
        ELSE
            SYSTEM.GET(dir.adr + dir.idx + OFSRECLEN, reclen);
            INC(dir.idx, reclen);
            IF dir.idx >= dir.size THEN
                dir.size := API.GetDents(dir.handle, dir.adr, 4096);
                IF (dir.size = -1) OR (dir.size = 0) THEN
                    dir.size := -1;
                    RETURN FALSE
                END;
                dir.idx := 0;
            END;
        END;
        RETURN TRUE
    END;
    RETURN FALSE
END DirNext;

(** Return length of current directory listing name string *)
PROCEDURE DirNameLength*(VAR dir: DirEntry): LENGTH;
VAR
    c : CHAR;
    i, idx : LENGTH;
BEGIN
    IF (dir.handle # INVALID_HANDLE) & (dir.size # -1) THEN
        i := 0;
        idx := dir.idx + OFSSTR;
        WHILE idx + i < dir.size DO
            SYSTEM.GET(dir.adr + idx + i, c);
            IF c = 00X THEN RETURN i END;
            INC(i);
        END;
    END;
    RETURN 0
END DirNameLength;

(** Return current directory listing name *)
PROCEDURE DirName*(dir-: DirEntry; VAR name: ARRAY OF CHAR);
VAR
    c : CHAR;
    i, idx : LENGTH;
BEGIN
    i := 0;
    IF (dir.handle # INVALID_HANDLE) & (dir.size # -1) THEN
        idx := dir.idx + OFSSTR;
        LOOP
            IF (idx + i > dir.size) OR (i >= LEN(name) - 1) THEN EXIT END;
            SYSTEM.GET(dir.adr + idx + i, c);
            IF c = 00X THEN EXIT END;
            name[i] := c;
            INC(i);
        END;
    END;
    name[i] := 00X
END DirName;

(** Return TRUE if current entry is a directory *)
PROCEDURE DirIsDir*(dir-: DirEntry): BOOLEAN;
VAR
    stat : Stat;
    ret : INTEGER;
BEGIN
    IF (dir.handle # INVALID_HANDLE) & (dir.size # -1) THEN
        ret := API.Stat(dir.adr + dir.idx + OFSSTR, SYSTEM.ADR(stat));
        CheckForError(ret);
        IF ret # 0 THEN
            RETURN FALSE
        END;
        RETURN stat.st_mode = 2;
    END;
    RETURN FALSE;
END DirIsDir;

(** Return TRUE if current entry is a file *)
PROCEDURE DirIsFile*(dir-: DirEntry): BOOLEAN;
VAR
    stat : Stat;
    ret : INTEGER;
BEGIN
    IF (dir.handle # INVALID_HANDLE) & (dir.size # -1) THEN
        ret := API.Stat(dir.adr + dir.idx + OFSSTR, SYSTEM.ADR(stat));
        CheckForError(ret);
        IF ret # 0 THEN
            RETURN FALSE
        END;
        RETURN stat.st_mode = 1;
    END;
    RETURN FALSE;
END DirIsFile;

(** Get current local time *)
PROCEDURE GetTime*(VAR time : DateTime; VAR delta : HUGEINT);
BEGIN
    time.year := 1970;
    time.month := 1;
    time.day := 1;
    time.hour := 0;
    time.min := 0;
    time.sec := 0;
    time.msec := 0;
    delta := API.Time(0);
    IF delta < 0 THEN
        CheckForError(INTEGER(delta));
        delta := -1;
    END;
END GetTime;

(** Get local time UTC offset *)
PROCEDURE GetTimeZoneOffset*(): INTEGER;
BEGIN RETURN 0
END GetTimeZoneOffset;

(** Get string length of the current directory *)
PROCEDURE CDNameLength*(): LENGTH;
VAR
    str : ARRAY 64 OF CHAR;
    arr : POINTER TO ARRAY OF CHAR;
    len : LENGTH;
    ret : ADDRESS;
BEGIN
    ret := API.GetCWD(SYSTEM.ADR(str[0]), 64);
    IF ret < 0 THEN
        CheckForError(INTEGER(ret));
        RETURN 0
    END;
    IF ret = 0 THEN
        NEW(arr, 4096);
        IF arr = NIL THEN RETURN -1 END;
        ret := API.GetCWD(SYSTEM.ADR(arr^[0]), 4096);
        IF ret <= 0 THEN
            CheckForError(INTEGER(ret));
            RETURN 0
        END;
        len := ArrayOfChar.Length(arr^);
        DISPOSE(arr);
        RETURN len
    END;
    RETURN ArrayOfChar.Length(str)
END CDNameLength;

(** Get the current directory *)
PROCEDURE CDName*(VAR name: ARRAY OF CHAR; length: LENGTH);
VAR ret : ADDRESS;
BEGIN
    ret := API.GetCWD(SYSTEM.ADR(name[0]), length);
    IF ret < 0 THEN CheckForError(INTEGER(ret)) END;
END CDName;

(** Get the current directory *)
PROCEDURE SetCD*(name-: ARRAY OF CHAR): BOOLEAN;
VAR ret : INTEGER;
BEGIN
    ret := API.ChDir(SYSTEM.ADR(name[0]));
    IF ret < 0 THEN CheckForError(ret) END;
    RETURN ret = 0
END SetCD;

(** Try to create directory. Return `TRUE` on success *)
PROCEDURE CreateDirectory*(name-: ARRAY OF CHAR): BOOLEAN;
VAR ret : INTEGER;
BEGIN
    ret := API.MkDir(SYSTEM.ADR(name[0]), 755O);
    IF ret < 0 THEN CheckForError(ret) END;
    RETURN ret = 0
END CreateDirectory;

(** Try to delete directory. Return `TRUE` on success *)
PROCEDURE RemoveDirectory*(name-: ARRAY OF CHAR): BOOLEAN;
VAR ret : INTEGER;
BEGIN
    ret := API.RmDir(SYSTEM.ADR(name[0]));
    IF ret < 0 THEN CheckForError(ret) END;
    RETURN ret = 0
END RemoveDirectory;

(** Get string length of the environment variable *)
PROCEDURE EnvVarLength*(name-: ARRAY OF CHAR): LENGTH;
VAR
    adr : ADDRESS;
    c : CHAR;
    i : LENGTH;
BEGIN
    adr := GetEnv(SYSTEM.ADR(name[0]));
    IF adr < 0 THEN CheckForError(INTEGER(adr)) END;
    IF adr <= 0 THEN RETURN 0 END;
    i := 0;
    LOOP
        SYSTEM.GET(adr + i, c);
        IF c = 00X THEN EXIT END;
        INC(i);
    END;
    RETURN i;
END EnvVarLength;

(** Get environment variable *)
PROCEDURE EnvVar*(VAR value: ARRAY OF CHAR; name-: ARRAY OF CHAR);
VAR
    adr : ADDRESS;
    c : CHAR;
    i : LENGTH;
BEGIN
    adr := GetEnv(SYSTEM.ADR(name[0]));
    IF adr < 0 THEN CheckForError(INTEGER(adr)) END;
    IF adr = 0 THEN
        value[0] := 00X;
        RETURN
    END;
    i := 0;
    LOOP
        SYSTEM.GET(adr + i, c);
        IF (c = 00X) OR (i >= LEN(value) - 1) THEN EXIT END;
        value[i] := c;
        INC(i);
    END;
    value[i] := 00X;
END EnvVar;

(** Exit with return code *)
PROCEDURE Exit*(code : INTEGER);
BEGIN API.Exit(code)
END Exit;

(* Get last error code or OK on no error. *)
PROCEDURE GetLastError*(VAR error: INTEGER);
CONST
    EPERM       = 1;
    ENOENT      = 2;
    EACCES      = 13;
    EEXIST      = 17;
    ENOTDIR     = 20;
    EINVAL      = 22;
    ENOTEMPTY   = 39;
BEGIN
    CASE errno OF
          0 : error := Const.OK
        | ENOENT, EINVAL : error := Const.ErrorFileNotFound
        | ENOTDIR : error := Const.ErrorPathNotFound
        | EACCES : error := Const.ErrorNoAccess
        | EPERM : error := Const.ErrorLocked
        | ENOTEMPTY : error := Const.ErrorDirNotEmpty
        | EEXIST : error := Const.ErrorAlreadyExists
    ELSE
        error := Const.ErrorUnknown
    END;
END GetLastError;

BEGIN
    argc := -1;
    errno := 0;
END OSHost.
