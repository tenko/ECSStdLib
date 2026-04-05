(** OS host dependent procedures. *)
MODULE OSHost IN Std;

IMPORT API := Windows IN API, SYSTEM;

IN Std IMPORT Const, Type, ArrayOfChar, ArrayOfByte;

TYPE
    ArgStr = POINTER TO - ARRAY MAX(LENGTH) OF CHAR;
    ADDRESS = SYSTEM.ADDRESS;
    WSET = SYSTEM.SET;
    HANDLE* = API.HANDLE;
    DWORD = API.DWORD;
    LONG = API.LONG;
    DirEntry* = RECORD
        handle* : HANDLE;
        data*: API.WIN32_FIND_DATAA;
        first*: BOOLEAN;
    END;
    DateTime* = RECORD
        year*, month*, day*, hour*, min*, sec*, msec*: INTEGER;
    END;

CONST
    INVALID_HANDLE* = API.INVALID_HANDLE_VALUE;
    STDIN* = -10;
    STDOUT* = -11;
    STDERR* = -12;
    (* Windows error codes *)
    ERROR_SUCCESS = 0;
    ERROR_FILE_NOT_FOUND = 2;
    ERROR_PATH_NOT_FOUND = 3;
    ERROR_ACCESS_DENIED = 5;
    ERROR_SHARING_VIOLATION = 32;
    ERROR_INVALID_NAME = 123;
    ERROR_DIR_NOT_EMPTY = 145;
    ERROR_ALREADY_EXISTS = 183;

VAR ^ argc ["_argc"]: INTEGER;
VAR ^ argv ["_argv"]: POINTER TO - ARRAY MAX(LENGTH) OF ArgStr;

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
    handle := API.GetStdHandle(DWORD(type));
    RETURN handle # INVALID_HANDLE
END StdHandle;

(* Read single key from console without echo. *)
PROCEDURE ConsoleReadKey*(): CHAR;
VAR
    handle : HANDLE;
    mode : DWORD;
    ch : CHAR;
BEGIN
    ch := 00X;
    handle := API.GetStdHandle(API.STD_INPUT_HANDLE);
    IF handle = API.INVALID_HANDLE_VALUE THEN RETURN 00X END;
    IGNORE(API.GetConsoleMode(handle, SYSTEM.ADR(mode)));
    IGNORE(API.SetConsoleMode(handle, 0));
    IGNORE(API.WaitForSingleObject(handle, API.INFINITE));
    IGNORE(API.ReadFile(handle, SYSTEM.ADR(ch), 1, 0, NIL));
    IGNORE(API.SetConsoleMode(handle, mode));
    RETURN ch
END ConsoleReadKey;

(* Open new or existing file with mode flags. Return TRUE on success.*)
PROCEDURE FileOpen*(VAR handle : HANDLE; filename- : ARRAY OF CHAR; mode : SET): BOOLEAN;
VAR
    access, share, attrs, method : DWORD;
    PROCEDURE Or(x, y : DWORD): DWORD;
    BEGIN RETURN DWORD(SET32(x) + SET32(y))
    END Or;
BEGIN
    access := 0;
    share := 0;
    attrs := DWORD(API.FILE_ATTRIBUTE_NORMAL);
    IF mode = {} THEN
        access := DWORD(API.GENERIC_READ);
    ELSE
        IF mode * Const.AccessRead # {} THEN
            share := DWORD(API.FILE_SHARE_READ);
            access := DWORD(API.GENERIC_READ);
        END;
        IF mode * Const.AccessWrite # {} THEN
            access := Or(access, DWORD(API.GENERIC_WRITE));
        END;
    END;
    IF mode * Const.ModeNew # {} THEN
        method := DWORD(API.CREATE_ALWAYS);
    ELSE
        method := DWORD(API.OPEN_EXISTING);
    END;
    handle := API.CreateFileA(SYSTEM.ADR(filename), access, share, NIL, method, attrs, 0);
    RETURN handle # INVALID_HANDLE
END FileOpen;

(* Close file. Return TRUE if success *)
PROCEDURE FileClose*(handle : HANDLE): BOOLEAN;
BEGIN RETURN API.CloseHandle(handle) # 0
END FileClose;

(*
Read from file into buffer.
Return number of bytes actually read or -1 on failure.
*)
PROCEDURE FileRead*(handle : HANDLE; buffer : ADDRESS; len : LENGTH): LENGTH;
VAR nbytes : DWORD;
BEGIN
    nbytes := 0;
    IF API.ReadFile(handle, buffer, DWORD(len), SYSTEM.ADR(nbytes), NIL) # 0 THEN
        RETURN LENGTH(nbytes)
    END;
    RETURN -1;
END FileRead;

(*
Write from file into buffer.
Return number of bytes actually written or -1 on failure.
*)
PROCEDURE FileWrite*(handle : HANDLE; buffer : ADDRESS; len : LENGTH): LENGTH;
VAR nbytes : DWORD;
BEGIN
    nbytes := 0;
    IF API.WriteFile(handle, buffer, DWORD(len), SYSTEM.ADR(nbytes), NIL) # 0 THEN
        RETURN LENGTH(nbytes)
    END;
    RETURN -1;
END FileWrite;

(*
Write from from buffer to std file handle.
Return number of bytes actually written or -1 on failure.
*)
PROCEDURE FileStdWrite*(handle : HANDLE; buffer : ADDRESS; len : LENGTH): LENGTH;
BEGIN RETURN FileWrite(handle, buffer, len)
END FileStdWrite;

(**
Set byte position in file. Return new position or -1 in case of failure.
*)
PROCEDURE FileSeek*(handle : HANDLE; offset : LENGTH; mode : INTEGER): LENGTH;
VAR
    high, low, method : LONG;
    ret : DWORD;
BEGIN
    CASE mode OF
          Const.SeekSet  : method := API.FILE_BEGIN;
        | Const.SeekCur  : method := API.FILE_CURRENT;
        | Const.SeekEnd  : method := API.FILE_END;
    ELSE
        RETURN -1
    END;
    high := LONG(SYSTEM.LSH(HUGECARD(offset), -32));
    low := LONG(WSET(offset) * WSET(0FFFFFFFFH));
    ret := API.SetFilePointer(handle, low, SYSTEM.ADR(high), method);
    IF (ret = 0FFFFFFFFH) & (API.GetLastError() # 0) THEN
        RETURN -1
    END;
    RETURN LENGTH(SYSTEM.LSH(HUGECARD(high), 32) + HUGECARD(ret))
END FileSeek;

(* Return byte position in file or -1 on failure. *)
PROCEDURE FileTell*(handle : HANDLE): LENGTH;
VAR
    high, low : LONG;
    ret : DWORD;
BEGIN
    high := 0; low := 0;
    ret := API.SetFilePointer(handle, low, SYSTEM.ADR(high), API.FILE_CURRENT);
    IF (ret = 0FFFFFFFFH) & (API.GetLastError() # 0) THEN
        RETURN -1
    END;
    RETURN LENGTH(SYSTEM.LSH(HUGECARD(high), 32) + HUGECARD(ret))
END FileTell;

(* Set end of file to current position. *)
PROCEDURE FileSetSize*(handle : HANDLE): BOOLEAN;
BEGIN RETURN API.SetEndOfFile(handle) # 0
END FileSetSize;

(* Truncate file to given size *)
PROCEDURE FileTruncate*(handle : HANDLE; size : LENGTH): LENGTH;
BEGIN
    IF FileSeek(handle, size, Const.SeekSet) = -1 THEN RETURN -1 END;
    IF ~FileSetSize(handle) THEN RETURN -1 END;
    RETURN size
END FileTruncate;

(*
Flush buffered write operations to disk.
Return TRUE on success.
*)
PROCEDURE FileFlush*(handle : HANDLE): BOOLEAN;
BEGIN RETURN API.FlushFileBuffers(handle) # 0
END FileFlush;

(** Check if file exists *)
PROCEDURE FileExists*(filename- : ARRAY OF CHAR): BOOLEAN;
BEGIN RETURN API.GetFileAttributesA(SYSTEM.ADR(filename)) # 0FFFFFFFFH
END FileExists;

(** Try to remove file. Return `TRUE` on success *)
PROCEDURE FileRemove*(filename- : ARRAY OF CHAR): BOOLEAN;
BEGIN RETURN API.DeleteFileA(SYSTEM.ADR(filename)) # 0
END FileRemove;

(** Try to rename file. Return `TRUE` on success *)
PROCEDURE FileRename*(oldname-, newname-: ARRAY OF CHAR): BOOLEAN;
BEGIN RETURN API.MoveFileA(SYSTEM.ADR(oldname), SYSTEM.ADR(newname)) # 0;
END FileRename;

(** Try to get modification time for file. Return `TRUE` on success *)
PROCEDURE FileModificationTime*(VAR time : DateTime; VAR delta : HUGEINT; filename-: ARRAY OF CHAR): BOOLEAN;
VAR
    fh: HANDLE;
    fdata: API.WIN32_FIND_DATAA;
    lftime: API.FILETIME;
    stime: API.SYSTEMTIME;
BEGIN
    fh := API.FindFirstFileA(SYSTEM.ADR(filename), fdata);
    IF fh = INVALID_HANDLE THEN RETURN FALSE END;
    IF API.FileTimeToLocalFileTime(fdata.ftLastWriteTime, lftime) = 0 THEN
        IGNORE(API.FindClose(fh));
        RETURN FALSE
    END;
    IGNORE(API.FindClose(fh));
    IF API.FileTimeToSystemTime(lftime, stime) = 0 THEN
        RETURN FALSE
    END;
    time.year := INTEGER(stime.wYear);
    time.month := INTEGER(stime.wMonth);
    time.day := INTEGER(stime.wDay);
    time.hour := INTEGER(stime.wHour);
    time.min := INTEGER(stime.wMinute);
    time.sec := INTEGER(stime.wSecond);
    time.msec := INTEGER(stime.wMilliseconds);
    delta := -1;
    RETURN TRUE
END FileModificationTime;

(** Open file/directory listing *)
PROCEDURE DirOpen*(VAR dir: DirEntry; name-: ARRAY OF CHAR);
VAR
    s : ARRAY API.MAX_PATH OF CHAR;
    i, len : LENGTH;
BEGIN
    dir.handle := INVALID_HANDLE;
    len := ArrayOfChar.Length(name);
    IF len = 0 THEN s := "*.*"
    ELSE
        ArrayOfChar.Assign(s, name);
        i := len - 1;
        IF s[i] = '\' THEN
            IF len >= API.MAX_PATH - 4 THEN 
                RETURN
            END;
            s[i + 1] := '*';
            s[i + 2] := '.';
            s[i + 3] := '*';
            s[i + 4] := 00X;
        END;
    END;
    dir.handle := API.FindFirstFileA(SYSTEM.ADR(s), dir.data);
    dir.first := TRUE
END DirOpen;

(** Close directory listing *)
PROCEDURE DirClose*(VAR dir: DirEntry);
BEGIN
    IF dir.handle # INVALID_HANDLE THEN
        IGNORE(API.FindClose(dir.handle));
        dir.handle := INVALID_HANDLE
    END;
END DirClose;

(** Return FALSE when end of file/directory listing is reached *)
PROCEDURE DirNext*(VAR dir: DirEntry): BOOLEAN;
VAR
    ret : BOOLEAN;
    handle : HANDLE;
BEGIN
    IF dir.handle # INVALID_HANDLE THEN
        IF dir.first THEN
            dir.first := FALSE;
            RETURN TRUE;
        END;
        handle := dir.handle;
        ret := API.FindNextFileA(handle, dir.data) # 0;
        IF ~ret THEN dir.handle := INVALID_HANDLE END;
        RETURN ret
    END;
    RETURN FALSE
END DirNext;

(** Return length of current directory listing name string *)
PROCEDURE DirNameLength*(VAR dir: DirEntry): LENGTH;
BEGIN
    IF dir.handle # INVALID_HANDLE THEN
        RETURN ArrayOfChar.Length(dir.data.cFileName)
    END;
    RETURN 0
END DirNameLength;

(** Return current directory listing name *)
PROCEDURE DirName*(dir-: DirEntry; VAR name: ARRAY OF CHAR);
VAR i : LENGTH;
BEGIN
    i := 0;
    IF dir.handle # INVALID_HANDLE THEN
        WHILE (i < LEN(name)) & (i < API.MAX_PATH) & (dir.data.cFileName[i] # 00X) DO
            name[i] := dir.data.cFileName[i];
            INC(i)
        END;
    END;
    name[i] := 00X
END DirName;

(** Return TRUE if current entry is a directory *)
PROCEDURE DirIsDir*(dir-: DirEntry): BOOLEAN;
BEGIN
    IF dir.handle # INVALID_HANDLE THEN
        RETURN WSET(dir.data.dwFileAttributes) * WSET(API.FILE_ATTRIBUTE_DIRECTORY) # {}
    END;
    RETURN FALSE;
END DirIsDir;

(** Return TRUE if current entry is a file *)
PROCEDURE DirIsFile*(dir-: DirEntry): BOOLEAN;
BEGIN
    IF dir.handle # INVALID_HANDLE THEN
        RETURN WSET(dir.data.dwFileAttributes) * WSET(API.FILE_ATTRIBUTE_DIRECTORY) = {}
    END;
    RETURN FALSE;
END DirIsFile;

(** Get current local time *)
PROCEDURE GetTime*(VAR time : DateTime; VAR delta : HUGEINT);
VAR stime: API.SYSTEMTIME;
BEGIN
    API.GetLocalTime(stime);
    time.year := INTEGER(stime.wYear);
    time.month := INTEGER(stime.wMonth);
    time.day := INTEGER(stime.wDay);
    time.hour := INTEGER(stime.wHour);
    time.min := INTEGER(stime.wMinute);
    time.sec := INTEGER(stime.wSecond);
    time.msec := INTEGER(stime.wMilliseconds);
    delta := -1;
END GetTime;

(** Get local time UTC offset *)
PROCEDURE GetTimeZoneOffset*(): INTEGER;
VAR tzinfo: API.TIME_ZONE_INFORMATION;
BEGIN
    IF API.GetTimeZoneInformation(tzinfo) = 0FFFFFFFFH THEN
        RETURN 0
    END;
    RETURN INTEGER(SIGNED32(tzinfo.Bias) DIV 60)
END GetTimeZoneOffset;

(** Get string length of the current directory *)
PROCEDURE CDNameLength*(): LENGTH;
BEGIN RETURN LENGTH(API.GetCurrentDirectoryA(0, 0))
END CDNameLength;

(** Get the current directory *)
PROCEDURE CDName*(VAR name: ARRAY OF CHAR; length: LENGTH);
BEGIN IGNORE(API.GetCurrentDirectoryA(DWORD(length), SYSTEM.ADR(name)))
END CDName;

(** Get the current directory *)
PROCEDURE SetCD*(name-: ARRAY OF CHAR): BOOLEAN;
VAR s : ARRAY API.MAX_PATH OF CHAR;
BEGIN
    IF ArrayOfChar.Length(name) > (API.MAX_PATH - 2) THEN
        RETURN FALSE
    END;
    (* Need to make a copy due to the string might be changed *)
    ArrayOfChar.Assign(s, name);
    RETURN API.SetCurrentDirectoryA(SYSTEM.ADR(s[0])) # 0
END SetCD;

(** Try to create directory. Return `TRUE` on success *)
PROCEDURE CreateDirectory*(name-: ARRAY OF CHAR): BOOLEAN;
BEGIN RETURN API.CreateDirectoryA(SYSTEM.ADR(name), NIL) # 0
END CreateDirectory;

(** Try to delete directory. Return `TRUE` on success *)
PROCEDURE RemoveDirectory*(name-: ARRAY OF CHAR): BOOLEAN;
BEGIN RETURN API.RemoveDirectoryA(SYSTEM.ADR(name)) # 0
END RemoveDirectory;

(** Get string length of the environment variable *)
PROCEDURE EnvVarLength*(name-: ARRAY OF CHAR): LENGTH;
BEGIN RETURN LENGTH(API.GetEnvironmentVariableA(SYSTEM.ADR(name), 0, 0))
END EnvVarLength;

(** Get environment variable *)
PROCEDURE EnvVar*(VAR value: ARRAY OF CHAR; name-: ARRAY OF CHAR);
BEGIN
    IGNORE(API.GetEnvironmentVariableA(SYSTEM.ADR(name), SYSTEM.ADR(value), DWORD(LEN(value))))
END EnvVar;

(**
Execute cmd with arguments.
STDIN, STDOUT & STDERR is redirected to /dev/null.
Returns 0 on success.
*)
PROCEDURE ExecuteArgs*(cmd- : ARRAY OF CHAR; args- : ARRAY OF Type.STRING): INTEGER;
CONST
    STARTF_USESTDHANDLES = 00000100H;
VAR
    si : API.STARTUPINFO;
    pi : API.PROCESS_INFORMATION;
    sa : API.SECURITY_ATTRIBUTES;
    filename : ARRAY 8 OF CHAR;
    hNul : API.HANDLE;
    str : Type.STRING;
    i, len, arg, slen : LENGTH;
BEGIN
    len := ArrayOfChar.Length(cmd);
    IF len = 0 THEN RETURN -1 END;
    arg := 0;
    FOR i := 0 TO LEN(args) - 1 DO
        IF args[i] # NIL THEN
            slen := ArrayOfChar.Length(args[i]^);
            INC(len, slen + 1);
            INC(arg, slen);
        END;
    END;
    NEW(str, len + 3);
    IF str = NIL THEN RETURN -1 END;
    str[0] := 00X;
    ArrayOfChar.AppendChar(str^, '"');
    ArrayOfChar.Append(str^, cmd);
    ArrayOfChar.AppendChar(str^, '"');
    IF arg > 0 THEN
        FOR i := 0 TO LEN(args) - 1 DO
            IF args[i] # NIL THEN
                ArrayOfChar.AppendChar(str^, ' ');
                ArrayOfChar.Append(str^, args[i]^)
            END;
        END;
    END;
    sa.nLength := SIZE(API.SECURITY_ATTRIBUTES);
    sa.lpSecurityDescriptor := 0;
    sa.bInheritHandle := 1;
    filename := "NUL";
    hNul := API.CreateFileA(SYSTEM.ADR(filename[0]),  API.DWORD(API.GENERIC_READ + API.GENERIC_WRITE),
                            API.DWORD(API.FILE_SHARE_READ + API.FILE_SHARE_WRITE), 
                           SYSTEM.VAL(API.LPSECURITY_ATTRIBUTES, SYSTEM.ADR(sa)), API.OPEN_EXISTING, 0, 0);
    ArrayOfByte.Zero(si);
    ArrayOfByte.Zero(pi);
    si.cb := SIZE(API.STARTUPINFO);
    si.dwFlags := STARTF_USESTDHANDLES;
    si.hStdOutput := hNul;
    si.hStdError := hNul;
    si.hStdInput := hNul;
    IF ~API.CreateProcessA(0, SYSTEM.ADR(str^[0]), SYSTEM.VAL(API.LPSECURITY_ATTRIBUTES, 0),
                           SYSTEM.VAL(API.LPSECURITY_ATTRIBUTES, 0), 1, 0, 0, 0,
                           SYSTEM.VAL(API.LPSTARTUPINFO, SYSTEM.ADR(si)), SYSTEM.VAL(API.LPPROCESS_INFORMATION, SYSTEM.ADR(pi))) THEN
        DISPOSE(str);
        RETURN -1
    END;
    IGNORE(API.WaitForSingleObject(pi.hProcess, API.INFINITE));
    IGNORE(API.CloseHandle(hNul));
    IGNORE(API.CloseHandle(pi.hProcess));
    IGNORE(API.CloseHandle(pi.hThread));
    DISPOSE(str);
    RETURN 0;
END ExecuteArgs;

(**
Execute cmd with arguments and capture STDOUT & STDERR.
STDIN is redirected to /dev/null.
Returns 0 on success.
*)
PROCEDURE ExecuteWithCapture*(cmd- : ARRAY OF CHAR; args- : ARRAY OF Type.STRING; VAR fh : Type.Stream): INTEGER;
CONST
    STARTF_USESTDHANDLES = 0100H;
    HANDLE_FLAG_INHERIT = 01H;
VAR
    si : API.STARTUPINFO;
    pi : API.PROCESS_INFORMATION;
    sa : API.SECURITY_ATTRIBUTES;
    buffer : ARRAY 64 OF CHAR;
    hNul, hPipeRd, hPipeWr : API.HANDLE;
    nbytes : API.DWORD;
    str : Type.STRING;
    i, len, arg, slen : LENGTH;
BEGIN
    len := ArrayOfChar.Length(cmd);
    IF len = 0 THEN RETURN -1 END;
    arg := 0;
    FOR i := 0 TO LEN(args) - 1 DO
        IF args[i] # NIL THEN
            slen := ArrayOfChar.Length(args[i]^);
            INC(len, slen + 1);
            INC(arg, slen);
        END;
    END;
    NEW(str, len + 3);
    IF str = NIL THEN RETURN -1 END;
    str[0] := 00X;
    ArrayOfChar.AppendChar(str^, '"');
    ArrayOfChar.Append(str^, cmd);
    ArrayOfChar.AppendChar(str^, '"');
    IF arg > 0 THEN
        FOR i := 0 TO LEN(args) - 1 DO
            IF args[i] # NIL THEN
                ArrayOfChar.AppendChar(str^, ' ');
                ArrayOfChar.Append(str^, args[i]^)
            END;
        END;
    END;
    sa.nLength := SIZE(API.SECURITY_ATTRIBUTES);
    sa.lpSecurityDescriptor := 0;
    sa.bInheritHandle := 1;
    buffer := "NUL";
    hNul := API.CreateFileA(SYSTEM.ADR(buffer[0]),  API.DWORD(API.GENERIC_READ + API.GENERIC_WRITE),
                            API.DWORD(API.FILE_SHARE_READ + API.FILE_SHARE_WRITE), 
                            SYSTEM.VAL(API.LPSECURITY_ATTRIBUTES, SYSTEM.ADR(sa)), API.OPEN_EXISTING, 0, 0);
    
    IGNORE(API.CreatePipe(SYSTEM.ADR(hPipeRd), SYSTEM.ADR(hPipeWr), SYSTEM.VAL(API.LPSECURITY_ATTRIBUTES, SYSTEM.ADR(sa)), 0));
    IGNORE(API.SetHandleInformation(hPipeRd, HANDLE_FLAG_INHERIT, 0));
    
    ArrayOfByte.Zero(si);
    ArrayOfByte.Zero(pi);
    si.cb := SIZE(API.STARTUPINFO);
    si.dwFlags := STARTF_USESTDHANDLES;
    si.hStdOutput := hPipeWr;
    si.hStdError := hPipeWr;
    si.hStdInput := hNul;
    IF ~API.CreateProcessA(0, SYSTEM.ADR(str^[0]), SYSTEM.VAL(API.LPSECURITY_ATTRIBUTES, 0),
                           SYSTEM.VAL(API.LPSECURITY_ATTRIBUTES, 0), 1, 0, 0, 0,
                           SYSTEM.VAL(API.LPSTARTUPINFO, SYSTEM.ADR(si)), SYSTEM.VAL(API.LPPROCESS_INFORMATION, SYSTEM.ADR(pi))) THEN
        DISPOSE(str);
        RETURN -1
    END;
    IGNORE(API.CloseHandle(hPipeWr));
    WHILE API.ReadFile(hPipeRd, SYSTEM.ADR(buffer), API.DWORD(LEN(buffer)), SYSTEM.ADR(nbytes), NIL) # 0 DO
        IGNORE(fh.WriteBytes(buffer, 0, nbytes));
    END;
    IGNORE(API.WaitForSingleObject(pi.hProcess, API.INFINITE));
    IGNORE(API.CloseHandle(hPipeRd));
    IGNORE(API.CloseHandle(hNul));
    IGNORE(API.CloseHandle(pi.hProcess));
    IGNORE(API.CloseHandle(pi.hThread));
    DISPOSE(str);
    RETURN 0;
END ExecuteWithCapture;
    
(** Exit with return code *)
PROCEDURE Exit*(code : INTEGER);
BEGIN API.ExitProcess(code)
END Exit;

(* Get last error code or OK on no error. *)
PROCEDURE GetLastError*(VAR error: INTEGER);
VAR winerr : DWORD;
BEGIN 
    winerr := API.GetLastError();
    CASE winerr OF
          ERROR_SUCCESS : error := Const.OK
        | ERROR_FILE_NOT_FOUND, ERROR_INVALID_NAME : error := Const.ErrorFileNotFound
        | ERROR_PATH_NOT_FOUND : error := Const.ErrorPathNotFound
        | ERROR_ACCESS_DENIED : error := Const.ErrorNoAccess
        | ERROR_SHARING_VIOLATION : error := Const.ErrorLocked
        | ERROR_DIR_NOT_EMPTY : error := Const.ErrorDirNotEmpty
        | ERROR_ALREADY_EXISTS : error := Const.ErrorAlreadyExists
    ELSE
        error := Const.ErrorUnknown
    END;
END GetLastError;

BEGIN
    IGNORE(API.SetConsoleOutputCP(65001)); (* UTF8 *)
END OSHost.