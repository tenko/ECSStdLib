(** OS host dependent procedures. *)
MODULE OSHost IN Std;

IMPORT SYSTEM;
IN Std IMPORT Const, Char, ArrayOfChar;

CONST
    INVALID_HANDLE* = -1;
    STDIN* = 0;
    STDOUT* = 1;
    STDERR* = 2;
    DATETIMEOFFSET* = TRUE;
    (* Semihost functions: https://github.com/ARM-software/abi-aa/blob/main/semihosting/semihosting.rst *)
    SYS_CLOSE = 02H;
    SYS_ERRNO = 13H;
    SYS_EXIT = 018H;
    SYS_EXIT_EXTENDED = 20H;
    SYS_FLEN = 0CH;
    SYS_GET_CMDLINE = 15H;
    SYS_OPEN = 01H;
    SYS_READ = 06H;
    SYS_READC = 07H;
    SYS_REMOVE = 0EH;
    SYS_RENAME = 0FH;
    SYS_SEEK = 0AH;
    SYS_WRITEC = 03H;
    SYS_WRITE0 = 04H;
    SYS_WRITE = 05H;
    ADP_Stopped_ApplicationExit = 20026H;
    (* libc errno codes *)
    EOK             = 0;    (* No error *)
    ENOENT	        = 2;    (* No such file or directory *)
    EACCES	        = 13;	(* Permission denied *)
    EEXIST	        = 17;	(* File exists *)
       
TYPE
    ADDRESS = SYSTEM.ADDRESS;
    HANDLE* = INTEGER;
    DirEntry* = RECORD END;
    DateTime* = RECORD
        year*, month*, day*, hour*, min*, sec*, msec*: INTEGER;
    END;

VAR
    argc, argvlen : LENGTH;
    argv : ARRAY 81 OF CHAR;
    buffer : ARRAY 81 OF CHAR;
    idx : LENGTH;

PROCEDURE ^ Putchar ["putchar"] (character: INTEGER): INTEGER;

PROCEDURE SemiHost(op : INTEGER; arg : ADDRESS): INTEGER;
VAR ret : INTEGER;
BEGIN
    SYSTEM.ASM("
        mov     r2, r11
        ldr  	r0, [r2, op]
        ldr     r1, [r2, arg]
        bkpt    0xab
        str	    r0, [r2, ret]
    ");
    RETURN ret
END SemiHost;

(* flush buffer on exit *)
PROCEDURE FlushBuffer;
VAR
    args : ARRAY 1 OF ADDRESS;
BEGIN
    IF idx = 0 THEN RETURN END;
    buffer[idx] := 00X;
    idx := 0;
    IGNORE(SemiHost(SYS_WRITE0, SYSTEM.ADR(buffer)));
END FlushBuffer;

(* Replace abort function in runtime *)
PROCEDURE Abort ["abort"] ();
VAR args : ARRAY 2 OF ADDRESS;
BEGIN
    FlushBuffer;
    args[0] := ADP_Stopped_ApplicationExit;
    args[1] := 0;
    IGNORE(SemiHost(SYS_EXIT_EXTENDED, SYSTEM.ADR(args)));
END Abort;

(* Replace putchar function in runtime *)
PROCEDURE PutChar ["putchar"] (character: INTEGER): INTEGER;
VAR
    ch : CHAR;
BEGIN
    ch := CHR(character);
    IF ch = Char.CR THEN buffer[idx] := ' '
    ELSE buffer[idx] := ch END;
    INC(idx);
    IF (idx >= 79) OR (ch = Char.LF) THEN
            buffer[idx] := 00X;
            idx := 0;
            IGNORE(SemiHost(SYS_WRITE0, SYSTEM.ADR(buffer)));
    END;
    RETURN character
END PutChar;

(* Replace getchar function in runtime *)
PROCEDURE Getchar ["getchar"] (): INTEGER;
VAR args : ARRAY 1 OF ADDRESS;
BEGIN
    args[0] := 0;
    RETURN SemiHost(SYS_READC, SYSTEM.ADR(args))
END Getchar;

(**
Get number of program arguments
*)
PROCEDURE Args*(): LENGTH;
VAR
    args : ARRAY 2 OF ADDRESS;
    i, cnt, ret : INTEGER;
BEGIN
    IF argc = -1 THEN
        argc := 0;
        args[0] := SYSTEM.ADR(argv);
        args[1] := 80;
        ret := SemiHost(SYS_GET_CMDLINE, SYSTEM.ADR(args));
        argvlen := LENGTH(args[1]);
        IF (ret = -1) OR (argvlen = 0) THEN
            RETURN argc
        END;
        i := 0; 
        LOOP
            IF i >= argvlen THEN EXIT END;
            WHILE (i < argvlen) & Char.IsSpace(argv[i]) DO INC(i) END;
            IF i >= argvlen THEN EXIT END;
            cnt := 0;
            WHILE (i < argvlen) & ~Char.IsSpace(argv[i]) DO INC(cnt); INC(i) END;
            IF cnt > 0 THEN INC(argc) END;
            INC(i);
        END;
    END;
    RETURN argc
END Args;

(**
Get length of program name string
*)
PROCEDURE ProgramNameLength*(): LENGTH;
VAR i, cnt : INTEGER;
BEGIN
    IF argc = -1 THEN IGNORE(Args()) END;
    IF argvlen = 0 THEN RETURN 0 END;
    i := 0;
    LOOP
        IF i >= argvlen THEN EXIT END;
        WHILE (i < argvlen) & Char.IsSpace(argv[i]) DO INC(i) END;
        IF i >= argvlen THEN EXIT END;
        cnt := 0;
        WHILE (i < argvlen) & ~Char.IsSpace(argv[i]) DO INC(cnt); INC(i) END;
        IF cnt > 0 THEN RETURN cnt END;
        EXIT;
    END;
    RETURN 0
END ProgramNameLength;

(**
Get program name
*)
PROCEDURE ProgramName*(VAR name : ARRAY OF CHAR);
VAR i : INTEGER;
BEGIN
    IF argc = -1 THEN IGNORE(Args()) END;
    IF argvlen = 0 THEN RETURN END;
    i := 0;
    LOOP
        IF i >= argvlen THEN EXIT END;
        WHILE (i < argvlen) & Char.IsSpace(argv[i]) DO INC(i) END;
        IF i >= argvlen THEN EXIT END;
        WHILE (i < argvlen) & ~Char.IsSpace(argv[i]) DO
            ArrayOfChar.AppendChar(name, argv[i]);
            INC(i)
        END;
        EXIT;
    END;
END ProgramName;

(**
Get length of argument string
*)
PROCEDURE ArgLength*(n : LENGTH): LENGTH;
VAR i, cnt, idx : INTEGER;
BEGIN
    IF argc = -1 THEN IGNORE(Args()) END;
    IF argvlen = 0 THEN RETURN 0 END;
    i := 0; idx := -1;
    LOOP
        IF i >= argvlen THEN EXIT END;
        WHILE (i < argvlen) & Char.IsSpace(argv[i]) DO INC(i) END;
        IF (i >= argvlen) THEN EXIT END;
        cnt := 0;
        WHILE (i < argvlen) & ~Char.IsSpace(argv[i]) DO INC(cnt); INC(i) END;
        IF cnt > 0 THEN INC(idx) END;
        IF n = idx THEN EXIT END;
    END;
    IF idx = n THEN RETURN cnt END;
    RETURN 0
END ArgLength;

(**
Get n-th argument
*)
PROCEDURE Arg*(VAR str : ARRAY OF CHAR; n : LENGTH);
VAR i, cnt, idx : INTEGER;
BEGIN
    IF argc = -1 THEN IGNORE(Args()) END;
    IF argvlen = 0 THEN RETURN END;
    i := 0; idx := -1;
    LOOP
        IF i >= argvlen THEN EXIT END;
        WHILE (i < argvlen) & Char.IsSpace(argv[i]) DO INC(i) END;
        IF (i >= argvlen) THEN EXIT END;
        cnt := 0;
        WHILE (i < argvlen) & ~Char.IsSpace(argv[i]) DO INC(cnt); INC(i) END;
        IF cnt > 0 THEN INC(idx) END;
        IF n = idx THEN EXIT END;
    END;
    IF idx = n THEN
        DEC(i, cnt);
        WHILE (i < argvlen) & ~Char.IsSpace(argv[i]) DO
            ArrayOfChar.AppendChar(str, argv[i]);
            INC(i)
        END;
    END
END Arg;

(* Open one of the standard streams STDING, STDOUT or STDERR. Return TRUE on success.*)
PROCEDURE StdHandle*(VAR handle : HANDLE; type : INTEGER): BOOLEAN;
VAR
    args : ARRAY 3 OF ADDRESS;
    name : ARRAY 4 OF CHAR;
    mode : INTEGER;
BEGIN
    name := ":tt";
    IF type = STDOUT THEN
        mode := 5
    ELSIF  type = STDERR THEN
        mode := 9
    ELSE
        handle := INVALID_HANDLE;
        RETURN FALSE
    END;
    args[0] := SYSTEM.ADR(name);
    args[1] := mode;
    args[2] := 3;
    handle := SemiHost(SYS_OPEN, SYSTEM.ADR(args));
    RETURN handle # INVALID_HANDLE
END StdHandle;

(* Open new or existing file with mode flags. Return TRUE on success.*)
PROCEDURE FileOpen*(VAR handle : HANDLE; filename- : ARRAY OF CHAR; mode : SET): BOOLEAN;
VAR
    args : ARRAY 3 OF ADDRESS;
    access : INTEGER;
BEGIN
    access := -1;
    IF mode = {} THEN
        access := 1; (* rb *)
    ELSE
        IF mode * Const.AccessWrite = {} THEN
            IF mode * Const.AccessRead # {} THEN
                access := 1; (* rb *)
            END;
        ELSE
            IF  mode * Const.ModeNew # {} THEN
                IF mode * Const.AccessRead # {} THEN
                    access := 7; (* w+b *)
                ELSE
                    access := 5; (* wb *)
                END;
            ELSE
                IF mode * Const.AccessRead # {} THEN
                    access := 11; (* a+b *)
                ELSE
                    access := 9; (* ab *)
                END;
            END;
        END;
    END;
    IF access = -1 THEN
        handle := INVALID_HANDLE;
        RETURN FALSE;
    END;
    args[0] := SYSTEM.ADR(filename);
    args[1] := access;
    args[2] := ArrayOfChar.Length(filename);
    handle := SemiHost(SYS_OPEN, SYSTEM.ADR(args));
    RETURN handle # INVALID_HANDLE
END FileOpen;

(* Close file. Return TRUE if success *)
PROCEDURE FileClose*(handle : HANDLE): BOOLEAN;
BEGIN RETURN SemiHost(SYS_CLOSE, SYSTEM.ADR(handle)) = 0
END FileClose;

(*
Read from file into buffer.
Return number of bytes actually read or -1 on failure.
*)
PROCEDURE FileRead*(handle : HANDLE; buffer : ADDRESS; len : LENGTH): LENGTH;
VAR
    args : ARRAY 3 OF ADDRESS;
    ret : INTEGER;
BEGIN
    args[0] := handle;
    args[1] := buffer;
    args[2] := len;
    ret := SemiHost(SYS_READ, SYSTEM.ADR(args));
    IF ret = 0 THEN RETURN len
    ELSIF ret = len THEN RETURN 0
    ELSE RETURN len - ret END;
END FileRead;

(*
Write from file into buffer.
Return number of bytes actually written or -1 on failure.
*)
PROCEDURE FileWrite*(handle : HANDLE; buffer : ADDRESS; len : LENGTH): LENGTH;
VAR
    args : ARRAY 3 OF ADDRESS;
    i, ret : INTEGER;
    ch : CHAR;
BEGIN
    args[0] := handle;
    args[1] := buffer;
    args[2] := len;
    ret := SemiHost(SYS_WRITE, SYSTEM.ADR(args));
    IF ret = 0 THEN RETURN len
    ELSE RETURN len - ret END;
END FileWrite;

(*
Write from std handle into buffer.
Return number of bytes actually written or -1 on failure.
*)
PROCEDURE FileStdWrite*(handle : HANDLE; buffer : ADDRESS; len : LENGTH): LENGTH;
VAR
    i : INTEGER;
    ch : CHAR;
BEGIN
    (* Direct to PutChar to perform buffered write due to slow speed of semihost interface  *)
    FOR i := 0 TO len - 1 DO
        SYSTEM.GET(buffer + i, ch);
        IGNORE(Putchar(ORD(ch)))
    END;
    RETURN len
END FileStdWrite;

(**
Set byte position in file. Return new position or -1 in case of failure.
*)
PROCEDURE FileSeek*(handle : HANDLE; offset : LENGTH; mode : INTEGER): LENGTH;
VAR
    args : ARRAY 3 OF ADDRESS;
    end : INTEGER;
BEGIN
    args[0] := handle;
    IF (offset < 0) OR (mode = Const.SeekCur) THEN RETURN -1 END;
    IF mode = Const.SeekEnd THEN
        end := SemiHost(SYS_FLEN, SYSTEM.ADR(args));
        IF end # -1 THEN
            offset := offset + end;
            mode := Const.SeekSet;
        END
    END;
    IF mode # Const.SeekSet THEN RETURN -1 END;
    args[1] := offset;
    RETURN SemiHost(SYS_SEEK, SYSTEM.ADR(args))
END FileSeek;

(* Return byte position in file or -1 on failure. *)
PROCEDURE FileTell*(handle : HANDLE): LENGTH;
BEGIN RETURN -1
END FileTell;

(* Set end of file to current position. *)
PROCEDURE FileSetSize*(handle : HANDLE): BOOLEAN;
BEGIN RETURN FALSE
END FileSetSize;

(* Truncate file to given size *)
PROCEDURE FileTruncate*(handle : HANDLE; size : LENGTH): LENGTH;
BEGIN RETURN -1
END FileTruncate;

(*
Flush buffered write operations to disk.
Return TRUE on success.
*)
PROCEDURE FileFlush*(handle : HANDLE): BOOLEAN;
BEGIN RETURN FALSE
END FileFlush;

(** Check if file exists *)
PROCEDURE FileExists*(filename- : ARRAY OF CHAR): BOOLEAN;
VAR fh : HANDLE;
BEGIN
    IF FileOpen(fh, filename, Const.AccessRead) THEN
        IGNORE(FileClose(fh));
        RETURN TRUE
    END;
    RETURN FALSE
END FileExists;

(** Try to remove file. Return `TRUE` on success *)
PROCEDURE FileRemove*(filename- : ARRAY OF CHAR): BOOLEAN;
VAR args : ARRAY 2 OF ADDRESS;
BEGIN
    args[0] := SYSTEM.ADR(filename);
    args[1] := ArrayOfChar.Length(filename);
    RETURN SemiHost(SYS_REMOVE, SYSTEM.ADR(args)) = 0;
END FileRemove;

(** Try to rename file. Return `TRUE` on success *)
PROCEDURE FileRename*(oldname-, newname-: ARRAY OF CHAR): BOOLEAN;
VAR args : ARRAY 4 OF ADDRESS;
BEGIN
    args[0] := SYSTEM.ADR(oldname);
    args[1] := ArrayOfChar.Length(oldname);
    args[2] := SYSTEM.ADR(newname);
    args[3] := ArrayOfChar.Length(newname);
    RETURN SemiHost(SYS_RENAME, SYSTEM.ADR(args)) = 0;
END FileRename;

(** Try to get modification time for file. Return `TRUE` on success *)
PROCEDURE FileModificationTime*(VAR time : DateTime; filename-: ARRAY OF CHAR): BOOLEAN;
BEGIN RETURN FALSE
END FileModificationTime;

(** Open file/directory listing *)
PROCEDURE DirOpen*(VAR dir: DirEntry; name-: ARRAY OF CHAR);
BEGIN END DirOpen;

(** Close directory listing *)
PROCEDURE DirClose*(VAR dir: DirEntry);
BEGIN END DirClose;

(** Return FALSE when end of file/directory listing is reached *)
PROCEDURE DirNext*(VAR dir: DirEntry): BOOLEAN;
BEGIN RETURN FALSE
END DirNext;

(** Return length of current directory listing name string *)
PROCEDURE DirNameLength*(VAR dir: DirEntry): LENGTH;
BEGIN RETURN 0
END DirNameLength;

(** Return current directory listing name *)
PROCEDURE DirName*(dir-: DirEntry; VAR name: ARRAY OF CHAR);
BEGIN END DirName;

(** Return TRUE if current entry is a directory *)
PROCEDURE DirIsDir*(dir-: DirEntry): BOOLEAN;
BEGIN RETURN FALSE
END DirIsDir;

(** Get current local time *)
PROCEDURE GetTime*(VAR time : DateTime);
VAR offset : UNSIGNED32;
BEGIN
    time.year := 1970;
    time.month := 1;
    time.day := 1;
    time.hour := 0;
    time.msec := 0;
    offset := SemiHost(SYS_REMOVE, 0);
    time.min := INTEGER(offset MOD 60);
    time.sec := INTEGER(offset DIV 60);
END GetTime;

(** Get local time UTC offset *)
PROCEDURE GetTimeZoneOffset*(): INTEGER;
BEGIN RETURN 0
END GetTimeZoneOffset;

(** Get string length of the current directory *)
PROCEDURE CDNameLength*(): LENGTH;
BEGIN RETURN 0
END CDNameLength;

(** Get the current directory *)
PROCEDURE CDName*(VAR name: ARRAY OF CHAR; length: LENGTH);
BEGIN END CDName;

(** Get the current directory *)
PROCEDURE SetCD*(name-: ARRAY OF CHAR): BOOLEAN;
BEGIN RETURN FALSE
END SetCD;

(** Try to create directory. Return `TRUE` on success *)
PROCEDURE CreateDirectory*(name-: ARRAY OF CHAR): BOOLEAN;
BEGIN RETURN FALSE
END CreateDirectory;

(** Try to delete directory. Return `TRUE` on success *)
PROCEDURE RemoveDirectory*(name-: ARRAY OF CHAR): BOOLEAN;
BEGIN RETURN FALSE
END RemoveDirectory;

(** Get string length of the environment variable *)
PROCEDURE EnvVarLength*(name-: ARRAY OF CHAR): LENGTH;
BEGIN RETURN 0
END EnvVarLength;

(** Get environment variable *)
PROCEDURE EnvVar*(VAR value: ARRAY OF CHAR; name-: ARRAY OF CHAR);
BEGIN END EnvVar;

(** Exit with return code *)
PROCEDURE Exit*(code : INTEGER);
VAR args : ARRAY 2 OF ADDRESS;
BEGIN
    FlushBuffer;
    args[0] := ADP_Stopped_ApplicationExit;
    args[1] := code;
    IGNORE(SemiHost(SYS_EXIT_EXTENDED, SYSTEM.ADR(args)));
END Exit;

(* Get last error code or OK on no error. *)
PROCEDURE GetLastError*(VAR error: INTEGER);
VAR errno : INTEGER;
BEGIN
    errno := SemiHost(SYS_ERRNO, 0);
    CASE errno OF
          EOK : error := Const.OK
        | ENOENT : error := Const.ErrorPathNotFound
        | EACCES : error := Const.ErrorNoAccess
        | EEXIST : error := Const.ErrorAlreadyExists
    ELSE
        error := Const.ErrorUnknown
    END;
END GetLastError;

BEGIN
    argc := -1;
    idx := 0;
END OSHost.