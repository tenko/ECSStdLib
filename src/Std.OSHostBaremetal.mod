(** OS host dependent procedures. *)
MODULE OSHost IN Std;

IMPORT SYSTEM;
IN Std IMPORT Const, Char;

CONST
    INVALID_HANDLE* = -1;
    STDIN* = 0;
    STDOUT* = 1;
    STDERR* = 2;
    DATETIMEOFFSET* = TRUE;

TYPE
    ADDRESS = SYSTEM.ADDRESS;
    HANDLE* = INTEGER;
    DirEntry* = RECORD END;
    DateTime* = RECORD
        year*, month*, day*, hour*, min*, sec*, msec*: INTEGER;
    END;

VAR
    argc: LENGTH;

PROCEDURE ^ Putchar ["putchar"] (character: INTEGER): INTEGER;

PROCEDURE ^ Abort ["abort"] ();

(**
Get number of program arguments
*)
PROCEDURE Args*(): LENGTH;
BEGIN RETURN 0
END Args;

(**
Get length of program name string
*)
PROCEDURE ProgramNameLength*(): LENGTH;
BEGIN RETURN 0
END ProgramNameLength;

(**
Get program name
*)
PROCEDURE ProgramName*(VAR name : ARRAY OF CHAR);
BEGIN END ProgramName;

(**
Get length of argument string
*)
PROCEDURE ArgLength*(n : LENGTH): LENGTH;
BEGIN RETURN 0
END ArgLength;

(**
Get n-th argument
*)
PROCEDURE Arg*(VAR str : ARRAY OF CHAR; n : LENGTH);
BEGIN END Arg;

(* Open one of the standard streams STDING, STDOUT or STDERR. Return TRUE on success.*)
PROCEDURE StdHandle*(VAR handle : HANDLE; type : INTEGER): BOOLEAN;
BEGIN
    IF (type # STDOUT) &  (type # STDERR) THEN
        handle := INVALID_HANDLE;
        RETURN FALSE
    END;
    handle := type;
    RETURN TRUE
END StdHandle;

(* Open new or existing file with mode flags. Return TRUE on success.*)
PROCEDURE FileOpen*(VAR handle : HANDLE; filename- : ARRAY OF CHAR; mode : SET): BOOLEAN;
BEGIN RETURN FALSE
END FileOpen;

(* Close file. Return TRUE if success *)
PROCEDURE FileClose*(handle : HANDLE): BOOLEAN;
BEGIN RETURN FALSE
END FileClose;

(*
Read from file into buffer.
Return number of bytes actually read or -1 on failure.
*)
PROCEDURE FileRead*(handle : HANDLE; buffer : ADDRESS; len : LENGTH): LENGTH;
BEGIN RETURN -1
END FileRead;

(*
Write from file into buffer.
Return number of bytes actually written or -1 on failure.
*)
PROCEDURE FileWrite*(handle : HANDLE; buffer : ADDRESS; len : LENGTH): LENGTH;
BEGIN RETURN -1
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
    (* Direct to PutChar *)
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
BEGIN RETURN -1
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
BEGIN RETURN FALSE
END FileExists;

(** Try to remove file. Return `TRUE` on success *)
PROCEDURE FileRemove*(filename- : ARRAY OF CHAR): BOOLEAN;
BEGIN RETURN FALSE
END FileRemove;

(** Try to rename file. Return `TRUE` on success *)
PROCEDURE FileRename*(oldname-, newname-: ARRAY OF CHAR): BOOLEAN;
BEGIN RETURN FALSE
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
PROCEDURE GetTime*(VAR time : DateTime; VAR delta : HUGEINT);
VAR offset : UNSIGNED32;
BEGIN
    time.year := 1970;
    time.month := 1;
    time.day := 1;
    time.hour := 0;
    time.msec := 0;
    delta := 0;
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
BEGIN Abort
END Exit;

(* Get last error code or OK on no error. *)
PROCEDURE GetLastError*(VAR error: INTEGER);
BEGIN error := Const.OK;
END GetLastError;

BEGIN
    argc := -1;
END OSHost.