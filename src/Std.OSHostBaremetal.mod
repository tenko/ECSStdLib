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
PROCEDURE ^ HostArgs ["HostArgs"](): LENGTH;
PROCEDURE Args*(): LENGTH;
BEGIN RETURN HostArgs()
END Args;

(**
Get length of program name string
*)
PROCEDURE ^ HostProgramNameLength ["HostProgramNameLength"](): LENGTH;
PROCEDURE ProgramNameLength*(): LENGTH;
BEGIN RETURN HostProgramNameLength()
END ProgramNameLength;

(**
Get program name
*)
PROCEDURE ^ HostProgramName ["HostProgramName"](VAR name : ARRAY OF CHAR);
PROCEDURE ProgramName*(VAR name : ARRAY OF CHAR);
BEGIN HostProgramName(name) END ProgramName;

(**
Get length of argument string
*)
PROCEDURE ^ HostArgLength ["HostArgLength"](n : LENGTH): LENGTH;
PROCEDURE ArgLength*(n : LENGTH): LENGTH;
BEGIN RETURN HostArgLength(n)
END ArgLength;

(**
Get n-th argument
*)
PROCEDURE ^ HostArg ["HostArg"](VAR str : ARRAY OF CHAR; n : LENGTH);
PROCEDURE Arg*(VAR str : ARRAY OF CHAR; n : LENGTH);
BEGIN HostArg(str, n) END Arg;

(* Open one of the standard streams STDING, STDOUT or STDERR. Return TRUE on success.*)
PROCEDURE ^ HostStdHandle ["HostStdHandle"](VAR handle : HANDLE; type : INTEGER): BOOLEAN;
PROCEDURE StdHandle*(VAR handle : HANDLE; type : INTEGER): BOOLEAN;
BEGIN RETURN HostStdHandle(handle, type) END StdHandle;

(* Open new or existing file with mode flags. Return TRUE on success.*)
PROCEDURE ^ HostFileOpen ["HostFileOpen"](VAR handle : HANDLE; filename- : ARRAY OF CHAR; mode : SET): BOOLEAN;
PROCEDURE FileOpen*(VAR handle : HANDLE; filename- : ARRAY OF CHAR; mode : SET): BOOLEAN;
BEGIN RETURN HostFileOpen(handle, filename, mode)
END FileOpen;

(* Close file. Return TRUE if success *)
PROCEDURE ^ HostFileClose ["FileClose"](handle : HANDLE): BOOLEAN;
PROCEDURE FileClose*(handle : HANDLE): BOOLEAN;
BEGIN RETURN HostFileClose(handle)
END FileClose;

(*
Read from file into buffer.
Return number of bytes actually read or -1 on failure.
*)
PROCEDURE ^ HostFileRead ["HostFileRead"](handle : HANDLE; buffer : ADDRESS; len : LENGTH): LENGTH;
PROCEDURE FileRead*(handle : HANDLE; buffer : ADDRESS; len : LENGTH): LENGTH;
BEGIN RETURN HostFileRead(handle, buffer, len)
END FileRead;

(*
Write from file into buffer.
Return number of bytes actually written or -1 on failure.
*)
PROCEDURE ^ HostFileWrite ["HostFileWrite"](handle : HANDLE; buffer : ADDRESS; len : LENGTH): LENGTH;
PROCEDURE FileWrite*(handle : HANDLE; buffer : ADDRESS; len : LENGTH): LENGTH;
BEGIN RETURN FileWrite(handle, buffer, len) END FileWrite;

(*
Write from std handle into buffer.
Return number of bytes actually written or -1 on failure.
*)
PROCEDURE ^ HostFileStdWrite ["HostFileStdWrite"](handle : HANDLE; buffer : ADDRESS; len : LENGTH): LENGTH;
PROCEDURE FileStdWrite*(handle : HANDLE; buffer : ADDRESS; len : LENGTH): LENGTH;
BEGIN RETURN HostFileStdWrite(handle, buffer, len) END FileStdWrite;

(**
Set byte position in file. Return new position or -1 in case of failure.
*)
PROCEDURE ^ HostFileSeek ["HostFileSeek"](handle : HANDLE; offset : LENGTH; mode : INTEGER): LENGTH;
PROCEDURE FileSeek*(handle : HANDLE; offset : LENGTH; mode : INTEGER): LENGTH;
BEGIN RETURN HostFileSeek(handle, offset, mode) END FileSeek;

(* Return byte position in file or -1 on failure. *)
PROCEDURE ^ HostFileTell ["HostFileTell"](handle : HANDLE): LENGTH;
PROCEDURE FileTell*(handle : HANDLE): LENGTH;
BEGIN RETURN HostFileTell(handle) END FileTell;

(* Set end of file to current position. *)
PROCEDURE ^ HostFileSetSize ["HostFileTell"](handle : HANDLE): BOOLEAN;
PROCEDURE FileSetSize*(handle : HANDLE): BOOLEAN;
BEGIN RETURN HostFileSetSize(handle) END FileSetSize;

(* Truncate file to given size *)
PROCEDURE ^ HostFileTruncate ["HostFileTruncate"](handle : HANDLE; size : LENGTH): LENGTH;
PROCEDURE FileTruncate*(handle : HANDLE; size : LENGTH): LENGTH;
BEGIN RETURN HostFileTruncate(handle, size) END FileTruncate;

(*
Flush buffered write operations to disk.
Return TRUE on success.
*)
PROCEDURE ^ HostFileFlush ["HostFileFlush"](handle : HANDLE): BOOLEAN;
PROCEDURE FileFlush*(handle : HANDLE): BOOLEAN;
BEGIN RETURN HostFileFlush(handle) END FileFlush;

(** Check if file exists *)
PROCEDURE ^ HostFileExists ["HostFileExists"](filename- : ARRAY OF CHAR): BOOLEAN;
PROCEDURE FileExists*(filename- : ARRAY OF CHAR): BOOLEAN;
BEGIN RETURN HostFileExists(filename) END FileExists;

(** Try to remove file. Return `TRUE` on success *)
PROCEDURE ^ HostFileRemove ["HostFileRemove"](filename- : ARRAY OF CHAR): BOOLEAN;
PROCEDURE FileRemove*(filename- : ARRAY OF CHAR): BOOLEAN;
BEGIN RETURN HostFileRemove(filename) END FileRemove;

(** Try to rename file. Return `TRUE` on success *)
PROCEDURE ^ HostFileRename ["HostFileRemove"](oldname-, newname-: ARRAY OF CHAR): BOOLEAN;
PROCEDURE FileRename*(oldname-, newname-: ARRAY OF CHAR): BOOLEAN;
BEGIN RETURN HostFileRename(oldname, newname) END FileRename;

(** Try to get modification time for file. Return `TRUE` on success *)
PROCEDURE ^ HostFileModificationTime ["HostFileModificationTime"](VAR time : DateTime; VAR delta : HUGEINT; filename-: ARRAY OF CHAR): BOOLEAN;
PROCEDURE FileModificationTime*(VAR time : DateTime; VAR delta : HUGEINT; filename-: ARRAY OF CHAR): BOOLEAN;
BEGIN RETURN HostFileModificationTime(time, delta, filename) END FileModificationTime;

(** Open file/directory listing *)
PROCEDURE ^ HostDirOpen ["HostDirOpen"](VAR dir: DirEntry; name-: ARRAY OF CHAR);
PROCEDURE DirOpen*(VAR dir: DirEntry; name-: ARRAY OF CHAR);
BEGIN HostDirOpen(dir, name) END DirOpen;

(** Close directory listing *)
PROCEDURE ^ HostDirClose ["HostDirClose"](VAR dir: DirEntry);
PROCEDURE DirClose*(VAR dir: DirEntry);
BEGIN HostDirClose(dir) END DirClose;

(** Return FALSE when end of file/directory listing is reached *)
PROCEDURE ^ HostDirNext ["HostDirNext"](VAR dir: DirEntry): BOOLEAN;
PROCEDURE DirNext*(VAR dir: DirEntry): BOOLEAN;
BEGIN RETURN HostDirNext(dir) END DirNext;

(** Return length of current directory listing name string *)
PROCEDURE ^ HostDirNameLength ["DirNameLength"](VAR dir: DirEntry): LENGTH;
PROCEDURE DirNameLength*(VAR dir: DirEntry): LENGTH;
BEGIN RETURN DirNameLength(dir) END DirNameLength;

(** Return current directory listing name *)
PROCEDURE ^ HostDirName ["HostDirName"](dir-: DirEntry; VAR name: ARRAY OF CHAR);
PROCEDURE DirName*(dir-: DirEntry; VAR name: ARRAY OF CHAR);
BEGIN HostDirName(dir, name) END DirName;

(** Return TRUE if current entry is a directory *)
PROCEDURE ^ HostDirIsDir ["HostDirIsDir"](dir-: DirEntry): BOOLEAN;
PROCEDURE DirIsDir*(dir-: DirEntry): BOOLEAN;
BEGIN RETURN HostDirIsDir(dir) END DirIsDir;

(** Return TRUE if current entry is a file *)
PROCEDURE ^ HostDirIsFile ["HostDirIsFile"](dir-: DirEntry): BOOLEAN;
PROCEDURE DirIsFile*(dir-: DirEntry): BOOLEAN;
BEGIN RETURN HostDirIsFile(dir) END DirIsFile;

(** Get current local time *)
PROCEDURE ^ HostGetTime ["HostGetTime"](VAR time : DateTime; VAR delta : HUGEINT);
PROCEDURE GetTime*(VAR time : DateTime; VAR delta : HUGEINT);
BEGIN HostGetTime(time, delta) END GetTime;

(** Get local time UTC offset *)
PROCEDURE ^ HostGetTimeZoneOffset ["HostGetTimeZoneOffset"](): INTEGER;
PROCEDURE GetTimeZoneOffset*(): INTEGER;
BEGIN RETURN HostGetTimeZoneOffset() END GetTimeZoneOffset;

(** Get string length of the current directory *)
PROCEDURE ^ HostCDNameLength ["HostCDNameLength"](): LENGTH;
PROCEDURE CDNameLength*(): LENGTH;
BEGIN RETURN HostCDNameLength() END CDNameLength;

(** Get the current directory *)
PROCEDURE ^ HostCDName ["HostCDName"](VAR name: ARRAY OF CHAR; length: LENGTH);
PROCEDURE CDName*(VAR name: ARRAY OF CHAR; length: LENGTH);
BEGIN HostCDName(name, length) END CDName;

(** Get the current directory *)
PROCEDURE ^ HostSetCD ["HostSetCD"](name-: ARRAY OF CHAR): BOOLEAN;
PROCEDURE SetCD*(name-: ARRAY OF CHAR): BOOLEAN;
BEGIN RETURN HostSetCD(name) END SetCD;

(** Try to create directory. Return `TRUE` on success *)
PROCEDURE ^ HostCreateDirectory ["HostCreateDirectory"](name-: ARRAY OF CHAR): BOOLEAN;
PROCEDURE CreateDirectory*(name-: ARRAY OF CHAR): BOOLEAN;
BEGIN RETURN HostCreateDirectory(name) END CreateDirectory;

(** Try to delete directory. Return `TRUE` on success *)
PROCEDURE ^ HostRemoveDirectory ["HostRemoveDirectory"](name-: ARRAY OF CHAR): BOOLEAN;
PROCEDURE RemoveDirectory*(name-: ARRAY OF CHAR): BOOLEAN;
BEGIN RETURN HostRemoveDirectory(name) END RemoveDirectory;

(** Get string length of the environment variable *)
PROCEDURE ^ HostEnvVarLength ["HostEnvVarLength"](name-: ARRAY OF CHAR): LENGTH;
PROCEDURE EnvVarLength*(name-: ARRAY OF CHAR): LENGTH;
BEGIN RETURN HostEnvVarLength(name) END EnvVarLength;

(** Get environment variable *)
PROCEDURE ^ HostEnvVar ["HostEnvVar"](VAR value: ARRAY OF CHAR; name-: ARRAY OF CHAR);
PROCEDURE EnvVar*(VAR value: ARRAY OF CHAR; name-: ARRAY OF CHAR);
BEGIN HostEnvVar(value, name) END EnvVar;

(** Exit with return code *)
PROCEDURE ^ HostExit ["HostExit"](code : INTEGER);
PROCEDURE Exit*(code : INTEGER);
BEGIN HostExit(code) END Exit;

(* Get last error code or OK on no error. *)
PROCEDURE ^ HostGetLastError ["HostGetLastError"](VAR error: INTEGER);
PROCEDURE GetLastError*(VAR error: INTEGER);
BEGIN HostGetLastError(error) END GetLastError;

BEGIN
    argc := -1;
END OSHost.