(**
Module for Stream class with access to OS files and the standard streams.
*)
MODULE OSStream IN Std;

IMPORT SYSTEM;
IN Std IMPORT Const, ADTStream, OSHost;

CONST
    INVALID_HANDLE* = OSHost.INVALID_HANDLE;
    STDIN* = OSHost.STDIN;
    STDOUT* = OSHost.STDOUT;
    STDERR* = OSHost.STDERR;
    (* Stream Open flags *)
    AccessRead*   = Const.AccessRead;
    AccessWrite*  = Const.AccessWrite;
    ModeNew*      = Const.ModeNew; (* forces file to be created or truncated  *)
    (* Stream Seek *)
    SeekSet* = Const.SeekSet;
    SeekCur* = Const.SeekCur;
    SeekEnd* = Const.SeekEnd;
    (* Stream Error *)
    OK* = Const.OK;

TYPE
    BYTE = SYSTEM.BYTE;
    File* = RECORD(ADTStream.ADTStream)
        fh : OSHost.HANDLE;
        mode : SET;
        opened : BOOLEAN;
    END;
    Std* = RECORD(ADTStream.ADTStream)
        fh : OSHost.HANDLE;
        type : INTEGER;
    END;

(*
 File
*)

(**
Open a OS file with given mode.
Return `TRUE` on success.
*)
PROCEDURE (VAR s : File) Open*(filename- : ARRAY OF CHAR; mode: SET): BOOLEAN;
BEGIN
    s.mode := mode;
    s.opened := OSHost.FileOpen(s.fh, filename, s.mode);
    RETURN s.opened
END Open;

(** Close Stream *)
PROCEDURE (VAR s : File) Close*();
BEGIN
    IF s.opened THEN s.opened := OSHost.FileClose(s.fh)
    ELSE s.error := Const.ErrorAlreadyOpen END
END Close;

(** Return `TRUE` if Stream is closed *)
PROCEDURE (VAR s : File) Closed*(): BOOLEAN;
BEGIN RETURN ~s.opened END Closed;

(** Read bytes into buffer with start and length. *)
PROCEDURE (VAR s : File) ReadBytes*(VAR buffer : ARRAY OF BYTE; start, length : LENGTH): LENGTH;
BEGIN RETURN OSHost.FileRead(s.fh, SYSTEM.ADR(buffer[start]), length)
END ReadBytes;

(** Write bytes from buffer with start and length. *)
PROCEDURE (VAR s : File) WriteBytes*(VAR buffer : ARRAY OF BYTE; start, length : LENGTH): LENGTH;
BEGIN RETURN OSHost.FileWrite(s.fh, SYSTEM.ADR(buffer[start]), length)
END WriteBytes;

(**
Offsets or set the current location depending on the
mode argument:

 * `SeekSet` : sets position relative to start of stream.
 * `SeekCur` : sets position relative to current position of stream.
 * `SeekEnd` : sets position relative to end position of stream (only negative offset values makes sense).

Return new position or -1 in case of failure.
*)
PROCEDURE (VAR s : File) Seek*(offset : LENGTH; mode : INTEGER): LENGTH;
BEGIN RETURN OSHost.FileSeek(s.fh, offset, mode)
END Seek;

(** Return current position or -1 on failure. *)
PROCEDURE (VAR s : File) Tell*(): LENGTH;
BEGIN RETURN OSHost.FileTell(s.fh)
END Tell;

(**
Truncates or extends stream to new size.
Return new size or -1 in case of failure.
*)
PROCEDURE (VAR s : File) Truncate*(size : LENGTH): LENGTH;
BEGIN RETURN OSHost.FileTruncate(s.fh, size)
END Truncate;

(** Flush buffers *)
PROCEDURE (VAR s : File) Flush*();
BEGIN IGNORE(OSHost.FileFlush(s.fh))
END Flush;

(** Return `TRUE` if Stream is readable *)
PROCEDURE (VAR s : File) Readable*(): BOOLEAN;
BEGIN RETURN s.mode * AccessRead # {} END Readable;

(** Return `TRUE` if Stream is writeable *)
PROCEDURE (VAR s : File) Writeable*(): BOOLEAN;
BEGIN RETURN s.mode * AccessWrite # {} END Writeable;

(** Return `TRUE` if Stream is seekable *)
PROCEDURE (VAR s : File) Seekable*(): BOOLEAN;
BEGIN RETURN TRUE END Seekable;

(*
 Std
*)

(**
Open one of the standard streams STDING, STDOUT or STDERR. Return TRUE on success.
*)
PROCEDURE (VAR s : Std) Open*(type : INTEGER): BOOLEAN;
BEGIN
    s.type := type;
    RETURN OSHost.StdHandle(s.fh, s.type)
END Open;

(** Close Stream *)
PROCEDURE (VAR s : Std) Close*();
BEGIN END Close;

(** Read bytes into buffer with start and length. *)
PROCEDURE (VAR s : Std) ReadBytes*(VAR buffer : ARRAY OF BYTE; start, length : LENGTH): LENGTH;
BEGIN RETURN OSHost.FileRead(s.fh, SYSTEM.ADR(buffer[start]), length)
END ReadBytes;

(** Write bytes from buffer with start and length. *)
PROCEDURE (VAR s : Std) WriteBytes*(VAR buffer : ARRAY OF BYTE; start, length : LENGTH): LENGTH;
BEGIN RETURN OSHost.FileStdWrite(s.fh, SYSTEM.ADR(buffer[start]), length)
END WriteBytes;

(** Return `TRUE` if Stream is a TTY *)
PROCEDURE (VAR s : Std) IsTTY*(): BOOLEAN;
BEGIN RETURN TRUE END IsTTY;

(** Return `TRUE` if Stream is readable *)
PROCEDURE (VAR s : Std) Readable*(): BOOLEAN;
BEGIN RETURN s.type = STDIN END Readable;

(** Return `TRUE` if Stream is writeable *)
PROCEDURE (VAR s : Std) Writeable*(): BOOLEAN;
BEGIN RETURN (s.type = STDOUT) OR (s.type = STDERR) END Writeable;

(** Return `TRUE` if Stream is seekable *)
PROCEDURE (VAR s : Std) Seekable*(): BOOLEAN;
BEGIN RETURN FALSE END Seekable;

END OSStream.