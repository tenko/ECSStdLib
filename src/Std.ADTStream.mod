MODULE ADTStream IN Std;

IMPORT SYSTEM;
IN Std IMPORT Config, Type, Const, Integer, Cardinal, Real;
IN Std IMPORT ArrayOfByte, ArrayOfChar, String, DateTime;

TYPE
    BYTE = SYSTEM.BYTE;
    (* Abstract Stream Class *)
    Stream* = RECORD(Type.Writer)
        error* : INTEGER;
    END;
    (* NullStrean *)
    NullStream* = RECORD (Stream)END;
    (* MemoryStream *)
    MemoryStorage = POINTER TO ARRAY OF BYTE;
    MemoryStream* = RECORD (Stream)
        storage : MemoryStorage;
        pos : LENGTH;
        last : LENGTH;
    END;

(*
   Stream (Abstract)
*)

(** Return `TRUE` if the `Stream` operation has returned an error.  *)
PROCEDURE (VAR s : Stream) HasError*(): BOOLEAN;
BEGIN RETURN s.error # Const.OK
END HasError;

(** Return last error code or `OK` if no error is set. *)
PROCEDURE (VAR s : Stream) LastError*(): LONGINT;
BEGIN RETURN s.error
END LastError;

(** Clear error status to `OK`. *)
PROCEDURE (VAR s : Stream) ClearError*();
BEGIN s.error := Const.OK
END ClearError;

(** Read bytes into buffer with start and length. *)
PROCEDURE (VAR s : Stream) ReadBytes*(VAR buffer : ARRAY OF BYTE; start, length : LENGTH): LENGTH;
BEGIN
    s.error := Const.ErrorNotImplemented;
    RETURN -1
END ReadBytes;

(** Read `BYTE` value. Return `TRUE` if success. *)
PROCEDURE (VAR s : Stream) ReadByte*(VAR value : BYTE): BOOLEAN;
BEGIN RETURN s.ReadBytes(value, 0, 1) = 1 END ReadByte;

(** Read `CHAR` value. Return `TRUE` if success. *)
PROCEDURE (VAR s : Stream) ReadChar*(VAR value : CHAR): BOOLEAN;
BEGIN RETURN s.ReadBytes(value, 0, 1) = 1 END ReadChar;

(** Read `INTEGER` value. Return `TRUE` if success. *)
PROCEDURE (VAR s : Stream) ReadInteger*(VAR value : INTEGER): BOOLEAN;
BEGIN RETURN s.ReadBytes(value, 0, SIZE(INTEGER)) = SIZE(INTEGER) END ReadInteger;

(** Read `LONGINT` value. Return `TRUE` if success. *)
PROCEDURE (VAR s : Stream) ReadLongInt*(VAR value : LONGINT): BOOLEAN;
BEGIN RETURN s.ReadBytes(value, 0, SIZE(LONGINT)) = SIZE(LONGINT) END ReadLongInt;

(** Read `REAL` value. Return `TRUE` if success. *)
PROCEDURE (VAR s : Stream) ReadReal*(VAR value : REAL): BOOLEAN;
BEGIN RETURN s.ReadBytes(value, 0, SIZE(REAL)) = SIZE(REAL) END ReadReal;

(** Read `LONGREAL` value. Return `TRUE` if success. *)
PROCEDURE (VAR s : Stream) ReadLongReal*(VAR value : LONGREAL): BOOLEAN;
BEGIN RETURN s.ReadBytes(value, 0, SIZE(LONGREAL)) = SIZE(LONGREAL) END ReadLongReal;

(**
Read line to `EOL` mark or `EOF` mark.
`STRING` possible reallocated to contain whole line if needed.
Return `TRUE` if success.
*)
PROCEDURE (VAR s : Stream) ReadLine*(VAR value : String.STRING): BOOLEAN;
VAR
    i, len : LENGTH;
    ch : CHAR;
    eof : BOOLEAN;
BEGIN
    String.Reserve(value, 1, FALSE);
    value^[0] := 00X;
    i := 0 ; eof := FALSE;
    len := LEN(value^);
    LOOP
        IF i >= len THEN
            len := 2 * len;
            String.Reserve(value, len, TRUE);
        END;
        IF ~s.ReadChar(ch) THEN eof := TRUE; EXIT END;
        IF ch = Config.NL[0] THEN
            IF Config.NL[1] = 00X THEN EXIT END;
            IF ~s.ReadChar(ch) THEN EXIT END;
            IF ch = Config.NL[1] THEN EXIT END;
        ELSIF ch = 00X THEN EXIT
        END;
        value^[i] := ch;
        INC(i);
    END;
    value[i] := 00X;
    IF (i = 0) & eof THEN RETURN FALSE END;
    RETURN TRUE
END ReadLine;

(** Write bytes from buffer with start and length. *)
PROCEDURE (VAR s : Stream) WriteBytes*(VAR buffer : ARRAY OF BYTE; start, length: LENGTH): LENGTH;
BEGIN
    s.error := Const.ErrorNotImplemented;
    RETURN -1
END WriteBytes;

(** Write `BYTE` value. Sets error to `ErrorWriteFailed` on failure. *)
PROCEDURE (VAR s : Stream) WriteByte*(value : BYTE);
BEGIN
    IF s.WriteBytes(value, 0, 1) = 1 THEN
        s.error := Const.ErrorWriteFailed
    END
END WriteByte;

(** Write `CHAR` value. Sets error to `ErrorWriteFailed` on failure. *)
PROCEDURE (VAR s : Stream) WriteChar*(value : CHAR);
BEGIN
    IF s.WriteBytes(value, 0, 1) # 1 THEN
        s.error := Const.ErrorWriteFailed
    END
END WriteChar;

(** Write `INTEGER` value. Sets error to `ErrorWriteFailed` on failure. *)
PROCEDURE (VAR s : Stream) WriteInteger*(value : INTEGER);
BEGIN
    IF s.WriteBytes(value, 0, SIZE(INTEGER)) # SIZE(INTEGER) THEN
        s.error := Const.ErrorWriteFailed
    END
END WriteInteger;

(** Write `LONGINT` value. Sets error to `ErrorWriteFailed` on failure. *)
PROCEDURE (VAR s : Stream) WriteLongInt*(value : LONGINT);
BEGIN
    IF s.WriteBytes(value, 0, SIZE(LONGINT)) # SIZE(LONGINT) THEN
        s.error := Const.ErrorWriteFailed
    END
END WriteLongInt;

(** Write `REAL` value. Sets error to `ErrorWriteFailed` on failure. *)
PROCEDURE (VAR s : Stream) WriteReal*(value : REAL);
BEGIN
    IF s.WriteBytes(value, 0, SIZE(REAL)) # SIZE(REAL) THEN
        s.error := Const.ErrorWriteFailed
    END
END WriteReal;

(** Write `LONGREAL` value. Sets error to `ErrorWriteFailed` on failure. *)
PROCEDURE (VAR s : Stream) WriteLongReal*(value : LONGREAL);
BEGIN
    IF s.WriteBytes(value, 0, SIZE(LONGREAL)) # SIZE(LONGREAL) THEN
        s.error := Const.ErrorWriteFailed
    END
END WriteLongReal;

(**
Write `ARRAY OF CHAR` value to NULL byte or length of array.
Sets error to `ErrorWriteFailed` on failure.
*)
PROCEDURE (VAR s : Stream) WriteString*(str- : ARRAY OF CHAR);
VAR i, len : LENGTH;
BEGIN
    i := 0; len := ArrayOfChar.Length(str);
    WHILE i < len DO s.WriteChar(str[i]); INC(i) END;
END WriteString;

(** Write platforms newline value to stream. Sets error to `ErrorWriteFailed` on failure. *)
PROCEDURE (VAR s : Stream) WriteNL*();
BEGIN
    s.WriteChar(Config.NL[0]);
    IF Config.NL[1] # 00X THEN s.WriteChar(Config.NL[1]) END;
END WriteNL;

(** Write `Stream` `src` to stream. Sets error to `ErrorWriteFailed` on failure. *)
PROCEDURE (VAR s : Stream) WriteStream*(src : Stream);
VAR value : BYTE;
BEGIN
    WHILE src.ReadByte(value) DO s.WriteByte(value) END
END WriteStream;

(**
Offsets or set the current location depending on the
mode argument:

 * `SeekSet` : sets position relative to start of stream.
 * `SeekCur` : sets position relative to current position of stream.
 * `SeekEnd` : sets position relative to end position of stream (only negative offset values makes sense).

Return new position or -1 in case of failure.
*)
PROCEDURE (VAR s : Stream) Seek*(offset : LENGTH; mode : INTEGER): LENGTH;
BEGIN
    s.error := Const.ErrorNotImplemented;
    RETURN -1
END Seek;

(** Return current position or -1 on failure. *)
PROCEDURE (VAR s : Stream) Tell*(): LENGTH;
BEGIN
    s.error := Const.ErrorNotImplemented;
    RETURN -1
END Tell;

(**
Truncates or extends stream to new size.
Return new size or -1 in case of failure.
*)
PROCEDURE (VAR s : Stream) Truncate*(size : LENGTH): LENGTH;
BEGIN
    s.error := Const.ErrorNotImplemented;
    RETURN -1
END Truncate;

(** Flush buffers *)
PROCEDURE (VAR s : Stream) Flush*();
BEGIN
    s.error := Const.ErrorNotImplemented
END Flush;

(** Close Stream *)
PROCEDURE (VAR s : Stream) Close*();
BEGIN
    s.error := Const.ErrorNotImplemented
END Close;

(** Return `TRUE` if Stream is closed *)
PROCEDURE (VAR s : Stream) Closed*(): BOOLEAN;
BEGIN RETURN FALSE END Closed;

(** Return `TRUE` if Stream is a TTY *)
PROCEDURE (VAR s : Stream) IsTTY*(): BOOLEAN;
BEGIN RETURN FALSE END IsTTY;

(** Return `TRUE` if Stream is readable *)
PROCEDURE (VAR s : Stream) Readable*(): BOOLEAN;
BEGIN RETURN FALSE END Readable;

(** Return `TRUE` if Stream is writeable *)
PROCEDURE (VAR s : Stream) Writeable*(): BOOLEAN;
BEGIN RETURN FALSE END Writeable;

(** Return `TRUE` if Stream is seekable *)
PROCEDURE (VAR s : Stream) Seekable*(): BOOLEAN;
BEGIN RETURN FALSE END Seekable;

(**
Format `ARRAY OF CHAR`. 

* `width` : Total field with. Can overflow if string is bigger.
* `prec` : The number of characters in string to add (if prec > 0)
* `flags` : The formatting flags defaults to `Left` alignment.

The `Upper` flag will make the whole string upper case.
The `Alt` flag will capitalize the string.
*)
PROCEDURE (VAR s : Stream) FormatString*(str- : ARRAY OF CHAR; width, prec: INTEGER; flags: SET);
BEGIN ArrayOfChar.Format(s, str, width, prec, flags);
END FormatString;

(**
Format `HUGEINT`.

* `width` : Total field with. Can overflow if number is bigger.

The formatting flags defaults to `Right` alignment.
The `Zero` flag fills with 0 of the formatting is right aligned.
The `Spc` flag fills in a blank character for `+` if the number is positive.
The `Sign` flag fills in a `+` character if the number is positive.
If both `Spc` and `Sign` are given then `Sign` precedes.
*)
PROCEDURE (VAR s : Stream) FormatInteger*(value : HUGEINT; width: LENGTH; flags: SET);
BEGIN Integer.Format(s, value, width, flags);
END FormatInteger;

(**
Format `REAL`.

* `prec` : Precision or zero for default value.
* `width` : Total field with. Can overflow if number is bigger.
* `flags` : `Exp` or `Fix` formatting supported. Defaults to `Fix`

The formatting flags defaults to `Right` alignment.
The `Spc` flag fills in a blank character for `+` if the number is positive.
The `Sign` flag fills in a `+` character if the number is positive.
If both `Spc` and `Sign` are given then `Sign` precedes.
*)
PROCEDURE (VAR s : Stream) FormatReal*(value : REAL; prec : INTEGER; width: LENGTH; flags: SET);
BEGIN Real.Format(s, value, prec, width, flags);
END FormatReal;

(**
Format `HUGECARD`.

* `base` : Number base. Defalts to 10.
* `width` : Total field with. Can overflow if number is bigger.

The formatting flags defaults to `Right` alignment.
The `Zero` flag fills with 0 of the formatting is right aligned.
The `Upper` flag the hex decimal letters are upper case.

The `Alt` flags prefix binary (base 2) numbers with `0b`,
octal numbers (base 8) with `0o` and hex decimal numbers
with either `0x` or `0X` depending on the `Upper` flag.
*)
PROCEDURE (VAR s : Stream) FormatCardinal*(value : HUGECARD; base, width: INTEGER; flags: SET);
BEGIN Cardinal.Format(s, value, base, width, flags);
END FormatCardinal;

(**
Format `DATETIME` according to format string arguments:

* `%a` : Weekday abbreviated name : Mon .. Sun
* `%A` : Weekday full name : Monday .. Sunday
* `%w` : Weekday as number : 0 .. 6
* `%b` : Month abbreviated name : Jan .. Des
* `%B` : Month full name : Januar .. Desember
* `%Y` : Year without century : 00 - 99
* `%y` : Year with century : 0000 - 9999
* `%m` : Month zero-padded : 00 - 12
* `%d` : Day of the month zero-padded : 01 - XX
* `%W` : Week of the year zero-padded : 01 - 53
* `%H` : Hour (24-hour clock) zero-padded : 00 - 23
* `%I` : Hour (12-hour clock) zero-padded : 1 - 12
* `%p` : AM or PM
* `%M` : Minute zero-padded : 00 - 59
* `%S` : Second zero-padded : 00 - 59
* `%f` : Milliseconds zero-padded : 000 - 999
* `%Z` : Timezone : UTC+/-
* `%%` : Literal `%` char

Other characters are copied to output.
*)
PROCEDURE (VAR s : Stream) FormatDateTime*(value : DateTime.DATETIME; fmt- : ARRAY OF CHAR);
BEGIN DateTime.Format(s, value, fmt);
END FormatDateTime;

(*
   NullStream
*)

(** Read bytes into buffer with optional start and length. *)
PROCEDURE (VAR s : NullStream) ReadBytes*(VAR buffer : ARRAY OF BYTE; start, length : LENGTH): LENGTH;
VAR i : LENGTH;
BEGIN
    IF length < 0 THEN length := LEN(buffer) END;
    i := start;
    WHILE i < length DO buffer[i] := 0; INC(i) END;
    RETURN i - start
END ReadBytes;

(** Write bytes from buffer with optional start and length. *)
PROCEDURE (VAR s : NullStream) WriteBytes*(VAR buffer : ARRAY OF BYTE; start, length : LENGTH): LENGTH;
BEGIN
    IF length < 0 THEN length := LEN(buffer) END;
    RETURN length - start
END WriteBytes;

(** Return `TRUE` if Stream is readable *)
PROCEDURE (VAR s : NullStream) Readable*(): BOOLEAN;
BEGIN RETURN TRUE END Readable;

(** Return `TRUE` if Stream is writeable *)
PROCEDURE (VAR s : NullStream) Writeable*(): BOOLEAN;
BEGIN RETURN TRUE END Writeable;

(** Return `TRUE` if Stream is seekable *)
PROCEDURE (VAR s : NullStream) Seekable*(): BOOLEAN;
BEGIN RETURN FALSE END Seekable;

(*
   MemoryStream
*)

(**
Open `MemoryStream` with optional size (minimum size is 64).

Return `TRUE` if success.
*)
PROCEDURE (VAR s : MemoryStream) Open*(size: LENGTH): BOOLEAN;
BEGIN
    IF size < 64 THEN size := 64 END;
    IF s.storage = NIL THEN
        NEW(s.storage, size);
        s.pos := 0;
        s.last := 0;
        RETURN TRUE
    END;
    RETURN FALSE
END Open;

(**
Copy Stream content to string.
The string is possible resized and is NUL terminated.
*)
PROCEDURE (VAR s : MemoryStream) ToString*(VAR str : Type.STRING);
BEGIN
    String.Reserve(str, s.last + 1, FALSE);
    ArrayOfByte.Copy(str^, s.storage, s.last);
    str[s.last] := 00X;
END ToString;

(** Read bytes into buffer with optional start and length. *)
PROCEDURE (VAR s : MemoryStream) ReadBytes*(VAR buffer : ARRAY OF BYTE; start, length: LENGTH): LENGTH;
VAR i : LENGTH;
BEGIN
    IF length < 0 THEN length := LEN(buffer) END;
    i := start;
    WHILE (s.pos < s.last) & (i < length) DO
        buffer[i] := s.storage[s.pos];
        INC(s.pos); INC(i)
    END;
    RETURN i - start
END ReadBytes;

(** Resize storage to accomodate capacity *)
PROCEDURE (VAR s : MemoryStream) Reserve*(capacity  : LENGTH);
VAR
    storage : MemoryStorage;
    cap : LENGTH;
BEGIN
    ASSERT(capacity > 0);
    cap := LEN(s.storage^);
    IF capacity > cap THEN
        WHILE cap < capacity DO cap := cap * 2 END;
        NEW(storage, cap);
        IF s.last > 0 THEN
            ArrayOfByte.Copy(storage, s.storage, s.last);
        END;
        DISPOSE(s.storage);
        s.storage := storage
    END;
END Reserve;

(** Shrink storage if possible *)
PROCEDURE (VAR s : MemoryStream) Shrink*();
VAR
    storage : MemoryStorage;
    cap : LENGTH;
BEGIN
    cap := LEN(s.storage^);
    IF cap > s.last + 1 THEN
        WHILE (cap > s.last) & (cap > 64) DO cap := cap DIV 2 END;
        IF cap < s.last THEN cap := cap * 2 END;
        NEW(storage, cap);
        IF s.last > 0 THEN
            ArrayOfByte.Copy(storage, s.storage, s.last);
        END;
        DISPOSE(s.storage);
        s.storage := storage;
    END;
END Shrink;

(** Append data to end and expand size if needed *)
PROCEDURE (VAR s : MemoryStream) Append(data : BYTE);
VAR capacity : LENGTH;
BEGIN
    capacity := LEN(s.storage^);
    IF s.last >= capacity THEN
        s.Reserve(capacity + 1)
    END;
    s.storage[s.last] := data;
    INC(s.last)
END Append;

(** Write bytes from buffer with optional start and length. *)
PROCEDURE (VAR s : MemoryStream) WriteBytes*(VAR buffer : ARRAY OF BYTE; start, length : LENGTH): LENGTH;
VAR i : LENGTH;
BEGIN
    IF length < 0 THEN length := LEN(buffer) END;
    i := start;
    WHILE (s.pos < s.last) & (i < length) DO
        s.storage[s.pos] := buffer[i];
        INC(s.pos); INC(i)
    END;
    WHILE i < length DO
        s.Append(buffer[i]);
        INC(s.pos); INC(i)
    END;
    RETURN i - start
END WriteBytes;

(**
Offsets or set the current location depending on the
mode argument:

 * `SeekSet` : sets position relative to start of stream.
 * `SeekCur` : sets position relative to current position of stream.
 * `SeekEnd` : sets position relative to end position of stream (only negative offset values makes sense).

Return new position or -1 in case of failure.
*)
PROCEDURE (VAR s : MemoryStream) Seek*(offset : LENGTH; mode : INTEGER): LENGTH;
BEGIN
    CASE mode OF
        Const.SeekSet  : s.pos := offset;
      | Const.SeekCur  : s.pos := s.pos + offset;
      | Const.SeekEnd  : s.pos := s.last + offset - 1;
    ELSE
        RETURN -1
    END;
    IF s.pos < 0 THEN s.pos := 0
    ELSIF s.pos > s.last THEN s.pos := s.last - 1 END;
    RETURN s.pos
END Seek;

(** Return current position or -1 on failure. *)
PROCEDURE (VAR s : MemoryStream) Tell*(): LENGTH;
BEGIN
    RETURN s.pos;
END Tell;

(**
Truncates or extends stream to new size.
Return new size or -1 in case of failure.
*)
PROCEDURE (VAR s : MemoryStream) Truncate*(size : LENGTH): LENGTH;
VAR i, cap : LENGTH;
BEGIN
    IF size <= 0 THEN size := 1 END;
    cap := LEN(s.storage^);
    s.Reserve(size);
    IF (cap > 4*64) & (LEN(s.storage^) < cap)  THEN s.Shrink() END;
    i := LEN(s.storage^) - 1;
    WHILE i >= size DO s.storage[i] := 0; DEC(i) END;
    IF size < s.last THEN s.last := size END;
    IF size < s.pos THEN s.pos := size - 1 END;
    RETURN size
END Truncate;

(** Close Stream *)
PROCEDURE (VAR s : MemoryStream) Close*();
BEGIN
    IF s.storage # NIL THEN
        DISPOSE(s.storage);
        s.storage := NIL;
        s.pos := 0;
        s.last := 0;
    ELSE s.error := Const.ErrorAlreadyOpen
    END;
END Close;

(** Return `TRUE` if Stream is closed *)
PROCEDURE (VAR s : MemoryStream) Closed*(): BOOLEAN;
BEGIN RETURN s.storage = NIL END Closed;

(** Return `TRUE` if Stream is readable *)
PROCEDURE (VAR s : MemoryStream) Readable*(): BOOLEAN;
BEGIN RETURN TRUE END Readable;

(** Return `TRUE` if Stream is writeable *)
PROCEDURE (VAR s : MemoryStream) Writeable*(): BOOLEAN;
BEGIN RETURN TRUE END Writeable;

(** Return `TRUE` if Stream is seekable *)
PROCEDURE (VAR s : MemoryStream) Seekable*(): BOOLEAN;
BEGIN RETURN TRUE END Seekable;

END ADTStream.