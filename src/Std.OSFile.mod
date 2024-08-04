(** Module for operating on OS files *)
MODULE OSFile IN Std;

IN Std IMPORT OSHost, DateTime;

(** Check if file exists *)
PROCEDURE Exists*(filename-: ARRAY OF CHAR): BOOLEAN;
BEGIN RETURN OSHost.FileExists(filename);
END Exists;

(** Try to delete file. Return `TRUE` on success *)
PROCEDURE Delete*(filename-: ARRAY OF CHAR): BOOLEAN;
BEGIN RETURN OSHost.FileRemove(filename);
END Delete;

(** Try to rename file. Return `TRUE` on success *)
PROCEDURE Rename*(oldname-, newname-: ARRAY OF CHAR): BOOLEAN;
BEGIN RETURN OSHost.FileRename(oldname, newname);
END Rename;

(** Try to get file access time. Return `TRUE` on success *)
PROCEDURE ModifyTime*(VAR time : DateTime.DATETIME; filename-: ARRAY OF CHAR): BOOLEAN;
VAR dt : OSHost.DateTime;
BEGIN
    IF ~OSHost.FileModificationTime(dt, filename) THEN RETURN FALSE END;
    RETURN DateTime.TryEncodeDateTime(time,
    dt.year, dt.month, dt.day, dt.hour, dt.min, dt.sec, dt.msec
    );
END ModifyTime;

END OSFile.