MODULE DataConfig IN Std;

IN Std IMPORT Char, Str := ArrayOfChar, String, Type;
IN Std IMPORT DictStrStr := ADTDictionary(String.STRING, String.STRING, String.Hash, String.Equal);
IN Std IMPORT SetStr := ADTSet(String.STRING, String.Hash, String.Equal);

TYPE
    VectorOfSections* = SetStr.ElementVector;
    Parser* = RECORD
        sections* : SetStr.Set;
        entries* : DictStrStr.Dictionary;
    END;

(** Initialize Parser *)
PROCEDURE InitParser*(VAR parser : Parser);
BEGIN
    parser.sections.Init(2);
    parser.sections.duplicate := String.Duplicate;
    parser.sections.dispose := String.Dispose;
    parser.entries.Init(4);
    parser.entries.duplicateKey := String.Duplicate;
    parser.entries.disposeKey := String.Dispose;
    parser.entries.duplicateValue := String.Duplicate;
    parser.entries.disposeValue := String.Dispose;
END InitParser;

(** Deallocate data *)
PROCEDURE (VAR this : Parser) Dispose*();
BEGIN
    this.sections.Dispose();
    this.entries.Dispose();
END Dispose;

(** Clear data *)
PROCEDURE (VAR this : Parser) Clear*();
BEGIN
    this.sections.Clear();
    this.entries.Clear();
END Clear;

(**
Get config value.
Return `TRUE` if success.
*)
PROCEDURE (VAR this- : Parser) Get*(VAR value : String.STRING; section-, key- : ARRAY OF CHAR) : BOOLEAN;
VAR
    ikey : String.STRING;
    i : LENGTH;
    ret : BOOLEAN;
BEGIN
    IF (Str.Length(section) = 0) OR (Str.Length(key) = 0) THEN RETURN FALSE END;
    IF Str.IndexChar("=", section, 0) # -1 THEN RETURN FALSE END;
    IF Str.IndexChar("=", key, 0) # -1 THEN RETURN FALSE END;
    String.Reserve(ikey, Str.Length(section) + Str.Length(key) + 1, FALSE);
    FOR i := 0 TO Str.Length(section) - 1 DO
        String.AppendChar(ikey, Char.Lower(section[i]))
    END;
    String.AppendChar(ikey, "=");
    FOR i := 0 TO Str.Length(key) - 1 DO
        String.AppendChar(ikey, Char.Lower(key[i]))
    END;
    ret := this.entries.Get(ikey, value);
    String.Dispose(ikey);
    RETURN ret
END Get;

(**
Set config value and add section if missing.
Return `TRUE` if success.
*)
PROCEDURE (VAR this : Parser) Set*(section-, key-, value- : ARRAY OF CHAR) : BOOLEAN;
VAR
    tmp, ikey : String.STRING;
BEGIN
    IF (Str.Length(section) = 0) OR (Str.Length(key) = 0) THEN RETURN FALSE END;
    IF Str.IndexChar("=", section, 0) # -1 THEN RETURN FALSE END;
    IF Str.IndexChar("=", key, 0) # -1 THEN RETURN FALSE END;
    String.Reserve(ikey, Str.Length(section) + Str.Length(key) + 1, FALSE);
    (* Remove surrounding white space and transform to lower case *)
    String.Assign(ikey, section);
    Str.Trim(ikey^);
    Str.LowerCase(ikey^);
    (* add section if missing *)
    IF ~this.sections.In(ikey) THEN
        this.sections.Incl(ikey);
    END;
    String.AppendChar(ikey, "=");
    (* Remove surrounding white space and transform to lower case *)
    String.Assign(tmp, key);
    Str.Trim(tmp^);
    Str.LowerCase(tmp^);
    String.Append(ikey, tmp^);
    (* update or set value *)
    this.entries.Set(ikey, String.S(tmp, value));
    String.Dispose(tmp); 
    String.Dispose(ikey);
    RETURN TRUE
END Set;

(**
Delete config value.
Return `TRUE` if success.
*)
PROCEDURE (VAR this : Parser) Delete*(section-, key- : ARRAY OF CHAR) : BOOLEAN;
VAR
    ikey : String.STRING;
    i : LENGTH;
    ret : BOOLEAN;
BEGIN
    IF (Str.Length(section) = 0) OR (Str.Length(key) = 0) THEN RETURN FALSE END;
    IF Str.IndexChar("=", section, 0) # -1 THEN RETURN FALSE END;
    IF Str.IndexChar("=", key, 0) # -1 THEN RETURN FALSE END;
    String.Reserve(ikey, Str.Length(section) + Str.Length(key) + 1, FALSE);
    FOR i := 0 TO Str.Length(section) - 1 DO
        String.AppendChar(ikey, Char.Lower(section[i]))
    END;
    String.AppendChar(ikey, "=");
    FOR i := 0 TO Str.Length(key) - 1 DO
        String.AppendChar(ikey, Char.Lower(key[i]))
    END;
    ret := this.entries.Remove(ikey);
    String.Dispose(ikey);
    RETURN ret
END Delete;

(** Return `TRUE` if section exists *)
PROCEDURE (VAR this- : Parser) HasSection*(section- : ARRAY OF CHAR): BOOLEAN;
VAR
    ikey : String.STRING;
    i : LENGTH;
    ret : BOOLEAN;
BEGIN
    IF (Str.Length(section) = 0) THEN RETURN FALSE END;
    IF Str.IndexChar("=", section, 0) # -1 THEN RETURN FALSE END;
    String.Reserve(ikey, Str.Length(section), FALSE);
    FOR i := 0 TO Str.Length(section) - 1 DO
        String.AppendChar(ikey, Char.Lower(section[i]))
    END;
    ret := this.sections.In(ikey);
    String.Dispose(ikey);
    RETURN ret
END HasSection;

(** Extract Vector of sections *)
PROCEDURE (VAR this- : Parser) Sections*(): VectorOfSections;
BEGIN RETURN this.sections.Elements()
END Sections;

(** Write data to Stream *)
PROCEDURE (VAR this- : Parser) Write*(VAR fh : Type.Stream): BOOLEAN;
VAR
    key, value, section : String.STRING;
    sections : VectorOfSections;
    it : DictStrStr.Iterator;
    i : LENGTH;
    first : BOOLEAN;
BEGIN
    (* IF fh.Closed() OR ~fh.Writeable() THEN RETURN FALSE END; *)
    sections := this.Sections();
    sections.Sort(String.Compare);
    FOR i := 0 TO sections.Size() - 1 DO
        section := sections.At(i);
        first := FALSE;
        this.entries.First(it);
        WHILE it.NextItem(key, value) DO
            IF Str.StartsWith(key^, section^) &
               (Str.IndexChar("=", key^, 0) = Str.Length(section^)) THEN
                IF ~first THEN
                    fh.WriteChar("["); fh.WriteString(section^); fh.WriteChar("]"); fh.WriteNL;
                    first := TRUE;
                END;
                Str.Delete(key^, 0, Str.Length(section^) + 1);
                fh.WriteString(key^); fh.WriteString(" = ");
                fh.WriteString(value^); fh.WriteNL;
            END;
        END;
        String.Dispose(section);
    END;
    String.Dispose(key); String.Dispose(value);
    sections.Dispose;
    RETURN TRUE
END Write;

(**
Read from Stream. Return 0 on success.
Positive return value indicate line with error.
Negative return value indicate internal error.
This operation will try to append the new data.
Clear the data before operation if this is not intended.
*)
PROCEDURE (VAR this : Parser) Read*(VAR fh : Type.Stream): INTEGER;
VAR
    line, value, name, section: String.STRING;
    i, j, slen : INTEGER;
    ch : CHAR;
    PROCEDURE Next;
    BEGIN IF i < LEN(line^) THEN ch := line^[i]; INC(i) ELSE ch := 00X END
    END Next;
    PROCEDURE Skip();
    BEGIN WHILE Char.IsSpace(ch) DO Next() END;
    END Skip;
    PROCEDURE Dispose();
    BEGIN 
        String.Dispose(line); String.Dispose(value);
        String.Dispose(name); String.Dispose(section)
    END Dispose;
BEGIN
    String.Assign(section, ""); String.Assign(name, ""); String.Assign(value, "");
    j := 1; slen := 0;
    WHILE fh.ReadLine(line) DO
        i := 0;
        Next; Skip();
        IF (ch = "#") OR (ch = ";") OR (ch = 00X) THEN
            ;
        ELSIF ch = "[" THEN
            Str.Clear(section^);
            Next;
            WHILE (ch # "]") & (ch # 00X) DO
                String.AppendChar(section, Char.Lower(ch));
                Next
            END;
            IF ch # "]" THEN Dispose; RETURN j END;
            Next; Skip();
            IF ch # 00X THEN Dispose; RETURN j END;
            IF Str.Length(section^) = 0 THEN Dispose; RETURN j END;
            IF this.sections.In(section) THEN Dispose; RETURN j END;
            this.sections.Incl(section);
            INC(slen)
        ELSE
            IF slen = 0 THEN Dispose; RETURN j END;
            Str.Clear(name^);
            WHILE (ch # "=") & (ch # 00X) DO
                String.AppendChar(name, Char.Lower(ch));
                Next
            END;
            Str.RightTrim(name^);
            IF Str.Length(name^) = 0 THEN Dispose; RETURN j END;
            IF ch = "=" THEN Next END;
            Skip();
            Str.Clear(value^);
            WHILE ch # 00X DO
                String.AppendChar(value, ch);
                Next
            END;
            Str.RightTrim(value^);
            IF ~this.Set(section^, name^, value^) THEN
                Dispose; RETURN -2
            END;
        END;
        INC(j);
    END;
    Dispose;
    RETURN 0
END Read;

END DataConfig.
