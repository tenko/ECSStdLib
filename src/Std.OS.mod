(**
Module for basic OS functionality
*)
MODULE OS IN Std;

IN Std IMPORT String, Char, Str := ArrayOfChar, OSHost;

CONST
    Unknown*    = 0;
    Position*   = 1;
    Flag*       = 2;
    Parameter*  = 3;

TYPE
    Argument* =
    RECORD
        name*   : String.STRING;
        value*  : String.STRING;
        index*  : LENGTH;
        type*   : LENGTH
    END;

(**
Get program name
*)
PROCEDURE ProgramName*(VAR name : String.STRING);
BEGIN
    IF OSHost.ProgramNameLength() = 0 THEN RETURN END;
    String.Reserve(name, OSHost.ProgramNameLength() + 1, FALSE);
    Str.Clear(name^);
    OSHost.ProgramName(name^)
END ProgramName;

(**
Get number of program arguments
*)
PROCEDURE Args*(): LENGTH;
BEGIN RETURN OSHost.Args()
END Args;

(**
Get n-th argument
*)
PROCEDURE Arg*(VAR str : String.STRING; n : LENGTH);
BEGIN
    IF OSHost.ArgLength(n) = 0 THEN
        Str.Clear(str^);
        RETURN
    END;
    String.Reserve(str, OSHost.ArgLength(n) + 1, FALSE);
    Str.Clear(str^);
    OSHost.Arg(str^, n)
END Arg;

(**
Initialize argument parser.

The parser support a on purpose limited for unambigious
parsing. The POSIX type of arguments with space between
flag and value is ambigious and therfore not supported.

* `-f`, sets type to `Flag` and name to `f`. value is empty.
* `-fval1`, sets type to `Parameter`, name to `f` and value to `val1`

Only alpha numeric characters are supported for short arguments.

* `--flag`, sets type to `Flag` and name to `flag`. value is empty.
* `-flag=val1`, sets type to `Parameter`, name to `flag` and value to `val1`

Any character is valid after `=`.
Whitespace is supported by enclosing the value in quotes.

An invalid argument is marked with type set to `Unknown` and
with value set the argument.

Other arguments are returned as type `Position` and
with value set to the argument.
*)
PROCEDURE InitArgument*(VAR arg : Argument);
BEGIN
    arg.index := -1;
    String.Assign(arg.name, "");
    String.Assign(arg.value, "");
    arg.type := Unknown;
END InitArgument;

(**
Fetch next argument or return `FALSE` if finished
*)
PROCEDURE NextArgument*(VAR arg : Argument): BOOLEAN;
VAR
    str : String.STRING;
    idx : LENGTH;
    c : CHAR;
    
    PROCEDURE Next();
    BEGIN
        IF idx < LEN(str^) THEN c := str^[idx]; INC(idx)
        ELSE c := Char.NUL END;
    END Next;
    PROCEDURE Peek() : CHAR;
    BEGIN
        IF idx < LEN(str^) THEN RETURN str^[idx]
        ELSE RETURN Char.NUL END;
    END Peek;
BEGIN
    idx := 0;
    Str.Clear(arg.name^);
    Str.Clear(arg.value^);
    arg.type := Unknown;
    
    INC(arg.index);
    IF arg.index >= Args() THEN
        arg.index := -1;
        RETURN FALSE
    END;

    Arg(str, arg.index);
    IF Peek() = '-' THEN
        Next();
        IF Peek() = '-' THEN (* Long type *)
            Next(); Next();
            LOOP
                IF (c = Char.NUL) OR (c = '=') THEN EXIT END;
                IF ~Char.IsAlpha(c) & ~Char.IsDigit(c) THEN EXIT END;
                String.AppendChar(arg.name, c);
                Next()
            END;
            IF (c = Char.NUL) & (Str.Length(arg.name^) > 0) THEN
                arg.type := Flag;
                RETURN TRUE
            ELSIF c = '=' THEN
                Next();
                LOOP
                    IF c = Char.NUL THEN EXIT END;
                    String.AppendChar(arg.value, c);
                    Next()
                END;
                arg.type := Parameter;
                RETURN TRUE
            END;
            Str.Clear(arg.name^);
            String.Assign(arg.value, str^);
            RETURN TRUE
        ELSE (* Short type *)
            Next();
            IF Char.IsAlpha(c) OR Char.IsDigit(c) THEN
                String.AppendChar(arg.name, c);
                Next();
                LOOP
                    IF c = Char.NUL THEN EXIT END;
                    IF ~Char.IsAlpha(c) & ~Char.IsDigit(c) THEN EXIT END;
                    String.AppendChar(arg.value, c);
                    Next()
                END;
                IF c = Char.NUL THEN
                    IF Str.Length(arg.value^) > 0 THEN
                        arg.type := Parameter;
                    ELSE
                        arg.type := Flag;
                    END;
                    RETURN TRUE
                END;
            END;
            Str.Clear(arg.name^);
            String.Assign(arg.value, str^);
            RETURN TRUE
        END
    ELSE
        arg.type := Position;
        String.Assign(arg.value, str^)
    END;
    RETURN TRUE
END NextArgument;

(** Check if environment variable exists *)
PROCEDURE HasEnv*(name- : ARRAY OF CHAR): BOOLEAN;
BEGIN RETURN OSHost.EnvVarLength(name) > 0
END HasEnv;

(** Get environment variable *)
PROCEDURE Env*(VAR value : String.STRING; name- : ARRAY OF CHAR);
BEGIN
    IF OSHost.EnvVarLength(name) = 0 THEN RETURN END;
    String.Reserve(value, OSHost.EnvVarLength(name) + 1, FALSE);
    Str.Clear(value^);
    OSHost.EnvVar(value^, name)
END Env;

(** Get last error code or OK on no error. *)
PROCEDURE GetLastError*(VAR error: INTEGER);
BEGIN OSHost.GetLastError(error)
END GetLastError;

(** Exit with return code *)
PROCEDURE Exit*(code : INTEGER);
BEGIN OSHost.Exit(code)
END Exit;

END OS.