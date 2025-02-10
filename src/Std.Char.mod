(** Module with operation on `CHAR` type. *)
MODULE Char IN Std;

CONST
    NUL* = 00X;
    TAB* = 09X;
    LF* = 0AX;
    CR* = 0DX;
    SPC* = 20X;
    DEL* = 7FX;

(** Returns true of ch is a control character *)
PROCEDURE IsControl* (ch: CHAR) : BOOLEAN ;
BEGIN RETURN (ch <= SPC) OR (ch = DEL)
END IsControl;

(** Returns true of ch is a graphical character *)
PROCEDURE IsGraph* (ch: CHAR) : BOOLEAN ;
BEGIN RETURN (ch >= 21X) & (ch <= 7EX)
END IsGraph;

(** Returns true of ch is a punctuation character *)
PROCEDURE IsPunct* (ch: CHAR) : BOOLEAN ;
BEGIN RETURN ((ch >= 21X) & (ch <= 2FX)) OR
             ((ch >= 3AX) & (ch <= 40X)) OR
             ((ch >= 5BX) & (ch <= 60X)) OR
             ((ch >= 7BX) & (ch <= 7EX))
END IsPunct;

(** Returns true of ch is a digit *)
PROCEDURE IsDigit* (ch: CHAR) : BOOLEAN ;
BEGIN RETURN (ch >= '0') & (ch <= '9')
END IsDigit;

(** Returns true of ch is a hex digit *)
PROCEDURE IsHexDigit* (ch: CHAR) : BOOLEAN ;
BEGIN RETURN ((ch >= '0') & (ch <= '9')) OR
             ((ch >= 'a') & (ch <= 'f')) OR
             ((ch >= 'A') & (ch <= 'F'))
END IsHexDigit;

(** Returns true of ch is a alphabet letter *)
PROCEDURE IsAlpha* (ch: CHAR) : BOOLEAN ;
BEGIN RETURN ((ch >= 'a') & (ch <= 'z')) OR ((ch >= 'A') & (ch <= 'Z')) 
END IsAlpha;

(** Returns true of ch is a alphabet letter or number *)
PROCEDURE IsAlphaNum* (ch: CHAR) : BOOLEAN ;
BEGIN RETURN ((ch >= 'a') & (ch <= 'z')) OR
             ((ch >= 'A') & (ch <= 'Z')) OR
             ((ch >= '0') & (ch <= '9'))
END IsAlphaNum;

(** Returns true of ch is a white space character *)
PROCEDURE IsSpace* (ch: CHAR) : BOOLEAN ;
BEGIN RETURN (ch = TAB) OR (ch = LF) OR (ch = CR) OR (ch = SPC)
END IsSpace;

(** Returns true of ch is a lower case letter *)
PROCEDURE IsLower* (ch: CHAR) : BOOLEAN ;
BEGIN RETURN (ch >= 'a') & (ch <= 'z')
END IsLower;

(** Returns true of ch is a upper case letter *)
PROCEDURE IsUpper* (ch: CHAR) : BOOLEAN ;
BEGIN RETURN (ch >= 'A') & (ch <= 'Z')
END IsUpper;

(** Returns lower case letter or unmodified char *)
PROCEDURE Lower* (ch: CHAR) : CHAR ;
BEGIN
    IF IsUpper(ch) THEN RETURN CHR(ORD(ch) - (ORD('A') - ORD('a'))) END;
    RETURN ch;
END Lower;

(** Returns upper case letter or unmodified char *)
PROCEDURE Upper* (ch: CHAR) : CHAR ;
BEGIN
    IF IsLower(ch) THEN RETURN CHR(ORD(ch) + (ORD('A') - ORD('a'))) END;
    RETURN ch;
END Upper;

END Char.