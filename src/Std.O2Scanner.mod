(* ETH Oberon, Copyright 2001 ETH Zuerich Institut fuer Computersysteme, ETH Zentrum, CH-8092 Zuerich.
Refer to the "General ETH Oberon System Source License" contract available at: http://www.oberon.ethz.ch/ *)

(* Oberon Non-Portable Scanner (front end) *)
(* NW, RC, ejz, rml, pjm, prk, rute *)
MODULE O2Scanner IN Std;

IN Std IMPORT Type;

CONST
	MaxStrLen*   = 256;
	MaxIdLen*     = 32;

	(* special character (< " ") returned by procedure Get, if end of text reached *)
	Eot* = 0X;

	(* parametrization of numeric scanner: *)
	MaxHDig* = 16;   (* maximal hexadecimal longint length *)
	MaxODig* = 23;   (* maximal octal longint length *)
	MaxRExp* = 38;   (* maximal real exponent *)
	MaxLExp* = 308;   (* maximal longreal exponent *)

	(* numtyp values *)
	char = 1; integer = 2; real = 3; longreal = 4;

	(*symbol values*)
	null* =   0; times* =   1; slash* =   2; div* =   3; mod* =   4; and* =   5;
	plus* =   6; minus* =   7; or* =   8; eql* =   9; neq* =  10; lss* =  11;
	leq* =  12; gtr* =  13; geq* =  14; in* =  15; is* =  16; arrow* =  17;
	period* =  18; comma* =  19; colon* =  20; upto* =  21; rparen* =  22;
	rbrak* =  23; rbrace* =  24; of* =  25; then* =  26; do* =  27; to* =  28;
	by* =  29; lparen* =  30; lbrak* =  31; lbrace* =  32; not* =  33;
	becomes* =  34; number* =  35; nil* =  36; true* =  37; false* =  38;
	string* =  39; ident* =  40; semicolon* =  41; bar* =  42; end* =  43;
	else* =  44; elsif* =  45; until* =  46; if* =  47; case* =  48; while* =  49;
	repeat* =  50; for* =  51; loop* =  52; with* =  53; exit* =  54;
	return* =  55; array* =  56; object* =  57; record* =  58; pointer* =  59;
	begin* =  60; code* =  61; const* =  62; type* =  63; var* =  64;
	procedure* =  65; import* =  66; module* =  67; eof* =  68; comment* = 69;

TYPE
	Name*    = ARRAY MaxIdLen OF CHAR;
	Str*  = ARRAY MaxStrLen OF CHAR;

	Scanner* = RECORD
		(* name, str, numtyp, intval, realval, lrlval are implicit results of Get *)
		name*    : Name;
		str*     : Str;
		numtyp*  : INTEGER; (* 1 = char, 2 = integer, 3 = real, 4 = longreal *)
		intval*  : LENGTH;   (* integer value or string length *)
		realval* : REAL32;
		lrlval*  : REAL64;
		cstart*  : LENGTH; (* Exported comment start *)
		cend*    : LENGTH; (* Exported comment end *)
		ch*      : CHAR;     (*current character*)
		error*   : INTEGER;
		stream   : POINTER TO VAR Type.Stream;
	END;

PROCEDURE Init* (VAR scanner : Scanner; VAR stream : Type.Stream);
BEGIN
	scanner.ch := " ";
 	scanner.error := 0;
	scanner.stream := PTR(stream);
END Init;

PROCEDURE (VAR this : Scanner) err(n: INTEGER);
BEGIN
    this.error := n;
END err;

PROCEDURE (VAR this : Scanner) Get();
BEGIN
    IF ~this.stream.ReadChar(this.ch)THEN
    	this.ch := Eot
    END;
END Get;

PROCEDURE (VAR this : Scanner) Str(VAR sym: INTEGER);
   VAR i: INTEGER; och: CHAR;
BEGIN i := 0; och := this.ch;
   LOOP this.Get();
      IF this.ch = och THEN EXIT END ;
      IF this.ch < " " THEN this.err(3); EXIT END ;
      IF i = MaxStrLen-1 THEN this.err(241); EXIT END ;
      this.str[i] := this.ch; INC(i)
   END ;
   this.Get(); this.str[i] := 0X; this.intval := i + 1;
   IF this.intval = 2 THEN
      sym := number; this.numtyp := 1; this.intval := ORD(this.str[0])
   ELSE sym := string
   END
END Str;

PROCEDURE (VAR this : Scanner) Identifier(VAR sym: INTEGER);
   VAR i: INTEGER;
BEGIN i := 0;
   REPEAT
      this.name[i] := this.ch; INC(i); this.Get()
   UNTIL ((this.ch < "0") OR ("9" < this.ch) & (CAP(this.ch) < "A") OR ("Z" < CAP(this.ch))) & (this.ch # "_")
      OR (i = MaxIdLen);
   IF i = MaxIdLen THEN this.err(240); DEC(i) END ;
   this.name[i] := 0X; sym := ident
END Identifier;

PROCEDURE (VAR this : Scanner) CheckCardinality*(nofp: SHORTINT): BOOLEAN;
BEGIN
   CASE this.name[0] OF
   | "+", "-": RETURN (nofp=1) OR (nofp=2)
   | "~": RETURN (this.name[1]=0X)&(nofp=1)
   ELSE RETURN nofp=2
   END
END CheckCardinality;

PROCEDURE (VAR this : Scanner) CheckOperator*(VAR id: INTEGER);
   VAR ch0, ch1, ch2: CHAR;
BEGIN id:=126; COPY(this.str, this.name);
   ch0:=this.str[0]; ch1:=this.str[1]; ch2:=this.str[2];
   CASE ch0 OF
   | "=", "#", "&": IF ch1=0X THEN RETURN END
   | "<", ">": IF (ch1=0X) OR ((ch1="=") & (ch2=0X)) THEN RETURN END   (* < , <=,  > , >= *)
   | "I": IF (ch1="N") & (ch2=0X) THEN RETURN END   (* IN *)
   | "D": IF this.str="DIV" THEN RETURN END   (* DIV *)
   | "M": IF this.str="MOD" THEN RETURN END   (* MOD *)
   | "O": IF this.str="OR" THEN RETURN END   (* OR *)
   | "+", "-", "*", "/" : IF (ch1=0X) OR ((ch2=0X) & (ch1="*")) THEN RETURN END;
   | "~": IF (ch1=0X) THEN RETURN END;
   | ":": IF this.str=":=" THEN RETURN END;
   ELSE
   END;
   id:=0
END CheckOperator;

PROCEDURE (VAR this : Scanner) Number*;
VAR i, m, n, d, e: INTEGER; dig: ARRAY 64 OF CHAR; f: REAL64; expCh: CHAR; neg: BOOLEAN;

   PROCEDURE Ten(e: INTEGER): REAL64;
      VAR x, p: REAL64;
   BEGIN x := 1; p := 10;
      WHILE e > 0 DO
         IF ODD(e) THEN x := x*p END;
         e := e DIV 2;
         IF e > 0 THEN p := p*p END (* prevent overflow *)
      END;
      RETURN x
   END Ten;

   PROCEDURE Ord(ch: CHAR; hex: BOOLEAN): INTEGER;
   BEGIN (* ("0" <= ch) & (ch <= "9") OR ("A" <= ch) & (ch <= "F") *)
      IF ch <= "9" THEN RETURN ORD(ch) - ORD("0")
      ELSIF hex THEN RETURN ORD(ch) - ORD("A") + 10
      ELSE this.err(2); RETURN 0
      END
   END Ord;

BEGIN (* ("0" <= ch) & (ch <= "9") *)
   i := 0; m := 0; n := 0; d := 0;
   LOOP (* read mantissa *)
      IF ("0" <= this.ch) & (this.ch <= "9") OR (d = 0) & ("A" <= this.ch) & (this.ch <= "F") THEN
         IF (m > 0) OR (this.ch # "0") THEN (* ignore leading zeros *)
            IF n < LEN(dig) THEN dig[n] := this.ch; INC(n) END;
            INC(m)
         END;
         this.Get(); INC(i)
      ELSIF this.ch = "'" THEN this.Get();
      	 IF ~(("0" <= this.ch) & (this.ch <= "9") OR (d = 0) & ("A" <= this.ch) & (this.ch <= "F")) THEN
      	 	ELSE this.err(2)
      	 END;
      ELSIF this.ch = "." THEN this.Get();
         IF this.ch = "." THEN (* ellipsis *) this.ch := 7FX; EXIT
         ELSIF d = 0 THEN (* i > 0 *) d := i
         ELSE this.err(2)
         END
      ELSE EXIT
      END
   END; (* 0 <= n <= m <= i, 0 <= d <= i *)
   IF d = 0 THEN (* integer *)
      IF n = m THEN this.intval := 0; i := 0;
         IF this.ch = "X" THEN (* character *) this.Get(); this.numtyp := char;
            IF n <= 2 THEN
               WHILE i < n DO this.intval := this.intval*10H + Ord(dig[i], TRUE); INC(i) END
            ELSE this.err(203)
            END
         ELSIF this.ch = "H" THEN (* hexadecimal *) this.Get(); this.numtyp := integer;
            IF n <= MaxHDig THEN
               IF (n = MaxHDig) & (dig[0] > "7") THEN (* prevent overflow *) this.intval := -1 END;
               WHILE i < n DO this.intval := this.intval*10H + Ord(dig[i], TRUE); INC(i) END
            ELSE this.err(203)
            END
         ELSIF this.ch = "O" THEN (* octal *) this.Get(); this.numtyp := integer;
            IF n <= MaxODig THEN
               WHILE i < n DO
               	IF dig[i] > '7' THEN this.err(203) END;
               	IF (n = MaxODig) & (dig[0] > "1") THEN (* prevent overflow *) this.intval := -1 END;
               	this.intval := this.intval*8 + Ord(dig[i], FALSE);
               	INC(i)
               END
            ELSE this.err(203)
            END
         ELSIF this.ch = "B" THEN (* binary *) this.Get(); this.numtyp := integer;
            IF n <= 64 THEN
               WHILE i < n DO
               	IF dig[i] > '1' THEN this.err(203) END;
               	this.intval := this.intval*2 + Ord(dig[i], FALSE);
               	INC(i)
               END
            ELSE this.err(203)
            END
         ELSE (* decimal *) this.numtyp := integer;
            WHILE i < n DO d := Ord(dig[i], FALSE); INC(i);
               IF this.intval <= (MAX(LONGINT) - d) DIV 10 THEN this.intval := this.intval*10 + d
               ELSE this.err(203)
               END
            END
         END
      ELSE this.err(203)
      END
   ELSE (* fraction *)
      f := 0; e := 0; expCh := "E";
      WHILE n > 0 DO (* 0 <= f < 1 *) DEC(n); f := (Ord(dig[n], FALSE) + f)/10 END;
      IF (this.ch = "E") OR (this.ch = "D") THEN expCh := this.ch; this.Get(); neg := FALSE;
         IF this.ch = "-" THEN neg := TRUE; this.Get()
         ELSIF this.ch = "+" THEN this.Get()
         END;
         IF ("0" <= this.ch) & (this.ch <= "9") THEN
            REPEAT n := Ord(this.ch, FALSE); this.Get();
               IF e <= (MAX(INTEGER) - n) DIV 10 THEN e := e*10 + n
               ELSE this.err(203)
               END
            UNTIL (this.ch < "0") OR ("9" < this.ch);
            IF neg THEN e := -e END
         ELSE this.err(2)
         END
      END;
      DEC(e, i-d-m); (* decimal point shift *)
      IF expCh = "E" THEN this.numtyp := real;
         IF (1-MaxRExp < e) & (e <= MaxRExp) THEN
            IF e < 0 THEN this.realval := SHORT(f / Ten(-e))
            ELSE this.realval := SHORT(f * Ten(e))
            END
         ELSE this.err(203)
         END
      ELSE this.numtyp := longreal;
         IF (1-MaxLExp < e) & (e <= MaxLExp) THEN
            IF e < 0 THEN this.lrlval := f / Ten(-e)
            ELSE this.lrlval := f * Ten(e)
            END
         ELSE this.err(203)
         END
      END
   END
END Number;

PROCEDURE (VAR this : Scanner) Next*(VAR sym: INTEGER);
   VAR s: INTEGER;

   PROCEDURE Comment;   (* do not read after end of file *)
   BEGIN this.Get();
      LOOP
         LOOP
            WHILE this.ch = "(" DO this.Get();
               IF this.ch = "*" THEN Comment END
            END ;
            IF this.ch = "*" THEN this.Get(); EXIT END ;
            IF this.ch = Eot THEN EXIT END ;
            this.Get()
         END ;
         IF this.ch = ")" THEN this.Get(); EXIT END ;
         IF this.ch = Eot THEN this.err(5); EXIT END
      END
   END Comment;

   PROCEDURE WingComment ();
   BEGIN
      REPEAT this.Get ()
      UNTIL this.ch < " "
   END WingComment;

   PROCEDURE CompilerDirective ();
   VAR prevTimes,done: BOOLEAN;
   BEGIN
      prevTimes := FALSE; done := FALSE;
      REPEAT
         this.Get ();
         IF this.ch = ">" THEN done := prevTimes
         ELSE prevTimes := this.ch = "*"
         END
      UNTIL done OR (this.ch = Eot);
      IF done THEN this.Get () END
   END CompilerDirective;

BEGIN
   WHILE this.ch <= " " DO (*ignore control characters*)
      IF this.ch = Eot THEN sym := eof; RETURN
      ELSE this.Get()
      END
   END;
   CASE this.ch OF   (* ch > " " *)
      | 22X, 27X  : this.Str(s)
      | "#"  : s := neq; this.name:="#"; this.Get()
      | "&"  : s :=  and; this.name:="&"; this.Get()
      | "("  : this.Get();
                   IF this.ch = "*" THEN
                        this.Get();
                        IF this.ch = "*" THEN (* Exported comments*)
                            this.cstart := this.stream.Tell() - 3;
                            Comment;
                            this.cend := this.stream.Tell() - 1;
                            s := comment;
                        ELSE Comment; this.Next(sym); RETURN   (*dont' record twice!*)
                        END;
                    ELSE s := lparen
                   END
      | ")"  : s := rparen; this.Get()
      | "*"  : this.name:="*"; s :=  times; this.Get()
      | "+"  : this.name:="+"; s :=  plus; this.Get()
      | ","  : s := comma; this.Get()
      | "-"  : this.Get ();
                  IF this.ch = "-" THEN WingComment; this.Next(sym); RETURN (* XDS wing comments *)
                  ELSE this.name := "-"; s := minus END
      | "."  : this.Get();
                   IF this.ch = "." THEN this.Get(); s := upto ELSE s := period END
      | "/"  : this.name:="/"; this.Get(); s :=  slash
      | "0".."9": this.Number; s := number
      | ":"  : this.Get();
                   IF this.ch = "=" THEN this.Get(); s := becomes; this.name:=":="  ELSE s := colon END
      | ";"  : s := semicolon; this.Get()
      | "<"  : this.Get();
                   IF this.ch = "=" THEN this.Get(); s := leq; this.name:="<="
                   ELSIF this.ch = "*" THEN CompilerDirective; this.Next(sym); RETURN (* XDS compiler directive *)
                   ELSE s := lss; this.name:="<" END
      | "="  : s :=  eql; this.name:="="; this.Get()
      | ">"  : this.Get();
                   IF this.ch = "=" THEN this.Get(); s := geq; this.name:=">=" ELSE s := gtr; this.name:=">" END
      | "_": this.Identifier(s); (* XDS supports underline characters in identifiers *)
      | "A": this.Identifier(s); IF this.name = "ARRAY" THEN s := array END
      | "B": this.Identifier(s);
               IF this.name = "BEGIN" THEN s := begin
               ELSIF this.name = "BY" THEN s := by
               END
      | "C": this.Identifier(s);
               IF this.name = "CASE" THEN s := case
               ELSIF this.name = "CODE" THEN s := code
               ELSIF this.name = "CONST" THEN s := const
               END
      | "D": this.Identifier(s);
               IF this.name = "DO" THEN s := do
               ELSIF this.name = "DIV" THEN s := div
               END
      | "E": this.Identifier(s);
               IF this.name = "END" THEN s := end
               ELSIF this.name = "ELSE" THEN s := else
               ELSIF this.name = "ELSIF" THEN s := elsif
               ELSIF this.name = "EXIT" THEN s := exit
               END
      | "F": this.Identifier(s);
               IF this.name = "FALSE" THEN s := false
               ELSIF this.name = "FOR" THEN s := for
               END
      | "I": this.Identifier(s);
               IF this.name = "IF" THEN s := if
               ELSIF this.name = "IN" THEN s := in
               ELSIF this.name = "IS" THEN s := is
               ELSIF this.name = "IMPORT" THEN s := import
               END
      | "L": this.Identifier(s); IF this.name = "LOOP" THEN s := loop END
      | "M": this.Identifier(s);
               IF this.name = "MOD" THEN s := mod
               ELSIF this.name = "MODULE" THEN s := module
               END
      | "N": this.Identifier(s); IF this.name = "NIL" THEN s := nil END
      | "O": this.Identifier(s);
               IF this.name = "OR" THEN s := or
               ELSIF this.name = "OF" THEN s := of
               ELSIF this.name = "OBJECT" THEN s := object
               END
      | "P": this.Identifier(s);
               IF this.name = "PROCEDURE" THEN s := procedure
               ELSIF this.name = "POINTER" THEN s := pointer
               END
      | "R": this.Identifier(s);
               IF this.name = "RECORD" THEN s := record
               ELSIF this.name = "REPEAT" THEN s := repeat
               ELSIF this.name = "RETURN" THEN s := return
               END
      | "T": this.Identifier(s);
               IF this.name = "THEN" THEN s := then
               ELSIF this.name = "TO" THEN s := to
               ELSIF this.name = "TRUE" THEN s := true
               ELSIF this.name = "TYPE" THEN s := type
               END
      | "U": this.Identifier(s); IF this.name = "UNTIL" THEN s := until END
      | "V": this.Identifier(s); IF this.name = "VAR" THEN s := var END
      | "W": this.Identifier(s);
               IF this.name = "WHILE" THEN s := while
               ELSIF this.name = "WITH" THEN s := with
               END
      | "G".."H", "J", "K", "Q", "S", "X".."Z": this.Identifier(s)
      | "["  : s := lbrak; this.Get()
      | "]"  : s := rbrak; this.Get()
      | "^"  : s := arrow; this.Get()
      | "a".."z": this.Identifier(s)
      | "{"  : s := lbrace; this.Get()
      | "|"  : s := bar; this.Get()
      | "}"  : s := rbrace; this.Get()
      | "~"  : s := not; this.name:="~"; this.Get()
      | 7FX  : s := upto; this.Get()
   ELSE s :=  null; this.Get()
   END;
   sym := s;
END Next;

END O2Scanner.