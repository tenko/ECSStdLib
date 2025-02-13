(**
String pattern matching. Syntax follow Lua style patterns and is a simplification
compared to traditional regex syntax format.

Character classes
 * `%a` - letters
 * `%c` - control characters
 * `%d` - digits
 * `%g` - graphical characters
 * `%l` - lower case letters
 * `%p` - punctuation characters
 * `%s` - white space characters
 * `%u` - upper case letters
 * `%w` - alphanumeric characters
 * `%x` - hexadecimal digits

The upper case version of the character class above represent the inverse version.
Special characters are escaped by prefixing with `%`. e.g. `%%` represent the
literal percent character.

Ranges and sets
 * `[aBc]` - represent letters `a`, `B` & `c`.
 * `[%a_]` - represent all letters and the underscore character.
 * `[0-0]` - represent all digits.

A range or set with `^` as the first character is the inverse version.

Repetitions
 * `*` - Match the previous character (or class) zero or more times, as many times as possible.
 * `+` - Match the previous character (or class) one or more times, as many times as possible.
 * `-` - Match the previous character (or class) zero or more times, as few times as possible.
 * `?` - Make the previous character (or class) optional.

Anchors
 * `^` - Match the start of the input string
 * `$` - Match the end of the input string.

Special
 * `%n` - for n between 1 and 9; such item matches a substring equal to the n-th captured string.
 * `%bxy` - where x and y are two distinct characters; such item matches strings that start with x, end with y, and where the x and y are balanced.
 * `%f[set]` - a frontier pattern; such item matches an empty string at any position such that the next character belongs to set and the previous character does not belong to set.

Patterns enclosed in parantheses marks a capture and is saved according to index count.
An empty paranthese will capture just the current index in the string.

Further example and more complete documentation can be found online in the official lua documentation
on this subject.

This module is ported from the code from patterns(7) from OpenBSD's httpd(8), which in turn is based on the
pattern-matching code from the Lua language. License is MIT.
*)
MODULE StringPattern IN Std;

IN Std IMPORT Char, ArrayOfChar;

CONST
    ERROR_NO_MATCH*                 = -1;
    ERROR_TO_COMPLEX*               = -2;
    ERROR_MALFORMED_PATTERN*        = -3;
    ERROR_MAX_REPITITIONS*          = -4;
    ERROR_INVALID_CAPTURE_IDX*      = -5;
    ERROR_INVALID_PATTERN_CAPTURE*  = -6;
    ERROR_TO_MANY_CAPTURE*          = -7;
	MAXCAPTURES		    = 32;		(* Max no. of allowed captures in pattern *)
	MAXCCALLS		    = 200;		(* Max recusion depth in pattern matching *)
	MAXREPETITION	    = 0FFFFFH;	(* Max for repetition items *)
    CAP_UNFINISHED	    = -1;
    CAP_POSITION	    = -2;

TYPE
	StringMatch = RECORD
		start, len : LENGTH;
	END;
	
	Pattern* = RECORD
		matchdepth : INTEGER; (* control of recursive depth to avoid stack overflow *)
		repetitioncounter : INTEGER; (* control the repetition items *)
		maxcaptures : INTEGER; (* configured capture limit *)
        sinit, slen : LENGTH; (* start & len match in src *)
        send, pend : LENGTH; (* end index of src, pat *)
		error- : INTEGER; (* Should be 0 *)
		level- : INTEGER; (* total number of captures (finished or unfinished) *)
		capture : ARRAY MAXCAPTURES OF StringMatch;
	END;

PROCEDURE ^ SingleMatch(VAR this : Pattern; pat-, src- : ARRAY OF CHAR; pidx, sidx, ep : LENGTH): BOOLEAN;
PROCEDURE ^ MatchAux(VAR this : Pattern; pat-, src- : ARRAY OF CHAR; pidx, sidx : LENGTH): LENGTH;

PROCEDURE MatchError(VAR this : Pattern; error : INTEGER);
BEGIN
    IF this.error = 0 THEN
        this.error := error
    END;
END MatchError;

PROCEDURE CheckCapture(VAR this : Pattern; l : CHAR): INTEGER;
VAR
    i : INTEGER;
BEGIN
    i := ORD(l) - ORD('1');
    IF (i < 0) OR (i > this.level) OR (this.capture[i].len = CAP_UNFINISHED) THEN
        MatchError(this, ERROR_INVALID_CAPTURE_IDX);
        RETURN -1
    END;
    RETURN i;
END CheckCapture;

PROCEDURE CaptureToClose(VAR this : Pattern): INTEGER;
VAR
    level : INTEGER;
BEGIN
    level := this.level;
    DEC(level);
    WHILE level >= 0 DO
        IF this.capture[level].len = CAP_UNFINISHED THEN
            RETURN level
        END;
        DEC(level);
    END;
    MatchError(this, ERROR_INVALID_PATTERN_CAPTURE);
    RETURN -1;
END CaptureToClose;

PROCEDURE NoSpecials(VAR this : Pattern; pat- : ARRAY OF CHAR): BOOLEAN;
VAR i : LENGTH;
BEGIN
	FOR i := 0 TO this.pend DO
		CASE pat[i] OF
			"^","$","*","+","?", ".","(","[","%","-", '"' : RETURN FALSE;
		ELSE
			;
		END;
	END;
	RETURN TRUE
END NoSpecials;

PROCEDURE MaxExpand(VAR this : Pattern; pat-, src- : ARRAY OF CHAR; pidx, sidx, ep : LENGTH): LENGTH;
VAR
    i, res : LENGTH;
BEGIN
    i := 0;
    (* counts maximum expand for item *)
    WHILE SingleMatch(this, pat, src, pidx, sidx + i, ep) DO INC(i) END;
    (* keeps trying to match with the maximum repetitions *)
    WHILE i >= 0 DO
        res := MatchAux(this, pat, src, ep + 1, sidx + i);
        IF res >= 0 THEN RETURN res END;
        DEC(i);
    END;
    RETURN -1
END MaxExpand;

PROCEDURE MinExpand(VAR this : Pattern; pat-, src- : ARRAY OF CHAR; pidx, sidx, ep : LENGTH): LENGTH;
VAR
    res : LENGTH;
BEGIN
    LOOP
        res := MatchAux(this, pat, src, ep + 1, sidx);
        IF res >= 0 THEN RETURN res
        ELSIF SingleMatch(this, pat, src, pidx, sidx, ep) THEN
            INC(sidx)
        ELSE
            RETURN -1
        END;
    END;
END MinExpand;

PROCEDURE ClassEnd(VAR this : Pattern; pat- : ARRAY OF CHAR; pidx: LENGTH): LENGTH;
VAR
    c : CHAR;
BEGIN
    c := pat[pidx];
    INC(pidx);
    CASE c OF
          '%' :
            IF pidx > this.pend THEN
                MatchError(this, ERROR_MALFORMED_PATTERN)
            END;
            RETURN pidx + 1
        | '[' :
            IF pat[pidx] = '^' THEN INC(pidx) END;
            LOOP
                (* look for a ']' *)
                IF pidx > this.pend THEN
                    MatchError(this, ERROR_MALFORMED_PATTERN);
                    EXIT
                END;
                c := pat[pidx];
                INC(pidx);
                IF (c = '%') & (pidx <= this.pend) THEN
                    (* skip escapes (e.g. '%]') *)
                    INC(pidx)
                END;
                IF (pidx <= this.pend) & (pat[pidx] = ']') THEN EXIT END;
            END;
            RETURN pidx + 1
    ELSE
        RETURN pidx
    END;
END ClassEnd;

PROCEDURE MatchClass(c, cl : CHAR): BOOLEAN;
VAR
    res : BOOLEAN;
BEGIN
    CASE Char.Lower(cl) OF
          'a' : res := Char.IsAlpha(c)
        | 'c' : res := Char.IsControl(c)
        | 'd' : res := Char.IsDigit(c)
        | 'g' : res := Char.IsGraph(c)
        | 'l' : res := Char.IsLower(c)
        | 'p' : res := Char.IsPunct(c)
        | 's' : res := Char.IsSpace(c)
        | 'u' : res := Char.IsUpper(c)
        | 'w' : res := Char.IsAlphaNum(c)
        | 'x' : res := Char.IsHexDigit(c)
    ELSE
        RETURN c = cl
    END;
    IF Char.IsLower(cl) THEN
        RETURN res
    ELSE
        RETURN ~res
    END;
END MatchClass;

PROCEDURE MatchBracketClass(c : CHAR; pat- : ARRAY OF CHAR; pidx, ec: LENGTH): BOOLEAN;
VAR
    sig : BOOLEAN;
BEGIN
    sig := TRUE;
    IF pat[pidx + 1] = '^' THEN
        sig := FALSE;
        (* skip the '^' *)
        INC(pidx);
    END;
    LOOP
        INC(pidx);
        IF pidx > ec THEN EXIT END;
        IF pat[pidx] = '%' THEN
            INC(pidx);
            IF MatchClass(c, pat[pidx]) THEN
                RETURN sig;
            END;
        ELSIF (pat[pidx + 1] = '-') & (pidx + 2 <= ec) THEN
            INC(pidx, 2);
            IF (pat[pidx - 2] <= c) & (c <= pat[pidx]) THEN
                RETURN sig
            END;
        ELSIF pat[pidx] = c THEN
            RETURN sig
        END;
    END;
    RETURN ~sig
END MatchBracketClass;

PROCEDURE MatchBalance(VAR this : Pattern; pat-, src- : ARRAY OF CHAR; pidx, sidx: LENGTH): LENGTH;
VAR
    b, e : CHAR;
    cont : INTEGER;
BEGIN
    IF pidx > this.pend - 1 THEN
        MatchError(this, ERROR_MALFORMED_PATTERN);
        RETURN -1
    END;
    IF pat[pidx] = src[sidx] THEN
        b := pat[pidx];
        e := pat[pidx + 1];
        cont := 1;
        LOOP
            INC(sidx);
            IF sidx > this.send THEN EXIT END;
            IF src[sidx] = e THEN
                DEC(cont);
                IF cont = 0 THEN RETURN sidx + 1 END;
            ELSIF src[sidx] = b THEN
                INC(cont);
            END;
        END;
    END;
    RETURN -1
END MatchBalance;

PROCEDURE SingleMatch(VAR this : Pattern; pat-, src- : ARRAY OF CHAR; pidx, sidx, ep : LENGTH): BOOLEAN;
BEGIN
    IF sidx > this.send THEN RETURN FALSE END;
    CASE pat[pidx] OF
          '.' :
            (* matches any char *)
            RETURN TRUE;
        | '%' :
            RETURN MatchClass(src[sidx], pat[pidx + 1])

        | '[' :
            RETURN MatchBracketClass(src[sidx], pat, pidx, ep - 1)
    ELSE
        RETURN pat[pidx] = src[sidx]
    END;
END SingleMatch;

PROCEDURE StartCapture(VAR this : Pattern; pat-, src- : ARRAY OF CHAR; pidx, sidx, what: LENGTH): LENGTH;
VAR
    res : LENGTH;
    level : INTEGER;
BEGIN
    level := this.level;
    IF level >= this.maxcaptures THEN
        MatchError(this, ERROR_TO_MANY_CAPTURE);
        RETURN -1
    END;
    this.capture[level].start := sidx;
    this.capture[level].len := what;
    INC(this.level);
    (* undo capture if match failed *)
    res := MatchAux(this, pat, src, pidx, sidx);
    IF res < 0 THEN DEC(this.level) END;
    RETURN res
END StartCapture;

PROCEDURE EndCapture(VAR this : Pattern; pat-, src- : ARRAY OF CHAR; pidx, sidx: LENGTH): LENGTH;
VAR
    res : LENGTH;
    l : INTEGER;
BEGIN
    l := CaptureToClose(this);
    IF l = -1 THEN RETURN -1 END;
    (* close capture *)
    this.capture[l].len := sidx - this.capture[l].start;
    (* undo capture if match failed *)
    res := MatchAux(this, pat, src, pidx, sidx);
    IF res < 0 THEN
        this.capture[l].len := CAP_UNFINISHED;
    END;
    RETURN res
END EndCapture;

PROCEDURE MatchCapture(VAR this : Pattern; src- : ARRAY OF CHAR; sidx : LENGTH; l : CHAR): LENGTH;
VAR
    idx : INTEGER;
    start, len, i : LENGTH;
BEGIN
    idx := CheckCapture(this, l);
    IF idx = -1 THEN RETURN -1 END;
    start := this.capture[idx].start;
    len := this.capture[idx].len;
    IF this.send - sidx + 1 >= len THEN
        FOR i := 0 TO len - 1 DO
            IF src[start + i] # src[sidx + i] THEN
                RETURN -1
            END;
        END;
        RETURN sidx + len - 1;
    END;
    RETURN -1;
END MatchCapture;

PROCEDURE MatchAux(VAR this : Pattern; pat-, src- : ARRAY OF CHAR; pidx, sidx : LENGTH): LENGTH;
CONST
    NONE = 0;
    DEF = 1;
    INIT = 2;
VAR
    nxt, ep, res : LENGTH;
    previous : CHAR;
BEGIN
    DEC(this.matchdepth);
    IF this.matchdepth = 0 THEN
        MatchError(this, ERROR_TO_COMPLEX);
        RETURN -1
    END;
    LOOP
        IF pidx > this.pend THEN EXIT END;
        (* INIT: *)
        nxt := NONE;
        CASE pat[pidx] OF
            '(' :
                (* start capture *)
                IF pat[pidx + 1] = ')' THEN
                    (* position capture? *)
                    sidx := StartCapture(this, pat, src, pidx + 2, sidx, CAP_POSITION)
                ELSE
                    sidx := StartCapture(this, pat, src, pidx + 1, sidx, CAP_UNFINISHED)
                END;
            | ')' :
                (* end capture *)
                sidx := EndCapture(this, pat, src, pidx + 1, sidx)
            | '$' :

                (* is the '$' the last char in pattern? *)
                IF pidx # this.pend THEN
                    (* no; go to default *)
                    nxt := DEF;
                ELSE
                    (* check end of string *)
                    IF sidx <= this.send THEN sidx := -1 END;
                END;
            | '%' :
                (* escaped sequences not in the format class[*+?-]? *)
                CASE pat[pidx + 1] OF
                      'b' :
                        (* balanced string? *)
                        sidx := MatchBalance(this, pat, src, pidx + 2, sidx);
                        IF sidx >= 0 THEN
                            INC(pidx, 4);
                            nxt := INIT;
                        END;
                    | 'f' :
                        (* frontier? *)
                        INC(pidx, 2);
                        IF pat[pidx] = '[' THEN
                            ep := ClassEnd(this, pat, pidx);
                            IF this.error = 0 THEN
                                IF sidx = this.sinit THEN
                                    previous := 00X
                                ELSE
                                    previous := src[sidx - 1]
                                END;
                                IF ~MatchBracketClass(previous, pat, pidx, ep - 1) &
                                   MatchBracketClass(src[sidx], pat, pidx, ep - 1) THEN
                                    pidx := ep;
                                    nxt := INIT;
                                ELSE
                                    (* match failed *)
                                    sidx := -1;
                                END; 
                            END;
                        ELSE
                            MatchError(this, ERROR_MALFORMED_PATTERN);
                        END;
                    | '0' .. '9' :
                        (* capture results (%0-%9)? *)
                        sidx := MatchCapture(this, src, sidx, pat[pidx + 1]);
                        IF sidx >= 0 THEN
                            INC(pidx, 2);
                            nxt := INIT;
                        END;
                ELSE
                    nxt := DEF;
                END;
        ELSE
            nxt := DEF;
        END;
        (* pattern class plus optional suffix *)
        IF nxt = DEF THEN
            (* points to optional suffix *)
            ep := ClassEnd(this, pat, pidx);
            IF this.error # 0 THEN EXIT END;
            
            IF ~SingleMatch(this, pat, src, pidx, sidx, ep) THEN (* does not match at least once? *)
                DEC(this.repetitioncounter);
                IF this.repetitioncounter = 0 THEN
                    MatchError(this, ERROR_MAX_REPITITIONS);
                    sidx := -1; (* fail *)
                ELSIF (pat[ep] = '*') OR (pat[ep] = '?') OR (pat[ep] = '-') THEN (* accept empty? *)
                    pidx := ep + 1;
                    nxt := INIT;
                ELSE (* '+' or no suffix *)
                    sidx := -1; (* fail *)
                END;
            ELSE
                (* matched once *)
                (* handle optional suffix *)
                CASE pat[ep] OF
                      '?' :
                        res := MatchAux(this, pat, src, ep + 1, sidx + 1);
                        IF res >= 0 THEN
                            sidx := res;
                        ELSE
                            pidx := ep + 1;
                            nxt := INIT;
                        END;
                    | '+' :
                        (* 1 or more repetitions *)
                        INC(sidx);
                        sidx := MaxExpand(this, pat, src, pidx, sidx, ep);
                    | '*' :
                        (* 0 or more repetitions *)
                        sidx := MaxExpand(this, pat, src, pidx, sidx, ep);
                    | '-' :
                        (* 0 or more repetitions (minimum) *)
                        sidx := MinExpand(this, pat, src, pidx, sidx, ep);
                ELSE
                    (* no suffix *)
                    INC(sidx);
                    pidx := ep;
                    nxt := INIT;
                END;
            END;
        END;
        IF nxt # INIT THEN EXIT END;
    END;
	INC(this.matchdepth);
    RETURN sidx
END MatchAux;

PROCEDURE (VAR this : Pattern) FindAux(pat- : ARRAY OF CHAR; src- : ARRAY OF CHAR; VAR sidx : LENGTH): LENGTH;
VAR
    anchor : BOOLEAN;
	res, pidx, i : LENGTH;
BEGIN
	IF NoSpecials(this, pat) THEN
		(* do a plain search *)
		sidx := ArrayOfChar.Index(pat, src, sidx);
		IF sidx >= 0 THEN
            this.slen := this.pend + 1;
		END;
		RETURN sidx
	END;
	pidx := 0;
	anchor := pat[pidx] = '^';
	IF anchor THEN INC(pidx) END;
	LOOP
        res := MatchAux(this, pat, src, pidx, sidx);
        IF this.error # 0 THEN
            RETURN this.error
        ELSIF res >= 0 THEN
            this.sinit := sidx;
            this.slen := res - this.sinit;
            FOR i := 1 TO this.level DO
                IF this.capture[i - 1].len = CAP_UNFINISHED THEN
                    this.error := ERROR_INVALID_PATTERN_CAPTURE;
                    RETURN this.error
                END;
            END;
			RETURN res
        END;
        IF (sidx > this.send) OR anchor THEN EXIT END;
        INC(sidx);
    END;
	RETURN -1
END FindAux;

(**
Find first occurence of `pattern` in `str`.
Return to TRUE if pattern is found.
*)
PROCEDURE (VAR this : Pattern) Match* (pat-, str-: ARRAY OF CHAR): BOOLEAN;
VAR
	ret, sidx : LENGTH;
BEGIN
	this.matchdepth := MAXCCALLS;
	this.repetitioncounter := MAXREPETITION;
	this.maxcaptures := MAXCAPTURES;
	this.sinit := 0;
    this.slen := 0;
	this.send := ArrayOfChar.Length(str) - 1;
	this.pend := ArrayOfChar.Length(pat) - 1;
	this.error := 0;
	this.level := 0;
    IF (this.pend = - 1) OR (this.send = -1) THEN RETURN TRUE END;
    sidx := 0;
	ret := this.FindAux(pat, str, sidx);
	RETURN ret >= 0;
END Match;

(**
Find occurence of `pattern` in `str` start at index start.
Return start of match position or -1 if no match.
*)
PROCEDURE (VAR this : Pattern) Find* (pat-, str-: ARRAY OF CHAR; start : LENGTH): LENGTH;
VAR
    ret : LENGTH;
BEGIN
	this.matchdepth := MAXCCALLS;
	this.repetitioncounter := MAXREPETITION;
	this.maxcaptures := MAXCAPTURES;
	this.send := ArrayOfChar.Length(str) - 1; 
	this.pend := ArrayOfChar.Length(pat) - 1;
    IF (this.pend = - 1) OR (this.send = -1) THEN RETURN 0 END;
	this.error := 0;
	this.level := 0;
	IF (start < 0) OR (start > this.send) THEN RETURN -1 END;
	this.sinit := start;
    this.slen := 0;
    ret := this.FindAux(pat, str, start);
    RETURN ret;
END Find;

(**
Get start position and length into src string of capture at index.
For match pattern index 0 it return position and length of whole string match.
Return TRUE if a valid capture exists and no error flag is set.
*)
PROCEDURE (VAR this : Pattern) Capture* (index : INTEGER; VAR start, len : LENGTH): BOOLEAN;
VAR
    ret : BOOLEAN;
BEGIN
    ret := FALSE;
    IF (this.error = 0) THEN
        IF index = 0 THEN
            start := this.sinit;
            len := this.slen;
            ret := TRUE;
        ELSIF (index <= this.level) & (this.capture[index - 1].len # CAP_UNFINISHED) THEN
            start := this.capture[index - 1].start;
            len := this.capture[index - 1].len;
            ret := TRUE;
        END
    END;
	RETURN ret;
END Capture;

END StringPattern.