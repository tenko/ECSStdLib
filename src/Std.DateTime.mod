(**
Date and Time module.

Adapted from the implementation of `Julia` language standard library.
License is MIT: https://julialang.org/license

A Date value is encoded as an `HUGEINT`, counting milliseconds
from an epoch. The epoch is 0000-12-31T00:00:00.

Days are adjusted accoring to `Rata Die` approch to simplify date calculations.

* Wikipedia: `Link <https://en.wikipedia.org/wiki/Rata_Die>`_
* Report: `Link <http://web.archive.org/web/20140910060704/http://mysite.verizon.net/aesir_research/date/date0.htm>`_
**)
MODULE DateTime IN Std;

IN Std IMPORT Const, Type, Char, Integer, OSHost, String := ArrayOfChar;

TYPE
    DATETIME* = Type.DATETIME;

CONST
	ERROR*      = MIN(HUGEINT);
    Year*       = 1;
    Quarter*    = 2;
    Month*      = 3;
    Week*       = 4;
    Day*        = 5;
    Weekday*    = 6;
    Hour*       = 7;
    Min*        = 8;
    Sec*        = 9;
    MSec*       = 10;

VAR
    MonthDays: ARRAY 2, 12 OF INTEGER;
    ShiftedMonthDays: ARRAY 12 OF INTEGER;
    UTCOffset- : INTEGER;

(* Return TRUE if the Year is a leap year *)
PROCEDURE IsLeapYear (year : INTEGER) : BOOLEAN;
BEGIN RETURN (year MOD 4 = 0) & ((year MOD 100 # 0) OR (year MOD 400 = 0))
END IsLeapYear;

(*
 Convert Year, Month, Day to # of Rata Die days
 Works by shifting the beginning of the year to March 1,
 so a leap day is the very last day of the year.
*)
PROCEDURE RataDie (year, month, day : INTEGER): HUGEINT;
VAR mdays : INTEGER;
BEGIN
    (* If we're in Jan/Feb, shift the given year back one *)
    IF month < 3 THEN DEC(year) END;
    mdays := ShiftedMonthDays[month - 1];
    RETURN day + mdays + 365*year + year DIV 4 - year DIV 100 + year DIV 400 - 306;
END RataDie;

(* Convert # of Rata Die Days to proleptic Gregorian calendar Year, Month & Day. *)
PROCEDURE InverseRataDie (days : HUGEINT; VAR year, month, day : INTEGER);
VAR a, b, c, h, m, y, z : HUGEINT;
BEGIN
    z := days + 306; h := 100*z - 25; a := h DIV 3652425; b := a - a DIV 4;
    y := (100*b + h) DIV 36525;
    c := b + z - 365*y - y DIV 4;
    m := (5*c + 456) DIV 153;
    day := INTEGER(c - (153*m - 457) DIV 5);
    month := INTEGER(m);
    year := INTEGER(y);
    IF month > 12 THEN
        year := year + 1;
        month := month - 12;
    END;
END InverseRataDie;

(** Return `TRUE` if the year, month & day is successful converted to a valid `DATETIME` *)
PROCEDURE TryEncodeDate* (VAR date : DATETIME; year,month,day : INTEGER) : BOOLEAN;
VAR ret : BOOLEAN;
BEGIN
    ret := (year > 0) & (year < 10000) & (month > 0) & (month < 13) & (day > 0) 
           & (day <= MonthDays[INTEGER(IsLeapYear(year)), month - 1]);
    IF ret THEN
        date := 86400000 * RataDie(year, month, day);
    END;
    RETURN ret;
END TryEncodeDate;

(** Return Encoded date. Return `ERROR` if not valid *)
PROCEDURE EncodeDate* (year, month, day : INTEGER): DATETIME;
VAR date : DATETIME;
BEGIN
    IF ~TryEncodeDate(date, year, month, day) THEN date := ERROR END;
    RETURN date;
END EncodeDate;

(** Return `TRUE` if Year, Month, Day, Hour, Min, Sec & MSec is successful converted to a valid `DATETIME` *)
PROCEDURE TryEncodeDateTime* (VAR datetime : DATETIME; year, month, day, hour, min, sec, msec : INTEGER) : BOOLEAN;
VAR ret : BOOLEAN;
BEGIN
    ret := TryEncodeDate(datetime, year, month, day);
    IF ret THEN
        ret := (hour >= 0) & (hour < 24) & (min >= 0) & (min < 60) & (sec >= 0) & (sec < 60)
               & (msec >= 0) & (msec < 1000);
        IF ret THEN
            INC (datetime, msec + 1000*(sec + 60*min + 3600*hour));
            (* datetime := datetime + msec + 1000*(sec + 60*min + 3600*hour); Waiting for fix*)
        END;
    END;
    RETURN ret;
END TryEncodeDateTime;

(** Return encoded `DATETIME`. Return `ERROR` if not valid *)
PROCEDURE EncodeDateTime* (year, month, day, hour, min, sec, msec: INTEGER): DATETIME;
VAR datetime : DATETIME;
BEGIN
    IF ~TryEncodeDateTime(datetime, year, month, day, hour, min, sec, msec) THEN
        datetime := ERROR;
    END;
    RETURN datetime;
END EncodeDateTime;

(** Decode `DATETIME` to Year, Month & Day *)
PROCEDURE DecodeDate* (datetime: DATETIME; VAR year, month, day: INTEGER);
BEGIN InverseRataDie(datetime DIV 86400000, year, month, day);
END DecodeDate;

(** Decode `DATETIME` to  Hour, Min, Sec & MSec *)
PROCEDURE DecodeTime* (datetime: DATETIME; VAR hour, min, sec, msec: INTEGER);
BEGIN
    hour := INTEGER((datetime DIV 3600000) MOD 24);
    min := INTEGER((datetime DIV 60000) MOD 60);
    sec := INTEGER((datetime DIV 1000) MOD 60);
    msec := INTEGER(datetime MOD 1000);
END DecodeTime;

(** Decode `DATETIME` to Year, Month, Day, Hour, Min, Sec & MSec *)
PROCEDURE DecodeDateTime* (datetime: DATETIME; VAR year, month, day, hour, min, sec, msec: INTEGER);
BEGIN
    InverseRataDie(datetime DIV 86400000, year, month, day);
    DecodeTime(datetime, hour, min, sec, msec);
END DecodeDateTime;

(** Remove time part of `DATETIME` *)
PROCEDURE DateTimeToDate* (datetime: DATETIME) : DATETIME;
VAR year, month, day : INTEGER;
BEGIN
    DecodeDate(datetime, year, month, day);
    RETURN EncodeDate(year, month, day);
END DateTimeToDate;

(** Increment Year of `DATETIME` and return modified value *)
PROCEDURE IncYear* (datetime: DATETIME; years : INTEGER) : DATETIME;
VAR year, month, day, hour, min, sec, msec: INTEGER;
BEGIN
    DecodeDateTime(datetime, year, month, day, hour, min, sec, msec);
    INC(year, years);
    IF day > MonthDays[INTEGER(IsLeapYear(year)), month - 1] THEN
        day := MonthDays[INTEGER(IsLeapYear(year)), month - 1];
    END;
    RETURN EncodeDateTime(year, month, day, hour, min, sec, msec);
END IncYear;

(** Decrement Year of `DATETIME` and return modified value *)
PROCEDURE DecYear* (datetime: DATETIME; years : INTEGER) : DATETIME;
VAR year, month, day, hour, min, sec, msec: INTEGER;
BEGIN
    DecodeDateTime(datetime, year, month, day, hour, min, sec, msec);
    DEC(year, years);
    IF day > MonthDays[INTEGER(IsLeapYear(year)), month - 1] THEN
        day := MonthDays[INTEGER(IsLeapYear(year)), month - 1];
    END;
    RETURN EncodeDateTime(year, month, day, hour, min, sec, msec);
END DecYear;

(** Increment Month of `DATETIME` and return modified value *)
PROCEDURE IncMonth* (datetime: DATETIME; months : INTEGER) : DATETIME;
    VAR tmp, year, month, day, hour, min, sec, msec: INTEGER;
BEGIN
    DecodeDateTime(datetime, year, month, day, hour, min, sec, msec);
    INC(year, months DIV 12);
    tmp := month + (months MOD 12) - 1;
    IF tmp > 11 THEN DEC(tmp, 12); INC(year) END;
    month := tmp + 1; (* months from 1 to 12 *)
    tmp := MonthDays[INTEGER(IsLeapYear(year)), month - 1];
    IF day > tmp THEN day := tmp END;
    RETURN EncodeDateTime(year, month, day, hour, min, sec, msec);
END IncMonth;

(* Decrement Month of `DATETIME` and return modified value *)
PROCEDURE DecMonth*(datetime: DATETIME; months : INTEGER) : DATETIME;
    VAR year, month, day, hour, min, sec, msec, tmp: INTEGER;
BEGIN
    DecodeDateTime(datetime, year, month, day, hour, min, sec, msec);
    DEC(year, months DIV 12);
    tmp := month - (months MOD 12) - 1;
    IF tmp < 0 THEN INC(tmp, 12); DEC(year) END;
    month := ABS(tmp) + 1; (* months from 1 to 12 *)
    tmp := MonthDays[INTEGER(IsLeapYear(year)), month - 1];
    IF day > tmp THEN day := tmp END;
    RETURN EncodeDateTime(year, month, day, hour, min, sec, msec);
END DecMonth;

(** Increment `DATETIME` with Value according to Type.*)
PROCEDURE Inc* (VAR datetime : DATETIME; typ : INTEGER; value : HUGEINT);
BEGIN
    CASE typ OF
        Year : 
            datetime := IncYear(datetime, INTEGER(value));
      | Quarter :
            datetime := IncMonth(datetime, INTEGER(4*value));
      | Month :
            datetime := IncMonth(datetime, INTEGER(value));
      | Week :
            datetime := datetime +  7 * value * 86400000;
      | Day :
            datetime := datetime +  value * 86400000;
      | Hour :
            datetime := datetime +  value * 3600000;
      | Min :
            datetime := datetime +  value * 60000;
      | Sec :
            datetime := datetime +  value * 1000;
      | MSec :
           datetime := datetime +  value;
    END;
END Inc;

(** Decrement `DATETIME` with Value according to Type.*)
PROCEDURE Dec* (VAR datetime : DATETIME; typ : INTEGER; value : HUGEINT);
BEGIN
    CASE typ OF
        Year : 
            datetime := DecYear(datetime, INTEGER(value));
      | Quarter :
            datetime := DecMonth(datetime, INTEGER(4*value));
      | Month :
            datetime := DecMonth(datetime, INTEGER(value));
      | Week :
            datetime := datetime - 7 * value * 86400000;
      | Day :
            datetime := datetime - value * 86400000;
      | Hour :
            datetime := datetime - value * 3600000;
      | Min :
            datetime := datetime - value * 60000;
      | Sec :
            datetime := datetime - value * 1000;
      | MSec :
           datetime := datetime - value;
    END;
END Dec;

(** Current `DATETIME` *)
PROCEDURE Now* (): DATETIME;
VAR
    dt : OSHost.DateTime;
    ret : DATETIME;
BEGIN
    OSHost.GetTime(dt);
    IF OSHost.DATETIMEOFFSET THEN
        ret := EncodeDate(dt.year, dt.month, dt.day);
        Inc(ret, Hour, dt.hour);
        Inc(ret, Min, dt.min);
        Inc(ret, Sec, dt.sec);
        Inc(ret, MSec, dt.msec);
    ELSE
        ret := EncodeDateTime(
            dt.year, dt.month, dt.day,
            dt.hour, dt.min, dt.sec, dt.msec
        );
    END;
    RETURN ret
END Now;

(** Current Date *)
PROCEDURE Today* (): DATETIME;
BEGIN RETURN DateTimeToDate(Now())
END Today;

(* Extract Week of `DATETIME` 
   # https://en.wikipedia.org/wiki/Talk:ISO_week_date#Algorithms
*)
PROCEDURE WeekOf(datetime : DATETIME): INTEGER;
    VAR days, w, c, f, s : INTEGER;
BEGIN
    days := INTEGER(datetime DIV 86400000);
    w := (ABS(days - 1) DIV 7) MOD 20871;
    s := INTEGER(w >= 10435);
    c := (w + s) DIV 5218;
    w := (w + s) MOD 5218;
    CASE c OF
        0 : f := 15;
      | 1 : f := 23;
      | 2 : f := 3;
    ELSE f := 11 END;
    w := (w * 28 + f) MOD 1461;
    RETURN w DIV 28 + 1
END WeekOf;

(** Extract component of `DATETIME` *)
PROCEDURE Extract* (datetime : DATETIME; typ : INTEGER) : INTEGER;
VAR year, month, day, ret : INTEGER;
BEGIN
    CASE typ OF
        Year : 
            DecodeDate(datetime, year, month, day);
            ret := year;
      | Quarter :
            DecodeDate(datetime, year, month, day);
            ret := 4;
            IF month < 4 THEN ret := 1;
            ELSIF month < 7 THEN ret := 2;
            ELSIF month < 10 THEN ret := 3 END
      | Month :
            DecodeDate(datetime, year, month, day);
            ret := month;
      | Week :
            ret := WeekOf(datetime);
      | Day :
            DecodeDate(datetime, year, month, day);
            ret := day;
      | Weekday :
            ret := INTEGER((datetime DIV 86400000) MOD 7);
      | Hour :
            ret := INTEGER((datetime DIV 3600000) MOD 24);
      | Min :
            ret := INTEGER((datetime DIV 60000) MOD 60);
      | Sec :
            ret := INTEGER((datetime DIV 1000) MOD 60);
      | MSec :
            ret := INTEGER(datetime MOD 1000);
    END;
    RETURN ret;  
END Extract;

(**
  Trucate `DATETIME` value according to Type.
  Usefull for comparison, calculate spans or finding start of periods (week, month).
*)
PROCEDURE Trunc* (datetime: DATETIME; typ: INTEGER): DATETIME;
VAR
    year, month, day: INTEGER;
    ret: DATETIME;
BEGIN
    CASE typ OF
        Year : 
            DecodeDate(datetime, year, month, day);
            ret := EncodeDate(year, 1, 1);
      | Quarter :
            DecodeDate(datetime, year, month, day);
            IF month < 4 THEN month := 1
            ELSIF month < 7 THEN month := 4
            ELSIF month < 10 THEN month := 7
            ELSE month := 10 END;
            ret := EncodeDate(year, month, 1);
      | Month :
            DecodeDate(datetime, year, month, day);
            ret := EncodeDate(year, month, 1);
      | Week :
            ret := datetime;
            day := 1 - Extract(datetime, Weekday);
            IF day < 0 THEN
                Dec(ret, Day, ABS(day));
            ELSE
                Inc(ret, Day, day);
            END;
      | Day :
            DecodeDate(datetime, year, month, day);
            ret := EncodeDate(year, month, day);
      | Hour :
            ret := (datetime DIV 3600000) * 3600000;
      | Min :
            ret := (datetime DIV 60000) * 60000;
      | Sec :
            ret := (datetime DIV 1000) * 1000;
    END;
    RETURN ret;
END Trunc;

(** Compute difference between two dates. Extract year, month, day to get difference *)
PROCEDURE Diff* (dtstart, dtend: DATETIME; addEndDay: BOOLEAN): DATETIME;
VAR ret : DATETIME;
BEGIN
    IF dtend < dtstart THEN ret := dtstart - dtend
    ELSE ret := dtend - dtstart END;
    Dec(ret, Year, 1);
    Dec(ret, Month, 1);
    Inc(ret, Day, 1 + INTEGER(addEndDay));
    RETURN ret;
END Diff;

(** Calculate `DATETIME` span between Start and End according to Type *)
PROCEDURE Span* (dtstart, dtend: DATETIME; typ: INTEGER): INTEGER;
VAR
    tmp : DATETIME;
    ret : INTEGER;
BEGIN
    tmp := Diff(dtstart, dtend, FALSE);
    CASE typ OF
        Year : 
            ret := Extract(tmp, Year);
      | Quarter :
            ret := 4*Extract(tmp, Year) + Extract(dtend - dtstart, Quarter) - 1; (* Quarter starts with 1 *)
      | Month :
            ret := 12*Extract(tmp, Year) + Extract(dtend - dtstart, Month) - 1; (* Month starts with 1 *)
      | Day :
            ret := INTEGER(dtend DIV 86400000 - dtstart DIV 86400000);
      | Hour :
            ret := INTEGER(dtend DIV 3600000 - dtstart DIV 3600000);
      | Min :
            ret := INTEGER(dtend DIV 60000 - dtstart DIV 60000);
      | Sec :
            ret := INTEGER(dtend DIV 1000 - dtstart DIV 1000);
      | MSec :
            ret := INTEGER(dtend - dtstart);
    END;
    IF ret < 0 THEN ret := 0 END;
    RETURN ret;
END Span;

(* Full month names *)
PROCEDURE MonthName (month : INTEGER; VAR ret : ARRAY OF CHAR);
BEGIN
    CASE month OF
          1  : String.Assign(ret, "January");
        | 2  : String.Assign(ret, "February");
        | 3  : String.Assign(ret, "March");
        | 4  : String.Assign(ret, "April");
        | 5  : String.Assign(ret, "May");
        | 6  : String.Assign(ret, "June");
        | 7  : String.Assign(ret, "July");
        | 8  : String.Assign(ret, "August");
        | 9  : String.Assign(ret, "September");
        | 10 : String.Assign(ret, "October");
        | 11 : String.Assign(ret, "November");
        | 12 : String.Assign(ret, "December");
    END
END MonthName;

(* Abbreviated month names *)
PROCEDURE MonthAbbr (month : INTEGER; VAR ret : ARRAY OF CHAR);
BEGIN
    CASE month OF
          1  : String.Assign(ret, "Jan");
        | 2  : String.Assign(ret, "Feb");
        | 3  : String.Assign(ret, "Mar");
        | 4  : String.Assign(ret, "Apr");
        | 5  : String.Assign(ret, "May");
        | 6  : String.Assign(ret, "Jun");
        | 7  : String.Assign(ret, "Jul");
        | 8  : String.Assign(ret, "Aug");
        | 9  : String.Assign(ret, "Sep");
        | 10 : String.Assign(ret, "Oct");
        | 11 : String.Assign(ret, "Nov");
        | 12 : String.Assign(ret, "Dec");
    END
END MonthAbbr;

(* Full day names *)
PROCEDURE DayName (weekday : INTEGER; VAR ret : ARRAY OF CHAR);
BEGIN
    CASE weekday OF
          1 : String.Assign(ret, "Monday");
        | 2 : String.Assign(ret, "Tuesday");
        | 3 : String.Assign(ret, "Wednesday");
        | 4 : String.Assign(ret, "Thursday");
        | 5 : String.Assign(ret, "Friday");
        | 6 : String.Assign(ret, "Saturday");
        | 7 : String.Assign(ret, "Sunday");
    END
END DayName;

(* Abbreviated day names *)
PROCEDURE DayAbbr (weekday : INTEGER; VAR ret : ARRAY OF CHAR);
BEGIN
    CASE weekday OF
          1 : String.Assign(ret, "Mon");
        | 2 : String.Assign(ret, "Tue");
        | 3 : String.Assign(ret, "Wed");
        | 4 : String.Assign(ret, "Thu");
        | 5 : String.Assign(ret, "Fri");
        | 6 : String.Assign(ret, "Sat");
        | 7 : String.Assign(ret, "Sun");
    END
END DayAbbr;

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
PROCEDURE Format* (VAR Writer : Type.Stream; datetime : DATETIME; fmt- : ARRAY OF CHAR);
VAR
    year, month, day: INTEGER;
    hour, min, sec, msec: INTEGER;
    i : LENGTH;
    s : ARRAY 20 OF CHAR;
    ch : CHAR;
    PROCEDURE Next();
    BEGIN
        IF i < LEN(fmt) THEN ch := fmt[i]; INC(i)
        ELSE ch := 00X END;
    END Next;
    PROCEDURE Peek() : CHAR;
    BEGIN
        IF i < LEN(fmt) THEN RETURN fmt[i]
        ELSE RETURN 00X END;
    END Peek;
BEGIN
    i := 0;
    DecodeDate(datetime, year, month, day);
    DecodeTime(datetime, hour, min, sec, msec);
    LOOP
        Next();
        IF ch = '%' THEN
            CASE Peek() OF
                  'a' : (* Weekday abbreviated name : Mon .. Sun *)
                        DayAbbr(Extract(datetime, Weekday), s);
                        Writer.WriteString(s);
                        Next();
                | 'A' : (* Weekday full name : Monday .. Sunday *)
                        DayName(Extract(datetime, Weekday), s);
                        Writer.WriteString(s);
                        Next();
                | 'w' : (* Weekday as number : 0 .. 6 *)
                        Integer.Format(Writer, Extract(datetime, Weekday), 0, {});
                        Next();
                | 'b' : (* Month abbreviated name : Jan .. Des *)
                        MonthAbbr(month, s);
                        Writer.WriteString(s);
                        Next();
                | 'B' : (* Month full name : Januar .. Desember *)
                        MonthName(month, s);
                        Writer.WriteString(s);
                        Next();
                | 'Y' : (* Year without century : 00 - 99 *)
                        Integer.Format(Writer, year MOD 100, 2, Const.Zero);
                        Next();
                | 'y' : (* Year with century : 0000 - 9999 *)
                        Integer.Format(Writer, year, 4, Const.Zero);
                        Next();
                | 'm' : (* Month zero-padded : 00 - 12 *)
                        Integer.Format(Writer, month, 2, Const.Zero);
                        Next();
                | 'd' : (* Day of the month zero-padded : 00 - XX *)
                        Integer.Format(Writer, day, 2, Const.Zero);
                        Next();
                | 'W' : (* Week of the year zero-padded : 01 - 53 *)
                        Integer.Format(Writer, Extract(datetime, Week), 2, Const.Zero);
                        Next();
                | 'H' : (* Hour (24-hour clock) zero-padded : 00 - 23 *)
                        Integer.Format(Writer, hour, 2, Const.Zero);
                        Next();
                | 'I' : (* Hour (12-hour clock) zero-padded : 1 - 12 *)
                        IF (hour = 0) OR (hour = 12) THEN
                            Writer.WriteString("12");
                        ELSE Integer.Format(Writer, hour MOD 12, 2, Const.Zero);
                        END;
                        Next();
                | 'p' : (* AM or PM *)
                        IF hour >= 12 THEN Writer.WriteString('PM')
                        ELSE Writer.WriteString('AM')
                        END;
                        Next();
                | 'M' : (* Minute zero-padded : 00 - 59 *)
                        Integer.Format(Writer, min, 2, Const.Zero);
                        Next();
                | 'S' : (* Second zero-padded : 00 - 59 *)
                        Integer.Format(Writer, sec, 2, Const.Zero);
                        Next();
                | 'f' : (* Milliseconds zero-padded : 000 - 999 *)
                        Integer.Format(Writer, msec, 3, Const.Zero);
                        Next();
                | 'Z' : (* Timezone : UTC *)
                        Writer.WriteString('UTC');
                        Integer.Format(Writer, -UTCOffset, 0, Const.Sign);
                        Next();     
                | '%' :   (* Literal '%' char *)
                        Writer.WriteChar('%');
                        Next();
                ELSE
                    Writer.WriteChar('%'); Writer.WriteChar(ch);
                END;
        ELSIF ch = 00X THEN EXIT
        ELSE Writer.WriteChar(ch)
        END
    END
END Format;

(**
Parse string to a `DATETIME` according to format string:

* `%y` : Year with century : 0 - 9999
* `%m` : Month : 1 - 12
* `%d` : Day of the month : 1 - XX
* `%H` : Hour (24-hour clock) : 0 - 23
* `%M` : Minute : 0 - 59
* `%S` : Second  : 0 - 59
* `%f` : Milliseconds : 0 - 999
* `%t` : One or more TAB or SPC characters
* `%%` : Literal `%` char

Numbers can be zero padded.
Other characters must match exactly.

Sets datetime to ERROR on failure.
*)
PROCEDURE FromSubString* (VAR datetime : DATETIME; src- : ARRAY OF CHAR; fmt-: ARRAY OF CHAR; start: LENGTH);
VAR
    year, month, day: INTEGER;
    hour, min, sec, msec: INTEGER;
    i, idx : LENGTH;
    ch : CHAR;

    PROCEDURE IsSpace(ch: CHAR) : BOOLEAN;
    BEGIN RETURN (ch = Char.TAB) OR (ch = Char.SPC)
    END IsSpace;

    PROCEDURE Next();
    BEGIN
        IF idx < LEN(fmt) THEN ch := fmt[idx]; INC(idx)
        ELSE ch := Char.NUL END;
    END Next;

    PROCEDURE Peek() : CHAR;
    BEGIN
        IF idx < LEN(fmt) THEN RETURN fmt[idx]
        ELSE RETURN Char.NUL END;
    END Peek;

    PROCEDURE Parse(VAR val : INTEGER; width : INTEGER) : BOOLEAN;
    VAR v : HUGEINT;
    BEGIN
        IF val # -1 THEN RETURN FALSE END;
        IF width = -1 THEN
            width := 0;
            WHILE Char.IsDigit(src[i + width]) DO INC(width) END
        END;
        IF ~Integer.FromSubString(v, src, i, width) THEN RETURN FALSE END;
        val := INTEGER(v); INC(i, width);
        RETURN TRUE;
    END Parse;
BEGIN
    i := start; idx := 0;
    datetime := ERROR;
    year := -1; month := -1; day := -1;
    hour := -1; min := -1; sec := -1; msec := -1;
    LOOP
        Next();
        IF ch = '%' THEN
            CASE Peek() OF
                'y' :   (* Year with century : 0 - 9999 *)
                        IF ~Parse(year, -1) THEN RETURN END;
                        Next(); |
                'm' :   (* Month : 1 - 12 *)
                        IF ~Parse(month, -1) THEN RETURN END;
                        Next(); |
                'd' :   (* Day of the month : 1 - XX *)
                        IF ~Parse(day, -1) THEN RETURN END;
                        Next(); |
                'H' :   (* Hour (24-hour clock) zero-padded : 00 - 23 *)
                        IF ~Parse(hour, 2) THEN RETURN END;
                        Next(); |
                'M' :   (* Minute : 0 - 59 *)
                        IF ~Parse(min, -1) THEN RETURN END;
                        Next(); |
                'S' :   (* Second : 0 - 59 *)
                        IF ~Parse(sec, 2) THEN RETURN END;
                        Next(); |
                'f' :   (* Milliseconds : 0 - 999 *)
                        IF ~Parse(msec, -1) THEN RETURN END;
                        Next(); |
                '%' :   (* Escaped '%' *)
                        IF ch # '%' THEN RETURN END;
                        Next(); |
                't' :   (* White space *)
                        IF ~IsSpace(src[i]) THEN RETURN END;
                        WHILE IsSpace(src[i]) DO INC(i) END;
                        Next(); |
            ELSE RETURN
            END;
        ELSIF ch = Char.NUL THEN EXIT
        ELSE
            IF ch # src[i] THEN RETURN END;
            INC(i)
        END
    END;
    IF Peek() # Char.NUL THEN RETURN END;
    IF (year # -1) & (month # -1) & (day # -1) THEN
        IF hour = -1 THEN hour := 0 END;
        IF min = -1 THEN min := 0 END;
        IF sec = -1 THEN sec := 0 END;
        IF msec = -1 THEN msec := 0 END;
        datetime := EncodeDateTime(year, month, day, hour, min, sec, msec);
    END;
END FromSubString;

(**
Parse string to a `DATETIME` according to format string:

* `%y` : Year with century : 0 - 9999
* `%m` : Month : 1 - 12
* `%d` : Day of the month : 1 - XX
* `%H` : Hour (24-hour clock) : 0 - 23
* `%M` : Minute : 0 - 59
* `%S` : Second  : 0 - 59
* `%f` : Milliseconds : 0 - 999
* `%t` : One or more TAB or SPC characters
* `%%` : Literal `%` char

Numbers can be zero padded.
Other characters must match exactly.

Sets datetime to ERROR on failure.
*)
PROCEDURE FromString* (VAR datetime : DATETIME; src- : ARRAY OF CHAR; fmt-: ARRAY OF CHAR);
BEGIN FromSubString(datetime, src, fmt, 0)
END FromString;

BEGIN
    UTCOffset := OSHost.GetTimeZoneOffset();
    MonthDays[0,0] := 31;  MonthDays[0,1] := 28; MonthDays[0,2] := 31; MonthDays[0,3] := 30;
	MonthDays[0,4] := 31;  MonthDays[0,5] := 30; MonthDays[0,6] := 31; MonthDays[0,7] := 31;
	MonthDays[0,8] := 30;  MonthDays[0,9] := 31; MonthDays[0,10] := 30; MonthDays[0,11] := 31;
	MonthDays[1,0] := 31;  MonthDays[1,1] := 29; MonthDays[1,2] := 31; MonthDays[1,3] := 30;
	MonthDays[1,4] := 31;  MonthDays[1,5] := 30; MonthDays[1,6] := 31; MonthDays[1,7] := 31;
	MonthDays[1,8] := 30;  MonthDays[1,9] := 31; MonthDays[1,10] := 30; MonthDays[1,11] := 31;
	ShiftedMonthDays[0] := 306; ShiftedMonthDays[1] := 337; ShiftedMonthDays[2] := 0;
	ShiftedMonthDays[3] := 31;  ShiftedMonthDays[4] := 61; ShiftedMonthDays[5] := 92;
	ShiftedMonthDays[6] := 122; ShiftedMonthDays[7] := 153; ShiftedMonthDays[8] := 184;
	ShiftedMonthDays[9] := 214; ShiftedMonthDays[10] := 245; ShiftedMonthDays[11] := 275;
END DateTime.
