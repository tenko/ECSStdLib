MODULE TestDateTime;
IN Std IMPORT Testing := O2Testing;
IN Std IMPORT Const, Char, S := ArrayOfChar, DateTime;

TYPE
    TEST = Testing.TEST;
    DATETIME = DateTime.DATETIME;

CONST
    M = "TestDateTime";

PROCEDURE Run* (VAR test: TEST);
VAR
    d, exp, end, start : DATETIME;
    year, month, day : INTEGER;
    str : ARRAY 64 OF CHAR;

    PROCEDURE Assert(b: BOOLEAN; id: LONGINT) ;
    BEGIN
        Testing.Assert(test, b, M, id);
    END Assert ;

BEGIN
    Testing.Begin(test, M);

    (* TryEncodeDate *)
    Assert(DateTime.TryEncodeDate(d,2020,12,32) = FALSE, __LINE__) ;
    Assert(DateTime.TryEncodeDate(d,2020,13,31) = FALSE, __LINE__) ;
    Assert(DateTime.TryEncodeDate(d,2020,12,31) = TRUE, __LINE__) ;

    (* EncodeDate *)
    Assert(DateTime.EncodeDate(2020,12,32) = DateTime.ERROR, __LINE__) ;
    Assert(DateTime.EncodeDate(2020,13,31) = DateTime.ERROR, __LINE__) ;
    Assert(DateTime.EncodeDate(2020,12,31) # DateTime.ERROR, __LINE__) ;

    (* DecodeDate *)
    Assert(DateTime.EncodeDate(2011,11,18) # DateTime.ERROR, __LINE__) ;
    d := DateTime.EncodeDate(2011,11,18);
    DateTime.DecodeDate(d, year, month, day);
    Assert(year = 2011, __LINE__) ;
    Assert(month = 11, __LINE__) ;
    Assert(day = 18, __LINE__) ;

    (* Extract *)
    Assert(DateTime.Extract(DateTime.EncodeDate(1989, 6, 22), DateTime.Year) = 1989, __LINE__) ;
    Assert(DateTime.Extract(DateTime.EncodeDate(1989, 6, 22), DateTime.Quarter) = 2, __LINE__) ;
    Assert(DateTime.Extract(DateTime.EncodeDate(1989, 6, 22), DateTime.Month) = 6, __LINE__) ;
    Assert(DateTime.Extract(DateTime.EncodeDate(1989, 6, 22), DateTime.Week) = 25, __LINE__) ;
    Assert(DateTime.Extract(DateTime.EncodeDate(1989, 6, 22), DateTime.Weekday) = 4, __LINE__) ;
    Assert(DateTime.Extract(DateTime.EncodeDate(1989, 6, 22), DateTime.Day) = 22, __LINE__) ;
    Assert(DateTime.Extract(DateTime.EncodeDate(2005, 1, 1), DateTime.Week) = 53, __LINE__) ;
    Assert(DateTime.Extract(DateTime.EncodeDate(2004, 12, 31), DateTime.Week) = 53, __LINE__) ;

    (* Diff *)
    start := DateTime.EncodeDate(2011,11,18);
    end := DateTime.EncodeDate(2023,08,01);
    d := DateTime.Diff(start, end, FALSE);
    DateTime.DecodeDate(d, year, month, day);
    Assert(year = 11, __LINE__) ;
    Assert(month = 8, __LINE__) ;
    Assert(day = 14, __LINE__) ;

    (* Span *)
    start := DateTime.EncodeDate(2011,11,18);
    end := DateTime.EncodeDate(2023,08,01);
    Assert(DateTime.Span(start, end, DateTime.Year) = 11, __LINE__) ;
    Assert(DateTime.Span(start, end, DateTime.Month) = 140, __LINE__) ;
    Assert(DateTime.Span(start, end, DateTime.Day) = 4274, __LINE__) ;

    (* Inc *)
    d := DateTime.EncodeDateTime(2019,11,15,16,43,20,0);
    DateTime.Inc(d,DateTime.Year,1);
    Assert(d = DateTime.EncodeDateTime(2020,11,15,16,43,20,0), __LINE__) ;

    d := DateTime.EncodeDateTime(2019,11,15,16,43,20,0);
    DateTime.Inc(d,DateTime.Month,12);
    Assert(d = DateTime.EncodeDateTime(2020,11,15,16,43,20,0), __LINE__) ;

    d := DateTime.EncodeDateTime(2019,11,15,16,43,20,0);
    DateTime.Inc(d,DateTime.Month,1);
    Assert(d = DateTime.EncodeDateTime(2019,12,15,16,43,20,0), __LINE__) ;

    d := DateTime.EncodeDateTime(2019,11,15,16,43,20,0);
    DateTime.Inc(d,DateTime.Day,1);
    Assert(d = DateTime.EncodeDateTime(2019,11,16,16,43,20,0), __LINE__) ;

    d := DateTime.EncodeDateTime(2019,11,15,16,43,20,0);
    DateTime.Inc(d,DateTime.Day,64);
    Assert(d = DateTime.EncodeDateTime(2020,01,18,16,43,20,0), __LINE__) ;

    d := DateTime.EncodeDateTime(2019,11,15,16,43,20,0);
    DateTime.Inc(d,DateTime.Week,1);
    Assert(d = DateTime.EncodeDateTime(2019,11,22,16,43,20,0), __LINE__) ;

    d := DateTime.EncodeDateTime(2019,11,15,16,43,20,0);
    DateTime.Inc(d,DateTime.Hour,1);
    Assert(d = DateTime.EncodeDateTime(2019,11,15,17,43,20,0), __LINE__) ;

    (* Dec *)
    d := DateTime.EncodeDateTime(2020,11,15,16,43,20,0);
    DateTime.Dec(d,DateTime.Year,1);
    Assert(d = DateTime.EncodeDateTime(2019,11,15,16,43,20,0), __LINE__) ;

    d := DateTime.EncodeDateTime(2020,11,15,16,43,20,0);
    DateTime.Dec(d,DateTime.Month,12);
    Assert(d = DateTime.EncodeDateTime(2019,11,15,16,43,20,0), __LINE__) ;

    d := DateTime.EncodeDateTime(2019,12,15,16,43,20,0);
    DateTime.Dec(d,DateTime.Month,1);
    Assert(d = DateTime.EncodeDateTime(2019,11,15,16,43,20,0), __LINE__) ;

    d := DateTime.EncodeDateTime(2019,11,16,16,43,20,0);
    DateTime.Dec(d,DateTime.Day,1);
    Assert(d = DateTime.EncodeDateTime(2019,11,15,16,43,20,0), __LINE__) ;

    d := DateTime.EncodeDateTime(2020,01,18,16,43,20,0);
    DateTime.Dec(d,DateTime.Day,64);
    Assert(d = DateTime.EncodeDateTime(2019,11,15,16,43,20,0), __LINE__) ;

    d := DateTime.EncodeDateTime(2019,11,22,16,43,20,0);
    DateTime.Dec(d,DateTime.Week,1);
    Assert(d = DateTime.EncodeDateTime(2019,11,15,16,43,20,0), __LINE__) ;

    d := DateTime.EncodeDateTime(2019,11,15,17,43,20,0);
    DateTime.Dec(d,DateTime.Hour,1);
    Assert(d = DateTime.EncodeDateTime(2019,11,15,16,43,20,0), __LINE__) ;

    (* Trunc *)
    start := DateTime.EncodeDate(2011,11,18);
    end := DateTime.EncodeDate(2011,08,01);
    Assert(DateTime.Trunc(start, DateTime.Year) = DateTime.Trunc(end, DateTime.Year), __LINE__) ;
    Assert(DateTime.Trunc(start, DateTime.Month) # DateTime.Trunc(end, DateTime.Month), __LINE__) ;

    start := DateTime.EncodeDate(2011,11,18);
    end := DateTime.EncodeDate(2011,11,01);
    Assert(DateTime.Trunc(start, DateTime.Month) = DateTime.Trunc(end, DateTime.Month), __LINE__) ;
    Assert(DateTime.Trunc(start, DateTime.Day) # DateTime.Trunc(end, DateTime.Day), __LINE__) ;

    start := DateTime.EncodeDate(2023,08,05);
    end := DateTime.EncodeDate(2023,07,31);
    Assert(DateTime.Trunc(start, DateTime.Week) = DateTime.Trunc(end, DateTime.Week), __LINE__) ;
    Assert(DateTime.Trunc(start, DateTime.Week) # DateTime.Trunc(end, DateTime.Month), __LINE__) ;

    start := DateTime.EncodeDateTime(2019,11,15,16,43,20,0);
    end := DateTime.EncodeDateTime(2019,11,15,20,43,20,0);
    Assert(DateTime.Trunc(start, DateTime.Day) = DateTime.Trunc(end, DateTime.Day), __LINE__) ;

    (* FromString *)
    exp := DateTime.EncodeDate(2020,1,1);
    DateTime.FromString(d, "2020-1-1 ", "%y-%m-%d");
    Assert(d = exp, __LINE__);

    exp := DateTime.EncodeDate(2020,1,1);
    DateTime.FromString(d, "2020-1-33", "%y-%m-%d");
    Assert(d = DateTime.ERROR, __LINE__);

    exp := DateTime.EncodeDate(2020,12,31);
    DateTime.FromString(d, "2020-12-31", "%y-%m-%d");
    Assert(d = exp, __LINE__);

    exp := DateTime.EncodeDate(2020,12,31);
    DateTime.FromString(d, "  2020-12-31", "%t%y-%m-%d");
    Assert(d = exp, __LINE__);

    exp := DateTime.EncodeDate(2020,12,31);
    DateTime.FromString(d, " 2020-12-31", "%y-%m-%d");
    Assert(d = DateTime.ERROR, __LINE__);

    exp := DateTime.EncodeDate(2020,12,31);
    DateTime.FromString(d, " 2020-12-31", "%y-%m-%d");
    Assert(d = DateTime.ERROR, __LINE__);

    exp := DateTime.EncodeDateTime(2020,12,31,18,30,00,000);
    DateTime.FromString(d, "2020-12-31 18:30:00", "%y-%m-%d %H:%M:%S");
    Assert(d = exp, __LINE__);

    exp := DateTime.EncodeDateTime(2020,12,31,18,30,00,000);
    DateTime.FromString(d, "2020-12-31 18:30:", "%y-%m-%d %H:%M:%S");
    Assert(d = DateTime.ERROR, __LINE__);

    Testing.End(test);
END Run;

END TestDateTime.