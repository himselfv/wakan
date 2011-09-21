
{ ----------------------------------------------------------------------
	Date and Time Manager written by William Yang (Dream Factory)
  E-mail : yang@btinternet.com     URL : http://www.btinternet.com/~yang
  ---------------------------------------------------------------------- }

{ ---------------------------------------------------------------------------
	TDateTime is formed by Date(Whole number) and Time(Float number).
  Which means a day in DateTime format will be "1", an hour will be "1/24",
  and so on...
  This table shows you all.
  +---------+-------------------+
  | PARTS   | IN DATETIME VALUE |
  +---------+-------------------+
  | Year    | 365 (Day/Year)    |
  | Month   | 31 (Day/Month)    |
  | Day     | 1                 |
  | Hour    | 1/24              |
  | Minutes | 1/24/60           |
  | Seconds | 1/24/60/60        |
  | M. Sec  | 1/24/60/60/1000   |
  +---------+-------------------+

  ---------------------------------------------------------------------------}
  
unit DateMan;

interface

uses SysUtils, Windows, NumMan, StrMan;

const
	HoursPerDay = 24;
  MinsPerHour = 60;
  SecondsPerMin = 60;
  MonthsPerYear = 12;
	First = 1;

	ValidDateChar = ['/', '0'..'9']; 

{ Return how many days in the given year of month }
function DayInMonth(Year, Month: Integer): Integer;
{ Return if given year is Leap Year }
function IsLeapYear(Year: Word): Boolean;

{ Return the day of the date }
function GetDay(Dest: TDateTime): Integer;
{ Replace the day with different value }
function SetDay(Dest: TDateTime; A: Word): TDateTime;
{ Return the month of the date }
function GetMonth(Dest: TDateTime): Integer;
{ Change the month }
function SetMonth(Dest: TDateTime; A: Word): TDateTime;
{ Return the year of the date }
function GetYear(Dest: TDateTime): Integer;
{ Set the year of the date }
function SetYear(Dest: TDateTime; A: Word): TDateTime;
{ Return the second of the time }
function GetSec(Dest: TDateTime): Integer;
{ Set the second of the time }
function SetSec(Dest: TDateTime; A: Word): TDateTime;
{ Get the hour of the time }
function GetHour(Dest: TDateTime):Integer;
{ Set the hour of the time }
function SetHour(Dest:TDateTime; A: Word): TDateTime;
{ Set the minute of the time }
function SetMin(Dest:TDateTime; A: Word): TDateTime;
{ Get the minute of the time }
function GetMin(Dest:TDateTime): Integer;

{ Return move the day to the end of the month }
function  EndOfMonth(D: TDateTime): TDateTime;
{ Return move the day to the begin of the month }
function  BeginOfMonth(D: TDateTime): TDateTime;
{ Return move the day to the end of the year }
function  EndOfYear(D: TDateTime): TDateTime;
{ Return move the day to the begin of the year }
function  BeginOfYear(D: TDateTime): TDateTime;

{ Increase days in DateTime }
procedure IncDay(var Dest: TDateTime; A: Integer);
{ Decrease days in DateTime }
procedure DecDay(var Dest: TDateTime; A: Integer);
{ Increase months in DateTime }
procedure IncMonth(var Dest: TDateTime; A: Integer);
{ Decrease months in DateTime }
procedure DecMonth(var Dest: TDateTime; A: Integer);
{ Increase Years in DateTime }
procedure IncYear(var Dest: TDateTime; A: Integer);
{ Decrease years in DateTime }
procedure DecYear(var Dest: TDateTime; A: Integer);
{ Increase seconds in DateTime }
procedure IncSec(var Dest: TDateTime; A: Integer);
{ Decrease seconds in DateTime }
procedure DecSec(var Dest: TDateTime; A: Integer);
{ Increase minutes in DateTime }
procedure IncMin(var Dest: TDateTime; A: Integer);
{ Decrease minutes in DateTime }
procedure DecMin(var Dest: TDateTime; A: Integer);
{ Increase hours in DateTime }
procedure IncHour(var Dest: TDateTime; A: Integer);
{ Decrease hours in DateTime }
procedure DecHour(var Dest: TDateTime; A: Integer);

{ Calculate date by hours }
function DateToHours(Src: TDateTime): Integer;
{ Calculate date by seconds }
function DateToSeconds(Src: TDateTime): Integer;
{ Calculate date by minutes }
function DateToMinutes(Src: TDateTime): Integer;

{ Convert hours back to Date }
function HoursToDate(A: Integer): TDateTime;
{ Convert seconds back to Date }
function SecondsToDate(A: Integer): TDateTime;
{ Convert minutes back to Date }
function MinutesToDate(A: Integer): TDateTime;

function EStrToDate(Str: String): Integer;
function StrMonth(i: Integer): String;
function StrMonthShort(i: Integer): String;

implementation

function StrMonth(i: Integer): String;
begin
	Result := FormatDateTime('mmmm', i*30);
end;

function StrMonthShort(i: Integer): String;
begin
	Result := FormatDateTime('mmm', i*30);
end;

function EStrToDate(Str: String): Integer;
var
	i: Integer;
  VD: String;
begin

	Str := upperCase(Str);
	Str := ReplaceAll(Str, '-', '/');
	Str := ReplaceAll(Str, '.', '/');
	Str := ReplaceAll(Str, '\', '/');
	Str := ReplaceAll(Str, '|', '/');
	Str := ReplaceAll(Str, '''', '/');
	Str := ReplaceAll(Str, ',', '/');
	for i := 1 to 12 do
		Str := ReplaceAll(Str, upperCase(StrMonthShort(i)), IntToStr(i));

  for i := 1 to Length(Str) do
  begin
  	if Str[i] in ValidDateChar then
    	VD := VD + Str[i];
  end;
  Result := Trunc(StrToDate(VD));
end;

function DayInMonth(Year, Month: Integer): Integer;
begin
	case Month of
  	2: Result := 28;
  	4, 6, 9, 11 : Result := 30;
  else Result := 31;
  end;
  if IsLeapYear(Year) and (Month = 2) then
  	Inc(Result);
end;

function IsLeapYear(Year: Word): Boolean;
begin
	if (Year / 4) = (Year div 4) then
  	Result := True
  else
  	Result := False;
end;

{ Date reading functions }
function GetDay(Dest: TDateTime): Integer;
var
  Year,Month,Day : Word;
begin
  DecodeDate(Dest,Year,Month,Day);
  Result:=Day;
end;

{ These procedures are copied from xProcs by            }
{       Copyright (c) 1995 Stefan Böther                }
{                            stefc@fabula.com           }
function GetMonth(Dest: TDateTime): Integer;
var
  Year,Month,Day : Word;
begin
  DecodeDate(Dest,Year,Month,Day);
  Result:=Month;
end;

function BeginOfYear(D: TDateTime): TDateTime;
var
  Year,Month,Day : Word;
begin
  DecodeDate(D,Year,Month,Day);
  Result:=EncodeDate(Year,1,1);
end;

function  EndOfYear(D: TDateTime): TDateTime;
var
  Year,Month,Day : Word;
begin
  DecodeDate(D,Year,Month,Day);
  Result:=EncodeDate(Year,12,31);
end;

function  BeginOfMonth(D: TDateTime): TDateTime;
var
  Year,Month,Day : Word;
begin
  DecodeDate(D,Year,Month,Day);
  Result:=EncodeDate(Year,Month,1);
end;

function  EndOfMonth(D: TDateTime): TDateTime;
var
  Year,Month,Day : Word;
begin
  DecodeDate(D,Year,Month,Day);
  if Month=12 then
  begin
    Inc(Year);
    Month:=1;
  end else
    Inc(Month);
  Result:=EncodeDate(Year,Month,1)-1;
end;

{ End Copies}

function SetDay(Dest: TDateTime; A: Word): TDateTime;
begin
  Result := Dest - GetDay(Dest) + A;
end;

function SetMonth(Dest: TDateTime; A: Word): TDateTime;
var
  Year,Month,Day : Word;
begin
  DecodeDate(Dest,Year,Month,Day);
  Result:=EncodeDate(Year,A,Day);
end;

function GetYear(Dest: TDateTime): Integer;
var
  Year,Month,Day : Word;
begin
  DecodeDate(Dest,Year,Month,Day);
	Result := Year;
end;

function SetYear(Dest: TDateTime; A : Word): TDateTime;
var
  Year,Month,Day : Word;
begin
  DecodeDate(Dest,Year,Month,Day);
  Result:=EncodeDate(A,Month,Day);
end;

function GetSec(Dest: TDateTime): Integer;
var
  H, M, S, MS : Word;
begin
  DecodeTime(Dest,H,M,S,MS);
  Result := S;
end;

function SetSec(Dest: TDateTime; A: Word): TDateTime;
begin
	Result := Dest;
  DecSec(Result, GetSec(Result));
  IncSec(Result, A);
end;

function GetHour(Dest: TDateTime):Integer;
var
  H, M, S, MS : Word;
begin
  DecodeTime(Dest,H,M,S,MS);
  Result := H;
end;

function SetHour(Dest:TDateTime; A: Word): TDateTime;
begin
	Result := Dest;
  DecHour(Result, GetSec(Dest));
  IncHour(Result, A);
end;

function GetMin(Dest:TDateTime): Integer;
var
  H, M, S, MS : Word;
begin
  DecodeTime(Dest,H,M,S,MS);
  Result := M;
end;

function SetMin(Dest:TDateTime; A: Word): TDateTime;
begin
	Result := Dest;
  DecMin(Result, GetSec(Dest));
  IncMin(Result, A);
end;

procedure IncDay(var Dest: TDateTime; A: Integer);
begin
	Dest := Dest + A;
end;

procedure DecDay(var Dest: TDateTime; A: Integer);
begin
	Dest := Dest - A;
end;

procedure IncMonth(var Dest: TDateTime; A: Integer);
var
	i : Integer;
	CM, Year, Month: Integer;
begin
	Cm := GetMonth(Dest);
	for i := Cm to Cm + A do
	begin
  	Year := GetYear(Dest);
		Month := MakeBetween(Cm, First, MonthsPerYear);
		Dest := Dest + DayInMonth(Year, Month);
	end;
end;

procedure DecMonth(var Dest: TDateTime; A: Integer);
var
	i : Integer;
	CM, Year, Month: Integer;
begin
	Cm := GetMonth(Dest);
	for i := Cm downto Cm - A do
	begin
  	Year := GetYear(Dest);
		Month := MakeBetween(Cm, First, MonthsPerYear);
		Dest := Dest - DayInMonth(Year, Month);
	end;
end;

procedure IncSec(var Dest: TDateTime; A: Integer);
begin
	Dest := Dest + A / HoursPerDay / MinsPerHour / SecondsPerMin;
end;

procedure DecSec(var Dest: TDateTime; A: Integer);
begin
	Dest := Dest - A / HoursPerDay / MinsPerHour / SecondsPerMin;
end;

procedure IncMin(var Dest: TDateTime; A: Integer);
begin
	Dest := Dest + A / HoursPerDay / MinsPerHour;
end;

procedure DecMin(var Dest: TDateTime; A: Integer);
begin
	Dest := Dest - A / HoursPerDay / MinsPerHour;
end;

procedure IncYear(var Dest: TDateTime; A: Integer);
var
  Year,Month,Day : Word;
begin
  DecodeDate(Dest, Year, Month, Day);
  Dest:=EncodeDate(Year+A, Month, Day) + Frac(Dest);
end;

procedure DecYear(var Dest: TDateTime; A: Integer);
var
  Year,Month,Day : Word;
begin
  DecodeDate(Dest, Year, Month, Day);
  Dest:=EncodeDate(Year-A, Month, Day) + Frac(Dest);
end;

procedure IncHour(var Dest: TDateTime; A: Integer);
begin
	Dest := Dest + A / HoursPerDay;
end;

procedure DecHour(var Dest: TDateTime; A: Integer);
begin
	Dest := Dest - A / HoursPerDay;
end;

function DateToHours(Src: TDateTime): Integer;
begin
	Result := Trunc(Src * HoursPerDay);
end;

function DateToSeconds(Src: TDateTime): Integer;
begin
	Result := Trunc(Src * HoursPerDay * MinsPerHour * SecondsPerMin);
end;

function DateToMinutes(Src: TDateTime): Integer;
begin
	Result := Trunc(Src * HoursPerDay * MinsPerHour);
end;

function HoursToDate(A: Integer): TDateTime;
begin
	Result := A / HoursPerDay;
end;

function SecondsToDate(A: Integer): TDateTime;
begin
	Result := A / HoursPerDay / MinsPerHour / SecondsPerMin;
end;

function MinutesToDate(A: Integer): TDateTime;
begin
	Result := A / HoursPerDay / MinsPerHour;
end;

end.
