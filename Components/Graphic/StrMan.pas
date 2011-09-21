
{ ---------------------------------------------------
		String Manager   Copyright (r) by DreamFactory
    Version : 1.75   Author : William Yang
		Last Update 25 - Aug - 97
  --------------------------------------------------- }


unit StrMan; {String Manager}

interface
{-- Declaretion Part --}

uses SysUtils, NumMan, Classes, Forms, Registry;

{$I DFDefin.inc}

  {$IFDEF DELPHI}
type

	TCharSet = set of Char;
  {$ENDIF}
  
  {$IFDEF DELPHI}
const
//  StdChrs = ['!'..'~'];

  Decimals = ['0'..'9'];
  FloatNums = ['0'..'9', '.'];
  {$ENDIF}
//  Operators = ['+', '-', '*', '/'];
//  HexDecimals = ['0'..'9', 'A'..'F'];
//  Letters = ['a'..'z', 'A'..'Z'];
//  Symbols = ['"', '''', '<', '>', '{', '}', '[', ']', '(', ',', ')'];
//  Masks : array[1..3] of Char  = ('*', '?', '#');

function ReplaceOne(Src, Ch : String; iStart, iCount : Integer ) : String;
function FillStr(Amount : Byte; C : Char) : String;
function TitleCase(SourceStr : String) : String;
function ReplaceAll(Source , Target, ChangeTo : String) : String ;
function Instr(iStart : Integer; Src, Find: String) : Integer;
function CompareStrAry(Source : String; CmpTo : Array of string) : Integer;
function LowCaseStr(S : String) : String;
function LoCase(C : Char) : Char;
procedure StrSplit(SrcStr : String; BreakDownPos : Integer;
	var S1, S2 : String);
function LeftStr(S : String; ToPos : Integer) : String;
function RightStr(S : String; ToPos : Integer) : String;
function CharCount(S : String; C : Char) : Integer;

{$IFDEF DELPHI}
function RemoveChars(S : String; C : TCharSet) : String;
function EStrToInt(S : String) : Integer;
function EStrToFloat(S : String) : Real;
{$ENDIF}

function LastDir(Dir: String): String;

function RPos(C: ShortString; Src : String) : Integer;

function ReturnLine(SList: TStringList; Find: String) : String;
procedure SplitStrC (S:string; C : char; var head, queue : string);
procedure Split(StringList: TStringList; S : string; C : char);
function AppPath: String;

function ReadBetween(Src, Mark1, Mark2: String): String;

implementation

{function RegSearch(Reg: TRegistry; Value: String);
begin
	Reg.Rootkey := HKEY_CLASSES_ROOT;
  Reg.
	RegSearchNext(Reg, Value);
end;

function RegSearchNext(Reg: TRegstry; Value: String);
begin

end; }


function ReadBetween(Src, Mark1, Mark2: String): String;
var
	i,j : Integer;
begin
	i := Pos(Mark1, Src);
  j := InStr(i + Length(Mark1), Src, Mark2);
  if (i > 0) and (j > 0) then
  	Result := Copy(Src, i + Length(Mark1) + 1, j-1);
end;

function AppPath: String;
begin
	Result := ExtractFilepath(Application.Exename);
end;

function LastDir(Dir: String): String;
begin
	if Dir[Length(Dir)]='\' then
  	Delete(Dir, Length(Dir), 1);
  Result := RightStr(Dir, RPos('\', Dir)+1);
end;

procedure SplitStrC (S:string; C : char; var head, queue : string);
var
    Index : integer;
begin
   head:=''; queue:='';
   Index := pos(C,S);
   if Index=0 then Index:=length(S)+1;
   head := copy(S,1,Index-1);
   delete(S,1,Index);
   queue := S;
end;

procedure Split(StringList: TStringList; S : string; C : char);
var
	i : Integer;
  Line: String;
begin
  while S <> '' do
  begin
   SplitStrC(S, C, Line, S);
   StringList.Add(Line);
  end;
end;


function ReturnLine(SList: TStringList; Find: String) : String;
var
	i :Integer;
  s: String;
begin
	Result := '';
	for i := 0 to SList.Count - 1 do
  begin
    s := SList[i];
  	if Pos(Find, s) > 0 then
    begin
    	Result := SList[i];
      Exit;
    end;
  end;
end;

{$IFDEF DELPHI}
function EStrToFloat(S : String) : Real;
var
	i : Integer;
  r : String;
begin
	r := '';
	for i := 1 to Length(S) do
  	if s[i] in FloatNums then
    	r := r + s[i];
  if r = '' then
  	Result := 0
  else
  	Result := StrToFloat(r);
end;

function EStrToInt(S: String) : Integer;
var
	i : Integer;
  r : String;
begin
	r := '';
	for i := 1 to Length(S) do
  	if s[i] in Decimals then
    	r := r + s[i];
  if r = '' then
  	Result := 0
  else
  	Result := StrToInt(r);
end;

function RemoveChars(S : String; C : TCharSet) : String;
var
  j : Integer;
begin
  Result := S;
	j := 1;
  while j <= Length(Result) do
	begin
    while Result[j] in C do
    	Delete(Result, j, 1);
    Inc(j);
  end;
end;

{$ENDIF}


function ReplaceOne(Src, Ch : String; iStart, iCount : Integer) : String;
var mResult : String;
begin
	mResult := Src;
	Delete(mResult, iStart, iCount);
	Insert(Ch, mResult, iStart);
	ReplaceOne := mResult;
end;

function Instr(iStart : Integer; Src, Find: String) : Integer;
var
  CS : String;
begin
		CS := Copy(Src, iStart, Length(Src)-iStart+1);
		if Pos(Find, CS) <> 0 then
			Result := Pos(Find, CS) + iStart - 1
		else
			Result := 0;
end;

function LeftStr(S : String; ToPos : Integer) : String;
begin
	Result := Copy(S, 1, ToPos);
end;

function RightStr(S : String; ToPos : Integer) : String;
begin
	Result := Copy(S, ToPos, Length(S) - ToPos + 1);
end;

procedure StrSplit(SrcStr : String; BreakDownPos : Integer;
	var S1, S2 : String);
begin
	S1 := LeftStr(SrcStr, BreakDownPos - 1);
	S2 := RightStr(SrcStr, BreakDownPos - 1);
end;

function ReplaceAll(Source , Target, ChangeTo : String) : String ;
var
i, Index : Integer;
Src, Tgt, Cht : String;
begin
	Src := Source;
  Tgt := Target;
  Cht := ChangeTo;
	Index := Pos(Tgt, Src);
	i := 0;
  while Index > 0 do
	begin
		Src := ReplaceOne(Src, Cht, Index, Length(Tgt));
		Index := Index + Length(Cht);
		Index := Instr(Index, Src, Tgt);
		i := i + 1;
	end;
  Result := Src;
end;

function LoCase(C : Char) : Char;
begin
	if (Ord(C) >= Ord('A')) and (Ord(C) <= Ord('Z')) then
		Result := Chr(Ord(C) - (Ord('A') - Ord('a')));
end;

function LowCaseStr(S : String) : String;
var i : Integer;
begin
	for i := 1 to Length(S) do
  	S[i] := LoCase(S[i]);
end;

{Make The First Letter Of Each Word To Upper Case}
function TitleCase(SourceStr : String) : String;

var
  I : Integer;
  First : boolean;
begin
  Result := SourceStr;
  First := True;
  for I := 1 to Length(SourceStr) do
  begin
		if First then
			Result[I] := UpCase(Result[I])
		else
			Result[I] := LoCase(Result[I]);
		First := False;
  	if Result[I] = ' ' then First := True;
  end;
  TitleCase := Result;
end;

{Fill The String With Parameter 'C'}
function FillStr(Amount : Byte; C : Char) : String;
var R : String;
	i : byte;
begin
	for i :=1 to Amount do
		R := R + C;
	Result := R;

end;

function CompareStrAry(Source : String; CmpTo : Array of string) : integer;
var i : Integer;
begin
	Result := -1;
	for i := Low(CmpTo) to High(CmpTo) do
  begin
  	if LowCaseStr(Source) = LowCaseStr(CmpTo[i]) then
    begin
    	Result := i;
      Exit;
    end;
  end;
end;

function RPos(C: ShortString; Src : String) : Integer;
var
	i : integer;
begin
  Result := 0;
	for i := Length(Src) downto 1 do
  	if Src[i] = C then
    begin
      Result := i;
     	Break;
    end;
end;

function CharCount(S : String; C : Char) : Integer;
var i : Integer;
begin
	Result := 0;
	for i := 1 to Length(S) do
  	if S[i] = C then Result := Result + 1;
end;

end.






