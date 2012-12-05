unit JWBUnit;

interface

uses Graphics, Windows, SysUtils, Classes, Dialogs, Grids, Forms, ExtCtrls, Registry,
  JWBUtils;

{$IF CompilerVersion < 21}
type
 //On older compilers we don't have a new, faster UnicodeString,
 //but we can (mostly) safely use a slower WideString in its stead.
  UnicodeString = WideString;
  PUnicodeString = PWideString;
{$IFEND}

{ Wakan uses a special Ansi string format, where each unicode symbol is kept
 as a 4 character (four-char) hex code.
 Since we're in the transition to unicode we'll have a special type for that,
 called four-char.
 It'll resolve to normal character on the platform }
type
 {$IFDEF UNICODE}
  FChar = char;
  PFChar = PChar;
  FString = string;
 {$ELSE}
  FChar = AnsiString; //because one character takes 4 AnsiChars
  PFChar = PAnsiChar;
  FString = AnsiString;
  //Useful for looking into FChars
  FCharData = array[1..4] of AnsiChar;
  PFCharData = ^FCharData;
 {$ENDIF}


const
 {
  Some character constants used throught the program
  Names MUST be descriptive. If you name something 'UH_SPACE' it means it FUNCTIONS as space,
  not just looks like one.
 }
 {$IFNDEF UNICODE}
  UH_NONE:FChar = ''; //for initializing stuff with it
  UH_ZERO:FChar = '0000'; //when you need zero char
  UH_LF: FChar = '000A'; //linefeed
  UH_CR: FChar = '000D'; //carriage return
  UH_SPACE:FChar = '0020';
  UH_HYPHEN:FChar = '002D'; //-
  UH_LOWLINE:FChar = '005F'; //_
  UH_ELLIPSIS:FChar = '2026'; //…
  UH_IDG_SPACE:FChar = '3000'; //Ideographic space (full-width)
  UH_IDG_COMMA:FChar = '3001';

 {
  Control Characters.
  Wakan uses single characters like 'U', '~', '@' to control text markup.
  We can't do that in Unicode because those characters can legitimately occur in text.
  Instead we use symbols U+E000..U+F8FF which are for private use by applications.

  Not all control characters are well understood, so they don't yet have descriptive names.
  For non-descriptive names use prefix ALTCH_
 }
  UH_UNKNOWN_KANJI:Char = 'U'; //Set by CheckKnownKanji

 { All used by DrawWordInfo }
  ALTCH_EXCL:Char = '!';
  ALTCH_SHARP:Char = '#';
  ALTCH_AT:Char = '@';
  ALTCH_TILDE:Char = '~'; //followed by 'I' and means 'italic'
  UH_SETCOLOR:Char = '%'; //followed by 6 character hex color
  UH_LBEG: Char ='<'; //begin flag text (ex.: <dEDICT> <gram> <suf>)
  UH_LEND: Char = '>'; //end flag text

 {$ELSE}
  UH_NONE:FChar = #$0000;
  UH_ZERO:FChar = #$0000;
  UH_LF: FChar = #$000A;
  UH_CR: FChar = #$000D;
  UH_SPACE:FChar = #$0020;
  UH_HYPHEN:FChar = '-';
  UH_LOWLINE:FChar = '_';
  UH_ELLIPSIS:FChar = #$2026;
  UH_IDG_SPACE:FChar = #$3000;
  UH_IDG_COMMA:FChar = #$3001;

  UH_UNKNOWN_KANJI:Char = #$E001;

  ALTCH_EXCL:Char = #$E002;
  ALTCH_SHARP:Char = #$E003;
  ALTCH_AT:Char = #$E004;
  ALTCH_TILDE:Char = #$E005;
  UH_SETCOLOR:Char = #$E006;
  UH_LBEG:Char = #$E008;
  UH_LEND:Char = #$E007;
 {$ENDIF}


{ Math }

{ Min and max so we don't have to link Math.pas just for that }
function min(a, b: integer): integer; inline;
function max(a, b: integer): integer; inline;


{ Files }

function GetModuleFilenameStr(hModule: HMODULE = 0): string;
function GetFileVersionInfoStr(Filename: string): string;


{ Strings }

function flength(s:FString): integer; {$IFDEF INLINE}inline;{$ENDIF}
function flenfc(lenn:integer): integer; {$IFDEF INLINE}inline;{$ENDIF}
function flenn(lenfc:integer): integer; {$IFDEF INLINE}inline;{$ENDIF}
function fcopy(s: FString; Index, Count: Integer):FString; {$IFDEF INLINE}inline;{$ENDIF}
procedure fdelete(var s: FString; Index, Count: Integer); {$IFDEF INLINE}inline;{$ENDIF}
function fgetch(s: FString; Index: integer): FChar; {$IFDEF INLINE}inline;{$ENDIF}
function fstr(s: string): FString; {$IFDEF INLINE}inline;{$ENDIF}

{$IFNDEF UNICODE}
function FcharCmp(a, b: PFChar; cnt: integer): boolean; {$IFDEF INLINE}inline;{$ENDIF}
{$ENDIF}

function HexCharCode(c:AnsiChar): byte; {$IFDEF INLINE}inline;{$ENDIF}
function HexCharCodeW(c:WideChar): byte; {$IFDEF INLINE}inline;{$ENDIF}
function HexToUnicode(ps:PAnsiChar; maxlen: integer): UnicodeString; overload;
function HexToUnicodeW(ps:PWideChar; maxlen: integer): UnicodeString; overload;
function HexToUnicode(const s:string):UnicodeString; overload;
function ByteToHex(pb:PByte;sz:integer):string;
function UnicodeToHex(const s:UnicodeString):string;
function AnsiToHex(const s:AnsiString):string; {$IFDEF INLINE}inline;{$ENDIF}
function CombUniToHex(s:string):string;
function HexToCombUni(s:string):string;
function UnicodeToML(s:widestring):string;

type
  TStringArray = array of string;

procedure SplitAdd(sl:TStringList;s:string;cnt:integer);
function SplitStr(s: string; cnt: integer): TStringArray;
procedure StrListAdd(sl: TStringList; sa: TStringArray);

function remexcl(s:string):string;
function strip_fl(s:string):string;


{ Rest }


procedure BeginDrawReg(p:TPaintBox);
procedure EndDrawReg;
function FindDrawReg(p:TPaintBox;x,y:integer;var cx,cy,cy2:integer):string;
procedure DrawUnicode(c:TCanvas;x,y,fs:integer;ch:FString;fontface:string);
function ConvertPinYin(s:string):FString;
function DeconvertPinYin(s:FString):string;
procedure DrawKana(c:TCanvas;x,y,fs:integer;ch:string;fontface:string;showr:boolean;romas:integer;lang:char);
function KanaToRomaji(s:FString;romatype:integer;lang:char):string;
function RomajiToKana(s:string;romatype:integer;clean:boolean;lang:char):FString;
procedure FreeRomaList;
function DrawWordInfo(canvas:TCanvas; Rect:TRect; sel,titrow:boolean; colx:integer; s:string; multiline,onlycount:boolean;fontsize:integer;boldfont:boolean):integer;
procedure DrawPackedWordInfo(canvas: TCanvas; Rect:TRect; s:string; ch:integer;boldfont:boolean);
procedure DrawWordCell(Grid:TStringGrid; ACol, ARow: Integer; Rect: TRect; State: TGridDrawState);
procedure DrawKanjiCard(canvas:TCanvas;u:string;x,y:integer;ch:double;stcount,outlin,alt,rad,inlin,comp,read,mean,strokeorder,fullcomp,sortfreq:boolean;sizhor,sizvert,nofullcomp:integer;calfont:string);
procedure ClearKanjiCardCache;
procedure FillWordGrid(grid:TStringGrid;sl:TStringList;stat,learn:boolean);
procedure CreateKnownList(listno:integer;charnumber:integer);
procedure FreeKnownLists;
procedure SaveKnownList(listno:integer;filename:string);
procedure LoadKnownList(listno:integer;stream:TStream);
procedure PaintScreenTipBlock;
procedure SetScreenTipBlock(x1,y1,x2,y2:integer;canvas:TCanvas);
function ConvertEdictEntry(s:string;var mark:string):string;
function GetMarkAbbr(mark:char):string;
function EnrichDictEntry(s,mark:string):string;
function ChinTo(s:string):string;
function ChinFrom(s:string):string;

function IsKnown(listno:integer;const char:FChar):boolean; overload;
procedure SetKnown(listno:integer;const char:FChar;known:boolean); overload;
function FirstUnknownKanjiIndex(const kanji:FString):integer;
function CheckKnownKanji(const kanji:FString): FString;
{$IFDEF UNICODE}
function IsKnown(listno:integer;const char:FString):boolean; overload; {$IFDEF INLINE}inline;{$ENDIF}
procedure SetKnown(listno:integer;const char:FString;known:boolean); overload; {$IFDEF INLINE}inline;{$ENDIF}
{$ENDIF}

const //Character types for EvalChar
  EC_UNKNOWN          = 0; // unrecognized
  EC_IDG_CHAR         = 1; // ideographic char
  EC_HIRAGANA         = 2; // hiragana
  EC_KATAKANA         = 3; // katakana
  EC_IDG_PUNCTUATION  = 4; // ideographic punctuation
  EC_IDG_OTHER        = 5; // ideographic other
  EC_LATIN_FW         = 6; // full-width latin
  EC_LATIN_HW         = 7; // half-width latin
  EC_KATAKANA_HW      = 8; // half-width katakana

{$IFDEF UNICODE}
function EvalChar(char:WideChar):integer; overload; {$IFDEF INLINE}inline;{$ENDIF}
function EvalChar(const char:FString):integer; overload; {$IFDEF INLINE}inline;{$ENDIF}
{$ELSE}
function EvalChar(const char:FString):integer;
{$ENDIF}

function StateStr(i:integer):string;
function DateForm(s:string):string;
procedure WritelnMixUni(var f:file;s:string);
function StripCatName(s:string):string;
procedure WriteMarkTable;
procedure InitWordGrid(grid:TStringGrid;stat,learn:boolean);
procedure AddWordGrid(var grid:TStringGrid;sp1,sp2,sp4,sp3:string);
procedure FinishWordGrid(grid:TStringGrid);
procedure SplitWord(s:string; var sp1,sp2,sp4,sp3:string);
procedure InitColors;
function GetColorString(i:integer):string;
procedure SetCol(col:integer;val:TColor);
function GetCol(col:integer):TColor;
function Col(col:string):TColor;
procedure WriteColors;
procedure SetColDefault(i:integer);
procedure DrawStrokeOrder(canvas:TCanvas;x,y,w,h:integer;char:string;fontsize:integer;color:TColor);

const Color_Max=100;

var FontStrokeOrder,FontChinese,FontChineseGB,FontChineseGrid,FontChineseGridGB,FontJapaneseGrid,FontJapanese,FontSmall,FontRadical,FontEnglish,FontPinYin:string;
    kcchind,kcchcomp:TStringList;
    GridFontSize:integer;

   { Romaji translation table.
    See comments where class is defined. }
    roma_t: TRomajiTranslationTable;

   { Chinese version, not upcased. Someone upgrade this one too... }
    romac: TStringList;

implementation
uses StrUtils, JWBMenu, JWBSettings, JWBUser, JWBDicSearch;

{ Math }

function min(a, b: integer): integer;
begin
  if a<b then Result := a else Result := b;
end;

function max(a, b: integer): integer;
begin
  if a<b then Result := a else Result := b;
end;


{ Files }

(*
  Returns full image address for specified module
  If hModule is zero, returns full executable image address
*)
function GetModuleFilenameStr(hModule: HMODULE = 0): string;
const MAX_PATH_LEN = 8192; //Max length, in symbols, of supported image path size.
var nSize, nRes: dword;
begin
 (*
   MSDN:
    If the length of the path is less than nSize characters, the function succeeds
    and the path is returned as a null-terminated string.

    If the length of the path exceeds nSize, the function succeeds and the string
    is truncated to nSize characters including the terminating null character.

    Windows XP/2000: The string is truncated to nSize characters and is not null terminated.
 *)

  nSize := 256;
  SetLength(Result, nSize);

  nRes := GetModuleFilename(hModule, @Result[1], nSize);
  while (nRes <> 0) and (nRes >= nSize) and (nSize < MAX_PATH_LEN) do begin
    nSize := nSize * 2;
    SetLength(Result, nSize+1);
    nRes := GetModuleFilename(hModule, @Result[1], nSize);
  end;

  if nRes = 0 then begin
    Result := ''; //cannot retrieve path, return null
    exit;
  end;

  if nRes >= nSize then begin
    Result := ''; //path too long, exceeded MAX_PATH_LEN and still not enough, return null
    exit;
  end;

  SetLength(Result, nRes); //else truncate the string, set terminating null
end;

function GetFileVersionInfoStr(Filename: string): string;
var verSize: dword;
  verHandle: dword;
  verData: array of byte;
  buf: pointer;
  len: cardinal;
begin
  verSize := GetFileVersionInfoSize(PChar(Filename), verHandle);
  if verSize=0 then begin
    Result := '';
    exit;
  end;

  buf := nil;
  len := 0;
  SetLength(verData, verSize);
  if not GetFileVersionInfo(PChar(Filename), verHandle, verSize, @verData[0]) then begin
    Result := '';
    exit;
  end;

  if not VerQueryValue(@verData[0], '\', buf, len)
  or (len=0) then begin
    Result := '';
    exit;
  end;

  with PVSFixedFileInfo(buf)^ do begin
   //dwFileVersionMS: Major version (HIWORD) + Minor version (LOWORD)
   //dwFileVersionLS: Release (HIWORD) + Build (LOWORD)
    Result :=
      IntToStr(HIWORD(dwFileVersionMS)) + '.' + IntToStr(LOWORD(dwFileVersionMS));
    if dwFileFlags and VS_FF_PRERELEASE = VS_FF_PRERELEASE then
      Result := Result + ' (dev)';
    if dwFileFlags and VS_FF_DEBUG = VS_FF_DEBUG then
      Result := Result + ' (debug)';
    if dwFileFlags and VS_FF_SPECIALBUILD = VS_FF_SPECIALBUILD then
      Result := Result + ' (special build)';
  end;
end;


{ Strings }

//returns length of the string in 4-characters
function flength(s:FString):integer;
begin
 {$IFDEF UNICODE}
  Result := Length(s);
 {$ELSE}
  Result := Length(s) div 4;
 {$ENDIF}
end;

//returns length in Native characters by length in 4-characters
//  i.e. 3 four-chars == 12 native characters on non-unicode
//       3 four-chars == 3 native characters on unicode
function flenn(lenfc:integer): integer;
begin
 {$IFDEF UNICODE}
  Result := lenfc;
 {$ELSE}
  Result := lenfc * 4;
 {$ENDIF}
end;

//returns length in 4-characters by length in native characters
function flenfc(lenn:integer): integer;
begin
 {$IFDEF UNICODE}
  Result := lenn;
 {$ELSE}
  Result := lenn div 4;
 {$ENDIF}
end;

//a version of copy for FChars where you specify length in FChars
//spares us multiplying by four every time, and on Unicode resolves to simpler version
function fcopy(s: FString; Index, Count: Integer):FString;
begin
{$IFDEF UNICODE}
  Result := copy(s, Index, Count);
{$ELSE}
  Result := copy(s, (Index-1)*4+1, Count*4);
{$ENDIF}
end;

procedure fdelete(var s: FString; Index, Count: Integer);
begin
{$IFDEF UNICODE}
  delete(s, Index, Count);
{$ELSE}
  delete(s, (Index-1)*4+1, Count*4);
{$ENDIF}
end;

function fgetch(s: FString; Index: integer): FChar;
begin
{$IFDEF UNICODE}
  Result := s[Index];
{$ELSE}
  Result := fcopy(s, Index, 1);
{$ENDIF}
end;

//Converts raw (unicode) data to FString. On unicode does nothing!
function fstr(s: string): FString; {$IFDEF INLINE}inline;{$ENDIF}
begin
 {$IFDEF UNICODE}
  Result := s;
 {$ELSE}
  Result := UnicodeToHex(s);
 {$ENDIF}
end;


{$IFNDEF UNICODE}
//Compares a cnt of 4-characters
//There should be at least this number of 4-characters in at least one of a, b
function FcharCmp(a, b: PFChar; cnt: integer): boolean;
var ia: PInteger absolute a;
  ib: PInteger absolute b;
begin
  while (cnt>0) and (ia^=ib^) do begin
    Inc(ia);
    Inc(ib);
    Dec(cnt);
  end;
  Result := cnt<=0;
end;
{$ENDIF}


{
HexToUnicode() is used in RomajiToKana() so it can be a bottleneck.
We'll try to do it fast.
}

//Increments a valid PChar pointer 4 characters or returns false if the string ends before that
function EatOneFChar(var pc: PAnsiChar): boolean; {$IFDEF INLINE}inline;{$ENDIF}
begin
  Result := false;
  if pc^=#00 then exit;
  Inc(pc);
  if pc^=#00 then exit;
  Inc(pc);
  if pc^=#00 then exit;
  Inc(pc);
  if pc^=#00 then exit;
  Inc(pc);
  Result := true;
end;

function EatOneFCharW(var pc: PWideChar): boolean; {$IFDEF INLINE}inline;{$ENDIF}
begin
  Result := false;
  if pc^=#00 then exit;
  Inc(pc);
  if pc^=#00 then exit;
  Inc(pc);
  if pc^=#00 then exit;
  Inc(pc);
  if pc^=#00 then exit;
  Inc(pc);
  Result := true;
end;

//Returns a value in range 0..15 for a given hex character, or throws an exception
function HexCharCode(c:AnsiChar): byte;
begin
  if (ord(c)>=ord('0')) and (ord(c)<=ord('9')) then
    Result := ord(c)-ord('0')
  else
  if (ord(c)>=ord('A')) and (ord(c)<=ord('F')) then
    Result := 10 + ord(c)-ord('A')
  else
  if (ord(c)>=ord('a')) and (ord(c)<=ord('f')) then
    Result := 10 + ord(c)-ord('a')
  else
    raise Exception.Create('Invalid hex character "'+c+'"');
end;

function HexCharCodeW(c:WideChar): byte;
begin
  if (ord(c)>=ord('0')) and (ord(c)<=ord('9')) then
    Result := ord(c)-ord('0')
  else
  if (ord(c)>=ord('A')) and (ord(c)<=ord('F')) then
    Result := 10 + ord(c)-ord('A')
  else
  if (ord(c)>=ord('a')) and (ord(c)<=ord('f')) then
    Result := 10 + ord(c)-ord('a')
  else
    raise Exception.Create('Invalid hex character "'+c+'"');
end;

//Converts up to maxlen hex characters into unicode symbols.
//Maxlen ought to be a multiplier of 4.
function HexToUnicode(ps:PAnsiChar; maxlen: integer): UnicodeString; overload;
var pn: PAnsiChar; //next symbol pointer
  cc: word; //character code
begin
  Result := '';
  if (ps=nil) or (ps^=#00) then exit;
 {$IFNDEF UNICODE}
  if ps^=UH_UNKNOWN_KANJI then Inc(ps);
 {$ENDIF}
  pn := ps;
  while (maxlen>=4) and EatOneFChar(pn) do begin
    cc := HexCharCode(ps^) shl 12;
    Inc(ps);
    Inc(cc, HexCharCode(ps^) shl 8);
    Inc(ps);
    Inc(cc, HexCharCode(ps^) shl 4);
    Inc(ps);
    Inc(cc, HexCharCode(ps^));
    Result := Result + WideChar(cc);
    ps := pn;
  end;
end;

function HexToUnicodeW(ps:PWideChar; maxlen: integer): UnicodeString; overload;
var pn: PWideChar; //next symbol pointer
  cc: word; //character code
begin
  Result := '';
  if (ps=nil) or (ps^=#00) then exit;
 {$IFNDEF UNICODE}
  if ps^=WideChar(UH_UNKNOWN_KANJI) then Inc(ps);
 {$ENDIF}
  pn := ps;
  while (maxlen>=4) and EatOneFCharW(pn) do begin
    cc := HexCharCodeW(ps^) shl 12;
    Inc(ps);
    Inc(cc, HexCharCodeW(ps^) shl 8);
    Inc(ps);
    Inc(cc, HexCharCodeW(ps^) shl 4);
    Inc(ps);
    Inc(cc, HexCharCodeW(ps^));
    Result := Result + WideChar(cc);
    ps := pn;
  end;
end;

function HexToUnicode(const s:string): UnicodeString; overload;
begin
  if s='' then
    Result := ''
  else
   {$IFDEF UNICODE}
    Result := HexToUnicodeW(PWideChar(pointer(s)), Length(s));
   {$ELSE}
    Result := HexToUnicode(PAnsiChar(pointer(s)), Length(s));
   {$ENDIF}
end;

function ByteToHex(pb:PByte;sz:integer):string;
const HexChars: string = '0123456789ABCDEF';
var i:integer;
begin
  if pb=nil then begin
    Result := '';
    exit;
  end;

  SetLength(Result, sz*2);
  for i := 0 to sz - 1 do begin
    Result[i*2+0] := HexChars[PByte(pb)^ shr 4];
    Result[i*2+1] := HexChars[PByte(pb)^ and $0F];
    Inc(pb);
  end;
end;

{
This is NOT equiualent to ByteToHex(s, Length(s)*2).
We're translating text in CHARS which are two-byte. I.e.
  ByteToHex:    01 02 03 04 05 06 07 08 09 10...
  UnicodeToHex: 02 01 04 03 06 05 08 07 10 09...
}
function UnicodeToHex(const s:UnicodeString):string;
const HexChars: string = '0123456789ABCDEF';
var i:integer;
  pc: PWideChar;
begin
  pc := PWideChar(s);
  if pc=nil then begin
    Result := '';
    exit;
  end;

  SetLength(Result, Length(s)*4);
  for i := 0 to Length(s) - 1 do begin
    Result[i*4+1] := HexChars[PWord(pc)^ shr 12];
    Result[i*4+2] := HexChars[(PWord(pc)^ shr 8) and $0F];
    Result[i*4+3] := HexChars[(PWord(pc)^ shr 4) and $0F];
    Result[i*4+4] := HexChars[PWord(pc)^ and $0F];
    Inc(pc);
  end;
end;

function AnsiToHex(const s:AnsiString):string;
begin
  Result := ByteToHex(PByte(s), Length(s));
end;

function UnicodeToML(s:widestring):string;
var i:integer;
    c:widechar;
    d:word;
    s2,s3:string;
begin
  s2:='';
  for i:=1 to length(s) do
  begin
    c:=s[i];
    d:=word(c);
    s3:=format('&#%d,',[d]);
    s2:=s2+s3;
  end;
  result:=s2;
end;




procedure SplitAdd(sl:TStringList;s:string;cnt:integer);
var i:integer;
begin
  i:=0;
  while i<cnt do
  begin
    inc(i);
    if pos(',',s)>0 then
    begin
      sl.Add(copy(s,1,pos(',',s)-1));
      delete(s,1,pos(',',s));
    end else
    begin
      sl.Add(s);
      s:='';
    end;
  end;
end;

//Same but doesn't add it anywhere
function SplitStr(s: string; cnt: integer): TStringArray;
var i:integer;
begin
  SetLength(Result, cnt);
  i:=0;
  while i<cnt do
  begin
    if pos(',',s)>0 then
    begin
      Result[i] := copy(s,1,pos(',',s)-1);
      delete(s,1,pos(',',s));
    end else
    begin
      Result[i] := s;
      s:='';
    end;
    inc(i);
  end;
end;

procedure StrListAdd(sl: TStringList; sa: TStringArray);
var i: integer;
begin
  for i := 0 to Length(sa) - 1 do
    sl.Add(sa[i]);
end;


function remexcl(s:string):string;
begin
  if (length(s)>1) and (s[2]=ALTCH_EXCL) then delete(s,2,2);
  if (length(s)>1) and (s[1]=ALTCH_EXCL) then delete(s,1,2);
  if (length(s)>1) and (s[2]=UH_UNKNOWN_KANJI) then delete(s,2,1);
  if (length(s)>1) and (s[1]=UH_UNKNOWN_KANJI) then delete(s,1,1);
  if (length(s)>1) and (s[1]=ALTCH_TILDE) then delete(s,1,2);
  result:=s;
end;

function strip_fl(s:string):string;
var beg, en: integer;
begin
  beg := pos(UH_LBEG,s);
  en := pos(UH_LEND,s);
  while (beg>0) and (en>0) and (en>beg) do begin
    delete(s,beg,en-beg+1);
    beg := pos(UH_LBEG,s);
    en := pos(UH_LEND,s);
  end;
  result:=trim(s);
end;




type TIntTextInfo=record
        act:boolean;
        p:TPaintBox;
        x,y,fs:integer;
        s:string;
     end;

const MAX_INTTEXTINFO = 4000;

var KnownList:array[1..20000] of pointer;
    KnownListSize:integer;
    wgcur:integer;
    itt:array[1..MAX_INTTEXTINFO] of TIntTextInfo;
    curpbox:TPaintBox;
    colarr:TStringList;
    colval:TStringList;
    colsval:array[0..Color_Max] of TColor;
    colsarr:TStringList;
    STB_x1,STB_y1,STB_x2,STB_y2:integer;
    STB_canvas:TCanvas;

procedure BeginDrawReg(p:TPaintBox);
var i:integer;
begin
  for i:=1 to MAX_INTTEXTINFO do if itt[i].p=p then itt[i].act:=false;
  curpbox:=p;
end;

procedure EndDrawReg;
begin
  curpbox:=nil;
end;

function FindDrawReg(p:TPaintBox;x,y:integer;var cx,cy,cy2:integer):string;
var i,j:integer;
begin
  result:='';
  for i:=1 to MAX_INTTEXTINFO do if itt[i].p=p then
    if (x>=itt[i].x) and (y>=itt[i].y) and (x<=itt[i].x+itt[i].fs*(length(itt[i].s) div 4)) and
    (y<=itt[i].y+itt[i].fs) then
    begin
      j:=(x-itt[i].x) div itt[i].fs;
      cy:=itt[i].y;
      cy2:=itt[i].y+itt[i].fs;
      cx:=itt[i].x+itt[i].fs*j;
      result:=copy(itt[i].s,j*4+1,length(itt[i].s)-j*4);
      exit;
    end;
end;

procedure DrawStrokeOrder(canvas:TCanvas;x,y,w,h:integer;char:string;fontsize:integer;color:TColor);
var i,l,r,m:integer;
    xx,yy:byte;
    p:pchar;
begin
  if sobin=nil then exit;
  l:=0;
  r:=sodir.Count-1;
  while l<=r do
  begin
    m:=l+(r-l) div 2;
    if (copy(sodir[m],1,4)<char) then l:=m+1 else
    if (copy(sodir[m],1,4)>char) then r:=m-1 else break;
  end;
  if l>r then exit;
  i:=strtoint('0x'+copy(sodir[m],5,4));
  p:=sobin;
  p:=p+i*2;
  xx:=255;
  yy:=255;
  i:=0;
  SetBkMode(canvas.Handle,TRANSPARENT);
  canvas.Font.Color:=color;
  canvas.Font.Style:=[fsBold];
  while (xx<>0) or (yy<>0) do
  begin
    xx:=byte(p^);
    p:=p+1;
    yy:=byte(p^);
    p:=p+1;
    inc(i);
    if (xx<>0) and (yy<>0) then
    begin
      canvas.Font.Color:=clWindow;
      DrawUnicode(canvas,round(x+w*(xx/256))+1,round(y+h*(yy/256)),fontsize,UnicodeToHex(inttostr(i)),FontEnglish);
      DrawUnicode(canvas,round(x+w*(xx/256))-1,round(y+h*(yy/256)),fontsize,UnicodeToHex(inttostr(i)),FontEnglish);
      DrawUnicode(canvas,round(x+w*(xx/256)),round(y+h*(yy/256))+1,fontsize,UnicodeToHex(inttostr(i)),FontEnglish);
      DrawUnicode(canvas,round(x+w*(xx/256)),round(y+h*(yy/256))-1,fontsize,UnicodeToHex(inttostr(i)),FontEnglish);
      canvas.Font.Color:=color;
      DrawUnicode(canvas,round(x+w*(xx/256)),round(y+h*(yy/256)),fontsize,UnicodeToHex(inttostr(i)),FontEnglish);
    end;
  end;
  canvas.Font.Color:=clWindowText;
  canvas.Font.Style:=[];
end;





function DrawTone(c:TCanvas;x,y,fw:integer;s:FString;dodraw:boolean):string;
var tb:integer;
    flat:boolean;
    s2,s3,s4:string;
    sc:char;
    i:integer;
    tr:double;
    w:pwidechar;
    oldtextflags:longint;
    s5:string;
begin
  s2:='';
  tb:=1;
  i:=0;
  flat:=false;
  s5:='';
  c.Font.Height:=fw;
  while s<>'' do
  begin
    s4:=copy(s,1,4);
    s3:=s4;
    if pos('F03',s3)=1 then delete(s3,1,3) else s3:='XXXX';
    if s3<>'' then sc:=s3[1] else sc:='?';
    sc:=upcase(sc);
    if (sc>='0') and (sc<='5') then
    begin
      tr:=(i-tb)/2+tb-1;
      w:='';
      oldtextflags:=c.TextFlags;
      c.TextFlags:=0;
      if dodraw then case sc of
        '3':w:=pwidechar(HexToUnicode('02C7'));
        '1':w:=pwidechar(HexToUnicode('02C9'));
        '2':w:=pwidechar(HexToUnicode('02CA'));
        '4':w:=pwidechar(HexToUnicode('02CB'));
      end;
      s5:=s5+floattostrf(tr,ffNumber,7,2)+',';
      if w<>'' then TextOutW(c.Handle,round(tr*fw),y,w,1);
      c.TextFlags:=oldtextflags;
      flat:=false;
      tb:=i+1;
    end else
    begin
      s2:=s2+s4;
      inc(i);
    end;
    delete(s,1,length(s4));
  end;
//  showmessage(s5);
  result:=s2;
end;

procedure DrawUnicode(c:TCanvas;x,y,fs:integer;ch:FString;fontface:string);
var w:pwidechar;
    chn:string;
    i:integer;
begin
  if ch='' then exit;
  SetBkMode(c.Handle,TRANSPARENT);
  c.Font.Name:=fontface;
  c.Font.Height:=fs;
//    c.Font.Style:=[];
  chn:=DrawTone(c,x,y,fs,ch,false);
 {$IFDEF UNICODE}
  w := PWideChar(chn);
 {$ELSE}
  w := PWideChar(HexToUnicode(chn));
 {$ENDIF}
  if chn<>ch then
  begin
    c.Font.Name:=FontRadical;
    fs:=round(fs/8*7);
    c.Font.Height:=fs;
    y:=y+fs div 4;
  end;
  chn:=DrawTone(c,x,y-fs div 4,fs,ch,true);
  if curpbox<>nil then for i:=1 to MAX_INTTEXTINFO do if not itt[i].act then
  begin
    itt[i].act:=true;
    itt[i].p:=curpbox;
    itt[i].x:=x;
    itt[i].y:=y;
    itt[i].fs:=fs;
    itt[i].s:=chn;
    break;
  end;
  TextOutW(c.Handle,x,y,w,flength(chn));
end;

function repl(var s:string;sub,repl:string):string;
begin
  while pos(sub,s)>0 do
    s:=copy(s,1,pos(sub,s)-1)+repl+copy(s,pos(sub,s)+length(sub),length(s)-pos(sub,s)+1-length(sub));
  result:=s;
end;

function ConvertEdictEntry(s:string;var mark:string):string;
var postm,post2m:string;
    s2:string;
    inmarker:boolean;
    curm,marker:string;
    i:integer;
    markerd:boolean;
    oldm:string;
    mm:byte;
    insection:boolean;
begin
  s2:='';
  marker:='';
  markerd:=false;
  mark:='';
  insection:=false;
  for i:=1 to length(s) do
  begin
    if s[i]='/'then
    begin
      s2:=s2+', ';
      insection:=false;
    end
    else if s[i]=' 'then
    begin
      if markerd then markerd:=false else if inmarker then marker:=marker+' 'else s2:=s2+' ';
    end
    else if s[i]='('then inmarker:=true
    else if s[i]=')'then
    begin
      oldm:=marker;
      while length(marker)>0 do
      begin
        if pos(',',marker)>0 then curm:=copy(marker,1,pos(',',marker)-1) else curm:=marker;
        delete(marker,1,length(curm));
        if (length(marker)>0) and (marker[1]=',') then delete(marker,1,1);
        markerd:=true;
        if curm='abbr'then mm:=1 else
        if curm='arch'then mm:=2 else
        if curm='fam'then mm:=3 else
        if curm='fem'then mm:=4 else
        if curm='MA'then mm:=5 else
        if curm='male'then mm:=6 else
        if curm='m-sl'then mm:=7 else
        if curm='vulg'then mm:=8 else
        if curm='X'then mm:=9 else
        if curm='col'then mm:=10 else
        if curm='adj'then mm:=11 else
        if curm='adv'then mm:=12 else
        if curm='adj-na'then mm:=13 else
        if curm='adj-no'then mm:=14 else
        if curm='adj-pn'then mm:=15 else
        if curm='adj-s'then mm:=16 else
        if curm='adj-t'then mm:=17 else
        if curm='aux'then mm:=18 else
        if curm='aux-v'then mm:=19 else
        if curm='conj'then mm:=20 else
        if curm='exp'then mm:=21 else
        if curm='gikun'then mm:=22 else
        if curm='gram'then mm:=23 else
        if curm='id'then mm:=24 else
        if curm='int'then mm:=25 else
        if curm='n'then mm:=26 else
        if curm='n-adv'then mm:=27 else
        if curm='n-t'then mm:=28 else
        if curm='n-suf'then mm:=29 else
        if curm='neg'then mm:=30 else
        if curm='neg-v'then mm:=31 else
        if curm='pref'then mm:=32 else
        if curm='suf'then mm:=33 else
        if curm='v1'then mm:=34 else
        if curm='v5'then mm:=35 else
        if curm='v5u'then mm:=36 else
        if curm='v5k'then mm:=37 else
        if curm='v5g'then mm:=38 else
        if curm='v5s'then mm:=39 else
        if curm='v5t'then mm:=40 else
        if curm='v5n'then mm:=41 else
        if curm='v5b'then mm:=42 else
        if curm='v5m'then mm:=43 else
        if curm='v5r'then mm:=44 else
        if curm='v5k-s'then mm:=45 else
        if curm='v5z'then mm:=46 else
        if curm='v5aru'then mm:=47 else
        if curm='v5uru'then mm:=48 else
        if curm='vi'then mm:=49 else
        if curm='vs'then mm:=50 else
        if curm='vs-s'then mm:=51 else
        if curm='vk'then mm:=52 else
        if curm='vt'then mm:=53 else
        if curm='hon'then mm:=54 else
        if curm='hum'then mm:=55 else
        if curm='ik'then mm:=56 else
        if curm='iK'then mm:=57 else
        if curm='io'then mm:=58 else
        if curm='obs'then mm:=59 else
        if curm='obsc'then mm:=60 else
        if curm='ok'then mm:=61 else
        if curm='oK'then mm:=62 else
        if curm='pol'then mm:=63 else
        if curm='uk'then mm:=64 else
        if curm='uK'then mm:=65 else
        if curm='P'then mm:=66 else
        begin
          if (oldm='1') or (oldm='2') or (oldm='3') then insection:=true;
          s2:=s2+'('+oldm+')'; mm:=0; markerd:=false;
          marker:='';
        end;
        if insection then mm:=0;
        if mm<>0 then mark:=mark+chr(mm+32);
      end;
      marker:='';
      inmarker:=false;
    end else if inmarker then marker:=marker+s[i] else s2:=s2+s[i];
  end;
  if copy(s2,length(s2)-1,2)=', 'then delete(s2,length(s2)-1,2);
  s2:=trim(s2);
  result:=s2;
end;

function GetMarkEdict(mark:char):string;
begin
  result:='';
  case mark of
    #33:result:='abbr';
    #34:result:='arch';
    #35:result:='fam';
    #36:result:='fem';
    #37:result:='MA';
    #38:result:='male';
    #39:result:='m-sl';
    #40:result:='vulg';
    #41:result:='X';
    #42:result:='col';
    #43:result:='adj';
    #44:result:='adv';
    #45:result:='adj-na';
    #46:result:='adj-no';
    #47:result:='adj-pn';
    #48:result:='adj-s';
    #49:result:='adj-t';
    #50:result:='aux';
    #51:result:='aux-v';
    #52:result:='conj';
    #53:result:='exp';
    #54:result:='gikun';
    #55:result:='gram';
    #56:result:='id';
    #57:result:='int';
    #58:result:='n';
    #59:result:='n-adv';
    #60:result:='n-t';
    #61:result:='n-suf';
    #62:result:='neg';
    #63:result:='neg-v';
    #64:result:='pref';
    #65:result:='suf';
    #66:result:='v1';
    #67:result:='v5';
    #68:result:='v5u';
    #69:result:='v5k';
    #70:result:='v5g';
    #71:result:='v5s';
    #72:result:='v5t';
    #73:result:='v5n';
    #74:result:='v5b';
    #75:result:='v5m';
    #76:result:='v5r';
    #77:result:='v5k-s';
    #78:result:='v5z';
    #79:result:='v5aru';
    #80:result:='v5uru';
    #81:result:='vi';
    #82:result:='vs';
    #83:result:='vs-s';
    #84:result:='vk';
    #85:result:='vt';
    #86:result:='hon';
    #87:result:='hum';
    #88:result:='ik';
    #89:result:='iK';
    #90:result:='io';
    #91:result:='obs';
    #92:result:='obsc';
    #93:result:='ok';
    #94:result:='oK';
    #95:result:='pol';
    #96:result:='uk';
    #97:result:='uK';
    #98:result:='P';
  end;
end;

function GetMarkDescription(edictmark:String):string;
var i:integer;
    s:String;
begin
  for i:=0 to markersl.Count-1 do
  begin
    if (pos(edictmark+' ',markersl[i])=1) or
       (pos(edictmark+#9,markersl[i])=1) then
    begin
      s:=markersl[i];
      delete(s,1,length(edictmark));
      s:=trim(s);
      result:=s;
      exit;
    end;
  end;
  result:='';
end;

procedure WriteMarkTable;
var t:textfile;
    i:char;
    s:string;
begin
  assignfile(t,'marks.html');
  rewrite(t);
  writeln(t,'<table>');
  for i:=#33 to #98 do
  begin
    writeln(t,'  <tr>');
    s:=GetMarkAbbr(i);
    writeln(t,'    <td>'+copy(s,2,length(s)-1)+'</td>');
    writeln(t,'    <td>'+s[1]+'</td>');
    s:=GetMarkEdict(i);
    writeln(t,'    <td>'+s+'</td>');
    writeln(t,'    <td>'+GetMarkDescription(s)+'</td>');
    writeln(t,'  </tr>');
  end;
  closefile(t);
end;

function GetMarkAbbr(mark:char):string;
var mr:string;
begin
  mr:='1?';
  case mark of
    #33:mr:='1abbr';
    #34:mr:='1archaic';
    #35:mr:='1familiar';
    #36:mr:='1female';
    #37:mr:='1martial-arts';
    #38:mr:='1male';
    #39:mr:='1manga-slang';
    #40:mr:='1vulgar';
    #41:mr:='1rude';
    #42:mr:='1col';
    #43:mr:='gadj';
    #44:mr:='gadv';
    #45:mr:='gna-adj';
    #46:mr:='gno-adj';
    #47:mr:='gpren-adj';
    #48:mr:='gspec-adj';
    #49:mr:='gtaru-adj';
    #50:mr:='gaux';
    #51:mr:='gaux-v';
    #52:mr:='gconj';
    #53:mr:='gexpr';
    #54:mr:='ggikun';
    #55:mr:='ggram';
    #56:mr:='gid';
    #57:mr:='gint';
    #58:mr:='gn';
    #59:mr:='gn-adv';
    #60:mr:='gn-temp';
    #61:mr:='gn-suf';
    #62:mr:='gneg';
    #63:mr:='gneg-verb';
    #64:mr:='gpref';
    #65:mr:='gsuf';
    #66:mr:='gru-v';
    #67:mr:='gu-v';
    #68:mr:='gu-v';
    #69:mr:='gu-v';
    #70:mr:='gu-v';
    #71:mr:='gu-v';
    #72:mr:='gu-v';
    #73:mr:='gu-v';
    #74:mr:='gu-v';
    #75:mr:='gu-v';
    #76:mr:='gu-v';
    #77:mr:='gIku-v';
    #78:mr:='gzuru-v';
    #79:mr:='garu-v';
    #80:mr:='guru-v';
    #81:mr:='gintrans-verb';
    #82:mr:='gp-suru';
    #83:mr:='gsuru-v';
    #84:mr:='gkuru-v';
    #85:mr:='gtrans-verb';
    #86:mr:='shonor';
    #87:mr:='shum';
    #88:mr:='sirreg-kana';
    #89:mr:='sirreg-kanji';
    #90:mr:='sirreg-okurigana';
    #91:mr:='sobsolete';
    #92:mr:='sobscure';
    #93:mr:='soutdated-kana';
    #94:mr:='soutdated-kanji';
    #95:mr:='spolite';
    #96:mr:='skana';
    #97:mr:='skanji';
    #98:mr:='spop';
  end;
  result:=mr;
end;

function EnrichDictEntry(s,mark:string):string;
var s2:string;
    i:integer;
    s3:string;
    mar1,marg,mars:string;
begin
  s2:=s;
  mar1:=''; marg:=''; mars:='';
  for i:=1 to length(mark) do
  begin
    s3:=GetMarkAbbr(mark[i]);
    if s3[1]='s'then mars:=mars+' '+UH_LBEG+s3+UH_LEND;
    if s3[1]='g'then marg:=marg+UH_LBEG+s3+UH_LEND+' ';
    if s3[1]='1'then mar1:=mar1+UH_LBEG+s3+UH_LEND+' ';
  end;
  result:=marg+mar1+s2+mars;
end;

//TODO: Upgrade to Unicode
function ResolveCrom(s:string;posin,posout:integer;clean:boolean):string;
var s2,s3:string;
    cr:string;
    cl:integer;
    i:integer;
begin
  s:=uppercase(s);
  s2:='';
  while s<>'' do
  begin
    cl:=0;
    for i:=0 to (romac.count div 4)-1 do
    begin
      if pos(uppercase(romac[i*4+posin]),s)=1 then
      begin
        if length(romac[i*4+posin])>cl then
        begin
          cl:=length(romac[i*4+posin]);
          cr:=romac[i*4+posout];
        end;
      end;
    end;
    if cl>0 then s2:=s2+cr else
      if s[1]='-'then s2:=s2+UnicodeToHex('-') else
      if s[1]='_'then s2:=s2+UnicodeToHex('_') else
      if pos(UnicodeToHex('-'),s)=1 then s2:=s2+'-'else
      if pos(UnicodeToHex('_'),s)=1 then s2:=s2+'_'else
      if not clean then
      begin
        if posout>0 then s2:=s2+'?'else s2:=s2+'003F';
      end;
    if cl>0 then delete(s,1,cl) else if posin>0 then delete(s,1,1) else delete(s,1,4);
    if posin=0 then
    begin
      if (length(s)>3) and (pos('F03',s)=1) and (s[4]>='0') and (s[4]<='5') then
      begin
        s2:=s2+s[4];
        delete(s,1,4);
      end else s2:=s2+'0';
    end else
    begin
      if (length(s)>0) and (s[1]>='0') and (s[1]<='5') then
      begin
        if posout>0 then s2:=s2+s[1] else s2:=s2+'F03'+s[1];
        delete(s,1,1);
      end else
      begin
        if posout>0 then s2:=s2+'0'else s2:=s2+'F030';
      end;
    end;
  end;
  if posout>0 then result:=lowercase(s2) else result:=s2;
end;


{
KanaToRomaji().
This function here is a major bottleneck when translating,
so we're going to try and implement it reallly fast.
}

//Try to compare strings as integers, without any string routines
{$DEFINE INTEGER_HELL}

{$IFDEF INTEGER_HELL}
//Returns a pointer to an integer p+#c counting from 0
//This is pretty fast when inlined, basically the same as typing that inplace, so use without fear.
//You can even put c==0 and the code will be almost eliminated at compilation time. Delphi is smart!
function IntgOff(p: pointer; c: integer): PInteger; inline;
begin
  Result := PInteger(integer(p)+c*4);
end;
{$ENDIF}

//ps must have at least one 4-char symbol in it
function SingleKanaToRomaji(var ps: PFChar; romatype: integer): string;
{$IFDEF UNICODE}{$POINTERMATH ON}{$ENDIF}
var i:integer;
  r: PRomajiTranslationRule;
 {$IFNDEF UNICODE}
  pe: PFChar;
 {$ENDIF}
begin
 {$IFDEF UNICODE}
  if ps^=UH_HYPHEN then begin
    Inc(ps);
 {$ELSE}
 {$IFDEF INTEGER_HELL}
  if pinteger(ps)^=pinteger(@UH_HYPHEN)^ then begin
 {$ELSE}
  if FcharCmp(ps, UH_HYPHEN, 1) then begin
 {$ENDIF}
    Inc(ps, 4);
 {$ENDIF}
    Result := '-';
    exit;
  end;

 {$IFDEF UNICODE}
  if ps^=UH_LOWLINE then begin
    Inc(ps);
 {$ELSE}
 {$IFDEF INTEGER_HELL}
  if pinteger(ps)^=pinteger(@UH_LOWLINE)^ then begin
 {$ELSE}
  if FcharCmp(ps, UH_LOWLINE, 1) then begin
 {$ENDIF}
    Inc(ps, 4);
 {$ENDIF}
    Result := '_';
    exit;
  end;

 //first try 2 FChars
 //but we have to test that we have at least that much
 {$IFDEF UNICODE}
  if (ps^<>#00) and ((ps+1)^<>#00) then begin
 {$ELSE}
  pe := ps;
  Inc(pe, 4); //first symbol must be there
  if EatOneFChar(pe) then begin
 {$ENDIF}
    for i := 0 to roma_t.Count - 1 do begin
      r := roma_t[i];
     {$IFDEF UNICODE}
     //Compare two characters at once (see note below)
      if (PInteger(ps)^=PInteger(r.hiragana_ptr)^)
      or (PInteger(ps)^=PInteger(r.katakana_ptr)^) then begin
     {
     //Safer and slower version:
      if ((ps^=r.hiragana_ptr^) and ((ps+1)^=(r.hiragana_ptr+1)^))
      or ((ps^=r.katakana_ptr^) and ((ps+1)^=(r.katakana_ptr+1)^)) then
     }
     {$ELSE}
     {$IFDEF INTEGER_HELL}
      {
      Note on integer comparison optimization:
      We're not checking if roma_t[i].hiragana has one or two 4-chars.
      It's okay. If it has one, then roma_t[i].hiragana[5]==#00, and it wouldn't match
      to any 4-char hex combination.
      It also won't AV because the memory's dword aligned and hiragana[5] is accessible already.
      }
      if ((pinteger(ps)^=pinteger(r.hiragana_ptr)^)
      and (IntgOff(ps, 1)^=IntgOff(r.hiragana_ptr, 1)^))
      or ((pinteger(ps)^=pinteger(r.katakana_ptr)^)
      and (IntgOff(ps,1)^=IntgOff(r.katakana_ptr, 1)^)) then begin
     {$ELSE}
      if FcharCmp(ps, r.hiragana_ptr, 2)
      or FcharCmp(ps, r.katakana_ptr, 2) then begin
     {$ENDIF}
     {$ENDIF}
        case romatype of
          2: Result := r.english;
          3: Result := r.czech;
        else
          Result := r.japanese;
        end;
       {$IFDEF UNICODE}
        Inc(ps, 2);
       {$ELSE}
        ps := pe;
       {$ENDIF}
        exit;
      end;
    end;
  end;

 //this time 1 FChar only
  for i := 0 to roma_t.Count - 1 do begin
    r := roma_t[i];
   {$IFDEF UNICODE}
    if (ps^=r.hiragana_ptr^) or (ps^=r.katakana_ptr^) then begin
   {$ELSE}
   {$IFDEF INTEGER_HELL}
    if (pinteger(ps)^=pinteger(r.hiragana_ptr)^)
    or (pinteger(ps)^=pinteger(r.katakana_ptr)^) then begin
   {$ELSE}
    if FcharCmp(ps, r.hiragana_ptr, 1)
    or FcharCmp(ps, r.katakana_ptr, 1) then begin
   {$ENDIF}
   {$ENDIF}
      case romatype of
        2: Result := r.english;
        3: Result := r.czech;
      else
        Result := r.japanese;
      end;
     {$IFDEF UNICODE}
      Inc(ps);
     {$ELSE}
      Inc(ps, 4);
     {$ENDIF}
      exit;
    end;
  end;

 //Latin symbol
 {$IFDEF UNICODE}
  if PWord(ps)^ and $FF00 = 0 then begin
    Result := ps^;
    Inc(ps);
 {$ELSE}
  if (ps^='0') and (PChar(integer(ps)+1)^='0') then begin
    Result := HexToUnicode(ps, 4);
    Inc(ps, 4);
 {$ENDIF}
    exit;
  end;

 {$IFDEF UNICODE}
  Inc(ps);
 {$ELSE}
  Inc(ps, 4);
 {$ENDIF}
  Result := '?';
{$IFDEF UNICODE}{$POINTERMATH OFF}{$ENDIF}
end;

function KanaToRomaji(s:FString;romatype:integer;lang:char):string;
var fn:string;
  s2:string;
 {$IFDEF UNICODE}
  ps: PWideChar;
 {$ELSE}
  ps, pn: PAnsiChar;
 {$ENDIF}
begin
  if lang='j'then
  begin
    if Length(s)<=0 then begin
      Result := '';
      exit;
    end;
    s := Uppercase(s);
    s2 := '';
   {$IFDEF UNICODE}
    ps := PWideChar(s);
   {$ELSE}
    ps := PAnsiChar(s);
   {$ENDIF}

   {$IFDEF UNICODE}
    while ps^<>#00 do begin
   {$ELSE}
    pn := ps;
    while EatOneFChar(pn) do begin
   {$ENDIF}
      fn := SingleKanaToRomaji(ps, romatype); //also eats one or two symbols
      if (fn='O') and (length(s2)>0) then fn:=upcase(s2[length(s2)]); ///WTF?!!
      s2:=s2+fn;
     {$IFNDEF UNICODE}
      pn := ps; //because ps might have advanced further
     {$ENDIF}
    end;

   {THIS HERE doesn't make much of a difference for speed}
    repl(s2,'a-','aa');
    repl(s2,'i-','ii');
    repl(s2,'u-','uu');
    repl(s2,'e-','ee');
    repl(s2,'o-','oo');
    if romatype>2 then repl(s2,'ou','oo');
    if romatype>2 then repl(s2,'ei','ee');
    repl(s2,'Dk','kk');
    repl(s2,'Dg','gg');
    repl(s2,'Ds','ss');
    repl(s2,'Dz','zz');
    repl(s2,'Dt','tt');
    repl(s2,'Dd','dd');
    repl(s2,'Dn','nn');
    repl(s2,'Dh','hh');
    repl(s2,'Db','bb');
    repl(s2,'Dp','pp');
    repl(s2,'Dm','mm');
    repl(s2,'Dr','rr');
    repl(s2,'Dy','yy');
    repl(s2,'Dw','ww');
    repl(s2,'Dj','jj');
    repl(s2,'Dš','šš');
    repl(s2,'Dè','èè');
    repl(s2,'Df','ff');
    repl(s2,'Dc','cc');
    repl(s2,'''','');
    if romatype>1 then repl(s2,'nb','mb');
    if romatype>1 then repl(s2,'np','mp');
    if romatype>1 then repl(s2,'nm','mm');
    if romatype=3 then
    begin
      repl(s2,'aa','á');
      repl(s2,'ii','í');
      repl(s2,'uu','ú');
      repl(s2,'oo','ó');
      repl(s2,'ee','é');
    end;
  {/THIS HERE}
    if (length(s2)>0) and (s2[length(s2)]='''') then delete(s2,length(s2),1);
    result:=s2;
  end;
  if lang='c'then
  begin
    result:=ResolveCrom(s,0,romatype,false);
  end;
end;

function RomajiToKana(s:string;romatype:integer;clean:boolean;lang:char):string;
var sr,s2,s3,fn:string;
  kata:integer;
  l,i,j:integer;
begin
  if lang='j'then
  begin
    s2:=s;
    if romatype>1 then repl(s2,'mb','nb');
    if romatype>1 then repl(s2,'mp','np');
    if romatype>1 then repl(s2,'mm','nm');
    repl(s2,'+','x');
    repl(s2,'kk','Dk');
    repl(s2,'gg','Dg');
    repl(s2,'ss','Ds');
    repl(s2,'zz','Dz');
    repl(s2,'tt','Dt');
    repl(s2,'dd','Dd');
  //  repl(s2,'nn','Dn');
    repl(s2,'hh','Dh');
    repl(s2,'bb','Db');
    repl(s2,'pp','Dp');
    repl(s2,'mm','Dm');
    repl(s2,'rr','Dr');
    repl(s2,'yy','Dy');
    repl(s2,'ww','Dw');
    repl(s2,'jj','Dj');
    repl(s2,'šš','Dš');
    repl(s2,'èè','Dè');
    repl(s2,'ff','Df');
    repl(s2,'cc','Dc');
    if romatype=3 then
    begin
      repl(s2,'á','aa');
      repl(s2,'í','ii');
      repl(s2,'ó','oo');
      repl(s2,'ú','uu');
      repl(s2,'ù','uu');
      repl(s2,'é','ee');
    end;
    kata:=0;
    s3:='';
  //  if romatype>1 then repl(s2,'oo','ou');
  //  if romatype>1 then repl(s2,'ee','ei');
    if (length(s2)>0) and (s2[1]<>'Q') then
    begin
      if (s2[1]='K') then
      begin
        repl(s2,'aa','a-');
        repl(s2,'ii','i-');
        repl(s2,'uu','u-');
        repl(s2,'ee','e-');
        repl(s2,'oo','o-');
        repl(s2,'yaA','ya-');
        repl(s2,'yuU','yu-');
        repl(s2,'yoO','yo-');
      end;
    end;
    while length(s2)>0 do
    begin
      fn:='';
     {$IFDEF UNICODE}
      if s2[1]='_'then fn:='_';
      if s2[1]='-'then fn:='-';
     {$ELSE}
      if s2[1]='_'then fn:=UnicodeToHex('_');
      if s2[1]='-'then fn:=UnicodeToHex('-');
     {$ENDIF}
      for i:=0 to roma_t.Count-1 do
      begin
        case romatype of
          2: sr := roma_t[i].english;
          3: sr := roma_t[i].czech;
        else
          sr := roma_t[i].japanese;
        end;
        if pos(sr,s2)=1 then
        begin
          l:=length(sr);
          if kata=0 then
            fn := roma_t[i].hiragana
          else
            fn := roma_t[i].katakana;
          break;
        end else
        if (romatype>0) and (pos(roma_t[i].english,s2)=1) then
        begin
          l:=length(roma_t[i].japanese);
          if kata=0 then
            fn := roma_t[i].hiragana
          else
            fn := roma_t[i].katakana;
          break;
        end;
      end;

     //If we haven't found the match, try other romaji types
      if fn='' then
      for i:=0 to roma_t.Count-1 do
        if pos(roma_t[i].japanese,s2)=1 then
        begin
          l:=length(roma_t[i].japanese);
          if kata=0 then
            fn := roma_t[i].hiragana
          else
            fn := roma_t[i].katakana;
          break;
        end else
        if pos(roma_t[i].english,s2)=1 then
        begin
          l:=length(roma_t[i].english);
          if kata=0 then
            fn := roma_t[i].hiragana
          else
            fn := roma_t[i].katakana;
          break;
        end else
        if pos(roma_t[i].czech,s2)=1 then
        begin
          l:=length(roma_t[i].czech);
          if kata=0 then
            fn := roma_t[i].hiragana
          else
            fn := roma_t[i].katakana;
          break;
        end;

      if fn='' then
      begin
        if not clean then
          if s2[1]<>'''' then
           //Latin letter (supposedly)
           {$IFDEF UNICODE}
            fn := s2[1]
           {$ELSE}
            fn:=Format('00%2.2X',[ord(s2[1])])
           {$ENDIF}
          else
            fn:='';
        l:=1;
      end;
      if s2[1]='H'then
      begin
        kata:=0;
        l:=1;
        fn:='';
      end;
      if (s2[1]='K') or (s2[1]='Q') then
      begin
        kata:=1;
        l:=1;
        fn:='';
      end;
      delete(s2,1,l);
      s3:=s3+fn;
    end;
    result:=s3;
  end;
  if lang='c'then
  begin
    repl(s,'v','u:');
    result:=ResolveCrom(s,romatype,0,clean);
  end;
end;

procedure FreeRomaList;
begin
  romac.Free;
  roma_t.Free;
end;

procedure InitWordGrid(grid:TStringGrid;stat,learn:boolean);
begin
  grid.Hide;
  grid.DefaultRowHeight:=GridFontSize+2;
  grid.FixedRows:=1;
  grid.Cells[0,0]:=_l('#00939^ePhonetic^cÈtení');
  grid.Cells[1,0]:=_l('#00632^eWritten^cZápis');
  grid.Cells[2,0]:=_l('#00317^eTranslation^cPøeklad');
  if stat then if learn then
    grid.Cells[3,0]:=_l('#00633^eAdded / Learned^cPøidáno / Nauèeno') else
    grid.Cells[3,0]:=_l('#00634^eCategories^cKategorie');
  wgcur:=1;
end;

procedure AddWordGrid(var grid:TStringGrid;sp1,sp2,sp4,sp3:string);
begin
  grid.Cells[0,wgcur]:=ALTCH_SHARP+sp2;
  grid.Cells[1,wgcur]:=ALTCH_AT+sp1;
  grid.Cells[2,wgcur]:=sp4;
  if sp3<>'' then grid.Cells[3,wgcur]:=sp3;
  inc(wgcur);
end;

procedure FinishWordGrid(grid:TStringGrid);
begin
  if wgcur=1 then grid.Hide else
  begin
    grid.RowCount:=wgcur;
    grid.Show;
  end;
end;

procedure SplitWord(s:string; var sp1,sp2,sp4,sp3:string);
begin
  sp2:='';
  sp1:='';
  sp3:='';
  sp1:='';
  while s[1]<>' 'do
  begin
    sp1:=sp1+s[1];
    delete(s,1,1);
  end;
  delete(s,1,1);
  if s[1]='['then
  begin
    delete(s,1,1);
    while s[1]<>']'do
    begin
      sp2:=sp2+s[1];
      delete(s,1,1);
    end;
    delete(s,1,2);
  end else sp2:=sp1;
  delete(s,1,1);
  sp3:=s;
  sp4:=copy(s,1,pos('}',s)-1);
  delete(sp3,1,length(sp4)+1);
end;

procedure FillWordGrid(grid:TStringGrid;sl:TStringList;stat,learn:boolean);
var i:integer;
    s,sp1,sp2,sp3,sp4:string;
begin
  if sl.Count=0 then
  begin
    grid.Hide;
    exit;
  end;
  InitWordGrid(grid,stat,learn);
  for i:=0 to sl.Count-1 do
  begin
    s:=sl[i];
    SplitWord(s,sp1,sp2,sp4,sp3);
    AddWordGrid(grid,sp1,sp2,sp4,sp3);
  end;
  if fSettings.CheckBox53.Checked then
    for i:=1 to grid.RowCount-1 do
      grid.RowHeights[i]:=(GridFontSize+2)*DrawWordInfo(grid.Canvas,grid.CellRect(2,i),false,false,2,grid.Cells[2,i],true,true,GridFontSize,true);
  FinishWordGrid(grid);
end;

function DrawWordInfo(canvas:TCanvas; Rect:TRect; sel,titrow:boolean; colx:integer; s:string; multiline,onlycount:boolean; fontsize:integer; boldfont:boolean):integer;
var x:integer;
    inmar,resinmar:boolean;
    curs:string;
    rect2:TRect;
    c:char;
    cursiv:boolean;
    w:integer;
    y:integer;
    cnt:integer;
    sbef:string;
    fontcolor:TColor;
begin
  if multiline then result:=1 else result:=0;
  Canvas.Brush.Color:=clWindow;
  Canvas.Font.Color:=clWindowText;
  Canvas.Font.Name:=FontEnglish;
  Canvas.Font.Style:=[];
  Canvas.Font.Size:=9;
  if (fSettings.CheckBox11.Checked) and (not fSettings.CheckBox9.Checked) and (not titrow) then
  begin
    c:=' ';
    if (length(s)>1) and (s[1]=ALTCH_EXCL) then c:=s[2];
    if (length(s)>2) and (s[2]=ALTCH_EXCL) then c:=s[3];
    case c of
      ' ':if sel then Canvas.Brush.Color:=Col('Dict_SelBack') else Canvas.Brush.Color:=Col('Dict_Back');
      '0':if sel then Canvas.Brush.Color:=Col('Dict_SelProblematic') else Canvas.Brush.Color:=Col('Dict_Problematic');
      '1':if sel then Canvas.Brush.Color:=Col('Dict_SelUnlearned') else Canvas.Brush.Color:=Col('Dict_Unlearned');
      '2':if sel then Canvas.Brush.Color:=Col('Dict_SelLearned') else Canvas.Brush.Color:=Col('Dict_Learned');
      '3':if sel then Canvas.Brush.Color:=Col('Dict_SelMastered') else Canvas.Brush.Color:=Col('Dict_Mastered');
    end;
  end;
  if (length(s)>1) and (s[1]=ALTCH_EXCL) then delete(s,1,2);
  if (length(s)>2) and (s[2]=ALTCH_EXCL) then delete(s,2,2);
  if (length(s)>0) and (Colx=0) and (s[1]=ALTCH_SHARP) then
  begin
    Canvas.FillRect(Rect);
    delete(s,1,1);
//    if showroma then
//      Grid.Canvas.TextRect(Rect,Rect.Left+2,Rect.Top+2,KanaToRomaji(s,romasys,curlang)) else
//    DrawUnicode(Grid.Canvas,Rect.Left+2,Rect.Top+2,12,s,FontSmall);
    DrawKana(Canvas,Rect.Left+2,Rect.Top+1,FontSize,s,FontSmall,showroma,romasys,curlang);
  end else
  if (length(s)>0) and (s[1]=ALTCH_AT) then
  begin
    Canvas.FillRect(Rect);
    delete(s,1,1);
    if (s[1]=UH_UNKNOWN_KANJI) then
    begin
      if (fSettings.CheckBox10.Checked) then Canvas.Font.COlor:=Col('Dict_UnknownChar') else Canvas.Font.Color:=Col('Dict_Text');
      delete(s,1,1);
    end
    else Canvas.Font.Color:=Col('Dict_Text');
    if fSettings.CheckBox9.Checked then Canvas.Font.Color:=clWindowText;
    DrawUnicode(Canvas,Rect.Left+2,Rect.Top+1,FontSize,s,FontSmall);
  end else if not titrow then
  begin
    cursiv:=false;
    FontColor:=Col('Dict_Text');
    if fSettings.CheckBox9.Checked then FontColor:=clWindowText;
    if (length(s)>1) and (s[1]=ALTCH_TILDE) then
    begin
      if s[2]='I'then cursiv:=true;
//      if not fUser.CheckBox1.Checked then cursiv:=false;
      delete(s,1,2);
    end;
    if (length(s)>1) and (s[1]=UH_SETCOLOR) then
    begin
      if (fSettings.CheckBox69.Checked) then
        if not TryStrToInt('0x'+copy(s,6,2)+copy(s,4,2)+copy(s,2,2), integer(FontColor)) then
          FontColor:=clWindowText;
      delete(s,1,7);
    end;
    if not onlycount then Canvas.FillRect(Rect);
    inmar:=false;
    x:=0;
    y:=0;
    cnt:=0;
    sbef:='';
    while length(s)>0 do
    begin
//      if sbef=s then
//      begin
//        showmessage(sbef);
//      end;
      sbef:=s;
      inc(cnt);
      if inmar then
        if pos(UH_LEND,s)>0 then curs:=copy(s,1,pos(UH_LEND,s)-1) else curs:=s;
      if not inmar then
        if pos(UH_LBEG,s)>0 then curs:=copy(s,1,pos(UH_LBEG,s)-1) else curs:=s;
      delete(s,1,length(curs));
      if (length(s)>0) and ((s[1]=UH_LBEG) or (s[1]=UH_LEND)) then delete(s,1,1);
      rect2:=rect;
      rect2.Left:=rect.left+x+2;
      rect2.Top:=rect.top+y;
      if x<rect.right-rect.left then
      begin
        if inmar then
        begin
          c:=curs[1];
          delete(curs,1,1);
          if fSettings.CheckBox9.Checked then Canvas.Font.Color:=FontColor;
          if not fSettings.CheckBox9.Checked then case c of
            '1':Canvas.Font.Color:=Col('Mark_Special');
            's':Canvas.Font.Color:=Col('Mark_Usage');
            'g':Canvas.Font.Color:=Col('Mark_Grammatical');
            'd':Canvas.Font.Color:=Col('Mark_Dict');
            'l':Canvas.Font.Color:=Col('Mark_Lesson');
          end;
          Canvas.Font.Height:=FontSize-3;
          Canvas.Font.Style:=[fsItalic];
        end else
        begin
          Canvas.Font.Color:=FontColor;
          Canvas.Font.Height:=FontSize;
          Canvas.Font.Style:=[];
          if boldfont then
            if cursiv then Canvas.Font.Style:=[fsItalic,fsBold] else
              Canvas.Font.Style:=[fsBold];
          if Colx=3 then Canvas.Font.Style:=[];
        end;
        w:=Canvas.TextExtent(curs).cx;
        if not multiline then result:=result+w;
        resinmar:=false;
        if (multiline) and (rect.left+2+x+w>rect.right) then
        begin
          if (length(curs)>0) and (curs[1]=' ') then curs[1]:=ALTCH_TILDE;
          if inmar or (pos(' ',curs)=0) or (Canvas.TextExtent(copy(curs,1,pos(' ',curs)-1)).cx+rect.left+2+x>rect.right) then
          begin
            if (length(curs)>0) and (curs[1]=ALTCH_TILDE) then curs[1]:=' ';
            x:=0;
            y:=y+FontSize+2;
            rect2.left:=rect.left+2;
            rect2.top:=rect.top+2+y;
            result:=result+1;
          end else
          if not inmar and (pos(' ',curs)>0) then
          begin
            if (length(curs)>0) and (curs[1]=' ') then curs[1]:=ALTCH_TILDE;
            s:=copy(curs,pos(' ',curs),length(curs)-pos(' ',curs)+1)+UH_LBEG+s;
            curs:=copy(curs,1,pos(' ',curs)-1);
            if (length(curs)>0) and (curs[1]=ALTCH_TILDE) then curs[1]:=' ';
            resinmar:=true;
          end else
          begin
            curs:=s;
            if (length(curs)>0) and (curs[1]=ALTCH_TILDE) then curs[1]:=' ';
            s:='';
          end;
        end;
        if not onlycount then
        begin
          if inmar then
            Canvas.TextRect(Rect2,Rect.Left+2+x,Rect.Top+5+y,curs) else
            Canvas.TextRect(Rect2,Rect.Left+2+x,Rect.Top+1+y,curs);
        end;
        x:=x+Canvas.TextExtent(curs).cx;
        if not resinmar then inmar:=not inmar;
      end else s:='';
    end;
  end else
  begin
    Canvas.Font.Style:=[fsBold];
    Canvas.Font.Size:=8;
    Canvas.FillRect(Rect);
    Canvas.TextRect(Rect,Rect.Left+2,Rect.Top+2,s);
  end;
  Canvas.Font.Color:=clWindowText;
  Canvas.Font.Name:=FontEnglish;
  Canvas.Font.Style:=[];
  Canvas.Font.Size:=9;
end;

procedure DrawPackedWordInfo(canvas: TCanvas; Rect:TRect; s:string; ch:integer;boldfont:boolean);
var s1,sx1,s2,s3,s4:string;
begin
  SplitWord(s,s1,s2,s3,s4);
  if curlang='c'then
  begin
    if s2[1]=ALTCH_EXCL then delete(s2,1,2);
    s2:=KanaToRomaji(s2,romasys,curlang);
    s2:=ConvertPinYin(s2);
    sx1:=s1;
    s1:=s1+UH_SPACE+s2;
    DrawWordInfo(Canvas,rect,false,false,0,ALTCH_AT+s1,false,false,ch-3,boldfont);
    rect.left:=rect.left+(length(sx1) div 4)*ch+ch+(length(s2) div 8)*ch;
  end else
  begin
    DrawWordInfo(Canvas,rect,false,false,0,ALTCH_AT+s1,false,false,ch-3,boldfont);
    rect.left:=rect.left+(length(s1) div 4)*ch;
  end;
  if (s2<>s1) and (curlang='j') then
  begin
    if s2[1]=ALTCH_EXCL then s2:=ALTCH_EXCL+s1[2]+UH_UNKNOWN_KANJI+copy(s2,3,length(s2)-2) else s2:=UH_UNKNOWN_KANJI+s2;
    DrawWordInfo(Canvas,rect,false,false,1,ALTCH_AT+s2,false,false,ch-3,boldfont);
    rect.left:=rect.left+(length(s2) div 4)*ch;
  end;
  DrawWordInfo(Canvas,rect,false,false,2,s3,false,false,ch-3,boldfont);
end;

procedure DrawWordCell(Grid:TStringGrid; ACol, ARow: Integer; Rect: TRect; State: TGridDrawState);
var s:string;
    gr:integer;
    rect2:TRect;
begin
  s:=Grid.Cells[ACol,ARow];
  rect2:=rect;
  rect2.bottom:=1000;
  if (fSettings.CheckBox53.Checked) and (ACol=2) and (ARow>0) then
  begin
    gr:=(2+GridFontSize)*DrawWordInfo(Grid.Canvas, Rect2, gdSelected in State, ARow=0, ACol, s, true, true, GridFontSize,true);
    if grid.rowheights[arow]<>gr then begin grid.rowheights[arow]:=gr; exit; end;
  end;
  DrawWordInfo(Grid.Canvas, Rect, gdSelected in State, ARow=0, ACol, s, true, false, GridFontSize,true);
  PaintScreenTipBlock;
end;

procedure ClearKanjiCardCache;
begin
  kcchind.Clear;
  kcchcomp.Clear;
end;

procedure DrawKanjiCard(canvas:TCanvas;u:string;x,y:integer;ch:double;
  stcount,outlin,alt,rad,inlin,comp,read,mean,strokeorder,fullcomp,sortfreq:boolean;
  sizhor,sizvert,nofullcomp:integer;calfont:string);
var ony,kuny,defy:string;
    radf:integer;
    sl:TStringList;
    s,ws:string;
    p:integer;
    i,j,k:integer;
    rect:TRect;
    nch,ncv:integer;
    fontjpch:string;
    FontJpChGrid:string;
    FontJpEnGrid:string;
    adddot:integer;
    dic:TJaletDic;
    mark,freq:string;
begin
  if curlang='j'then fontjpch:=FontJapanese else fontjpch:=FontChinese;
  if curlang='j'then fontjpchgrid:=FontJapaneseGrid else fontjpchgrid:=FontChineseGrid;
  if curlang='j'then fontjpengrid:=FontJapaneseGrid else fontjpengrid:=FontPinYin;
  ncv:=sizvert;
  if read then inc(ncv,3);
  if mean then inc(ncv,2);
  if fullcomp then inc(ncv,1+nofullcomp);
  nch:=sizvert;
  if alt or rad then inc(nch,(sizvert div 2)+1);
  if comp then nch:=nch+1+sizhor;
  sl:=TStringList.Create;
  TChar.Locate('Unicode',u,false);
  DrawUnicode(canvas,trunc(x+ch/2),trunc(y+ch/2),trunc((sizvert-1)*ch),u,calfont);
  if stcount then
    if curlang<>'j'then DrawUnicode(canvas,trunc(x+ch/2),trunc(y+ch/2),trunc(ch),UnicodeToHex(TChar.Str(TCharStrokeCount)),FontEnglish);
  if stcount then
    if curlang='j'then DrawUnicode(canvas,trunc(x+ch/2),trunc(y+ch/2),trunc(ch),UnicodeToHex(TChar.Str(TCharJpStrokeCount)),FontEnglish);
  if strokeorder then
    DrawStrokeOrder(canvas,trunc(x+ch/2),trunc(y+ch/2),trunc((sizvert-1)*ch),trunc((sizvert-1)*ch),u,trunc(ch/3*2),clBlack);
  {outer lines}
  if outlin then
  begin
    canvas.MoveTo(x,y);
    canvas.LineTo(trunc(x+nch*ch),y);
    canvas.LineTo(trunc(x+nch*ch),trunc(y+ncv*ch));
    canvas.LineTo(x,trunc(y+ncv*ch));
    canvas.LineTo(x,y);
  end;
  {alternate}
  radf:=fSettings.ComboBox1.ItemIndex+12;
  TRadicals.Locate('Number',inttostr(fMenu.GetCharValueRad(TChar.Int(TCharIndex),radf)),true);
  if alt then
    DrawUnicode(canvas,trunc(x+ch/2+(sizvert)*ch*17/16),trunc(y+ch/2),trunc(sizvert/8*3*ch),TRadicals.Str(TRadicalsUnicode),FontRadical);
  {radical}
  if rad then
    DrawUnicode(canvas,trunc(x+ch/2+(sizvert)*ch*17/16),trunc(y+ch/2+(sizvert/2)*ch),trunc((sizvert/8*3)*ch),u,FontJpchGrid);
  if inlin then
  begin
    canvas.MoveTo(trunc(x+ch*sizvert),trunc(y+ch/2));
    canvas.LineTo(trunc(x+ch*sizvert),trunc(y+ch*sizvert-ch/2));
  end;
  {compounds}
  sl.Clear;
  if comp then
  begin
    TUserIdx.SetOrder('Kanji_Ind');
    TUserIdx.Locate('Kanji',u,false);
    while (not TUserIdx.EOF) and (TUserIdx.Str(TUserIdxKanji)=u) do
    begin
      TUser.Locate('Index',TUserIdx.Str(TUserIdxWord),true);
      if length(TUser.Str(TUserKanji)) div 4<10 then
//      if FirstUnknownKanjiIndex(TUser.Str(TUserKanji))<0 then
        if TUserIdx.Bool(TUserIdxBegin) then
          sl.Add('+'+inttostr(length(TUser.Str(TUserKanji)) div 4)+TUser.Str(TUserKanji)) else
          sl.Add('-'+inttostr(length(TUser.Str(TUserKanji)) div 4)+TUser.Str(TUserKanji));
      TUserIdx.Next;
    end;
    sl.Sort;
    p:=0;
    for j:=0 to sizvert-2 do
    begin
      s:='';
      while (p<sl.Count) and ((length(sl[p])-2) div 4+length(s) div 4+1<=sizhor) do
      begin
        s:=s+copy(sl[p],3,length(sl[p])-2)+UH_IDG_COMMA;
        inc(p);
      end;
      if (p>=sl.Count) and (length(s)>3) then delete(s,length(s)-3,4);
      if alt or rad then
        DrawUnicode(canvas,trunc(x+((sizvert)*3*ch)/2+ch+ch/2),trunc(y+ch/2+j*ch),trunc(ch),s,FontJpChGrid) else
        DrawUnicode(canvas,trunc(x+(sizvert)*ch+ch+ch/2),trunc(y+ch/2+j*ch),trunc(ch),s,FontJpChGrid);
    end;
    if (alt or rad) and (inlin) then
    begin
      canvas.MoveTo(trunc(x+ch*(((sizvert*3)/2)+1)),trunc(y+ch/2));
      canvas.LineTo(trunc(x+ch*(((sizvert*3)/2)+1)),trunc(y+ch*sizvert-ch/2));
    end;
  end;
  {full compounds}
  sl.Clear;
  if fullcomp then
  begin
    if kcchind.IndexOf(u)=-1 then
    begin
      for i:=0 to dicts.Count-1 do if ((dicts.Objects[i] as TJaletDic).loaded) and (pos(','+(dicts.Objects[i] as TJaletDic).name,NotGroupDicts[4])=0)
        and ((dicts.Objects[i] as TJaletDic).TDictFrequency<>-1) then
      begin
        dic:=dicts.Objects[i] as TJaletDic;
        dic.Demand;
        dic.FindIndexString(false,u);
        k:=0;
        j:=dic.ReadIndex;
        while (j>0) do
        begin
          dic.TDict.Locate('Index',inttostr(j),true);
          inc(k);
          if dic.TDictMarkers<>-1 then mark:=dic.TDict.Str(dic.TDictMarkers) else mark:='';
          freq:='0000000';
          if (dic.TDictFrequency<>-1) and (sortfreq) then freq:=inttostr(9999999-dic.TDict.Int(dic.TDictFrequency));
          while length(freq)<7 do freq:='0'+freq;
          if pos(UH_LBEG+'spop'+UH_LEND,EnrichDictEntry(dic.TDict.Str(dic.TDictEnglish),mark))=0 then freq[1]:='a';
          if freq<>'9999999'then
          sl.Add(freq+#9+ChinTo(dic.TDict.Str(dic.TDictKanji))+' ['+dic.TDict.Str(dic.TDictPhonetic)+'] {'+EnrichDictEntry(dic.TDict.Str(dic.TDictEnglish),mark)+'}{');
          j:=dic.ReadIndex;
        end;
      end;
      sl.Sort;
      for j:=0 to nofullcomp-1 do if sl.Count>j then kcchcomp.Add(sl[j]) else kcchcomp.Add('');
      kcchind.Add(u);
    end;
    p:=0;
    i:=kcchind.IndexOf(u);
    for j:=0 to nofullcomp-1 do
    begin
      s:=kcchcomp[i*nofullcomp+j];
      rect.left:=x+round(ch/2);
      rect.right:=x+round(nch*ch-ch/2);
      rect.top:=y+round(sizvert*ch+j*ch+ch/2);
      rect.bottom:=y+round(sizvert*ch+j*ch+ch+ch/2);
      if read then
      begin
        rect.top:=rect.top+trunc(ch*3);
        rect.bottom:=rect.bottom+trunc(ch*3);
      end;
      if mean then
      begin
        rect.top:=rect.top+trunc(ch*2);
        rect.bottom:=rect.bottom+trunc(ch*2);
      end;
      if s<>'' then DrawPackedWordInfo(canvas,rect,copy(s,9,length(s)-8),trunc(ch),false);
    end;
    if inlin then
    begin
      rect.top:=y+round(sizvert*ch);
      if read then
        rect.top:=rect.top+trunc(ch*3);
      if mean then
        rect.top:=rect.top+trunc(ch*2);
      canvas.MoveTo(trunc(x+ch/2),rect.top);
      canvas.LineTo(trunc(x+nch*ch-ch/2),rect.top);
    end;
  end;
  {readings}
  if read then
  begin
    if inlin then
    begin
      canvas.MoveTo(trunc(x+ch/2),trunc(y+ch*sizvert));
      canvas.LineTo(trunc(x+nch*ch-ch/2),trunc(y+ch*sizvert));
    end;
    ony:='';
    kuny:='';
    TCharRead.SetOrder('');
    TCharRead.Locate('Kanji',TChar.Str(TCharIndex),true);
    while (not TCharRead.EOF) and (TCharRead.Int(TCharReadKanji)=TChar.Int(TCharIndex)) do
    begin
      s:=TCharRead.Str(TCharReadReading);
      if curlang='j'then
        if (TCharRead.Int(TCharReadType)>3) and (TCharRead.Int(TCharReadType)<7) then
        begin
          ws:='';
          adddot:=0;
          if s[1]='+'then
          begin
            ws:={$IFNDEF UNICODE}'FF0B'{$ELSE}#$FF0B{$ENDIF};
            delete(s,1,1);
            adddot:=1;
          end;
          if s[1]='-'then
          begin
            ws:=ws+{$IFNDEF UNICODE}'FF0D'{$ELSE}#$FF0D{$ENDIF};
            delete(s,1,1);
            adddot:=1;
          end;
          if TCharRead.Int(TCharReadReadDot)>0 then
          begin
            ws:=ws+copy(s,1,TCharRead.Int(TCharReadReadDot)-1-adddot);
            ws:=ws+{$IFNDEF UNICODE}'FF0E'{$ELSE}#$FF0E{$ENDIF};
            delete(s,1,TCharRead.Int(TCharReadReadDot)-1-adddot);
          end;
          if s[length(s)]='-'then ws:=ws+copy(s,1,length(s)-1)+{$IFNDEF UNICODE}'FF0D'{$ELSE}#$FF0D{$ENDIF}
            else ws:=ws+s;
        end;
      if curlang='c'then ws:=TCharRead.Str(TCharReadReading);
      case TCharRead.Int(TCharReadType) of
        2:if curlang='c'then if ony='' then ony:=ConvertPinYin(ws) else ony:=ony+UnicodeToHex(', ')+ConvertPinYin(ws);
        8:if curlang='c'then if kuny='' then kuny:=UnicodeToHex(lowercase(ws)) else kuny:=kuny+UnicodeToHex(', ')+UnicodeToHex(lowercase(ws));
        4:if curlang='j'then if length(ony) div 4+length(ws) div 4+2<=nch then if ony='' then ony:=ws else ony:=ony+UH_IDG_COMMA+ws;
        5:if curlang='j'then if length(kuny) div 4+length(ws) div 4+2<=nch then if kuny='' then kuny:=ws else kuny:=kuny+UH_IDG_COMMA+ws;
      end;
      TCharRead.Next;
    end;
//        ony:=UnicodeToHex(KanaToRomaji(ony,3));
//        kuny:=UnicodeToHex(KanaToRomaji(ony,3));
    DrawUnicode(canvas,trunc(x+ch/2),trunc(y+sizvert*ch+ch/2),trunc(ch),ony,FontJpEnGrid);
    DrawUnicode(canvas,trunc(x+ch/2),trunc(y+sizvert*ch+ch/2+ch),trunc(ch),kuny,FontJpEnGrid);
  end;
  if mean then
  begin
    if inlin then if not read then
    begin
      canvas.MoveTo(trunc(x+ch/2),trunc(y+ch*sizvert));
      canvas.LineTo(trunc(x+nch*ch-ch/2),trunc(y+ch*sizvert));
    end else
    begin
      canvas.MoveTo(trunc(x+ch/2),trunc(y+ch*(3+sizvert)));
      canvas.LineTo(trunc(x+nch*ch-ch/2),trunc(y+ch*(3+sizvert)));
    end;
    rect.left:=trunc(x+ch/2);
    rect.right:=trunc(x+nch*ch-ch/2);
    rect.top:=trunc(y+ch*(sizvert)+ch/2);
    rect.bottom:=trunc(y+ch*(1+sizvert)+ch/2);
    if read then
    begin
      rect.top:=rect.top+trunc(ch*3);
      rect.bottom:=rect.bottom+trunc(ch*3);
    end;
    defy:='';
    TCharRead.SetOrder('');
    TCharRead.Locate('Kanji',TChar.Str(TCharIndex),true);
    while (not TCharRead.EOF) and (TCharRead.Int(TCharReadKanji)=TChar.Int(TCharIndex)) do
    begin
      s:=TCharRead.Str(TCharReadReading);
      if ((curlang='j') and (TCharRead.Int(TCharReadType)=3)) or ((curlang='c') and (TCharRead.Int(TCharReadType)=7)) then
      begin
        if defy='' then defy:=defy+s else defy:=defy+', '+s;
      end;
      TCharRead.Next;
    end;
    canvas.Font.Name:=FontEnglish;
    canvas.Font.Height:=trunc(ch);
    canvas.TextRect(rect,rect.left,rect.top,defy);
  end;
  sl.Free;
end;

//TODO: Convert to Unicode
//Make the function build the string in unicode and conver to hex at exit, if non-unicode
//Doesn't work!
function ConvertPinYin(s:string):FString;
const UH_DUMMY_CHAR:FChar = #$F8F0; //used in place of a char when it's unknown or whatever
var nch:string;
  li:integer;
  ali:string;
  cnv,cv2aft:string;
  cnv2: FString;
  cc:char;
  i:integer;
  iscomma:boolean;
begin
  cnv:=lowercase(s);
  cnv2:='';
  li:=0;
  ali:='';
  iscomma:=false;
  for i:=1 to length(cnv) do
  begin
    if (li=0) and ((cnv[i]='a') or (cnv[i]='e') or (cnv[i]='o') or (cnv[i]='u') or (cnv[i]='i')) then li:=i;
    if (li<i) and ((cnv[li]='i') or (cnv[li]='u') or (cnv[li]='ü')) and
      ((cnv[i]='a') or (cnv[i]='e') or (cnv[i]='o') or (cnv[i]='u') or (cnv[i]='i')) then li:=i;
    if (cnv[i]>='0') and (cnv[i]<='5') and (li>0) then
    begin
      cc:=cnv[li];
      ali:=copy(cnv2,length(cnv2)-i-li,i-li-1);
      delete(cnv2,length(cnv2)-i-li-1,i-li);
      if iscomma and (cc='u') then cc:='w';
      case cnv[i] of
        '2':case cc of
              'a':cnv2:=cnv2+#$00E1;
              'e':cnv2:=cnv2+#$00E9;
              'i':cnv2:=cnv2+#$00ED;
              'o':cnv2:=cnv2+#$00F3;
              'u':cnv2:=cnv2+#$00FA;
              'w':cnv2:=cnv2+#$01D8;
            end;
        '4':case cc of
              'a':cnv2:=cnv2+#$00E0;
              'e':cnv2:=cnv2+#$00E8;
              'i':cnv2:=cnv2+#$00EC;
              'o':cnv2:=cnv2+#$00F2;
              'u':cnv2:=cnv2+#$00F9;
              'w':cnv2:=cnv2+#$01DC;
            end;
        '1':case cc of
              'a':cnv2:=cnv2+#$0101;
              'e':cnv2:=cnv2+#$0113;
              'i':cnv2:=cnv2+#$012B;
              'o':cnv2:=cnv2+#$014D;
              'u':cnv2:=cnv2+#$016B;
              'w':cnv2:=cnv2+#$01D6;
            end;
        '3':case cc of
              'a':cnv2:=cnv2+#$01CE;
              'e':cnv2:=cnv2+#$011B;
              'i':cnv2:=cnv2+#$01D0;
              'o':cnv2:=cnv2+#$01D2;
              'u':cnv2:=cnv2+#$01D4;
              'w':cnv2:=cnv2+#$01DA;
            end;
      end;
      li:=0;
      if (cnv[i]='0') or (cnv[i]='5') then
        if cc='w'then
          cnv2:=cnv2+#$00FC
        else
          cnv2:=cnv2+cc;
      cnv2:=cnv2+ali;
      iscomma:=false;
    end else
    if cnv[i]=':'then begin
      cnv2:=cnv2+UH_DUMMY_CHAR;
      iscomma:=true
    end else
    if (cnv[i]<'0') or (cnv[i]>'5') then
      cnv2:=cnv2+cnv[i];
  end;

  //Remove dummy chars
  while pos(UH_DUMMY_CHAR,cnv2)>0 do
    delete(cnv2,pos(UH_DUMMY_CHAR,cnv2),1);
 {$IFDEF UNICODE}
  Result := cnv2;
 {$ELSE}
  Result := UnicodeToHex(cnv2);
 {$ENDIF}
end;

//TODO: Convert to Unicode
function DeconvertPinYin(s:FString):string;
var nch:string;
    cnv,cnv2:string;
    i,j:integer;
    curs,curcc:string;
    curp,curpx:char;
    mustbegin,mustnotbegin,befmustnotbegin,befbefmustnotbegin:boolean;
    number:boolean;
    putcomma,fnd:boolean;
    cc:char;
begin
  cnv:=s;
  putcomma:=false;
  cnv2:='';
  for i:=0 to (length(cnv) div 4)-1 do
  begin
    curs:=copy(cnv,i*4+1,4);
    if putcomma and (curs<>UnicodeToHex('e')) then begin curs:=UnicodeToHex(':')+curs; putcomma:=false; end;
    if curs='01D6'then begin putcomma:=true; curs:='016B'; end;
    if curs='01D8'then begin putcomma:=true; curs:='00FA'; end;
    if curs='01DA'then begin putcomma:=true; curs:='01D4'; end;
    if curs='01DC'then begin putcomma:=true; curs:='00F9'; end;
    if curs='00FC'then begin putcomma:=true; curs:='0075'; end;
    if curs='2026'then curs:=UnicodeToHex('_');
    cnv2:=cnv2+curs;
  end;
  if putcomma then cnv2:=cnv2+UnicodeToHex(':');
  cnv:=cnv2;
  cnv2:='';
  curp:='0';
  mustbegin:=true;
  mustnotbegin:=false;
  befmustnotbegin:=false;
  number:=false;
  for i:=0 to (length(cnv) div 4)-1 do
  begin
    curs:=copy(cnv,i*4+1,4);
    cc:=char((HexToUnicode(curs))[1]);
    if (cc>='0') and (cc<='9') then number:=true;
  end;
  if number then
  begin
    result:=HexToUnicode(s);
    exit;
  end;
  cc:=' ';
  curcc:='';
  for i:=0 to (length(cnv) div 4)-1 do
  begin
    curs:=copy(cnv,i*4+1,4);
    curpx:='0';
    if (curs[1]='0') and (curs[2]='0') and (curs[3]<'8') then cc:=upcase(char((HexToUnicode(curs))[1]))
    else if curs='00E1'then begin cc:='A'; curpx:='2'; end
    else if curs='00E9'then begin cc:='E'; curpx:='2'; end
    else if curs='00ED'then begin cc:='I'; curpx:='2'; end
    else if curs='00F3'then begin cc:='O'; curpx:='2'; end
    else if curs='00FA'then begin cc:='U'; curpx:='2'; end
    else if curs='00E0'then begin cc:='A'; curpx:='4'; end
    else if curs='00E8'then begin cc:='E'; curpx:='4'; end
    else if curs='00EC'then begin cc:='I'; curpx:='4'; end
    else if curs='00F2'then begin cc:='O'; curpx:='4'; end
    else if curs='00F9'then begin cc:='U'; curpx:='4'; end
    else if curs='0101'then begin cc:='A'; curpx:='1'; end
    else if curs='0113'then begin cc:='E'; curpx:='1'; end
    else if curs='012B'then begin cc:='I'; curpx:='1'; end
    else if curs='014D'then begin cc:='O'; curpx:='1'; end
    else if curs='016B'then begin cc:='U'; curpx:='1'; end
    else if curs='0103'then begin cc:='A'; curpx:='3'; end
    else if curs='0115'then begin cc:='E'; curpx:='3'; end
    else if curs='012D'then begin cc:='I'; curpx:='3'; end
    else if curs='014F'then begin cc:='O'; curpx:='3'; end
    else if curs='016D'then begin cc:='U'; curpx:='3'; end
    else if curs='01CE'then begin cc:='A'; curpx:='3'; end
    else if curs='011B'then begin cc:='E'; curpx:='3'; end
    else if curs='01D0'then begin cc:='I'; curpx:='3'; end
    else if curs='01D2'then begin cc:='O'; curpx:='3'; end
    else if curs='01D4'then begin cc:='U'; curpx:='3'; end
    else cc:='?';
    if (((cc>='A') and (cc<='Z')) or (cc=':')) and (cc<>'''') then curcc:=curcc+cc;
    fnd:=false;
    for j:=0 to (romac.count div 4)-1 do
      if pos(lowercase(curcc),lowercase(romac[j*4+1]))=1 then fnd:=true;
    if ((cc<'A') or (cc>'Z')) and (cc<>':') then
    begin
      if curcc<>'' then cnv2:=cnv2+lowercase(curcc)+curp;
      curcc:='';
      cnv2:=cnv2+cc;
      curp:='0';
    end else
    if ((not fnd) or ((curpx<>'0') and (curp<>'0'))) and
       ((copy(curcc,length(curcc)-1,2)='GU') or
        (copy(curcc,length(curcc)-1,2)='NU') or
        (copy(curcc,length(curcc)-1,2)='NI') or
        (copy(curcc,length(curcc)-1,2)='NO')) then
    begin
      cnv2:=cnv2+lowercase(copy(curcc,1,length(curcc)-2))+curp;
      delete(curcc,1,length(curcc)-2);
      curp:='0';
    end else if (not fnd) or ((curpx<>'0') and (curp<>'0')) then
    begin
      cnv2:=cnv2+lowercase(copy(curcc,1,length(curcc)-1))+curp;
      delete(curcc,1,length(curcc)-1);
      curp:='0';
    end else if (cc='?') or (cc='''') then
    begin
      cnv2:=cnv2+lowercase(curcc)+curp;
      curcc:='';
      curp:='0';
    end;
    if curpx<>'0'then curp:=curpx;
  end;
  result:=cnv2+lowercase(curcc)+curp;
end;

procedure DrawKana(c:TCanvas;x,y,fs:integer;ch:string;fontface:string;showr:boolean;romas:integer;lang:char);
var cnv,cnv2:string;
begin
  c.Font.Style:=[];
  if showr then
  begin
    cnv:=KanaToRomaji(ch,romas,lang);
    if lang='c'then cnv2:=ConvertPinYin(cnv) else cnv2:=UnicodeToHex(cnv);
    DrawUnicode(c,x,y,fs+1,cnv2,FontPinYin);
  end else DrawUnicode(c,x,y,fs,ch,fontface);
end;

procedure CreateKnownList(listno:integer;charnumber:integer);
begin
  KnownListSize:=65536 div 8;
  if listno>20000 then showmessage('ListNo size exceeded!');
  getmem(KnownList[listno],KnownListSize);
  fillchar(KnownList[listno]^,KnownListSize,0);
end;

procedure FreeKnownLists;
var i:integer;
begin
  for i:=1 to 20000 do if KnownList[i]<>nil then freemem(KnownList[i],KnownListSize);
end;

procedure SaveKnownList(listno:integer;filename:string);
var f:file;
begin
  if listno>20000 then showmessage('ListNo size exceeded!');
  assignfile(f,filename);
  rewrite(f,1);
  blockwrite(f,KnownList[listno]^,KnownListSize);
  closefile(f);
end;

procedure LoadKnownList(listno:integer;stream:TStream);
var f:file;
    TempStr:TStream;
    i,kj:integer;
    b:byte;
    w:integer;
begin
  if listno>20000 then showmessage('ListNo size exceeded!');
  if stream.Size<KnownListSize then
  begin
    w:=stream.Size;
    for i:=1 to w do
    begin
      stream.Read(b,1);
      for kj:=0 to 7 do
        if (((b) shr kj) and 1)<>0 then
        begin
          if TChar.Locate('Index',inttostr((i-1)*8+1+kj),true) then
          begin
            SetKnown(listno,TChar.Str(TChar.Field('Unicode')),true);
          end;
        end;
    end;
  end
//    Application.MessageBox(pchar(_l('#00635^cSeznam nauèených znakù má zastaralou strukturu a proto nebude nahrán.^eKnown kanji list has outdated structure and will not be loaded.')),pchar(_l('#00090^eWarning^cVarování')),MB_ICONWARNING or MB_OK)
  else
    stream.Read(KnownList[listno]^,KnownListSize);
end;

function IsKnown(listno:integer;const char:FChar):boolean;
var w:widechar{$IFDEF UNICODE} absolute char{$ENDIF};
  ki,kj:integer;
begin
 {$IFNDEF UNICODE}
  w:=HexToUnicode(char)[1];
 {$ENDIF}
  ki:=ord(w) div 8;
  kj:=ord(w) mod 8;
  if ki>=KnownListSize then
  begin
    result:=false;
    exit;
  end;
  result:=(((TByteArray(KnownList[listno]^)[ki]) shr kj) and 1)<>0;
end;

procedure SetKnown(listno:integer;const char:FChar;known:boolean);
var ki,kj:integer;
  a:byte;
  w:widechar{$IFDEF UNICODE} absolute char{$ENDIF};
begin
 {$IFNDEF UNICODE}
  w:=HexToUnicode(char)[1];
 {$ENDIF}
  ki:=ord(w) div 8;
  kj:=ord(w) mod 8;
  if ki>=KnownListSize then exit;
  a:=TByteArray(KnownList[listno]^)[ki];
  if known then a:=a or (1 shl kj) else a:=a and not (1 shl kj);
  TByteArray(KnownList[listno]^)[ki]:=a;
end;

{
Parses the string, fchar by fchar, checking that all kanji are "Learned".
If it encounters a character you haven't learned, it returns that character's
number, else it just returns -1.
}
function FirstUnknownKanjiIndex(const kanji:FString):integer;
{$IFDEF UNICODE}
var i: integer;
begin
  Result := -1;
  for i := 1 to Length(kanji) - 1 do
    if (Word(kanji[i]) and $F000 > $3000) and not IsKnown(KnownLearned, kanji[i]) then begin
      Result := i;
      break;
    end;
end;
{$ELSE}
var i, ch: integer;
begin
  Result := -1;
 //Original function had similar check so let's keep it
  if Length(kanji) mod 4 <> 0 then
    raise Exception.Create('Invalid FChar string at FirstUnknownKanjiIndex(): '+kanji);

  for i := 1 to Length(kanji) div 4 do begin
    ch := PInteger(@kanji[4*(i-1)+1])^;
    if (PFCharData(@ch)^[1]>'3') and not IsKnown(KnownLearned, fcopy(kanji, i, 1)) then begin
      Result := i;
      break;
    end;
  end;
end;
{$ENDIF}

{
Backward compability.
Prepends 'U' to the string if it contains kanjis not yet "learned".
}
function CheckKnownKanji(const kanji:FString): FString;
var i: integer;
begin
  i := FirstUnknownKanjiIndex(kanji);
  if i<0 then
    Result := kanji
  else
    Result := UH_UNKNOWN_KANJI + kanji;
end;


{$IFDEF UNICODE}
//Variants of the functions for cases where we pass a string
function IsKnown(listno:integer;const char:FString):boolean;
begin
  Result := (pointer(char)<>nil) and IsKnown(listno, PFChar(char)^);
end;
procedure SetKnown(listno:integer;const char:FString;known:boolean);
begin
  if Length(char)>=1 then
    SetKnown(listno, char[1], known);
end;
{$ENDIF}

function ChinTo(s:string):string;
var s2,cd:string;
    bk:string;
begin
  if (curlang='j') or (fSettings.RadioGroup5.ItemIndex<>1) then
  begin
    result:=s;
    exit;
  end;
  bk:=TChar.Str(TCharUnicode);
  result:='';
  while s<>'' do
  begin
    s2:=copy(s,1,4);
    delete(s,1,4);
    if (s2[1]>'3') and (TChar.Locate('Unicode',s2,false)) then
    begin
      cd:=fMenu.GetCharValue(TChar.Int(TCharIndex),43);
      if cd<>'' then result:=result+cd else result:=result+s2
    end else result:=result+s2;
  end;
  TChar.Locate('Unicode',bk,false);
end;

//TODO: Upgrade this function to Unicode
function ChinFrom(s:string):string;
var s2,cd:string;
    bk:string;
begin
  if (curlang='j') or (fSettings.RadioGroup5.ItemIndex=0) then
  begin
    result:=s;
    exit;
  end;
  bk:=TChar.Str(TCharUnicode);
  result:='';
  while s<>'' do
  begin
    s2:=copy(s,1,4);
    delete(s,1,4);
    if (s2[1]>'3') and (TChar.Locate('Unicode',s2,false)) then
    begin
      cd:=fMenu.GetCharValue(TChar.Int(TCharIndex),44);
      if cd<>'' then result:=result+cd else result:=result+s2
    end else result:=result+s2;
  end;
  TChar.Locate('Unicode',bk,false);
end;

{$IFDEF UNICODE}
function EvalChar(char:WideChar):integer;
var ch: Word absolute char;
begin
  if ch=$3005 then result:=1 else // kurikaeshi
  if (ch>=$3000) and (ch<=$303F) then result:=EC_IDG_PUNCTUATION else
  if (ch>=$3040) and (ch<=$309F) then result:=EC_HIRAGANA else
  if (ch>=$30A0) and (ch<=$30FF) then result:=EC_KATAKANA else
  if (ch>=$3200) and (ch<=$33FF) then result:=EC_IDG_OTHER else
  if (ch>=$3400) and (ch<=$9FFF) then result:=EC_IDG_CHAR else
  if (ch>=$F900) and (ch<=$FAFF) then result:=EC_IDG_CHAR else
  if (ch>=$FE30) and (ch<=$FE4F) then result:=EC_IDG_PUNCTUATION else
  if (ch>=$FF00) and (ch<=$FF5F) then result:=EC_LATIN_FW else
  if (ch>=$FF60) and (ch<=$FF9F) then result:=EC_KATAKANA_HW else
  if (ch>=$0000) and (ch<=$007F) then result:=EC_LATIN_HW else
  result:=EC_UNKNOWN;
end;

function EvalChar(const char:FString):integer;
begin
  Result := EvalChar(WideChar(PWideChar(char)^));
end;
{$ELSE}
function EvalChar(const char:FString):integer;
begin
  if char='3005'then result:=1 else // kurikaeshi
  if (char>='3000') and (char<='303F') then result:=EC_IDG_PUNCTUATION else
  if (char>='3040') and (char<='309F') then result:=EC_HIRAGANA else
  if (char>='30A0') and (char<='30FF') then result:=EC_KATAKANA else
  if (char>='3200') and (char<='33FF') then result:=EC_IDG_OTHER else
  if (char>='3400') and (char<='9FFF') then result:=EC_IDG_CHAR else
  if (char>='F900') and (char<='FAFF') then result:=EC_IDG_CHAR else
  if (char>='FE30') and (char<='FE4F') then result:=EC_IDG_PUNCTUATION else
  if (char>='FF00') and (char<='FF5F') then result:=EC_LATIN_FW else
  if (char>='FF60') and (char<='FF9F') then result:=EC_KATAKANA_HW else
  if (char>='0000') and (char<='007F') then result:=EC_LATIN_HW else
  result:=EC_UNKNOWN;
end;
{$ENDIF}


function StateStr(i:integer):string;
begin
  case i of
    0:result:=_l('#00638^eProblematic^cProblematické');
    1:result:=_l('#00639^eUnlearned^cNenauèené');
    2:result:=_l('#00640^eLearned^cNauèené');
    3:result:=_l('#00641^eMastered^cDobøe nauèené');
  end;
end;

function DateForm(s:string):string;
begin
  if s='00000000'then result:='-'else
  result:=copy(s,7,2)+'.'+copy(s,5,2)+'.'+copy(s,1,4);
end;

procedure WritelnMixUni(var f:file;s:string);
var bl,bh:byte;
    inuni:boolean;
    s2:string;
begin
  inuni:=false;
  while length(s)>0 do
  begin
    if inuni and (s[1]='}') then
    begin
      inuni:=false;
      delete(s,1,1);
    end else if not inuni and (s[1]='{') then
    begin
      inuni:=true;
      delete(s,1,1);
    end else if inuni then
    begin
      s2:=copy(s,1,2);
      delete(s,1,2);
      bh:=strtoint('0x'+s2);
      s2:=copy(s,1,2);
      delete(s,1,2);
      bl:=strtoint('0x'+s2);
      blockwrite(f,bl,1);
      blockwrite(f,bh,1);
    end else
    begin
      bh:=0;
      bl:=ord(s[1]);
      delete(s,1,1);
      blockwrite(f,bl,1);
      blockwrite(f,bh,1);
    end;
  end;
  bh:=0;
  bl:=10;
  blockwrite(f,bl,1);
  blockwrite(f,bh,1);
  bh:=0;
  bl:=13;
  blockwrite(f,bl,1);
  blockwrite(f,bh,1);
end;

function StripCatName(s:string):string;
begin
  if (length(s)>1) and (s[2]='~') then delete(s,1,2);
  result:=s;
end;

procedure InitColors;
var i:integer;
    reg:TRegIniFile;
    s:string;
    s2:string;
begin
  colarr:=TStringList.Create;
  colarr.add('0Kanji_Back=FFFFFF,^eBackground^cPozadí');
  colarr.add('0Kanji_Common=000000,^eCommon characters^cBìžné znaky');
  colarr.add('0Kanji_Rare=4F4F4F,^eRare characters^cVzácné znaky');
  colarr.add('0Kanji_Names=005F00,^eCharacters in names^cZnaky ve jménech');
  colarr.add('0Kanji_Learned=7F0000,^eLearned characters^cNauèené znaky');
  colarr.add('0Kanji_RadCommon=000000,^eCommon radicals^cBìžné radikály');
  colarr.add('0Kanji_RadRare=4F4F4F,^eRare radicals^cVzácné radikály');
  colarr.add('0Kanji_RadLearned=7F0000,^eLearned radicals^cNauèené radikály');
  colarr.add('1Dict_Back=FFFFFF,^eBackground^cPozadí');
  colarr.add('1Dict_Text=000000,^eText^cText');
  colarr.add('1Dict_UnknownChar=2F2F7F,^eUnknown characters^cNeznámé znaky');
  colarr.add('1Dict_Problematic=DDDDFF,^eProblematic words^cProblematická slova');
  colarr.add('1Dict_Unlearned=FFEEDD,^eUnlearned words^cNenauèená slova');
  colarr.add('1Dict_Learned=BBFFFF,^eLearned words^cNauèená slova');
  colarr.add('1Dict_Mastered=BBFFBB,^eMastered words^cDobøe nauèená slova');
  colarr.add('1Dict_SelBack=BBBBBB,^eBackground (selected)^cPozadí (vybrané)');
  colarr.add('1Dict_SelProblematic=9999BB,^eProblematic words (selected)^cProblematická slova (vybraná)');
  colarr.add('1Dict_SelUnlearned=BBAA99,^eUnlearned words (selected)^cNenauèená slova (vybraná)');
  colarr.add('1Dict_SelLearned=99BBBB,^eLearned words (selected)^cNauèená slova (vybraná)');
  colarr.add('1Dict_SelMastered=77BB77,^eMastered words (selected)^cDobøe nauèená slova (vybraná)');
  colarr.add('2Mark_Special=7F007F,^eSpecial markers^cSpeciální pøíznaky');
  colarr.add('2Mark_Usage=00007F,^eUsage markers^cPøíznaky použití');
  colarr.add('2Mark_Grammatical=7F0000,^eGrammatical markers^cGramatické pøíznaky');
  colarr.add('2Mark_Dict=4F4F4F,^eDictionary markers^cPøíznaky slovníku');
  colarr.add('2Mark_Lesson=004F00,^eLesson markers,^cPøíznaky lekce');
  colarr.add('3Editor_Back=FFFFFF,^eBackground^cPozadí');
  colarr.add('3Editor_Text=000000,^eText color^cBarva písma');
  colarr.add('3Editor_ASCII=2F2F2F,^eASCII text^cASCII text');
  colarr.add('3Editor_Active=FF0000,^eText being written^cText, který je právì psán');
  colarr.add('3Editor_Aftertouch=0000FF,^eText just converted^cText právì zkonvertován');
  colarr.add('3Editor_Untranslated=FFFFFF,^eUntranslated text^cNepøeložený text');
  colarr.add('3Editor_NotFound=003FFF,^eText where translation failed^cText, u kterého pøeklad selhal');
  colarr.add('3Editor_Particle=FFAAFF,^eEstimated particle^cOdhadovaná partikule');
  colarr.add('3Editor_Translated=EEEEEE,^eWord not in vocabulary^cSlovo, které není ve slovíèkách');
  colarr.add('3Editor_Problematic=DDDDFF,^eProblematic vocabulary word^cProblematické slovíèko');
  colarr.add('3Editor_Unlearned=FFEEDD,^eUnlearned vocabulary word^cNenauèené slovíèko');
  colarr.add('3Editor_Learned=BBFFFF,^eLearned vocabulary word^cNauèené slovíèko');
  colarr.add('3Editor_Mastered=BBFFBB,^eMastered vocabulary word^cDobøe nauèené slovíèko');
  colarr.add('3Editor_HintBack=EFEFEF,^eHint background^cPozadí nápovìdy');
  colarr.add('3Editor_HintSelected=00FFFF,^eHint selected background^cVybrané pozadí nápovìdy');
  colarr.add('3Editor_HintText=000000,^eHint text^cText nápovìdy');
  colarr.add('4Popup_Back=A0FFFF,^eBackground^cPozadí');
  colarr.add('4Popup_Lines=000000,^eLines^cÈáry');
  colarr.add('4Popup_Card=FFFFFF,^eCharacter card^cKarta znaku');
  colarr.add('4Popup_Text=000000,^eText on the caracter card^cText na kartì znaku');
  colval:=TStringList.Create;
  reg:=TRegIniFile.Create('Software\Labyrinth\Wakan');
  colsarr:=TStringList.Create;
  for i:=0 to colarr.Count-1 do
  begin
    s:=colarr[i];
    delete(s,1,1);
    s2:=s;
    delete(s2,1,pos('=',s));
    s:=copy(s,1,pos('=',s)-1);
    s2:=copy(s2,1,pos(',',s2)-1);
    colval.Add(reg.ReadString('Colors',s,s2));
    colsarr.Add(s);
  end;
  colsarr.Sorted:=true;
  colsarr.Sort;
  reg.Free;
  SetCol(-1,clBlack);
end;

procedure SetColDefault(i:integer);
var s,s2:string;
begin
  s:=colarr[i];
  delete(s,1,1);
  s2:=s;
  delete(s2,1,pos('=',s));
  s:=copy(s,1,pos('=',s)-1);
  s2:=copy(s2,1,pos(',',s2)-1);
  colval[i]:=s2;
  SetCol(-1,clBlack);
end;

function GetColorString(i:integer):string;
var s:string;
begin
  result:='';
  if i>=colarr.Count then exit;
  s:=colarr[i];
  result:=s[1];
  delete(s,1,pos(',',s));
  result:=result+s;
end;

procedure SetCol(col:integer;val:TColor);
var i,j:integer;
begin
  if col>-1 then colval[col]:=Format('%6.6X',[val]);
  for i:=0 to colsarr.Count-1 do for j:=0 to colarr.Count-1 do if copy(colarr[j],2,length(colsarr[i]))=colsarr[i] then
    colsval[i]:=strtoint('0x'+colval[j]);
end;

function GetCol(col:integer):TColor;
begin
  result:=strtoint('0x'+colval[col]);
end;

function Col(col:string):TColor;
var i:integer;
begin
  result:=colsval[colsarr.IndexOf(col)];
end;

procedure WriteColors;
var i:integer;
    reg:TRegIniFile;
    s:string;
    s2:string;
begin
  reg:=TRegIniFile.Create('Software\Labyrinth\Wakan');
  for i:=0 to colarr.Count-1 do
  begin
    s:=colarr[i];
    delete(s,1,1);
    s2:=s;
    delete(s2,1,pos('=',s));
    s:=copy(s,1,pos('=',s)-1);
    s2:=copy(s,1,pos(',',s));
    reg.WriteString('Colors',s,colval[i]);
  end;
end;

procedure PaintScreenTipBlock;
var oldR2:integer;
begin
  if STB_Canvas<>nil then
  begin
    oldR2:=SetROP2(STB_Canvas.Handle,R2_NOT);
    STB_Canvas.Rectangle(STB_x1,STB_y1,STB_x2,STB_y2);
    SetROP2(STB_Canvas.Handle,R2_NOT);
  end;
end;

procedure SetScreenTipBlock(x1,y1,x2,y2:integer;canvas:TCanvas);
begin
  PaintScreenTipBlock;
  STB_x1:=x1;
  STB_y1:=y1;
  STB_x2:=x2;
  STB_y2:=y2;
  STB_canvas:=canvas;
  PaintScreenTipBlock;
end;

function CombUniToHex(s:string):string;
var i,lhcount:integer;
begin
  result:='';
  lhcount:=0;
  for i:=1 to length(s) do
    if lhcount>0 then
    begin
      dec(lhcount);
      result:=result+format('%2.2X',[ord(s[i])]);
    end else if ord(s[i])>128 then lhcount:=(ord(s[i])-128)*2 else
      result:=result+'00'+format('%2.2X',[ord(s[i])]);
end;

function HexToCombUni(s:string):string;
var i,j,lhcount:integer;
begin
  result:='';
  lhcount:=0;
  for i:=0 to (length(s) div 4)-1 do
  begin
    j:=i;
    if (lhcount=0) then
    begin
      while (j<(length(s) div 4)) and ((s[j*4+1]<>'0') or (s[j*4+2]<>'0') or (s[j*4+3]>'7')) do
      begin
        inc(j); inc(lhcount);
      end;
      if lhcount>0 then result:=result+chr(128+lhcount);
    end;
    if lhcount>0 then
    begin
      dec(lhcount);
      result:=result+chr(StrToInt('0x'+copy(s,i*4+1,2)))+chr(StrToInt('0x'+copy(s,i*4+3,2)));
    end else result:=result+chr(StrToInt('0x'+copy(s,i*4+3,2)));
  end;
end;

var i:integer;

begin
  CurPBox:=nil;
  for i:=1 to MAX_INTTEXTINFO do itt[i].act:=false;
  for i:=1 to 20000 do KnownList[i]:=nil;
  GridFontSize:=14;
  STB_Canvas:=nil;
  kcchind:=TStringList.Create;
  kcchcomp:=TStringList.Create;
end.
