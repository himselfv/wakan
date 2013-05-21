unit JWBStrings;
{ Wakan string types and routines + some other basic stuff (JWBBasicTypes if you wish).
 This one is lower level unit, do not make any circular references (this kills inlining)
 or put stuff here which requires those references.
 Use JWBUnit for that. }

interface
uses SysUtils, Classes, Windows;

{$IF NOT Defined(IntPtr)}
type
 {$IF Defined(WIN64)}
  IntPtr = int64;
 {$ELSEIF Defined(WIN32)}
  IntPtr = integer;
 {$ELSE}
  {$MESSAGE Error 'Cannot declare IntPtr for this target platform'}
 {$IFEND}
{$IFEND}

{$IF CompilerVersion < 21}
type
{ For cases when we need a HARD unicode string ("unicode on any versions"):
  On older compilers we don't have a new, faster UnicodeString,
  but we can safely use a slower WideString in its stead. }
  UnicodeString = WideString;
  PUnicodeString = PWideString;
{$IFEND}

{ One day we're going to enable typechecks so that you can't mix strings and FStrings
 without proper conversion }
//{$DEFINE STRICT_FSTRING}

{ Wakan uses a special Ansi string format, where each unicode symbol is kept
 as a 4 character (four-char) hex code.
 On Unicode it's replaced with normal unicode string.
 Please use a special set of functions defined further below when working with
 such strings. }
type
 {$IFDEF UNICODE}
 //Wired to WideChar and not Char because who knows,
 //we might want to compile on Ansi with UNICODE set!
  FChar = WideChar;
  PFChar = {$IFDEF STRICT_FSTRING}type{$ENDIF} PWideChar;
  FString = {$IFDEF STRICT_FSTRING}type{$ENDIF} UnicodeString;
 {$ELSE}
  FChar = AnsiString; //because one 4-character takes 4 AnsiChars
  PFChar = {$IFDEF STRICT_FSTRING}type{$ENDIF} PAnsiChar;
  FString = {$IFDEF STRICT_FSTRING}type{$ENDIF} AnsiString;
  //Useful for looking into FChars
  FCharData = array[1..4] of AnsiChar;
  PFCharData = ^FCharData;
 {$ENDIF}

  PFString = ^FString;

 { Hex FChars on both Ansi and Unicode. Used to read/write data from formats which store it as hex }
  FHex = {$IFDEF STRICT_FSTRING}type{$ENDIF} string;


const
 { Some character constants used throught the program
  Names MUST be descriptive. If you name something 'UH_SPACE' it means it FUNCTIONS as space,
  not just looks like one. }
 {$IFNDEF UNICODE}
  UH_NOCHAR:FChar = ''; //for initializing stuff with it
  UH_ZERO:FChar = '0000'; //when you need zero char
  UH_LF: FChar = '000A'; //linefeed
  UH_CR: FChar = '000D'; //carriage return
  UH_SPACE:FChar = '0020';
  UH_HYPHEN:FChar = '002D'; //-
  UH_LOWLINE:FChar = '005F'; //_
  UH_ELLIPSIS:FChar = '2026'; //
  UH_IDG_SPACE:FChar = '3000'; //Ideographic space (full-width)
  UH_IDG_COMMA:FChar = '3001';

  UH_AORUBY_TEXTBREAK:FChar = 'FF5C';
  UH_AORUBY_OPEN:FChar = '300A';
  UH_AORUBY_CLOSE:FChar = '300B';
  UH_AORUBY_COMM_OPEN:FChar = 'FF3B';
  UH_AORUBY_COMM_CLOSE:FChar = 'FF3D';
  UH_AORUBY_TAG_OPEN:FChar = '003C';
  UH_AORUBY_TAG_CLOSE:FChar = '003E';

 { Control Characters.
  Wakan uses single characters like 'U', '~', '@' to control text markup.
  We can't do that in Unicode because those characters can legitimately occur in text.
  Instead we use symbols U+E000..U+F8FF which are for private use by applications.

  Not all control characters are well understood, so they don't yet have descriptive names.
  For non-descriptive names use prefix ALTCH_ }
  UH_UNKNOWN_KANJI:Char = 'U'; //Set by CheckKnownKanji

 { All used by DrawWordInfo }
  ALTCH_EXCL:Char = '!';
  UH_DRAWWORD_KANA:Char = '#'; //can only be first character in the string. Means that this field is Kana.
  UH_DRAWWORD_KANJI:Char = '@'; //same, kanji
  ALTCH_TILDE:Char = '~'; //followed by 1 character word type (F, I)
  UH_SETCOLOR:Char = '%'; //followed by 6 character hex color
  UH_LBEG:Char ='<'; //begin flag text (ex.: <dEDICT> <gram> <suf>)
  UH_LEND:Char = '>'; //end flag text

  UH_RUBY_PLACEHOLDER:FChar = 'E100'; //when ruby has nothing to be attached to,
    //but we still have to store it in the decoded form

 {$ELSE}
  UH_NOCHAR:FChar = #$0000;
  UH_ZERO:FChar = #$0000;
  UH_LF: FChar = #$000A;
  UH_CR: FChar = #$000D;
  UH_SPACE:FChar = #$0020;
  UH_HYPHEN:FChar = '-';
  UH_LOWLINE:FChar = '_';
  UH_ELLIPSIS:FChar = #$2026;
  UH_IDG_SPACE:FChar = #$3000;
  UH_IDG_COMMA:FChar = #$3001;

  UH_AORUBY_TEXTBREAK:FChar = '｜';
  UH_AORUBY_OPEN:FChar = '《';
  UH_AORUBY_CLOSE:FChar = '》';
  UH_AORUBY_COMM_OPEN:FChar = '［';
  UH_AORUBY_COMM_CLOSE:FChar = '］';
  UH_AORUBY_TAG_OPEN:FChar = '<';
  UH_AORUBY_TAG_CLOSE:FChar = '>';

  UH_UNKNOWN_KANJI:Char = #$E001;

  ALTCH_EXCL:Char = #$E002;
  UH_DRAWWORD_KANA = #$E003;
  UH_DRAWWORD_KANJI = #$E004;
  ALTCH_TILDE:Char = #$E005;
  UH_SETCOLOR:Char = #$E006;
  UH_LBEG:Char = #$E008;
  UH_LEND:Char = #$E007;

  UH_RUBY_PLACEHOLDER:FChar = #$E100;
 {$ENDIF}


{ Math }

{ Min and max so we don't have to link Math.pas just for that }
function min(a, b: integer): integer; inline;
function max(a, b: integer): integer; inline;


{ Files }

function GetModuleFilenameStr(hModule: HMODULE = 0): string;
function GetFileVersionInfoStr(Filename: string): string;
function GetTempPathStr: string;
function CreateGuidStr: string;
function CreateRandomTempDirName: string;
function CreateRandomTempDir: string;
function AppFolder: string;
function GetSpecialFolderPath(folderId: integer): string;


{ FChar string functions.
Please use these when working with FChars! They resolve to proper manipulations,
whether we are working with UnicodeStrings or 4-character hex AnsiStrings.
If you need a general purpose string function which works differently
on Ansi and Unicode builds, add it here.

In particular:
  (length(s) div 4)-1   --> flength(s)-1
  copy(s,i*4-3,4)       --> fgetch(s,i)            but note that fgetch requires the symbol to exist, copy doesn't
  copy(s,i*4+1,4)       --> fgetch(s,i+1)
  copy(s,i*4+5,4)       --> fgetch(s,i+2)
  copy(s,i*4+1,len*4)   --> fcopy(s,i+1,len)
  delete(s,i*4+1,len*4) --> fdelete(s,i+1,len)
  HexToUnicode(unistr)  --> fstr(unistr)           when converting to FStrings

As a rule, any "length()" inside of "fcopy()" MUST be "flength()".

When working with normal strings, or when accessing control characters in FStrings,
please continue to use normal functions:
  var sample: FString;
  if (Length(sample)>0) and (sample[1]='~')  --- keep intact!

It is very important that in release builds these functions are all inlined.
Therefore if you're doing the building, READ THIS:
  http://docwiki.embarcadero.com/RADStudio/XE/en/Calling_Procedures_and_Functions#Using_the_inline_Directive
Note the list of cases when the inlining is not done.
Enable inlining.
Disable Compiling> String format checking in Delphi Project Options:
  http://www.micro-isv.asia/2008/10/needless-string-checks-with-ensureunicodestring/
}

function flength(const s:FString): integer; {$IFDEF INLINE}inline;{$ENDIF}
function flenfc(lenn:integer): integer; {$IFDEF INLINE}inline;{$ENDIF}
function flenn(lenfc:integer): integer; {$IFDEF INLINE}inline;{$ENDIF}
function fcopy(const s: FString; Index, Count: Integer):FString; {$IFDEF INLINE}inline;{$ENDIF}
procedure fdelete(var s: FString; Index, Count: Integer); {$IFDEF INLINE}inline;{$ENDIF}
function fpos(const substr: FString; const str:FString):integer; {$IFDEF INLINE}inline;{$ENDIF}
function fgetch(const s: FString; Index: integer): FChar; {$IFDEF INLINE}inline;{$ENDIF}
function fgetchl(const s: FString; Index: integer): FChar; {$IFDEF INLINE}inline;{$ENDIF}
procedure fputch(var s: FString; Index: integer; const ch: FChar); {$IFDEF INLINE}inline;{$ENDIF}
function fstr(const s: AnsiString): FString; overload; {$IFDEF INLINE}inline;{$ENDIF}
function fstr(const s: UnicodeString): FString; overload; {$IFDEF INLINE}inline;{$ENDIF}
function fstrtouni(const s: FString): UnicodeString; {$IFDEF INLINE}inline;{$ENDIF}
function hextofstr(const s: FHex): FString; {$IFDEF INLINE}inline;{$ENDIF}
function fstrtohex(const s: FString): FHex; {$IFDEF INLINE}inline;{$ENDIF}

{$IFNDEF UNICODE}
function FcharCmp(a, b: PFChar; cnt: integer): boolean; {$IFDEF INLINE}inline;{$ENDIF}
{$ENDIF}
function EatOneFChar(var pc: PAnsiChar): boolean; {$IFDEF INLINE}inline;{$ENDIF}
function EatOneFCharW(var pc: PWideChar): boolean; {$IFDEF INLINE}inline;{$ENDIF}

function fch(const c: AnsiChar): FChar; overload; {$IFDEF INLINE}inline;{$ENDIF}
function fch(const c: WideChar): FChar; overload; {$IFDEF INLINE}inline;{$ENDIF}
function ftoansi(c: FChar; out ac: AnsiChar): boolean; overload;
function ftoansi(c: FChar): AnsiChar; overload;
function ftowide(c: FChar): WideChar; {$IFDEF INLINE}inline;{$ENDIF}

{
Unicode versions of some function which are unavailable/inefficient on Ansi builds.
On Unicode builds all resolve to default Unicode equivalents.
}
function upos(const substr, str: UnicodeString):integer; {$IFDEF UNICODE}inline;{$ENDIF}
function UTrim(const S: UnicodeString): UnicodeString; {$IFDEF UNICODE}inline;{$ENDIF}
function UTrimLeft(const S: UnicodeString): UnicodeString; {$IFDEF UNICODE}inline;{$ENDIF}
function UTrimRight(const S: UnicodeString): UnicodeString; {$IFDEF UNICODE}inline;{$ENDIF}
function ULowerCase(const S: UnicodeString): UnicodeString; {$IFDEF UNICODE}inline;{$ENDIF}
function UUpperCase(const S: UnicodeString): UnicodeString; {$IFDEF UNICODE}inline;{$ENDIF}

{ General purpose string functions }

function IsHexChar(wc: WideChar): boolean;
function HexCharCode(c:AnsiChar): byte; {$IFDEF INLINE}inline;{$ENDIF}
function HexCharCodeW(c:WideChar): byte; {$IFDEF INLINE}inline;{$ENDIF}
function HexToUnicode(ps:PAnsiChar; maxlen: integer): UnicodeString; overload;
function HexToUnicodeW(ps:PWideChar; maxlen: integer): UnicodeString; overload;
function HexToUnicode(const s:string):UnicodeString; overload;
function BytesToHex(pb:PByte;sz:integer):string;
function UnicodeToHex(pc:PWideChar; len: integer):string; overload;
function UnicodeToHex(const s:UnicodeString):string; overload;
function AnsiToHex(const s:AnsiString):string; {$IFDEF INLINE}inline;{$ENDIF}
function UnicodeToML(s:widestring):string;

type
  TStringArray = array of string;

procedure SplitAdd(sl:TStringList;s:string;cnt:integer);
function SplitStr(s: string; cnt: integer; ch: char=','): TStringArray; overload;
function SplitStr(s: string; ch: char=','): TStringArray; overload;
procedure StrListAdd(sl: TStringList; sa: TStringArray);

function remexcl(const s:string):string;
function remmark(s:string):string;
function repl(var s:string;const sub,rep:string):string;
function urepl(var s:UnicodeString;const sub,rep:UnicodeString):UnicodeString;
function replc(const s:string;const sub,rep:string):string;
function ureplc(const s:UnicodeString;const sub,rep:UnicodeString):UnicodeString;
function strqpop(var s:string;c:char):string; overload;
function strqpop(var s:string;const cs:string):string; overload;
function ustrqpop(var s:UnicodeString;c:WideChar):UnicodeString; overload;

procedure MakeFixedLen(var s: AnsiString; len: integer; pad_ch: AnsiChar); overload; inline;
procedure MakeFixedLen(var s: UnicodeString; len: integer; pad_ch: WideChar); overload; inline;


{ Filename list }

type
  TFilenameList = array of string;
  TFileList = TFilenameList;

procedure AddFilename(var list: TFilenameList; const filename: string);
function MakeFilenameList(const filenames: array of string): TFilenameList;
function IsFileInList(const filename: string; const list: TFilenameList): boolean;
function FilenameListToString(const list: TFilenameList; const sep: string = ', '): string;


{ Character processing }

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


function EvalChar(char:WideChar):integer; overload; {$IFDEF INLINE}inline;{$ENDIF}
{$IFDEF UNICODE}
function EvalChar(const char:FString):integer; overload; {$IFDEF INLINE}inline;{$ENDIF}
{$ELSE}
function EvalChar(const char:FString):integer; overload;
{$ENDIF}

//Returns a set of (1 shl EC_*) flags indicating some chars are present in string
function EvalChars(const chars:FString):integer;

function IsUpcaseLatin(ch: AnsiChar): boolean; overload; inline;
function IsUpcaseLatin(ch: WideChar): boolean; overload; inline;
function IsLocaseLatin(ch: AnsiChar): boolean; overload; inline;
function IsLocaseLatin(ch: WideChar): boolean; overload; inline;
function LoCase(ch: AnsiChar): AnsiChar; overload; inline; //Delphi has UpCase but not LoCase for chars
function LoCase(ch: WideChar): WideChar; overload; inline;
function IsDigit(ch: AnsiChar): boolean; overload; inline;
function IsDigit(ch: WideChar): boolean; overload; inline;

//Shit sucks. We have to have a ton of versions!
function IsLatinLetter(c:char): boolean; {$IFDEF INLINE}inline;{$ENDIF}
function IsLatinLetterW(c:WideChar): boolean; {$IFDEF INLINE}inline;{$ENDIF}
function IsLatinLetterF(c:FChar): boolean; {$IFDEF INLINE}inline;{$ENDIF}
function IsHalfWidthChar(c:FChar): boolean;
function IsLatinDigit(c:char):boolean;
function IsLatinDigitF(c:FChar):boolean;

function IsKanaCharKatakana(c:FString; i:integer): boolean;
function IsSokuon(const c: FChar): boolean; inline;
function IsSmallKanaVowel(const c: FChar): boolean; inline;

implementation
uses WideStrUtils, ShlObj;

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

function GetTempPathStr: String;
var tempFolder: array[0..MAX_PATH+1] of Char; //MAX_PATH+2 characters
begin
  GetTempPath(MAX_PATH+1, @tempFolder); //supports at most maxpath+1 characters + null, per documentation
  result := tempFolder;
end;

function CreateGuidStr: string;
var g: TGuid;
begin
  CreateGuid(g);
  Result := GuidToString(g);
end;

function CreateRandomTempDirName: string;
begin
 //Let's be nice and mark all our temp folders with "wakan_", so that they're identifiable
  Result := GetTempPathStr() + '\wakan_' + CreateGuidStr();
end;

function CreateRandomTempDir: string;
begin
  Result := CreateRandomTempDirName();
  ForceDirectories(Result);
end;

function AppFolder: string;
begin
  Result := ExtractFilePath(GetModuleFilenameStr(0));
end;

function GetSpecialFolderPath(folderId: integer): string;
const SHGFP_TYPE_CURRENT = 0;
var hr: HRESULT;
begin
  SetLength(Result, MAX_PATH+1);
  hr := SHGetFolderPath(0, folderId, 0, SHGFP_TYPE_CURRENT or CSIDL_FLAG_CREATE, @Result[1]);
  if FAILED(hr) then
    raise Exception.Create('Cannot obtain special folder path: error 0x'+IntToHex(hr,8));

 //Truncate result
  SetLength(Result, WStrLen(PWideChar(Result)));
end;


{ Strings }

//returns length of the string in 4-characters
function flength(const s:FString):integer;
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
function fcopy(const s: FString; Index, Count: Integer):FString;
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

function fpos(const substr: FString; const str:FString):integer; {$IFDEF INLINE}inline;{$ENDIF}
begin
  Result := pos(substr,str){$IFNDEF UNICODE}div 4{$ENDIF};
end;

//Returns a character from the string
function fgetch(const s: FString; Index: integer): FChar;
begin
{$IFDEF UNICODE}
  Result := s[Index];
{$ELSE}
  Result := fcopy(s, Index, 1);
{$ENDIF}
end;

//Safe fgetch: if Index<1 or Index>Length it's guaranteed to not fail (return UH_NOCHAR, in fact)
function fgetchl(const s: FString; Index: integer): FChar;
begin
{$IFDEF UNICODE}
  if (Index<1) or (Index>Length(s)) then
    Result := UH_NOCHAR
  else
    Result := s[Index];
{$ELSE}
  Result := fcopy(s, Index, 1); //UH_NOCHAR='' on non-unicode, so it's okay
{$ENDIF}
end;

procedure fputch(var s: FString; Index: integer; const ch: FChar);
begin
{$IFDEF UNICODE}
  s[index] := ch;
{$ELSE}
  s[4*(Index-1)+1] := ch[1];
  s[4*(Index-1)+2] := ch[2];
  s[4*(Index-1)+3] := ch[3];
  s[4*(Index-1)+4] := ch[4];
{$ENDIF}
end;

function fstr(const s: AnsiString): FString;
begin
  Result := fstr(UnicodeString(s));
end;

//Converts raw (unicode) data to FString. On unicode does nothing!
//Only Unicode input is accepted because Ansi MUST be converted to Unicode.
//We can't do AnsiToHex, that would leave us with 2-byte and not 4-byte symbols.
function fstr(const s: UnicodeString): FString;
begin
 {$IFDEF UNICODE}
  Result := FString(s);
 {$ELSE}
  Result := UnicodeToHex(s);
 {$ENDIF}
end;

//Converts FString to raw UnicodeString. On unicode does nothing!
//Call when you want to pass FString to someone who accepts UnicodeStrings
function fstrtouni(const s: FString): UnicodeString; {$IFDEF INLINE}inline;{$ENDIF}
begin
 {$IFDEF UNICODE}
  Result := UnicodeString(s);
 {$ELSE}
  Result := HexToUnicode(s);
 {$ENDIF}
end;

//Converts Hex to FString. On Ansi builds does nothing!
//Call when you want to parse Ansi Hex data (ex. from file) to FString
function hextofstr(const s: FHex): FString;
begin
 {$IFDEF UNICODE}
  Result := HexToUnicode(s);
 {$ELSE}
  Result := FHex(s);
 {$ENDIF}
end;

//Converts FString to Hex. On Ansi builds does nothing
//Call when you want to store FString as Ansi Hex data (ex. in file of such format)
function fstrtohex(const s: FString): FHex;
begin
 {$IFDEF UNICODE}
  Result := UnicodeToHex(s);
 {$ELSE}
  Result := FString(s);
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

{ Converts one character to FChar }
function fch(const c: AnsiChar): FChar;
begin
{$IFDEF UNICODE}
  Result := FChar(c);
{$ELSE}
  Result := BytesToHex(@c, SizeOf(AnsiChar));
{$ENDIF}
end;

function fch(const c: WideChar): FChar;
begin
{$IFDEF UNICODE}
  Result := FChar(c);
{$ELSE}
  Result := UnicodeToHex(@c, SizeOf(WideChar));
{$ENDIF}
end;

{ Converts FChar to AnsiChar, returns false if it was impossible.
 Only for simple transformations, else use WideCharToMultiByte(fstrtouni(s)) }
function ftoansi(c: FChar; out ac: AnsiChar): boolean;
begin
 {$IFDEF UNICODE}
  Result := (ord(c)<256);
  if Result then ac := AnsiChar(c);
 {$ELSE}
  Result := (Length(c)=4) and (c[1]='0') and (c[2]='0');
  if Result then
    ac := Chr((HexCharCode(c[3]) shl 4) + HexCharCode(c[4]));
 {$ENDIF}
end;

//Same but it's your problem if the input wasn't ansi
function ftoansi(c: FChar): AnsiChar;
begin
{$IFDEF UNICODE}
  Result := AnsiChar(c);
{$ELSE}
  Result := Chr((HexCharCode(c[3]) shl 4) + HexCharCode(c[4]));
{$ENDIF}
end;

function ftowide(c: FChar): WideChar;
begin
{$IFDEF UNICODE}
  Result := c;
{$ELSE}
  Result := WideChar(
    (HexCharCode(c[1]) shl 12)
    (HexCharCode(c[2]) shl 8)
    (HexCharCode(c[3]) shl 4)
    + HexCharCode(c[4]));
{$ENDIF}
end;


{
Unicode equivalents of common functions, missing on Ansi
}

{ Unicode version of pos() -- resolves to whatever is best on the platform }
function upos(const substr, str: UnicodeString):integer;
{$IFDEF UNICODE}
begin
  Result := pos(substr,str);
end;
{$ELSE}
var pw: PWideChar;
begin
  if str='' then begin
    Result := 0;
    exit;
  end;
  pw := WStrPos(PWideChar(str),PWideChar(substr)); //this one has reverse argument order!
  Result := (IntPtr(pw)-IntPtr(str)) div 2 - 1;
end;
{$ENDIF}

function UTrim(const S: UnicodeString): UnicodeString;
{$IFDEF UNICODE}
begin
  Result := Trim(S);
end;
{$ELSE}
var
  I, L: Integer;
begin
  L := Length(S);
  I := 1;
  while (I <= L) and (S[I] <= ' ') do Inc(I);
  if I > L then Result := '' else
  begin
    while S[L] <= ' ' do Dec(L);
    if (I>1) or (L<Length(S)) then
      Result := Copy(S, I, L - I + 1)
    else
      Result := S;
  end;
end;
{$ENDIF}


function UTrimLeft(const S: UnicodeString): UnicodeString;
{$IFDEF UNICODE}
begin
  Result := TrimLeft(S);
end;
{$ELSE}
var
  I, L: Integer;
begin
  L := Length(S);
  I := 1;
  while (I <= L) and (S[I] <= ' ') do Inc(I);
  if I>1 then
    Result := Copy(S, I, Maxint)
  else
    Result := S;
end;
{$ENDIF}

function UTrimRight(const S: UnicodeString): UnicodeString;
{$IFDEF UNICODE}
begin
  Result := TrimRight(S);
end;
{$ELSE}
var
  I: Integer;
begin
  I := Length(S);
  while (I > 0) and (S[I] <= ' ') do Dec(I);
  if I<Length(S) then
    Result := Copy(S, 1, I)
  else
    Result := S;
end;
{$ENDIF}

function ULowerCase(const S: UnicodeString): UnicodeString;
{$IFDEF UNICODE}
begin
  Result := lowercase(S);
end;
{$ELSE}
begin
  Result := WideLowerCase(S);
end;
{$ENDIF}

function UUpperCase(const S: UnicodeString): UnicodeString;
{$IFDEF UNICODE}
begin
  Result := uppercase(S);
end;
{$ELSE}
begin
  Result := WideUpperCase(S);
end;
{$ENDIF}


{
HexToUnicode() is used in RomajiToKana() so it can be a bottleneck.
We'll try to do it fast.
}

//Increments a valid PChar pointer 4 characters or returns false if the string ends before that
function EatOneFChar(var pc: PAnsiChar): boolean;
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

function EatOneFCharW(var pc: PWideChar): boolean;
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

function IsHexChar(wc: WideChar): boolean;
begin
  Result := ((wc>='a') and (wc<='f'))
    or ((wc>='A') and (wc<='F'))
    or ((wc>='0') and (wc<='9'));
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
    Dec(maxlen,4);
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

function BytesToHex(pb:PByte;sz:integer):string;
const HexChars: string = '0123456789ABCDEF';
var i:integer;
begin
  if pb=nil then begin
    Result := '';
    exit;
  end;

  SetLength(Result, sz*2);
  for i := 0 to sz - 1 do begin
    Result[i*2+1] := HexChars[1 + PByte(pb)^ shr 4];
    Result[i*2+2] := HexChars[1 + PByte(pb)^ and $0F];
    Inc(pb);
  end;
end;

{
This is NOT equiualent to BytesToHex(s, Length(s)*2).
We're translating text in CHARS which are two-byte. I.e.
  BytesToHex:   01 02 03 04 05 06 07 08 09 10...
  UnicodeToHex: 02 01 04 03 06 05 08 07 10 09...
}
function UnicodeToHex(pc:PWideChar; len: integer):string;
const HexChars: string = '0123456789ABCDEF';
var i:integer;
begin
  if pc=nil then begin
    Result := '';
    exit;
  end;

  SetLength(Result, len*4);
  for i := 0 to len - 1 do begin
    Result[i*4+1] := HexChars[1 + PWord(pc)^ shr 12];
    Result[i*4+2] := HexChars[1 + (PWord(pc)^ shr 8) and $0F];
    Result[i*4+3] := HexChars[1 + (PWord(pc)^ shr 4) and $0F];
    Result[i*4+4] := HexChars[1 + PWord(pc)^ and $0F];
    Inc(pc);
  end;
end;

function UnicodeToHex(const s:UnicodeString):string;
begin
  Result := UnicodeToHex(PWideChar(s), Length(s));
end;

function AnsiToHex(const s:AnsiString):string;
begin
  Result := BytesToHex(PByte(s), Length(s));
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


//If there's more than cnt separators, the rest of the string is added
//to the last part
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
function SplitStr(s: string; cnt: integer; ch:char): TStringArray;
var i:integer;
begin
  SetLength(Result, cnt);
  i:=0;
  while i<cnt do
  begin
    if pos(ch,s)>0 then
    begin
      Result[i] := copy(s,1,pos(ch,s)-1);
      delete(s,1,pos(ch,s));
    end else
    begin
      Result[i] := s;
      s:='';
    end;
    inc(i);
  end;
end;

//Same but does not use fixed number of parts
function SplitStr(s: string; ch: char=','): TStringArray;
var i: integer;
begin
  SetLength(Result, 0);
  i := pos(s, ch);
  while i>0 do begin
    SetLength(Result, Length(Result)+1);
    Result[Length(Result)-1] := copy(s, 1, i-1);
    delete(s, 1, i);
    i := pos(s, ch);
  end;
  if s<>'' then begin
    SetLength(Result, Length(Result)+1);
    Result[Length(Result)-1] := s;
  end;
end;

procedure StrListAdd(sl: TStringList; sa: TStringArray);
var i: integer;
begin
  for i := 0 to Length(sa) - 1 do
    sl.Add(sa[i]);
end;

{ Removes all Wakan marks (unknown kanji, field type, word type etc) from the string }
function remexcl(const s:string):string;
begin
{
 I have three versions of this code and they all look different!

  if (length(s)>1) and (s[2]=ALTCH_EXCL) then delete(s,2,2);
  if (length(s)>1) and (s[1]=ALTCH_EXCL) then delete(s,1,2);
  if (length(s)>1) and (s[2]=UH_UNKNOWN_KANJI) then delete(s,2,1);
  if (length(s)>1) and (s[1]=UH_UNKNOWN_KANJI) then delete(s,1,1);
  if (length(s)>1) and (s[1]=ALTCH_TILDE) then delete(s,1,2);
  result:=s;

  if (length(s1)>0) and (s1[1]=ALTCH_EXCL) then delete(s1,1,2);
  if (length(s2)>0) and (s2[1]=ALTCH_EXCL) then delete(s2,1,2);
  if (length(s1)>0) and (s1[1]=UH_UNKNOWN_KANJI) then delete(s1,1,1);
  if (length(s2)>0) and (s2[1]=UH_UNKNOWN_KANJI) then delete(s2,1,1);

  if (length(s)>1) and (s[1]=ALTCH_EXCL) then delete(s,1,2);
  if (length(s)>2) and (s[2]=ALTCH_EXCL) then delete(s,2,2);
  if (length(s)>1) and (s[1]=UH_DRAWWORD_KANA) then delete(s,1,1);
  if (length(s)>1) and (s[1]=UH_DRAWWORD_KANJI) then delete(s,1,1);
  if (length(s)>0) and (s[1]=UH_UNKNOWN_KANJI) then delete(s,1,1);

 This is just crazy. I'm going to write something reasonable now and pray it works.
}

 { All marks are at the beginning of the string }
  Result := s;
  while Length(Result)>0 do begin
    if Result[1]=ALTCH_EXCL then delete(Result,1,2) else
    if Result[1]=UH_DRAWWORD_KANA then delete(Result,1,1) else
    if Result[1]=UH_DRAWWORD_KANJI then delete(Result,1,1) else
    if Result[1]=UH_UNKNOWN_KANJI then delete(Result,1,1) else
    if Result[1]=ALTCH_TILDE then delete(Result,1,2) else
      break;
  end;
end;

{ Removes all internal Wakan markers (<dEDICT> <gabbr> etc) from the string }
function remmark(s:string):string;
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

function repl(var s:string;const sub,rep:string):string;
var i_pos: integer;
begin
  i_pos := pos(sub,s);
  while i_pos>0 do begin
    s:=copy(s,1,i_pos-1)+rep+copy(s,i_pos+length(sub),length(s)-i_pos+1-length(sub));
    i_pos := pos(sub,s);
  end;
  result:=s;
end;

function urepl(var s:UnicodeString;const sub,rep:UnicodeString):UnicodeString;
{$IFDEF UNICODE}
begin
  Result := repl(s,sub,rep);
end;
{$ELSE}
var i_pos: integer;
begin
  i_pos := upos(sub,s);
  while i_pos>0 do begin
    s:=copy(s,1,i_pos-1)+rep+copy(s,i_pos+length(sub),length(s)-i_pos+1-length(sub));
    i_pos := upos(sub,s);
  end;
  result:=s;
end;
{$ENDIF}

function replc(const s:string;const sub,rep:string):string;
begin
  Result := s;
  repl(Result,sub,rep);
end;

function ureplc(const s:UnicodeString;const sub,rep:UnicodeString):UnicodeString;
begin
  Result := s;
  urepl(Result,sub,rep);
end;

{ Returns part of the string from the start to the first occurence of "c",
 and cuts that part, up to and including "c".
 If no "c" is found, returns the remainder of the string. }
function strqpop(var s:string;c:char):string;
var i: integer;
begin
  i := pos(c,s);
  if i=0 then
  begin
    Result:=s;
    s:='';
  end else
  begin
    Result:=copy(s,1,i-1);
    delete(s,1,i);
  end;
end;

function strqpop(var s:string;const cs:string):string; overload;
var i: integer;
begin
  i := pos(cs,s);
  if i=0 then
  begin
    Result:=s;
    s:='';
  end else
  begin
    Result:=copy(s,1,i-1);
    delete(s,1,i+Length(cs)-1);
  end;
end;

//Inefficient, so use strqpop where available
function ustrqpop(var s:UnicodeString;c:WideChar):UnicodeString;
{$IFDEF UNICODE}
begin
  Result := strqpop(s,c);
end;
{$ELSE}
var i: integer;
begin
  i := upos(c,s);
  if i=0 then
  begin
    Result:=s;
    s:='';
  end else
  begin
    Result:=copy(s,1,i-1);
    delete(s,1,i);
  end;
end;
{$ENDIF}

{ Makes the string fixed length -- cuts if it's too long or pads if it's too short }
procedure MakeFixedLen(var s: AnsiString; len: integer; pad_ch: AnsiChar);
var old_len: integer;
begin
  old_len := length(s);
  if old_len<>len then SetLength(s,len);
  while old_len<len do begin
    s[old_len+1] := pad_ch;
    Inc(old_len);
  end;
end;

procedure MakeFixedLen(var s: UnicodeString; len: integer; pad_ch: WideChar);
var old_len: integer;
begin
  old_len := length(s);
  if old_len<>len then SetLength(s,len);
  while old_len<len do begin
    s[old_len+1] := pad_ch;
    Inc(old_len);
  end;
end;


{ Filename list }

procedure AddFilename(var list: TFilenameList; const filename: string);
begin
  SetLength(list, Length(list)+1);
  List[Length(list)-1] := lowercase(filename);
end;

function MakeFilenameList(const filenames: array of string): TFilenameList;
var i,l: integer;
begin
  SetLength(Result, Length(filenames));
  l := Low(filenames);
  for i := l to High(filenames) do
    Result[i-l] := filenames[i];
end;

function IsFileInList(const filename: string;const list: TFilenameList): boolean;
var l_filename: string;
  i: integer;
begin
  Result := false;
  l_filename := lowercase(filename);
  for i := 0 to Length(list)-1 do
    if lowercase(list[i])=l_filename then begin
      Result := true;
      break;
    end;
end;

function FilenameListToString(const list: TFilenameList; const sep: string = ', '): string;
var i: integer;
begin
  if Length(list)<=0 then begin
    Result := '';
    exit;
  end;

  Result := list[0];
  for i := 1 to Length(list) - 1 do
    Result := Result + sep + list[i];
end;


{ Character processing }

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
  if {(ch>=$0000) and} (ch<=$007F) then result:=EC_LATIN_HW else //first part always true
  result:=EC_UNKNOWN;
end;

{$IFDEF UNICODE}
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

function EvalChars(const chars:FString):integer;
var i: integer;
begin
  Result := 0;
  for i := 1 to flength(chars) do
    Result := Result or (1 shl EvalChar(fgetch(chars,i)));
end;

function IsUpcaseLatin(ch: AnsiChar): boolean;
begin
  Result := ch in ['A'..'Z'];
end;

function IsUpcaseLatin(ch: WideChar): boolean;
begin
  Result := (Word(ch) and $FF00 = 0) and (AnsiChar(ch) in ['A'..'Z']);
end;

function IsLocaseLatin(ch: AnsiChar): boolean;
begin
  Result := ch in ['a'..'z'];
end;

function IsLocaseLatin(ch: WideChar): boolean;
begin
  Result := (Word(ch) and $FF00 = 0) and (AnsiChar(ch) in ['a'..'z']);
end;

function LoCase(ch: AnsiChar): AnsiChar;
begin
  Result := Ch;
  if Result in ['A'..'Z'] then
    Inc(Result, Ord('a')-Ord('A'));
end;

function LoCase(Ch: WideChar): WideChar;
begin
  Result := Ch;
  case Ch of
    'A'..'Z':
      Result := WideChar(Word(Ch) or $0020);
  end;
end;

function IsDigit(ch: AnsiChar): boolean;
begin
  Result := (Ord(ch)>=Ord('0')) and (Ord(ch)<=Ord('9'));
end;

function IsDigit(ch: WideChar): boolean;
begin
  Result := (Ord(ch)>=Ord('0')) and (Ord(ch)<=Ord('9'));
end;

//Ansi/Default version
function IsLatinLetter(c:char): boolean;
begin
  Result := ((c>='a') and (c<='z')) or ((c>='A') and (c<='Z'));
end;

//Strict Wide
function IsLatinLetterW(c:WideChar): boolean;
begin
  Result := ((c>='a') and (c<='z')) or ((c>='A') and (c<='Z'));
end;

//FChar
function IsLatinLetterF(c:FChar): boolean; {$IFDEF INLINE}inline;{$ENDIF}
begin
{$IFDEF UNICODE}
  Result := IsLatinLetterW(c);
{$ELSE}
  Result := ((c>='0041') and (c<='005A')) or ((c>='0061') and (c<='007A'));
{$ENDIF}
end;

function IsHalfWidthChar(c:FChar):boolean;
begin
 {$IFNDEF UNICODE}
  Result:=(c[1]='0') and (c[2]='0');
 {$ELSE}
  Result := (Word(c) and $FF00 = 0);
 {$ENDIF}
end;

function IsLatinDigit(c:char):boolean;
begin
  Result := (Ord(c)>=Ord('0')) and (Ord(c)<=Ord('9'));
end;

function IsLatinDigitF(c:FChar):boolean;
begin
 {$IFDEF UNICODE}
  Result := (Ord(c)>=Ord('0')) and (Ord(c)<=Ord('9'));
 {$ELSE}
  Result := (c>='0030') and (c<='0039');
 {$ENDIF}
end;

{ True if a specified kana character is katakana. Doesn't check if it is kana to begin with. }
function IsKanaCharKatakana(c:FString; i:integer): boolean;
begin
 {$IFDEF UNICODE}
  Result := (Length(c)>=i) and ((ord(c[i]) and $00F0)>$A0);
 {$ELSE}
  i := (i-1)*4+3;
  Result := (Length(c)>i) and (c[i]>='A');
 {$ENDIF}
end;

//True if a char is a doubling character from any kana
function IsSokuon(const c: FChar): boolean;
begin
 {$IFDEF UNICODE}
  Result := (c='ッ') or (c='っ');
 {$ELSE}
  Result := (c='30C3') or (c='3063');
 {$ENDIF}
end;

function IsSmallKanaVowel(const c: FChar): boolean;
begin
 {$IFDEF UNICODE}
  Result :=
       (c='ぁ') or (c='ぃ') or (c='ぅ') or (c='ぇ') or (c='ぉ') or (c='ゃ') or (c='ゅ') or (c='ょ')
    or (c='ァ') or (c='ィ') or (c='ゥ') or (c='ェ') or (c='ォ') or (c='ャ') or (c='ュ') or (c='ョ');
 {$ELSE}
  Result :=
       (c='3041') or (c='3043') or (c='3045') or (c='3047') or (c='3049') or (c='3083') or (c='3085') or (c='3087')
    or (c='30A1') or (c='30A3') or (c='30A5') or (c='30A7') or (c='30A9') or (c='30E3') or (c='30E5') or (c='30E7');
 {$ENDIF}
end;

end.
