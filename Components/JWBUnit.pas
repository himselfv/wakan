unit JWBUnit;
{ Common stuff for Wakan. }

interface
uses Graphics, Windows, SysUtils, Classes, Dialogs, Grids, Forms, ExtCtrls, Registry,
  JWBStrings, JWBUtils, JWBKanaConv;

{ Misc }

var
  AppFilename: string = '';
  AppFolder: string = ''; //path to program, set on load
  WakanVer: string = ''; //taken from resources on load

const
  CurStructVer=2;
  CurDictVer=7;

type
  TPortabilityMode = (pmStandalone, pmPortable, pmCompatible);

const
  WakanAppName = 'WaKan - Japanese & Chinese Learning Tool';
  WakanCopyright = '(C) Filip Kabrt and others 2002-2013';
  WakanRegKey = 'Software\Labyrinth\Wakan';

  UserDataDir: string = '';
  DictionaryDir: string = '';
  PortabilityMode: TPortabilityMode = pmCompatible;

procedure SetPortabilityMode(AMode: TPortabilityMode);
function GetAppDataFolder: string;


{ Some logging tools.
Define NOLOG to make sure that nothing in the application calls these. }
//{$DEFINE NOLOG}
{$IFNDEF NOLOG}
procedure Log(const msg: string); overload; {$IFNDEF DEBUG}inline;{$ENDIF} //inline in debug so that it's completely eliminated
procedure Log(const msg: string; args: array of const); overload;
procedure DumpHdc(const h: HDC; const r: TRect; const pref: string='hdc-'); {$IFNDEF DEBUG}inline;{$ENDIF}
{$ENDIF}


{ Romaji conversions }
{
Katakana and romaji in dictionaries can contain latin letters and punctuation.
In general, it is safe to mix latin letters with katakana/bopomofo, but not with
romaji (i.e. is "ding" an english word or a pinyin syllable?)
Dictionaries address this in different ways: EDICT stores in katakana,
CCEDICT separates letters with spaces ("D N A jian4 ding4").

Internally we support:
- kana/bopomofo with latin letters + punctuation
- clean romaji (can be converted back to kana)
 Since clean romaji is still ANSI, we replace some punctuation with ANSI chars.
- romaji signature ("DNAjian4ding4" -- cannot be converted back to anything)
 Signature romaji is used for database lookups. It's stripped of punctuation,
but keeps latin letters. This allows us to quickly find all possible matches
instead of trying different interpretations of the input.
If you just want to display romaji for the record, do KanaToRomaji(kana) instead.

So,
1. On import we fail on unknown characters, otherwise convert text to database
 format per above:
  - kanji + latin + punctuation
  - kana + latin + punctuation
  - romaji + latin
2. On kanji/kana input we strip unknown characters, then do a lookup.
3. On romaji input we strip unknown characters and punctuation, then do a lookup.
4. On conversions from kana to romaji for display we only strip unknown characters.
}

//See JWBKanaConv.ResolveFlag

var
 { Romaji translation table. Populated on load.
  See comments where class is defined. }
  roma_t: TRomajiTranslator;

 { Chinese version, not upcased. Someone upgrade this one too... }
  romac: TStringList;

function KanaToRomaji(const s:FString;romatype:integer;lang:char):string; overload; inline;
function KanaToRomaji(const s:FString;romatype:integer;lang:char;flags:TResolveFlags):string; overload;
function RomajiToKana(const s:string;romatype:integer;lang:char;flags:TResolveFlags):FString;

//Packs and cleans any romaji (perhaps coming from user) to signature format:
//no punctuation, no spaces, no unknown chars, lowercase.
//This is NOT ALL that's required from signature in dict, but other conditions require
//knowing the syllable composition and handled on import before calling this.
function SignatureFrom(const s:string): string;


//Pin3yin4<->pi'nyi^n
function ConvertPinYin(const str:string):FString;
function DeconvertPinYin(const str:FString):string;

{ WordGrid }

procedure InitWordGrid(grid:TStringGrid;stat,learn:boolean);
procedure AddWordGrid(grid:TStringGrid;sp1,sp2,sp4,sp3:string);
procedure FinishWordGrid(grid:TStringGrid);
procedure FillWordGrid(grid:TStringGrid;sl:TStringList;stat,learn:boolean);


{ Colors }

procedure InitColors;
function GetColorString(i:integer):string;
procedure SetCol(col:integer;val:TColor);
function GetCol(col:integer):TColor;
function Col(col:string):TColor;
procedure WriteColors;
procedure SetColDefault(i:integer);


{ Painting }

type
  TFindDrawRegFlag = (
    ffHalfCharRounding  //if more than half char is covered, return next char --
      //like it's usually done with text selection.
  );
  TFindDrawRegFlags = set of TFindDrawRegFlag;

procedure BeginDrawReg(p:TPaintBox);
procedure EndDrawReg;
function FindDrawReg(p:TPaintBox;x,y:integer;flags:TFindDrawRegFlags;out id,cx,cy,fs:integer):FString;
function GetDrawReg(id:integer;out cx,cy,fs:integer):FString;

function PaintBoxUpdateSelection(p:TPaintBox;DragStart,CursorPos:TPoint):FString;
function DrawGridUpdateSelection(p:TCustomDrawGrid;DragStart,CursorPos:TPoint):FString;

procedure DrawStrokeOrder(canvas:TCanvas;x,y,w,h:integer;char:string;fontsize:integer;color:TColor);
procedure DrawUnicode(c:TCanvas;x,y,fs:integer;ch:FString;fontface:string);
procedure DrawKana(c:TCanvas;x,y,fs:integer;ch:string;fontface:string;showr:boolean;romas:integer;lang:char);
function DrawWordInfo(canvas:TCanvas; Rect:TRect; sel,titrow:boolean; colx:integer; s:string; multiline,onlycount:boolean;fontsize:integer;boldfont:boolean):integer;
procedure DrawPackedWordInfo(canvas: TCanvas; Rect:TRect; s:FString; ch:integer;boldfont:boolean);
procedure DrawWordCell(Grid:TStringGrid; ACol, ARow: Integer; Rect: TRect; State: TGridDrawState);

procedure PaintSelectionHighlight(canv: TCanvas=nil; in_rect: PRect=nil);
procedure SetSelectionHighlight(x1,y1,x2,y2:integer;canvas:TCanvas);


{ Rest }

function StateStr(i:integer):string;
function DateForm(s:string):string;
procedure SplitWord(s:string; var sp1,sp2,sp4,sp3:string);
function ChinTo(s:FString):FString;
function ChinFrom(s:FString):FString;

{ Upgrades vocabulary entry -- see implementation comments }
function FixVocabEntry(const s:string):string;
function UnfixVocabEntry(const s:string):string;

procedure DeleteDirectory(dir:string);
procedure Backup(const filename: string);

procedure RegeditAtKey(const key: string);


var
 //Fonts
  FontStrokeOrder,
  FontChinese,
  FontChineseGB,
  FontChineseGrid,
  FontChineseGridGB,
  FontJapaneseGrid,
  FontJapanese,
  FontSmall,
  FontRadical,
  FontEnglish,
  FontPinYin:string;

  GridFontSize:integer;


{ Translation }

function _l(const id:string):string; overload;
function _l(const id:string; args: array of const):string; overload;

implementation
uses Messages, StrUtils, ShlObj, JWBMenu, JWBSettings, JWBLanguage, TextTable;


{ Portable/standalone }

function GetAppDataFolder: string;
begin
  Result := GetSpecialFolderPath(CSIDL_APPDATA);
 //There's also CSIDL_LOCAL_APPDATA which might do if CSIDL_APPDATA is somehow not available.
 //But I don't think that can be the case!
  Assert(Result<>''); //just in case
  Result:=Result+'\Wakan';
  ForceDirectories(Result);
end;

procedure SetPortabilityMode(AMode: TPortabilityMode);
begin
  PortabilityMode := AMode;
  case AMode of
    pmStandalone: UserDataDir := GetAppDataFolder;
    pmPortable: UserDataDir := AppFolder;
    pmCompatible: UserDataDir := AppFolder;
  end;
 //Dictionaries are always stored in application folder
  DictionaryDir := AppFolder;
end;


{ Log }
{$IFNDEF NOLOG}

{$IFDEF DEBUG}
const
  BOM_UTF16BE: AnsiString = #254#255; //FE FF
  BOM_UTF16LE: AnsiString = #255#254; //FF FE
 //must be ansi or we'll get two unicode characters

function ForceFilePath(const Filename: string): boolean;
var Path: string;
begin
  Path := ExtractFilePath(Filename);
  Result := (Path='') {current dir always exists} or ForceDirectories(Path);
end;

{ Writes to UTF16BE, adding BOM to new file.
 Better employ critical sections if you use it from multiple threads. }
procedure WriteFileWithBom(const Filename: string; data: pointer; sz: cardinal);
var HFile: THandle;
  dwBytesWritten: cardinal;
begin
// if not ForceFilePath(Filename) then exit; --- do not waste time at first, try only if file/path is not found

 //Try open or create a file
  HFile := CreateFile(PChar(Filename), GENERIC_WRITE,
    FILE_SHARE_READ, nil, OPEN_ALWAYS, 0, 0);

 //The only problem we know how to solve is "path not found"
  if HFile = INVALID_HANDLE_VALUE then begin
    if GetLastError() <> ERROR_PATH_NOT_FOUND then exit;

   //Create directories and try again
    if not ForceFilePath(Filename) then exit;

    HFile := CreateFile(PChar(Filename), GENERIC_WRITE,
      FILE_SHARE_READ, nil, OPEN_ALWAYS, 0, 0);
    if HFile = INVALID_HANDLE_VALUE then exit;
  end;

 //If the file was present, scroll to the end
  if GetLastError() = ERROR_ALREADY_EXISTS then
    SetFilePointer(HFile, 0, nil, FILE_END)
  else //Write BOM
    WriteFile(HFile, BOM_UTF16LE[1], Length(BOM_UTF16LE), dwBytesWritten, nil);
 {Can be optimized: write BOM+first message at once. But the gain is too low.}

 //Anyway write the data
  WriteFile(HFile, data^, sz, dwBytesWritten, nil);
  CloseHandle(HFile);
end;
{$ENDIF}

procedure Log(const msg: string);
{$IFDEF DEBUG}
var tmp: string;
begin
  tmp:=msg+#13#10;
  WriteFileWithBom('wakan.log', @tmp[1], Length(tmp)*SizeOf(char));
end;
{$ELSE}
begin
end;
{$ENDIF}

procedure Log(const msg: string; args: array of const); overload;
begin
{$IFDEF DEBUG}
  Log(Format(msg,args));
{$ENDIF}
end;

var
  DumpId: integer = 0;

{ Outputs the contents of a canvas to a file.
 Files are named sequentially and recorded in log so that you know which goes where. }
procedure DumpHdc(const h: HDC; const r: TRect; const pref: string);
{$IFDEF DEBUG}
var bmp: Graphics.TBitmap;
  fname: string;
begin
  if (r.Right-r.Left<=0) or (r.Bottom-r.Top<=0) then exit;

  bmp := Graphics.TBitmap.Create;
  try
    bmp.PixelFormat := pf32bit;
    bmp.SetSize(r.Right-r.Left,r.Bottom-r.Top);
    if not BitBlt(bmp.Canvas.Handle,0,0,r.Right-r.Left,r.Bottom-r.Top,h,r.Left,r.Top,SRCCOPY) then
      RaiseLastOsError();
    inc(DumpId);
    fname := 'dump-'+IntToStr(GetTickCount)+'-'+IntToStr(DumpId)+'-'+pref+IntToHex(integer(h),8)+'.bmp';
    bmp.SaveToFile(fname);
    Log('Dumped: '+fname);
  finally
    FreeAndNil(bmp);
  end;
end;
{$ELSE}
begin
end;
{$ENDIF}

{$ENDIF} //IFNDEF NOLOG

{ Romaji conversions }

type
  EvilString = string; //contains FString if we convert uni->pinyin, string otherwise

//Index of romac entry for this item or -1
//Text must be uppercased
function FindRomaC(const find: EvilString; roma_type: integer; partial: boolean): integer;
var i: integer;
begin
  Result := -1;
  for i:=0 to (romac.count div 4)-1 do
    if (partial and (pos(find,romac[i*4+roma_type])=1))
    or ((not partial) and (romac[i*4+roma_type]=find)) then begin
      Result := i;
      break;
    end;
end;

//Finds best roma_c match at the start of the string
function FindRomaCBestMatch(const s: EvilString; roma_type: integer): integer;
var i, cl: integer;
begin
  Result := -1;
  cl := 0;
  for i:=0 to (romac.count div 4)-1 do
    if pos(romac[i*4+roma_type],s)=1 then
      if length(romac[i*4+roma_type])>cl then
      begin
        cl:=length(romac[i*4+roma_type]);
        Result:=i;
      end;
end;


{
Converts chinese string between:
  0 - bopomofo (fstring)
  1 - pinyin
  2 - wade-giles
  3 - yale
TypeIn: conversion source
TypeOut: conversion target type
Clean: ignore unknown characters instead of adding '?'
}

function ResolveCrom(s:EvilString;typein,typeout:integer;flags:TResolveFlags):EvilString;
var s2:string;
  cl:integer;
  i:integer;
  ch:WideChar;
begin
  s:=uppercase(s);
  s2:='';
  while s<>'' do
  begin
    //Find longest match for character sequence starting at this point
    i := FindRomaCBestMatch(s,typein);
    if i>=0 then
      cl := Length(romac[i*4+typein])
    else
      cl := 0;
   { We'd like to try other romanization systemss if the specified one failed us,
    but we cannot.
    Most of the times, when there's no "correct" match, there would still be
    *some* match (i.e. KE instead of KEI).
    And we can't just "select best match from all the romanizations", that would
    be dangerous. }

    if i>=0 then begin
      s2:=s2+romac[i*4+typeout];
      delete(s,1,cl);

     //with ansi pinyin, we only try to extract tone after a syllable match
      if typein>0 then
        if (length(s)>0) and (s[1]>='0') and (s[1]<='5') then
        begin
          if typeout>0 then
            s2:=s2+s[1]
          else
            s2:=s2+fpytone(Ord(s[1])-Ord('0')); //from digit
          delete(s,1,1);
        end else
        begin
          if typeout>0 then s2:=s2+'0' else s2:=s2+UH_PY_TONE;
        end;
    end
    else begin
      if typein>0 then begin
        ch:=s[1];
        delete(s,1,1)
      end else begin
        ch:=fstrtouni(fgetch(s,1))[1];
        fdelete(s,1,1);
      end;

      if (rfConvertLatin in flags) and IsLatinLetterW(ch) then
        s2:=s2+ch
      else
      if (rfConvertPunctuation in flags) and IsAllowedPunctuation(ch) then
        s2:=s2+ConvertPunctuation(ch)
      else

      if not (rfDeleteInvalidChars in flags) then
     {$IFDEF UNICODE}
        s2 := s2 + '?'; //in unicode both strings are in native form
     {$ELSE}
        if typeout>0 then s2:=s2+'?' else s2:=s2+'003F';
     {$ENDIF}
    end;

   //With bopomofo, we extract tones always, as they are in special characters
    if typein=0 then
      if fpygettone(s) in [0..5] then begin
        s2 := s2 + Chr(Ord('0') + fpyextrtone(s)); //to digit
      end;
  end;
  if typeout>0 then result:=lowercase(s2) else result:=s2;
end;

{ Converts kana to romaji in a default way, that is assuming the input is a
 kana from database and the output is for user display.
 Equivalent to older "KanaToRomaji(clean)". }
function KanaToRomaji(const s:FString;romatype:integer;lang:char):string;
begin
  Result:=KanaToRomaji(s,romatype,lang,[rfConvertLatin,rfConvertPunctuation,rfDeleteInvalidChars])
end;

//Converts kana to romaji, per flags
function KanaToRomaji(const s:FString;romatype:integer;lang:char;flags:TResolveFlags):string;
begin
  if lang='j'then
    Result:=roma_t.KanaToRomaji(s,romatype,flags)
  else
  if lang='c'then
    result:=ResolveCrom(s,0,romatype,flags);
end;

{ Converts romaji to kana. Romaji must be a clean-romaji, i.e. no latin letters,
 no punctuation. Appropriate keep flags have no effect.
 All in all, romaji-to-kana conversion ought to happen only on import, therefore
 no flagless version. }
function RomajiToKana(const s:string;romatype:integer;lang:char;flags:TResolveFlags):FString;
var s_rep: string;
begin
  if lang='j'then
    Result := roma_t.RomajiToKana(s,romatype,flags)
  else
  if lang='c'then
  begin
    s_rep := s;
    repl(s_rep,'v','u:');
    result:=ResolveCrom(s_rep,romatype,0,flags);
  end;
end;

function SignatureFrom(const s:string): string;
var i,j: integer;
begin
  Result:=lowercase(s);
  i:=1;
  j:=0;
  while i+j<=length(Result) do
    if not IsLatinLetter(Result[i+j])
    and not IsLatinDigit(Result[i+j]) then
      Inc(j)
    else begin
      if j>0 then
        Result[i]:=Result[i+j];
      Inc(i);
    end;
  if j>0 then
    SetLength(Result,length(Result)-j);
end;

{
Converts raw database pinyin:
  jing4 xiang3
To enhanced unicode pinyin with marks:
  ji'ng xia^ng (only merged)
}
function ConvertPinYin(const str:string):FString;
const UH_DUMMY_CHAR:FChar = {$IFNDEF UNICODE}'XXXX'{$ELSE}#$F8F0{$ENDIF}; //used in place of a char when it's unknown or whatever
 //does not go out of this function
var cnv:string;
  li:integer;
  ali:FString;
  cnv2:FString;
  cc:char;
  i:integer;
  iscomma:boolean;
begin
  cnv:=lowercase(str); //source string, not modified
  cnv2:=''; //building output here
  li:=0;
  ali:='';
  iscomma:=false;
  for i:=1 to length(cnv) do
  begin
    if (li=0) and ((cnv[i]='a') or (cnv[i]='e') or (cnv[i]='o') or (cnv[i]='u') or (cnv[i]='i')) then li:=i;
    if (li<i) and ((cnv[li]='i') or (cnv[li]='u') or (cnv[li]='ь')) and
      ((cnv[i]='a') or (cnv[i]='e') or (cnv[i]='o') or (cnv[i]='u') or (cnv[i]='i')) then li:=i;
    if (cnv[i]>='0') and (cnv[i]<='5') and (li>0) then
    begin
      cc:=cnv[li]; //the character to be replaced
      ali:=fcopy(cnv2,length(cnv2)-(i-li-1)+1,i-li-1); //copy the rest of the output
      fdelete(cnv2,length(cnv2)-(i-li)+1,i-li); //delete char + rest from the output
      if iscomma and (cc='u') then cc:='w';
      case cnv[i] of
        '2':case cc of
              'a':cnv2:=cnv2+{$IFDEF UNICODE}#$00E1{$ELSE}'00E1'{$ENDIF};
              'e':cnv2:=cnv2+{$IFDEF UNICODE}#$00E9{$ELSE}'00E9'{$ENDIF};
              'i':cnv2:=cnv2+{$IFDEF UNICODE}#$00ED{$ELSE}'00ED'{$ENDIF};
              'o':cnv2:=cnv2+{$IFDEF UNICODE}#$00F3{$ELSE}'00F3'{$ENDIF};
              'u':cnv2:=cnv2+{$IFDEF UNICODE}#$00FA{$ELSE}'00FA'{$ENDIF};
              'w':cnv2:=cnv2+{$IFDEF UNICODE}#$01D8{$ELSE}'01D8'{$ENDIF};
            end;
        '4':case cc of
              'a':cnv2:=cnv2+{$IFDEF UNICODE}#$00E0{$ELSE}'00E0'{$ENDIF};
              'e':cnv2:=cnv2+{$IFDEF UNICODE}#$00E8{$ELSE}'00E8'{$ENDIF};
              'i':cnv2:=cnv2+{$IFDEF UNICODE}#$00EC{$ELSE}'00EC'{$ENDIF};
              'o':cnv2:=cnv2+{$IFDEF UNICODE}#$00F2{$ELSE}'00F2'{$ENDIF};
              'u':cnv2:=cnv2+{$IFDEF UNICODE}#$00F9{$ELSE}'00F9'{$ENDIF};
              'w':cnv2:=cnv2+{$IFDEF UNICODE}#$01DC{$ELSE}'01DC'{$ENDIF};
            end;
        '1':case cc of
              'a':cnv2:=cnv2+{$IFDEF UNICODE}#$0101{$ELSE}'0101'{$ENDIF};
              'e':cnv2:=cnv2+{$IFDEF UNICODE}#$0113{$ELSE}'0113'{$ENDIF};
              'i':cnv2:=cnv2+{$IFDEF UNICODE}#$012B{$ELSE}'012B'{$ENDIF};
              'o':cnv2:=cnv2+{$IFDEF UNICODE}#$014D{$ELSE}'014D'{$ENDIF};
              'u':cnv2:=cnv2+{$IFDEF UNICODE}#$016B{$ELSE}'016B'{$ENDIF};
              'w':cnv2:=cnv2+{$IFDEF UNICODE}#$01D6{$ELSE}'01D6'{$ENDIF};
            end;
        '3':case cc of
              'a':cnv2:=cnv2+{$IFDEF UNICODE}#$01CE{$ELSE}'01CE'{$ENDIF};
              'e':cnv2:=cnv2+{$IFDEF UNICODE}#$011B{$ELSE}'011B'{$ENDIF};
              'i':cnv2:=cnv2+{$IFDEF UNICODE}#$01D0{$ELSE}'01D0'{$ENDIF};
              'o':cnv2:=cnv2+{$IFDEF UNICODE}#$01D2{$ELSE}'01D2'{$ENDIF};
              'u':cnv2:=cnv2+{$IFDEF UNICODE}#$01D4{$ELSE}'01D4'{$ENDIF};
              'w':cnv2:=cnv2+{$IFDEF UNICODE}#$01DA{$ELSE}'01DA'{$ENDIF};
            end;
      end;
      li:=0;
      if (cnv[i]='0') or (cnv[i]='5') then
        if cc='w'then
          cnv2:=cnv2+{$IFDEF UNICODE}#$00FC{$ELSE}'00FC'{$ENDIF}
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
  i := fpos(UH_DUMMY_CHAR,cnv2);
  while i>0 do begin
    fdelete(cnv2,i,1);
    i := fpos(UH_DUMMY_CHAR,cnv2);
  end;
  Result := cnv2;
end;


{
Converts tonemark-enhanced pinyin back into ansi pinyin.
Nobody use this. Currently it's only employed by ImportVocab.
Implemented only in Unicode. This is slower on Ansi, but FStrings are deprecated anyway.
}
//TODO: Test Unicode conversion
function DeconvertPinYin(const str:FString):string; deprecated;
var cnv:UnicodeString;
  curs:UnicodeString; //although by logic it ought to be a Char...
  nch:string;
    cnv2:string;
    i,j:integer;
    curcc:string;
    curp,curpx:char;
    mustbegin,mustnotbegin,befmustnotbegin,befbefmustnotbegin:boolean;
    number:boolean;
    putcomma,fnd:boolean;
    cc:char;
begin
  cnv:=fstrtouni(str);

  putcomma:=false;
  cnv2:='';
  for i:=1 to length(cnv) do
  begin
    curs:=cnv[i];
    if putcomma and (curs<>'e') then begin curs:=':'+curs; putcomma:=false; end;
    if curs=#$01D6 then begin putcomma:=true; curs:=#$016B; end;
    if curs=#$01D8 then begin putcomma:=true; curs:=#$00FA; end;
    if curs=#$01DA then begin putcomma:=true; curs:=#$01D4; end;
    if curs=#$01DC then begin putcomma:=true; curs:=#$00F9; end;
    if curs=#$00FC then begin putcomma:=true; curs:=#$0075; end;
    if curs=#$2026 then curs:='_';
    cnv2:=cnv2+curs;
  end;
  if putcomma then cnv2:=cnv2+':';
  cnv:=cnv2;
  cnv2:='';

  mustbegin:=true;
  mustnotbegin:=false;
  befmustnotbegin:=false;
  number:=false;
  for i:=1 to length(cnv) do
  begin
    curs:=cnv[i];
    cc:=curs[1];
    if (cc>='0') and (cc<='9') then number:=true; //WTF? Maybe we should test that ALL chars are digits, not ANY?
  end;
  if number then
  begin
    result:=fstrtouni(str);
    exit;
  end;

  curp:='0';
  curcc:='';
  for i:=1 to length(cnv) do
  begin
    curs:=cnv[i];
    curpx:='0';
    if Ord(curs[1])<$0080 then cc:=upcase(curs[1])
    else if curs=#$00E1 then begin cc:='A'; curpx:='2'; end
    else if curs=#$00E9 then begin cc:='E'; curpx:='2'; end
    else if curs=#$00ED then begin cc:='I'; curpx:='2'; end
    else if curs=#$00F3 then begin cc:='O'; curpx:='2'; end
    else if curs=#$00FA then begin cc:='U'; curpx:='2'; end
    else if curs=#$00E0 then begin cc:='A'; curpx:='4'; end
    else if curs=#$00E8 then begin cc:='E'; curpx:='4'; end
    else if curs=#$00EC then begin cc:='I'; curpx:='4'; end
    else if curs=#$00F2 then begin cc:='O'; curpx:='4'; end
    else if curs=#$00F9 then begin cc:='U'; curpx:='4'; end
    else if curs=#$0101 then begin cc:='A'; curpx:='1'; end
    else if curs=#$0113 then begin cc:='E'; curpx:='1'; end
    else if curs=#$012B then begin cc:='I'; curpx:='1'; end
    else if curs=#$014D then begin cc:='O'; curpx:='1'; end
    else if curs=#$016B then begin cc:='U'; curpx:='1'; end
    else if curs=#$0103 then begin cc:='A'; curpx:='3'; end
    else if curs=#$0115 then begin cc:='E'; curpx:='3'; end
    else if curs=#$012D then begin cc:='I'; curpx:='3'; end
    else if curs=#$014F then begin cc:='O'; curpx:='3'; end
    else if curs=#$016D then begin cc:='U'; curpx:='3'; end
    else if curs=#$01CE then begin cc:='A'; curpx:='3'; end
    else if curs=#$011B then begin cc:='E'; curpx:='3'; end
    else if curs=#$01D0 then begin cc:='I'; curpx:='3'; end
    else if curs=#$01D2 then begin cc:='O'; curpx:='3'; end
    else if curs=#$01D4 then begin cc:='U'; curpx:='3'; end
    else cc:='?';
    if (((cc>='A') and (cc<='Z')) or (cc=':')) and (cc<>'''') then curcc:=curcc+cc;

    fnd:=FindRomaC(uppercase(curcc),1,{Partial=}true)>=0;
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



var wgcur:integer;

procedure InitWordGrid(grid:TStringGrid;stat,learn:boolean);
begin
  grid.Perform(WM_SETREDRAW, 0, 0);
  grid.DefaultRowHeight:=GridFontSize+2;
  grid.FixedRows:=1;
  grid.Cells[0,0]:=_l('#00939^ePhonetic');
  grid.Cells[1,0]:=_l('#00632^eWritten');
  grid.Cells[2,0]:=_l('#00317^eTranslation');
  if stat then if learn then
    grid.Cells[3,0]:=_l('#00633^eAdded / Learned') else
    grid.Cells[3,0]:=_l('#00634^eCategories');
  wgcur:=1;
end;

procedure AddWordGrid(grid:TStringGrid;sp1,sp2,sp4,sp3:string);
begin
  grid.Cells[0,wgcur]:=UH_DRAWWORD_KANA+sp2;
  grid.Cells[1,wgcur]:=UH_DRAWWORD_KANJI+sp1;
  grid.Cells[2,wgcur]:=sp4;
  if sp3<>'' then grid.Cells[3,wgcur]:=sp3;
  inc(wgcur);
end;

procedure FinishWordGrid(grid:TStringGrid);
begin
  grid.Perform(WM_SETREDRAW, 1, 0);
  if wgcur=1 then
    grid.Hide
  else begin
    grid.RowCount:=wgcur;
    if not grid.Visible then
      grid.Show;
  end;
  grid.Invalidate;
end;

//Splits translation record in old Wakan format into parts:
//  kanji [kana] {translation} rest
procedure SplitWord(s:FString; var sp1,sp2,sp4,sp3:FString);
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



function StateStr(i:integer):string;
begin
  case i of
    0:result:=_l('#00638^eProblematic');
    1:result:=_l('#00639^eUnlearned');
    2:result:=_l('#00640^eLearned');
    3:result:=_l('#00641^eMastered');
  end;
end;

function DateForm(s:string):string;
begin
  if s='00000000'then result:='-'else
  result:=copy(s,7,2)+'.'+copy(s,5,2)+'.'+copy(s,1,4);
end;


{ Color configuration }
const
  Color_Max=100;
var
  colarr:TStringList;
  colval:TStringList;
  colsval:array[0..Color_Max] of TColor;
  colsarr:TStringList;

procedure InitColors;
var i:integer;
    reg:TRegIniFile;
    s:string;
    s2:string;
begin
  colarr:=TStringList.Create;
  colarr.add('0Kanji_Back=FFFFFF,^eBackground');
  colarr.add('0Kanji_Common=000000,^eCommon characters');
  colarr.add('0Kanji_Rare=4F4F4F,^eRare characters');
  colarr.add('0Kanji_Names=005F00,^eCharacters in names');
  colarr.add('0Kanji_Learned=7F0000,^eLearned characters');
  colarr.add('0Kanji_RadCommon=000000,^eCommon radicals');
  colarr.add('0Kanji_RadRare=4F4F4F,^eRare radicals');
  colarr.add('0Kanji_RadLearned=7F0000,^eLearned radicals');
  colarr.add('1Dict_Back=FFFFFF,^eBackground');
  colarr.add('1Dict_Text=000000,^eText');
  colarr.add('1Dict_UnknownChar=2F2F7F,^eUnknown characters');
  colarr.add('1Dict_Problematic=DDDDFF,^eProblematic words');
  colarr.add('1Dict_Unlearned=FFEEDD,^eUnlearned words');
  colarr.add('1Dict_Learned=BBFFFF,^eLearned words');
  colarr.add('1Dict_Mastered=BBFFBB,^eMastered words');
  colarr.add('1Dict_SelBack=BBBBBB,^eBackground (selected)');
  colarr.add('1Dict_SelProblematic=9999BB,^eProblematic words (selected)');
  colarr.add('1Dict_SelUnlearned=BBAA99,^eUnlearned words (selected)');
  colarr.add('1Dict_SelLearned=99BBBB,^eLearned words (selected)');
  colarr.add('1Dict_SelMastered=77BB77,^eMastered words (selected)');
  colarr.add('2Mark_Special=7F007F,^eSpecial markers');
  colarr.add('2Mark_Usage=00007F,^eUsage markers');
  colarr.add('2Mark_Grammatical=7F0000,^eGrammatical markers');
  colarr.add('2Mark_Dict=4F4F4F,^eDictionary markers');
  colarr.add('2Mark_Lesson=004F00,^eLesson markers');
  colarr.add('3Editor_Back=FFFFFF,^eBackground');
  colarr.add('3Editor_Text=000000,^eText color');
  colarr.add('3Editor_ASCII=2F2F2F,^eASCII text');
  colarr.add('3Editor_Active=FF0000,^eText being written');
  colarr.add('3Editor_Aftertouch=0000FF,^eText just converted');
  colarr.add('3Editor_Untranslated=FFFFFF,^eUntranslated text');
  colarr.add('3Editor_NotFound=003FFF,^eText where translation failed');
  colarr.add('3Editor_Particle=FFAAFF,^eEstimated particle');
  colarr.add('3Editor_Translated=EEEEEE,^eWord not in vocabulary');
  colarr.add('3Editor_Problematic=DDDDFF,^eProblematic vocabulary word');
  colarr.add('3Editor_Unlearned=FFEEDD,^eUnlearned vocabulary word');
  colarr.add('3Editor_Learned=BBFFFF,^eLearned vocabulary word');
  colarr.add('3Editor_Mastered=BBFFBB,^eMastered vocabulary word');
  colarr.add('3Editor_HintBack=EFEFEF,^eHint background');
  colarr.add('3Editor_HintSelected=00FFFF,^eHint selected background');
  colarr.add('3Editor_HintText=000000,^eHint text');
  colarr.add('3Editor_AozoraTag=C0C0C0,^eAozora Ruby <tag>');
  colarr.add('3Editor_AozoraComment=C0C0C0,^eAozora Ruby ［comment］');
  colarr.add('3Editor_AozoraRuby=C0C0C0,^eAozora Ruby 《ruby》');
  colarr.add('4Popup_Back=A0FFFF,^eBackground');
  colarr.add('4Popup_Lines=000000,^eLines');
  colarr.add('4Popup_Card=FFFFFF,^eCharacter card');
  colarr.add('4Popup_Text=000000,^eText on the caracter card');
  colval:=TStringList.Create;
  reg:=TRegIniFile.Create(WakanRegKey);
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
begin
  result:=colsval[colsarr.IndexOf(col)];
end;

procedure WriteColors;
var i:integer;
    reg:TRegIniFile;
    s:string;
    s2:string;
begin
  reg:=TRegIniFile.Create(WakanRegKey);
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


//TODO: Test Unicode conversion for the following functions

function ChinTo(s:FString):FString;
var s2:FString;
  cd:FString;
  bk:FString;
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
    s2:=fcopy(s,1,1);
    fdelete(s,1,1);
    if ({$IFNDEF UNICODE}s2[1]>'3'{$ELSE}Ord(s2[1])>$3000{$ENDIF})
    and TChar.Locate('Unicode',s2) then
    begin
      cd:=fMenu.GetCharValue(TChar.Int(TCharIndex),43);
      if cd<>'' then result:=result+cd else result:=result+s2
    end else result:=result+s2;
  end;
  TChar.Locate('Unicode',bk);
end;

function ChinFrom(s:FString):FString;
var s2:FString;
  cd:FString;
  bk:FString;
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
    s2:=fcopy(s,1,1);
    fdelete(s,1,1);
    if ({$IFNDEF UNICODE}s2[1]>'3'{$ELSE}Ord(s2[1])>$3000{$ENDIF})
    and TChar.Locate('Unicode',s2) then
    begin
      cd:=fMenu.GetCharValue(TChar.Int(TCharIndex),44);
      if cd<>'' then result:=result+cd else result:=result+s2
    end else result:=result+s2;
  end;
  TChar.Locate('Unicode',bk);
end;


{
Vocabulary entries are stored in severely deprecated format.
They have to be upgraded before working with them.
}
function FixVocabEntry(const s: string): string;
begin
  Result := s;
 {$IFDEF UNICODE}
 //User dictionaries often have inline markers in old format (<gvn>)
  repl(Result,'<',UH_LBEG);
  repl(Result,'>',UH_LEND);
 {$ENDIF}
end;

{ Reverts some fixes when storing the string back into vocabulary }
function UnfixVocabEntry(const s:string):string;
begin
  Result := s;
 {$IFDEF UNICODE}
  repl(Result,UH_LBEG,'<');
  repl(Result,UH_LEND,'>');
 {$ENDIF}
end;


{
Drawing registry.
To support text selection, Wakan keeps a list of all text lines
it has drawn with the help of DrawUnicode.
Each time a control is redrawn, all cells related to it are cleared.
}

type
  TIntTextInfo=record
    act:boolean;
    p:TPaintBox;
    x,y,fs:integer;
    s:FString;
  end;

const
  MAX_INTTEXTINFO = 4000;
var
  itt:array[1..MAX_INTTEXTINFO] of TIntTextInfo;
  curpbox:TPaintBox;

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

{ Retrieves a subblock of a block of text drawn with DrawUnicode.
p,x,y: position to probe.
Result: substring of a block of text under mouse starting at mouse position, inclusive.
id: index of a draw call, if these are equal, it's the same line
cx,cy: position of a first character of a returned subblock on a control.
fs: character height for this string. Since DrawUnicode uses square fonts,
 this is also character width.
flags: see TFindDrawRegFlags description }
function FindDrawReg(p:TPaintBox;x,y:integer;flags:TFindDrawRegFlags;out id,cx,cy,fs:integer):FString;
var i,j:integer;
begin
  id:=-1;
  result:='';
  for i:=1 to MAX_INTTEXTINFO do if itt[i].p=p then
    if  (y>=itt[i].y) and (y<=itt[i].y+itt[i].fs)
    and (x>=itt[i].x) and (x<=itt[i].x+itt[i].fs*flength(itt[i].s)) then begin
      if ffHalfCharRounding in flags then
        j:=(x-itt[i].x+itt[i].fs div 2) div itt[i].fs
      else
        j:=(x-itt[i].x) div itt[i].fs;
      id:=i;
      cy:=itt[i].y;
      fs:=itt[i].fs;
      cx:=itt[i].x+itt[i].fs*j;
      result:=fcopy(itt[i].s,j+1,flength(itt[i].s)-j);
      break;
    end;
end;

function GetDrawReg(id:integer;out cx,cy,fs:integer):FString;
begin
  cx:=itt[id].x;
  cy:=itt[id].y;
  fs:=itt[id].fs;
  Result:=itt[id].s;
end;

procedure AddDrawReg(p:TPaintBox;x,y,fs:integer;const s:FString);
var i: integer;
begin
  for i:=1 to MAX_INTTEXTINFO do
    if not itt[i].act then begin
      itt[i].act:=true;
      itt[i].p:=p;
      itt[i].x:=x;
      itt[i].y:=y;
      itt[i].fs:=fs;
      itt[i].s:=s;
      break;
    end;
end;

procedure DrawStrokeOrder(canvas:TCanvas;x,y,w,h:integer;char:string;fontsize:integer;color:TColor);
var i,l,r,m:integer;
    xx,yy:byte;
    p:pchar;
begin
  if sobin=nil then exit;
  l:=0;
  m:=0;//not really used but shut up delphi
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

{
Extracts all chinese tone mark replacement symbols from the string
and draws appropriate tone marks over their positions.
Returns the string without the tone marks.
DoDraw: if false, only delete the tone marks but don't draw them.
}
//TODO: Test Unicode conversion
function DrawToneMarks(c:TCanvas;x,y,fs:integer;s:FString;dodraw:boolean):FString;
var tb:integer;
    i:integer;
    tr:double;
    oldtextflags:longint;
    sc:byte;
    w:UnicodeString;
begin
  Result:='';
  tb:=1;
  i:=0;
  c.Font.Height:=fs;
  while s<>'' do begin
    sc := fpygettone(s);
    if sc in [0..5] then
    begin
      tr:=(i-tb)/2+tb-1;
      oldtextflags:=c.TextFlags;
      c.TextFlags:=0;
      if dodraw then begin
        case sc of
          3:w:=#$02C7;
          1:w:=#$02C9;
          2:w:=#$02CA;
          4:w:=#$02CB;
        else w:='';
        end;
        if w<>'' then
          TextOutW(c.Handle,round(tr*fs),y,PWideChar(w),1);
      end;
      c.TextFlags:=oldtextflags;
      tb:=i+1;
    end else
    begin
      Result:=Result+fcopy(s,1,1); //not a recognized tone char -- just copy it
      inc(i);
    end;
    fdelete(s,1,1);
  end;
end;

{
x, y: Where to draw.
fs: Font size
ch: Text
}
procedure DrawUnicode(c:TCanvas;x,y,fs:integer;ch:FString;fontface:string);
var w:UnicodeString;
  chn:FString;
begin
  if ch='' then exit;
  SetBkMode(c.Handle,TRANSPARENT);
  c.Font.Name:=fontface;
  c.Font.Height:=fs;
 //Retrieve markless text to draw
  chn:=DrawToneMarks(c,x,y,fs,ch,false);
  w := fstrtouni(chn);
 //If there are markers to be drawn, adjust font
  if chn<>ch then
  begin
    c.Font.Name:=FontRadical;
    fs:=round(fs/8*7);
    c.Font.Height:=fs;
    y:=y+fs div 4;
  end;
  if curpbox<>nil then
    AddDrawReg(curpbox,x,y,fs,chn);
  TextOutW(c.Handle,x,y,PWideChar(w),flength(chn));
  DrawToneMarks(c,x,y-fs div 4,fs,ch,true);
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
  if (length(s)>0) and (Colx=0) and (s[1]=UH_DRAWWORD_KANA) then
  begin
    Canvas.FillRect(Rect);
    delete(s,1,1);
//    if showroma then
//      Grid.Canvas.TextRect(Rect,Rect.Left+2,Rect.Top+2,KanaToRomaji(s,romasys,curlang)) else
//    DrawUnicode(Grid.Canvas,Rect.Left+2,Rect.Top+2,12,s,FontSmall);
    DrawKana(Canvas,Rect.Left+2,Rect.Top+1,FontSize,s,FontSmall,showroma,romasys,curlang);
  end else
  if (length(s)>0) and (s[1]=UH_DRAWWORD_KANJI) then
  begin
    Canvas.FillRect(Rect);
    delete(s,1,1);
    if (Length(s)>0) and (s[1]=UH_UNKNOWN_KANJI) then
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
    sbef:='';
    while length(s)>0 do
    begin
//      if sbef=s then
//      begin
//        showmessage(sbef);
//      end;
      sbef:=s;
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

procedure DrawPackedWordInfo(canvas: TCanvas; Rect:TRect; s:FString; ch:integer;boldfont:boolean);
var s1,sx1,s2,s3,s4:FString;
begin
  SplitWord(s,s1,s2,s3,s4);
  if curlang='c'then
  begin
    if s2[1]=ALTCH_EXCL then delete(s2,1,2);
    s2:=KanaToRomaji(s2,romasys,curlang);
    s2:=ConvertPinYin(s2);
    sx1:=s1;
    s1:=s1+UH_SPACE+s2;
    DrawWordInfo(Canvas,rect,false,false,0,UH_DRAWWORD_KANJI+s1,false,false,ch-3,boldfont);
    rect.left:=rect.left+flength(sx1)*ch+ch+(flength(s2) div 2)*ch;
  end else
  begin
    DrawWordInfo(Canvas,rect,false,false,0,UH_DRAWWORD_KANJI+s1,false,false,ch-3,boldfont);
    rect.left:=rect.left+flength(remexcl(s1))*ch;
  end;
  if (s2<>s1) and (curlang='j') then
  begin
    if s2[1]=ALTCH_EXCL then s2:=ALTCH_EXCL+s1[2]+UH_UNKNOWN_KANJI+copy(s2,3,length(s2)-2) else s2:=UH_UNKNOWN_KANJI+s2;
    DrawWordInfo(Canvas,rect,false,false,1,UH_DRAWWORD_KANJI+s2,false,false,ch-3,boldfont);
    rect.left:=rect.left+flength(remexcl(s2))*ch;
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
  PaintSelectionHighlight(Grid.Canvas,@rect);
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


{ Text selection highlight.
 Remembers one block of pixels to be highlighted, in any control.
 Used by DrawUnicode-powered controls for their custom selection mechanics. }
var
  STB_canvas:TCanvas;
  STB_x1,STB_y1,STB_x2,STB_y2:integer;

{ Clears old one, and sets and paints new selection highlight. }
procedure SetSelectionHighlight(x1,y1,x2,y2:integer;canvas:TCanvas);
begin
  //No flicker please
  if (STB_canvas=canvas) and (STB_x1=x1) and (STB_x2=x2) and (STB_y1=y1)
    and (STB_y2=y2) then exit;
  PaintSelectionHighlight;
  STB_x1:=x1;
  STB_y1:=y1;
  STB_x2:=x2;
  STB_y2:=y2;
  STB_canvas:=canvas;
  PaintSelectionHighlight;
end;

{ Re-applies currently active selection highlight where it has to be applied.
Pass valid Canvas and Rect so we do proper clipping, otherwise we'll flip
highlight even on unrelated repaints (=> highlight state broken).
Only pass wildcard when the highlight has to be redrawn outside of repaint handlers
(such as when it changes). }
procedure PaintSelectionHighlight(canv: TCanvas; in_rect: PRect);
var oldR2:integer;
  rgn: HRGN;
begin
  if STB_Canvas=nil then exit;
  if canv<>nil then if canv<>STB_canvas then exit;
  oldR2:=SetROP2(STB_Canvas.Handle,R2_NOT);
  if in_rect<>nil then begin
    rgn:=CreateRectRgn(0,0,0,0);
    GetClipRgn(STB_Canvas.Handle,rgn);
    IntersectClipRect(STB_Canvas.Handle,in_rect.Left,in_rect.Top,in_rect.Right,in_rect.Bottom);
  end else
    rgn := 0;
  STB_Canvas.Rectangle(STB_x1,STB_y1,STB_x2,STB_y2);
  if rgn<>0 then begin
    SelectClipRgn(STB_Canvas.Handle,rgn);
    DeleteObject(rgn); //copied into canvas, can delete
  end;
  SetROP2(STB_Canvas.Handle,oldR2);
end;


{ DrawUnicode-powered text selection.
 See comments to AddDrawReg/FindDrawReg, also see TfMenu.UpdateSelection.
 Uses SelectionHighlight for highlighting blocks of pixels. }

{ Updates ScreenTipBox and returns the substring of one of the strings drawn
 with DrawUnicode, currently selected in PaintBox according to DragStart->CursorPos.
p: PaintBox which currently receives mouse events (the one mouse is over,
  or the one which captures it because of dragging)
If DragStart equals CursorPos, assumes no selection. }
function PaintBoxUpdateSelection(p:TPaintBox;DragStart,CursorPos:TPoint):FString;
var id1,id2:integer;
  x1,x2,y1,fs,fs2,x_tmp:integer;
  s2,s_tmp:string;
begin
  if (DragStart.X=CursorPos.X) and (DragStart.Y=CursorPos.Y) then begin
   //No drag, mouse-over. Get text without char rounding (first half char also gives us this char)
    Result:=FindDrawReg(p,CursorPos.x,CursorPos.y,[],id1,x1,y1,fs);
    SetSelectionHighlight(0,0,0,0,nil);
    exit;
  end;

  Result:=FindDrawReg(p,CursorPos.x,CursorPos.y,[ffHalfCharRounding],id1,x1,y1,fs);
  s2:=FindDrawReg(p,DragStart.x,DragStart.y,[ffHalfCharRounding],id2,x2,y1,fs2);
  if id2<0 then begin //drag from dead point => no selection
    Result := '';
    SetSelectionHighlight(0,0,0,0,nil);
    exit;
  end;

  if id1<>id2 then begin //mouse over different control/line
   //Try again, with Y set to that of DragStart
    CursorPos.Y := DragStart.Y;
    Result:=FindDrawReg(p,CursorPos.X,CursorPos.Y,[ffHalfCharRounding],id1,x1,y1,fs);
    if id1<>id2 then begin
     //Just set the endpoint to the start or the end of the capturing line
      if CursorPos.X>DragStart.X then begin
        Result:=s2;
        SetSelectionHighlight(x2,y1,x2+flength(s2)*fs2,y1+fs2,p.Canvas);
        exit;
      end else begin
        Result:=GetDrawReg(id2,x1,y1,fs); //get whole line
        //and continue with normal handling
      end;
    end;
  end;

  if length(s2)>length(Result) then begin
   //Swap s1 and s2
    s_tmp:=s2; s2:=Result; Result:=s_tmp;
    x_tmp:=x2; x2:=x1; x1:=x_tmp;
  end;
  Result:=copy(Result,1,length(Result)-length(s2));
  SetSelectionHighlight(x1,y1,x2,y1+fs,p.Canvas);
end;

function DrawGridUpdateSelection(p:TCustomDrawGrid;DragStart,CursorPos:TPoint):FString;
var gc,gc2:TGridCoord;
    rect:TRect;
    mox1,mox2:integer;
begin
  gc:=p.MouseCoord(DragStart.x,DragStart.y);

  if (gc.x<0) or (gc.x>=2) or (gc.y<=0) then begin
   //Drag from header or drag from no-cell
    Result:='';
    SetSelectionHighlight(0,0,0,0,nil);
    exit;
  end;

  Result:=remexcl(TStringGrid(p).Cells[gc.x,gc.y]);
  rect:=p.CellRect(gc.x,gc.y);
  if (DragStart.X=CursorPos.X) and (DragStart.Y=CursorPos.Y) then begin
   //No drag, mouse over
    fdelete(Result,1,((CursorPos.x-rect.left-2) div GridFontSize));
    SetSelectionHighlight(0,0,0,0,nil);
    exit;
  end;

  gc2:=p.MouseCoord(CursorPos.x,CursorPos.y);
  if (gc2.x<>gc.x) or (gc2.y<>gc.y) then begin //mouse over different control/line
   //Try again, with Y set to that of DragStart
    CursorPos.Y := DragStart.Y;
    gc2:=p.MouseCoord(CursorPos.x,CursorPos.y);
    if (gc2.x<>gc.x) or (gc2.y<>gc.y) then begin
     //Just set the endpoint to the start or the end of the capturing line
      if CursorPos.X>DragStart.X then
        CursorPos.X:=rect.Right
      else
        CursorPos.X:=rect.Left;
     //and continue with normal handling
    end;
  end;

 //Swap points so that mox2 is to the right
  if DragStart.x>CursorPos.x then
  begin
    mox1:=CursorPos.x;
    mox2:=DragStart.x;
  end else
  begin
    mox1:=DragStart.x;
    mox2:=CursorPos.x;
  end;

  //calculate char count -- if half of the char is covered, it's covered
  mox1:=(mox1+(GridFontSize div 2)-rect.left-2) div GridFontSize;
  mox2:=(mox2+(GridFontSize div 2)-rect.left-2) div GridFontSize;
  Result:=fcopy(Result,1+mox1,mox2-mox1);
  if flength(Result)<mox2-mox1 then mox2:=mox1+flength(Result); //don't select over the end of text
  SetSelectionHighlight(mox1*GridFontSize+rect.left+2,rect.top,mox2*GridFontSize+rect.left+2,rect.bottom,p.Canvas);
end;


{ Misc }

procedure DeleteDirectory(dir:string);
var sRec: TSearchRec;
begin
  if dir='' then exit; //just in case! don't delete random files
  if not FindFirst(dir + '\*.*', faAnyFile, sRec) = 0 then
    exit;
  repeat
    if sRec.Attr and faDirectory <> 0 then
      if (sRec.Name = '.') or (sRec.Name = '..') then begin
       //Nothing
      end else
        RemoveDirectory(PChar(dir + '\' + sRec.Name))
    else
      DeleteFile(PChar(dir + '\' + sRec.Name));
  until FindNext(sRec) <> 0;
  FindClose(sRec);
  Windows.RemoveDirectory(PChar(dir));
end;

{ Universal backup function. Backups everything to the directory designated for backups. }
procedure Backup(const filename: string);
begin
 //For now works as it did in previous Wakan versions.
 //Has to be reworked to put backups into user folder.
  {$I-}
  mkdir(UserDataDir+'\backup');
  {$I+}
  ioresult;
 //dir\wakan.usr --> wakan-20130111.usr
  CopyFile(PChar(filename),pchar(UserDataDir+'\backup\'+ChangeFileExt(ExtractFilename(filename),'')+'-'
    +FormatDateTime('yyyymmdd',now)+ExtractFileExt(filename)),false);
end;

//Opens registry editor at the specific key
procedure RegeditAtKey(const key: string);
var reg: TRegistry;
begin
  reg := TRegistry.Create;
  try
    reg.RootKey := HKEY_CURRENT_USER;
    reg.OpenKey('Software', true);
    reg.OpenKey('Microsoft', true);
    reg.OpenKey('Windows', true);
    reg.OpenKey('CurrentVersion', true);
    reg.OpenKey('Applets', true);
    reg.OpenKey('Regedit', true);
    reg.WriteString('Lastkey', key);
    WinExec(PAnsiChar(AnsiString('regedit.exe')), SW_SHOW);
  finally
    FreeAndNil(reg);
  end;
end;


function _l(const id:string):string;
begin
  result:=fLanguage.TranslateString(id);
end;

function _l(const id:string; args: array of const):string;
begin
  Result := Format(fLanguage.TranslateString(id), args);
end;


var
  i:integer;

initialization
  AppFilename := GetModuleFilenameStr(0);
  AppFolder := ExtractFilePath(AppFilename);
  WakanVer := GetFileVersionInfoStr(AppFilename)
    {$IFDEF UNICODE}+' unicode'{$ENDIF};

  CurPBox:=nil;
  for i:=1 to MAX_INTTEXTINFO do itt[i].act:=false;
  GridFontSize:=14;
  STB_Canvas:=nil;

  roma_t:=TRomajiTranslator.Create;
  romac:=TStringList.Create;

finalization
  romac.Free;
  roma_t.Free;


end.
