unit JWBUnit;
{ Common stuff for Wakan. }

{$DEFINE NODICLOADPROMPT}
{ Do not show "Loading dictionary..." window when hot-loading a dictionary.
 It was needed before when loading was slow, but I feel like now it only makes
 the interface feel sluggish. }

interface
uses Graphics, Windows, SysUtils, Classes, Controls, Dialogs, Grids, Forms,
  ExtCtrls, IniFiles, JWBStrings, KanaConv, JWBDic;

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

//See KanaConv.ResolveFlag

var
  curlang: char;       //currently active eastern language (j or c)
  cromasys:integer;

  jshowroma: boolean;  //show romaji instead of kana
  cshowroma: boolean;  //show pinyin instead of bopomofo
  showroma: boolean;   //one of the above, depending on the active language

 { Romaji translation tables. Populated on load. }
  roma_db: TKanaTranslator;
  roma_user: TKanaTranslator;
  rpy_db: TPinYinTranslator;
  rpy_user: TPinYinTranslator;

//We allow some punctuation marks in kana/kanji fields in Wakan, but only some

function IsAllowedPunctuation(c:WideChar): boolean;
function ConvertPunctuation(c:WideChar): char;
function ConvertPunctuationF(c:FChar): char;

type
  TSanitizeFlag = (
    sfKeepLatin,              //keep HW latin
    sfKeepAllowedPunctuation, //keep allowed punctuation (+ replace where needed)
    sfKeepKanji               //keep kanji characters
  );
  TSanitizeFlags = set of TSanitizeFlag;

function SanitizeKana(s: string; flags: TSanitizeFlags = [];
  const replChar: string = ''): string;

//Converts according to user preferences. Use for live input/output.
function KanaToRomaji(const s:FString;lang:char):string; overload; inline;
function KanaToRomajiF(const s:FString;lang:char):FString; overload; inline;
function KanaToRomaji(const s:FString;lang:char;flags:TResolveFlags):string; overload;
function RomajiToKana(const s:string;lang:char;flags:TResolveFlags):FString;

//Converts with a standard romaji. Use for DB/dictionaries/persistence.
function DbKanaToRomaji(const s:FString;lang:char):string; overload; inline;
function DbKanaToRomajiF(const s:FString;lang:char):FString; overload; inline;
function DbKanaToRomaji(const s:FString;lang:char;flags:TResolveFlags):string; overload;
function DbRomajiToKana(const s:string;lang:char;flags:TResolveFlags):FString;

{ Packs and cleans any romaji (perhaps coming from user) to signature format:
 no punctuation, no spaces, no unknown chars, lowercase.
 This is NOT ALL that's required from signature in dict, but other conditions require
 knowing the syllable composition and handled on import before calling this. }
function SignatureFrom(const s:string): string;

{ Converts characters to traditional or simplified variants, if configured to. }
function ChinSimplified(s:FString):FString;
function ChinTraditional(s:FString):FString;

{ Converts raw kana/bopomofo to romaji/kana for presentation }
function ConvertKana(const ch: FString;showr:boolean;lang:char): FString; overload;
function ConvertKana(const ch: FString): FString; overload;


{ Colors }

procedure InitColors(const reg: TCustomIniFile);
function GetColorString(i:integer):string;
procedure SetCol(col:integer;val:TColor);
function GetCol(col:integer):TColor;
function Col(col:string):TColor;
procedure WriteColors(const reg: TCustomIniFile);
procedure SetColDefault(i:integer);


{ Painting and currently selected fonts }
var
  FontJapanese,         //Default Japanese font
  FontJapaneseGrid,     //Alternative Japanese font for character grid
  FontSmallJp: string;  { A weird "Japanese font, but small" for certain situations.
                          Where it is used, it replaces ANY font (Japanese or Chinese),
                          so it has to support both.
                          Because of this it's being outphased now }

  //Traditional/Big5 versions
  FontChinese,          //Default Chinese font
  FontChineseGrid:      //Alternative Chinese font for character grid
    string;

  //Simplified/GB2132 versions
  //Both are used only if configured in Settings, and currently in only a few places (KanjiList + KanjiDetails)
  FontChineseGB,        //Default Chinese font
  FontChineseGridGB:    //Alternative Chinese font for character grid.
    string;

  //Font for drawing radicals. These may contain rare symbols so full unicode font
  //is recommended.
  //Also used for drawing Chinese/ChineseGrid in "Both Traditional+Simplified" mode
  FontRadical: string;

  FontEnglish: string;  //Western translation/meaning and other misc things
  FontPinYin: string;   //Font for Romaji AND Pinyin. Maybe we'll split them someday (see DrawKana).
  FontStrokeOrder: string;   //Stroke order only, not stroke count (that's FontEnglish)

  GridFontSize:integer;

//Retreives CJK font (Japanese/Chinese) configured for the currently selected language
function GetCJKFont: string; overload; inline;
function GetCJKFont(const lang: char): string; overload; inline;
function GetCJKGridFont: string; overload; inline;
function GetCJKGridFont(const lang: char): string; overload; inline;
function GetKanaFont: string; overload; inline;
function GetKanaFont(const lang: char): string; overload; inline;

procedure DrawUnicode(c:TCanvas; x,y,fh:integer; const ch:FString; const fontface:string); inline;

//Draws phonetic line: kana/bopomofo or romaji/pinyin, depending on the language and settings.
//Chooses appropriate font.
procedure DrawKana(Canvas: TCanvas; x,y,fs:integer; ch:string); overload; inline;
procedure DrawKana(Canvas: TCanvas; x,y,fs:integer; ch:string; lang:char); overload; inline;



{ Misc }

function DateForm(s:string):string;

implementation
uses Messages, UITypes, StrUtils, ShlObj, Registry, AppData, JWBSettings, JWBLanguage,
  JWBCharData, JWBLegacyMarkup, MemSource, JWBDrawText;


{ Romaji conversions }

//True if c is a punctuation mark we allow in kanji and kana fields.
function IsAllowedPunctuation(c:WideChar): boolean;
begin
  Result :=
    (c='·') or (c=',') //in CCEDICT
    or (c='・') or (c='、') or (c='〜') or (c='～') //in EDICT2
    or (c='.') or (c='「') or (c='」') or (c='『') or (c='』') or (c='。') or (c='!'); //Wadoku-jiten
end;

//When we need to store punctuation into pinyin, we have to make it ansi
function ConvertPunctuation(c:WideChar): char;
begin
  case c of
    '·': Result := '-';
    '・': Result := '-';
    '、': Result := ',';
    '〜',
    '～': Result := '~';
    '.',
    '。': Result := '.';
    '「',
    '『',
    '」',
    '』': Result := '"';
    '!': Result := '!';
  else
    Result := c;
  end;
end;

function ConvertPunctuationF(c:FChar): char;
begin
  Result := ConvertPunctuation(c);
end;

//Removes everything except for kana / pinyin from the string or replaces it
//with appropriate characters
function SanitizeKana(s: string; flags: TSanitizeFlags; const replChar: string): string;
var i: integer;
  ec: TEvalCharType;
begin
  Result := '';
  for i := 1 to Length(s) do begin
    ec := EvalChar(s[i]);
    if ec in [EC_HIRAGANA, EC_KATAKANA, EC_BOPOMOFO] then
      Result := Result + s[i]
    else
    if (ec in [EC_IDG_CHAR]) and (sfKeepKanji in flags) then
      Result := Result + s[i]
    else
    if IsAllowedPunctuation(s[i]) and (sfKeepAllowedPunctuation in flags) then
      Result := Result + ConvertPunctuation(s[i])
    else
    if IsLatinLetter(s[i]) and (sfKeepLatin in flags) then
      Result := Result + s[i]
    else
      Result := Result + replChar;
  end;
end;

{ Converts kana to romaji in a default way, that is assuming the input is a
 kana from database and the output is for user display.
 Equivalent to older "KanaToRomaji(clean)". }
function KanaToRomaji(const s:FString;lang:char):string;
begin
  Result := KanaToRomaji(SanitizeKana(s,[sfKeepLatin,sfKeepAllowedPunctuation]),lang,[])
end;

//Same, but also converts the result to FString, enhances PinYin
function KanaToRomajiF(const s:FString;lang:char):FString;
begin
  if lang='c'then
    Result:=ConvertPinYin(KanaToRomaji(s,lang))
  else
    Result:=fstr(KanaToRomaji(s,lang));
end;

//Converts kana to romaji, per flags
function KanaToRomaji(const s:FString;lang:char;flags:TResolveFlags):string;
begin
  if lang='j'then
    Result:=roma_user.KanaToRomaji(s,flags)
  else
  if lang='c'then
    result:=rpy_user.KanaToRomaji(s,flags);
end;

{ Converts romaji to kana. Romaji must be a clean-romaji, i.e. no latin letters,
 no punctuation. Appropriate keep flags have no effect.
 All in all, romaji-to-kana conversion ought to happen only on import, therefore
 no flagless version. }
function RomajiToKana(const s:string;lang:char;flags:TResolveFlags):FString;
begin
  if lang='j'then
    Result:=roma_user.RomajiToKana(s,flags)
  else
  if lang='c'then
    result:=rpy_user.RomajiToKana(s,flags);
end;

function DbKanaToRomaji(const s:FString;lang:char):string;
begin
  Result:=DbKanaToRomaji(SanitizeKana(s,[sfKeepLatin,sfKeepAllowedPunctuation]),lang,[])
end;

function DbKanaToRomajiF(const s:FString;lang:char):FString;
begin
  if lang='c'then
    Result:=ConvertPinYin(DbKanaToRomaji(s,lang))
  else
    Result:=fstr(DbKanaToRomaji(s,lang));
end;

function DbKanaToRomaji(const s:FString;lang:char;flags:TResolveFlags):string;
begin
  if lang='j'then
    Result:=roma_db.KanaToRomaji(s,flags)
  else
  if lang='c'then
    result:=rpy_db.KanaToRomaji(s,flags);
end;

{ Converts romaji to kana. Romaji must be a clean-romaji, i.e. no latin letters,
 no punctuation. Appropriate keep flags have no effect.
 All in all, romaji-to-kana conversion ought to happen only on import, therefore
 no flagless version. }
function DbRomajiToKana(const s:string;lang:char;flags:TResolveFlags):FString;
begin
  if lang='j'then
    Result:=roma_db.RomajiToKana(s,flags)
  else
  if lang='c'then
    result:=rpy_db.RomajiToKana(s,flags);
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


function ChinSimplified(s:FString):FString;
var s2:FString;
  cd:FString;
begin
  if (curlang='j') or (fSettings.RadioGroup5.ItemIndex<>1) then
  begin
    result:=s;
    exit;
  end;
  result:='';
  while s<>'' do begin
    s2:=fcopy(s,1,1);
    fdelete(s,1,1);
    if ({$IFNDEF UNICODE}s2[1]>'3'{$ELSE}Ord(s2[1])>$3000{$ENDIF}) then
    begin
      cd:=GetCharProp(s2,ptSimplifiedVariant);
      if cd<>'' then result:=result+cd else result:=result+s2
    end else result:=result+s2;
  end;
end;

function ChinTraditional(s:FString):FString;
var s2:FString;
  cd:FString;
begin
  if (curlang='j') or (fSettings.RadioGroup5.ItemIndex<>0) then
  begin
    result:=s;
    exit;
  end;
  result:='';
  while s<>'' do
  begin
    s2:=fcopy(s,1,1);
    fdelete(s,1,1);
    if ({$IFNDEF UNICODE}s2[1]>'3'{$ELSE}Ord(s2[1])>$3000{$ENDIF}) then
    begin
      cd:=GetCharProp(s2,ptTraditionalVariant);
      if cd<>'' then result:=result+cd else result:=result+s2
    end else result:=result+s2;
  end;
end;




{ Color configuration }

const
  Color_Max=100;
var
  colarr:TStringList;
  colval:TStringList;
  colsval:array[0..Color_Max] of TColor;
  colsarr:TStringList;

procedure InitColors(const reg: TCustomIniFile);
var i:integer;
    s:string;
    s2:string;
begin
  FreeAndNil(colarr);
  colarr:=TStringList.Create;
  colarr.add('0Kanji_Back=FFFFFF,^Background');
  colarr.add('0Kanji_Common=000000,^Common characters');
  colarr.add('0Kanji_Rare=4F4F4F,^Rare characters');
  colarr.add('0Kanji_Names=005F00,^Characters in names');
  colarr.add('0Kanji_Learned=7F0000,^Learned characters');
  colarr.add('0Kanji_RadCommon=000000,^Common radicals');
  colarr.add('0Kanji_RadRare=4F4F4F,^Rare radicals');
  colarr.add('0Kanji_RadLearned=7F0000,^Learned radicals');
  colarr.add('1Dict_Back=FFFFFF,^Background');
  colarr.add('1Dict_Text=000000,^Text');
  colarr.add('1Dict_UnknownChar=2F2F7F,^Unknown characters');
  colarr.add('1Dict_Problematic=DDDDFF,^Problematic words');
  colarr.add('1Dict_Unlearned=FFEEDD,^Unlearned words');
  colarr.add('1Dict_Learned=BBFFFF,^Learned words');
  colarr.add('1Dict_Mastered=BBFFBB,^Mastered words');
  colarr.add('1Dict_SelBack=BBBBBB,^Background (selected)');
  colarr.add('1Dict_SelProblematic=9999BB,^Problematic words (selected)');
  colarr.add('1Dict_SelUnlearned=BBAA99,^Unlearned words (selected)');
  colarr.add('1Dict_SelLearned=99BBBB,^Learned words (selected)');
  colarr.add('1Dict_SelMastered=77BB77,^Mastered words (selected)');
  colarr.add('2Mark_Special=7F007F,^Special markers');
  colarr.add('2Mark_Usage=00007F,^Usage markers');
  colarr.add('2Mark_Grammatical=7F0000,^Grammatical markers');
  colarr.add('2Mark_Dict=4F4F4F,^Dictionary markers');
  colarr.add('2Mark_Lesson=004F00,^Lesson markers');
  colarr.add('3Editor_Back=FFFFFF,^Background');
  colarr.add('3Editor_Text=000000,^Text color');
  colarr.add('3Editor_ASCII=2F2F2F,^ASCII text');
  colarr.add('3Editor_Active=FF0000,^Text being written');
  colarr.add('3Editor_Aftertouch=0000FF,^Text just converted');
  colarr.add('3Editor_Untranslated=FFFFFF,^Untranslated text');
  colarr.add('3Editor_NotFound=003FFF,^Text where translation failed');
  colarr.add('3Editor_Particle=FFAAFF,^Estimated particle');
  colarr.add('3Editor_Translated=EEEEEE,^Word not in vocabulary');
  colarr.add('3Editor_Problematic=DDDDFF,^Problematic vocabulary word');
  colarr.add('3Editor_Unlearned=FFEEDD,^Unlearned vocabulary word');
  colarr.add('3Editor_Learned=BBFFFF,^Learned vocabulary word');
  colarr.add('3Editor_Mastered=BBFFBB,^Mastered vocabulary word');
  colarr.add('3Editor_HintBack=EFEFEF,^Hint background');
  colarr.add('3Editor_HintSelected=00FFFF,^Hint selected background');
  colarr.add('3Editor_HintText=000000,^Hint text');
  colarr.add('3Editor_AozoraTag=C0C0C0,^Aozora Ruby <tag>');
  colarr.add('3Editor_AozoraComment=C0C0C0,^Aozora Ruby ［comment］');
  colarr.add('3Editor_AozoraRuby=C0C0C0,^Aozora Ruby 《ruby》');
  colarr.add('4Popup_Back=A0FFFF,^Background');
  colarr.add('4Popup_Lines=000000,^Lines');
  colarr.add('4Popup_Card=FFFFFF,^Character card');
  colarr.add('4Popup_Text=000000,^Text on the caracter card');
  FreeAndNil(colval);
  FreeAndNil(colsarr);
  colval:=TStringList.Create;
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

procedure WriteColors(const reg: TCustomIniFile);
var i:integer;
    s:string;
    s2:string;
begin
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

//Compatibility
procedure DrawUnicode(c:TCanvas; x,y,fh:integer; const ch:FString; const fontface:string);
begin
  DrawCJK(c, x, y, fh, ch, fontface);
end;

{ Converts raw kana/bopomofo to romaji/kana for presentation }
function ConvertKana(const ch: FString;showr:boolean;lang:char): FString;
begin
  if showr then
    Result := KanaToRomajiF(ch,lang)
  else
    Result := ConvertBopomofo(ch);
end;

function ConvertKana(const ch: FString): FString;
begin
  Result := ConvertKana(ch,showroma,curlang);
end;

{
Retrieves Japanese or Chinese font configured for the currently selected language
TODO:
  We can cache the fonts in shared variables for speed access
  and update on curlang chanes.
}
function GetCJKFont: string;
begin
  Result := GetCJKFont(curlang);
end;

function GetCJKFont(const lang: char): string;
begin
  if lang <> 'c' then
    Result := FontJapanese
  else
  case fSettings.RadioGroup5.ItemIndex of
    0: Result := FontChinese;
    1: Result := FontChineseGB;
    2: Result := FontRadical;
  else Result := FontChinese;
  end;
end;

function GetCJKGridFont: string;
begin
  Result := GetCJKGridFont(curlang);
end;

function GetCJKGridFont(const lang: char): string; overload; inline;
begin
  if lang <> 'c' then
    Result := FontJapanese
  else
  case fSettings.RadioGroup5.ItemIndex of
    0: Result := FontChineseGrid;
    1: Result := FontChineseGridGB;
    2: Result := FontRadical;
  else Result := FontChineseGrid;
  end;
end;

function GetKanaFont: string; overload; inline;
begin
  Result := FontPinYin; //currently both languages use the same font
end;

function GetKanaFont(const lang: char): string; overload; inline;
begin
  Result := GetKanaFont();
end;

{
DrawKana: Draws phonetics in any eastern language/settings.
All phonetics are drawn in:
  Kana, Bopomofo: appropriate CJK font
  Romaji, Pinyin: FontPinyin
Some places used FontEnglish for Romaji before the unification. MAYBE we should
follow this rule. I'll see how much of each style is there.
}
procedure DrawKana(Canvas: TCanvas; x,y,fs:integer; ch:string; lang:char); overload; inline;
var lshowroma: boolean;
  cnv: FString;
begin
  Canvas.Font.Style:=[];
  if lang='c' then
    lshowroma := cshowroma
  else
    lshowroma := jshowroma;
  if lshowroma then begin
    //some places used FontEnglish before unification
    //some places used fs+1 for romaji, others maintained fs
    cnv := KanaToRomajiF(ch,lang); //includes ConvertPinYin
    DrawUnicode(Canvas,x,y,fs+1,cnv,GetKanaFont(lang));
  end else
    DrawUnicode(Canvas,x,y,fs,ConvertBopomofo(ch),GetCJKFont(lang));
end;

procedure DrawKana(Canvas: TCanvas; x,y,fs:integer; ch:string);
begin
  DrawKana(Canvas,x,y,fs,ch,curlang);
end;


{ Misc }

function DateForm(s:string):string;
begin
  if s='00000000'then result:='-'else
  result:=copy(s,7,2)+'.'+copy(s,5,2)+'.'+copy(s,1,4);
end;

initialization
  GridFontSize:=14;
  showroma:=false;
  roma_db := TKanaTranslator.Create;
  roma_user := TKanaTranslator.Create;
  rpy_db := TPinyinTranslator.Create;
  rpy_user := TPinyinTranslator.Create;

finalization
 {$IFDEF CLEAN_DEINIT}
  FreeAndNil(rpy_user);
  FreeAndNil(rpy_db);
  FreeAndNil(roma_user);
  FreeAndNil(roma_db);
 {$ENDIF}

end.
