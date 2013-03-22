unit JWBKanaConv;
{
Converts kana to romaji and back according to a set of rules.
This is a VERY hot codepath when translating in Wakan. Therefore we use
balanced binary trees and do everything in a very optimal way.
}

interface
uses Classes, JWBStrings, BalancedTree;

{
Romaji translation table.
For more info see wakan.cfg.
}
type
 {$IFNDEF UNICODE}
  //Points to at least two FChars (8 bytes)
  PFCharPtr = PAnsiChar;
 {$ELSE}
  //These point to two UNICODE chars (4 bytes)
  PFCharPtr = PWideChar;
 {$ENDIF}

  TRomajiTranslationRule = record
   { Hiragana and katakana: FStrings. Hex is already upcased! Do not upcase. }
    hiragana: string;
    katakana: string;
    japanese: string;
    english: string;
    czech: string;
   { These pointers always point to somewhere.
    If hiragana or katakana strings are nil, they're pointing to const strings of '000000000'
    So there's no need to check for nil; they're also guaranteed to have at least two 4-chars available
    (for real strings which are one 4-char in length, they have #00 as next 4-char's first symbol,
    so it won't match to anything) }
    hiragana_ptr: PFCharPtr;
    katakana_ptr: PFCharPtr;
  end;
  PRomajiTranslationRule = ^TRomajiTranslationRule;

  TRomajiRuleNode = class(TBinTreeItem)
  protected
    FTextPtr: PFCharPtr; //same as hiragana_ptr, katakana_ptr above
    FRule: PRomajiTranslationRule;
  public
    constructor Create(const ATextPtr: PFCharPtr; ARule: PRomajiTranslationRule);
    function Compare(a:TBinTreeItem):Integer; override;
    procedure Copy(ToA:TBinTreeItem); override;
    procedure List; override;
  end;
  TRomajiRuleNode1 = class(TRomajiRuleNode) //1-char comparison
    function CompareData(const a):Integer; override;
  end;
  TRomajiRuleNode2 = class(TRomajiRuleNode) //2-char comparison
    function CompareData(const a):Integer; override;
  end;

  TRomajiTranslationTable = class
  protected
    FList: array of PRomajiTranslationRule;
    FListUsed: integer;
    FOneCharTree: TBinTree;
    FTwoCharTree: TBinTree;
    procedure Grow(ARequiredFreeLen: integer);
    function GetItemPtr(Index: integer): PRomajiTranslationRule;{$IFDEF INLINE} inline;{$ENDIF}
    function MakeNewItem: PRomajiTranslationRule;
    procedure SetupRule(r: PRomajiTranslationRule);
    procedure AddToIndex(r: PRomajiTranslationRule);
  public
    constructor Create;
    destructor Destroy; override;
    procedure Add(const r: TRomajiTranslationRule); overload;
    procedure Add(const AHiragana, AKatakana, AJapanese, AEnglish, ACzech: string); overload;
    procedure Add(const s: string); overload;{$IFDEF INLINE} inline;{$ENDIF}
    procedure Clear;
    property Count: integer read FListUsed;
    property Items[Index: integer]: PRomajiTranslationRule read GetItemPtr; default;
  end;

  TRomajiReplacementRule = record
    s_find: string;
    s_repl: string;
    romatype: integer; //0 means any
    pref: char; //Q, H, K, #00 means any
  end;
  PRomajiReplacementRule = ^TRomajiReplacementRule;

  TRomajiReplacementTable = class
  protected
    FList: array of TRomajiReplacementRule;
    FListUsed: integer;
    procedure Grow(ARequiredFreeLen: integer);
    function GetItemPtr(Index: integer): PRomajiReplacementRule;{$IFDEF INLINE} inline;{$ENDIF}
    function MakeNewItem: PRomajiReplacementRule;
  public
    procedure Add(const r: TRomajiReplacementRule); overload;
    procedure Add(const AFind, ARepl: string; ARomaType: integer; APref: char); overload;
    procedure Add(const s: string; const pref: char); overload;{$IFDEF INLINE} inline;{$ENDIF}
    procedure Clear;
    property Count: integer read FListUsed;
    property Items[Index: integer]: PRomajiReplacementRule read GetItemPtr; default;
  end;

  TResolveFlag = (
    rfConvertLatin,
      //Convert latin letters to target notation instead of treating as invalid chars.
      //This is only reliable when doing Bopomofo->PinYin, not the reverse.
    rfConvertPunctuation,
      //Convert some punctuation to target notation.
    rfDeleteInvalidChars
      //Delete invalid characters instead of replacing with '?'
  );
  TResolveFlags = set of TResolveFlag;

 { Instantiate and call this. }
  TRomajiTranslator = class
  protected
    FTrans: TRomajiTranslationTable;
    FReplKtr: TRomajiReplacementTable;
    FReplRtk: TRomajiReplacementTable;
    function SingleKanaToRomaji(var ps: PFChar; romatype: integer; flags: TResolveFlags): string;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Clear;
    procedure LoadFromFile(const filename: string);
    procedure LoadFromStrings(const sl: TStrings);
    function KanaToRomaji(const s:FString;romatype:integer;flags:TResolveFlags):string;
    function RomajiToKana(const s:string;romatype:integer;flags:TResolveFlags):FString;
  end;

function IsAllowedPunctuation(c:WideChar): boolean;
function ConvertPunctuation(c:WideChar): char;
function ConvertPunctuationF(c:FChar): char;

implementation
uses SysUtils;

{ TRomajiRuleNode }

constructor TRomajiRuleNode.Create(const ATextPtr: PFCharPtr; ARule: PRomajiTranslationRule);
begin
  inherited Create;
  Self.FTextPtr := ATextPtr;
  Self.FRule := ARule;
end;

{ Returns a pointer to an integer p+#c counting from 0
 This is pretty fast when inlined, basically the same as typing that inplace, so use without fear.
 You can even put c==0 and the code will be almost eliminated at compilation time. Delphi is smart! }
function IntgOff(p: pointer; c: integer): PInteger; inline;
begin
  Result := PInteger(IntPtr(p)+c*4);
end;

function TRomajiRuleNode1.CompareData(const a):Integer;
begin
 {$IFDEF UNICODE}
  Result := PWord(a)^ - PWord(FTextPtr)^;
 {$ELSE}
  Result := PInteger(a)^ - PInteger(FTextPtr)^;
 {$ENDIF}
end;

//TRomajiRuleNode expects PFChar (i.e. pointer to an FString data) with at least
//two FChars in it.
function TRomajiRuleNode2.CompareData(const a):Integer;
{$IFNDEF UNICODE}
var cmp: Int64;
{$ENDIF}
begin
 {$IFDEF UNICODE}
 //Compare two characters at once
  Result := PInteger(a)^-PInteger(FTextPtr)^;
 {$ELSE}
 { Both this node and the text we compare it against MUST have two full FChars.
  If they don't, there would be sorting problems as random data will be in the
  highest bytes of these integers.
  But then again, we don't add non-2char nodes to this tree, and we don't
  call a search on it if we don't have two characters. }
  cmp := PInt64(a)^-PInt64(FTextPtr)^;
 //Int64 wouldn't fit in Result:integer, we would get invalid results
  if cmp<0 then
    Result := -1
  else
  if cmp>0 then
    Result := 1
  else
    Result := 0;
 {$ENDIF}
end;

function TRomajiRuleNode.Compare(a:TBinTreeItem):Integer;
begin
  Result := CompareData(TRomajiRuleNode(a).FTextPtr);
end;

procedure TRomajiRuleNode.Copy(ToA:TBinTreeItem);
begin
  Self.FRule := TRomajiRuleNode(ToA).FRule;
end;

procedure TRomajiRuleNode.List;
begin
 //Nothing
end;

{ TRomajiTranslationTable }

constructor TRomajiTranslationTable.Create;
begin
  inherited;
  FOneCharTree := TBinTree.Create;
  FTwoCharTree := TBinTree.Create;
end;

destructor TRomajiTranslationTable.Destroy;
begin
  Clear();
  FreeAndNil(FTwoCharTree);
  FreeAndNil(FOneCharTree);
  inherited;
end;

function TRomajiTranslationTable.GetItemPtr(Index: integer): PRomajiTranslationRule;
begin
  Result := FList[Index]; //valid until next list growth
end;

function TRomajiTranslationTable.MakeNewItem: PRomajiTranslationRule;
begin
 //Thread unsafe
  Grow(1);
  New(Result);
  FList[FListUsed] := Result;
  Inc(FListUsed);
end;

//Reserves enough memory to store at least ARequiredFreeLen additional items to list.
procedure TRomajiTranslationTable.Grow(ARequiredFreeLen: integer);
const MIN_GROW_LEN = 40;
begin
  if Length(FList)-FListUsed>=ARequiredFreeLen then exit; //already have the space
 //else we don't grow in less than a chunk
  if ARequiredFreeLen < MIN_GROW_LEN then
    ARequiredFreeLen := MIN_GROW_LEN;
  SetLength(FList, Length(FList)+ARequiredFreeLen);
end;

const
 {$IFDEF UNICODE}
  UNICODE_ZERO_CODE: string = #00#00;
 {$ELSE}
  UNICODE_ZERO_CODE: string = '00000000';
 {$ENDIF}

//Makes various safety checks and sets up optimization fields for a rule
procedure TRomajiTranslationTable.SetupRule(r: PRomajiTranslationRule);
begin
 {$IFNDEF UNICODE}
 //Make sure hiragana and katakana have at least one 4-char in length, or are disabled
  if Length(r.hiragana)<4 then SetLength(r.hiragana, 0);
  if Length(r.katakana)<4 then SetLength(r.katakana, 0);
 //Drop symbols till the nearest 4-char
  if Length(r.hiragana) mod 4 <> 0 then SetLength(r.hiragana, Length(r.hiragana) - Length(r.hiragana) mod 4);
  if Length(r.katakana) mod 4 <> 0 then SetLength(r.katakana, Length(r.katakana) - Length(r.katakana) mod 4);
 {$ENDIF}
 //Setup optimization pointers
  r.hiragana_ptr := pointer(r.hiragana);
  if r.hiragana_ptr=nil then r.hiragana_ptr := pointer(UNICODE_ZERO_CODE);
  r.katakana_ptr := pointer(r.katakana);
  if r.katakana_ptr=nil then r.katakana_ptr := pointer(UNICODE_ZERO_CODE);
end;

procedure TRomajiTranslationTable.AddToIndex(r: PRomajiTranslationRule);
begin
  case flength(r.hiragana) of
    1: FOneCharTree.Add(TRomajiRuleNode1.Create(r.hiragana_ptr, r));
    2: FTwoCharTree.Add(TRomajiRuleNode2.Create(r.hiragana_ptr, r));
  //0 needs not to be indexed and 3 and above are not supported
  //We can support 3 if push comes to shove... so far no need.
  end;
  case flength(r.katakana) of
    1: FOneCharTree.Add(TRomajiRuleNode1.Create(r.katakana_ptr, r));
    2: FTwoCharTree.Add(TRomajiRuleNode2.Create(r.katakana_ptr, r));
  end;
end;

procedure TRomajiTranslationTable.Add(const r: TRomajiTranslationRule);
var pr: PRomajiTranslationRule;
begin
  pr := MakeNewItem;
  pr^ := r;
  SetupRule(pr);
  AddToIndex(pr);
end;

//Hiragana and katakana must be in decoded format (unicode on UFCHAR builds)
procedure TRomajiTranslationTable.Add(const AHiragana, AKatakana, AJapanese, AEnglish, ACzech: string);
var r: PRomajiTranslationRule;
begin
  r := MakeNewItem;
  r.hiragana := AHiragana;
  r.katakana := AKatakana;
  r.japanese := AJapanese;
  r.english := AEnglish;
  r.czech := ACzech;
  SetupRule(r);
end;

//Parses romaji translation rule from string form into record
//See comments in wakan.cfg for format details.
function ParseRomajiTranslationRule(const s: string): TRomajiTranslationRule; {$IFDEF INLINE}inline;{$ENDIF}
var s_parts: TStringArray;
begin
  s_parts := SplitStr(s, 5);
 {$IFDEF UNICODE}
  Result.hiragana := HexToUnicode(s_parts[0]);
  Result.katakana := HexToUnicode(s_parts[1]);
 {$ELSE}
  Result.hiragana := Uppercase(s_parts[0]);
  Result.katakana := Uppercase(s_parts[1]);
 {$ENDIF}
  Result.japanese := s_parts[2];
  Result.english := s_parts[3];
  Result.czech := s_parts[4];
end;

procedure TRomajiTranslationTable.Add(const s: string);
begin
  Add(ParseRomajiTranslationRule(s));
end;

procedure TRomajiTranslationTable.Clear;
var i: integer;
begin
  for i := 0 to FListUsed - 1 do
    Dispose(FList[i]);
  SetLength(FList, 0);
  FListUsed := 0;
  FOneCharTree.Clear;
  FTwoCharTree.Clear;
end;


{ TRomajiReplacementTable }

function TRomajiReplacementTable.GetItemPtr(Index: integer): PRomajiReplacementRule;
begin
  Result := @FList[Index]; //valid until next list growth
end;

function TRomajiReplacementTable.MakeNewItem: PRomajiReplacementRule;
begin
 //Thread unsafe
  Grow(1);
  Result := @FList[FListUsed];
  Inc(FListUsed);
end;

//Reserves enough memory to store at least ARequiredFreeLen additional items to list.
procedure TRomajiReplacementTable.Grow(ARequiredFreeLen: integer);
const MIN_GROW_LEN = 40;
begin
  if Length(FList)-FListUsed>=ARequiredFreeLen then exit; //already have the space
 //else we don't grow in less than a chunk
  if ARequiredFreeLen < MIN_GROW_LEN then
    ARequiredFreeLen := MIN_GROW_LEN;
  SetLength(FList, Length(FList)+ARequiredFreeLen);
end;

procedure TRomajiReplacementTable.Add(const r: TRomajiReplacementRule);
begin
  MakeNewItem^ := r;
end;

//Hiragana and katakana must be in decoded format (unicode on UFCHAR builds)
procedure TRomajiReplacementTable.Add(const AFind, ARepl: string; ARomaType: integer; APref: char);
var r: PRomajiReplacementRule;
begin
  r := MakeNewItem;
  r.s_find := AFind;
  r.s_repl := ARepl;
  r.romatype := ARomaType;
  r.pref := APref;
end;

//Parses romaji translation rule from string form into record
//See comments in wakan.cfg for format details.
function ParseRomajiReplacementRule(const s: string): TRomajiReplacementRule; {$IFDEF INLINE}inline;{$ENDIF}
var s_parts: TStringArray;
begin
  s_parts := SplitStr(s, 3);
  Result.s_find := s_parts[0];
  Result.s_repl := s_parts[1];
  if s_parts[2]<>'' then
    Result.romatype := StrToInt(s_parts[2])
  else
    Result.romatype := 0; //any
  Result.pref := #00; //by default
end;

procedure TRomajiReplacementTable.Add(const s: string; const pref: char);
var r: TRomajiReplacementRule;
begin
  r := ParseRomajiReplacementRule(s);
  r.pref := pref;
  Add(r);
end;

procedure TRomajiReplacementTable.Clear;
begin
  SetLength(FList, 0);
  FListUsed := 0;
end;



{ TRomajiTranslator }

constructor TRomajiTranslator.Create;
begin
  inherited Create;
  FTrans := TRomajiTranslationTable.Create;
  FReplKtr := TRomajiReplacementTable.Create;
  FReplRtk := TRomajiReplacementTable.Create;
end;

destructor TRomajiTranslator.Destroy;
begin
  FreeAndNil(FReplRtk);
  FreeAndNil(FReplKtr);
  FreeAndNil(FTrans);
  inherited;
end;

procedure TRomajiTranslator.Clear;
begin
  FTrans.Clear;
  FReplKtr.Clear;
  FReplRtk.Clear;
end;

{
Loads data from file.
The file must contain [Table], [KanaToRomaji] and [RomajiToKana*] sections,
and it can contain other sections, they'll be ignored.
}

procedure TRomajiTranslator.LoadFromFile(const filename: string);
var sl: TStringList;
begin
  sl := TStringList.Create();
  try
    sl.LoadFromFile(filename);
    LoadFromStrings(sl);
  finally
    FreeAndNil(sl);
  end;
end;

procedure TRomajiTranslator.LoadFromStrings(const sl: TStrings);
const //sections
  LS_NONE = 0;
  LS_TABLE = 1;
  LS_KANATOROMAJI = 2;
  LS_ROMAJITOKANA = 3;
var i: integer;
  ln: string;
  sect: integer;
  pref: char;
begin
  Clear;
  pref := #00;
  sect := LS_NONE;
  for i := 0 to sl.Count - 1 do begin
    ln := Trim(sl[i]);
    if (Length(ln)<=0) or (ln[1]='#') or (ln[1]=';') then
      continue;

    if ln='[Romaji]' then
      sect := LS_TABLE
    else
    if ln='[KanaToRomaji]' then begin
      sect := LS_KANATOROMAJI;
      pref := #00;
    end else
    if ln='[RomajiToKana]' then begin
      sect := LS_ROMAJITOKANA;
      pref := #00;
    end else
    if ln='[RomajiToKanaK]' then begin
      sect := LS_ROMAJITOKANA;
      pref := 'K';
    end else
    if ln='[RomajiToKanaQ]' then begin
      sect := LS_ROMAJITOKANA;
      pref := 'Q';
    end else
    if ln='[RomajiToKanaH]' then begin
      sect := LS_ROMAJITOKANA;
      pref := 'H';
    end else
    if (Length(ln)>=2) and (ln[1]='[') then
     //Some unknown section, skip it
      sect := LS_NONE
    else
    case sect of
      LS_TABLE: FTrans.Add(ln);
      LS_ROMAJITOKANA: FReplRtk.Add(ln, pref);
      LS_KANATOROMAJI: FReplKtr.Add(ln, pref);
    end;
  end;
end;

{
KanaToRomaji().
This function here is a major bottleneck when translating,
so we're going to try and implement it reallly fast.
}

//ps must have at least one 4-char symbol in it
function TRomajiTranslator.SingleKanaToRomaji(var ps: PFChar; romatype: integer; flags: TResolveFlags): string;
var
 {$IFNDEF UNICODE}
  pe: PFChar;
 {$ENDIF}
  bn: TBinTreeItem;
begin
 {$IFDEF UNICODE}
  if ps^=UH_HYPHEN then begin
    Inc(ps);
 {$ELSE}
  if pinteger(ps)^=pinteger(@UH_HYPHEN)^ then begin
    Inc(ps, 4);
 {$ENDIF}
    Result := '-';
    exit;
  end;

 {$IFDEF UNICODE}
  if ps^=UH_LOWLINE then begin
    Inc(ps);
 {$ELSE}
  if pinteger(ps)^=pinteger(@UH_LOWLINE)^ then begin
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
    bn := FTrans.FTwoCharTree.SearchData(ps);
    if bn<>nil then begin
      case romatype of
        2: Result := TRomajiRuleNode(bn).FRule.english;
        3: Result := TRomajiRuleNode(bn).FRule.czech;
      else
        Result := TRomajiRuleNode(bn).FRule.japanese;
      end;
     {$IFDEF UNICODE}
      Inc(ps, 2);
     {$ELSE}
      ps := pe;
     {$ENDIF}
      exit;
    end;
  end;

 //this time 1 FChar only
  bn := FTrans.FOneCharTree.SearchData(ps);
  if bn<>nil then begin
    case romatype of
      2: Result := TRomajiRuleNode(bn).FRule.english;
      3: Result := TRomajiRuleNode(bn).FRule.czech;
    else
      Result := TRomajiRuleNode(bn).FRule.japanese;
    end;
   {$IFDEF UNICODE}
    Inc(ps);
   {$ELSE}
    Inc(ps, 4);
   {$ENDIF}
    exit;
  end;

(*
  I think this is not right.

 {$IFDEF UNICODE}
  if PWord(ps)^ and $FF00 = 0 then begin
    Result := ps^;
    Inc(ps);
 {$ELSE}
  if (ps^='0') and (PChar(IntPtr(ps)+1)^='0') then begin
    Result := HexToUnicode(ps, 4);
    Inc(ps, 4);
 {$ENDIF}
    exit;
  end;
*)

 //Latin symbol
  if IsLatinLetterF(ps^) and (rfConvertLatin in flags) then
    Result := Char(ftoansi(ps^))
  else
  if IsAllowedPunctuation(ps^) and (rfConvertPunctuation in flags) then
    Result := ConvertPunctuationF(ps^)
  else
  if rfDeleteInvalidChars in flags then
    Result := ''
  else
    Result := '?';

 {$IFDEF UNICODE}
  Inc(ps);
 {$ELSE}
  Inc(ps, 4);
 {$ENDIF}
end;

function TRomajiTranslator.KanaToRomaji(const s:FString;romatype:integer;flags:TResolveFlags):string;
var upcased_s: FString;
  fn:string;
  s2:string;
 {$IFDEF UNICODE}
  ps: PWideChar;
 {$ELSE}
  ps, pn: PAnsiChar;
 {$ENDIF}
  i: integer;
  r: PRomajiReplacementRule;
begin
  if Length(s)<=0 then begin
    Result := '';
    exit;
  end;
  upcased_s := Uppercase(s);
  s2 := '';
 {$IFDEF UNICODE}
  ps := PWideChar(upcased_s);
 {$ELSE}
  ps := PAnsiChar(upcased_s);
 {$ENDIF}

 { Translation }
 {$IFDEF UNICODE}
  while ps^<>#00 do begin
 {$ELSE}
  pn := ps;
  while EatOneFChar(pn) do begin
 {$ENDIF}
    fn := SingleKanaToRomaji(ps, romatype, flags); //also eats one or two symbols
    if (fn='O') and (length(s2)>0) then fn:=upcase(s2[length(s2)]); ///TODO:WTF?!!
    s2:=s2+fn;
   {$IFNDEF UNICODE}
    pn := ps; //because ps might have advanced further
   {$ENDIF}
  end;

 { Replacements }
  for i := 0 to FReplKtr.Count - 1 do begin
    r := FReplKtr[i];
    if ((r.romatype=0) or (r.romatype=romatype)) then
      repl(s2, r.s_find, r.s_repl);
  end;

  if (length(s2)>0) and (s2[length(s2)]='''') then delete(s2,length(s2),1);
  result:=s2;
end;

{
Also accepts strange first letter flags:
  Q
  K
  H
}
function TRomajiTranslator.RomajiToKana(const s:string;romatype:integer;flags:TResolveFlags):string;
var sr,s2,s3,fn:string;
  kata:integer;
  l,i:integer;
  pref: char;
  r: PRomajiReplacementRule;
begin
  if length(s)<=0 then begin
    Result := '';
    exit;
  end;

 { First character sometimes codes something (sometimes doesn't...) -- see replacements }
  pref := s[1];
  s2 := s;

 { Replacements }
  for i := 0 to FReplRtk.Count - 1 do begin
    r := FReplRtk[i];
    if ((r.romatype=0) or (r.romatype=romatype))
    and ((r.pref=#00) or (r.pref=pref)) then
   { Only a limited set of first letters are prefixes, so we shouldn't just compare pref to whatever --
    -- but since we only load those supported prefixes into r.pref and there's no way to break that,
     this will do. }
      repl(s2, r.s_find, r.s_repl);
  end;

 { Translation }
  kata:=0;
  s3:='';
  while length(s2)>0 do
  begin
    fn:='';
    if s2[1]='_'then fn:=fstr('_');
    if s2[1]='-'then fn:=fstr('-');
    for i:=0 to FTrans.Count-1 do
    begin
      case romatype of
        2: sr := FTrans[i].english;
        3: sr := FTrans[i].czech;
      else
        sr := FTrans[i].japanese;
      end;
      if pos(sr,s2)=1 then
      begin
        l:=length(sr);
        if kata=0 then
          fn := FTrans[i].hiragana
        else
          fn := FTrans[i].katakana;
        break;
      end else
      if (romatype>0) and (pos(FTrans[i].english,s2)=1) then
      begin
        l:=length(FTrans[i].japanese);
        if kata=0 then
          fn := FTrans[i].hiragana
        else
          fn := FTrans[i].katakana;
        break;
      end;
    end;

   //If we haven't found the match, try other romaji types
    if fn='' then
    for i:=0 to FTrans.Count-1 do
      if pos(FTrans[i].japanese,s2)=1 then
      begin
        l:=length(FTrans[i].japanese);
        if kata=0 then
          fn := FTrans[i].hiragana
        else
          fn := FTrans[i].katakana;
        break;
      end else
      if pos(FTrans[i].english,s2)=1 then
      begin
        l:=length(FTrans[i].english);
        if kata=0 then
          fn := FTrans[i].hiragana
        else
          fn := FTrans[i].katakana;
        break;
      end else
      if pos(FTrans[i].czech,s2)=1 then
      begin
        l:=length(FTrans[i].czech);
        if kata=0 then
          fn := FTrans[i].hiragana
        else
          fn := FTrans[i].katakana;
        break;
      end;

    if fn='' then
    begin
      if not (rfDeleteInvalidChars in flags) then
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

//True if c is a punctuation mark we allow in kanji and kana fields.
function IsAllowedPunctuation(c:WideChar): boolean;
begin
  Result :=
    (c='·') or (c=',') //in CCEDICT
    or (c='・') or (c='、') or (c='〜'); //in EDICT2
end;

//When we need to store punctuation into pinyin, we have to make it ansi
function ConvertPunctuation(c:WideChar): char;
begin
  case c of
    '·': Result:='-';
    '・': Result:='-';
    '、': Result:=',';
    '〜': Result:='~';
  else
    Result := c;
  end;
end;

function ConvertPunctuationF(c:FChar): char;
begin
{$IFDEF UNICODE}
  Result := ConvertPunctuation(c);
{$ELSE}
  Result := ConvertPunctuation(c);
{$ENDIF}
end;


end.
