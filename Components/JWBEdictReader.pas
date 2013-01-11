unit JWBEdictReader;
{
Reads EDICT format.

Example of string:
  いい加減(P);好い加減;好加減(io) [いいかげん]
  /(adj-na) (1) (uk) irresponsible/perfunctory/careless
  /(2) lukewarm/half-baked/halfhearted/vague
  /(3) (See いい加減にする) reasonable/moderate (usu. in suggestions or orders)
  /(adv) (4) considerably/quite/rather/pretty
  /(P)
  /EntL1277440X/

Features of EDICT2 to watch out for:

Multi-kanji, multi-reading, with specific pairings:
  あっと言う間;あっという間;あっとゆう間
  [あっというま(あっと言う間,あっという間); あっとゆうま(あっと言う間,あっとゆう間)]
  /(exp) (See あっと言う間に) a blink of time/the time it takes to say "Ah!"
  /EntL2208410/

Common markers for some kanji/readings:
Separate (P) markers for article and readings:
(P) marker for article applies to all of its entries
  あり得ない(P);有り得ない(P);有得ない [ありえない]
  /(adj-i) (uk) (See 有り得る・ありうる) impossible/unlikely/improbable/(P)
  /EntL2109610X/

POS markers apply to "all senses starting with this one":
Non-POS markers apply only to current sense:
  いい事[いいこと] /(exp,n) (1) good thing/nice thing
    /(2) (usu. as ～をいいことに(して)) good excuse/good grounds/good opportunity
    /(int) (3) (fem) interjection used to impress an idea or to urge a response/EntL2583070/

Sequence '/(' can occur legitimately:
  /(expression of) effort

}

interface
uses SysUtils, JWBStrings, JWBIO, JWBEdictMarkers;

{
We're using UnicodeStrings throughout this module. They resolve to slow WideStrings
on Ansi compilers, but:
  - it's too inconvenient to bother messing with FStrings
  - Ansi builds are being deprecated anyway
  - importing dictionaries is rare operation
}

const
  MaxKanji = 20; //max seen: 14
  MaxKana = 20; //must be not less than MaxKanji
  MaxSense = 50; //at most I've seen 27 in EDICT2

type
  EEdictParsingException = class(Exception);

  TKanjiEntry = record
    kanji: UnicodeString;
    markers: TMarkers;
    procedure Reset;
  end;
  PKanjiEntry = ^TKanjiEntry;

  TKanaEntry = record
   kana: UnicodeString;
   kanji: array[0..MaxKanji-1] of UnicodeString;
   kanji_used: integer;
   markers: TMarkers;
   procedure Reset;
   function AddKanji: PUnicodeString;
  end;
  PKanaEntry = ^TKanaEntry;

  TSenseEntry = record
    pos:TMarkers;
    markers: TMarkers;
    text: UnicodeString;
    procedure Reset;
  end;
  PSenseEntry = ^TSenseEntry;

  TEdictArticle = record
    ref: string;
    kanji: array[0..MaxKanji-1] of TKanjiEntry;
    kanji_used: integer;
    kana: array[0..MaxKana-1] of TKanaEntry;
    kana_used: integer;
    senses: array[0..MaxSense-1] of TSenseEntry;
    senses_used: integer;
    pop: boolean;
    procedure Reset;
    function AddKanji: PKanjiEntry;
    function AddKana: PKanaEntry;
    function AddSense: PSenseEntry;
  end;
  PEdictArticle = ^TEdictArticle;

procedure ParseEdict2Line(const s:UnicodeString; ed: PEdictArticle);

implementation

procedure TKanjiEntry.Reset;
begin
  kanji := '';
  markers := '';
end;

procedure TKanaEntry.Reset;
begin
  kana := '';
  kanji_used := 0;
  markers := '';
end;

function TKanaEntry.AddKanji: PUnicodeString;
begin
  Inc(kanji_used);
  if kanji_used>Length(kanji) then
    raise EEdictParsingException.Create('KanaEntry: Cannot add one more kanji');
  Result := @kanji[kanji_used-1];
  Result^ := '';
end;

procedure TSenseEntry.Reset;
begin
  pos := '';
  markers := '';
  text := '';
end;

procedure TEdictArticle.Reset;
begin
  ref := '';
  kanji_used := 0;
  kana_used := 0;
  senses_used := 0;
end;

function TEdictArticle.AddKanji: PKanjiEntry;
begin
  Inc(kanji_used);
  if kanji_used>Length(kanji) then
    raise EEdictParsingException.Create('EdictArticle: Cannot add one more kanji');
  Result := @kanji[kanji_used-1];
  Result^.Reset;
end;

function TEdictArticle.AddKana: PKanaEntry;
begin
  Inc(kana_used);
  if kana_used>Length(kana) then
    raise EEdictParsingException.Create('EdictArticle: Cannot add one more kana');
  Result := @kana[kana_used-1];
  Result^.Reset;
end;

function TEdictArticle.AddSense: PSenseEntry;
begin
  Inc(senses_used);
  if senses_used>Length(senses) then
    raise EEdictParsingException.Create('EdictArticle: Cannot add one more meaning');
  Result := @senses[senses_used-1];
  Result^.Reset;
end;

function IsNumeric(const s:UnicodeString): boolean;
var i: integer;
begin
  Result := true;
  for i := 1 to Length(s) do
    if (s[i]<'0') or (s[i]>'9') then begin
      Result := false;
      exit;
    end;
end;

procedure ParseEdict2Line(const s:UnicodeString; ed: PEdictArticle);
const
  EH_KANJI = 1;
  EH_KANA = 2;
  EH_KANAKANJI = 3;
  EH_SENSE = 4;
var
  curkanji: PKanjiEntry;
  curkana: PKanaEntry;
  curkanakanji: PUnicodeString;
  cursense: PSenseEntry;
  nextsense: TSenseEntry; //current part of the sense, still not commited
  curmark: UnicodeString; //current marker word, to be tested
  inmarker: boolean;
  markdef: PEdictMarkerDef;
  markch: WideChar; //if inmarker, this holds the marker opening braket type -- only for senses
  markopen: boolean; //if set, some of the markers weren't found and we added a marker braket back to curtext -- have to close it
  commpos: TMarkers; //common POS markers -- carried over from the previous sense
  eh: integer;
  i,j: integer;
  ch: WideChar;

  procedure CommitNextSense;
  begin
   //detect ref
    if (upos(' ', nextsense.text)=0) and (Length(nextsense.text)>4)
    and (nextsense.text[1]='E') and (nextsense.text[2]='n') and (nextsense.text[3]='t') and (nextsense.text[4]='L') then begin
      ed.ref := nextsense.text;
    end else begin
      if nextsense.text<>'' then // stuff like /(P)/ can leave us with empty text
        if cursense^.text<>'' then
          cursense^.text := cursense^.text + '/' + UTrim(nextsense.text)
        else
          cursense^.text := UTrim(nextsense.text);
      cursense^.markers := cursense^.markers + nextsense.markers;
      cursense^.pos := cursense^.pos + nextsense.pos;
    end;
    nextsense.Reset;
  end;

  procedure NewSense;
  begin
    //Commit old sense
    if cursense.pos='' then
      cursense.pos := commpos
    else
      commpos := cursense.pos; //new pos markers
    //Add sense
    cursense := ed.AddSense;
  end;

begin
  ed.Reset;
  eh := EH_KANJI;
  curkanji := ed.AddKanji;
  curkana := nil;
  curkanakanji := nil;
  cursense := nil;
  nextsense.Reset;
  inmarker := false;
  markopen := false;
  markch := #00;
  commpos := '';

  i := 1;
  while i<=Length(s) do begin
    ch := s[i];
    if (eh=EH_KANJI) and (ch='(') then inmarker := true else
    if (eh=EH_KANJI) and inmarker and ((ch=',') or (ch=')')) then begin
      markdef := FindMarkDef(UTrim(curmark));
     //recognized EDICT marker
      if markdef<>nil then
        curkanji.markers := curkanji.markers + markdef.id;
     //there's nothing we can do if it's unrecognized -- have to just drop it
      curmark := '';
      if ch=')' then
        inmarker := false;
    end else
    if (eh=EH_KANJI) and inmarker then curmark := curmark + ch else
    if (eh=EH_KANJI) and inmarker and ((ch=';')or(ch='[')or(ch='/')) then //safety
      raise EEdictParsingException.Create('Invalid characters in kanji markers')
    else
    if (eh=EH_KANJI) and (ch=';') then curkanji := ed.AddKanji else
    if (eh=EH_KANJI) and (ch='[') then begin eh := EH_KANA; curkana := ed.AddKana; end else
    if (eh=EH_KANJI) and (ch='/') then begin eh := EH_SENSE; cursense := ed.AddSense; end else
    if (eh=EH_KANJI) then curkanji.kanji := curkanji.kanji + ch else
    if (eh=EH_KANA) and (ch='(') then begin
      if EvalChar(s[i+1])=EC_IDG_CHAR then begin
        eh := EH_KANAKANJI;
        curkanakanji := curkana.AddKanji;
      end else
        inmarker := true;
    end else
    if (eh=EH_KANA) and inmarker and ((ch=',') or (ch=')')) then begin
      markdef := FindMarkDef(UTrim(curmark));
     //recognized EDICT marker
      if markdef<>nil then
        curkana.markers := curkana.markers + markdef.id;
      curmark := '';
      if ch=')' then
        inmarker := false;
    end else
    if (eh=EH_KANA) and inmarker then curmark := curmark + ch else
    if (eh=EH_KANJI) and inmarker and ((ch=';')or(ch=']')) then //safety
      raise EEdictParsingException.Create('Invalid characters in kanji markers')
    else
    if (eh=EH_KANA) and (ch=';') then curkana := ed.AddKana else
    if (eh=EH_KANA) and (ch=']') then begin eh := EH_SENSE; cursense := ed.AddSense; end else
    if (eh=EH_KANA) then curkana.kana := curkana.kana + ch else
    if (eh=EH_KANAKANJI) and (ch=',') then curkanakanji := curkana.AddKanji else
    if (eh=EH_KANAKANJI) and (ch=')') then eh := EH_KANA else
    if (eh=EH_KANAKANJI) then curkanakanji^ := curkanakanji^ + ch else
   {
    We use a special approach for meaning.
    Anything we read, we put into 'curpart'. When we encounter a '/', we flush
    'curpart' into 'current meaning'.
    If, inside a part, we encounter (2), (3) etc, then it's a new part, and
    we replace 'current meaning' with a new one.
    When we encounter markers, we strip known ones and leave unrecognized ones.
  }
    if (eh=EH_SENSE) and (ch='/') then begin
      CommitNextSense();
    end else
    if (eh=EH_SENSE) and ((ch='(')or(ch='{')) then begin inmarker := true; markch:=ch; markopen:=false; curmark:=''; end else
    if (eh=EH_SENSE) and inmarker and ((ch=',')or(ch=')')or(ch='}')) then begin
     //New entry
      if IsNumeric(curmark) then
        if curmark='1' then
          begin end //first entry doesn't need any additional slot, but we still handle it to cut the (1) mark
        else
          NewSense()
      else
     //(P) marker
      if curmark='P' then begin
        ed.pop := true;
      end else begin
        markdef := FindMarkDef(UTrim(curmark));
       //recognized EDICT marker
        if markdef<>nil then begin
          if mfPos in markdef.f then
            nextsense.pos := nextsense.pos + markdef.id
          else
            nextsense.markers := nextsense.markers + markdef.id;
        end else
       //unrecognized marker or normal text
        begin
          if not markopen then begin
            nextsense.text := nextsense.text + markch + curmark; //no trim
            markopen := true;
          end else
            nextsense.text := nextsense.text + ',' + curmark;
        end;
      end;

      curmark := '';
      if ch<>',' then begin
        if markopen then
          case markch of
            '(': nextsense.text := nextsense.text + ')';
            '{': nextsense.text := nextsense.text + '}';
          end;
        inmarker := false;
      end;
    end else
    if (eh=EH_SENSE) and inmarker then curmark := curmark + ch else
    if (eh=EH_SENSE) then nextsense.text := nextsense.text + ch;

    Inc(i);
  end;

  if (eh=EH_SENSE) and (nextsense.text<>'') or (nextsense.markers<>'') or (nextsense.pos<>'') then //shouldnt happen
    CommitNextSense();

  for i := 0 to ed.kanji_used - 1 do
    ed.kanji[i].kanji := UTrim(ed.kanji[i].kanji);
  for i := 0 to ed.kana_used - 1 do begin
    ed.kana[i].kana := UTrim(ed.kana[i].kana);
    for j := 0 to ed.kana[i].kanji_used - 1 do
      ed.kana[i].kanji[j] := UTrim(ed.kana[i].kanji[j]);
  end;
  for i := 0 to ed.senses_used - 1 do
    ed.senses[i].text := UTrim(ed.senses[i].text);
end;

end.
