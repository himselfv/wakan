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
(P) marker for article applies to all of it's entries
  あり得ない(P);有り得ない(P);有得ない [ありえない]
  /(adj-i) (uk) (See 有り得る・ありうる) impossible/unlikely/improbable/(P)
  /EntL2109610X/

Markers can be applied both to "only this entry" and to "all entries starting
with this":
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
  MaxMeaning = 50; //at most I've seen 27 in EDICT2

type
  EEdictParsingException = class(Exception);

  TKanjiEntry = record
    kanji: UnicodeString;
    markers: string;
    procedure Reset;
  end;
  PKanjiEntry = ^TKanjiEntry;

  TKanaEntry = record
   kana: UnicodeString;
   kanji: array[0..MaxKanji-1] of UnicodeString;
   kanji_used: integer;
   markers: string;
   procedure Reset;
   function AddKanji: PUnicodeString;
  end;
  PKanaEntry = ^TKanaEntry;

  TMeaningEntry = record
    markers: string;
    text: UnicodeString;
    procedure Reset;
  end;
  PMeaningEntry = ^TMeaningEntry;

  TEdictArticle = record
    ref: string;
    kanji: array[0..MaxKanji-1] of TKanjiEntry;
    kanji_used: integer;
    kana: array[0..MaxKana-1] of TKanaEntry;
    kana_used: integer;
    meanings: array[0..MaxMeaning-1] of TMeaningEntry;
    meanings_used: integer;
    pop: boolean;
    procedure Reset;
    function AddKanji: PKanjiEntry;
    function AddKana: PKanaEntry;
    function AddMeaning: PMeaningEntry;
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

procedure TMeaningEntry.Reset;
begin
  markers := '';
  text := '';
end;

procedure TEdictArticle.Reset;
begin
  ref := '';
  kanji_used := 0;
  kana_used := 0;
  meanings_used := 0;
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

function TEdictArticle.AddMeaning: PMeaningEntry;
begin
  Inc(meanings_used);
  if meanings_used>Length(meanings) then
    raise EEdictParsingException.Create('EdictArticle: Cannot add one more meaning');
  Result := @meanings[meanings_used-1];
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
  EH_MEANING = 4;
var
  curkanji: PKanjiEntry;
  curkana: PKanaEntry;
  curkanakanji: PUnicodeString;
  curmeaning: PMeaningEntry;
  curtext: UnicodeString; //current part of the meaning, still not commited
  curmarkers: string;
  curmark: UnicodeString; //current marker, to be tested -- only for meanings
  inmarker: boolean;
  markch: WideChar; //if inmarker, this holds the marker opening braket type -- only for meanings
  markopen: boolean; //if set, some of the markers weren't found and we added a marker braket back to curtext -- have to close it
  eh: integer;
  i,j: integer;
  ch: WideChar;

  procedure CommitCurText;
  begin
   //detect ref
    if (upos(' ', curtext)=0) and (Length(curtext)>4)
    and (curtext[1]='E') and (curtext[2]='n') and (curtext[3]='t') and (curtext[4]='L') then begin
      ed.ref := curtext;
    end else begin
      if curtext<>'' then // stuff like /(P)/ can leave us with empty text
        if curmeaning^.text<>'' then
          curmeaning^.text := curmeaning^.text + '/' + UTrim(curtext)
        else
          curmeaning^.text := UTrim(curtext);
      if curmarkers<>'' then
        if curmeaning^.markers<>'' then
          curmeaning^.markers := curmeaning^.markers + ',' +UTrim(curmarkers)
        else
          curmeaning^.markers := UTrim(curmarkers);
    end;
    curtext := '';
    curmarkers := '';
  end;

begin
  ed.Reset;
  eh := EH_KANJI;
  curkanji := ed.AddKanji;
  curkana := nil;
  curkanakanji := nil;
  curmeaning := nil;
  curtext := '';
  curmarkers := '';
  inmarker := false;
  markopen := false;
  markch := #00;

  i := 1;
  while i<=Length(s) do begin
    ch := s[i];
    if (eh=EH_KANJI) and (ch='(') then inmarker := true else
    if (eh=EH_KANJI) and inmarker and (ch=')') then inmarker := false else
    if (eh=EH_KANJI) and inmarker then curkanji.markers := curkanji.markers + ch else
    if (eh=EH_KANJI) and (ch=';') then curkanji := ed.AddKanji else
    if (eh=EH_KANJI) and (ch='[') then begin eh := EH_KANA; curkana := ed.AddKana; end else
    if (eh=EH_KANJI) and (ch='/') then begin eh := EH_MEANING; curmeaning := ed.AddMeaning; curtext := ''; curmarkers := ''; end else
    if (eh=EH_KANJI) then curkanji.kanji := curkanji.kanji + ch else
    if (eh=EH_KANA) and (ch='(') then begin
      if EvalChar(s[i+1])=EC_IDG_CHAR then begin
        eh := EH_KANAKANJI;
        curkanakanji := curkana.AddKanji;
      end else
        inmarker := true;
    end else
    if (eh=EH_KANA) and inmarker and (ch=')') then inmarker := false else
    if (eh=EH_KANA) and inmarker then curkana.markers := curkana.markers + ch else
    if (eh=EH_KANA) and (ch=';') then curkana := ed.AddKana else
    if (eh=EH_KANA) and (ch=']') then begin eh := EH_MEANING; curmeaning := ed.AddMeaning; curtext := ''; curmarkers := ''; end else
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
    if (eh=EH_MEANING) and (ch='/') then begin
     //Commit
      CommitCurText;
    end else
    if (eh=EH_MEANING) and ((ch='(')or(ch='{')) then begin inmarker := true; markch:=ch; markopen:=false; curmark:=''; end else
    if (eh=EH_MEANING) and inmarker and ((ch=',')or(ch=')')or(ch='}')) then begin
     //New entry
      if IsNumeric(curmark) then
        if curmark='1' then
          begin end //first entry doesn't need any additional slot, but we still handle it to cut the (1) mark
        else
          curmeaning := ed.AddMeaning
      else
     //(P) marker
      if curmark='P' then begin
        ed.pop := true;
      end else
     //recognized EDICT marker
      if FindMark(UTrim(curmark))<>#00 then begin
        if curmarkers<>'' then
          curmarkers := curmarkers + ',' + UTrim(curmark)
        else
          curmarkers := UTrim(curmark);
      end else
     //unrecognized marker or normal text
      begin
        if not markopen then begin
          curtext := curtext + markch + curmark; //no trim
          markopen := true;
        end else
          curtext := curtext + ',' + curmark;
      end;

      curmark := '';
      if ch<>',' then begin
        if markopen then
          case markch of
            '(': curtext := curtext + ')';
            '{': curtext := curtext + '}';
          end;
        inmarker := false;
      end;
    end else
    if (eh=EH_MEANING) and inmarker then curmark := curmark + ch else
    if (eh=EH_MEANING) then curtext := curtext + ch;

    Inc(i);
  end;

  if (eh=EH_MEANING) and (curtext<>'') or (curmarkers<>'') then //shouldnt happen
    CommitCurText();

  for i := 0 to ed.kanji_used - 1 do
    ed.kanji[i].kanji := UTrim(ed.kanji[i].kanji);
  for i := 0 to ed.kana_used - 1 do begin
    ed.kana[i].kana := UTrim(ed.kana[i].kana);
    for j := 0 to ed.kana[i].kanji_used - 1 do
      ed.kana[i].kanji[j] := UTrim(ed.kana[i].kanji[j]);
  end;
  for i := 0 to ed.meanings_used - 1 do
    ed.meanings[i].text := UTrim(ed.meanings[i].text);
end;


procedure omfgno;
begin
//Don't begin!

{
          lreat:=false;
          kanji:='';
          phon:='';
          writ:='';
          ppp:=0;
          while not lreat do
          begin
            if (bufp>=bufc) and (not feof) then
            begin
              blockread(buff,buf,4000,bufc);
              if bufc<4000 then feof:=true;
              bufp:=0;
              blockansi(PWideChar(@buf[0]),@abuf[0],2000);
            end;
            if bufp>=bufc then lreat:=true else
            begin
              uc:=PWideChar(@buf[bufp])^;
              ac := abuf[bufp div 2];
              inc(bufp,2);
  //            if Ord(uc)>255 then ac:=' ' else ac:=AnsiChar(uc);
              if uc=#$000A then lreat:=true;
              if (uc<>#$000A) and (uc<>#$000D) then
                case ppp of
                  0: if uc=#$0020 then ppp:=1 else kanji:=kanji+fstr(uc);
                  1: if uc=#$005B then ppp:=2 else ppp:=3;
                  2: if uc=#$002F then ppp:=3 else
                       if (uc<>#$005D) and (uc<>#$0020) then phon:=phon+fstr(uc);
                else
                  writ:=writ+ac;
                end;
            end;
          end;
          if (length(writ)>0) and (writ[length(writ)]='/') then
            delete(writ,length(writ),1);
          if phon='' then phon:=kanji;
}

end;

end.
