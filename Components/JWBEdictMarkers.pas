unit JWBEdictMarkers;

interface
uses JWBStrings;

{ EDict processing }
{ Marker IDs are stored in dictionaries so we have to keep absolute backward
 compability.
 Do not change marker IDs.
 Do not deleted markers (even deprecated ones).
 Only add marker IDs after the last one.
 New marker IDs have no effect on previously compiled dictionaries. }

type
  TEdictMarker = record
    m: string;
    id: integer;
    ab: string; //type + expanded name. If name is the same as m, it's not specified
    { Supported types: 1, g, s }
    abl: string; { localized type + expanded name.
     Loaded each time application language changes. Can be empty, ab is used then }
  end;

const
  EdictMarkers: array[0..113] of TEdictMarker = (
   //Part of Speech Marking
    (m: 'adj-i'; id: 67; ab: 'g'),
    (m: 'adj-na'; id: 13; ab: 'gna-adj'),
    (m: 'adj-no'; id: 14; ab: 'gno-adj'),
    (m: 'adj-pn'; id: 15; ab: 'gpren-adj'),
    (m: 'adj-s'; id: 16; ab: 'gspec-adj'), //deprecated
    (m: 'adj-t'; id: 17; ab: 'gtaru-adj'),
    (m: 'adj-f'; id: 68; ab: 'g'),
    (m: 'adj'; id: 11; ab: 'g'),
    (m: 'adv'; id: 12; ab: 'g'),
    (m: 'adv-n'; id: 69; ab: 'g'),
    (m: 'adv-to'; id: 70; ab: 'g'),
    (m: 'aux'; id: 18; ab: 'g'),
    (m: 'aux-v'; id: 19; ab: 'g'),
    (m: 'aux-adj'; id: 71; ab: 'g'),
    (m: 'conj'; id: 20; ab: 'g'),
    (m: 'ctr'; id: 72; ab: 'g'),
    (m: 'exp'; id: 21; ab: 'gexpr'),
    (m: 'int'; id: 25; ab: 'g'),
    (m: 'iv'; id: 73; ab: 'g'),
    (m: 'n'; id: 26; ab: 'g'),
    (m: 'n-adv'; id: 27; ab: 'g'),
    (m: 'n-pref'; id: 74; ab: 'g'),
    (m: 'n-suf'; id: 29; ab: 'g'),
    (m: 'n-t'; id: 28; ab: 'g'),
    (m: 'neg'; id: 30; ab: 'g'), //deprecated
    (m: 'neg-v'; id: 31; ab: 'gneg-verb'), //deprecated
    (m: 'num'; id: 75; ab: 'g'),
    (m: 'pn'; id: 76; ab: 'g'),
    (m: 'pref'; id: 32; ab: 'g'),
    (m: 'prt'; id: 77; ab: 'g'),
    (m: 'suf'; id: 33; ab: 'g'),
    (m: 'v1'; id: 34; ab: 'gru-v'),
    (m: 'v2a-s'; id: 78; ab: 'g'),
    (m: 'v4h'; id: 79; ab: 'g'),
    (m: 'v4r'; id: 80; ab: 'g'),
    (m: 'v5'; id: 35; ab: 'gu-v'),
    (m: 'v5aru'; id: 47; ab: 'garu-v'),
    (m: 'v5b'; id: 42; ab: 'gu-v'),
    (m: 'v5g'; id: 38; ab: 'gu-v'),
    (m: 'v5k'; id: 37; ab: 'gu-v'),
    (m: 'v5k-s'; id: 45; ab: 'gIku-v'),
    (m: 'v5m'; id: 43; ab: 'gu-v'),
    (m: 'v5n'; id: 41; ab: 'gu-v'),
    (m: 'v5r'; id: 44; ab: 'gu-v'),
    (m: 'v5r-i'; id: 81; ab: 'g'),
    (m: 'v5s'; id: 39; ab: 'gu-v'),
    (m: 'v5t'; id: 40; ab: 'gu-v'),
    (m: 'v5u'; id: 36; ab: 'gu-v'),
    (m: 'v5u-s'; id: 82; ab: 'g'),
    (m: 'v5uru'; id: 48; ab: 'guru-v'),
    (m: 'v5z'; id: 46; ab: 'gzuru-v'),
    (m: 'vz'; id: 83; ab: 'g'),
    (m: 'vi'; id: 49; ab: 'gintrans-verb'),
    (m: 'vk'; id: 52; ab: 'gkuru-v'),
    (m: 'vn'; id: 84; ab: 'g'),
    (m: 'vs'; id: 50; ab: 'gp-suru'),
    (m: 'vs-c'; id: 85; ab: 'g'),
    (m: 'vs-i'; id: 86; ab: 'g'),
    (m: 'vs-s'; id: 51; ab: 'gsuru-v'),
    (m: 'vt'; id: 53; ab: 'gtrans-verb'),

   //Field of Application
    (m: 'Buddh'; id: 87),
    (m: 'MA'; id: 5; ab: '1martial-arts'),
    (m: 'comp'; id: 88),
    (m: 'food'; id: 89),
    (m: 'geom'; id: 90),
    (m: 'gram'; id: 23; ab: 'g'),
    (m: 'ling'; id: 91),
    (m: 'math'; id: 92),
    (m: 'mil'; id: 93),
    (m: 'physics'; id: 94),

   //Miscellaneous Markings
    (m: 'X'; id: 9; ab: '1rude'),
    (m: 'abbr'; id: 1; ab: '1'),
    (m: 'arch'; id: 2; ab: '1archaic'),
    (m: 'ateji'; id: 95; ab: '1'),
    (m: 'chn'; id: 96; ab: '1child'),
    (m: 'col'; id: 10; ab: '1'),
    (m: 'derog'; id: 97; ab: '1'),
    (m: 'eK'; id: 98),
    (m: 'ek'; id: 99),
    (m: 'fam'; id: 3; ab: '1familiar'),
    (m: 'fem'; id: 4; ab: '1female'),
    (m: 'gikun'; id: 22; ab: 'g'),
    (m: 'hon'; id: 54; ab: 'shonor'),
    (m: 'hum'; id: 55; ab: 's'),
    (m: 'ik'; id: 56; ab: 'sirreg-kana'),
    (m: 'iK'; id: 57; ab: 'sirreg-kanji'),
    (m: 'id'; id: 24; ab: 'gidiom'),
    (m: 'io'; id: 58; ab: 'sirreg-okurigana'),
    (m: 'm-sl'; id: 7; ab: '1manga-slang'),
    (m: 'male'; id: 6; ab: '1'),
    (m: 'male-sl'; id: 100; ab: '1'),
    (m: 'oK'; id: 62; ab: 'soutdated-kanji'),
    (m: 'obs'; id: 59; ab: 'sobsolete'),
    (m: 'obsc'; id: 60; ab: 'sobscure'),
    (m: 'ok'; id: 61; ab: 'soutdated-kana'),
    (m: 'on-mim'; id: 101),
    (m: 'poet'; id: 102; ab: '1'),
    (m: 'pol'; id: 63; ab: 'spolite'),
    (m: 'rare'; id: 103; ab: 's'),
    (m: 'sens'; id: 104; ab: '1'),
    (m: 'sl'; id: 105; ab: '1slang'),
    (m: 'uK'; id: 65; ab: 'skanji'),
    (m: 'uk'; id: 64; ab: 'skana'),
    (m: 'vulg'; id: 8; ab: '1vulgar'),

   //Word Priority Marking
    (m: 'P'; id: 66; ab: 'spop'),

   //Regional Words
    (m: 'kyb'; id: 106),
    (m: 'osb'; id: 107),
    (m: 'ksb'; id: 108),
    (m: 'ktb'; id: 109),
    (m: 'tsb'; id: 110),
    (m: 'thb'; id: 111),
    (m: 'tsug'; id: 112),
    (m: 'kyu'; id: 113),
    (m: 'rkb'; id: 114)
  );

  LastMarkerID = 114;

  MarkPop: char = Chr(66+32);

function ConvertEdictEntry(s:string;var mark:string):string;
function GetMarkEdict(mark:char):string;
function GetMarkAbbr(mark:char):string;
function EnrichDictEntry(s,mark:string):string;
function DropEdictMarkers(s:string):string;
function MarkersToStr(const s:string; out pop: boolean):string;

implementation
uses SysUtils, StrUtils;

{ EDict processing }

function ConvertEdictEntry(s:string;var mark:string):string;
var s2:string;
    inmarker:boolean;
    curm,marker:string;
    i,j:integer;
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
  inmarker:=false;
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
        mm:=255;
        for j := 0 to Length(EdictMarkers) - 1 do
          if EdictMarkers[j].m=curm then begin
            mm := EdictMarkers[j].id;
            break;
          end;
        if mm=255 then
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
var i: integer;
begin
  Result := '';
  for i := 0 to Length(EdictMarkers) - 1 do
    if EdictMarkers[i].id=ord(mark)-32 then begin
      Result := EdictMarkers[i].m;
      break;
    end;
end;

function GetMarkAbbr(mark:char):string;
var i: integer;
begin
  Result := '1?';
  for i := 0 to Length(EdictMarkers) - 1 do
    if EdictMarkers[i].id=ord(mark)-32 then begin
      Result := EdictMarkers[i].abl;
      if Result='' then Result := EdictMarkers[i].ab;
      if Result='' then Result := 's'+EdictMarkers[i].m else
      if Length(Result)=1 {only type} then Result := Result + EdictMarkers[i].m;
      break;
    end;
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

function DropEdictMarkers(s:string):string;
var lst, lcnt: integer;
  i: integer;
begin
 //Count entries
  lst := -1; //LBEG marker pos
  lcnt := 0;
  for i := 1 to Length(s) - 1 do
    if s[i]=UH_LBEG then
      lst := i
    else
    if (s[i]=UH_LEND) and (lst>=0) then begin
      Inc(lcnt, i-lst+1);
      lst := -1;
    end;

 //Allocate memory and process
  SetLength(Result, Length(s)-lcnt);
  lst := -1;
  lcnt := 0;
  for i := 1 to Length(s) - 1 do
    if s[i]=UH_LBEG then
      lst := i
    else
    if (s[i]=UH_LEND) and (lst>=0) then begin
      Inc(lcnt, i-lst+1);
      lst := -1;
    end else
    if (lst>=0) then begin
     //skip char
    end else
      Result[i+lcnt] := s[i];
end;

{ Converts a Wakan dictionary markers field to EDICT markers string  }
function MarkersToStr(const s:string; out pop: boolean):string;
var i: integer;
begin
  pop := false;
  Result := '';
  for i := 1 to Length(s) do begin
    if s[i]=MarkPop then
      pop := true
    else
      Result := Result + ',' + GetMarkEdict(s[i]);
  end;
  delete(Result,1,1);
end;


end.
