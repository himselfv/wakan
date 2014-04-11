unit JWBLegacyMarkup;
{
Dictionary entries have associated information such as source dic, popularity
and so on.

Normally this is stored in database and handled on a field-by-field basis, but
there are parts of Wakan where this markup is used or stored as a string:
  ~I entry text; entry text <dDict> <gGrammar> <gGrammar> <lCategory>

This usage is discouraged and only allowed for a bunch of cases.

1. Presentation.
Traditionally Wakan presents entry for editing in a format similar to the above.
The text has to be parsed back into parts after editing.

2. Legacy drawing routines.
Drawing routines are being upgraded to handle structured data, but many of them
yet expect inline drawing hints (similar to #1).

3. Older format vocabularies.
Wakan 1.65 style vocabularies (used everywhere) store entried completely in
presentational format from #1.
When working with these, we have no choice but to compile entries on store /
split on reading.
}

interface
uses JWBStrings, JWBEdictMarkers, JWBDic, JWBDicSearch;

const
{
Control/marker characters.
These are legacy markers to store information inside the text. They are still
used in rendering but added only at the last moment.

Unfortunately, older vocabularies stored text with ascii equivalent of these
markers:
  UH_WORDTYPE    ~
  UH_LBEG        <
  UH_LEND        >
So we have to produce/parse them when it comes to working with vocabularies.

Wakan used single characters like 'U', '~', '@' to control text markup. We can't
do that in Unicode because those characters can legitimately occur in text.
Instead we use symbols U+E000..U+F8FF which are for private use by applications.
}

 //Set by CheckKnownKanji
  AH_UNKNOWN_KANJI: Char = 'U';
  UH_UNKNOWN_KANJI = #$E001;

 { All used by DrawWordInfo }
  AH_ALTCH_EXCL: Char = '!';
  ALTCH_EXCL = #$E002;

 //can only be first character in the string. Means that this field is Kana.
  AH_DRAWWORD_KANA: Char = '#';
  UH_DRAWWORD_KANA = #$E003;

 //same, kanji
  AH_DRAWWORD_KANJI: Char = '@';
  UH_DRAWWORD_KANJI = #$E004;

  AH_WORDTYPE: Char = '~';
  UH_WORDTYPE = #$E005;        //followed by 1 character word type (F, I)
{
UH_WORDTYPE can occur once in a result, and is always followed by match type -
see JWBDicSearch.TCandidateLookup.verbType. In practice all types are always
dumbed down to either I or F, where I is italicised.
Only DrawWordInfo() uses this flag and after we upgrade it, no one ever should.
}

  AH_SETCOLOR: Char = '%';
  UH_SETCOLOR = #$E006;   //followed by 6 character hex color

  AH_LBEG:Char ='<';
  AH_LEND:Char = '>';
  UH_LBEG = #$E008;       //begin flag text (ex.: <dEDICT> <gram> <suf>)
  UH_LEND = #$E007;       //end flag text
{
UH_LBEG/LEND are generic brackets which contain a prefix + content. Known
prefixes:
  1[string]   "Special"
  s[string]   "Usage"
  g[string]   "Grammatical"
     dictionary marker abbreviations of three different types. "s" can also be
     used to add generic tiny-font comment (users do this in vocabularies)

  d[string]   "Dict"
     dictionary name from where this particular match came from, in a string with
     multiple matches

  l[string]   "Lesson"
     unsure, something related to vocabularies
     also used to list word categories for vocab entries, one by one

  pp[Self.score div 100: int]
     this match score (in a string with multiple matches). Not used in the app
     but older vocabularies may contain remnants of this.

  pwc[frequency: int]
     this match's word frequency as marked in the dict where it came from. Again,
     not used in the app, but vocabs may contain this.
}


{ Removes control chars / all markers from the string  }
function remexcl(const s:string):string;
function remmark(s:string):string;

{ Changes vocabulary entry marker format: Ansi->Unicode, Unicode->Ansi, Unicode->user readable ansi }
function FixVocabEntry(const s: string): string;
function UnfixVocabEntry(const s: string): string;
function FinalizeVocabEntry(const s: string): string;

{ Parses legacy markup into structured format to the best possible extent }
function ParseLegacyArticle(s: string): TSearchResArticle;

{ Matches markers and glosses }
function SplitGlosses(const s: string): TStringArray;

type
  TReplaceProc = reference to function(const s: string): string;

function MatchMarkers(const s: string; AOnMarker: TReplaceProc): string;

{ Edict markers }
function ConvertEdictEntry(const s:string;var mark:TMarkers):string;
function FConvertEdictEntry(const s:FString;var mark:TMarkers):FString; deprecated;
function EnrichDictEntry(const s:string;const mark:TMarkers):string;
function DropEdictMarkers(const s:string):string;

implementation
uses SysUtils;

{ Removes Wakan "exclamation" markers (unknown kanji, field type, word type etc)
 from the beginning of the string }
function remexcl(const s:string):string;
begin
 { All marks are at the beginning of the string }
  Result := s;
  while Length(Result)>0 do begin
    if Result[1]=ALTCH_EXCL then delete(Result,1,2) else
    if Result[1]=UH_DRAWWORD_KANA then delete(Result,1,1) else
    if Result[1]=UH_DRAWWORD_KANJI then delete(Result,1,1) else
    if Result[1]=UH_UNKNOWN_KANJI then delete(Result,1,1) else
    if Result[1]=UH_WORDTYPE then delete(Result,1,2) else
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



{
Upgrades vocabulary entry from Ansi to Unicode marker format.
Vocabulary entries are stored in AH_ marker format. Code expects UH_ so they
have to be upgraded.
}
function FixVocabEntry(const s: string): string;
begin
  Result := s;
  Result := repl(Result,AH_LBEG,UH_LBEG);
  Result := repl(Result,AH_LEND,UH_LEND);
  Result := repl(Result,AH_SETCOLOR,UH_SETCOLOR);
 //Let's hope other flags weren't used (they shouldn't have been)
end;

{ Degrades vocabulary entry from Unicode to Ansi marker format for user editing/storage }
function UnfixVocabEntry(const s: string): string;
begin
  Result := s;
  Result := repl(Result,UH_LBEG,AH_LBEG);
  Result := repl(Result,UH_LEND,AH_LEND);
  Result := repl(Result,UH_SETCOLOR,AH_SETCOLOR);
end;

{ Degrades vocabulary entry for textual presentation: removes control characters
 and converts markers to text inplace }
function FinalizeVocabEntry(const s: string): string;
begin
  Result := remexcl(s);
  Result := repl(Result,UH_LBEG,'');
  Result := repl(Result,UH_LEND,'');
  Result := repl(Result,UH_SETCOLOR,'');
end;


{ Parses legacy markup into structured format to the best possible extent.
 Used to decode vocabulary entries.

 Since vocabulary entry is simply string, anything can be entered and there's no
 guarantee any markup is preserved. Still, entries are displayed with the same
 rendering code so it's reasonable to assume they're compatible.

 Flags are stored as localized names and cannot be decoded back into flag-ids
 reliably:
   <gVerb> <gГлагол>
 It's further complicated by the fact that random strings can be stored as flags,
 e.g.
   <sRandom string>
 Yep, this really happens. So there's no choice but to keep flags as text.
 Remember to call UnfixVocabEntry / FinalizeVocabEntry or something.
}

function ParseLegacyArticle(s: string): TSearchResArticle;
var ps, pc: PChar;
  ftype: char;
  text: string;
  clauseOpen: boolean;

  procedure NeedClause;
  begin
    clauseOpen := true; //ensures that we add it, even if the text is empty
  end;

  procedure EndClause;
  begin
    text := Trim(text);
    if (Length(text)>0) and (text[Length(text)]=';') then
      delete(text, Length(text), 1);
    if (text<>'') or clauseOpen then
      Result.entries.Add(Trim(text), '');
    text := '';
    clauseOpen := false;
  end;

  procedure CommitText;
  begin
    if pc<=ps then exit;
    NeedClause;
    text := text + spancopy(ps,pc);
  end;

  procedure CommitTrimSpace;
  begin
   //We're going to cut something from this point on,
   //so maybe trim a space at the end of the previous block
    Dec(pc);
    if pc^=' ' then begin
      CommitText;
      Inc(pc);
    end else begin
      Inc(pc);
      CommitText;
    end;
  end;

  procedure CommitFlags;
  begin
   //At this time there's nothing to commit in flags
  end;

  procedure IncTrimSpace(var pc: PChar);
  begin
   //We have cut something before this point in the string,
   //so maybe trim a space at the start of the next block
    Inc(pc);
    if pc^=' ' then begin
      Inc(pc);
      if pc^<>' ' then
        Dec(pc);
    end;
  end;

begin
  Result.Reset;
  if s='' then exit;

{ The resulting article is usually copied from search results and so can be
  an amalgamation of several search matches from different dictionaries.
  It is still a single article but at least we split that into clauses }
  s := repl(s, '//', '/'); //can sometimes happen

  text := '';

  pc := PChar(s);
  ps := pc;
  while pc^<>#00 do

    if pc^=UH_LBEG then begin
      CommitText;
      Inc(pc);
      ps := pc;
      while (pc^<>#00) and (pc^<>UH_LEND) do
        Inc(pc);

      if pc<=ps+1 then begin //not a valid marker, e.g. <> or <s>
        text := text + spancopy(ps-1,pc+1);
        Inc(pc);
      end else begin
       //First char of the flag is always its type
        ftype := ps^;
        Inc(ps);
        if ftype='d' then begin //dictionary
          Result.dicname := spancopy(ps,pc);
          IncTrimSpace(pc);
        end else begin
         //At this time can't do any other special parsing
          text := text + spancopy(ps-2,pc+1);
          Inc(pc);
        end;
      end;

      ps := pc;
    end else

    if (pc[0]=' ') and (pc[1]='/') and (pc[2]=' ') then begin
      CommitTrimSpace;
      CommitFlags;
      EndClause;
      NeedClause;
      Inc(pc,3);
      ps := pc;
    end else

    if (pc^='(') and IsDigit(pc[1]) and (
      (pc[2]=')') or ( IsDigit(pc[2]) and (pc[3]=')')  )
    ) then begin
      CommitTrimSpace;
      CommitFlags;
      EndClause;
      NeedClause;
     //At this time we ignore the id value and just assume it's sequential as we expect it to be
      if pc[2]=')' then begin
        Inc(pc,3);
      end else begin
        Inc(pc,4);
      end;
      if pc^=' ' then Inc(pc);
      ps := pc;
    end else

      Inc(pc); //any other char

  CommitText;
  CommitFlags;
  EndClause;
end;

{ Tries to split the entry text into glosses. Minds brackets and marker chars,
 but still is not guaranteed to produce 100% precise result }
function SplitGlosses(const s: string): TStringArray;
var ps, pc: PChar;

  procedure CommitText;
  begin
    if ps>=pc then exit;
    SetLength(Result, Length(Result)+1);
    Result[Length(Result)-1] := spancopy(ps, pc);
  end;

begin
  SetLength(Result, 0);
  if s='' then exit;

  pc := PChar(s);
  ps := pc;
  while pc^<>#00 do

    if pc^=UH_LBEG then begin
     //Skip marker contents
      Inc(pc);
      while (pc^<>#00) and (pc^<>UH_LEND) do
        Inc(pc);
      Inc(pc);
    end else

    if pc^='(' then begin
     //Skip contents
      Inc(pc);
      while (pc^<>#00) and (pc^<>')') do
        Inc(pc);
      Inc(pc);
    end else

    if pc^=';' then begin
      CommitText;
      Inc(pc);
      if pc^=' ' then Inc(pc);
      ps := pc;
    end else

      Inc(pc); //any other char

  CommitText;
end;

{ Locates all markers in the string and passes their content to AOnMarker.
 Puts whatever it returns instead of the marker.
 Space handling: as long as the replacement is empty, removes up to one space
 from each side of the marker.
 To better handle spaces at the start/end of the glosses, pass glosses
 separately. }
function MatchMarkers(const s: string; AOnMarker: TReplaceProc): string;
var ps, pc: PChar;

  procedure CommitText;
  begin
    Result := Result + spancopy(ps, pc);
  end;

begin
  Result := '';
  if s='' then exit;

  pc := PChar(s);
  ps := pc;
  while pc^<>#00 do

    if pc^=UH_LBEG then begin
      CommitText;
      Inc(pc);
      ps := pc;
      while (pc^<>#00) and (pc^<>UH_LEND) do
        Inc(pc);
      Result := Result + AOnMarker(spancopy(ps,pc));
      Inc(pc);

     //Collapse up to one space from later (works both if marker ends with space,
     //and if it was preceded by space and was destroyed)
      if ((Length(Result)<=0) or (Result[Length(Result)]=' ')) then
        if pc^=' ' then
          Inc(pc)
        else
        if pc^=#00 then
          delete(Result,Length(Result),1);

      ps := pc;
    end else

      Inc(pc); //any other char

  CommitText;
end;


{ EDict processing }

function ConvertEdictEntry(const s:string;var mark:TMarkers):string;
var s2:string;
    inmarker:boolean;
    curm,marker:string;
    i:integer;
    markerd:boolean;
    oldm:string;
    mm:TMarker;
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
        mm:=FindMark(curm);
        if mm=#00 then
        begin
          if (oldm='1') or (oldm='2') or (oldm='3') then insection:=true;
          s2:=s2+'('+oldm+')'; mm:=#00; markerd:=false;
          marker:='';
        end;
        if insection then mm:=#00;
        if mm<>#00 then mark:=mark+mm;
      end;
      marker:='';
      inmarker:=false;
    end else if inmarker then marker:=marker+s[i] else s2:=s2+s[i];
  end;
  if copy(s2,length(s2)-1,2)=', 'then delete(s2,length(s2)-1,2);
  s2:=trim(s2);
  result:=s2;
end;

{
We are too lazy to implement a proper FString version for ConvertEdictEntry,
especiall since it's only used in two cases now:
  1. When importing chinese dictionaries on Ansi
  2. When working with VERY OLD dictionaries which have no separate markers field.
In the second case, the articles are without exception stored in Ansi,
and in the first case we just don't have a multilingual chinese dic support yet.
}
function FConvertEdictEntry(const s:FString;var mark:TMarkers):FString;
begin
  Result := fstr(ConvertEdictEntry(string(fstrtouni(s)),mark))
end;

function EnrichDictEntry(const s:string;const mark:TMarkers):string;
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

function DropEdictMarkers(const s:string):string;
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
    if lst>=0 then begin
     //skip char
    end else
      Result[i+lcnt] := s[i];
end;

end.
