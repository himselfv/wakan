unit JWBVocab1;
{ Wakan 1.90 and older vocabulary format, very different from dictionaries,
 there's usually only one such dictionary and there's a lot of hacking going
 on to make it compatible with new features.
 Lots of data is stored in UserData.

 Also includes the functionality of JWBVocab1Search (analoguous to JWBDicSearch),
 e.g. converting vocabulary search results into search result format. }

interface
uses JWBDic;

//Vocabulary uses these in place of UH_* counterparts from JWBDic.
//Call FixVocabEntry entry to upgrade these to UH_*.
const
  AH_LBEG: Char = '<';
  AH_LEND: Char = '>';
  AH_SETCOLOR: Char = '%';

{ Changes vocabulary entry marker format: Ansi->Unicode, Unicode->Ansi, Unicode->user readable ansi }
function FixVocabEntry(const s: string): string;
function UnfixVocabEntry(const s: string): string;
function FinalizeVocabEntry(const s: string): string;

{ Vocab1 article in parsed form. Can be restored back into string form with
 ToLegacyString().
 Can be converted to common SearchResArticle with routines from JWBDicSearch }
type
  TVocab1Article = record
    { Vocab1 entries sometimes contain data from multiple dictionaries, the
     sources specified inline.
     We need this info to match and remove duplicate entries in unified vocab /
     dic results. }
    dicname: string;
    entries: TEntries;
    procedure Reset;
    function ToLegacyString: string;
  end;
  PVocab1Article = ^TVocab1Article;

{ Parses legacy markup into structured format to the best possible extent }
function ParseVocab1Article(s: string): TVocab1Article;

implementation
uses SysUtils, JWBStrings, JWBLegacyMarkup;

{
Upgrades vocabulary entry from Ansi to Unicode marker format.
Vocabulary entries are stored in AH_ marker format. Code expects UH_*.
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


procedure TVocab1Article.Reset;
begin
  dicname := '';
  entries.Reset;
end;

{ Returns backward-compatible entry string. Eventually do be deleted. }
function TVocab1Article.ToLegacyString: string;
begin
  Result := entries.ToEnrichedString;
  if Self.dicname<>'' then
    Result := Result  +' '+UH_LBEG+'d'+Self.dicname+UH_LEND;
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

 Remember to do FixVocabEntry() before passing the raw vocab data here.
}

function ParseVocab1Article(s: string): TVocab1Article;
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

end.
