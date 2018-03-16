unit JWBRomaSort;
{
Phonetic sorting order.
Used only in the user vocabulary. Loads from Wakan.cfg
Stored as a separate unit to minimize dependencies.

Required only when adding words to the dictionary, in which case:
1. Populate romasortl.
2. Call GetPhoneticSortStr(phonetic,lang) on phonetic for a given word to get
  TUserPhoneticSort value.
}

interface
uses JWBStrings;

procedure ClearRomaSortRecords;
procedure AddRomaSortRecord(const s: string);

function GetPhoneticSortStr(const phonetic: FString; const lang: char): string;

implementation
uses KanaConv, JWBUnit;

var
  romasortl: array of record //romaji sort order
    roma: FString;
    order: string; //although it's integer inside
  end;

procedure ClearRomaSortRecords;
begin
  SetLength(romasortl, 0);
end;

procedure AddRomaSortRecord(const s: string);
var parts: TStringArray;
  i: integer;
begin
  parts := SplitStr(s, 2);
  if Length(parts)<=0 then exit;
  i := Length(romasortl);
  SetLength(romasortl, i+1);
  romasortl[i].roma := autohextofstr(parts[0]);
  if Length(parts)>=2 then
    romasortl[i].order := parts[1]
  else
    romasortl[i].order := '';
end;

//Returns a string which will represent this phonetic in sorting in User dictionary.
//Normal dictionaries don't use this.
function GetPhoneticSortStr(const phonetic: FString;const lang: char): string;
var s: FString;
  s2: FChar;
  a1,a2: string;
  i, j: integer;
begin
  if lang='j'then
  begin
   //Reconvert to some standard format and then use a preset table of
   //katakana syllable weights
    Result:='';
    s:=RomajiToKana('H'+DbKanaToRomaji(phonetic,'j'),'j',[rfDeleteInvalidChars]);
    for i:=1 to flength(s) do
    begin
      s2:=fgetch(s,i);
      a1:='';
      a2:='';
      for j:=0 to Length(romasortl)-1 do
      begin
        if romasortl[j].roma=s2 then a1:=romasortl[j].order;
        if romasortl[j].roma=
         {$IFNDEF UNICODE}
          copy(s2,1,3)+chr(ord(s2[4])+1)
         {$ELSE}
          chr(ord(s2)+1)
         {$ENDIF}
        then a2:=romasortl[j].order;
      end;
      if a1='' then Result:=Result+a2 else Result:=Result+a1;
    end;
  end else
    Result:=DbKanaToRomaji(phonetic,'c');
end;


initialization
  ClearRomaSortRecords();

end.
