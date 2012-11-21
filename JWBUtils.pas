unit JWBUtils;
{
Various high-speed containers used in program.
For now they are grouped together but may be regrouped in the future.
}

interface
uses SysUtils, Classes;

{
Deflection parsing code and deflection list.
For more info see wakan.cfg.
}
type
 //Verb deflection rule --- see comments in wakan.cfg
 //Rules should probably be parsed on loading and stored in that form,
 //but for now we have what we have. 
  TDeflectionRule = record
    vt: char;       {
      Supported verb types:
        1 for godan verbs
        2 for ichidan verbs
        K for kuru verb
        I for Iku verb
        A for adjective
        N for noun
    }
    sufcat: char;   //suffix category
    infl: string;   //inflected suffix
    defl: string;   //deflected suffix
  end;
  PDeflectionRule = ^TDeflectionRule;
  TDeflectionArray = array of TDeflectionRule;
  TDeflectionList = class
  protected
    FList: TDeflectionArray;
    FListUsed: integer;
    procedure Grow(ARequiredFreeLen: integer);
    function GetItemPtr(Index: integer): PDeflectionRule; inline;
    function MakeNewItem: PDeflectionRule;
  public
    procedure Add(r: TDeflectionRule); overload;
    procedure Add(s: string); overload; inline;
    procedure Clear;
    property Count: integer read FListUsed;
    property Items[Index: integer]: PDeflectionRule read GetItemPtr; default;
  end;

function ParseDeflectionRule(s: string): TDeflectionRule; inline;


{
Candidate translation list for JWBUser's dictionary lookups.
Should be reasonably fast.
Not thread safe.
}

type
  TCandidateTranslation = record
    priority: integer; {1..9}
    len: integer;  {
      I'm not sure why we can't just take length(str),
      but for now I will replicate how it was done in Wakan with strings }
    verbType: char; {
      Supported verb types:
        Same as in TDeflectionRule +
        F for whatever it stands for (probably "unknown")
    }
    str: string;
  end;
  PCandidateTranslation = ^TCandidateTranslation;
  TCandidateTranslationArray = array of TCandidateTranslation;
  TCandidateTranslationList = class
  protected
    FList: TCandidateTranslationArray;
    FListUsed: integer;
    procedure Grow(ARequiredFreeLen: integer);
    function GetItemPtr(Index: integer): PCandidateTranslation; inline;
    function MakeNewItem: PCandidateTranslation;
  public
    procedure Add(priority: integer; len: integer; verbType: char; str: string); overload;
    procedure Add(ct: TCandidateTranslation); overload; inline;
    procedure Delete(Index: integer);
    procedure Clear;
    property Count: integer read FListUsed;
    property Items[Index: integer]: PCandidateTranslation read GetItemPtr; default;
  end;

implementation

//Parses deflection rule from string form into record
//See comments in wakan.cfg for format details.
function ParseDeflectionRule(s: string): TDeflectionRule; inline;
var i: integer;
begin
  Result.vt := s[1];
  Result.sufcat := s[2];
  i := pos('->', s);
  Result.infl := copy(s,3,i-1);
  Result.defl := copy(s,i+2,Length(s)-(i+2)+1);
end;

function TDeflectionList.GetItemPtr(Index: integer): PDeflectionRule;
begin
  Result := @FList[Index]; //valid until next list growth
end;

function TDeflectionList.MakeNewItem: PDeflectionRule;
begin
 //Thread unsafe
  Grow(1);
  Result := @FList[FListUsed];
  Inc(FListUsed);
end;

//Reserves enough memory to store at least ARequiredFreeLen additional items to list.
procedure TDeflectionList.Grow(ARequiredFreeLen: integer);
const MIN_GROW_LEN = 20;
begin
  if Length(FList)-FListUsed>=ARequiredFreeLen then exit; //already have the space
 //else we don't grow in less than, say, 20 items
  if ARequiredFreeLen < MIN_GROW_LEN then
    ARequiredFreeLen := MIN_GROW_LEN;
  SetLength(FList, Length(FList)+ARequiredFreeLen);
end;

procedure TDeflectionList.Add(r: TDeflectionRule);
begin
  MakeNewItem^ := r;
end;

procedure TDeflectionList.Add(s: string);
begin
  Add(ParseDeflectionRule(s));
end;

procedure TDeflectionList.Clear;
begin
  SetLength(FList, 0);
  FListUsed := 0;
end;


function TCandidateTranslationList.GetItemPtr(Index: integer): PCandidateTranslation;
begin
  Result := @FList[Index]; //valid until next list growth
end;

function TCandidateTranslationList.MakeNewItem: PCandidateTranslation;
begin
 //Thread unsafe
  Grow(1);
  Result := @FList[FListUsed];
  Inc(FListUsed);
end;

//Reserves enough memory to store at least ARequiredFreeLen additional items to list.
procedure TCandidateTranslationList.Grow(ARequiredFreeLen: integer);
const MIN_GROW_LEN = 20;
begin
  if Length(FList)-FListUsed>=ARequiredFreeLen then exit; //already have the space
 //else we don't grow in less than, say, 20 items
  if ARequiredFreeLen < MIN_GROW_LEN then
    ARequiredFreeLen := MIN_GROW_LEN;
  SetLength(FList, Length(FList)+ARequiredFreeLen);
end;

procedure TCandidateTranslationList.Add(priority: integer; len: integer; verbType: char; str: string);
var item: PCandidateTranslation;
begin
 //Only priorities 0..9 are supported
  if priority>9 then priority := 9;
  if priority<0 then priority := 0;

  item := MakeNewItem;
  item.priority := priority;
  item.len := len;
  item.verbType := verbType;
  item.str := str;
end;

procedure TCandidateTranslationList.Add(ct: TCandidateTranslation);
begin
  Add(ct.priority, ct.len, ct.verbType, ct.str);
end;

//Slow, so try to not use
procedure TCandidateTranslationList.Delete(Index: integer);
begin
 //Properly release the cell's data
  Finalize(FList[Index]);
 //Move everything up one cell
  Move(FList[Index+1], FList[Index], (FListUsed-Index-1)*SizeOf(FList[0]));
  Dec(FListUsed);
 //Zero out last cell
  FillChar(FList[FListUsed], SizeOf(FList[0]), 00); //so that we don't properly release last cell's data, it's been moved to previous cell
end;

procedure TCandidateTranslationList.Clear;
begin
  SetLength(FList, 0);
  FListUsed := 0;
end;



end.
