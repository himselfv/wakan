unit JWBUtils;
{
Various high-speed containers used in program.
For now they are grouped together but may be regrouped in the future.
}

interface
uses SysUtils, Classes, JWBStrings;

{
Wakan keeps a list of "graphical lines", i.e. lines as displayed on screen.
They refer to a "logical lines", that is, lines from a text document. 
}
type
  TGraphicalLineInfo = record
    xs: integer; //First character index in a logical line
    ys: integer; //Logical line number
    len: integer; //Number of characters in a logical line
  end;
  PGraphicalLineInfo = ^TGraphicalLineInfo;

  TGraphicalLineList = class
  protected
    FList: array of TGraphicalLineInfo;
    FListUsed: integer;
    procedure Grow(ARequiredFreeLen: integer);
    function GetItemPtr(Index: integer): PGraphicalLineInfo;{$IFDEF INLINE} inline;{$ENDIF}
    function MakeNewItem: PGraphicalLineInfo;
    function InsertNewItem(Index: integer): PGraphicalLineInfo;
  public
    procedure Add(xs, ys, len: integer); overload;
    procedure Insert(Index: Integer; xs, ys, len: integer); overload;
    procedure Delete(Index: integer);
    procedure Clear;
    property Count: integer read FListUsed;
    property Items[Index: integer]: PGraphicalLineInfo read GetItemPtr; default;
  end;


{ Character properties for the editor.
Each character gets its own set of properties. They are made into chains with
the help of wordState field. '<' means continue looking backwards to find:
- reading
- translation
- or word information.
Usually the three are kept together at the start of the word, but chains can
cover several words. It's possible to have a chain like this:
  * chain and word start (has reading + meaning)
  < char
  < char
  < word start (has meaning)
  < char
  < char
So if you need anything, go backwards till the start of the chain (or the string),
and look for the first character that has it set. }
{ Porting notes:
Originally properties were stored in strings, taking 9 characters for one property set:
  -90000001
1 char: wordstate
1 char: learnstate as digit
6 chars: dicidx as integer in text form
1 char: docdic as digit }
type
 { Aozora ruby supports explicit marks to show what text is being annotated:
     text text | text <<ruby>> text text
   Usually this is not needed. }
  TRubyTextBreakType = (
    btAuto,     //guess if ruby text break mark is needed
    btBreak,    //force break mark before this char
    btSkip      //don't use break mark. If we add anything new before this char, this should be changed to "Auto"
  );

  TCharacterFlag = (
    cfExplicitRuby, //this character had explicit ruby attached, even if it's empty
    cfRoot          //word root character.
      { Roots are needed so that we only annotate the root part (that's how ruby's usually done),
       but if we highlight/color the word then it's the whole word.
       Only explicit ruby currently supports word roots (for implicit annotations
       word root is guessed dynamically) }
  );
  TCharacterFlags = set of TCharacterFlag;

 { A character can have both ruby (loaded from file) and normal translation (dictionary+index)
  In that case, reading is taken from ruby and meaning from normal tl.
  Be careful when adding translations: don't break the existing ruby chains (something like '-<<<<' in wordstates). }

  TCharacterProps = record
    wordstate: char;
    learnstate: byte;
    dicidx: integer;
    docdic: byte; //document dictionary index.
      //I don't know why the hell do we have "local dictionaries", but it's a pain
      //to replace this system since they're saved into wtt files too.
    rubyTextBreak: TRubyTextBreakType;
    ruby: FString;
    flags: TCharacterFlags;
    procedure Reset;
    procedure SetChar(awordstate: char; alearnstate: byte; adicidx: integer; adocdic: byte); {$IFDEF INLINE}inline;{$ENDIF}
    procedure SetRubyChar(awordstate: char; alearnstate: byte; arubyBreak: boolean; aruby: FString); {$IFDEF INLINE}inline;{$ENDIF}
  end;
  PCharacterProps = ^TCharacterProps;
  TCharacterPropArray = array of TCharacterProps;

  TCharacterLineProps = record
    chars: TCharacterPropArray;
    charcount: integer;
    procedure Grow(ARequiredFreeLen: integer);
    function AddChar(): PCharacterProps; overload;
    procedure AddChar(const acp: TCharacterProps); overload; {$IFDEF INLINE}inline;{$ENDIF}
    procedure AddChar(awordstate: char; alearnstate: byte; adicidx: integer; adocdic: byte); overload; {$IFDEF INLINE}inline;{$ENDIF}
    procedure AddChars(Count: integer); overload;
    procedure AddChars(const AChars: TCharacterPropArray); overload; {$IFDEF INLINE}inline;{$ENDIF}
    procedure AddChars(const AChars: TCharacterLineProps); overload; {$IFDEF INLINE}inline;{$ENDIF}
    function InsertChar(Index: integer): PCharacterProps;
    procedure InsertChars(Index: integer; Count: integer); overload;
    procedure InsertChars(Index: integer; const AChars: TCharacterPropArray); overload;
    procedure DeleteChar(Index: integer);
    procedure DeleteChars(Index: integer; Count: integer); overload;
    procedure DeleteChars(Index: integer); overload; {$IFDEF INLINE}inline;{$ENDIF}
    function CopySubstr(Index: integer; Count: integer): TCharacterLineProps; overload;
    function CopySubstr(Index: integer): TCharacterLineProps; overload; {$IFDEF INLINE}inline;{$ENDIF}
    procedure Clear;
  end;
  PCharacterLineProps = ^TCharacterLineProps;

  TCharacterPropList = class
  protected
    FList: array of TCharacterLineProps;
    FListUsed: integer;
    procedure Grow(ARequiredFreeLen: integer);
    function GetLinePtr(Index: integer): PCharacterLineProps;{$IFDEF INLINE} inline;{$ENDIF}
  public
    function AddNewLine: PCharacterLineProps;
    function InsertNewLine(Index: integer): PCharacterLineProps;
    procedure AddLine(const l: TCharacterLineProps); {$IFDEF INLINE}inline;{$ENDIF}
    procedure InsertLine(Index: integer; const l: TCharacterLineProps); {$IFDEF INLINE}inline;{$ENDIF}
    procedure DeleteLine(Index: integer);
    procedure Clear;
    property Count: integer read FListUsed;
    property Lines[Index: integer]: PCharacterLineProps read GetLinePtr; default;
  end;

function CharPropArray(a: array of TCharacterProps): TCharacterPropArray;


implementation



function TGraphicalLineList.GetItemPtr(Index: integer): PGraphicalLineInfo;
begin
  Assert(Index<FListUsed);
  Result := @FList[Index]; //valid until next list growth
end;

function TGraphicalLineList.MakeNewItem: PGraphicalLineInfo;
begin
 //Thread unsafe
  Grow(1);
  Result := @FList[FListUsed];
  Inc(FListUsed);
end;

function TGraphicalLineList.InsertNewItem(Index: integer): PGraphicalLineInfo;
begin
  Grow(1);
//  Initialize(FList[FListUsed]); //needs no initialize for now
 //Move everything down one cell
  Move(FList[Index], FList[Index+1], (FListUsed-Index)*SizeOf(FList[0]));
  Inc(FListUsed);
 //Zero out the cell so that no reference counting is done
  FillChar(FList[Index], SizeOf(FList[Index]), 00);
  Result := @FList[Index];
end;

//Reserves enough memory to store at least ARequiredFreeLen additional items to list.
procedure TGraphicalLineList.Grow(ARequiredFreeLen: integer);
const MIN_GROW_LEN = 50; //there's usually a lot of lines in the document so grow in large chunks
begin
  if Length(FList)-FListUsed>=ARequiredFreeLen then exit; //already have the space
 //else we don't grow in less than a chunk
  if ARequiredFreeLen < MIN_GROW_LEN then
    ARequiredFreeLen := MIN_GROW_LEN;
  SetLength(FList, Length(FList)+ARequiredFreeLen);
end;

procedure TGraphicalLineList.Add(xs, ys, len: integer);
var item: PGraphicalLineInfo;
begin
  item := MakeNewItem;
  item.xs := xs;
  item.ys := ys;
  item.len := len;
end;

procedure TGraphicalLineList.Insert(Index: Integer; xs, ys, len: integer);
var item: PGraphicalLineInfo;
begin
  item := InsertNewItem(Index);
  item.xs := xs;
  item.ys := ys;
  item.len := len;
end;

//Slow, so try to not use
procedure TGraphicalLineList.Delete(Index: integer);
begin
 //Properly release the cell's data
 // Finalize(FList[Index]); //needs no finalize for now
   //I know it'd be better to keep the call but Delphi emits a warning
 //Move everything up one cell
  Move(FList[Index+1], FList[Index], (FListUsed-Index-1)*SizeOf(FList[0]));
  Dec(FListUsed);
 //Zero out last cell
  FillChar(FList[FListUsed], SizeOf(FList[0]), 00); //so that we don't properly release last cell's data, it's been moved to previous cell
end;

procedure TGraphicalLineList.Clear;
begin
  SetLength(FList, 0);
  FListUsed := 0;
end;


{ Character props }

//Resets everything to most basic state
procedure TCharacterProps.Reset;
begin
  wordstate := '-'; //nothing is known
  learnstate := 9;
  dicidx := 0;
  docdic := 1;
  ruby := '';
  rubyTextBreak := btAuto;
  flags := [];
end;

//Sets normally annotated char data
procedure TCharacterProps.SetChar(awordstate: char; alearnstate: byte;
  adicidx: integer; adocdic: byte);
begin
  wordstate := awordstate;
  learnstate := alearnstate;
  dicidx := adicidx;
  docdic := adocdic;
 //Leave the rest alone
end;

procedure TCharacterProps.SetRubyChar(awordstate: char; alearnstate: byte; arubyBreak: boolean; aruby: FString);
begin
  if arubyBreak then
    rubyTextBreak := btBreak
  else
    rubyTextBreak := btSkip;
  ruby := aruby;
end;

//Reserves enough memory to store at least ARequiredFreeLen additional lines.
procedure TCharacterLineProps.Grow(ARequiredFreeLen: integer);
const MIN_GROW_LEN = 40;
begin
  if Length(chars)-charcount>=ARequiredFreeLen then exit; //already have the space
 //else we don't grow in less than a chunk
  if ARequiredFreeLen < MIN_GROW_LEN then
    ARequiredFreeLen := MIN_GROW_LEN;
  SetLength(chars, Length(chars)+ARequiredFreeLen);
end;

function TCharacterLineProps.AddChar: PCharacterProps;
begin
  Grow(1);
  Result := @chars[charcount];
  Inc(charcount);
  Result^.Reset;
end;

procedure TCharacterLineProps.AddChar(const acp: TCharacterProps);
begin
  AddChar^ := acp;
end;

//Adds normally annotated char to a line
procedure TCharacterLineProps.AddChar(awordstate: char; alearnstate: byte; adicidx: integer; adocdic: byte);
begin
  AddChar^.SetChar(awordstate, alearnstate, adicidx, adocdic);
end;

procedure TCharacterLineProps.AddChars(Count: integer);
begin
  Grow(Count);
  Inc(charcount, Count);
end;

procedure TCharacterLineProps.AddChars(const AChars: TCharacterPropArray);
var i: integer;
begin
  Grow(Length(AChars));
  for i := 0 to Length(AChars) - 1 do
    AddChar(AChars[i]);
end;

procedure TCharacterLineProps.AddChars(const AChars: TCharacterLineProps);
var i: integer;
begin
  Grow(AChars.charcount);
  for i := 0 to AChars.charcount - 1 do
    AddChar(AChars.chars[i]);
end;

function TCharacterLineProps.InsertChar(Index: integer): PCharacterProps;
begin
  Grow(1);
//  Initialize(FList[FListUsed]); //needs no initialize for now
 //Move everything down one cell
  Move(chars[Index], chars[Index+1], (charcount-Index)*SizeOf(chars[0]));
  Inc(charcount);
 //Zero out the cell so that no reference counting is done
  FillChar(chars[Index], SizeOf(chars[Index]), 00);
  Result := @chars[Index];
end;

//Inserts a bunch of empty characters, accessible by chars[Index]...chars[Index+Count-1]
procedure TCharacterLineProps.InsertChars(Index: integer; Count: integer);
begin
  Grow(Count);
 //Move everything down one cell
  Move(chars[Index], chars[Index+Count], (charcount-Index)*SizeOf(chars[0]));
  Inc(charcount, Count);
 //Zero out the cells so that no reference counting is done
  FillChar(chars[Index], SizeOf(chars[Index])*Count, 00);
end;

procedure TCharacterLineProps.InsertChars(Index: integer; const AChars: TCharacterPropArray);
var i: integer;
begin
  InsertChars(Index, Length(AChars));
  for i := 0 to Length(AChars) - 1 do
    chars[Index+i] := AChars[i];
end;

procedure TCharacterLineProps.DeleteChar(Index: integer);
begin
 //Properly release the cell's data
  Finalize(chars[Index]);
 //Move everything up one cell
  Move(chars[Index+1], chars[Index], (charcount-Index-1)*SizeOf(chars[0]));
  Dec(charcount);
 //Zero out last cell
  FillChar(chars[charcount], SizeOf(chars[0]), 00); //so that we don't properly release last cell's data, it's been moved to previous cell
end;

//See CopySubstr comment about count
procedure TCharacterLineProps.DeleteChars(Index: integer; Count: integer);
begin
  if Count<0 then
    Count := charcount+Count;
  if Count>charcount-Index then //we can't delete more than there is
    Count := charcount-Index;
  if Count<=0 then
    exit; //nothing to do!
 //Properly release the cell's data
  Finalize(chars[Index], Count);
 //Move everything up Count cell
  Move(chars[Index+Count], chars[Index], (charcount-(Index+Count))*SizeOf(chars[0]));
  Dec(charcount, Count);
 //Zero out last cells
  FillChar(chars[charcount], SizeOf(chars[0])*Count, 00);
end;

procedure TCharacterLineProps.DeleteChars(Index: integer);
begin
  Assert(Index<charcount);
  DeleteChars(Index, charcount-Index)
end;

//If Count<0 then it means "Till the end of the string minus Count characters"
//  CopySubstr(0,0) == empty string
//  CopySubstr(0)   == copy entire string
//  CopySubStr(0,x) == copy from start to x
//  CopySubstr(x,-1) == copy from x to end-1
//  CopySubStr(x)   == copy from x to end
function TCharacterLineProps.CopySubstr(Index: integer; Count: integer): TCharacterLineProps;
begin
  if Count<=0 then
    Count := charcount+Count;
  if Count>charcount-Index then //we can't copy more than there is
    Count := charcount-Index;
  if Count<0 then
    Count := 0; //copy checks for this but we don't want to set charcount to <0 later
  Result.chars := copy(Self.chars, Index, Count);
  Result.charcount := Count;
end;

function TCharacterLineProps.CopySubstr(Index: integer): TCharacterLineProps;
begin
  Assert(Index<charcount);
  CopySubstr(Index, charcount-Index);
end;

procedure TCharacterLineProps.Clear;
begin
  charcount := 0;
  //No need to clear the array
end;

//Reserves enough memory to store at least ARequiredFreeLen additional lines.
procedure TCharacterPropList.Grow(ARequiredFreeLen: integer);
const MIN_GROW_LEN = 50; //there's usually a lot of lines in the document so grow in large chunks
begin
  if Length(FList)-FListUsed>=ARequiredFreeLen then exit; //already have the space
 //else we don't grow in less than a chunk
  if ARequiredFreeLen < MIN_GROW_LEN then
    ARequiredFreeLen := MIN_GROW_LEN;
  SetLength(FList, Length(FList)+ARequiredFreeLen);
end;

function TCharacterPropList.GetLinePtr(Index: integer): PCharacterLineProps;
begin
  Assert(Index<FListUsed);
  Result := @FList[Index]; //valid until next list growth
end;

function TCharacterPropList.AddNewLine: PCharacterLineProps;
begin
 //Thread unsafe
  Grow(1);
  Result := @FList[FListUsed];
  Inc(FListUsed);
end;

function TCharacterPropList.InsertNewLine(Index: integer): PCharacterLineProps;
begin
  Grow(1);
  Initialize(FList[FListUsed]);
 //Move everything down one cell
  Move(FList[Index], FList[Index+1], (FListUsed-Index)*SizeOf(FList[0]));
  Inc(FListUsed);
 //Zero out the cell so that no reference counting is done
  FillChar(FList[Index], SizeOf(FList[Index]), 00);
  Result := @FList[Index];
end;

procedure TCharacterPropList.AddLine(const l: TCharacterLineProps);
var nl: PCharacterLineProps;
begin
  nl := AddNewLine;
  nl^ := l;
  nl.chars := Copy(l.chars);
end;

procedure TCharacterPropList.InsertLine(Index: integer; const l: TCharacterLineProps);
var nl: PCharacterLineProps;
begin
  nl := InsertNewLine(Index);
  nl^ := l;
  nl.chars := Copy(l.chars);
end;

procedure TCharacterPropList.DeleteLine(Index: integer);
begin
 //Properly release the cell's data
  Finalize(FList[Index]);
 //Move everything up one cell
  Move(FList[Index+1], FList[Index], (FListUsed-Index-1)*SizeOf(FList[0]));
  Dec(FListUsed);
 //Zero out last cell
  FillChar(FList[FListUsed], SizeOf(FList[0]), 00); //so that we don't properly release last cell's data, it's been moved to previous cell
end;

procedure TCharacterPropList.Clear;
begin
  SetLength(FList, 0);
  FListUsed := 0;
end;

function CharPropArray(a: array of TCharacterProps): TCharacterPropArray;
var i: integer;
begin
  SetLength(Result, Length(a));
  for i := 0 to Length(a) - 1 do
    Result[i] := a[i];
end;


end.
