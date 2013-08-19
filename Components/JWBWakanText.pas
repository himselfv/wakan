unit JWBWakanText;

interface
uses SysUtils, Classes, JWBStrings, JWBConvert;

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
  Be careful when adding translations: don't break existing ruby chains (something like '-<<<<' in wordstates). }

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

type
 { Character position in source text. }
  TSourcePos = record
    y: integer; //line, 0-based
    x: integer;  //char, 0-based
    class operator Equal(const a: TSourcePos; const b: TSourcePos): boolean; inline;
    class operator NotEqual(const a: TSourcePos; const b: TSourcePos): boolean; inline;
    class operator GreaterThan(const a: TSourcePos; const b: TSourcePos): boolean; inline;
    class operator GreaterThanOrEqual(const a: TSourcePos; const b: TSourcePos): boolean; inline;
    class operator LessThan(const a: TSourcePos; const b: TSourcePos): boolean; inline;
    class operator LessThanOrEqual(const a: TSourcePos; const b: TSourcePos): boolean; inline;
  end;
  PSourcePos = ^TSourcePos;

 { Text block, see SourcePos. Usually not inclusive on the read end }
  TSourceBlock = record
    fromy: integer;
    fromx: integer;
    toy: integer;
    tox: integer;
    function FromPoint: TSourcePos; inline;
    function ToPoint: TSourcePos; inline;
    class operator Equal(const a: TSourceBlock; const b: TSourceBlock): boolean; inline;
    class operator NotEqual(const a: TSourceBlock; const b: TSourceBlock): boolean; inline;
    class operator BitwiseAnd(const a: TSourceBlock; const b: TSourceBlock): TSourceBlock;
  end;
  PSourceBlock = ^TSourceBlock;
  TTextSelection = TSourceBlock; { Text selection in logical coordinates }
  PTextSelection = ^TTextSelection;

  TTextAnnotMode = (
    amNone,
      //do not parse ruby when loading
      //save without any ruby
    amDefault,
      //do not parse ruby when loading
      //parse ruby when pasting if configured to
      //save only those annotations which were loaded from ruby
    amRuby
      //parse ruby when loading/pasting
      //save all annotations as ruby
  );

  TTextSaveFormat = class;

  EBadWakanTextFormat = class(Exception);

  TGetDictionaryEntryEvent = procedure(Sender: TObject; const APos: TSourcePos;
    out kanji, reading: FString; out meaning: string) of object;

  TWakanText = class
  public
    constructor Create;
    destructor Destroy; override;
    procedure Clear;

  public //File opening/saving
    procedure LoadText(const filename:string;tp:byte;AnnotMode:TTextAnnotMode);
    function LoadWakanText(AStream: TStream; ASilent: boolean = false): boolean; overload;
    procedure LoadWakanText(const AFilename:string); overload;
    procedure SaveWakanText(AStream: TStream; ABlock: PTextSelection = nil); overload;
    procedure SaveWakanText(const AFilename:string); overload;
    procedure SaveText(AAnnotMode:TTextAnnotMode; AFormat: TTextSaveFormat;
      AStream: TStream; ABlock: PTextSelection = nil);

  protected //Document
    doc: TStringList; //document lines
    doctr: TCharacterPropList; //character property list (translation, wordstate, etc)
    FOnGetDictionaryEntry: TGetDictionaryEntryEvent;
  public
    docdic: TStringList;
    function GetDoc(ax,ay:integer):FChar;
    function GetDocChain(cx,cy:integer):FString;
    procedure AdjustDocument;
    function EndOfDocument: TSourcePos; {$IFDEF INLINE}inline;{$ENDIF}
    function EndOfLine(const LogicalLineIndex: integer): TSourcePos; {$IFDEF INLINE}inline;{$ENDIF}
    procedure GetDictionaryEntry(const APos: TSourcePos; out kanji, reading: FString;
      out meaning: string);
    property Lines: TStringList read doc;
    property PropertyLines: TCharacterPropList read doctr;
    property OnGetDictionaryEntry: TGetDictionaryEntryEvent
      read FOnGetDictionaryEntry write FOnGetDictionaryEntry;

  protected //Editing
    procedure CheckTransCont(x,y:integer);
  public
    procedure AddLine(const chars: FString; const props: PCharacterLineProps = nil);
    procedure SplitLine(x,y:integer);
    procedure JoinLine(y:integer);
    procedure DeleteCharacter(const APos: TSourcePos); overload; inline;
    procedure DeleteCharacter(x,y:integer); overload;
    procedure DeleteLine(AIndex: integer);
    procedure DeleteBlock(block: TSourceBlock);
    procedure PasteText(const APos: TSourcePos; const chars: FString;
      const props: TCharacterLineProps; AnnotMode: TTextAnnotMode;
      AEndPos: PSourcePos = nil);
    procedure PasteDoc(const APos: TSourcePos; const ADoc: TWakanText;
      AEndPos: PSourcePos = nil);

  protected //Ruby stuff
    procedure CollapseRuby(var s: FString; var sp: TCharacterLineProps);

  end;

 {
  This saves data or selection to file or to memory to be copied to clipboard.
  Write your own formats if you wish.

  Rules:
    Default implementation saves simply text.

    One encoding converter is created by default implementation, unless you
    don't call inherited BeginDocument, in which case don't call inherited at all.

    By the end of EndDocument all caches have to be flushed down to FStream.
 }

  TTextSaveFormat = class
  protected
    FStream: TStream;
    FOwnsStream: boolean;
    FOutput: TJwbConvert;
    FEncType: integer;
    FNoBOM: boolean;
    LastChar: FChar;
    AtSpace: boolean;
    AtNewline: boolean;
    procedure outpln(const s: string);
    procedure outp(const s: string);
  public
    constructor Create(AEncType: integer; ANoBOM: boolean=false);
    destructor Destroy; override;
    procedure BeginDocument; virtual;
    procedure AddChars(const s: FString); virtual;
    procedure AddWord(const word,reading: FString; const meaning: string;
      rubyTextBreak: TRubyTextBreakType); virtual;
    procedure EndDocument; virtual;
    property Stream: TStream read FStream write FStream;
    property OwnsStream: boolean read FOwnsStream write FOwnsStream;
    property NoBOM: boolean read FNoBOM write FNoBOM;
  end;

  TKanaOnlyFormat = class(TTextSaveFormat)
  protected
    FAddSpaces: boolean;
  public
    constructor Create(AEncType: integer; AAddSpaces: boolean);
    procedure AddWord(const word,reading: FString; const meaning: string;
      rubyTextBreak: TRubyTextBreakType); override;
  end;

  TKanjiOnlyFormat = class(TTextSaveFormat)
  public
    procedure AddWord(const word,reading: FString; const meaning: string;
      rubyTextBreak: TRubyTextBreakType); override;
  end;

  TKanjiKanaFormat = class(TTextSaveFormat)
  public
    procedure AddWord(const word,reading: FString; const meaning: string;
      rubyTextBreak: TRubyTextBreakType); override;
  end;

 { UTF8 only for simplicity }
  THtmlFormatOption = (
    hoClipFragment    //add <!-- start fragment --> <!-- end fragment --> marks to use as HTML clipboard format
  );
  THtmlFormatOptions = set of THtmlFormatOption;

  THtmlFormat = class(TTextSaveFormat)
  protected
    FOptions: THtmlFormatOptions;
  public
    constructor Create(AOptions: THtmlFormatOptions);
    procedure BeginDocument; override;
    procedure AddChars(const s: FString); override;
    procedure AddWord(const word,reading: FString; const meaning: string;
      rubyTextBreak: TRubyTextBreakType); override;
    procedure EndDocument; override;
    property Options: THtmlFormatOptions read FOptions write FOptions;
  end;

  TRubyTextFormat = class(TTextSaveFormat)
  public
    procedure AddWord(const word,reading: FString; const meaning: string;
      rubyTextBreak: TRubyTextBreakType); override;
  end;

  { UTF8 only by format }
  {$R 'odt.res' 'Assets\ODT\odt.rc'}
  TOpenDocumentContentFormat = class(TTextSaveFormat)
  public
    constructor Create;
    procedure BeginDocument; override;
    procedure AddChars(const s: FString); override;
    procedure AddWord(const word,reading: FString; const meaning: string;
      rubyTextBreak: TRubyTextBreakType); override;
    procedure EndDocument; override;
  end;

  TOpenDocumentFormat = class(TTextSaveFormat)
  protected
    FContentMem: TMemoryStream;
    FContentFormat: TOpenDocumentContentFormat;
  public
    constructor Create();
    destructor Destroy; override;
    procedure BeginDocument; override;
    procedure AddChars(const s: FString); override;
    procedure AddWord(const word,reading: FString; const meaning: string;
      rubyTextBreak: TRubyTextBreakType); override;
    procedure EndDocument; override;
  end;

const
 //Markers used by THtmlFormat to indicate start and end of fragment meant for Clipboard
 //See CF_HTML documentation.
  HtmlStartFragment = '<!--StartFragment -->';
  HtmlEndFragment = '<!--EndFragment -->';

function SourcePos(x,y: integer): TSourcePos; inline;
function SourceBlock(const AFromY, AFromX, AToY, AToX: integer): TSourceBlock; inline;

implementation
uses Forms, Windows, JclCompression, JWBUnit, JWBCharData,
  StreamUtils;

{ Source position }

function SourcePos(x,y: integer): TSourcePos;
begin
  Result.x := x;
  Result.y := y;
end;

class operator TSourcePos.Equal(const a: TSourcePos; const b: TSourcePos): boolean;
begin
  Result := (a.y=b.y) and (a.x=b.x);
end;

class operator TSourcePos.NotEqual(const a: TSourcePos; const b: TSourcePos): boolean;
begin
  Result := (a.y<>b.y) or (a.x<>b.x);
end;

class operator TSourcePos.GreaterThan(const a: TSourcePos; const b: TSourcePos): boolean;
begin
  Result := (a.y>b.y) or ((a.y=b.y) and (a.x>b.x));
end;

class operator TSourcePos.GreaterThanOrEqual(const a: TSourcePos; const b: TSourcePos): boolean;
begin
  Result := (a.y>b.y) or ((a.y=b.y) and (a.x>=b.x));
end;

class operator TSourcePos.LessThan(const a: TSourcePos; const b: TSourcePos): boolean;
begin
  Result := (a.y<b.y) or ((a.y=b.y) and (a.x<b.x));
end;

class operator TSourcePos.LessThanOrEqual(const a: TSourcePos; const b: TSourcePos): boolean;
begin
  Result := (a.y<b.y) or ((a.y=b.y) and (a.x<=b.x));
end;

function SourceBlock(const AFromY, AFromX, AToY, AToX: integer): TSourceBlock;
begin
  Result.fromy := AFromY;
  Result.fromx := AFromX;
  Result.toy := AToY;
  Result.tox := AToX;
end;

function TSourceBlock.FromPoint: TSourcePos;
begin
  Result.y := fromy;
  Result.x := fromx;
end;

function TSourceBlock.ToPoint: TSourcePos;
begin
  Result.y := toy;
  Result.x := tox;
end;

class operator TSourceBlock.Equal(const a: TSourceBlock; const b: TSourceBlock): boolean;
begin
  Result := (a.fromy=b.fromy) and (a.fromx=b.fromx) and (a.toy=b.toy) and (a.tox=b.tox);
end;

class operator TSourceBlock.NotEqual(const a: TSourceBlock; const b: TSourceBlock): boolean;
begin
  Result := (a.fromy<>b.fromy) or (a.fromx<>b.fromx) or (a.toy<>b.toy) or (a.tox<>b.tox);
end;

class operator TSourceBlock.BitwiseAnd(const a: TSourceBlock; const b: TSourceBlock): TSourceBlock;
begin
 //highest "from"
  if a.fromy=b.fromy then begin
    Result.fromy := a.fromy;
    if a.fromx>=b.fromx then
      Result.fromx := a.fromx
    else
      Result.fromx := b.fromx;
  end else
  if a.fromy>b.fromy then begin
    Result.fromy := a.fromy;
    Result.fromx := a.fromx;
  end else begin
    Result.fromy := b.fromy;
    Result.fromx := b.fromx;
  end;

 //lowest "to"
  if a.toy=b.toy then begin
    Result.toy := a.toy;
    if a.tox<=b.tox then
      Result.tox := a.tox
    else
      Result.tox := b.tox;
  end else
  if a.toy<b.toy then begin
    Result.toy := a.toy;
    Result.tox := a.tox;
  end else begin
    Result.toy := b.toy;
    Result.tox := b.tox;
  end;
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


{ Text save formats }

constructor TTextSaveFormat.Create(AEncType: integer; ANoBOM: boolean);
begin
  inherited Create;
  FEncType := AEncType;
  FNoBOM := ANoBOM;
  AtSpace := false;
  AtNewline := true;
  LastChar := UH_NOCHAR;
end;

destructor TTextSaveFormat.Destroy;
begin
  FreeAndNil(FOutput);
  if FOwnsStream then
    FreeAndNil(FStream);
  inherited;
end;

//Used to simplify writing strings as fstrings
procedure TTextSaveFormat.outpln(const s: string);
begin
  FOutput.Write(fstr(s)+UH_CR+UH_LF);
end;

procedure TTextSaveFormat.outp(const s: string);
begin
  FOutput.Write(fstr(s));
end;

procedure TTextSaveFormat.BeginDocument;
var AFlags: TJwbCreateFlags;
begin
  AFlags := [];
  if not FNoBOM then AFlags := AFlags + [cfBom];
  FOutput := TJwbConvert.CreateNew(FStream,FEncType,AFlags);
end;

procedure TTextSaveFormat.EndDocument;
begin
  FreeAndNil(FOutput);
end;

procedure TTextSaveFormat.AddChars(const s: FString);
var i: integer;
begin
  for i := 1 to flength(s) do begin
    LastChar := fgetch(s,i);
    FOutput.Write(LastChar);
    AtSpace := LastChar=UH_SPACE;
    AtNewline := LastChar=UH_LF;
  end;
end;

{ Word cannot be empty because there's "no word". If it's empty, don't be smart
 about it: print empty line. }
procedure TTextSaveFormat.AddWord(const word,reading: FString; const meaning: string;
  rubyTextBreak: TRubyTextBreakType);
begin
  AddChars(word);
end;

constructor TKanaOnlyFormat.Create(AEncType:integer; AAddSpaces:boolean);
begin
  inherited Create(AEncType);
  FAddSpaces := AAddSpaces;
end;

procedure TKanaOnlyFormat.AddWord(const word,reading: FString; const meaning: string;
  rubyTextBreak: TRubyTextBreakType);
begin
  if FAddSpaces and not (AtSpace or AtNewline) then
    AddChars(UH_SPACE+reading+UH_SPACE)
  else
    AddChars(reading);
end;

procedure TKanjiOnlyFormat.AddWord(const word,reading: FString; const meaning: string;
  rubyTextBreak: TRubyTextBreakType);
begin
  if not (AtSpace or AtNewline) then
    AddChars(UH_SPACE+word+UH_SPACE)
  else
    AddChars(word);
end;

procedure TKanjiKanaFormat.AddWord(const word,reading: FString; const meaning: string;
  rubyTextBreak: TRubyTextBreakType);
begin
  if not (AtSpace or AtNewline) then
    AddChars(UH_SPACE);
  if reading<>'' then
    AddChars(word+UH_SPACE+reading+UH_SPACE)
  else
    AddChars(word); //without space if no reading
end;

constructor THtmlFormat.Create(AOptions: THtmlFormatOptions);
begin
  inherited Create(FILETYPE_UTF8);
  FOptions := AOptions;
  FNoBOM := true; //always no bom with html
end;

procedure THtmlFormat.BeginDocument;
begin
  inherited BeginDocument;
  outpln('<!DOCTYPE html><html><head>');
  outp('<meta http-equiv="Content-Type" content="text/html; charset=utf-8">');
  outpln('</head>');
  outpln('<body>');
  if hoClipFragment in Options then
    outpln(HtmlStartFragment);
  outp('<p>');
end;

{
Fast HTMLEncode.
See http://stackoverflow.com/questions/2968082/is-there-a-delphi-standard-function-for-escaping-html
}
function HTMLEncode3(const Data: string): string;
var iPos, i: Integer;

  procedure Encode(const AStr: String);
  begin
    Move(AStr[1], result[iPos], Length(AStr) * SizeOf(Char));
    Inc(iPos, Length(AStr));
  end;

begin
  SetLength(result, Length(Data) * 6);
  iPos := 1;
  for i := 1 to length(Data) do
    case Data[i] of
      '<': Encode('&lt;');
      '>': Encode('&gt;');
      '&': Encode('&amp;');
      '"': Encode('&quot;');
    else
      result[iPos] := Data[i];
      Inc(iPos);
    end;
  SetLength(result, iPos - 1);
end;

procedure THtmlFormat.AddChars(const s: FString);
var tmp: FString;
begin
  tmp := HTMLEncode3(s);
  repl(tmp,UH_CR+UH_LF,fstr('</p><p>'));
  repl(tmp,UH_LF,fstr('</p><p>'));
  repl(tmp,UH_CR,'');
  repl(tmp,fstr('</p><p>'),fstr('</p>'#13#10'<p>')); //if we did that before, we'd be stuck in infinite replacements
  FOutput.Write(tmp);
end;

procedure THtmlFormat.AddWord(const word,reading: FString; const meaning: string;
  rubyTextBreak: TRubyTextBreakType);
begin
 { Up to two nested ruby brackets, as defined in
  http://www.w3.org/TR/html5/text-level-semantics.html#the-ruby-element }
  if reading<>'' then begin
    if meaning<>'' then
      outp('<ruby title="'+HTMLEncode3(meaning)+'">')
    else
      outp('<ruby>');
  end else
  if meaning<>'' then
    outp('<span title="'+HTMLEncode3(meaning)+'">');

  FOutput.Write(HTMLEncode3(word));

  if (word<>'') and (reading<>'') then begin
    outp('<rt>');
    FOutput.Write(HTMLEncode3(reading));
    outp('</ruby>');
  end else
  if meaning<>'' then
    outp('</span>');
end;

procedure THtmlFormat.EndDocument;
begin
  outpln('</p>');
  if hoClipFragment in Options then
    outpln(HtmlEndFragment);
  outpln('</body></html>');
  inherited EndDocument;
end;

procedure TRubyTextFormat.AddWord(const word,reading: FString; const meaning: string;
  rubyTextBreak: TRubyTextBreakType);
begin
  if rubyTextBreak=btAuto then
    if AtNewline then
      rubyTextBreak := btSkip //first char in a line
    else
    if flength(word)<1 then
      rubyTextBreak := btBreak //empty base requires break
    else
    if EvalChar(fgetch(word,1))=EvalChar(LastChar) then
      rubyTextBreak := btBreak //break when there are two characters of the same type in a row
    else
    if EvalChars(word) and not (1 shl EC_IDG_CHAR) <> 0 then
      rubyTextBreak := btBreak //break when there's something other than kanji in the line,
      //such as 髪の毛 -- or ruby will be applied only to 毛
    else
      rubyTextBreak := btSkip;
  if rubyTextBreak=btBreak then
    FOutput.Write(UH_AORUBY_TEXTBREAK+word)
  else
    FOutput.Write(word);

 //We don't check that reading is not empty, because if it was empty
 //we wouldn't have set inReading, except if it was explicit ruby,
 //in which case we must write it even if empty.
  FOutput.Write(UH_AORUBY_OPEN + reading + UH_AORUBY_CLOSE);
end;

constructor TOpenDocumentContentFormat.Create;
begin
  inherited Create(FILETYPE_UTF8);
end;

procedure TOpenDocumentContentFormat.BeginDocument;
var res: TResourceStream;
begin
  res := TResourceStream.Create(hInstance,'ODT_CONTENT_START',RT_RCDATA);
  try
    FStream.CopyFrom(res,res.Size);
  finally
    FreeAndNil(res);
  end;

  inherited BeginDocument;
  outp('<text:p>');
end;

procedure TOpenDocumentContentFormat.AddChars(const s: FString);
var tmp: FString;
begin
  tmp := HTMLEncode3(s);
  repl(tmp,UH_CR+UH_LF,fstr('</text:p><text:p>'));
  repl(tmp,UH_LF,fstr('</text:p><text:p>'));
  repl(tmp,UH_CR,'');
  repl(tmp,fstr('</text:p><text:p>'),fstr('</text:p>'#10'<text:p>')); //ODT MUST use LF-only linebreaks
  FOutput.Write(tmp);
end;

procedure TOpenDocumentContentFormat.AddWord(const word,reading: FString; const meaning: string;
  rubyTextBreak: TRubyTextBreakType);
begin
  outp('<text:ruby><text:ruby-base>');
  FOutput.Write(word);
  outp('</text:ruby-base><text:ruby-text>');
  FOutput.Write(reading);
  outp('</text:ruby-text></text:ruby>');
end;

procedure TOpenDocumentContentFormat.EndDocument;
var res: TResourceStream;
begin
  outp('</text:p>');
  FOutput.Flush;
  inherited EndDocument;

  res := TResourceStream.Create(hInstance,'ODT_CONTENT_END',RT_RCDATA);
  try
    FStream.CopyFrom(res,res.Size);
  finally
    FreeAndNil(res);
  end;
end;

constructor TOpenDocumentFormat.Create();
begin
  inherited Create(0); //inherited converted not used
  FContentMem := TMemoryStream.Create;
  FContentFormat := TOpenDocumentContentFormat.Create();
  FContentFormat.Stream := FContentMem;
end;

destructor TOpenDocumentFormat.Destroy;
begin
  FreeAndNil(FContentFormat);
  FreeAndNil(FContentMem);
  inherited;
end;

procedure TOpenDocumentFormat.BeginDocument;
begin
 //no inherited
  FContentFormat.BeginDocument;
end;

procedure TOpenDocumentFormat.AddChars(const s: FString);
begin
  FContentFormat.AddChars(s);
end;

procedure TOpenDocumentFormat.AddWord(const word,reading: FString; const meaning: string;
  rubyTextBreak: TRubyTextBreakType);
begin
  FContentFormat.AddWord(word,reading,meaning,rubyTextBreak);
end;

procedure TOpenDocumentFormat.EndDocument;
var arc: TJclZipCompressArchive;
begin
  FContentFormat.EndDocument;

 //Pack into archive
  arc := TJclZipCompressArchive.Create(FStream);
  try
    arc.AddFile('META-INF\manifest.xml',TResourceStream.Create(hInstance,'ODT_MANIFEST',RT_RCDATA),{OwnsStream=}true);
    arc.AddFile('mimetype',TResourceStream.Create(hInstance,'ODT_MIMETYPE',RT_RCDATA),{OwnsStream=}true);
    arc.AddFile('styles.xml',TResourceStream.Create(hInstance,'ODT_STYLES',RT_RCDATA),{OwnsStream=}true);
    FContentMem.Seek(0,soFromBeginning);
    arc.AddFile('content.xml',FContentMem,{OwnsStream=}false);
    arc.Compress;
  finally
    FreeAndNil(arc);
  end;
end;


{ WakanText }

constructor TWakanText.Create;
begin
  inherited;
  doc:=TStringList.Create;
  doctr:=TCharacterPropList.Create;
  docdic:=TStringList.Create;
end;

destructor TWakanText.Destroy;
begin
  doc.Free;
  doctr.Free;
  docdic.Free;
  inherited;
end;

procedure TWakanText.Clear;
begin
  doc.Clear;
  doctr.Clear;
  docdic.Clear;
end;



{ Document }

//Makes sure there's at least one line in document, as required
procedure TWakanText.AdjustDocument;
begin
  if doc.Count=0 then begin
    doc.Add('');
    doctr.AddNewLine();
  end;
end;

//ax is 0-based
function TWakanText.GetDoc(ax,ay:integer):FChar;
begin
  Assert(ay<doc.Count, 'Illegal doc access!');
  if ax>=flength(doc[ay]) then result:=UH_ZERO else result:=fgetch(doc[ay],ax+1);
end;

{ Returns the text of a single character chain:
   [start type] < < < <
Reads from (cx,cy) till chain end. }
function TWakanText.GetDocChain(cx,cy:integer):FString;
begin
  Result := fgetch(doc[cy],cx+1);
  Inc(cx);
  while (cx < flength(doc[cy])) and (doctr[cy].chars[cx].wordstate='<') do begin
    Result := Result + fgetch(doc[cy],cx+1);
    Inc(cx);
  end;
end;

//Returns position just after the last character in the document
function TWakanText.EndOfDocument: TSourcePos;
begin
  AdjustDocument();
  Result.y := doc.Count-1;
  Result.x := flength(doc[Result.y]);
end;

function TWakanText.EndOfLine(const LogicalLineIndex: integer): TSourcePos;
begin
 //Document must already be adjusted as they're using logical lines
  Result.y := LogicalLineIndex;
  Result.x := flength(doc[LogicalLineIndex]);
end;

{ Returns dictionary entry information associated with a word beginning at
 the specified position.
 TWakanText does not implement actual dictionary lookups. Whoever uses it
 must provide their own dictionaries and text analysis tools. }
procedure TWakanText.GetDictionaryEntry(const APos: TSourcePos;
  out kanji, reading: FString; out meaning: string);
begin
  if Assigned(FOnGetDictionaryEntry) then
    FOnGetDictionaryEntry(Self, APos, kanji, reading, meaning)
  else begin
    kanji := '';
    reading := '';
    meaning := '';
  end;
end;

{ Loading/saving }

//Loads classic text file in any encoding.
procedure TWakanText.LoadText(const filename:string;tp:byte;AnnotMode:TTextAnnotMode);
var c: FChar;
  cp: TCharacterProps;
  s: FString; //current line text
  sp: TCharacterLineProps; //current line props

  //Called before we go to the next line,
  //to save whatever needs saving and apply whatever needs applying.
  procedure FinalizeLine;
  begin
    if AnnotMode=amRuby then
      CollapseRuby(s, sp);
    doc.Add(s);
    doctr.AddLine(sp);
    s:='';
    sp.Clear;
  end;

begin
  s := '';
  sp.Clear;

  Conv_Open(filename,tp);
  while Conv_ReadChar(c) do
  begin
   //Default properties for this character
    cp.Reset;
    cp.SetChar('-', 9, 0, 1);
    cp.rubyTextBreak := btAuto;
    cp.ruby := '';
    cp.flags := [];

   //A linebreak breaks everything, even a ruby
    if c=UH_CR then
      FinalizeLine
    else

   //Normal symbol
    if c<>UH_LF then begin
      s:=s+c;
      sp.AddChar(cp);
    end;
  end;
  Conv_Close;
  if s<>'' then
    FinalizeLine();
end;


{ Ruby }

//Receives a string of characters and their properties.
//Parses all aozora-ruby sequences and converts them to annotations, removing from the line
//Has no particular adherrence to TfTranslate, we should probably move it someplace else.
procedure TWakanText.CollapseRuby(var s: FString; var sp: TCharacterLineProps);
var
  idx: integer; //current char index
  c: FChar; //current char

  explicitTextBreak: boolean;
  idxLastTextBreak: integer; //index of last ruby text break (|) on the current line, or 0
  idxRubyOpen: integer;
  inRubyText: boolean;

 //WARNING! Indexing in sp starts with 0, in s with 1, remember that while reading.

  { Tries to guess where's the start of the annotated word.
   Receives:
     idx -- index of an annotation open char -- 0..Length(s)
   Returns: 1..idx-1
   When given 0, returns 0 which means ruby was the first char on the line.
   Rarely returns idx which means, all chars before are already ruby-fied. }
  function GuessLastTextBreak(curidx: integer): integer;
  begin
   //Note: sp.chars is 0-indexes, curidx is 1-indexed
    if curidx<=1 then begin //beginning of the string
      Result := curidx;
      exit;
    end else
    if (sp.chars[(curidx-1)-1].wordstate<>'-') then begin //char before is already annotated
      Result := curidx;
      exit;
    end;
    Dec(curidx); //ok we have a char just before, move to it and start comparing EvalChar
    while (curidx>1) and (sp.chars[(curidx-1)-1].wordstate='-') do //char before is not yet annotated
    begin
     //Might do more guesswork in the future
      if EvalChar(fgetch(s, curidx))<>EvalChar(fgetch(s, curidx-1)) then //and char before is of the same kind
        break;
      Dec(curidx); //go to char before
    end;
    Result := curidx;
  end;

  //Called to finalize currently open ruby
  procedure FinalizeRuby;
  begin
    if idxLastTextBreak<=0 then begin
     //this means ruby was the first char on the line, but we have to store it somehow
     //on save we'll ignore UH_RUBY_PLACEHOLDERs and only write ruby
      s := UH_RUBY_PLACEHOLDER + s;
      with sp.InsertChar(0)^ do
        Reset;
      idxLastTextBreak := 1;
      Inc(idx);
      Inc(idxRubyOpen);
    end else
    if explicitTextBreak then begin
     //Delete the char
      s := fcopy(s, 1, idxLastTextBreak-1) + fcopy(s, idxLastTextBreak+1, flength(s)-idxLastTextBreak);
      sp.DeleteChar(idxLastTextBreak-1);
      Dec(idx);
      Dec(idxRubyOpen);
    end;
    //Now idxLastTextBreak points to first character of an annotated word (or to <<AORUBY_OPEN)

    Assert(idxRubyOpen>=idxLastTextBreak);
    if idxRubyOpen=idxLastTextBreak then begin
     //Again, empty annotated expression
      s := fcopy(s, 1, idxLastTextBreak-1) + UH_RUBY_PLACEHOLDER + fcopy(s, idxLastTextBreak, flength(s)-idxLastTextBreak+1);
      with sp.InsertChar(idxLastTextBreak-1)^ do
        Reset; //init as default char
      Inc(idx);
      Inc(idxRubyOpen);
    end;
    //Now we have at least one character to be annotated

    //Copy annotation itself to char props..
    with sp.chars[idxLastTextBreak-1] do begin
      wordstate := 'F';
      ruby := fcopy(s, idxRubyOpen+1, idx-idxRubyOpen-1);
      if explicitTextBreak then
        rubyTextBreak := btBreak
      else
        rubyTextBreak := btSkip;
      flags := flags + [cfExplicitRuby, cfRoot];
    end;

   //..and delete it
    s := fcopy(s, 1, idxRubyOpen-1) + fcopy(s, idx+1, flength(s)-idx);
    sp.DeleteChars(idxRubyOpen-1, idx-idxRubyOpen+1);
    idx := idxRubyOpen-1;

   //idx is now at the last char of the word to annotate
   //first char is already marked, mark the rest as "word continuation"
    Inc(idxLastTextBreak);
    while idxLastTextBreak<=idx do begin
      with sp.chars[idxLastTextBreak-1] do begin
        wordstate := '<';
        flags := flags + [cfRoot];
      end;
      Inc(idxLastTextBreak);
    end;

   //for now we don't try to guess the tail of the word

   //reset everything
    explicitTextBreak := false;
    idxLastTextBreak := 0;
    idxRubyOpen := 0;
    inRubyText := false;
  end;

begin
  explicitTextBreak := false;
  idxLastTextBreak := 0;
  idxRubyOpen := 0;
  inRubyText := false;

  idx := 1;
  while idx<=flength(s) do begin
    c := fgetch(s,idx);

   //Inside of a ruby skip everything until close
    if inRubyText then begin
      if c=UH_AORUBY_CLOSE then
        FinalizeRuby;
    end else

   //Ruby text break
    if c=UH_AORUBY_TEXTBREAK then begin
      explicitTextBreak := true;
      idxLastTextBreak := idx;
     { And continue like nothing had happened.
      If we encounter another TEXTBREAK, that one will override this one,
      and this one will appear like a normal char.
      On the other hand, if we find <<ruby opener>> only then we'll go back and mark
      everything from here to there as "annotated". }
    end else

   //Ruby opener
    if c=UH_AORUBY_OPEN then
    begin
      if idxLastTextBreak<1 then
        idxLastTextBreak := GuessLastTextBreak(idx);
      idxRubyOpen := idx;
      inRubyText := true;
    end;

   { Ruby comments and tags are not parsed here.
    They are colored dynamically on render; see comments there. }

    Inc(idx);
  end;
end;

{
Save content to a AStream in a given AFormat.
AFormat is freed after completing the task, but disposing of AStream is left
to the caller.

Ruby saving strategy:
- We always save "hard ruby" since if its present then we have loaded it from the file
- As for soft ruby, we save that to the file according to user's choice.
Perhaps we should move Ruby code to ExpandRuby/DropRuby someday, and just write
strings here.
}
procedure TWakanText.SaveText(AAnnotMode:TTextAnnotMode; AFormat:TTextSaveFormat;
  AStream: TStream; ABlock: PTextSelection);
var i,j,k: integer;
  inReading:boolean;
  meaning: string;
  reading,kanji:FString;
  rubyTextBreak: TRubyTextBreakType;
  rootLen: integer; //remaining length of the word's root, if calculated dynamically. <0 means ignore this check
  explicitRuby: boolean;
  sel: TTextSelection;
  chFirst,chLast: integer;

  procedure outp(s: FString);
  begin
    AFormat.AddChars(s)
  end;

  procedure FinalizeRuby();
  begin
    if (AAnnotMode=amRuby) or (explicitRuby and (AAnnotMode<>amNone)) then
      AFormat.AddWord(kanji,reading,meaning,rubyTextBreak)
    else
      AFormat.AddChars(kanji);
    inReading := false;
    reading := '';
    explicitRuby := false;
  end;

begin
  inReading := false;
  rootLen := -1;
  explicitRuby := true;

  if ABlock<>nil then begin
    sel := ABlock^;
    if sel.fromy<0 then sel.fromy := 0;
    if sel.toy<0 then sel.toy := doc.Count-1;
    if sel.fromx<0 then sel.fromx := 0;
    if sel.tox<0 then sel.tox := flength(doc[sel.toy]);
  end else begin
    sel.fromy := 0;
    sel.fromx := 0;
    sel.toy := doc.Count-1;
    sel.tox := flength(doc[sel.toy]);
  end;

  AFormat.Stream := AStream;
  AFormat.BeginDocument;
  for i:=sel.fromy to sel.toy do
  begin
    if i=sel.fromy then
      chFirst := sel.fromx
    else
      chFirst := 0;
    if i=sel.toy then
      chLast := sel.tox
    else
      chLast := flength(doc[i]);
    for j:= chFirst to chLast-1 do
    begin
      if inReading then begin
        Dec(rootLen);
       //End of word
        if (doctr[i].chars[j].wordstate<>'<')
       //End of word root. That's where we output ruby and continue with just printing kana
        or (explicitRuby and not (cfRoot in doctr[i].chars[j].flags))
        or (rootLen=0) then begin
         //=> End of annotated chain
          FinalizeRuby;
         //and we continue through to the "no inReading" case where we might start a new chain
        end else begin
         //Inside of annotated chain -- skip the symbols.
         //We got them into "kanji" from GetDocChain() or GetTextWordInfo() when we first encountered this chain.
        end;
      end; //yep, no 'else'

      if not inReading then begin
       //Explicit ruby load, even if ruby's empty
        if cfExplicitRuby in doctr[i].chars[j].flags then begin
          reading := doctr[i].chars[j].ruby;
          kanji := GetDocChain(j,i);
          if kanji=UH_RUBY_PLACEHOLDER then //there can be only one placeholder
            kanji:='';
          inReading := true;
          explicitRuby := true;
          rootLen := -1;
        end;

       //Implicit ruby load (if explicit is not loaded -- checked by inReading)
        if (AAnnotMode=amRuby) and not inReading then begin
          GetDictionaryEntry(SourcePos(j,i),kanji,reading,meaning);
          inReading := (reading<>'');
          explicitRuby := false;
          if inReading then begin
            rootLen := 0;
           //Explicit ruby has it from file, but with implicit we have to find the word root
            for k := j to min(j+flength(kanji), doctr[i].charcount) do
              if fgetch(kanji, k-j+1)=GetDoc(k,i) then
                Inc(rootLen)
              else
                break;
            if (rootLen=0)
           //we have found no match per kanji, let's try it the other way:
           //assume the whole word is matched and check if there's anything special to the reading
            and (reading<>fcopy(doc[i], j, j+flength(reading)))
            and (AAnnotMode=amRuby) //better annotate something than nothing
              //in amKana it's the reverse: better replace nothing than too much
            then
              rootLen := -1; //annotate whole word

           //In these cases we also hide reading during the rendering.
           //I'm not yet sure what are these
            if (upcase(doctr[i].chars[j].wordstate)<>'F') and (upcase(doctr[i].chars[j].wordstate)<>'D') then
              rootLen := 0;
            if rootLen=0 then
              inReading := false;
          end;
        end;

       //Ruby break -- if we have some kind of reading
        if inReading then
          rubyTextBreak := doctr[i].chars[j].rubyTextBreak;
      end;

      if not inReading then
        outp(GetDoc(j,i));
    end;

    if inReading then
      FinalizeRuby;
    if i<>sel.toy then
      outp(UH_CR+UH_LF);
  end;

  AFormat.EndDocument;
  FreeAndNil(AFormat);
end;

{
Wakan text format.
- "WaKan Translated Text>"+[WAKAN.CHR version]+$3294
- List of dictionaries used in file
  2 bytes: string length in characters
  2*length bytes: string data
 String contains comma-separated dictionary names, first one has index 1:
  EDICT,dictA,dictB
 May contain terminating "$$$$".
- Characters. Each takes 8 bytes:
  1 byte learn-state
  1 byte char-state
  1 byte dictionary index in local list
  3 bytes dictionary entry #
  2 bytes char
 A learn-state of "$" means newline character.

Explicit ruby is currently stored expanded (as text). This is not the best solution,
but otherwise we'd need to change the file format.
}

{ Loads Wakan text from stream into this document, replacing its contents. }
function TWakanText.LoadWakanText(AStream: TStream; ASilent: boolean): boolean;
var s,s2:string;
  s3: TCharacterLineProps;
  i:integer;
  w:word;
  reat:integer;
  buf:array[0..16383] of word;
  ws:array[0..31] of AnsiChar;
  wss:array[0..4091] of AnsiChar;
  wc:widechar;
  l:integer;
  ls:string;
  dp:char;

  chars: FString;
  props: TCharacterLineProps;

  locdics: TStringList;
  locdicidx: integer;
  dicconv: array of integer; //maps local dictionaries to document dictionaries

begin
  Clear;

  reat:=AStream.Read(w,2);
  if (reat<1) or (w<>$f1ff) then
    raise EBadWakanTextFormat.Create(_l('#00679^eThis is not a valid UTF-8 or JTT file.'));

  AStream.Read(ws,32);
  s:=string(ws);
  if copy(s,1,22)<>'WaKan Translated Text>'then
    raise EBadWakanTextFormat.Create(_l('#00679^eThis is not a valid UTF-8 or JTT file.'));
  delete(s,1,22);

  AStream.Read(w,2);
  if w<>3294 then
    raise EBadWakanTextFormat.Create(_l('#00681^eThis JTT file was created by '
      +'old version of WaKan.'#13'It is not compatible with the current version.'));

  locdics := TStringList.Create;
  try
   //Read local dictionaries
    AStream.Read(w,2);
    AStream.Read(wss,w*2);
    wss[w*2]:=#0;
    s:=string(wss);
    while (s<>'') and (s[1]<>'$') do
    begin
      s2:=copy(s,1,pos(',',s)-1);
      delete(s,1,pos(',',s));
      locdics.Add(s2);
    end;

   //Create dictionary conversion table
    SetLength(dicconv, locdics.Count);
    for i := 0 to Length(dicconv) - 1 do
      if not docdic.Find(locdics[i], dicconv[i]) then
        dicconv[i] := docdic.Add(locdics[i]);
  finally
    FreeAndNil(locdics);
  end;

 //We will build a text in these variables, then pass it to PasteText.
  chars := '';
  props.Clear;

  s:='';
  s3.Clear;
  reat := AStream.Read(buf,Length(buf)*SizeOf(word));
  while reat>0 do
  begin

    for i:=0 to (reat div 8)-1 do
    begin
      dp:=chr(buf[i*4] mod 256);
      if dp='$'then
      begin
        chars := chars + s + UH_CR + UH_LF;
        props.AddChars(s3);
        props.AddChar('-', 9, 0, 1);
        props.AddChar('-', 9, 0, 1);
        s:='';
        s3.Clear;
      end else
      begin
        wc:=widechar(buf[i*4+1]);
        l:=(buf[i*4+2] mod 256)*65536+buf[i*4+3];
        ls:=inttostr(l);
        while length(ls)<6 do ls:='0'+ls;
        s:=s+fstr(wc);
        if length(dp+inttostr(buf[i*4] div 256)+ls+chr(buf[i*4+2] div 256))<>9 then begin
          Application.MessageBox(PChar('<<'+dp+'--'+inttostr(buf[i*4] div 256)+'--'+ls+'--'+chr(buf[i*4+2] div 256)+'>>'),
            PChar(''));
        end;

       //dic index is apparently stored as character (ex. '1', '2') and we need it as int
        locdicidx := buf[i*4+2] div 256 - ord('0');
       //convert to document indexes
        if (locdicidx<0) or (locdicidx>=Length(dicconv)) then
          locdicidx := 0 //trust no one
          { this in fact might happen because Wakan by default sets dicidx to 1,
           even if there's no dictionaries }
        else
          locdicidx := dicconv[locdicidx];
        s3.AddChar(dp, buf[i*4] div 256, l, locdicidx);
      end;
    end;

    reat := AStream.Read(buf,Length(buf)*SizeOf(word));
  end;

  if s<>'' then
  begin
    chars := chars + s;
    props.AddChars(s3);
  end;

  PasteText(SourcePos(0,0),chars,props,amRuby); //paste as is, do not expand ruby
  Result := true;
end;

procedure TWakanText.LoadWakanText(const AFilename:string);
var ms: TStream;
begin
  ms := TStreamReader.Create(
    TFileStream.Create(AFilename, fmOpenRead or fmShareDenyWrite),
    {OwnsStream=}true
  );
  try
    LoadWakanText(ms);
  finally
    FreeAndNil(ms);
  end;
end;

procedure TWakanText.SaveWakanText(AStream: TStream; ABlock: PTextSelection = nil);
var i,j,bc:integer;
  buf:array[0..16383] of word;
  sig:word;
  s:AnsiString;
  w:word;
  cp: PCharacterProps;
  sel: TTextSelection;
  chFirst,chLast: integer;

  inReading:boolean;
  reading,kanji:FString;
  rubyTextBreak: TRubyTextBreakType;

  procedure PutBC(bc0, bc1, bc2, bc3: word);
  begin
    buf[bc+0]:=bc0;
    buf[bc+1]:=bc1;
    buf[bc+2]:=bc2;
    buf[bc+3]:=bc3;
    inc(bc,4);
    if bc=16384 then
    begin
      AStream.Write(buf,bc*2);
      bc:=0;
    end;
  end;

  procedure PutChar(const ch: WideChar; wordstate: char; learnstate: integer; docidx: integer; docdic: integer);
  begin
    PutBC(
      ord(wordstate)+learnstate*256,
      word(ch),
      docidx div 65536+(Ord('0')+docdic)*256, //apparently dic # is stored as a char ('1','2'...)
      docidx mod 65536
    );
  end;

  procedure PutChars(const str: FString);
  var i: integer;
    tmp: UnicodeString;
  begin
    tmp := fstrtouni(str);
    for i := 1 to flength(tmp) do
      PutChar(tmp[i], '-', 9, 0, 1); //standard default char
  end;

  procedure PutWord(const kanji, reading: FString; textBreak: TRubyTextBreakType);
  begin
    if textBreak<>btSkip then
      PutChars(UH_AORUBY_TEXTBREAK);
    PutChars(kanji);
    PutChars(UH_AORUBY_OPEN);
    PutChars(reading);
    PutChars(UH_AORUBY_CLOSE);
  end;

  procedure FinalizeRuby();
  begin
    PutWord(kanji,reading,rubyTextBreak);
    inReading := false;
    reading := '';
  end;

begin
  sig:=$f1ff;
  AStream.Write(sig,2);

  inReading := false;
  reading := '';
  kanji := '';

  s:=AnsiString('WaKan Translated Text>'+WakanDatestamp(CharDataProps.DicBuildDate));
  while length(s)<32 do s:=s+' ';
  AStream.Write(s[1],32);

  w:=3294;
  AStream.Write(w,2);

  s:='';
  for i:=0 to docdic.Count-1 do s:=s+AnsiString(docdic[i])+',';
  w:=(length(s)+1) div 2;
  AStream.Write(w,2);
  s:=s+'$$$$';
  AStream.Write(s[1],w*2);

  if ABlock<>nil then begin
    sel := ABlock^;
    if sel.fromy<0 then sel.fromy := 0;
    if sel.toy<0 then sel.toy := doc.Count-1;
    if sel.fromx<0 then sel.fromx := 0;
    if sel.tox<0 then sel.tox := flength(doc[sel.toy]);
  end else begin
    sel.fromy := 0;
    sel.fromx := 0;
    sel.toy := doc.Count-1;
    sel.tox := flength(doc[sel.toy]);
  end;

  bc:=0;
  for i:=sel.fromy to sel.toy do
  begin
    if i=sel.fromy then
      chFirst := sel.fromx
    else
      chFirst := 0;
    if i=sel.toy then
      chLast := sel.tox
    else
      chLast := flength(doc[i]);
    for j:= chFirst to chLast-1 do
    begin
     //NOTE: Similar to what happens in SaveText(). Perhaps one day we should
     //  unify all saving functions. (Or made one the enhancement of another).

      if inReading then begin
       //End of word
        if (doctr[i].chars[j].wordstate<>'<')
       //End of word root. That's where we output ruby and continue with just printing kana
        or not (cfRoot in doctr[i].chars[j].flags) then begin
         //=> End of annotated chain
          FinalizeRuby;
         //and we continue through to the "no inReading" case where we might start a new chain
        end else begin
         //Inside of annotated chain -- skip the symbols.
         //We got them into "kanji" from GetDocChain() or GetTextWordInfo() when we first encountered this chain.
        end;
      end; //yep, no 'else'

      if not inReading then begin
       //Explicit ruby load, even if ruby's empty
        if cfExplicitRuby in doctr[i].chars[j].flags then begin
          reading := doctr[i].chars[j].ruby;
          kanji := GetDocChain(j,i);
          if kanji=UH_RUBY_PLACEHOLDER then //there can be only one placeholder
            kanji:='';
          inReading := true;
        end;

       //Implicit ruby is preserved directly in this format -- leave for later

       //Ruby break -- if we have some kind of reading
        if inReading then
          rubyTextBreak := doctr[i].chars[j].rubyTextBreak;
      end;

      if not inReading then begin
        cp := @doctr[i].chars[j];
        PutChar(fstrtouni(GetDoc(j,i))[1], cp.wordstate, cp.learnstate,
          cp.dicidx, cp.docdic);
      end;

    end;

    if inReading then
      FinalizeRuby;
    if i<>sel.toy then
      PutBC(ord('$'), 0, 0, 0);
  end;
  AStream.Write(buf,bc*2);
end;

procedure TWakanText.SaveWakanText(const AFilename:string);
var ms: TStream;
begin
  ms := TStreamWriter.Create(
    TFileStream.Create(AFilename, fmCreate),
    {OwnsStream=}true
  );
  try
    SaveWakanText(ms, nil);
  finally
    FreeAndNil(ms);
  end;
end;



{ Editing }

procedure TWakanText.CheckTransCont(x,y:integer);
var cp: PCharacterProps;
begin
  cp := @doctr[y].chars[x];
  while cp.wordstate='<'do
  begin
    cp.SetChar('-', 9, 0, 1);
    inc(x);
    cp := @doctr[y].chars[x];
  end;
end;

procedure TWakanText.AddLine(const chars: FString; const props: PCharacterLineProps);
var _props: TCharacterLineProps;
  i:integer;
begin
  Lines.Add(chars);
  if props<>nil then
    PropertyLines.AddLine(props^)
  else begin
    _props.Clear;
    _props.AddChars(flength(chars));
    for i := 1 to flength(chars) do
      _props.chars[i-1].SetChar('-', 9, 0, 1);
    PropertyLines.AddLine(_props);
  end;
end;

procedure TWakanText.SplitLine(x,y:integer);
var ins:string;
  lp: TCharacterLineProps;
begin
  if flength(doc[y])<=x then
  begin
    if doc.Count-1=y then
    begin
      doc.Add('');
      doctr.AddNewLine();
    end else
    begin
      doc.Insert(y+1,'');
      doctr.InsertNewLine(y+1);
    end;
  end else
  begin
    ins:=fcopy(doc[y],x+1,flength(doc[y])-x);
    lp := doctr[y].CopySubstr(x,0);
    if doc.Count-1=y then
    begin
      doc.Add(ins);
      doctr.AddLine(lp);
    end else
    begin
      doc.Insert(y+1,ins);
      doctr.InsertLine(y+1,lp);
    end;
    doc[y]:=fcopy(doc[y],1,x);
    doctr[y].DeleteChars(x);
    CheckTransCont(0,y+1);
  end;
end;

{ y is the # of the line into which you want to merge the following one }
procedure TWakanText.JoinLine(y:integer);
begin
  if (y<0) or (y>=doc.Count-1) then exit;
  doc[y]:=doc[y]+doc[y+1];
  doctr[y].AddChars(doctr[y+1]^);
  doc.delete(y+1);
  doctr.DeleteLine(y+1);
end;

procedure TWakanText.DeleteCharacter(const APos: TSourcePos);
begin
  DeleteCharacter(APos.x, APos.y);
end;

procedure TWakanText.DeleteCharacter(x,y:integer);
begin
  if flength(doc[y])<=x then JoinLine(y) else
  begin
    doc[y]:=fcopy(doc[y],1,x)+fcopy(doc[y],x+2,flength(doc[y])-x-1);
    doctr[y].DeleteChar(x);
    CheckTransCont(x,y);
  end;
end;

procedure TWakanText.DeleteLine(AIndex: integer);
begin
  doc.Delete(AIndex);
  doctr.DeleteLine(AIndex);
end;

{ Deletes the text contained between [block.from and block.to), last position
 not inclusive.
 Lines strictly inside the block (fromy < y < toy), are deleted completely. }
procedure TWakanText.DeleteBlock(block: TSourceBlock);
var i: integer;
begin
  if block.fromy>block.toy then exit;
  if block.fromy=block.toy then begin
    if block.fromx<=0 then block.fromx:=0;
    if (block.toy<0) or (block.toy>=Lines.Count) then exit; //invalid line
    if block.tox>=Length(Lines[block.toy]) then block.tox:=Length(Lines[block.toy]);
    if block.fromx>block.tox then exit; //empty block
    doc[block.fromy]:=fcopy(doc[block.fromy],1,block.fromx)
      +fcopy(doc[block.fromy],block.tox+1,flength(doc[block.fromy])-block.tox);
    doctr[block.fromy].DeleteChars(block.fromx, block.tox-block.fromx);
  end else begin
   { We have to figure out which lines are "strictly" inside the block later,
    so we cannot just fix the block outright if it's oversized. }
    if block.fromy>=0 then begin
      doc[block.fromy]:=fcopy(doc[block.fromy],1,block.fromx);
      if block.fromx < doctr[block.fromy].charcount then
        doctr[block.fromy].DeleteChars(block.fromx);
    end else
      block.fromy:=-1; //one less than allowed
    if block.toy<Lines.Count then begin
      doc[block.toy]:=fcopy(doc[block.toy],block.tox+1,flength(doc[block.toy])-block.tox);
      if block.tox < doctr[block.toy].charcount then
        doctr[block.toy].DeleteChars(0, block.tox)
      else
        if doctr[block.toy].charcount>0 then
          doctr[block.toy].DeleteChars(0);
    end else
      block.toy:=Lines.Count; //one more than allowed
   { Enumerate only internal lines }
    for i:=block.fromy+1 to block.toy-1 do
    begin
      doc.Delete(block.fromy+1);
      doctr.DeleteLine(block.fromy+1);
    end;
    JoinLine(block.fromy);
  end;
end;

{ Receives a string contanining multiple lines of text separated by CRLF.
 Splits ti into lines, inserts at the specified position. Expands ruby if
 AnnotMode dictates so.

 Props may be omitted, in which case default props are used for all symbols.

 Returns SourcePos of the end of the inserted text in AEndPos }
procedure TWakanText.PasteText(const APos: TSourcePos; const chars: FString;
  const props: TCharacterLineProps; AnnotMode: TTextAnnotMode;
  AEndPos: PSourcePos);
var s: string;
  sp: TCharacterLineProps;
  y:integer;
  i:integer;
  l:integer;

  procedure FinalizeLine;
  begin
    if AnnotMode=amRuby then
      CollapseRuby(s, sp);
    doc[y] := doc[y] + s;
    doctr[y].AddChars(sp);
    s := '';
    sp.Clear;
  end;

  procedure InsertNewLine;
  begin
    inc(y);
    doc.Insert(y,'');
    doctr.InsertNewLine(y);
  end;

begin
  s := '';
  sp.Clear;

 { This function is sometimes used to load text, at which point there's not even
  a single default line available and SplitLine would die. }
  if APos.y >= doc.Count then begin
    doc.Add('');
    doctr.AddNewLine;
  end else
    SplitLine(APos.x,APos.y);

  y:=APos.y;
  for i:=1 to flength(chars) do
  begin
    if fgetch(chars,i)=UH_LF then begin
      FinalizeLine;
      InsertNewLine;
    end else
    if fgetch(chars,i)<>UH_CR then
    begin
      s:=s+fgetch(chars,i);
      if props.charcount>i-1 then
        sp.AddChar(props.chars[i-1])
      else
        sp.AddChar('-', 9, 0, 1);
    end;
  end;

  if Length(s)>0 then
    FinalizeLine;

  l:=flength(doc[y]);
  JoinLine(y);
  if AEndPos<>nil then
    AEndPos^ := SourcePos(l,y);
end;

procedure TWakanText.PasteDoc(const APos: TSourcePos; const ADoc: TWakanText;
  AEndPos: PSourcePos = nil);
var dicconv: array of integer; //maps document dictionaries to local dictionaries
  i, j: integer;
  y, x_s:integer;

begin
 //Create dictionary conversion table
  SetLength(dicconv, ADoc.docdic.Count);
  for i := 0 to Length(dicconv) - 1 do
    if not docdic.Find(ADoc.docdic[i], dicconv[i]) then
      dicconv[i] := docdic.Add(ADoc.docdic[i]);

 { This function is sometimes used to load text, at which point there's not even
  a single default line available and SplitLine would die. }
  if APos.y >= doc.Count then begin
    doc.Add('');
    doctr.AddNewLine;
  end else
    SplitLine(APos.x,APos.y);

  y := APos.y; //in case ADoc has no lines at all
  for i := 0 to ADoc.Lines.Count-1 do begin
    y := APos.y+i;
    if i=0 then
      x_s := flength(doc[y])
    else begin
      doc.Insert(y,'');
      doctr.InsertNewLine(y);
      x_s := 0;
    end;

    doc[y] := doc[y] + ADoc.Lines[i];
    doctr[y].AddChars(ADoc.PropertyLines[i]^);

   //Fix dictionary references
    for j := x_s to doctr[y].charcount-1 do
      if doctr[y].chars[j].dicidx>0 then
        doctr[y].chars[j].docdic := dicconv[doctr[y].chars[j].docdic]
      else
        doctr[y].chars[j].docdic := 0; //just in case
  end;

  x_s:=flength(doc[y]);
  JoinLine(y);
  if AEndPos<>nil then
    AEndPos^ := SourcePos(x_s,y);
end;

end.
