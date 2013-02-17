unit JWBTranslate;
{
Text editor and translator.

Contents
============
doc: TStringList -- contains document text, line by line.
doctr: TPropList -- character properties, line by line.
They are synchronized, except doc[y] lines are indexed starting from 1,
and doctr[y] starting from 0.
All operations on one must also similarly change the other.

Layout
============
"linl" contains graphical line descriptions:
  ys = source line index in "doc"
  xs = first character in source line
  len = length
Latin characters are considered "half-width" and require only half the slot
kanji and kana uses.

Cursor
============
Cursor position is stored in curx, cury. Both are zero-based:
  x==0         => before the first character
  x==len(line) => after the last character
When doing selection, mouse drag start is kept in blockx, blocky.

CalcBlockFromTo() converts that to:
  blockfromy      first line in selection
  blockfromx      first selected char
  blocktox        last line in selection
  blocktoy        last selected char
All inclusive.

Breaking changes
==================
- Graphical lines were previously declared one character longer than needed.
 This was WRONG and was fixed, but there could still be code which relied on that.
}

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, RXCtrls, ExtCtrls, Buttons, JWBUtils, JWBStrings, JWBUser,
  JWBDicSearch, JWBConvert, JclCompression;

//If enabled, support multithreaded translation
{$DEFINE MTHREAD_SUPPORT}

{$IFDEF DEBUG} //don't do in release versions

  //Display a window showing how much time Auto-TL took
  {$DEFINE TLSPEEDREPORT}

{$ENDIF}

type
 { Character position in source text. }
  TSourcePos = record
    y: integer; //line, 0-based
    x: integer  //char, 0-based
  end;
  PSourcePos = ^TSourcePos;

 { Cursor position in graphical lines }
  TCursorPos = record
    y: integer; //line, 0-based
    x: integer; //char, 0-based: [0..length(line)], length(line)=after last char
  end;
  PCursorPos = ^TCursorPos;

  TSelection = record
    fromy: integer;
    fromx: integer;
    toy: integer;
    tox: integer;
  end;
  PSelection = ^TSelection;
  TLineSelection = TSelection; { Text selection in graphical lines. }
  TTextSelection = TSelection; { Text selection in logical coordinates }
  PTextSelection = ^TTextSelection;

  TTextAnnotMode = (
    amDefault,
      //do not load ruby
      //save only those annotations which were loaded from ruby
    amRuby
      //load ruby
      //save generated kana as aozora-ruby
  );

  TSetWordTransFlag = (
    tfScanParticle,
    tfManuallyChosen
  );
  TSetWordTransFlags = set of TSetWordTransFlag;

  TTranslationThread = class;
  TTranslationThreads = array of TTranslationThread;

  TTextSaveFormat = class;

  TfTranslate = class(TForm)
    Shape10: TShape;
    EditorPaintBox: TPaintBox;
    ListBox1: TListBox;
    EditorScrollBar: TScrollBar;
    Bevel1: TBevel;
    sbFileOpen: TSpeedButton;
    sbFileSave: TSpeedButton;
    sbFileNew: TSpeedButton;
    sbClipCut: TSpeedButton;
    sbAsciiMode: TSpeedButton;
    sbKanjiMode: TSpeedButton;
    sbKanaMode: TSpeedButton;
    sbDisplayReading: TSpeedButton;
    sbDisplayMeaning: TSpeedButton;
    sbClearTranslation: TSpeedButton;
    sbAutoTranslate: TSpeedButton;
    sbSetTranslation: TSpeedButton;
    sbPrint: TSpeedButton;
    Bevel2: TBevel;
    sbClipCopy: TSpeedButton;
    sbClipPaste: TSpeedButton;
    lblFilename: TLabel;
    Bevel3: TBevel;
    lblControlsHint: TLabel;
    sbDockDictionary: TSpeedButton;
    sbDockKanjiDetails: TSpeedButton;
    sbUseTlColors: TSpeedButton;
    BlinkCursorTimer: TTimer;
    OpenTextDialog: TOpenDialog;
    SaveTextDialog: TSaveDialog;
    cbFontSize: TComboBox;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure FormHide(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure FormActivate(Sender: TObject);
    procedure FormDeactivate(Sender: TObject);
    procedure FormResize(Sender: TObject);
    procedure FormMouseWheelUp(Sender: TObject; Shift: TShiftState;
      MousePos: TPoint; var Handled: Boolean);
    procedure FormMouseWheelDown(Sender: TObject; Shift: TShiftState;
      MousePos: TPoint; var Handled: Boolean);
    procedure EditorPaintBoxClick(Sender: TObject);
    procedure EditorPaintBoxMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure EditorPaintBoxPaint(Sender: TObject);
    procedure EditorScrollBarChange(Sender: TObject);
    procedure EditorPaintBoxMouseMove(Sender: TObject; Shift: TShiftState; X,
      Y: Integer);
    procedure EditorPaintBoxDblClick(Sender: TObject);
    procedure ListBox1Enter(Sender: TObject);
    procedure ListBox1Exit(Sender: TObject);
    procedure ListBox1KeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure ListBox1KeyPress(Sender: TObject; var Key: Char);
    procedure sbDisplayReadingClick(Sender: TObject);
    procedure sbDisplayMeaningClick(Sender: TObject);
    procedure sbClearTranslationClick(Sender: TObject);
    procedure sbAutoTranslateClick(Sender: TObject);
    procedure sbSetTranslationClick(Sender: TObject);
    procedure sbPrintClick(Sender: TObject);
    procedure sbFileOpenClick(Sender: TObject);
    procedure sbFileSaveClick(Sender: TObject);
    procedure sbFileNewClick(Sender: TObject);
    procedure sbClipCutClick(Sender: TObject);
    procedure sbClipCopyClick(Sender: TObject);
    procedure sbClipPasteClick(Sender: TObject);
    procedure sbKanjiModeClick(Sender: TObject);
    procedure sbKanaModeClick(Sender: TObject);
    procedure sbAsciiModeClick(Sender: TObject);
    procedure sbDockKanjiDetailsClick(Sender: TObject);
    procedure sbDockDictionaryClick(Sender: TObject);
    procedure sbUseTlColorsClick(Sender: TObject);
    procedure BlinkCursorTimerTimer(Sender: TObject);
    procedure cbFontSizeChange(Sender: TObject);
    procedure cbFontSizeExit(Sender: TObject);
    procedure cbFontSizeKeyPress(Sender: TObject; var Key: Char);

  protected
    CopyShort, CopyAsShort,
    CutShort, PasteShort,
    AllShort: TShortCut;
    CF_HTML: integer;
    CF_ODT: integer;
    CF_WAKAN: integer;
    function GenerateHtmlClipHeader(const lenHtml: integer;
      const startFragment, endFragment: integer): UnicodeString;
  public
    procedure CopyAs; //extended copy to clipboard
    function CopyAsText: UnicodeString;
    function CopyAsHtml: Utf8String;
    function CopyAsClipHtml: Utf8String;
    function CopyAsRuby: UnicodeString;
    function CopyAsOpenDocumentTextContent: Utf8String;
    function CopyAsOpenDocument: TMemoryStream;
    function CopyAsWakanText: TMemoryStream;

  public
    linl: TGraphicalLineList; //lines as they show on screen
    docfilename:string;
    doctp:byte;

  protected //Mostly repainting
    EditorBitmap:TBitmap;
    procedure MakeEditorBitmap;
    procedure PasteEditorBitmap;
    procedure RecalculateGraphicalLines(ll: TGraphicalLineList; rs: integer;
      screenw: integer; vert: boolean);
    procedure RenderText(x,y:integer;canvas:TCanvas;l,t,w,h:integer;
      ll:TGraphicalLineList;var printl,xsiz,ycnt:integer;printing,onlylinl:boolean);
  public
    mustrepaint:boolean;
    procedure RepaintText(dolook:boolean=false);
    procedure ShowText(dolook:boolean);

  protected //Unsorted
    procedure HandleWheel(down:boolean);
    procedure CalcBlockFromTo(backtrack:boolean);
    function SetWordTrans(x,y:integer;flags:TSetWordTransFlags;gridfirst:boolean):integer; overload;
    function SetWordTrans(x,y:integer;flags:TSetWordTransFlags;const word:PSearchResult):integer; overload;
    procedure DrawCursor(blink:boolean);
    procedure DrawBlock(Canvas: TCanvas);
    procedure CheckTransCont(x,y:integer);
    procedure SplitLine(x,y:integer);
    procedure JoinLine(y:integer);
    procedure DeleteCharacter(x,y:integer);
    procedure RefreshLines;
    procedure CopySelection(format:TTextSaveFormat;stream:TStream;
      AnnotMode:TTextAnnotMode=amRuby);
    procedure DeleteSelection;
    procedure PasteOp;
    procedure PasteText(const chars: FString; const props: TCharacterLineProps;
      AnnotMode: TTextAnnotMode);
    function PosToWidth(x,y:integer):integer;
    function WidthToPos(x,y:integer):integer;
    function HalfUnitsToTextPos(x,y: integer):integer;
    function GetClosestCursorPos(x,y:integer):TCursorPos;
  public
    procedure CalcMouseCoords(x,y:integer;var rx,ry:integer);

  protected
    FFontSize: integer;
    procedure SetFontSize(Value: integer);
    procedure cbFontSizeGuessItem(Value: string);
  public
    property FontSize: integer read FFontSize write SetFontSize;

  public //Document
    doc: TStringList; //document lines
    doctr: TCharacterPropList; //character property list (translation, wordstate, etc)
    docdic: TStringList;
    function GetDoc(ax,ay:integer):FChar;
    function GetDocChain(cx,cy:integer):FString;
    function IsHalfWidth(x,y:integer):boolean;
    function GetDocWord(x,y:integer;var wordtype:integer;stopuser:boolean):string;
    procedure GetTextWordInfo(cx,cy:integer;var meaning:string;var reading,kanji:FString);

  protected
    oldcur: TCursorPos; //last cursor position, where it was drawn.
    cur: TCursorPos; //cursor position (maybe not drawn yet, may differ from where cursor is drawn -- see CursorScreenX/Y)
    procedure SetCurPos(x,y:integer);
  public
    view:integer; //index of a first visible graphical line
    rview: TSourcePos; //logical coordinates of a start of a first visible graphical line
    rcur: TSourcePos; //cursor position in logical coordinates (cursor is before this char)
    cursorposcache:integer; //cursor X in pixels, from last DrawCursor. -1 means recalculate
    lastxsiz: integer; //size of one half-char in pixels, at the time of last render
    lastycnt,printl:integer;
    //Actual cursor position --- differs from (curx,cury) if we're at the end of the text
    function CursorScreenX:integer;
    function CursorScreenY:integer;
    procedure SelectAll;

  protected //Insert buffer
    insertbuffer:string; //collects keypresses
    resolvebuffer:boolean; //set to true before doing ResolveInsert to replace kana with kanji suggestion, false to keep input intact
    procedure DisplayInsert(const convins:FString;transins:TCharacterPropArray;leaveinserted:boolean);
    procedure ResolveInsert(final:boolean);
    procedure InsertCharacter(c:char);
    procedure ClearInsBlock;
    procedure CloseInsert;
    function TryReleaseCursorFromInsert: boolean;
  public
   //Unfortunately some stuff is used from elswehere
    ins: TSourcePos; //editor aftertouch --- after we have inserted the word, it's highlighted
    inslen: integer;
    buffertype:char;
    function GetInsertKana(display:boolean):FString;

  protected //Ruby stuff
    procedure CollapseRuby(var s: FString; var sp: TCharacterLineProps);

  protected //File opening/saving
    FFileChanged: boolean;
    FFullTextTranslated: boolean; //applied full text translation at least once since loading
     //this is needed for saving in Kana mode -- we don't show a reminder if it's obvious the text was translated
    SaveAnnotMode: TTextAnnotMode; //if we have saved the file once, we remember the choice
    procedure SetFileChanged(Value: boolean);
    procedure LoadText(const filename:string;tp:byte;AnnotMode:TTextAnnotMode);
    function LoadWakanText(stream: TStream; silent: boolean = false): boolean; overload;
    procedure LoadWakanText(const filename:string); overload;
    procedure SaveWakanText(stream: TStream; block: PTextSelection = nil); overload;
    procedure SaveWakanText(const filename:string); overload;
  public //File open/save
    procedure ClearEditor;
    procedure OpenAnyFile(filename:string);
    procedure OpenFile(filename:string;tp:byte);
    procedure SaveToFile(filename:string;tp:byte;AnnotMode:TTextAnnotMode);
    procedure SaveText(AnnotMode:TTextAnnotMode; format: TTextSaveFormat;
      stream: TStream; block: PTextSelection = nil);
    function SaveAs: boolean;
    function CommitFile:boolean;
    property FileChanged: boolean read FFileChanged write SetFileChanged;
    property FullTextTranslated: boolean read FFullTextTranslated write FFullTextTranslated;

  protected
   {$IFDEF MTHREAD_SUPPORT}
    function CreateTranslationThreads(abfromy, abtoy: integer; var y: integer): TTranslationThreads;
   {$ENDIF}
    procedure AutoTranslateLine(y: integer; x_bg, x_en: integer;
      req: TDicSearchRequest; dicsl: TSearchResults);
  public
    procedure AutoTranslate();
    procedure SetTranslation();

  end;

  TTranslationThread = class(TThread)
  protected
    req: TDicSearchRequest;
    dicsl: TSearchResults;
    blockfromy: integer;
    blocktoy: integer;
    blockfromx: integer;
    blocktox: integer;
  public
    constructor Create(ablockfromy, ablocktoy: integer);
    destructor Destroy; override;
    procedure Execute; override;
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
    constructor Create(AEncType: integer);
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

var
  fTranslate: TfTranslate;

const
  FontSizeSmall = 8;
  FontSizeMedium = 12;
  FontSizeLarge = 16;

implementation

uses JWBMenu, JWBHint, JWBKanjiDetails, JWBKanji, JWBStatistics,
  JWBSettings, JWBPrint, StdPrompt, JWBUnit,
  JWBCategories, ComCtrls, JWBDic, JWBEdictMarkers,
  JWBUserData, StreamUtils;

{$R *.DFM}

function SourcePos(x,y: integer): TSourcePos; {$IFDEF INLINE}inline;{$ENDIF}
begin
  Result.x := x;
  Result.y := y;
end;

function CursorPos(x,y: integer): TCursorPos; {$IFDEF INLINE}inline;{$ENDIF}
begin
  Result.x := x;
  Result.y := y;
end;

function Selection(fromx, fromy, tox, toy: integer): TSelection; {$IFDEF INLINE}inline;{$ENDIF}
begin
  Result.fromy := fromy;
  Result.fromx := fromx;
  Result.toy := toy;
  Result.toy := tox;
end;

var
 //Selection block in 0-coords [0..doc.Count]x[0..flen(line)-1]
  block: TTextSelection;
 //Selection block last time it was repainted (allows us to only repaint changed parts)
  oldblock: TTextSelection;

 //When selecting, the position where we started dragging mouse.
 //Selection block is generated from this on mouse-release.
  dragstart: TSourcePos; //cursor was before this char when drag started

 //Last character which the mouse was over
  lastmm:TCursorPos;

  insconfirmed:boolean;
  shiftpressed:boolean;
  cursorend:boolean;
  priorkanji:string;
  cursorblinked:boolean; //cursor is currently in "hidden" blink state

//ax is 0-based
function TfTranslate.GetDoc(ax,ay:integer):FChar;
begin
  if ay>=doc.Count then showmessage('Illegal doc access!');
  if ax>=flength(doc[ay]) then result:=UH_ZERO else result:=fgetch(doc[ay],ax+1);
end;

{ Returns the text of a single character chain:
   [start type] < < < <
Reads from (cx,cy) till chain end. }
function TfTranslate.GetDocChain(cx,cy:integer):FString;
begin
  Result := fgetch(doc[cy],cx+1);
  Inc(cx);
  while (cx < flength(doc[cy])) and (doctr[cy].chars[cx].wordstate='<') do begin
    Result := Result + fgetch(doc[cy],cx+1);
    Inc(cx);
  end;
end;

function TfTranslate.IsHalfWidth(x,y:integer):boolean;
begin
  result:=IsHalfWidthChar(GetDoc(x,y));
end;


var
 //Printing vars
  plinl:TGraphicalLineList; //graphical lines for printing
  printpl:integer;

function GetPageNum(canvas:TCanvas; width,height:integer; userdata:pointer):integer;
var pl,xs,yc:integer;
begin
  plinl.Clear;
  fTranslate.RenderText(0,0,canvas,width div 50,height div 50,width-width div 25,
    height-height div 25,plinl,pl,xs,yc,true,true);
  printpl:=pl;
  result:=((plinl.Count-1) div pl)+1;
  if result<1 then result:=1;
end;

procedure DrawPage(canvas:TCanvas; pagenum:integer; width,height,origwidth,origheight:integer; userdata:pointer);
var pl,xs,yc:integer;
begin
  if plinl.Count<=(pagenum-1)*printpl then exit;
  fTranslate.RenderText(
    plinl[(pagenum-1)*printpl].xs,
    plinl[(pagenum-1)*printpl].ys,
    canvas,width div 50,height div 50,width-width div 25,height-height div 25,
    plinl,pl,xs,yc,true,false);
end;


constructor TTextSaveFormat.Create(AEncType: integer);
begin
  inherited Create;
  FEncType := AEncType;
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


procedure TfTranslate.FormCreate(Sender: TObject);
begin
  doc:=TStringList.Create;
  doctr:=TCharacterPropList.Create;
  docdic:=TStringList.Create;

  docfilename:='';
  doctp:=0;
  FileChanged:=false;
  FullTextTranslated:=false;

  view:=0;
  rview := SourcePos(-1, -1);
  cur := CursorPos(-1, -1);
  oldcur := cur;
  rcur := SourcePos(-1, -1);
  ins := SourcePos(-1, -1);
  inslen:=0;
  cursorposcache:=-1;
  lastxsiz:=16;
  lastycnt:=2;
  printl:=1;
  mustrepaint:=true;

  FFontSize := FontSizeMedium;

  linl:=TGraphicalLineList.Create;
  plinl:=TGraphicalLineList.Create;
  CopyShort:=fMenu.aEditorCopy.ShortCut;
  CopyAsShort:=fMenu.aEditorCopyAs.ShortCut;
  CutShort:=fMenu.aEditorCut.ShortCut;
  PasteShort:=fMenu.aEditorPaste.ShortCut;
  AllShort:=fMenu.aEditorSelectAll.ShortCut;
  fMenu.aEditorCopy.ShortCut:=0;
  fMenu.aEditorCut.ShortCut:=0;
  fMenu.aEditorPaste.ShortCut:=0;
  fMenu.aEditorSelectAll.ShortCut:=0;

  CF_HTML := RegisterClipboardFormat(PChar('HTML Format'));
  if CF_HTML=0 then RaiseLastOsError();

  CF_ODT := RegisterClipboardFormat(PChar('Star Embed Source (XML)'));
  if CF_ODT=0 then RaiseLastOsError();

  CF_WAKAN := RegisterClipboardFormat(PChar('Wakan Text'));
  if CF_WAKAN=0 then RaiseLastOsError();

end;

procedure TfTranslate.FormDestroy(Sender: TObject);
begin
  linl.Free;
  plinl.Free;
  EditorBitmap.Free;
  doc.Free;
  doctr.Free;
  docdic.Free;
end;

procedure TfTranslate.FormShow(Sender: TObject);
begin
  ShowText(true);
  ListBox1.ItemIndex:=0;
  ListBox1.SetFocus;
end;

procedure TfTranslate.FormHide(Sender: TObject);
begin
  case dictmodeset of
    0: fUser.btnLookupJtoE.Down:=true;
    1: fUser.btnLookupEtoJ.Down:=true;
    2: fUser.btnLookupClip.Down:=true;
  end;
//  fUser.Look(false);
end;

procedure TfTranslate.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  fUser.SpeedButton8.Down:=false;
  fMenu.aDictEditor.Checked:=false;
end;

procedure TfTranslate.FormResize(Sender: TObject);
begin
  linl.Clear;
  Invalidate;
end;

procedure TfTranslate.FormActivate(Sender: TObject);
begin
  ListBox1.SetFocus;
end;

procedure TfTranslate.FormDeactivate(Sender: TObject);
begin
  if fHint.Visible then fUser.HideHint;
end;


procedure TfTranslate.ClearEditor;
begin
  doc.Clear;
  doctr.Clear;
  docdic.Clear;
  linl.Clear;
  cur := CursorPos(0, 0);
  view:=0;
  lblFilename.Caption:=_l('#00678^e<UNNAMED>');
  docfilename:='';
  mustrepaint:=true;
  ShowText(true);
  FFileChanged := false;
  FullTextTranslated := false;
end;

{ Opens a file by guessing format/encoding or asking user for it }
procedure TfTranslate.OpenAnyFile(filename:string);
var tp:byte;
begin
  if not FileExists(filename) then
    raise Exception.Create('File not found: "'+filename+'"');
  if not Conv_DetectTypeEx(filename, tp) then begin
    tp:=Conv_ChooseType(curlang='c', tp);
    if tp=0 then exit;
  end;
  OpenFile(filename, tp);
end;

procedure TfTranslate.OpenFile(filename:string;tp:byte);
var LoadAnnotMode: TTextAnnotMode;
begin
  docfilename:=filename;
  doctp:=tp;

  //by default we set SaveAnnotMode to default, meaning no preference has been chosen
  //auto-loaded rubys will be saved either way
  SaveAnnotMode := amDefault;

  //LoadAnnotMode governs how we treat incoming ruby (loading/pasting)
  if fSettings.cbLoadAozoraRuby.Checked then
    LoadAnnotMode := amRuby
  else
    LoadAnnotMode := amDefault;

  lblFilename.Caption:=uppercase(ExtractFilename(filename));
  doc.Clear;
  doctr.Clear;
  docdic.Clear;
  linl.Clear;
  Screen.Cursor:=crHourGlass;
  try

    view:=0;
    cur.x:=0;
    cur.y:=0;
    mustrepaint:=true;
    FileChanged:=false;
    FullTextTranslated:=false;

    if tp=FILETYPE_WTT then
      LoadWakanText(docfilename)
    else
      LoadText(docfilename, tp, LoadAnnotMode);

    ShowText(true);

  finally
    Screen.Cursor:=crDefault;
  end;
end;

{ Doesn't save Filename, tp or AnnotMode choice. That is correct.
This function can be called by others to make one-time special-format save.
SaveAs does the choice remembering. }
procedure TfTranslate.SaveToFile(filename:string;tp:byte;AnnotMode:TTextAnnotMode);
var stream: TStream;
begin
  Screen.Cursor:=crHourGlass;
  if (tp=FILETYPE_WTT) or (pos('.WTT',UpperCase(filename))>0) then
    SaveWakanText(filename)
  else begin
    stream := TStreamWriter.Create(TFileStream.Create(filename,fmCreate),true);
    try
      SaveText(
        AnnotMode,
        TRubyTextFormat.Create(tp),
        stream
      );
    finally
      FreeAndNil(stream);
    end;
  end;
  Screen.Cursor:=crDefault;
  FileChanged:=false;
end;

//Receives a string of characters and their properties.
//Parses all aozora-ruby sequences and converts them to annotations, removing from the line
//Has no particular adherrence to TfTranslate, we should probably move it someplace else.
procedure TfTranslate.CollapseRuby(var s: FString; var sp: TCharacterLineProps);
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

//Loads classic text file in any encoding.
procedure TfTranslate.LoadText(const filename:string;tp:byte;AnnotMode:TTextAnnotMode);
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

{
Ruby saving strategy:
- We always save "hard ruby" since if its present then we have loaded it from the file
- As for soft ruby, we save that to the file according to user's choice.
}
//Perhaps we should move Ruby code to ExpandRuby/DropRuby someday,
//and just write strings here.

procedure TfTranslate.SaveText(AnnotMode:TTextAnnotMode; format:TTextSaveFormat;
  stream: TStream; block: PTextSelection);
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
    format.AddChars(s)
  end;

  procedure FinalizeRuby();
  begin
    if (AnnotMode=amRuby) or explicitRuby then
      format.AddWord(kanji,reading,meaning,rubyTextBreak)
    else
      format.AddChars(kanji);
    inReading := false;
    reading := '';
    explicitRuby := false;
  end;

begin
  inReading := false;
  rootLen := -1;
  explicitRuby := true;

  if block<>nil then begin
    sel := block^;
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

  format.Stream := stream;
  format.BeginDocument;
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
        if (AnnotMode=amRuby) and not inReading then begin
          GetTextWordInfo(j,i,meaning,reading,kanji);
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
            and (AnnotMode=amRuby) //better annotate something than nothing
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

  format.EndDocument;
  FreeAndNil(format);
end;

type
  EBadWakanTextFormat = class(Exception);

//TODO: Load text into memory instead of directly into document?
//TODO: Or allow to specify where to paste text instead of replacing the document?
//TODO: Update current dictionary set with new dictionaries and update dict
//  references in the loaded text for dictionaries which got other indexes.
function TfTranslate.LoadWakanText(stream: TStream; silent: boolean): boolean;
var s,s2:string;
  s3: TCharacterLineProps;
  i:integer;
  w:word;
  reat:integer;
  buf:array[0..16383] of word;
  ws:array[0..31] of AnsiChar;
  wss:array[0..4091] of AnsiChar;
  wc:widechar;
  dot:boolean;
  l:integer;
  ls:string;
  dp:char;

  chars: FString;
  props: TCharacterLineProps;
begin
  reat:=stream.Read(w,2);
  if (reat<1) or (w<>$f1ff) then
    raise EBadWakanTextFormat.Create(_l('#00679^eThis is not a valid UTF-8 or JTT file.'));

  dot:=true;

  stream.Read(ws,32);
  s:=string(ws);
  if copy(s,1,22)<>'WaKan Translated Text>'then
    raise EBadWakanTextFormat.Create(_l('#00679^eThis is not a valid UTF-8 or JTT file.'));
  delete(s,1,22);

  if copy(s,1,length(fStatistics.Label15.Caption))<>fStatistics.Label15.Caption then
  begin
    if silent
    or (Application.MessageBox(
      pchar(_l('#00680^eThis JTT file was made using different WAKAN.CHR version. '
        +'Translation cannot be loaded.'#13#13'Do you want to continue?')),
      pchar(_l('#00090^eWarning')),
      MB_YESNO or MB_ICONWARNING)=idNo) then
    begin
      Result := false;
      exit;
    end;
    dot:=false;
  end;

  stream.Read(w,2);
  if w<>3294 then
    raise EBadWakanTextFormat.Create(_l('#00681^eThis JTT file was created by '
      +'old version of WaKan.'#13'It is not compatible with the current version.'));

  stream.Read(w,2);
  stream.Read(wss,w*2);
  wss[w*2]:=#0;
  s:=string(wss);
  while (s<>'') and (s[1]<>'$') do
  begin
    s2:=copy(s,1,pos(',',s)-1);
    delete(s,1,pos(',',s));
    docdic.Add(s2);
  end;

 //We will build a text in these variables, then pass it to PasteText.
  chars := '';
  props.Clear;

  s:='';
  s3.Clear;
  reat := stream.Read(buf,Length(buf)*SizeOf(word));
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
          showmessage('<<'+dp+'--'+inttostr(buf[i*4] div 256)+'--'+ls+'--'+chr(buf[i*4+2] div 256)+'>>');
        end;
        if not dot then
          s3.AddChar('-', 9, 0, 1)
        else
         //last param is apparently stored as character (ex. '1', '2') and we need it as int
          s3.AddChar(dp, buf[i*4] div 256, l, buf[i*4+2] div 256 - ord('0'));
      end;
    end;

    reat := stream.Read(buf,Length(buf)*SizeOf(word));
  end;

  if s<>'' then
  begin
    chars := chars + s;
    props.AddChars(s3);
  end;

  PasteText(chars,props,amDefault);
  Result := true;
end;

procedure TfTranslate.LoadWakanText(const filename:string);
var ms: TStream;
begin
  ms := TStreamReader.Create(
    TFileStream.Create(filename, fmOpenRead or fmShareDenyWrite),
    {OwnsStream=}true
  );
  try
    LoadWakanText(ms);
  finally
    FreeAndNil(ms);
  end;
end;

procedure TfTranslate.SaveWakanText(stream: TStream; block: PTextSelection = nil);
var i,j,bc:integer;
  buf:array[0..16383] of word;
  sig:word;
  s:AnsiString;
  l:integer;
  w:word;
  cp: PCharacterProps;
  sel: TTextSelection;
  chFirst,chLast: integer;
begin
  sig:=$f1ff;
  stream.Write(sig,2);

  s:=AnsiString('WaKan Translated Text>'+fStatistics.Label15.Caption);
  while length(s)<32 do s:=s+' ';
  stream.Write(s[1],32);

  w:=3294;
  stream.Write(w,2);

  s:='';
  for i:=0 to docdic.Count-1 do s:=s+AnsiString(docdic[i])+',';
  w:=(length(s)+1) div 2;
  stream.Write(w,2);
  s:=s+'$$$$';
  stream.Write(s[1],w*2);

  if block<>nil then begin
    sel := block^;
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
      cp := @doctr[i].chars[j];
      buf[bc]:=ord(cp.wordstate)+cp.learnstate*256;
      l:=cp.dicidx;
      buf[bc+2]:=l div 65536+(Ord('0')+cp.docdic)*256; //apparently dic # is stored as a char ('1','2'...)
      buf[bc+3]:=l mod 65536;
      buf[bc+1]:=word(fstrtouni(GetDoc(j,i))[1]);
      inc(bc,4);
      if bc=16384 then
      begin
        stream.Write(buf,bc*2);
        bc:=0;
      end;
    end;

    if i<>sel.toy then begin
     //Newline
      buf[bc]:=ord('$');
      buf[bc+1]:=0;
      buf[bc+2]:=0;
      buf[bc+3]:=0;
      inc(bc,4);
    end;

    if bc=16384 then
    begin
      stream.Write(buf,bc*2);
      bc:=0;
    end;
  end;
  stream.Write(buf,bc*2);
end;

procedure TfTranslate.SaveWakanText(const filename:string);
var ms: TStream;
begin
  ms := TStreamWriter.Create(
    TFileStream.Create(filename, fmCreate),
    {OwnsStream=}true
  );
  try
    SaveWakanText(ms);
  finally
    FreeAndNil(ms);
  end;
end;

//Returns false if user have cancelled the dialog
function TfTranslate.SaveAs: boolean;
var tp: byte;
begin
 //If configured to, or if we have chosen this option before
  if (SaveAnnotMode=amRuby) or fSettings.cbSaveAnnotationsToRuby.Checked then
    SaveTextDialog.FilterIndex := 2 //"Text with readings as Aozora Ruby"
  else
    SaveTextDialog.FilterIndex := 1; //"Text file"

  Result := SaveTextDialog.Execute;
  if not Result then exit;

  case SaveTextDialog.FilterIndex of
    1: SaveAnnotMode := amDefault;
    2: SaveAnnotMode := amRuby;
   //WTT file option is handled differently, that's how it was inherited.
  end;

  //Choose encoding
  if pos('.WTT',uppercase(SaveTextDialog.FileName))>0 then
    tp:=FILETYPE_WTT
  else
    tp:=Conv_ChooseType(curlang='c',0);

  SaveToFile(SaveTextDialog.FileName,tp,SaveAnnotMode);
  docfilename:=SaveTextDialog.FileName;
  doctp:=tp;
  lblFilename.Caption:=uppercase(ExtractFilename(SaveTextDialog.FileName));
end;

procedure TfTranslate.SetFileChanged(Value: boolean);
begin
  FFileChanged:=Value;
  sbFileSave.Enabled:=Value;
  fMenu.aEditorSave.Enabled:=Value;
end;

{
This function saves the changes to the file, possibly interacting with the user.
It has three possible outcomes:
1. "Yes": No changes or changes saved, dirty flag cleared.
  Result = true
2. "No": Changes not saved, do not ask again (dirty flag cleared).
  Result = true
3. "Cancel": Changes not saved, dirty flag not cleared, operation cancelled.
  Result = false
}
function TfTranslate.CommitFile:boolean;
var i:integer;
begin
  Result := true;
  if not filechanged then exit;

  if (fSettings.CheckBox60.Checked) and (docfilename<>'') then begin
   //Auto-"Yes"
    SaveToFile(docfilename,doctp,SaveAnnotMode);
    filechanged := false;
    exit;
  end;

  if fSettings.cbNoSaveChangesWarning.Checked then begin
   //We've been asked not to bother the user with save warnings. So "No".
    filechanged := false;
    exit;
  end;

  i:=Application.MessageBox(pchar(_l('#00687^eDocument has been changed. Do you want to save it?')),
   pchar(_l('#00090^eWarning')),MB_ICONWARNING or MB_YESNOCANCEL);
  if i<>idYes then begin
   //"No" or "Cancel"
    if i=idCancel then Result:=false;
    if i=idNo then filechanged := false;
    exit;
  end;

  if docfilename<>'' then begin
   //"Yes"
    SaveToFile(docfilename,doctp,SaveAnnotMode);
    filechanged := false;
    exit;
  end;

  if not SaveAs then begin
   //"Cancel" through cancelling dialog
    Result := false;
    exit;
  end;

 //"Yes"
  filechanged:=false;
  Result := true;
end;

constructor TTranslationThread.Create(ablockfromy, ablocktoy: integer);
begin
 {$IF CompilerVersion<21}
  inherited Create({CreateSuspended=}true);
 {$ELSE}
 { On newer compilers non-suspended threads are still suspended until the end of Create().
  Moreover, if we CreateSuspeneded and do Resume()/Start() manually,
  it'll clear CreateSuspended and it'll be called again in AfterConstruction => bug.
  This sucks. }
  inherited Create(false);
 {$IFEND}
  blockfromy := ablockfromy;
  blocktoy := ablocktoy;
  req := nil;
  dicsl := nil;
 {$IF CompilerVersion<21}
  Resume;
 {$IFEND}
end;

destructor TTranslationThread.Destroy;
begin
  inherited;
end;

procedure TTranslationThread.Execute;
var bg, en: integer;
  i: integer;
begin
  fUser.SetupSearchRequest(stEditorAuto, req);
  req.dic_ignorekana := true;
  req.Prepare;

  dicsl := TSearchResults.Create;
  try
    i := blockfromy;
    while (not Terminated) and (i<=blocktoy) do begin
      bg:=0;
      en:=flength(fTranslate.doc[i])-1;
      fTranslate.AutoTranslateLine(i, bg, en, req, dicsl);
      Inc(i);
    end;

  finally
    FreeAndNil(dicsl);
    FreeAndNil(req);
  end;
end;

{$IFDEF MTHREAD_SUPPORT}
{ Creates enough translation threads to use available cores and distributes work between those.
 Always leaves the last chunk to the calling thread because the last line of the last chunk
 might be incomplete and it's easier to care about that only in the main thread.
 On exit sets y to reflect calling thread's new share of work. }
function TfTranslate.CreateTranslationThreads(abfromy, abtoy: integer; var y: integer): TTranslationThreads;
var sysinfo: SYSTEM_INFO;
  i, yshare: integer;
begin
  GetSystemInfo(sysinfo);
  SetLength(Result,0);
  if sysinfo.dwNumberOfProcessors<=1 then exit;
  if abtoy-y < y-abfromy then exit; //don't start threads if there's less left than we have done
  if abtoy-y < integer(sysinfo.dwNumberOfProcessors) then exit; //less than one line per thread!

  SetLength(Result, sysinfo.dwNumberOfProcessors-1);
  yshare := (abtoy-y) div (Length(Result)+1);
  for i := 0 to Length(Result)-1 do begin
    Result[i] := TTranslationThread.Create(y, y+yshare-1);
    Inc(y, yshare);
  end;
end;
{$ENDIF}

procedure TfTranslate.AutoTranslate;
var j:integer;
  bg,en:integer;
  y:integer;

  sp: TSMPromptForm;
  startTime: cardinal;
  req: TDicSearchRequest;
  dicsl: TSearchResults;

  donework: integer;
  totalwork: integer;

 {$IFDEF MTHREAD_SUPPORT}
  threads: TTranslationThreads;
  useThreads: boolean;
  threadsCreated: boolean;
 {$ENDIF}
begin
  if (dragstart.x=rcur.x) and (dragstart.y=rcur.y) then
  begin
    if not fSettings.cbTranslateNoLongTextWarning.Checked then
      if Application.MessageBox(
        pchar(_l('#00682^eNo block is selected. Do you want generate translation for entire document?'#13#13
          +'This action can take a very long time.')),
        pchar(_l('#00683^eConfirmation')),
        MB_ICONWARNING or MB_YESNO)<>idYes then exit;
    block.fromx:=0;
    block.fromy:=0;
    block.tox:=flength(doc[doc.Count-1])-1;
    block.toy:=doc.Count-1;
  end else
    CalcBlockFromTo(true);
  Screen.Cursor:=crHourGlass;

 {$IFDEF MTHREAD_SUPPORT}
  useThreads := fSettings.cbMultithreadedTranslation.Checked;
  threadsCreated := false;
  for j := 0 to dicts.Count - 1 do
    if dicts[j].Offline then begin
      useThreads := false; //offline dictionaries don't support multithreaded access, so no.
      break;
    end;
 {$ENDIF}

 //Don't show the progress window at all unless the operation is taking a long time.
  sp := nil;
  req := nil;
  dicsl := TSearchResults.Create;
  startTime := GetTickCount;
  try

   //Setup everything for translation
    fUser.btnLookupJtoE.Down:=false;
    fUser.btnLookupEtoJ.Down:=false;
    fUser.btnLookupClip.Down:=false;
    fUser.SetupSearchRequest(stEditorAuto, req);
    req.dic_ignorekana := true;
    req.Prepare;

    totalwork := block.toy-block.fromy+1;
    donework := 0;

    y := block.fromy;
    while y<=block.toy do
    begin
      bg:=0;
      en:=flength(doc[y])-1;
      if y=block.fromy then bg:=block.fromx;
      if y=block.toy then en:=block.tox;

      //If the operation is taking too long to be noticeable
      if (sp=nil) and (GetTickCount-startTime > 200) then begin
       //Bring up the progress window
        sp:=SMProgressDlgCreate(
          _l('#00684^eTranslator'),
          _l('#00685^eTranslating...'),
          totalwork,
          {canCancel=}true);
        sp.Width := 200; //we like it narrow
        sp.AppearModal;
      end;

      if sp<>nil then begin
       //Internally we only update once in a while
        sp.SetProgress(donework);
        sp.ProcessMessages;
        if sp.ModalResult=mrCancel then begin
          sp.Hide;
          sp.SetProgressPaused(true); //although not important when hidden
          if Application.MessageBox(
            PChar(_l('#01005^eThe text has only been partially translated. Do you '
              +'really want to abort the operation?')),
            PChar(_l('#01006^eConfirm abort')),
            MB_ICONQUESTION+MB_YESNO
          )=idYes then
          begin
           {$IFDEF MTHREAD_SUPPORT}
            for j := 0 to Length(threads) - 1 do
              threads[j].Terminate; //even if it was doing work
           {$ENDIF}
            break; //and restore+repaint
          end;
          sp.ModalResult := 0;
          sp.SetProgressPaused(false);
          sp.Show;
        end;
      end;

     {$IFDEF MTHREAD_SUPPORT}
      if useThreads and not threadsCreated and (GetTickCount-startTime > 200) then begin
        threadsCreated := true; //skip this block afterwards
       //Load all dictionaries because on-demand loading might mess up multithreading
        for j := 0 to dicts.Count - 1 do
          if dicts[j].loaded then dicts[j].Demand;

        threads := CreateTranslationThreads(block.fromy,block.toy,y);
        //recalculate total work
        totalwork := (block.toy-y+1)+donework;
        if sp<>nil then
          sp.SetMaxProgress(totalwork);
      end;
     {$ENDIF}

      AutoTranslateLine(y, bg, en, req, dicsl);
      Inc(y);
      Inc(donework);
    end;

  finally
    sp.Free; //Important, because it's modal window
   {$IFDEF MTHREAD_SUPPORT}
    for j := 0 to Length(threads) - 1 do begin
      threads[j].WaitFor;
      threads[j].Free;
    end;
   {$ENDIF}
    FreeAndNil(req);
    FreeAndNil(dicsl);
  end;

  if (dragstart.x=rcur.x) and (dragstart.y=rcur.y) then
    FullTextTranslated := true; //translated all text at least once

 {$IFDEF TLSPEEDREPORT}
  Application.MessageBox(
    PChar('total time: '+IntToStr(GetTickCount-startTime)),
    PChar('thread test results'),
    MB_OK
  );
 {$ENDIF}
  mustrepaint:=true;
  ShowText(true);
  Screen.Cursor:=crDefault;
end;

procedure TfTranslate.AutoTranslateLine(y: integer; x_bg, x_en: integer;
  req: TDicSearchRequest; dicsl: TSearchResults);
var x: integer;
  a:integer;
  s:string;
  word:PSearchResult;
  wt:integer;
begin
  x:=x_bg;
  while x<=x_en do
   //skip latin words
    if IsLocaseLatin(doctr[y].chars[x].wordstate) then
    begin
      inc(x);
      while (x<doctr[y].charcount) and (doctr[y].chars[x].wordstate='<') do inc(x);
    end else
   //skip ruby-fied parts (by translating single words we'll break ruby-chain into parts, see Issue30@googlecode)
    if cfExplicitRuby in doctr[y].chars[x].flags then
    begin
      inc(x);
      while (x<doctr[y].charcount) and (doctr[y].chars[x].wordstate='<') do inc(x);
    end else
    begin
      dicsl.Clear;
      s:=GetDocWord(x,y,wt,{stopuser=}true);
      req.Search(s, wt, dicsl);
      if dicsl.Count>0 then word:=dicsl[0] else word:=nil;
      a:=SetWordTrans(x,y,[tfScanParticle],word);
      if a=0 then a:=1;
      inc(x,a);
    end;
end;

procedure TfTranslate.SetTranslation();
begin
  if (dragstart.x=rcur.x) and (dragstart.y=rcur.y) then
  begin
    SetWordTrans(rcur.x,rcur.y,[tfScanParticle,tfManuallyChosen],false);
    mustrepaint:=true;
    ShowText(true);
  end else
    AutoTranslate();
end;

procedure TfTranslate.ListBox1Enter(Sender: TObject);
begin
  fMenu.aEditorCopy.ShortCut:=CopyShort;
  fMenu.aEditorCopyAs.ShortCut:=CopyAsShort;
  fMenu.aEditorCut.ShortCut:=CutShort;
  fMenu.aEditorPaste.ShortCut:=PasteShort;
  fMenu.aEditorSelectAll.ShortCut:=AllShort;
  DrawCursor(true); //show cursor
end;

procedure TfTranslate.ListBox1Exit(Sender: TObject);
begin
  fMenu.aEditorCopy.ShortCut:=0;
  fMenu.aEditorCopyAs.ShortCut:=0;
  fMenu.aEditorCut.ShortCut:=0;
  fMenu.aEditorPaste.ShortCut:=0;
  fMenu.aEditorSelectAll.ShortCut:=0;
  DrawCursor(false); //kill cursor
end;

procedure TfTranslate.ListBox1KeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
procedure recalcy(oldy,newy:integer);
begin
  cur.x:=WidthToPos(PosToWidth(cur.x,oldy),newy);
  cur.y:=newy;
end;
var bx,by:integer;
    ukn:boolean;
begin
  bx:=cur.x; by:=cur.y;
  if (ins.x=-1) or (insconfirmed) then
  begin
    ukn:=false;
    if key=VK_RIGHT then
    begin
      inc(cur.x);
      if cur.x>linl[cur.y].len then
        if cur.y+1<linl.Count then
        begin
          inc(cur.y);
          cur.x:=0;
        end else dec(cur.x);
      cursorend:=false;
    end
    else if key=VK_LEFT then
    begin
      dec(cur.x);
      if cur.x<0 then
        if cur.y>0 then
        begin
          dec(cur.y);
          cur.x:=linl[cur.y].len;
        end else inc(cur.x);
      cursorend:=false;
    end
    else if key=VK_UP then recalcy(cur.y,cur.y-1)
    else if key=VK_DOWN then recalcy(cur.y,cur.y+1)
    else if (key=VK_PRIOR) and (ssCtrl in Shift) then
    begin
      cur.x:=0;
      cur.y:=0;
    end
    else if (key=VK_NEXT) and (ssCtrl in Shift) then
    begin
      cur.y:=linl.Count-1;
      cur.x:=100;
    end
    else if key=VK_PRIOR then recalcy(cur.y,cur.y-printl)
    else if key=VK_NEXT then recalcy(cur.y,cur.y+printl)
    else if key=VK_HOME then
    begin
      if (cursorend) and (cur.y>0) then dec(cur.y) else cur.x:=0;
      cursorend:=false;
    end
    else if key=VK_END then
    begin
      if not cursorend then cur.x:=100;
    end
    else if key=VK_DELETE then
    begin
      ResolveInsert(true);
      if (dragstart.x<>rcur.x) or (dragstart.y<>rcur.y) then
        DeleteSelection()
      else
        DeleteCharacter(rcur.x,rcur.y);
      RefreshLines;
    end else ukn:=true;
    if not ukn then
    begin
      ClearInsBlock;
      if ssShift in Shift then shiftpressed:=true;
      if (bx<>cur.x) or (by<>cur.y) then ResolveInsert(true);
      if (bx<>cur.x) or (by<>cur.y) then ShowText(true);
    end;
  end else
  with fUser do
  begin
    if (key=VK_UP) and (StringGrid1.Row>1) then StringGrid1.Row:=StringGrid1.Row-1;
    if (key=VK_DOWN) and (StringGrid1.Row<StringGrid1.RowCount-1) then StringGrid1.Row:=StringGrid1.Row+1;
    if (StringGrid1.RowCount>1) and (StringGrid1.Visible) and (ins.x<>-1) then ShowHint else HideHint;
  end;
end;

procedure TfTranslate.ListBox1KeyPress(Sender: TObject; var Key: Char);
begin
  InsertCharacter(key);
end;

procedure TfTranslate.EditorPaintBoxClick(Sender: TObject);
begin
  ListBox1.SetFocus;
end;

procedure TfTranslate.EditorPaintBoxMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  if not TryReleaseCursorFromInsert() then
    exit; //cannot move cursor!
  shiftpressed:=(ssShift in Shift);
  if linl.Count>0 then //else lines are to be recalculated and we can't do much
    cur:=GetClosestCursorPos(X,Y);
  mustrepaint:=true;
  ShowText(true);
end;

procedure TfTranslate.EditorPaintBoxMouseMove(Sender: TObject;
  Shift: TShiftState; X, Y: Integer);
begin
  //Sometimes we receive "ssLeft + MouseMove" when we double click something
  //in another window and that window closes, leaving us in editor.
  //This results in ugly unexpected text selection.
  if Mouse.Capture=Self.Handle then //EditorPaintBox doesn't have it's own handle
    if ssLeft in Shift then begin;
      if not TryReleaseCursorFromInsert() then
        exit; //cannot move cursor!
      if linl.Count>0 then begin
        cur:=GetClosestCursorPos(X+lastxsiz div 2,Y);
        if (cur.x=lastmm.x) and (cur.y=lastmm.y) then exit;
        lastmm.x:=cur.x;
        lastmm.y:=cur.y;
      end;
      shiftpressed:=true;
      ShowText(false);
    end;

  fMenu.IntTipPaintOver(EditorPaintBox,x,y,false);
end;

procedure TfTranslate.EditorPaintBoxDblClick(Sender: TObject);
begin
  if not fKanjiDetails.Visible then
    fMenu.aKanjiDetailsExecute(nil);
end;

procedure TfTranslate.EditorPaintBoxPaint(Sender: TObject);
begin
  MakeEditorBitmap;
end;

procedure TfTranslate.sbDisplayReadingClick(Sender: TObject);
begin
  fMenu.aEditorReading.Checked:=sbDisplayReading.Down;
  RepaintText(true);
end;

procedure TfTranslate.sbDisplayMeaningClick(Sender: TObject);
begin
  fMenu.aEditorMeaning.Checked:=sbDisplayMeaning.Down;
  RepaintText(true);
end;

procedure TfTranslate.sbClearTranslationClick(Sender: TObject);
var i,j:integer;
    bg,en:integer;
begin
  CalcBlockFromTo(true);
  for i:=block.fromy to block.toy do
  begin
    bg:=0;
    en:=flength(doc[i])-1;
    if i=block.fromy then bg:=block.fromx;
    if i=block.toy then en:=block.tox;
    for j:=bg to en do
      doctr[i].chars[j].Reset();
  end;
  mustrepaint:=true;
  ShowText(true);
end;

procedure TfTranslate.sbAutoTranslateClick(Sender: TObject);
begin
  Self.AutoTranslate();
end;

procedure TfTranslate.sbSetTranslationClick(Sender: TObject);
begin
  Self.SetTranslation();
end;

procedure PrintConfigure(userdata:pointer);
begin
  fSettings.pcPages.ActivePage:=fSettings.tsTextTranslator;
  fSettings.ShowModal;
end;

procedure TfTranslate.sbPrintClick(Sender: TObject);
begin
  fPrint.Preview(GetPageNum,DrawPage,PrintConfigure,nil,_l('#00686^eTranslated text'));
end;

procedure TfTranslate.sbFileOpenClick(Sender: TObject);
begin
  if not CommitFile then exit;
  if OpenTextDialog.Execute then
    OpenAnyFile(OpenTextDialog.Filename);
end;

procedure TfTranslate.sbFileSaveClick(Sender: TObject);
begin
  if docfilename<>'' then
    SaveToFile(docfilename,doctp,SaveAnnotMode)
  else
    SaveAs;
end;

procedure TfTranslate.sbFileNewClick(Sender: TObject);
begin
  if not CommitFile then exit;
  ClearEditor;
end;

procedure TfTranslate.sbClipCutClick(Sender: TObject);
begin
  sbClipCopyClick(Sender);
  DeleteSelection;
end;

//Copies selection as text with only those Ruby which were read from file
function TfTranslate.CopyAsText: UnicodeString;
var stream: TStream;
begin
  stream := TUnicodeStringStream.Create(@Result);
  CopySelection(TRubyTextFormat.Create(FILETYPE_UTF16LE),stream,amDefault);
  FreeAndNil(stream);
end;

//Copies selection as a text with Aozora-Ruby
function TfTranslate.CopyAsRuby: UnicodeString;
var stream: TStream;
begin
  stream := TUnicodeStringStream.Create(@Result);
  CopySelection(TRubyTextFormat.Create(FILETYPE_UTF16LE),stream);
  FreeAndNil(stream);
end;

//Copies selection as HTML, with <!--StartFragment --><!--EndFragment --> marks
function TfTranslate.CopyAsHtml: Utf8String;
var stream: TStream;
begin
  stream := TAnsiStringStream.Create(@Result);
  CopySelection(THtmlFormat.Create([hoClipFragment]),stream);
  FreeAndNil(stream);
end;

//Generates CF_HTML clipboard header for a text
function TfTranslate.GenerateHtmlClipHeader(const lenHtml: integer;
  const startFragment, endFragment: integer): UnicodeString;
const
 //The way we use it, header must produce fixed-length text no matter the input
  headerForm = 'Version:0.9'#13#10
    +'StartHTML:%.8d'#13#10
    +'EndHTML:%.8d'#13#10
    +'StartFragment:%.8d'#13#10
    +'EndFragment:%.8d'#13#10;
var lenHeader: integer;
begin
  lenHeader := Length(Format(headerForm,[0,0,0,0]));
  Result := Format(headerForm,[lenHeader, lenHeader+lenHtml,lenHeader+startFragment,lenHeader+endFragment]);
end;

//Copies selection as HTML enhanced with a CF_HTML clipboard header
function TfTranslate.CopyAsClipHtml: Utf8String;
var startFragment, endFragment: integer;
begin
  Result := CopyAsHtml;
  startFragment := pos(Utf8String(HtmlStartFragment),Result);
  if startFragment <> 0 then startFragment := startFragment + Length(Utf8String(HtmlStartFragment)); //else keep it 0
  endFragment := pos(Utf8String(HtmlEndFragment),Result);
  if endFragment <= 0 then endFragment := Length(HtmlEndFragment);
  Result := Utf8String(GenerateHtmlClipHeader(Length(Result), startFragment, endFragment))+Result;
end;

//Copies selection as OpenDocumentText, content file
function TfTranslate.CopyAsOpenDocumentTextContent: Utf8String;
var stream: TStream;
begin
  stream := TAnsiStringStream.Create(@Result);
  CopySelection(TOpenDocumentContentFormat.Create(),stream);
  FreeAndNil(stream);
end;

function TfTranslate.CopyAsOpenDocument: TMemoryStream;
begin
  Result := TMemoryStream.Create;
  Self.CopySelection(TOpenDocumentFormat.Create(),Result);
end;

function TfTranslate.CopyAsWakanText: TMemoryStream;
begin
  Result := TMemoryStream.Create;
  CalcBlockFromTo(false);
  Self.SaveWakanText(Result,@block);
end;

{ Normal Ctrl-C -- only in a few basic formats.
 For enhanced copy, use Ctrl+Alt+C / CopyAs() }
procedure TfTranslate.sbClipCopyClick(Sender: TObject);
var NormalText: UnicodeString;
begin
  NormalText := CopyAsText;
  fMenu.ResetClipboard;
  try
    clip := fstr(NormalText);
    fMenu.AddToClipboard(CF_WAKAN,CopyAsWakanText(),{OwnsStream=}true);
    fMenu.AddToClipboard(CF_HTML,CopyAsClipHtml());
    fMenu.AddToClipboard(CF_UNICODETEXT,NormalText);
  finally
    fMenu.PublishClipboard;
  end;
end;

{ Enhanced Ctrl-Alt-C -- all ruby + all supported formats.
 In the future perhaps this will pop up a dialog asking to choose a format }
procedure TfTranslate.CopyAs;
var RubyText: UnicodeString;
begin
  RubyText := CopyAsRuby;
  fMenu.ResetClipboard;
  try
    clip := fstr(RubyText);
    fMenu.AddToClipboard(CF_WAKAN,CopyAsWakanText(),{OwnsStream=}true);
   {$IFDEF DEBUG}
   //No point since no one supports this... Even LibreOffice doesn't paste this.
    fMenu.AddToClipboard(CF_ODT,CopyAsOpenDocument(),{OwnsStream=}true);
   {$ENDIF}
    fMenu.AddToClipboard(CF_HTML,CopyAsClipHtml());
    fMenu.AddToClipboard(CF_UNICODETEXT,RubyText);
  finally
    fMenu.PublishClipboard;
  end;
end;

procedure TfTranslate.sbClipPasteClick(Sender: TObject);
begin
  PasteOp;
end;

procedure TfTranslate.sbKanjiModeClick(Sender: TObject);
begin
  sbKanjiMode.Down:=true;
  fMenu.aEditorKanjiMode.Checked:=true;
  fMenu.aEditorKanaMode.Checked:=false;
  fMenu.aEditorASCIIMode.Checked:=false;
end;

procedure TfTranslate.sbKanaModeClick(Sender: TObject);
begin
  sbKanaMode.Down:=true;
  fMenu.aEditorKanaMode.Checked:=true;
  fMenu.aEditorKanjiMode.Checked:=false;
  fMenu.aEditorASCIIMode.Checked:=false;
end;

procedure TfTranslate.sbAsciiModeClick(Sender: TObject);
begin
  sbAsciiMode.Down:=true;
  fMenu.aEditorASCIIMode.Checked:=true;
  fMenu.aEditorKanaMode.Checked:=false;
  fMenu.aEditorKanjiMode.Checked:=false;
end;

procedure TfTranslate.EditorScrollBarChange(Sender: TObject);
begin
  view:=EditorScrollBar.Position;
  if (view>=0) and (view<linl.Count) then
  begin
    rview.x:=linl[view].xs;
    rview.y:=linl[view].ys;
  end;
  MakeEditorBitmap;
end;

procedure TfTranslate.FormMouseWheelUp(Sender: TObject; Shift: TShiftState;
  MousePos: TPoint; var Handled: Boolean);
begin
  HandleWheel(false);
  handled:=true;
end;

procedure TfTranslate.FormMouseWheelDown(Sender: TObject;
  Shift: TShiftState; MousePos: TPoint; var Handled: Boolean);
begin
  HandleWheel(true);
  handled:=true;
end;

procedure TfTranslate.sbDockKanjiDetailsClick(Sender: TObject);
begin
  fMenu.aKanjiDetails.Execute;
end;

procedure TfTranslate.sbDockDictionaryClick(Sender: TObject);
begin
  fMenu.TabControl1Change(sender);
end;

procedure TfTranslate.sbUseTlColorsClick(Sender: TObject);
begin
  fMenu.aEditorCOlors.Execute;
end;

procedure TfTranslate.BlinkCursorTimerTimer(Sender: TObject);
begin
  if Self.Visible then DrawCursor(true);
end;

procedure TfTranslate.SelectAll;
begin
  if not TryReleaseCursorFromInsert() then
    exit; //cannot move cursor!
  dragstart := SourcePos(0, 0);
  cur := CursorPos(linl[linl.Count-1].len, linl.Count-1);
  shiftpressed:=true;
  ShowText(true);
end;

procedure TfTranslate.MakeEditorBitmap;
begin
  editorbitmap.Free;
  editorbitmap:=TBitmap.Create;
  editorbitmap.Width:=EditorPaintBox.Width;
  editorbitmap.Height:=EditorPaintBox.Height;
  RenderText(rview.x,rview.y,editorbitmap.Canvas,2,2,EditorPaintBox.Width-4,
    EditorPaintBox.Height-4,linl,printl,lastxsiz,lastycnt,false,false);
  oldcur := CursorPos(-1, -1);
  oldblock := Selection(-1, -1, -1, -1);
  DrawBlock(EditorBitmap.Canvas);
  PasteEditorBitmap;
  DrawCursor(false);
end;

procedure TfTranslate.PasteEditorBitmap;
begin
  EditorPaintBox.Canvas.Draw(0,0,EditorBitmap);
end;

procedure TfTranslate.HandleWheel(down:boolean);
begin
  if not EditorScrollBar.Enabled then exit;
  if down then inc(view) else dec(view);
  if (view>=0) and (view<=EditorScrollBar.Max) then
  begin
    rview := SourcePos(linl[view].xs, linl[view].ys);
  end else
    if view<0 then view:=0 else view:=EditorScrollBar.Max;
  EditorScrollBar.Position:=view;
  MakeEditorBitmap;
end;

procedure TfTranslate.ShowText(dolook:boolean);
var oldview:integer;
  s:string;
  wt:integer;
begin
  if not Visible then exit;
  oldview:=view;
  RenderText(-1,-1,EditorPaintBox.Canvas,0,0,EditorPaintBox.Width-4,
    EditorPaintBox.Height-4,linl,printl,lastxsiz,lastycnt,false,true);
  if linl.Count=0 then
  begin
    rcur := SourcePos(-1, -1);
    rview := SourcePos(0, 0);
    EditorPaintBox.Invalidate;
    EditorScrollBar.Enabled:=false;
    exit;
  end;
  if cur.y<0 then cur.y:=0;
  if cur.y>=linl.Count then
  begin
    cur.y:=linl.Count-1;
    cur.x:=2555;
  end;
  if cur.x<0 then cur.x:=0;
  if (cursorend) and (cur.x=0) and (cur.y>0) and (linl[cur.y-1].ys<>linl[cur.y].ys) then
  begin
    dec(cur.y);
    cur.x:=linl[cur.y].len;
    cursorend:=false;
  end;
  if cur.x>linl[cur.y].len then
    if (cur.y+1<linl.Count) and (linl[cur.y].ys=linl[cur.y+1].ys) then
    begin
      cur.x:=0;
      inc(cur.y);
      cursorend:=true;
    end else
      cur.x:=linl[cur.y].len;
  if view>cur.y then if cur.y>0 then view:=cur.y else view:=0;
  if view+printl-1<cur.y then view:=cur.y-printl+1;
  if view+printl-1>=linl.Count then view:=linl.Count-printl;
  if view<0 then view:=0;
  if view>=linl.Count then view:=0;
  rcur := SourcePos(linl[cur.y].xs+cur.x, linl[cur.y].ys);
  rview := SourcePos(linl[view].xs, linl[view].ys);
  if not shiftpressed then
  begin
    dragstart.x:=rcur.x;
    dragstart.y:=rcur.y;
  end;
  fUser.btnLookupJtoE.Down:=false;
  fUser.btnLookupEtoJ.Down:=false;
  fUser.btnLookupClip.Down:=false;
  if (dolook) and ((fUser.Visible) or (insertBuffer<>'')) then fUser.Look() else
  if dolook then begin
    s:=GetDocWord(rcur.x,rcur.y,wt,false);
    if flength(s)>=1 then fKanjiDetails.SetCharDetails(fgetch(s,1));
  end;
  if oldview<>view then mustrepaint:=true;
  if mustrepaint then MakeEditorBitmap else
  begin
    DrawCursor(false);
    DrawBlock(EditorPaintBox.Canvas);
  end;
//  if mustrepaint then oldblock.fromx:=-1; //why did we need this here? It breaks setting new selection with mustrepaint==true
  mustrepaint:=false;
  shiftpressed:=false;
  if linl.Count-printl<=0 then
    EditorScrollBar.Enabled:=false
  else
  begin
    EditorScrollBar.Min:=0;
    EditorScrollBar.Max:=linl.Count-printl;
    EditorScrollBar.Position:=view;
    EditorScrollBar.Enabled:=true;
  end;
  with fUser do
    if (StringGrid1.RowCount>1) and (StringGrid1.Visible) and (ins.x<>-1) then ShowHint else HideHint;
end;

{ Converts startdrag+cursor positions to block selection. }
procedure TfTranslate.CalcBlockFromTo(backtrack:boolean);
begin
  if (rcur.y<dragstart.y) or ((rcur.y=dragstart.y) and (rcur.x<dragstart.x)) then
  begin
    block.fromx:=rcur.x;
    block.fromy:=rcur.y;
    block.tox:=dragstart.x;
    block.toy:=dragstart.y;
  end else
  begin
    block.fromx:=dragstart.x;
    block.fromy:=dragstart.y;
    block.tox:=rcur.x;
    block.toy:=rcur.y;
  end;
  if backtrack then
  begin
    while (block.fromx>=0) and (doctr[block.fromy].chars[block.fromx].wordstate='<') do dec(block.fromx);
    while (block.tox+1<doctr[block.toy].charcount) and (doctr[block.toy].chars[block.tox+1].wordstate='<') do inc(block.tox);
  end;
end;

{
We should probably keep graphical lines for the whole document,
and only re-process the parts which change.
But for now we RecalculateGraphicalLines() every full render,
and start with current position, not with the start of the document.
}

{
RecalculateGraphicalLines()
Does a full rebuild of graphical line schema for a document.
rs: half-char size in pixels (depends on font)
}
procedure TfTranslate.RecalculateGraphicalLines(ll: TGraphicalLineList;
  rs: integer; screenw: integer; vert: boolean);
var
  cx, cy: integer;
  px: integer;
  wx, wxl: integer;
begin
  ll.Clear;
  cx:=0;
  cy:=0;
  while cy<doc.Count do
  begin
    wx:=flength(doc[cy]);

    px:=0;
    wxl:=cx;
    while px<=screenw do
    begin
      if (not vert) and (wxl<wx) and IsHalfWidth(wxl,cy) then
        inc(px,rs)
      else
        inc(px,rs*2);
      inc(wxl);
    end;
    dec(wxl);
    //wxl now points to a symbol which is at least partially visible

    if wx>wxl then wx:=wxl;
    //wx now points to a symbol just after the last one we can draw on this graphical line

    if (wx<=cx) then
    begin
     { We can draw nothing. Either the logical line is over, or the graphical line
      is too small to hold even one symbol.
      Skip to the next line (or this'll never end). }
      if cx=0 then
        ll.Add(0, cy, 0); //or we would miss empty lines since they have no characters
      inc(cy);
      cx:=0;
    end else
    begin
      ll.Add(cx, cy, wx-cx);
      cx:=wx;
    end;
  end;

  if ll.Count=0 then //empty document
    ll.Add(0, 0, 0); //add one empty line
end;


procedure FixReading(gd0,gd1,gd2:FChar; var reading:FString);
var gd: FString;
begin
  if (EvalChar(gd1)=EC_KATAKANA) and not showroma then
    reading := RomajiToKana('H'+KanaToRomaji(gd1,1,'j'),1,true,'j');
  if (EvalChar(gd1) in [EC_HIRAGANA, EC_KATAKANA]) and showroma then
  begin
    gd := '';
   {$IFNDEF UNICODE}
    if (gd1='30C3') or (gd1='3063') then gd:='' else
    if ((gd0='30C3') or (gd0='3063')) and
       ((gd2='3041') or (gd2='3043') or (gd2='3045') or (gd2='3047') or (gd2='3049') or
       (gd2='3083') or (gd2='3085') or (gd2='3087') or
       (gd2='30A1') or (gd2='30A3') or (gd2='30A5') or (gd2='30A7') or (gd2='30A9') or
       (gd2='30E3') or (gd2='30E5') or (gd2='30E7')) then gd:=gd0+gd1+gd2 else
    if (gd0='30C3') or (gd0='3063') then gd:=gd0+gd1 else
    if (gd2='3041') or (gd2='3043') or (gd2='3045') or (gd2='3047') or (gd2='3049') or
       (gd2='3083') or (gd2='3085') or (gd2='3087') or
       (gd2='30A1') or (gd2='30A3') or (gd2='30A5') or (gd2='30A7') or (gd2='30A9') or
       (gd2='30E3') or (gd2='30E5') or (gd2='30E7') then gd:=gd1+gd2 else
    if (gd1='3041') or (gd1='3043') or (gd1='3045') or (gd1='3047') or (gd1='3049') or
       (gd1='3083') or (gd1='3085') or (gd1='3087') or
       (gd1='30A1') or (gd1='30A3') or (gd1='30A5') or (gd1='30A7') or (gd1='30A9') or
       (gd1='30E3') or (gd1='30E5') or (gd1='30E7') then gd:='';
   {$ELSE}
    if (gd1=#$30C3) or (gd1=#$3063) then gd:='' else
    if ((gd0=#$30C3) or (gd0=#$3063)) and
       ((gd2=#$3041) or (gd2=#$3043) or (gd2=#$3045) or (gd2=#$3047) or (gd2=#$3049) or
       (gd2=#$3083) or (gd2=#$3085) or (gd2=#$3087) or
       (gd2=#$30A1) or (gd2=#$30A3) or (gd2=#$30A5) or (gd2=#$30A7) or (gd2=#$30A9) or
       (gd2=#$30E3) or (gd2=#$30E5) or (gd2=#$30E7)) then gd:=gd0+gd1+gd2 else
    if (gd0=#$30C3) or (gd0=#$3063) then gd:=gd0+gd1 else
    if (gd2=#$3041) or (gd2=#$3043) or (gd2=#$3045) or (gd2=#$3047) or (gd2=#$3049) or
       (gd2=#$3083) or (gd2=#$3085) or (gd2=#$3087) or
       (gd2=#$30A1) or (gd2=#$30A3) or (gd2=#$30A5) or (gd2=#$30A7) or (gd2=#$30A9) or
       (gd2=#$30E3) or (gd2=#$30E5) or (gd2=#$30E7) then gd:=gd1+gd2 else
    if (gd1=#$3041) or (gd1=#$3043) or (gd1=#$3045) or (gd1=#$3047) or (gd1=#$3049) or
       (gd1=#$3083) or (gd1=#$3085) or (gd1=#$3087) or
       (gd1=#$30A1) or (gd1=#$30A3) or (gd1=#$30A5) or (gd1=#$30A7) or (gd1=#$30A9) or
       (gd1=#$30E3) or (gd1=#$30E5) or (gd1=#$30E7) then gd:='';
   {$ENDIF}
    if (flength(gd)>1) and (EvalChar(fgetch(gd,1))=EC_KATAKANA) then
      gd:=RomajiToKana('H'+KanaToRomaji(gd,1,'j'),1,true,'j');
    reading:=gd;
  end;
 {$IFNDEF UNICODE}
  if (gd1='30FC') then reading:='30FC';
 {$ELSE}
  if (gd1=#$30FC) then reading:=#$30FC;
 {$ENDIF}
end;

{
x, y:
  start point in logical line coordinates (line:first character)
l, t, w, h:
  left, top, width, height of the block to draw in on the canvas
ll:
  graphical line list (all lines for this control)
printl (out):
  total number of lines printed
}
procedure TfTranslate.RenderText(x,y:integer;canvas:TCanvas;l,t,w,h:integer;
  ll:TGraphicalLineList;var printl,xsiz,ycnt:integer;printing,onlylinl:boolean);
var st0,lst0,st2,st3:boolean;
    linec,linec2,lineh,screenh,screenw,lines:integer;
    st2c:integer;
    rs:integer;
    vert:boolean; //vertical printing. Only for printing.
    cl,cx,cxsc,cy,px,py,wx:integer;
    kanaq:FString;
    undersolid:boolean;
    color,fcolor:TColor;
    boldness:boolean;
    meaning:string;
    reading,kanji:FString;
    learnstate:integer;
    wordstate,lastwordstate:char;
    kanjilearned:boolean;
    cnty,cntx:integer;
    realx,realy,realx2,realy2:integer;
    we:integer;
    rect:TRect;
    i:integer;
    invert:boolean;
    dnam:string;
    dic:TJaletDic;
    markers:string;
    worddict:integer; //word's dicidx
    lastworddict:integer; //last word's dicidx
    colback, coltext:TColor; //standard colors for editor's text and background
    gd1,gd2,gd0:FChar;
    a:integer;

  inRubyTag: boolean;
  inRubyComment: boolean;

  function RecodeChar(ch:FChar):FChar;
  begin
   {$IFNDEF UNICODE}
    if ch='FF00' then ch:='0020';
   {$ELSE}
    if ch=#$FF00 then ch:=#$0020;
   {$ENDIF}
    result:=ch;
  end;

 //Traces the logical string back until the first character which provides wordstate and dictionary.
 //Stores the result in lastwordstate, lastworddict
  procedure FindLastWordState(cl: integer);
  var xp, yp: integer;
    s: PCharacterProps;
  begin
    xp := ll[cl].xs;
    yp := ll[cl].ys;
    s := @doctr[yp].chars[xp];
    while (xp>0) and (s.wordstate='<') do begin
      Dec(xp);
      s := @doctr[yp].chars[xp];
    end;
    lastwordstate := s.wordstate;
    lastworddict := s.dicidx;
  end;

  //Goes back the line and checks if we're ALREADY inside a given tag (this symbol notwithstanding).
  //Note that ruby tags spanning multiple lines are not supported
  function IsInRubyTag(xp, yp: integer; tagOpenSymbol, tagCloseSymbol: FChar): boolean;
  var s: string;
  begin
    s := doc[yp];
    Dec(xp); //once, because the current symbol doesn't matter
    while (xp>0) and (fgetch(s, xp)<>tagOpenSymbol) and (fgetch(s, xp)<>tagCloseSymbol) do
      Dec(xp);
    Result := (xp>0) and (fgetch(s, xp)=tagOpenSymbol);
  end;

begin
  colback:=Col('Editor_Back');
  coltext:=Col('Editor_Text');
  if doc.Count=0 then
  begin
    doc.Add('');
    doctr.AddNewLine();
  end;
  if printing then st2:=fSettings.cbPrintMeaning.Checked else st2:=sbDisplayMeaning.Down;
  lst0:=false;
  if fSettings.CheckBox56.Checked then lst0:=true;
  if printing then st0:=fSettings.cbPrintReading.Checked else st0:=sbDisplayReading.Down;
  st3:=fSettings.CheckBox42.Checked;
  linec:=2;
  if not TryStrToInt(fSettings.Edit17.Text, st2c) then
    st2c:=1;
  if st3 then inc(linec);
  if st0 or lst0 then inc(linec);
  if st2 then inc(linec,st2c);
  vert:=fSettings.cbVerticalPrint.Checked and printing;
  if vert then screenh:=w else screenh:=h;
  if vert then screenw:=h else screenw:=w;
  if not printing then
  begin
    rs := FontSize;
  end else
  begin
    if not TryStrToInt(fSettings.Edit18.Text, lineh) then
      lineh:=20;
    rs:=screenh div lineh div linec;
  end;
  a:=GetTickCount;

  if ll.Count=0 then //has been invalidated
    RecalculateGraphicalLines(ll, rs, screenw, vert);

  printl:=screenh div (rs*linec);
  xsiz:=rs;
  ycnt:=linec;
  if onlylinl then exit;

 //Find a graphical line which covers the starting point
  cl := -1;
  for i:=0 to ll.Count-1 do
    if (ll[i].ys=y) and (ll[i].xs<=x) then cl:=i;
  Assert(cl>=0, 'Cannot find graphical line which covers the starting point');

  lineh:=rs;
  cx:=x;
  cy:=y;
  px:=0;
  py:=0;
  kanaq:='';

 { Last character's dictionary and word state
  If next character has '<' as a wordstate, we extend these to it. }
  lastwordstate := '-';
  lastworddict := 0;

 { If we're starting from the middle of a paragraph, go back until we find a suitable wordstate }
  if ll[cl].xs > 0 then begin
    FindLastWordState(cl);
    inRubyTag := IsInRubyTag(cx+1,cy,UH_AORUBY_TAG_OPEN,UH_AORUBY_TAG_CLOSE);
    inRubyComment := IsInRubyTag(cx+1,cy,UH_AORUBY_COMM_OPEN,UH_AORUBY_COMM_CLOSE);
  end else begin
    inRubyTag := false;
    inRubyComment := false;
  end;
  //else we except first character of a paragraph to not be '<'

  if printing then
    Canvas.Brush.Color:=clWhite
  else
  if fSettings.cbNoColors.Checked then
    Canvas.Brush.Color:=clWindow
  else
    Canvas.Brush.Color:=colBack;
  rect.Left:=l-2;
  rect.Top:=t-2;
  rect.Right:=l+w+4;
  rect.Bottom:=t+h+4;
  Canvas.FillRect(rect);
  try
  while (py{+lineh*linec}<screenh) and (cl<ll.Count) do
  begin
    cx:=ll[cl].xs;
    cy:=ll[cl].ys;
    wx:=cx+ll[cl].len;
{    if (fSettings.cbDisplayLines.Checked) and (st2) then
    begin
      linec2:=linec;
      if st3 then dec(linec2);
      if vert then
      begin
        canvas.MoveTo(w-py-rs*linec2+l+1,t);
        canvas.LineTo(w-py-rs*linec2+l+1,t+(wx-cx)*rs*2);
      end else
      begin
        canvas.MoveTo(l,py+rs*linec2+t-1);
        canvas.LineTo(l+(wx-cx)*rs*2,py+rs*linec2+t-1);
      end;
    end; }
    while (cx<wx) and ((cx<flength(doc[cy])) or ((kanaq<>'') and st0)) do
    try
     { Note that we can get here even if CX is outside the legal characters for the string.
      This happens if we have some reading remainder in kanaq. Be careful. }

      wordstate:=doctr[cy].chars[cx].wordstate;
      learnstate:=doctr[cy].chars[cx].learnstate;
//      CalcBlockFromTo(false); //why did we even call this here?

      GetTextWordInfo(cx,cy,meaning,reading,kanji);
      if cfExplicitRuby in doctr[cy].chars[cx].flags then
        reading := doctr[cy].chars[cx].ruby; //replacing GetTextWordInfo's reading

      kanjilearned:=(FirstUnknownKanjiIndex(kanji)<0);
      worddict:=doctr[cy].chars[cx].dicidx;
      if wordstate='<'then begin
        worddict:=lastworddict;
        wordstate:=lastwordstate;
      end;
      lastwordstate:=wordstate;
      lastworddict:=worddict;
      undersolid:=worddict<>0;
      if (upcase(wordstate)<>'F') and (upcase(wordstate)<>'D') then reading:='';

      if fSettings.CheckBox36.Checked then begin
        if cx>0 then gd0 := GetDoc(cx-1,cy) else gd0 := UH_NOCHAR;
        if cx<flength(doc[cy]) then gd2 := GetDoc(cx+1,cy) else gd2 := UH_NOCHAR;
        FixReading(gd0,GetDoc(cx,cy),gd2,reading);
      end;

      if fgetchl(doc[cy], cx+1)=UH_AORUBY_TAG_OPEN then
        inRubyTag := true;
      if fgetchl(doc[cy], cx+1)=UH_AORUBY_COMM_OPEN then
        inRubyComment := true;
     //will check for closers after we draw current symbol as is (closers are still inside the tag)

      if not fSettings.cbDisplayLines.Checked then undersolid:=false;
      if fSettings.cbNoColors.Checked then color:=clWindow else color:=colBack;
      if fSettings.cbNoColors.Checked then fcolor:=clWindowText else fcolor:=colText;
      if printing then color:=clWhite;
      if not fSettings.cbNoColors.Checked then begin
        if printing and fSettings.cbNoPrintColors.Checked then begin
          case upcase(wordstate) of
            '-','X':color:=$00FFFFFF;
            '?':color:=$00FFFFFF;
            'P':color:=$00FFFFFF;
            'I':color:=$00FFFFFF;
            'F':color:=$00FFFFFF;
            'D':color:=$00FFFFFF;
            'H':color:=$00FFFFFF;
            'K':color:=$00FFFFFF;
          end;
        end else
        if fMenu.aEditorColors.Checked then begin
          case upcase(wordstate) of
            '-','X':color:=Col('Editor_Untranslated');
            '?':color:=Col('Editor_NotFound');
            'P':color:=Col('Editor_Particle');
            'I':color:=Col('Editor_Untranslated');
            'F':color:=Col('Editor_Translated');
            'D':color:=Col('Editor_Translated');
            'H':color:=Col('Editor_Translated');
            'K':color:=Col('Editor_Translated');
          end;
        end else
          color:=Col('Editor_Untranslated');
        if inRubyTag then fcolor:=Col('Editor_AozoraTag');
        if inRubyComment then fcolor:=Col('Editor_AozoraComment');
      end;
      invert:=false;
      if (fSettings.cbNoMeaningLearned.Checked) and (learnstate>1) and (learnstate<4) then meaning:='';
      if upcase(wordstate)<>wordstate then boldness:=true else boldness:=false;
      if printing and fSettings.cbNoPrintColors.Checked then
      begin
        //Nothing.
      end else
        if fMenu.aEditorColors.Checked then
          case learnstate of
            0: color:=Col('Editor_Problematic');
            1: color:=Col('Editor_Unlearned');
            2: color:=Col('Editor_Learned');
            3: color:=Col('Editor_Mastered');
          end;
      if not fSettings.cbUserBold.Checked then boldness:=false;
      if fSettings.cbNoReadingLearned.Checked and kanjilearned then reading:='';

      if printing then begin
        Canvas.Brush.Color:=clWhite;
        Canvas.Font.Color:=clBlack;
      end else
      if fSettings.cbNoColors.Checked then begin
        Canvas.Brush.Color:=clWindow;
        Canvas.Font.Color:=clWindowText;
      end else begin
        Canvas.Brush.Color:=colBack;
        Canvas.Font.Color:=colText;
      end;

      if (st2) and (meaning<>'') then
      begin
        cnty:=py+rs*2;
        we:=cx+1;
        cntx:=px+rs*2;
        while (we<wx) and (we<cx+6) and (doctr[cy].chars[we].dicidx=0) and (doctr[cy].chars[we].wordstate<>'?') do
        begin
          if IsHalfWidth(we,cy) and not vert then inc(cntx,rs) else inc(cntx,rs*2);
          inc(we);
        end;
        if st0 or lst0 then cnty:=cnty+rs;
        if vert then
        begin
          realx:=w-cnty-st2c*rs;
          realy:=px;
          realx2:=w-cnty;
          realy2:=cntx;
        end else
        begin
          realx:=px;
          realy:=cnty;
          realx2:=cntx;
          realy2:=cnty+st2c*rs;
        end;
        rect.left:=realx+l+2;
        rect.right:=realx2+l-2;
        rect.top:=realy+t;
        rect.bottom:=realy2+t;
        canvas.Font.Name:=FontEnglish;
        if not fSettings.CheckBox27.Checked then
          canvas.Font.Height:=rs else
          canvas.Font.Height:=rs*2;
        canvas.Font.Style:=[];
        DrawText(canvas.Handle,pchar(meaning),length(meaning),rect,DT_WORDBREAK);
        if fSettings.cbDisplayLines.Checked then
          if vert then
          begin
            canvas.MoveTo(realx2+l+1,realy+t);
            canvas.LineTo(realx+l+1,realy+t);
            canvas.LineTo(realx+l+1,realy2+t);
            canvas.LineTo(realx2+l+1,realy2+t);
          end else
          begin
            canvas.MoveTo(realx+l,realy+t-1);
            canvas.LineTo(realx+l,realy2+t-1);
            canvas.LineTo(realx2+l,realy2+t-1);
            canvas.LineTo(realx2+l,realy+t-1);
          end;
      end;
      if showroma then
        if curlang='c'then
          reading:=ConvertPinYin(KanaToRomaji(reading,romasys,curlang))
        else
          reading:=fstr(KanaToRomaji(reading,romasys,curlang));
      if reading<>'' then kanaq:=kanaq+reading;
      cntx:=px;

      if printing then
        Canvas.Font.Color:=clBlack
      else
      if fSettings.cbNoColors.Checked then
        Canvas.Font.Color:=clWindowText
      else
        Canvas.Font.Color:=colText;
      if not fSettings.cbNoColors.Checked then
      begin
        if (fSettings.CheckBox41.Checked) and ((EvalChar(GetDoc(cx,cy))>4) or (EvalChar(GetDoc(cx,cy))=0)) then
          canvas.Font.Color:=Col('Editor_ASCII');
        if wordstate='I'then
          canvas.Font.Color:=Col('Editor_Active')
        else
        begin
          canvas.Font.Color:=fcolor;
          if (cy=ins.y) and (cx>=ins.x) and (cx<ins.x+inslen) then
            canvas.Font.Color:=Col('Editor_Aftertouch');
          canvas.Brush.Color:=color;
        end;
      end;

      if st0 then for i:=1 to 2 do if kanaq<>'' then
      if (i=1) or (vert) or (not IsHalfWidth(cx,cy)) then
      begin
        if vert then
        begin
          realx:=w-py-rs-1;
          realy:=cntx;
        end else
        begin
          realx:=cntx;
          realy:=py+1;
        end;
        if showroma then
        begin
          if curlang='c'then
            DrawUnicode(canvas,realx+l,realy+t-1,rs,fcopy(kanaq,1,2),FontChineseGrid)
          else
            DrawUnicode(canvas,realx+l,realy+t-1,rs,fcopy(kanaq,1,2),FontJapaneseGrid);
          fdelete(kanaq,1,2);
        end else
        begin
          if curlang='c'then
            DrawUnicode(canvas,realx+l,realy+t-1,rs,fcopy(kanaq,1,1),FontChineseGrid)
          else
            DrawUnicode(canvas,realx+l,realy+t-1,rs,fcopy(kanaq,1,1),FontJapaneseGrid);
          fdelete(kanaq,1,1);
        end;
        inc(cntx,rs);
      end;
      if boldness then canvas.Font.Style:=[fsBold] else canvas.Font.Style:=[];
      if vert then
      begin
        realx:=w-py-rs*2;
        if st0 or lst0 then realx:=realx-rs;
        realy:=px;
      end else
      begin
        realx:=px;
        realy:=py;
        if st0 or lst0 then realy:=realy+rs;
      end;
      rect.Left:=realx+l;
      rect.Right:=realx+l+rs*2;
      if (not vert) and (IsHalfWidth(cx,cy)) then rect.Right:=realx+l+rs;
      rect.Top:=realy+t;
      rect.Bottom:=realy+t+rs*2;
      canvas.FillRect(rect);
      if curlang='c'then
        DrawUnicode(canvas,realx+l,realy+t,rs*2,RecodeChar(GetDoc(cx,cy)),FontChineseGrid)
      else
        DrawUnicode(canvas,realx+l,realy+t,rs*2,RecodeChar(GetDoc(cx,cy)),FontJapaneseGrid);

     //we check for openers before rendering, and for closers here
      if fgetchl(doc[cy], cx+1)=UH_AORUBY_TAG_CLOSE then
        inRubyTag := false;
      if fgetchl(doc[cy], cx+1)=UH_AORUBY_COMM_CLOSE then
        inRubyComment := false;

      if undersolid and st2 and fSettings.cbDisplayLines.Checked then
        if vert then
        begin
          canvas.MoveTo(realx+l,realy+t);
          canvas.LineTo(realx+l,realy+t+rs*2);
        end else
        begin
          canvas.MoveTo(realx+l,realy+t+rs*2);
          canvas.LineTo(realx+l+rs*2,realy+t+rs*2);
        end;

      canvas.Font.Style:=[];
      if printing then begin
        Canvas.Brush.Color:=clWhite;
        Canvas.Font.Color:=clBlack;
      end else
      if fSettings.cbNoColors.Checked then begin
        Canvas.Brush.Color:=clWindow;
        Canvas.Font.Color:=clWindowText;
      end else begin
        Canvas.Brush.Color:=colBack;
        Canvas.Font.Color:=colText;
      end;

      if (not vert) and (IsHalfWidth(cx,cy)) then inc(px,rs) else inc(px,rs*2);
      inc(cx);
    except
      on E: Exception do begin
        E.Message := 'Paint exception ('+inttostr(cx)+','+inttostr(cy)+'): '+E.Message;
        raise;
      end;
    end;

   //Next line
    inRubyTag := false;
    inRubyComment := false;
    inc(py,rs*linec);
    px:=0;
    inc(cl);
  end;
  except
    showmessage('Paint exception: '+(ExceptObject as Exception).Message);
  end;
end;


procedure TfTranslate.ClearInsBlock;
begin
  if (priorkanji<>'') and fSettings.cbAdjustCharPriorities.Checked then
  begin
    if TUserPrior.Locate('Kanji',priorkanji) then
      TUserPrior.Edit([TUserPrior.Field('Count')],[inttostr(TUserPrior.Int(TUserPrior.Field('Count'))+1)])
    else
      TUserPrior.Insert([priorkanji,'1']);
    priorkanji:='';
    fMenu.ChangeUserData;
  end;
  ins := SourcePos(-1,-1);
  inslen:=0;
  insertbuffer:='';
end;

{ Called when the insert is not finalized, but we really have to end it now.
 Either cancels it or finalizes it if it was already confirmed. }
procedure TfTranslate.CloseInsert;
begin
  if not insconfirmed then begin
    resolvebuffer:=false; //cancel suggestion
    if insertbuffer<>'' then ResolveInsert(true);
  end;
  ClearInsBlock;
end;

{ Called when we are about to do an operation which requires us to not be in insert mode.
 Returns true if the insert mode was aborted, or false if according to user preferences this is impossible. }
function TfTranslate.TryReleaseCursorFromInsert: boolean;
begin
  if insertbuffer='' then begin //not in insert mode
    Result := true;
    exit;
  end;

  if insconfirmed then begin //insert confirmed -- can relatively safely close insert
    CloseInsert();
    Result := true;
    exit;
  end;

  if fSettings.rgReleaseCursorMode.ItemIndex=0 then begin //disallow
    Result := false;
    exit;
  end;

  CloseInsert();
  Result := true;
end;

procedure TfTranslate.DisplayInsert(const convins:FString;transins:TCharacterPropArray;leaveinserted:boolean);
var i:integer;
  s: FString;
  lp: PCharacterLineProps;
begin
  if ins.x=-1 then
  begin
    ins:=rcur;
    inslen:=0;
  end;
  s:=doc[ins.y];
  fdelete(s,ins.x+1,inslen);
  doc[ins.y]:=s;
  lp:=doctr[ins.y];
  lp.DeleteChars(ins.x,inslen);
  inslen:=flength(convins);
  if transins=nil then begin
    SetLength(transins, flength(convins));
    for i:=1 to flength(convins) do
      transins[i-1].SetChar('I', 9, 0, 1);
  end;
  doc[ins.y]:=fcopy(doc[ins.y],1,ins.x)+convins+fcopy(doc[ins.y],ins.x+1,flength(doc[ins.y])-ins.x);
  doctr[ins.y].InsertChars(ins.x,transins);
  linl.Clear;
  RenderText(cur.x,cur.y,EditorPaintBox.Canvas,0,0,EditorPaintBox.Width-4,
    EditorPaintBox.Height-4,linl,printl,lastxsiz,lastycnt,false,true);
  SetCurPos(ins.x+inslen, ins.y);
  if not leaveinserted then
  begin
    insconfirmed:=true;
  end;
end;

procedure TfTranslate.ResolveInsert(final:boolean);
var s,s2,s3:string;
  i:integer;
  lp: TCharacterPropArray;
begin
  if (ins.x=-1) and (final) then exit;
  if (buffertype='H') and (resolvebuffer) then
  begin
    with fUser do
    if StringGrid1.Visible then
    begin
      s:=curkanji;
      priorkanji:=curkanji;
      s2:=GetInsertKana(false);
      s3:=curphonetic;
      //Delete common ending
      while (s<>'') and (s3<>'') and (fgetch(s,flength(s))=fgetch(s3,flength(s3))) do
      begin
        fdelete(s,flength(s),1);
        fdelete(s3,flength(s3),1);
      end;
      if (s='') and ({$IFNDEF UNICODE}curkanji[3]>='A'{$ELSE}Ord(curkanji[1]) and $00F0 > $00A0{$ENDIF}) then //TODO: Only 3-rd symbol? WTF?
        s:=curkanji
      else
        s:=s+copy(s2,length(s3)+1,length(s2)-length(s3));
      DisplayInsert(s,nil,true);
    end else
    if not final then
      DisplayInsert(GetInsertKana(true),nil,true);
    if final then
    begin
      SetWordTrans(ins.x,ins.y,[tfManuallyChosen],false);
      insconfirmed:=true;
      mustrepaint:=true;
      ShowText(false);
    end;
  end else
  begin
    if final then
    begin
      s:=GetInsertKana(false);
      SetLength(lp, flength(s));
      for i:=0 to flength(s)-1 do
        if i=0 then
          lp[i].SetChar(buffertype, 9, 0, 1)
        else
          lp[i].SetChar('<', 9, 0, 1); //word continues
      DisplayInsert(s,lp,true);
      if resolvebuffer then SetWordTrans(ins.x,ins.y,[tfManuallyChosen],false);
      insconfirmed:=true;
      mustrepaint:=true;
      ShowText(false);
    end else
      DisplayInsert(GetInsertKana(true),nil,true);
  end;
end;

function TfTranslate.GetInsertKana(display:boolean):FString;
begin
  if curlang='j'then
  begin
    if buffertype='H' then
      Result:=RomajiToKana('H'+lowercase(insertbuffer),romasys,false,curlang)
    else
    if buffertype='K'then
      Result:=RomajiToKana('K'+lowercase(insertbuffer),romasys,false,curlang)
    else
      Result:=fstr(insertbuffer); //latin
  end else
  begin
    if display then
      Result:=fstr(insertbuffer)
    else
    if buffertype='H' then
      Result:=RomajiToKana(lowercase(insertbuffer),romasys,false,curlang)
    else
      Result:=fstr(insertbuffer);
  end;
end;

procedure TfTranslate.InsertCharacter(c:char);
const
  DEFCPROPS: TCharacterProps = (wordstate:'-';learnstate:9;dicidx:0;docdic:1);
var chartype:char;
    immchar:string;
    immmode:boolean;
begin
  if (c='[') or (c=']') then
  with fUser do
  begin
    if (c='[') and (StringGrid1.Row>1) then StringGrid1.Row:=StringGrid1.Row-1;
    if (c=']') and (StringGrid1.Row<StringGrid1.RowCount-1) then StringGrid1.Row:=StringGrid1.Row+1;
    if insconfirmed then ResolveInsert(true);
    if (StringGrid1.RowCount>1) and (StringGrid1.Visible) and (ins.x<>-1) then ShowHint else HideHint;
    exit;
  end;
  if insconfirmed then ClearInsBlock;
  FileChanged:=true;
  immmode:=sbAsciiMode.down;
  if (c=' ') and (insertbuffer<>'') then
  begin
   //Accept suggestion
    resolvebuffer:=sbKanjiMode.down;
    ResolveInsert(true);
    if sbKanjiMode.down then exit;
  end;
  if (c=#13) and (insertbuffer<>'') then
  begin
   //Reject suggestion
    resolvebuffer:=false;
    ResolveInsert(true);
    if sbKanjiMode.down then exit;
  end;
  if (c=#8) and (insertbuffer<>'') then
  begin
    delete(insertbuffer,length(insertbuffer),1);
    DisplayInsert(GetInsertKana(true),nil,insertbuffer<>'');
    mustrepaint:=true;
    ShowText(true);
    exit;
  end;
  if c=#13 then
  begin
//    if blockfromx<>-1 then DeleteSelection();
    SplitLine(rcur.x,rcur.y);
    cur.x:=0;
    inc(cur.y);
    RefreshLines;
    exit;
  end;
  if c=#8 then
  begin
    if (dragstart.x<>rcur.x) or (dragstart.y<>rcur.y) then DeleteSelection() else
    begin
      if cur.x>0 then dec(cur.x) else
        if rcur.x=0 then
        begin
          dec(cur.y);
          cur.x:=2550;
        end else
        begin
          dec(cur.y);
          cur.x:=linl[cur.y].len;
        end;
      ShowText(true);
      DeleteCharacter(rcur.x,rcur.y);
    end;
    RefreshLines;
    exit;
  end;
  immchar:='';
  case c of
    ',':immchar:={$IFNDEF UNICODE}'3001'{$ELSE}#$3001{$ENDIF};
    '.':immchar:={$IFNDEF UNICODE}'3002'{$ELSE}#$3002{$ENDIF};
    '"':immchar:={$IFNDEF UNICODE}'3003'{$ELSE}#$3003{$ENDIF};
    '<':immchar:={$IFNDEF UNICODE}'3008'{$ELSE}#$3008{$ENDIF};
    '>':immchar:={$IFNDEF UNICODE}'3009'{$ELSE}#$3009{$ENDIF};
    '(':immchar:={$IFNDEF UNICODE}'300C'{$ELSE}#$300C{$ENDIF};
    ')':immchar:={$IFNDEF UNICODE}'300D'{$ELSE}#$300D{$ENDIF};
    '[':immchar:={$IFNDEF UNICODE}'3016'{$ELSE}#$3016{$ENDIF};
    ']':immchar:={$IFNDEF UNICODE}'3017'{$ELSE}#$3017{$ENDIF};
    '{':immchar:={$IFNDEF UNICODE}'3010'{$ELSE}#$3010{$ENDIF};
    '}':immchar:={$IFNDEF UNICODE}'3011'{$ELSE}#$3011{$ENDIF};
    ' ':immchar:={$IFNDEF UNICODE}'0020'{$ELSE}#$0020{$ENDIF};
  end;
  if (AnsiUppercase(c)=c) and ((c<'0') or (c>'9')) then
  begin
    if curlang='c'then chartype:='-'else chartype:='K'
  end else chartype:='H';
  if immmode then chartype:='-';
  if c='''' then chartype:='0';
  if c='+'then chartype:='H';
  if immchar<>'' then chartype:='-';
  if (chartype='-') then
  begin
    resolvebuffer:=false;
    if insertbuffer<>'' then ResolveInsert(true);
    ClearInsBlock;
    if (immchar<>'') and (not immmode) then
      DisplayInsert(immchar,CharPropArray(DEFCPROPS),false)
    else
      DisplayInsert(UnicodeToHex(c),CharPropArray(DEFCPROPS),false);
    mustrepaint:=true;
    ShowText(true);
    exit;
  end;
  if insertbuffer='' then
  begin
    if chartype='0'then buffertype:='-'else buffertype:=chartype;
    insertbuffer:=c;
    insconfirmed:=false;
  end else
  begin
    if (chartype<>'0') and (chartype<>buffertype) then
    begin
      resolvebuffer:=false;
      ResolveInsert(true);
      ClearInsBlock;
      buffertype:=chartype;
      insertbuffer:=c;
    end else
      insertbuffer:=insertbuffer+c;
    insconfirmed:=false;
  end;
  DisplayInsert(GetInsertKana(true),nil,true);
//  resolvebuffer:=true;
//  ResolveInsert(false);
  mustrepaint:=true;
  ShowText(true);
//  Look(false);
end;

procedure TfTranslate.CheckTransCont(x,y:integer);
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

procedure TfTranslate.SplitLine(x,y:integer);
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

procedure TfTranslate.JoinLine(y:integer);
begin
  if y+1=doc.Count then exit;
  doc[y]:=doc[y]+doc[y+1];
  doctr[y].AddChars(doctr[y+1]^);
  doc.delete(y+1);
  doctr.DeleteLine(y+1);
end;

procedure TfTranslate.DeleteCharacter(x,y:integer);
begin
  if flength(doc[y])<=x then JoinLine(y) else
  begin
    doc[y]:=fcopy(doc[y],1,x)+fcopy(doc[y],x+2,flength(doc[y])-x-1);
    doctr[y].DeleteChar(x);
    CheckTransCont(x,y);
  end;
end;

procedure TfTranslate.RefreshLines;
begin
  linl.Clear;
  mustrepaint:=true;
  ShowText(true);
end;

{ Set word translation to whatever is selected in Dictionary Search results grid,
 or to the first result if gridfirst==true }
function TfTranslate.SetWordTrans(x,y:integer;flags:TSetWordTransFlags;gridfirst:boolean):integer;
var i: integer;
  word: PSearchResult;
begin
  with fUser do
  begin
    if gridfirst then
      i:=0
    else
      if not StringGrid1.Visible then
        i:=-1
      else
        i:=StringGrid1.Row-1;
    if dicrl.Count=0 then i:=-1;
    if i<0 then
      word := nil
    else
      word := dicrl[i];
  end;
  Result := SetWordTrans(x,y,flags,word);
end;

{
Set word translation to the specified article.
word:
  a string specifying dictionary + article index, in fUser.dicrl list format
}
function TfTranslate.SetWordTrans(x,y:integer;flags:TSetWordTransFlags;const word:PSearchResult):integer;
var wordpart:char;
    i:integer;
    rlen:integer;
    s,s2:string;
    wt:integer;
    dw:string;

  globdict_s: string;
  learnstate_s: string;

  wordstate: char;
  learnstate: byte;
  worddict: integer;
  globdict: integer;
begin
  FileChanged:=true;
  if fSettings.cbNoSearchParticles.Checked then flags := flags - [tfScanParticle];
  if (y=-1) or (y>=doctr.Count) or (x=-1) then begin
    Result := 0;
    exit;
  end;
  s2:=GetDoc(x,y);
  dw:=GetDocWord(x,y,wt,not (tfManuallyChosen in flags));
  rlen:=flength(dw);
  globdict:=0;

  if word=nil then
  begin
    wordpart:='-';
    worddict:=0;
    learnstate:=9;
    if wt=1 then rlen:=1;
  end else
  begin
    wordpart:=word.sdef;
    worddict:=word.dicindex;
    s:=word.ArticlesToString;
    globdict:=0;
    if (pos(UH_LBEG+'d',s)>0) then
    begin
      globdict_s:=copy(s,pos(UH_LBEG+'d',s)+2,length(s)-pos(UH_LBEG+'d',s)-1);
      globdict_s:=copy(globdict_s,1,pos(UH_LEND,globdict_s)-1);
      globdict := docdic.IndexOf(globdict_s);
      if globdict<0 then
      begin
        docdic.add(globdict_s);
        globdict:=docdic.Count-1;
      end;
    end;
    if word.userindex=0 then
      learnstate:=9
    else
    begin
      TUser.Locate('Index',word.userindex);
      learnstate_s:=TUser.Str(TUserScore);
      if learnstate_s='' then
        learnstate:=9
      else
      if (Ord(learnstate_s[1])>=Ord('0')) and (Ord(learnstate_s[1])<=Ord('9')) then
        learnstate := Ord(learnstate_s[1])-Ord('0')
      else
        learnstate := 9;
    end;

    rlen:=word.slen;
  end;
 //This subroutine ^^^:
 //local --- s, i, globdict_s, learnstate_s
 //in    --> word, wt
 //out   <-- wordpart, worddict, globdict, learnstate, rlen,

  if wordpart='-' then begin
    if wt<>1 then wordstate:='-' else wordstate:='?'
  end else
    case wt of
      2:if fSettings.cbNoTranslateHiragana.Checked then wordstate:='-'else wordstate:='H';
      3:if s2={$IFNDEF UNICODE}'30FC'{$ELSE}#$30FC{$ENDIF} then wordstate:='-' else wordstate:='K';
      1:if wordpart='I'then wordstate:='D' else wordstate:='F';
      else wordstate:='-';
    end;
  if wordpart='P' then wordstate:='P';
  if tfManuallyChosen in flags then wordstate:=LoCase(wordstate);

  if wordstate='-' then
    doctr[y].chars[x].SetChar(wordstate, 9, 0, globdict)
  else
    doctr[y].chars[x].SetChar(wordstate, learnstate, worddict, globdict);
  for i:=2 to rlen do
    if (x+i-1)<flength(doc[y]) then
      doctr[y].chars[x+i-1].SetChar('<', learnstate, 0, globdict);

  fdelete(dw,1,rlen);
  if (wordstate='K') and (flength(doc[y])>x+rlen) then
  begin
    dw:=GetDocWord(x+rlen,y,wt,false);
    if wt<>2 then dw:='';
  end;
  if flength(dw)>4 then dw:=fcopy(dw,1,4); //yes 4 in unicode. Cut overly long tails
  for i:=flength(dw) downto 1 do
    if EvalChar(fgetch(dw,i))=EC_IDG_CHAR then fdelete(dw,i,length(dw)-i+1); //cut kanji since it clearly belongs to next word
  result:=rlen;
  if (tfScanParticle in flags) and (wordstate<>'-') and (partl.IndexOf(dw)>-1) then
  begin
    if tfManuallyChosen in flags then
      doctr[y].chars[x+rlen].SetChar('p', 9, 0, 1)
    else
      doctr[y].chars[x+rlen].SetChar('P', 9, 0, 1);
    for i:=2 to flength(dw) do
      doctr[y].chars[x+rlen+i-1].SetChar('<', 9, 0, 1);
    result:=rlen+flength(dw);
    exit;
  end;
end;

procedure TfTranslate.DrawCursor(blink:boolean);
function OnScreen(x,y:integer):boolean;
begin
  if (x<0) or (y<0) or (y>=linl.Count) or (x>linl[y].len) then
  begin
    result:=false;
    exit;
  end;
  if (y<view) or (y>=view+printl) then result:=false else result:=true;
end;
procedure CalcCache(x,y:integer);
begin
  cursorposcache:=PosToWidth(x,y);
end;
procedure DrawIt(x,y:integer);
var rect:TRect;
begin
  rect.top:=y*lastxsiz*lastycnt+2;
  rect.left:=cursorposcache*lastxsiz;
  rect.bottom:=rect.top+lastxsiz*lastycnt-1;
  rect.right:=rect.left+2;
  InvertRect(EditorPaintBox.Canvas.Handle,rect);
end;
var tmp: TCursorPos;
begin
  if not ListBox1.Focused then blink:=false;
  if cursorposcache=-1 then CalcCache(oldcur.x,oldcur.y);
  if (OnScreen(oldcur.x,oldcur.y)) and (not cursorblinked) then DrawIt(oldcur.x,oldcur.y-view); //invert=>erase
  tmp := CursorPos(CursorScreenX(), CursorScreenY());
  if (cursorposcache=-1) or (oldcur.x<>tmp.x) or (oldcur.y<>tmp.y) then CalcCache(tmp.x, tmp.y);
  if OnScreen(tmp.x, tmp.y) and blink and cursorblinked then DrawIt(tmp.x,tmp.y-view); //draw new
  if blink then
    cursorblinked:=not cursorblinked
  else
    cursorblinked:=true;
  oldcur := tmp;
end;

{
Updates text selection. A bit suboptimal, with two InSelection checks for every char.
This function can be used without buffering, so try to only draw where it's really needed.
Canvas:
  A canvas to draw on. Either edit control (when updating) or backbuffer.
}
procedure TfTranslate.DrawBlock(Canvas: TCanvas);
var rect:TRect;
    i,js:integer;
    hw: boolean;

    ypos: integer;      //logical line containing this graphical line
    xpos: integer;      //current symbol in the logical line
    llen: integer;      //graphical line length


  function InSelection(x, y: integer; const sel: TTextSelection): boolean;
  begin
    Result := ((y>sel.fromy) or ((y=sel.fromy) and (x>=sel.fromx)))
      and ((y<sel.toy) or ((y=sel.toy) and (x<sel.tox)));
  end;

 //Inverts color for a character at graphical position (i, j),
 //where i is measured in lines and j in half-characters.
  procedure InvertColor(i, js: integer; halfwidth: boolean);
  begin
    rect.top:=(i-view)*lastxsiz*lastycnt+2;
    rect.left:=js*lastxsiz;
    rect.bottom:=rect.top+lastxsiz*lastycnt-1;
    if not halfwidth then
      rect.right:=rect.left+lastxsiz*2
    else
      rect.right:=rect.left+lastxsiz;
    InvertRect(Canvas.Handle,rect);
  end;

begin
  if oldblock.fromx=-1 then
   //safe values for the rest of the algorithm
    oldblock := Selection(-1, -1, -1, -1);
  CalcBlockFromTo(false);

 {
  i: graphical line index
  j: graphical character index
 }
 //For every visible graphical line
  for i:=view to min(linl.Count-1, view+printl) do begin
    xpos := linl[i].xs;
    ypos := linl[i].ys;
    llen := linl[i].len;

    if llen=0 then begin //empty lines get one half-character for selection to indicate they're there
      if InSelection(xpos, ypos, oldblock)
      xor InSelection(xpos, ypos, block) then
        InvertColor(i, 0, {HalfWidth=}true);
    end;

    js:=0; //distantion in half-characters from the left
    while llen>0 do begin
      hw := IsHalfWidth(xpos, ypos);
      if InSelection(xpos, ypos, oldblock)
      xor InSelection(xpos, ypos, block) then
        InvertColor(i, js, hw);
      if hw then inc(js) else inc(js,2);
      Inc(xpos);
      Dec(llen);
    end;
  end;

  oldblock := block;
end;

procedure TfTranslate.RepaintText(dolook:boolean=false);
begin
  mustrepaint:=true;
  ShowText(dolook);
end;

procedure TfTranslate.SetCurPos(x,y:integer);
var i:integer;
begin
  for i:=0 to linl.Count-1 do
    if (linl[i].ys=y) and (linl[i].xs<=x) and (linl[i].xs+linl[i].len>=x) then
  begin
    cur.x:=x-linl[i].xs;
    cur.y:=i;
    break;
  end;
end;

procedure TfTranslate.CopySelection(format:TTextSaveFormat;stream:TStream;
  AnnotMode: TTextAnnotMode);
begin
  CalcBlockFromTo(false);
  SaveText(AnnotMode,format,stream,@block);
end;

{ Receives a string contanining multiple lines of text separated by CRLF.
 Splits into lines, inserts at the current position of the cursor.
 Expands ruby, if AnnotMode dictates so.

 Props may be omitted, in which case default props are used for all symbols }
procedure TfTranslate.PasteText(const chars: FString; const props: TCharacterLineProps;
  AnnotMode: TTextAnnotMode);
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
  if rcur.y >= doc.Count then begin
    doc.Add('');
    doctr.AddNewLine;
  end else
    SplitLine(rcur.x,rcur.y);

  y:=rcur.y;
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
  RefreshLines;
  SetCurPos(l,y);
  FileChanged:=true;
end;

procedure TfTranslate.PasteOp;
var AnnotMode: TTextAnnotMode;
  ms: TMemoryStream;
  props: TCharacterLineProps;
  Loaded: boolean;
begin
  if not TryReleaseCursorFromInsert() then
    exit; //cannot do!

  DeleteSelection; //selected text is replaced

  if fSettings.cbLoadAozoraRuby.Checked then
    AnnotMode := amRuby
  else
    AnnotMode := amDefault;

  Loaded := false;
  if fMenu.GetClipboard(CF_WAKAN, ms) then
  try
    ms.Seek(0,soFromBeginning);
    try
      Loaded := LoadWakanText(ms,{silent=}true);
    except
      on E: EBadWakanTextFormat do Loaded := false;
    end;
  finally
    FreeAndNil(ms);
  end;

  if not Loaded then begin //default to bare text
    props.Clear;
    PasteText(clip,props,AnnotMode);
  end;

  ShowText(true);
end;

procedure TfTranslate.DeleteSelection;
var i:integer;
begin
  CalcBlockFromTo(false);
  SetCurPos(block.fromx,block.fromy);
  if block.fromy=block.toy then
  begin
    doc[block.fromy]:=fcopy(doc[block.fromy],1,block.fromx)
      +fcopy(doc[block.fromy],block.tox+1,flength(doc[block.fromy])-block.tox);
    doctr[block.fromy].DeleteChars(block.fromx, block.tox-block.fromx);
  end else
  begin
    doc[block.fromy]:=fcopy(doc[block.fromy],1,block.fromx);
    doc[block.toy]:=fcopy(doc[block.toy],block.tox+1,flength(doc[block.toy])-block.tox);
    if block.fromx < doctr[block.fromy].charcount then
      doctr[block.fromy].DeleteChars(block.fromx);
    if block.tox < doctr[block.toy].charcount then
      doctr[block.toy].DeleteChars(0, block.tox)
    else
      if doctr[block.toy].charcount>0 then
        doctr[block.toy].DeleteChars(0);
    for i:=block.fromy+1 to block.toy-1 do
    begin
      doc.Delete(block.fromy+1);
      doctr.DeleteLine(block.fromy+1);
    end;
    JoinLine(block.fromy);
    RefreshLines;
  end;
  FileChanged:=true;
end;

procedure TfTranslate.GetTextWordInfo(cx,cy:integer;var meaning:string;var reading,kanji:string);
var dnam:string;
    dic:TJaletDic;
    i:integer;
    defy,s:string;
  markers: TMarkers;
begin
  meaning:='';
  reading:='';
  kanji:='';
  if doctr[cy].chars[cx].dicidx<>0 then
  begin
    i := doctr[cy].chars[cx].docdic;
    dnam := docdic[i];
    dic:=dicts.Find(dnam);
    if (dic<>nil) and (dic.loaded) then
      with dic.GetRecord(doctr[cy].chars[cx].dicidx) do try
        if dic.SupportsMarkers then
          meaning:=GetArticleBody
        else
          meaning:=ConvertEdictEntry(GetArticleBody,markers);
        reading:=GetPhonetic;
        kanji:=GetKanji;
      finally
        Free;
      end;
  end else
  if doctr[cy].chars[cx].wordstate='?'then
  begin
    if TChar.Locate('Unicode',GetDoc(cx,cy)) then
    begin
      TCharRead.SetOrder('');
      TCharRead.Locate('Kanji',TChar.TrueInt(TCharIndex));
      while (not TCharRead.EOF) and (TCharRead.Int(TCharReadKanji)=TChar.Int(TCharIndex)) do
      begin
        s:=TCharRead.Str(TCharReadReading);
        if ((curlang='j') and (TCharRead.Int(TCharReadType)=3)) or ((curlang='c') and (TCharRead.Int(TCharReadType)=7)) then
        begin
          if defy='' then defy:=defy+s else defy:=defy+', '+s;
        end;
        TCharRead.Next;
      end;
      meaning:=defy;
    end;
  end;
  while (reading<>'') and (kanji<>'') and (fgetch(reading,flength(reading))=fgetch(kanji,flength(kanji))) do
  begin
    fdelete(reading,flength(reading),1);
    fdelete(kanji,flength(kanji),1);
  end;
end;

function TfTranslate.GetDocWord(x,y:integer;var wordtype:integer;stopuser:boolean):string;
var wt2:integer;
    i:integer;
    nmk:boolean;
    tc:string;
    honor:boolean;
    stray:integer;
begin
  if (y=-1) or (y>doc.Count-1) or (x>flength(doc[y])-1) or (x=-1) then
  begin
    wordtype:=0;
    result:='';
    exit;
  end;
  if curlang='c'then
  begin
    result:='';
    wordtype:=1;
    for i:=1 to 4 do
    begin
      result:=result+fgetch(doc[y],x+1);
      inc(x);
      if x=flength(doc[y]) then exit;
    end;
    exit;
  end;
  tc:=fgetch(doc[y],x+1);
  honor:=false;
  if (tc={$IFNDEF UNICODE}'304A'{$ELSE}#$304A{$ENDIF})
  or (tc={$IFNDEF UNICODE}'3054'{$ELSE}#$3054{$ENDIF}) then honor:=true;
  if (honor) and (flength(doc[y])>x+2) and (EvalChar(fgetch(doc[y],x+2))<=2) then
    wordtype:=EvalChar(fgetch(doc[y],x+2))
  else
    wordtype:=EvalChar(fgetch(doc[y],x+1));
  if wordtype>4 then wordtype:=4;
  nmk:=false;
  stray:=0;
  result:=fgetch(doc[y],x+1);
  repeat
    inc(x);
    if stopuser and IsLocaseLatin(doctr[y].chars[x].wordstate) then exit;
    wt2:=0;
    if x<flength(doc[y]) then
    begin
      wt2:=EvalChar(fgetch(doc[y],x+1));
      if wt2>4 then wt2:=4;
      if (wordtype=1) and (wt2=2) then begin
        nmk:=true;
        if stray=0 then stray:=1 else stray:=-1;
      end;
      if (nmk) and (wt2=1) then
      begin
        if stray=1 then wt2:=1 else wt2:=4;
        stray:=-1;
        nmk:=false;
      end;
      if (wt2<>wordtype) and ((wordtype<>1) or (wt2<>2)) then exit;
    end;
    if wt2=0 then exit;
    result:=result+fgetch(doc[y],x+1);
  until false;
end;

function TfTranslate.CursorScreenX:integer;
begin
  if (cursorend) and (cur.x=0) and (cur.y>0) then result:=linl[cur.y-1].len else result:=cur.x;
end;

function TfTranslate.CursorScreenY:integer;
begin
  if (cursorend) and (cur.x=0) and (cur.y>0) then result:=cur.y-1 else result:=cur.y;
end;

procedure TfTranslate.CalcMouseCoords(x,y:integer;var rx,ry:integer);
var cx,cy:integer;
begin
  rx:=-1;
  ry:=-1;
  cx:=x div (lastxsiz);
  cy:=y div (lastxsiz*lastycnt)+view;
  if cy<0 then cy:=0;
  cx:=WidthToPos(cx,cy);
  if cy>=linl.Count then exit;
  ry:=linl[cy].ys;
  rx:=cx+linl[cy].xs;
  if (ry>=doc.Count) or (rx>=flength(doc[ry])) then
  begin
    ry:=-1;
    rx:=-1;
    exit;
  end;
end;

procedure TfTranslate.SetFontSize(Value: integer);
begin
  if FFontSize=Value then exit;
  FFontSize := Value;
  fMenu.aEditorSmallFont.Checked:=(FFontSize=FontSizeSmall);
  fMenu.aEditorLargeFont.Checked:=(FFontSize=FontSizeMedium);
  fMenu.aEditorMedFont.Checked:=(FFontSize=FontSizeLarge);
  if cbFontSize.Text<>IntToStr(Value) then begin
    cbFontSize.Text := IntToStr(Value);
    cbFontSizeGuessItem(cbFontSize.Text);
    cbFontSizeChange(cbFontSize);
  end;
  RefreshLines;
end;

//If there's an item in the listbox with the exact same text, select that item
//instead of just setting text property (there's a difference: ItemIndex is set)
//Does not call OnChange.
procedure TfTranslate.cbFontSizeGuessItem(Value: string);
var i: integer;
begin
  for i := 0 to cbFontSize.Items.Count - 1 do
    if cbFontSize.Items[i]=Value then begin
      cbFontSize.ItemIndex := i;
      exit;
    end;
end;

procedure TfTranslate.cbFontSizeChange(Sender: TObject);
var tmp: integer;
begin
  if TryStrToInt(cbFontSize.Text,tmp) and (tmp>=2) then
    SetFontSize(tmp);
end;

procedure TfTranslate.cbFontSizeExit(Sender: TObject);
var tmp: integer;
begin
  if TryStrToInt(cbFontSize.Text,tmp) and (tmp>=2) then begin
    SetFontSize(tmp);
    cbFontSizeGuessItem(cbFontSize.Text);
  end else
    cbFontSize.Text := IntToStr(FontSize);
end;

procedure TfTranslate.cbFontSizeKeyPress(Sender: TObject; var Key: Char);
begin
  if Ord(Key)=VK_RETURN then
    ListBox1.SetFocus; //jump to editor
end;

{ Wakan has "half-width" and "full-width" symbols, and therefore position
 in the string (POS) and the number of "graphical half-units" (WIDTH) are
 not the same.
 These functions convert between these two things! They are unrelated to
 pixel widths. }

function TfTranslate.PosToWidth(x,y:integer):integer;
var i,cx,cy:integer;
begin
  if (x<0) or (y<0) or (y>=linl.Count) then
  begin
    result:=-1;
    exit;
  end;
  cx:=linl[y].xs;
  cy:=linl[y].ys;
  result:=0;
  for i:=0 to x-1 do
  begin
    if IsHalfWidth(cx,cy) then inc(result) else inc(result,2);
    inc(cx);
  end;
end;

function TfTranslate.WidthToPos(x,y:integer):integer;
var i,jx,cx,cy,clen:integer;
begin
  if (x<0) or (y<0) or (y>=linl.Count) then
  begin
    result:=-1;
    exit;
  end;
  cx:=linl[y].xs;
  cy:=linl[y].ys;
  clen:=linl[y].len;
  jx:=0;
  for i:=0 to x-1 do
  begin
    if clen<=0 then break;
    if IsHalfWidth(cx,cy) then inc(jx) else inc(jx,2);
    if jx>=x then
    begin
      if jx=x then result:=i+1 else result:=i;
      exit;
    end;
    inc(cx);
    dec(clen);
  end;
  result:=0;
end;

{ Similar, but error handling is clearly defined. If there's no char at the position,
 closest text position is returned. }
function TfTranslate.HalfUnitsToTextPos(x,y:integer):integer;
var i,jx,cx,cy,clen:integer;
begin
  Assert((y>=0) and (y<linl.Count));
  if x<0 then x:=0;

  cx:=linl[y].xs;
  cy:=linl[y].ys;
  clen:=linl[y].len;

  jx:=0;
  for i:=0 to x-1 do
  begin
    if clen<=0 then break;
    if IsHalfWidth(cx,cy) then inc(jx) else inc(jx,2);
    if jx>=x then
    begin
      if jx=x then result:=i+1 else result:=i;
      exit;
    end;
    inc(cx);
    dec(clen);
  end;
  result:=cx;
end;

{
 Returns closest text position to the given screen point.
 Makes sure what you get is a legal text point, not some negative or over-the-end value.
 NOTE:
  - Do not call if linl is cleared (linl.Count=0)
}
function TfTranslate.GetClosestCursorPos(x,y:integer): TCursorPos;
begin
  if y<0 then y:=0;
  if x<0 then x:=0;
  Result.x := x div lastxsiz;
  Result.y := y div (lastxsiz*lastycnt)+view;
  if Result.y>=linl.Count then
    Result.y:=linl.Count-1;
  Assert(Result.y>=0); //we must have at least one line
  Result.x := HalfUnitsToTextPos(Result.x, Result.y);
  Assert(Result.x>=0);
end;



procedure EditorInit;
begin
  priorkanji:='';
  cursorblinked:=false;
  shiftpressed:=false;
  dragstart := SourcePos(-1, -1);
  oldblock := Selection(-1, -1, -1, -1);
  insconfirmed:=false;
  cursorend:=false;
  lastmm := CursorPos(-1, -1);
end;

initialization
  EditorInit;

end.
