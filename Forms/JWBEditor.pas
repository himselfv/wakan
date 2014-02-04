unit JWBEditor;
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

GetTextSelection() converts that to:
  fromy      first line in selection
  fromx      first selected char
  tox        last line in selection
  toy        last selected char
All inclusive.
}

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, ExtCtrls, Buttons, JWBStrings, JWBWordLookup,
  JWBDicSearch, WakanPaintbox, WinSpeedButton,
  ComCtrls, ToolWin, ImgList, JWBWakanText, JWBIO;

//If enabled, support multithreaded translation
{$DEFINE MTHREAD_SUPPORT}

//Ignore kana only words when auto-translating. Faster but worse results.
//{$DEFINE TL_IGNORE_KANA}

{$IFDEF DEBUG} //don't do in release versions

  //Display a window showing how much time Auto-TL took
  {$DEFINE TLSPEEDREPORT}

{$ENDIF}

type
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

  TCaretState = (csHidden, csVisible, csBlink);

  TSetWordTransFlag = (
    tfScanParticle,
    tfManuallyChosen
  );
  TSetWordTransFlags = set of TSetWordTransFlag;

  {
  Wakan keeps a list of "graphical lines", i.e. lines as displayed on screen.
  They refer to a "logical lines", that is, lines from a text document.
  }
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
    function GetItemPtr(Index: integer): PGraphicalLineInfo; inline;
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

  TTranslationThread = class;
  TTranslationThreads = array of TTranslationThread;

 //Supported document types
  TDocType = (dtText, dtWakanText);

  TfEditor = class(TForm)
    Bevel: TPanel;
    lblControlsHint: TLabel;
    sbDockDictionary: TSpeedButton;
    btnKanjiDetails: TSpeedButton;
    EditorPaintbox: TWakanPaintbox;
    ListBox1: TListBox;
    EditorScrollBar: TScrollBar;
    ToolBar1: TToolBar;
    sbFileNew: TToolButton;
    sbFileOpen: TToolButton;
    sbFileSave: TToolButton;
    ToolButton4: TToolButton;
    sbClipCut: TToolButton;
    sbClipCopy: TToolButton;
    sbClipPaste: TToolButton;
    ToolButton8: TToolButton;
    sbKanjiMode: TToolButton;
    sbKanaMode: TToolButton;
    sbAsciiMode: TToolButton;
    ToolButton12: TToolButton;
    sbDisplayReading: TToolButton;
    sbDisplayMeaning: TToolButton;
    sbUseTlColors: TToolButton;
    ToolButton16: TToolButton;
    sbClearTranslation: TToolButton;
    sbAutoTranslate: TToolButton;
    sbSetTranslation: TToolButton;
    ToolButton20: TToolButton;
    cbFontSize: TComboBox;
    ToolButton21: TToolButton;
    sbPrint: TToolButton;
    BlinkCursorTimer: TTimer;
    OpenTextDialog: TOpenDialog;
    SaveTextDialog: TSaveDialog;
    SaveAsKanaDialog: TSaveDialog;
    ImageList1: TImageList;
    lblFilename: TPanel;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure FormHide(Sender: TObject);
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
    procedure EditorPaintBoxPaint(Sender: TObject; Canvas: TCanvas);
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
    procedure btnKanjiDetailsClick(Sender: TObject);
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

  protected
    FDocFilename:string;
    FDocType: TDocType;
    FDocEncoding: CEncoding; //for text documents
    function Get_doctr(Index: integer): PCharacterLineProps;
  public
    doc: TWakanText;
    property DocFilename: string read FDocFilename write FDocFilename;
    property DocType: TDocType read FDocType write FDocType;
    property DocEncoding: CEncoding read FDocEncoding write FDocEncoding;
    property doctr[Index: integer]: PCharacterLineProps read Get_doctr;

  protected //Unsorted
    procedure GetTextWordInfo(cx,cy:integer;var meaning:string;var reading,kanji:FString);
    procedure DocGetDictionaryEntry(Sender: TObject; const APos: TSourcePos;
      out kanji, reading: FString; out meaning: string);
    function SetWordTrans(x,y:integer;flags:TSetWordTransFlags;gridfirst:boolean):integer; overload;
    function SetWordTrans(x,y:integer;flags:TSetWordTransFlags;const word:PSearchResult):integer; overload;
    procedure RefreshLines;
    procedure CopySelection(format:TTextSaveFormat;stream:TStream;
      AnnotMode:TTextAnnotMode=amRuby);
    procedure DeleteSelection;
    procedure PasteOp;
    procedure PasteText(const chars: FString; const props: TCharacterLineProps;
      AnnotMode: TTextAnnotMode);
    function IsHalfWidth(x,y:integer):boolean;
    function PosToWidth(x,y:integer):integer;
    function WidthToPos(x,y:integer):integer;
    function HalfUnitsToCursorPos(x,y: integer):integer;
  public
    function GetDocWord(x,y:integer;var wordtype: TEvalCharType;stopuser:boolean):string;
    function GetWordAtCaret(out AWordtype: TEvalCharType): string;
    function GetClosestCursorPos(x,y:integer):TCursorPos;
    function GetExactLogicalPos(x,y:integer):TSourcePos;
    function TryGetExactLogicalPos(x,y: integer):TSourcePos;

  protected
    FFontSize: integer;
    procedure SetFontSize(Value: integer);
    procedure cbFontSizeGuessItem(Value: string);
  public
    property FontSize: integer read FFontSize write SetFontSize;

  protected //Mostly repainting
    EditorBitmap: TBitmap;
    mustrepaint:boolean;
    function PaintBoxClientRect: TRect;
    procedure RecalculateGraphicalLines(ll: TGraphicalLineList; rs: integer;
      screenw: integer; vert: boolean);
    procedure RenderText(canvas:TCanvas;r:TRect;ll:TGraphicalLineList;
      view:integer; var printl,xsiz,ycnt:integer;printing,onlylinl:boolean);
    procedure ReflowText(force:boolean=false);
  public
    procedure RepaintText;
    procedure ShowText(dolook:boolean);

  protected
    linl: TGraphicalLineList; //lines as they show on screen
    FRCur: TSourcePos;
    CursorEnd: boolean; { Cursor is visually "at the end of the previous line",
      although its logical position is at the start of the next graphical line.
      This is expected in some situations during the editing. }
    lastmm: TCursorPos; //Last character which felt mouse-click over itself.
    dragstart: TSourcePos; {
      When selecting, the position where we started dragging mouse (before this char).
      Selection block is generated from this on mouse-release. }
    lastxsiz: integer; //size of one half-char in pixels, at the time of last render
    lastycnt:integer;
    printl:integer; //number of lines which fit on the screen last time
    FCachedCursorPos:TCursorPos;
    FCursorPosInvalid: boolean;
    function GetCur: TCursorPos;
    procedure SetCur(Value: TCursorPos);
    procedure SetRCur(const Value: TSourcePos);
    procedure InvalidateLines;
    procedure InvalidateCursorPos;
    function GetCursorScreenPos: TCursorPos;
  public
    procedure CursorJumpToLine(newy: integer);
    property RCur: TSourcePos read FRCur write SetRCur; //cursor position in logical coordinates (cursor is before this char)
    property Cur: TCursorPos read GetCur write SetCur; //cursor position (maybe not drawn yet, may differ from where cursor is drawn -- see CursorScreenX/Y)
    property CursorScreenPos: TCursorPos read GetCursorScreenPos; //visible cursor position -- differs sometimes -- see comments

  protected
    FViewPos: TSourcePos; //logical coordinates of a first visible graphical line anchor
    FViewLineCached: integer; //index of a first visible graphical line -- cached
    FUpdatingScrollbar: boolean;
    FNormalizeViewPlanned: boolean; //do NormalizeView on next occasion (i.e. repaint or GetView or something)
    function GetView: integer;
    procedure SetView(Value: integer);
    procedure SetViewPos(const Value: TSourcePos);
    function NormalizeViewPos(const APos: TSourcePos): TSourcePos;
    function NormalizeView: boolean;
    function ScrollIntoView: boolean;
    procedure InvalidateNormalizeView;
    procedure UpdateViewLine;
    procedure InvalidateViewLine;
    function GetScreenLineCount: integer;
    procedure HandleWheel(down:boolean);
    procedure UpdateScrollbar; //to reflect lines.Count/View
  public
    function IsCursorOnScreen(const APos: TCursorPos): boolean;
    property View: integer read GetView write SetView;
    property ViewPos: TSourcePos read FViewPos write SetViewPos;
    property ScreenLineCount: integer read GetScreenLineCount; //number of fully visible graphical lines which fit on the screen

  protected //DrawCaret-related stuff. No one else use this
    FLastCaretPos: TCursorPos; //last position where caret was drawn. Used by DrawCaret
    FCaretPosCache:integer; //cursor X in pixels, from last DrawCaret. -1 means recalculate
    FCaretVisible:boolean; //cursor is currently in "hidden" blink state
  public
    procedure DrawCaret(AState: TCaretState);

  protected //Selection block
    oldblock: TTextSelection; //Selection block last time it was repainted (allows us to only repaint changed parts)
    function GetTextSelection: TTextSelection;
    function BacktrackSelection(const ASelection: TTextSelection): TTextSelection;
  public
    procedure DrawBlock(Canvas: TCanvas; ClientRect: TRect);
    procedure SelectAll;
    property TextSelection: TTextSelection read GetTextSelection; //Selection block in 0-coords [0..doc.Count]x[0..flen(line)-1]

  public //Hint
    procedure ShowHint;
    procedure HideHint;

  protected //Insert buffer
    insconfirmed:boolean;
    priorkanji:string;
    insertbuffer:string; //collects keypresses
    buffertype:char; //type of data in the buffer: H=hiragana, K=katakana, -=latin/unknown
    resolvebuffer:boolean; //set to true before doing ResolveInsert to replace kana with kanji suggestion, false to keep input intact
    shiftpressed:boolean;
    procedure DisplayInsert(const convins:FString;transins:TCharacterPropArray;leaveinserted:boolean);
    procedure ResolveInsert(final:boolean);
    procedure InsertCharacter(c:char);
    procedure ClearInsBlock;
    procedure CloseInsert;
    function TryReleaseCursorFromInsert: boolean;
    function NextSuggestion(const ANext: boolean): boolean;
  public
   //Unfortunately some stuff is used from elswehere
    ins: TSourcePos; //editor aftertouch --- after we have inserted the word, it's highlighted
    inslen: integer;
    function GetInsertKana(const APreview: boolean): FString;

  protected //File opening/saving
    FFileChanged: boolean;
    LastAutoSave:TDateTime;
    FFullTextTranslated: boolean; //applied full text translation at least once since loading
     //this is needed for saving in Kana mode -- we don't show a reminder if it's obvious the text was translated
    SaveAnnotMode: TTextAnnotMode; //if we have saved the file once, we remember the choice
    procedure SetFileChanged(Value: boolean);
  public //File open/save
    procedure ClearEditor;
    procedure OpenAnyFile(const AFilename: string);
    procedure OpenFile(const AFilename: string; const AType: TDocType;
      const AEncoding: CEncoding);
    procedure SaveToFile(const AFilename: string; const AType: TDocType;
      const AEncoding: CEncoding; AnnotMode: TTextAnnotMode);
    function SaveAs: boolean;
    function CommitFile:boolean;
    function ExportAs: boolean;
    property FileChanged: boolean read FFileChanged write SetFileChanged;
    property FullTextTranslated: boolean read FFullTextTranslated write FFullTextTranslated;

  protected
    function SetupSearchRequest: TDicSearchRequest;
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
    FEditor: TfEditor;
    req: TDicSearchRequest;
    dicsl: TSearchResults;
    blockfromy: integer;
    blocktoy: integer;
    blockfromx: integer;
    blocktox: integer;
  public
    constructor Create(AEditor: TfEditor; ablockfromy, ablocktoy: integer);
    destructor Destroy; override;
    procedure Execute; override;
  end;

var
  fEditor: TfEditor;

const
  FontSizeSmall = 8;
  FontSizeMedium = 12;
  FontSizeLarge = 16;

function CursorPos(x,y: integer): TCursorPos; inline;
function Selection(fromx, fromy, tox, toy: integer): TTextSelection; inline;

type
  TRectHelper = record helper for TRect
  protected
    function GetWidth: integer;
    function GetHeight: integer;
  public
    property Width: integer read GetWidth;
    property Height: integer read GetHeight;
  end;

function RectWH(const Left,Top,Width,Height: integer): TRect;

implementation

uses Types, TextTable, JWBMenu, JWBHint, JWBKanjiDetails, JWBKanji,
  JWBSettings, JWBPrint, StdPrompt, JWBKanaConv, JWBUnit,
  JWBCategories, JWBDic, JWBEdictMarkers, JWBFileType,
  JWBUserData, JWBCharData, StreamUtils, JWBLegacyMarkup;

{$R *.DFM}

function CursorPos(x,y: integer): TCursorPos;
begin
  Result.x := x;
  Result.y := y;
end;

function Selection(fromx, fromy, tox, toy: integer): TTextSelection;
begin
  Result.fromy := fromy;
  Result.fromx := fromx;
  Result.toy := toy;
  Result.toy := tox;
end;

function TRectHelper.GetWidth: integer;
begin
  Result := Right-Left;
end;

function TRectHelper.GetHeight: integer;
begin
  Result := Bottom-Top;
end;

function RectWH(const Left,Top,Width,Height: integer): TRect;
begin
  Result.Left := Left;
  Result.Top := Top;
  Result.Right := Result.Left + Width;
  Result.Bottom := Result.Top + Height;
end;

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


var
 //Printing vars
  plinl:TGraphicalLineList; //graphical lines for printing
  printpl:integer;

function GetPageNum(canvas:TCanvas; width,height:integer; userdata:pointer):integer;
var pl,xs,yc:integer;
begin
  plinl.Clear;
  fEditor.RenderText(Canvas,
    RectWH(width div 50,height div 50,width-width div 25,height-height div 25),
    plinl,0,pl,xs,yc,true,true);
  printpl:=pl;
  result:=((plinl.Count-1) div pl)+1;
  if result<1 then result:=1;
end;

procedure DrawPage(canvas:TCanvas; pagenum:integer; width,height,origwidth,origheight:integer; userdata:pointer);
var pl,xs,yc:integer;
begin
  if plinl.Count<=(pagenum-1)*printpl then exit;
  fEditor.RenderText(canvas,
    RectWH(width div 50,height div 50,width-width div 25,height-height div 25),
    plinl,(pagenum-1)*printpl,pl,xs,yc,true,false);
end;

procedure TfEditor.FormCreate(Sender: TObject);
begin
  doc:=TWakanText.Create;
  doc.OnGetDictionaryEntry := DocGetDictionaryEntry;

  docfilename:='';
  FDocType := dtText;
  FDocEncoding := nil;
  FileChanged:=false;
  FullTextTranslated:=false;

  ViewPos := SourcePos(0, 0);
  rcur := SourcePos(-1, -1);
  ins := SourcePos(-1, -1);
  inslen:=0;
  cursorend:=false;
  lastxsiz:=16;
  lastycnt:=2;
  printl:=1;
  mustrepaint:=true;

  FLastCaretPos := CursorPos(-1, -1);
  FCaretPosCache:=-1;
  FCaretVisible:=false;

  priorkanji:='';
  shiftpressed:=false;
  dragstart := SourcePos(-1, -1);
  oldblock := Selection(-1, -1, -1, -1);
  insconfirmed:=false;
  lastmm := CursorPos(-1, -1);

  FFontSize := 0;
 //We need to update controls when we set FontSize, and if we set ItemIndex here,
 //it'll get overwritten for some buggy VCL reason. So we use FormShow.

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

procedure TfEditor.FormDestroy(Sender: TObject);
begin
  FreeAndNil(EditorBitmap);
  linl.Free;
  plinl.Free;
  FreeAndNil(doc);
end;

procedure TfEditor.FormShow(Sender: TObject);
begin
  if FFontSize<=0 then FontSize:=FontSizeMedium; //see FormCreate for explanation
  ShowText(true);
  ListBox1.ItemIndex:=0;
  ListBox1.SetFocus;
end;

procedure TfEditor.FormHide(Sender: TObject);
begin
  fWordLookup.RestoreLookupMode; //which we had overriden with word suggestions
end;

procedure TfEditor.FormResize(Sender: TObject);
begin
  InvalidateLines;
  Invalidate;
 { NormalizeView needs Lines and we don't want to recalculate Lines now --
  the editor might be invisible so why bother. }
  InvalidateNormalizeView;
end;

procedure TfEditor.FormActivate(Sender: TObject);
begin
  ListBox1.SetFocus;
end;

procedure TfEditor.FormDeactivate(Sender: TObject);
begin
  if fHint.Visible then HideHint;
end;


procedure TfEditor.ClearEditor;
begin
  doc.Clear;
  InvalidateLines;
  rcur := SourcePos(0, 0);
  ViewPos := SourcePos(0, 0);
  lblFilename.Caption:=_l('#00678^e<UNNAMED>');
  docfilename:='';
  mustrepaint:=true;
  ShowText(true);
  FFileChanged := false;
  FullTextTranslated := false;
end;

{ Opens a file by guessing format/encoding or asking user for it }
procedure TfEditor.OpenAnyFile(const AFilename:string);
var AEncoding: CEncoding;
begin
  if not FileExists(AFilename) then
    raise Exception.Create('File not found: "'+AFilename+'"');
 //TODO: Open the stream once, reuse for all detections and load
  if IsWakanText(AFilename) then
    OpenFile(AFilename, dtWakanText, nil)
  else begin
    if not Conv_DetectType(AFilename, AEncoding) then begin
      AEncoding:=Conv_ChooseType(curlang='c', AEncoding);
      if AEncoding=nil then exit;
    end;
    OpenFile(AFilename, dtText, AEncoding);
  end;
end;

procedure TfEditor.OpenFile(const AFilename:string; const AType: TDocType;
  const AEncoding: CEncoding);
var LoadAnnotMode: TTextAnnotMode;
begin
  docfilename:=AFilename;
  FDocType := AType;
  FDocEncoding := AEncoding;

  //by default we set SaveAnnotMode to default, meaning no preference has been chosen
  //auto-loaded rubys will be saved either way
  SaveAnnotMode := amDefault;

  //LoadAnnotMode governs how we treat incoming ruby (loading/pasting)
  if fSettings.cbLoadAozoraRuby.Checked then
    LoadAnnotMode := amRuby
  else
    LoadAnnotMode := amNone;

  lblFilename.Caption:=uppercase(ExtractFilename(AFilename));
  doc.Clear;
  InvalidateLines;
  Screen.Cursor:=crHourGlass;
  try

    ViewPos:=SourcePos(0,0);
    rcur:=SourcePos(0,0);
    mustrepaint:=true;
    FileChanged:=false;
    FullTextTranslated:=false;

    case AType of
      dtText: doc.LoadText(docfilename, AEncoding, LoadAnnotMode);
      dtWakanText: doc.LoadWakanText(docfilename);
    end;

    ShowText(true);

  finally
    Screen.Cursor:=crDefault;
  end;
end;

{ Doesn't save Filename, tp or AnnotMode choice. That is correct.
This function can be called by others to make one-time special-format save.
SaveAs does the choice remembering. }
procedure TfEditor.SaveToFile(const AFilename: string; const AType: TDocType;
  const AEncoding: CEncoding; AnnotMode: TTextAnnotMode);
var stream: TStream;
begin
  Screen.Cursor:=crHourGlass;

  if AType=dtWakanText then
    doc.SaveWakanText(AFilename)
  else begin
    stream := TStreamWriter.Create(TFileStream.Create(AFilename, fmCreate),true);
    try
      doc.SaveText(
        AnnotMode,
        TRubyTextFormat.Create(AEncoding),
        stream
      );
    finally
      FreeAndNil(stream);
    end;
  end;
  Screen.Cursor:=crDefault;
  FileChanged:=false;
end;

//Returns false if user have cancelled the dialog
function TfEditor.SaveAs: boolean;
var ADocType: TDocType;
  AAnnotMode: TTextAnnotMode;
  AEncoding: CEncoding;
begin
 //If configured to, or if we have chosen this option before
  if (SaveAnnotMode=amRuby) or fSettings.cbSaveAnnotationsToRuby.Checked then
    SaveTextDialog.FilterIndex := 2 //"Text with readings as Aozora Ruby"
  else
    SaveTextDialog.FilterIndex := 1; //"Text file"

  Result := SaveTextDialog.Execute;
  if not Result then exit;

  AEncoding := nil;
  case SaveTextDialog.FilterIndex of
    1: begin
      ADocType := dtText;
      AAnnotMode := amDefault;
    end;
    2: begin
      ADocType := dtText;
      AAnnotMode := amRuby;
    end;
    3: begin
      ADocType := dtWakanText;
      AAnnotMode := amDefault;
    end;
  else
    exit; //wtf though?
  end;

  //Hack: If the file name has .WTT, we assume the user made an error and chose
  //the wrong item.
  if pos('.WTT',uppercase(SaveTextDialog.FileName))>0 then begin
    ADocType := dtWakanText;
    AAnnotMode := amDefault;
  end;

  //Choose encoding
  if ADocType=dtText then
    AEncoding := Conv_ChooseType(curlang='c', nil);

  SaveToFile(SaveTextDialog.FileName, ADocType, AEncoding, AAnnotMode);
  docfilename:=SaveTextDialog.FileName;
  FDocType := ADocType;
  FDocEncoding := AEncoding;
  lblFilename.Caption:=uppercase(ExtractFilename(SaveTextDialog.FileName));
end;

procedure TfEditor.SetFileChanged(Value: boolean);
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
function TfEditor.CommitFile:boolean;
var i:integer;
begin
  Result := true;
  if not filechanged then exit;

  if (fSettings.CheckBox60.Checked) and (docfilename<>'') then begin
   //Auto-"Yes"
    SaveToFile(docfilename, FDocType, FDocEncoding, SaveAnnotMode);
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
    SaveToFile(docfilename, FDocType, FDocEncoding, SaveAnnotMode);
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

function TfEditor.ExportAs: boolean;
var stream: TStream;
  enctype: CEncoding;
begin
  if not Self.FullTextTranslated then
    Application.MessageBox(
      pchar(_l('#00369^eDo not forget to fill kana readings or use the auto-fill function before using this feature.')),
      pchar(_l('#00364^eNotice')),
      MB_ICONINFORMATION or MB_OK);
  SaveAsKanaDialog.FileName := ExtractFilename(ChangeFileExt(docfilename,'')); //Default name's the same, without extension
  if not SaveAsKanaDialog.Execute then begin
    Result := false;
    exit;
  end;

  case SaveAsKanaDialog.FilterIndex of
    1,2,3: enctype := Conv_ChooseType(false, nil);
    4,5: enctype := TUTF8Encoding; //UTF8 only for HTML, ODT
  else enctype := nil; //should not be used
  end;

  stream := nil;
  try
    stream := TStreamWriter.Create(
      TFileStream.Create(SaveAsKanaDialog.FileName,fmCreate),
      true
    );

    case SaveAsKanaDialog.FilterIndex of
      1: doc.SaveText(amRuby,TKanaOnlyFormat.Create(enctype,{AddSpaces=}true),stream);
      2: doc.SaveText(amRuby,TKanjiKanaFormat.Create(enctype),stream);
      3: doc.SaveText(amRuby,TKanjiOnlyFormat.Create(enctype),stream);
      4: doc.SaveText(amRuby,THtmlFormat.Create([]),stream);
      5: doc.SaveText(amRuby,TOpenDocumentFormat.Create(),stream);
    end;
  finally
    FreeAndNil(stream);
  end;

  Result := true;
end;


{ Auto-translation. Does NOT use anything from DictLookupForm (settings, setup etc). }

constructor TTranslationThread.Create(AEditor: TfEditor; ablockfromy, ablocktoy: integer);
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
  FEditor := AEditor;
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
  req := FEditor.SetupSearchRequest();
  req.Prepare;

  dicsl := TSearchResults.Create;
  try
    i := blockfromy;
    while (not Terminated) and (i<=blocktoy) do begin
      bg:=0;
      en:=flength(FEditor.doc.Lines[i])-1;
      FEditor.AutoTranslateLine(i, bg, en, req, dicsl);
      Inc(i);
    end;

  finally
    FreeAndNil(dicsl);
    FreeAndNil(req);
  end;
end;

{ Instantiates and appropriately configures a search request object for
 auto-translation.
 Do Prepare() with the result after making any modifications to defaults. }
function TfEditor.SetupSearchRequest: TDicSearchRequest;
begin
  Result := TDicSearchRequest.Create;
  Result.st := stJapanese;
  Result.dictgroup := 5;
  Result.MatchType := mtExactMatch;
 { If we used mtMatchLeft, queries like "sama" would get results like "samazama"
  which is obviously not what we want. }
  Result.maxwords:=0; //Ignored if Full is true?
  Result.full := true; //Always true in auto-translation?
  Result.AutoDeflex := true; //Always true for auto-translation?
 {$IFDEF TL_IGNORE_KANA}
  Result.dic_ignorekana := true;
 {$ELSE}
  Result.dic_ignorekana := false;
 {$ENDIF}
end;

{$IFDEF MTHREAD_SUPPORT}
{ Creates enough translation threads to use available cores and distributes work between those.
 Always leaves the last chunk to the calling thread because the last line of the last chunk
 might be incomplete and it's easier to care about that only in the main thread.
 On exit sets y to reflect calling thread's new share of work. }
function TfEditor.CreateTranslationThreads(abfromy, abtoy: integer; var y: integer): TTranslationThreads;
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
    Result[i] := TTranslationThread.Create(Self, y, y+yshare-1);
    Inc(y, yshare);
  end;
end;
{$ENDIF}

procedure TfEditor.AutoTranslate;
var j:integer;
  bg,en:integer;
  y:integer;

  sp: TSMPromptForm;
  startTime: cardinal;
  req: TDicSearchRequest;
  dicsl: TSearchResults;

  donework: integer;
  totalwork: integer;

  block: TTextSelection;

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
    block.tox:=flength(doc.Lines[doc.Lines.Count-1])-1;
    block.toy:=doc.Lines.Count-1;
  end else
    block := BacktrackSelection(Self.TextSelection);
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
    req := Self.SetupSearchRequest;
    req.Prepare;

    totalwork := block.toy-block.fromy+1;
    donework := 0;

    y := block.fromy;
    while y<=block.toy do
    begin
      bg:=0;
      en:=flength(doc.Lines[y])-1;
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

procedure TfEditor.AutoTranslateLine(y: integer; x_bg, x_en: integer;
  req: TDicSearchRequest; dicsl: TSearchResults);
var x: integer;
  a:integer;
  s:string;
  word:PSearchResult;
  wt:TEvalCharType;
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
      if a=0 then a:=1; //move at least one character forward
      inc(x,a);
    end;
end;

procedure TfEditor.SetTranslation();
begin
  if (dragstart.x=rcur.x) and (dragstart.y=rcur.y) then
  begin
    SetWordTrans(rcur.x,rcur.y,[tfScanParticle,tfManuallyChosen],false);
    mustrepaint:=true;
    ShowText(true);
  end else
    AutoTranslate();
end;

procedure TfEditor.ListBox1Enter(Sender: TObject);
begin
  fMenu.aEditorCopy.ShortCut:=CopyShort;
  fMenu.aEditorCopyAs.ShortCut:=CopyAsShort;
  fMenu.aEditorCut.ShortCut:=CutShort;
  fMenu.aEditorPaste.ShortCut:=PasteShort;
  fMenu.aEditorSelectAll.ShortCut:=AllShort;
  DrawCaret(csVisible); //show cursor
end;

procedure TfEditor.ListBox1Exit(Sender: TObject);
begin
  fMenu.aEditorCopy.ShortCut:=0;
  fMenu.aEditorCopyAs.ShortCut:=0;
  fMenu.aEditorCut.ShortCut:=0;
  fMenu.aEditorPaste.ShortCut:=0;
  fMenu.aEditorSelectAll.ShortCut:=0;
  DrawCaret(csHidden); //kill cursor
end;

procedure TfEditor.ListBox1KeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
var oldCur: TCursorPos;
  ukn:boolean;
  tmp: TCursorPos;
begin
  oldCur:=GetCur;
  tmp:=oldCur;
  if (ins.x=-1) or insconfirmed then
  begin
    ukn:=false;
    if key=VK_RIGHT then
    begin
      if rcur.x<flength(doc.Lines[rcur.y]) then
        rcur := SourcePos(rcur.x+1, rcur.y)
      else
      if rcur.y+1<doc.Lines.Count then
        rcur := SourcePos(0, rcur.y+1);
      CursorEnd := false; //even if not changed rcur
    end else
    if key=VK_LEFT then
    begin
      if rcur.x>0 then
        rcur := SourcePos(rcur.x-1, rcur.y)
      else
      if rcur.y>0 then
        rcur := doc.EndOfLine(rcur.y-1);
      CursorEnd := false; //even if not changed rcur
    end else
    if key=VK_UP then CursorJumpToLine(tmp.y-1) else
    if key=VK_DOWN then CursorJumpToLine(tmp.y+1) else
    if key=VK_PRIOR then CursorJumpToLine(tmp.y-ScreenLineCount) else
    if key=VK_NEXT then CursorJumpToLine(tmp.y+ScreenLineCount) else
    if (key=VK_HOME) and (ssCtrl in Shift) then rcur := SourcePos(0, 0) else
    if (key=VK_END) and (ssCtrl in Shift) then rcur := doc.EndOfDocument else
    if key=VK_HOME then begin
      tmp.x:=0;
      SetCur(tmp);
    end else
    if key=VK_END then begin
      tmp.x:=MaxWord; //I don't think more chars will fit on a graphical line
      SetCur(tmp);
    end else
    if key=VK_DELETE then begin
      ResolveInsert(true);
      if (dragstart.x<>rcur.x) or (dragstart.y<>rcur.y) then
        DeleteSelection()
      else
        doc.DeleteCharacter(rcur.x,rcur.y);
      RefreshLines;
    end else
      ukn:=true;
    if not ukn then
    begin
      ClearInsBlock;
      if ssShift in Shift then shiftpressed:=true;
      tmp := GetCur;
      if (oldCur.x<>tmp.x) or (oldCur.y<>tmp.y) then begin
       //We have moved somewhere else, finalize insert
        ResolveInsert(true);
        ShowText(true);
      end;
    end;
  end else
  if key=VK_UP then NextSuggestion(false) else
  if key=VK_DOWN then NextSuggestion(true);
end;

procedure TfEditor.ListBox1KeyPress(Sender: TObject; var Key: Char);
begin
  InsertCharacter(key);
end;

procedure TfEditor.EditorPaintBoxClick(Sender: TObject);
begin
  ListBox1.SetFocus;
end;

procedure TfEditor.EditorPaintBoxMouseDown(Sender: TObject;
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

procedure TfEditor.EditorPaintBoxMouseMove(Sender: TObject;
  Shift: TShiftState; X, Y: Integer);
begin
  //Sometimes we receive "ssLeft + MouseMove" when we double click something
  //in another window and that window closes, leaving us in editor.
  //This results in ugly unexpected text selection.
  if Mouse.Capture=EditorPaintbox.Handle then
    if ssLeft in Shift then begin;
      if not TryReleaseCursorFromInsert() then
        exit; //cannot move cursor!
     //Auto-scroll down or up on drag
      if Y<0 then
        HandleWheel({down=}false)
      else
      if Y>EditorPaintBox.Height then
        HandleWheel({down=}true);
      if linl.Count>0 then begin
        cur:=GetClosestCursorPos(X+lastxsiz div 2,Y);
        if (cur.x=lastmm.x) and (cur.y=lastmm.y) then exit;
        lastmm.x:=cur.x;
        lastmm.y:=cur.y;
      end;
      shiftpressed:=true;
      ShowText(false);
    end;

  fMenu.IntTipMouseMove(EditorPaintBox,x,y,false);
end;

procedure TfEditor.EditorPaintBoxDblClick(Sender: TObject);
begin
  if not fKanjiDetails.Visible then
    fMenu.aKanjiDetailsExecute(nil);
end;

procedure TfEditor.EditorPaintBoxPaint(Sender: TObject; Canvas: TCanvas);
var r: TRect;
begin
  r := PaintBoxClientRect;

 { Create/update a backbuffer.
  We do it this way even though the control is double-buffered by itself to stop
  RenderText from drawing outside the clipping rectangle. }
  if EditorBitmap=nil then begin
    EditorBitmap := TBitmap.Create;
    EditorBitmap.SetSize(r.Right-r.Left,r.Bottom-r.Top);
  end else
  if (EditorBitmap.Width<>r.Right-r.Left) or (EditorBitmap.Height<>r.Bottom-r.Top) then
    EditorBitmap.SetSize(r.Right-r.Left,r.Bottom-r.Top);
  ReflowText(); //because we need View
  if FNormalizeViewPlanned then
    NormalizeView();
  RenderText(EditorBitmap.Canvas,RectWH(0,0,EditorBitmap.Width,EditorBitmap.Height),
    linl,View,printl,lastxsiz,lastycnt,false,false);
  Canvas.Draw(r.Left,r.Top,EditorBitmap);
  oldblock := Selection(-1, -1, -1, -1);
  DrawBlock(EditorPaintbox.Canvas,r);
  FLastCaretPos := CursorPos(-1, -1);
  DrawCaret(csHidden);
end;

{ These are auto-check grouped buttones so they handle Down/Undown automatically }
procedure TfEditor.sbKanjiModeClick(Sender: TObject);
begin
  fMenu.aEditorKanjiMode.Checked:=true;
  fMenu.aEditorKanaMode.Checked:=false;
  fMenu.aEditorASCIIMode.Checked:=false;
end;

procedure TfEditor.sbKanaModeClick(Sender: TObject);
begin
  fMenu.aEditorKanaMode.Checked:=true;
  fMenu.aEditorKanjiMode.Checked:=false;
  fMenu.aEditorASCIIMode.Checked:=false;
end;

procedure TfEditor.sbAsciiModeClick(Sender: TObject);
begin
  fMenu.aEditorASCIIMode.Checked:=true;
  fMenu.aEditorKanaMode.Checked:=false;
  fMenu.aEditorKanjiMode.Checked:=false;
end;

{ These are auto-check allow-all-up buttons so they handle Down/Undown automatically }
procedure TfEditor.sbDisplayReadingClick(Sender: TObject);
begin
 //Keeps fMenu.Actions in sync but doesn't call Execute
  fMenu.aEditorReading.Checked:=sbDisplayReading.Down;
  RepaintText();
end;

procedure TfEditor.sbDisplayMeaningClick(Sender: TObject);
begin
  fMenu.aEditorMeaning.Checked:=sbDisplayMeaning.Down;
  RepaintText();
end;

procedure TfEditor.sbUseTlColorsClick(Sender: TObject);
begin
  fMenu.aEditorColors.Checked:=sbUseTlColors.Down;
  RepaintText();
end;

procedure TfEditor.sbClearTranslationClick(Sender: TObject);
var i,j:integer;
  bg,en:integer;
  block: TTextSelection;
begin
  block := BacktrackSelection(Self.TextSelection);
  for i:=block.fromy to block.toy do
  begin
    bg:=0;
    en:=flength(doc.Lines[i])-1;
    if i=block.fromy then bg:=block.fromx;
    if i=block.toy then if block.tox<en then en:=block.tox;
   { Remember, both beginning and ending can be after the last char,
    so we had to check at least for en }

    for j:=bg to en do
      doctr[i].chars[j].Reset();
  end;
  mustrepaint:=true;
  ShowText(true);
end;

procedure TfEditor.sbAutoTranslateClick(Sender: TObject);
begin
  Self.AutoTranslate();
end;

procedure TfEditor.sbSetTranslationClick(Sender: TObject);
begin
  Self.SetTranslation();
end;

procedure PrintConfigure(userdata:pointer);
begin
  fSettings.pcPages.ActivePage:=fSettings.tsTextTranslator;
  fSettings.ShowModal;
end;

procedure TfEditor.sbPrintClick(Sender: TObject);
begin
  PrintPreview(GetPageNum,DrawPage,PrintConfigure,nil,_l('#00686^eTranslated text'));
end;

procedure TfEditor.sbFileOpenClick(Sender: TObject);
begin
  if not CommitFile then exit;
  if OpenTextDialog.Execute then
    OpenAnyFile(OpenTextDialog.Filename);
end;

procedure TfEditor.sbFileSaveClick(Sender: TObject);
begin
  if docfilename<>'' then
    SaveToFile(docfilename, FDocType, FDocEncoding, SaveAnnotMode)
  else
    SaveAs;
end;

procedure TfEditor.sbFileNewClick(Sender: TObject);
begin
  if not CommitFile then exit;
  ClearEditor;
end;

procedure TfEditor.sbClipCutClick(Sender: TObject);
begin
  if Self.TextSelection.IsEmpty then exit;
  sbClipCopyClick(Sender);
  DeleteSelection;
end;

//Copies selection as text without any Ruby
function TfEditor.CopyAsText: UnicodeString;
var stream: TStream;
begin
  stream := TUnicodeStringStream.Create(@Result);
  CopySelection(TRubyTextFormat.Create(TUTF16LEEncoding,{NoBOM=}true),stream,amNone);
  FreeAndNil(stream);
end;

//Copies selection as a text with Aozora-Ruby
function TfEditor.CopyAsRuby: UnicodeString;
var stream: TStream;
begin
  stream := TUnicodeStringStream.Create(@Result);
  CopySelection(TRubyTextFormat.Create(TUTF16LEEncoding,{NoBOM=}true),stream);
  FreeAndNil(stream);
end;

//Copies selection as HTML, with <!--StartFragment --><!--EndFragment --> marks
function TfEditor.CopyAsHtml: Utf8String;
var stream: TStream;
begin
  stream := TAnsiStringStream.Create(@Result);
  CopySelection(THtmlFormat.Create([hoClipFragment]),stream);
  FreeAndNil(stream);
end;

//Generates CF_HTML clipboard header for a text
function TfEditor.GenerateHtmlClipHeader(const lenHtml: integer;
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
function TfEditor.CopyAsClipHtml: Utf8String;
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
function TfEditor.CopyAsOpenDocumentTextContent: Utf8String;
var stream: TStream;
begin
  stream := TAnsiStringStream.Create(@Result);
  CopySelection(TOpenDocumentContentFormat.Create(),stream);
  FreeAndNil(stream);
end;

function TfEditor.CopyAsOpenDocument: TMemoryStream;
begin
  Result := TMemoryStream.Create;
  Self.CopySelection(TOpenDocumentFormat.Create(),Result);
end;

function TfEditor.CopyAsWakanText: TMemoryStream;
var block: TTextSelection;
begin
  Result := TMemoryStream.Create;
  block := self.TextSelection;
  doc.SaveWakanText(Result,@block);
end;

{ Normal Ctrl-C -- only in a few basic formats.
 For enhanced copy, use Ctrl+Alt+C / CopyAs() }
procedure TfEditor.sbClipCopyClick(Sender: TObject);
var NormalText: UnicodeString;
begin
  if Self.TextSelection.IsEmpty then exit;
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
procedure TfEditor.CopyAs;
var RubyText: UnicodeString;
begin
  if Self.TextSelection.IsEmpty then exit;
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

procedure TfEditor.sbClipPasteClick(Sender: TObject);
begin
  PasteOp;
end;

function TfEditor.GetView: integer;
begin
  Assert(linl.Count>0, 'GetView() without lines flow done');
  if FViewLineCached<0 then
    UpdateViewLine;
  Result := FViewLineCached;
end;

{ Sets ViewPos to a position which represents given View (line) value most closely }
procedure TfEditor.SetView(Value: integer);
begin
  Assert(linl.Count>0, 'SetView() without lines flow done');

  if Value<0 then
    ViewPos := SourcePos(0,0)
  else
  if Value>=linl.Count then
    ViewPos := SourcePos(linl[linl.Count-1].xs, linl[linl.Count-1].ys)
  else
    ViewPos := SourcePos(linl[Value].xs, linl[Value].ys);

 { We normalize view when changing it (e.g. scrolling) }
  NormalizeView;
end;

procedure TfEditor.InvalidateViewLine;
begin
  FViewLineCached := -1;
end;

{ Recalculates cached ViewLine and updates visual controls }
procedure TfEditor.UpdateViewLine;
var i: integer;
begin
  Assert(linl.Count>0, 'UpdateViewLine() without lines flow done');

  FViewLineCached := -1;
  for i := 0 to linl.Count - 1 do
   //Overshot
    if (FViewPos.y<linl[i].ys)
    or (
     //Matching logical line
      (FViewPos.y=linl[i].ys) and (
       //On this graphical line or before it (overshot)
        (FViewPos.x<linl[i].xs+linl[i].len)
       //At the end of the logical line (special case because FViewPos.x == xs+len)
        or (Length(doc.Lines[linl[i].ys])<=linl[i].xs+linl[i].len)
      )
    ) then begin
      FViewLineCached := i;
      break;
    end;

 { This method does not care about View normalization, or we'd have trouble
  even determining View is not normalized.
  Only basic safety applies here }
  if FViewLineCached>linl.Count-1 then FViewLineCached:=linl.Count-1;
  if FViewLineCached<0 then FViewLineCached := 0;

  UpdateScrollbar;
end;

procedure TfEditor.SetViewPos(const Value: TSourcePos);
var oldview: integer;
begin
  oldview := 0;
  if (linl<>nil) and (linl.Count>0) then
    oldview := View; //try to save on repainting when not needed

  FViewPos := NormalizeViewPos(Value); //Basic safety: set anchor only on valid chars.
  InvalidateViewLine;

 { Sometimes we change ViewPos but the resulting View stays the same.
  Let's try to save on repainting. }

 { No lines => no choice but to reflow and repaint everything }
  if ((linl=nil) or (linl.Count<=0))
 { Have lines => calculate new View and check if it changed }
  or (oldview<>View) then
    EditorPaintbox.Invalidate; //this will also trigger GetView => UpdateScrollbar etc.
end;

{ Adjusts ViewPos so that it's placed on a legal point in a document }
function TfEditor.NormalizeViewPos(const APos: TSourcePos): TSourcePos;
begin
  Result := APos;
  if doc=nil then begin
    Result := SourcePos(0, 0);
    exit;
  end;

  if Result.y>doc.Lines.Count-1 then Result.y := doc.Lines.Count-1;
  if Result.y<0 then Result.y := 0;

  if Result.y>=doc.Lines.Count then
    Result.x := 0
  else begin
    if Result.x>flength(doc.Lines[Result.y])-1 then Result.x := flength(doc.Lines[Result.y])-1;
    if Result.x<0 then Result.x := 0;
  end;
end;

{ View anchor is normalized so that its position is valid (<= end of line)
and we have at least one full screen of text.
This happens on SetView() e.g. scroll, and on any changes (additions/deletions). }
function TfEditor.NormalizeView: boolean;
var LCurView: integer;
  LCurViewPos: TSourcePos;
begin
  Assert(linl.Count>0, 'NormalizeView() without lines flow done');
  LCurView := View;

 //Basic rule: at least one line on the screen
  if LCurView>linl.Count-1 then LCurView:=linl.Count-1;
 //Stricter rule: at least one screen of text on the screen
  if LCurView>linl.Count-ScreenLineCount then
    LCurView:=linl.Count-ScreenLineCount; //can make it < 0
  if LCurView<0 then LCurView:=0;

  Result := View<>LCurView;
  if Result then
    View := LCurView //this also forces ViewPos re-assignment
  else begin
   //Just do ViewPos safety checks
    LCurViewPos := NormalizeViewPos(FViewPos);
    if (LCurViewPos.x<>FViewPos.x) or (LCurViewPos.y<>FViewPos.y) then begin
      ViewPos := LCurViewPos;
      Result := View<>LCurView; //if this resulted in view adjustment
    end;
  end;

  FNormalizeViewPlanned := false;
end;

//Changes View so that cursor is visible
function TfEditor.ScrollIntoView: boolean;
var LCurView: integer;
begin
  Assert(linl.Count>0, 'ScrollIntoView() without lines flow done');
  LCurView := View;
  if LCurView>cur.y then if cur.y>0 then LCurView:=cur.y else LCurView:=0;
  if LCurView+printl-1<cur.y then LCurView:=cur.y-printl+1;
  Result := View<>LCurView;
  if Result then
    View:=LCurView;
end;

procedure TfEditor.InvalidateNormalizeView;
begin
  FNormalizeViewPlanned := true;
end;

procedure TfEditor.EditorScrollBarChange(Sender: TObject);
begin
  if not FUpdatingScrollbar then //else we'd update View even when reacting to View changes
    View:=EditorScrollBar.Position;
end;

procedure TfEditor.HandleWheel(down:boolean);
begin
  if down then View := View+1 else View := View-1;
end;

procedure TfEditor.UpdateScrollbar;
begin
  FUpdatingScrollbar := true;
  try
    if linl.Count-ScreenLineCount<=0 then begin
      if EditorScrollBar.Enabled then
        EditorScrollBar.Enabled := false;
      exit;
    end;
    if not EditorScrollBar.Enabled then
      EditorScrollBar.Enabled := true;
    if EditorScrollBar.Min<>0 then
      EditorScrollBar.Min := 0;
    if EditorScrollBar.Max<>linl.Count-ScreenLineCount then
      EditorScrollBar.Max := linl.Count-ScreenLineCount;
    if EditorScrollBar.Position<>View then
      EditorScrollBar.Position:=View;
  finally
    FUpdatingScrollbar := false;
  end;
end;

{ Number of fully visible graphical lines which fit on the screen }
function TfEditor.GetScreenLineCount: integer;
begin
 //For now we use printl, although it can be not set
 //TODO: Calculate this dynamically
  Result := printl;
end;

function TfEditor.IsCursorOnScreen(const APos: TCursorPos): boolean;
begin
  Assert(linl.Count>0, 'IsCursorOnScreen() without lines flow done');
  if (APos.x<0) or (APos.y<0) or (APos.y>=linl.Count) or (APos.x>linl[APos.y].len) then
  begin
    result:=false;
    exit;
  end;
  Result:= (APos.y>=View) and (APos.y<View+printl);
end;

procedure TfEditor.FormMouseWheelUp(Sender: TObject; Shift: TShiftState;
  MousePos: TPoint; var Handled: Boolean);
begin
  HandleWheel(false);
  handled:=true;
end;

procedure TfEditor.FormMouseWheelDown(Sender: TObject;
  Shift: TShiftState; MousePos: TPoint; var Handled: Boolean);
begin
  HandleWheel(true);
  handled:=true;
end;

procedure TfEditor.btnKanjiDetailsClick(Sender: TObject);
begin
  fMenu.aKanjiDetails.Execute;
end;

procedure TfEditor.sbDockDictionaryClick(Sender: TObject);
begin
  fMenu.TabControl1Change(sender);
end;

procedure TfEditor.BlinkCursorTimerTimer(Sender: TObject);
begin
  if Self.Visible then DrawCaret(csBlink);
end;

procedure TfEditor.SelectAll;
begin
  if not TryReleaseCursorFromInsert() then
    exit; //cannot move cursor!
  dragstart := SourcePos(0, 0);
  cur := CursorPos(linl[linl.Count-1].len, linl.Count-1);
  shiftpressed:=true;
  ShowText(true);
end;

{ Shows hint window if configured to }
procedure TfEditor.ShowHint;
var p: TPoint;
  tmp: TCursorPos;
begin
  if not Self.sbKanjiMode.Down then
    HideHint;
  tmp := CursorScreenPos;
  tmp.x := PosToWidth(tmp.x, tmp.y);
  p:=EditorPaintbox.ClientToScreen(Point(0,4));
  p.x:=p.x+tmp.x*Self.lastxsiz;
  p.y:=p.y+(tmp.y+1-Self.View)*Self.lastxsiz*Self.lastycnt;
  fHint.ShowHint(p);
  ListBox1.SetFocus;
end;

procedure TfEditor.HideHint;
begin
  if (fHint<>nil) and fHint.Visible then fHint.Hide;
end;

function TfEditor.PaintBoxClientRect: TRect;
begin
  Result := EditorPaintBox.ClientRect;
  Result.Left := Result.Left + 1 {normal margin} + 1;
  Result.Top := Result.Top + 1 {normal margin} + 1;
  Result.Right := Result.Right - 1 {normal margin} - 18 {ScrollBar} - 1;
  Result.Bottom := Result.Bottom - 1 {normal margin} - 1;
end;

{ Also updates various controls such as ScrollBar, to match current state }
procedure TfEditor.ShowText(dolook:boolean);
var s:string;
  wt: TEvalCharType;
  tmp: TCursorPos;
begin
  if not Visible then exit;
  ReflowText();
  if linl.Count=0 then
  begin
    rcur := SourcePos(-1, -1);
    EditorPaintBox.Invalidate;
    EditorScrollBar.Enabled:=false;
    exit;
  end;

  if FRCur.y<0 then FRCur.y:=0;
  if rcur.y>=doc.Lines.Count then
    rcur:=doc.EndOfDocument;
  if FRCur.x<0 then FRCur.x:=0;

 //Invalid cursorend => fix
  tmp := GetCur;

 //Fix view
  if ScrollIntoView then
    mustrepaint := true;
  { ScrollIntoView calls Invalidate which would trigger repaint anyway,
   but if it says it did, we'll just repaint it right now. }
  if NormalizeView then
    mustrepaint := true;

 //Reset dragstart
  if not shiftpressed then
  begin
    dragstart.x:=rcur.x;
    dragstart.y:=rcur.y;
  end;

  if fWordLookup<>nil then begin
    fWordLookup.btnLookupJtoE.Down:=false;
    fWordLookup.btnLookupEtoJ.Down:=false;
    fWordLookup.btnLookupClip.Down:=false;

    if dolook then
      if fWordLookup.Visible or (insertBuffer<>'') then
        fWordLookup.Look()
      else begin
        s:=GetDocWord(rcur.x,rcur.y,wt,{stopuser=}false);
        if flength(s)>=1 then fKanjiDetails.SetCharDetails(fgetch(s,1));
      end;
  end;

  if mustrepaint then
    EditorPaintbox.Repaint //not just Invalidate() because we want Paint be done now
  else begin
    DrawCaret(csVisible); //this is often called as a result of keypress/click,
     //and user wants to see the effect of their actions
    DrawBlock(EditorPaintBox.Canvas,PaintBoxClientRect);
  end;

  mustrepaint:=false;
  shiftpressed:=false;
  UpdateScrollbar;
  if fWordLookup<>nil then
    with fWordLookup do
      if (StringGrid.RowCount>1) and (StringGrid.Visible) and (ins.x<>-1) then Self.ShowHint else HideHint;
end;

{ Converts startdrag+cursor positions to block selection. }
function TfEditor.GetTextSelection: TTextSelection;
begin
  if (rcur.y<dragstart.y) or ((rcur.y=dragstart.y) and (rcur.x<dragstart.x)) then
  begin
    Result.fromx:=rcur.x;
    Result.fromy:=rcur.y;
    Result.tox:=dragstart.x;
    Result.toy:=dragstart.y;
  end else
  begin
    Result.fromx:=dragstart.x;
    Result.fromy:=dragstart.y;
    Result.tox:=rcur.x;
    Result.toy:=rcur.y;
  end;
end;

{ Expands selection so that it starts with a nearest word. Selection must be valid. }
function TfEditor.BacktrackSelection(const ASelection: TTextSelection): TTextSelection;
begin
  Result := ASelection;
  if Result.fromx<doctr[Result.fromy].charcount then //it can be after the last char in the line
    while (Result.fromx>=0) and (doctr[Result.fromy].chars[Result.fromx].wordstate='<') do dec(Result.fromx);
  while (Result.tox+1<doctr[Result.toy].charcount)
    and (doctr[Result.toy].chars[Result.tox+1].wordstate='<') do inc(Result.tox);
end;

{
We should probably keep graphical lines for the whole document,
and only re-process the parts which change.
But for now we RecalculateGraphicalLines() every full render,
and start with current position, not with the start of the document.
}

procedure TfEditor.InvalidateLines;
begin
  linl.Clear;
  InvalidateViewLine;
  InvalidateCursorPos;
end;

{
RecalculateGraphicalLines()
Does a full rebuild of graphical line schema for a document.
rs: half-char size in pixels (depends on font)
}
procedure TfEditor.RecalculateGraphicalLines(ll: TGraphicalLineList;
  rs: integer; screenw: integer; vert: boolean);
var
  cx, cy: integer;
  px: integer;
  wx, wxl: integer;
begin
  ll.Clear;
  cx:=0;
  cy:=0;
  while cy<doc.Lines.Count do
  begin
    wx:=flength(doc.Lines[cy]);

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


{ Returns simplified reading for this kana/bopomofo character.
Depending on the settings it might be:
  katakana -> hiragana
  any kana -> romaji
  nothing (active character is not kana or doesn't require reading)
Usually the reading returned has to be added to reading drawing queue. }
function GetKanaReading(const gd0,gd1,gd2:FChar): FString;
begin
  if gd1={$IFDEF UNICODE}'ー'{$ELSE}'30FC'{$ENDIF} then begin
    Result:=gd1;
    exit;
  end;

  if not showroma and (EvalChar(gd1)=EC_KATAKANA) then
    Result := ToHiragana(gd1)
  else
  if showroma and (EvalChar(gd1) in [EC_HIRAGANA, EC_KATAKANA]) then begin
    Result := '';
    if IsSokuon(gd1) then
      Result:=''
    else
    if IsSokuon(gd0) then begin
      if IsSmallKanaVowel(gd2) then
        Result:=gd0+gd1+gd2
      else
        Result:=gd0+gd1;
    end else
    if IsSmallKanaVowel(gd2) then
      Result:=gd1+gd2
    else
    if IsSmallKanaVowel(gd1) then
      Result:=''
    else
      Result := gd1; //by default

    if (flength(Result)>1) and (EC_KATAKANA in EvalChars(Result)) then
      Result := ToHiragana(Result);
  end
  else
    Result := '';
end;

{
x, y:
  start point in logical line coordinates (line:first character)
l, t, w, h:
  left, top, width, height of the block to draw in on the canvas
ll:
  graphical line list (all lines for this control)
printl (out):
  total number of lines which fit on the screen
}
procedure TfEditor.RenderText(canvas:TCanvas; r:TRect; ll:TGraphicalLineList;
  view:integer;var printl,xsiz,ycnt:integer;printing,onlylinl:boolean);
var
  x,y:integer;
  colback, coltext:TColor; //standard colors for editor's text and background
  PrintReading,
  ReserveSpaceForReading,
  PrintMeaning,
  LeaveSpaceBetweenLines:boolean;
  Vertical:boolean; //vertical layout (only for printing)
  screenh,screenw:integer; //reversed in Vertical printing
  MeaningLines:integer; //number of "line units" reserved for meaning
  linec:integer; //final number of "line units" in one text line
  rs:integer; //height of one "line unit"
  px,py:integer;

  cl:integer; //current graphical line
  cx,cy,wx:integer; //char no, source line no, graphical line length
  kanaq:FString; //current remainder of "reading" to print above

  color,fcolor:TColor;
  boldness:boolean;
  meaning:string;
  reading,kanji:FString;
  learnstate:integer;
  kanjilearned:boolean;

  cnty,cntx:integer;

    realx,realy,realx2,realy2:integer;
    we:integer;
    rect:TRect;
    i:integer;
    invert:boolean;
    dnam:string;
    dic:TJaletDic;

  wordstate:char; //word state for this position
  worddict:integer; //word dictionary for this position
  aftertouch:boolean; //the position is colored as "aftertouch" (just inserted)
  lastwordstate:char; //some state is carried over to next position in some cases
  lastworddict:integer;
  lastaftertouch:boolean;


  gd0,gd2:FChar;

  inRubyTag: boolean;
  inRubyComment: boolean;
  validChar: boolean; //this iteration we're at valid source position, not just drawing the reading tail

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
    s := doc.Lines[yp];
    Dec(xp); //once, because the current symbol doesn't matter
    while (xp>0) and (fgetch(s, xp)<>tagOpenSymbol) and (fgetch(s, xp)<>tagCloseSymbol) do
      Dec(xp);
    Result := (xp>0) and (fgetch(s, xp)=tagOpenSymbol);
  end;

begin
  doc.AdjustDocument();

 { Cache render settings }
  colback:=Col('Editor_Back');
  coltext:=Col('Editor_Text');
  if printing then begin
    PrintMeaning:=fSettings.cbPrintMeaning.Checked;
    PrintReading:=fSettings.cbPrintReading.Checked;
  end else begin
    PrintMeaning:=sbDisplayMeaning.Down;
    PrintReading:=sbDisplayReading.Down;
  end;
  ReserveSpaceForReading:=fSettings.cbReserveSpaceForReading.Checked;
  LeaveSpaceBetweenLines:=fSettings.cbSpaceBetweenLines.Checked;
  if not TryStrToInt(fSettings.edtMeaningLines.Text, MeaningLines) then
    MeaningLines:=1;
  Vertical:=fSettings.cbVerticalPrint.Checked and printing;
  if Vertical then begin
    screenh:=r.Width;
    screenw:=r.Height;
  end else begin
    screenh:=r.Height;
    screenw:=r.Width;
  end;

 { Calculate number of basic "line units" in a line }
  linec:=2;
  if LeaveSpaceBetweenLines then inc(linec);
  if PrintReading or ReserveSpaceForReading then inc(linec);
  if PrintMeaning then inc(linec,MeaningLines);

 { Establish "line unit" height == half-char width in pixes }
  if not printing then
    rs := FontSize
  else begin
    if not TryStrToInt(fSettings.edtPrintLines.Text, rs) then
      rs:=20;
    rs:=screenh div rs div linec;
  end;

  if ll.Count=0 then //has been invalidated
    RecalculateGraphicalLines(ll, rs, screenw, Vertical);

  printl:=screenh div (rs*linec);
  xsiz:=rs;
  ycnt:=linec;
  if onlylinl then exit;

  if view>ll.Count then exit; //nothing to draw
  x := ll[view].xs;
  y := ll[view].ys;

 //Find a graphical line which covers the starting point
  cl := -1;
  for i:=0 to ll.Count-1 do
    if (ll[i].ys=y) and (ll[i].xs<=x) then cl:=i;
  Assert(cl>=0, 'Cannot find graphical line which covers the starting point');

  cx:=x;
  cy:=y;
  px:=0;
  py:=0;
  kanaq:='';

 { Last character's dictionary and word state
  If next character has '<' as a wordstate, we extend these to it. }
  lastwordstate := '-';
  lastworddict := 0;
  lastaftertouch := false;

 { If we're starting from the middle of a paragraph, go back until we find a suitable wordstate }
  if ll[cl].xs > 0 then begin
    FindLastWordState(cl);
    inRubyTag := IsInRubyTag(cx+1,cy,UH_AORUBY_TAG_OPEN,UH_AORUBY_TAG_CLOSE);
    inRubyComment := IsInRubyTag(cx+1,cy,UH_AORUBY_COMM_OPEN,UH_AORUBY_COMM_CLOSE);
  end else begin
    inRubyTag := false;
    inRubyComment := false;
  end;
  //else we expect first character of a paragraph to not be '<'

 { Fill the background }
  if printing then
    Canvas.Brush.Color:=clWhite
  else
  if fSettings.cbNoEditorColors.Checked then
    Canvas.Brush.Color:=clWindow
  else
    Canvas.Brush.Color:=colBack;
  rect.Left:=r.Left-2;
  rect.Top:=r.Top-2;
  rect.Right:=r.Right+4;
  rect.Bottom:=r.Bottom+4;
  Canvas.FillRect(rect);

  try
    while (py<screenh) and (cl<ll.Count) do
    begin
      cx:=ll[cl].xs;
      cy:=ll[cl].ys;
      wx:=cx+ll[cl].len;

      while (px<screenw) and (
       ((cx<wx) and (cx<flength(doc.Lines[cy])))
       or ((kanaq<>'') and PrintReading)
      ) do
      try
       { Note that we can get here even if cx is outside the legal characters for the string.
        This happens if we have some reading remainder in kanaq. Be careful. }
        validChar := (cx<wx) and (cx<flength(doc.Lines[cy]));

        if validChar then begin
          wordstate:=doctr[cy].chars[cx].wordstate;
          learnstate:=doctr[cy].chars[cx].learnstate;

          GetTextWordInfo(cx,cy,meaning,reading,kanji);
          if cfExplicitRuby in doctr[cy].chars[cx].flags then
            reading := doctr[cy].chars[cx].ruby; //replacing GetTextWordInfo's reading

          kanjilearned:=(FirstUnknownKanjiIndex(kanji)<0);
          worddict:=doctr[cy].chars[cx].dicidx;
          aftertouch:=(cy=ins.y) and (cx>=ins.x) and (cx<ins.x+inslen);
          if wordstate='<'then begin
            worddict:=lastworddict;
            wordstate:=lastwordstate;
           //aftertouch is not carried over for valid chars
          end;
          lastwordstate:=wordstate;
          lastworddict:=worddict;
          lastaftertouch:=aftertouch;
          if (upcase(wordstate)<>'F') and (upcase(wordstate)<>'D') then reading:='';

          if fSettings.cbReadingKatakana.Checked then begin
            if cx>0 then gd0 := doc.GetDoc(cx-1,cy) else gd0 := UH_NOCHAR;
            if cx<flength(doc.Lines[cy]) then gd2 := doc.GetDoc(cx+1,cy) else gd2 := UH_NOCHAR;
            reading := reading + GetKanaReading(gd0,doc.GetDoc(cx,cy),gd2);
          end;

          if fgetchl(doc.Lines[cy], cx+1)=UH_AORUBY_TAG_OPEN then
            inRubyTag := true;
          if fgetchl(doc.Lines[cy], cx+1)=UH_AORUBY_COMM_OPEN then
            inRubyComment := true;
         //will check for closers after we draw current symbol as is (closers are still inside the tag)
        end else begin
          kanji := '';
          reading := '';
          meaning := '';
          worddict := lastworddict;
          wordstate := lastwordstate;
          aftertouch := lastaftertouch;
        end;
       { Do not check on kanji<>'' to decide whether we draw char or not. It's set
        only for dictionary-linked entries.
        Check for individual parts before drawing those, and for validChar before drawing a char }

        if fSettings.cbNoEditorColors.Checked then begin
          color:=clWindow;
          fcolor:=clWindowText;
        end else begin
          color:=colBack;
          fcolor:=colText;
        end;
        if printing then color:=clWhite;
        if not fSettings.cbNoEditorColors.Checked then begin
          if printing and fSettings.cbNoPrintColors.Checked then
            color:=$00FFFFFF
          else
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

        boldness:=(upcase(wordstate)<>wordstate) and fSettings.cbUserBold.Checked;
        if validChar then begin
          if fSettings.cbNoMeaningLearned.Checked and (learnstate>1) and (learnstate<4) then meaning:='';
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
          if fSettings.cbNoReadingLearned.Checked and kanjilearned then reading:='';
        end;

        if printing then begin
          Canvas.Brush.Color:=clWhite;
          Canvas.Font.Color:=clBlack;
        end else
        if fSettings.cbNoEditorColors.Checked then begin
          Canvas.Brush.Color:=clWindow;
          Canvas.Font.Color:=clWindowText;
        end else begin
          Canvas.Brush.Color:=colBack;
          Canvas.Font.Color:=colText;
        end;

        //Print meaning
        if PrintMeaning and (meaning<>'') and validChar then
        begin
          cnty:=py+rs*2; //base char height
          if PrintReading or ReserveSpaceForReading then
            cnty:=cnty+rs; //reading height
          we:=cx+1;
          cntx:=px+rs*2; //total width
          while (we<wx) and (we<cx+6) and (doctr[cy].chars[we].dicidx=0) and (doctr[cy].chars[we].wordstate<>'?') do
          begin
            if IsHalfWidth(we,cy) and not Vertical then inc(cntx,rs) else inc(cntx,rs*2);
            inc(we);
          end;
          if Vertical then
          begin
            realx:=r.Width-cnty-MeaningLines*rs;
            realy:=px;
            realx2:=r.Width-cnty;
            realy2:=cntx;
          end else
          begin
            realx:=px;
            realy:=cnty;
            realx2:=cntx;
            realy2:=cnty+MeaningLines*rs;
          end;
          rect.left:=realx+r.Left+2;
          rect.right:=realx2+r.Left-2;
          rect.top:=realy+r.Top;
          rect.bottom:=realy2+r.Top;
          canvas.Font.Name:=FontEnglish;
          if not fSettings.CheckBox27.Checked then
            canvas.Font.Height:=rs
          else
            canvas.Font.Height:=rs*2;
          canvas.Font.Style:=[];
          DrawText(canvas.Handle,pchar(meaning),length(meaning),rect,DT_WORDBREAK);
         { Box border around meaning -- partial (top line is drawn char-by-char) }
          if fSettings.cbDisplayLines.Checked then
            if Vertical then
            begin
              canvas.MoveTo(realx2+r.Left+1,realy+r.Top);
              canvas.LineTo(realx+r.Left+1,realy+r.Top);
              canvas.LineTo(realx+r.Left+1,realy2+r.Top);
              canvas.LineTo(realx2+r.Left+1,realy2+r.Top);
            end else
            begin
              canvas.MoveTo(realx+r.Left,realy+r.Top-1);
              canvas.LineTo(realx+r.Left,realy2+r.Top-1);
              canvas.LineTo(realx2+r.Left,realy2+r.Top-1);
              canvas.LineTo(realx2+r.Left,realy+r.Top-1);
            end;
        end;

        //Append reading to print later
        if reading<>'' then begin
          if showroma then
            reading:=KanaToRomajiF(reading,curlang)
          else
            reading:=ConvertBopomofo(reading); //pointless in roma
          kanaq:=kanaq+reading;
        end;

        if printing then
          Canvas.Font.Color:=clBlack
        else
        if fSettings.cbNoEditorColors.Checked then
          Canvas.Font.Color:=clWindowText
        else
          Canvas.Font.Color:=colText;

        if not fSettings.cbNoEditorColors.Checked then
        begin
          if fSettings.CheckBox41.Checked and validChar
          and not (EvalChar(doc.GetDoc(cx,cy)) in [EC_IDG_CHAR, EC_HIRAGANA,
            EC_KATAKANA, EC_IDG_PUNCTUATION]) then
            canvas.Font.Color:=Col('Editor_ASCII');
          if wordstate='I'then
            canvas.Font.Color:=Col('Editor_Active')
          else
          begin
            canvas.Font.Color:=fcolor;
            if aftertouch then
              canvas.Font.Color:=Col('Editor_Aftertouch');
            canvas.Brush.Color:=color;
          end;
        end;

       { Print reading: 1 or 2 positions, 1 full-width or 2 half-width reading chars each }
        if PrintReading then begin
          canvas.Font.Style:=[]; //not bold
          cntx:=px;
          for i:=1 to 2 do
           //If this is validChar and it's half-width and we're in horizontal print, there's only one position
            if (kanaq<>'') and ((i=1) or Vertical or not validChar or not IsHalfWidth(cx,cy)) then
            begin
              if Vertical then
              begin
                realx:=r.Width-py-rs-1;
                realy:=cntx;
              end else
              begin
                realx:=cntx;
                realy:=py+1;
              end;
              if showroma then
              begin
                if curlang='c'then
                  DrawUnicode(canvas,realx+r.Left,realy+r.Top-1,rs,fcopy(kanaq,1,2),FontChineseGrid)
                else
                  DrawUnicode(canvas,realx+r.Left,realy+r.Top-1,rs,fcopy(kanaq,1,2),FontJapaneseGrid);
                fdelete(kanaq,1,2);
              end else
              begin
                if curlang='c'then
                  DrawUnicode(canvas,realx+r.Left,realy+r.Top-1,rs,fcopy(kanaq,1,1),FontChineseGrid)
                else
                  DrawUnicode(canvas,realx+r.Left,realy+r.Top-1,rs,fcopy(kanaq,1,1),FontJapaneseGrid);
                fdelete(kanaq,1,1);
              end;
              inc(cntx,rs);
            end;
        end;

       { Draw single char }
        if validChar then begin
          if boldness then canvas.Font.Style:=[fsBold] else canvas.Font.Style:=[];
          if Vertical then
          begin
            realx:=r.Width-py-rs*2;
            if PrintReading or ReserveSpaceForReading then realx:=realx-rs;
            realy:=px;
          end else
          begin
            realx:=px;
            realy:=py;
            if PrintReading or ReserveSpaceForReading then realy:=realy+rs;
          end;
          rect.Left:=realx+r.Left;
          rect.Right:=realx+r.Left+rs*2;
          if (not Vertical) and (IsHalfWidth(cx,cy)) then rect.Right:=realx+r.Left+rs;
          rect.Top:=realy+r.Top;
          rect.Bottom:=realy+r.Top+rs*2;
          canvas.FillRect(rect);
          if curlang='c'then
            DrawUnicode(canvas,realx+r.Left,realy+r.Top,rs*2,RecodeChar(doc.GetDoc(cx,cy)),FontChineseGrid)
          else
            DrawUnicode(canvas,realx+r.Left,realy+r.Top,rs*2,RecodeChar(doc.GetDoc(cx,cy)),FontJapaneseGrid);

         { Box border for meaning => underline.
          This one is drawn char-by-char, so we check for worddict + valid
          (meaning=='' from second char on). }
          if PrintMeaning and (worddict<>0) and fSettings.cbDisplayLines.Checked then
            if Vertical then
            begin
              canvas.MoveTo(realx+r.Left,realy+r.Top);
              canvas.LineTo(realx+r.Left,realy+r.Top+rs*2);
            end else
            begin
              canvas.MoveTo(realx+r.Left,realy+r.Top+rs*2);
              canvas.LineTo(realx+r.Left+rs*2,realy+r.Top+rs*2);
            end;

         //we check for openers before rendering, and for closers here
          if fgetchl(doc.Lines[cy], cx+1)=UH_AORUBY_TAG_CLOSE then
            inRubyTag := false;
          if fgetchl(doc.Lines[cy], cx+1)=UH_AORUBY_COMM_CLOSE then
            inRubyComment := false;
        end;

       //Increment position in any case, full-width by default
        if (not Vertical) and (validChar and IsHalfWidth(cx,cy)) then inc(px,rs) else inc(px,rs*2);
        inc(cx);
      except
        on E: Exception do begin
          E.Message := '('+inttostr(cx)+','+inttostr(cy)+'): '+E.Message;
          raise;
        end;
      end;

     //Next line
      inc(py,rs*linec);
      px:=0;
      if cl<ll.Count then
       //on logical newline (not just word wrap)
        if ll[cl].xs + ll[cl].len >= flength(doc.Lines[cy]) then begin
          kanaq:=''; //reset reading
          inRubyTag := false; //reset ruby tag highlight
          inRubyComment := false;
        end;
      inc(cl);
    end;
  except
    on E: Exception do begin
      E.Message := 'Paint exception: '+E.Message;
      raise;
    end;
  end;
end;

{ Makes sure graphical lines and related variables are up to date.
 Use force=true to force full reflow. }
procedure TfEditor.ReflowText(force:boolean);
begin
  if force then
    InvalidateLines;
  RenderText(EditorPaintBox.Canvas,PaintBoxClientRect,linl,-1,printl,
    lastxsiz,lastycnt,false,true);
 //NOTE: We must always have at least one logical and graphical line after reflow (maybe empty)
end;


procedure TfEditor.ClearInsBlock;
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
procedure TfEditor.CloseInsert;
begin
  if not insconfirmed then begin
    resolvebuffer:=false; //cancel suggestion
    if insertbuffer<>'' then ResolveInsert(true);
  end;
  ClearInsBlock;
end;

{ Called when we are about to do an operation which requires us to not be in insert mode.
 Returns true if the insert mode was aborted, or false if according to user preferences this is impossible. }
function TfEditor.TryReleaseCursorFromInsert: boolean;
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

procedure TfEditor.DisplayInsert(const convins:FString;transins:TCharacterPropArray;leaveinserted:boolean);
var i:integer;
  s: FString;
  lp: PCharacterLineProps;
begin
  if ins.x=-1 then
  begin
    ins:=rcur;
    inslen:=0;
  end;
  s:=doc.Lines[ins.y];
  fdelete(s,ins.x+1,inslen);
  doc.Lines[ins.y]:=s;
  lp:=doctr[ins.y];
  lp.DeleteChars(ins.x,inslen);
  inslen:=flength(convins);
  if transins=nil then begin
    SetLength(transins, flength(convins));
    for i:=1 to flength(convins) do
      transins[i-1].SetChar('I', 9, 0, 1);
  end;
  doc.Lines[ins.y]:=fcopy(doc.Lines[ins.y],1,ins.x)+convins+fcopy(doc.Lines[ins.y],ins.x+1,flength(doc.Lines[ins.y])-ins.x);
  doctr[ins.y].InsertChars(ins.x,transins);
  ReflowText({force=}true);
  rcur := SourcePos(ins.x+inslen, ins.y);
  if not leaveinserted then
    insconfirmed:=true;
end;

procedure TfEditor.ResolveInsert(final:boolean);
var inskana: string;
  s,s3:string;
  i:integer;
  lp: TCharacterPropArray;
begin
  if (ins.x=-1) and final then exit;

  if (buffertype='H') and resolvebuffer then
  begin
    with fWordLookup do
      if StringGrid.Visible then
      begin
        s:=curkanji;
        priorkanji:=curkanji;
        inskana:=GetInsertKana(false);
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
          s:=s+copy(inskana,length(s3)+1,length(inskana)-length(s3));
        DisplayInsert(s,nil,true);
      end else
      if not final then
        DisplayInsert(GetInsertKana(true),nil,true);
    if final then begin
      inskana := GetInsertKana(false);
      i:=SetWordTrans(ins.x,ins.y,[tfManuallyChosen],false);
     { Not all word may be covered, so we reset prop for other chars.
      In older Wakans the rest was colored as match as well. I'm not against it,
      but either way it needs to happen here, not in SetWordTrans }
      while i<Length(inskana) do begin
        Inc(i);
        doctr[ins.y].chars[ins.x+i-1].Reset;
      end;
      insconfirmed:=true;
      mustrepaint:=true;
      ShowText(false);
    end;
  end else

  if final then
  begin
   { We're accepting input as kana, so we have no dictionary word to check against.
    Therefore if the user didn't enter tones we don't know tones. In F03* notation
    we add F030 meaning "try all tones", but this can't be printed and ConvertBopomofo
    just drops these. }
    s:=ConvertBopomofo(GetInsertKana(false));
    SetLength(lp, flength(s));
    for i:=0 to flength(s)-1 do
      if i=0 then
        lp[i].SetChar(buffertype, 9, 0, 1)
      else
        lp[i].SetChar('<', 9, 0, 1); //word continues
    DisplayInsert(s,lp,true);
    if resolvebuffer then begin
      i:=SetWordTrans(ins.x,ins.y,[tfManuallyChosen],false);
     { Not all word may be covered, so we reset prop for other chars. See above. }
      while i<Length(inskana) do begin
        Inc(i);
        doctr[ins.y].chars[ins.x+i-1].Reset;
      end;
    end;
    insconfirmed:=true;
    mustrepaint:=true;
    ShowText(false);
  end else
    DisplayInsert(GetInsertKana(true),nil,true);
end;

{
Returns the contents of input buffer upgraded for presentation according
to buffer type (to hiragana/katakana, to FW-latin etc.)
  APreview: return contents for preview, not insertion. In chinese we don't convert
    input to bopomofo until the last moment, so this'll return raw romaji.
When returning chinese, tones are in F03* format (this is used for DB lookups)
}
function TfEditor.GetInsertKana(const APreview: boolean):FString;
begin
  if curlang='j'then
  begin
    if buffertype='H' then
      Result:=RomajiToKana('H'+lowercase(insertbuffer),curlang,[])
    else
    if buffertype='K'then
      Result:=RomajiToKana('K'+lowercase(insertbuffer),curlang,[])
    else
      Result:=fstr(insertbuffer); //latin
  end else
  begin
    if APreview then
      Result:=fstr(insertbuffer)
    else
    if buffertype='H' then
      Result:=RomajiToKana(lowercase(insertbuffer),curlang,[])
    else
      Result:=fstr(insertbuffer);
  end;
end;

{ If the suggestion box is open, moves to the next/previous suggestions.
ANext: whether to move to the next (true) or previous (false) suggestion.
Returns false if there was no next/previous suggestion to move to. }
function TfEditor.NextSuggestion(const ANext: boolean): boolean;
begin
  if fWordLookup.IsEmpty then begin
   //When no results, fWordLookup.StringGrid.RowCount might be 200, so cut that
   //case out now.
    Result := false;
    exit;
  end;

  with fWordLookup do begin
    if ANext then begin
      Result := StringGrid.Row<StringGrid.RowCount-1;
      if Result then StringGrid.Row:=StringGrid.Row+1;
    end else begin
      Result := StringGrid.Row>1;
      if Result then StringGrid.Row:=StringGrid.Row-1;
    end;
    if insconfirmed then ResolveInsert(true);
    if (StringGrid.RowCount>1) and StringGrid.Visible and (ins.x<>-1) then Self.ShowHint else HideHint;
  end;
end;

procedure TfEditor.InsertCharacter(c:char);
const DEFCPROPS: TCharacterProps = (wordstate:'-';learnstate:9;dicidx:0;docdic:1);
var chartype:char; //H=hiragana, K=katakana, -=as is, immediate, 0=keep current
  AsciiMode: boolean;
  ImmediateChar: boolean; //drop any chains and print the char
begin
 { [ ] scroll works the same way UP/DOWN does, but after you've applied one
  suggestion it still lets you change it inline.
  UP/DOWN stops working at that point and turns into normal arrow keys.
  But [] keys are also needed in typing, so we only steal them if the suggestion
  box is visible. }
  if not fWordLookup.IsEmpty then
  case c of
   '[': begin NextSuggestion(false); exit; end;
   ']': begin NextSuggestion(true); exit; end;
  end;

  if insconfirmed then ClearInsBlock;
  AsciiMode:=sbAsciiMode.down;
  if (c=' ') and (insertbuffer<>'') then
  begin
   //Accept suggestion
    resolvebuffer:=sbKanjiMode.down;
    ResolveInsert(true);
    FileChanged:=true;
    if sbKanjiMode.down then exit;
  end;
  if (c=#13) and (insertbuffer<>'') then
  begin
   //Reject suggestion
    resolvebuffer:=false;
    ResolveInsert(true);
    FileChanged:=true;
    if sbKanjiMode.down then exit;
  end;
  if (c=#8) and (insertbuffer<>'') then
  begin
    delete(insertbuffer,length(insertbuffer),1);
    DisplayInsert(GetInsertKana(true),nil,insertbuffer<>'');
    FileChanged:=true;
    mustrepaint:=true;
    ShowText(true);
    exit;
  end;
  if c=#13 then
  begin
    DeleteSelection(); //Updates block and verifies everything
    doc.SplitLine(rcur.x,rcur.y);
    FileChanged:=true;
    rcur := SourcePos(0,rcur.y+1);
    RefreshLines;
    exit;
  end;
  if c=#8 then
  begin
    if (dragstart.x<>rcur.x) or (dragstart.y<>rcur.y) then
      DeleteSelection()
    else
    if (cur.x>0) or (cur.y>0) then
    begin
      if cur.x>0 then
        Cur := CursorPos(Cur.x-1,Cur.y)
      else //cur.y>0
        if rcur.x=0 then
          cur := CursorPos(2550,cur.y-1)
        else
          cur := CursorPos(linl[cur.y-1].len, cur.y-1);
      ShowText(true);
      doc.DeleteCharacter(rcur);
    end;
    FileChanged:=true;
    RefreshLines;
    exit;
  end;

  if Ord(c)<$0020 then //not a printable char
    exit;

 { We accept characters and store them mostly as-is in inputbuffer. We also keep
  track of what kind of word we're typing (hiragana H/katakana K/other -).
  Elsewhere we take current contents of input buffer and convert according to
  its type (to hiragana/to katakana/leave as is).
  There are also immediate characters (punctuation and the like) which flush
  input buffer and are printed instantly. }

  if AsciiMode then begin
   { In AsciiMode all characters are immediate + no conversion }
    chartype:='-';

  end else begin
   {  At this time all punctuation is split into breaking and non-breaking.
      Breaking punctuation is immediate, and because of that cannot be used in
      kana formulas.
      Non-breaking punctuation can be used in formulas, but it's non-breaking.

      Digits are non-breaking, at the very least because they are needed for
      pinyin tones. Some other stuff which can be used in a dictionary entry is
      non-breaking too.

      Ideally, we'd like a selected set of characters to be breaking irrelevant
      to if they're used in kana or not, BUT if they're used in kana, we'd like
      to override it and make them non-breaking.
      E.g.
        if AsciiMode then
          breaking := true
        else
          breaking := IsBreakingPunctuation(char);
        if kanaconv.IsMeaningfulCharacter(char) then
          breaking := false;
  }
    case c of
      ' ', ',', '.', '<', '>', '(', ')', '[', ']', '{', '}':
        ImmediateChar := true;
    else ImmediateChar := false;
    end;

   //Uppercase letter in Japanese mode => katakana
    if (AnsiUppercase(c)=c) and ((c<'0') or (c>'9')) then begin
      if curlang='c' then chartype:='-' else chartype:='K'
    end
    else chartype:='H'; //hiragana

    if c='''' then chartype:='0'; //WTF? "'" continues any chain
    if c='+' then chartype:='H'; //WTF? "+" continues hiragana chain...

    if ImmediateChar then chartype:='-'; //overrides previous lines

   { Make permanent replacements. Ideally we'd like to avoid this and replace
    on presentation in GetInsertKana, so that we keep raw input as long as
    possible.
    Chars which we replace here we basically make unavailable for kana formulas
    as there's no way to type them now. }
    case c of
      ',': c:=#$3001;
      '.': c:=#$3002;
     //Special uses for standard chars. How do we type <>()[]{} then?
      '<': c:=#$3008;
      '>': c:=#$3009;
      '(': c:=#$300C;
      ')': c:=#$300D;
      '[': c:=#$3016;
      ']': c:=#$3017;
      '{': c:=#$3010;
      '}': c:=#$3011;
     //This should later be moved to common HW->FW upgrade:
      '~': c:= '～';
    end;

  {
    We'd like to use this code to convert all latin to fullwidth, but we shouldn't:

     //Upgrade all punctuation to fullwidth, but do not touch latin (need it for romaji).
     //Two char blocks are equal: 0021..007E <-> FF01..FF5E
     //TODO: In fact, what if romaji decoder uses any of punctuation?
     //#$3000; //upgrade space
      if (Ord(c)>$0020) and (Ord(c)<$0080)
      and ((Ord(c)<Ord('a')) or (Ord(c)>Ord('z')))
      and ((Ord(c)<Ord('A')) or (Ord(c)>Ord('Z'))) then
        immchar := Chr(Ord(c)-$0020+$FF00);

    Fullwidth latin here will break kana conversion later. We should accept raw
    input and:
      - either attach it to inputbuffer and convert on presentation, according
       to input buffer type
      - or if this is an immediate char, convert later in this function.
  }
  end;


 //Instant output
  if chartype='-' then begin
    resolvebuffer:=false;
    if insertbuffer<>'' then ResolveInsert(true);
    ClearInsBlock;
    DisplayInsert(fstring(c),CharPropArray(DEFCPROPS),false);
    FileChanged:=true;
    mustrepaint:=true;
    ShowText(true);
    exit;
  end;

 //Input buffer
  FileChanged:=true;
  if insertbuffer='' then
  begin
    if chartype='0' then buffertype:='-' else buffertype:=chartype;
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


procedure TfEditor.RefreshLines;
begin
  if linl=nil then exit; //still creating
  InvalidateLines;
  mustrepaint:=true;
  ShowText(true);
end;

{ Set word translation to whatever is selected in Dictionary Search results grid,
 or to the first result if gridfirst==true }
function TfEditor.SetWordTrans(x,y:integer;flags:TSetWordTransFlags;gridfirst:boolean):integer;
var i: integer;
  word: PSearchResult;
begin
  with fWordLookup do
  begin
    if gridfirst then
      i:=0
    else
      if not StringGrid.Visible then
        i:=-1
      else
        i:=StringGrid.Row-1;
    if Results.Count=0 then i:=-1;
    if i<0 then
      word := nil
    else
      word := Results[i];
  end;
  Result := SetWordTrans(x,y,flags,word);
end;

{
Attaches a deflexion/search result to the position in the text. Colors the
inflected word. Recognizes and colors particles after the word.
Returns the number of characters covered (inflected word length + particles).
x,y: Starting point.
word: Translation guess (dictionary, article#, inflexion details).
}
function TfEditor.SetWordTrans(x,y:integer;flags:TSetWordTransFlags;const word:PSearchResult):integer;
var wordpart:char;
    i:integer;
    rlen:integer;
    s,s2:string;
  wt:TEvalCharType;
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
  if (y=-1) or (y>=doc.Lines.Count) or (x=-1) then begin
    Result := 0;
    exit;
  end;
  s2:=doc.GetDoc(x,y);
  dw:=GetDocWord(x,y,wt,{stopuser=}not (tfManuallyChosen in flags));
 { GetDocWord makes upper bound guess on the length of the word,
  then search result gives us exact value.
  It may be shorter (itteoku => only ITTE is parsed) or longer (rarely) }
  if word<>nil then
    rlen:=word.slen
  else
    rlen:=flength(dw);
  globdict:=0;

  if word=nil then
  begin
    wordpart:='-';
    worddict:=0;
    learnstate:=9;
    if wt=EC_IDG_CHAR then rlen:=1;
  end else
  begin
    wordpart:=word.sdef;
    worddict:=word.dicindex;
    s:=word.ToLegacyString;
    globdict:=0;
    if (pos(UH_LBEG+'d',s)>0) then
    begin
      globdict_s:=copy(s,pos(UH_LBEG+'d',s)+2,length(s)-pos(UH_LBEG+'d',s)-1);
      globdict_s:=copy(globdict_s,1,pos(UH_LEND,globdict_s)-1);
      globdict := doc.docdic.IndexOf(globdict_s);
      if globdict<0 then
      begin
        doc.docdic.add(globdict_s);
        globdict:=doc.docdic.Count-1;
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
  end;
 //This subroutine ^^^:
 //local --- s, i, globdict_s, learnstate_s
 //in    --> word, wt
 //out   <-- wordpart, worddict, globdict, learnstate, rlen,

  if wordpart='-' then begin
    if wt<>EC_IDG_CHAR then wordstate:='-' else wordstate:='?'
  end else
    case wt of
      EC_HIRAGANA:if fSettings.cbNoTranslateHiragana.Checked then wordstate:='-'else wordstate:='H';
      EC_KATAKANA:if s2={$IFNDEF UNICODE}'30FC'{$ELSE}#$30FC{$ENDIF} then wordstate:='-' else wordstate:='K';
      EC_IDG_CHAR:if wordpart='I'then wordstate:='D' else wordstate:='F';
    else wordstate:='-';
    end;
  if wordpart='P' then wordstate:='P';
  if tfManuallyChosen in flags then wordstate:=LoCase(wordstate);

  if wordstate='-' then
    doctr[y].chars[x].SetChar(wordstate, 9, 0, globdict)
  else
    doctr[y].chars[x].SetChar(wordstate, learnstate, worddict, globdict);
  for i:=2 to rlen do
    if (x+i-1)<flength(doc.Lines[y]) then
      doctr[y].chars[x+i-1].SetChar('<', learnstate, 0, globdict);

  Result := rlen;

 //Analyze the tail after the inflected result and maybe color the particles.
  fdelete(dw,1,rlen);
  if (wordstate='K') and (flength(doc.Lines[y])>x+rlen) then
  begin
    dw:=GetDocWord(x+rlen,y,wt,{stopuser=}false);
    if wt<>EC_HIRAGANA then dw:='';
  end;
  if flength(dw)>4 then delete(dw,5,MaxInt); //yes 4 in unicode. Cut overly long particle tails
  for i:=flength(dw) downto 1 do
    if EvalChar(fgetch(dw,i))=EC_IDG_CHAR then fdelete(dw,i,MaxInt); //cut kanji since it clearly belongs to next word
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

{
Paints/hides caret.
AState:
  csHidden: erase caret from the control
  csVisible: show caret on the control
  csBlink: change caret state
}
procedure TfEditor.DrawCaret(AState: TCaretState);
var pbRect: TRect;

  procedure DrawIt(x,y:integer);
  var rect:TRect;
  begin
    rect.top:=pbRect.Top+y*lastxsiz*lastycnt;
    rect.left:=pbRect.Left+FCaretPosCache*lastxsiz;
    rect.bottom:=rect.top+lastxsiz*lastycnt;
    rect.right:=rect.left+2;
    InvertRect(EditorPaintBox.Canvas.Handle,rect);
  end;

var tmp: TCursorPos;
begin
  pbRect:=PaintBoxClientRect;
  if not ListBox1.Focused then AState := csHidden;
  if FCaretPosCache=-1 then
    FCaretPosCache:=PosToWidth(FLastCaretPos.x,FLastCaretPos.y);
  if FCaretVisible and IsCursorOnScreen(FLastCaretPos) then
    DrawIt(FLastCaretPos.x,FLastCaretPos.y-View); //invert=>erase
  tmp := CursorScreenPos;
  if (FCaretPosCache=-1) or (FLastCaretPos.x<>tmp.x) or (FLastCaretPos.y<>tmp.y) then
    FCaretPosCache:=PosToWidth(tmp.x, tmp.y);
  case AState of
    csBlink: FCaretVisible:=not FCaretVisible;
    csVisible: FCaretVisible:=true;
  else
    FCaretVisible:=false;
  end;
  if FCaretVisible and IsCursorOnScreen(tmp) then
    DrawIt(tmp.x,tmp.y-View); //draw new

  FLastCaretPos := tmp;
end;

{
Updates text selection. A bit suboptimal, with two InSelection checks for every char.
This function can be used without buffering, so try to only draw where it's really needed.
Canvas:
  A canvas to draw on. Either edit control (when updating) or backbuffer.
}
procedure TfEditor.DrawBlock(Canvas: TCanvas; ClientRect: TRect);
var rect:TRect;
  i,js:integer;
  hw: boolean;

  block: TTextSelection;

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
    rect.top:=ClientRect.Top+(i-View)*lastxsiz*lastycnt;
    rect.left:=ClientRect.Left+js*lastxsiz;
    rect.bottom:=rect.top+lastxsiz*lastycnt;
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
  block := Self.TextSelection;

 {
  i: graphical line index
  j: graphical character index
 }
 //For every visible graphical line
  for i:=View to min(linl.Count-1, View+printl) do begin
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

procedure TfEditor.RepaintText;
begin
  mustrepaint:=true;
  ShowText(true);
end;

procedure TfEditor.CopySelection(format:TTextSaveFormat;stream:TStream;
  AnnotMode: TTextAnnotMode);
var block: TTextSelection;
begin
  block := Self.TextSelection;
  doc.SaveText(AnnotMode,format,stream,@block);
end;

procedure TfEditor.PasteText(const chars: FString; const props: TCharacterLineProps;
  AnnotMode: TTextAnnotMode);
var tmp: TSourcePos;
begin
  if Length(chars)<=0 then exit; //do not mark the document dirty

  if AnnotMode=amDefault then
    if fSettings.cbLoadAozoraRuby.Checked then
      AnnotMode := amRuby
    else
      AnnotMode := amNone;

  doc.PasteText(rcur, chars, props, AnnotMode, @tmp);
  rcur := tmp; //end of inserted text
  RefreshLines;
  FileChanged:=true;
end;

procedure TfEditor.PasteOp;
var ms: TMemoryStream;
  props: TCharacterLineProps;
  Loaded: boolean;
  pasteEndPos: TSourcePos;
  tmpDoc: TWakanText;
begin
  if not TryReleaseCursorFromInsert() then
    exit; //cannot do!

  DeleteSelection; //selected text is replaced

  Loaded := false;
  if fMenu.GetClipboard(CF_WAKAN, ms) then
  try
    ms.Seek(0,soFromBeginning);
    tmpDoc := nil;
    try
      try
        tmpDoc := TWakanText.Create;
        Loaded := tmpDoc.LoadWakanText(ms,{silent=}true);
        doc.PasteDoc(rcur, tmpDoc, @pasteEndPos);
      except
        on E: EBadWakanTextFormat do
          Loaded := false;
      end;
    finally
      FreeAndNil(tmpDoc);
    end;
    rcur := pasteEndPos; //end of inserted text
    RefreshLines;
    if Loaded then
      FileChanged:=true;
  finally
    FreeAndNil(ms);
  end;

  if not Loaded then begin //default to bare text
    props.Clear;
    PasteText(clip,props,amDefault); //does RefreshLines/FileChanged internally
  end;

  ShowText(true);
end;

procedure TfEditor.DeleteSelection;
var block: TTextSelection;
begin
  block := Self.TextSelection;
  doc.DeleteBlock(block);
  rcur := SourcePos(block.fromx, block.fromy);
  RefreshLines;
  FileChanged:=true;
end;

{ Returns kanji, reading and meaning for a text at a specified logical position.
 Reading may contain unconverted F03*-tones (in chinese parts of text),
 but may contain bopomofo tones too (in hand-made annotations). }
procedure TfEditor.GetTextWordInfo(cx,cy:integer;var meaning:string;var reading,kanji:string);
var dnam:string;
    dic:TJaletDic;
    i:integer;
  markers: TMarkers;
  CCharProp: TCharPropertyCursor;
begin
  meaning:='';
  reading:='';
  kanji:='';
  if doctr[cy].chars[cx].dicidx<>0 then
  begin
    i := doctr[cy].chars[cx].docdic;
    dnam := doc.docdic[i];
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
    if TChar.Locate('Unicode',doc.GetDoc(cx,cy)) then
    begin
      CCharProp := TCharPropertyCursor.Create(TCharProp);
      try
        if curlang='j' then
          meaning := CCharProp.GetJapaneseDefinitions(TChar.TrueInt(TCharIndex))
        else
        if curlang='c' then
          meaning := CCharProp.GetChineseDefinitions(TChar.TrueInt(TCharIndex))
        else
          meaning := '';
      finally
        FreeAndNil(CCharProp);
      end;
    end;
  end;
  while (reading<>'') and (kanji<>'') and (fgetch(reading,flength(reading))=fgetch(kanji,flength(kanji))) do
  begin
    fdelete(reading,flength(reading),1);
    fdelete(kanji,flength(kanji),1);
  end;
end;

procedure TfEditor.DocGetDictionaryEntry(Sender: TObject; const APos: TSourcePos;
  out kanji, reading: FString; out meaning: string);
begin
  GetTextWordInfo(APos.x, APos.y, meaning, reading, kanji);
  reading := ConvertBopomofo(reading); //dictionary stores it with F03*-style tones
end;

function TfEditor.Get_doctr(Index: integer): PCharacterLineProps;
begin
  Result := doc.PropertyLines[Index];
end;

{ What the hell is "stopuser"? }
function TfEditor.GetDocWord(x,y:integer;var wordtype:TEvalCharType;stopuser:boolean):string;
var wt2:TEvalCharType;
    i:integer;
    nmk:boolean;
    tc:string;
    honor:boolean;
    stray:integer;
begin
  if (y=-1) or (y>doc.Lines.Count-1) or (x>flength(doc.Lines[y])-1) or (x=-1) then
  begin
    wordtype:=EC_UNKNOWN;
    result:='';
    exit;
  end;
  if curlang='c'then
  begin
    result:='';
    wordtype:=EC_IDG_CHAR;
    for i:=1 to 4 do
    begin
      result:=result+fgetch(doc.Lines[y],x+1);
      inc(x);
      if x=flength(doc.Lines[y]) then exit;
    end;
    exit;
  end;
  tc:=fgetch(doc.Lines[y],x+1);
  honor:=false;
  if (tc={$IFNDEF UNICODE}'304A'{$ELSE}#$304A{$ENDIF})
  or (tc={$IFNDEF UNICODE}'3054'{$ELSE}#$3054{$ENDIF}) then honor:=true;
  if (honor) and (flength(doc.Lines[y])>=x+2) and (EvalChar(fgetch(doc.Lines[y],x+2)) in [EC_UNKNOWN, EC_IDG_CHAR, EC_HIRAGANA]) then
    wordtype:=EvalChar(fgetch(doc.Lines[y],x+2))
  else
    wordtype:=EvalChar(fgetch(doc.Lines[y],x+1));
  if not (wordtype in [EC_UNKNOWN, EC_IDG_CHAR, EC_HIRAGANA, EC_KATAKANA,
    EC_IDG_PUNCTUATION]) then
    wordtype:=EC_IDG_PUNCTUATION;
  nmk:=false;
  stray:=0;
  result:=fgetch(doc.Lines[y],x+1);
  repeat
    inc(x);
    if stopuser and IsLocaseLatin(doctr[y].chars[x].wordstate) then exit;
    wt2:=EC_UNKNOWN;
    if x<flength(doc.Lines[y]) then
    begin
      wt2:=EvalChar(fgetch(doc.Lines[y],x+1));
      if not (wt2 in [EC_UNKNOWN, EC_IDG_CHAR, EC_HIRAGANA, EC_KATAKANA,
        EC_IDG_PUNCTUATION]) then
        wt2:=EC_IDG_PUNCTUATION;
      if (wordtype=EC_IDG_CHAR) and (wt2=EC_HIRAGANA) then begin
        nmk:=true;
        if stray=0 then stray:=1 else stray:=-1;
      end;
      if (nmk) and (wt2=EC_IDG_CHAR) then
      begin
        if stray=1 then wt2:=EC_IDG_CHAR else wt2:=EC_IDG_PUNCTUATION;
        stray:=-1;
        nmk:=false;
      end;
      if (wt2<>wordtype) and ((wordtype<>EC_IDG_CHAR) or (wt2<>EC_HIRAGANA)) then exit;
    end;
    if wt2=EC_UNKNOWN then exit;
    result:=result+fgetch(doc.Lines[y],x+1);
  until false;
end;

{ Returns the word currently under the caret }
function TfEditor.GetWordAtCaret(out AWordtype: TEvalCharType): string;
var fcur: TSourcePos;
begin
  fcur := RCur;
  AWordtype := EC_UNKNOWN;
  Result := GetDocWord(fcur.x, fcur.y, AWordtype, {stopuser=}false);
end;


{ Font }

procedure TfEditor.SetFontSize(Value: integer);
begin
  if FFontSize=Value then exit;
  FFontSize := Value;
  fMenu.aEditorSmallFont.Checked:=(FFontSize=FontSizeSmall);
  fMenu.aEditorMedFont.Checked:=(FFontSize=FontSizeMedium);
  fMenu.aEditorLargeFont.Checked:=(FFontSize=FontSizeLarge);
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
procedure TfEditor.cbFontSizeGuessItem(Value: string);
var i: integer;
begin
  for i := 0 to cbFontSize.Items.Count - 1 do
    if cbFontSize.Items[i]=Value then begin
      cbFontSize.ItemIndex := i;
      exit;
    end;
end;

procedure TfEditor.cbFontSizeChange(Sender: TObject);
var tmp: integer;
begin
  if TryStrToInt(cbFontSize.Text,tmp) and (tmp>=2) then
    SetFontSize(tmp);
end;

procedure TfEditor.cbFontSizeExit(Sender: TObject);
var tmp: integer;
begin
  if TryStrToInt(cbFontSize.Text,tmp) and (tmp>=2) then begin
    SetFontSize(tmp);
    cbFontSizeGuessItem(cbFontSize.Text);
  end else
    cbFontSize.Text := IntToStr(FontSize);
end;

procedure TfEditor.cbFontSizeKeyPress(Sender: TObject; var Key: Char);
begin
  if Ord(Key)=VK_RETURN then
    ListBox1.SetFocus; //jump to editor
end;


{ Cursor }

procedure TfEditor.InvalidateCursorPos;
begin
  FCursorPosInvalid:=true;
end;

{ Returns current graphical cursor position. }
function TfEditor.GetCur: TCursorPos;
var i:integer;
begin
  if not FCursorPosInvalid then begin
    Result := FCachedCursorPos;
    exit;
  end;

  if linl.Count=0 then ReflowText;
  for i:=0 to linl.Count-1 do
    if (linl[i].ys=rcur.y) and (linl[i].xs<=rcur.x) and (linl[i].xs+linl[i].len>=rcur.x) then
    begin
      if (not CursorEnd) and (linl[i].xs+linl[i].len=rcur.x)
        and (i<linl.Count-1) and (linl[i+1].ys=linl[i].ys) then continue; //exact match goes to the next line
      Result.x:=rcur.x-linl[i].xs;
      Result.y:=i;
      exit;
    end;

 //No line found => return 0, 0
  Result.y := 0;
  Result.x := 0;

  FCursorPosInvalid := false;

 //TODO: Nicety: In complicated cases (no exact matching line found) try to
 // return the closest position (last one with y<=rcur.y and x<=rcur.x or the
 // next one after that, or 0:0).
end;

{ Sets current graphical cursor position }
procedure TfEditor.SetCur(Value: TCursorPos);
var newrcur: TSourcePos;
  NewCursorEnd: boolean;
begin
 //We don't recalculate lines automatically: if they're setting Cur without Lines,
 //they're doing something wrong.
  Assert(linl.Count<>0, 'SetCur() without lines flow done');
  newrcur.y := linl[Value.y].ys;
  if Value.x>=linl[Value.y].len then begin
    Value.x:=linl[Value.y].len; //x can't be > length
    NewCursorEnd:=true;
  end else
    NewCursorEnd:=false;
  newrcur.x := linl[Value.y].xs+Value.x;
 //Some safety + we don't want CursorEnd when we're at the end of *logical* line
  if newrcur.x>=flength(doc.Lines[newrcur.y]) then begin
    newrcur.x:=flength(doc.Lines[newrcur.y]);
    NewCursorEnd:=false;
  end;
  rcur := newrcur;
  CursorEnd := NewCursorEnd;
end;

{ Differs from GetCur in that there are special states
 when the cursor is logically at one place but visually at another (see CursorEnd).
 This function returns actual resulting visual position of the cursor. }
function TfEditor.GetCursorScreenPos: TCursorPos;
begin
  Result := cur;
 //In CursorEnd mode we draw cursor at the end of the previous graphical line
  if cursorend and (Result.x=0) and (Result.y>0) then begin
    Result.y:=Result.y-1;
    Result.x:=linl[Result.y].len;
  end;
end;

{ Moves the cursor to another graphical line while keeping it at the same column.
 Remeber that there are half-width and full-width chars, and if we simply chose
 the same char index on the new line, it'd be at a different column. }
procedure TfEditor.CursorJumpToLine(newy: integer);
var tmp: TCursorPos;
begin
 { It's important that we jump at least how much we can.
  PageDown/Up relies on this: CursorJumpToLine(curLine-OnePage) }
  if newy>linl.Count-1 then newy:=linl.Count-1;
  if newy<0 then newy:=0;

  tmp := GetCur;
  if tmp.y=newy then exit; //no changes
  tmp.x := WidthToPos(PosToWidth(cur.x,cur.y),newy);
  tmp.y := newy;
  if tmp.x<0 then tmp.x := linl[tmp.y].len; //over to the right
  SetCur(tmp);
end;

procedure TfEditor.SetRCur(const Value: TSourcePos);
begin
  FRCur := Value;
  CursorEnd := false;
   //we can't know whether to put graphical cursor before or after the line wrap
   //if you know, update CursorEnd after setting RCur
  InvalidateCursorPos;
end;

function TfEditor.IsHalfWidth(x,y:integer):boolean;
begin
  result:=IsHalfWidthChar(doc.GetDoc(x,y));
end;

{ Wakan has "half-width" and "full-width" symbols, and therefore position
 in the string (POS) and the number of "graphical half-units" (WIDTH) are
 not the same.
 These functions convert between these two things! They are unrelated to
 pixel widths. }

function TfEditor.PosToWidth(x,y:integer):integer;
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

{
y: line no
x: number of half-width positions from the left
Returns: number of characters from the start of the line or -1 if the position is
 outside of existing chars.
}
function TfEditor.WidthToPos(x,y:integer):integer;
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
  i:=0;
  while clen>0 do begin
    if IsHalfWidth(cx,cy) then inc(jx) else inc(jx,2);
    if jx>=x then
    begin
      if jx=x then result:=i+1 else result:=i;
      exit;
    end;
    inc(cx);
    inc(i);
    dec(clen);
  end;
  Result:=-1;
end;

{
y: line no
x: number of half-width positions from the left
Returns: number of characters from the start of the line to the closes valid
  TCursorPos.
  (There's got to be at least one at every line)
}
function TfEditor.HalfUnitsToCursorPos(x,y:integer):integer;
var i,jx,cx,cy,clen:integer;
begin
  Assert((y>=0) and (y<linl.Count));
  if x<0 then x:=0;

  cx:=linl[y].xs;
  cy:=linl[y].ys;
  clen:=linl[y].len;

  jx:=0;
  i:=0;
  while i<x do begin //there can't be more characters than half-width positions
    if clen<=0 then break; //no more text
    if IsHalfWidth(cx,cy) then inc(jx) else inc(jx,2);
    if jx>=x then
    begin
      if jx=x then result:=i+1 else result:=i;
      exit;
    end;
    inc(cx);
    dec(clen);
    Inc(i);
  end;
  Result:=i;
end;

{
Returns closest cursor position to the given screen point.
Makes sure what you get is a legal text point, not some negative or over-the-end value.
NOTE:
- Do not call if linl is cleared (linl.Count=0)
}
function TfEditor.GetClosestCursorPos(x,y:integer): TCursorPos;
begin
  if y<0 then y:=0;
  if x<0 then x:=0;
  Result.x := x div lastxsiz;
  Result.y := y div (lastxsiz*lastycnt)+View;
  if Result.y>=linl.Count then
    Result.y:=linl.Count-1;
  Assert(Result.y>=0); //we must have at least one line
  Result.x := HalfUnitsToCursorPos(Result.x, Result.y);
  Assert(Result.x>=0);
end;

{
Returns exact logical coordinates of a character at a given screen point.
If there's no character at that point, returns -1 as either of the coordinates.
NOTE:
- Do not call if linl is cleared (linl.Count=0)
}
function TfEditor.GetExactLogicalPos(x,y:integer):TSourcePos;
var cx,cy:integer;
begin
  Result.y:=-1;
  Result.x:=-1;
  cx:=x div (lastxsiz);
  cy:=y div (lastxsiz*lastycnt)+view;
  if cy<0 then cy:=0;
  if cy>=linl.Count then exit;
  cx:=WidthToPos(cx,cy);
  if cx<0 then exit;
  Result.y:=linl[cy].ys;
  Result.x:=cx+linl[cy].xs;
  if (Result.y>=doc.Lines.Count) or (Result.x>=flength(doc.Lines[Result.y])) then
  begin
    Result.y:=-1;
    Result.x:=-1;
    exit;
  end;
end;

{ Same, but also returns -1 if linl is cleared }
function TfEditor.TryGetExactLogicalPos(x,y: integer):TSourcePos;
begin
  if linl.Count<=0 then
    Result := SourcePos(0,-1)
  else
    Result := GetExactLogicalPos(x,y);
end;

end.
