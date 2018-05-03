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
  StdCtrls, ExtCtrls, Buttons, ImgList, ComCtrls, ToolWin, Actions, ActnList,
  WakanPaintbox, JWBStrings, JWBDicSearch, JWBWakanText, JWBIO, JWBEditorHint;

//If enabled, support multithreaded translation
{$DEFINE MTHREAD_SUPPORT}

//Ignore kana only words when auto-translating. Faster but worse results.
//{$DEFINE TL_IGNORE_KANA}

{$IFDEF DEBUG}

  //Display a window showing how much time Auto-TL took
  {$DEFINE TLSPEEDREPORT}

{$ENDIF}

type
 { Cursor position in graphical lines }
  TCursorPos = record
    y: integer; //line, 0-based
    x: integer; //char, 0-based: [0..length(line)], length(line)=after last char
    class operator Equal(const a: TCursorPos; const b: TCursorPos): boolean; inline;
    class operator NotEqual(const a: TCursorPos; const b: TCursorPos): boolean; inline;
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
  They refer to "logical lines" from a text document.
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
    BlinkCursorTimer: TTimer;
    OpenTextDialog: TOpenDialog;
    SaveTextDialog: TSaveDialog;
    SaveAsKanaDialog: TSaveDialog;
    ActionIcons: TImageList;
    ToolBar1: TToolBar;
    btnFileNew: TToolButton;
    btnFileOpen: TToolButton;
    btnFileSave: TToolButton;
    ToolButton4: TToolButton;
    btnClipCut: TToolButton;
    btnClipCopy: TToolButton;
    btnClipPaste: TToolButton;
    ToolButton8: TToolButton;
    btnKanjiMode: TToolButton;
    btnKanaMode: TToolButton;
    btnAsciiMode: TToolButton;
    ToolButton12: TToolButton;
    btnDisplayReading: TToolButton;
    btnDisplayMeaning: TToolButton;
    btnUseTlColors: TToolButton;
    ToolButton16: TToolButton;
    btnClearTranslation: TToolButton;
    btnAutoTranslate: TToolButton;
    btnSetTranslation: TToolButton;
    ToolButton20: TToolButton;
    cbFontSize: TComboBox;
    ToolButton21: TToolButton;
    btnPrint: TToolButton;
    ListBox1: TListBox;
    EditorPaintbox: TWakanPaintbox;
    EditorScrollBar: TScrollBar;
    lblControlsHint: TLabel;
    btnKanjiDetails: TSpeedButton;
    sbDockDictionary: TSpeedButton;
    ToolButton1: TToolButton;
    btnFullwidth: TToolButton;
    Actions: TActionList;
    aNew: TAction;
    aOpen: TAction;
    aSave: TAction;
    aSaveAs: TAction;
    aCut: TAction;
    aCopy: TAction;
    aPaste: TAction;
    aSelectAll: TAction;
    aKanjiMode: TAction;
    aKanaMode: TAction;
    aASCIIMode: TAction;
    aDisplayReading: TAction;
    aDisplayMeaning: TAction;
    aTranslationClear: TAction;
    aTranslationFill: TAction;
    aTranslationSet: TAction;
    aPrint: TAction;
    aSmallFont: TAction;
    aLargeFont: TAction;
    aMedFont: TAction;
    aUseColors: TAction;
    aExport: TAction;
    aCopyAs: TAction;
    aFullwidthLatin: TAction;
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
    procedure btnKanjiDetailsClick(Sender: TObject);
    procedure sbDockDictionaryClick(Sender: TObject);
    procedure BlinkCursorTimerTimer(Sender: TObject);
    procedure cbFontSizeChange(Sender: TObject);
    procedure cbFontSizeExit(Sender: TObject);
    procedure cbFontSizeKeyPress(Sender: TObject; var Key: Char);
    procedure aNewExecute(Sender: TObject);
    procedure aOpenExecute(Sender: TObject);
    procedure aSaveExecute(Sender: TObject);
    procedure aSaveAsExecute(Sender: TObject);
    procedure aExportExecute(Sender: TObject);
    procedure aCutExecute(Sender: TObject);
    procedure aCopyExecute(Sender: TObject);
    procedure aCopyAsExecute(Sender: TObject);
    procedure aPasteExecute(Sender: TObject);
    procedure aSelectAllExecute(Sender: TObject);
    procedure aKanjiModeExecute(Sender: TObject);
    procedure aKanaModeExecute(Sender: TObject);
    procedure aASCIIModeExecute(Sender: TObject);
    procedure aDisplayReadingExecute(Sender: TObject);
    procedure aDisplayMeaningExecute(Sender: TObject);
    procedure aUseColorsExecute(Sender: TObject);
    procedure aFullwidthLatinExecute(Sender: TObject);
    procedure aTranslationClearExecute(Sender: TObject);
    procedure aTranslationFillExecute(Sender: TObject);
    procedure aTranslationSetExecute(Sender: TObject);
    procedure aPrintExecute(Sender: TObject);
    procedure aSmallFontExecute(Sender: TObject);
    procedure aMedFontExecute(Sender: TObject);
    procedure aLargeFontExecute(Sender: TObject);
    procedure ListBox1KeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure EditorPaintboxMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);

  public
    procedure LanguageChanging;
    procedure LanguageChanged;
    procedure SettingsChanged;

  {
  Currently opened document's logical content.
  Organized into lines which are independent of the window width.
  }
  protected
    function Get_doctr(Index: integer): PCharacterLineProps;
    procedure GetTextWordInfo(cx,cy:integer;var meaning:string;var reading,kanji:FString);
    function SetWordTrans(x,y:integer;flags:TSetWordTransFlags;const word:PSearchResult):integer;
    procedure DocGetDictionaryEntry(Sender: TObject; const APos: TSourcePos;
      out kanji, reading: FString; out meaning: string);
  public
    doc: TWakanText;
    property doctr[Index: integer]: PCharacterLineProps read Get_doctr;
    function GetDocWord(x,y:integer;var wordtype: TEvalCharType):string;
    function GetWordAtCaret(out AWordtype: TEvalCharType): string;
    procedure InvalidateText;

  protected //Copy and paste operations
    procedure CopySelection(format:TTextSaveFormat;stream:TStream;
      AnnotMode:TTextAnnotMode=amRuby);
    procedure DeleteSelection;
    procedure PasteOp;
    procedure PasteText(const chars: FString; const props: TCharacterLineProps;
      AnnotMode: TTextAnnotMode);

  protected //Font and font size
    FFontSize: integer;
    function GetFontName: string;
    procedure SetFontSize(Value: integer);
    procedure cbFontSizeGuessItem(Value: string);
  public
    property FontName: string read GetFontName;
    property FontSize: integer read FFontSize write SetFontSize;

  //Common latin characters display mode (half- or full width).
  //Fullwidth latin is always fullwidth.
  protected
    FEnableHalfWidth: boolean;
    procedure SetEnableHalfWidth(const Value: boolean);
    function IsHalfWidth(x, y: integer): boolean;
  public
    procedure RetestHalfwidthSupport;
    property EnableHalfWidth: boolean read FEnableHalfWidth write SetEnableHalfWidth;

  //Position calculation
  protected
    function PosToWidth(x,y:integer): integer;
    function WidthToPos(x,y:integer): integer;
    function HalfUnitsToCursorPos(x,y: integer): integer;
  public
    function GetClosestCursorPos(x,y:integer): TCursorPos;
    function GetExactLogicalPos(x,y:integer): TSourcePos;
    function TryGetExactLogicalPos(x,y: integer): TSourcePos;

  {
  Core repainting.
  We paint in EditorPaintbox.Paint => its Invalidate and Repaint work.
  We RenderText() onto EditorBitmap, post that to EditorPaintbox.Canvas
  then redraw SelectionBlock and Caret.
  }
  protected
    EditorBitmap: TBitmap;
    //These layout params are recalculated in RenderText:
    lastxsiz: integer; //size of one half-char in pixels, at the time of last render
    lastycnt:integer;  //height of one graphical line, in half-chars (depends on whether readings and tls are enabled)
    printl:integer; //number of lines which fit on the screen last time
    function PaintBoxClientRect: TRect;
    procedure RenderText(canvas:TCanvas;r:TRect;ll:TGraphicalLineList;
      view:integer; var printl,xsiz,ycnt:integer;printing,onlylinl:boolean);

  //Graphical lines
  protected
    linl: TGraphicalLineList; //lines as they show on screen
    procedure RecalculateGraphicalLines(ll: TGraphicalLineList; rs: integer;
      screenw: integer; vert: boolean);
  public
    procedure InvalidateGraphicalLines;
    procedure CalculateGraphicalLines;

  protected
    mustrepaint:boolean;
  public
    procedure RepaintText;
    procedure ShowText(dolook:boolean);

  protected
    FSourceCur: TSourcePos;  //Cursor position in the source
    FNormalizeSourceCur: boolean;
    FCachedCursorPos: TCursorPos;
    FCursorPosInvalid: boolean;
    CursorEnd: boolean; { Cursor is visually "at the end of the previous line",
      although its logical position is at the start of the next graphical line.
      This is expected in some situations during the editing. }
    FSelectionLock: integer; //Pressing Shift or holding mouse button both increase this
    FSelectionStart: TSourcePos; //Selection block starts just before this char
    FShiftPressed: boolean;
    FLeftMouseDown: boolean;
    function GetSourceCur: TSourcePos; inline;
    procedure SetSourceCur(const Value: TSourcePos);
    procedure InvalidateNormalizeSourceCur;
    procedure NormalizeSourceCur;
    procedure SetSelectionStart(const Value: TSourcePos);
    function GetCur: TCursorPos;
    procedure SetCur(Value: TCursorPos);
    procedure InvalidateCursorPos;
    function GetCursorScreenPos: TCursorPos;
    function InSelectionMode: boolean;
    procedure ResetSelection;
  public
    procedure CursorJumpToLine(newy: integer);
    property SourceCur: TSourcePos read GetSourceCur write SetSourceCur; //cursor position in logical coordinates (cursor is before this char)
    property Cur: TCursorPos read GetCur write SetCur; //cursor position (maybe not drawn yet, may differ from where cursor is drawn -- see CursorScreenX/Y)
    property CursorScreenPos: TCursorPos read GetCursorScreenPos; //visible cursor position -- differs sometimes -- see comments
    property SelectionStart: TSourcePos read FSelectionStart write SetSelectionStart;

  protected //View and scrollbar
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

  protected //Hint
    fEditorHint: TfEditorHint;
  public
    procedure ShowHint;
    procedure HideHint;
    procedure UpdateHintVisibility;

  protected { Provisional insert.
    Recently typed part of the text, colored in a special way, can be altered
    dynamically, e.g. by "use different substitution" keys. }
    ins: TSourcePos; //Insert position
    inslen: integer; //Length of yet unconfirmed text
    function GetProvisionalInsertText: FString;
    procedure SetProvisionalInsert(const AText: FString; AProps: TCharacterPropArray);

{ Wakan supports three input modes:
  1. HW Latin mode: All characters are output instantly as is, no replacements.
  Latin/cyrillic/czech/punctuation/whatever.

  FW latin enabled: If there's a FW version of the character, it's used instead.
  There's a FW version of latin (0x20..0x80) and maybe others.

  2. Kana mode.
  Keystrokes are accumulated in input buffer as we type, converted to
  presentational format (hiragana/katakana/as is) before showing.
  Backspace deletes one keystroke, updates presentation.
  Space commits input buffer.

  BREAKING PUNCTUATION: Some symbols are declared "breaking" and commit the
  buffer instantly, but not if:
  - it still can be continued to form a romaji syllable. E.g. if ' is breaking,
  but there's a romaji formula which says:
    h'ag => hug
  Then the chain isn't broken at h' until we continue with h'o or h'u or h'ax.

  REPLACEMENTS: Any lowercase = hiragana, any uppercase = katakana, other
  characters are replaced with japanese equivalents as we see fit at the time of
  presentation.
  E.g. if there's a rule which says:
    '  => "
  Then as we type h'ag, it should be displayed as
    h"ag
  But still converted to hug per the rule above.

  HIRAGANA/KATAKANA SEPARATION: We remember if we started typing lowercase or
  uppercase and auto-commit if we switch to other case.

  PINYIN: Works similarly, but the provisional presentation for the buffer is
  not bopomofo (kana) but latin. It's replaced with bopomofo when we accept
  the buffer.
  Digits are also mandatory non-breaking with Pinyin as they are used to
  specify tones.

  3. Kanji mode.
  Same as kana mode, only we're constantly presented with possible kanji
  replacements for the kana generated from current input buffer.
  We can choose one with [ ] or Up-Down and accept with SPACE. This will then
  be substituted instead of the typed part, and marked as Provisional Insert.
  Provisional Insert can still be replaced with other substitutions with [ ].
 }

  protected //Input/Insert buffer
  type
    TInputBufferType = (
      ibAsIs,           //latin/other characters as is
      ibHiragana,       //romaji to convert into hiragana
      ibKatakana        //...into katakana
    );
    TInsertionState = (
      isTyping,         //typing in letters
      isConfirmedAsIs,  //input confirmed as is (kana/bopomofo/whatever)
      isConverted       //input converted to focused dictionary entry
    );
    TInsertKanaType = (
      ikPreview,        //processed text as it shows while we're typing it
      ikFinal           //as it is accepted when we finalize it (unless replaced by dict. entry)
    );
  protected
    FInsertionState: TInsertionState;
    FInputBuffer: string; //collects keypresses
    FInputBufferType: TInputBufferType; //type of data in the buffer (ibAsIs is usually not buffered)
    procedure HandleKeystroke(c: char);
    procedure ResolveInsert(AAcceptSuggestion: boolean = true);
    procedure ClearInsBlock;
    procedure CloseInsert;
    function TryReleaseCursorFromInsert: boolean;
    procedure RequeryDictSuggestions;
    function NextSuggestion(const ANext: boolean): boolean;
    function ConvertImmediateChar(const c: char): char;
    function ConvertImmediateChars(const str: FString): FString;
  public
    function GetInsertRomaji: string;
    function GetInsertKana(const AType: TInsertKanaType): FString;

  protected //Clipboard formats
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

  protected //File opening/saving
    FDocFilename: string;
    FDocType: TDocType;
    FDocEncoding: CEncoding; //for text documents
    FFileChanged: boolean;
    LastAutoSave:TDateTime;
    FFullTextTranslated: boolean; //applied full text translation at least once since loading
     //this is needed for saving in Kana mode -- we don't show a reminder if it's obvious the text was translated
    SaveAnnotMode: TTextAnnotMode; //if we have saved the file once, we remember the choice
    procedure SetFileChanged(Value: boolean);
  public
    procedure ClearEditor(const DoRepaint: boolean = true);
    procedure AutoloadLastFile;
    procedure OpenAnyFile(const AFilename: string);
    procedure OpenFile(const AFilename: string; const AType: TDocType;
      const AEncoding: CEncoding);
    procedure SaveToFile(const AFilename: string; const AType: TDocType;
      const AEncoding: CEncoding; AnnotMode: TTextAnnotMode);
    function SaveAs: boolean;
    function CommitFile:boolean;
    function ExportAs: boolean;
    property DocFilename: string read FDocFilename write FDocFilename;
    property DocType: TDocType read FDocType write FDocType;
    property DocEncoding: CEncoding read FDocEncoding write FDocEncoding;
    property FileChanged: boolean read FFileChanged write SetFileChanged;
    property FullTextTranslated: boolean read FFullTextTranslated write FFullTextTranslated;

  protected //Automatic translation
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
uses Types, TextTable, JWBCore, JWBLanguage, JWBKanjiDetails,
  JWBSettings, JWBPrint, StdPrompt, KanaConv, JWBUnit, JWBCategories, JWBDictionaries,
  JWBDic, JWBEdictMarkers, JWBFileType, JWBUserData, JWBCharData, StreamUtils,
  JWBLegacyMarkup, System.Character, JWBMenu, JWBClipboard, JWBWordLookup,
  JWBIntTip, JWBDrawText;

var
  EditorWindowTitle: string = '#00610^eText editor / translator'; //one param: file name

{$R *.DFM}

function CursorPos(x,y: integer): TCursorPos;
begin
  Result.x := x;
  Result.y := y;
end;

class operator TCursorPos.Equal(const a: TCursorPos; const b: TCursorPos): boolean;
begin
  Result := (a.x = b.x) and (a.y = b.y);
end;

class operator TCursorPos.NotEqual(const a: TCursorPos; const b: TCursorPos): boolean;
begin
  Result := not (a = b);
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

procedure TfEditor.FormCreate(Sender: TObject);
begin
  doc := TWakanText.Create;
  doc.OnGetDictionaryEntry := DocGetDictionaryEntry;

  docfilename:='';
  FDocType := dtText;
  FDocEncoding := nil;
  FileChanged := false;
  FullTextTranslated:=false;

  ViewPos := SourcePos(0, 0);
  SourceCur := SourcePos(-1, -1);
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

  FSelectionLock := 0;
  FSelectionStart := SourcePos(-1, -1);
  oldblock := Selection(-1, -1, -1, -1);
  FInsertionState := isTyping;

  FFontSize := 0;
 //We need to update controls when we set FontSize, and if we set ItemIndex here,
 //it'll get overwritten for some buggy VCL reason. So we use FormShow.

  FEnableHalfWidth := true;

  linl := TGraphicalLineList.Create;

  fEditorHint := TfEditorHint.Create(Self);

  CopyShort := aCopy.ShortCut;
  CopyAsShort := aCopyAs.ShortCut;
  CutShort := aCut.ShortCut;
  PasteShort := aPaste.ShortCut;
  AllShort := aSelectAll.ShortCut;
  aCopy.ShortCut:=0;
  aCut.ShortCut:=0;
  aPaste.ShortCut:=0;
  aSelectAll.ShortCut:=0;

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
  FreeAndNil(fEditorHint);
  linl.Free;
  FreeAndNil(doc);
end;

procedure TfEditor.FormShow(Sender: TObject);
begin
  if FFontSize<=0 then FontSize:=FontSizeMedium; //see FormCreate for explanation
  RetestHalfwidthSupport; //initially
  ShowText(true);
  ListBox1.ItemIndex:=0;
  ListBox1.SetFocus;
end;

procedure TfEditor.FormHide(Sender: TObject);
begin
  if (fWordLookup<>nil) and not (csDestroying in ComponentState) then
    fWordLookup.RestoreLookupMode; //which we had overriden with word suggestions
end;

procedure TfEditor.FormResize(Sender: TObject);
begin
  InvalidateGraphicalLines;
 { NormalizeView needs Lines and we don't want to recalculate Lines now --
  the editor might be invisible so why bother. }
  InvalidateNormalizeView;
end;

procedure TfEditor.FormActivate(Sender: TObject);
begin
  ListBox1.SetFocus;
  if (fWordLookup <> nil) and (fWordLookup.LookupMode <> lmEditorInsert) then
    RequeryDictSuggestions();
  UpdateHintVisibility;
end;

procedure TfEditor.FormDeactivate(Sender: TObject);
begin
  if fEditorHint.Visible then HideHint;
end;

procedure TfEditor.LanguageChanging;
begin
 //This has to happen before language changes, because we may need to convert
 //input to kana one final time.
 //Modes such as Pinyin have it in a different form (latin text) until just
 //the last moment.
  CloseInsert;
 //have to RepaintText to hide hint after CloseInsert; will do after change
end;

procedure TfEditor.LanguageChanged;
begin
  RepaintText; //hide hint after CloseInsert + font changed
end;

procedure TfEditor.SettingsChanged;
begin
  //Fonts might have changed, halfwidth support might have changed
  Self.RetestHalfwidthSupport;
  if Self.Visible then
    Self.RepaintText;
end;


{
File opening-saving
}

procedure TfEditor.ClearEditor(const DoRepaint: boolean = true);
begin
  doc.Clear;
  InvalidateGraphicalLines;
  SourceCur := SourcePos(0, 0);
  ViewPos := SourcePos(0, 0);
  Self.Caption := _l(EditorWindowTitle) + ' - ' + _l('#00678^e<UNNAMED>');
  docfilename:='';
  mustrepaint:=true;
  if DoRepaint then
    ShowText(true);
  FFileChanged := false;
  FullTextTranslated := false;
end;

{ Autoloads most recently opened file. Called on initialization. }
procedure TfEditor.AutoloadLastFile;
var ADocFilename: string;
begin
  //Used in conjunction with fSettings which reads the params and stores it here
  if Self.DocFilename = '' then exit;
  try
    //OpenFile() is going to be modifying these and it accepts params as const,
    //so they won't be reference-incremented.
    //We have to make a copy, otherwise it'll rewrite DocFilename killing its
    //own param.
    ADocFilename := Self.DocFilename;
    //ADocType and ADocEncoding are by-value
    fEditor.OpenFile(ADocFilename, fEditor.DocType, fEditor.DocEncoding);
  except
    on E: Exception do begin
     //Re-raise with additional comment
      E := Exception(AcquireExceptionObject); //needed for some exception types
      E.Message := 'Cannot autoload your last-used file: '#13+E.Message;
      raise E;
    end;
  end;
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

procedure TfEditor.OpenFile(const AFilename: string; const AType: TDocType;
  const AEncoding: CEncoding);
var LoadAnnotMode: TTextAnnotMode;
begin
  //By default we set SaveAnnotMode to default, meaning no preference has been chosen
  //auto-loaded rubys will be saved either way
  SaveAnnotMode := amDefault;

  //LoadAnnotMode governs how we treat incoming ruby (loading/pasting)
  if fSettings.cbLoadAozoraRuby.Checked then
    LoadAnnotMode := amRuby
  else
    LoadAnnotMode := amNone;

  Screen.Cursor:=crHourGlass;
  try
    //We need to clear the doc, so do a full clear - we must remain in a stable
    //state after a failed load.
    ClearEditor({DoRepaint=}false);

    ViewPos:=SourcePos(0,0);
    SourceCur:=SourcePos(0,0);
    mustrepaint:=true;
    FileChanged:=false;
    FullTextTranslated:=false;

    case AType of
      dtText: doc.LoadText(AFilename, AEncoding, LoadAnnotMode);
      dtWakanText: doc.LoadWakanText(AFilename);
    end;

    //Set the caption and the internal fields after we've loaded the text -- it could fail
    FDocFilename := AFilename;
    FDocType := AType;
    FDocEncoding := AEncoding;
    Self.Caption:= _l(EditorWindowTitle) + ' - ' + ExtractFilename(AFilename);

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
  FDocFilename:=SaveTextDialog.FileName;
  FDocType := ADocType;
  FDocEncoding := AEncoding;
  Self.Caption := _l(EditorWindowTitle) + ' - ' +ExtractFilename(FDocFilename);
end;

procedure TfEditor.SetFileChanged(Value: boolean);
begin
  FFileChanged:=Value;
  aSave.Enabled:=Value;
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

  if (fSettings.cbEditorAutosave.Checked) and (docfilename<>'') then begin
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
  Result.MatchType := mtBestGuessLeft;
 { If we used mtMatchLeft, queries like "sama" would get results like "samazama"
  which is obviously not what we want. }
  Result.MaxWords := 0; //Always true for auto-translation?
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
  if SelectionStart = SourceCur then
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

  if SelectionStart = SourceCur then
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
      s:=GetDocWord(x,y,wt);
      req.Search(s, dicsl, wt);
      if dicsl.Count>0 then word:=dicsl[0] else word:=nil;
      a:=SetWordTrans(x,y,[tfScanParticle],word);
      if a=0 then a:=1; //move at least one character forward
      inc(x,a);
    end;
end;

procedure TfEditor.SetTranslation();
begin
  if SelectionStart = SourceCur then begin
    SetWordTrans(SourceCur.x,SourceCur.y,[tfScanParticle,tfManuallyChosen],fWordLookup.FocusedResult);
    mustrepaint:=true;
    ShowText(true);
  end else
    AutoTranslate();
end;



procedure TfEditor.ListBox1Enter(Sender: TObject);
begin
  aCopy.ShortCut:=CopyShort;
  aCopyAs.ShortCut:=CopyAsShort;
  aCut.ShortCut:=CutShort;
  aPaste.ShortCut:=PasteShort;
  aSelectAll.ShortCut:=AllShort;
  DrawCaret(csVisible); //show cursor
end;

procedure TfEditor.ListBox1Exit(Sender: TObject);
begin
  aCopy.ShortCut:=0;
  aCopyAs.ShortCut:=0;
  aCut.ShortCut:=0;
  aPaste.ShortCut:=0;
  aSelectAll.ShortCut:=0;
  DrawCaret(csHidden); //kill cursor
end;

procedure TfEditor.ListBox1KeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
var oldCur: TCursorPos;
  IsMoveKey: boolean;
begin
  oldCur := GetCur;

  if (ins.x <> -1) and not (FInsertionState in [isConfirmedAsIs, isConverted]) then begin
    //Keys while in insertion mode

    if key=VK_UP then NextSuggestion(false) else
    if key=VK_DOWN then NextSuggestion(true);

    exit;
  end;

  //Normal mode
  Self.FShiftPressed := (ssShift in Shift); //before we handle the move keys

  IsMoveKey := true;
  if key=VK_RIGHT then
  begin
    //If we're breaking the selection mode, there are special rules as to where to jump
    if not Self.InSelectionMode and (Self.SelectionStart > SourceCur) then
      //Start from the rightmost selected char
      SourceCur := doc.NextPos(Self.SelectionStart)
    else
     //Start from the cursor
      SourceCur := doc.NextPos(SourceCur);
    CursorEnd := false;
  end else
  if key=VK_LEFT then
  begin
    if not Self.InSelectionMode and (Self.SelectionStart < SourceCur) then
      SourceCur := doc.PreviousPos(Self.SelectionStart)
    else
      SourceCur := doc.PreviousPos(SourceCur);
    CursorEnd := false;
  end else
  if key=VK_UP then CursorJumpToLine(oldCur.y-1) else
  if key=VK_DOWN then CursorJumpToLine(oldCur.y+1) else
  if key=VK_PRIOR then CursorJumpToLine(oldCur.y-ScreenLineCount) else
  if key=VK_NEXT then CursorJumpToLine(oldCur.y+ScreenLineCount) else
  if (key=VK_HOME) and (ssCtrl in Shift) then SourceCur := SourcePos(0, 0) else
  if (key=VK_END) and (ssCtrl in Shift) then SourceCur := doc.EndOfDocument else
  if key=VK_HOME then
    SetCur(CursorPos(0, oldCur.y))
  else
  if key=VK_END then
    SetCur(CursorPos(MaxWord, oldCur.y)) //I don't think more chars will fit on a graphical line
  else
  if key=VK_DELETE then begin
    ResolveInsert();
    if SourceCur <> SelectionStart then
      DeleteSelection()
    else
      doc.DeleteCharacter(SourceCur.x, SourceCur.y);
    InvalidateText;
  end else begin
    IsMoveKey := false;
    //Shift also means other things such as katakana input, so ignore it for non-move keys:
    Self.FShiftPressed := false;
  end;

  if IsMoveKey then begin
    ClearInsBlock;
    if oldCur <> GetCur then begin
     //We have moved somewhere else, finalize insert
      ResolveInsert();
      ShowText(true);
    end;
  end;
end;

procedure TfEditor.ListBox1KeyUp(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if key = VK_SHIFT then
    Self.FShiftPressed := false;
end;

procedure TfEditor.ListBox1KeyPress(Sender: TObject; var Key: Char);
begin
  HandleKeystroke(key);

  //Any keystroke without shift/mouse pressed resets block selection
  if not Self.InSelectionMode then
    ResetSelection;
    //AFTER we've had the chance to:
    // - do special VK_LEFT/VK_RIGHT selection exit
    // - do special backspace/del handling
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
  if linl.Count>0 then //else lines are to be recalculated and we can't do much
    cur:=GetClosestCursorPos(X,Y);
  if not Self.InSelectionMode then
    Self.FSelectionStart := Self.SourceCur; //reset selection on click
  if Button = mbLeft then
    Self.FLeftMouseDown := true;
  mustrepaint:=true;
  ShowText(true);
end;

procedure TfEditor.EditorPaintBoxMouseMove(Sender: TObject;
  Shift: TShiftState; X, Y: Integer);
var newCur: TCursorPos;
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
        newCur := GetClosestCursorPos(X+lastxsiz div 2,Y);
        //Do not repaint/invalidate if nothing has changed
        if newCur = Self.Cur then exit;
        Self.Cur := newCur;
      end;
      ShowText(false);
    end;

  IntTip.MouseMove(EditorPaintBox,x,y,false);
end;

procedure TfEditor.EditorPaintboxMouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  if Button = mbLeft then
    Self.FLeftMouseDown := false;
end;

procedure TfEditor.EditorPaintBoxDblClick(Sender: TObject);
begin
  if not fKanjiDetails.Visible then
    fMenu.aKanjiDetailsExecute(nil);
end;

function TfEditor.PaintBoxClientRect: TRect;
begin
  Result := EditorPaintBox.ClientRect;
  Result.Left := Result.Left + 1 {normal margin} + 1;
  Result.Top := Result.Top + 1 {normal margin} + 1;
  Result.Right := Result.Right - 1 {normal margin} - 18 {ScrollBar} - 1;
  Result.Bottom := Result.Bottom - 1 {normal margin} - 1;
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
  CalculateGraphicalLines(); //because we need View
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

procedure TfEditor.aNewExecute(Sender: TObject);
begin
  if not CommitFile then exit;
  ClearEditor;
end;

procedure TfEditor.aOpenExecute(Sender: TObject);
begin
  if not CommitFile then exit;
  if OpenTextDialog.Execute then
    OpenAnyFile(OpenTextDialog.Filename);
end;

procedure TfEditor.aSaveExecute(Sender: TObject);
begin
  if docfilename<>'' then
    SaveToFile(docfilename, FDocType, FDocEncoding, SaveAnnotMode)
  else
    SaveAs;
end;

procedure TfEditor.aSaveAsExecute(Sender: TObject);
begin
  Self.SaveAs;
end;

procedure TfEditor.aExportExecute(Sender: TObject);
begin
  Self.ExportAs;
end;

procedure TfEditor.aCutExecute(Sender: TObject);
begin
  if Self.TextSelection.IsEmpty then exit;
  aCopy.Execute;
  DeleteSelection;
end;

{ Normal Ctrl-C -- only in a few basic formats.
 For enhanced copy, use Ctrl+Alt+C / CopyAs() }
procedure TfEditor.aCopyExecute(Sender: TObject);
var NormalText: UnicodeString;
begin
  if Self.TextSelection.IsEmpty then exit;
  NormalText := CopyAsText;
  Clipboard.ResetClipboard;
  try
    Clipboard.AddToClipboard(CF_WAKAN,CopyAsWakanText(),{OwnsStream=}true);
    Clipboard.AddToClipboard(CF_HTML,CopyAsClipHtml());
    Clipboard.AddToClipboard(CF_UNICODETEXT,NormalText);
  finally
    Clipboard.PublishClipboard;
  end;
end;

procedure TfEditor.aCopyAsExecute(Sender: TObject);
begin
  Self.CopyAs;
end;

procedure TfEditor.aPasteExecute(Sender: TObject);
begin
  if not Self.ListBox1.Focused then exit;
  Self.PasteOp;
end;

procedure TfEditor.aSelectAllExecute(Sender: TObject);
begin
  if not Self.ListBox1.Focused then exit;
  Self.SelectAll;
end;

{ These are auto-check grouped buttones so they handle Down/Undown automatically }
procedure TfEditor.aKanjiModeExecute(Sender: TObject);
begin
  aKanjiMode.Checked:=true;
  aKanaMode.Checked:=false;
  aASCIIMode.Checked:=false;
  ResolveInsert(false); //old buffer invalid
end;

procedure TfEditor.aKanaModeExecute(Sender: TObject);
begin
  aKanaMode.Checked:=true;
  aKanjiMode.Checked:=false;
  aASCIIMode.Checked:=false;
  ResolveInsert(false); //old buffer invalid
end;

procedure TfEditor.aASCIIModeExecute(Sender: TObject);
begin
  aASCIIMode.Checked:=true;
  aKanaMode.Checked:=false;
  aKanjiMode.Checked:=false;
  ResolveInsert(false); //old buffer invalid
end;

{ These are auto-check allow-all-up buttons so they handle Down/Undown automatically }
procedure TfEditor.aDisplayReadingExecute(Sender: TObject);
begin
  RepaintText();
end;

procedure TfEditor.aDisplayMeaningExecute(Sender: TObject);
begin
  RepaintText();
end;

procedure TfEditor.aUseColorsExecute(Sender: TObject);
begin
  RepaintText();
end;

procedure TfEditor.aFullwidthLatinExecute(Sender: TObject);
begin
  RepaintText();
end;

procedure TfEditor.aTranslationClearExecute(Sender: TObject);
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

procedure TfEditor.aTranslationFillExecute(Sender: TObject);
begin
  Self.AutoTranslate();
end;

procedure TfEditor.aTranslationSetExecute(Sender: TObject);
begin
  Self.SetTranslation();
end;


type
  TEditorPainter = class(TPrintPainter)
  protected
    FEditor: TfEditor;
    plinl: TGraphicalLineList; //graphical lines for printing
    printpl: integer;
  public
    constructor Create(AEditor: TfEditor);
    destructor Destroy; override;
    function GetPageNum(Canvas: TCanvas; Width, Height: integer): integer;
      override;
    procedure DrawPage(Canvas: TCanvas; PageNum: integer; Width, Height: integer;
      OrigWidth, OrigHeight: integer); override;
    procedure Configure; override;
  end;

constructor TEditorPainter.Create(AEditor: TfEditor);
begin
  inherited Create();
  plinl := TGraphicalLineList.Create;
  FEditor := AEditor;
end;

destructor TEditorPainter.Destroy;
begin
  plinl.Free;
  inherited;
end;

function TEditorPainter.GetPageNum(canvas:TCanvas; width,height:integer):integer;
var pl,xs,yc:integer;
begin
  plinl.Clear;
  FEditor.RenderText(Canvas,
    RectWH(width div 50,height div 50,width-width div 25,height-height div 25),
    plinl,0,pl,xs,yc,true,true);
  printpl:=pl;
  result:=((plinl.Count-1) div pl)+1;
  if result<1 then result:=1;
end;

procedure TEditorPainter.DrawPage(canvas:TCanvas; pagenum:integer;
  width,height,origwidth,origheight:integer);
var pl,xs,yc:integer;
begin
  if plinl.Count<=(pagenum-1)*printpl then exit;
  FEditor.RenderText(canvas,
    RectWH(width div 50,height div 50,width-width div 25,height-height div 25),
    plinl,(pagenum-1)*printpl,pl,xs,yc,true,false);
end;

procedure TEditorPainter.Configure();
begin
  fSettings.pcPages.ActivePage := fSettings.tsEditorPrinting;
  fSettings.ShowModal;
end;

procedure TfEditor.aPrintExecute(Sender: TObject);
var painter: TEditorPainter;
begin
  painter := TEditorPainter.Create(Self);
  try
    PrintPreview(painter, _l('#00686^eTranslated text'));
  finally
    FreeAndNil(painter);
  end;
end;


procedure TfEditor.aSmallFontExecute(Sender: TObject);
begin
  Self.FontSize := FontSizeSmall;
end;

procedure TfEditor.aMedFontExecute(Sender: TObject);
begin
  Self.FontSize := FontSizeMedium;
end;

procedure TfEditor.aLargeFontExecute(Sender: TObject);
begin
  Self.FontSize := FontSizeLarge;
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

{
Generates CF_HTML clipboard header for a text
startFragment is a 1-based index to the first character of the fragment contents
  in the text (just after the <!--StartFragment--> block).
endFragment is a 1-based index to the first character of the <!--EndFragment-->
  block.
}
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
  Result := Format(headerForm,[lenHeader, lenHeader+lenHtml,lenHeader+startFragment-1,lenHeader+endFragment-1]);
end;

//Copies selection as HTML enhanced with a CF_HTML clipboard header
function TfEditor.CopyAsClipHtml: Utf8String;
var startFragment, endFragment: integer;
begin
  Result := CopyAsHtml;
  startFragment := pos(Utf8String(HtmlStartFragment),Result);
  if startFragment <> 0 then
    startFragment := startFragment + Length(Utf8String(HtmlStartFragment)); //else keep it 0
  endFragment := pos(Utf8String(HtmlEndFragment),Result);
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

{ Enhanced Ctrl-Alt-C -- all ruby + all supported formats.
 In the future perhaps this will pop up a dialog asking to choose a format }
procedure TfEditor.CopyAs;
var RubyText: UnicodeString;
begin
  if Self.TextSelection.IsEmpty then exit;
  RubyText := CopyAsRuby;
  Clipboard.ResetClipboard;
  try
    Clipboard.AddToClipboard(CF_WAKAN,CopyAsWakanText(),{OwnsStream=}true);
   {$IFDEF DEBUG}
   //No point since no one supports this... Even LibreOffice doesn't paste this.
    Clipboard.AddToClipboard(CF_ODT,CopyAsOpenDocument(),{OwnsStream=}true);
   {$ENDIF}
    Clipboard.AddToClipboard(CF_HTML,CopyAsClipHtml());
    Clipboard.AddToClipboard(CF_UNICODETEXT,RubyText);
  finally
    Clipboard.PublishClipboard;
  end;
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
  FSelectionStart := SourcePos(0, 0);
  cur := CursorPos(linl[linl.Count-1].len, linl.Count-1);
  ShowText(true);
end;

{ Shows hint window if configured to }
procedure TfEditor.ShowHint;
var p: TPoint;
  tmp: TCursorPos;
begin
  if not Self.aKanjiMode.Checked then begin
    HideHint;
    exit;
  end;
  tmp := CursorScreenPos;
  tmp.x := PosToWidth(tmp.x, tmp.y);
  p:=EditorPaintbox.ClientToScreen(Point(0,4));
  p.x:=p.x+tmp.x*Self.lastxsiz;
  p.y:=p.y+(tmp.y+1-Self.View)*Self.lastxsiz*Self.lastycnt;
  fEditorHint.ShowHint(p);
end;

procedure TfEditor.HideHint;
begin
  if (fEditorHint<>nil) and fEditorHint.Visible then fEditorHint.Hide;
end;

//Shows or hides the hint window as appropriate to the situation. Does not reset
//the suggestion list or our position in it
procedure TfEditor.UpdateHintVisibility;
begin
  if fWordLookup<>nil then
    if (not fWordLookup.IsEmpty) and (ins.x<>-1) then Self.ShowHint else HideHint;
end;

procedure TfEditor.RepaintText;
begin
  mustrepaint:=true;
  ShowText(true);
end;

{ Also updates various controls such as ScrollBar, to match current state }
procedure TfEditor.ShowText(dolook:boolean);
var s:string;
  wt: TEvalCharType;
begin
  if not Visible then exit;
  CalculateGraphicalLines();
  if linl.Count=0 then
  begin
    SourceCur := SourcePos(-1, -1);
    EditorPaintBox.Invalidate;
    EditorScrollBar.Enabled:=false;
    exit;
  end;

  //Fix cursor position
  GetCur();

 //Fix view
  if ScrollIntoView then
    mustrepaint := true;
  { ScrollIntoView calls Invalidate which would trigger repaint anyway,
   but if it says it did, we'll just repaint it right now. }
  if NormalizeView then
    mustrepaint := true;

  if dolook then
    RequeryDictSuggestions;

  //In any mode except active typing show char under cursor
  if (fKanjiDetails<>nil) and dolook and (FInputBuffer='') then begin
    s:=GetDocWord(SourceCur.x,SourceCur.y,wt);
    if flength(s)>=1 then fKanjiDetails.SetCharDetails(fgetch(s,1));
  end;

  if mustrepaint then
    EditorPaintbox.Repaint //not just Invalidate() because we want Paint be done now
  else begin
    DrawCaret(csVisible); //this is often called as a result of keypress/click,
     //and user wants to see the effect of their actions
    DrawBlock(EditorPaintBox.Canvas,PaintBoxClientRect);
  end;

  mustrepaint:=false;
  UpdateScrollbar;
  UpdateHintVisibility;
end;

{ Converts startdrag+cursor positions to block selection. }
function TfEditor.GetTextSelection: TTextSelection;
begin
  if SourceCur < SelectionStart then
    Result := TTextSelection.CreateFromTo(SourceCur, SelectionStart)
  else
    Result := TTextSelection.CreateFromTo(SelectionStart, SourceCur);
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
We should probably keep graphical lines for the whole document and only
re-process the parts which change.
But for now we RecalculateGraphicalLines() every full render,
and start with current position, not with the start of the document.
}

procedure TfEditor.InvalidateGraphicalLines;
begin
  linl.Clear;
  InvalidateViewLine;
  InvalidateCursorPos;
  EditorPaintbox.Invalidate; //lines are invalidated, the control needs to redraw
end;

{ Makes sure graphical lines and related variables are available. To force
 recalculation call InvalidateGraphicalLines }
procedure TfEditor.CalculateGraphicalLines;
begin
  //Call RenderText with "onlylinl" to only recalculate layout and maybe graphical lines,
  //if they are invalidated
  RenderText(EditorPaintBox.Canvas,PaintBoxClientRect,linl,-1,printl,
    lastxsiz,lastycnt,false,true);
 //NOTE: We must always have at least one logical and graphical line after reflow (maybe empty)
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
Recalculates layout depending on the settings and paintbox size,
rebuilds graphical lines and redraws the visible part of the text.

x, y:
  start point in logical line coordinates (line:first character)
l, t, w, h:
  left, top, width, height of the block to draw in on the canvas
ll:
  graphical line list (all lines for this control)
printing:
  We're in printing mode (some special processing)
onlylinl:
  Only recalculate layout and lines, but do not actually repaint.

Returns the layout details to be stored for secondary painting:

xsiz (out)
ycnt (out)
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
    PrintMeaning:=aDisplayMeaning.Checked;
    PrintReading:=aDisplayReading.Checked;
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
  Canvas.Pen.Style := psClear;
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
          if aUseColors.Checked then begin
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
            if aUseColors.Checked then
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
                DrawUnicode(canvas,realx+r.Left,realy+r.Top-1,rs,fcopy(kanaq,1,2),Self.FontName);
                fdelete(kanaq,1,2);
              end else
              begin
                DrawUnicode(canvas,realx+r.Left,realy+r.Top-1,rs,fcopy(kanaq,1,1),Self.FontName);
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
          DrawCJKChar(canvas, rect, rs*2, RecodeChar(doc.GetDoc(cx,cy)), Self.FontName);

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


procedure TfEditor.ClearInsBlock;
var insText: FString;
  i: integer;
begin
  //Upgrade kanji priorities for previous insert
  if (ins.x>=0) and fSettings.cbAdjustCharPriorities.Checked then begin
    insText := GetProvisionalInsertText();
    for i := 1 to flength(insText) do
      if EvalChar(fgetch(insText, i))=EC_IDG_CHAR then begin
        IncCharPriority(fgetch(insText, i));
        fMenu.ChangeUserData();
      end;
  end;

 //We have to clear suggestions or the suggestion box is going to hang there
 //forever (and [ ] keys put older results for punctuation and spaces)
  fWordLookup.Clear;

 //Reset everything
  ins := SourcePos(-1,-1);
  inslen:=0;
  FInputBuffer := '';
  FInputBufferType := ibAsIs;
  FInsertionState := isTyping;
end;

{ Called when the insert is not finalized, but we really have to end it now.
 Either cancels it or finalizes it if it was already confirmed. }
procedure TfEditor.CloseInsert;
begin
  if (FInsertionState=isTyping) and (FInputBuffer<>'') then
    ResolveInsert({AcceptSuggestion=}false); //cancel suggestion
  ClearInsBlock;
end;

{ Called when we are about to do an operation which requires us to not be in insert mode.
 Returns true if the insert mode was aborted, or false if according to user preferences this is impossible. }
function TfEditor.TryReleaseCursorFromInsert: boolean;
begin
  if FInputBuffer = '' then begin //not in insert mode
    Result := true;
    exit;
  end;

  if FInsertionState in [isConfirmedAsIs, isConverted] then begin //insert confirmed -- can relatively safely close insert
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

{ Returns current provisional insert text, the part of the text which has just
 been inserted and can yet be replaced with substitutions etc. }
function TfEditor.GetProvisionalInsertText: FString;
begin
  if ins.x<0 then exit('');
  Result := fcopy(doc.Lines[ins.y], ins.x, inslen);
end;

{ Replaces current provisional insert in the editor with the specified one,
 updates provisional insert state (position/length).
 Props can be nil for default props }
procedure TfEditor.SetProvisionalInsert(const AText: FString; AProps: TCharacterPropArray);
var i:integer;
  s: FString;
  lp: PCharacterLineProps;
begin
  if ins.x=-1 then begin
    ins:=SourceCur;
    inslen:=0;
  end;

  if AProps=nil then begin
    SetLength(AProps, flength(AText));
    for i:=1 to flength(AText) do
      AProps[i-1].SetChar('I', 9, 0, 1);
  end;

  s := doc.Lines[ins.y];
  doc.Lines[ins.y] := fcopy(s,1,ins.x) + AText
    +fcopy(s,ins.x+inslen+1,flength(s)-ins.x-inslen);

  lp := doctr[ins.y];
  lp.DeleteChars(ins.x,inslen);
  lp.InsertChars(ins.x,AProps);

  inslen:=flength(AText);
  InvalidateGraphicalLines;

  SourceCur := SourcePos(ins.x+inslen, ins.y);
end;

{ Irreversibly convert input keystrokes to kana/text, add it to the document and
clear insert buffer.
AAcceptSuggestion: replace converted kana with focused dictionary suggestion.
Attach that dictionary reference to the word.
After the insert is finalized, it remains marked as "provisional", colored with
editor aftertouch, and can be switched to other suggestion, but if you do
anything else then it's accepted forever.
Can be called several times, updating the provisional insert with the currently
focused suggestion }
procedure TfEditor.ResolveInsert(AAcceptSuggestion: boolean);
var inskana: string;
  expr, kana: string;
  exprRoot, kanaRoot: string;
  i:integer;
  insText: string;
  insProp: TCharacterPropArray;
  tl: PSearchResult;
  curTl: TSearchResult;
begin
  if ins.x=-1 then exit;

  //Can't change confirmation mode afterwards
  case FInsertionState of
    isConfirmedAsIs: AAcceptSuggestion := false;
    isConverted: AAcceptSuggestion := true;
  end;

  inskana := GetInsertKana(ikFinal);
  tl := nil; //We'll need to edit the search result so we'll copy it locally. Tl will point to it, or to nil

  if AAcceptSuggestion and (FInputBufferType = ibHiragana) and not fWordLookup.IsEmpty then begin
   //Replace input text with focused dictionary suggestion
    if fWordLookup.FocusedResult <> nil then begin
      curTl := fWordLookup.FocusedResult^;
      tl := @curTl;
    end else
      curTl.Reset;

    expr := curTl.kanji;
    kana := curTl.kana;

    //Kana-only words might be imported with either empty expr, empty kana or expr=kana
    //Let's handle all these cases well
    //If either of them is empty, assume kanji==kana (otherwise we'll do weird substitutions)
    if expr = '' then
      expr := kana
    else
      if kana = '' then
        kana := expr;

    //Normally, katakana-only words are stored as expr=katakana, read=hiragana
    //and don't need special treatment (e.g. サボる/さぼる).
    //But sometimes both are kata: サボる/サボる, or even something more evil:
    //  仕事をサボる/しごとをサボる
    //To figure out the root we need the reading fully in hiragana:
    kana := ToHiragana(kana);

    //Delete common ending
    exprRoot := expr;
    kanaRoot := kana;
    while (exprRoot<>'') and (kanaRoot<>'') and (fgetch(exprRoot,flength(exprRoot))=fgetch(kanaRoot,flength(kanaRoot))) do
    begin
      fdelete(exprRoot,flength(exprRoot),1);
      fdelete(kanaRoot,flength(kanaRoot),1);
    end;
    //Empty roots mean the whole word is identical in expr and kana forms.

    if (kanaRoot <> '') and not StartsStr(kanaRoot, inskana) then begin
     //In rare weird cases inskana differs from the match kana - e.g. when the match
     //is by exact db-roma while inskana is converted with user-roma or even fails
     //to convert at all.
     //There isn't much we can do here but replace the whole input with the match.
      insText := expr;
      curTL.inflen := flength(instext);
    end else begin
      //Replace the root with it's expr version
      //Default case works for normal expr/read pairs, for kata/hira and for hira/hira alike
      insText := exprRoot+copy(inskana,length(kanaRoot)+1,length(inskana)-length(kanaRoot)); //use kanji/hiragana/whatever + tail
      //Our existing search result is tailored for the request we did in kana
      //If we're replacing kana with kanji, we need to adjust the result we're attaching to it
      curTL.inflen := curTl.inflen + (flength(kanaRoot) - flength(exprRoot));
    end;

    SetProvisionalInsert(insText,nil);
  end else
  begin
   //Replacement disabled => Keep hiragana/bopomofo/whatever
   { We're accepting input as kana, so we have no dictionary word to check against.
    Therefore if the user didn't enter tones we don't know tones. In F03* notation
    we add F030 meaning "try all tones", but this can't be printed and ConvertBopomofo
    just drops these. }
    insText:=ConvertBopomofo(inskana);
    SetLength(insProp, flength(insText));
    for i:=0 to flength(insText)-1 do
      if i=0 then
        case FInputBufferType of
          ibHiragana: insProp[i].SetChar('H', 9, 0, 1);
          ibKatakana: insProp[i].SetChar('K', 9, 0, 1);
        else insProp[i].SetChar('-', 9, 0, 1)
        end
      else
        insProp[i].SetChar('<', 9, 0, 1); //word continues
    SetProvisionalInsert(insText, insProp);
  end;

 //Attach translation, if available, to the replaced text
  if AAcceptSuggestion then begin
    i := SetWordTrans(ins.x,ins.y,[tfManuallyChosen], tl); //tl==nil is okay
   { Not all word may be covered, so we reset prop for other chars.
    In older Wakans the rest was colored as match as well. I'm not against it,
    but either way it needs to happen here, not in SetWordTrans }
    while i<Length(inskana) do begin
      Inc(i);
      doctr[ins.y].chars[ins.x+i-1].Reset;
    end;
  end;

  if AAcceptSuggestion then
    FInsertionState := isConverted
  else
    FInsertionState := isConfirmedAsIs;
  mustrepaint:=true;
  ShowText(false);
end;


{
Returns the contents of the input buffer.
}
function TfEditor.GetInsertRomaji: string;
begin
  Result := FInputBuffer;
end;

{
Returns the contents of input buffer upgraded for presentation according
to buffer type (to hiragana/katakana, to FW-latin etc.)
AType: Whether to return content for preview or for final insertion.
  In chinese we don't convert input to bopomofo until the last moment,
  so Preview returns raw romaji.
When returning chinese, tones are in F03* format (this is used for DB lookups).
}
function TfEditor.GetInsertKana(const AType: TInsertKanaType): FString;
begin
  if curlang='j'then begin
    case FInputBufferType of
      ibHiragana: Result:=RomajiToKana('H'+lowercase(FInputBuffer),curlang,[]);
      ibKatakana: Result:=RomajiToKana('K'+lowercase(FInputBuffer),curlang,[]);
    else Result:=fstr(FInputBuffer); //latin
    end;
  end else
  begin //'c'
    if AType=ikPreview then
      Result:=fstr(FInputBuffer)
    else
    if FInputBufferType in [ibHiragana, ibKatakana] then
      Result:=RomajiToKana(lowercase(FInputBuffer),curlang,[])
    else
      Result:=fstr(FInputBuffer);
  end;
 //Convert characters which weren't taken by kana parser
  Result := ConvertImmediateChars(Result);
end;

{
Switches the attached dictionary to the editor insert mode and asks to repopulate
the list of its typing / translation suggestions.
Called every time:
 * an input text changes,
 * an input text is empty and cursor position changes (for tl suggestions)
 * an attached dictionary has lost our context for some reason.
Resets the currently selected suggestion.
}
procedure TfEditor.RequeryDictSuggestions;
begin
  if fWordLookup = nil then exit;
  if fWordLookup.LookupMode <> lmEditorInsert then
    fWordLookup.LookupMode := lmEditorInsert;
  //Currently the WordLookup itself chooses where to look (InputBuffer or
  //the word under the cursor)
  if fWordLookup.Visible or (FInputBuffer<>'') then
    fWordLookup.Look()
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

    //If we have already posted the substitution then update it inplace
    if FInsertionState in [isConfirmedAsIs, isConverted] then
      ResolveInsert();
    if (StringGrid.RowCount>1) and StringGrid.Visible and (ins.x<>-1) then Self.ShowHint else HideHint;
  end;
end;

//True if the specified character is output instantly, commiting the current buffer.
//May be overriden by romaji rules.
function IsImmediateCharacter(const c: char): boolean;
begin
  case c of
    ' ', ',', '.', '<', '>', '(', ')', '[', ']', '{', '}':
      Result := true;
  else Result := false;
  end;
end;

{ True if there's a romaji translation rule which starts with one or more last
characters from the string. }
function RomaHasPotentialMatches(const str: string): boolean;
var i, j: integer;
  tran: TRomajiTranslator;
begin
  if Length(str)<=0 then begin
    Result := false;
    exit;
  end;

  if curlang='j' then
    tran := roma_user
  else
    tran := rpy_user;

  Result := false;
  for i := 1 to Length(str) do begin
    j := tran.RomajiPartialMatch(copy(str,Length(str)-i+1,i));
    if j>=0 then begin
      Result := true;
      break;
    end;
  end;
end;

//Handles a keystroke directed at the editor
procedure TfEditor.HandleKeystroke(c: char);
const DEFCPROPS: TCharacterProps = (wordstate:'-';learnstate:9;dicidx:0;docdic:1);
var CharType: TInputBufferType;
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

  //If the old block is converted, start a new insert block
  if FInsertionState in [isConfirmedAsIs, isConverted] then
    ClearInsBlock;

  if not Self.TextSelection.IsEmpty then
    DeleteSelection();

  if (c=' ') and (FInputBuffer<>'') then
  begin
   //Accept suggestion
    ResolveInsert({AcceptSuggestion=}aKanjiMode.Checked);
    FileChanged:=true;
    if aKanjiMode.Checked then
      exit;
  end;
  if (c=Chr(VK_RETURN)) and (FInputBuffer<>'') then
  begin
   //Reject suggestion
    ResolveInsert({AcceptSuggestion=}false);
    FileChanged:=true;
    if aKanjiMode.Checked then exit;
  end;
  if (c=Chr(VK_BACK)) and (FInputBuffer<>'') then
  begin
    delete(FInputBuffer, Length(FInputBuffer), 1);
    SetProvisionalInsert(GetInsertKana(ikPreview),nil);
    if FInputBuffer='' then
      FInsertionState := isConfirmedAsIs;
    FileChanged:=true;
    mustrepaint:=true;
    ShowText(true);
    exit;
  end;
  if c=Chr(VK_RETURN) then
  begin
    doc.SplitLine(SourceCur.x, SourceCur.y);
    FileChanged:=true;
    SourceCur := SourcePos(0, SourceCur.y+1);
    InvalidateText;
    exit;
  end;
  if c=Chr(VK_BACK) then
  begin
    if SourceCur <> SelectionStart then
      DeleteSelection()
    else
    if (cur.x>0) or (cur.y>0) then
    begin
      if cur.x>0 then
        Cur := CursorPos(Cur.x-1,Cur.y)
      else //cur.y>0
        if SourceCur.x = 0 then
          cur := CursorPos(2550,cur.y-1)
        else
          cur := CursorPos(linl[cur.y-1].len, cur.y-1);
      ShowText(true);
      doc.DeleteCharacter(SourceCur);
    end;
    FileChanged:=true;
    InvalidateText;
    exit;
  end;

  if Ord(c)<$0020 then //not a printable char
    exit;

 { We accept characters and store them as-is in inputbuffer. We also keep
  track of what kind of word we're typing (hiragana/katakana/other).
  Elsewhere we take current contents of input buffer and convert according to
  its type (to hiragana/to katakana/leave as is).
  There are also immediate characters (punctuation and the like) which flush
  input buffer and are printed instantly. }

  if aAsciiMode.Checked then begin
   //In AsciiMode all characters are immediate + no conversion
    CharType := ibAsIs;
  end else begin
    if TCharacter.IsUpper(c) then begin
      if curlang='c' then
        CharType := ibAsIs
      else
        CharType := ibKatakana //uppercase encodes katakana
    end else
      CharType := ibHiragana; //lowercase = hiragana

    if (not RomaHasPotentialMatches(FInputBuffer+c))
    and IsImmediateCharacter(c) then
      CharType := ibAsIs; //immediate output
  end;

 //Instant output - AsIs data is not buffered
  if CharType = ibAsIs then begin
    if FInputBuffer<>'' then
      ResolveInsert({AcceptSuggestion=}false);
    ClearInsBlock;
    SetProvisionalInsert(fstring(ConvertImmediateChar(c)),CharPropArray(DEFCPROPS));
    FInsertionState:=isConfirmedAsIs;
    FileChanged:=true;
    mustrepaint:=true;
    ShowText(true);
    exit;
  end;

 //Input buffer
  FileChanged:=true;
  if FInputBuffer='' then begin
    FInputBufferType := CharType;
    FInputBuffer:=c;
    FInsertionState:=isTyping;
  end else begin
    if chartype<>FInputBufferType then begin
      ResolveInsert({AcceptSuggestion=}false);
      ClearInsBlock;
      FInputBufferType:=chartype;
      FInputBuffer:=c;
    end else
      FInputBuffer:=FInputBuffer+c;
    FInsertionState:=isTyping;
  end;
  SetProvisionalInsert(GetInsertKana(ikPreview),nil);
  mustrepaint:=true;
  ShowText(true);
end;

//Converts a single keypress into a character for immediate insertion,
//or [may be used] when there are keypresses left after roma->kana conversion.
//Only for presentation.
function TfEditor.ConvertImmediateChar(const c: char): char;
begin
  if aAsciiMode.Checked then
    Result := c
  else begin
   //Kana mode has some non-trivial character replacements
    case c of
      ',': Result := #$3001;
      '.': Result := #$3002;
     //Special uses for standard chars. How do we type <>()[]{} then?
      '<': Result := #$3008;
      '>': Result := #$3009;
      '(': Result := #$300C;
      ')': Result := #$300D;
      '[': Result := #$3016;
      ']': Result := #$3017;
      '{': Result := #$3010;
      '}': Result := #$3011;
    else Result := c;
    end;
  end;

 //In any case, honor fullwidth preference
  if aFullwidthLatin.Checked then begin
   //Two char blocks are equal: 0021..007E <-> FF01..FF5E
    if (Ord(Result)>$0020) and (Ord(Result)<$0080) then
      Result := Chr(Ord(Result)-$0020+$FF00)
    else
    if Ord(Result)=$0020 then
      Result := #$3000;
   //Add any additional HW->FW transformations here.
  end;
end;

//Same but processes the whole string
function TfEditor.ConvertImmediateChars(const str: FString): FString;
var i: integer;
begin
  Result := '';
  for i := 1 to flength(str) do
    Result := Result + ConvertImmediateChar(fgetch(str, i));
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
  dw:=GetDocWord(x,y,wt);
 { GetDocWord makes upper bound guess on the length of the word,
  then search result gives us exact value.
  It may be shorter (itteoku => only ITTE is parsed) or longer (rarely) }
  if word<>nil then begin
    rlen:=word.inflen
  end else
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
    if word.dicname<>'' then begin
      globdict := doc.docdic.IndexOf(word.dicname);
      if globdict<0 then
        globdict := doc.docdic.add(word.dicname);
    end else begin
      globdict := 0;
      worddict := 0; //how did this happen though?
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
 //local --- s, i, learnstate_s
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
    dw:=GetDocWord(x+rlen,y,wt);
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

  doc.PasteText(SourceCur, chars, props, AnnotMode, @tmp);
  SourceCur := tmp; //end of inserted text
  InvalidateText;
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
  if Clipboard.GetClipboard(CF_WAKAN, ms) then
  try
    ms.Seek(0,soFromBeginning);
    tmpDoc := nil;
    try
      try
        tmpDoc := TWakanText.Create;
        Loaded := tmpDoc.LoadWakanText(ms,{silent=}true);
        doc.PasteDoc(SourceCur, tmpDoc, @pasteEndPos);
      except
        on E: EBadWakanTextFormat do
          Loaded := false;
      end;
    finally
      FreeAndNil(tmpDoc);
    end;
    SourceCur := pasteEndPos; //end of inserted text
    InvalidateText;
    if Loaded then
      FileChanged:=true;
  finally
    FreeAndNil(ms);
  end;

  if not Loaded then begin //default to bare text
    props.Clear;
    PasteText(Clipboard.Text,props,amDefault); //does InvalidateText/FileChanged internally
  end;

  ShowText(true);
end;

procedure TfEditor.DeleteSelection;
var block: TTextSelection;
begin
  block := Self.TextSelection;
  doc.DeleteBlock(block);
  SourceCur := SourcePos(block.fromx, block.fromy);
  ResetSelection; //explicitly, since it's now invalid
  InvalidateText;
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
  ch: FString;
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
    ch := doc.GetDoc(cx,cy);
    CCharProp := TCharPropertyCursor.Create(TCharProp);
    try
      if curlang='j' then
        meaning := CCharProp.GetCharProps(ch, ptJapaneseDefinition)
      else
      if curlang='c' then
        meaning := CCharProp.GetCharProps(ch, ptChineseDefinition)
      else
        meaning := '';
    finally
      FreeAndNil(CCharProp);
    end;
  end;
  while (reading<>'') and (kanji<>'') and (fgetch(reading,flength(reading))=fgetch(kanji,flength(kanji))) do
  begin
    fdelete(reading,flength(reading),1);
    fdelete(kanji,flength(kanji),1);
  end;
end;

//JWBWakanText calls this callback to retrieve a dictionary entry associated with a position
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

{ Makes upper bound guess on the length of the word starting at the specified
 position. See JWBDicSearch.GuessWord() }
function TfEditor.GetDocWord(x,y: integer; var wordtype: TEvalCharType):string;
begin
  if (y=-1) or (y>doc.Lines.Count-1) then begin
    WordType := EC_UNKNOWN;
    Result := '';
  end else
    Result := GuessWord(doc.Lines[y], x+1, wordtype);
end;

{ Returns the word currently under the caret }
function TfEditor.GetWordAtCaret(out AWordtype: TEvalCharType): string;
var fcur: TSourcePos;
begin
  fcur := SourceCur;
  AWordtype := EC_UNKNOWN;
  Result := GetDocWord(fcur.x, fcur.y, AWordtype);
end;

{ Called when the document text changes - as a result of loading or editing. }
procedure TfEditor.InvalidateText;
begin
  InvalidateGraphicalLines;
  InvalidateNormalizeSourceCur;
  //FOR NOW we're calling ShowText here because it redoes a number of other things
  //Eventually we should move all that to their own Invalidate* stuff and just
  //invalidate it here.
  RepaintText;
end;


{ Cursor and selection }

function TfEditor.GetSourceCur: TSourcePos;
begin
  if FNormalizeSourceCur then
    NormalizeSourceCur;
  Result := FSourceCur;
end;

procedure TfEditor.SetSourceCur(const Value: TSourcePos);
begin
  //Do not invalidate stuff if nothing has changed
  if FSourceCur = Value then
    exit;
  FSourceCur := Value;
  if not Self.InSelectionMode then
    FSelectionStart := FSourceCur;
  CursorEnd := false;
  //we can't know whether to put graphical cursor before or after the line wrap
  //if you know, update CursorEnd after setting RCur
  InvalidateCursorPos;
  InvalidateNormalizeSourceCur;
end;

//Adjust SourceCur to fit the document next time we access it
procedure TfEditor.InvalidateNormalizeSourceCur;
begin
  FNormalizeSourceCur := true;
end;

//Adjust SourceCur to fit the document
procedure TfEditor.NormalizeSourceCur;
begin
  if FSourceCur.y < 0 then FSourceCur.y := 0;
  if FSourceCur.y >= doc.Lines.Count then
    FSourceCur := doc.EndOfDocument;
  if FSourceCur.x < 0 then FSourceCur.x := 0;
  FNormalizeSourceCur := false;
  //We could've checked whether we've changed anything, but whatever:
  InvalidateCursorPos;
end;

procedure TfEditor.SetSelectionStart(const Value: TSourcePos);
begin
  FSelectionStart := Value;
end;

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

  if linl.Count=0 then CalculateGraphicalLines;
  for i:=0 to linl.Count-1 do
    if (linl[i].ys=SourceCur.y) and (linl[i].xs<=SourceCur.x) and (linl[i].xs+linl[i].len>=SourceCur.x) then
    begin
      if (not CursorEnd) and (linl[i].xs+linl[i].len=SourceCur.x)
        and (i<linl.Count-1) and (linl[i+1].ys=linl[i].ys) then continue; //exact match goes to the next line
      Result.x:=SourceCur.x-linl[i].xs;
      Result.y:=i;
      exit;
    end;

 //No line found => return 0, 0
  Result.y := 0;
  Result.x := 0;

  FCursorPosInvalid := false;

 //TODO: In complicated cases (no exact matching line found) try to return
 //the closest position (last one with y<=rcur.y and x<=rcur.x or the next one
 //after that, or 0:0).
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
  SourceCur := newrcur;
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

//True if we're currently in the highlight selection mode
function TfEditor.InSelectionMode: boolean;
begin
  Result := FShiftPressed or FLeftMouseDown;
end;

//Resets the selection highlight
procedure TfEditor.ResetSelection;
begin
  FSelectionStart := Self.SourceCur;
 //Repaint currently rechecks the highlight for changes every time,
 //so only trigger repaint
  EditorPaintBox.Invalidate;
end;



{ Font }

//Returns active main font face
//Currently this is always the appropriate globally configured font
function TfEditor.GetFontName: string;
begin
  if curlang='c' then
    Result := FontChineseGrid
  else
    Result := FontJapaneseGrid;
end;

procedure TfEditor.SetFontSize(Value: integer);
begin
  if FFontSize=Value then exit;
  FFontSize := Value;
  aSmallFont.Checked:=(FFontSize=FontSizeSmall);
  aMedFont.Checked:=(FFontSize=FontSizeMedium);
  aLargeFont.Checked:=(FFontSize=FontSizeLarge);
  if cbFontSize.Text<>IntToStr(Value) then begin
    cbFontSize.Text := IntToStr(Value);
    cbFontSizeGuessItem(cbFontSize.Text);
    cbFontSizeChange(cbFontSize);
  end;
  InvalidateGraphicalLines;
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


{
Half-width latin support.
This is different from HW/FW typing! FW latin is special latin, HW latin is
normal latin which monospaced fonts can display 2 in a cell.
We can still draw HW latin as FW if the font is not monospace.
}

//Retests whether half-width latin is enabled and the current font supports it.
//Enables or disables it appropriately.
procedure TfEditor.RetestHalfwidthSupport;
var tm: TOutlineTextMetric;
begin
  EditorPaintbox.Canvas.Font.Name := Self.FontName;
  if GetOutlineTextMetrics(EditorPaintbox.Canvas.Handle, sizeof(tm), @tm) = 0 then begin
    //Cannot query the font details, let's not complain
    Self.EnableHalfWidth := false;
    exit;
  end;
  {
  Two values are acceptable:
    PAN_PROP_EVEN_WIDTH   = the latin characters are of equal width
    PAN_PROP_MONOSPACED   = EVEN_WIDTH + the height is roughly twice the width (true 2-in-a-cell)
  EVEN_WIDTH could be used if we shrank the characters proportionally to fit,
  but we don't bother since most monospaced fonts have MONOSPACED correctly.
  }
  Self.EnableHalfWidth := (tm.otmPanoseNumber.bProportion = PAN_PROP_MONOSPACED);
end;

//Enables or disables the drawing of latin as half-cell monospaced characters.
//If disabled, all characters will occupy the whole cell.
procedure TfEditor.SetEnableHalfWidth(const Value: boolean);
begin
  if FEnableHalfWidth = Value then exit;
  FEnableHalfWidth := Value;
  InvalidateGraphicalLines;
end;

function TfEditor.IsHalfWidth(x,y:integer):boolean;
begin
  Result := FEnableHalfWidth and IsHalfWidthChar(doc.GetDoc(x,y));
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
Returns: number of characters from the start of the line to the closest valid
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


function EditorHighlightContent(Control: TControl; DragStart, MousePos: TPoint): string;
var rx,ry: integer;
  rpos: TSourcePos;
  wtt: TEvalCharType;
begin
  if (fEditor=nil) or (Control<>fEditor.EditorPaintBox) then begin
    Result := '';
    exit;
  end;
  rpos := fEditor.TryGetExactLogicalPos(MousePos.x,MousePos.y);
  rx := rpos.x; ry := rpos.y;
  if (ry>=0) and (rx>=0) and (rx<=fEditor.doctr[ry].charcount) then
    Result:=fEditor.GetDocWord(rx,ry,wtt)
  else
    Result:='';
  SetSelectionHighlight(0,0,0,0,nil);
end;

initialization
  IntTip.RegisterHighlightHandler(TWakanPaintBox, EditorHighlightContent);

end.
