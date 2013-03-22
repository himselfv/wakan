unit JWBMenu;

interface

{$R WINXP.RES}

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, ComCtrls, RXCtrls, Db,
  DBTables, ExtCtrls, Grids, TextTable, Buttons, {ThemeMgr,} MemSource, ShellApi,
  ActnList, Menus, rxPlacemnt{MCH, madCodeHook}, JWBStrings,
  StdPrompt, JWBDic, JWBDicSearch;

type
  TfMenu = class(TForm)
    Panel1: TPanel;
    tab1: TSpeedButton;
    tab5: TSpeedButton;
    tab2: TSpeedButton;
    btnChineseMode: TSpeedButton;
    Bevel3: TBevel;
    btnJapaneseMode: TSpeedButton;
    Bevel4: TBevel;
    SpeedButton22: TSpeedButton;
    Bevel5: TBevel;
    tab3: TSpeedButton;
    Timer1: TTimer;
    ActionList1: TActionList;
    aSaveUser: TAction;
    aCancelUser: TAction;
    aStatistics: TAction;
    aExit: TAction;
    aKanji: TAction;
    aKanjiSearch: TAction;
    aKanjiDetails: TAction;
    aKanjiCompounds: TAction;
    aKanjiPrint: TAction;
    aDict: TAction;
    aDictDetails: TAction;
    aDictKanji: TAction;
    aDictCategories: TAction;
    aDictAdd: TAction;
    aDictEditor: TAction;
    aUser: TAction;
    aUserAdd: TAction;
    aUserSettings: TAction;
    aUserDetails: TAction;
    aUserPrint: TAction;
    aUserGenerate: TAction;
    aSettings: TAction;
    aSettingsDict: TAction;
    aHelp: TAction;
    aAbout: TAction;
    aJapanese: TAction;
    aChinese: TAction;
    aEditorNew: TAction;
    aEditorOpen: TAction;
    aEditorSave: TAction;
    aEditorSaveAs: TAction;
    aEditorCut: TAction;
    aEditorCopy: TAction;
    aEditorPaste: TAction;
    aEditorSelectAll: TAction;
    aEditorKanjiMode: TAction;
    aEditorKanaMode: TAction;
    aEditorASCIIMode: TAction;
    aEditorReading: TAction;
    aEditorMeaning: TAction;
    aEditorClear: TAction;
    aEditorFill: TAction;
    aEditorSet: TAction;
    aEditorPrint: TAction;
    aKanjiAll: TAction;
    aKanjiLearned: TAction;
    aKanjiCommon: TAction;
    aKanjiClipboard: TAction;
    aKanjiPinYin: TAction;
    aKanjiYomi: TAction;
    aKanjiRadical: TAction;
    aKanjiAddClipboard: TAction;
    aKanjiFullDetails: TAction;
    aDictJapanese: TAction;
    aDictEnglish: TAction;
    aDictClipboard: TAction;
    aDictAddClipboard: TAction;
    aDictExact: TAction;
    aDictBeginning: TAction;
    aDictEnd: TAction;
    aKanjiWindow: TAction;
    aKanjiSetLearned: TAction;
    aKanjiMeaning: TAction;
    aEditorWindow: TAction;
    aEditorSmallFont: TAction;
    aEditorLargeFont: TAction;
    aEditorMedFont: TAction;
    MainMenu1: TMainMenu;
    miDatabase: TMenuItem;
    JapMode1: TMenuItem;
    ChinMode1: TMenuItem;
    miSaveUserChanges: TMenuItem;
    miCancelUserChanges: TMenuItem;
    N1: TMenuItem;
    miStatistics: TMenuItem;
    miSettings: TMenuItem;
    miDictionaryManager: TMenuItem;
    N15: TMenuItem;
    miExit: TMenuItem;
    miCharacters: TMenuItem;
    N5: TMenuItem;
    miDisplayAll: TMenuItem;
    miLearnedOnly: TMenuItem;
    miCommonOnly: TMenuItem;
    miInClipboardOnly: TMenuItem;
    N3: TMenuItem;
    miSearchByPinYin: TMenuItem;
    miSearchByYomi: TMenuItem;
    miSearchByMeaning: TMenuItem;
    miSearchByRadical: TMenuItem;
    N6: TMenuItem;
    miAddToClipboard: TMenuItem;
    miSetAsLearned: TMenuItem;
    miPrintCards: TMenuItem;
    N13: TMenuItem;
    miSearch: TMenuItem;
    miDetails: TMenuItem;
    miCompounds: TMenuItem;
    eDictionarycSlovnk1: TMenuItem;
    miSearchJapanese: TMenuItem;
    miSearchEnglish: TMenuItem;
    miSearchByClipboard: TMenuItem;
    N8: TMenuItem;
    miSearchExactWord: TMenuItem;
    miSearchBeginning: TMenuItem;
    miSearchEnding: TMenuItem;
    N12: TMenuItem;
    miAddToClipboard2: TMenuItem;
    N16: TMenuItem;
    miCharactersInWord: TMenuItem;
    miExamples: TMenuItem;
    Editor2: TMenuItem;
    N22: TMenuItem;
    miEditorNew: TMenuItem;
    miEditorOpen: TMenuItem;
    miEditorSave: TMenuItem;
    miEditorSaveAs: TMenuItem;
    N17: TMenuItem;
    miEditorCut: TMenuItem;
    miEditorCopy: TMenuItem;
    miEditorPaste: TMenuItem;
    miEditorSelectAll: TMenuItem;
    N18: TMenuItem;
    miCharacterMode: TMenuItem;
    miKanaMode: TMenuItem;
    miASCIIMode: TMenuItem;
    N19: TMenuItem;
    miDisplayReading: TMenuItem;
    miDisplayMeaning: TMenuItem;
    miFontSize: TMenuItem;
    miFontSizeSmall: TMenuItem;
    miFontSizeMedium: TMenuItem;
    miFontSizeLarge: TMenuItem;
    N20: TMenuItem;
    miTranslation: TMenuItem;
    miClearTranslation: TMenuItem;
    miSetTranslation: TMenuItem;
    miAutoFillTranslation: TMenuItem;
    N21: TMenuItem;
    miPrint: TMenuItem;
    miVocabulary1: TMenuItem;
    miAddWord: TMenuItem;
    miListSettings: TMenuItem;
    miWordDetails: TMenuItem;
    N7: TMenuItem;
    miPrintList: TMenuItem;
    miListGenerator: TMenuItem;
    miHelp: TMenuItem;
    miHelpContents: TMenuItem;
    N11: TMenuItem;
    miAbout: TMenuItem;
    Panel3: TPanel;
    Panel2: TPanel;
    Shape9: TShape;
    PaintBox3: TPaintBox;
    aMode1: TAction;
    aMode2: TAction;
    aMode3: TAction;
    aMode4: TAction;
    aMode5: TAction;
    miTools: TMenuItem;
    miCharacterList: TMenuItem;
    miDictionary: TMenuItem;
    miTextEditor: TMenuItem;
    miVocabulary: TMenuItem;
    miCharacterList2: TMenuItem;
    miDictionary2: TMenuItem;
    N4: TMenuItem;
    miTextEditor2: TMenuItem;
    miVocabulary2: TMenuItem;
    N9: TMenuItem;
    Bevel1: TBevel;
    ScreenTimer: TTimer;
    Panel4: TPanel;
    SpeedButton1: TSpeedButton;
    SpeedButton2: TSpeedButton;
    miEditorExportAs: TMenuItem;
    aDictInflect: TAction;
    aDictAuto: TAction;
    aDictGroup1: TAction;
    aDictGroup2: TAction;
    aDictGroup3: TAction;
    miSearchInflectedWords: TMenuItem;
    miAutoSearchWhileTyping: TMenuItem;
    miDictionaryGroup: TMenuItem;
    N10: TMenuItem;
    miDictionaryGroup1: TMenuItem;
    miDictionaryGroup2: TMenuItem;
    miDictionaryGroup3: TMenuItem;
    aUserExamples: TAction;
    miExamples2: TMenuItem;
    aEditorColors: TAction;
    miUseColors: TMenuItem;
    aDictMiddle: TAction;
    miSearchSubstring: TMenuItem;
    N23: TMenuItem;
    N25: TMenuItem;
    N2: TMenuItem;
    miSaveCharactersToFile: TMenuItem;
    miAddWordIntoVocabulary: TMenuItem;
    miChangeLanguage: TMenuItem;
    aChangeLanguage: TAction;
    FormPlacement1: TFormPlacement;
    aFullscreenMode: TAction;
    FullscreenMode1: TMenuItem;
    N26: TMenuItem;
    N27: TMenuItem;
    aCategoryManager: TAction;
    miManageCategories: TMenuItem;
    N24: TMenuItem;
    aEditorCopyAs: TAction;
    aPortraitMode: TAction;
    PortraitMode1: TMenuItem;
    aEditorExport: TAction;
    aVocabExport: TAction;
    aVocabImport: TAction;
    N14: TMenuItem;
    N00934eExport1: TMenuItem;
    aVocabImport1: TMenuItem;
    procedure FormDestroy(Sender: TObject);
    procedure BitBtn1Click(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure SpeedButton2Click(Sender: TObject);
    procedure SpeedButton7Click(Sender: TObject);
    procedure tab1Click(Sender: TObject);
    procedure tab2Click(Sender: TObject);
    procedure tab5Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure Timer1Timer(Sender: TObject);
    procedure btnJapaneseModeClick(Sender: TObject);
    procedure btnChineseModeClick(Sender: TObject);
    procedure Action1Execute(Sender: TObject);
    procedure PaintBox3Paint(Sender: TObject);
    procedure SpeedButton22Click(Sender: TObject);
    procedure aSaveUserExecute(Sender: TObject);
    procedure aCancelUserExecute(Sender: TObject);
    procedure aStatisticsExecute(Sender: TObject);
    procedure aExitExecute(Sender: TObject);
    procedure aKanjiExecute(Sender: TObject);
    procedure aDictExecute(Sender: TObject);
    procedure aUserExecute(Sender: TObject);
    procedure aKanjiSearchExecute(Sender: TObject);
    procedure aKanjiDetailsExecute(Sender: TObject);
    procedure aKanjiCompoundsExecute(Sender: TObject);
    procedure aKanjiPrintExecute(Sender: TObject);
    procedure aDictDetailsExecute(Sender: TObject);
    procedure aDictKanjiExecute(Sender: TObject);
    procedure aDictCategoriesExecute(Sender: TObject);
    procedure aDictAddExecute(Sender: TObject);
    procedure aDictEditorExecute(Sender: TObject);
    procedure aUserAddExecute(Sender: TObject);
    procedure aUserSettingsExecute(Sender: TObject);
    procedure aUserDetailsExecute(Sender: TObject);
    procedure aUserPrintExecute(Sender: TObject);
    procedure aUserGenerateExecute(Sender: TObject);
    procedure aSettingsExecute(Sender: TObject);
    procedure aSettingsDictExecute(Sender: TObject);
    procedure aHelpExecute(Sender: TObject);
    procedure aAboutExecute(Sender: TObject);
    procedure aJapaneseExecute(Sender: TObject);
    procedure aChineseExecute(Sender: TObject);
    procedure aEditorNewExecute(Sender: TObject);
    procedure aEditorOpenExecute(Sender: TObject);
    procedure aEditorSaveExecute(Sender: TObject);
    procedure aEditorSaveAsExecute(Sender: TObject);
    procedure aEditorCutExecute(Sender: TObject);
    procedure aEditorCopyExecute(Sender: TObject);
    procedure aEditorPasteExecute(Sender: TObject);
    procedure aEditorSelectAllExecute(Sender: TObject);
    procedure aEditorKanjiModeExecute(Sender: TObject);
    procedure aEditorKanaModeExecute(Sender: TObject);
    procedure aEditorASCIIModeExecute(Sender: TObject);
    procedure aEditorReadingExecute(Sender: TObject);
    procedure aEditorMeaningExecute(Sender: TObject);
    procedure aEditorClearExecute(Sender: TObject);
    procedure aEditorFillExecute(Sender: TObject);
    procedure aEditorSetExecute(Sender: TObject);
    procedure aEditorPrintExecute(Sender: TObject);
    procedure aKanjiAllExecute(Sender: TObject);
    procedure aKanjiLearnedExecute(Sender: TObject);
    procedure aKanjiCommonExecute(Sender: TObject);
    procedure aKanjiClipboardExecute(Sender: TObject);
    procedure aKanjiPinYinExecute(Sender: TObject);
    procedure aKanjiYomiExecute(Sender: TObject);
    procedure aKanjiRadicalExecute(Sender: TObject);
    procedure aKanjiAddClipboardExecute(Sender: TObject);
    procedure aKanjiSetLearnedExecute(Sender: TObject);
    procedure aKanjiFullDetailsExecute(Sender: TObject);
    procedure aDictJapaneseExecute(Sender: TObject);
    procedure aDictEnglishExecute(Sender: TObject);
    procedure aDictClipboardExecute(Sender: TObject);
    procedure aDictAddClipboardExecute(Sender: TObject);
    procedure aDictExactExecute(Sender: TObject);
    procedure aDictBeginningExecute(Sender: TObject);
    procedure aDictEndExecute(Sender: TObject);
    procedure aKanjiWindowExecute(Sender: TObject);
    procedure aKanjiMeaningExecute(Sender: TObject);
    procedure aEditorWindowExecute(Sender: TObject);
    procedure aEditorSmallFontExecute(Sender: TObject);
    procedure aEditorLargeFontExecute(Sender: TObject);
    procedure aEditorMedFontExecute(Sender: TObject);
    procedure tab3Click(Sender: TObject);
    procedure FormResize(Sender: TObject);
    procedure TabControl1Change(Sender: TObject);
    procedure aMode1Execute(Sender: TObject);
    procedure aMode2Execute(Sender: TObject);
    procedure aMode3Execute(Sender: TObject);
    procedure aMode4Execute(Sender: TObject);
    procedure aMode5Execute(Sender: TObject);
    procedure SpeedButton1Click(Sender: TObject);
    procedure ScreenTimerTimer(Sender: TObject);
    procedure PaintBox3MouseMove(Sender: TObject; Shift: TShiftState; X,
      Y: Integer);
    procedure aDictInflectExecute(Sender: TObject);
    procedure aDictAutoExecute(Sender: TObject);
    procedure aDictGroup1Execute(Sender: TObject);
    procedure aDictGroup2Execute(Sender: TObject);
    procedure aDictGroup3Execute(Sender: TObject);
    procedure aUserExamplesExecute(Sender: TObject);
    procedure aEditorColorsExecute(Sender: TObject);
    procedure PaintBox3MouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure miSaveCharactersToFileClick(
      Sender: TObject);
    procedure aDictMiddleExecute(Sender: TObject);
    procedure aChangeLanguageExecute(Sender: TObject);
    procedure aFullscreenModeExecute(Sender: TObject);
    procedure aCategoryManagerExecute(Sender: TObject);
    procedure aEditorCopyAsExecute(Sender: TObject);
    procedure aPortraitModeExecute(Sender: TObject);
    procedure aEditorExportExecute(Sender: TObject);
    procedure aVocabExportExecute(Sender: TObject);
    procedure aVocabImportExecute(Sender: TObject);

  private
    initdone:boolean;
    procedure LoadWakanCfg(const filename: string);
  public
    procedure InitializeWakan;

  private //Docking
    procedure MainDock(form:TForm;panel:TPanel);
    procedure MainUndock(form:TForm);
  protected
   { Kanji details can work in docked or free-floating mode.
    If CharDetDocked is set, KanjiDetails is docked and will be shown/hidden by
    ChangeDisplay, like other panels. }
    FCharDetDocked: boolean;
  public
   { In docked mode, we remember whether to show KanjiDetails on page 3 and on
    page 4 separately. }
    CharDetDockedVis1,
    CharDetDockedVis2:boolean;
    function DockExpress(form:TForm;dock:boolean):boolean;
    procedure SetCharDetDocked(Value: boolean; Loading: boolean);
    property CharDetDocked: boolean read FCharDetDocked;

  public
    StrokeOrderPackage:TPackageSource; //apparently a remnant from an older way of drawing stroke order. Always == nil

    screenTipDebug:string;
    screenModeSc,screenModeWk:boolean;
    ctlFileMap:cardinal;
    ptrFileMap:pointer;

    procedure TranslateAll;
    procedure WriteUserPackage(dir:string);
    procedure RefreshCategory;
    procedure RefreshKanjiCategory;
    procedure ToggleForm(form:TForm;sb:TSpeedButton;action:TAction); overload;
    procedure ToggleForm(form:TForm;sb:TSpeedButton;action:TAction;state:boolean); overload;
    procedure RescanDicts;
    procedure ClearDicts;
    procedure SwitchLanguage(lanchar:char);
    function GetCharValue(index,vt:integer):string;
    function GetCharValueInt(index,vt:integer):integer;
    function GetCharValueRad(index,vt:integer):integer;
    procedure ChangeDisplay;

  protected
    DicLoadPrompt: TSMPromptForm;
    procedure DicLoadStart(Sender: TObject);
    procedure DicLoadEnd(Sender: TObject);
    procedure DicLoadException(Sender: TObject; E: Exception);
  public
    function NewDict(dicname: string): TJaletDic;

  protected
    FUserDataChanged:boolean;
    LastSaveTime:TDatetime; //for UserData
    procedure SetUserDataChanged(Value: boolean);
    function FlushUserData:boolean;
    procedure AutosaveUserData; //check if it's time for autosave
  public
    procedure SaveUserData;
    procedure LoadUserData;
    procedure ChangeUserData;
    procedure RebuildUserIndex;
    procedure ExportUserData(filename:string);
    procedure ImportUserData(filename:string);
    property UserDataChanged: boolean read FUserDataChanged write SetUserDataChanged;

  public
    displaymode:integer; //will be applied on ChangeDisplay
    curdisplaymode:integer; //last applied mode

  private //Text under mouse
   //Drag start control+point when drag-selecting, else nil.
    DragStartCtl:TControl;
    DragStartPt:TPoint;
   //Last selection-enabled control mouse was sighted over
    HoverCtl:TControl;
    HoverPt:TPoint;
   //Currently selected string/String under the mouse pointer right now
   //UpdateSelection changes this member
    StringUnderMouse:string;
    HandlingPopup:boolean; //set while we're doing popup handling -- TODO: Do we really need this?
    LastMousePt:TPoint; //used to check whether the mouse stays still
    LastMouseMove:cardinal; //tick count
    procedure AbortDrag;
    procedure UpdateSelection;
    procedure HandlePopup(ShowImmediate:boolean=false);
  public
    procedure IntTipMouseMove(c:TControl;x,y:integer;leftDown:boolean);
    procedure IntTipMouseDown(c:TControl;x,y:integer);
    procedure IntTipMouseUp;
    procedure PopupMouseUp(button:TMouseButton;shift:TShiftState;x,y:integer);
    procedure PopupImmediate(left:boolean);

  protected //Clipboard
   { SetClipboardViewer is supported starting with Windows 2000,
    so if there's a need we can implement dynamic linking and fall back to polling -
    it's kept as a safety measure anyway since CB chains are prone to breaking }
    CbNextViewer: HWND;
    procedure WmChangeCbChain(var Msg: TMessage); message WM_CHANGECBCHAIN;
    procedure WmDrawClipboard(var Msg: TMessage); message WM_DRAWCLIPBOARD;
    procedure ClipboardChanged;
    procedure Clipboard_Paint;
    procedure Clipboard_Update; //update clip with UNICODETEXT from Clipboard
    procedure Clipboard_Clear;
  public
   { 
    How to use:
      Reset
      Add(...)
      Add(...)
      clip := '...'
      Publish
   }
    procedure ResetClipboard;
    procedure AddToClipboard(uFormat: UINT; data: pointer; size: integer); overload;
    procedure AddToClipboard(uFormat: UINT; text: RawByteString); overload;
    procedure AddToClipboard(uFormat: UINT; text: UnicodeString); overload;
    procedure AddToClipboard(uFormat: UINT; data: TMemoryStream; AOwnsStream: boolean = false); overload;
    procedure PublishClipboard;
    procedure SetClipboard; //to whatever is in clip
    function GetClipboard(uFormat: UINT; out ms: TMemoryStream): boolean;

  end;

  TTextInfo = record
    hwnd:HWND;
    hdc:HDC;
    x,y,w,h:integer;
    slen:byte;
    str:array[0..255] of word;
    len:array[0..255] of byte;
    dcinfo:integer;
  end;
  TBitInfo = record
    hwnd:HWND;
    destdc,srcdc:HDC;
    xofs,yofs:integer;
  end;



var
  fMenu: TfMenu;
  romasys,jromasys,cromasys:integer;
  showroma,jshowroma,cshowroma:boolean;

 { Dictionaries }
  dicts: TDictionaryList; //Active dictionary list

 { Tables and fields }

  TChar: TTextTable;
  TCharIndex,
  TCharChinese,
  TCharType,
  TCharUnicode,
  TCharStrokeCount,
  TCharJpStrokeCount,
  TCharJpFrequency,
  TCharChFrequency,
  TCharJouyouGrade: integer;

  TCharRead: TTextTable;
  TCharReadKanji,
  TCharReadType,
  TCharReadReading, { WARNING! This field here is dangerous.
    Some properties store normal AnsiStrings here, other 4-char hex AnsiStrings,
    i.e. no auto-conversion to unicode is possible.
    When reading from, or searching by this field, you have to check it's property type:
       type := CCharRead.Int(TCharReadType);
       propType := GetCharPropType(type,3);
    And then recode data:
       R: TRadicals.Number field value, possibly int or 'int' (int in quotes)
       U,P: 4char hex
       other: AnsiString
   }
  TCharReadIndex,
  TCharReadReadDot,
  TCharReadPosition: integer;

  TRadicals: TTextTable;
  TRadicalsNumber,
  TRadicalsVariant,
  TRadicalsUnicode,
  TRadicalsStrokeCount,
  TRadicalsUnicodeCount,
  TRadicalsBushuCount,
  TRadicalsJapaneseCount,
  TRadicalsKangXiCount: integer;

  clip:FString;
  oldhandle:THandle;
  critsec:boolean;
  globheight:integer;
  ChinesePresent:boolean;

 //Loaded from config file -- see comments in wakan.cfg
  partl: TStringList; //particles such as NO, NI, etc
  suffixl: TStringList; //suffixes
  defll: TDeflectionList; //verb deflections
  romasortl: array of record //romaji sort order
    roma: FString;
    order: string; //although it's integer inside
  end;
  readchl: TStringList; //list of readings to include to the reading chart

  userdataloaded:boolean;
  curlang:char;
  rdcnt,bitcnt:integer;
  curtext:array[1..100] of TTextInfo;
  curbit:array[1..100] of TBitInfo;
  ftext:array[0..255] of word;
  ftextbeg:array[0..255] of integer;
  ftextend:array[0..255] of integer;
  ftextpos:integer;
  popcreated:boolean;
  vocmode,exmode:integer;
  rainesearch:pointer;
  raineradicals:TStringList;
  sodir:TStringList;
  sobin:pointer;
  dictbeginset,dictmodeset:integer;
  kanji_othersearch:integer;

var
  CharPropTypes:TStringList; { All possible pieces of information to display in KanjiDetails info box. }
  chardetl:TStringList; { User configuration for KanjiDetails info box. }

{
Character property information available:
  0 - charPropId
  1 - sourceType -- where did we get that info
    'D': EDICT
    'U': UNIHAN
  2 - sourceField
  3 - propType -- controls how data for this property is stored and handled
    'U', 'P': stored as 'a'-type hex string, contains unicode
    'R', 'N', 'T'
  4 - english name
  5 - czech name
  6 - description (english)
}
function GetCharPropType(idx:integer;fld:integer):string;
function GetCharDet(i,j:integer):string;
function FindCharPropType(charPropId: string): integer;

function _l(const id:string):string; //shouldn't inline because it's for cases when JWBUnit is not in Uses!

implementation

uses StrUtils, JWBKanji, JWBUnit, JWBRadical, JWBForms,
  JWBSettings, JWBSplash, PKGWrite, JWBUser, UnicodeFont, registry, clipbrd,
  JWBWords, JWBNewCategory, JWBPrint, JWBStatistics,
  JWBWordList, JWBBitmap, JWBKanjiCompounds,
  JWBExamples, JWBUserDetails, JWBUserAdd, JWBUserFilters, JWBUserData,
  JWBKanjiDetails, JWBKanjiSearch, JWBWordDetails,
  JWBWordCategory, JWBWordKanji, JWBTranslate, JWBStrokeOrder,
  JWBDictMan, JWBDictImport, JWBDictCoding, JWBCharItem, JWBScreenTip,
  JWBInvalidator, JWBDicAdd, JWBLanguage, JWBFileType, JWBConvert,
  JWBWordsExpChoose, JWBMedia, JWBKanjiCard,
  JWBCategories, JWBAnnotations, JWBIO, JWBCommandLine,
  JWBEdictMarkers, JWBAutoImport, JWBDownloader, JWBDownloadSources,
  JWBPortableMode, JWBCategoryMgr, StreamUtils;

{$R *.DFM}

{ Pieces of information about Kanji }

function GetCharPropType(idx:integer;fld:integer):string;
var s:string;
begin
  s:=CharPropTypes[idx];
  while fld>0 do
  begin
    delete(s,1,pos(',',s));
    dec(fld);
  end;
  if fld<6 then delete(s,pos(',',s),length(s)-pos(',',s)+1);
  result:=s;
end;

function GetCharDet(i,j:integer):string;
var s:string;
begin
  s:=chardetl[i];
  while j>0 do
  begin
    delete(s,1,pos(';',s));
    dec(j);
  end;
  delete(s,pos(';',s),length(s)-pos(';',s)+1);
  result:=s;
end;

//Returns chartypel index for a given char type, or -1
function FindCharPropType(charPropId: string): integer;
var i: integer;
begin
  Result := -1;
  for i := 0 to CharPropTypes.Count - 1 do
    if charPropId=GetCharPropType(i,0) then begin
      Result := i;
      break;
    end;
end;


{ TfMenu }

procedure TfMenu.FormCreate(Sender: TObject);
begin
  initdone:=false;
  TChar := nil; //will be created on FormShow
  TCharRead := nil;
  TRadicals := nil;

  defll:=TDeflectionList.Create;
  suffixl:=TStringList.Create;
  partl:=TStringList.Create;
  readchl:=TStringList.Create;
  dicts:=TDictionaryList.Create;

  curlang:='j';
  DragStartCtl:=nil;
  HoverCtl:=nil;
  HandlingPopup:=false;
  LastMouseMove:=GetTickCount;

 //Nothing is docked to these so initialized them to hidden
  Panel2.Width := 0;
  Panel2.Height := 0;
  Panel4.Width := 0;
  Panel4.Height := 0;
end;

procedure TfMenu.FormDestroy(Sender: TObject);
begin
  TChar.Free;
  TCharRead.Free;
  TRadicals.Free;
  FreeKnownLists;

  defll.Free; //+
  suffixl.Free; //+
  partl.Free; //+
  readchl.Free; //+
  dicts.Free; //+
end;

procedure AddRomaSortRecord(const s: string);
var parts: TStringArray;
  i: integer;
begin
  parts := SplitStr(s, 2);
  if Length(parts)<=0 then exit;
  i := Length(romasortl);
  SetLength(romasortl, i+1);
  romasortl[i].roma := hextofstr(parts[0]);
  if Length(parts)>=2 then
    romasortl[i].order := parts[1]
  else
    romasortl[i].order := '';
end;

procedure AddPinYinRecord(const s: string);
var parts: TStringArray;
begin
  parts := SplitStr(s,4);
  parts[0] := hextofstr(parts[0]);
  parts[1] := uppercase(parts[1]);
  parts[2] := uppercase(parts[2]);
  parts[3] := uppercase(parts[3]);
  StrListAdd(romac, parts);
end;

procedure TfMenu.InitializeWakan;
var ps:TPackageSource;
  vi:TStringList;
  ms:TMemoryStream;
  i:integer;
begin
  LastSaveTime := now;
  examstruct:=nil;
  examindex:=nil;
  exampackage:=nil;
  screenModeSc:=false;
  screenModeWk:=false;
  if initdone then exit;

  try
    ParseCommandLine();

   //Load language or suggest to choose one
    fLanguage.LoadRegistrySettings;

    fLanguage.TranslateForm(fSplash);
    Caption:='WaKan '+WakanVer+' - '+_l('^eTool for learning Japanese & Chinese');
    if (Screen.Width<800) or (Screen.Height<600) then
      if Application.MessageBox(
        pchar(_l('^eThis version of WaKan requires at least 800x600 resolution.'#13#13'Do you really want to continue?')),
        pchar(_l('#00020^eError')),
        MB_YESNO or MB_ICONERROR)=idNo then
      begin
        Application.Terminate;
        exit;
      end;
    if (not FileExists('wakan.chr')) then
    begin
      Application.MessageBox(
        pchar(_l('#00346^eFile WAKAN.CHR was not found.'#13
          +'This file is required for application to run.'#13
          +'Please download this file from WAKAN website.'#13#13
          +'Application will now be terminated.')),
        pchar(_l('#00020^eError')),
        MB_OK or MB_ICONERROR);
      Application.Terminate;
      exit;
    end;
    if (not FileExists('wakan.cfg')) then
    begin
      Application.MessageBox(
        pchar(_l('#00347^eFile WAKAN.CFG is missing.'#13
          +'This file contains important configuration parameters and is required'
          +'for application to run.'#13#13'Application will now be terminated.')),
        pchar(_l('#00020^eError')),
        MB_OK or MB_ICONERROR);
      Application.Terminate;
      exit;
    end;

    oldhandle:=0;
    critsec:=false;
    TranslateAll;
    romasys:=1;
    showroma:=false;
    clip:='';

    fSettings.LoadSettings({DelayUI=}true);

    if fSettings.cbShowSplashscreen.Checked then begin
      fSplash.Show;
      fSplash.Update;
    end;

    //Configuration file
    try
      LoadWakanCfg('wakan.cfg');
    except
      Application.MessageBox(
        pchar(_l('#00352^eCannot load main configuration file.'#13
          +'File WAKAN.CFG is corrupted.'#13#13'Application will now exit.')),
        pchar(_l('#00020^eError')),
        MB_OK or MB_ICONERROR);
      Application.Terminate;
      exit;
    end;

   //It'll only read sections which it understands
    roma_t.LoadFromFile('wakan.cfg');

   { At this point we have loaded basic settings and functionality.
    Package enhancements are going to be loaded now. }

   { DownloadTest(); }

   { Import now before these packages are loaded }
    if Command='makeexamples'then
    begin
      fExamples.BuildExamplesPackage;
      Application.Terminate;
      exit;
    end else
    if Command='makedic'then
    begin
      fDictImport.edtDictFilename.Text:=MakeDicParams.Filename;
      fDictImport.edtDictName.Text:=MakeDicParams.Name;
      fDictImport.edtVersion.Text:=MakeDicParams.Version;
      if (MakeDicParams.Language='C')
      or (MakeDicParams.Language='c') then
        fDictImport.rgLanguage.ItemIndex:=1
      else
        fDictImport.rgLanguage.ItemIndex:=0;
      fDictImport.rgPriority.ItemIndex:=MakeDicParams.Priority;
      fDictImport.edtDescription.Text:=MakeDicParams.Description;
      fDictImport.edtCopyright.Text:=MakeDicParams.Copyright;
      fDictImport.cbAddWordIndex.Checked:=MakeDicParams.AddWordIndex;
      fDictImport.cbAddCharacterIndex.Checked:=MakeDicParams.AddCharacterIndex;
      fDictImport.cbAddFrequencyInfo.Checked:=MakeDicParams.AddFrequencyInfo;
      fDictImport.Silent := true;
      for i := 0 to Length(MakeDicParams.Files) - 1 do
        fDictImport.lbFiles.Items.Add(MakeDicParams.Files[i]);
      fDictImport.btnBuildClick(self);
      Application.Terminate;
      exit;
    end;

    AutoImportDicts();

   //Force user to select fonts
    fSettings.CheckFontsPresent;

   //Wakan.chr
    try
      ps:=TPackageSource.Create('wakan.chr',791564,978132,978123);
      vi:=TStringList.Create;
      ms:=ps['jalet.ver'].Lock;
      vi.LoadFromStream(ms);
      ps['jalet.ver'].Unlock;

      if (vi[0]<>'JALET.DIC') and (vi[0]<>'JALET.CHR') then
        raise Exception.Create('Unknown DICT.VER header.');
      if strtoint(vi[1])<CurDictVer then
      begin
        Application.MessageBox(
          pchar(_l('#00354^eWAKAN.CHR has old structure. Please download new '
            +'version.'#13#13'Application will now exit.')),
          pchar(_l('#00020^eError')),
          MB_ICONERROR or MB_OK);
        Application.Terminate;
        exit;
      end;
      if strtoint(vi[1])>CurDictVer then
      begin
        Application.MessageBox(
          pchar(_l('#00355^eWAKAN.CHR has newer structure. Please download new '
            +'WAKAN.EXE.'#13#13'Application will now exit.')),
          pchar(_l('#00020^eError')),
          MB_ICONERROR or MB_OK);
        Application.Terminate;
        exit;
      end;
      fStatistics.Label13.Caption:=datetostr(strtoint(vi[2]));
      fStatistics.Label15.Caption:=vi[4];
      fStatistics.Label16.Caption:=vi[5];
      ChinesePresent:=vi[6]='CHINESE';
      vi.Free;
      fSplash.ProgressBar1.Position:=1;
      fSplash.ProgressBar1.Update;
      TChar:=TTextTable.Create(ps,'Char',true,false);
      fSplash.ProgressBar1.Position:=2;
      fSplash.ProgressBar1.Update;
      TCharRead:=TTextTable.Create(ps,'CharRead',true,false);
      fSplash.ProgressBar1.Position:=3;
      fSplash.ProgressBar1.Update;
      TRadicals:=TTextTable.Create(ps,'Radicals',true,false);
      if (fSettings.CheckBox64.Checked) and (fSettings.CheckBox65.Checked) then RebuildAnnotations;
      if (fSettings.CheckBox64.Checked) then LoadAnnotations;
    except
      Application.MessageBox(
        pchar(_l('#00356^eCannot load main dictionary file.'#13
          +'File WAKAN.CHR is corrupted.'#13#13'Application will now exit.')),
        pchar(_l('#00020^eError')),
        MB_OK or MB_ICONERROR);
      Application.Terminate;
      exit;
    end;


   { Radical search }

   //Stroke-order rebuilding -- before complaining about missing sod
    if Command='makerad' then
    begin
     //If no filename is set, assume defaults
      if Length(MakeRadParams.Files)<=0 then begin
        if FileExists('RADKFILE') then AddFilename(MakeRadParams.Files, 'RADKFILE');
       // if FileExists('RADKFILE2') then AddFilename(MakeRadParams.Files, 'RADKFILE2'); //not ready for this, it has chars in EUC we can't handle
        if Length(MakeRadParams.Files)<=0 then
          raise Exception.Create(_l('No RADKFILE is found in the application directory. '
            +'Either put this file there or explicitly specify which files to use to MAKERAD.'));
      end;

      fRadical.BuildRadicalPackage(MakeRadParams.Files);
      Application.Terminate;
      exit;
    end;

   //Auto-rebuild
    if not FileExists('wakan.rad') then begin
      SetLength(MakeRadParams.Files,0);
      if FileExists('RADKFILE') then AddFilename(MakeRadParams.Files, 'RADKFILE');
     // if FileExists('RADKFILE2') then AddFilename(MakeRadParams.Files, 'RADKFILE2'); //see above
      if Length(MakeRadParams.Files)>0 then //else just continue and fail later
        fRadical.BuildRadicalPackage(MakeRadParams.Files);
    end;

   //Radical search
    if not FileExists('wakan.rad') then
    begin
      Application.MessageBox(
        pchar(_l('#00357^eFile WAKAN.RAD was not found.'#13
          +'Japanese advanced radicals search will be disabled.')),
        pchar(_l('#00020^eError')),
        MB_OK or MB_ICONERROR);
      rainesearch:=nil;
    end else
    begin
      try
        ps:=TPackageSource.Create('wakan.rad',791564,978132,978123);
        ms:=ps['search.bin'].Lock;
        GetMem(rainesearch,ms.Size);
        ms.Read(rainesearch^,ms.Size);
        ps['search.bin'].Unlock;
        ms:=ps['radicals.txt'].Lock;
        raineradicals:=TStringList.Create;
        raineradicals.LoadFromStream(ms);
        ps['radicals.txt'].Unlock;
        ps.Free;
      except
        Application.MessageBox(
          pchar(_l('#00358^eCannot load Japanese radicals file.'#13
            +'File WAKAN.RAD is corrupted.'#13#13'Application will now exit.')),
          pchar(_l('#00020^eError')),
          MB_OK or MB_ICONERROR);
        Application.Terminate;
        exit;
      end;
    end;


   { Stroke order display }

   //Stroke-order rebuilding -- before complaining about missing sod
    if Command='makesod' then
    begin
      fWords.BuildStrokeOrderPackage('STROKES.CSV');
      Application.Terminate;
      exit;
    end;

   //Auto-rebuild
    if not FileExists('wakan.sod')
    and FileExists('STROKES.CSV') then
      fWords.BuildStrokeOrderPackage('STROKES.CSV');

   //Stroke-order display
    if not FileExists('wakan.sod') then
    begin
      Application.MessageBox(
        pchar(_l('#00359^eFile WAKAN.SOD was not found.'#13
          +'Japanese stroke-order display will be disabled.')),
        pchar(_l('#00020^eError')),
        MB_OK or MB_ICONERROR);
      sodir:=nil;
      sobin:=nil;
    end else
    begin
      try
        ps:=TPackageSource.Create('wakan.sod',791564,978132,978123);
        ms:=ps['strokes.bin'].Lock;
        GetMem(sobin,ms.Size);
        ms.Read(sobin^,ms.Size);
        ps['strokes.bin'].Unlock;
        ms:=ps['dir.txt'].Lock;
        sodir:=TStringList.Create;
        sodir.LoadFromStream(ms);
        ps['dir.txt'].Unlock;
        ps.Free;
      except
        Application.MessageBox(
          pchar(_l('#00360^eCannot load Japanese stroke-order file.'#13
            +'File WAKAN.SOD is corrupted.'#13#13'Application will now exit.')),
          pchar(_l('#00020^eError')),
          MB_OK or MB_ICONERROR);
        Application.Terminate;
        exit;
      end;
    end;
    StrokeOrderPackage:=nil;


   { User data }
    try
      userdataloaded:=false;
      LoadUserData;
    except
      if FileExists(UserDataDir+'\WAKAN.USR') then Application.MessageBox(
        pchar(_l('#00361^eCannot load user data file.'#13'File WAKAN.USR is corrupted.'#13
          +'If you delete this file, it will be created anew.'#13#13'Application will now exit.')),
        pchar(_l('#00020^eError')),
        MB_OK or MB_ICONERROR)
      else Application.MessageBox(
        pchar(_l('#00362^eUnable to create user data file WAKAN.USR.'#13
          +'Please run this program from a folder that is not read-only.'#13#13
          +'Application will now exit.')),
        pchar(_l('#00020^eError')),
        MB_OK or MB_ICONERROR);
      Application.Terminate;
      exit;
    end;
    if Application.Terminated then exit;

    MaxUserIndex := FindMaxUserIndex();
    MaxCategoryIndex := FindMaxCategoryIndex();

    jromasys:=fSettings.RadioGroup1.ItemIndex+1;
    jshowroma:=fSettings.RadioGroup2.ItemIndex=1;
    cromasys:=fSettings.RadioGroup6.ItemIndex+1;
    cshowroma:=fSettings.RadioGroup7.ItemIndex=1;

   //Prepare for SwitchLanguage->RescanDicts->AutoUpdate(dic)
    if Command='updatedics' then begin
     //Fix filenames now, or they won't match and force updates in AutoUpdate
      AutoFixFilenames(UpdateDicsParams.Files);
      JWBAutoImport.ForceUpdates := true;
      JWBAutoImport.ForceUpdateList := UpdateDicsParams.Files;
    end;

    SwitchLanguage(curlang);
    { SwitchLanguage will do this:
    RescanDicts;
    RefreshCategory;
    RefreshKanjiCategory; }

    if Command='updatedics' then begin
      if Length(UpdateDicsParams.Files)>0 then
        JWBAutoImport.AutoUpdateFiles(UpdateDicsParams.Files);
      Application.Terminate;
      exit; //so this can be run in batch mode
    end;

    fSplash.Hide;
  {  if ((Screen.Width<1024) or (Screen.Height<768)) then
    begin
      fMenu.Constraints.MinHeight:=80;
      fMenu.Constraints.MaxHeight:=80;
      fMenu.Height:=80;
      fMenu.Image1.Top:=0;
    end;}
    curdisplaymode:=0;
    FormPlacement1.RestoreFormPlacement;
    fSettings.ApplyUISettings();

   { Init clipboard viewer }
    CbNextViewer := SetClipboardViewer(Self.Handle);

   { Open file in the editor }
    fTranslate.FileChanged := false;
   //Explicitly specified file
    if Command='open' then begin
      fTranslate.OpenAnyFile(OpenParams.Filename);
     //Press "Editor" programmatically
      tab3.Down := true;
      TabControl1Change(tab3);
    end else
   //Last opened file in Editor
    if (fSettings.CheckBox61.Checked) and (fTranslate.docfilename<>'') then
    try
      fTranslate.OpenFile(fTranslate.docfilename, fTranslate.doctp);
    except
      on E: Exception do begin
       //Re-raise with additional comment
        E.Message := 'Cannot autoload your last-used file: '+E.Message;
        raise;
      end;
    end;

    initdone:=true;
  except
    on E: EBadUsage do begin
      ShowUsage(E.Message);
      Application.Terminate;
    end;
    on E: EAbort do begin
      Application.Terminate; //Silently
    end;
    on E: Exception do begin
      Application.MessageBox(
        pchar('Cannot load Wakan. '+E.Classname+': '#13+E.Message),
        pchar('Error'), //Do not translate! The translation might not even be loaded yet.
        MB_ICONERROR or MB_OK
      );
     //It's better to exit right now than to continue uninitialized.
      Application.Terminate;
    end;
  end;

  Timer1.Enabled:=true;
  Timer1Timer(Timer1);

 { Done. }
end;

procedure TfMenu.LoadWakanCfg(const filename: string);
var sl: TStringList;
  i: integer;
  ln: string;
  sect:integer;
begin
  defll.Clear;
  suffixl.Clear;
  partl.Clear;
  romac.Clear;
  roma_t.Clear;
  SetLength(romasortl, 0);

  sl := TStringList.Create();
  try
    sl.LoadFromFile(filename);
    sect:=0;
    for i := 0 to sl.Count - 1 do begin
      ln := sl[i];
      if (length(ln)>0) and (ln[1]<>';') then
      begin
        if ln[1]='['then
        begin
          delete(ln,length(ln),1);
          delete(ln,1,1);
          if ln='Particles'then sect:=1 else
          if ln='Deflection'then sect:=2 else
          if ln='PinYin'then sect:=4 else
          if ln='CharInfo'then sect:=5 else
          if ln='RomajiSort'then sect:=6 else
          if ln='Suffixes'then sect:=7 else
          if ln='IgnoreWords'then sect:=8 else
          if ln='ReadingChart'then sect:=9 else
          if ln='KnownDictSources' then sect:=10 else
          sect:=0;
        end else
        begin
         //Some of the fields are in hex unicode, so we have to convert them
          if sect=1 then partl.Add(hextofstr(ln));
          if sect=2 then defll.Add(ln);
          if sect=4 then AddPinYinRecord(ln);
          if sect=5 then CharPropTypes.Add(ln);
          if sect=6 then AddRomaSortRecord(ln);
          if sect=7 then suffixl.Add(copy(ln,1,1)+hextofstr(copy(ln,2,Length(ln)-1))); //Format: {type:char}{suffix:fhex}
          if sect=8 then ignorel.Add(fstr(ln));
          if sect=9 then readchl.Add(copy(ln,1,1)+hextofstr(copy(ln,2,Length(ln)-1))); //Format: {type:char}{reading:fhex}
          if sect=10 then KnownDictSources.Add(ln);
        end;
      end;
    end;

  finally
    FreeAndNil(sl);
  end;

  suffixl.Sorted:=true;
  suffixl.Sort;
end;

procedure TfMenu.WriteUserPackage(dir:string);
var f:file of byte;
    b:byte;
begin
  assignfile(f,dir+'\struct.ver');
  rewrite(f);
  b:=CurStructVer;
  write(f,b);
  closefile(f);
  PKGWriteForm.PKGWriteCmd('NotShow');
  PKGWriteForm.PKGWriteCmd('PKGFileName '+UserDataDir+'\wakan.usr');
  PKGWriteForm.PKGWriteCmd('MemoryLimit 100000000');
  PKGWriteForm.PKGWriteCmd('Name WaKan User Data');
  PKGWriteForm.PKGWriteCmd('TitleName WaKan User Data File');
  PKGWriteForm.PKGWriteCmd('CopyrightName '+WakanCopyright);
  PKGWriteForm.PKGWriteCmd('FormatName Pure Package File');
  PKGWriteForm.PKGWriteCmd('CommentName File is used by '+WakanAppName);
  PKGWriteForm.PKGWriteCmd('VersionName 1.0');
  PKGWriteForm.PKGWriteCmd('HeaderCode 621030');
  PKGWriteForm.PKGWriteCmd('FileSysCode 587135');
  PKGWriteForm.PKGWriteCmd('WriteHeader');
  PKGWriteForm.PKGWriteCmd('TemporaryLoad');
  PKGWriteForm.PKGWriteCmd('CryptMode 0');
  PKGWriteForm.PKGWriteCmd('CRCMode 0');
  PKGWriteForm.PKGWriteCmd('PackMode 0');
  PKGWriteForm.PKGWriteCmd('CryptCode 978312');
  PKGWriteForm.PKGWriteCmd('Include '+dir);
  PKGWriteForm.PKGWriteCmd('Finish');
end;

procedure TfMenu.SetUserDataChanged(Value: boolean);
begin
  FUserDataChanged := Value;
  aSaveUser.Enabled:=FUserDataChanged;
  aCancelUser.Enabled:=FUserDataChanged;
end;

procedure TfMenu.ChangeUserData;
begin
  UserDataChanged:=true;
end;


function TfMenu.GetCharValueInt(index,vt:integer):integer;
var s:string;
begin
  s:=GetCharValue(index,vt);
  if (length(s)<>0) and (s[length(s)]='''') then delete(s,length(s),1);
  if (s='') or not TryStrToInt(s, Result) then
    Result:=65535;
end;

function TfMenu.GetCharValueRad(index,vt:integer):integer;
var s:string;
begin
  s:=GetCharValue(index,vt);
  if pos('.',s)>0 then delete(s,pos('.',s),length(s)-pos('.',s)+1);
  if (length(s)<>0) and (s[length(s)]='''') then delete(s,length(s),1);
  if (s='') or not TryStrToInt(s, Result) then
    Result:=65535;
end;

function TfMenu.GetCharValue(index,vt:integer):string;
begin
  TCharRead.SetOrder('');
  if TCharRead.Locate('Kanji',index) then
  while (not TCharRead.EOF) and (TCharRead.Int(TCharReadKanji)=index) do
  begin
    if TCharRead.Int(TCharReadType)=vt then
    begin
      result:=TCharRead.Str(TCharReadReading);
      exit;
    end;
    TCharRead.Next;
  end;
  result:='';
end;

procedure TfMenu.ClearDicts;
var i:integer;
begin
  for i:=0 to dicts.Count-1 do
  begin
    if dicts[i].loaded then
      dicts[i].unload;
    dicts[i].Free;
  end;
  dicts.Clear;
end;

procedure TfMenu.RescanDicts();
var sr:TSearchRec;
    dic:TJaletDic;
begin
  ClearDicts;
  if FindFirst('*.dic',faAnyFile,sr)=0 then
  repeat
    dic := NewDict(sr.name);
    if dic.tested then
    begin
      if Uppercase(dic.pname)='JALET.DIC'then
        Application.MessageBox(
          pchar(_l('#00326^eIt is not recommended to use old style JALET.DIC dictionary.')),
          pchar(_l('#00090^eWarning')),
          MB_ICONWARNING or MB_OK);
      if not initdone then //we're still loading
        AutoUpdate(dic);
      if curlang=dic.language then
      begin
        dicts.Add(dic);
        if not dicts.IsInGroup(dic,GROUP_NOTUSED) then
          dic.Load;
      end;
    end else dic.Free;
  until FindNext(sr)<>0;
  FindClose(sr);

  if dicts.Count=0 then
  begin
    if curlang='j'then
      Application.MessageBox(
        pchar(_l('#00327^eNo valid japanese dictionary was found.'#13
          +'Please download some japanese .DIC files from WAKAN website.')),
        pchar(_l('#00090^eWarning')),
        MB_ICONWARNING or MB_OK)
    else
      Application.MessageBox(
        pchar(_l('#00328^eNo valid chinese dictionary was found.'#13
          +'Please download some chinese .DIC files from WAKAN website.')),
          pchar(_l('#00090^eWarning')),
          MB_ICONWARNING or MB_OK);
  end;
end;

{
Creates a new standardly configured dictionary object from a specified file.
Applies all default settings such as Offline/LoadOnDemand per dict settings.
}
function TfMenu.NewDict(dicname: string): TJaletDic;
begin
  Result:=TJaletDic.Create;
  Result.OnLoadStart := DicLoadStart;
  Result.OnLoadEnd := DicLoadEnd;
  Result.OnLoadException := DicLoadException;
  Result.LoadOnDemand := fSettings.CheckBox49.Checked;
  Result.Offline := dicts.IsInGroup(dicname,GROUP_OFFLINE);
  try
    Result.FillInfo(dicname);
  except
    Application.MessageBox(
      pchar(_l('#00321^eCannot register dictionary ')+dicname+#13#13+(ExceptObject as Exception).Message),
      pchar(_l('#00020^eError')),
      MB_ICONERROR or MB_OK);
  end;
end;

procedure TfMenu.DicLoadStart(Sender: TObject);
begin
  DicLoadPrompt.Free; //just in case
  DicLoadPrompt := SMMessageDlg(
    _l('#00323^eDictionary loading'),
    _l('#00324^eLoading dictionary ')+TJaletDic(Sender).name+'...');
end;

procedure TfMenu.DicLoadEnd(Sender: TObject);
begin
  FreeAndNil(DicLoadPrompt);
end;

procedure TfMenu.DicLoadException(Sender: TObject; E: Exception);
begin
  Application.MessageBox(
    pchar(_l('#00325^eCannot load dictionary ')+TJaletDic(Sender).name+#13#13+E.Message),
    pchar(_l('#00020^eError')),
    MB_ICONERROR or MB_OK);
end;

procedure TfMenu.SwitchLanguage(lanchar:char);
begin
  curlang:=lanchar;
  if lanchar='j'then
  begin
    romasys:=fSettings.RadioGroup1.ItemIndex+1;
    showroma:=fSettings.RadioGroup2.ItemIndex=1;
    btnJapaneseMode.Down:=true;
    aJapanese.Checked:=true;
    aChinese.Checked:=false;
    fUser.btnLookupJtoE.Caption:=_l('#00329^eJapanese ->English');
    fUser.btnLookupEtoJ.Caption:=_l('#00330^eEnglish -> Japanese');
  end else
  begin
    romasys:=fSettings.RadioGroup6.ItemIndex+1;
    showroma:=fSettings.RadioGroup7.ItemIndex=1;
    btnChineseMode.Down:=true;
    aJapanese.Checked:=false;
    aChinese.Checked:=true;
    fUser.btnLookupJtoE.Caption:=_l('#00331^eChinese ->English');
    fUser.btnLookupEtoJ.Caption:=_l('#00332^eEnglish -> Chinese');
  end;
  RescanDicts;
  fKanji.KanjiSearch_SpeedButton20Click(self);
  if (not fUser.btnLookupClip.Enabled) and (fUser.btnLookupClip.Down) then fUser.btnLookupJtoE.Down:=true;
  fExamples.ReloadExamples;
  fUser.Look();
  RefreshCategory;
  RefreshKanjiCategory;
end;

procedure TfMenu.TranslateAll;
var i: integer;
begin
  for i := 0 to Screen.FormCount - 1 do
    fLanguage.TranslateForm(Screen.Forms[i]);
end;

procedure TfMenu.RefreshCategory;
var b:boolean;
    lc:char;
    s:string;
begin
  fDicAdd.ComboBox1.Items.Clear;
  fUserAdd.ComboBox1.Items.Clear;
  fUserDetails.cbAddCategory.Items.Clear;
  fUserFilters.tabCatListChange(fMenu,fUserFilters.tabCatList.TabIndex,b);

  TUserCat.First;
  while not TUserCat.EOF do
  begin
    s:=TUserCat.Str(TUserCatName);
    lc:=GetCatPrefix(s);
    if lc='?' then begin
      lc := 'j';
      TUserCat.Edit([TUserCatName],['j~'+s])
    end;
    s:=StripCatName(s);
    if lc=curlang then
    begin
      fDicAdd.ComboBox1.Items.Add(s);
      fUserAdd.ComboBox1.Items.Add(s);
      fUserDetails.cbAddCategory.Items.Add(s);
    end;
    TUserCat.Next;
  end;

  if fDicAdd.ComboBox1.Items.Count>0 then fDicAdd.ComboBox1.Text:=fDicAdd.ComboBox1.Items[0];
  if fUserAdd.ComboBox1.Items.Count>0 then fUserAdd.ComboBox1.Text:=fUserAdd.ComboBox1.Items[0];
end;

procedure TfMenu.RefreshKanjiCategory;
begin
  ReloadKanjiCategories();
  PasteKanjiCategoriesTo(fKanjiDetails.cbCategories.Items);
  PasteKanjiCategoriesTo(fKanjiSearch.lbCategories.Items);
  fKanjiDetails.cbCategories.ItemIndex:=0;
  fKanjiSearch.lbCategories.ItemIndex:=0;
  fKanjiSearch.lbCategoriesClick(Self); //react to changes
end;

procedure TfMenu.SaveUserData;
var un,i:integer;
  tempDir: string;
begin
  if not UserDataChanged then exit;

  CopyFile(PChar(UserDataDir+'\wakan.usr'),PChar(UserDataDir+'\wakan.bak'),false);
  ReloadKanjiCategories(); //in case they weren't loaded which shouldn't happen
  Screen.Cursor:=crHourGlass;
  tempDir := CreateRandomTempDirName();
  ForceDirectories(tempDir);
  for i:=0 to Length(KanjiCats)-1 do
  begin
    un:=KanjiCats[i].idx;
    if un=KnownLearned then
      SaveKnownList(un,tempDir+'\knownchar.bin')
    else
      SaveKnownList(un,tempDir+'\char'+inttostr(un)+'.bin');
  end;
  TUser.WriteTable(tempDir+'\User',false);
  TUserIdx.WriteTable(tempDir+'\UserIdx',false);
  TUserSheet.WriteTable(tempDir+'\UserSheet',false);
  TUserCat.WriteTable(tempDir+'\UserCat',false);
  TUserPrior.WriteTable(tempDir+'\UserPrior',false);
  WriteUserPackage(tempDir);
  DeleteDirectory(tempDir);
  Backup(UserDataDir+'\wakan.usr');
  Screen.Cursor:=crDefault;
  UserDataChanged:=false;
  LastSaveTime:=now;
end;

procedure TfMenu.LoadUserData;
var tempDir: string;
  t:textfile;
  ps:TPackageSource;
  ms:TMemoryStream;
  ver:integer;
  CatIdx:integer;
  CatName: string;
  CatType: char;
begin
  UserDataChanged:=false;
  aSaveUser.Enabled:=false;
  aCancelUser.Enabled:=false;
  Screen.Cursor:=crHourGlass;
  if userdataloaded then
  begin
    TUser.Free;
    TUserIdx.Free;
    TUserSheet.Free;
    TUserCat.Free;
    userdataloaded:=false;
  end;

  if not FileExists(UserDataDir+'\wakan.usr') then
  begin
    tempDir := CreateRandomTempDir();
    CreateKnownList(1,0);
    SaveKnownList(1,tempDir+'\knownchar.bin');
    assignfile(t,tempDir+'\User.info');
    rewrite(t);
    writeln(t,'$TEXTTABLE');
    writeln(t,'$PREBUFFER');
    writeln(t,'$FIELDS');
    writeln(t,'iIndex');
    writeln(t,'sEnglish');
    writeln(t,'xPhonetic');
    writeln(t,'sPhoneticSort');
    writeln(t,'xKanji');
    writeln(t,'sAdded');
    writeln(t,'sPrinted');
    writeln(t,'sLearned');
    writeln(t,'sMastered');
    writeln(t,'iNoPrinted');
    writeln(t,'bScore');
    writeln(t,'bMaxScore');
    writeln(t,'$ORDERS');
    writeln(t,'Index_Ind');
    writeln(t,'Kanji_Ind');
    writeln(t,'Phonetic_Ind');
    writeln(t,'PhoneticSeek_Ind');
    writeln(t,'English_Ind');
    writeln(t,'Added_Ind');
    writeln(t,'Printed_Ind');
    writeln(t,'Score_Ind');
    writeln(t,'$SEEKS');
    writeln(t,'0');
    writeln(t,'Index');
    writeln(t,'Kanji+PhoneticSort');
    writeln(t,'PhoneticSort+Phonetic');
    writeln(t,'Phonetic');
    writeln(t,'English');
    writeln(t,'Added+PhoneticSort');
    writeln(t,'Printed+PhoneticSort');
    writeln(t,'Score+PhoneticSort');
    writeln(t,'$CREATE');
    closefile(t);
    assignfile(t,tempDir+'\UserIdx.info');
    rewrite(t);
    writeln(t,'$TEXTTABLE');
    writeln(t,'$PREBUFFER');
    writeln(t,'$FIELDS');
    writeln(t,'iWord');
    writeln(t,'xKanji');
    writeln(t,'lBegin');
    writeln(t,'$ORDERS');
    writeln(t,'Kanji_Ind');
    writeln(t,'$SEEKS');
    writeln(t,'0');
    writeln(t,'Kanji');
    writeln(t,'$CREATE');
    closefile(t);
    assignfile(t,tempDir+'\UserSheet.info');
    rewrite(t);
    writeln(t,'$TEXTTABLE');
    writeln(t,'$PREBUFFER');
    writeln(t,'$FIELDS');
    writeln(t,'iWord');
    writeln(t,'wNumber');
    writeln(t,'wPos');
    writeln(t,'$ORDERS');
    writeln(t,'Word_Ind');
    writeln(t,'Sheet_Ind');
    writeln(t,'$SEEKS');
    writeln(t,'0');
    writeln(t,'Word+Number+Pos');
    writeln(t,'Number+Pos');
    writeln(t,'$CREATE');
    closefile(t);
    assignfile(t,tempDir+'\UserCat.info');
    rewrite(t);
    writeln(t,'$TEXTTABLE');
    writeln(t,'$PREBUFFER');
    writeln(t,'$FIELDS');
    writeln(t,'iIndex');
    writeln(t,'sName');
    writeln(t,'bType');
    writeln(t,'sCreated');
    writeln(t,'$ORDERS');
    writeln(t,'Index_Ind');
    writeln(t,'Type_Ind');
    writeln(t,'Name_Ind');
    writeln(t,'Created_Ind');
    writeln(t,'$SEEKS');
    writeln(t,'0');
    writeln(t,'Index');
    writeln(t,'Type+Name');
    writeln(t,'Name');
    writeln(t,'Created+Name');
    writeln(t,'$CREATE');
    closefile(t);
    assignfile(t,tempDir+'\UserPrior.info');
    rewrite(t);
    writeln(t,'$TEXTTABLE');
    writeln(t,'$PREBUFFER');
    writeln(t,'$FIELDS');
    writeln(t,'xKanji');
    writeln(t,'wCount');
    writeln(t,'$ORDERS');
    writeln(t,'Kanji_Ind');
    writeln(t,'Count_Ind');
    writeln(t,'$SEEKS');
    writeln(t,'0');
    writeln(t,'Kanji');
    writeln(t,'Count');
    writeln(t,'$CREATE');
    closefile(t);
    WriteUserPackage(tempDir);
    DeleteDirectory(tempDir);
  end;

  ps:=TPackageSource.Create(UserDataDir+'\wakan.usr',621030,587135,978312);
  ver:=0;
  TUser:=TTextTable.Create(ps,'User',false,false);
  TUserIdx:=TTextTable.Create(ps,'UserIdx',false,false);
  TUserSheet:=TTextTable.Create(ps,'UserSheet',false,false);
  TUserCat:=TTextTable.Create(ps,'UserCat',false,false);
  if ps['UserPrior.info']<>nil then
    TUserPrior:=TTextTable.Create(ps,'UserPrior',false,false) else
  begin
    tempDir := CreateRandomTempDir();
    assignfile(t,tempDir+'\UserPrior.info');
    rewrite(t);
    writeln(t,'$TEXTTABLE');
    writeln(t,'$PREBUFFER');
    writeln(t,'$FIELDS');
    writeln(t,'xKanji');
    writeln(t,'wCount');
    writeln(t,'$ORDERS');
    writeln(t,'Kanji_Ind');
    writeln(t,'Count_Ind');
    writeln(t,'$SEEKS');
    writeln(t,'0');
    writeln(t,'Kanji');
    writeln(t,'Count');
    writeln(t,'$CREATE');
    closefile(t);
    TUserPrior:=TTextTable.Create(nil,tempDir+'\UserPrior',false,false);
    DeleteDirectory(tempDir);
  end;
  TCharIndex:=TChar.Field('Index');
  TCharChinese:=TChar.Field('Chinese');
  TCharType:=TChar.Field('Type');
  TCharUnicode:=TChar.Field('Unicode');
  TCharStrokeCount:=TChar.Field('StrokeCount');
  TCharJpStrokeCount:=TChar.Field('JpStrokeCount');
  TCharJpFrequency:=TChar.Field('JpFrequency');
  TCharChFrequency:=TChar.Field('ChFrequency');
  TCharJouyouGrade:=TChar.Field('JouyouGrade');
  TCharReadKanji:=TCharRead.Field('Kanji');
  TCharReadType:=TCharRead.Field('Type');
  TCharReadReading:=TCharRead.Field('Reading');
  TCharReadIndex:=TCharRead.Field('Index');
  TCharReadReadDot:=TCharRead.Field('ReadDot');
  TCharReadPosition:=TCharRead.Field('Position');
  TRadicalsNumber:=TRadicals.Field('Number');
  TRadicalsVariant:=TRadicals.Field('Variant');
  TRadicalsUnicode:=TRadicals.Field('Unicode');
  TRadicalsStrokeCount:=TRadicals.Field('StrokeCount');
  TRadicalsUnicodeCount:=TRadicals.Field('UnicodeCount');
  TRadicalsBushuCount:=TRadicals.Field('BushuCount');
  TRadicalsJapaneseCount:=TRadicals.Field('JapaneseCount');
  TRadicalsKangXiCount:=TRadicals.Field('KangXiCount');
  TUserIndex:=TUser.Field('Index');
  TUserEnglish:=TUser.Field('English');
  TUserPhonetic:=TUser.Field('Phonetic');
  TUserPhoneticSort:=TUser.Field('PhoneticSort');
  TUserKanji:=TUser.Field('Kanji');
  TUserAdded:=TUser.Field('Added');
  TUserPrinted:=TUser.Field('Printed');
  TUserLearned:=TUser.Field('Learned');
  TUserMastered:=TUser.Field('Mastered');
  TUserNoPrinted:=TUser.Field('NoPrinted');
  TUserScore:=TUser.Field('Score');
  TUserMaxScore:=TUser.Field('MaxScore');
  TUserIdxWord:=TUserIdx.Field('Word');
  TUserIdxKanji:=TUserIdx.Field('Kanji');
  TUserIdxBegin:=TUserIdx.Field('Begin');
  TUserIdxIndex:=TUserIdx.Field('Index');
  TUserSheetWord:=TUserSheet.Field('Word');
  TUserSheetNumber:=TUserSheet.Field('Number');
  TUserSheetPos:=TUserSheet.Field('Pos');
  TUserCatIndex:=TUserCat.Field('Index');
  TUserCatName:=TUserCat.Field('Name');
  TUserCatType:=TUserCat.Field('Type');
  TUserCatCreated:=TUserCat.Field('Created');

  KnownLearned:=-1; //not found
  TUserCat.First;
  while not TUserCat.EOF do
  begin
    CatIdx:=strtoint(TUserCat.Str(TUserCatIndex));
    CatName:=TUserCat.Str(TUserCatName);
    CatType:=chr(TUserCat.Int(TUserCatType));

    if CatType='Q' then
    begin
     //First Q category is selected as LEARNED, rest are bugs and are ignored here.
     //But we still load them, that'd do us no harm and simplify processing later.
      if KnownLearned<0 then
        KnownLearned:=CatIdx;
      ms:=ps['knownchar.bin'].Lock;
      CreateKnownList(CatIdx,0);
      LoadKnownList(CatIdx,ms);
      ps['knownchar.bin'].Unlock;
    end else
    if CatType='K' then
    begin
      ms:=ps['char'+inttostr(CatIdx)+'.bin'].Lock;
      CreateKnownList(CatIdx,0);
      LoadKnownList(CatIdx,ms);
      ps['char'+inttostr(CatIdx)+'.bin'].Unlock;
    end;

    TUserCat.Next;
  end;

  if FixDuplicateCategories() then begin //have to do this after we (perhaps) found the KnownLearned
    UserDataChanged := true;
    Application.MessageBox(
      PChar(_l('^eYour user data contained duplicate kanji/word groups. This is a result '
      +'of a bug in older versions of Wakan.'#13
      +'The data has been repaired and duplicates have been merged. Please check '
      +'your user groups, save user data, and you should never see this message again.')),
      PChar(_l('User data repaired')),
      MB_ICONINFORMATION or MB_OK
    );
  end;

 //Add "LEARNED" category, if missing
  if KnownLearned<0 then
  begin
    KnownLearned := FindMaxCategoryIndex()+1; //can't use CatIdx since we want MAX index, not the LAST one
    TUserCat.Insert([IntToStr(KnownLearned), 'k~'+_l('LEARNED'), inttostr(ord('Q')), FormatDateTime('yyyymmdd',now)]);
    ms:=ps['knownchar.bin'].Lock;
    CreateKnownList(KnownLearned,0);
    KnownLearned:=KnownLearned;
    LoadKnownList(KnownLearned,ms);
    ps['knownchar.bin'].Unlock;
    UserDataChanged := true;
  end;

  try
    ms:=ps['struct.ver'].Lock;
    ms.Read(ver,1);
    ps['struct.ver'].UnLock;
  except end;
  ps.Free;
  if ver<>CurStructVer then
  begin
    if Application.MessageBox(
      pchar(_l('#00338^eFile WAKAN.USR has old structure.'#13'It must be converted.'#13
        +'You should make backup before converting'#13#13'Do you want to convert it now?')),
      pchar(_l('#00339^eOld structure')),
      MB_YESNO or MB_ICONWARNING)=idYes then
    begin
      if ver<=0 then
      begin
        tempDir := CreateRandomTempDir();
        try
          ExportUserData(tempDir+'\wakan_temp.jbk');
          ImportUserData(tempDir+'\wakan_temp.jbk');
          DeleteFile(tempDir+'\wakan_temp.jbk');
        finally
          DeleteDirectory(tempDir);
        end;
      end;
      if ver<=1 then
      begin
        RebuildUserIndex;
        ChangeUserData;
        SaveUserData;
      end;
    end else begin
      Application.Terminate;
      exit;
    end;
  end;

  if not TUser.CheckIndex then begin
    TUser.Reindex;
    UserDataChanged:=true; //or we'd be reindexing each load
  end;
  if not TUserIdx.CheckIndex then begin
    TUserIdx.Reindex;
    UserDataChanged:=true;
  end;
  if not TUserSheet.CheckIndex then begin
    TUserSheet.Reindex;
    UserDataChanged:=true;
  end;
  if not TUserCat.CheckIndex then begin
    TUserCat.Reindex;
    UserDataChanged:=true;
  end;

  //Refresh everything
  RefreshCategory;
  RefreshKanjiCategory;
  fKanjiDetails.RefreshDetails;

  Screen.Cursor:=crDefault;
  UserDataLoaded:=true;
end;

procedure TfMenu.ExportUserData(filename:string);
var t:TCustomFileWriter;
  i:integer;
  w:widechar;
begin
  if not FlushUserData then exit;
  //User data is stored in Ansi, because compability.
  t := TAnsiFileWriter.Rewrite(filename);
  t.Writeln('$User');
  TUser.ExportToText(t,'Index_Ind');
  t.Writeln('$UserIdx');
  TUserIdx.ExportToText(t,'Kanji_Ind');
  t.Writeln('$UserCat');
  TUserCat.ExportToText(t,'Index_Ind');
  t.Writeln('$UserSheet');
  TUserSheet.ExportToText(t,'Sheet_Ind');
  t.Writeln('$KnownKanji');
  t.Writeln('>Unicode');
  for i:=1 to 65536 do
  begin
    w:=widechar(i);
    if IsKnown(KnownLearned,{$IFDEF UNICODE}w{$ELSE}UnicodeToHex(w){$ENDIF}) then
      t.Writeln('+'+UnicodeToHex(w));
  end;
  t.Writeln('.');
  t.Free;
end;

procedure TfMenu.ImportUserData(filename:string);
var t:TCustomFileReader;
  s:string;
begin
  DeleteFile(UserDataDir+'\wakan.usr');
  LoadUserData;
  Screen.Cursor:=crHourGlass;
 //User data is stored in Ansi, because compability.
  t := TAnsiFileReader.Create(filename);
  while not t.Eof() do
  begin
    s := t.ReadLn();
    if s[1]='$'then
    begin
      if s='$User'then TUser.ImportFromText(t,nil,'');
      if s='$UserIdx'then TUserIdx.ImportFromText(t,nil,'');
      if s='$UserCat'then TUserCat.ImportFromText(t,nil,'');
      if s='$UserSheet'then TUserSheet.ImportFromText(t,nil,'');
      if s='$KnownKanji'then
      begin
        t.ReadLn();
        s := t.ReadLn();
        while s[1]<>'.'do
        begin
          delete(s,1,1);
          SetKnown(KnownLearned,s,true);
          s := t.ReadLn();
        end;
      end;
    end;
  end;
  t.Free;
  ChangeUserData;
  SaveUserData;
  MaxUserIndex := FindMaxUserIndex();
  MaxCategoryIndex := FindMaxCategoryIndex();
  RefreshCategory;
  Screen.Cursor:=crDefault;
end;

function TfMenu.FlushUserData:boolean;
var res:integer;
begin
  result:=true;
  if not UserDataChanged then
    exit;

  if fSettings.CheckBox46.Checked then
    SaveUserData
  else
  begin
    res:=Application.MessageBox(
      pchar(_l('#00340^eUser data was changed. Do you want to save it?')),
      pchar(_l('#00341^eApplication exit')),
      MB_YESNOCANCEL or MB_ICONQUESTION);
    case res of
      idYes:SaveUserData;
      idNo:UserDataChanged:=false; //do not save
      idCancel:Result:=false;
    end;
  end;
end;

procedure TfMenu.AutosaveUserData; //check if it's time for autosave
var curt: TDatetime;
  AutoSavePeriod:integer;
begin
  if not fSettings.cbAutosave.Checked then exit;
  curt:=now-LastSaveTime;
  if not TryStrToInt(fSettings.edtAutoSavePeriod.Text, AutoSavePeriod) then
    AutoSavePeriod := DefaultAutoSavePeriod;
  if curt>1/24/60*AutoSavePeriod then
    SaveUserData; //updates LastSaveTime
end;

procedure TfMenu.RebuildUserIndex;
begin
  TUserIdx.First;
  while not TUserIdx.EOF do
  begin
    if not TUser.Locate('Index',TUserIdx.TrueInt(TUserIdxWord)) then
      TUserIdx.Delete;
    TUserIdx.Next;
  end;
end;

procedure TfMenu.BitBtn1Click(Sender: TObject);
begin
  Close;
end;

procedure TfMenu.FormClose(Sender: TObject; var Action: TCloseAction);
begin
 { Disconnect from clipboard viewer }
  ChangeClipboardChain(Self.Handle, CbNextViewer);

  if not FlushUserData then Action:=caNone;
  if not fTranslate.CommitFile then Action:=caNone;
  FormPlacement1.SaveFormPlacement;
  if Action<>caNone then
  begin
    if fExamples.btnDisplayTranslation.Down then exmode:=0;
    if fExamples.btnUseBigFont.Down then exmode:=1;
    if fExamples.btnUseSmallFont.Down then exmode:=2;
    if SpeedButton1.Down then
    begin
      Screen.Cursor:=crHourGlass;
{MCH      UninjectLibrary(ALL_SESSIONS or SYSTEM_PROCESSES, 'wakanh.dll');}
      Screen.Cursor:=crDefault;
    end;
    fSettings.SaveSettings;
    fTranslate.Close;
    fUser.Close;
    fWords.Close;
    fKanji.Close;
  end;
end;

procedure TfMenu.WmChangeCbChain(var Msg: TMessage);
begin
  if HWND(Msg.wParam)=CbNextViewer then begin
   // Replace next window and return
    CbNextViewer := Msg.lParam;
    Msg.Result := 0;
  end else
   // Pass to the next window
    if CbNextViewer<>0 then
      Msg.Result := SendMessage(CbNextViewer, Msg.Msg, Msg.wParam, Msg.lParam);
end;

procedure TfMenu.WmDrawClipboard(var Msg: TMessage);
begin
  Clipboard_Update;
  if CbNextViewer<>0 then
    SendMessage(CbNextViewer, Msg.Msg, Msg.wParam, Msg.lParam); //call next viewer
end;

procedure TfMenu.Clipboard_Paint;
begin
  PaintBox3.Canvas.Brush.Color:=clWindow;
  PaintBox3.Canvas.Font.Color:=clWindowText;
  DrawUnicode(PaintBox3.Canvas,1,1,22,copy(clip,1,200),FontRadical);
end;

procedure TfMenu.Clipboard_Update;
var i:integer;
  h:boolean;
  newclip:FString;
  MyHandle:THandle;
  textptr:PWideChar;
  s:widestring;
begin
  if critsec then exit;
  critsec:=true;
  try
    Clipboard.Open;
    h:=false;
    for i:=0 to Clipboard.FormatCount-1 do
      if Clipboard.Formats[i]=CF_UNICODETEXT then h:=true;
    if h then
    begin
      MyHandle:=Clipboard.GetAsHandle(CF_UNICODETEXT);
      TextPtr:=GlobalLock(MyHandle);
      s:=textptr;
//      if length(s)>64000 then s:=_l('#00342^eToo much data.');
      newclip := fstr(s);
      GlobalUnlock(MyHandle);
      oldhandle:=MyHandle;
    end;
    Clipboard.Close;
  except
    newclip := {$IFDEF UNICODE}'ERROR'{$ELSE}UnicodeToHex('ERROR'){$ENDIF};
  end;
  if newclip<>clip then
  begin
    clip := newclip;
    PaintBox3.Invalidate;
    fUserAdd.PaintBox2.Invalidate;
    if (fKanji.Visible) and (fKanjiSearch.btnInClipboard.Down) then fKanji.DoIt;
    if (fUser.Visible) and (fUser.btnLookupClip.Down) then fUser.Look();
  end;
  critsec:=false;
end;

procedure TfMenu.Clipboard_Clear;
begin
  clip:='';
  ResetClipboard;
  PublishClipboard;
end;

procedure TfMenu.SetClipboard;
begin
  ResetClipboard;
  try
    AddToClipboard(CF_UNICODETEXT, clip)
  finally
    PublishClipboard;
  end;
end;

//Open the clipboard and clear it
procedure TfMenu.ResetClipboard;
begin
  OpenClipboard(Handle);
  EmptyClipboard;
end;

//Does not add terminating null
procedure TfMenu.AddToClipboard(uFormat: UINT; data: pointer; size: integer);
var
  DataHandle :  THandle;
  ToPointer  :  Pointer;
begin
  DataHandle := GlobalAlloc(GMEM_DDESHARE OR GMEM_MOVEABLE, size);
  ToPointer := GlobalLock(DataHandle);
  if size>0 then
    Move(data^, ToPointer^, size); //die if data is invalid
  GlobalUnlock(DataHandle);
  SetClipboardData(uFormat, DataHandle);
end;

procedure TfMenu.AddToClipboard(uFormat: UINT; text: RawByteString);
begin
  if Length(text)>0 then
    AddToClipboard(uFormat, pointer(text), Length(text)+1); //string + term 0
end;

procedure TfMenu.AddToClipboard(uFormat: UINT; text: UnicodeString);
begin
  if Length(text)>0 then
    AddToClipboard(uFormat, pointer(text), Length(text)*2 + 2); //string + term 0
end;

procedure TfMenu.AddToClipboard(uFormat: UINT; data: TMemoryStream; AOwnsStream: boolean = false);
begin
  AddToClipboard(uFormat,data.Memory,data.Size);
  if AOwnsStream then FreeAndNil(data);
end;

procedure TfMenu.PublishClipboard;
begin
  CloseClipboard;
  ClipboardChanged;
end;

procedure TfMenu.ClipboardChanged;
begin
  PaintBox3.Invalidate;
  fUserAdd.PaintBox2.Invalidate;

  if (fKanji.Visible) and (fKanjiSearch.btnInClipboard.Down) then fKanji.DoIt;
  if (fUser.Visible) and (fUser.btnLookupClip.Down) then fUser.Look();
end;

//Retrieves a data for an HGLOBAL-containing clipboard format (most of them are)
function TfMenu.GetClipboard(uFormat: UINT; out ms: TMemoryStream): boolean;
var h: THandle;
  pb: PByte;
  sz: integer;
  format: cardinal;
begin
  OpenClipboard(Handle);
  try
    format := 0;
    repeat
      format := EnumClipboardFormats(format);
    until (format=0) or (format=uFormat);

    if format=0 then begin
      Result := false;
      exit;
    end;

    h := GetClipboardData(uFormat);
    if h=0 then RaiseLastOsError();

    pb := GlobalLock(h);
    if pb=nil then RaiseLastOsError();

    sz := GlobalSize(h);
    if sz>MaxWord then sz := MaxWord; //won't accept more
    
    ms := TMemoryStream.Create;
    ms.Write(pb^,sz);
    
    GlobalUnlock(h);
    Result := true;
  finally
    CloseClipboard();
  end;
end;

procedure TfMenu.SpeedButton2Click(Sender: TObject);
begin
  screenModeWk:=SpeedButton2.Down;
end;

procedure TfMenu.SpeedButton7Click(Sender: TObject);
begin
  LoadUserData;
end;

procedure TfMenu.tab1Click(Sender: TObject);
begin
  ToggleForm(fKanji,nil,nil);
end;

procedure TfMenu.tab2Click(Sender: TObject);
begin
  ToggleForm(fUser,nil,nil);
end;

procedure TfMenu.tab5Click(Sender: TObject);
begin
  ToggleForm(fWords,nil,nil);
end;

procedure TfMenu.Timer1Timer(Sender: TObject);
begin
  Clipboard_Update;
end;

procedure TfMenu.btnJapaneseModeClick(Sender: TObject);
begin
  SwitchLanguage('j');
end;

procedure TfMenu.btnChineseModeClick(Sender: TObject);
begin
  SwitchLanguage('c');
end;

procedure TfMenu.Action1Execute(Sender: TObject);
begin
  fUser.btnLookupEtoJ.Down:=true;
end;

procedure TfMenu.PaintBox3Paint(Sender: TObject);
begin
  Clipboard_Paint;
end;

procedure TfMenu.SpeedButton22Click(Sender: TObject);
begin
  Clipboard_Clear;
end;

procedure TfMenu.aSaveUserExecute(Sender: TObject);
begin
  SaveUserData;
end;

procedure TfMenu.aCancelUserExecute(Sender: TObject);
begin
  LoadUserData;
end;

procedure TfMenu.aStatisticsExecute(Sender: TObject);
begin
  fWords.DoStatistic;
end;

procedure TfMenu.aVocabExportExecute(Sender: TObject);
begin
  fWords.ExportVocab;
end;

procedure TfMenu.aVocabImportExecute(Sender: TObject);
begin
  fWords.ImportVocab;
end;

procedure TfMenu.aExitExecute(Sender: TObject);
begin
  Close;
end;

//switch between resizable window with borders and menu
//and fullscreen borderless, menuless mode
procedure TfMenu.aFullscreenModeExecute(Sender: TObject);
{$J+} //writeable constants on
const
  rect: TRect = (Left:0; Top:0; Right:0; Bottom:0);
  ws : TWindowState = wsNormal;
{$J-} //writeable constants off
var
  r : TRect;
begin
  if BorderStyle <> bsNone then
  begin
    ws := WindowState;
    rect := BoundsRect;
		fmenu.Menu := nil;	//hide menu
    BorderStyle := bsNone;
    r := Screen.MonitorFromWindow(Handle).BoundsRect;
    SetBounds(r.Left, r.Top, r.Right-r.Left, r.Bottom-r.Top) ;
  end
  else
  begin
  	fmenu.Menu := fmenu.MainMenu1; //show menu
    BorderStyle := bsSizeable;
    if ws = wsMaximized then
      WindowState := wsMaximized
    else
      SetBounds(rect.Left, rect.Top, rect.Right-rect.Left, rect.Bottom-rect.Top) ;
  end;
end;

procedure TfMenu.aKanjiExecute(Sender: TObject);
begin
  displaymode:=1;
  ChangeDisplay;
end;

procedure TfMenu.aDictExecute(Sender: TObject);
begin
  displaymode:=2;
  ChangeDisplay;
end;

procedure TfMenu.aUserExecute(Sender: TObject);
begin
  displaymode:=5;
  ChangeDisplay;
end;

procedure TfMenu.aKanjiSearchExecute(Sender: TObject);
var pre:boolean;
begin
  pre:=aKanjiSearch.Checked;
  if not fKanji.Visible then ToggleForm(fKanji,nil,nil);
  if aKanjiSearch.Checked<>pre then exit;
  ToggleForm(fKanjiSearch,fKanji.btnSearchSort,aKanjiSearch);
end;

{ Changes the mode of KanjiDetails window: docked or free-floating }
procedure TfMenu.SetCharDetDocked(Value: boolean; Loading: boolean);
begin
  if (not Loading) and (FCharDetDocked=Value) then exit;
  FCharDetDocked := Value;
  if Value then begin
    fKanjiDetails.SetDocked(Value,Loading);
    if not Loading then begin
      CharDetDockedVis1:=true;
      CharDetDockedVis2:=true;
      ChangeDisplay; //docks it and shows if on appropriate page
    end;
  end else begin
    if not Loading then
      DockExpress(fKanjiDetails,false); //hides and undocks it
    fKanjiDetails.SetDocked(false, Loading);
    if not Loading then
      aKanjiDetails.Execute; //shows it as free floating
  end;
end;

{ Shows/hides KanjiDetails, whether it's in free-floating mode or not.
 In docked mode this appropriately updates CharDetDockedVis*  }
procedure TfMenu.aKanjiDetailsExecute(Sender: TObject);
begin
  if CharDetDocked then
  begin
    if curdisplaymode in [2,5] then
    begin
     //Make it undocked unvisible to hide dock panel
      SetCharDetDocked(false,false);
     //This is probably wrong because SetCharDetDocked will call aKanjiDetails.Execute again
    end else
    begin
      if curdisplaymode=1 then
        CharDetDockedVis1:=not CharDetDockedVis1
      else
        CharDetDockedVis2:=not CharDetDockedVis2;
      ChangeDisplay;
    end;
  end;
  if not CharDetDocked then
  begin
   //In free-floating mode just show or hide it
    if fKanjiDetails.Visible then
      fKanjiDetails.Visible := false
    else
     //We have to be careful if this form has just been undocked and it's window
     //is not yet properly recreated on first show.
     //This requires special treatment:
      UndockedMakeVisible(fKanjiDetails);
  end;
  fMenu.aKanjiDetails.Checked:=fKanjiDetails.Visible;
  fKanji.btnKanjiDetails.Down:=fKanjiDetails.Visible;
  fTranslate.sbDockKanjiDetails.Down:=fKanji.btnKanjiDetails.Down;
end;

procedure TfMenu.aKanjiCompoundsExecute(Sender: TObject);
var pre:boolean;
begin
  pre:=aKanjiCompounds.Checked;
  if not fKanji.Visible then ToggleForm(fKanji,nil,nil);
  if aKanjiCompounds.Checked<>pre then exit;
  ToggleForm(fKanjiCompounds,fKanji.btnCompounds,aKanjiCompounds);
end;

procedure TfMenu.aKanjiPrintExecute(Sender: TObject);
begin
  fKanji.btnPrintCardsClick(Sender);
end;

procedure TfMenu.aDictDetailsExecute(Sender: TObject);
var pre:boolean;
begin
  pre:=aDictDetails.Checked;
  if not fUser.Visible then ToggleForm(fUser,nil,nil);
  if aDictDetails.Checked<>pre then exit;
  ToggleForm(fWordDetails,fUser.SpeedButton5,aDictDetails);
end;

procedure TfMenu.aDictKanjiExecute(Sender: TObject);
var pre:boolean;
begin
  pre:=aDictKanji.Checked;
  if not fUser.Visible then ToggleForm(fUser,nil,nil);
  if aDictKanji.Checked<>pre then exit;
  ToggleForm(fWordKanji,fUser.SpeedButton6,aDictKanji);
end;

procedure TfMenu.aDictCategoriesExecute(Sender: TObject);
var pre:boolean;
begin
  pre:=aDictCategories.Checked;
  if not fUser.Visible then ToggleForm(fUser,nil,nil);
  if aDictCategories.Checked<>pre then exit;
  ToggleForm(fWordCategory,fUser.SpeedButton7,aDictCategories);
end;

procedure TfMenu.aDictAddExecute(Sender: TObject);
var pre:boolean;
begin
  pre:=aDictAdd.Checked;
  if not fUser.Visible then ToggleForm(fUser,nil,nil);
  if aDictAdd.Checked<>pre then exit;
  ToggleForm(fExamples,fUser.SpeedButton9,aDictAdd);
end;

procedure TfMenu.aDictEditorExecute(Sender: TObject);
begin
  displaymode:=3;
  ChangeDisplay;
end;

procedure TfMenu.aUserAddExecute(Sender: TObject);
begin
  if fWords.Visible then
    fWords.Button2Click(Sender) else
  if fUser.Visible then
    fUser.SpeedButton17Click(Sender) else
  if fKanjiCompounds.Visible then
    fKanjiCompounds.sbInsertIntoVocabClick(Sender) else
end;

procedure TfMenu.aUserSettingsExecute(Sender: TObject);
var pre:boolean;
begin
  pre:=aUserSettings.Checked;
  if not fWords.Visible then ToggleForm(fWords,nil,nil);
  if aUserSettings.Checked<>pre then exit;
  ToggleForm(fUserFilters,fWords.SpeedButton2,aUserSettings);
end;

procedure TfMenu.aUserDetailsExecute(Sender: TObject);
var pre:boolean;
begin
  pre:=aUserDetails.Checked;
  if not fWords.Visible then ToggleForm(fWords,nil,nil);
  if aUserDetails.Checked<>pre then exit;
  ToggleForm(fUserDetails,fWords.SpeedButton4,aUserDetails);
end;

procedure TfMenu.aUserPrintExecute(Sender: TObject);
begin
  fWords.Button15Click(sender);
end;

procedure TfMenu.aUserGenerateExecute(Sender: TObject);
begin
  fWords.Button19Click(sender);
end;

procedure TfMenu.aSettingsExecute(Sender: TObject);
begin
  fSettings.pcPages.ActivePage:=fSettings.tsGeneral;
  fSettings.ShowModal;
  if fKanji.Visible then fKanji.DoIt;
  if fUser.Visible then fUser.Look();
  if fWords.Visible then fWords.ShowIt(false);
  if fTranslate.Visible then fTranslate.RepaintText;
end;

procedure TfMenu.aChangeLanguageExecute(Sender: TObject);
begin
  fLanguage.ShowModal;
end;

procedure TfMenu.aSettingsDictExecute(Sender: TObject);
begin
  fDictMan.ShowModal;
end;

procedure TfMenu.aHelpExecute(Sender: TObject);
begin
  if FileExists('wakan_'+curGUILanguage+'.chm') then
    ShellExecute(fMenu.handle,nil,pchar('wakan_'+curGUILanguage+'.chm'),nil,nil,SW_SHOW) else
  if FileExists('wakan.chm') then
    ShellExecute(fMenu.handle,nil,'wakan.chm',nil,nil,SW_SHOW) else
  if FileExists('wakan_bld.chm') then
  begin
    Application.MessageBox(
      pchar(_l('#00363^eHelp file is under construction, the information may be inaccurate.')),
      pchar(_l('#00364^eNotice')),
      MB_ICONWARNING or MB_OK);
    ShellExecute(fMenu.handle,nil,'wakan_bld.chm',nil,nil,SW_SHOW);
  end else
  if FileExists('wakan_en.chm') then
    Application.MessageBox(
      pchar(_l('#00365^eHelp file is out of date. Please download new help file '
        +'from WaKan website: wakan.manga.cz.')),
      pchar(_l('#00020^eError')),
      MB_ICONERROR or MB_OK)
  else
    Application.MessageBox(
      pchar(_l('#00366^eCannot find file WAKAN.CHM.')),
      pchar(_l('#00020^eError')),
      MB_ICONERROR or MB_OK);
end;

procedure TfMenu.aAboutExecute(Sender: TObject);
begin
  fSplash.ProgressBar1.Hide;
  fSplash.BitBtn1.Show;
  fSplash.ShowModal;
end;

procedure TfMenu.aJapaneseExecute(Sender: TObject);
begin
  btnJapaneseModeClick(Sender);
end;

procedure TfMenu.aChineseExecute(Sender: TObject);
begin
  btnChineseModeClick(Sender);
end;

procedure TfMenu.aEditorNewExecute(Sender: TObject);
begin
  if not fTranslate.Visible then aDictEditorExecute(Sender);
  fTranslate.sbFileNewClick(sender);
end;

procedure TfMenu.aEditorOpenExecute(Sender: TObject);
begin
  if not fTranslate.Visible then aDictEditorExecute(Sender);
  fTranslate.sbFileOpenClick(sender);
end;

procedure TfMenu.aEditorSaveExecute(Sender: TObject);
begin
  if not fTranslate.Visible then aDictEditorExecute(Sender);
  fTranslate.sbFileSaveClick(sender);
end;

procedure TfMenu.aEditorSaveAsExecute(Sender: TObject);
begin
  if not fTranslate.Visible then aDictEditorExecute(Sender);
  fTranslate.SaveAs;
end;

procedure TfMenu.aEditorCutExecute(Sender: TObject);
begin
  if not fTranslate.ListBox1.Focused then exit;
  if not fTranslate.Visible then aDictEditorExecute(Sender);
  fTranslate.sbClipCutClick(sender);
end;

procedure TfMenu.aEditorExportExecute(Sender: TObject);
begin
  if not fTranslate.Visible then aDictEditorExecute(Sender);
  fTranslate.ExportAs;
end;

procedure TfMenu.aEditorCopyExecute(Sender: TObject);
begin
  if not fTranslate.ListBox1.Focused then exit;
  if not fTranslate.Visible then aDictEditorExecute(Sender);
  fTranslate.sbClipCopyClick(sender);
end;

procedure TfMenu.aEditorCopyAsExecute(Sender: TObject);
begin
  if not fTranslate.ListBox1.Focused then exit;
  if not fTranslate.Visible then aDictEditorExecute(Sender);
  fTranslate.CopyAs;
end;

procedure TfMenu.aEditorPasteExecute(Sender: TObject);
begin
  if not fTranslate.ListBox1.Focused then exit;
  if not fTranslate.Visible then aDictEditorExecute(Sender);
  fTranslate.sbClipPasteClick(sender);
end;

procedure TfMenu.aEditorSelectAllExecute(Sender: TObject);
begin
  if not fTranslate.ListBox1.Focused then exit;
  if not fTranslate.Visible then aDictEditorExecute(Sender);
  fTranslate.SelectAll;
end;

procedure TfMenu.aEditorKanjiModeExecute(Sender: TObject);
begin
  if not fTranslate.Visible then aDictEditorExecute(Sender);
  fTranslate.sbKanjiModeClick(sender);
end;

procedure TfMenu.aEditorKanaModeExecute(Sender: TObject);
begin
  if not fTranslate.Visible then aDictEditorExecute(Sender);
  fTranslate.sbKanaModeClick(sender);
end;

procedure TfMenu.aEditorASCIIModeExecute(Sender: TObject);
begin
  if not fTranslate.Visible then aDictEditorExecute(Sender);
  fTranslate.sbAsciiModeClick(sender);
end;

procedure TfMenu.aEditorReadingExecute(Sender: TObject);
begin
  if not fTranslate.Visible then aDictEditorExecute(Sender);
  fTranslate.sbDisplayReading.Down:=not fTranslate.sbDisplayReading.Down;
  fTranslate.sbDisplayReadingClick(sender);
end;

procedure TfMenu.aEditorMeaningExecute(Sender: TObject);
begin
  if not fTranslate.Visible then aDictEditorExecute(Sender);
  fTranslate.sbDisplayMeaning.Down:=not fTranslate.sbDisplayMeaning.Down;
  fTranslate.sbDisplayMeaningClick(sender);
end;

procedure TfMenu.aEditorClearExecute(Sender: TObject);
begin
  if not fTranslate.Visible then aDictEditorExecute(Sender);
  fTranslate.sbClearTranslationClick(sender);
end;

procedure TfMenu.aEditorFillExecute(Sender: TObject);
begin
  if not fTranslate.Visible then aDictEditorExecute(Sender);
  fTranslate.sbAutoTranslateClick(sender);
end;

procedure TfMenu.aEditorSetExecute(Sender: TObject);
begin
  if not fTranslate.Visible then aDictEditorExecute(Sender);
  fTranslate.sbSetTranslationClick(sender);
end;

procedure TfMenu.aEditorPrintExecute(Sender: TObject);
begin
  if not fTranslate.Visible then aDictEditorExecute(Sender);
  fTranslate.sbPrintClick(sender);
end;

procedure TfMenu.aKanjiAllExecute(Sender: TObject);
begin
  if not fKanji.Visible then aKanjiExecute(Sender);
  fKanjiSearch.sbClearFiltersClick(Sender);
end;

procedure TfMenu.aKanjiLearnedExecute(Sender: TObject);
begin
  if not fKanji.Visible then aKanjiExecute(Sender);
  fKanjiSearch.SpeedButton1.Down:=not fKanjiSearch.SpeedButton1.Down;
  fKanjiSearch.sbPinYinClick(Sender);
end;

procedure TfMenu.aKanjiCommonExecute(Sender: TObject);
begin
  if not fKanji.Visible then aKanjiExecute(Sender);
  fKanjiSearch.btnOnlyCommon.Down:=not fKanjiSearch.btnOnlyCommon.Down;
  fKanjiSearch.sbPinYinClick(Sender);
end;

procedure TfMenu.aKanjiClipboardExecute(Sender: TObject);
begin
  if not fKanji.Visible then aKanjiExecute(Sender);
  fKanjiSearch.btnInClipboard.Down:=not fKanjiSearch.btnInClipboard.Down;
  fKanjiSearch.sbPinYinClick(Sender);
end;

procedure TfMenu.aKanjiPinYinExecute(Sender: TObject);
begin
  if (not fKanji.Visible) then aKanjiExecute(Sender);
  if (not fKanjiSearch.Visible) then aKanjiSearchExecute(Sender);
  fKanjiSearch.edtPinYin.SetFocus;
end;

procedure TfMenu.aKanjiYomiExecute(Sender: TObject);
begin
  if (not fKanji.Visible) then aKanjiExecute(Sender);
  if (not fKanjiSearch.Visible) then aKanjiSearchExecute(Sender);
  fKanjiSearch.edtYomi.SetFocus;
end;

procedure TfMenu.aKanjiRadicalExecute(Sender: TObject);
begin
  if (not fKanji.Visible) then aKanjiExecute(Sender);
  if (not fKanjiSearch.Visible) then aKanjiSearchExecute(Sender);
  fKanjiSearch.sbListRadicalsClick(Sender);
end;

procedure TfMenu.aKanjiAddClipboardExecute(Sender: TObject);
begin
  if not fKanji.Visible then aKanjiExecute(Sender);
  fKanjiDetails.SpeedButton23Click(Sender);
end;

procedure TfMenu.aKanjiSetLearnedExecute(Sender: TObject);
begin
  if not fKanji.Visible then aKanjiExecute(Sender);
  fKanjiDetails.btnAddToCategoryClick(Sender);
end;

procedure TfMenu.aKanjiFullDetailsExecute(Sender: TObject);
begin
  if not fKanjiDetails.Visible then aKanjiDetailsExecute(Sender);
  fKanjiDetails.btnStrokeOrderClick(Sender);
end;

procedure TfMenu.aDictJapaneseExecute(Sender: TObject);
begin
  if not fUser.Visible then aDictExecute(Sender);
  fUser.btnLookupJtoE.Down:=true;
//  fUser.Edit1.Text:='';
  fUser.btnLookupJtoEClick(Sender);
end;

procedure TfMenu.aDictEnglishExecute(Sender: TObject);
begin
  if not fUser.Visible then aDictExecute(Sender);
  fUser.btnLookupEtoJ.Down:=true;
//  fUser.Edit1.Text:='';
  fUser.btnLookupJtoEClick(Sender);
end;

procedure TfMenu.aDictClipboardExecute(Sender: TObject);
begin
  if not fUser.Visible then aDictExecute(Sender);
  fUser.btnLookupClip.Down:=true;
//  fUser.Edit1.Text:='';
  fUser.btnLookupJtoEClick(Sender);
end;

procedure TfMenu.aDictAddClipboardExecute(Sender: TObject);
begin
  if not fUser.Visible then aDictExecute(Sender);
  fUser.btnCopyToClipboardClick(Sender);
end;

procedure TfMenu.aDictExactExecute(Sender: TObject);
begin
  if not fUser.Visible then aDictExecute(Sender);
  fUser.SpeedButton10.Down:=true;
  dictbeginset:=0;
  fUser.btnLookupJtoEClick(Sender);
end;

procedure TfMenu.aDictBeginningExecute(Sender: TObject);
begin
  if not fUser.Visible then aDictExecute(Sender);
  fUser.SpeedButton11.Down:=true;
  dictbeginset:=1;
  fUser.btnLookupJtoEClick(Sender);
end;

procedure TfMenu.aDictEndExecute(Sender: TObject);
begin
  if not fUser.Visible then aDictExecute(Sender);
  fUser.SpeedButton12.Down:=true;
  dictbeginset:=2;
  fUser.btnLookupJtoEClick(Sender);
end;

procedure TfMenu.aKanjiWindowExecute(Sender: TObject);
begin
  if not fKanji.Visible then aKanjiExecute(Sender);
  fKanji.DrawGrid1.SetFocus;
end;

procedure TfMenu.aKanjiMeaningExecute(Sender: TObject);
begin
  if not fKanjiSearch.Visible then aKanjiSearchExecute(Sender);
  fKanjiSearch.edtDefinition.SetFocus;
end;

procedure TfMenu.aEditorWindowExecute(Sender: TObject);
begin
  if not fTranslate.Visible then aDictEditorExecute(Sender);
  fTranslate.ListBox1.SetFocus;
end;

procedure TfMenu.aEditorSmallFontExecute(Sender: TObject);
begin
  if not fTranslate.Visible then aDictEditorExecute(Sender);
  fTranslate.FontSize := FontSizeSmall;
end;

procedure TfMenu.aEditorMedFontExecute(Sender: TObject);
begin
  if not fTranslate.Visible then aDictEditorExecute(Sender);
  fTranslate.FontSize := FontSizeMedium;
end;

procedure TfMenu.aEditorLargeFontExecute(Sender: TObject);
begin
  if not fTranslate.Visible then aDictEditorExecute(Sender);
  fTranslate.FontSize := FontSizeLarge;
end;

procedure TfMenu.tab3Click(Sender: TObject);
begin
  ToggleForm(fTranslate,nil,nil);
end;

procedure TfMenu.FormResize(Sender: TObject);
begin
//  PaintBox3.Visible:=Width>815;
//  Shape9.Visible:=Width>815;
//  SpeedButton22.Visible:=Width>815;
//  Bevel5.Visible:=Width>815;
end;

{ Panel docker.
 If given "dock", docks the form to its rightful place and shows it,
 else hides it and undocks it.
 Returns true if the form was docked before this call. }
function TfMenu.DockExpress(form:TForm;dock:boolean): boolean;
begin
  Result := false;
  if form=fKanjiDetails then begin
    if aPortraitMode.Checked then
      Result:=DockProc(fKanjiDetails,Panel4,alBottom,dock)
    else
      Result:=DockProc(fKanjiDetails,Panel4,alRight,dock);
  end;
  if form=fKanjiSearch then
    Result:=DockProc(fKanjiSearch,fKanji.pnlDockSearch,alTop,dock);
  if form=fKanjiCompounds then begin
    Result:=DockProc(fKanjiCompounds,fKanji.pnlDockCompounds,alBottom,dock);
    fKanji.splDockCompounds.Visible := dock;
    fKanji.splDockCompounds.Top := fKanji.pnlDockCompounds.Top - 1;
  end;
  if (form=fExamples) and (curdisplaymode<>5) then
    Result:=DockProc(fExamples,fUser.pnlDockExamples,alBottom,dock);
  if (form=fExamples) and (curdisplaymode=5) then
    Result:=DockProc(fExamples,fWords.pnlDockExamples,alBottom,dock);
  if form=fWordKanji then
    if aPortraitMode.Checked then
      Result:=DockProc(fWordKanji,fUser.Panel3,alBottom,dock)
    else
      Result:=DockProc(fWordKanji,fUser.Panel3,alRight,dock);
  if form=fUserFilters then
    if aPortraitMode.Checked then
      Result:=DockProc(fUserFilters,fWords.pnlDockFilters,alBottom,dock)
    else
      Result:=DockProc(fUserFilters,fWords.pnlDockFilters,alRight,dock);
  if form=fUserDetails then begin
    Result:=DockProc(fUserDetails,fWords.pnlDockDetails,alBottom,dock);
    fWords.splDockDetails.Visible := dock;
    fWords.splDockDetails.Top := fWords.pnlDockDetails.Top - 1;
  end;
end;

{ Only use this to dock and undock modules of a main form.
 Modules are docked once, then sometimes hidden (form.Hide) and shown with MainDock(form,panel) again.
 Some are redocked to other places at times. }
procedure TfMenu.MainDock(form:TForm;panel:TPanel);
begin
  if form.HostDockSite<>panel then begin
    form.Width:=panel.ClientWidth;
    form.Height:=panel.ClientHeight;
    form.ManualDock(panel);
    form.Align:=alClient;
  end;
  form.Show;
end;

{ Modules are undocked at the end of the program. After undocking they remain hidden. }
procedure TfMenu.MainUndock(form:TForm);
var rect:TRect;
begin
  form.Hide;
  rect.left:=0;
  rect.top:=0;
  rect.right:=form.width;
  rect.bottom:=form.height;
  form.ManualFloat(rect);
  form.Align:=alNone;
end;

//Shows or hides a form. Some forms only shows.
procedure TfMenu.ToggleForm(form:TForm;sb:TSpeedButton;action:TAction;state:boolean);
begin
  if form=fKanji then
  begin
    displaymode:=1;
    ChangeDisplay;
    exit;
  end;
  if form=fUser then
  begin
    displaymode:=2;
    ChangeDisplay;
    exit;
  end;
  if form=fTranslate then
  begin
    displaymode:=3;
    ChangeDisplay;
    exit;
  end;
  if form=fWords then
  begin
    displaymode:=5;
    ChangeDisplay;
    exit;
  end;

  if (sb=nil) or (form=fKanjiDetails) then
  begin
   //Initially this had no checks for "if visible/not visible", I added those to minimize
   //pointless resizes and OnShows.
   //If something breaks, keep this in mind.
    if state then begin
      if not form.visible then form.show;
    end else
      if form.visible then form.hide;
  end else
    if state then begin
      if not form.visible then
      begin
        DockExpress(form,true);
        form.show;
      end;
    end else begin
      if form.visible then
      begin
        form.hide;
        DockExpress(form,false);
      end;
    end;

  if sb<>nil then sb.down:=form.visible;
  if action<>nil then action.checked:=form.visible;
end;

procedure TfMenu.ToggleForm(form:TForm;sb:TSpeedButton;action:TAction);
begin
 //Special case -- that's how it was
  if sb=nil then
    ToggleForm(form,sb,action,not form.Visible)
  else
  if action<>nil then
    ToggleForm(form,sb,action,not action.Checked)
  else
    ToggleForm(form,sb,action,not form.Visible);
end;

//React to page changes and update main form, showing or hiding various panels
procedure TfMenu.ChangeDisplay;
begin
 //Hide active module
  case curdisplaymode of
    1:fKanji.Hide;
    2:fUser.Hide;
    5:fWords.Hide;
    3:fTranslate.Hide;
    4:begin
        fUser.Hide;
        fTranslate.Hide;
      end;
  end;
  Panel2.Height:=0;
  aMode1.Checked:=false;
  aMode2.Checked:=false;
  aMode3.Checked:=false;
  aMode4.Checked:=false;
  aMode5.Checked:=false;
 //If KanjiDetails is in docked mode, show or hide it as needed.
 //When in free-floating mode it doesn't need our attention.
  if CharDetDocked then begin
    if (displaymode=1) and CharDetDockedVis1 then
      DockExpress(fKanjiDetails,true)
    else
    if (displaymode in [3, 4]) and CharDetDockedVis2 then
      DockExpress(fKanjiDetails,true)
    else
      DockExpress(fKanjiDetails,false);
  end else begin
   //Hide dock panel
    Panel4.Width := 0;
    Panel4.Height := 0;
  end;
  if fExamples.visible then
    DockExpress(fExamples,false);
  case displaymode of
    1:begin
        MainDock(fKanji,Panel3);
        tab1.Down:=true;
        if fKanji.DrawGrid1.CanFocus then
          fKanji.DrawGrid1.SetFocus;
        aMode1.Checked:=true;
      end;
    2:begin
        MainDock(fUser,Panel3);
        tab2.Down:=true;
        aMode2.Checked:=true;
      end;
    3:begin
        MainDock(fTranslate,Panel3);
        tab3.Down:=true;
        fTranslate.sbDockDictionary.Down:=false;
        aMode3.Checked:=true;
      end;
    4:begin
        Panel2.height:=250;
        MainDock(fUser,Panel2);
        MainDock(fTranslate,Panel3);
        tab3.Down:=true;
        fTranslate.sbDockDictionary.Down:=true;
        aMode3.Checked:=true;
      end;
    5:begin
        MainDock(fWords,Panel3);
        tab5.Down:=true;
        aMode5.Checked:=true;
      end;
  end;
  curdisplaymode:=displaymode;
  fKanjiDetails.FormShow(fMenu);
  if (((curdisplaymode=2) or (displaymode=4)) and (aDictAdd.Checked)) or
     ((curdisplaymode=5) and (aUserExamples.Checked)) then
    DockExpress(fExamples,true);
end;

procedure TfMenu.TabControl1Change(Sender: TObject);
begin
  if tab1.Down then displaymode:=1;
  if tab2.Down then displaymode:=2;
  if tab3.Down and not fTranslate.sbDockDictionary.Down then displaymode:=3;
  if tab3.Down and fTranslate.sbDockDictionary.Down then displaymode:=4;
  if tab5.Down then displaymode:=5;
  ChangeDisplay;
end;

procedure TfMenu.aMode1Execute(Sender: TObject);
begin
  displaymode:=1;
  ChangeDisplay;
end;

procedure TfMenu.aMode2Execute(Sender: TObject);
begin
  displaymode:=2;
  ChangeDisplay;
end;

procedure TfMenu.aMode3Execute(Sender: TObject);
begin
  if fTranslate.sbDockDictionary.Down then displaymode:=4 else displaymode:=3;
  ChangeDisplay;
end;

procedure TfMenu.aMode4Execute(Sender: TObject);
begin
  displaymode:=4;
  ChangeDisplay;
end;

procedure TfMenu.aMode5Execute(Sender: TObject);
begin
  displaymode:=5;
  ChangeDisplay;
end;

procedure TfMenu.SpeedButton1Click(Sender: TObject);
begin
  if SpeedButton1.Down then
  begin
    if not FileExists('wakanh.dll') then
    begin
      Application.MessageBox(
        pchar(_l('#00367^eCannot find file WAKANH.DLL.')),
        pchar(_l('#00020^eError')),
        MB_ICONERROR or MB_OK);
      exit;
    end;
    Screen.Cursor:=crHourGlass;
    ctlFileMap:=CreateFileMapping($FFFFFFFF,nil,PAGE_READWRITE,0,1,'wakanh_ctl_sharemem');
    if ctlFileMap=0 then
    begin
      showmessage('Win32 API error: CreateFileMap() failed.');
      exit;
    end;
    ptrFileMap:=MapViewOfFile(ctlFileMap,FILE_MAP_WRITE,0,0,1);
    if ptrFileMap=nil then
    begin
      showmessage('Win32 API error: MapViewOfFile() failed.');
      exit;
    end;
    byte(ptrFileMap^):=0;
{MCH    InjectLibrary(ALL_SESSIONS or SYSTEM_PROCESSES, 'wakanh.dll');}
    Screen.Cursor:=crDefault;
    screenModeSc:=true;
  end else
  begin
    screenModeSc:=false;
    Screen.Cursor:=crHourGlass;
    UnMapViewOfFile(ptrFileMap);
    CloseHandle(ctlFileMap);
{MCH    UninjectLibrary(ALL_SESSIONS or SYSTEM_PROCESSES, 'wakanh.dll');}
    Screen.Cursor:=crDefault;
  end;
end;

//Called by ScreenTip to handle button clicking
procedure TfMenu.PopupMouseUp(button:TMouseButton;shift:TShiftState;x,y:integer);
begin
  if fRadical.Visible then exit;
  fScreenTip.PopupMouseMove(x,y);
  if fScreenTip.screenTipButton=0 then exit;
  if (fScreenTip.screenTipButton>2) and (not fMenu.Focused) then fMenu.Show;
  case fScreenTip.screenTipButton of
    1:begin
        clip:=clip+fScreenTip.screenTipText;
        SetClipboard;
      end;
    2:begin
        clip:=fScreenTip.screenTipText;
        SetClipboard;
      end;
    3:begin
        clip:=fScreenTip.screenTipText;
        SetClipboard;
        if not fRadical.Visible then aDictClipboard.Execute;
      end;
    4:begin
        if fRadical.Visible then exit;
        if not fKanjiDetails.Visible then aKanjiDetails.Execute;
        fKanjiDetails.SetCharDetails(fcopy(fScreenTip.screenTipText,1,1));
      end;
  end;
end;

procedure TfMenu.PopupImmediate(left:boolean);
begin
  HandlePopup({ShowImmediate=}true);
end;

procedure IPCCallback(name: pchar; messageBuf : pointer; messageLen : dword;
                      answerBuf  : pointer; answerLen  : dword); stdcall;
var mycnt:integer;
begin
  if messageLen<>sizeof(TTextInfo) then exit;
  inc(rdcnt);
  if rdcnt>90 then exit;
  mycnt:=rdcnt;
  move(messageBuf^,curtext[mycnt],sizeof(TTextInfo));
end;

procedure BitCallback(name: pchar; messageBuf : pointer; messageLen : dword;
                      answerBuf  : pointer; answerLen  : dword); stdcall;
var mycnt:integer;
begin
  if messageLen<>sizeof(TBitInfo) then exit;
  inc(bitcnt);
  if bitcnt>90 then exit;
  mycnt:=bitcnt;
  move(messageBuf^,curbit[mycnt],sizeof(TBitInfo));
end;

procedure TfMenu.ScreenTimerTimer(Sender: TObject);
begin
  if not initdone then exit;
  AutosaveUserData;
  HandlePopup({ShowImmediate=}false);
end;

{ Shows or hides or updates popup, reacting to mouse movements.
 Call on timer, or possibly OnMouseMove, or manually with ShowImmediate=true. }
procedure TfMenu.HandlePopup(ShowImmediate:boolean);
  procedure wwadd(bg,en:integer;w:word);
  var b:integer;
      i,j:integer;
      ass:boolean;
  begin
    b:=0;
    for i:=0 to ftextpos-1 do if (ftextend[i]=en) and (ftextbeg[i]=bg) then
    begin end else
    begin
      ftext[b]:=ftext[i];
      ftextbeg[b]:=ftextbeg[i];
      ftextend[b]:=ftextend[i];
      inc(b);
    end;
    ftextpos:=b;
    ass:=false;
    for i:=0 to ftextpos-1 do if (not ass) and (ftextbeg[i]>bg) then
    begin
      for j:=ftextpos-1 downto i do
      begin
        ftext[j+1]:=ftext[j];
        ftextbeg[j+1]:=ftextbeg[j];
        ftextend[j+1]:=ftextend[j];
      end;
      ftext[i]:=w;
      ftextbeg[i]:=bg;
      ftextend[i]:=en;
      inc(ftextpos);
      ass:=true;
    end;
    if not ass then
    begin
      ftext[ftextpos]:=w;
      ftextbeg[ftextpos]:=bg;
      ftextend[ftextpos]:=en;
      inc(ftextpos);
    end;
  end;
var pt:TPoint;
    s:string;
    s2:FString;
    i,j:integer;
    b:byte;
    wbg,wen:integer;
    lp:TPoint;
    cx:integer;
    last:integer;
    wtp:integer;
    k:integer;
    gbg:array[0..255] of integer;
    gen:array[0..255] of integer;
    wp:WINDOWPLACEMENT;
    ct:TTextInfo;
    ev,cev:integer;
    ttim,tleft,tright:integer;
    wnd:HWnd;
    wt:shortstring;
    wr:TRect;
    savedx:array[1..100] of integer;
    savedy:array[1..100] of integer;
    evc:integer;
    rx,ry:integer;
    gc:TGridCoord;
    rect:TRect;
    wtt:integer;
    curt:TDateTime;
  intmosc:TPoint;
begin
  if not screenModeWk and not screenModeSc and not ShowImmediate and not popcreated then exit;

  try
    pt:=Mouse.CursorPos;
  except
   //Mouse.CursorPos can raise EOSError on some versions of Delphi,
   //as underlying WINAPI GetCursorPos returns false if this is not the active desktop.
    on E: EOSError do exit;
  end;

  if HandlingPopup then exit;
  HandlingPopup:=true;
  try
    if not TryStrToInt(fSettings.Edit21.Text, ttim) then ttim:=10;
    if not TryStrToInt(fSettings.Edit22.Text, tleft) then tleft:=10;
    if not TryStrToInt(fSettings.Edit23.Text, tright) then tright:=100;

   //Popup active + mouse inside popup => exit
    if popcreated
    and (pt.x>=fScreenTip.Left-10)
    and (pt.y>=fScreenTip.Top-10)
    and (pt.x<=fScreenTip.Left+fScreenTip.Width+10)
    and (pt.y<=fScreenTip.Top+fScreenTip.Height+10) then
      exit;

   //Mouse moved => hide popup, reset popup timer
    if (not ShowImmediate) and ((pt.x<>LastMousePt.x) or (pt.y<>LastMousePt.y)) then
    begin
      if popcreated then
      begin
        HideScreenTip;
        popcreated:=false;
      end;
      LastMousePt:=pt;
      LastMouseMove:=GetTickCount;
      exit;
    end;

   //Do not show popup if we're doing drag-selection
    if (not ShowImmediate) and (not popcreated) and (DragStartCtl<>nil) then
      exit;

   //Wait for popup delay
    if (not ShowImmediate) and (not popcreated) and (GetTickCount()-LastMouseMove<cardinal(ttim)*100) then
      exit;

    ftextpos:=0;
    s:='';
    if screenModeWk or ShowImmediate then
    begin
     //Popup delay might expire while we're over some unrelated place
      if HoverCtl<>nil then begin
        intmosc:=HoverCtl.ClientToScreen(HoverPt);
        if (pt.x=intmosc.x) and (pt.y=intmosc.y) then
          s:=StringUnderMouse;
      end;
      if flength(s)>=1 then evc:=EvalChar(fgetch(s,1));
    end;
    if (s='') and screenModeSc then
    begin
      fInvalidator:=TfInvalidator.Create(nil);
      fInvalidator.Width:=tleft+tright;
      fInvalidator.Height:=1;
      fInvalidator.Top:=pt.y;
      fInvalidator.Left:=pt.x-tleft;
      for i:=1 to 100 do curtext[i].slen:=0;
      rdcnt:=0;
      bitcnt:=0;
  {MCH    CreateIPCQueue('texthook_data',IPCCallback);
      CreateIPCQueue('texthook_bit',BitCallback);}
      byte(ptrFileMap^):=1;
      SetWindowPos(fInvalidator.handle,HWND_TOPMOST,0,0,0,0,SWP_NOMOVE or SWP_NOSIZE or SWP_NOACTIVATE or SWP_SHOWWINDOW);
      fInvalidator.Hide;
      fInvalidator.Free;
      sleep(100);
      byte(ptrFileMap^):=0;
  {MCH    DestroyIPCQueue('texthook_data');
      DestroyIPCQueue('texthook_bit');}
      s:='';
      s2:='';
      ftextpos:=0;
      if rdcnt>90 then rdcnt:=90;
      if bitcnt>90 then bitcnt:=90;
      // create debug
      for i:=1 to rdcnt do
      begin
        ct:=curtext[i];
        savedx[i]:=ct.x;
        savedy[i]:=ct.y;
        if (ct.dcinfo and (TA_TOP or TA_BOTTOM or TA_BASELINE))=TA_BOTTOM then ct.y:=ct.y-ct.h;
        if (ct.dcinfo and (TA_TOP or TA_BOTTOM or TA_BASELINE))=TA_BASELINE then ct.y:=ct.y-ct.h;
        if (ct.dcinfo and (TA_RIGHT or TA_LEFT or TA_CENTER))=TA_CENTER then ct.x:=ct.x-ct.w div 2;
        if (ct.dcinfo and (TA_LEFT or TA_RIGHT or TA_CENTER))=TA_RIGHT then ct.x:=ct.x-ct.w;
        curtext[i]:=ct;
      end;
      wnd:=WindowFromPoint(pt);
      wt[0]:=AnsiChar(chr(Windows.GetWindowText(wnd,@(wt[1]),255)));
      s2:=s2+fstr('Window name: '+wt)+UH_CR+UH_LF;
      Windows.GetWindowRect(wnd,wr);
      s2:=s2+fstr('Window rect: ['+inttostr(wr.Left)+':'+
        inttostr(wr.Top)+']-['+inttostr(wr.Right)+':'+inttostr(wr.Bottom)+']')+UH_CR+UH_LF;
      Windows.GetClientRect(wnd,wr);
      s2:=s2+fstr('Client area: '+inttostr(wr.Right)+':'+inttostr(wr.Bottom)+'')+UH_CR+UH_LF;
      s2:=s2+fstr('Cursor pos: '+inttostr(pt.x)+':'+inttostr(pt.y))+UH_CR+UH_LF;
      s2:=s2+UH_CR+UH_LF+fstr('BitBlts:')+UH_CR+UH_LF;
      for i:=1 to bitcnt do
      begin
        s2:=s2+fstr(inttostr(i)+'# Mod:');
        for j:=1 to rdcnt do if curbit[i].srcdc=curtext[j].hdc then
        begin
          s2:=s2+fstr(inttostr(j)+';');
          curtext[j].hwnd:=curbit[i].hwnd;
          curtext[j].x:=curtext[j].x+curbit[i].xofs;
          curtext[j].y:=curtext[j].y+curbit[i].yofs;
      //    s:=s+'T+'+inttostr(curbit[i].xofs)+'='+inttostr(curbit[i].yofs)+'+';
        end;
        s2:=s2+fstr(' Ofs:'+inttostr(curbit[i].xofs)+':'+inttostr(curbit[i].yofs))+UH_CR+UH_LF;
      end;
      s2:=s2+UH_CR+UH_LF+fstr('TextOuts:')+UH_CR+UH_LF;
      for i:=1 to rdcnt do
      begin
        lp.x:=curtext[i].x;
        lp.y:=curtext[i].y;
        Windows.GetWindowRect(wnd,wr);
        lp.x:=lp.x+wr.Left;
        lp.y:=lp.y+wr.Top;
        s2:=s2+fstr(inttostr(i)+'# "');
        for j:=0 to curtext[i].slen-1 do if curtext[i].str[j]>=32 then
          s2:=s2+fstr(widechar(curtext[i].str[j]));
        s2:=s2+fstr('"'+
          ' Org:'+inttostr(savedx[i])+':'+inttostr(savedy[i])+
          ' Align:'+inttostr(curtext[i].x)+':'+inttostr(curtext[i].y)+
          ' Trans:'+inttostr(lp.x)+':'+inttostr(lp.y)+
          ' Size:'+inttostr(curtext[i].w)+':'+inttostr(curtext[i].h)+' ');
        if curtext[i].hwnd=wnd then s2:=s2+fstr('OK') else s2:=s2+fstr('BAD WND');
        s2:=s2+UH_CR+UH_LF;
      end;
      for i:=1 to rdcnt do if (curtext[i].slen>0) and (curtext[i].hwnd=wnd) then
      begin
        wbg:=0;
        lp.x:=curtext[i].x;
        lp.y:=curtext[i].y;
        Windows.GetWindowRect(wnd,wr);
        lp.x:=lp.x+wr.Left;
        lp.y:=lp.y+wr.Top;
        if (lp.y<=pt.y) and (lp.x+curtext[i].w>=pt.x) and (lp.y+curtext[i].h>=pt.y) then
        begin
          cx:=lp.x;
          wen:=-1;
          for j:=0 to curtext[i].slen-1 do
          begin
            gbg[j]:=cx;
            gen[j]:=cx+curtext[i].len[j];
    //        s2:=s2+chr(curtext[i].str[j] mod 256);
            if wen=-1 then
            begin
              if (curtext[i].str[j]<ord('A')) or ((curtext[i].str[j]>ord('Z')) and
                  (curtext[i].str[j]<ord('a'))) or (curtext[i].str[j]>ord('z')) then
                  begin wbg:=j+1; wtp:=2; end else wtp:=1;
            end;
            if (cx+curtext[i].len[j]>pt.x) and (wen=-1) then
            begin
              if wbg>j then wbg:=j;
              if wbg=-1 then wbg:=0;
              wen:=wbg;
              if wtp=1 then while (wen+1<curtext[i].slen) and
                (((curtext[i].str[wen+1]>=ord('a')) and (curtext[i].str[wen+1]<=ord('z'))) or
                 ((curtext[i].str[wen+1]>=ord('A')) and (curtext[i].str[wen+1]<=ord('Z')))) do inc(wen);
              if wtp=2 then wen:=wbg+10;
              if wen>=curtext[i].slen then wen:=curtext[i].slen-1;
            end;
            cx:=cx+curtext[i].len[j];
          end;
          if wen<>-1 then for k:=wbg to wen do
            wwadd(gbg[k],gen[k],curtext[i].str[k]);
          if wtp=1 then wwadd(gen[wen],gen[wen],32);
        end;
      end;
      cev:=0;
      cx:=-100;
      last:=0;
      if (ftextpos>0) and (ftextbeg[0]<=pt.x+2) then
      for i:=0 to ftextpos-1 do
      begin
        if cev=0 then cev:=EvalChar(WideChar(ftext[i]));
        ev:=EvalChar(WideChar(ftext[i]));
        if (cev=ev) or ((cev=1) and (ev=2)) then
        begin
          if (ev<>0) and ((ftext[i]<>last) or (ftextbeg[i]>cx+2)) then
            s:=s+fstr(widechar(ftext[i]))
        end else break;
        cx:=ftextbeg[i];
        last:=ftext[i];
        cev:=EvalChar(WideChar(ftext[i]));
      end;
      if s<>'' then evc:=EvalChar(WideChar(ftext[0]));
    end;
    screenTipDebug:=s2;
    if s<>'' then
    begin
      DragStartCtl:=nil; //TODO: Perhaps, move this to some common function?
      SetSelectionHighlight(0,0,0,0,nil); //This too
      if ShowImmediate then
        ShowScreenTip(pt.x-10,pt.y-10,s,evc,true)
      else
        ShowScreenTip(pt.x+10,pt.y+10,s,evc,false);
      if fScreenTip<>nil then popcreated:=true;
    end;
  finally
    HandlingPopup:=false;
  end;
end;

{
IntTip*()
Various controls from all over the program call these on mouse events,
to support text selection and hint popups.
Minimal set you have to call from a control:
 IntTipControlOver(...) on mouse move
 IntTipMouseUp() on mouse up

Your control also has to be supported by UpdateSelection (see comments there).
}
procedure TfMenu.IntTipMouseMove(c:TControl;x,y:integer;leftDown:boolean);
begin
  if leftDown and (DragStartCtl<>c) then begin
    if DragStartCtl<>nil then AbortDrag; //at old control
    IntTipMouseDown(c,x,y); //auto-down
  end;
  if (not leftDown) and (DragStartCtl<>nil) then
   //auto-up: some controls send MouseOver(leftDown=false) first, and we'd lose
   //selected text if we just continued.
    IntTipMouseUp;
  HoverPt:=Point(x,y);
  HoverCtl:=c;
  UpdateSelection;
end;

procedure TfMenu.IntTipMouseDown(c:TControl;x,y:integer);
begin
 //Remember "drag start" point and control
  DragStartPt:=Point(x,y);
  DragStartCtl:=c;
end;

procedure TfMenu.IntTipMouseUp;
begin
 //Remove "drag start" point. Important - or we'll continue dragging
  DragStartCtl:=nil;
  fMenu.PopupImmediate(true);
end;

//Simply abort drag but show no popup
procedure TfMenu.AbortDrag;
begin
  DragStartCtl:=nil;
end;

//Updates text selection highlight and currently highlighted contents in intcurString
procedure TfMenu.UpdateSelection;
var s1:string;
    rx,ry,wtt:integer;
    gc:TGridCoord;
  rpos: TSourcePos;
  MouseControl: TControl; //control which receives the mouse events
  MousePos: TPoint; //mouse pos in that control coordinate system
begin
 //It is important that we route mouse events to the control which captured mouse,
 //else we'd get popup even when clicking in Text Editor, then dragging the mouse outside
  if DragStartCtl=nil then begin
   //mouse is free, hovering over intmo
    MouseControl:=HoverCtl;
    MousePos:=HoverPt;
  end else begin
   //mouse is captured by a different control
    MouseControl:=DragStartCtl;
    if DragStartCtl=HoverCtl then
      MousePos:=HoverPt
    else
      MousePos:=MouseControl.ScreenToClient(HoverCtl.ClientToScreen(HoverPt)); //convert to capture control coordinate system
  end;
 //Now MouseControl can either be DragStart control, MouseOver control or nil.

  if MouseControl=nil then begin
    s1 := '';
    SetSelectionHighlight(0,0,0,0,nil);
  end else

  if MouseControl=fTranslate.EditorPaintBox then
  begin
    rpos:=fTranslate.GetExactLogicalPos(MousePos.x,MousePos.y);
    rx := rpos.x; ry := rpos.y;
    if (ry<>-1) and (rx>=0) and (rx<=fTranslate.doctr[ry].charcount) then
      s1:=fTranslate.GetDocWord(rx,ry,wtt,false)
    else
      s1:='';
    SetSelectionHighlight(0,0,0,0,nil);
  end else

  if MouseControl is TPaintBox then
  begin
    if DragStartCtl<>nil then //dragging
      s1:=PaintBoxUpdateSelection(TPaintBox(MouseControl),DragStartPt,MousePos)
    else //just hovering
      s1:=PaintBoxUpdateSelection(TPaintBox(MouseControl),MousePos,MousePos);
  end else

  if MouseControl is TCustomDrawGrid then
  begin
    gc:=TCustomDrawGrid(MouseControl).MouseCoord(MousePos.x,MousePos.y);
    if MouseControl=fKanji.DrawGrid1 then begin
      s1:=fKanji.GetKanji(gc.x,gc.y);
      SetSelectionHighlight(0,0,0,0,nil);
    end else
    if MouseControl=fRadical.DrawGrid then begin
      s1:=fRadical.GetKanji(gc.x,gc.y);
      SetSelectionHighlight(0,0,0,0,nil);
    end else
    if DragStartCtl<>nil then //dragging
      s1:=DrawGridUpdateSelection(TCustomDrawGrid(MouseControl),DragStartPt,MousePos)
    else //just hovering
      s1:=DrawGridUpdateSelection(TCustomDrawGrid(MouseControl),MousePos,MousePos);
  end else

  begin
    s1 := ''; //invalid control in intmo
    SetSelectionHighlight(0,0,0,0,nil);
  end;

  StringUnderMouse:=s1;
end;


procedure TfMenu.PaintBox3MouseMove(Sender: TObject; Shift: TShiftState; X,
  Y: Integer);
begin
  fMenu.IntTipMouseMove(PaintBox3,x,y,ssLeft in Shift);
end;

procedure TfMenu.aDictInflectExecute(Sender: TObject);
begin
  if not fUser.Visible then aDictExecute(Sender);
  fUser.SpeedButton4.Down:=not fUser.SpeedButton4.Down;
  fUser.btnLookupJtoEClick(Sender);
end;

procedure TfMenu.aDictAutoExecute(Sender: TObject);
begin
  if not fUser.Visible then aDictExecute(Sender);
  fUser.SpeedButton13.Down:=not fUser.SpeedButton13.Down;
  fUser.btnLookupJtoEClick(Sender);
end;

procedure TfMenu.aDictGroup1Execute(Sender: TObject);
begin
  if not fUser.Visible then aDictExecute(Sender);
  fUser.SpeedButton14.Down:=true;
  fUser.btnLookupJtoEClick(Sender);
end;

procedure TfMenu.aDictGroup2Execute(Sender: TObject);
begin
  if not fUser.Visible then aDictExecute(Sender);
  fUser.SpeedButton15.Down:=true;
  fUser.btnLookupJtoEClick(Sender);
end;

procedure TfMenu.aDictGroup3Execute(Sender: TObject);
begin
  if not fUser.Visible then aDictExecute(Sender);
  fUser.SpeedButton16.Down:=true;
  fUser.btnLookupJtoEClick(Sender);
end;

procedure TfMenu.aUserExamplesExecute(Sender: TObject);
var pre:boolean;
begin
  pre:=aUserExamples.Checked;
  if not fWords.Visible then ToggleForm(fWords,nil,nil);
  if aUserExamples.Checked<>pre then exit;
  ToggleForm(fExamples,fWords.SpeedButton1,aUserExamples);
end;

procedure TfMenu.aEditorColorsExecute(Sender: TObject);
begin
  if not fTranslate.Visible then aDictEditorExecute(Sender);
  aEditorColors.Checked:=not aEditorColors.Checked;
  fTranslate.sbUseTlColors.Down:=aEditorColors.Checked;
  fTranslate.mustrepaint:=true;
  fTranslate.ShowText(true);
end;

function _l(const id:string):string;
begin
  Result := JWBUnit._l(id);
end;

procedure TfMenu.PaintBox3MouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  if mbLeft=Button then Self.IntTipMouseUp;
end;

procedure TfMenu.miSaveCharactersToFileClick(
  Sender: TObject);
begin
  fKanji.SaveChars;
end;

procedure TfMenu.aDictMiddleExecute(Sender: TObject);
begin
  if not fUser.Visible then aDictExecute(Sender);
  fUser.SpeedButton18.Down:=true;
  dictbeginset:=3;
  fUser.btnLookupJtoEClick(Sender);
end;

procedure TfMenu.aCategoryManagerExecute(Sender: TObject);
begin
  fCategoryMgr.ShowModal;
end;

procedure TfMenu.aPortraitModeExecute(Sender: TObject);
var
  UserFiltersDocked: boolean;
  WordKanjiDocked: boolean;
  KanjiDetailsDocked: boolean;
begin
  UserFiltersDocked := DockExpress(fUserFilters,false);
  WordKanjiDocked := DockExpress(fWordKanji,false);
  KanjiDetailsDocked := CharDetDocked and DockExpress(fKanjiDetails,false);

  if aPortraitMode.Checked then begin
    Panel4.Align := alBottom;
    fWords.pnlDockFilters.Align := alBottom;
    fWords.splDockFilters.Align := alBottom;
    fWords.splDockFilters.Top := fWords.pnlDockFilters.Top - 1;
    fUser.Panel3.Align := alBottom;
  end else begin
    Panel4.Align := alRight;
    fWords.pnlDockFilters.Align := alRight;
    fWords.splDockFilters.Align := alRight;
    fWords.splDockFilters.Left := fWords.pnlDockFilters.Left - 1;
    fUser.Panel3.Align := alRight;
  end;

 //New dock mode will be applied to forms on re-docking

 //If CharDetDocked was false (logically Undocked), then KanjiDetailsDocked
 //will be false too, and we won't even try to redock fKanjiDetails, which is right.

  if UserFiltersDocked then DockExpress(fUserFilters,true);
  if WordKanjiDocked then DockExpress(fWordKanji,true);
  if KanjiDetailsDocked then begin
    DockExpress(fKanjiDetails,true);
//    fKanjiDetails.UpdateAlignment; //TODO: Do we need this?
  end;
 //ChangeDisplay -- should not be needed
end;

initialization
  rdcnt:=0;
  popcreated:=false;
  chardetl:=TStringList.Create;
  CharPropTypes:=TStringList.Create;

finalization

end.
