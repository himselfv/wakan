unit JWBMenu;

interface

{$R WINXP.RES}

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, ComCtrls, RXCtrls, Db,
  DBTables, ExtCtrls, Grids, TextTable, Buttons, {ThemeMgr,} MemSource, ShellApi,
  ActnList, Menus, rxPlacemnt{MCH, madCodeHook}, JWBStrings, JWBUtils,
  StdPrompt, JWBDic;

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
    aLayout: TAction;
    aQuick1: TAction;
    aQuick2: TAction;
    aQuick3: TAction;
    aQuick4: TAction;
    aQuick5: TAction;
    aQuick6: TAction;
    aBorders: TAction;
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
    fdfd1: TMenuItem;
    JapMode1: TMenuItem;
    ChinMode1: TMenuItem;
    eSaveuserchangescUloituivatelskzmny1: TMenuItem;
    eCanceluserchangescZruituivatelskzmny1: TMenuItem;
    N1: TMenuItem;
    eStatisticscStatistika1: TMenuItem;
    eSettingscNastaven2: TMenuItem;
    eDictionarymanagercManaerslovnk1: TMenuItem;
    N15: TMenuItem;
    eExitcUkonit1: TMenuItem;
    eCharacterscZnaky1: TMenuItem;
    N5: TMenuItem;
    eDisplayallcZobrazitvechny1: TMenuItem;
    eLearnedonlycPouzenauen1: TMenuItem;
    eCommononlycPouzebn1: TMenuItem;
    eInclipboardonlycPouzeveschrnce1: TMenuItem;
    N3: TMenuItem;
    eSearchbyPinYincHledatpodlePinYin1: TMenuItem;
    aSearchbyYomicHledatpodleYomi1: TMenuItem;
    eSearchbymeaningcHledatpodlevznamu1: TMenuItem;
    eSearchbyradicalcHledatpodleradiklu1: TMenuItem;
    N6: TMenuItem;
    eAddtoclipboardcPidatdoschrnky1: TMenuItem;
    eSetasunlearnedcNastavitjakonenauen1: TMenuItem;
    ePrintcardscTiskkaret1: TMenuItem;
    N13: TMenuItem;
    eSearchcHledn1: TMenuItem;
    eDetailscDetaily1: TMenuItem;
    eCompoundscSloeniny1: TMenuItem;
    eDictionarycSlovnk1: TMenuItem;
    eJapaneseChineseEnglishcJaponskynskyAnglicky1: TMenuItem;
    eEnglishJapaneseChinesecAnglickyJaponskynsky1: TMenuItem;
    eSearchbyclipboardcHledatpodleschrnky1: TMenuItem;
    N8: TMenuItem;
    eSearchexactwordcHledatpesnslovo1: TMenuItem;
    eSearchbeginningcHledatzatek1: TMenuItem;
    eSearchendingcHledatkonec1: TMenuItem;
    N12: TMenuItem;
    eAddtoclipboardcPidatdoschrnky2: TMenuItem;
    N16: TMenuItem;
    eCharactersinwordcZnakyveslov1: TMenuItem;
    eAddtovocabularycPidatdoslovek1: TMenuItem;
    Editor2: TMenuItem;
    N22: TMenuItem;
    eNewcNov1: TMenuItem;
    eOpencOtevt1: TMenuItem;
    eSavecUloit1: TMenuItem;
    eSaveascUloitjako1: TMenuItem;
    N17: TMenuItem;
    eCutcVyjmout1: TMenuItem;
    eCopycKoprovat1: TMenuItem;
    ePastecVloit1: TMenuItem;
    eSelectallcVybratve1: TMenuItem;
    N18: TMenuItem;
    eCharactermodecReimznak1: TMenuItem;
    eKanamodecReimkany1: TMenuItem;
    eASCIImodecReimASCII1: TMenuItem;
    N19: TMenuItem;
    eDisplayreadingcZobrazitten1: TMenuItem;
    eDisplaymeaningcZobrazitzpis1: TMenuItem;
    eFontsizecVelikostpsma1: TMenuItem;
    eSmallcMal1: TMenuItem;
    eMediumcStedn1: TMenuItem;
    eLargecVelk1: TMenuItem;
    N20: TMenuItem;
    eTranslationcPeklad1: TMenuItem;
    eCleartranslationcSmazatpeklad1: TMenuItem;
    eSettranslationcNastavitpeklad1: TMenuItem;
    eAutofilltranslationcAutomatickydoplnitpeklad1: TMenuItem;
    N21: TMenuItem;
    ePrintcTisk1: TMenuItem;
    eVocabularycSlovka1: TMenuItem;
    eAddwordcPidatslovko1: TMenuItem;
    eListsettingscNastavenseznamu1: TMenuItem;
    eWorddetailscDetailyslovka1: TMenuItem;
    N7: TMenuItem;
    ePrintlistcTiskseznamu1: TMenuItem;
    eListgeneratorcGenertorseznamu1: TMenuItem;
    eHelpcNpovda1: TMenuItem;
    eHelpcontentscObsahnpovdy1: TMenuItem;
    N11: TMenuItem;
    eAboutcOprogramu1: TMenuItem;
    Timer2: TTimer;
    Panel3: TPanel;
    Panel2: TPanel;
    Shape9: TShape;
    PaintBox3: TPaintBox;
    aMode1: TAction;
    aMode2: TAction;
    aMode3: TAction;
    aMode4: TAction;
    aMode5: TAction;
    eModecReim1: TMenuItem;
    eCharacterlistcSeznamznak1: TMenuItem;
    eDictionarycSlovnk2: TMenuItem;
    eTexteditorcTextoveditor1: TMenuItem;
    eVocabularycSlovka2: TMenuItem;
    eCharacterlistcSeznamznak2: TMenuItem;
    eDictionarycSlovnk3: TMenuItem;
    N4: TMenuItem;
    eTexteditorcTextoveditor2: TMenuItem;
    eVocabularycSlovka3: TMenuItem;
    N9: TMenuItem;
    Bevel1: TBevel;
    ScreenTimer: TTimer;
    Panel4: TPanel;
    SpeedButton1: TSpeedButton;
    SpeedButton2: TSpeedButton;
    eSavekanatranscriptcUloitpepisdokany1: TMenuItem;
    SaveAsKanaDialog: TSaveDialog;
    aDictInflect: TAction;
    aDictAuto: TAction;
    aDictGroup1: TAction;
    aDictGroup2: TAction;
    aDictGroup3: TAction;
    eSearchinflectedwordscHledatinflektovanslova1: TMenuItem;
    eAutosearchwhiletypingcAutomatickyhledatpipsan1: TMenuItem;
    eUseddictionarygroupcPouitskupinaslovnk1: TMenuItem;
    N10: TMenuItem;
    eUsedictionariesingroup1cPouvatslovnkyzeskupiny11: TMenuItem;
    eUsedictionariesingroup2cPouvatslovnkyzeskupiny21: TMenuItem;
    eUsedictionariesingroup3cPouvatslovnkyzeskupiny31: TMenuItem;
    aDictVoc1: TAction;
    aDictVoc2: TAction;
    aDictVoc3: TAction;
    aUserExamples: TAction;
    Pklady1: TMenuItem;
    aEditorColors: TAction;
    eUsecolorscPouvatbarvy1: TMenuItem;
    aDictMiddle: TAction;
    eSearchsubstringcHledatpodetzec1: TMenuItem;
    N23: TMenuItem;
    N25: TMenuItem;
    N2: TMenuItem;
    eSavecharacterstofilecUloitznakydosouboru1: TMenuItem;
    N00242eAddwordcPidatslovko1: TMenuItem;
    N00929eChangelanguage1: TMenuItem;
    aChangeLanguage: TAction;
    FormPlacement1: TFormPlacement;
    aFullscreenMode: TAction;
    FullscreenMode1: TMenuItem;
    N26: TMenuItem;
    N27: TMenuItem;
    aCategoryManager: TAction;
    N14: TMenuItem;
    N24: TMenuItem;
    aEditorCopyAs: TAction;
    aPortraitMode: TAction;
    PortraitMode1: TMenuItem;
    procedure FormDestroy(Sender: TObject);
    procedure RadioGroup1Click(Sender: TObject);
    procedure RadioGroup2Click(Sender: TObject);
    procedure BitBtn1Click(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure SpeedButton5Click(Sender: TObject);
    procedure ArtLabel1Click(Sender: TObject);
    procedure SpeedButton2Click(Sender: TObject);
    procedure SpeedButton7Click(Sender: TObject);
    procedure SpeedButton8Click(Sender: TObject);
    procedure tab1Click(Sender: TObject);
    procedure tab2Click(Sender: TObject);
    procedure tab5Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure SpeedButton9Click(Sender: TObject);
    procedure Timer1Timer(Sender: TObject);
    procedure SpeedButton11Click(Sender: TObject);
    procedure SpeedButton12Click(Sender: TObject);
    procedure btnJapaneseModeClick(Sender: TObject);
    procedure btnChineseModeClick(Sender: TObject);
    procedure Image1Click(Sender: TObject);
    procedure SpeedButton15Click(Sender: TObject);
    procedure Action1Execute(Sender: TObject);
    procedure SpeedButton16Click(Sender: TObject);
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
    procedure aQuick1Execute(Sender: TObject);
    procedure aQuick2Execute(Sender: TObject);
    procedure aQuick3Execute(Sender: TObject);
    procedure aQuick4Execute(Sender: TObject);
    procedure aQuick5Execute(Sender: TObject);
    procedure aQuick6Execute(Sender: TObject);
    procedure aBordersExecute(Sender: TObject);
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
    procedure Timer2Timer(Sender: TObject);
    procedure tab3Click(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure FormResize(Sender: TObject);
    procedure tab4Click(Sender: TObject);
    procedure TabControl1Change(Sender: TObject);
    procedure eCharacterlistcSeznamznak1Click(Sender: TObject);
    procedure eDictionarycSlovnk2Click(Sender: TObject);
    procedure eTexteditorcTextoveditor1Click(Sender: TObject);
    procedure aMode1Execute(Sender: TObject);
    procedure aMode2Execute(Sender: TObject);
    procedure aMode3Execute(Sender: TObject);
    procedure aMode4Execute(Sender: TObject);
    procedure aMode5Execute(Sender: TObject);
    procedure SpeedButton1Click(Sender: TObject);
    procedure ScreenTimerTimer(Sender: TObject);
    procedure PaintBox3MouseMove(Sender: TObject; Shift: TShiftState; X,
      Y: Integer);
    procedure eSavekanatranscriptcUloitpepisdokany1Click(Sender: TObject);
    procedure aDictInflectExecute(Sender: TObject);
    procedure aDictAutoExecute(Sender: TObject);
    procedure aDictGroup1Execute(Sender: TObject);
    procedure aDictGroup2Execute(Sender: TObject);
    procedure aDictGroup3Execute(Sender: TObject);
    procedure aDictVoc1Execute(Sender: TObject);
    procedure aDictVoc2Execute(Sender: TObject);
    procedure aDictVoc3Execute(Sender: TObject);
    procedure aUserExamplesExecute(Sender: TObject);
    procedure aEditorColorsExecute(Sender: TObject);
    procedure PaintBox3MouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure eSavecharacterstofilecUloitznakydosouboru1Click(
      Sender: TObject);
    procedure aDictMiddleExecute(Sender: TObject);
    procedure aChangeLanguageExecute(Sender: TObject);
    procedure aFullscreenModeExecute(Sender: TObject);
    procedure aCategoryManagerExecute(Sender: TObject);
    procedure aEditorCopyAsExecute(Sender: TObject);
    procedure aPortraitModeExecute(Sender: TObject);

  private
    initdone:boolean;
    procedure LoadWakanCfg(const filename: string);
  public
    procedure InitializeWakan;

  private //Docking
    procedure DockProc(slave,host:TForm;panel:TPanel;dir:integer;dock:boolean);
    procedure FixConstraints(form:TForm;w,h:integer);
  public
    procedure DockExpress(form:TForm;dock:boolean);
    procedure FormUndock(form:TForm);

  public
    ResFixVal:integer;
    StrokeOrderPackage:TPackageSource; //apparently a remnant from an older way of drawing stroke order. Always == nil
    screenTipShown:boolean;

    screenTipDebug:string;
    screenModeSc,screenModeWk:boolean;
    screenTipImmediate:boolean;
    ctlFileMap:cardinal;
    ptrFileMap:pointer;

    procedure XPResFix(form:TForm);
    procedure TranslateAll;
    procedure SetFormPos(form:TForm);
    procedure WriteUserPackage(dir:string);
    procedure SaveUserData;
    procedure LoadUserData;
    procedure RefreshCategory;
    procedure RefreshKanjiCategory;
    procedure ExportUserData(filename:string);
    procedure ImportUserData(filename:string);
    function FlushUserData:boolean;
    procedure ToggleForm(form:TForm;sb:TSpeedButton;action:TAction);
    procedure WriteLayout(filename:string);
    procedure ReadLayout(filename:string);
    procedure ShowForm(button:TSpeedButton;action:TAction;form:TForm);
    procedure HideForm(button:TSpeedButton;action:TAction;form:TForm);
    procedure StandardLayout(lay:integer;perc:integer);
    procedure RescanDicts;
    procedure ClearDicts;
    procedure SwitchLanguage(lanchar:char);
    function GetCharValue(index,vt:integer):string;
    function GetCharValueInt(index,vt:integer):integer;
    function GetCharValueRad(index,vt:integer):integer;
    procedure ChangeDisplay;
    procedure ShowScreenTip(x,y:integer;s:string;wt:integer;immediate:boolean);
    procedure HideScreenTip;
    procedure PopupMouseUp(button:TMouseButton;shift:TShiftState;x,y:integer);
    procedure PopupImmediate(left:boolean);
    procedure RebuildUserIndex;
//    procedure LoadLayout(filename:string);
//    procedure SaveFixedLayout(filename:string);

  protected
    DicLoadPrompt: TSMPromptForm;
    procedure DicLoadStart(Sender: TObject);
    procedure DicLoadEnd(Sender: TObject);
    procedure DicLoadException(Sender: TObject; E: Exception);
  public
    function NewDict(dicname: string): TJaletDic;

  protected
    FUserDataChanged:boolean;
    procedure SetUserDataChanged(Value: boolean);
  public
    procedure ChangeUserData;
    property UserDataChanged: boolean read FUserDataChanged write SetUserDataChanged;


  private //Text under mouse
    intcurString:string; { String under the mouse pointer right now.
      Only CalculateCurString changes this member, and only ScreenTimerTimer uses it.
      And CalculateCurString is only called from IntTipPaintOver/IntTipGridOver }
    //What. Are. These.
    //And why are there two of them?
    intorgPaint:TPaintBox;
    intorgGrid:TCustomDrawGrid;
    intorgcx,intorgcy,intorgsx,intorgsy:integer;
    intmoPaint:TPaintBox;
    intmoGrid:TCustomDrawGrid;
    intmocx,intmocy,intmosx,intmosy:integer;
    procedure CalculateCurString;
  public
    procedure IntTipPaintOver(p:TPaintBox;x,y:integer;leftDown:boolean);
    procedure IntTipGridOver(sg:TCustomDrawGrid;x,y:integer;leftDown:boolean);

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
  CharDetDocked,CharDetNowDocked,CharDetDockedVis1,CharDetDockedVis2:boolean;

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
    order: string; //although it's integer insidee
  end;
  readchl: TStringList; //list of readings to include to the reading chart

  userdataloaded:boolean;
  curlang:char;
  curqlayout:integer;
  proposedlayout:integer;
  borderchange:boolean;
  displaymode:integer;
  curdisplaymode:integer;
  oldpt,begpt:TPoint;
  tim:integer;
  rdcnt,bitcnt:integer;
  curtext:array[1..100] of TTextInfo;
  curbit:array[1..100] of TBitInfo;
  ftext:array[0..255] of word;
  ftextbeg:array[0..255] of integer;
  ftextend:array[0..255] of integer;
  ftextpos:integer;
  inproc:boolean;
  popcreated:boolean;
  vocmode,exmode:integer;
  rainesearch:pointer;
  raineradicals:TStringList;
  sodir:TStringList;
  sobin:pointer;
  lastautosave:TDateTime;
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

uses JWBKanji, JWBUnit, JWBRadical,
  JWBSettings, JWBSplash, PKGWrite, JWBUser, UnicodeFont, registry, clipbrd,
  JWBWords, JWBNewCategory, JWBPrint, JWBStatistics,
  JWBWordList, JWBBitmap, JWBKanjiCompounds,
  JWBExamples, JWBUserDetails, JWBUserAdd, JWBUserFilters, JWBUserData,
  JWBKanjiDetails, JWBKanjiSearch, JWBWordDetails,
  JWBWordCategory, JWBWordKanji, JWBTranslate, JWBLayout, JWBStrokeOrder,
  JWBDictMan, JWBDictImport, JWBDictCoding, JWBCharItem, JWBScreenTip,
  JWBInvalidator, JWBDicAdd, JWBLanguage, JWBFileType, JWBConvert,
  JWBWordsExpChoose, JWBMedia, JWBDicSearch, JWBKanjiCard,
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
  intmopaint:=nil;
  intmogrid:=nil;
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
  sx:string;
  vi:TStringList;
  ms:TMemoryStream;
  i:integer;
  tempDir: string;
  LastModified: TDatetime;
begin
  lastautosave:=now;
  screenTipImmediate:=false;
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

    sx:='';
  {  if ChooseFont([SHIFTJIS_CHARSET],'',s,'',true)='!'then sx:=sx+',Shift-JIS';
    if ChooseFont([CHINESEBIG5_CHARSET],'',s,'',true)='!'then sx:=sx+',Big5';
    if ChooseFont([GB2312_CHARSET],'',s,'',true)='!'then sx:=sx+',GB2312';
    if sx<>'' then
    begin
      delete(sx,1,1);
      Application.MessageBox(
        pchar(_l('#00348^eNo fonts of there character sets were found on your computer:'#13#13
          +sx+#13#13
          +'You must have at least one font of each of these sets on your computer '
          +'to run this application.'#13#13
          +'I recommend installing Ms Mincho, MS Gothic, SimSun and MingLiU fonts.'#13
          +'These fonts are automatically installed when you install support for '
          +'reading Japanese & Chinese language in windows.'#13#13
          +'Please install required fonts and run this application again.')),
        pchar(_l('#00020^eError')),
        MB_OK or MB_ICONERROR);
      Application.Terminate;
      exit;
    end; }
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

   {
    DownloadSources.LoadFromFile('Dependencies.cfg');

    //Just a test
    tempDir := CreateRandomTempDirName();
    ForceDirectories(tempDir);
    //DownloadFile('http://ftp.monash.edu.au/pub/nihongo/edict2.gz', tempDir+'\EDICT2.gz');
    DownloadFileIfModified('http://ftp.monash.edu.au/pub/nihongo/edicthdr.txt', tempDir+'\edicthdr.txt',
      now, LastModified);

   //Download dependencies!
    VerifyDependency('WORDFREQ_CK', 'WORDFREQ_CK');
    VerifyDependency('UNICONV.exe', 'UNICONV');
   }

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
      if MakeDicParams.Language='C' then
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
    while (pos('!',FontJapanese)>0) or (pos('!',FontJapaneseGrid)>0) or
      (pos('!',FontChinese)>0) or (pos('!',FontChineseGrid)>0) or
      (pos('!',FontChineseGB)>0) or (pos('!',FontChineseGridGB)>0) or
      (pos('!',FontSmall)>0) or (pos('!',FontRadical)>0) or (pos('!',FontEnglish)>0) or (pos('!',FontPinYin)>0) or (pos('!',FontStrokeOrder)>0) do
    begin
      Application.MessageBox(
        pchar(_l('#00353^eSome standard fonts were not found on your system.'#13
          +'Please reselect all fonts in the following dialog. Missing fonts are '
          +'preceded by !.'#13
          +'Application cannot continue unless all fonts are selected.')),
        pchar(_l('#00090^eWarning')),
        MB_ICONWARNING or MB_OK);
      fSettings.pcPages.ActivePage:=fSettings.tsFonts;
      fSettings.ShowModal;
    end;

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
      //showmessage(TChar.GetField(0,3));
      //showmessage(TChar.GetField(1,3));
      //showmessage(TChar.GetField(2,3));
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
  //  Left:=0;
  //  Top:=0;
    SetFormPos(fKanji);
    SetFormPos(fWords);
    SetFormPos(fUser);
    XPResFix(fMenu);
    XPResFix(fKanji);
    XPResFix(fWords);
    XPResFix(fUser);
    XPResFix(fUserAdd);
  //  XPResFix(fKanjiSearch);
    XPResFix(fKanjiCompounds);
    XPResFix(fWordDetails);
  //  XPResFix(fExamples);
    XPResFix(fTranslate);

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
    aBorders.Checked:=true;
    proposedlayout:=0;
    borderchange:=false;
  { Old way of loading layout:
    if (FileExists('wakan.lay')) and (setlayout=0) then ReadLayout('wakan.lay') else
    begin
      proposedlayout:=setlayout;
      timer2.enabled:=true;
    end;
    StandardLayout(0,100); }
    curdisplaymode:=0;
    if PortabilityMode=pmPortable then begin
      FormPlacement1.UseRegistry := false;
      FormPlacement1.IniFileName := AppFolder + '\wakan.ini';
    end else begin
      FormPlacement1.UseRegistry := true;
      FormPlacement1.IniFileName := WakanRegKey;
    end;
    FormPlacement1.IniSection := 'MainPos';
    FormPlacement1.RestoreFormPlacement;
    fKanjiDetails.FormPlacement1.UseRegistry := FormPlacement1.UseRegistry;
    fKanjiDetails.FormPlacement1.IniFileName := FormPlacement1.IniFileName;
    fKanjiDetails.FormPlacement1.IniSection := 'DetailPos';
    fKanjiDetails.FormPlacement1.RestoreFormPlacement;

    fSettings.ApplyUISettings();

    screenTipShown:=false;

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
//  SpeedButton2.Enabled:=FUserDataChanged;
//  SpeedButton7.Enabled:=FUserDataChanged;
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
//  fUser.SpeedButton4.Enabled:=lanchar='j';
  if (not fUser.btnLookupClip.Enabled) and (fUser.btnLookupClip.Down) then fUser.btnLookupJtoE.Down:=true;
  fExamples.ReloadExamples;
  fUser.Look();
  RefreshCategory;
  RefreshKanjiCategory;
end;

procedure TfMenu.XPResFix(form:TForm);
var sl:TStringList;
    i:integer;
    ct:TControl;
    s:string;
begin
  exit;
  if ResFixVal<>0 then
  begin
    if form.Constraints.MinHeight>0 then form.Constraints.MinHeight:=form.Constraints.MinHeight+ResFixVal;
    if form.Constraints.MaxHeight>0 then form.Constraints.MaxHeight:=form.Constraints.MaxHeight+ResFixVal;
    sl:=TStringList.Create;
    for i:=0 to form.ComponentCount-1 do
    begin
      if form.Components[i] is TControl then
      begin
        ct:=form.Components[i] as TControl;
        s:='';
        if akTop in ct.Anchors then s:=s+'T';
        if akBottom in ct.Anchors then s:=s+'B';
        sl.Add(s);
        ct.Anchors:=ct.Anchors+[akTop];
        ct.Anchors:=ct.Anchors-[akBottom];
      end else sl.Add('');
    end;
    form.height:=form.height+ResFixVal;
    for i:=0 to form.ComponentCount-1 do
    begin
      if form.Components[i] is TControl then
      begin
        ct:=form.Components[i] as TControl;
        if (sl[i]='') or (sl[i]='B') then
          ct.Anchors:=ct.Anchors-[akTop];
        if (sl[i]='B') or (sl[i]='TB') then
          ct.Anchors:=ct.Anchors+[akBottom];
      end;
    end;
  end;
end;

procedure TfMenu.ShowForm(button:TSpeedButton;action:TAction;form:TForm);
begin
  exit;
  if borderchange then exit;
  if aBorders.Checked then
  begin
    if form.tag=1 then
    begin
      form.show;
      DockExpress(form,true);
    end else
    begin
      form.Show;
      DockExpress(form,true);
      form.hide;
    end;
  end else if form.tag=1 then form.show else form.hide;
  button.down:=form.visible;
  if action<>nil then action.checked:=form.visible;
end;

procedure TfMenu.HideForm(button:TSpeedButton;action:TAction;form:TForm);
begin
  exit;
  if borderchange then exit;
  if (not aBorders.Checked) or (form=fKanjiDetails) then
  begin
    if form.visible then form.tag:=1 else form.tag:=0;
    form.hide;
  end;
  button.down:=false;
  if action<>nil then action.checked:=false;
end;

procedure TfMenu.ToggleForm(form:TForm;sb:TSpeedButton;action:TAction);
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
  if (not aBorders.Checked) or (sb=nil) or (form=fKanjiDetails) then
  begin
    if form.visible then form.hide else form.show;
    if form.visible then form.tag:=1 else form.tag:=0;
  end else
  begin
    if action.checked then
    begin
      if form.visible then
      begin
        form.hide;
        DockExpress(form,false);
        form.tag:=0;
      end;
    end else
    begin
      if not form.visible then
      begin
        form.tag:=1;
        DockExpress(form,true);
        form.show;
        form.tag:=2;
      end;
    end;
  end;
  if sb<>nil then sb.down:=form.visible;
  if action<>nil then action.checked:=form.visible;
end;

procedure TfMenu.TranslateAll;
begin
  fLanguage.TranslateForm(self);
  fLanguage.TranslateForm(fKanji);
  fLanguage.TranslateForm(fRadical);
  fLanguage.TranslateForm(fWords);
  fLanguage.TranslateForm(fUser);
  fLanguage.TranslateForm(fSettings);
  fLanguage.TranslateForm(fSelectFont);
  fLanguage.TranslateForm(fNewCategory);
  fLanguage.TranslateForm(fPrint);
  fLanguage.TranslateForm(fStatistics);
  fLanguage.TranslateForm(fWordList);
  fLanguage.TranslateForm(fBitmap);
  fLanguage.TranslateForm(fUserAdd);
  fLanguage.TranslateForm(fUserFilters);
  fLanguage.TranslateForm(fUserDetails);
  fLanguage.TranslateForm(fKanjiDetails);
  fLanguage.TranslateForm(fKanjiSearch);
  fLanguage.TranslateForm(fKanjiCompounds);
  fLanguage.TranslateForm(fWordDetails);
  fLanguage.TranslateForm(fExamples);
  fLanguage.TranslateForm(fWordCategory);
  fLanguage.TranslateForm(fWordKanji);
  fLanguage.TranslateForm(fTranslate);
  fLanguage.TranslateForm(fLayout);
  fLanguage.TranslateForm(fMedia);
  fLanguage.TranslateForm(fStrokeOrder);
  fLanguage.TranslateForm(fDictMan);
  fLanguage.TranslateForm(fDictImport);
  fLanguage.TranslateForm(fDictCoding);
  fLanguage.TranslateForm(fCharItem);
  fLanguage.TranslateForm(fDicAdd);
  fLanguage.TranslateForm(fFileType);
  fLanguage.TranslateForm(fWordsExpChoose);
  fLanguage.TranslateForm(fPortableMode);
  fLanguage.TranslateForm(fCategoryMgr);
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

function GetFormLayout(form:TForm):string;
var s:string;
begin
  if form.visible then s:='VISIBLE'else if form=fKanjiDetails then s:='PARENT'else s:='HIDDEN';
  if form.BorderStyle=bsNone then s:=s+',N'else s:=s+',Y';
  if (form=fMenu) or (form=fKanji) or (form=fUser) or (form=fWords) or (form=fKanjiDetails) or (form=fStrokeOrder) or (form=fTranslate) then
    s:=s+',ABSOLUTE,'else s:=s+',LEAVE,';
  s:=s+inttostr(form.left)+','+inttostr(form.top)+','+inttostr(form.width)+','+inttostr(form.height);
  result:=s;
end;

procedure SetFormLayout(form:TForm;s:string;fullborder:TFormBorderStyle;var screenorig:TPoint; var screendim:TPoint);
var params:array[1..10] of string;
    i:integer;
    v:integer;
    vf,vt:integer;
    sf,sfw:boolean;
begin
  i:=1;
  while pos(',',s)>0 do
  begin
    params[i]:=uppercase(copy(s,1,pos(',',s)-1));
    delete(s,1,pos(',',s));
    inc(i);
  end;
  params[i]:=uppercase(s);
  if (params[1]='VISIBLE') or (params[1]='PARENT') then form.tag:=1 else form.tag:=0;
//  if params[2]='N'then if form<>fMenu then form.borderStyle:=bsNone else form.borderStyle:=fullborder;
//  if (form=fMenu) then if params[2]='N'then form.tag:=1 else form.tag:=0;
  if uppercase(params[3])='LEAVE'then
  begin end else
  if uppercase(params[3])='ABSOLUTE'then
  begin
    form.left:=strtoint(params[4]);
    form.top:=strtoint(params[5]);
    sf:=((form.Constraints.minHeight=form.Constraints.MaxHeight) and
      (form.Constraints.minHeight>0)) or (fullborder=bsToolWindow);
    sfw:=((form.Constraints.minWidth=form.Constraints.MaxWidth) and
      (form.Constraints.minWidth>0)) or (fullborder=bsToolWindow);
    if not sfw then form.width:=strtoint(params[6]);
    if not sf then form.height:=strtoint(params[7]);
  end else if uppercase(params[3])='CENTER'then
  begin
    form.width:=strtoint(params[4]);
    form.height:=strtoint(params[5]);
    form.left:=screendim.x div 2-form.width div 2;
    form.top:=screendim.y div 2-form.height div 2;
  end else
  begin
    if (params[4]='%') and ((params[3]='TOP') or (params[3]='BOTTOM')) then v:=screendim.y*strtoint(params[5]) div 100 else
    if (params[4]='%') and ((params[3]='LEFT') or (params[3]='RIGHT')) then v:=screendim.x*strtoint(params[5]) div 100 else
    if (params[4]='.') and ((params[3]='TOP') or (params[3]='BOTTOM')) then v:=form.height else
    if (params[4]='.') and ((params[3]='LEFT') or (params[3]='RIGHT')) then v:=form.width else
      v:=strtoint(params[5]);
    if (params[6]='%') and ((params[3]='TOP') or (params[3]='BOTTOM')) then vf:=screendim.x*strtoint(params[7]) div 100 else
    if (params[6]='%') and ((params[3]='LEFT') or (params[3]='RIGHT')) then vf:=screendim.y*strtoint(params[7]) div 100 else
      vf:=strtoint(params[7]);
    if (params[8]='%') and ((params[3]='TOP') or (params[3]='BOTTOM')) then vt:=screendim.x*strtoint(params[9]) div 100 else
    if (params[8]='%') and ((params[3]='LEFT') or (params[3]='RIGHT')) then vt:=screendim.y*strtoint(params[9]) div 100 else
    if (params[8]='.') and ((params[3]='TOP') or (params[3]='BOTTOM')) then vt:=vf+form.width else
    if (params[8]='.') and ((params[3]='LEFT') or (params[3]='RIGHT')) then vt:=vf+form.height else
    if (params[8]='R') and ((params[3]='TOP') or (params[3]='BOTTOM')) then vt:=screendim.x else
    if (params[8]='R') and ((params[3]='LEFT') or (params[3]='RIGHT')) then vt:=screendim.y else
      vt:=strtoint(params[9]);
    if (params[3]='TOP') or (params[3]='BOTTOM') then form.height:=v else form.width:=v;
    if params[3]='TOP'then
    begin
      form.top:=screenorig.y;
      if params[10]='SET'then
      begin
        screenorig.y:=screenorig.y+form.height;
        screendim.y:=screendim.y-form.height;
      end;
      form.left:=vf+screenorig.x; form.width:=vt-vf;
    end;
    if params[3]='BOTTOM'then
    begin
      form.top:=screenorig.y+screendim.y-form.height;
      if params[10]='SET'then screendim.y:=screendim.y-form.height;
      form.left:=vf+screenorig.x; form.width:=vt-vf;
    end;
    if params[3]='LEFT'then
    begin
      form.left:=screenorig.x;
      if params[10]='SET'then
      begin
        screenorig.x:=screenorig.x+form.width;
        screendim.x:=screendim.x-form.width;
      end;
      form.top:=vf+screenorig.y; form.height:=vt-vf;
    end;
    if params[3]='RIGHT'then
    begin
      form.left:=screenorig.x+screendim.x-form.width;
      if params[10]='SET'then screendim.x:=screendim.x-form.width;
      form.top:=vf+screenorig.y; form.height:=vt-vf;
    end;
  end;
end;

procedure TfMenu.ReadLayout(filename:string);
var t:textfile;
  s:string;
  so,sd:TPoint;
begin
  assignfile(t,filename);
  reset(t);
  readln(t,s);
  if s<>'VERSION,6'then
  begin
    Application.MessageBox(
      pchar(_l('#00334^eCannot load layout. Outdated version.'#13#13
        +'Settings standard layout instead.')),
      pchar(_l('#00335^eLayout loading')),
      MB_ICONINFORMATION or MB_OK);
    closefile(t);
    StandardLayout(0,100);
    exit;
  end;
  readln(t,s);
  fUser.Hide;
  fKanji.Hide;
  fWords.Hide;
  SetFormLayout(fMenu,s,bsSizeToolWin,so,sd); readln(t,s);
  SetFormLayout(fKanji,s,bsSizeToolWin,so,sd); readln(t,s);
  SetFormLayout(fWords,s,bsSizeToolWin,so,sd); readln(t,s);
  SetFormLayout(fUser,s,bsSizeToolWin,so,sd); readln(t,s);
  SetFormLayout(fExamples,s,bsSizeToolWin,so,sd); readln(t,s);
  SetFormLayout(fUserFilters,s,bsToolWindow,so,sd); readln(t,s);
  SetFormLayout(fUserDetails,s,bsToolWindow,so,sd); readln(t,s);
  SetFormLayout(fKanjiDetails,s,bsSizeToolWin,so,sd); readln(t,s);
  SetFormLayout(fKanjiSearch,s,bsSizeToolWin,so,sd); readln(t,s);
  SetFormLayout(fKanjiCompounds,s,bsSizeToolWin,so,sd); readln(t,s);
//  SetFormLayout(fWordDetails,s,bsSizeToolWin,so,sd); readln(t,s);
  SetFormLayout(fExamples,s,bsSizeToolWin,so,sd); readln(t,s);
//  SetFormLayout(fWordCategory,s,bsToolWindow,so,sd); readln(t,s);
  SetFormLayout(fWordKanji,s,bsToolWindow,so,sd); readln(t,s);
  SetFormLayout(fTranslate,s,bsSizeToolWin,so,sd); readln(t,s);
  SetFormLayout(fStrokeOrder,s,bsToolWindow,so,sd); readln(t,s);
  if StrokeOrderPackage=nil then fStrokeOrder.Tag:=0;
  if fKanji.tag=1 then fKanji.Show;
  if fUser.tag=1 then fUser.Show;
  if fWords.tag=1 then fWords.Show;
  if fKanjiDetails.tag=1 then fKanjiDetails.Show;
  if fTranslate.tag=1 then fTranslate.Show;
  closefile(t);
end;

procedure TfMenu.WriteLayout(filename:string);
var t:textfile;
    s:string;
    i:integer;
begin
  assignfile(t,filename);
  rewrite(t);
  writeln(t,'VERSION,7');
  s:=GetFormLayout(fMenu); writeln(t,s);
  s:=GetFormLayout(fKanji); writeln(t,s);
  s:=GetFormLayout(fWords); writeln(t,s);
  s:=GetFormLayout(fUser); writeln(t,s);
  s:=GetFormLayout(fExamples); writeln(t,s);
  s:=GetFormLayout(fUserFilters); writeln(t,s);
  s:=GetFormLayout(fUserDetails); writeln(t,s);
  s:=GetFormLayout(fKanjiDetails); writeln(t,s);
  s:=GetFormLayout(fKanjiSearch); writeln(t,s);
  s:=GetFormLayout(fKanjiCompounds); writeln(t,s);
//  s:=GetFormLayout(fWordDetails); writeln(t,s);
  s:=GetFormLayout(fExamples); writeln(t,s);
//  s:=GetFormLayout(fWordCategory); writeln(t,s);
  s:=GetFormLayout(fWordKanji); writeln(t,s);
  s:=GetFormLayout(fTranslate); writeln(t,s);
  s:=GetFormLayout(fStrokeOrder); writeln(t,s);
  if not aBorders.Checked then writeln(t,'HIDEBORDERS') else writeln(t,'SHOWBORDERS');
  for i:=0 to chardetl.Count-1 do writeln(t,chardetl[i]);
  closefile(t);
end;

procedure TfMenu.StandardLayout(lay:integer;perc:integer);
var so,sd,so1,sd1:TPoint;
procedure sfl(form:TForm;s:string);
begin
  if (form=fUserFilters)
  or (form=fWordCategory)
  or (form=fWordKanji)
  or (form=fUserDetails)
  or (form=fKanjiCompounds) then
    SetFormLayout(form,s,bsToolWindow,so,sd) else
    SetFormLayout(form,s,bsSizeToolWin,so,sd);
end;
procedure res;
begin
  so.x:=0; so.y:=0;
  sd.x:=Screen.Width;
  sd.y:=(Screen.height-24)*perc div 100;
end;
procedure StdVocab;
begin
  sd:=sd1; so:=so1;
  sfl(fUserFilters,'VISIBLE,Y,RIGHT,.,0,=,0,.,0,LEAVE');
  sfl(fWords,'HIDDEN,Y,TOP,=,'+inttostr(fUserFilters.height)+',=,0,=,'+inttostr(sd.x-fUserFilters.width)+',SET');
  sfl(fUserDetails,'VISIBLE,Y,TOP,.,0,=,0,.,0,LEAVE');
  sfl(fUserAdd,'HIDDEN,Y,TOP,.,0,=,0,r,0,LEAVE');
end;
var sx:string;
begin
  if ((Screen.Width<1024) or (Screen.Height<768)) and (lay>0) then
  begin
    Application.MessageBox(
      pchar(_l('#00336^eAdvanced layouts require at least 1024x768 resolution.')),
      pchar(_l('#00337^eResolution too low')),
      MB_ICONERROR or MB_OK);
    exit;
  end;
  fUser.Hide;
  fKanji.Hide;
  fWords.Hide;
  fTranslate.Hide;
  fKanjiDetails.Hide;
  DockExpress(nil,false);
  if (lay>0) and (aBorders.Checked) then
  begin
    aBorders.Checked:=false;
    SpeedButton9Click(self);
  end;
  if (lay=0) and (not aBorders.Checked) then
  begin
    aBorders.Checked:=true;
    SpeedButton9Click(self);
  end;
  if fMenu.BorderStyle=bsNone then sx:='40'else sx:='74';
  res;
  sfl(fMenu,'VISIBLE,Y,TOP,.,0,=,0,r,0,SET');
  so1:=so; sd1:=sd;
{  case lay of
    0:begin
        sfl(fKanjiDetails,'VISIBLE,Y,RIGHT,%,100,=,0,r,0,SET');
        sfl(fKanji,'VISIBLE,Y,TOP,%,100,=,0,r,0,SET');
        fKanjiCompounds.Tag:=0;
        fKanjiSearch.Tag:=1;
        sd:=sd1; so:=so1;
        sfl(fUser,'HIDDEN,Y,TOP,%,100,=,0,r,0,LEAVE');
        fWordKanji.Tag:=1;
        fExamples.Tag:=0;
        sfl(fTranslate,'HIDDEN,Y,TOP,%,100,=,0,r,0,LEAVE');
        sfl(fWords,'HIDDEN,Y,TOP,%,100,=,0,r,0,LEAVE');
        fUserFilters.Tag:=1;
        fUserAdd.Tag:=0;
        fUserDetails.Tag:=0;
      end;
    1:begin
//        sfl(fWordDetails,'HIDDEN,Y,TOP,.,0,=,0,r,0,LEAVE');
//        sfl(fWordCategory,'HIDDEN,Y,BOTTOM,.,0,=,'+inttostr(sd.x-fWordCategory.Width)+',r,0,LEAVE');
        fKanjiDetails.Height:=350;
        sfl(fUser,'VISIBLE,Y,BOTTOM,=,180,=,0,=,'+inttostr(sd.x-fWordKanji.Width)+',LEAVE');
        sfl(fWordKanji,'VISIBLE,Y,BOTTOM,=,180,=,'+inttostr(sd.x-fWordKanji.Width)+',.,0,SET');
        sfl(fExamples,'HIDDEN,Y,BOTTOM,.,0,=,0,r,0,LEAVE');
        sfl(fTranslate,'VISIBLE,Y,BOTTOM,=,'+inttostr(sd.y-fKanjiDetails.Height)+',=,0,r,0,SET');
        sfl(fStrokeOrder,'HIDDEN,Y,BOTTOM,.,0,=,0,.,0,LEAVE');
        sfl(fKanjiDetails,'VISIBLE,Y,RIGHT,.,0,=,0,.,0,SET');
        sfl(fKanji,'VISIBLE,Y,TOP,=,170,=,0,r,0,SET');
        sfl(fKanjiSearch,'HIDDEN,Y,LEFT,.,0,=,0,.,0,LEAVE');
        sfl(fKanjiCompounds,'VISIBLE,Y,TOP,%,100,=,0,r,0,SET');
        StdVocab;
      end;
    2:begin
        fKanjiDetails.Height:=350;
        sfl(fKanji,'VISIBLE,Y,TOP,=,'+inttostr(sd.y-fKanjiDetails.height)+',=,0,r,0,SET');
        sfl(fKanjiDetails,'VISIBLE,Y,LEFT,.,0,=,0,.,0,SET');
        sfl(fKanjiCompounds,'VISIBLE,Y,TOP,=,'+inttostr(sd.y-fKanjiSearch.height)+',=,0,r,0,SET');
        sfl(fKanjiSearch,'VISIBLE,Y,LEFT,%,100,=,0,.,0,SET');
        sd:=sd1; so:=so1;
//        sfl(fWordDetails,'VISIBLE,Y,TOP,.,0,=,0,r,0,SET');
        sfl(fUser,'HIDDEN,Y,TOP,=,'+inttostr(sd.y-180-fExamples.Height)+',=,0,r,0,SET');
        sfl(fExamples,'VISIBLE,Y,TOP,.,0,=,0,r,0,SET');
//        sfl(fWordCategory,'HIDDEN,Y,BOTTOM,.,0,=,'+inttostr(sd.x-fWordCategory.Width)+',r,0,LEAVE');
        sfl(fWordKanji,'VISIBLE,Y,RIGHT,.,0,=,0,r,0,SET');
        sfl(fTranslate,'HIDDEN,Y,BOTTOM,%,100,=,0,r,0,LEAVE');
        StdVocab;
      end;
    3:begin
        fKanjiDetails.Height:=300;
        sfl(fTranslate,'HIDDEN,Y,BOTTOM,%,100,=,0,r,0,LEAVE');
//        sfl(fWordDetails,'HIDDEN,Y,TOP,.,0,=,0,r,0,LEAVE');
        sfl(fExamples,'VISIBLE,Y,TOP,.,0,=,0,r,0,SET');
        sfl(fWordKanji,'VISIBLE,Y,RIGHT,.,0,=,0,=,180,LEAVE');
//        sfl(fWordCategory,'HIDDEN,Y,RIGHT,.,0,=,0,.,0,LEAVE');
        sfl(fUser,'VISIBLE,Y,TOP,=,180,=,0,=,'+inttostr(sd.x-fWordKanji.width)+',SET');
        sfl(fKanji,'VISIBLE,Y,TOP,=,'+inttostr(sd.y-300)+',=,0,r,0,SET');
        sfl(fKanjiDetails,'VISIBLE,Y,LEFT,.,0,=,0,.,0,SET');
        sfl(fKanjiCompounds,'VISIBLE,Y,TOP,=,'+inttostr(sd.y-fKanjiSearch.height)+',=,0,r,0,SET');
        sfl(fKanjiSearch,'VISIBLE,Y,LEFT,%,100,=,0,.,0,SET');
        StdVocab;
      end;
    4:begin
        fKanjiDetails.Height:=350;
        sfl(fKanjiDetails,'HIDDEN,Y,RIGHT,.,0,=,0,.,0,SET');
        sfl(fKanji,'HIDDEN,Y,TOP,=,190,=,0,r,0,SET');
        sfl(fKanjiCompounds,'VISIBLE,Y,TOP,=,'+inttostr(fKanjiDetails.height-190)+',=,0,r,0,SET');
        sfl(fKanjiSearch,'HIDDEN,Y,LEFT,.,0,=,0,r,0,LEAVE');
        sd:=sd1; so:=so1;
//        sfl(fWordDetails,'HIDDEN,Y,TOP,.,0,=,0,r,0,LEAVE');
        sfl(fWordKanji,'VISIBLE,Y,RIGHT,.,0,=,0,=,180,LEAVE');
//        sfl(fWordCategory,'HIDDEN,Y,RIGHT,.,0,=,0,.,0,LEAVE');
        sfl(fUser,'VISIBLE,Y,TOP,=,180,=,0,=,'+inttostr(sd.x-fWordKanji.width)+',SET');
        sfl(fExamples,'HIDDEN,Y,TOP,.,0,=,0,r,0,LEAVE');
        sfl(fTranslate,'HIDDEN,Y,TOP,%,100,=,0,r,0,LEAVE');
        StdVocab;
      end;
    5:begin
        sfl(fKanjiDetails,'VISIBLE,Y,RIGHT,.,0,=,0,=,300,LEAVE');
        sfl(fKanji,'HIDDEN,Y,BOTTOM,=,190,=,0,r,0,SET');
        sfl(fKanjiCompounds,'VISIBLE,Y,BOTTOM,=,'+inttostr(fKanjiDetails.height-190)+',=,0,r,0,SET');
        sfl(fKanjiSearch,'HIDDEN,Y,LEFT,.,0,=,0,.,0,LEAVE');
        sd:=sd1; so:=so1;
        sfl(fWordKanji,'HIDDEN,Y,RIGHT,.,0,=,0,=,180,LEAVE');
//        sfl(fWordCategory,'HIDDEN,Y,RIGHT,.,0,=,0,.,0,LEAVE');
        sfl(fUser,'VISIBLE,Y,TOP,=,300,=,0,=,'+inttostr(sd.x-fKanjiDetails.width)+',SET');
        sfl(fExamples,'HIDDEN,Y,TOP,.,0,=,0,r,0,LEAVE');
//        sfl(fWordDetails,'HIDDEN,Y,TOP,.,0,=,0,r,0,LEAVE');
        sfl(fTranslate,'VISIBLE,Y,TOP,%,100,=,0,r,0,LEAVE');
        StdVocab;
      end;
  end;
}  if fKanji.tag=1 then fKanji.Show;
  if fUser.tag=1 then fUser.Show;
  if fWords.tag=1 then fWords.Show;
  if fTranslate.tag=1 then fTranslate.Show;
  if fKanjiDetails.tag=1 then fKanjiDetails.Show;
{  if lay=0 then SpeedButton16.Down:=true;
  if lay=1 then SpeedButton18.Down:=true;
  if lay=2 then SpeedButton17.Down:=true;
  if lay=3 then SpeedButton21.Down:=true;
  if lay=4 then SpeedButton19.Down:=true;
  if lay=5 then SpeedButton20.Down:=true;
  if lay=5 then aEditorWindow.Execute;
  if lay<3 then aKanjiWindow.Execute;
  if (lay=3) or (lay=4) then aDictJapanese.Execute;}
  aQuick1.Checked:=lay=0;
  aQuick2.Checked:=lay=1;
  aQuick3.Checked:=lay=2;
  aQuick4.Checked:=lay=3;
  aQuick5.Checked:=lay=4;
  aQuick6.Checked:=lay=5;
  curqlayout:=lay;
  fKanji.ManualDock(Panel3);
  fKanji.Align:=alClient;
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
//  SpeedButton2.Enabled:=false;
//  SpeedButton7.Enabled:=false;
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

procedure TfMenu.SetFormPos(form:TForm);
begin
{  form.Left:=0;
  form.Width:=Width;
  form.WindowState:=wsNormal;
  form.Height:=globheight-48;
  form.Top:=40;
  form.Borderstyle:=bsNone;}
end;

procedure ConvUniToMixUni(inpf,outf:string;var recn:integer);
var f:file of byte;
    t:textfile;
    bl,bh:byte;
    s:string;
    inuni:boolean;
begin
  recn:=0;
  assignfile(f,inpf);
  reset(f);
  assignfile(t,outf);
  rewrite(t);
  s:='';
  inuni:=false;
  while not eof(f) do
  begin
    read(f,bl);
    read(f,bh);
    if (bh=0) and (bl=10) then
    begin
      if inuni then s:=s+'>';
      writeln(t,s);
      s:='';
      inuni:=false;
      inc(recn);
    end;
    if (bh=0) and (bl<>10) then
    begin
      if inuni then s:=s+'>';
      inuni:=false;
      s:=s+chr(bl);
    end;
    if (bh<>0) then
    begin
      if not inuni then s:=s+'<';
      inuni:=true;
      s:=s+Format('%2.2X%2.2X',[bh,bl]);
    end;
  end;
  if s<>'' then writeln(t,s);
  closefile(f);
  closefile(t);
end;

procedure TfMenu.RadioGroup1Click(Sender: TObject);
begin
//  romasys:=RadioGroup1.ItemIndex+1;
end;

procedure TfMenu.RadioGroup2Click(Sender: TObject);
begin
//  showroma:=RadioGroup2.ItemIndex=1;
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
    fSettings.btnOkClick(self);
    fTranslate.Close;
    fUser.Close;
    fWords.Close;
    fKanji.Close;
  end;
end;

procedure TfMenu.SpeedButton5Click(Sender: TObject);
begin
  fSettings.pcPages.ActivePage:=fSettings.tsGeneral;
  fSettings.ShowModal;
  if fKanji.Visible then fKanji.DoIt;
  if fUser.Visible then fUser.Look();
  if fWords.Visible then fWords.ShowIt(false);
  if fTranslate.Visible then fTranslate.RepaintText;
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
    if (fKanji.Visible) and (fKanjiSearch.SpeedButton3.Down) then fKanji.DoIt;
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
  AddToClipboard(uFormat, pointer(text), Length(text)+1); //string + term 0
end;

procedure TfMenu.AddToClipboard(uFormat: UINT; text: UnicodeString);
begin
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

  if (fKanji.Visible) and (fKanjiSearch.SpeedButton3.Down) then fKanji.DoIt;
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

procedure TfMenu.ArtLabel1Click(Sender: TObject);
begin
  fSplash.ProgressBar1.Hide;
  fSplash.BitBtn1.Show;
  fSplash.ShowModal;
end;

procedure TfMenu.SpeedButton2Click(Sender: TObject);
begin
  screenModeWk:=SpeedButton2.Down;
end;

procedure TfMenu.SpeedButton7Click(Sender: TObject);
begin
  LoadUserData;
end;

procedure TfMenu.SpeedButton8Click(Sender: TObject);
begin
  fDictMan.ShowModal;
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

procedure TfMenu.SpeedButton9Click(Sender: TObject);
procedure SetBorder(form:Tform;bs:TFormBorderStyle);
var bch,bcw,bw,bh,rw,rh:integer;
    sf,sfw:boolean;
    cx1,cx2,cy1,cy2:integer;
begin
  bch:=form.ClientHeight;
  bcw:=form.ClientWidth;
  bw:=form.Width;
  bh:=form.Height;
  cx1:=form.Constraints.minWidth;
  cx2:=form.Constraints.MaxWidth;
  cy1:=form.Constraints.minHeight;
  cy2:=form.Constraints.maxHeight;
  sf:=((form.Constraints.minHeight=form.Constraints.MaxHeight) and
    (form.Constraints.minHeight>0)) or (bs=bsToolWindow);
  sfw:=((form.Constraints.minWidth=form.Constraints.MaxWidth) and
    (form.Constraints.minWidth>0)) or (bs=bsToolWindow);
  form.Constraints.minWidth:=0;
  form.Constraints.maxWidth:=0;
  form.Constraints.minheight:=0;
  form.Constraints.maxheight:=0;
  if not aBorders.Checked then
  begin
    form.BorderStyle:=bsNone;
  end else
  begin
    form.BorderStyle:=bs;
  end;
  if sf then form.ClientHeight:=bch;
  if sfw then form.ClientWidth:=bcw;
  rh:=form.Height-bh;
  rw:=form.Width-bw;
  if cx1>0 then form.Constraints.minWidth:=cx1+rw;
  if cx2>0 then form.Constraints.maxWidth:=cx2+rw;
  if cy1>0 then form.Constraints.minHeight:=cy1+rh;
  if cy2>0 then form.Constraints.maxHeight:=cy2+rh;
end;
begin
//  if not aBorders.Checked then DockExpress(nil,false);
//  showmessage('bef');
  borderchange:=true;
  SetBorder(fMenu,bsSizeToolWin);
  SetBorder(fKanji,bsSizeToolWin);
  SetBorder(fWords,bsSizeToolWin);
  SetBorder(fUser,bsSizeToolWin);
//  SetBorder(fUserAdd,bsSizeToolWin);
//  SetBorder(fUserFilters,bsToolWindow);
//  SetBorder(fUserDetails,bsToolWindow);
  SetBorder(fKanjiDetails,bsToolWindow);
//  SetBorder(fKanjiSearch,bsSizeToolWin);
//  SetBorder(fKanjiCompounds,bsSizeToolWin);
//  SetBorder(fWordDetails,bsSizeToolWin);
//  SetBorder(fExamples,bsSizeToolWin);
//  SetBorder(fWordCategory,bsToolWindow);
//  SetBorder(fWordKanji,bsToolWindow);
  SetBorder(fTranslate,bsSizeToolWin);
  SetBorder(fStrokeOrder,bsToolWindow);
//  if aBorders.Checked then SpeedButton16.Down:=true;
//  if aBorders.Checked then DockExpress(nil,true);
//  showmessage('aft');
  borderchange:=false;
end;

procedure TfMenu.Timer1Timer(Sender: TObject);
begin
//  SpeedButton9Click(self);
//  timer1.enabled:=false;
  Clipboard_Update;
end;

procedure TfMenu.SpeedButton11Click(Sender: TObject);
begin
  fWords.DoStatistic;
end;

procedure TfMenu.SpeedButton12Click(Sender: TObject);
begin
  Close;
end;

procedure TfMenu.btnJapaneseModeClick(Sender: TObject);
begin
  SwitchLanguage('j');
end;

procedure TfMenu.btnChineseModeClick(Sender: TObject);
begin
  SwitchLanguage('c');
end;

procedure TfMenu.Image1Click(Sender: TObject);
begin
  fSplash.ProgressBar1.Hide;
  fSplash.BitBtn1.Show;
  fSplash.ShowModal;
end;

procedure TfMenu.SpeedButton15Click(Sender: TObject);
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

procedure TfMenu.Action1Execute(Sender: TObject);
begin
  fUser.btnLookupEtoJ.Down:=true;
end;

procedure TfMenu.SpeedButton16Click(Sender: TObject);
begin
  StandardLayout((sender as TComponent).tag-1,100);
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

procedure TfMenu.aKanjiDetailsExecute(Sender: TObject);
begin
  if CharDetDocked then
  begin
    if (curdisplaymode=2) or (curdisplaymode=5) then
    begin
      fMenu.aKanjiDetails.Checked:=false;
      fKanji.btnKanjiDetails.Down:=false;
      fTranslate.sbDockKanjiDetails.Down:=false;
      CharDetDocked:=false
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
    fMenu.ToggleForm(fKanjiDetails,fKanji.btnKanjiDetails,fMenu.aKanjiDetails);
    fTranslate.sbDockKanjiDetails.Down:=fKanji.btnKanjiDetails.Down;
  end;
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
  SpeedButton5Click(Sender);
end;

procedure TfMenu.aChangeLanguageExecute(Sender: TObject);
begin
  fLanguage.ShowModal;
end;

procedure TfMenu.aSettingsDictExecute(Sender: TObject);
begin
  SpeedButton8Click(sender);
end;

procedure TfMenu.aQuick1Execute(Sender: TObject);
begin
  StandardLayout(0,100);
end;

procedure TfMenu.aQuick2Execute(Sender: TObject);
begin
  StandardLayout(1,100);
end;

procedure TfMenu.aQuick3Execute(Sender: TObject);
begin
  StandardLayout(2,100);
end;

procedure TfMenu.aQuick4Execute(Sender: TObject);
begin
  StandardLayout(3,100);
end;

procedure TfMenu.aQuick5Execute(Sender: TObject);
begin
  StandardLayout(4,100);
end;

procedure TfMenu.aQuick6Execute(Sender: TObject);
begin
  StandardLayout(5,100);
end;

procedure TfMenu.aBordersExecute(Sender: TObject);
begin
  aBorders.Checked:=not aBorders.Checked;
  SpeedButton9Click(Sender);
end;

procedure TfMenu.aHelpExecute(Sender: TObject);
begin
  SpeedButton15Click(Sender);
end;

procedure TfMenu.aAboutExecute(Sender: TObject);
begin
  Image1Click(Sender);
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
  fKanjiSearch.SpeedButton2.Down:=not fKanjiSearch.SpeedButton2.Down;
  fKanjiSearch.sbPinYinClick(Sender);
end;

procedure TfMenu.aKanjiClipboardExecute(Sender: TObject);
begin
  if not fKanji.Visible then aKanjiExecute(Sender);
  fKanjiSearch.SpeedButton3.Down:=not fKanjiSearch.SpeedButton3.Down;
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

procedure TfMenu.Timer2Timer(Sender: TObject);
begin
  timer2.enabled:=false;
  StandardLayout(proposedlayout,100);
end;

procedure TfMenu.tab3Click(Sender: TObject);
begin
  ToggleForm(fTranslate,nil,nil);
end;

//TODO: This function is not used anywhere.
//  Supposedly it was meant to update MainForm constraints to mind docked forms,
//  but if it's not used by later, delete it.
procedure TfMenu.FixConstraints(form:TForm;w,h:integer);
begin
  if (form.Constraints.MinHeight>0) then form.Constraints.MinHeight:=form.Constraints.MinHeight+h;
  if (form.Constraints.MinWidth>0) then form.Constraints.MinWidth:=form.Constraints.MinWidth+w;
  if (form.Constraints.MaxHeight>0) then form.Constraints.MaxHeight:=form.Constraints.MaxHeight+h;
  if (form.Constraints.MaxWidth>0) then form.Constraints.MaxWidth:=form.Constraints.MaxWidth+w;
end;

procedure TfMenu.DockProc(slave,host:TForm;panel:TPanel;dir:integer;dock:boolean);
var adocked:boolean;
    vert:boolean;
    fixsiz,flomin:integer;
    rect:TRect;
    bch,bcw:integer;
{    smw,smh,sxw,sxh:integer;}
{    clisiz:integer;
    simulshow:boolean;}
begin
  adocked:=slave.tag>=2;
{  simulshow:=slave.tag=0; }
  if adocked=dock then exit;
  vert:=(dir=1) or (dir=3);
  bch:=slave.Height;
  bcw:=slave.Width;
  if vert then fixsiz:=slave.ClientHeight else fixsiz:=slave.ClientWidth;
  if vert then flomin:=slave.ClientWidth else flomin:=slave.ClientHeight;
{  if (vert) and (slave.Constraints.MaxWidth<>0) then showmessage('INTERNAL ERROR: Fixed Slave constraints!');
  if (not vert) and (slave.Constraints.MinWidth<>0) then showmessage('INTERNAL ERROR: Fixed Slave constraints!');
  if (dock) and (vert) then host.Constraints.MinHeight:=host.Constraints.MinHeight+fixsiz;
  if (dock) and (not vert) then host.Constraints.MinWidth:=host.Constraints.MinWidth+fixsiz;
  if (not dock) and (vert) then host.Constraints.MinHeight:=host.Constraints.MinHeight-fixsiz;
  if (not dock) and (not vert) then host.Constraints.MinWidth:=host.Constraints.MinWidth-fixsiz;}
  if (dock) then if (vert) then panel.height:=fixsiz else panel.width:=fixsiz;
{  smw:=slave.Constraints.MinWidth;
  smh:=slave.Constraints.MinHeight;
  sxw:=slave.Constraints.MaxWidth;
  sxh:=slave.Constraints.MaxHeight;}
  if not vert then slave.Width:=panel.width else slave.Height:=panel.Height;
  if dock then
  begin
    slave.Show;
    slave.Hide;
  end;
{  slave.Constraints.MinWidth:=0;
  slave.Constraints.MaxWidth:=0;
  slave.Constraints.MinHeight:=0;
  slave.Constraints.MaxHeight:=0;}
  if dock then slave.ManualDock(panel);
  slave.ClientHeight:=bch;
  if (dock) then if (vert) then panel.height:=fixsiz else panel.width:=fixsiz;
  if dock then slave.align:=alClient else slave.align:=alNone;
  rect.left:=0;
  rect.top:=0;
  if vert then rect.right:=flomin else rect.right:=fixsiz;
  if vert then rect.bottom:=fixsiz else rect.bottom:=flomin;
  if not dock then slave.ManualFloat(rect);
  if (not dock) then if (vert) then panel.height:=0 else panel.width:=0;
  if dock then slave.tag:=2;
  if not dock then slave.tag:=0;
  if not dock then slave.hide;
{  if smw>0 then slave.Constraints.MinWidth:=smw+slave.Width-bcw;
  if smh>0 then slave.Constraints.MinHeight:=smh+slave.Height-bch;
  if sxw>0 then slave.Constraints.MaxWidth:=sxw+slave.Width-bcw;
  if sxh>0 then slave.Constraints.MaxHeight:=sxh+slave.Height-bch;         }
//  if (dock) and (slave.tag=2) and (simulshow) then DockExpress(form,false);
end;

procedure TfMenu.DockExpress(form:TForm;dock:boolean);
begin
  if (form=fKanjiSearch) or (form=nil) then DockProc(fKanjiSearch,fKanji,fKanji.Panel3,1,dock);
  if (form=fKanjiCompounds) or (form=nil) then DockProc(fKanjiCompounds,fKanji,fKanji.Panel2,3,dock);
  if ((form=fExamples) or (form=nil)) and (curdisplaymode<>5) then DockProc(fExamples,fUser,fUser.Panel2,3,dock);
  if ((form=fExamples) or (form=nil)) and (curdisplaymode=5) then DockProc(fExamples,fWords,fWords.Panel5,3,dock);
  if (form=fWordKanji) or (form=nil) then DockProc(fWordKanji,fUser,fUser.Panel3,2,dock);
  if (form=fUserFilters) or (form=nil) then DockProc(fUserFilters,fWords,fWords.Panel2,2,dock);
  if (form=fUserDetails) or (form=nil) then DockProc(fUserDetails,fWords,fWords.Panel4,3,dock);
end;

{ Only use this to undock forms from containers which don't need to be hidden,
 such as modules from a main form. For dock panels use DockExpress(form,false).
 One exception is fKanjiDetails (probably historically) }
procedure TfMenu.FormUndock(form:TForm);
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

procedure TfMenu.Button1Click(Sender: TObject);
var t:textfile;
    gb:array[0..96*96-1] of word;
    big5:array[0..96*160-1] of word;
    gbs,b5s:string;
    i1,i2:integer;
    s:string;
begin
  for i1:=0 to 96*96-1 do gb[i1]:=0;
  for i1:=0 to 96*160-1 do big5[i1]:=0;
  TChar.First;
  while not TChar.EOF do
  begin
    b5s:=GetCharValue(TChar.Int(TCharIndex),51);
    gbs:=GetCharValue(TChar.Int(TCharIndex),54);
    if b5s<>'' then
    begin
      i1:=strtoint('0x'+copy(b5s,1,2))-160;
      i2:=strtoint('0x'+copy(b5s,3,4));
      if i2>$a0 then i2:=i2-160 else i2:=i2-64;
      big5[i1*160+i2]:=strtoint('0x'+TChar.Str(TCharUnicode));
    end;
    if gbs<>'' then
    begin
      i1:=strtoint(copy(gbs,1,2));
      i2:=strtoint(copy(gbs,3,4));
      gb[i1*96+i2]:=strtoint('0x'+TChar.Str(TCharUnicode));
    end;
    TChar.Next;
  end;
  assignfile(t,'table.dat');
  rewrite(t);
  s:='  ';
  for i1:=0 to 96*96-1 do
  begin
    s:=s+'$'+lowercase(Format('%4.4x',[gb[i1]]))+',';
    if i1 mod 10=9 then begin writeln(t,s); s:='  '; end;
  end;
  writeln(t,s);
  writeln(t,'');
  s:='  ';
  for i1:=0 to 96*160-1 do
  begin
    s:=s+'$'+lowercase(Format('%4.4x',[big5[i1]]))+',';
    if i1 mod 10=9 then begin writeln(t,s); s:='  '; end;
  end;
  writeln(t,s);
  closefile(t);
end;

procedure TfMenu.Button2Click(Sender: TObject);
begin
  DockExpress(nil,false);
end;

procedure TfMenu.FormResize(Sender: TObject);
begin
//  PaintBox3.Visible:=Width>815;
//  Shape9.Visible:=Width>815;
//  SpeedButton22.Visible:=Width>815;
//  Bevel5.Visible:=Width>815;
end;

procedure TfMenu.tab4Click(Sender: TObject);
begin
  ToggleForm(fKanjiDetails,nil,nil);
end;

procedure TfMenu.ChangeDisplay;
begin
  case curdisplaymode of
    1:FormUndock(fKanji);
    2:fUser.Hide;
    5:fWords.Hide;
    3:fTranslate.Hide;
    4:begin
        fUser.Hide;
        fTranslate.Hide;
      end;
  end;
  if CharDetNowDocked then FormUndock(fKanjiDetails);
  CharDetNowDocked:=false;
  Panel2.Height:=0;
  Panel4.Width:=0;
  aMode1.Checked:=false;
  aMode2.Checked:=false;
  aMode3.Checked:=false;
  aMode4.Checked:=false;
  aMode5.Checked:=false;
  if CharDetDocked then
  begin
    if displaymode=1 then
    begin
      if CharDetDockedVis1 then
      begin
        Panel4.Width:=fKanjiDetails.Width;
        fKanjiDetails.ManualDock(Panel4);
        fKanjiDetails.Align:=alClient;
        fKanjiDetails.Show;
      end;
      fKanji.btnKanjiDetails.Down:=CharDetDockedVis1;
      aKanjiDetails.Checked:=CharDetDockedVis1;
      CharDetNowDocked:=CharDetDockedVis1;
    end;
    if (displaymode=4) or (displaymode=3) then
    begin
      if CharDetDockedVis2 then
      begin
        Panel4.Width:=fKanjiDetails.Width;
        fKanjiDetails.ManualDock(Panel4);
        fKanjiDetails.Align:=alClient;
        fKanjiDetails.Show;
      end;
      fTranslate.sbDockKanjiDetails.Down:=CharDetDockedVis2;
      aKanjiDetails.Checked:=CharDetDockedVis2;
      CharDetNowDocked:=CharDetDockedVis2;
    end;
  end;
  if fExamples.visible then
  begin
    fExamples.hide;
    DockExpress(fExamples,false);
    fExamples.tag:=0;
  end;
  case displaymode of
    1:begin
        fKanji.Width:=Panel3.ClientWidth;
        fKanji.Height:=Panel3.ClientHeight;
        fKanji.ManualDock(Panel3);
        fKanji.Align:=alClient;
        fKanji.Show;
        tab1.Down:=true;
        if fKanji.DrawGrid1.CanFocus then
          fKanji.DrawGrid1.SetFocus;
        aMode1.Checked:=true;
      end;
    2:begin
        fUser.Width:=Panel3.ClientWidth;
        fUser.Height:=Panel3.ClientHeight;
        fUser.ManualDock(Panel3);
        fUser.Align:=alClient;
        fUser.Show;
        tab2.Down:=true;
        aMode2.Checked:=true;
      end;
    5:begin
        fWords.Width:=Panel3.ClientWidth;
        fWords.Height:=Panel3.ClientHeight;
        fWords.ManualDock(Panel3);
        fWords.Align:=alClient;
        fWords.Show;
        tab5.Down:=true;
        aMode5.Checked:=true;
      end;
    3:begin
        fTranslate.Width:=Panel3.ClientWidth;
        fTranslate.Height:=Panel3.ClientHeight;
        fTranslate.ManualDock(Panel3);
        fTranslate.Align:=alClient;
        fTranslate.Show;
        tab3.Down:=true;
        fTranslate.sbDockDictionary.Down:=false;
        aMode3.Checked:=true;
      end;
    4:begin
        Panel2.height:=250;
        fUser.Width:=Panel2.ClientWidth;
        fUser.Height:=Panel2.ClientHeight;
        fUser.ManualDock(Panel2);
        fUser.Align:=alClient;
        fUser.Show;
        fTranslate.ManualDock(Panel3);
        fTranslate.Align:=alClient;
        fTranslate.Show;
        fTranslate.Width:=Panel3.ClientWidth;
        fTranslate.Height:=Panel3.ClientHeight;
        tab3.Down:=true;
        fTranslate.sbDockDictionary.Down:=true;
        aMode3.Checked:=true;
      end;
  end;
  curdisplaymode:=displaymode;
  fKanjiDetails.FormShow(fMenu);
  if (((curdisplaymode=2) or (displaymode=4)) and (aDictAdd.Checked)) or
     ((curdisplaymode=5) and (aUserExamples.Checked)) then
  begin
    fExamples.tag:=1;
    DockExpress(fExamples,true);
    fExamples.show;
    fExamples.tag:=2;
  end;
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

procedure TfMenu.eCharacterlistcSeznamznak1Click(Sender: TObject);
begin
  displaymode:=1;
  ChangeDisplay;
end;

procedure TfMenu.eDictionarycSlovnk2Click(Sender: TObject);
begin
  displaymode:=2;
  ChangeDisplay;
end;

procedure TfMenu.eTexteditorcTextoveditor1Click(Sender: TObject);
begin
  displaymode:=3;
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

procedure TfMenu.ShowScreenTip(x,y:integer;s:FString;wt:integer;immediate:boolean);
var maxwords,maxwordss:integer;
    wasfull:boolean;
    s1,s2:FString; //kinda fstring, has control chars
    s3,s4:string;
    ss:string;
    ch,kch:integer;
    rect:TRect;
    kkch,kkcw:integer;
    vsiz,hsiz,vfsiz,hfsiz:integer;
    i:integer;
    sep:integer;
    tpp:integer;
    optwidth,cw:integer;
    proposeds:string;
    maxslen,slen:integer;
begin
  SetScreenTipBlock(0,0,0,0,nil);
  intorgPaint:=nil;
  intorgGrid:=nil;
  if ((wt=7) and (not fSettings.CheckBox47.Checked)) then exit;
  if ((wt<7) and (not fSettings.CheckBox28.Checked)) then exit;
  fScreenTip:=TfScreenTip.Create(nil);
  screenTipShown:=true;
  maxwords:=strtoint(fSettings.Edit24.Text);
  if maxwordss<10 then maxwordss:=10;
  if wt=7 then
  begin
    //Apparently, word type 7 means "latin word", so we try to look for one
    //DicSearch expects latin text to be raw, contrary to every other case when it's in FChars.
    DicSearch(fstrtouni(s),stEn,mtExactMatch,false,7,maxwordss,fScreenTip.screenTipList,5,wasfull);
    if (fScreenTip.screenTipList.Count=0) then
    begin
      ss:=fstrtouni(s);
     //What the hell are we doing here?! "If nothing matches, try deleting
     //some letters, but only if those are 'ed' or 's'"?
     //I think this calls for a proper english deflexion function.
      if (length(ss)>2) and (copy(ss,length(ss)-1,2)='ed') then delete(ss,length(ss)-1,2) else
        if (length(ss)>1) and (ss[length(ss)]='s') then delete(ss,length(ss),1);
      DicSearch(ss,stEn,mtExactMatch,false,7,maxwordss,fScreenTip.screenTipList,5,wasfull);
    end;
  end;
  if wt<7 then
    DicSearch(s,stEditorInsert,mtExactMatch,false,wt,maxwordss,fScreenTip.screenTipList,5,wasfull);
  if maxwords>fScreenTip.screenTipList.Count then
    maxwords:=fScreenTip.screenTipList.Count;
  fScreenTip.screenTipWords:=maxwords;
  tpp:=20;
  ch:=GridFontSize+3;
  kch:=strtoint(fSettings.Edit26.Text);
  optwidth:=0;
  proposeds:='';
  maxslen:=0;
  for i:=0 to maxwords-1 do
  begin
    slen:=fScreenTip.screenTipList[i].slen;
    if slen>maxslen then maxslen:=slen;
    ss:=fScreenTip.screenTipList[i].ArticlesToString;
    SplitWord(ss,s1,s2,s3,s4);
    rect.left:=0;
    rect.right:=Screen.Width;
    rect.top:=0;
    rect.bottom:=100;
    s1 := remexcl(s1);
    s2 := remexcl(s2);
    cw:=DrawWordInfo(fScreenTip.pb.Canvas,rect,false,false,2,s3,false,true,GridFontSize,true)+GridFontSize*(3+flength(s1+s2));
    if cw>optwidth then optwidth:=cw;
  end;
  if maxslen>0 then proposeds:=fcopy(s,1,maxslen);
  vsiz:=5;
  hsiz:=20;
  optwidth:=optwidth-5*kch;
  optwidth:=optwidth div kch;
  if optwidth<strtoint(fSettings.Edit27.Text) then optwidth:=strtoint(fSettings.Edit27.Text);
  if optwidth>strtoint(fSettings.Edit28.Text) then optwidth:=strtoint(fSettings.Edit28.Text);
  fScreenTip.ScreenTipWidth:=optwidth;
  hsiz:=optwidth;
  sep:=4;
  vfsiz:=vsiz+5;
  hfsiz:=hsiz+vsiz+4;
  kkcw:=hfsiz*kch;
  kkch:=vfsiz*kch;
  fScreenTip.screenTipText:=s;
  if proposeds<>'' then
    fScreenTip.screenTipText:=proposeds;
  fScreenTip.screenTipWt:=wt;
  fScreenTip.Left:=x;
  fScreenTip.Top:=y;
  fScreenTip.Width:=kkcw+sep*2+1;
  if (wt=1) and (fSettings.CheckBox48.Checked) then
    fScreenTip.Height:=maxwords*ch+sep*3+kkch+tpp else fScreenTip.Height:=maxwords*ch+sep*2+1+tpp;
  if not immediate then
  begin
    if y+fScreenTip.Height>Screen.Height then fScreenTip.Top:=y-20-fScreenTip.Height;
    if x+fScreenTip.Width>Screen.Width then fScreenTip.Left:=Screen.Width-fScreenTip.Width;
  end;
  SetWindowPos(fScreenTip.handle,HWND_TOPMOST,0,0,0,0,SWP_NOMOVE or SWP_NOSIZE or SWP_NOACTIVATE or SWP_SHOWWINDOW);
  fScreenTip.screenTipButton:=0;
  fScreenTip.PopupMouseMove(Mouse.CursorPos.x-fScreenTip.left,Mouse.CursorPos.y-fScreenTip.Top);
end;

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
  screenTipImmediate:=true;
  ScreenTimerTimer(nil);
  screenTipImmediate:=false;
end;

procedure TfMenu.HideScreenTip;
begin
  fScreenTip.Hide;
  fScreenTip.Free;
  screenTipShown:=false;
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
begin
  if not initdone then exit;
  curt:=now-lastautosave;
  if curt>1/24/60*strtoint(fSettings.Edit29.Text) then
  begin
    lastautosave:=now;
    SaveUserData;
  end;
  if not screenModeWk and not screenModeSc and not screenTipImmediate and not popcreated then exit;
  if inproc then exit;
  inproc:=true;
  try
    pt:=Mouse.CursorPos;
  except inproc:=false; exit; end;
  try
  if not TryStrToInt(fSettings.Edit21.Text, ttim) then ttim:=10;
  if not TryStrToInt(fSettings.Edit22.Text, tleft) then tleft:=10;
  if not TryStrToInt(fSettings.Edit23.Text, tright) then tright:=100;

  if (popcreated) and (pt.x>=fScreenTip.Left-10) and
     (pt.y>=fScreenTip.Top-10) and (pt.x<=fScreenTip.Left+fScreenTip.Width+10) and
     (pt.y<=fScreenTip.Top+fScreenTip.Height+10) then
  begin
    inproc:=false;
    exit;
  end;
  if ((pt.x<>oldpt.x) or (pt.y<>oldpt.y)) and (not screenTipImmediate) then
  begin
    if popcreated then
    begin
      fMenu.HideScreenTip;
      popcreated:=false;
    end;
    oldpt:=pt;
    tim:=0;
    inproc:=false;
    exit;
  end;
//  Label1.Caption:=datetimetostr(now)+' <'+inttostr(tim)+'> '+inttostr(pt.x)+'-'+inttostr(pt.y);
  if tim<>-1 then inc(tim);
  if (tim<ttim) and (not screenTipImmediate) then begin
    inproc:=false;
    exit;
  end;
  ftextpos:=0;
  s:='';
  if (screenModeWk) or (screenTipImmediate) then
  begin
    if ((intmoPaint<>nil) or (intmoGrid<>nil)) and (pt.x=intmosx) and (pt.y=intmosy) then
    begin
      s:=intcurString;
    end;
    begpt:=pt;
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
    begpt:=pt;
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
  if paramstr(1)='debug'then
  begin
    clip:=s2;
    SetClipboard;
  end;
  if s<>'' then
  begin
    if screenTipImmediate then
      fMenu.ShowScreenTip(pt.x-10,pt.y-10,s,evc,true) else
      fMenu.ShowScreenTip(pt.x+10,pt.y+10,s,evc,false);
    if screenTipShown then popcreated:=true;
  end;
  tim:=-1;
  finally
  inproc:=false;
  end;
end;


{
IntTipPaintOver(), IntTipGridOver()
Various controls from all over the program call these on mouse move,
to determine which characters are under mouse cursor right now.
This information is used only by ScreenTimerTimer (showing popup tip),
so we could have calculated it there, but we'd need to figure out the class
of control mouse is over.
}
procedure TfMenu.IntTipPaintOver(p:TPaintBox;x,y:integer;leftDown:boolean);
begin
  if leftDown and ((intorgGrid<>nil) or (intorgPaint<>p)) then
  begin
    intorgcx:=x; intorgcy:=y; intorgsx:=Mouse.CursorPos.x; intorgsy:=Mouse.CursorPos.y;
    intorgPaint:=p; intorgGrid:=nil;
  end;
  intmocx:=x; intmocy:=y; intmosx:=Mouse.CursorPos.x; intmosy:=Mouse.CursorPos.y;
  intmopaint:=p;
  intmogrid:=nil;
  CalculateCurString;
end;

procedure TfMenu.IntTipGridOver(sg:TCustomDrawGrid;x,y:integer;leftDown:boolean);
begin
  if leftDown and ((intorgGrid<>sg) or (intorgPaint<>nil)) then
  begin
    intorgcx:=x; intorgcy:=y; intorgsx:=Mouse.CursorPos.x; intorgsy:=Mouse.CursorPos.y;
    intorgPaint:=nil; intorgGrid:=sg;
  end;
  intmocx:=x; intmocy:=y; intmosx:=Mouse.CursorPos.x; intmosy:=Mouse.CursorPos.y;
  intmopaint:=nil;
  intmogrid:=sg;
  CalculateCurString;
end;

procedure TfMenu.CalculateCurString;
var s,s2:string;
    rx,ry,wtt:integer;
    x1,y1,x2,y2:integer;
    gc,gc2:TGridCoord;
    rect:TRect;
    mox1,mox2:integer;
    mo:boolean;
begin
  SetScreenTipBlock(0,0,0,0,nil);
  if (intmoPaint<>nil) and (intmoPaint<>fTranslate.EditorPaintBox) then
  begin
    s:=FindDrawReg(intmoPaint,intmocx,intmocy,x1,y1,y2);
    if (intorgPaint=intmoPaint) then
    begin
      s2:=FindDrawReg(intorgPaint,intorgcx,intorgcy,x2,y1,y2);
      if length(s2)>length(s) then
      begin
        s2:=s;
        x2:=x1;
        s:=FindDrawReg(intorgPaint,intorgcx,intorgcy,x1,y1,y2);
      end;
      if copy(s,length(s)-length(s2)+1,length(s2))=s2 then
      begin
        s:=copy(s,1,length(s)-length(s2));
        SetScreenTipBlock(x1,y1,x2,y2,intmoPaint.Canvas);
      end;
    end;
  end else
  if intmoPaint=fTranslate.EditorPaintBox then
  begin
    fTranslate.CalcMouseCoords(intmocx,intmocy,rx,ry);
    if ry<>-1 then s:=fTranslate.GetDocWord(rx,ry,wtt,false);
  end else
  begin
    gc:=intmoGrid.MouseCoord(intmocx,intmocy);
    if (intmoGrid<>fKanji.DrawGrid1) and (intmoGrid<>fRadical.DrawGrid) then
    begin
      if (gc.x>=0) and (gc.x<2) and (gc.y>0) then
      begin
        if intorgGrid=intmoGrid then
        begin
          gc2:=intmoGrid.MouseCoord(intorgcx,intorgcy);
          mo:=(gc2.x=gc.x) and (gc2.y=gc.y);
        end else mo:=false;
        s:=remexcl((TStringGrid(intmoGrid)).Cells[gc.x,gc.y]);
        rect:=intmoGrid.CellRect(gc.x,gc.y);
        if not mo then fdelete(s,1,((intmocx-rect.left-2) div GridFontSize));
        if mo then
        begin
          if intorgcx>intmocx then
          begin
            mox1:=intmocx;
            mox2:=intorgcx;
          end else
          begin
            mox1:=intorgcx;
            mox2:=intmocx;
          end;
          mox1:=(mox1-rect.left-2) div GridFontSize;
          mox2:=(mox2-rect.left-2) div GridFontSize;
          s:=fcopy(s,mox1,(mox2-mox1));
          SetScreenTipBlock(mox1*GridFontSize+rect.left+2,rect.top,mox2*GridFontSize+rect.left+2,rect.bottom,intmoGrid.Canvas);
        end;
      end;
    end else if intmoGrid=fKanji.DrawGrid1 then
      s:=fKanji.GetKanji(gc.x,gc.y) else s:=fRadical.GetKanji(gc.x,gc.y);
  end;
  intcurString:=s;
end;


procedure TfMenu.PaintBox3MouseMove(Sender: TObject; Shift: TShiftState; X,
  Y: Integer);
begin
  fMenu.IntTipPaintOver(PaintBox3,x,y,ssLeft in Shift);
end;

procedure TfMenu.eSavekanatranscriptcUloitpepisdokany1Click(
  Sender: TObject);
var
  stream: TStream;
  enctype: integer;
begin
  if not fTranslate.Visible then aDictEditorExecute(Sender);
  if not fTranslate.FullTextTranslated then
    Application.MessageBox(
      pchar(_l('#00369^eDo not forget to fill kana readings or use the auto-fill function before using this feature.')),
      pchar(_l('#00364^eNotice')),
      MB_ICONINFORMATION or MB_OK);
  SaveAsKanaDialog.FileName := ExtractFilename(ChangeFileExt(fTranslate.docfilename,'')); //Default name's the same, without extension
  if not SaveAsKanaDialog.Execute then exit;

  case SaveAsKanaDialog.FilterIndex of
    1,2,3: enctype := Conv_ChooseType(false,0);
    4,5: enctype := FILETYPE_UTF8; //UTF8 only for HTML, ODT
  end;

  stream := nil;
  try
    stream := TStreamWriter.Create(
      TFileStream.Create(SaveAsKanaDialog.FileName,fmCreate),
      true
    );

    case SaveAsKanaDialog.FilterIndex of
      1: fTranslate.SaveText(amRuby,TKanaOnlyFormat.Create(enctype,{AddSpaces=}true),stream);
      2: fTranslate.SaveText(amRuby,TKanjiKanaFormat.Create(enctype),stream);
      3: fTranslate.SaveText(amRuby,TKanjiOnlyFormat.Create(enctype),stream);
      4: fTranslate.SaveText(amRuby,THtmlFormat.Create([]),stream);
      5: fTranslate.SaveText(amRuby,TOpenDocumentFormat.Create(),stream);
    end;

  finally
    FreeAndNil(stream);
  end;
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

procedure TfMenu.aDictVoc1Execute(Sender: TObject);
begin
{  if not fUser.Visible then aDictExecute(Sender);
  fExamples.SpeedButton1.Down:=true;}
end;

procedure TfMenu.aDictVoc2Execute(Sender: TObject);
begin
{  if not fUser.Visible then aDictExecute(Sender);
  fExamples.SpeedButton2.Down:=true;}
end;

procedure TfMenu.aDictVoc3Execute(Sender: TObject);
begin
{  if not fUser.Visible then aDictExecute(Sender);
  fExamples.SpeedButton3.Down:=true;}
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
  if mbLeft=Button then fMenu.PopupImmediate(true);
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

procedure TfMenu.eSavecharacterstofilecUloitznakydosouboru1Click(
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
begin
  if aPortraitMode.Checked then begin
    Panel4.Align := alBottom;
    fWords.Panel2.Align := alBottom;
    fWords.Splitter1.Align := alBottom;
    fWords.Splitter1.Top := fWords.Panel2.Top - 10;
    fUser.Panel3.Align := alBottom;
  end else begin
    Panel4.Align := alRight;
    fWords.Panel2.Align := alRight;
    fWords.Splitter1.Align := alRight;
    fWords.Splitter1.Left := fWords.Panel2.Left - 10;
    fUser.Panel3.Align := alRight;
  end;
  fUserFilters.SetPortraitMode(aPortraitMode.Checked);
  fWordKanji.SetPortraitMode(aPortraitMode.Checked);
  fKanjiDetails.SetPortraitMode(aPortraitMode.Checked);
  ChangeDisplay;
 //TODO
end;

initialization
  tim:=0;
  rdcnt:=0;
  inproc:=false;
  popcreated:=false;
  chardetl:=TStringList.Create;
  CharPropTypes:=TStringList.Create;

finalization

end.
